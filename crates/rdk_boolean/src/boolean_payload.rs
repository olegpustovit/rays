//! Port of lib/rdk/boolean/boolean_payload.ml: primitive-owned payload transfer for exact Boolean
//! extraction. Every per-element loop writes its own slot, so the loops run sequentially (the
//! parallel path of the OCaml is byte-identical by construction); `grain` is validated and passed
//! to group construction. Cancellation is polled every 4096 elements like `Support.parallel_for`.
use std::sync::Arc;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};

use rays_math::text_array::TextArray;
use rdk_core::support;
use rdk_core::attribute::{self, Owner as AttributeOwner, Storage};
use rdk_core::{cancel, edge_group, error, geometry, group, packed, topology, topology_index};
use rdk_core::error::Code;

use crate::boolean_constraints::Side;
use crate::boolean_extract::{self as extract, view as extract_view};

const OPERATION: &str = "boolean_payload";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PointConflict {
    Reject,
    PromoteToVertex,
}

struct AttributePair {
    name: String,
    source_owner: AttributeOwner,
    kind: i64,
    left: Option<attribute::T>,
    right: Option<attribute::T>,
}

struct GroupPair {
    group_name: String,
    source_group_owner: group::Owner,
    left_group: Option<group::T>,
    right_group: Option<group::T>,
}

struct EdgeGroupPair {
    edge_group_name: String,
    left_edge_group: Option<edge_group::T>,
    right_edge_group: Option<edge_group::T>,
}

enum Fail {
    Cancelled,
    Invalid(String),
    Rdk(error::T),
}

impl From<error::T> for Fail {
    fn from(value: error::T) -> Fail {
        if error::is_cancelled(&value) { Fail::Cancelled } else { Fail::Rdk(value) }
    }
}

fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

fn payload_fail(code: Code, message: String) -> Fail {
    Fail::Rdk(make(code, message))
}

fn invalid_fail(message: &str) -> Fail {
    Fail::Invalid(message.to_owned())
}

fn finish_fail(failure: Fail) -> error::T {
    match failure {
        Fail::Cancelled => error::cancelled(OPERATION),
        Fail::Invalid(message) => make(Code::InvalidPayload, message),
        Fail::Rdk(failure) => failure,
    }
}

fn storage_kind(storage: &Storage) -> i64 {
    match storage {
        Storage::Float(_) => 0,
        Storage::Int(_) => 1,
        Storage::IntArray(_) => 2,
        Storage::FloatArray(_) => 3,
        Storage::Float2(_) => 4,
        Storage::Float3(_) => 5,
        Storage::Float4(_) => 6,
        Storage::Text(_) => 7,
    }
}

use rdk_core::attribute::owner_name;

fn collect_attributes(
    owner: AttributeOwner,
    left: &geometry::T,
    right: &geometry::T,
) -> Result<Vec<AttributePair>, Fail> {
    let mut table: HashMap<String, usize> = HashMap::new();
    let mut pairs: Vec<AttributePair> = Vec::new();
    for (side, source) in [(0, left), (1, right)] {
        for item in geometry::view::attributes(source) {
            if attribute::owner(item) != owner {
                continue;
            }
            let name = attribute::name(item).to_owned();
            let kind = storage_kind(attribute::view::storage(item)?);
            match table.get(&name) {
                None => {
                    table.insert(name.clone(), pairs.len());
                    let mut pair =
                        AttributePair { name, source_owner: owner, kind, left: None, right: None };
                    if side == 0 {
                        pair.left = Some(item.clone());
                    } else {
                        pair.right = Some(item.clone());
                    }
                    pairs.push(pair);
                }
                Some(&slot) => {
                    let pair = &mut pairs[slot];
                    if pair.kind != kind {
                        return Err(payload_fail(Code::AttributeStorageMismatch, format!(
                                "{} attribute {:?} has different storage on the Boolean operands",
                                owner_name(owner),
                                name
                            )));
                    }
                    if side == 0 {
                        pair.left = Some(item.clone());
                    } else {
                        pair.right = Some(item.clone());
                    }
                }
            }
        }
    }
    Ok(pairs)
}

fn collect_groups(owner: group::Owner, left: &geometry::T, right: &geometry::T) -> Vec<GroupPair> {
    let mut table: HashMap<String, usize> = HashMap::new();
    let mut pairs: Vec<GroupPair> = Vec::new();
    for (side, source) in [(0, left), (1, right)] {
        for item in geometry::groups(source) {
            if group::owner(&item) != owner {
                continue;
            }
            let name = group::name(&item).to_owned();
            let slot = match table.get(&name) {
                Some(&slot) => slot,
                None => {
                    table.insert(name.clone(), pairs.len());
                    pairs.push(GroupPair {
                        group_name: name,
                        source_group_owner: owner,
                        left_group: None,
                        right_group: None,
                    });
                    pairs.len() - 1
                }
            };
            if side == 0 {
                pairs[slot].left_group = Some(item);
            } else {
                pairs[slot].right_group = Some(item);
            }
        }
    }
    pairs
}

fn collect_edge_groups(left: &geometry::T, right: &geometry::T) -> Vec<EdgeGroupPair> {
    let mut table: HashMap<String, usize> = HashMap::new();
    let mut pairs: Vec<EdgeGroupPair> = Vec::new();
    for (side, source) in [(0, left), (1, right)] {
        for item in geometry::edge_groups(source) {
            let name = edge_group::name(&item).to_owned();
            let slot = match table.get(&name) {
                Some(&slot) => slot,
                None => {
                    table.insert(name.clone(), pairs.len());
                    pairs.push(EdgeGroupPair {
                        edge_group_name: name,
                        left_edge_group: None,
                        right_edge_group: None,
                    });
                    pairs.len() - 1
                }
            };
            if side == 0 {
                pairs[slot].left_edge_group = Some(item);
            } else {
                pairs[slot].right_edge_group = Some(item);
            }
        }
    }
    pairs
}

type View = extract_view::AncestryView;

fn is_left(view: &View, primitive: usize) -> bool {
    view.primitive_sides[primitive] == 0
}

fn source_index(view: &View, owner: AttributeOwner, primitive: usize, local: usize) -> usize {
    match owner {
        AttributeOwner::Point => view.primitive_source_points[primitive * 3 + local] as usize,
        AttributeOwner::Vertex => view.primitive_source_vertices[primitive * 3 + local] as usize,
        _ => unreachable!(),
    }
}

fn dominant_values(a: f64, b: f64, c: f64) -> usize {
    if a >= b && a >= c {
        0
    } else if b >= c {
        1
    } else {
        2
    }
}

fn dominant_corner(view: &View, corner: usize) -> usize {
    dominant_values(view.barycentric_a[corner], view.barycentric_b[corner], view.barycentric_c[corner])
}

fn float_equal(tolerance: f64, left: f64, right: f64) -> bool {
    left == right
        || left.to_bits() == right.to_bits()
        || (left.is_finite()
            && right.is_finite()
            && (left - right).abs() <= tolerance * 1.0_f64.max(left.abs().max(right.abs())))
}

fn normalize3(x: f64, y: f64, z: f64) -> (f64, f64, f64) {
    let scale = x.abs().max(y.abs().max(z.abs()));
    if scale == 0.0 || !scale.is_finite() {
        return (x, y, z);
    }
    let (sx, sy, sz) = (x / scale, y / scale, z / scale);
    let length = (sx * sx + sy * sy + sz * sz).sqrt();
    if length == 0.0 || !length.is_finite() {
        return (x, y, z);
    }
    (sx / length, sy / length, sz / length)
}

fn corner_poll(cancel_value: Option<&cancel::T>, corner: usize) -> Result<(), Fail> {
    cancel::poll(cancel_value, corner as i64, 4095)?;
    Ok(())
}

fn sorted_offsets(lengths: &[i64], what: &str) -> Result<Vec<i64>, Fail> {
    let mut offsets = vec![0_i64; lengths.len() + 1];
    for index in 0..lengths.len() {
        match offsets[index].checked_add(lengths[index]) {
            Some(value) => offsets[index + 1] = value,
            None => return Err(invalid_fail(what)),
        }
    }
    Ok(offsets)
}

fn interpolate_fixed(
    cancel_value: Option<&cancel::T>,
    ancestry: &extract::Ancestry,
    view: &View,
    corner_count: usize,
    pair: &AttributePair,
) -> Result<Storage, Fail> {
    let left = pair.left.as_ref().map(attribute::view::storage).transpose()?;
    let right = pair.right.as_ref().map(attribute::view::storage).transpose()?;
    if pair.kind == 0 || (pair.kind >= 4 && pair.kind <= 6) {
        let like = left.or(right).expect("attribute pair has a source");
        let planes_of = planes_of_storage;
        let left_planes = planes_of(left)?;
        let right_planes = planes_of(right)?;
        let plane_count = attribute::float_planes(like)?.expect("float planes").len();
        let mut output: Vec<Vec<f64>> = (0..plane_count).map(|_| vec![0.0; corner_count]).collect();
        for corner in 0..corner_count {
            corner_poll(cancel_value, corner)?;
            let primitive = corner / 3;
            let values = if is_left(view, primitive) { left_planes.as_ref() } else { right_planes.as_ref() };
            let Some(values) = values else { continue };
            let a = source_index(view, pair.source_owner, primitive, 0);
            let b = source_index(view, pair.source_owner, primitive, 1);
            let c = source_index(view, pair.source_owner, primitive, 2);
            let wa = view.barycentric_a[corner];
            let wb = view.barycentric_b[corner];
            let wc = view.barycentric_c[corner];
            for (p, plane) in output.iter_mut().enumerate() {
                plane[corner] = wa * values[p][a] + wb * values[p][b] + wc * values[p][c];
            }
            if pair.name == "N" && output.len() == 3 {
                let winding = extract::primitive_winding(ancestry, primitive as i64) as f64;
                let (x, y, z) = normalize3(
                    winding * output[0][corner],
                    winding * output[1][corner],
                    winding * output[2][corner],
                );
                output[0][corner] = x;
                output[1][corner] = y;
                output[2][corner] = z;
            }
        }
        return Ok(attribute::of_float_planes(like, output)?);
    }
    if pair.kind == 1 {
        let int_of = int_of_storage;
        let (left, right) = (int_of(left), int_of(right));
        let mut output = vec![0_i64; corner_count];
        for corner in 0..corner_count {
            corner_poll(cancel_value, corner)?;
            let primitive = corner / 3;
            let values = if is_left(view, primitive) { left } else { right };
            if let Some(values) = values {
                output[corner] = values[source_index(
                    view,
                    pair.source_owner,
                    primitive,
                    dominant_corner(view, corner),
                )];
            }
        }
        return Ok(Storage::Int(Arc::new(output)));
    }
    if pair.kind == 7 {
        let text_of = text_of_storage;
        let (left, right) = (text_of(left), text_of(right));
        let mut output = TextArray::new();
        for corner in 0..corner_count {
            corner_poll(cancel_value, corner)?;
            let primitive = corner / 3;
            let values = if is_left(view, primitive) { left } else { right };
            output.push(match values {
                Some(values) => &values[source_index(view, pair.source_owner, primitive, dominant_corner(view, corner))],
                None => &[],
            });
        }
        return Ok(Storage::Text(Arc::new(output)));
    }
    Err(payload_fail(Code::UnsupportedStorage, format!(
            "{} attribute {:?} uses CSR storage not yet handled by the fixed-width interpolator",
            if pair.source_owner == AttributeOwner::Point { "point" } else { "vertex" },
            pair.name
        )))
}

fn interpolate_csr(
    cancel_value: Option<&cancel::T>,
    ancestry: &extract::Ancestry,
    view: &View,
    corner_count: usize,
    pair: &AttributePair,
) -> Result<Storage, Fail> {
    let left = pair.left.as_ref().map(attribute::view::storage).transpose()?;
    let right = pair.right.as_ref().map(attribute::view::storage).transpose()?;
    if pair.kind == 2 {
        let view_of = |storage: Option<&Storage>| -> Option<packed::int_array::view::View> {
            match storage {
                Some(Storage::IntArray(value)) => Some(packed::int_array::view::view(value)),
                _ => None,
            }
        };
        let (left, right) = (view_of(left), view_of(right));
        let mut lengths = vec![0_i64; corner_count];
        for corner in 0..corner_count {
            corner_poll(cancel_value, corner)?;
            let primitive = corner / 3;
            let values = if is_left(view, primitive) { left.as_ref() } else { right.as_ref() };
            if let Some(values) = values {
                let local = dominant_corner(view, corner);
                let source = source_index(view, pair.source_owner, primitive, local);
                lengths[corner] = values.offsets[source + 1] - values.offsets[source];
            }
        }
        let offsets = sorted_offsets(&lengths, "Boolean corner integer-array payload exceeds array limits")?;
        let mut values = vec![0_i64; offsets[corner_count] as usize];
        for corner in 0..corner_count {
            corner_poll(cancel_value, corner)?;
            let primitive = corner / 3;
            let source_values = if is_left(view, primitive) { left.as_ref() } else { right.as_ref() };
            if let Some(source_values) = source_values {
                let local = dominant_corner(view, corner);
                let source = source_index(view, pair.source_owner, primitive, local);
                let from = source_values.offsets[source] as usize;
                let length = lengths[corner] as usize;
                let to = offsets[corner] as usize;
                values[to..to + length].copy_from_slice(&source_values.values[from..from + length]);
            }
        }
        return Ok(Storage::IntArray(packed::int_array::view::create_validated_owned(offsets, values)));
    }
    if pair.kind == 3 {
        let view_of = |storage: Option<&Storage>| -> Option<packed::float_array::view::View> {
            match storage {
                Some(Storage::FloatArray(value)) => Some(packed::float_array::view::view(value)),
                _ => None,
            }
        };
        let (left, right) = (view_of(left), view_of(right));
        let mut lengths = vec![0_i64; corner_count];
        let mut interpolate = vec![0_u8; corner_count];
        for corner in 0..corner_count {
            corner_poll(cancel_value, corner)?;
            let primitive = corner / 3;
            let values = if is_left(view, primitive) { left.as_ref() } else { right.as_ref() };
            if let Some(values) = values {
                let a = source_index(view, pair.source_owner, primitive, 0);
                let b = source_index(view, pair.source_owner, primitive, 1);
                let c = source_index(view, pair.source_owner, primitive, 2);
                let al = values.offsets[a + 1] - values.offsets[a];
                let bl = values.offsets[b + 1] - values.offsets[b];
                let cl = values.offsets[c + 1] - values.offsets[c];
                if al == bl && al == cl {
                    lengths[corner] = al;
                    interpolate[corner] = 1;
                } else {
                    let local = dominant_corner(view, corner);
                    let source = [a, b, c][local];
                    lengths[corner] = values.offsets[source + 1] - values.offsets[source];
                }
            }
        }
        let offsets = sorted_offsets(&lengths, "Boolean corner float-array payload exceeds array limits")?;
        let mut values = vec![0.0_f64; offsets[corner_count] as usize];
        for corner in 0..corner_count {
            corner_poll(cancel_value, corner)?;
            let primitive = corner / 3;
            let source_values = if is_left(view, primitive) { left.as_ref() } else { right.as_ref() };
            let Some(source_values) = source_values else { continue };
            let a = source_index(view, pair.source_owner, primitive, 0);
            let b = source_index(view, pair.source_owner, primitive, 1);
            let c = source_index(view, pair.source_owner, primitive, 2);
            let output = offsets[corner] as usize;
            if interpolate[corner] != 0 {
                let wa = view.barycentric_a[corner];
                let wb = view.barycentric_b[corner];
                let wc = view.barycentric_c[corner];
                let ao = source_values.offsets[a] as usize;
                let bo = source_values.offsets[b] as usize;
                let co = source_values.offsets[c] as usize;
                for component in 0..lengths[corner] as usize {
                    values[output + component] = wa * source_values.values[ao + component]
                        + wb * source_values.values[bo + component]
                        + wc * source_values.values[co + component];
                }
            } else {
                let local = dominant_corner(view, corner);
                let source = [a, b, c][local];
                let from = source_values.offsets[source] as usize;
                let length = lengths[corner] as usize;
                values[output..output + length]
                    .copy_from_slice(&source_values.values[from..from + length]);
            }
        }
        return Ok(Storage::FloatArray(packed::float_array::view::create_validated_owned(offsets, values)));
    }
    interpolate_fixed(cancel_value, ancestry, view, corner_count, pair)
}

fn first_corners(vertex_points: &[i64], point_count: usize) -> Vec<i64> {
    let mut representatives = vec![-1_i64; point_count];
    for (corner, &point) in vertex_points.iter().enumerate() {
        if representatives[point as usize] < 0 {
            representatives[point as usize] = corner as i64;
        }
    }
    representatives
}

fn conflict_fail(name: &str, point: usize) -> Fail {
    payload_fail(
        Code::PointPayloadConflict,
        format!(
            "point payload {name:?} disagrees across incident Boolean corners at output point {point}; use Promote_to_vertex or split the schema"
        ),
    )
}

fn consolidate_to_points(
    tolerance: f64,
    name: &str,
    vertex_points: &[i64],
    point_count: usize,
    representatives: &[i64],
    storage: Storage,
) -> Result<Storage, Fail> {
    let rep = |point: usize| representatives[point] as usize;
    let planes_check = |planes: &[&[f64]], outputs: &[&[f64]]| -> Result<(), Fail> {
        for (corner, &point) in vertex_points.iter().enumerate() {
            let point = point as usize;
            for (plane, output) in planes.iter().zip(outputs) {
                if !float_equal(tolerance, output[point], plane[corner]) {
                    return Err(conflict_fail(name, point));
                }
            }
        }
        Ok(())
    };
    let gather = |plane: &[f64]| -> Vec<f64> { (0..point_count).map(|point| plane[rep(point)]).collect() };
    match storage {
        source @ (Storage::Float(_) | Storage::Float2(_) | Storage::Float3(_) | Storage::Float4(_)) => {
            let output = attribute::map_float_planes(&source, |plane| Ok(gather(plane)))?.expect("float storage has planes");
            let planes = attribute::float_planes(&source)?.expect("float source planes");
            let outputs = attribute::float_planes(&output)?.expect("float output planes");
            planes_check(&planes, &outputs)?;
            Ok(output)
        }
        Storage::Int(source) => {
            let output: Vec<i64> = (0..point_count).map(|point| source[rep(point)]).collect();
            for (corner, &point) in vertex_points.iter().enumerate() {
                if output[point as usize] != source[corner] {
                    return Err(conflict_fail(name, point as usize));
                }
            }
            Ok(Storage::Int(Arc::new(output)))
        }
        Storage::Text(source) => {
            let output: TextArray = (0..point_count).map(|point| &source[rep(point)]).collect();
            for (corner, &point) in vertex_points.iter().enumerate() {
                if output[point as usize] != source[corner] {
                    return Err(conflict_fail(name, point as usize));
                }
            }
            Ok(Storage::Text(Arc::new(output)))
        }
        Storage::IntArray(value) => {
            let source = packed::int_array::view::view(&value);
            let lengths: Vec<i64> = (0..point_count)
                .map(|point| {
                    let corner = rep(point);
                    source.offsets[corner + 1] - source.offsets[corner]
                })
                .collect();
            for (corner, &point) in vertex_points.iter().enumerate() {
                let (first, last) = (source.offsets[corner] as usize, source.offsets[corner + 1] as usize);
                let representative = rep(point as usize);
                let (other_first, other_last) =
                    (source.offsets[representative] as usize, source.offsets[representative + 1] as usize);
                if last - first != other_last - other_first {
                    return Err(conflict_fail(name, point as usize));
                }
                for slot in 0..last - first {
                    if source.values[first + slot] != source.values[other_first + slot] {
                        return Err(conflict_fail(name, point as usize));
                    }
                }
            }
            let offsets = sorted_offsets(&lengths, "Boolean point integer-array payload exceeds array limits")?;
            let mut values = vec![0_i64; offsets[point_count] as usize];
            for point in 0..point_count {
                let from = source.offsets[rep(point)] as usize;
                let length = lengths[point] as usize;
                let to = offsets[point] as usize;
                values[to..to + length].copy_from_slice(&source.values[from..from + length]);
            }
            Ok(Storage::IntArray(packed::int_array::view::create_validated_owned(offsets, values)))
        }
        Storage::FloatArray(value) => {
            let source = packed::float_array::view::view(&value);
            let lengths: Vec<i64> = (0..point_count)
                .map(|point| {
                    let corner = rep(point);
                    source.offsets[corner + 1] - source.offsets[corner]
                })
                .collect();
            for (corner, &point) in vertex_points.iter().enumerate() {
                let (first, last) = (source.offsets[corner] as usize, source.offsets[corner + 1] as usize);
                let representative = rep(point as usize);
                let (other_first, other_last) =
                    (source.offsets[representative] as usize, source.offsets[representative + 1] as usize);
                if last - first != other_last - other_first {
                    return Err(conflict_fail(name, point as usize));
                }
                for slot in 0..last - first {
                    if !float_equal(tolerance, source.values[first + slot], source.values[other_first + slot]) {
                        return Err(conflict_fail(name, point as usize));
                    }
                }
            }
            let offsets = sorted_offsets(&lengths, "Boolean point float-array payload exceeds array limits")?;
            let mut values = vec![0.0_f64; offsets[point_count] as usize];
            for point in 0..point_count {
                let from = source.offsets[rep(point)] as usize;
                let length = lengths[point] as usize;
                let to = offsets[point] as usize;
                values[to..to + length].copy_from_slice(&source.values[from..from + length]);
            }
            Ok(Storage::FloatArray(packed::float_array::view::create_validated_owned(offsets, values)))
        }
    }
}

fn create_attribute(pair: &AttributePair, owner: AttributeOwner, storage: Storage) -> Result<attribute::T, Fail> {
    attribute::create_owned(&pair.name, owner, storage)
        .map_err(|failure| Fail::Invalid(error::message(&failure).to_owned()))
}

fn group_source_index(view: &View, owner: group::Owner, primitive: usize, local: usize) -> usize {
    match owner {
        group::Owner::Point => view.primitive_source_points[primitive * 3 + local] as usize,
        group::Owner::Vertex => view.primitive_source_vertices[primitive * 3 + local] as usize,
        group::Owner::Primitive => unreachable!(),
    }
}

/// `Group.iter_ordered`: the stored order when there is one, else ascending membership.
fn ordered_members(value: &group::T) -> Vec<i64> {
    group::ordered_elements(value)
        .unwrap_or_else(|| (0..group::length(value)).filter(|&index| group::mem(index, value)).collect())
}

fn ranks(count: i64, source: Option<&group::T>) -> Vec<i64> {
    let mut output = vec![i64::MAX; count as usize];
    if let Some(source) = source {
        for (rank, element) in ordered_members(source).into_iter().enumerate() {
            output[element as usize] = rank as i64;
        }
    }
    output
}

fn init_group(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    owner: group::Owner,
    name: &str,
    length: i64,
    predicate: &(dyn Fn(i64) -> bool + Send + Sync),
) -> Result<group::T, Fail> {
    let cancelled = AtomicBool::new(false);
    let result = group::init(Some(grain), owner, name, length, &|element| {
        if cancel::poll(cancel_value, element, 4095).is_err() {
            cancelled.store(true, Ordering::Relaxed);
            return false;
        }
        predicate(element)
    });
    if cancelled.load(Ordering::Relaxed) {
        return Err(Fail::Cancelled);
    }
    Ok(result?)
}

#[allow(clippy::too_many_arguments)]
fn copy_group(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    view: &View,
    ancestry: &extract::Ancestry,
    vertex_points: &[i64],
    point_count: usize,
    representatives: &[i64],
    point_conflict: PointConflict,
    pair: &GroupPair,
) -> Result<group::T, Fail> {
    let corner_count = vertex_points.len();
    let mut membership = vec![0_u8; corner_count];
    for corner in 0..corner_count {
        corner_poll(cancel_value, corner)?;
        let primitive = corner / 3;
        let local = dominant_corner(view, corner);
        let source = if is_left(view, primitive) { pair.left_group.as_ref() } else { pair.right_group.as_ref() };
        if let Some(source) = source {
            let index = group_source_index(view, pair.source_group_owner, primitive, local);
            if group::mem(index as i64, source) {
                membership[corner] = 1;
            }
        }
    }
    let point_mode = pair.source_group_owner == group::Owner::Point
        && point_conflict == PointConflict::Reject;
    let (target_owner, target_count, target_membership) = if point_mode {
        let mut points = vec![0_u8; point_count];
        for (corner, &point) in vertex_points.iter().enumerate() {
            let value = membership[corner];
            let representative = representatives[point as usize];
            if corner as i64 == representative {
                points[point as usize] = value;
            } else if value != points[point as usize] {
                return Err(payload_fail(Code::PointGroupConflict, format!(
                        "point group {:?} disagrees across incident Boolean corners at output point {}; use Promote_to_vertex or split the group",
                        pair.group_name, point
                    )));
            }
        }
        (group::Owner::Point, point_count as i64, points)
    } else {
        (group::Owner::Vertex, corner_count as i64, membership)
    };
    let output = init_group(cancel_value, grain, target_owner, &pair.group_name, target_count, &|element| {
        target_membership[element as usize] != 0
    })?;
    let is_ordered = pair.left_group.as_ref().is_some_and(group::is_ordered)
        || pair.right_group.as_ref().is_some_and(group::is_ordered);
    if !is_ordered {
        return Ok(output);
    }
    let source_count = |geometry_value: &geometry::T| -> i64 {
        match pair.source_group_owner {
            group::Owner::Point => geometry::point_count(geometry_value),
            group::Owner::Vertex => geometry::vertex_count(geometry_value),
            group::Owner::Primitive => unreachable!(),
        }
    };
    let left_geometry = extract_view::left_geometry(ancestry);
    let right_geometry = extract_view::right_geometry(ancestry);
    let left_ranks = ranks(source_count(left_geometry), pair.left_group.as_ref());
    let right_ranks = ranks(source_count(right_geometry), pair.right_group.as_ref());
    let mut keyed: Vec<(i64, i64, i64)> = Vec::new();
    for element in 0..group::length(&output) {
        if !group::mem(element, &output) {
            continue;
        }
        let corner = if point_mode { representatives[element as usize] as usize } else { element as usize };
        let primitive = corner / 3;
        let local = dominant_corner(view, corner);
        let source = group_source_index(view, pair.source_group_owner, primitive, local);
        keyed.push(if is_left(view, primitive) {
            (0, left_ranks[source], element)
        } else {
            (1, right_ranks[source], element)
        });
    }
    keyed.sort_unstable();
    Ok(group::view::with_owned_order(keyed.into_iter().map(|(_, _, element)| element).collect(), &output)?)
}

fn copy_edge_group(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    view: &View,
    topology_value: &topology::T,
    index: &topology_index::T,
    pair: &EdgeGroupPair,
) -> Result<edge_group::T, Fail> {
    let output_index = topology_index::view::view(index);
    let cancelled = AtomicBool::new(false);
    let member = |edge: i64| -> bool {
        if cancel::poll(cancel_value, edge, 4095).is_err() {
            cancelled.store(true, Ordering::Relaxed);
            return false;
        }
        let first = output_index.edge_offsets[edge as usize];
        let last = output_index.edge_offsets[edge as usize + 1];
        for incident in first..last {
            let corner = output_index.edge_vertices[incident as usize] as usize;
            let source = view.edge_source_offsets[corner] as usize;
            let source_last = view.edge_source_offsets[corner + 1] as usize;
            for entry in source..source_last {
                let group = if view.edge_source_sides[entry] == 0 {
                    pair.left_edge_group.as_ref()
                } else {
                    pair.right_edge_group.as_ref()
                };
                if let Some(group) = group {
                    if edge_group::mem(view.edge_source_edges[entry], group) {
                        return true;
                    }
                }
            }
        }
        false
    };
    let result =
        edge_group::init(Some(grain), topology_value, index, &pair.edge_group_name, &member);
    if cancelled.load(Ordering::Relaxed) {
        return Err(Fail::Cancelled);
    }
    Ok(result?)
}

/// Interpolate point/vertex payload through exact corner barycentrics.
pub fn copy_points_and_vertices(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_conflict: PointConflict,
    point_tolerance: f64,
    ancestry: &extract::Ancestry,
    base: &geometry::T,
) -> Result<geometry::T, error::T> {
    support::check_positive(grain).map_err(|_| make(Code::InvalidParameter, "grain must be positive".to_owned()))?;
    if !point_tolerance.is_finite() || point_tolerance < 0.0 {
        return Err(make(
            Code::InvalidParameter,
            "point_tolerance must be finite and non-negative".to_owned(),
        ));
    }
    copy_points_inner(cancel_value, grain, point_conflict, point_tolerance, ancestry, base).map_err(|f| finish_fail(f))
}

fn copy_points_inner(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_conflict: PointConflict,
    point_tolerance: f64,
    ancestry: &extract::Ancestry,
    base: &geometry::T,
) -> Result<geometry::T, Fail> {
    cancel::check_opt(cancel_value)?;
    let ancestry_base = extract::geometry(ancestry);
    if packed::float3::data_id(geometry::positions(base))
        != packed::float3::data_id(geometry::positions(ancestry_base))
        || topology::data_id(geometry::topology(base))
            != topology::data_id(geometry::topology(ancestry_base))
    {
        return Err(Fail::Rdk(make(
            Code::GeometryMismatch,
            "corner payload target does not share the Boolean ancestry topology".to_owned(),
        )));
    }
    let topology_view = topology::view::view(geometry::topology(base))?;
    let primitive_count = geometry::primitive_count(base);
    let point_count = geometry::point_count(base) as usize;
    let corner_count = geometry::vertex_count(base);
    if corner_count != primitive_count * 3 || topology_view.vertex_points.len() as i64 != corner_count {
        return Err(invalid_fail("Boolean corner payload requires triangular extraction topology"));
    }
    let vertex_points: &[i64] = &topology_view.vertex_points;
    let corner_count = corner_count as usize;
    let representatives = first_corners(vertex_points, point_count);
    if representatives.iter().any(|&corner| corner < 0) {
        return Err(invalid_fail("Boolean corner payload found an unreferenced output point"));
    }
    let left = extract_view::left_geometry(ancestry);
    let right = extract_view::right_geometry(ancestry);
    let view = extract_view::ancestry_view(ancestry);
    let point_attributes = collect_attributes(AttributeOwner::Point, left, right)?;
    let vertex_attributes = collect_attributes(AttributeOwner::Vertex, left, right)?;
    let point_groups = collect_groups(group::Owner::Point, left, right);
    let vertex_groups = collect_groups(group::Owner::Vertex, left, right);
    let edge_groups = collect_edge_groups(left, right);
    if point_conflict == PointConflict::PromoteToVertex {
        for pair in &point_attributes {
            if vertex_attributes.iter().any(|other| other.name == pair.name) {
                return Err(payload_fail(Code::PromotionCollision, format!(
                        "point attribute {:?} cannot be promoted because a vertex attribute has the same name",
                        pair.name
                    )));
            }
        }
        for pair in &point_groups {
            if vertex_groups.iter().any(|other| other.group_name == pair.group_name) {
                return Err(payload_fail(Code::PromotionCollision, format!(
                        "point group {:?} cannot be promoted because a vertex group has the same name",
                        pair.group_name
                    )));
            }
        }
    }
    let mut attributes: Vec<attribute::T> = Vec::new();
    for pair in point_attributes.iter().chain(vertex_attributes.iter()) {
        let corner_storage = interpolate_csr(cancel_value, ancestry, &view, corner_count, pair)?;
        match (pair.source_owner, point_conflict) {
            (AttributeOwner::Point, PointConflict::Reject) => {
                let storage = consolidate_to_points(
                    point_tolerance,
                    &pair.name,
                    vertex_points,
                    point_count,
                    &representatives,
                    corner_storage,
                )?;
                attributes.push(create_attribute(pair, AttributeOwner::Point, storage)?);
            }
            _ => attributes.push(create_attribute(pair, AttributeOwner::Vertex, corner_storage)?),
        }
    }
    let mut groups: Vec<group::T> = Vec::new();
    for pair in point_groups.iter().chain(vertex_groups.iter()) {
        groups.push(copy_group(
            cancel_value,
            grain,
            &view,
            ancestry,
            vertex_points,
            point_count,
            &representatives,
            point_conflict,
            pair,
        )?);
    }
    let topology_value = geometry::topology(base);
    let topology_idx = topology_index::create(cancel_value, topology_value)?;
    let mut copied_edge_groups: Vec<edge_group::T> = Vec::new();
    for pair in &edge_groups {
        copied_edge_groups.push(copy_edge_group(
            cancel_value,
            grain,
            &view,
            topology_value,
            &topology_idx,
            pair,
        )?);
    }
    let mut all_attributes: Vec<attribute::T> = geometry::attributes(base)
        .into_iter()
        .filter(|item| matches!(attribute::owner(item), AttributeOwner::Primitive | AttributeOwner::Detail))
        .collect();
    all_attributes.extend(attributes);
    let mut all_groups: Vec<group::T> = geometry::groups(base)
        .into_iter()
        .filter(|item| group::owner(item) == group::Owner::Primitive)
        .collect();
    all_groups.extend(groups);
    let mut all_edge_groups: Vec<edge_group::T> = geometry::edge_groups(base)
        .into_iter()
        .filter(|item| !edge_groups.iter().any(|pair| pair.edge_group_name == edge_group::name(item)))
        .collect();
    all_edge_groups.extend(copied_edge_groups);
    geometry::create(
        geometry::positions(base).clone(),
        topology_value.clone(),
        Some(all_attributes),
        Some(all_groups),
        Some(all_edge_groups),
    )
    .map_err(|failure| Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())))
}

fn selected_primitive(ancestry: &extract::Ancestry, primitive: usize) -> (bool, usize) {
    (
        extract::primitive_side(ancestry, primitive as i64) == Side::Left,
        extract::primitive_face(ancestry, primitive as i64) as usize,
    )
}

fn make_attribute(pair: &AttributePair, storage: Storage) -> Result<attribute::T, Fail> {
    create_attribute(pair, AttributeOwner::Primitive, storage)
}

fn copy_attribute(
    cancel_value: Option<&cancel::T>,
    ancestry: &extract::Ancestry,
    primitive_count: usize,
    pair: &AttributePair,
) -> Result<attribute::T, Fail> {
    let left = pair.left.as_ref().map(attribute::view::storage).transpose()?;
    let right = pair.right.as_ref().map(attribute::view::storage).transpose()?;
    if pair.kind == 0 || (pair.kind >= 4 && pair.kind <= 6) {
        let like = left.or(right).expect("attribute pair has a source");
        let planes_of = planes_of_storage;
        let left_planes = planes_of(left)?;
        let right_planes = planes_of(right)?;
        let plane_count = attribute::float_planes(like)?.expect("float planes").len();
        let mut output: Vec<Vec<f64>> = (0..plane_count).map(|_| vec![0.0; primitive_count]).collect();
        for primitive in 0..primitive_count {
            corner_poll(cancel_value, primitive)?;
            let (is_left, index) = selected_primitive(ancestry, primitive);
            let values = if is_left { left_planes.as_ref() } else { right_planes.as_ref() };
            if let Some(values) = values {
                for (p, plane) in output.iter_mut().enumerate() {
                    plane[primitive] = values[p][index];
                }
            }
        }
        return make_attribute(pair, attribute::of_float_planes(like, output)?);
    }
    if pair.kind == 1 {
        let int_of = int_of_storage;
        let (left, right) = (int_of(left), int_of(right));
        let mut output = vec![0_i64; primitive_count];
        for primitive in 0..primitive_count {
            corner_poll(cancel_value, primitive)?;
            let (is_left, index) = selected_primitive(ancestry, primitive);
            if let Some(values) = if is_left { left } else { right } {
                output[primitive] = values[index];
            }
        }
        return make_attribute(pair, Storage::Int(Arc::new(output)));
    }
    if pair.kind == 7 {
        let text_of = text_of_storage;
        let (left, right) = (text_of(left), text_of(right));
        let mut output = TextArray::new();
        for primitive in 0..primitive_count {
            corner_poll(cancel_value, primitive)?;
            let (is_left, index) = selected_primitive(ancestry, primitive);
            output.push(match if is_left { left } else { right } {
                Some(values) => &values[index],
                None => &[],
            });
        }
        return make_attribute(pair, Storage::Text(Arc::new(output)));
    }
    if pair.kind == 2 {
        let view_of = |storage: Option<&Storage>| -> Option<packed::int_array::view::View> {
            match storage {
                Some(Storage::IntArray(value)) => Some(packed::int_array::view::view(value)),
                _ => None,
            }
        };
        let (left, right) = (view_of(left), view_of(right));
        let mut lengths = vec![0_i64; primitive_count];
        for primitive in 0..primitive_count {
            corner_poll(cancel_value, primitive)?;
            let (is_left, index) = selected_primitive(ancestry, primitive);
            if let Some(values) = if is_left { left.as_ref() } else { right.as_ref() } {
                lengths[primitive] = values.offsets[index + 1] - values.offsets[index];
            }
        }
        let offsets =
            sorted_offsets(&lengths, "Boolean primitive integer-array payload exceeds array limits")?;
        let mut values = vec![0_i64; offsets[primitive_count] as usize];
        for primitive in 0..primitive_count {
            corner_poll(cancel_value, primitive)?;
            let (is_left, index) = selected_primitive(ancestry, primitive);
            if let Some(source) = if is_left { left.as_ref() } else { right.as_ref() } {
                let from = source.offsets[index] as usize;
                let length = lengths[primitive] as usize;
                let to = offsets[primitive] as usize;
                values[to..to + length].copy_from_slice(&source.values[from..from + length]);
            }
        }
        return make_attribute(
            pair,
            Storage::IntArray(packed::int_array::view::create_validated_owned(offsets, values)),
        );
    }
    if pair.kind == 3 {
        let view_of = |storage: Option<&Storage>| -> Option<packed::float_array::view::View> {
            match storage {
                Some(Storage::FloatArray(value)) => Some(packed::float_array::view::view(value)),
                _ => None,
            }
        };
        let (left, right) = (view_of(left), view_of(right));
        let mut lengths = vec![0_i64; primitive_count];
        for primitive in 0..primitive_count {
            corner_poll(cancel_value, primitive)?;
            let (is_left, index) = selected_primitive(ancestry, primitive);
            if let Some(values) = if is_left { left.as_ref() } else { right.as_ref() } {
                lengths[primitive] = values.offsets[index + 1] - values.offsets[index];
            }
        }
        let offsets =
            sorted_offsets(&lengths, "Boolean primitive float-array payload exceeds array limits")?;
        let mut values = vec![0.0_f64; offsets[primitive_count] as usize];
        for primitive in 0..primitive_count {
            corner_poll(cancel_value, primitive)?;
            let (is_left, index) = selected_primitive(ancestry, primitive);
            if let Some(source) = if is_left { left.as_ref() } else { right.as_ref() } {
                let from = source.offsets[index] as usize;
                let length = lengths[primitive] as usize;
                let to = offsets[primitive] as usize;
                values[to..to + length].copy_from_slice(&source.values[from..from + length]);
            }
        }
        return make_attribute(
            pair,
            Storage::FloatArray(packed::float_array::view::create_validated_owned(offsets, values)),
        );
    }
    Err(Fail::Invalid(format!(
        "primitive attribute {:?} has an inconsistent Boolean payload schema",
        pair.name
    )))
}

/// Copy the union of primitive attribute and primitive group schemas from the two exact source
/// operands; a field missing on a primitive's operand receives its storage default.
pub fn copy_primitives(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    ancestry: &extract::Ancestry,
) -> Result<geometry::T, error::T> {
    support::check_positive(grain).map_err(|_| make(Code::InvalidParameter, "grain must be positive".to_owned()))?;
    copy_primitives_inner(cancel_value, grain, ancestry).map_err(|f| finish_fail(f))
}

fn copy_primitives_inner(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    ancestry: &extract::Ancestry,
) -> Result<geometry::T, Fail> {
    cancel::check_opt(cancel_value)?;
    let left = extract_view::left_geometry(ancestry);
    let right = extract_view::right_geometry(ancestry);
    let base = extract::geometry(ancestry);
    let attribute_pairs = collect_attributes(AttributeOwner::Primitive, left, right)?;
    let group_pairs = collect_groups(group::Owner::Primitive, left, right);
    let primitive_count = geometry::primitive_count(base);
    let mut attributes: Vec<attribute::T> = Vec::new();
    for pair in &attribute_pairs {
        attributes.push(copy_attribute(cancel_value, ancestry, primitive_count as usize, pair)?);
    }
    let mut groups: Vec<group::T> = Vec::new();
    for pair in &group_pairs {
        let output = init_group(
            cancel_value,
            grain,
            group::Owner::Primitive,
            &pair.group_name,
            primitive_count,
            &|primitive| {
                let (is_left, index) = selected_primitive(ancestry, primitive as usize);
                match if is_left { pair.left_group.as_ref() } else { pair.right_group.as_ref() } {
                    None => false,
                    Some(source) => group::mem(index as i64, source),
                }
            },
        )?;
        let ordered = pair.left_group.as_ref().is_some_and(group::is_ordered)
            || pair.right_group.as_ref().is_some_and(group::is_ordered);
        if !ordered {
            groups.push(output);
            continue;
        }
        let left_ranks = ranks(geometry::primitive_count(left), pair.left_group.as_ref());
        let right_ranks = ranks(geometry::primitive_count(right), pair.right_group.as_ref());
        let mut keyed: Vec<(i64, i64, i64)> = Vec::new();
        for primitive in 0..group::length(&output) {
            if group::mem(primitive, &output) {
                let face = extract::primitive_face(ancestry, primitive) as usize;
                keyed.push(match extract::primitive_side(ancestry, primitive) {
                    Side::Left => (0, left_ranks[face], primitive),
                    Side::Right => (1, right_ranks[face], primitive),
                });
            }
        }
        keyed.sort_unstable();
        groups.push(group::view::with_owned_order(
            keyed.into_iter().map(|(_, _, primitive)| primitive).collect(),
            &output,
        )?);
    }
    let mut all_attributes: Vec<attribute::T> = geometry::attributes(base)
        .into_iter()
        .filter(|item| attribute::owner(item) != AttributeOwner::Primitive)
        .collect();
    all_attributes.extend(attributes);
    let mut all_groups: Vec<group::T> = geometry::groups(base)
        .into_iter()
        .filter(|item| group::owner(item) != group::Owner::Primitive)
        .collect();
    all_groups.extend(groups);
    geometry::create(
        geometry::positions(base).clone(),
        geometry::topology(base).clone(),
        Some(all_attributes),
        Some(all_groups),
        Some(geometry::edge_groups(base)),
    )
    .map_err(|failure| Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())))
}

fn planes_of_storage(storage: Option<&Storage>) -> Result<Option<Vec<&[f64]>>, Fail> {
    match storage {
        None => Ok(None),
        Some(storage) => Ok(attribute::float_planes(storage)?),
    }
}

fn int_of_storage(storage: Option<&Storage>) -> Option<&Vec<i64>> {
    match storage {
        Some(Storage::Int(values)) => Some(values),
        _ => None,
    }
}

fn text_of_storage(storage: Option<&Storage>) -> Option<&TextArray> {
    match storage {
        Some(Storage::Text(values)) => Some(values),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boolean_solid::{self, Operation, Treatment};
    use std::sync::Mutex;

    fn cube_quads() -> geometry::T {
        let points = [
            (0., 0., 0.),
            (1., 0., 0.),
            (1., 1., 0.),
            (0., 1., 0.),
            (0., 0., 1.),
            (1., 0., 1.),
            (1., 1., 1.),
            (0., 1., 1.),
        ];
        let x = points.iter().map(|p: &(f64, f64, f64)| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let topo = topology::polygons_owned(
            8,
            vec![0, 3, 2, 1, 4, 5, 6, 7, 0, 1, 5, 4, 3, 7, 6, 2, 0, 4, 7, 3, 1, 2, 6, 5],
            vec![0, 4, 8, 12, 16, 20, 24],
        )
        .unwrap();
        geometry::create(packed::float3::view::of_owned_exn(x, y, z).unwrap(), topo, None, None, None)
            .unwrap()
    }

    fn find_storage(owner: AttributeOwner, name: &str, geometry_value: &geometry::T) -> Storage {
        let found = geometry::find_attribute(owner, name, geometry_value)
            .unwrap_or_else(|| panic!("missing attribute {name}"));
        attribute::view::storage(&found).unwrap().clone()
    }

    type Signature = (Vec<i64>, Vec<bool>);

    // test_surface_cut_payload
    fn run(domains: i64) -> Signature {
        let left = cube_quads();
        let positions = packed::float3::view::of_owned_exn(
            vec![-1., 2., -1., 2.],
            vec![0.5, 0.5, 0.5, 0.5],
            vec![0.25, 0.25, 0.75, 0.75],
        )
        .unwrap();
        let topo = topology::polygons_owned(4, vec![0, 1, 2, 1, 3, 2], vec![0, 3, 6]).unwrap();
        let cut_id =
            attribute::create_owned("cut_id", AttributeOwner::Primitive, Storage::Int(Arc::new(vec![42, 43]))).unwrap();
        let cut_group =
            group::init(None, group::Owner::Primitive, "cut_surface", 2, &|_| true).unwrap();
        let right =
            geometry::create(positions, topo, Some(vec![cut_id]), Some(vec![cut_group]), None).unwrap();
        let slot: Mutex<Option<Signature>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            let prepared = boolean_solid::prepare(
                None,
                None,
                None,
                None,
                Some(Treatment::Surface),
                1,
                &left,
                &right,
            )
            .unwrap();
            let ancestry = boolean_solid::extract_product_with_ancestry(
                None,
                None,
                None,
                None,
                Operation::Difference,
                &prepared,
            )
            .unwrap();
            let output = copy_primitives(None, 1, &ancestry).unwrap();
            let ids = match find_storage(AttributeOwner::Primitive, "cut_id", &output) {
                Storage::Int(values) => values,
                _ => panic!("cut_id storage changed"),
            };
            let cut = geometry::find_group(group::Owner::Primitive, "cut_surface", &output).unwrap();
            let mut sheet_count = 0;
            for primitive in 0..geometry::primitive_count(&output) {
                let member = group::mem(primitive, &cut);
                let id = ids[primitive as usize];
                match extract::primitive_side(&ancestry, primitive) {
                    Side::Left => assert!(id == 0 && !member, "solid side inherited surface-only cut payload"),
                    Side::Right => {
                        sheet_count += 1;
                        assert!(
                            (id == 42 || id == 43) && member,
                            "paired surface cut wall lost its primitive payload"
                        );
                    }
                }
            }
            assert!(sheet_count > 0 && sheet_count & 1 == 0, "surface cut payload did not cover paired walls");
            *slot.lock().unwrap() = Some((
                ids.to_vec(),
                (0..group::length(&cut)).map(|element| group::mem(element, &cut)).collect(),
            ));
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }

    #[test]
    fn surface_cut_payload() {
        assert!(run(1) == run(4), "surface cut payload differs between one and four domains");
    }

    #[test]
    fn invalid_parameters_and_geometry_mismatch() {
        let left = cube_quads();
        let prepared =
            boolean_solid::prepare(None, None, None, None, None, 1, &left, &cube_quads()).unwrap();
        let ancestry = boolean_solid::extract_product_with_ancestry(
            None,
            None,
            None,
            None,
            Operation::Union,
            &prepared,
        )
        .unwrap();
        assert_eq!(error::code(&copy_primitives(None, 0, &ancestry).err().unwrap()), Code::InvalidParameter);
        assert_eq!(
            error::code(
                &copy_points_and_vertices(None, 1, PointConflict::Reject, -1.0, &ancestry, &left)
                    .err()
                    .unwrap()
            ), Code::InvalidParameter
        );
        assert_eq!(
            error::code(
                &copy_points_and_vertices(None, 1, PointConflict::Reject, 0.0, &ancestry, &left)
                    .err()
                    .unwrap()
            ), Code::GeometryMismatch
        );
    }
}
