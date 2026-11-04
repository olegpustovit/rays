//! Attribute transfer: spatial (points, primitives), detail, surface, vertices and multi-owner.
use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use rdk_core::{attribute, attribute_pattern, cancel, error, geometry, group, packed, support, topology};
use rdk_geom::{spatial_index, surface_index};

use crate::attribute_ops::{is_blank, make_error, map_ints, map_planes, map_texts};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kernel {
    Links,
    RenderMan,
    Hart,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mode {
    Nearest,
    InverseDistance { neighbors: i64, power: f64 },
    Kernel { neighbors: i64, radius: f64, kernel: Kernel },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unmatched {
    KeepTarget,
    DefaultValue,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Falloff {
    Linear,
    Smoothstep,
    Uniform(f64),
}

pub type SurfaceFalloff = Falloff;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SurfaceAttribute {
    pub source_owner: attribute::Owner,
    pub source_name: String,
    pub target_name: String,
}

#[derive(Clone)]
pub struct SpatialParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub names: Option<Vec<String>>,
    pub pattern: Option<String>,
    pub mode: Mode,
    pub max_distance: Option<f64>,
    pub blend_width: f64,
    pub falloff: Falloff,
    pub unmatched: Unmatched,
    pub source_elements: Option<group::T>,
    pub target_elements: Option<group::T>,
    pub source: geometry::T,
    pub target: geometry::T,
}

#[derive(Clone)]
pub struct DetailParams {
    pub names: Option<Vec<String>>,
    pub pattern: Option<String>,
    pub source: geometry::T,
    pub target: geometry::T,
}

#[derive(Clone)]
pub struct SurfaceParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub max_distance: Option<f64>,
    pub blend_width: f64,
    pub falloff: SurfaceFalloff,
    pub unmatched: Unmatched,
    pub target_owner: attribute::Owner,
    pub distance_attribute: Option<String>,
    pub source_primitives: Option<group::T>,
    pub source_vertices: Option<group::T>,
    pub source_vertex_selection: surface_index::VertexSelection,
    pub target_elements: Option<group::T>,
    pub attributes: Vec<SurfaceAttribute>,
    pub source: geometry::T,
    pub target: geometry::T,
}

#[derive(Clone)]
pub struct VerticesParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub names: Option<Vec<String>>,
    pub pattern: Option<String>,
    pub max_distance: Option<f64>,
    pub blend_width: f64,
    pub falloff: Falloff,
    pub unmatched: Unmatched,
    pub source_primitives: Option<group::T>,
    pub source_vertices: Option<group::T>,
    pub source_vertex_selection: surface_index::VertexSelection,
    pub target_vertices: Option<group::T>,
    pub source: geometry::T,
    pub target: geometry::T,
}

#[derive(Clone)]
pub struct AllParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub point_pattern: Option<String>,
    pub vertex_pattern: Option<String>,
    pub primitive_pattern: Option<String>,
    pub detail_pattern: Option<String>,
    pub mode: Mode,
    pub max_distance: Option<f64>,
    pub blend_width: f64,
    pub falloff: Falloff,
    pub unmatched: Unmatched,
    pub source: geometry::T,
    pub target: geometry::T,
}

/// An `Error msg` of the OCaml API (coded `invalid_transfer`).
fn fail(message: &str) -> error::T {
    make_error("attribute_transfer", "invalid_transfer", message)
}

/// An `invalid_arg msg` of the OCaml API (coded `invalid_position`).
fn invalid(message: &str) -> error::T {
    make_error("attribute_transfer", "invalid_position", message)
}

/// The `Error.guard` of an entry point: cancellation becomes its own message and the operation name
/// is stamped on; `string`-result failures stay `invalid_transfer`, `Invalid_argument` stays
/// `invalid_position`.
fn finish(result: Result<geometry::T, error::T>, operation: &str, cancelled: &str) -> Result<geometry::T, error::T> {
    result.map_err(|failure| {
        let code = error::code(&failure);
        if code == "cancelled" {
            make_error(operation, "cancelled", cancelled)
        } else if code == "invalid_position" {
            make_error(operation, "invalid_position", error::message(&failure))
        } else {
            make_error(operation, "invalid_transfer", error::message(&failure))
        }
    })
}

/// `Error.get` inside a guard: any non-cancelled failure becomes `invalid_transfer`.
fn flatten(result: Result<geometry::T, error::T>) -> Result<geometry::T, error::T> {
    result.map_err(|failure| {
        if error::code(&failure) == "cancelled" {
            failure
        } else {
            fail(error::message(&failure))
        }
    })
}

fn owner_count(geometry_value: &geometry::T, owner: attribute::Owner) -> i64 {
    match owner {
        attribute::Owner::Point => geometry::point_count(geometry_value),
        attribute::Owner::Vertex => geometry::vertex_count(geometry_value),
        attribute::Owner::Primitive => geometry::primitive_count(geometry_value),
        attribute::Owner::Detail => 1,
    }
}

fn owner_label(owner: attribute::Owner) -> &'static str {
    match owner {
        attribute::Owner::Point => "point",
        attribute::Owner::Vertex => "vertex",
        attribute::Owner::Primitive => "primitive",
        attribute::Owner::Detail => "detail",
    }
}

fn same_kind(left: &attribute::T, right: &attribute::T) -> bool {
    attribute::kind_name(left) == attribute::kind_name(right)
}

fn selected_attributes(
    operation: &str,
    owner: attribute::Owner,
    names: Option<&Vec<String>>,
    pattern: Option<&String>,
    source: &geometry::T,
) -> Result<Vec<attribute::T>, error::T> {
    let available: Vec<attribute::T> = geometry::attributes(source)
        .into_iter()
        .filter(|item| attribute::owner(item) == owner)
        .collect();
    match (names, pattern) {
        (Some(_), Some(_)) => Err(fail(&format!("{operation}: names and pattern are mutually exclusive"))),
        (None, None) => Ok(available),
        (None, Some(pattern)) => {
            let compiled = attribute_pattern::compile(pattern).map_err(|failure| fail(error::message(&failure)))?;
            Ok(available
                .into_iter()
                .filter(|item| attribute_pattern::matches(&compiled, attribute::name(item)))
                .collect())
        }
        (Some(names), None) => {
            let mut seen: HashSet<&str> = HashSet::new();
            let mut result = Vec::new();
            for name in names {
                if is_blank(name) {
                    return Err(fail(&format!("{operation}: attribute names must not be empty")));
                }
                if name == "P" && owner == attribute::Owner::Point {
                    return Err(fail(&format!(
                        "{operation}: canonical P cannot be transferred as an ordinary attribute"
                    )));
                }
                if !seen.insert(name.as_str()) {
                    return Err(fail(&format!("{operation}: duplicate attribute name {name}")));
                }
                match geometry::find_attribute(owner, name, source) {
                    None => {
                        return Err(fail(&format!(
                            "{operation}: missing source {} attribute {name}",
                            owner_label(owner)
                        )))
                    }
                    Some(found) => result.push(found),
                }
            }
            Ok(result)
        }
    }
}

fn validate_target_kinds(
    operation: &str,
    owner: attribute::Owner,
    attributes: &[attribute::T],
    target: &geometry::T,
) -> Result<(), error::T> {
    for source_attribute in attributes {
        match geometry::find_attribute(owner, attribute::name(source_attribute), target) {
            None => {}
            Some(target_attribute) if same_kind(source_attribute, &target_attribute) => {}
            Some(target_attribute) => {
                return Err(fail(&format!(
                    "{operation}: target attribute {} has {} storage, source has {}",
                    attribute::name(source_attribute),
                    attribute::kind_name(&target_attribute),
                    attribute::kind_name(source_attribute)
                )))
            }
        }
    }
    Ok(())
}

fn validate_sampled_storage(operation: &str, attributes: &[attribute::T]) -> Result<(), error::T> {
    for item in attributes {
        if matches!(
            attribute::View::storage(item)?,
            attribute::Storage::IntArray(_) | attribute::Storage::FloatArray(_)
        ) {
            return Err(fail(&format!(
                "{operation}: sampled transfer of array attribute {} requires an explicit array policy",
                attribute::name(item)
            )));
        }
    }
    Ok(())
}

fn reset_selected(selection: Option<&group::T>, reset: &mut dyn FnMut(usize)) {
    if let Some(value) = selection {
        for element in 0..group::length(value) {
            if group::mem(element, value) {
                reset(element as usize);
            }
        }
    }
}

fn existing_storage(
    unmatched: Unmatched,
    selection: Option<&group::T>,
    target_attribute: &Option<attribute::T>,
) -> Option<attribute::T> {
    match (unmatched, target_attribute) {
        (Unmatched::KeepTarget, Some(item)) => Some(item.clone()),
        (Unmatched::DefaultValue, Some(item)) if selection.is_some() => Some(item.clone()),
        _ => None,
    }
}

fn initial_float(
    selection: Option<&group::T>,
    unmatched: Unmatched,
    target_attribute: &Option<attribute::T>,
    count: usize,
) -> Result<Vec<f64>, error::T> {
    if let Some(item) = existing_storage(unmatched, selection, target_attribute) {
        if let attribute::Storage::Float(values) = attribute::View::storage(&item)? {
            let mut output = values.clone();
            if unmatched == Unmatched::DefaultValue {
                reset_selected(selection, &mut |element| output[element] = 0.0);
            }
            return Ok(output);
        }
    }
    Ok(vec![0.0; count])
}

fn initial_int(
    selection: Option<&group::T>,
    unmatched: Unmatched,
    target_attribute: &Option<attribute::T>,
    count: usize,
) -> Result<Vec<i64>, error::T> {
    if let Some(item) = existing_storage(unmatched, selection, target_attribute) {
        if let attribute::Storage::Int(values) = attribute::View::storage(&item)? {
            let mut output = values.clone();
            if unmatched == Unmatched::DefaultValue {
                reset_selected(selection, &mut |element| output[element] = 0);
            }
            return Ok(output);
        }
    }
    Ok(vec![0; count])
}

fn initial_text(
    selection: Option<&group::T>,
    unmatched: Unmatched,
    target_attribute: &Option<attribute::T>,
    count: usize,
) -> Result<Vec<String>, error::T> {
    if let Some(item) = existing_storage(unmatched, selection, target_attribute) {
        if let attribute::Storage::Text(values) = attribute::View::storage(&item)? {
            let mut output = values.clone();
            if unmatched == Unmatched::DefaultValue {
                reset_selected(selection, &mut |element| output[element] = String::new());
            }
            return Ok(output);
        }
    }
    Ok(vec![String::new(); count])
}

/// Output planes for a Float..Float4 storage `like`: copies of the existing target's planes
/// (selected elements zeroed for `DefaultValue`) or zeros.
fn initial_planes(
    selection: Option<&group::T>,
    unmatched: Unmatched,
    target_attribute: &Option<attribute::T>,
    like: &attribute::Storage,
    count: usize,
) -> Result<Vec<Vec<f64>>, error::T> {
    if let Some(item) = existing_storage(unmatched, selection, target_attribute) {
        if let Some(planes) = attribute::float_planes(attribute::View::storage(&item)?)? {
            let mut planes: Vec<Vec<f64>> = planes.iter().map(|plane| plane.to_vec()).collect();
            if unmatched == Unmatched::DefaultValue {
                reset_selected(selection, &mut |element| {
                    for plane in planes.iter_mut() {
                        plane[element] = 0.0;
                    }
                });
            }
            return Ok(planes);
        }
    }
    let width = attribute::float_planes(like)?.map_or(0, |planes| planes.len());
    Ok((0..width).map(|_| vec![0.0; count]).collect())
}

/// Renormalize the xyz planes of the elements `keep` accepts.
fn normalize_selected(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    keep: &(dyn Fn(i64) -> bool + Sync),
    planes: &[Vec<f64>],
) -> Result<Vec<Vec<f64>>, error::T> {
    let init: Vec<&[f64]> = planes.iter().map(|plane| plane.as_slice()).collect();
    map_planes(cancel_token, grain, &init, &|element, slot| {
        if keep(element) {
            let length = ((slot[0] * slot[0]) + (slot[1] * slot[1]) + (slot[2] * slot[2])).sqrt();
            if length > 1e-20 {
                slot[0] /= length;
                slot[1] /= length;
                slot[2] /= length;
            }
        }
        Ok(())
    })
}

fn group_owner_of_attribute(owner: attribute::Owner) -> Result<group::Owner, error::T> {
    match owner {
        attribute::Owner::Point => Ok(group::Owner::Point),
        attribute::Owner::Vertex => Ok(group::Owner::Vertex),
        attribute::Owner::Primitive => Ok(group::Owner::Primitive),
        attribute::Owner::Detail => Err(invalid("detail attributes do not have element groups")),
    }
}

fn validate_transfer_falloff(operation: &str, falloff: Falloff) -> Result<(), error::T> {
    match falloff {
        Falloff::Linear | Falloff::Smoothstep => Ok(()),
        Falloff::Uniform(bias) if bias.is_finite() && (0.0..=1.0).contains(&bias) => Ok(()),
        Falloff::Uniform(_) => Err(fail(&format!("{operation}: uniform bias must be finite and in [0, 1]"))),
    }
}

fn transfer_distance_window(
    operation: &str,
    blend_width: f64,
    falloff: Falloff,
    max_distance: Option<f64>,
) -> Result<(f64, f64), error::T> {
    if !blend_width.is_finite() || blend_width < 0.0 {
        return Err(fail(&format!("{operation}: blend width must be finite and non-negative")));
    }
    validate_transfer_falloff(operation, falloff)?;
    match max_distance {
        None if blend_width > 0.0 => Err(fail(&format!("{operation}: blend width requires a maximum distance"))),
        None => Ok((f64::INFINITY, f64::INFINITY)),
        Some(value) if value.is_finite() && value >= 0.0 && value <= f64::MAX.sqrt() => {
            let outer = value + blend_width;
            if !outer.is_finite() || outer > f64::MAX.sqrt() {
                Err(fail(&format!("{operation}: distance plus blend width must be safely squarable")))
            } else {
                Ok((value * value, outer * outer))
            }
        }
        Some(_) => Err(fail(&format!(
            "{operation}: maximum distance must be finite, non-negative, and safely squarable"
        ))),
    }
}

fn transfer_influence(falloff: Falloff, threshold_squared: f64, blend_width: f64, distance_squared: f64) -> f64 {
    if blend_width == 0.0 || distance_squared <= threshold_squared {
        1.0
    } else {
        let threshold = threshold_squared.sqrt();
        let remaining = 1.0 - ((distance_squared.sqrt() - threshold) / blend_width);
        let remaining = 0.0_f64.max(1.0_f64.min(remaining));
        match falloff {
            Falloff::Linear => remaining,
            Falloff::Smoothstep => remaining * remaining * (3.0 - (2.0 * remaining)),
            Falloff::Uniform(bias) => bias,
        }
    }
}

fn transfer_kernel_weight(kernel: Kernel, t: f64) -> f64 {
    if t >= 1.0 {
        return 0.0;
    }
    let t = 0.0_f64.max(t);
    match kernel {
        Kernel::Links => {
            if t < 1.0 / 3.0 {
                1.0 - (3.0 * t * t)
            } else {
                1.5 * (1.0 - t) * (1.0 - t)
            }
        }
        Kernel::RenderMan => {
            let one_minus_square = 1.0 - (t * t);
            one_minus_square * one_minus_square * one_minus_square
        }
        Kernel::Hart => {
            let t2 = t * t;
            let t3 = t * t * t;
            1.0 - (10.0 * t3) + (15.0 * t2 * t2) - (6.0 * t2 * t3)
        }
    }
}

fn primitive_barycenters(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<packed::Float3::T, error::T> {
    let topology_view = topology::View::view(geometry::topology(geometry_value))?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let count = geometry::primitive_count(geometry_value) as usize;
    let zeros = vec![0.0_f64; count];
    let planes = map_planes(cancel_token, grain, &[&zeros, &zeros, &zeros], &|primitive, slot| {
        let first = topology_view.primitive_offsets[primitive as usize];
        let last = topology_view.primitive_offsets[primitive as usize + 1];
        let (mut sx, mut sy, mut sz) = (0.0_f64, 0.0_f64, 0.0_f64);
        for vertex in first..last {
            let point = topology_view.vertex_points[vertex as usize] as usize;
            sx += positions.x[point];
            sy += positions.y[point];
            sz += positions.z[point];
        }
        let scale = 1.0 / (last - first) as f64;
        slot[0] = sx * scale;
        slot[1] = sy * scale;
        slot[2] = sz * scale;
        Ok(())
    })?;
    let mut planes = planes.into_iter();
    packed::Float3::View::of_owned_exn(
        planes.next().unwrap_or_default(),
        planes.next().unwrap_or_default(),
        planes.next().unwrap_or_default(),
    )
}

fn install_attributes_batch(attributes: Vec<attribute::T>, target: &geometry::T) -> Result<geometry::T, error::T> {
    geometry::View::with_merged_attributes_owned(attributes, target)
}

type WeightSlots = Vec<Mutex<Option<Vec<f64>>>>;

/// Per-target weight rows: `work(target, row)` fills `row` (`capacity` values, initialised from
/// `distances`) and rows are joined in range order.
fn weight_rows(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    target_count: i64,
    capacity: i64,
    distances: &[f64],
    work: &(dyn Fn(i64, &mut [f64]) + Sync),
) -> Result<Vec<f64>, error::T> {
    let ranges = if target_count <= 0 { 0 } else { ((target_count - 1) / grain + 1) as usize };
    let slots: WeightSlots = (0..ranges).map(|_| Mutex::new(None)).collect();
    support::run_indexed_ranges(grain, target_count, &|range, first, last| {
        let mut local = distances[(first * capacity) as usize..(last * capacity) as usize].to_vec();
        for target_element in first..last {
            cancel::poll(cancel_token, target_element, 4095)?;
            let at = ((target_element - first) * capacity) as usize;
            work(target_element, &mut local[at..at + capacity as usize]);
        }
        *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some(local);
        Ok(())
    })?;
    let mut output = Vec::with_capacity(distances.len());
    for slot in slots {
        output.extend(slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner).unwrap_or_default());
    }
    Ok(output)
}

const MAX_ARRAY_LENGTH: i64 = (1_i64 << 54) - 1;

#[allow(clippy::too_many_arguments)]
fn spatial_transfer(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    names: Option<&Vec<String>>,
    pattern: Option<&String>,
    mode: Mode,
    max_distance: Option<f64>,
    blend_width: f64,
    falloff: Falloff,
    unmatched: Unmatched,
    source_elements: Option<&group::T>,
    target_elements: Option<&group::T>,
    operation: &str,
    owner: attribute::Owner,
    source_positions: &dyn Fn() -> Result<packed::Float3::T, error::T>,
    target_positions: &dyn Fn() -> Result<packed::Float3::T, error::T>,
    source: &geometry::T,
    target: &geometry::T,
) -> Result<geometry::T, error::T> {
    if owner == attribute::Owner::Detail || owner == attribute::Owner::Vertex {
        return Err(invalid(&format!("{operation}: spatial samples require point or primitive ownership")));
    }
    if grain <= 0 {
        return Err(invalid(&format!("{operation}: grain must be positive")));
    }
    cancel::check_opt(cancel_token)?;
    let neighbor_capacity_result: Result<i64, error::T> = match mode {
        Mode::Nearest => Ok(1),
        Mode::InverseDistance { neighbors, power } if neighbors > 0 && power.is_finite() && power > 0.0 => Ok(neighbors),
        Mode::InverseDistance { .. } => {
            Err(fail(&format!("{operation}: neighbors and power must be positive; power must be finite")))
        }
        Mode::Kernel { neighbors, radius, .. }
            if neighbors > 0 && radius.is_finite() && radius >= 0.0 && radius <= f64::MAX.sqrt() =>
        {
            Ok(neighbors)
        }
        Mode::Kernel { .. } => Err(fail(&format!(
            "{operation}: kernel neighbors must be positive and radius must be finite, non-negative, and safely squarable"
        ))),
    };
    let maximum_result = transfer_distance_window(operation, blend_width, falloff, max_distance);
    let expected_group_owner = group_owner_of_attribute(owner)?;
    let source_count = owner_count(source, owner);
    let target_count = owner_count(target, owner);
    let groups_result: Result<(), error::T> = if source_elements
        .is_some_and(|value| group::owner(value) != expected_group_owner || group::length(value) != source_count)
    {
        Err(fail(&format!("{operation}: source selection must be a matching {} group", owner_label(owner))))
    } else if target_elements
        .is_some_and(|value| group::owner(value) != expected_group_owner || group::length(value) != target_count)
    {
        Err(fail(&format!("{operation}: target selection must be a matching {} group", owner_label(owner))))
    } else {
        Ok(())
    };
    let neighbor_capacity = neighbor_capacity_result?;
    let (threshold_squared, maximum_squared) = maximum_result?;
    groups_result?;
    let attributes = selected_attributes(operation, owner, names, pattern, source)?;
    validate_sampled_storage(operation, &attributes)?;
    validate_target_kinds(operation, owner, &attributes, target)?;
    if attributes.is_empty() {
        return Ok(target.clone());
    }
    if target_count > MAX_ARRAY_LENGTH / neighbor_capacity {
        return Err(fail(&format!("{operation}: neighbor table is too large")));
    }
    let source_positions = source_positions()?;
    let target_positions = target_positions()?;
    if packed::Float3::length(&source_positions) != source_count || packed::Float3::length(&target_positions) != target_count {
        return Err(invalid(&format!("{operation}: internal sample cardinality mismatch")));
    }
    let source_points = source_elements.map(|value| group::View::with_owner(group::Owner::Point, value));
    let target_points = target_elements.map(|value| group::View::with_owner(group::Owner::Point, value));
    let index = spatial_index::create(cancel_token, Some(grain), source_points.as_ref(), &source_positions).map_err(
        |failure| {
            if error::code(&failure) == "cancelled" {
                failure
            } else {
                fail(&error::to_string(&failure))
            }
        },
    )?;
    let target_view = packed::Float3::View::view(&target_positions)?;
    for element in 0..target_count {
        cancel::poll(cancel_token, element, 16_383)?;
        let e = element as usize;
        if target_elements.map_or(true, |value| group::mem(element, value))
            && !(target_view.x[e].is_finite() && target_view.y[e].is_finite() && target_view.z[e].is_finite())
        {
            return Err(invalid("target positions must be finite"));
        }
    }
    let slots = (target_count * neighbor_capacity) as usize;
    let mut neighbors = vec![-1_i64; slots];
    let mut distances = vec![f64::INFINITY; slots];
    let mut neighbor_counts = vec![0_i64; target_count as usize];
    spatial_index::View::nearest_k_many_into(
        cancel_token,
        target_points.as_ref(),
        grain,
        &index,
        &target_positions,
        maximum_squared,
        neighbor_capacity,
        &mut neighbors,
        &mut distances,
        &mut neighbor_counts,
    )?;
    let influence: Option<Vec<f64>> = if blend_width == 0.0 {
        None
    } else {
        let zeros = vec![0.0_f64; target_count as usize];
        let mut planes = map_planes(cancel_token, grain, &[&zeros], &|target_element, slot| {
            if neighbor_counts[target_element as usize] > 0 {
                slot[0] = transfer_influence(
                    falloff,
                    threshold_squared,
                    blend_width,
                    distances[(target_element * neighbor_capacity) as usize],
                );
            }
            Ok(())
        })?;
        Some(planes.pop().unwrap_or_default())
    };
    let sample_weights: Option<Vec<f64>> = match mode {
        Mode::Nearest => None,
        Mode::InverseDistance { power, .. } => Some(weight_rows(
            cancel_token,
            grain,
            target_count,
            neighbor_capacity,
            &distances,
            &|target_element, row| {
                let count = neighbor_counts[target_element as usize] as usize;
                if count > 0 {
                    let nearest_distance = row[0];
                    if nearest_distance == 0.0 || !nearest_distance.is_finite() {
                        for slot in 0..count {
                            row[slot] = 0.0;
                        }
                        row[0] = 1.0;
                    } else {
                        let nearest = nearest_distance.sqrt();
                        let mut total = 0.0_f64;
                        for slot in 0..count {
                            let weight = (nearest / row[slot].sqrt()).powf(power);
                            row[slot] = weight;
                            total += weight;
                        }
                        let inverse = 1.0 / total;
                        for slot in 0..count {
                            row[slot] *= inverse;
                        }
                    }
                }
            },
        )?),
        Mode::Kernel { radius, kernel, .. } => Some(weight_rows(
            cancel_token,
            grain,
            target_count,
            neighbor_capacity,
            &distances,
            &|target_element, row| {
                let count = neighbor_counts[target_element as usize] as usize;
                if count > 0 {
                    if radius == 0.0 {
                        for slot in 0..count {
                            row[slot] = 0.0;
                        }
                        row[0] = 1.0;
                    } else {
                        let mut total = 0.0_f64;
                        for slot in 0..count {
                            let weight = transfer_kernel_weight(kernel, row[slot].sqrt() / radius);
                            row[slot] = weight;
                            total += weight;
                        }
                        if total > 0.0 && total.is_finite() {
                            let inverse = 1.0 / total;
                            for slot in 0..count {
                                row[slot] *= inverse;
                            }
                        } else {
                            for slot in 0..count {
                                row[slot] = 0.0;
                            }
                            row[0] = 1.0;
                        }
                    }
                }
            },
        )?),
    };
    let transfer_plane = |source_values: &[f64], output: &[f64]| -> Result<Vec<f64>, error::T> {
        let mut planes = map_planes(cancel_token, grain, &[output], &|target_element, slot| {
            let count = neighbor_counts[target_element as usize];
            if count > 0 {
                let offset = (target_element * neighbor_capacity) as usize;
                let sampled = match (&mode, &sample_weights) {
                    (Mode::Nearest, _) | (_, None) => source_values[neighbors[offset] as usize],
                    (_, Some(weights)) => {
                        let mut weighted = 0.0_f64;
                        for position in 0..count as usize {
                            weighted += weights[offset + position] * source_values[neighbors[offset + position] as usize];
                        }
                        weighted
                    }
                };
                slot[0] = match &influence {
                    None => sampled,
                    Some(values) => slot[0] + values[target_element as usize] * (sampled - slot[0]),
                };
            }
            Ok(())
        })?;
        Ok(planes.pop().unwrap_or_default())
    };
    let discrete_selected = |target_element: i64| -> bool {
        neighbor_counts[target_element as usize] > 0
            && match &influence {
                None => true,
                Some(values) => values[target_element as usize] >= 0.5,
            }
    };
    let mut made: Vec<attribute::T> = Vec::with_capacity(attributes.len());
    for source_attribute in &attributes {
        let name = attribute::name(source_attribute);
        let existing = geometry::find_attribute(owner, name, target);
        let storage = match attribute::View::storage(source_attribute)? {
            like @ (attribute::Storage::Float(_)
            | attribute::Storage::Float2(_)
            | attribute::Storage::Float3(_)
            | attribute::Storage::Float4(_)) => {
                let source_planes = attribute::float_planes(like)?.unwrap_or_default();
                let output = initial_planes(target_elements, unmatched, &existing, like, target_count as usize)?;
                let mut transferred: Vec<Vec<f64>> = Vec::with_capacity(output.len());
                for (plane, source_plane) in source_planes.iter().enumerate() {
                    transferred.push(transfer_plane(source_plane, &output[plane])?);
                }
                if name == "N" && transferred.len() == 3 {
                    transferred = normalize_selected(
                        cancel_token,
                        grain,
                        &|element| {
                            neighbor_counts[element as usize] > 0
                                && match &influence {
                                    None => true,
                                    Some(values) => values[element as usize] > 0.0,
                                }
                        },
                        &transferred,
                    )?;
                }
                let refs: Vec<&[f64]> = transferred.iter().map(|plane| plane.as_slice()).collect();
                attribute::of_float_planes(like, &refs)?
            }
            attribute::Storage::Int(source_values) => {
                let output = initial_int(target_elements, unmatched, &existing, target_count as usize)?;
                attribute::Storage::Int(map_ints(cancel_token, grain, &output, &|target_element, slot| {
                    if discrete_selected(target_element) {
                        *slot = source_values[neighbors[(target_element * neighbor_capacity) as usize] as usize];
                    }
                    Ok(())
                })?)
            }
            attribute::Storage::Text(source_values) => {
                let output = initial_text(target_elements, unmatched, &existing, target_count as usize)?;
                attribute::Storage::Text(map_texts(cancel_token, grain, &output, &|target_element, slot| {
                    if discrete_selected(target_element) {
                        *slot = source_values[neighbors[(target_element * neighbor_capacity) as usize] as usize].clone();
                    }
                    Ok(())
                })?)
            }
            attribute::Storage::IntArray(_) | attribute::Storage::FloatArray(_) => {
                return Err(fail(&format!("{operation}: array attributes cannot be sampled")))
            }
        };
        made.push(attribute::create_owned(name, owner, storage)?);
    }
    install_attributes_batch(made, target)
}

fn spatial(owner: attribute::Owner, params: &SpatialParams) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let (source, target) = (&params.source, &params.target);
    let result = (|| {
        let (operation, source_positions, target_positions): (
            &str,
            Box<dyn Fn() -> Result<packed::Float3::T, error::T> + '_>,
            Box<dyn Fn() -> Result<packed::Float3::T, error::T> + '_>,
        ) = match owner {
            attribute::Owner::Point => (
                "Rdk.Attribute_ops.transfer_points",
                Box::new(|| Ok(geometry::positions(source).clone())),
                Box::new(|| Ok(geometry::positions(target).clone())),
            ),
            attribute::Owner::Primitive => (
                "Rdk.Attribute_ops.transfer_primitives",
                Box::new(|| primitive_barycenters(cancel_token, grain, source)),
                Box::new(|| primitive_barycenters(cancel_token, grain, target)),
            ),
            attribute::Owner::Vertex | attribute::Owner::Detail => {
                return Err(invalid("Rdk.Attribute_ops.transfer: owner must be point or primitive"))
            }
        };
        spatial_transfer(
            cancel_token,
            grain,
            params.names.as_ref(),
            params.pattern.as_ref(),
            params.mode,
            params.max_distance,
            params.blend_width,
            params.falloff,
            params.unmatched,
            params.source_elements.as_ref(),
            params.target_elements.as_ref(),
            operation,
            owner,
            &*source_positions,
            &*target_positions,
            source,
            target,
        )
    })();
    finish(result, "attribute_transfer", "attribute transfer was cancelled")
}

pub fn transfer_points(params: &SpatialParams) -> Result<geometry::T, error::T> {
    spatial(attribute::Owner::Point, params)
}

pub fn transfer_primitives(params: &SpatialParams) -> Result<geometry::T, error::T> {
    spatial(attribute::Owner::Primitive, params)
}

pub fn transfer_detail(params: &DetailParams) -> Result<geometry::T, error::T> {
    let result = (|| {
        let operation = "Rdk.Attribute_ops.transfer_detail";
        let attributes = selected_attributes(
            operation,
            attribute::Owner::Detail,
            params.names.as_ref(),
            params.pattern.as_ref(),
            &params.source,
        )?;
        validate_target_kinds(operation, attribute::Owner::Detail, &attributes, &params.target)?;
        install_attributes_batch(attributes, &params.target)
    })();
    finish(result, "attribute_transfer", "geometry operation was cancelled")
}

pub fn surface_attribute(
    into: Option<&str>,
    owner: attribute::Owner,
    name: &str,
) -> Result<SurfaceAttribute, error::T> {
    let bad = |message: &str| make_error("surface_attribute", "invalid_argument", message);
    if owner == attribute::Owner::Detail {
        return Err(bad("Rdk.Attribute_ops.surface_attribute: detail ownership is not spatial"));
    }
    if is_blank(name) {
        return Err(bad("Rdk.Attribute_ops.surface_attribute: empty source name"));
    }
    let target_name = into.unwrap_or(name).to_owned();
    if is_blank(&target_name) {
        return Err(bad("Rdk.Attribute_ops.surface_attribute: empty target name"));
    }
    if owner == attribute::Owner::Point && name == "P" {
        return Err(bad("Rdk.Attribute_ops.surface_attribute: canonical P is not an ordinary attribute"));
    }
    Ok(SurfaceAttribute { source_owner: owner, source_name: name.to_owned(), target_name })
}

pub fn transfer_surface(params: &SurfaceParams) -> Result<geometry::T, error::T> {
    finish(
        transfer_surface_inner(params),
        "attribute_transfer_surface",
        "surface attribute transfer was cancelled",
    )
}

fn transfer_surface_inner(params: &SurfaceParams) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let (source, target) = (&params.source, &params.target);
    let target_owner = params.target_owner;
    let target_elements = params.target_elements.as_ref();
    if grain <= 0 {
        return Err(invalid("Rdk.Attribute_ops.transfer_surface: grain must be positive"));
    }
    cancel::check_opt(cancel_token)?;
    let operation = "Rdk.Attribute_ops.transfer_surface";
    let (threshold_squared, maximum_squared) =
        transfer_distance_window(operation, params.blend_width, params.falloff, params.max_distance)?;
    let specifications = &params.attributes;
    let mut source_attributes: Vec<Option<attribute::T>> = vec![None; specifications.len()];
    let mut seen: HashSet<String> = HashSet::new();
    let mut failure: Option<String> = None;
    if target_owner == attribute::Owner::Detail {
        failure = Some("detail ownership is not a spatial surface destination".to_owned());
    }
    if let Some(value) = &params.source_primitives {
        if group::owner(value) != group::Owner::Primitive || group::length(value) != geometry::primitive_count(source) {
            failure = Some("source selection must be a matching primitive group".to_owned());
        }
    }
    if let Some(value) = &params.source_vertices {
        if group::owner(value) != group::Owner::Vertex || group::length(value) != geometry::vertex_count(source) {
            failure = Some("source vertex selection must be a matching vertex group".to_owned());
        }
    }
    if let Some(value) = target_elements {
        let (expected_owner, expected_length) = match target_owner {
            attribute::Owner::Point => (group::Owner::Point, geometry::point_count(target)),
            attribute::Owner::Vertex => (group::Owner::Vertex, geometry::vertex_count(target)),
            attribute::Owner::Primitive => (group::Owner::Primitive, geometry::primitive_count(target)),
            attribute::Owner::Detail => (group::Owner::Point, 0),
        };
        if group::owner(value) != expected_owner || group::length(value) != expected_length {
            failure = Some("target selection owner/length does not match destination elements".to_owned());
        }
    }
    for (index, specification) in specifications.iter().enumerate() {
        if failure.is_some() {
            break;
        }
        if specification.source_owner == attribute::Owner::Detail {
            failure = Some("detail attributes are not spatial surface attributes".to_owned());
        } else if is_blank(&specification.source_name) || is_blank(&specification.target_name) {
            failure = Some("attribute names must not be empty".to_owned());
        } else if specification.source_owner == attribute::Owner::Point && specification.source_name == "P" {
            failure = Some("canonical P cannot be transferred as an ordinary attribute".to_owned());
        } else if target_owner == attribute::Owner::Point && specification.target_name == "P" {
            failure = Some("canonical target P cannot be an ordinary attribute".to_owned());
        } else if seen.contains(&specification.target_name) {
            failure = Some(format!("duplicate target attribute {}", specification.target_name));
        } else {
            seen.insert(specification.target_name.clone());
            match geometry::find_attribute(specification.source_owner, &specification.source_name, source) {
                None => {
                    failure = Some(format!(
                        "missing source {} attribute {}",
                        owner_label(specification.source_owner),
                        specification.source_name
                    ))
                }
                Some(found) => match attribute::View::storage(&found)? {
                    attribute::Storage::IntArray(_) | attribute::Storage::FloatArray(_) => {
                        failure = Some(format!(
                            "sampled transfer of array attribute {} requires an explicit array policy",
                            specification.source_name
                        ))
                    }
                    _ => match geometry::find_attribute(target_owner, &specification.target_name, target) {
                        Some(target_attribute) if !same_kind(&found, &target_attribute) => {
                            failure = Some(format!(
                                "target attribute {} has {} storage, source has {}",
                                specification.target_name,
                                attribute::kind_name(&target_attribute),
                                attribute::kind_name(&found)
                            ))
                        }
                        _ => source_attributes[index] = Some(found),
                    },
                },
            }
        }
    }
    let distance_name: Option<String> = match &params.distance_attribute {
        None => None,
        Some(name) if is_blank(name) => {
            failure = Some("distance attribute name must not be empty".to_owned());
            None
        }
        Some(name) if seen.contains(name) => {
            failure = Some(format!("distance attribute conflicts with {name}"));
            None
        }
        Some(name) if name == "P" && target_owner == attribute::Owner::Point => {
            failure = Some("canonical target P cannot store transfer distance".to_owned());
            None
        }
        Some(name) => {
            if let Some(found) = geometry::find_attribute(target_owner, name, target) {
                if attribute::kind_name(&found) != "float" {
                    failure = Some(format!("distance target attribute {name} is not float"));
                }
            }
            Some(name.clone())
        }
    };
    if let Some(message) = failure {
        return Err(fail(&message));
    }
    if specifications.is_empty() && distance_name.is_none() {
        return Ok(target.clone());
    }
    let empty_source = match (&params.source_primitives, &params.source_vertices) {
        (Some(value), _) if group::cardinality(value) == 0 => true,
        (_, Some(value)) if group::cardinality(value) == 0 => true,
        _ => geometry::primitive_count(source) == 0,
    };
    let unmatched = params.unmatched;
    let target_count = owner_count(target, target_owner) as usize;
    if empty_source {
        let mut made: Vec<attribute::T> = Vec::new();
        for (index, specification) in specifications.iter().enumerate() {
            let Some(source_attribute) = &source_attributes[index] else { continue };
            let existing = geometry::find_attribute(target_owner, &specification.target_name, target);
            let storage = match attribute::View::storage(source_attribute)? {
                like @ (attribute::Storage::Float(_)
                | attribute::Storage::Float2(_)
                | attribute::Storage::Float3(_)
                | attribute::Storage::Float4(_)) => {
                    let planes = initial_planes(target_elements, unmatched, &existing, like, target_count)?;
                    let refs: Vec<&[f64]> = planes.iter().map(|plane| plane.as_slice()).collect();
                    attribute::of_float_planes(like, &refs)?
                }
                attribute::Storage::Int(_) => {
                    attribute::Storage::Int(initial_int(target_elements, unmatched, &existing, target_count)?)
                }
                attribute::Storage::Text(_) => {
                    attribute::Storage::Text(initial_text(target_elements, unmatched, &existing, target_count)?)
                }
                attribute::Storage::IntArray(_) | attribute::Storage::FloatArray(_) => {
                    return Err(fail("array attributes cannot be sampled"))
                }
            };
            made.push(attribute::create_owned(&specification.target_name, target_owner, storage)?);
        }
        if let Some(name) = &distance_name {
            let existing = geometry::find_attribute(target_owner, name, target);
            let values = initial_float(target_elements, unmatched, &existing, target_count)?;
            made.push(attribute::create_owned(name, target_owner, attribute::Storage::Float(values))?);
        }
        return install_attributes_batch(made, target);
    }
    let surface = surface_index::create(
        cancel_token,
        Some(grain),
        params.source_primitives.as_ref(),
        params.source_vertices.as_ref(),
        Some(params.source_vertex_selection),
        source,
    )
    .map_err(|failure| {
        if error::code(&failure) == "cancelled" {
            failure
        } else {
            fail(&error::to_string(&failure))
        }
    })?;
    let (query_positions, position_indices): (packed::Float3::T, Option<Arc<[i64]>>) = match target_owner {
        attribute::Owner::Point => (geometry::positions(target).clone(), None),
        attribute::Owner::Vertex => {
            let topology_view = topology::View::view(geometry::topology(target))?;
            (geometry::positions(target).clone(), Some(topology_view.vertex_points))
        }
        attribute::Owner::Primitive => (primitive_barycenters(cancel_token, grain, target)?, None),
        attribute::Owner::Detail => return Err(fail("detail ownership is not a spatial surface destination")),
    };
    let mut primitives = vec![-1_i64; target_count];
    let mut triangles = vec![-1_i64; target_count];
    let mut barycentric_a = vec![0.0_f64; target_count];
    let mut barycentric_b = vec![0.0_f64; target_count];
    let mut barycentric_c = vec![0.0_f64; target_count];
    let mut distances_squared = vec![f64::INFINITY; target_count];
    surface_index::closest_many_into(
        cancel_token,
        target_elements,
        position_indices.as_deref(),
        grain,
        &surface,
        &query_positions,
        maximum_squared,
        &mut primitives,
        &mut triangles,
        &mut barycentric_a,
        &mut barycentric_b,
        &mut barycentric_c,
        &mut distances_squared,
    )?;
    let falloff = params.falloff;
    let blend_width = params.blend_width;
    let influence: Option<Vec<f64>> = if blend_width == 0.0 {
        None
    } else {
        let zeros = vec![0.0_f64; target_count];
        let mut planes = map_planes(cancel_token, grain, &[&zeros], &|point, slot| {
            if primitives[point as usize] >= 0 {
                slot[0] = transfer_influence(falloff, threshold_squared, blend_width, distances_squared[point as usize]);
            }
            Ok(())
        })?;
        Some(planes.pop().unwrap_or_default())
    };
    let source_topology = topology::View::view(geometry::topology(source))?;
    let source_element = |owner: attribute::Owner, primitive: i64, triangle: i64, local: i64| -> i64 {
        match owner {
            attribute::Owner::Point => {
                let vertex = surface_index::View::triangle_vertex(&surface, triangle, local);
                source_topology.vertex_points[vertex as usize]
            }
            attribute::Owner::Vertex => surface_index::View::triangle_vertex(&surface, triangle, local),
            attribute::Owner::Primitive => primitive,
            attribute::Owner::Detail => 0,
        }
    };
    let transfer_numeric = |owner: attribute::Owner, source_values: &[f64], output: &[f64]| -> Result<Vec<f64>, error::T> {
        let mut planes = map_planes(cancel_token, grain, &[output], &|point, slot| {
            let p = point as usize;
            let primitive = primitives[p];
            if primitive >= 0 {
                let sampled = if owner == attribute::Owner::Primitive {
                    source_values[primitive as usize]
                } else {
                    barycentric_a[p] * source_values[source_element(owner, primitive, triangles[p], 0) as usize]
                        + barycentric_b[p] * source_values[source_element(owner, primitive, triangles[p], 1) as usize]
                        + barycentric_c[p] * source_values[source_element(owner, primitive, triangles[p], 2) as usize]
                };
                slot[0] = match &influence {
                    None => sampled,
                    Some(values) => slot[0] + (values[p] * (sampled - slot[0])),
                };
            }
            Ok(())
        })?;
        Ok(planes.pop().unwrap_or_default())
    };
    let discrete_local = |owner: attribute::Owner, point: usize| -> Option<i64> {
        if primitives[point] < 0 {
            return None;
        }
        if let Some(values) = &influence {
            if values[point] < 0.5 {
                return None;
            }
        }
        Some(if owner == attribute::Owner::Primitive {
            0
        } else if barycentric_a[point] >= barycentric_b[point] && barycentric_a[point] >= barycentric_c[point] {
            0
        } else if barycentric_b[point] >= barycentric_c[point] {
            1
        } else {
            2
        })
    };
    let mut made: Vec<attribute::T> = Vec::new();
    for (index, specification) in specifications.iter().enumerate() {
        let Some(source_attribute) = &source_attributes[index] else { continue };
        let existing = geometry::find_attribute(target_owner, &specification.target_name, target);
        let owner = specification.source_owner;
        let storage = match attribute::View::storage(source_attribute)? {
            like @ (attribute::Storage::Float(_)
            | attribute::Storage::Float2(_)
            | attribute::Storage::Float3(_)
            | attribute::Storage::Float4(_)) => {
                let source_planes = attribute::float_planes(like)?.unwrap_or_default();
                let output = initial_planes(target_elements, unmatched, &existing, like, target_count)?;
                let mut transferred: Vec<Vec<f64>> = Vec::with_capacity(output.len());
                for (plane, source_plane) in source_planes.iter().enumerate() {
                    transferred.push(transfer_numeric(owner, source_plane, &output[plane])?);
                }
                if specification.source_name == "N" && transferred.len() == 3 {
                    transferred = normalize_selected(
                        cancel_token,
                        grain,
                        &|point| {
                            primitives[point as usize] >= 0
                                && match &influence {
                                    None => true,
                                    Some(values) => values[point as usize] > 0.0,
                                }
                        },
                        &transferred,
                    )?;
                }
                let refs: Vec<&[f64]> = transferred.iter().map(|plane| plane.as_slice()).collect();
                attribute::of_float_planes(like, &refs)?
            }
            attribute::Storage::Int(values) => {
                let output = initial_int(target_elements, unmatched, &existing, target_count)?;
                attribute::Storage::Int(map_ints(cancel_token, grain, &output, &|point, slot| {
                    if let Some(local) = discrete_local(owner, point as usize) {
                        let primitive = primitives[point as usize];
                        *slot = values[source_element(owner, primitive, triangles[point as usize], local) as usize];
                    }
                    Ok(())
                })?)
            }
            attribute::Storage::Text(values) => {
                let output = initial_text(target_elements, unmatched, &existing, target_count)?;
                attribute::Storage::Text(map_texts(cancel_token, grain, &output, &|point, slot| {
                    if let Some(local) = discrete_local(owner, point as usize) {
                        let primitive = primitives[point as usize];
                        *slot = values[source_element(owner, primitive, triangles[point as usize], local) as usize]
                            .clone();
                    }
                    Ok(())
                })?)
            }
            attribute::Storage::IntArray(_) | attribute::Storage::FloatArray(_) => {
                return Err(fail("array attributes cannot be sampled"))
            }
        };
        made.push(attribute::create_owned(&specification.target_name, target_owner, storage)?);
    }
    if let Some(name) = &distance_name {
        let existing = geometry::find_attribute(target_owner, name, target);
        let values = initial_float(target_elements, unmatched, &existing, target_count)?;
        let mut planes = map_planes(cancel_token, grain, &[&values], &|point, slot| {
            if primitives[point as usize] >= 0 {
                slot[0] = distances_squared[point as usize].sqrt();
            }
            Ok(())
        })?;
        made.push(attribute::create_owned(
            name,
            target_owner,
            attribute::Storage::Float(planes.pop().unwrap_or_default()),
        )?);
    }
    install_attributes_batch(made, target)
}

pub fn transfer_vertices(params: &VerticesParams) -> Result<geometry::T, error::T> {
    let result = (|| {
        let attributes = selected_attributes(
            "Rdk.Attribute_ops.transfer_vertices",
            attribute::Owner::Vertex,
            params.names.as_ref(),
            params.pattern.as_ref(),
            &params.source,
        )?;
        let mut specs = Vec::with_capacity(attributes.len());
        for item in &attributes {
            specs.push(surface_attribute(None, attribute::Owner::Vertex, attribute::name(item))?);
        }
        flatten(transfer_surface(&SurfaceParams {
            cancel: params.cancel.clone(),
            grain: params.grain,
            max_distance: params.max_distance,
            blend_width: params.blend_width,
            falloff: params.falloff,
            unmatched: params.unmatched,
            target_owner: attribute::Owner::Vertex,
            distance_attribute: None,
            source_primitives: params.source_primitives.clone(),
            source_vertices: params.source_vertices.clone(),
            source_vertex_selection: params.source_vertex_selection,
            target_elements: params.target_vertices.clone(),
            attributes: specs,
            source: params.source.clone(),
            target: params.target.clone(),
        }))
    })();
    finish(result, "attribute_transfer", "attribute transfer was cancelled")
}

pub fn transfer_all(params: &AllParams) -> Result<geometry::T, error::T> {
    let result = (|| {
        let spatial_step = |owner: attribute::Owner, pattern: &str, target: geometry::T| {
            flatten(spatial(
                owner,
                &SpatialParams {
                    cancel: params.cancel.clone(),
                    grain: params.grain,
                    names: None,
                    pattern: Some(pattern.to_owned()),
                    mode: params.mode,
                    max_distance: params.max_distance,
                    blend_width: params.blend_width,
                    falloff: params.falloff,
                    unmatched: params.unmatched,
                    source_elements: None,
                    target_elements: None,
                    source: params.source.clone(),
                    target,
                },
            ))
        };
        let mut target = params.target.clone();
        if let Some(pattern) = &params.point_pattern {
            target = spatial_step(attribute::Owner::Point, pattern, target)?;
        }
        if let Some(pattern) = &params.primitive_pattern {
            target = spatial_step(attribute::Owner::Primitive, pattern, target)?;
        }
        if let Some(pattern) = &params.vertex_pattern {
            target = flatten(transfer_vertices(&VerticesParams {
                cancel: params.cancel.clone(),
                grain: params.grain,
                names: None,
                pattern: Some(pattern.clone()),
                max_distance: params.max_distance,
                blend_width: params.blend_width,
                falloff: params.falloff,
                unmatched: params.unmatched,
                source_primitives: None,
                source_vertices: None,
                source_vertex_selection: surface_index::VertexSelection::AllTriangleVertices,
                target_vertices: None,
                source: params.source.clone(),
                target,
            }))?;
        }
        if let Some(pattern) = &params.detail_pattern {
            target = flatten(transfer_detail(&DetailParams {
                names: None,
                pattern: Some(pattern.clone()),
                source: params.source.clone(),
                target,
            }))?;
        }
        Ok(target)
    })();
    finish(result, "attribute_transfer_all", "multi-owner attribute transfer was cancelled")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::attribute_ops::{self, fixtures::*, fixtures2::*};
    use attribute::Owner::{Detail, Point, Primitive, Vertex};

    fn spatial_params(source: &geometry::T, target: &geometry::T) -> SpatialParams {
        SpatialParams {
            cancel: None,
            grain: Some(1),
            names: None,
            pattern: None,
            mode: Mode::Nearest,
            max_distance: None,
            blend_width: 0.0,
            falloff: Falloff::Smoothstep,
            unmatched: Unmatched::KeepTarget,
            source_elements: None,
            target_elements: None,
            source: source.clone(),
            target: target.clone(),
        }
    }

    fn surface_params(source: &geometry::T, target: &geometry::T, attributes: Vec<SurfaceAttribute>) -> SurfaceParams {
        SurfaceParams {
            cancel: None,
            grain: Some(1),
            max_distance: None,
            blend_width: 0.0,
            falloff: Falloff::Smoothstep,
            unmatched: Unmatched::KeepTarget,
            target_owner: Point,
            distance_attribute: None,
            source_primitives: None,
            source_vertices: None,
            source_vertex_selection: surface_index::VertexSelection::AllTriangleVertices,
            target_elements: None,
            attributes,
            source: source.clone(),
            target: target.clone(),
        }
    }

    fn vertices_params(source: &geometry::T, target: &geometry::T, pattern: &str) -> VerticesParams {
        VerticesParams {
            cancel: None,
            grain: Some(1),
            names: None,
            pattern: Some(pattern.to_owned()),
            max_distance: None,
            blend_width: 0.0,
            falloff: Falloff::Smoothstep,
            unmatched: Unmatched::KeepTarget,
            source_primitives: None,
            source_vertices: None,
            source_vertex_selection: surface_index::VertexSelection::AllTriangleVertices,
            target_vertices: None,
            source: source.clone(),
            target: target.clone(),
        }
    }

    fn all_params(source: &geometry::T, target: &geometry::T) -> AllParams {
        AllParams {
            cancel: None,
            grain: Some(1),
            point_pattern: None,
            vertex_pattern: None,
            primitive_pattern: None,
            detail_pattern: None,
            mode: Mode::Nearest,
            max_distance: None,
            blend_width: 0.0,
            falloff: Falloff::Smoothstep,
            unmatched: Unmatched::KeepTarget,
            source: source.clone(),
            target: target.clone(),
        }
    }

    fn spec(owner: attribute::Owner, source: &str, target: &str) -> SurfaceAttribute {
        SurfaceAttribute { source_owner: owner, source_name: source.to_owned(), target_name: target.to_owned() }
    }

    fn make_source() -> geometry::T {
        let g = triangle(0.0);
        let g = add_values(Point, "point_value", vec![10.0, 20.0, 30.0], &g);
        let g = add_values(Vertex, "vertex_value", vec![4.0, 8.0, 12.0], &g);
        let g = add_values(Primitive, "primitive_value", vec![40.0], &g);
        add_ints(Detail, "detail_value", vec![50], &g)
    }

    fn make_target() -> geometry::T {
        let g = triangle(1.5);
        let g = add_values(Point, "point_value", vec![2.0; 3], &g);
        let g = add_values(Vertex, "vertex_value", vec![2.0; 3], &g);
        let g = add_values(Primitive, "primitive_value", vec![2.0], &g);
        add_ints(Detail, "detail_value", vec![2], &g)
    }

    fn on(domains: i64, work: &(dyn Fn() -> Result<geometry::T, error::T> + Sync)) -> geometry::T {
        on_domains(domains, &|| work().unwrap())
    }

    #[test]
    fn source_kernels_and_fallbacks() {
        let source = points(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.0, 0.0, 0.0)]);
        let source = add_values(Point, "weight", vec![0.0, 10.0, 20.0], &source);
        let source = add_ints(Point, "id", vec![3, 5, 7], &source);
        let target = points(&[(0.5, 0.0, 0.0)]);
        let target = add_values(Point, "weight", vec![100.0], &target);
        let target = add_ints(Point, "id", vec![99], &target);
        let sample = |kernel: Kernel| {
            let mut params = spatial_params(&source, &target);
            params.mode = Mode::Kernel { neighbors: 3, radius: 2.0, kernel };
            attribute_ops::transfer_points(&params).unwrap()
        };
        let (links, renderman, hart) = (sample(Kernel::Links), sample(Kernel::RenderMan), sample(Kernel::Hart));
        assert!(near(floats(Point, "weight", &links)[0], 64.0 / 11.0));
        assert!(near(floats(Point, "weight", &renderman)[0], 40610.0 / 7093.0));
        assert!(near(floats(Point, "weight", &hart)[0], 5650.0 / 971.0));
        for result in [&links, &renderman, &hart] {
            assert_eq!(ints(Point, "id", result), vec![3]);
        }
        let fallback = |radius: f64| {
            let mut params = spatial_params(&source, &target);
            params.mode = Mode::Kernel { neighbors: 3, radius, kernel: Kernel::Hart };
            attribute_ops::transfer_points(&params).unwrap()
        };
        assert_eq!(floats(Point, "weight", &fallback(0.0)), vec![0.0]);
        assert_eq!(floats(Point, "weight", &fallback(0.1)), vec![0.0]);

        let large: Vec<(f64, f64, f64)> = (0..20_003_i64).map(|p| ((p % 2001) as f64 / 1000.0, 0.0, 0.0)).collect();
        let large_target = points(&large);
        let run = |domains: i64| {
            on(domains, &|| {
                let mut params = spatial_params(&source, &large_target);
                params.grain = Some(257);
                params.mode = Mode::Kernel { neighbors: 3, radius: 2.0, kernel: Kernel::RenderMan };
                attribute_ops::transfer_points(&params)
            })
        };
        let (one, four) = (run(1), run(4));
        assert_eq!(floats(Point, "weight", &one), floats(Point, "weight", &four));
        assert_eq!(ints(Point, "id", &one), ints(Point, "id", &four));
        for mode in [
            Mode::Kernel { neighbors: 0, radius: 1.0, kernel: Kernel::Links },
            Mode::Kernel { neighbors: 1, radius: f64::NAN, kernel: Kernel::RenderMan },
            Mode::Kernel { neighbors: 1, radius: f64::MAX, kernel: Kernel::Hart },
        ] {
            let mut params = spatial_params(&source, &target);
            params.mode = mode;
            assert_eq!(err_code(attribute_ops::transfer_points(&params)), "invalid_transfer");
        }
    }

    #[test]
    fn blend_falloff_bands() {
        let source = points(&[(0.0, 0.0, 0.0)]);
        let source = add_values(Point, "weight", vec![10.0], &source);
        let source = add_ints(Point, "id", vec![7], &source);
        let source = add_float3s(Point, "N", vec![1.0], vec![0.0], vec![0.0], &source);
        let target = points(&[(1.5, 0.0, 0.0), (2.5, 0.0, 0.0)]);
        let target = add_values(Point, "weight", vec![2.0, 2.0], &target);
        let target = add_ints(Point, "id", vec![3, 3], &target);
        let target = add_float3s(Point, "N", vec![0.0, 0.0], vec![2.0, 2.0], vec![0.0, 0.0], &target);
        let banded = |falloff: Falloff, names: Option<Vec<String>>| {
            let mut params = spatial_params(&source, &target);
            params.max_distance = Some(1.0);
            params.blend_width = 1.0;
            params.falloff = falloff;
            params.names = names;
            attribute_ops::transfer_points(&params)
        };
        let linear = banded(Falloff::Linear, None).unwrap();
        assert_eq!(floats(Point, "weight", &linear), vec![6.0, 2.0]);
        assert_eq!(ints(Point, "id", &linear), vec![7, 3]);
        let uniform = banded(Falloff::Uniform(0.25), None).unwrap();
        assert_eq!(floats(Point, "weight", &uniform), vec![4.0, 2.0]);
        assert_eq!(ints(Point, "id", &uniform), vec![3, 3]);
        let zero = banded(Falloff::Uniform(0.0), Some(vec!["N".to_owned()])).unwrap();
        let (x, y, z) = float3(Point, "N", &zero);
        assert_eq!((x, y, z), (vec![0.0, 0.0], vec![2.0, 2.0], vec![0.0, 0.0]));
        assert_eq!(err_code(banded(Falloff::Uniform(f64::NAN), None)), "invalid_transfer");
        let mut no_threshold = spatial_params(&source, &target);
        no_threshold.blend_width = 1.0;
        assert_eq!(err_code(attribute_ops::transfer_points(&no_threshold)), "invalid_transfer");
    }

    #[test]
    fn multi_owner_exactness() {
        let (source, target) = (make_source(), make_target());
        let all = |domains: i64| {
            on(domains, &|| {
                let mut params = all_params(&source, &target);
                params.point_pattern = Some("point_*".to_owned());
                params.primitive_pattern = Some("primitive_*".to_owned());
                params.vertex_pattern = Some("vertex_*".to_owned());
                params.detail_pattern = Some("detail_*".to_owned());
                params.max_distance = Some(1.0);
                params.blend_width = 1.0;
                params.falloff = Falloff::Linear;
                attribute_ops::transfer_all(&params)
            })
        };
        let (one, four) = (all(1), all(4));
        assert_eq!(signature(&one), signature(&four));
        let mut step = spatial_params(&source, &target);
        step.pattern = Some("point_*".to_owned());
        step.max_distance = Some(1.0);
        step.blend_width = 1.0;
        step.falloff = Falloff::Linear;
        let sequential = attribute_ops::transfer_points(&step).unwrap();
        step.target = sequential;
        step.pattern = Some("primitive_*".to_owned());
        let sequential = attribute_ops::transfer_primitives(&step).unwrap();
        let mut vertices = vertices_params(&source, &sequential, "vertex_*");
        vertices.max_distance = Some(1.0);
        vertices.blend_width = 1.0;
        vertices.falloff = Falloff::Linear;
        let sequential = attribute_ops::transfer_vertices(&vertices).unwrap();
        let sequential = attribute_ops::transfer_detail(&DetailParams {
            names: None,
            pattern: Some("detail_*".to_owned()),
            source: source.clone(),
            target: sequential,
        })
        .unwrap();
        assert_eq!(signature(&one), signature(&sequential));
        assert_eq!(floats(Point, "point_value", &one), vec![6.0, 11.0, 16.0]);
        assert_eq!(floats(Primitive, "primitive_value", &one), vec![21.0]);
        assert_eq!(floats(Vertex, "vertex_value", &one), vec![3.0, 5.0, 7.0]);
        assert_eq!(ints(Detail, "detail_value", &one), vec![50]);
        let mut bad = all_params(&source, &target);
        bad.point_pattern = Some("bad[".to_owned());
        assert_eq!(err_code(attribute_ops::transfer_all(&bad)), "invalid_transfer");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut cancelled = all_params(&source, &target);
        cancelled.cancel = Some(token);
        cancelled.point_pattern = Some("*".to_owned());
        assert_eq!(err_code(attribute_ops::transfer_all(&cancelled)), "cancelled");
    }

    #[test]
    fn empty_surface_selection_publishes_defaults() {
        let (source, target) = (make_source(), make_target());
        let empty = group_of(group::Owner::Primitive, "empty", &[], 1);
        let mut params = vertices_params(&source, &target, "vertex_*");
        params.unmatched = Unmatched::DefaultValue;
        params.source_primitives = Some(empty);
        let result = attribute_ops::transfer_vertices(&params).unwrap();
        assert_eq!(floats(Vertex, "vertex_value", &result), vec![0.0, 0.0, 0.0]);
    }

    #[test]
    fn source_vertex_surface_selection() {
        let source = polygons(
            &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (10.0, 0.0, 0.0), (11.0, 0.0, 0.0), (10.0, 1.0, 0.0)],
            &[vec![0, 1, 2], vec![3, 4, 5]],
        );
        let source = add_values(Vertex, "corner_value", vec![0.0, 10.0, 20.0, 100.0, 110.0, 120.0], &source);
        let target = points(&[(0.25, 0.25, 0.2), (10.25, 0.25, 0.2)]);
        let target = add_values(Point, "sampled", vec![9.0, 9.0], &target);
        let specs = vec![spec(Vertex, "corner_value", "sampled")];
        let first_triangle = group_of(group::Owner::Vertex, "first_triangle", &[0, 1, 2], 6);
        let run = |domains: i64| {
            on(domains, &|| {
                let mut params = surface_params(&source, &target, specs.clone());
                params.max_distance = Some(0.5);
                params.unmatched = Unmatched::DefaultValue;
                params.source_vertices = Some(first_triangle.clone());
                attribute_ops::transfer_surface(&params)
            })
        };
        let (one, four) = (run(1), run(4));
        assert_eq!(floats(Point, "sampled", &one), vec![7.5, 0.0]);
        assert_eq!(floats(Point, "sampled", &one), floats(Point, "sampled", &four));
        let one_corner = group_of(group::Owner::Vertex, "one_corner", &[3], 6);
        let mut any = surface_params(&source, &target, specs.clone());
        any.max_distance = Some(0.5);
        any.unmatched = Unmatched::DefaultValue;
        any.source_vertices = Some(one_corner.clone());
        any.source_vertex_selection = surface_index::VertexSelection::AnyTriangleVertex;
        assert_eq!(floats(Point, "sampled", &attribute_ops::transfer_surface(&any).unwrap()), vec![0.0, 107.5]);
        let mut none = any.clone();
        none.source_vertex_selection = surface_index::VertexSelection::AllTriangleVertices;
        assert_eq!(floats(Point, "sampled", &attribute_ops::transfer_surface(&none).unwrap()), vec![0.0, 0.0]);
        let mut disjoint = surface_params(&source, &target, specs.clone());
        disjoint.max_distance = Some(0.5);
        disjoint.unmatched = Unmatched::DefaultValue;
        disjoint.source_primitives = Some(group_of(group::Owner::Primitive, "second_primitive", &[1], 2));
        disjoint.source_vertices = Some(first_triangle.clone());
        assert_eq!(floats(Point, "sampled", &attribute_ops::transfer_surface(&disjoint).unwrap()), vec![0.0, 0.0]);
        let mut wrong = surface_params(&source, &target, specs.clone());
        wrong.source_vertices = Some(group_of(group::Owner::Point, "wrong", &[0, 1, 2, 3, 4, 5], 6));
        assert_eq!(err_code(attribute_ops::transfer_surface(&wrong)), "invalid_transfer");
        let mut cancelled = surface_params(&source, &target, specs);
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        cancelled.cancel = Some(token);
        cancelled.source_vertices = Some(first_triangle);
        assert_eq!(err_code(attribute_ops::transfer_surface(&cancelled)), "cancelled");
    }

    #[test]
    fn scale_exactness_over_twenty_thousand_points() {
        let count = 20_001_i64;
        let positions: Vec<(f64, f64, f64)> = (0..count)
            .map(|index| {
                let x = index as f64 * 0.001;
                (x, x.sin(), x.cos())
            })
            .collect();
        let source = add_values(Point, "sample", (0..count).map(|i| i as f64).collect(), &points(&positions));
        let target = points(&positions);
        let run = |domains: i64| {
            on(domains, &|| {
                let mut params = all_params(&source, &target);
                params.grain = Some(257);
                params.point_pattern = Some("sample".to_owned());
                params.max_distance = Some(0.0);
                attribute_ops::transfer_all(&params)
            })
        };
        let (one, four) = (run(1), run(4));
        let expected: Vec<f64> = (0..count).map(|i| i as f64).collect();
        assert_eq!(floats(Point, "sample", &one), expected);
        assert_eq!(floats(Point, "sample", &one), floats(Point, "sample", &four));
    }

    fn quad() -> geometry::T {
        polygons(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)], &[vec![0, 1, 2, 3]])
    }

    fn two_triangle_quad() -> geometry::T {
        polygons(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)], &[vec![0, 1, 2], vec![0, 2, 3]])
    }

    #[test]
    fn point_and_primitive_transfer() {
        let transfer_source = add_ints(
            Point,
            "id",
            vec![5, 9],
            &add_values(Point, "weight", vec![0.0, 10.0], &points(&[(0.0, 0.0, 0.0), (2.0, 0.0, 0.0)])),
        );
        let transfer_target = add_values(
            Point,
            "weight",
            vec![100.0; 3],
            &points(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (10.0, 0.0, 0.0)]),
        );
        let transferred = |domains: i64| {
            on(domains, &|| {
                let mut params = spatial_params(&transfer_source, &transfer_target);
                params.max_distance = Some(3.0);
                params.mode = Mode::InverseDistance { neighbors: 2, power: 1.0 };
                attribute_ops::transfer_points(&params)
            })
        };
        let (one, many) = (transferred(1), transferred(4));
        assert_eq!(signature(&one), signature(&many));
        assert_eq!(floats(Point, "weight", &one), vec![0.0, 5.0, 100.0]);
        assert_eq!(ints(Point, "id", &one), vec![5, 5, 0]);
        let mut both = spatial_params(&transfer_source, &transfer_target);
        both.names = Some(vec!["weight".to_owned()]);
        both.pattern = Some("*".to_owned());
        assert_eq!(err_code(attribute_ops::transfer_points(&both)), "invalid_transfer");
        let mut malformed = spatial_params(&transfer_source, &transfer_target);
        malformed.pattern = Some("bad[".to_owned());
        assert_eq!(err_code(attribute_ops::transfer_points(&malformed)), "invalid_transfer");

        let source_second = group_of(group::Owner::Point, "source_second", &[1], 2);
        let target_outer = group_of(group::Owner::Point, "target_outer", &[0, 2], 3);
        let restricted = |domains: i64| {
            on(domains, &|| {
                let mut params = spatial_params(&transfer_source, &transfer_target);
                params.max_distance = Some(3.0);
                params.source_elements = Some(source_second.clone());
                params.target_elements = Some(target_outer.clone());
                attribute_ops::transfer_points(&params)
            })
        };
        let (restricted_one, restricted_many) = (restricted(1), restricted(4));
        assert_eq!(signature(&restricted_one), signature(&restricted_many));
        assert_eq!(floats(Point, "weight", &restricted_one), vec![10.0, 100.0, 100.0]);

        let triangles = two_triangle_quad();
        let (px, py, pz) = positions(&triangles);
        let scaled = polygons(
            &px.iter().zip(&py).zip(&pz).map(|((x, y), z)| (x * 3.0, y * 3.0, z * 3.0)).collect::<Vec<_>>(),
            &[vec![0, 1, 2], vec![0, 2, 3]],
        );
        let primitive_source = add_ints(
            Primitive,
            "primitive_id",
            vec![4, 8],
            &add_values(Primitive, "primitive_weight", vec![10.0, 20.0], &scaled),
        );
        let primitive_target = polygons(&[(0.0, 0.0, 0.0), (3.0, 0.0, 0.0), (1.5, 4.5, 0.0)], &[vec![0, 1, 2]]);
        let primitive_run = |domains: i64| {
            on(domains, &|| {
                let mut params = spatial_params(&primitive_source, &primitive_target);
                params.mode = Mode::InverseDistance { neighbors: 2, power: 1.0 };
                attribute_ops::transfer_primitives(&params)
            })
        };
        let (primitive_one, primitive_many) = (primitive_run(1), primitive_run(4));
        assert_eq!(signature(&primitive_one), signature(&primitive_many));
        assert_eq!(ints(Primitive, "primitive_id", &primitive_one), vec![4]);
        assert!(near(floats(Primitive, "primitive_weight", &primitive_one)[0], 15.0));
        let mut point_group = spatial_params(&primitive_source, &primitive_target);
        point_group.source_elements = Some(source_second);
        assert_eq!(err_code(attribute_ops::transfer_primitives(&point_group)), "invalid_transfer");

        let asset = attribute::create_owned("asset", Detail, attribute::Storage::Text(vec!["oak".to_owned()])).unwrap();
        let detail_source = geometry::with_attribute(asset.clone(), &primitive_source).unwrap();
        let detail_target = attribute_ops::transfer_detail(&DetailParams {
            names: None,
            pattern: None,
            source: detail_source.clone(),
            target: primitive_target.clone(),
        })
        .unwrap();
        let moved = find(Detail, "asset", &detail_target);
        assert_eq!(attribute::storage_id(&moved), attribute::storage_id(&asset));
        assert_eq!(texts(Detail, "asset", &detail_target), vec!["oak".to_owned()]);
        let detail_p = attribute::create_owned("P", Detail, attribute::Storage::Int(vec![23])).unwrap();
        let detail_p_source = geometry::with_attribute(detail_p, &detail_source).unwrap();
        let excluded = attribute_ops::transfer_detail(&DetailParams {
            names: None,
            pattern: Some("^P".to_owned()),
            source: detail_p_source.clone(),
            target: primitive_target.clone(),
        })
        .unwrap();
        assert!(has(Detail, "asset", &excluded) && !has(Detail, "P", &excluded));
        let named = attribute_ops::transfer_detail(&DetailParams {
            names: Some(vec!["P".to_owned()]),
            pattern: None,
            source: detail_p_source,
            target: primitive_target,
        })
        .unwrap();
        assert_eq!(ints(Detail, "P", &named), vec![23]);

        let conflicting = add_values(Point, "id", vec![0.0; 3], &transfer_target);
        let mut incompatible = spatial_params(&transfer_source, &conflicting);
        incompatible.names = Some(vec!["id".to_owned()]);
        assert_eq!(err_code(attribute_ops::transfer_points(&incompatible)), "invalid_transfer");
        let mut zero_neighbors = spatial_params(&transfer_source, &transfer_target);
        zero_neighbors.mode = Mode::InverseDistance { neighbors: 0, power: 1.0 };
        assert_eq!(err_code(attribute_ops::transfer_points(&zero_neighbors)), "invalid_transfer");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut cancelled = spatial_params(&transfer_source, &transfer_target);
        cancelled.cancel = Some(token);
        assert_eq!(err_code(attribute_ops::transfer_points(&cancelled)), "cancelled");
    }

    #[test]
    fn surface_and_vertex_transfer() {
        let triangle_geometry = two_triangle_quad();
        let surface_source = polygons(&[(0.0, 0.0, 0.0), (2.0, 0.0, 0.0), (0.0, 2.0, 0.0)], &[vec![0, 1, 2]]);
        let surface_source = add_values(Point, "weight", vec![0.0, 2.0, 4.0], &surface_source);
        let surface_source = add_values(Vertex, "corner_value", vec![10.0, 20.0, 30.0], &surface_source);
        let surface_source = add_ints(Primitive, "piece", vec![7], &surface_source);
        let vertex_run = |domains: i64| {
            on(domains, &|| {
                attribute_ops::transfer_vertices(&vertices_params(&surface_source, &triangle_geometry, "corner*"))
            })
        };
        let (vertex_one, vertex_many) = (vertex_run(1), vertex_run(4));
        let corners = floats(Vertex, "corner_value", &vertex_one);
        assert!(near_all(&[10.0, 15.0, 25.0, 10.0, 25.0, 20.0], &corners), "{corners:?}");
        assert_eq!(corners, floats(Vertex, "corner_value", &vertex_many));

        let duplicate = polygons(&[(0.0, 0.0, 0.0), (2.0, 0.0, 0.0), (0.0, 2.0, 0.0)], &[vec![0, 1, 2], vec![0, 1, 2]]);
        let restricted_source = add_ints(Primitive, "piece", vec![7, 9], &duplicate);
        let mut target = points(&[(0.5, 0.5, 1.0), (0.5, 0.5, 1.0)]);
        target = add_values(Point, "sampled_weight", vec![99.0, 99.0], &target);
        target = add_values(Point, "sampled_corner", vec![88.0, 88.0], &target);
        target = add_ints(Point, "sampled_piece", vec![6, 6], &target);
        target = add_values(Point, "surface_distance", vec![77.0, 77.0], &target);
        let target_signature = signature(&target);
        let target_first = group_of(group::Owner::Point, "first", &[0], 2);
        let attributes = vec![
            spec(Point, "weight", "sampled_weight"),
            spec(Vertex, "corner_value", "sampled_corner"),
            spec(Primitive, "piece", "sampled_piece"),
        ];
        let sampled = |domains: i64| {
            on(domains, &|| {
                let mut params = surface_params(&surface_source, &target, attributes.clone());
                params.target_elements = Some(target_first.clone());
                params.distance_attribute = Some("surface_distance".to_owned());
                attribute_ops::transfer_surface(&params)
            })
        };
        let (sampled_one, sampled_many) = (sampled(1), sampled(4));
        assert_eq!(signature(&sampled_one), signature(&sampled_many));
        assert_eq!(signature(&target), target_signature);
        assert_eq!(floats(Point, "sampled_weight", &sampled_one), vec![1.5, 99.0]);
        assert_eq!(floats(Point, "sampled_corner", &sampled_one), vec![17.5, 88.0]);
        assert_eq!(ints(Point, "sampled_piece", &sampled_one), vec![7, 6]);
        assert_eq!(floats(Point, "surface_distance", &sampled_one), vec![1.0, 77.0]);

        let second = group_of(group::Owner::Primitive, "second", &[1], 2);
        let restricted = |domains: i64| {
            on(domains, &|| {
                let mut params = surface_params(&restricted_source, &target, vec![spec(Primitive, "piece", "sampled_piece")]);
                params.source_primitives = Some(second.clone());
                params.target_elements = Some(target_first.clone());
                attribute_ops::transfer_surface(&params)
            })
        };
        let (restricted_one, restricted_many) = (restricted(1), restricted(4));
        assert_eq!(signature(&restricted_one), signature(&restricted_many));
        assert_eq!(ints(Point, "sampled_piece", &restricted_one), vec![9, 6]);
        assert_eq!(floats(Point, "surface_distance", &restricted_one), vec![77.0, 77.0]);

        let quad_source = add_values(Vertex, "quad_corner", vec![10.0, 20.0, 30.0, 40.0], &add_values(Point, "quad_weight", vec![0.0, 1.0, 2.0, 3.0], &quad()));
        let quad_target = points(&[(0.2, 0.7, 1.0), (0.8, 0.7, 1.0)]);
        let transferred = attribute_ops::transfer_surface(&surface_params(
            &quad_source,
            &quad_target,
            vec![spec(Point, "quad_weight", "quad_weight"), spec(Vertex, "quad_corner", "quad_corner")],
        ))
        .unwrap();
        assert!(near_all(&[2.3, 1.9], &floats(Point, "quad_weight", &transferred)));
        assert!(near_all(&[33.0, 29.0], &floats(Point, "quad_corner", &transferred)));
    }
}
