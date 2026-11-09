//! Mirror attributes across a plane or an explicit element mapping.
use std::sync::Arc;

use rdk_core::{support, 
    attribute, attribute_pattern, cancel, error, geometry, group, packed, ragged_ops, topology,
};
use rdk_geom::spatial_index;

use rdk_core::error::Code;
use crate::attribute_ops::{is_blank, make_error, map_ints, map_planes, map_texts, ocaml_quote};
use rays_math::text_array::TextArray;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupUse {
    MirrorGroupAsSource,
    MirrorGroupAsDestination,
}

#[derive(Clone)]
pub enum Method {
    MirrorByPlane {
        origin: rays_math::vec3::T,
        normal: rays_math::vec3::T,
        distance: f64,
        tolerance: f64,
    },
    MirrorByMapping {
        mapping_attribute: String,
        destination_group: group::T,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Transform {
    MirrorCopy,
    MirrorUv { origin_u: f64, origin_v: f64, direction_u: f64, direction_v: f64 },
    MirrorVector,
    MirrorPoint,
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub group: Option<group::T>,
    pub group_use: GroupUse,
    pub attributes: String,
    pub transform: Transform,
    pub string_replace: Option<(Vec<u8>, Vec<u8>)>,
    pub output_mapping: Option<String>,
    pub source_group: Option<String>,
    pub destination_group: Option<String>,
    pub owner: attribute::Owner,
    pub method_: Method,
}

#[derive(Clone, Copy)]
struct Plane {
    ox: f64,
    oy: f64,
    oz: f64,
    nx: f64,
    ny: f64,
    nz: f64,
    tolerance: f64,
}

struct Mapping {
    source_of_output: Vec<i64>,
    pair_destination: Vec<u8>,
    plane: Option<Plane>,
}

const OPERATION: &str = "attribute_mirror";
const CODE: Code = Code::InvalidAttributeMirror;

fn fail(message: &str) -> error::T {
    make_error(OPERATION, CODE, message)
}

fn detail_refused() -> error::T {
    fail("Attribute Mirror owner must be point, vertex or primitive")
}

fn attribute_owner(owner: attribute::Owner) -> Result<attribute::Owner, error::T> {
    match owner {
        attribute::Owner::Detail => Err(detail_refused()),
        owner => Ok(owner),
    }
}

fn group_owner(owner: attribute::Owner) -> Result<group::Owner, error::T> {
    match owner {
        attribute::Owner::Point => Ok(group::Owner::Point),
        attribute::Owner::Vertex => Ok(group::Owner::Vertex),
        attribute::Owner::Primitive => Ok(group::Owner::Primitive),
        attribute::Owner::Detail => Err(detail_refused()),
    }
}

fn owner_name(owner: attribute::Owner) -> Result<&'static str, error::T> {
    match owner {
        attribute::Owner::Point => Ok("point"),
        attribute::Owner::Vertex => Ok("vertex"),
        attribute::Owner::Primitive => Ok("primitive"),
        attribute::Owner::Detail => Err(detail_refused()),
    }
}

fn owner_count(owner: attribute::Owner, geometry_value: &geometry::T) -> Result<i64, error::T> {
    match owner {
        attribute::Owner::Point => Ok(geometry::point_count(geometry_value)),
        attribute::Owner::Vertex => Ok(geometry::vertex_count(geometry_value)),
        attribute::Owner::Primitive => Ok(geometry::primitive_count(geometry_value)),
        attribute::Owner::Detail => Err(detail_refused()),
    }
}

fn validate_group(label: &str, owner: attribute::Owner, count: i64, value: Option<&group::T>) -> Result<(), error::T> {
    match value {
        Some(value) if group::owner(value) != group_owner(owner)? || group::length(value) != count => {
            Err(fail(&format!("Attribute Mirror {label} must be a matching {} group", owner_name(owner)?)))
        }
        _ => Ok(()),
    }
}

fn validate_name(label: &str, value: &Option<String>) -> Result<(), error::T> {
    match value {
        Some(name) if is_blank(name) => Err(fail(&format!("Attribute Mirror {label} must be non-empty"))),
        _ => Ok(()),
    }
}

fn resolve_plane(
    origin: rays_math::vec3::T,
    normal: rays_math::vec3::T,
    distance: f64,
    tolerance: f64,
) -> Result<Plane, error::T> {
    let (ox, oy, oz) = (origin.x, origin.y, origin.z);
    let (nx, ny, nz) = (normal.x, normal.y, normal.z);
    if ![ox, oy, oz, nx, ny, nz, distance, tolerance].iter().all(|value| value.is_finite()) {
        return Err(fail("Attribute Mirror plane parameters must be finite"));
    }
    if tolerance < 0.0 || tolerance > f64::MAX.sqrt() {
        return Err(fail("Attribute Mirror tolerance must be non-negative and safely squarable"));
    }
    let scale = nx.abs().max(ny.abs().max(nz.abs()));
    if scale == 0.0 {
        return Err(fail("Attribute Mirror plane normal must be non-zero"));
    }
    let (sx, sy, sz) = (nx / scale, ny / scale, nz / scale);
    let length = ((sx * sx) + (sy * sy) + (sz * sz)).sqrt();
    let (nx, ny, nz) = (sx / length, sy / length, sz / length);
    let (ox, oy, oz) = (ox + (distance * nx), oy + (distance * ny), oz + (distance * nz));
    if !(ox.is_finite() && oy.is_finite() && oz.is_finite()) {
        return Err(fail("Attribute Mirror displaced plane origin is not representable"));
    }
    Ok(Plane { ox, oy, oz, nx, ny, nz, tolerance })
}

fn primitive_locations(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<packed::float3::T, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    let count = geometry::primitive_count(geometry_value) as usize;
    let zeros = vec![0.0_f64; count];
    let output = map_planes(cancel_token, grain, &[&zeros, &zeros, &zeros], &|primitive, slot| {
        let bad = || fail(&format!("Attribute Mirror primitive {primitive} has no finite bounding-box center"));
        let (first, last) = topology::primitive_vertex_range(topology_value, primitive)?;
        if last <= first {
            return Err(bad());
        }
        let point = topology::point_of_vertex(topology_value, first)? as usize;
        let (mut min_x, mut max_x) = (positions.x[point], positions.x[point]);
        let (mut min_y, mut max_y) = (positions.y[point], positions.y[point]);
        let (mut min_z, mut max_z) = (positions.z[point], positions.z[point]);
        for vertex in first + 1..last {
            let point = topology::point_of_vertex(topology_value, vertex)? as usize;
            let (px, py, pz) = (positions.x[point], positions.y[point], positions.z[point]);
            if px < min_x {
                min_x = px;
            }
            if px > max_x {
                max_x = px;
            }
            if py < min_y {
                min_y = py;
            }
            if py > max_y {
                max_y = py;
            }
            if pz < min_z {
                min_z = pz;
            }
            if pz > max_z {
                max_z = pz;
            }
        }
        let cx = (min_x * 0.5) + (max_x * 0.5);
        let cy = (min_y * 0.5) + (max_y * 0.5);
        let cz = (min_z * 0.5) + (max_z * 0.5);
        if cx.is_finite() && cy.is_finite() && cz.is_finite() {
            slot[0] = cx;
            slot[1] = cy;
            slot[2] = cz;
            Ok(())
        } else {
            Err(bad())
        }
    })?;
    let mut planes = output.into_iter();
    let (x, y, z) = (planes.next().unwrap_or_default(), planes.next().unwrap_or_default(), planes.next().unwrap_or_default());
    packed::float3::view::of_owned_exn(x, y, z)
}

fn locations(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    owner: attribute::Owner,
    geometry_value: &geometry::T,
) -> Result<packed::float3::T, error::T> {
    match owner {
        attribute::Owner::Point => Ok(geometry::positions(geometry_value).clone()),
        attribute::Owner::Primitive => primitive_locations(cancel_token, grain, geometry_value),
        attribute::Owner::Vertex => {
            Err(fail("Attribute Mirror plane correspondence does not yet support vertex attributes"))
        }
        attribute::Owner::Detail => Err(detail_refused()),
    }
}

fn plane_distance(plane: &Plane, x: f64, y: f64, z: f64) -> f64 {
    ((x - plane.ox) * plane.nx) + ((y - plane.oy) * plane.ny) + ((z - plane.oz) * plane.nz)
}

fn selected_for_side(value: Option<&group::T>, group_use: GroupUse, source: bool, element: i64) -> bool {
    match value {
        None => true,
        Some(value) => {
            let member = group::mem(element, value);
            match (group_use, source) {
                (GroupUse::MirrorGroupAsSource, true) | (GroupUse::MirrorGroupAsDestination, false) => member,
                (GroupUse::MirrorGroupAsSource, false) | (GroupUse::MirrorGroupAsDestination, true) => !member,
            }
        }
    }
}

fn finalize_mapping(plane: Option<Plane>, mut source_of_output: Vec<i64>) -> Mapping {
    let mut pair_destination = vec![0_u8; source_of_output.len()];
    for destination in 0..source_of_output.len() {
        if source_of_output[destination] >= 0 {
            pair_destination[destination] = 1;
        } else {
            source_of_output[destination] = destination as i64;
        }
    }
    Mapping { source_of_output, pair_destination, plane }
}

fn plane_mapping(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    value: Option<&group::T>,
    group_use: GroupUse,
    owner: attribute::Owner,
    plane: Plane,
    geometry_value: &geometry::T,
) -> Result<Mapping, error::T> {
    let count = owner_count(owner, geometry_value)?;
    let located = locations(cancel_token, grain, owner, geometry_value)?;
    let view = packed::float3::view::view(&located)?;
    let label = owner_name(owner)?;
    let zeros = vec![0_i64; count as usize];
    let side = map_ints(cancel_token, grain, &zeros, &|element, slot| {
        let distance = plane_distance(&plane, view.x[element as usize], view.y[element as usize], view.z[element as usize]);
        if !distance.is_finite() {
            return Err(fail(&format!("Attribute Mirror {label} {element} has an unrepresentable plane distance")));
        }
        if distance < 0.0 && selected_for_side(value, group_use, true, element) {
            *slot = 1;
        } else if distance > 0.0 && selected_for_side(value, group_use, false, element) {
            *slot = 2;
        }
        Ok(())
    })?;
    let source_elements: Vec<i64> = (0..count).filter(|&element| side[element as usize] == 1).collect();
    let destination_elements: Vec<i64> = (0..count).filter(|&element| side[element as usize] == 2).collect();
    let (mut x, mut y, mut z) = (Vec::new(), Vec::new(), Vec::new());
    for &element in &source_elements {
        let e = element as usize;
        let (px, py, pz) = (view.x[e], view.y[e], view.z[e]);
        let distance = plane_distance(&plane, px, py, pz);
        let (rx, ry, rz) = (
            px - (2.0 * distance * plane.nx),
            py - (2.0 * distance * plane.ny),
            pz - (2.0 * distance * plane.nz),
        );
        if !(rx.is_finite() && ry.is_finite() && rz.is_finite()) {
            return Err(fail(&format!("Attribute Mirror reflected {label} {element} is not representable")));
        }
        x.push(rx);
        y.push(ry);
        z.push(rz);
    }
    let reflected = packed::float3::view::of_owned_exn(x, y, z)?;
    let index = spatial_index::create(cancel_token, Some(grain), None, &reflected)
        .map_err(|failure| fail(&error::to_string(&failure)))?;
    let (mut query_x, mut query_y, mut query_z) = (Vec::new(), Vec::new(), Vec::new());
    for &element in &destination_elements {
        let e = element as usize;
        query_x.push(view.x[e]);
        query_y.push(view.y[e]);
        query_z.push(view.z[e]);
    }
    let queries = packed::float3::view::of_owned_exn(query_x, query_y, query_z)?;
    let n = destination_elements.len();
    let mut nearest = vec![-1_i64; n];
    let mut distances = vec![f64::INFINITY; n];
    let mut counts = vec![0_i64; n];
    spatial_index::view::nearest_k_many_into(
        cancel_token,
        None,
        grain,
        &index,
        &queries,
        plane.tolerance * plane.tolerance,
        1,
        &mut nearest,
        &mut distances,
        &mut counts,
    )?;
    let mut source_of_output = vec![-1_i64; count as usize];
    for slot in 0..n {
        if counts[slot] == 1 {
            source_of_output[destination_elements[slot] as usize] = source_elements[nearest[slot] as usize];
        }
    }
    Ok(finalize_mapping(Some(plane), source_of_output))
}

fn explicit_mapping(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    value: Option<&group::T>,
    group_use: GroupUse,
    owner: attribute::Owner,
    mapping_attribute: &str,
    destination_group: &group::T,
    geometry_value: &geometry::T,
) -> Result<Mapping, error::T> {
    let count = owner_count(owner, geometry_value)?;
    let owner_of_attribute = attribute_owner(owner)?;
    if is_blank(mapping_attribute) {
        return Err(fail("Attribute Mirror mapping attribute must be non-empty"));
    }
    validate_group("mapping destination group", owner, count, Some(destination_group))?;
    let Some(found) = geometry::find_attribute(owner_of_attribute, mapping_attribute, geometry_value) else {
        return Err(fail(&format!(
            "Attribute Mirror could not find {} mapping attribute {}",
            owner_name(owner)?,
            ocaml_quote(mapping_attribute)
        )));
    };
    let values = match attribute::view::storage(&found)? {
        attribute::Storage::Int(values) if values.len() as i64 == count => values.clone(),
        attribute::Storage::Int(_) => return Err(fail("Attribute Mirror mapping cardinality mismatch")),
        _ => return Err(fail("Attribute Mirror mapping attribute must be integer")),
    };
    let init = vec![-1_i64; count as usize];
    let source_of_output = map_ints(cancel_token, grain, &init, &|destination, slot| {
        if group::mem(destination, destination_group) {
            let source = values[destination as usize];
            if source >= 0 && source < count {
                let selected = match value {
                    None => true,
                    Some(value) => match group_use {
                        GroupUse::MirrorGroupAsSource => group::mem(source, value),
                        GroupUse::MirrorGroupAsDestination => group::mem(destination, value),
                    },
                };
                if selected {
                    *slot = source;
                }
            }
        }
        Ok(())
    })?;
    Ok(finalize_mapping(None, source_of_output))
}

fn mapping_changed(mapping: &Mapping) -> bool {
    mapping.pair_destination.iter().any(|&flag| flag == 1)
}

fn output_mapping_values(mapping: &Mapping) -> Vec<i64> {
    let mut output = vec![-1_i64; mapping.source_of_output.len()];
    for destination in 0..output.len() {
        if mapping.pair_destination[destination] == 1 {
            let source = mapping.source_of_output[destination];
            output[destination] = source;
            output[source as usize] = source;
        }
    }
    output
}

fn source_members(mapping: &Mapping) -> Vec<u8> {
    let mut members = vec![0_u8; mapping.source_of_output.len()];
    for destination in 0..members.len() {
        if mapping.pair_destination[destination] == 1 {
            members[mapping.source_of_output[destination] as usize] = 1;
        }
    }
    members
}

fn copy_ints(cancel_token: Option<&cancel::T>, grain: i64, mapping: &Mapping, values: &[i64]) -> Result<Vec<i64>, error::T> {
    if values.is_empty() {
        return Ok(Vec::new());
    }
    let zeros = vec![0_i64; mapping.source_of_output.len()];
    map_ints(cancel_token, grain, &zeros, &|element, slot| {
        *slot = values[mapping.source_of_output[element as usize] as usize];
        Ok(())
    })
}

fn copy_texts(cancel_token: Option<&cancel::T>, grain: i64, mapping: &Mapping, values: &TextArray) -> Result<TextArray, error::T> {
    if values.is_empty() {
        return Ok(TextArray::new());
    }
    let blanks: TextArray = std::iter::repeat_n(&[][..], mapping.source_of_output.len()).collect();
    map_texts(cancel_token, grain, &blanks, &|element, slot| {
        *slot = values[mapping.source_of_output[element as usize] as usize].to_vec();
        Ok(())
    })
}

fn copy_float(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    mapping: &Mapping,
    label: &str,
    values: &[f64],
) -> Result<Vec<f64>, error::T> {
    let zeros = vec![0.0_f64; mapping.source_of_output.len()];
    let mut output = map_planes(cancel_token, grain, &[&zeros], &|element, slot| {
        let value = values[mapping.source_of_output[element as usize] as usize];
        if value.is_finite() {
            slot[0] = value;
            Ok(())
        } else {
            Err(fail(&format!("Attribute Mirror {label} is non-finite at element {element}")))
        }
    })?;
    Ok(output.pop().unwrap_or_default())
}

fn reflected_uv(transform: Transform, u: f64, v: f64) -> (f64, f64) {
    match transform {
        Transform::MirrorUv { origin_u, origin_v, direction_u, direction_v } => {
            let (du, dv) = (u - origin_u, v - origin_v);
            let (normal_u, normal_v) = (-direction_v, direction_u);
            let distance = (du * normal_u) + (dv * normal_v);
            (u - (2.0 * distance * normal_u), v - (2.0 * distance * normal_v))
        }
        _ => (u, v),
    }
}

fn reflected_xyz(transform: Transform, plane: Option<Plane>, x: f64, y: f64, z: f64) -> (f64, f64, f64) {
    match (transform, plane) {
        (Transform::MirrorVector, Some(plane)) => {
            let distance = (x * plane.nx) + (y * plane.ny) + (z * plane.nz);
            (
                x - (2.0 * distance * plane.nx),
                y - (2.0 * distance * plane.ny),
                z - (2.0 * distance * plane.nz),
            )
        }
        (Transform::MirrorPoint, Some(plane)) => {
            let distance = plane_distance(&plane, x, y, z);
            (
                x - (2.0 * distance * plane.nx),
                y - (2.0 * distance * plane.ny),
                z - (2.0 * distance * plane.nz),
            )
        }
        _ => (x, y, z),
    }
}

fn copy_tuple(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    mapping: &Mapping,
    transform: Transform,
    label: &str,
    source: &[&[f64]],
) -> Result<Vec<Vec<f64>>, error::T> {
    let width = source.len();
    let zeros = vec![0.0_f64; mapping.source_of_output.len()];
    let init: Vec<&[f64]> = (0..width).map(|_| zeros.as_slice()).collect();
    map_planes(cancel_token, grain, &init, &|element, slot| {
        let bad = || fail(&format!("Attribute Mirror {label} is or becomes non-finite at element {element}"));
        let source_element = mapping.source_of_output[element as usize] as usize;
        if (0..width).any(|component| !source[component][source_element].is_finite()) {
            return Err(bad());
        }
        for component in 0..width {
            slot[component] = source[component][source_element];
        }
        if mapping.pair_destination[element as usize] == 1 {
            match transform {
                Transform::MirrorUv { .. } if width >= 2 => {
                    let (u, v) = reflected_uv(transform, slot[0], slot[1]);
                    slot[0] = u;
                    slot[1] = v;
                }
                Transform::MirrorVector | Transform::MirrorPoint if width >= 3 => {
                    let (x, y, z) = reflected_xyz(transform, mapping.plane, slot[0], slot[1], slot[2]);
                    slot[0] = x;
                    slot[1] = y;
                    slot[2] = z;
                }
                _ => {}
            }
            if (0..width).any(|component| !slot[component].is_finite()) {
                return Err(bad());
            }
        }
        Ok(())
    })
}

fn remap_attribute(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    transform: Transform,
    string_replace: &Option<(Vec<u8>, Vec<u8>)>,
    mapping: &Mapping,
    item: &attribute::T,
) -> Result<attribute::T, error::T> {
    let name = attribute::name(item);
    let owner = attribute::owner(item);
    let label = format!(
        "{} attribute {}",
        match owner {
            attribute::Owner::Point => "point",
            attribute::Owner::Vertex => "vertex",
            attribute::Owner::Primitive => "primitive",
            attribute::Owner::Detail => "detail",
        },
        ocaml_quote(name)
    );
    let source = attribute::view::storage(item)?;
    let storage = match source {
        attribute::Storage::Float(values) => {
            attribute::Storage::Float(Arc::new(copy_float(cancel_token, grain, mapping, &label, values)?))
        }
        attribute::Storage::Int(values) => attribute::Storage::Int(Arc::new(copy_ints(cancel_token, grain, mapping, values)?)),
        attribute::Storage::Text(values) => {
            let mut output = copy_texts(cancel_token, grain, mapping, values)?;
            if let Some((search, replacement)) = string_replace {
                let mut replaced = TextArray::with_capacity(output.len(), output.byte_len());
                for (element, text) in output.iter().enumerate() {
                    let mut remaining = text;
                    let mut next = remaining.windows(search.len()).position(|window| window == search.as_slice());
                    if mapping.pair_destination[element] != 1 || next.is_none() {
                        replaced.push(text);
                        continue;
                    }
                    let mut row = Vec::with_capacity(text.len());
                    while let Some(at) = next {
                        row.extend_from_slice(&remaining[..at]);
                        row.extend_from_slice(replacement);
                        remaining = &remaining[at + search.len()..];
                        next = remaining.windows(search.len()).position(|window| window == search.as_slice());
                    }
                    row.extend_from_slice(remaining);
                    replaced.push(&row);
                }
                output = replaced;
            }
            attribute::Storage::Text(Arc::new(output))
        }
        attribute::Storage::Float2(_) | attribute::Storage::Float3(_) | attribute::Storage::Float4(_) => {
            let planes = attribute::float_planes(source)?.expect("a tuple storage has float planes");
            let output = copy_tuple(cancel_token, grain, mapping, transform, &label, &planes)?;
            attribute::of_float_planes(source, output)?
        }
        attribute::Storage::IntArray(values) => attribute::Storage::IntArray(ragged_ops::remap_int(
            cancel_token,
            Some(grain),
            &mapping.source_of_output,
            values,
        )?),
        attribute::Storage::FloatArray(values) => attribute::Storage::FloatArray(ragged_ops::remap_float(
            cancel_token,
            Some(grain),
            &mapping.source_of_output,
            values,
        )?),
    };
    attribute::create_owned(name, owner, storage)
}

fn position_attribute(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    transform: Transform,
    mapping: &Mapping,
    geometry_value: &geometry::T,
) -> Result<packed::float3::T, error::T> {
    let values = packed::float3::view::view(geometry::positions(geometry_value))?;
    let output = copy_tuple(
        cancel_token,
        grain,
        mapping,
        transform,
        "point attribute P",
        &[&values.x, &values.y, &values.z],
    )?;
    let mut planes = output.into_iter();
    packed::float3::view::of_owned_exn(
        planes.next().unwrap_or_default(),
        planes.next().unwrap_or_default(),
        planes.next().unwrap_or_default(),
    )
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    run_inner(params, geometry_value)
        .map_err(|failure| error::guard(failure, OPERATION, CODE))
}

fn run_inner(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let owner = params.owner;
    support::check_positive(grain).map_err(|_| fail("Attribute Mirror grain must be positive"))?;
    let count = owner_count(owner, geometry_value)?;
    validate_group("group", owner, count, params.group.as_ref())?;
    validate_name("output mapping name", &params.output_mapping)?;
    validate_name("source group name", &params.source_group)?;
    validate_name("destination group name", &params.destination_group)?;
    if let (Some(source), Some(destination)) = (&params.source_group, &params.destination_group) {
        if source == destination {
            return Err(fail("Attribute Mirror source and destination group names must differ"));
        }
    }
    if let Some((search, _)) = &params.string_replace {
        if search.is_empty() {
            return Err(fail("Attribute Mirror string search must be non-empty"));
        }
    }
    let pattern = attribute_pattern::compile(&params.attributes)
        .map_err(|failure| fail(error::message(&failure)))?;
    let transform = match params.transform {
        Transform::MirrorUv { origin_u, origin_v, direction_u, direction_v } => {
            if !(origin_u.is_finite() && origin_v.is_finite() && direction_u.is_finite() && direction_v.is_finite()) {
                return Err(fail("Attribute Mirror UV line must be finite"));
            }
            let scale = direction_u.abs().max(direction_v.abs());
            if scale == 0.0 {
                return Err(fail("Attribute Mirror UV direction must be non-zero"));
            }
            let (u, v) = (direction_u / scale, direction_v / scale);
            let length = ((u * u) + (v * v)).sqrt();
            Transform::MirrorUv { origin_u, origin_v, direction_u: u / length, direction_v: v / length }
        }
        other => other,
    };
    let mapping = match &params.method_ {
        Method::MirrorByPlane { origin, normal, distance, tolerance } => {
            let plane = resolve_plane(*origin, *normal, *distance, *tolerance)?;
            plane_mapping(cancel_token, grain, params.group.as_ref(), params.group_use, owner, plane, geometry_value)?
        }
        Method::MirrorByMapping { mapping_attribute, destination_group } => {
            if matches!(transform, Transform::MirrorVector | Transform::MirrorPoint) {
                return Err(fail("Attribute Mirror vector/point transformation requires plane correspondence"));
            }
            explicit_mapping(
                cancel_token,
                grain,
                params.group.as_ref(),
                params.group_use,
                owner,
                mapping_attribute,
                destination_group,
                geometry_value,
            )?
        }
    };
    let owner_of_attribute = attribute_owner(owner)?;
    let changed = mapping_changed(&mapping);
    let mirror_position =
        owner == attribute::Owner::Point && changed && attribute_pattern::matches(&pattern, "P");
    let selected: Vec<attribute::T> = geometry::attributes(geometry_value)
        .into_iter()
        .filter(|item| {
            attribute::owner(item) == owner_of_attribute && attribute_pattern::matches(&pattern, attribute::name(item))
        })
        .collect();
    let mut attributes: Vec<attribute::T> = Vec::new();
    if changed {
        for item in &selected {
            attributes.push(remap_attribute(
                cancel_token,
                grain,
                transform,
                &params.string_replace,
                &mapping,
                item,
            )?);
        }
    }
    let positions = if mirror_position {
        Some(position_attribute(cancel_token, grain, transform, &mapping, geometry_value)?)
    } else {
        None
    };
    let had_attributes = !attributes.is_empty();
    let mut generated: Vec<attribute::T> = attributes;
    if let Some(name) = &params.output_mapping {
        let item = attribute::create_owned(
            name,
            owner_of_attribute,
            attribute::Storage::Int(Arc::new(output_mapping_values(&mapping))),
        )?;
        generated.insert(0, item);
    }
    // OCaml builds this list by consing and reverses it once more at the end.
    generated.reverse();
    let mut groups: Vec<group::T> = Vec::new();
    if let Some(name) = &params.source_group {
        let members = source_members(&mapping);
        groups.push(group::init(Some(grain), group_owner(owner)?, name, count, &|element| {
            members[element as usize] == 1
        })?);
    }
    if let Some(name) = &params.destination_group {
        groups.push(group::init(Some(grain), group_owner(owner)?, name, count, &|element| {
            mapping.pair_destination[element as usize] == 1
        })?);
    }
    if !changed && positions.is_none() && !had_attributes && params.output_mapping.is_none() && groups.is_empty() {
        return Ok(geometry_value.clone());
    }
    let has_positions = positions.is_some();
    let output = geometry::view::with_merged_owned(positions, generated, groups, geometry_value)?;
    if !has_positions {
        return Ok(output);
    }
    let mirrored_normal = |owner: attribute::Owner| {
        selected.iter().any(|item| attribute::owner(item) == owner && attribute::name(item) == "N")
    };
    let output = if mirrored_normal(attribute::Owner::Point) {
        output
    } else {
        geometry::without_attribute(attribute::Owner::Point, "N", &output)
    };
    Ok(if mirrored_normal(attribute::Owner::Vertex) {
        output
    } else {
        geometry::without_attribute(attribute::Owner::Vertex, "N", &output)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::attribute_ops::{fixtures::*, fixtures2::*};
    use attribute::Owner::{Point, Primitive, Vertex};
    use rdk_core::topology::PrimitiveKind;

    fn line(xs: &[f64]) -> geometry::T {
        points(&xs.iter().map(|&x| (x, 0.0, 0.0)).collect::<Vec<_>>())
    }

    fn make_group(owner: group::Owner, name: &str, length: i64, members: &[i64]) -> group::T {
        group_of(owner, name, members, length)
    }

    fn params(owner: attribute::Owner, method_: Method) -> Params {
        Params {
            cancel: None,
            grain: None,
            group: None,
            group_use: GroupUse::MirrorGroupAsSource,
            attributes: "Cd".to_owned(),
            transform: Transform::MirrorCopy,
            string_replace: None,
            output_mapping: None,
            source_group: None,
            destination_group: None,
            owner,
            method_,
        }
    }

    fn mapping(attribute_name: &str, destination: &group::T) -> Method {
        Method::MirrorByMapping { mapping_attribute: attribute_name.to_owned(), destination_group: destination.clone() }
    }

    fn plane(tolerance: f64) -> Method {
        Method::MirrorByPlane {
            origin: rays_math::vec3::zero(),
            normal: rays_math::vec3::unit_x(),
            distance: 0.0,
            tolerance,
        }
    }

    fn point_fixture() -> geometry::T {
        let g = line(&[-2.0, -1.0, 1.0, 2.0]);
        let g = add_ints(Point, "map", vec![-1, -1, 1, 0], &g);
        let g = add_values(Point, "value", vec![10.0, 20.0, 0.0, 0.0], &g);
        let g = add_ints(Point, "id", vec![10, 20, 0, 0], &g);
        let g = add_storage(
            Point,
            "label",
            attribute::Storage::Text(Arc::new(["left_A", "left_B", "old", "old"].iter().map(|s| s.as_bytes()).collect())),
            &g,
        );
        let g = add_storage(
            Point,
            "uv",
            attribute::Storage::Float2(packed::float2::of_owned(vec![-0.2, -0.4, 0.0, 0.0], vec![0.1, 0.2, 0.0, 0.0]).unwrap()),
            &g,
        );
        let g = add_storage(
            Point,
            "Cd",
            attribute::Storage::Float4(
                packed::float4::of_owned(
                    vec![1.0, 0.5, 0.0, 0.0],
                    vec![0.0, 0.2, 0.0, 0.0],
                    vec![0.1, 0.3, 0.0, 0.0],
                    vec![1.0, 1.0, 0.0, 0.0],
                )
                .unwrap(),
            ),
            &g,
        );
        let g = add_storage(
            Point,
            "rows",
            attribute::Storage::IntArray(packed::int_array::create_owned(vec![0, 1, 3, 3, 3], vec![7, 8, 9]).unwrap()),
            &g,
        );
        geometry::with_group(make_group(group::Owner::Point, "destination", 4, &[2, 3]), &g).unwrap()
    }

    fn destination_of(g: &geometry::T) -> group::T {
        geometry::find_group(group::Owner::Point, "destination", g).unwrap()
    }

    #[test]
    fn explicit_mapping_covers_every_storage() {
        let source = point_fixture();
        let mut p = params(Point, mapping("map", &destination_of(&source)));
        p.attributes = "value id label uv Cd rows".to_owned();
        p.string_replace = Some((b"left".to_vec(), b"right".to_vec()));
        p.transform = Transform::MirrorUv { origin_u: 0.0, origin_v: 0.0, direction_u: 0.0, direction_v: 1.0 };
        p.output_mapping = Some("mirror_pair".to_owned());
        p.source_group = Some("mirror_source".to_owned());
        p.destination_group = Some("mirror_destination".to_owned());
        let output = run(&p, &source).unwrap();
        assert_eq!(floats(Point, "value", &output), vec![10.0, 20.0, 20.0, 10.0]);
        assert_eq!(ints(Point, "id", &output), vec![10, 20, 20, 10]);
        assert_eq!(
            texts(Point, "label", &output),
            vec![b"left_A".to_vec(), b"left_B".to_vec(), b"right_B".to_vec(), b"right_A".to_vec()]
        );
        let uv = match attribute::storage(&find(Point, "uv", &output)).unwrap() {
            attribute::Storage::Float2(values) => packed::float2::view::view(&values),
            _ => panic!("uv storage"),
        };
        assert_eq!(&*uv.x, &[-0.2, -0.4, 0.4, 0.2]);
        assert_eq!(&*uv.y, &[0.1, 0.2, 0.2, 0.1]);
        let rows = match attribute::storage(&find(Point, "rows", &output)).unwrap() {
            attribute::Storage::IntArray(values) => packed::int_array::view::view(&values),
            _ => panic!("rows storage"),
        };
        assert_eq!(&*rows.offsets, &[0, 1, 3, 5, 6]);
        assert_eq!(&*rows.values, &[7, 8, 9, 8, 9, 7]);
        assert_eq!(ints(Point, "mirror_pair", &output), vec![0, 1, 1, 0]);
        let source_group = geometry::find_group(group::Owner::Point, "mirror_source", &output).unwrap();
        let destination_group = geometry::find_group(group::Owner::Point, "mirror_destination", &output).unwrap();
        assert_eq!(group::cardinality(&source_group), 2);
        assert!(group::mem(0, &source_group) && group::mem(1, &source_group));
        assert_eq!(group::cardinality(&destination_group), 2);
        assert!(group::mem(2, &destination_group) && group::mem(3, &destination_group));
        assert_eq!(
            topology::data_id(geometry::topology(&output)),
            topology::data_id(geometry::topology(&source))
        );
    }

    #[test]
    fn plane_points_and_transforms() {
        let g = line(&[-2.0, -1.0, 0.9, 2.1]);
        let g = add_values(Point, "value", vec![10.0, 20.0, 0.0, 0.0], &g);
        let g = add_float3s(Point, "N", vec![1.0, 2.0, 0.0, 0.0], vec![0.0; 4], vec![0.0; 4], &g);
        let g = add_float3s(Point, "rest", vec![-2.0, -1.0, 0.0, 0.0], vec![1.0, 2.0, 0.0, 0.0], vec![0.0; 4], &g);
        let mut copy = params(Point, plane(0.11));
        copy.attributes = "value".to_owned();
        assert_eq!(floats(Point, "value", &run(&copy, &g).unwrap()), vec![10.0, 20.0, 20.0, 10.0]);
        let mut vector = params(Point, plane(0.11));
        vector.attributes = "N".to_owned();
        vector.transform = Transform::MirrorVector;
        let (nx, _, _) = float3(Point, "N", &run(&vector, &g).unwrap());
        assert_eq!(nx, vec![1.0, 2.0, -2.0, -1.0]);
        let mut point = params(Point, plane(0.11));
        point.attributes = "P rest".to_owned();
        point.transform = Transform::MirrorPoint;
        let out = run(&point, &g).unwrap();
        let (px, _, _) = positions(&out);
        let (rx, _, _) = float3(Point, "rest", &out);
        assert_eq!(px, vec![-2.0, -1.0, 1.0, 2.0]);
        assert_eq!(rx, vec![-2.0, -1.0, 1.0, 2.0]);
    }

    fn paired_triangles() -> geometry::T {
        let g = mesh(
            &[(-2.0, 0.0, 0.0), (-1.0, -1.0, 0.0), (-1.0, 1.0, 0.0), (1.0, -1.0, 0.0), (1.0, 1.0, 0.0), (2.0, 0.0, 0.0)],
            &[(PrimitiveKind::Polygon, vec![0, 1, 2]), (PrimitiveKind::Polygon, vec![3, 4, 5])],
        );
        let g = add_ints(Vertex, "vertex_map", vec![-1, -1, -1, 2, 1, 0], &g);
        let g = add_values(Vertex, "v", vec![1.0, 2.0, 3.0, 0.0, 0.0, 0.0], &g);
        let g = add_ints(Primitive, "primitive_map", vec![-1, 0], &g);
        let g = add_values(Primitive, "p", vec![9.0, 0.0], &g);
        let g = geometry::with_group(make_group(group::Owner::Vertex, "vertex_dest", 6, &[3, 4, 5]), &g).unwrap();
        geometry::with_group(make_group(group::Owner::Primitive, "primitive_dest", 2, &[1]), &g).unwrap()
    }

    #[test]
    fn vertex_primitive_and_plane_primitive_correspondence() {
        let g = paired_triangles();
        let vertex_dest = geometry::find_group(group::Owner::Vertex, "vertex_dest", &g).unwrap();
        let primitive_dest = geometry::find_group(group::Owner::Primitive, "primitive_dest", &g).unwrap();
        let mut vertices = params(Vertex, mapping("vertex_map", &vertex_dest));
        vertices.attributes = "v".to_owned();
        assert_eq!(floats(Vertex, "v", &run(&vertices, &g).unwrap()), vec![1.0, 2.0, 3.0, 3.0, 2.0, 1.0]);
        let mut primitives = params(Primitive, mapping("primitive_map", &primitive_dest));
        primitives.attributes = "p".to_owned();
        assert_eq!(floats(Primitive, "p", &run(&primitives, &g).unwrap()), vec![9.0, 9.0]);
        let mut by_plane = params(Primitive, plane(1e-12));
        by_plane.attributes = "p".to_owned();
        assert_eq!(floats(Primitive, "p", &run(&by_plane, &g).unwrap()), vec![9.0, 9.0]);
    }

    #[test]
    fn group_policies_and_no_op_identity() {
        let g = point_fixture();
        let method = mapping("map", &destination_of(&g));
        let mut source = params(Point, method.clone());
        source.group = Some(make_group(group::Owner::Point, "only_source", 4, &[0]));
        source.attributes = "value".to_owned();
        assert_eq!(floats(Point, "value", &run(&source, &g).unwrap()), vec![10.0, 20.0, 0.0, 10.0]);
        let mut destination = params(Point, method.clone());
        destination.group = Some(make_group(group::Owner::Point, "only_destination", 4, &[2]));
        destination.group_use = GroupUse::MirrorGroupAsDestination;
        destination.attributes = "value".to_owned();
        assert_eq!(floats(Point, "value", &run(&destination, &g).unwrap()), vec![10.0, 20.0, 20.0, 0.0]);
        let mut empty = params(Point, method);
        empty.group = Some(make_group(group::Owner::Point, "empty", 4, &[]));
        empty.group_use = GroupUse::MirrorGroupAsDestination;
        empty.attributes = "*".to_owned();
        let unchanged = run(&empty, &g).unwrap();
        assert_eq!(geometry::data_id(&unchanged), geometry::data_id(&g));
    }

    #[test]
    fn errors_and_cancellation_are_structured() {
        let g = point_fixture();
        let method = mapping("map", &destination_of(&g));
        let expect = |p: Params, source: &geometry::T, wanted: &str| {
            assert_eq!(err_code(run(&p, source)), wanted);
        };
        let base = || params(Point, method.clone());
        let mut p = base();
        p.grain = Some(0);
        expect(p, &g, "invalid_attribute_mirror");
        let mut p = base();
        p.attributes = "[".to_owned();
        expect(p, &g, "invalid_attribute_mirror");
        let mut p = base();
        p.transform = Transform::MirrorVector;
        expect(p, &g, "invalid_attribute_mirror");
        let mut p = base();
        p.string_replace = Some((Vec::new(), b"x".to_vec()));
        expect(p, &g, "invalid_attribute_mirror");
        expect(params(Vertex, plane(1.0)), &g, "invalid_attribute_mirror");
        let zero_normal = Method::MirrorByPlane {
            origin: rays_math::vec3::zero(),
            normal: rays_math::vec3::zero(),
            distance: 0.0,
            tolerance: 1.0,
        };
        expect(params(Point, zero_normal), &g, "invalid_attribute_mirror");
        expect(params(Point, plane(-1.0)), &g, "invalid_attribute_mirror");
        let mut p = base();
        p.transform = Transform::MirrorUv { origin_u: 0.0, origin_v: 0.0, direction_u: 0.0, direction_v: 0.0 };
        expect(p, &g, "invalid_attribute_mirror");
        let mut p = base();
        p.source_group = Some("same".to_owned());
        p.destination_group = Some("same".to_owned());
        expect(p, &g, "invalid_attribute_mirror");
        let mut p = base();
        p.group = Some(make_group(group::Owner::Primitive, "wrong", 0, &[]));
        expect(p, &g, "invalid_attribute_mirror");
        let bad = add_values(Point, "value", vec![1.0, f64::NAN, 0.0, 0.0], &g);
        let mut p = base();
        p.attributes = "value".to_owned();
        expect(p, &bad, "invalid_attribute_mirror");
        let wrong_mapping = add_values(Point, "map", vec![-1.0, -1.0, 1.0, 0.0], &g);
        expect(params(Point, mapping("map", &destination_of(&g))), &wrong_mapping, "invalid_attribute_mirror");
        let mut p = base();
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        p.cancel = Some(token);
        expect(p, &g, "cancelled");
    }

    #[test]
    fn parallel_scale_is_exact_across_domains() {
        let count = 100_000_i64;
        let half = 50_000_i64;
        let xs: Vec<f64> = (0..count)
            .map(|point| if point < half { -((half - point) as f64) } else { (point - half + 1) as f64 })
            .collect();
        let g = line(&xs);
        let g = add_ints(Point, "map", (0..count).map(|p| if p < half { -1 } else { count - p - 1 }).collect(), &g);
        let g = add_storage(
            Point,
            "Cd",
            attribute::Storage::Float4(
                packed::float4::of_owned(
                    (0..count).map(|p| (p % 251) as f64).collect(),
                    (0..count).map(|p| (p % 127) as f64).collect(),
                    (0..count).map(|p| (p % 67) as f64).collect(),
                    vec![1.0; count as usize],
                )
                .unwrap(),
            ),
            &g,
        );
        let members: Vec<i64> = (half..count).collect();
        let dest = make_group(group::Owner::Point, "dest", count, &members);
        let g = geometry::with_group(dest.clone(), &g).unwrap();
        let run_with = |domains: i64| {
            on_domains(domains, &|| {
                let mut p = params(Point, mapping("map", &dest));
                p.grain = Some(127);
                p.output_mapping = Some("pair".to_owned());
                run(&p, &g).unwrap()
            })
        };
        let (one, four) = (run_with(1), run_with(4));
        assert_eq!(signature(&one), signature(&four));
        assert_eq!(geometry::point_count(&one), count);
        assert_eq!(
            topology::data_id(geometry::topology(&one)),
            topology::data_id(geometry::topology(&g))
        );
    }

    #[test]
    fn text_replacement_preserves_arbitrary_bytes() {
        let rows = vec![b"\xff\0\xff\0".to_vec(), b"\0\x80\xff\0z".to_vec(), Vec::new(), Vec::new()];
        let source = add_storage(Point, "label", attribute::Storage::Text(Arc::new(rows.clone().into())), &point_fixture());
        let mut p = params(Point, mapping("map", &destination_of(&source)));
        p.attributes = "label".to_owned();
        p.string_replace = Some((b"\xff\0".to_vec(), b"\x80\xff".to_vec()));
        let output = run(&p, &source).unwrap();
        assert_eq!(texts(Point, "label", &output), vec![rows[0].clone(), rows[1].clone(), b"\0\x80\x80\xffz".to_vec(), b"\x80\xff\x80\xff".to_vec()]);
        assert_eq!(texts(Point, "label", &source), rows);
    }
}
