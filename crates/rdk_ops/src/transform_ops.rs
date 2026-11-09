//! Transform composition, soft (falloff-weighted) transforms and distance-from-geometry weights.
//!
//! The OCaml entry points wrap their work in `Error.guard`: a failure has the guard's operation and
//! code (`compose_transform` / `soft_transform` / `distance_from_geometry`) and the OCaml message,
//! a cancellation has code `cancelled`.
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::Arc;

use rays_math::{mat4, vec3};
use rdk_core::{
    attribute, cancel, element_selection, error, geometry, group, normal_ops, packed, support,
    topology_index,
};
use rdk_core::error::Code;
use rdk_core::support::Windows;
use rdk_geom::point_grid::{float_max, float_min};
use rdk_geom::{spatial_index, surface_index};

use crate::shortest_paths;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransformOrder {
    TransformSrt,
    TransformStr,
    TransformRst,
    TransformRts,
    TransformTsr,
    TransformTrs,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransformRotationOrder {
    TransformXyz,
    TransformXzy,
    TransformYxz,
    TransformYzx,
    TransformZxy,
    TransformZyx,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SoftTransformMetric {
    SoftRadius,
    SoftEdge,
    SoftAttribute { attribute: String, apply_rolloff: bool },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SoftTransformFalloff {
    SoftLinear,
    SoftQuadratic,
    SoftCubic,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DistanceAlongRadius {
    DistanceFixed(f64),
    DistanceMaximum,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DistanceFromGeometryReference {
    DistanceReferencePoints,
    DistanceReferencePrimitives,
}

fn fail(message: String) -> error::T {
    error::new("Error.Fail", Code::InvalidParameter, message)
}

/// OCaml `%S`: a quoted string with OCaml escapes (non-ASCII bytes as decimal `\ddd`).
pub(crate) use crate::attribute_ops::ocaml_quote;

fn transform_rotation_matrix(order: TransformRotationOrder, angles: vec3::T) -> mat4::T {
    let x = mat4::rotation_x(angles.x);
    let y = mat4::rotation_y(angles.y);
    let z = mat4::rotation_z(angles.z);
    let (first, second, third) = match order {
        TransformRotationOrder::TransformXyz => (x, y, z),
        TransformRotationOrder::TransformXzy => (x, z, y),
        TransformRotationOrder::TransformYxz => (y, x, z),
        TransformRotationOrder::TransformYzx => (y, z, x),
        TransformRotationOrder::TransformZxy => (z, x, y),
        TransformRotationOrder::TransformZyx => (z, y, x),
    };
    mat4::mul(&third, &mat4::mul(&second, &first))
}

#[derive(Clone, Debug)]
pub struct ComposeTransformParams {
    pub order: TransformOrder,
    pub rotation_order: TransformRotationOrder,
    pub translate: vec3::T,
    pub rotate: vec3::T,
    pub scale: vec3::T,
    pub shear: vec3::T,
    pub uniform_scale: f64,
    pub pivot: vec3::T,
    pub pivot_rotation: vec3::T,
    pub invert: bool,
}

fn compose(params: &ComposeTransformParams) -> Result<mat4::T, error::T> {
    let vectors = [
        ("translate", params.translate),
        ("rotate", params.rotate),
        ("scale", params.scale),
        ("shear", params.shear),
        ("pivot", params.pivot),
        ("pivot_rotation", params.pivot_rotation),
    ];
    if let Some((name, _)) = vectors
        .iter()
        .find(|(_, value)| !(value.x.is_finite() && value.y.is_finite() && value.z.is_finite()))
    {
        return Err(fail(format!("Rdk.Transform_ops.compose_transform: non-finite {name}")));
    }
    if !params.uniform_scale.is_finite() {
        return Err(fail("Rdk.Transform_ops.compose_transform: non-finite uniform scale".to_owned()));
    }
    let scale = vec3::scale(params.scale, params.uniform_scale);
    let shear = params.shear;
    let scale_shear = mat4::mul(
        &mat4::of_rows(
            (1.0, shear.x, shear.y, 0.0),
            (0.0, 1.0, shear.z, 0.0),
            (0.0, 0.0, 1.0, 0.0),
            (0.0, 0.0, 0.0, 1.0),
        ),
        &mat4::scaling(scale),
    );
    let rotation = transform_rotation_matrix(params.rotation_order, params.rotate);
    let translation = mat4::translation(params.translate);
    let (first, second, third) = match params.order {
        TransformOrder::TransformSrt => (scale_shear, rotation, translation),
        TransformOrder::TransformStr => (scale_shear, translation, rotation),
        TransformOrder::TransformRst => (rotation, scale_shear, translation),
        TransformOrder::TransformRts => (rotation, translation, scale_shear),
        TransformOrder::TransformTsr => (translation, scale_shear, rotation),
        TransformOrder::TransformTrs => (translation, rotation, scale_shear),
    };
    let core = mat4::mul(&third, &mat4::mul(&second, &first));
    let pivot_rotation = transform_rotation_matrix(params.rotation_order, params.pivot_rotation);
    let pivot_inverse = mat4::transpose(&pivot_rotation);
    let matrix = mat4::mul(
        &mat4::translation(params.pivot),
        &mat4::mul(
            &pivot_rotation,
            &mat4::mul(
                &core,
                &mat4::mul(&pivot_inverse, &mat4::translation(vec3::scale(params.pivot, -1.0))),
            ),
        ),
    );
    if !params.invert {
        return Ok(matrix);
    }
    match mat4::inverse(&matrix) {
        Some(inverse) => Ok(inverse),
        None => Err(fail(
            "Rdk.Transform_ops.compose_transform: cannot invert a singular transform".to_owned(),
        )),
    }
}

pub fn compose_transform(params: &ComposeTransformParams) -> Result<mat4::T, error::T> {
    compose(params).map_err(|failure| error::guard(failure, "compose_transform", Code::InvalidTransform))
}

fn transform_matrix_finite(matrix: &mat4::T) -> bool {
    let (a, b, c, d) = mat4::to_rows(matrix);
    let finite_row = |row: (f64, f64, f64, f64)| {
        row.0.is_finite() && row.1.is_finite() && row.2.is_finite() && row.3.is_finite()
    };
    finite_row(a) && finite_row(b) && finite_row(c) && finite_row(d)
}

fn soft_transform_weight(falloff: SoftTransformFalloff, distance: f64, radius: f64) -> f64 {
    if distance > radius {
        return 0.0;
    }
    let t = float_max(0.0, float_min(1.0, distance / radius));
    match falloff {
        SoftTransformFalloff::SoftLinear => 1.0 - t,
        SoftTransformFalloff::SoftQuadratic => 1.0 - t * t,
        SoftTransformFalloff::SoftCubic => {
            let t2 = t * t;
            1.0 - (3.0 * t2 - 2.0 * t2 * t)
        }
    }
}

/// One value per index in `0..count`, computed in ranges of `grain` (cancellation polled every 4096th
/// index).
fn map_floats(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    count: i64,
    value: &(dyn Fn(i64) -> f64 + Sync),
) -> Result<Vec<f64>, error::T> {
    let mut output = vec![0.0; count.max(0) as usize];
    support::par_fill(cancel_value, grain, &mut output, &|index| Ok(value(index)))?;
    Ok(output)
}

/// Lowers `first_invalid` to `point`, keeping the smallest index seen from any worker.
fn lower(first_invalid: &AtomicI64, point: i64) {
    first_invalid.fetch_min(point, Ordering::Relaxed);
}

struct Positions {
    x: Arc<Vec<f64>>,
    y: Arc<Vec<f64>>,
    z: Arc<Vec<f64>>,
}

fn validate_finite_positions(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    operation: &str,
    value: &geometry::T,
) -> Result<Positions, error::T> {
    let view = packed::float3::view::view(geometry::positions(value))?;
    let count = view.x.len() as i64;
    let first_invalid = AtomicI64::new(i64::MAX);
    if count > 0 {
        support::parallel_for(cancel_value, grain, count, &|point| {
            let p = point as usize;
            if !(view.x[p].is_finite() && view.y[p].is_finite() && view.z[p].is_finite()) {
                lower(&first_invalid, point);
            }
            Ok(())
        })?;
    }
    let invalid = first_invalid.load(Ordering::Relaxed);
    if invalid == i64::MAX {
        Ok(Positions { x: view.x, y: view.y, z: view.z })
    } else {
        Err(fail(format!("{operation}: point {invalid} has a non-finite position")))
    }
}

fn soft_radius_weights(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    radius: f64,
    falloff: SoftTransformFalloff,
    selected: Option<&group::T>,
    value: &geometry::T,
) -> Result<Vec<f64>, error::T> {
    let positions = geometry::positions(value);
    let index = spatial_index::create(cancel_value, Some(grain), selected, positions)
        .map_err(|failure| fail(error::to_string(&failure)))?;
    let count = geometry::point_count(value) as usize;
    let mut indices = vec![-1_i64; count];
    let mut distances = vec![f64::INFINITY; count];
    let mut counts = vec![0_i64; count];
    spatial_index::view::nearest_k_many_into(
        cancel_value,
        None,
        grain,
        &index,
        positions,
        radius * radius,
        1,
        &mut indices,
        &mut distances,
        &mut counts,
    )?;
    map_floats(cancel_value, grain, count as i64, &|point| {
        if counts[point as usize] != 0 {
            soft_transform_weight(falloff, distances[point as usize].sqrt(), radius)
        } else {
            0.0
        }
    })
}

fn soft_edge_distances(
    cancel_value: Option<&cancel::T>,
    radius: f64,
    selected: Option<&group::T>,
    value: &geometry::T,
) -> Result<Vec<f64>, error::T> {
    let count = geometry::point_count(value);
    let source_count = match selected {
        None => count,
        Some(selection) => group::cardinality(selection),
    };
    if source_count == 0 || count == 0 {
        return Ok(vec![f64::INFINITY; count as usize]);
    }
    if source_count == count {
        return Ok(vec![0.0; count as usize]);
    }
    let reverse_index = topology_index::create(cancel_value, geometry::topology(value))?;
    let reverse = topology_index::view::view(&reverse_index);
    let positions = packed::float3::view::view(geometry::positions(value))?;
    let lengths: Vec<f64> = (0..reverse.edge_a.len())
        .map(|edge| {
            let (a, b) = (reverse.edge_a[edge] as usize, reverse.edge_b[edge] as usize);
            let dx = positions.x[b] - positions.x[a];
            let dy = positions.y[b] - positions.y[a];
            let dz = positions.z[b] - positions.z[a];
            (dx * dx + dy * dy + dz * dz).sqrt()
        })
        .collect();
    let mut roots = Vec::with_capacity(source_count as usize);
    for point in 0..count {
        cancel::poll(cancel_value, point, 16_383)?;
        let member = match selected {
            None => true,
            Some(selection) => group::mem(point, selection),
        };
        if member {
            roots.push(point);
        }
    }
    let params = shortest_paths::SearchParams {
        cancel: mirror(cancel_value).map(Arc::new),
        radius,
        point_edge_offsets: Arc::clone(&reverse.point_edge_offsets),
        point_edges: Arc::clone(&reverse.point_edges),
        edge_a: Arc::clone(&reverse.edge_a),
        edge_b: Arc::clone(&reverse.edge_b),
        lengths: Arc::from(lengths),
    };
    Ok(shortest_paths::search(&params, &roots)?.0)
}

/// A token in the same state as `value`, for the APIs that own their token.
fn mirror(value: Option<&cancel::T>) -> Option<cancel::T> {
    value.map(|token| {
        let copy = cancel::create();
        if cancel::is_cancelled(token) {
            cancel::cancel(&copy);
        }
        copy
    })
}

fn point_float_output(
    operation: &str,
    name: &str,
    default: f64,
    value: &geometry::T,
) -> Result<Vec<f64>, error::T> {
    match geometry::find_attribute(attribute::Owner::Point, name, value) {
        None => Ok(vec![default; geometry::point_count(value) as usize]),
        Some(item) => match attribute::view::storage(&item)? {
            attribute::Storage::Float(values) => {
                if values.len() as i64 != geometry::point_count(value) {
                    Err(fail(format!("{operation}: point attribute {} length mismatch", ocaml_quote(name))))
                } else {
                    Ok(values.to_vec())
                }
            }
            _ => Err(fail(format!("{operation}: point attribute {} must be float", ocaml_quote(name)))),
        },
    }
}

fn valid_output_attribute_name(name: Option<&str>) -> bool {
    match name {
        None => true,
        Some(name) => {
            let trimmed = name.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c'));
            !trimmed.is_empty() && trimmed != "P"
        }
    }
}

use rdk_core::support::ocaml_trim_is_empty as blank;

pub struct DistanceFromGeometryParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub affected: Option<element_selection::T>,
    pub reference_selection: Option<element_selection::T>,
    pub reference_kind: DistanceFromGeometryReference,
    pub falloff: SoftTransformFalloff,
    pub radius: DistanceAlongRadius,
    pub distance_attribute: Option<String>,
    pub mask_attribute: Option<String>,
    pub reference: geometry::T,
}

/// Replaces `base[i]` with `value(i)` where it returns `Some`, in ranges joined in order.
fn overwrite_floats(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    base: Vec<f64>,
    value: &(dyn Fn(i64) -> Option<f64> + Sync),
) -> Result<Vec<f64>, error::T> {
    let count = base.len() as i64;
    map_floats(cancel_value, grain, count, &|point| value(point).unwrap_or(base[point as usize]))
}

fn run_distance_from_geometry(
    params: &DistanceFromGeometryParams,
    source: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let operation = "Rdk.Transform_ops.distance_from_geometry";
    let distance_attribute = params.distance_attribute.as_deref();
    let mask_attribute = params.mask_attribute.as_deref();
    support::check_positive(grain).map_err(|_| fail(format!("{operation}: grain must be positive")))?;
    if !valid_output_attribute_name(distance_attribute) {
        return Err(fail(format!("{operation}: distance attribute name must be non-empty and not P")));
    }
    if !valid_output_attribute_name(mask_attribute) {
        return Err(fail(format!("{operation}: mask attribute name must be non-empty and not P")));
    }
    if distance_attribute.is_none() && mask_attribute.is_none() {
        return Err(fail(format!("{operation}: enable at least one distance or mask output")));
    }
    if let (Some(distance), Some(mask)) = (distance_attribute, mask_attribute) {
        if distance == mask {
            return Err(fail(format!("{operation}: distance and mask attributes must have distinct names")));
        }
    }
    if let DistanceAlongRadius::DistanceFixed(value) = params.radius {
        if !value.is_finite() || value <= 0.0 {
            return Err(fail(format!("{operation}: fixed radius must be finite and positive")));
        }
    }
    let source_topology = geometry::topology(source);
    let reference_topology = geometry::topology(&params.reference);
    element_selection::validate(operation, source_topology, params.affected.as_ref())?;
    element_selection::validate(operation, reference_topology, params.reference_selection.as_ref())?;
    validate_finite_positions(cancel_value, grain, operation, source)?;
    let affected_points = match params.affected.as_ref() {
        None => None,
        Some(selection) => Some(element_selection::promote(
            cancel_value,
            grain,
            Some("__distance_from_affected"),
            group::Owner::Point,
            selection,
            source_topology,
        )?),
    };
    let reference_owner = match params.reference_kind {
        DistanceFromGeometryReference::DistanceReferencePoints => group::Owner::Point,
        DistanceFromGeometryReference::DistanceReferencePrimitives => group::Owner::Primitive,
    };
    let reference_group = match params.reference_selection.as_ref() {
        None => None,
        Some(selection) => Some(element_selection::promote(
            cancel_value,
            grain,
            Some("__distance_from_reference"),
            reference_owner,
            selection,
            reference_topology,
        )?),
    };
    let point_count = geometry::point_count(source);
    let mut distances_squared = vec![f64::INFINITY; point_count as usize];
    let maximum_squared = match (params.radius, distance_attribute) {
        (DistanceAlongRadius::DistanceMaximum, _) | (DistanceAlongRadius::DistanceFixed(_), Some(_)) => {
            f64::INFINITY
        }
        (DistanceAlongRadius::DistanceFixed(value), None) => value * value,
    };
    match params.reference_kind {
        DistanceFromGeometryReference::DistanceReferencePoints => {
            let index = spatial_index::create(
                cancel_value,
                Some(grain),
                reference_group.as_ref(),
                geometry::positions(&params.reference),
            )
            .map_err(|failure| fail(error::to_string(&failure)))?;
            spatial_index::view::nearest_distances_many_into(
                cancel_value,
                affected_points.as_ref(),
                grain,
                &index,
                geometry::positions(source),
                maximum_squared,
                &mut distances_squared,
            )?;
        }
        DistanceFromGeometryReference::DistanceReferencePrimitives => {
            let index = surface_index::create(
                cancel_value,
                Some(grain),
                reference_group.as_ref(),
                None,
                None,
                &params.reference,
            )
            .map_err(|failure| fail(error::to_string(&failure)))?;
            surface_index::view::closest_distances_many_into(
                cancel_value,
                affected_points.as_ref(),
                grain,
                &index,
                geometry::positions(source),
                maximum_squared,
                &mut distances_squared,
            )?;
        }
    }
    let affected_point = |point: i64| match affected_points.as_ref() {
        None => true,
        Some(selection) => group::mem(point, selection),
    };
    let maximum = match params.radius {
        DistanceAlongRadius::DistanceFixed(value) => value,
        DistanceAlongRadius::DistanceMaximum => {
            let mut maximum = 0.0_f64;
            for point in 0..point_count {
                cancel::poll(cancel_value, point, 16_383)?;
                let squared = distances_squared[point as usize];
                if affected_point(point) && squared.is_finite() {
                    let distance = squared.sqrt();
                    if distance > maximum {
                        maximum = distance;
                    }
                }
            }
            maximum
        }
    };
    let distance_values = match distance_attribute {
        None => None,
        Some(name) => Some(point_float_output(operation, name, -1.0, source)?),
    };
    let mask_values = match mask_attribute {
        None => None,
        Some(name) => Some(point_float_output(operation, name, 0.0, source)?),
    };
    let distance_of = |point: i64| -> f64 {
        let squared = distances_squared[point as usize];
        if squared.is_finite() { squared.sqrt() } else { f64::INFINITY }
    };
    let distance_values = match distance_values {
        None => None,
        Some(base) => Some(overwrite_floats(cancel_value, grain, base, &|point| {
            if affected_point(point) {
                let distance = distance_of(point);
                Some(if distance.is_finite() { distance } else { -1.0 })
            } else {
                None
            }
        })?),
    };
    let falloff = params.falloff;
    let mask_values = match mask_values {
        None => None,
        Some(base) => Some(overwrite_floats(cancel_value, grain, base, &|point| {
            if affected_point(point) {
                let distance = distance_of(point);
                Some(if !distance.is_finite() {
                    0.0
                } else if maximum == 0.0 {
                    if distance == 0.0 { 1.0 } else { 0.0 }
                } else {
                    soft_transform_weight(falloff, distance, maximum)
                })
            } else {
                None
            }
        })?),
    };
    let mut attributes = Vec::new();
    if let (Some(name), Some(values)) = (distance_attribute, distance_values) {
        attributes.push(attribute::create_owned(
            name,
            attribute::Owner::Point,
            attribute::Storage::Float(Arc::new(values)),
        )?);
    }
    if let (Some(name), Some(values)) = (mask_attribute, mask_values) {
        attributes.push(attribute::create_owned(
            name,
            attribute::Owner::Point,
            attribute::Storage::Float(Arc::new(values)),
        )?);
    }
    geometry::view::with_merged_attributes_owned(attributes, source)
}

pub fn distance_from_geometry(
    params: &DistanceFromGeometryParams,
    source: &geometry::T,
) -> Result<geometry::T, error::T> {
    run_distance_from_geometry(params, source)
        .map_err(|failure| error::guard(failure, "distance_from_geometry", Code::InvalidDistance))
}

fn soft_attribute_weights(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    radius: f64,
    falloff: SoftTransformFalloff,
    apply_rolloff: bool,
    selected: Option<&group::T>,
    name: &str,
    value: &geometry::T,
) -> Result<Vec<f64>, error::T> {
    if blank(name) {
        return Err(fail("Rdk.Transform_ops.soft_transform: empty distance attribute name".to_owned()));
    }
    let Some(item) = geometry::find_attribute(attribute::Owner::Point, name, value) else {
        return Err(fail(format!(
            "Rdk.Transform_ops.soft_transform: missing point distance attribute {}",
            ocaml_quote(name)
        )));
    };
    let attribute::Storage::Float(values) = attribute::view::storage(&item)? else {
        return Err(fail(format!(
            "Rdk.Transform_ops.soft_transform: point distance attribute {} must be float",
            ocaml_quote(name)
        )));
    };
    let count = values.len() as i64;
    if count != geometry::point_count(value) {
        return Err(fail("Rdk.Transform_ops.soft_transform: distance attribute length mismatch".to_owned()));
    }
    let first_invalid = AtomicI64::new(i64::MAX);
    let weights = map_floats(cancel_value, grain, count, &|point| {
        let current = values[point as usize];
        if !current.is_finite() {
            lower(&first_invalid, point);
            0.0
        } else if match selected {
            None => true,
            Some(selection) => group::mem(point, selection),
        } {
            if apply_rolloff { soft_transform_weight(falloff, current, radius) } else { current }
        } else {
            0.0
        }
    })?;
    let invalid = first_invalid.load(Ordering::Relaxed);
    if invalid == i64::MAX {
        Ok(weights)
    } else {
        Err(fail(format!(
            "Rdk.Transform_ops.soft_transform: distance attribute {} element {invalid} is non-finite",
            ocaml_quote(name)
        )))
    }
}

pub struct SoftTransformParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selection: Option<element_selection::T>,
    pub metric: SoftTransformMetric,
    pub falloff: SoftTransformFalloff,
    pub radius: f64,
    pub falloff_attribute: Option<String>,
    pub recompute_normals: bool,
}

fn run_soft_transform(
    params: &SoftTransformParams,
    matrix: &mat4::T,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let operation = "Rdk.Transform_ops.soft_transform";
    let (radius, falloff) = (params.radius, params.falloff);
    support::check_positive(grain).map_err(|_| fail(format!("{operation}: grain must be positive")))?;
    if !radius.is_finite() || radius < 0.0 {
        return Err(fail(format!("{operation}: radius must be finite and non-negative")));
    }
    if !transform_matrix_finite(matrix) {
        return Err(fail(format!("{operation}: matrix must be finite")));
    }
    if let Some(name) = params.falloff_attribute.as_deref() {
        if blank(name) {
            return Err(fail(format!("{operation}: empty falloff attribute name")));
        }
    }
    let needs_radius = match &params.metric {
        SoftTransformMetric::SoftRadius | SoftTransformMetric::SoftEdge => radius <= 0.0,
        SoftTransformMetric::SoftAttribute { apply_rolloff: true, .. } => radius <= 0.0,
        SoftTransformMetric::SoftAttribute { apply_rolloff: false, .. } => false,
    };
    if needs_radius {
        return Err(fail(format!("{operation}: rolloff radius must be positive")));
    }
    element_selection::validate(operation, geometry::topology(value), params.selection.as_ref())?;
    let positions = validate_finite_positions(cancel_value, grain, operation, value)?;
    let selected = match params.selection.as_ref() {
        None => None,
        Some(selection) => Some(element_selection::promote(
            cancel_value,
            grain,
            None,
            group::Owner::Point,
            selection,
            geometry::topology(value),
        )?),
    };
    let all_selected = match selected.as_ref() {
        None => true,
        Some(selection) => group::cardinality(selection) == geometry::point_count(value),
    };
    let weights = match &params.metric {
        SoftTransformMetric::SoftRadius | SoftTransformMetric::SoftEdge if all_selected => {
            vec![1.0; geometry::point_count(value) as usize]
        }
        SoftTransformMetric::SoftRadius => {
            soft_radius_weights(cancel_value, grain, radius, falloff, selected.as_ref(), value)?
        }
        SoftTransformMetric::SoftEdge => {
            let distances = soft_edge_distances(cancel_value, radius, selected.as_ref(), value)?;
            map_floats(cancel_value, grain, distances.len() as i64, &|point| {
                soft_transform_weight(falloff, distances[point as usize], radius)
            })?
        }
        SoftTransformMetric::SoftAttribute { attribute, apply_rolloff } => soft_attribute_weights(
            cancel_value,
            grain,
            radius,
            falloff,
            *apply_rolloff,
            selected.as_ref(),
            attribute,
            value,
        )?,
    };
    let count = weights.len() as i64;
    let changed = AtomicBool::new(false);
    let moves_positions = !mat4::nearly_equal(matrix, &mat4::identity(), 0.0);
    let ((m00, m01, m02, m03), (m10, m11, m12, m13), (m20, m21, m22, m23), (m30, m31, m32, m33)) =
        mat4::to_rows(matrix);
    let (mut x, mut y, mut z) = (vec![0.0; count as usize], vec![0.0; count as usize], vec![0.0; count as usize]);
    {
        let (wx, wy, wz) = (Windows::even(&mut x, grain), Windows::even(&mut y, grain), Windows::even(&mut z, grain));
        support::run_indexed_ranges(grain, count, &|range, first, last| {
            let (moved_x, moved_y, moved_z) = (wx.take(range), wy.take(range), wz.take(range));
            for point in first..last {
                support::poll(cancel_value, point)?;
                let (p, at) = (point as usize, (point - first) as usize);
                let weight = weights[p];
                let (vx, vy, vz) = (positions.x[p], positions.y[p], positions.z[p]);
                if moves_positions && weight != 0.0 {
                    changed.store(true, Ordering::Relaxed);
                    let ox = m00 * vx + m01 * vy + m02 * vz + m03;
                    let oy = m10 * vx + m11 * vy + m12 * vz + m13;
                    let oz = m20 * vx + m21 * vy + m22 * vz + m23;
                    let ow = m30 * vx + m31 * vy + m32 * vz + m33;
                    let (tx, ty, tz) = if ow.abs() <= 1e-12 { (ox, oy, oz) } else { (ox / ow, oy / ow, oz / ow) };
                    moved_x[at] = vx + weight * (tx - vx);
                    moved_y[at] = vy + weight * (ty - vy);
                    moved_z[at] = vz + weight * (tz - vz);
                } else {
                    moved_x[at] = vx;
                    moved_y[at] = vy;
                    moved_z[at] = vz;
                }
            }
            Ok(())
        })?;
    }
    let did_change = changed.load(Ordering::Relaxed);
    let mut output = if did_change {
        geometry::with_positions(packed::float3::view::of_owned_exn(x, y, z)?, value)?
    } else {
        value.clone()
    };
    if did_change && !params.recompute_normals {
        output = geometry::without_attribute(attribute::Owner::Point, "N", &output);
        output = geometry::without_attribute(attribute::Owner::Vertex, "N", &output);
    }
    if did_change && params.recompute_normals {
        for owner in [attribute::Owner::Point, attribute::Owner::Vertex] {
            if geometry::find_attribute(owner, "N", value).is_some() {
                output = normal_ops::run(
                    &normal_ops::Params {
                        cancel: mirror(cancel_value),
                        grain: Some(grain),
                        selection: None,
                        primitives: None,
                        owner,
                        weighting: normal_ops::Weighting::FaceArea,
                        cusp_angle: std::f64::consts::PI,
                        attribute: "N".to_owned(),
                    },
                    &output,
                )?;
            }
        }
    }
    match params.falloff_attribute.as_deref() {
        None => Ok(output),
        Some(name) => {
            let item = attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::Float(Arc::new(weights)))?;
            geometry::with_attribute(item, &output)
        }
    }
}

pub fn soft_transform(
    params: &SoftTransformParams,
    matrix: &mat4::T,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    run_soft_transform(params, matrix, value)
        .map_err(|failure| error::guard(failure, "soft_transform", Code::InvalidTransform))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology;
    use std::sync::Mutex;

    fn close(left: f64, right: f64) -> bool {
        (left - right).abs() <= 1e-10
    }

    fn v3(x: f64, y: f64, z: f64) -> vec3::T {
        vec3::create(x, y, z)
    }

    fn compose_params() -> ComposeTransformParams {
        ComposeTransformParams {
            order: TransformOrder::TransformSrt,
            rotation_order: TransformRotationOrder::TransformXyz,
            translate: vec3::zero(),
            rotate: vec3::zero(),
            scale: v3(1.0, 1.0, 1.0),
            shear: vec3::zero(),
            uniform_scale: 1.0,
            pivot: vec3::zero(),
            pivot_rotation: vec3::zero(),
            invert: false,
        }
    }

    /// Applies matrices in order: `[a, b, c]` is `c * b * a`.
    fn applied(matrices: &[&mat4::T]) -> mat4::T {
        let mut matrix = mat4::identity();
        for operation in matrices {
            matrix = mat4::mul(operation, &matrix);
        }
        matrix
    }

    #[test]
    fn composition_orders_pivot_inverse_and_errors() {
        let translation = v3(10.0, 0.0, 0.0);
        let rotation = v3(0.0, 0.0, std::f64::consts::PI / 2.0);
        let scale = v3(2.0, 1.0, 1.0);
        let base = ComposeTransformParams { translate: translation, rotate: rotation, scale, ..compose_params() };
        let srt = compose_transform(&base).unwrap();
        let p = mat4::transform_point(&srt, v3(1.0, 0.0, 0.0));
        assert!(close(p.x, 10.0) && close(p.y, 2.0) && close(p.z, 0.0), "SRT application order");
        let trs = compose_transform(&ComposeTransformParams { order: TransformOrder::TransformTrs, ..base.clone() }).unwrap();
        let p = mat4::transform_point(&trs, v3(1.0, 0.0, 0.0));
        assert!(close(p.x, 0.0) && close(p.y, 11.0) && close(p.z, 0.0), "TRS application order");
        let s = mat4::scaling(scale);
        let r = mat4::mul(
            &mat4::rotation_z(rotation.z),
            &mat4::mul(&mat4::rotation_y(rotation.y), &mat4::rotation_x(rotation.x)),
        );
        let t = mat4::translation(translation);
        let orders = [
            (TransformOrder::TransformSrt, [&s, &r, &t]),
            (TransformOrder::TransformStr, [&s, &t, &r]),
            (TransformOrder::TransformRst, [&r, &s, &t]),
            (TransformOrder::TransformRts, [&r, &t, &s]),
            (TransformOrder::TransformTsr, [&t, &s, &r]),
            (TransformOrder::TransformTrs, [&t, &r, &s]),
        ];
        for (order, expected) in orders {
            let actual = compose_transform(&ComposeTransformParams { order, ..base.clone() }).unwrap();
            assert!(mat4::nearly_equal(&actual, &applied(&expected), 1e-12), "affine order matrix");
        }
        let (rx, ry, rz) = (mat4::rotation_x(0.2), mat4::rotation_y(-0.3), mat4::rotation_z(0.4));
        let angles = v3(0.2, -0.3, 0.4);
        let eulers = [
            (TransformRotationOrder::TransformXyz, [&rx, &ry, &rz]),
            (TransformRotationOrder::TransformXzy, [&rx, &rz, &ry]),
            (TransformRotationOrder::TransformYxz, [&ry, &rx, &rz]),
            (TransformRotationOrder::TransformYzx, [&ry, &rz, &rx]),
            (TransformRotationOrder::TransformZxy, [&rz, &rx, &ry]),
            (TransformRotationOrder::TransformZyx, [&rz, &ry, &rx]),
        ];
        for (rotation_order, expected) in eulers {
            let actual =
                compose_transform(&ComposeTransformParams { rotation_order, rotate: angles, ..compose_params() }).unwrap();
            assert!(mat4::nearly_equal(&actual, &applied(&expected), 1e-12), "Euler order matrix");
        }
        let pivoted = compose_transform(&ComposeTransformParams {
            scale: v3(2.0, 2.0, 2.0),
            pivot: v3(1.0, 0.0, 0.0),
            ..compose_params()
        })
        .unwrap();
        let p = mat4::transform_point(&pivoted, v3(2.0, 0.0, 0.0));
        assert!(close(p.x, 3.0) && close(p.y, 0.0) && close(p.z, 0.0), "pivot");
        let general = ComposeTransformParams {
            translate: v3(3.0, -2.0, 5.0),
            rotate: v3(0.2, -0.4, 0.7),
            scale: v3(2.0, 3.0, 4.0),
            shear: v3(0.1, -0.2, 0.3),
            ..compose_params()
        };
        let forward = compose_transform(&general).unwrap();
        let inverse = compose_transform(&ComposeTransformParams { invert: true, ..general }).unwrap();
        let source = v3(-1.2, 4.1, 0.7);
        let back = mat4::transform_point(&inverse, mat4::transform_point(&forward, source));
        assert!(close(source.x, back.x) && close(source.y, back.y) && close(source.z, back.z));
        let singular = compose_transform(&ComposeTransformParams { scale: vec3::zero(), invert: true, ..compose_params() });
        assert_eq!(error::code(&singular.unwrap_err()), Code::InvalidTransform);
        let bad = compose_transform(&ComposeTransformParams { shear: v3(f64::NAN, 0.0, 0.0), ..compose_params() })
            .unwrap_err();
        assert_eq!(error::message(&bad), "Rdk.Transform_ops.compose_transform: non-finite shear");
        assert_eq!(error::operation(&bad), "compose_transform");
    }

    fn points_geometry(points: &[(f64, f64, f64)]) -> geometry::T {
        let positions = packed::float3::of_owned(
            points.iter().map(|p| p.0).collect(),
            points.iter().map(|p| p.1).collect(),
            points.iter().map(|p| p.2).collect(),
        )
        .unwrap();
        geometry::create(positions, topology::empty(points.len() as i64).unwrap(), None, None, None).unwrap()
    }

    fn line_with_free_point() -> geometry::T {
        let positions = packed::float3::of_owned(
            vec![0.0, 1.0, 0.1, 3.0, 4.0],
            vec![0.0; 5],
            vec![0.0; 5],
        )
        .unwrap();
        let mut builder = topology::builder::create(5, Some(4), Some(1)).unwrap();
        topology::builder::add_open_polyline(&mut builder, &[0, 1, 3, 4]).unwrap();
        let topo = topology::builder::freeze(&mut builder).unwrap();
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    /// A flat triangulated grid in the XZ plane with point normals, `columns x rows` cells.
    fn grid(columns: i64, rows: i64, size: f64) -> geometry::T {
        let (mut x, mut z, mut vertices, mut offsets) = (Vec::new(), Vec::new(), Vec::new(), vec![0_i64]);
        for row in 0..=rows {
            for column in 0..=columns {
                x.push(-size / 2.0 + size * column as f64 / columns as f64);
                z.push(-size / 2.0 + size * row as f64 / rows as f64);
            }
        }
        let width = columns + 1;
        for row in 0..rows {
            for column in 0..columns {
                let a = row * width + column;
                let (b, c, d) = (a + 1, a + width + 1, a + width);
                vertices.extend([a, b, c, a, c, d]);
                offsets.push(vertices.len() as i64 - 3);
                offsets.push(vertices.len() as i64);
            }
        }
        let count = x.len();
        let positions = packed::float3::of_owned(x, vec![0.0; count], z).unwrap();
        let topo = topology::polygons_owned(count as i64, vertices, offsets).unwrap();
        let normals = packed::float3::of_owned(vec![0.0; count], vec![1.0; count], vec![0.0; count]).unwrap();
        let normal = attribute::create_key_owned(
            &attribute::normal(attribute::Owner::Point),
            attribute::Storage::Float3(normals),
        )
        .unwrap();
        geometry::create(positions, topo, Some(vec![normal]), None, None).unwrap()
    }

    fn point_group(name: &str, length: i64, predicate: &(dyn Fn(i64) -> bool + Send + Sync)) -> group::T {
        group::init(Some(1), group::Owner::Point, name, length, &|index| Ok(predicate(index))).unwrap()
    }

    fn ys(value: &geometry::T) -> Vec<f64> {
        packed::float3::view::view(geometry::positions(value)).unwrap().y.to_vec()
    }

    fn float_attribute(name: &str, value: &geometry::T) -> Vec<f64> {
        let item = geometry::find_attribute(attribute::Owner::Point, name, value).expect("attribute");
        match attribute::view::storage(&item).unwrap() {
            attribute::Storage::Float(values) => values.to_vec(),
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn with_float(name: &str, values: Vec<f64>, value: &geometry::T) -> geometry::T {
        let item = attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::Float(Arc::new(values))).unwrap();
        geometry::with_attribute(item, value).unwrap()
    }

    fn soft(
        metric: SoftTransformMetric,
        falloff: SoftTransformFalloff,
        radius: f64,
        selection: Option<group::T>,
        falloff_attribute: Option<&str>,
    ) -> SoftTransformParams {
        SoftTransformParams {
            cancel: None,
            grain: Some(1),
            selection: selection.map(element_selection::T::SelectedPoints),
            metric,
            falloff,
            radius,
            falloff_attribute: falloff_attribute.map(str::to_owned),
            recompute_normals: true,
        }
    }

    fn up(distance: f64) -> mat4::T {
        mat4::translation(v3(0.0, distance, 0.0))
    }

    fn failure_code(result: Result<geometry::T, error::T>) -> String {
        match result {
            Ok(_) => "ok".to_owned(),
            Err(failure) => error::code(&failure).as_str().to_owned(),
        }
    }

    #[test]
    fn soft_transform_radius_edge_and_falloffs() {
        let source = line_with_free_point();
        let seed = || Some(point_group("seed", 5, &|point| point == 0));
        use SoftTransformFalloff::*;
        use SoftTransformMetric::*;
        let radius = soft_transform(&soft(SoftRadius, SoftLinear, 2.0, seed(), Some("falloff")), &up(2.0), &source).unwrap();
        let (y, weights) = (ys(&radius), float_attribute("falloff", &radius));
        assert!(close(y[0], 2.0) && close(y[1], 1.0) && close(y[2], 1.9) && close(y[3], 0.0) && close(y[4], 0.0));
        assert!(close(weights[0], 1.0) && close(weights[1], 0.5) && close(weights[2], 0.95) && close(weights[3], 0.0));
        let edge = soft_transform(&soft(SoftEdge, SoftLinear, 3.5, seed(), Some("falloff")), &up(2.0), &source).unwrap();
        let (y, weights) = (ys(&edge), float_attribute("falloff", &edge));
        assert!(close(y[0], 2.0));
        assert!(close(weights[1], 1.0 - 1.0 / 3.5) && close(y[2], 0.0) && close(weights[2], 0.0));
        assert!(close(weights[3], 1.0 - 3.0 / 3.5) && close(weights[4], 0.0));
        let quadratic = soft_transform(&soft(SoftRadius, SoftQuadratic, 2.0, seed(), None), &up(2.0), &source).unwrap();
        assert!(close(ys(&quadratic)[1], 1.5));
        let cubic = soft_transform(&soft(SoftRadius, SoftCubic, 2.0, seed(), None), &up(2.0), &source).unwrap();
        assert!(close(ys(&cubic)[1], 1.0));
    }

    #[test]
    fn soft_transform_attribute_weights_normals_and_sharing() {
        let source = with_float(
            "distance",
            vec![0.0, 1.0, 2.0, 3.0, 4.0],
            &with_float("weight", vec![1.0, 0.5, 2.0, -0.5, 0.0], &line_with_free_point()),
        );
        use SoftTransformFalloff::*;
        use SoftTransformMetric::*;
        let affected = Some(point_group("affected", 5, &|point| point == 1 || point == 2));
        let direct = soft_transform(
            &soft(SoftAttribute { attribute: "weight".to_owned(), apply_rolloff: false }, SoftCubic, 1.0, affected, Some("used_weight")),
            &up(2.0),
            &source,
        )
        .unwrap();
        let (y, weights) = (ys(&direct), float_attribute("used_weight", &direct));
        assert!(close(y[0], 0.0) && close(y[1], 1.0) && close(y[2], 4.0) && close(y[3], 0.0));
        assert!(close(weights[0], 0.0) && close(weights[2], 2.0));
        let rolled = soft_transform(
            &soft(SoftAttribute { attribute: "distance".to_owned(), apply_rolloff: true }, SoftLinear, 4.0, None, None),
            &up(2.0),
            &source,
        )
        .unwrap();
        let y = ys(&rolled);
        assert!(close(y[0], 2.0) && close(y[1], 1.5) && close(y[2], 1.0) && close(y[3], 0.5) && close(y[4], 0.0));
        let plane = grid(2, 2, 2.0);
        let seed = || Some(point_group("seed", geometry::point_count(&plane), &|point| point == 4));
        let stale = SoftTransformParams { recompute_normals: false, ..soft(SoftRadius, SoftCubic, 2.0, seed(), None) };
        let deformed = soft_transform(&stale, &up(2.0), &plane).unwrap();
        assert!(geometry::find_attribute(attribute::Owner::Point, "N", &deformed).is_none());
        let repaired = soft_transform(&soft(SoftRadius, SoftCubic, 2.0, seed(), None), &up(2.0), &plane).unwrap();
        assert!(geometry::find_attribute(attribute::Owner::Point, "N", &repaired).is_some());
        let empty = Some(point_group("empty", 5, &|_| false));
        let unchanged = soft_transform(&soft(SoftRadius, SoftCubic, 2.0, empty, None), &up(2.0), &source).unwrap();
        assert_eq!(geometry::data_id(&unchanged), geometry::data_id(&source), "empty influence shares the source");
    }

    #[test]
    fn soft_transform_errors_cancellation_and_domains() {
        let source = line_with_free_point();
        use SoftTransformFalloff::*;
        use SoftTransformMetric::*;
        let seed = || Some(point_group("seed", 5, &|point| point == 0));
        let zero = soft_transform(&soft(SoftRadius, SoftCubic, 0.0, seed(), None), &mat4::identity(), &source);
        assert_eq!(failure_code(zero), "invalid_transform");
        let attribute_metric = |name: &str| SoftAttribute { attribute: name.to_owned(), apply_rolloff: false };
        let missing = soft_transform(&soft(attribute_metric("missing"), SoftCubic, 1.0, None, None), &mat4::identity(), &source);
        assert_eq!(failure_code(missing), "invalid_transform");
        let wrong = attribute::create_owned("wrong", attribute::Owner::Point, attribute::Storage::Int(Arc::new(vec![1; 5]))).unwrap();
        let wrong = geometry::with_attribute(wrong, &source).unwrap();
        let params = soft(attribute_metric("wrong"), SoftCubic, 1.0, None, None);
        assert_eq!(failure_code(soft_transform(&params, &mat4::identity(), &wrong)), "invalid_transform");
        let nonfinite = with_float("bad", vec![0.0, 1.0, f64::NAN, 3.0, 4.0], &source);
        let params = soft(attribute_metric("bad"), SoftCubic, 1.0, None, None);
        let Err(failure) = soft_transform(&params, &mat4::identity(), &nonfinite) else { panic!("expected failure") };
        assert_eq!(error::message(&failure), "Rdk.Transform_ops.soft_transform: distance attribute \"bad\" element 2 is non-finite");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let params = SoftTransformParams { cancel: Some(token), ..soft(SoftRadius, SoftCubic, 1.0, seed(), None) };
        assert_eq!(failure_code(soft_transform(&params, &up(1.0), &source)), "cancelled");
        // One and four domains agree exactly on a grid large enough to split into many ranges.
        let dense = grid(400, 240, 20.0);
        let width = 401;
        let count = geometry::point_count(&dense);
        let center = 120 * width + 200;
        let group_seed = point_group("seed", count, &move |point| point == center);
        for metric in [SoftRadius, SoftEdge] {
            let run = |domains: i64| -> geometry::T {
                let output = Mutex::new(None);
                parallel::run_unit(Some(domains), &|| {
                    let params = SoftTransformParams {
                        grain: Some(257),
                        ..soft(metric.clone(), SoftCubic, 4.0, Some(group_seed.clone()), Some("falloff"))
                    };
                    let result = soft_transform(&params, &up(1.0), &dense).map_err(|f| error::to_string(&f))?;
                    *output.lock().unwrap() = Some(result);
                    Ok(())
                })
                .unwrap();
                output.into_inner().unwrap().unwrap()
            };
            let (one, four) = (run(1), run(4));
            let one_positions = packed::float3::view::view(geometry::positions(&one)).unwrap();
            let four_positions = packed::float3::view::view(geometry::positions(&four)).unwrap();
            assert_eq!(one_positions.x, four_positions.x);
            assert_eq!(one_positions.y, four_positions.y);
            assert_eq!(one_positions.z, four_positions.z);
            assert_eq!(float_attribute("falloff", &one), float_attribute("falloff", &four));
        }
    }

    #[test]
    fn distance_from_geometry_points_surfaces_and_selections() {
        let source = points_geometry(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (3.0, 0.0, 0.0)]);
        let reference = points_geometry(&[(0.0, 1.0, 0.0)]);
        let base = |reference: &geometry::T, kind| DistanceFromGeometryParams {
            cancel: None,
            grain: Some(1),
            affected: None,
            reference_selection: None,
            reference_kind: kind,
            falloff: SoftTransformFalloff::SoftLinear,
            radius: DistanceAlongRadius::DistanceMaximum,
            distance_attribute: Some("distance".to_owned()),
            mask_attribute: None,
            reference: reference.clone(),
        };
        use DistanceFromGeometryReference::*;
        let fixed = distance_from_geometry(
            &DistanceFromGeometryParams {
                radius: DistanceAlongRadius::DistanceFixed(2.0),
                mask_attribute: Some("mask".to_owned()),
                ..base(&reference, DistanceReferencePoints)
            },
            &source,
        )
        .unwrap();
        let (distance, mask) = (float_attribute("distance", &fixed), float_attribute("mask", &fixed));
        assert!(close(distance[0], 1.0) && close(distance[1], 2.0_f64.sqrt()) && close(distance[2], 10.0_f64.sqrt()));
        assert!(close(mask[0], 0.5) && close(mask[1], 1.0 - 2.0_f64.sqrt() / 2.0) && close(mask[2], 0.0));
        let maximum = distance_from_geometry(
            &DistanceFromGeometryParams {
                falloff: SoftTransformFalloff::SoftQuadratic,
                mask_attribute: Some("mask".to_owned()),
                ..base(&reference, DistanceReferencePoints)
            },
            &source,
        )
        .unwrap();
        let mask = float_attribute("mask", &maximum);
        assert!(close(mask[0], 0.9) && close(mask[1], 0.8) && close(mask[2], 0.0));
        let mask_only = distance_from_geometry(
            &DistanceFromGeometryParams {
                radius: DistanceAlongRadius::DistanceFixed(2.0),
                distance_attribute: None,
                mask_attribute: Some("mask".to_owned()),
                ..base(&reference, DistanceReferencePoints)
            },
            &source,
        )
        .unwrap();
        assert!(geometry::find_attribute(attribute::Owner::Point, "distance", &mask_only).is_none());
        assert_eq!(float_attribute("mask", &fixed), float_attribute("mask", &mask_only));

        let surface_source = with_float(
            "mask",
            vec![80.0, 81.0, 82.0],
            &with_float(
                "distance",
                vec![90.0, 91.0, 92.0],
                &points_geometry(&[(0.0, 1.0, 0.0), (1.0, 2.0, 1.0), (-1.0, 3.0, -1.0)]),
            ),
        );
        let plane = grid(2, 2, 10.0);
        let affected = point_group("affected", 3, &|point| point != 1);
        let output = distance_from_geometry(
            &DistanceFromGeometryParams {
                affected: Some(element_selection::T::SelectedPoints(affected)),
                mask_attribute: Some("mask".to_owned()),
                ..base(&plane, DistanceReferencePrimitives)
            },
            &surface_source,
        )
        .unwrap();
        assert_eq!(float_attribute("distance", &output), vec![1.0, 91.0, 3.0]);
        let mask = float_attribute("mask", &output);
        assert!(close(mask[0], 2.0 / 3.0) && mask[1] == 81.0 && close(mask[2], 0.0));
        let all = group::init(Some(1), group::Owner::Primitive, "surface", geometry::primitive_count(&plane), &|_| Ok(true)).unwrap();
        let selected = distance_from_geometry(
            &DistanceFromGeometryParams {
                reference_selection: Some(element_selection::T::SelectedPrimitives(all)),
                ..base(&plane, DistanceReferencePrimitives)
            },
            &surface_source,
        )
        .unwrap();
        assert_eq!(float_attribute("distance", &selected), vec![1.0, 2.0, 3.0]);
        let none = point_group("empty", geometry::point_count(&plane), &|_| false);
        let missed = distance_from_geometry(
            &DistanceFromGeometryParams {
                reference_selection: Some(element_selection::T::SelectedPoints(none)),
                mask_attribute: Some("mask".to_owned()),
                ..base(&plane, DistanceReferencePoints)
            },
            &surface_source,
        )
        .unwrap();
        assert_eq!(float_attribute("distance", &missed), vec![-1.0, -1.0, -1.0]);
        assert_eq!(float_attribute("mask", &missed), vec![0.0, 0.0, 0.0]);
    }

    #[test]
    fn distance_from_geometry_rejects_bad_requests_and_honours_cancellation() {
        let source = points_geometry(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]);
        let reference = points_geometry(&[(0.0, 1.0, 0.0)]);
        let params = |reference: &geometry::T| DistanceFromGeometryParams {
            cancel: None,
            grain: None,
            affected: None,
            reference_selection: None,
            reference_kind: DistanceFromGeometryReference::DistanceReferencePrimitives,
            falloff: SoftTransformFalloff::SoftLinear,
            radius: DistanceAlongRadius::DistanceMaximum,
            distance_attribute: Some("distance".to_owned()),
            mask_attribute: None,
            reference: reference.clone(),
        };
        let invalid = |p: DistanceFromGeometryParams, source: &geometry::T| {
            assert_eq!(failure_code(distance_from_geometry(&p, source)), "invalid_distance");
        };
        invalid(DistanceFromGeometryParams { distance_attribute: None, ..params(&reference) }, &source);
        invalid(DistanceFromGeometryParams { distance_attribute: Some(" P ".to_owned()), ..params(&reference) }, &source);
        invalid(
            DistanceFromGeometryParams {
                distance_attribute: Some("same".to_owned()),
                mask_attribute: Some("same".to_owned()),
                ..params(&reference)
            },
            &source,
        );
        invalid(DistanceFromGeometryParams { radius: DistanceAlongRadius::DistanceFixed(0.0), ..params(&reference) }, &source);
        let wrong = attribute::create_owned("distance", attribute::Owner::Point, attribute::Storage::Int(Arc::new(vec![1, 2]))).unwrap();
        invalid(params(&reference), &geometry::with_attribute(wrong, &source).unwrap());
        let malformed = point_group("malformed", 3, &|point| point == 0);
        invalid(
            DistanceFromGeometryParams { affected: Some(element_selection::T::SelectedPoints(malformed)), ..params(&reference) },
            &source,
        );
        invalid(params(&reference), &points_geometry(&[(f64::NAN, 0.0, 0.0)]));
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let stopped = DistanceFromGeometryParams {
            cancel: Some(token),
            reference_kind: DistanceFromGeometryReference::DistanceReferencePoints,
            ..params(&reference)
        };
        assert_eq!(failure_code(distance_from_geometry(&stopped, &source)), "cancelled");
    }

    /// Remaining checks of `test_errors_cancellation_and_parallel` in lib/rdk/test_distance_from_geometry.ml
    /// (the curve as surface reference, and full grid/sphere one/four-domain exactness).
    #[test]
    fn distance_from_geometry_curve_reference_and_domain_exactness() {
        let source = points_geometry(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]);
        let curve = crate::line_geometry::polyline(
            &crate::line_geometry::Params { closed: false },
            &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)],
        )
        .unwrap();
        let params = |reference: &geometry::T, kind, cancel| DistanceFromGeometryParams {
            cancel,
            grain: Some(257),
            affected: None,
            reference_selection: None,
            reference_kind: kind,
            falloff: SoftTransformFalloff::SoftCubic,
            radius: DistanceAlongRadius::DistanceFixed(4.0),
            distance_attribute: Some("distance".to_owned()),
            mask_attribute: Some("mask".to_owned()),
            reference: reference.clone(),
        };
        use DistanceFromGeometryReference::*;
        assert_eq!(
            failure_code(distance_from_geometry(&params(&curve, DistanceReferencePrimitives, None), &source)),
            "invalid_distance",
            "curve surface reference"
        );
        let mut row = Vec::new();
        for j in 0..=200 {
            for i in 0..=320 {
                row.push((-10.0 + 20.0 * i as f64 / 320.0, 1.5, -10.0 + 20.0 * j as f64 / 200.0));
            }
        }
        let dense = points_geometry(&row);
        // The source's radius-5 UV sphere: 80 latitude bands, 120 segments,
        // shared poles and the same triangle order as test_support/uv_sphere.ml.
        let (rings, segments) = (80_i64, 120_i64);
        let mut sphere_points = vec![(0.0, 5.0, 0.0)];
        for ring in 1..rings {
            let theta = std::f64::consts::PI * ring as f64 / rings as f64;
            for segment in 0..segments {
                let phi = std::f64::consts::PI * 2.0 * segment as f64 / segments as f64;
                sphere_points.push((5.0 * theta.sin() * phi.cos(), 5.0 * theta.cos(), 5.0 * theta.sin() * phi.sin()));
            }
        }
        sphere_points.push((0.0, -5.0, 0.0));
        let point = |ring: i64, segment: i64| {
            if ring == 0 { 0 } else if ring == rings { 1 + (rings - 1) * segments }
            else { 1 + (ring - 1) * segments + segment % segments }
        };
        let mut triangles = Vec::new();
        for segment in 0..segments {
            triangles.extend([point(0, segment), point(1, segment + 1), point(1, segment)]);
        }
        for ring in 1..rings - 1 {
            for segment in 0..segments {
                let (a, b, c, d) = (point(ring, segment), point(ring, segment + 1),
                    point(ring + 1, segment + 1), point(ring + 1, segment));
                triangles.extend([a, c, d, a, b, c]);
            }
        }
        for segment in 0..segments {
            triangles.extend([point(rings, segment), point(rings - 1, segment), point(rings - 1, segment + 1)]);
        }
        let offsets = (0..=triangles.len() / 3).map(|index| index as i64 * 3).collect();
        let sphere_topology = topology::polygons_owned(sphere_points.len() as i64, triangles, offsets).unwrap();
        let surface = geometry::create(
            geometry::positions(&points_geometry(&sphere_points)).clone(), sphere_topology, None, None, None,
        ).unwrap();
        let count = geometry::point_count(&dense);
        let affected = point_group("affected", count, &|point| point % 3 != 0);
        for kind in [DistanceReferencePoints, DistanceReferencePrimitives] {
            let run = |domains: i64| {
                let slot = std::sync::Mutex::new(None);
                parallel::run_unit(Some(domains), &|| {
                    let mut p = params(&surface, kind, None);
                    p.affected = Some(element_selection::T::SelectedPoints(affected.clone()));
                    *slot.lock().unwrap() = Some(distance_from_geometry(&p, &dense).unwrap());
                    Ok(())
                })
                .unwrap();
                slot.into_inner().unwrap().unwrap()
            };
            let (one, four) = (run(1), run(4));
            assert_eq!(float_attribute("distance", &one), float_attribute("distance", &four));
            assert_eq!(float_attribute("mask", &one), float_attribute("mask", &four));
        }
    }
}
