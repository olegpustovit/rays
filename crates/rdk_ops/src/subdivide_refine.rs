//! Port of lib/rdk/ops/mesh/subdivide_refine.ml: stencil plans, creases, face-varying refinement,
//! attribute/group remapping and local-part combination for polygon subdivision.
use std::sync::Arc;
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::support::Windows;
use rdk_core::{
    cancel, edge_group, error, geometry, group, packed, ragged_ops, support, topology,
    topology_index, topology_remap,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scheme {
    CatmullClark,
    Loop,
    Bilinear,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoundaryInterpolation {
    SubdivideBoundaryNone,
    SubdivideBoundaryEdgeOnly,
    SubdivideBoundaryEdgeAndCorner,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FaceVaryingInterpolation {
    SubdivideFvarNone,
    SubdivideFvarCornersOnly,
    SubdivideFvarCornersPlus1,
    SubdivideFvarCornersPlus2,
    SubdivideFvarBoundaries,
    SubdivideFvarAll,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TrianglePolicy {
    SubdivideTrianglesCatmullClark,
    SubdivideTrianglesSmooth,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CreasingMethod {
    SubdivideCreasingUniform,
    SubdivideCreasingChaikin,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CrackPolicy {
    SubdivideDoNotClose,
    SubdividePullNoEdgeDivision,
    SubdividePullDivideEdges(f64),
    SubdividePullTriangulate(f64),
    SubdivideStitchNoEdgeDivision,
    SubdivideStitchDivideEdges,
    SubdivideStitchTriangulate,
}

use BoundaryInterpolation::*;
use CreasingMethod::*;
use FaceVaryingInterpolation::*;
use TrianglePolicy::*;

pub const EDGE_ANCESTRY_PREFIX: &str = "__rdk_subdivide_edge_ancestry_";

/// OCaml `Error.Fail message`: the guard at the public entry rebuilds it with the operation's code.
pub fn fail(message: String) -> error::T {
    error::make(
        "Rdk_mesh.Subdivide.subdivide".to_owned(),
        "invalid_parameter".to_owned(),
        message,
        None,
    )
    .unwrap_or_else(|failure| failure)
}

pub fn failm(message: &str) -> error::T {
    fail(message.to_owned())
}

/// OCaml `Invalid_argument`.
pub fn invalid_arg(message: String) -> error::T {
    error::make(
        "Rdk_mesh.Subdivide.subdivide".to_owned(),
        "invalid_argument".to_owned(),
        message,
        None,
    )
    .unwrap_or_else(|failure| failure)
}

/// OCaml `max_int` is 2^62 - 1.
pub const OCAML_MAX_INT: i64 = (1_i64 << 62) - 1;

pub fn checked_add(name: &str, a: i64, b: i64) -> Result<i64, error::T> {
    if b < 0 || a > OCAML_MAX_INT - b {
        return Err(fail(format!("{name} exceeds integer limits")));
    }
    Ok(a + b)
}

pub fn checked_mul(name: &str, a: i64, b: i64) -> Result<i64, error::T> {
    if a < 0 || b < 0 || (a != 0 && b > OCAML_MAX_INT / a) {
        return Err(fail(format!("{name} exceeds integer limits")));
    }
    Ok(a * b)
}

pub fn gi(values: &[i64], index: i64) -> i64 {
    values[index as usize]
}

/// An index stored in the i32 per-vertex scratch arrays; `make_topology` rejects sizes beyond i32.
fn narrow(value: i64) -> i32 {
    i32::try_from(value).expect("scratch index fits i32")
}

/// `gi` for the i32 scratch arrays.
pub fn gn(values: &[i32], index: i64) -> i64 {
    i64::from(values[index as usize])
}

pub fn gf(values: &[f64], index: i64) -> f64 {
    values[index as usize]
}

pub fn gu(values: &[u8], index: i64) -> u8 {
    values[index as usize]
}

/// Stdlib `max` on floats: `if a >= b then a else b`.
pub fn fmax(a: f64, b: f64) -> f64 {
    if a >= b { a } else { b }
}

/// Stdlib `min` on floats: `if a <= b then a else b`.
pub fn fmin(a: f64, b: f64) -> f64 {
    if a <= b { a } else { b }
}

/// The `?grain` argument: positive, default 16 384.
fn grain_of(grain: Option<i64>) -> Result<i64, error::T> {
    let grain = grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(invalid_arg("Rdk_mesh.Subdivide.subdivide: grain must be positive".to_owned()));
    }
    Ok(grain)
}

/// `Vec` of `count` elements with `out[i] = work(i)`, written through disjoint `grain`-sized windows.
fn filled<T: Clone + Default + Send>(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    count: i64,
    work: &(dyn Fn(i64) -> Result<T, error::T> + Sync),
) -> Result<Vec<T>, error::T> {
    let mut output = vec![T::default(); count.max(0) as usize];
    support::par_fill(cancel_token, grain_of(grain)?, &mut output, work)?;
    Ok(output)
}

#[derive(Clone)]
pub struct Plan {
    pub scheme: Scheme,
    pub source: geometry::T,
    pub source_topology: topology::View::View,
    pub index: topology_index::View::View,
    pub source_points: i64,
    pub source_vertices: i64,
    pub source_primitives: i64,
    pub edge_count: i64,
    pub face_offset: i64,
    pub output_points: i64,
    pub point_offsets: Vec<i64>,
    pub point_sources: Vec<i64>,
    pub point_weights: Vec<f64>,
    pub point_representative: Vec<i64>,
    pub edge_ancestry_attribute: Option<String>,
    pub boundary_interpolation: BoundaryInterpolation,
    pub triangle_subdivision: TrianglePolicy,
    pub creasing_method: CreasingMethod,
    pub creases: Option<CreasePlan>,
    pub vertex_kind: Vec<u8>,
    pub vertex_a: Vec<i32>,
    pub vertex_b: Vec<i32>,
    pub vertex_primitive: Vec<i32>,
    pub vertex_edge_source: Vec<i32>,
    pub primitive_source: Vec<i32>,
    pub topology: topology::T,
}

#[derive(Clone)]
pub struct CreasePlan {
    pub method_: CreasingMethod,
    pub edge_sharpness: Vec<f64>,
    pub child_edge_sharpness: Vec<f64>,
    pub has_edge_creases: bool,
    pub vertex_masks: VertexMaskPlan,
}

#[derive(Clone)]
pub struct VertexMaskPlan {
    pub parent_kind: Vec<u8>,
    pub child_kind: Vec<u8>,
    pub transition_factor: Vec<f64>,
    pub parent_a: Vec<i64>,
    pub parent_b: Vec<i64>,
    pub child_a: Vec<i64>,
    pub child_b: Vec<i64>,
}

fn owner_name(owner: Owner) -> &'static str {
    match owner {
        Owner::Point => "point",
        Owner::Vertex => "vertex",
        Owner::Primitive => "primitive",
        Owner::Detail => "detail",
    }
}

pub fn float_attribute(
    owner: Owner,
    name: &str,
    geometry_value: &geometry::T,
) -> Result<Option<Vec<f64>>, error::T> {
    match geometry::find_attribute(owner, name, geometry_value) {
        None => Ok(None),
        Some(found) => match attribute::View::storage(&found)? {
            Storage::Float(values) => Ok(Some(values.to_vec())),
            _ => Err(fail(format!("{} {} must use float storage", owner_name(owner), name))),
        },
    }
}

fn validate_sharpness(name: &str, values: &[f64]) -> Result<(), error::T> {
    for &value in values {
        if !value.is_finite() || value < 0.0 {
            return Err(fail(format!("{name} values must be finite and non-negative")));
        }
    }
    Ok(())
}

/// (edge sharpness, corner sharpness, has edge creases, has corner creases).
pub type CreaseData = (Vec<f64>, Vec<f64>, bool, bool);

pub fn crease_data(
    edge_override: Option<f64>,
    geometry_value: &geometry::T,
    topology_view: &topology::View::View,
    index: &topology_index::View::View,
) -> Result<Option<CreaseData>, error::T> {
    let edge_count = index.edge_a.len();
    let vertex_weights = float_attribute(Owner::Vertex, "creaseweight", geometry_value)?;
    let primitive_weights = float_attribute(Owner::Primitive, "creaseweight", geometry_value)?;
    let corner_weights = float_attribute(Owner::Point, "cornerweight", geometry_value)?;
    if let Some(value) = edge_override {
        if !value.is_finite() || value < 0.0 {
            return Err(failm("crease override must be finite and non-negative"));
        }
    }
    if let Some(values) = &vertex_weights {
        validate_sharpness("vertex creaseweight", values)?;
    }
    if let Some(values) = &primitive_weights {
        validate_sharpness("primitive creaseweight", values)?;
    }
    if let Some(values) = &corner_weights {
        validate_sharpness("point cornerweight", values)?;
    }
    if edge_override.is_none()
        && vertex_weights.is_none()
        && primitive_weights.is_none()
        && corner_weights.is_none()
    {
        return Ok(None);
    }
    let edge_sharpness = match edge_override {
        Some(value) => vec![value; edge_count],
        None => {
            let mut edge_sharpness = vec![0.0; edge_count];
            if let Some(values) = &vertex_weights {
                for vertex in 0..topology_view.vertex_points.len() {
                    let edge = index.edge_of_vertex[vertex];
                    if edge >= 0 {
                        let slot = &mut edge_sharpness[edge as usize];
                        *slot = fmax(*slot, values[vertex]);
                    }
                }
            }
            if let Some(values) = &primitive_weights {
                for primitive in 0..topology_view.primitive_kinds.len() {
                    let weight = values[primitive];
                    for vertex in topology_view.primitive_offsets[primitive]
                        ..topology_view.primitive_offsets[primitive + 1]
                    {
                        let edge = index.edge_of_vertex[vertex as usize];
                        if edge >= 0 {
                            let slot = &mut edge_sharpness[edge as usize];
                            *slot = fmax(*slot, weight);
                        }
                    }
                }
            }
            edge_sharpness
        }
    };
    let corner_sharpness = match &corner_weights {
        Some(values) => values.clone(),
        None => vec![0.0; topology_view.point_count as usize],
    };
    Ok(Some((
        edge_sharpness,
        corner_sharpness,
        edge_override.is_some() || vertex_weights.is_some() || primitive_weights.is_some(),
        corner_weights.is_some(),
    )))
}

fn same_topology(left: &topology::View::View, right: &topology::View::View) -> bool {
    left.point_count == right.point_count
        && left.vertex_points == right.vertex_points
        && left.primitive_offsets == right.primitive_offsets
        && left.primitive_kinds == right.primitive_kinds
}

fn validate_crease_selection(selection: &group::T, creases: &geometry::T) -> Result<(), error::T> {
    if group::owner(selection) != group::Owner::Primitive {
        return Err(failm("crease selection must be a primitive group"));
    }
    let expected = geometry::primitive_count(creases);
    if group::length(selection) != expected {
        return Err(fail(format!(
            "crease selection length {} does not match the crease primitive count {}",
            group::length(selection),
            expected
        )));
    }
    Ok(())
}

pub fn validate_hole_group(holes: &group::T, geometry_value: &geometry::T) -> Result<(), error::T> {
    if group::owner(holes) != group::Owner::Primitive {
        return Err(failm("subdivision holes must be a primitive group"));
    }
    let expected = geometry::primitive_count(geometry_value);
    if group::length(holes) != expected {
        return Err(fail(format!(
            "subdivision hole group length {} does not match the primitive count {}",
            group::length(holes),
            expected
        )));
    }
    Ok(())
}

pub fn apply_crease_input(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    selection: Option<&group::T>,
    override_value: Option<f64>,
    source: &geometry::T,
    creases: &geometry::T,
) -> Result<geometry::T, error::T> {
    if let Some(value) = override_value {
        if !value.is_finite() || value < 0.0 {
            return Err(failm("crease override must be finite and non-negative"));
        }
    }
    if let Some(selection) = selection {
        validate_crease_selection(selection, creases)?;
    }
    let source_topology = topology::View::view(geometry::topology(source))?;
    let crease_topology = topology::View::view(geometry::topology(creases))?;
    let topology_identical = same_topology(&source_topology, &crease_topology);
    let crease_vertex_weights = float_attribute(Owner::Vertex, "creaseweight", creases)?;
    let crease_primitive_weights = float_attribute(Owner::Primitive, "creaseweight", creases)?;
    if let Some(values) = &crease_vertex_weights {
        validate_sharpness("second-input vertex creaseweight", values)?;
    }
    if let Some(values) = &crease_primitive_weights {
        validate_sharpness("second-input primitive creaseweight", values)?;
    }
    if override_value.is_none()
        && (crease_vertex_weights.is_some() || crease_primitive_weights.is_some())
        && !topology_identical
    {
        return Err(failm(
            "creaseweight attributes require the source and crease inputs to have identical topology",
        ));
    }
    let contributes = override_value.is_some()
        || crease_vertex_weights.is_some()
        || crease_primitive_weights.is_some();
    if !contributes {
        return Ok(source.clone());
    }
    let source_index_value = topology_index::create(cancel_token, geometry::topology(source))?;
    let crease_index_value = if topology_identical {
        source_index_value.clone()
    } else {
        topology_index::create(cancel_token, geometry::topology(creases))?
    };
    let source_index = topology_index::View::view(&source_index_value);
    let crease_index = topology_index::View::view(&crease_index_value);
    let source_edge_count = source_index.edge_a.len() as i64;
    let mut edge_sharpness = match crease_data(None, source, &source_topology, &source_index)? {
        Some((values, _, _, _)) => values,
        None => vec![0.0; source_edge_count as usize],
    };
    let selected = |primitive: i64| match selection {
        None => true,
        Some(selection) => group::mem(primitive, selection),
    };
    let contribution = |vertex: i64, primitive: i64| match override_value {
        Some(weight) => weight,
        None => fmax(
            match &crease_vertex_weights {
                None => 0.0,
                Some(values) => gf(values, vertex),
            },
            match &crease_primitive_weights {
                None => 0.0,
                Some(values) => gf(values, primitive),
            },
        ),
    };
    let assigned: Vec<u8>;
    let assigned_weights: Vec<f64>;
    if topology_identical {
        if crease_index.edge_a.len() as i64 != source_edge_count {
            return Err(failm("identical crease topology produced a different edge index"));
        }
        for edge in 0..source_edge_count as usize {
            if crease_index.edge_a[edge] != source_index.edge_a[edge]
                || crease_index.edge_b[edge] != source_index.edge_b[edge]
            {
                return Err(failm("identical crease topology produced unstable edge numbering"));
            }
        }
        let mut edge_assigned = vec![0u8; source_edge_count as usize];
        let mut edge_weight = vec![0.0; source_edge_count as usize];
        let grain = grain_of(grain)?;
        let assigned_windows = Windows::even(&mut edge_assigned, grain);
        let weight_windows = Windows::even(&mut edge_weight, grain);
        support::run_indexed_ranges(grain, source_edge_count, &|range, first, last| {
            let (assigned_out, weight_out) = (assigned_windows.take(range), weight_windows.take(range));
            for edge in first..last {
                support::poll(cancel_token, edge)?;
                let mut have = false;
                let mut weight = 0.0;
                for at in
                    gi(&crease_index.edge_offsets, edge)..gi(&crease_index.edge_offsets, edge + 1)
                {
                    let vertex = gi(&crease_index.edge_vertices, at);
                    let primitive = gi(&crease_index.primitive_of_vertex, vertex);
                    if selected(primitive) {
                        let value = contribution(vertex, primitive);
                        if !have || value > weight {
                            have = true;
                            weight = value;
                        }
                    }
                }
                assigned_out[(edge - first) as usize] = have as u8;
                weight_out[(edge - first) as usize] = weight;
            }
            Ok(())
        })?;
        assigned = edge_assigned;
        assigned_weights = edge_weight;
    } else {
        let mut local_assigned = vec![0u8; source_edge_count as usize];
        let mut local_weights = vec![0.0f64; source_edge_count as usize];
        for primitive in 0..crease_topology.primitive_kinds.len() as i64 {
            cancel::poll(cancel_token, primitive, 4095)?;
            if selected(primitive) {
                let first = gi(&crease_topology.primitive_offsets, primitive);
                let last = gi(&crease_topology.primitive_offsets, primitive + 1);
                for vertex in first..last {
                    let crease_edge = gi(&crease_index.edge_of_vertex, vertex);
                    if crease_edge >= 0 {
                        let a = gi(&crease_index.edge_a, crease_edge);
                        let b = gi(&crease_index.edge_b, crease_edge);
                        if let Some(source_edge) =
                            topology_index::find_edge(&source_index_value, a, b)
                        {
                            let weight = contribution(vertex, primitive);
                            let slot = source_edge as usize;
                            if local_assigned[slot] == 0 || weight > local_weights[slot] {
                                local_assigned[slot] = 1;
                                local_weights[slot] = weight;
                            }
                        }
                    }
                }
            }
        }
        assigned = local_assigned;
        assigned_weights = local_weights;
    }
    let mut changed = false;
    for edge in 0..source_edge_count as usize {
        if assigned[edge] != 0 {
            if edge_sharpness[edge] != assigned_weights[edge] {
                changed = true;
            }
            edge_sharpness[edge] = assigned_weights[edge];
        }
    }
    if !changed
        && geometry::find_attribute(Owner::Vertex, "creaseweight", source).is_some()
        && geometry::find_attribute(Owner::Primitive, "creaseweight", source).is_none()
    {
        return Ok(source.clone());
    }
    let mut values = vec![0.0; source_topology.vertex_points.len()];
    for vertex in 0..values.len() {
        let edge = source_index.edge_of_vertex[vertex];
        if edge >= 0 {
            values[vertex] = edge_sharpness[edge as usize];
        }
    }
    let created = attribute::create_owned("creaseweight", Owner::Vertex, Storage::Float(Arc::new(values)))?;
    let stripped = geometry::without_attribute(Owner::Primitive, "creaseweight", source);
    geometry::with_attribute(created, &stripped)
}

pub fn clamp_sharpness(value: f64) -> f64 {
    if value <= 0.0 {
        0.0
    } else if value >= 1.0 {
        1.0
    } else {
        value
    }
}

pub fn build_child_edge_sharpness(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    method_: CreasingMethod,
    index: &topology_index::View::View,
    point_count: i64,
    edge_sharpness: &[f64],
) -> Result<Vec<f64>, error::T> {
    // Chaikin averages the positive sharpness of the edges around a point.
    let sharp_totals = filled(cancel_token, grain, point_count, &|point| {
        let mut sharp_sum = 0.0;
        let mut sharp_count = 0_i64;
        if method_ == SubdivideCreasingChaikin {
            for at in gi(&index.point_edge_offsets, point)..gi(&index.point_edge_offsets, point + 1) {
                let weight = gf(edge_sharpness, gi(&index.point_edges, at));
                if weight > 0.0 {
                    sharp_sum += weight;
                    sharp_count += 1;
                }
            }
        }
        Ok((sharp_sum, sharp_count))
    })?;
    // Slot `edge * 2 + side` belongs to the edge's endpoint on that side.
    filled(cancel_token, grain, edge_sharpness.len() as i64 * 2, &|slot| {
        let (edge, side) = (slot / 2, slot % 2);
        let point = gi(if side == 0 { &index.edge_a } else { &index.edge_b }, edge);
        // A loop edge lists its point once, so its second slot stays 0.
        if side == 1 && point == gi(&index.edge_a, edge) {
            return Ok(0.0);
        }
        let (sharp_sum, sharp_count) = sharp_totals[point as usize];
        let parent = gf(edge_sharpness, edge);
        Ok(match method_ {
            SubdivideCreasingUniform => fmax(0.0, parent - 1.0),
            SubdivideCreasingChaikin => {
                if parent <= 0.0 {
                    0.0
                } else if sharp_count <= 1 {
                    fmax(0.0, parent - 1.0)
                } else {
                    let others = (sharp_sum - parent) / ((sharp_count - 1) as f64);
                    fmax(0.0, (0.75 * parent) + (0.25 * others) - 1.0)
                }
            }
        })
    })
}

pub fn build_vertex_sharp_plan(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    method_: CreasingMethod,
    index: &topology_index::View::View,
    edge_sharpness: &[f64],
    child_edge_sharpness: &[f64],
    corner_sharpness: &[f64],
) -> Result<VertexMaskPlan, error::T> {
    let point_count = corner_sharpness.len() as i64;
    let count = point_count as usize;
    let mut parent_kind = vec![0u8; count];
    let mut child_kind = vec![0u8; count];
    let mut transition_factor = vec![1.0; count];
    let mut parent_a = vec![0_i64; count];
    let mut parent_b = vec![0_i64; count];
    let mut child_a = vec![0_i64; count];
    let mut child_b = vec![0_i64; count];
    let child_weight = |point: i64, edge: i64| {
        let side = if gi(&index.edge_a, edge) == point { 0 } else { 1 };
        gf(child_edge_sharpness, edge * 2 + side)
    };
    let grain = grain_of(grain)?;
    let parent_kind_windows = Windows::even(&mut parent_kind, grain);
    let child_kind_windows = Windows::even(&mut child_kind, grain);
    let transition_windows = Windows::even(&mut transition_factor, grain);
    let parent_a_windows = Windows::even(&mut parent_a, grain);
    let parent_b_windows = Windows::even(&mut parent_b, grain);
    let child_a_windows = Windows::even(&mut child_a, grain);
    let child_b_windows = Windows::even(&mut child_b, grain);
    support::run_indexed_ranges(grain, point_count, &|range, first_point, last_point| {
        let parent_kind_out = parent_kind_windows.take(range);
        let child_kind_out = child_kind_windows.take(range);
        let transition_out = transition_windows.take(range);
        let parent_a_out = parent_a_windows.take(range);
        let parent_b_out = parent_b_windows.take(range);
        let child_a_out = child_a_windows.take(range);
        let child_b_out = child_b_windows.take(range);
        for point in first_point..last_point {
            support::poll(cancel_token, point)?;
            let slot = (point - first_point) as usize;
            let first = gi(&index.point_edge_offsets, point);
            let last = gi(&index.point_edge_offsets, point + 1);
            let classify = |child: bool| -> (u8, i64, i64, f64) {
                let mut count = 0_i64;
                let mut a = point;
                let mut b = point;
                for at in first..last {
                    let edge = gi(&index.point_edges, at);
                    let weight =
                        if child { child_weight(point, edge) } else { gf(edge_sharpness, edge) };
                    if weight > 0.0 {
                        let neighbor = if gi(&index.edge_a, edge) == point {
                            gi(&index.edge_b, edge)
                        } else {
                            gi(&index.edge_a, edge)
                        };
                        if count == 0 {
                            a = neighbor;
                        }
                        b = neighbor;
                        count += 1;
                    }
                }
                let corner = if child {
                    fmax(0.0, gf(corner_sharpness, point) - 1.0)
                } else {
                    gf(corner_sharpness, point)
                };
                let kind = if corner > 0.0 || count > 2 {
                    2
                } else if count == 2 {
                    1
                } else {
                    0
                };
                (kind, a, b, corner)
            };
            let (p_kind, p_a, p_b, parent_corner) = classify(false);
            let (c_kind, c_a, c_b, child_corner) = classify(true);
            parent_kind_out[slot] = p_kind;
            child_kind_out[slot] = c_kind;
            parent_a_out[slot] = p_a;
            parent_b_out[slot] = p_b;
            child_a_out[slot] = c_a;
            child_b_out[slot] = c_b;
            if p_kind != c_kind {
                let mut sum = 0.0;
                let mut count = 0_i64;
                if parent_corner > 0.0 && child_corner <= 0.0 {
                    sum = parent_corner;
                    count = 1;
                }
                for at in first..last {
                    let edge = gi(&index.point_edges, at);
                    let parent = gf(edge_sharpness, edge);
                    let child = child_weight(point, edge);
                    let transitioned = match method_ {
                        SubdivideCreasingUniform => parent > 0.0 && parent <= 1.0,
                        SubdivideCreasingChaikin => parent > 0.0 && child <= 0.0,
                    };
                    if transitioned {
                        sum += parent;
                        count += 1;
                    }
                }
                transition_out[slot] =
                    if count == 0 { 0.0 } else { clamp_sharpness(sum / (count as f64)) };
            }
        }
        Ok(())
    })?;
    Ok(VertexMaskPlan {
        parent_kind,
        child_kind,
        transition_factor,
        parent_a,
        parent_b,
        child_a,
        child_b,
    })
}

pub fn primitive_size(topology_view: &topology::View::View, primitive: i64) -> i64 {
    gi(&topology_view.primitive_offsets, primitive + 1)
        - gi(&topology_view.primitive_offsets, primitive)
}

fn not_finite(value: &f64) -> bool {
    !value.is_finite()
}

pub fn validate(
    cancel_token: Option<&cancel::T>,
    scheme: Scheme,
    geometry_value: &geometry::T,
    topology_view: &topology::View::View,
    index: &topology_index::View::View,
) -> Result<(), error::T> {
    if topology_view.point_count == 0 || topology_view.primitive_kinds.is_empty() {
        return Err(failm("input must contain polygon geometry"));
    }
    let mut point_marks = vec![-1_i64; topology_view.point_count as usize];
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        cancel::poll(cancel_token, primitive, 1023)?;
        if topology_view.primitive_kinds[primitive as usize] != 0 {
            return Err(failm("only polygon primitives are supported"));
        }
        let first = gi(&topology_view.primitive_offsets, primitive);
        let last = gi(&topology_view.primitive_offsets, primitive + 1);
        let size = last - first;
        if size < 3 {
            return Err(failm("polygons must contain at least three vertices"));
        }
        if scheme == Scheme::Loop && size != 3 {
            return Err(failm("Loop subdivision requires triangle polygons"));
        }
        for vertex in first..last {
            let next = if vertex + 1 == last { first } else { vertex + 1 };
            if gi(&topology_view.vertex_points, vertex) == gi(&topology_view.vertex_points, next) {
                return Err(failm("a polygon contains a zero-length topology edge"));
            }
        }
        for vertex in first..last {
            let point = gi(&topology_view.vertex_points, vertex);
            if point_marks[point as usize] == primitive {
                return Err(failm("a polygon references the same point more than once"));
            }
            point_marks[point as usize] = primitive;
        }
    }
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    if positions.x.iter().any(not_finite)
        || positions.y.iter().any(not_finite)
        || positions.z.iter().any(not_finite)
    {
        return Err(failm("positions must be finite"));
    }
    for edge in 0..index.edge_a.len() as i64 {
        let incidence = gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge);
        if incidence > 2 {
            return Err(failm("non-manifold edges are not supported"));
        }
    }
    let mut face_marks = vec![-1_i64; topology_view.primitive_kinds.len()];
    let mut fan_stack = vec![0_i64; topology_view.vertex_points.len()];
    for point in 0..topology_view.point_count {
        cancel::poll(cancel_token, point, 4095)?;
        let edge_first = gi(&index.point_edge_offsets, point);
        let edge_last = gi(&index.point_edge_offsets, point + 1);
        let mut boundary = 0_i64;
        for at in edge_first..edge_last {
            let edge = gi(&index.point_edges, at);
            if gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge) == 1 {
                boundary += 1;
            }
        }
        let faces = gi(&index.point_offsets, point + 1) - gi(&index.point_offsets, point);
        let edges = edge_last - edge_first;
        if boundary != 0 && boundary != 2 {
            return Err(failm("a boundary vertex does not have exactly two boundary edges"));
        }
        if (boundary == 0 && edges != faces) || (boundary == 2 && edges != faces + 1) {
            return Err(failm("a vertex has non-manifold polygon incidence"));
        }
        if faces > 0 {
            let mut stack_count = 1_i64;
            let mut reached = 0_i64;
            fan_stack[0] = gi(&index.point_vertices, gi(&index.point_offsets, point));
            while stack_count > 0 {
                stack_count -= 1;
                let corner = fan_stack[stack_count as usize];
                let primitive = gi(&index.primitive_of_vertex, corner);
                if face_marks[primitive as usize] != point {
                    face_marks[primitive as usize] = point;
                    reached += 1;
                    let outgoing = gi(&index.edge_of_vertex, corner);
                    let incoming = gi(&index.edge_of_vertex, gi(&index.previous_vertex, corner));
                    for side in 0..2 {
                        let edge = if side == 0 { outgoing } else { incoming };
                        if gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge) == 2 {
                            for at in gi(&index.edge_offsets, edge)..gi(&index.edge_offsets, edge + 1)
                            {
                                let directed = gi(&index.edge_vertices, at);
                                let other_primitive = gi(&index.primitive_of_vertex, directed);
                                if other_primitive != primitive
                                    && face_marks[other_primitive as usize] != point
                                {
                                    let adjacent =
                                        if gi(&topology_view.vertex_points, directed) == point {
                                            directed
                                        } else {
                                            gi(&index.next_vertex, directed)
                                        };
                                    if adjacent < 0
                                        || gi(&topology_view.vertex_points, adjacent) != point
                                    {
                                        return Err(failm(
                                            "edge incidence is inconsistent at a vertex",
                                        ));
                                    }
                                    fan_stack[stack_count as usize] = adjacent;
                                    stack_count += 1;
                                }
                            }
                        }
                    }
                }
            }
            if reached != faces {
                return Err(failm("a vertex has disconnected polygon fans"));
            }
        }
    }
    Ok(())
}

pub fn boundary_neighbors(index: &topology_index::View::View, point: i64) -> (i64, i64, i64) {
    let first = gi(&index.point_edge_offsets, point);
    let last = gi(&index.point_edge_offsets, point + 1);
    let mut a = -1;
    let mut b = -1;
    let mut count = 0_i64;
    for at in first..last {
        let edge = gi(&index.point_edges, at);
        if gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge) == 1 {
            let neighbor = if gi(&index.edge_a, edge) == point {
                gi(&index.edge_b, edge)
            } else {
                gi(&index.edge_a, edge)
            };
            if count == 0 {
                a = neighbor;
            } else if count == 1 {
                b = neighbor;
            }
            count += 1;
        }
    }
    (a, b, count)
}

pub fn boundary_corner(
    boundary_interpolation: BoundaryInterpolation,
    index: &topology_index::View::View,
    point: i64,
) -> bool {
    boundary_interpolation == SubdivideBoundaryEdgeAndCorner
        && gi(&index.point_offsets, point + 1) - gi(&index.point_offsets, point) == 1
}

pub fn point_term_count(
    boundary_interpolation: BoundaryInterpolation,
    scheme: Scheme,
    topology_view: &topology::View::View,
    index: &topology_index::View::View,
    source_points: i64,
    edge_count: i64,
    output: i64,
) -> Result<i64, error::T> {
    if output < source_points {
        let point = output;
        match scheme {
            Scheme::Bilinear => Ok(1),
            Scheme::Loop => {
                let (_, _, boundary) = boundary_neighbors(index, point);
                if boundary == 2 {
                    Ok(if boundary_corner(boundary_interpolation, index, point) { 1 } else { 3 })
                } else {
                    checked_add(
                        "Loop stencil size",
                        1,
                        gi(&index.point_edge_offsets, point + 1)
                            - gi(&index.point_edge_offsets, point),
                    )
                }
            }
            Scheme::CatmullClark => {
                let (_, _, boundary) = boundary_neighbors(index, point);
                if boundary == 2 {
                    Ok(if boundary_corner(boundary_interpolation, index, point) { 1 } else { 3 })
                } else {
                    let edges = gi(&index.point_edge_offsets, point + 1)
                        - gi(&index.point_edge_offsets, point);
                    let mut corners = 0_i64;
                    for at in gi(&index.point_offsets, point)..gi(&index.point_offsets, point + 1) {
                        let vertex = gi(&index.point_vertices, at);
                        let primitive = gi(&index.primitive_of_vertex, vertex);
                        corners += primitive_size(topology_view, primitive);
                    }
                    checked_add(
                        "Catmull-Clark stencil size",
                        1,
                        checked_add(
                            "Catmull-Clark stencil size",
                            checked_mul("Catmull-Clark stencil size", edges, 2)?,
                            corners,
                        )?,
                    )
                }
            }
        }
    } else if output < source_points + edge_count {
        let edge = output - source_points;
        let incidence = gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge);
        match scheme {
            Scheme::Bilinear => Ok(2),
            Scheme::Loop => Ok(if incidence == 2 { 4 } else { 2 }),
            Scheme::CatmullClark => {
                if incidence == 2 {
                    let mut count = 2_i64;
                    for at in gi(&index.edge_offsets, edge)..gi(&index.edge_offsets, edge + 1) {
                        let vertex = gi(&index.edge_vertices, at);
                        count = checked_add(
                            "Catmull-Clark edge stencil size",
                            count,
                            primitive_size(topology_view, gi(&index.primitive_of_vertex, vertex)),
                        )?;
                    }
                    Ok(count)
                } else {
                    Ok(2)
                }
            }
        }
    } else {
        Ok(primitive_size(topology_view, output - source_points - edge_count))
    }
}

/// Writes `output`'s stencil terms into its exactly-sized `sources`/`weights` slots.
#[allow(clippy::too_many_arguments)]
pub fn fill_point_stencil(
    boundary_interpolation: BoundaryInterpolation,
    triangle_subdivision: TrianglePolicy,
    scheme: Scheme,
    topology_view: &topology::View::View,
    index: &topology_index::View::View,
    source_points: i64,
    edge_count: i64,
    sources: &mut [i64],
    weights: &mut [f64],
    representative: &mut i64,
    output: i64,
) -> Result<(), error::T> {
    let mut cursor = 0;
    let mut add = |source: i64, weight: f64| {
        sources[cursor] = source;
        weights[cursor] = weight;
        cursor += 1;
    };
    if output < source_points {
        let point = output;
        *representative = point;
        match scheme {
            Scheme::Bilinear => add(point, 1.0),
            Scheme::Loop => {
                let (left, right, boundary) = boundary_neighbors(index, point);
                if boundary == 2 {
                    if boundary_corner(boundary_interpolation, index, point) {
                        add(point, 1.0);
                    } else {
                        add(point, 0.75);
                        add(left, 0.125);
                        add(right, 0.125);
                    }
                } else {
                    let first = gi(&index.point_edge_offsets, point);
                    let last = gi(&index.point_edge_offsets, point + 1);
                    let n = last - first;
                    if n == 0 {
                        add(point, 1.0);
                    } else {
                        let nf = n as f64;
                        let angle = 2.0 * std::f64::consts::PI / nf;
                        let beta = (0.625 - (0.375 + (0.25 * angle.cos())).powf(2.0)) / nf;
                        add(point, 1.0 - (nf * beta));
                        for at in first..last {
                            let edge = gi(&index.point_edges, at);
                            add(
                                if gi(&index.edge_a, edge) == point {
                                    gi(&index.edge_b, edge)
                                } else {
                                    gi(&index.edge_a, edge)
                                },
                                beta,
                            );
                        }
                    }
                }
            }
            Scheme::CatmullClark => {
                let (left, right, boundary) = boundary_neighbors(index, point);
                if boundary == 2 {
                    if boundary_corner(boundary_interpolation, index, point) {
                        add(point, 1.0);
                    } else {
                        add(point, 0.75);
                        add(left, 0.125);
                        add(right, 0.125);
                    }
                } else {
                    let edge_first = gi(&index.point_edge_offsets, point);
                    let edge_last = gi(&index.point_edge_offsets, point + 1);
                    let face_first = gi(&index.point_offsets, point);
                    let face_last = gi(&index.point_offsets, point + 1);
                    let n = edge_last - edge_first;
                    let faces = face_last - face_first;
                    if n == 0 || faces == 0 {
                        add(point, 1.0);
                    } else {
                        let nf = n as f64;
                        add(point, (nf - 3.0) / nf);
                        let edge_weight = 1.0 / (nf * nf);
                        for at in edge_first..edge_last {
                            let edge = gi(&index.point_edges, at);
                            add(gi(&index.edge_a, edge), edge_weight);
                            add(gi(&index.edge_b, edge), edge_weight);
                        }
                        let face_count = faces as f64;
                        for at in face_first..face_last {
                            let vertex = gi(&index.point_vertices, at);
                            let primitive = gi(&index.primitive_of_vertex, vertex);
                            let first = gi(&topology_view.primitive_offsets, primitive);
                            let last = gi(&topology_view.primitive_offsets, primitive + 1);
                            let weight = 1.0 / (nf * face_count * ((last - first) as f64));
                            for corner in first..last {
                                add(gi(&topology_view.vertex_points, corner), weight);
                            }
                        }
                    }
                }
            }
        }
    } else if output < source_points + edge_count {
        let edge = output - source_points;
        let a = gi(&index.edge_a, edge);
        let b = gi(&index.edge_b, edge);
        *representative = a.min(b);
        let incidence = gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge);
        match scheme {
            Scheme::Bilinear => {
                add(a, 0.5);
                add(b, 0.5);
            }
            Scheme::Loop => {
                if incidence == 2 {
                    add(a, 0.375);
                    add(b, 0.375);
                    for at in gi(&index.edge_offsets, edge)..gi(&index.edge_offsets, edge + 1) {
                        let vertex = gi(&index.edge_vertices, at);
                        let next = gi(&index.next_vertex, vertex);
                        add(gi(&topology_view.vertex_points, gi(&index.next_vertex, next)), 0.125);
                    }
                } else {
                    add(a, 0.5);
                    add(b, 0.5);
                }
            }
            Scheme::CatmullClark => {
                if incidence == 2 {
                    let first_incidence = gi(&index.edge_offsets, edge);
                    let face_weight = match triangle_subdivision {
                        SubdivideTrianglesCatmullClark => 0.25,
                        SubdivideTrianglesSmooth => {
                            let primitive_weight = |at: i64| {
                                let vertex = gi(&index.edge_vertices, at);
                                let primitive = gi(&index.primitive_of_vertex, vertex);
                                if primitive_size(topology_view, primitive) == 3 { 0.470 } else { 0.25 }
                            };
                            0.5 * (primitive_weight(first_incidence)
                                + primitive_weight(first_incidence + 1))
                        }
                    };
                    let vertex_weight = 0.5 * (1.0 - (2.0 * face_weight));
                    add(a, vertex_weight);
                    add(b, vertex_weight);
                    for at in gi(&index.edge_offsets, edge)..gi(&index.edge_offsets, edge + 1) {
                        let vertex = gi(&index.edge_vertices, at);
                        let primitive = gi(&index.primitive_of_vertex, vertex);
                        let first = gi(&topology_view.primitive_offsets, primitive);
                        let last = gi(&topology_view.primitive_offsets, primitive + 1);
                        let weight = face_weight / ((last - first) as f64);
                        for corner in first..last {
                            add(gi(&topology_view.vertex_points, corner), weight);
                        }
                    }
                } else {
                    add(a, 0.5);
                    add(b, 0.5);
                }
            }
        }
    } else {
        let primitive = output - source_points - edge_count;
        let first = gi(&topology_view.primitive_offsets, primitive);
        let last = gi(&topology_view.primitive_offsets, primitive + 1);
        let weight = 1.0 / ((last - first) as f64);
        *representative = gi(&topology_view.vertex_points, first);
        for vertex in first..last {
            add(gi(&topology_view.vertex_points, vertex), weight);
        }
    }
    if cursor != sources.len() {
        return Err(failm("internal stencil cardinality mismatch"));
    }
    Ok(())
}

/// Stencil terms and representatives of every output point; `point_offsets` fixes each point's term slots.
#[allow(clippy::too_many_arguments)]
fn build_stencils(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    boundary_interpolation: BoundaryInterpolation,
    triangle_subdivision: TrianglePolicy,
    scheme: Scheme,
    topology_view: &topology::View::View,
    index: &topology_index::View::View,
    source_points: i64,
    edge_count: i64,
    point_offsets: &[i64],
) -> Result<(Vec<i64>, Vec<f64>, Vec<i64>), error::T> {
    let output_points = point_offsets.len() as i64 - 1;
    let stencil_total = gi(point_offsets, output_points);
    let mut sources = vec![0_i64; stencil_total as usize];
    let mut weights = vec![0.0_f64; stencil_total as usize];
    let mut representatives = vec![0_i64; output_points as usize];
    let grain = grain_of(grain)?;
    let ranges = support::range_count(output_points, grain);
    let term_bound =
        |range: i64| gi(point_offsets, range.saturating_mul(grain).min(output_points)) as usize;
    let source_windows = Windows::new(&mut sources, ranges, term_bound);
    let weight_windows = Windows::new(&mut weights, ranges, term_bound);
    let representative_windows = Windows::even(&mut representatives, grain);
    support::run_indexed_ranges(grain, output_points, &|range, first, last| {
        let (mut source_rest, mut weight_rest) = (source_windows.take(range), weight_windows.take(range));
        let representative_out = representative_windows.take(range);
        for output in first..last {
            support::poll(cancel_token, output)?;
            let terms = (gi(point_offsets, output + 1) - gi(point_offsets, output)) as usize;
            let (output_sources, rest) = std::mem::take(&mut source_rest).split_at_mut(terms);
            source_rest = rest;
            let (output_weights, rest) = std::mem::take(&mut weight_rest).split_at_mut(terms);
            weight_rest = rest;
            fill_point_stencil(
                boundary_interpolation,
                triangle_subdivision,
                scheme,
                topology_view,
                index,
                source_points,
                edge_count,
                output_sources,
                output_weights,
                &mut representative_out[(output - first) as usize],
                output,
            )?;
        }
        Ok(())
    })?;
    Ok((sources, weights, representatives))
}

pub struct Built {
    pub vertex_kind: Vec<u8>,
    pub vertex_a: Vec<i32>,
    pub vertex_b: Vec<i32>,
    pub vertex_primitive: Vec<i32>,
    pub vertex_edge_source: Vec<i32>,
    pub primitive_source: Vec<i32>,
    pub topology: topology::T,
}

#[allow(clippy::too_many_arguments)]
pub fn make_topology(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    holes: Option<&group::T>,
    scheme: Scheme,
    topology_view: &topology::View::View,
    index: &topology_index::View::View,
    source_points: i64,
    edge_count: i64,
    face_offset: i64,
) -> Result<Built, error::T> {
    // The per-vertex scratch arrays hold source indices as i32 (half the bytes of i64).
    for (name, count) in [
        ("source vertex count", topology_view.vertex_points.len() as i64),
        ("source primitive count", topology_view.primitive_kinds.len() as i64),
        ("source edge count", edge_count),
    ] {
        if count > i64::from(i32::MAX) {
            return Err(fail(format!("{name} exceeds integer limits")));
        }
    }
    let visible = |primitive: i64| match holes {
        None => true,
        Some(holes) => !group::mem(primitive, holes),
    };
    match scheme {
        Scheme::CatmullClark | Scheme::Bilinear => {
            let source_primitive_count = topology_view.primitive_kinds.len() as i64;
            let visible_offsets = match holes {
                None => None,
                Some(_) => {
                    let mut offsets = vec![0_i64; source_primitive_count as usize + 1];
                    for primitive in 0..source_primitive_count {
                        offsets[primitive as usize + 1] = if visible(primitive) {
                            checked_add(
                                "visible subdivision corner count",
                                offsets[primitive as usize],
                                primitive_size(topology_view, primitive),
                            )?
                        } else {
                            offsets[primitive as usize]
                        };
                    }
                    Some(offsets)
                }
            };
            let output_primitives = match &visible_offsets {
                None => topology_view.vertex_points.len() as i64,
                Some(offsets) => offsets[source_primitive_count as usize],
            };
            let output_vertices = checked_mul("output vertex count", output_primitives, 4)?;
            let vertices = topology_view.vertex_points.len() as i64;
            // Output primitives produced by the source corners before `vertex`.
            let output_before = |vertex: i64| match &visible_offsets {
                None => vertex,
                Some(offsets) if vertex >= vertices => offsets[source_primitive_count as usize],
                Some(offsets) => {
                    let primitive = gi(&index.primitive_of_vertex, vertex);
                    gi(offsets, primitive)
                        + if visible(primitive) {
                            vertex - gi(&topology_view.primitive_offsets, primitive)
                        } else {
                            0
                        }
                }
            };
            let mut vertex_points = vec![0_i64; output_vertices as usize];
            let primitive_offsets: Vec<i64> = (0..=output_primitives).map(|i| i * 4).collect();
            let primitive_kinds = vec![0u8; output_primitives as usize];
            let mut vertex_kind = vec![0u8; output_vertices as usize];
            let mut vertex_a = vec![0_i32; output_vertices as usize];
            let mut vertex_b = vec![-1_i32; output_vertices as usize];
            let mut vertex_primitive = vec![-1_i32; output_vertices as usize];
            let mut vertex_edge_source = vec![-1_i32; output_vertices as usize];
            let mut primitive_source = vec![0_i32; output_primitives as usize];
            let grain = grain_of(grain)?;
            let ranges = support::range_count(vertices, grain);
            let window = |range: i64, stride: i64| {
                (output_before(range.saturating_mul(grain).min(vertices)) * stride) as usize
            };
            let points_windows = Windows::new(&mut vertex_points, ranges, |range| window(range, 4));
            let kind_windows = Windows::new(&mut vertex_kind, ranges, |range| window(range, 4));
            let a_windows = Windows::new(&mut vertex_a, ranges, |range| window(range, 4));
            let b_windows = Windows::new(&mut vertex_b, ranges, |range| window(range, 4));
            let primitive_windows = Windows::new(&mut vertex_primitive, ranges, |range| window(range, 4));
            let edge_source_windows =
                Windows::new(&mut vertex_edge_source, ranges, |range| window(range, 4));
            let source_windows = Windows::new(&mut primitive_source, ranges, |range| window(range, 1));
            support::run_indexed_ranges(grain, vertices, &|range, first, last| {
                let points_out = points_windows.take(range);
                let kind_out = kind_windows.take(range);
                let a_out = a_windows.take(range);
                let b_out = b_windows.take(range);
                let primitive_out = primitive_windows.take(range);
                let edge_source_out = edge_source_windows.take(range);
                let source_out = source_windows.take(range);
                let base = output_before(first);
                for vertex in first..last {
                    support::poll(cancel_token, vertex)?;
                    let primitive = gi(&index.primitive_of_vertex, vertex);
                    let previous = gi(&index.previous_vertex, vertex);
                    let next = gi(&index.next_vertex, vertex);
                    if visible(primitive) {
                        let output_primitive = match &visible_offsets {
                            None => vertex,
                            Some(offsets) => {
                                gi(offsets, primitive) + vertex
                                    - gi(&topology_view.primitive_offsets, primitive)
                            }
                        };
                        let edge = gi(&index.edge_of_vertex, vertex);
                        let previous_edge = gi(&index.edge_of_vertex, previous);
                        let local = (output_primitive - base) as usize;
                        let at = local * 4;
                        points_out[at] = gi(&topology_view.vertex_points, vertex);
                        points_out[at + 1] = source_points + edge;
                        points_out[at + 2] = face_offset + primitive;
                        points_out[at + 3] = source_points + previous_edge;
                        kind_out[at] = 0;
                        a_out[at] = narrow(vertex);
                        edge_source_out[at] = narrow(edge);
                        kind_out[at + 1] = 1;
                        a_out[at + 1] = narrow(vertex);
                        b_out[at + 1] = narrow(next);
                        kind_out[at + 2] = 2;
                        primitive_out[at + 2] = narrow(primitive);
                        kind_out[at + 3] = 1;
                        a_out[at + 3] = narrow(previous);
                        b_out[at + 3] = narrow(vertex);
                        edge_source_out[at + 3] = narrow(previous_edge);
                        source_out[local] = narrow(primitive);
                    }
                }
                Ok(())
            })?;
            let point_count = checked_add(
                "output point count",
                face_offset,
                topology_view.primitive_kinds.len() as i64,
            )?;
            Ok(Built {
                vertex_kind,
                vertex_a,
                vertex_b,
                vertex_primitive,
                vertex_edge_source,
                primitive_source,
                topology: topology::View::create_validated_owned(
                    point_count,
                    vertex_points,
                    primitive_offsets,
                    primitive_kinds,
                ),
            })
        }
        Scheme::Loop => {
            let source_primitives = topology_view.primitive_kinds.len() as i64;
            let visible_offsets = match holes {
                None => None,
                Some(_) => {
                    let mut offsets = vec![0_i64; source_primitives as usize + 1];
                    for primitive in 0..source_primitives {
                        offsets[primitive as usize + 1] =
                            offsets[primitive as usize] + if visible(primitive) { 1 } else { 0 };
                    }
                    Some(offsets)
                }
            };
            let visible_primitives = match &visible_offsets {
                None => source_primitives,
                Some(offsets) => offsets[source_primitives as usize],
            };
            let output_primitives = checked_mul("output primitive count", visible_primitives, 4)?;
            let output_vertices = checked_mul("output vertex count", visible_primitives, 12)?;
            let mut vertex_points = vec![0_i64; output_vertices as usize];
            let primitive_offsets: Vec<i64> = (0..=output_primitives).map(|i| i * 3).collect();
            let primitive_kinds = vec![0u8; output_primitives as usize];
            let mut vertex_kind = vec![0u8; output_vertices as usize];
            let mut vertex_a = vec![0_i32; output_vertices as usize];
            let mut vertex_b = vec![-1_i32; output_vertices as usize];
            let mut vertex_primitive = vec![-1_i32; output_vertices as usize];
            let mut vertex_edge_source = vec![-1_i32; output_vertices as usize];
            let mut primitive_source = vec![0_i32; output_primitives as usize];
            // Output primitives produced by the source primitives before `primitive`.
            let output_before = |primitive: i64| match &visible_offsets {
                None => primitive,
                Some(offsets) => gi(offsets, primitive),
            };
            let grain = grain_of(grain)?;
            let ranges = support::range_count(source_primitives, grain);
            let window = |range: i64, stride: i64| {
                (output_before(range.saturating_mul(grain).min(source_primitives)) * stride) as usize
            };
            let points_windows = Windows::new(&mut vertex_points, ranges, |range| window(range, 12));
            let kind_windows = Windows::new(&mut vertex_kind, ranges, |range| window(range, 12));
            let a_windows = Windows::new(&mut vertex_a, ranges, |range| window(range, 12));
            let b_windows = Windows::new(&mut vertex_b, ranges, |range| window(range, 12));
            let primitive_windows =
                Windows::new(&mut vertex_primitive, ranges, |range| window(range, 12));
            let edge_source_windows =
                Windows::new(&mut vertex_edge_source, ranges, |range| window(range, 12));
            let source_windows = Windows::new(&mut primitive_source, ranges, |range| window(range, 4));
            support::run_indexed_ranges(grain, source_primitives, &|range, first, last| {
                let points_out = points_windows.take(range);
                let kind_out = kind_windows.take(range);
                let a_out = a_windows.take(range);
                let b_out = b_windows.take(range);
                let primitive_out = primitive_windows.take(range);
                let edge_source_out = edge_source_windows.take(range);
                let source_out = source_windows.take(range);
                let base = output_before(first);
                for primitive in first..last {
                    support::poll(cancel_token, primitive)?;
                    if visible(primitive) {
                        let local = output_before(primitive) - base;
                        let first = gi(&topology_view.primitive_offsets, primitive);
                        let va = first;
                        let vb = first + 1;
                        let vc = first + 2;
                        let a = gi(&topology_view.vertex_points, va);
                        let b = gi(&topology_view.vertex_points, vb);
                        let c = gi(&topology_view.vertex_points, vc);
                        let ea = gi(&index.edge_of_vertex, va);
                        let eb = gi(&index.edge_of_vertex, vb);
                        let ec = gi(&index.edge_of_vertex, vc);
                        let ab = source_points + ea;
                        let bc = source_points + eb;
                        let ca = source_points + ec;
                        let at = (local * 12) as usize;
                        let mut set = |vertex: usize, point: i64, kind: u8, a: i64, b: i64| {
                            points_out[vertex] = point;
                            kind_out[vertex] = kind;
                            a_out[vertex] = narrow(a);
                            b_out[vertex] = narrow(b);
                            primitive_out[vertex] = narrow(primitive);
                        };
                        set(at, a, 0, va, -1);
                        edge_source_out[at] = narrow(ea);
                        set(at + 1, ab, 1, va, vb);
                        set(at + 2, ca, 1, vc, va);
                        edge_source_out[at + 2] = narrow(ec);
                        set(at + 3, b, 0, vb, -1);
                        edge_source_out[at + 3] = narrow(eb);
                        set(at + 4, bc, 1, vb, vc);
                        set(at + 5, ab, 1, va, vb);
                        edge_source_out[at + 5] = narrow(ea);
                        set(at + 6, c, 0, vc, -1);
                        edge_source_out[at + 6] = narrow(ec);
                        set(at + 7, ca, 1, vc, va);
                        set(at + 8, bc, 1, vb, vc);
                        edge_source_out[at + 8] = narrow(eb);
                        set(at + 9, ab, 1, va, vb);
                        set(at + 10, bc, 1, vb, vc);
                        set(at + 11, ca, 1, vc, va);
                        source_out[local as usize * 4..][..4].fill(narrow(primitive));
                    }
                }
                Ok(())
            })?;
            Ok(Built {
                vertex_kind,
                vertex_a,
                vertex_b,
                vertex_primitive,
                vertex_edge_source,
                primitive_source,
                topology: topology::View::create_validated_owned(
                    source_points + edge_count,
                    vertex_points,
                    primitive_offsets,
                    primitive_kinds,
                ),
            })
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn make_plan(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    edge_ancestry_attribute: Option<String>,
    edge_crease_override: Option<f64>,
    holes: Option<&group::T>,
    boundary_interpolation: BoundaryInterpolation,
    triangle_subdivision: TrianglePolicy,
    creasing_method: CreasingMethod,
    scheme: Scheme,
    source: &geometry::T,
) -> Result<Plan, error::T> {
    let topology_view = topology::View::view(geometry::topology(source))?;
    let topology_index_value = topology_index::create(cancel_token, geometry::topology(source))?;
    let index = topology_index::View::view(&topology_index_value);
    validate(cancel_token, scheme, source, &topology_view, &index)?;
    let source_points = topology_view.point_count;
    let source_vertices = topology_view.vertex_points.len() as i64;
    let source_primitives = topology_view.primitive_kinds.len() as i64;
    let edge_count = index.edge_a.len() as i64;
    let face_offset = checked_add("output point count", source_points, edge_count)?;
    let output_points = match scheme {
        Scheme::Loop => face_offset,
        Scheme::CatmullClark | Scheme::Bilinear => {
            checked_add("output point count", face_offset, source_primitives)?
        }
    };
    let mut point_offsets = vec![0_i64; output_points as usize + 1];
    for output in 0..output_points {
        point_offsets[output as usize + 1] = checked_add(
            "subdivision stencil storage",
            point_offsets[output as usize],
            point_term_count(
                boundary_interpolation,
                scheme,
                &topology_view,
                &index,
                source_points,
                edge_count,
                output,
            )?,
        )?;
    }
    let (point_sources, point_weights, point_representative) = build_stencils(
        cancel_token,
        grain,
        boundary_interpolation,
        triangle_subdivision,
        scheme,
        &topology_view,
        &index,
        source_points,
        edge_count,
        &point_offsets,
    )?;
    let creases = match crease_data(edge_crease_override, source, &topology_view, &index)? {
        None => None,
        Some((edge_sharpness, corner_sharpness, has_edge_creases, _has_corner_creases)) => {
            let child_edge_sharpness = build_child_edge_sharpness(
                cancel_token,
                grain,
                creasing_method,
                &index,
                source_points,
                &edge_sharpness,
            )?;
            let vertex_masks = build_vertex_sharp_plan(
                cancel_token,
                grain,
                creasing_method,
                &index,
                &edge_sharpness,
                &child_edge_sharpness,
                &corner_sharpness,
            )?;
            Some(CreasePlan {
                method_: creasing_method,
                edge_sharpness,
                child_edge_sharpness,
                has_edge_creases,
                vertex_masks,
            })
        }
    };
    let built = make_topology(
        cancel_token,
        grain,
        holes,
        scheme,
        &topology_view,
        &index,
        source_points,
        edge_count,
        face_offset,
    )?;
    Ok(Plan {
        scheme,
        source: source.clone(),
        source_topology: topology_view,
        index,
        source_points,
        source_vertices,
        source_primitives,
        edge_count,
        face_offset,
        output_points,
        point_offsets,
        point_sources,
        point_weights,
        point_representative,
        edge_ancestry_attribute,
        boundary_interpolation,
        triangle_subdivision,
        creasing_method,
        creases,
        vertex_kind: built.vertex_kind,
        vertex_a: built.vertex_a,
        vertex_b: built.vertex_b,
        vertex_primitive: built.vertex_primitive,
        vertex_edge_source: built.vertex_edge_source,
        primitive_source: built.primitive_source,
        topology: built.topology,
    })
}

pub fn creased_vertex_value(creases: &CreasePlan, source: &[f64], point: i64, smooth: f64) -> f64 {
    let masks = &creases.vertex_masks;
    let value = |kind: u8, a: i64, b: i64| match kind {
        2 => gf(source, point),
        1 => (gf(source, point) * 0.75) + ((gf(source, a) + gf(source, b)) * 0.125),
        _ => smooth,
    };
    let parent_kind = gu(&masks.parent_kind, point);
    let parent = value(parent_kind, gi(&masks.parent_a, point), gi(&masks.parent_b, point));
    let child_kind = gu(&masks.child_kind, point);
    if parent_kind == child_kind {
        parent
    } else {
        let child = value(child_kind, gi(&masks.child_a, point), gi(&masks.child_b, point));
        let factor = gf(&masks.transition_factor, point);
        child + ((parent - child) * factor)
    }
}

pub fn creased_edge_factor(creases: &CreasePlan, edge: i64) -> f64 {
    let parent = gf(&creases.edge_sharpness, edge);
    if creases.method_ == SubdivideCreasingChaikin
        && parent > 0.0
        && parent < 1.0
        && gf(&creases.child_edge_sharpness, edge * 2) > 0.0
        && gf(&creases.child_edge_sharpness, edge * 2 + 1) > 0.0
    {
        1.0
    } else {
        clamp_sharpness(parent)
    }
}

pub fn point_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    filled(cancel_token, grain, plan.output_points, &|point| {
        let mut sum = 0.0;
        for at in gi(&plan.point_offsets, point)..gi(&plan.point_offsets, point + 1) {
            sum += gf(source, gi(&plan.point_sources, at)) * gf(&plan.point_weights, at);
        }
        let value = sum;
        Ok(if plan.scheme == Scheme::Bilinear {
            value
        } else if point < plan.source_points {
            match &plan.creases {
                None => value,
                Some(creases) => creased_vertex_value(creases, source, point, value),
            }
        } else if point < plan.source_points + plan.edge_count {
            match &plan.creases {
                None => value,
                Some(creases) if !creases.has_edge_creases => value,
                Some(creases) => {
                    let edge = point - plan.source_points;
                    let factor = creased_edge_factor(creases, edge);
                    if factor == 0.0 {
                        value
                    } else {
                        let midpoint = (gf(source, gi(&plan.index.edge_a, edge))
                            + gf(source, gi(&plan.index.edge_b, edge)))
                            * 0.5;
                        value + ((midpoint - value) * factor)
                    }
                }
            }
        } else {
            value
        })
    })
}

#[derive(Clone)]
pub struct FvarPointPlan {
    pub fvar_scheme: Scheme,
    pub fvar_source_points: i64,
    pub fvar_edge_count: i64,
    pub fvar_output_points: i64,
    pub fvar_point_offsets: Vec<i64>,
    pub fvar_point_sources: Vec<i64>,
    pub fvar_point_weights: Vec<f64>,
    pub fvar_index: topology_index::View::View,
    pub fvar_creases: Option<CreasePlan>,
}

#[derive(Clone)]
pub enum FvarRefinement {
    LinearFvar(Plan),
    IsolatedFvar {
        geometry_refinement: Plan,
        corner_sharpness: Vec<f64>,
    },
    ContinuousFvar {
        geometry_refinement: Plan,
        source_vertex_of_point: Vec<i64>,
        pinned_points: Vec<u8>,
        output_point_of_vertex: Vec<i64>,
    },
    SplitFvar {
        refinement: FvarPointPlan,
        source_vertex_of_point: Vec<i64>,
        output_point_of_vertex: Vec<i64>,
    },
}

/// Union-find `find` with path compression.
pub fn find_root(parents: &mut [i64], value: i64) -> i64 {
    let mut root = value;
    while parents[root as usize] != root {
        root = parents[root as usize];
    }
    let mut current = value;
    while parents[current as usize] != root {
        let next = parents[current as usize];
        parents[current as usize] = root;
        current = next;
    }
    root
}

/// Union by rank.
pub fn union_roots(parents: &mut [i64], ranks: &mut [i64], left: i64, right: i64) {
    let left = find_root(parents, left);
    let right = find_root(parents, right);
    if left != right {
        if ranks[left as usize] < ranks[right as usize] {
            parents[left as usize] = right;
        } else {
            parents[right as usize] = left;
            if ranks[left as usize] == ranks[right as usize] {
                ranks[left as usize] += 1;
            }
        }
    }
}

/// OCaml `Float.equal`: NaN equals itself, zeros of either sign are equal.
pub fn float_equal(a: f64, b: f64) -> bool {
    a == b || (a.is_nan() && b.is_nan())
}

// The face-varying refinement walks union-find sets, so it runs sequentially (the OCaml runs
// disjoint per-point sets concurrently); the result is identical.
pub fn fvar_refinement(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    mode: FaceVaryingInterpolation,
    plan: &Plan,
    equal: &dyn Fn(i64, i64) -> bool,
) -> Result<FvarRefinement, error::T> {
    let topology_view = &plan.source_topology;
    let index = &plan.index;
    let vertex_count = plan.source_vertices;
    let source_points = plan.source_points;
    let mut continuous_source = vec![-1_i64; source_points as usize];
    let mut discontinuous_points = vec![0u8; source_points as usize];
    for point in 0..source_points {
        cancel::poll(cancel_token, point, 4095)?;
        let first = gi(&index.point_offsets, point);
        let last = gi(&index.point_offsets, point + 1);
        if first < last {
            let reference = gi(&index.point_vertices, first);
            continuous_source[point as usize] = reference;
            let mut at = first + 1;
            while at < last && discontinuous_points[point as usize] == 0 {
                if !equal(reference, gi(&index.point_vertices, at)) {
                    discontinuous_points[point as usize] = 1;
                }
                at += 1;
            }
        }
    }
    if !discontinuous_points.iter().any(|&value| value != 0) {
        let mut pinned_points = vec![0u8; source_points as usize];
        for point in 0..source_points {
            cancel::poll(cancel_token, point, 4095)?;
            let faces = gi(&index.point_offsets, point + 1) - gi(&index.point_offsets, point);
            let (_, _, boundary_edges) = boundary_neighbors(index, point);
            let pinned = match mode {
                SubdivideFvarNone => false,
                SubdivideFvarCornersOnly | SubdivideFvarCornersPlus1 | SubdivideFvarCornersPlus2 => {
                    faces == 1
                }
                SubdivideFvarBoundaries => boundary_edges > 0,
                SubdivideFvarAll => {
                    return Err(failm("face-varying refinement requires a non-All mode"));
                }
            };
            if pinned {
                pinned_points[point as usize] = 1;
            }
        }
        return Ok(FvarRefinement::ContinuousFvar {
            geometry_refinement: plan.clone(),
            source_vertex_of_point: continuous_source,
            pinned_points,
            output_point_of_vertex: topology::View::view(&plan.topology)?.vertex_points.to_vec(),
        });
    }
    let mut parents: Vec<i64> = (0..vertex_count).collect();
    let mut ranks = vec![0_i64; vertex_count as usize];
    let corner_at_point = |directed: i64, point: i64| -> Result<i64, error::T> {
        if gi(&topology_view.vertex_points, directed) == point {
            Ok(directed)
        } else {
            let next = gi(&index.next_vertex, directed);
            if next < 0 || gi(&topology_view.vertex_points, next) != point {
                return Err(failm("face-varying edge incidence is inconsistent"));
            }
            Ok(next)
        }
    };
    for point in 0..source_points {
        cancel::poll(cancel_token, point, 4095)?;
        for at in gi(&index.point_edge_offsets, point)..gi(&index.point_edge_offsets, point + 1) {
            let edge = gi(&index.point_edges, at);
            let first = gi(&index.edge_offsets, edge);
            let last = gi(&index.edge_offsets, edge + 1);
            if last - first == 2 {
                let left = corner_at_point(gi(&index.edge_vertices, first), point)?;
                let right = corner_at_point(gi(&index.edge_vertices, first + 1), point)?;
                if equal(left, right) {
                    union_roots(&mut parents, &mut ranks, left, right);
                }
            }
        }
    }
    let mut root_seen = vec![0u8; vertex_count as usize];
    let mut component_counts = vec![0_i64; source_points as usize];
    for point in 0..source_points {
        cancel::poll(cancel_token, point, 4095)?;
        let mut count = 0_i64;
        for at in gi(&index.point_offsets, point)..gi(&index.point_offsets, point + 1) {
            let root = find_root(&mut parents, gi(&index.point_vertices, at));
            if root_seen[root as usize] == 0 {
                root_seen[root as usize] = 1;
                count += 1;
            }
        }
        component_counts[point as usize] = count;
    }
    let mut component_offsets = vec![0_i64; source_points as usize + 1];
    for point in 0..source_points as usize {
        component_offsets[point + 1] = checked_add(
            "face-varying value count",
            component_offsets[point],
            component_counts[point],
        )?;
    }
    let fvar_point_count = component_offsets[source_points as usize];
    let mut root_target = vec![-1_i64; vertex_count as usize];
    let mut source_vertex_of_point = vec![0_i64; fvar_point_count as usize];
    let mut original_point_of_fvar = vec![0_i64; fvar_point_count as usize];
    for point in 0..source_points {
        cancel::poll(cancel_token, point, 4095)?;
        let mut next = component_offsets[point as usize];
        for at in gi(&index.point_offsets, point)..gi(&index.point_offsets, point + 1) {
            let vertex = gi(&index.point_vertices, at);
            let root = find_root(&mut parents, vertex);
            if root_target[root as usize] < 0 {
                root_target[root as usize] = next;
                source_vertex_of_point[next as usize] = vertex;
                original_point_of_fvar[next as usize] = point;
                next += 1;
            }
        }
        if next != component_offsets[point as usize + 1] {
            return Err(failm("face-varying component cardinality mismatch"));
        }
    }
    let mut fvar_vertex_points = vec![0_i64; vertex_count as usize];
    for vertex in 0..vertex_count {
        cancel::poll(cancel_token, vertex, 4095)?;
        let root = find_root(&mut parents, vertex);
        let target = root_target[root as usize];
        if target < 0 {
            return Err(failm("face-varying corner has no value component"));
        }
        fvar_vertex_points[vertex as usize] = target;
    }
    let mut component_faces = vec![0_i64; fvar_point_count as usize];
    for point in 0..source_points {
        for at in gi(&index.point_offsets, point)..gi(&index.point_offsets, point + 1) {
            let component = gi(&fvar_vertex_points, gi(&index.point_vertices, at));
            component_faces[component as usize] += 1;
        }
    }
    let make_isolated = |corner_weights: &[f64]| -> FvarRefinement {
        let corner_sharpness: Vec<f64> = (0..vertex_count)
            .map(|vertex| gf(corner_weights, gi(&fvar_vertex_points, vertex)))
            .collect();
        FvarRefinement::IsolatedFvar { geometry_refinement: plan.clone(), corner_sharpness }
    };
    if component_faces.iter().all(|&faces| faces == 1) {
        let mut corner_weights = vec![0.0_f64; fvar_point_count as usize];
        if let Some(source) = float_attribute(Owner::Point, "cornerweight", &plan.source)? {
            for component in 0..fvar_point_count as usize {
                corner_weights[component] = gf(&source, original_point_of_fvar[component]);
            }
        }
        for component in 0..fvar_point_count as usize {
            let point = original_point_of_fvar[component];
            let source_faces = gi(&index.point_offsets, point + 1) - gi(&index.point_offsets, point);
            if mode != SubdivideFvarNone
                || (plan.boundary_interpolation == SubdivideBoundaryEdgeAndCorner
                    && source_faces == 1)
            {
                corner_weights[component] = f64::MAX;
            }
        }
        if corner_weights.iter().all(|&weight| weight >= 1.0) {
            return Ok(FvarRefinement::LinearFvar(plan.clone()));
        }
        return Ok(make_isolated(&corner_weights));
    }
    let fvar_topology = topology::View::create_validated_owned(
        fvar_point_count,
        fvar_vertex_points.clone(),
        topology_view.primitive_offsets.to_vec(),
        topology_view.primitive_kinds.to_vec(),
    );
    let fvar_index_value = topology_index::create(cancel_token, &fvar_topology)?;
    let fvar_index = topology_index::View::view(&fvar_index_value);
    let mut corner_weights = vec![0.0_f64; fvar_point_count as usize];
    if let Some(source) = float_attribute(Owner::Point, "cornerweight", &plan.source)? {
        for point in 0..fvar_point_count as usize {
            corner_weights[point] = gf(&source, original_point_of_fvar[point]);
        }
    }
    let mut component_boundary_edges = vec![0_i64; fvar_point_count as usize];
    for point in 0..fvar_point_count {
        cancel::poll(cancel_token, point, 4095)?;
        let mut count = 0_i64;
        for at in gi(&fvar_index.point_edge_offsets, point)..gi(&fvar_index.point_edge_offsets, point + 1)
        {
            let edge = gi(&fvar_index.point_edges, at);
            if gi(&fvar_index.edge_offsets, edge + 1) - gi(&fvar_index.edge_offsets, edge) == 1 {
                count += 1;
            }
        }
        component_boundary_edges[point as usize] = count;
    }
    let mut point_has_corner_component = vec![0u8; source_points as usize];
    for point in 0..source_points {
        for component in component_offsets[point as usize]..component_offsets[point as usize + 1] {
            if component_faces[component as usize] == 1 {
                point_has_corner_component[point as usize] = 1;
            }
        }
    }
    for fvar_point in 0..fvar_point_count {
        cancel::poll(cancel_token, fvar_point, 4095)?;
        let point = original_point_of_fvar[fvar_point as usize];
        let source_faces = gi(&index.point_offsets, point + 1) - gi(&index.point_offsets, point);
        let components = component_counts[point as usize];
        let (_, _, source_boundary_edges) = boundary_neighbors(index, point);
        let faces = component_faces[fvar_point as usize];
        let boundary_edges = component_boundary_edges[fvar_point as usize];
        let policy_pin = match mode {
            SubdivideFvarNone => false,
            SubdivideFvarCornersOnly => faces == 1,
            SubdivideFvarCornersPlus1 => faces == 1 || components > 2,
            SubdivideFvarCornersPlus2 => {
                faces == 1
                    || components > 2
                    || (components == 1 && boundary_edges > 0 && source_boundary_edges == 0)
                    || (components == 2 && point_has_corner_component[point as usize] != 0)
            }
            SubdivideFvarBoundaries => boundary_edges > 0,
            SubdivideFvarAll => {
                return Err(failm("face-varying refinement requires a non-All mode"));
            }
        };
        let geometry_corner =
            plan.boundary_interpolation == SubdivideBoundaryEdgeAndCorner && source_faces == 1;
        if policy_pin || geometry_corner {
            corner_weights[fvar_point as usize] = f64::MAX;
        }
    }
    // Plus modes inherit semi-sharpness from the other of two regions when exactly one region
    // contains a sharp interior edge. Seam and geometric boundary edges are deliberately excluded.
    if let (SubdivideFvarCornersPlus1 | SubdivideFvarCornersPlus2, Some(creases)) =
        (mode, &plan.creases)
    {
        if creases.has_edge_creases {
            let mut internal_sharpness = vec![0.0_f64; fvar_point_count as usize];
            for point in 0..fvar_point_count {
                cancel::poll(cancel_token, point, 4095)?;
                let mut sharpness = 0.0_f64;
                for at in gi(&fvar_index.point_edge_offsets, point)
                    ..gi(&fvar_index.point_edge_offsets, point + 1)
                {
                    let edge = gi(&fvar_index.point_edges, at);
                    if gi(&fvar_index.edge_offsets, edge + 1) - gi(&fvar_index.edge_offsets, edge)
                        == 2
                    {
                        let vertex =
                            gi(&fvar_index.edge_vertices, gi(&fvar_index.edge_offsets, edge));
                        let source_edge = gi(&index.edge_of_vertex, vertex);
                        if source_edge >= 0 {
                            sharpness = fmax(sharpness, gf(&creases.edge_sharpness, source_edge));
                        }
                    }
                }
                internal_sharpness[point as usize] = sharpness;
            }
            for point in 0..source_points {
                if component_counts[point as usize] == 2 {
                    let left = component_offsets[point as usize] as usize;
                    let right = left + 1;
                    if internal_sharpness[left] == 0.0
                        && internal_sharpness[right] > 0.0
                        && component_faces[right] > 1
                    {
                        corner_weights[left] = fmax(corner_weights[left], internal_sharpness[right]);
                    }
                    if internal_sharpness[right] == 0.0
                        && internal_sharpness[left] > 0.0
                        && component_faces[left] > 1
                    {
                        corner_weights[right] = fmax(corner_weights[right], internal_sharpness[left]);
                    }
                }
            }
        }
    }
    let every_component_pinned = corner_weights.iter().all(|&weight| weight >= 1.0);
    let every_component_is_one_face = component_faces.iter().all(|&faces| faces == 1);
    let mut every_edge_is_boundary = true;
    for edge in 0..fvar_index.edge_a.len() as i64 {
        if gi(&fvar_index.edge_offsets, edge + 1) - gi(&fvar_index.edge_offsets, edge) != 1 {
            every_edge_is_boundary = false;
        }
    }
    if every_component_pinned && every_edge_is_boundary {
        return Ok(FvarRefinement::LinearFvar(plan.clone()));
    }
    if every_component_is_one_face && every_edge_is_boundary {
        return Ok(make_isolated(&corner_weights));
    }
    let mut attributes: Vec<attribute::T> = Vec::new();
    if let Some(creases) = &plan.creases {
        if creases.has_edge_creases {
            let mut weights = vec![0.0_f64; vertex_count as usize];
            for vertex in 0..vertex_count {
                let edge = gi(&index.edge_of_vertex, vertex);
                if edge >= 0 {
                    weights[vertex as usize] = gf(&creases.edge_sharpness, edge);
                }
            }
            attributes.insert(
                0,
                attribute::create_owned("creaseweight", Owner::Vertex, Storage::Float(Arc::new(weights)))?,
            );
        }
    }
    if corner_weights.iter().any(|&weight| weight > 0.0) {
        attributes.insert(
            0,
            attribute::create_owned(
                "cornerweight",
                Owner::Point,
                Storage::Float(Arc::new(corner_weights.clone())),
            )?,
        );
    }
    let zeros = vec![0.0_f64; fvar_point_count as usize];
    let positions = packed::Float3::View::of_owned_exn(zeros.clone(), zeros.clone(), zeros)?;
    let fvar_geometry =
        geometry::create(positions, fvar_topology.clone(), Some(attributes), None, None)?;
    let fvar_topology_view = topology::View::view(&fvar_topology)?;
    let edge_count = fvar_index.edge_a.len() as i64;
    let face_offset = checked_add("face-varying output point count", fvar_point_count, edge_count)?;
    let output_points = match plan.scheme {
        Scheme::Loop => face_offset,
        Scheme::CatmullClark | Scheme::Bilinear => checked_add(
            "face-varying output point count",
            face_offset,
            plan.source_primitives,
        )?,
    };
    let mut point_offsets = vec![0_i64; output_points as usize + 1];
    for output in 0..output_points {
        point_offsets[output as usize + 1] = checked_add(
            "face-varying stencil storage",
            point_offsets[output as usize],
            point_term_count(
                SubdivideBoundaryEdgeOnly,
                plan.scheme,
                &fvar_topology_view,
                &fvar_index,
                fvar_point_count,
                edge_count,
                output,
            )?,
        )?;
    }
    let (point_sources, point_weights, _point_representatives) = build_stencils(
        cancel_token,
        grain,
        SubdivideBoundaryEdgeOnly,
        plan.triangle_subdivision,
        plan.scheme,
        &fvar_topology_view,
        &fvar_index,
        fvar_point_count,
        edge_count,
        &point_offsets,
    )?;
    let creases = match crease_data(None, &fvar_geometry, &fvar_topology_view, &fvar_index)? {
        None => None,
        Some((edge_sharpness, corner_sharpness, has_edge_creases, _has_corner_creases)) => {
            let child_edge_sharpness = build_child_edge_sharpness(
                cancel_token,
                grain,
                plan.creasing_method,
                &fvar_index,
                fvar_point_count,
                &edge_sharpness,
            )?;
            let vertex_masks = build_vertex_sharp_plan(
                cancel_token,
                grain,
                plan.creasing_method,
                &fvar_index,
                &edge_sharpness,
                &child_edge_sharpness,
                &corner_sharpness,
            )?;
            Some(CreasePlan {
                method_: plan.creasing_method,
                edge_sharpness,
                child_edge_sharpness,
                has_edge_creases,
                vertex_masks,
            })
        }
    };
    let output_count = plan.vertex_kind.len() as i64;
    let mut output_point_of_vertex = vec![0_i64; output_count as usize];
    for output in 0..output_count {
        cancel::poll(cancel_token, output, 4095)?;
        output_point_of_vertex[output as usize] = match gu(&plan.vertex_kind, output) {
            0 => gi(&fvar_vertex_points, gn(&plan.vertex_a, output)),
            1 => {
                let edge = gi(&fvar_index.edge_of_vertex, gn(&plan.vertex_a, output));
                if edge < 0 {
                    return Err(failm("face-varying output edge has no source edge"));
                }
                fvar_point_count + edge
            }
            _ => face_offset + gn(&plan.vertex_primitive, output),
        };
    }
    let refinement = FvarPointPlan {
        fvar_scheme: plan.scheme,
        fvar_source_points: fvar_point_count,
        fvar_edge_count: edge_count,
        fvar_output_points: output_points,
        fvar_point_offsets: point_offsets.clone(),
        fvar_point_sources: point_sources,
        fvar_point_weights: point_weights,
        fvar_index,
        fvar_creases: creases,
    };
    Ok(FvarRefinement::SplitFvar { refinement, source_vertex_of_point, output_point_of_vertex })
}

pub fn fvar_point_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &FvarPointPlan,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    filled(cancel_token, grain, plan.fvar_output_points, &|point| {
        let mut sum = 0.0;
        for at in gi(&plan.fvar_point_offsets, point)..gi(&plan.fvar_point_offsets, point + 1) {
            sum += gf(source, gi(&plan.fvar_point_sources, at)) * gf(&plan.fvar_point_weights, at);
        }
        let value = sum;
        Ok(if plan.fvar_scheme == Scheme::Bilinear {
            value
        } else if point < plan.fvar_source_points {
            match &plan.fvar_creases {
                None => value,
                Some(creases) => creased_vertex_value(creases, source, point, value),
            }
        } else if point < plan.fvar_source_points + plan.fvar_edge_count {
            match &plan.fvar_creases {
                None => value,
                Some(creases) if !creases.has_edge_creases => value,
                Some(creases) => {
                    let edge = point - plan.fvar_source_points;
                    let factor = creased_edge_factor(creases, edge);
                    if factor == 0.0 {
                        value
                    } else {
                        let midpoint = (gf(source, gi(&plan.fvar_index.edge_a, edge))
                            + gf(source, gi(&plan.fvar_index.edge_b, edge)))
                            * 0.5;
                        value + ((midpoint - value) * factor)
                    }
                }
            }
        } else {
            value
        })
    })
}

pub fn linear_vertex_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    let count = plan.vertex_kind.len() as i64;
    filled(cancel_token, grain, count, &|vertex| {
        Ok(match gu(&plan.vertex_kind, vertex) {
            0 => gf(source, gn(&plan.vertex_a, vertex)),
            1 => {
                (gf(source, gn(&plan.vertex_a, vertex)) + gf(source, gn(&plan.vertex_b, vertex)))
                    * 0.5
            }
            _ => {
                let primitive = gn(&plan.vertex_primitive, vertex);
                let first = gi(&plan.source_topology.primitive_offsets, primitive);
                let last = gi(&plan.source_topology.primitive_offsets, primitive + 1);
                let mut sum = 0.0;
                for corner in first..last {
                    sum += gf(source, corner);
                }
                sum / ((last - first) as f64)
            }
        })
    })
}

pub fn isolated_vertex_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    corner_sharpness: &[f64],
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    let count = plan.vertex_kind.len() as i64;
    filled(cancel_token, grain, count, &|output_vertex| {
        Ok(match gu(&plan.vertex_kind, output_vertex) {
            0 => {
                let vertex = gn(&plan.vertex_a, output_vertex);
                let previous = gi(&plan.index.previous_vertex, vertex);
                let next = gi(&plan.index.next_vertex, vertex);
                let smooth = (0.75 * gf(source, vertex))
                    + (0.125 * gf(source, previous))
                    + (0.125 * gf(source, next));
                let factor = clamp_sharpness(gf(corner_sharpness, vertex));
                smooth + ((gf(source, vertex) - smooth) * factor)
            }
            1 => {
                (gf(source, gn(&plan.vertex_a, output_vertex))
                    + gf(source, gn(&plan.vertex_b, output_vertex)))
                    * 0.5
            }
            _ => {
                let primitive = gn(&plan.vertex_primitive, output_vertex);
                let first = gi(&plan.source_topology.primitive_offsets, primitive);
                let last = gi(&plan.source_topology.primitive_offsets, primitive + 1);
                let mut sum = 0.0;
                for corner in first..last {
                    sum += gf(source, corner);
                }
                sum / ((last - first) as f64)
            }
        })
    })
}

pub fn fvar_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    fvar: &FvarRefinement,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    match fvar {
        FvarRefinement::LinearFvar(plan) => linear_vertex_float(cancel_token, grain, plan, source),
        FvarRefinement::IsolatedFvar { geometry_refinement, corner_sharpness } => {
            isolated_vertex_float(cancel_token, grain, geometry_refinement, corner_sharpness, source)
        }
        FvarRefinement::ContinuousFvar {
            geometry_refinement,
            source_vertex_of_point,
            pinned_points,
            output_point_of_vertex,
        } => {
            let source_values: Vec<f64> = source_vertex_of_point
                .iter()
                .map(|&vertex| if vertex < 0 { 0.0 } else { gf(source, vertex) })
                .collect();
            let mut refined = point_float(cancel_token, grain, geometry_refinement, &source_values)?;
            for point in 0..source_values.len() {
                if pinned_points[point] != 0 {
                    refined[point] = source_values[point];
                }
            }
            Ok(output_point_of_vertex.iter().map(|&point| gf(&refined, point)).collect())
        }
        FvarRefinement::SplitFvar { refinement, source_vertex_of_point, output_point_of_vertex } => {
            let source_values: Vec<f64> =
                source_vertex_of_point.iter().map(|&vertex| gf(source, vertex)).collect();
            let refined = fvar_point_float(cancel_token, grain, refinement, &source_values)?;
            Ok(output_point_of_vertex.iter().map(|&point| gf(&refined, point)).collect())
        }
    }
}

pub fn vertex_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    linear_vertex_float(cancel_token, grain, plan, source)
}

pub fn primitive_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    let count = plan.primitive_source.len() as i64;
    filled(cancel_token, grain, count, &|primitive| {
        Ok(gf(source, gn(&plan.primitive_source, primitive)))
    })
}

/// For each output element of `owner`, the source element a discrete attribute copies.
pub fn discrete_mapping(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    owner: Owner,
) -> Result<Vec<i64>, error::T> {
    let count = match owner {
        Owner::Point => plan.output_points,
        Owner::Vertex => plan.vertex_kind.len() as i64,
        Owner::Primitive => plan.primitive_source.len() as i64,
        Owner::Detail => 1,
    };
    filled(cancel_token, grain, count, &|index| {
        Ok(match owner {
            Owner::Point => gi(&plan.point_representative, index),
            Owner::Vertex => match gu(&plan.vertex_kind, index) {
                0 => gn(&plan.vertex_a, index),
                1 => gn(&plan.vertex_a, index).min(gn(&plan.vertex_b, index)),
                _ => gi(&plan.source_topology.primitive_offsets, gn(&plan.vertex_primitive, index)),
            },
            Owner::Primitive => gn(&plan.primitive_source, index),
            Owner::Detail => 0,
        })
    })
}

fn discrete_map_ints(mapping: &[i64], source: &[i64]) -> Result<Vec<i64>, error::T> {
    if source.is_empty() {
        return Err(invalid_arg("index out of bounds".to_owned()));
    }
    Ok(mapping.iter().map(|&at| gi(source, at)).collect())
}

fn discrete_map_text(mapping: &[i64], source: &[Vec<u8>]) -> Result<Vec<Vec<u8>>, error::T> {
    if source.is_empty() {
        return Err(invalid_arg("index out of bounds".to_owned()));
    }
    Ok(mapping.iter().map(|&at| source[at as usize].clone()).collect())
}

pub fn vertex_edge_discrete_map(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    source: &[i64],
    default: i64,
) -> Result<Vec<i64>, error::T> {
    let count = plan.vertex_kind.len() as i64;
    filled(cancel_token, grain, count, &|vertex| {
        let edge = gn(&plan.vertex_edge_source, vertex);
        Ok(if edge >= 0 {
            let directed = gi(&plan.index.edge_vertices, gi(&plan.index.edge_offsets, edge));
            gi(source, directed)
        } else {
            default
        })
    })
}

pub fn numeric_map(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    owner: Owner,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    match owner {
        Owner::Point => point_float(cancel_token, grain, plan, source),
        Owner::Vertex => vertex_float(cancel_token, grain, plan, source),
        Owner::Primitive => primitive_float(cancel_token, grain, plan, source),
        Owner::Detail => Ok(source.to_vec()),
    }
}

fn fvar_enabled(
    owner: Owner,
    face_varying_interpolation: FaceVaryingInterpolation,
    plan: &Plan,
) -> bool {
    owner == Owner::Vertex
        && face_varying_interpolation != SubdivideFvarAll
        && plan.scheme != Scheme::Bilinear
}

#[allow(clippy::too_many_arguments)]
fn map_planes(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    owner: Owner,
    fvar: &Option<FvarRefinement>,
    planes: &[&[f64]],
) -> Result<Vec<Vec<f64>>, error::T> {
    let mut out = Vec::with_capacity(planes.len());
    for values in planes {
        out.push(match fvar {
            None => numeric_map(cancel_token, grain, plan, owner, values)?,
            Some(fvar) => fvar_float(cancel_token, grain, fvar, values)?,
        });
    }
    Ok(out)
}

pub fn remap_attribute(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    generate_resulting_creases: bool,
    face_varying_interpolation: FaceVaryingInterpolation,
    plan: &Plan,
    attribute_value: &attribute::T,
) -> Result<Option<attribute::T>, error::T> {
    let owner = attribute::owner(attribute_value);
    let name = attribute::name(attribute_value);
    if owner == Owner::Vertex && plan.edge_ancestry_attribute.as_deref() == Some(name) {
        let values = match attribute::View::storage(attribute_value)? {
            Storage::Int(values) => values,
            _ => return Err(failm("internal edge ancestry must use integer storage")),
        };
        return Ok(Some(attribute::create_owned(
            name,
            Owner::Vertex,
            Storage::Int(Arc::new(vertex_edge_discrete_map(cancel_token, grain, plan, values, -1)?)),
        )?));
    }
    if name == "creaseweight" && (owner == Owner::Vertex || owner == Owner::Primitive) {
        return Ok(None);
    }
    if name == "cornerweight" && owner == Owner::Point {
        let values = match attribute::View::storage(attribute_value)? {
            Storage::Float(values) => values,
            _ => return Err(failm("cornerweight must use float storage")),
        };
        if !generate_resulting_creases || !values.iter().any(|&value| value > 1.0) {
            return Ok(None);
        }
        let mut output = vec![0.0; plan.output_points as usize];
        support::par_fill(
            cancel_token,
            grain_of(grain)?,
            &mut output[..plan.source_points as usize],
            &|point| Ok(fmax(0.0, gf(values, point) - 1.0)),
        )?;
        return Ok(Some(attribute::create_owned(
            "cornerweight",
            Owner::Point,
            Storage::Float(Arc::new(output)),
        )?));
    }
    let use_fvar = fvar_enabled(owner, face_varying_interpolation, plan);
    let storage = match attribute::View::storage(attribute_value)? {
        Storage::Float(values) => {
            let mapped = if use_fvar {
                let fvar = fvar_refinement(
                    cancel_token,
                    grain,
                    face_varying_interpolation,
                    plan,
                    &|left, right| float_equal(gf(values, left), gf(values, right)),
                )?;
                fvar_float(cancel_token, grain, &fvar, values)?
            } else {
                numeric_map(cancel_token, grain, plan, owner, values)?
            };
            Storage::Float(Arc::new(mapped))
        }
        Storage::Int(values) => {
            let mapping = discrete_mapping(cancel_token, grain, plan, owner)?;
            Storage::Int(Arc::new(discrete_map_ints(&mapping, values)?))
        }
        Storage::Text(values) => {
            let mapping = discrete_mapping(cancel_token, grain, plan, owner)?;
            Storage::Text(discrete_map_text(&mapping, values)?)
        }
        Storage::IntArray(values) => {
            let mapping = discrete_mapping(cancel_token, grain, plan, owner)?;
            Storage::IntArray(ragged_ops::remap_int(cancel_token, grain, &mapping, values)?)
        }
        Storage::FloatArray(values) => {
            let mapping = discrete_mapping(cancel_token, grain, plan, owner)?;
            Storage::FloatArray(ragged_ops::remap_float(cancel_token, grain, &mapping, values)?)
        }
        Storage::Float2(values) => {
            let view = packed::Float2::View::view(values);
            let fvar = if use_fvar {
                Some(fvar_refinement(
                    cancel_token,
                    grain,
                    face_varying_interpolation,
                    plan,
                    &|left, right| {
                        float_equal(gf(&view.x, left), gf(&view.x, right))
                            && float_equal(gf(&view.y, left), gf(&view.y, right))
                    },
                )?)
            } else {
                None
            };
            let mut planes = map_planes(cancel_token, grain, plan, owner, &fvar, &[&view.x, &view.y])?;
            let y = planes.pop().unwrap_or_default();
            let x = planes.pop().unwrap_or_default();
            Storage::Float2(packed::Float2::of_owned(x, y)?)
        }
        Storage::Float3(values) => {
            let view = packed::Float3::View::view(values)?;
            let fvar = if use_fvar {
                Some(fvar_refinement(
                    cancel_token,
                    grain,
                    face_varying_interpolation,
                    plan,
                    &|left, right| {
                        float_equal(gf(&view.x, left), gf(&view.x, right))
                            && float_equal(gf(&view.y, left), gf(&view.y, right))
                            && float_equal(gf(&view.z, left), gf(&view.z, right))
                    },
                )?)
            } else {
                None
            };
            let mut planes =
                map_planes(cancel_token, grain, plan, owner, &fvar, &[&view.x, &view.y, &view.z])?;
            let z = planes.pop().unwrap_or_default();
            let y = planes.pop().unwrap_or_default();
            let x = planes.pop().unwrap_or_default();
            Storage::Float3(packed::Float3::View::of_owned_exn(x, y, z)?)
        }
        Storage::Float4(values) => {
            let view = packed::Float4::View::view(values);
            let fvar = if use_fvar {
                Some(fvar_refinement(
                    cancel_token,
                    grain,
                    face_varying_interpolation,
                    plan,
                    &|left, right| {
                        float_equal(gf(&view.x, left), gf(&view.x, right))
                            && float_equal(gf(&view.y, left), gf(&view.y, right))
                            && float_equal(gf(&view.z, left), gf(&view.z, right))
                            && float_equal(gf(&view.w, left), gf(&view.w, right))
                    },
                )?)
            } else {
                None
            };
            let mut planes = map_planes(
                cancel_token,
                grain,
                plan,
                owner,
                &fvar,
                &[&view.x, &view.y, &view.z, &view.w],
            )?;
            let w = planes.pop().unwrap_or_default();
            let z = planes.pop().unwrap_or_default();
            let y = planes.pop().unwrap_or_default();
            let x = planes.pop().unwrap_or_default();
            Storage::Float4(packed::Float4::of_owned(x, y, z, w)?)
        }
    };
    Ok(Some(attribute::create_owned(name, owner, storage)?))
}

pub fn child_crease_sharpness(plan: &Plan) -> Option<Vec<f64>> {
    match &plan.creases {
        None => None,
        Some(creases) if !creases.has_edge_creases => None,
        Some(creases) => {
            if creases.child_edge_sharpness.iter().any(|&value| value > 0.0) {
                Some(creases.child_edge_sharpness.clone())
            } else {
                None
            }
        }
    }
}

pub fn output_child_slot(plan: &Plan, vertex: i64, source_edge: i64) -> Result<i64, error::T> {
    let topology_view = topology::View::view(&plan.topology)?;
    output_child_slot_in(plan, &topology_view, vertex, source_edge)
}

pub fn resulting_crease_attribute(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    child_sharpness: Option<&[f64]>,
) -> Result<Option<attribute::T>, error::T> {
    let Some(child_sharpness) = child_sharpness else {
        return Ok(None);
    };
    let count = plan.vertex_edge_source.len() as i64;
    let mut values = vec![0.0; count as usize];
    // The output topology is read once, not per vertex.
    let topology_view = topology::View::view(&plan.topology)?;
    support::par_fill(cancel_token, grain_of(grain)?, &mut values, &|vertex| {
        let edge = gn(&plan.vertex_edge_source, vertex);
        Ok(if edge >= 0 {
            let slot = output_child_slot_in(plan, &topology_view, vertex, edge)?;
            gf(child_sharpness, slot)
        } else {
            0.0
        })
    })?;
    Ok(Some(attribute::create_owned(
        "creaseweight",
        Owner::Vertex,
        Storage::Float(Arc::new(values)),
    )?))
}

fn output_child_slot_in(
    plan: &Plan,
    topology_view: &topology::View::View,
    vertex: i64,
    source_edge: i64,
) -> Result<i64, error::T> {
    let primitive = match plan.scheme {
        Scheme::CatmullClark | Scheme::Bilinear => vertex / 4,
        Scheme::Loop => vertex / 3,
    };
    let next = if vertex + 1 == gi(&topology_view.primitive_offsets, primitive + 1) {
        gi(&topology_view.primitive_offsets, primitive)
    } else {
        vertex + 1
    };
    let a = gi(&topology_view.vertex_points, vertex);
    let b = gi(&topology_view.vertex_points, next);
    let point = if a < plan.source_points {
        a
    } else if b < plan.source_points {
        b
    } else {
        return Err(failm("subdivision child edge omitted its source endpoint"));
    };
    let side = if point == gi(&plan.index.edge_a, source_edge) {
        0
    } else if point == gi(&plan.index.edge_b, source_edge) {
        1
    } else {
        return Err(failm("subdivision child edge endpoint ancestry is inconsistent"));
    };
    Ok(source_edge * 2 + side)
}

pub struct OutputEdgePlan {
    pub child_edges: Vec<i64>,
    pub output_edge_count: i64,
}

pub fn output_edge_plan(cancel_token: Option<&cancel::T>, plan: &Plan) -> Result<OutputEdgePlan, error::T> {
    let internal_count = match plan.scheme {
        Scheme::CatmullClark | Scheme::Bilinear => plan.source_vertices,
        Scheme::Loop => checked_mul("subdivision internal edge count", plan.source_primitives, 3)?,
    };
    let child_count = checked_mul("subdivision child edge count", plan.edge_count, 2)?;
    let logical_count = checked_add("subdivision edge count", child_count, internal_count)?;
    let mut ordinal_of_key = vec![-1_i64; logical_count as usize];
    let mut next_ordinal = 0_i64;
    let topology_view = topology::View::view(&plan.topology)?;
    for vertex in 0..topology_view.vertex_points.len() as i64 {
        cancel::poll(cancel_token, vertex, 16_383)?;
        let source_edge = gn(&plan.vertex_edge_source, vertex);
        let key = if source_edge >= 0 {
            output_child_slot_in(plan, &topology_view, vertex, source_edge)?
        } else {
            match plan.scheme {
                Scheme::CatmullClark | Scheme::Bilinear => {
                    let output_primitive = vertex / 4;
                    let source_vertex = gn(&plan.vertex_a, output_primitive * 4);
                    let slot = vertex & 3;
                    let source_vertex = if slot == 1 {
                        source_vertex
                    } else if slot == 2 {
                        gi(&plan.index.previous_vertex, source_vertex)
                    } else {
                        return Err(failm("Catmull-Clark internal edge classification failed"));
                    };
                    child_count + source_vertex
                }
                Scheme::Loop => {
                    let output_source = vertex / 12;
                    let slot = vertex % 12;
                    let source_primitive = gn(&plan.primitive_source, output_source * 4);
                    let local = match slot {
                        1 | 11 => 0,
                        4 | 9 => 1,
                        7 | 10 => 2,
                        _ => return Err(failm("Loop internal edge classification failed")),
                    };
                    child_count + (source_primitive * 3) + local
                }
            }
        };
        if ordinal_of_key[key as usize] < 0 {
            ordinal_of_key[key as usize] = next_ordinal;
            next_ordinal += 1;
        }
    }
    ordinal_of_key.truncate(child_count as usize);
    Ok(OutputEdgePlan { child_edges: ordinal_of_key, output_edge_count: next_ordinal })
}

pub fn set_edge_bit(bits: &mut [u8], edge: i64) {
    bits[(edge >> 3) as usize] |= 1 << (edge & 7);
}

pub fn edge_group_from_source(
    cancel_token: Option<&cancel::T>,
    plan: &Plan,
    edge_plan: &OutputEdgePlan,
    name: &str,
    selected: &dyn Fn(i64) -> bool,
) -> Result<edge_group::T, error::T> {
    let mut bits = vec![0u8; ((edge_plan.output_edge_count + 7) / 8) as usize];
    for edge in 0..plan.edge_count {
        cancel::poll(cancel_token, edge, 16_383)?;
        if selected(edge) {
            let left = gi(&edge_plan.child_edges, edge * 2);
            let right = gi(&edge_plan.child_edges, edge * 2 + 1);
            if left >= 0 {
                set_edge_bit(&mut bits, left);
            }
            if right >= 0 {
                set_edge_bit(&mut bits, right);
            }
        }
    }
    edge_group::View::of_owned_bits(&plan.topology, edge_plan.output_edge_count, name, bits)
}

pub fn resulting_crease_group(
    cancel_token: Option<&cancel::T>,
    name: &str,
    plan: &Plan,
    edge_plan: &OutputEdgePlan,
    child_sharpness: Option<&[f64]>,
) -> Result<edge_group::T, error::T> {
    let mut bits = vec![0u8; ((edge_plan.output_edge_count + 7) / 8) as usize];
    if let Some(sharpness) = child_sharpness {
        for child in 0..sharpness.len() as i64 {
            cancel::poll(cancel_token, child, 16_383)?;
            if gf(sharpness, child) > 0.0 {
                let output_edge = gi(&edge_plan.child_edges, child);
                if output_edge >= 0 {
                    set_edge_bit(&mut bits, output_edge);
                }
            }
        }
    }
    edge_group::View::of_owned_bits(&plan.topology, edge_plan.output_edge_count, name, bits)
}

pub fn remap_group(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    group_value: &group::T,
) -> Result<group::T, error::T> {
    let owner = group::owner(group_value);
    let count = match owner {
        group::Owner::Point => plan.output_points,
        group::Owner::Vertex => plan.vertex_kind.len() as i64,
        group::Owner::Primitive => plan.primitive_source.len() as i64,
    };
    cancel::check_opt(cancel_token)?;
    let target = group::init(grain, owner, group::name(group_value), count, &|output| match owner {
        group::Owner::Point => {
            if output < plan.source_points {
                group::mem(output, group_value)
            } else if output < plan.source_points + plan.edge_count {
                let edge = output - plan.source_points;
                group::mem(gi(&plan.index.edge_a, edge), group_value)
                    && group::mem(gi(&plan.index.edge_b, edge), group_value)
            } else {
                let primitive = output - plan.face_offset;
                let first = gi(&plan.source_topology.primitive_offsets, primitive);
                let last = gi(&plan.source_topology.primitive_offsets, primitive + 1);
                let mut all = true;
                for vertex in first..last {
                    if !group::mem(gi(&plan.source_topology.vertex_points, vertex), group_value) {
                        all = false;
                    }
                }
                all
            }
        }
        group::Owner::Vertex => match gu(&plan.vertex_kind, output) {
            0 => group::mem(gn(&plan.vertex_a, output), group_value),
            1 => {
                group::mem(gn(&plan.vertex_a, output), group_value)
                    && group::mem(gn(&plan.vertex_b, output), group_value)
            }
            _ => {
                let primitive = gn(&plan.vertex_primitive, output);
                let first = gi(&plan.source_topology.primitive_offsets, primitive);
                let last = gi(&plan.source_topology.primitive_offsets, primitive + 1);
                let mut all = true;
                for vertex in first..last {
                    if !group::mem(vertex, group_value) {
                        all = false;
                    }
                }
                all
            }
        },
        group::Owner::Primitive => group::mem(gn(&plan.primitive_source, output), group_value),
    })?;
    if !group::is_ordered(group_value) {
        return Ok(target);
    }
    let source_of_target: Vec<i64> = match owner {
        group::Owner::Primitive => plan.primitive_source.iter().map(|&source| i64::from(source)).collect(),
        group::Owner::Point => {
            (0..count).map(|output| if output < plan.source_points { output } else { -1 }).collect()
        }
        group::Owner::Vertex => (0..count)
            .map(|output| {
                if gu(&plan.vertex_kind, output) == 0 { gn(&plan.vertex_a, output) } else { -1 }
            })
            .collect(),
    };
    group::View::remap_order(group_value, &source_of_target, &target)
}

pub fn owner_count(geometry_value: &geometry::T, owner: Owner) -> i64 {
    match owner {
        Owner::Point => geometry::point_count(geometry_value),
        Owner::Vertex => geometry::vertex_count(geometry_value),
        Owner::Primitive => geometry::primitive_count(geometry_value),
        Owner::Detail => 1,
    }
}

pub fn same_attribute_slot(left: &attribute::T, right: &attribute::T) -> bool {
    attribute::owner(left) == attribute::owner(right) && attribute::name(left) == attribute::name(right)
}

pub fn same_attribute_schema(left: &attribute::T, right: &attribute::T) -> bool {
    same_attribute_slot(left, right) && attribute::kind_name(left) == attribute::kind_name(right)
}

pub fn find_attribute_like(
    template: &attribute::T,
    geometry_value: &geometry::T,
) -> Option<attribute::T> {
    geometry::attributes(geometry_value)
        .into_iter()
        .find(|candidate| same_attribute_slot(template, candidate))
}

pub fn checked_total(name: &str, values: &[i64]) -> Result<i64, error::T> {
    let mut total = 0;
    for &value in values {
        total = checked_add(name, total, value)?;
    }
    Ok(total)
}

/// The part's attribute of the template's slot, if it has one of the same schema.
fn part_attributes(
    template: &attribute::T,
    parts: &[geometry::T],
) -> Result<Vec<Option<attribute::T>>, error::T> {
    let mut out = Vec::with_capacity(parts.len());
    for part in parts {
        out.push(match find_attribute_like(template, part) {
            None => None,
            Some(found) if same_attribute_schema(template, &found) => Some(found),
            Some(_) => {
                return Err(fail(format!(
                    "attribute {} changes storage while combining a local subdivision",
                    attribute::name(template)
                )));
            }
        });
    }
    Ok(out)
}

fn wrong_storage() -> error::T {
    failm("attribute storage does not match its template")
}

pub fn concatenate_part_attribute(
    cancel_token: Option<&cancel::T>,
    extra_sources: &[i64],
    template: &attribute::T,
    parts: &[geometry::T],
) -> Result<Option<attribute::T>, error::T> {
    let owner = attribute::owner(template);
    if owner == Owner::Detail {
        return Ok(Some(template.clone()));
    }
    let counts: Vec<i64> = parts.iter().map(|part| owner_count(part, owner)).collect();
    let base_total = checked_total("combined attribute cardinality", &counts)?;
    let total = checked_add(
        "combined attribute cardinality",
        base_total,
        extra_sources.len() as i64,
    )?;
    let attributes = part_attributes(template, parts)?;
    let kind = attribute::kind_name(template);
    let check_source = |source: i64, label: &str| -> Result<(), error::T> {
        if source < 0 || source >= base_total {
            return Err(fail(format!("combined {label} ancestry is out of range")));
        }
        Ok(())
    };
    let storage = match attribute::View::storage(template)? {
        Storage::Float(_) | Storage::Float2(_) | Storage::Float3(_) | Storage::Float4(_) => {
            let plane_count = match attribute::View::storage(template)? {
                Storage::Float(_) => 1,
                Storage::Float2(_) => 2,
                Storage::Float3(_) => 3,
                _ => 4,
            };
            let mut planes = vec![vec![0.0_f64; total as usize]; plane_count];
            let mut offset = 0_i64;
            for (count, found) in counts.iter().zip(attributes.iter()) {
                cancel::check_opt(cancel_token)?;
                if let Some(found) = found {
                    let source_planes = attribute::float_planes(attribute::View::storage(found)?)?
                        .ok_or_else(wrong_storage)?;
                    if source_planes.len() != plane_count {
                        return Err(wrong_storage());
                    }
                    for (plane, values) in planes.iter_mut().zip(source_planes.iter()) {
                        if values.len() as i64 != *count {
                            let label = if kind == "float" { "float" } else { kind };
                            return Err(fail(format!("{label} attribute cardinality mismatch")));
                        }
                        plane[offset as usize..(offset + count) as usize].copy_from_slice(values);
                    }
                }
                offset = checked_add("combined attribute offset", offset, *count)?;
            }
            for plane in planes.iter_mut() {
                for (extra, &source) in extra_sources.iter().enumerate() {
                    check_source(source, "attribute")?;
                    plane[base_total as usize + extra] = plane[source as usize];
                }
            }
            let slices: Vec<&[f64]> = planes.iter().map(|plane| plane.as_slice()).collect();
            attribute::of_float_planes(attribute::View::storage(template)?, &slices)?
        }
        Storage::Int(_) => {
            let mut output = vec![0_i64; total as usize];
            let mut offset = 0_i64;
            for (count, found) in counts.iter().zip(attributes.iter()) {
                cancel::check_opt(cancel_token)?;
                if let Some(found) = found {
                    let Storage::Int(values) = attribute::View::storage(found)? else {
                        return Err(wrong_storage());
                    };
                    if values.len() as i64 != *count {
                        return Err(failm("integer attribute cardinality mismatch"));
                    }
                    output[offset as usize..(offset + count) as usize].copy_from_slice(values);
                }
                offset = checked_add("combined attribute offset", offset, *count)?;
            }
            for (extra, &source) in extra_sources.iter().enumerate() {
                check_source(source, "attribute")?;
                output[base_total as usize + extra] = output[source as usize];
            }
            Storage::Int(Arc::new(output))
        }
        Storage::Text(_) => {
            let mut output = vec![Vec::new(); total as usize];
            let mut offset = 0_i64;
            for (count, found) in counts.iter().zip(attributes.iter()) {
                cancel::check_opt(cancel_token)?;
                if let Some(found) = found {
                    let Storage::Text(values) = attribute::View::storage(found)? else {
                        return Err(wrong_storage());
                    };
                    if values.len() as i64 != *count {
                        return Err(failm("text attribute cardinality mismatch"));
                    }
                    output[offset as usize..(offset + count) as usize].clone_from_slice(values);
                }
                offset = checked_add("combined attribute offset", offset, *count)?;
            }
            for (extra, &source) in extra_sources.iter().enumerate() {
                check_source(source, "attribute")?;
                output[base_total as usize + extra] = output[source as usize].clone();
            }
            Storage::Text(output)
        }
        Storage::IntArray(_) => {
            let mut views = Vec::with_capacity(attributes.len());
            for found in &attributes {
                views.push(match found {
                    None => None,
                    Some(found) => match attribute::View::storage(found)? {
                        Storage::IntArray(values) => Some(packed::IntArray::View::view(values)),
                        _ => return Err(wrong_storage()),
                    },
                });
            }
            let row_length = |source: i64| -> Result<i64, error::T> {
                let mut offset = 0_i64;
                for (count, view) in counts.iter().zip(views.iter()) {
                    if source < offset + count {
                        return Ok(match view {
                            None => 0,
                            Some(view) => {
                                gi(&view.offsets, source - offset + 1)
                                    - gi(&view.offsets, source - offset)
                            }
                        });
                    }
                    offset += count;
                }
                Err(failm("combined integer-array ancestry is out of range"))
            };
            let mut value_total = 0_i64;
            for view in views.iter().flatten() {
                value_total =
                    checked_add("combined integer-array payload", value_total, view.values.len() as i64)?;
            }
            let mut extra_values = 0_i64;
            for &source in extra_sources {
                check_source(source, "integer-array")?;
                extra_values =
                    checked_add("combined integer-array payload", extra_values, row_length(source)?)?;
            }
            let value_total = checked_add("combined integer-array payload", value_total, extra_values)?;
            let mut offsets =
                vec![0_i64; checked_add("combined integer-array offsets", total, 1)? as usize];
            let mut values = vec![0_i64; value_total as usize];
            let mut row_at = 0_i64;
            let mut value_at = 0_i64;
            for (count, view) in counts.iter().zip(views.iter()) {
                match view {
                    None => {
                        for row in 0..*count {
                            offsets[(row_at + row + 1) as usize] = value_at;
                        }
                        row_at += count;
                    }
                    Some(source) => {
                        if source.offsets.len() as i64 != count + 1 {
                            return Err(failm("integer-array attribute cardinality mismatch"));
                        }
                        for row in 0..*count {
                            cancel::poll(cancel_token, row, 4095)?;
                            offsets[(row_at + row + 1) as usize] =
                                value_at + gi(&source.offsets, row + 1);
                        }
                        let length = source.values.len();
                        values[value_at as usize..value_at as usize + length]
                            .copy_from_slice(&source.values);
                        row_at += count;
                        value_at += length as i64;
                    }
                }
            }
            for (extra, &source) in extra_sources.iter().enumerate() {
                let first = offsets[source as usize];
                let last = offsets[source as usize + 1];
                let length = last - first;
                values.copy_within(first as usize..last as usize, value_at as usize);
                value_at += length;
                offsets[base_total as usize + extra + 1] = value_at;
            }
            Storage::IntArray(packed::IntArray::View::create_validated_owned(offsets, values))
        }
        Storage::FloatArray(_) => {
            let mut views = Vec::with_capacity(attributes.len());
            for found in &attributes {
                views.push(match found {
                    None => None,
                    Some(found) => match attribute::View::storage(found)? {
                        Storage::FloatArray(values) => Some(packed::FloatArray::View::view(values)),
                        _ => return Err(wrong_storage()),
                    },
                });
            }
            let row_length = |source: i64| -> Result<i64, error::T> {
                let mut offset = 0_i64;
                for (count, view) in counts.iter().zip(views.iter()) {
                    if source < offset + count {
                        return Ok(match view {
                            None => 0,
                            Some(view) => {
                                gi(&view.offsets, source - offset + 1)
                                    - gi(&view.offsets, source - offset)
                            }
                        });
                    }
                    offset += count;
                }
                Err(failm("combined float-array ancestry is out of range"))
            };
            let mut value_total = 0_i64;
            for view in views.iter().flatten() {
                value_total =
                    checked_add("combined float-array payload", value_total, view.values.len() as i64)?;
            }
            let mut extra_values = 0_i64;
            for &source in extra_sources {
                check_source(source, "float-array")?;
                extra_values =
                    checked_add("combined float-array payload", extra_values, row_length(source)?)?;
            }
            let value_total = checked_add("combined float-array payload", value_total, extra_values)?;
            let mut offsets =
                vec![0_i64; checked_add("combined float-array offsets", total, 1)? as usize];
            let mut values = vec![0.0_f64; value_total as usize];
            let mut row_at = 0_i64;
            let mut value_at = 0_i64;
            for (count, view) in counts.iter().zip(views.iter()) {
                match view {
                    None => {
                        for row in 0..*count {
                            offsets[(row_at + row + 1) as usize] = value_at;
                        }
                        row_at += count;
                    }
                    Some(source) => {
                        if source.offsets.len() as i64 != count + 1 {
                            return Err(failm("float-array attribute cardinality mismatch"));
                        }
                        for row in 0..*count {
                            cancel::poll(cancel_token, row, 4095)?;
                            offsets[(row_at + row + 1) as usize] =
                                value_at + gi(&source.offsets, row + 1);
                        }
                        let length = source.values.len();
                        values[value_at as usize..value_at as usize + length]
                            .copy_from_slice(&source.values);
                        row_at += count;
                        value_at += length as i64;
                    }
                }
            }
            for (extra, &source) in extra_sources.iter().enumerate() {
                let first = offsets[source as usize];
                let last = offsets[source as usize + 1];
                let length = last - first;
                values.copy_within(first as usize..last as usize, value_at as usize);
                value_at += length;
                offsets[base_total as usize + extra + 1] = value_at;
            }
            Storage::FloatArray(packed::FloatArray::View::create_validated_owned(offsets, values))
        }
    };
    Ok(Some(attribute::create_owned(attribute::name(template), owner, storage)?))
}

#[derive(Clone)]
pub struct TriangleAppend {
    pub triangle_points: Vec<i64>,
    pub triangle_vertex_sources: Vec<i64>,
    pub triangle_primitive_sources: Vec<i64>,
}

#[derive(Clone)]
pub struct PointWeld {
    pub old_to_output: Vec<i64>,
    pub output_source: Vec<i64>,
    pub merge_offsets: Vec<i64>,
    pub merge_members: Vec<i64>,
    pub bias: f64,
}

pub fn locate_part(offsets: &[i64], source: i64) -> Result<(usize, i64), error::T> {
    let mut part = 0_usize;
    while part + 1 < offsets.len() && source >= offsets[part + 1] {
        part += 1;
    }
    if part + 1 >= offsets.len() || source < offsets[part] {
        return Err(failm("combined point ancestry is out of range"));
    }
    Ok((part, source - offsets[part]))
}

pub fn welded_float_plane(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    weld: &PointWeld,
    point_offsets: &[i64],
    sources: &[Option<&[f64]>],
) -> Result<Vec<f64>, error::T> {
    let count = weld.output_source.len() as i64;
    let value = |source: i64| -> Result<f64, error::T> {
        let (part, local) = locate_part(point_offsets, source)?;
        Ok(match sources[part] {
            None => 0.0,
            Some(values) => gf(values, local),
        })
    };
    filled(cancel_token, grain, count, &|point| {
        let first = gi(&weld.merge_offsets, point);
        let last = gi(&weld.merge_offsets, point + 1);
        let coarse = value(gi(&weld.output_source, point))?;
        if first == last {
            Ok(coarse)
        } else {
            let mut sum = 0.0;
            for at in first..last {
                sum += value(gi(&weld.merge_members, at))?;
            }
            let refined = sum / ((last - first) as f64);
            Ok(((1.0 - weld.bias) * coarse) + (weld.bias * refined))
        }
    })
}

pub fn welded_representatives(weld: &PointWeld) -> Vec<i64> {
    weld.output_source
        .iter()
        .enumerate()
        .map(|(point, &source)| {
            let first = weld.merge_offsets[point];
            if first == weld.merge_offsets[point + 1] || weld.bias < 0.5 {
                source
            } else {
                weld.merge_members[first as usize]
            }
        })
        .collect()
}

pub fn concatenate_welded_point_attribute(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    template: &attribute::T,
    parts: &[geometry::T],
    point_offsets: &[i64],
    weld: &PointWeld,
) -> Result<attribute::T, error::T> {
    let attributes = part_attributes(template, parts)?;
    // For each welded point, the part attribute and local row of its representative source.
    let representative_rows = || -> Result<Vec<Option<(i64, usize)>>, error::T> {
        let mapping = welded_representatives(weld);
        let mut rows = Vec::with_capacity(mapping.len());
        for source in mapping {
            let (part, local) = locate_part(point_offsets, source)?;
            rows.push(match &attributes[part] {
                None => None,
                Some(_) => Some((local, part)),
            });
        }
        Ok(rows)
    };
    let storage = match attribute::View::storage(template)? {
        Storage::Float(_) | Storage::Float2(_) | Storage::Float3(_) | Storage::Float4(_) => {
            let plane_count = match attribute::View::storage(template)? {
                Storage::Float(_) => 1,
                Storage::Float2(_) => 2,
                Storage::Float3(_) => 3,
                _ => 4,
            };
            let mut per_part: Vec<Option<Vec<&[f64]>>> = Vec::with_capacity(attributes.len());
            for found in &attributes {
                per_part.push(match found {
                    None => None,
                    Some(found) => Some(
                        attribute::float_planes(attribute::View::storage(found)?)?
                            .ok_or_else(wrong_storage)?,
                    ),
                });
            }
            let mut planes = Vec::with_capacity(plane_count);
            for plane in 0..plane_count {
                let sources: Vec<Option<&[f64]>> = per_part
                    .iter()
                    .map(|part| part.as_ref().and_then(|planes| planes.get(plane).copied()))
                    .collect();
                planes.push(welded_float_plane(cancel_token, grain, weld, point_offsets, &sources)?);
            }
            let slices: Vec<&[f64]> = planes.iter().map(|plane| plane.as_slice()).collect();
            attribute::of_float_planes(attribute::View::storage(template)?, &slices)?
        }
        Storage::Int(_) => {
            let rows = representative_rows()?;
            let mut output = Vec::with_capacity(rows.len());
            for row in rows {
                output.push(match row {
                    None => 0,
                    Some((local, part)) => {
                        let found = attributes[part].as_ref().ok_or_else(wrong_storage)?;
                        match attribute::View::storage(found)? {
                            Storage::Int(values) => gi(values, local),
                            _ => return Err(wrong_storage()),
                        }
                    }
                });
            }
            Storage::Int(Arc::new(output))
        }
        Storage::Text(_) => {
            let rows = representative_rows()?;
            let mut output = Vec::with_capacity(rows.len());
            for row in rows {
                output.push(match row {
                    None => Vec::new(),
                    Some((local, part)) => {
                        let found = attributes[part].as_ref().ok_or_else(wrong_storage)?;
                        match attribute::View::storage(found)? {
                            Storage::Text(values) => values[local as usize].clone(),
                            _ => return Err(wrong_storage()),
                        }
                    }
                });
            }
            Storage::Text(output)
        }
        Storage::IntArray(_) => {
            let rows = representative_rows()?;
            let mut views: Vec<Option<packed::IntArray::View::View>> = Vec::new();
            for found in &attributes {
                views.push(match found {
                    None => None,
                    Some(found) => match attribute::View::storage(found)? {
                        Storage::IntArray(values) => Some(packed::IntArray::View::view(values)),
                        _ => return Err(wrong_storage()),
                    },
                });
            }
            let mut offsets = vec![0_i64; rows.len() + 1];
            for (row, entry) in rows.iter().enumerate() {
                offsets[row + 1] = match entry {
                    None => offsets[row],
                    Some((local, part)) => {
                        let view = views[*part].as_ref().ok_or_else(wrong_storage)?;
                        checked_add(
                            "combined integer-array payload",
                            offsets[row],
                            gi(&view.offsets, local + 1) - gi(&view.offsets, *local),
                        )?
                    }
                };
            }
            let mut values = vec![0_i64; offsets[rows.len()] as usize];
            for (row, entry) in rows.iter().enumerate() {
                if let Some((local, part)) = entry {
                    let source = views[*part].as_ref().ok_or_else(wrong_storage)?;
                    let first = gi(&source.offsets, *local) as usize;
                    let last = gi(&source.offsets, local + 1) as usize;
                    let at = offsets[row] as usize;
                    values[at..at + (last - first)].copy_from_slice(&source.values[first..last]);
                }
            }
            Storage::IntArray(packed::IntArray::View::create_validated_owned(offsets, values))
        }
        Storage::FloatArray(_) => {
            let rows = representative_rows()?;
            let mut views: Vec<Option<packed::FloatArray::View::View>> = Vec::new();
            for found in &attributes {
                views.push(match found {
                    None => None,
                    Some(found) => match attribute::View::storage(found)? {
                        Storage::FloatArray(values) => Some(packed::FloatArray::View::view(values)),
                        _ => return Err(wrong_storage()),
                    },
                });
            }
            let mut offsets = vec![0_i64; rows.len() + 1];
            for (row, entry) in rows.iter().enumerate() {
                offsets[row + 1] = match entry {
                    None => offsets[row],
                    Some((local, part)) => {
                        let view = views[*part].as_ref().ok_or_else(wrong_storage)?;
                        checked_add(
                            "combined float-array payload",
                            offsets[row],
                            gi(&view.offsets, local + 1) - gi(&view.offsets, *local),
                        )?
                    }
                };
            }
            let mut values = vec![0.0_f64; offsets[rows.len()] as usize];
            for (row, entry) in rows.iter().enumerate() {
                if let Some((local, part)) = entry {
                    let source = views[*part].as_ref().ok_or_else(wrong_storage)?;
                    let first = gi(&source.offsets, *local) as usize;
                    let last = gi(&source.offsets, local + 1) as usize;
                    let at = offsets[row] as usize;
                    values[at..at + (last - first)].copy_from_slice(&source.values[first..last]);
                }
            }
            Storage::FloatArray(packed::FloatArray::View::create_validated_owned(offsets, values))
        }
    };
    attribute::create_owned(attribute::name(template), Owner::Point, storage)
}

pub fn collect_group_members(group_value: &group::T) -> Vec<i64> {
    let members = std::cell::RefCell::new(Vec::new());
    group::iter(&|element| members.borrow_mut().push(element), group_value);
    members.into_inner()
}

pub fn collect_ordered_members(group_value: &group::T) -> Vec<i64> {
    let members = std::cell::RefCell::new(Vec::new());
    group::iter_ordered(&|element| members.borrow_mut().push(element), group_value);
    members.into_inner()
}

pub fn collect_edge_members(group_value: &edge_group::T) -> Vec<i64> {
    let members = std::cell::RefCell::new(Vec::new());
    edge_group::iter(&|edge| members.borrow_mut().push(edge), group_value);
    members.into_inner()
}

pub fn combine_parts(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    append: Option<&TriangleAppend>,
    weld: Option<&PointWeld>,
    source: &geometry::T,
    parts: &[geometry::T],
) -> Result<geometry::T, error::T> {
    let point_counts: Vec<i64> = parts.iter().map(geometry::point_count).collect();
    let vertex_counts: Vec<i64> = parts.iter().map(geometry::vertex_count).collect();
    let primitive_counts: Vec<i64> = parts.iter().map(geometry::primitive_count).collect();
    let base_point_count = checked_total("combined point count", &point_counts)?;
    let base_vertex_count = checked_total("combined vertex count", &vertex_counts)?;
    let base_primitive_count = checked_total("combined primitive count", &primitive_counts)?;
    let (added_vertices, added_primitives) = match append {
        None => (0, 0),
        Some(append) => {
            let vertices = append.triangle_points.len() as i64;
            if vertices % 3 != 0
                || append.triangle_vertex_sources.len() as i64 != vertices
                || append.triangle_primitive_sources.len() as i64 != vertices / 3
            {
                return Err(failm("combined stitch triangle storage is inconsistent"));
            }
            (vertices, vertices / 3)
        }
    };
    let mut point_offsets = vec![0_i64; parts.len() + 1];
    for (part, geometry_value) in parts.iter().enumerate() {
        point_offsets[part + 1] = checked_add(
            "combined point offsets",
            point_offsets[part],
            geometry::point_count(geometry_value),
        )?;
    }
    let point_count = match weld {
        None => base_point_count,
        Some(weld) => {
            if weld.old_to_output.len() as i64 != base_point_count
                || weld.merge_offsets.len() != weld.output_source.len() + 1
            {
                return Err(failm("combined point weld cardinality is inconsistent"));
            }
            weld.output_source.len() as i64
        }
    };
    let vertex_count = checked_add("combined vertex count", base_vertex_count, added_vertices)?;
    let primitive_count =
        checked_add("combined primitive count", base_primitive_count, added_primitives)?;
    let mut px = vec![0.0_f64; point_count as usize];
    let mut py = vec![0.0_f64; point_count as usize];
    let mut pz = vec![0.0_f64; point_count as usize];
    let mut vertex_points = vec![0_i64; vertex_count as usize];
    let mut primitive_offsets =
        vec![0_i64; checked_add("combined primitive offsets", primitive_count, 1)? as usize];
    let mut primitive_kinds = vec![0u8; primitive_count as usize];
    let mut point_at = 0_i64;
    let mut vertex_at = 0_i64;
    let mut primitive_at = 0_i64;
    for geometry_value in parts {
        cancel::check_opt(cancel_token)?;
        let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
        let topology_view = topology::View::view(geometry::topology(geometry_value))?;
        let points = geometry::point_count(geometry_value);
        let vertices = geometry::vertex_count(geometry_value);
        let primitives = geometry::primitive_count(geometry_value);
        if weld.is_none() {
            let at = point_at as usize;
            px[at..at + points as usize].copy_from_slice(&positions.x);
            py[at..at + points as usize].copy_from_slice(&positions.y);
            pz[at..at + points as usize].copy_from_slice(&positions.z);
        }
        let base_point = point_at;
        let base_vertex = vertex_at;
        let base_primitive = primitive_at;
        let grain = grain_of(grain)?;
        support::par_fill(
            cancel_token,
            grain,
            &mut vertex_points[base_vertex as usize..(base_vertex + vertices) as usize],
            &|vertex| {
                let source_point = gi(&topology_view.vertex_points, vertex) + base_point;
                Ok(match weld {
                    None => source_point,
                    Some(weld) => gi(&weld.old_to_output, source_point),
                })
            },
        )?;
        support::par_fill(
            cancel_token,
            grain,
            &mut primitive_offsets[base_primitive as usize + 1..(base_primitive + primitives) as usize + 1],
            &|primitive| Ok(gi(&topology_view.primitive_offsets, primitive + 1) + base_vertex),
        )?;
        support::par_fill(
            cancel_token,
            grain,
            &mut primitive_kinds[base_primitive as usize..(base_primitive + primitives) as usize],
            &|primitive| Ok(gu(&topology_view.primitive_kinds, primitive)),
        )?;
        point_at += points;
        vertex_at += vertices;
        primitive_at += primitives;
    }
    if let Some(append) = append {
        match weld {
            None => {
                for added in 0..added_vertices {
                    vertex_points[(base_vertex_count + added) as usize] = gi(&append.triangle_points, added);
                }
            }
            Some(weld) => {
                support::par_fill(
                    cancel_token,
                    grain_of(grain)?,
                    &mut vertex_points[base_vertex_count as usize..],
                    &|added| Ok(gi(&weld.old_to_output, gi(&append.triangle_points, added))),
                )?;
            }
        }
        support::par_fill(
            cancel_token,
            grain_of(grain)?,
            &mut primitive_offsets[base_primitive_count as usize + 1..],
            &|added| Ok(base_vertex_count + ((added + 1) * 3)),
        )?;
    }
    if let Some(weld) = weld {
        let mut views = Vec::with_capacity(parts.len());
        for geometry_value in parts {
            views.push(packed::Float3::View::view(geometry::positions(geometry_value))?);
        }
        let x_sources: Vec<Option<&[f64]>> = views.iter().map(|view| Some(&view.x[..])).collect();
        let y_sources: Vec<Option<&[f64]>> = views.iter().map(|view| Some(&view.y[..])).collect();
        let z_sources: Vec<Option<&[f64]>> = views.iter().map(|view| Some(&view.z[..])).collect();
        px = welded_float_plane(cancel_token, grain, weld, &point_offsets, &x_sources)?;
        py = welded_float_plane(cancel_token, grain, weld, &point_offsets, &y_sources)?;
        pz = welded_float_plane(cancel_token, grain, weld, &point_offsets, &z_sources)?;
    }
    let topology_value = topology::View::create_validated_owned(
        point_count,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let source_templates = geometry::attributes(source);
    let mut extra_templates: Vec<attribute::T> = Vec::new();
    for geometry_value in parts {
        for found in geometry::attributes(geometry_value) {
            if !source_templates.iter().any(|template| same_attribute_slot(&found, template))
                && !extra_templates.iter().any(|template| same_attribute_slot(&found, template))
            {
                extra_templates.push(found);
            }
        }
    }
    let mut templates = source_templates;
    templates.extend(extra_templates);
    let empty: Vec<i64> = Vec::new();
    let mut attributes: Vec<attribute::T> = Vec::new();
    for template in &templates {
        let extra_sources: &[i64] = match (append, attribute::owner(template)) {
            (Some(append), Owner::Vertex) => &append.triangle_vertex_sources,
            (Some(append), Owner::Primitive) => &append.triangle_primitive_sources,
            _ => &empty,
        };
        let produced = match (weld, attribute::owner(template)) {
            (Some(weld), Owner::Point) => Some(concatenate_welded_point_attribute(
                cancel_token,
                grain,
                template,
                parts,
                &point_offsets,
                weld,
            )?),
            _ => concatenate_part_attribute(cancel_token, extra_sources, template, parts)?,
        };
        if let Some(produced) = produced {
            attributes.push(produced);
        }
    }
    let mut groups: Vec<group::T> = Vec::new();
    for template in geometry::groups(source) {
        let owner = group::owner(&template);
        let name = group::name(&template).to_owned();
        let count_of = |geometry_value: &geometry::T| match owner {
            group::Owner::Point => geometry::point_count(geometry_value),
            group::Owner::Vertex => geometry::vertex_count(geometry_value),
            group::Owner::Primitive => geometry::primitive_count(geometry_value),
        };
        let counts: Vec<i64> = parts.iter().map(count_of).collect();
        let base_total = checked_total("combined group cardinality", &counts)?;
        let extra_sources: &[i64] = match (append, owner) {
            (Some(append), group::Owner::Vertex) => &append.triangle_vertex_sources,
            (Some(append), group::Owner::Primitive) => &append.triangle_primitive_sources,
            _ => &empty,
        };
        let total = match (weld, owner) {
            (Some(weld), group::Owner::Point) => weld.output_source.len() as i64,
            _ => checked_add("combined group cardinality", base_total, extra_sources.len() as i64)?,
        };
        let mut builder = group::Builder::create(owner, &name, base_total)?;
        let mut offset = 0_i64;
        let mut ordered = false;
        let mut sources: Vec<(i64, group::T)> = Vec::new();
        for geometry_value in parts {
            let found = match geometry::find_group(owner, &name, geometry_value) {
                Some(found) => found,
                None => return Err(failm("local subdivision dropped a group")),
            };
            for element in collect_group_members(&found) {
                group::Builder::set(&mut builder, offset + element, true)?;
            }
            if group::is_ordered(&found) {
                ordered = true;
            }
            sources.push((offset, found));
            offset += count_of(geometry_value);
        }
        let base_target = group::Builder::freeze(&mut builder)?;
        let target = match (weld, owner) {
            (Some(weld), group::Owner::Point) => {
                cancel::check_opt(cancel_token)?;
                group::init(grain, owner, &name, total, &|output| {
                    let mut selected =
                        group::mem(gi(&weld.output_source, output), &base_target);
                    for at in gi(&weld.merge_offsets, output)..gi(&weld.merge_offsets, output + 1) {
                        if group::mem(gi(&weld.merge_members, at), &base_target) {
                            selected = true;
                        }
                    }
                    selected
                })?
            }
            _ if extra_sources.is_empty() => base_target.clone(),
            _ => {
                cancel::check_opt(cancel_token)?;
                group::init(grain, owner, &name, total, &|output| {
                    if output < base_total {
                        group::mem(output, &base_target)
                    } else {
                        group::mem(gi(extra_sources, output - base_total), &base_target)
                    }
                })?
            }
        };
        if !ordered {
            groups.push(target);
            continue;
        }
        let ancestry: Vec<i64> = match (weld, owner) {
            (Some(weld), group::Owner::Point) => (0..total)
                .map(|output| {
                    let primary = gi(&weld.output_source, output);
                    if group::mem(primary, &base_target) {
                        primary
                    } else {
                        let mut selected = -1_i64;
                        for at in
                            gi(&weld.merge_offsets, output)..gi(&weld.merge_offsets, output + 1)
                        {
                            let candidate = gi(&weld.merge_members, at);
                            if selected < 0 && group::mem(candidate, &base_target) {
                                selected = candidate;
                            }
                        }
                        selected
                    }
                })
                .collect(),
            _ => (0..total)
                .map(|output| {
                    if output < base_total { output } else { gi(extra_sources, output - base_total) }
                })
                .collect(),
        };
        let mut order = vec![0_i64; group::cardinality(&base_target) as usize];
        let mut at = 0_usize;
        for (offset, found) in &sources {
            for element in collect_ordered_members(found) {
                order[at] = offset + element;
                at += 1;
            }
        }
        let base_ordered = group::View::with_owned_order(order, &base_target)?;
        groups.push(group::View::remap_order(&base_ordered, &ancestry, &target)?);
    }
    let source_edge_groups = geometry::edge_groups(source);
    let mut edge_groups: Vec<edge_group::T> = Vec::new();
    if !source_edge_groups.is_empty() {
        let target_index = topology_index::create(cancel_token, &topology_value)?;
        for template in &source_edge_groups {
            let name = edge_group::name(template).to_owned();
            let mut builder = edge_group::Builder::create(&topology_value, &target_index, &name)?;
            let mut point_offset = 0_i64;
            for geometry_value in parts {
                let source_topology = geometry::topology(geometry_value);
                let source_index = topology_index::create(cancel_token, source_topology)?;
                let found = match geometry::find_edge_group(&name, geometry_value) {
                    Some(found) => found,
                    None => return Err(failm("local subdivision dropped an edge group")),
                };
                for edge in collect_edge_members(&found) {
                    let (a, b) = topology_index::edge_points(&source_index, edge)?;
                    let a = a + point_offset;
                    let b = b + point_offset;
                    let (a, b) = match weld {
                        None => (a, b),
                        Some(weld) => (gi(&weld.old_to_output, a), gi(&weld.old_to_output, b)),
                    };
                    match topology_index::find_edge(&target_index, a, b) {
                        None => return Err(failm("local subdivision edge ancestry is inconsistent")),
                        Some(target) => edge_group::Builder::set(&mut builder, target, true)?,
                    }
                }
                point_offset += geometry::point_count(geometry_value);
            }
            edge_groups.push(edge_group::Builder::freeze(&mut builder)?);
        }
    }
    let positions = packed::Float3::View::of_owned_exn(px, py, pz)?;
    geometry::create(positions, topology_value, Some(attributes), Some(groups), Some(edge_groups))
}

pub fn select_floats(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    mapping: &[i64],
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    let count = mapping.len() as i64;
    if count == 0 {
        return Ok(Vec::new());
    }
    filled(cancel_token, grain, count, &|point| Ok(gf(source, gi(mapping, point))))
}

/// `select_storage` over the refiner's grain.
fn select_storage(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    mapping: &[i64],
    storage: &Storage,
) -> Result<Storage, error::T> {
    topology_remap::select_storage(cancel_token, grain_of(grain)?, false, mapping, storage)
}

pub fn duplicate_point_attribute(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    point_source: &[i64],
    attribute_value: &attribute::T,
) -> Result<attribute::T, error::T> {
    if attribute::owner(attribute_value) != Owner::Point {
        return Ok(attribute_value.clone());
    }
    let storage =
        select_storage(cancel_token, grain, point_source, attribute::View::storage(attribute_value)?)?;
    attribute::create_owned(attribute::name(attribute_value), Owner::Point, storage)
}

pub fn split_disconnected_point_fans(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let topology_view = topology::View::view(topology_value)?;
    let vertex_count = topology_view.vertex_points.len() as i64;
    if vertex_count == 0 {
        return Ok(geometry_value.clone());
    }
    let topology_index_value = topology_index::create(cancel_token, topology_value)?;
    let index = topology_index::View::view(&topology_index_value);
    let mut parents: Vec<i64> = (0..vertex_count).collect();
    let mut ranks = vec![0_i64; vertex_count as usize];
    for edge in 0..index.edge_a.len() as i64 {
        cancel::poll(cancel_token, edge, 16_383)?;
        let first = gi(&index.edge_offsets, edge);
        let last = gi(&index.edge_offsets, edge + 1);
        if last - first >= 2 {
            let reference = gi(&index.edge_vertices, first);
            let reference_next = gi(&index.next_vertex, reference);
            if reference_next < 0 {
                return Err(failm("topology index exposed an incomplete edge"));
            }
            let reference_a = gi(&topology_view.vertex_points, reference);
            for at in first + 1..last {
                let vertex = gi(&index.edge_vertices, at);
                let next = gi(&index.next_vertex, vertex);
                if next < 0 {
                    return Err(failm("topology index exposed an incomplete edge"));
                }
                if gi(&topology_view.vertex_points, vertex) == reference_a {
                    union_roots(&mut parents, &mut ranks, reference, vertex);
                    union_roots(&mut parents, &mut ranks, reference_next, next);
                } else {
                    union_roots(&mut parents, &mut ranks, reference, next);
                    union_roots(&mut parents, &mut ranks, reference_next, vertex);
                }
            }
        }
    }
    let roots: Vec<i64> = (0..vertex_count).map(|vertex| find_root(&mut parents, vertex)).collect();
    let mut root_target = vec![-1_i64; vertex_count as usize];
    let mut root_source = vec![-1_i64; vertex_count as usize];
    let mut extra = 0_i64;
    for point in 0..topology_view.point_count {
        cancel::poll(cancel_token, point, 4095)?;
        let first = gi(&index.point_offsets, point);
        let last = gi(&index.point_offsets, point + 1);
        let mut first_component = true;
        for at in first..last {
            let vertex = gi(&index.point_vertices, at);
            let root = gi(&roots, vertex);
            if root_target[root as usize] < 0 {
                root_source[root as usize] = point;
                if first_component {
                    root_target[root as usize] = point;
                    first_component = false;
                } else {
                    root_target[root as usize] =
                        checked_add("split-fan point index", topology_view.point_count, extra)?;
                    extra += 1;
                }
            }
        }
    }
    if extra == 0 {
        return Ok(geometry_value.clone());
    }
    let output_points = checked_add("split-fan point count", topology_view.point_count, extra)?;
    let mut point_source: Vec<i64> = (0..output_points)
        .map(|point| if point < topology_view.point_count { point } else { 0 })
        .collect();
    for (root, &target) in root_target.iter().enumerate() {
        if target >= topology_view.point_count {
            point_source[target as usize] = root_source[root];
        }
    }
    let vertex_points = filled(cancel_token, grain, vertex_count, &|vertex| {
        let root = gi(&roots, vertex);
        let target = gi(&root_target, root);
        if target < 0 {
            return Err(failm("split-fan corner has no component"));
        }
        Ok(target)
    })?;
    let output_topology = topology::View::create_validated_owned(
        output_points,
        vertex_points.clone(),
        topology_view.primitive_offsets.to_vec(),
        topology_view.primitive_kinds.to_vec(),
    );
    let source_positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let positions = packed::Float3::View::of_owned_exn(
        select_floats(cancel_token, grain, &point_source, &source_positions.x)?,
        select_floats(cancel_token, grain, &point_source, &source_positions.y)?,
        select_floats(cancel_token, grain, &point_source, &source_positions.z)?,
    )?;
    let mut attributes = Vec::new();
    for found in geometry::attributes(geometry_value) {
        attributes.push(duplicate_point_attribute(cancel_token, grain, &point_source, &found)?);
    }
    let mut groups = Vec::new();
    for found in geometry::groups(geometry_value) {
        if group::owner(&found) != group::Owner::Point {
            groups.push(found);
        } else {
            cancel::check_opt(cancel_token)?;
            let target = group::init(
                grain,
                group::Owner::Point,
                group::name(&found),
                output_points,
                &|point| group::mem(gi(&point_source, point), &found),
            )?;
            groups.push(group::View::remap_order(&found, &point_source, &target)?);
        }
    }
    let source_groups = geometry::edge_groups(geometry_value);
    let mut edge_groups = Vec::new();
    if !source_groups.is_empty() {
        let target_index = topology_index::create(cancel_token, &output_topology)?;
        for found in &source_groups {
            if edge_group::topology_data_id(found)
                != topology_index::topology_data_id(&topology_index_value)
            {
                return Err(failm("edge group belongs to a different topology"));
            }
            let mut builder =
                edge_group::Builder::create(&output_topology, &target_index, edge_group::name(found))?;
            for edge in collect_edge_members(found) {
                for at in gi(&index.edge_offsets, edge)..gi(&index.edge_offsets, edge + 1) {
                    let vertex = gi(&index.edge_vertices, at);
                    let next = gi(&index.next_vertex, vertex);
                    if next < 0 {
                        return Err(failm("topology index exposed an incomplete edge"));
                    }
                    match topology_index::find_edge(
                        &target_index,
                        gi(&vertex_points, vertex),
                        gi(&vertex_points, next),
                    ) {
                        None => return Err(failm("split-fan edge ancestry is inconsistent")),
                        Some(target) => edge_group::Builder::set(&mut builder, target, true)?,
                    }
                }
            }
            edge_groups.push(edge_group::Builder::freeze(&mut builder)?);
        }
    }
    geometry::create(positions, output_topology, Some(attributes), Some(groups), Some(edge_groups))
}

#[derive(Clone)]
pub struct PullPlan {
    pub ancestry_attribute: String,
    pub source_topology: topology::T,
    pub source_index: topology_index::T,
    pub source_positions: packed::Float3::T,
    pub selected_edge_start: Vec<i64>,
    pub unselected_primitive_of_edge: Vec<i64>,
    pub unselected_point_of_source: Vec<i64>,
    pub unselected_primitive_of_source: Vec<i64>,
}

pub fn fresh_edge_ancestry_name(geometry_value: &geometry::T) -> String {
    let mut suffix = 0_i64;
    loop {
        let name = format!("{EDGE_ANCESTRY_PREFIX}{suffix}");
        if geometry::find_attribute(Owner::Vertex, &name, geometry_value).is_none() {
            return name;
        }
        suffix += 1;
    }
}

pub fn annotate_pull_interfaces(
    cancel_token: Option<&cancel::T>,
    primitives: &group::T,
    geometry_value: &geometry::T,
) -> Result<(geometry::T, Option<PullPlan>), error::T> {
    let topology_value = geometry::topology(geometry_value);
    let topology_view = topology::View::view(topology_value)?;
    let topology_index_value = topology_index::create(cancel_token, topology_value)?;
    let index = topology_index::View::view(&topology_index_value);
    let edge_total = index.edge_a.len();
    let mut interface_edges = vec![0u8; edge_total];
    let mut unselected_primitive_of_edge = vec![-1_i64; edge_total];
    let mut interface_count = 0_i64;
    for edge in 0..edge_total as i64 {
        cancel::poll(cancel_token, edge, 16_383)?;
        let mut selected_count = 0_i64;
        let mut unselected_count = 0_i64;
        let mut non_polygon = false;
        for at in gi(&index.edge_offsets, edge)..gi(&index.edge_offsets, edge + 1) {
            let primitive = gi(&index.primitive_of_vertex, gi(&index.edge_vertices, at));
            if gu(&topology_view.primitive_kinds, primitive) != 0 {
                non_polygon = true;
            }
            if group::mem(primitive, primitives) {
                selected_count += 1;
            } else {
                unselected_count += 1;
                if unselected_primitive_of_edge[edge as usize] < 0 {
                    unselected_primitive_of_edge[edge as usize] = primitive;
                }
            }
        }
        if selected_count > 0 && unselected_count > 0 {
            if selected_count != 1 || unselected_count != 1 {
                return Err(fail(format!(
                    "crack closure requires one selected and one unselected face on interface edge {edge}"
                )));
            }
            if non_polygon {
                return Err(failm("crack closure supports polygon surrounding faces only"));
            }
            interface_edges[edge as usize] = 1;
            interface_count += 1;
        }
    }
    if interface_count == 0 {
        return Ok((geometry_value.clone(), None));
    }
    let name = fresh_edge_ancestry_name(geometry_value);
    let vertex_total = topology_view.vertex_points.len();
    let mut ancestry = vec![-1_i64; vertex_total];
    let mut selected_edge_start = vec![-1_i64; edge_total];
    for vertex in 0..vertex_total as i64 {
        cancel::poll(cancel_token, vertex, 16_383)?;
        let edge = gi(&index.edge_of_vertex, vertex);
        if edge >= 0 && interface_edges[edge as usize] != 0 {
            ancestry[vertex as usize] = edge;
            let primitive = gi(&index.primitive_of_vertex, vertex);
            if group::mem(primitive, primitives) {
                selected_edge_start[edge as usize] = gi(&topology_view.vertex_points, vertex);
            }
        }
    }
    let created = attribute::create_owned(&name, Owner::Vertex, Storage::Int(Arc::new(ancestry)))?;
    let annotated = geometry::with_attribute(created, geometry_value)?;
    let mut unselected_used_points = vec![0u8; topology_view.point_count as usize];
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        cancel::poll(cancel_token, primitive, 4095)?;
        if !group::mem(primitive, primitives) {
            for vertex in gi(&topology_view.primitive_offsets, primitive)
                ..gi(&topology_view.primitive_offsets, primitive + 1)
            {
                unselected_used_points[gi(&topology_view.vertex_points, vertex) as usize] = 1;
            }
        }
    }
    let mut unselected_point_of_source = vec![-1_i64; topology_view.point_count as usize];
    let mut point_at = 0_i64;
    for point in 0..topology_view.point_count as usize {
        if unselected_used_points[point] != 0 {
            unselected_point_of_source[point] = point_at;
            point_at += 1;
        }
    }
    let primitive_count = topology_view.primitive_kinds.len();
    let mut unselected_primitive_of_source = vec![-1_i64; primitive_count];
    let mut primitive_at = 0_i64;
    for primitive in 0..primitive_count {
        if !group::mem(primitive as i64, primitives) {
            unselected_primitive_of_source[primitive] = primitive_at;
            primitive_at += 1;
        }
    }
    Ok((
        annotated,
        Some(PullPlan {
            ancestry_attribute: name,
            source_topology: topology_value.clone(),
            source_index: topology_index_value,
            source_positions: geometry::positions(geometry_value).clone(),
            selected_edge_start,
            unselected_primitive_of_edge,
            unselected_point_of_source,
            unselected_primitive_of_source,
        }),
    ))
}

#[derive(Clone)]
pub struct InterfaceChains {
    pub edge_offsets: Vec<i64>,
    pub points: Vec<i64>,
    pub vertices: Vec<i64>,
}

pub fn interface_chains(
    cancel_token: Option<&cancel::T>,
    plan: &PullPlan,
    refined: &geometry::T,
) -> Result<InterfaceChains, error::T> {
    let ancestry = match geometry::find_attribute(Owner::Vertex, &plan.ancestry_attribute, refined) {
        Some(found) => match attribute::View::storage(&found)? {
            Storage::Int(values) => values.clone(),
            _ => return Err(failm("internal edge ancestry changed storage")),
        },
        None => return Err(failm("local subdivision dropped internal edge ancestry")),
    };
    let topology_view = topology::View::view(geometry::topology(refined))?;
    if ancestry.len() != topology_view.vertex_points.len() {
        return Err(failm("internal edge ancestry cardinality mismatch"));
    }
    let source_index = topology_index::View::view(&plan.source_index);
    let source_edges = source_index.edge_a.len();
    let mut counts = vec![0_i64; source_edges];
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        cancel::poll(cancel_token, primitive, 4095)?;
        for vertex in gi(&topology_view.primitive_offsets, primitive)
            ..gi(&topology_view.primitive_offsets, primitive + 1)
        {
            let edge = gi(&ancestry, vertex);
            if edge >= 0 {
                if edge as usize >= source_edges {
                    return Err(failm("internal edge ancestry is out of range"));
                }
                counts[edge as usize] =
                    checked_add("interface descendant edge count", counts[edge as usize], 1)?;
            }
        }
    }
    let mut record_offsets = vec![0_i64; source_edges + 1];
    let mut edge_offsets = vec![0_i64; source_edges + 1];
    for edge in 0..source_edges {
        record_offsets[edge + 1] =
            checked_add("interface descendant storage", record_offsets[edge], counts[edge])?;
        edge_offsets[edge + 1] = checked_add(
            "interface chain storage",
            edge_offsets[edge],
            if counts[edge] == 0 { 0 } else { counts[edge] + 1 },
        )?;
    }
    let descendants = record_offsets[source_edges] as usize;
    let mut starts = vec![0_i64; descendants];
    let mut ends = vec![0_i64; descendants];
    let mut start_vertices = vec![0_i64; descendants];
    let mut end_vertices = vec![0_i64; descendants];
    let mut cursor = record_offsets.clone();
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        let first = gi(&topology_view.primitive_offsets, primitive);
        let last = gi(&topology_view.primitive_offsets, primitive + 1);
        for vertex in first..last {
            let edge = gi(&ancestry, vertex);
            if edge >= 0 {
                let next = if vertex + 1 == last { first } else { vertex + 1 };
                let at = cursor[edge as usize] as usize;
                starts[at] = gi(&topology_view.vertex_points, vertex);
                ends[at] = gi(&topology_view.vertex_points, next);
                start_vertices[at] = vertex;
                end_vertices[at] = next;
                cursor[edge as usize] = at as i64 + 1;
            }
        }
    }
    let chain_total = edge_offsets[source_edges] as usize;
    let mut points = vec![0_i64; chain_total];
    let mut vertices = vec![0_i64; chain_total];
    let mut next_point = vec![-1_i64; topology_view.point_count as usize];
    let mut record_of_start = vec![-1_i64; topology_view.point_count as usize];
    let mut incoming = vec![0u8; topology_view.point_count as usize];
    for edge in 0..source_edges {
        let first = record_offsets[edge];
        let last = record_offsets[edge + 1];
        let count = last - first;
        if count > 0 {
            cancel::poll(cancel_token, edge as i64, 4095)?;
            for at in first..last {
                let start = gi(&starts, at);
                let finish = gi(&ends, at);
                if next_point[start as usize] >= 0 || incoming[finish as usize] != 0 {
                    return Err(failm("interface descendants do not form a simple directed chain"));
                }
                next_point[start as usize] = finish;
                record_of_start[start as usize] = at;
                incoming[finish as usize] = 1;
            }
            let mut chain_start = -1_i64;
            for at in first..last {
                let point = gi(&starts, at);
                if incoming[point as usize] == 0 {
                    if chain_start >= 0 && chain_start != point {
                        return Err(failm("interface descendants contain multiple chains"));
                    }
                    chain_start = point;
                }
            }
            if chain_start < 0 {
                return Err(failm("interface descendants form a closed loop"));
            }
            let output = edge_offsets[edge];
            let mut current = chain_start;
            for local in 0..count {
                let record = record_of_start[current as usize];
                if record < first || record >= last {
                    return Err(failm("interface descendant chain is disconnected"));
                }
                points[(output + local) as usize] = current;
                vertices[(output + local) as usize] = gi(&start_vertices, record);
                current = next_point[current as usize];
                if local + 1 == count {
                    points[(output + local + 1) as usize] = current;
                    vertices[(output + local + 1) as usize] = gi(&end_vertices, record);
                }
            }
            for at in first..last {
                let start = gi(&starts, at);
                let finish = gi(&ends, at);
                next_point[start as usize] = -1;
                record_of_start[start as usize] = -1;
                incoming[finish as usize] = 0;
            }
        }
    }
    Ok(InterfaceChains { edge_offsets, points, vertices })
}

pub fn interpolate_array(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    left: &[i64],
    right: &[i64],
    weight: &[f64],
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    let count = left.len() as i64;
    filled(cancel_token, grain, count, &|target| {
        let a = gi(left, target);
        let b = gi(right, target);
        if a == b {
            Ok(gf(source, a))
        } else {
            let t = gf(weight, target);
            Ok(((1.0 - t) * gf(source, a)) + (t * gf(source, b)))
        }
    })
}

pub fn representative_map(left: &[i64], right: &[i64], weight: &[f64]) -> Vec<i64> {
    left.iter()
        .enumerate()
        .map(|(target, &source)| if weight[target] < 0.5 { source } else { right[target] })
        .collect()
}

#[allow(clippy::too_many_arguments)]
pub fn interpolate_owned_attribute(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    point_left: &[i64],
    point_right: &[i64],
    point_weight: &[f64],
    vertex_left: &[i64],
    vertex_right: &[i64],
    vertex_weight: &[f64],
    primitive_source: &[i64],
    attribute_value: &attribute::T,
) -> Result<attribute::T, error::T> {
    let owner = attribute::owner(attribute_value);
    if owner == Owner::Detail {
        return Ok(attribute_value.clone());
    }
    let zeros;
    let (left, right, weight): (&[i64], &[i64], &[f64]) = match owner {
        Owner::Point => (point_left, point_right, point_weight),
        Owner::Vertex => (vertex_left, vertex_right, vertex_weight),
        _ => {
            zeros = vec![0.0_f64; primitive_source.len()];
            (primitive_source, primitive_source, &zeros)
        }
    };
    let source = attribute::View::storage(attribute_value)?;
    let storage = match attribute::map_float_planes(source, |plane| {
        interpolate_array(cancel_token, grain, left, right, weight, plane)
    })? {
        Some(storage) => storage,
        None => select_storage(cancel_token, grain, &representative_map(left, right, weight), source)?,
    };
    attribute::create_owned(attribute::name(attribute_value), owner, storage)
}

#[derive(Clone)]
pub struct DividedBoundary {
    pub geometry: geometry::T,
    pub points: Vec<i64>,
    pub vertices: Vec<i64>,
    pub source_primitive_to_output: Vec<i64>,
}

pub fn divide_unselected_edges(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &PullPlan,
    chains: &InterfaceChains,
    source: &geometry::T,
) -> Result<DividedBoundary, error::T> {
    let topology_view = topology::View::view(&plan.source_topology)?;
    let source_index = topology_index::View::view(&plan.source_index);
    let source_edges = source_index.edge_a.len() as i64;
    let edge_segments = |edge: i64| -> i64 {
        let length = gi(&chains.edge_offsets, edge + 1) - gi(&chains.edge_offsets, edge);
        if length == 0 { 0 } else { length - 1 }
    };
    let base_points = plan
        .unselected_point_of_source
        .iter()
        .fold(0_i64, |count, &point| if point < 0 { count } else { count.max(point + 1) });
    let primitive_count = plan
        .unselected_primitive_of_source
        .iter()
        .fold(0_i64, |count, &primitive| if primitive < 0 { count } else { count.max(primitive + 1) });
    let mut point_source = vec![0_i64; base_points as usize];
    for (source_point, &target) in plan.unselected_point_of_source.iter().enumerate() {
        if target >= 0 {
            point_source[target as usize] = source_point as i64;
        }
    }
    let mut point_base = vec![-1_i64; source_edges as usize];
    let mut extra_points = 0_i64;
    for edge in 0..source_edges {
        let segments = edge_segments(edge);
        if segments > 0 {
            point_base[edge as usize] =
                checked_add("divided boundary point base", base_points, extra_points)?;
            extra_points =
                checked_add("divided boundary point count", extra_points, segments - 1)?;
        }
    }
    let point_count = checked_add("divided boundary point count", base_points, extra_points)?;
    let mut point_left: Vec<i64> = (0..point_count)
        .map(|point| if point < base_points { point_source[point as usize] } else { 0 })
        .collect();
    let mut point_right = point_left.clone();
    let mut point_weight = vec![0.0_f64; point_count as usize];
    let mut chain_points = vec![0_i64; chains.points.len()];
    for edge in 0..source_edges {
        let first = gi(&chains.edge_offsets, edge);
        let last = gi(&chains.edge_offsets, edge + 1);
        let segments = last - first - 1;
        if segments >= 1 {
            let source_a = gi(&plan.selected_edge_start, edge);
            if source_a < 0 {
                return Err(failm("divided boundary edge has no selected orientation"));
            }
            let edge_a = gi(&source_index.edge_a, edge);
            let edge_b = gi(&source_index.edge_b, edge);
            let source_b = if source_a == edge_a {
                edge_b
            } else if source_a == edge_b {
                edge_a
            } else {
                return Err(failm("divided boundary source orientation is inconsistent"));
            };
            let coarse_a = gi(&plan.unselected_point_of_source, source_a);
            let coarse_b = gi(&plan.unselected_point_of_source, source_b);
            if coarse_a < 0 || coarse_b < 0 {
                return Err(failm("divided boundary endpoint was removed from the coarse region"));
            }
            chain_points[first as usize] = coarse_a;
            chain_points[(last - 1) as usize] = coarse_b;
            for local in 1..segments {
                let output = gi(&point_base, edge) + local - 1;
                chain_points[(first + local) as usize] = output;
                point_left[output as usize] = source_a;
                point_right[output as usize] = source_b;
                point_weight[output as usize] = (local as f64) / (segments as f64);
            }
        }
    }
    let mut vertex_output = vec![-1_i64; topology_view.vertex_points.len()];
    let mut vertex_count = 0_i64;
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        if gi(&plan.unselected_primitive_of_source, primitive) >= 0 {
            for vertex in gi(&topology_view.primitive_offsets, primitive)
                ..gi(&topology_view.primitive_offsets, primitive + 1)
            {
                vertex_output[vertex as usize] = vertex_count;
                vertex_count += 1;
                let edge = gi(&source_index.edge_of_vertex, vertex);
                if edge >= 0 && edge_segments(edge) > 0 {
                    vertex_count = checked_add(
                        "divided boundary vertex count",
                        vertex_count,
                        edge_segments(edge) - 1,
                    )?;
                }
            }
        }
    }
    let mut vertex_points = vec![0_i64; vertex_count as usize];
    let mut vertex_left = vec![0_i64; vertex_count as usize];
    let mut vertex_right = vec![0_i64; vertex_count as usize];
    let mut vertex_weight = vec![0.0_f64; vertex_count as usize];
    let mut primitive_offsets = vec![0_i64; primitive_count as usize + 1];
    let mut primitive_kinds = vec![0u8; primitive_count as usize];
    let mut primitive_source = vec![0_i64; primitive_count as usize];
    let mut chain_vertices = vec![-1_i64; chains.vertices.len()];
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        let target = gi(&plan.unselected_primitive_of_source, primitive);
        if target >= 0 {
            primitive_offsets[target as usize] =
                gi(&vertex_output, gi(&topology_view.primitive_offsets, primitive));
            primitive_kinds[target as usize] = gu(&topology_view.primitive_kinds, primitive);
            primitive_source[target as usize] = primitive;
        }
    }
    primitive_offsets[primitive_count as usize] = vertex_count;
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        if gi(&plan.unselected_primitive_of_source, primitive) >= 0 {
            for vertex in gi(&topology_view.primitive_offsets, primitive)
                ..gi(&topology_view.primitive_offsets, primitive + 1)
            {
                cancel::poll(cancel_token, vertex, 16_383)?;
                let output = gi(&vertex_output, vertex);
                let source_point = gi(&topology_view.vertex_points, vertex);
                let output_point = gi(&plan.unselected_point_of_source, source_point);
                if output_point < 0 {
                    return Err(failm("coarse corner point ancestry was removed"));
                }
                vertex_points[output as usize] = output_point;
                vertex_left[output as usize] = vertex;
                vertex_right[output as usize] = vertex;
                let edge = gi(&source_index.edge_of_vertex, vertex);
                if edge >= 0 && edge_segments(edge) > 0 {
                    let first = gi(&chains.edge_offsets, edge);
                    let last = gi(&chains.edge_offsets, edge + 1);
                    let segments = last - first - 1;
                    let next = gi(&source_index.next_vertex, vertex);
                    if next < 0 {
                        return Err(failm("divided boundary cannot split an incomplete edge"));
                    }
                    let source_a = gi(&plan.selected_edge_start, edge);
                    let forward = source_point == source_a;
                    for local in 1..segments {
                        let target = output + local;
                        let chain_local = if forward { local } else { segments - local };
                        vertex_points[target as usize] = gi(&chain_points, first + chain_local);
                        vertex_left[target as usize] = vertex;
                        vertex_right[target as usize] = next;
                        vertex_weight[target as usize] = (local as f64) / (segments as f64);
                    }
                    if primitive == gi(&plan.unselected_primitive_of_edge, edge) {
                        let (first_vertex, last_vertex) = if forward {
                            (gi(&vertex_output, vertex), gi(&vertex_output, next))
                        } else {
                            (gi(&vertex_output, next), gi(&vertex_output, vertex))
                        };
                        chain_vertices[first as usize] = first_vertex;
                        chain_vertices[(last - 1) as usize] = last_vertex;
                        for local in 1..segments {
                            chain_vertices[(first + local) as usize] =
                                if forward { output + local } else { output + segments - local };
                        }
                    }
                }
            }
        }
    }
    if chain_vertices.iter().any(|&vertex| vertex < 0) {
        return Err(failm("divided boundary chain has no coarse face-varying corner"));
    }
    let source_positions = packed::Float3::View::view(geometry::positions(source))?;
    let positions = packed::Float3::View::of_owned_exn(
        interpolate_array(cancel_token, grain, &point_left, &point_right, &point_weight, &source_positions.x)?,
        interpolate_array(cancel_token, grain, &point_left, &point_right, &point_weight, &source_positions.y)?,
        interpolate_array(cancel_token, grain, &point_left, &point_right, &point_weight, &source_positions.z)?,
    )?;
    let output_topology = topology::View::create_validated_owned(
        point_count,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let mut attributes = Vec::new();
    for found in geometry::attributes(source) {
        attributes.push(interpolate_owned_attribute(
            cancel_token,
            grain,
            &point_left,
            &point_right,
            &point_weight,
            &vertex_left,
            &vertex_right,
            &vertex_weight,
            &primitive_source,
            &found,
        )?);
    }
    let zero_weights = vec![0.0_f64; primitive_count as usize];
    let mut groups = Vec::new();
    for found in geometry::groups(source) {
        let owner = group::owner(&found);
        let (left, right, weight): (&[i64], &[i64], &[f64]) = match owner {
            group::Owner::Point => (&point_left, &point_right, &point_weight),
            group::Owner::Vertex => (&vertex_left, &vertex_right, &vertex_weight),
            group::Owner::Primitive => (&primitive_source, &primitive_source, &zero_weights),
        };
        cancel::check_opt(cancel_token)?;
        let target = group::init(grain, owner, group::name(&found), left.len() as i64, &|output| {
            group::mem(gi(left, output), &found) && group::mem(gi(right, output), &found)
        })?;
        let source_of_target: Vec<i64> = left
            .iter()
            .enumerate()
            .map(|(output, &source_row)| {
                if weight[output] == 0.0 && source_row == right[output] { source_row } else { -1 }
            })
            .collect();
        if group::is_ordered(&found) {
            groups.push(group::View::remap_order(&found, &source_of_target, &target)?);
        } else {
            groups.push(target);
        }
    }
    let target_index = topology_index::create(cancel_token, &output_topology)?;
    let mut edge_groups = Vec::new();
    for found in geometry::edge_groups(source) {
        let mut builder =
            edge_group::Builder::create(&output_topology, &target_index, edge_group::name(&found))?;
        for edge in collect_edge_members(&found) {
            if edge_segments(edge) > 0 {
                let first = gi(&chains.edge_offsets, edge);
                let last = gi(&chains.edge_offsets, edge + 1);
                for at in first..last - 1 {
                    match topology_index::find_edge(
                        &target_index,
                        gi(&chain_points, at),
                        gi(&chain_points, at + 1),
                    ) {
                        None => return Err(failm("divided boundary dropped a child native edge")),
                        Some(child) => edge_group::Builder::set(&mut builder, child, true)?,
                    }
                }
            } else {
                let a = gi(&plan.unselected_point_of_source, gi(&source_index.edge_a, edge));
                let b = gi(&plan.unselected_point_of_source, gi(&source_index.edge_b, edge));
                if a >= 0 && b >= 0 {
                    if let Some(child) = topology_index::find_edge(&target_index, a, b) {
                        edge_group::Builder::set(&mut builder, child, true)?;
                    }
                }
            }
        }
        edge_groups.push(edge_group::Builder::freeze(&mut builder)?);
    }
    let divided_geometry = geometry::create(
        positions,
        output_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )?;
    Ok(DividedBoundary {
        geometry: divided_geometry,
        points: chain_points,
        vertices: chain_vertices,
        source_primitive_to_output: plan.unselected_primitive_of_source.clone(),
    })
}

pub fn remap_divided_attribute(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    vertex_map: &[i64],
    primitive_map: &[i64],
    attribute_value: &attribute::T,
) -> Result<attribute::T, error::T> {
    let mapping = match attribute::owner(attribute_value) {
        Owner::Point | Owner::Detail => return Ok(attribute_value.clone()),
        Owner::Vertex => vertex_map,
        Owner::Primitive => primitive_map,
    };
    let storage =
        select_storage(cancel_token, grain, mapping, attribute::View::storage(attribute_value)?)?;
    attribute::create_owned(
        attribute::name(attribute_value),
        attribute::owner(attribute_value),
        storage,
    )
}

pub fn remap_divided_group(
    grain: Option<i64>,
    vertex_map: &[i64],
    primitive_map: &[i64],
    group_value: &group::T,
) -> Result<group::T, error::T> {
    let mapping = match group::owner(group_value) {
        group::Owner::Point => return Ok(group_value.clone()),
        group::Owner::Vertex => vertex_map,
        group::Owner::Primitive => primitive_map,
    };
    let target = group::init(
        grain,
        group::owner(group_value),
        group::name(group_value),
        mapping.len() as i64,
        &|output| group::mem(gi(mapping, output), group_value),
    )?;
    if group::is_ordered(group_value) {
        group::View::remap_order(group_value, mapping, &target)
    } else {
        Ok(target)
    }
}

struct TriangleOutput {
    vertex_points: Vec<i64>,
    vertex_map: Vec<i64>,
    primitive_map: Vec<i64>,
    primitive_offsets: Vec<i64>,
    primitive_kinds: Vec<u8>,
    first_vertex_output: Vec<i64>,
    first_primitive_output: Vec<i64>,
    vertex_at: i64,
    primitive_at: i64,
}

impl TriangleOutput {
    fn begin_primitive(&mut self, kind: u8, primitive: i64) {
        if self.first_primitive_output[primitive as usize] < 0 {
            self.first_primitive_output[primitive as usize] = self.primitive_at;
        }
        self.primitive_map[self.primitive_at as usize] = primitive;
        self.primitive_kinds[self.primitive_at as usize] = kind;
    }

    fn emit_vertex(&mut self, source_points: &[i64], source_vertex: i64) {
        self.vertex_points[self.vertex_at as usize] = gi(source_points, source_vertex);
        self.vertex_map[self.vertex_at as usize] = source_vertex;
        if self.first_vertex_output[source_vertex as usize] < 0 {
            self.first_vertex_output[source_vertex as usize] = self.vertex_at;
        }
        self.vertex_at += 1;
    }

    fn end_primitive(&mut self) {
        self.primitive_at += 1;
        self.primitive_offsets[self.primitive_at as usize] = self.vertex_at;
    }

    fn emit_triangle(&mut self, source_points: &[i64], primitive: i64, a: i64, b: i64, c: i64) {
        self.begin_primitive(0, primitive);
        self.emit_vertex(source_points, a);
        self.emit_vertex(source_points, b);
        self.emit_vertex(source_points, c);
        self.end_primitive();
    }
}

#[allow(clippy::too_many_arguments)]
pub fn triangulate_divided_boundary(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    pull_bias: Option<f64>,
    consistent_topology: bool,
    plan: &PullPlan,
    chains: &InterfaceChains,
    divided: &DividedBoundary,
    refined: &geometry::T,
) -> Result<DividedBoundary, error::T> {
    let geometry_value = &divided.geometry;
    let topology_value = geometry::topology(geometry_value);
    let topology_view = topology::View::view(topology_value)?;
    let primitive_total = topology_view.primitive_kinds.len();
    let mut boundary = vec![0u8; primitive_total];
    for edge in 0..chains.edge_offsets.len().saturating_sub(1) as i64 {
        if gi(&chains.edge_offsets, edge + 1) > gi(&chains.edge_offsets, edge) {
            let source_primitive = gi(&plan.unselected_primitive_of_edge, edge);
            let primitive = gi(&divided.source_primitive_to_output, source_primitive);
            if primitive < 0 {
                return Err(failm("triangulated boundary primitive was removed"));
            }
            boundary[primitive as usize] = 1;
        }
    }
    let mut output_vertices = 0_i64;
    let mut output_primitives = 0_i64;
    for primitive in 0..primitive_total as i64 {
        cancel::poll(cancel_token, primitive, 1023)?;
        let corners = gi(&topology_view.primitive_offsets, primitive + 1)
            - gi(&topology_view.primitive_offsets, primitive);
        if boundary[primitive as usize] == 0 {
            output_vertices = checked_add("boundary triangulation vertices", output_vertices, corners)?;
            output_primitives += 1;
        } else {
            if gu(&topology_view.primitive_kinds, primitive) != 0 {
                return Err(failm("Triangulate crack closure requires polygon surrounding faces"));
            }
            if corners < 3 {
                return Err(failm("Triangulate crack closure found an invalid polygon"));
            }
            let triangles = corners - 2;
            output_vertices = checked_add(
                "boundary triangulation vertices",
                output_vertices,
                checked_mul("boundary triangulation vertices", triangles, 3)?,
            )?;
            output_primitives =
                checked_add("boundary triangulation primitives", output_primitives, triangles)?;
        }
    }
    let mut out = TriangleOutput {
        vertex_points: vec![0; output_vertices as usize],
        vertex_map: vec![0; output_vertices as usize],
        primitive_map: vec![0; output_primitives as usize],
        primitive_offsets: vec![0; output_primitives as usize + 1],
        primitive_kinds: vec![0; output_primitives as usize],
        first_vertex_output: vec![-1; topology_view.vertex_points.len()],
        first_primitive_output: vec![-1; primitive_total],
        vertex_at: 0,
        primitive_at: 0,
    };
    let mut scratch = rdk_core::polygon_triangulation::create_scratch();
    let source_positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let positions = match pull_bias {
        Some(bias) if !consistent_topology => {
            let refined_positions = packed::Float3::View::view(geometry::positions(refined))?;
            let mut x = source_positions.x.to_vec();
            let mut y = source_positions.y.to_vec();
            let mut z = source_positions.z.to_vec();
            let point_total = topology_view.point_count as usize;
            let mut counts = vec![0_i64; point_total];
            let mut sx = vec![0.0_f64; point_total];
            let mut sy = vec![0.0_f64; point_total];
            let mut sz = vec![0.0_f64; point_total];
            for at in 0..chains.points.len() as i64 {
                let coarse = gi(&divided.points, at) as usize;
                let point = gi(&chains.points, at);
                counts[coarse] += 1;
                sx[coarse] += gf(&refined_positions.x, point);
                sy[coarse] += gf(&refined_positions.y, point);
                sz[coarse] += gf(&refined_positions.z, point);
            }
            for point in 0..point_total {
                if counts[point] > 0 {
                    let inverse = 1.0 / (counts[point] as f64);
                    x[point] = ((1.0 - bias) * x[point]) + (bias * sx[point] * inverse);
                    y[point] = ((1.0 - bias) * y[point]) + (bias * sy[point] * inverse);
                    z[point] = ((1.0 - bias) * z[point]) + (bias * sz[point] * inverse);
                }
            }
            packed::Float3::View::View {
                x: std::sync::Arc::from(x),
                y: std::sync::Arc::from(y),
                z: std::sync::Arc::from(z),
            }
        }
        _ => source_positions,
    };
    for primitive in 0..primitive_total as i64 {
        cancel::poll(cancel_token, primitive, 255)?;
        if boundary[primitive as usize] == 0 {
            let first = gi(&topology_view.primitive_offsets, primitive);
            let last = gi(&topology_view.primitive_offsets, primitive + 1);
            out.begin_primitive(gu(&topology_view.primitive_kinds, primitive), primitive);
            for vertex in first..last {
                out.emit_vertex(&topology_view.vertex_points, vertex);
            }
            out.end_primitive();
        } else if consistent_topology {
            let first = gi(&topology_view.primitive_offsets, primitive);
            let last = gi(&topology_view.primitive_offsets, primitive + 1);
            for vertex in first + 1..last - 1 {
                out.emit_triangle(&topology_view.vertex_points, primitive, first, vertex, vertex + 1);
            }
        } else {
            let mut triangles: Vec<(i64, i64, i64)> = Vec::new();
            let result = rdk_core::polygon_triangulation::primitive(
                cancel_token,
                &positions,
                &topology_view,
                Some(&mut scratch),
                primitive,
                &mut |_, a, b, c| triangles.push((a, b, c)),
            );
            if let Err(failure) = result {
                if error::code(&failure) == "cancelled" {
                    return Err(failure);
                }
                return Err(fail(format!(
                    "Triangulate crack closure: {}",
                    error::message(&failure)
                )));
            }
            for (a, b, c) in triangles {
                out.emit_triangle(&topology_view.vertex_points, primitive, a, b, c);
            }
        }
    }
    if out.vertex_at != output_vertices || out.primitive_at != output_primitives {
        return Err(failm("boundary triangulation output cardinality mismatch"));
    }
    let output_topology = topology::View::create_validated_owned(
        topology_view.point_count,
        out.vertex_points.clone(),
        out.primitive_offsets.clone(),
        out.primitive_kinds.clone(),
    );
    let mut attributes = Vec::new();
    for found in geometry::attributes(geometry_value) {
        attributes.push(remap_divided_attribute(
            cancel_token,
            grain,
            &out.vertex_map,
            &out.primitive_map,
            &found,
        )?);
    }
    let mut groups = Vec::new();
    for found in geometry::groups(geometry_value) {
        groups.push(remap_divided_group(grain, &out.vertex_map, &out.primitive_map, &found)?);
    }
    let source_index = topology_index::create(cancel_token, topology_value)?;
    let target_index = topology_index::create(cancel_token, &output_topology)?;
    let point_map: Vec<i64> = (0..topology_view.point_count).collect();
    let mut edge_groups = Vec::new();
    for found in geometry::edge_groups(geometry_value) {
        edge_groups.push(edge_group::remap(
            cancel_token,
            &source_index,
            &output_topology,
            &target_index,
            &point_map,
            &found,
        )?);
    }
    let rebuilt = geometry::create(
        geometry::positions(geometry_value).clone(),
        output_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )?;
    let mut vertices = Vec::with_capacity(divided.vertices.len());
    for &vertex in divided.vertices.iter() {
        let output = gi(&out.first_vertex_output, vertex);
        if output < 0 {
            return Err(failm("triangulated boundary lost a chain corner"));
        }
        vertices.push(output);
    }
    let source_primitive_to_output = divided
        .source_primitive_to_output
        .iter()
        .map(|&primitive| if primitive < 0 { -1 } else { gi(&out.first_primitive_output, primitive) })
        .collect();
    Ok(DividedBoundary {
        geometry: rebuilt,
        points: divided.points.clone(),
        vertices,
        source_primitive_to_output,
    })
}

pub fn validate_primitive_selection(primitives: &group::T, geometry_value: &geometry::T) -> Result<(), error::T> {
    if group::owner(primitives) != group::Owner::Primitive {
        return Err(failm("selection must be a primitive group"));
    }
    let expected = geometry::primitive_count(geometry_value);
    if group::length(primitives) != expected {
        return Err(fail(format!(
            "selection length {} does not match the primitive count {}",
            group::length(primitives),
            expected
        )));
    }
    Ok(())
}

/// The optional arguments of `Subdivide_refine.once`.
#[derive(Clone)]
pub struct OnceOptions {
    pub grain: Option<i64>,
    pub edge_ancestry_attribute: Option<String>,
    pub edge_crease_override: Option<f64>,
    pub generate_resulting_creases: bool,
    pub resulting_crease_group_name: Option<String>,
    pub remove_holes: bool,
    pub boundary_interpolation: BoundaryInterpolation,
    pub face_varying_interpolation: FaceVaryingInterpolation,
    pub triangle_subdivision: TrianglePolicy,
    pub creasing_method: CreasingMethod,
}

impl Default for OnceOptions {
    fn default() -> OnceOptions {
        OnceOptions {
            grain: None,
            edge_ancestry_attribute: None,
            edge_crease_override: None,
            generate_resulting_creases: true,
            resulting_crease_group_name: None,
            remove_holes: true,
            boundary_interpolation: SubdivideBoundaryEdgeOnly,
            face_varying_interpolation: SubdivideFvarAll,
            triangle_subdivision: SubdivideTrianglesCatmullClark,
            creasing_method: SubdivideCreasingUniform,
        }
    }
}

pub fn once(
    cancel_token: Option<&cancel::T>,
    options: &OnceOptions,
    scheme: Scheme,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let grain = options.grain;
    let holes = if !options.remove_holes {
        None
    } else {
        geometry::find_group(group::Owner::Primitive, "subdivision_hole", geometry_value)
    };
    if let Some(holes) = &holes {
        validate_hole_group(holes, geometry_value)?;
    }
    let plan = make_plan(
        cancel_token,
        grain,
        options.edge_ancestry_attribute.clone(),
        options.edge_crease_override,
        holes.as_ref(),
        options.boundary_interpolation,
        options.triangle_subdivision,
        options.creasing_method,
        scheme,
        geometry_value,
    )?;
    let positions_source = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let positions = packed::Float3::View::of_owned_exn(
        point_float(cancel_token, grain, &plan, &positions_source.x)?,
        point_float(cancel_token, grain, &plan, &positions_source.y)?,
        point_float(cancel_token, grain, &plan, &positions_source.z)?,
    )?;
    let mut attributes: Vec<attribute::T> = Vec::new();
    for found in geometry::attributes(geometry_value) {
        if let Some(remapped) = remap_attribute(
            cancel_token,
            grain,
            options.generate_resulting_creases,
            options.face_varying_interpolation,
            &plan,
            &found,
        )? {
            attributes.push(remapped);
        }
    }
    let child_sharpness =
        if options.generate_resulting_creases { child_crease_sharpness(&plan) } else { None };
    if options.generate_resulting_creases {
        if let Some(created) =
            resulting_crease_attribute(cancel_token, grain, &plan, child_sharpness.as_deref())?
        {
            attributes.push(created);
        }
    }
    let mut groups = Vec::new();
    for found in geometry::groups(geometry_value) {
        groups.push(remap_group(cancel_token, grain, &plan, &found)?);
    }
    let source_groups = geometry::edge_groups(geometry_value);
    let edge_plan = if source_groups.is_empty() && options.resulting_crease_group_name.is_none() {
        None
    } else {
        Some(output_edge_plan(cancel_token, &plan)?)
    };
    let mut edge_groups: Vec<edge_group::T> = Vec::new();
    if !source_groups.is_empty() {
        let Some(edge_plan) = &edge_plan else {
            return Err(failm("internal edge plan is missing"));
        };
        for found in &source_groups {
            if edge_group::topology_data_id(found)
                != topology::data_id(geometry::topology(geometry_value))
            {
                return Err(failm("edge group belongs to a different topology"));
            }
            edge_groups.push(edge_group_from_source(
                cancel_token,
                &plan,
                edge_plan,
                edge_group::name(found),
                &|edge| edge_group::mem(edge, found),
            )?);
        }
    }
    if let Some(name) = &options.resulting_crease_group_name {
        let Some(edge_plan) = &edge_plan else {
            return Err(failm("internal edge plan is missing"));
        };
        let generated =
            resulting_crease_group(cancel_token, name, &plan, edge_plan, child_sharpness.as_deref())?;
        edge_groups.retain(|found| edge_group::name(found) != name);
        edge_groups.push(generated);
    }
    geometry::create(
        positions,
        plan.topology.clone(),
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}
