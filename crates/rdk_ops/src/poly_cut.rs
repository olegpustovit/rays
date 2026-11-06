//! Packed polygon-curve cutting kernel (Poly Cut): removes or splits curve edges at points, edges,
//! attribute crossings or attribute changes.
//!
//! OCaml wraps `cut` in `Error.guard ~operation:"poly_cut" ~code:"invalid_poly_cut"`: a failure has
//! those fields and the OCaml message, a cancellation has code `cancelled`.
use std::sync::atomic::{AtomicI64, AtomicU64, AtomicU8, Ordering};
use std::sync::{Arc, Mutex};

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, support, topology, topology_index,
    topology_remap,
};

use crate::curve_join;
use crate::transform_ops::ocaml_quote;

const MAX_ARRAY_LENGTH: i64 = (1_i64 << 54) - 1;
const OPEN_POLYLINE: u8 = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Element {
    PolyCutPoints,
    PolyCutEdges,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Strategy {
    PolyCutRemove,
    PolyCutCut,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Detection {
    PolyCutAll,
    PolyCutCrossing { attribute: String, value: f64 },
    PolyCutChange { attribute: String, threshold: f64 },
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub primitives: Option<group::T>,
    pub cut_points: Option<group::T>,
    pub cut_edges: Option<edge_group::T>,
    pub element: Element,
    pub strategy: Strategy,
    pub detection: Detection,
    pub keep_closed: bool,
}

fn fail(message: String) -> error::T {
    error::make("Error.Fail".to_owned(), "invalid_parameter".to_owned(), message, None)
        .unwrap_or_else(|bad| bad)
}

/// `Error.guard`: a failure takes the guard's operation and code, a cancellation keeps its own code.
fn recode(operation: &str, code: &str, failure: &error::T) -> error::T {
    let made = if error::code(failure) == "cancelled" {
        error::make(
            operation.to_owned(),
            "cancelled".to_owned(),
            "geometry operation was cancelled".to_owned(),
            None,
        )
    } else {
        error::make(
            operation.to_owned(),
            code.to_owned(),
            error::message(failure).to_owned(),
            None,
        )
    };
    made.unwrap_or_else(|bad| bad)
}

/// Output arrays written by index from the pool; joined into plain vectors afterwards.
struct AInts(Vec<AtomicI64>);
struct AFloats(Vec<AtomicU64>);
struct ABytes(Vec<AtomicU8>);

impl AInts {
    fn new(count: usize, fill: i64) -> AInts {
        AInts((0..count).map(|_| AtomicI64::new(fill)).collect())
    }
    fn set(&self, index: i64, value: i64) {
        self.0[index as usize].store(value, Ordering::Relaxed);
    }
    fn into_vec(self) -> Vec<i64> {
        self.0.into_iter().map(AtomicI64::into_inner).collect()
    }
}

impl AFloats {
    fn new(count: usize, fill: f64) -> AFloats {
        AFloats((0..count).map(|_| AtomicU64::new(fill.to_bits())).collect())
    }
    fn set(&self, index: i64, value: f64) {
        self.0[index as usize].store(value.to_bits(), Ordering::Relaxed);
    }
    fn into_vec(self) -> Vec<f64> {
        self.0.into_iter().map(|value| f64::from_bits(value.into_inner())).collect()
    }
}

impl ABytes {
    fn new(count: usize, fill: u8) -> ABytes {
        ABytes((0..count).map(|_| AtomicU8::new(fill)).collect())
    }
    fn set(&self, index: i64, value: u8) {
        self.0[index as usize].store(value, Ordering::Relaxed);
    }
    fn into_vec(self) -> Vec<u8> {
        self.0.into_iter().map(AtomicU8::into_inner).collect()
    }
}

fn kind_of(code: u8) -> topology::PrimitiveKind {
    match code {
        0 => topology::PrimitiveKind::Polygon,
        1 => topology::PrimitiveKind::OpenPolyline,
        _ => topology::PrimitiveKind::ClosedPolyline,
    }
}

/// `Stdlib.max` on floats.
fn omax(a: f64, b: f64) -> f64 {
    if a >= b { a } else { b }
}

fn selected(selection: Option<&group::T>, index: i64) -> bool {
    match selection {
        None => true,
        Some(value) => group::mem(index, value),
    }
}

fn validate_primitive_selection(primitive_count: i64, selection: Option<&group::T>) -> Result<(), error::T> {
    if let Some(value) = selection {
        if group::owner(value) != group::Owner::Primitive {
            return Err(fail("primitive selection must be primitive-owned".to_owned()));
        }
        if group::length(value) != primitive_count {
            return Err(fail(format!(
                "primitive selection length {} does not match primitive count {primitive_count}",
                group::length(value)
            )));
        }
    }
    Ok(())
}

fn validate_point_selection(point_count: i64, selection: Option<&group::T>) -> Result<(), error::T> {
    if let Some(value) = selection {
        if group::owner(value) != group::Owner::Point {
            return Err(fail("cut-point selection must be point-owned".to_owned()));
        }
        if group::length(value) != point_count {
            return Err(fail(format!(
                "cut-point selection length {} does not match point count {point_count}",
                group::length(value)
            )));
        }
    }
    Ok(())
}

enum Numeric {
    ScalarFloat(Vec<f64>),
    ScalarInt(Vec<i64>),
    Tuple2(packed::Float2::View::View),
    Tuple3(packed::Float3::View::View),
    Tuple4(packed::Float4::View::View),
}

// ponytail: scalar attribute payload is copied once per cut; borrow it if a profile shows this matters.
fn numeric_attribute(name: &str, value: &geometry::T) -> Result<Numeric, error::T> {
    if name.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c')).is_empty() {
        return Err(fail("cut attribute name must not be empty".to_owned()));
    }
    if name == "P" {
        return Ok(Numeric::Tuple3(packed::Float3::View::view(geometry::positions(value))?));
    }
    let Some(item) = geometry::find_attribute(attribute::Owner::Point, name, value) else {
        return Err(fail(format!("point cut attribute {} was not found", ocaml_quote(name))));
    };
    Ok(match attribute::View::storage(&item)? {
        attribute::Storage::Float(values) => Numeric::ScalarFloat(values.clone()),
        attribute::Storage::Int(values) => Numeric::ScalarInt(values.clone()),
        attribute::Storage::Float2(values) => Numeric::Tuple2(packed::Float2::View::view(values)),
        attribute::Storage::Float3(values) => Numeric::Tuple3(packed::Float3::View::view(values)?),
        attribute::Storage::Float4(values) => Numeric::Tuple4(packed::Float4::View::view(values)),
        _ => {
            return Err(fail(format!(
                "point cut attribute {} must have numeric scalar or tuple storage, not {}",
                ocaml_quote(name),
                attribute::kind_name(&item)
            )))
        }
    })
}

fn scalar(value: &Numeric, point: i64) -> f64 {
    match value {
        Numeric::ScalarFloat(values) => values[point as usize],
        Numeric::ScalarInt(values) => values[point as usize] as f64,
        _ => unreachable!("scalar of a tuple attribute"),
    }
}

fn finite_numeric(value: &Numeric, point: i64) -> bool {
    let p = point as usize;
    match value {
        Numeric::ScalarFloat(values) => values[p].is_finite(),
        Numeric::ScalarInt(_) => true,
        Numeric::Tuple2(values) => values.x[p].is_finite() && values.y[p].is_finite(),
        Numeric::Tuple3(values) => values.x[p].is_finite() && values.y[p].is_finite() && values.z[p].is_finite(),
        Numeric::Tuple4(values) => {
            values.x[p].is_finite() && values.y[p].is_finite() && values.z[p].is_finite() && values.w[p].is_finite()
        }
    }
}

fn stable_norm2(x: f64, y: f64) -> f64 {
    let scale = omax(x.abs(), y.abs());
    if scale == 0.0 {
        0.0
    } else if !scale.is_finite() {
        f64::INFINITY
    } else {
        scale * (x / scale * (x / scale) + y / scale * (y / scale)).sqrt()
    }
}

fn stable_norm3(x: f64, y: f64, z: f64) -> f64 {
    let scale = omax(x.abs(), omax(y.abs(), z.abs()));
    if scale == 0.0 {
        0.0
    } else if !scale.is_finite() {
        f64::INFINITY
    } else {
        scale * (x / scale * (x / scale) + y / scale * (y / scale) + z / scale * (z / scale)).sqrt()
    }
}

fn stable_norm4(x: f64, y: f64, z: f64, w: f64) -> f64 {
    let scale = omax(omax(x.abs(), y.abs()), omax(z.abs(), w.abs()));
    if scale == 0.0 {
        0.0
    } else if !scale.is_finite() {
        f64::INFINITY
    } else {
        scale
            * (x / scale * (x / scale) + y / scale * (y / scale) + z / scale * (z / scale) + w / scale * (w / scale))
                .sqrt()
    }
}

fn change_distance(value: &Numeric, left: i64, right: i64) -> f64 {
    let (l, r) = (left as usize, right as usize);
    match value {
        Numeric::ScalarFloat(values) => (values[r] - values[l]).abs(),
        Numeric::ScalarInt(values) => (values[r] as f64 - values[l] as f64).abs(),
        Numeric::Tuple2(values) => stable_norm2(values.x[r] - values.x[l], values.y[r] - values.y[l]),
        Numeric::Tuple3(values) => {
            stable_norm3(values.x[r] - values.x[l], values.y[r] - values.y[l], values.z[r] - values.z[l])
        }
        Numeric::Tuple4(values) => stable_norm4(
            values.x[r] - values.x[l],
            values.y[r] - values.y[l],
            values.z[r] - values.z[l],
            values.w[r] - values.w[l],
        ),
    }
}

fn crossing_weight(threshold: f64, left: f64, right: f64) -> f64 {
    if left == threshold {
        0.0
    } else if right == threshold {
        1.0
    } else {
        let scale = omax(threshold.abs(), omax(left.abs(), right.abs()));
        if scale == 0.0 {
            0.5
        } else {
            let distance_left = (threshold / scale - left / scale).abs();
            let distance_right = (right / scale - threshold / scale).abs();
            distance_left / (distance_left + distance_right)
        }
    }
}

fn interpolate(left: f64, right: f64, weight: f64) -> f64 {
    let delta = right - left;
    if delta.is_finite() {
        left + delta * weight
    } else {
        left * (1.0 - weight) + right * weight
    }
}

fn remap_edge_groups(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    source_index: &topology_index::T,
    target_topology: &topology::T,
    source_edge_of_vertex: &[i64],
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let target_index = topology_index::create(cancel_value, target_topology)?;
    let target = topology_index::View::view(&target_index);
    let edge_count = topology_index::edge_count(&target_index);
    let mut source_of_target = vec![-1_i64; edge_count as usize];
    let mut conflict: Option<i64> = None;
    for vertex in 0..source_edge_of_vertex.len() {
        cancel::poll(cancel_value, vertex as i64, 4095)?;
        let source_edge = source_edge_of_vertex[vertex];
        let target_edge = target.edge_of_vertex[vertex];
        if source_edge >= 0 && target_edge >= 0 {
            let previous = source_of_target[target_edge as usize];
            if previous >= 0 && previous != source_edge {
                conflict = Some(target_edge);
            } else {
                source_of_target[target_edge as usize] = source_edge;
            }
        }
    }
    if let Some(edge) = conflict {
        return Err(fail(format!("output edge {edge} has conflicting source ancestry")));
    }
    let mut output = Vec::with_capacity(groups.len());
    for item in groups {
        if edge_group::topology_data_id(item) != topology_index::topology_data_id(source_index) {
            return Err(fail(format!(
                "edge group {} belongs to a different topology",
                ocaml_quote(edge_group::name(item))
            )));
        }
        let byte_count = (edge_count + 7) / 8;
        let mut bits: Vec<u8> = Vec::with_capacity(byte_count as usize);
        if byte_count > 0 {
            let chunk = (grain / 8).max(1);
            let ranges = ((byte_count - 1) / chunk + 1) as usize;
            let slots: Vec<Mutex<Vec<u8>>> = (0..ranges).map(|_| Mutex::new(Vec::new())).collect();
            support::run_indexed_ranges(chunk, byte_count, &|range, first, last| {
                let mut local = Vec::with_capacity((last - first) as usize);
                for byte in first..last {
                    cancel::poll(cancel_value, byte, 511)?;
                    let mut value = 0_u8;
                    for bit in 0..8 {
                        let edge = (byte << 3) + bit;
                        if edge < edge_count {
                            let source_edge = source_of_target[edge as usize];
                            if source_edge >= 0 && edge_group::mem(source_edge, item) {
                                value |= 1 << bit;
                            }
                        }
                    }
                    local.push(value);
                }
                *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = local;
                Ok(())
            })?;
            for slot in slots {
                bits.extend(slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner));
            }
        }
        output.push(edge_group::View::of_owned_bits(
            target_topology,
            edge_count,
            edge_group::name(item),
            bits,
        )?);
    }
    Ok(output)
}

/// What a cut needs from the source geometry besides its topology.
struct Source {
    view: topology::View::View,
    index: topology_index::View::View,
}

#[allow(clippy::too_many_arguments)]
fn remap_all(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    value: &geometry::T,
    source_index: &topology_index::T,
    output_topology: &topology::T,
    vertex_map: &[i64],
    primitive_map: &[i64],
    source_edge_of_vertex: &[i64],
) -> Result<geometry::T, error::T> {
    let mut attributes = Vec::new();
    for item in geometry::attributes(value) {
        attributes.push(match attribute::owner(&item) {
            attribute::Owner::Point | attribute::Owner::Detail => item,
            attribute::Owner::Vertex => topology_remap::attribute(cancel_value, grain, None, vertex_map, &item)?,
            attribute::Owner::Primitive => topology_remap::attribute(cancel_value, grain, None, primitive_map, &item)?,
        });
    }
    let mut groups = Vec::new();
    for item in geometry::groups(value) {
        groups.push(match group::owner(&item) {
            group::Owner::Point => item,
            group::Owner::Vertex => topology_remap::group(cancel_value, grain, vertex_map, &item)?,
            group::Owner::Primitive => topology_remap::group(cancel_value, grain, primitive_map, &item)?,
        });
    }
    let edge_groups = remap_edge_groups(
        cancel_value,
        grain,
        source_index,
        output_topology,
        source_edge_of_vertex,
        &geometry::edge_groups(value),
    )?;
    geometry::create(
        geometry::positions(value).clone(),
        output_topology.clone(),
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

#[allow(clippy::too_many_arguments)]
fn edge_remove_fast(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    keep_closed: bool,
    source: &Source,
    source_index: &topology_index::T,
    edge_parts: &[i64],
    primitive_action: &[u8],
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let topology = &source.view;
    let index = &source.index;
    let primitive_count = geometry::primitive_count(value);
    let chunk = (grain / 256).max(1);
    let fragment_counts = AInts::new(primitive_count as usize, 0);
    support::parallel_for(cancel_value, chunk, primitive_count, &|primitive| {
        let pu = primitive as usize;
        let first = topology.primitive_offsets[pu];
        let last = topology.primitive_offsets[pu + 1];
        if primitive_action[pu] == 0 {
            fragment_counts.set(primitive, 1);
        } else {
            let mut fragments = 0;
            if topology.primitive_kinds[pu] == OPEN_POLYLINE {
                let mut in_run = false;
                for vertex in first..last {
                    if index.next_vertex[vertex as usize] >= 0 && edge_parts[vertex as usize] > 0 {
                        if !in_run {
                            fragments += 1;
                            in_run = true;
                        }
                    } else {
                        in_run = false;
                    }
                }
            } else {
                for vertex in first..last {
                    if edge_parts[vertex as usize] > 0 {
                        let previous = index.previous_vertex[vertex as usize];
                        if previous < 0 || edge_parts[previous as usize] == 0 {
                            fragments += 1;
                        }
                    }
                }
            }
            fragment_counts.set(primitive, fragments);
        }
        Ok(())
    })?;
    let fragment_counts = fragment_counts.into_vec();
    let mut primitive_bases = vec![0_i64; primitive_count as usize + 1];
    for primitive in 0..primitive_count as usize {
        let count = fragment_counts[primitive];
        if primitive_bases[primitive] > MAX_ARRAY_LENGTH - count {
            return Err(fail("output primitive cardinality exceeds array limits".to_owned()));
        }
        primitive_bases[primitive + 1] = primitive_bases[primitive] + count;
    }
    let output_primitive_count = primitive_bases[primitive_count as usize];
    let opc = output_primitive_count as usize;
    let fragment_start = AInts::new(opc, -1);
    let fragment_edges = AInts::new(opc, -1);
    let primitive_map = AInts::new(opc, 0);
    let fragment_sizes = AInts::new(opc, 0);
    support::parallel_for(cancel_value, chunk, primitive_count, &|primitive| {
        let pu = primitive as usize;
        let target_base = primitive_bases[pu];
        let first = topology.primitive_offsets[pu];
        let last = topology.primitive_offsets[pu + 1];
        if primitive_action[pu] == 0 {
            primitive_map.set(target_base, primitive);
            fragment_sizes.set(target_base, last - first);
            return Ok(());
        }
        let mut state = RunState { target: target_base, active: false, edge_count: 0 };
        let begin = |state: &mut RunState, vertex: i64| {
            state.active = true;
            state.edge_count = 0;
            fragment_start.set(state.target, vertex);
            primitive_map.set(state.target, primitive);
        };
        let finish = |state: &mut RunState| {
            if state.active {
                fragment_edges.set(state.target, state.edge_count);
                fragment_sizes.set(state.target, state.edge_count + 1);
                state.target += 1;
                state.active = false;
            }
        };
        if topology.primitive_kinds[pu] == OPEN_POLYLINE {
            for vertex in first..last {
                if index.next_vertex[vertex as usize] >= 0 && edge_parts[vertex as usize] > 0 {
                    if !state.active {
                        begin(&mut state, vertex);
                    }
                    state.edge_count += 1;
                } else {
                    finish(&mut state);
                }
            }
            finish(&mut state);
        } else {
            let mut start = -1_i64;
            for vertex in first..last {
                let previous = index.previous_vertex[vertex as usize];
                if start < 0 && edge_parts[vertex as usize] > 0 && (previous < 0 || edge_parts[previous as usize] == 0) {
                    start = vertex;
                }
            }
            if start >= 0 {
                let size = last - first;
                for offset in 0..size {
                    let vertex = first + (start - first + offset) % size;
                    if edge_parts[vertex as usize] > 0 {
                        if !state.active {
                            begin(&mut state, vertex);
                        }
                        state.edge_count += 1;
                    } else {
                        finish(&mut state);
                    }
                }
                finish(&mut state);
            }
        }
        Ok(())
    })?;
    let (fragment_start, fragment_edges) = (fragment_start.into_vec(), fragment_edges.into_vec());
    let (primitive_map, fragment_sizes) = (primitive_map.into_vec(), fragment_sizes.into_vec());
    let mut primitive_offsets = vec![0_i64; opc + 1];
    for primitive in 0..opc {
        let size = fragment_sizes[primitive];
        if primitive_offsets[primitive] > MAX_ARRAY_LENGTH - size {
            return Err(fail("output corner cardinality exceeds array limits".to_owned()));
        }
        primitive_offsets[primitive + 1] = primitive_offsets[primitive] + size;
    }
    let output_vertex_count = primitive_offsets[opc];
    let ovc = output_vertex_count as usize;
    let vertex_points = AInts::new(ovc, 0);
    let vertex_map = AInts::new(ovc, 0);
    let primitive_kinds = ABytes::new(opc, OPEN_POLYLINE);
    let source_edge_of_vertex = AInts::new(ovc, -1);
    support::parallel_for(cancel_value, (grain / 8).max(1), output_primitive_count, &|target| {
        let tu = target as usize;
        let primitive = primitive_map[tu];
        let pu = primitive as usize;
        let output = primitive_offsets[tu];
        let source_first = topology.primitive_offsets[pu];
        let source_last = topology.primitive_offsets[pu + 1];
        if fragment_start[tu] < 0 {
            primitive_kinds.set(target, topology.primitive_kinds[pu]);
            for vertex in source_first..source_last {
                let target_vertex = output + vertex - source_first;
                vertex_points.set(target_vertex, topology.vertex_points[vertex as usize]);
                vertex_map.set(target_vertex, vertex);
                source_edge_of_vertex.set(target_vertex, index.edge_of_vertex[vertex as usize]);
            }
        } else {
            let start = fragment_start[tu];
            let edges = fragment_edges[tu];
            let source_size = source_last - source_first;
            let source_kind = topology.primitive_kinds[pu];
            for local in 0..=edges {
                let source_vertex = if source_kind == OPEN_POLYLINE {
                    start + local
                } else {
                    source_first + (start - source_first + local) % source_size
                };
                vertex_points.set(output + local, topology.vertex_points[source_vertex as usize]);
                vertex_map.set(output + local, source_vertex);
                if local < edges {
                    source_edge_of_vertex.set(output + local, index.edge_of_vertex[source_vertex as usize]);
                }
            }
            let size = edges + 1;
            primitive_kinds.set(
                target,
                if source_kind != OPEN_POLYLINE && keep_closed && size >= 3 { source_kind } else { OPEN_POLYLINE },
            );
        }
        Ok(())
    })?;
    let output_topology = topology::create_owned(
        geometry::point_count(value),
        vertex_points.into_vec(),
        primitive_offsets,
        primitive_kinds.into_vec().into_iter().map(kind_of).collect(),
    )?;
    remap_all(
        cancel_value,
        grain,
        value,
        source_index,
        &output_topology,
        &vertex_map.into_vec(),
        &primitive_map,
        &source_edge_of_vertex.into_vec(),
    )
}

struct RunState {
    target: i64,
    active: bool,
    edge_count: i64,
}

enum Compiled {
    Crossing(Numeric, f64),
    Change(Numeric, f64),
}

/// Writes one output sample (a corner) of a cut edge: which source points/vertices it blends.
struct Samples {
    point_left: AInts,
    point_right: AInts,
    vertex_left: AInts,
    vertex_right: AInts,
    weights: AFloats,
    generated: ABytes,
}

impl Samples {
    #[allow(clippy::too_many_arguments)]
    fn write_sample(
        &self,
        source: &Source,
        edge_parts: &[i64],
        edge_cut_weight: &[f64],
        output: i64,
        source_vertex: i64,
        part: i64,
        at_end: bool,
        clone: bool,
    ) {
        let sv = source_vertex as usize;
        let next = source.index.next_vertex[sv];
        let parts = edge_parts[sv];
        let crossing = edge_cut_weight[sv];
        let weight = if crossing >= 0.0 && parts == 2 {
            if at_end {
                if part == 0 { crossing } else { 1.0 }
            } else if part == 0 {
                0.0
            } else {
                crossing
            }
        } else if at_end {
            (part + 1) as f64 / parts as f64
        } else {
            part as f64 / parts as f64
        };
        let left_point = source.view.vertex_points[sv];
        let right_point = source.view.vertex_points[next as usize];
        if weight <= 0.0 {
            self.point_left.set(output, left_point);
            self.point_right.set(output, left_point);
            self.vertex_left.set(output, source_vertex);
            self.vertex_right.set(output, source_vertex);
            self.weights.set(output, 0.0);
        } else if weight >= 1.0 {
            self.point_left.set(output, right_point);
            self.point_right.set(output, right_point);
            self.vertex_left.set(output, next);
            self.vertex_right.set(output, next);
            self.weights.set(output, 0.0);
        } else {
            self.point_left.set(output, left_point);
            self.point_right.set(output, right_point);
            self.vertex_left.set(output, source_vertex);
            self.vertex_right.set(output, next);
            self.weights.set(output, weight);
        }
        if clone || (weight > 0.0 && weight < 1.0) {
            self.generated.set(output, 1);
        }
    }
}

fn run_cut(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let (element, strategy, keep_closed) = (params.element, params.strategy, params.keep_closed);
    let primitives = params.primitives.as_ref();
    let cut_points = params.cut_points.as_ref();
    let cut_edges = params.cut_edges.as_ref();
    if grain <= 0 {
        return Err(fail("grain must be positive".to_owned()));
    }
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(value);
    let view = topology::View::view(topology_value)?;
    let source_index = topology_index::create(cancel_value, topology_value)?;
    let source = Source { view, index: topology_index::View::view(&source_index) };
    let topology = &source.view;
    let index = &source.index;
    let point_count = geometry::point_count(value);
    let vertex_count = geometry::vertex_count(value);
    let primitive_count = geometry::primitive_count(value);
    validate_primitive_selection(primitive_count, primitives)?;
    match element {
        Element::PolyCutPoints => {
            validate_point_selection(point_count, cut_points)?;
            if cut_edges.is_some() {
                return Err(fail("an edge selection is only valid when cutting edges".to_owned()));
            }
        }
        Element::PolyCutEdges => {
            if cut_points.is_some() {
                return Err(fail("a point selection is only valid when cutting points".to_owned()));
            }
            if let Some(selection) = cut_edges {
                if edge_group::topology_data_id(selection) != topology::data_id(topology_value)
                    || edge_group::length(selection) != topology_index::edge_count(&source_index)
                {
                    return Err(fail("cut-edge selection belongs to a different topology".to_owned()));
                }
            }
        }
    }
    let compiled: Option<Compiled> = match &params.detection {
        Detection::PolyCutAll => None,
        Detection::PolyCutCrossing { attribute, value: threshold } => {
            if !threshold.is_finite() {
                return Err(fail("crossing value must be finite".to_owned()));
            }
            let numeric = numeric_attribute(attribute, value)?;
            if !matches!(numeric, Numeric::ScalarFloat(_) | Numeric::ScalarInt(_)) {
                return Err(fail("attribute crossing requires scalar float or integer storage".to_owned()));
            }
            Some(Compiled::Crossing(numeric, *threshold))
        }
        Detection::PolyCutChange { attribute, threshold } => {
            if !threshold.is_finite() || *threshold < 0.0 {
                return Err(fail("change threshold must be finite and non-negative".to_owned()));
            }
            if strategy == Strategy::PolyCutCut && *threshold == 0.0 {
                return Err(fail("cut-at-change requires a positive threshold".to_owned()));
            }
            Some(Compiled::Change(numeric_attribute(attribute, value)?, *threshold))
        }
    };
    let vc = vertex_count as usize;
    let edge_parts = AInts::new(vc, 0);
    let edge_cut_weight = AFloats::new(vc, -1.0);
    let endpoint_bits = ABytes::new(vc, 0);
    let invalid_edges = ABytes::new(vc, 0);
    let blocks = if vertex_count == 0 { 0 } else { 1 + (vertex_count - 1) / grain };
    let error_vertices = AInts::new(blocks as usize, -1);
    let error_kinds = ABytes::new(blocks as usize, 0);
    if blocks > 0 {
        support::run_indexed_ranges(grain, vertex_count, &|block, first, last| {
            cancel::check_opt(cancel_value)?;
            let mut first_error = -1_i64;
            let mut first_kind = 0_u8;
            for vertex in first..last {
                cancel::poll(cancel_value, vertex, 4095)?;
                let vu = vertex as usize;
                let next = index.next_vertex[vu];
                if next < 0 {
                    continue;
                }
                let primitive = index.primitive_of_vertex[vu];
                let edge = index.edge_of_vertex[vu];
                let excluded = !selected(primitives, primitive)
                    || match cut_edges {
                        Some(selection) if element == Element::PolyCutEdges => !edge_group::mem(edge, selection),
                        _ => false,
                    };
                if excluded {
                    edge_parts.set(vertex, 1);
                    continue;
                }
                let left = topology.vertex_points[vu];
                let right = topology.vertex_points[next as usize];
                let eligible = match (element, cut_points) {
                    (Element::PolyCutPoints, Some(selection)) => {
                        group::mem(left, selection) || group::mem(right, selection)
                    }
                    _ => true,
                };
                if !eligible {
                    edge_parts.set(vertex, 1);
                    continue;
                }
                // (invalid, cut_weight, parts, endpoint)
                let (invalid, cut_weight, parts, endpoint): (bool, f64, i64, u8) = match &compiled {
                    None => (true, -1.0, 0, 0),
                    Some(Compiled::Crossing(values, threshold)) => {
                        if !(finite_numeric(values, left) && finite_numeric(values, right)) {
                            if first_error < 0 {
                                first_error = vertex;
                                first_kind = 1;
                            }
                            (false, -1.0, 1, 0)
                        } else {
                            let (a, b) = (scalar(values, left), scalar(values, right));
                            let threshold = *threshold;
                            let crosses = (a <= threshold && b >= threshold) || (a >= threshold && b <= threshold);
                            if !crosses {
                                (false, -1.0, 1, 0)
                            } else if a == threshold && b == threshold {
                                (true, -1.0, 0, 0)
                            } else {
                                let weight = crossing_weight(threshold, a, b);
                                if weight <= 0.0 {
                                    (true, -1.0, 1, 1)
                                } else if weight >= 1.0 {
                                    (true, -1.0, 1, 2)
                                } else {
                                    (true, weight, 2, 0)
                                }
                            }
                        }
                    }
                    Some(Compiled::Change(values, threshold)) => {
                        if !(finite_numeric(values, left) && finite_numeric(values, right)) {
                            if first_error < 0 {
                                first_error = vertex;
                                first_kind = 1;
                            }
                            (false, -1.0, 1, 0)
                        } else {
                            let distance = change_distance(values, left, right);
                            if !distance.is_finite() {
                                if first_error < 0 {
                                    first_error = vertex;
                                    first_kind = 2;
                                }
                                (false, -1.0, 1, 0)
                            } else if distance <= *threshold {
                                (false, -1.0, 1, 0)
                            } else if strategy == Strategy::PolyCutRemove {
                                (true, -1.0, 0, 0)
                            } else {
                                let ratio = distance / *threshold;
                                if !ratio.is_finite() || ratio > MAX_ARRAY_LENGTH as f64 {
                                    if first_error < 0 {
                                        first_error = vertex;
                                        first_kind = 3;
                                    }
                                    (false, -1.0, 1, 0)
                                } else {
                                    (true, -1.0, ratio.ceil() as i64, 0)
                                }
                            }
                        }
                    }
                };
                if invalid {
                    invalid_edges.set(vertex, 1);
                }
                if element == Element::PolyCutEdges {
                    let output_parts = match strategy {
                        Strategy::PolyCutRemove => {
                            if invalid { 0 } else { 1 }
                        }
                        Strategy::PolyCutCut => {
                            if invalid { parts } else { 1 }
                        }
                    };
                    edge_parts.set(vertex, output_parts);
                    edge_cut_weight.set(vertex, cut_weight);
                    endpoint_bits.set(vertex, endpoint);
                }
            }
            if first_error >= 0 {
                error_vertices.set(block, first_error);
                error_kinds.set(block, first_kind);
            }
            Ok(())
        })?;
    }
    let error_vertices = error_vertices.into_vec();
    let error_kinds = error_kinds.into_vec();
    if let Some(block) = error_vertices.iter().position(|&vertex| vertex >= 0) {
        let vertex = error_vertices[block];
        let point = topology.vertex_points[vertex as usize];
        let message = match error_kinds[block] {
            1 => format!("cut attribute is non-finite at operated point {point}"),
            2 => format!("attribute change is not representable on edge from vertex {vertex}"),
            _ => format!("cut subdivision cardinality exceeds array limits at vertex {vertex}"),
        };
        return Err(fail(message));
    }
    let mut edge_parts = edge_parts.into_vec();
    let edge_cut_weight = edge_cut_weight.into_vec();
    let endpoint_bits = endpoint_bits.into_vec();
    let invalid_edges = invalid_edges.into_vec();
    let vertex_break = ABytes::new(vc, 0);
    support::parallel_for(cancel_value, grain, vertex_count, &|vertex| {
        let vu = vertex as usize;
        let next = index.next_vertex[vu];
        let previous = index.previous_vertex[vu];
        if next >= 0 || previous >= 0 {
            match element {
                Element::PolyCutPoints => {
                    let point = topology.vertex_points[vu];
                    if selected(cut_points, point) {
                        let outgoing = next >= 0 && invalid_edges[vu] != 0;
                        let incoming = previous >= 0 && invalid_edges[previous as usize] != 0;
                        if outgoing || incoming {
                            vertex_break.set(vertex, 1);
                        }
                    }
                }
                Element::PolyCutEdges => {
                    let outgoing = next >= 0 && endpoint_bits[vu] & 1 != 0;
                    let incoming = previous >= 0 && endpoint_bits[previous as usize] & 2 != 0;
                    if outgoing || incoming {
                        vertex_break.set(vertex, 1);
                    }
                }
            }
        }
        Ok(())
    })?;
    let vertex_break = vertex_break.into_vec();
    if element == Element::PolyCutPoints {
        let updated = AInts::new(vc, 0);
        support::parallel_for(cancel_value, grain, vertex_count, &|vertex| {
            let vu = vertex as usize;
            let next = index.next_vertex[vu];
            updated.set(
                vertex,
                if next >= 0 {
                    match strategy {
                        Strategy::PolyCutCut => 1,
                        Strategy::PolyCutRemove => {
                            if vertex_break[vu] != 0 || vertex_break[next as usize] != 0 { 0 } else { 1 }
                        }
                    }
                } else {
                    edge_parts[vu]
                },
            );
            Ok(())
        })?;
        edge_parts = updated.into_vec();
    }
    let primitive_action = ABytes::new(primitive_count as usize, 0);
    let chunk = (grain / 256).max(1);
    support::parallel_for(cancel_value, chunk, primitive_count, &|primitive| {
        if selected(primitives, primitive) {
            let first = topology.primitive_offsets[primitive as usize];
            let last = topology.primitive_offsets[primitive as usize + 1];
            let mut changed = false;
            for vertex in first..last {
                if index.next_vertex[vertex as usize] >= 0 && edge_parts[vertex as usize] != 1 {
                    changed = true;
                }
            }
            if !changed {
                for vertex in first..last {
                    let previous = index.previous_vertex[vertex as usize];
                    let next = index.next_vertex[vertex as usize];
                    if previous >= 0
                        && next >= 0
                        && edge_parts[previous as usize] > 0
                        && edge_parts[vertex as usize] > 0
                        && vertex_break[vertex as usize] != 0
                    {
                        changed = true;
                    }
                }
            }
            if changed {
                primitive_action.set(primitive, 1);
            }
        }
        Ok(())
    })?;
    let primitive_action = primitive_action.into_vec();
    if !primitive_action.iter().any(|&byte| byte != 0) {
        return Ok(value.clone());
    }
    if element == Element::PolyCutEdges && strategy == Strategy::PolyCutRemove {
        return edge_remove_fast(
            cancel_value,
            grain,
            keep_closed,
            &source,
            &source_index,
            &edge_parts,
            &primitive_action,
            value,
        );
    }

    let pc = primitive_count as usize;
    let mut primitive_segment_offsets = vec![0_i64; pc + 1];
    for primitive in 0..pc {
        let first = topology.primitive_offsets[primitive];
        let last = topology.primitive_offsets[primitive + 1];
        let mut count = 0_i64;
        for vertex in first..last {
            let parts = edge_parts[vertex as usize];
            if count > MAX_ARRAY_LENGTH - parts {
                return Err(fail("expanded cut-segment cardinality exceeds array limits".to_owned()));
            }
            count += parts;
        }
        if primitive_segment_offsets[primitive] > MAX_ARRAY_LENGTH - count {
            return Err(fail("expanded cut-segment cardinality exceeds array limits".to_owned()));
        }
        primitive_segment_offsets[primitive + 1] = primitive_segment_offsets[primitive] + count;
    }
    let segment_count = primitive_segment_offsets[pc];
    let sc = segment_count as usize;
    let segment_vertex = AInts::new(sc, 0);
    let segment_part = AInts::new(sc, 0);
    let segment_boundary = ABytes::new(sc, 0);
    support::parallel_for(cancel_value, chunk, primitive_count, &|primitive| {
        let pu = primitive as usize;
        let first = topology.primitive_offsets[pu];
        let last = topology.primitive_offsets[pu + 1];
        let mut output = primitive_segment_offsets[pu];
        let closed = topology.primitive_kinds[pu] != OPEN_POLYLINE;
        for vertex in first..last {
            let parts = edge_parts[vertex as usize];
            for part in 0..parts {
                segment_vertex.set(output, vertex);
                segment_part.set(output, part);
                let boundary = if part > 0 {
                    2
                } else if !closed && vertex == first {
                    1
                } else {
                    let previous = index.previous_vertex[vertex as usize];
                    if previous < 0 || edge_parts[previous as usize] == 0 {
                        1
                    } else if vertex_break[vertex as usize] != 0 {
                        2
                    } else {
                        0
                    }
                };
                segment_boundary.set(output, boundary);
                output += 1;
            }
        }
        Ok(())
    })?;
    let (segment_vertex, segment_part) = (segment_vertex.into_vec(), segment_part.into_vec());
    let segment_boundary = segment_boundary.into_vec();
    let fragment_counts = AInts::new(pc, 0);
    support::parallel_for(cancel_value, chunk, primitive_count, &|primitive| {
        let pu = primitive as usize;
        if primitive_action[pu] == 0 {
            fragment_counts.set(primitive, 1);
        } else {
            let first = primitive_segment_offsets[pu];
            let last = primitive_segment_offsets[pu + 1];
            let segments = last - first;
            if segments > 0 {
                let mut boundaries = 0_i64;
                for segment in first..last {
                    if segment_boundary[segment as usize] != 0 {
                        boundaries += 1;
                    }
                }
                let closed = topology.primitive_kinds[pu] != OPEN_POLYLINE;
                fragment_counts.set(primitive, if closed && boundaries == 0 { 1 } else { boundaries });
            }
        }
        Ok(())
    })?;
    let fragment_counts = fragment_counts.into_vec();
    let mut primitive_bases = vec![0_i64; pc + 1];
    for primitive in 0..pc {
        let count = fragment_counts[primitive];
        if primitive_bases[primitive] > MAX_ARRAY_LENGTH - count {
            return Err(fail("output primitive cardinality exceeds array limits".to_owned()));
        }
        primitive_bases[primitive + 1] = primitive_bases[primitive] + count;
    }
    let output_primitive_count = primitive_bases[pc];
    let opc = output_primitive_count as usize;
    let fragment_first = AInts::new(opc, -1);
    let fragment_segments = AInts::new(opc, -1);
    let primitive_map = AInts::new(opc, 0);
    let fragment_sizes = AInts::new(opc, 0);
    support::parallel_for(cancel_value, chunk, primitive_count, &|primitive| {
        let pu = primitive as usize;
        let target_base = primitive_bases[pu];
        let source_size = topology.primitive_offsets[pu + 1] - topology.primitive_offsets[pu];
        if primitive_action[pu] == 0 {
            primitive_map.set(target_base, primitive);
            fragment_sizes.set(target_base, source_size);
            return Ok(());
        }
        let segment_base = primitive_segment_offsets[pu];
        let segment_last = primitive_segment_offsets[pu + 1];
        let segments = segment_last - segment_base;
        if segments > 0 {
            let closed = topology.primitive_kinds[pu] != OPEN_POLYLINE;
            let mut first_boundary = -1_i64;
            for segment in segment_base..segment_last {
                if first_boundary < 0 && segment_boundary[segment as usize] != 0 {
                    first_boundary = segment;
                }
            }
            if closed && first_boundary < 0 {
                primitive_map.set(target_base, primitive);
                fragment_sizes.set(target_base, source_size);
            } else {
                let start = if first_boundary >= 0 { first_boundary } else { segment_base };
                let mut target = target_base - 1;
                let mut current_count = 0_i64;
                for offset in 0..segments {
                    let segment = segment_base + (start - segment_base + offset) % segments;
                    if segment_boundary[segment as usize] != 0 {
                        if target >= target_base {
                            fragment_segments.set(target, current_count);
                            fragment_sizes.set(target, current_count + 1);
                        }
                        target += 1;
                        current_count = 0;
                        fragment_first.set(target, segment);
                        primitive_map.set(target, primitive);
                    }
                    current_count += 1;
                }
                if target >= target_base {
                    fragment_segments.set(target, current_count);
                    fragment_sizes.set(target, current_count + 1);
                }
            }
        }
        Ok(())
    })?;
    let (fragment_first, fragment_segments) = (fragment_first.into_vec(), fragment_segments.into_vec());
    let (primitive_map, fragment_sizes) = (primitive_map.into_vec(), fragment_sizes.into_vec());
    let mut primitive_offsets = vec![0_i64; opc + 1];
    for primitive in 0..opc {
        let size = fragment_sizes[primitive];
        if primitive_offsets[primitive] > MAX_ARRAY_LENGTH - size {
            return Err(fail("output corner cardinality exceeds array limits".to_owned()));
        }
        primitive_offsets[primitive + 1] = primitive_offsets[primitive] + size;
    }
    let output_vertex_count = primitive_offsets[opc];
    let ovc = output_vertex_count as usize;
    let primitive_kinds = ABytes::new(opc, OPEN_POLYLINE);
    let samples = Samples {
        point_left: AInts::new(ovc, 0),
        point_right: AInts::new(ovc, 0),
        vertex_left: AInts::new(ovc, 0),
        vertex_right: AInts::new(ovc, 0),
        weights: AFloats::new(ovc, 0.0),
        generated: ABytes::new(ovc, 0),
    };
    let source_edge_of_vertex = AInts::new(ovc, -1);
    support::parallel_for(cancel_value, (grain / 8).max(1), output_primitive_count, &|target| {
        let tu = target as usize;
        let primitive = primitive_map[tu];
        let pu = primitive as usize;
        let output_first = primitive_offsets[tu];
        if fragment_first[tu] < 0 {
            let source_first = topology.primitive_offsets[pu];
            let source_last = topology.primitive_offsets[pu + 1];
            primitive_kinds.set(target, topology.primitive_kinds[pu]);
            for source_vertex in source_first..source_last {
                let output = output_first + source_vertex - source_first;
                let point = topology.vertex_points[source_vertex as usize];
                samples.point_left.set(output, point);
                samples.point_right.set(output, point);
                samples.vertex_left.set(output, source_vertex);
                samples.vertex_right.set(output, source_vertex);
                source_edge_of_vertex.set(output, index.edge_of_vertex[source_vertex as usize]);
            }
        } else {
            let first_segment = fragment_first[tu];
            let count = fragment_segments[tu];
            let segment_base = primitive_segment_offsets[pu];
            let total_segments = primitive_segment_offsets[pu + 1] - segment_base;
            let first_source_vertex = segment_vertex[first_segment as usize];
            let first_part = segment_part[first_segment as usize];
            let clone = segment_boundary[first_segment as usize] == 2;
            samples.write_sample(&source, &edge_parts, &edge_cut_weight, output_first, first_source_vertex, first_part, false, clone);
            for local in 0..count {
                let segment = segment_base + (first_segment - segment_base + local) % total_segments;
                let source_vertex = segment_vertex[segment as usize];
                let part = segment_part[segment as usize];
                let output = output_first + local;
                source_edge_of_vertex.set(output, index.edge_of_vertex[source_vertex as usize]);
                samples.write_sample(&source, &edge_parts, &edge_cut_weight, output + 1, source_vertex, part, true, false);
            }
            let size = count + 1;
            let source_kind = topology.primitive_kinds[pu];
            primitive_kinds.set(
                target,
                if source_kind != OPEN_POLYLINE && keep_closed && size >= 3 { source_kind } else { OPEN_POLYLINE },
            );
        }
        Ok(())
    })?;
    let primitive_kinds = primitive_kinds.into_vec();
    let point_left = samples.point_left.into_vec();
    let point_right = samples.point_right.into_vec();
    let vertex_left = samples.vertex_left.into_vec();
    let vertex_right = samples.vertex_right.into_vec();
    let weights = samples.weights.into_vec();
    let generated = samples.generated.into_vec();
    let source_edge_of_vertex = source_edge_of_vertex.into_vec();

    let pcount = point_count as usize;
    let mut source_point_used = vec![0_u8; pcount];
    let mut vertex_points = vec![0_i64; ovc];
    if element != Element::PolyCutPoints || strategy != Strategy::PolyCutRemove {
        source_point_used.fill(1);
    } else {
        for point in 0..pcount {
            if index.point_offsets[point] == index.point_offsets[point + 1] {
                source_point_used[point] = 1;
            }
        }
    }
    let mut generated_count = 0_i64;
    for vertex in 0..ovc {
        if generated[vertex] != 0 {
            vertex_points[vertex] = generated_count;
            generated_count += 1;
        } else {
            let point = point_left[vertex];
            if point != point_right[vertex] || weights[vertex] != 0.0 {
                return Err(fail("internal non-generated cut point is not an exact source point".to_owned()));
            }
            source_point_used[point as usize] = 1;
        }
    }
    let mut source_to_output = vec![-1_i64; pcount];
    let mut base_point_count = 0_i64;
    for point in 0..pcount {
        if source_point_used[point] != 0 {
            source_to_output[point] = base_point_count;
            base_point_count += 1;
        }
    }
    if base_point_count > MAX_ARRAY_LENGTH - generated_count {
        return Err(fail("output point cardinality exceeds array limits".to_owned()));
    }
    let output_point_count = base_point_count + generated_count;
    for vertex in 0..ovc {
        if generated[vertex] != 0 {
            vertex_points[vertex] += base_point_count;
        } else {
            vertex_points[vertex] = source_to_output[point_left[vertex] as usize];
        }
    }
    let opcount = output_point_count as usize;
    let mut output_point_left = vec![0_i64; opcount];
    let mut output_point_right = vec![0_i64; opcount];
    let mut output_point_weight = vec![0.0_f64; opcount];
    for point in 0..pcount {
        let output = source_to_output[point];
        if output >= 0 {
            output_point_left[output as usize] = point as i64;
            output_point_right[output as usize] = point as i64;
        }
    }
    for vertex in 0..ovc {
        if generated[vertex] != 0 {
            let output = vertex_points[vertex] as usize;
            output_point_left[output] = point_left[vertex];
            output_point_right[output] = point_right[vertex];
            output_point_weight[output] = weights[vertex];
        }
    }
    let source_positions = packed::Float3::View::view(geometry::positions(value))?;
    let position_blocks = if output_point_count == 0 { 0 } else { 1 + (output_point_count - 1) / grain };
    let position_slots: Vec<Mutex<Option<(Vec<f64>, Vec<f64>, Vec<f64>, i64)>>> =
        (0..position_blocks).map(|_| Mutex::new(None)).collect();
    if output_point_count > 0 {
        support::run_indexed_ranges(grain, output_point_count, &|block, first, last| {
            cancel::check_opt(cancel_value)?;
            let mut first_error = -1_i64;
            let n = (last - first) as usize;
            let (mut xs, mut ys, mut zs) = (Vec::with_capacity(n), Vec::with_capacity(n), Vec::with_capacity(n));
            for point in first..last {
                let (left, right) = (output_point_left[point as usize] as usize, output_point_right[point as usize] as usize);
                let weight = output_point_weight[point as usize];
                let px = interpolate(source_positions.x[left], source_positions.x[right], weight);
                let py = interpolate(source_positions.y[left], source_positions.y[right], weight);
                let pz = interpolate(source_positions.z[left], source_positions.z[right], weight);
                if first_error < 0 && !(px.is_finite() && py.is_finite() && pz.is_finite()) {
                    first_error = point;
                }
                xs.push(px);
                ys.push(py);
                zs.push(pz);
            }
            *position_slots[block as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) =
                Some((xs, ys, zs, first_error));
            Ok(())
        })?;
    }
    let (mut x, mut y, mut z) = (Vec::with_capacity(opcount), Vec::with_capacity(opcount), Vec::with_capacity(opcount));
    let mut position_error: Option<i64> = None;
    for slot in position_slots {
        if let Some((xs, ys, zs, first_error)) = slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
            if position_error.is_none() && first_error >= 0 {
                position_error = Some(first_error);
            }
            x.extend(xs);
            y.extend(ys);
            z.extend(zs);
        }
    }
    if let Some(point) = position_error {
        return Err(fail(format!("interpolated position is non-finite at output point {point}")));
    }
    let output_topology = topology::create_owned(
        output_point_count,
        vertex_points,
        primitive_offsets,
        primitive_kinds.into_iter().map(kind_of).collect(),
    )?;
    let mut attributes = Vec::new();
    for item in geometry::attributes(value) {
        attributes.push(match attribute::owner(&item) {
            attribute::Owner::Point => curve_join::interpolate_attribute(
                cancel_value,
                Some(grain),
                &output_point_left,
                &output_point_right,
                &output_point_weight,
                &[],
                &[],
                &[],
                &item,
            )?,
            attribute::Owner::Vertex => curve_join::interpolate_attribute(
                cancel_value,
                Some(grain),
                &[],
                &[],
                &[],
                &vertex_left,
                &vertex_right,
                &weights,
                &item,
            )?,
            attribute::Owner::Primitive => topology_remap::attribute(cancel_value, grain, None, &primitive_map, &item)?,
            attribute::Owner::Detail => item,
        });
    }
    let nearest = |left: &[i64], right: &[i64], weight: &[f64]| -> Vec<i64> {
        (0..weight.len()).map(|at| if weight[at] < 0.5 { left[at] } else { right[at] }).collect()
    };
    let mut point_group_map: Option<Vec<i64>> = None;
    let mut vertex_group_map: Option<Vec<i64>> = None;
    let mut groups = Vec::new();
    for item in geometry::groups(value) {
        groups.push(match group::owner(&item) {
            group::Owner::Point => {
                let map = point_group_map
                    .get_or_insert_with(|| nearest(&output_point_left, &output_point_right, &output_point_weight));
                topology_remap::group(cancel_value, grain, map, &item)?
            }
            group::Owner::Vertex => {
                let map = vertex_group_map.get_or_insert_with(|| nearest(&vertex_left, &vertex_right, &weights));
                topology_remap::group(cancel_value, grain, map, &item)?
            }
            group::Owner::Primitive => topology_remap::group(cancel_value, grain, &primitive_map, &item)?,
        });
    }
    let edge_groups = remap_edge_groups(
        cancel_value,
        grain,
        &source_index,
        &output_topology,
        &source_edge_of_vertex,
        &geometry::edge_groups(value),
    )?;
    geometry::create(
        packed::Float3::View::of_owned_exn(x, y, z)?,
        output_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

/// Packed polygon-curve cutting kernel.
pub fn cut(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    run_cut(params, value).map_err(|failure| recode("poly_cut", "invalid_poly_cut", &failure))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::packed;

    fn with_attribute(
        owner: attribute::Owner,
        name: &str,
        storage: attribute::Storage,
        value: &geometry::T,
    ) -> geometry::T {
        let item = attribute::create_owned(name, owner, storage).unwrap();
        geometry::with_attribute(item, value).unwrap()
    }

    fn with_float(name: &str, values: Vec<f64>, value: &geometry::T) -> geometry::T {
        with_attribute(attribute::Owner::Point, name, attribute::Storage::Float(values), value)
    }

    fn polyline(points: &[(f64, f64, f64)], closed: bool) -> geometry::T {
        let positions = packed::Float3::of_owned(
            points.iter().map(|p| p.0).collect(),
            points.iter().map(|p| p.1).collect(),
            points.iter().map(|p| p.2).collect(),
        )
        .unwrap();
        let ids: Vec<i64> = (0..points.len() as i64).collect();
        let mut builder = topology::Builder::create(points.len() as i64, None, None).unwrap();
        if closed {
            topology::Builder::add_closed_polyline(&mut builder, &ids).unwrap();
        } else {
            topology::Builder::add_open_polyline(&mut builder, &ids).unwrap();
        }
        let topo = topology::Builder::freeze(&mut builder).unwrap();
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    fn open_curve(count: usize) -> geometry::T {
        let points: Vec<(f64, f64, f64)> = (0..count).map(|p| (p as f64, 0.0, 0.0)).collect();
        polyline(&points, false)
    }

    fn view(value: &geometry::T) -> topology::View::View {
        topology::View::view(geometry::topology(value)).unwrap()
    }

    fn xs(value: &geometry::T) -> Vec<f64> {
        packed::Float3::View::view(geometry::positions(value)).unwrap().x.to_vec()
    }

    fn primitive_points(value: &geometry::T, primitive: usize) -> Vec<i64> {
        let v = view(value);
        v.vertex_points[v.primitive_offsets[primitive] as usize..v.primitive_offsets[primitive + 1] as usize].to_vec()
    }

    fn params() -> Params {
        Params {
            cancel: None,
            grain: Some(1),
            primitives: None,
            cut_points: None,
            cut_edges: None,
            element: Element::PolyCutPoints,
            strategy: Strategy::PolyCutRemove,
            detection: Detection::PolyCutAll,
            keep_closed: true,
        }
    }

    fn edges_cut(detection: Detection) -> Params {
        Params { element: Element::PolyCutEdges, strategy: Strategy::PolyCutCut, detection, ..params() }
    }

    fn crossing(name: &str, value: f64) -> Detection {
        Detection::PolyCutCrossing { attribute: name.to_owned(), value }
    }

    fn point_group(name: &str, count: i64, member: &(dyn Fn(i64) -> bool + Send + Sync)) -> group::T {
        group::init(None, group::Owner::Point, name, count, member).unwrap()
    }

    fn crossing_source() -> geometry::T {
        let curve = with_float("signal", vec![-1.0, 1.0, 2.0, -1.0, -2.0], &open_curve(5));
        let curve = with_float("weight", vec![0.0, 10.0, 20.0, 30.0, 40.0], &curve);
        let curve = with_attribute(attribute::Owner::Point, "id", attribute::Storage::Int(vec![0, 1, 2, 3, 4]), &curve);
        let curve = with_attribute(
            attribute::Owner::Vertex,
            "vertex_weight",
            attribute::Storage::Float(vec![0.0, 10.0, 20.0, 30.0, 40.0]),
            &curve,
        );
        with_attribute(attribute::Owner::Primitive, "piece", attribute::Storage::Int(vec![17]), &curve)
    }

    fn message(result: Result<geometry::T, error::T>) -> (String, String) {
        match result {
            Ok(_) => ("ok".to_owned(), String::new()),
            Err(failure) => (error::code(&failure).to_owned(), error::message(&failure).to_owned()),
        }
    }

    fn expect_invalid(result: Result<geometry::T, error::T>, what: &str) {
        assert_eq!(message(result).0, "invalid_poly_cut", "{what}");
    }

    #[test]
    fn change_subdivision_and_scale_normalised_crossing() {
        let delta = packed::Float2::of_owned(vec![0.0, 3.0], vec![0.0, 4.0]).unwrap();
        let source = with_attribute(attribute::Owner::Point, "delta", attribute::Storage::Float2(delta), &open_curve(2));
        let cut = cut(
            &edges_cut(Detection::PolyCutChange { attribute: "delta".to_owned(), threshold: 2.0 }),
            &source,
        )
        .unwrap();
        assert!(geometry::primitive_count(&cut) == 3 && geometry::point_count(&cut) == 6);
        let x = xs(&cut);
        let (a, b, c) = (primitive_points(&cut, 0), primitive_points(&cut, 1), primitive_points(&cut, 2));
        assert_eq!(x[a[1] as usize], 1.0 / 3.0);
        assert_eq!(x[b[0] as usize], 1.0 / 3.0);
        assert_eq!(x[b[1] as usize], 2.0 / 3.0);
        assert_eq!(x[c[0] as usize], 2.0 / 3.0);
        let extreme = with_float("signal", vec![-f64::MAX, f64::MAX], &open_curve(2));
        let extreme = cut_of(&edges_cut(crossing("signal", 0.0)), &extreme);
        let fragment = primitive_points(&extreme, 0);
        assert_eq!(xs(&extreme)[fragment[1] as usize], 0.5);
    }

    fn cut_of(p: &Params, value: &geometry::T) -> geometry::T {
        cut(p, value).unwrap()
    }

    #[test]
    fn edge_cut_boundary_semantics() {
        let equal = with_float("signal", vec![0.0, 0.0, 1.0], &open_curve(3));
        let out = cut_of(&edges_cut(crossing("signal", 0.0)), &equal);
        assert!(geometry::point_count(&out) == 3 && geometry::primitive_count(&out) == 1);
        assert_eq!(primitive_points(&out, 0), vec![1, 2]);
        let source = open_curve(4);
        let removed = cut_of(&Params { element: Element::PolyCutEdges, ..params() }, &source);
        let cut_all = cut_of(&edges_cut(Detection::PolyCutAll), &source);
        assert_eq!(view(&removed).vertex_points, view(&cut_all).vertex_points);
        assert_eq!(view(&removed).primitive_offsets, view(&cut_all).primitive_offsets);
        assert_eq!(xs(&removed), xs(&cut_all));
        assert!(geometry::point_count(&cut_all) == 4 && geometry::primitive_count(&cut_all) == 0);
    }

    #[test]
    fn discrete_ragged_payload_and_ordered_groups() {
        let base = with_float("signal", vec![-1.0, 1.0, 1.0], &open_curve(3));
        let mut attributes = geometry::attributes(&base);
        let mk = |owner, name: &str, storage| attribute::create_owned(name, owner, storage).unwrap();
        attributes.push(mk(
            attribute::Owner::Point,
            "label",
            attribute::Storage::Text(vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec()]),
        ));
        attributes.push(mk(
            attribute::Owner::Point,
            "rows",
            attribute::Storage::FloatArray(
                packed::FloatArray::create_owned(vec![0, 1, 3, 4], vec![1.0, 2.0, 3.0, 4.0]).unwrap(),
            ),
        ));
        attributes.push(mk(
            attribute::Owner::Vertex,
            "corner_label",
            attribute::Storage::Text(vec![b"va".to_vec(), b"vb".to_vec(), b"vc".to_vec()]),
        ));
        attributes.push(mk(
            attribute::Owner::Vertex,
            "links",
            attribute::Storage::IntArray(
                packed::IntArray::create_owned(vec![0, 1, 3, 4], vec![10, 20, 21, 30]).unwrap(),
            ),
        ));
        attributes.push(mk(attribute::Owner::Detail, "author", attribute::Storage::Text(vec![b"polycut".to_vec()])));
        let groups = vec![
            group::ordered(group::Owner::Point, "marked_points", 3, &[1]).unwrap(),
            group::ordered(group::Owner::Vertex, "marked_vertices", 3, &[1]).unwrap(),
            group::ordered(group::Owner::Primitive, "marked_curve", 1, &[0]).unwrap(),
        ];
        let source = geometry::create(
            geometry::positions(&base).clone(),
            geometry::topology(&base).clone(),
            Some(attributes),
            Some(groups),
            None,
        )
        .unwrap();
        let output = cut_of(&edges_cut(crossing("signal", 0.0)), &source);
        let text = |owner, name: &str| {
            let item = geometry::find_attribute(owner, name, &output).expect(name);
            match attribute::View::storage(&item).unwrap() {
                attribute::Storage::Text(values) => values.clone(),
                _ => panic!("{name} storage"),
            }
        };
        let owned = |values: &[&str]| -> Vec<Vec<u8>> { values.iter().map(|v| v.as_bytes().to_vec()).collect() };
        assert_eq!(text(attribute::Owner::Point, "label"), owned(&["a", "b", "c", "b", "b"]));
        assert_eq!(text(attribute::Owner::Vertex, "corner_label"), owned(&["va", "vb", "vb", "vb", "vc"]));
        let rows = geometry::find_attribute(attribute::Owner::Point, "rows", &output).unwrap();
        let links = geometry::find_attribute(attribute::Owner::Vertex, "links", &output).unwrap();
        let (attribute::Storage::FloatArray(rows), attribute::Storage::IntArray(links)) =
            (attribute::View::storage(&rows).unwrap(), attribute::View::storage(&links).unwrap())
        else {
            panic!("ragged storage")
        };
        assert_eq!(packed::FloatArray::get(rows, 3).unwrap(), vec![2.0, 3.0]);
        assert_eq!(packed::FloatArray::get(rows, 4).unwrap(), vec![2.0, 3.0]);
        assert_eq!(packed::IntArray::get(links, 1).unwrap(), vec![20, 21]);
        assert_eq!(packed::IntArray::get(links, 2).unwrap(), vec![20, 21]);
        let ordered = |owner, name: &str| {
            group::ordered_elements(&geometry::find_group(owner, name, &output).unwrap())
        };
        assert_eq!(ordered(group::Owner::Point, "marked_points"), Some(vec![1, 3, 4]));
        assert_eq!(ordered(group::Owner::Vertex, "marked_vertices"), Some(vec![1, 2, 3]));
        assert_eq!(ordered(group::Owner::Primitive, "marked_curve"), Some(vec![0, 1]));
        assert_eq!(text(attribute::Owner::Detail, "author"), owned(&["polycut"]));
    }

    #[test]
    fn point_remove_and_cut() {
        let source = open_curve(5);
        let middle = point_group("middle", 5, &|point| point == 2);
        let removed = cut_of(&Params { cut_points: Some(middle.clone()), ..params() }, &source);
        assert!(geometry::point_count(&removed) == 4 && geometry::primitive_count(&removed) == 2);
        assert_eq!(xs(&removed), vec![0.0, 1.0, 3.0, 4.0]);
        assert_eq!(primitive_points(&removed, 0), vec![0, 1]);
        assert_eq!(primitive_points(&removed, 1), vec![2, 3]);
        let cut_value = cut_of(
            &Params { strategy: Strategy::PolyCutCut, cut_points: Some(middle), ..params() },
            &source,
        );
        assert!(geometry::point_count(&cut_value) == 6 && geometry::primitive_count(&cut_value) == 2);
        let (a, b) = (primitive_points(&cut_value, 0), primitive_points(&cut_value, 1));
        assert!(a == vec![0, 1, 2] && b.len() == 3 && b[0] != 2 && b[1] == 3 && b[2] == 4);
        let endpoint = point_group("endpoint", 5, &|point| point == 0);
        let identity = cut_of(
            &Params { strategy: Strategy::PolyCutCut, cut_points: Some(endpoint), ..params() },
            &source,
        );
        assert_eq!(geometry::data_id(&identity), geometry::data_id(&source), "endpoint-only no-op keeps identity");
    }

    #[test]
    fn closed_policy_and_restrictions() {
        let square = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)];
        let source = polyline(&square, true);
        let index = topology_index::create(None, geometry::topology(&source)).unwrap();
        let one = edge_group::init(None, geometry::topology(&source), &index, "one", &|edge| edge == 0).unwrap();
        let edges = |keep_closed| Params {
            element: Element::PolyCutEdges,
            cut_edges: Some(one.clone()),
            keep_closed,
            grain: None,
            ..params()
        };
        let open_result = cut_of(&edges(false), &source);
        let closed_result = cut_of(&edges(true), &source);
        assert_eq!(geometry::primitive_count(&open_result), 1);
        assert_eq!(view(&open_result).primitive_kinds[0], 1);
        assert_eq!(geometry::primitive_count(&closed_result), 1);
        assert_eq!(view(&closed_result).primitive_kinds[0], 2);
        // A filled polygon keeps its polygon kind for a closed fragment.
        let positions = packed::Float3::of_owned(
            square.iter().map(|p| p.0).collect(),
            square.iter().map(|p| p.1).collect(),
            square.iter().map(|p| p.2).collect(),
        )
        .unwrap();
        let face = geometry::create(
            positions,
            topology::polygons_owned(4, vec![0, 1, 2, 3], vec![0, 4]).unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        let face_index = topology_index::create(None, geometry::topology(&face)).unwrap();
        let face_edge = edge_group::init(None, geometry::topology(&face), &face_index, "face_edge", &|edge| edge == 0).unwrap();
        let face_cut = cut_of(
            &Params { element: Element::PolyCutEdges, cut_edges: Some(face_edge), grain: None, ..params() },
            &face,
        );
        assert_eq!(view(&face_cut).primitive_kinds[0], 0);
    }

    #[test]
    fn validation_cancellation_and_domain_stable_diagnostics() {
        let source = crossing_source();
        expect_invalid(cut(&Params { grain: Some(0), ..params() }, &source), "zero grain");
        expect_invalid(
            cut(&Params { detection: crossing("missing", 0.0), ..params() }, &source),
            "missing cut attribute",
        );
        expect_invalid(cut(&Params { detection: crossing("P", 0.0), ..params() }, &source), "tuple crossing");
        expect_invalid(
            cut(
                &edges_cut(Detection::PolyCutChange { attribute: "signal".to_owned(), threshold: 0.0 }),
                &source,
            ),
            "zero cut-at-change threshold",
        );
        let overflow = with_float("signal", vec![-f64::MAX, f64::MAX], &open_curve(2));
        expect_invalid(
            cut(
                &Params {
                    element: Element::PolyCutEdges,
                    strategy: Strategy::PolyCutCut,
                    detection: Detection::PolyCutChange { attribute: "signal".to_owned(), threshold: 1.0 },
                    ..params()
                },
                &overflow,
            ),
            "unrepresentable change",
        );
        let wrong_points = group::init(None, group::Owner::Primitive, "wrong", geometry::primitive_count(&source), &|_| true).unwrap();
        expect_invalid(cut(&Params { cut_points: Some(wrong_points), ..params() }, &source), "wrong point-selection owner");
        let wrong_owner = point_group("wrong_owner", geometry::point_count(&source), &|_| true);
        expect_invalid(cut(&Params { primitives: Some(wrong_owner), ..params() }, &source), "wrong primitive-selection owner");
        let wrong_length = group::init(None, group::Owner::Primitive, "wrong_length", 2, &|_| true).unwrap();
        expect_invalid(cut(&Params { primitives: Some(wrong_length), ..params() }, &source), "wrong primitive-selection length");
        let other = open_curve(5);
        let other_index = topology_index::create(None, geometry::topology(&other)).unwrap();
        let wrong_edges = edge_group::init(None, geometry::topology(&other), &other_index, "wrong", &|_| true).unwrap();
        expect_invalid(
            cut(&Params { element: Element::PolyCutEdges, cut_edges: Some(wrong_edges), ..params() }, &source),
            "wrong edge affinity",
        );
        let malformed = with_float("signal", vec![-1.0, f64::NAN, 1.0, f64::INFINITY, 1.0], &source);
        let message_at = |domains: i64| -> String {
            let output = Mutex::new(String::new());
            parallel::run_unit(Some(domains), &|| {
                let result = cut(
                    &Params {
                        element: Element::PolyCutEdges,
                        detection: crossing("signal", 0.0),
                        ..params()
                    },
                    &malformed,
                );
                match result {
                    Err(failure) => *output.lock().unwrap() = error::message(&failure).to_owned(),
                    Ok(_) => return Err("accepted malformed operated field".to_owned()),
                }
                Ok(())
            })
            .unwrap();
            output.into_inner().unwrap()
        };
        let (one, four) = (message_at(1), message_at(4));
        assert_eq!(one, four);
        assert!(one.ends_with("point 0"), "{one}");
        let none = group::init(None, group::Owner::Primitive, "none", 1, &|_| false).unwrap();
        let opaque = cut_of(
            &Params {
                primitives: Some(none),
                element: Element::PolyCutEdges,
                detection: crossing("signal", 0.0),
                ..params()
            },
            &malformed,
        );
        assert_eq!(geometry::data_id(&opaque), geometry::data_id(&malformed), "unselected values are opaque");
        let no_points = point_group("no_points", 5, &|_| false);
        let opaque_points = cut_of(
            &Params { cut_points: Some(no_points), detection: crossing("signal", 0.0), ..params() },
            &malformed,
        );
        assert_eq!(geometry::data_id(&opaque_points), geometry::data_id(&malformed));
        let malformed_index = topology_index::create(None, geometry::topology(&malformed)).unwrap();
        let no_edges = edge_group::init(None, geometry::topology(&malformed), &malformed_index, "no_edges", &|_| false).unwrap();
        let opaque_edges = cut_of(
            &Params {
                cut_edges: Some(no_edges),
                detection: crossing("signal", 0.0),
                ..edges_cut(crossing("signal", 0.0))
            },
            &malformed,
        );
        assert_eq!(geometry::data_id(&opaque_edges), geometry::data_id(&malformed));
        let bad_base = with_float("signal", vec![-1.0, 1.0], &open_curve(2));
        let bad_positions = geometry::create(
            packed::Float3::of_owned(vec![f64::NAN, 1.0], vec![0.0, 0.0], vec![0.0, 0.0]).unwrap(),
            geometry::topology(&bad_base).clone(),
            Some(geometry::attributes(&bad_base)),
            None,
            None,
        )
        .unwrap();
        expect_invalid(cut(&edges_cut(crossing("signal", 0.0)), &bad_positions), "non-finite interpolated position");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let (code, _) = message(cut(&Params { cancel: Some(token), ..params() }, &source));
        assert_eq!(code, "cancelled");
    }

    #[test]
    fn one_and_four_domains_cut_identically() {
        let points: Vec<(f64, f64, f64)> = (0..400).map(|p| (p as f64, (p % 7) as f64, 0.0)).collect();
        let source = with_float("signal", (0..400).map(|p| ((p * 37) % 11) as f64 - 5.0).collect(), &polyline(&points, false));
        let run = |domains: i64| -> (Vec<f64>, Vec<i64>, Vec<i64>) {
            let output = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                let result = cut(&Params { grain: Some(16), ..edges_cut(crossing("signal", 0.0)) }, &source)
                    .map_err(|failure| error::to_string(&failure))?;
                let v = view(&result);
                *output.lock().unwrap() = Some((xs(&result), v.vertex_points.to_vec(), v.primitive_offsets.to_vec()));
                Ok(())
            })
            .unwrap();
            output.into_inner().unwrap().unwrap()
        };
        let (one, four) = (run(1), run(4));
        assert!(one.2.len() > 2);
        assert_eq!(one, four);
    }
}
