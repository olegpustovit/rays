//! Polygon-curve cutting preserves invalid_poly_cut failures and cancelled errors.
use rays_math::float::max as omax;
use std::sync::Arc;

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, support, topology, topology_index,
    topology_remap,
};
use rdk_core::error::Code;
use rdk_core::support::Windows;

use crate::curve_join;

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
    error::new("Error.Fail", Code::InvalidParameter, message)
}

/// Start offset of the `grain`-sized range `range` over `count` items whose output begins at `offsets[item]`.
fn range_start(offsets: &[i64], grain: i64, count: i64, range: i64) -> usize {
    offsets[range.saturating_mul(grain).min(count) as usize] as usize
}

fn kind_of(code: u8) -> topology::PrimitiveKind {
    match code {
        0 => topology::PrimitiveKind::Polygon,
        1 => topology::PrimitiveKind::OpenPolyline,
        _ => topology::PrimitiveKind::ClosedPolyline,
    }
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
    Tuple2(packed::float2::view::View),
    Tuple3(packed::float3::view::View),
    Tuple4(packed::float4::view::View),
}

// ponytail: scalar attribute payload is copied once per cut; borrow it if a profile shows this matters.
fn numeric_attribute(name: &str, value: &geometry::T) -> Result<Numeric, error::T> {
    if name.trim().is_empty() {
        return Err(fail("cut attribute name must not be empty".to_owned()));
    }
    if name == "P" {
        return Ok(Numeric::Tuple3(packed::float3::view::view(geometry::positions(value))?));
    }
    let Some(item) = geometry::find_attribute(attribute::Owner::Point, name, value) else {
        return Err(fail(format!("point cut attribute {} was not found", rdk_core::attribute::quote_bytes((name).as_ref()))));
    };
    Ok(match attribute::view::storage(&item)? {
        attribute::Storage::Float(values) => Numeric::ScalarFloat(values.to_vec()),
        attribute::Storage::Int(values) => Numeric::ScalarInt(values.to_vec()),
        attribute::Storage::Float2(values) => Numeric::Tuple2(packed::float2::view::view(values)),
        attribute::Storage::Float3(values) => Numeric::Tuple3(packed::float3::view::view(values)?),
        attribute::Storage::Float4(values) => Numeric::Tuple4(packed::float4::view::view(values)),
        _ => {
            return Err(fail(format!(
                "point cut attribute {} must have numeric scalar or tuple storage, not {}",
                rdk_core::attribute::quote_bytes((name).as_ref()),
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
    let target = topology_index::view::view(&target_index);
    let edge_count = topology_index::edge_count(&target_index);
    let mut source_of_target = vec![-1_i64; edge_count as usize];
    let mut conflict: Option<i64> = None;
    for vertex in 0..source_edge_of_vertex.len() {
        cancel::poll(cancel_value, vertex as i64, 4095)?;
        let source_edge = source_edge_of_vertex[vertex];
        let target_edge = rdk_core::topology_index::widen(target.edge_of_vertex[vertex]);
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
                rdk_core::attribute::quote_bytes((edge_group::name(item)).as_ref())
            )));
        }
        let byte_count = (edge_count + 7) / 8;
        let mut bits = vec![0_u8; byte_count as usize];
        if byte_count > 0 {
            let chunk = (grain / 8).max(1);
            let windows = Windows::even(&mut bits, chunk);
            support::run_indexed_ranges(chunk, byte_count, &|range, first, last| {
                for (byte, slot) in (first..last).zip(windows.take(range)) {
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
                    *slot = value;
                }
                Ok(())
            })?;
        }
        output.push(edge_group::view::of_owned_bits(
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
    view: topology::view::View,
    index: topology_index::view::View,
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
    let mut fragment_counts = vec![0_i64; primitive_count as usize];
    support::par_fill(cancel_value, chunk, &mut fragment_counts, &|primitive| {
        let pu = primitive as usize;
        let first = i64::from(topology.primitive_offsets[pu]);
        let last = i64::from(topology.primitive_offsets[pu + 1]);
        if primitive_action[pu] == 0 {
            return Ok(1);
        }
        let mut fragments = 0;
        if topology.primitive_kinds[pu] == OPEN_POLYLINE {
            let mut in_run = false;
            for vertex in first..last {
                if rdk_core::topology_index::widen(index.next_vertex[vertex as usize]) >= 0 && edge_parts[vertex as usize] > 0 {
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
                    let previous = rdk_core::topology_index::widen(index.previous_vertex[vertex as usize]);
                    if previous < 0 || edge_parts[previous as usize] == 0 {
                        fragments += 1;
                    }
                }
            }
        }
        Ok(fragments)
    })?;
    let mut primitive_bases = vec![0_i64; primitive_count as usize + 1];
    for primitive in 0..primitive_count as usize {
        let count = fragment_counts[primitive];
        if primitive_bases[primitive].checked_add(count).is_none() {
            return Err(fail("output primitive cardinality exceeds array limits".to_owned()));
        }
        primitive_bases[primitive + 1] = primitive_bases[primitive] + count;
    }
    let output_primitive_count = primitive_bases[primitive_count as usize];
    let opc = output_primitive_count as usize;
    let (mut fragment_start, mut fragment_edges) = (vec![-1_i64; opc], vec![-1_i64; opc]);
    let (mut primitive_map, mut fragment_sizes) = (vec![0_i64; opc], vec![0_i64; opc]);
    {
        let bound = |range: i64| range_start(&primitive_bases, chunk, primitive_count, range);
        let ranges = support::range_count(primitive_count, chunk);
        let (w_start, w_edges) = (
            Windows::new(&mut fragment_start, ranges, bound),
            Windows::new(&mut fragment_edges, ranges, bound),
        );
        let (w_map, w_sizes) = (
            Windows::new(&mut primitive_map, ranges, bound),
            Windows::new(&mut fragment_sizes, ranges, bound),
        );
        support::run_indexed_ranges(chunk, primitive_count, &|range, first_primitive, last_primitive| {
            let (starts, edges) = (w_start.take(range), w_edges.take(range));
            let (maps, sizes) = (w_map.take(range), w_sizes.take(range));
            let base = bound(range) as i64;
            for primitive in first_primitive..last_primitive {
                support::poll(cancel_value, primitive)?;
                let pu = primitive as usize;
                let target_base = primitive_bases[pu];
                let first = i64::from(topology.primitive_offsets[pu]);
                let last = i64::from(topology.primitive_offsets[pu + 1]);
                if primitive_action[pu] == 0 {
                    maps[(target_base - base) as usize] = primitive;
                    sizes[(target_base - base) as usize] = last - first;
                    continue;
                }
                let mut state = RunState { target: target_base, active: false, edge_count: 0 };
                let mut begin = |state: &mut RunState, vertex: i64| {
                    state.active = true;
                    state.edge_count = 0;
                    starts[(state.target - base) as usize] = vertex;
                    maps[(state.target - base) as usize] = primitive;
                };
                let mut finish = |state: &mut RunState| {
                    if state.active {
                        edges[(state.target - base) as usize] = state.edge_count;
                        sizes[(state.target - base) as usize] = state.edge_count + 1;
                        state.target += 1;
                        state.active = false;
                    }
                };
                if topology.primitive_kinds[pu] == OPEN_POLYLINE {
                    for vertex in first..last {
                        if rdk_core::topology_index::widen(index.next_vertex[vertex as usize]) >= 0 && edge_parts[vertex as usize] > 0 {
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
                        let previous = rdk_core::topology_index::widen(index.previous_vertex[vertex as usize]);
                        if start < 0
                            && edge_parts[vertex as usize] > 0
                            && (previous < 0 || edge_parts[previous as usize] == 0)
                        {
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
            }
            Ok(())
        })?;
    }
    let mut primitive_offsets = vec![0_i64; opc + 1];
    for primitive in 0..opc {
        let size = fragment_sizes[primitive];
        if primitive_offsets[primitive].checked_add(size).is_none() {
            return Err(fail("output corner cardinality exceeds array limits".to_owned()));
        }
        primitive_offsets[primitive + 1] = primitive_offsets[primitive] + size;
    }
    let output_vertex_count = primitive_offsets[opc];
    let ovc = output_vertex_count as usize;
    let mut vertex_points = vec![0_i64; ovc];
    let mut vertex_map = vec![0_i64; ovc];
    let mut primitive_kinds = vec![OPEN_POLYLINE; opc];
    let mut source_edge_of_vertex = vec![-1_i64; ovc];
    {
        let target_grain = (grain / 8).max(1);
        let bound = |range: i64| range_start(&primitive_offsets, target_grain, output_primitive_count, range);
        let ranges = support::range_count(output_primitive_count, target_grain);
        let w_kinds = Windows::even(&mut primitive_kinds, target_grain);
        let (w_points, w_map, w_edge) = (
            Windows::new(&mut vertex_points, ranges, bound),
            Windows::new(&mut vertex_map, ranges, bound),
            Windows::new(&mut source_edge_of_vertex, ranges, bound),
        );
        support::run_indexed_ranges(target_grain, output_primitive_count, &|range, first_target, last_target| {
            let (kinds, points) = (w_kinds.take(range), w_points.take(range));
            let (maps, edge_of_vertex) = (w_map.take(range), w_edge.take(range));
            let base = bound(range) as i64;
            for target in first_target..last_target {
                support::poll(cancel_value, target)?;
                let tu = target as usize;
                let primitive = primitive_map[tu];
                let pu = primitive as usize;
                let output = primitive_offsets[tu] - base;
                let source_first = i64::from(topology.primitive_offsets[pu]);
                let source_last = i64::from(topology.primitive_offsets[pu + 1]);
                if fragment_start[tu] < 0 {
                    kinds[(target - first_target) as usize] = topology.primitive_kinds[pu];
                    for vertex in source_first..source_last {
                        let target_vertex = (output + vertex - source_first) as usize;
                        points[target_vertex] = i64::from(topology.vertex_points[vertex as usize]);
                        maps[target_vertex] = vertex;
                        edge_of_vertex[target_vertex] = rdk_core::topology_index::widen(index.edge_of_vertex[vertex as usize]);
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
                        let slot = (output + local) as usize;
                        points[slot] = i64::from(topology.vertex_points[source_vertex as usize]);
                        maps[slot] = source_vertex;
                        if local < edges {
                            edge_of_vertex[slot] = rdk_core::topology_index::widen(index.edge_of_vertex[source_vertex as usize]);
                        }
                    }
                    let size = edges + 1;
                    kinds[(target - first_target) as usize] = if source_kind != OPEN_POLYLINE && keep_closed && size >= 3 {
                        source_kind
                    } else {
                        OPEN_POLYLINE
                    };
                }
            }
            Ok(())
        })?;
    }
    let output_topology = topology::create_owned(
        geometry::point_count(value),
        topology::narrow("Poly_cut", &vertex_points)?,
        topology::narrow("Poly_cut", &primitive_offsets)?,
        primitive_kinds.into_iter().map(kind_of).collect(),
    )?;
    remap_all(
        cancel_value,
        grain,
        value,
        source_index,
        &output_topology,
        &vertex_map,
        &primitive_map,
        &source_edge_of_vertex,
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
/// The slices are one range's windows; `output` indexes into them.
struct Samples<'a> {
    point_left: &'a mut [i64],
    point_right: &'a mut [i64],
    vertex_left: &'a mut [i64],
    vertex_right: &'a mut [i64],
    weights: &'a mut [f64],
    generated: &'a mut [u8],
}

impl Samples<'_> {
    #[allow(clippy::too_many_arguments)]
    fn write_sample(
        &mut self,
        source: &Source,
        edge_parts: &[i64],
        edge_cut_weight: &[f64],
        output: usize,
        source_vertex: i64,
        part: i64,
        at_end: bool,
        clone: bool,
    ) {
        let sv = source_vertex as usize;
        let next = rdk_core::topology_index::widen(source.index.next_vertex[sv]);
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
        let left_point = i64::from(source.view.vertex_points[sv]);
        let right_point = i64::from(source.view.vertex_points[next as usize]);
        if weight <= 0.0 {
            self.point_left[output] = left_point;
            self.point_right[output] = left_point;
            self.vertex_left[output] = source_vertex;
            self.vertex_right[output] = source_vertex;
            self.weights[output] = 0.0;
        } else if weight >= 1.0 {
            self.point_left[output] = right_point;
            self.point_right[output] = right_point;
            self.vertex_left[output] = next;
            self.vertex_right[output] = next;
            self.weights[output] = 0.0;
        } else {
            self.point_left[output] = left_point;
            self.point_right[output] = right_point;
            self.vertex_left[output] = source_vertex;
            self.vertex_right[output] = next;
            self.weights[output] = weight;
        }
        if clone || (weight > 0.0 && weight < 1.0) {
            self.generated[output] = 1;
        }
    }
}

fn run_cut(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = support::resolve(params.grain).map_err(|_| fail("grain must be positive".to_owned()))?;
    let (element, strategy, keep_closed) = (params.element, params.strategy, params.keep_closed);
    let primitives = params.primitives.as_ref();
    let cut_points = params.cut_points.as_ref();
    let cut_edges = params.cut_edges.as_ref();
    support::check_positive(grain).map_err(|_| fail("grain must be positive".to_owned()))?;
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(value);
    let view = topology::view::view(topology_value)?;
    let source_index = topology_index::create(cancel_value, topology_value)?;
    let source = Source { view, index: topology_index::view::view(&source_index) };
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
    let (mut edge_parts, mut edge_cut_weight) = (vec![0_i64; vc], vec![-1.0_f64; vc]);
    let (mut endpoint_bits, mut invalid_edges) = (vec![0_u8; vc], vec![0_u8; vc]);
    let blocks = if vertex_count == 0 { 0 } else { 1 + (vertex_count - 1) / grain };
    let (mut error_vertices, mut error_kinds) = (vec![-1_i64; blocks as usize], vec![0_u8; blocks as usize]);
    if blocks > 0 {
        let (w_parts, w_weight) = (Windows::even(&mut edge_parts, grain), Windows::even(&mut edge_cut_weight, grain));
        let (w_endpoint, w_invalid) = (Windows::even(&mut endpoint_bits, grain), Windows::even(&mut invalid_edges, grain));
        let (w_error_vertex, w_error_kind) = (Windows::even(&mut error_vertices, 1), Windows::even(&mut error_kinds, 1));
        support::run_indexed_ranges(grain, vertex_count, &|block, first, last| {
            let (parts_out, weight_out) = (w_parts.take(block), w_weight.take(block));
            let (endpoint_out, invalid_out) = (w_endpoint.take(block), w_invalid.take(block));
            cancel::check_opt(cancel_value)?;
            let mut first_error = -1_i64;
            let mut first_kind = 0_u8;
            for vertex in first..last {
                cancel::poll(cancel_value, vertex, 4095)?;
                let slot = (vertex - first) as usize;
                let vu = vertex as usize;
                let next = rdk_core::topology_index::widen(index.next_vertex[vu]);
                if next < 0 {
                    continue;
                }
                let primitive = i64::from(index.primitive_of_vertex[vu]);
                let edge = rdk_core::topology_index::widen(index.edge_of_vertex[vu]);
                let excluded = !selected(primitives, primitive)
                    || match cut_edges {
                        Some(selection) if element == Element::PolyCutEdges => !edge_group::mem(edge, selection),
                        _ => false,
                    };
                if excluded {
                    parts_out[slot] = 1;
                    continue;
                }
                let left = i64::from(topology.vertex_points[vu]);
                let right = i64::from(topology.vertex_points[next as usize]);
                let eligible = match (element, cut_points) {
                    (Element::PolyCutPoints, Some(selection)) => {
                        group::mem(left, selection) || group::mem(right, selection)
                    }
                    _ => true,
                };
                if !eligible {
                    parts_out[slot] = 1;
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
                                if !ratio.is_finite() || ratio > i64::MAX as f64 {
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
                    invalid_out[slot] = 1;
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
                    parts_out[slot] = output_parts;
                    weight_out[slot] = cut_weight;
                    endpoint_out[slot] = endpoint;
                }
            }
            if first_error >= 0 {
                w_error_vertex.take(block)[0] = first_error;
                w_error_kind.take(block)[0] = first_kind;
            }
            Ok(())
        })?;
    }
    if let Some(block) = error_vertices.iter().position(|&vertex| vertex >= 0) {
        let vertex = error_vertices[block];
        let point = i64::from(topology.vertex_points[vertex as usize]);
        let message = match error_kinds[block] {
            1 => format!("cut attribute is non-finite at operated point {point}"),
            2 => format!("attribute change is not representable on edge from vertex {vertex}"),
            _ => format!("cut subdivision cardinality exceeds array limits at vertex {vertex}"),
        };
        return Err(fail(message));
    }
    let mut vertex_break = vec![0_u8; vc];
    support::par_fill(cancel_value, grain, &mut vertex_break, &|vertex| {
        let vu = vertex as usize;
        let next = rdk_core::topology_index::widen(index.next_vertex[vu]);
        let previous = rdk_core::topology_index::widen(index.previous_vertex[vu]);
        let mut breaks = false;
        if next >= 0 || previous >= 0 {
            match element {
                Element::PolyCutPoints => {
                    let point = i64::from(topology.vertex_points[vu]);
                    if selected(cut_points, point) {
                        let outgoing = next >= 0 && invalid_edges[vu] != 0;
                        let incoming = previous >= 0 && invalid_edges[previous as usize] != 0;
                        breaks = outgoing || incoming;
                    }
                }
                Element::PolyCutEdges => {
                    let outgoing = next >= 0 && endpoint_bits[vu] & 1 != 0;
                    let incoming = previous >= 0 && endpoint_bits[previous as usize] & 2 != 0;
                    breaks = outgoing || incoming;
                }
            }
        }
        Ok(u8::from(breaks))
    })?;
    if element == Element::PolyCutPoints {
        let mut updated = vec![0_i64; vc];
        support::par_fill(cancel_value, grain, &mut updated, &|vertex| {
            let vu = vertex as usize;
            let next = rdk_core::topology_index::widen(index.next_vertex[vu]);
            Ok(if next >= 0 {
                match strategy {
                    Strategy::PolyCutCut => 1,
                    Strategy::PolyCutRemove => {
                        if vertex_break[vu] != 0 || vertex_break[next as usize] != 0 { 0 } else { 1 }
                    }
                }
            } else {
                edge_parts[vu]
            })
        })?;
        edge_parts = updated;
    }
    let mut primitive_action = vec![0_u8; primitive_count as usize];
    let chunk = (grain / 256).max(1);
    support::par_fill(cancel_value, chunk, &mut primitive_action, &|primitive| {
        let mut changed = false;
        if selected(primitives, primitive) {
            let first = i64::from(topology.primitive_offsets[primitive as usize]);
            let last = i64::from(topology.primitive_offsets[primitive as usize + 1]);
            for vertex in first..last {
                if rdk_core::topology_index::widen(index.next_vertex[vertex as usize]) >= 0 && edge_parts[vertex as usize] != 1 {
                    changed = true;
                }
            }
            if !changed {
                for vertex in first..last {
                    let previous = rdk_core::topology_index::widen(index.previous_vertex[vertex as usize]);
                    let next = rdk_core::topology_index::widen(index.next_vertex[vertex as usize]);
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
        }
        Ok(u8::from(changed))
    })?;
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
        let first = i64::from(topology.primitive_offsets[primitive]);
        let last = i64::from(topology.primitive_offsets[primitive + 1]);
        let mut count = 0_i64;
        for vertex in first..last {
            let parts = edge_parts[vertex as usize];
            if count.checked_add(parts).is_none() {
                return Err(fail("expanded cut-segment cardinality exceeds array limits".to_owned()));
            }
            count += parts;
        }
        if primitive_segment_offsets[primitive].checked_add(count).is_none() {
            return Err(fail("expanded cut-segment cardinality exceeds array limits".to_owned()));
        }
        primitive_segment_offsets[primitive + 1] = primitive_segment_offsets[primitive] + count;
    }
    let segment_count = primitive_segment_offsets[pc];
    let sc = segment_count as usize;
    let (mut segment_vertex, mut segment_part) = (vec![0_i64; sc], vec![0_i64; sc]);
    let mut segment_boundary = vec![0_u8; sc];
    {
        let bound = |range: i64| range_start(&primitive_segment_offsets, chunk, primitive_count, range);
        let ranges = support::range_count(primitive_count, chunk);
        let (w_vertex, w_part) =
            (Windows::new(&mut segment_vertex, ranges, bound), Windows::new(&mut segment_part, ranges, bound));
        let w_boundary = Windows::new(&mut segment_boundary, ranges, bound);
        support::run_indexed_ranges(chunk, primitive_count, &|range, first_primitive, last_primitive| {
            let (vertices_out, parts_out) = (w_vertex.take(range), w_part.take(range));
            let boundary_out = w_boundary.take(range);
            let base = bound(range) as i64;
            for primitive in first_primitive..last_primitive {
                support::poll(cancel_value, primitive)?;
                let pu = primitive as usize;
                let first = i64::from(topology.primitive_offsets[pu]);
                let last = i64::from(topology.primitive_offsets[pu + 1]);
                let mut output = primitive_segment_offsets[pu] - base;
                let closed = topology.primitive_kinds[pu] != OPEN_POLYLINE;
                for vertex in first..last {
                    let parts = edge_parts[vertex as usize];
                    for part in 0..parts {
                        vertices_out[output as usize] = vertex;
                        parts_out[output as usize] = part;
                        boundary_out[output as usize] = if part > 0 {
                            2
                        } else if !closed && vertex == first {
                            1
                        } else {
                            let previous = rdk_core::topology_index::widen(index.previous_vertex[vertex as usize]);
                            if previous < 0 || edge_parts[previous as usize] == 0 {
                                1
                            } else if vertex_break[vertex as usize] != 0 {
                                2
                            } else {
                                0
                            }
                        };
                        output += 1;
                    }
                }
            }
            Ok(())
        })?;
    }
    let mut fragment_counts = vec![0_i64; pc];
    support::par_fill(cancel_value, chunk, &mut fragment_counts, &|primitive| {
        let pu = primitive as usize;
        if primitive_action[pu] == 0 {
            return Ok(1);
        }
        let first = primitive_segment_offsets[pu];
        let last = primitive_segment_offsets[pu + 1];
        if last - first == 0 {
            return Ok(0);
        }
        let mut boundaries = 0_i64;
        for segment in first..last {
            if segment_boundary[segment as usize] != 0 {
                boundaries += 1;
            }
        }
        let closed = topology.primitive_kinds[pu] != OPEN_POLYLINE;
        Ok(if closed && boundaries == 0 { 1 } else { boundaries })
    })?;
    let mut primitive_bases = vec![0_i64; pc + 1];
    for primitive in 0..pc {
        let count = fragment_counts[primitive];
        if primitive_bases[primitive].checked_add(count).is_none() {
            return Err(fail("output primitive cardinality exceeds array limits".to_owned()));
        }
        primitive_bases[primitive + 1] = primitive_bases[primitive] + count;
    }
    let output_primitive_count = primitive_bases[pc];
    let opc = output_primitive_count as usize;
    let (mut fragment_first, mut fragment_segments) = (vec![-1_i64; opc], vec![-1_i64; opc]);
    let (mut primitive_map, mut fragment_sizes) = (vec![0_i64; opc], vec![0_i64; opc]);
    {
        let bound = |range: i64| range_start(&primitive_bases, chunk, primitive_count, range);
        let ranges = support::range_count(primitive_count, chunk);
        let (w_first, w_segments) = (
            Windows::new(&mut fragment_first, ranges, bound),
            Windows::new(&mut fragment_segments, ranges, bound),
        );
        let (w_map, w_sizes) = (
            Windows::new(&mut primitive_map, ranges, bound),
            Windows::new(&mut fragment_sizes, ranges, bound),
        );
        support::run_indexed_ranges(chunk, primitive_count, &|range, first_primitive, last_primitive| {
            let (firsts, segments_out) = (w_first.take(range), w_segments.take(range));
            let (maps, sizes) = (w_map.take(range), w_sizes.take(range));
            let base = bound(range) as i64;
            for primitive in first_primitive..last_primitive {
                support::poll(cancel_value, primitive)?;
                let pu = primitive as usize;
                let target_base = primitive_bases[pu];
                let source_size = i64::from(topology.primitive_offsets[pu + 1]) - i64::from(topology.primitive_offsets[pu]);
                if primitive_action[pu] == 0 {
                    maps[(target_base - base) as usize] = primitive;
                    sizes[(target_base - base) as usize] = source_size;
                    continue;
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
                        maps[(target_base - base) as usize] = primitive;
                        sizes[(target_base - base) as usize] = source_size;
                    } else {
                        let start = if first_boundary >= 0 { first_boundary } else { segment_base };
                        let mut target = target_base - 1;
                        let mut current_count = 0_i64;
                        for offset in 0..segments {
                            let segment = segment_base + (start - segment_base + offset) % segments;
                            if segment_boundary[segment as usize] != 0 {
                                if target >= target_base {
                                    segments_out[(target - base) as usize] = current_count;
                                    sizes[(target - base) as usize] = current_count + 1;
                                }
                                target += 1;
                                current_count = 0;
                                firsts[(target - base) as usize] = segment;
                                maps[(target - base) as usize] = primitive;
                            }
                            current_count += 1;
                        }
                        if target >= target_base {
                            segments_out[(target - base) as usize] = current_count;
                            sizes[(target - base) as usize] = current_count + 1;
                        }
                    }
                }
            }
            Ok(())
        })?;
    }
    let mut primitive_offsets = vec![0_i64; opc + 1];
    for primitive in 0..opc {
        let size = fragment_sizes[primitive];
        if primitive_offsets[primitive].checked_add(size).is_none() {
            return Err(fail("output corner cardinality exceeds array limits".to_owned()));
        }
        primitive_offsets[primitive + 1] = primitive_offsets[primitive] + size;
    }
    let output_vertex_count = primitive_offsets[opc];
    let ovc = output_vertex_count as usize;
    let mut primitive_kinds = vec![OPEN_POLYLINE; opc];
    let (mut point_left, mut point_right) = (vec![0_i64; ovc], vec![0_i64; ovc]);
    let (mut vertex_left, mut vertex_right) = (vec![0_i64; ovc], vec![0_i64; ovc]);
    let (mut weights, mut generated) = (vec![0.0_f64; ovc], vec![0_u8; ovc]);
    let mut source_edge_of_vertex = vec![-1_i64; ovc];
    {
        let target_grain = (grain / 8).max(1);
        let bound = |range: i64| range_start(&primitive_offsets, target_grain, output_primitive_count, range);
        let ranges = support::range_count(output_primitive_count, target_grain);
        let w_kinds = Windows::even(&mut primitive_kinds, target_grain);
        let (w_point_left, w_point_right) = (
            Windows::new(&mut point_left, ranges, bound),
            Windows::new(&mut point_right, ranges, bound),
        );
        let (w_vertex_left, w_vertex_right) = (
            Windows::new(&mut vertex_left, ranges, bound),
            Windows::new(&mut vertex_right, ranges, bound),
        );
        let (w_weights, w_generated) =
            (Windows::new(&mut weights, ranges, bound), Windows::new(&mut generated, ranges, bound));
        let w_edge = Windows::new(&mut source_edge_of_vertex, ranges, bound);
        support::run_indexed_ranges(target_grain, output_primitive_count, &|range, first_target, last_target| {
            let kinds = w_kinds.take(range);
            let edge_of_vertex = w_edge.take(range);
            let mut samples = Samples {
                point_left: w_point_left.take(range),
                point_right: w_point_right.take(range),
                vertex_left: w_vertex_left.take(range),
                vertex_right: w_vertex_right.take(range),
                weights: w_weights.take(range),
                generated: w_generated.take(range),
            };
            let base = bound(range) as i64;
            for target in first_target..last_target {
                support::poll(cancel_value, target)?;
                let tu = target as usize;
                let kind_slot = (target - first_target) as usize;
                let primitive = primitive_map[tu];
                let pu = primitive as usize;
                let output_first = primitive_offsets[tu] - base;
                if fragment_first[tu] < 0 {
                    let source_first = i64::from(topology.primitive_offsets[pu]);
                    let source_last = i64::from(topology.primitive_offsets[pu + 1]);
                    kinds[kind_slot] = topology.primitive_kinds[pu];
                    for source_vertex in source_first..source_last {
                        let output = (output_first + source_vertex - source_first) as usize;
                        let point = i64::from(topology.vertex_points[source_vertex as usize]);
                        samples.point_left[output] = point;
                        samples.point_right[output] = point;
                        samples.vertex_left[output] = source_vertex;
                        samples.vertex_right[output] = source_vertex;
                        edge_of_vertex[output] = rdk_core::topology_index::widen(index.edge_of_vertex[source_vertex as usize]);
                    }
                } else {
                    let first_segment = fragment_first[tu];
                    let count = fragment_segments[tu];
                    let segment_base = primitive_segment_offsets[pu];
                    let total_segments = primitive_segment_offsets[pu + 1] - segment_base;
                    let first_source_vertex = segment_vertex[first_segment as usize];
                    let first_part = segment_part[first_segment as usize];
                    let clone = segment_boundary[first_segment as usize] == 2;
                    samples.write_sample(&source, &edge_parts, &edge_cut_weight, output_first as usize, first_source_vertex, first_part, false, clone);
                    for local in 0..count {
                        let segment = segment_base + (first_segment - segment_base + local) % total_segments;
                        let source_vertex = segment_vertex[segment as usize];
                        let part = segment_part[segment as usize];
                        let output = (output_first + local) as usize;
                        edge_of_vertex[output] = rdk_core::topology_index::widen(index.edge_of_vertex[source_vertex as usize]);
                        samples.write_sample(&source, &edge_parts, &edge_cut_weight, output + 1, source_vertex, part, true, false);
                    }
                    let size = count + 1;
                    let source_kind = topology.primitive_kinds[pu];
                    kinds[kind_slot] = if source_kind != OPEN_POLYLINE && keep_closed && size >= 3 {
                        source_kind
                    } else {
                        OPEN_POLYLINE
                    };
                }
            }
            Ok(())
        })?;
    }

    let pcount = point_count as usize;
    let mut source_point_used = vec![0_u8; pcount];
    let mut vertex_points = vec![0_i64; ovc];
    if element != Element::PolyCutPoints || strategy != Strategy::PolyCutRemove {
        source_point_used.fill(1);
    } else {
        for point in 0..pcount {
            if i64::from(index.point_offsets[point]) == i64::from(index.point_offsets[point + 1]) {
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
    if base_point_count.checked_add(generated_count).is_none() {
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
    let source_positions = packed::float3::view::view(geometry::positions(value))?;
    let (mut x, mut y, mut z) = (vec![0.0; opcount], vec![0.0; opcount], vec![0.0; opcount]);
    if output_point_count > 0 {
        let (wx, wy, wz) = (Windows::even(&mut x, grain), Windows::even(&mut y, grain), Windows::even(&mut z, grain));
        support::run_indexed_ranges(grain, output_point_count, &|block, first, last| {
            cancel::check_opt(cancel_value)?;
            let (xs, ys, zs) = (wx.take(block), wy.take(block), wz.take(block));
            for point in first..last {
                let (left, right) = (output_point_left[point as usize] as usize, output_point_right[point as usize] as usize);
                let weight = output_point_weight[point as usize];
                let at = (point - first) as usize;
                xs[at] = interpolate(source_positions.x[left], source_positions.x[right], weight);
                ys[at] = interpolate(source_positions.y[left], source_positions.y[right], weight);
                zs[at] = interpolate(source_positions.z[left], source_positions.z[right], weight);
            }
            Ok(())
        })?;
    }
    if let Some(point) = (0..opcount).find(|&point| !(x[point].is_finite() && y[point].is_finite() && z[point].is_finite())) {
        return Err(fail(format!("interpolated position is non-finite at output point {point}")));
    }
    let output_topology = topology::create_owned(
        output_point_count,
        topology::narrow("Poly_cut", &vertex_points)?,
        topology::narrow("Poly_cut", &primitive_offsets)?,
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
        packed::float3::view::of_owned_exn(x, y, z)?,
        output_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

/// Packed polygon-curve cutting kernel.
pub fn cut(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    run_cut(params, value).map_err(|failure| error::guard(failure, "poly_cut", Code::InvalidPolyCut))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::packed;
    use std::sync::Mutex;

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
        with_attribute(attribute::Owner::Point, name, attribute::Storage::Float(Arc::new(values)), value)
    }

    fn polyline(points: &[(f64, f64, f64)], closed: bool) -> geometry::T {
        let positions = packed::float3::of_owned(
            points.iter().map(|p| p.0).collect(),
            points.iter().map(|p| p.1).collect(),
            points.iter().map(|p| p.2).collect(),
        )
        .unwrap();
        let ids: Vec<u32> = (0..points.len() as u32).collect();
        let mut builder = topology::builder::create(points.len() as i64, None, None).unwrap();
        if closed {
            topology::builder::add_closed_polyline(&mut builder, &ids).unwrap();
        } else {
            topology::builder::add_open_polyline(&mut builder, &ids).unwrap();
        }
        let topo = topology::builder::freeze(&mut builder).unwrap();
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    fn open_curve(count: usize) -> geometry::T {
        let points: Vec<(f64, f64, f64)> = (0..count).map(|p| (p as f64, 0.0, 0.0)).collect();
        polyline(&points, false)
    }

    fn view(value: &geometry::T) -> topology::view::View {
        topology::view::view(geometry::topology(value)).unwrap()
    }

    fn xs(value: &geometry::T) -> Vec<f64> {
        packed::float3::view::view(geometry::positions(value)).unwrap().x.to_vec()
    }

    fn primitive_points(value: &geometry::T, primitive: usize) -> Vec<i64> {
        let v = view(value);
        v.vertex_points[v.primitive_offsets[primitive] as usize..v.primitive_offsets[primitive + 1] as usize].iter().map(|&p| i64::from(p)).collect()
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
        group::init(None, group::Owner::Point, name, count, &|index| Ok(member(index))).unwrap()
    }

    fn crossing_source() -> geometry::T {
        let curve = with_float("signal", vec![-1.0, 1.0, 2.0, -1.0, -2.0], &open_curve(5));
        let curve = with_float("weight", vec![0.0, 10.0, 20.0, 30.0, 40.0], &curve);
        let curve = with_attribute(attribute::Owner::Point, "id", attribute::Storage::Int(Arc::new(vec![0, 1, 2, 3, 4])), &curve);
        let curve = with_attribute(
            attribute::Owner::Vertex,
            "vertex_weight",
            attribute::Storage::Float(Arc::new(vec![0.0, 10.0, 20.0, 30.0, 40.0])),
            &curve,
        );
        with_attribute(attribute::Owner::Primitive, "piece", attribute::Storage::Int(Arc::new(vec![17])), &curve)
    }

    fn message(result: Result<geometry::T, error::T>) -> (String, String) {
        match result {
            Ok(_) => ("ok".to_owned(), String::new()),
            Err(failure) => (error::code(&failure).as_str().to_owned(), error::message(&failure).to_owned()),
        }
    }

    fn expect_invalid(result: Result<geometry::T, error::T>, what: &str) {
        assert_eq!(message(result).0, "invalid_poly_cut", "{what}");
    }

    #[test]
    fn change_subdivision_and_scale_normalised_crossing() {
        let delta = packed::float2::of_owned(vec![0.0, 3.0], vec![0.0, 4.0]).unwrap();
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
            attribute::Storage::Text(std::sync::Arc::new(vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec()].into())),
        ));
        attributes.push(mk(
            attribute::Owner::Point,
            "rows",
            attribute::Storage::FloatArray(
                packed::float_array::create_owned(vec![0, 1, 3, 4], vec![1.0, 2.0, 3.0, 4.0]).unwrap(),
            ),
        ));
        attributes.push(mk(
            attribute::Owner::Vertex,
            "corner_label",
            attribute::Storage::Text(std::sync::Arc::new(vec![b"va".to_vec(), b"vb".to_vec(), b"vc".to_vec()].into())),
        ));
        attributes.push(mk(
            attribute::Owner::Vertex,
            "links",
            attribute::Storage::IntArray(
                packed::int_array::create_owned(vec![0, 1, 3, 4], vec![10, 20, 21, 30]).unwrap(),
            ),
        ));
        attributes.push(mk(attribute::Owner::Detail, "author", attribute::Storage::Text(std::sync::Arc::new(vec![b"polycut".to_vec()].into()))));
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
            match attribute::view::storage(&item).unwrap() {
                attribute::Storage::Text(values) => (**values).clone(),
                _ => panic!("{name} storage"),
            }
        };
        let owned = |values: &[&str]| -> Vec<Vec<u8>> { values.iter().map(|v| v.as_bytes().to_vec()).collect() };
        assert_eq!(text(attribute::Owner::Point, "label"), owned(&["a", "b", "c", "b", "b"]));
        assert_eq!(text(attribute::Owner::Vertex, "corner_label"), owned(&["va", "vb", "vb", "vb", "vc"]));
        let rows = geometry::find_attribute(attribute::Owner::Point, "rows", &output).unwrap();
        let links = geometry::find_attribute(attribute::Owner::Vertex, "links", &output).unwrap();
        let (attribute::Storage::FloatArray(rows), attribute::Storage::IntArray(links)) =
            (attribute::view::storage(&rows).unwrap(), attribute::view::storage(&links).unwrap())
        else {
            panic!("ragged storage")
        };
        assert_eq!(packed::float_array::get(rows, 3).unwrap(), vec![2.0, 3.0]);
        assert_eq!(packed::float_array::get(rows, 4).unwrap(), vec![2.0, 3.0]);
        assert_eq!(packed::int_array::get(links, 1).unwrap(), vec![20, 21]);
        assert_eq!(packed::int_array::get(links, 2).unwrap(), vec![20, 21]);
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
        let one = edge_group::init(None, geometry::topology(&source), &index, "one", &|edge| Ok(edge == 0)).unwrap();
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
        let positions = packed::float3::of_owned(
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
        let face_edge = edge_group::init(None, geometry::topology(&face), &face_index, "face_edge", &|edge| Ok(edge == 0)).unwrap();
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
        let wrong_points = group::init(None, group::Owner::Primitive, "wrong", geometry::primitive_count(&source), &|_| Ok(true)).unwrap();
        expect_invalid(cut(&Params { cut_points: Some(wrong_points), ..params() }, &source), "wrong point-selection owner");
        let wrong_owner = point_group("wrong_owner", geometry::point_count(&source), &|_| true);
        expect_invalid(cut(&Params { primitives: Some(wrong_owner), ..params() }, &source), "wrong primitive-selection owner");
        let wrong_length = group::init(None, group::Owner::Primitive, "wrong_length", 2, &|_| Ok(true)).unwrap();
        expect_invalid(cut(&Params { primitives: Some(wrong_length), ..params() }, &source), "wrong primitive-selection length");
        let other = open_curve(5);
        let other_index = topology_index::create(None, geometry::topology(&other)).unwrap();
        let wrong_edges = edge_group::init(None, geometry::topology(&other), &other_index, "wrong", &|_| Ok(true)).unwrap();
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
        let none = group::init(None, group::Owner::Primitive, "none", 1, &|_| Ok(false)).unwrap();
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
        let no_edges = edge_group::init(None, geometry::topology(&malformed), &malformed_index, "no_edges", &|_| Ok(false)).unwrap();
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
            packed::float3::of_owned(vec![f64::NAN, 1.0], vec![0.0, 0.0], vec![0.0, 0.0]).unwrap(),
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
        let run = |domains: i64| -> (Vec<f64>, Vec<u32>, Vec<u32>) {
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
