//! Curve subdivision (Catmull-Clark and bilinear) over packed polygon curves.
//!
//! OCaml raises `Error.Fail`, `Invalid_argument` and `Cancel.Cancelled` out of `iterate`; here each
//! is an `error::T`: a failure has code `invalid_parameter` and the OCaml message, a cancellation
//! has code `cancelled`.
use std::sync::atomic::{AtomicBool, Ordering};

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, ragged_ops, support, topology,
    topology_index, topology_remap,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Scheme {
    CatmullClark,
    Bilinear,
}

pub struct Plan {
    pub source: geometry::T,
    pub output_topology: topology::T,
    pub output_index: Option<topology_index::T>,
    pub point_offsets: Vec<i64>,
    pub point_sources: Vec<i64>,
    pub point_weights: Vec<f64>,
    pub point_representative: Vec<i64>,
    pub point_old: Vec<bool>,
    pub vertex_left: Vec<i64>,
    pub vertex_right: Vec<i64>,
    pub vertex_weight: Vec<f64>,
    pub vertex_old: Vec<bool>,
    pub primitive_source: Vec<i64>,
    pub source_edge_of_output_vertex: Vec<i64>,
}

fn fail(message: &str) -> error::T {
    error::make(
        "Rdk_curve.Curve_subdivide".to_owned(),
        "invalid_parameter".to_owned(),
        message.to_owned(),
        None,
    )
    .unwrap_or_else(|bad| bad)
}

fn checked_add(name: &str, left: i64, right: i64) -> Result<i64, error::T> {
    if right < 0 || left > (1_i64 << 62) - 1 - right {
        return Err(fail(&format!("{name} exceeds integer limits")));
    }
    Ok(left + right)
}

fn grain_size(grain: Option<i64>) -> Result<i64, error::T> {
    let grain = grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(fail("Rdk_curve.Curve_subdivide.subdivide: grain must be positive"));
    }
    Ok(grain)
}

/// One float per index.
fn map_floats(
    grain: Option<i64>,
    cancel_value: Option<&cancel::T>,
    count: i64,
    value: &(dyn Fn(i64) -> f64 + Sync),
) -> Result<Vec<f64>, error::T> {
    let chunk = grain_size(grain)?;
    let mut output = vec![0.0; count.max(0) as usize];
    support::par_fill(cancel_value, chunk, &mut output, &|index| Ok(value(index)))?;
    Ok(output)
}

fn primitive_size(offsets: &[i64], primitive: usize) -> i64 {
    offsets[primitive + 1] - offsets[primitive]
}

fn validate(cancel_value: Option<&cancel::T>, value: &geometry::T) -> Result<(), error::T> {
    let view = topology::View::view(geometry::topology(value))?;
    if view.point_count == 0 || view.primitive_kinds.is_empty() {
        return Err(fail("polygon-curve input must contain at least one primitive"));
    }
    for primitive in 0..view.primitive_kinds.len() {
        cancel::poll(cancel_value, primitive as i64, 1023)?;
        let kind = view.primitive_kinds[primitive];
        if kind == 0 {
            return Err(fail("curve refinement received a polygon primitive"));
        } else if kind != 1 && kind != 2 {
            return Err(fail("curve refinement received an unsupported primitive kind"));
        }
        let first = view.primitive_offsets[primitive];
        let last = view.primitive_offsets[primitive + 1];
        let cyclic = kind == 2;
        for vertex in first..last {
            let next = if vertex + 1 < last {
                vertex + 1
            } else if cyclic {
                first
            } else {
                -1
            };
            if next >= 0 && view.vertex_points[vertex as usize] == view.vertex_points[next as usize] {
                return Err(fail("a polygon curve contains a zero-length topology edge"));
            }
        }
    }
    let positions = packed::Float3::View::view(geometry::positions(value))?;
    if positions.x.iter().any(|v| !v.is_finite())
        || positions.y.iter().any(|v| !v.is_finite())
        || positions.z.iter().any(|v| !v.is_finite())
    {
        return Err(fail("positions must be finite"));
    }
    Ok(())
}

fn other_endpoint(index: &topology_index::View::View, point: i64, edge: i64) -> Result<i64, error::T> {
    if index.edge_a[edge as usize] == point {
        Ok(index.edge_b[edge as usize])
    } else if index.edge_b[edge as usize] == point {
        Ok(index.edge_a[edge as usize])
    } else {
        Err(fail("curve point-edge incidence is inconsistent"))
    }
}

fn shared_plan(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    scheme: Scheme,
    value: &geometry::T,
    view: &topology::View::View,
    index: &topology_index::View::View,
) -> Result<Plan, error::T> {
    let source_points = view.point_count;
    let source_primitives = view.primitive_kinds.len();
    let edge_count = index.edge_a.len() as i64;
    let output_points = checked_add("curve subdivision point count", source_points, edge_count)?;
    let mut point_offsets = vec![0_i64; output_points as usize + 1];
    for point in 0..source_points as usize {
        let degree = index.point_edge_offsets[point + 1] - index.point_edge_offsets[point];
        point_offsets[point + 1] =
            point_offsets[point] + if scheme == Scheme::CatmullClark && degree == 2 { 3 } else { 1 };
    }
    for edge in 0..edge_count as usize {
        let at = source_points as usize + edge;
        point_offsets[at + 1] = checked_add("curve subdivision point stencil", point_offsets[at], 2)?;
    }
    let stencil_count = point_offsets[output_points as usize] as usize;
    let grain = grain_size(grain)?;
    let mut point_sources = vec![0_i64; stencil_count];
    let mut point_weights = vec![0.0_f64; stencil_count];
    let mut point_representative = vec![0_i64; output_points as usize];
    let mut point_old = vec![false; output_points as usize];
    {
        let ranges = support::range_count(source_points, grain);
        let slot = |range: i64| range.saturating_mul(grain).min(source_points);
        let rows = |range: i64| point_offsets[slot(range) as usize] as usize;
        let (sources, weights) = (
            support::Windows::new(&mut point_sources, ranges, rows),
            support::Windows::new(&mut point_weights, ranges, rows),
        );
        let (representative, old) = (
            support::Windows::new(&mut point_representative, ranges, |range| slot(range) as usize),
            support::Windows::new(&mut point_old, ranges, |range| slot(range) as usize),
        );
        support::run_indexed_ranges(grain, source_points, &|range, first_point, last_point| {
            let (sources, weights) = (sources.take(range), weights.take(range));
            let (representative, old) = (representative.take(range), old.take(range));
            let base = rows(range);
            for point in first_point..last_point {
                cancel::poll(cancel_value, point, 4095)?;
                let first = point_offsets[point as usize] as usize - base;
                let slot = (point - first_point) as usize;
                old[slot] = true;
                representative[slot] = point;
                let edge_first = index.point_edge_offsets[point as usize];
                let edge_last = index.point_edge_offsets[point as usize + 1];
                if scheme == Scheme::CatmullClark && edge_last - edge_first == 2 {
                    sources[first] = other_endpoint(index, point, index.point_edges[edge_first as usize])?;
                    weights[first] = 0.125;
                    sources[first + 1] = point;
                    weights[first + 1] = 0.75;
                    sources[first + 2] = other_endpoint(index, point, index.point_edges[edge_first as usize + 1])?;
                    weights[first + 2] = 0.125;
                } else {
                    sources[first] = point;
                    weights[first] = 1.0;
                }
            }
            Ok(())
        })?;
    }
    {
        let ranges = support::range_count(edge_count, grain);
        let slot = |range: i64| range.saturating_mul(grain).min(edge_count);
        let rows = |range: i64| point_offsets[(source_points + slot(range)) as usize] as usize;
        let (sources, weights) = (
            support::Windows::new(&mut point_sources, ranges, rows),
            support::Windows::new(&mut point_weights, ranges, rows),
        );
        let representative = support::Windows::new(&mut point_representative, ranges, |range| (source_points + slot(range)) as usize);
        support::run_indexed_ranges(grain, edge_count, &|range, first_edge, last_edge| {
            let (sources, weights, representative) = (sources.take(range), weights.take(range), representative.take(range));
            let base = rows(range);
            for edge in first_edge..last_edge {
                cancel::poll(cancel_value, edge, 4095)?;
                let first = point_offsets[(source_points + edge) as usize] as usize - base;
                let (a, b) = (index.edge_a[edge as usize], index.edge_b[edge as usize]);
                sources[first] = a;
                weights[first] = 0.5;
                sources[first + 1] = b;
                weights[first + 1] = 0.5;
                representative[(edge - first_edge) as usize] = a.min(b);
            }
            Ok(())
        })?;
    }
    let mut primitive_offsets = vec![0_i64; source_primitives + 1];
    for primitive in 0..source_primitives {
        let size = primitive_size(&view.primitive_offsets, primitive);
        let closed = view.primitive_kinds[primitive] == 2;
        primitive_offsets[primitive + 1] = checked_add(
            "curve subdivision vertex count",
            primitive_offsets[primitive],
            2 * size - if closed { 0 } else { 1 },
        )?;
    }
    let output_vertices = primitive_offsets[source_primitives] as usize;
    let mut vertex_points = vec![0_i64; output_vertices];
    let primitive_kinds: Vec<u8> = view.primitive_kinds.to_vec();
    let mut vertex_left = vec![0_i64; output_vertices];
    let mut vertex_right = vec![0_i64; output_vertices];
    let mut vertex_weight = vec![0.0_f64; output_vertices];
    let mut vertex_old = vec![false; output_vertices];
    let mut source_edge_of_output_vertex = vec![-1_i64; output_vertices];
    {
        let primitives = source_primitives as i64;
        let ranges = support::range_count(primitives, grain);
        let rows = |range: i64| primitive_offsets[range.saturating_mul(grain).min(primitives) as usize] as usize;
        let (points, left, right, old, source_edge) = (
            support::Windows::new(&mut vertex_points, ranges, rows),
            support::Windows::new(&mut vertex_left, ranges, rows),
            support::Windows::new(&mut vertex_right, ranges, rows),
            support::Windows::new(&mut vertex_old, ranges, rows),
            support::Windows::new(&mut source_edge_of_output_vertex, ranges, rows),
        );
        let weight = support::Windows::new(&mut vertex_weight, ranges, rows);
        support::run_indexed_ranges(grain, primitives, &|range, first_primitive, last_primitive| {
            let (points, left, right) = (points.take(range), left.take(range), right.take(range));
            let (old, source_edge, weight) = (old.take(range), source_edge.take(range), weight.take(range));
            let base = rows(range) as i64;
            for primitive in first_primitive..last_primitive {
                cancel::poll(cancel_value, primitive, 4095)?;
                let first = view.primitive_offsets[primitive as usize];
                let last = view.primitive_offsets[primitive as usize + 1];
                let mut output = primitive_offsets[primitive as usize] - base;
                for vertex in first..last {
                    let at = output as usize;
                    output += 1;
                    points[at] = view.vertex_points[vertex as usize];
                    left[at] = vertex;
                    right[at] = vertex;
                    old[at] = true;
                    let edge = index.edge_of_vertex[vertex as usize];
                    if edge >= 0 {
                        source_edge[at] = edge;
                        let midpoint = output as usize;
                        output += 1;
                        let next = index.next_vertex[vertex as usize];
                        if next < 0 {
                            return Err(fail("curve edge has no following corner"));
                        }
                        points[midpoint] = source_points + edge;
                        left[midpoint] = vertex;
                        right[midpoint] = next;
                        weight[midpoint] = 0.5;
                        source_edge[midpoint] = edge;
                    }
                }
                if output != primitive_offsets[primitive as usize + 1] - base {
                    return Err(fail("curve subdivision vertex cardinality mismatch"));
                }
            }
            Ok(())
        })?;
    }
    let output_topology = topology::View::create_validated_owned(
        output_points,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let output_index = if geometry::edge_groups(value).is_empty() {
        None
    } else {
        Some(topology_index::create(cancel_value, &output_topology)?)
    };
    Ok(Plan {
        source: value.clone(),
        output_topology,
        output_index,
        point_offsets,
        point_sources,
        point_weights,
        point_representative,
        point_old,
        vertex_left,
        vertex_right,
        vertex_weight,
        vertex_old,
        primitive_source: (0..source_primitives as i64).collect(),
        source_edge_of_output_vertex,
    })
}

fn independent_plan(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    scheme: Scheme,
    value: &geometry::T,
    view: &topology::View::View,
    index: &topology_index::View::View,
) -> Result<Plan, error::T> {
    let source_primitives = view.primitive_kinds.len();
    let mut output_points = 0_i64;
    let mut primitive_offsets = vec![0_i64; source_primitives + 1];
    for primitive in 0..source_primitives {
        let size = primitive_size(&view.primitive_offsets, primitive);
        let closed = view.primitive_kinds[primitive] == 2;
        output_points = checked_add(
            "independent curve subdivision point count",
            output_points,
            2 * size - if closed { 0 } else { 1 },
        )?;
        primitive_offsets[primitive + 1] = output_points;
    }
    let count = output_points as usize;
    let mut point_offsets = vec![0_i64; count + 1];
    let mut point_representative = vec![0_i64; count];
    let mut point_old = vec![false; count];
    let vertex_points: Vec<i64> = (0..output_points).collect();
    let mut vertex_left = vec![0_i64; count];
    let mut vertex_right = vec![0_i64; count];
    let mut vertex_weight = vec![0.0_f64; count];
    let mut vertex_old = vec![false; count];
    let mut source_edge_of_output_vertex = vec![-1_i64; count];
    for primitive in 0..source_primitives {
        let first = view.primitive_offsets[primitive];
        let last = view.primitive_offsets[primitive + 1];
        let closed = view.primitive_kinds[primitive] == 2;
        let mut output = primitive_offsets[primitive];
        for vertex in first..last {
            let old = output as usize;
            output += 1;
            let smooth = scheme == Scheme::CatmullClark && (closed || (vertex > first && vertex + 1 < last));
            point_offsets[old + 1] = point_offsets[old] + if smooth { 3 } else { 1 };
            point_representative[old] = view.vertex_points[vertex as usize];
            point_old[old] = true;
            vertex_left[old] = vertex;
            vertex_right[old] = vertex;
            vertex_old[old] = true;
            let edge = index.edge_of_vertex[vertex as usize];
            if edge >= 0 {
                source_edge_of_output_vertex[old] = edge;
                let midpoint = output as usize;
                output += 1;
                point_offsets[midpoint + 1] = point_offsets[midpoint] + 2;
                let next = index.next_vertex[vertex as usize];
                if next < 0 {
                    return Err(fail("curve edge has no following corner"));
                }
                let (a, b) = (view.vertex_points[vertex as usize], view.vertex_points[next as usize]);
                point_representative[midpoint] = a.min(b);
                vertex_left[midpoint] = vertex;
                vertex_right[midpoint] = next;
                vertex_weight[midpoint] = 0.5;
                source_edge_of_output_vertex[midpoint] = edge;
            }
        }
        if output != primitive_offsets[primitive + 1] {
            return Err(fail("independent curve vertex cardinality mismatch"));
        }
    }
    let stencil_count = point_offsets[count] as usize;
    let grain = grain_size(grain)?;
    let mut point_sources = vec![0_i64; stencil_count];
    let mut point_weights = vec![0.0_f64; stencil_count];
    {
        let primitives = source_primitives as i64;
        let ranges = support::range_count(primitives, grain);
        let rows = |range: i64| {
            point_offsets[primitive_offsets[range.saturating_mul(grain).min(primitives) as usize] as usize] as usize
        };
        let (sources, weights) = (
            support::Windows::new(&mut point_sources, ranges, rows),
            support::Windows::new(&mut point_weights, ranges, rows),
        );
        support::run_indexed_ranges(grain, primitives, &|range, first_primitive, last_primitive| {
            let (sources, weights) = (sources.take(range), weights.take(range));
            let base = rows(range) as i64;
            for primitive in first_primitive..last_primitive {
                cancel::poll(cancel_value, primitive, 4095)?;
                let first = view.primitive_offsets[primitive as usize];
                let last = view.primitive_offsets[primitive as usize + 1];
                let closed = view.primitive_kinds[primitive as usize] == 2;
                let mut output = primitive_offsets[primitive as usize];
                for vertex in first..last {
                    let at = (point_offsets[output as usize] - base) as usize;
                    output += 1;
                    let smooth = scheme == Scheme::CatmullClark && (closed || (vertex > first && vertex + 1 < last));
                    if smooth {
                        let previous = if vertex > first { vertex - 1 } else { last - 1 };
                        let next = if vertex + 1 < last { vertex + 1 } else { first };
                        sources[at] = view.vertex_points[previous as usize];
                        weights[at] = 0.125;
                        sources[at + 1] = view.vertex_points[vertex as usize];
                        weights[at + 1] = 0.75;
                        sources[at + 2] = view.vertex_points[next as usize];
                        weights[at + 2] = 0.125;
                    } else {
                        sources[at] = view.vertex_points[vertex as usize];
                        weights[at] = 1.0;
                    }
                    let next = index.next_vertex[vertex as usize];
                    if next >= 0 {
                        let midpoint_at = (point_offsets[output as usize] - base) as usize;
                        output += 1;
                        sources[midpoint_at] = view.vertex_points[vertex as usize];
                        weights[midpoint_at] = 0.5;
                        sources[midpoint_at + 1] = view.vertex_points[next as usize];
                        weights[midpoint_at + 1] = 0.5;
                    }
                }
            }
            Ok(())
        })?;
    }
    let output_topology = topology::View::create_validated_owned(
        output_points,
        vertex_points,
        primitive_offsets,
        view.primitive_kinds.to_vec(),
    );
    let output_index = if geometry::edge_groups(value).is_empty() {
        None
    } else {
        Some(topology_index::create(cancel_value, &output_topology)?)
    };
    Ok(Plan {
        source: value.clone(),
        output_topology,
        output_index,
        point_offsets,
        point_sources,
        point_weights,
        point_representative,
        point_old,
        vertex_left,
        vertex_right,
        vertex_weight,
        vertex_old,
        primitive_source: (0..source_primitives as i64).collect(),
        source_edge_of_output_vertex,
    })
}

fn make_plan(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    independent: bool,
    scheme: Scheme,
    value: &geometry::T,
) -> Result<Plan, error::T> {
    validate(cancel_value, value)?;
    let view = topology::View::view(geometry::topology(value))?;
    let index_value = topology_index::create(cancel_value, geometry::topology(value))?;
    let index = topology_index::View::view(&index_value);
    if independent {
        independent_plan(cancel_value, grain, scheme, value, &view, &index)
    } else {
        shared_plan(cancel_value, grain, scheme, value, &view, &index)
    }
}

fn point_float(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    map_floats(grain, cancel_value, plan.point_representative.len() as i64, &|point| {
        let mut sum = 0.0;
        for at in plan.point_offsets[point as usize]..plan.point_offsets[point as usize + 1] {
            sum += source[plan.point_sources[at as usize] as usize] * plan.point_weights[at as usize];
        }
        sum
    })
}

fn vertex_float(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    map_floats(grain, cancel_value, plan.vertex_left.len() as i64, &|vertex| {
        let weight = plan.vertex_weight[vertex as usize];
        let left = source[plan.vertex_left[vertex as usize] as usize];
        left + (source[plan.vertex_right[vertex as usize] as usize] - left) * weight
    })
}

fn discrete_mapping(plan: &Plan, owner: attribute::Owner) -> Vec<i64> {
    match owner {
        attribute::Owner::Point => plan.point_representative.clone(),
        attribute::Owner::Vertex => (0..plan.vertex_left.len())
            .map(|vertex| {
                if plan.vertex_old[vertex] {
                    plan.vertex_left[vertex]
                } else {
                    plan.vertex_left[vertex].min(plan.vertex_right[vertex])
                }
            })
            .collect(),
        attribute::Owner::Primitive => plan.primitive_source.clone(),
        attribute::Owner::Detail => vec![0],
    }
}

fn numeric(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    owner: attribute::Owner,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    match owner {
        attribute::Owner::Point => point_float(cancel_value, grain, plan, source),
        attribute::Owner::Vertex => vertex_float(cancel_value, grain, plan, source),
        attribute::Owner::Primitive => topology_remap::select_float(
            cancel_value,
            grain.unwrap_or(16_384),
            None,
            &plan.primitive_source,
            source,
        ),
        attribute::Owner::Detail => Ok(source.to_vec()),
    }
}

fn remap_attribute(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    value: &attribute::T,
) -> Result<attribute::T, error::T> {
    let owner = attribute::owner(value);
    let chunk = grain.unwrap_or(16_384);
    let storage = match attribute::View::storage(value)? {
        attribute::Storage::Float(values) => {
            attribute::Storage::Float(numeric(cancel_value, grain, plan, owner, values)?)
        }
        attribute::Storage::Int(values) => attribute::Storage::Int(topology_remap::select_int(
            cancel_value,
            chunk,
            None,
            &discrete_mapping(plan, owner),
            values,
        )?),
        attribute::Storage::Text(values) => attribute::Storage::Text(topology_remap::select_text(
            cancel_value,
            chunk,
            None,
            &discrete_mapping(plan, owner),
            values,
        )?),
        attribute::Storage::IntArray(values) => attribute::Storage::IntArray(ragged_ops::remap_int(
            cancel_value,
            grain,
            &discrete_mapping(plan, owner),
            values,
        )?),
        attribute::Storage::FloatArray(values) => {
            attribute::Storage::FloatArray(ragged_ops::remap_float(
                cancel_value,
                grain,
                &discrete_mapping(plan, owner),
                values,
            )?)
        }
        attribute::Storage::Float2(values) => {
            let view = packed::Float2::View::view(values);
            attribute::Storage::Float2(packed::Float2::of_owned(
                numeric(cancel_value, grain, plan, owner, &view.x)?,
                numeric(cancel_value, grain, plan, owner, &view.y)?,
            )?)
        }
        attribute::Storage::Float3(values) => {
            let view = packed::Float3::View::view(values)?;
            attribute::Storage::Float3(packed::Float3::View::of_owned_exn(
                numeric(cancel_value, grain, plan, owner, &view.x)?,
                numeric(cancel_value, grain, plan, owner, &view.y)?,
                numeric(cancel_value, grain, plan, owner, &view.z)?,
            )?)
        }
        attribute::Storage::Float4(values) => {
            let view = packed::Float4::View::view(values);
            attribute::Storage::Float4(packed::Float4::of_owned(
                numeric(cancel_value, grain, plan, owner, &view.x)?,
                numeric(cancel_value, grain, plan, owner, &view.y)?,
                numeric(cancel_value, grain, plan, owner, &view.z)?,
                numeric(cancel_value, grain, plan, owner, &view.w)?,
            )?)
        }
    };
    attribute::create_owned(attribute::name(value), owner, storage)
}

fn remap_group(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    value: &group::T,
) -> Result<group::T, error::T> {
    let owner = group::owner(value);
    let count = match owner {
        group::Owner::Point => plan.point_representative.len(),
        group::Owner::Vertex => plan.vertex_left.len(),
        group::Owner::Primitive => plan.primitive_source.len(),
    } as i64;
    let cancelled = AtomicBool::new(false);
    let target = group::init(grain, owner, group::name(value), count, &|output| {
        if cancel::poll(cancel_value, output, 4095).is_err() {
            cancelled.store(true, Ordering::Relaxed);
            return false;
        }
        let u = output as usize;
        match owner {
            group::Owner::Point => {
                if plan.point_old[u] {
                    group::mem(plan.point_representative[u], value)
                } else {
                    let first = plan.point_offsets[u] as usize;
                    group::mem(plan.point_sources[first], value) && group::mem(plan.point_sources[first + 1], value)
                }
            }
            group::Owner::Vertex => {
                if plan.vertex_old[u] {
                    group::mem(plan.vertex_left[u], value)
                } else {
                    group::mem(plan.vertex_left[u], value) && group::mem(plan.vertex_right[u], value)
                }
            }
            group::Owner::Primitive => group::mem(plan.primitive_source[u], value),
        }
    })?;
    if cancelled.load(Ordering::Relaxed) {
        cancel::check_opt(cancel_value)?;
    }
    if !group::is_ordered(value) {
        return Ok(target);
    }
    let source_of_target: Vec<i64> = match owner {
        group::Owner::Point => (0..count as usize)
            .map(|output| if plan.point_old[output] { plan.point_representative[output] } else { -1 })
            .collect(),
        group::Owner::Vertex => (0..count as usize)
            .map(|output| if plan.vertex_old[output] { plan.vertex_left[output] } else { -1 })
            .collect(),
        group::Owner::Primitive => plan.primitive_source.clone(),
    };
    group::View::remap_order(value, &source_of_target, &target)
}

fn remap_edge_group(
    cancel_value: Option<&cancel::T>,
    plan: &Plan,
    value: &edge_group::T,
) -> Result<edge_group::T, error::T> {
    if edge_group::topology_data_id(value) != topology::data_id(geometry::topology(&plan.source)) {
        return Err(fail("edge group belongs to a different topology"));
    }
    let Some(output_index) = plan.output_index.as_ref() else {
        return Err(fail("curve edge-group remap is missing its output topology index"));
    };
    let output_view = topology_index::View::view(output_index);
    let mut builder = edge_group::Builder::create(&plan.output_topology, output_index, edge_group::name(value))?;
    for vertex in 0..plan.source_edge_of_output_vertex.len() {
        cancel::poll(cancel_value, vertex as i64, 16_383)?;
        let source_edge = plan.source_edge_of_output_vertex[vertex];
        if source_edge >= 0 && edge_group::mem(source_edge, value) {
            let output_edge = output_view.edge_of_vertex[vertex];
            if output_edge < 0 {
                return Err(fail("curve child edge ancestry is incomplete"));
            }
            edge_group::Builder::set(&mut builder, output_edge, true)?;
        }
    }
    edge_group::Builder::freeze(&mut builder)
}

fn once(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    independent: bool,
    scheme: Scheme,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let plan = make_plan(cancel_value, grain, independent, scheme, value)?;
    let source_positions = packed::Float3::View::view(geometry::positions(value))?;
    let positions = packed::Float3::View::of_owned_exn(
        point_float(cancel_value, grain, &plan, &source_positions.x)?,
        point_float(cancel_value, grain, &plan, &source_positions.y)?,
        point_float(cancel_value, grain, &plan, &source_positions.z)?,
    )?;
    let mut attributes = Vec::new();
    for item in geometry::attributes(value) {
        attributes.push(remap_attribute(cancel_value, grain, &plan, &item)?);
    }
    let mut groups = Vec::new();
    for item in geometry::groups(value) {
        groups.push(remap_group(cancel_value, grain, &plan, &item)?);
    }
    let mut edge_groups = Vec::new();
    for item in geometry::edge_groups(value) {
        edge_groups.push(remap_edge_group(cancel_value, &plan, &item)?);
    }
    geometry::create(
        positions,
        plan.output_topology.clone(),
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

pub fn iterate(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    independent: bool,
    scheme: Scheme,
    iterations: i64,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let mut remaining = iterations;
    let mut current = value.clone();
    loop {
        cancel::check_opt(cancel_value)?;
        if remaining == 0 {
            return Ok(current);
        }
        remaining -= 1;
        current = once(cancel_value, grain, independent, scheme, &current)?;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn polyline(closed: bool) -> geometry::T {
        let positions = packed::Float3::of_owned(
            vec![0.0, 1.0, 1.0, 0.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0; 4],
        )
        .unwrap();
        let mut builder = topology::Builder::create(4, None, None).unwrap();
        if closed {
            topology::Builder::add_closed_polyline(&mut builder, &[0, 1, 2, 3]).unwrap();
        } else {
            topology::Builder::add_open_polyline(&mut builder, &[0, 1, 2, 3]).unwrap();
        }
        let topo = topology::Builder::freeze(&mut builder).unwrap();
        let weights = attribute::create_owned("w", attribute::Owner::Point, attribute::Storage::Float(vec![0.0, 4.0, 8.0, 12.0])).unwrap();
        geometry::create(positions, topo, Some(vec![weights]), None, None).unwrap()
    }

    fn xs(value: &geometry::T) -> Vec<f64> {
        packed::Float3::View::view(geometry::positions(value)).unwrap().x.to_vec()
    }

    #[test]
    fn bilinear_inserts_midpoints_and_keeps_originals() {
        let out = iterate(None, None, false, Scheme::Bilinear, 1, &polyline(false)).unwrap();
        assert_eq!(geometry::point_count(&out), 7);
        assert_eq!(geometry::vertex_count(&out), 7);
        assert_eq!(&xs(&out)[..4], &[0.0, 1.0, 1.0, 0.0]);
        assert_eq!(&xs(&out)[4..], &[0.5, 1.0, 0.5]);
    }

    #[test]
    fn catmull_clark_smooths_interior_points_of_a_closed_curve() {
        let out = iterate(None, None, false, Scheme::CatmullClark, 1, &polyline(true)).unwrap();
        assert_eq!(geometry::point_count(&out), 8);
        // point 0 = 1/8 * p3 + 3/4 * p0 + 1/8 * p1
        assert_eq!(xs(&out)[0], 0.125 * 0.0 + 0.75 * 0.0 + 0.125 * 1.0);
        let weights = geometry::find_attribute(attribute::Owner::Point, "w", &out).unwrap();
        match attribute::View::storage(&weights).unwrap() {
            attribute::Storage::Float(values) => assert_eq!(values[4], 2.0),
            _ => panic!("float"),
        }
    }

    #[test]
    fn independent_curves_get_their_own_points_and_grains_agree() {
        let single = iterate(None, None, true, Scheme::CatmullClark, 2, &polyline(true)).unwrap();
        let tiny = iterate(None, Some(1), true, Scheme::CatmullClark, 2, &polyline(true)).unwrap();
        assert_eq!(geometry::point_count(&single), 16);
        assert_eq!(xs(&single), xs(&tiny));
        let shared = iterate(None, Some(1), false, Scheme::CatmullClark, 2, &polyline(true)).unwrap();
        assert_eq!(xs(&shared), xs(&iterate(None, None, false, Scheme::CatmullClark, 2, &polyline(true)).unwrap()));
    }

    #[test]
    fn rejects_bad_input() {
        let Err(zero) = iterate(None, Some(0), false, Scheme::Bilinear, 1, &polyline(false)) else {
            panic!("expected failure")
        };
        assert_eq!(error::message(&zero), "Rdk_curve.Curve_subdivide.subdivide: grain must be positive");
        let cancelled = cancel::create();
        cancel::cancel(&cancelled);
        let Err(stopped) = iterate(Some(&cancelled), None, false, Scheme::Bilinear, 1, &polyline(false)) else {
            panic!("expected cancellation")
        };
        assert_eq!(error::code(&stopped), "cancelled");
    }
}
