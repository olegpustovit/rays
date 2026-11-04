//! Port of lib/rdk/ops/mesh/subdivide_stitch.ml: crack closure by pulling or stitching the
//! subdivided boundary to the unselected faces.
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::{cancel, geometry, packed, topology, topology_index};

use crate::subdivide_refine::{
    checked_add, checked_mul, failm, fmax, fmin, gf, gi, run, AFloats, DividedBoundary,
    InterfaceChains, PointWeld, PullPlan, Res, TriangleAppend,
};

pub fn shared_edge_endpoint(index: &topology_index::View::View, left: i64, right: i64) -> i64 {
    let left_a = gi(&index.edge_a, left);
    let left_b = gi(&index.edge_b, left);
    let right_a = gi(&index.edge_a, right);
    let right_b = gi(&index.edge_b, right);
    if left_a == right_a || left_a == right_b {
        left_a
    } else if left_b == right_a || left_b == right_b {
        left_b
    } else {
        -1
    }
}

pub fn project_to_source_edge(
    source_positions: &packed::Float3::View::View,
    index: &topology_index::View::View,
    edge: i64,
    x: f64,
    y: f64,
    z: f64,
) -> (f64, f64, f64) {
    let a = gi(&index.edge_a, edge);
    let b = gi(&index.edge_b, edge);
    let ax = gf(&source_positions.x, a);
    let ay = gf(&source_positions.y, a);
    let az = gf(&source_positions.z, a);
    let bx = gf(&source_positions.x, b);
    let by = gf(&source_positions.y, b);
    let bz = gf(&source_positions.z, b);
    let max3 = |a: f64, b: f64, c: f64| fmax(a.abs(), fmax(b.abs(), c.abs()));
    let scale = fmax(max3(x, y, z), fmax(max3(ax, ay, az), max3(bx, by, bz)));
    if scale == 0.0 {
        (ax, ay, az)
    } else {
        let dx = (bx / scale) - (ax / scale);
        let dy = (by / scale) - (ay / scale);
        let dz = (bz / scale) - (az / scale);
        let rx = (x / scale) - (ax / scale);
        let ry = (y / scale) - (ay / scale);
        let rz = (z / scale) - (az / scale);
        let denominator = (dx * dx) + (dy * dy) + (dz * dz);
        let t = if denominator <= 0.0 || !denominator.is_finite() {
            0.0
        } else {
            fmax(0.0, fmin(1.0, ((rx * dx) + (ry * dy) + (rz * dz)) / denominator))
        };
        let one_minus_t = 1.0 - t;
        (
            (one_minus_t * ax) + (t * bx),
            (one_minus_t * ay) + (t * by),
            (one_minus_t * az) + (t * bz),
        )
    }
}

fn ancestry_of(plan: &PullPlan, geometry_value: &geometry::T) -> Res<Vec<i64>> {
    match geometry::find_attribute(Owner::Vertex, &plan.ancestry_attribute, geometry_value) {
        Some(found) => match attribute::View::storage(&found)? {
            Storage::Int(values) => Ok(values.clone()),
            _ => Err(failm("internal edge ancestry changed storage")),
        },
        None => Err(failm("local subdivision dropped internal edge ancestry")),
    }
}

pub fn pull_boundary_no_edge_division(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &PullPlan,
    geometry_value: &geometry::T,
) -> Res<geometry::T> {
    let ancestry = ancestry_of(plan, geometry_value)?;
    let topology_view = topology::View::view(geometry::topology(geometry_value))?;
    if ancestry.len() != topology_view.vertex_points.len() {
        return Err(failm("internal edge ancestry cardinality mismatch"));
    }
    let source_index = topology_index::View::view(&plan.source_index);
    let mut first_edge = vec![-1_i64; topology_view.point_count as usize];
    let mut common_endpoint = vec![-1_i64; topology_view.point_count as usize];
    let mut add_constraint = |point: i64, edge: i64| -> Res<()> {
        if edge < 0 || edge as usize >= source_index.edge_a.len() {
            return Err(failm("internal edge ancestry is out of range"));
        }
        let first = first_edge[point as usize];
        if first < 0 {
            first_edge[point as usize] = edge;
        } else if first != edge {
            let common = common_endpoint[point as usize];
            if common >= 0 {
                if gi(&source_index.edge_a, edge) != common && gi(&source_index.edge_b, edge) != common
                {
                    return Err(failm("refined boundary point has incompatible source-edge ancestry"));
                }
            } else {
                let shared = shared_edge_endpoint(&source_index, first, edge);
                if shared < 0 {
                    return Err(failm("refined boundary point has incompatible source-edge ancestry"));
                }
                common_endpoint[point as usize] = shared;
            }
        }
        Ok(())
    };
    for primitive in 0..topology_view.primitive_kinds.len() as i64 {
        cancel::poll(cancel_token, primitive, 4095)?;
        let first = gi(&topology_view.primitive_offsets, primitive);
        let last = gi(&topology_view.primitive_offsets, primitive + 1);
        for vertex in first..last {
            let edge = gi(&ancestry, vertex);
            if edge >= 0 {
                let next = if vertex + 1 == last { first } else { vertex + 1 };
                add_constraint(gi(&topology_view.vertex_points, vertex), edge)?;
                add_constraint(gi(&topology_view.vertex_points, next), edge)?;
            }
        }
    }
    let source_positions = packed::Float3::View::view(&plan.source_positions)?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let x = AFloats::from_slice(&positions.x);
    let y = AFloats::from_slice(&positions.y);
    let z = AFloats::from_slice(&positions.z);
    run(grain, cancel_token, topology_view.point_count, &|point| {
        let edge = gi(&first_edge, point);
        if edge >= 0 {
            let (px, py, pz) = if gi(&common_endpoint, point) >= 0 {
                packed::Float3::get(&plan.source_positions, gi(&common_endpoint, point))?
            } else {
                project_to_source_edge(
                    &source_positions,
                    &source_index,
                    edge,
                    gf(&positions.x, point),
                    gf(&positions.y, point),
                    gf(&positions.z, point),
                )
            };
            if !(px.is_finite() && py.is_finite() && pz.is_finite()) {
                return Err(failm("Pull Closed generated a non-finite boundary position"));
            }
            x.set(point, px);
            y.set(point, py);
            z.set(point, pz);
        }
        Ok(())
    })?;
    let positions = packed::Float3::View::of_owned_exn(x.into_vec(), y.into_vec(), z.into_vec())?;
    let moved = geometry::with_positions(positions, geometry_value)?;
    Ok(geometry::without_attribute(Owner::Vertex, &plan.ancestry_attribute, &moved))
}

struct TriangleSink {
    triangle_points: Vec<i64>,
    vertex_sources: Vec<i64>,
    primitive_sources: Vec<i64>,
    triangle_at: i64,
    primitive_at: i64,
}

impl TriangleSink {
    fn new(triangle_vertices: i64, added_primitives: i64) -> TriangleSink {
        TriangleSink {
            triangle_points: vec![0; triangle_vertices as usize],
            vertex_sources: vec![0; triangle_vertices as usize],
            primitive_sources: vec![0; added_primitives as usize],
            triangle_at: 0,
            primitive_at: 0,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn emit(&mut self, primitive_source: i64, p0: i64, v0: i64, p1: i64, v1: i64, p2: i64, v2: i64) {
        let at = self.triangle_at as usize;
        self.triangle_points[at] = p0;
        self.triangle_points[at + 1] = p1;
        self.triangle_points[at + 2] = p2;
        self.vertex_sources[at] = v0;
        self.vertex_sources[at + 1] = v1;
        self.vertex_sources[at + 2] = v2;
        self.primitive_sources[self.primitive_at as usize] = primitive_source;
        self.triangle_at += 3;
        self.primitive_at += 1;
    }

    fn finish(self) -> TriangleAppend {
        TriangleAppend {
            triangle_points: self.triangle_points,
            triangle_vertex_sources: self.vertex_sources,
            triangle_primitive_sources: self.primitive_sources,
        }
    }
}

pub fn stitch_boundary_no_edge_division(
    cancel_token: Option<&cancel::T>,
    plan: &PullPlan,
    unselected: &geometry::T,
    free_points: i64,
    refined: &geometry::T,
) -> Res<TriangleAppend> {
    let ancestry = ancestry_of(plan, refined)?;
    let refined_topology = topology::View::view(geometry::topology(refined))?;
    if ancestry.len() != refined_topology.vertex_points.len() {
        return Err(failm("internal edge ancestry cardinality mismatch"));
    }
    let source_index = topology_index::View::view(&plan.source_index);
    let source_edges = source_index.edge_a.len();
    let mut counts = vec![0_i64; source_edges];
    for primitive in 0..refined_topology.primitive_kinds.len() as i64 {
        cancel::poll(cancel_token, primitive, 4095)?;
        for vertex in gi(&refined_topology.primitive_offsets, primitive)
            ..gi(&refined_topology.primitive_offsets, primitive + 1)
        {
            let edge = gi(&ancestry, vertex);
            if edge >= 0 {
                if edge as usize >= source_edges {
                    return Err(failm("internal edge ancestry is out of range"));
                }
                counts[edge as usize] =
                    checked_add("stitch descendant edge count", counts[edge as usize], 1)?;
            }
        }
    }
    let mut offsets = vec![0_i64; source_edges + 1];
    for edge in 0..source_edges {
        offsets[edge + 1] = checked_add("stitch descendant storage", offsets[edge], counts[edge])?;
    }
    let descendant_count = offsets[source_edges] as usize;
    let mut starts = vec![0_i64; descendant_count];
    let mut ends = vec![0_i64; descendant_count];
    let mut start_vertices = vec![0_i64; descendant_count];
    let mut end_vertices = vec![0_i64; descendant_count];
    let mut cursor = offsets.clone();
    for primitive in 0..refined_topology.primitive_kinds.len() as i64 {
        let first = gi(&refined_topology.primitive_offsets, primitive);
        let last = gi(&refined_topology.primitive_offsets, primitive + 1);
        for vertex in first..last {
            let edge = gi(&ancestry, vertex);
            if edge >= 0 {
                let next = if vertex + 1 == last { first } else { vertex + 1 };
                let at = cursor[edge as usize] as usize;
                starts[at] = gi(&refined_topology.vertex_points, vertex);
                ends[at] = gi(&refined_topology.vertex_points, next);
                start_vertices[at] = vertex;
                end_vertices[at] = next;
                cursor[edge as usize] = at as i64 + 1;
            }
        }
    }
    let mut added_primitives = 0_i64;
    for edge in 0..source_edges {
        if counts[edge] > 0 {
            added_primitives = checked_add("stitch primitive count", added_primitives, counts[edge])?;
        }
    }
    let triangle_vertices = checked_mul("stitch vertex count", added_primitives, 3)?;
    let mut sink = TriangleSink::new(triangle_vertices, added_primitives);
    let refined_point_offset =
        checked_add("stitch refined point offset", geometry::point_count(unselected), free_points)?;
    let refined_vertex_offset = geometry::vertex_count(unselected);
    let unselected_topology = topology::View::view(geometry::topology(unselected))?;
    let unselected_index_value = topology_index::create(cancel_token, geometry::topology(unselected))?;
    let unselected_index = topology_index::View::view(&unselected_index_value);
    let mut next_point = vec![-1_i64; refined_topology.point_count as usize];
    let mut record_of_start = vec![-1_i64; refined_topology.point_count as usize];
    let mut incoming = vec![0u8; refined_topology.point_count as usize];
    let mut chain_points = vec![0_i64; descendant_count + 1];
    let mut chain_vertices = vec![0_i64; descendant_count + 1];
    for edge in 0..source_edges as i64 {
        let first = offsets[edge as usize];
        let last = offsets[edge as usize + 1];
        let count = last - first;
        if count > 0 {
            cancel::poll(cancel_token, edge, 4095)?;
            for at in first..last {
                let start = gi(&starts, at);
                let finish = gi(&ends, at);
                if next_point[start as usize] >= 0 || incoming[finish as usize] != 0 {
                    return Err(failm("stitch descendants do not form a simple directed chain"));
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
                        return Err(failm("stitch descendants contain multiple chains"));
                    }
                    chain_start = point;
                }
            }
            if chain_start < 0 {
                return Err(failm("stitch descendants form a closed loop"));
            }
            let mut current = chain_start;
            for local in 0..count {
                let record = record_of_start[current as usize];
                if record < first || record >= last {
                    return Err(failm("stitch descendant chain is disconnected"));
                }
                chain_points[local as usize] = current;
                chain_vertices[local as usize] = gi(&start_vertices, record);
                current = next_point[current as usize];
                if local + 1 == count {
                    chain_points[local as usize + 1] = current;
                    chain_vertices[local as usize + 1] = gi(&end_vertices, record);
                }
            }
            let source_a = gi(&plan.selected_edge_start, edge);
            if source_a < 0 {
                return Err(failm("stitch source edge has no selected orientation"));
            }
            let edge_a = gi(&source_index.edge_a, edge);
            let edge_b = gi(&source_index.edge_b, edge);
            let source_b = if edge_a == source_a {
                edge_b
            } else if edge_b == source_a {
                edge_a
            } else {
                return Err(failm("stitch source orientation is inconsistent"));
            };
            let coarse_a = gi(&plan.unselected_point_of_source, source_a);
            let coarse_b = gi(&plan.unselected_point_of_source, source_b);
            if coarse_a < 0 || coarse_b < 0 {
                return Err(failm("stitch coarse edge was removed from the unselected region"));
            }
            let coarse_edge = match topology_index::find_edge(&unselected_index_value, coarse_a, coarse_b)
            {
                Some(found) => found,
                None => return Err(failm("stitch could not find the coarse boundary edge")),
            };
            let coarse_directed =
                gi(&unselected_index.edge_vertices, gi(&unselected_index.edge_offsets, coarse_edge));
            let coarse_next = gi(&unselected_index.next_vertex, coarse_directed);
            if coarse_next < 0 {
                return Err(failm("stitch coarse edge has no complete corner"));
            }
            let (coarse_a_vertex, coarse_b_vertex) =
                if gi(&unselected_topology.vertex_points, coarse_directed) == coarse_a {
                    (coarse_directed, coarse_next)
                } else {
                    (coarse_next, coarse_directed)
                };
            let source_primitive = gi(&plan.unselected_primitive_of_edge, edge);
            if source_primitive < 0 {
                return Err(failm("stitch edge has no unselected primitive"));
            }
            let primitive_source = gi(&plan.unselected_primitive_of_source, source_primitive);
            if primitive_source < 0 {
                return Err(failm("stitch primitive ancestry was removed"));
            }
            let split = count / 2;
            let q = |point: i64| refined_point_offset + gi(&chain_points, point);
            let qv = |point: i64| refined_vertex_offset + gi(&chain_vertices, point);
            for segment in 0..split {
                sink.emit(
                    primitive_source,
                    coarse_a,
                    coarse_a_vertex,
                    q(segment + 1),
                    qv(segment + 1),
                    q(segment),
                    qv(segment),
                );
            }
            for segment in split..count {
                sink.emit(
                    primitive_source,
                    coarse_b,
                    coarse_b_vertex,
                    q(segment + 1),
                    qv(segment + 1),
                    q(segment),
                    qv(segment),
                );
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
    if sink.triangle_at != triangle_vertices || sink.primitive_at != added_primitives {
        return Err(failm("stitch output cardinality mismatch"));
    }
    Ok(sink.finish())
}

pub fn stitch_boundary_divide_edges(
    cancel_token: Option<&cancel::T>,
    consistent_topology: bool,
    plan: &PullPlan,
    chains: &InterfaceChains,
    divided: &DividedBoundary,
    free_points: i64,
    free_vertices: i64,
    refined: &geometry::T,
) -> Res<TriangleAppend> {
    let source_edges = chains.edge_offsets.len() as i64 - 1;
    let coarse_positions = packed::Float3::View::view(geometry::positions(&divided.geometry))?;
    let refined_positions = packed::Float3::View::view(geometry::positions(refined))?;
    let coincident = |at: i64| -> bool {
        if consistent_topology {
            false
        } else {
            let coarse = gi(&divided.points, at);
            let refined_point = gi(&chains.points, at);
            gf(&coarse_positions.x, coarse) == gf(&refined_positions.x, refined_point)
                && gf(&coarse_positions.y, coarse) == gf(&refined_positions.y, refined_point)
                && gf(&coarse_positions.z, coarse) == gf(&refined_positions.z, refined_point)
        }
    };
    let mut added_primitives = 0_i64;
    for edge in 0..source_edges {
        let chain_length = gi(&chains.edge_offsets, edge + 1) - gi(&chains.edge_offsets, edge);
        if chain_length > 0 {
            let first = gi(&chains.edge_offsets, edge);
            let last = gi(&chains.edge_offsets, edge + 1);
            for at in first..last - 1 {
                let triangles = 2 - (if coincident(at) { 1 } else { 0 })
                    - (if coincident(at + 1) { 1 } else { 0 });
                added_primitives =
                    checked_add("divided stitch primitive count", added_primitives, triangles)?;
            }
        }
    }
    let triangle_vertices = checked_mul("divided stitch vertex count", added_primitives, 3)?;
    let mut sink = TriangleSink::new(triangle_vertices, added_primitives);
    let refined_point_offset = checked_add(
        "divided stitch refined point offset",
        geometry::point_count(&divided.geometry),
        free_points,
    )?;
    let refined_vertex_offset = checked_add(
        "divided stitch refined vertex offset",
        geometry::vertex_count(&divided.geometry),
        free_vertices,
    )?;
    for edge in 0..source_edges {
        let first = gi(&chains.edge_offsets, edge);
        let last = gi(&chains.edge_offsets, edge + 1);
        if last > first {
            cancel::poll(cancel_token, edge, 4095)?;
            let source_primitive = gi(&plan.unselected_primitive_of_edge, edge);
            if source_primitive < 0 {
                return Err(failm("divided stitch edge has no coarse primitive"));
            }
            let primitive_source = gi(&divided.source_primitive_to_output, source_primitive);
            if primitive_source < 0 {
                return Err(failm("divided stitch coarse primitive ancestry was removed"));
            }
            for at in first..last - 1 {
                let coarse_a = gi(&divided.points, at);
                let coarse_b = gi(&divided.points, at + 1);
                let refined_a = refined_point_offset + gi(&chains.points, at);
                let refined_b = refined_point_offset + gi(&chains.points, at + 1);
                let coarse_a_vertex = gi(&divided.vertices, at);
                let coarse_b_vertex = gi(&divided.vertices, at + 1);
                let refined_a_vertex = refined_vertex_offset + gi(&chains.vertices, at);
                let refined_b_vertex = refined_vertex_offset + gi(&chains.vertices, at + 1);
                if !coincident(at) {
                    sink.emit(
                        primitive_source,
                        coarse_a,
                        coarse_a_vertex,
                        refined_b,
                        refined_b_vertex,
                        refined_a,
                        refined_a_vertex,
                    );
                }
                if !coincident(at + 1) {
                    sink.emit(
                        primitive_source,
                        coarse_a,
                        coarse_a_vertex,
                        coarse_b,
                        coarse_b_vertex,
                        refined_b,
                        refined_b_vertex,
                    );
                }
            }
        }
    }
    if sink.triangle_at != triangle_vertices || sink.primitive_at != added_primitives {
        return Err(failm("divided stitch output cardinality mismatch"));
    }
    Ok(sink.finish())
}

pub fn make_boundary_weld(
    cancel_token: Option<&cancel::T>,
    bias: f64,
    only_equal: bool,
    chains: &InterfaceChains,
    divided: &DividedBoundary,
    free_points: i64,
    total_points: i64,
    refined: &geometry::T,
) -> Res<PointWeld> {
    let refined_point_offset = checked_add(
        "Pull Divide refined point offset",
        geometry::point_count(&divided.geometry),
        free_points,
    )?;
    if refined_point_offset + geometry::point_count(refined) != total_points {
        return Err(failm("boundary weld part cardinality is inconsistent"));
    }
    let mut representative: Vec<i64> = (0..total_points).collect();
    let mut removed = vec![0u8; total_points as usize];
    let coarse_positions = packed::Float3::View::view(geometry::positions(&divided.geometry))?;
    let refined_positions = packed::Float3::View::view(geometry::positions(refined))?;
    let coincident = |coarse: i64, refined_point: i64| {
        gf(&coarse_positions.x, coarse) == gf(&refined_positions.x, refined_point)
            && gf(&coarse_positions.y, coarse) == gf(&refined_positions.y, refined_point)
            && gf(&coarse_positions.z, coarse) == gf(&refined_positions.z, refined_point)
    };
    let source_edges = chains.edge_offsets.len() as i64 - 1;
    for edge in 0..source_edges {
        cancel::poll(cancel_token, edge, 4095)?;
        let first = gi(&chains.edge_offsets, edge);
        let last = gi(&chains.edge_offsets, edge + 1);
        for at in first..last {
            let coarse = gi(&divided.points, at);
            let refined_local = gi(&chains.points, at);
            let refined_point = refined_point_offset + refined_local;
            if !only_equal || coincident(coarse, refined_local) {
                let previous = representative[refined_point as usize];
                if previous != refined_point && previous != coarse {
                    return Err(failm(
                        "boundary weld maps one refined point to incompatible coarse points",
                    ));
                }
                representative[refined_point as usize] = coarse;
                removed[refined_point as usize] = 1;
            }
        }
    }
    let mut output_of_old = vec![-1_i64; total_points as usize];
    let mut output_points = 0_i64;
    for point in 0..total_points as usize {
        if removed[point] == 0 {
            output_of_old[point] = output_points;
            output_points += 1;
        }
    }
    let mut point_map = vec![0_i64; total_points as usize];
    for point in 0..total_points as usize {
        let root = representative[point];
        let target = gi(&output_of_old, root);
        if target < 0 {
            return Err(failm("Pull Divide representative was unexpectedly removed"));
        }
        point_map[point] = target;
    }
    let mut output_source = vec![0_i64; output_points as usize];
    for point in 0..total_points as usize {
        let output = output_of_old[point];
        if output >= 0 {
            output_source[output as usize] = point as i64;
        }
    }
    let mut merge_counts = vec![0_i64; output_points as usize];
    for point in 0..total_points as usize {
        if removed[point] != 0 {
            let output = point_map[point] as usize;
            merge_counts[output] = checked_add("Pull Divide merge incidence", merge_counts[output], 1)?;
        }
    }
    let mut merge_offsets = vec![0_i64; output_points as usize + 1];
    for point in 0..output_points as usize {
        merge_offsets[point + 1] =
            checked_add("Pull Divide merge storage", merge_offsets[point], merge_counts[point])?;
    }
    let mut merge_members = vec![0_i64; merge_offsets[output_points as usize] as usize];
    let mut cursor = merge_offsets.clone();
    for point in 0..total_points as usize {
        if removed[point] != 0 {
            let output = point_map[point] as usize;
            let at = cursor[output] as usize;
            merge_members[at] = point as i64;
            cursor[output] = at as i64 + 1;
        }
    }
    Ok(PointWeld { old_to_output: point_map, output_source, merge_offsets, merge_members, bias })
}
