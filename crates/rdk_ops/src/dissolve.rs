use rdk_core::support::DisjointSet;
use std::sync::Arc;

use rdk_core::topology::PrimitiveKind;
use rdk_core::{support, attribute, cancel, edge_group, error, geometry, normal_ops, packed, topology, topology_index, topology_remap};
use rdk_core::error::Code;

use crate::fuse_cleanup;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
    DissolveSelected,
    DissolveNonSelected,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BridgePolicy {
    CreateBridgedPolygons,
    CreateDisjointPolygons,
    DeleteBridgePolygons,
}

fn make(operation: &str, code: Code, message: String) -> error::T {
    error::new(operation, code, message)
}

fn fail(message: &str) -> error::T {
    make("Rdk.Dissolve", Code::InvalidParameter, format!("Rdk.Dissolve.dissolve: {message}"))
}

// Error.get / get_ok of the original: a cancellation stays one, any other message gains the prefix.
fn recode(failure: error::T) -> error::T {
    if error::is_cancelled(&failure) { failure } else { fail(error::message(&failure)) }
}

pub mod int_buffer {
    #[derive(Clone, Debug, Default)]
    pub struct T {
        pub values: Vec<i64>,
        pub length: i64,
    }

    pub(crate) fn create(capacity: usize) -> T {
        T { values: Vec::with_capacity(capacity.max(8)), length: 0 }
    }

    pub(crate) fn add(value: &mut T, item: i64) {
        value.values.push(item);
        value.length += 1;
    }
}

pub mod output {
    use super::int_buffer;
    use rdk_core::topology::PrimitiveKind;

    pub struct T {
        pub points: int_buffer::T,
        pub vertices: int_buffer::T,
        pub offsets: int_buffer::T,
        pub primitives: int_buffer::T,
        pub kinds: Vec<PrimitiveKind>,
    }

    pub(crate) fn create(source_vertices: usize, source_primitives: usize) -> T {
        let mut offsets = int_buffer::create(source_primitives + 1);
        int_buffer::add(&mut offsets, 0);
        T {
            points: int_buffer::create(source_vertices),
            vertices: int_buffer::create(source_vertices),
            offsets,
            primitives: int_buffer::create(source_primitives),
            kinds: Vec::new(),
        }
    }

    pub(crate) fn add(value: &mut T, source: i64, kind: PrimitiveKind, corners: &[i64], source_points: &[i64]) {
        let minimum = if kind == PrimitiveKind::OpenPolyline { 2 } else { 3 };
        if corners.len() >= minimum {
            for &vertex in corners {
                int_buffer::add(&mut value.points, source_points[vertex as usize]);
                int_buffer::add(&mut value.vertices, vertex);
            }
            value.kinds.push(kind);
            int_buffer::add(&mut value.primitives, source);
            let length = value.points.length;
            int_buffer::add(&mut value.offsets, length);
        }
    }
}

fn selected(bits: &[u8], edge: usize) -> bool {
    bits[edge >> 3] & (1 << (edge & 7)) != 0
}

fn set_selected(bits: &mut [u8], edge: usize) {
    bits[edge >> 3] |= 1 << (edge & 7);
}

#[allow(clippy::too_many_arguments)]
fn simplify(
    cancel_value: Option<&cancel::T>,
    tolerance: f64,
    affected: &[u8],
    closed: bool,
    positions: &packed::float3::view::View,
    view: &topology::view::View,
    corners: &[i64],
) -> Result<Vec<i64>, error::T> {
    let count = corners.len();
    let minimum = if closed { 3 } else { 2 };
    if count <= minimum {
        return Ok(corners.to_vec());
    }
    let mut keep = vec![true; count];
    let mut queued = vec![true; count];
    let mut previous = vec![0i64; count];
    let mut next = vec![0i64; count];
    let mut queue: Vec<usize> = (0..count).collect();
    for index in 0..count {
        previous[index] = if index == 0 {
            if closed { count as i64 - 1 } else { -1 }
        } else {
            index as i64 - 1
        };
        next[index] = if index + 1 == count {
            if closed { 0 } else { -1 }
        } else {
            index as i64 + 1
        };
    }
    let mut head = 0usize;
    let mut tail = 0usize;
    let mut queued_count = count;
    let mut active = count;
    let cosine = tolerance.cos();
    let point_of = |vertex: i64| view.vertex_points[vertex as usize] as usize;
    let inline = |index: usize, previous: &[i64], next: &[i64]| -> Result<bool, error::T> {
        let (left, right) = (previous[index], next[index]);
        if left < 0 || right < 0 {
            return Ok(false);
        }
        let a = point_of(corners[left as usize]);
        let b = point_of(corners[index]);
        let c = point_of(corners[right as usize]);
        if affected[b] != 1 {
            return Ok(false);
        }
        let (ux, uy, uz) = (positions.x[b] - positions.x[a], positions.y[b] - positions.y[a], positions.z[b] - positions.z[a]);
        let (vx, vy, vz) = (positions.x[c] - positions.x[b], positions.y[c] - positions.y[b], positions.z[c] - positions.z[b]);
        if ![ux, uy, uz, vx, vy, vz].iter().all(|v| v.is_finite()) {
            return Err(fail("Remove Inline Points encountered non-finite positions"));
        }
        let us = ux.abs().max(uy.abs().max(uz.abs()));
        let vs = vx.abs().max(vy.abs().max(vz.abs()));
        if us == 0.0 || vs == 0.0 {
            return Ok(true);
        }
        let (ux, uy, uz) = (ux / us, uy / us, uz / us);
        let (vx, vy, vz) = (vx / vs, vy / vs, vz / vs);
        let ul = ((ux * ux) + (uy * uy) + (uz * uz)).sqrt();
        let vl = ((vx * vx) + (vy * vy) + (vz * vz)).sqrt();
        let value = ((ux * vx) + (uy * vy) + (uz * vz)) / (ul * vl);
        Ok(value >= cosine - (64.0 * f64::EPSILON))
    };
    while queued_count > 0 && active > minimum {
        cancel::poll(cancel_value, head as i64, 4095)?;
        let index = queue[head];
        head = (head + 1) % count;
        queued_count -= 1;
        queued[index] = false;
        if keep[index] && inline(index, &previous, &next)? {
            let (left, right) = (previous[index], next[index]);
            keep[index] = false;
            active -= 1;
            if left >= 0 {
                next[left as usize] = right;
            }
            if right >= 0 {
                previous[right as usize] = left;
            }
            for item in [left, right] {
                if item >= 0 && keep[item as usize] && !queued[item as usize] {
                    queue[tail] = item as usize;
                    tail = (tail + 1) % count;
                    queued_count += 1;
                    queued[item as usize] = true;
                }
            }
        }
    }
    if active == count {
        Ok(corners.to_vec())
    } else {
        Ok((0..count).filter(|&index| keep[index]).map(|index| corners[index]).collect())
    }
}

fn rotate(value: &[i64], at: usize) -> Vec<i64> {
    (0..value.len()).map(|index| value[(at + index) % value.len()]).collect()
}

fn find_point(view: &topology::view::View, corners: &[i64], point: i64) -> i64 {
    for (index, &vertex) in corners.iter().enumerate() {
        if view.vertex_points[vertex as usize] == point {
            return index as i64;
        }
    }
    -1
}

fn bridge_loops(
    cancel_value: Option<&cancel::T>,
    view: &topology::view::View,
    index: &topology_index::view::View,
    selected_edges: &[u8],
    loops: &[Vec<i64>],
) -> Result<Vec<i64>, error::T> {
    let Some((first, rest)) = loops.split_first() else {
        return Err(fail("bridge component has no boundary"));
    };
    let mut current = first.clone();
    let mut pending: Vec<Vec<i64>> = rest.to_vec();
    while !pending.is_empty() {
        cancel::check_opt(cancel_value)?;
        let mut found_loop = -1i64;
        let mut found_current = -1i64;
        let mut found_other = -1i64;
        let mut edge = 0usize;
        while found_loop < 0 && edge < index.edge_a.len() {
            if selected(selected_edges, edge) {
                let (a, b) = (index.edge_a[edge], index.edge_b[edge]);
                let ca = find_point(view, &current, a);
                let cb = find_point(view, &current, b);
                let mut candidate = 0usize;
                while found_loop < 0 && candidate < pending.len() {
                    let oa = find_point(view, &pending[candidate], a);
                    let ob = find_point(view, &pending[candidate], b);
                    if ca >= 0 && ob >= 0 {
                        found_loop = candidate as i64;
                        found_current = ca;
                        found_other = ob;
                    } else if cb >= 0 && oa >= 0 {
                        found_loop = candidate as i64;
                        found_current = cb;
                        found_other = oa;
                    }
                    candidate += 1;
                }
            }
            edge += 1;
        }
        if found_loop < 0 {
            return Err(fail("bridge loops are not connected by a selected source edge"));
        }
        let left = rotate(&current, found_current as usize);
        let right = rotate(&pending[found_loop as usize], found_other as usize);
        let mut merged = Vec::with_capacity(left.len() + right.len() + 2);
        merged.extend_from_slice(&left);
        merged.push(left[0]);
        merged.extend_from_slice(&right);
        merged.push(right[0]);
        current = merged;
        pending.remove(found_loop as usize);
    }
    Ok(current)
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub edges: Option<edge_group::T>,
    pub operation: Operation,
    pub bridge_policy: BridgePolicy,
    pub remove_inline_points: bool,
    pub collinearity_tolerance: f64,
    pub remove_unused_points: bool,
    pub create_boundary_curves: bool,
    pub recompute_normals: bool,
}

fn dissolve(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = support::resolve(params.grain).map_err(|_| fail("grain must be positive"))?;
    let tolerance = params.collinearity_tolerance;
    if !tolerance.is_finite() || !(0.0..=std::f64::consts::PI).contains(&tolerance) {
        return Err(fail("collinearity tolerance must be finite and between zero and pi radians"));
    }
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(geometry_value);
    let view = topology::view::view(topology_value)?;
    let index_value = topology_index::create(cancel_value, topology_value)?;
    let index = topology_index::view::view(&index_value);
    let edge_count = index.edge_a.len();
    let primitive_count = view.primitive_kinds.len();
    let vertex_count = view.vertex_points.len();
    if let Some(group_value) = &params.edges {
        if edge_group::topology_data_id(group_value) != topology::data_id(topology_value) {
            return Err(fail("edge selection belongs to a different topology"));
        }
        if edge_group::length(group_value) != edge_count as i64 {
            return Err(fail("edge selection length does not match topology"));
        }
    }
    let mut selected_edges = vec![0u8; edge_count.div_ceil(8)];
    let mut affected_points = vec![0u8; view.point_count as usize];
    let mut selected_count = 0i64;
    for edge in 0..edge_count {
        cancel::poll(cancel_value, edge as i64, 4095)?;
        let requested = match (&params.edges, params.operation) {
            (None, Operation::DissolveSelected) => false,
            (None, Operation::DissolveNonSelected) => true,
            (Some(item), Operation::DissolveSelected) => edge_group::mem(edge as i64, item),
            (Some(item), Operation::DissolveNonSelected) => !edge_group::mem(edge as i64, item),
        };
        if requested {
            let first = index.edge_offsets[edge];
            let last = index.edge_offsets[edge + 1];
            let mut polygon = true;
            for slot in first..last {
                let primitive = index.primitive_of_vertex[index.edge_vertices[slot as usize] as usize];
                if view.primitive_kinds[primitive as usize] != 0 {
                    polygon = false;
                }
            }
            if polygon {
                if last - first > 2 {
                    return Err(fail(&format!("selected edge {edge} is non-manifold")));
                }
                set_selected(&mut selected_edges, edge);
                selected_count += 1;
                affected_points[index.edge_a[edge] as usize] = 1;
                affected_points[index.edge_b[edge] as usize] = 1;
            }
        }
    }
    if selected_count == 0 {
        return Ok(geometry_value.clone());
    }
    let point_of_vertex = |vertex: i64| view.vertex_points[vertex as usize];
    let mut parent = DisjointSet::new(primitive_count);
    for edge in 0..edge_count {
        if selected(&selected_edges, edge) {
            let first = index.edge_offsets[edge] as usize;
            let last = index.edge_offsets[edge + 1] as usize;
            if last - first == 2 {
                let left_vertex = index.edge_vertices[first];
                let right_vertex = index.edge_vertices[first + 1];
                let left_next = index.next_vertex[left_vertex as usize];
                let right_next = index.next_vertex[right_vertex as usize];
                if point_of_vertex(left_vertex) != point_of_vertex(right_next)
                    || point_of_vertex(left_next) != point_of_vertex(right_vertex)
                {
                    return Err(fail(&format!(
                        "selected manifold edge {edge} has inconsistent polygon winding"
                    )));
                }
                parent.union_min(
                    index.primitive_of_vertex[left_vertex as usize] as usize,
                    index.primitive_of_vertex[right_vertex as usize] as usize,
                );
            }
        }
    }
    for primitive in 0..primitive_count {
        let root = parent.find(primitive);
        parent.parent[primitive] = root as i64;
    }
    let mut component_selected = vec![false; primitive_count];
    let mut component_boundary = vec![false; primitive_count];
    for edge in 0..edge_count {
        if selected(&selected_edges, edge) {
            let first = index.edge_offsets[edge] as usize;
            let last = index.edge_offsets[edge + 1] as usize;
            if first < last {
                let root = parent.parent[index.primitive_of_vertex[index.edge_vertices[first] as usize] as usize] as usize;
                component_selected[root] = true;
                if last - first == 1 {
                    component_boundary[root] = true;
                }
            }
        }
    }
    let mut candidate = vec![false; vertex_count];
    let mut successor = vec![-2i64; vertex_count];
    let mut predecessor_count = vec![0u8; vertex_count];
    let mut component_vertices: Vec<Vec<i64>> = vec![Vec::new(); primitive_count];
    for vertex in 0..vertex_count {
        let primitive = index.primitive_of_vertex[vertex] as usize;
        if view.primitive_kinds[primitive] == 0 {
            let root = parent.parent[primitive] as usize;
            let edge = index.edge_of_vertex[vertex];
            if component_selected[root] && edge >= 0 && !selected(&selected_edges, edge as usize) {
                candidate[vertex] = true;
                component_vertices[root].push(vertex as i64);
            }
        }
    }
    let next_boundary = |vertex: usize| -> Result<i64, error::T> {
        let mut current = index.next_vertex[vertex];
        let mut steps = 0usize;
        while current >= 0 && selected(&selected_edges, index.edge_of_vertex[current as usize] as usize) {
            steps += 1;
            if steps > vertex_count {
                return Err(fail("selected edges form an invalid cycle"));
            }
            let edge = index.edge_of_vertex[current as usize] as usize;
            let first = index.edge_offsets[edge];
            let last = index.edge_offsets[edge + 1];
            if last - first == 1 {
                current = -1;
            } else {
                let opposite = index.opposite_vertex[current as usize];
                if opposite < 0 {
                    return Err(fail("selected edge has no manifold opposite"));
                }
                current = index.next_vertex[opposite as usize];
            }
        }
        Ok(current)
    };
    for vertex in 0..vertex_count {
        if candidate[vertex] {
            let next = next_boundary(vertex)?;
            successor[vertex] = next;
            if next >= 0 {
                if !candidate[next as usize] {
                    return Err(fail("dissolved component boundary escaped its component"));
                }
                let count = predecessor_count[next as usize] + 1;
                if count > 1 {
                    return Err(fail("dissolved component has branching boundary topology"));
                }
                predecessor_count[next as usize] = count;
            }
        }
    }
    let dense_selection = selected_count > (edge_count / 4) as i64;
    let initial_vertices = if dense_selection { vertex_count.min(4_096) } else { vertex_count };
    let initial_primitives = if dense_selection { primitive_count.min(4_096) } else { primitive_count };
    let mut visited = vec![false; vertex_count];
    let mut output = output::create(initial_vertices, initial_primitives);
    let mut processed = vec![false; primitive_count];
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    let trace = |start: i64, visited: &mut Vec<bool>| -> Result<(Vec<i64>, bool), error::T> {
        let mut values: Vec<i64> = Vec::new();
        let mut current = start;
        let mut closed = false;
        while current >= 0 && !visited[current as usize] {
            visited[current as usize] = true;
            values.push(current);
            let next = successor[current as usize];
            if next == start {
                closed = true;
                current = -1;
            } else {
                current = next;
            }
        }
        if current >= 0 {
            return Err(fail("dissolved boundary joins an earlier path"));
        }
        Ok((values, closed))
    };
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 1023)?;
        let root = parent.parent[primitive] as usize;
        if !component_selected[root] {
            let first = view.primitive_offsets[primitive];
            let last = view.primitive_offsets[primitive + 1];
            let corners: Vec<i64> = (first..last).collect();
            let kind = topology::primitive_kind(topology_value, primitive as i64)?;
            output::add(&mut output, primitive as i64, kind, &corners, &view.vertex_points);
        } else if !processed[root] {
            processed[root] = true;
            let mut vertices = component_vertices[root].clone();
            vertices.sort_unstable();
            let mut paths: Vec<(Vec<i64>, bool)> = Vec::new();
            for &vertex in &vertices {
                if !visited[vertex as usize] && predecessor_count[vertex as usize] == 0 {
                    paths.push(trace(vertex, &mut visited)?);
                }
            }
            for &vertex in &vertices {
                if !visited[vertex as usize] {
                    paths.push(trace(vertex, &mut visited)?);
                }
            }
            if component_boundary[root] {
                if params.create_boundary_curves {
                    for (corners, closed) in &paths {
                        let mut corners = corners.clone();
                        if !closed {
                            let last = corners.last().copied();
                            if let Some(last) = last {
                                corners.push(index.next_vertex[last as usize]);
                            }
                        }
                        if params.remove_inline_points {
                            corners = simplify(
                                cancel_value, tolerance, &affected_points, *closed, &positions, &view, &corners,
                            )?;
                        }
                        let kind = if *closed { PrimitiveKind::ClosedPolyline } else { PrimitiveKind::OpenPolyline };
                        output::add(&mut output, root as i64, kind, &corners, &view.vertex_points);
                    }
                }
            } else {
                let mut loops: Vec<Vec<i64>> = Vec::new();
                for (corners, closed) in &paths {
                    if !closed {
                        return Err(fail("interior dissolve produced an open boundary"));
                    }
                    loops.push(corners.clone());
                }
                let emit = |corners: &[i64], output: &mut output::T| -> Result<(), error::T> {
                    let corners = if params.remove_inline_points {
                        simplify(cancel_value, tolerance, &affected_points, true, &positions, &view, corners)?
                    } else {
                        corners.to_vec()
                    };
                    output::add(output, root as i64, PrimitiveKind::Polygon, &corners, &view.vertex_points);
                    Ok(())
                };
                if loops.is_empty() {
                } else if loops.len() == 1 {
                    emit(&loops[0], &mut output)?;
                } else {
                    match params.bridge_policy {
                        BridgePolicy::CreateDisjointPolygons => {
                            for item in &loops {
                                emit(item, &mut output)?;
                            }
                        }
                        BridgePolicy::DeleteBridgePolygons => {}
                        BridgePolicy::CreateBridgedPolygons => {
                            let bridged = bridge_loops(cancel_value, &view, &index, &selected_edges, &loops)?;
                            emit(&bridged, &mut output)?;
                        }
                    }
                }
            }
        }
    }
    let output_topology = topology::create_owned(
        view.point_count,
        output.points.values,
        output.offsets.values,
        output.kinds,
    )
    .map_err(recode)?;
    let had_point_normals = geometry::find_attribute(attribute::Owner::Point, "N", geometry_value).is_some();
    let had_vertex_normals = geometry::find_attribute(attribute::Owner::Vertex, "N", geometry_value).is_some();
    let mut result = topology_remap::preserving_points(
        cancel_value,
        grain,
        &output_topology,
        &output.vertices.values,
        &output.primitives.values,
        geometry_value,
    )
    .map_err(recode)?;
    if params.remove_unused_points {
        result = fuse_cleanup::apply(cancel_value, grain, false, false, true, &result).map_err(recode)?;
    }
    if !params.recompute_normals || !(had_point_normals || had_vertex_normals) {
        return Ok(result);
    }
    let result = geometry::without_attribute(attribute::Owner::Point, "N", &result);
    let result = geometry::without_attribute(attribute::Owner::Vertex, "N", &result);
    // ponytail: core Normal_ops owns its cancel token; the Arc token is checked just before the call.
    cancel::check_opt(cancel_value)?;
    normal_ops::run(
        &normal_ops::Params {
            cancel: None,
            grain: Some(grain),
            selection: None,
            primitives: None,
            owner: if had_point_normals { attribute::Owner::Point } else { attribute::Owner::Vertex },
            weighting: normal_ops::Weighting::FaceArea,
            cusp_angle: std::f64::consts::PI,
            attribute: "N".to_owned(),
        },
        &result,
    )
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    dissolve(params, geometry_value).map_err(|failure| {
        if error::is_cancelled(&failure) {
            error::cancelled("dissolve")
        } else {
            make("dissolve", Code::InvalidTopology, error::message(&failure).to_owned())
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};
    use rdk_core::group;

    fn params() -> Params {
        Params {
            cancel: None,
            grain: None,
            edges: None,
            operation: Operation::DissolveSelected,
            bridge_policy: BridgePolicy::CreateBridgedPolygons,
            remove_inline_points: false,
            collinearity_tolerance: 0.0,
            remove_unused_points: true,
            create_boundary_curves: false,
            recompute_normals: true,
        }
    }

    fn incidence_group(g: &geometry::T, name: &str, count: i64) -> edge_group::T {
        let index = topology_index::create(None, geometry::topology(g)).unwrap();
        edge_group_where(g, name, &move |edge, _, _| {
            topology_index::edge_incidence_count(&index, edge).unwrap() == count
        })
    }

    fn manifold_edges(g: &geometry::T) -> edge_group::T {
        incidence_group(g, "interior", 2)
    }

    fn boundary_edges(g: &geometry::T) -> edge_group::T {
        incidence_group(g, "boundary", 1)
    }

    fn add_payload(g: &geometry::T) -> geometry::T {
        let vertices = geometry::vertex_count(g);
        let primitives = geometry::primitive_count(g);
        let points = geometry::point_count(g);
        let g = with_attr(g, Owner::Point, "weight", Storage::Float(Arc::new((0..points).map(|v| v as f64).collect())));
        let g = with_attr(&g, Owner::Vertex, "corner", Storage::Int(Arc::new((0..vertices).map(|v| 100 + v).collect())));
        let g = with_attr(&g, Owner::Primitive, "face", Storage::Int(Arc::new((0..primitives).map(|v| 10 + v).collect())));
        let lists = packed::int_array::create_owned(
            (0..=vertices).map(|v| v * 2).collect(),
            (0..vertices * 2).map(|v| v % 7).collect(),
        )
        .unwrap();
        let g = with_attr(&g, Owner::Vertex, "neighbors", Storage::IntArray(lists));
        let ordered_corners =
            group::ordered(group::Owner::Vertex, "ordered_corners", vertices, &(0..vertices).rev().collect::<Vec<_>>()).unwrap();
        let ordered_faces =
            group::ordered(group::Owner::Primitive, "ordered_faces", primitives, &(0..primitives).rev().collect::<Vec<_>>()).unwrap();
        let g = geometry::with_group(ordered_corners, &g).unwrap();
        geometry::with_group(ordered_faces, &g).unwrap()
    }

    fn two_quads() -> geometry::T {
        let g = add_payload(&grid(2, 1, 2.0, true));
        geometry::with_edge_group(boundary_edges(&g), &g).unwrap()
    }

    fn code(result: Result<geometry::T, error::T>) -> String {
        error::code(&err(result)).as_str().to_owned()
    }

    #[test]
    fn interior_dissolve_keeps_payload() {
        let source = two_quads();
        let edges = manifold_edges(&source);
        let out = run(&Params { edges: Some(edges.clone()), remove_unused_points: false, ..params() }, &source).unwrap();
        assert_eq!(
            (geometry::point_count(&out), geometry::vertex_count(&out), geometry::primitive_count(&out)),
            (6, 6, 1)
        );
        assert_eq!(ints(Owner::Primitive, "face", &out), vec![10]);
        assert_eq!(ints(Owner::Vertex, "corner", &out).len(), 6);
        let lists = geometry::find_attribute(Owner::Vertex, "neighbors", &out).unwrap();
        assert_eq!(attribute::length(&lists), 6);
        let ordered = geometry::find_group(group::Owner::Vertex, "ordered_corners", &out).unwrap();
        assert_eq!(group::cardinality(&ordered), 6);
        let faces = geometry::find_group(group::Owner::Primitive, "ordered_faces", &out).unwrap();
        assert_eq!(group::ordered_elements(&faces), Some(vec![0]));
        let boundary = geometry::find_edge_group("boundary", &out).unwrap();
        assert_eq!(edge_group::cardinality(&boundary), 6);
        let before = geometry::find_attribute(Owner::Point, "weight", &source).unwrap();
        let after = geometry::find_attribute(Owner::Point, "weight", &out).unwrap();
        assert_eq!(attribute::storage_id(&before), attribute::storage_id(&after), "point payload was copied");

        let cleaned = run(
            &Params {
                edges: Some(edges),
                remove_inline_points: true,
                collinearity_tolerance: 1e-10,
                ..params()
            },
            &source,
        )
        .unwrap();
        assert_eq!(
            (geometry::point_count(&cleaned), geometry::vertex_count(&cleaned), geometry::primitive_count(&cleaned)),
            (4, 4, 1)
        );
        let inverted = run(
            &Params {
                edges: Some(boundary_edges(&source)),
                operation: Operation::DissolveNonSelected,
                remove_unused_points: false,
                ..params()
            },
            &source,
        )
        .unwrap();
        assert_eq!((geometry::primitive_count(&inverted), geometry::vertex_count(&inverted)), (1, 6));
    }

    #[test]
    fn boundary_dissolve_deletes_or_makes_curves() {
        let source = grid(1, 1, 1.0, true);
        let selected = edge_group_where(&source, "one", &|edge, _, _| edge == 0);
        let deleted = run(&Params { edges: Some(selected.clone()), ..params() }, &source).unwrap();
        assert_eq!((geometry::point_count(&deleted), geometry::primitive_count(&deleted)), (0, 0));
        let curve =
            run(&Params { edges: Some(selected), create_boundary_curves: true, ..params() }, &source).unwrap();
        assert_eq!(
            (geometry::point_count(&curve), geometry::vertex_count(&curve), geometry::primitive_count(&curve)),
            (4, 4, 1)
        );
        assert_eq!(topology::primitive_kind(geometry::topology(&curve), 0).unwrap(), PrimitiveKind::OpenPolyline);
    }

    fn ring() -> geometry::T {
        geom(
            &[-2., 2., 2., -2., -1., 1., 1., -1.],
            &[0.; 8],
            &[-2., -2., 2., 2., -1., -1., 1., 1.],
            &[0, 1, 5, 4, 1, 2, 6, 5, 2, 3, 7, 6, 3, 0, 4, 7],
            &[0, 4, 8, 12, 16],
            None,
        )
    }

    #[test]
    fn bridge_policies() {
        let source = ring();
        let edges = manifold_edges(&source);
        let with = |policy: BridgePolicy, unused: bool| {
            run(
                &Params {
                    edges: Some(edges.clone()),
                    bridge_policy: policy,
                    remove_unused_points: unused,
                    ..params()
                },
                &source,
            )
            .unwrap()
        };
        let disjoint = with(BridgePolicy::CreateDisjointPolygons, false);
        assert_eq!((geometry::primitive_count(&disjoint), geometry::vertex_count(&disjoint)), (2, 8));
        let bridged = with(BridgePolicy::CreateBridgedPolygons, false);
        assert_eq!((geometry::primitive_count(&bridged), geometry::vertex_count(&bridged)), (1, 10));
        let deleted = with(BridgePolicy::DeleteBridgePolygons, true);
        assert_eq!((geometry::primitive_count(&deleted), geometry::point_count(&deleted)), (0, 0));
    }

    #[test]
    fn errors_and_curves() {
        let curve = polyline(&[(0., 0., 0.), (1., 0., 0.), (2., 0., 0.)]);
        let all = edge_group_where(&curve, "all", &|_, _, _| true);
        let out = run(&Params { edges: Some(all), ..params() }, &curve).unwrap();
        assert_eq!(geometry::data_id(&out), geometry::data_id(&curve), "polygon-curve edges were not ignored");

        let source = two_quads();
        let token = std::sync::Arc::new(cancel::create());
        cancel::cancel(&token);
        assert_eq!(
            code(run(&Params { cancel: Some(token), edges: Some(manifold_edges(&source)), ..params() }, &source)),
            "cancelled"
        );
        let foreign = manifold_edges(&ring());
        assert_eq!(code(run(&Params { edges: Some(foreign), ..params() }, &source)), "invalid_topology");

        let malformed = |vertex_points: &[i64]| {
            geom(
                &[0., 1., 0., 1., 0.5],
                &[0., 0., 1., 1., 2.],
                &[0.; 5],
                vertex_points,
                &(0..=(vertex_points.len() / 3) as i64).map(|p| p * 3).collect::<Vec<_>>(),
                None,
            )
        };
        let expect_invalid = |label: &str, g: geometry::T| {
            let edges = edge_group_where(&g, "bad", &|_, a, b| a.min(b) == 0 && a.max(b) == 1);
            assert_eq!(code(run(&Params { edges: Some(edges), ..params() }, &g)), "invalid_topology", "{label}");
        };
        expect_invalid("non-manifold", malformed(&[0, 1, 2, 1, 0, 3, 0, 1, 4]));
        expect_invalid("inconsistent winding", malformed(&[0, 1, 2, 0, 1, 3]));
        assert_eq!(code(run(&Params { collinearity_tolerance: f64::NAN, ..params() }, &source)), "invalid_topology");
    }

    #[test]
    fn dissolve_recomputes_unit_normals() {
        let source = two_quads();
        let count = geometry::point_count(&source) as usize;
        let source = with_attr(&source, Owner::Point, "N", float3_attr(vec![1.; count], vec![0.; count], vec![0.; count]));
        let out = run(
            &Params { edges: Some(manifold_edges(&source)), remove_inline_points: true, ..params() },
            &source,
        )
        .unwrap();
        let normal = geometry::find_attribute(Owner::Point, "N", &out).unwrap();
        assert_eq!(attribute::length(&normal), geometry::point_count(&out));
        let Storage::Float3(values) = attribute::view::storage(&normal).unwrap() else { panic!("storage") };
        let v = packed::float3::view::view(values).unwrap();
        for point in 0..v.x.len() {
            let length = (v.x[point] * v.x[point] + v.y[point] * v.y[point] + v.z[point] * v.z[point]).sqrt();
            assert!((length - 1.0).abs() < 1e-12);
            assert!((v.x[point] - 1.0).abs() > 1e-12 || v.y[point].abs() > 1e-12 || v.z[point].abs() > 1e-12);
        }
    }

    #[test]
    fn large_grid_collapses_to_a_rectangle() {
        let source = add_payload(&grid(300, 220, 20.0, true));
        let edges = manifold_edges(&source);
        let out = exact_at_one_and_four(&|| {
            run(
                &Params {
                    grain: Some(257),
                    edges: Some(edges.clone()),
                    remove_inline_points: true,
                    collinearity_tolerance: 1e-10,
                    ..params()
                },
                &source,
            )
            .unwrap()
        });
        assert_eq!(
            (geometry::point_count(&out), geometry::vertex_count(&out), geometry::primitive_count(&out)),
            (4, 4, 1)
        );
    }
}
