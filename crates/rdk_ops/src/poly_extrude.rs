use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use rdk_core::{
    attribute, cancel, edge_group, error, face_normals, geometry, group, packed, support, topology,
    topology_edge_lookup, topology_index, topology_remap,
};
use rdk_core::error::Code;
use rdk_core::support::Windows;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Divide {
    ExtrudeIndividual,
    ExtrudeConnectedComponents,
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub primitives: Option<group::T>,
    pub split_edges: Option<edge_group::T>,
    pub divide: Divide,
    pub divisions: i64,
    pub output_front: bool,
    pub output_back: bool,
    pub output_side: bool,
    pub front_group: Option<String>,
    pub back_group: Option<String>,
    pub side_group: Option<String>,
    pub front_boundary_group: Option<String>,
    pub back_boundary_group: Option<String>,
    pub distance: f64,
}

impl Params {
    /// The defaults of the OCaml test front end (`Poly_extrude_front.run`).
    pub fn new(distance: f64) -> Params {
        Params {
            cancel: None,
            grain: None,
            primitives: None,
            split_edges: None,
            divide: Divide::ExtrudeIndividual,
            divisions: 1,
            output_front: true,
            output_back: true,
            output_side: true,
            front_group: None,
            back_group: None,
            side_group: None,
            front_boundary_group: None,
            back_boundary_group: None,
            distance,
        }
    }
}

fn make(code: Code, message: &str) -> error::T {
    error::new("poly_extrude", code, message.to_owned())
}

fn fail(message: &str) -> error::T {
    make(Code::InvalidGeometry, message)
}

fn blank(value: &str) -> bool {
    value.bytes().all(|b| matches!(b, b' ' | 0x0c | b'\n' | b'\r' | b'\t'))
}

fn find_root(parent: &mut [i64], value: usize) -> usize {
    let mut root = value;
    while parent[root] as usize != root {
        root = parent[root] as usize;
    }
    let mut current = value;
    while parent[current] as usize != root {
        let next = parent[current] as usize;
        parent[current] = root as i64;
        current = next;
    }
    root
}

fn union_roots(parent: &mut [i64], left: usize, right: usize) {
    let left = find_root(parent, left);
    let right = find_root(parent, right);
    if left != right {
        if left < right {
            parent[right] = left as i64;
        } else {
            parent[left] = right as i64;
        }
    }
}

fn merge_group(item: group::T, groups: Vec<group::T>) -> Result<Vec<group::T>, error::T> {
    let mut groups = groups;
    for existing in groups.iter_mut() {
        if group::owner(existing) == group::owner(&item) && group::name(existing) == group::name(&item) {
            *existing = group::union(existing, &item)?;
            return Ok(groups);
        }
    }
    groups.push(item);
    Ok(groups)
}

fn merge_edge_group(item: edge_group::T, groups: Vec<edge_group::T>) -> Result<Vec<edge_group::T>, error::T> {
    let mut groups = groups;
    for existing in groups.iter_mut() {
        if edge_group::name(existing) == edge_group::name(&item) {
            *existing = edge_group::union(existing, &item)?;
            return Ok(groups);
        }
    }
    groups.push(item);
    Ok(groups)
}

fn validate_name(label: &str, name: &Option<String>) -> Result<(), error::T> {
    match name {
        Some(name) if blank(name) => Err(fail(&format!(
            "Rdk_mesh.Poly_extrude.poly_extrude: empty {label} group name"
        ))),
        _ => Ok(()),
    }
}

fn remap_attributes(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    vertex_map: &[i64],
    primitive_map: &[i64],
    geometry_value: &geometry::T,
) -> Result<Vec<attribute::T>, error::T> {
    let drop = topology_remap::drop_point_vertex_normals();
    let mut result = Vec::new();
    for source in geometry::attributes(geometry_value) {
        if let Some(mapped) = topology_remap::mapped_attribute(
            cancel_ref,
            grain,
            point_map,
            vertex_map,
            primitive_map,
            &drop,
            &source,
        )? {
            result.push(mapped);
        }
    }
    Ok(result)
}

fn remap_groups(
    grain: i64,
    point_map: &[i64],
    vertex_map: &[i64],
    primitive_map: &[i64],
    geometry_value: &geometry::T,
) -> Result<Vec<group::T>, error::T> {
    let mut result = Vec::new();
    for source in geometry::groups(geometry_value) {
        result.push(topology_remap::mapped_group(grain, point_map, vertex_map, primitive_map, &source)?);
    }
    Ok(result)
}

fn run_default(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    distance: f64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if !distance.is_finite() {
        return Err(fail("Rdk_mesh.Poly_extrude.poly_extrude: distance must be finite"));
    }
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let source_vertices = geometry::vertex_count(geometry_value) as usize;
    let source_primitives = geometry::primitive_count(geometry_value) as usize;
    for primitive in 0..source_primitives {
        if view.primitive_kinds[primitive] != 0 {
            return Err(fail(&format!(
                "Rdk_mesh.Poly_extrude.poly_extrude: primitive {primitive} is a curve"
            )));
        }
    }
    let output_points = source_vertices * 2;
    let output_vertices = source_vertices * 6;
    let output_primitives = source_primitives * 2 + source_vertices;
    let (mut px, mut py, mut pz) = (vec![0.0f64; output_points], vec![0.0f64; output_points], vec![0.0f64; output_points]);
    let mut point_map = vec![0i64; output_points];
    let mut vertex_points = vec![0i64; output_vertices];
    let mut vertex_map = vec![0i64; output_vertices];
    let mut primitive_offsets = vec![0i64; output_primitives + 1];
    let mut primitive_map = vec![0i64; output_primitives];
    let primitive_kinds = vec![0u8; output_primitives];
    let ranges = support::range_count(source_primitives as i64, grain);
    // Range `r` owns the output of primitives `r * grain..`: points `2 * first_vertex`, vertices `6 * first_vertex`,
    // primitives `2 * first_primitive + first_vertex` onwards.
    let first_primitive = |range: i64| range.saturating_mul(grain).min(source_primitives as i64) as usize;
    let first_vertex = |range: i64| view.primitive_offsets[first_primitive(range)] as usize;
    {
        let (wx, wy, wz) = (
            Windows::new(&mut px, ranges, |r| first_vertex(r) * 2),
            Windows::new(&mut py, ranges, |r| first_vertex(r) * 2),
            Windows::new(&mut pz, ranges, |r| first_vertex(r) * 2),
        );
        let wpoint = Windows::new(&mut point_map, ranges, |r| first_vertex(r) * 2);
        let (wvp, wvm) = (
            Windows::new(&mut vertex_points, ranges, |r| first_vertex(r) * 6),
            Windows::new(&mut vertex_map, ranges, |r| first_vertex(r) * 6),
        );
        let primitive_start = |r| first_primitive(r) * 2 + first_vertex(r);
        let (woff, wpm) = (
            Windows::new(&mut primitive_offsets, ranges, primitive_start),
            Windows::new(&mut primitive_map, ranges, primitive_start),
        );
        support::run_indexed_ranges(grain, source_primitives as i64, &|range, begin, end| {
            let (px, py, pz, point_map) = (wx.take(range), wy.take(range), wz.take(range), wpoint.take(range));
            let (vertex_points, vertex_map) = (wvp.take(range), wvm.take(range));
            let (primitive_offsets, primitive_map) = (woff.take(range), wpm.take(range));
            let vertex_base = first_vertex(range);
            let primitive_base = primitive_start(range);
            for primitive in begin..end {
                support::poll(cancel_ref, primitive)?;
                let primitive = primitive as usize;
                let first = view.primitive_offsets[primitive] as usize;
                let last = view.primitive_offsets[primitive + 1] as usize;
                let count = last - first;
                let (mut nx, mut ny, mut nz) = (0.0f64, 0.0f64, 0.0f64);
                for local in 0..count {
                    let next = (local + 1) % count;
                    let a = view.vertex_points[first + local] as usize;
                    let b = view.vertex_points[first + next] as usize;
                    let (ax, ay, az) = (positions.x[a], positions.y[a], positions.z[a]);
                    let (bx, by, bz) = (positions.x[b], positions.y[b], positions.z[b]);
                    nx += (ay - by) * (az + bz);
                    ny += (az - bz) * (ax + bx);
                    nz += (ax - bx) * (ay + by);
                }
                let length = (nx * nx + ny * ny + nz * nz).sqrt();
                if length <= 1e-20 || !length.is_finite() {
                    return Err(fail(&format!(
                        "Rdk_mesh.Poly_extrude.poly_extrude: primitive {primitive} is degenerate"
                    )));
                }
                let dx = distance * nx / length;
                let dy = distance * ny / length;
                let dz = distance * nz / length;
                let output_vertex = first * 6 - vertex_base * 6;
                let output_primitive = primitive * 2 + first - primitive_base;
                for local in 0..count {
                    let vertex = first + local;
                    let point = view.vertex_points[vertex] as usize;
                    let bottom = (vertex - vertex_base) * 2;
                    px[bottom] = positions.x[point];
                    py[bottom] = positions.y[point];
                    pz[bottom] = positions.z[point];
                    px[bottom + 1] = positions.x[point] + dx;
                    py[bottom + 1] = positions.y[point] + dy;
                    pz[bottom + 1] = positions.z[point] + dz;
                    point_map[bottom] = point as i64;
                    point_map[bottom + 1] = point as i64;
                    let reversed_vertex = first + count - local - 1;
                    vertex_points[output_vertex + local] = (reversed_vertex * 2) as i64;
                    vertex_map[output_vertex + local] = reversed_vertex as i64;
                    vertex_points[output_vertex + count + local] = (vertex * 2 + 1) as i64;
                    vertex_map[output_vertex + count + local] = vertex as i64;
                }
                let global_vertex = first * 6;
                primitive_offsets[output_primitive] = global_vertex as i64;
                primitive_offsets[output_primitive + 1] = (global_vertex + count) as i64;
                primitive_map[output_primitive] = primitive as i64;
                primitive_map[output_primitive + 1] = primitive as i64;
                for local in 0..count {
                    let next = (local + 1) % count;
                    let current_vertex = first + local;
                    let next_vertex = first + next;
                    let output = output_vertex + 2 * count + local * 4;
                    let output_side = output_primitive + 2 + local;
                    vertex_points[output] = (current_vertex * 2) as i64;
                    vertex_points[output + 1] = (next_vertex * 2) as i64;
                    vertex_points[output + 2] = (next_vertex * 2 + 1) as i64;
                    vertex_points[output + 3] = (current_vertex * 2 + 1) as i64;
                    vertex_map[output] = current_vertex as i64;
                    vertex_map[output + 1] = next_vertex as i64;
                    vertex_map[output + 2] = next_vertex as i64;
                    vertex_map[output + 3] = current_vertex as i64;
                    primitive_offsets[output_side] = (global_vertex + 2 * count + local * 4) as i64;
                    primitive_map[output_side] = primitive as i64;
                }
            }
            Ok(())
        })?;
    }
    primitive_offsets[output_primitives] = output_vertices as i64;
    let output_positions = packed::Float3::View::of_owned_exn(px, py, pz)?;
    let output_topology = topology::View::create_validated_owned(
        output_points as i64,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let attributes = remap_attributes(cancel_ref, grain, &point_map, &vertex_map, &primitive_map, geometry_value)?;
    let groups = remap_groups(grain, &point_map, &vertex_map, &primitive_map, geometry_value)?;
    let source_groups = geometry::edge_groups(geometry_value);
    let mut edge_groups = Vec::new();
    if !source_groups.is_empty() {
        let source_index = topology_index::create(cancel_ref, source_topology)?;
        let target_index = topology_index::create(cancel_ref, &output_topology)?;
        for source_group in &source_groups {
            let mut builder = edge_group::Builder::create(&output_topology, &target_index, edge_group::name(source_group))?;
            for edge in 0..edge_group::length(source_group) {
                if !edge_group::mem(edge, source_group) {
                    continue;
                }
                let incidence = topology_index::edge_incidence_count(&source_index, edge)?;
                for local in 0..incidence {
                    let current = topology_index::edge_vertex(&source_index, edge, local)?;
                    let next = topology_index::next_vertex(&source_index, current)?;
                    if next >= 0 {
                        for (a, b) in [(current * 2, next * 2), (current * 2 + 1, next * 2 + 1)] {
                            if let Some(target) = topology_index::find_edge(&target_index, a, b) {
                                edge_group::Builder::set(&mut builder, target, true)?;
                            }
                        }
                    }
                }
            }
            edge_groups.push(edge_group::Builder::freeze(&mut builder)?);
        }
    }
    geometry::create(output_positions, output_topology, Some(attributes), Some(groups), Some(edge_groups))
}

fn pair_find(table: &HashMap<(i64, i64), usize>, component: i64, point: i64) -> Result<usize, error::T> {
    table
        .get(&(component, point))
        .copied()
        .ok_or_else(|| fail("Poly Extrude: missing component/point association"))
}

#[allow(clippy::too_many_lines)]
fn run_general(
    params: &Params,
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let divisions = params.divisions;
    let distance = params.distance;
    let divide = params.divide;
    if divisions <= 0 {
        return Err(fail("Rdk_mesh.Poly_extrude.poly_extrude: divisions must be positive"));
    }
    if !distance.is_finite() {
        return Err(fail("Rdk_mesh.Poly_extrude.poly_extrude: distance must be finite"));
    }
    let source_topology = geometry::topology(geometry_value);
    match &params.primitives {
        Some(selection) if group::owner(selection) != group::Owner::Primitive => {
            return Err(fail("Rdk_mesh.Poly_extrude.poly_extrude: selection must own primitives"));
        }
        Some(selection) if group::length(selection) != geometry::primitive_count(geometry_value) => {
            return Err(fail(
                "Rdk_mesh.Poly_extrude.poly_extrude: selection length does not match primitive count",
            ));
        }
        _ => {}
    }
    validate_name("front", &params.front_group)?;
    validate_name("back", &params.back_group)?;
    validate_name("side", &params.side_group)?;
    validate_name("front boundary", &params.front_boundary_group)?;
    validate_name("back boundary", &params.back_boundary_group)?;
    match &params.split_edges {
        Some(_) if divide == Divide::ExtrudeIndividual => {
            return Err(fail(
                "Rdk_mesh.Poly_extrude.poly_extrude: split edges require connected-components mode",
            ));
        }
        Some(selection) if edge_group::topology_data_id(selection) != topology::data_id(source_topology) => {
            return Err(fail(
                "Rdk_mesh.Poly_extrude.poly_extrude: split edge group belongs to another topology",
            ));
        }
        _ => {}
    }
    let topology_view = topology::View::view(source_topology)?;
    let source_points = topology_view.point_count as usize;
    let source_primitives = topology_view.primitive_kinds.len();
    let selected: Vec<bool> = (0..source_primitives)
        .map(|primitive| match &params.primitives {
            None => true,
            Some(selection) => group::mem(primitive as i64, selection),
        })
        .collect();
    let selected_count = selected.iter().filter(|&&value| value).count();
    if selected_count == 0 {
        return Ok(geometry_value.clone());
    }
    let (face_nx, face_ny, face_nz) = face_normals::compute(
        cancel_ref,
        Some(grain),
        params.primitives.as_ref(),
        "Rdk_mesh.Poly_extrude.poly_extrude",
        geometry_value,
    )?;
    let index = topology_index::create(cancel_ref, source_topology)?;
    let index_view = topology_index::View::view(&index);
    let mut primitive_component = vec![-1i64; source_primitives];
    let component_count: i64 = match divide {
        Divide::ExtrudeIndividual => {
            let mut next = 0i64;
            for primitive in 0..source_primitives {
                if selected[primitive] {
                    primitive_component[primitive] = next;
                    next += 1;
                }
            }
            next
        }
        Divide::ExtrudeConnectedComponents => {
            let mut parent: Vec<i64> = (0..source_primitives)
                .map(|primitive| if selected[primitive] { primitive as i64 } else { -1 })
                .collect();
            let mut non_manifold: i64 = -1;
            for edge in 0..topology_index::edge_count(&index) as usize {
                cancel::poll(cancel_ref, edge as i64, 16_383)?;
                let first = index_view.edge_offsets[edge] as usize;
                let last = index_view.edge_offsets[edge + 1] as usize;
                let mut selected_incidence = 0;
                for at in first..last {
                    let primitive = index_view.primitive_of_vertex[index_view.edge_vertices[at] as usize] as usize;
                    if selected[primitive] {
                        selected_incidence += 1;
                    }
                }
                let split = match &params.split_edges {
                    None => false,
                    Some(selection) => edge_group::mem(edge as i64, selection),
                };
                if selected_incidence > 0 && last - first > 2 {
                    non_manifold = edge as i64;
                } else if selected_incidence > 1 && !split {
                    let mut base: i64 = -1;
                    for at in first..last {
                        let primitive =
                            index_view.primitive_of_vertex[index_view.edge_vertices[at] as usize] as usize;
                        if selected[primitive] {
                            if base < 0 {
                                base = primitive as i64;
                            } else {
                                union_roots(&mut parent, base as usize, primitive);
                            }
                        }
                    }
                }
            }
            if non_manifold >= 0 {
                return Err(fail(&format!(
                    "Rdk_mesh.Poly_extrude.poly_extrude: connected extrusion touches non-manifold edge {non_manifold}"
                )));
            }
            let mut root_component = vec![-1i64; source_primitives];
            let mut next = 0i64;
            for primitive in 0..source_primitives {
                if selected[primitive] {
                    let root = find_root(&mut parent, primitive);
                    if root_component[root] < 0 {
                        root_component[root] = next;
                        next += 1;
                    }
                    primitive_component[primitive] = root_component[root];
                }
            }
            next
        }
    };
    let _ = component_count;
    let mut selected_vertices = 0usize;
    for primitive in 0..source_primitives {
        cancel::poll(cancel_ref, primitive as i64, 4_095)?;
        if selected[primitive] {
            selected_vertices +=
                (topology_view.primitive_offsets[primitive + 1] - topology_view.primitive_offsets[primitive]) as usize;
        }
    }
    let mut boundary_vertices: Vec<usize> = Vec::new();
    for primitive in 0..source_primitives {
        cancel::poll(cancel_ref, primitive as i64, 4_095)?;
        if !selected[primitive] {
            continue;
        }
        let first = topology_view.primitive_offsets[primitive] as usize;
        let last = topology_view.primitive_offsets[primitive + 1] as usize;
        for vertex in first..last {
            let boundary = match divide {
                Divide::ExtrudeIndividual => true,
                Divide::ExtrudeConnectedComponents => {
                    let edge = index_view.edge_of_vertex[vertex] as usize;
                    match &params.split_edges {
                        Some(selection) if edge_group::mem(edge as i64, selection) => true,
                        _ => {
                            let edge_first = index_view.edge_offsets[edge] as usize;
                            let edge_last = index_view.edge_offsets[edge + 1] as usize;
                            if edge_last - edge_first == 1 {
                                true
                            } else {
                                let mut other: i64 = -1;
                                for at in edge_first..edge_last {
                                    let candidate =
                                        index_view.primitive_of_vertex[index_view.edge_vertices[at] as usize];
                                    if candidate != primitive as i64 {
                                        other = candidate;
                                    }
                                }
                                other < 0 || !selected[other as usize]
                            }
                        }
                    }
                }
            };
            if boundary {
                boundary_vertices.push(vertex);
            }
        }
    }
    let boundary_count = boundary_vertices.len();
    let output_front = params.output_front;
    let output_back = params.output_back;
    let output_side = params.output_side;
    let need_new_points = output_front || output_side;
    let mut association_table: HashMap<(i64, i64), usize> = HashMap::new();
    let mut association_sources: Vec<i64> = Vec::new();
    if need_new_points {
        for primitive in 0..source_primitives {
            cancel::poll(cancel_ref, primitive as i64, 4_095)?;
            if selected[primitive] {
                let component = primitive_component[primitive];
                let first = topology_view.primitive_offsets[primitive] as usize;
                let last = topology_view.primitive_offsets[primitive + 1] as usize;
                for vertex in first..last {
                    let point = topology_view.vertex_points[vertex];
                    if !association_table.contains_key(&(component, point)) {
                        association_table.insert((component, point), association_sources.len());
                        association_sources.push(point);
                    }
                }
            }
        }
    }
    let association_count = association_sources.len();
    let mut direction_x = vec![0.0f64; association_count];
    let mut direction_y = vec![0.0f64; association_count];
    let mut direction_z = vec![0.0f64; association_count];
    if need_new_points {
        for primitive in 0..source_primitives {
            cancel::poll(cancel_ref, primitive as i64, 4_095)?;
            if selected[primitive] {
                let component = primitive_component[primitive];
                let first = topology_view.primitive_offsets[primitive] as usize;
                let last = topology_view.primitive_offsets[primitive + 1] as usize;
                for vertex in first..last {
                    let point = topology_view.vertex_points[vertex];
                    let association = pair_find(&association_table, component, point)?;
                    direction_x[association] += face_nx[primitive];
                    direction_y[association] += face_ny[primitive];
                    direction_z[association] += face_nz[primitive];
                }
            }
        }
        let invalid_direction = AtomicBool::new(false);
        let (wx, wy, wz) = (
            Windows::even(&mut direction_x, grain),
            Windows::even(&mut direction_y, grain),
            Windows::even(&mut direction_z, grain),
        );
        support::run_indexed_ranges(grain, association_count as i64, &|range, begin, end| {
            let (dir_x, dir_y, dir_z) = (wx.take(range), wy.take(range), wz.take(range));
            for association in begin..end {
                support::poll(cancel_ref, association)?;
                let slot = (association - begin) as usize;
                let (x, y, z) = (dir_x[slot], dir_y[slot], dir_z[slot]);
                let scale = x.abs().max(y.abs().max(z.abs()));
                if scale == 0.0 || !scale.is_finite() {
                    invalid_direction.store(true, Ordering::Relaxed);
                } else {
                    let (x, y, z) = (x / scale, y / scale, z / scale);
                    let inverse = 1.0 / (x * x + y * y + z * z).sqrt();
                    dir_x[slot] = x * inverse;
                    dir_y[slot] = y * inverse;
                    dir_z[slot] = z * inverse;
                }
            }
            Ok(())
        })?;
        if invalid_direction.load(Ordering::Relaxed) {
            return Err(fail("Rdk_mesh.Poly_extrude.poly_extrude: selected point normals cancel to zero"));
        }
    }
    let mut base_index = vec![-1i64; source_primitives];
    let mut front_index = vec![-1i64; source_primitives];
    let mut base_primitives = 0usize;
    let mut base_vertices = 0usize;
    let mut front_cursor = 0usize;
    // First output vertex (within the base / front region) of every source primitive; they bound the windows below.
    let mut base_start = Vec::with_capacity(source_primitives + 1);
    let mut front_start = Vec::with_capacity(source_primitives + 1);
    for primitive in 0..source_primitives {
        cancel::poll(cancel_ref, primitive as i64, 4_095)?;
        let count = (topology_view.primitive_offsets[primitive + 1] - topology_view.primitive_offsets[primitive]) as usize;
        base_start.push(base_vertices);
        front_start.push(front_cursor);
        if !selected[primitive] || output_back {
            base_index[primitive] = base_primitives as i64;
            base_primitives += 1;
            base_vertices += count;
        }
        if output_front && selected[primitive] {
            front_cursor += count;
        }
    }
    base_start.push(base_vertices);
    front_start.push(front_cursor);
    let divisions_u = divisions as usize;
    let front_primitives = if output_front { selected_count } else { 0 };
    let front_vertices = if output_front { selected_vertices } else { 0 };
    let side_primitives = if output_side { boundary_count * divisions_u } else { 0 };
    let side_vertices = if output_side { 4 * boundary_count * divisions_u } else { 0 };
    let output_points = source_points + if need_new_points { association_count * divisions_u } else { 0 };
    let output_primitives = base_primitives + front_primitives + side_primitives;
    let output_vertices = base_vertices + front_vertices + side_vertices;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let (mut px, mut py, mut pz) = (vec![0.0f64; output_points], vec![0.0f64; output_points], vec![0.0f64; output_points]);
    let mut point_map = vec![0i64; output_points];
    for point in 0..source_points {
        px[point] = positions.x[point];
        py[point] = positions.y[point];
        pz[point] = positions.z[point];
        point_map[point] = point as i64;
    }
    let new_point_count = output_points - source_points;
    {
        let (wx, wy, wz, wmap) = (
            Windows::even(&mut px[source_points..], grain),
            Windows::even(&mut py[source_points..], grain),
            Windows::even(&mut pz[source_points..], grain),
            Windows::even(&mut point_map[source_points..], grain),
        );
        support::run_indexed_ranges(grain, new_point_count as i64, &|range, begin, end| {
            let (px, py, pz, point_map) = (wx.take(range), wy.take(range), wz.take(range), wmap.take(range));
            for local in begin..end {
                support::poll(cancel_ref, local)?;
                let slot = (local - begin) as usize;
                let local = local as usize;
                let association = local / divisions_u;
                let layer = local % divisions_u + 1;
                let point = association_sources[association] as usize;
                let t = layer as f64 / divisions as f64;
                px[slot] = positions.x[point] + distance * t * direction_x[association];
                py[slot] = positions.y[point] + distance * t * direction_y[association];
                pz[slot] = positions.z[point] + distance * t * direction_z[association];
                point_map[slot] = point as i64;
            }
            Ok(())
        })?;
    }
    let mut vertex_points = vec![0i64; output_vertices];
    let mut vertex_map = vec![0i64; output_vertices];
    let mut primitive_offsets = vec![0i64; output_primitives + 1];
    let mut primitive_map = vec![0i64; output_primitives];
    let mut primitive_kinds = vec![0u8; output_primitives];
    let mut front_membership = vec![0u8; output_primitives];
    let mut back_membership = vec![0u8; output_primitives];
    let mut side_membership = vec![0u8; output_primitives];
    let mut primitive_cursor = 0usize;
    let mut vertex_cursor = 0usize;
    for primitive in 0..source_primitives {
        cancel::poll(cancel_ref, primitive as i64, 4_095)?;
        if base_index[primitive] >= 0 {
            let output = primitive_cursor;
            base_index[primitive] = output as i64;
            primitive_offsets[output] = vertex_cursor as i64;
            primitive_map[output] = primitive as i64;
            primitive_kinds[output] = topology_view.primitive_kinds[primitive];
            if selected[primitive] {
                back_membership[output] = 1;
            }
            vertex_cursor +=
                (topology_view.primitive_offsets[primitive + 1] - topology_view.primitive_offsets[primitive]) as usize;
            primitive_cursor += 1;
        }
    }
    if output_front {
        for primitive in 0..source_primitives {
            cancel::poll(cancel_ref, primitive as i64, 4_095)?;
            if selected[primitive] {
                let output = primitive_cursor;
                front_index[primitive] = output as i64;
                primitive_offsets[output] = vertex_cursor as i64;
                primitive_map[output] = primitive as i64;
                front_membership[output] = 1;
                vertex_cursor += (topology_view.primitive_offsets[primitive + 1]
                    - topology_view.primitive_offsets[primitive]) as usize;
                primitive_cursor += 1;
            }
        }
    }
    let side_first_primitive = primitive_cursor;
    let side_first_vertex = vertex_cursor;
    if output_side {
        for boundary in 0..boundary_count {
            for division in 0..divisions_u {
                let output = side_first_primitive + boundary * divisions_u + division;
                primitive_offsets[output] = (side_first_vertex + (boundary * divisions_u + division) * 4) as i64;
                primitive_map[output] = index_view.primitive_of_vertex[boundary_vertices[boundary]];
                side_membership[output] = 1;
            }
        }
    }
    primitive_offsets[output_primitives] = output_vertices as i64;
    // Base, front and side vertices live in three consecutive regions of the vertex arrays.
    let (base_points, rest) = vertex_points.split_at_mut(base_vertices);
    let (front_points, side_points) = rest.split_at_mut(front_vertices);
    let (base_map, rest) = vertex_map.split_at_mut(base_vertices);
    let (front_map, side_map) = rest.split_at_mut(front_vertices);
    let first_primitive = |range: i64| range.saturating_mul(grain).min(source_primitives as i64) as usize;
    let ranges = support::range_count(source_primitives as i64, grain);
    let (wbp, wbm, wfp, wfm) = (
        Windows::new(base_points, ranges, |r| base_start[first_primitive(r)]),
        Windows::new(base_map, ranges, |r| base_start[first_primitive(r)]),
        Windows::new(front_points, ranges, |r| front_start[first_primitive(r)]),
        Windows::new(front_map, ranges, |r| front_start[first_primitive(r)]),
    );
    support::run_indexed_ranges(grain, source_primitives as i64, &|range, begin, end| {
        let (base_points, base_map) = (wbp.take(range), wbm.take(range));
        let (front_points, front_map) = (wfp.take(range), wfm.take(range));
        let (base_origin, front_origin) = (base_start[begin as usize], front_start[begin as usize]);
        for primitive in begin..end {
            support::poll(cancel_ref, primitive)?;
            let primitive = primitive as usize;
            let first = topology_view.primitive_offsets[primitive] as usize;
            let last = topology_view.primitive_offsets[primitive + 1] as usize;
            if base_index[primitive] >= 0 {
                let output = base_start[primitive] - base_origin;
                for vertex in first..last {
                    let local = vertex - first;
                    base_points[output + local] = topology_view.vertex_points[vertex];
                    base_map[output + local] = vertex as i64;
                }
            }
            if front_index[primitive] >= 0 {
                let output = front_start[primitive] - front_origin;
                let component = primitive_component[primitive];
                for vertex in first..last {
                    let local = vertex - first;
                    let point = topology_view.vertex_points[vertex];
                    let association = pair_find(&association_table, component, point)?;
                    front_points[output + local] = (source_points + association * divisions_u + divisions_u - 1) as i64;
                    front_map[output + local] = vertex as i64;
                }
            }
        }
        Ok(())
    })?;
    let layer_point = |point: i64, association: usize, layer: usize| -> i64 {
        if layer == 0 {
            point
        } else {
            (source_points + association * divisions_u + layer - 1) as i64
        }
    };
    if output_side {
        let side_ranges = support::range_count(boundary_count as i64, grain);
        let first_boundary = |range: i64| range.saturating_mul(grain).min(boundary_count as i64) as usize;
        let (wsp, wsm) = (
            Windows::new(side_points, side_ranges, |r| first_boundary(r) * divisions_u * 4),
            Windows::new(side_map, side_ranges, |r| first_boundary(r) * divisions_u * 4),
        );
        support::run_indexed_ranges(grain, boundary_count as i64, &|range, begin, end| {
            let (side_points, side_map) = (wsp.take(range), wsm.take(range));
            for boundary in begin..end {
                support::poll(cancel_ref, boundary)?;
                let local_boundary = (boundary - begin) as usize;
                let boundary = boundary as usize;
                let vertex = boundary_vertices[boundary];
                let next = index_view.next_vertex[vertex] as usize;
                let primitive = index_view.primitive_of_vertex[vertex] as usize;
                let component = primitive_component[primitive];
                let point = topology_view.vertex_points[vertex];
                let next_point = topology_view.vertex_points[next];
                let association = pair_find(&association_table, component, point)?;
                let next_association = pair_find(&association_table, component, next_point)?;
                for division in 0..divisions_u {
                    let output = (local_boundary * divisions_u + division) * 4;
                    side_points[output] = layer_point(point, association, division);
                    side_points[output + 1] = layer_point(next_point, next_association, division);
                    side_points[output + 2] = layer_point(next_point, next_association, division + 1);
                    side_points[output + 3] = layer_point(point, association, division + 1);
                    side_map[output] = vertex as i64;
                    side_map[output + 1] = next as i64;
                    side_map[output + 2] = next as i64;
                    side_map[output + 3] = vertex as i64;
                }
            }
            Ok(())
        })?;
    }
    let output_positions = packed::Float3::View::of_owned_exn(px, py, pz)?;
    let output_topology = topology::View::create_validated_owned(
        output_points as i64,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let attributes = remap_attributes(cancel_ref, grain, &point_map, &vertex_map, &primitive_map, geometry_value)?;
    let mut groups = remap_groups(grain, &point_map, &vertex_map, &primitive_map, geometry_value)?;
    let add_role = |name: &Option<String>,
                    membership: &Vec<u8>,
                    groups: Vec<group::T>|
     -> Result<Vec<group::T>, error::T> {
        match name {
            None => Ok(groups),
            Some(name) => merge_group(
                group::init(Some(grain), group::Owner::Primitive, name, output_primitives as i64, &|primitive| {
                    membership[primitive as usize] != 0
                })?,
                groups,
            ),
        }
    };
    groups = add_role(&params.front_group, &front_membership, groups)?;
    groups = add_role(&params.back_group, &back_membership, groups)?;
    groups = add_role(&params.side_group, &side_membership, groups)?;
    let source_edge_groups = geometry::edge_groups(geometry_value);
    let needs_edge_output =
        !source_edge_groups.is_empty() || params.front_boundary_group.is_some() || params.back_boundary_group.is_some();
    let mut edge_groups: Vec<edge_group::T> = Vec::new();
    if needs_edge_output {
        let target_edges =
            topology_edge_lookup::create(cancel_ref, &topology::View::view(&output_topology)?)?;
        let edge_count = topology_edge_lookup::count(&target_edges) as usize;
        let make_bits = || vec![0u8; edge_count.div_ceil(8)];
        let add_edge = |bits: &mut Vec<u8>, a: i64, b: i64| {
            let edge = topology_edge_lookup::find(&target_edges, a, b);
            if edge >= 0 {
                bits[(edge >> 3) as usize] |= 1 << (edge & 7);
            }
        };
        let freeze = |name: &str, bits: Vec<u8>| -> Result<edge_group::T, error::T> {
            edge_group::View::of_owned_bits(&output_topology, edge_count as i64, name, bits)
        };
        for source_group in &source_edge_groups {
            let mut bits = make_bits();
            for edge in 0..edge_group::length(source_group) {
                if !edge_group::mem(edge, source_group) {
                    continue;
                }
                cancel::poll(cancel_ref, edge, 16_383)?;
                let (a, b) = topology_index::edge_points(&index, edge)?;
                add_edge(&mut bits, a, b);
                if need_new_points {
                    let first = index_view.edge_offsets[edge as usize] as usize;
                    let last = index_view.edge_offsets[edge as usize + 1] as usize;
                    for at in first..last {
                        let primitive =
                            index_view.primitive_of_vertex[index_view.edge_vertices[at] as usize] as usize;
                        if selected[primitive] {
                            let component = primitive_component[primitive];
                            let association_a = pair_find(&association_table, component, a)?;
                            let association_b = pair_find(&association_table, component, b)?;
                            for layer in 1..=divisions_u {
                                add_edge(
                                    &mut bits,
                                    layer_point(a, association_a, layer),
                                    layer_point(b, association_b, layer),
                                );
                            }
                        }
                    }
                }
            }
            edge_groups.push(freeze(edge_group::name(source_group), bits)?);
        }
        let add_boundary = |name: &Option<String>,
                            layer: usize,
                            edge_groups: Vec<edge_group::T>|
         -> Result<Vec<edge_group::T>, error::T> {
            match name {
                None => Ok(edge_groups),
                Some(name) => {
                    let mut bits = make_bits();
                    for (boundary, &vertex) in boundary_vertices.iter().enumerate() {
                        cancel::poll(cancel_ref, boundary as i64, 4_095)?;
                        let next = index_view.next_vertex[vertex] as usize;
                        let primitive = index_view.primitive_of_vertex[vertex] as usize;
                        let component = primitive_component[primitive];
                        let a = topology_view.vertex_points[vertex];
                        let b = topology_view.vertex_points[next];
                        if layer == 0 {
                            add_edge(&mut bits, a, b);
                        } else {
                            let association_a = pair_find(&association_table, component, a)?;
                            let association_b = pair_find(&association_table, component, b)?;
                            add_edge(
                                &mut bits,
                                layer_point(a, association_a, layer),
                                layer_point(b, association_b, layer),
                            );
                        }
                    }
                    merge_edge_group(freeze(name, bits)?, edge_groups)
                }
            }
        };
        edge_groups = add_boundary(&params.back_boundary_group, 0, edge_groups)?;
        if need_new_points {
            edge_groups = add_boundary(&params.front_boundary_group, divisions_u, edge_groups)?;
        } else if let Some(name) = &params.front_boundary_group {
            edge_groups = merge_edge_group(freeze(name, make_bits())?, edge_groups)?;
        }
    }
    geometry::create(output_positions, output_topology, Some(attributes), Some(groups), Some(edge_groups))
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(1_024);
    let cancel_ref = params.cancel.as_deref();
    let result = if grain <= 0 {
        Err(make(Code::InvalidArgument, "Rdk_mesh.Poly_extrude.poly_extrude: grain must be positive"))
    } else if params.primitives.is_none()
        && params.split_edges.is_none()
        && params.divide == Divide::ExtrudeIndividual
        && params.divisions == 1
        && params.output_front
        && params.output_back
        && params.output_side
        && params.front_group.is_none()
        && params.back_group.is_none()
        && params.side_group.is_none()
        && params.front_boundary_group.is_none()
        && params.back_boundary_group.is_none()
    {
        run_default(cancel_ref, grain, params.distance, geometry_value)
    } else {
        run_general(params, cancel_ref, grain, geometry_value)
    };
    result.map_err(|value| match error::kind(&value) {
        Code::Cancelled => error::cancelled("poly_extrude"),
        Code::InvalidArgument => value,
        _ => make(Code::InvalidGeometry, error::message(&value)),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plane_clip::test_util::*;
    use rdk_core::topology::PrimitiveKind;

    fn grid(columns: usize, rows: usize) -> geometry::T {
        let (mut x, mut y, mut z) = (Vec::new(), Vec::new(), Vec::new());
        for r in 0..=rows {
            for c in 0..=columns {
                x.push(c as f64 * 20.0 / columns as f64);
                y.push(0.0);
                z.push(r as f64 * 20.0 / rows as f64);
            }
        }
        let width = (columns + 1) as i64;
        let mut polys = Vec::new();
        for r in 0..rows as i64 {
            for c in 0..columns as i64 {
                let p = r * width + c;
                polys.push(vec![p, p + width, p + width + 1]);
                polys.push(vec![p, p + width + 1, p + 1]);
            }
        }
        polygons(x, y, z, &polys)
    }

    fn cardinality(source: &geometry::T, name: &str) -> i64 {
        group::cardinality(&geometry::find_group(group::Owner::Primitive, name, source).expect("missing group"))
    }

    #[test]
    fn mixed_curve_and_manifold_checks() {
        let mixed = geom(
            vec![-1.0, 1.0, -1.0, 1.0, 3.0, 4.0],
            vec![0.0; 6],
            vec![-1.0, -1.0, 1.0, 1.0, 0.0, 0.0],
            &[
                (PrimitiveKind::Polygon, vec![0, 1, 3]),
                (PrimitiveKind::Polygon, vec![0, 3, 2]),
                (PrimitiveKind::OpenPolyline, vec![4, 5]),
            ],
        );
        let polygon_selection = group::init(None, group::Owner::Primitive, "polygons", 3, &|p| p < 2).unwrap();
        let mixed = geometry::with_group(polygon_selection.clone(), &mixed).unwrap();
        let mut p = Params::new(1.0);
        p.primitives = Some(polygon_selection);
        p.divide = Divide::ExtrudeConnectedComponents;
        let output = run(&p, &mixed).unwrap();
        assert_eq!(geometry::primitive_count(&output), 9);
        let kinds = topo(&output).primitive_kinds;
        assert!(kinds.iter().any(|&k| k == 1), "did not preserve an unselected curve");
        let curve_selection = group::init(None, group::Owner::Primitive, "curve", 3, &|p| p == 2).unwrap();
        let mut p = Params::new(1.0);
        p.primitives = Some(curve_selection);
        p.divide = Divide::ExtrudeConnectedComponents;
        expect_code(run(&p, &mixed), Code::InvalidGeometry);

        let non_manifold = polygons(
            vec![0.0, 1.0, 0.0, 0.0, 0.0],
            vec![0.0, 0.0, 1.0, -1.0, 0.0],
            vec![0.0, 0.0, 0.0, 0.0, 1.0],
            &[vec![0, 1, 2], vec![1, 0, 3], vec![0, 1, 4]],
        );
        let mut p = Params::new(1.0);
        p.divide = Divide::ExtrudeConnectedComponents;
        expect_code(run(&p, &non_manifold), Code::InvalidGeometry);
    }

    #[test]
    fn cancelled_run_reports_cancelled() {
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut p = Params::new(1.0);
        p.cancel = Some(token);
        match run(&p, &grid(2, 2)) {
            Err(e) => assert_eq!(error::kind(&e), Code::Cancelled),
            Ok(_) => panic!("expected cancellation"),
        }
    }

    #[test]
    fn connected_extrude_scale_cardinality() {
        let (columns, rows) = (320usize, 240usize);
        let dense = grid(columns, rows);
        let build = || {
            let mut p = Params::new(0.75);
            p.grain = Some(257);
            p.divide = Divide::ExtrudeConnectedComponents;
            p.divisions = 4;
            p.front_group = Some("front".to_owned());
            p.side_group = Some("side".to_owned());
            p.front_boundary_group = Some("front_boundary".to_owned());
            p.back_boundary_group = Some("back_boundary".to_owned());
            run(&p, &dense).unwrap()
        };
        let one = with_domains(1, &build);
        let many = with_domains(4, &build);
        assert!(same_geometry(&one, &many), "connected Poly Extrude differs across domain counts");
        assert_eq!(geometry::point_count(&one) as usize, (columns + 1) * (rows + 1) * 5);
        assert_eq!(cardinality(&one, "front") as usize, columns * rows * 2);
        assert_eq!(cardinality(&one, "side") as usize, (columns + rows) * 2 * 4);
        assert_eq!(
            edge_group::cardinality(&geometry::find_edge_group("front_boundary", &one).unwrap()) as usize,
            (columns + rows) * 2
        );
    }

    #[test]
    fn default_path_extrudes_each_polygon() {
        let quad = polygons(
            vec![0.0, 1.0, 1.0, 0.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0; 4],
            &[vec![0, 1, 2, 3]],
        );
        let output = run(&Params::new(2.0), &quad).unwrap();
        assert_eq!(geometry::point_count(&output), 8);
        assert_eq!(geometry::primitive_count(&output), 6);
        let (_, _, z) = xyz(&output);
        assert_eq!(z.iter().filter(|&&v| v == 2.0).count(), 4);
        let mut p = Params::new(1.0);
        p.grain = Some(0);
        expect_code(run(&p, &quad), Code::InvalidArgument);
        expect_code(run(&Params::new(f64::NAN), &quad), Code::InvalidGeometry);
    }

    #[test]
    fn default_extrude_parallel_exact() {
        use crate::compact_points::fixtures::exact_at_one_and_four;
        let dense = grid(60, 40);
        let build = || {
            let mut p = Params::new(0.5);
            p.grain = Some(100);
            run(&p, &dense).unwrap()
        };
        let out = exact_at_one_and_four(&build);
        assert_eq!(geometry::primitive_count(&out), geometry::primitive_count(&dense) * 2 + geometry::vertex_count(&dense));
    }
}
