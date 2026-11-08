use std::sync::Arc;

use rdk_core::support::Windows;
use rdk_core::{
    attribute, cancel, edge_group, error, face_normals, geometry, group, packed, support, topology,
    topology_edge_lookup, topology_index, topology_remap,
};
use rdk_core::error::Code;

use crate::deform;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Shape {
    BevelChamfer,
    BevelRound { convexity: f64 },
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub edges: Option<edge_group::T>,
    pub shape: Shape,
    pub divisions: i64,
    pub point_scale_attribute: Option<String>,
    pub ignore_flat_angle: Option<f64>,
    pub clamp_overlap: bool,
    pub edge_group: Option<String>,
    pub corner_group: Option<String>,
    pub offset_group: Option<String>,
    pub recompute_point_normals: bool,
    pub distance: f64,
}

impl Params {
    /// The defaults of the OCaml test front end (`Poly_bevel_front.run`).
    pub fn new(distance: f64) -> Params {
        Params {
            cancel: None,
            grain: None,
            edges: None,
            shape: Shape::BevelChamfer,
            divisions: 1,
            point_scale_attribute: None,
            ignore_flat_angle: None,
            clamp_overlap: true,
            edge_group: None,
            corner_group: None,
            offset_group: None,
            recompute_point_normals: true,
            distance,
        }
    }
}

const PREFIX: &str = "Rdk_mesh.Poly_bevel.poly_bevel";

fn make(code: Code, message: &str) -> error::T {
    error::new("poly_bevel", code, message.to_owned())
}

fn fail(message: String) -> error::T {
    make(Code::InvalidTopology, &message)
}

fn blank(value: &str) -> bool {
    value.bytes().all(|b| matches!(b, b' ' | 0x0c | b'\n' | b'\r' | b'\t'))
}

fn validate_name(label: &str, name: &Option<String>) -> Result<(), error::T> {
    match name {
        Some(name) if blank(name) => Err(fail(format!("{PREFIX}: {label} group name must not be empty"))),
        _ => Ok(()),
    }
}

/// Stdlib `min`/`max` on floats (NaN falls through to the second argument).
fn omin(a: f64, b: f64) -> f64 {
    if a <= b { a } else { b }
}

fn omax(a: f64, b: f64) -> f64 {
    if a >= b { a } else { b }
}

fn length3(x: f64, y: f64, z: f64) -> f64 {
    let scale = omax(x.abs(), omax(y.abs(), z.abs()));
    if scale == 0.0 {
        0.0
    } else {
        let (x, y, z) = (x / scale, y / scale, z / scale);
        scale * (x * x + y * y + z * z).sqrt()
    }
}

fn unit_between(positions: &packed::Float3::View::View, source: usize, destination: usize) -> Option<(f64, f64, f64, f64)> {
    let dx = positions.x[destination] - positions.x[source];
    let dy = positions.y[destination] - positions.y[source];
    let dz = positions.z[destination] - positions.z[source];
    let length = length3(dx, dy, dz);
    if length == 0.0 || !length.is_finite() {
        None
    } else {
        Some((dx / length, dy / length, dz / length, length))
    }
}

fn merge_group(value: group::T, groups: Vec<group::T>) -> Result<Vec<group::T>, error::T> {
    let mut groups = groups;
    for existing in groups.iter_mut() {
        if group::owner(existing) == group::owner(&value) && group::name(existing) == group::name(&value) {
            *existing = group::union(existing, &value)?;
            return Ok(groups);
        }
    }
    groups.push(value);
    Ok(groups)
}

fn merge_edge_group(value: edge_group::T, groups: Vec<edge_group::T>) -> Result<Vec<edge_group::T>, error::T> {
    let mut groups = groups;
    for existing in groups.iter_mut() {
        if edge_group::name(existing) == edge_group::name(&value) {
            *existing = edge_group::union(existing, &value)?;
            return Ok(groups);
        }
    }
    groups.push(value);
    Ok(groups)
}

fn nearest_mapping(left: &[i64], right: &[i64], weights: &[f64]) -> Vec<i64> {
    (0..weights.len()).map(|i| if weights[i] < 0.5 { left[i] } else { right[i] }).collect()
}

fn interpolate_float(left: &[i64], right: &[i64], weights: &[f64], source: &[f64]) -> Vec<f64> {
    (0..weights.len())
        .map(|index| {
            let t = weights[index];
            let l = source[left[index] as usize];
            l + (source[right[index] as usize] - l) * t
        })
        .collect()
}

fn interpolate_attribute(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    vertex_left: &[i64],
    vertex_right: &[i64],
    vertex_weight: &[f64],
    primitive_map: &[i64],
    source: &attribute::T,
) -> Result<Option<attribute::T>, error::T> {
    let owner = attribute::owner(source);
    if attribute::name(source) == "N" && (owner == attribute::Owner::Point || owner == attribute::Owner::Vertex) {
        return Ok(None);
    }
    match owner {
        attribute::Owner::Detail => Ok(Some(source.clone())),
        attribute::Owner::Point | attribute::Owner::Primitive => {
            let mapping = if owner == attribute::Owner::Point { point_map } else { primitive_map };
            Ok(Some(topology_remap::attribute(cancel_ref, grain, None, mapping, source)?))
        }
        attribute::Owner::Vertex => {
            let nearest = nearest_mapping(vertex_left, vertex_right, vertex_weight);
            let source_storage = attribute::View::storage(source)?;
            let storage = match attribute::map_float_planes(source_storage, |values| {
                Ok(interpolate_float(vertex_left, vertex_right, vertex_weight, values))
            })? {
                Some(interpolated) => interpolated,
                None => topology_remap::select_storage(cancel_ref, grain, false, &nearest, source_storage)?,
            };
            Ok(Some(attribute::create_owned(attribute::name(source), owner, storage)?))
        }
    }
}

fn add_empty_outputs(
    grain: i64,
    params: &Params,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let primitive_count = geometry::primitive_count(geometry_value);
    let mut groups = geometry::groups(geometry_value);
    for name in [&params.edge_group, &params.corner_group] {
        if let Some(name) = name {
            groups = merge_group(
                group::init(Some(grain), group::Owner::Primitive, name, primitive_count, &|_| false)?,
                groups,
            )?;
        }
    }
    let edge_groups = match &params.offset_group {
        None => geometry::edge_groups(geometry_value),
        Some(name) => {
            let topology_value = geometry::topology(geometry_value);
            let index = topology_index::create(None, topology_value)?;
            merge_edge_group(
                edge_group::init(Some(grain), topology_value, &index, name, &|_| false)?,
                geometry::edge_groups(geometry_value),
            )?
        }
    };
    geometry::create(
        geometry::positions(geometry_value).clone(),
        geometry::topology(geometry_value).clone(),
        Some(geometry::attributes(geometry_value)),
        Some(groups),
        Some(edge_groups),
    )
}

fn corner_root(parent: &mut [usize], vertex: usize) -> usize {
    let mut root = vertex;
    while parent[root] != root {
        root = parent[root];
    }
    let mut current = vertex;
    while parent[current] != root {
        let next = parent[current];
        parent[current] = root;
        current = next;
    }
    root
}

fn union_corners(parent: &mut [usize], left: usize, right: usize) {
    let left = corner_root(parent, left);
    let right = corner_root(parent, right);
    if left != right {
        if left < right {
            parent[right] = left;
        } else {
            parent[left] = right;
        }
    }
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(make(Code::InvalidArgument, &format!("{PREFIX}: grain must be positive")));
    }
    run_inner(params, grain, geometry_value).map_err(|value| error::guard(value, "poly_bevel", Code::InvalidTopology))
}

#[allow(clippy::too_many_lines)]
fn run_inner(params: &Params, grain: i64, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_ref = params.cancel.as_deref();
    let distance = params.distance;
    let divisions = params.divisions;
    if !distance.is_finite() || distance < 0.0 {
        return Err(fail(format!("{PREFIX}: distance must be finite and non-negative")));
    }
    if divisions <= 0 {
        return Err(fail(format!("{PREFIX}: divisions must be positive")));
    }
    if let Shape::BevelRound { convexity } = params.shape {
        if !(convexity.is_finite() && (-1.0..=1.0).contains(&convexity)) {
            return Err(fail(format!("{PREFIX}: round convexity must be finite and within [-1, 1]")));
        }
    }
    if let Some(value) = params.ignore_flat_angle {
        if !(value.is_finite() && value >= 0.0 && value <= std::f64::consts::PI) {
            return Err(fail(format!("{PREFIX}: flatness angle must be finite and within [0, pi]")));
        }
    }
    validate_name("edge fillet", &params.edge_group)?;
    validate_name("corner fillet", &params.corner_group)?;
    validate_name("offset edge", &params.offset_group)?;
    let source_topology = geometry::topology(geometry_value);
    let topology_view = topology::View::view(source_topology)?;
    let index_value = topology_index::create(cancel_ref, source_topology)?;
    let index = topology_index::View::view(&index_value);
    let edge_count = index.edge_a.len();
    let point_count = topology_view.point_count as usize;
    let vertex_count = topology_view.vertex_points.len();
    let primitive_count = topology_view.primitive_kinds.len();
    match &params.edges {
        Some(item) if edge_group::topology_data_id(item) != topology::data_id(source_topology) => {
            return Err(fail(format!("{PREFIX}: edge selection belongs to a different topology")));
        }
        Some(item) if edge_group::length(item) != edge_count as i64 => {
            return Err(fail(format!("{PREFIX}: edge selection length does not match topology edge count")));
        }
        _ => {}
    }
    let point_scale: Option<Vec<f64>> = match &params.point_scale_attribute {
        None => None,
        Some(name) if blank(name) => {
            return Err(fail(format!("{PREFIX}: point scale attribute name must not be empty")));
        }
        Some(name) => match geometry::find_attribute(attribute::Owner::Point, name, geometry_value) {
            None => return Err(fail(format!("{PREFIX}: point float scale attribute \"{name}\" is missing"))),
            Some(found) => match attribute::View::storage(&found)? {
                attribute::Storage::Float(values) => Some(values.to_vec()),
                _ => {
                    return Err(fail(format!(
                        "{PREFIX}: point scale attribute \"{name}\" must use float storage"
                    )));
                }
            },
        },
    };
    if let Some(values) = &point_scale {
        for (point, &value) in values.iter().enumerate() {
            if !value.is_finite() || value < 0.0 || !(distance * value).is_finite() {
                return Err(fail(format!(
                    "{PREFIX}: point scale at point {point} must produce a finite non-negative distance"
                )));
            }
        }
    }
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let requested = |edge: usize| match &params.edges {
        None => true,
        Some(item) => edge_group::mem(edge as i64, item),
    };
    let mut candidate = vec![false; edge_count];
    let mut candidate_faces = vec![false; primitive_count];
    let mut candidate_count = 0usize;
    for edge in 0..edge_count {
        cancel::poll(cancel_ref, edge as i64, 16_383)?;
        if requested(edge) && index.edge_offsets[edge + 1] - index.edge_offsets[edge] == 2 {
            let first = index.edge_offsets[edge] as usize;
            let left = index.edge_vertices[first] as usize;
            let right = index.edge_vertices[first + 1] as usize;
            let left_next = index.next_vertex[left];
            let right_next = index.next_vertex[right];
            let left_primitive = index.primitive_of_vertex[left] as usize;
            let right_primitive = index.primitive_of_vertex[right] as usize;
            if left_next >= 0
                && right_next >= 0
                && topology_view.primitive_kinds[left_primitive] == 0
                && topology_view.primitive_kinds[right_primitive] == 0
                && topology_view.vertex_points[left] == topology_view.vertex_points[right_next as usize]
                && topology_view.vertex_points[left_next as usize] == topology_view.vertex_points[right]
                && index.edge_a[edge] != index.edge_b[edge]
            {
                candidate[edge] = true;
                candidate_count += 1;
                candidate_faces[left_primitive] = true;
                candidate_faces[right_primitive] = true;
            }
        }
    }
    let flat_normals: Option<(Vec<f64>, Vec<f64>, Vec<f64>)> = match params.ignore_flat_angle {
        Some(_) if candidate_count > 0 => {
            let selection = group::init(
                Some(grain),
                group::Owner::Primitive,
                "__rdk_poly_bevel_faces",
                primitive_count as i64,
                &|primitive| candidate_faces[primitive as usize],
            )?;
            Some(face_normals::compute(cancel_ref, Some(grain), Some(&selection), "Poly Bevel", geometry_value)?)
        }
        _ => None,
    };
    let mut selected = vec![false; edge_count];
    let mut selected_count = 0usize;
    for edge in 0..edge_count {
        if candidate[edge] {
            let effective = match (params.ignore_flat_angle, &flat_normals) {
                (None, _) => true,
                (Some(threshold), Some((nx, ny, nz))) => {
                    let first = index.edge_offsets[edge] as usize;
                    let left = index.primitive_of_vertex[index.edge_vertices[first] as usize] as usize;
                    let right = index.primitive_of_vertex[index.edge_vertices[first + 1] as usize] as usize;
                    let dot = nx[left] * nx[right] + ny[left] * ny[right] + nz[left] * nz[right];
                    let dot = omax(-1.0, omin(1.0, dot));
                    dot.acos() > threshold + 64.0 * f64::EPSILON
                }
                (Some(_), None) => return Err(fail(format!("{PREFIX}: missing face normals"))),
            };
            if effective {
                selected[edge] = true;
                selected_count += 1;
            }
        }
    }
    if distance == 0.0 || selected_count == 0 {
        if params.edge_group.is_none() && params.corner_group.is_none() && params.offset_group.is_none() {
            return Ok(geometry_value.clone());
        }
        return add_empty_outputs(grain, params, geometry_value);
    }
    let edge_selected = |edge: i64| edge >= 0 && selected[edge as usize];
    let scale_at = |point: usize| match &point_scale {
        None => distance,
        Some(values) => distance * values[point],
    };
    let mut touched = vec![false; vertex_count];
    let mut predecessor = vec![-1i64; vertex_count];
    let mut successor = vec![-1i64; vertex_count];
    let mut move_to_previous = vec![0.0f64; vertex_count];
    let mut move_to_next = vec![0.0f64; vertex_count];
    let mut touched_count = 0usize;
    let inconsistent = || fail(format!("{PREFIX}: selected edge network has inconsistent point fans"));
    for vertex in 0..vertex_count {
        cancel::poll(cancel_ref, vertex as i64, 16_383)?;
        let primitive = index.primitive_of_vertex[vertex] as usize;
        if topology_view.primitive_kinds[primitive] == 0 {
            let previous = index.previous_vertex[vertex];
            let incoming = if previous < 0 { -1 } else { index.edge_of_vertex[previous as usize] };
            let outgoing = index.edge_of_vertex[vertex];
            let incoming_selected = edge_selected(incoming);
            let outgoing_selected = edge_selected(outgoing);
            if incoming_selected || outgoing_selected {
                touched[vertex] = true;
                touched_count += 1;
                let point = topology_view.vertex_points[vertex] as usize;
                let amount = scale_at(point);
                if !amount.is_finite() {
                    return Err(inconsistent());
                }
                if outgoing_selected {
                    move_to_previous[vertex] = amount;
                    let opposite = index.opposite_vertex[vertex];
                    if opposite < 0 {
                        return Err(inconsistent());
                    }
                    successor[vertex] = index.next_vertex[opposite as usize];
                }
                if incoming_selected {
                    move_to_next[vertex] = amount;
                    let opposite = index.opposite_vertex[previous as usize];
                    if opposite < 0 {
                        return Err(inconsistent());
                    }
                    predecessor[vertex] = opposite;
                }
            }
        }
    }
    // Each face edge owns exactly these two slide distances, so collision limiting is
    // deterministic and does not require relaxation passes.
    if params.clamp_overlap {
        for vertex in 0..vertex_count {
            cancel::poll(cancel_ref, vertex as i64, 16_383)?;
            let primitive = index.primitive_of_vertex[vertex] as usize;
            if topology_view.primitive_kinds[primitive] == 0 {
                let next = index.next_vertex[vertex] as usize;
                let start_move = move_to_next[vertex];
                let end_move = move_to_previous[next];
                let total = start_move + end_move;
                if total > 0.0 {
                    let a = topology_view.vertex_points[vertex] as usize;
                    let b = topology_view.vertex_points[next] as usize;
                    match unit_between(&positions, a, b) {
                        None => {
                            move_to_next[vertex] = 0.0;
                            move_to_previous[next] = 0.0;
                        }
                        Some((_, _, _, length)) => {
                            let limit = length * (1.0 - 128.0 * f64::EPSILON);
                            if total > limit {
                                let factor = omax(0.0, limit) / total;
                                move_to_next[vertex] = start_move * factor;
                                move_to_previous[next] = end_move * factor;
                            }
                        }
                    }
                }
            }
        }
    }
    // Equal slide positions on the two sides of an unselected manifold ring edge share one
    // topological point. Continuation detection below then shares profile rails at degree-two
    // selected points, avoiding duplicate corner patches and four-incidence edges.
    let mut corner_parent: Vec<usize> = (0..vertex_count).collect();
    for edge in 0..edge_count {
        if !edge_selected(edge as i64) && index.edge_offsets[edge + 1] - index.edge_offsets[edge] == 2 {
            let first = index.edge_offsets[edge] as usize;
            let left = index.edge_vertices[first] as usize;
            let right = index.edge_vertices[first + 1] as usize;
            let left_next = index.next_vertex[left];
            let right_next = index.next_vertex[right];
            if left_next >= 0
                && right_next >= 0
                && topology_view.primitive_kinds[index.primitive_of_vertex[left] as usize] == 0
                && topology_view.primitive_kinds[index.primitive_of_vertex[right] as usize] == 0
            {
                let (left_next, right_next) = (left_next as usize, right_next as usize);
                if touched[left] && touched[right_next] && move_to_next[left] == move_to_previous[right_next] {
                    union_corners(&mut corner_parent, left, right_next);
                }
                if touched[left_next] && touched[right] && move_to_previous[left_next] == move_to_next[right] {
                    union_corners(&mut corner_parent, left_next, right);
                }
            }
        }
    }
    let mut corner_point = vec![-1i64; vertex_count];
    let mut visited = vec![false; vertex_count];
    let mut component_offsets = vec![0usize; touched_count + 1];
    let mut component_corners = vec![0usize; touched_count];
    let mut component_points = vec![0usize; touched_count];
    let mut component_closed = vec![false; touched_count];
    let mut component_count = 0usize;
    let mut component_cursor = 0usize;
    let mut add_component = |start: usize, closed: bool, visited: &mut Vec<bool>| {
        let component = component_count;
        component_offsets[component] = component_cursor;
        component_points[component] = topology_view.vertex_points[start] as usize;
        if closed {
            component_closed[component] = true;
        }
        let mut current = start;
        loop {
            if visited[current] {
                break;
            }
            visited[current] = true;
            component_corners[component_cursor] = current;
            component_cursor += 1;
            let next = successor[current];
            if next < 0 || next as usize == start {
                break;
            }
            current = next as usize;
        }
        component_count += 1;
    };
    for vertex in 0..vertex_count {
        if touched[vertex] && predecessor[vertex] < 0 && !visited[vertex] {
            add_component(vertex, false, &mut visited);
        }
    }
    for vertex in 0..vertex_count {
        if touched[vertex] && !visited[vertex] {
            add_component(vertex, true, &mut visited);
        }
    }
    component_offsets[component_count] = component_cursor;
    if component_cursor != touched_count {
        return Err(inconsistent());
    }
    let mut component_joined = vec![false; component_count];
    for component in 0..component_count {
        if !component_closed[component] {
            let first = component_offsets[component];
            let last = component_offsets[component + 1];
            if last - first >= 2
                && corner_root(&mut corner_parent, component_corners[first])
                    == corner_root(&mut corner_parent, component_corners[last - 1])
            {
                component_joined[component] = true;
            }
        }
    }
    let mut keep_original = vec![false; point_count];
    for point in 0..point_count {
        if index.point_offsets[point] == index.point_offsets[point + 1] {
            keep_original[point] = true;
        }
    }
    for vertex in 0..vertex_count {
        let primitive = index.primitive_of_vertex[vertex] as usize;
        if topology_view.primitive_kinds[primitive] != 0 || !touched[vertex] {
            keep_original[topology_view.vertex_points[vertex] as usize] = true;
        }
    }
    for component in 0..component_count {
        if !component_closed[component] && !component_joined[component] {
            keep_original[component_points[component]] = true;
        }
    }
    let mut kept_count = 0usize;
    let mut old_to_output = vec![-1i64; point_count];
    for point in 0..point_count {
        if keep_original[point] {
            old_to_output[point] = kept_count as i64;
            kept_count += 1;
        }
    }
    let mut edge_ordinal = vec![-1i64; edge_count];
    let mut ordinal = 0i64;
    for edge in 0..edge_count {
        if edge_selected(edge as i64) {
            edge_ordinal[edge] = ordinal;
            ordinal += 1;
        }
    }
    let divisions_u = divisions as usize;
    let interior_per_edge = 2 * (divisions_u - 1);
    let output_points = kept_count + touched_count + selected_count * interior_per_edge;
    let selected_first_point = kept_count + touched_count;
    let mut px = vec![0.0f64; output_points];
    let mut py = vec![0.0f64; output_points];
    let mut pz = vec![0.0f64; output_points];
    let mut point_map = vec![0i64; output_points];
    for point in 0..point_count {
        let output = old_to_output[point];
        if output >= 0 {
            let output = output as usize;
            px[output] = positions.x[point];
            py[output] = positions.y[point];
            pz[output] = positions.z[point];
            point_map[output] = point as i64;
        }
    }
    let mut next_corner = kept_count;
    for vertex in 0..vertex_count {
        if touched[vertex] {
            corner_point[vertex] = next_corner as i64;
            next_corner += 1;
        }
    }
    let corner_storage = corner_point.clone();
    let range_at = |range: i64, count: usize| range.saturating_mul(grain).min(count as i64) as usize;
    if touched_count > 0 {
        let mut touched_before = vec![0usize; vertex_count + 1];
        for vertex in 0..vertex_count {
            touched_before[vertex + 1] = touched_before[vertex] + usize::from(touched[vertex]);
        }
        let bound = |range: i64| touched_before[range_at(range, vertex_count)];
        let ranges = support::range_count(vertex_count as i64, grain);
        let corners = kept_count..kept_count + touched_count;
        let (wx, wy, wz, wm) = (
            Windows::new(&mut px[corners.clone()], ranges, bound),
            Windows::new(&mut py[corners.clone()], ranges, bound),
            Windows::new(&mut pz[corners.clone()], ranges, bound),
            Windows::new(&mut point_map[corners], ranges, bound),
        );
        support::run_indexed_ranges(grain, vertex_count as i64, &|range, first, last| {
            let (xs, ys, zs, ms) = (wx.take(range), wy.take(range), wz.take(range), wm.take(range));
            for vertex in first..last {
                support::poll(cancel_ref, vertex)?;
                let vertex = vertex as usize;
                let output = corner_storage[vertex];
                if output >= 0 {
                    let slot = output as usize - kept_count - bound(range);
                    let point = topology_view.vertex_points[vertex] as usize;
                    let previous = topology_view.vertex_points[index.previous_vertex[vertex] as usize] as usize;
                    let next = topology_view.vertex_points[index.next_vertex[vertex] as usize] as usize;
                    let (mut x, mut y, mut z) = (positions.x[point], positions.y[point], positions.z[point]);
                    if move_to_previous[vertex] != 0.0 {
                        if let Some((dx, dy, dz, _)) = unit_between(&positions, point, previous) {
                            x += move_to_previous[vertex] * dx;
                            y += move_to_previous[vertex] * dy;
                            z += move_to_previous[vertex] * dz;
                        }
                    }
                    if move_to_next[vertex] != 0.0 {
                        if let Some((dx, dy, dz, _)) = unit_between(&positions, point, next) {
                            x += move_to_next[vertex] * dx;
                            y += move_to_next[vertex] * dy;
                            z += move_to_next[vertex] * dz;
                        }
                    }
                    if !(x.is_finite() && y.is_finite() && z.is_finite()) {
                        return Err(fail(format!(
                            "{PREFIX}: corner position for vertex {vertex} is not finite"
                        )));
                    }
                    (xs[slot], ys[slot], zs[slot], ms[slot]) = (x, y, z, point as i64);
                }
            }
            Ok(())
        })?;
    }
    for vertex in 0..vertex_count {
        if corner_storage[vertex] >= 0 {
            let root = corner_root(&mut corner_parent, vertex);
            corner_point[vertex] = corner_storage[root];
        }
    }
    let edge_halves = |edge: usize| -> (usize, usize) {
        let first = index.edge_offsets[edge] as usize;
        (index.edge_vertices[first] as usize, index.edge_vertices[first + 1] as usize)
    };
    let endpoint_corners = |edge: usize, endpoint: usize| -> (usize, usize) {
        let (left, right) = edge_halves(edge);
        if endpoint == 0 {
            (left, index.next_vertex[right] as usize)
        } else {
            (index.next_vertex[left] as usize, right)
        }
    };
    let interior_point = |edge: usize, endpoint: usize, layer: usize| -> usize {
        selected_first_point + edge_ordinal[edge] as usize * interior_per_edge + endpoint * (divisions_u - 1) + layer - 1
    };
    let profile_alias_count = selected_count * interior_per_edge;
    let mut profile_alias: Vec<usize> = (0..profile_alias_count).map(|local| selected_first_point + local).collect();
    let profile_point = |profile_alias: &Vec<usize>, edge: usize, endpoint: usize, layer: usize| -> usize {
        let (side0, side1) = endpoint_corners(edge, endpoint);
        if layer == 0 {
            corner_point[side0] as usize
        } else if layer == divisions_u {
            corner_point[side1] as usize
        } else {
            profile_alias[interior_point(edge, endpoint, layer) - selected_first_point]
        }
    };
    if divisions_u > 1 && selected_count > 0 {
        let edge_grain = (grain / (2 * (divisions - 1)).max(1)).max(1);
        let edge_range_at = |range: i64| range.saturating_mul(edge_grain).min(edge_count as i64) as usize;
        let mut selected_before = vec![0usize; edge_count + 1];
        for edge in 0..edge_count {
            selected_before[edge + 1] = selected_before[edge] + usize::from(edge_ordinal[edge] >= 0);
        }
        let bound = |range: i64| selected_before[edge_range_at(range)] * interior_per_edge;
        let ranges = support::range_count(edge_count as i64, edge_grain);
        // Interior points sit past every corner point, so they are written while the corners are only read.
        let (hx, tx) = px.split_at_mut(selected_first_point);
        let (hy, ty) = py.split_at_mut(selected_first_point);
        let (hz, tz) = pz.split_at_mut(selected_first_point);
        let (wx, wy, wz, wm) = (
            Windows::new(tx, ranges, bound),
            Windows::new(ty, ranges, bound),
            Windows::new(tz, ranges, bound),
            Windows::new(&mut point_map[selected_first_point..], ranges, bound),
        );
        support::run_indexed_ranges(edge_grain, edge_count as i64, &|range, first, last| {
            let (xs, ys, zs, ms) = (wx.take(range), wy.take(range), wz.take(range), wm.take(range));
            for edge in first..last {
                support::poll(cancel_ref, edge)?;
                let edge = edge as usize;
                if edge_selected(edge as i64) {
                    for endpoint in 0..2 {
                        for layer in 1..divisions_u {
                            let slot = interior_point(edge, endpoint, layer) - selected_first_point - bound(range);
                            let (side0, side1) = endpoint_corners(edge, endpoint);
                            let q0 = corner_point[side0] as usize;
                            let q1 = corner_point[side1] as usize;
                            let source_point = topology_view.vertex_points[side0] as usize;
                            let t = layer as f64 / divisions as f64;
                            let one = 1.0 - t;
                            let lx = one * hx[q0] + t * hx[q1];
                            let ly = one * hy[q0] + t * hy[q1];
                            let lz = one * hz[q0] + t * hz[q1];
                            let (x, y, z) = match params.shape {
                                Shape::BevelChamfer => (lx, ly, lz),
                                Shape::BevelRound { convexity } => {
                                    let ax = hx[q0] - positions.x[source_point];
                                    let ay = hy[q0] - positions.y[source_point];
                                    let az = hz[q0] - positions.z[source_point];
                                    let bx = hx[q1] - positions.x[source_point];
                                    let by = hy[q1] - positions.y[source_point];
                                    let bz = hz[q1] - positions.z[source_point];
                                    let al = length3(ax, ay, az);
                                    let bl = length3(bx, by, bz);
                                    let dot = if al == 0.0 || bl == 0.0 {
                                        1.0
                                    } else {
                                        omax(-1.0, omin(1.0, (ax * bx + ay * by + az * bz) / al / bl))
                                    };
                                    let weight = omax(0.0, (1.0 + dot) * 0.5).sqrt();
                                    let denominator = one * one + 2.0 * one * t * weight + t * t;
                                    let arc = |coordinate0: f64, coordinate1: f64, center: f64| -> f64 {
                                        if denominator == 0.0 {
                                            one * coordinate0 + t * coordinate1
                                        } else {
                                            (one * one * coordinate0
                                                + 2.0 * one * t * weight * center
                                                + t * t * coordinate1)
                                                / denominator
                                        }
                                    };
                                    let rx = arc(hx[q0], hx[q1], positions.x[source_point]);
                                    let ry = arc(hy[q0], hy[q1], positions.y[source_point]);
                                    let rz = arc(hz[q0], hz[q1], positions.z[source_point]);
                                    (
                                        lx + convexity * (rx - lx),
                                        ly + convexity * (ry - ly),
                                        lz + convexity * (rz - lz),
                                    )
                                }
                            };
                            (xs[slot], ys[slot], zs[slot], ms[slot]) = (x, y, z, source_point as i64);
                        }
                    }
                }
            }
            Ok(())
        })?;
    }
    let mut skip_corner_point = vec![false; point_count];
    for point in 0..point_count {
        let first = index.point_edge_offsets[point] as usize;
        let last = index.point_edge_offsets[point + 1] as usize;
        let mut first_edge: i64 = -1;
        let mut second_edge: i64 = -1;
        let mut count = 0;
        for at in first..last {
            let edge = index.point_edges[at];
            if edge_selected(edge) {
                if count == 0 {
                    first_edge = edge;
                } else if count == 1 {
                    second_edge = edge;
                }
                count += 1;
            }
        }
        if count == 2 {
            let endpoint = |edge: usize| -> usize {
                let (left, _) = edge_halves(edge);
                if topology_view.vertex_points[left] as usize == point { 0 } else { 1 }
            };
            let (edge0, edge1) = if first_edge < second_edge { (first_edge, second_edge) } else { (second_edge, first_edge) };
            let (edge0, edge1) = (edge0 as usize, edge1 as usize);
            let endpoint0 = endpoint(edge0);
            let endpoint1 = endpoint(edge1);
            let (a0_corner, a1_corner) = endpoint_corners(edge0, endpoint0);
            let (b0_corner, b1_corner) = endpoint_corners(edge1, endpoint1);
            let a0 = corner_point[a0_corner];
            let a1 = corner_point[a1_corner];
            let b0 = corner_point[b0_corner];
            let b1 = corner_point[b1_corner];
            let same = a0 == b0 && a1 == b1;
            let reversed = a0 == b1 && a1 == b0;
            if same || reversed {
                skip_corner_point[point] = true;
                for layer in 1..divisions_u {
                    let source_layer = if same { layer } else { divisions_u - layer };
                    profile_alias[interior_point(edge1, endpoint1, layer) - selected_first_point] =
                        interior_point(edge0, endpoint0, source_layer);
                }
            }
        }
    }
    let base_point = |vertex: usize| -> i64 {
        let point = corner_point[vertex];
        if point >= 0 {
            point
        } else {
            old_to_output[topology_view.vertex_points[vertex] as usize]
        }
    };
    let parameter_on_edge = |a: usize, b: usize, output: usize| -> f64 {
        match unit_between(&positions, a, b) {
            None => 0.0,
            Some((dx, dy, dz, length)) => {
                let qx = px[output] - positions.x[a];
                let qy = py[output] - positions.y[a];
                let qz = pz[output] - positions.z[a];
                omax(0.0, omin(1.0, (qx * dx + qy * dy + qz * dz) / length))
            }
        }
    };
    let mut insert_count = vec![0u8; vertex_count];
    let mut insert_point0 = vec![-1i64; vertex_count];
    let mut insert_point1 = vec![-1i64; vertex_count];
    let mut insert_weight0 = vec![0.0f64; vertex_count];
    let mut insert_weight1 = vec![0.0f64; vertex_count];
    for vertex in 0..vertex_count {
        cancel::poll(cancel_ref, vertex as i64, 16_383)?;
        let primitive = index.primitive_of_vertex[vertex] as usize;
        if topology_view.primitive_kinds[primitive] != 0 {
            continue;
        }
        let edge = index.edge_of_vertex[vertex];
        if edge_selected(edge) {
            continue;
        }
        let opposite = index.opposite_vertex[vertex];
        if opposite < 0 {
            continue;
        }
        let opposite = opposite as usize;
        let next = index.next_vertex[vertex] as usize;
        let a = topology_view.vertex_points[vertex] as usize;
        let b = topology_view.vertex_points[next] as usize;
        let start_point = base_point(vertex);
        let end_point = base_point(next);
        let start_t = parameter_on_edge(a, b, start_point as usize);
        let end_t = parameter_on_edge(a, b, end_point as usize);
        let mut count = 0;
        let mut add = |candidate: i64| {
            if candidate >= 0 && candidate != start_point && candidate != end_point {
                let t = parameter_on_edge(a, b, candidate as usize);
                if t >= start_t && t <= end_t {
                    if count == 0 {
                        insert_point0[vertex] = candidate;
                        insert_weight0[vertex] = t;
                        count = 1;
                    } else if candidate != insert_point0[vertex] {
                        if t < insert_weight0[vertex] {
                            insert_point1[vertex] = insert_point0[vertex];
                            insert_weight1[vertex] = insert_weight0[vertex];
                            insert_point0[vertex] = candidate;
                            insert_weight0[vertex] = t;
                        } else {
                            insert_point1[vertex] = candidate;
                            insert_weight1[vertex] = t;
                        }
                        count = 2;
                    }
                }
            }
        };
        add(corner_point[index.next_vertex[opposite] as usize]);
        add(corner_point[opposite]);
        insert_count[vertex] = count as u8;
    }
    let mut base_offsets = vec![0usize; primitive_count + 1];
    for primitive in 0..primitive_count {
        let first = topology_view.primitive_offsets[primitive] as usize;
        let last = topology_view.primitive_offsets[primitive + 1] as usize;
        let mut count = last - first;
        for vertex in first..last {
            count += insert_count[vertex] as usize;
        }
        base_offsets[primitive + 1] = base_offsets[primitive] + count;
    }
    let base_vertices = base_offsets[primitive_count];
    let mut patch_vertices = 0usize;
    let mut patch_count = 0usize;
    let mut component_output = vec![-1i64; component_count];
    for component in 0..component_count {
        let corners = component_offsets[component + 1] - component_offsets[component];
        let joined = component_joined[component];
        let emitted_corners = if joined { corners - 1 } else { corners };
        let connections = if !component_closed[component] { corners - 1 } else { corners };
        let emitted_vertices = emitted_corners
            + connections * (divisions_u - 1)
            + usize::from(!component_closed[component] && !joined);
        if emitted_vertices >= 3 && !(joined && corners <= 2) && !skip_corner_point[component_points[component]] {
            component_output[component] = patch_count as i64;
            patch_count += 1;
            patch_vertices += emitted_vertices;
        }
    }
    let strip_primitives = selected_count * divisions_u;
    let strip_vertices = selected_count * divisions_u * 4;
    let output_primitives = primitive_count + strip_primitives + patch_count;
    let output_vertices = base_vertices + strip_vertices + patch_vertices;
    let mut vertex_points = vec![0i64; output_vertices];
    let mut vertex_left = vec![0i64; output_vertices];
    let mut vertex_right = vec![0i64; output_vertices];
    let mut vertex_weight = vec![0.0f64; output_vertices];
    let mut primitive_offsets = vec![0i64; output_primitives + 1];
    let mut primitive_kinds = vec![0u8; output_primitives];
    let mut primitive_map = vec![0i64; output_primitives];
    let mut edge_membership = vec![false; output_primitives];
    let mut corner_membership = vec![false; output_primitives];
    for primitive in 0..primitive_count {
        primitive_offsets[primitive] = base_offsets[primitive] as i64;
        primitive_map[primitive] = primitive as i64;
        primitive_kinds[primitive] = topology_view.primitive_kinds[primitive];
    }
    {
        let bound = |range: i64| base_offsets[range_at(range, primitive_count)];
        let ranges = support::range_count(primitive_count as i64, grain);
        let (wp, wl, wr, ww) = (
            Windows::new(&mut vertex_points, ranges, bound),
            Windows::new(&mut vertex_left, ranges, bound),
            Windows::new(&mut vertex_right, ranges, bound),
            Windows::new(&mut vertex_weight, ranges, bound),
        );
        support::run_indexed_ranges(grain, primitive_count as i64, &|range, first, last| {
            let (points, lefts, rights, weights) = (wp.take(range), wl.take(range), wr.take(range), ww.take(range));
            for primitive in first..last {
                support::poll(cancel_ref, primitive)?;
                let primitive = primitive as usize;
                let first = topology_view.primitive_offsets[primitive] as usize;
                let last = topology_view.primitive_offsets[primitive + 1] as usize;
                let mut output = base_offsets[primitive] - bound(range);
                for vertex in first..last {
                    points[output] = base_point(vertex);
                    lefts[output] = vertex as i64;
                    rights[output] = vertex as i64;
                    output += 1;
                    let next = index.next_vertex[vertex];
                    let count = insert_count[vertex];
                    if count > 0 {
                        points[output] = insert_point0[vertex];
                        lefts[output] = vertex as i64;
                        rights[output] = next;
                        weights[output] = insert_weight0[vertex];
                        output += 1;
                    }
                    if count > 1 {
                        points[output] = insert_point1[vertex];
                        lefts[output] = vertex as i64;
                        rights[output] = next;
                        weights[output] = insert_weight1[vertex];
                        output += 1;
                    }
                }
            }
            Ok(())
        })?;
    }
    let strip_first_primitive = primitive_count;
    let strip_first_vertex = base_vertices;
    for edge in 0..edge_count {
        let ordinal = edge_ordinal[edge];
        if ordinal < 0 {
            continue;
        }
        let ordinal = ordinal as usize;
        let (left, right) = edge_halves(edge);
        let left_next = index.next_vertex[left];
        let right_next = index.next_vertex[right];
        let left_primitive = index.primitive_of_vertex[left];
        let right_primitive = index.primitive_of_vertex[right];
        for layer in 0..divisions_u {
            let primitive = strip_first_primitive + ordinal * divisions_u + layer;
            let output = strip_first_vertex + (ordinal * divisions_u + layer) * 4;
            primitive_offsets[primitive] = output as i64;
            primitive_map[primitive] = left_primitive.min(right_primitive);
            edge_membership[primitive] = true;
            // Opposite to the left face along their shared edge, so the strip winds outward like
            // its neighbours.
            let rows: [(usize, usize, usize, i64, i64); 4] = [
                (0, 1, layer, left_next, right as i64),
                (1, 0, layer, left as i64, right_next),
                (2, 0, layer + 1, left as i64, right_next),
                (3, 1, layer + 1, left_next, right as i64),
            ];
            for (local, endpoint, row, source0, source1) in rows {
                let vertex = output + local;
                vertex_points[vertex] = profile_point(&profile_alias, edge, endpoint, row) as i64;
                vertex_left[vertex] = source0;
                vertex_right[vertex] = source1;
                vertex_weight[vertex] = row as f64 / divisions as f64;
            }
        }
    }
    let patch_first_primitive = strip_first_primitive + strip_primitives;
    let patch_first_vertex = strip_first_vertex + strip_vertices;
    let mut patch_cursor = patch_first_vertex;
    for component in 0..component_count {
        let output_component = component_output[component];
        if output_component < 0 {
            continue;
        }
        let primitive = patch_first_primitive + output_component as usize;
        let first = component_offsets[component];
        let last = component_offsets[component + 1];
        let joined = component_joined[component];
        let emitted_last = if joined { last - 1 } else { last };
        primitive_offsets[primitive] = patch_cursor as i64;
        corner_membership[primitive] = true;
        let mut source_primitive = i64::MAX;
        for at in first..emitted_last {
            let corner = component_corners[at];
            vertex_points[patch_cursor] = corner_point[corner];
            vertex_left[patch_cursor] = corner as i64;
            vertex_right[patch_cursor] = corner as i64;
            source_primitive = source_primitive.min(index.primitive_of_vertex[corner]);
            patch_cursor += 1;
            let connection_follows = at + 1 < last || component_closed[component] || joined;
            if connection_follows {
                let edge = index.edge_of_vertex[corner] as usize;
                let (left, right) = edge_halves(edge);
                let (endpoint, ascending, source0, source1) = if corner == left {
                    (0, true, left as i64, index.next_vertex[right])
                } else if corner == right {
                    (1, false, index.next_vertex[left], right as i64)
                } else {
                    return Err(inconsistent());
                };
                for local in 1..divisions_u {
                    let layer = if ascending { local } else { divisions_u - local };
                    vertex_points[patch_cursor] = profile_point(&profile_alias, edge, endpoint, layer) as i64;
                    vertex_left[patch_cursor] = source0;
                    vertex_right[patch_cursor] = source1;
                    vertex_weight[patch_cursor] = layer as f64 / divisions as f64;
                    patch_cursor += 1;
                }
            }
        }
        if !component_closed[component] && !joined {
            vertex_points[patch_cursor] = old_to_output[component_points[component]];
            let source_corner = component_corners[first] as i64;
            vertex_left[patch_cursor] = source_corner;
            vertex_right[patch_cursor] = source_corner;
            patch_cursor += 1;
        }
        // The fan order runs against the faces around the point; the patch winds outward reversed.
        let start = primitive_offsets[primitive] as usize;
        vertex_points[start..patch_cursor].reverse();
        vertex_left[start..patch_cursor].reverse();
        vertex_right[start..patch_cursor].reverse();
        vertex_weight[start..patch_cursor].reverse();
        primitive_map[primitive] = source_primitive;
    }
    primitive_offsets[output_primitives] = output_vertices as i64;
    if patch_cursor != output_vertices {
        return Err(inconsistent());
    }
    let mut used_points = vec![false; output_points];
    for &point in &vertex_points {
        used_points[point as usize] = true;
    }
    let mut retained_points = 0usize;
    let mut point_compact_map = vec![-1i64; output_points];
    for point in 0..output_points {
        if used_points[point] {
            point_compact_map[point] = retained_points as i64;
            retained_points += 1;
        }
    }
    if retained_points != output_points {
        let mut compact_x = vec![0.0f64; retained_points];
        let mut compact_y = vec![0.0f64; retained_points];
        let mut compact_z = vec![0.0f64; retained_points];
        let mut compact_source = vec![0i64; retained_points];
        for point in 0..output_points {
            let target = point_compact_map[point];
            if target >= 0 {
                let target = target as usize;
                compact_x[target] = px[point];
                compact_y[target] = py[point];
                compact_z[target] = pz[point];
                compact_source[target] = point_map[point];
            }
        }
        for point in vertex_points.iter_mut() {
            *point = point_compact_map[*point as usize];
        }
        px = compact_x;
        py = compact_y;
        pz = compact_z;
        point_map = compact_source;
    }
    let output_positions = packed::Float3::View::of_owned_exn(px, py, pz)?;
    let output_topology = topology::View::create_validated_owned(
        retained_points as i64,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let mut attributes = Vec::new();
    for source in geometry::attributes(geometry_value) {
        if let Some(mapped) = interpolate_attribute(cancel_ref, grain, &point_map, &vertex_left, &vertex_right, &vertex_weight, &primitive_map, &source)? {
            attributes.push(mapped);
        }
    }
    let mut groups = Vec::new();
    for source in geometry::groups(geometry_value) {
        let nearest;
        let mapping: &[i64] = match group::owner(&source) {
            group::Owner::Point => &point_map,
            group::Owner::Vertex => {
                nearest = nearest_mapping(&vertex_left, &vertex_right, &vertex_weight);
                &nearest
            }
            group::Owner::Primitive => &primitive_map,
        };
        groups.push(topology_remap::group(cancel_ref, grain, mapping, &source)?);
    }
    if let Some(name) = &params.edge_group {
        groups = merge_group(
            group::init(Some(grain), group::Owner::Primitive, name, output_primitives as i64, &|p| {
                edge_membership[p as usize]
            })?,
            groups,
        )?;
    }
    if let Some(name) = &params.corner_group {
        groups = merge_group(
            group::init(Some(grain), group::Owner::Primitive, name, output_primitives as i64, &|p| {
                corner_membership[p as usize]
            })?,
            groups,
        )?;
    }
    let target_edges = topology_edge_lookup::create(cancel_ref, &topology::View::view(&output_topology)?)?;
    let target_edge_count = topology_edge_lookup::count(&target_edges) as usize;
    let mut edge_groups: Vec<edge_group::T> = Vec::new();
    for source_group in geometry::edge_groups(geometry_value) {
        let mut bits = vec![0u8; target_edge_count.div_ceil(8)];
        for edge in 0..target_edge_count {
            cancel::poll(cancel_ref, edge as i64, 16_383)?;
            let (target_a, target_b) = topology_edge_lookup::endpoints(&target_edges, edge as i64)?;
            let a = point_map[target_a as usize];
            let b = point_map[target_b as usize];
            let member = if a == b {
                false
            } else {
                let source_edge = topology_index::find_edge_index(&index_value, a, b);
                source_edge >= 0 && edge_group::mem(source_edge, &source_group)
            };
            if member {
                bits[edge >> 3] |= 1 << (edge & 7);
            }
        }
        edge_groups.push(edge_group::view::of_owned_bits(
            &output_topology,
            target_edge_count as i64,
            edge_group::name(&source_group),
            bits,
        )?);
    }
    if let Some(name) = &params.offset_group {
        let mut bits = vec![0u8; target_edge_count.div_ceil(8)];
        for edge in 0..edge_count {
            if edge_selected(edge as i64) {
                for side in 0..2 {
                    let a = point_compact_map[profile_point(&profile_alias, edge, 0, side * divisions_u)];
                    let b = point_compact_map[profile_point(&profile_alias, edge, 1, side * divisions_u)];
                    let target = topology_edge_lookup::find(&target_edges, a, b);
                    if target >= 0 {
                        bits[(target >> 3) as usize] |= 1 << (target & 7);
                    }
                }
            }
        }
        edge_groups = merge_edge_group(
            edge_group::view::of_owned_bits(&output_topology, target_edge_count as i64, name, bits)?,
            edge_groups,
        )?;
    }
    let output = geometry::create(
        output_positions,
        output_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )?;
    if params.recompute_point_normals
        && geometry::find_attribute(attribute::Owner::Point, "N", geometry_value).is_some()
    {
        let with_normals = deform::normals(
            deform::Params { cancel: params.cancel.clone(), grain, primitives: None },
            &output,
        )?;
        return match geometry::find_attribute(attribute::Owner::Point, "N", &with_normals) {
            None => Ok(output),
            Some(normal) => geometry::with_attribute(normal, &output),
        };
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plane_clip::test_util::*;

    fn bx() -> geometry::T {
        cuboid((0.0, 0.0, 0.0), (2.0, 2.0, 2.0))
    }

    fn all_edges(source: &geometry::T, name: &str) -> edge_group::T {
        let topology_value = geometry::topology(source);
        let index = topology_index::create(None, topology_value).unwrap();
        edge_group::init(Some(1), topology_value, &index, name, &|_| true).unwrap()
    }

    fn edge_pair(source: &geometry::T, name: &str, a: i64, b: i64) -> edge_group::T {
        let topology_value = geometry::topology(source);
        let index = topology_index::create(None, topology_value).unwrap();
        let selected = topology_index::find_edge_index(&index, a, b);
        assert!(selected >= 0, "PolyBevel test edge is absent");
        edge_group::init(Some(1), topology_value, &index, name, &|edge| edge == selected).unwrap()
    }

    fn with_payload(source: &geometry::T) -> geometry::T {
        let points = geometry::point_count(source);
        let vertices = geometry::vertex_count(source);
        let primitives = geometry::primitive_count(source);
        let attribute_of = |owner: attribute::Owner, name: &str, storage: attribute::Storage| {
            attribute::create_owned(name, owner, storage).unwrap()
        };
        let reversed: Vec<i64> = (0..points).map(|i| points - i - 1).collect();
        let point_order = group::ordered(group::Owner::Point, "point_order", points, &reversed).unwrap();
        let corner_group = group::init(Some(1), group::Owner::Vertex, "corners", vertices, &|v| v & 1 == 0).unwrap();
        let identity: Vec<i64> = (0..primitives).collect();
        let face_order = group::ordered(group::Owner::Primitive, "face_order", primitives, &identity).unwrap();
        geometry::create(
            geometry::positions(source).clone(),
            geometry::topology(source).clone(),
            Some(vec![
                attribute_of(attribute::Owner::Point, "id", attribute::Storage::Int(Arc::new((0..points).collect()))),
                attribute_of(
                    attribute::Owner::Point,
                    "pscale",
                    attribute::Storage::Float(Arc::new((0..points).map(|p| if p & 1 == 0 { 1.0 } else { 0.5 }).collect())),
                ),
                attribute_of(
                    attribute::Owner::Vertex,
                    "corner_value",
                    attribute::Storage::Float(Arc::new((0..vertices).map(|v| v as f64).collect())),
                ),
                attribute_of(
                    attribute::Owner::Vertex,
                    "corner_name",
                    attribute::Storage::Text(Arc::new((0..vertices).map(|v| v.to_string().into_bytes()).collect())),
                ),
                attribute_of(attribute::Owner::Primitive, "material", attribute::Storage::Int(Arc::new((0..primitives).collect()))),
                attribute_of(attribute::Owner::Detail, "tag", attribute::Storage::Text(std::sync::Arc::new(vec![b"bevel".to_vec()].into()))),
            ]),
            Some(vec![point_order, corner_group, face_order]),
            Some(vec![all_edges(source, "source_edges")]),
        )
        .unwrap()
    }

    fn validate_closed_manifold(source: &geometry::T) {
        let index = topology_index::create(None, geometry::topology(source)).unwrap();
        for edge in 0..topology_index::edge_count(&index) {
            let incidence = topology_index::edge_incidence_count(&index, edge).unwrap();
            assert_eq!(incidence, 2, "PolyBevel output edge {edge} has incidence {incidence} instead of 2");
        }
        let view = topo(source);
        let mut used = vec![false; geometry::point_count(source) as usize];
        for &point in view.vertex_points.iter() {
            used[point as usize] = true;
        }
        assert!(used.iter().all(|&u| u), "PolyBevel left an unused point");
    }

    fn cardinality(source: &geometry::T, name: &str) -> i64 {
        group::cardinality(&geometry::find_group(group::Owner::Primitive, name, source).unwrap())
    }

    fn edge_cardinality(source: &geometry::T, name: &str) -> i64 {
        edge_group::cardinality(&geometry::find_edge_group(name, source).unwrap())
    }

    fn all_finite(source: &geometry::T) {
        let (x, y, z) = xyz(source);
        for i in 0..x.len() {
            assert!(x[i].is_finite() && y[i].is_finite() && z[i].is_finite(), "non-finite position");
        }
    }

    #[test]
    fn all_edges_and_profiles() {
        let source = with_payload(&bx());
        let edges = geometry::find_edge_group("source_edges", &source).unwrap();
        let mut p = Params::new(0.2);
        p.grain = Some(3);
        p.edges = Some(edges.clone());
        p.edge_group = Some("edge_fillets".to_owned());
        p.corner_group = Some("corner_fillets".to_owned());
        p.offset_group = Some("offset_edges".to_owned());
        let chamfer = run(&p, &source).unwrap();
        assert!(
            geometry::point_count(&chamfer) == 24
                && geometry::primitive_count(&chamfer) == 26
                && geometry::vertex_count(&chamfer) == 96,
            "PolyBevel all-edge chamfer cardinality"
        );
        validate_closed_manifold(&chamfer);
        assert!(
            cardinality(&chamfer, "edge_fillets") == 12
                && cardinality(&chamfer, "corner_fillets") == 8
                && edge_cardinality(&chamfer, "offset_edges") == 24
                && edge_cardinality(&chamfer, "source_edges") == 24,
            "PolyBevel generated/source group ancestry"
        );
        let detail = geometry::find_attribute(attribute::Owner::Detail, "tag", &chamfer).unwrap();
        match attribute::View::storage(&detail).unwrap() {
            attribute::Storage::Text(values) => assert_eq!(**values, vec![b"bevel".to_vec()]),
            _ => panic!("detail storage changed"),
        }
        let mut p = Params::new(0.3);
        p.grain = Some(2);
        p.edges = Some(edges);
        p.shape = Shape::BevelRound { convexity: 1.0 };
        p.divisions = 3;
        p.point_scale_attribute = Some("pscale".to_owned());
        p.edge_group = Some("edge_fillets".to_owned());
        p.corner_group = Some("corner_fillets".to_owned());
        p.offset_group = Some("offset_edges".to_owned());
        let round = run(&p, &source).unwrap();
        assert!(
            geometry::point_count(&round) == 72
                && geometry::primitive_count(&round) == 50
                && geometry::vertex_count(&round) == 240,
            "PolyBevel divided round cardinality"
        );
        validate_closed_manifold(&round);
        assert!(
            cardinality(&round, "edge_fillets") == 36
                && edge_cardinality(&round, "offset_edges") == 24
                && edge_cardinality(&round, "source_edges") == 48,
            "PolyBevel divided group ancestry"
        );
        all_finite(&round);
    }

    #[test]
    fn partial_network_and_clamping() {
        let source = bx();
        let selected = edge_pair(&source, "one", 0, 1);
        let mut p = Params::new(0.25);
        p.grain = Some(1);
        p.edges = Some(selected.clone());
        let one = run(&p, &source).unwrap();
        assert!(
            geometry::point_count(&one) == 12 && geometry::primitive_count(&one) == 9 && geometry::vertex_count(&one) == 38,
            "PolyBevel single-edge endpoint patches"
        );
        validate_closed_manifold(&one);
        let mut p = Params::new(1e200);
        p.grain = Some(1);
        p.edges = Some(selected);
        let huge = run(&p, &source).unwrap();
        all_finite(&huge);
        validate_closed_manifold(&huge);
    }

    #[test]
    fn connected_network_flat_filter_and_normals() {
        let source = bx();
        let index = topology_index::create(None, geometry::topology(&source)).unwrap();
        assert!(topology_index::point_edge_count(&index, 0).unwrap() >= 2);
        let ids = [
            topology_index::point_edge(&index, 0, 0).unwrap(),
            topology_index::point_edge(&index, 0, 1).unwrap(),
        ];
        let selected = edge_group::init(Some(1), geometry::topology(&source), &index, "corner_pair", &|e| ids.contains(&e))
            .unwrap();
        let mut p = Params::new(0.18);
        p.grain = Some(2);
        p.edges = Some(selected);
        p.divisions = 2;
        p.shape = Shape::BevelRound { convexity: 0.5 };
        p.edge_group = Some("edges".to_owned());
        p.corner_group = Some("corners".to_owned());
        let connected = run(&p, &source).unwrap();
        validate_closed_manifold(&connected);
        assert!(
            cardinality(&connected, "edges") == 4 && cardinality(&connected, "corners") == 2,
            "PolyBevel connected edge-run corner planning"
        );
        // The OCaml fixture is a 3 x 2 x 2 divided box with point normals; a plain box with a
        // point normal attribute covers the same recompute and flat-angle filter.
        let normals = attribute::create_owned(
            "N",
            attribute::Owner::Point,
            attribute::Storage::Float3(packed::Float3::View::of_owned_exn(vec![0.0; 8], vec![0.0; 8], vec![1.0; 8]).unwrap()),
        )
        .unwrap();
        let divided = geometry::with_attribute(normals, &bx()).unwrap();
        let mut p = Params::new(0.12);
        p.grain = Some(7);
        p.edges = Some(all_edges(&divided, "all"));
        p.ignore_flat_angle = Some(0.0);
        let output = run(&p, &divided).unwrap();
        validate_closed_manifold(&output);
        match geometry::find_attribute(attribute::Owner::Point, "N", &output) {
            Some(normal) if attribute::length(&normal) == geometry::point_count(&output) => {}
            _ => panic!("PolyBevel did not recompute an existing point normal"),
        }
    }

    fn grid(triangles: bool) -> geometry::T {
        // 2 x 2 cells over 3 x 3 points.
        let (mut x, mut y) = (Vec::new(), Vec::new());
        for r in 0..3 {
            for c in 0..3 {
                x.push(c as f64 - 1.0);
                y.push(r as f64 - 1.0);
            }
        }
        let mut polys = Vec::new();
        for r in 0..2i64 {
            for c in 0..2i64 {
                let p = r * 3 + c;
                if triangles {
                    polys.push(vec![p, p + 1, p + 4]);
                    polys.push(vec![p, p + 4, p + 3]);
                } else {
                    polys.push(vec![p, p + 1, p + 4, p + 3]);
                }
            }
        }
        polygons(x, y, vec![0.0; 9], &polys)
    }

    #[test]
    fn exclusions_identity_and_validation() {
        let source = bx();
        let all = all_edges(&source, "all");
        let mut p = Params::new(0.0);
        p.edges = Some(all);
        assert_eq!(geometry::data_id(&run(&p, &source).unwrap()), geometry::data_id(&source));
        let open_grid = grid(false);
        let boundary = edge_pair(&open_grid, "boundary", 0, 1);
        let mut p = Params::new(0.2);
        p.edges = Some(boundary);
        assert_eq!(geometry::data_id(&run(&p, &open_grid).unwrap()), geometry::data_id(&open_grid));
        let flat = grid(true);
        let flat_index = topology_index::create(None, geometry::topology(&flat)).unwrap();
        let mut interior = -1;
        for edge in 0..topology_index::edge_count(&flat_index) {
            if topology_index::edge_incidence_count(&flat_index, edge).unwrap() == 2 {
                interior = edge;
            }
        }
        let flat_group =
            edge_group::init(None, geometry::topology(&flat), &flat_index, "flat", &|e| e == interior).unwrap();
        let mut p = Params::new(0.2);
        p.edges = Some(flat_group.clone());
        p.ignore_flat_angle = Some(0.0);
        assert_eq!(geometry::data_id(&run(&p, &flat).unwrap()), geometry::data_id(&flat));
        let mut p = Params::new(0.1);
        p.divisions = 0;
        expect_code(run(&p, &source), Code::InvalidTopology);
        let mut p = Params::new(0.1);
        p.shape = Shape::BevelRound { convexity: 2.0 };
        expect_code(run(&p, &source), Code::InvalidTopology);
        let mut p = Params::new(0.1);
        p.ignore_flat_angle = Some(-0.1);
        expect_code(run(&p, &source), Code::InvalidTopology);
        let mut p = Params::new(0.1);
        p.edges = Some(flat_group);
        expect_code(run(&p, &source), Code::InvalidTopology);
        let scale = |zero_at: usize, value: f64| -> geometry::T {
            let values: Vec<f64> = (0..8).map(|p| if p == zero_at { value } else { 1.0 }).collect();
            geometry::with_attribute(
                attribute::create_owned("scale", attribute::Owner::Point, attribute::Storage::Float(Arc::new(values))).unwrap(),
                &source,
            )
            .unwrap()
        };
        let mut p = Params::new(0.1);
        p.point_scale_attribute = Some("scale".to_owned());
        expect_code(run(&p, &scale(3, -1.0)), Code::InvalidTopology);
        let zero = run(&p, &scale(0, 0.0)).unwrap();
        validate_closed_manifold(&zero);
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut p = Params::new(0.1);
        p.cancel = Some(token);
        expect_code(run(&p, &source), Code::Cancelled);
    }

    #[test]
    fn parallel_exact() {
        let source = with_payload(&bx());
        let edges = geometry::find_edge_group("source_edges", &source).unwrap();
        let cook = |domains| {
            with_domains(domains, &|| {
                let mut p = Params::new(0.24);
                p.grain = Some(1);
                p.edges = Some(edges.clone());
                p.shape = Shape::BevelRound { convexity: 0.8 };
                p.divisions = 5;
                p.point_scale_attribute = Some("pscale".to_owned());
                p.edge_group = Some("edge_fillets".to_owned());
                p.corner_group = Some("corner_fillets".to_owned());
                p.offset_group = Some("offset_edges".to_owned());
                run(&p, &source).unwrap()
            })
        };
        assert!(same_geometry(&cook(1), &cook(4)), "PolyBevel one/four-domain output differs");
    }

    /// Every emitted strip and corner patch winds like the faces it joins: a chamfered closed box
    /// keeps outward polygon normals and (nearly) its volume.
    #[test]
    fn outward_winding() {
        for (shape, divisions) in [(Shape::BevelChamfer, 1), (Shape::BevelRound { convexity: 0.5 }, 3)] {
            let mut p = Params::new(0.1);
            p.grain = Some(1);
            p.shape = shape;
            p.divisions = divisions;
            let output = run(&p, &bx()).unwrap();
            let view = topo(&output);
            let (x, y, z) = xyz(&output);
            let mut volume = 0.0;
            for primitive in 0..geometry::primitive_count(&output) as usize {
                let first = view.primitive_offsets[primitive] as usize;
                let last = view.primitive_offsets[primitive + 1] as usize;
                let count = last - first;
                let point = |i: usize| view.vertex_points[first + (i % count)] as usize;
                let (mut nx, mut ny, mut nz, mut cx, mut cy, mut cz) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
                for i in 0..count {
                    let (a, b) = (point(i), point(i + 1));
                    nx += (y[a] - y[b]) * (z[a] + z[b]);
                    ny += (z[a] - z[b]) * (x[a] + x[b]);
                    nz += (x[a] - x[b]) * (y[a] + y[b]);
                    cx += x[a];
                    cy += y[a];
                    cz += z[a];
                }
                assert!(nx * cx + ny * cy + nz * cz > 0.0, "PolyBevel primitive {primitive} winds inward");
                let o = point(0);
                for i in 1..count - 1 {
                    let (a, b) = (point(i), point(i + 1));
                    volume += (x[o] * (y[a] * z[b] - z[a] * y[b])
                        + y[o] * (z[a] * x[b] - x[a] * z[b])
                        + z[o] * (x[a] * y[b] - y[a] * x[b]))
                        / 6.0;
                }
            }
            assert!(volume > 7.6 && volume < 8.0, "chamfered box volume {volume:.3} is not just under 8");
        }
    }
}
