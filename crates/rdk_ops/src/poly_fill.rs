use std::sync::Arc;

use rdk_core::support::Windows;
use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, polygon_triangulation, support, topology_remap,
    topology, topology_index,
};
use rdk_core::error::Code;
use rdk_geom::point_grid::{float_max, float_min};

use crate::deform;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    FillSinglePolygon,
    FillTriangles,
    FillTriangleFan,
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub boundary: Option<edge_group::T>,
    pub mode: Mode,
    pub reverse_patches: bool,
    pub unique_points: bool,
    pub update_point_normals: bool,
    pub patch_group: Option<String>,
}

impl Params {
    pub fn new(mode: Mode) -> Params {
        Params {
            cancel: None,
            grain: None,
            boundary: None,
            mode,
            reverse_patches: false,
            unique_points: false,
            update_point_normals: false,
            patch_group: None,
        }
    }
}

const OPERATION: &str = "Rdk.Poly_fill.poly_fill";

fn make(code: Code, message: &str) -> error::T {
    error::new("poly_fill", code, message.to_owned())
}

fn fail(message: String) -> error::T {
    make(Code::InvalidGeometry, &message)
}

fn blank(value: &str) -> bool {
    value.bytes().all(|b| matches!(b, b' ' | 0x0c | b'\n' | b'\r' | b'\t'))
}

/// First loop of `range` when `loop_count` loops are split into `grain`-sized ranges.
fn first_loop(range: i64, grain: i64, loop_count: usize) -> usize {
    range.saturating_mul(grain).min(loop_count as i64) as usize
}

struct BoundaryPlan {
    source_index: topology_index::T,
    loop_offsets: Vec<usize>,
    loop_vertices: Vec<usize>,
    loop_points: Vec<usize>,
}

fn find_root(parent: &mut [i64], value: usize) -> usize {
    let mut root = value;
    while parent[root] as usize != root {
        root = parent[root] as usize;
    }
    let mut at = value;
    while parent[at] as usize != root {
        let next = parent[at] as usize;
        parent[at] = root as i64;
        at = next;
    }
    root
}

fn union_min(parent: &mut [i64], left: usize, right: usize) {
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

fn plan_boundaries(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    boundary: Option<&edge_group::T>,
    geometry_value: &geometry::T,
) -> Result<BoundaryPlan, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let source = topology::view::view(topology_value)?;
    let index = topology_index::create(cancel_ref, topology_value)?;
    let reverse = topology_index::view::view(&index);
    let edge_count = topology_index::edge_count(&index) as usize;
    match boundary {
        Some(item) if edge_group::topology_data_id(item) != topology::data_id(topology_value) => {
            return Err(fail(format!("{OPERATION}: boundary edge group belongs to another topology")));
        }
        Some(item) if edge_group::length(item) != edge_count as i64 => {
            return Err(fail(format!("{OPERATION}: boundary edge group length does not match topology")));
        }
        _ => {}
    }
    let point_count = source.point_count as usize;
    let mut candidates = vec![false; edge_count];
    let mut parent = vec![-1i64; edge_count];
    let mut first_at_point = vec![-1i64; point_count];
    let mut outgoing = vec![-1i64; point_count];
    let mut incoming = vec![-1i64; point_count];
    let mut point_bad = vec![false; point_count];
    for edge in 0..edge_count {
        cancel::poll(cancel_ref, edge as i64, 16_383)?;
        if reverse.edge_offsets[edge + 1] - reverse.edge_offsets[edge] == 1 {
            let vertex = reverse.edge_vertices[reverse.edge_offsets[edge] as usize] as usize;
            let primitive = reverse.primitive_of_vertex[vertex] as usize;
            if source.primitive_kinds[primitive] == 0 {
                candidates[edge] = true;
                parent[edge] = edge as i64;
            }
        }
    }
    if let Some(item) = boundary {
        for edge in 0..edge_count {
            if edge_group::mem(edge as i64, item) && !candidates[edge] {
                return Err(fail(format!(
                    "{OPERATION}: selected edge {edge} is not a one-sided polygon boundary edge"
                )));
            }
        }
    }
    for edge in 0..edge_count {
        cancel::poll(cancel_ref, edge as i64, 16_383)?;
        if candidates[edge] {
            let a = reverse.edge_a[edge] as usize;
            let b = reverse.edge_b[edge] as usize;
            for point in [a, b] {
                let first = first_at_point[point];
                if first < 0 {
                    first_at_point[point] = edge as i64;
                } else {
                    union_min(&mut parent, first as usize, edge);
                }
            }
        }
    }
    for edge in 0..edge_count {
        if parent[edge] >= 0 {
            parent[edge] = find_root(&mut parent, edge) as i64;
        }
    }
    for edge in 0..edge_count {
        cancel::poll(cancel_ref, edge as i64, 16_383)?;
        if candidates[edge] {
            let vertex = reverse.edge_vertices[reverse.edge_offsets[edge] as usize] as usize;
            let next = reverse.next_vertex[vertex] as usize;
            let a = source.vertex_points[vertex] as usize;
            let b = source.vertex_points[next] as usize;
            if a == b {
                point_bad[a] = true;
            }
            if outgoing[a] >= 0 {
                point_bad[a] = true;
            } else {
                outgoing[a] = edge as i64;
            }
            if incoming[b] >= 0 {
                point_bad[b] = true;
            } else {
                incoming[b] = edge as i64;
            }
        }
    }
    let mut component_bad = vec![false; edge_count];
    let mut component_size = vec![0usize; edge_count];
    let mut component_selected = vec![false; edge_count];
    for edge in 0..edge_count {
        if parent[edge] >= 0 {
            let root = parent[edge] as usize;
            component_size[root] += 1;
            match boundary {
                None => component_selected[root] = true,
                Some(item) if edge_group::mem(edge as i64, item) => component_selected[root] = true,
                Some(_) => {}
            }
        }
    }
    for point in 0..point_count {
        cancel::poll(cancel_ref, point as i64, 16_383)?;
        let edge = first_at_point[point];
        if edge >= 0 {
            let root = find_root(&mut parent, edge as usize);
            if outgoing[point] < 0 || incoming[point] < 0 || point_bad[point] {
                component_bad[root] = true;
            }
        }
    }
    let mut invalid_root: i64 = -1;
    let mut loop_count = 0usize;
    let mut boundary_count = 0usize;
    for edge in 0..edge_count {
        if parent[edge] == edge as i64 && component_selected[edge] {
            if component_bad[edge] && invalid_root < 0 {
                invalid_root = edge as i64;
            }
            loop_count += 1;
            boundary_count += component_size[edge];
        }
    }
    if invalid_root >= 0 {
        return Err(fail(format!(
            "{OPERATION}: boundary component beginning at edge {invalid_root} is branched, open, or inconsistently wound"
        )));
    }
    let mut loop_offsets = vec![0usize; loop_count + 1];
    let mut loop_roots = vec![0usize; loop_count];
    let mut loop_index = 0;
    for edge in 0..edge_count {
        if parent[edge] == edge as i64 && component_selected[edge] {
            loop_roots[loop_index] = edge;
            loop_offsets[loop_index + 1] = loop_offsets[loop_index] + component_size[edge];
            loop_index += 1;
        }
    }
    let mut loop_vertices = vec![0usize; boundary_count];
    let mut loop_points = vec![0usize; boundary_count];
    let failed_loop = std::sync::atomic::AtomicI64::new(i64::MAX);
    let loop_grain = (grain / 16).max(1);
    let loop_ranges = support::range_count(loop_count as i64, loop_grain);
    let loop_start = |range: i64| loop_offsets[first_loop(range, loop_grain, loop_count)];
    let (wvertices, wpoints) = (
        Windows::new(&mut loop_vertices, loop_ranges, loop_start),
        Windows::new(&mut loop_points, loop_ranges, loop_start),
    );
    support::run_indexed_ranges(loop_grain, loop_count as i64, &|range, begin, end| {
        let (loop_vertices, loop_points) = (wvertices.take(range), wpoints.take(range));
        let origin = loop_offsets[begin as usize];
        for loop_index in begin..end {
            support::poll(cancel_ref, loop_index)?;
            let loop_index = loop_index as usize;
            let mut failed = false;
            let first = loop_offsets[loop_index];
            let last = loop_offsets[loop_index + 1];
            let start_edge = loop_roots[loop_index];
            let mut current = start_edge as i64;
            for at in first..last {
                cancel::poll(cancel_ref, at as i64, 16_383)?;
                if current < 0 {
                    failed = true;
                    break;
                }
                let edge = current as usize;
                let vertex = reverse.edge_vertices[reverse.edge_offsets[edge] as usize] as usize;
                let next = reverse.next_vertex[vertex] as usize;
                let point = source.vertex_points[vertex] as usize;
                let next_point = source.vertex_points[next] as usize;
                loop_vertices[at - origin] = vertex;
                loop_points[at - origin] = point;
                current = outgoing[next_point];
            }
            if current != start_edge as i64 {
                failed = true;
            }
            if failed {
                failed_loop.fetch_min(loop_index as i64, std::sync::atomic::Ordering::Relaxed);
            }
        }
        Ok(())
    })?;
    let failed: i64 = match failed_loop.load(std::sync::atomic::Ordering::Relaxed) {
        i64::MAX => -1,
        value => value,
    };
    if failed >= 0 {
        return Err(fail(format!("{OPERATION}: boundary loop {failed} did not close exactly")));
    }
    Ok(BoundaryPlan { source_index: index, loop_offsets, loop_vertices, loop_points })
}

fn safe_component_mean(source: &[f64], indices: &[usize], first: usize, last: usize) -> f64 {
    let mut scale = 0.0f64;
    let mut finite = true;
    for at in first..last {
        let value = source[indices[at]];
        if !value.is_finite() {
            finite = false;
        } else if value.abs() > scale {
            scale = value.abs();
        }
    }
    let count = (last - first) as f64;
    if !finite {
        let mut sum = 0.0;
        for at in first..last {
            sum += source[indices[at]];
        }
        sum / count
    } else if scale == 0.0 {
        0.0
    } else {
        let mut sum = 0.0;
        for at in first..last {
            sum += source[indices[at]] / scale;
        }
        scale * (sum / count)
    }
}

type Centers = (Vec<f64>, Vec<f64>, Vec<f64>);

fn analyze_positions(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    plan: &BoundaryPlan,
    positions: &packed::float3::view::View,
) -> Result<Centers, error::T> {
    let loop_count = plan.loop_offsets.len() - 1;
    let (mut center_x, mut center_y, mut center_z) = (vec![0.0f64; loop_count], vec![0.0f64; loop_count], vec![0.0f64; loop_count]);
    let loop_grain = (grain / 64).max(1);
    let (wx, wy, wz) = (
        Windows::even(&mut center_x, loop_grain),
        Windows::even(&mut center_y, loop_grain),
        Windows::even(&mut center_z, loop_grain),
    );
    support::run_indexed_ranges(loop_grain, loop_count as i64, &|range, begin, end| {
        let (center_x, center_y, center_z) = (wx.take(range), wy.take(range), wz.take(range));
        for loop_index in begin..end {
            support::poll(cancel_ref, loop_index)?;
            let slot = (loop_index - begin) as usize;
            let loop_index = loop_index as usize;
            cancel::check_opt(cancel_ref)?;
            let first = plan.loop_offsets[loop_index];
            let last = plan.loop_offsets[loop_index + 1];
            let valid = (first..last).all(|at| {
                let point = plan.loop_points[at];
                positions.x[point].is_finite() && positions.y[point].is_finite() && positions.z[point].is_finite()
            });
            if !valid {
                return Err(fail(format!(
                    "{OPERATION}: boundary loop {loop_index} contains a non-finite position"
                )));
            }
            center_x[slot] = safe_component_mean(&positions.x, &plan.loop_points, first, last);
            center_y[slot] = safe_component_mean(&positions.y, &plan.loop_points, first, last);
            center_z[slot] = safe_component_mean(&positions.z, &plan.loop_points, first, last);
        }
        Ok(())
    })?;
    Ok((center_x, center_y, center_z))
}

fn triangulate_loops(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    plan: &BoundaryPlan,
    positions: &packed::float3::view::View,
) -> Result<Vec<usize>, error::T> {
    let loop_count = plan.loop_offsets.len() - 1;
    let boundary_count = plan.loop_points.len();
    let triangle_count = boundary_count - 2 * loop_count;
    let mut triangle_local = vec![0usize; triangle_count * 3];
    // failure code per loop: 2 degenerate projected area, 3 non-simple
    let mut failures = vec![0u8; loop_count];
    let loop_grain = (grain / 64).max(1);
    let ranges = support::range_count(loop_count as i64, loop_grain);
    let (wtriangle, wfailure) = (
        Windows::new(&mut triangle_local, ranges, |range| {
            let first = plan.loop_offsets[first_loop(range, loop_grain, loop_count)];
            (first - 2 * first_loop(range, loop_grain, loop_count)) * 3
        }),
        Windows::even(&mut failures, loop_grain),
    );
    support::run_indexed_ranges(loop_grain, loop_count as i64, &|range, begin, end| {
        let (triangle_local, failures) = (wtriangle.take(range), wfailure.take(range));
        let triangle_origin = (plan.loop_offsets[begin as usize] - 2 * begin as usize) * 3;
        for loop_index in begin..end {
            support::poll(cancel_ref, loop_index)?;
            let failure = &mut failures[(loop_index - begin) as usize];
            let loop_index = loop_index as usize;
            cancel::check_opt(cancel_ref)?;
            let first = plan.loop_offsets[loop_index];
            let last = plan.loop_offsets[loop_index + 1];
            let size = last - first;
            let mut u = vec![0.0f64; size];
            let mut v = vec![0.0f64; size];
            let mut remaining = vec![0i64; size];
            let mut coordinate_scale = 0.0f64;
            for at in first..last {
                let point = plan.loop_points[at];
                coordinate_scale = float_max(coordinate_scale, positions.x[point].abs());
                coordinate_scale = float_max(coordinate_scale, positions.y[point].abs());
                coordinate_scale = float_max(coordinate_scale, positions.z[point].abs());
            }
            if coordinate_scale == 0.0 {
                *failure = 2;
                continue;
            }
            let (mut nx, mut ny, mut nz) = (0.0f64, 0.0f64, 0.0f64);
            for local in 0..size {
                let next = (local + 1) % size;
                let a = plan.loop_points[first + local];
                let b = plan.loop_points[first + next];
                let ax = positions.x[a] / coordinate_scale;
                let ay = positions.y[a] / coordinate_scale;
                let az = positions.z[a] / coordinate_scale;
                let bx = positions.x[b] / coordinate_scale;
                let by = positions.y[b] / coordinate_scale;
                let bz = positions.z[b] / coordinate_scale;
                nx += (ay - by) * (az + bz);
                ny += (az - bz) * (ax + bx);
                nz += (ax - bx) * (ay + by);
            }
            let (ax, ay, az) = (nx.abs(), ny.abs(), nz.abs());
            let dominant = if ax >= ay && ax >= az { 0 } else if ay >= az { 1 } else { 2 };
            let mut umin = f64::INFINITY;
            let mut umax = f64::NEG_INFINITY;
            let mut vmin = f64::INFINITY;
            let mut vmax = f64::NEG_INFINITY;
            for local in 0..size {
                let point = plan.loop_points[first + local];
                let x = positions.x[point] / coordinate_scale;
                let y = positions.y[point] / coordinate_scale;
                let z = positions.z[point] / coordinate_scale;
                let (pu, pv) = if dominant == 0 { (y, z) } else if dominant == 1 { (x, z) } else { (x, y) };
                u[local] = pu;
                v[local] = pv;
                umin = float_min(umin, pu);
                umax = float_max(umax, pu);
                vmin = float_min(vmin, pv);
                vmax = float_max(vmax, pv);
            }
            let projection_scale = float_max(umax - umin, vmax - vmin);
            if projection_scale == 0.0 || !projection_scale.is_finite() {
                *failure = 2;
                continue;
            }
            for local in 0..size {
                u[local] = (u[local] - umin) / projection_scale;
                v[local] = (v[local] - vmin) / projection_scale;
                remaining[local] = local as i64;
            }
            let mut area = 0.0f64;
            for local in 0..size {
                let next = (local + 1) % size;
                area += u[local] * v[next] - u[next] * v[local];
            }
            if !area.is_finite() || area.abs() <= 1e-14 {
                *failure = 2;
                continue;
            }
            let orientation = if area > 0.0 { 1.0 } else { -1.0 };
            let triangle_base = (first - 2 * loop_index) * 3 - triangle_origin;
            let mut active = size;
            let mut emitted = 0usize;
            let mut ear_failed = false;
            while active > 3 && !ear_failed {
                cancel::check_opt(cancel_ref)?;
                let ear = polygon_triangulation::find_projected_ear(
                    &u,
                    &v,
                    0,
                    orientation,
                    &remaining,
                    active as i64,
                );
                if ear < 0 {
                    ear_failed = true;
                } else {
                    let ear = ear as usize;
                    let a = remaining[(ear + active - 1) % active] as usize;
                    let b = remaining[ear] as usize;
                    let c = remaining[(ear + 1) % active] as usize;
                    let output = triangle_base + emitted * 3;
                    triangle_local[output] = a;
                    triangle_local[output + 1] = b;
                    triangle_local[output + 2] = c;
                    remaining.copy_within(ear + 1..active, ear);
                    active -= 1;
                    emitted += 1;
                }
            }
            if ear_failed {
                *failure = 3;
            } else {
                let output = triangle_base + emitted * 3;
                triangle_local[output] = remaining[0] as usize;
                triangle_local[output + 1] = remaining[1] as usize;
                triangle_local[output + 2] = remaining[2] as usize;
            }
        }
        Ok(())
    })?;
    if let Some(failed) = failures.iter().position(|&code| code != 0) {
        if failures[failed] == 2 {
            return Err(fail(format!("{OPERATION}: boundary loop {failed} has degenerate projected area")));
        }
        return Err(fail(format!("{OPERATION}: boundary loop {failed} is non-simple or cannot be triangulated")));
    }
    Ok(triangle_local)
}

#[derive(Clone)]
enum CenterMap {
    None,
    Point(usize),
    Vertex { vertex_points: Arc<Vec<i64>>, first_point: usize },
}

fn center_loop(centers: &CenterMap, index: usize) -> i64 {
    match centers {
        CenterMap::None => -1,
        CenterMap::Point(first) => {
            if index < *first {
                -1
            } else {
                (index - first) as i64
            }
        }
        CenterMap::Vertex { vertex_points, first_point } => {
            let point = vertex_points[index] as usize;
            if point < *first_point {
                -1
            } else {
                (point - first_point) as i64
            }
        }
    }
}

fn map_array_f(mapping: &[i64], source: &[f64]) -> Vec<f64> {
    mapping.iter().map(|&m| source[m as usize]).collect()
}

fn map_float(
    loop_offsets: &[usize],
    loop_sources: &[usize],
    mapping: &[i64],
    centers: &CenterMap,
    source: &[f64],
) -> Vec<f64> {
    let mut output = map_array_f(mapping, source);
    if !matches!(centers, CenterMap::None) {
        let loop_count = loop_offsets.len() - 1;
        let means: Vec<f64> = (0..loop_count)
            .map(|loop_index| safe_component_mean(source, loop_sources, loop_offsets[loop_index], loop_offsets[loop_index + 1]))
            .collect();
        for index in 0..output.len() {
            let loop_index = center_loop(centers, index);
            if loop_index >= 0 {
                output[index] = means[loop_index as usize];
            }
        }
    }
    output
}

#[allow(clippy::too_many_arguments)]
fn remap_attribute(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    point_centers: &CenterMap,
    vertex_map: &[i64],
    vertex_centers: &CenterMap,
    primitive_map: &[i64],
    loop_offsets: &[usize],
    loop_points: &[usize],
    loop_vertices: &[usize],
    source: &attribute::T,
) -> Result<attribute::T, error::T> {
    let owner = attribute::owner(source);
    if owner == attribute::Owner::Detail || (owner == attribute::Owner::Point && point_map.is_empty()) {
        return Ok(source.clone());
    }
    let none = CenterMap::None;
    let (mapping, centers, center_sources): (&[i64], &CenterMap, &[usize]) = match owner {
        attribute::Owner::Point => (point_map, point_centers, loop_points),
        attribute::Owner::Vertex => (vertex_map, vertex_centers, loop_vertices),
        attribute::Owner::Primitive => (primitive_map, &none, &[]),
        attribute::Owner::Detail => return Ok(source.clone()),
    };
    let source_storage = attribute::view::storage(source)?;
    let storage = match attribute::map_float_planes(source_storage, |values| {
        Ok(map_float(loop_offsets, center_sources, mapping, centers, values))
    })? {
        Some(mapped) => mapped,
        None => topology_remap::select_storage(cancel_ref, grain, false, mapping, source_storage)?,
    };
    attribute::create_owned(attribute::name(source), owner, storage)
}

#[allow(clippy::too_many_arguments)]
fn remap_group(
    grain: i64,
    source_points: usize,
    source_vertices: usize,
    source_primitives: usize,
    point_map: &[i64],
    point_center_base: Option<usize>,
    vertex_map: &[i64],
    primitive_map: &[i64],
    source: &group::T,
) -> Result<group::T, error::T> {
    let (mapping, source_count, target_count): (&[i64], usize, usize) = match group::owner(source) {
        group::Owner::Point => (
            point_map,
            source_points,
            if point_map.is_empty() { source_points } else { point_map.len() },
        ),
        group::Owner::Vertex => (vertex_map, source_vertices, vertex_map.len()),
        group::Owner::Primitive => (primitive_map, source_primitives, primitive_map.len()),
    };
    if target_count == source_count {
        return Ok(source.clone());
    }
    let owner = group::owner(source);
    let selected = |target: i64| -> bool {
        let target = target as usize;
        match owner {
            group::Owner::Point => {
                if target < source_points {
                    group::mem(target as i64, source)
                } else {
                    match point_center_base {
                        Some(first) if target >= first => false,
                        _ => group::mem(point_map[target], source),
                    }
                }
            }
            group::Owner::Vertex => target < source_vertices && group::mem(target as i64, source),
            group::Owner::Primitive => target < source_primitives && group::mem(target as i64, source),
        }
    };
    let target = group::init(Some(grain), owner, group::name(source), target_count as i64, &selected)?;
    let ancestry: Vec<i64> = if mapping.is_empty() { (0..target_count as i64).collect() } else { mapping.to_vec() };
    group::view::remap_order(source, &ancestry, &target)
}

fn merge_patch_group(patch: group::T, groups: Vec<group::T>) -> Result<Vec<group::T>, error::T> {
    let mut groups = groups;
    for existing in groups.iter_mut() {
        if group::owner(existing) == group::Owner::Primitive && group::name(existing) == group::name(&patch) {
            *existing = group::union(existing, &patch)?;
            return Ok(groups);
        }
    }
    groups.push(patch);
    Ok(groups)
}

fn target_edge_count(
    cancel_ref: Option<&cancel::T>,
    mode: Mode,
    unique_points: bool,
    plan: &BoundaryPlan,
    triangle_local: &[usize],
) -> Result<usize, error::T> {
    let source_edges = topology_index::edge_count(&plan.source_index) as usize;
    let loop_count = plan.loop_offsets.len() - 1;
    let boundary_count = plan.loop_points.len();
    Ok(if unique_points {
        source_edges
            + match mode {
                Mode::FillSinglePolygon => boundary_count,
                Mode::FillTriangles => 2 * boundary_count - 3 * loop_count,
                Mode::FillTriangleFan => 2 * boundary_count,
            }
    } else {
        match mode {
            Mode::FillSinglePolygon => source_edges,
            Mode::FillTriangleFan => source_edges + boundary_count,
            Mode::FillTriangles => {
                let mut added = 0;
                for loop_index in 0..loop_count {
                    cancel::poll(cancel_ref, loop_index as i64, 4_095)?;
                    let first = plan.loop_offsets[loop_index];
                    let size = plan.loop_offsets[loop_index + 1] - first;
                    let triangle_base = (first - 2 * loop_index) * 3;
                    for triangle in 0..size.saturating_sub(3) {
                        let a = triangle_local[triangle_base + triangle * 3];
                        let c = triangle_local[triangle_base + triangle * 3 + 2];
                        let a = plan.loop_points[first + a] as i64;
                        let c = plan.loop_points[first + c] as i64;
                        if topology_index::find_edge_index(&plan.source_index, a, c) < 0 {
                            added += 1;
                        }
                    }
                }
                source_edges + added
            }
        }
    })
}

fn remap_edge_groups(
    cancel_ref: Option<&cancel::T>,
    mode: Mode,
    unique_points: bool,
    plan: &BoundaryPlan,
    triangle_local: &[usize],
    target_topology: &topology::T,
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let edge_count = target_edge_count(cancel_ref, mode, unique_points, plan, triangle_local)?;
    let mut result = Vec::new();
    for source in groups {
        let mut bits = vec![0u8; edge_count.div_ceil(8)];
        for edge in 0..edge_group::length(source) {
            if edge_group::mem(edge, source) {
                bits[(edge >> 3) as usize] |= 1 << (edge & 7);
            }
        }
        result.push(edge_group::view::of_owned_bits(
            target_topology,
            edge_count as i64,
            edge_group::name(source),
            bits,
        )?);
    }
    Ok(result)
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(make(Code::InvalidArgument, &format!("{OPERATION}: grain must be positive")));
    }
    run_inner(params, grain, geometry_value).map_err(|value| error::guard(value, "poly_fill", Code::InvalidGeometry))
}

#[allow(clippy::too_many_lines)]
fn run_inner(params: &Params, grain: i64, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_ref = params.cancel.as_deref();
    let mode = params.mode;
    let unique_points = params.unique_points;
    let reverse_patches = params.reverse_patches;
    if let Some(name) = &params.patch_group {
        if blank(name) {
            return Err(fail(format!("{OPERATION}: patch group name must not be empty")));
        }
    }
    let plan = plan_boundaries(cancel_ref, grain, params.boundary.as_ref(), geometry_value)?;
    let loop_count = plan.loop_offsets.len() - 1;
    if loop_count == 0 {
        return Ok(geometry_value.clone());
    }
    let source_topology = geometry::topology(geometry_value);
    let source = topology::view::view(source_topology)?;
    let source_positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    let source_points = source.point_count as usize;
    let source_vertices = source.vertex_points.len();
    let source_primitives = source.primitive_kinds.len();
    let boundary_count = plan.loop_points.len();
    let patch_primitives = match mode {
        Mode::FillSinglePolygon => loop_count,
        Mode::FillTriangles => boundary_count - 2 * loop_count,
        Mode::FillTriangleFan => boundary_count,
    };
    let patch_vertices = match mode {
        Mode::FillSinglePolygon => boundary_count,
        Mode::FillTriangles => 3 * patch_primitives,
        Mode::FillTriangleFan => 3 * boundary_count,
    };
    let added_boundary_points = if unique_points { boundary_count } else { 0 };
    let added_centers = if mode == Mode::FillTriangleFan { loop_count } else { 0 };
    let output_points = source_points + added_boundary_points + added_centers;
    let output_vertices = source_vertices + patch_vertices;
    let output_primitives = source_primitives + patch_primitives;
    let (center_x, center_y, center_z) = analyze_positions(cancel_ref, grain, &plan, &source_positions)?;
    let triangle_local = if mode == Mode::FillTriangles {
        triangulate_loops(cancel_ref, grain, &plan, &source_positions)?
    } else {
        Vec::new()
    };
    let mut point_map: Vec<i64> = if output_points == source_points { Vec::new() } else { vec![0; output_points] };
    let point_center_base = if mode == Mode::FillTriangleFan { Some(source_points + added_boundary_points) } else { None };
    let output_positions = if output_points == source_points {
        geometry::positions(geometry_value).clone()
    } else {
        let mut x = vec![0.0f64; output_points];
        let mut y = vec![0.0f64; output_points];
        let mut z = vec![0.0f64; output_points];
        x[..source_points].copy_from_slice(&source_positions.x[..source_points]);
        y[..source_points].copy_from_slice(&source_positions.y[..source_points]);
        z[..source_points].copy_from_slice(&source_positions.z[..source_points]);
        for point in 0..source_points {
            point_map[point] = point as i64;
        }
        let center_base = source_points + added_boundary_points;
        for loop_index in 0..loop_count {
            let first = plan.loop_offsets[loop_index];
            let last = plan.loop_offsets[loop_index + 1];
            if unique_points {
                for at in first..last {
                    let output = source_points + at;
                    let source_point = plan.loop_points[at];
                    point_map[output] = source_point as i64;
                    x[output] = source_positions.x[source_point];
                    y[output] = source_positions.y[source_point];
                    z[output] = source_positions.z[source_point];
                }
            }
            if mode == Mode::FillTriangleFan {
                let output = center_base + loop_index;
                point_map[output] = plan.loop_points[first] as i64;
                x[output] = center_x[loop_index];
                y[output] = center_y[loop_index];
                z[output] = center_z[loop_index];
            }
        }
        packed::float3::view::of_owned_exn(x, y, z)?
    };
    let mut patch_primitive_base = vec![0usize; loop_count + 1];
    let mut patch_vertex_base = vec![0usize; loop_count + 1];
    let mut primitive_at = source_primitives;
    let mut vertex_at = source_vertices;
    for loop_index in 0..loop_count {
        let size = plan.loop_offsets[loop_index + 1] - plan.loop_offsets[loop_index];
        patch_primitive_base[loop_index] = primitive_at;
        patch_vertex_base[loop_index] = vertex_at;
        primitive_at += match mode {
            Mode::FillSinglePolygon => 1,
            Mode::FillTriangles => size - 2,
            Mode::FillTriangleFan => size,
        };
        vertex_at += match mode {
            Mode::FillSinglePolygon => size,
            Mode::FillTriangles => (size - 2) * 3,
            Mode::FillTriangleFan => size * 3,
        };
    }
    patch_primitive_base[loop_count] = primitive_at;
    patch_vertex_base[loop_count] = vertex_at;
    let mut vertex_points = vec![0i64; output_vertices];
    let mut vertex_map = vec![0i64; output_vertices];
    let mut primitive_offsets = vec![0i64; output_primitives + 1];
    let mut primitive_map = vec![0i64; output_primitives];
    let mut primitive_kinds = vec![0u8; output_primitives];
    vertex_points[..source_vertices].copy_from_slice(&source.vertex_points[..source_vertices]);
    primitive_offsets[..source_primitives + 1].copy_from_slice(&source.primitive_offsets[..source_primitives + 1]);
    primitive_kinds[..source_primitives].copy_from_slice(&source.primitive_kinds[..source_primitives]);
    for vertex in 0..source_vertices {
        vertex_map[vertex] = vertex as i64;
    }
    for primitive in 0..source_primitives {
        primitive_map[primitive] = primitive as i64;
    }
    let reverse = topology_index::view::view(&plan.source_index);
    let point_of_local = |loop_index: usize, local: usize| -> i64 {
        if unique_points {
            (source_points + plan.loop_offsets[loop_index] + local) as i64
        } else {
            plan.loop_points[plan.loop_offsets[loop_index] + local] as i64
        }
    };
    let vertex_of_local = |loop_index: usize, local: usize| -> i64 {
        plan.loop_vertices[plan.loop_offsets[loop_index] + local] as i64
    };
    let source_primitive_of_local = |loop_index: usize, local: usize| -> i64 {
        reverse.primitive_of_vertex[vertex_of_local(loop_index, local) as usize]
    };
    let center_base = source_points + added_boundary_points;
    let patch_grain = (grain / 64).max(1);
    let patch_ranges = support::range_count(loop_count as i64, patch_grain);
    let (patch_points, patch_map) = (&mut vertex_points[source_vertices..], &mut vertex_map[source_vertices..]);
    let vertex_start = |range: i64| patch_vertex_base[first_loop(range, patch_grain, loop_count)] - source_vertices;
    let primitive_start = |range: i64| patch_primitive_base[first_loop(range, patch_grain, loop_count)] - source_primitives;
    let (wvp, wvm) = (
        Windows::new(patch_points, patch_ranges, vertex_start),
        Windows::new(patch_map, patch_ranges, vertex_start),
    );
    let (woff, wpm) = (
        Windows::new(&mut primitive_offsets[source_primitives..output_primitives], patch_ranges, primitive_start),
        Windows::new(&mut primitive_map[source_primitives..], patch_ranges, primitive_start),
    );
    support::run_indexed_ranges(patch_grain, loop_count as i64, &|range, begin, end| {
        let (vertex_points, vertex_map) = (wvp.take(range), wvm.take(range));
        let (primitive_offsets, primitive_map) = (woff.take(range), wpm.take(range));
        let (primitive_origin, vertex_origin) = (patch_primitive_base[begin as usize], patch_vertex_base[begin as usize]);
        for loop_index in begin..end {
            support::poll(cancel_ref, loop_index)?;
            let loop_index = loop_index as usize;
            cancel::check_opt(cancel_ref)?;
            let first = plan.loop_offsets[loop_index];
            let size = plan.loop_offsets[loop_index + 1] - first;
            let primitive_base = patch_primitive_base[loop_index];
            let vertex_base = patch_vertex_base[loop_index];
            match mode {
                Mode::FillSinglePolygon => {
                    primitive_offsets[primitive_base - primitive_origin] = vertex_base as i64;
                    primitive_map[primitive_base - primitive_origin] = source_primitive_of_local(loop_index, 0);
                    for output_local in 0..size {
                        let source_local =
                            if reverse_patches || output_local == 0 { output_local } else { size - output_local };
                        let output = vertex_base + output_local - vertex_origin;
                        vertex_points[output] = point_of_local(loop_index, source_local);
                        vertex_map[output] = vertex_of_local(loop_index, source_local);
                    }
                }
                Mode::FillTriangles => {
                    let triangle_base = (first - 2 * loop_index) * 3;
                    for triangle in 0..size - 2 {
                        let primitive = primitive_base + triangle - primitive_origin;
                        let output = vertex_base + triangle * 3;
                        let window = output - vertex_origin;
                        let source_triangle = triangle_base + triangle * 3;
                        let a = triangle_local[source_triangle];
                        let b = triangle_local[source_triangle + 1];
                        let c = triangle_local[source_triangle + 2];
                        let (a, b, c) = if reverse_patches { (a, b, c) } else { (a, c, b) };
                        primitive_offsets[primitive] = output as i64;
                        primitive_map[primitive] = source_primitive_of_local(loop_index, a);
                        vertex_points[window] = point_of_local(loop_index, a);
                        vertex_points[window + 1] = point_of_local(loop_index, b);
                        vertex_points[window + 2] = point_of_local(loop_index, c);
                        vertex_map[window] = vertex_of_local(loop_index, a);
                        vertex_map[window + 1] = vertex_of_local(loop_index, b);
                        vertex_map[window + 2] = vertex_of_local(loop_index, c);
                    }
                }
                Mode::FillTriangleFan => {
                    let center = (center_base + loop_index) as i64;
                    for edge in 0..size {
                        let next = (edge + 1) % size;
                        let primitive = primitive_base + edge - primitive_origin;
                        let output = vertex_base + edge * 3;
                        let window = output - vertex_origin;
                        primitive_offsets[primitive] = output as i64;
                        primitive_map[primitive] = source_primitive_of_local(loop_index, edge);
                        if reverse_patches {
                            vertex_points[window] = point_of_local(loop_index, edge);
                            vertex_points[window + 1] = point_of_local(loop_index, next);
                            vertex_points[window + 2] = center;
                            vertex_map[window] = vertex_of_local(loop_index, edge);
                            vertex_map[window + 1] = vertex_of_local(loop_index, next);
                            vertex_map[window + 2] = vertex_of_local(loop_index, 0);
                        } else {
                            vertex_points[window] = point_of_local(loop_index, edge);
                            vertex_points[window + 1] = center;
                            vertex_points[window + 2] = point_of_local(loop_index, next);
                            vertex_map[window] = vertex_of_local(loop_index, edge);
                            vertex_map[window + 1] = vertex_of_local(loop_index, 0);
                            vertex_map[window + 2] = vertex_of_local(loop_index, next);
                        }
                    }
                }
            }
        }
        Ok(())
    })?;
    primitive_offsets[output_primitives] = output_vertices as i64;
    let point_centers = match point_center_base {
        None => CenterMap::None,
        Some(first) => CenterMap::Point(first),
    };
    let vertex_centers = match point_center_base {
        None => CenterMap::None,
        Some(first_point) => CenterMap::Vertex { vertex_points: Arc::new(vertex_points.clone()), first_point },
    };
    let output_topology = topology::view::create_validated_owned(
        output_points as i64,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        attributes.push(remap_attribute(
            cancel_ref,
            grain,
            &point_map,
            &point_centers,
            &vertex_map,
            &vertex_centers,
            &primitive_map,
            &plan.loop_offsets,
            &plan.loop_points,
            &plan.loop_vertices,
            &item,
        )?);
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        groups.push(remap_group(
            grain,
            source_points,
            source_vertices,
            source_primitives,
            &point_map,
            point_center_base,
            &vertex_map,
            &primitive_map,
            &item,
        )?);
    }
    if let Some(name) = &params.patch_group {
        let patch = group::init(Some(grain), group::Owner::Primitive, name, output_primitives as i64, &|primitive| {
            primitive as usize >= source_primitives
        })?;
        groups = merge_patch_group(patch, groups)?;
    }
    let edge_groups = remap_edge_groups(
        cancel_ref,
        mode,
        unique_points,
        &plan,
        &triangle_local,
        &output_topology,
        &geometry::edge_groups(geometry_value),
    )?;
    let output = geometry::create(
        output_positions,
        output_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )?;
    if params.update_point_normals
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
    use rdk_core::topology::PrimitiveKind;

    fn near(a: f64, b: f64) -> bool {
        (a - b).abs() <= 1e-12
    }

    fn open_box(offset: f64) -> geometry::T {
        let x = vec![-1.0 + offset, 1.0 + offset, 1.0 + offset, -1.0 + offset, -1.0 + offset, 1.0 + offset, 1.0 + offset, -1.0 + offset];
        let y = vec![-1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0];
        let z = vec![-1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0];
        let source = polygons(
            x,
            y,
            z,
            &[vec![0, 3, 2, 1], vec![0, 1, 5, 4], vec![1, 2, 6, 5], vec![2, 3, 7, 6], vec![3, 0, 4, 7]],
        );
        let add = |owner: attribute::Owner, name: &str, storage: attribute::Storage, g: &geometry::T| {
            with_owned_attribute(name, owner, storage, g)
        };
        let ints = |n: i64| (0..n).collect::<Vec<i64>>();
        let floats = |n: i64| (0..n).map(|v| v as f64).collect::<Vec<f64>>();
        let g = add(attribute::Owner::Point, "weight", attribute::Storage::Float(Arc::new(floats(8))), &source);
        let g = add(attribute::Owner::Point, "point_id", attribute::Storage::Int(Arc::new(ints(8))), &g);
        let g = add(
            attribute::Owner::Point,
            "point_tag",
            attribute::Storage::Text(Arc::new((0..8).map(|p| format!("p{p}").into_bytes()).collect())),
            &g,
        );
        let g = add(
            attribute::Owner::Point,
            "point_uv",
            attribute::Storage::Float2(packed::float2::of_owned(floats(8), (0..8).map(|p| -(p as f64)).collect()).unwrap()),
            &g,
        );
        let g = add(
            attribute::Owner::Point,
            "point_vector",
            attribute::Storage::Float3(
                packed::float3::view::of_owned_exn(
                    floats(8),
                    (0..8).map(|p| (p + 10) as f64).collect(),
                    (0..8).map(|p| (p + 20) as f64).collect(),
                )
                .unwrap(),
            ),
            &g,
        );
        let g = add(
            attribute::Owner::Point,
            "point_quaternion",
            attribute::Storage::Float4(packed::float4::of_owned(floats(8), vec![0.0; 8], vec![0.0; 8], vec![1.0; 8]).unwrap()),
            &g,
        );
        let g = add(
            attribute::Owner::Point,
            "point_int_array",
            attribute::Storage::IntArray(
                packed::int_array::create_owned((0..9).map(|i| i * 2).collect(), ints(16)).unwrap(),
            ),
            &g,
        );
        let g = add(
            attribute::Owner::Point,
            "point_float_array",
            attribute::Storage::FloatArray(
                packed::float_array::create_owned((0..9).map(|i| i * 2).collect(), floats(16)).unwrap(),
            ),
            &g,
        );
        let g = add(attribute::Owner::Vertex, "vertex_weight", attribute::Storage::Float(Arc::new(floats(20))), &g);
        let g = add(attribute::Owner::Primitive, "primitive_id", attribute::Storage::Int(Arc::new(vec![10, 20, 30, 40, 50])), &g);
        let top = group::init(None, group::Owner::Point, "top", 8, &|p| p >= 4).unwrap();
        let first_vertex = group::init(None, group::Owner::Vertex, "first_vertex", 20, &|v| v == 0).unwrap();
        let bottom = group::init(None, group::Owner::Primitive, "bottom", 5, &|p| p == 0).unwrap();
        let g = geometry::with_group(top, &g).unwrap();
        let g = geometry::with_group(first_vertex, &g).unwrap();
        let g = geometry::with_group(bottom, &g).unwrap();
        let topology_value = geometry::topology(&g).clone();
        let index = topology_index::create(None, &topology_value).unwrap();
        let (_, _, pz) = xyz(&g);
        let rim = edge_group::init(None, &topology_value, &index, "rim", &|edge| {
            topology_index::edge_incidence_count(&index, edge).unwrap() == 1 && {
                let (a, b) = topology_index::edge_points(&index, edge).unwrap();
                pz[a as usize] == 1.0 && pz[b as usize] == 1.0
            }
        })
        .unwrap();
        geometry::with_edge_group(rim, &g).unwrap()
    }

    fn run_mode(mode: Mode, patch: bool, source: &geometry::T) -> geometry::T {
        let mut p = Params::new(mode);
        if patch {
            p.patch_group = Some("patch".to_owned());
        }
        run(&p, source).unwrap()
    }

    fn cardinality(owner: group::Owner, name: &str, source: &geometry::T) -> i64 {
        group::cardinality(&geometry::find_group(owner, name, source).unwrap_or_else(|| panic!("missing group {name}")))
    }

    fn assert_closed(source: &geometry::T, message: &str) {
        assert_eq!(boundary_and_non_manifold(source).0, 0, "{message}");
    }

    #[test]
    fn modes_and_payload() {
        let source = open_box(0.0);
        let single = run_mode(Mode::FillSinglePolygon, true, &source);
        assert!(geometry::point_count(&single) == 8 && geometry::vertex_count(&single) == 24 && geometry::primitive_count(&single) == 6);
        let (st, tt) = (topo(&source), topo(&single));
        assert_eq!(tt.vertex_points[..st.vertex_points.len()], st.vertex_points[..]);
        assert_eq!(tt.primitive_offsets[..st.primitive_offsets.len()], st.primitive_offsets[..]);
        assert_eq!(tt.primitive_kinds[..st.primitive_kinds.len()], st.primitive_kinds[..]);
        assert_closed(&single, "single-polygon fill left a boundary");
        assert_eq!(cardinality(group::Owner::Primitive, "patch", &single), 1);
        assert_eq!(cardinality(group::Owner::Primitive, "bottom", &single), 1);
        let (sx, sy, _) = xyz(&single);
        let first = tt.primitive_offsets[5] as usize;
        let (sa, sb, sc) = (
            tt.vertex_points[first] as usize,
            tt.vertex_points[first + 1] as usize,
            tt.vertex_points[first + 2] as usize,
        );
        assert!((sx[sb] - sx[sa]) * (sy[sc] - sy[sa]) - (sy[sb] - sy[sa]) * (sx[sc] - sx[sa]) > 0.0);
        let triangles = run_mode(Mode::FillTriangles, true, &source);
        assert_eq!(topo(&triangles).vertex_points[20..26], [6, 4, 5, 4, 6, 7]);
        assert!(geometry::point_count(&triangles) == 8 && geometry::vertex_count(&triangles) == 26 && geometry::primitive_count(&triangles) == 7);
        assert_closed(&triangles, "triangle fill left a boundary");
        assert_eq!(cardinality(group::Owner::Primitive, "patch", &triangles), 2);
        let fan = run_mode(Mode::FillTriangleFan, true, &source);
        assert!(geometry::point_count(&fan) == 9 && geometry::vertex_count(&fan) == 32 && geometry::primitive_count(&fan) == 9);
        assert_closed(&fan, "triangle fan left a boundary");
        let (fx, fy, fz) = xyz(&fan);
        assert!(near(fx[8], 0.0) && near(fy[8], 0.0) && near(fz[8], 1.0));
        match attribute::view::storage(&geometry::find_attribute(attribute::Owner::Point, "weight", &fan).unwrap()).unwrap() {
            attribute::Storage::Float(values) => assert!(near(values[8], 5.5)),
            _ => panic!("point weight storage changed"),
        }
        let ids = int_attribute(attribute::Owner::Point, "point_id", &fan);
        assert!(ids[8] >= 4 && ids[8] <= 7);
        assert_eq!(cardinality(group::Owner::Point, "top", &fan), 4);
        assert_eq!(cardinality(group::Owner::Vertex, "first_vertex", &fan), 1);
        assert_eq!(cardinality(group::Owner::Primitive, "bottom", &fan), 1);
        assert_eq!(cardinality(group::Owner::Primitive, "patch", &fan), 4);
        let source_index = topology_index::create(None, geometry::topology(&source)).unwrap();
        let (_, _, source_z) = xyz(&source);
        let mut boundary_vertices = Vec::new();
        for edge in 0..topology_index::edge_count(&source_index) {
            if topology_index::edge_incidence_count(&source_index, edge).unwrap() == 1 {
                let (a, b) = topology_index::edge_points(&source_index, edge).unwrap();
                if source_z[a as usize] == 1.0 && source_z[b as usize] == 1.0 {
                    boundary_vertices.push(topology_index::edge_vertex(&source_index, edge, 0).unwrap());
                }
            }
        }
        let expected_vertex_mean = boundary_vertices.iter().sum::<i64>() as f64 / boundary_vertices.len() as f64;
        let fan_topology = topo(&fan);
        match attribute::view::storage(&geometry::find_attribute(attribute::Owner::Vertex, "vertex_weight", &fan).unwrap()).unwrap() {
            attribute::Storage::Float(values) => {
                for vertex in 20..fan_topology.vertex_points.len() {
                    if fan_topology.vertex_points[vertex] == 8 {
                        assert!(near(values[vertex], expected_vertex_mean));
                    }
                }
            }
            _ => panic!("vertex weight storage changed"),
        }
        for (owner, name, expected) in [
            (attribute::Owner::Point, "point_tag", 9),
            (attribute::Owner::Point, "point_uv", 9),
            (attribute::Owner::Point, "point_vector", 9),
            (attribute::Owner::Point, "point_quaternion", 9),
            (attribute::Owner::Point, "point_int_array", 9),
            (attribute::Owner::Point, "point_float_array", 9),
            (attribute::Owner::Primitive, "primitive_id", 9),
        ] {
            let found = geometry::find_attribute(owner, name, &fan).unwrap();
            assert_eq!(attribute::length(&found), expected, "attribute length mismatch for {name}");
        }
        assert_eq!(edge_group::cardinality(&geometry::find_edge_group("rim", &fan).unwrap()), 4);
        for (label, g) in [("single", &single), ("triangles", &triangles), ("fan", &fan)] {
            let rim = geometry::find_edge_group("rim", g).unwrap();
            let edges = topology_index::edge_count(&topology_index::create(None, geometry::topology(g)).unwrap());
            assert_eq!(edge_group::length(&rim), edges, "{label} native edge-group target cardinality");
        }
    }

    #[test]
    fn unique_reverse_and_normals() {
        let source = open_box(0.0);
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.unique_points = true;
        let unique = run(&p, &source).unwrap();
        assert_eq!(geometry::point_count(&unique), 12);
        assert_eq!(boundary_and_non_manifold(&unique).0, 8);
        assert_eq!(cardinality(group::Owner::Point, "top", &unique), 8);
        let unique_rim = geometry::find_edge_group("rim", &unique).unwrap();
        assert_eq!(
            edge_group::length(&unique_rim),
            topology_index::edge_count(&topology_index::create(None, geometry::topology(&unique)).unwrap())
        );
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.reverse_patches = true;
        let reversed = run(&p, &source).unwrap();
        let t = topo(&reversed);
        let (x, y, _) = xyz(&reversed);
        let first = t.primitive_offsets[5] as usize;
        let (a, b, c) = (t.vertex_points[first] as usize, t.vertex_points[first + 1] as usize, t.vertex_points[first + 2] as usize);
        assert!((x[b] - x[a]) * (y[c] - y[a]) - (y[b] - y[a]) * (x[c] - x[a]) < 0.0);
        let point_normal = attribute::create_owned(
            "N",
            attribute::Owner::Point,
            attribute::Storage::Float3(packed::float3::view::of_owned_exn(vec![1.0; 8], vec![0.0; 8], vec![0.0; 8]).unwrap()),
        )
        .unwrap();
        let with_normal = geometry::with_attribute(point_normal, &source).unwrap();
        let mut p = Params::new(Mode::FillTriangleFan);
        p.update_point_normals = true;
        let updated = run(&p, &with_normal).unwrap();
        let normal = geometry::find_attribute(attribute::Owner::Point, "N", &updated).unwrap();
        assert_eq!(attribute::length(&normal), 9);
        match attribute::view::storage(&normal).unwrap() {
            attribute::Storage::Float3(values) => {
                let values = packed::float3::view::view(values).unwrap();
                assert!(near(values.x[8], 0.0) && near(values.y[8], 0.0) && values.z[8] > 0.999999);
            }
            _ => panic!("point normal storage changed"),
        }
        let no_normal = run(&p, &source).unwrap();
        assert!(geometry::find_attribute(attribute::Owner::Point, "N", &no_normal).is_none());
    }

    fn polygon_of(points: &[(f64, f64, f64)]) -> geometry::T {
        let n = points.len() as i64;
        polygons(
            points.iter().map(|p| p.0).collect(),
            points.iter().map(|p| p.1).collect(),
            points.iter().map(|p| p.2).collect(),
            &[(0..n).collect()],
        )
    }

    #[test]
    fn selection_and_failures() {
        let merged = merge(&[open_box(0.0), open_box(4.0)]);
        let topology_value = geometry::topology(&merged).clone();
        let index = topology_index::create(None, &topology_value).unwrap();
        let (x, _, z) = xyz(&merged);
        let (mut one_edge, mut interior) = (-1i64, -1i64);
        for edge in 0..topology_index::edge_count(&index) {
            let (a, b) = topology_index::edge_points(&index, edge).unwrap();
            let (a, b) = (a as usize, b as usize);
            let incidence = topology_index::edge_incidence_count(&index, edge).unwrap();
            if one_edge < 0 && incidence == 1 && x[a] < 2.0 && x[b] < 2.0 && z[a] == 1.0 && z[b] == 1.0 {
                one_edge = edge;
            }
            if interior < 0 && incidence == 2 {
                interior = edge;
            }
        }
        let selected = edge_group::init(None, &topology_value, &index, "selected", &|e| e == one_edge).unwrap();
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.boundary = Some(selected);
        let output = run(&p, &merged).unwrap();
        assert_eq!(geometry::primitive_count(&output), geometry::primitive_count(&merged) + 1);
        let bad = edge_group::init(None, &topology_value, &index, "bad", &|e| e == interior).unwrap();
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.boundary = Some(bad);
        expect_code(run(&p, &merged), Code::InvalidGeometry);
        let empty = edge_group::init(None, &topology_value, &index, "empty", &|_| false).unwrap();
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.boundary = Some(empty);
        let unchanged = run(&p, &merged).unwrap();
        assert_eq!(geometry::data_id(&unchanged), geometry::data_id(&merged), "empty explicit boundary was not identity");
        let other = open_box(20.0);
        let other_index = topology_index::create(None, geometry::topology(&other)).unwrap();
        let foreign = edge_group::init(None, geometry::topology(&other), &other_index, "foreign", &|e| e == 0).unwrap();
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.boundary = Some(foreign);
        expect_code(run(&p, &merged), Code::InvalidGeometry);
        let branched = polygons(
            vec![0.0, 1.0, 0.0, -1.0, 0.0],
            vec![0.0, 0.0, 1.0, 0.0, -1.0],
            vec![0.0; 5],
            &[vec![0, 1, 2], vec![0, 3, 4]],
        );
        expect_code(run(&Params::new(Mode::FillSinglePolygon), &branched), Code::InvalidGeometry);
        let nonfinite = open_box(0.0);
        let (mut nx, ny, nz) = xyz(&nonfinite);
        nx[4] = f64::NAN;
        let nonfinite = geometry::with_positions(packed::float3::view::of_owned_exn(nx, ny, nz).unwrap(), &nonfinite).unwrap();
        expect_code(run(&Params::new(Mode::FillSinglePolygon), &nonfinite), Code::InvalidGeometry);
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.patch_group = Some(String::new());
        expect_code(run(&p, &merged), Code::InvalidGeometry);
        let concave = polygon_of(&[(0.0, 0.0, 0.0), (2.0, 0.0, 0.0), (2.0, 2.0, 0.0), (1.0, 0.8, 0.0), (0.0, 2.0, 0.0)]);
        let concave_fill = run(&Params::new(Mode::FillTriangles), &concave).unwrap();
        assert!(geometry::primitive_count(&concave_fill) == 4 && geometry::vertex_count(&concave_fill) == 14);
        let crossed = polygon_of(&[(0.0, 0.0, 0.0), (2.0, 2.0, 0.0), (0.0, 2.0, 0.0), (2.0, 0.0, 0.0)]);
        expect_code(run(&Params::new(Mode::FillTriangles), &crossed), Code::InvalidGeometry);
        let diagonal_source = open_box(0.0);
        let dt = topo(&diagonal_source);
        let mut vertex_points = dt.vertex_points.to_vec();
        vertex_points.extend_from_slice(&[4, 6]);
        let topology_value = topology::create_owned(
            8,
            vertex_points,
            vec![0, 4, 8, 12, 16, 20, 22],
            vec![
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::OpenPolyline,
            ],
        )
        .unwrap();
        let diagonal_source = geometry::create(geometry::positions(&diagonal_source).clone(), topology_value.clone(), None, None, None).unwrap();
        let diagonal_index = topology_index::create(None, &topology_value).unwrap();
        let all_edges = edge_group::init(None, &topology_value, &diagonal_index, "all", &|_| true).unwrap();
        let diagonal_source = geometry::with_edge_group(all_edges, &diagonal_source).unwrap();
        let diagonal_fill = run(&Params::new(Mode::FillTriangles), &diagonal_source).unwrap();
        let mapped = geometry::find_edge_group("all", &diagonal_fill).unwrap();
        assert_eq!(
            edge_group::length(&mapped),
            topology_index::edge_count(&topology_index::create(None, geometry::topology(&diagonal_fill)).unwrap()),
            "existing source diagonal was double-counted in target edge cardinality"
        );
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut p = Params::new(Mode::FillSinglePolygon);
        p.cancel = Some(token);
        expect_code(run(&p, &merged), Code::Cancelled);
    }

    fn many_open_boxes(count: usize) -> geometry::T {
        let local_x = [-1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0];
        let local_y = [-1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0];
        let local_z = [-1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0];
        let local_vertices: [i64; 20] = [0, 3, 2, 1, 0, 1, 5, 4, 1, 2, 6, 5, 2, 3, 7, 6, 3, 0, 4, 7];
        let (mut x, mut y, mut z) = (Vec::new(), Vec::new(), Vec::new());
        let mut polys = Vec::new();
        for b in 0..count {
            let (gx, gy) = ((b % 256) as f64, (b / 256) as f64);
            for l in 0..8 {
                x.push(gx * 3.0 + local_x[l]);
                y.push(gy * 3.0 + local_y[l]);
                z.push(local_z[l]);
            }
            for p in 0..5 {
                polys.push((0..4).map(|k| local_vertices[p * 4 + k] + (b * 8) as i64).collect::<Vec<i64>>());
            }
        }
        let g = polygons(x, y, z, &polys);
        let g = with_owned_attribute("id", attribute::Owner::Point, attribute::Storage::Int(Arc::new((0..count as i64 * 8).collect())), &g);
        let g = with_owned_attribute(
            "u",
            attribute::Owner::Vertex,
            attribute::Storage::Float(Arc::new((0..count * 20).map(|v| v as f64).collect())),
            &g,
        );
        with_owned_attribute(
            "piece",
            attribute::Owner::Primitive,
            attribute::Storage::Int(Arc::new((0..count as i64 * 5).map(|p| p / 5).collect())),
            &g,
        )
    }

    #[test]
    fn parallel_scale() {
        let n = 20_000usize;
        let source = many_open_boxes(n);
        let triangles = |domains| {
            with_domains(domains, &|| {
                let mut p = Params::new(Mode::FillTriangles);
                p.grain = Some(257);
                p.patch_group = Some("patch".to_owned());
                run(&p, &source).unwrap()
            })
        };
        let (one, four) = (triangles(1), triangles(4));
        assert!(same_geometry(&one, &four));
        assert!(
            geometry::point_count(&one) as usize == n * 8
                && geometry::vertex_count(&one) as usize == n * 26
                && geometry::primitive_count(&one) as usize == n * 7
        );
        let fan = |domains| {
            with_domains(domains, &|| {
                let mut p = Params::new(Mode::FillTriangleFan);
                p.grain = Some(257);
                p.unique_points = true;
                p.patch_group = Some("patch".to_owned());
                run(&p, &source).unwrap()
            })
        };
        let (one, four) = (fan(1), fan(4));
        assert!(same_geometry(&one, &four));
        assert!(
            geometry::point_count(&one) as usize == n * 13
                && geometry::vertex_count(&one) as usize == n * 32
                && geometry::primitive_count(&one) as usize == n * 9
                && cardinality(group::Owner::Primitive, "patch", &one) as usize == n * 4
        );
    }
}
