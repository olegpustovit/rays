//! The Facet SOP: unique points, point and normal consolidation, inline-point removal, polygon
//! orientation and cusping, degenerate removal, planarization and normal adjustment, composed
//! over an optional selection.
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use rdk_core::{
    attribute, cancel, edge_group, element_selection, error, face_normals, geometry, group, packed, support,
    topology, topology_index, topology_remap,
};
use rdk_geom::point_clusters::{self, Metric};

use crate::clean;
use crate::deform;
use crate::fuse_grid;

const GUARD_OPERATION: &str = "facet";
/// OCaml `max_int`.
const MAX_INT: i64 = (1 << 62) - 1;


#[derive(Default)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selection: Option<element_selection::T>,
    pub pre_compute_normals: Option<bool>,
    pub make_normals_unit_length: Option<bool>,
    pub unique_points: Option<bool>,
    pub consolidate_distance: Option<f64>,
    pub consolidate_normals_distance: Option<f64>,
    pub remove_inline_points: Option<bool>,
    pub inline_distance: Option<f64>,
    pub orient_polygons: Option<bool>,
    pub cusp_angle: Option<f64>,
    pub remove_degenerate: Option<bool>,
    pub make_planar: Option<bool>,
    pub post_compute_normals: Option<bool>,
    pub reverse_normals: Option<bool>,
}

fn make(code: &str, message: String) -> error::T {
    error::make(GUARD_OPERATION.to_owned(), code.to_owned(), message, None).unwrap_or_else(|bad| bad)
}

/// A plain OCaml string error.
fn fail(message: &str) -> error::T {
    make("invalid_geometry", message.to_owned())
}

fn failf(message: String) -> error::T {
    make("invalid_geometry", message)
}

fn cancelled_error() -> error::T {
    make("cancelled", "geometry operation was cancelled".to_owned())
}

/// Every error leaves `Error.guard` as the message under this operation, or as a cancellation.
fn lift(value: error::T) -> error::T {
    if error::code(&value) == "cancelled" {
        cancelled_error()
    } else {
        make("invalid_geometry", error::message(&value).to_owned())
    }
}

fn max_abs3(x: f64, y: f64, z: f64) -> f64 {
    let (x, y, z) = (x.abs(), y.abs(), z.abs());
    if x >= y {
        if x >= z { x } else { z }
    } else if y >= z {
        y
    } else {
        z
    }
}

fn max_abs9(ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64, cx: f64, cy: f64, cz: f64) -> f64 {
    let a = max_abs3(ax, ay, az);
    let b = max_abs3(bx, by, bz);
    let c = max_abs3(cx, cy, cz);
    if a >= b {
        if a >= c { a } else { c }
    } else if b >= c {
        b
    } else {
        c
    }
}

fn poll(cancel_value: Option<&cancel::T>, index: i64, mask: i64) -> Result<(), error::T> {
    cancel::poll(cancel_value, index, mask)
}

fn remap_point_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    item: &attribute::T,
) -> Result<attribute::T, error::T> {
    if attribute::owner(item) != attribute::Owner::Point {
        Ok(item.clone())
    } else {
        topology_remap::attribute(cancel_value, grain, None, point_map, item)
    }
}

fn remap_point_group(cancel_value: Option<&cancel::T>, grain: i64, point_map: &[i64], item: &group::T) -> Result<group::T, error::T> {
    if group::owner(item) != group::Owner::Point {
        Ok(item.clone())
    } else {
        topology_remap::group(cancel_value, grain, point_map, item)
    }
}

fn remap_vertex_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    vertex_map: &[i64],
    item: &attribute::T,
) -> Result<attribute::T, error::T> {
    if attribute::owner(item) != attribute::Owner::Vertex {
        Ok(item.clone())
    } else {
        topology_remap::attribute(cancel_value, grain, None, vertex_map, item)
    }
}

fn remap_vertex_group(cancel_value: Option<&cancel::T>, grain: i64, vertex_map: &[i64], item: &group::T) -> Result<group::T, error::T> {
    if group::owner(item) != group::Owner::Vertex {
        Ok(item.clone())
    } else {
        topology_remap::group(cancel_value, grain, vertex_map, item)
    }
}

fn remap_point_attributes(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    geometry_value: &geometry::T,
) -> Result<Vec<attribute::T>, error::T> {
    geometry::attributes(geometry_value)
        .iter()
        .map(|item| remap_point_attribute(cancel_value, grain, point_map, item))
        .collect()
}

fn remap_point_groups(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    geometry_value: &geometry::T,
) -> Result<Vec<group::T>, error::T> {
    geometry::groups(geometry_value)
        .iter()
        .map(|item| remap_point_group(cancel_value, grain, point_map, item))
        .collect()
}

fn select_positions(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    point_map: &[i64],
    geometry_value: &geometry::T,
) -> Result<packed::Float3::T, error::T> {
    let source = packed::Float3::View::view(geometry::positions(geometry_value))?;
    packed::Float3::View::of_owned_exn(
        topology_remap::select_float(cancel_value, grain, None, point_map, &source.x)?,
        topology_remap::select_float(cancel_value, grain, None, point_map, &source.y)?,
        topology_remap::select_float(cancel_value, grain, None, point_map, &source.z)?,
    )
}

fn remap_edge_groups_from_index(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    source_index: &topology_index::T,
    target_topology: &topology::T,
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let target_index = topology_index::create(cancel_value, target_topology)?;
    let source = topology_index::View::view(source_index);
    let target = topology_index::View::view(&target_index);
    let cancelled = AtomicBool::new(false);
    let mut output = Vec::with_capacity(groups.len());
    for item in groups {
        output.push(edge_group::init(Some(grain), target_topology, &target_index, edge_group::name(item), &|edge| {
            if cancel::poll(cancel_value, edge, 4095).is_err() {
                cancelled.store(true, Ordering::Relaxed);
            }
            let first = target.edge_offsets[edge as usize];
            let target_vertex = target.edge_vertices[first as usize];
            let source_edge = source.edge_of_vertex[target_vertex as usize];
            source_edge >= 0 && edge_group::mem(source_edge, item)
        })?);
    }
    if cancelled.load(Ordering::Relaxed) {
        return Err(cancelled_error());
    }
    Ok(output)
}

fn remap_edge_groups(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    source_topology: &topology::T,
    target_topology: &topology::T,
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let source_index = topology_index::create(cancel_value, source_topology)?;
    remap_edge_groups_from_index(cancel_value, grain, &source_index, target_topology, groups)
}

fn remap_healed_edge_groups(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    source_index: &topology_index::T,
    target_topology: &topology::T,
    vertex_map: &[i64],
    groups: &[edge_group::T],
) -> Result<Vec<edge_group::T>, error::T> {
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let target_index = topology_index::create(cancel_value, target_topology)?;
    let source = topology_index::View::view(source_index);
    let target = topology_index::View::view(&target_index);
    let cancelled = AtomicBool::new(false);
    let mut output = Vec::with_capacity(groups.len());
    for item in groups {
        output.push(edge_group::init(Some(grain), target_topology, &target_index, edge_group::name(item), &|edge| {
            if cancel::poll(cancel_value, edge, 4095).is_err() {
                cancelled.store(true, Ordering::Relaxed);
            }
            let mut found = false;
            let mut incidence = target.edge_offsets[edge as usize];
            let incidence_last = target.edge_offsets[edge as usize + 1];
            while !found && incidence < incidence_last {
                let target_vertex = target.edge_vertices[incidence as usize];
                let source_first = vertex_map[target_vertex as usize];
                let source_last = vertex_map[target.next_vertex[target_vertex as usize] as usize];
                let mut source_vertex = source_first;
                let mut traversed = 0_usize;
                while !found && source_vertex != source_last && traversed <= source.next_vertex.len() {
                    let source_edge = source.edge_of_vertex[source_vertex as usize];
                    if source_edge >= 0 && edge_group::mem(source_edge, item) {
                        found = true;
                    }
                    source_vertex = source.next_vertex[source_vertex as usize];
                    traversed += 1;
                }
                incidence += 1;
            }
            found
        })?);
    }
    if cancelled.load(Ordering::Relaxed) {
        return Err(cancelled_error());
    }
    Ok(output)
}

fn selection_error(primitives: Option<&group::T>, primitive_count: i64) -> Option<&'static str> {
    match primitives {
        Some(item) if group::owner(item) != group::Owner::Primitive => Some("Facet selection must own primitives"),
        Some(item) if group::length(item) != primitive_count => {
            Some("Facet selection length does not match primitive count")
        }
        _ => None,
    }
}

fn is_selected(primitives: Option<&group::T>, primitive: i64) -> bool {
    match primitives {
        None => true,
        Some(item) => group::mem(primitive, item),
    }
}

/// Collinearity of one corner: 0 keep, 1 remove, 2 non-finite input.
fn inline_corner(
    tolerance: f64,
    positions: &packed::Float3::View::View,
    topology_view: &topology::View::View,
    previous: i64,
    next: i64,
    vertex: i64,
) -> i32 {
    let previous_point = topology_view.vertex_points[previous as usize] as usize;
    let point = topology_view.vertex_points[vertex as usize] as usize;
    let next_point = topology_view.vertex_points[next as usize] as usize;
    let (ax, ay, az) = (positions.x[previous_point], positions.y[previous_point], positions.z[previous_point]);
    let (bx, by, bz) = (positions.x[point], positions.y[point], positions.z[point]);
    let (cx, cy, cz) = (positions.x[next_point], positions.y[next_point], positions.z[next_point]);
    if !(ax.is_finite()
        && ay.is_finite()
        && az.is_finite()
        && bx.is_finite()
        && by.is_finite()
        && bz.is_finite()
        && cx.is_finite()
        && cy.is_finite()
        && cz.is_finite())
    {
        return 2;
    }
    let scale = max_abs9(ax, ay, az, bx, by, bz, cx, cy, cz);
    if scale == 0.0 {
        return 1;
    }
    let (ax, ay, az) = (ax / scale, ay / scale, az / scale);
    let (bx, by, bz) = (bx / scale, by / scale, bz / scale);
    let (cx, cy, cz) = (cx / scale, cy / scale, cz / scale);
    let (dx, dy, dz) = (cx - ax, cy - ay, cz - az);
    let (vx, vy, vz) = (bx - ax, by - ay, bz - az);
    let difference_scale = {
        let d = max_abs3(dx, dy, dz);
        let v = max_abs3(vx, vy, vz);
        if d >= v { d } else { v }
    };
    let denominator = (dx * dx) + (dy * dy) + (dz * dz);
    let parameter = if denominator == 0.0 {
        0.0
    } else {
        let value = ((vx * dx) + (vy * dy) + (vz * dz)) / denominator;
        if value <= 0.0 {
            0.0
        } else if value >= 1.0 {
            1.0
        } else {
            value
        }
    };
    let rx = vx - (parameter * dx);
    let ry = vy - (parameter * dy);
    let rz = vz - (parameter * dz);
    let scaled_tolerance = (tolerance / scale) + (64.0 * f64::EPSILON * difference_scale);
    if (rx * rx) + (ry * ry) + (rz * rz) <= scaled_tolerance * scaled_tolerance { 1 } else { 0 }
}

/// Retained corner count, per-corner keep flags and failure flag of one primitive.
#[derive(Clone, Default)]
struct InlineResult {
    retained: i64,
    keep: Vec<bool>,
    failure: bool,
}

#[allow(clippy::too_many_arguments)]
fn inline_primitive(
    distance: f64,
    positions: &packed::Float3::View::View,
    topology_view: &topology::View::View,
    primitives: Option<&group::T>,
    primitive: i64,
) -> InlineResult {
    let first = topology_view.primitive_offsets[primitive as usize];
    let last = topology_view.primitive_offsets[primitive as usize + 1];
    let count = (last - first) as usize;
    if !is_selected(primitives, primitive) || topology_view.primitive_kinds[primitive as usize] != 0 || count <= 3 {
        return InlineResult { retained: count as i64, keep: vec![true; count], failure: false };
    }
    let mut keep = vec![true; count];
    let mut queued = vec![true; count];
    let mut previous: Vec<usize> = (0..count).map(|local| (local + count - 1) % count).collect();
    let mut next: Vec<usize> = (0..count).map(|local| (local + 1) % count).collect();
    let mut queue: Vec<usize> = (0..count).collect();
    let (mut head, mut tail) = (0_usize, 0_usize);
    let mut queued_count = count;
    let mut active = count;
    let mut failure = false;
    while queued_count > 0 && active > 3 {
        let local = queue[head];
        head = (head + 1) % count;
        queued_count -= 1;
        queued[local] = false;
        if keep[local] {
            match inline_corner(
                distance,
                positions,
                topology_view,
                first + previous[local] as i64,
                first + next[local] as i64,
                first + local as i64,
            ) {
                2 => failure = true,
                1 => {
                    let left = previous[local];
                    let right = next[local];
                    keep[local] = false;
                    active -= 1;
                    next[left] = right;
                    previous[right] = left;
                    if keep[left] && !queued[left] {
                        queue[tail] = left;
                        tail = (tail + 1) % count;
                        queued_count += 1;
                        queued[left] = true;
                    }
                    if keep[right] && !queued[right] {
                        queue[tail] = right;
                        tail = (tail + 1) % count;
                        queued_count += 1;
                        queued[right] = true;
                    }
                }
                _ => {}
            }
        }
    }
    InlineResult { retained: active as i64, keep, failure }
}

fn remove_inline_points(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    distance: f64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if !distance.is_finite() || distance < 0.0 {
        return Err(fail("Facet inline distance must be finite and non-negative"));
    }
    cancel::check_opt(cancel_value)?;
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let primitive_count = view.primitive_offsets.len() as i64 - 1;
    let vertex_count = view.vertex_points.len() as i64;
    if let Some(message) = selection_error(primitives, primitive_count) {
        return Err(fail(message));
    }
    let chunk = (grain / 8).max(1);
    let mut results = vec![InlineResult::default(); primitive_count.max(0) as usize];
    let windows = support::Windows::even(&mut results, chunk);
    support::run_indexed_ranges(chunk, primitive_count, &|range, first, last| {
        for (primitive, out) in (first..last).zip(windows.take(range)) {
            cancel::poll(cancel_value, primitive, 1023)?;
            *out = inline_primitive(distance, &positions, &view, primitives, primitive);
        }
        Ok(())
    })?;
    if let Some(invalid) = results.iter().position(|result| result.failure) {
        return Err(failf(format!(
            "Facet Remove Inline Points encountered a non-finite position in primitive {invalid}"
        )));
    }
    let output_vertices: i64 = results.iter().map(|result| result.retained).sum();
    if output_vertices == vertex_count {
        return Ok(geometry_value.clone());
    }
    let mut primitive_offsets = vec![0_i64; primitive_count as usize + 1];
    for primitive in 0..primitive_count as usize {
        primitive_offsets[primitive + 1] = primitive_offsets[primitive] + results[primitive].retained;
    }
    let mut vertex_map: Vec<i64> = Vec::with_capacity(output_vertices as usize);
    for primitive in 0..primitive_count as usize {
        let first = view.primitive_offsets[primitive];
        for (local, &kept) in results[primitive].keep.iter().enumerate() {
            if kept {
                vertex_map.push(first + local as i64);
            }
        }
    }
    let source_points = view.point_count;
    let mut source_used = vec![false; source_points as usize];
    let mut target_used = vec![false; source_points as usize];
    for vertex in 0..vertex_count as usize {
        source_used[view.vertex_points[vertex] as usize] = true;
    }
    for &vertex in &vertex_map {
        target_used[view.vertex_points[vertex as usize] as usize] = true;
    }
    let mut point_map: Vec<i64> = Vec::new();
    let mut old_to_new = vec![-1_i64; source_points as usize];
    for point in 0..source_points as usize {
        if target_used[point] || !source_used[point] {
            old_to_new[point] = point_map.len() as i64;
            point_map.push(point as i64);
        }
    }
    let output_points = point_map.len() as i64;
    let vertex_points: Vec<i64> = vertex_map.iter().map(|&vertex| old_to_new[view.vertex_points[vertex as usize] as usize]).collect();
    let target_topology = topology::View::create_validated_owned(
        output_points,
        vertex_points,
        primitive_offsets,
        view.primitive_kinds.to_vec(),
    );
    let point_identity = output_points == source_points;
    let new_positions =
        if point_identity { geometry::positions(geometry_value).clone() } else { select_positions(cancel_value, grain, &point_map, geometry_value)? };
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        let item = if point_identity { item } else { remap_point_attribute(cancel_value, grain, &point_map, &item)? };
        attributes.push(remap_vertex_attribute(cancel_value, grain, &vertex_map, &item)?);
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        let item = if point_identity { item } else { remap_point_group(cancel_value, grain, &point_map, &item)? };
        groups.push(remap_vertex_group(cancel_value, grain, &vertex_map, &item)?);
    }
    let source_edge_groups = geometry::edge_groups(geometry_value);
    let edge_groups = if source_edge_groups.is_empty() {
        Vec::new()
    } else {
        let source_index = topology_index::create(cancel_value, source_topology)?;
        remap_healed_edge_groups(cancel_value, grain, &source_index, &target_topology, &vertex_map, &source_edge_groups)?
    };
    geometry::create(new_positions, target_topology, Some(attributes), Some(groups), Some(edge_groups))
}

fn unique_points_all(cancel_value: Option<&cancel::T>, grain: i64, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let index = topology_index::create(cancel_value, source_topology)?;
    let incidence = topology_index::View::view(&index);
    let source_points = view.point_count;
    let source_vertices = view.vertex_points.len() as i64;
    let mut shared = false;
    let mut point = 0_i64;
    while !shared && point < source_points {
        poll(cancel_value, point, 4095)?;
        if incidence.point_offsets[point as usize + 1] - incidence.point_offsets[point as usize] > 1 {
            shared = true;
        }
        point += 1;
    }
    if !shared {
        return Ok(geometry_value.clone());
    }
    let mut isolated: Vec<i64> = Vec::new();
    for point in 0..source_points {
        poll(cancel_value, point, 4095)?;
        if incidence.point_offsets[point as usize] == incidence.point_offsets[point as usize + 1] {
            isolated.push(point);
        }
    }
    if isolated.len() as i64 > MAX_INT - source_vertices {
        return Err(fail("Facet Unique Points output exceeds integer cardinality limits"));
    }
    let output_points = source_vertices + isolated.len() as i64;
    let mut point_map: Vec<i64> = view.vertex_points.to_vec();
    point_map.extend_from_slice(&isolated);
    let target_topology = topology::View::create_validated_owned(
        output_points,
        (0..source_vertices).collect(),
        view.primitive_offsets.to_vec(),
        view.primitive_kinds.to_vec(),
    );
    let positions = select_positions(cancel_value, grain, &point_map, geometry_value)?;
    let attributes = remap_point_attributes(cancel_value, grain, &point_map, geometry_value)?;
    let groups = remap_point_groups(cancel_value, grain, &point_map, geometry_value)?;
    let edge_groups = remap_edge_groups_from_index(
        cancel_value,
        grain,
        &index,
        &target_topology,
        &geometry::edge_groups(geometry_value),
    )?;
    geometry::create(positions, target_topology, Some(attributes), Some(groups), Some(edge_groups))
}

fn unique_points_selected(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: &group::T,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let primitive_count = topology::primitive_count(source_topology);
    if group::owner(primitives) != group::Owner::Primitive {
        return Err(fail("Facet selection must own primitives"));
    }
    if group::length(primitives) != primitive_count {
        return Err(fail("Facet selection length does not match primitive count"));
    }
    let selected_count = group::cardinality(primitives);
    if selected_count == 0 {
        return Ok(geometry_value.clone());
    }
    if selected_count == primitive_count {
        return unique_points_all(cancel_value, grain, geometry_value);
    }
    let source_points = view.point_count;
    let source_vertices = view.vertex_points.len();
    let mut incidence_count = vec![0_u8; source_points as usize];
    let mut selected_reference = vec![false; source_points as usize];
    let mut unselected_reference = vec![false; source_points as usize];
    for primitive in 0..primitive_count {
        poll(cancel_value, primitive, 1023)?;
        let selected = group::mem(primitive, primitives);
        for vertex in view.primitive_offsets[primitive as usize]..view.primitive_offsets[primitive as usize + 1] {
            let point = view.vertex_points[vertex as usize] as usize;
            if incidence_count[point] < 2 {
                incidence_count[point] += 1;
            }
            if selected {
                selected_reference[point] = true;
            } else {
                unselected_reference[point] = true;
            }
        }
    }
    let mut selected_shared = false;
    let mut point = 0_i64;
    while !selected_shared && point < source_points {
        poll(cancel_value, point, 4095)?;
        if selected_reference[point as usize] && incidence_count[point as usize] > 1 {
            selected_shared = true;
        }
        point += 1;
    }
    if !selected_shared {
        return Ok(geometry_value.clone());
    }
    let mut selected_offsets = vec![0_i64; primitive_count as usize + 1];
    for primitive in 0..primitive_count as usize {
        let selected_vertices = if group::mem(primitive as i64, primitives) {
            view.primitive_offsets[primitive + 1] - view.primitive_offsets[primitive]
        } else {
            0
        };
        selected_offsets[primitive + 1] = selected_offsets[primitive] + selected_vertices;
    }
    let mut retained = vec![false; source_points as usize];
    let mut retained_count = 0_i64;
    for source_point in 0..source_points {
        poll(cancel_value, source_point, 4095)?;
        if incidence_count[source_point as usize] == 0 || unselected_reference[source_point as usize] {
            retained[source_point as usize] = true;
            retained_count += 1;
        }
    }
    let selected_vertices = selected_offsets[primitive_count as usize];
    if retained_count > MAX_INT - selected_vertices {
        return Err(fail("Facet Unique Points output exceeds integer cardinality limits"));
    }
    let output_points = retained_count + selected_vertices;
    let mut point_map = vec![0_i64; output_points as usize];
    let mut old_to_retained = vec![-1_i64; source_points as usize];
    let mut output = 0_usize;
    for source_point in 0..source_points as usize {
        if retained[source_point] {
            point_map[output] = source_point as i64;
            old_to_retained[source_point] = output as i64;
            output += 1;
        }
    }
    let mut vertex_points = vec![0_i64; source_vertices];
    for primitive in 0..primitive_count {
        poll(cancel_value, primitive, 1023)?;
        let first = view.primitive_offsets[primitive as usize];
        let last = view.primitive_offsets[primitive as usize + 1];
        if group::mem(primitive, primitives) {
            let target = retained_count + selected_offsets[primitive as usize];
            for vertex in first..last {
                let point = target + vertex - first;
                point_map[point as usize] = view.vertex_points[vertex as usize];
                vertex_points[vertex as usize] = point;
            }
        } else {
            for vertex in first..last {
                vertex_points[vertex as usize] = old_to_retained[view.vertex_points[vertex as usize] as usize];
            }
        }
    }
    let target_topology = topology::View::create_validated_owned(
        output_points,
        vertex_points,
        view.primitive_offsets.to_vec(),
        view.primitive_kinds.to_vec(),
    );
    let positions = select_positions(cancel_value, grain, &point_map, geometry_value)?;
    let attributes = remap_point_attributes(cancel_value, grain, &point_map, geometry_value)?;
    let groups = remap_point_groups(cancel_value, grain, &point_map, geometry_value)?;
    let edge_groups =
        remap_edge_groups(cancel_value, grain, source_topology, &target_topology, &geometry::edge_groups(geometry_value))?;
    geometry::create(positions, target_topology, Some(attributes), Some(groups), Some(edge_groups))
}

fn unique_points(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    match primitives {
        None => unique_points_all(cancel_value, grain, geometry_value),
        Some(item) => unique_points_selected(cancel_value, grain, item, geometry_value),
    }
}

fn orient_polygons(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let index = topology_index::create(cancel_value, source_topology)?;
    let index_view = topology_index::View::view(&index);
    let primitive_count = view.primitive_offsets.len() as i64 - 1;
    if let Some(message) = selection_error(primitives, primitive_count) {
        return Err(fail(message));
    }
    let is_polygon = |primitive: i64| view.primitive_kinds[primitive as usize] == 0;
    // 255: unassigned, 0: keep, 1: flip.
    let mut orientation = vec![255_u8; primitive_count as usize];
    let mut queue = vec![0_i64; primitive_count as usize];
    let mut invalid_edge = -1_i64;
    let mut contradiction = -1_i64;
    let mut primitive = 0_i64;
    while primitive < primitive_count && invalid_edge < 0 && contradiction < 0 {
        poll(cancel_value, primitive, 1023)?;
        if orientation[primitive as usize] == 255 {
            if !is_selected(primitives, primitive) || !is_polygon(primitive) {
                orientation[primitive as usize] = 0;
            } else {
                let (mut queue_first, mut queue_last) = (0_usize, 0_usize);
                orientation[primitive as usize] = 0;
                queue[queue_last] = primitive;
                queue_last += 1;
                while queue_first < queue_last && invalid_edge < 0 && contradiction < 0 {
                    let current = queue[queue_first];
                    queue_first += 1;
                    let current_flipped = orientation[current as usize] == 1;
                    let first = view.primitive_offsets[current as usize];
                    let last = view.primitive_offsets[current as usize + 1];
                    for vertex in first..last {
                        let edge = index_view.edge_of_vertex[vertex as usize];
                        if edge >= 0 && invalid_edge < 0 && contradiction < 0 {
                            let edge_first = index_view.edge_offsets[edge as usize];
                            let edge_last = index_view.edge_offsets[edge as usize + 1];
                            let mut incidence = 0;
                            let mut other_vertex = -1_i64;
                            for at in edge_first..edge_last {
                                let candidate_vertex = index_view.edge_vertices[at as usize];
                                let candidate = index_view.primitive_of_vertex[candidate_vertex as usize];
                                if is_selected(primitives, candidate) {
                                    incidence += 1;
                                    if candidate != current {
                                        other_vertex = candidate_vertex;
                                    }
                                }
                            }
                            if incidence > 2 {
                                invalid_edge = edge;
                            } else if incidence == 2 && other_vertex >= 0 {
                                let other = index_view.primitive_of_vertex[other_vertex as usize];
                                if other != current && is_polygon(other) {
                                    let same_direction =
                                        view.vertex_points[vertex as usize] == view.vertex_points[other_vertex as usize];
                                    let other_flipped = current_flipped != same_direction;
                                    let expected = if other_flipped { 1_u8 } else { 0_u8 };
                                    match orientation[other as usize] {
                                        255 => {
                                            orientation[other as usize] = expected;
                                            queue[queue_last] = other;
                                            queue_last += 1;
                                        }
                                        assigned if assigned != expected => contradiction = edge,
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        primitive += 1;
    }
    if invalid_edge >= 0 {
        return Err(failf(format!(
            "Facet Orient Polygons requires manifold edges; edge {invalid_edge} has more than two incident primitives"
        )));
    }
    if contradiction >= 0 {
        return Err(failf(format!("Facet Orient Polygons found a non-orientable constraint at edge {contradiction}")));
    }
    if !orientation.iter().any(|&value| value == 1) {
        return Ok(geometry_value.clone());
    }
    let vertex_count = view.vertex_points.len();
    let mut vertex_points = vec![0_i64; vertex_count];
    let mut vertex_map = vec![0_i64; vertex_count];
    for primitive in 0..primitive_count as usize {
        poll(cancel_value, primitive as i64, 1023)?;
        let first = view.primitive_offsets[primitive];
        let last = view.primitive_offsets[primitive + 1];
        for vertex in first..last {
            let source = if orientation[primitive] == 1 { first + last - vertex - 1 } else { vertex };
            vertex_points[vertex as usize] = view.vertex_points[source as usize];
            vertex_map[vertex as usize] = source;
        }
    }
    let target_topology = topology::View::create_validated_owned(
        view.point_count,
        vertex_points,
        view.primitive_offsets.to_vec(),
        view.primitive_kinds.to_vec(),
    );
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        attributes.push(remap_vertex_attribute(cancel_value, grain, &vertex_map, &item)?);
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        groups.push(remap_vertex_group(cancel_value, grain, &vertex_map, &item)?);
    }
    let source_groups = geometry::edge_groups(geometry_value);
    let mut edge_groups = Vec::new();
    if !source_groups.is_empty() {
        let target_index = topology_index::create(cancel_value, &target_topology)?;
        let point_map: Vec<i64> = (0..view.point_count).collect();
        for item in &source_groups {
            edge_groups.push(edge_group::remap(cancel_value, &index, &target_topology, &target_index, &point_map, item)?);
        }
    }
    geometry::create(
        geometry::positions(geometry_value).clone(),
        target_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

struct UnionFind {
    parent: Vec<i64>,
    rank: Vec<u8>,
}

impl UnionFind {
    fn root(&mut self, vertex: i64) -> i64 {
        let mut representative = vertex;
        while self.parent[representative as usize] != representative {
            representative = self.parent[representative as usize];
        }
        let mut current = vertex;
        while self.parent[current as usize] != representative {
            let next = self.parent[current as usize];
            self.parent[current as usize] = representative;
            current = next;
        }
        representative
    }

    fn union(&mut self, left: i64, right: i64) {
        let left = self.root(left);
        let right = self.root(right);
        if left != right {
            let left_rank = self.rank[left as usize];
            let right_rank = self.rank[right as usize];
            if left_rank < right_rank {
                self.parent[left as usize] = right;
            } else if right_rank < left_rank {
                self.parent[right as usize] = left;
            } else {
                let (representative, child) = if left < right { (left, right) } else { (right, left) };
                self.parent[child as usize] = representative;
                self.rank[representative as usize] = left_rank + 1;
            }
        }
    }
}

fn split_points_on_edge_ends(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    index: &topology_index::T,
    split_ends: &[u8],
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let iv = topology_index::View::view(index);
    let vertex_count = view.vertex_points.len() as i64;
    let edge_count = iv.edge_a.len() as i64;
    let mut sets = UnionFind { parent: (0..vertex_count).collect(), rank: vec![0; vertex_count as usize] };
    let mut affected = vec![false; view.point_count as usize];
    for edge in 0..edge_count as usize {
        let flags = split_ends[edge];
        if flags & 1 != 0 {
            affected[iv.edge_a[edge] as usize] = true;
        }
        if flags & 2 != 0 {
            affected[iv.edge_b[edge] as usize] = true;
        }
    }
    for point in 0..view.point_count {
        poll(cancel_value, point, 4095)?;
        let first = iv.point_offsets[point as usize];
        let last = iv.point_offsets[point as usize + 1];
        let mut representative = -1_i64;
        for at in first..last {
            let vertex = iv.point_vertices[at as usize];
            if !affected[point as usize] || !is_selected(primitives, iv.primitive_of_vertex[vertex as usize]) {
                if representative < 0 {
                    representative = vertex;
                } else {
                    sets.union(representative, vertex);
                }
            }
        }
    }
    for edge in 0..edge_count {
        poll(cancel_value, edge, 4095)?;
        let flags = split_ends[edge as usize];
        let first = iv.edge_offsets[edge as usize];
        let last = iv.edge_offsets[edge as usize + 1];
        if last > first {
            let a = iv.edge_a[edge as usize];
            let b = iv.edge_b[edge as usize];
            let mut representative_a = -1_i64;
            let mut representative_b = -1_i64;
            for at in first..last {
                let vertex = iv.edge_vertices[at as usize];
                let vertex_a = if view.vertex_points[vertex as usize] == a { vertex } else { iv.next_vertex[vertex as usize] };
                let vertex_b = if view.vertex_points[vertex as usize] == b { vertex } else { iv.next_vertex[vertex as usize] };
                if flags & 1 == 0 {
                    if representative_a < 0 {
                        representative_a = vertex_a;
                    } else {
                        sets.union(representative_a, vertex_a);
                    }
                }
                if flags & 2 == 0 {
                    if representative_b < 0 {
                        representative_b = vertex_b;
                    } else {
                        sets.union(representative_b, vertex_b);
                    }
                }
            }
        }
    }
    let source_points = view.point_count;
    let mut root_output = vec![-1_i64; vertex_count as usize];
    let mut extra_count = 0_i64;
    let mut overflow = false;
    for point in 0..source_points {
        poll(cancel_value, point, 4095)?;
        let first = iv.point_offsets[point as usize];
        let last = iv.point_offsets[point as usize + 1];
        let mut component = 0;
        if primitives.is_some() {
            let mut at = first;
            while at < last && component == 0 {
                let vertex = iv.point_vertices[at as usize];
                if !is_selected(primitives, iv.primitive_of_vertex[vertex as usize]) {
                    let representative = sets.root(vertex);
                    root_output[representative as usize] = point;
                    component = 1;
                }
                at += 1;
            }
        }
        for local in first..last {
            let representative = sets.root(iv.point_vertices[local as usize]);
            if root_output[representative as usize] < 0 {
                if component == 0 {
                    root_output[representative as usize] = point;
                } else if extra_count >= MAX_INT - source_points {
                    overflow = true;
                } else {
                    root_output[representative as usize] = source_points + extra_count;
                    extra_count += 1;
                }
                component += 1;
            }
        }
    }
    if overflow {
        return Err(fail("Facet point split exceeds integer cardinality limits"));
    }
    if extra_count == 0 {
        return Ok(geometry_value.clone());
    }
    let output_points = source_points + extra_count;
    let mut point_map: Vec<i64> = (0..output_points).map(|point| if point < source_points { point } else { 0 }).collect();
    for representative in 0..vertex_count as usize {
        let output = root_output[representative];
        if output >= source_points {
            point_map[output as usize] = view.vertex_points[representative];
        }
    }
    let mut vertex_points = vec![0_i64; vertex_count as usize];
    for vertex in 0..vertex_count {
        poll(cancel_value, vertex, 4095)?;
        let root = sets.root(vertex);
        vertex_points[vertex as usize] = root_output[root as usize];
    }
    let target_topology = topology::View::create_validated_owned(
        output_points,
        vertex_points,
        view.primitive_offsets.to_vec(),
        view.primitive_kinds.to_vec(),
    );
    let positions = select_positions(cancel_value, grain, &point_map, geometry_value)?;
    let attributes = remap_point_attributes(cancel_value, grain, &point_map, geometry_value)?;
    let groups = remap_point_groups(cancel_value, grain, &point_map, geometry_value)?;
    let edge_groups =
        remap_edge_groups_from_index(cancel_value, grain, index, &target_topology, &geometry::edge_groups(geometry_value))?;
    geometry::create(positions, target_topology, Some(attributes), Some(groups), Some(edge_groups))
}

fn cusp_polygons(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    angle: f64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if !angle.is_finite() || !(0.0..=std::f64::consts::PI).contains(&angle) {
        return Err(fail("Facet cusp angle must be finite and within [0, pi]"));
    }
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let primitive_count = view.primitive_offsets.len() as i64 - 1;
    if let Some(message) = selection_error(primitives, primitive_count) {
        return Err(fail(message));
    }
    let is_polygon = |primitive: i64| view.primitive_kinds[primitive as usize] == 0;
    let mut non_polygon = -1_i64;
    let mut primitive = 0_i64;
    while non_polygon < 0 && primitive < primitive_count {
        poll(cancel_value, primitive, 1023)?;
        if is_selected(primitives, primitive) && !is_polygon(primitive) {
            non_polygon = primitive;
        }
        primitive += 1;
    }
    if non_polygon >= 0 {
        return Err(failf(format!(
            "Facet Cusp Polygons requires polygon-only topology; primitive {non_polygon} is not a polygon"
        )));
    }
    let index = topology_index::create(cancel_value, source_topology)?;
    let iv = topology_index::View::view(&index);
    let normal_primitives = match primitives {
        None => None,
        Some(_) => Some(group::init(
            Some(grain),
            group::Owner::Primitive,
            "__facet_cusp_normals",
            primitive_count,
            &|primitive| {
                if is_selected(primitives, primitive) {
                    true
                } else if !is_polygon(primitive) {
                    false
                } else {
                    let first = view.primitive_offsets[primitive as usize];
                    let last = view.primitive_offsets[primitive as usize + 1];
                    let mut found = false;
                    let mut vertex = first;
                    while !found && vertex < last {
                        let edge = iv.edge_of_vertex[vertex as usize];
                        if edge >= 0 {
                            let mut at = iv.edge_offsets[edge as usize];
                            let finish = iv.edge_offsets[edge as usize + 1];
                            while !found && at < finish {
                                found = is_selected(primitives, iv.primitive_of_vertex[iv.edge_vertices[at as usize] as usize]);
                                at += 1;
                            }
                        }
                        vertex += 1;
                    }
                    found
                }
            },
        )?),
    };
    let (nx, ny, nz) = face_normals::compute(
        cancel_value,
        Some(grain),
        normal_primitives.as_ref(),
        "Facet Cusp Polygons",
        geometry_value,
    )?;
    let edge_count = iv.edge_a.len() as i64;
    let threshold = angle.cos();
    let epsilon = 64.0 * f64::EPSILON;
    let mut cusp = vec![0_u8; edge_count.max(0) as usize];
    let windows = support::Windows::even(&mut cusp, grain);
    support::run_indexed_ranges(grain, edge_count, &|range, first_edge, last_edge| {
        for (edge, out) in (first_edge..last_edge).zip(windows.take(range)) {
            cancel::poll(cancel_value, edge, 4095)?;
            let first = iv.edge_offsets[edge as usize];
            let last = iv.edge_offsets[edge as usize + 1];
            let mut touches_selection = false;
            for at in first..last {
                let primitive = iv.primitive_of_vertex[iv.edge_vertices[at as usize] as usize];
                if is_selected(primitives, primitive) {
                    touches_selection = true;
                }
            }
            let mut value = 0_u8;
            if touches_selection && last - first > 2 {
                value = 2;
            } else if touches_selection && last - first == 2 {
                let left = iv.primitive_of_vertex[iv.edge_vertices[first as usize] as usize];
                let right = iv.primitive_of_vertex[iv.edge_vertices[first as usize + 1] as usize];
                if is_polygon(left) && is_polygon(right) {
                    let (l, r) = (left as usize, right as usize);
                    let dot = (nx[l] * nx[r]) + (ny[l] * ny[r]) + (nz[l] * nz[r]);
                    if dot < threshold - epsilon {
                        value = 1;
                    }
                }
            }
            *out = value;
        }
        Ok(())
    })?;
    if let Some(invalid_edge) = cusp.iter().position(|&value| value == 2) {
        return Err(failf(format!(
            "Facet Cusp Polygons requires manifold edges; edge {invalid_edge} has more than two incident primitives"
        )));
    }
    let split_ends: Vec<u8> = cusp.iter().map(|&value| if value == 1 { 3 } else { 0 }).collect();
    split_points_on_edge_ends(cancel_value, grain, primitives, &index, &split_ends, geometry_value)
}

fn planar_local(values: &[f64], point: usize, origin: f64, scale: f64, relative: bool) -> f64 {
    if relative { (values[point] - origin) / scale } else { values[point] / scale }
}

fn planar_world(value: f64, origin: f64, scale: f64, relative: bool) -> f64 {
    if relative { origin + (value * scale) } else { value * scale }
}

#[derive(Clone, Default)]
struct PlanarResult {
    failure: u8,
    changed: bool,
    projected: Vec<(f64, f64, f64)>,
}

fn planar_primitive(
    positions: &packed::Float3::View::View,
    topology_view: &topology::View::View,
    primitives: Option<&group::T>,
    primitive: i64,
) -> PlanarResult {
    let first = topology_view.primitive_offsets[primitive as usize];
    let last = topology_view.primitive_offsets[primitive as usize + 1];
    let count = last - first;
    let mut result = PlanarResult { failure: 0, changed: false, projected: vec![(0.0, 0.0, 0.0); count as usize] };
    if !(is_selected(primitives, primitive) && topology_view.primitive_kinds[primitive as usize] == 0 && count > 3) {
        return result;
    }
    let point_of = |vertex: i64| topology_view.vertex_points[vertex as usize] as usize;
    let first_point = point_of(first);
    let (ox, oy, oz) = (positions.x[first_point], positions.y[first_point], positions.z[first_point]);
    if !(ox.is_finite() && oy.is_finite() && oz.is_finite()) {
        result.failure = 1;
        return result;
    }
    let mut scale = 0.0_f64;
    let mut relative = true;
    for vertex in first + 1..last {
        let point = point_of(vertex);
        let (px, py, pz) = (positions.x[point], positions.y[point], positions.z[point]);
        if !(px.is_finite() && py.is_finite() && pz.is_finite()) {
            result.failure = 1;
        } else {
            let (dx, dy, dz) = (px - ox, py - oy, pz - oz);
            if !(dx.is_finite() && dy.is_finite() && dz.is_finite()) {
                relative = false;
            } else {
                let local_scale = max_abs3(dx, dy, dz);
                if local_scale > scale {
                    scale = local_scale;
                }
            }
        }
    }
    if result.failure != 0 {
        return result;
    }
    if !relative {
        scale = max_abs3(ox, oy, oz);
        for vertex in first + 1..last {
            let point = point_of(vertex);
            let value = max_abs3(positions.x[point], positions.y[point], positions.z[point]);
            if value > scale {
                scale = value;
            }
        }
    }
    if scale == 0.0 {
        result.failure = 2;
        return result;
    }
    let local = |vertex: i64| {
        let point = point_of(vertex);
        (
            planar_local(&positions.x, point, ox, scale, relative),
            planar_local(&positions.y, point, oy, scale, relative),
            planar_local(&positions.z, point, oz, scale, relative),
        )
    };
    let (mut cx, mut cy, mut cz) = (0.0_f64, 0.0_f64, 0.0_f64);
    for vertex in first..last {
        let (x, y, z) = local(vertex);
        cx += x;
        cy += y;
        cz += z;
    }
    let divisor = count as f64;
    cx /= divisor;
    cy /= divisor;
    cz /= divisor;
    let (mut nx, mut ny, mut nz) = (0.0_f64, 0.0_f64, 0.0_f64);
    for vertex in first..last {
        let next = if vertex + 1 < last { vertex + 1 } else { first };
        let (ax, ay, az) = local(vertex);
        let (bx, by, bz) = local(next);
        nx += (ay - by) * (az + bz);
        ny += (az - bz) * (ax + bx);
        nz += (ax - bx) * (ay + by);
    }
    let normal_scale = max_abs3(nx, ny, nz);
    if normal_scale == 0.0 || !(nx.is_finite() && ny.is_finite() && nz.is_finite()) {
        result.failure = 2;
        return result;
    }
    let (nx, ny, nz) = (nx / normal_scale, ny / normal_scale, nz / normal_scale);
    let inverse = 1.0 / ((nx * nx) + (ny * ny) + (nz * nz)).sqrt();
    let (nx, ny, nz) = (nx * inverse, ny * inverse, nz * inverse);
    let mut maximum_distance = 0.0_f64;
    for vertex in first..last {
        let (x, y, z) = local(vertex);
        let distance = ((x - cx) * nx) + ((y - cy) * ny) + ((z - cz) * nz);
        let absolute = distance.abs();
        if absolute > maximum_distance {
            maximum_distance = absolute;
        }
        let wx = planar_world(x - (distance * nx), ox, scale, relative);
        let wy = planar_world(y - (distance * ny), oy, scale, relative);
        let wz = planar_world(z - (distance * nz), oz, scale, relative);
        if !(wx.is_finite() && wy.is_finite() && wz.is_finite()) {
            result.failure = 3;
        } else {
            result.projected[(vertex - first) as usize] = (wx, wy, wz);
        }
    }
    if result.failure == 0 && maximum_distance > 512.0 * f64::EPSILON {
        result.changed = true;
    }
    result
}

fn make_planar(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    cancel::check_opt(cancel_value)?;
    let source_topology = geometry::topology(geometry_value);
    let view = topology::View::view(source_topology)?;
    let primitive_count = topology::primitive_count(source_topology);
    if let Some(item) = primitives {
        if group::owner(item) != group::Owner::Primitive || group::length(item) != primitive_count {
            return Err(fail("Facet selection must be a matching primitive group"));
        }
    }
    let source_positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let chunk = (grain / 8).max(1);
    let mut results = vec![PlanarResult::default(); primitive_count.max(0) as usize];
    let windows = support::Windows::even(&mut results, chunk);
    support::run_indexed_ranges(chunk, primitive_count, &|range, first, last| {
        for (primitive, out) in (first..last).zip(windows.take(range)) {
            cancel::poll(cancel_value, primitive, 1023)?;
            *out = planar_primitive(&source_positions, &view, primitives, primitive);
        }
        Ok(())
    })?;
    if let Some(failed) = results.iter().position(|result| result.failure != 0) {
        let reason = match results[failed].failure {
            1 => "contains a non-finite position",
            2 => "has no stable plane",
            _ => "projection is outside finite coordinate range",
        };
        return Err(failf(format!("Facet Make Planar primitive {failed} {reason}")));
    }
    if !results.iter().any(|result| result.changed) {
        return Ok(geometry_value.clone());
    }
    let point_count = view.point_count;
    let mut unchanged_reference = vec![false; point_count as usize];
    for primitive in 0..primitive_count as usize {
        if !results[primitive].changed {
            for vertex in view.primitive_offsets[primitive]..view.primitive_offsets[primitive + 1] {
                unchanged_reference[view.vertex_points[vertex as usize] as usize] = true;
            }
        }
    }
    let mut claimed = vec![false; point_count as usize];
    let mut extras = 0_i64;
    let mut overflow = false;
    for primitive in 0..primitive_count as usize {
        if results[primitive].changed {
            for vertex in view.primitive_offsets[primitive]..view.primitive_offsets[primitive + 1] {
                let point = view.vertex_points[vertex as usize] as usize;
                if unchanged_reference[point] || claimed[point] {
                    if extras >= MAX_INT - point_count {
                        overflow = true;
                    } else {
                        extras += 1;
                    }
                } else {
                    claimed[point] = true;
                }
            }
        }
    }
    if overflow {
        return Err(fail("Facet Make Planar output exceeds integer cardinality limits"));
    }
    let output_points = point_count + extras;
    let mut point_map: Vec<i64> = (0..output_points).map(|point| if point < point_count { point } else { 0 }).collect();
    let mut vertex_points = view.vertex_points.to_vec();
    let mut x = vec![0.0_f64; output_points as usize];
    let mut y = vec![0.0_f64; output_points as usize];
    let mut z = vec![0.0_f64; output_points as usize];
    x[..point_count as usize].copy_from_slice(&source_positions.x);
    y[..point_count as usize].copy_from_slice(&source_positions.y);
    z[..point_count as usize].copy_from_slice(&source_positions.z);
    claimed.iter_mut().for_each(|slot| *slot = false);
    let mut extra = point_count;
    for primitive in 0..primitive_count as usize {
        poll(cancel_value, primitive as i64, 1023)?;
        if results[primitive].changed {
            let first = view.primitive_offsets[primitive];
            for vertex in first..view.primitive_offsets[primitive + 1] {
                let source_point = view.vertex_points[vertex as usize];
                let target = if !unchanged_reference[source_point as usize] && !claimed[source_point as usize] {
                    claimed[source_point as usize] = true;
                    source_point
                } else {
                    let target = extra;
                    extra += 1;
                    point_map[target as usize] = source_point;
                    target
                };
                vertex_points[vertex as usize] = target;
                let (px, py, pz) = results[primitive].projected[(vertex - first) as usize];
                x[target as usize] = px;
                y[target as usize] = py;
                z[target as usize] = pz;
            }
        }
    }
    let positions = packed::Float3::View::of_owned_exn(x, y, z)?;
    if extras == 0 {
        return geometry::with_positions(positions, geometry_value);
    }
    let target_topology = topology::View::create_validated_owned(
        output_points,
        vertex_points,
        view.primitive_offsets.to_vec(),
        view.primitive_kinds.to_vec(),
    );
    let attributes = remap_point_attributes(cancel_value, grain, &point_map, geometry_value)?;
    let groups = remap_point_groups(cancel_value, grain, &point_map, geometry_value)?;
    let edge_groups =
        remap_edge_groups(cancel_value, grain, source_topology, &target_topology, &geometry::edge_groups(geometry_value))?;
    geometry::create(positions, target_topology, Some(attributes), Some(groups), Some(edge_groups))
}

fn owner_word(owner: attribute::Owner) -> &'static str {
    if owner == attribute::Owner::Point { "point" } else { "vertex" }
}

fn validate_normal_view(owner: attribute::Owner, values: &packed::Float3::T) -> Result<packed::Float3::View::View, error::T> {
    let values = packed::Float3::View::view(values)?;
    for element in 0..values.x.len() {
        if !(values.x[element].is_finite() && values.y[element].is_finite() && values.z[element].is_finite()) {
            return Err(failf(format!(
                "Facet Consolidate Normals {} N contains a non-finite vector at element {element}",
                owner_word(owner)
            )));
        }
    }
    Ok(values)
}

fn scaled_average(sum: f64, divisor: f64, scale: f64) -> f64 {
    if scale == 0.0 {
        0.0
    } else {
        let value = sum / divisor;
        let value = if value > 1.0 {
            1.0
        } else if value < -1.0 {
            -1.0
        } else {
            value
        };
        value * scale
    }
}

fn consolidate_point_normal(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &point_clusters::Clusters,
    source: &packed::Float3::View::View,
) -> Result<packed::Float3::T, error::T> {
    let mut x = source.x.to_vec();
    let mut y = source.y.to_vec();
    let mut z = source.z.to_vec();
    let chunk = (grain / 8).max(1);
    // ponytail: per-cluster values are computed in parallel into a 1:1 buffer; the scatter into x/y/z is sequential.
    let mut averages: Vec<Option<(f64, f64, f64)>> = vec![None; clusters.count.max(0) as usize];
    let windows = support::Windows::even(&mut averages, chunk);
    support::run_indexed_ranges(chunk, clusters.count, &|range, first_cluster, last_cluster| {
        for (cluster, out) in (first_cluster..last_cluster).zip(windows.take(range)) {
            cancel::poll(cancel_value, cluster, 1023)?;
            let first = clusters.offsets[cluster as usize];
            let last = clusters.offsets[cluster as usize + 1];
            if last - first > 1 {
                let mut scale = 0.0_f64;
                for slot in first..last {
                    let point = clusters.members[slot as usize] as usize;
                    let value = max_abs3(source.x[point], source.y[point], source.z[point]);
                    if value > scale {
                        scale = value;
                    }
                }
                let (mut ax, mut ay, mut az) = (0.0_f64, 0.0_f64, 0.0_f64);
                if scale != 0.0 {
                    for slot in first..last {
                        let point = clusters.members[slot as usize] as usize;
                        ax += source.x[point] / scale;
                        ay += source.y[point] / scale;
                        az += source.z[point] / scale;
                    }
                }
                let divisor = (last - first) as f64;
                *out = Some((
                    scaled_average(ax, divisor, scale),
                    scaled_average(ay, divisor, scale),
                    scaled_average(az, divisor, scale),
                ));
            }
        }
        Ok(())
    })?;
    for (cluster, average) in averages.iter().enumerate() {
        let Some((nx, ny, nz)) = *average else { continue };
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            let point = clusters.members[slot as usize] as usize;
            x[point] = nx;
            y[point] = ny;
            z[point] = nz;
        }
    }
    packed::Float3::View::of_owned_exn(x, y, z)
}

fn consolidate_vertex_normal(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    clusters: &point_clusters::Clusters,
    incidence: &topology_index::View::View,
    source: &packed::Float3::View::View,
) -> Result<packed::Float3::T, error::T> {
    let mut x = source.x.to_vec();
    let mut y = source.y.to_vec();
    let mut z = source.z.to_vec();
    let chunk = (grain / 8).max(1);
    // ponytail: per-cluster values are computed in parallel into a 1:1 buffer; the scatter into x/y/z is sequential.
    let mut averages: Vec<Option<(f64, f64, f64)>> = vec![None; clusters.count.max(0) as usize];
    let windows = support::Windows::even(&mut averages, chunk);
    let in_selection =
        |vertex: i64| is_selected(primitives, incidence.primitive_of_vertex[vertex as usize]);
    support::run_indexed_ranges(chunk, clusters.count, &|range, first_cluster, last_cluster| {
        for (cluster, out) in (first_cluster..last_cluster).zip(windows.take(range)) {
            cancel::poll(cancel_value, cluster, 1023)?;
            let first = clusters.offsets[cluster as usize];
            let last = clusters.offsets[cluster as usize + 1];
            if last - first > 1 {
                let mut scale = 0.0_f64;
                let mut elements = 0_i64;
                for slot in first..last {
                    let point = clusters.members[slot as usize] as usize;
                    for at in incidence.point_offsets[point]..incidence.point_offsets[point + 1] {
                        let vertex = incidence.point_vertices[at as usize];
                        if in_selection(vertex) {
                            let v = vertex as usize;
                            let value = max_abs3(source.x[v], source.y[v], source.z[v]);
                            if value > scale {
                                scale = value;
                            }
                            elements += 1;
                        }
                    }
                }
                if elements > 0 {
                    let (mut ax, mut ay, mut az) = (0.0_f64, 0.0_f64, 0.0_f64);
                    if scale != 0.0 {
                        for slot in first..last {
                            let point = clusters.members[slot as usize] as usize;
                            for at in incidence.point_offsets[point]..incidence.point_offsets[point + 1] {
                                let vertex = incidence.point_vertices[at as usize];
                                if in_selection(vertex) {
                                    let v = vertex as usize;
                                    ax += source.x[v] / scale;
                                    ay += source.y[v] / scale;
                                    az += source.z[v] / scale;
                                }
                            }
                        }
                    }
                    let divisor = elements as f64;
                    *out = Some((
                        scaled_average(ax, divisor, scale),
                        scaled_average(ay, divisor, scale),
                        scaled_average(az, divisor, scale),
                    ));
                }
            }
        }
        Ok(())
    })?;
    for (cluster, average) in averages.iter().enumerate() {
        let Some((nx, ny, nz)) = *average else { continue };
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            let point = clusters.members[slot as usize] as usize;
            for at in incidence.point_offsets[point]..incidence.point_offsets[point + 1] {
                let vertex = incidence.point_vertices[at as usize];
                if in_selection(vertex) {
                    x[vertex as usize] = nx;
                    y[vertex as usize] = ny;
                    z[vertex as usize] = nz;
                }
            }
        }
    }
    packed::Float3::View::of_owned_exn(x, y, z)
}

fn is_normal_attribute(item: &attribute::T) -> bool {
    attribute::name(item) == "N"
        && (attribute::owner(item) == attribute::Owner::Point || attribute::owner(item) == attribute::Owner::Vertex)
}

fn consolidate_normals(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    distance: f64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if !distance.is_finite() || distance < 0.0 {
        return Err(fail("Facet normal consolidation distance must be finite and non-negative"));
    }
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(geometry_value);
    let primitive_count = topology::primitive_count(topology_value);
    if let Some(message) = selection_error(primitives, primitive_count) {
        return Err(fail(message));
    }
    let point_selection = match primitives {
        None => None,
        Some(selection) => {
            let index = topology_index::create(cancel_value, topology_value)?;
            let cancelled = AtomicBool::new(false);
            let built = group::init(
                Some(grain),
                group::Owner::Point,
                "__facet_points",
                topology::point_count(topology_value),
                &|point| {
                    let mut local = 0;
                    let mut found = false;
                    let count = topology_index::point_incidence_count(&index, point).unwrap_or_else(|_| {
                        cancelled.store(true, Ordering::Relaxed);
                        0
                    });
                    while !found && local < count {
                        let vertex = topology_index::point_vertex(&index, point, local).unwrap_or(0);
                        found = group::mem(topology_index::primitive_of_vertex(&index, vertex).unwrap_or(0), selection);
                        local += 1;
                    }
                    found
                },
            )?;
            if cancelled.load(Ordering::Relaxed) {
                return Err(fail("Facet normal consolidation point incidence is out of range"));
            }
            Some(built)
        }
    };
    let normals: Vec<attribute::T> = geometry::attributes(geometry_value).into_iter().filter(is_normal_attribute).collect();
    if normals.is_empty() {
        return Ok(geometry_value.clone());
    }
    let mut validated: Vec<(attribute::Owner, packed::Float3::View::View)> = Vec::new();
    for item in &normals {
        match attribute::View::storage(item)? {
            attribute::Storage::Float3(values) => {
                validated.push((attribute::owner(item), validate_normal_view(attribute::owner(item), values)?));
            }
            _ => {
                return Err(failf(format!(
                    "Facet Consolidate Normals {} N must have float3 storage",
                    owner_word(attribute::owner(item))
                )));
            }
        }
    }
    let clusters = point_clusters::create(
        cancel_value,
        point_selection.as_ref(),
        Metric::Euclidean,
        true,
        "Facet Consolidate Normals",
        distance,
        &|_, _| true,
        geometry_value,
    )?;
    let clusters = match clusters {
        point_clusters::T::Identity => return Ok(geometry_value.clone()),
        point_clusters::T::Clusters(value) => value,
    };
    let vertex_needed = validated.iter().any(|(owner, _)| *owner == attribute::Owner::Vertex);
    let incidence = if vertex_needed {
        Some(topology_index::View::view(&topology_index::create(cancel_value, topology_value)?))
    } else {
        None
    };
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        if !is_normal_attribute(&item) {
            attributes.push(item);
            continue;
        }
        let source = match attribute::View::storage(&item)? {
            attribute::Storage::Float3(values) => packed::Float3::View::view(values)?,
            _ => return Err(fail("Facet Consolidate Normals normal storage changed")),
        };
        let values = if attribute::owner(&item) == attribute::Owner::Point {
            consolidate_point_normal(cancel_value, grain, &clusters, &source)?
        } else {
            let view = incidence.as_ref().ok_or_else(|| fail("Facet Consolidate Normals lost its incidence"))?;
            consolidate_vertex_normal(cancel_value, grain, primitives, &clusters, view, &source)?
        };
        attributes.push(attribute::create_owned("N", attribute::owner(&item), attribute::Storage::Float3(values))?);
    }
    geometry::create(
        geometry::positions(geometry_value).clone(),
        topology_value.clone(),
        Some(attributes),
        Some(geometry::groups(geometry_value)),
        Some(geometry::edge_groups(geometry_value)),
    )
}

/// The adjusted attribute and whether it differs from the input.
fn adjust_normal_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    selected: Option<&[bool]>,
    unit_length: bool,
    reverse: bool,
    item: &attribute::T,
) -> Result<(attribute::T, bool), error::T> {
    let owner = attribute::owner(item);
    if !is_normal_attribute(item) {
        return Ok((item.clone(), false));
    }
    match attribute::View::storage(item)? {
        attribute::Storage::Float3(values) => {
            let values = packed::Float3::View::view(values)?;
            let count = values.x.len() as i64;
            let zeros = vec![0.0_f64; count.max(0) as usize];
            let (mut x, mut y, mut z) = (zeros.clone(), zeros.clone(), zeros);
            let (wx, wy, wz) = (
                support::Windows::even(&mut x, grain),
                support::Windows::even(&mut y, grain),
                support::Windows::even(&mut z, grain),
            );
            let failed = AtomicBool::new(false);
            support::run_indexed_ranges(grain, count, &|range, first, last| {
                let (ox, oy, oz) = (wx.take(range), wy.take(range), wz.take(range));
                for element in first..last {
                    cancel::poll(cancel_value, element, 4095)?;
                    let e = element as usize;
                    let (vx, vy, vz) = (values.x[e], values.y[e], values.z[e]);
                    let mut output = (vx, vy, vz);
                    if selected.is_none_or(|mask| mask[e]) {
                        if !(vx.is_finite() && vy.is_finite() && vz.is_finite()) {
                            failed.store(true, Ordering::Relaxed);
                        } else {
                            let sign = if reverse { -1.0 } else { 1.0 };
                            if !unit_length {
                                output = (sign * vx, sign * vy, sign * vz);
                            } else {
                                let scale = max_abs3(vx, vy, vz);
                                if scale == 0.0 {
                                    output = (0.0, 0.0, 0.0);
                                } else {
                                    let (sx, sy, sz) = (vx / scale, vy / scale, vz / scale);
                                    let length = ((sx * sx) + (sy * sy) + (sz * sz)).sqrt();
                                    output = (sign * sx / length, sign * sy / length, sign * sz / length);
                                }
                            }
                        }
                    }
                    let at = (element - first) as usize;
                    (ox[at], oy[at], oz[at]) = output;
                }
                Ok(())
            })?;
            if failed.load(Ordering::Relaxed) {
                return Err(failf(format!("{} normal attribute contains a non-finite vector", owner_word(owner))));
            }
            let created = attribute::create_owned(
                "N",
                owner,
                attribute::Storage::Float3(packed::Float3::View::of_owned_exn(x, y, z)?),
            )?;
            Ok((created, true))
        }
        _ => Err(failf(format!("{} normal attribute must have float3 storage", owner_word(owner)))),
    }
}

fn adjust_normals(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    unit_length: bool,
    reverse: bool,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if !unit_length && !reverse {
        return Ok(geometry_value.clone());
    }
    let topology_value = geometry::topology(geometry_value);
    if let Some(message) = selection_error(primitives, topology::primitive_count(topology_value)) {
        return Err(fail(message));
    }
    let (selected_points, selected_vertices): (Option<Vec<bool>>, Option<Vec<bool>>) = match primitives {
        None => (None, None),
        Some(item) => {
            let view = topology::View::view(topology_value)?;
            let mut points = vec![false; view.point_count as usize];
            let mut vertices = vec![false; view.vertex_points.len()];
            for primitive in 0..topology::primitive_count(topology_value) {
                poll(cancel_value, primitive, 1023)?;
                if group::mem(primitive, item) {
                    for vertex in view.primitive_offsets[primitive as usize]..view.primitive_offsets[primitive as usize + 1] {
                        vertices[vertex as usize] = true;
                        points[view.vertex_points[vertex as usize] as usize] = true;
                    }
                }
            }
            (Some(points), Some(vertices))
        }
    };
    let mut changed = false;
    let mut result = Vec::new();
    for item in geometry::attributes(geometry_value) {
        let mask = if attribute::owner(&item) == attribute::Owner::Vertex { &selected_vertices } else { &selected_points };
        let (adjusted, differs) =
            adjust_normal_attribute(cancel_value, grain, mask.as_deref(), unit_length, reverse, &item)?;
        changed |= differs;
        result.push(adjusted);
    }
    if !changed {
        Ok(geometry_value.clone())
    } else {
        geometry::View::with_attributes_owned(result, geometry_value)
    }
}

fn facet_point_selection(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: &group::T,
    geometry_value: &geometry::T,
) -> Result<group::T, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let index = topology_index::create(cancel_value, topology_value)?;
    let view = topology_index::View::view(&index);
    group::init(
        Some(grain),
        group::Owner::Point,
        "__facet_selected_points",
        topology::point_count(topology_value),
        &|point| {
            let mut found = false;
            let mut at = view.point_offsets[point as usize];
            let finish = view.point_offsets[point as usize + 1];
            while !found && at < finish {
                found = group::mem(view.primitive_of_vertex[view.point_vertices[at as usize] as usize], primitives);
                at += 1;
            }
            found
        },
    )
}

fn facet_primitives_of_selection(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    selection: &element_selection::T,
    geometry_value: &geometry::T,
) -> Result<group::T, error::T> {
    let topology_value = geometry::topology(geometry_value);
    cancel::check_opt(cancel_value)?;
    deform::validate_selection(topology_value, Some(selection))?;
    let view = topology::View::view(topology_value)?;
    let primitive_count = topology::primitive_count(topology_value);
    let cancelled = AtomicBool::new(false);
    let check = |primitive: i64| {
        if cancel::poll(cancel_value, primitive, 4095).is_err() {
            cancelled.store(true, Ordering::Relaxed);
        }
    };
    let built = match selection {
        element_selection::T::SelectedPrimitives(item) => return Ok(item.clone()),
        element_selection::T::SelectedPoints(points) => group::init(
            Some(grain),
            group::Owner::Primitive,
            "__facet_from_points",
            primitive_count,
            &|primitive| {
                check(primitive);
                (view.primitive_offsets[primitive as usize]..view.primitive_offsets[primitive as usize + 1])
                    .any(|vertex| group::mem(view.vertex_points[vertex as usize], points))
            },
        )?,
        element_selection::T::SelectedVertices(vertices) => group::init(
            Some(grain),
            group::Owner::Primitive,
            "__facet_from_vertices",
            primitive_count,
            &|primitive| {
                check(primitive);
                (view.primitive_offsets[primitive as usize]..view.primitive_offsets[primitive as usize + 1])
                    .any(|vertex| group::mem(vertex, vertices))
            },
        )?,
        element_selection::T::SelectedEdges(edges) => {
            let index = topology_index::create(cancel_value, topology_value)?;
            let incidence = topology_index::View::view(&index);
            group::init(Some(grain), group::Owner::Primitive, "__facet_from_edges", primitive_count, &|primitive| {
                check(primitive);
                (view.primitive_offsets[primitive as usize]..view.primitive_offsets[primitive as usize + 1]).any(|vertex| {
                    let edge = incidence.edge_of_vertex[vertex as usize];
                    edge >= 0 && edge_group::mem(edge, edges)
                })
            })?
        }
    };
    if cancelled.load(Ordering::Relaxed) {
        return Err(cancelled_error());
    }
    Ok(built)
}

struct Options {
    cancel: Option<Arc<cancel::T>>,
    grain: i64,
    pre_compute_normals: bool,
    make_normals_unit_length: bool,
    unique_points: bool,
    consolidate_distance: Option<f64>,
    consolidate_normals_distance: Option<f64>,
    remove_inline_points: bool,
    inline_distance: f64,
    orient_polygons: bool,
    cusp_angle: Option<f64>,
    remove_degenerate: bool,
    make_planar: bool,
    post_compute_normals: bool,
    reverse_normals: bool,
}

fn distance_invalid(value: Option<f64>) -> bool {
    value.is_some_and(|value| !value.is_finite() || value < 0.0)
}

fn facet(options: &Options, primitives: Option<&group::T>, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_arc: Option<Arc<cancel::T>> = options.cancel.clone();
    let cancel_value = cancel_arc.as_deref();
    let grain = options.grain;
    let primitive_count = geometry::primitive_count(geometry_value);
    if let Some(item) = primitives {
        if group::owner(item) != group::Owner::Primitive || group::length(item) != primitive_count {
            return Err(fail("Facet selection must be a matching primitive group"));
        }
    }
    if distance_invalid(options.consolidate_distance) {
        return Err(fail("Facet consolidation distance must be finite and non-negative"));
    }
    if distance_invalid(options.consolidate_normals_distance) {
        return Err(fail("Facet normal consolidation distance must be finite and non-negative"));
    }
    if options.consolidate_distance.is_some() && options.consolidate_normals_distance.is_some() {
        return Err(fail("Facet point and normal consolidation modes are mutually exclusive"));
    }
    if options.remove_inline_points && (!options.inline_distance.is_finite() || options.inline_distance < 0.0) {
        return Err(fail("Facet inline distance must be finite and non-negative"));
    }
    if primitives.is_some_and(|item| group::cardinality(item) == 0) {
        return Ok(geometry_value.clone());
    }
    let original_geometry = geometry_value;
    let (selection_name, mut current): (Option<String>, geometry::T) = match primitives {
        None => (None, geometry_value.clone()),
        Some(item) if group::cardinality(item) == primitive_count => (None, geometry_value.clone()),
        Some(item) => {
            let mut suffix = 0;
            let name = loop {
                let candidate = format!("__rdk_facet_selection_{suffix}");
                if geometry::find_group(group::Owner::Primitive, &candidate, geometry_value).is_none() {
                    break candidate;
                }
                suffix += 1;
            };
            let private_group = group::init(Some(grain), group::Owner::Primitive, &name, primitive_count, &|primitive| {
                group::mem(primitive, item)
            })?;
            (Some(name), geometry::with_group(private_group, geometry_value)?)
        }
    };
    let selection_data_id = geometry::data_id(&current);
    let selection_of = |value: &geometry::T| -> Option<group::T> {
        selection_name.as_ref().and_then(|name| geometry::find_group(group::Owner::Primitive, name, value))
    };
    if options.pre_compute_normals {
        current = deform::normals(
            deform::Params { cancel: cancel_arc.clone(), grain, primitives: selection_of(&current) },
            &current,
        )?;
    }
    current = adjust_normals(
        cancel_value,
        grain,
        selection_of(&current).as_ref(),
        options.make_normals_unit_length && !options.pre_compute_normals,
        false,
        &current,
    )?;
    if options.unique_points {
        current = unique_points(cancel_value, grain, selection_of(&current).as_ref(), &current)?;
    }
    match (options.consolidate_distance, options.consolidate_normals_distance) {
        (None, None) => {}
        (Some(tolerance), None) => {
            let selection = match selection_of(&current) {
                None => None,
                Some(item) => Some(facet_point_selection(cancel_value, grain, &item, &current)?),
            };
            current = fuse_grid::weld(
                fuse_grid::WeldParams { cancel: cancel_arc.clone(), grain, selection, tolerance },
                &current,
            )?;
        }
        (None, Some(distance)) => {
            current = consolidate_normals(cancel_value, grain, selection_of(&current).as_ref(), distance, &current)?;
        }
        (Some(_), Some(_)) => return Err(fail("Facet point and normal consolidation modes are mutually exclusive")),
    }
    if options.remove_inline_points {
        current = remove_inline_points(cancel_value, grain, selection_of(&current).as_ref(), options.inline_distance, &current)?;
    }
    if options.orient_polygons {
        current = orient_polygons(cancel_value, grain, selection_of(&current).as_ref(), &current)?;
    }
    if let Some(angle) = options.cusp_angle {
        current = cusp_polygons(cancel_value, grain, selection_of(&current).as_ref(), angle, &current)?;
    }
    if options.remove_degenerate {
        current = clean::delete_degenerate(
            &clean::DeleteDegenerateParams {
                cancel: cancel_arc.clone(),
                grain,
                primitives: selection_of(&current),
                epsilon: 1e-12,
            },
            &current,
        )?;
    }
    if options.make_planar {
        current = make_planar(cancel_value, grain, selection_of(&current).as_ref(), &current)?;
    }
    if options.post_compute_normals {
        current = deform::normals(
            deform::Params { cancel: cancel_arc.clone(), grain, primitives: selection_of(&current) },
            &current,
        )?;
    }
    current = adjust_normals(
        cancel_value,
        grain,
        selection_of(&current).as_ref(),
        false,
        options.reverse_normals,
        &current,
    )?;
    Ok(match &selection_name {
        None => current,
        Some(_) if geometry::data_id(&current) == selection_data_id => original_geometry.clone(),
        Some(name) => geometry::without_group(group::Owner::Primitive, name, &current),
    })
}

/// Run the Facet SOP. The selection (point, vertex, primitive or edge) is promoted to the
/// primitives it touches.
pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(make("invalid_argument", "Rdk_mesh.Facet.run: grain must be positive".to_owned()));
    }
    let cancel_value = params.cancel.as_deref();
    let primitives = match &params.selection {
        None => None,
        Some(selection) => {
            Some(facet_primitives_of_selection(cancel_value, grain, selection, geometry_value).map_err(lift)?)
        }
    };
    let options = Options {
        cancel: params.cancel.clone(),
        grain,
        pre_compute_normals: params.pre_compute_normals.unwrap_or(false),
        make_normals_unit_length: params.make_normals_unit_length.unwrap_or(false),
        unique_points: params.unique_points.unwrap_or(false),
        consolidate_distance: params.consolidate_distance,
        consolidate_normals_distance: params.consolidate_normals_distance,
        remove_inline_points: params.remove_inline_points.unwrap_or(false),
        inline_distance: params.inline_distance.unwrap_or(0.0),
        orient_polygons: params.orient_polygons.unwrap_or(false),
        cusp_angle: params.cusp_angle,
        remove_degenerate: params.remove_degenerate.unwrap_or(false),
        make_planar: params.make_planar.unwrap_or(false),
        post_compute_normals: params.post_compute_normals.unwrap_or(false),
        reverse_normals: params.reverse_normals.unwrap_or(false),
    };
    facet(&options, primitives.as_ref(), geometry_value).map_err(lift)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;
    use std::sync::Mutex;

    type P3 = (f64, f64, f64);

    fn check(condition: bool, message: &str) {
        assert!(condition, "{message}");
    }

    fn near(left: f64, right: f64) -> bool {
        (left - right).abs() < 1e-9
    }

    fn f3(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> packed::Float3::T {
        packed::Float3::View::of_owned_exn(x, y, z).unwrap()
    }

    fn polygons(point_count: i64, vertex_points: Vec<i64>, offsets: Vec<i64>) -> topology::T {
        topology::polygons_owned(point_count, vertex_points, offsets).unwrap()
    }

    fn build(
        positions: packed::Float3::T,
        topology_value: topology::T,
        attributes: Vec<attribute::T>,
        groups: Vec<group::T>,
    ) -> geometry::T {
        geometry::create(positions, topology_value, Some(attributes), Some(groups), None).unwrap()
    }

    fn point_cloud(values: &[P3]) -> geometry::T {
        geometry::create(
            f3(values.iter().map(|p| p.0).collect(), values.iter().map(|p| p.1).collect(), values.iter().map(|p| p.2).collect()),
            topology::empty(values.len() as i64).unwrap(),
            None,
            None,
            None,
        ).unwrap()
    }

    fn add_attribute(owner: attribute::Owner, name: &str, storage: attribute::Storage, value: &geometry::T) -> geometry::T {
        geometry::with_attribute(attribute::create_owned(name, owner, storage).unwrap(), value).unwrap()
    }

    fn primitive_group(value: &geometry::T, predicate: &(dyn Fn(i64) -> bool + Send + Sync)) -> group::T {
        group::init(None, group::Owner::Primitive, "selected", geometry::primitive_count(value), predicate).unwrap()
    }

    fn run_facet(params: Params, value: &geometry::T) -> Result<geometry::T, error::T> {
        run(&params, value)
    }

    fn facet_ok(params: Params, value: &geometry::T) -> geometry::T {
        run_facet(params, value).unwrap()
    }

    fn selection(items: &group::T) -> Option<element_selection::T> {
        Some(element_selection::T::SelectedPrimitives(items.clone()))
    }

    fn same(left: &geometry::T, right: &geometry::T) -> bool {
        geometry::data_id(left) == geometry::data_id(right)
    }

    fn expect_code(value: Result<geometry::T, error::T>, code: &str, message: &str) {
        match value {
            Err(failure) if error::code(&failure) == code => {}
            _ => panic!("{message}"),
        }
    }

    fn storage_equal(left: &attribute::T, right: &attribute::T) -> bool {
        use attribute::Storage;
        match (attribute::View::storage(left).unwrap(), attribute::View::storage(right).unwrap()) {
            (Storage::Float(a), Storage::Float(b)) => a == b,
            (Storage::Int(a), Storage::Int(b)) => a == b,
            (Storage::Text(a), Storage::Text(b)) => a == b,
            (Storage::Float2(a), Storage::Float2(b)) => {
                let (a, b) = (packed::Float2::View::view(a), packed::Float2::View::view(b));
                a.x == b.x && a.y == b.y
            }
            (Storage::Float3(a), Storage::Float3(b)) => {
                let (a, b) = (packed::Float3::View::view(a).unwrap(), packed::Float3::View::view(b).unwrap());
                a.x.iter().zip(b.x.iter()).all(|(p, q)| p.to_bits() == q.to_bits() || (p.is_nan() && q.is_nan()))
                    && a.y == b.y
                    && a.z == b.z
            }
            (Storage::Float4(a), Storage::Float4(b)) => {
                let (a, b) = (packed::Float4::View::view(a), packed::Float4::View::view(b));
                a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w
            }
            (Storage::IntArray(a), Storage::IntArray(b)) => {
                let (a, b) = (packed::IntArray::View::view(a), packed::IntArray::View::view(b));
                a.offsets == b.offsets && a.values == b.values
            }
            (Storage::FloatArray(a), Storage::FloatArray(b)) => {
                let (a, b) = (packed::FloatArray::View::view(a), packed::FloatArray::View::view(b));
                a.offsets == b.offsets && a.values == b.values
            }
            _ => false,
        }
    }

    fn equal_group(a: &group::T, b: &group::T) -> bool {
        group::name(a) == group::name(b)
            && group::owner(a) == group::owner(b)
            && group::length(a) == group::length(b)
            && group::ordered_elements(a) == group::ordered_elements(b)
            && (0..group::length(a)).all(|element| group::mem(element, a) == group::mem(element, b))
    }

    fn equal_geometry(left: &geometry::T, right: &geometry::T) -> bool {
        let (lp, rp) = (packed::Float3::View::view(geometry::positions(left)).unwrap(), packed::Float3::View::view(geometry::positions(right)).unwrap());
        let (lt, rt) = (topology::View::view(geometry::topology(left)).unwrap(), topology::View::view(geometry::topology(right)).unwrap());
        let (la, ra) = (geometry::attributes(left), geometry::attributes(right));
        let (lg, rg) = (geometry::groups(left), geometry::groups(right));
        let (le, re) = (geometry::edge_groups(left), geometry::edge_groups(right));
        lp.x == rp.x
            && lp.y == rp.y
            && lp.z == rp.z
            && lt.point_count == rt.point_count
            && lt.vertex_points == rt.vertex_points
            && lt.primitive_offsets == rt.primitive_offsets
            && lt.primitive_kinds == rt.primitive_kinds
            && la.len() == ra.len()
            && la.iter().zip(&ra).all(|(a, b)| {
                attribute::name(a) == attribute::name(b) && attribute::owner(a) == attribute::owner(b) && storage_equal(a, b)
            })
            && lg.len() == rg.len()
            && lg.iter().zip(&rg).all(|(a, b)| equal_group(a, b))
            && le.len() == re.len()
            && le.iter().zip(&re).all(|(a, b)| {
                edge_group::name(a) == edge_group::name(b)
                    && edge_group::length(a) == edge_group::length(b)
                    && (0..edge_group::length(a)).all(|edge| edge_group::mem(edge, a) == edge_group::mem(edge, b))
            })
    }

    fn at_domains(domains: i64, work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
        let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            *slot.lock().unwrap() = Some(work());
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }

    fn one_four(work: &(dyn Fn() -> geometry::T + Sync)) -> (geometry::T, geometry::T) {
        (at_domains(1, work), at_domains(4, work))
    }

    fn view_of(value: &geometry::T) -> topology::View::View {
        topology::View::view(geometry::topology(value)).unwrap()
    }

    fn point_ints(name: &str, value: &geometry::T) -> Vec<i64> {
        match attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, name, value).unwrap()).unwrap() {
            attribute::Storage::Int(values) => values.clone(),
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn named_group(owner: group::Owner, name: &str, value: &geometry::T) -> group::T {
        geometry::find_group(owner, name, value).unwrap_or_else(|| panic!("group {name} missing"))
    }

    fn normal_attribute(value: &geometry::T) -> packed::Float3::View::View {
        match attribute::View::storage(
            &geometry::find_attribute(attribute::Owner::Point, "N", value).expect("Facet lost N"),
        )
        .unwrap()
        {
            attribute::Storage::Float3(values) => packed::Float3::View::view(values).unwrap(),
            _ => panic!("Facet N has wrong storage"),
        }
    }

    fn find_edge(value: &geometry::T, a: i64, b: i64) -> i64 {
        let index = topology_index::create(None, geometry::topology(value)).unwrap();
        let view = topology_index::View::view(&index);
        let mut found = -1;
        for edge in 0..view.edge_a.len() {
            if view.edge_a[edge] == a && view.edge_b[edge] == b {
                found = edge as i64;
            }
        }
        found
    }

    fn source() -> geometry::T {
        let positions = f3(vec![0., 1., 0., 0., 4.], vec![0., 0., 1., 1., 5.], vec![0., 0., 0., 1., 6.]);
        let topology_value = polygons(5, vec![0, 1, 2, 0, 2, 3], vec![0, 3, 6]);
        let geometry_value = geometry::create(positions, topology_value.clone(), None, None, None).unwrap();
        let indices: Vec<i64> = (0..5).collect();
        let csr_offsets = vec![0_i64, 2, 4, 6, 8, 10];
        let p = attribute::Owner::Point;
        let mut value = geometry_value;
        value = add_attribute(p, "weight", attribute::Storage::Float(indices.iter().map(|&v| v as f64 + 0.25).collect()), &value);
        value = add_attribute(p, "id", attribute::Storage::Int(indices.clone()), &value);
        value = add_attribute(p, "label", attribute::Storage::Text(indices.iter().map(|v| format!("p{v}").into_bytes()).collect()), &value);
        value = add_attribute(
            p,
            "uv",
            attribute::Storage::Float2(packed::Float2::of_owned(
                indices.iter().map(|&v| v as f64).collect(),
                indices.iter().map(|&v| -(v as f64)).collect(),
            ).unwrap()),
            &value,
        );
        value = add_attribute(
            p,
            "vector",
            attribute::Storage::Float3(f3(
                indices.iter().map(|&v| v as f64).collect(),
                indices.iter().map(|&v| (v + 10) as f64).collect(),
                indices.iter().map(|&v| (v + 20) as f64).collect(),
            )),
            &value,
        );
        value = add_attribute(
            p,
            "quaternion",
            attribute::Storage::Float4(packed::Float4::of_owned(
                indices.iter().map(|&v| v as f64).collect(),
                vec![0.; 5],
                vec![0.; 5],
                vec![1.; 5],
            ).unwrap()),
            &value,
        );
        value = add_attribute(
            p,
            "neighbors",
            attribute::Storage::IntArray(packed::IntArray::create_owned(csr_offsets.clone(), (0..10).collect()).unwrap()),
            &value,
        );
        value = add_attribute(
            p,
            "samples",
            attribute::Storage::FloatArray(packed::FloatArray::create_owned(
                csr_offsets,
                (0..10).map(|v| v as f64 * 0.5).collect(),
            ).unwrap()),
            &value,
        );
        let point_group = group::ordered(group::Owner::Point, "marked", 5, &[2, 0, 4]).unwrap();
        let vertex_group = group::init(None, group::Owner::Vertex, "corners", 6, &|v| v & 1 == 0).unwrap();
        let primitive_group = group::init(None, group::Owner::Primitive, "faces", 2, &|p| p == 1).unwrap();
        value = geometry::with_group(point_group, &value).unwrap();
        value = geometry::with_group(vertex_group, &value).unwrap();
        value = geometry::with_group(primitive_group, &value).unwrap();
        let index = topology_index::create(None, &topology_value).unwrap();
        let shared_edge = topology_index::find_edge_index(&index, 0, 2);
        let edges = edge_group::init(None, &topology_value, &index, "crease", &|edge| edge == shared_edge).unwrap();
        geometry::with_edge_group(edges, &value).unwrap()
    }

    const POINT_SOURCE_MAP: [i64; 7] = [0, 1, 2, 0, 2, 3, 4];

    fn point_storage_matches(source: &attribute::T, output: &attribute::T) -> bool {
        use attribute::Storage;
        let all = |predicate: &dyn Fn(usize, usize) -> bool| {
            POINT_SOURCE_MAP.iter().enumerate().all(|(target, &origin)| predicate(target, origin as usize))
        };
        match (attribute::View::storage(source).unwrap(), attribute::View::storage(output).unwrap()) {
            (Storage::Float(s), Storage::Float(o)) => all(&|t, g| o[t] == s[g]),
            (Storage::Int(s), Storage::Int(o)) => all(&|t, g| o[t] == s[g]),
            (Storage::Text(s), Storage::Text(o)) => all(&|t, g| o[t] == s[g]),
            (Storage::Float2(s), Storage::Float2(o)) => {
                all(&|t, g| packed::Float2::get(o, t as i64).unwrap() == packed::Float2::get(s, g as i64).unwrap())
            }
            (Storage::Float3(s), Storage::Float3(o)) => {
                all(&|t, g| packed::Float3::get(o, t as i64).unwrap() == packed::Float3::get(s, g as i64).unwrap())
            }
            (Storage::Float4(s), Storage::Float4(o)) => {
                all(&|t, g| packed::Float4::get(o, t as i64).unwrap() == packed::Float4::get(s, g as i64).unwrap())
            }
            (Storage::IntArray(s), Storage::IntArray(o)) => {
                all(&|t, g| packed::IntArray::get(o, t as i64).unwrap() == packed::IntArray::get(s, g as i64).unwrap())
            }
            (Storage::FloatArray(s), Storage::FloatArray(o)) => {
                all(&|t, g| packed::FloatArray::get(o, t as i64).unwrap() == packed::FloatArray::get(s, g as i64).unwrap())
            }
            _ => false,
        }
    }

    #[test]
    fn unique_points() {
        let source = source();
        let output = facet_ok(Params { unique_points: Some(true), ..Params::default() }, &source);
        check(
            geometry::point_count(&output) == 7 && geometry::vertex_count(&output) == 6 && geometry::primitive_count(&output) == 2,
            "Facet Unique Points cardinality",
        );
        check(view_of(&output).vertex_points[..] == [0, 1, 2, 3, 4, 5], "Facet Unique Points topology");
        let positions = packed::Float3::View::view(geometry::positions(&output)).unwrap();
        let source_positions = packed::Float3::View::view(geometry::positions(&source)).unwrap();
        for (output_point, &source_point) in POINT_SOURCE_MAP.iter().enumerate() {
            let s = source_point as usize;
            check(
                positions.x[output_point] == source_positions.x[s]
                    && positions.y[output_point] == source_positions.y[s]
                    && positions.z[output_point] == source_positions.z[s],
                "Facet Unique Points position ancestry",
            );
        }
        for name in ["weight", "id", "label", "uv", "vector", "quaternion", "neighbors", "samples"] {
            match (
                geometry::find_attribute(attribute::Owner::Point, name, &source),
                geometry::find_attribute(attribute::Owner::Point, name, &output),
            ) {
                (Some(s), Some(o)) => check(point_storage_matches(&s, &o), &format!("Facet point attribute ancestry: {name}")),
                _ => panic!("Facet lost point attribute {name}"),
            }
        }
        let marked = named_group(group::Owner::Point, "marked", &output);
        check(
            group::cardinality(&marked) == 5 && group::ordered_elements(&marked) == Some(vec![2, 4, 0, 3, 6]),
            "Facet ordered point-group ancestry",
        );
        check(
            edge_group::cardinality(&geometry::find_edge_group("crease", &output).unwrap()) == 2,
            "Facet one-to-many native edge-group ancestry",
        );
        let consolidated = facet_ok(
            Params { unique_points: Some(true), consolidate_distance: Some(0.), ..Params::default() },
            &source,
        );
        check(geometry::point_count(&consolidated) == 5, "Facet Unique Points then Consolidate pipeline order");
    }

    #[test]
    fn primitive_group_unique_points() {
        let source = source();
        let selected = primitive_group(&source, &|p| p == 0);
        let output = facet_ok(
            Params { selection: selection(&selected), unique_points: Some(true), ..Params::default() },
            &source,
        );
        let output_view = view_of(&output);
        check(
            geometry::point_count(&output) == 7 && output_view.vertex_points[..] == [4, 5, 6, 0, 1, 2],
            "grouped Facet Unique Points topology",
        );
        check(point_ints("id", &output) == [0, 2, 3, 4, 0, 1, 2], "grouped Facet Unique Points point ancestry");
        let source_view = view_of(&source);
        let ids = point_ints("id", &output);
        for local in 0..3 {
            let output_point = output_view.vertex_points[3 + local] as usize;
            let source_point = source_view.vertex_points[3 + local];
            check(ids[output_point] == source_point, "grouped Facet changed an unselected primitive point");
        }
        let empty = primitive_group(&source, &|_| false);
        check(
            same(
                &facet_ok(Params { selection: selection(&empty), unique_points: Some(true), ..Params::default() }, &source),
                &source,
            ),
            "empty Facet primitive group is not identity",
        );
        let all = primitive_group(&source, &|_| true);
        check(
            equal_geometry(
                &facet_ok(Params { selection: selection(&all), unique_points: Some(true), ..Params::default() }, &source),
                &facet_ok(Params { unique_points: Some(true), ..Params::default() }, &source),
            ),
            "all-selected Facet Unique Points differs from compatibility path",
        );
    }

    #[test]
    fn typed_selections() {
        let source = source();
        let first = primitive_group(&source, &|p| p == 0);
        let unique = || Params { unique_points: Some(true), ..Params::default() };
        let expected = facet_ok(Params { selection: selection(&first), ..unique() }, &source);
        let points = group::init(None, group::Owner::Point, "facet_point", 5, &|p| p == 1).unwrap();
        let point_output = facet_ok(
            Params { selection: Some(element_selection::T::SelectedPoints(points)), ..unique() },
            &source,
        );
        check(equal_geometry(&point_output, &expected), "Facet point selection did not promote to incident primitives");
        let vertices = group::init(None, group::Owner::Vertex, "facet_vertex", 6, &|v| v == 1).unwrap();
        let vertex_output = facet_ok(
            Params { selection: Some(element_selection::T::SelectedVertices(vertices)), ..unique() },
            &source,
        );
        check(equal_geometry(&vertex_output, &expected), "Facet vertex selection did not promote to its owning primitive");
        let topology_value = geometry::topology(&source);
        let index = topology_index::create(None, topology_value).unwrap();
        let boundary_edge = topology_index::find_edge_index(&index, 0, 1);
        let edges = edge_group::init(None, topology_value, &index, "facet_edge", &|e| e == boundary_edge).unwrap();
        let edge_output = facet_ok(
            Params { selection: Some(element_selection::T::SelectedEdges(edges)), ..unique() },
            &source,
        );
        check(equal_geometry(&edge_output, &expected), "Facet edge selection did not promote to its incident primitive");
        let shared = geometry::find_edge_group("crease", &source).unwrap();
        let all = facet_ok(Params { selection: Some(element_selection::T::SelectedEdges(shared)), ..unique() }, &source);
        check(
            equal_geometry(&all, &facet_ok(unique(), &source)),
            "Facet shared-edge selection did not promote both incident primitives",
        );
        let empty = group::init(None, group::Owner::Point, "empty", 5, &|_| false).unwrap();
        check(
            same(
                &facet_ok(Params { selection: Some(element_selection::T::SelectedPoints(empty)), ..unique() }, &source),
                &source,
            ),
            "Facet empty typed selection is not identity",
        );
        let wrong = group::init(None, group::Owner::Primitive, "wrong", 2, &|_| true).unwrap();
        expect_code(
            run_facet(Params { selection: Some(element_selection::T::SelectedPoints(wrong)), ..unique() }, &source),
            "invalid_geometry",
            "Facet accepted a typed selection with the wrong owner",
        );
        let foreign = geometry::create(
            f3(vec![0., 1., 1., 0.], vec![0., 0., 1., 1.], vec![0.; 4]),
            polygons(4, vec![0, 1, 2, 3], vec![0, 4]),
            None,
            None,
            None,
        ).unwrap();
        let foreign_index = topology_index::create(None, geometry::topology(&foreign)).unwrap();
        let foreign_edge = edge_group::init(None, geometry::topology(&foreign), &foreign_index, "foreign", &|e| e == 0).unwrap();
        expect_code(
            run_facet(Params { selection: Some(element_selection::T::SelectedEdges(foreign_edge)), ..unique() }, &source),
            "invalid_geometry",
            "Facet accepted a foreign-topology edge selection",
        );
    }

    #[test]
    fn normals() {
        let geometry_value = source();
        let pre = facet_ok(
            Params { pre_compute_normals: Some(true), unique_points: Some(true), ..Params::default() },
            &geometry_value,
        );
        let post = facet_ok(
            Params { unique_points: Some(true), post_compute_normals: Some(true), ..Params::default() },
            &geometry_value,
        );
        let (pre_n, post_n) = (normal_attribute(&pre), normal_attribute(&post));
        check(
            near(pre_n.x[0], pre_n.x[3]) && near(pre_n.y[0], pre_n.y[3]) && near(pre_n.z[0], pre_n.z[3]),
            "Facet pre-computed smooth normals were not duplicated",
        );
        check(
            !(near(post_n.x[0], post_n.x[3]) && near(post_n.y[0], post_n.y[3]) && near(post_n.z[0], post_n.z[3])),
            "Facet post-computed normals did not become face-hard",
        );
        let with_normal = add_attribute(
            attribute::Owner::Point,
            "N",
            attribute::Storage::Float3(f3(vec![2.; 5], vec![0.; 5], vec![0.; 5])),
            &source(),
        );
        let adjusted = facet_ok(
            Params { make_normals_unit_length: Some(true), reverse_normals: Some(true), ..Params::default() },
            &with_normal,
        );
        let normal = normal_attribute(&adjusted);
        check(
            normal.x[..] == [-1.; 5] && normal.y[..] == [0.; 5] && normal.z[..] == [0.; 5],
            "Facet normal normalization/reversal",
        );
    }

    #[test]
    fn consolidate_normals() {
        let positions = f3(vec![0., 0.01, 1., 2., 0., 2.], vec![0., 0., 0., 0., 1., 1.], vec![0.; 6]);
        let topology_value = polygons(6, vec![0, 2, 4, 0, 4, 5, 1, 3, 5], vec![0, 3, 6, 9]);
        let point_normal = attribute::create_owned(
            "N",
            attribute::Owner::Point,
            attribute::Storage::Float3(f3(vec![1., 0., 2., 4., 0., 0.], vec![0., 1., 0., 0., 0., 0.], vec![0.; 6])),
        ).unwrap();
        let vertex_normal = attribute::create_owned(
            "N",
            attribute::Owner::Vertex,
            attribute::Storage::Float3(f3(
                vec![1., 2., 3., 0., 4., 5., 0., 6., 7.],
                vec![0., 0., 0., 1., 0., 0., 0., 0., 0.],
                vec![0., 0., 0., 0., 0., 0., 1., 0., 0.],
            )),
        ).unwrap();
        let tag = attribute::create_owned("tag", attribute::Owner::Detail, attribute::Storage::Text(vec![b"normals".to_vec()])).unwrap();
        let selected = group::ordered(group::Owner::Point, "selected", 6, &[1, 0, 4]).unwrap();
        let source = build(positions, topology_value, vec![point_normal, vertex_normal, tag], vec![selected]);
        let consolidate = |distance: f64| Params { consolidate_normals_distance: Some(distance), ..Params::default() };
        let output = facet_ok(consolidate(0.01), &source);
        check(
            packed::Float3::data_id(geometry::positions(&output)) == packed::Float3::data_id(geometry::positions(&source))
                && topology::data_id(geometry::topology(&output)) == topology::data_id(geometry::topology(&source))
                && geometry::groups(&output).len() == geometry::groups(&source).len()
                && geometry::groups(&output).iter().zip(geometry::groups(&source).iter()).all(|(a, b)| equal_group(a, b)),
            "Facet Consolidate Normals changed non-normal geometry payload",
        );
        let point = geometry::find_attribute(attribute::Owner::Point, "N", &output).unwrap();
        let vertex = geometry::find_attribute(attribute::Owner::Vertex, "N", &output).unwrap();
        match attribute::View::storage(&point).unwrap() {
            attribute::Storage::Float3(values) => {
                let values = packed::Float3::View::view(values).unwrap();
                check(
                    near(values.x[0], 0.5) && near(values.y[0], 0.5) && near(values.x[1], 0.5) && near(values.y[1], 0.5),
                    "Facet point-normal arithmetic average",
                );
                check(
                    values.x[2].to_bits() == 2.0_f64.to_bits() && values.x[3].to_bits() == 4.0_f64.to_bits(),
                    "Facet point-normal singleton bit preservation",
                );
            }
            _ => panic!("Facet consolidated point N has wrong storage"),
        }
        match attribute::View::storage(&vertex).unwrap() {
            attribute::Storage::Float3(values) => {
                let values = packed::Float3::View::view(values).unwrap();
                for element in [0, 3, 6] {
                    check(
                        near(values.x[element], 1. / 3.) && near(values.y[element], 1. / 3.) && near(values.z[element], 1. / 3.),
                        "Facet vertex-normal point-cluster average",
                    );
                }
                check(values.x[2] == 3. && values.x[4] == 4., "Facet vertex-normal singleton fan changed");
            }
            _ => panic!("Facet consolidated vertex N has wrong storage"),
        }
        let unit = normal_attribute_of(&facet_ok(
            Params { make_normals_unit_length: Some(true), consolidate_normals_distance: Some(0.01), ..Params::default() },
            &source,
        ));
        check(near(unit.x[0], 0.5) && near(unit.y[0], 0.5), "Facet unit-normal then consolidation pipeline order");
        let no_normals = point_cloud(&[(0., 0., 0.), (0., 0., 0.)]);
        check(same(&facet_ok(consolidate(0.), &no_normals), &no_normals), "Facet Consolidate Normals no-normal identity");
        let extreme = add_attribute(
            attribute::Owner::Point,
            "N",
            attribute::Storage::Float3(f3(vec![f64::MAX, f64::MAX], vec![0., 0.], vec![0., 0.])),
            &point_cloud(&[(0., 0., 0.), (0., 0., 0.)]),
        );
        let extreme = normal_attribute_of(&facet_ok(consolidate(0.), &extreme));
        check(extreme.x[0].is_finite() && extreme.x[0] == f64::MAX, "Facet Consolidate Normals overflowed a finite average");
        for distance in [-0.1, f64::NAN] {
            expect_code(
                run_facet(consolidate(distance), &source),
                "invalid_geometry",
                "Facet accepted an invalid normal consolidation distance",
            );
        }
        expect_code(
            run_facet(
                Params { consolidate_distance: Some(0.), consolidate_normals_distance: Some(0.), ..Params::default() },
                &source,
            ),
            "invalid_geometry",
            "Facet accepted mutually exclusive consolidation modes",
        );
        let wrong = add_attribute(
            attribute::Owner::Point,
            "N",
            attribute::Storage::Float(vec![1.; 6]),
            &geometry::without_attribute(attribute::Owner::Point, "N", &source),
        );
        expect_code(
            run_facet(consolidate(0.01), &wrong),
            "invalid_geometry",
            "Facet accepted scalar N for normal consolidation",
        );
    }

    fn normal_attribute_of(value: &geometry::T) -> packed::Float3::View::View {
        normal_attribute(value)
    }

    fn inline_source() -> geometry::T {
        let positions = f3(
            vec![0., 1., 2., 3., 3., 0., 9., 1., 2., 0., 1., 2.],
            vec![0., 0., 0., 0., 0., 0., 9., 0., 1., 3., 3., 3.],
            vec![0., 0., 0., 0., 2., 2., 9., 1., 1., 0., 0., 0.],
        );
        let topology_value = topology::View::create_validated_owned(
            12,
            vec![0, 1, 2, 3, 4, 5, 1, 7, 8, 9, 10, 11],
            vec![0, 6, 9, 12],
            vec![0, 0, 1],
        );
        let point_id = attribute::create_owned("point_id", attribute::Owner::Point, attribute::Storage::Int((0..12).collect())).unwrap();
        let corner_id = attribute::create_owned("corner_id", attribute::Owner::Vertex, attribute::Storage::Int((0..12).collect())).unwrap();
        let face_id = attribute::create_owned("face_id", attribute::Owner::Primitive, attribute::Storage::Int(vec![10, 11, 12])).unwrap();
        let detail = attribute::create_owned("tag", attribute::Owner::Detail, attribute::Storage::Text(vec![b"inline".to_vec()])).unwrap();
        let points = group::ordered(group::Owner::Point, "point_order", 12, &[2, 1, 6]).unwrap();
        let corners = group::ordered(group::Owner::Vertex, "corner_order", 12, &[1, 2, 4, 10]).unwrap();
        let faces = group::ordered(group::Owner::Primitive, "face_order", 3, &[2, 0]).unwrap();
        let geometry_value = build(positions, topology_value.clone(), vec![point_id, corner_id, face_id, detail], vec![points, corners, faces]);
        let index = topology_index::create(None, &topology_value).unwrap();
        let view = topology_index::View::view(&index);
        let segment = edge_group::init(None, &topology_value, &index, "removed_segment", &|edge| {
            view.edge_a[edge as usize] == 1 && view.edge_b[edge as usize] == 2
        }).unwrap();
        geometry::with_edge_group(segment, &geometry_value).unwrap()
    }

    fn int_attr(owner: attribute::Owner, name: &str, value: &geometry::T) -> Vec<i64> {
        match attribute::View::storage(&geometry::find_attribute(owner, name, value).unwrap()).unwrap() {
            attribute::Storage::Int(values) => values.clone(),
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn inline_params(distance: Option<f64>) -> Params {
        Params { remove_inline_points: Some(true), inline_distance: distance, ..Params::default() }
    }

    #[test]
    fn remove_inline_points() {
        let source = inline_source();
        let output = facet_ok(inline_params(None), &source);
        check(
            geometry::point_count(&output) == 11 && geometry::vertex_count(&output) == 10 && geometry::primitive_count(&output) == 3,
            "Facet Remove Inline Points cardinality",
        );
        let topology_view = view_of(&output);
        check(
            topology_view.vertex_points[..] == [0, 2, 3, 4, 1, 6, 7, 8, 9, 10]
                && topology_view.primitive_offsets[..] == [0, 4, 7, 10]
                && topology_view.primitive_kinds[2] == 1,
            "Facet Remove Inline Points topology or curve pass-through",
        );
        check(
            int_attr(attribute::Owner::Point, "point_id", &output) == [0, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11],
            "Facet Remove Inline Points point ancestry",
        );
        check(
            int_attr(attribute::Owner::Vertex, "corner_id", &output) == [0, 3, 4, 5, 6, 7, 8, 9, 10, 11],
            "Facet Remove Inline Points vertex ancestry",
        );
        check(
            int_attr(attribute::Owner::Primitive, "face_id", &output) == [10, 11, 12]
                && match attribute::View::storage(&geometry::find_attribute(attribute::Owner::Detail, "tag", &output).unwrap()).unwrap() {
                    attribute::Storage::Text(values) => values[..] == [b"inline".to_vec()],
                    _ => false,
                },
            "Facet Remove Inline Points changed primitive/detail payload",
        );
        check(
            group::ordered_elements(&named_group(group::Owner::Point, "point_order", &output)) == Some(vec![1, 5]),
            "Facet Remove Inline Points ordered point group",
        );
        check(
            group::ordered_elements(&named_group(group::Owner::Vertex, "corner_order", &output)) == Some(vec![2, 8]),
            "Facet Remove Inline Points ordered vertex group",
        );
        check(
            group::ordered_elements(&named_group(group::Owner::Primitive, "face_order", &output)) == Some(vec![2, 0]),
            "Facet Remove Inline Points primitive group",
        );
        let healed = geometry::find_edge_group("removed_segment", &output).unwrap();
        let expected_edge = find_edge(&output, 0, 2);
        check(
            expected_edge >= 0 && edge_group::cardinality(&healed) == 1 && edge_group::mem(expected_edge, &healed),
            "Facet Remove Inline Points healed edge ancestry",
        );
        let retained = facet_ok(inline_params(Some(0.)), &triangle_grid(8, 6, 2.0));
        check(geometry::vertex_count(&retained) == 8 * 6 * 6, "Facet Remove Inline Points changed non-inline triangles");
        let translated = |middle_offset: f64| {
            let base = 1e12;
            let positions = f3(
                vec![base, base + 1., base + 2., base + 2., base],
                vec![0.; 5],
                vec![base, base + middle_offset, base, base + 2., base + 2.],
            );
            geometry::create(positions, polygons(5, vec![0, 1, 2, 3, 4], vec![0, 5]), None, None, None).unwrap()
        };
        let translated_corner = translated(0.01);
        check(
            same(&facet_ok(inline_params(Some(0.001)), &translated_corner), &translated_corner),
            "Facet inline tolerance changed under a large translation",
        );
        check(
            geometry::point_count(&facet_ok(inline_params(None), &translated(0.))) == 4,
            "Facet missed a translated exact-inline corner",
        );
        for distance in [-0.1, f64::NAN] {
            expect_code(
                run_facet(inline_params(Some(distance)), &source),
                "invalid_geometry",
                "Facet accepted an invalid inline distance",
            );
        }
    }

    #[test]
    fn primitive_group_inline_points() {
        let source = inline_source();
        let first = primitive_group(&source, &|p| p == 0);
        let output = facet_ok(Params { selection: selection(&first), ..inline_params(None) }, &source);
        check(
            geometry::vertex_count(&output) == 10 && geometry::point_count(&output) == 11,
            "grouped Facet Remove Inline Points cardinality",
        );
        check(
            int_attr(attribute::Owner::Vertex, "corner_id", &output) == [0, 3, 4, 5, 6, 7, 8, 9, 10, 11],
            "grouped Facet changed unselected vertex payload",
        );
        let curve_only = primitive_group(&source, &|p| p == 2);
        check(
            same(&facet_ok(Params { selection: selection(&curve_only), ..inline_params(None) }, &source), &source),
            "grouped Facet removed inline points outside its selection",
        );
    }

    #[test]
    fn orient_polygons() {
        let base = source();
        let point_count = view_of(&base).point_count;
        let topology_value = polygons(point_count, vec![0, 1, 2, 0, 3, 2], vec![0, 3, 6]);
        let corner = attribute::create_owned("corner_id", attribute::Owner::Vertex, attribute::Storage::Int((0..6).collect())).unwrap();
        let corners = group::ordered(group::Owner::Vertex, "corner_order", 6, &[3, 5]).unwrap();
        let geometry_value = build(geometry::positions(&base).clone(), topology_value.clone(), vec![corner], vec![corners]);
        let index = topology_index::create(None, &topology_value).unwrap();
        let view = topology_index::View::view(&index);
        let crease = edge_group::init(None, &topology_value, &index, "shared", &|edge| {
            view.edge_a[edge as usize] == 0 && view.edge_b[edge as usize] == 2
        }).unwrap();
        let geometry_value = geometry::with_edge_group(crease, &geometry_value).unwrap();
        let oriented = facet_ok(Params { orient_polygons: Some(true), ..Params::default() }, &geometry_value);
        check(view_of(&oriented).vertex_points[3..6] == [2, 3, 0], "Facet Orient Polygons did not reverse the inconsistent face");
        check(
            int_attr(attribute::Owner::Vertex, "corner_id", &oriented) == [0, 1, 2, 5, 4, 3],
            "Facet Orient Polygons vertex-attribute ancestry",
        );
        check(
            group::ordered_elements(&named_group(group::Owner::Vertex, "corner_order", &oriented)) == Some(vec![5, 3]),
            "Facet Orient Polygons ordered vertex-group ancestry",
        );
        check(
            edge_group::cardinality(&geometry::find_edge_group("shared", &oriented).unwrap()) == 1,
            "Facet Orient Polygons native edge-group ancestry",
        );
        let nonmanifold = geometry::create(
            f3(vec![0., 1., 0., 0., 0.], vec![0., 0., 1., 0., -1.], vec![0., 0., 0., 1., 0.]),
            polygons(5, vec![0, 1, 2, 1, 0, 3, 0, 1, 4], vec![0, 3, 6, 9]),
            None,
            None,
            None,
        ).unwrap();
        expect_code(
            run_facet(Params { orient_polygons: Some(true), ..Params::default() }, &nonmanifold),
            "invalid_geometry",
            "Facet Orient Polygons accepted a non-manifold edge",
        );
    }

    #[test]
    fn primitive_group_orient_polygons() {
        let positions = f3(vec![0., 1., 0., 1., 2.], vec![0., 0., 1., 1., 1.], vec![0.; 5]);
        let topology_value = polygons(5, vec![0, 1, 2, 0, 3, 2, 2, 3, 4], vec![0, 3, 6, 9]);
        let corners = attribute::create_owned("corner_id", attribute::Owner::Vertex, attribute::Storage::Int((0..9).collect())).unwrap();
        let source = build(positions.clone(), topology_value, vec![corners], Vec::new());
        let selected = primitive_group(&source, &|p| p < 2);
        let output = facet_ok(Params { selection: selection(&selected), orient_polygons: Some(true), ..Params::default() }, &source);
        check(
            view_of(&output).vertex_points[..] == [0, 1, 2, 2, 3, 0, 2, 3, 4],
            "grouped Facet Orient Polygons changed selection boundary behavior",
        );
        check(
            int_attr(attribute::Owner::Vertex, "corner_id", &output) == [0, 1, 2, 5, 4, 3, 6, 7, 8],
            "grouped Facet Orient Polygons changed unselected vertices",
        );
        let nonmanifold = geometry::create(
            positions,
            polygons(5, vec![0, 1, 2, 1, 0, 3, 0, 1, 4], vec![0, 3, 6, 9]),
            None,
            None,
            None,
        ).unwrap();
        let one_face = primitive_group(&nonmanifold, &|p| p == 0);
        check(
            same(
                &facet_ok(Params { selection: selection(&one_face), orient_polygons: Some(true), ..Params::default() }, &nonmanifold),
                &nonmanifold,
            ),
            "grouped Facet inspected an unselected non-manifold neighborhood",
        );
    }

    /// 8-point cube, six outward quads, `point_id` and a point-normal-free payload.
    fn cube() -> geometry::T {
        let corners = [
            (-1., -1., -1.),
            (1., -1., -1.),
            (1., 1., -1.),
            (-1., 1., -1.),
            (-1., -1., 1.),
            (1., -1., 1.),
            (1., 1., 1.),
            (-1., 1., 1.),
        ];
        let faces = vec![0, 3, 2, 1, 4, 5, 6, 7, 0, 1, 5, 4, 2, 3, 7, 6, 0, 4, 7, 3, 1, 2, 6, 5];
        geometry::create(
            f3(corners.iter().map(|p| p.0).collect(), corners.iter().map(|p| p.1).collect(), corners.iter().map(|p| p.2).collect()),
            polygons(8, faces, vec![0, 4, 8, 12, 16, 20, 24]),
            None,
            None,
            None,
        ).unwrap()
    }

    #[test]
    fn cusp_polygons() {
        let source = add_attribute(attribute::Owner::Point, "point_id", attribute::Storage::Int((0..8).collect()), &cube());
        let corner = group::ordered(group::Owner::Point, "corner", 8, &[0]).unwrap();
        let source = geometry::with_group(corner, &source).unwrap();
        let topology_value = geometry::topology(&source);
        let index = topology_index::create(None, topology_value).unwrap();
        let crease = edge_group::init(None, topology_value, &index, "marked_edge", &|edge| edge == 0).unwrap();
        let source = geometry::with_edge_group(crease, &source).unwrap();
        let cusp = |angle: f64| Params { cusp_angle: Some(angle), ..Params::default() };
        check(same(&facet_ok(cusp(2.), &source), &source), "Facet Cusp Polygons split edges below the threshold");
        check(
            same(&facet_ok(cusp(std::f64::consts::PI / 2.), &source), &source),
            "Facet Cusp Polygons split edges equal to the threshold",
        );
        let hard = facet_ok(cusp(1.), &source);
        check(
            geometry::point_count(&hard) == 24 && geometry::vertex_count(&hard) == 24,
            "Facet Cusp Polygons cube cardinality",
        );
        check(
            group::cardinality(&named_group(group::Owner::Point, "corner", &hard)) == 3,
            "Facet Cusp Polygons point-group ancestry",
        );
        check(
            point_ints("point_id", &hard).iter().filter(|&&value| value == 0).count() == 3,
            "Facet Cusp Polygons point-attribute ancestry",
        );
        check(
            edge_group::cardinality(&geometry::find_edge_group("marked_edge", &hard).unwrap()) == 2,
            "Facet Cusp Polygons one-to-many edge ancestry",
        );
        let shaded = facet_ok(Params { cusp_angle: Some(1.), post_compute_normals: Some(true), ..Params::default() }, &source);
        let normal = normal_attribute(&shaded);
        for point in 0..geometry::point_count(&shaded) as usize {
            let length = ((normal.x[point] * normal.x[point]) + (normal.y[point] * normal.y[point]) + (normal.z[point] * normal.z[point])).sqrt();
            check(near(length, 1.), "Facet cusped post normal");
        }
        for angle in [-0.1, std::f64::consts::PI + 0.1, f64::NAN] {
            expect_code(run_facet(cusp(angle), &source), "invalid_geometry", "Facet accepted an invalid cusp angle");
        }
        let curve = geometry::create(
            f3(vec![0., 1.], vec![0., 0.], vec![0., 0.]),
            topology::create_owned(2, vec![0, 1], vec![0, 2], vec![PrimitiveKind::OpenPolyline]).unwrap(),
            None,
            None,
            None,
        ).unwrap();
        expect_code(run_facet(cusp(1.), &curve), "invalid_geometry", "Facet Cusp Polygons accepted curve topology");
    }

    #[test]
    fn primitive_group_cusp_polygons() {
        let source = cube();
        let selected = primitive_group(&source, &|p| p == 0);
        let output = facet_ok(Params { selection: selection(&selected), cusp_angle: Some(1.), ..Params::default() }, &source);
        check(geometry::point_count(&output) == 12, "grouped Facet Cusp Polygons cardinality");
        let topology_view = view_of(&output);
        for vertex in topology_view.primitive_offsets[0]..topology_view.primitive_offsets[1] {
            check(topology_view.vertex_points[vertex as usize] >= 8, "grouped Facet did not separate a selected cusp corner");
        }
        for primitive in 1..geometry::primitive_count(&output) as usize {
            for vertex in topology_view.primitive_offsets[primitive]..topology_view.primitive_offsets[primitive + 1] {
                check(topology_view.vertex_points[vertex as usize] < 8, "grouped Facet changed an unselected cusp point");
            }
        }
    }

    fn polygon_planarity_error(value: &geometry::T, primitive: usize) -> f64 {
        let view = view_of(value);
        let positions = packed::Float3::View::view(geometry::positions(value)).unwrap();
        let first = view.primitive_offsets[primitive] as usize;
        let last = view.primitive_offsets[primitive + 1] as usize;
        if last - first <= 3 {
            return 0.0;
        }
        let point = |index: usize| view.vertex_points[first + index] as usize;
        let (a, b, c) = (point(0), point(1), point(2));
        let (ux, uy, uz) = (positions.x[b] - positions.x[a], positions.y[b] - positions.y[a], positions.z[b] - positions.z[a]);
        let (vx, vy, vz) = (positions.x[c] - positions.x[a], positions.y[c] - positions.y[a], positions.z[c] - positions.z[a]);
        let (nx, ny, nz) = ((uy * vz) - (uz * vy), (uz * vx) - (ux * vz), (ux * vy) - (uy * vx));
        let length = ((nx * nx) + (ny * ny) + (nz * nz)).sqrt();
        let mut maximum = 0.0_f64;
        if length > 0.0 {
            for vertex in first + 3..last {
                let p = view.vertex_points[vertex] as usize;
                let distance = (((positions.x[p] - positions.x[a]) * nx)
                    + ((positions.y[p] - positions.y[a]) * ny)
                    + ((positions.z[p] - positions.z[a]) * nz))
                    .abs()
                    / length;
                if distance > maximum {
                    maximum = distance;
                }
            }
        }
        maximum
    }

    fn warped_quads() -> geometry::T {
        let positions = f3(
            vec![0., 1., 1., 0., 0., -1.],
            vec![0., 0., 1., 1., 2., 2.],
            vec![0., 0., 0.6, 0., 0.4, 0.],
        );
        let point_id = attribute::create_owned("point_id", attribute::Owner::Point, attribute::Storage::Int((0..6).collect())).unwrap();
        build(positions, polygons(6, vec![0, 1, 2, 3, 0, 3, 4, 5], vec![0, 4, 8]), vec![point_id], Vec::new())
    }

    #[test]
    fn make_planar() {
        let source = warped_quads();
        let selected = primitive_group(&source, &|p| p == 0);
        let planar = || Params { make_planar: Some(true), ..Params::default() };
        let output = facet_ok(Params { selection: selection(&selected), ..planar() }, &source);
        check(
            geometry::point_count(&output) == 8 && polygon_planarity_error(&output, 0) <= 1e-12,
            "grouped Facet Make Planar did not flatten the selected polygon",
        );
        check(
            polygon_planarity_error(&output, 1) == polygon_planarity_error(&source, 1),
            "grouped Facet Make Planar moved an unselected polygon",
        );
        let topology_view = view_of(&output);
        check(
            topology_view.vertex_points[0] >= 6
                && topology_view.vertex_points[3] >= 6
                && topology_view.vertex_points[4..8] == [0, 3, 4, 5],
            "Facet Make Planar did not isolate shared projected corners",
        );
        let ids = point_ints("point_id", &output);
        check(
            ids[topology_view.vertex_points[0] as usize] == 0 && ids[topology_view.vertex_points[3] as usize] == 3,
            "Facet Make Planar point ancestry",
        );
        let planar_topology = polygons(4, vec![0, 1, 2, 3], vec![0, 4]);
        let planar_geometry = geometry::create(
            f3(vec![0., 1., 1., 0.], vec![0., 0., 1., 1.], vec![0.; 4]),
            planar_topology.clone(),
            None,
            None,
            None,
        ).unwrap();
        check(same(&facet_ok(planar(), &planar_geometry), &planar_geometry), "Facet Make Planar changed an already planar polygon");
        let curve = geometry::create(
            f3(vec![0., 1., 2., 3.], vec![0., 0., 1., 0.], vec![0., 1., 0., 1.]),
            topology::create_owned(4, vec![0, 1, 2, 3], vec![0, 4], vec![PrimitiveKind::OpenPolyline]).unwrap(),
            None,
            None,
            None,
        ).unwrap();
        check(same(&facet_ok(planar(), &curve), &curve), "Facet Make Planar changed a polygon curve");
        let degenerate = geometry::create(f3(vec![0., 1., 2., 3.], vec![0.; 4], vec![0.; 4]), planar_topology, None, None, None).unwrap();
        expect_code(run_facet(planar(), &degenerate), "invalid_geometry", "Facet Make Planar accepted a plane-less polygon");
        let nonfinite = geometry::create(
            f3(
                vec![0., 1., 1., 0., 2., 3., 3., 2.],
                vec![0., 0., 1., 1., 0., 0., 1., 1.],
                vec![0., 0., 0., 0., 0., 0., f64::NAN, 0.],
            ),
            polygons(8, (0..8).collect(), vec![0, 4, 8]),
            None,
            None,
            None,
        ).unwrap();
        let finite_only = primitive_group(&nonfinite, &|p| p == 0);
        check(
            same(&facet_ok(Params { selection: selection(&finite_only), ..planar() }, &nonfinite), &nonfinite),
            "Facet Make Planar inspected an unselected non-finite polygon",
        );
        let all = primitive_group(&nonfinite, &|_| true);
        expect_code(
            run_facet(Params { selection: selection(&all), ..planar() }, &nonfinite),
            "invalid_geometry",
            "Facet Make Planar accepted a selected non-finite polygon",
        );
    }

    #[test]
    fn primitive_group_normals_and_cleanup() {
        let n = |values: Vec<f64>| {
            let len = values.len();
            attribute::Storage::Float3(f3(values, vec![0.; len], vec![0.; len]))
        };
        let source = add_attribute(
            attribute::Owner::Point,
            "N",
            n(vec![1., 2., 3., 4., f64::NAN]),
            &source(),
        );
        let source = add_attribute(attribute::Owner::Vertex, "N", n(vec![1., 2., 3., 4., 5., 6.]), &source);
        let selected = primitive_group(&source, &|p| p == 0);
        let output = facet_ok(
            Params { selection: selection(&selected), reverse_normals: Some(true), ..Params::default() },
            &source,
        );
        let point_normal = normal_attribute(&output);
        check(
            point_normal.x[0] == -1.
                && point_normal.x[1] == -2.
                && point_normal.x[2] == -3.
                && point_normal.x[3] == 4.
                && point_normal.x[4].is_nan(),
            "grouped Facet point-normal ownership",
        );
        match attribute::View::storage(&geometry::find_attribute(attribute::Owner::Vertex, "N", &output).unwrap()).unwrap() {
            attribute::Storage::Float3(values) => {
                check(
                    packed::Float3::View::view(values).unwrap().x[..] == [-1., -2., -3., 4., 5., 6.],
                    "grouped Facet vertex-normal ownership",
                );
            }
            _ => panic!("grouped Facet vertex N has wrong storage"),
        }
        let face_id = attribute::create_owned("face_id", attribute::Owner::Primitive, attribute::Storage::Int(vec![10, 20])).unwrap();
        let degenerate = build(
            f3(vec![0., 1., 2., 0., 1., 2.], vec![0., 0., 0., 1., 1., 1.], vec![0.; 6]),
            polygons(6, (0..6).collect(), vec![0, 3, 6]),
            vec![face_id],
            Vec::new(),
        );
        let selected = primitive_group(&degenerate, &|p| p == 0);
        let output = facet_ok(
            Params {
                selection: selection(&selected),
                remove_degenerate: Some(true),
                post_compute_normals: Some(true),
                reverse_normals: Some(true),
                ..Params::default()
            },
            &degenerate,
        );
        check(geometry::primitive_count(&output) == 1, "grouped Facet degenerate cleanup cardinality");
        check(
            int_attr(attribute::Owner::Primitive, "face_id", &output) == [20],
            "grouped Facet degenerate cleanup ancestry",
        );
        let geometry_value = geometry::create(
            f3(vec![0., 0., 1., 0., 2., 3.], vec![0.; 6], vec![0., 0., 1., 0., 1., 1.]),
            polygons(6, (0..6).collect(), vec![0, 3, 6]),
            None,
            None,
            None,
        ).unwrap();
        let selected = primitive_group(&geometry_value, &|p| p == 0);
        let fused = facet_ok(
            Params { selection: selection(&selected), consolidate_distance: Some(0.), ..Params::default() },
            &geometry_value,
        );
        check(geometry::point_count(&fused) == 5, "grouped Facet point consolidation cardinality");
        let fused_view = view_of(&fused);
        check(
            fused_view.vertex_points[0] == fused_view.vertex_points[1] && fused_view.vertex_points[3] != fused_view.vertex_points[0],
            "grouped Facet point consolidation crossed its selection boundary",
        );
    }

    #[test]
    fn validation() {
        let source = source();
        check(same(&facet_ok(Params::default(), &source), &source), "Facet default is not identity");
        expect_code(
            run_facet(Params { consolidate_distance: Some(-1.), ..Params::default() }, &source),
            "invalid_geometry",
            "Facet accepted a negative consolidation distance",
        );
        let wrong = add_attribute(attribute::Owner::Point, "N", attribute::Storage::Float(vec![1.; 5]), &source);
        expect_code(
            run_facet(Params { make_normals_unit_length: Some(true), ..Params::default() }, &wrong),
            "invalid_geometry",
            "Facet accepted non-vector normals",
        );
        let cancelled = Arc::new(cancel::create());
        cancel::cancel(&cancelled);
        let with_cancel = |params: Params| Params { cancel: Some(cancelled.clone()), ..params };
        let cancelled_code = |params: Params, value: &geometry::T, message: &str| {
            expect_code(run_facet(with_cancel(params), value), "cancelled", message)
        };
        cancelled_code(Params { unique_points: Some(true), ..Params::default() }, &source, "Facet ignored cancellation");
        cancelled_code(Params { remove_inline_points: Some(true), ..Params::default() }, &source, "Facet inline removal ignored cancellation");
        cancelled_code(
            Params { consolidate_normals_distance: Some(0.), ..Params::default() },
            &source,
            "Facet normal consolidation ignored cancellation",
        );
        cancelled_code(Params { make_planar: Some(true), ..Params::default() }, &warped_quads(), "Facet Make Planar ignored cancellation");
        let selected_points = group::init(None, group::Owner::Point, "selected", 5, &|p| p == 0).unwrap();
        cancelled_code(
            Params {
                selection: Some(element_selection::T::SelectedPoints(selected_points)),
                unique_points: Some(true),
                ..Params::default()
            },
            &source,
            "Facet typed selection promotion ignored cancellation",
        );
        let wrong_owner = group::init(None, group::Owner::Point, "wrong", 5, &|_| true).unwrap();
        let wrong_length = group::init(None, group::Owner::Primitive, "wrong", 1, &|_| true).unwrap();
        for primitives in [wrong_owner, wrong_length] {
            expect_code(
                run_facet(
                    Params {
                        selection: Some(element_selection::T::SelectedPrimitives(primitives)),
                        unique_points: Some(true),
                        ..Params::default()
                    },
                    &source,
                ),
                "invalid_geometry",
                "Facet accepted a malformed primitive selection",
            );
        }
        let collision = group::init(None, group::Owner::Primitive, "__rdk_facet_selection_0", 2, &|p| p == 1).unwrap();
        let collision_source = geometry::with_group(collision.clone(), &source).unwrap();
        let selected = primitive_group(&collision_source, &|p| p == 0);
        let output = facet_ok(
            Params { selection: selection(&selected), unique_points: Some(true), ..Params::default() },
            &collision_source,
        );
        let kept = geometry::find_group(group::Owner::Primitive, "__rdk_facet_selection_0", &output)
            .expect("Facet removed a colliding user primitive group");
        check(equal_group(&kept, &collision), "Facet replaced a colliding user primitive group");
    }

    /// Triangle grid of `columns` x `rows` cells with `(columns + 1) * (rows + 1)` points.
    fn triangle_grid(columns: i64, rows: i64, size: f64) -> geometry::T {
        let mut x = Vec::new();
        let mut y = Vec::new();
        for row in 0..=rows {
            for column in 0..=columns {
                x.push((column as f64 / columns as f64 - 0.5) * size);
                y.push((row as f64 / rows as f64 - 0.5) * size);
            }
        }
        let stride = columns + 1;
        let mut vertex_points = Vec::new();
        for row in 0..rows {
            for column in 0..columns {
                let a = row * stride + column;
                vertex_points.extend_from_slice(&[a, a + 1, a + stride + 1, a, a + stride + 1, a + stride]);
            }
        }
        let primitives = vertex_points.len() as i64 / 3;
        let count = x.len();
        geometry::create(
            f3(x, y, vec![0.; count]),
            polygons(count as i64, vertex_points, (0..=primitives).map(|p| p * 3).collect()),
            None,
            None,
            None,
        ).unwrap()
    }

    fn repeated_inline_source(polygons_count: i64) -> geometry::T {
        let points = (polygons_count * 6) as usize;
        let (mut x, y, mut z) = (vec![0.0_f64; points], vec![0.0_f64; points], vec![0.0_f64; points]);
        for primitive in 0..polygons_count as usize {
            let point = primitive * 6;
            let base = primitive as f64 * 4.0;
            x[point] = base;
            x[point + 1] = base + 1.0;
            x[point + 2] = base + 2.0;
            x[point + 3] = base + 3.0;
            x[point + 4] = base + 3.0;
            z[point + 4] = 1.0;
            x[point + 5] = base;
            z[point + 5] = 1.0;
        }
        let point_id = attribute::create_owned("point_id", attribute::Owner::Point, attribute::Storage::Int((0..points as i64).collect())).unwrap();
        build(
            f3(x, y, z),
            polygons(points as i64, (0..points as i64).collect(), (0..=polygons_count).map(|p| p * 6).collect()),
            vec![point_id],
            Vec::new(),
        )
    }

    fn repeated_normal_source(pairs: usize) -> geometry::T {
        let count = pairs * 2;
        let (mut x, y, z) = (vec![0.0_f64; count], vec![0.0_f64; count], vec![0.0_f64; count]);
        let (mut nx, mut ny, nz) = (vec![0.0_f64; count], vec![0.0_f64; count], vec![0.0_f64; count]);
        for pair in 0..pairs {
            let point = pair * 2;
            let base = pair as f64 * 2.0;
            x[point] = base;
            x[point + 1] = base + 0.0001;
            nx[point] = 1.0;
            ny[point + 1] = 1.0;
        }
        let normal = attribute::create_owned("N", attribute::Owner::Point, attribute::Storage::Float3(f3(nx, ny, nz))).unwrap();
        build(
            f3(x, y, z),
            topology::View::create_validated_owned(count as i64, Vec::new(), vec![0], Vec::new()),
            vec![normal],
            Vec::new(),
        )
    }

    fn repeated_warped_quads(count: i64) -> geometry::T {
        let point_count = (count * 4) as usize;
        let (mut x, mut y, mut z) = (vec![0.0_f64; point_count], vec![0.0_f64; point_count], vec![0.0_f64; point_count]);
        for primitive in 0..count as usize {
            let point = primitive * 4;
            let base = primitive as f64 * 2.0;
            x[point] = base;
            x[point + 1] = base + 1.0;
            x[point + 2] = base + 1.0;
            y[point + 2] = 1.0;
            y[point + 3] = 1.0;
            z[point + 2] = if primitive & 1 == 0 { 0.25 } else { -0.25 };
        }
        geometry::create(
            f3(x, y, z),
            polygons(point_count as i64, (0..point_count as i64).collect(), (0..=count).map(|p| p * 4).collect()),
            None,
            None,
            None,
        ).unwrap()
    }

    #[test]
    fn parallel_exact() {
        let source = triangle_grid(300, 240, 20.0);
        let (one, many) = one_four(&|| {
            facet_ok(
                Params {
                    grain: Some(257),
                    pre_compute_normals: Some(true),
                    unique_points: Some(true),
                    make_normals_unit_length: Some(true),
                    reverse_normals: Some(true),
                    ..Params::default()
                },
                &source,
            )
        });
        check(equal_geometry(&one, &many), "one-domain and four-domain Facet geometry differ");
        let inline_count = 40_000;
        let inline = repeated_inline_source(inline_count);
        let (one, many) =
            one_four(&|| facet_ok(Params { grain: Some(257), ..inline_params(None) }, &inline));
        check(
            geometry::point_count(&one) == 4 * inline_count && geometry::vertex_count(&one) == 4 * inline_count,
            "Facet Remove Inline Points parallel cardinality",
        );
        check(equal_geometry(&one, &many), "one-domain and four-domain Facet inline removal differ");
        let pairs = 200_000;
        let normal_source = repeated_normal_source(pairs);
        let (one, many) = one_four(&|| {
            facet_ok(
                Params { grain: Some(257), consolidate_normals_distance: Some(0.001), ..Params::default() },
                &normal_source,
            )
        });
        check(equal_geometry(&one, &many), "one-domain and four-domain Facet normal consolidation differ");
        let normal = normal_attribute(&one);
        check(
            near(normal.x[0], 0.5) && near(normal.y[0], 0.5) && near(normal.x[2 * pairs - 1], 0.5) && near(normal.y[2 * pairs - 1], 0.5),
            "Facet parallel consolidated-normal values",
        );
        let source_view = view_of(&source);
        let mut vertex_points = source_view.vertex_points.to_vec();
        for primitive in 1..geometry::primitive_count(&source) as usize {
            if primitive & 1 == 1 {
                let first = source_view.primitive_offsets[primitive] as usize;
                let last = source_view.primitive_offsets[primitive + 1] as usize;
                for local in 0..(last - first) / 2 {
                    vertex_points.swap(first + local, last - local - 1);
                }
            }
        }
        let inconsistent = geometry::create(
            geometry::positions(&source).clone(),
            topology::View::create_validated_owned(
                source_view.point_count,
                vertex_points,
                source_view.primitive_offsets.to_vec(),
                source_view.primitive_kinds.to_vec(),
            ),
            Some(geometry::attributes(&source)),
            Some(geometry::groups(&source)),
            None,
        ).unwrap();
        let (one, many) = one_four(&|| {
            facet_ok(Params { grain: Some(257), orient_polygons: Some(true), ..Params::default() }, &inconsistent)
        });
        check(equal_geometry(&one, &many), "one-domain and four-domain Facet orientation differ");
        check(!equal_geometry(&one, &inconsistent), "Facet orientation left the inconsistent grid unchanged");
        let displaced = {
            let flat = triangle_grid(240, 180, 20.0);
            let p = packed::Float3::View::view(geometry::positions(&flat)).unwrap();
            let z: Vec<f64> = (0..p.x.len()).map(|i| 0.8 * (3.0 * p.x[i]).sin() * (3.0 * p.y[i]).cos()).collect();
            geometry::with_positions(f3(p.x.to_vec(), p.y.to_vec(), z), &flat).unwrap()
        };
        let (one, many) =
            one_four(&|| facet_ok(Params { grain: Some(257), cusp_angle: Some(0.08), ..Params::default() }, &displaced));
        check(equal_geometry(&one, &many), "one-domain and four-domain Facet cusping differ");
        check(geometry::point_count(&one) > geometry::point_count(&displaced), "Facet cusping split no displaced edges");
        let selected = primitive_group(&source, &|p| p & 1 == 0);
        let (one, many) = one_four(&|| {
            facet_ok(
                Params {
                    grain: Some(257),
                    selection: selection(&selected),
                    pre_compute_normals: Some(true),
                    unique_points: Some(true),
                    reverse_normals: Some(true),
                    ..Params::default()
                },
                &source,
            )
        });
        check(equal_geometry(&one, &many), "one-domain and four-domain grouped Facet geometry differ");
        let half = geometry::point_count(&source) / 2;
        let selected_points = group::init(None, group::Owner::Point, "facet_left", geometry::point_count(&source), &|p| p < half).unwrap();
        let (one, many) = one_four(&|| {
            facet_ok(
                Params {
                    grain: Some(257),
                    selection: Some(element_selection::T::SelectedPoints(selected_points.clone())),
                    unique_points: Some(true),
                    ..Params::default()
                },
                &source,
            )
        });
        check(equal_geometry(&one, &many), "one-domain and four-domain point-selected Facet geometry differ");
        check(geometry::point_count(&one) > geometry::point_count(&source), "point-selected Facet split no points");
        let topology_value = geometry::topology(&source);
        let index = topology_index::create(None, topology_value).unwrap();
        let selected_edges = edge_group::init(None, topology_value, &index, "facet_edges", &|e| e % 7 == 0).unwrap();
        let (one, many) = one_four(&|| {
            facet_ok(
                Params {
                    grain: Some(257),
                    selection: Some(element_selection::T::SelectedEdges(selected_edges.clone())),
                    unique_points: Some(true),
                    ..Params::default()
                },
                &source,
            )
        });
        check(equal_geometry(&one, &many), "one-domain and four-domain edge-selected Facet geometry differ");
        check(geometry::point_count(&one) > geometry::point_count(&source), "edge-selected Facet split no points");
        let inline_selected = primitive_group(&inline, &|p| p & 1 == 0);
        let (one, many) = one_four(&|| {
            facet_ok(
                Params { grain: Some(257), selection: selection(&inline_selected), ..inline_params(None) },
                &inline,
            )
        });
        check(
            geometry::point_count(&one) == 5 * inline_count && geometry::vertex_count(&one) == 5 * inline_count,
            "grouped Facet inline parallel cardinality",
        );
        check(equal_geometry(&one, &many), "one-domain and four-domain grouped Facet inline removal differ");
        let warped = repeated_warped_quads(100_000);
        let warped_selected = primitive_group(&warped, &|p| p & 1 == 0);
        let (one, many) = one_four(&|| {
            facet_ok(
                Params { grain: Some(257), selection: selection(&warped_selected), make_planar: Some(true), ..Params::default() },
                &warped,
            )
        });
        check(equal_geometry(&one, &many), "one-domain and four-domain grouped Facet Make Planar differ");
        check(
            polygon_planarity_error(&one, 0) <= 1e-12 && polygon_planarity_error(&one, 1) > 0.1,
            "grouped Facet Make Planar parallel selection",
        );
    }
}
