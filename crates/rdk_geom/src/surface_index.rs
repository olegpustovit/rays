//! Deterministic packed polygon-surface acceleration.
//!
//! Construction triangulates finite simple polygons in stable ear-clipping order without
//! materializing another geometry, builds a median-split AABB hierarchy, and retains original
//! primitive/corner identities.
//!
//! Parallel ranges never share an output slot: each range writes its own fixed output window, so
//! every result is byte-identical to the sequential path.

use rdk_core::{cancel, error, geometry, group, packed, polygon_triangulation, support, topology};

use crate::bounds3_index::{self, Extra};
use crate::linear_piece_index::{join_pairs, members, pair_slots, remap};
use crate::point_grid::{float_max, float_min, point_triangle_barycentric_into};
use crate::predicates;

const OPERATION: &str = "surface_index";

pub struct T {
    geometry: geometry::T,
    positions: packed::Float3::View::View,
    topology: topology::View::View,
    primitives: Vec<i64>,
    vertex_a: Vec<i64>,
    vertex_b: Vec<i64>,
    vertex_c: Vec<i64>,
    order: Vec<i64>,
    triangle_min_x: Vec<f64>,
    triangle_min_y: Vec<f64>,
    triangle_min_z: Vec<f64>,
    triangle_max_x: Vec<f64>,
    triangle_max_y: Vec<f64>,
    triangle_max_z: Vec<f64>,
    triangle_min_d: [Vec<f64>; 4],
    triangle_max_d: [Vec<f64>; 4],
    min_x: Vec<f64>,
    min_y: Vec<f64>,
    min_z: Vec<f64>,
    max_x: Vec<f64>,
    max_y: Vec<f64>,
    max_z: Vec<f64>,
    min_d: [Vec<f64>; 4],
    max_d: [Vec<f64>; 4],
    left: Vec<i64>,
    right: Vec<i64>,
    first: Vec<i64>,
    count: Vec<i64>,
    common_a: Vec<i64>,
    common_b: Vec<i64>,
    common_c: Vec<i64>,
    node_count: i64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Hit {
    pub primitive: i64,
    pub distance: f64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VertexSelection {
    AllTriangleVertices,
    AnyTriangleVertex,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RayDirectionMode {
    RayForward,
    RayReverse,
    RayBidirectionalClosest,
    RayBidirectionalFarthest,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RaySurfaceHit {
    RayFirstSurface,
    RayLastSurface,
}

fn fail_code(code: &str, message: String) -> error::T {
    error::make(OPERATION.to_owned(), code.to_owned(), message, None).unwrap_or_else(|bad| bad)
}

fn fail(message: String) -> error::T {
    fail_code("invalid_surface", message)
}

fn invalid_argument(message: &str) -> error::T {
    fail_code("invalid_parameter", message.to_owned())
}

const DIAGONAL_COUNT: usize = 4;

#[inline]
fn diagonal_value(axis: usize, x: f64, y: f64, z: f64) -> f64 {
    match axis {
        0 => x + y + z,
        1 => x + y - z,
        2 => x - y + z,
        _ => -x + y + z,
    }
}

#[inline]
fn diagonal_error(x: f64, y: f64, z: f64) -> f64 {
    (8.0 * f64::EPSILON * (x.abs() + y.abs() + z.abs())) + f64::from_bits(1)
}

#[inline]
fn diagonal_lower(axis: usize, x: f64, y: f64, z: f64) -> f64 {
    let value = diagonal_value(axis, x, y, z);
    if value.is_finite() { value - diagonal_error(x, y, z) } else { f64::NEG_INFINITY }
}

#[inline]
fn diagonal_upper(axis: usize, x: f64, y: f64, z: f64) -> f64 {
    let value = diagonal_value(axis, x, y, z);
    if value.is_finite() { value + diagonal_error(x, y, z) } else { f64::INFINITY }
}

#[inline]
fn selected(bits: &[u8], index: i64) -> bool {
    bits[(index >> 3) as usize] & (1 << (index & 7)) != 0
}

struct Triangles {
    primitives: Vec<i64>,
    vertex_a: Vec<i64>,
    vertex_b: Vec<i64>,
    vertex_c: Vec<i64>,
}

struct Bounds {
    centroid: [Vec<f64>; 3],
    min: [Vec<f64>; 3],
    max: [Vec<f64>; 3],
    min_d: [Vec<f64>; 4],
    max_d: [Vec<f64>; 4],
}

fn empty_bounds(length: usize) -> Bounds {
    Bounds {
        centroid: std::array::from_fn(|_| vec![0.0; length]),
        min: std::array::from_fn(|_| vec![0.0; length]),
        max: std::array::from_fn(|_| vec![0.0; length]),
        min_d: std::array::from_fn(|_| vec![0.0; length]),
        max_d: std::array::from_fn(|_| vec![0.0; length]),
    }
}

fn plane_windows<'a>(planes: &'a mut [Vec<f64>], grain: i64) -> Vec<support::Windows<'a, f64>> {
    planes.iter_mut().map(|plane| support::Windows::even(plane, grain)).collect()
}

fn take_planes<'a>(windows: &[support::Windows<'a, f64>], range: i64) -> Vec<&'a mut [f64]> {
    windows.iter().map(|window| window.take(range)).collect()
}

/// Keeps the entries of `items` (in order) for which `keep` holds, one parallel range at a time.
fn filter_items(
    cancel_value: Option<&cancel::T>,
    chunk: i64,
    items: &[i64],
    keep: &(dyn Fn(i64) -> bool + Sync),
) -> Result<Vec<i64>, error::T> {
    let count = items.len() as i64;
    let mut kept = vec![false; items.len()];
    {
        let windows = support::Windows::even(&mut kept, chunk);
        support::run_indexed_ranges(chunk, count, &|range, first, last| {
            for (at, flag) in (first..last).zip(windows.take(range)) {
                cancel::poll(cancel_value, at, 4095)?;
                *flag = keep(items[at as usize]);
            }
            Ok(())
        })?;
    }
    Ok(items.iter().zip(&kept).filter(|&(_, &flag)| flag).map(|(&item, _)| item).collect())
}

fn create_with(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    selection: Option<&group::T>,
    vertices: Option<&group::T>,
    vertex_selection: VertexSelection,
    validated_triangles: bool,
    geometry_value: &geometry::T,
) -> Result<T, error::T> {
    if grain <= 0 {
        return Err(invalid_argument("Surface_index: grain must be positive"));
    }
    if validated_triangles && (selection.is_some() || vertices.is_some()) {
        return Err(invalid_argument("Surface_index: validated triangle construction cannot select elements"));
    }
    cancel::check_opt(cancel_value)?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let topology_view = topology::View::view(geometry::topology(geometry_value))?;
    let primitive_count = topology_view.primitive_kinds.len() as i64;
    if let Some(vertex_group) = vertices {
        if group::owner(vertex_group) != group::Owner::Vertex {
            return Err(fail_code("invalid_group", "source vertex selection must be a vertex group".to_owned()));
        }
        if group::length(vertex_group) != topology_view.vertex_points.len() as i64 {
            return Err(fail_code(
                "invalid_group",
                "source vertex group length does not match geometry".to_owned(),
            ));
        }
    }
    let mut selected_primitives: Vec<i64> = match selection {
        None => (0..primitive_count).collect(),
        Some(primitive_group) => {
            if group::owner(primitive_group) != group::Owner::Primitive {
                return Err(fail_code("invalid_group", "source selection must be a primitive group".to_owned()));
            }
            if group::length(primitive_group) != primitive_count {
                return Err(fail_code(
                    "invalid_group",
                    "source primitive group length does not match geometry".to_owned(),
                ));
            }
            members(primitive_group)
        }
    };
    if let Some(vertex_group) = vertices {
        let vertex_bits = group::View::bits_view(vertex_group);
        selected_primitives = filter_items(cancel_value, grain.max(1), &selected_primitives, &|primitive| {
            let last = topology_view.primitive_offsets[primitive as usize + 1];
            (topology_view.primitive_offsets[primitive as usize]..last).any(|vertex| selected(vertex_bits, vertex))
        })?;
    }
    let selected_count = selected_primitives.len();
    let triangle_offsets: Vec<i64> = if validated_triangles {
        (0..=selected_count as i64).collect()
    } else {
        let mut offsets = vec![0_i64; selected_count + 1];
        for at in 0..selected_count {
            let primitive = selected_primitives[at];
            cancel::poll(cancel_value, primitive, 4095)?;
            if topology_view.primitive_kinds[primitive as usize] != 0 {
                return Err(fail(format!("primitive {primitive} is a curve, not a polygon")));
            }
            let size = topology_view.primitive_offsets[primitive as usize + 1]
                - topology_view.primitive_offsets[primitive as usize];
            if size < 3 {
                return Err(fail(format!("primitive {primitive} has fewer than three corners")));
            }
            offsets[at + 1] = offsets[at] + size - 2;
        }
        offsets
    };
    let triangle_total = triangle_offsets[selected_count];
    let mut triangles = if validated_triangles {
        let n = triangle_total as usize;
        Triangles {
            primitives: (0..triangle_total).collect(),
            vertex_a: (0..n).map(|t| topology_view.primitive_offsets[t]).collect(),
            vertex_b: (0..n).map(|t| topology_view.primitive_offsets[t] + 1).collect(),
            vertex_c: (0..n).map(|t| topology_view.primitive_offsets[t] + 2).collect(),
        }
    } else {
        let average = if selected_count == 0 {
            1
        } else {
            support::ceiling_div(triangle_total, selected_count as i64)?.max(1)
        };
        let primitive_chunk = (grain / average).max(1);
        let selected_total = selected_count as i64;
        let ranges = support::range_count(selected_total, primitive_chunk);
        let n = triangle_total as usize;
        let mut joined = Triangles { primitives: vec![0; n], vertex_a: vec![0; n], vertex_b: vec![0; n], vertex_c: vec![0; n] };
        let mut failures: Vec<Option<String>> = vec![None; ranges as usize];
        {
            let bound = |range: i64| triangle_offsets[(range * primitive_chunk).min(selected_total) as usize] as usize;
            let (primitive_windows, a_windows, b_windows, c_windows) = (
                support::Windows::new(&mut joined.primitives, ranges, bound),
                support::Windows::new(&mut joined.vertex_a, ranges, bound),
                support::Windows::new(&mut joined.vertex_b, ranges, bound),
                support::Windows::new(&mut joined.vertex_c, ranges, bound),
            );
            let failure_windows = support::Windows::even(&mut failures, 1);
            support::run_indexed_ranges(primitive_chunk, selected_total, &|range, first_selected, last_selected| {
                let base = triangle_offsets[first_selected as usize];
                let (primitives, vertex_a, vertex_b, vertex_c) =
                    (primitive_windows.take(range), a_windows.take(range), b_windows.take(range), c_windows.take(range));
                let failure = &mut failure_windows.take(range)[0];
                let mut scratch = polygon_triangulation::create_scratch();
                for at in first_selected..last_selected {
                    cancel::poll(cancel_value, at, 4095)?;
                    if failure.is_some() {
                        continue;
                    }
                    let primitive = selected_primitives[at as usize];
                    let offset = (triangle_offsets[at as usize] - base) as usize;
                    let result = polygon_triangulation::primitive(
                        cancel_value,
                        &positions,
                        &topology_view,
                        Some(&mut scratch),
                        primitive,
                        &mut |index, a, b, c| {
                            let triangle = offset + index as usize;
                            primitives[triangle] = primitive;
                            vertex_a[triangle] = a;
                            vertex_b[triangle] = b;
                            vertex_c[triangle] = c;
                        },
                    );
                    if let Err(problem) = result {
                        if error::code(&problem) == "cancelled" {
                            return Err(problem);
                        }
                        *failure = Some(error::message(&problem).to_owned());
                    }
                }
                Ok(())
            })?;
        }
        if let Some(message) = failures.into_iter().flatten().next() {
            return Err(fail(message));
        }
        joined
    };
    if let Some(vertex_group) = vertices {
        let bits = group::View::bits_view(vertex_group);
        let count = triangles.primitives.len() as i64;
        let mut keep = vec![false; count as usize];
        {
            let windows = support::Windows::even(&mut keep, grain);
            support::run_indexed_ranges(grain, count, &|range, first, last| {
                for (triangle, flag) in (first..last).zip(windows.take(range)) {
                    cancel::poll(cancel_value, triangle, 4095)?;
                    let t = triangle as usize;
                    let a = selected(bits, triangles.vertex_a[t]);
                    let b = selected(bits, triangles.vertex_b[t]);
                    let c = selected(bits, triangles.vertex_c[t]);
                    *flag = match vertex_selection {
                        VertexSelection::AllTriangleVertices => a && b && c,
                        VertexSelection::AnyTriangleVertex => a || b || c,
                    };
                }
                Ok(())
            })?;
        }
        // ponytail: the kept count is only known after the mask, so compaction is one sequential pass.
        let mut joined = Triangles {
            primitives: Vec::new(),
            vertex_a: Vec::new(),
            vertex_b: Vec::new(),
            vertex_c: Vec::new(),
        };
        for t in (0..keep.len()).filter(|&t| keep[t]) {
            joined.primitives.push(triangles.primitives[t]);
            joined.vertex_a.push(triangles.vertex_a[t]);
            joined.vertex_b.push(triangles.vertex_b[t]);
            joined.vertex_c.push(triangles.vertex_c[t]);
        }
        triangles = joined;
    }
    let count = triangles.primitives.len();
    let total = count as i64;
    let vertex_points = &topology_view.vertex_points;
    let mut bounds = empty_bounds(count);
    let mut failures: Vec<Option<String>> = vec![None; support::range_count(total, grain) as usize];
    {
        let Bounds { centroid, min, max, min_d, max_d } = &mut bounds;
        let (centroid_windows, min_windows, max_windows) =
            (plane_windows(centroid, grain), plane_windows(min, grain), plane_windows(max, grain));
        let (min_d_windows, max_d_windows) = (plane_windows(min_d, grain), plane_windows(max_d, grain));
        let failure_windows = support::Windows::even(&mut failures, 1);
        support::run_indexed_ranges(grain, total, &|range, first_slot, last_slot| {
            let (mut centroid, mut min, mut max) = (
                take_planes(&centroid_windows, range),
                take_planes(&min_windows, range),
                take_planes(&max_windows, range),
            );
            let (mut min_d, mut max_d) = (take_planes(&min_d_windows, range), take_planes(&max_d_windows, range));
            let failure = &mut failure_windows.take(range)[0];
            for slot in first_slot..last_slot {
                cancel::poll(cancel_value, slot, 4095)?;
                if failure.is_some() {
                    continue;
                }
                let s = slot as usize;
                let at = s - first_slot as usize;
                let primitive = triangles.primitives[s];
                let a = vertex_points[triangles.vertex_a[s] as usize];
                let b = vertex_points[triangles.vertex_b[s] as usize];
                let c = vertex_points[triangles.vertex_c[s] as usize];
                let (ax, ay, az) = (positions.x[a as usize], positions.y[a as usize], positions.z[a as usize]);
                let (bx, by, bz) = (positions.x[b as usize], positions.y[b as usize], positions.z[b as usize]);
                let (cx, cy, cz) = (positions.x[c as usize], positions.y[c as usize], positions.z[c as usize]);
                if !validated_triangles
                    && !(ax.is_finite()
                        && ay.is_finite()
                        && az.is_finite()
                        && bx.is_finite()
                        && by.is_finite()
                        && bz.is_finite()
                        && cx.is_finite()
                        && cy.is_finite()
                        && cz.is_finite())
                {
                    *failure = Some(format!("primitive {primitive} has a non-finite position"));
                    continue;
                }
                let degenerate = if validated_triangles {
                    false
                } else {
                    let xy = predicates::orient2d_packed(&positions.x, &positions.y, a, b, c);
                    let yz = predicates::orient2d_packed(&positions.y, &positions.z, a, b, c);
                    let zx = predicates::orient2d_packed(&positions.z, &positions.x, a, b, c);
                    xy == predicates::Sign::Zero && yz == predicates::Sign::Zero && zx == predicates::Sign::Zero
                };
                if degenerate {
                    *failure = Some(format!("primitive {primitive} is degenerate"));
                    continue;
                }
                centroid[0][at] = (ax + bx + cx) / 3.0;
                centroid[1][at] = (ay + by + cy) / 3.0;
                centroid[2][at] = (az + bz + cz) / 3.0;
                min[0][at] = float_min(ax, float_min(bx, cx));
                min[1][at] = float_min(ay, float_min(by, cy));
                min[2][at] = float_min(az, float_min(bz, cz));
                max[0][at] = float_max(ax, float_max(bx, cx));
                max[1][at] = float_max(ay, float_max(by, cy));
                max[2][at] = float_max(az, float_max(bz, cz));
                for axis in 0..DIAGONAL_COUNT {
                    min_d[axis][at] = float_min(
                        diagonal_lower(axis, ax, ay, az),
                        float_min(diagonal_lower(axis, bx, by, bz), diagonal_lower(axis, cx, cy, cz)),
                    );
                    max_d[axis][at] = float_max(
                        diagonal_upper(axis, ax, ay, az),
                        float_max(diagonal_upper(axis, bx, by, bz), diagonal_upper(axis, cx, cy, cz)),
                    );
                }
            }
            Ok(())
        })?;
    }
    if let Some(message) = failures.into_iter().flatten().next() {
        return Err(fail(message));
    }
    let extend = |order: &[i64]| -> Extra {
        let mut min_d = [f64::INFINITY; 4];
        let mut max_d = [f64::NEG_INFINITY; 4];
        let first_triangle = order[0] as usize;
        let mut common = [
            vertex_points[triangles.vertex_a[first_triangle] as usize],
            vertex_points[triangles.vertex_b[first_triangle] as usize],
            vertex_points[triangles.vertex_c[first_triangle] as usize],
        ];
        for &item in order {
            let triangle = item as usize;
            let a = vertex_points[triangles.vertex_a[triangle] as usize];
            let b = vertex_points[triangles.vertex_b[triangle] as usize];
            let c = vertex_points[triangles.vertex_c[triangle] as usize];
            for slot in common.iter_mut() {
                if !(*slot < 0 || *slot == a || *slot == b || *slot == c) {
                    *slot = -1;
                }
            }
            for axis in 0..DIAGONAL_COUNT {
                if bounds.min_d[axis][triangle] < min_d[axis] {
                    min_d[axis] = bounds.min_d[axis][triangle];
                }
                if bounds.max_d[axis][triangle] > max_d[axis] {
                    max_d[axis] = bounds.max_d[axis][triangle];
                }
            }
        }
        Extra { common, min_d, max_d }
    };
    let tree = bounds3_index::create(
        cancel_value,
        Some(&extend),
        grain.max(4_096),
        &bounds.centroid[0],
        &bounds.centroid[1],
        &bounds.centroid[2],
        &bounds.min[0],
        &bounds.min[1],
        &bounds.min[2],
        &bounds.max[0],
        &bounds.max[1],
        &bounds.max[2],
    )?;
    let node_count = tree.left.len();
    let node_min_d: [Vec<f64>; 4] = std::array::from_fn(|axis| tree.extra.iter().map(|e| e.min_d[axis]).collect());
    let node_max_d: [Vec<f64>; 4] = std::array::from_fn(|axis| tree.extra.iter().map(|e| e.max_d[axis]).collect());
    let [min_x, min_y, min_z] = bounds.min;
    let [max_x, max_y, max_z] = bounds.max;
    Ok(T {
        geometry: geometry_value.clone(),
        positions,
        topology: topology_view,
        primitives: triangles.primitives,
        vertex_a: triangles.vertex_a,
        vertex_b: triangles.vertex_b,
        vertex_c: triangles.vertex_c,
        order: tree.order,
        triangle_min_x: min_x,
        triangle_min_y: min_y,
        triangle_min_z: min_z,
        triangle_max_x: max_x,
        triangle_max_y: max_y,
        triangle_max_z: max_z,
        triangle_min_d: bounds.min_d,
        triangle_max_d: bounds.max_d,
        min_x: tree.min_x,
        min_y: tree.min_y,
        min_z: tree.min_z,
        max_x: tree.max_x,
        max_y: tree.max_y,
        max_z: tree.max_z,
        min_d: node_min_d,
        max_d: node_max_d,
        left: tree.left,
        right: tree.right,
        first: tree.first,
        count: tree.count,
        common_a: tree.extra.iter().map(|e| e.common[0]).collect(),
        common_b: tree.extra.iter().map(|e| e.common[1]).collect(),
        common_c: tree.extra.iter().map(|e| e.common[2]).collect(),
        node_count: node_count as i64,
    })
}

fn create_remapped(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    selection: Option<&group::T>,
    vertices: Option<&group::T>,
    vertex_selection: VertexSelection,
    validated_triangles: bool,
    geometry_value: &geometry::T,
) -> Result<T, error::T> {
    create_with(
        cancel_value,
        grain.unwrap_or(16_384),
        selection,
        vertices,
        vertex_selection,
        validated_triangles,
        geometry_value,
    )
    .map_err(|failure| remap(failure, OPERATION, "surface-index construction was cancelled"))
}

/// `primitives` restricts construction to a matching primitive group. `vertices` additionally
/// retains emitted triangles whose source corners all or any belong to a matching vertex group.
/// Primitives without a selected corner are skipped before polygon validation. An empty eligible
/// surface is valid and every query misses.
pub fn create(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    primitives: Option<&group::T>,
    vertices: Option<&group::T>,
    vertex_selection: Option<VertexSelection>,
    geometry_value: &geometry::T,
) -> Result<T, error::T> {
    create_remapped(
        cancel_value,
        grain,
        primitives,
        vertices,
        vertex_selection.unwrap_or(VertexSelection::AllTriangleVertices),
        false,
        geometry_value,
    )
}

pub fn triangle_count(value: &T) -> i64 {
    value.primitives.len() as i64
}

fn triangle_bounds_into(value: &T, triangle: usize, output: &mut [f64; 15]) {
    output[0] = value.triangle_min_x[triangle];
    output[1] = value.triangle_min_y[triangle];
    output[2] = value.triangle_min_z[triangle];
    output[3] = value.triangle_max_x[triangle];
    output[4] = value.triangle_max_y[triangle];
    output[5] = value.triangle_max_z[triangle];
    for axis in 0..DIAGONAL_COUNT {
        output[7 + axis] = value.triangle_min_d[axis][triangle];
        output[11 + axis] = value.triangle_max_d[axis][triangle];
    }
}

#[inline]
fn node_bounds_overlap(query: &[f64; 15], surface: &T, node: usize) -> bool {
    let tolerance = query[6];
    query[0] <= surface.max_x[node] + tolerance
        && query[3] >= surface.min_x[node] - tolerance
        && query[1] <= surface.max_y[node] + tolerance
        && query[4] >= surface.min_y[node] - tolerance
        && query[2] <= surface.max_z[node] + tolerance
        && query[5] >= surface.min_z[node] - tolerance
        && query[7] <= surface.max_d[0][node] + tolerance
        && query[11] >= surface.min_d[0][node] - tolerance
        && query[8] <= surface.max_d[1][node] + tolerance
        && query[12] >= surface.min_d[1][node] - tolerance
        && query[9] <= surface.max_d[2][node] + tolerance
        && query[13] >= surface.min_d[2][node] - tolerance
        && query[10] <= surface.max_d[3][node] + tolerance
        && query[14] >= surface.min_d[3][node] - tolerance
}

#[inline]
fn triangle_bounds_overlap(query: &[f64; 15], surface: &T, triangle: usize) -> bool {
    let tolerance = query[6];
    query[0] <= surface.triangle_max_x[triangle] + tolerance
        && query[3] >= surface.triangle_min_x[triangle] - tolerance
        && query[1] <= surface.triangle_max_y[triangle] + tolerance
        && query[4] >= surface.triangle_min_y[triangle] - tolerance
        && query[2] <= surface.triangle_max_z[triangle] + tolerance
        && query[5] >= surface.triangle_min_z[triangle] - tolerance
        && query[7] <= surface.triangle_max_d[0][triangle] + tolerance
        && query[11] >= surface.triangle_min_d[0][triangle] - tolerance
        && query[8] <= surface.triangle_max_d[1][triangle] + tolerance
        && query[12] >= surface.triangle_min_d[1][triangle] - tolerance
        && query[9] <= surface.triangle_max_d[2][triangle] + tolerance
        && query[13] >= surface.triangle_min_d[2][triangle] - tolerance
        && query[10] <= surface.triangle_max_d[3][triangle] + tolerance
        && query[14] >= surface.triangle_min_d[3][triangle] - tolerance
}

fn corner_point(surface: &T, triangle: usize, local: i64) -> i64 {
    let vertex = match local {
        0 => surface.vertex_a[triangle],
        1 => surface.vertex_b[triangle],
        _ => surface.vertex_c[triangle],
    };
    surface.topology.vertex_points[vertex as usize]
}

fn triangle_strictly_on_one_side(plane_surface: &T, plane: usize, triangle_surface: &T, triangle: usize) -> bool {
    let a = corner_point(plane_surface, plane, 0);
    let b = corner_point(plane_surface, plane, 1);
    let c = corner_point(plane_surface, plane, 2);
    let same = std::ptr::eq(plane_surface, triangle_surface);
    let sign = |local: i64| -> predicates::Sign {
        let d = corner_point(triangle_surface, triangle, local);
        let p = &plane_surface.positions;
        if same {
            predicates::orient3d_packed(&p.x, &p.y, &p.z, a, b, c, d)
        } else {
            let t = &triangle_surface.positions;
            let (a, b, c, d) = (a as usize, b as usize, c as usize, d as usize);
            predicates::orient3d(
                p.x[a], p.y[a], p.z[a], p.x[b], p.y[b], p.z[b], p.x[c], p.y[c], p.z[c], t.x[d], t.y[d], t.z[d],
            )
        }
    };
    match sign(0) {
        predicates::Sign::Zero => false,
        first => sign(1) == first && sign(2) == first,
    }
}

fn exact_plane_overlap(left_surface: &T, left_triangle: usize, right_surface: &T, right_triangle: usize) -> bool {
    !(triangle_strictly_on_one_side(left_surface, left_triangle, right_surface, right_triangle)
        || triangle_strictly_on_one_side(right_surface, right_triangle, left_surface, left_triangle))
}

fn triangle_has_point(surface: &T, triangle: usize, point: i64) -> bool {
    if point < 0 {
        return false;
    }
    let points = &surface.topology.vertex_points;
    point == points[surface.vertex_a[triangle] as usize]
        || point == points[surface.vertex_b[triangle] as usize]
        || point == points[surface.vertex_c[triangle] as usize]
}

fn triangles_share_point(left_surface: &T, left_triangle: usize, right_surface: &T, right_triangle: usize) -> bool {
    let points = &left_surface.topology.vertex_points;
    triangle_has_point(right_surface, right_triangle, points[left_surface.vertex_a[left_triangle] as usize])
        || triangle_has_point(right_surface, right_triangle, points[left_surface.vertex_b[left_triangle] as usize])
        || triangle_has_point(right_surface, right_triangle, points[left_surface.vertex_c[left_triangle] as usize])
}

#[allow(clippy::too_many_arguments)]
fn triangle_pairs_of(
    cancel_value: Option<&cancel::T>,
    single_pass: bool,
    is_self: bool,
    exact_plane_filter: bool,
    skip_shared_points: bool,
    grain: i64,
    tolerance: f64,
    left_surface: &T,
    right_surface: &T,
) -> Result<(Vec<i64>, Vec<i64>), error::T> {
    if grain <= 0 {
        return Err(invalid_argument("Surface_index.overlapping_triangle_pairs: grain must be positive"));
    }
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(invalid_argument(
            "Surface_index.overlapping_triangle_pairs: tolerance must be finite and non-negative",
        ));
    }
    let left_count = left_surface.primitives.len() as i64;
    if left_count == 0 || right_surface.primitives.is_empty() {
        return Ok((Vec::new(), Vec::new()));
    }
    let shared_topology = std::sync::Arc::ptr_eq(&left_surface.topology.vertex_points, &right_surface.topology.vertex_points);
    // A lower bound keeps empty/disjoint workloads O(F / 128) in range metadata even when the caller
    // requests grain one; output order stays triangle-major, independent of this subdivision.
    // ponytail: OCaml's non-single-pass mode counts then fills; per-range buffers joined in range
    // order give the same bytes in one traversal, so both modes share this path.
    let range_size = if single_pass { grain.max(128) } else { grain.max(1) };
    let ranges = support::range_count(left_count, range_size) as usize;
    let slots = pair_slots(ranges);
    support::run_indexed_ranges(range_size, left_count, &|range, first, last| {
        let mut node_stack = [0_i64; 65];
        let mut left_bounds = [0.0_f64; 15];
        let mut out_left: Vec<i64> = Vec::new();
        let mut out_right: Vec<i64> = Vec::new();
        for triangle in first..last {
            cancel::poll(cancel_value, triangle, 4095)?;
            let left_triangle = triangle as usize;
            triangle_bounds_into(left_surface, left_triangle, &mut left_bounds);
            left_bounds[6] = tolerance;
            let mut top = 1_usize;
            node_stack[0] = 0;
            while top > 0 {
                top -= 1;
                let node = node_stack[top] as usize;
                let shared_node = skip_shared_points
                    && shared_topology
                    && (triangle_has_point(left_surface, left_triangle, right_surface.common_a[node])
                        || triangle_has_point(left_surface, left_triangle, right_surface.common_b[node])
                        || triangle_has_point(left_surface, left_triangle, right_surface.common_c[node]));
                if !shared_node && node_bounds_overlap(&left_bounds, right_surface, node) {
                    if right_surface.count[node] > 0 {
                        let start = right_surface.first[node];
                        for slot in start..start + right_surface.count[node] {
                            let right_triangle = right_surface.order[slot as usize];
                            let r = right_triangle as usize;
                            if (!is_self
                                || (right_triangle > triangle
                                    && right_surface.primitives[r] != left_surface.primitives[left_triangle]))
                                && (!(skip_shared_points && shared_topology)
                                    || !triangles_share_point(left_surface, left_triangle, right_surface, r))
                                && triangle_bounds_overlap(&left_bounds, right_surface, r)
                                && (!exact_plane_filter
                                    || exact_plane_overlap(left_surface, left_triangle, right_surface, r))
                            {
                                out_left.push(triangle);
                                out_right.push(right_triangle);
                            }
                        }
                    } else {
                        node_stack[top] = right_surface.right[node];
                        node_stack[top + 1] = right_surface.left[node];
                        top += 2;
                    }
                }
            }
        }
        *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some((out_left, out_right));
        Ok(())
    })?;
    Ok(join_pairs(slots))
}

fn overlapping_triangle_pairs(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    tolerance: f64,
    left_surface: &T,
    right_surface: &T,
) -> Result<(Vec<i64>, Vec<i64>), error::T> {
    triangle_pairs_of(cancel_value, false, false, false, false, grain, tolerance, left_surface, right_surface)
}

fn overlapping_self_triangle_pairs(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    tolerance: f64,
    surface: &T,
) -> Result<(Vec<i64>, Vec<i64>), error::T> {
    triangle_pairs_of(cancel_value, false, true, false, false, grain, tolerance, surface, surface)
}

fn ordered(a: i64, b: i64) -> (i64, i64) {
    if a < b { (a, b) } else { (b, a) }
}

fn self_pairs_disjoint_topology(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    tolerance: f64,
    surface: &T,
) -> Result<(Vec<i64>, Vec<i64>), error::T> {
    let triangles = surface.primitives.len();
    let primitive_count = surface.topology.primitive_kinds.len();
    let mut original_by_primitive = vec![-1_i64; primitive_count];
    let mut eligible = triangles == primitive_count;
    for triangle in 0..triangles {
        let primitive = surface.primitives[triangle];
        if primitive < 0
            || primitive as usize >= primitive_count
            || surface.topology.primitive_offsets[primitive as usize + 1]
                - surface.topology.primitive_offsets[primitive as usize]
                != 3
            || original_by_primitive[primitive as usize] >= 0
        {
            eligible = false;
        } else {
            original_by_primitive[primitive as usize] = triangle as i64;
        }
    }
    if !eligible {
        return triangle_pairs_of(cancel_value, true, true, false, true, grain, tolerance, surface, surface);
    }
    let subset = |indexed: &T, point: i64, incident: bool| -> Result<T, error::T> {
        let mut bits = vec![0_u8; primitive_count.div_ceil(8)];
        for triangle in 0..indexed.primitives.len() {
            if triangle_has_point(indexed, triangle, point) == incident {
                let primitive = indexed.primitives[triangle] as usize;
                bits[primitive >> 3] |= 1 << (primitive & 7);
            }
        }
        let selection = group::View::of_owned_bits(
            group::Owner::Primitive,
            "__rdk_surface_disjoint_partition",
            primitive_count as i64,
            bits,
        )?;
        create_with(
            cancel_value,
            grain,
            Some(&selection),
            None,
            VertexSelection::AllTriangleVertices,
            false,
            &indexed.geometry,
        )
    };
    let mut first_all: Vec<i64> = Vec::new();
    let mut second_all: Vec<i64> = Vec::new();
    // `gather` of OCaml, unrolled: cross pairs of every peeled hub, then the final self pairs.
    let mut owned: Option<T> = None;
    loop {
        let indexed: &T = owned.as_ref().unwrap_or(surface);
        cancel::check_opt(cancel_value)?;
        let count = indexed.primitives.len();
        if count < 2 {
            break;
        }
        let mut incidences = vec![0_i64; indexed.positions.x.len()];
        let points = &indexed.topology.vertex_points;
        for triangle in 0..count {
            incidences[points[indexed.vertex_a[triangle] as usize] as usize] += 1;
            incidences[points[indexed.vertex_b[triangle] as usize] as usize] += 1;
            incidences[points[indexed.vertex_c[triangle] as usize] as usize] += 1;
        }
        let mut point = 0_usize;
        for candidate in 1..incidences.len() {
            if incidences[candidate] > incidences[point] {
                point = candidate;
            }
        }
        let incidence = incidences[point];
        if incidence >= 32 && incidence * 4 >= count as i64 {
            let incident = subset(indexed, point as i64, true)?;
            let remainder = subset(indexed, point as i64, false)?;
            if !remainder.primitives.is_empty() {
                let (first, second) = triangle_pairs_of(
                    cancel_value, true, false, false, true, grain, tolerance, &incident, &remainder,
                )?;
                for pair in 0..first.len() {
                    let a = original_by_primitive[incident.primitives[first[pair] as usize] as usize];
                    let b = original_by_primitive[remainder.primitives[second[pair] as usize] as usize];
                    let (a, b) = ordered(a, b);
                    first_all.push(a);
                    second_all.push(b);
                }
            }
            owned = Some(remainder);
        } else {
            let (first, second) =
                triangle_pairs_of(cancel_value, true, true, false, true, grain, tolerance, indexed, indexed)?;
            for pair in 0..first.len() {
                let a = original_by_primitive[indexed.primitives[first[pair] as usize] as usize];
                let b = original_by_primitive[indexed.primitives[second[pair] as usize] as usize];
                if a < 0 || b < 0 {
                    return Err(invalid_argument("Surface_index disjoint-topology primitive map is incomplete"));
                }
                let (a, b) = ordered(a, b);
                first_all.push(a);
                second_all.push(b);
            }
            break;
        }
    }
    Ok((first_all, second_all))
}

#[inline]
fn aabb_distance_squared(value: &T, node: usize, qx: f64, qy: f64, qz: f64) -> f64 {
    let dx = if qx < value.min_x[node] {
        value.min_x[node] - qx
    } else if qx > value.max_x[node] {
        qx - value.max_x[node]
    } else {
        0.0
    };
    let dy = if qy < value.min_y[node] {
        value.min_y[node] - qy
    } else if qy > value.max_y[node] {
        qy - value.max_y[node]
    } else {
        0.0
    };
    let dz = if qz < value.min_z[node] {
        value.min_z[node] - qz
    } else if qz > value.max_z[node] {
        qz - value.max_z[node]
    } else {
        0.0
    };
    (dx * dx) + (dy * dy) + (dz * dz)
}

fn triangle_corners(value: &T, triangle: usize) -> [(f64, f64, f64); 3] {
    let points = &value.topology.vertex_points;
    let a = points[value.vertex_a[triangle] as usize] as usize;
    let b = points[value.vertex_b[triangle] as usize] as usize;
    let c = points[value.vertex_c[triangle] as usize] as usize;
    let p = &value.positions;
    [(p.x[a], p.y[a], p.z[a]), (p.x[b], p.y[b], p.z[b]), (p.x[c], p.y[c], p.z[c])]
}

#[inline]
fn closest_triangle_into(value: &T, triangle: usize, qx: f64, qy: f64, qz: f64, scratch: &mut [f64]) {
    let [(ax, ay, az), (bx, by, bz), (cx, cy, cz)] = triangle_corners(value, triangle);
    point_triangle_barycentric_into(qx, qy, qz, ax, ay, az, bx, by, bz, cx, cy, cz, scratch);
}

struct Closest {
    best_primitive: i64,
    best_triangle: i64,
}

fn visit_closest(value: &T, node: usize, qx: f64, qy: f64, qz: f64, scratch: &mut [f64], best: &mut Closest) {
    if aabb_distance_squared(value, node, qx, qy, qz) > scratch[0] {
        return;
    }
    if value.count[node] > 0 {
        let start = value.first[node];
        for at in start..start + value.count[node] {
            let triangle = value.order[at as usize];
            let primitive = value.primitives[triangle as usize];
            closest_triangle_into(value, triangle as usize, qx, qy, qz, scratch);
            let distance = scratch[4];
            if distance < scratch[0]
                || (distance == scratch[0]
                    && (best.best_primitive < 0
                        || primitive < best.best_primitive
                        || (primitive == best.best_primitive && triangle < best.best_triangle)))
            {
                best.best_primitive = primitive;
                best.best_triangle = triangle;
                scratch[0] = distance;
                scratch[1] = scratch[5];
                scratch[2] = scratch[6];
                scratch[3] = scratch[7];
            }
        }
    } else {
        let left = value.left[node];
        let right = value.right[node];
        let left_distance = aabb_distance_squared(value, left as usize, qx, qy, qz);
        let right_distance = aabb_distance_squared(value, right as usize, qx, qy, qz);
        if left_distance < right_distance || (left_distance == right_distance && left < right) {
            visit_closest(value, left as usize, qx, qy, qz, scratch, best);
            visit_closest(value, right as usize, qx, qy, qz, scratch, best);
        } else {
            visit_closest(value, right as usize, qx, qy, qz, scratch, best);
            visit_closest(value, left as usize, qx, qy, qz, scratch, best);
        }
    }
}

/// One closest-point result: primitive, triangle, barycentrics and squared distance.
struct ClosestResult {
    primitive: i64,
    triangle: i64,
    a: f64,
    b: f64,
    c: f64,
    distance: f64,
}

fn query_closest(value: &T, qx: f64, qy: f64, qz: f64, maximum: f64, scratch: &mut [f64]) -> ClosestResult {
    let mut best = Closest { best_primitive: -1, best_triangle: -1 };
    scratch[0] = maximum;
    scratch[1] = 0.0;
    scratch[2] = 0.0;
    scratch[3] = 0.0;
    if value.node_count > 0 {
        visit_closest(value, 0, qx, qy, qz, scratch, &mut best);
    }
    ClosestResult {
        primitive: best.best_primitive,
        triangle: best.best_triangle,
        a: scratch[1],
        b: scratch[2],
        c: scratch[3],
        distance: if best.best_primitive < 0 { f64::INFINITY } else { scratch[0] },
    }
}

#[allow(clippy::too_many_arguments)]
pub fn closest_many_into(
    cancel_value: Option<&cancel::T>,
    selection: Option<&group::T>,
    position_indices: Option<&[i64]>,
    grain: i64,
    value: &T,
    queries: &packed::Float3::T,
    max_distance_squared: f64,
    primitives: &mut [i64],
    triangles: &mut [i64],
    barycentric_a: &mut [f64],
    barycentric_b: &mut [f64],
    barycentric_c: &mut [f64],
    distances_squared: &mut [f64],
) -> Result<(), error::T> {
    if grain <= 0 {
        return Err(invalid_argument("Surface_index: grain must be positive"));
    }
    let position_count = packed::Float3::length(queries);
    let query_count = match position_indices {
        None => position_count,
        Some(values) => values.len() as i64,
    };
    if let Some(selected_group) = selection {
        if group::length(selected_group) != query_count {
            return Err(invalid_argument("Surface_index: query selection length does not match queries"));
        }
    }
    if let Some(values) = position_indices {
        if values.iter().any(|&point| point < 0 || point >= position_count) {
            return Err(invalid_argument("Surface_index: query position index is out of bounds"));
        }
    }
    let n = query_count as usize;
    if primitives.len() < n
        || triangles.len() < n
        || barycentric_a.len() < n
        || barycentric_b.len() < n
        || barycentric_c.len() < n
        || distances_squared.len() < n
    {
        return Err(invalid_argument("Surface_index: output arrays are too short"));
    }
    let query_view = packed::Float3::View::view(queries)?;
    {
        let windows = (
            support::Windows::even(&mut primitives[..n], grain),
            support::Windows::even(&mut triangles[..n], grain),
            support::Windows::even(&mut barycentric_a[..n], grain),
            support::Windows::even(&mut barycentric_b[..n], grain),
            support::Windows::even(&mut barycentric_c[..n], grain),
            support::Windows::even(&mut distances_squared[..n], grain),
        );
        support::run_indexed_ranges(grain, query_count, &|range, first, last| {
            // Unselected slots keep the caller's initialized values.
            let (out_primitives, out_triangles, out_a, out_b, out_c, out_d) = (
                windows.0.take(range),
                windows.1.take(range),
                windows.2.take(range),
                windows.3.take(range),
                windows.4.take(range),
                windows.5.take(range),
            );
            let mut scratch = [0.0_f64; 8];
            for query in first..last {
                cancel::poll(cancel_value, query, 4095)?;
                if selection.is_none_or(|selected_group| group::mem(query, selected_group)) {
                    let point = match position_indices {
                        None => query,
                        Some(values) => values[query as usize],
                    } as usize;
                    let (qx, qy, qz) = (query_view.x[point], query_view.y[point], query_view.z[point]);
                    if !(qx.is_finite() && qy.is_finite() && qz.is_finite()) {
                        return Err(invalid_argument("Surface_index: query positions must be finite"));
                    }
                    let result = query_closest(value, qx, qy, qz, max_distance_squared, &mut scratch);
                    let at = (query - first) as usize;
                    out_primitives[at] = result.primitive;
                    out_triangles[at] = result.triangle;
                    out_a[at] = result.a;
                    out_b[at] = result.b;
                    out_c[at] = result.c;
                    out_d[at] = result.distance;
                }
            }
            Ok(())
        })?;
    }
    Ok(())
}

fn visit_distance(value: &T, node: usize, qx: f64, qy: f64, qz: f64, scratch: &mut [f64]) {
    if aabb_distance_squared(value, node, qx, qy, qz) > scratch[0] {
        return;
    }
    if value.count[node] > 0 {
        let start = value.first[node];
        for at in start..start + value.count[node] {
            closest_triangle_into(value, value.order[at as usize] as usize, qx, qy, qz, scratch);
            if scratch[4] < scratch[0] {
                scratch[0] = scratch[4];
                scratch[1] = 1.0;
            }
        }
    } else {
        let left = value.left[node];
        let right = value.right[node];
        let left_distance = aabb_distance_squared(value, left as usize, qx, qy, qz);
        let right_distance = aabb_distance_squared(value, right as usize, qx, qy, qz);
        if left_distance < right_distance || (left_distance == right_distance && left < right) {
            visit_distance(value, left as usize, qx, qy, qz, scratch);
            visit_distance(value, right as usize, qx, qy, qz, scratch);
        } else {
            visit_distance(value, right as usize, qx, qy, qz, scratch);
            visit_distance(value, left as usize, qx, qy, qz, scratch);
        }
    }
}

/// Fill only closest squared distance, retaining infinity for misses and unselected queries.
pub fn closest_distances_many_into(
    cancel_value: Option<&cancel::T>,
    selection: Option<&group::T>,
    grain: i64,
    value: &T,
    queries: &packed::Float3::T,
    max_distance_squared: f64,
    distances_squared: &mut [f64],
) -> Result<(), error::T> {
    if grain <= 0 {
        return Err(invalid_argument("Surface_index: grain must be positive"));
    }
    let query_count = packed::Float3::length(queries);
    if let Some(selected_group) = selection {
        if group::owner(selected_group) != group::Owner::Point || group::length(selected_group) != query_count {
            return Err(invalid_argument("Surface_index: query selection must be a matching point group"));
        }
    }
    if (distances_squared.len() as i64) < query_count {
        return Err(invalid_argument("Surface_index: output array is too short"));
    }
    let query_view = packed::Float3::View::view(queries)?;
    {
        let windows = support::Windows::even(&mut distances_squared[..query_count as usize], grain);
        support::run_indexed_ranges(grain, query_count, &|range, first, last| {
            let chunk = windows.take(range);
            let mut scratch = [0.0_f64; 8];
            for query in first..last {
                cancel::poll(cancel_value, query, 4095)?;
                if selection.is_none_or(|selected_group| group::mem(query, selected_group)) {
                    let q = query as usize;
                    let (qx, qy, qz) = (query_view.x[q], query_view.y[q], query_view.z[q]);
                    if !(qx.is_finite() && qy.is_finite() && qz.is_finite()) {
                        return Err(invalid_argument("Surface_index: query positions must be finite"));
                    }
                    scratch[0] = max_distance_squared;
                    scratch[1] = 0.0;
                    if value.node_count > 0 {
                        visit_distance(value, 0, qx, qy, qz, &mut scratch);
                    }
                    chunk[(query - first) as usize] = if scratch[1] == 0.0 { f64::INFINITY } else { scratch[0] };
                }
            }
            Ok(())
        })?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
#[inline]
fn ray_aabb_intersects(
    value: &T, node: usize, ox: f64, oy: f64, oz: f64, dx: f64, dy: f64, dz: f64, minimum: f64, maximum: f64,
    tolerance: f64,
) -> bool {
    let xmin = value.min_x[node] - tolerance;
    let ymin = value.min_y[node] - tolerance;
    let zmin = value.min_z[node] - tolerance;
    let xmax = value.max_x[node] + tolerance;
    let ymax = value.max_y[node] + tolerance;
    let zmax = value.max_z[node] + tolerance;
    let slab = |direction: f64, origin: f64, low: f64, high: f64| -> (f64, f64) {
        let near = if direction > 0.0 {
            (low - origin) / direction
        } else if direction < 0.0 {
            (high - origin) / direction
        } else {
            f64::NEG_INFINITY
        };
        let far = if direction > 0.0 {
            (high - origin) / direction
        } else if direction < 0.0 {
            (low - origin) / direction
        } else {
            f64::INFINITY
        };
        (near, far)
    };
    let (x_near, x_far) = slab(dx, ox, xmin, xmax);
    if dx == 0.0 && (ox < xmin || ox > xmax) {
        return false;
    }
    let (y_near, y_far) = slab(dy, oy, ymin, ymax);
    if dy == 0.0 && (oy < ymin || oy > ymax) {
        return false;
    }
    let (z_near, z_far) = slab(dz, oz, zmin, zmax);
    if dz == 0.0 && (oz < zmin || oz > zmax) {
        return false;
    }
    let entry = float_max(minimum, float_max(x_near, float_max(y_near, z_near)));
    let exit = float_min(maximum, float_min(x_far, float_min(y_far, z_far)));
    entry <= exit
}

#[allow(clippy::too_many_arguments)]
#[inline]
fn ray_triangle_into(
    value: &T, triangle: usize, ox: f64, oy: f64, oz: f64, dx: f64, dy: f64, dz: f64, minimum: f64, maximum: f64,
    tolerance: f64, scratch: &mut [f64],
) -> bool {
    let [(ax, ay, az), (bx, by, bz), (cx, cy, cz)] = triangle_corners(value, triangle);
    let e1x = bx - ax;
    let e1y = by - ay;
    let e1z = bz - az;
    let e2x = cx - ax;
    let e2y = cy - ay;
    let e2z = cz - az;
    let px = (dy * e2z) - (dz * e2y);
    let py = (dz * e2x) - (dx * e2z);
    let pz = (dx * e2y) - (dy * e2x);
    let determinant = (e1x * px) + (e1y * py) + (e1z * pz);
    let e1_length = e1x.hypot(e1y.hypot(e1z));
    let e2_length = e2x.hypot(e2y.hypot(e2z));
    let determinant_scale = e1_length * e2_length;
    if determinant_scale == 0.0 || !determinant_scale.is_finite() || determinant.abs() <= f64::EPSILON * determinant_scale {
        return false;
    }
    let inverse = 1.0 / determinant;
    let tx = ox - ax;
    let ty = oy - ay;
    let tz = oz - az;
    let u = ((tx * px) + (ty * py) + (tz * pz)) * inverse;
    let qx = (ty * e1z) - (tz * e1y);
    let qy = (tz * e1x) - (tx * e1z);
    let qz = (tx * e1y) - (ty * e1x);
    let v = ((dx * qx) + (dy * qy) + (dz * qz)) * inverse;
    let distance = ((e2x * qx) + (e2y * qy) + (e2z * qz)) * inverse;
    if !(u.is_finite() && v.is_finite() && distance.is_finite()) || distance < minimum || distance > maximum {
        return false;
    }
    if u >= 0.0 && v >= 0.0 && u + v <= 1.0 {
        scratch[0] = distance;
        scratch[1] = 1.0 - u - v;
        scratch[2] = u;
        scratch[3] = v;
        return true;
    }
    if tolerance == 0.0 {
        return false;
    }
    let hx = ox + (distance * dx);
    let hy = oy + (distance * dy);
    let hz = oz + (distance * dz);
    closest_triangle_into(value, triangle, hx, hy, hz, scratch);
    if scratch[4] > tolerance * tolerance {
        return false;
    }
    scratch[0] = distance;
    scratch[1] = scratch[5];
    scratch[2] = scratch[6];
    scratch[3] = scratch[7];
    true
}

#[allow(non_snake_case)]
pub mod View {
    use super::*;

    #[derive(Clone)]
    pub enum RayDirections {
        ConstantDirection { x: f64, y: f64, z: f64 },
        PerQueryDirections(packed::Float3::View::View),
    }

    /// Build directly from geometry already certified finite, nondegenerate, polygon-triangle-only,
    /// and three-corners-per-primitive by the caller. This is a narrow internal ownership boundary,
    /// not a validation bypass for arbitrary inputs.
    pub fn create_validated_triangles(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        geometry_value: &geometry::T,
    ) -> Result<T, error::T> {
        create_remapped(
            cancel_value,
            Some(grain),
            None,
            None,
            VertexSelection::AllTriangleVertices,
            true,
            geometry_value,
        )
    }

    pub use super::{closest_distances_many_into, closest_many_into};

    /// Original source vertex index for local corner 0, 1, or 2 of one internal triangle.
    pub fn triangle_vertex(value: &T, triangle: i64, local: i64) -> i64 {
        let t = triangle as usize;
        if local == 0 {
            value.vertex_a[t]
        } else if local == 1 {
            value.vertex_b[t]
        } else {
            value.vertex_c[t]
        }
    }

    /// Original source point index referenced by one internal triangle corner.
    pub fn triangle_point(value: &T, triangle: i64, local: i64) -> i64 {
        value.topology.vertex_points[triangle_vertex(value, triangle, local) as usize]
    }

    /// Original source primitive for one internal triangle.
    pub fn triangle_primitive(value: &T, triangle: i64) -> i64 {
        value.primitives[triangle as usize]
    }

    /// Deterministic internal-triangle pairs whose bounds overlap within the non-negative
    /// world-space `tolerance`; independent left-triangle ranges are parallel.
    pub fn overlapping_triangle_pairs(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        tolerance: f64,
        left: &T,
        right: &T,
    ) -> Result<(Vec<i64>, Vec<i64>), error::T> {
        super::overlapping_triangle_pairs(cancel_value, grain, tolerance, left, right)
    }

    /// Each unordered pair of bounds-overlapping internal triangles once, excluding pairs generated
    /// from the same source primitive.
    pub fn overlapping_self_triangle_pairs(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        tolerance: f64,
        surface: &T,
    ) -> Result<(Vec<i64>, Vec<i64>), error::T> {
        super::overlapping_self_triangle_pairs(cancel_value, grain, tolerance, surface)
    }

    /// AABB/K-DOP candidates that do not share a topology point. BVH nodes whose complete subtree
    /// shares one query point are pruned before descent. The caller must validate the omitted local
    /// vertex fans.
    pub fn self_pairs_disjoint_topology(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        tolerance: f64,
        surface: &T,
    ) -> Result<(Vec<i64>, Vec<i64>), error::T> {
        super::self_pairs_disjoint_topology(cancel_value, grain, tolerance, surface)
    }

    /// Writes one hit (primitive, triangle; distance and barycentric weights) into slot `result` of
    /// the caller's result arrays (-1 and a signed infinity when it misses). `scratch` holds 8
    /// floats and `node_stack` 65 ints. The direction must be normalized.
    #[allow(clippy::too_many_arguments)]
    pub fn ray_query_once(
        value: &T,
        ox: f64,
        oy: f64,
        oz: f64,
        dx: f64,
        dy: f64,
        dz: f64,
        minimum: f64,
        maximum: f64,
        tolerance: f64,
        surface_hit: RaySurfaceHit,
        scratch: &mut [f64],
        node_stack: &mut [i64],
        integer_results: &mut [i64],
        float_results: &mut [f64],
        result: i64,
    ) -> Result<(), error::T> {
        let integer = (result * 2) as usize;
        let floating = (result * 4) as usize;
        integer_results[integer] = -1;
        integer_results[integer + 1] = -1;
        float_results[floating] = match surface_hit {
            RaySurfaceHit::RayFirstSurface => f64::INFINITY,
            RaySurfaceHit::RayLastSurface => f64::NEG_INFINITY,
        };
        float_results[floating + 1] = 0.0;
        float_results[floating + 2] = 0.0;
        float_results[floating + 3] = 0.0;
        let top_slot = node_stack.len() - 1;
        if value.node_count > 0 {
            node_stack[0] = 0;
            node_stack[top_slot] = 1;
        } else {
            node_stack[top_slot] = 0;
        }
        while node_stack[top_slot] > 0 {
            let top = (node_stack[top_slot] - 1) as usize;
            node_stack[top_slot] = top as i64;
            let node = node_stack[top] as usize;
            let bound = match surface_hit {
                RaySurfaceHit::RayFirstSurface => float_min(maximum, float_results[floating]),
                RaySurfaceHit::RayLastSurface => maximum,
            };
            if !ray_aabb_intersects(value, node, ox, oy, oz, dx, dy, dz, minimum, bound, tolerance) {
                continue;
            }
            if value.count[node] > 0 {
                let start = value.first[node];
                for at in start..start + value.count[node] {
                    let triangle = value.order[at as usize];
                    if ray_triangle_into(
                        value, triangle as usize, ox, oy, oz, dx, dy, dz, minimum, bound, tolerance, scratch,
                    ) {
                        let primitive = value.primitives[triangle as usize];
                        let distance = scratch[0];
                        let current = float_results[floating];
                        let better = match surface_hit {
                            RaySurfaceHit::RayFirstSurface => distance < current,
                            RaySurfaceHit::RayLastSurface => distance > current,
                        };
                        if better
                            || (distance == current
                                && (integer_results[integer] < 0
                                    || primitive < integer_results[integer]
                                    || (primitive == integer_results[integer]
                                        && triangle < integer_results[integer + 1])))
                        {
                            integer_results[integer] = primitive;
                            integer_results[integer + 1] = triangle;
                            float_results[floating] = distance;
                            float_results[floating + 1] = scratch[1];
                            float_results[floating + 2] = scratch[2];
                            float_results[floating + 3] = scratch[3];
                        }
                    }
                }
            } else {
                if top as i64 > top_slot as i64 - 2 {
                    return Err(invalid_argument("Surface_index: ray traversal stack overflow"));
                }
                node_stack[top] = value.right[node];
                node_stack[top + 1] = value.left[node];
                node_stack[top_slot] = (top + 2) as i64;
            }
        }
        Ok(())
    }

    pub fn direction_at(directions: &RayDirections, query: i64) -> (f64, f64, f64) {
        match directions {
            RayDirections::ConstantDirection { x, y, z } => (*x, *y, *z),
            RayDirections::PerQueryDirections(values) => {
                (values.x[query as usize], values.y[query as usize], values.z[query as usize])
            }
        }
    }

    /// Rejects a non-finite or zero direction.
    pub fn normalized_direction(vx: f64, vy: f64, vz: f64) -> Result<(f64, f64, f64), error::T> {
        let scale = float_max(vx.abs(), float_max(vy.abs(), vz.abs()));
        if !(vx.is_finite() && vy.is_finite() && vz.is_finite()) || scale == 0.0 {
            return Err(invalid_argument("Surface_index: selected ray directions must be finite and non-zero"));
        }
        let sx = vx / scale;
        let sy = vy / scale;
        let sz = vz / scale;
        let inverse = 1.0 / ((sx * sx) + (sy * sy) + (sz * sz)).sqrt();
        Ok((sx * inverse, sy * inverse, sz * inverse))
    }

    /// Unit geometric normal of a triangle into the first three slots, zero when degenerate.
    pub fn triangle_normal_into(value: &T, triangle: i64, output: &mut [f64]) {
        let [(ax, ay, az), (bx, by, bz), (cx, cy, cz)] = triangle_corners(value, triangle as usize);
        let scale = float_max(
            float_max(ax.abs(), float_max(ay.abs(), az.abs())),
            float_max(
                float_max(bx.abs(), float_max(by.abs(), bz.abs())),
                float_max(cx.abs(), float_max(cy.abs(), cz.abs())),
            ),
        );
        let scale = if scale == 0.0 { 1.0 } else { scale };
        let abx = (bx / scale) - (ax / scale);
        let aby = (by / scale) - (ay / scale);
        let abz = (bz / scale) - (az / scale);
        let acx = (cx / scale) - (ax / scale);
        let acy = (cy / scale) - (ay / scale);
        let acz = (cz / scale) - (az / scale);
        let nx = (aby * acz) - (abz * acy);
        let ny = (abz * acx) - (abx * acz);
        let nz = (abx * acy) - (aby * acx);
        let length = nx.hypot(ny.hypot(nz));
        if length == 0.0 || !length.is_finite() {
            output[0] = 0.0;
            output[1] = 0.0;
            output[2] = 0.0;
        } else {
            output[0] = nx / length;
            output[1] = ny / length;
            output[2] = nz / length;
        }
    }

    /// Allocation-bounded batch ray query. Each worker owns fixed scratch; selected queries write
    /// disjoint slots. Missed/unselected slots are reset to primitive/triangle -1, zero weights,
    /// and infinity.
    #[allow(clippy::too_many_arguments)]
    pub fn raycast_many_into(
        cancel_value: Option<&cancel::T>,
        selection: Option<&group::T>,
        grain: i64,
        value: &T,
        queries: &packed::Float3::T,
        directions: &RayDirections,
        min_distance: f64,
        max_distance: f64,
        tolerance: f64,
        direction_mode: RayDirectionMode,
        surface_hit: RaySurfaceHit,
        primitives: &mut [i64],
        triangles: &mut [i64],
        barycentric_a: &mut [f64],
        barycentric_b: &mut [f64],
        barycentric_c: &mut [f64],
        distances: &mut [f64],
    ) -> Result<(), error::T> {
        super::raycast_many_into(
            cancel_value,
            selection,
            grain,
            value,
            queries,
            directions,
            min_distance,
            max_distance,
            tolerance,
            direction_mode,
            surface_hit,
            primitives,
            triangles,
            barycentric_a,
            barycentric_b,
            barycentric_c,
            distances,
        )
    }
}

use View::RayDirections;

#[allow(clippy::too_many_arguments)]
fn raycast_many_into(
    cancel_value: Option<&cancel::T>,
    selection: Option<&group::T>,
    grain: i64,
    value: &T,
    queries: &packed::Float3::T,
    directions: &RayDirections,
    min_distance: f64,
    max_distance: f64,
    tolerance: f64,
    direction_mode: RayDirectionMode,
    surface_hit: RaySurfaceHit,
    primitives: &mut [i64],
    triangles: &mut [i64],
    barycentric_a: &mut [f64],
    barycentric_b: &mut [f64],
    barycentric_c: &mut [f64],
    distances: &mut [f64],
) -> Result<(), error::T> {
    if grain <= 0 {
        return Err(invalid_argument("Surface_index: grain must be positive"));
    }
    let query_count = packed::Float3::length(queries);
    if let RayDirections::PerQueryDirections(values) = directions {
        let n = query_count as usize;
        if values.x.len() != n || values.y.len() != n || values.z.len() != n {
            return Err(invalid_argument("Surface_index: ray direction count does not match queries"));
        }
    }
    if let Some(selected_group) = selection {
        if group::owner(selected_group) != group::Owner::Point || group::length(selected_group) != query_count {
            return Err(invalid_argument("Surface_index: ray selection must match query points"));
        }
    }
    if !(min_distance.is_finite() && min_distance >= 0.0) {
        return Err(invalid_argument("Surface_index: minimum ray distance must be finite and non-negative"));
    }
    if !((max_distance.is_finite() || max_distance == f64::INFINITY) && max_distance >= min_distance) {
        return Err(invalid_argument("Surface_index: maximum ray distance must be at least the minimum"));
    }
    if !(tolerance.is_finite() && tolerance >= 0.0 && tolerance <= f64::MAX.sqrt()) {
        return Err(invalid_argument("Surface_index: ray tolerance must be finite, non-negative, and safely squarable"));
    }
    let n = query_count as usize;
    if primitives.len() < n
        || triangles.len() < n
        || barycentric_a.len() < n
        || barycentric_b.len() < n
        || barycentric_c.len() < n
        || distances.len() < n
    {
        return Err(invalid_argument("Surface_index: ray output arrays are too short"));
    }
    let query_view = packed::Float3::View::view(queries)?;
    let windows = (
        support::Windows::even(&mut primitives[..n], grain),
        support::Windows::even(&mut triangles[..n], grain),
        support::Windows::even(&mut barycentric_a[..n], grain),
        support::Windows::even(&mut barycentric_b[..n], grain),
        support::Windows::even(&mut barycentric_c[..n], grain),
        support::Windows::even(&mut distances[..n], grain),
    );
    support::run_indexed_ranges(grain, query_count, &|range, first, last| {
        let (out_primitives, out_triangles, out_a, out_b, out_c, out_d) = (
            windows.0.take(range),
            windows.1.take(range),
            windows.2.take(range),
            windows.3.take(range),
            windows.4.take(range),
            windows.5.take(range),
        );
        out_primitives.fill(-1);
        out_triangles.fill(-1);
        out_a.fill(0.0);
        out_b.fill(0.0);
        out_c.fill(0.0);
        out_d.fill(f64::INFINITY);
        let mut scratch = [0.0_f64; 8];
        let mut node_stack = [0_i64; 65];
        let mut integer_results = [-1_i64; 4];
        let mut float_results = [0.0_f64; 8];
        for query in first..last {
            cancel::poll(cancel_value, query, 4095)?;
            if !selection.is_none_or(|selected_group| group::mem(query, selected_group)) {
                continue;
            }
            let q = query as usize;
            let (ox, oy, oz) = (query_view.x[q], query_view.y[q], query_view.z[q]);
            let (vx, vy, vz) = View::direction_at(directions, query);
            if !(ox.is_finite() && oy.is_finite() && oz.is_finite()) {
                return Err(invalid_argument("Surface_index: selected ray origins must be finite"));
            }
            let (dx, dy, dz) = View::normalized_direction(vx, vy, vz)?;
            let mut once = |sign: f64, slot: i64| {
                View::ray_query_once(
                    value,
                    ox,
                    oy,
                    oz,
                    sign * dx,
                    sign * dy,
                    sign * dz,
                    min_distance,
                    max_distance,
                    tolerance,
                    surface_hit,
                    &mut scratch,
                    &mut node_stack,
                    &mut integer_results,
                    &mut float_results,
                    slot,
                )
            };
            match direction_mode {
                RayDirectionMode::RayForward => once(1.0, 0)?,
                RayDirectionMode::RayReverse => once(-1.0, 0)?,
                RayDirectionMode::RayBidirectionalClosest | RayDirectionMode::RayBidirectionalFarthest => {
                    once(1.0, 0)?;
                    once(-1.0, 1)?;
                }
            }
            let chosen = match direction_mode {
                RayDirectionMode::RayForward | RayDirectionMode::RayReverse => 0,
                _ => {
                    let forward = integer_results[0] >= 0;
                    let reverse = integer_results[2] >= 0;
                    if !forward {
                        1
                    } else if !reverse {
                        0
                    } else {
                        let fd = float_results[0];
                        let rd = float_results[4];
                        match direction_mode {
                            RayDirectionMode::RayBidirectionalClosest => usize::from(rd < fd),
                            _ => usize::from(rd > fd),
                        }
                    }
                }
            };
            let integer = chosen * 2;
            let floating = chosen * 4;
            if integer_results[integer] >= 0 {
                let at = (query - first) as usize;
                out_primitives[at] = integer_results[integer];
                out_triangles[at] = integer_results[integer + 1];
                out_a[at] = float_results[floating + 1];
                out_b[at] = float_results[floating + 2];
                out_c[at] = float_results[floating + 3];
                out_d[at] = float_results[floating];
            }
        }
        Ok(())
    })?;
    Ok(())
}

/// Intersect a two-sided polygon surface with a world-space ray. Direction is robustly normalized,
/// so returned distance and distance bounds use world units. Bidirectional selection compares
/// unsigned distances; exact ties prefer the forward direction. Equal-distance triangle ties select
/// lower source primitive and internal triangle numbers.
pub fn raycast(
    value: &T,
    origin: rays_math::vec3::T,
    direction: rays_math::vec3::T,
) -> Result<Option<Hit>, error::T> {
    let queries = packed::Float3::View::of_owned_exn(vec![origin.x], vec![origin.y], vec![origin.z])?;
    let mut primitive = [-1_i64];
    let mut triangle = [-1_i64];
    let mut a = [0.0_f64];
    let mut b = [0.0_f64];
    let mut c = [0.0_f64];
    let mut distance = [f64::INFINITY];
    raycast_many_into(
        None,
        None,
        1,
        value,
        &queries,
        &RayDirections::ConstantDirection { x: direction.x, y: direction.y, z: direction.z },
        0.0,
        f64::INFINITY,
        0.0,
        RayDirectionMode::RayForward,
        RaySurfaceHit::RayFirstSurface,
        &mut primitive,
        &mut triangle,
        &mut a,
        &mut b,
        &mut c,
        &mut distance,
    )
    .map_err(|failure| {
        error::make(OPERATION.to_owned(), "invalid_ray".to_owned(), error::message(&failure).to_owned(), None)
            .unwrap_or_else(|bad| bad)
    })?;
    Ok(if primitive[0] < 0 { None } else { Some(Hit { primitive: primitive[0], distance: distance[0] }) })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A unit square (quad, two ear triangles) at z = 0 and a triangle at z = 2.
    fn two_primitives() -> geometry::T {
        let x = vec![0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0];
        let y = vec![0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0];
        let z = vec![0.0, 0.0, 0.0, 0.0, 2.0, 2.0, 2.0];
        let positions = packed::Float3::View::of_owned_exn(x, y, z).unwrap();
        let topo = topology::View::create_validated_owned(7, vec![0, 1, 2, 3, 4, 5, 6], vec![0, 4, 7], vec![0, 0]);
        geometry::create(positions, topo, None, None, None).unwrap()
    }
    #[test]
    fn distance_only_surface_matches_provenance_and_bounded_misses() {
        let positions = packed::Float3::View::of_owned_exn(
            vec![-5.0, 5.0, 5.0, -5.0], vec![0.0; 4], vec![-5.0, -5.0, 5.0, 5.0],
        ).unwrap();
        let topology = topology::polygons_owned(4, vec![0, 1, 2, 3], vec![0, 4]).unwrap();
        let geometry = geometry::create(positions, topology, None, None, None).unwrap();
        let index = create(None, Some(1), None, None, None, &geometry).unwrap();
        let queries = packed::Float3::View::of_owned_exn(
            vec![0.0, 1.0, 4.0], vec![0.0, 2.0, 0.0], vec![0.0; 3],
        ).unwrap();
        let (mut primitives, mut triangles) = ([-1; 3], [-1; 3]);
        let (mut a, mut b, mut c) = ([0.0; 3], [0.0; 3], [0.0; 3]);
        let (mut generic, mut specialized) = ([f64::INFINITY; 3], [f64::INFINITY; 3]);
        closest_many_into(None, None, None, 1, &index, &queries, f64::INFINITY,
            &mut primitives, &mut triangles, &mut a, &mut b, &mut c, &mut generic).unwrap();
        closest_distances_many_into(None, None, 1, &index, &queries, f64::INFINITY, &mut specialized).unwrap();
        assert_eq!(generic, specialized, "distance-only surface index matches provenance query");
        let mut bounded = [0.0; 3];
        closest_distances_many_into(None, None, 1, &index, &queries, 0.01, &mut bounded).unwrap();
        assert_eq!(bounded[1], f64::INFINITY, "distance-only surface index bounded miss sentinel");
    }


    #[test]
    fn construction_triangulates_and_raycasts_with_stable_ties() {
        let index = create(None, Some(1), None, None, None, &two_primitives()).unwrap();
        assert_eq!(triangle_count(&index), 3);
        let origin = rays_math::vec3::create(0.25, 0.25, -1.0);
        let up = rays_math::vec3::create(0.0, 0.0, 1.0);
        let hit = raycast(&index, origin, up).unwrap().unwrap();
        assert_eq!(hit.primitive, 0);
        assert_eq!(hit.distance, 1.0);
        assert_eq!(raycast(&index, origin, rays_math::vec3::create(0.0, 0.0, -1.0)).unwrap(), None);
        let error = raycast(&index, origin, rays_math::vec3::create(0.0, 0.0, 0.0)).unwrap_err();
        assert_eq!(error::code(&error), "invalid_ray");
    }

    #[test]
    fn closest_queries_fill_stable_slots_and_parallel_equals_sequential() {
        let index = create(None, Some(1), None, None, None, &two_primitives()).unwrap();
        let queries = packed::Float3::View::of_owned_exn(
            vec![0.25, 0.5, 5.0, 0.0],
            vec![0.25, 0.5, 5.0, 0.0],
            vec![1.0, 1.9, 5.0, 3.0],
        )
        .unwrap();
        let run = |grain: i64| {
            let mut p = [-1_i64; 4];
            let mut t = [-1_i64; 4];
            let mut a = [0.0; 4];
            let mut b = [0.0; 4];
            let mut c = [0.0; 4];
            let mut d = [f64::INFINITY; 4];
            closest_many_into(None, None, None, grain, &index, &queries, 4.0, &mut p, &mut t, &mut a, &mut b, &mut c, &mut d)
                .unwrap();
            (p, t, a, b, c, d)
        };
        let sequential = run(100);
        assert_eq!(sequential, run(1));
        assert_eq!(sequential.0[0], 0);
        assert_eq!(sequential.0[1], 1);
        assert_eq!(sequential.0[2], -1);
        assert_eq!(sequential.5[0], 1.0);
        assert_eq!(sequential.5[2], f64::INFINITY);
        let mut distances = [f64::INFINITY; 4];
        closest_distances_many_into(None, None, 1, &index, &queries, 4.0, &mut distances).unwrap();
        assert_eq!(distances.to_vec(), sequential.5.to_vec());
    }

    #[test]
    fn candidate_pairs_exclude_same_primitive_and_tolerance_widens() {
        let index = create(None, Some(1), None, None, None, &two_primitives()).unwrap();
        let (left, right) = View::overlapping_self_triangle_pairs(None, 1, 0.0, &index).unwrap();
        // the two quad triangles share a primitive and the far triangle is 2 away
        assert!(left.is_empty() && right.is_empty());
        let (left, right) = View::overlapping_self_triangle_pairs(None, 1, 2.5, &index).unwrap();
        assert_eq!((left, right), (vec![0, 1], vec![2, 2]));
        let (first, second) = View::self_pairs_disjoint_topology(None, 1, 2.5, &index).unwrap();
        assert_eq!((first.len(), second.len()), (2, 2));
        assert!(View::overlapping_triangle_pairs(None, 0, 0.0, &index, &index).is_err());
    }

    #[test]
    fn vertex_selection_and_degenerate_surfaces() {
        let geom = two_primitives();
        let bits = vec![0b0001_1111_u8, 0];
        let vertices = group::View::of_owned_bits(group::Owner::Vertex, "v", 7, vec![bits[0] & 0x7f]).unwrap();
        let index = create(None, None, None, Some(&vertices), Some(VertexSelection::AnyTriangleVertex), &geom).unwrap();
        assert!(triangle_count(&index) >= 2);
        let flat = packed::Float3::View::of_owned_exn(vec![0.0, 1.0, 2.0], vec![0.0; 3], vec![0.0; 3]).unwrap();
        let topo = topology::View::create_validated_owned(3, vec![0, 1, 2], vec![0, 3], vec![0]);
        let degenerate = geometry::create(flat, topo, None, None, None).unwrap();
        let failure = create(None, None, None, None, None, &degenerate).err().unwrap();
        assert_eq!(error::message(&failure), "primitive 0 is degenerate");
        assert_eq!(error::code(&failure), "invalid_surface");
    }
}

#[cfg(test)]
mod ported_tests {
    //! Port of lib/rdk/test_surface_index.ml.
    use super::*;

    fn high_valence_surface() -> geometry::T {
        let ring = 40_usize;
        let point_count = 1 + ring + 6;
        let mut x = vec![0.0; point_count];
        let mut y = vec![0.0; point_count];
        let z = vec![0.0; point_count];
        for point in 0..ring {
            let angle = 2.0 * std::f64::consts::PI * point as f64 / ring as f64;
            x[1 + point] = angle.cos();
            y[1 + point] = angle.sin();
        }
        let extra = 1 + ring;
        let extras = [(-0.31, -0.11), (0.29, -0.09), (0.02, 0.34), (-0.27, 0.13), (0.32, 0.15), (-0.01, -0.30)];
        for (at, (px, py)) in extras.into_iter().enumerate() {
            x[extra + at] = px;
            y[extra + at] = py;
        }
        let primitive_count = ring + 2;
        let mut vertices = vec![0_i64; primitive_count * 3];
        for primitive in 0..ring {
            vertices[primitive * 3] = 0;
            vertices[primitive * 3 + 1] = 1 + primitive as i64;
            vertices[primitive * 3 + 2] = 1 + ((primitive + 1) % ring) as i64;
        }
        for at in 0..6 {
            vertices[ring * 3 + at] = (extra + at) as i64;
        }
        let offsets: Vec<i64> = (0..=primitive_count as i64).map(|value| value * 3).collect();
        let topology_value = topology::polygons_owned(point_count as i64, vertices, offsets).unwrap();
        let positions = packed::Float3::View::of_owned_exn(x, y, z).unwrap();
        geometry::create(positions, topology_value, None, None, None).unwrap()
    }

    fn sorted_keys(triangle_count: i64, first: &[i64], second: &[i64]) -> Vec<i64> {
        let mut keys: Vec<i64> = (0..first.len()).map(|pair| first[pair] * triangle_count + second[pair]).collect();
        keys.sort_unstable();
        keys
    }

    fn filtered_reference(surface: &T, first: &[i64], second: &[i64]) -> (Vec<i64>, Vec<i64>) {
        let mut output_first = Vec::new();
        let mut output_second = Vec::new();
        for pair in 0..first.len() {
            let mut shared = false;
            for left in 0..3 {
                let point = View::triangle_point(surface, first[pair], left);
                for right in 0..3 {
                    if point == View::triangle_point(surface, second[pair], right) {
                        shared = true;
                    }
                }
            }
            if !shared {
                output_first.push(first[pair]);
                output_second.push(second[pair]);
            }
        }
        (output_first, output_second)
    }

    #[test]
    fn median_split_bounds_keep_tied_centroid_order() {
        let centroids = vec![0.0; 9];
        let lower: Vec<f64> = (0..9).map(|item| item as f64).collect();
        let upper: Vec<f64> = (0..9).map(|item| item as f64 + 1.0).collect();
        let bounds = bounds3_index::create(
            None, None, 16, &centroids, &centroids, &centroids, &lower, &lower, &lower, &upper, &upper, &upper,
        )
        .unwrap();
        assert_eq!(bounds.order, (0..9).collect::<Vec<i64>>(), "median-split bounds changed stable tied-centroid order");
        assert_eq!(bounds.left, vec![1, -1, -1]);
        assert_eq!(bounds.right, vec![2, -1, -1]);
        assert_eq!(bounds.first, vec![0, 0, 4]);
        assert_eq!(bounds.count, vec![0, 4, 5]);
    }

    #[test]
    fn disjoint_topology_candidates_match_raw_filtering_across_grains() {
        let geometry_value = high_valence_surface();
        let surface = create(None, Some(1), None, None, None, &geometry_value).unwrap();
        let (raw_first, raw_second) = View::overlapping_self_triangle_pairs(None, 1, 0.0, &surface).unwrap();
        let (reference_first, reference_second) = filtered_reference(&surface, &raw_first, &raw_second);
        let (one_first, one_second) = View::self_pairs_disjoint_topology(None, 1, 0.0, &surface).unwrap();
        let (many_first, many_second) = View::self_pairs_disjoint_topology(None, 8, 0.0, &surface).unwrap();
        let triangles = triangle_count(&surface);
        let reference = sorted_keys(triangles, &reference_first, &reference_second);
        let one = sorted_keys(triangles, &one_first, &one_second);
        let many = sorted_keys(triangles, &many_first, &many_second);
        assert!(!reference.is_empty(), "high-valence candidate fixture produced no disjoint pairs");
        assert_eq!(one, reference, "disjoint-topology candidates differ from raw shared-point filtering");
        assert_eq!(many, one, "disjoint-topology candidates differ across scheduling grains");
    }
}

#[cfg(test)]
mod attribute_suite_tests {
    //! The surface-index checks of lib/rdk/test_rdk_attributes.ml that lib/rdk/test_surface_index.ml
    //! does not cover (the transfer half is ported in rdk_ops/attribute_transfer.rs).
    use super::*;

    fn single_polygon(x: Vec<f64>, y: Vec<f64>) -> geometry::T {
        let count = x.len() as i64;
        let positions = packed::Float3::View::of_owned_exn(x, y, vec![0.0; count as usize]).unwrap();
        let topo = topology::polygons_owned(count, (0..count).collect(), vec![0, count]).unwrap();
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    #[test]
    fn degenerate_tiny_concave_and_self_intersecting_polygons() {
        let collinear = single_polygon(vec![0.0, 1.0, 2.0], vec![0.0; 3]);
        assert_eq!(error::code(&create(None, None, None, None, None, &collinear).err().unwrap()), "invalid_surface");
        // exact non-collinear triangle whose area underflows to a subnormal
        let tiny = single_polygon(vec![0.0, 1e-100, 0.0], vec![0.0, 0.0, 1e-100]);
        assert_eq!(triangle_count(&create(None, None, None, None, None, &tiny).unwrap()), 1);
        let concave = single_polygon(vec![0.0, 2.0, 2.0, 1.0, 0.0], vec![0.0, 0.0, 2.0, 1.0, 2.0]);
        let index = create(None, None, None, None, None, &concave).unwrap();
        assert_eq!(triangle_count(&index), 3, "concave polygon triangle cardinality");
        let hit = raycast(&index, rays_math::vec3::create(0.4, 1.4, 0.75), rays_math::vec3::create(0.0, 0.0, -1.0))
            .unwrap()
            .expect("concave polygon interior was missed");
        assert!(hit.primitive == 0 && (hit.distance - 0.75).abs() <= 1e-14);
        let bow_tie = single_polygon(vec![0.0, 2.0, 0.0, 2.0], vec![0.0, 2.0, 2.0, 0.0]);
        assert_eq!(error::code(&create(None, None, None, None, None, &bow_tie).err().unwrap()), "invalid_surface");
    }

    #[test]
    fn cancelled_construction_reports_cancelled() {
        let token = cancel::create();
        cancel::cancel(&token);
        let square = single_polygon(vec![0.0, 1.0, 1.0, 0.0], vec![0.0, 0.0, 1.0, 1.0]);
        assert_eq!(error::code(&create(Some(&token), None, None, None, None, &square).err().unwrap()), "cancelled");
    }
}
