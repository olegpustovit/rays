//! Shared allocation-free triangle/triangle narrow phase.
//!
//! The module is private to RDK. It is the single floating-point decision and event-generation
//! boundary used by Boolean diagnostics and intersection point construction.
use std::sync::Arc;

use rdk_core::{error, geometry, packed, topology};
use rdk_core::error::Code;

use crate::point_grid::{float_max, float_min};
use crate::surface_index;

pub struct Surface {
    pub index: Arc<surface_index::T>,
    pub positions: packed::Float3::View::View,
    pub topology: topology::View::View,
}

pub fn surface(index: Arc<surface_index::T>, geometry_value: &geometry::T) -> Result<Surface, error::T> {
    Ok(Surface {
        index,
        positions: packed::Float3::View::view(geometry::positions(geometry_value))?,
        topology: topology::View::view(geometry::topology(geometry_value))?,
    })
}

pub const MAX_EVENTS: usize = 12;
pub const EVENT_STRIDE: usize = 9;

#[inline]
pub(crate) fn clamp01(value: f64) -> f64 {
    if value < 0.0 {
        0.0
    } else if value > 1.0 {
        1.0
    } else {
        value
    }
}

#[inline]
fn coordinate(scratch: &[f64], point: usize, component: usize) -> f64 {
    scratch[(point * 3) + component]
}

#[inline]
fn projected_u(scratch: &[f64], axis: i64, point: usize) -> f64 {
    if axis == 0 { coordinate(scratch, point, 1) } else { coordinate(scratch, point, 0) }
}

#[inline]
fn projected_v(scratch: &[f64], axis: i64, point: usize) -> f64 {
    if axis == 2 { coordinate(scratch, point, 1) } else { coordinate(scratch, point, 2) }
}

#[inline]
fn cross2(ax: f64, ay: f64, bx: f64, by: f64) -> f64 {
    (ax * by) - (ay * bx)
}

fn point_in_projected_triangle(scratch: &[f64], epsilon: f64, axis: i64, point: usize, a: usize, b: usize, c: usize) -> bool {
    let px = projected_u(scratch, axis, point);
    let py = projected_v(scratch, axis, point);
    let ax = projected_u(scratch, axis, a);
    let ay = projected_v(scratch, axis, a);
    let bx = projected_u(scratch, axis, b);
    let by = projected_v(scratch, axis, b);
    let cx = projected_u(scratch, axis, c);
    let cy = projected_v(scratch, axis, c);
    let ab = cross2(bx - ax, by - ay, px - ax, py - ay);
    let bc = cross2(cx - bx, cy - by, px - bx, py - by);
    let ca = cross2(ax - cx, ay - cy, px - cx, py - cy);
    (ab >= -epsilon && bc >= -epsilon && ca >= -epsilon) || (ab <= epsilon && bc <= epsilon && ca <= epsilon)
}

fn barycentric_into(scratch: &[f64], point: usize, a: usize, b: usize, c: usize, output: &mut [f64], offset: usize) -> bool {
    let (px, py, pz) = (scratch[point], scratch[point + 1], scratch[point + 2]);
    let (ax, ay, az) = (scratch[a], scratch[a + 1], scratch[a + 2]);
    let (bx, by, bz) = (scratch[b], scratch[b + 1], scratch[b + 2]);
    let (cx, cy, cz) = (scratch[c], scratch[c + 1], scratch[c + 2]);
    let v0x = bx - ax;
    let v0y = by - ay;
    let v0z = bz - az;
    let v1x = cx - ax;
    let v1y = cy - ay;
    let v1z = cz - az;
    let v2x = px - ax;
    let v2y = py - ay;
    let v2z = pz - az;
    let d00 = (v0x * v0x) + (v0y * v0y) + (v0z * v0z);
    let d01 = (v0x * v1x) + (v0y * v1y) + (v0z * v1z);
    let d11 = (v1x * v1x) + (v1y * v1y) + (v1z * v1z);
    let d20 = (v2x * v0x) + (v2y * v0y) + (v2z * v0z);
    let d21 = (v2x * v1x) + (v2y * v1y) + (v2z * v1z);
    let denominator = (d00 * d11) - (d01 * d01);
    if denominator <= 0.0 {
        return false;
    }
    let v = ((d11 * d20) - (d01 * d21)) / denominator;
    let w = ((d00 * d21) - (d01 * d20)) / denominator;
    let u = 1.0 - v - w;
    let (u, v, w) = (clamp01(u), clamp01(v), clamp01(w));
    let sum = u + v + w;
    output[offset] = u / sum;
    output[offset + 1] = v / sum;
    output[offset + 2] = w / sum;
    true
}

fn point_segment_distance_squared(scratch: &[f64], point: usize, a: usize, b: usize) -> f64 {
    let (px, py, pz) = (scratch[point], scratch[point + 1], scratch[point + 2]);
    let (ax, ay, az) = (scratch[a], scratch[a + 1], scratch[a + 2]);
    let (bx, by, bz) = (scratch[b], scratch[b + 1], scratch[b + 2]);
    let dx = bx - ax;
    let dy = by - ay;
    let dz = bz - az;
    let length_squared = (dx * dx) + (dy * dy) + (dz * dz);
    let along = if length_squared == 0.0 {
        0.0
    } else {
        clamp01((((px - ax) * dx) + ((py - ay) * dy) + ((pz - az) * dz)) / length_squared)
    };
    let qx = ax + (along * dx);
    let qy = ay + (along * dy);
    let qz = az + (along * dz);
    let ex = px - qx;
    let ey = py - qy;
    let ez = pz - qz;
    (ex * ex) + (ey * ey) + (ez * ez)
}

fn suppressed_self_event(scratch: &[f64], point_info: &[i64], point: usize, epsilon: f64) -> bool {
    match point_info[6] {
        1 => {
            let shared = point_info[7] as usize * 3;
            let dx = scratch[point] - scratch[shared];
            let dy = scratch[point + 1] - scratch[shared + 1];
            let dz = scratch[point + 2] - scratch[shared + 2];
            (dx * dx) + (dy * dy) + (dz * dz) <= epsilon * epsilon
        }
        2 => {
            point_segment_distance_squared(scratch, point, point_info[7] as usize * 3, point_info[8] as usize * 3)
                <= epsilon * epsilon
        }
        _ => false,
    }
}

fn add_event(scratch: &[f64], point_info: &[i64], events: &mut [f64], count: usize, is_self: bool, point: usize) -> usize {
    let epsilon = scratch[18];
    if is_self && suppressed_self_event(scratch, point_info, point, epsilon) {
        return count;
    }
    let mut duplicate = false;
    let mut event = 0;
    while !duplicate && event < count {
        let offset = event * EVENT_STRIDE;
        let dx = scratch[point] - events[offset];
        let dy = scratch[point + 1] - events[offset + 1];
        let dz = scratch[point + 2] - events[offset + 2];
        duplicate = (dx * dx) + (dy * dy) + (dz * dz) <= epsilon * epsilon;
        event += 1;
    }
    if duplicate || count == MAX_EVENTS {
        return count;
    }
    let offset = count * EVENT_STRIDE;
    events[offset] = scratch[point];
    events[offset + 1] = scratch[point + 1];
    events[offset + 2] = scratch[point + 2];
    if barycentric_into(scratch, point, 0, 3, 6, events, offset + 3)
        && barycentric_into(scratch, point, 9, 12, 15, events, offset + 6)
    {
        count + 1
    } else {
        count
    }
}

#[allow(clippy::too_many_arguments)]
fn add_interpolated(
    scratch: &mut [f64], point_info: &[i64], events: &mut [f64], count: usize, is_self: bool, p0: usize, p1: usize,
    along: f64,
) -> usize {
    let along = clamp01(along);
    scratch[23] = scratch[p0] + (along * (scratch[p1] - scratch[p0]));
    scratch[24] = scratch[p0 + 1] + (along * (scratch[p1 + 1] - scratch[p0 + 1]));
    scratch[25] = scratch[p0 + 2] + (along * (scratch[p1 + 2] - scratch[p0 + 2]));
    add_event(scratch, point_info, events, count, is_self, 23)
}

#[allow(clippy::too_many_arguments)]
fn segment_triangle_event(
    scratch: &mut [f64], point_info: &[i64], events: &mut [f64], count: usize, is_self: bool, p0: usize, p1: usize,
    a0: usize, a1: usize, a2: usize,
) -> usize {
    let (p0x, p0y, p0z) = (scratch[p0], scratch[p0 + 1], scratch[p0 + 2]);
    let (p1x, p1y, p1z) = (scratch[p1], scratch[p1 + 1], scratch[p1 + 2]);
    let (a0x, a0y, a0z) = (scratch[a0], scratch[a0 + 1], scratch[a0 + 2]);
    let (a1x, a1y, a1z) = (scratch[a1], scratch[a1 + 1], scratch[a1 + 2]);
    let (a2x, a2y, a2z) = (scratch[a2], scratch[a2 + 1], scratch[a2 + 2]);
    let epsilon = scratch[18];
    let dx = p1x - p0x;
    let dy = p1y - p0y;
    let dz = p1z - p0z;
    let e1x = a1x - a0x;
    let e1y = a1y - a0y;
    let e1z = a1z - a0z;
    let e2x = a2x - a0x;
    let e2y = a2y - a0y;
    let e2z = a2z - a0z;
    let px = (dy * e2z) - (dz * e2y);
    let py = (dz * e2x) - (dx * e2z);
    let pz = (dx * e2y) - (dy * e2x);
    let determinant = (e1x * px) + (e1y * py) + (e1z * pz);
    if determinant.abs() <= epsilon {
        return count;
    }
    let inverse = 1.0 / determinant;
    let tx = p0x - a0x;
    let ty = p0y - a0y;
    let tz = p0z - a0z;
    let u = ((tx * px) + (ty * py) + (tz * pz)) * inverse;
    if u < -epsilon || u > 1.0 + epsilon {
        return count;
    }
    let qx = (ty * e1z) - (tz * e1y);
    let qy = (tz * e1x) - (tx * e1z);
    let qz = (tx * e1y) - (ty * e1x);
    let v = ((dx * qx) + (dy * qy) + (dz * qz)) * inverse;
    if v < -epsilon || u + v > 1.0 + epsilon {
        return count;
    }
    let along = ((e2x * qx) + (e2y * qy) + (e2z * qz)) * inverse;
    if along < -epsilon || along > 1.0 + epsilon {
        return count;
    }
    add_interpolated(scratch, point_info, events, count, is_self, p0, p1, along)
}

#[allow(clippy::too_many_arguments)]
fn coplanar_segment_events(
    scratch: &mut [f64], point_info: &[i64], events: &mut [f64], count: usize, is_self: bool, axis: i64, a: usize,
    b: usize, c: usize, d: usize,
) -> usize {
    let ax = projected_u(scratch, axis, a);
    let ay = projected_v(scratch, axis, a);
    let bx = projected_u(scratch, axis, b);
    let by = projected_v(scratch, axis, b);
    let cx = projected_u(scratch, axis, c);
    let cy = projected_v(scratch, axis, c);
    let dx = projected_u(scratch, axis, d);
    let dy = projected_v(scratch, axis, d);
    let epsilon = scratch[18];
    let rx = bx - ax;
    let ry = by - ay;
    let sx = dx - cx;
    let sy = dy - cy;
    let denominator = cross2(rx, ry, sx, sy);
    let qpx = cx - ax;
    let qpy = cy - ay;
    if denominator.abs() > epsilon {
        let along = cross2(qpx, qpy, sx, sy) / denominator;
        let other = cross2(qpx, qpy, rx, ry) / denominator;
        if along >= -epsilon && along <= 1.0 + epsilon && other >= -epsilon && other <= 1.0 + epsilon {
            add_interpolated(scratch, point_info, events, count, is_self, a * 3, b * 3, along)
        } else {
            count
        }
    } else if cross2(qpx, qpy, rx, ry).abs() > epsilon {
        count
    } else {
        let use_x = rx.abs() >= ry.abs();
        let start = if use_x { ax } else { ay };
        let finish = if use_x { bx } else { by };
        let c_value = if use_x { cx } else { cy };
        let d_value = if use_x { dx } else { dy };
        let delta = finish - start;
        if delta.abs() <= epsilon {
            let ex = ax - cx;
            let ey = ay - cy;
            if (ex * ex) + (ey * ey) <= epsilon * epsilon {
                add_interpolated(scratch, point_info, events, count, is_self, a * 3, b * 3, 0.0)
            } else {
                count
            }
        } else {
            let t0 = (c_value - start) / delta;
            let t1 = (d_value - start) / delta;
            let lower = float_max(0.0, float_min(t0, t1));
            let upper = float_min(1.0, float_max(t0, t1));
            if lower > upper + epsilon {
                count
            } else {
                let count = add_interpolated(scratch, point_info, events, count, is_self, a * 3, b * 3, lower);
                if upper - lower <= epsilon {
                    count
                } else {
                    add_interpolated(scratch, point_info, events, count, is_self, a * 3, b * 3, upper)
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn add_vertex_if_inside(
    scratch: &[f64], point_info: &[i64], events: &mut [f64], count: usize, is_self: bool, axis: i64, point: usize,
    a: usize, b: usize, c: usize,
) -> usize {
    if point_in_projected_triangle(scratch, scratch[18], axis, point, a, b, c) {
        add_event(scratch, point_info, events, count, is_self, point * 3)
    } else {
        count
    }
}

#[allow(clippy::too_many_arguments)]
fn prepare_points(
    scratch: &mut [f64], point_info: &mut [i64], is_self: bool, tolerance: f64,
    left_positions: &packed::Float3::View::View, a0: i64, a1: i64, a2: i64,
    right_positions: &packed::Float3::View::View, b0: i64, b1: i64, b2: i64,
) -> bool {
    point_info[0] = a0;
    point_info[1] = a1;
    point_info[2] = a2;
    point_info[3] = b0;
    point_info[4] = b1;
    point_info[5] = b2;
    point_info[6] = 0;
    if is_self {
        for source in 0..3_usize {
            let mut already = false;
            for slot in 0..point_info[6] as usize {
                if point_info[7 + slot] == source as i64 {
                    already = true;
                }
            }
            if !already {
                let mut collision = 3;
                let mut found = false;
                while !found && collision <= 5 {
                    found = point_info[source] == point_info[collision];
                    collision += 1;
                }
                if found {
                    point_info[7 + point_info[6] as usize] = source as i64;
                    point_info[6] += 1;
                }
            }
        }
    }
    let (a0, a1, a2) = (a0 as usize, a1 as usize, a2 as usize);
    let (b0, b1, b2) = (b0 as usize, b1 as usize, b2 as usize);
    let origin_x = left_positions.x[a0];
    let origin_y = left_positions.y[a0];
    let origin_z = left_positions.z[a0];
    scratch[0] = 0.0;
    scratch[1] = 0.0;
    scratch[2] = 0.0;
    scratch[3] = left_positions.x[a1] - origin_x;
    scratch[4] = left_positions.y[a1] - origin_y;
    scratch[5] = left_positions.z[a1] - origin_z;
    scratch[6] = left_positions.x[a2] - origin_x;
    scratch[7] = left_positions.y[a2] - origin_y;
    scratch[8] = left_positions.z[a2] - origin_z;
    scratch[9] = right_positions.x[b0] - origin_x;
    scratch[10] = right_positions.y[b0] - origin_y;
    scratch[11] = right_positions.z[b0] - origin_z;
    scratch[12] = right_positions.x[b1] - origin_x;
    scratch[13] = right_positions.y[b1] - origin_y;
    scratch[14] = right_positions.z[b1] - origin_z;
    scratch[15] = right_positions.x[b2] - origin_x;
    scratch[16] = right_positions.y[b2] - origin_y;
    scratch[17] = right_positions.z[b2] - origin_z;
    let mut scale = 0.0_f64;
    for component in 3..=17 {
        scale = float_max(scale, scratch[component].abs());
    }
    if scale == 0.0 {
        return false;
    }
    let inverse = 1.0 / scale;
    for component in 3..=17 {
        scratch[component] *= inverse;
    }
    scratch[18] = float_max(128.0 * f64::EPSILON, tolerance * inverse);
    scratch[19] = origin_x;
    scratch[20] = origin_y;
    scratch[21] = origin_z;
    scratch[22] = scale;
    true
}

/// The same decision/event kernel over explicit packed point indices. This is used by mixed
/// triangle/curve indexes so they do not need to construct a redundant triangle-only BVH.
///
/// Writes at most `MAX_EVENTS` unique pair-local events to `events` and returns the count. Each event
/// stores world position, source barycentric weights, and collision barycentric weights in
/// `EVENT_STRIDE` consecutive floats. The three worker-local arrays must have lengths at least 26,
/// 10, and `MAX_EVENTS * EVENT_STRIDE`.
#[allow(clippy::too_many_arguments)]
pub fn events_points_into(
    scratch: &mut [f64],
    point_info: &mut [i64],
    events: &mut [f64],
    is_self: bool,
    tolerance: f64,
    include_coplanar: bool,
    left_positions: &packed::Float3::View::View,
    left_a: i64,
    left_b: i64,
    left_c: i64,
    right_positions: &packed::Float3::View::View,
    right_a: i64,
    right_b: i64,
    right_c: i64,
) -> Result<usize, error::T> {
    if scratch.len() < 26 || point_info.len() < 10 || events.len() < MAX_EVENTS * EVENT_STRIDE {
        return Err(error::new(
            "triangle_intersection",
            Code::InvalidParameter,
            "Triangle_intersection.events_into: scratch is too small".to_owned(),
        ));
    }
    if !prepare_points(
        scratch, point_info, is_self, tolerance, left_positions, left_a, left_b, left_c, right_positions, right_a,
        right_b, right_c,
    ) {
        return Ok(0);
    }
    let ax = scratch[3];
    let ay = scratch[4];
    let az = scratch[5];
    let bx = scratch[6];
    let by = scratch[7];
    let bz = scratch[8];
    let nx = (ay * bz) - (az * by);
    let ny = (az * bx) - (ax * bz);
    let nz = (ax * by) - (ay * bx);
    let normal_length = ((nx * nx) + (ny * ny) + (nz * nz)).sqrt();
    let epsilon = scratch[18];
    let b0_plane = (nx * scratch[9]) + (ny * scratch[10]) + (nz * scratch[11]);
    let b1_plane = (nx * scratch[12]) + (ny * scratch[13]) + (nz * scratch[14]);
    let b2_plane = (nx * scratch[15]) + (ny * scratch[16]) + (nz * scratch[17]);
    let coplanar = normal_length > 0.0
        && b0_plane.abs() <= epsilon * normal_length
        && b1_plane.abs() <= epsilon * normal_length
        && b2_plane.abs() <= epsilon * normal_length;
    let count = if coplanar {
        if !include_coplanar {
            0
        } else {
            let (anx, any, anz) = (nx.abs(), ny.abs(), nz.abs());
            let axis: i64 = if anx >= any && anx >= anz {
                0
            } else if any >= anz {
                1
            } else {
                2
            };
            let mut count = 0;
            for (point, a, b, c) in [(0, 3, 4, 5), (1, 3, 4, 5), (2, 3, 4, 5), (3, 0, 1, 2), (4, 0, 1, 2), (5, 0, 1, 2)] {
                count = add_vertex_if_inside(scratch, point_info, events, count, is_self, axis, point, a, b, c);
            }
            for (a, b, c, d) in [
                (0, 1, 3, 4),
                (0, 1, 4, 5),
                (0, 1, 5, 3),
                (1, 2, 3, 4),
                (1, 2, 4, 5),
                (1, 2, 5, 3),
                (2, 0, 3, 4),
                (2, 0, 4, 5),
                (2, 0, 5, 3),
            ] {
                count = coplanar_segment_events(scratch, point_info, events, count, is_self, axis, a, b, c, d);
            }
            count
        }
    } else {
        let mut count = 0;
        for (p0, p1, a0, a1, a2) in [
            (0, 3, 9, 12, 15),
            (3, 6, 9, 12, 15),
            (6, 0, 9, 12, 15),
            (9, 12, 0, 3, 6),
            (12, 15, 0, 3, 6),
            (15, 9, 0, 3, 6),
        ] {
            count = segment_triangle_event(scratch, point_info, events, count, is_self, p0, p1, a0, a1, a2);
        }
        count
    };
    let origin_x = scratch[19];
    let origin_y = scratch[20];
    let origin_z = scratch[21];
    let scale = scratch[22];
    for event in 0..count {
        let offset = event * EVENT_STRIDE;
        events[offset] = origin_x + (events[offset] * scale);
        events[offset + 1] = origin_y + (events[offset + 1] * scale);
        events[offset + 2] = origin_z + (events[offset + 2] * scale);
    }
    Ok(count)
}

/// Pair-local events between a left and a right triangle of two indexed surfaces. Ordinary
/// shared-point/shared-edge contact is suppressed in self mode; duplicate triangles and overlap
/// beyond a shared edge remain visible.
#[allow(clippy::too_many_arguments)]
pub fn events_into(
    scratch: &mut [f64],
    point_info: &mut [i64],
    events: &mut [f64],
    is_self: bool,
    tolerance: f64,
    include_coplanar: bool,
    left: &Surface,
    left_triangle: i64,
    right: &Surface,
    right_triangle: i64,
) -> Result<usize, error::T> {
    let left_vertices = &left.topology.vertex_points;
    let right_vertices = &right.topology.vertex_points;
    let corner = |surface: &Surface, vertices: &[i64], triangle: i64, local: i64| {
        vertices[surface_index::View::triangle_vertex(&surface.index, triangle, local) as usize]
    };
    events_points_into(
        scratch,
        point_info,
        events,
        is_self,
        tolerance,
        include_coplanar,
        &left.positions,
        corner(left, left_vertices, left_triangle, 0),
        corner(left, left_vertices, left_triangle, 1),
        corner(left, left_vertices, left_triangle, 2),
        &right.positions,
        corner(right, right_vertices, right_triangle, 0),
        corner(right, right_vertices, right_triangle, 1),
        corner(right, right_vertices, right_triangle, 2),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn view(points: &[[f64; 3]]) -> packed::Float3::View::View {
        packed::Float3::View::View {
            x: points.iter().map(|p| p[0]).collect::<Vec<_>>().into(),
            y: points.iter().map(|p| p[1]).collect::<Vec<_>>().into(),
            z: points.iter().map(|p| p[2]).collect::<Vec<_>>().into(),
        }
    }

    fn run(positions: &packed::Float3::View::View, is_self: bool, include_coplanar: bool, right: [i64; 3]) -> (usize, Vec<f64>) {
        let mut scratch = [0.0; 26];
        let mut point_info = [0_i64; 10];
        let mut events = [0.0; MAX_EVENTS * EVENT_STRIDE];
        let count = events_points_into(
            &mut scratch, &mut point_info, &mut events, is_self, 0.0, include_coplanar, positions, 0, 1, 2, positions,
            right[0], right[1], right[2],
        )
        .unwrap();
        (count, events[..count * EVENT_STRIDE].to_vec())
    }

    #[test]
    fn crossing_triangles_make_two_events_and_shared_edges_are_suppressed() {
        // triangle 0..3 in z = 0; triangle 3..6 pierces it vertically through (0.25, 0.25) and (0.5, 0.25)
        let positions = view(&[
            [0.0, 0.0, 0.0],
            [2.0, 0.0, 0.0],
            [0.0, 2.0, 0.0],
            [0.25, 0.25, -1.0],
            [0.5, 0.25, 1.0],
            [0.25, 0.25, 1.0],
        ]);
        let (count, events) = run(&positions, false, true, [3, 4, 5]);
        assert!(count >= 1 && count <= MAX_EVENTS);
        assert_eq!(events.len(), count * EVENT_STRIDE);
        assert!((events[2]).abs() < 1e-12, "events lie in the z = 0 plane");
        // a triangle sharing an edge with the first one only touches along that shared edge
        let touching = view(&[
            [0.0, 0.0, 0.0],
            [2.0, 0.0, 0.0],
            [0.0, 2.0, 0.0],
            [2.0, 2.0, 0.0],
        ]);
        let (visible, _) = run(&touching, false, true, [1, 2, 3]);
        let (suppressed, _) = run(&touching, true, true, [1, 2, 3]);
        assert!(visible > suppressed);
        assert_eq!(suppressed, 0);
        let (none, _) = run(&touching, false, false, [1, 2, 3]);
        assert_eq!(none, 0);
        let mut tiny = [0.0; 4];
        let mut info = [0_i64; 10];
        let mut events = [0.0; MAX_EVENTS * EVENT_STRIDE];
        assert!(events_points_into(&mut tiny, &mut info, &mut events, false, 0.0, true, &touching, 0, 1, 2, &touching, 1, 2, 3)
            .is_err());
    }
}
