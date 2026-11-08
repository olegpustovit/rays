//! Allocation-free polygon-curve narrow phase used by Intersection Analysis.
//!
//! Event records use `Triangle_intersection::EVENT_STRIDE`: world position, then left and right
//! primitive UVW triplets. A curve UVW is `(u, 0, 0)` and a triangle UVW is barycentric.
use rdk_core::{error, packed};
use rdk_core::error::Code;

use crate::point_grid::{float_max, float_min};
use crate::triangle_intersection::{clamp01, EVENT_STRIDE as STRIDE};

#[inline]
fn dot(ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64) -> f64 {
    (ax * bx) + (ay * by) + (az * bz)
}

#[allow(clippy::too_many_arguments)]
fn barycentric(
    px: f64, py: f64, pz: f64, ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64, cx: f64, cy: f64, cz: f64,
    output: &mut [f64], offset: usize,
) -> bool {
    let v0x = bx - ax;
    let v0y = by - ay;
    let v0z = bz - az;
    let v1x = cx - ax;
    let v1y = cy - ay;
    let v1z = cz - az;
    let v2x = px - ax;
    let v2y = py - ay;
    let v2z = pz - az;
    let d00 = dot(v0x, v0y, v0z, v0x, v0y, v0z);
    let d01 = dot(v0x, v0y, v0z, v1x, v1y, v1z);
    let d11 = dot(v1x, v1y, v1z, v1x, v1y, v1z);
    let d20 = dot(v2x, v2y, v2z, v0x, v0y, v0z);
    let d21 = dot(v2x, v2y, v2z, v1x, v1y, v1z);
    let denominator = (d00 * d11) - (d01 * d01);
    if denominator <= 0.0 {
        return false;
    }
    let v = clamp01(((d11 * d20) - (d01 * d21)) / denominator);
    let w = clamp01(((d00 * d21) - (d01 * d20)) / denominator);
    let u = clamp01(1.0 - v - w);
    let sum = u + v + w;
    output[offset] = u / sum;
    output[offset + 1] = v / sum;
    output[offset + 2] = w / sum;
    true
}

fn load_relative(scratch: &mut [f64], target: usize, positions: &packed::float3::view::View, point: i64, ox: f64, oy: f64, oz: f64) {
    let p = point as usize;
    scratch[target] = positions.x[p] - ox;
    scratch[target + 1] = positions.y[p] - oy;
    scratch[target + 2] = positions.z[p] - oz;
}

fn normalize(scratch: &mut [f64], tolerance: f64, last: usize, ox: f64, oy: f64, oz: f64) -> bool {
    let mut scale = 0.0_f64;
    for component in 0..=last {
        scale = float_max(scale, scratch[component].abs());
    }
    if scale == 0.0 {
        return false;
    }
    let inverse = 1.0 / scale;
    for component in 0..=last {
        scratch[component] *= inverse;
    }
    scratch[18] = float_max(128.0 * f64::EPSILON, tolerance * inverse);
    scratch[19] = ox;
    scratch[20] = oy;
    scratch[21] = oz;
    scratch[22] = scale;
    true
}

#[allow(clippy::too_many_arguments)]
fn shared_point_suppressed(
    is_self: bool, shared_count: i64, shared_x: f64, shared_y: f64, shared_z: f64, epsilon: f64, x: f64, y: f64,
    z: f64,
) -> bool {
    if !(is_self && shared_count == 1) {
        return false;
    }
    let dx = x - shared_x;
    let dy = y - shared_y;
    let dz = z - shared_z;
    (dx * dx) + (dy * dy) + (dz * dz) <= epsilon * epsilon
}

#[allow(clippy::too_many_arguments)]
fn emit_segment_segment(
    events: &mut [f64], count: usize, left_u0: f64, left_u1: f64, right_u0: f64, right_u1: f64, left_t: f64,
    right_t: f64, x: f64, y: f64, z: f64,
) -> usize {
    let offset = count * STRIDE;
    events[offset] = x;
    events[offset + 1] = y;
    events[offset + 2] = z;
    events[offset + 3] = left_u0 + (clamp01(left_t) * (left_u1 - left_u0));
    events[offset + 4] = 0.0;
    events[offset + 5] = 0.0;
    events[offset + 6] = right_u0 + (clamp01(right_t) * (right_u1 - right_u0));
    events[offset + 7] = 0.0;
    events[offset + 8] = 0.0;
    count + 1
}

fn too_small(operation: &str) -> error::T {
    error::new(
        "curve_intersection",
        Code::InvalidParameter,
        format!("Curve_intersection.{operation}: scratch too small"),
    )
}

fn restore_world(scratch: &[f64], events: &mut [f64], count: usize) {
    let scale = scratch[22];
    let (ox, oy, oz) = (scratch[19], scratch[20], scratch[21]);
    for event in 0..count {
        let offset = event * STRIDE;
        events[offset] = ox + (events[offset] * scale);
        events[offset + 1] = oy + (events[offset + 1] * scale);
        events[offset + 2] = oz + (events[offset + 2] * scale);
    }
}

/// Events between two curve segments; returns the event count (at most two).
#[allow(clippy::too_many_arguments)]
pub fn segment_segment_events_into(
    scratch: &mut [f64],
    events: &mut [f64],
    is_self: bool,
    tolerance: f64,
    left_positions: &packed::float3::view::View,
    left_a: i64,
    left_b: i64,
    left_u0: f64,
    left_u1: f64,
    right_positions: &packed::float3::view::View,
    right_a: i64,
    right_b: i64,
    right_u0: f64,
    right_u1: f64,
) -> Result<usize, error::T> {
    if scratch.len() < 26 || events.len() < 2 * STRIDE {
        return Err(too_small("segment_segment_events_into"));
    }
    let la = left_a as usize;
    let (ox, oy, oz) = (left_positions.x[la], left_positions.y[la], left_positions.z[la]);
    scratch[0] = 0.0;
    scratch[1] = 0.0;
    scratch[2] = 0.0;
    load_relative(scratch, 3, left_positions, left_b, ox, oy, oz);
    load_relative(scratch, 6, right_positions, right_a, ox, oy, oz);
    load_relative(scratch, 9, right_positions, right_b, ox, oy, oz);
    if !normalize(scratch, tolerance, 11, ox, oy, oz) {
        return Ok(0);
    }
    let (d1x, d1y, d1z) = (scratch[3], scratch[4], scratch[5]);
    let (p2x, p2y, p2z) = (scratch[6], scratch[7], scratch[8]);
    let d2x = scratch[9] - p2x;
    let d2y = scratch[10] - p2y;
    let d2z = scratch[11] - p2z;
    let a = dot(d1x, d1y, d1z, d1x, d1y, d1z);
    let e = dot(d2x, d2y, d2z, d2x, d2y, d2z);
    let b = dot(d1x, d1y, d1z, d2x, d2y, d2z);
    let c = -dot(d1x, d1y, d1z, p2x, p2y, p2z);
    let f = -dot(d2x, d2y, d2z, p2x, p2y, p2z);
    let epsilon = scratch[18];
    let cross_x = (d1y * d2z) - (d1z * d2y);
    let cross_y = (d1z * d2x) - (d1x * d2z);
    let cross_z = (d1x * d2y) - (d1y * d2x);
    let offset_cross_x = (p2y * d1z) - (p2z * d1y);
    let offset_cross_y = (p2z * d1x) - (p2x * d1z);
    let offset_cross_z = (p2x * d1y) - (p2y * d1x);
    let parallel2 = dot(cross_x, cross_y, cross_z, cross_x, cross_y, cross_z);
    let collinear2 = dot(offset_cross_x, offset_cross_y, offset_cross_z, offset_cross_x, offset_cross_y, offset_cross_z);
    let shares = |point: i64| point == right_a || point == right_b;
    let shared_count: i64 = if is_self { i64::from(shares(left_a)) + i64::from(shares(left_b)) } else { 0 };
    let shared_point = if shared_count == 1 {
        if shares(left_a) { 0 } else { 3 }
    } else {
        0
    };
    let (shared_x, shared_y, shared_z) = (scratch[shared_point], scratch[shared_point + 1], scratch[shared_point + 2]);
    let count = if parallel2 <= epsilon * epsilon * a * e && collinear2 <= epsilon * epsilon * a {
        let t0 = dot(p2x, p2y, p2z, d1x, d1y, d1z) / a;
        let t1 = dot(scratch[9], scratch[10], scratch[11], d1x, d1y, d1z) / a;
        let lower = float_max(0.0, float_min(t0, t1));
        let upper = float_min(1.0, float_max(t0, t1));
        if lower > upper + epsilon {
            0
        } else {
            let emit = |events: &mut [f64], count: usize, t: f64| -> usize {
                let x = t * d1x;
                let y = t * d1y;
                let z = t * d1z;
                if shared_point_suppressed(is_self, shared_count, shared_x, shared_y, shared_z, epsilon, x, y, z) {
                    count
                } else {
                    let right_t = dot(x - p2x, y - p2y, z - p2z, d2x, d2y, d2z) / e;
                    emit_segment_segment(events, count, left_u0, left_u1, right_u0, right_u1, t, right_t, x, y, z)
                }
            };
            let count = emit(events, 0, lower);
            if upper - lower <= epsilon { count } else { emit(events, count, upper) }
        }
    } else {
        let denominator = (a * e) - (b * b);
        let mut left_t = if denominator <= epsilon * epsilon {
            0.0
        } else {
            clamp01(((b * f) - (c * e)) / denominator)
        };
        let mut right_t = ((b * left_t) + f) / e;
        if right_t < 0.0 {
            right_t = 0.0;
            left_t = clamp01(-c / a);
        } else if right_t > 1.0 {
            right_t = 1.0;
            left_t = clamp01((b - c) / a);
        }
        let lx = left_t * d1x;
        let ly = left_t * d1y;
        let lz = left_t * d1z;
        let rx = p2x + (right_t * d2x);
        let ry = p2y + (right_t * d2y);
        let rz = p2z + (right_t * d2z);
        let dx = lx - rx;
        let dy = ly - ry;
        let dz = lz - rz;
        if (dx * dx) + (dy * dy) + (dz * dz) > epsilon * epsilon {
            0
        } else {
            let x = (lx + rx) * 0.5;
            let y = (ly + ry) * 0.5;
            let z = (lz + rz) * 0.5;
            if shared_point_suppressed(is_self, shared_count, shared_x, shared_y, shared_z, epsilon, x, y, z) {
                0
            } else {
                emit_segment_segment(events, 0, left_u0, left_u1, right_u0, right_u1, left_t, right_t, x, y, z)
            }
        }
    };
    restore_world(scratch, events, count);
    Ok(count)
}

#[allow(clippy::too_many_arguments)]
fn emit_segment_triangle(
    events: &mut [f64], count: usize, segment_first: bool, segment_u0: f64, segment_u1: f64, along: f64, x: f64,
    y: f64, z: f64, ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64, cx: f64, cy: f64, cz: f64,
) -> usize {
    let offset = count * STRIDE;
    events[offset] = x;
    events[offset + 1] = y;
    events[offset + 2] = z;
    let (segment_offset, triangle_offset) = if segment_first { (offset + 3, offset + 6) } else { (offset + 6, offset + 3) };
    events[segment_offset] = segment_u0 + (clamp01(along) * (segment_u1 - segment_u0));
    events[segment_offset + 1] = 0.0;
    events[segment_offset + 2] = 0.0;
    if barycentric(x, y, z, ax, ay, az, bx, by, bz, cx, cy, cz, events, triangle_offset) {
        count + 1
    } else {
        count
    }
}

/// Events between a curve segment and a triangle; returns the event count (at most two).
#[allow(clippy::too_many_arguments)]
pub fn segment_triangle_events_into(
    scratch: &mut [f64],
    events: &mut [f64],
    is_self: bool,
    tolerance: f64,
    include_coplanar: bool,
    segment_first: bool,
    segment_positions: &packed::float3::view::View,
    segment_a: i64,
    segment_b: i64,
    segment_u0: f64,
    segment_u1: f64,
    triangle_positions: &packed::float3::view::View,
    triangle_a: i64,
    triangle_b: i64,
    triangle_c: i64,
) -> Result<usize, error::T> {
    if scratch.len() < 26 || events.len() < 2 * STRIDE {
        return Err(too_small("segment_triangle_events_into"));
    }
    let sa = segment_a as usize;
    let (ox, oy, oz) = (segment_positions.x[sa], segment_positions.y[sa], segment_positions.z[sa]);
    scratch[0] = 0.0;
    scratch[1] = 0.0;
    scratch[2] = 0.0;
    load_relative(scratch, 3, segment_positions, segment_b, ox, oy, oz);
    load_relative(scratch, 6, triangle_positions, triangle_a, ox, oy, oz);
    load_relative(scratch, 9, triangle_positions, triangle_b, ox, oy, oz);
    load_relative(scratch, 12, triangle_positions, triangle_c, ox, oy, oz);
    if !normalize(scratch, tolerance, 14, ox, oy, oz) {
        return Ok(0);
    }
    let (dx, dy, dz) = (scratch[3], scratch[4], scratch[5]);
    let (ax, ay, az) = (scratch[6], scratch[7], scratch[8]);
    let (bx, by, bz) = (scratch[9], scratch[10], scratch[11]);
    let (cx, cy, cz) = (scratch[12], scratch[13], scratch[14]);
    let epsilon = scratch[18];
    let e1x = bx - ax;
    let e1y = by - ay;
    let e1z = bz - az;
    let e2x = cx - ax;
    let e2y = cy - ay;
    let e2z = cz - az;
    let nx = (e1y * e2z) - (e1z * e2y);
    let ny = (e1z * e2x) - (e1x * e2z);
    let nz = (e1x * e2y) - (e1y * e2x);
    let normal_length = dot(nx, ny, nz, nx, ny, nz).sqrt();
    let p0_plane = -dot(nx, ny, nz, ax, ay, az);
    let p1_plane = dot(nx, ny, nz, dx - ax, dy - ay, dz - az);
    let coplanar = p0_plane.abs() <= epsilon * normal_length && p1_plane.abs() <= epsilon * normal_length;
    let on_triangle = |point: i64| point == triangle_a || point == triangle_b || point == triangle_c;
    let shared_count: i64 =
        if is_self { i64::from(on_triangle(segment_a)) + i64::from(on_triangle(segment_b)) } else { 0 };
    let suppress = |scratch: &[f64], x: f64, y: f64, z: f64| -> bool {
        if !is_self || shared_count == 0 {
            false
        } else if shared_count == 2 {
            true
        } else {
            let point = if on_triangle(segment_a) { 0 } else { 3 };
            let ex = x - scratch[point];
            let ey = y - scratch[point + 1];
            let ez = z - scratch[point + 2];
            (ex * ex) + (ey * ey) + (ez * ez) <= epsilon * epsilon
        }
    };
    let emit = |scratch: &[f64], events: &mut [f64], count: usize, along: f64| -> usize {
        let along = clamp01(along);
        let x = along * dx;
        let y = along * dy;
        let z = along * dz;
        if suppress(scratch, x, y, z) {
            count
        } else {
            emit_segment_triangle(
                events, count, segment_first, segment_u0, segment_u1, along, x, y, z, ax, ay, az, bx, by, bz, cx, cy, cz,
            )
        }
    };
    let count = if coplanar {
        if !include_coplanar {
            0
        } else {
            let (anx, any, anz) = (nx.abs(), ny.abs(), nz.abs());
            let drop = if anx >= any && anx >= anz {
                0
            } else if any >= anz {
                1
            } else {
                2
            };
            let project = |x: f64, y: f64, z: f64| -> (f64, f64) {
                if drop == 0 {
                    (y, z)
                } else if drop == 1 {
                    (x, z)
                } else {
                    (x, y)
                }
            };
            let (sx, sy) = project(0.0, 0.0, 0.0);
            let (ex, ey) = project(dx, dy, dz);
            let (a2x, a2y) = project(ax, ay, az);
            let (b2x, b2y) = project(bx, by, bz);
            let (c2x, c2y) = project(cx, cy, cz);
            let orient = |x0: f64, y0: f64, x1: f64, y1: f64, x: f64, y: f64| ((x1 - x0) * (y - y0)) - ((y1 - y0) * (x - x0));
            let area = orient(a2x, a2y, b2x, b2y, c2x, c2y);
            let sign = if area >= 0.0 { 1.0 } else { -1.0 };
            let mut lower = 0.0_f64;
            let mut upper = 1.0_f64;
            let mut valid = true;
            let mut clip = |x0: f64, y0: f64, x1: f64, y1: f64| {
                let f0 = sign * orient(x0, y0, x1, y1, sx, sy);
                let f1 = sign * orient(x0, y0, x1, y1, ex, ey);
                if f0 < -epsilon && f1 < -epsilon {
                    valid = false;
                } else if f0 < -epsilon || f1 < -epsilon {
                    let at = f0 / (f0 - f1);
                    if f0 < -epsilon {
                        lower = float_max(lower, at);
                    } else {
                        upper = float_min(upper, at);
                    }
                }
            };
            clip(a2x, a2y, b2x, b2y);
            clip(b2x, b2y, c2x, c2y);
            clip(c2x, c2y, a2x, a2y);
            if !valid || lower > upper + epsilon {
                0
            } else {
                let count = emit(scratch, events, 0, lower);
                if upper - lower <= epsilon { count } else { emit(scratch, events, count, upper) }
            }
        }
    } else {
        let px = (dy * e2z) - (dz * e2y);
        let py = (dz * e2x) - (dx * e2z);
        let pz = (dx * e2y) - (dy * e2x);
        let determinant = dot(e1x, e1y, e1z, px, py, pz);
        if determinant.abs() <= epsilon {
            0
        } else {
            let inverse = 1.0 / determinant;
            let (tx, ty, tz) = (-ax, -ay, -az);
            let u = dot(tx, ty, tz, px, py, pz) * inverse;
            if u < -epsilon || u > 1.0 + epsilon {
                0
            } else {
                let qx = (ty * e1z) - (tz * e1y);
                let qy = (tz * e1x) - (tx * e1z);
                let qz = (tx * e1y) - (ty * e1x);
                let v = dot(dx, dy, dz, qx, qy, qz) * inverse;
                if v < -epsilon || u + v > 1.0 + epsilon {
                    0
                } else {
                    let along = dot(e2x, e2y, e2z, qx, qy, qz) * inverse;
                    if along < -epsilon || along > 1.0 + epsilon { 0 } else { emit(scratch, events, 0, along) }
                }
            }
        }
    };
    restore_world(scratch, events, count);
    Ok(count)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn view(points: &[[f64; 3]]) -> packed::float3::view::View {
        packed::float3::view::View {
            x: points.iter().map(|p| p[0]).collect::<Vec<_>>().into(),
            y: points.iter().map(|p| p[1]).collect::<Vec<_>>().into(),
            z: points.iter().map(|p| p[2]).collect::<Vec<_>>().into(),
        }
    }

    #[test]
    fn crossing_segments_meet_once_and_shared_endpoint_is_suppressed() {
        let positions = view(&[[-1.0, 0.0, 0.0], [1.0, 0.0, 0.0], [0.0, -1.0, 0.0], [0.0, 1.0, 0.0], [5.0, 0.0, 0.0]]);
        let mut scratch = [0.0; 26];
        let mut events = [0.0; 2 * STRIDE];
        let count = segment_segment_events_into(
            &mut scratch, &mut events, false, 0.0, &positions, 0, 1, 0.0, 1.0, &positions, 2, 3, 0.0, 1.0,
        )
        .unwrap();
        assert_eq!(count, 1);
        assert_eq!(&events[..3], &[0.0, 0.0, 0.0]);
        assert_eq!((events[3], events[6]), (0.5, 0.5));
        let miss = segment_segment_events_into(
            &mut scratch, &mut events, false, 0.0, &positions, 0, 1, 0.0, 1.0, &positions, 4, 4, 0.0, 1.0,
        )
        .unwrap();
        assert_eq!(miss, 0);
        // two segments of one curve meeting at a shared point 1
        let curve = view(&[[0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [1.0, 1.0, 0.0]]);
        let touching = segment_segment_events_into(
            &mut scratch, &mut events, false, 0.0, &curve, 0, 1, 0.0, 0.5, &curve, 1, 2, 0.5, 1.0,
        )
        .unwrap();
        assert_eq!(touching, 1);
        let ignored = segment_segment_events_into(
            &mut scratch, &mut events, true, 0.0, &curve, 0, 1, 0.0, 0.5, &curve, 1, 2, 0.5, 1.0,
        )
        .unwrap();
        assert_eq!(ignored, 0);
        assert!(segment_segment_events_into(&mut [0.0; 3], &mut events, false, 0.0, &curve, 0, 1, 0.0, 1.0, &curve, 1, 2, 0.0, 1.0)
            .is_err());
    }

    #[test]
    fn segment_pierces_triangle_with_curve_and_triangle_weights() {
        let positions = view(&[
            [0.0, 0.0, 0.0],
            [4.0, 0.0, 0.0],
            [0.0, 4.0, 0.0],
            [1.0, 1.0, -1.0],
            [1.0, 1.0, 1.0],
        ]);
        let mut scratch = [0.0; 26];
        let mut events = [0.0; 2 * STRIDE];
        let run = |scratch: &mut [f64], events: &mut [f64], segment_first: bool| {
            segment_triangle_events_into(
                scratch, events, false, 0.0, true, segment_first, &positions, 3, 4, 0.0, 1.0, &positions, 0, 1, 2,
            )
            .unwrap()
        };
        assert_eq!(run(&mut scratch, &mut events, true), 1);
        assert_eq!(&events[..3], &[1.0, 1.0, 0.0]);
        assert_eq!(events[3], 0.5);
        assert_eq!(events[6..9].iter().sum::<f64>(), 1.0);
        assert_eq!(run(&mut scratch, &mut events, false), 1);
        assert_eq!(events[6], 0.5);
        assert_eq!(events[3..6].iter().sum::<f64>(), 1.0);
    }
}
