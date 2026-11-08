//! Port of lib/rdk/boolean/boolean_constraints.ml: exact triangle-triangle constraint planning
//! for the Boolean pipeline (broad phase, exact classification, implicit event points, CSR
//! incidence per face).
#![allow(non_snake_case)]

use std::sync::Arc;

use rdk_core::{cancel, error, geometry, packed, support, topology};
use rdk_core::error::Code;
use rdk_geom::implicit_point::{self, Error as PointError};
use rdk_geom::predicates::{self, Sign};
use rdk_geom::surface_index;

pub const OPERATION: &str = "boolean_constraints";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
}

/// OCaml's optional arguments of `build`; `grain` is required there too.
#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub resolve_left_self_intersections: bool,
    pub resolve_right_self_intersections: bool,
    pub skip_opposite_self_pairs: bool,
    pub grain: i64,
}

pub fn params(grain: i64) -> Params {
    Params {
        cancel: None,
        resolve_left_self_intersections: false,
        resolve_right_self_intersections: false,
        skip_opposite_self_pairs: false,
        grain,
    }
}

pub struct T {
    left_geometry: geometry::T,
    right_geometry: geometry::T,
    resolve_left_self_intersections: bool,
    resolve_right_self_intersections: bool,
    left_surface: surface_index::T,
    right_surface: surface_index::T,
    source: implicit_point::Source,
    source_points: Vec<implicit_point::T>,
    left_triangle_points: Vec<i64>,
    right_triangle_points: Vec<i64>,
    points: Vec<implicit_point::T>,
    first: Vec<i64>,
    second: Vec<i64>,
    constraint_first_sides: Vec<u8>,
    constraint_first_triangles: Vec<i64>,
    constraint_second_sides: Vec<u8>,
    constraint_second_triangles: Vec<i64>,
    left_offsets: Vec<i64>,
    left_constraints: Vec<i64>,
    right_offsets: Vec<i64>,
    right_constraints: Vec<i64>,
    coplanar_first_sides: Vec<u8>,
    coplanar_first_triangles: Vec<i64>,
    coplanar_second_sides: Vec<u8>,
    coplanar_second_triangles: Vec<i64>,
    degenerate_pairs: i64,
}

fn fail(code: Code, message: &str) -> error::T {
    error::new(OPERATION, code, message.to_owned())
}

pub fn point_count(value: &T) -> i64 {
    value.points.len() as i64
}
pub fn constraint_count(value: &T) -> i64 {
    value.first.len() as i64
}
pub fn constraint_first(value: &T, constraint_index: i64) -> i64 {
    value.first[constraint_index as usize]
}
pub fn constraint_second(value: &T, constraint_index: i64) -> i64 {
    value.second[constraint_index as usize]
}

fn side_at(sides: &[u8], index: i64) -> Side {
    if sides[index as usize] == 0 { Side::Left } else { Side::Right }
}

pub fn constraint_first_side(value: &T, constraint_index: i64) -> Side {
    side_at(&value.constraint_first_sides, constraint_index)
}
pub fn constraint_second_side(value: &T, constraint_index: i64) -> Side {
    side_at(&value.constraint_second_sides, constraint_index)
}
pub fn constraint_first_triangle(value: &T, constraint_index: i64) -> i64 {
    value.constraint_first_triangles[constraint_index as usize]
}
pub fn constraint_second_triangle(value: &T, constraint_index: i64) -> i64 {
    value.constraint_second_triangles[constraint_index as usize]
}
pub fn left_triangle_count(value: &T) -> i64 {
    value.left_offsets.len() as i64 - 1
}
pub fn right_triangle_count(value: &T) -> i64 {
    value.right_offsets.len() as i64 - 1
}
pub fn left_constraint_range(value: &T, triangle: i64) -> (i64, i64) {
    (value.left_offsets[triangle as usize], value.left_offsets[triangle as usize + 1])
}
pub fn right_constraint_range(value: &T, triangle: i64) -> (i64, i64) {
    (value.right_offsets[triangle as usize], value.right_offsets[triangle as usize + 1])
}
pub fn left_constraint(value: &T, slot: i64) -> i64 {
    value.left_constraints[slot as usize]
}
pub fn right_constraint(value: &T, slot: i64) -> i64 {
    value.right_constraints[slot as usize]
}
pub fn coplanar_pair_count(value: &T) -> i64 {
    value.coplanar_first_triangles.len() as i64
}
pub fn coplanar_first_side(value: &T, pair: i64) -> Side {
    side_at(&value.coplanar_first_sides, pair)
}
pub fn coplanar_second_side(value: &T, pair: i64) -> Side {
    side_at(&value.coplanar_second_sides, pair)
}
pub fn coplanar_first_triangle(value: &T, pair: i64) -> i64 {
    value.coplanar_first_triangles[pair as usize]
}
pub fn coplanar_second_triangle(value: &T, pair: i64) -> i64 {
    value.coplanar_second_triangles[pair as usize]
}
pub fn degenerate_pair_count(value: &T) -> i64 {
    value.degenerate_pairs
}

pub mod View {
    use super::*;

    pub fn left_geometry(value: &T) -> &geometry::T {
        &value.left_geometry
    }
    pub fn right_geometry(value: &T) -> &geometry::T {
        &value.right_geometry
    }
    pub fn resolve_left_self_intersections(value: &T) -> bool {
        value.resolve_left_self_intersections
    }
    pub fn resolve_right_self_intersections(value: &T) -> bool {
        value.resolve_right_self_intersections
    }
    pub fn left_surface(value: &T) -> &surface_index::T {
        &value.left_surface
    }
    pub fn right_surface(value: &T) -> &surface_index::T {
        &value.right_surface
    }
    pub fn point(value: &T, point: i64) -> &implicit_point::T {
        &value.points[point as usize]
    }
    pub fn source(value: &T) -> &implicit_point::Source {
        &value.source
    }
    pub fn source_point_count(value: &T) -> i64 {
        value.source_points.len() as i64
    }
    pub fn source_point(value: &T, point: i64) -> &implicit_point::T {
        &value.source_points[point as usize]
    }
    pub fn left_triangle_point(value: &T, triangle: i64, local: i64) -> i64 {
        value.left_triangle_points[(triangle * 3 + local) as usize]
    }
    pub fn right_triangle_point(value: &T, triangle: i64, local: i64) -> i64 {
        value.right_triangle_points[(triangle * 3 + local) as usize]
    }
    pub fn triangle_point(value: &T, side: Side, triangle: i64, local: i64) -> i64 {
        match side {
            Side::Left => left_triangle_point(value, triangle, local),
            Side::Right => right_triangle_point(value, triangle, local),
        }
    }
}

fn concatenate_positions(
    left: &geometry::T,
    right: &geometry::T,
) -> Result<(i64, Vec<f64>, Vec<f64>, Vec<f64>), error::T> {
    let left = packed::Float3::View::view(geometry::positions(left))?;
    let right = packed::Float3::View::view(geometry::positions(right))?;
    let total = left.x.len() + right.x.len();
    let mut x = Vec::with_capacity(total);
    let mut y = Vec::with_capacity(total);
    let mut z = Vec::with_capacity(total);
    x.extend_from_slice(&left.x);
    x.extend_from_slice(&right.x);
    y.extend_from_slice(&left.y);
    y.extend_from_slice(&right.y);
    z.extend_from_slice(&left.z);
    z.extend_from_slice(&right.z);
    Ok((left.x.len() as i64, x, y, z))
}

fn all_triangle_points(vertex_points: &[i64], surface: &surface_index::T, offset: i64) -> Vec<i64> {
    let triangle_count = surface_index::triangle_count(surface);
    let mut output = vec![0i64; (triangle_count * 3) as usize];
    for triangle in 0..triangle_count {
        for local in 0..3 {
            let vertex = surface_index::View::triangle_vertex(surface, triangle, local);
            output[(triangle * 3 + local) as usize] = offset + vertex_points[vertex as usize];
        }
    }
    output
}

fn local_edge(triangle: &[i64; 3], edge: i64) -> (i64, i64) {
    (triangle[edge as usize], triangle[((edge + 1) % 3) as usize])
}

fn edge_constant_components(source: &implicit_point::Source, triangle: &[i64; 3], edge: i64) -> i64 {
    let (first, second) = local_edge(triangle, edge);
    implicit_point::constant_components(source, first, second)
}

fn construct_event(
    source: &implicit_point::Source,
    source_points: &[implicit_point::T],
    left_triangle: &[i64; 3],
    right_triangle: &[i64; 3],
    left_feature: i64,
    right_feature: i64,
) -> Result<implicit_point::T, PointError> {
    let left_vertex = predicates::view::triangle_feature_vertex_local(left_feature);
    let right_vertex = predicates::view::triangle_feature_vertex_local(right_feature);
    if left_vertex >= 0 {
        return Ok(source_points[left_triangle[left_vertex as usize] as usize].clone());
    }
    if right_vertex >= 0 {
        return Ok(source_points[right_triangle[right_vertex as usize] as usize].clone());
    }
    let construct = |triangle: &[i64; 3], edge: i64, plane: &[i64; 3]| {
        let (line_start, line_end) = local_edge(triangle, edge);
        implicit_point::line_plane(source, line_start, line_end, plane[0], plane[1], plane[2])
    };
    let left_edge = predicates::view::triangle_feature_edge_local(left_feature);
    let right_edge = predicates::view::triangle_feature_edge_local(right_feature);
    if left_edge >= 0
        && right_edge >= 0
        && edge_constant_components(source, right_triangle, right_edge)
            > edge_constant_components(source, left_triangle, left_edge)
    {
        match construct(right_triangle, right_edge, left_triangle) {
            Err(PointError::ParallelLineAndPlane) => construct(left_triangle, left_edge, right_triangle),
            other => other,
        }
    } else if left_edge >= 0 {
        match construct(left_triangle, left_edge, right_triangle) {
            Err(PointError::ParallelLineAndPlane) => {
                if right_edge >= 0 {
                    construct(right_triangle, right_edge, left_triangle)
                } else {
                    Err(PointError::ParallelLineAndPlane)
                }
            }
            other => other,
        }
    } else if right_edge >= 0 {
        construct(right_triangle, right_edge, left_triangle)
    } else {
        Err(PointError::DependentPlanes)
    }
}

fn compare_points(points: &[Option<implicit_point::T>], left: usize, right: usize) -> std::cmp::Ordering {
    let left_point = points[left].as_ref().unwrap();
    let right_point = points[right].as_ref().unwrap();
    let mut comparison = implicit_point::compare_x(left_point, right_point);
    if comparison == 0 {
        comparison = implicit_point::compare_y(left_point, right_point);
    }
    if comparison == 0 {
        comparison = implicit_point::compare_z(left_point, right_point);
    }
    if comparison != 0 { comparison.cmp(&0) } else { left.cmp(&right) }
}

fn compact_pairs(
    counts: &[i64],
    expected: usize,
    first_candidate_sides: &[u8],
    first_candidates: &[i64],
    second_candidate_sides: &[u8],
    second_candidates: &[i64],
) -> (Vec<u8>, Vec<i64>, Vec<u8>, Vec<i64>) {
    let mut first_sides = Vec::with_capacity(expected);
    let mut second_sides = Vec::with_capacity(expected);
    let mut first = Vec::with_capacity(expected);
    let mut second = Vec::with_capacity(expected);
    for candidate in 0..counts.len() {
        if counts[candidate] == -1 {
            first_sides.push(first_candidate_sides[candidate]);
            second_sides.push(second_candidate_sides[candidate]);
            first.push(first_candidates[candidate]);
            second.push(second_candidates[candidate]);
        }
    }
    (first_sides, first, second_sides, second)
}

fn make_face_csr(
    target_side: Side,
    triangle_count: i64,
    first_sides: &[u8],
    first_triangles: &[i64],
    second_sides: &[u8],
    second_triangles: &[i64],
) -> (Vec<i64>, Vec<i64>) {
    let n = triangle_count as usize;
    let mut counts = vec![0i64; n];
    for c in 0..first_triangles.len() {
        if side_at(first_sides, c as i64) == target_side {
            counts[first_triangles[c] as usize] += 1;
        }
        if side_at(second_sides, c as i64) == target_side {
            counts[second_triangles[c] as usize] += 1;
        }
    }
    let mut offsets = vec![0i64; n + 1];
    for t in 0..n {
        offsets[t + 1] = offsets[t] + counts[t];
    }
    let mut values = vec![0i64; offsets[n] as usize];
    let mut cursors = offsets.clone();
    for c in 0..first_triangles.len() {
        if side_at(first_sides, c as i64) == target_side {
            let t = first_triangles[c] as usize;
            values[cursors[t] as usize] = c as i64;
            cursors[t] += 1;
        }
        if side_at(second_sides, c as i64) == target_side {
            let t = second_triangles[c] as usize;
            values[cursors[t] as usize] = c as i64;
            cursors[t] += 1;
        }
    }
    (offsets, values)
}

fn event_explicit_point(
    first_triangle: &[i64; 3],
    second_triangle: &[i64; 3],
    first_feature: i64,
    second_feature: i64,
) -> i64 {
    let first = predicates::view::triangle_feature_vertex_local(first_feature);
    if first >= 0 {
        return first_triangle[first as usize];
    }
    let second = predicates::view::triangle_feature_vertex_local(second_feature);
    if second >= 0 { second_triangle[second as usize] } else { -1 }
}

fn ordinary_topology_contact(
    first_triangle: &[i64; 3],
    second_triangle: &[i64; 3],
    count: i64,
    feature0: i64,
    feature1: i64,
    feature2: i64,
    feature3: i64,
) -> bool {
    if count <= 0 {
        return false;
    }
    let common = |point: i64| point == second_triangle[0] || point == second_triangle[1] || point == second_triangle[2];
    let pick = |i: usize| if common(first_triangle[i]) { first_triangle[i] } else { -1 };
    let (c0, c1, c2) = (pick(0), pick(1), pick(2));
    let common_count = (c0 >= 0) as i64 + (c1 >= 0) as i64 + (c2 >= 0) as i64;
    let is_common = |point: i64| point == c0 || point == c1 || point == c2;
    let first = event_explicit_point(first_triangle, second_triangle, feature0, feature1);
    if count == 1 {
        first >= 0 && is_common(first)
    } else if count == 2 && common_count >= 2 {
        let second = event_explicit_point(first_triangle, second_triangle, feature2, feature3);
        first >= 0 && second >= 0 && first != second && is_common(first) && is_common(second)
    } else {
        false
    }
}

fn ordinary_shared_edge_only(
    x: &[f64],
    y: &[f64],
    z: &[f64],
    first_triangle: &[i64; 3],
    second_triangle: &[i64; 3],
) -> bool {
    let (b0, b1, b2) = (second_triangle[0], second_triangle[1], second_triangle[2]);
    let in_second = |point: i64| point == b0 || point == b1 || point == b2;
    let pick = |i: usize| if in_second(first_triangle[i]) { first_triangle[i] } else { -1 };
    let (c0, c1, c2) = (pick(0), pick(1), pick(2));
    let shared_count = (c0 >= 0) as i64 + (c1 >= 0) as i64 + (c2 >= 0) as i64;
    if shared_count != 2 {
        return false;
    }
    let first = if c0 >= 0 { c0 } else { c1 };
    let second = if c2 >= 0 { c2 } else { c1 };
    let first_other = if !(first_triangle[0] == first || first_triangle[0] == second) {
        first_triangle[0]
    } else if !(first_triangle[1] == first || first_triangle[1] == second) {
        first_triangle[1]
    } else {
        first_triangle[2]
    };
    let second_other = if !(b0 == first || b0 == second) {
        b0
    } else if !(b1 == first || b1 == second) {
        b1
    } else {
        b2
    };
    match predicates::orient3d_packed(x, y, z, first, second, first_other, second_other) {
        Sign::Positive | Sign::Negative => true,
        Sign::Zero => {
            let a = predicates::orient2d_packed(x, y, first, second, first_other);
            let (a_sign, b_sign) = if a != Sign::Zero {
                (a, predicates::orient2d_packed(x, y, first, second, second_other))
            } else {
                let a = predicates::orient2d_packed(y, z, first, second, first_other);
                if a != Sign::Zero {
                    (a, predicates::orient2d_packed(y, z, first, second, second_other))
                } else {
                    (
                        predicates::orient2d_packed(z, x, first, second, first_other),
                        predicates::orient2d_packed(z, x, first, second, second_other),
                    )
                }
            };
            matches!((a_sign, b_sign), (Sign::Positive, Sign::Negative) | (Sign::Negative, Sign::Positive))
        }
    }
}

fn opposite_duplicate(first_triangle: &[i64; 3], second_triangle: &[i64; 3]) -> bool {
    let [a, b, c] = *first_triangle;
    let [x, y, z] = *second_triangle;
    (a == x && b == z && c == y) || (a == y && b == x && c == z) || (a == z && b == y && c == x)
}

fn poll(cancel_value: Option<&cancel::T>, index: i64) -> Result<(), error::T> {
    cancel::poll(cancel_value, index, 4095)
        .map_err(|_| error::cancelled(OPERATION))
}

fn triple(points: &[i64], index: i64) -> [i64; 3] {
    let i = (index * 3) as usize;
    [points[i], points[i + 1], points[i + 2]]
}

pub fn build(params: &Params, left: &geometry::T, right: &geometry::T) -> Result<T, error::T> {
    let cancel_value = params.cancel.as_deref();
    if params.grain <= 0 {
        return Err(fail(Code::InvalidParameter, "grain must be positive"));
    }
    let grain = params.grain;
    let left_surface = surface_index::create(cancel_value, Some(grain), None, None, None, left)?;
    let right_surface = surface_index::create(cancel_value, Some(grain), None, None, None, right)?;
    let (cross_first, cross_second) =
        surface_index::View::overlapping_triangle_pairs(cancel_value, grain, 0., &left_surface, &right_surface)?;
    let (left_self_first, left_self_second) = if params.resolve_left_self_intersections {
        surface_index::View::overlapping_self_triangle_pairs(cancel_value, grain, 0., &left_surface)?
    } else {
        (Vec::new(), Vec::new())
    };
    let (right_self_first, right_self_second) = if params.resolve_right_self_intersections {
        surface_index::View::overlapping_self_triangle_pairs(cancel_value, grain, 0., &right_surface)?
    } else {
        (Vec::new(), Vec::new())
    };
    let cross_count = cross_first.len();
    let left_self_count = left_self_first.len();
    let right_self_count = right_self_first.len();
    let candidate_count = cross_count + left_self_count + right_self_count;
    let mut first_candidate_sides = vec![0u8; candidate_count];
    let mut second_candidate_sides = vec![0u8; candidate_count];
    for side in &mut second_candidate_sides[..cross_count] {
        *side = 1;
    }
    let (first_candidates, second_candidates) = if left_self_count == 0 && right_self_count == 0 {
        (cross_first, cross_second)
    } else {
        let mut first = Vec::with_capacity(candidate_count);
        let mut second = Vec::with_capacity(candidate_count);
        first.extend_from_slice(&cross_first);
        second.extend_from_slice(&cross_second);
        first.extend_from_slice(&left_self_first);
        second.extend_from_slice(&left_self_second);
        first.extend_from_slice(&right_self_first);
        second.extend_from_slice(&right_self_second);
        let right_start = cross_count + left_self_count;
        for i in right_start..candidate_count {
            first_candidate_sides[i] = 1;
            second_candidate_sides[i] = 1;
        }
        (first, second)
    };
    let (left_offset, x, y, z) = concatenate_positions(left, right)?;
    let source = implicit_point::source(x.clone(), y.clone(), z.clone())
        .map_err(|_| fail(Code::InvalidGeometry, "Boolean constraint source is invalid"))?;
    let mut source_points = Vec::with_capacity(x.len());
    for point in 0..x.len() {
        source_points.push(implicit_point::explicit(&source, point as i64).unwrap());
    }
    let left_topology = topology::View::view(geometry::topology(left))?;
    let right_topology = topology::View::view(geometry::topology(right))?;
    let left_triangle_points = all_triangle_points(&left_topology.vertex_points, &left_surface, 0);
    let right_triangle_points = all_triangle_points(&right_topology.vertex_points, &right_surface, left_offset);

    // Classification: each candidate writes its count and features in place through disjoint windows.
    let chunk = grain.max(1);
    let mut counts = vec![0i64; candidate_count];
    let mut feature0 = vec![0i64; candidate_count];
    let mut feature1 = vec![0i64; candidate_count];
    let mut feature2 = vec![0i64; candidate_count];
    let mut feature3 = vec![0i64; candidate_count];
    {
        let count_windows = support::Windows::even(&mut counts, chunk);
        let feature0_windows = support::Windows::even(&mut feature0, chunk);
        let feature1_windows = support::Windows::even(&mut feature1, chunk);
        let feature2_windows = support::Windows::even(&mut feature2, chunk);
        let feature3_windows = support::Windows::even(&mut feature3, chunk);
        let skip_opposite = params.skip_opposite_self_pairs;
        let (xs, ys, zs) = (&x[..], &y[..], &z[..]);
        let (fcs, scs) = (&first_candidate_sides, &second_candidate_sides);
        let (fc, sc) = (&first_candidates, &second_candidates);
        let (ltp, rtp) = (&left_triangle_points, &right_triangle_points);
        support::run_indexed_ranges(chunk, candidate_count as i64, &|range, first, last| {
            let mut scratch = [0i64; 4];
            let outputs = count_windows
                .take(range)
                .iter_mut()
                .zip(feature0_windows.take(range))
                .zip(feature1_windows.take(range))
                .zip(feature2_windows.take(range))
                .zip(feature3_windows.take(range));
            for (candidate, ((((out_count, out0), out1), out2), out3)) in (first..last).zip(outputs) {
                poll(cancel_value, candidate)?;
                let c = candidate as usize;
                let first_points = if fcs[c] == 0 { ltp } else { rtp };
                let second_points = if scs[c] == 0 { ltp } else { rtp };
                let left_triangle = triple(first_points, fc[c]);
                let right_triangle = triple(second_points, sc[c]);
                let self_pair = fcs[c] == scs[c];
                let mut count = if self_pair
                    && ((skip_opposite && opposite_duplicate(&left_triangle, &right_triangle))
                        || ordinary_shared_edge_only(xs, ys, zs, &left_triangle, &right_triangle))
                {
                    0
                } else {
                    predicates::view::triangle_triangle_features_into(
                        xs,
                        ys,
                        zs,
                        left_triangle[0],
                        left_triangle[1],
                        left_triangle[2],
                        right_triangle[0],
                        right_triangle[1],
                        right_triangle[2],
                        &mut scratch,
                    )
                };
                let (mut f0, mut f1, mut f2, mut f3) = (0, 0, 0, 0);
                if count > 0 {
                    f0 = scratch[0];
                    f1 = scratch[1];
                }
                if count > 1 {
                    f2 = scratch[2];
                    f3 = scratch[3];
                }
                if self_pair && ordinary_topology_contact(&left_triangle, &right_triangle, count, f0, f1, f2, f3) {
                    count = 0;
                }
                (*out_count, *out0, *out1, *out2, *out3) = (count, f0, f1, f2, f3);
            }
            Ok(())
        })?;
    }

    let mut event_offsets = vec![0usize; candidate_count + 1];
    let mut coplanar_count = 0usize;
    let mut degenerate_pairs = 0i64;
    for candidate in 0..candidate_count {
        let count = counts[candidate];
        event_offsets[candidate + 1] = event_offsets[candidate] + if count > 0 { count as usize } else { 0 };
        if count == -1 {
            coplanar_count += 1;
        } else if count == -2 {
            degenerate_pairs += 1;
        }
    }
    let event_count = event_offsets[candidate_count];
    let mut event_points: Vec<Option<implicit_point::T>> = vec![None; event_count];
    let mut first_error: Option<PointError> = None;
    // Exact construction stays on one stable stream (as in OCaml).
    for candidate in 0..candidate_count {
        poll(cancel_value, candidate as i64)?;
        let count = counts[candidate];
        if count > 0 {
            let first_points = if first_candidate_sides[candidate] == 0 { &left_triangle_points } else { &right_triangle_points };
            let second_points = if second_candidate_sides[candidate] == 0 { &left_triangle_points } else { &right_triangle_points };
            let left_triangle = triple(first_points, first_candidates[candidate]);
            let right_triangle = triple(second_points, second_candidates[candidate]);
            for event in 0..count as usize {
                let (lf, rf) = if event == 0 {
                    (feature0[candidate], feature1[candidate])
                } else {
                    (feature2[candidate], feature3[candidate])
                };
                match construct_event(&source, &source_points, &left_triangle, &right_triangle, lf, rf) {
                    Ok(point) => event_points[event_offsets[candidate] + event] = Some(point),
                    Err(failure) => {
                        if first_error.is_none() {
                            first_error = Some(failure);
                        }
                    }
                }
            }
        }
    }
    if first_error.is_some() {
        return Err(fail(
            Code::ExactConstructionFailed,
            "an exact triangle intersection point could not be constructed",
        ));
    }
    let mut order: Vec<usize> = (0..event_count).collect();
    // Total order (ties broken by index), so sort stability is irrelevant.
    order.sort_by(|&a, &b| compare_points(&event_points, a, b));
    let mut unique_of_event = vec![0i64; event_count];
    let mut points: Vec<implicit_point::T> = Vec::new();
    let mut previous: Option<usize> = None;
    for &event in &order {
        let distinct = match previous {
            None => true,
            Some(p) => !implicit_point::equal(event_points[event].as_ref().unwrap(), event_points[p].as_ref().unwrap()),
        };
        if distinct {
            previous = Some(event);
            points.push(event_points[event].as_ref().unwrap().clone());
        }
        unique_of_event[event] = points.len() as i64 - 1;
    }
    let constraint_total = counts.iter().filter(|&&c| c > 0).count();
    let mut first = Vec::with_capacity(constraint_total);
    let mut second = Vec::with_capacity(constraint_total);
    let mut constraint_first_sides = Vec::with_capacity(constraint_total);
    let mut constraint_second_sides = Vec::with_capacity(constraint_total);
    let mut constraint_first_triangles = Vec::with_capacity(constraint_total);
    let mut constraint_second_triangles = Vec::with_capacity(constraint_total);
    for candidate in 0..candidate_count {
        let count = counts[candidate];
        if count > 0 {
            let first_point = unique_of_event[event_offsets[candidate]];
            let second_point = if count == 1 { first_point } else { unique_of_event[event_offsets[candidate] + 1] };
            first.push(first_point);
            second.push(second_point);
            constraint_first_sides.push(first_candidate_sides[candidate]);
            constraint_second_sides.push(second_candidate_sides[candidate]);
            constraint_first_triangles.push(first_candidates[candidate]);
            constraint_second_triangles.push(second_candidates[candidate]);
        }
    }
    let (left_offsets, left_constraints) = make_face_csr(
        Side::Left,
        surface_index::triangle_count(&left_surface),
        &constraint_first_sides,
        &constraint_first_triangles,
        &constraint_second_sides,
        &constraint_second_triangles,
    );
    let (right_offsets, right_constraints) = make_face_csr(
        Side::Right,
        surface_index::triangle_count(&right_surface),
        &constraint_first_sides,
        &constraint_first_triangles,
        &constraint_second_sides,
        &constraint_second_triangles,
    );
    let (coplanar_first_sides, coplanar_first_triangles, coplanar_second_sides, coplanar_second_triangles) =
        compact_pairs(
            &counts,
            coplanar_count,
            &first_candidate_sides,
            &first_candidates,
            &second_candidate_sides,
            &second_candidates,
        );
    Ok(T {
        left_geometry: left.clone(),
        right_geometry: right.clone(),
        resolve_left_self_intersections: params.resolve_left_self_intersections,
        resolve_right_self_intersections: params.resolve_right_self_intersections,
        left_surface,
        right_surface,
        source,
        source_points,
        left_triangle_points,
        right_triangle_points,
        points,
        first,
        second,
        constraint_first_sides,
        constraint_first_triangles,
        constraint_second_sides,
        constraint_second_triangles,
        left_offsets,
        left_constraints,
        right_offsets,
        right_constraints,
        coplanar_first_sides,
        coplanar_first_triangles,
        coplanar_second_sides,
        coplanar_second_triangles,
        degenerate_pairs,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;

    fn geom(points: &[(f64, f64, f64)], triangles: &[i64]) -> geometry::T {
        let x: Vec<f64> = points.iter().map(|p| p.0).collect();
        let y: Vec<f64> = points.iter().map(|p| p.1).collect();
        let z: Vec<f64> = points.iter().map(|p| p.2).collect();
        let positions = packed::Float3::View::of_owned_exn(x, y, z).unwrap();
        let offsets: Vec<i64> = (0..=(triangles.len() as i64 / 3)).map(|p| p * 3).collect();
        let kinds = vec![PrimitiveKind::Polygon; offsets.len() - 1];
        let topology = topology::create_owned(points.len() as i64, triangles.to_vec(), offsets, kinds).unwrap();
        geometry::create(positions, topology, None, None, None).unwrap()
    }

    fn left_triangle() -> geometry::T {
        geom(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)], &[0, 1, 2])
    }
    fn crossing_triangle() -> geometry::T {
        geom(&[(0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)], &[0, 1, 2])
    }

    #[test]
    fn right_operand_constraints() {
        let left = geom(&[(10., 10., 0.), (11., 10., 0.), (10., 11., 0.)], &[0, 1, 2]);
        let right = geom(
            &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)],
            &[0, 1, 2, 3, 4, 5],
        );
        let mut p = params(1);
        p.resolve_right_self_intersections = true;
        let plan = build(&p, &left, &right).unwrap();
        assert!(
            constraint_count(&plan) == 1
                && constraint_first_side(&plan, 0) == Side::Right
                && constraint_second_side(&plan, 0) == Side::Right
        );
        assert!(
            right_constraint_range(&plan, 0) == (0, 1)
                && right_constraint_range(&plan, 1) == (1, 2)
                && left_constraint_range(&plan, 0) == (0, 0)
        );
    }

    #[test]
    fn same_operand_coplanar_pair() {
        let left = geom(
            &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (0.25, 0.25, 0.), (1., 0.25, 0.), (0.25, 1., 0.)],
            &[0, 1, 2, 3, 4, 5],
        );
        let right = geom(&[(10., 10., 0.), (11., 10., 0.), (10., 11., 0.)], &[0, 1, 2]);
        let mut p = params(1);
        p.resolve_left_self_intersections = true;
        let plan = build(&p, &left, &right).unwrap();
        assert!(
            coplanar_pair_count(&plan) == 1
                && coplanar_first_side(&plan, 0) == Side::Left
                && coplanar_second_side(&plan, 0) == Side::Left
        );
        let (a, b) = (coplanar_first_triangle(&plan, 0), coplanar_second_triangle(&plan, 0));
        assert!((a == 0 && b == 1) || (a == 1 && b == 0));
    }

    #[test]
    fn invalid_grain() {
        match build(&params(0), &left_triangle(), &crossing_triangle()) {
            Err(e) => assert_eq!(error::code(&e), "invalid_parameter"),
            Ok(_) => panic!("zero grain was accepted"),
        }
    }

    #[test]
    fn cancellation() {
        let token = cancel::create();
        cancel::cancel(&token);
        let mut p = params(1);
        p.cancel = Some(Arc::new(token));
        match build(&p, &left_triangle(), &crossing_triangle()) {
            Err(e) => assert_eq!(error::code(&e), "cancelled"),
            Ok(_) => panic!("cancelled constraint planning completed"),
        }
    }

    #[test]
    fn crossing_plan_is_found() {
        let plan = build(&params(1), &left_triangle(), &crossing_triangle()).unwrap();
        assert_eq!(constraint_count(&plan), 1);
        assert_eq!(point_count(&plan), 2);
    }
}
