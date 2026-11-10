//! exact planar splitting of one face's
//! non-coplanar Boolean constraints.
#![allow(non_snake_case)]
use rays_math::float::{min as fmin, max as fmax, compare as fcmp};

use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;

use rdk_core::{cancel, error};
use rdk_core::error::Code;
use rdk_geom::bounds2_index;
use rdk_geom::implicit_point::{self, Projection};
use rdk_geom::predicates::Sign;

use crate::boolean_constraints::{self as constraints_mod, Side};
use crate::boolean_coplanar as coplanar_mod;

pub const OPERATION: &str = "boolean_face_arrangement";

pub struct T {
    points: Vec<implicit_point::T>,
    point_handles: Vec<i64>,
    segment_first: Vec<i64>,
    segment_second: Vec<i64>,
}

fn fail(code: Code, message: &str) -> error::T {
    error::new(OPERATION, code, message.to_owned())
}

fn invalid(message: &str) -> error::T {
    fail(Code::InvalidConstraints, message)
}

pub fn point_count(value: &T) -> i64 {
    value.points.len() as i64
}
pub fn segment_count(value: &T) -> i64 {
    value.segment_first.len() as i64
}
pub fn segment_first(value: &T, segment: i64) -> i64 {
    value.segment_first[segment as usize]
}
pub fn segment_second(value: &T, segment: i64) -> i64 {
    value.segment_second[segment as usize]
}

pub mod view {
    use super::*;

    pub fn point(value: &T, point: i64) -> &implicit_point::T {
        &value.points[point as usize]
    }
    pub fn point_handle(value: &T, point: i64) -> i64 {
        value.point_handles[point as usize]
    }
}

fn face_constraint_range(constraints: &constraints_mod::T, side: Side, triangle: i64) -> (i64, i64) {
    match side {
        Side::Left => constraints_mod::left_constraint_range(constraints, triangle),
        Side::Right => constraints_mod::right_constraint_range(constraints, triangle),
    }
}

fn face_constraint(constraints: &constraints_mod::T, side: Side, slot: i64) -> i64 {
    match side {
        Side::Left => constraints_mod::left_constraint(constraints, slot),
        Side::Right => constraints_mod::right_constraint(constraints, slot),
    }
}

fn face_coplanar_range(coplanar: &coplanar_mod::T, side: Side, triangle: i64) -> (i64, i64) {
    match side {
        Side::Left => coplanar_mod::view::left_pair_range(coplanar, triangle),
        Side::Right => coplanar_mod::view::right_pair_range(coplanar, triangle),
    }
}

fn face_coplanar_pair(coplanar: &coplanar_mod::T, side: Side, slot: i64) -> i64 {
    match side {
        Side::Left => coplanar_mod::view::left_pair(coplanar, slot),
        Side::Right => coplanar_mod::view::right_pair(coplanar, slot),
    }
}

fn face_triangle_point(constraints: &constraints_mod::T, side: Side, triangle: i64, local: i64) -> i64 {
    constraints_mod::view::triangle_point(constraints, side, triangle, local)
}

fn opponent(constraints: &constraints_mod::T, side: Side, triangle: i64, constraint_index: i64) -> (Side, i64) {
    let first_side = constraints_mod::constraint_first_side(constraints, constraint_index);
    let first_triangle = constraints_mod::constraint_first_triangle(constraints, constraint_index);
    if first_side == side && first_triangle == triangle {
        (
            constraints_mod::constraint_second_side(constraints, constraint_index),
            constraints_mod::constraint_second_triangle(constraints, constraint_index),
        )
    } else {
        (first_side, first_triangle)
    }
}

fn sign_opposite(left: Sign, right: Sign) -> bool {
    matches!((left, right), (Sign::Negative, Sign::Positive) | (Sign::Positive, Sign::Negative))
}

fn projection(points: &[implicit_point::T]) -> i64 {
    match implicit_point::orient2d_xy(&points[0], &points[1], &points[2]) {
        Sign::Positive | Sign::Negative => 0,
        Sign::Zero => match implicit_point::orient2d_yz(&points[0], &points[1], &points[2]) {
            Sign::Positive | Sign::Negative => 1,
            Sign::Zero => 2,
        },
    }
}

type Orient = fn(&implicit_point::T, &implicit_point::T, &implicit_point::T) -> Sign;

fn orient_of(projection: i64) -> Orient {
    match projection {
        0 => implicit_point::orient2d_xy,
        1 => implicit_point::orient2d_yz,
        _ => implicit_point::orient2d_zx,
    }
}

fn reverse(sign: Sign) -> Sign {
    match sign {
        Sign::Negative => Sign::Positive,
        Sign::Positive => Sign::Negative,
        Sign::Zero => Sign::Zero,
    }
}

type CompareAxis = fn(&implicit_point::T, &implicit_point::T) -> i64;

fn compare_axis(axis: i64) -> CompareAxis {
    match axis {
        0 => implicit_point::compare_x,
        1 => implicit_point::compare_y,
        _ => implicit_point::compare_z,
    }
}

fn dominant_segment_axis(first: &implicit_point::T, second: &implicit_point::T) -> i64 {
    if implicit_point::compare_x(first, second) != 0 {
        0
    } else if implicit_point::compare_y(first, second) != 0 {
        1
    } else {
        2
    }
}

fn between(first: &implicit_point::T, point: &implicit_point::T, second: &implicit_point::T) -> bool {
    let compare = compare_axis(dominant_segment_axis(first, second));
    let order = compare(first, second);
    if order < 0 {
        compare(first, point) <= 0 && compare(point, second) <= 0
    } else {
        compare(second, point) <= 0 && compare(point, first) <= 0
    }
}

fn sort_range(values: &mut [i64], first: usize, last: usize, compare: &dyn Fn(i64, i64) -> i64) {
    values[first..last].sort_by(|&a, &b| compare(a, b).cmp(&0));
}

struct CandidatePlan {
    first: Vec<i64>,
    second: Vec<i64>,
    order: Vec<usize>,
}

enum CandidateResult {
    SmallScan,
    Candidates(CandidatePlan),
    Dense,
}

fn candidate_limit(item_count: usize) -> usize {
    let scaled = if item_count > 1_000_000 / 64 { 1_000_000 } else { item_count * 64 };
    4_096usize.max(1_000_000usize.min(scaled))
}

fn finish_candidates(first: Vec<i64>, second: Vec<i64>) -> CandidatePlan {
    let mut order: Vec<usize> = (0..first.len()).collect();
    // (first, second) pairs are unique, so the order is total.
    order.sort_by(|&l, &r| first[l].cmp(&first[r]).then(second[l].cmp(&second[r])));
    CandidatePlan { first, second, order }
}

struct Planes {
    u_min: Vec<f64>,
    u_max: Vec<f64>,
    v_min: Vec<f64>,
    v_max: Vec<f64>,
}

fn bounds_index(cancel_value: Option<&cancel::T>, planes: &Planes) -> Result<bounds2_index::T, error::T> {
    bounds2_index::create(cancel_value, &planes.u_min, &planes.v_min, &planes.u_max, &planes.v_max)
}

/// Items of `index` overlapping the closed box of `planes[item]` with a number above `after`, ascending.
fn overlapping_after(
    cancel_value: Option<&cancel::T>,
    index: &bounds2_index::T,
    after: i64,
    planes: &Planes,
    item: usize,
    found: &mut Vec<i64>,
) -> Result<(), error::T> {
    found.clear();
    bounds2_index::query(
        cancel_value,
        index,
        planes.u_min[item],
        planes.v_min[item],
        planes.u_max[item],
        planes.v_max[item],
        &mut |hit| {
            if hit > after {
                found.push(hit);
            }
            Ok(())
        },
    )?;
    found.sort_unstable();
    Ok(())
}

/// An exact implicit point ordered by (x, y, z), so a `BTreeMap` finds equal points.
struct PointKey(implicit_point::T);

fn compare_xyz(first: &implicit_point::T, second: &implicit_point::T) -> i64 {
    let comparison = implicit_point::compare_x(first, second);
    if comparison != 0 {
        return comparison;
    }
    let comparison = implicit_point::compare_y(first, second);
    if comparison != 0 {
        return comparison;
    }
    implicit_point::compare_z(first, second)
}

impl Ord for PointKey {
    fn cmp(&self, other: &Self) -> Ordering {
        compare_xyz(&self.0, &other.0).cmp(&0)
    }
}

impl PartialOrd for PointKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for PointKey {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for PointKey {}

fn poll(cancel_value: Option<&cancel::T>, index: i64, mask: i64) -> Result<(), error::T> {
    cancel::poll(cancel_value, index, mask).map_err(|_| error::cancelled(OPERATION))
}

fn projected_bounds(points: &[implicit_point::T], point_count: usize, projection: i64) -> Planes {
    let mut out = Planes {
        u_min: vec![0.; point_count],
        u_max: vec![0.; point_count],
        v_min: vec![0.; point_count],
        v_max: vec![0.; point_count],
    };
    for point in 0..point_count {
        let ((x_min, x_max), (y_min, y_max), (z_min, z_max)) = implicit_point::bounds(&points[point]);
        let (a, b, c, d) = match projection {
            0 => (x_min, x_max, y_min, y_max),
            1 => (y_min, y_max, z_min, z_max),
            _ => (z_min, z_max, x_min, x_max),
        };
        out.u_min[point] = a;
        out.u_max[point] = b;
        out.v_min[point] = c;
        out.v_max[point] = d;
    }
    out
}

fn segment_bounds(segment_first: &[i64], segment_second: &[i64], segment_count: usize, point: &Planes) -> Planes {
    let mut out = Planes {
        u_min: vec![0.; segment_count],
        u_max: vec![0.; segment_count],
        v_min: vec![0.; segment_count],
        v_max: vec![0.; segment_count],
    };
    for segment in 0..segment_count {
        let first = segment_first[segment] as usize;
        let second = segment_second[segment] as usize;
        out.u_min[segment] = fmin(point.u_min[first], point.u_min[second]);
        out.u_max[segment] = fmax(point.u_max[first], point.u_max[second]);
        out.v_min[segment] = fmin(point.v_min[first], point.v_min[second]);
        out.v_max[segment] = fmax(point.v_max[first], point.v_max[second]);
    }
    out
}

fn interval_score(lower: &[f64], upper: &[f64]) -> f64 {
    let count = lower.len();
    if count == 0 {
        return 0.;
    }
    let mut global_lower = lower[0];
    let mut global_upper = upper[0];
    let mut widths = 0.;
    for index in 0..count {
        global_lower = fmin(global_lower, lower[index]);
        global_upper = fmax(global_upper, upper[index]);
        widths += fmax(0., upper[index] - lower[index]);
    }
    let span = global_upper - global_lower;
    if span > 0. { widths / span } else { f64::MAX }
}

fn swap_axes(planes: Planes) -> Planes {
    Planes { u_min: planes.v_min, u_max: planes.v_max, v_min: planes.u_min, v_max: planes.u_max }
}

fn choose_sweep_axis(planes: Planes) -> Planes {
    if interval_score(&planes.v_min, &planes.v_max) < interval_score(&planes.u_min, &planes.u_max) {
        swap_axes(planes)
    } else {
        planes
    }
}

fn segment_pair_candidates(
    cancel_value: Option<&cancel::T>,
    segment_first: &[i64],
    segment_second: &[i64],
    segment_count: usize,
    points: &[implicit_point::T],
    point_count: usize,
    projection: i64,
) -> Result<CandidateResult, error::T> {
    if segment_count < 32 {
        return Ok(CandidateResult::SmallScan);
    }
    let point_planes = projected_bounds(points, point_count, projection);
    let seg = segment_bounds(segment_first, segment_second, segment_count, &point_planes);
    let seg = choose_sweep_axis(seg);
    let mut order: Vec<usize> = (0..segment_count).collect();
    order.sort_by(|&l, &r| fcmp(seg.u_min[l], seg.u_min[r]).then(l.cmp(&r)));
    let mut active = vec![0usize; segment_count];
    let mut active_count = 0usize;
    let mut first: Vec<i64> = Vec::new();
    let mut second: Vec<i64> = Vec::new();
    let mut dense = false;
    let limit = candidate_limit(segment_count);
    let mut event = 0usize;
    while event < segment_count && !dense {
        poll(cancel_value, event as i64, 1023)?;
        let current = order[event];
        let mut retained = 0usize;
        for slot in 0..active_count {
            let candidate = active[slot];
            if seg.u_max[candidate] >= seg.u_min[current] {
                active[retained] = candidate;
                retained += 1;
                if seg.v_max[candidate] >= seg.v_min[current] && seg.v_max[current] >= seg.v_min[candidate] {
                    if first.len() == limit {
                        dense = true;
                    } else {
                        first.push(candidate.min(current) as i64);
                        second.push(candidate.max(current) as i64);
                    }
                }
            }
        }
        active_count = retained;
        active[active_count] = current;
        active_count += 1;
        event += 1;
    }
    if dense { Ok(CandidateResult::Dense) } else { Ok(CandidateResult::Candidates(finish_candidates(first, second))) }
}

fn point_segment_candidates(
    cancel_value: Option<&cancel::T>,
    segment_first: &[i64],
    segment_second: &[i64],
    segment_count: usize,
    points: &[implicit_point::T],
    point_count: usize,
    projection: i64,
) -> Result<CandidateResult, error::T> {
    if segment_count < 32 || point_count < 32 {
        return Ok(CandidateResult::SmallScan);
    }
    let point_planes = projected_bounds(points, point_count, projection);
    let seg_planes = segment_bounds(segment_first, segment_second, segment_count, &point_planes);
    let swap = interval_score(&seg_planes.v_min, &seg_planes.v_max) < interval_score(&seg_planes.u_min, &seg_planes.u_max);
    let (point, seg) = if swap { (swap_axes(point_planes), swap_axes(seg_planes)) } else { (point_planes, seg_planes) };
    let event_count = segment_count + point_count;
    let mut events: Vec<usize> = (0..event_count).collect();
    let event_lower = |event: usize| if event < segment_count { seg.u_min[event] } else { point.u_min[event - segment_count] };
    events.sort_by(|&l, &r| fcmp(event_lower(l), event_lower(r)).then(l.cmp(&r)));
    let mut active_segments = vec![0usize; segment_count];
    let mut active_segment_count = 0usize;
    let mut active_points = vec![0usize; point_count];
    let mut active_point_count = 0usize;
    let mut first: Vec<i64> = Vec::new();
    let mut second: Vec<i64> = Vec::new();
    let mut dense = false;
    let limit = candidate_limit(segment_count);
    let mut event_slot = 0usize;
    while event_slot < event_count && !dense {
        poll(cancel_value, event_slot as i64, 1023)?;
        let event = events[event_slot];
        if event < segment_count {
            let segment = event;
            let mut retained = 0usize;
            for slot in 0..active_point_count {
                let p = active_points[slot];
                if point.u_max[p] >= seg.u_min[segment] {
                    active_points[retained] = p;
                    retained += 1;
                    if point.v_max[p] >= seg.v_min[segment] && seg.v_max[segment] >= point.v_min[p] {
                        if first.len() == limit {
                            dense = true;
                        } else {
                            first.push(segment as i64);
                            second.push(p as i64);
                        }
                    }
                }
            }
            active_point_count = retained;
            active_segments[active_segment_count] = segment;
            active_segment_count += 1;
        } else {
            let p = event - segment_count;
            let mut retained = 0usize;
            for slot in 0..active_segment_count {
                let segment = active_segments[slot];
                if seg.u_max[segment] >= point.u_min[p] {
                    active_segments[retained] = segment;
                    retained += 1;
                    if seg.v_max[segment] >= point.v_min[p] && point.v_max[p] >= seg.v_min[segment] {
                        if first.len() == limit {
                            dense = true;
                        } else {
                            first.push(segment as i64);
                            second.push(p as i64);
                        }
                    }
                }
            }
            active_segment_count = retained;
            active_points[active_point_count] = p;
            active_point_count += 1;
        }
        event_slot += 1;
    }
    if dense { Ok(CandidateResult::Dense) } else { Ok(CandidateResult::Candidates(finish_candidates(first, second))) }
}

fn canonicalize_points(points: &[implicit_point::T], count: usize) -> (Vec<implicit_point::T>, Vec<usize>) {
    if count == 0 {
        return (Vec::new(), Vec::new());
    }
    let mut order: Vec<usize> = (0..count).collect();
    order.sort_by(|&l, &r| compare_xyz(&points[l], &points[r]).cmp(&0).then(l.cmp(&r)));
    let mut representative = vec![0usize; count];
    let mut first = 0usize;
    while first < count {
        let mut last = first + 1;
        let mut earliest = order[first];
        while last < count && implicit_point::equal(&points[order[first]], &points[order[last]]) {
            earliest = earliest.min(order[last]);
            last += 1;
        }
        for slot in first..last {
            representative[order[slot]] = earliest;
        }
        first = last;
    }
    let mut old_to_new = vec![0usize; count];
    let mut unique_count = 0usize;
    for point in 0..count {
        if representative[point] == point {
            old_to_new[point] = unique_count;
            unique_count += 1;
        }
    }
    for point in 0..count {
        old_to_new[point] = old_to_new[representative[point]];
    }
    let mut output = vec![points[0].clone(); unique_count];
    for point in 0..count {
        if representative[point] == point {
            output[old_to_new[point]] = points[point].clone();
        }
    }
    (output, old_to_new)
}

struct State {
    constraints: Arc<constraints_mod::T>,
    side: Side,
    triangle: i64,
    source: implicit_point::Source,
    projection_axis: i64,
    orient_points: Orient,
    points: Vec<implicit_point::T>,
    point_handles: Vec<i64>,
    segment_first: Vec<i64>,
    segment_second: Vec<i64>,
    segment_constraint: Vec<i64>,
    segment_coplanar: Vec<bool>,
    segment_support_first: Vec<i64>,
    segment_support_second: Vec<i64>,
    orientation_cache: HashMap<(i64, i64, i64), Sign>,
    point_index: BTreeMap<PointKey, i64>,
    split_segments: Vec<i64>,
    split_points: Vec<i64>,
    split_next: Vec<i64>,
    split_set: HashSet<(i64, i64)>,
    split_head: Vec<i64>,
    segment_split_count: Vec<i64>,
}

impl State {
    fn append_point(&mut self, point: implicit_point::T, handle: i64) -> i64 {
        self.points.push(point);
        self.point_handles.push(handle);
        self.points.len() as i64 - 1
    }

    fn orient_ids(&mut self, a: i64, b: i64, c: i64) -> Sign {
        if a == b || b == c || c == a {
            return Sign::Zero;
        }
        let inversions = (a > b) as i64 + (a > c) as i64 + (b > c) as i64;
        let (a, b, c) = if a <= b {
            if b <= c {
                (a, b, c)
            } else if a <= c {
                (a, c, b)
            } else {
                (c, a, b)
            }
        } else if a <= c {
            (b, a, c)
        } else if b <= c {
            (b, c, a)
        } else {
            (c, b, a)
        };
        let sign = match self.orientation_cache.get(&(a, b, c)) {
            Some(&sign) => sign,
            None => {
                let sign = (self.orient_points)(&self.points[a as usize], &self.points[b as usize], &self.points[c as usize]);
                self.orientation_cache.insert((a, b, c), sign);
                sign
            }
        };
        if inversions & 1 == 0 { sign } else { reverse(sign) }
    }

    fn intern_point(&mut self, point: implicit_point::T) -> i64 {
        let key = PointKey(point);
        if let Some(&existing) = self.point_index.get(&key) {
            return existing;
        }
        let point_id = self.append_point(key.0.clone(), -1);
        self.point_index.insert(key, point_id);
        point_id
    }

    fn add_split(&mut self, segment: i64, point: i64) {
        if self.split_set.insert((segment, point)) {
            self.split_segments.push(segment);
            self.split_points.push(point);
            self.split_next.push(self.split_head[segment as usize]);
            self.split_head[segment as usize] = self.split_segments.len() as i64 - 1;
            self.segment_split_count[segment as usize] += 1;
        }
    }

    fn common(&self, source_segment: i64, target_segment: i64) -> i64 {
        if self.segment_split_count[source_segment as usize] > 16 {
            return -1;
        }
        let mut split = self.split_head[source_segment as usize];
        while split >= 0 {
            let point_id = self.split_points[split as usize];
            if self.split_set.contains(&(target_segment, point_id)) {
                return point_id;
            }
            split = self.split_next[split as usize];
        }
        -1
    }

    fn search(&mut self, source_segment: i64, target_segment: i64) -> i64 {
        if self.segment_split_count[source_segment as usize] > 16 {
            return -1;
        }
        let target_first_id = self.segment_first[target_segment as usize];
        let target_second_id = self.segment_second[target_segment as usize];
        let mut split = self.split_head[source_segment as usize];
        while split >= 0 {
            let point_id = self.split_points[split as usize];
            if point_id != target_first_id && point_id != target_second_id {
                if self.orient_ids(target_first_id, target_second_id, point_id) == Sign::Zero
                    && between(
                        &self.points[target_first_id as usize],
                        &self.points[point_id as usize],
                        &self.points[target_second_id as usize],
                    )
                {
                    return point_id;
                }
            }
            split = self.split_next[split as usize];
        }
        -1
    }

    fn existing_crossing(&mut self, left: i64, right: i64) -> i64 {
        let found = self.common(left, right);
        if found >= 0 {
            return found;
        }
        let found = self.search(left, right);
        if found >= 0 { found } else { self.search(right, left) }
    }

    fn opponent_plane(&self, constraint_index: i64) -> [i64; 3] {
        let (opponent_side, opponent_triangle) = opponent(&self.constraints, self.side, self.triangle, constraint_index);
        [
            face_triangle_point(&self.constraints, opponent_side, opponent_triangle, 0),
            face_triangle_point(&self.constraints, opponent_side, opponent_triangle, 1),
            face_triangle_point(&self.constraints, opponent_side, opponent_triangle, 2),
        ]
    }

    fn line_plane(&self, coplanar_segment: usize, noncoplanar_segment: usize) -> Result<implicit_point::T, implicit_point::Error> {
        let plane = self.opponent_plane(self.segment_constraint[noncoplanar_segment]);
        implicit_point::line_plane(
            &self.source,
            self.segment_support_first[coplanar_segment],
            self.segment_support_second[coplanar_segment],
            plane[0],
            plane[1],
            plane[2],
        )
    }

    fn process_segment_pair(&mut self, left: i64, right: i64) -> Result<(), error::T> {
        let (l, r) = (left as usize, right as usize);
        let a_id = self.segment_first[l];
        let b_id = self.segment_second[l];
        let c_id = self.segment_first[r];
        let d_id = self.segment_second[r];
        if a_id == c_id || a_id == d_id || b_id == c_id || b_id == d_id {
            return Ok(());
        }
        let o1 = self.orient_ids(a_id, b_id, c_id);
        let o2 = self.orient_ids(a_id, b_id, d_id);
        let o3 = self.orient_ids(c_id, d_id, a_id);
        let o4 = self.orient_ids(c_id, d_id, b_id);
        if sign_opposite(o1, o2) && sign_opposite(o3, o4) {
            let existing = self.existing_crossing(left, right);
            if existing >= 0 {
                self.add_split(left, existing);
                self.add_split(right, existing);
            } else {
                let coplanar_left = self.segment_coplanar[l];
                let coplanar_right = self.segment_coplanar[r];
                let projected = match self.projection_axis {
                    0 => Projection::XY,
                    1 => Projection::YZ,
                    _ => Projection::ZX,
                };
                let point = match (coplanar_left, coplanar_right) {
                    (false, false) => {
                        let left_plane = self.opponent_plane(self.segment_constraint[l]);
                        let right_plane = self.opponent_plane(self.segment_constraint[r]);
                        let face = |local| face_triangle_point(&self.constraints, self.side, self.triangle, local);
                        implicit_point::triple_plane(
                            &self.source,
                            face(0),
                            face(1),
                            face(2),
                            left_plane[0],
                            left_plane[1],
                            left_plane[2],
                            right_plane[0],
                            right_plane[1],
                            right_plane[2],
                        )
                    }
                    (true, true) => implicit_point::line_line(
                        &self.source,
                        projected,
                        self.segment_support_first[l],
                        self.segment_support_second[l],
                        self.segment_support_first[r],
                        self.segment_support_second[r],
                    ),
                    (true, false) => self.line_plane(l, r),
                    (false, true) => self.line_plane(r, l),
                };
                match point {
                    Err(_) => return Err(invalid("exact construction failed at proper constraint crossing")),
                    Ok(point) => {
                        let point = self.intern_point(point);
                        self.add_split(left, point);
                        self.add_split(right, point);
                    }
                }
            }
        } else {
            self.add_if_on(c_id, a_id, b_id, left);
            self.add_if_on(d_id, a_id, b_id, left);
            self.add_if_on(a_id, c_id, d_id, right);
            self.add_if_on(b_id, c_id, d_id, right);
        }
        Ok(())
    }

    fn add_if_on(&mut self, point_id: i64, first_id: i64, second_id: i64, segment: i64) {
        if self.orient_ids(first_id, second_id, point_id) == Sign::Zero
            && between(
                &self.points[first_id as usize],
                &self.points[point_id as usize],
                &self.points[second_id as usize],
            )
        {
            self.add_split(segment, point_id);
        }
    }

    fn process_point_segment(&mut self, segment: i64, point_id: i64) {
        let first_id = self.segment_first[segment as usize];
        let second_id = self.segment_second[segment as usize];
        if point_id != first_id && point_id != second_id {
            if self.orient_ids(first_id, second_id, point_id) == Sign::Zero
                && between(
                    &self.points[first_id as usize],
                    &self.points[point_id as usize],
                    &self.points[second_id as usize],
                )
            {
                self.add_split(segment, point_id);
            }
        }
    }
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    coplanar: Option<&coplanar_mod::T>,
    constraints: &Arc<constraints_mod::T>,
    side: Side,
    triangle: i64,
) -> Result<T, error::T> {
    poll(cancel_value, 0, 0)?;
    if let Some(value) = coplanar {
        if !Arc::ptr_eq(coplanar_mod::view::constraints(value), constraints) {
            return Err(invalid("coplanar plan belongs to a different constraint plan"));
        }
    }
    let constraints_ref: &constraints_mod::T = constraints;
    let (first_slot, last_slot) = face_constraint_range(constraints_ref, side, triangle);
    let face_constraint_count = last_slot - first_slot;
    let (first_pair, last_pair) = match coplanar {
        None => (0, 0),
        Some(coplanar) => face_coplanar_range(coplanar, side, triangle),
    };
    let empty = || T { points: Vec::new(), point_handles: Vec::new(), segment_first: Vec::new(), segment_second: Vec::new() };
    if face_constraint_count == 0 && first_pair == last_pair {
        return Ok(empty());
    }
    let mut raw_segment_capacity = 0usize;
    let mut point_capacity = 0usize;
    for slot in first_slot..last_slot {
        let constraint_index = face_constraint(constraints_ref, side, slot);
        let first = constraints_mod::constraint_first(constraints_ref, constraint_index);
        let second = constraints_mod::constraint_second(constraints_ref, constraint_index);
        point_capacity += 1;
        if second != first {
            point_capacity += 1;
            raw_segment_capacity += 1;
        }
    }
    if let Some(coplanar) = coplanar {
        for slot in first_pair..last_pair {
            let pair = face_coplanar_pair(coplanar, side, slot);
            point_capacity += coplanar_mod::point_count(coplanar, pair) as usize;
            raw_segment_capacity += coplanar_mod::boundary_count(coplanar, pair) as usize;
        }
    }
    let source = constraints_mod::view::source(constraints_ref).clone();
    let mut points: Vec<implicit_point::T> = Vec::with_capacity(point_capacity.max(4));
    let mut point_handles: Vec<i64> = Vec::with_capacity(point_capacity.max(4));
    let mut segment_first: Vec<i64> = Vec::with_capacity(raw_segment_capacity);
    let mut segment_second: Vec<i64> = Vec::with_capacity(raw_segment_capacity);
    let mut segment_constraint: Vec<i64> = Vec::with_capacity(raw_segment_capacity);
    let mut segment_coplanar: Vec<bool> = Vec::with_capacity(raw_segment_capacity);
    let mut segment_support_first: Vec<i64> = Vec::with_capacity(raw_segment_capacity);
    let mut segment_support_second: Vec<i64> = Vec::with_capacity(raw_segment_capacity);
    for slot in first_slot..last_slot {
        let constraint_index = face_constraint(constraints_ref, side, slot);
        let first_handle = constraints_mod::constraint_first(constraints_ref, constraint_index);
        let second_handle = constraints_mod::constraint_second(constraints_ref, constraint_index);
        points.push(constraints_mod::view::point(constraints_ref, first_handle).clone());
        point_handles.push(first_handle);
        let first = points.len() as i64 - 1;
        if first_handle != second_handle {
            points.push(constraints_mod::view::point(constraints_ref, second_handle).clone());
            point_handles.push(second_handle);
            let second = points.len() as i64 - 1;
            segment_first.push(first);
            segment_second.push(second);
            segment_constraint.push(constraint_index);
            segment_coplanar.push(false);
            segment_support_first.push(-1);
            segment_support_second.push(-1);
        }
    }
    if let Some(coplanar) = coplanar {
        for slot in first_pair..last_pair {
            let pair = face_coplanar_pair(coplanar, side, slot);
            let count = coplanar_mod::point_count(coplanar, pair);
            let mut point_map: Vec<i64> = Vec::with_capacity(count as usize);
            for point in 0..count {
                points.push(coplanar_mod::view::point(coplanar, pair, point).clone());
                point_handles.push(-1);
                point_map.push(points.len() as i64 - 1);
            }
            for boundary in 0..coplanar_mod::boundary_count(coplanar, pair) {
                segment_first.push(point_map[coplanar_mod::boundary_first(coplanar, pair, boundary) as usize]);
                segment_second.push(point_map[coplanar_mod::boundary_second(coplanar, pair, boundary) as usize]);
                segment_constraint.push(-1);
                segment_coplanar.push(true);
                segment_support_first.push(coplanar_mod::view::boundary_support_first(coplanar, pair, boundary));
                segment_support_second.push(coplanar_mod::view::boundary_support_second(coplanar, pair, boundary));
            }
        }
    }
    let raw_segment_count = segment_first.len();
    let raw_point_count = points.len();
    let (canonical, point_map) = canonicalize_points(&points, raw_point_count);
    let mut canonical_handles = vec![-1i64; canonical.len()];
    for point in 0..raw_point_count {
        let handle = point_handles[point];
        if handle >= 0 {
            let target = point_map[point];
            let present = canonical_handles[target];
            if present < 0 || handle < present {
                canonical_handles[target] = handle;
            }
        }
    }
    let points = canonical;
    let point_handles = canonical_handles;
    for segment in 0..raw_segment_count {
        segment_first[segment] = point_map[segment_first[segment] as usize] as i64;
        segment_second[segment] = point_map[segment_second[segment] as usize] as i64;
    }
    if points.is_empty() {
        return Ok(empty());
    }
    let face_points: Vec<implicit_point::T> = (0..3)
        .map(|local| implicit_point::explicit(&source, face_triangle_point(constraints_ref, side, triangle, local)).unwrap())
        .collect();
    let projection_axis = projection(&face_points);
    let point_index: BTreeMap<PointKey, i64> =
        points.iter().enumerate().map(|(id, point)| (PointKey(point.clone()), id as i64)).collect();
    let split_capacity = (raw_segment_count * 2).max(4);
    let mut state = State {
        constraints: constraints.clone(),
        side,
        triangle,
        source,
        projection_axis,
        orient_points: orient_of(projection_axis),
        points,
        point_handles,
        segment_first,
        segment_second,
        segment_constraint,
        segment_coplanar,
        segment_support_first,
        segment_support_second,
        orientation_cache: HashMap::new(),
        point_index,
        split_segments: Vec::with_capacity(split_capacity),
        split_points: Vec::with_capacity(split_capacity),
        split_next: Vec::with_capacity(split_capacity),
        split_set: HashSet::new(),
        split_head: vec![-1; raw_segment_count],
        segment_split_count: vec![0; raw_segment_count],
    };
    for segment in 0..raw_segment_count {
        let (first, second) = (state.segment_first[segment], state.segment_second[segment]);
        state.add_split(segment as i64, first);
        state.add_split(segment as i64, second);
    }
    // Segment/segment pairs.
    let candidates = segment_pair_candidates(
        cancel_value,
        &state.segment_first,
        &state.segment_second,
        raw_segment_count,
        &state.points,
        state.points.len(),
        projection_axis,
    )?;
    match candidates {
        CandidateResult::SmallScan => {
            for left in 0..raw_segment_count as i64 {
                poll(cancel_value, left, 255)?;
                for right in (left + 1)..raw_segment_count as i64 {
                    state.process_segment_pair(left, right)?;
                }
            }
        }
        CandidateResult::Dense => {
            let point_planes = projected_bounds(&state.points, state.points.len(), projection_axis);
            let planes = segment_bounds(&state.segment_first, &state.segment_second, raw_segment_count, &point_planes);
            let planes = choose_sweep_axis(planes);
            let index = bounds_index(cancel_value, &planes)?;
            let mut found = Vec::new();
            for left in 0..raw_segment_count {
                poll(cancel_value, left as i64, 255)?;
                overlapping_after(cancel_value, &index, left as i64, &planes, left, &mut found)?;
                for &right in &found {
                    state.process_segment_pair(left as i64, right)?;
                }
            }
        }
        CandidateResult::Candidates(plan) => {
            for (slot, &candidate) in plan.order.iter().enumerate() {
                poll(cancel_value, slot as i64, 1023)?;
                state.process_segment_pair(plan.first[candidate], plan.second[candidate])?;
            }
        }
    }
    // A point-only contact or a multi-way TPI may lie in the interior of a segment without
    // participating in that segment's pair which created it.
    let candidates = point_segment_candidates(
        cancel_value,
        &state.segment_first,
        &state.segment_second,
        raw_segment_count,
        &state.points,
        state.points.len(),
        projection_axis,
    )?;
    match candidates {
        CandidateResult::SmallScan => {
            for segment in 0..raw_segment_count as i64 {
                poll(cancel_value, segment, 255)?;
                for point_id in 0..state.points.len() as i64 {
                    state.process_point_segment(segment, point_id);
                }
            }
        }
        CandidateResult::Dense => {
            let point_planes = projected_bounds(&state.points, state.points.len(), projection_axis);
            let seg_planes = segment_bounds(&state.segment_first, &state.segment_second, raw_segment_count, &point_planes);
            let swap = interval_score(&seg_planes.v_min, &seg_planes.v_max)
                < interval_score(&seg_planes.u_min, &seg_planes.u_max);
            let point_planes = if swap { swap_axes(point_planes) } else { point_planes };
            let seg = if swap { swap_axes(seg_planes) } else { seg_planes };
            let index = bounds_index(cancel_value, &point_planes)?;
            let mut found = Vec::new();
            for segment in 0..raw_segment_count {
                poll(cancel_value, segment as i64, 255)?;
                overlapping_after(cancel_value, &index, -1, &seg, segment, &mut found)?;
                for &point_id in &found {
                    state.process_point_segment(segment as i64, point_id);
                }
            }
        }
        CandidateResult::Candidates(plan) => {
            for (slot, &candidate) in plan.order.iter().enumerate() {
                poll(cancel_value, slot as i64, 1023)?;
                state.process_point_segment(plan.first[candidate], plan.second[candidate]);
            }
        }
    }
    // Materialize the split points of every segment, in axis order.
    let split_count = state.split_segments.len();
    let mut counts = vec![0usize; raw_segment_count];
    for split in 0..split_count {
        counts[state.split_segments[split] as usize] += 1;
    }
    let mut offsets = vec![0usize; raw_segment_count + 1];
    for segment in 0..raw_segment_count {
        offsets[segment + 1] = offsets[segment] + counts[segment];
    }
    let mut values = vec![0i64; split_count];
    let mut cursors = offsets.clone();
    for split in 0..split_count {
        let segment = state.split_segments[split] as usize;
        values[cursors[segment]] = state.split_points[split];
        cursors[segment] += 1;
    }
    let mut output_first: Vec<i64> = Vec::new();
    let mut output_second: Vec<i64> = Vec::new();
    for segment in 0..raw_segment_count {
        let (first, last) = (offsets[segment], offsets[segment + 1]);
        let axis = dominant_segment_axis(
            &state.points[state.segment_first[segment] as usize],
            &state.points[state.segment_second[segment] as usize],
        );
        let compare = compare_axis(axis);
        let pts = &state.points;
        sort_range(&mut values, first, last, &|l, r| compare(&pts[l as usize], &pts[r as usize]));
    }
    for segment in 0..raw_segment_count {
        let (first, last) = (offsets[segment], offsets[segment + 1]);
        let mut previous = values[first];
        for split in (first + 1)..last {
            let point = values[split];
            if point != previous {
                output_first.push(previous);
                output_second.push(point);
                previous = point;
            }
        }
    }
    let mut order: Vec<usize> = (0..output_first.len()).collect();
    let key = |i: usize| (output_first[i].min(output_second[i]), output_first[i].max(output_second[i]));
    order.sort_by(|&l, &r| key(l).cmp(&key(r)));
    let mut unique_first: Vec<i64> = Vec::new();
    let mut unique_second: Vec<i64> = Vec::new();
    let (mut previous_a, mut previous_b) = (-1i64, -1i64);
    for &segment in &order {
        let (a, b) = key(segment);
        if a != previous_a || b != previous_b {
            unique_first.push(a);
            unique_second.push(b);
            previous_a = a;
            previous_b = b;
        }
    }
    Ok(T {
        points: state.points,
        point_handles: state.point_handles,
        segment_first: unique_first,
        segment_second: unique_second,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;
    use rdk_core::{geometry, packed, topology};

    #[test]
    fn bounds_index_overlaps_match_a_brute_force_scan_in_ascending_order() {
        let mut state = rays_math::rand::seed(7);
        let mut next = move || {
            let (value, after) = rays_math::rand::float(state); state = after;
            value * 100.0
        };
        let mut planes = Planes { u_min: vec![], u_max: vec![], v_min: vec![], v_max: vec![] };
        for _ in 0..300 {
            let (u, v, w, h) = (next(), next(), next() / 5.0, next() / 5.0);
            planes.u_min.push(u);
            planes.u_max.push(u + w);
            planes.v_min.push(v);
            planes.v_max.push(v + h);
        }
        let index = bounds_index(None, &planes).unwrap();
        let overlap = |a: usize, b: usize| {
            planes.u_max[a] >= planes.u_min[b] && planes.u_max[b] >= planes.u_min[a]
                && planes.v_max[a] >= planes.v_min[b] && planes.v_max[b] >= planes.v_min[a]
        };
        let mut found = Vec::new();
        for item in 0..300 {
            for after in [-1, item as i64, 150] {
                overlapping_after(None, &index, after, &planes, item, &mut found).unwrap();
                let expected: Vec<i64> = (0..300).filter(|&other| other as i64 > after && overlap(item, other)).map(|other| other as i64).collect();
                assert_eq!(found, expected);
            }
        }
    }

    type P = (f64, f64, f64);

    fn geom(points: &[P], triangles: &[i64]) -> geometry::T {
        let x: Vec<f64> = points.iter().map(|p| p.0).collect();
        let y: Vec<f64> = points.iter().map(|p| p.1).collect();
        let z: Vec<f64> = points.iter().map(|p| p.2).collect();
        let positions = packed::float3::view::of_owned_exn(x, y, z).unwrap();
        let offsets: Vec<i64> = (0..=(triangles.len() as i64 / 3)).map(|p| p * 3).collect();
        let kinds = vec![PrimitiveKind::Polygon; offsets.len() - 1];
        let topology = topology::create_owned(points.len() as i64, triangles.to_vec(), offsets, kinds).unwrap();
        geometry::create(positions, topology, None, None, None).unwrap()
    }

    fn constraints_of(grain: i64, left: &geometry::T, right: &geometry::T) -> Arc<constraints_mod::T> {
        Arc::new(constraints_mod::build(&constraints_mod::params(grain), left, right).unwrap())
    }

    fn arrange(constraints: &Arc<constraints_mod::T>, coplanar: Option<&coplanar_mod::T>, side: Side) -> T {
        build(None, coplanar, constraints, side, 0).unwrap()
    }

    fn approx(value: &T, point: i64) -> P {
        implicit_point::approximate(view::point(value, point))
    }

    fn close(l: f64, r: f64) -> bool {
        (l - r).abs() <= 1e-11
    }

    fn sorted_points(value: &T) -> Vec<P> {
        let mut points: Vec<P> = (0..point_count(value)).map(|p| approx(value, p)).collect();
        points.sort_by(|a, b| a.partial_cmp(b).unwrap());
        points
    }

    fn triangle(points: &[P]) -> geometry::T {
        geom(points, &[0, 1, 2])
    }

    fn crossing_plan() -> Arc<constraints_mod::T> {
        let left = triangle(&[(-3., -3., 0.), (3., -3., 0.), (0., 3., 0.)]);
        let right = geom(
            &[(0., -2., -1.), (0., 2., -1.), (0., 0., 1.), (-2., 0., -1.), (2., 0., -1.), (0., 0., 1.)],
            &[0, 1, 2, 3, 4, 5],
        );
        constraints_of(1, &left, &right)
    }

    fn incident(value: &T, point: i64) -> i64 {
        (0..segment_count(value)).filter(|&s| segment_first(value, s) == point || segment_second(value, s) == point).count() as i64
    }

    fn find_point(value: &T, x: f64, y: f64, z: f64) -> i64 {
        let mut found = -1;
        for p in 0..point_count(value) {
            let (px, py, pz) = approx(value, p);
            if close(px, x) && close(py, y) && close(pz, z) {
                found = p;
            }
        }
        found
    }

    #[test]
    fn proper_crossing() {
        let arrangement = arrange(&crossing_plan(), None, Side::Left);
        assert_eq!(point_count(&arrangement), 5, "proper crossing did not add one exact TPI");
        assert_eq!(segment_count(&arrangement), 4);
        let origin = find_point(&arrangement, 0., 0., 0.);
        assert!(origin >= 0, "TPI origin is missing");
        for s in 0..segment_count(&arrangement) {
            assert!(segment_first(&arrangement, s) != segment_second(&arrangement, s));
        }
        assert_eq!(incident(&arrangement, origin), 4);
        let expected = [(-1., 0., 0.), (0., -1., 0.), (0., 0., 0.), (0., 1., 0.), (1., 0., 0.)];
        for (index, (x, y, z)) in sorted_points(&arrangement).iter().enumerate() {
            let (ex, ey, ez) = expected[index];
            assert!(close(*x, ex) && close(*y, ey) && close(*z, ez), "arrangement point {index} differs");
        }
    }

    #[test]
    fn single_segment() {
        let left = triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]);
        let right = triangle(&[(0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)]);
        let arrangement = arrange(&constraints_of(1, &left, &right), None, Side::Left);
        assert!(point_count(&arrangement) == 2 && segment_count(&arrangement) == 1);
    }

    #[test]
    fn multiway_crossing() {
        let left = triangle(&[(-3., -3., 0.), (3., -3., 0.), (0., 3., 0.)]);
        let right = geom(
            &[
                (0., -2., -1.),
                (0., 2., -1.),
                (0., 0., 1.),
                (-2., 0., -1.),
                (2., 0., -1.),
                (0., 0., 1.),
                (-1.6, -1.6, -1.),
                (1.6, 1.6, -1.),
                (0., 0., 1.),
            ],
            &[0, 1, 2, 3, 4, 5, 6, 7, 8],
        );
        let arrangement = arrange(&constraints_of(1, &left, &right), None, Side::Left);
        assert_eq!(point_count(&arrangement), 7, "three-way crossing did not deduplicate the shared TPI");
        assert_eq!(segment_count(&arrangement), 6);
        let origin = find_point(&arrangement, 0., 0., 0.);
        assert_eq!(incident(&arrangement, origin), 6);
    }

    #[test]
    fn coplanar_overlay() {
        let left = triangle(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)]);
        let right = triangle(&[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.)]);
        let constraints = constraints_of(1, &left, &right);
        let coplanar = coplanar_mod::build(None, 1, &constraints).unwrap();
        let arrangement = arrange(&constraints, Some(&coplanar), Side::Left);
        assert_eq!(point_count(&arrangement), 6);
        assert_eq!(segment_count(&arrangement), 6);
        let right_arrangement = arrange(&constraints, Some(&coplanar), Side::Right);
        assert!(
            sorted_points(&arrangement) == sorted_points(&right_arrangement),
            "two sides of a coplanar pair received different exact boundaries"
        );
    }

    #[test]
    fn mixed_coplanar_noncoplanar_crossing() {
        let left = triangle(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)]);
        let right = geom(
            &[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.), (2., -1., -1.), (2., 5., -1.), (2., 2., 1.)],
            &[0, 1, 2, 3, 4, 5],
        );
        let constraints = constraints_of(1, &left, &right);
        assert_eq!(constraints_mod::coplanar_pair_count(&constraints), 1);
        let coplanar = coplanar_mod::build(None, 1, &constraints).unwrap();
        let arrangement = arrange(&constraints, Some(&coplanar), Side::Left);
        assert_eq!(point_count(&arrangement), 9);
        assert_eq!(segment_count(&arrangement), 9);
        let crossing = find_point(&arrangement, 2., 3., 0.);
        assert!(crossing >= 0, "mixed exact crossing at (2,3,0) is missing");
        assert_eq!(incident(&arrangement, crossing), 4);
    }

    fn many_parallel_plan(count: usize, point_contact: bool) -> (geometry::T, geometry::T) {
        let scale = count as f64;
        let left = triangle(&[(-scale, -scale, 0.), (2. * scale, -scale, 0.), (scale / 2., 2. * scale, 0.)]);
        let primitive_count = count + usize::from(point_contact);
        let mut points = vec![(0., 0., 0.); primitive_count * 3];
        let triangles: Vec<i64> = (0..(primitive_count * 3) as i64).collect();
        for segment in 0..count {
            let point = segment * 3;
            let x = segment as f64 + 0.25;
            points[point] = (x, -1., -1.);
            points[point + 1] = (x, 1., -1.);
            points[point + 2] = (x, 0., 1.);
        }
        if point_contact {
            let point = count * 3;
            let x = (count / 2) as f64 + 0.25;
            points[point] = (x, 0., 0.);
            points[point + 1] = (x - 0.25, 0., 1.);
            points[point + 2] = (x + 0.25, 0., 1.);
        }
        (left, geom(&points, &triangles))
    }

    type Signature = (Vec<P>, Vec<(i64, i64)>);

    fn signature(value: &T) -> Signature {
        (
            (0..point_count(value)).map(|p| approx(value, p)).collect(),
            (0..segment_count(value)).map(|s| (segment_first(value, s), segment_second(value, s))).collect(),
        )
    }

    use parallel::with_domains as on_domains;

    #[test]
    fn sparse_sweep_and_point_candidates() {
        let count = 64usize;
        let run = |domains: i64, point_contact: bool| {
            on_domains(domains, &|| {
                let (left, right) = many_parallel_plan(count, point_contact);
                let constraints = constraints_of(7, &left, &right);
                signature(&arrange(&constraints, None, Side::Left))
            })
        };
        let plain = run(1, false);
        assert!(plain.0.len() == count * 2 && plain.1.len() == count);
        assert!(plain == run(4, false), "sparse face sweep differs between domain counts");
        let contacted = run(1, true);
        assert!(contacted.0.len() == count * 2 + 1 && contacted.1.len() == count + 1);
        assert!(contacted == run(4, true));
    }

    fn multiway_plan(count: usize) -> Arc<constraints_mod::T> {
        let left = triangle(&[(-10., -10., 0.), (10., -10., 0.), (0., 10., 0.)]);
        let mut points = vec![(0., 0., 0.); count * 3];
        let triangles: Vec<i64> = (0..(count * 3) as i64).collect();
        for line in 0..count {
            let angle = std::f64::consts::PI * line as f64 / count as f64;
            let dx = 4. * angle.cos();
            let dy = 4. * angle.sin();
            let point = line * 3;
            points[point] = (-dx, -dy, -1.);
            points[point + 1] = (dx, dy, -1.);
            points[point + 2] = (0., 0., 1.);
        }
        constraints_of(7, &left, &geom(&points, &triangles))
    }

    #[test]
    fn dense_multiway_sweep() {
        let count = 96usize;
        let run = |domains: i64| on_domains(domains, &|| signature(&arrange(&multiway_plan(count), None, Side::Left)));
        let arrangement = run(1);
        assert!(
            arrangement.0.len() == count * 2 + 1 && arrangement.1.len() == count * 2,
            "dense face sweep did not aggregate a multi-way exact crossing"
        );
        assert!(arrangement == run(4), "dense face sweep differs between domain counts");
    }

    #[test]
    fn cancellation() {
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), None, &crossing_plan(), Side::Left, 0) {
            Err(e) => assert_eq!(error::code(&e), Code::Cancelled),
            Ok(_) => panic!("cancelled face arrangement completed"),
        }
    }

    #[test]
    fn split_sort_keeps_equal_axis_points_in_source_order() {
        let source = implicit_point::source((0..12).map(|i| (i / 3) as f64).collect(), vec![0.; 12], vec![0.; 12]).unwrap();
        let points: Vec<_> = (0..12).map(|i| implicit_point::explicit(&source, i).unwrap()).collect();
        let mut order = vec![-1, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -2];
        sort_range(&mut order, 1, 13, &|a, b| implicit_point::compare_x(&points[a as usize], &points[b as usize]));
        assert_eq!(order, [-1, 2, 1, 0, 5, 4, 3, 8, 7, 6, 11, 10, 9, -2]);
        if std::env::var_os("RAYS_SORT_BENCH").is_some() {
            const COUNT: usize = 2048;
            let source = implicit_point::source((0..COUNT).map(|i| (i / 3) as f64).collect(), vec![0.; COUNT], vec![0.; COUNT]).unwrap();
            let points: Vec<_> = (0..COUNT as i64).map(|i| implicit_point::explicit(&source, i).unwrap()).collect();
            let input: Vec<i64> = (0..COUNT as i64).rev().collect();
            let mut samples = Vec::new();
            for _ in 0..7 {
                let mut values = input.clone();
                let started = std::time::Instant::now();
                sort_range(&mut values, 0, COUNT, &|a, b| implicit_point::compare_x(&points[a as usize], &points[b as usize]));
                samples.push(started.elapsed().as_nanos());
                assert!(values.windows(2).all(|pair| implicit_point::compare_x(&points[pair[0] as usize], &points[pair[1] as usize]) <= 0));
            }
            samples.sort_unstable();
            println!("exact-axis sort: median {} ns (7 sorts of {COUNT} points; setup excluded)", samples[3]);
        }
    }
}
