//! Deterministic triangulation of one simple outer contour with simple holes.
use rdk_core::{cancel, error, polygon_triangulation};

const EPSILON: f64 = 1e-12;

enum Stop {
    Fail(String),
    Cancelled(error::T),
}

impl From<error::T> for Stop {
    fn from(value: error::T) -> Stop {
        Stop::Cancelled(value)
    }
}

fn fail(message: &str) -> Stop {
    Stop::Fail(message.to_owned())
}

/// OCaml `Float.min`.
fn float_min(x: f64, y: f64) -> f64 {
    if y > x || (!y.is_sign_negative() && x.is_sign_negative()) {
        if y.is_nan() { y } else { x }
    } else if x.is_nan() {
        x
    } else {
        y
    }
}

/// OCaml `Float.max`.
fn float_max(x: f64, y: f64) -> f64 {
    if y > x || (!y.is_sign_negative() && x.is_sign_negative()) {
        if x.is_nan() { x } else { y }
    } else if y.is_nan() {
        y
    } else {
        x
    }
}

/// OCaml `Float.compare` (nan equals itself and sorts first).
fn float_compare(a: f64, b: f64) -> std::cmp::Ordering {
    use std::cmp::Ordering::*;
    if a < b {
        Less
    } else if a > b {
        Greater
    } else if a == b {
        Equal
    } else if a.is_nan() {
        if b.is_nan() { Equal } else { Less }
    } else {
        Greater
    }
}

fn cross(x: &[f64], y: &[f64], a: i64, b: i64, c: i64) -> f64 {
    let (a, b, c) = (a as usize, b as usize, c as usize);
    ((x[b] - x[a]) * (y[c] - y[a])) - ((y[b] - y[a]) * (x[c] - x[a]))
}

fn same_point(x: &[f64], y: &[f64], a: i64, b: i64) -> bool {
    let (a, b) = (a as usize, b as usize);
    (x[a] - x[b]).abs() <= EPSILON && (y[a] - y[b]).abs() <= EPSILON
}

fn signed_area(x: &[f64], y: &[f64], contour: &[i64]) -> f64 {
    let mut area = 0.;
    let count = contour.len();
    for index in 0..count {
        let next = if index + 1 == count { 0 } else { index + 1 };
        let a = contour[index] as usize;
        let b = contour[next] as usize;
        area += (x[a] * y[b]) - (x[b] * y[a]);
    }
    area
}

fn oriented(
    x: &[f64],
    y: &[f64],
    counter_clockwise: bool,
    contour: Vec<i64>,
) -> Result<Vec<i64>, Stop> {
    let area = signed_area(x, y, &contour);
    if !area.is_finite() || area.abs() <= EPSILON {
        return Err(fail("contour has degenerate projected area"));
    }
    if (area > 0.) == counter_clockwise {
        Ok(contour)
    } else {
        Ok(contour.into_iter().rev().collect())
    }
}

fn point_inside(x: &[f64], y: &[f64], contour: &[i64], px: f64, py: f64) -> bool {
    let mut inside = false;
    let count = contour.len();
    let mut previous = count - 1;
    for current in 0..count {
        let a = contour[previous] as usize;
        let b = contour[current] as usize;
        if (y[a] > py) != (y[b] > py) && px < ((x[b] - x[a]) * (py - y[a]) / (y[b] - y[a])) + x[a]
        {
            inside = !inside;
        }
        previous = current;
    }
    inside
}

fn between(value: f64, left: f64, right: f64) -> bool {
    value >= float_min(left, right) - EPSILON && value <= float_max(left, right) + EPSILON
}

fn point_on_segment(x: &[f64], y: &[f64], point: i64, a: i64, b: i64) -> bool {
    cross(x, y, a, b, point).abs() <= EPSILON
        && between(x[point as usize], x[a as usize], x[b as usize])
        && between(y[point as usize], y[a as usize], y[b as usize])
}

fn segments_intersect(x: &[f64], y: &[f64], a: i64, b: i64, c: i64, d: i64) -> bool {
    let ab_c = cross(x, y, a, b, c);
    let ab_d = cross(x, y, a, b, d);
    let cd_a = cross(x, y, c, d, a);
    let cd_b = cross(x, y, c, d, b);
    (((ab_c > EPSILON && ab_d < -EPSILON) || (ab_c < -EPSILON && ab_d > EPSILON))
        && ((cd_a > EPSILON && cd_b < -EPSILON) || (cd_a < -EPSILON && cd_b > EPSILON)))
        || (ab_c.abs() <= EPSILON && point_on_segment(x, y, c, a, b))
        || (ab_d.abs() <= EPSILON && point_on_segment(x, y, d, a, b))
        || (cd_a.abs() <= EPSILON && point_on_segment(x, y, a, c, d))
        || (cd_b.abs() <= EPSILON && point_on_segment(x, y, b, c, d))
}

fn clear_of_contour(x: &[f64], y: &[f64], a: i64, b: i64, contour: &[i64]) -> bool {
    let count = contour.len();
    for edge in 0..count {
        let c = contour[edge];
        let d = contour[if edge + 1 == count { 0 } else { edge + 1 }];
        if c != a && d != a && c != b && d != b && segments_intersect(x, y, a, b, c, d) {
            return false;
        }
    }
    true
}

fn splice(outer: &[i64], outer_index: usize, hole: &[i64], hole_index: usize) -> Vec<i64> {
    let outer_count = outer.len();
    let hole_count = hole.len();
    (0..outer_count + hole_count + 2)
        .map(|output| {
            if output <= outer_index {
                outer[output]
            } else {
                let local = output - outer_index - 1;
                if local <= hole_count {
                    hole[(hole_index + local) % hole_count]
                } else {
                    outer[outer_index + local - hole_count - 1]
                }
            }
        })
        .collect()
}

fn run(
    cancel_value: Option<&cancel::T>,
    point: &dyn Fn(i64) -> (f64, f64),
    outer: &[i64],
    holes: &[Vec<i64>],
) -> Result<Vec<i64>, Stop> {
    if outer.len() < 3 {
        return Err(fail("outer contour has fewer than three points"));
    }
    for (hole, contour) in holes.iter().enumerate() {
        if contour.len() < 3 {
            return Err(Stop::Fail(format!(
                "hole contour {} has fewer than three points",
                hole
            )));
        }
    }
    let original_count: i64 =
        outer.len() as i64 + holes.iter().map(|contour| contour.len() as i64).sum::<i64>();
    let original_count = original_count as usize;
    let mut token = vec![0i64; original_count];
    let mut raw_x = vec![0.; original_count];
    let mut raw_y = vec![0.; original_count];
    let mut at = 0usize;
    let mut copy_contour = |contour: &[i64]| -> Result<Vec<i64>, Stop> {
        let mut local = vec![0i64; contour.len()];
        for (index, &value) in contour.iter().enumerate() {
            cancel::check_opt(cancel_value)?;
            let (px, py) = point(value);
            if !(px.is_finite() && py.is_finite()) {
                return Err(fail("contour contains a non-finite projected point"));
            }
            token[at] = value;
            raw_x[at] = px;
            raw_y[at] = py;
            local[index] = at as i64;
            at += 1;
        }
        Ok(local)
    };
    let outer = copy_contour(outer)?;
    let mut hole_list = Vec::with_capacity(holes.len());
    for contour in holes {
        hole_list.push(copy_contour(contour)?);
    }
    let holes = hole_list;
    let mut coordinate_scale: f64 = 0.;
    for index in 0..original_count {
        coordinate_scale = float_max(coordinate_scale, raw_x[index].abs());
        coordinate_scale = float_max(coordinate_scale, raw_y[index].abs());
    }
    if coordinate_scale == 0. {
        return Err(fail("contours have zero coordinate extent"));
    }
    let mut x = vec![0.; original_count];
    let mut y = vec![0.; original_count];
    let mut min_x = f64::INFINITY;
    let mut max_x = f64::NEG_INFINITY;
    let mut min_y = f64::INFINITY;
    let mut max_y = f64::NEG_INFINITY;
    for index in 0..original_count {
        x[index] = raw_x[index] / coordinate_scale;
        y[index] = raw_y[index] / coordinate_scale;
        min_x = float_min(min_x, x[index]);
        max_x = float_max(max_x, x[index]);
        min_y = float_min(min_y, y[index]);
        max_y = float_max(max_y, y[index]);
    }
    let projection_scale = float_max(max_x - min_x, max_y - min_y);
    if projection_scale == 0. || !projection_scale.is_finite() {
        return Err(fail("contours have degenerate projected extent"));
    }
    for index in 0..original_count {
        x[index] = (x[index] - min_x) / projection_scale;
        y[index] = (y[index] - min_y) / projection_scale;
    }
    let outer = oriented(&x, &y, true, outer)?;
    let mut oriented_holes = Vec::with_capacity(holes.len());
    for contour in holes {
        oriented_holes.push(oriented(&x, &y, false, contour)?);
    }
    let holes = oriented_holes;
    let mut contours = vec![outer.clone()];
    contours.extend(holes.iter().cloned());
    let rightmost = |contour: &Vec<i64>| -> usize {
        let mut best = 0usize;
        for index in 1..contour.len() {
            let point = contour[index] as usize;
            let current = contour[best] as usize;
            if x[point] > x[current] || (x[point] == x[current] && y[point] < y[current]) {
                best = index;
            }
        }
        best
    };
    let hole_bridge: Vec<usize> = holes.iter().map(rightmost).collect();
    let mut order: Vec<usize> = (0..holes.len()).collect();
    // The comparator is a total order (it ends on the index), so sort stability is moot.
    order.sort_by(|&left, &right| {
        let lp = holes[left][hole_bridge[left]] as usize;
        let rp = holes[right][hole_bridge[right]] as usize;
        let compared = float_compare(x[rp], x[lp]);
        if compared != std::cmp::Ordering::Equal {
            return compared;
        }
        let compared = float_compare(y[lp], y[rp]);
        if compared != std::cmp::Ordering::Equal {
            return compared;
        }
        left.cmp(&right)
    });
    let mut ring = outer.clone();
    for &hole_number in &order {
        cancel::check_opt(cancel_value)?;
        let hole = &holes[hole_number];
        let hole_index = hole_bridge[hole_number];
        let h = hole[hole_index];
        let mut best: i64 = -1;
        let mut best_distance = f64::INFINITY;
        for candidate in 0..ring.len() {
            let v = ring[candidate];
            if !same_point(&x, &y, h, v) {
                let (hu, vu) = (h as usize, v as usize);
                let mx = (x[hu] + x[vu]) * 0.5;
                let my = (y[hu] + y[vu]) * 0.5;
                let in_material = point_inside(&x, &y, &outer, mx, my)
                    && !holes.iter().any(|contour| point_inside(&x, &y, contour, mx, my));
                let clear = in_material
                    && clear_of_contour(&x, &y, h, v, &ring)
                    && contours.iter().all(|contour| clear_of_contour(&x, &y, h, v, contour));
                if clear {
                    let dx = x[hu] - x[vu];
                    let dy = y[hu] - y[vu];
                    let distance = (dx * dx) + (dy * dy);
                    if distance < best_distance - EPSILON
                        || (distance - best_distance).abs() <= EPSILON
                            && (best < 0 || (candidate as i64) < best)
                    {
                        best = candidate as i64;
                        best_distance = distance;
                    }
                }
            }
        }
        if best < 0 {
            return Err(Stop::Fail(format!(
                "hole contour {} has no non-crossing visibility bridge",
                hole_number
            )));
        }
        ring = splice(&ring, best as usize, hole, hole_index);
    }
    let ring_count = ring.len();
    let mut output = vec![0i64; (ring_count - 2) * 3];
    let mut remaining = ring.clone();
    let mut active = ring_count;
    let mut emitted = 0usize;
    while active > 3 {
        cancel::check_opt(cancel_value)?;
        let distinct = |p: i64, a: i64, b: i64, c: i64| -> bool {
            !(same_point(&x, &y, p, a) || same_point(&x, &y, p, b) || same_point(&x, &y, p, c))
        };
        let ear = polygon_triangulation::find_ear(&distinct, &x, &y, 0, 1., &remaining, active as i64);
        if ear < 0 {
            return Err(fail("bridged contours are non-simple or cannot be triangulated"));
        }
        let ear = ear as usize;
        let a = remaining[(ear + active - 1) % active];
        let b = remaining[ear];
        let c = remaining[(ear + 1) % active];
        let base = emitted * 3;
        output[base] = token[a as usize];
        output[base + 1] = token[b as usize];
        output[base + 2] = token[c as usize];
        remaining.copy_within(ear + 1..active, ear);
        active -= 1;
        emitted += 1;
    }
    let base = emitted * 3;
    output[base] = token[remaining[0] as usize];
    output[base + 1] = token[remaining[1] as usize];
    output[base + 2] = token[remaining[2] as usize];
    Ok(output)
}

/// Returns flattened token triples. The outer `Err` is a cancellation (an OCaml exception); the
/// inner `Err` is the OCaml `result` error message.
pub fn triangulate(
    cancel_value: Option<&cancel::T>,
    point: &dyn Fn(i64) -> (f64, f64),
    outer: &[i64],
    holes: &[Vec<i64>],
) -> Result<Result<Vec<i64>, String>, error::T> {
    match run(cancel_value, point, outer, holes) {
        Ok(output) => Ok(Ok(output)),
        Err(Stop::Fail(message)) => Ok(Err(message)),
        Err(Stop::Cancelled(value)) => Err(value),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pts() -> Vec<(f64, f64)> {
        vec![
            (0., 0.),
            (10., 0.),
            (10., 10.),
            (0., 10.),
            (3., 3.),
            (3., 7.),
            (7., 7.),
            (7., 3.),
        ]
    }

    #[test]
    fn square_with_hole_triangulates() {
        let p = pts();
        let point = |i: i64| p[i as usize];
        let out = triangulate(None, &point, &[0, 1, 2, 3], &[vec![4, 5, 6, 7]])
            .unwrap()
            .unwrap();
        assert_eq!(out.len(), (4 + 4 + 2 - 2) * 3);
        let mut area = 0.;
        for t in out.chunks(3) {
            let (a, b, c) = (p[t[0] as usize], p[t[1] as usize], p[t[2] as usize]);
            area += ((b.0 - a.0) * (c.1 - a.1) - (b.1 - a.1) * (c.0 - a.0)) / 2.;
        }
        assert!((area.abs() - 84.).abs() < 1e-9);
    }

    #[test]
    fn rejects_short_outer() {
        let p = pts();
        let point = |i: i64| p[i as usize];
        assert_eq!(
            triangulate(None, &point, &[0, 1], &[]).unwrap().unwrap_err(),
            "outer contour has fewer than three points"
        );
    }
}
