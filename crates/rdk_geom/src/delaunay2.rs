//! Packed deterministic unconstrained Delaunay triangulation in two dimensions.
//!
//! Combinatorial decisions use exact binary64 predicates; no epsilon participates in topology.
use rays_math::float::compare as float_compare;
use std::cmp::Ordering;

use rays_math::rand;
use rdk_core::{cancel, error};
use rdk_core::error::Code;

use crate::exact_dyadic;
use crate::predicates::{self, Sign};

pub struct T {
    source_count: i64,
    unique_source: Vec<i64>,
    source_unique: Vec<i64>,
    triangle_points: Vec<i64>,
}

fn invalid(label: &str, message: String) -> error::T {
    error::new(label, Code::InvalidArgument, message)
}

fn check_index(label: &str, count: i64, index: i64) -> Result<(), error::T> {
    if index < 0 || index >= count {
        return Err(invalid(
            &format!("Delaunay2.{}", label),
            format!(
                "Delaunay2.{}: index {} is outside [0,{})",
                label, index, count
            ),
        ));
    }
    Ok(())
}

pub fn unique_source(value: &T, unique: i64) -> Result<i64, error::T> {
    check_index("unique_source", value.unique_source.len() as i64, unique)?;
    Ok(value.unique_source[unique as usize])
}

pub fn source_unique(value: &T, source: i64) -> Result<i64, error::T> {
    check_index("source_unique", value.source_count, source)?;
    Ok(value.source_unique[source as usize])
}

#[allow(non_snake_case)]
pub mod view {
    use super::T;

    #[derive(Clone, Debug)]
    pub struct View {
        pub unique_source: Vec<i64>,
        pub triangle_points: Vec<i64>,
    }

    pub fn view(value: &T) -> View {
        View {
            unique_source: value.unique_source.clone(),
            triangle_points: value.triangle_points.clone(),
        }
    }
}

struct Triangles {
    a: Vec<i64>,
    b: Vec<i64>,
    c: Vec<i64>,
    n0: Vec<i64>,
    n1: Vec<i64>,
    n2: Vec<i64>,
    alive: Vec<bool>,
    count: i64,
    free: Vec<i64>,
}

fn make_triangles() -> Triangles {
    Triangles {
        a: Vec::new(),
        b: Vec::new(),
        c: Vec::new(),
        n0: Vec::new(),
        n1: Vec::new(),
        n2: Vec::new(),
        alive: Vec::new(),
        count: 0,
        free: Vec::new(),
    }
}

fn allocate_triangle(triangles: &mut Triangles) -> i64 {
    if let Some(reused) = triangles.free.pop() {
        reused
    } else {
        let result = triangles.count;
        triangles.count = result + 1;
        triangles.a.push(0);
        triangles.b.push(0);
        triangles.c.push(0);
        triangles.n0.push(-1);
        triangles.n1.push(-1);
        triangles.n2.push(-1);
        triangles.alive.push(false);
        result
    }
}

fn release_triangle(triangles: &mut Triangles, triangle: i64) -> Result<(), String> {
    if !triangles.alive[triangle as usize] {
        return Err("Delaunay triangulation released an inactive triangle".to_owned());
    }
    triangles.alive[triangle as usize] = false;
    triangles.free.push(triangle);
    Ok(())
}

fn neighbor(triangles: &Triangles, triangle: i64, edge: i64) -> i64 {
    let t = triangle as usize;
    match edge {
        0 => triangles.n0[t],
        1 => triangles.n1[t],
        _ => triangles.n2[t],
    }
}

fn edge_first(triangles: &Triangles, triangle: i64, edge: i64) -> i64 {
    let t = triangle as usize;
    match edge {
        0 => triangles.a[t],
        1 => triangles.b[t],
        _ => triangles.c[t],
    }
}

fn edge_second(triangles: &Triangles, triangle: i64, edge: i64) -> i64 {
    let t = triangle as usize;
    match edge {
        0 => triangles.b[t],
        1 => triangles.c[t],
        _ => triangles.a[t],
    }
}

fn replace_neighbor_edge(
    triangles: &mut Triangles,
    triangle: i64,
    first: i64,
    second: i64,
    replacement: i64,
) -> Result<(), String> {
    if triangle >= 0 {
        let mut found = -1;
        for edge in 0..3 {
            let other_first = edge_first(triangles, triangle, edge);
            let other_second = edge_second(triangles, triangle, edge);
            if other_first == second && other_second == first {
                found = edge;
            }
        }
        if found < 0 {
            return Err("Delaunay triangulation lost a boundary edge".to_owned());
        }
        let t = triangle as usize;
        match found {
            0 => triangles.n0[t] = replacement,
            1 => triangles.n1[t] = replacement,
            _ => triangles.n2[t] = replacement,
        }
    }
    Ok(())
}

fn compare_source_position(x: &[f64], y: &[f64], left: i64, right: i64) -> Ordering {
    let (l, r) = (left as usize, right as usize);
    // Numeric duplicate positions include both signed zeros; keep those keys adjacent.
    let comparison = float_compare(0. + x[l], 0. + x[r]);
    if comparison != Ordering::Equal {
        return comparison;
    }
    let comparison = float_compare(0. + y[l], 0. + y[r]);
    if comparison != Ordering::Equal {
        return comparison;
    }
    left.cmp(&right)
}

fn exact_duplicates(x: &[f64], y: &[f64]) -> (Vec<f64>, Vec<f64>, Vec<i64>, Vec<i64>) {
    let source_count = x.len();
    let mut order: Vec<i64> = (0..source_count as i64).collect();
    // Total order (ends on the index), so the sort's stability is moot.
    order.sort_by(|&l, &r| compare_source_position(x, y, l, r));
    let mut unique_x = Vec::with_capacity(source_count);
    let mut unique_y = Vec::with_capacity(source_count);
    let mut unique_source = Vec::with_capacity(source_count);
    let mut source_unique = vec![0i64; source_count];
    let mut position = 0usize;
    while position < source_count {
        let first = order[position] as usize;
        let mut last = position + 1;
        let mut representative = order[position];
        while last < source_count
            && x[order[last] as usize] == x[first]
            && y[order[last] as usize] == y[first]
        {
            representative = std::cmp::min(representative, order[last]);
            last += 1;
        }
        let unique = unique_x.len() as i64;
        unique_x.push(x[first]);
        unique_y.push(y[first]);
        unique_source.push(representative);
        for index in position..last {
            source_unique[order[index] as usize] = unique;
        }
        position = last;
    }
    (unique_x, unique_y, unique_source, source_unique)
}

fn shuffle_spatial_blocks(seed: i64, order: &mut [i64]) -> Result<(), String> {
    let mut random = rand::seed(seed);
    let block = 256usize;
    let mut first = 0usize;
    while first < order.len() {
        let last = std::cmp::min(order.len(), first + block);
        let mut index = last - 1;
        while index > first {
            let (offset, next) = rand::int((index - first + 1) as i64, random)?;
            random = next;
            let other = first + offset as usize;
            order.swap(index, other);
            index -= 1;
        }
        first = last;
    }
    Ok(())
}

fn canonical_triangle_into(output: &mut [i64], offset: usize, first: i64, second: i64, third: i64) {
    if first <= second && first <= third {
        output[offset] = first;
        output[offset + 1] = second;
        output[offset + 2] = third;
    } else if second <= first && second <= third {
        output[offset] = second;
        output[offset + 1] = third;
        output[offset + 2] = first;
    } else {
        output[offset] = third;
        output[offset + 1] = first;
        output[offset + 2] = second;
    }
}

/// Return the binary exponent of a finite nonnegative value.
fn frexp_exponent(value: f64) -> i64 {
    if value == 0. || !value.is_finite() {
        return 0;
    }
    let bits = value.to_bits();
    let biased = ((bits >> 52) & 0x7ff) as i64;
    if biased == 0 {
        // Subnormal: normalize through an exact power-of-two scale.
        let scaled = value * 2f64.powi(100);
        return frexp_exponent(scaled) - 100;
    }
    biased - 1022
}

fn sign_int(sign: Sign) -> i64 {
    match sign {
        Sign::Negative => -1,
        Sign::Zero => 0,
        Sign::Positive => 1,
    }
}

fn run(
    cancel_value: Option<&cancel::T>,
    seed: i64,
    x: &[f64],
    y: &[f64],
) -> Result<T, String> {
    cancel::check_opt(cancel_value).map_err(|_| cancelled())?;
    // Rust panics replace Invalid_argument in predicates: equal length and finiteness are validated here.
    if x.len() != y.len() {
        return Err("Delaunay x and y arrays must have equal cardinality".to_owned());
    }
    for (point, value) in x.iter().enumerate() {
        if !(value.is_finite() && y[point].is_finite()) {
            return Err("Delaunay coordinates must be finite".to_owned());
        }
    }
    let source_count = x.len() as i64;
    let (unique_x, unique_y, unique_source, source_unique) = exact_duplicates(x, y);
    let point_count = unique_x.len() as i64;
    if point_count < 3 {
        return Ok(T {
            source_count,
            unique_source,
            source_unique,
            triangle_points: Vec::new(),
        });
    }
    let pc = point_count as usize;
    let mut maximum: f64 = 0.;
    for point in 0..pc {
        let m = {
            let (a, b) = (unique_x[point].abs(), unique_y[point].abs());
            if a >= b { a } else { b }
        };
        maximum = if maximum >= m { maximum } else { m };
    }
    let base_exponent = frexp_exponent(maximum);
    let radius_exponent = std::cmp::min(1026, std::cmp::max(2, base_exponent + 2));
    let super_weight = exact_dyadic::ldexp(1., -radius_exponent);
    let finite_super = radius_exponent <= 1021;
    let exact_one = exact_dyadic::of_float(1.);
    let exact_weight = exact_dyadic::of_float(super_weight);
    let super_x = [-4., 4., 0.];
    let super_y = [-2., -2., 4.];
    let exact_super_x: Vec<_> = super_x.iter().map(|&v| exact_dyadic::of_float(v)).collect();
    let exact_super_y: Vec<_> = super_y.iter().map(|&v| exact_dyadic::of_float(v)).collect();
    let (predicate_x, predicate_y) = if finite_super {
        let mut vx = unique_x.clone();
        let mut vy = unique_y.clone();
        for local in 0..3 {
            vx.push(exact_dyadic::ldexp(super_x[local], radius_exponent));
            vy.push(exact_dyadic::ldexp(super_y[local], radius_exponent));
        }
        (vx, vy)
    } else {
        (unique_x.clone(), unique_y.clone())
    };
    let homogeneous = |point: i64| -> (exact_dyadic::T, exact_dyadic::T, exact_dyadic::T) {
        if point < point_count {
            (
                exact_dyadic::of_float(unique_x[point as usize]),
                exact_dyadic::of_float(unique_y[point as usize]),
                exact_one.clone(),
            )
        } else {
            let local = (point - point_count) as usize;
            (
                exact_super_x[local].clone(),
                exact_super_y[local].clone(),
                exact_weight.clone(),
            )
        }
    };
    let orient = |first: i64, second: i64, third: i64| -> i64 {
        if finite_super || (first < point_count && second < point_count && third < point_count) {
            sign_int(predicates::orient2d_packed(
                &predicate_x,
                &predicate_y,
                first,
                second,
                third,
            ))
        } else {
            let (ax, ay, aw) = homogeneous(first);
            let (bx, by, bw) = homogeneous(second);
            let (cx, cy, cw) = homogeneous(third);
            exact_dyadic::scratch::orient2d_homogeneous(
                &ax, &ay, &aw, &bx, &by, &bw, &cx, &cy, &cw,
            )
        }
    };
    let incircle = |first: i64, second: i64, third: i64, point: i64| -> i64 {
        if finite_super
            || (first < point_count
                && second < point_count
                && third < point_count
                && point < point_count)
        {
            sign_int(predicates::incircle_packed(
                &predicate_x,
                &predicate_y,
                first,
                second,
                third,
                point,
            ))
        } else {
            let (ax, ay, aw) = homogeneous(first);
            let (bx, by, bw) = homogeneous(second);
            let (cx, cy, cw) = homogeneous(third);
            let (dx, dy, dw) = homogeneous(point);
            exact_dyadic::scratch::incircle_homogeneous(
                &ax, &ay, &aw, &bx, &by, &bw, &cx, &cy, &cw, &dx, &dy, &dw,
            )
        }
    };
    let mut triangles = make_triangles();
    let initial = allocate_triangle(&mut triangles);
    let (s0, s1, s2) = (point_count, point_count + 1, point_count + 2);
    if orient(s0, s1, s2) <= 0 {
        return Err("Delaunay internal super triangle is not counter-clockwise".to_owned());
    }
    {
        let i = initial as usize;
        triangles.a[i] = s0;
        triangles.b[i] = s1;
        triangles.c[i] = s2;
        triangles.alive[i] = true;
    }
    let mut insertion_order: Vec<i64> = (0..point_count).collect();
    shuffle_spatial_blocks(seed, &mut insertion_order)?;
    let mut marks: Vec<i64> = Vec::new();
    let mut generation: i64 = 0;
    let mut walk_start = initial;
    let mut work: Vec<i64> = Vec::new();
    let mut boundary_u: Vec<i64> = Vec::new();
    let mut boundary_v: Vec<i64> = Vec::new();
    let mut boundary_outside: Vec<i64> = Vec::new();
    let mut boundary_order: Vec<i64> = Vec::new();
    let mut created: Vec<i64> = Vec::new();
    let mut boundary_next = vec![-1i64; pc + 3];
    let mut boundary_stamp = vec![0i64; pc + 3];
    let mut boundary_generation: i64 = 0;
    for insertion in 0..point_count {
        cancel::poll(cancel_value, insertion, 255).map_err(|_| cancelled())?;
        let point = insertion_order[insertion as usize];
        if marks.len() < triangles.a.len() {
            marks.resize(triangles.a.len(), 0);
        }
        let mut current = walk_start;
        let mut steps: i64 = 0;
        let mut located = false;
        while !located && steps <= triangles.count {
            if current < 0 || current >= triangles.count || !triangles.alive[current as usize] {
                return Err("Delaunay point-location walk reached inactive topology".to_owned());
            }
            let c = current as usize;
            let first = triangles.a[c];
            let second = triangles.b[c];
            let third = triangles.c[c];
            let ab = orient(first, second, point);
            if ab < 0 {
                current = triangles.n0[c];
            } else {
                let bc = orient(second, third, point);
                if bc < 0 {
                    current = triangles.n1[c];
                } else {
                    let ca = orient(third, first, point);
                    if ca < 0 {
                        current = triangles.n2[c];
                    } else {
                        located = true;
                    }
                }
            }
            steps += 1;
        }
        if !located {
            return Err("Delaunay point-location walk did not converge".to_owned());
        }
        generation += 1;
        if generation == i64::MAX {
            marks.iter_mut().for_each(|m| *m = 0);
            generation = 1;
        }
        let visited_generation = generation;
        work.clear();
        work.push(current);
        let mut cursor = 0usize;
        marks[current as usize] = visited_generation;
        while cursor < work.len() {
            let triangle = work[cursor];
            cursor += 1;
            for edge in 0..3 {
                let adjacent = neighbor(&triangles, triangle, edge);
                if adjacent >= 0 && marks[adjacent as usize] != visited_generation {
                    marks[adjacent as usize] = visited_generation;
                    let adj = adjacent as usize;
                    if incircle(triangles.a[adj], triangles.b[adj], triangles.c[adj], point) >= 0 {
                        work.push(adjacent);
                    }
                }
            }
        }
        let cur = current as usize;
        if incircle(triangles.a[cur], triangles.b[cur], triangles.c[cur], point) < 0 {
            return Err("Delaunay cavity is empty for a located point".to_owned());
        }
        generation += 1;
        if generation == i64::MAX {
            marks.iter_mut().for_each(|m| *m = 0);
            generation = 1;
        }
        let cavity_generation = generation;
        for &t in &work {
            marks[t as usize] = cavity_generation;
        }
        boundary_u.clear();
        boundary_v.clear();
        boundary_outside.clear();
        for &triangle in &work {
            for edge in 0..3 {
                let adjacent = neighbor(&triangles, triangle, edge);
                if adjacent < 0 || marks[adjacent as usize] != cavity_generation {
                    boundary_u.push(edge_first(&triangles, triangle, edge));
                    boundary_v.push(edge_second(&triangles, triangle, edge));
                    boundary_outside.push(adjacent);
                }
            }
        }
        let boundary_count = boundary_u.len();
        if boundary_count < 3 {
            return Err("Delaunay cavity has an invalid boundary".to_owned());
        }
        boundary_generation += 1;
        if boundary_generation == i64::MAX {
            boundary_stamp.iter_mut().for_each(|m| *m = 0);
            boundary_generation = 1;
        }
        for boundary in 0..boundary_count {
            let first = boundary_u[boundary] as usize;
            if boundary_stamp[first] == boundary_generation {
                return Err(format!(
                    "Delaunay cavity boundary repeats start {} while inserting point {}",
                    first, point
                ));
            }
            boundary_stamp[first] = boundary_generation;
            boundary_next[first] = boundary as i64;
        }
        boundary_order.clear();
        boundary_order.resize(boundary_count, 0);
        let mut start = 0usize;
        for boundary in 1..boundary_count {
            if boundary_u[boundary] < boundary_u[start]
                || (boundary_u[boundary] == boundary_u[start]
                    && boundary_v[boundary] < boundary_v[start])
            {
                start = boundary;
            }
        }
        let mut boundary = start as i64;
        for index in 0..boundary_count {
            if boundary < 0 {
                return Err("Delaunay cavity boundary cycle is open".to_owned());
            }
            boundary_order[index] = boundary;
            let next_start = boundary_v[boundary as usize] as usize;
            boundary = if boundary_stamp[next_start] == boundary_generation {
                boundary_next[next_start]
            } else {
                -1
            };
        }
        if boundary != start as i64 {
            return Err("Delaunay cavity boundary contains multiple cycles".to_owned());
        }
        for &t in &work {
            release_triangle(&mut triangles, t)?;
        }
        created.clear();
        created.resize(boundary_count, 0);
        for index in 0..boundary_count {
            let boundary = boundary_order[index] as usize;
            let triangle = allocate_triangle(&mut triangles);
            let first = boundary_u[boundary];
            let second = boundary_v[boundary];
            let outside = boundary_outside[boundary];
            if orient(first, second, point) <= 0 {
                return Err("Delaunay cavity emitted a non-positive triangle".to_owned());
            }
            let t = triangle as usize;
            triangles.a[t] = first;
            triangles.b[t] = second;
            triangles.c[t] = point;
            triangles.n0[t] = outside;
            triangles.alive[t] = true;
            if outside >= 0 {
                replace_neighbor_edge(&mut triangles, outside, first, second, triangle)?;
            }
            created[index] = triangle;
        }
        let n = boundary_count;
        for index in 0..n {
            let t = created[index] as usize;
            triangles.n1[t] = created[(index + 1) % n];
            triangles.n2[t] = created[(index + n - 1) % n];
        }
        walk_start = created[0];
    }
    let mut output_count = 0usize;
    for triangle in 0..triangles.count as usize {
        if triangles.alive[triangle]
            && triangles.a[triangle] < point_count
            && triangles.b[triangle] < point_count
            && triangles.c[triangle] < point_count
        {
            output_count += 1;
        }
    }
    let mut unsorted = vec![0i64; output_count * 3];
    let mut output = 0usize;
    for triangle in 0..triangles.count as usize {
        if triangles.alive[triangle]
            && triangles.a[triangle] < point_count
            && triangles.b[triangle] < point_count
            && triangles.c[triangle] < point_count
        {
            let first = unique_source[triangles.a[triangle] as usize];
            let second = unique_source[triangles.b[triangle] as usize];
            let third = unique_source[triangles.c[triangle] as usize];
            canonical_triangle_into(&mut unsorted, output * 3, first, second, third);
            output += 1;
        }
    }
    let mut order: Vec<usize> = (0..output_count).collect();
    order.sort_by(|&left, &right| {
        let (l, r) = (left * 3, right * 3);
        unsorted[l]
            .cmp(&unsorted[r])
            .then(unsorted[l + 1].cmp(&unsorted[r + 1]))
            .then(unsorted[l + 2].cmp(&unsorted[r + 2]))
    });
    let mut triangle_points = vec![0i64; output_count * 3];
    for (triangle, &source) in order.iter().enumerate() {
        let (target, source) = (triangle * 3, source * 3);
        triangle_points[target] = unsorted[source];
        triangle_points[target + 1] = unsorted[source + 1];
        triangle_points[target + 2] = unsorted[source + 2];
    }
    Ok(T {
        source_count,
        unique_source,
        source_unique,
        triangle_points,
    })
}

fn cancelled() -> String {
    "Delaunay triangulation was cancelled".to_owned()
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    seed: Option<i64>,
    x: &[f64],
    y: &[f64],
) -> Result<T, String> {
    run(cancel_value, seed.unwrap_or(0), x, y)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn failures_and_cancellation() {
        assert!(build(None, None, &[0.], &[]).is_err());
        assert!(build(None, None, &[f64::NAN], &[0.]).is_err());
        let token = cancel::create();
        cancel::cancel(&token);
        assert!(build(Some(&token), None, &[0., 1., 0.], &[0., 0., 1.]).is_err());
    }

    #[test]
    fn unit_square_has_two_triangles() {
        let t = build(None, None, &[0., 1., 1., 0., 1.], &[0., 0., 1., 1., 1.]).unwrap();
        assert_eq!(t.triangle_points.len(), 6);
        assert_eq!(source_unique(&t, 4).unwrap(), source_unique(&t, 2).unwrap());
        assert_eq!(unique_source(&t, source_unique(&t, 4).unwrap()).unwrap(), 2);
        assert!(source_unique(&t, 5).is_err());
        let mixed_zeros = build(None, None, &[-0., 0., -0., 1.], &[0., 0., 1., 0.]).unwrap();
        assert_eq!(mixed_zeros.triangle_points.len(), 3);
        assert_eq!(source_unique(&mixed_zeros, 0).unwrap(), source_unique(&mixed_zeros, 1).unwrap());
        assert_eq!(unique_source(&mixed_zeros, source_unique(&mixed_zeros, 0).unwrap()).unwrap(), 0);
    }
}
