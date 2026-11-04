//! Port of lib/rdk/boolean/boolean_coplanar.ml: exact overlap arrangements for coplanar
//! Boolean face pairs.
#![allow(non_snake_case)]

use std::sync::{Arc, Mutex};

use rdk_core::{cancel, error, support};
use rdk_geom::implicit_point::{self, Projection};
use rdk_geom::predicates::Sign;

use crate::boolean_constraints::{self as constraints_mod, Side};

pub const OPERATION: &str = "boolean_coplanar";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OverlapKind {
    Empty,
    Point,
    Segment,
    Polygon,
}

#[derive(Clone)]
struct Overlap {
    kind: OverlapKind,
    points: Vec<implicit_point::T>,
    support_first: Vec<i64>,
    support_second: Vec<i64>,
}

pub struct T {
    constraints: Arc<constraints_mod::T>,
    overlaps: Vec<Overlap>,
    left_offsets: Vec<i64>,
    left_pairs: Vec<i64>,
    right_offsets: Vec<i64>,
    right_pairs: Vec<i64>,
}

fn fail(code: &str, message: &str) -> error::T {
    error::make(OPERATION.to_owned(), code.to_owned(), message.to_owned(), None).unwrap_or_else(|e| e)
}

fn empty() -> Overlap {
    Overlap { kind: OverlapKind::Empty, points: Vec::new(), support_first: Vec::new(), support_second: Vec::new() }
}

pub fn pair_count(value: &T) -> i64 {
    value.overlaps.len() as i64
}
pub fn first_triangle(value: &T, pair: i64) -> i64 {
    constraints_mod::coplanar_first_triangle(&value.constraints, pair)
}
pub fn second_triangle(value: &T, pair: i64) -> i64 {
    constraints_mod::coplanar_second_triangle(&value.constraints, pair)
}
pub fn kind(value: &T, pair: i64) -> OverlapKind {
    value.overlaps[pair as usize].kind
}
pub fn point_count(value: &T, pair: i64) -> i64 {
    value.overlaps[pair as usize].points.len() as i64
}

pub fn boundary_count(value: &T, pair: i64) -> i64 {
    match kind(value, pair) {
        OverlapKind::Empty | OverlapKind::Point => 0,
        OverlapKind::Segment => 1,
        OverlapKind::Polygon => point_count(value, pair),
    }
}

/// `Invalid_argument` in OCaml: a panic here (a contract violation by the caller).
pub fn boundary_first(value: &T, pair: i64, boundary: i64) -> i64 {
    match kind(value, pair) {
        OverlapKind::Segment if boundary == 0 => 0,
        OverlapKind::Polygon => {
            if boundary < 0 || boundary >= point_count(value, pair) {
                unreachable!("Coplanar Boolean boundary index is out of bounds");
            }
            boundary
        }
        _ => unreachable!("Coplanar Boolean overlap has no requested boundary"),
    }
}

pub fn boundary_second(value: &T, pair: i64, boundary: i64) -> i64 {
    match kind(value, pair) {
        OverlapKind::Segment if boundary == 0 => 1,
        OverlapKind::Polygon => {
            let count = point_count(value, pair);
            if boundary < 0 || boundary >= count {
                unreachable!("Coplanar Boolean boundary index is out of bounds");
            }
            (boundary + 1) % count
        }
        _ => unreachable!("Coplanar Boolean overlap has no requested boundary"),
    }
}

pub mod View {
    use super::*;

    pub fn constraints(value: &T) -> &Arc<constraints_mod::T> {
        &value.constraints
    }
    pub fn point(value: &T, pair: i64, point: i64) -> &implicit_point::T {
        &value.overlaps[pair as usize].points[point as usize]
    }
    pub fn boundary_support_first(value: &T, pair: i64, boundary: i64) -> i64 {
        value.overlaps[pair as usize].support_first[boundary as usize]
    }
    pub fn boundary_support_second(value: &T, pair: i64, boundary: i64) -> i64 {
        value.overlaps[pair as usize].support_second[boundary as usize]
    }
    pub fn left_pair_range(value: &T, triangle: i64) -> (i64, i64) {
        (value.left_offsets[triangle as usize], value.left_offsets[triangle as usize + 1])
    }
    pub fn right_pair_range(value: &T, triangle: i64) -> (i64, i64) {
        (value.right_offsets[triangle as usize], value.right_offsets[triangle as usize + 1])
    }
    pub fn left_pair(value: &T, slot: i64) -> i64 {
        value.left_pairs[slot as usize]
    }
    pub fn right_pair(value: &T, slot: i64) -> i64 {
        value.right_pairs[slot as usize]
    }
}

fn sign_opposite(left: Sign, right: Sign) -> bool {
    matches!((left, right), (Sign::Negative, Sign::Positive) | (Sign::Positive, Sign::Negative))
}

fn projection(points: &[implicit_point::T]) -> Projection {
    match implicit_point::orient2d_xy(&points[0], &points[1], &points[2]) {
        Sign::Positive | Sign::Negative => Projection::XY,
        Sign::Zero => match implicit_point::orient2d_yz(&points[0], &points[1], &points[2]) {
            Sign::Positive | Sign::Negative => Projection::YZ,
            Sign::Zero => Projection::ZX,
        },
    }
}

type Orient = fn(&implicit_point::T, &implicit_point::T, &implicit_point::T) -> Sign;

fn orient_of(projection: Projection) -> Orient {
    match projection {
        Projection::XY => implicit_point::orient2d_xy,
        Projection::YZ => implicit_point::orient2d_yz,
        Projection::ZX => implicit_point::orient2d_zx,
    }
}

fn compare_projected(projection: Projection, left: &implicit_point::T, right: &implicit_point::T) -> std::cmp::Ordering {
    let (first, second): (fn(&implicit_point::T, &implicit_point::T) -> i64, fn(&implicit_point::T, &implicit_point::T) -> i64) =
        match projection {
            Projection::XY => (implicit_point::compare_x, implicit_point::compare_y),
            Projection::YZ => (implicit_point::compare_y, implicit_point::compare_z),
            Projection::ZX => (implicit_point::compare_z, implicit_point::compare_x),
        };
    let comparison = first(left, right);
    if comparison != 0 { comparison.cmp(&0) } else { second(left, right).cmp(&0) }
}

/// None for a degenerate triangle (OCaml `Invalid_argument`).
fn inside_triangle(orient: Orient, triangle: &[implicit_point::T], point: &implicit_point::T) -> Option<bool> {
    let orientation = orient(&triangle[0], &triangle[1], &triangle[2]);
    let first = orient(&triangle[0], &triangle[1], point);
    let second = orient(&triangle[1], &triangle[2], point);
    let third = orient(&triangle[2], &triangle[0], point);
    match orientation {
        Sign::Positive => Some(first != Sign::Negative && second != Sign::Negative && third != Sign::Negative),
        Sign::Negative => Some(first != Sign::Positive && second != Sign::Positive && third != Sign::Positive),
        Sign::Zero => None,
    }
}

fn convex_hull(projection: Projection, candidates: &[implicit_point::T], count: usize) -> Vec<implicit_point::T> {
    if count <= 1 {
        return candidates[..count].to_vec();
    }
    let mut points = candidates[..count].to_vec();
    // Candidates are pairwise distinct, so the order is total: sort stability is unobservable.
    points.sort_by(|a, b| compare_projected(projection, a, b));
    let orient = orient_of(projection);
    let mut hull = vec![0usize; count * 2];
    let mut size = 0usize;
    let append = |hull: &mut Vec<usize>, size: &mut usize, lower: usize, index: usize| {
        while *size >= lower
            && *size >= 2
            && orient(&points[hull[*size - 2]], &points[hull[*size - 1]], &points[index]) != Sign::Positive
        {
            *size -= 1;
        }
        hull[*size] = index;
        *size += 1;
    };
    for index in 0..count {
        append(&mut hull, &mut size, 0, index);
    }
    let upper_start = size + 1;
    for index in (0..count - 1).rev() {
        append(&mut hull, &mut size, upper_start, index);
    }
    if size > 1 {
        size -= 1;
    }
    (0..size).map(|index| points[hull[index]].clone()).collect()
}

/// None: Construction_failed or Invalid_argument in OCaml.
fn build_pair(constraints: &constraints_mod::T, pair: i64) -> Option<Overlap> {
    let source = constraints_mod::View::source(constraints);
    let first_side = constraints_mod::coplanar_first_side(constraints, pair);
    let second_side = constraints_mod::coplanar_second_side(constraints, pair);
    let first_face = constraints_mod::coplanar_first_triangle(constraints, pair);
    let second_face = constraints_mod::coplanar_second_triangle(constraints, pair);
    let first_ids: Vec<i64> =
        (0..3).map(|local| constraints_mod::View::triangle_point(constraints, first_side, first_face, local)).collect();
    let second_ids: Vec<i64> =
        (0..3).map(|local| constraints_mod::View::triangle_point(constraints, second_side, second_face, local)).collect();
    let first_triangle: Vec<implicit_point::T> =
        first_ids.iter().map(|&p| implicit_point::explicit(source, p).unwrap()).collect();
    let second_triangle: Vec<implicit_point::T> =
        second_ids.iter().map(|&p| implicit_point::explicit(source, p).unwrap()).collect();
    let projection = projection(&first_triangle);
    let orient = orient_of(projection);
    let mut candidates: Vec<implicit_point::T> = Vec::with_capacity(15);
    let append = |candidates: &mut Vec<implicit_point::T>, point: &implicit_point::T| {
        if !candidates.iter().any(|c| implicit_point::equal(c, point)) {
            candidates.push(point.clone());
        }
    };
    for local in 0..3 {
        if inside_triangle(orient, &second_triangle, &first_triangle[local])? {
            append(&mut candidates, &first_triangle[local]);
        }
        if inside_triangle(orient, &first_triangle, &second_triangle[local])? {
            append(&mut candidates, &second_triangle[local]);
        }
    }
    for first_edge in 0..3 {
        let first_next = (first_edge + 1) % 3;
        for second_edge in 0..3 {
            let second_next = (second_edge + 1) % 3;
            let first = orient(&first_triangle[first_edge], &first_triangle[first_next], &second_triangle[second_edge]);
            let second = orient(&first_triangle[first_edge], &first_triangle[first_next], &second_triangle[second_next]);
            let third = orient(&second_triangle[second_edge], &second_triangle[second_next], &first_triangle[first_edge]);
            let fourth = orient(&second_triangle[second_edge], &second_triangle[second_next], &first_triangle[first_next]);
            if sign_opposite(first, second) && sign_opposite(third, fourth) {
                match implicit_point::line_line(
                    source,
                    projection,
                    first_ids[first_edge],
                    first_ids[first_next],
                    second_ids[second_edge],
                    second_ids[second_next],
                ) {
                    Ok(point) => append(&mut candidates, &point),
                    Err(_) => return None,
                }
            }
        }
    }
    let count = candidates.len();
    let points = convex_hull(projection, &candidates, count);
    let kind = match points.len() {
        0 => OverlapKind::Empty,
        1 => OverlapKind::Point,
        2 => OverlapKind::Segment,
        _ => OverlapKind::Polygon,
    };
    let mut common: Vec<i64> = Vec::new();
    if first_side == second_side {
        for a in 0..3 {
            if (0..3).any(|b| first_ids[a] == second_ids[b]) {
                common.push(first_ids[a]);
            }
        }
    }
    let common_point = |point: &implicit_point::T| {
        common.iter().any(|&slot| implicit_point::equal(point, &implicit_point::explicit(source, slot).unwrap()))
    };
    let topology_contact = match kind {
        OverlapKind::Point => !common.is_empty() && common_point(&points[0]),
        OverlapKind::Segment => common.len() >= 2 && common_point(&points[0]) && common_point(&points[1]),
        OverlapKind::Empty | OverlapKind::Polygon => false,
    };
    if topology_contact {
        return Some(empty());
    }
    let boundary_total = match kind {
        OverlapKind::Empty | OverlapKind::Point => 0,
        OverlapKind::Segment => 1,
        OverlapKind::Polygon => points.len(),
    };
    let mut support_first = vec![0i64; boundary_total];
    let mut support_second = vec![0i64; boundary_total];
    for boundary in 0..boundary_total {
        let first = &points[boundary];
        let second = &points[if kind == OverlapKind::Segment { 1 } else { (boundary + 1) % points.len() }];
        let mut best_first = -1i64;
        let mut best_second = -1i64;
        let mut consider = |ids: &[i64], triangle: &[implicit_point::T], edge: usize| {
            let next = (edge + 1) % 3;
            if orient(&triangle[edge], &triangle[next], first) == Sign::Zero
                && orient(&triangle[edge], &triangle[next], second) == Sign::Zero
            {
                let a = ids[edge].min(ids[next]);
                let b = ids[edge].max(ids[next]);
                if best_first < 0 || a < best_first || (a == best_first && b < best_second) {
                    best_first = a;
                    best_second = b;
                }
            }
        };
        for edge in 0..3 {
            consider(&first_ids, &first_triangle, edge);
            consider(&second_ids, &second_triangle, edge);
        }
        if best_first < 0 {
            return None; // "coplanar overlap boundary has no source-edge support"
        }
        support_first[boundary] = best_first;
        support_second[boundary] = best_second;
    }
    Some(Overlap { kind, points, support_first, support_second })
}

fn make_face_csr(target_side: Side, face_count: i64, constraints: &constraints_mod::T) -> (Vec<i64>, Vec<i64>) {
    let pair_total = constraints_mod::coplanar_pair_count(constraints);
    let n = face_count as usize;
    let mut counts = vec![0i64; n];
    for pair in 0..pair_total {
        if constraints_mod::coplanar_first_side(constraints, pair) == target_side {
            counts[constraints_mod::coplanar_first_triangle(constraints, pair) as usize] += 1;
        }
        if constraints_mod::coplanar_second_side(constraints, pair) == target_side {
            counts[constraints_mod::coplanar_second_triangle(constraints, pair) as usize] += 1;
        }
    }
    let mut offsets = vec![0i64; n + 1];
    for face in 0..n {
        offsets[face + 1] = offsets[face] + counts[face];
    }
    let mut pairs = vec![0i64; offsets[n] as usize];
    let mut cursor = offsets.clone();
    for pair in 0..pair_total {
        if constraints_mod::coplanar_first_side(constraints, pair) == target_side {
            let face = constraints_mod::coplanar_first_triangle(constraints, pair) as usize;
            pairs[cursor[face] as usize] = pair;
            cursor[face] += 1;
        }
        if constraints_mod::coplanar_second_side(constraints, pair) == target_side {
            let face = constraints_mod::coplanar_second_triangle(constraints, pair) as usize;
            pairs[cursor[face] as usize] = pair;
            cursor[face] += 1;
        }
    }
    (offsets, pairs)
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    constraints: &Arc<constraints_mod::T>,
) -> Result<T, error::T> {
    if grain <= 0 {
        return Err(fail("invalid_parameter", "grain must be positive"));
    }
    let cancelled = |_: error::T| fail("cancelled", "Coplanar Boolean arrangement was cancelled");
    cancel::check_opt(cancel_value).map_err(cancelled)?;
    let count = constraints_mod::coplanar_pair_count(constraints);
    // Each pair owns its output; ranges are joined in index order.
    let range_count = if count == 0 { 0 } else { ((count - 1) / grain + 1) as usize };
    let slots: Vec<Mutex<Vec<Option<Overlap>>>> = (0..range_count).map(|_| Mutex::new(Vec::new())).collect();
    {
        let constraints_ref: &constraints_mod::T = constraints;
        support::run_indexed_ranges(grain, count, &|range, first, last| {
            let mut local = Vec::with_capacity((last - first) as usize);
            for pair in first..last {
                cancel::poll(cancel_value, pair, 255)?;
                local.push(build_pair(constraints_ref, pair));
            }
            *slots[range as usize].lock().unwrap() = local;
            Ok(())
        })
        .map_err(cancelled)?;
    }
    let mut overlaps: Vec<Overlap> = Vec::with_capacity(count as usize);
    for slot in &slots {
        for built in slot.lock().unwrap().iter() {
            match built {
                Some(overlap) => overlaps.push(overlap.clone()),
                None => {
                    return Err(fail(
                        "exact_construction_failed",
                        &format!("coplanar pair {} could not be arranged exactly", overlaps.len()),
                    ));
                }
            }
        }
    }
    let (left_offsets, left_pairs) =
        make_face_csr(Side::Left, constraints_mod::left_triangle_count(constraints), constraints);
    let (right_offsets, right_pairs) =
        make_face_csr(Side::Right, constraints_mod::right_triangle_count(constraints), constraints);
    Ok(T { constraints: Arc::clone(constraints), overlaps, left_offsets, left_pairs, right_offsets, right_pairs })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;
    use rdk_core::{geometry, packed, topology};

    type P = (f64, f64, f64);

    fn geom(points: &[P], triangles: &[i64]) -> geometry::T {
        let x: Vec<f64> = points.iter().map(|p| p.0).collect();
        let y: Vec<f64> = points.iter().map(|p| p.1).collect();
        let z: Vec<f64> = points.iter().map(|p| p.2).collect();
        let positions = packed::Float3::View::of_owned_exn(x, y, z).unwrap();
        let offsets: Vec<i64> = (0..=(triangles.len() as i64 / 3)).map(|p| p * 3).collect();
        let kinds = vec![PrimitiveKind::Polygon; offsets.len() - 1];
        let topology = topology::create_owned(points.len() as i64, triangles.to_vec(), offsets, kinds).unwrap();
        geometry::create(positions, topology, None, None, None).unwrap()
    }

    fn triangle(points: &[P]) -> geometry::T {
        geom(points, &[0, 1, 2])
    }

    fn plan(left: &geometry::T, right: &geometry::T) -> Arc<constraints_mod::T> {
        Arc::new(constraints_mod::build(&constraints_mod::params(1), left, right).unwrap())
    }

    fn arrange(left: &geometry::T, right: &geometry::T) -> T {
        let result = build(None, 1, &plan(left, right)).unwrap();
        assert_eq!(pair_count(&result), 1, "expected one coplanar candidate pair");
        result
    }

    fn cmp3(a: &P, b: &P) -> std::cmp::Ordering {
        a.partial_cmp(b).unwrap()
    }

    fn approx(value: &T, pair: i64, point: i64) -> P {
        implicit_point::approximate(View::point(value, pair, point))
    }

    fn expect_points(expected: &[P], actual: &T) {
        let mut got: Vec<P> = (0..point_count(actual, 0)).map(|i| approx(actual, 0, i)).collect();
        got.sort_by(cmp3);
        assert_eq!(got.len(), expected.len(), "overlap point count differs");
        let mut want = expected.to_vec();
        want.sort_by(cmp3);
        for (i, (e, a)) in want.iter().zip(got.iter()).enumerate() {
            let close = |l: f64, r: f64| (l - r).abs() <= 1e-12;
            assert!(close(e.0, a.0) && close(e.1, a.1) && close(e.2, a.2), "point {i}: expected {e:?}, got {a:?}");
        }
    }

    fn left_triangle() -> geometry::T {
        triangle(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)])
    }
    fn downward_triangle() -> geometry::T {
        triangle(&[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.)])
    }

    #[test]
    fn six_edge_overlap() {
        let result = arrange(&left_triangle(), &downward_triangle());
        assert!(kind(&result, 0) == OverlapKind::Polygon, "six-edge overlap is not a polygon");
        assert_eq!(boundary_count(&result, 0), 6, "six-edge boundary was incomplete");
        expect_points(
            &[(0.75, 1.5, 0.), (1.5, 0., 0.), (1.5, 3., 0.), (2.5, 0., 0.), (2.5, 3., 0.), (3.25, 1.5, 0.)],
            &result,
        );
        let mut area = 0.;
        for edge in 0..boundary_count(&result, 0) {
            let first = boundary_first(&result, 0, edge);
            let second = boundary_second(&result, 0, edge);
            let (ax, ay, _) = approx(&result, 0, first);
            let (bx, by, _) = approx(&result, 0, second);
            area += ax * by - ay * bx;
        }
        assert!(area > 0., "coplanar polygon boundary is not projected CCW");
    }

    #[test]
    fn containment_and_reversed_identity() {
        let inside = triangle(&[(1., 1., 0.), (2., 1., 0.), (1., 2., 0.)]);
        let contained = arrange(&left_triangle(), &inside);
        assert!(kind(&contained, 0) == OverlapKind::Polygon && point_count(&contained, 0) == 3);
        expect_points(&[(1., 1., 0.), (2., 1., 0.), (1., 2., 0.)], &contained);
        let reversed = triangle(&[(2., 4., 0.), (4., 0., 0.), (0., 0., 0.)]);
        let identical = arrange(&left_triangle(), &reversed);
        assert!(kind(&identical, 0) == OverlapKind::Polygon && point_count(&identical, 0) == 3);
        expect_points(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)], &identical);
    }

    #[test]
    fn lower_dimensional_contacts() {
        let left = triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]);
        let edge = triangle(&[(0., 0., 0.), (2., 0., 0.), (1., -1., 0.)]);
        let segment = arrange(&left, &edge);
        assert!(
            kind(&segment, 0) == OverlapKind::Segment && point_count(&segment, 0) == 2 && boundary_count(&segment, 0) == 1
        );
        expect_points(&[(0., 0., 0.), (2., 0., 0.)], &segment);
        let tip = triangle(&[(2., 0., 0.), (3., -1., 0.), (3., 1., 0.)]);
        let point = arrange(&left, &tip);
        assert!(kind(&point, 0) == OverlapKind::Point && point_count(&point, 0) == 1 && boundary_count(&point, 0) == 0);
        expect_points(&[(2., 0., 0.)], &point);
    }

    #[test]
    fn invalid_grain_and_cancellation() {
        let constraints = plan(&left_triangle(), &downward_triangle());
        match build(None, 0, &constraints) {
            Err(e) => assert_eq!(error::code(&e), "invalid_parameter"),
            Ok(_) => panic!("zero coplanar grain was accepted"),
        }
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), 1, &constraints) {
            Err(e) => assert_eq!(error::code(&e), "cancelled"),
            Ok(_) => panic!("cancelled coplanar arrangement completed"),
        }
    }

    #[test]
    fn one_vs_many_ranges_equal() {
        // `repeated` fixture of the OCaml file, shrunk: independent coplanar pairs.
        let mut points = Vec::new();
        let mut tris = Vec::new();
        for face in 0..6i64 {
            let o = (face * 10) as f64;
            for p in [(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)] {
                points.push((p.0 + o, p.1, p.2));
                tris.push(points.len() as i64 - 1);
            }
        }
        let left = geom(&points, &tris);
        let right = geom(&points, &tris);
        let c = plan(&left, &right);
        let one = build(None, 100, &c).unwrap();
        let many = build(None, 1, &c).unwrap();
        assert_eq!(pair_count(&one), pair_count(&many));
        for pair in 0..pair_count(&one) {
            assert!(kind(&one, pair) == kind(&many, pair));
            for p in 0..point_count(&one, pair) {
                assert_eq!(approx(&one, pair, p), approx(&many, pair, p));
            }
        }
    }
}
