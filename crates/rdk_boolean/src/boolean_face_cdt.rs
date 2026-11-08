//! Port of lib/rdk/boolean/boolean_face_cdt.ml: deterministic exact-predicate constrained
//! Delaunay refinement of one face (adapter over `Planar_cdt`).
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::collections::HashMap;

use rdk_core::{cancel, error};
use rdk_core::error::Code;
use rdk_geom::implicit_point;
use rdk_geom::planar_cdt;
use rdk_geom::predicates::Sign;

use crate::boolean_constraints::{self as constraints_mod, Side};
use crate::boolean_face_arrangement as arrangement_mod;

pub const OPERATION: &str = "boolean_face_cdt";

pub struct T {
    points: Vec<implicit_point::T>,
    global_point_tokens: Vec<i64>,
    source_winding: i64,
    triangle_a: Vec<i64>,
    triangle_b: Vec<i64>,
    triangle_c: Vec<i64>,
}

/// Exclusively owned scratch reused across sequential face refinements.
pub type Workspace = planar_cdt::view::Workspace;

pub fn create_workspace() -> Workspace {
    planar_cdt::view::create_workspace(Some(16), 16).expect("Planar CDT workspace of 16 points and triangles")
}

fn fail(code: Code, message: &str) -> error::T {
    error::new(OPERATION, code, message.to_owned())
}

pub fn point_count(value: &T) -> i64 {
    value.points.len() as i64
}
pub fn triangle_count(value: &T) -> i64 {
    value.triangle_a.len() as i64
}

/// `Invalid_argument` in OCaml for a corner other than 0, 1 or 2: a panic here.
pub fn triangle_point(value: &T, triangle: i64, local: i64) -> i64 {
    match local {
        0 => value.triangle_a[triangle as usize],
        1 => value.triangle_b[triangle as usize],
        2 => value.triangle_c[triangle as usize],
        _ => unreachable!("Boolean face CDT local corner must be 0, 1, or 2"),
    }
}

pub mod view {
    use super::*;

    pub fn point(value: &T, point: i64) -> &implicit_point::T {
        &value.points[point as usize]
    }
    pub fn global_point_token(value: &T, point: i64) -> i64 {
        value.global_point_tokens[point as usize]
    }
    pub fn source_winding(value: &T) -> i64 {
        value.source_winding
    }
}

fn face_triangle_point(constraints: &constraints_mod::T, side: Side, triangle: i64, local: i64) -> i64 {
    constraints_mod::view::triangle_point(constraints, side, triangle, local)
}

fn face_triangle_implicit(constraints: &constraints_mod::T, side: Side, triangle: i64, local: i64) -> implicit_point::T {
    constraints_mod::view::source_point(constraints, face_triangle_point(constraints, side, triangle, local)).clone()
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
type Incircle = fn(&implicit_point::T, &implicit_point::T, &implicit_point::T, &implicit_point::T) -> Sign;

fn orient_of(projection: i64) -> Orient {
    match projection {
        0 => implicit_point::orient2d_xy,
        1 => implicit_point::orient2d_yz,
        _ => implicit_point::orient2d_zx,
    }
}

fn incircle_of(projection: i64) -> Incircle {
    match projection {
        0 => implicit_point::incircle_xy,
        1 => implicit_point::incircle_yz,
        _ => implicit_point::incircle_zx,
    }
}

fn reverse(sign: Sign) -> Sign {
    match sign {
        Sign::Negative => Sign::Positive,
        Sign::Positive => Sign::Negative,
        Sign::Zero => Sign::Zero,
    }
}

/// Orientation of index triples, cached under the sorted triple (sign flips with the permutation
/// parity). The OCaml table is only a cache: results do not depend on its representation.
fn cached_orient(
    cache: &RefCell<HashMap<(i64, i64, i64), Sign>>,
    points: &[implicit_point::T],
    orient_points: Orient,
    a: i64,
    b: i64,
    c: i64,
) -> Sign {
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
    let known = cache.borrow().get(&(a, b, c)).copied();
    let sign = match known {
        Some(sign) => sign,
        None => {
            let sign = orient_points(&points[a as usize], &points[b as usize], &points[c as usize]);
            cache.borrow_mut().insert((a, b, c), sign);
            sign
        }
    };
    if inversions & 1 == 0 { sign } else { reverse(sign) }
}

fn global_point_tokens(
    constraints: &constraints_mod::T,
    arrangement: &arrangement_mod::T,
    arrangement_to_point: &[i64],
    side: Side,
    triangle: i64,
    point_count: usize,
) -> Vec<i64> {
    let mut tokens = vec![-1i64; point_count];
    for local in 0..3 {
        tokens[local as usize] = face_triangle_point(constraints, side, triangle, local);
    }
    let source_count = constraints_mod::view::source_point_count(constraints);
    for arrangement_point in 0..arrangement_to_point.len() {
        let handle = arrangement_mod::view::point_handle(arrangement, arrangement_point as i64);
        if handle >= 0 {
            let point = arrangement_to_point[arrangement_point] as usize;
            if tokens[point] < 0 {
                tokens[point] = source_count + handle;
            }
        }
    }
    tokens
}

// Stdlib.min / Stdlib.max on finite floats, tie to the first argument.
fn fmin(a: f64, b: f64) -> f64 {
    if a <= b { a } else { b }
}
fn fmax(a: f64, b: f64) -> f64 {
    if a >= b { a } else { b }
}

/// Boolean arrangements and the general planar CDT share the same exact PSLG contract. This
/// adapter keeps the representation boundary; recovery is `Planar_cdt`'s.
pub fn build(
    cancel_value: Option<&cancel::T>,
    workspace: Option<&mut Workspace>,
    constraints: &constraints_mod::T,
    arrangement: &arrangement_mod::T,
    side: Side,
    triangle: i64,
) -> Result<T, error::T> {
    cancel::check_opt(cancel_value).map_err(|_| error::cancelled(OPERATION))?;
    let arrangement_count = arrangement_mod::point_count(arrangement) as usize;
    let mut points: Vec<implicit_point::T> = Vec::with_capacity(3 + arrangement_count);
    for local in 0..3 {
        points.push(face_triangle_implicit(constraints, side, triangle, local));
    }
    let mut arrangement_to_point = vec![0i64; arrangement_count];
    for arrangement_point in 0..arrangement_count {
        let point = arrangement_mod::view::point(arrangement, arrangement_point as i64);
        let found = (0..3).find(|&candidate| implicit_point::equal(&points[candidate], point));
        match found {
            Some(candidate) => arrangement_to_point[arrangement_point] = candidate as i64,
            None => {
                arrangement_to_point[arrangement_point] = points.len() as i64;
                points.push(point.clone());
            }
        }
    }
    let projection_axis = projection(&points);
    let orient_points = orient_of(projection_axis);
    let incircle_points = incircle_of(projection_axis);
    let cache: RefCell<HashMap<(i64, i64, i64), Sign>> = RefCell::new(HashMap::new());
    let orient_indices = |a: i64, b: i64, c: i64| cached_orient(&cache, &points, orient_points, a, b, c);
    let source_winding = match orient_indices(0, 1, 2) {
        Sign::Positive => 1,
        Sign::Negative => -1,
        Sign::Zero => return Err(fail(Code::InvalidConstraints, "degenerate source triangle during face CDT")),
    };
    let initial_triangle_points: [i64; 3] = if source_winding > 0 { [0, 1, 2] } else { [0, 2, 1] };
    let insert_points: Vec<i64> = (3..points.len() as i64).collect();
    let source_constraint_count = arrangement_mod::segment_count(arrangement);
    // Face arrangement already canonicalizes every construction and performs its own indexed
    // incidence pass, so no point/segment rescan is needed here.
    let mut constraint_points: Vec<i64> = Vec::with_capacity((source_constraint_count * 2) as usize);
    for segment in 0..source_constraint_count {
        constraint_points.push(arrangement_to_point[arrangement_mod::segment_first(arrangement, segment) as usize]);
        constraint_points.push(arrangement_to_point[arrangement_mod::segment_second(arrangement, segment) as usize]);
    }
    let n = points.len();
    let mut u_min = vec![0.; n];
    let mut u_max = vec![0.; n];
    let mut v_min = vec![0.; n];
    let mut v_max = vec![0.; n];
    for point in 0..n {
        let ((x_min, x_max), (y_min, y_max), (z_min, z_max)) = implicit_point::bounds(&points[point]);
        let (a, b, c, d) = match projection_axis {
            0 => (x_min, x_max, y_min, y_max),
            1 => (y_min, y_max, z_min, z_max),
            _ => (z_min, z_max, x_min, x_max),
        };
        u_min[point] = a;
        u_max[point] = b;
        v_min[point] = c;
        v_max[point] = d;
    }
    let bounds_overlap = |first: i64, second: i64, edge_a: i64, edge_b: i64| {
        let (f, s, a, b) = (first as usize, second as usize, edge_a as usize, edge_b as usize);
        let first_u_min = fmin(u_min[f], u_min[s]);
        let first_u_max = fmax(u_max[f], u_max[s]);
        let first_v_min = fmin(v_min[f], v_min[s]);
        let first_v_max = fmax(v_max[f], v_max[s]);
        let second_u_min = fmin(u_min[a], u_min[b]);
        let second_u_max = fmax(u_max[a], u_max[b]);
        let second_v_min = fmin(v_min[a], v_min[b]);
        let second_v_max = fmax(v_max[a], v_max[b]);
        first_u_max >= second_u_min && second_u_max >= first_u_min && first_v_max >= second_v_min && second_v_max >= first_v_min
    };
    let incircle_indices = |a: i64, b: i64, c: i64, d: i64| {
        incircle_points(&points[a as usize], &points[b as usize], &points[c as usize], &points[d as usize])
    };
    let built = planar_cdt::build(
        cancel_value,
        workspace,
        n as i64,
        &orient_indices,
        &incircle_indices,
        Some(&bounds_overlap),
        &initial_triangle_points,
        Some(&insert_points),
        false,
        &constraint_points,
        None,
        false,
    );
    match built {
        Err(message) => Err(fail(
            Code::InvalidConstraints,
            &format!(
                "{} (side={} source_triangle={} points={} constraints={})",
                message,
                match side {
                    Side::Left => "left",
                    Side::Right => "right",
                },
                triangle,
                n,
                source_constraint_count
            ),
        )),
        Ok(triangulation) => {
            let view = planar_cdt::view::view(&triangulation);
            let count = view.triangle_points.len() / 3;
            let pick = |corner: usize| -> Vec<i64> { (0..count).map(|t| view.triangle_points[t * 3 + corner]).collect() };
            let tokens = global_point_tokens(constraints, arrangement, &arrangement_to_point, side, triangle, n);
            Ok(T {
                points,
                global_point_tokens: tokens,
                source_winding,
                triangle_a: pick(0),
                triangle_b: pick(1),
                triangle_c: pick(2),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boolean_coplanar as coplanar_mod;
    use rdk_core::topology::PrimitiveKind;
    use rdk_core::{geometry, packed, topology};
    use std::sync::Arc;

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

    fn crossing_input() -> (Arc<constraints_mod::T>, arrangement_mod::T) {
        let left = geom(&[(-3., -3., 0.), (3., -3., 0.), (0., 3., 0.)], &[0, 1, 2]);
        let right = geom(
            &[(0., -2., -1.), (0., 2., -1.), (0., 0., 1.), (-2., 0., -1.), (2., 0., -1.), (0., 0., 1.)],
            &[0, 1, 2, 3, 4, 5],
        );
        let constraints = constraints_of(1, &left, &right);
        let arrangement = arrangement_mod::build(None, None, &constraints, Side::Left, 0).unwrap();
        (constraints, arrangement)
    }

    fn coplanar_input() -> (Arc<constraints_mod::T>, arrangement_mod::T) {
        let left = geom(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)], &[0, 1, 2]);
        let right = geom(&[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.)], &[0, 1, 2]);
        let constraints = constraints_of(1, &left, &right);
        let coplanar = coplanar_mod::build(None, 1, &constraints).unwrap();
        let arrangement = arrangement_mod::build(None, Some(&coplanar), &constraints, Side::Left, 0).unwrap();
        (constraints, arrangement)
    }

    fn many_constraint_input(count: usize) -> (Arc<constraints_mod::T>, arrangement_mod::T) {
        let scale = count as f64;
        let left = geom(&[(-scale, -scale, 0.), (2. * scale, -scale, 0.), (scale / 2., 2. * scale, 0.)], &[0, 1, 2]);
        let mut points = vec![(0., 0., 0.); count * 3];
        let triangles: Vec<i64> = (0..(count * 3) as i64).collect();
        for segment in 0..count {
            let point = segment * 3;
            let x = segment as f64 + 0.25;
            points[point] = (x, -1., -1.);
            points[point + 1] = (x, 1., -1.);
            points[point + 2] = (x, 0., 1.);
        }
        let constraints = constraints_of(7, &left, &geom(&points, &triangles));
        let arrangement = arrangement_mod::build(None, None, &constraints, Side::Left, 0).unwrap();
        (constraints, arrangement)
    }

    fn edge_present(value: &T, first: i64, second: i64) -> bool {
        (0..triangle_count(value)).any(|t| {
            let (a, b, c) = (triangle_point(value, t, 0), triangle_point(value, t, 1), triangle_point(value, t, 2));
            (a == first && b == second)
                || (a == second && b == first)
                || (b == first && c == second)
                || (b == second && c == first)
                || (c == first && a == second)
                || (c == second && a == first)
        })
    }

    /// No arrangement point coincides with a corner, so arrangement point `i` is CDT point `3 + i`.
    fn assert_constraints_recovered(constraints: &constraints_mod::T, arrangement: &arrangement_mod::T) {
        let triangulation = build(None, None, constraints, arrangement, Side::Left, 0).unwrap();
        assert_eq!(point_count(&triangulation), 3 + arrangement_mod::point_count(arrangement));
        assert!(triangle_count(&triangulation) > 0);
        assert_eq!(view::source_winding(&triangulation), 1);
        for segment in 0..arrangement_mod::segment_count(arrangement) {
            let first = 3 + arrangement_mod::segment_first(arrangement, segment);
            let second = 3 + arrangement_mod::segment_second(arrangement, segment);
            assert!(edge_present(&triangulation, first, second), "constraint {segment} missing");
        }
    }

    #[test]
    fn cancellation() {
        let (constraints, arrangement) = crossing_input();
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), None, &constraints, &arrangement, Side::Left, 0) {
            Err(e) => assert_eq!(error::code(&e), Code::Cancelled),
            Ok(_) => panic!("cancelled face CDT completed"),
        }
    }

    #[test]
    fn crossing_constraints_recovered() {
        let (constraints, arrangement) = crossing_input();
        assert_constraints_recovered(&constraints, &arrangement);
    }

    #[test]
    fn coplanar_constraints_recovered_with_workspace() {
        let (constraints, arrangement) = coplanar_input();
        let mut workspace = create_workspace();
        let triangulation =
            build(None, Some(&mut workspace), &constraints, &arrangement, Side::Left, 0).unwrap();
        for segment in 0..arrangement_mod::segment_count(&arrangement) {
            assert!(edge_present(
                &triangulation,
                3 + arrangement_mod::segment_first(&arrangement, segment),
                3 + arrangement_mod::segment_second(&arrangement, segment)
            ));
        }
    }

    #[test]
    fn many_constraints_recovered() {
        let (constraints, arrangement) = many_constraint_input(24);
        assert_constraints_recovered(&constraints, &arrangement);
    }
}
