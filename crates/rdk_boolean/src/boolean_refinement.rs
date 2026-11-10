//! parallel deterministic per-face Boolean
//! constraint refinement.


use std::sync::Arc;

use rdk_core::{cancel, error, support};
use rdk_core::error::Code;

use crate::boolean_constraints::{self as constraints_mod, Side};
use crate::boolean_coplanar as coplanar_mod;
use crate::boolean_face_arrangement as arrangement_mod;
use crate::boolean_face_cdt as cdt_mod;

pub const OPERATION: &str = "boolean_refinement";

pub struct T {
    constraints: Arc<constraints_mod::T>,
    left: Vec<Option<cdt_mod::T>>,
    right: Vec<Option<cdt_mod::T>>,
}

fn fail(code: Code, message: &str) -> error::T {
    error::new(OPERATION, code, message.to_owned())
}

pub fn left_face(value: &T, face: i64) -> Option<&cdt_mod::T> {
    value.left[face as usize].as_ref()
}
pub fn right_face(value: &T, face: i64) -> Option<&cdt_mod::T> {
    value.right[face as usize].as_ref()
}

pub mod view {
    use super::*;

    pub fn constraints(value: &T) -> &Arc<constraints_mod::T> {
        &value.constraints
    }
}

type FaceResult = Result<cdt_mod::T, error::T>;

/// Process faces in index order; the first failing face wins.
fn build_side(
    cancel_value: Option<&cancel::T>,
    coplanar: Option<&coplanar_mod::T>,
    grain: i64,
    constraints: &Arc<constraints_mod::T>,
    side: Side,
    face_count: i64,
) -> Result<Vec<Option<cdt_mod::T>>, error::T> {
    // Enough sequential faces per exclusively owned CDT workspace to amortize its planes while
    // keeping many coarse tasks on small, high-intersection inputs.
    let range_size = grain.max(8);
    let mut faces: Vec<Option<FaceResult>> = std::iter::repeat_with(|| None).take(face_count.max(0) as usize).collect();
    let windows = support::Windows::even(&mut faces, range_size);
    let constraints_ref: &constraints_mod::T = constraints;
    support::run_indexed_ranges(range_size, face_count, &|range, first_face, last_face| {
        let mut workspace = cdt_mod::create_workspace();
        for (face, slot) in (first_face..last_face).zip(windows.take(range)) {
            cancel::poll(cancel_value, face, 255)
                .map_err(|_| error::cancelled(OPERATION))?;
            let (first, last) = match side {
                Side::Left => constraints_mod::left_constraint_range(constraints_ref, face),
                Side::Right => constraints_mod::right_constraint_range(constraints_ref, face),
            };
            let (coplanar_first, coplanar_last) = match coplanar {
                None => (0, 0),
                Some(value) => match side {
                    Side::Left => coplanar_mod::view::left_pair_range(value, face),
                    Side::Right => coplanar_mod::view::right_pair_range(value, face),
                },
            };
            if first < last || coplanar_first < coplanar_last {
                let result = arrangement_mod::build(cancel_value, coplanar, constraints, side, face).and_then(
                    |arrangement| {
                        cdt_mod::build(cancel_value, Some(&mut workspace), constraints_ref, &arrangement, side, face)
                    },
                );
                *slot = Some(result);
            }
        }
        Ok(())
    })?;
    let mut output: Vec<Option<cdt_mod::T>> = Vec::with_capacity(faces.len());
    for item in faces {
        match item {
            None => output.push(None),
            Some(Ok(face)) => output.push(Some(face)),
            Some(Err(failure)) => return Err(failure),
        }
    }
    Ok(output)
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    coplanar: Option<&coplanar_mod::T>,
    grain: i64,
    constraints: &Arc<constraints_mod::T>,
) -> Result<T, error::T> {
    support::check_positive(grain).map_err(|_| fail(Code::InvalidParameter, "grain must be positive"))?;
    let left_count = constraints_mod::left_triangle_count(constraints);
    let right_count = constraints_mod::right_triangle_count(constraints);
    let left = build_side(cancel_value, coplanar, grain, constraints, Side::Left, left_count)?;
    let right = build_side(cancel_value, coplanar, grain, constraints, Side::Right, right_count)?;
    Ok(T { constraints: Arc::clone(constraints), left, right })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;
    use rdk_core::{geometry, packed, topology};

    fn geom(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> geometry::T {
        let point_count = x.len() as i64;
        let vertices: Vec<i64> = (0..point_count).collect();
        let offsets: Vec<i64> = (0..=(point_count / 3)).map(|p| p * 3).collect();
        let kinds = vec![PrimitiveKind::Polygon; offsets.len() - 1];
        let positions = packed::float3::view::of_owned_exn(x, y, z).unwrap();
        let topology = topology::create_owned(point_count, topology::narrow("test", &vertices).unwrap(), topology::narrow("test", &offsets).unwrap(), kinds).unwrap();
        geometry::create(positions, topology, None, None, None).unwrap()
    }

    fn crossing_geometry(pair_count: usize, right: bool) -> geometry::T {
        let n = pair_count * 3;
        let (mut x, mut y, mut z) = (vec![0.; n], vec![0.; n], vec![0.; n]);
        for pair in 0..pair_count {
            let point = pair * 3;
            let offset = pair as f64 * 4.;
            if right {
                (x[point], y[point], z[point]) = (offset + 0.5, -0.5, -1.);
                (x[point + 1], y[point + 1], z[point + 1]) = (offset + 0.5, 1.5, 1.);
                (x[point + 2], y[point + 2], z[point + 2]) = (offset + 0.5, 1.5, -1.);
            } else {
                (x[point], y[point], z[point]) = (offset, 0., 0.);
                (x[point + 1], y[point + 1], z[point + 1]) = (offset + 2., 0., 0.);
                (x[point + 2], y[point + 2], z[point + 2]) = (offset, 2., 0.);
            }
        }
        geom(x, y, z)
    }

    fn coplanar_geometry(pair_count: usize, right: bool) -> geometry::T {
        let n = pair_count * 3;
        let (mut x, mut y, z) = (vec![0.; n], vec![0.; n], vec![0.; n]);
        for pair in 0..pair_count {
            let point = pair * 3;
            let offset = pair as f64 * 8.;
            if right {
                (x[point], y[point]) = (offset, 3.);
                (x[point + 1], y[point + 1]) = (offset + 4., 3.);
                (x[point + 2], y[point + 2]) = (offset + 2., -1.);
            } else {
                (x[point], y[point]) = (offset, 0.);
                (x[point + 1], y[point + 1]) = (offset + 4., 0.);
                (x[point + 2], y[point + 2]) = (offset + 2., 4.);
            }
        }
        geom(x, y, z)
    }

    fn constraints(pair_count: usize) -> Arc<constraints_mod::T> {
        Arc::new(
            constraints_mod::build(
                &constraints_mod::params(7),
                &crossing_geometry(pair_count, false),
                &crossing_geometry(pair_count, true),
            )
            .unwrap(),
        )
    }

    fn signature(face: Option<&cdt_mod::T>) -> Vec<(i64, i64, i64)> {
        match face {
            None => Vec::new(),
            Some(face) => (0..cdt_mod::triangle_count(face))
                .map(|t| (cdt_mod::triangle_point(face, t, 0), cdt_mod::triangle_point(face, t, 1), cdt_mod::triangle_point(face, t, 2)))
                .collect(),
        }
    }

    #[test]
    fn validation() {
        let input = constraints(1);
        match build(None, None, 0, &input) {
            Err(e) => assert_eq!(error::code(&e), Code::InvalidParameter),
            Ok(_) => panic!("zero refinement grain was accepted"),
        }
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), None, 1, &input) {
            Err(e) => assert_eq!(error::code(&e), Code::Cancelled),
            Ok(_) => panic!("cancelled refinement completed"),
        }
    }

    #[test]
    fn grain_does_not_change_faces() {
        let input = constraints(20);
        let one = build(None, None, 100, &input).unwrap();
        let many = build(None, None, 1, &input).unwrap();
        for face in 0..constraints_mod::left_triangle_count(&input) {
            assert_eq!(signature(left_face(&one, face)), signature(left_face(&many, face)));
            assert!(left_face(&one, face).is_some());
        }
        for face in 0..constraints_mod::right_triangle_count(&input) {
            assert_eq!(signature(right_face(&one, face)), signature(right_face(&many, face)));
        }
    }

    #[test]
    fn coplanar_pairs_refine_both_sides() {
        let constraints = Arc::new(
            constraints_mod::build(
                &constraints_mod::params(7),
                &coplanar_geometry(4, false),
                &coplanar_geometry(4, true),
            )
            .unwrap(),
        );
        let coplanar = coplanar_mod::build(None, 7, &constraints).unwrap();
        let result = build(None, Some(&coplanar), 1, &constraints).unwrap();
        for face in 0..4 {
            assert!(left_face(&result, face).is_some() && right_face(&result, face).is_some());
        }
    }
}
