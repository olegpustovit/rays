//! Port of lib/rdk/boolean/boolean_radial.ml: exact radial ordering of surface charts around
//! complex edges. `Invalid_argument` becomes an `invalid_complex` error.
use std::cell::Cell;
use std::cmp::Ordering;
use std::sync::Arc;

use rdk_core::{cancel, error};
use rdk_core::error::Code;
use rdk_geom::implicit_point;
use rdk_geom::predicates::Sign;

use crate::boolean_complex as complex;

const OPERATION: &str = "boolean_radial";

pub struct T {
    complex: Arc<complex::T>,
    offsets: Vec<i64>,
    facets: Vec<i64>,
    locals: Vec<u8>,
}

fn fail(code: Code, message: &str) -> error::T {
    error::new(OPERATION, code, message.to_owned())
}

pub fn incident_range(value: &T, edge: i64) -> (i64, i64) {
    (value.offsets[edge as usize], value.offsets[edge as usize + 1])
}

pub fn incident_facet(value: &T, incident: i64) -> i64 {
    value.facets[incident as usize]
}

pub fn incident_local(value: &T, incident: i64) -> i64 {
    i64::from(value.locals[incident as usize])
}

#[allow(non_snake_case)]
pub mod view {
    use super::*;

    pub fn complex(value: &T) -> &Arc<complex::T> {
        &value.complex
    }
}

fn opposite_vertex(c: &complex::T, facet: i64, local: i64) -> i64 {
    complex::facet_vertex(c, facet, (local + 2) % 3)
}

fn cancelled(failure: error::T) -> error::T {
    if error::is_cancelled(&failure) {
        error::cancelled(OPERATION)
    } else {
        failure
    }
}

pub fn build(cancel_value: Option<&cancel::T>, c: &Arc<complex::T>) -> Result<T, error::T> {
    cancel::check_opt(cancel_value).map_err(cancelled)?;
    let edges = complex::edge_count(c);
    let mut offsets = vec![0_i64; edges as usize + 1];
    for edge in 0..edges {
        let (first, last) = complex::edge_incident_range(c, edge);
        offsets[edge as usize + 1] = offsets[edge as usize] + (last - first);
    }
    let total = offsets[edges as usize] as usize;
    let mut facets = vec![0_i64; total];
    let mut locals = vec![0_u8; total];
    for edge in 0..edges {
        cancel::poll(cancel_value, edge, 255).map_err(cancelled)?;
        let (first, last) = complex::edge_incident_range(c, edge);
        let output = offsets[edge as usize];
        let count = last - first;
        // Cyclic order is unique only from three charts onward. One/two-chart edges retain the
        // complex's stable facet order without exact angular arithmetic.
        let order: Vec<i64> = if count <= 2 {
            (0..count).collect()
        } else {
            radial_order(c, edge, first, count)?
        };
        for (index, chosen) in order.iter().enumerate() {
            let incident = first + chosen;
            let destination = output as usize + index;
            facets[destination] = complex::edge_incident_facet(c, incident);
            locals[destination] = complex::edge_incident_local(c, incident) as u8;
        }
    }
    Ok(T { complex: Arc::clone(c), offsets, facets, locals })
}

fn radial_order(c: &complex::T, edge: i64, first: i64, count: i64) -> Result<Vec<i64>, error::T> {
    let edge_start = complex::view::vertex(c, complex::edge_first(c, edge));
    let edge_end = complex::view::vertex(c, complex::edge_second(c, edge));
    let reference_facet = complex::edge_incident_facet(c, first);
    let reference_local = complex::edge_incident_local(c, first);
    let reference = complex::view::vertex(c, opposite_vertex(c, reference_facet, reference_local));
    let points: Vec<_> = (0..count)
        .map(|index| {
            let facet = complex::edge_incident_facet(c, first + index);
            let local = complex::edge_incident_local(c, first + index);
            complex::view::vertex(c, opposite_vertex(c, facet, local))
        })
        .collect();
    let same_source_face = |left: i64, right: i64| -> bool {
        let (left_member, _) = complex::facet_member_range(c, left);
        let (right_member, _) = complex::facet_member_range(c, right);
        complex::member_side(c, left_member) == complex::member_side(c, right_member)
            && complex::member_face(c, left_member) == complex::member_face(c, right_member)
    };
    let invalid = Cell::new(false);
    let halves: Vec<i64> = (0..count as usize)
        .map(|index| {
            let facet = complex::edge_incident_facet(c, first + index as i64);
            if facet == reference_facet {
                0
            } else if same_source_face(reference_facet, facet) {
                // Two distinct refined triangles of one nondegenerate source face incident to the
                // same edge lie in opposite planar half-planes.
                2
            } else {
                match implicit_point::orient3d(&edge_start, &edge_end, &reference, &points[index]) {
                    Sign::Negative => 1,
                    Sign::Positive => 3,
                    Sign::Zero => match implicit_point::radial_dot(
                        &edge_start,
                        &edge_end,
                        &reference,
                        &points[index],
                    ) {
                        Sign::Positive => 0,
                        Sign::Negative => 2,
                        Sign::Zero => {
                            invalid.set(true);
                            0
                        }
                    },
                }
            }
        })
        .collect();
    if invalid.get() {
        return Err(fail(Code::InvalidComplex, "radial chart opposite point lies on its edge"));
    }
    let mut order: Vec<i64> = (0..count).collect();
    order.sort_by(|&left, &right| {
        let by_half = halves[left as usize].cmp(&halves[right as usize]);
        if by_half != Ordering::Equal {
            return by_half;
        }
        match implicit_point::orient3d(
            &edge_start,
            &edge_end,
            &points[left as usize],
            &points[right as usize],
        ) {
            Sign::Negative => Ordering::Less,
            Sign::Positive => Ordering::Greater,
            Sign::Zero => {
                let left_facet = complex::edge_incident_facet(c, first + left);
                let right_facet = complex::edge_incident_facet(c, first + right);
                left_facet.cmp(&right_facet).then_with(|| {
                    complex::edge_incident_local(c, first + left)
                        .cmp(&complex::edge_incident_local(c, first + right))
                })
            }
        }
    });
    Ok(order)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{boolean_coplanar, boolean_constraints, boolean_refinement};
    use rdk_core::{geometry, packed, topology};

    type P = (f64, f64, f64);

    fn close(left: f64, right: f64) -> bool {
        (left - right).abs() <= 1e-11
    }

    fn geom(points: &[P], triangles: &[i64]) -> geometry::T {
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let offsets: Vec<i64> = (0..=(triangles.len() as i64 / 3)).map(|p| p * 3).collect();
        let topo = topology::polygons_owned(points.len() as i64, triangles.to_vec(), offsets).unwrap();
        geometry::create(packed::Float3::View::of_owned_exn(x, y, z).unwrap(), topo, None, None, None)
            .unwrap()
    }

    fn pipeline(left: &geometry::T, right: &geometry::T) -> (Arc<complex::T>, T) {
        let constraints =
            Arc::new(boolean_constraints::build(&boolean_constraints::params(1), left, right).unwrap());
        let coplanar = boolean_coplanar::build(None, 1, &constraints).unwrap();
        let refinement = boolean_refinement::build(None, Some(&coplanar), 1, &constraints).unwrap();
        let value = Arc::new(complex::build(None, &constraints, &refinement).unwrap());
        let radial = build(None, &value).unwrap();
        (value, radial)
    }

    #[test]
    fn exact_radial_dot() {
        let source = implicit_point::source(
            vec![0., 0., 1., 2., -1., 0., 0.],
            vec![0., 0., 0., 0., 0., 1., 0.],
            vec![0., 1., 0., 0.5, 0.2, 0.4, 0.5],
        )
        .unwrap();
        let points: Vec<_> = (0..7).map(|p| implicit_point::explicit(&source, p).unwrap()).collect();
        let dot = |a: usize, b: usize, c: usize, d: usize| {
            implicit_point::radial_dot(&points[a], &points[b], &points[c], &points[d])
        };
        assert!(dot(0, 1, 2, 3) == Sign::Positive, "same projected ray has non-positive dot");
        assert!(dot(0, 1, 2, 4) == Sign::Negative, "opposite projected ray has non-negative dot");
        assert!(dot(0, 1, 2, 5) == Sign::Zero, "orthogonal projected rays have non-zero dot");
        assert!(dot(0, 1, 2, 6) == Sign::Zero, "point on the edge has non-zero radial projection");
    }

    fn seam_bundle() -> (Arc<complex::T>, T, i64) {
        let left = geom(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)], &[0, 1, 2]);
        let right = geom(&[(0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)], &[0, 1, 2]);
        let (value, radial) = pipeline(&left, &right);
        let mut seam = -1;
        for edge in 0..complex::edge_count(&value) {
            let (ax, ay, az) = complex::approximate_vertex(&value, complex::edge_first(&value, edge));
            let (bx, by, bz) = complex::approximate_vertex(&value, complex::edge_second(&value, edge));
            if (close(ax, 0.5) && close(ay, 0.5) && close(az, 0.) && close(bx, 0.5) && close(by, 1.5) && close(bz, 0.))
                || (close(bx, 0.5) && close(by, 0.5) && close(bz, 0.) && close(ax, 0.5) && close(ay, 1.5) && close(az, 0.))
            {
                seam = edge;
            }
        }
        assert!(seam >= 0, "radial seam edge is missing");
        (value, radial, seam)
    }

    #[test]
    fn transverse_cyclic_order() {
        let (value, radial, seam) = seam_bundle();
        let (first, last) = incident_range(&radial, seam);
        assert!(last - first == 4, "radial seam does not contain four charts");
        let sides: Vec<_> = (0..4)
            .map(|index| {
                let facet = incident_facet(&radial, first + index);
                let (member, _) = complex::facet_member_range(&value, facet);
                complex::member_side(&value, member)
            })
            .collect();
        for index in 0..4 {
            assert!(
                sides[index] != sides[(index + 1) % 4],
                "two charts from one surface are adjacent in the radial cycle"
            );
        }
    }

    #[test]
    fn cancellation() {
        let (value, _, _) = seam_bundle();
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), &value) {
            Err(failure) => assert_eq!(error::code(&failure), Code::Cancelled),
            Ok(_) => panic!("cancelled radial ordering completed"),
        }
    }
}
