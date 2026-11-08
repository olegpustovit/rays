//! Port of lib/rdk/boolean/boolean_weiler.ml: Weiler half-facet adjacency and local shell graph.
use std::sync::Arc;

use rdk_core::{cancel, error};
use rdk_core::error::Code;

use crate::boolean_complex as complex;
use crate::boolean_constraints::Side as OperandSide;
use crate::boolean_radial as radial;

const OPERATION: &str = "boolean_weiler";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    Negative,
    Positive,
}

pub struct T {
    complex: Arc<complex::T>,
    shells: Vec<i64>,
    shell_count: i64,
    left_winding: Vec<i64>,
    right_winding: Vec<i64>,
}

fn fail(code: Code, message: &str) -> error::T {
    error::new(OPERATION, code, message.to_owned())
}

pub fn half_facet(facet: i64, side: Side) -> i64 {
    facet * 2
        + match side {
            Side::Negative => 0,
            Side::Positive => 1,
        }
}

pub fn shell_count(value: &T) -> i64 {
    value.shell_count
}

pub fn half_facet_shell(value: &T, half_facet: i64) -> i64 {
    value.shells[half_facet as usize]
}

pub fn facet_left_winding(value: &T, facet: i64) -> i64 {
    value.left_winding[facet as usize]
}

pub fn facet_right_winding(value: &T, facet: i64) -> i64 {
    value.right_winding[facet as usize]
}

#[allow(non_snake_case)]
pub mod View {
    use super::*;

    pub fn complex(value: &T) -> &Arc<complex::T> {
        &value.complex
    }
}

fn find(parent: &mut [i64], value: i64) -> i64 {
    let mut root = value;
    while parent[root as usize] != root {
        root = parent[root as usize];
    }
    let mut cursor = value;
    while parent[cursor as usize] != cursor {
        let next = parent[cursor as usize];
        parent[cursor as usize] = root;
        cursor = next;
    }
    root
}

fn unite(parent: &mut [i64], left: i64, right: i64) {
    let left = find(parent, left);
    let right = find(parent, right);
    if left != right {
        if left < right {
            parent[right as usize] = left;
        } else {
            parent[left as usize] = right;
        }
    }
}

fn cancelled(failure: error::T) -> error::T {
    if error::is_cancelled(&failure) {
        error::cancelled(OPERATION)
    } else {
        failure
    }
}

fn invalid(message: &str) -> error::T {
    fail(Code::InvalidComplex, message)
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    c: &Arc<complex::T>,
    radial_value: &radial::T,
) -> Result<T, error::T> {
    cancel::check_opt(cancel_value).map_err(cancelled)?;
    if !Arc::ptr_eq(radial::View::complex(radial_value), c) {
        return Err(invalid("radial order belongs to a different Boolean complex"));
    }
    let facets = complex::facet_count(c);
    let half_facets = facets * 2;
    let mut neighbors = vec![-1_i64; (half_facets * 3) as usize];
    let mut parent: Vec<i64> = (0..half_facets).collect();
    let mut set_neighbor = |half: i64, local: i64, neighbor: i64| -> Result<(), error::T> {
        let slot = (half * 3 + local) as usize;
        if neighbors[slot] >= 0 && neighbors[slot] != neighbor {
            return Err(invalid("radial cycle assigned two neighbors to one half-edge"));
        }
        neighbors[slot] = neighbor;
        Ok(())
    };
    for edge in 0..complex::edge_count(c) {
        cancel::poll(cancel_value, edge, 255).map_err(cancelled)?;
        let (first, last) = radial::incident_range(radial_value, edge);
        let count = last - first;
        if count == 0 {
            return Err(invalid("complex edge has no radial chart"));
        }
        let edge_first = complex::edge_first(c, edge);
        let edge_second = complex::edge_second(c, edge);
        let aligned = |facet: i64, local: i64| -> Result<bool, error::T> {
            let a = complex::facet_vertex(c, facet, local);
            let b = complex::facet_vertex(c, facet, (local + 1) % 3);
            if a == edge_first && b == edge_second {
                Ok(true)
            } else if a == edge_second && b == edge_first {
                Ok(false)
            } else {
                Err(invalid("radial chart local edge does not match complex edge"))
            }
        };
        for index in 0..count {
            let next_index = (index + 1) % count;
            let current_slot = first + index;
            let next_slot = first + next_index;
            let current_facet = radial::incident_facet(radial_value, current_slot);
            let current_local = radial::incident_local(radial_value, current_slot);
            let next_facet = radial::incident_facet(radial_value, next_slot);
            let next_local = radial::incident_local(radial_value, next_slot);
            let current_side =
                if aligned(current_facet, current_local)? { Side::Positive } else { Side::Negative };
            let next_side =
                if aligned(next_facet, next_local)? { Side::Negative } else { Side::Positive };
            let current_half = half_facet(current_facet, current_side);
            let next_half = half_facet(next_facet, next_side);
            set_neighbor(current_half, current_local, next_half)?;
            set_neighbor(next_half, next_local, current_half)?;
            unite(&mut parent, current_half, next_half);
        }
    }
    if neighbors.iter().any(|&slot| slot < 0) {
        return Err(invalid("Weiler half-edge has no radial neighbor"));
    }
    for half in 0..half_facets {
        let root = find(&mut parent, half);
        parent[half as usize] = root;
    }
    let mut shell_of_root = vec![-1_i64; half_facets as usize];
    let mut shell_count = 0_i64;
    for half in 0..half_facets {
        if parent[half as usize] == half {
            shell_of_root[half as usize] = shell_count;
            shell_count += 1;
        }
    }
    let shells: Vec<i64> =
        (0..half_facets).map(|half| shell_of_root[parent[half as usize] as usize]).collect();
    let mut left_winding = vec![0_i64; facets as usize];
    let mut right_winding = vec![0_i64; facets as usize];
    for facet in 0..facets {
        let (first, last) = complex::facet_member_range(c, facet);
        for member in first..last {
            let winding = complex::member_winding(c, member);
            match complex::member_side(c, member) {
                OperandSide::Left => left_winding[facet as usize] += winding,
                OperandSide::Right => right_winding[facet as usize] += winding,
            }
        }
    }
    Ok(T { complex: Arc::clone(c), shells, shell_count, left_winding, right_winding })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{boolean_constraints, boolean_coplanar, boolean_refinement};
    use rdk_core::{geometry, packed, topology};

    fn triangle(points: &[(f64, f64, f64)]) -> geometry::T {
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let topo = topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        geometry::create(packed::Float3::View::of_owned_exn(x, y, z).unwrap(), topo, None, None, None)
            .unwrap()
    }

    #[test]
    fn cancellation() {
        let left = triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]);
        let right = triangle(&[(10., 0., 0.), (12., 0., 0.), (10., 2., 0.)]);
        let constraints =
            Arc::new(boolean_constraints::build(&boolean_constraints::params(1), &left, &right).unwrap());
        let coplanar = boolean_coplanar::build(None, 1, &constraints).unwrap();
        let refinement = boolean_refinement::build(None, Some(&coplanar), 1, &constraints).unwrap();
        let value = Arc::new(complex::build(None, &constraints, &refinement).unwrap());
        let order = radial::build(None, &value).unwrap();
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), &value, &order) {
            Err(failure) => assert_eq!(error::kind(&failure), Code::Cancelled),
            Ok(_) => panic!("cancelled Weiler construction completed"),
        }
        let built = build(None, &value, &order).unwrap();
        assert!(shell_count(&built) >= 2);
    }
}
