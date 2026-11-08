//! Checked packed curve topology operations.
use std::sync::Arc;

use rdk_core::{cancel, error, geometry, group};
use rdk_core::error::Code;

use crate::curve_join;

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub primitives: Option<group::T>,
    pub picked_ends: Option<Vec<curve_join::CurveJoinPick>>,
    pub orient_closest: Option<bool>,
    pub connect_closest_ends: Option<bool>,
    pub only_connected: Option<bool>,
    pub group_size: Option<i64>,
    pub keep_originals: Option<bool>,
    pub tolerance: Option<f64>,
    pub wrap: Option<bool>,
}

pub fn join_curves(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    curve_join::join(
        &curve_join::Params {
            cancel: params.cancel.clone(),
            grain: params.grain,
            primitives: params.primitives.clone(),
            picked_ends: params.picked_ends.clone(),
            orient_closest: params.orient_closest.unwrap_or(true),
            connect_closest_ends: params.connect_closest_ends.unwrap_or(false),
            only_connected: params.only_connected.unwrap_or(false),
            group_size: params.group_size,
            keep_originals: params.keep_originals.unwrap_or(false),
            tolerance: params.tolerance.unwrap_or(0.0),
            wrap: params.wrap.unwrap_or(false),
        },
        value,
    )
    .map_err(|failure| error::guard(failure, "join_curves", Code::InvalidGeometry))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::{packed, topology};

    fn two_curves() -> geometry::T {
        let positions = packed::Float3::of_owned(
            vec![0.0, 1.0, 2.0, 3.0],
            vec![0.0; 4],
            vec![0.0; 4],
        )
        .unwrap();
        let mut builder = topology::Builder::create(4, None, None).unwrap();
        topology::Builder::add_open_polyline(&mut builder, &[0, 1]).unwrap();
        topology::Builder::add_open_polyline(&mut builder, &[2, 3]).unwrap();
        let topo = topology::Builder::freeze(&mut builder).unwrap();
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    fn empty_params() -> Params {
        Params {
            cancel: None,
            grain: None,
            primitives: None,
            picked_ends: None,
            orient_closest: None,
            connect_closest_ends: None,
            only_connected: None,
            group_size: None,
            keep_originals: None,
            tolerance: None,
            wrap: None,
        }
    }

    #[test]
    fn defaults_join_everything_and_failures_are_recoded() {
        let joined = join_curves(&empty_params(), &two_curves()).unwrap();
        assert_eq!(geometry::primitive_count(&joined), 1);
        let Err(failure) = join_curves(&Params { tolerance: Some(-1.0), ..empty_params() }, &two_curves())
        else {
            panic!("expected failure")
        };
        assert_eq!(error::operation(&failure), "join_curves");
        assert_eq!(error::code(&failure), Code::InvalidGeometry);
        assert_eq!(error::message(&failure), "Curve Join tolerance must be finite and non-negative");
    }
}
