//! Packed point clouds and straight/open/closed polylines.
use rdk_core::{error, geometry, packed, topology};

use crate::attribute_ops::{make_error, recode};

pub struct Params {
    pub closed: bool,
}

pub fn points(values: &[(f64, f64, f64)]) -> Result<geometry::T, error::T> {
    let count = values.len();
    let mut builder = packed::Float3::Builder::create(count as i64)?;
    for (index, &(x, y, z)) in values.iter().enumerate() {
        packed::Float3::Builder::set(&mut builder, index as i64, x, y, z)?;
    }
    let positions = packed::Float3::Builder::freeze(&mut builder)?;
    geometry::create(positions, topology::empty(count as i64)?, None, None, None)
}

pub fn polyline(params: &Params, values: &[(f64, f64, f64)]) -> Result<geometry::T, error::T> {
    polyline_inner(params, values)
        .map_err(|failure| recode(&failure, "polyline", "invalid_parameter", "geometry operation was cancelled"))
}

fn polyline_inner(params: &Params, values: &[(f64, f64, f64)]) -> Result<geometry::T, error::T> {
    let count = values.len();
    let minimum = if params.closed { 3 } else { 2 };
    if count < minimum {
        return Err(make_error(
            "polyline",
            "invalid_parameter",
            &format!(
                "Rdk.Line_geometry.polyline: {} polylines require at least {minimum} points",
                if params.closed { "closed" } else { "open" }
            ),
        ));
    }
    if values.iter().any(|&(x, y, z)| !(x.is_finite() && y.is_finite() && z.is_finite())) {
        return Err(make_error("polyline", "invalid_parameter", "Rdk.Line_geometry.polyline: positions must be finite"));
    }
    let cloud = points(values)?;
    let mut builder = topology::Builder::create(count as i64, Some(count as i64), Some(1))?;
    let indices: Vec<i64> = (0..count as i64).collect();
    if params.closed {
        topology::Builder::add_closed_polyline(&mut builder, &indices)?;
    } else {
        topology::Builder::add_open_polyline(&mut builder, &indices)?;
    }
    geometry::create(
        geometry::positions(&cloud).clone(),
        topology::Builder::freeze(&mut builder)?,
        None,
        None,
        None,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;

    #[test]
    fn points_are_a_topology_free_cloud() {
        let cloud = points(&[(1.0, 2.0, 3.0), (4.0, 5.0, 6.0)]).unwrap();
        assert_eq!(geometry::point_count(&cloud), 2);
        assert_eq!(geometry::primitive_count(&cloud), 0);
        assert_eq!(packed::Float3::get(geometry::positions(&cloud), 1).unwrap(), (4.0, 5.0, 6.0));
    }

    #[test]
    fn polylines_open_closed_and_diagnostics() {
        let values = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0)];
        let open = polyline(&Params { closed: false }, &values).unwrap();
        assert_eq!(geometry::primitive_count(&open), 1);
        assert_eq!(geometry::vertex_count(&open), 3);
        assert_eq!(topology::primitive_kind(geometry::topology(&open), 0).unwrap(), PrimitiveKind::OpenPolyline);
        let closed = polyline(&Params { closed: true }, &values).unwrap();
        assert_eq!(topology::primitive_kind(geometry::topology(&closed), 0).unwrap(), PrimitiveKind::ClosedPolyline);
        let too_short = polyline(&Params { closed: true }, &values[..2]);
        let failure = too_short.err().unwrap();
        assert_eq!(error::code(&failure), "invalid_parameter");
        assert_eq!(
            error::message(&failure),
            "Rdk.Line_geometry.polyline: closed polylines require at least 3 points"
        );
        let two_open = polyline(&Params { closed: false }, &values[..2]);
        assert!(two_open.is_ok());
        let nonfinite = polyline(&Params { closed: false }, &[(0.0, 0.0, 0.0), (f64::NAN, 0.0, 0.0)]);
        assert_eq!(error::message(&nonfinite.err().unwrap()), "Rdk.Line_geometry.polyline: positions must be finite");
    }
}
