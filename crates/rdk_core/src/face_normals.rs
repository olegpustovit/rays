use crate::{cancel, error, geometry, group, packed, topology};
use crate::error::Code;

fn selected(selection: Option<&group::T>, primitive: i64) -> bool {
    match selection {
        None => true,
        Some(value) => group::mem(primitive, value),
    }
}
fn invalid(operation: &str, message: String) -> error::T {
    error::new(operation, Code::InvalidTopology, message)
}

fn cancelled(operation: &str) -> error::T {
    error::new(operation, Code::Cancelled, "geometry operation was cancelled".to_owned())
}

fn poll(cancel_value: Option<&cancel::T>, operation: &str, index: i64, mask: i64) -> Result<(), error::T> {
    cancel::poll(cancel_value, index, mask).map_err(|_| cancelled(operation))
}
fn view_error(operation: &str, value: error::T) -> error::T {
    if error::code(&value) == "cancelled" {
        value
    } else {
        invalid(operation, error::message(&value).to_owned())
    }
}

pub fn polygon_area_vector(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    need_inverse: bool,
    first_failure: bool,
    primitives: Option<&group::T>,
    operation: &str,
    geometry_value: &geometry::T,
)-> Result<((Vec<f64>, Vec<f64>, Vec<f64>), Vec<f64>), error::T> {
    if grain <= 0 {
        return Err(invalid(operation, format!("{operation}: grain must be positive")));
    }
    let topology_value = geometry::topology(geometry_value);
    let source = topology::View::view(topology_value)
        .map_err(|value| view_error(operation, value))?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))
        .map_err(|value| view_error(operation, value))?;
    let count = topology::primitive_count(topology_value);
    let count_usize = usize::try_from(count)
        .map_err(|_| invalid(operation, format!("{operation}: invalid primitive count")))?;
    let mut x = vec![0.0; count_usize];
    let mut y = vec![0.0; count_usize];
    let mut z = vec![0.0; count_usize];
    let mut inverse_length = if need_inverse { vec![0.0; count_usize] } else { Vec::new() };
    let grain = usize::try_from(grain)
        .map_err(|_| invalid(operation, format!("{operation}: grain must be positive")))?;
    let range_count = count_usize.div_ceil(grain);
    let mut errors = vec![-1i64; range_count];

    // ponytail: serial primitive ranges avoid duplicating output planes; use disjoint slices if the pool exposes them.
    for (range, first) in (0..count_usize).step_by(grain).enumerate() {
        let last = (first + grain).min(count_usize);
        for primitive in first..last {
            poll(cancel_value, operation, primitive as i64, 4095)?;
            if selected(primitives, primitive as i64) && source.primitive_kinds[primitive] == 0 {
                let vertex_first = source.primitive_offsets[primitive] as usize;
                let vertex_last = source.primitive_offsets[primitive + 1] as usize;
                let anchor = source.vertex_points[vertex_first] as usize;
                let ax = positions.x[anchor];
                let ay = positions.y[anchor];
                let az = positions.z[anchor];
                for vertex in vertex_first + 1..vertex_last.saturating_sub(1) {
                    let b = source.vertex_points[vertex] as usize;
                    let c = source.vertex_points[vertex + 1] as usize;
                    let ux = positions.x[b] - ax;
                    let uy = positions.y[b] - ay;
                    let uz = positions.z[b] - az;
                    let vx = positions.x[c] - ax;
                    let vy = positions.y[c] - ay;
                    let vz = positions.z[c] - az;
                    x[primitive] += uy * vz - uz * vy;
                    y[primitive] += uz * vx - ux * vz;
                    z[primitive] += ux * vy - uy * vx;
                }
                if !(x[primitive].is_finite() && y[primitive].is_finite() && z[primitive].is_finite()) {
                    if !first_failure || errors[range] < 0 {
                        errors[range] = primitive as i64;
                    }
                } else if need_inverse {
                    let nx = x[primitive];
                    let ny = y[primitive];
                    let nz = z[primitive];
                    let scale = nx.abs().max(ny.abs()).max(nz.abs());
                    if scale > 0.0 {
                        let sx = nx / scale;
                        let sy = ny / scale;
                        let sz = nz / scale;
                        inverse_length[primitive] = 1.0 / scale / (sx * sx + sy * sy + sz * sz).sqrt();
                    }
                }
            }
        }
    }
    if let Some(primitive) = errors.into_iter().find(|&value| value >= 0) {
        return Err(invalid(
            operation,
            format!("{operation}: non-finite geometric normal at primitive {primitive}"),
        ));
    }
    Ok(((x, y, z), inverse_length))
}

pub fn compute(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    primitives: Option<&group::T>,
    operation: &str,
    geometry_value: &geometry::T,
)-> Result<(Vec<f64>, Vec<f64>, Vec<f64>), error::T> {
    let grain = grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(invalid(operation, format!("{operation}: grain must be positive")));
    }
    let topology_value = geometry::topology(geometry_value);
    let primitive_count = topology::primitive_count(topology_value);
    if let Some(group_value) = primitives {
        if group::owner(group_value) != group::Owner::Primitive {
            return Err(invalid(operation, format!("{operation}: selection must own primitives")));
        }
        if group::length(group_value) != primitive_count {
            return Err(invalid(
                operation,
                format!("{operation}: selection length does not match primitive count"),
            ));
        }
    }
    let topology_view = topology::View::view(topology_value)
        .map_err(|value| view_error(operation, value))?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))
        .map_err(|value| view_error(operation, value))?;
    let count = usize::try_from(primitive_count)
        .map_err(|_| invalid(operation, format!("{operation}: invalid primitive count")))?;
    let mut nx = vec![0.0; count];
    let mut ny = vec![0.0; count];
    let mut nz = vec![0.0; count];
    let mut has_invalid = false;
    for primitive in 0..count {
        poll(cancel_value, operation, primitive as i64, 16_383)?;
        if !selected(primitives, primitive as i64) {
            continue;
        }
        if topology_view.primitive_kinds[primitive] != 0 {
            has_invalid = true;
            continue;
        }
        let first = topology_view.primitive_offsets[primitive] as usize;
        let last = topology_view.primitive_offsets[primitive + 1] as usize;
        let first_point = topology_view.vertex_points[first] as usize;
        let ax = positions.x[first_point];
        let ay = positions.y[first_point];
        let az = positions.z[first_point];
        for vertex in first + 1..last.saturating_sub(1) {
            poll(cancel_value, operation, vertex as i64, 16_383)?;
            let b = topology_view.vertex_points[vertex] as usize;
            let c = topology_view.vertex_points[vertex + 1] as usize;
            let ux = positions.x[b] - ax;
            let uy = positions.y[b] - ay;
            let uz = positions.z[b] - az;
            let vx = positions.x[c] - ax;
            let vy = positions.y[c] - ay;
            let vz = positions.z[c] - az;
            nx[primitive] += uy * vz - uz * vy;
            ny[primitive] += uz * vx - ux * vz;
            nz[primitive] += ux * vy - uy * vx;
        }
        let x = nx[primitive];
        let y = ny[primitive];
        let z = nz[primitive];
        let scale = x.abs().max(y.abs()).max(z.abs());
        if scale == 0.0 || !(x.is_finite() && y.is_finite() && z.is_finite()) {
            has_invalid = true;
        } else {
            let x = x / scale;
            let y = y / scale;
            let z = z / scale;
            let inverse = 1.0 / (x * x + y * y + z * z).sqrt();
            nx[primitive] = x * inverse;
            ny[primitive] = y * inverse;
            nz[primitive] = z * inverse;
        }
    }
    let _ = grain;
    if has_invalid {
        Err(invalid(
            operation,
            format!("{operation}: selected primitives must be finite non-degenerate polygons"),
        ))
    } else {
        Ok((nx, ny, nz))
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    fn triangle() -> geometry::T {
        let topology_value =
            topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        let positions = packed::Float3::View::of_owned_exn(
            vec![0.0, 1.0, 0.0],
            vec![0.0, 0.0, 1.0],
            vec![0.0, 0.0, 0.0],
        )
        .unwrap();
        geometry::create(positions, topology_value, None, None, None).unwrap()
    }

    #[test]
    fn triangle_area_and_unit_normal_match_winding() {
        let geometry_value = triangle();
        let ((x, y, z), inverse) =
            polygon_area_vector(None, 1, true, false, None, "face_normals", &geometry_value)
                .unwrap();
        assert_eq!(x, vec![0.0]);
        assert_eq!(y, vec![0.0]);
        assert_eq!(z, vec![1.0]);
        assert_eq!(inverse, vec![1.0]);

        let (x, y, z) = compute(None, None, None, "face_normals", &geometry_value).unwrap();
        assert_eq!((x[0], y[0], z[0]), (0.0, 0.0, 1.0));
    }

    #[test]
    fn normal_kernels_report_cancellation_as_a_structured_error() {
        let token = cancel::create();
        cancel::cancel(&token);
        let error = compute(Some(&token), Some(1), None, "face_normals", &triangle()).unwrap_err();
        assert_eq!(error::code(&error), "cancelled");
    }
}
