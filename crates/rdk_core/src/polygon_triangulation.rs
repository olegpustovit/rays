use crate::{cancel, error, packed, topology};
use crate::error::Code;
fn invalid_polygon(message: String) -> error::T {
    error::new("polygon_triangulation", Code::InvalidPolygon, message)
}

fn cancelled_error() -> error::T {
    error::cancelled("polygon_triangulation")
}

pub struct T {
    u: Vec<f64>,
    v: Vec<f64>,
    remaining: Vec<i64>,
    accumulators: [f64; 5],
}

pub fn create_scratch() -> T {
    T { u: Vec::new(), v: Vec::new(), remaining: Vec::new(), accumulators: [0.0; 5] }
}

fn ensure(scratch: &mut T, size: usize) {
    if scratch.u.len() < size {
        let mut capacity = scratch.u.len().max(4);
        while capacity < size {
            capacity = capacity.checked_mul(2).unwrap_or(size).max(size);
        }
        scratch.u.resize(capacity, 0.0);
        scratch.v.resize(capacity, 0.0);
        scratch.remaining.resize(capacity, 0);
    }
}

fn cross(u: &[f64], v: &[f64], base: i64, orientation: f64, a: i64, b: i64, c: i64) -> f64 {
    let a = (base + a) as usize;
    let b = (base + b) as usize;
    let c = (base + c) as usize;
    (((u[b] - u[a]) * (v[c] - v[a])) - ((v[b] - v[a]) * (u[c] - u[a]))) * orientation
}

fn contains(u: &[f64], v: &[f64], base: i64, orientation: f64, a: i64, b: i64, c: i64, point: i64) -> bool {
    cross(u, v, base, orientation, a, b, point) >= -1e-12
        && cross(u, v, base, orientation, b, c, point) >= -1e-12
        && cross(u, v, base, orientation, c, a, point) >= -1e-12
}

pub fn find_ear(
    distinct: &dyn Fn(i64, i64, i64, i64) -> bool,
    u: &[f64],
    v: &[f64],
    base: i64,
    orientation: f64,
    remaining: &[i64],
    active: i64,
) -> i64 {
    let mut slot = 0;
    while slot < active {
        let a = remaining[(base + (slot + active - 1) % active) as usize];
        let b = remaining[(base + slot) as usize];
        let c = remaining[(base + (slot + 1) % active) as usize];
        if cross(u, v, base, orientation, a, b, c) > 1e-12 {
            let mut blocked = false;
            let mut other = 0;
            while other < active && !blocked {
                let point = remaining[(base + other) as usize];
                if distinct(point, a, b, c)
                    && contains(u, v, base, orientation, a, b, c, point)
                {
                    blocked = true;
                }
                other += 1;
            }
            if !blocked {
                return slot;
            }
        }
        slot += 1;
    }
    -1
}

pub fn find_projected_ear(
    u: &[f64],
    v: &[f64],
    base: i64,
    orientation: f64,
    remaining: &[i64],
    active: i64,
) -> i64 {
    find_ear(
        &|point, a, b, c| point != a && point != b && point != c,
        u,
        v,
        base,
        orientation,
        remaining,
        active,
    )
}

pub fn primitive(
    cancel_token: Option<&cancel::T>,
    positions: &packed::Float3::View::View,
    topology_view: &topology::View::View,
    scratch: Option<&mut T>,
    primitive: i64,
    emit: &mut dyn FnMut(i64, i64, i64, i64),
)-> Result<(), error::T> {
    let primitive_index = primitive as usize;
    let first = topology_view.primitive_offsets[primitive_index];
    let last = topology_view.primitive_offsets[primitive_index + 1];
    let size = last - first;
    if topology_view.primitive_kinds[primitive_index] != 0 {
        return Err(invalid_polygon(format!("primitive {primitive} is a curve, not a polygon")));
    }
    if size < 3 {
        return Err(invalid_polygon(format!("primitive {primitive} has fewer than three corners")));
    }
    if size == 3 {
        emit(0, first, first + 1, first + 2);
        return Ok(());
    }

    let mut local_scratch;
    let scratch = match scratch {
        Some(value) => value,
        None => {
            local_scratch = create_scratch();
            &mut local_scratch
        }
    };
    let size_usize = size as usize;
    ensure(scratch, size_usize);
    let sums = &mut scratch.accumulators;
    *sums = [0.0; 5];
    sums[4] = 1.0;
    for local in 0..size {
        cancel::poll(cancel_token, local, 4095).map_err(|_| cancelled_error())?;
        let next = (local + 1) % size;
        let pi = topology_view.vertex_points[(first + local) as usize] as usize;
        let pj = topology_view.vertex_points[(first + next) as usize] as usize;
        let xi = positions.x[pi];
        let yi = positions.y[pi];
        let zi = positions.z[pi];
        let xj = positions.x[pj];
        let yj = positions.y[pj];
        let zj = positions.z[pj];
        if !(xi.is_finite() && yi.is_finite() && zi.is_finite() && xj.is_finite() && yj.is_finite() && zj.is_finite()) {
            sums[4] = 0.0;
        } else {
            sums[0] += (yi - yj) * (zi + zj);
            sums[1] += (zi - zj) * (xi + xj);
            sums[2] += (xi - xj) * (yi + yj);
        }
    }
    if sums[4] == 0.0 {
        return Err(invalid_polygon(format!("primitive {primitive} has a non-finite position")));
    }

    let ax = sums[0].abs();
    let ay = sums[1].abs();
    let az = sums[2].abs();
    let dominant = if ax >= ay && ax >= az { 0 } else if ay >= az { 1 } else { 2 };
    for local in 0..size {
        let point = topology_view.vertex_points[(first + local) as usize] as usize;
        if dominant == 0 {
            scratch.u[local as usize] = positions.y[point];
            scratch.v[local as usize] = positions.z[point];
        } else if dominant == 1 {
            scratch.u[local as usize] = positions.x[point];
            scratch.v[local as usize] = positions.z[point];
        } else {
            scratch.u[local as usize] = positions.x[point];
            scratch.v[local as usize] = positions.y[point];
        }
    }

    sums[3] = 0.0;
    for local in 0..size {
        let next = (local + 1) % size;
        sums[3] += (scratch.u[local as usize] * scratch.v[next as usize])
            - (scratch.u[next as usize] * scratch.v[local as usize]);
    }
    if !sums[3].is_finite() || sums[3].abs() <= 1e-14 {
        return Err(invalid_polygon(format!("primitive {primitive} has degenerate projected area")));
    }

    let orientation = if sums[3] > 0.0 { 1.0 } else { -1.0 };
    for local in 0..size {
        scratch.remaining[local as usize] = local;
    }
    let mut active = size;
    let mut emitted = 0;
    while active > 3 {
        cancel::check_opt(cancel_token).map_err(|_| cancelled_error())?;
        let ear = find_projected_ear(&scratch.u, &scratch.v, 0, orientation, &scratch.remaining, active);
        if ear < 0 {
            return Err(invalid_polygon(format!("primitive {primitive} is non-simple or degenerate")));
        }
        let a = scratch.remaining[((ear + active - 1) % active) as usize];
        let b = scratch.remaining[ear as usize];
        let c = scratch.remaining[((ear + 1) % active) as usize];
        emit(emitted, first + a, first + b, first + c);
        scratch.remaining.copy_within((ear + 1) as usize..active as usize, ear as usize);
        active -= 1;
        emitted += 1;
    }
    emit(
        emitted,
        first + scratch.remaining[0],
        first + scratch.remaining[1],
        first + scratch.remaining[2],
    );
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_convex_ear_is_stable() {
        assert_eq!(
            find_projected_ear(&[0.0, 1.0, 1.0, 0.0], &[0.0, 0.0, 1.0, 1.0], 0, 1.0, &[0, 1, 2, 3], 4),
            0
        );
    }

    #[test]
    fn degenerate_polygon_reports_source_failure() {
        let positions = packed::Float3::View::of_owned_exn(
            (0..8).map(f64::from).collect(),
            vec![0.0; 8],
            vec![0.0; 8],
        )
        .unwrap();
        let topology = topology::polygons_owned(8, vec![0, 1, 2, 3, 4, 5, 6, 7], vec![0, 4, 8]).unwrap();
        let positions = packed::Float3::View::view(&positions).unwrap();
        let topology = topology::View::view(&topology).unwrap();
        let mut scratch = create_scratch();
        let mut emit = |_: i64, _: i64, _: i64, _: i64| {};
        let error = primitive(None, &positions, &topology, Some(&mut scratch), 0, &mut emit).unwrap_err();
        assert_eq!(crate::error::code(&error), "invalid_polygon");
        assert_eq!(crate::error::message(&error), "primitive 0 has degenerate projected area");
    }
}
