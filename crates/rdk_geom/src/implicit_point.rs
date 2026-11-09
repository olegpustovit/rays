//! Port of lib/rdk/geom/exact/implicit_point.ml: exact homogeneous points used by
//! mesh-arrangement kernels. `Invalid_argument` becomes a panic; indices are `i64`.
//! The float filters keep OCaml's exact operation order (no FMA contraction).
use rays_math::float::{min as float_min, max as float_max};
use crate::exact_dyadic as exact;
use crate::exact_dyadic::ldexp;
use crate::exact_dyadic::scratch;
use crate::predicates;
use crate::predicates::Sign;
use std::sync::{Arc, OnceLock};

#[derive(Debug)]
struct SourceData {
    x: Vec<f64>,
    y: Vec<f64>,
    z: Vec<f64>,
}

/// Validated packed coordinate planes. Cloning shares the planes; identity (pointer equality)
/// decides whether two points belong to the same source.
#[derive(Clone, Debug)]
pub struct Source {
    data: Arc<SourceData>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Projection {
    XY,
    YZ,
    ZX,
}

#[derive(Clone, Debug)]
struct Homogeneous {
    hx: exact::T,
    hy: exact::T,
    hz: exact::T,
    hw: exact::T,
}

#[derive(Clone, Debug)]
pub enum Construction {
    Explicit(i64),
    Rounded { x: f64, y: f64, z: f64 },
    LinePlane { line_start: i64, line_end: i64, plane_a: i64, plane_b: i64, plane_c: i64 },
    LineLine {
        projection: Projection,
        first_start: i64,
        first_end: i64,
        second_start: i64,
        second_end: i64,
    },
    TriplePlane {
        first_a: i64,
        first_b: i64,
        first_c: i64,
        second_a: i64,
        second_b: i64,
        second_c: i64,
        third_a: i64,
        third_b: i64,
        third_c: i64,
    },
    Midpoint { first: T, second: T },
    Centroid3 { first: T, second: T, third: T },
    Circumcenter2Xy { first: T, second: T, third: T },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Error {
    CoordinatePlaneSizeMismatch,
    NonFiniteCoordinate(i64),
    IndexOutOfBounds(i64),
    ParallelLineAndPlane,
    ParallelLines,
    DependentPlanes,
    PointSourceMismatch,
    DegenerateTriangle,
    NonFiniteApproximation,
}

#[derive(Debug)]
struct Node {
    source: Source,
    construction: Construction,
    homogeneous: OnceLock<Homogeneous>,
    /// Approximation, certified lower/upper bounds and error radius per axis (x, y, z).
    approximate: [f64; 3],
    lower: [f64; 3],
    upper: [f64; 3],
    error: [f64; 3],
}

/// An exact implicit point; cloning shares the node (identity is pointer equality).
#[derive(Clone, Debug)]
pub struct T {
    node: Arc<Node>,
}

fn same_point(left: &T, right: &T) -> bool {
    Arc::ptr_eq(&left.node, &right.node)
}

fn same_source_ref(left: &Source, right: &Source) -> bool {
    Arc::ptr_eq(&left.data, &right.data)
}

const MINIMUM_SUBNORMAL: f64 = f64::from_bits(1);

/// Validate and take the packed coordinate planes. They are immutable for the lifetime of every
/// point constructed from the source.
pub fn source(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> Result<Source, Error> {
    let count = x.len();
    if y.len() != count || z.len() != count {
        return Err(Error::CoordinatePlaneSizeMismatch);
    }
    for index in 0..count {
        if !(x[index].is_finite() && y[index].is_finite() && z[index].is_finite()) {
            return Err(Error::NonFiniteCoordinate(index as i64));
        }
    }
    Ok(Source { data: Arc::new(SourceData { x, y, z }) })
}

pub fn construction(point: &T) -> &Construction {
    &point.node.construction
}

/// Number of exactly equal binary64 coordinate components on two source vertices.
pub fn constant_components(source: &Source, first: i64, second: i64) -> i64 {
    let (first, second) = (first as usize, second as usize);
    let d = &source.data;
    (if d.x[first] == d.x[second] { 1 } else { 0 })
        + (if d.y[first] == d.y[second] { 1 } else { 0 })
        + if d.z[first] == d.z[second] { 1 } else { 0 }
}

fn valid_index(source: &Source, index: i64) -> bool {
    index >= 0 && (index as usize) < source.data.x.len()
}

pub fn source_coordinate(source: &Source, point: i64) -> (f64, f64, f64) {
    if !valid_index(source, point) {
        unreachable!("implicit source point is out of bounds");
    }
    let p = point as usize;
    (source.data.x[p], source.data.y[p], source.data.z[p])
}

type Triple = (scratch::V, scratch::V, scratch::V);

fn exact_at(scope: &scratch::Scope, source: &Source, index: i64) -> Triple {
    let i = index as usize;
    (
        scope.of_float(source.data.x[i]),
        scope.of_float(source.data.y[i]),
        scope.of_float(source.data.z[i]),
    )
}

fn add3(scope: &scratch::Scope, a: &Triple, b: &Triple) -> Triple {
    (
        scope.add(&a.0, &b.0),
        scope.add(&a.1, &b.1),
        scope.add(&a.2, &b.2),
    )
}

fn subtract3(scope: &scratch::Scope, a: &Triple, b: &Triple) -> Triple {
    (
        scope.subtract(&a.0, &b.0),
        scope.subtract(&a.1, &b.1),
        scope.subtract(&a.2, &b.2),
    )
}

fn scale3(scope: &scratch::Scope, scalar: &scratch::V, v: &Triple) -> Triple {
    (
        scope.multiply(scalar, &v.0),
        scope.multiply(scalar, &v.1),
        scope.multiply(scalar, &v.2),
    )
}

fn dot(scope: &scratch::Scope, a: &Triple, b: &Triple) -> scratch::V {
    scope.add(
        &scope.add(&scope.multiply(&a.0, &b.0), &scope.multiply(&a.1, &b.1)),
        &scope.multiply(&a.2, &b.2),
    )
}

fn cross(scope: &scratch::Scope, a: &Triple, b: &Triple) -> Triple {
    (
        scope.subtract(&scope.multiply(&a.1, &b.2), &scope.multiply(&a.2, &b.1)),
        scope.subtract(&scope.multiply(&a.2, &b.0), &scope.multiply(&a.0, &b.2)),
        scope.subtract(&scope.multiply(&a.0, &b.1), &scope.multiply(&a.1, &b.0)),
    )
}

fn owned(scope: &scratch::Scope, mut coordinates: [scratch::V; 4]) -> Homogeneous {
    if scope.compare_zero(&coordinates[3]) < 0 {
        coordinates = coordinates.map(|value| scope.negate(&value));
    }
    let [hx, hy, hz, hw] = coordinates.map(|value| scope.export(value));
    Homogeneous { hx, hy, hz, hw }
}

fn imported(scope: &scratch::Scope, point: &Homogeneous) -> [scratch::V; 4] {
    [&point.hx, &point.hy, &point.hz, &point.hw].map(|value| scope.of_exact(value))
}

fn normalize(point: Homogeneous) -> Homogeneous {
    if exact::compare_zero(&point.hw) < 0 {
        Homogeneous {
            hx: exact::negate(&point.hx),
            hy: exact::negate(&point.hy),
            hz: exact::negate(&point.hz),
            hw: exact::negate(&point.hw),
        }
    } else {
        point
    }
}

fn approximate_ratio(numerator: &exact::T, denominator: &exact::T) -> f64 {
    if exact::is_zero(numerator) {
        0.
    } else {
        let (numerator_mantissa, numerator_exponent) = exact::to_scaled_float(numerator);
        let (denominator_mantissa, denominator_exponent) = exact::to_scaled_float(denominator);
        ldexp(
            numerator_mantissa / denominator_mantissa,
            numerator_exponent - denominator_exponent,
        )
    }
}

fn downward(value: f64) -> f64 {
    value.next_down()
}

fn upward(value: f64) -> f64 {
    value.next_up()
}

fn ratio_interval(numerator: &exact::T, denominator: &exact::T) -> (f64, f64) {
    if exact::is_zero(numerator) {
        (0., 0.)
    } else {
        let (numerator_lower, numerator_upper, numerator_exponent) =
            exact::to_scaled_interval(numerator);
        let (denominator_lower, denominator_upper, denominator_exponent) =
            exact::to_scaled_interval(denominator);
        assert!(denominator_lower > 0.);
        let first = numerator_lower / denominator_lower;
        let second = numerator_lower / denominator_upper;
        let third = numerator_upper / denominator_lower;
        let fourth = numerator_upper / denominator_upper;
        let lower = float_min(float_min(first, second), float_min(third, fourth));
        let upper = float_max(float_max(first, second), float_max(third, fourth));
        let exponent = numerator_exponent - denominator_exponent;
        (downward(ldexp(lower, exponent)), upward(ldexp(upper, exponent)))
    }
}

fn make(source: &Source, construction: Construction, homogeneous: Homogeneous) -> Result<T, Error> {
    let homogeneous = normalize(homogeneous);
    let approximate_x = approximate_ratio(&homogeneous.hx, &homogeneous.hw);
    let approximate_y = approximate_ratio(&homogeneous.hy, &homogeneous.hw);
    let approximate_z = approximate_ratio(&homogeneous.hz, &homogeneous.hw);
    let (x_lower, x_upper) = ratio_interval(&homogeneous.hx, &homogeneous.hw);
    let (y_lower, y_upper) = ratio_interval(&homogeneous.hy, &homogeneous.hw);
    let (z_lower, z_upper) = ratio_interval(&homogeneous.hz, &homogeneous.hw);
    if approximate_x.is_finite() && approximate_y.is_finite() && approximate_z.is_finite() {
        Ok(T {
            node: Arc::new(Node {
                source: source.clone(),
                construction,
                homogeneous: OnceLock::from(homogeneous),
                approximate: [approximate_x, approximate_y, approximate_z],
                lower: [x_lower, y_lower, z_lower],
                upper: [x_upper, y_upper, z_upper],
                error: [
                    (approximate_x - x_lower).abs().max((x_upper - approximate_x).abs()),
                    (approximate_y - y_lower).abs().max((y_upper - approximate_y).abs()),
                    (approximate_z - z_lower).abs().max((z_upper - approximate_z).abs()),
                ],
            }),
        })
    } else {
        Err(Error::NonFiniteApproximation)
    }
}

fn make_deferred_explicit(source: &Source, construction: Construction, x: f64, y: f64, z: f64) -> T {
    T {
        node: Arc::new(Node {
            source: source.clone(),
            construction,
            homogeneous: OnceLock::new(),
            approximate: [x, y, z],
            lower: [x, y, z],
            upper: [x, y, z],
            error: [0., 0., 0.],
        }),
    }
}

pub fn explicit(source: &Source, index: i64) -> Result<T, Error> {
    if !valid_index(source, index) {
        return Err(Error::IndexOutOfBounds(index));
    }
    let i = index as usize;
    Ok(make_deferred_explicit(
        source,
        Construction::Explicit(index),
        source.data.x[i],
        source.data.y[i],
        source.data.z[i],
    ))
}

/// Attach an explicitly rounded finite construction to an existing source arena.
pub fn rounded(reference: &T, x: f64, y: f64, z: f64) -> Result<T, Error> {
    if !(x.is_finite() && y.is_finite() && z.is_finite()) {
        Err(Error::NonFiniteApproximation)
    } else {
        Ok(make_deferred_explicit(&reference.node.source, Construction::Rounded { x, y, z }, x, y, z))
    }
}

fn first_invalid(source: &Source, indices: &[i64]) -> Option<i64> {
    indices.iter().copied().find(|index| !valid_index(source, *index))
}

/// `(|l| * re) + (|r| * le) + (le * re) + (factor * |p|) + smallest`
fn product_error(l: f64, le: f64, r: f64, re: f64, p: f64, factor: f64, smallest: f64) -> f64 {
    (l.abs() * re) + (r.abs() * le) + (le * re) + (factor * p.abs()) + smallest
}

/// `ae + be + (factor * (|a| + |b|)) + smallest`
fn difference_error(ae: f64, be: f64, a: f64, b: f64, factor: f64, smallest: f64) -> f64 {
    ae + be + (factor * (a.abs() + b.abs())) + smallest
}

/// `(factor * (|a| + |b|)) + smallest`
fn endpoint_error(factor: f64, a: f64, b: f64, smallest: f64) -> f64 {
    (factor * (a.abs() + b.abs())) + smallest
}

fn all_finite(values: &[f64]) -> bool {
    values.iter().all(|value| value.is_finite())
}

/// Shared tail of the LPI/LLI deferred constructions: `parameter` error then per-axis result.
/// Returns `None` when a quantity is not finite.
fn deferred_point(
    source: &Source,
    construction: Construction,
    parts: [(f64, f64, f64, f64); 3],
) -> Option<T> {
    // parts per axis: (approximate, error, lower, upper)
    let mut approximate = [0.; 3];
    let mut error = [0.; 3];
    let mut lower = [0.; 3];
    let mut upper = [0.; 3];
    for axis in 0..3 {
        approximate[axis] = parts[axis].0;
        error[axis] = parts[axis].1;
        lower[axis] = parts[axis].2;
        upper[axis] = parts[axis].3;
    }
    if all_finite(&approximate) && all_finite(&lower) && all_finite(&upper) && all_finite(&error) {
        Some(T {
            node: Arc::new(Node {
                source: source.clone(),
                construction,
                homogeneous: OnceLock::new(),
                approximate,
                lower,
                upper,
                error,
            }),
        })
    } else {
        None
    }
}

/// `(parameter, parameter_error)` of a filtered line intersection, or `None` when the
/// denominator is not certified away from zero.
fn filtered_parameter(
    numerator: f64,
    denominator: f64,
    numerator_error: f64,
    denominator_error: f64,
    factor: f64,
    smallest: f64,
) -> Option<(f64, f64)> {
    let absolute_denominator = denominator.abs();
    if !(denominator.is_finite()
        && numerator.is_finite()
        && denominator_error.is_finite()
        && numerator_error.is_finite())
        || absolute_denominator <= denominator_error
    {
        None
    } else {
        let parameter = numerator / denominator;
        let margin = absolute_denominator - denominator_error;
        let parameter_error = upward(
            ((numerator_error * absolute_denominator) + (numerator.abs() * denominator_error))
                / (absolute_denominator * margin)
                + (factor * (numerator / denominator).abs())
                + smallest,
        );
        Some((parameter, parameter_error))
    }
}

/// Per-axis result of `p + direction * parameter` with propagated error.
fn deferred_coordinate(
    p: f64,
    direction: f64,
    direction_error: f64,
    parameter: f64,
    parameter_error: f64,
    factor: f64,
    smallest: f64,
) -> (f64, f64, f64, f64) {
    let delta = direction * parameter;
    let delta_error =
        product_error(direction, direction_error, parameter, parameter_error, delta, factor, smallest);
    let raw = p + delta;
    let raw_error = upward(delta_error + (factor * (p.abs() + delta.abs())) + smallest);
    if direction == 0. {
        (p, 0., p, p)
    } else {
        (raw, raw_error, downward(raw - raw_error), upward(raw + raw_error))
    }
}

fn try_line_plane_deferred(
    source: &Source,
    construction: Construction,
    line_start: i64,
    line_end: i64,
    plane_a: i64,
    plane_b: i64,
    plane_c: i64,
) -> Option<T> {
    let d = &source.data;
    let pick = |i: i64| -> [f64; 3] {
        let i = i as usize;
        [d.x[i], d.y[i], d.z[i]]
    };
    let (p, q, r, s, t) = (pick(line_start), pick(line_end), pick(plane_a), pick(plane_b), pick(plane_c));
    let factor = 8. * f64::EPSILON;
    let smallest = MINIMUM_SUBNORMAL;
    let mut dd = [0.; 3];
    let mut u = [0.; 3];
    let mut v = [0.; 3];
    let mut w = [0.; 3];
    let mut d_error = [0.; 3];
    let mut u_error = [0.; 3];
    let mut v_error = [0.; 3];
    let mut w_error = [0.; 3];
    for i in 0..3 {
        dd[i] = q[i] - p[i];
        u[i] = s[i] - r[i];
        v[i] = t[i] - r[i];
        w[i] = r[i] - p[i];
        d_error[i] = endpoint_error(factor, q[i], p[i], smallest);
        u_error[i] = endpoint_error(factor, s[i], r[i], smallest);
        v_error[i] = endpoint_error(factor, t[i], r[i], smallest);
        w_error[i] = endpoint_error(factor, r[i], p[i], smallest);
    }
    let mut n = [0.; 3];
    let mut n_error = [0.; 3];
    for i in 0..3 {
        let (j, k) = ((i + 1) % 3, (i + 2) % 3);
        let left = u[j] * v[k];
        let right = u[k] * v[j];
        let left_error = product_error(u[j], u_error[j], v[k], v_error[k], left, factor, smallest);
        let right_error = product_error(u[k], u_error[k], v[j], v_error[j], right, factor, smallest);
        n[i] = left - right;
        n_error[i] = difference_error(left_error, right_error, left, right, factor, smallest);
    }
    let mut denominator_part = [0.; 3];
    let mut numerator_part = [0.; 3];
    let mut denominator_part_error = [0.; 3];
    let mut numerator_part_error = [0.; 3];
    for i in 0..3 {
        denominator_part[i] = n[i] * dd[i];
        numerator_part[i] = n[i] * w[i];
        denominator_part_error[i] =
            product_error(n[i], n_error[i], dd[i], d_error[i], denominator_part[i], factor, smallest);
        numerator_part_error[i] =
            product_error(n[i], n_error[i], w[i], w_error[i], numerator_part[i], factor, smallest);
    }
    let denominator = denominator_part[0] + denominator_part[1] + denominator_part[2];
    let numerator = numerator_part[0] + numerator_part[1] + numerator_part[2];
    let denominator_error = upward(
        denominator_part_error[0]
            + denominator_part_error[1]
            + denominator_part_error[2]
            + (factor
                * (denominator_part[0].abs() + denominator_part[1].abs() + denominator_part[2].abs()))
            + smallest,
    );
    let numerator_error = upward(
        numerator_part_error[0]
            + numerator_part_error[1]
            + numerator_part_error[2]
            + (factor * (numerator_part[0].abs() + numerator_part[1].abs() + numerator_part[2].abs()))
            + smallest,
    );
    let (parameter, parameter_error) =
        filtered_parameter(numerator, denominator, numerator_error, denominator_error, factor, smallest)?;
    // An LPI coordinate is exactly constant when both line endpoints carry the same binary64
    // component. Preserve that singleton certificate.
    let parts = [
        deferred_coordinate(p[0], dd[0], d_error[0], parameter, parameter_error, factor, smallest),
        deferred_coordinate(p[1], dd[1], d_error[1], parameter, parameter_error, factor, smallest),
        deferred_coordinate(p[2], dd[2], d_error[2], parameter, parameter_error, factor, smallest),
    ];
    deferred_point(source, construction, parts)
}

pub fn line_plane(
    source: &Source,
    line_start: i64,
    line_end: i64,
    plane_a: i64,
    plane_b: i64,
    plane_c: i64,
) -> Result<T, Error> {
    if let Some(index) = first_invalid(source, &[line_start, line_end, plane_a, plane_b, plane_c]) {
        return Err(Error::IndexOutOfBounds(index));
    }
    let construction = Construction::LinePlane {
        line_start,
        line_end,
        plane_a,
        plane_b,
        plane_c,
    };
    if let Some(point) = try_line_plane_deferred(
        source,
        construction.clone(),
        line_start,
        line_end,
        plane_a,
        plane_b,
        plane_c,
    ) {
        return Ok(point);
    }
    make(
        source,
        construction,
        line_plane_homogeneous(source, line_start, line_end, plane_a, plane_b, plane_c)?,
    )
}

fn line_plane_homogeneous(
    source: &Source,
    line_start: i64,
    line_end: i64,
    plane_a: i64,
    plane_b: i64,
    plane_c: i64,
) -> Result<Homogeneous, Error> {
    let scope = &scratch::Scope::new();
    let p = exact_at(scope, source, line_start);
    let q = exact_at(scope, source, line_end);
    let r = exact_at(scope, source, plane_a);
    let s = exact_at(scope, source, plane_b);
    let plane_t = exact_at(scope, source, plane_c);
    let direction = subtract3(scope, &q, &p);
    let normal = cross(
        scope,
        &subtract3(scope, &s, &r),
        &subtract3(scope, &plane_t, &r),
    );
    let hw = dot(scope, &normal, &direction);
    if scope.is_zero(&hw) {
        return Err(Error::ParallelLineAndPlane);
    }
    let line_parameter_numerator = dot(scope, &normal, &subtract3(scope, &r, &p));
    let (hx, hy, hz) = add3(
        scope,
        &scale3(scope, &hw, &p),
        &scale3(scope, &line_parameter_numerator, &direction),
    );
    Ok(owned(scope, [hx, hy, hz, hw]))
}

fn projected(projection: Projection, v: &Triple) -> (scratch::V, scratch::V) {
    match projection {
        Projection::XY => (v.0, v.1),
        Projection::YZ => (v.1, v.2),
        Projection::ZX => (v.2, v.0),
    }
}

fn line_line_homogeneous(
    source: &Source,
    projection: Projection,
    first_start: i64,
    first_end: i64,
    second_start: i64,
    second_end: i64,
) -> Result<Homogeneous, Error> {
    let scope = &scratch::Scope::new();
    let p = exact_at(scope, source, first_start);
    let q = exact_at(scope, source, first_end);
    let r = exact_at(scope, source, second_start);
    let s = exact_at(scope, source, second_end);
    let direction = subtract3(scope, &q, &p);
    let other_direction = subtract3(scope, &s, &r);
    let (dx, dy) = projected(projection, &direction);
    let (ox, oy) = projected(projection, &other_direction);
    let (rx, ry) = projected(projection, &subtract3(scope, &r, &p));
    let hw = scope.subtract(&scope.multiply(&dx, &oy), &scope.multiply(&dy, &ox));
    if scope.is_zero(&hw) {
        Err(Error::ParallelLines)
    } else {
        let parameter = scope.subtract(&scope.multiply(&rx, &oy), &scope.multiply(&ry, &ox));
        let (hx, hy, hz) = add3(
            scope,
            &scale3(scope, &hw, &p),
            &scale3(scope, &parameter, &direction),
        );
        Ok(owned(scope, [hx, hy, hz, hw]))
    }
}

fn try_line_line_deferred(
    source: &Source,
    construction: Construction,
    projection: Projection,
    first_start: i64,
    first_end: i64,
    second_start: i64,
    second_end: i64,
) -> Option<T> {
    let d = &source.data;
    let (first_plane, second_plane) = match projection {
        Projection::XY => (&d.x, &d.y),
        Projection::YZ => (&d.y, &d.z),
        Projection::ZX => (&d.z, &d.x),
    };
    let (fs, fe, ss, se) =
        (first_start as usize, first_end as usize, second_start as usize, second_end as usize);
    let (pu, pv) = (first_plane[fs], second_plane[fs]);
    let (qu, qv) = (first_plane[fe], second_plane[fe]);
    let (ru, rv) = (first_plane[ss], second_plane[ss]);
    let (su, sv) = (first_plane[se], second_plane[se]);
    let factor = 8. * f64::EPSILON;
    let smallest = MINIMUM_SUBNORMAL;
    let du = qu - pu;
    let dv = qv - pv;
    let ou = su - ru;
    let ov = sv - rv;
    let wu = ru - pu;
    let wv = rv - pv;
    let du_error = endpoint_error(factor, qu, pu, smallest);
    let dv_error = endpoint_error(factor, qv, pv, smallest);
    let ou_error = endpoint_error(factor, su, ru, smallest);
    let ov_error = endpoint_error(factor, sv, rv, smallest);
    let wu_error = endpoint_error(factor, ru, pu, smallest);
    let wv_error = endpoint_error(factor, rv, pv, smallest);
    let denominator_left = du * ov;
    let denominator_right = dv * ou;
    let numerator_left = wu * ov;
    let numerator_right = wv * ou;
    let denominator_left_error =
        product_error(du, du_error, ov, ov_error, denominator_left, factor, smallest);
    let denominator_right_error =
        product_error(dv, dv_error, ou, ou_error, denominator_right, factor, smallest);
    let numerator_left_error =
        product_error(wu, wu_error, ov, ov_error, numerator_left, factor, smallest);
    let numerator_right_error =
        product_error(wv, wv_error, ou, ou_error, numerator_right, factor, smallest);
    let denominator = denominator_left - denominator_right;
    let numerator = numerator_left - numerator_right;
    let denominator_error = upward(
        denominator_left_error
            + denominator_right_error
            + (factor * (denominator_left.abs() + denominator_right.abs()))
            + smallest,
    );
    let numerator_error = upward(
        numerator_left_error
            + numerator_right_error
            + (factor * (numerator_left.abs() + numerator_right.abs()))
            + smallest,
    );
    let (parameter, parameter_error) =
        filtered_parameter(numerator, denominator, numerator_error, denominator_error, factor, smallest)?;
    let coordinate = |plane: &Vec<f64>| -> (f64, f64, f64, f64) {
        let p = plane[fs];
        let q = plane[fe];
        let direction = q - p;
        let direction_error = (factor * (q.abs() + p.abs())) + smallest;
        deferred_coordinate(p, direction, direction_error, parameter, parameter_error, factor, smallest)
    };
    let parts = [coordinate(&d.x), coordinate(&d.y), coordinate(&d.z)];
    deferred_point(source, construction, parts)
}

pub fn line_line(
    source: &Source,
    projection: Projection,
    first_start: i64,
    first_end: i64,
    second_start: i64,
    second_end: i64,
) -> Result<T, Error> {
    if let Some(index) = first_invalid(source, &[first_start, first_end, second_start, second_end]) {
        return Err(Error::IndexOutOfBounds(index));
    }
    let construction =
        Construction::LineLine { projection, first_start, first_end, second_start, second_end };
    if let Some(point) = try_line_line_deferred(
        source,
        construction.clone(),
        projection,
        first_start,
        first_end,
        second_start,
        second_end,
    ) {
        return Ok(point);
    }
    let homogeneous =
        line_line_homogeneous(source, projection, first_start, first_end, second_start, second_end)?;
    make(source, construction, homogeneous)
}

fn plane(scope: &scratch::Scope, source: &Source, a: i64, b: i64, c: i64) -> (Triple, scratch::V) {
    let p = exact_at(scope, source, a);
    let normal = cross(
        scope,
        &subtract3(scope, &exact_at(scope, source, b), &p),
        &subtract3(scope, &exact_at(scope, source, c), &p),
    );
    let constant = dot(scope, &normal, &p);
    (normal, constant)
}

fn triple_plane_homogeneous(source: &Source, planes: [i64; 9]) -> Option<Homogeneous> {
    let scope = &scratch::Scope::new();
    let (first_normal, first_constant) = plane(scope, source, planes[0], planes[1], planes[2]);
    let (second_normal, second_constant) = plane(scope, source, planes[3], planes[4], planes[5]);
    let (third_normal, third_constant) = plane(scope, source, planes[6], planes[7], planes[8]);
    let second_cross_third = cross(scope, &second_normal, &third_normal);
    let third_cross_first = cross(scope, &third_normal, &first_normal);
    let first_cross_second = cross(scope, &first_normal, &second_normal);
    let hw = dot(scope, &first_normal, &second_cross_third);
    if scope.is_zero(&hw) {
        return None;
    }
    let (hx, hy, hz) = add3(
        scope,
        &add3(
            scope,
            &scale3(scope, &first_constant, &second_cross_third),
            &scale3(scope, &second_constant, &third_cross_first),
        ),
        &scale3(scope, &third_constant, &first_cross_second),
    );
    Some(owned(scope, [hx, hy, hz, hw]))
}

#[allow(clippy::too_many_arguments)]
pub fn triple_plane(
    source: &Source,
    first_a: i64,
    first_b: i64,
    first_c: i64,
    second_a: i64,
    second_b: i64,
    second_c: i64,
    third_a: i64,
    third_b: i64,
    third_c: i64,
) -> Result<T, Error> {
    let planes = [first_a, first_b, first_c, second_a, second_b, second_c, third_a, third_b, third_c];
    if let Some(index) = first_invalid(source, &planes) {
        return Err(Error::IndexOutOfBounds(index));
    }
    match triple_plane_homogeneous(source, planes) {
        None => Err(Error::DependentPlanes),
        Some(homogeneous) => make(
            source,
            Construction::TriplePlane {
                first_a,
                first_b,
                first_c,
                second_a,
                second_b,
                second_c,
                third_a,
                third_b,
                third_c,
            },
            homogeneous,
        ),
    }
}

#[derive(Clone)]
struct Rational {
    numerator: scratch::V,
    denominator: scratch::V,
}

fn rational_add(scope: &scratch::Scope, left: &Rational, right: &Rational) -> Rational {
    Rational {
        numerator: scope.add(
            &scope.multiply(&left.numerator, &right.denominator),
            &scope.multiply(&right.numerator, &left.denominator),
        ),
        denominator: scope.multiply(&left.denominator, &right.denominator),
    }
}

fn rational_subtract(scope: &scratch::Scope, left: &Rational, right: &Rational) -> Rational {
    Rational {
        numerator: scope.subtract(
            &scope.multiply(&left.numerator, &right.denominator),
            &scope.multiply(&right.numerator, &left.denominator),
        ),
        denominator: scope.multiply(&left.denominator, &right.denominator),
    }
}

fn rational_multiply(scope: &scratch::Scope, left: &Rational, right: &Rational) -> Rational {
    Rational {
        numerator: scope.multiply(&left.numerator, &right.numerator),
        denominator: scope.multiply(&left.denominator, &right.denominator),
    }
}

fn rational_divide(scope: &scratch::Scope, left: &Rational, right: &Rational) -> Rational {
    Rational {
        numerator: scope.multiply(&left.numerator, &right.denominator),
        denominator: scope.multiply(&left.denominator, &right.numerator),
    }
}

fn rational_scale(scope: &scratch::Scope, value: &Rational, scalar: &scratch::V) -> Rational {
    Rational {
        numerator: scope.multiply(&value.numerator, scalar),
        denominator: value.denominator,
    }
}

fn rational_integer(scope: &scratch::Scope, value: i64) -> Rational {
    Rational {
        numerator: scope.of_float(value as f64),
        denominator: scope.of_float(1.),
    }
}

fn rational_square(scope: &scratch::Scope, value: &Rational) -> Rational {
    rational_multiply(scope, value, value)
}

fn circumcenter_homogeneous(
    first: &Homogeneous,
    second: &Homogeneous,
    third: &Homogeneous,
) -> Result<Homogeneous, Error> {
    let scope = &scratch::Scope::new();
    let of = |point: &Homogeneous, component: &exact::T| Rational {
        numerator: scope.of_exact(component),
        denominator: scope.of_exact(&point.hw),
    };
    let ax = of(first, &first.hx);
    let ay = of(first, &first.hy);
    let az = of(first, &first.hz);
    let bx = of(second, &second.hx);
    let by = of(second, &second.hy);
    let bz = of(second, &second.hz);
    let cx = of(third, &third.hx);
    let cy = of(third, &third.hy);
    let _cz = of(third, &third.hz);
    let norm = |x: &Rational, y: &Rational| {
        rational_add(
            scope,
            &rational_square(scope, x),
            &rational_square(scope, y),
        )
    };
    let an = norm(&ax, &ay);
    let bn = norm(&bx, &by);
    let cn = norm(&cx, &cy);
    let denominator = rational_scale(
        scope,
        &rational_add(
            scope,
            &rational_add(
                scope,
                &rational_multiply(scope, &ax, &rational_subtract(scope, &by, &cy)),
                &rational_multiply(scope, &bx, &rational_subtract(scope, &cy, &ay)),
            ),
            &rational_multiply(scope, &cx, &rational_subtract(scope, &ay, &by)),
        ),
        &scope.of_float(2.),
    );
    if scope.is_zero(&denominator.numerator) {
        return Err(Error::DegenerateTriangle);
    }
    let ux = rational_divide(
        scope,
        &rational_add(
            scope,
            &rational_add(
                scope,
                &rational_multiply(scope, &an, &rational_subtract(scope, &by, &cy)),
                &rational_multiply(scope, &bn, &rational_subtract(scope, &cy, &ay)),
            ),
            &rational_multiply(scope, &cn, &rational_subtract(scope, &ay, &by)),
        ),
        &denominator,
    );
    let uy = rational_divide(
        scope,
        &rational_add(
            scope,
            &rational_add(
                scope,
                &rational_multiply(scope, &an, &rational_subtract(scope, &cx, &bx)),
                &rational_multiply(scope, &bn, &rational_subtract(scope, &ax, &cx)),
            ),
            &rational_multiply(scope, &cn, &rational_subtract(scope, &bx, &ax)),
        ),
        &denominator,
    );
    let uz = rational_divide(
        scope,
        &rational_add(scope, &rational_add(scope, &az, &bz), &of(third, &third.hz)),
        &rational_integer(scope, 3),
    );
    let xy_denominator = scope.multiply(&ux.denominator, &uy.denominator);
    Ok(owned(
        scope,
        [
            scope.multiply(
                &ux.numerator,
                &scope.multiply(&uy.denominator, &uz.denominator),
            ),
            scope.multiply(
                &uy.numerator,
                &scope.multiply(&ux.denominator, &uz.denominator),
            ),
            scope.multiply(&uz.numerator, &xy_denominator),
            scope.multiply(&xy_denominator, &uz.denominator),
        ],
    ))
}

fn compute_homogeneous(source: &Source, construction: &Construction) -> Homogeneous {
    match construction {
        Construction::Explicit(index) => {
            let i = *index as usize;
            Homogeneous {
                hx: exact::of_float(source.data.x[i]),
                hy: exact::of_float(source.data.y[i]),
                hz: exact::of_float(source.data.z[i]),
                hw: exact::of_float(1.),
            }
        }
        Construction::Rounded { x, y, z } => Homogeneous {
            hx: exact::of_float(*x),
            hy: exact::of_float(*y),
            hz: exact::of_float(*z),
            hw: exact::of_float(1.),
        },
        Construction::LinePlane {
            line_start,
            line_end,
            plane_a,
            plane_b,
            plane_c,
        } => line_plane_homogeneous(source, *line_start, *line_end, *plane_a, *plane_b, *plane_c)
            .expect("intersecting line and plane"),
        Construction::LineLine {
            projection,
            first_start,
            first_end,
            second_start,
            second_end,
        } => {
            match line_line_homogeneous(
                source,
                *projection,
                *first_start,
                *first_end,
                *second_start,
                *second_end,
            ) {
                Ok(homogeneous) => homogeneous,
                Err(_) => unreachable!(),
            }
        }
        Construction::TriplePlane {
            first_a,
            first_b,
            first_c,
            second_a,
            second_b,
            second_c,
            third_a,
            third_b,
            third_c,
        } => {
            let planes = [
                *first_a, *first_b, *first_c, *second_a, *second_b, *second_c, *third_a, *third_b,
                *third_c,
            ];
            normalize(triple_plane_homogeneous(source, planes).expect("independent planes"))
        }
        Construction::Midpoint { first, second } => {
            let first = exact_homogeneous(first);
            let second = exact_homogeneous(second);
            let scope = &scratch::Scope::new();
            let first = imported(scope, first);
            let second = imported(scope, second);
            owned(
                scope,
                [
                    scope.add(
                        &scope.multiply(&first[0], &second[3]),
                        &scope.multiply(&second[0], &first[3]),
                    ),
                    scope.add(
                        &scope.multiply(&first[1], &second[3]),
                        &scope.multiply(&second[1], &first[3]),
                    ),
                    scope.add(
                        &scope.multiply(&first[2], &second[3]),
                        &scope.multiply(&second[2], &first[3]),
                    ),
                    scope.multiply(&scope.of_float(2.), &scope.multiply(&first[3], &second[3])),
                ],
            )
        }
        Construction::Centroid3 {
            first,
            second,
            third,
        } => {
            let first = exact_homogeneous(first);
            let second = exact_homogeneous(second);
            let third = exact_homogeneous(third);
            let scope = &scratch::Scope::new();
            let first = imported(scope, first);
            let second = imported(scope, second);
            let third = imported(scope, third);
            let second_third = scope.multiply(&second[3], &third[3]);
            let first_third = scope.multiply(&first[3], &third[3]);
            let first_second = scope.multiply(&first[3], &second[3]);
            let component = |first_component: &scratch::V,
                             second_component: &scratch::V,
                             third_component: &scratch::V| {
                scope.add(
                    &scope.add(
                        &scope.multiply(first_component, &second_third),
                        &scope.multiply(second_component, &first_third),
                    ),
                    &scope.multiply(third_component, &first_second),
                )
            };
            owned(
                scope,
                [
                    component(&first[0], &second[0], &third[0]),
                    component(&first[1], &second[1], &third[1]),
                    component(&first[2], &second[2], &third[2]),
                    scope.multiply(
                        &scope.of_float(3.),
                        &scope.multiply(&first[3], &second_third),
                    ),
                ],
            )
        }
        Construction::Circumcenter2Xy {
            first,
            second,
            third,
        } => {
            match circumcenter_homogeneous(
                exact_homogeneous(first),
                exact_homogeneous(second),
                exact_homogeneous(third),
            ) {
                Ok(value) => value,
                Err(_) => unreachable!(),
            }
        }
    }
}

fn exact_homogeneous(point: &T) -> &Homogeneous {
    point
        .node
        .homogeneous
        .get_or_init(|| compute_homogeneous(&point.node.source, &point.node.construction))
}

pub fn centroid3(first: &T, second: &T, third: &T) -> Result<T, Error> {
    if !same_source_ref(&first.node.source, &second.node.source)
        || !same_source_ref(&first.node.source, &third.node.source)
    {
        return Err(Error::PointSourceMismatch);
    }
    let construction =
        Construction::Centroid3 { first: first.clone(), second: second.clone(), third: third.clone() };
    let homogeneous = compute_homogeneous(&first.node.source, &construction);
    make(&first.node.source, construction, homogeneous)
}

pub fn midpoint(first: &T, second: &T) -> Result<T, Error> {
    if !same_source_ref(&first.node.source, &second.node.source) {
        return Err(Error::PointSourceMismatch);
    }
    let construction = Construction::Midpoint { first: first.clone(), second: second.clone() };
    let homogeneous = compute_homogeneous(&first.node.source, &construction);
    make(&first.node.source, construction, homogeneous)
}

/// Finite Cartesian approximation for acceleration and presentation only.
pub fn approximate(point: &T) -> (f64, f64, f64) {
    let a = &point.node.approximate;
    (a[0], a[1], a[2])
}

/// Certified Cartesian intervals, primarily for kernel diagnostics.
pub fn bounds(point: &T) -> ((f64, f64), (f64, f64), (f64, f64)) {
    let (l, u) = (&point.node.lower, &point.node.upper);
    ((l[0], u[0]), (l[1], u[1]), (l[2], u[2]))
}

fn ensure_same_source(left: &T, right: &T) {
    if !same_source_ref(&left.node.source, &right.node.source) {
        unreachable!("Rdk implicit points belong to different coordinate sources");
    }
}

fn sign_of_comparison(comparison: i64) -> Sign {
    if comparison < 0 {
        Sign::Negative
    } else if comparison > 0 {
        Sign::Positive
    } else {
        Sign::Zero
    }
}

fn component(h: &Homogeneous, axis: usize) -> &exact::T {
    match axis {
        0 => &h.hx,
        1 => &h.hy,
        _ => &h.hz,
    }
}

/// Exact sign of `(point-first) dot (point-second)` in XY. A negative result means `point`
/// strictly encroaches the segment's open diametral disk.
pub fn diametral_dot_xy(first: &T, second: &T, point: &T) -> Sign {
    ensure_same_source(first, second);
    ensure_same_source(first, point);
    let first = exact_homogeneous(first);
    let second = exact_homogeneous(second);
    let point = exact_homogeneous(point);
    let scope = &scratch::Scope::new();
    let difference = |axis: usize, endpoint: &Homogeneous| {
        scope.subtract(
            &scope.multiply(
                &scope.of_exact(component(point, axis)),
                &scope.of_exact(&endpoint.hw),
            ),
            &scope.multiply(
                &scope.of_exact(component(endpoint, axis)),
                &scope.of_exact(&point.hw),
            ),
        )
    };
    let pfx = difference(0, first);
    let pfy = difference(1, first);
    let psx = difference(0, second);
    let psy = difference(1, second);
    let value = scope.add(&scope.multiply(&pfx, &psx), &scope.multiply(&pfy, &psy));
    sign_of_comparison(scope.compare_zero(&value))
}

fn compare_axis(axis: usize, left: &T, right: &T) -> i64 {
    if same_point(left, right) {
        return 0;
    }
    ensure_same_source(left, right);
    let (l, r) = (&left.node, &right.node);
    if l.upper[axis] < r.lower[axis] {
        -1
    } else if l.lower[axis] > r.upper[axis] {
        1
    } else if l.lower[axis] == l.upper[axis]
        && r.lower[axis] == r.upper[axis]
        && l.lower[axis] == r.lower[axis]
    {
        0
    } else {
        let left_h = exact_homogeneous(left);
        let right_h = exact_homogeneous(right);
        scratch::compare_products(component(left_h, axis), &right_h.hw, component(right_h, axis), &left_h.hw)
    }
}

/// Exact homogeneous comparison returning -1, 0, or 1.
pub fn compare_x(left: &T, right: &T) -> i64 {
    compare_axis(0, left, right)
}

pub fn compare_y(left: &T, right: &T) -> i64 {
    compare_axis(1, left, right)
}

pub fn compare_z(left: &T, right: &T) -> i64 {
    compare_axis(2, left, right)
}

pub fn equal(left: &T, right: &T) -> bool {
    same_point(left, right)
        || (compare_x(left, right) == 0 && compare_y(left, right) == 0 && compare_z(left, right) == 0)
}

pub fn source_triangle_projection(source: &Source, a: i64, b: i64, c: i64) -> Projection {
    if !(valid_index(source, a) && valid_index(source, b) && valid_index(source, c)) {
        unreachable!("implicit source triangle index is out of bounds");
    }
    let d = &source.data;
    let (a, b, c) = (a as usize, b as usize, c as usize);
    if predicates::orient2d(d.x[a], d.y[a], d.x[b], d.y[b], d.x[c], d.y[c]) != Sign::Zero {
        Projection::XY
    } else if predicates::orient2d(d.y[a], d.z[a], d.y[b], d.z[b], d.y[c], d.z[c]) != Sign::Zero {
        Projection::YZ
    } else if predicates::orient2d(d.z[a], d.x[a], d.z[b], d.x[b], d.z[c], d.x[c]) != Sign::Zero {
        Projection::ZX
    } else {
        unreachable!("implicit source triangle is exactly degenerate")
    }
}

fn compare_homogeneous_float_bound(
    numerator: &exact::T,
    denominator: &exact::T,
    lower: f64,
    upper: f64,
    value: f64,
) -> i64 {
    if upper < value {
        -1
    } else if lower > value {
        1
    } else {
        scratch::compare_homogeneous_float(numerator, denominator, value)
    }
}

fn between_homogeneous(
    numerator: &exact::T,
    denominator: &exact::T,
    lower: f64,
    upper: f64,
    first: f64,
    second: f64,
) -> bool {
    if first <= second {
        compare_homogeneous_float_bound(numerator, denominator, lower, upper, first) >= 0
            && compare_homogeneous_float_bound(numerator, denominator, lower, upper, second) <= 0
    } else {
        compare_homogeneous_float_bound(numerator, denominator, lower, upper, second) >= 0
            && compare_homogeneous_float_bound(numerator, denominator, lower, upper, first) <= 0
    }
}

/// Exact source-feature ancestry query: the segment test accepts any implicit point over the
/// same source and includes both endpoints.
pub fn source_segment_contains(
    source: &Source,
    projection: Projection,
    first: i64,
    second: i64,
    point: &T,
) -> bool {
    if !(valid_index(source, first) && valid_index(source, second)) {
        unreachable!("implicit source segment index is out of bounds");
    }
    if !same_source_ref(&point.node.source, source) {
        unreachable!("Rdk implicit point and source segment belong to different sources");
    }
    let homogeneous = exact_homogeneous(point);
    let d = &source.data;
    let (f, s) = (first as usize, second as usize);
    let orientation = match projection {
        Projection::XY => scratch::orient2d_ee_homogeneous(
            d.x[f], d.y[f], d.x[s], d.y[s], &homogeneous.hx, &homogeneous.hy, &homogeneous.hw,
        ),
        Projection::YZ => scratch::orient2d_ee_homogeneous(
            d.y[f], d.z[f], d.y[s], d.z[s], &homogeneous.hy, &homogeneous.hz, &homogeneous.hw,
        ),
        Projection::ZX => scratch::orient2d_ee_homogeneous(
            d.z[f], d.x[f], d.z[s], d.x[s], &homogeneous.hz, &homogeneous.hx, &homogeneous.hw,
        ),
    };
    if orientation != 0 {
        return false;
    }
    let n = &point.node;
    if d.x[f] != d.x[s] {
        between_homogeneous(&homogeneous.hx, &homogeneous.hw, n.lower[0], n.upper[0], d.x[f], d.x[s])
    } else if d.y[f] != d.y[s] {
        between_homogeneous(&homogeneous.hy, &homogeneous.hw, n.lower[1], n.upper[1], d.y[f], d.y[s])
    } else {
        between_homogeneous(&homogeneous.hz, &homogeneous.hw, n.lower[2], n.upper[2], d.z[f], d.z[s])
    }
}

/// Exact-construction barycentric weights against three explicit source points.
pub fn barycentric_source_triangle(source: &Source, a: i64, b: i64, c: i64, point: &T) -> (f64, f64, f64) {
    if !(valid_index(source, a) && valid_index(source, b) && valid_index(source, c)) {
        unreachable!("implicit source triangle index is out of bounds");
    }
    if !same_source_ref(&point.node.source, source) {
        unreachable!("Rdk implicit point and source triangle belong to different sources");
    }
    let homogeneous = exact_homogeneous(point);
    let d = &source.data;
    let (a, b, c) = (a as usize, b as usize, c as usize);
    scratch::barycentric_tri_homogeneous(
        d.x[a], d.y[a], d.z[a], d.x[b], d.y[b], d.z[b], d.x[c], d.y[c], d.z[c], &homogeneous.hx,
        &homogeneous.hy, &homogeneous.hz, &homogeneous.hw,
    )
}

fn exact_orient2d_components(first_axis: usize, second_axis: usize, a: &T, b: &T, c: &T) -> i64 {
    let ah = exact_homogeneous(a);
    let bh = exact_homogeneous(b);
    let ch = exact_homogeneous(c);
    scratch::orient2d_homogeneous(
        component(ah, first_axis),
        component(ah, second_axis),
        &ah.hw,
        component(bh, first_axis),
        component(bh, second_axis),
        &bh.hw,
        component(ch, first_axis),
        component(ch, second_axis),
        &ch.hw,
    )
}

fn radial_dot_arena_exact(edge_start: &T, edge_end: &T, left: &T, right: &T) -> Sign {
    ensure_same_source(edge_start, edge_end);
    ensure_same_source(edge_start, left);
    ensure_same_source(edge_start, right);
    let s = exact_homogeneous(edge_start);
    let e = exact_homogeneous(edge_end);
    let l = exact_homogeneous(left);
    let r = exact_homogeneous(right);
    sign_of_comparison(scratch::radial_dot_homogeneous(
        &s.hx, &s.hy, &s.hz, &s.hw, &e.hx, &e.hy, &e.hz, &e.hw, &l.hx, &l.hy, &l.hz, &l.hw, &r.hx,
        &r.hy, &r.hz, &r.hw,
    ))
}

/// Exact sign of the dot product between the last two vectors after projection perpendicular to
/// the directed first-two-point edge.
pub fn radial_dot(edge_start: &T, edge_end: &T, left: &T, right: &T) -> Sign {
    ensure_same_source(edge_start, edge_end);
    ensure_same_source(edge_start, left);
    ensure_same_source(edge_start, right);
    let factor = 16. * f64::EPSILON;
    let smallest = MINIMUM_SUBNORMAL;
    let (s, e, l, r) = (&edge_start.node, &edge_end.node, &left.node, &right.node);
    let mut ev = [0.; 3];
    let mut lv = [0.; 3];
    let mut rv = [0.; 3];
    let mut ev_error = [0.; 3];
    let mut lv_error = [0.; 3];
    let mut rv_error = [0.; 3];
    for i in 0..3 {
        ev[i] = e.approximate[i] - s.approximate[i];
        lv[i] = l.approximate[i] - s.approximate[i];
        rv[i] = r.approximate[i] - s.approximate[i];
        ev_error[i] =
            difference_error(e.error[i], s.error[i], e.approximate[i], s.approximate[i], factor, smallest);
        lv_error[i] =
            difference_error(l.error[i], s.error[i], l.approximate[i], s.approximate[i], factor, smallest);
        rv_error[i] =
            difference_error(r.error[i], s.error[i], r.approximate[i], s.approximate[i], factor, smallest);
    }
    // dot_error: sum of product errors + factor * (|px| + |py| + |pz|) + smallest
    let dot_error = |a: &[f64; 3], a_error: &[f64; 3], b: &[f64; 3], b_error: &[f64; 3], p: &[f64; 3]| {
        product_error(a[0], a_error[0], b[0], b_error[0], p[0], factor, smallest)
            + product_error(a[1], a_error[1], b[1], b_error[1], p[1], factor, smallest)
            + product_error(a[2], a_error[2], b[2], b_error[2], p[2], factor, smallest)
            + (factor * (p[0].abs() + p[1].abs() + p[2].abs()))
            + smallest
    };
    let parts = |a: &[f64; 3], b: &[f64; 3]| [a[0] * b[0], a[1] * b[1], a[2] * b[2]];
    let lr_p = parts(&lv, &rv);
    let ee_p = parts(&ev, &ev);
    let le_p = parts(&lv, &ev);
    let re_p = parts(&rv, &ev);
    let lr = lr_p[0] + lr_p[1] + lr_p[2];
    let ee = ee_p[0] + ee_p[1] + ee_p[2];
    let le = le_p[0] + le_p[1] + le_p[2];
    let re = re_p[0] + re_p[1] + re_p[2];
    let lr_error = dot_error(&lv, &lv_error, &rv, &rv_error, &lr_p);
    let ee_error = dot_error(&ev, &ev_error, &ev, &ev_error, &ee_p);
    let le_error = dot_error(&lv, &lv_error, &ev, &ev_error, &le_p);
    let re_error = dot_error(&rv, &rv_error, &ev, &ev_error, &re_p);
    let first = lr * ee;
    let second = le * re;
    let first_error = product_error(lr, lr_error, ee, ee_error, first, factor, smallest);
    let second_error = product_error(le, le_error, re, re_error, second, factor, smallest);
    let determinant = first - second;
    let determinant_error =
        upward(first_error + second_error + (factor * (first.abs() + second.abs())) + smallest);
    if determinant.is_finite() && determinant_error.is_finite() && determinant.abs() > determinant_error
    {
        if determinant < 0. { Sign::Negative } else { Sign::Positive }
    } else {
        radial_dot_arena_exact(edge_start, edge_end, left, right)
    }
}

/// Exact sign for the positive-infinitesimal direction `(1, epsilon, epsilon^2)`.
pub fn normal_dot_symbolic(first: &T, second: &T, third: &T) -> Sign {
    ensure_same_source(first, second);
    ensure_same_source(first, third);
    let f = exact_homogeneous(first);
    let s = exact_homogeneous(second);
    let t = exact_homogeneous(third);
    sign_of_comparison(scratch::normal_dot_symbolic_homogeneous(
        &f.hx, &f.hy, &f.hz, &f.hw, &s.hx, &s.hy, &s.hz, &s.hw, &t.hx, &t.hy, &t.hz, &t.hw,
    ))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolicRayTriangle {
    SymbolicMiss,
    SymbolicHit(Sign),
    SymbolicOriginBoundary,
    SymbolicDegenerate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AxisRayTriangle {
    AxisMiss,
    AxisHit(Sign),
    AxisBoundary,
    AxisParallel,
}

pub fn symbolic_ray_source_triangle(
    source: &Source,
    a: i64,
    b: i64,
    c: i64,
    query: &T,
) -> SymbolicRayTriangle {
    if !(valid_index(source, a) && valid_index(source, b) && valid_index(source, c)) {
        unreachable!("implicit symbolic source triangle index is out of bounds");
    }
    if !same_source_ref(&query.node.source, source) {
        unreachable!("Rdk implicit point and symbolic source triangle belong to different sources");
    }
    let q = exact_homogeneous(query);
    let d = &source.data;
    let (a, b, c) = (a as usize, b as usize, c as usize);
    match scratch::symbolic_ray_tri_homogeneous(
        d.x[a], d.y[a], d.z[a], d.x[b], d.y[b], d.z[b], d.x[c], d.y[c], d.z[c], &q.hx, &q.hy, &q.hz,
        &q.hw,
    ) {
        0 => SymbolicRayTriangle::SymbolicMiss,
        1 => SymbolicRayTriangle::SymbolicHit(Sign::Negative),
        2 => SymbolicRayTriangle::SymbolicHit(Sign::Positive),
        3 => SymbolicRayTriangle::SymbolicOriginBoundary,
        4 => SymbolicRayTriangle::SymbolicDegenerate,
        _ => unreachable!(),
    }
}

fn filter_subtraction_error(left: f64, right: f64, right_error: f64) -> f64 {
    right_error + (4. * f64::EPSILON * (left.abs() + right.abs())) + MINIMUM_SUBNORMAL
}

fn filter_product_error(left: f64, left_error: f64, right: f64, right_error: f64, product: f64) -> f64 {
    (left.abs() * right_error)
        + (right.abs() * left_error)
        + (left_error * right_error)
        + (4. * f64::EPSILON * product.abs())
        + MINIMUM_SUBNORMAL
}

#[allow(clippy::too_many_arguments)]
fn filtered_projected_edge(
    au: f64,
    av: f64,
    bu: f64,
    bv: f64,
    qu: f64,
    qv: f64,
    qu_error: f64,
    qv_error: f64,
) -> i64 {
    let acu = au - qu;
    let bcu = bu - qu;
    let acv = av - qv;
    let bcv = bv - qv;
    let acu_error = filter_subtraction_error(au, qu, qu_error);
    let bcu_error = filter_subtraction_error(bu, qu, qu_error);
    let acv_error = filter_subtraction_error(av, qv, qv_error);
    let bcv_error = filter_subtraction_error(bv, qv, qv_error);
    let left = acu * bcv;
    let right = acv * bcu;
    let left_error = filter_product_error(acu, acu_error, bcv, bcv_error, left);
    let right_error = filter_product_error(acv, acv_error, bcu, bcu_error, right);
    let determinant = left - right;
    let error = upward(
        left_error
            + right_error
            + (4. * f64::EPSILON * (left.abs() + right.abs()))
            + MINIMUM_SUBNORMAL,
    );
    if determinant.is_finite() && error.is_finite() && determinant.abs() > error {
        if determinant < 0. { -1 } else { 1 }
    } else {
        0
    }
}

fn filtered_axis_edge(source: &Source, query: &Node, axis: i64, first: i64, second: i64) -> i64 {
    let d = &source.data;
    let (f, s) = (first as usize, second as usize);
    let (qa, qe) = (&query.approximate, &query.error);
    if axis == 0 {
        filtered_projected_edge(d.y[f], d.z[f], d.y[s], d.z[s], qa[1], qa[2], qe[1], qe[2])
    } else if axis == 1 {
        filtered_projected_edge(d.z[f], d.x[f], d.z[s], d.x[s], qa[2], qa[0], qe[2], qe[0])
    } else {
        filtered_projected_edge(d.x[f], d.y[f], d.x[s], d.y[s], qa[0], qa[1], qe[0], qe[1])
    }
}

/// `(determinant, error)` of `ad . (bd x cd)` from rows with per-component error bounds, using
/// `error_for_minor` (the minor/difference factor) and `error_for_term` (the product factor).
fn determinant3(
    ad: &[f64; 3],
    ad_error: &[f64; 3],
    bd: &[f64; 3],
    bd_error: &[f64; 3],
    cd: &[f64; 3],
    cd_error: &[f64; 3],
) -> (f64, f64) {
    let factor = 4. * f64::EPSILON;
    let smallest = MINIMUM_SUBNORMAL;
    let mut minor = [0.; 3];
    let mut minor_error = [0.; 3];
    for i in 0..3 {
        let (j, k) = ((i + 1) % 3, (i + 2) % 3);
        let left = bd[j] * cd[k];
        let right = bd[k] * cd[j];
        let left_error = product_error(bd[j], bd_error[j], cd[k], cd_error[k], left, factor, smallest);
        let right_error = product_error(bd[k], bd_error[k], cd[j], cd_error[j], right, factor, smallest);
        minor[i] = left - right;
        minor_error[i] = difference_error(left_error, right_error, left, right, factor, smallest);
    }
    let first = ad[0] * minor[0];
    let second = ad[1] * minor[1];
    let third = ad[2] * minor[2];
    let first_error = product_error(ad[0], ad_error[0], minor[0], minor_error[0], first, factor, smallest);
    let second_error = product_error(ad[1], ad_error[1], minor[1], minor_error[1], second, factor, smallest);
    let third_error = product_error(ad[2], ad_error[2], minor[2], minor_error[2], third, factor, smallest);
    let determinant = first + second + third;
    let error = upward(
        first_error
            + second_error
            + third_error
            + (factor * (first.abs() + second.abs() + third.abs()))
            + smallest,
    );
    (determinant, error)
}

fn filtered_orient3_explicit_query(source: &Source, a: i64, b: i64, c: i64, query: &Node) -> i64 {
    let d = &source.data;
    let pick = |i: i64| -> [f64; 3] {
        let i = i as usize;
        [d.x[i], d.y[i], d.z[i]]
    };
    let rows = [pick(a), pick(b), pick(c)];
    let mut delta = [[0.; 3]; 3];
    let mut delta_error = [[0.; 3]; 3];
    for row in 0..3 {
        for i in 0..3 {
            delta[row][i] = rows[row][i] - query.approximate[i];
            delta_error[row][i] =
                filter_subtraction_error(rows[row][i], query.approximate[i], query.error[i]);
        }
    }
    let (determinant, error) = determinant3(
        &delta[0],
        &delta_error[0],
        &delta[1],
        &delta_error[1],
        &delta[2],
        &delta_error[2],
    );
    if determinant.is_finite() && error.is_finite() && determinant.abs() > error {
        if determinant < 0. { -1 } else { 1 }
    } else {
        0
    }
}

fn exact_axis_ray_source_triangle(
    source: &Source,
    a: i64,
    b: i64,
    c: i64,
    axis: i64,
    positive: bool,
    query: &T,
) -> AxisRayTriangle {
    let q = exact_homogeneous(query);
    let d = &source.data;
    let (a, b, c) = (a as usize, b as usize, c as usize);
    match scratch::axis_ray_tri_homogeneous(
        axis, positive, d.x[a], d.y[a], d.z[a], d.x[b], d.y[b], d.z[b], d.x[c], d.y[c], d.z[c], &q.hx,
        &q.hy, &q.hz, &q.hw,
    ) {
        0 => AxisRayTriangle::AxisMiss,
        1 => AxisRayTriangle::AxisHit(Sign::Negative),
        2 => AxisRayTriangle::AxisHit(Sign::Positive),
        3 => AxisRayTriangle::AxisBoundary,
        4 => AxisRayTriangle::AxisParallel,
        _ => unreachable!(),
    }
}

/// Exact ray/triangle decision for signed coordinate axis 0, 1, or 2. `AxisParallel` means the
/// carrier has zero projected normal for that direction; `AxisBoundary` asks the caller to try
/// another exact direction.
pub fn axis_ray_source_triangle(
    source: &Source,
    a: i64,
    b: i64,
    c: i64,
    axis: i64,
    positive: bool,
    query: &T,
) -> AxisRayTriangle {
    if !(valid_index(source, a) && valid_index(source, b) && valid_index(source, c)) {
        unreachable!("implicit axis-ray source triangle index is out of bounds");
    }
    if !same_source_ref(&query.node.source, source) {
        unreachable!("Rdk implicit point and axis-ray source triangle belong to different sources");
    }
    if !(0..=2).contains(&axis) {
        unreachable!("Rdk exact ray axis is out of bounds");
    }
    let d = &source.data;
    let (first_plane, second_plane) = if axis == 0 {
        (&d.y, &d.z)
    } else if axis == 1 {
        (&d.z, &d.x)
    } else {
        (&d.x, &d.y)
    };
    let (au, bu, cu) = (a as usize, b as usize, c as usize);
    let normal = predicates::orient2d(
        first_plane[au],
        second_plane[au],
        first_plane[bu],
        second_plane[bu],
        first_plane[cu],
        second_plane[cu],
    );
    if normal == Sign::Zero {
        return AxisRayTriangle::AxisParallel;
    }
    let node = &query.node;
    let first = filtered_axis_edge(source, node, axis, a, b);
    let second = filtered_axis_edge(source, node, axis, b, c);
    let third = filtered_axis_edge(source, node, axis, c, a);
    let plane = filtered_orient3_explicit_query(source, a, b, c, node);
    if first == 0 || second == 0 || third == 0 || plane == 0 {
        exact_axis_ray_source_triangle(source, a, b, c, axis, positive, query)
    } else {
        let direction = if positive { 1 } else { -1 };
        let normal = (if normal == Sign::Positive { 1 } else { -1 }) * direction;
        let first = first * direction;
        let second = second * direction;
        let third = third * direction;
        if first != normal || second != normal || third != normal {
            AxisRayTriangle::AxisMiss
        } else if plane != normal {
            AxisRayTriangle::AxisMiss
        } else {
            AxisRayTriangle::AxisHit(if normal < 0 { Sign::Negative } else { Sign::Positive })
        }
    }
}

/// Filtered 2D orientation in the plane of axes `(u, v)`; falls back to the exact predicate.
fn orient2d_axes(u: usize, v: usize, a: &T, b: &T, c: &T) -> Sign {
    ensure_same_source(a, b);
    ensure_same_source(a, c);
    let (an, bn, cn) = (&a.node, &b.node, &c.node);
    let acu = an.approximate[u] - cn.approximate[u];
    let bcu = bn.approximate[u] - cn.approximate[u];
    let acv = an.approximate[v] - cn.approximate[v];
    let bcv = bn.approximate[v] - cn.approximate[v];
    let subtraction_factor = 4. * f64::EPSILON;
    let smallest = MINIMUM_SUBNORMAL;
    let acu_error = difference_error(
        an.error[u], cn.error[u], an.approximate[u], cn.approximate[u], subtraction_factor, smallest,
    );
    let bcu_error = difference_error(
        bn.error[u], cn.error[u], bn.approximate[u], cn.approximate[u], subtraction_factor, smallest,
    );
    let acv_error = difference_error(
        an.error[v], cn.error[v], an.approximate[v], cn.approximate[v], subtraction_factor, smallest,
    );
    let bcv_error = difference_error(
        bn.error[v], cn.error[v], bn.approximate[v], cn.approximate[v], subtraction_factor, smallest,
    );
    let left = acu * bcv;
    let right = acv * bcu;
    let product_factor = 4. * f64::EPSILON;
    let left_error = product_error(acu, acu_error, bcv, bcv_error, left, product_factor, smallest);
    let right_error = product_error(acv, acv_error, bcu, bcu_error, right, product_factor, smallest);
    let determinant = left - right;
    let error = upward(
        left_error + right_error + (product_factor * (left.abs() + right.abs())) + smallest,
    );
    if determinant.is_finite() && error.is_finite() && determinant.abs() > error {
        if determinant < 0. { Sign::Negative } else { Sign::Positive }
    } else {
        sign_of_comparison(exact_orient2d_components(u, v, a, b, c))
    }
}

pub fn orient2d_xy(a: &T, b: &T, c: &T) -> Sign {
    orient2d_axes(0, 1, a, b, c)
}

pub fn orient2d_yz(a: &T, b: &T, c: &T) -> Sign {
    orient2d_axes(1, 2, a, b, c)
}

pub fn orient2d_zx(a: &T, b: &T, c: &T) -> Sign {
    orient2d_axes(2, 0, a, b, c)
}

/// Exact predicate supporting any mixture of explicit, LPI, and TPI points created from the same
/// packed source.
pub fn orient3d(a: &T, b: &T, c: &T, d: &T) -> Sign {
    ensure_same_source(a, b);
    ensure_same_source(a, c);
    ensure_same_source(a, d);
    let subtraction_factor = 4. * f64::EPSILON;
    let smallest = MINIMUM_SUBNORMAL;
    let (an, bn, cn, dn) = (&a.node, &b.node, &c.node, &d.node);
    let rows = [an, bn, cn];
    let mut delta = [[0.; 3]; 3];
    let mut delta_error = [[0.; 3]; 3];
    for row in 0..3 {
        for i in 0..3 {
            delta[row][i] = rows[row].approximate[i] - dn.approximate[i];
            delta_error[row][i] = difference_error(
                rows[row].error[i],
                dn.error[i],
                rows[row].approximate[i],
                dn.approximate[i],
                subtraction_factor,
                smallest,
            );
        }
    }
    let (determinant, error) = determinant3(
        &delta[0],
        &delta_error[0],
        &delta[1],
        &delta_error[1],
        &delta[2],
        &delta_error[2],
    );
    if determinant.is_finite() && error.is_finite() && determinant.abs() > error {
        if determinant < 0. { Sign::Negative } else { Sign::Positive }
    } else {
        let ah = exact_homogeneous(a);
        let bh = exact_homogeneous(b);
        let ch = exact_homogeneous(c);
        let dh = exact_homogeneous(d);
        sign_of_comparison(scratch::orient3d_homogeneous(
            &ah.hx, &ah.hy, &ah.hz, &ah.hw, &bh.hx, &bh.hy, &bh.hz, &bh.hw, &ch.hx, &ch.hy, &ch.hz,
            &ch.hw, &dh.hx, &dh.hy, &dh.hz, &dh.hw,
        ))
    }
}

fn incircle_components(first_axis: usize, second_axis: usize, a: &T, b: &T, c: &T, d: &T) -> Sign {
    ensure_same_source(a, b);
    ensure_same_source(a, c);
    ensure_same_source(a, d);
    let ah = exact_homogeneous(a);
    let bh = exact_homogeneous(b);
    let ch = exact_homogeneous(c);
    let dh = exact_homogeneous(d);
    sign_of_comparison(scratch::incircle_homogeneous(
        component(ah, first_axis),
        component(ah, second_axis),
        &ah.hw,
        component(bh, first_axis),
        component(bh, second_axis),
        &bh.hw,
        component(ch, first_axis),
        component(ch, second_axis),
        &ch.hw,
        component(dh, first_axis),
        component(dh, second_axis),
        &dh.hw,
    ))
}

/// Exact projected incircle determinant. For a positively oriented first triangle, `Positive`
/// means the fourth point is inside its circumcircle.
fn incircle_projected(first_axis: usize, second_axis: usize, a: &T, b: &T, c: &T, d: &T) -> Sign {
    ensure_same_source(a, b);
    ensure_same_source(a, c);
    ensure_same_source(a, d);
    let (u, v) = (first_axis, second_axis);
    let (an, bn, cn, dn) = (&a.node, &b.node, &c.node, &d.node);
    let a_first = an.approximate[u];
    let a_second = an.approximate[v];
    let b_first = bn.approximate[u];
    let b_second = bn.approximate[v];
    let c_first = cn.approximate[u];
    let c_second = cn.approximate[v];
    let d_first = dn.approximate[u];
    let d_second = dn.approximate[v];
    let a_first_error = an.error[u];
    let a_second_error = an.error[v];
    let b_first_error = bn.error[u];
    let b_second_error = bn.error[v];
    let c_first_error = cn.error[u];
    let c_second_error = cn.error[v];
    let d_first_error = dn.error[u];
    let d_second_error = dn.error[v];
    let adx = a_first - d_first;
    let ady = a_second - d_second;
    let bdx = b_first - d_first;
    let bdy = b_second - d_second;
    let cdx = c_first - d_first;
    let cdy = c_second - d_second;
    let subtraction_factor = 4. * f64::EPSILON;
    let smallest = MINIMUM_SUBNORMAL;
    let adx_error =
        difference_error(a_first_error, d_first_error, a_first, d_first, subtraction_factor, smallest);
    let ady_error =
        difference_error(a_second_error, d_second_error, a_second, d_second, subtraction_factor, smallest);
    let bdx_error =
        difference_error(b_first_error, d_first_error, b_first, d_first, subtraction_factor, smallest);
    let bdy_error =
        difference_error(b_second_error, d_second_error, b_second, d_second, subtraction_factor, smallest);
    let cdx_error =
        difference_error(c_first_error, d_first_error, c_first, d_first, subtraction_factor, smallest);
    let cdy_error =
        difference_error(c_second_error, d_second_error, c_second, d_second, subtraction_factor, smallest);
    let product_factor = 4. * f64::EPSILON;
    let adx2 = adx * adx;
    let ady2 = ady * ady;
    let bdx2 = bdx * bdx;
    let bdy2 = bdy * bdy;
    let cdx2 = cdx * cdx;
    let cdy2 = cdy * cdy;
    // (2 * |x| * x_error) + (x_error * x_error) + (factor * |x2|) + smallest
    let square_error = |x: f64, x_error: f64, x2: f64| {
        (2. * x.abs() * x_error) + (x_error * x_error) + (product_factor * x2.abs()) + smallest
    };
    let adx2_error = square_error(adx, adx_error, adx2);
    let ady2_error = square_error(ady, ady_error, ady2);
    let bdx2_error = square_error(bdx, bdx_error, bdx2);
    let bdy2_error = square_error(bdy, bdy_error, bdy2);
    let cdx2_error = square_error(cdx, cdx_error, cdx2);
    let cdy2_error = square_error(cdy, cdy_error, cdy2);
    let alift = adx2 + ady2;
    let blift = bdx2 + bdy2;
    let clift = cdx2 + cdy2;
    let alift_error = difference_error(adx2_error, ady2_error, adx2, ady2, product_factor, smallest);
    let blift_error = difference_error(bdx2_error, bdy2_error, bdx2, bdy2, product_factor, smallest);
    let clift_error = difference_error(cdx2_error, cdy2_error, cdx2, cdy2, product_factor, smallest);
    let bc_left = bdx * cdy;
    let bc_right = cdx * bdy;
    let ca_left = cdx * ady;
    let ca_right = adx * cdy;
    let ab_left = adx * bdy;
    let ab_right = bdx * ady;
    let bc_left_error = product_error(bdx, bdx_error, cdy, cdy_error, bc_left, product_factor, smallest);
    let bc_right_error = product_error(cdx, cdx_error, bdy, bdy_error, bc_right, product_factor, smallest);
    let ca_left_error = product_error(cdx, cdx_error, ady, ady_error, ca_left, product_factor, smallest);
    let ca_right_error = product_error(adx, adx_error, cdy, cdy_error, ca_right, product_factor, smallest);
    let ab_left_error = product_error(adx, adx_error, bdy, bdy_error, ab_left, product_factor, smallest);
    let ab_right_error = product_error(bdx, bdx_error, ady, ady_error, ab_right, product_factor, smallest);
    let bc = bc_left - bc_right;
    let ca = ca_left - ca_right;
    let ab = ab_left - ab_right;
    let bc_error =
        difference_error(bc_left_error, bc_right_error, bc_left, bc_right, subtraction_factor, smallest);
    let ca_error =
        difference_error(ca_left_error, ca_right_error, ca_left, ca_right, subtraction_factor, smallest);
    let ab_error =
        difference_error(ab_left_error, ab_right_error, ab_left, ab_right, subtraction_factor, smallest);
    let first = alift * bc;
    let second = blift * ca;
    let third = clift * ab;
    let first_error = product_error(alift, alift_error, bc, bc_error, first, product_factor, smallest);
    let second_error = product_error(blift, blift_error, ca, ca_error, second, product_factor, smallest);
    let third_error = product_error(clift, clift_error, ab, ab_error, third, product_factor, smallest);
    let determinant = first + second + third;
    let determinant_error = first_error
        + second_error
        + third_error
        + (8. * f64::EPSILON * (first.abs() + second.abs() + third.abs()))
        + smallest;
    if determinant.is_finite() && determinant_error.is_finite() && determinant.abs() > determinant_error {
        if determinant > 0. { Sign::Positive } else { Sign::Negative }
    } else {
        incircle_components(first_axis, second_axis, a, b, c, d)
    }
}

pub fn incircle_xy(a: &T, b: &T, c: &T, d: &T) -> Sign {
    incircle_projected(0, 1, a, b, c, d)
}

pub fn incircle_yz(a: &T, b: &T, c: &T, d: &T) -> Sign {
    incircle_projected(1, 2, a, b, c, d)
}

pub fn incircle_zx(a: &T, b: &T, c: &T, d: &T) -> Sign {
    incircle_projected(2, 0, a, b, c, d)
}

#[cfg(test)]
mod tests {
    use super::*;

    // OCaml's test_implicit_point.ml defines only this fixture and an empty `run`; the checks
    // below exercise the same coordinates table.
    const COORDINATES: [(f64, f64, f64); 19] = [
        (0., 0., 0.),
        (2., 0., 0.),
        (1., -1., -1.),
        (1., 1., -1.),
        (1., 0., 1.),
        (1., 0., 0.),
        (1., 0., 0.),
        (1., 1., 0.),
        (1., 0., 1.),
        (0., 2., 0.),
        (1., 2., 0.),
        (0., 2., 1.),
        (0., 0., 3.),
        (1., 0., 3.),
        (0., 1., 3.),
        (1., 2., 3.),
        (0., 2., 0.),
        (2., 2., 0.),
        (1., 1., 1.),
    ];

    fn fixture() -> Source {
        source(
            COORDINATES.iter().map(|c| c.0).collect(),
            COORDINATES.iter().map(|c| c.1).collect(),
            COORDINATES.iter().map(|c| c.2).collect(),
        )
        .unwrap()
    }

    #[test]
    fn source_validation() {
        assert_eq!(source(vec![0.], vec![], vec![0.]).unwrap_err(), Error::CoordinatePlaneSizeMismatch);
        assert_eq!(
            source(vec![0., 1.], vec![0., f64::NAN], vec![0., 0.]).unwrap_err(),
            Error::NonFiniteCoordinate(1)
        );
        let s = fixture();
        assert_eq!(explicit(&s, 99).unwrap_err(), Error::IndexOutOfBounds(99));
        assert_eq!(line_plane(&s, 0, 1, 2, 3, 77).unwrap_err(), Error::IndexOutOfBounds(77));
    }

    #[test]
    fn lpi_tpi_midpoint_equal_explicit() {
        let s = fixture();
        let lpi = line_plane(&s, 0, 1, 2, 3, 4).unwrap();
        let e5 = explicit(&s, 5).unwrap();
        assert!(equal(&lpi, &e5));
        assert_eq!(approximate(&lpi), (1., 0., 0.));
        let tpi = triple_plane(&s, 6, 7, 8, 9, 10, 11, 12, 13, 14).unwrap();
        assert!(equal(&tpi, &explicit(&s, 15).unwrap()));
        assert_eq!(approximate(&tpi), (1., 2., 3.));
        let mid = midpoint(&explicit(&s, 0).unwrap(), &explicit(&s, 1).unwrap()).unwrap();
        assert!(equal(&mid, &e5));
        assert_eq!(compare_x(&lpi, &explicit(&s, 1).unwrap()), -1);
        assert_eq!(compare_y(&tpi, &explicit(&s, 15).unwrap()), 0);
        let ll = line_line(&s, Projection::XY, 0, 1, 6, 7).unwrap();
        assert!(equal(&ll, &e5));
    }

    #[test]
    fn errors_and_predicates() {
        let s = fixture();
        assert_eq!(line_plane(&s, 0, 9, 6, 7, 8).unwrap_err(), Error::ParallelLineAndPlane);
        assert_eq!(
            triple_plane(&s, 6, 7, 8, 6, 7, 8, 12, 13, 14).unwrap_err(),
            Error::DependentPlanes
        );
        let other = source(vec![0.], vec![0.], vec![0.]).unwrap();
        let foreign = explicit(&other, 0).unwrap();
        assert_eq!(midpoint(&explicit(&s, 0).unwrap(), &foreign).unwrap_err(), Error::PointSourceMismatch);
        let p = |i| explicit(&s, i).unwrap();
        assert_eq!(orient2d_xy(&p(0), &p(1), &p(17)), Sign::Positive);
        assert_eq!(orient3d(&p(0), &p(1), &p(9), &p(12)), Sign::Negative);
        assert_eq!(source_triangle_projection(&s, 0, 1, 9), Projection::XY);
        assert_eq!(diametral_dot_xy(&p(0), &p(1), &p(5)), Sign::Negative);
        // The exact path for an LPI point agrees with the explicit point it equals.
        let lpi = line_plane(&s, 0, 1, 2, 3, 4).unwrap();
        assert_eq!(orient2d_xy(&p(0), &p(1), &lpi), Sign::Zero);
        assert!(source_segment_contains(&s, Projection::XY, 0, 1, &lpi));
        let w = barycentric_source_triangle(&s, 0, 1, 9, &lpi);
        assert!((w.0 - 0.5).abs() < 1e-14 && (w.1 - 0.5).abs() < 1e-14 && w.2.abs() < 1e-14);
    }
    /// Run with RAYS_EXACT_BENCH_ITERATIONS=512 for the same debug-profile timing workload.
    #[test]
    fn exact_d3_benchmark_and_construction_regression() {
        use std::hint::black_box;
        use std::time::Instant;
        let iterations = std::env::var("RAYS_EXACT_BENCH_ITERATIONS")
            .map(|value| value.parse::<usize>().unwrap()).unwrap_or(16);
        let cases: Vec<[f64; 12]> = (0..48u64).map(|i| std::array::from_fn(|j| {
            let bits = (i * 1_664_525 + j as u64 * 1_013_904_223 + 97).wrapping_mul(6_364_136_223_846_793_005);
            let exponent = [1, 100, 600, 1000, 1400, 2046][(bits >> 16) as usize % 6];
            f64::from_bits((bits & 0x800f_ffff_ffff_ffff) | (exponent << 52))
        })).collect();
        let signs = |v: &[f64; 12]| [
            scratch::orient3d(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11]),
            scratch::incircle(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]),
        ];
        let expected: Vec<_> = cases.iter().map(signs).collect();
        let bytes: Vec<_> = expected.iter().flatten().map(|sign| (sign + 1) as u8).collect();
        assert_eq!(rays_math::hash::md5_hex(&bytes), "351d2fc1d4d1c92afde379899b06cff1");
        let parallel_signs = std::sync::Mutex::new(vec![[0; 2]; cases.len()]);
        parallel::run_unit(Some(4), &|| {
            parallel::for_(8, 0, cases.len() as i64 - 1, &|index| {
                parallel_signs.lock().unwrap()[index as usize] = signs(&cases[index as usize]);
                Ok(())
            })
        }).unwrap();
        assert_eq!(parallel_signs.into_inner().unwrap(), expected, "exact signs are domain-independent");
        let source = fixture();
        let first = explicit(&source, 0).unwrap();
        let second = explicit(&source, 1).unwrap();
        let third = explicit(&source, 17).unwrap();
        let inputs = [exact_homogeneous(&first), exact_homogeneous(&second), exact_homogeneous(&third)];
        let mut predicate_us = Vec::new();
        let mut construction_us = Vec::new();
        for _ in 0..5 {
            let started = Instant::now();
            for i in 0..iterations {
                let index = i % cases.len();
                assert_eq!(signs(black_box(&cases[index])), expected[index]);
            }
            predicate_us.push(started.elapsed().as_micros());
            let started = Instant::now();
            for _ in 0..iterations {
                let center = circumcenter_homogeneous(black_box(inputs[0]), inputs[1], inputs[2]).unwrap();
                assert_eq!(approximate_ratio(&center.hx, &center.hw), 1.);
                assert_eq!(approximate_ratio(&center.hy, &center.hw), 1.);
                assert_eq!(approximate_ratio(&center.hz, &center.hw), 0.);
                black_box(center);
            }
            construction_us.push(started.elapsed().as_micros());
        }
        predicate_us.sort_unstable();
        construction_us.sort_unstable();
        println!("D3 debug medians ({iterations} iterations): exact orient3d+incircle {} us; implicit circumcenter {} us", predicate_us[2], construction_us[2]);
    }

}
