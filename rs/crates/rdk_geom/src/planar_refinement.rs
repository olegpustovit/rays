//! Bounded deterministic quality refinement for a planar constrained Delaunay triangulation.
//!
//! `Invalid_argument` sites that OCaml's top-level handler turns into `Error msg` are validated
//! up front or returned as `Err(msg)`; nothing here relies on a panic.
use std::cmp::Ordering;
use std::sync::Arc;

use rdk_core::{cancel, error};

use crate::bounds2_index;
use crate::implicit_point::{self, Error as PointError};
use crate::planar_cdt;
use crate::predicates::Sign;

const MAX_ARRAY_LENGTH: i64 = (1 << 54) - 1;
const MASK63: u64 = (1u64 << 63) - 1;
const MAX_INT: i64 = (1 << 62) - 1;

type R<V> = Result<V, String>;

pub struct T {
    initial_point_count: i64,
    points: Vec<implicit_point::T>,
    x: Vec<f64>,
    y: Vec<f64>,
    triangles: Arc<[i64]>,
    constraints: Vec<i64>,
    point_roots: Vec<i64>,
    provenance_first: Vec<i64>,
    provenance_second: Vec<i64>,
    provenance_third: Vec<i64>,
    provenance_weight_first: Vec<f64>,
    provenance_weight_second: Vec<f64>,
    provenance_weight_third: Vec<f64>,
}

fn invalid(message: &str) -> error::T {
    match error::make(
        "Planar_refinement".to_owned(),
        "invalid_argument".to_owned(),
        message.to_owned(),
        None,
    ) {
        Ok(value) | Err(value) => value,
    }
}

fn point_count_of(value: &T) -> i64 {
    value.points.len() as i64
}

pub fn new_point_count(value: &T) -> i64 {
    point_count_of(value) - value.initial_point_count
}

pub fn triangle_points(value: &T) -> Vec<i64> {
    value.triangles.to_vec()
}

pub fn constraint_points(value: &T) -> Vec<i64> {
    value.constraints.clone()
}

fn check_new(value: &T, point: i64) -> Result<i64, error::T> {
    if point < value.initial_point_count || point >= point_count_of(value) {
        return Err(invalid("Planar_refinement: generated point is out of range"));
    }
    Ok(point - value.initial_point_count)
}

pub fn point_provenance_ref(value: &T, point: i64) -> Result<i64, error::T> {
    Ok(value.point_roots[check_new(value, point)? as usize])
}

pub fn provenance_node_count(value: &T) -> i64 {
    value.provenance_first.len() as i64
}

fn check_provenance(value: &T, node: i64) -> Result<usize, error::T> {
    if node < 0 || node >= provenance_node_count(value) {
        return Err(invalid("Planar_refinement: provenance node is out of range"));
    }
    Ok(node as usize)
}

pub fn provenance_parent_first(value: &T, node: i64) -> Result<i64, error::T> {
    Ok(value.provenance_first[check_provenance(value, node)?])
}

pub fn provenance_parent_second(value: &T, node: i64) -> Result<i64, error::T> {
    Ok(value.provenance_second[check_provenance(value, node)?])
}

pub fn provenance_parent_third(value: &T, node: i64) -> Result<i64, error::T> {
    Ok(value.provenance_third[check_provenance(value, node)?])
}

pub fn provenance_parent_weights(value: &T, node: i64) -> Result<(f64, f64, f64), error::T> {
    let n = check_provenance(value, node)?;
    Ok((
        value.provenance_weight_first[n],
        value.provenance_weight_second[n],
        value.provenance_weight_third[n],
    ))
}

#[allow(non_snake_case)]
pub mod View {
    use super::T;

    pub fn approximate_x(value: &T) -> &[f64] {
        &value.x
    }

    pub fn approximate_y(value: &T) -> &[f64] {
        &value.y
    }
}

fn omin(a: f64, b: f64) -> f64 {
    if a <= b { a } else { b }
}

fn omax(a: f64, b: f64) -> f64 {
    if a >= b { a } else { b }
}

fn to_ordering(value: i64) -> Ordering {
    value.cmp(&0)
}

fn finite_positive(label: &str, value: Option<f64>) -> R<()> {
    match value {
        None => Ok(()),
        Some(value) if value.is_finite() && value > 0. => Ok(()),
        Some(_) => Err(format!("Planar refinement {} must be finite and positive", label)),
    }
}

fn stable_length(ax: f64, ay: f64, bx: f64, by: f64) -> f64 {
    let dx = bx - ax;
    let dy = by - ay;
    let scale = omax(dx.abs(), dy.abs());
    if scale == 0. {
        0.
    } else if !scale.is_finite() {
        f64::INFINITY
    } else {
        let dx = dx / scale;
        let dy = dy / scale;
        scale * ((dx * dx) + (dy * dy)).sqrt()
    }
}

fn stable_angle(x: &[f64], y: &[f64], opposite: i64, first: i64, second: i64) -> f64 {
    let (o, f, s) = (opposite as usize, first as usize, second as usize);
    let ux = x[f] - x[o];
    let uy = y[f] - y[o];
    let vx = x[s] - x[o];
    let vy = y[s] - y[o];
    let scale = omax(ux.abs(), omax(uy.abs(), omax(vx.abs(), vy.abs())));
    if scale == 0. || !scale.is_finite() {
        0.
    } else {
        let ux = ux / scale;
        let uy = uy / scale;
        let vx = vx / scale;
        let vy = vy / scale;
        ((ux * vy) - (uy * vx)).abs().atan2((ux * vx) + (uy * vy))
    }
}

#[allow(clippy::too_many_arguments)]
fn triangle_requested(
    x: &[f64],
    y: &[f64],
    a: i64,
    b: i64,
    c: i64,
    minimum_angle: Option<f64>,
    maximum_area: Option<f64>,
    target_edge_length: Option<f64>,
    minimum_edge_length: f64,
) -> bool {
    let (au, bu, cu) = (a as usize, b as usize, c as usize);
    let ab = stable_length(x[au], y[au], x[bu], y[bu]);
    let bc = stable_length(x[bu], y[bu], x[cu], y[cu]);
    let ca = stable_length(x[cu], y[cu], x[au], y[au]);
    let longest = omax(ab, omax(bc, ca));
    let scale = omax(
        (x[bu] - x[au]).abs(),
        omax(
            (y[bu] - y[au]).abs(),
            omax((x[cu] - x[au]).abs(), (y[cu] - y[au]).abs()),
        ),
    );
    let twice_area = if scale == 0. {
        0.
    } else {
        let abx = (x[bu] - x[au]) / scale;
        let aby = (y[bu] - y[au]) / scale;
        let acx = (x[cu] - x[au]) / scale;
        let acy = (y[cu] - y[au]) / scale;
        ((abx * acy) - (aby * acx)).abs() * scale * scale
    };
    let smallest_angle = omin(
        stable_angle(x, y, a, b, c),
        omin(stable_angle(x, y, b, c, a), stable_angle(x, y, c, a, b)),
    );
    let area = 0.5 * twice_area;
    let requested = (match minimum_angle {
        Some(limit) => smallest_angle < limit,
        None => false,
    }) || (match maximum_area {
        Some(limit) => area > limit,
        None => false,
    }) || match target_edge_length {
        Some(limit) => longest > limit,
        None => false,
    };
    requested && longest >= minimum_edge_length
}

fn inside_triangle(points: &[implicit_point::T], a: i64, b: i64, c: i64, point: &implicit_point::T) -> bool {
    let orient = |u: i64, v: i64| implicit_point::orient2d_xy(&points[u as usize], &points[v as usize], point);
    let nonnegative = |s: Sign| s != Sign::Negative;
    nonnegative(orient(a, b)) && nonnegative(orient(b, c)) && nonnegative(orient(c, a))
}

fn strict_segment_contains(
    first: &implicit_point::T,
    second: &implicit_point::T,
    point: &implicit_point::T,
) -> bool {
    if implicit_point::equal(first, point) || implicit_point::equal(second, point) {
        return false;
    }
    if implicit_point::orient2d_xy(first, second, point) != Sign::Zero {
        return false;
    }
    let compare: fn(&implicit_point::T, &implicit_point::T) -> i64 =
        if implicit_point::compare_x(first, second) != 0 {
            implicit_point::compare_x
        } else {
            implicit_point::compare_y
        };
    let (lower, upper) = if compare(first, second) < 0 { (first, second) } else { (second, first) };
    compare(lower, point) < 0 && compare(point, upper) < 0
}

fn barycentric(x: &[f64], y: &[f64], a: i64, b: i64, c: i64, px: f64, py: f64) -> (f64, f64, f64) {
    let (au, bu, cu) = (a as usize, b as usize, c as usize);
    let third = (1. / 3., 1. / 3., 1. / 3.);
    let scale = omax(
        (x[bu] - x[au]).abs(),
        omax(
            (y[bu] - y[au]).abs(),
            omax(
                (x[cu] - x[au]).abs(),
                omax(
                    (y[cu] - y[au]).abs(),
                    omax((px - x[au]).abs(), (py - y[au]).abs()),
                ),
            ),
        ),
    );
    if scale == 0. || !scale.is_finite() {
        return third;
    }
    let ax = x[au] / scale;
    let ay = y[au] / scale;
    let bx = x[bu] / scale;
    let by = y[bu] / scale;
    let cx = x[cu] / scale;
    let cy = y[cu] / scale;
    let px = px / scale;
    let py = py / scale;
    let denominator = ((bx - ax) * (cy - ay)) - ((by - ay) * (cx - ax));
    if denominator == 0. || !denominator.is_finite() {
        return third;
    }
    let wb = (((px - ax) * (cy - ay)) - ((py - ay) * (cx - ax))) / denominator;
    let wc = (((bx - ax) * (py - ay)) - ((by - ay) * (px - ax))) / denominator;
    let wa = 1. - wb - wc;
    if wa.is_finite() && wb.is_finite() && wc.is_finite() {
        (wa, wb, wc)
    } else {
        third
    }
}

/// OCaml `Result.get_ok`.
fn get_ok(value: Result<implicit_point::T, PointError>) -> R<implicit_point::T> {
    value.map_err(|_| "result is Error _".to_owned())
}

fn rounded_centroid(points: &[implicit_point::T], x: &[f64], y: &[f64], a: i64, b: i64, c: i64) -> R<implicit_point::T> {
    let coordinate = |values: &[f64]| -> f64 {
        let first = values[a as usize];
        let second = values[b as usize];
        let third = values[c as usize];
        let scale = omax(first.abs(), omax(second.abs(), third.abs()));
        if scale == 0. {
            0.
        } else {
            scale * ((first / scale / 3.) + (second / scale / 3.) + (third / scale / 3.))
        }
    };
    get_ok(implicit_point::rounded(&points[a as usize], coordinate(x), coordinate(y), 0.))
}

fn rounded_circumcenter(
    fallback: &implicit_point::T,
    points: &[implicit_point::T],
    x: &[f64],
    y: &[f64],
    a: i64,
    b: i64,
    c: i64,
) -> implicit_point::T {
    let (au, bu, cu) = (a as usize, b as usize, c as usize);
    let ax = x[au];
    let ay = y[au];
    let bax = x[bu] - ax;
    let bay = y[bu] - ay;
    let cax = x[cu] - ax;
    let cay = y[cu] - ay;
    let scale = omax(bax.abs(), omax(bay.abs(), omax(cax.abs(), cay.abs())));
    if scale == 0. || !scale.is_finite() {
        return fallback.clone();
    }
    let bx = bax / scale;
    let by = bay / scale;
    let cx = cax / scale;
    let cy = cay / scale;
    let denominator = 2. * ((bx * cy) - (by * cx));
    if denominator == 0. || !denominator.is_finite() {
        return fallback.clone();
    }
    let blift = (bx * bx) + (by * by);
    let clift = (cx * cx) + (cy * cy);
    let ux = ((blift * cy) - (clift * by)) / denominator;
    let uy = ((clift * bx) - (blift * cx)) / denominator;
    let px = ax + (scale * ux);
    let py = ay + (scale * uy);
    if px.is_finite() && py.is_finite() {
        match implicit_point::rounded(&points[au], px, py, 0.) {
            Ok(point) => point,
            Err(_) => fallback.clone(),
        }
    } else {
        fallback.clone()
    }
}

fn canonical_bits(value: f64) -> i64 {
    if value == 0. { 0 } else { value.to_bits() as i64 }
}

fn wrap63(value: i64) -> i64 {
    (value << 1) >> 1
}

fn lsr63(value: i64, amount: u32) -> i64 {
    (((value as u64) & MASK63) >> amount) as i64
}

struct Buckets {
    keys: Vec<i64>,
    heads: Vec<i64>,
    next: Vec<i64>,
    count: i64,
}

fn buckets_capacity(expected: i64) -> R<usize> {
    if expected < 0 || expected > MAX_ARRAY_LENGTH / 2 {
        return Err("Planar refinement coordinate table exceeds array limits".to_owned());
    }
    let mut capacity: i64 = 16;
    while capacity < std::cmp::max(16, expected * 2) {
        if capacity > MAX_ARRAY_LENGTH / 2 {
            return Err("Planar refinement coordinate table exceeds array limits".to_owned());
        }
        capacity *= 2;
    }
    Ok(capacity as usize)
}

fn buckets_create(expected: i64) -> R<Buckets> {
    let size = buckets_capacity(expected)?;
    Ok(Buckets {
        keys: vec![-1; size],
        heads: vec![-1; size],
        next: vec![-1; std::cmp::max(1, expected) as usize],
        count: 0,
    })
}

fn buckets_key(x: i64, y: i64) -> i64 {
    // Collisions are checked against canonical coordinates and symbolic points.
    let mix = |value: i64| -> i64 {
        let value = wrap63((value ^ lsr63(value, 30)).wrapping_mul(0x3f4a7c159e3779b));
        let value = wrap63((value ^ lsr63(value, 27)).wrapping_mul(0x2f1bbcdc676dfd1));
        value ^ lsr63(value, 31)
    };
    (mix(wrap63(x)) ^ wrap63(mix(wrap63(y)) << 1)) & MAX_INT
}

fn buckets_slot(keys: &[i64], key: i64) -> usize {
    let mask = (keys.len() - 1) as i64;
    let mut result = key & mask;
    while keys[result as usize] != -1 && keys[result as usize] != key {
        result = (result + 1) & mask;
    }
    result as usize
}

fn buckets_grow(table: &mut Buckets) -> R<()> {
    if table.keys.len() as i64 > MAX_ARRAY_LENGTH / 2 {
        return Err("Planar refinement coordinate table exceeds array limits".to_owned());
    }
    let size = table.keys.len() * 2;
    let mut keys = vec![-1i64; size];
    let mut heads = vec![-1i64; size];
    for index in 0..table.keys.len() {
        let key = table.keys[index];
        if key != -1 {
            let target = buckets_slot(&keys, key);
            keys[target] = key;
            heads[target] = table.heads[index];
        }
    }
    table.keys = keys;
    table.heads = heads;
    Ok(())
}

fn buckets_ensure_next(table: &mut Buckets, index: i64) -> R<()> {
    if index >= table.next.len() as i64 {
        let mut size = table.next.len() as i64;
        while size <= index {
            if size > MAX_ARRAY_LENGTH / 2 {
                return Err("Planar refinement coordinate chain exceeds array limits".to_owned());
            }
            size *= 2;
        }
        table.next.resize(size as usize, -1);
    }
    Ok(())
}

fn buckets_find(table: &Buckets, key: i64) -> i64 {
    let index = buckets_slot(&table.keys, key);
    if table.keys[index] == key { table.heads[index] } else { -1 }
}

fn buckets_add(table: &mut Buckets, key: i64, value: i64) -> R<()> {
    if table.count >= (table.keys.len() / 2) as i64 {
        buckets_grow(table)?;
    }
    buckets_ensure_next(table, value)?;
    let index = buckets_slot(&table.keys, key);
    if table.keys[index] == -1 {
        table.keys[index] = key;
        table.count += 1;
    }
    table.next[value as usize] = table.heads[index];
    table.heads[index] = value;
    Ok(())
}

fn downward(value: f64) -> f64 {
    value.next_down()
}

fn upward(value: f64) -> f64 {
    value.next_up()
}

fn half_sum_lower(first: f64, second: f64) -> f64 {
    downward((first * 0.5) + (second * 0.5))
}

fn half_sum_upper(first: f64, second: f64) -> f64 {
    upward((first * 0.5) + (second * 0.5))
}

fn difference_upper(first: f64, second: f64) -> f64 {
    let value = first - second;
    if value.is_finite() { upward(value.abs()) } else { f64::INFINITY }
}

fn radius_upper(first: &implicit_point::T, second: &implicit_point::T) -> f64 {
    let ((fxl, fxu), (fyl, fyu), _) = implicit_point::bounds(first);
    let ((sxl, sxu), (syl, syu), _) = implicit_point::bounds(second);
    let dx = omax(difference_upper(fxl, sxu), difference_upper(fxu, sxl));
    let dy = omax(difference_upper(fyl, syu), difference_upper(fyu, syl));
    if !(dx.is_finite() && dy.is_finite()) {
        f64::INFINITY
    } else {
        upward(upward(dx * 0.5) + upward(dy * 0.5))
    }
}

fn lower_radius(center: f64, radius: f64) -> f64 {
    if radius == f64::INFINITY { f64::NEG_INFINITY } else { downward(center - radius) }
}

fn upper_radius(center: f64, radius: f64) -> f64 {
    if radius == f64::INFINITY { f64::INFINITY } else { upward(center + radius) }
}

fn cancelled() -> String {
    "Planar refinement was cancelled".to_owned()
}

/// Maps an `error::T` raised by a helper (cancellation or invalid argument) to the OCaml message.
fn conv(value: error::T) -> String {
    if error::code(&value) == "cancelled" {
        cancelled()
    } else {
        error::message(&value).to_owned()
    }
}

fn check(cancel_value: Option<&cancel::T>) -> R<()> {
    cancel::check_opt(cancel_value).map_err(|_| cancelled())
}

fn poll(cancel_value: Option<&cancel::T>, index: i64, mask: i64) -> R<()> {
    cancel::poll(cancel_value, index, mask).map_err(|_| cancelled())
}

fn constraint_index(
    cancel_value: Option<&cancel::T>,
    points: &[implicit_point::T],
    constraints: &[i64],
) -> R<bounds2_index::T> {
    let count = constraints.len() / 2;
    let mut min_x = vec![0.; count];
    let mut min_y = vec![0.; count];
    let mut max_x = vec![0.; count];
    let mut max_y = vec![0.; count];
    for segment in 0..count {
        poll(cancel_value, segment as i64, 16_383)?;
        let (fi, si) = (constraints[segment * 2], constraints[segment * 2 + 1]);
        if fi < 0 || fi >= points.len() as i64 || si < 0 || si >= points.len() as i64 {
            return Err("index out of bounds".to_owned());
        }
        let first = &points[fi as usize];
        let second = &points[si as usize];
        let ((fxl, fxu), (fyl, fyu), _) = implicit_point::bounds(first);
        let ((sxl, sxu), (syl, syu), _) = implicit_point::bounds(second);
        let center_x_lower = half_sum_lower(fxl, sxl);
        let center_x_upper = half_sum_upper(fxu, sxu);
        let center_y_lower = half_sum_lower(fyl, syl);
        let center_y_upper = half_sum_upper(fyu, syu);
        let radius = radius_upper(first, second);
        min_x[segment] = lower_radius(center_x_lower, radius);
        min_y[segment] = lower_radius(center_y_lower, radius);
        max_x[segment] = upper_radius(center_x_upper, radius);
        max_y[segment] = upper_radius(center_y_upper, radius);
    }
    bounds2_index::create(cancel_value, &min_x, &min_y, &max_x, &max_y).map_err(conv)
}

struct Provenance {
    first: Vec<i64>,
    second: Vec<i64>,
    third: Vec<i64>,
    weight_first: Vec<f64>,
    weight_second: Vec<f64>,
    weight_third: Vec<f64>,
}

fn append_provenance(value: &mut Provenance, first: i64, second: i64, third: i64, wa: f64, wb: f64, wc: f64) -> i64 {
    let node = value.first.len() as i64;
    value.first.push(first);
    value.second.push(second);
    value.third.push(third);
    value.weight_first.push(wa);
    value.weight_second.push(wb);
    value.weight_third.push(wc);
    node
}

struct MeshIndex {
    neighbor_offsets: Vec<i64>,
    neighbors: Vec<i64>,
    incident_offsets: Vec<i64>,
    incidents: Vec<i64>,
    fixed: Vec<u8>,
}

fn mesh_index(
    cancel_value: Option<&cancel::T>,
    point_count: i64,
    triangles: &[i64],
    constraints: &[i64],
) -> R<MeshIndex> {
    let triangle_count = triangles.len() / 3;
    if triangle_count as i64 > MAX_ARRAY_LENGTH / 3 {
        return Err("Planar refinement edge cardinality exceeds array limits".to_owned());
    }
    let edge_count = triangle_count * 3;
    let mut edge_a = vec![0i64; edge_count];
    let mut edge_b = vec![0i64; edge_count];
    let mut write = |edge: usize, first: i64, second: i64| {
        if first < second {
            edge_a[edge] = first;
            edge_b[edge] = second;
        } else {
            edge_a[edge] = second;
            edge_b[edge] = first;
        }
    };
    for triangle in 0..triangle_count {
        poll(cancel_value, triangle as i64, 4095)?;
        let a = triangles[triangle * 3];
        let b = triangles[triangle * 3 + 1];
        let c = triangles[triangle * 3 + 2];
        write(triangle * 3, a, b);
        write(triangle * 3 + 1, b, c);
        write(triangle * 3 + 2, c, a);
    }
    let mut order: Vec<usize> = (0..edge_count).collect();
    order.sort_by(|&l, &r| edge_a[l].cmp(&edge_a[r]).then(edge_b[l].cmp(&edge_b[r])));
    let mut unique_a: Vec<i64> = Vec::new();
    let mut unique_b: Vec<i64> = Vec::new();
    let mut incidence: Vec<i64> = Vec::new();
    for index in 0..edge_count {
        let edge = order[index];
        if index == 0
            || edge_a[edge] != edge_a[order[index - 1]]
            || edge_b[edge] != edge_b[order[index - 1]]
        {
            unique_a.push(edge_a[edge]);
            unique_b.push(edge_b[edge]);
            incidence.push(0);
        }
        let last = incidence.len() - 1;
        incidence[last] += 1;
    }
    let unique = unique_a.len();
    if unique as i64 > MAX_ARRAY_LENGTH / 2 {
        return Err("Planar refinement neighbor cardinality exceeds array limits".to_owned());
    }
    let pc = point_count as usize;
    let mut neighbor_counts = vec![0i64; pc];
    let mut fixed = vec![0u8; pc];
    for edge in 0..unique {
        let (a, b) = (unique_a[edge] as usize, unique_b[edge] as usize);
        neighbor_counts[a] += 1;
        neighbor_counts[b] += 1;
        if incidence[edge] == 1 {
            fixed[a] = 1;
            fixed[b] = 1;
        } else if incidence[edge] != 2 {
            return Err("Planar refinement topology contains a non-manifold edge".to_owned());
        }
    }
    for &point in constraints {
        if point < 0 || point >= point_count {
            return Err("Planar refinement constraint endpoint is out of range".to_owned());
        }
        fixed[point as usize] = 1;
    }
    let mut neighbor_offsets = vec![0i64; pc + 1];
    for point in 0..pc {
        neighbor_offsets[point + 1] = neighbor_offsets[point] + neighbor_counts[point];
    }
    let mut neighbors = vec![0i64; neighbor_offsets[pc] as usize];
    let mut next = neighbor_offsets.clone();
    for edge in 0..unique {
        let (a, b) = (unique_a[edge], unique_b[edge]);
        neighbors[next[a as usize] as usize] = b;
        next[a as usize] += 1;
        neighbors[next[b as usize] as usize] = a;
        next[b as usize] += 1;
    }
    let mut incident_counts = vec![0i64; pc];
    for &point in triangles {
        incident_counts[point as usize] += 1;
    }
    let mut incident_offsets = vec![0i64; pc + 1];
    for point in 0..pc {
        incident_offsets[point + 1] = incident_offsets[point] + incident_counts[point];
    }
    let mut incidents = vec![0i64; incident_offsets[pc] as usize];
    let mut next = incident_offsets.clone();
    for triangle in 0..triangle_count {
        for local in 0..3 {
            let point = triangles[triangle * 3 + local] as usize;
            incidents[next[point] as usize] = triangle as i64;
            next[point] += 1;
        }
    }
    Ok(MeshIndex {
        neighbor_offsets,
        neighbors,
        incident_offsets,
        incidents,
        fixed,
    })
}

fn stable_average(values: &[f64], offsets: &[i64], members: &[i64], point: usize) -> f64 {
    let first = offsets[point];
    let last = offsets[point + 1];
    let mut scale: f64 = 0.;
    for at in first..last {
        scale = omax(scale, values[members[at as usize] as usize].abs());
    }
    if scale == 0. {
        0.
    } else {
        let mut sum = 0.;
        let mut compensation = 0.;
        for at in first..last {
            let value = (values[members[at as usize] as usize] / scale) - compensation;
            let next = sum + value;
            compensation = next - sum - value;
            sum = next;
        }
        let normalized = sum / ((last - first) as f64);
        scale * omax(-1., omin(1., normalized))
    }
}

fn stable_blend(first: f64, second: f64, alpha: f64) -> f64 {
    let scale = omax(first.abs(), second.abs());
    if scale == 0. {
        0.
    } else {
        scale * ((first / scale * (1. - alpha)) + (second / scale * alpha))
    }
}

fn run_cdt(
    cancel_value: Option<&cancel::T>,
    workspace: &mut planar_cdt::Workspace,
    points: &[implicit_point::T],
    point_count: i64,
    triangles: &[i64],
    insert_points: Option<&[i64]>,
    constraints: &[i64],
    winding: &[i64],
    prefix: &str,
) -> R<Arc<[i64]>> {
    let orient = |a: i64, b: i64, c: i64| {
        implicit_point::orient2d_xy(&points[a as usize], &points[b as usize], &points[c as usize])
    };
    let incircle = |a: i64, b: i64, c: i64, d: i64| {
        implicit_point::incircle_xy(
            &points[a as usize],
            &points[b as usize],
            &points[c as usize],
            &points[d as usize],
        )
    };
    match planar_cdt::build(
        cancel_value,
        Some(workspace),
        point_count,
        &orient,
        &incircle,
        None,
        triangles,
        insert_points,
        false,
        constraints,
        Some(winding),
        false,
    ) {
        Err(message) => Err(format!("{}{}", prefix, message)),
        Ok(rebuilt) => Ok(planar_cdt::View::view(&rebuilt).triangle_points),
    }
}

#[allow(clippy::too_many_arguments)]
fn run(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    initial_points: &[implicit_point::T],
    initial_triangle_points: &[i64],
    constraint_points: &[i64],
    constraint_winding: &[i64],
    minimum_angle: Option<f64>,
    maximum_area: Option<f64>,
    target_edge_length: Option<f64>,
    minimum_edge_length: f64,
    maximum_new_points: i64,
    allow_constraint_splitting: bool,
    regularization_steps: i64,
    allow_movement_of_interior_input_points: bool,
) -> R<T> {
    if grain <= 0 {
        return Err("Planar refinement grain must be positive".to_owned());
    }
    if minimum_edge_length < 0. || !minimum_edge_length.is_finite() {
        return Err("Planar refinement minimum edge length must be finite and non-negative".to_owned());
    }
    if maximum_new_points < 0 {
        return Err("Planar refinement maximum new points must be non-negative".to_owned());
    }
    if regularization_steps < 0 {
        return Err("Planar refinement regularization steps must be non-negative".to_owned());
    }
    finite_positive("minimum angle", minimum_angle)?;
    finite_positive("maximum area", maximum_area)?;
    finite_positive("target edge length", target_edge_length)?;
    if let Some(angle) = minimum_angle {
        if angle >= std::f64::consts::PI / 3. {
            return Err("Planar refinement minimum angle must be less than pi/3".to_owned());
        }
    }
    if constraint_points.len() & 1 != 0 {
        return Err("Planar refinement constraint endpoint array is malformed".to_owned());
    }
    if !constraint_winding.is_empty() && constraint_winding.len() != constraint_points.len() / 2 {
        return Err("Planar refinement constraint winding array is malformed".to_owned());
    }
    let initial_len = initial_points.len() as i64;
    if initial_len > MAX_ARRAY_LENGTH - maximum_new_points {
        return Err("Planar refinement point limit exceeds array limits".to_owned());
    }
    // OCaml `initial_points.(0)` raises Invalid_argument "index out of bounds" when empty.
    if initial_points.is_empty() {
        return Err("index out of bounds".to_owned());
    }
    // Implicit_point predicates panic (OCaml Invalid_argument) on mixed sources; reject up front.
    for point in initial_points.iter().skip(1) {
        if implicit_point::midpoint(&initial_points[0], point).is_err() {
            return Err("Rdk implicit points belong to different coordinate sources".to_owned());
        }
    }
    // OCaml indexes x/y by these triangle points in the first classification: bounds-check here.
    for &point in &initial_triangle_points[..initial_triangle_points.len() / 3 * 3] {
        if point < 0 || point >= initial_len {
            return Err("index out of bounds".to_owned());
        }
    }
    let mut points: Vec<implicit_point::T> = initial_points.to_vec();
    let mut x: Vec<f64> = Vec::with_capacity(initial_points.len());
    let mut y: Vec<f64> = Vec::with_capacity(initial_points.len());
    for value in initial_points {
        let (px, py, _) = implicit_point::approximate(value);
        x.push(px);
        y.push(py);
    }
    let capacity = initial_len + maximum_new_points;
    let mut point_roots = vec![0i64; maximum_new_points as usize];
    let mut provenance = Provenance {
        first: Vec::new(),
        second: Vec::new(),
        third: Vec::new(),
        weight_first: Vec::new(),
        weight_second: Vec::new(),
        weight_third: Vec::new(),
    };
    let mut point_count = initial_len;
    let mut constraints: Vec<i64> = constraint_points.to_vec();
    let mut winding: Vec<i64> = if constraint_winding.is_empty() {
        vec![0; constraint_points.len() / 2]
    } else {
        constraint_winding.to_vec()
    };
    let mut triangles: Arc<[i64]> = Arc::from(initial_triangle_points.to_vec());
    let mut cdt_workspace = planar_cdt::View::create_workspace(
        Some(capacity),
        std::cmp::max(1, initial_triangle_points.len() as i64 / 3),
    )?;
    let mut buckets = buckets_create(initial_len)?;
    let add_bucket = |buckets: &mut Buckets, x: &[f64], y: &[f64], point: i64| -> R<()> {
        let key = buckets_key(canonical_bits(x[point as usize]), canonical_bits(y[point as usize]));
        buckets_add(buckets, key, point)
    };
    for point in 0..point_count {
        add_bucket(&mut buckets, &x, &y, point)?;
    }
    // refine
    loop {
        check(cancel_value)?;
        let triangle_count = triangles.len() / 3;
        let mut bad = vec![false; triangle_count];
        for triangle in 0..triangle_count {
            poll(cancel_value, triangle as i64, 4095)?;
            let a = triangles[triangle * 3];
            let b = triangles[triangle * 3 + 1];
            let c = triangles[triangle * 3 + 2];
            if triangle_requested(
                &x,
                &y,
                a,
                b,
                c,
                minimum_angle,
                maximum_area,
                target_edge_length,
                minimum_edge_length,
            ) {
                bad[triangle] = true;
            }
        }
        let bad_count = bad.iter().filter(|&&b| b).count();
        if !(bad_count > 0 && point_count - initial_len < maximum_new_points) {
            break;
        }
        let mut candidate_points: Vec<Option<implicit_point::T>> = vec![None; bad_count];
        let mut candidate_a = vec![0i64; bad_count];
        let mut candidate_b = vec![0i64; bad_count];
        let mut candidate_c = vec![0i64; bad_count];
        let mut candidate_split = vec![-1i64; bad_count];
        let mut at = 0usize;
        for triangle in 0..triangle_count {
            if bad[triangle] {
                candidate_a[at] = triangles[triangle * 3];
                candidate_b[at] = triangles[triangle * 3 + 1];
                candidate_c[at] = triangles[triangle * 3 + 2];
                at += 1;
            }
        }
        let segment_count = constraints.len() / 2;
        let constraint_tree = if segment_count == 0 {
            None
        } else {
            Some(constraint_index(cancel_value, &points, &constraints)?)
        };
        // ponytail: sequential where OCaml runs `construct` through Parallel.for_ (same result).
        for candidate in 0..bad_count {
            poll(cancel_value, candidate as i64, 1023)?;
            let (a, b, c) = (candidate_a[candidate], candidate_b[candidate], candidate_c[candidate]);
            let centroid = rounded_centroid(&points, &x, &y, a, b, c)?;
            let circumcenter = rounded_circumcenter(&centroid, &points, &x, &y, a, b, c);
            let center = if inside_triangle(&points, a, b, c, &circumcenter) {
                circumcenter
            } else {
                centroid.clone()
            };
            let mut encroached: i64 = -1;
            if let Some(tree) = &constraint_tree {
                let ((pminx, pmaxx), (pminy, pmaxy), _) = implicit_point::bounds(&center);
                bounds2_index::query(cancel_value, tree, pminx, pminy, pmaxx, pmaxy, &mut |segment| {
                    let first = constraints[segment as usize * 2];
                    let second = constraints[segment as usize * 2 + 1];
                    if implicit_point::diametral_dot_xy(
                        &points[first as usize],
                        &points[second as usize],
                        &center,
                    ) != Sign::Positive
                        && (encroached < 0 || segment < encroached)
                    {
                        encroached = segment;
                    }
                    Ok(())
                })
                .map_err(conv)?;
            }
            if encroached >= 0 && allow_constraint_splitting {
                let first = constraints[encroached as usize * 2];
                let second = constraints[encroached as usize * 2 + 1];
                candidate_points[candidate] = Some(get_ok(implicit_point::midpoint(
                    &points[first as usize],
                    &points[second as usize],
                ))?);
                candidate_a[candidate] = first;
                candidate_b[candidate] = second;
                candidate_c[candidate] = first;
                candidate_split[candidate] = encroached;
            } else {
                candidate_points[candidate] = Some(if encroached >= 0 { centroid } else { center });
            }
        }
        let candidate_points: Vec<implicit_point::T> =
            candidate_points.into_iter().map(|p| p.expect("constructed")).collect();
        let remaining = maximum_new_points - (point_count - initial_len);
        let mut selected = vec![false; bad_count];
        let mut selected_count: i64 = 0;
        let mut candidate_buckets = buckets_create(bad_count as i64)?;
        for candidate in 0..bad_count {
            if selected_count < remaining {
                // exists candidate
                let (px, py, _) = implicit_point::approximate(&candidate_points[candidate]);
                let (bx, by) = (canonical_bits(px), canonical_bits(py));
                let mut point = buckets_find(&buckets, buckets_key(bx, by));
                let mut found = false;
                while point >= 0 && !found {
                    found = canonical_bits(x[point as usize]) == bx
                        && canonical_bits(y[point as usize]) == by
                        && implicit_point::equal(&points[point as usize], &candidate_points[candidate]);
                    point = buckets.next[point as usize];
                }
                if !found {
                    let key = buckets_key(bx, by);
                    let mut other = buckets_find(&candidate_buckets, key);
                    let mut duplicate = false;
                    while other >= 0 && !duplicate {
                        let (ox, oy, _) = implicit_point::approximate(&candidate_points[other as usize]);
                        duplicate = canonical_bits(ox) == bx
                            && canonical_bits(oy) == by
                            && implicit_point::equal(
                                &candidate_points[other as usize],
                                &candidate_points[candidate],
                            );
                        other = candidate_buckets.next[other as usize];
                    }
                    if !duplicate {
                        selected[candidate] = true;
                        selected_count += 1;
                        buckets_add(&mut candidate_buckets, key, candidate as i64)?;
                    }
                }
            }
        }
        if selected_count == 0 {
            break;
        }
        let first_new_point = point_count;
        for candidate in 0..bad_count {
            if selected[candidate] {
                let output = point_count;
                let generated = output - initial_len;
                let value = candidate_points[candidate].clone();
                let (px, py, _) = implicit_point::approximate(&value);
                points.push(value);
                x.push(px);
                y.push(py);
                let (a, b, c) = (candidate_a[candidate], candidate_b[candidate], candidate_c[candidate]);
                let value_ref = |point: i64| -> i64 {
                    if point < initial_len { point } else { point_roots[(point - initial_len) as usize] }
                };
                let (wa, wb, wc) = if candidate_split[candidate] >= 0 {
                    (0.5, 0.5, 0.)
                } else {
                    barycentric(&x, &y, a, b, c, px, py)
                };
                let node = append_provenance(
                    &mut provenance,
                    value_ref(a),
                    value_ref(b),
                    value_ref(c),
                    wa,
                    wb,
                    wc,
                );
                point_roots[generated as usize] = initial_len + node;
                point_count += 1;
                add_bucket(&mut buckets, &x, &y, output)?;
            }
        }
        let mut events: Vec<Vec<i64>> = vec![Vec::new(); segment_count];
        let mut event_count: usize = 0;
        if let Some(tree) = &constraint_tree {
            for point in first_new_point..point_count {
                let ((pminx, pmaxx), (pminy, pmaxy), _) = implicit_point::bounds(&points[point as usize]);
                bounds2_index::query(cancel_value, tree, pminx, pminy, pmaxx, pmaxy, &mut |segment| {
                    let a = constraints[segment as usize * 2];
                    let b = constraints[segment as usize * 2 + 1];
                    if strict_segment_contains(
                        &points[a as usize],
                        &points[b as usize],
                        &points[point as usize],
                    ) {
                        events[segment as usize].push(point);
                        event_count += 1;
                    }
                    Ok(())
                })
                .map_err(conv)?;
            }
        }
        if event_count > 0 {
            let mut next_constraints = vec![0i64; (segment_count + event_count) * 2];
            let mut next_winding = vec![0i64; segment_count + event_count];
            let mut output = 0usize;
            for segment in 0..segment_count {
                let a = constraints[segment * 2];
                let b = constraints[segment * 2 + 1];
                let by_x = implicit_point::compare_x(&points[a as usize], &points[b as usize]) != 0;
                let compare = |left: i64, right: i64| -> i64 {
                    if by_x {
                        implicit_point::compare_x(&points[left as usize], &points[right as usize])
                    } else {
                        implicit_point::compare_y(&points[left as usize], &points[right as usize])
                    }
                };
                // OCaml builds the list by consing, so it starts newest-first; List.sort is stable.
                let mut ordered: Vec<i64> = events[segment].iter().rev().copied().collect();
                ordered.sort_by(|&l, &r| to_ordering(compare(l, r)));
                if compare(a, b) >= 0 {
                    ordered.reverse();
                }
                let mut previous = a;
                for &next in ordered.iter().chain(std::iter::once(&b)) {
                    next_constraints[output * 2] = previous;
                    next_constraints[output * 2 + 1] = next;
                    next_winding[output] = winding[segment];
                    previous = next;
                    output += 1;
                }
            }
            constraints = next_constraints;
            winding = next_winding;
        }
        let insert_points: Vec<i64> = (first_new_point..point_count).collect();
        triangles = run_cdt(
            cancel_value,
            &mut cdt_workspace,
            &points,
            point_count,
            &triangles,
            Some(&insert_points),
            &constraints,
            &winding,
            "Planar refinement: ",
        )?;
    }
    // regularize
    let mut step = 0;
    let mut continue_ = true;
    while continue_ && step < regularization_steps {
        check(cancel_value)?;
        let index = mesh_index(cancel_value, point_count, &triangles, &constraints)?;
        let pc = point_count as usize;
        let mut eligible = vec![false; pc];
        let mut target_x: Vec<f64> = x[..pc].to_vec();
        let mut target_y: Vec<f64> = y[..pc].to_vec();
        for point in 0..pc {
            poll(cancel_value, point as i64, 4095)?;
            let first = index.neighbor_offsets[point];
            let last = index.neighbor_offsets[point + 1];
            if first < last
                && index.fixed[point] == 0
                && (point as i64 >= initial_len || allow_movement_of_interior_input_points)
            {
                let tx = stable_average(&x, &index.neighbor_offsets, &index.neighbors, point);
                let ty = stable_average(&y, &index.neighbor_offsets, &index.neighbors, point);
                if tx.is_finite() && ty.is_finite() {
                    target_x[point] = tx;
                    target_y[point] = ty;
                    eligible[point] = true;
                }
            }
        }
        let mut next_points: Vec<implicit_point::T> = points[..pc].to_vec();
        let mut next_x: Vec<f64> = x[..pc].to_vec();
        let mut next_y: Vec<f64> = y[..pc].to_vec();
        let mut containing = vec![-1i64; pc];
        let mut moved = vec![false; pc];
        let mut accepted = false;
        let mut alpha = 1.;
        let mut attempt = 0;
        while !accepted && attempt < 30 {
            for point in 0..pc {
                if eligible[point] {
                    let px = stable_blend(x[point], target_x[point], alpha);
                    let py = stable_blend(y[point], target_y[point], alpha);
                    let candidate = get_ok(implicit_point::rounded(&points[point], px, py, 0.))?;
                    next_x[point] = px;
                    next_y[point] = py;
                    if implicit_point::equal(&candidate, &points[point]) {
                        moved[point] = false;
                        containing[point] = -2;
                    } else {
                        moved[point] = true;
                        containing[point] = -1;
                        let mut at = index.incident_offsets[point];
                        let last = index.incident_offsets[point + 1];
                        while containing[point] < 0 && at < last {
                            let triangle = index.incidents[at as usize];
                            let a = triangles[triangle as usize * 3];
                            let b = triangles[triangle as usize * 3 + 1];
                            let c = triangles[triangle as usize * 3 + 2];
                            if inside_triangle(&points, a, b, c, &candidate) {
                                containing[point] = triangle;
                            }
                            at += 1;
                        }
                    }
                    next_points[point] = candidate;
                } else {
                    next_points[point] = points[point].clone();
                    next_x[point] = x[point];
                    next_y[point] = y[point];
                    containing[point] = -2;
                    moved[point] = false;
                }
            }
            let triangle_count = triangles.len() / 3;
            let mut invalid = false;
            for triangle in 0..triangle_count {
                poll(cancel_value, triangle as i64, 4095)?;
                let a = triangles[triangle * 3] as usize;
                let b = triangles[triangle * 3 + 1] as usize;
                let c = triangles[triangle * 3 + 2] as usize;
                if implicit_point::orient2d_xy(&next_points[a], &next_points[b], &next_points[c])
                    != Sign::Positive
                {
                    invalid = true;
                }
            }
            let mut point = 0;
            while !invalid && point < pc {
                if moved[point] && containing[point] < 0 {
                    invalid = true;
                }
                point += 1;
            }
            if !invalid {
                accepted = true;
            } else {
                alpha *= 0.5;
                attempt += 1;
            }
        }
        if !accepted {
            continue_ = false;
        } else {
            let moved_count = moved.iter().filter(|&&m| m).count();
            if moved_count == 0 {
                continue_ = false;
            } else {
                let current_new_count = pc - initial_len as usize;
                let mut next_roots: Vec<i64> = point_roots[..current_new_count].to_vec();
                let value_ref = |point: i64, roots: &[i64]| -> i64 {
                    if point < initial_len { point } else { roots[(point - initial_len) as usize] }
                };
                for point in initial_len as usize..pc {
                    if moved[point] {
                        let triangle = containing[point] as usize;
                        let a = triangles[triangle * 3];
                        let b = triangles[triangle * 3 + 1];
                        let c = triangles[triangle * 3 + 2];
                        let (wa, wb, wc) = barycentric(&x, &y, a, b, c, next_x[point], next_y[point]);
                        let node = append_provenance(
                            &mut provenance,
                            value_ref(a, &point_roots),
                            value_ref(b, &point_roots),
                            value_ref(c, &point_roots),
                            wa,
                            wb,
                            wc,
                        );
                        next_roots[point - initial_len as usize] = initial_len + node;
                    }
                }
                point_roots[..current_new_count].copy_from_slice(&next_roots);
                points[..pc].clone_from_slice(&next_points);
                x[..pc].copy_from_slice(&next_x);
                y[..pc].copy_from_slice(&next_y);
                triangles = run_cdt(
                    cancel_value,
                    &mut cdt_workspace,
                    &points,
                    point_count,
                    &triangles,
                    None,
                    &constraints,
                    &winding,
                    "Planar regularization: ",
                )?;
                step += 1;
            }
        }
    }
    let new_count = (point_count - initial_len) as usize;
    Ok(T {
        initial_point_count: initial_len,
        points: points[..point_count as usize].to_vec(),
        x: x[..point_count as usize].to_vec(),
        y: y[..point_count as usize].to_vec(),
        triangles,
        constraints,
        point_roots: point_roots[..new_count].to_vec(),
        provenance_first: provenance.first,
        provenance_second: provenance.second,
        provenance_third: provenance.third,
        provenance_weight_first: provenance.weight_first,
        provenance_weight_second: provenance.weight_second,
        provenance_weight_third: provenance.weight_third,
    })
}

/// Refine in deterministic stable batches. `Err` carries the OCaml `Error` message.
#[allow(clippy::too_many_arguments)]
pub fn build(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    initial_points: &[implicit_point::T],
    initial_triangle_points: &[i64],
    constraint_points: &[i64],
    constraint_winding: &[i64],
    minimum_angle: Option<f64>,
    maximum_area: Option<f64>,
    target_edge_length: Option<f64>,
    minimum_edge_length: f64,
    maximum_new_points: i64,
    allow_constraint_splitting: bool,
    regularization_steps: i64,
    allow_movement_of_interior_input_points: bool,
) -> Result<T, String> {
    run(
        cancel_value,
        grain,
        initial_points,
        initial_triangle_points,
        constraint_points,
        constraint_winding,
        minimum_angle,
        maximum_area,
        target_edge_length,
        minimum_edge_length,
        maximum_new_points,
        allow_constraint_splitting,
        regularization_steps,
        allow_movement_of_interior_input_points,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn refines_a_thin_triangle() {
        let x = vec![0., 4., 0.];
        let y = vec![0., 0., 1.];
        let source = implicit_point::source(x, y, vec![0.; 3]).unwrap();
        let points: Vec<_> = (0..3).map(|i| implicit_point::explicit(&source, i).unwrap()).collect();
        let r = build(
            None,
            16,
            &points,
            &[0, 1, 2],
            &[],
            &[],
            Some(0.4),
            None,
            None,
            0.,
            20,
            true,
            0,
            false,
        )
        .unwrap();
        assert!(new_point_count(&r) > 0);
        assert!(triangle_points(&r).len() > 3);
        assert!(build(None, 0, &points, &[0, 1, 2], &[], &[], None, None, None, 0., 1, true, 0, false).is_err());
    }
}
