//! Spatial nearest-entity queries over packed primitive and edge features.
//!
//! OCaml returns `(_, string) result`; a failure here is an `error::T` with code
//! `invalid_parameter` and the OCaml message, and a cancellation keeps code `cancelled`.
use std::sync::Mutex;

use rdk_core::{cancel, error, geometry, packed, polygon_triangulation, support, topology, topology_index};
use rdk_core::error::Code;

use crate::bounds3_index;
use crate::point_grid::{float_max, float_min, point_triangle_squared};

const OPERATION: &str = "proximity_index";

pub struct Features {
    pub positions: packed::Float3::View::View,
    pub entity_count: i64,
    pub offsets: Vec<i64>,
    pub entities: Vec<i64>,
    pub kinds: Vec<u8>,
    pub a: Vec<i64>,
    pub b: Vec<i64>,
    pub c: Vec<i64>,
}

pub struct T {
    pub features: Features,
    pub order: Vec<i64>,
    pub min_x: Vec<f64>,
    pub min_y: Vec<f64>,
    pub min_z: Vec<f64>,
    pub max_x: Vec<f64>,
    pub max_y: Vec<f64>,
    pub max_z: Vec<f64>,
    pub left: Vec<i64>,
    pub right: Vec<i64>,
    pub first: Vec<i64>,
    pub count: Vec<i64>,
}

const SEGMENT: u8 = 0;
const TRIANGLE: u8 = 1;

fn failure(message: String) -> error::T {
    error::new(OPERATION, Code::InvalidParameter, message)
}

fn feature_count(features: &Features) -> usize {
    features.a.len()
}

fn validate_point(positions: &packed::Float3::View::View, point: i64, label: &str) -> Result<(), error::T> {
    let p = point as usize;
    if !(positions.x[p].is_finite() && positions.y[p].is_finite() && positions.z[p].is_finite()) {
        return Err(failure(format!("{label} references non-finite point {point}")));
    }
    Ok(())
}

struct Chunk {
    entities: Vec<i64>,
    kinds: Vec<u8>,
    a: Vec<i64>,
    b: Vec<i64>,
    c: Vec<i64>,
    failure: Option<String>,
}

/// Triangulates polygons (stable ear order) and splits curves into segments, one feature each.
pub fn primitive_features(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    geometry_value: &geometry::T,
) -> Result<Features, error::T> {
    let grain = grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(failure("Proximity index: grain must be positive".to_owned()));
    }
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let topology_view = topology::View::view(geometry::topology(geometry_value))?;
    let entity_count = geometry::primitive_count(geometry_value);
    let mut offsets = vec![0_i64; entity_count as usize + 1];
    for primitive in 0..entity_count as usize {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        let corners = topology_view.primitive_offsets[primitive + 1] - topology_view.primitive_offsets[primitive];
        let count = match topology_view.primitive_kinds[primitive] {
            0 => corners - 2,
            1 => corners - 1,
            _ => corners,
        };
        offsets[primitive + 1] = offsets[primitive] + count;
    }
    let total = offsets[entity_count as usize] as usize;
    let ranges = if entity_count == 0 { 0 } else { ((entity_count - 1) / grain + 1) as usize };
    let slots: Vec<Mutex<Option<Chunk>>> = (0..ranges).map(|_| Mutex::new(None)).collect();
    support::run_indexed_ranges(grain, entity_count, &|range, first_primitive, last_primitive| {
        let base = offsets[first_primitive as usize];
        let length = (offsets[last_primitive as usize] - base) as usize;
        let mut chunk = Chunk {
            entities: vec![0; length],
            kinds: vec![SEGMENT; length],
            a: vec![0; length],
            b: vec![0; length],
            c: vec![-1; length],
            failure: None,
        };
        let mut scratch = polygon_triangulation::create_scratch();
        for primitive in first_primitive..last_primitive {
            cancel::poll(cancel_value, primitive, 4095)?;
            if chunk.failure.is_some() {
                continue;
            }
            let p = primitive as usize;
            let first = topology_view.primitive_offsets[p];
            let last = topology_view.primitive_offsets[p + 1];
            let at = (offsets[p] - base) as usize;
            match topology_view.primitive_kinds[p] {
                0 => {
                    let result = polygon_triangulation::primitive(
                        cancel_value,
                        &positions,
                        &topology_view,
                        Some(&mut scratch),
                        primitive,
                        &mut |local, va, vb, vc| {
                            let feature = at + local as usize;
                            chunk.entities[feature] = primitive;
                            chunk.kinds[feature] = TRIANGLE;
                            chunk.a[feature] = topology_view.vertex_points[va as usize];
                            chunk.b[feature] = topology_view.vertex_points[vb as usize];
                            chunk.c[feature] = topology_view.vertex_points[vc as usize];
                        },
                    );
                    if let Err(problem) = result {
                        if error::is_cancelled(&problem) {
                            return Err(problem);
                        }
                        chunk.failure = Some(error::message(&problem).to_owned());
                    }
                }
                kind => {
                    for local in 0..(last - first - 1).max(0) {
                        let feature = at + local as usize;
                        chunk.entities[feature] = primitive;
                        chunk.a[feature] = topology_view.vertex_points[(first + local) as usize];
                        chunk.b[feature] = topology_view.vertex_points[(first + local + 1) as usize];
                    }
                    if kind == 2 {
                        let feature = (offsets[p + 1] - 1 - base) as usize;
                        chunk.entities[feature] = primitive;
                        chunk.a[feature] = topology_view.vertex_points[(last - 1) as usize];
                        chunk.b[feature] = topology_view.vertex_points[first as usize];
                    }
                }
            }
        }
        *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some(chunk);
        Ok(())
    })?;
    let mut entities = Vec::with_capacity(total);
    let mut kinds = Vec::with_capacity(total);
    let mut a = Vec::with_capacity(total);
    let mut b = Vec::with_capacity(total);
    let mut c = Vec::with_capacity(total);
    let mut first_failure: Option<String> = None;
    for slot in slots {
        if let Some(chunk) = slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
            if first_failure.is_none() {
                first_failure = chunk.failure;
            }
            entities.extend(chunk.entities);
            kinds.extend(chunk.kinds);
            a.extend(chunk.a);
            b.extend(chunk.b);
            c.extend(chunk.c);
        }
    }
    if let Some(message) = first_failure {
        return Err(failure(message));
    }
    for feature in 0..total {
        cancel::poll(cancel_value, feature as i64, 4095)?;
        validate_point(&positions, a[feature], "primitive")?;
        validate_point(&positions, b[feature], "primitive")?;
        if kinds[feature] == TRIANGLE {
            validate_point(&positions, c[feature], "primitive")?;
            let (pa, pb, pc) = (a[feature] as usize, b[feature] as usize, c[feature] as usize);
            let abx = positions.x[pb] - positions.x[pa];
            let aby = positions.y[pb] - positions.y[pa];
            let abz = positions.z[pb] - positions.z[pa];
            let acx = positions.x[pc] - positions.x[pa];
            let acy = positions.y[pc] - positions.y[pa];
            let acz = positions.z[pc] - positions.z[pa];
            let nx = (aby * acz) - (abz * acy);
            let ny = (abz * acx) - (abx * acz);
            let nz = (abx * acy) - (aby * acx);
            let area_squared = (nx * nx) + (ny * ny) + (nz * nz);
            if !area_squared.is_finite() || area_squared <= 1e-30 {
                return Err(failure(format!("primitive feature {feature} is degenerate")));
            }
        }
    }
    Ok(Features { positions, entity_count, offsets, entities, kinds, a, b, c })
}

/// One segment feature per unique edge.
pub fn edge_features(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    geometry_value: &geometry::T,
) -> Result<Features, error::T> {
    if grain.unwrap_or(16_384) <= 0 {
        return Err(failure("Proximity index: grain must be positive".to_owned()));
    }
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let index = topology_index::create(cancel_value, geometry::topology(geometry_value))?;
    let reverse = topology_index::View::view(&index);
    let entity_count = topology_index::edge_count(&index);
    let offsets: Vec<i64> = (0..=entity_count).collect();
    let entities: Vec<i64> = (0..entity_count).collect();
    let kinds = vec![SEGMENT; entity_count as usize];
    let a = reverse.edge_a.to_vec();
    let b = reverse.edge_b.to_vec();
    let c = vec![-1_i64; entity_count as usize];
    for edge in 0..entity_count as usize {
        cancel::poll(cancel_value, edge as i64, 4095)?;
        validate_point(&positions, a[edge], "edge")?;
        validate_point(&positions, b[edge], "edge")?;
    }
    Ok(Features { positions, entity_count, offsets, entities, kinds, a, b, c })
}

pub fn create(cancel_value: Option<&cancel::T>, grain: Option<i64>, features: Features) -> Result<T, error::T> {
    let grain = grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(failure("Proximity index: grain must be positive".to_owned()));
    }
    let total = feature_count(&features);
    if total == 0 {
        return Ok(T {
            features,
            order: Vec::new(),
            min_x: Vec::new(),
            min_y: Vec::new(),
            min_z: Vec::new(),
            max_x: Vec::new(),
            max_y: Vec::new(),
            max_z: Vec::new(),
            left: Vec::new(),
            right: Vec::new(),
            first: Vec::new(),
            count: Vec::new(),
        });
    }
    // Per-range chunks of nine planes: centroid, min, max.
    type Planes = [Vec<f64>; 9];
    let ranges = (total - 1) / grain as usize + 1;
    let slots: Vec<Mutex<Option<Planes>>> = (0..ranges).map(|_| Mutex::new(None)).collect();
    let positions = &features.positions;
    support::run_indexed_ranges(grain, total as i64, &|range, first, last| {
        let length = (last - first) as usize;
        let mut planes: Planes = std::array::from_fn(|_| vec![0.0; length]);
        for feature in first..last {
            cancel::poll(cancel_value, feature, 4095)?;
            let f = feature as usize;
            let at = f - first as usize;
            let point_a = features.a[f] as usize;
            let point_b = features.b[f] as usize;
            let (ax, ay, az) = (positions.x[point_a], positions.y[point_a], positions.z[point_a]);
            let (bx, by, bz) = (positions.x[point_b], positions.y[point_b], positions.z[point_b]);
            let point_c = features.c[f];
            let (cx, cy, cz) = if point_c >= 0 {
                let pc = point_c as usize;
                (positions.x[pc], positions.y[pc], positions.z[pc])
            } else {
                (bx, by, bz)
            };
            planes[3][at] = float_min(ax, float_min(bx, cx));
            planes[4][at] = float_min(ay, float_min(by, cy));
            planes[5][at] = float_min(az, float_min(bz, cz));
            planes[6][at] = float_max(ax, float_max(bx, cx));
            planes[7][at] = float_max(ay, float_max(by, cy));
            planes[8][at] = float_max(az, float_max(bz, cz));
            planes[0][at] = (ax / 3.0) + (bx / 3.0) + (cx / 3.0);
            planes[1][at] = (ay / 3.0) + (by / 3.0) + (cy / 3.0);
            planes[2][at] = (az / 3.0) + (bz / 3.0) + (cz / 3.0);
        }
        *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some(planes);
        Ok(())
    })?;
    let mut all: Planes = std::array::from_fn(|_| Vec::with_capacity(total));
    for slot in slots {
        if let Some(planes) = slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
            for (target, part) in all.iter_mut().zip(planes) {
                target.extend(part);
            }
        }
    }
    let bounds = bounds3_index::create(
        cancel_value, None, grain, &all[0], &all[1], &all[2], &all[3], &all[4], &all[5], &all[6], &all[7], &all[8],
    )?;
    Ok(T {
        features,
        order: bounds.order,
        min_x: bounds.min_x,
        min_y: bounds.min_y,
        min_z: bounds.min_z,
        max_x: bounds.max_x,
        max_y: bounds.max_y,
        max_z: bounds.max_z,
        left: bounds.left,
        right: bounds.right,
        first: bounds.first,
        count: bounds.count,
    })
}

fn clamp01(value: f64) -> f64 {
    if value <= 0.0 {
        0.0
    } else if value >= 1.0 {
        1.0
    } else {
        value
    }
}

#[allow(clippy::too_many_arguments)]
fn segment_parameters_squared(
    rx: f64, ry: f64, rz: f64, d1x: f64, d1y: f64, d1z: f64, d2x: f64, d2y: f64, d2z: f64, s: f64, t: f64,
) -> f64 {
    let dx = rx + (s * d1x) - (t * d2x);
    let dy = ry + (s * d1y) - (t * d2y);
    let dz = rz + (s * d1z) - (t * d2z);
    (dx * dx) + (dy * dy) + (dz * dz)
}

#[allow(clippy::too_many_arguments)]
fn segment_segment_squared(
    p1x: f64, p1y: f64, p1z: f64, q1x: f64, q1y: f64, q1z: f64, p2x: f64, p2y: f64, p2z: f64, q2x: f64, q2y: f64,
    q2z: f64,
) -> f64 {
    let d1x = q1x - p1x;
    let d1y = q1y - p1y;
    let d1z = q1z - p1z;
    let d2x = q2x - p2x;
    let d2y = q2y - p2y;
    let d2z = q2z - p2z;
    let rx = p1x - p2x;
    let ry = p1y - p2y;
    let rz = p1z - p2z;
    let a = (d1x * d1x) + (d1y * d1y) + (d1z * d1z);
    let e = (d2x * d2x) + (d2y * d2y) + (d2z * d2z);
    let at = |s: f64, t: f64| segment_parameters_squared(rx, ry, rz, d1x, d1y, d1z, d2x, d2y, d2z, s, t);
    if a <= 1e-30 && e <= 1e-30 {
        return at(0.0, 0.0);
    }
    if a <= 1e-30 {
        let t = clamp01(((d2x * rx) + (d2y * ry) + (d2z * rz)) / e);
        return at(0.0, t);
    }
    let c = (d1x * rx) + (d1y * ry) + (d1z * rz);
    if e <= 1e-30 {
        return at(clamp01(-c / a), 0.0);
    }
    let b = (d1x * d2x) + (d1y * d2y) + (d1z * d2z);
    let f = (d2x * rx) + (d2y * ry) + (d2z * rz);
    let denominator = (a * e) - (b * b);
    let s = if denominator != 0.0 { clamp01(((b * f) - (c * e)) / denominator) } else { 0.0 };
    let t = ((b * s) + f) / e;
    if t < 0.0 {
        at(clamp01(-c / a), 0.0)
    } else if t > 1.0 {
        at(clamp01((b - c) / a), 1.0)
    } else {
        at(s, t)
    }
}

#[allow(clippy::too_many_arguments)]
fn segment_intersects_triangle(
    px: f64, py: f64, pz: f64, qx: f64, qy: f64, qz: f64, ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64,
    cx: f64, cy: f64, cz: f64,
) -> bool {
    let dx = qx - px;
    let dy = qy - py;
    let dz = qz - pz;
    let e1x = bx - ax;
    let e1y = by - ay;
    let e1z = bz - az;
    let e2x = cx - ax;
    let e2y = cy - ay;
    let e2z = cz - az;
    let hx = (dy * e2z) - (dz * e2y);
    let hy = (dz * e2x) - (dx * e2z);
    let hz = (dx * e2y) - (dy * e2x);
    let determinant = (e1x * hx) + (e1y * hy) + (e1z * hz);
    if determinant == 0.0 {
        return false;
    }
    let inverse = 1.0 / determinant;
    let sx = px - ax;
    let sy = py - ay;
    let sz = pz - az;
    let u = inverse * ((sx * hx) + (sy * hy) + (sz * hz));
    if !(0.0..=1.0).contains(&u) {
        return false;
    }
    let vx = (sy * e1z) - (sz * e1y);
    let vy = (sz * e1x) - (sx * e1z);
    let vz = (sx * e1y) - (sy * e1x);
    let v = inverse * ((dx * vx) + (dy * vy) + (dz * vz));
    if v < 0.0 || u + v > 1.0 {
        return false;
    }
    let along = inverse * ((e2x * vx) + (e2y * vy) + (e2z * vz));
    (0.0..=1.0).contains(&along)
}

#[allow(clippy::too_many_arguments)]
fn segment_triangle_squared(
    px: f64, py: f64, pz: f64, qx: f64, qy: f64, qz: f64, ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64,
    cx: f64, cy: f64, cz: f64,
) -> f64 {
    if segment_intersects_triangle(px, py, pz, qx, qy, qz, ax, ay, az, bx, by, bz, cx, cy, cz) {
        return 0.0;
    }
    float_min(
        float_min(
            point_triangle_squared(px, py, pz, ax, ay, az, bx, by, bz, cx, cy, cz),
            point_triangle_squared(qx, qy, qz, ax, ay, az, bx, by, bz, cx, cy, cz),
        ),
        float_min(
            segment_segment_squared(px, py, pz, qx, qy, qz, ax, ay, az, bx, by, bz),
            float_min(
                segment_segment_squared(px, py, pz, qx, qy, qz, bx, by, bz, cx, cy, cz),
                segment_segment_squared(px, py, pz, qx, qy, qz, cx, cy, cz, ax, ay, az),
            ),
        ),
    )
}

fn corner(features: &Features, point: i64, fallback: (f64, f64, f64)) -> (f64, f64, f64) {
    if point >= 0 {
        let p = point as usize;
        (features.positions.x[p], features.positions.y[p], features.positions.z[p])
    } else {
        fallback
    }
}

fn feature_distance_squared(left: &Features, li: usize, right: &Features, ri: usize) -> f64 {
    let (lpa, lpb, rpa, rpb) = (left.a[li] as usize, left.b[li] as usize, right.a[ri] as usize, right.b[ri] as usize);
    let (lax, lay, laz) = (left.positions.x[lpa], left.positions.y[lpa], left.positions.z[lpa]);
    let (lbx, lby, lbz) = (left.positions.x[lpb], left.positions.y[lpb], left.positions.z[lpb]);
    let (rax, ray, raz) = (right.positions.x[rpa], right.positions.y[rpa], right.positions.z[rpa]);
    let (rbx, rby, rbz) = (right.positions.x[rpb], right.positions.y[rpb], right.positions.z[rpb]);
    let (lcx, lcy, lcz) = corner(left, left.c[li], (lbx, lby, lbz));
    let (rcx, rcy, rcz) = corner(right, right.c[ri], (rbx, rby, rbz));
    let left_kind = left.kinds[li];
    let right_kind = right.kinds[ri];
    if left_kind == SEGMENT && right_kind == SEGMENT {
        return segment_segment_squared(lax, lay, laz, lbx, lby, lbz, rax, ray, raz, rbx, rby, rbz);
    }
    if left_kind == SEGMENT {
        return segment_triangle_squared(lax, lay, laz, lbx, lby, lbz, rax, ray, raz, rbx, rby, rbz, rcx, rcy, rcz);
    }
    if right_kind == SEGMENT {
        return segment_triangle_squared(rax, ray, raz, rbx, rby, rbz, lax, lay, laz, lbx, lby, lbz, lcx, lcy, lcz);
    }
    let first = segment_triangle_squared(lax, lay, laz, lbx, lby, lbz, rax, ray, raz, rbx, rby, rbz, rcx, rcy, rcz);
    if first == 0.0 {
        return 0.0;
    }
    let second = segment_triangle_squared(lbx, lby, lbz, lcx, lcy, lcz, rax, ray, raz, rbx, rby, rbz, rcx, rcy, rcz);
    if second == 0.0 {
        return 0.0;
    }
    let third = segment_triangle_squared(lcx, lcy, lcz, lax, lay, laz, rax, ray, raz, rbx, rby, rbz, rcx, rcy, rcz);
    if third == 0.0 {
        return 0.0;
    }
    let fourth = segment_triangle_squared(rax, ray, raz, rbx, rby, rbz, lax, lay, laz, lbx, lby, lbz, lcx, lcy, lcz);
    if fourth == 0.0 {
        return 0.0;
    }
    let fifth = segment_triangle_squared(rbx, rby, rbz, rcx, rcy, rcz, lax, lay, laz, lbx, lby, lbz, lcx, lcy, lcz);
    if fifth == 0.0 {
        return 0.0;
    }
    let sixth = segment_triangle_squared(rcx, rcy, rcz, rax, ray, raz, lax, lay, laz, lbx, lby, lbz, lcx, lcy, lcz);
    float_min(first, float_min(second, float_min(third, float_min(fourth, float_min(fifth, sixth)))))
}

fn feature_aabb_into(features: &Features, feature: usize, bounds: &mut [f64; 6]) {
    let pa = features.a[feature] as usize;
    let pb = features.b[feature] as usize;
    let (ax, ay, az) = (features.positions.x[pa], features.positions.y[pa], features.positions.z[pa]);
    let (bx, by, bz) = (features.positions.x[pb], features.positions.y[pb], features.positions.z[pb]);
    let (cx, cy, cz) = corner(features, features.c[feature], (bx, by, bz));
    bounds[0] = float_min(ax, float_min(bx, cx));
    bounds[1] = float_min(ay, float_min(by, cy));
    bounds[2] = float_min(az, float_min(bz, cz));
    bounds[3] = float_max(ax, float_max(bx, cx));
    bounds[4] = float_max(ay, float_max(by, cy));
    bounds[5] = float_max(az, float_max(bz, cz));
}

fn aabb_distance_squared(index: &T, node: usize, q: &[f64; 6]) -> f64 {
    let dx = if q[3] < index.min_x[node] {
        index.min_x[node] - q[3]
    } else if q[0] > index.max_x[node] {
        q[0] - index.max_x[node]
    } else {
        0.0
    };
    let dy = if q[4] < index.min_y[node] {
        index.min_y[node] - q[4]
    } else if q[1] > index.max_y[node] {
        q[1] - index.max_y[node]
    } else {
        0.0
    };
    let dz = if q[5] < index.min_z[node] {
        index.min_z[node] - q[5]
    } else if q[2] > index.max_z[node] {
        q[2] - index.max_z[node]
    } else {
        0.0
    };
    (dx * dx) + (dy * dy) + (dz * dz)
}

#[allow(clippy::too_many_arguments)]
fn query_feature(
    index: &T,
    queries: &Features,
    query: usize,
    best_distance: &mut f64,
    best_ids: &mut [i64; 2],
    stack: &mut [i64; 128],
    bounds: &mut [f64; 6],
) -> Result<(), error::T> {
    if index.order.is_empty() {
        return Ok(());
    }
    feature_aabb_into(queries, query, bounds);
    let q = *bounds;
    let mut stack_size = 1_usize;
    stack[0] = 0;
    while stack_size > 0 {
        stack_size -= 1;
        let node = stack[stack_size] as usize;
        if aabb_distance_squared(index, node, &q) > *best_distance {
            continue;
        }
        if index.count[node] > 0 {
            let start = index.first[node];
            for at in start..start + index.count[node] {
                let feature = index.order[at as usize] as usize;
                let entity = index.features.entities[feature];
                let distance = feature_distance_squared(&index.features, feature, queries, query);
                if !distance.is_finite() {
                    return Err(failure("feature distance overflowed for finite input coordinates".to_owned()));
                }
                if distance < *best_distance
                    || (distance == *best_distance
                        && (best_ids[0] < 0
                            || entity < best_ids[0]
                            || (entity == best_ids[0] && (feature as i64) < best_ids[1])))
                {
                    *best_distance = distance;
                    best_ids[0] = entity;
                    best_ids[1] = feature as i64;
                }
            }
        } else {
            let left = index.left[node];
            let right = index.right[node];
            let ld = aabb_distance_squared(index, left as usize, &q);
            let rd = aabb_distance_squared(index, right as usize, &q);
            if ld <= *best_distance || rd <= *best_distance {
                if ld < rd || (ld == rd && left < right) {
                    if rd <= *best_distance {
                        stack[stack_size] = right;
                        stack_size += 1;
                    }
                    if ld <= *best_distance {
                        stack[stack_size] = left;
                        stack_size += 1;
                    }
                } else {
                    if ld <= *best_distance {
                        stack[stack_size] = left;
                        stack_size += 1;
                    }
                    if rd <= *best_distance {
                        stack[stack_size] = right;
                        stack_size += 1;
                    }
                }
            }
        }
    }
    Ok(())
}

/// For every query entity, the nearest source entity within `max_distance` (or -1) and the squared
/// distance (or infinity). Ties prefer the lower entity, then the lower feature.
pub fn nearest_entities_with_distances(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    max_distance: f64,
    source: &T,
    queries: &Features,
) -> Result<(Vec<i64>, Vec<f64>), error::T> {
    let grain = grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(failure("Proximity query: grain must be positive".to_owned()));
    }
    if !max_distance.is_finite() || max_distance < 0.0 || max_distance > f64::MAX.sqrt() {
        return Err(failure(
            "Proximity query: distance threshold must be finite, non-negative, and safely squarable".to_owned(),
        ));
    }
    let maximum = max_distance * max_distance;
    let count = queries.entity_count;
    let ranges = if count <= 0 { 0 } else { ((count - 1) / grain + 1) as usize };
    let slots: Vec<Mutex<Option<(Vec<i64>, Vec<f64>)>>> = (0..ranges).map(|_| Mutex::new(None)).collect();
    support::run_indexed_ranges(grain, count, &|range, first_entity, last_entity| {
        let length = (last_entity - first_entity) as usize;
        let mut out_ids = vec![-1_i64; length];
        let mut out_distances = vec![f64::INFINITY; length];
        let mut stack = [0_i64; 128];
        let mut bounds = [0.0_f64; 6];
        for entity in first_entity..last_entity {
            cancel::poll(cancel_value, entity, 4095)?;
            let mut best_distance = maximum;
            let mut best_ids = [-1_i64; 2];
            for feature in queries.offsets[entity as usize]..queries.offsets[entity as usize + 1] {
                query_feature(source, queries, feature as usize, &mut best_distance, &mut best_ids, &mut stack, &mut bounds)?;
            }
            if best_ids[0] >= 0 {
                out_ids[(entity - first_entity) as usize] = best_ids[0];
                out_distances[(entity - first_entity) as usize] = best_distance;
            }
        }
        *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some((out_ids, out_distances));
        Ok(())
    })?;
    let mut output = Vec::with_capacity(count as usize);
    let mut distances = Vec::with_capacity(count as usize);
    for slot in slots {
        if let Some((ids, range_distances)) = slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
            output.extend(ids);
            distances.extend(range_distances);
        }
    }
    Ok((output, distances))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn geometry_of(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>, vertices: Vec<i64>, offsets: Vec<i64>, kinds: Vec<u8>) -> geometry::T {
        let count = x.len() as i64;
        let positions = packed::Float3::View::of_owned_exn(x, y, z).unwrap();
        let topo = topology::View::create_validated_owned(count, vertices, offsets, kinds);
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    #[test]
    fn nearest_entity_prefers_lower_entity_on_ties() {
        // two parallel segments at x=0 and x=2, query segment at x=1 is equidistant
        let source = geometry_of(
            vec![0.0, 0.0, 2.0, 2.0],
            vec![0.0, 1.0, 0.0, 1.0],
            vec![0.0; 4],
            vec![0, 1, 2, 3],
            vec![0, 2, 4],
            vec![1, 1],
        );
        let query = geometry_of(vec![1.0, 1.0], vec![0.0, 1.0], vec![0.0; 2], vec![0, 1], vec![0, 2], vec![1]);
        let index = create(None, Some(1), primitive_features(None, Some(1), &source).unwrap()).unwrap();
        let features = primitive_features(None, Some(1), &query).unwrap();
        let (ids, distances) = nearest_entities_with_distances(None, Some(1), 5.0, &index, &features).unwrap();
        assert_eq!(ids, vec![0]);
        assert_eq!(distances, vec![1.0]);
        let (ids, distances) = nearest_entities_with_distances(None, None, 0.5, &index, &features).unwrap();
        assert_eq!(ids, vec![-1]);
        assert_eq!(distances, vec![f64::INFINITY]);
        assert!(nearest_entities_with_distances(None, Some(0), 0.5, &index, &features).is_err());
    }

    #[test]
    fn triangle_features_follow_ear_order_and_edges_are_unique() {
        let quad = geometry_of(
            vec![0.0, 1.0, 1.0, 0.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0; 4],
            vec![0, 1, 2, 3],
            vec![0, 4],
            vec![0],
        );
        let features = primitive_features(None, None, &quad).unwrap();
        assert_eq!(features.entities, vec![0, 0]);
        assert_eq!(features.kinds, vec![TRIANGLE, TRIANGLE]);
        assert_eq!(features.offsets, vec![0, 2]);
        let edges = edge_features(None, None, &quad).unwrap();
        assert_eq!(edges.entity_count, 4);
    }
}
