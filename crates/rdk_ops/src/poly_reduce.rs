use std::sync::Arc;

use rdk_core::support::Windows;
use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, support, topology, topology_index,
};
use rdk_core::error::Code;
use rdk_geom::point_grid::float_max;

use crate::{deform, edge_collapse, fuse_reduce, triangulate};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Target {
    ReduceRatio(f64),
    ReducePrimitiveCount(i64),
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub target: Target,
    pub primitives: Option<group::T>,
    pub hard_points: Option<group::T>,
    pub hard_edges: Option<edge_group::T>,
    pub preserve_boundary: bool,
    pub only_original_positions: bool,
    pub equalize_lengths: f64,
    pub max_normal_deviation: Option<f64>,
    pub output_group: Option<String>,
    pub recompute_point_normals: bool,
}

impl Params {
    #[cfg(test)]
    pub(crate) fn new(target: Target) -> Params {
        Params {
            cancel: None,
            grain: None,
            target,
            primitives: None,
            hard_points: None,
            hard_edges: None,
            preserve_boundary: true,
            only_original_positions: false,
            equalize_lengths: 1e-10,
            max_normal_deviation: None,
            output_group: None,
            recompute_point_normals: true,
        }
    }
}

struct Plan {
    edges: edge_group::T,
    removed_primitives: i64,
}

fn make(code: Code, message: &str) -> error::T {
    error::new("poly_reduce", code, message.to_owned())
}

fn fail(message: &str) -> error::T {
    make(Code::InvalidTopology, message)
}


fn validate_group(owner: group::Owner, length: i64, label: &str, item: Option<&group::T>) -> Result<(), error::T> {
    match item {
        Some(item) if group::owner(item) != owner || group::length(item) != length => Err(fail(&format!(
            "{label} must be a matching {} group",
            match owner {
                group::Owner::Point => "point",
                group::Owner::Vertex => "vertex",
                group::Owner::Primitive => "primitive",
            }
        ))),
        _ => Ok(()),
    }
}

fn validate_edge_group(
    topology_value: &topology::T,
    edge_count: i64,
    label: &str,
    item: Option<&edge_group::T>,
) -> Result<(), error::T> {
    match item {
        Some(item)
            if edge_group::topology_data_id(item) != topology::data_id(topology_value)
                || edge_group::length(item) != edge_count =>
        {
            Err(fail(&format!("{label} must belong to the input topology")))
        }
        _ => Ok(()),
    }
}

#[allow(clippy::too_many_arguments)]
fn quadric_error(
    q00: f64,
    q01: f64,
    q02: f64,
    q03: f64,
    q11: f64,
    q12: f64,
    q13: f64,
    q22: f64,
    q23: f64,
    q33: f64,
    x: f64,
    y: f64,
    z: f64,
) -> f64 {
    (q00 * x * x)
        + (2.0 * q01 * x * y)
        + (2.0 * q02 * x * z)
        + (2.0 * q03 * x)
        + (q11 * y * y)
        + (2.0 * q12 * y * z)
        + (2.0 * q13 * y)
        + (q22 * z * z)
        + (2.0 * q23 * z)
        + q33
}

struct Stamps {
    neighbor: Vec<i64>,
    primitive: Vec<i64>,
    stamp: i64,
}

fn link_valid(index: &topology_index::view::View, stamps: &mut Stamps, incidence: &[i64], edge: usize) -> bool {
    stamps.stamp += 1;
    let current = stamps.stamp;
    let a = i64::from(index.edge_a[edge]);
    let b = i64::from(index.edge_b[edge]);
    for slot in i64::from(index.point_edge_offsets[a as usize])..i64::from(index.point_edge_offsets[a as usize + 1]) {
        let adjacent = index.point_edges[slot as usize] as usize;
        let neighbor = if i64::from(index.edge_a[adjacent]) == a { i64::from(index.edge_b[adjacent]) } else { i64::from(index.edge_a[adjacent]) };
        if neighbor != b {
            stamps.neighbor[neighbor as usize] = current;
        }
    }
    let mut common = 0;
    for slot in i64::from(index.point_edge_offsets[b as usize])..i64::from(index.point_edge_offsets[b as usize + 1]) {
        let adjacent = index.point_edges[slot as usize] as usize;
        let neighbor = if i64::from(index.edge_a[adjacent]) == b { i64::from(index.edge_b[adjacent]) } else { i64::from(index.edge_a[adjacent]) };
        if neighbor != a && stamps.neighbor[neighbor as usize] == current {
            common += 1;
        }
    }
    common == incidence[edge]
}

struct Faces {
    a: Vec<f64>,
    b: Vec<f64>,
    c: Vec<f64>,
    valid: Vec<bool>,
}

#[allow(clippy::too_many_arguments)]
fn normal_valid(
    index: &topology_index::view::View,
    topology_view: &topology::view::View,
    positions: &packed::float3::view::View,
    coordinate_scale: f64,
    only_original_positions: bool,
    max_normal_deviation: Option<f64>,
    faces: &Faces,
    stamps: &mut Stamps,
    edge: usize,
) -> bool {
    let a = i64::from(index.edge_a[edge]);
    let b = i64::from(index.edge_b[edge]);
    let (au, bu) = (a as usize, b as usize);
    let ax = positions.x[au] / coordinate_scale;
    let ay = positions.y[au] / coordinate_scale;
    let az = positions.z[au] / coordinate_scale;
    let bx = positions.x[bu] / coordinate_scale;
    let by = positions.y[bu] / coordinate_scale;
    let bz = positions.z[bu] / coordinate_scale;
    let x = if only_original_positions { ax } else { (ax + bx) * 0.5 };
    let y = if only_original_positions { ay } else { (ay + by) * 0.5 };
    let z = if only_original_positions { az } else { (az + bz) * 0.5 };
    let cosine = match max_normal_deviation {
        None => 0.0,
        Some(angle) => angle.cos(),
    };
    let mut valid = true;
    let seen_stamp = stamps.stamp + 1;
    stamps.stamp = seen_stamp;
    for point in [a, b] {
        let point = point as usize;
        for slot in i64::from(index.point_offsets[point])..i64::from(index.point_offsets[point + 1]) {
            let primitive = index.primitive_of_vertex[index.point_vertices[slot as usize] as usize] as usize;
            if stamps.primitive[primitive] != seen_stamp {
                stamps.primitive[primitive] = seen_stamp;
                let first = topology_view.primitive_offsets[primitive] as usize;
                let p0 = i64::from(topology_view.vertex_points[first]);
                let p1 = i64::from(topology_view.vertex_points[first + 1]);
                let p2 = i64::from(topology_view.vertex_points[first + 2]);
                if !((p0 == a || p1 == a || p2 == a) && (p0 == b || p1 == b || p2 == b)) {
                    let coord = |point: i64, axis: usize| -> f64 {
                        if point == a || point == b {
                            if axis == 0 { x } else if axis == 1 { y } else { z }
                        } else if axis == 0 {
                            positions.x[point as usize] / coordinate_scale
                        } else if axis == 1 {
                            positions.y[point as usize] / coordinate_scale
                        } else {
                            positions.z[point as usize] / coordinate_scale
                        }
                    };
                    let (ax, ay, az) = (coord(p0, 0), coord(p0, 1), coord(p0, 2));
                    let (bx, by, bz) = (coord(p1, 0), coord(p1, 1), coord(p1, 2));
                    let (cx, cy, cz) = (coord(p2, 0), coord(p2, 1), coord(p2, 2));
                    let (ux, uy, uz) = (bx - ax, by - ay, bz - az);
                    let (vx, vy, vz) = (cx - ax, cy - ay, cz - az);
                    let scale = float_max(
                        ux.abs(),
                        float_max(
                            uy.abs(),
                            float_max(uz.abs(), float_max(vx.abs(), float_max(vy.abs(), vz.abs()))),
                        ),
                    );
                    if !faces.valid[primitive] {
                        valid = false;
                    } else {
                        let (onx, ony, onz) = (faces.a[primitive], faces.b[primitive], faces.c[primitive]);
                        if scale == 0.0 || !scale.is_finite() {
                            valid = false;
                        } else {
                            let (ux, uy, uz) = (ux / scale, uy / scale, uz / scale);
                            let (vx, vy, vz) = (vx / scale, vy / scale, vz / scale);
                            let nx = uy * vz - uz * vy;
                            let ny = uz * vx - ux * vz;
                            let nz = ux * vy - uy * vx;
                            let length = (nx * nx + ny * ny + nz * nz).sqrt();
                            if length == 0.0
                                || !length.is_finite()
                                || (onx * nx + ony * ny + onz * nz) / length < cosine
                            {
                                valid = false;
                            }
                        }
                    }
                }
            }
        }
    }
    valid
}

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
fn plan_round(
    cancel_ref: Option<&cancel::T>,
    params: &Params,
    primitive_selection: Option<&group::T>,
    hard_points: Option<&group::T>,
    hard_edges: Option<&edge_group::T>,
    primitive_budget: i64,
    geometry_value: &geometry::T,
) -> Result<Plan, error::T> {
    let grain = params.grain.unwrap_or(16_384).max(1);
    let equalize_lengths = params.equalize_lengths;
    let only_original_positions = params.only_original_positions;
    if primitive_budget <= 0 {
        return Err(fail("Rdk.Poly_reduce: primitive budget must be positive"));
    }
    if !equalize_lengths.is_finite() || equalize_lengths < 0.0 {
        return Err(fail("Rdk.Poly_reduce: equalize lengths must be finite and non-negative"));
    }
    if let Some(angle) = params.max_normal_deviation {
        if !angle.is_finite() || angle < 0.0 || angle > std::f64::consts::PI {
            return Err(fail("Rdk.Poly_reduce: normal deviation must be finite and in [0, pi]"));
        }
    }
    cancel::check_opt(cancel_ref)?;
    let topology_value = geometry::topology(geometry_value);
    if !topology::all_triangles(topology_value)? {
        return Err(fail("Rdk.Poly_reduce: the round planner requires triangle polygons"));
    }
    let topology_view = topology::view::view(topology_value)?;
    let index_value = topology_index::create(cancel_ref, topology_value)?;
    let index = topology_index::view::view(&index_value);
    let point_count = geometry::point_count(geometry_value) as usize;
    let primitive_count = geometry::primitive_count(geometry_value) as usize;
    let edge_count = index.edge_a.len();
    validate_group(group::Owner::Primitive, primitive_count as i64, "Rdk.Poly_reduce: primitive selection", primitive_selection)?;
    validate_group(group::Owner::Point, point_count as i64, "Rdk.Poly_reduce: hard points", hard_points)?;
    validate_edge_group(topology_value, edge_count as i64, "Rdk.Poly_reduce: hard edges", hard_edges)?;
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    support::parallel_for(cancel_ref, (grain / 3).max(1), point_count as i64, &|point| {
        let point = point as usize;
        if !(positions.x[point].is_finite() && positions.y[point].is_finite() && positions.z[point].is_finite()) {
            return Err(fail(&format!("Rdk.Poly_reduce: point {point} has a non-finite position")));
        }
        Ok(())
    })?;
    let mut coordinate_scale = 0.0f64;
    for point in 0..point_count {
        coordinate_scale = float_max(
            coordinate_scale,
            float_max(
                positions.x[point].abs(),
                float_max(positions.y[point].abs(), positions.z[point].abs()),
            ),
        );
    }
    let coordinate_scale = if coordinate_scale == 0.0 { 1.0 } else { coordinate_scale };
    let (mut face_a, mut face_b, mut face_c, mut face_d) =
        (vec![0.0f64; primitive_count], vec![0.0f64; primitive_count], vec![0.0f64; primitive_count], vec![0.0f64; primitive_count]);
    let mut face_valid = vec![false; primitive_count];
    let face_grain = (grain / 3).max(1);
    let (wa, wb, wc, wd, wvalid) = (
        Windows::even(&mut face_a, face_grain),
        Windows::even(&mut face_b, face_grain),
        Windows::even(&mut face_c, face_grain),
        Windows::even(&mut face_d, face_grain),
        Windows::even(&mut face_valid, face_grain),
    );
    support::run_indexed_ranges(face_grain, primitive_count as i64, &|range, begin, end| {
      let (face_a, face_b, face_c) = (wa.take(range), wb.take(range), wc.take(range));
      let (face_d, face_valid) = (wd.take(range), wvalid.take(range));
      for primitive in begin..end {
        support::poll(cancel_ref, primitive)?;
        let slot = (primitive - begin) as usize;
        let primitive = primitive as usize;
        let first = topology_view.primitive_offsets[primitive] as usize;
        let p0 = topology_view.vertex_points[first] as usize;
        let p1 = topology_view.vertex_points[first + 1] as usize;
        let p2 = topology_view.vertex_points[first + 2] as usize;
        let ax = positions.x[p0] / coordinate_scale;
        let ay = positions.y[p0] / coordinate_scale;
        let az = positions.z[p0] / coordinate_scale;
        let ux = positions.x[p1] / coordinate_scale - ax;
        let uy = positions.y[p1] / coordinate_scale - ay;
        let uz = positions.z[p1] / coordinate_scale - az;
        let vx = positions.x[p2] / coordinate_scale - ax;
        let vy = positions.y[p2] / coordinate_scale - ay;
        let vz = positions.z[p2] / coordinate_scale - az;
        let nx = uy * vz - uz * vy;
        let ny = uz * vx - ux * vz;
        let nz = ux * vy - uy * vx;
        let scale = float_max(nx.abs(), float_max(ny.abs(), nz.abs()));
        if scale != 0.0 && scale.is_finite() {
            let (nx, ny, nz) = (nx / scale, ny / scale, nz / scale);
            let length = (nx * nx + ny * ny + nz * nz).sqrt();
            if length != 0.0 && length.is_finite() {
                let (a, b, c) = (nx / length, ny / length, nz / length);
                face_a[slot] = a;
                face_b[slot] = b;
                face_c[slot] = c;
                face_d[slot] = -(a * ax + b * ay + c * az);
                face_valid[slot] = true;
            }
        }
      }
      Ok(())
    })?;
    let faces = Faces { a: face_a, b: face_b, c: face_c, valid: face_valid };
    // Quadric planes per point: q[0..10] = q00 q01 q02 q03 q11 q12 q13 q22 q23 q33.
    let mut q = vec![[0.0f64; 10]; point_count];
    support::par_fill(cancel_ref, (grain / 6).max(1), &mut q, &|point| {
        let point = point as usize;
        let mut entry = [0.0f64; 10];
        for slot in i64::from(index.point_offsets[point])..i64::from(index.point_offsets[point + 1]) {
            let vertex = index.point_vertices[slot as usize] as usize;
            let primitive = index.primitive_of_vertex[vertex] as usize;
            if faces.valid[primitive] {
                let (a, b, c, d) = (faces.a[primitive], faces.b[primitive], faces.c[primitive], face_d[primitive]);
                entry[0] += a * a;
                entry[1] += a * b;
                entry[2] += a * c;
                entry[3] += a * d;
                entry[4] += b * b;
                entry[5] += b * c;
                entry[6] += b * d;
                entry[7] += c * c;
                entry[8] += c * d;
                entry[9] += d * d;
            }
        }
        Ok(entry)
    })?;
    let mut locked = vec![false; point_count];
    if let Some(item) = hard_points {
        for point in 0..point_count {
            if group::mem(point as i64, item) {
                locked[point] = true;
            }
        }
    }
    if let Some(item) = hard_edges {
        for edge in 0..edge_count {
            if edge_group::mem(edge as i64, item) {
                locked[index.edge_a[edge] as usize] = true;
                locked[index.edge_b[edge] as usize] = true;
            }
        }
    }
    for edge in 0..edge_count {
        let edge_incidence = i64::from(index.edge_offsets[edge + 1]) - i64::from(index.edge_offsets[edge]);
        if edge_incidence > 2 || (params.preserve_boundary && edge_incidence == 1) {
            locked[index.edge_a[edge] as usize] = true;
            locked[index.edge_b[edge] as usize] = true;
        }
    }
    if let Some(item) = primitive_selection {
        for point in 0..point_count {
            let mut slot = i64::from(index.point_offsets[point]);
            while !locked[point] && slot < i64::from(index.point_offsets[point + 1]) {
                let primitive = i64::from(index.primitive_of_vertex[index.point_vertices[slot as usize] as usize]);
                if !group::mem(primitive, item) {
                    locked[point] = true;
                }
                slot += 1;
            }
        }
    }
    let mut costs = vec![f64::INFINITY; edge_count];
    let mut incidence = vec![0i64; edge_count];
    let eligible_primitives = |edge: usize| -> bool {
        let first = i64::from(index.edge_offsets[edge]);
        let last = i64::from(index.edge_offsets[edge + 1]);
        let count = last - first;
        count >= 1
            && count <= 2
            && match primitive_selection {
                None => true,
                Some(item) => (first..last).all(|slot| {
                    let primitive = i64::from(index.primitive_of_vertex[index.edge_vertices[slot as usize] as usize]);
                    group::mem(primitive, item)
                }),
            }
    };
    let (wcosts, wincidence) = (Windows::even(&mut costs, grain), Windows::even(&mut incidence, grain));
    support::run_indexed_ranges(grain, edge_count as i64, &|range, begin, end| {
      let (costs, incidence) = (wcosts.take(range), wincidence.take(range));
      for edge in begin..end {
        support::poll(cancel_ref, edge)?;
        let slot = (edge - begin) as usize;
        let edge = edge as usize;
        let a = index.edge_a[edge] as usize;
        let b = index.edge_b[edge] as usize;
        let count = i64::from(index.edge_offsets[edge + 1]) - i64::from(index.edge_offsets[edge]);
        incidence[slot] = count;
        if a != b && !locked[a] && !locked[b] && eligible_primitives(edge) {
            let ax = positions.x[a] / coordinate_scale;
            let ay = positions.y[a] / coordinate_scale;
            let az = positions.z[a] / coordinate_scale;
            let bx = positions.x[b] / coordinate_scale;
            let by = positions.y[b] / coordinate_scale;
            let bz = positions.z[b] / coordinate_scale;
            let x = if only_original_positions { ax } else { (ax + bx) * 0.5 };
            let y = if only_original_positions { ay } else { (ay + by) * 0.5 };
            let z = if only_original_positions { az } else { (az + bz) * 0.5 };
            let mut sum = [0.0f64; 10];
            for k in 0..10 {
                sum[k] = q[a][k] + q[b][k];
            }
            let (dx, dy, dz) = (bx - ax, by - ay, bz - az);
            let err = quadric_error(
                sum[0], sum[1], sum[2], sum[3], sum[4], sum[5], sum[6], sum[7], sum[8], sum[9], x, y, z,
            ) + (equalize_lengths * (dx * dx + dy * dy + dz * dz));
            if err.is_finite() {
                costs[slot] = float_max(0.0, err);
            }
        }
      }
      Ok(())
    })?;
    let mut order: Vec<usize> = (0..edge_count).collect();
    order.sort_by(|&left, &right| {
        costs[left].total_cmp(&costs[right]).then(left.cmp(&right))
    });
    let mut stamps = Stamps { neighbor: vec![0; point_count], primitive: vec![0; primitive_count], stamp: 0 };
    let mut selected = vec![0u8; edge_count.div_ceil(8)];
    let mut reserved = vec![false; point_count];
    let mut removed = 0i64;
    let mut ordinal = 0usize;
    while ordinal < edge_count && removed < primitive_budget {
        cancel::poll(cancel_ref, ordinal as i64, 4095)?;
        let edge = order[ordinal];
        let a = index.edge_a[edge] as usize;
        let b = index.edge_b[edge] as usize;
        let reduction = incidence[edge];
        if costs[edge].is_finite()
            && !reserved[a]
            && !reserved[b]
            && (reduction <= primitive_budget - removed || removed == 0)
            && link_valid(&index, &mut stamps, &incidence, edge)
            && normal_valid(
                &index,
                &topology_view,
                &positions,
                coordinate_scale,
                only_original_positions,
                params.max_normal_deviation,
                &faces,
                &mut stamps,
                edge,
            )
        {
            selected[edge >> 3] |= 1 << (edge & 7);
            removed += reduction;
            for point in [a, b] {
                reserved[point] = true;
                for slot in i64::from(index.point_edge_offsets[point])..i64::from(index.point_edge_offsets[point + 1]) {
                    let adjacent = index.point_edges[slot as usize] as usize;
                    reserved[index.edge_a[adjacent] as usize] = true;
                    reserved[index.edge_b[adjacent] as usize] = true;
                }
            }
        }
        ordinal += 1;
    }
    let edges =
        edge_group::view::of_owned_bits(topology_value, edge_count as i64, "__rdk_poly_reduce_round", selected)?;
    Ok(Plan { edges, removed_primitives: removed })
}

fn unique_group_name(owner: group::Owner, base: &str, geometry_value: &geometry::T) -> String {
    let mut suffix = 0;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}{suffix}") };
        if geometry::find_group(owner, &name, geometry_value).is_none() {
            return name;
        }
        suffix += 1;
    }
}

fn unique_edge_name(base: &str, geometry_value: &geometry::T) -> String {
    let mut suffix = 0;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}{suffix}") };
        if geometry::find_edge_group(&name, geometry_value).is_none() {
            return name;
        }
        suffix += 1;
    }
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(16_384);
    support::check_positive(grain).map_err(|_| make(Code::InvalidTopology, "Rdk.Poly_reduce.poly_reduce: grain must be positive"))?;
    run_inner(params, grain, geometry_value).map_err(|value| error::guard(value, "poly_reduce", Code::InvalidTopology))
}

#[allow(clippy::too_many_lines)]
fn run_inner(params: &Params, grain: i64, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_ref = params.cancel.as_deref();
    let original_primitives = geometry::primitive_count(geometry_value);
    let original_points = geometry::point_count(geometry_value);
    let original_topology = geometry::topology(geometry_value);
    match params.target {
        Target::ReduceRatio(ratio) if !ratio.is_finite() || ratio < 0.0 || ratio > 1.0 => {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: ratio must be finite and in [0, 1]"));
        }
        Target::ReducePrimitiveCount(count) if count < 0 => {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: target primitive count must be non-negative"));
        }
        _ => {}
    }
    if let Some(name) = &params.output_group {
        if (name).trim().is_empty() {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: output group name must not be empty"));
        }
    }
    if let Some(item) = &params.primitives {
        if group::owner(item) != group::Owner::Primitive || group::length(item) != original_primitives {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: selection must be a matching primitive group"));
        }
    }
    if let Some(item) = &params.hard_points {
        if group::owner(item) != group::Owner::Point || group::length(item) != original_points {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: hard points must be a matching point group"));
        }
    }
    let original_index = topology_index::create(cancel_ref, original_topology)?;
    if let Some(item) = &params.hard_edges {
        if edge_group::topology_data_id(item) != topology::data_id(original_topology)
            || edge_group::length(item) != topology_index::edge_count(&original_index)
        {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: hard edges must belong to the input topology"));
        }
    }
    let requested_original = match params.target {
        Target::ReduceRatio(ratio) => (ratio * original_primitives as f64).ceil() as i64,
        Target::ReducePrimitiveCount(count) => count,
    };
    if requested_original >= original_primitives {
        return Ok(geometry_value.clone());
    }
    cancel::check_opt(cancel_ref)?;
    let mut geometry_work = geometry_value.clone();
    let mut primitive_name: Option<String> = None;
    if let Some(item) = &params.primitives {
        let name = unique_group_name(group::Owner::Primitive, "__rdk_poly_reduce_primitives", &geometry_work);
        geometry_work = geometry::with_group(group::with_name(&name, item)?, &geometry_work)?;
        primitive_name = Some(name);
    }
    let mut hard_point_name: Option<String> = None;
    if let Some(item) = &params.hard_points {
        let name = unique_group_name(group::Owner::Point, "__rdk_poly_reduce_hard_points", &geometry_work);
        geometry_work = geometry::with_group(group::with_name(&name, item)?, &geometry_work)?;
        hard_point_name = Some(name);
    }
    let mut hard_edge_name: Option<String> = None;
    if let Some(item) = &params.hard_edges {
        let name = unique_edge_name("__rdk_poly_reduce_hard_edges", &geometry_work);
        geometry_work = geometry::with_edge_group(edge_group::with_name(&name, item)?, &geometry_work)?;
        hard_edge_name = Some(name);
    }
    let triangulated = triangulate::run(
        &triangulate::Params { cancel: params.cancel.clone(), grain: Some(grain), primitives: None },
        &geometry_work,
    )?;
    let working_count = geometry::primitive_count(&triangulated);
    let target_count = match params.target {
        Target::ReduceRatio(ratio) => (ratio * working_count as f64).ceil() as i64,
        Target::ReducePrimitiveCount(count) => count,
    };
    let had_point_normals = geometry::find_attribute(attribute::Owner::Point, "N", &geometry_work).is_some();
    let mut current = triangulated.clone();
    let mut round = 0;
    loop {
        cancel::check_opt(cancel_ref)?;
        let count = geometry::primitive_count(&current);
        if count <= target_count {
            break;
        }
        if round > 128 {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: adaptive reduction exceeded 128 rounds"));
        }
        let primitive_selection = primitive_name
            .as_ref()
            .and_then(|name| geometry::find_group(group::Owner::Primitive, name, &current));
        let hard_points = hard_point_name
            .as_ref()
            .and_then(|name| geometry::find_group(group::Owner::Point, name, &current));
        let hard_edges = hard_edge_name.as_ref().and_then(|name| geometry::find_edge_group(name, &current));
        let plan = plan_round(
            cancel_ref,
            params,
            primitive_selection.as_ref(),
            hard_points.as_ref(),
            hard_edges.as_ref(),
            count - target_count,
            &current,
        )?;
        if plan.removed_primitives == 0 {
            break;
        }
        let position = if params.only_original_positions {
            fuse_reduce::Position::LeastPoint
        } else {
            fuse_reduce::Position::Average
        };
        let next = edge_collapse::raw(
            cancel_ref,
            Some(grain),
            Some(&plan.edges),
            None,
            Some(position),
            Some(true),
            Some(false),
            &current,
        )?;
        if geometry::primitive_count(&next) >= count {
            return Err(fail("Rdk.Poly_reduce.poly_reduce: a planned contraction made no progress"));
        }
        current = next;
        round += 1;
    }
    let output = current;
    if geometry::data_id(&output) == geometry::data_id(&triangulated)
        && topology::data_id(geometry::topology(&triangulated)) == topology::data_id(original_topology)
    {
        return match &params.output_group {
            None => Ok(geometry_value.clone()),
            Some(name) => {
                let item = match &params.primitives {
                    Some(item) => group::with_name(name, item)?,
                    None => group::init(Some(grain), group::Owner::Primitive, name, original_primitives, &|_| Ok(true))?,
                };
                geometry::with_group(item, geometry_value)
            }
        };
    }
    let mut output = output;
    if let Some(name) = &params.output_group {
        let item = match &primitive_name {
            Some(source_name) => match geometry::find_group(group::Owner::Primitive, source_name, &output) {
                Some(found) => group::with_name(name, &found)?,
                None => group::init(
                    Some(grain),
                    group::Owner::Primitive,
                    name,
                    geometry::primitive_count(&output),
                    &|_| Ok(false),
                )?,
            },
            None => group::init(
                Some(grain),
                group::Owner::Primitive,
                name,
                geometry::primitive_count(&output),
                &|_| Ok(true),
            )?,
        };
        output = geometry::with_group(item, &output)?;
    }
    if let Some(name) = &primitive_name {
        output = geometry::without_group(group::Owner::Primitive, name, &output);
    }
    if let Some(name) = &hard_point_name {
        output = geometry::without_group(group::Owner::Point, name, &output);
    }
    if let Some(name) = &hard_edge_name {
        output = geometry::without_edge_group(name, &output);
    }
    output = geometry::without_attribute(attribute::Owner::Point, "N", &output);
    output = geometry::without_attribute(attribute::Owner::Vertex, "N", &output);
    if params.recompute_point_normals && had_point_normals {
        return deform::normals(
            deform::Params { cancel: params.cancel.clone(), grain, primitives: None },
            &output,
        );
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plane_clip::test_util::*;
    use rdk_core::topology::PrimitiveKind;

    /// Plane grid over `columns` x `rows` points with alternating diagonals, spanning [-4, 4].
    fn grid(columns: usize, rows: usize) -> geometry::T {
        let (mut x, mut z) = (Vec::new(), Vec::new());
        for r in 0..rows {
            for c in 0..columns {
                x.push(-4.0 + 8.0 * c as f64 / (columns - 1) as f64);
                z.push(-4.0 + 8.0 * r as f64 / (rows - 1) as f64);
            }
        }
        let width = columns as i64;
        let mut polys = Vec::new();
        for r in 0..(rows - 1) as i64 {
            for c in 0..(columns - 1) as i64 {
                let p = r * width + c;
                if (r + c) % 2 == 0 {
                    polys.push(vec![p, p + width, p + width + 1]);
                    polys.push(vec![p, p + width + 1, p + 1]);
                } else {
                    polys.push(vec![p, p + width, p + 1]);
                    polys.push(vec![p + 1, p + width, p + width + 1]);
                }
            }
        }
        polygons(x, vec![0.0; columns * rows], z, &polys)
    }

    fn sorted_boundary(source: &geometry::T) -> Vec<(u64, u64, u64)> {
        let (x, y, z) = xyz(source);
        let mut out: Vec<(u64, u64, u64)> = (0..x.len())
            .filter(|&p| x[p].abs() == 4.0 || z[p].abs() == 4.0)
            .map(|p| (x[p].to_bits(), y[p].to_bits(), z[p].to_bits()))
            .collect();
        out.sort();
        out
    }

    #[test]
    fn reduces_and_preserves_boundary() {
        let source = grid(24, 18);
        let source_count = geometry::primitive_count(&source);
        let mut p = Params::new(Target::ReduceRatio(0.5));
        p.grain = Some(17);
        p.equalize_lengths = 1e-10;
        let output = run(&p, &source).unwrap();
        assert!(geometry::primitive_count(&output) < source_count, "did not reduce a dense triangle grid");
        assert!(geometry::primitive_count(&output) >= source_count / 2 - 1, "overshot its target");
        assert_eq!(sorted_boundary(&source), sorted_boundary(&output), "changed a preserved open boundary");
    }

    #[test]
    fn original_positions_and_constraints() {
        let source = grid(18, 14);
        let (sx, sy, sz) = xyz(&source);
        let hard = group::init(None, group::Owner::Point, "pin", geometry::point_count(&source), &|p| Ok(p == 100)).unwrap();
        let mut p = Params::new(Target::ReduceRatio(0.35));
        p.grain = Some(11);
        p.equalize_lengths = 1e-10;
        p.preserve_boundary = false;
        p.hard_points = Some(hard);
        p.only_original_positions = true;
        p.output_group = Some("reduced".to_owned());
        let output = run(&p, &source).unwrap();
        let reduced = geometry::find_group(group::Owner::Primitive, "reduced", &output).unwrap();
        assert_eq!(group::cardinality(&reduced), geometry::primitive_count(&output));
        let (ox, oy, oz) = xyz(&output);
        let same = |a: usize, b: usize| ox[a] == sx[b] && oy[a] == sy[b] && oz[a] == sz[b];
        for point in 0..ox.len() {
            assert!((0..sx.len()).any(|s| same(point, s)), "original-position mode invented a position");
        }
        assert!((0..ox.len()).any(|point| same(point, 100)), "removed a hard point");
    }

    #[test]
    fn payload_group_and_edge_ancestry() {
        let base = grid(20, 16);
        let (points, vertices, primitives) =
            (geometry::point_count(&base), geometry::vertex_count(&base), geometry::primitive_count(&base));
        let point_id =
            attribute::create_owned("point_id", attribute::Owner::Point, attribute::Storage::Int(Arc::new((0..points).collect()))).unwrap();
        let corner_id =
            attribute::create_owned("corner_id", attribute::Owner::Vertex, attribute::Storage::Int(Arc::new((0..vertices).collect())))
                .unwrap();
        let source_face = attribute::create_owned(
            "source_face",
            attribute::Owner::Primitive,
            attribute::Storage::Int(Arc::new((0..primitives).collect())),
        )
        .unwrap();
        let detail = attribute::create_owned(
            "tag",
            attribute::Owner::Detail,
            attribute::Storage::Text(std::sync::Arc::new(vec![b"reduce-fixture".to_vec()].into())),
        )
        .unwrap();
        let ordered = group::ordered(group::Owner::Point, "ordered_points", points, &[3, 7, 11, 15, 19]).unwrap();
        let topology_value = geometry::topology(&base).clone();
        let index = topology_index::create(None, &topology_value).unwrap();
        let boundary = edge_group::init(None, &topology_value, &index, "boundary", &|edge| Ok({
            topology_index::edge_incidence_count(&index, edge).unwrap() == 1
        }))
        .unwrap();
        let source = geometry::create(
            geometry::positions(&base).clone(),
            topology_value,
            Some(vec![point_id, corner_id, source_face, detail.clone()]),
            Some(vec![ordered]),
            Some(vec![boundary.clone()]),
        )
        .unwrap();
        let mut p = Params::new(Target::ReduceRatio(0.45));
        p.grain = Some(23);
        p.equalize_lengths = 1e-10;
        let output = run(&p, &source).unwrap();
        let find = |owner, name| geometry::find_attribute(owner, name, &output).unwrap();
        assert!(
            attribute::length(&find(attribute::Owner::Point, "point_id")) == geometry::point_count(&output)
                && attribute::length(&find(attribute::Owner::Vertex, "corner_id")) == geometry::vertex_count(&output)
                && attribute::length(&find(attribute::Owner::Primitive, "source_face")) == geometry::primitive_count(&output),
            "payload cardinality"
        );
        assert_eq!(
            attribute::storage_id(&find(attribute::Owner::Detail, "tag")),
            attribute::storage_id(&detail),
            "did not structurally share detail payload"
        );
        let ordered_out = geometry::find_group(group::Owner::Point, "ordered_points", &output).expect("dropped ordered group");
        assert!(group::is_ordered(&ordered_out), "lost ordered-group traversal");
        let boundary_out = geometry::find_edge_group("boundary", &output).expect("dropped boundary ancestry");
        assert_eq!(edge_group::cardinality(&boundary_out), edge_group::cardinality(&boundary));
    }

    #[test]
    fn noop_invalid_and_cancel() {
        let source = grid(8, 7);
        let mut p = Params::new(Target::ReduceRatio(1.0));
        p.equalize_lengths = 1e-10;
        assert_eq!(geometry::data_id(&run(&p, &source).unwrap()), geometry::data_id(&source));
        let all_hard =
            group::init(None, group::Owner::Point, "all_hard", geometry::point_count(&source), &|_| Ok(true)).unwrap();
        let mut p = Params::new(Target::ReduceRatio(0.0));
        p.hard_points = Some(all_hard);
        assert_eq!(geometry::data_id(&run(&p, &source).unwrap()), geometry::data_id(&source));
        let wrong = group::init(None, group::Owner::Point, "wrong", geometry::point_count(&source), &|_| Ok(true)).unwrap();
        let mut p = Params::new(Target::ReduceRatio(0.5));
        p.primitives = Some(wrong);
        assert!(run(&p, &source).is_err(), "accepted a point-owned primitive selection");
        let mut p = Params::new(Target::ReduceRatio(0.5));
        p.equalize_lengths = -1.0;
        assert!(run(&p, &source).is_err(), "accepted a negative equal-length weight");
        let mut p = Params::new(Target::ReduceRatio(0.5));
        p.max_normal_deviation = Some(std::f64::consts::PI + 0.01);
        assert!(run(&p, &source).is_err(), "accepted an invalid normal-deviation limit");
        let curve = geom(
            vec![0.0, 1.0, 2.0],
            vec![0.0; 3],
            vec![0.0; 3],
            &[(PrimitiveKind::OpenPolyline, vec![0, 1, 2])],
        );
        let p = Params::new(Target::ReducePrimitiveCount(0));
        assert!(run(&p, &curve).is_err(), "accepted curve topology");
        let (mut x, y, z) = xyz(&source);
        x[3] = f64::NAN;
        let non_finite = geometry::with_positions(packed::float3::view::of_owned_exn(x, y, z).unwrap(), &source).unwrap();
        assert!(run(&Params::new(Target::ReduceRatio(0.5)), &non_finite).is_err(), "accepted a non-finite position");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut p = Params::new(Target::ReduceRatio(0.5));
        p.cancel = Some(token);
        expect_code(run(&p, &source), Code::Cancelled);
    }

    #[test]
    fn parallel_exact() {
        let source = grid(80, 60);
        let build = || {
            let mut p = Params::new(Target::ReduceRatio(0.42));
            p.grain = Some(127);
            p.equalize_lengths = 1e-10;
            p.preserve_boundary = false;
            run(&p, &source).unwrap()
        };
        assert!(same_geometry(&with_domains(1, &build), &with_domains(4, &build)));
    }

    #[test]
    fn extreme_coordinate_normalization() {
        let limit = f64::MAX;
        let source = polygons(
            vec![-limit, limit, limit, -limit],
            vec![0.0; 4],
            vec![-limit, -limit, limit, limit],
            &[vec![0, 1, 2], vec![0, 2, 3]],
        );
        let mut p = Params::new(Target::ReducePrimitiveCount(1));
        p.preserve_boundary = false;
        p.equalize_lengths = 1e-10;
        let output = run(&p, &source).unwrap();
        let (x, y, z) = xyz(&output);
        for i in 0..x.len() {
            assert!(x[i].is_finite() && y[i].is_finite() && z[i].is_finite(), "non-finite output");
        }
    }

    #[test]
    fn parallel_exact_at_scale() {
        use crate::compact_points::fixtures::exact_at_one_and_four;
        let source = grid(70, 60);
        let source_count = geometry::primitive_count(&source);
        let mut p = Params::new(Target::ReduceRatio(0.5));
        p.grain = Some(257);
        p.equalize_lengths = 1e-10;
        let out = exact_at_one_and_four(&|| run(&p, &source).unwrap());
        assert!(geometry::primitive_count(&out) < source_count);
    }
}
