//! Geometry adapter for the shared packed planar Delaunay kernel.
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use rays_math::vec3;
use rdk_core::{
    attribute, cancel, edge_group, element_selection, error, geometry, group, normal_ops, packed, support,
    topology, topology_index, topology_remap,
};
use rdk_core::error::Code;
use rdk_geom::implicit_point;
use rdk_geom::delaunay2;
use rdk_geom::planar_cdt;
use rdk_geom::planar_constraints;
use rdk_geom::planar_refinement;
use rdk_geom::predicates::{self, Sign};

use crate::compact_points;
use crate::deletion;

const OPERATION: &str = "Rdk_mesh.Triangulate2d.triangulate_2d";
const GUARD_OPERATION: &str = "triangulate_2d";


pub enum Projection {
    BestFit,
    PlaneXy,
    PlaneYz,
    PlaneZx,
    Plane { origin: vec3::T, normal: vec3::T },
    PointAttribute(String),
}

/// Every field is required in OCaml; `Default` carries the defaults the tests rely on.
pub struct Params {
    pub grain: i64,
    pub selection: Option<element_selection::T>,
    pub constraint_edges: Option<edge_group::T>,
    pub constraint_primitives: Option<group::T>,
    pub projection: Projection,
    pub seed: i64,
    pub split_crossing_constraints: bool,
    pub flood_from_hull_boundary: bool,
    pub remove_outside_constraint_polygons: bool,
    pub silhouette_constraints: bool,
    pub remove_outside_silhouette: bool,
    pub ignore_non_constraint_points: bool,
    pub remove_duplicate_points: bool,
    pub refine: bool,
    pub allow_constraint_splitting: bool,
    pub minimum_angle: f64,
    pub maximum_area: Option<f64>,
    pub target_edge_length: Option<f64>,
    pub minimum_edge_length: f64,
    pub maximum_new_points: i64,
    pub regularization_steps: i64,
    pub allow_movement_of_interior_input_points: bool,
    pub preserve_point_payload: bool,
    pub restore_original_point_positions: bool,
    pub keep_primitives: bool,
    pub split_point_group: Option<String>,
    pub refinement_point_group: Option<String>,
    pub triangle_group: Option<String>,
    pub constraint_group: Option<String>,
    pub remove_unused_points: bool,
    pub recompute_point_normals: bool,
}

impl Default for Params {
    fn default() -> Params {
        Params {
            grain: 16_384,
            selection: None,
            constraint_edges: None,
            constraint_primitives: None,
            projection: Projection::BestFit,
            seed: 0,
            split_crossing_constraints: false,
            flood_from_hull_boundary: false,
            remove_outside_constraint_polygons: false,
            silhouette_constraints: false,
            remove_outside_silhouette: false,
            ignore_non_constraint_points: false,
            remove_duplicate_points: false,
            refine: false,
            allow_constraint_splitting: true,
            minimum_angle: std::f64::consts::PI / 9.0,
            maximum_area: None,
            target_edge_length: None,
            minimum_edge_length: 0.0,
            maximum_new_points: 100_000,
            regularization_steps: 0,
            allow_movement_of_interior_input_points: false,
            preserve_point_payload: true,
            restore_original_point_positions: true,
            keep_primitives: false,
            split_point_group: None,
            refinement_point_group: None,
            triangle_group: None,
            constraint_group: None,
            remove_unused_points: false,
            recompute_point_normals: false,
        }
    }
}

fn make(code: Code, message: String) -> error::T {
    error::new(GUARD_OPERATION, code, message)
}

/// `Error.guard ~code:"invalid_triangulation"`: every OCaml string error.
fn fail(message: String) -> error::T {
    make(Code::InvalidTriangulation, message)
}

fn invalid(message: &str) -> error::T {
    fail(format!("{OPERATION}: {message}"))
}

/// A core error that was a plain string in OCaml, inside the `try` that reports cancellation as
/// a string error.
fn lift(value: error::T) -> error::T {
    if error::is_cancelled(&value) {
        fail(format!("{OPERATION}: triangulation was cancelled"))
    } else {
        fail(error::message(&value).to_owned())
    }
}

/// `Error.get`: cancellation stays cancellation, everything else becomes a failure message.
fn get_core(value: error::T) -> error::T {
    if error::is_cancelled(&value) {
        error::cancelled(GUARD_OPERATION)
    } else {
        fail(error::message(&value).to_owned())
    }
}

/// Core errors that OCaml raised as Rdk exceptions keep their identity, except cancellation.
fn pass(value: error::T) -> error::T {
    if error::is_cancelled(&value) {
        fail(format!("{OPERATION}: triangulation was cancelled"))
    } else {
        value
    }
}

fn snapshot(cancel_value: Option<&cancel::T>) -> Option<Arc<cancel::T>> {
    cancel_value.map(|value| {
        let copy = cancel::create();
        if cancel::is_cancelled(value) {
            cancel::cancel(&copy);
        }
        Arc::new(copy)
    })
}

fn finite3(x: f64, y: f64, z: f64) -> bool {
    x.is_finite() && y.is_finite() && z.is_finite()
}

fn normalize3(x: f64, y: f64, z: f64) -> Result<(f64, f64, f64), error::T> {
    let scale = x.abs().max(y.abs().max(z.abs()));
    if scale == 0.0 || !scale.is_finite() {
        return Err(invalid("plane normal must be finite and nonzero"));
    }
    let (x, y, z) = (x / scale, y / scale, z / scale);
    let length = ((x * x) + (y * y) + (z * z)).sqrt();
    Ok((x / length, y / length, z / length))
}

type Triple = (f64, f64, f64);

fn frame_of_normal(normal: vec3::T) -> Result<(Triple, Triple), error::T> {
    let (nx, ny, nz) = vec3::to_triple(normal);
    let (nx, ny, nz) = normalize3(nx, ny, nz)?;
    let (ax, ay, az) = if nx.abs() <= ny.abs() && nx.abs() <= nz.abs() {
        (1.0, 0.0, 0.0)
    } else if ny.abs() <= nz.abs() {
        (0.0, 1.0, 0.0)
    } else {
        (0.0, 0.0, 1.0)
    };
    let ux = (ay * nz) - (az * ny);
    let uy = (az * nx) - (ax * nz);
    let uz = (ax * ny) - (ay * nx);
    let (ux, uy, uz) = normalize3(ux, uy, uz)?;
    let vx = (ny * uz) - (nz * uy);
    let vy = (nz * ux) - (nx * uz);
    let vz = (nx * uy) - (ny * ux);
    Ok(((ux, uy, uz), (vx, vy, vz)))
}

fn smallest_eigenvector(xx: f64, xy: f64, xz: f64, yy: f64, yz: f64, zz: f64) -> vec3::T {
    let mut matrix = [xx, xy, xz, xy, yy, yz, xz, yz, zz];
    let mut vectors = [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0];
    for _ in 0..=15 {
        let (p, q) = {
            let a01 = matrix[1].abs();
            let a02 = matrix[2].abs();
            let a12 = matrix[5].abs();
            if a01 >= a02 && a01 >= a12 {
                (0, 1)
            } else if a02 >= a12 {
                (0, 2)
            } else {
                (1, 2)
            }
        };
        let apq = matrix[p * 3 + q];
        if apq != 0.0 {
            let app = matrix[p * 3 + p];
            let aqq = matrix[q * 3 + q];
            let angle = 0.5 * (2.0 * apq).atan2(aqq - app);
            let cosine = angle.cos();
            let sine = angle.sin();
            for k in 0..3 {
                if k != p && k != q {
                    let akp = matrix[k * 3 + p];
                    let akq = matrix[k * 3 + q];
                    let next_p = (cosine * akp) - (sine * akq);
                    let next_q = (sine * akp) + (cosine * akq);
                    matrix[k * 3 + p] = next_p;
                    matrix[p * 3 + k] = next_p;
                    matrix[k * 3 + q] = next_q;
                    matrix[q * 3 + k] = next_q;
                }
            }
            matrix[p * 3 + p] = (cosine * cosine * app) - (2.0 * sine * cosine * apq) + (sine * sine * aqq);
            matrix[q * 3 + q] = (sine * sine * app) + (2.0 * sine * cosine * apq) + (cosine * cosine * aqq);
            matrix[p * 3 + q] = 0.0;
            matrix[q * 3 + p] = 0.0;
            for row in 0..3 {
                let vip = vectors[row * 3 + p];
                let viq = vectors[row * 3 + q];
                vectors[row * 3 + p] = (cosine * vip) - (sine * viq);
                vectors[row * 3 + q] = (sine * vip) + (cosine * viq);
            }
        }
    }
    let column = if matrix[0] <= matrix[4] && matrix[0] <= matrix[8] {
        0
    } else if matrix[4] <= matrix[8] {
        1
    } else {
        2
    };
    vec3::create(vectors[column], vectors[3 + column], vectors[6 + column])
}

/// Members of a group in its preferred (ordered, else ascending) order.
fn members_preferred(value: &group::T) -> Vec<i64> {
    match group::ordered_elements(value) {
        Some(order) => order,
        None => (0..group::length(value)).filter(|&index| group::mem(index, value)).collect(),
    }
}

fn selected_points(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    selection: Option<&element_selection::T>,
    geometry_value: &geometry::T,
) -> Result<Vec<i64>, error::T> {
    let topology_value = geometry::topology(geometry_value);
    element_selection::validate(OPERATION, topology_value, selection).map_err(lift)?;
    match selection {
        None => Ok((0..geometry::point_count(geometry_value)).collect()),
        Some(selection) => {
            let promoted =
                element_selection::promote(cancel_value, grain, None, group::Owner::Point, selection, topology_value)
                    .map_err(lift)?;
            Ok(members_preferred(&promoted))
        }
    }
}

fn constraint_endpoint_selection(
    cancel_value: Option<&cancel::T>,
    points: &[i64],
    constraint_edges: Option<&edge_group::T>,
    constraint_primitives: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<Vec<i64>, error::T> {
    let point_count = geometry::point_count(geometry_value) as usize;
    let topology_value = geometry::topology(geometry_value);
    let view = topology::view::view(topology_value).map_err(pass)?;
    let mut selected = vec![false; point_count];
    for &point in points {
        selected[point as usize] = true;
    }
    let mut retained = vec![false; point_count];
    let mut retained_count = 0_usize;
    let mut retain = |point: i64| -> Result<(), error::T> {
        if !selected[point as usize] {
            return Err(invalid("every constraint endpoint must belong to the point selection"));
        }
        if !retained[point as usize] {
            retained[point as usize] = true;
            retained_count += 1;
        }
        Ok(())
    };
    if let Some(item) = constraint_edges {
        if edge_group::topology_data_id(item) != topology::data_id(topology_value) {
            return Err(invalid("constraint edge group belongs to different topology"));
        }
        let index = topology_index::create(cancel_value, topology_value).map_err(pass)?;
        let index_view = topology_index::view::view(&index);
        for edge in 0..edge_group::length(item) {
            if !edge_group::mem(edge, item) {
                continue;
            }
            cancel::poll(cancel_value, edge, 4095).map_err(pass)?;
            retain(index_view.edge_a[edge as usize])?;
            retain(index_view.edge_b[edge as usize])?;
        }
    }
    if let Some(item) = constraint_primitives {
        if group::owner(item) != group::Owner::Primitive {
            return Err(invalid("constraint primitive group must own primitives"));
        }
        if group::length(item) != geometry::primitive_count(geometry_value) {
            return Err(invalid("constraint primitive group length is invalid"));
        }
        for primitive in members_preferred(item) {
            cancel::poll(cancel_value, primitive, 4095).map_err(pass)?;
            for vertex in view.primitive_offsets[primitive as usize]..view.primitive_offsets[primitive as usize + 1] {
                retain(view.vertex_points[vertex as usize])?;
            }
        }
    }
    if retained_count == 0 {
        return Err(invalid("Ignore Non-Constraint Points requires at least one constraint"));
    }
    Ok(points.iter().copied().filter(|&point| retained[point as usize]).collect())
}

/// Chunked parallel map into an i64 vector, joined in range order.
fn par_i64(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    count: i64,
    value: &(dyn Fn(i64) -> Result<i64, error::T> + Sync),
) -> Result<Vec<i64>, error::T> {
    let mut output = vec![0; count.max(0) as usize];
    let windows = support::Windows::even(&mut output, grain);
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        for (index, slot) in (first..last).zip(windows.take(range)) {
            cancel::poll(cancel_value, index, 4095)?;
            *slot = value(index)?;
        }
        Ok(())
    })
    .map_err(pass)?;
    Ok(output)
}

/// Chunked parallel map into two f64 vectors.
fn par_xy(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    count: i64,
    value: &(dyn Fn(i64) -> Result<(f64, f64), error::T> + Sync),
) -> Result<(Vec<f64>, Vec<f64>), error::T> {
    let length = count.max(0) as usize;
    let (mut x, mut y) = (vec![0.0; length], vec![0.0; length]);
    let (wx, wy) = (support::Windows::even(&mut x, grain), support::Windows::even(&mut y, grain));
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        let (ox, oy) = (wx.take(range), wy.take(range));
        for index in first..last {
            cancel::poll(cancel_value, index, 4095)?;
            let at = (index - first) as usize;
            (ox[at], oy[at]) = value(index)?;
        }
        Ok(())
    })
    .map_err(pass)?;
    Ok((x, y))
}

fn sign_code(sign: Sign) -> i64 {
    match sign {
        Sign::Negative => -1,
        Sign::Zero => 0,
        Sign::Positive => 1,
    }
}

#[allow(clippy::too_many_arguments)]
fn constraint_segments(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    polygon_winding: bool,
    silhouette: bool,
    silhouette_winding: bool,
    x: &[f64],
    y: &[f64],
    constraint_edges: Option<&edge_group::T>,
    constraint_primitives: Option<&group::T>,
    geometry_value: &geometry::T,
    local_of_point: &[i64],
    triangulation: &delaunay2::T,
) -> Result<(Vec<i64>, Vec<i64>), error::T> {
    let topology_value = geometry::topology(geometry_value);
    let view = topology::view::view(topology_value).map_err(pass)?;
    let mut source: Vec<i64> = Vec::new();
    let mut winding: Vec<i64> = Vec::new();
    let add_local = |source: &mut Vec<i64>, winding: &mut Vec<i64>, weight: i64, a: i64, b: i64| -> Result<(), error::T> {
        if a == b {
            return Err(invalid("a constraint collapses after projected-coordinate deduplication"));
        }
        source.push(a);
        source.push(b);
        winding.push(weight);
        Ok(())
    };
    let unique_of = |local: i64| -> Result<i64, error::T> {
        let unique = delaunay2::source_unique(triangulation, local).map_err(pass)?;
        delaunay2::unique_source(triangulation, unique).map_err(pass)
    };
    let add = |source: &mut Vec<i64>, winding: &mut Vec<i64>, weight: i64, source_a: i64, source_b: i64| -> Result<(), error::T> {
        let local_a = local_of_point[source_a as usize];
        let local_b = local_of_point[source_b as usize];
        if local_a < 0 || local_b < 0 {
            return Err(invalid("every constraint endpoint must belong to the point selection"));
        }
        let a = unique_of(local_a)?;
        let b = unique_of(local_b)?;
        if a != b {
            add_local(source, winding, weight, a, b)
        } else {
            Err(invalid("a constraint collapses after projected-coordinate deduplication"))
        }
    };
    if let Some(item) = constraint_edges {
        if edge_group::topology_data_id(item) != topology::data_id(topology_value) {
            return Err(invalid("constraint edge group belongs to different topology"));
        }
        let index = topology_index::create(cancel_value, topology_value).map_err(pass)?;
        let index_view = topology_index::view::view(&index);
        for edge in 0..edge_group::length(item) {
            if !edge_group::mem(edge, item) {
                continue;
            }
            cancel::poll(cancel_value, edge, 4095).map_err(pass)?;
            add(&mut source, &mut winding, 0, index_view.edge_a[edge as usize], index_view.edge_b[edge as usize])?;
        }
    }
    if let Some(item) = constraint_primitives {
        if group::owner(item) != group::Owner::Primitive {
            return Err(invalid("constraint primitive group must own primitives"));
        }
        if group::length(item) != geometry::primitive_count(geometry_value) {
            return Err(invalid("constraint primitive group length is invalid"));
        }
        for primitive in members_preferred(item) {
            cancel::poll(cancel_value, primitive, 4095).map_err(pass)?;
            let first = view.primitive_offsets[primitive as usize];
            let last = view.primitive_offsets[primitive as usize + 1];
            let size = last - first;
            let kind = view.primitive_kinds[primitive as usize];
            let segments = if kind == 1 { (size - 1).max(0) } else { size };
            let weight = if kind == 1 || !polygon_winding { 0 } else { 1 };
            for local in 0..segments {
                let next = if local + 1 < size { local + 1 } else { 0 };
                add(
                    &mut source,
                    &mut winding,
                    weight,
                    view.vertex_points[(first + local) as usize],
                    view.vertex_points[(first + next) as usize],
                )?;
            }
        }
    }
    if silhouette {
        let primitive_count = geometry::primitive_count(geometry_value);
        let vertex_count = geometry::vertex_count(geometry_value);
        let projected_vertex_points = par_i64(cancel_value, grain, vertex_count, &|vertex| {
            let source_point = view.vertex_points[vertex as usize];
            let local = local_of_point[source_point as usize];
            if local >= 0 { unique_of(local) } else { Ok(-1) }
        })?;
        let mut selected_polygons = vec![false; primitive_count as usize];
        for primitive in 0..primitive_count {
            cancel::poll(cancel_value, primitive, 4095).map_err(pass)?;
            if view.primitive_kinds[primitive as usize] == 0 {
                let first = view.primitive_offsets[primitive as usize];
                let last = view.primitive_offsets[primitive as usize + 1];
                let selected = (first..last).filter(|&vertex| projected_vertex_points[vertex as usize] >= 0).count() as i64;
                if selected != 0 && selected != last - first {
                    return Err(invalid("silhouette polygons must be wholly inside or outside the point selection"));
                }
                if selected > 0 {
                    selected_polygons[primitive as usize] = true;
                }
            }
        }
        let signs = par_i64(cancel_value, grain, primitive_count, &|primitive| {
            if selected_polygons[primitive as usize] {
                let first = view.primitive_offsets[primitive as usize];
                let count = view.primitive_offsets[primitive as usize + 1] - first;
                if count < 3 || first > projected_vertex_points.len() as i64 - count {
                    return Err(fail("Rdk.Predicates.polygon_area_sign_packed: invalid polygon range".to_owned()));
                }
                Ok(sign_code(predicates::polygon_area_sign_packed(x, y, &projected_vertex_points, first, count)))
            } else {
                Ok(0)
            }
        })?;
        let chosen = if signs.contains(&1) { 1 } else { -1 };
        let index = topology_index::create(cancel_value, topology_value).map_err(pass)?;
        let index_view = topology_index::view::view(&index);
        let edge_count = index_view.edge_a.len() as i64;
        let edge_winding = par_i64(cancel_value, grain, edge_count, &|edge| {
            let first = index_view.edge_offsets[edge as usize];
            let last = index_view.edge_offsets[edge as usize + 1];
            let mut weight = 0_i64;
            for incidence in first..last {
                let vertex = index_view.edge_vertices[incidence as usize];
                let primitive = index_view.primitive_of_vertex[vertex as usize];
                if signs[primitive as usize] == chosen {
                    let next = index_view.next_vertex[vertex as usize];
                    if next >= 0 {
                        let a = projected_vertex_points[vertex as usize];
                        let b = projected_vertex_points[next as usize];
                        if a != b {
                            let contribution = if a < b { 1 } else { -1 };
                            weight = weight
                                .checked_add(contribution)
                                .ok_or_else(|| invalid("silhouette winding exceeds integer range"))?;
                        }
                    }
                }
            }
            Ok(weight)
        })?;
        for edge in 0..edge_count {
            cancel::poll(cancel_value, edge, 4095).map_err(pass)?;
            let weight = edge_winding[edge as usize];
            if weight != 0 {
                let source_a = index_view.edge_a[edge as usize];
                let source_b = index_view.edge_b[edge as usize];
                let local_a = local_of_point[source_a as usize];
                let local_b = local_of_point[source_b as usize];
                if local_a < 0 || local_b < 0 {
                    return Err(invalid("silhouette edge crosses the point selection boundary"));
                }
                let a = unique_of(local_a)?;
                let b = unique_of(local_b)?;
                if a != b {
                    let (canonical_a, canonical_b, weight) = if a < b { (a, b, weight) } else { (b, a, -weight) };
                    add_local(
                        &mut source,
                        &mut winding,
                        if silhouette_winding { weight } else { 0 },
                        canonical_a,
                        canonical_b,
                    )?;
                }
            }
        }
    }
    let _ = (x, y);
    Ok((source, winding))
}

struct ProjectedPoints {
    x: Vec<f64>,
    y: Vec<f64>,
    embed: Box<dyn Fn(f64, f64) -> (f64, f64, f64) + Send + Sync>,
}

fn planes_from_positions(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    positions: &packed::float3::T,
    points: &[i64],
) -> Result<ProjectedPoints, error::T> {
    let source = packed::float3::view::view(positions).map_err(pass)?;
    let count = points.len();
    if count == 0 {
        return Err(invalid("selection contains no points"));
    }
    let mut maximum = 0.0_f64;
    for (index, &point) in points.iter().enumerate() {
        cancel::poll(cancel_value, index as i64, 4095).map_err(pass)?;
        let p = point as usize;
        if !finite3(source.x[p], source.y[p], source.z[p]) {
            return Err(fail(format!("{OPERATION}: point {point} has a non-finite position")));
        }
        maximum = maximum.max(source.x[p].abs().max(source.y[p].abs().max(source.z[p].abs())));
    }
    let scale = if maximum == 0.0 { 1.0 } else { maximum };
    let (mut mx, mut my, mut mz) = (0.0_f64, 0.0_f64, 0.0_f64);
    for (index, &point) in points.iter().enumerate() {
        let p = point as usize;
        let n = (index + 1) as f64;
        mx += ((source.x[p] / scale) - mx) / n;
        my += ((source.y[p] / scale) - my) / n;
        mz += ((source.z[p] / scale) - mz) / n;
    }
    let (mut xx, mut xy, mut xz, mut yy, mut yz, mut zz) = (0.0_f64, 0.0_f64, 0.0_f64, 0.0_f64, 0.0_f64, 0.0_f64);
    for &point in points {
        let p = point as usize;
        let x = (source.x[p] / scale) - mx;
        let y = (source.y[p] / scale) - my;
        let z = (source.z[p] / scale) - mz;
        xx += x * x;
        xy += x * y;
        xz += x * z;
        yy += y * y;
        yz += y * z;
        zz += z * z;
    }
    let normal = smallest_eigenvector(xx, xy, xz, yy, yz, zz);
    let ((ux, uy, uz), (vx, vy, vz)) = frame_of_normal(normal)?;
    let (output_x, output_y) = par_xy(cancel_value, grain, count as i64, &|index| {
        let p = points[index as usize] as usize;
        let x = (source.x[p] / scale) - mx;
        let y = (source.y[p] / scale) - my;
        let z = (source.z[p] / scale) - mz;
        Ok(((x * ux) + (y * uy) + (z * uz), (x * vx) + (y * vy) + (z * vz)))
    })?;
    let (cx, cy, cz) = (mx * scale, my * scale, mz * scale);
    let embed = move |x: f64, y: f64| {
        (
            cx + (scale * ((x * ux) + (y * vx))),
            cy + (scale * ((x * uy) + (y * vy))),
            cz + (scale * ((x * uz) + (y * vz))),
        )
    };
    Ok(ProjectedPoints { x: output_x, y: output_y, embed: Box::new(embed) })
}

fn project(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    projection: &Projection,
    geometry_value: &geometry::T,
    points: &[i64],
) -> Result<ProjectedPoints, error::T> {
    let positions = packed::float3::view::view(geometry::positions(geometry_value)).map_err(pass)?;
    let count = points.len() as i64;
    let fill = |coordinate_x: &(dyn Fn(usize) -> f64 + Sync), coordinate_y: &(dyn Fn(usize) -> f64 + Sync)| {
        par_xy(cancel_value, grain, count, &|index| {
            let point = points[index as usize];
            let px = coordinate_x(point as usize);
            let py = coordinate_y(point as usize);
            if !(px.is_finite() && py.is_finite()) {
                return Err(fail(format!("{OPERATION}: point {point} has a non-finite projected position")));
            }
            Ok((px, py))
        })
    };
    match projection {
        Projection::PlaneXy => {
            let (x, y) = fill(&|p| positions.x[p], &|p| positions.y[p])?;
            Ok(ProjectedPoints { x, y, embed: Box::new(|x, y| (x, y, 0.0)) })
        }
        Projection::PlaneYz => {
            let (x, y) = fill(&|p| positions.y[p], &|p| positions.z[p])?;
            Ok(ProjectedPoints { x, y, embed: Box::new(|y, z| (0.0, y, z)) })
        }
        Projection::PlaneZx => {
            let (x, y) = fill(&|p| positions.z[p], &|p| positions.x[p])?;
            Ok(ProjectedPoints { x, y, embed: Box::new(|z, x| (x, 0.0, z)) })
        }
        Projection::BestFit => planes_from_positions(cancel_value, grain, geometry::positions(geometry_value), points),
        Projection::Plane { origin, normal } => {
            let (ox, oy, oz) = vec3::to_triple(*origin);
            if !finite3(ox, oy, oz) {
                return Err(invalid("plane origin must be finite"));
            }
            let ((ux, uy, uz), (vx, vy, vz)) = frame_of_normal(*normal)?;
            let (x, y) = fill(
                &|p| {
                    let x = positions.x[p] - ox;
                    let y = positions.y[p] - oy;
                    let z = positions.z[p] - oz;
                    (x * ux) + (y * uy) + (z * uz)
                },
                &|p| {
                    let x = positions.x[p] - ox;
                    let y = positions.y[p] - oy;
                    let z = positions.z[p] - oz;
                    (x * vx) + (y * vy) + (z * vz)
                },
            )?;
            Ok(ProjectedPoints {
                x,
                y,
                embed: Box::new(move |x, y| {
                    (ox + (x * ux) + (y * vx), oy + (x * uy) + (y * vy), oz + (x * uz) + (y * vz))
                }),
            })
        }
        Projection::PointAttribute(name) => {
            if support::ocaml_trim_is_empty(name) {
                return Err(invalid("point position attribute name is empty"));
            }
            let Some(item) = geometry::find_attribute(attribute::Owner::Point, name, geometry_value) else {
                return Err(fail(format!("{OPERATION}: missing point attribute {}", ocaml_quoted(name))));
            };
            match attribute::view::storage(&item).map_err(pass)? {
                attribute::Storage::Float2(values) => {
                    let values = packed::float2::view::view(values);
                    let (x, y) = fill(&|p| values.x[p], &|p| values.y[p])?;
                    Ok(ProjectedPoints { x, y, embed: Box::new(|x, y| (x, y, 0.0)) })
                }
                attribute::Storage::Float3(values) => {
                    let values = packed::float3::view::view(values).map_err(pass)?;
                    let (x, y) = fill(&|p| values.x[p], &|p| values.y[p])?;
                    Ok(ProjectedPoints { x, y, embed: Box::new(|x, y| (x, y, 0.0)) })
                }
                _ => Err(fail(format!(
                    "{OPERATION}: point attribute {} must be float2 or float3",
                    ocaml_quoted(name)
                ))),
            }
        }
    }
}

/// OCaml `%S`: a double-quoted string with `\\`, `\"` and control characters escaped.
fn ocaml_quoted(text: &str) -> String {
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if (c as u32) < 32 || c as u32 == 127 => out.push_str(&format!("\\{:03}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

struct PointMaterialization {
    positions: packed::float3::T,
    representative: Vec<i64>,
    roots: Vec<i64>,
    parent_first: Vec<i64>,
    parent_second: Vec<i64>,
    parent_third: Vec<i64>,
    weight_first: Vec<f64>,
    weight_second: Vec<f64>,
    weight_third: Vec<f64>,
    value_count: i64,
    source_point_count: i64,
    selected_count: i64,
    initial_local_count: i64,
    points: Vec<i64>,
    split_count: i64,
    refinement_count: i64,
}

impl PointMaterialization {
    fn local_to_output(&self, point: i64) -> i64 {
        if point < self.selected_count {
            self.points[point as usize]
        } else if point < self.initial_local_count {
            self.source_point_count + (point - self.selected_count)
        } else {
            self.source_point_count + self.split_count + (point - self.initial_local_count)
        }
    }
}

fn weighted_value(source: &[f64], first: i64, second: i64, third: i64, wa: f64, wb: f64, wc: f64) -> f64 {
    let a = source[first as usize];
    let b = source[second as usize];
    let c = source[third as usize];
    let scale = a.abs().max(b.abs().max(c.abs()));
    if scale == 0.0 {
        0.0
    } else if scale.is_finite() {
        scale * (a / scale * wa + b / scale * wb + c / scale * wc)
    } else {
        a * wa + b * wb + c * wc
    }
}

#[allow(clippy::too_many_arguments)]
fn materialization(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    restore_original_point_positions: bool,
    projected_x: &[f64],
    projected_y: &[f64],
    embed: &(dyn Fn(f64, f64) -> (f64, f64, f64) + Send + Sync),
    points: &[i64],
    arrangement: Option<&planar_constraints::T>,
    refinement: Option<&planar_refinement::T>,
    geometry_value: &geometry::T,
) -> Result<PointMaterialization, error::T> {
    let source_point_count = geometry::point_count(geometry_value);
    let selected_count = points.len() as i64;
    let split_count = arrangement.map_or(0, planar_constraints::split_point_count);
    let refinement_count = refinement.map_or(0, planar_refinement::new_point_count);
    let initial_local_count = selected_count + split_count;
    let output_point_count = source_point_count + split_count + refinement_count;
    let provenance_count = refinement.map_or(0, planar_refinement::provenance_node_count);
    let value_count = output_point_count + provenance_count;
    let mut result = PointMaterialization {
        positions: geometry::positions(geometry_value).clone(),
        representative: Vec::new(),
        roots: (0..output_point_count).collect(),
        parent_first: (0..value_count).collect(),
        parent_second: (0..value_count).collect(),
        parent_third: (0..value_count).collect(),
        weight_first: vec![1.0; value_count as usize],
        weight_second: vec![0.0; value_count as usize],
        weight_third: vec![0.0; value_count as usize],
        value_count,
        source_point_count,
        selected_count,
        initial_local_count,
        points: points.to_vec(),
        split_count,
        refinement_count,
    };
    if let Some(arrangement) = arrangement {
        for split in 0..split_count {
            let output = (source_point_count + split) as usize;
            let weight = planar_constraints::split_source_parameter(arrangement, split).map_err(pass)?;
            result.parent_first[output] =
                points[planar_constraints::split_source_first(arrangement, split).map_err(pass)? as usize];
            result.parent_second[output] =
                points[planar_constraints::split_source_second(arrangement, split).map_err(pass)? as usize];
            result.parent_third[output] = result.parent_first[output];
            result.weight_first[output] = 1.0 - weight;
            result.weight_second[output] = weight;
        }
    }
    if let Some(refinement) = refinement {
        let map_reference = |reference: i64, result: &PointMaterialization| -> i64 {
            if reference < initial_local_count {
                result.local_to_output(reference)
            } else {
                output_point_count + reference - initial_local_count
            }
        };
        for node in 0..provenance_count {
            let slot = (output_point_count + node) as usize;
            let first = planar_refinement::provenance_parent_first(refinement, node).map_err(pass)?;
            let second = planar_refinement::provenance_parent_second(refinement, node).map_err(pass)?;
            let third = planar_refinement::provenance_parent_third(refinement, node).map_err(pass)?;
            result.parent_first[slot] = map_reference(first, &result);
            result.parent_second[slot] = map_reference(second, &result);
            result.parent_third[slot] = map_reference(third, &result);
            let (wa, wb, wc) = planar_refinement::provenance_parent_weights(refinement, node).map_err(pass)?;
            result.weight_first[slot] = wa;
            result.weight_second[slot] = wb;
            result.weight_third[slot] = wc;
        }
        for generated in 0..refinement_count {
            let local = initial_local_count + generated;
            let reference = planar_refinement::point_provenance_ref(refinement, local).map_err(pass)?;
            let root = map_reference(reference, &result);
            let output = result.local_to_output(local);
            result.roots[output as usize] = root;
        }
    }
    let mut value_representative: Vec<i64> = (0..value_count).collect();
    for output in source_point_count..source_point_count + split_count {
        let o = output as usize;
        let parent = if result.weight_second[o] < 0.5 { result.parent_first[o] } else { result.parent_second[o] };
        value_representative[o] = value_representative[parent as usize];
    }
    for node in 0..provenance_count {
        let output = (output_point_count + node) as usize;
        let mut best_weight = result.weight_first[output];
        let mut chosen = value_representative[result.parent_first[output] as usize];
        let mut consider = |weight: f64, point: i64| {
            if weight > best_weight || (weight == best_weight && point < chosen) {
                best_weight = weight;
                chosen = point;
            }
        };
        consider(result.weight_second[output], value_representative[result.parent_second[output] as usize]);
        consider(result.weight_third[output], value_representative[result.parent_third[output] as usize]);
        value_representative[output] = chosen;
    }
    result.representative = (0..output_point_count as usize)
        .map(|output| value_representative[result.roots[output] as usize])
        .collect();
    let evaluate = |source: &[f64]| -> Vec<f64> {
        let mut values = vec![0.0_f64; value_count as usize];
        values[..source_point_count as usize].copy_from_slice(&source[..source_point_count as usize]);
        for slot in source_point_count..source_point_count + split_count {
            let s = slot as usize;
            values[s] = weighted_value(
                &values,
                result.parent_first[s],
                result.parent_second[s],
                result.parent_third[s],
                result.weight_first[s],
                result.weight_second[s],
                result.weight_third[s],
            );
        }
        for node in 0..provenance_count {
            let s = (output_point_count + node) as usize;
            values[s] = weighted_value(
                &values,
                result.parent_first[s],
                result.parent_second[s],
                result.parent_third[s],
                result.weight_first[s],
                result.weight_second[s],
                result.weight_third[s],
            );
        }
        for output in source_point_count + split_count..output_point_count {
            values[output as usize] = values[result.roots[output as usize] as usize];
        }
        values.truncate(output_point_count as usize);
        values
    };
    if !restore_original_point_positions {
        let source = packed::float3::view::view(geometry::positions(geometry_value)).map_err(pass)?;
        let mut x = vec![0.0_f64; output_point_count as usize];
        let mut y = vec![0.0_f64; output_point_count as usize];
        let mut z = vec![0.0_f64; output_point_count as usize];
        x[..source_point_count as usize].copy_from_slice(&source.x);
        y[..source_point_count as usize].copy_from_slice(&source.y);
        z[..source_point_count as usize].copy_from_slice(&source.z);
        let (local_x, local_y): (&[f64], &[f64]) = match (refinement, arrangement) {
            (Some(refinement), _) => {
                (planar_refinement::view::approximate_x(refinement), planar_refinement::view::approximate_y(refinement))
            }
            (None, Some(arrangement)) => {
                let view = planar_constraints::view::view(arrangement);
                // The arrangement coordinates are shared planes; keep them alive for the loop.
                return finish_unrestored(
                    cancel_value,
                    grain,
                    result,
                    (x, y, z),
                    (&view.x, &view.y),
                    embed,
                    initial_local_count + refinement_count,
                );
            }
            (None, None) => (projected_x, projected_y),
        };
        return finish_unrestored(
            cancel_value,
            grain,
            result,
            (x, y, z),
            (local_x, local_y),
            embed,
            initial_local_count + refinement_count,
        );
    }
    if split_count + refinement_count == 0 {
        return Ok(result);
    }
    let source = packed::float3::view::view(geometry::positions(geometry_value)).map_err(pass)?;
    let x = evaluate(&source.x);
    let y = evaluate(&source.y);
    let z = evaluate(&source.z);
    for output in source_point_count..output_point_count {
        cancel::poll(cancel_value, output, 4095).map_err(pass)?;
        let o = output as usize;
        if !finite3(x[o], y[o], z[o]) {
            return Err(fail(format!("{OPERATION}: generated point {output} has a non-finite 3D interpolation")));
        }
    }
    result.positions = packed::float3::view::of_owned_exn(x, y, z).map_err(pass)?;
    Ok(result)
}

fn finish_unrestored(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    mut result: PointMaterialization,
    planes: (Vec<f64>, Vec<f64>, Vec<f64>),
    local: (&[f64], &[f64]),
    embed: &(dyn Fn(f64, f64) -> (f64, f64, f64) + Send + Sync),
    local_count: i64,
) -> Result<PointMaterialization, error::T> {
    let (mut x, mut y, mut z) = planes;
    let (local_x, local_y) = local;
    if local_x.len() as i64 != local_count || local_y.len() as i64 != local_count {
        return Err(invalid("projected point materialization is malformed"));
    }
    // Embedded positions in disjoint ranges, then scattered to their output slots.
    let mut embedded = vec![(0.0, 0.0, 0.0); local_count.max(0) as usize];
    {
        let view: &PointMaterialization = &result;
        let windows = support::Windows::even(&mut embedded, grain);
        support::run_indexed_ranges(grain, local_count, &|range, first, last| {
            for (index, slot) in (first..last).zip(windows.take(range)) {
                cancel::poll(cancel_value, index, 4095)?;
                let (px, py, pz) = embed(local_x[index as usize], local_y[index as usize]);
                if !finite3(px, py, pz) {
                    return Err(fail(format!(
                        "{OPERATION}: projected output point {} is non-finite",
                        view.local_to_output(index)
                    )));
                }
                *slot = (px, py, pz);
            }
            Ok(())
        })
        .map_err(pass)?;
    }
    for (local, (px, py, pz)) in embedded.into_iter().enumerate() {
        let output = result.local_to_output(local as i64) as usize;
        x[output] = px;
        y[output] = py;
        z[output] = pz;
    }
    result.positions = packed::float3::view::of_owned_exn(x, y, z).map_err(pass)?;
    Ok(result)
}

fn interpolate_point_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    plan: &PointMaterialization,
    item: &attribute::T,
) -> Result<attribute::T, error::T> {
    use attribute::Storage;
    let count = plan.representative.len() as i64;
    let numeric = |source: &[f64]| -> Result<Vec<f64>, error::T> {
        let mut values = vec![0.0_f64; plan.value_count as usize];
        values[..plan.source_point_count as usize].copy_from_slice(&source[..plan.source_point_count as usize]);
        let blend = |values: &mut Vec<f64>, point: i64| {
            let p = point as usize;
            values[p] = weighted_value(
                values,
                plan.parent_first[p],
                plan.parent_second[p],
                plan.parent_third[p],
                plan.weight_first[p],
                plan.weight_second[p],
                plan.weight_third[p],
            );
        };
        for point in plan.source_point_count..plan.source_point_count + plan.split_count {
            cancel::poll(cancel_value, point, 4095).map_err(pass)?;
            blend(&mut values, point);
        }
        for point in count..plan.value_count {
            cancel::poll(cancel_value, point, 4095).map_err(pass)?;
            blend(&mut values, point);
        }
        let mut output = values[..count as usize].to_vec();
        for point in plan.source_point_count + plan.split_count..count {
            output[point as usize] = values[plan.roots[point as usize] as usize];
        }
        Ok(output)
    };
    let source = attribute::view::storage(item).map_err(pass)?;
    let storage = match source {
        Storage::Float3(values) if attribute::name(item) == "N" => {
            let values = packed::float3::view::view(values).map_err(pass)?;
            let mut x = numeric(&values.x)?;
            let mut y = numeric(&values.y)?;
            let mut z = numeric(&values.z)?;
            for point in 0..count as usize {
                let length = ((x[point] * x[point]) + (y[point] * y[point]) + (z[point] * z[point])).sqrt();
                if length > 1e-20 {
                    x[point] /= length;
                    y[point] /= length;
                    z[point] /= length;
                }
            }
            Storage::Float3(packed::float3::view::of_owned_exn(x, y, z).map_err(pass)?)
        }
        source => match attribute::map_float_planes(source, numeric).map_err(pass)? {
            Some(storage) => storage,
            None => topology_remap::select_storage(cancel_value, grain, false, &plan.representative, source).map_err(pass)?,
        },
    };
    attribute::create_owned(attribute::name(item), attribute::Owner::Point, storage).map_err(pass)
}

fn checked_add(label: &str, first: i64, second: i64) -> Result<i64, error::T> {
    first.checked_add(second).filter(|_| first >= 0 && second >= 0)
        .ok_or_else(|| invalid(&format!("{label} exceeds integer range")))
}

fn defaulted_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    mapping: &[i64],
    item: &attribute::T,
) -> Result<attribute::T, error::T> {
    topology_remap::attribute(cancel_value, grain, Some(true), mapping, item).map_err(pass)
}

fn defaulted_group(cancel_value: Option<&cancel::T>, grain: i64, mapping: &[i64], source: &group::T) -> Result<group::T, error::T> {
    let cancelled = AtomicBool::new(false);
    let output = group::init(Some(grain), group::owner(source), group::name(source), mapping.len() as i64, &|target| {
        if cancel::poll(cancel_value, target, 4095).is_err() {
            cancelled.store(true, Ordering::Relaxed);
        }
        let from = mapping[target as usize];
        from >= 0 && group::mem(from, source)
    })
    .map_err(pass)?;
    if cancelled.load(Ordering::Relaxed) {
        return Err(fail(format!("{OPERATION}: triangulation was cancelled")));
    }
    group::view::remap_order(source, mapping, &output).map_err(pass)
}

struct OutputTopologyPlan {
    topology: topology::T,
    vertex_source: Vec<i64>,
    primitive_source: Vec<i64>,
    kept_primitive_count: i64,
}

#[allow(clippy::too_many_arguments)]
fn output_topology(
    cancel_value: Option<&cancel::T>,
    keep_primitives: bool,
    constraint_primitives: Option<&group::T>,
    source_point_output: &[i64],
    output_point_count: i64,
    plan: &PointMaterialization,
    triangle_points: &[i64],
    geometry_value: &geometry::T,
) -> Result<OutputTopologyPlan, error::T> {
    let triangle_count = (triangle_points.len() / 3) as i64;
    if !keep_primitives {
        let mut vertex_points = Vec::with_capacity(triangle_points.len());
        for (vertex, &local) in triangle_points.iter().enumerate() {
            cancel::poll(cancel_value, vertex as i64, 4095).map_err(pass)?;
            vertex_points.push(plan.local_to_output(local));
        }
        let primitive_offsets: Vec<i64> = (0..=triangle_count).map(|primitive| primitive * 3).collect();
        return Ok(OutputTopologyPlan {
            topology: topology::polygons_owned(output_point_count, vertex_points, primitive_offsets).map_err(pass)?,
            vertex_source: Vec::new(),
            primitive_source: Vec::new(),
            kept_primitive_count: 0,
        });
    }
    let source = topology::view::view(geometry::topology(geometry_value)).map_err(pass)?;
    let source_primitive_count = geometry::primitive_count(geometry_value);
    if let Some(item) = constraint_primitives {
        if group::owner(item) != group::Owner::Primitive || group::length(item) != source_primitive_count {
            return Err(invalid("constraint primitive group is incompatible with input topology"));
        }
    }
    let mut kept_vertex_offsets = vec![0_i64; source_primitive_count as usize + 1];
    let mut source_to_output = vec![-1_i64; source_primitive_count as usize];
    let mut kept_primitive_count = 0_i64;
    for primitive in 0..source_primitive_count as usize {
        cancel::poll(cancel_value, primitive as i64, 4095).map_err(pass)?;
        let retained = match constraint_primitives {
            None => true,
            Some(item) => !group::mem(primitive as i64, item),
        };
        kept_vertex_offsets[primitive + 1] = kept_vertex_offsets[primitive];
        if retained {
            source_to_output[primitive] = kept_primitive_count;
            kept_primitive_count += 1;
            kept_vertex_offsets[primitive + 1] = checked_add(
                "kept vertex count",
                kept_vertex_offsets[primitive],
                source.primitive_offsets[primitive + 1] - source.primitive_offsets[primitive],
            )?;
        }
    }
    let kept_vertex_count = kept_vertex_offsets[source_primitive_count as usize];
    let output_primitive_count = checked_add("output primitive count", kept_primitive_count, triangle_count)?;
    let output_vertex_count = checked_add("output vertex count", kept_vertex_count, triangle_points.len() as i64)?;
    let mut vertex_points = vec![0_i64; output_vertex_count as usize];
    let mut vertex_source = vec![-1_i64; output_vertex_count as usize];
    let mut primitive_offsets = vec![0_i64; output_primitive_count as usize + 1];
    let mut primitive_kinds = vec![0_u8; output_primitive_count as usize];
    let mut primitive_source = vec![-1_i64; output_primitive_count as usize];
    for primitive in 0..source_primitive_count as usize {
        cancel::poll(cancel_value, primitive as i64, 1023).map_err(pass)?;
        let output = source_to_output[primitive];
        if output >= 0 {
            let source_first = source.primitive_offsets[primitive];
            let source_last = source.primitive_offsets[primitive + 1];
            let target_first = kept_vertex_offsets[primitive];
            primitive_offsets[output as usize] = target_first;
            primitive_kinds[output as usize] = source.primitive_kinds[primitive];
            primitive_source[output as usize] = primitive as i64;
            for local in 0..source_last - source_first {
                let source_vertex = source_first + local;
                let target_vertex = (target_first + local) as usize;
                let point = source.vertex_points[source_vertex as usize];
                vertex_points[target_vertex] =
                    if source_point_output.is_empty() { point } else { source_point_output[point as usize] };
                vertex_source[target_vertex] = source_vertex;
            }
        }
    }
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095).map_err(pass)?;
        let primitive = kept_primitive_count + triangle;
        let vertex = kept_vertex_count + triangle * 3;
        primitive_offsets[primitive as usize] = vertex;
        for corner in 0..3 {
            vertex_points[(vertex + corner) as usize] = plan.local_to_output(triangle_points[(triangle * 3 + corner) as usize]);
        }
    }
    primitive_offsets[output_primitive_count as usize] = output_vertex_count;
    Ok(OutputTopologyPlan {
        topology: topology::view::create_validated_owned(
            output_point_count,
            vertex_points,
            primitive_offsets,
            primitive_kinds,
        ),
        vertex_source,
        primitive_source,
        kept_primitive_count,
    })
}

fn replace_group(item: group::T, groups: Vec<group::T>) -> Vec<group::T> {
    let mut output = vec![item.clone()];
    output.extend(
        groups
            .into_iter()
            .filter(|existing| group::owner(existing) != group::owner(&item) || group::name(existing) != group::name(&item)),
    );
    output
}

fn remap_source_edge_groups(
    cancel_value: Option<&cancel::T>,
    point_map: &[i64],
    target_topology: &topology::T,
    target_index: &topology_index::T,
    geometry_value: &geometry::T,
) -> Result<Vec<edge_group::T>, error::T> {
    let groups = geometry::edge_groups(geometry_value);
    if groups.is_empty() {
        return Ok(Vec::new());
    }
    let source_index = topology_index::create(cancel_value, geometry::topology(geometry_value)).map_err(pass)?;
    let identity: Vec<i64>;
    let point_map: &[i64] = if point_map.is_empty() {
        identity = (0..geometry::point_count(geometry_value)).collect();
        &identity
    } else {
        point_map
    };
    groups
        .iter()
        .map(|item| {
            edge_group::remap(cancel_value, &source_index, target_topology, target_index, point_map, item)
                .map_err(|failure| fail(format!("{OPERATION}: {}", error::message(&failure))))
        })
        .collect()
}

type Seed = (Vec<i64>, Vec<f64>, Vec<f64>, delaunay2::T, Vec<i64>, Vec<i64>);

#[allow(clippy::too_many_arguments)]
fn constraint_only_seed(
    cancel_value: Option<&cancel::T>,
    seed: i64,
    points: &[i64],
    x: &[f64],
    y: &[f64],
    constraints: &[i64],
    constraint_winding: Vec<i64>,
) -> Result<Seed, error::T> {
    let count = points.len();
    if constraints.is_empty() {
        return Err(invalid("Ignore Non-Constraint Points requires at least one constraint"));
    }
    let mut used = vec![false; count];
    let mut used_count = 0_usize;
    for &point in constraints {
        if point < 0 || point as usize >= count {
            return Err(invalid("internal constraint endpoint is out of range"));
        }
        if !used[point as usize] {
            used[point as usize] = true;
            used_count += 1;
        }
    }
    let mut output_points = Vec::with_capacity(used_count);
    let mut output_x = Vec::with_capacity(used_count);
    let mut output_y = Vec::with_capacity(used_count);
    let mut target_of_source = vec![-1_i64; count];
    for source in 0..count {
        cancel::poll(cancel_value, source as i64, 4095).map_err(pass)?;
        if used[source] {
            target_of_source[source] = output_points.len() as i64;
            output_points.push(points[source]);
            output_x.push(x[source]);
            output_y.push(y[source]);
        }
    }
    let mut output_constraints = Vec::with_capacity(constraints.len());
    for (index, &endpoint) in constraints.iter().enumerate() {
        cancel::poll(cancel_value, index as i64, 4095).map_err(pass)?;
        let target = target_of_source[endpoint as usize];
        if target < 0 {
            return Err(invalid("internal constraint endpoint was not retained"));
        }
        output_constraints.push(target);
    }
    let triangulation = delaunay2::build(cancel_value, Some(seed), &output_x, &output_y).map_err(fail)?;
    Ok((output_points, output_x, output_y, triangulation, output_constraints, constraint_winding))
}

fn exact_points(x: &[f64], y: &[f64], arrangement: Option<&planar_constraints::T>) -> Result<Vec<implicit_point::T>, error::T> {
    match arrangement {
        Some(arrangement) => (0..planar_constraints::point_count(arrangement))
            .map(|point| planar_constraints::view::point(arrangement, point).map_err(pass))
            .collect(),
        None => {
            let source = implicit_point::source(x.to_vec(), y.to_vec(), vec![0.0; x.len()])
                .map_err(|_| invalid("exact source points are invalid"))?;
            (0..x.len() as i64)
                .map(|point| implicit_point::explicit(&source, point).map_err(|_| invalid("exact point is out of range")))
                .collect()
        }
    }
}

fn blank(name: &Option<String>, message: &str) -> Result<(), error::T> {
    match name {
        Some(name) if support::ocaml_trim_is_empty(name) => Err(invalid(message)),
        _ => Ok(()),
    }
}

/// Triangulate the selected points of `geometry_value`.
pub fn run(cancel_value: Option<&cancel::T>, params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let output = triangulate(cancel_value, params, geometry_value)?;
    finish(cancel_value, params, geometry_value, output)
}

fn finish(
    cancel_value: Option<&cancel::T>,
    params: &Params,
    geometry_value: &geometry::T,
    output: geometry::T,
) -> Result<geometry::T, error::T> {
    let output = if params.remove_unused_points {
        compact_points::run(cancel_value, Some(params.grain), &output).map_err(get_core)?
    } else {
        output
    };
    if params.recompute_point_normals
        && geometry::find_attribute(attribute::Owner::Point, "N", geometry_value).is_some()
    {
        let normal_params = normal_ops::Params {
            cancel: cancel_value.map(|value| {
                let copy = cancel::create();
                if cancel::is_cancelled(value) {
                    cancel::cancel(&copy);
                }
                copy
            }),
            grain: Some(params.grain),
            selection: None,
            primitives: None,
            owner: attribute::Owner::Point,
            weighting: normal_ops::Weighting::FaceArea,
            cusp_angle: std::f64::consts::PI,
            attribute: "N".to_owned(),
        };
        normal_ops::run(&normal_params, &output).map_err(get_core)
    } else {
        Ok(output)
    }
}

fn triangulate(cancel_value: Option<&cancel::T>, params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain;
    if grain <= 0 {
        return Err(invalid("grain must be positive"));
    }
    blank(&params.triangle_group, "triangle group name must be non-empty")?;
    blank(&params.constraint_group, "constraint group name must be non-empty")?;
    blank(&params.split_point_group, "split point group name must be non-empty")?;
    blank(&params.refinement_point_group, "refinement point group name must be non-empty")?;
    let mut points = selected_points(cancel_value, grain, params.selection.as_ref(), geometry_value)?;
    if points.is_empty() {
        return Err(invalid("selection contains no points"));
    }
    let constraint_edges = params.constraint_edges.as_ref();
    let constraint_primitives = params.constraint_primitives.as_ref();
    let prefiltered_constraint_points =
        params.ignore_non_constraint_points && !params.silhouette_constraints && !params.remove_outside_silhouette;
    if prefiltered_constraint_points {
        points = constraint_endpoint_selection(cancel_value, &points, constraint_edges, constraint_primitives, geometry_value)?;
    }
    let projected = project(cancel_value, grain, &params.projection, geometry_value, &points)?;
    let mut x = projected.x.clone();
    let mut y = projected.y.clone();
    let mut triangulation =
        delaunay2::build(cancel_value, Some(params.seed), &x, &y).map_err(fail)?;
    let mut local_of_point = vec![-1_i64; geometry::point_count(geometry_value) as usize];
    for (local, &point) in points.iter().enumerate() {
        local_of_point[point as usize] = local as i64;
    }
    let (mut constraints, mut constraint_winding) = constraint_segments(
        cancel_value,
        grain,
        params.remove_outside_constraint_polygons,
        params.silhouette_constraints || params.remove_outside_silhouette,
        params.remove_outside_silhouette,
        &x,
        &y,
        constraint_edges,
        constraint_primitives,
        geometry_value,
        &local_of_point,
        &triangulation,
    )?;
    if params.ignore_non_constraint_points && !prefiltered_constraint_points {
        let (next_points, next_x, next_y, next_triangulation, next_constraints, next_winding) = constraint_only_seed(
            cancel_value,
            params.seed,
            &points,
            &x,
            &y,
            &constraints,
            std::mem::take(&mut constraint_winding),
        )?;
        points = next_points;
        x = next_x;
        y = next_y;
        triangulation = next_triangulation;
        constraints = next_constraints;
        constraint_winding = next_winding;
    }
    let seed_view = delaunay2::view::view(&triangulation);
    let arrangement = if constraints.is_empty() {
        None
    } else {
        Some(
            planar_constraints::build(
                cancel_value,
                Some(grain),
                params.split_crossing_constraints,
                &x,
                &y,
                &constraints,
                Some(&seed_view.unique_source),
                Some(&constraint_winding),
            )
            .map_err(fail)?,
        )
    };
    let (initial_triangles, initial_constraints): (Vec<i64>, Vec<i64>) = match &arrangement {
        None if !params.flood_from_hull_boundary
            && !params.remove_outside_constraint_polygons
            && !params.remove_outside_silhouette =>
        {
            (seed_view.triangle_points.clone(), Vec::new())
        }
        None => {
            let value = planar_cdt::build(
                cancel_value,
                None,
                points.len() as i64,
                &|a, b, c| predicates::orient2d_packed(&x, &y, a, b, c),
                &|a, b, c, d| predicates::incircle_packed(&x, &y, a, b, c, d),
                None,
                &seed_view.triangle_points,
                None,
                params.flood_from_hull_boundary,
                &[],
                Some(&[]),
                params.remove_outside_constraint_polygons || params.remove_outside_silhouette,
            )
            .map_err(fail)?;
            let view = planar_cdt::view::view(&value);
            (view.triangle_points.to_vec(), view.constraint_points.to_vec())
        }
        Some(arrangement) => {
            let arrangement_view = planar_constraints::view::view(arrangement);
            let ax = &arrangement_view.x;
            let ay = &arrangement_view.y;
            let overlap = |a: i64, b: i64, u: i64, v: i64| -> bool {
                let (a, b, u, v) = (a as usize, b as usize, u as usize, v as usize);
                ax[a].min(ax[b]).max(ax[u].min(ax[v])) <= ax[a].max(ax[b]).min(ax[u].max(ax[v]))
                    && ay[a].min(ay[b]).max(ay[u].min(ay[v])) <= ay[a].max(ay[b]).min(ay[u].max(ay[v]))
            };
            let value = planar_cdt::build(
                cancel_value,
                None,
                planar_constraints::point_count(arrangement),
                &|a, b, c| planar_constraints::view::orient2d(arrangement, a, b, c),
                &|a, b, c, d| planar_constraints::view::incircle(arrangement, a, b, c, d),
                Some(&overlap),
                &seed_view.triangle_points,
                Some(&arrangement_view.insert_points),
                params.flood_from_hull_boundary,
                &arrangement_view.constraint_points,
                Some(&arrangement_view.constraint_winding),
                params.remove_outside_constraint_polygons || params.remove_outside_silhouette,
            )
            .map_err(fail)?;
            let view = planar_cdt::view::view(&value);
            (view.triangle_points.to_vec(), view.constraint_points.to_vec())
        }
    };
    let (triangle_points, constraint_points, refinement): (Vec<i64>, Vec<i64>, Option<planar_refinement::T>) =
        if !params.refine {
            (initial_triangles, initial_constraints, None)
        } else {
            let initial_points = exact_points(&x, &y, arrangement.as_ref())?;
            let arrangement_new_points = arrangement.as_ref().map_or(0, planar_constraints::split_point_count);
            if params.maximum_new_points < arrangement_new_points {
                return Err(invalid("maximum new points is smaller than the required constraint split count"));
            }
            let (refinement_constraints, refinement_winding): (Vec<i64>, Vec<i64>) = match &arrangement {
                None => (initial_constraints.clone(), Vec::new()),
                Some(arrangement) => {
                    let view = planar_constraints::view::view(arrangement);
                    (view.constraint_points.to_vec(), view.constraint_winding.to_vec())
                }
            };
            let value = planar_refinement::build(
                cancel_value,
                grain,
                &initial_points,
                &initial_triangles,
                &refinement_constraints,
                &refinement_winding,
                Some(params.minimum_angle),
                params.maximum_area,
                params.target_edge_length,
                params.minimum_edge_length,
                params.maximum_new_points - arrangement_new_points,
                params.allow_constraint_splitting,
                params.regularization_steps,
                params.allow_movement_of_interior_input_points,
            )
            .map_err(fail)?;
            (
                planar_refinement::triangle_points(&value),
                planar_refinement::constraint_points(&value),
                Some(value),
            )
        };
    let plan = materialization(
        cancel_value,
        grain,
        params.restore_original_point_positions,
        &x,
        &y,
        projected.embed.as_ref(),
        &points,
        arrangement.as_ref(),
        refinement.as_ref(),
        geometry_value,
    )?;
    let positions = plan.positions.clone();
    let output_point_count = packed::float3::length(&positions);
    let split_count = plan.split_count;
    let refinement_count = plan.refinement_count;
    let triangle_count = (triangle_points.len() / 3) as i64;
    let keep = params.keep_primitives;
    let mut source_point_output: Vec<i64> = Vec::new();
    if keep && params.remove_duplicate_points {
        source_point_output = (0..geometry::point_count(geometry_value)).collect();
        for local in 0..points.len() {
            cancel::poll(cancel_value, local as i64, 4095).map_err(pass)?;
            let unique = delaunay2::source_unique(&triangulation, local as i64).map_err(pass)?;
            let representative = delaunay2::unique_source(&triangulation, unique).map_err(pass)?;
            source_point_output[points[local] as usize] = points[representative as usize];
        }
    }
    let topology_plan = output_topology(
        cancel_value,
        keep,
        constraint_primitives,
        &source_point_output,
        output_point_count,
        &plan,
        &triangle_points,
        geometry_value,
    )?;
    let topology_value = topology_plan.topology.clone();
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        match attribute::owner(&item) {
            attribute::Owner::Detail => attributes.push(item),
            attribute::Owner::Point => {
                if !params.preserve_point_payload || attribute::name(&item) == "N" {
                } else if split_count + refinement_count == 0 {
                    attributes.push(item);
                } else {
                    attributes.push(interpolate_point_attribute(cancel_value, grain, &plan, &item)?);
                }
            }
            attribute::Owner::Vertex => {
                if keep && attribute::name(&item) != "N" {
                    attributes.push(defaulted_attribute(cancel_value, grain, &topology_plan.vertex_source, &item)?);
                }
            }
            attribute::Owner::Primitive => {
                if keep {
                    attributes.push(defaulted_attribute(cancel_value, grain, &topology_plan.primitive_source, &item)?);
                }
            }
        }
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        match group::owner(&item) {
            group::Owner::Point => {
                if params.preserve_point_payload {
                    groups.push(if split_count + refinement_count == 0 {
                        item
                    } else {
                        topology_remap::group(cancel_value, grain, &plan.representative, &item).map_err(pass)?
                    });
                }
            }
            group::Owner::Vertex => {
                if keep {
                    groups.push(defaulted_group(cancel_value, grain, &topology_plan.vertex_source, &item)?);
                }
            }
            group::Owner::Primitive => {
                if keep {
                    groups.push(defaulted_group(cancel_value, grain, &topology_plan.primitive_source, &item)?);
                }
            }
        }
    }
    let source_point_count = geometry::point_count(geometry_value);
    if let Some(name) = &params.split_point_group {
        let item = group::init(Some(grain), group::Owner::Point, name, output_point_count, &|point| {
            point >= source_point_count && point < source_point_count + split_count
        })
        .map_err(pass)?;
        groups = replace_group(item, groups);
    }
    if let Some(name) = &params.refinement_point_group {
        let item = group::init(Some(grain), group::Owner::Point, name, output_point_count, &|point| {
            point >= output_point_count - refinement_count
        })
        .map_err(pass)?;
        groups = replace_group(item, groups);
    }
    if let Some(name) = &params.triangle_group {
        let kept = topology_plan.kept_primitive_count;
        let item = group::init(Some(grain), group::Owner::Primitive, name, kept + triangle_count, &|primitive| {
            primitive >= kept
        })
        .map_err(pass)?;
        groups = replace_group(item, groups);
    }
    let mut target_index: Option<topology_index::T> = None;
    let source_edge_groups = if keep && !geometry::edge_groups(geometry_value).is_empty() {
        let index = topology_index::create(cancel_value, &topology_value).map_err(pass)?;
        let remapped = remap_source_edge_groups(cancel_value, &source_point_output, &topology_value, &index, geometry_value)?;
        target_index = Some(index);
        remapped
    } else {
        Vec::new()
    };
    let edge_groups = match &params.constraint_group {
        None => source_edge_groups,
        Some(name) => {
            let index = match target_index.take() {
                Some(index) => index,
                None => topology_index::create(cancel_value, &topology_value).map_err(pass)?,
            };
            let mut builder = edge_group::builder::create(&topology_value, &index, name).map_err(pass)?;
            for constraint_index in 0..constraint_points.len() / 2 {
                let a = plan.local_to_output(constraint_points[constraint_index * 2]);
                let b = plan.local_to_output(constraint_points[constraint_index * 2 + 1]);
                let edge = topology_index::find_edge_index(&index, a, b);
                if edge < 0 {
                    return Err(invalid("recovered constraint is absent from output topology"));
                }
                edge_group::builder::set(&mut builder, edge, true).map_err(pass)?;
            }
            let constraints = edge_group::builder::freeze(&mut builder).map_err(pass)?;
            let mut output = vec![constraints];
            output.extend(source_edge_groups.into_iter().filter(|item| edge_group::name(item) != name.as_str()));
            output
        }
    };
    let output =
        geometry::create(positions, topology_value, Some(attributes), Some(groups), Some(edge_groups)).map_err(lift)?;
    if !params.remove_duplicate_points {
        return Ok(output);
    }
    let mut duplicate = vec![false; output_point_count as usize];
    let mut duplicate_count = 0_usize;
    for local in 0..points.len() {
        cancel::poll(cancel_value, local as i64, 4095).map_err(pass)?;
        let unique = delaunay2::source_unique(&triangulation, local as i64).map_err(pass)?;
        if delaunay2::unique_source(&triangulation, unique).map_err(pass)? != local as i64 {
            let point = points[local] as usize;
            if !duplicate[point] {
                duplicate[point] = true;
                duplicate_count += 1;
            }
        }
    }
    if duplicate_count == 0 {
        return Ok(output);
    }
    let item = group::init(
        Some(grain),
        group::Owner::Point,
        "__triangulate_2d_duplicate_points",
        output_point_count,
        &|point| duplicate[point as usize],
    )
    .map_err(pass)?;
    deletion::delete(
        &deletion::DeleteParams { cancel: snapshot(cancel_value), grain: Some(grain), selected: true, compact_points: false },
        &item,
        &output,
    )
    .map_err(get_core)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology::PrimitiveKind;
    use std::sync::Mutex;

    type P3 = (f64, f64, f64);

    fn check(condition: bool, message: &str) {
        assert!(condition, "{message}");
    }

    fn f3(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> packed::float3::T {
        packed::float3::view::of_owned_exn(x, y, z).unwrap()
    }

    fn points(values: &[P3]) -> geometry::T {
        geometry::create(
            f3(values.iter().map(|p| p.0).collect(), values.iter().map(|p| p.1).collect(), values.iter().map(|p| p.2).collect()),
            topology::empty(values.len() as i64).unwrap(),
            None,
            None,
            None,
        ).unwrap()
    }

    fn build(
        positions: packed::float3::T,
        topology_value: topology::T,
        attributes: Vec<attribute::T>,
        groups: Vec<group::T>,
        edge_groups: Vec<edge_group::T>,
    ) -> geometry::T {
        geometry::create(positions, topology_value, Some(attributes), Some(groups), Some(edge_groups)).unwrap()
    }

    fn plain(positions: packed::float3::T, topology_value: topology::T) -> geometry::T {
        build(positions, topology_value, Vec::new(), Vec::new(), Vec::new())
    }

    fn polylines(point_count: i64, vertex_points: Vec<i64>, offsets: Vec<i64>, kinds: Vec<PrimitiveKind>) -> topology::T {
        topology::create_owned(point_count, vertex_points, offsets, kinds).unwrap()
    }

    fn polygons(point_count: i64, vertex_points: Vec<i64>, offsets: Vec<i64>) -> topology::T {
        topology::polygons_owned(point_count, vertex_points, offsets).unwrap()
    }

    fn attr(name: &str, owner: attribute::Owner, storage: attribute::Storage) -> attribute::T {
        attribute::create_owned(name, owner, storage).unwrap()
    }

    fn pgroup(owner: group::Owner, name: &str, length: i64, member: &(dyn Fn(i64) -> bool + Send + Sync)) -> group::T {
        group::init(None, owner, name, length, member).unwrap()
    }

    fn run_with(params: Params, input: &geometry::T) -> Result<geometry::T, error::T> {
        run(None, &params, input)
    }

    fn tri(params: Params, input: &geometry::T) -> geometry::T {
        run_with(params, input).unwrap()
    }

    fn xy() -> Params {
        Params { projection: Projection::PlaneXy, ..Params::default() }
    }

    fn topology_signature(value: &geometry::T) -> (Vec<i64>, Vec<i64>) {
        let view = topology::view::view(geometry::topology(value)).unwrap();
        (view.vertex_points.to_vec(), view.primitive_offsets.to_vec())
    }

    type Positions = (Vec<f64>, Vec<f64>, Vec<f64>);

    fn position_signature(value: &geometry::T) -> Positions {
        let view = packed::float3::view::view(geometry::positions(value)).unwrap();
        (view.x.to_vec(), view.y.to_vec(), view.z.to_vec())
    }

    fn storage(owner: attribute::Owner, name: &str, value: &geometry::T) -> attribute::Storage {
        let item = geometry::find_attribute(owner, name, value).unwrap_or_else(|| panic!("missing attribute {name}"));
        attribute::view::storage(&item).unwrap().clone()
    }

    fn float_values(owner: attribute::Owner, name: &str, value: &geometry::T) -> Vec<f64> {
        match storage(owner, name, value) {
            attribute::Storage::Float(values) => values.to_vec(),
            _ => panic!("float storage changed"),
        }
    }

    fn int_values(owner: attribute::Owner, name: &str, value: &geometry::T) -> Vec<i64> {
        match storage(owner, name, value) {
            attribute::Storage::Int(values) => values.to_vec(),
            _ => panic!("integer storage changed"),
        }
    }

    fn at_domains(domains: i64, work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
        let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            *slot.lock().unwrap() = Some(work());
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }

    fn one_four(work: &(dyn Fn() -> geometry::T + Sync)) -> (geometry::T, geometry::T) {
        (at_domains(1, work), at_domains(4, work))
    }

    fn find_group(owner: group::Owner, name: &str, value: &geometry::T) -> group::T {
        geometry::find_group(owner, name, value).unwrap_or_else(|| panic!("group {name} missing"))
    }

    fn edge_cardinality(name: &str, value: &geometry::T) -> i64 {
        edge_group::cardinality(&geometry::find_edge_group(name, value).unwrap_or_else(|| panic!("edge group {name} missing")))
    }

    fn edge_index(value: &geometry::T, a: i64, b: i64) -> i64 {
        let index = topology_index::create(None, geometry::topology(value)).unwrap();
        topology_index::find_edge_index(&index, a, b)
    }

    fn expect_error(value: Result<geometry::T, error::T>) {
        assert!(value.is_err(), "invalid input succeeded");
    }

    #[test]
    fn cardinality_guards_are_native_i64() {
        assert_eq!(checked_add("count", 1 << 62, 1).unwrap(), (1 << 62) + 1);
        assert_eq!(checked_add("count", i64::MAX, 0).unwrap(), i64::MAX);
        for (a, b) in [(i64::MAX, 1), (i64::MIN, -1), (-1, 1), (1, -1)] {
            assert!(checked_add("count", a, b).is_err());
        }
    }

    #[test]
    fn xy_payload_and_group() {
        let input = points(&[(0., 0., 3.), (1., 0., 4.), (1., 1., 5.), (0., 1., 6.)]);
        let id = attr("id", attribute::Owner::Point, attribute::Storage::Int(Arc::new(vec![10, 11, 12, 13])));
        let selected = pgroup(group::Owner::Point, "selected", 4, &|_| true);
        let input = geometry::with_group(selected, &geometry::with_attribute(id, &input).unwrap()).unwrap();
        let output = tri(Params { triangle_group: Some("triangles".to_owned()), ..xy() }, &input);
        check(
            geometry::point_count(&output) == 4 && geometry::primitive_count(&output) == 2 && geometry::vertex_count(&output) == 6,
            "XY cardinality",
        );
        check(
            packed::float3::data_id(geometry::positions(&output)) == packed::float3::data_id(geometry::positions(&input)),
            "positions were not structurally shared",
        );
        check(geometry::find_attribute(attribute::Owner::Point, "id", &output).is_some(), "point payload was dropped");
        check(geometry::find_group(group::Owner::Point, "selected", &output).is_some(), "point group was dropped");
        check(group::cardinality(&find_group(group::Owner::Primitive, "triangles", &output)) == 2, "triangle group cardinality");
    }

    #[test]
    fn best_fit_and_explicit_plane() {
        let input = points(&[(0., 0., 0.), (1., 0., 1.), (1., 1., 3.), (0., 1., 2.), (0.5, 0.5, 1.5)]);
        let best = tri(Params::default(), &input);
        check(geometry::primitive_count(&best) == 4, "best-fit tilted plane cardinality");
        let explicit = tri(
            Params {
                projection: Projection::Plane { origin: vec3::create(0., 0., 0.), normal: vec3::create(-1., -2., 1.) },
                ..Params::default()
            },
            &input,
        );
        check(geometry::primitive_count(&explicit) == 4, "explicit tilted plane cardinality");
    }

    fn unrestored(projection: Projection) -> Params {
        Params { grain: 1, projection, restore_original_point_positions: false, ..Params::default() }
    }

    #[test]
    fn projected_output_positions() {
        let selected = group::ordered(group::Owner::Point, "selected", 5, &[0, 1, 2, 3]).unwrap();
        let source = geometry::with_group(
            selected.clone(),
            &points(&[(0., 0., 10.), (2., 0., 20.), (2., 2., 30.), (0., 2., 40.), (9., 9., 9.)]),
        ).unwrap();
        let output = tri(
            Params {
                selection: Some(element_selection::T::SelectedPoints(selected)),
                ..unrestored(Projection::PlaneXy)
            },
            &source,
        );
        let (x, y, z) = position_signature(&output);
        check(
            x == [0., 2., 2., 0., 9.] && y == [0., 0., 2., 2., 9.] && z == [0., 0., 0., 0., 9.],
            "XY projected output or unselected-point policy",
        );
        let yz = tri(
            unrestored(Projection::PlaneYz),
            &points(&[(10., 0., 0.), (11., 1., 0.), (12., 1., 1.), (13., 0., 1.)]),
        );
        let (x, y, z) = position_signature(&yz);
        check(x == [0., 0., 0., 0.] && y == [0., 1., 1., 0.] && z == [0., 0., 1., 1.], "YZ projected output");
        let zx = tri(
            unrestored(Projection::PlaneZx),
            &points(&[(0., 10., 0.), (0., 11., 1.), (1., 12., 1.), (1., 13., 0.)]),
        );
        let (x, y, z) = position_signature(&zx);
        check(x == [0., 0., 1., 1.] && y == [0., 0., 0., 0.] && z == [0., 1., 1., 0.], "ZX projected output");
        let explicit_source = points(&[(0., 0., -5.), (2., 0., 20.), (2., 2., -10.), (0., 2., 30.)]);
        let explicit = tri(
            unrestored(Projection::Plane { origin: vec3::create(0., 0., 10.), normal: vec3::create(0., 0., 1.) }),
            &explicit_source,
        );
        let (x, y, z) = position_signature(&explicit);
        check(
            x == [0., 2., 2., 0.] && y == [0., 0., 2., 2.] && z == [10., 10., 10., 10.],
            "explicit-plane projected output",
        );
        let best_source = points(&[(0., 0., 3.), (2., 0., 5.), (2., 2., 9.), (0., 2., 7.), (1., 1., 6.)]);
        let best = tri(unrestored(Projection::BestFit), &best_source);
        check(
            packed::float3::data_id(geometry::positions(&best)) != packed::float3::data_id(geometry::positions(&best_source)),
            "best-fit projected output retained source P identity",
        );
        let (bx, by, bz) = position_signature(&best);
        for point in 0..bx.len() {
            check((bz[point] - bx[point] - (2. * by[point]) - 3.).abs() < 1e-10, "best-fit output left its fitted plane");
        }
        let attribute_source = points(&[(9., 9., 9.), (8., 8., 8.), (7., 7., 7.), (6., 6., 6.)]);
        let coordinates = attr(
            "planar3",
            attribute::Owner::Point,
            attribute::Storage::Float3(f3(vec![0., 2., 2., 0.], vec![0., 0., 2., 2.], vec![f64::NAN, 100., -200., f64::INFINITY])),
        );
        let attribute_source = geometry::with_attribute(coordinates, &attribute_source).unwrap();
        let attribute_output = tri(unrestored(Projection::PointAttribute("planar3".to_owned())), &attribute_source);
        let (x, y, z) = position_signature(&attribute_output);
        check(
            x == [0., 2., 2., 0.] && y == [0., 0., 2., 2.] && z == [0., 0., 0., 0.],
            "float3 projection did not use exactly its first two components",
        );
        let refined_source = points(&[(0., 0., 10.), (2., 0., 20.), (2., 2., 30.), (0., 2., 40.)]);
        let (refined, parallel) = one_four(&|| {
            tri(
                Params {
                    refine: true,
                    maximum_area: Some(0.2),
                    maximum_new_points: 64,
                    regularization_steps: 2,
                    ..unrestored(Projection::PlaneXy)
                },
                &refined_source,
            )
        });
        check(
            topology_signature(&refined) == topology_signature(&parallel)
                && position_signature(&refined) == position_signature(&parallel),
            "projected refinement differs across domains",
        );
        let (_, _, z) = position_signature(&refined);
        check(
            geometry::point_count(&refined) > 4 && z.iter().all(|&v| v == 0.),
            "generated/refined points were not embedded on the output plane",
        );
    }

    #[test]
    fn attribute_and_selection() {
        let input = points(&[(0., 0., 0.), (1., 0., 0.), (2., 0., 0.), (3., 0., 0.), (4., 0., 0.)]);
        let uv = packed::float2::of_owned(vec![0., 1., 1., 0., 5.], vec![0., 0., 1., 1., 5.]).unwrap();
        let uv = attr("planar", attribute::Owner::Point, attribute::Storage::Float2(uv));
        let input = geometry::with_attribute(uv, &input).unwrap();
        let selected = group::ordered(group::Owner::Point, "four", 5, &[0, 1, 2, 3]).unwrap();
        let output = tri(
            Params {
                selection: Some(element_selection::T::SelectedPoints(selected)),
                projection: Projection::PointAttribute("planar".to_owned()),
                ..Params::default()
            },
            &input,
        );
        check(geometry::primitive_count(&output) == 2, "attribute/selection cardinality");
        let view = topology::view::view(geometry::topology(&output)).unwrap();
        check(view.vertex_points.iter().all(|&point| point < 4), "unselected point entered triangulation");
    }

    fn rows_int(offsets: Vec<i64>, values: Vec<i64>) -> attribute::Storage {
        attribute::Storage::IntArray(packed::int_array::create_owned(offsets, values).unwrap())
    }

    #[test]
    fn keep_primitives_payload_groups_and_edges() {
        let positions = f3(
            vec![0., 2., 2., 0., 0.5, 1.5],
            vec![0., 0., 2., 2., 0.5, 1.5],
            vec![0., 1., 2., 3., 4., 5.],
        );
        let topology_value = polylines(
            6,
            vec![0, 1, 2, 3, 0, 2, 4, 5],
            vec![0, 4, 6, 8],
            vec![PrimitiveKind::Polygon, PrimitiveKind::OpenPolyline, PrimitiveKind::OpenPolyline],
        );
        let v = attribute::Owner::Vertex;
        let range = |offset: f64| -> Vec<f64> { (0..8).map(|i| i as f64 + offset).collect() };
        let attributes = vec![
            attr("vertex_weight", v, attribute::Storage::Float(Arc::new(vec![10., 11., 12., 13., 14., 15., 16., 17.]))),
            attr("vertex_id", v, attribute::Storage::Int(Arc::new(vec![10, 11, 12, 13, 14, 15, 16, 17]))),
            attr("vertex_rows", v, rows_int(vec![0, 1, 3, 3, 4, 5, 7, 8, 10], vec![0, 10, 11, 30, 40, 50, 51, 60, 70, 71])),
            attr(
                "vertex_float_rows",
                v,
                attribute::Storage::FloatArray(packed::float_array::create_owned(
                    vec![0, 1, 3, 3, 4, 5, 7, 8, 10],
                    vec![0., 10., 11., 30., 40., 50., 51., 60., 70., 71.],
                ).unwrap()),
            ),
            attr("vertex_uv", v, attribute::Storage::Float2(packed::float2::of_owned(range(0.), range(10.)).unwrap())),
            attr("vertex_vector", v, attribute::Storage::Float3(f3(range(0.), range(10.), range(20.)))),
            attr(
                "vertex_color",
                v,
                attribute::Storage::Float4(packed::float4::of_owned(range(0.), range(10.), range(20.), range(30.)).unwrap()),
            ),
            attr(
                "primitive_label",
                attribute::Owner::Primitive,
                attribute::Storage::Text(std::sync::Arc::new(vec![b"surface".to_vec(), b"constraint".to_vec(), b"guide".to_vec()].into())),
            ),
            attr("detail_id", attribute::Owner::Detail, attribute::Storage::Int(Arc::new(vec![73]))),
        ];
        let constraints = pgroup(group::Owner::Primitive, "constraints", 3, &|p| p == 1);
        let vertex_order = group::ordered(group::Owner::Vertex, "vertex_order", 8, &[7, 4, 2, 0]).unwrap();
        let primitive_order = group::ordered(group::Owner::Primitive, "primitive_order", 3, &[2, 1, 0]).unwrap();
        let old_triangles = pgroup(group::Owner::Primitive, "triangles", 3, &|p| p == 0);
        let source_index = topology_index::create(None, &topology_value).unwrap();
        let retained_edge = topology_index::find_edge_index(&source_index, 0, 1);
        let boundary = edge_group::init(None, &topology_value, &source_index, "boundary", &|e| e == retained_edge).unwrap();
        let old_recovered = edge_group::init(None, &topology_value, &source_index, "recovered", &|e| e == retained_edge).unwrap();
        let input = build(
            positions,
            topology_value,
            attributes,
            vec![constraints.clone(), vertex_order, primitive_order, old_triangles],
            vec![boundary, old_recovered],
        );
        let (output, parallel) = one_four(&|| {
            tri(
                Params {
                    grain: 1,
                    projection: Projection::PlaneXy,
                    constraint_primitives: Some(constraints.clone()),
                    keep_primitives: true,
                    triangle_group: Some("triangles".to_owned()),
                    constraint_group: Some("recovered".to_owned()),
                    ..Params::default()
                },
                &input,
            )
        });
        check(topology_signature(&output) == topology_signature(&parallel), "Keep Primitives topology differs across domains");
        let output_topology = geometry::topology(&output);
        let view = topology::view::view(output_topology).unwrap();
        check(
            geometry::primitive_count(&output) > 2
                && topology::primitive_kind(output_topology, 0).unwrap() == PrimitiveKind::Polygon
                && topology::primitive_kind(output_topology, 1).unwrap() == PrimitiveKind::OpenPolyline
                && view.vertex_points[..6] == [0, 1, 2, 3, 4, 5]
                && view.primitive_offsets[..3] == [0, 4, 6],
            "Keep Primitives did not preserve the non-constraint primitive prefix",
        );
        let weights = float_values(v, "vertex_weight", &output);
        check(
            weights[..6] == [10., 11., 12., 13., 16., 17.] && weights[6..].iter().all(|&w| w == 0.),
            "Keep Primitives vertex fixed payload/defaults",
        );
        match storage(v, "vertex_rows", &output) {
            attribute::Storage::IntArray(values) => check(
                packed::int_array::get(&values, 0).unwrap() == [0]
                    && packed::int_array::get(&values, 1).unwrap() == [10, 11]
                    && packed::int_array::get(&values, 4).unwrap() == [60]
                    && packed::int_array::get(&values, 5).unwrap() == [70, 71]
                    && packed::int_array::get(&values, 6).unwrap().is_empty(),
                "Keep Primitives vertex ragged payload/defaults",
            ),
            _ => panic!("Keep Primitives vertex ragged storage changed"),
        }
        let ids = int_values(v, "vertex_id", &output);
        check(ids[..6] == [10, 11, 12, 13, 16, 17] && ids[6] == 0, "Keep Primitives vertex integer payload/defaults");
        match storage(v, "vertex_float_rows", &output) {
            attribute::Storage::FloatArray(values) => {
                check(packed::float_array::get(&values, 5).unwrap() == [70., 71.], "Keep Primitives vertex float-row payload");
                check(packed::float_array::get(&values, 6).unwrap().is_empty(), "Keep Primitives vertex float-row defaults");
            }
            _ => panic!("Keep Primitives vertex float-row storage changed"),
        }
        match storage(v, "vertex_uv", &output) {
            attribute::Storage::Float2(values) => {
                let values = packed::float2::view::view(&values);
                check(
                    values.x[4] == 6. && values.y[5] == 17. && values.x[6] == 0. && values.y[6] == 0.,
                    "Keep Primitives vertex float2 payload/defaults",
                );
            }
            _ => panic!("Keep Primitives vertex float2 storage changed"),
        }
        match storage(v, "vertex_vector", &output) {
            attribute::Storage::Float3(values) => {
                let values = packed::float3::view::view(&values).unwrap();
                check(
                    values.x[4] == 6. && values.z[5] == 27. && values.x[6] == 0. && values.y[6] == 0. && values.z[6] == 0.,
                    "Keep Primitives vertex float3 payload/defaults",
                );
            }
            _ => panic!("Keep Primitives vertex float3 storage changed"),
        }
        match storage(v, "vertex_color", &output) {
            attribute::Storage::Float4(values) => {
                let values = packed::float4::view::view(&values);
                check(
                    values.x[4] == 6. && values.w[5] == 37. && values.x[6] == 0. && values.w[6] == 0.,
                    "Keep Primitives vertex float4 payload/defaults",
                );
            }
            _ => panic!("Keep Primitives vertex float4 storage changed"),
        }
        match storage(attribute::Owner::Primitive, "primitive_label", &output) {
            attribute::Storage::Text(values) => check(
                &values[0] == b"surface" && &values[1] == b"guide" && values.iter().skip(2).all(|s| s.is_empty()),
                "Keep Primitives primitive payload/defaults",
            ),
            _ => panic!("Keep Primitives primitive storage changed"),
        }
        check(int_values(attribute::Owner::Detail, "detail_id", &output) == [73], "Keep Primitives detail payload");
        check(
            group::ordered_elements(&find_group(group::Owner::Vertex, "vertex_order", &output)) == Some(vec![5, 2, 0]),
            "Keep Primitives ordered vertex-group ancestry",
        );
        check(
            group::ordered_elements(&find_group(group::Owner::Primitive, "primitive_order", &output)) == Some(vec![1, 0]),
            "Keep Primitives ordered primitive-group ancestry",
        );
        check(
            group::cardinality(&find_group(group::Owner::Primitive, "constraints", &output)) == 0,
            "Keep Primitives retained constraint primitive membership",
        );
        let triangles = find_group(group::Owner::Primitive, "triangles", &output);
        check(
            group::cardinality(&triangles) == geometry::primitive_count(&output) - 2
                && !group::mem(0, &triangles)
                && !group::mem(1, &triangles),
            "Keep Primitives triangle output group",
        );
        let output_edge = edge_index(&output, 0, 1);
        let native = geometry::find_edge_group("boundary", &output).expect("Keep Primitives native edge group missing");
        check(output_edge >= 0 && edge_group::mem(output_edge, &native), "Keep Primitives native-edge ancestry");
        let recovered = geometry::find_edge_group("recovered", &output).expect("recovered constraint group missing");
        check(
            edge_group::cardinality(&recovered) == 3 && !edge_group::mem(output_edge, &recovered),
            "Keep Primitives constraint group did not replace a colliding source name",
        );
        check(
            float_values(v, "vertex_weight", &output) == float_values(v, "vertex_weight", &parallel),
            "Keep Primitives payload differs across domains",
        );
        let duplicate_positions = f3(vec![0., 1., 1., 0., 0.], vec![0., 0., 1., 1., 0.], vec![0., 0., 0., 0., 9.]);
        let duplicate_input = plain(duplicate_positions, polygons(5, vec![4, 1, 2], vec![0, 3]));
        let deduplicated = tri(
            Params { grain: 1, keep_primitives: true, remove_duplicate_points: true, ..xy() },
            &duplicate_input,
        );
        let deduplicated_topology = topology::view::view(geometry::topology(&deduplicated)).unwrap();
        check(
            geometry::point_count(&deduplicated) == 4
                && geometry::primitive_count(&deduplicated) == 3
                && deduplicated_topology.vertex_points[..3] == [0, 1, 2],
            "Keep Primitives duplicate compaction deleted rather than rewired a retained face",
        );
    }

    fn constrained_square() -> (geometry::T, group::T) {
        let positions = f3(vec![0., 1., 1., 0.], vec![0., 0., 1., 1.], vec![0., 0., 0., 0.]);
        let topology_value = polylines(4, vec![1, 3], vec![0, 2], vec![PrimitiveKind::OpenPolyline]);
        (plain(positions, topology_value), pgroup(group::Owner::Primitive, "constraint", 1, &|_| true))
    }

    fn check_constraint_edge(value: &geometry::T) {
        let item = geometry::find_edge_group("constraints", value).expect("constraint output group is missing");
        check(edge_group::cardinality(&item) == 1, "constraint group cardinality");
        let edge = edge_index(value, 1, 3);
        check(edge >= 0 && edge_group::mem(edge, &item), "forced constraint edge is missing");
    }

    #[test]
    fn constraints() {
        let (input, primitives) = constrained_square();
        let output = tri(
            Params {
                constraint_primitives: Some(primitives),
                constraint_group: Some("constraints".to_owned()),
                ..xy()
            },
            &input,
        );
        check_constraint_edge(&output);
        let source_index = topology_index::create(None, geometry::topology(&input)).unwrap();
        let edge = topology_index::find_edge_index(&source_index, 1, 3);
        let edges = edge_group::init(None, geometry::topology(&input), &source_index, "edge_constraint", &|c| c == edge).unwrap();
        let output = tri(
            Params { constraint_edges: Some(edges), constraint_group: Some("constraints".to_owned()), ..xy() },
            &input,
        );
        check_constraint_edge(&output);
        let positions = f3(vec![0., 1., 2., 0., 2., 1.], vec![0., 0., 0., 2., 2., 1.], vec![0.; 6]);
        let topology_value = polylines(6, vec![0, 2], vec![0, 2], vec![PrimitiveKind::OpenPolyline]);
        let geometry_value = plain(positions, topology_value);
        let primitives = pgroup(group::Owner::Primitive, "embedded", 1, &|_| true);
        let (embedded, parallel) = one_four(&|| {
            tri(
                Params {
                    grain: 1,
                    constraint_primitives: Some(primitives.clone()),
                    constraint_group: Some("constraints".to_owned()),
                    ..xy()
                },
                &geometry_value,
            )
        });
        check(geometry::point_count(&embedded) == 6, "authored constraint point was materialized as a new point");
        check(topology_signature(&embedded) == topology_signature(&parallel), "embedded constraint point differs across domains");
        check(edge_cardinality("constraints", &embedded) == 2, "embedded point did not split the output constraint");
    }

    fn crossing_constraints() -> (geometry::T, group::T) {
        let positions = f3(vec![0., 2., 2., 0.], vec![0., 0., 2., 2.], vec![0., 100., 10., 200.]);
        let topology_value = polylines(
            4,
            vec![0, 2, 1, 3],
            vec![0, 2, 4],
            vec![PrimitiveKind::OpenPolyline, PrimitiveKind::OpenPolyline],
        );
        let weight = attr("weight", attribute::Owner::Point, attribute::Storage::Float(Arc::new(vec![0., 100., 10., 200.])));
        let id = attr("id", attribute::Owner::Point, attribute::Storage::Int(Arc::new(vec![10, 11, 12, 13])));
        let label = attr(
            "label",
            attribute::Owner::Point,
            attribute::Storage::Text(Arc::new(["a", "b", "c", "d"].iter().map(|s| s.as_bytes()).collect())),
        );
        let selected = pgroup(group::Owner::Point, "selected", 4, &|p| p == 2);
        let constraints = pgroup(group::Owner::Primitive, "constraints", 2, &|_| true);
        (
            build(positions, topology_value, vec![weight, id, label], vec![selected, constraints.clone()], Vec::new()),
            constraints,
        )
    }

    #[test]
    fn crossing_constraints_and_payload() {
        let (input, constraints) = crossing_constraints();
        expect_error(run_with(Params { constraint_primitives: Some(constraints.clone()), ..xy() }, &input));
        expect_error(run_with(
            Params {
                constraint_primitives: Some(constraints.clone()),
                split_crossing_constraints: true,
                refine: true,
                maximum_new_points: 0,
                ..xy()
            },
            &input,
        ));
        let (output, parallel) = one_four(&|| {
            tri(
                Params {
                    grain: 1,
                    constraint_primitives: Some(constraints.clone()),
                    split_crossing_constraints: true,
                    split_point_group: Some("crossings".to_owned()),
                    constraint_group: Some("constraints".to_owned()),
                    ..xy()
                },
                &input,
            )
        });
        check(
            geometry::point_count(&output) == 5 && geometry::primitive_count(&output) == 4 && geometry::vertex_count(&output) == 12,
            "crossing output cardinality",
        );
        check(topology_signature(&output) == topology_signature(&parallel), "crossing one/four-domain topology differs");
        let positions = packed::float3::view::view(geometry::positions(&output)).unwrap();
        check(positions.x[4] == 1. && positions.y[4] == 1. && positions.z[4] == 5., "crossing 3D interpolation");
        let p = attribute::Owner::Point;
        let weights = float_values(p, "weight", &output);
        check(weights == float_values(p, "weight", &parallel) && weights[4] == 5., "crossing float payload interpolation");
        let labels = match storage(p, "label", &output) {
            attribute::Storage::Text(values) => values,
            _ => panic!("text storage changed"),
        };
        check(int_values(p, "id", &output)[4] == 12 && &labels[4] == b"c", "crossing discrete payload policy");
        check(group::mem(4, &find_group(group::Owner::Point, "selected", &output)), "split point group ancestry");
        let crossings = find_group(group::Owner::Point, "crossings", &output);
        check(group::cardinality(&crossings) == 1 && group::mem(4, &crossings), "split output group");
        check(edge_cardinality("constraints", &output) == 4, "split constraint edge cardinality");
        let projected = tri(
            Params {
                grain: 1,
                constraint_primitives: Some(constraints.clone()),
                split_crossing_constraints: true,
                restore_original_point_positions: false,
                ..xy()
            },
            &input,
        );
        let (_, _, projected_z) = position_signature(&projected);
        check(projected_z.iter().all(|&z| z == 0.), "crossing split point was not embedded on the projected output plane");
        let authored_positions = f3(vec![0., 2., 2., 0., 1.], vec![0., 0., 2., 2., 1.], vec![0., 0., 0., 0., 7.]);
        let authored_topology = polylines(
            5,
            vec![0, 2, 1, 3],
            vec![0, 2, 4],
            vec![PrimitiveKind::OpenPolyline, PrimitiveKind::OpenPolyline],
        );
        let authored = plain(authored_positions, authored_topology);
        let authored_constraints = pgroup(group::Owner::Primitive, "authored_crossing", 2, &|_| true);
        let authored = tri(
            Params {
                grain: 1,
                constraint_primitives: Some(authored_constraints),
                split_crossing_constraints: true,
                split_point_group: Some("generated".to_owned()),
                constraint_group: Some("constraints".to_owned()),
                ..xy()
            },
            &authored,
        );
        check(
            geometry::point_count(&authored) == 5 && geometry::primitive_count(&authored) == 4,
            "authored crossing point was duplicated by exact construction",
        );
        check(
            group::cardinality(&find_group(group::Owner::Point, "generated", &authored)) == 0,
            "authored crossing point was marked as generated",
        );
        check(edge_cardinality("constraints", &authored) == 4, "authored crossing constraint cardinality");
    }

    #[test]
    fn hull_boundary_flood() {
        let positions = f3(
            vec![-2., 2., 2., -2., -1., 1., 1., -1., 0.],
            vec![-2., -2., 2., 2., -1., -1., 1., 1., 0.],
            vec![0.; 9],
        );
        let topology_value = polylines(9, vec![4, 5, 6, 7], vec![0, 4], vec![PrimitiveKind::ClosedPolyline]);
        let constraint_group = pgroup(group::Owner::Primitive, "boundary", 1, &|_| true);
        let input = build(positions, topology_value.clone(), Vec::new(), vec![constraint_group.clone()], Vec::new());
        let flood = || Params {
            grain: 1,
            constraint_primitives: Some(constraint_group.clone()),
            constraint_group: Some("boundary".to_owned()),
            ..xy()
        };
        let (output, parallel) = one_four(&|| tri(Params { flood_from_hull_boundary: true, ..flood() }, &input));
        check(topology_signature(&output) == topology_signature(&parallel), "hull flood one/four-domain topology differs");
        check(geometry::primitive_count(&output) == 4, "hull flood retained the wrong triangle count");
        let view = topology::view::view(geometry::topology(&output)).unwrap();
        check(view.vertex_points.iter().all(|&point| point >= 4), "hull flood retained exterior geometry");
        check(edge_cardinality("boundary", &output) == 4, "hull flood constrained-edge cardinality");
        let polygon = tri(Params { remove_outside_constraint_polygons: true, ..flood() }, &input);
        check(
            topology_signature(&polygon) == topology_signature(&output),
            "constraint-polygon winding differs from simple hull-flood interior",
        );
        let empty = tri(
            Params { flood_from_hull_boundary: true, ..xy() },
            &points(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)]),
        );
        check(geometry::primitive_count(&empty) == 0, "unblocked adapter hull flood did not remove all triangles");
        let index = topology_index::create(None, &topology_value).unwrap();
        let all_edges = edge_group::init(None, &topology_value, &index, "edge_only", &|_| true).unwrap();
        let edge_only = tri(
            Params { constraint_edges: Some(all_edges), remove_outside_constraint_polygons: true, ..xy() },
            &input,
        );
        check(geometry::primitive_count(&edge_only) == 0, "edge-only constraints incorrectly contributed polygon winding");
    }

    #[test]
    fn ignore_non_constraint_points() {
        let positions = f3(vec![0., 2., 2., 0., 1.], vec![0., 0., 2., 2., 1.], vec![0., 0., 0., 0., 9.]);
        let topology_value = polygons(5, vec![0, 1, 2, 3], vec![0, 4]);
        let authored_normal = attr(
            "N",
            attribute::Owner::Point,
            attribute::Storage::Float3(f3(vec![1.; 5], vec![0.; 5], vec![0.; 5])),
        );
        let input = build(positions, topology_value, vec![authored_normal], Vec::new(), Vec::new());
        let boundary = pgroup(group::Owner::Primitive, "boundary", 1, &|_| true);
        let ordinary = tri(Params { constraint_primitives: Some(boundary.clone()), ..xy() }, &input);
        check(geometry::primitive_count(&ordinary) == 4, "ordinary triangulation did not include the interior point");
        let (ignored, parallel) = one_four(&|| {
            tri(
                Params {
                    grain: 1,
                    constraint_primitives: Some(boundary.clone()),
                    ignore_non_constraint_points: true,
                    constraint_group: Some("boundary".to_owned()),
                    ..xy()
                },
                &input,
            )
        });
        check(
            geometry::point_count(&ignored) == 5 && geometry::primitive_count(&ignored) == 2,
            "Ignore Non-Constraint Points cardinality",
        );
        check(topology_signature(&ignored) == topology_signature(&parallel), "Ignore Non-Constraint Points differs across domains");
        let view = topology::view::view(geometry::topology(&ignored)).unwrap();
        check(view.vertex_points.iter().all(|&point| point != 4), "ignored interior point entered output topology");
        check(edge_cardinality("boundary", &ignored) == 4, "Ignore Non-Constraint Points lost constraint edges");
        let compacted = tri(
            Params {
                grain: 1,
                constraint_primitives: Some(boundary),
                ignore_non_constraint_points: true,
                remove_unused_points: true,
                recompute_point_normals: true,
                ..xy()
            },
            &input,
        );
        check(geometry::point_count(&compacted) == 4, "Triangulate 2D unused-point compaction cardinality");
        match storage(attribute::Owner::Point, "N", &compacted) {
            attribute::Storage::Float3(values) => {
                let values = packed::float3::view::view(&values).unwrap();
                check(
                    values.x.iter().all(|&v| v == 0.) && values.y.iter().all(|&v| v == 0.) && values.z.iter().all(|&v| v == 1.),
                    "Triangulate 2D point normals were not recomputed",
                );
            }
            _ => panic!("Triangulate 2D recomputed N changed storage"),
        }
        expect_error(run_with(
            Params { ignore_non_constraint_points: true, ..xy() },
            &points(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)]),
        ));
    }

    #[test]
    fn remove_duplicate_points() {
        let input = points(&[(0., 0., 0.), (1., 0., 0.), (1., 1., 0.), (0., 1., 0.), (0., 0., 7.), (9., 9., 9.)]);
        let id = attr("id", attribute::Owner::Point, attribute::Storage::Int(Arc::new(vec![10, 11, 12, 13, 14, 15])));
        let selected = group::ordered(group::Owner::Point, "selected", 6, &[0, 1, 2, 3, 4]).unwrap();
        let markers = pgroup(group::Owner::Point, "markers", 6, &|p| p == 4 || p == 5);
        let input = geometry::with_group(
            markers,
            &geometry::with_group(selected.clone(), &geometry::with_attribute(id, &input).unwrap()).unwrap(),
        ).unwrap();
        let selection = || Some(element_selection::T::SelectedPoints(selected.clone()));
        let ordinary = tri(Params { selection: selection(), ..xy() }, &input);
        check(geometry::point_count(&ordinary) == 6, "projected duplicate was removed without the output policy");
        let (output, parallel) =
            one_four(&|| tri(Params { grain: 1, selection: selection(), remove_duplicate_points: true, ..xy() }, &input));
        check(
            geometry::point_count(&output) == 5 && geometry::primitive_count(&output) == 2,
            "projected duplicate removal cardinality",
        );
        check(
            topology_signature(&output) == topology_signature(&parallel),
            "projected duplicate removal topology differs across domains",
        );
        let (x, y, z) = position_signature(&output);
        check(
            x == [0., 1., 1., 0., 9.] && y == [0., 0., 1., 1., 9.] && z == [0., 0., 0., 0., 9.],
            "projected duplicate removal deleted or reordered an unrelated point",
        );
        check(
            int_values(attribute::Owner::Point, "id", &output) == [10, 11, 12, 13, 15],
            "projected duplicate point attribute remap",
        );
        let selected_out = find_group(group::Owner::Point, "selected", &output);
        check(
            group::cardinality(&selected_out) == 4 && !group::mem(4, &selected_out),
            "projected duplicate selection group remap",
        );
        let markers_out = find_group(group::Owner::Point, "markers", &output);
        check(
            group::cardinality(&markers_out) == 1 && group::mem(4, &markers_out),
            "projected duplicate removal lost unrelated group membership",
        );
    }

    #[test]
    fn quality_refinement() {
        let input = points(&[(0., 0., 0.), (2., 0., 2.), (2., 2., 6.), (0., 2., 4.)]);
        let value = attr("value", attribute::Owner::Point, attribute::Storage::Float(Arc::new(vec![0., 2., 6., 4.])));
        let input = geometry::with_attribute(value, &input).unwrap();
        let (output, parallel) = one_four(&|| {
            tri(
                Params {
                    grain: 1,
                    refine: true,
                    maximum_area: Some(0.3),
                    maximum_new_points: 100,
                    refinement_point_group: Some("refined".to_owned()),
                    ..xy()
                },
                &input,
            )
        });
        check(
            geometry::point_count(&output) > 4 && geometry::point_count(&output) <= 104,
            "quality refinement point budget",
        );
        check(
            topology_signature(&output) == topology_signature(&parallel)
                && position_signature(&output) == position_signature(&parallel),
            "quality refinement differs across domains",
        );
        check(
            group::cardinality(&find_group(group::Owner::Point, "refined", &output)) == geometry::point_count(&output) - 4,
            "quality refinement output group",
        );
        let (px, py, pz) = position_signature(&output);
        let values = float_values(attribute::Owner::Point, "value", &output);
        for point in 0..geometry::point_count(&output) as usize {
            check((pz[point] - values[point]).abs() < 1e-10, "quality refinement payload interpolation");
        }
        let topology_view = topology::view::view(geometry::topology(&output)).unwrap();
        for primitive in 0..geometry::primitive_count(&output) as usize {
            let first = topology_view.primitive_offsets[primitive] as usize;
            let a = topology_view.vertex_points[first] as usize;
            let b = topology_view.vertex_points[first + 1] as usize;
            let c = topology_view.vertex_points[first + 2] as usize;
            let twice_area = (((px[b] - px[a]) * (py[c] - py[a])) - ((py[b] - py[a]) * (px[c] - px[a]))).abs();
            check(0.5 * twice_area <= 0.3 + 1e-12, "quality refinement maximum area");
        }
        let constrained = plain(
            f3(vec![0., 4., 2.], vec![0., 0., 0.2], vec![0., 8., 9.]),
            polylines(3, vec![0, 1], vec![0, 2], vec![PrimitiveKind::OpenPolyline]),
        );
        let base = || pgroup(group::Owner::Primitive, "base", 1, &|_| true);
        let split = tri(
            Params {
                grain: 1,
                constraint_primitives: Some(base()),
                refine: true,
                minimum_angle: std::f64::consts::PI / 6.,
                maximum_new_points: 1,
                allow_constraint_splitting: true,
                refinement_point_group: Some("refined".to_owned()),
                constraint_group: Some("constraints".to_owned()),
                ..xy()
            },
            &constrained,
        );
        check(geometry::point_count(&split) == 4, "constraint refinement point cardinality");
        let (sx, sy, sz) = position_signature(&split);
        check(sx[3] == 2. && sy[3] == 0. && sz[3] == 4., "constraint refinement midpoint interpolation");
        check(edge_cardinality("constraints", &split) == 2, "constraint refinement did not split the protected edge");
        let boundary_geometry = plain(
            f3(vec![0., 2., 2., 0.], vec![0., 0., 2., 2.], vec![0.; 4]),
            polygons(4, vec![0, 1, 2, 3], vec![0, 4]),
        );
        let boundary_refined = tri(
            Params {
                grain: 1,
                constraint_primitives: Some(pgroup(group::Owner::Primitive, "boundary", 1, &|_| true)),
                ignore_non_constraint_points: true,
                remove_outside_constraint_polygons: true,
                refine: true,
                minimum_angle: 1e-6,
                maximum_area: Some(0.3),
                maximum_new_points: 64,
                allow_constraint_splitting: true,
                constraint_group: Some("constraints".to_owned()),
                ..xy()
            },
            &boundary_geometry,
        );
        check(geometry::primitive_count(&boundary_refined) > 2, "constraint-splitting refinement lost a bounded polygon interior");
        let crossing_geometry = plain(
            f3(vec![20., 300., 300., 20.], vec![20., 20., 200., 200.], vec![0.; 4]),
            polylines(
                4,
                vec![0, 1, 2, 3, 0, 2, 1, 3],
                vec![0, 4, 6, 8],
                vec![PrimitiveKind::Polygon, PrimitiveKind::OpenPolyline, PrimitiveKind::OpenPolyline],
            ),
        );
        let crossing_group = pgroup(group::Owner::Primitive, "all_constraints", 3, &|_| true);
        for maximum_new_points in [1_i64, 2, 4, 8, 16, 32, 64, 96] {
            let refined = tri(
                Params {
                    grain: 1,
                    constraint_primitives: Some(crossing_group.clone()),
                    split_crossing_constraints: true,
                    flood_from_hull_boundary: true,
                    remove_outside_constraint_polygons: true,
                    silhouette_constraints: true,
                    remove_outside_silhouette: true,
                    ignore_non_constraint_points: true,
                    refine: true,
                    minimum_angle: std::f64::consts::PI / 18.,
                    maximum_area: Some(1_500.),
                    maximum_new_points,
                    allow_constraint_splitting: true,
                    ..xy()
                },
                &crossing_geometry,
            );
            check(
                geometry::primitive_count(&refined) > 0,
                &format!("multi-constraint refinement lost its interior at point budget {maximum_new_points}"),
            );
        }
        let unsplit = tri(
            Params {
                grain: 1,
                constraint_primitives: Some(base()),
                refine: true,
                minimum_angle: std::f64::consts::PI / 6.,
                maximum_new_points: 1,
                allow_constraint_splitting: false,
                constraint_group: Some("constraints".to_owned()),
                ..xy()
            },
            &constrained,
        );
        check(edge_cardinality("constraints", &unsplit) == 1, "disabled constraint splitting changed the protected edge");
        let targeted = tri(
            Params {
                grain: 1,
                refine: true,
                minimum_angle: 1e-6,
                target_edge_length: Some(0.75),
                maximum_new_points: 100,
                ..xy()
            },
            &input,
        );
        let (tx, ty, _) = position_signature(&targeted);
        let targeted_index = topology_index::create(None, geometry::topology(&targeted)).unwrap();
        let targeted_view = topology_index::view::view(&targeted_index);
        for edge in 0..targeted_view.edge_a.len() {
            let a = targeted_view.edge_a[edge] as usize;
            let b = targeted_view.edge_b[edge] as usize;
            let dx = tx[b] - tx[a];
            let dy = ty[b] - ty[a];
            check((dx * dx + dy * dy).sqrt() <= 0.75 + 1e-12, "quality refinement target edge length");
        }
        let stopped = tri(
            Params { refine: true, maximum_area: Some(0.01), maximum_new_points: 0, ..xy() },
            &input,
        );
        check(geometry::point_count(&stopped) == 4, "zero refinement point budget was not respected");
        let edge_limited = tri(
            Params {
                refine: true,
                maximum_area: Some(0.01),
                minimum_edge_length: 10.,
                maximum_new_points: 100,
                ..xy()
            },
            &input,
        );
        check(geometry::point_count(&edge_limited) == 4, "minimum refinement edge length was not respected");
        let irregular = points(&[(0., 0., 0.), (3., 0., 3.), (2., 2., 6.), (0., 1., 2.)]);
        let regularize = |steps: i64| Params {
            grain: 1,
            refine: true,
            minimum_angle: 1e-6,
            maximum_area: Some(0.2),
            maximum_new_points: 64,
            regularization_steps: steps,
            ..xy()
        };
        let unregularized = tri(regularize(0), &irregular);
        let (regularized, regularized_parallel) = one_four(&|| tri(regularize(2), &irregular));
        check(
            topology_signature(&regularized) == topology_signature(&regularized_parallel)
                && position_signature(&regularized) == position_signature(&regularized_parallel),
            "quality regularization differs across domains",
        );
        let (rx, ry, rz) = position_signature(&regularized);
        let (ux, uy, _) = position_signature(&unregularized);
        let mut moved = false;
        for point in 4..geometry::point_count(&regularized) as usize {
            if rx[point] != ux[point] || ry[point] != uy[point] {
                moved = true;
            }
            check((rz[point] - (rx[point] + 2. * ry[point])).abs() < 1e-9, "regularization provenance lost linear 3D payload");
        }
        check(moved, "quality regularization did not move any generated interior point");
        let movable = plain(
            f3(vec![0., 2., 2., 0., 0.2], vec![0., 0., 2., 2., 0.3], vec![0., 2., 6., 4., 0.8]),
            polygons(5, vec![0, 1, 2, 3], vec![0, 4]),
        );
        let movable_boundary = pgroup(group::Owner::Primitive, "boundary", 1, &|_| true);
        let (moved_one, moved_four) = one_four(&|| {
            tri(
                Params {
                    grain: 1,
                    constraint_primitives: Some(movable_boundary.clone()),
                    remove_outside_constraint_polygons: true,
                    refine: true,
                    minimum_angle: 1e-6,
                    maximum_new_points: 0,
                    regularization_steps: 2,
                    allow_movement_of_interior_input_points: true,
                    constraint_group: Some("boundary".to_owned()),
                    ..xy()
                },
                &movable,
            )
        });
        check(
            topology_signature(&moved_one) == topology_signature(&moved_four) && geometry::point_count(&moved_one) == 5,
            "input-point regularization differs across domains",
        );
        check(
            edge_cardinality("boundary", &moved_one) == 4,
            "input-point regularization moved or lost a protected boundary",
        );
    }

    #[test]
    fn projected_silhouette() {
        let positions = f3(
            vec![-2., 2., 2., -2., -1., 1., 1., -1., 0.],
            vec![-2., -2., 2., 2., -1., -1., 1., 1., 0.],
            vec![0.; 9],
        );
        let input = plain(positions.clone(), polygons(9, vec![4, 5, 6, 4, 6, 7], vec![0, 3, 6]));
        let constrained = tri(
            Params {
                grain: 1,
                silhouette_constraints: true,
                constraint_group: Some("silhouette".to_owned()),
                ..xy()
            },
            &input,
        );
        check(edge_cardinality("silhouette", &constrained) == 4, "silhouette retained an internal same-facing edge");
        let (output, parallel) = one_four(&|| {
            tri(
                Params {
                    grain: 1,
                    remove_outside_silhouette: true,
                    constraint_group: Some("silhouette".to_owned()),
                    ..xy()
                },
                &input,
            )
        });
        check(topology_signature(&output) == topology_signature(&parallel), "silhouette one/four-domain topology differs");
        check(geometry::primitive_count(&output) == 4, "silhouette removal retained the wrong triangle count");
        let view = topology::view::view(geometry::topology(&output)).unwrap();
        check(view.vertex_points.iter().all(|&point| point >= 4), "silhouette removal retained an exterior point");
        let reversed = tri(
            Params { remove_outside_silhouette: true, ..xy() },
            &plain(positions, polygons(9, vec![6, 5, 4, 7, 6, 4], vec![0, 3, 6])),
        );
        check(
            topology_signature(&reversed) == topology_signature(&output),
            "silhouette removal depends on global face orientation",
        );
        let partial = pgroup(group::Owner::Point, "partial", 9, &|p| p == 4 || p == 5 || p == 7);
        expect_error(run_with(
            Params {
                selection: Some(element_selection::T::SelectedPoints(partial)),
                silhouette_constraints: true,
                ..xy()
            },
            &input,
        ));
        let flat = tri(
            Params { remove_outside_silhouette: true, ..xy() },
            &plain(
                f3(vec![0., 1., 2., 0., 2.], vec![0., 0., 0., 1., 1.], vec![0.; 5]),
                polygons(5, vec![0, 1, 2], vec![0, 3]),
            ),
        );
        check(geometry::primitive_count(&flat) == 0, "zero-area projected silhouette invented an interior");
    }

    #[test]
    fn domain_exactness() {
        let count = 2_000_i64;
        let input = points(
            &(0..count)
                .map(|point| {
                    let x = (point * 7919 % 2003) as f64;
                    let y = (point * 3571 % 2011) as f64 + (point as f64 * 1e-8);
                    (x, y, (x * 0.25) - (y * 0.125))
                })
                .collect::<Vec<_>>(),
        );
        let (one, four) = one_four(&|| tri(Params { grain: 31, ..Params::default() }, &input));
        check(topology_signature(&one) == topology_signature(&four), "one/four-domain topology differs");
    }

    #[test]
    fn errors() {
        let input = points(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)]);
        expect_error(run_with(
            Params { projection: Projection::Plane { origin: vec3::create(0., 0., 0.), normal: vec3::create(0., 0., 0.) }, ..Params::default() },
            &input,
        ));
        expect_error(run_with(Params { projection: Projection::PointAttribute("missing".to_owned()), ..Params::default() }, &input));
        let bad_coordinates = attr(
            "bad_coordinates",
            attribute::Owner::Point,
            attribute::Storage::Float2(packed::float2::of_owned(vec![0., f64::NAN, 0.], vec![0., 0., 1.]).unwrap()),
        );
        let nonfinite = geometry::with_attribute(bad_coordinates, &input).unwrap();
        expect_error(run_with(
            Params { projection: Projection::PointAttribute("bad_coordinates".to_owned()), ..Params::default() },
            &nonfinite,
        ));
        expect_error(run_with(Params { refine: true, minimum_angle: std::f64::consts::PI / 3., ..Params::default() }, &input));
        expect_error(run_with(Params { refine: true, maximum_area: Some(0.), ..Params::default() }, &input));
        expect_error(run_with(Params { refine: true, target_edge_length: Some(f64::NAN), ..Params::default() }, &input));
        expect_error(run_with(Params { refine: true, minimum_edge_length: -1., ..Params::default() }, &input));
        expect_error(run_with(Params { refine: true, maximum_new_points: -1, ..Params::default() }, &input));
        expect_error(run_with(Params { refine: true, regularization_steps: -1, ..Params::default() }, &input));
        expect_error(run_with(Params { refinement_point_group: Some(" ".to_owned()), ..Params::default() }, &input));
        let token = cancel::create();
        cancel::cancel(&token);
        expect_error(run(Some(&token), &Params::default(), &input));
        expect_error(run(
            Some(&token),
            &Params { refine: true, maximum_area: Some(0.01), ..Params::default() },
            &input,
        ));
    }
}
