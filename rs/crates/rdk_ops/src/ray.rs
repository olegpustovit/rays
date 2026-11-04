use std::sync::Arc;

use rdk_core::{
    attribute, attribute_pattern, cancel, element_selection, error, geometry, group, packed, topology,
    topology_index,
};
use rdk_geom::point_grid::float_max;
use rdk_geom::surface_index::View::RayDirections;
use rdk_geom::surface_index::{self, RayDirectionMode, RaySurfaceHit};

use crate::attribute_ops;
use crate::attribute_transfer::Unmatched;
use crate::deform;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Method {
    RayMinimumDistance,
    RayProject,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Direction {
    RayVector(rays_math::vec3::T),
    RayNormal,
    RayAttribute(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Combine {
    RayAverage,
    RayMedian,
    RayShortest,
    RayLongest,
}

/// Every field is required: the SOP declaration owns the defaults.
pub struct Params {
    pub grain: i64,
    pub selection: Option<element_selection::T>,
    pub collision_primitives: Option<group::T>,
    pub method: Method,
    pub direction: Direction,
    pub direction_mode: RayDirectionMode,
    pub surface_hit: RaySurfaceHit,
    pub samples: i64,
    pub jitter_scale: f64,
    pub seed: i64,
    pub combine: Combine,
    pub min_distance: f64,
    pub max_distance: Option<f64>,
    pub tolerance: f64,
    pub scale: f64,
    pub lift: f64,
    pub distance_attribute: Option<String>,
    pub primitive_attribute: Option<String>,
    pub source_vertex_numbers_attribute: Option<String>,
    pub source_vertex_weights_attribute: Option<String>,
    pub hit_group: Option<String>,
    pub normal_attribute: Option<String>,
    pub point_pattern: Option<String>,
    pub vertex_pattern: Option<String>,
    pub primitive_pattern: Option<String>,
    pub detail_pattern: Option<String>,
    pub match_groups: bool,
}

fn fail(code: &str, message: &str) -> error::T {
    error::make("ray".to_owned(), code.to_owned(), message.to_owned(), None).unwrap_or_else(|e| e)
}

fn blank(value: &str) -> bool {
    value.bytes().all(|b| matches!(b, b' ' | 0x0c | b'\n' | b'\r' | b'\t'))
}

fn validate_name(label: &str, name: &Option<String>) -> Result<(), error::T> {
    match name {
        None => Ok(()),
        Some(name) if blank(name) => Err(fail("invalid_name", &format!("{label} must not be empty"))),
        Some(name) if name == "P" => Err(fail("invalid_name", &format!("{label} cannot be P"))),
        Some(_) => Ok(()),
    }
}

fn unique_attribute_name(
    source: &geometry::T,
    collision: &geometry::T,
    reserved: &[String],
    base: &str,
) -> String {
    let exists = |g: &geometry::T, name: &str| geometry::attributes(g).iter().any(|a| attribute::name(a) == name);
    let mut suffix = 0;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}{suffix}") };
        if reserved.contains(&name) || exists(source, &name) || exists(collision, &name) {
            suffix += 1;
        } else {
            return name;
        }
    }
}

fn point_selection(
    grain: i64,
    geometry_value: &geometry::T,
    selection: Option<&element_selection::T>,
    cancel_ref: Option<&cancel::T>,
) -> Result<Option<group::T>, error::T> {
    let topology_value = geometry::topology(geometry_value);
    deform::validate_selection(topology_value, selection)
        .map_err(|e| fail("invalid_selection", error::message(&e)))?;
    match selection {
        None => Ok(None),
        Some(element_selection::T::SelectedPoints(item)) => Ok(Some(item.clone())),
        Some(_) => {
            let index = topology_index::create(cancel_ref, topology_value)?;
            Ok(Some(group::init(
                Some(grain),
                group::Owner::Point,
                "__ray_selection",
                geometry::point_count(geometry_value),
                &|point| deform::point_selected(selection, Some(&index), point).unwrap_or(false),
            )?))
        }
    }
}

fn direction_planes(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    source: &geometry::T,
    direction: &Direction,
) -> Result<RayDirections, error::T> {
    let per_query = |values: deform::Planes| -> Result<RayDirections, error::T> {
        let shared = packed::Float3::View::of_shared_exn(
            Arc::from(values.x),
            Arc::from(values.y),
            Arc::from(values.z),
        )?;
        Ok(RayDirections::PerQueryDirections(packed::Float3::View::view(&shared)?))
    };
    match direction {
        Direction::RayVector(value) => {
            if !(value.x.is_finite() && value.y.is_finite() && value.z.is_finite()) {
                return Err(fail("invalid_direction", "ray direction vector must be finite"));
            }
            if value.x == 0.0 && value.y == 0.0 && value.z == 0.0 {
                return Err(fail("invalid_direction", "ray direction vector must be non-zero"));
            }
            Ok(RayDirections::ConstantDirection { x: value.x, y: value.y, z: value.z })
        }
        Direction::RayAttribute(name) => {
            if blank(name) {
                return Err(fail("invalid_direction", "ray direction attribute name must not be empty"));
            }
            let values = deform::point_vector_attribute("Rdk.Ray.ray", name, source)
                .map_err(|e| fail("invalid_direction", error::message(&e)))?;
            per_query(values)
        }
        Direction::RayNormal => {
            let values = deform::resolve_directions(cancel_ref, grain, source)
                .map_err(|e| fail("invalid_direction", error::message(&e)))?;
            per_query(values)
        }
    }
}

fn wrap63(value: i64) -> i64 {
    (value << 1) >> 1
}

fn jitter_stream(query: i64, sample: i64, component: i64) -> i64 {
    wrap63(
        wrap63(wrap63(query.wrapping_mul(0x1e3779b97f4a7c15)) ^ wrap63(sample.wrapping_mul(0x14d049bb133111eb)))
            ^ wrap63(component.wrapping_mul(0x11b54a32d192ed03)),
    )
}

#[allow(clippy::too_many_arguments)]
fn jittered_direction(
    seed: i64,
    query: i64,
    sample: i64,
    jitter_scale: f64,
    dx: f64,
    dy: f64,
    dz: f64,
    output: &mut [f64],
) {
    if sample == 0 || jitter_scale == 0.0 {
        output[0] = dx;
        output[1] = dy;
        output[2] = dz;
    } else {
        let inverse_xy = 1.0 / dx.hypot(dy);
        let (ux, uy, uz) = if inverse_xy.is_finite() { (-dy * inverse_xy, dx * inverse_xy, 0.0) } else { (1.0, 0.0, 0.0) };
        let vx = dy * uz - dz * uy;
        let vy = dz * ux - dx * uz;
        let vz = dx * uy - dy * ux;
        let random = rays_math::rand::seed(seed);
        let radius = rays_math::rand::float_at(random, jitter_stream(query, sample, 0)).sqrt() * jitter_scale;
        let angle = 2.0 * std::f64::consts::PI * rays_math::rand::float_at(random, jitter_stream(query, sample, 1));
        let tangent_u = radius * angle.cos();
        let tangent_v = radius * angle.sin();
        let scale = float_max(1.0, float_max(tangent_u.abs(), tangent_v.abs()));
        let tangent_u = tangent_u / scale;
        let tangent_v = tangent_v / scale;
        let x = dx / scale + tangent_u * ux + tangent_v * vx;
        let y = dy / scale + tangent_u * uy + tangent_v * vy;
        let z = dz / scale + tangent_u * uz + tangent_v * vz;
        let inverse = 1.0 / (x * x + y * y + z * z).sqrt();
        output[0] = x * inverse;
        output[1] = y * inverse;
        output[2] = z * inverse;
    }
}

fn sample_distance(float_results: &[f64], result: usize) -> f64 {
    float_results[result * 4]
}

fn sample_hit(integer_results: &[i64], result: usize) -> bool {
    integer_results[result * 2] >= 0
}

fn compare_samples(float_results: &[f64], left: usize, right: usize) -> i64 {
    let left_distance = float_results[left * 4];
    let right_distance = float_results[right * 4];
    if left_distance < right_distance {
        -1
    } else if left_distance > right_distance {
        1
    } else if left < right {
        -1
    } else if left > right {
        1
    } else {
        0
    }
}

fn sample_sift_down(float_results: &[f64], values: &mut [usize], root: usize, count: usize) {
    let mut at = root;
    loop {
        let child = at * 2 + 1;
        if child >= count {
            break;
        }
        let selected = if child + 1 < count && compare_samples(float_results, values[child], values[child + 1]) < 0 {
            child + 1
        } else {
            child
        };
        if compare_samples(float_results, values[at], values[selected]) < 0 {
            values.swap(at, selected);
            at = selected;
        } else {
            break;
        }
    }
}

fn sort_samples(float_results: &[f64], values: &mut [usize], count: usize) {
    for root in (0..count / 2).rev() {
        sample_sift_down(float_results, values, root, count);
    }
    for remaining in (1..count).rev() {
        values.swap(0, remaining);
        sample_sift_down(float_results, values, 0, remaining);
    }
}

struct Combined {
    counts: [i64; 2],
    distances: [f64; 2],
    selected: [i64; 2],
}

#[allow(clippy::too_many_arguments)]
fn combine_samples(
    combine: Combine,
    integer_results: &[i64],
    float_results: &[f64],
    first: usize,
    samples: usize,
    order: &mut [usize],
    combined: &mut Combined,
    direction: usize,
) {
    let mut count: i64 = 0;
    let mut mean = 0.0f64;
    let mut shortest: i64 = -1;
    let mut longest: i64 = -1;
    for sample in 0..samples {
        let result = first + sample;
        if sample_hit(integer_results, result) {
            order[count as usize] = result;
            count += 1;
            let distance = sample_distance(float_results, result);
            mean += (distance - mean) / count as f64;
            if shortest < 0 || distance < sample_distance(float_results, shortest as usize) {
                shortest = result as i64;
            }
            if longest < 0 || distance > sample_distance(float_results, longest as usize) {
                longest = result as i64;
            }
        }
    }
    combined.counts[direction] = count;
    if count > 0 {
        let (distance, selected) = match combine {
            Combine::RayShortest => (sample_distance(float_results, shortest as usize), shortest),
            Combine::RayLongest => (sample_distance(float_results, longest as usize), longest),
            Combine::RayMedian => {
                sort_samples(float_results, order, count as usize);
                let selected = order[count as usize / 2];
                (sample_distance(float_results, selected), selected as i64)
            }
            Combine::RayAverage => {
                let distance = mean;
                let mut selected = order[0];
                let mut difference = f64::INFINITY;
                for slot in 0..count as usize {
                    let candidate = order[slot];
                    let candidate_difference = (sample_distance(float_results, candidate) - distance).abs();
                    if candidate_difference < difference {
                        difference = candidate_difference;
                        selected = candidate;
                    }
                }
                (distance, selected as i64)
            }
        };
        combined.distances[direction] = distance;
        combined.selected[direction] = selected;
    }
}

struct SampleOutputs<'a> {
    primitives: &'a mut [i64],
    triangles: &'a mut [i64],
    barycentric_a: &'a mut [f64],
    barycentric_b: &'a mut [f64],
    barycentric_c: &'a mut [f64],
    distances: &'a mut [f64],
    direction_signs: &'a mut [i64],
    hit_counts: &'a mut [i64],
}

#[allow(clippy::too_many_arguments)]
fn raycast_samples_into(
    cancel_ref: Option<&cancel::T>,
    selection: Option<&group::T>,
    grain: i64,
    value: &surface_index::T,
    queries: &packed::Float3::T,
    directions: &RayDirections,
    min_distance: f64,
    max_distance: f64,
    tolerance: f64,
    direction_mode: RayDirectionMode,
    surface_hit: RaySurfaceHit,
    samples: i64,
    jitter_scale: f64,
    seed: i64,
    combine: Combine,
    out: &mut SampleOutputs,
    normals: Option<(&mut [f64], &mut [f64], &mut [f64])>,
) -> Result<(), error::T> {
    let invalid = |m: &str| fail("invalid_parameter", m);
    if grain <= 0 {
        return Err(invalid("Surface_index: grain must be positive"));
    }
    if !(2..=1024).contains(&samples) {
        return Err(invalid("Surface_index: sampled ray count must be between 2 and 1024"));
    }
    if !(jitter_scale.is_finite() && jitter_scale >= 0.0) {
        return Err(invalid("Surface_index: ray jitter scale must be finite and non-negative"));
    }
    let query_count = packed::Float3::length(queries) as usize;
    if out.primitives.len() < query_count
        || out.triangles.len() < query_count
        || out.barycentric_a.len() < query_count
        || out.barycentric_b.len() < query_count
        || out.barycentric_c.len() < query_count
        || out.distances.len() < query_count
        || out.direction_signs.len() < query_count
        || out.hit_counts.len() < query_count
    {
        return Err(invalid("Surface_index: sampled ray output arrays are too short"));
    }
    if let RayDirections::PerQueryDirections(values) = directions {
        if values.x.len() != query_count || values.y.len() != query_count || values.z.len() != query_count {
            return Err(invalid("Surface_index: ray direction count does not match queries"));
        }
    }
    if let Some(item) = selection {
        if group::owner(item) != group::Owner::Point || group::length(item) != query_count as i64 {
            return Err(invalid("Surface_index: ray selection must match query points"));
        }
    }
    let mut normals = normals;
    if let Some((x, y, z)) = &normals {
        if x.len() < query_count || y.len() < query_count || z.len() < query_count {
            return Err(invalid("Surface_index: sampled ray normal arrays are too short"));
        }
    }
    let queries = packed::Float3::View::view(queries)?;
    let samples_u = samples as usize;
    let range_count = query_count.div_ceil(grain as usize);
    for range in 0..range_count {
        let first = range * grain as usize;
        let last = query_count.min(first + grain as usize);
        // Scratch is per range, as in the OCaml worker: its `combined` slots carry over between
        // queries of one range.
        let result_count = samples_u * 2;
        let mut scratch = [0.0f64; 8];
        let mut node_stack = [0i64; 65];
        let mut integer_results = vec![-1i64; result_count * 2];
        let mut float_results = vec![0.0f64; result_count * 4];
        let mut order = vec![0usize; samples_u];
        let mut direction = [0.0f64; 3];
        let mut normal = [0.0f64; 3];
        let mut combined = Combined { counts: [0; 2], distances: [f64::INFINITY; 2], selected: [-1; 2] };
        for query in first..last {
            cancel::poll(cancel_ref, query as i64, 4095)?;
            out.primitives[query] = -1;
            out.triangles[query] = -1;
            out.barycentric_a[query] = 0.0;
            out.barycentric_b[query] = 0.0;
            out.barycentric_c[query] = 0.0;
            out.distances[query] = f64::INFINITY;
            out.direction_signs[query] = 0;
            out.hit_counts[query] = 0;
            let selected_query = match selection {
                None => true,
                Some(item) => group::mem(query as i64, item),
            };
            if !selected_query {
                continue;
            }
            let (ox, oy, oz) = (queries.x[query], queries.y[query], queries.z[query]);
            if !(ox.is_finite() && oy.is_finite() && oz.is_finite()) {
                return Err(invalid("Surface_index: selected ray origins must be finite"));
            }
            let (vx, vy, vz) = surface_index::View::direction_at(directions, query as i64);
            let (dx, dy, dz) = surface_index::View::normalized_direction(vx, vy, vz)?;
            let directions_to_query = match direction_mode {
                RayDirectionMode::RayForward | RayDirectionMode::RayReverse => 1,
                RayDirectionMode::RayBidirectionalClosest | RayDirectionMode::RayBidirectionalFarthest => 2,
            };
            for side in 0..directions_to_query {
                let sign = match direction_mode {
                    RayDirectionMode::RayReverse => -1.0,
                    RayDirectionMode::RayForward => 1.0,
                    RayDirectionMode::RayBidirectionalClosest | RayDirectionMode::RayBidirectionalFarthest => {
                        if side == 0 { 1.0 } else { -1.0 }
                    }
                };
                let base = side * samples_u;
                for sample in 0..samples {
                    cancel::poll(cancel_ref, sample, 63)?;
                    jittered_direction(seed, query as i64, sample, jitter_scale, sign * dx, sign * dy, sign * dz, &mut direction);
                    surface_index::View::ray_query_once(
                        value,
                        ox,
                        oy,
                        oz,
                        direction[0],
                        direction[1],
                        direction[2],
                        min_distance,
                        max_distance,
                        tolerance,
                        surface_hit,
                        &mut scratch,
                        &mut node_stack,
                        &mut integer_results,
                        &mut float_results,
                        (base + sample as usize) as i64,
                    )?;
                }
                combine_samples(combine, &integer_results, &float_results, base, samples_u, &mut order, &mut combined, side);
            }
            let chosen = if directions_to_query == 1 {
                0
            } else if combined.counts[0] == 0 {
                1
            } else if combined.counts[1] == 0 {
                0
            } else {
                match direction_mode {
                    RayDirectionMode::RayBidirectionalClosest => {
                        if combined.distances[1] < combined.distances[0] { 1 } else { 0 }
                    }
                    RayDirectionMode::RayBidirectionalFarthest => {
                        if combined.distances[1] > combined.distances[0] { 1 } else { 0 }
                    }
                    RayDirectionMode::RayForward | RayDirectionMode::RayReverse => {
                        return Err(invalid("Surface_index: unreachable ray direction mode"));
                    }
                }
            };
            let selected = combined.selected[chosen];
            if selected >= 0 {
                let integer = selected as usize * 2;
                let floating = selected as usize * 4;
                out.primitives[query] = integer_results[integer];
                out.triangles[query] = integer_results[integer + 1];
                out.barycentric_a[query] = float_results[floating + 1];
                out.barycentric_b[query] = float_results[floating + 2];
                out.barycentric_c[query] = float_results[floating + 3];
                out.distances[query] = combined.distances[chosen];
                out.hit_counts[query] = combined.counts[chosen];
                out.direction_signs[query] = match direction_mode {
                    RayDirectionMode::RayReverse => -1,
                    RayDirectionMode::RayForward => 1,
                    RayDirectionMode::RayBidirectionalClosest | RayDirectionMode::RayBidirectionalFarthest => {
                        if chosen == 0 { 1 } else { -1 }
                    }
                };
                if let Some((nx_out, ny_out, nz_out)) = normals.as_mut() {
                    let (mut nx, mut ny, mut nz) = (0.0f64, 0.0f64, 0.0f64);
                    if combine == Combine::RayAverage {
                        let base = chosen * samples_u;
                        for sample in 0..samples_u {
                            let result = base + sample;
                            if sample_hit(&integer_results, result) {
                                surface_index::View::triangle_normal_into(
                                    value,
                                    integer_results[result * 2 + 1],
                                    &mut normal,
                                );
                                nx += normal[0];
                                ny += normal[1];
                                nz += normal[2];
                            }
                        }
                        let inverse = 1.0 / combined.counts[chosen] as f64;
                        nx *= inverse;
                        ny *= inverse;
                        nz *= inverse;
                    } else {
                        surface_index::View::triangle_normal_into(value, out.triangles[query], &mut normal);
                        nx = normal[0];
                        ny = normal[1];
                        nz = normal[2];
                    }
                    nx_out[query] = nx;
                    ny_out[query] = ny;
                    nz_out[query] = nz;
                }
            }
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn raycast_average_drivers_into(
    cancel_ref: Option<&cancel::T>,
    selection: Option<&group::T>,
    grain: i64,
    value: &surface_index::T,
    queries: &packed::Float3::T,
    directions: &RayDirections,
    min_distance: f64,
    max_distance: f64,
    tolerance: f64,
    surface_hit: RaySurfaceHit,
    samples: i64,
    jitter_scale: f64,
    seed: i64,
    direction_signs: &[i64],
    hit_counts: &[i64],
    offsets: &[i64],
    vertex_numbers: &mut [i64],
    vertex_weights: &mut [f64],
) -> Result<(), error::T> {
    let query_count = packed::Float3::length(queries) as usize;
    if direction_signs.len() < query_count || hit_counts.len() < query_count || offsets.len() != query_count + 1 {
        return Err(fail("invalid_parameter", "Surface_index: average ray driver arrays do not match queries"));
    }
    let queries = packed::Float3::View::view(queries)?;
    let range_count = query_count.div_ceil(grain as usize);
    for range in 0..range_count {
        let first = range * grain as usize;
        let last = query_count.min(first + grain as usize);
        let mut scratch = [0.0f64; 8];
        let mut node_stack = [0i64; 65];
        let mut integer_results = [-1i64; 2];
        let mut float_results = [0.0f64; 4];
        let mut direction = [0.0f64; 3];
        for query in first..last {
            cancel::poll(cancel_ref, query as i64, 4095)?;
            let selected_query = match selection {
                None => true,
                Some(item) => group::mem(query as i64, item),
            };
            if direction_signs[query] != 0 && selected_query {
                let (ox, oy, oz) = (queries.x[query], queries.y[query], queries.z[query]);
                let (vx, vy, vz) = surface_index::View::direction_at(directions, query as i64);
                let (dx, dy, dz) = surface_index::View::normalized_direction(vx, vy, vz)?;
                let sign = direction_signs[query] as f64;
                let scale = 1.0 / hit_counts[query] as f64;
                let mut at = offsets[query] as usize;
                for sample in 0..samples {
                    cancel::poll(cancel_ref, sample, 63)?;
                    jittered_direction(seed, query as i64, sample, jitter_scale, sign * dx, sign * dy, sign * dz, &mut direction);
                    surface_index::View::ray_query_once(
                        value,
                        ox,
                        oy,
                        oz,
                        direction[0],
                        direction[1],
                        direction[2],
                        min_distance,
                        max_distance,
                        tolerance,
                        surface_hit,
                        &mut scratch,
                        &mut node_stack,
                        &mut integer_results,
                        &mut float_results,
                        0,
                    )?;
                    if integer_results[0] >= 0 {
                        let triangle = integer_results[1];
                        vertex_numbers[at] = surface_index::View::triangle_vertex(value, triangle, 0);
                        vertex_numbers[at + 1] = surface_index::View::triangle_vertex(value, triangle, 1);
                        vertex_numbers[at + 2] = surface_index::View::triangle_vertex(value, triangle, 2);
                        vertex_weights[at] = float_results[1] * scale;
                        vertex_weights[at + 1] = float_results[2] * scale;
                        vertex_weights[at + 2] = float_results[3] * scale;
                        at += 3;
                    }
                }
                if at as i64 != offsets[query + 1] {
                    return Err(fail("invalid_parameter", "Surface_index: average ray driver recount differed"));
                }
            }
        }
    }
    Ok(())
}

fn existing_point_float(name: &str, count: usize, source: &geometry::T, default: f64) -> Result<Vec<f64>, error::T> {
    match geometry::find_attribute(attribute::Owner::Point, name, source) {
        None => Ok(vec![default; count]),
        Some(found) => match attribute::View::storage(&found)? {
            attribute::Storage::Float(values) => Ok(values.clone()),
            _ => Err(fail("invalid_output", &format!("point output attribute \"{name}\" must have float storage"))),
        },
    }
}

fn existing_point_int(name: &str, count: usize, source: &geometry::T, default: i64) -> Result<Vec<i64>, error::T> {
    match geometry::find_attribute(attribute::Owner::Point, name, source) {
        None => Ok(vec![default; count]),
        Some(found) => match attribute::View::storage(&found)? {
            attribute::Storage::Int(values) => Ok(values.clone()),
            _ => Err(fail("invalid_output", &format!("point output attribute \"{name}\" must have integer storage"))),
        },
    }
}

type Planes3 = (Vec<f64>, Vec<f64>, Vec<f64>);

fn existing_point_float3(name: &str, count: usize, source: &geometry::T) -> Result<Planes3, error::T> {
    match geometry::find_attribute(attribute::Owner::Point, name, source) {
        None => Ok((vec![0.0; count], vec![0.0; count], vec![0.0; count])),
        Some(found) => match attribute::View::storage(&found)? {
            attribute::Storage::Float3(values) => {
                let values = packed::Float3::View::view(values)?;
                Ok((values.x.to_vec(), values.y.to_vec(), values.z.to_vec()))
            }
            _ => Err(fail("invalid_output", &format!("point output attribute \"{name}\" must have float3 storage"))),
        },
    }
}

/// Runs the projection; errors keep their code and are reported under the `ray` operation.
pub fn run(
    cancel_token: Option<&cancel::T>,
    params: &Params,
    source: &geometry::T,
    collision: &geometry::T,
) -> Result<geometry::T, error::T> {
    run_inner(cancel_token, params, source, collision).map_err(|value| {
        let code = error::code(&value);
        let message = error::message(&value);
        if code == "cancelled" {
            fail("cancelled", "ray projection was cancelled")
        } else if code == "invalid_parameter" && message.starts_with("Surface_index: selected ray origins") {
            fail("invalid_query", message)
        } else if code == "invalid_parameter" && message.starts_with("Surface_index: selected ray directions") {
            fail("invalid_direction", message)
        } else {
            error::make("ray".to_owned(), code.to_owned(), message.to_owned(), Some(error::hints(&value).to_vec()))
                .unwrap_or_else(|e| e)
        }
    })
}

#[allow(clippy::too_many_lines)]
fn run_inner(
    cancel_ref: Option<&cancel::T>,
    params: &Params,
    source: &geometry::T,
    collision: &geometry::T,
) -> Result<geometry::T, error::T> {
    let grain = params.grain;
    let samples = params.samples;
    let method = params.method;
    let combine = params.combine;
    let scale = params.scale;
    let lift = params.lift;
    let min_distance = params.min_distance;
    let tolerance = params.tolerance;
    if grain <= 0 {
        return Err(fail("invalid_parameter", "grain must be positive"));
    }
    if !(1..=1024).contains(&samples) {
        return Err(fail("invalid_parameter", "ray samples must be between 1 and 1024"));
    }
    if !(params.jitter_scale.is_finite() && params.jitter_scale >= 0.0) {
        return Err(fail("invalid_parameter", "ray jitter scale must be finite and non-negative"));
    }
    if method == Method::RayMinimumDistance && samples != 1 {
        return Err(fail("invalid_parameter", "multiple samples require directional projection"));
    }
    if !(scale.is_finite() && lift.is_finite()) {
        return Err(fail("invalid_parameter", "scale and lift must be finite"));
    }
    if !(min_distance.is_finite() && min_distance >= 0.0) {
        return Err(fail("invalid_distance", "minimum distance must be finite and non-negative"));
    }
    if !(tolerance.is_finite() && tolerance >= 0.0 && tolerance <= f64::MAX.sqrt()) {
        return Err(fail("invalid_distance", "ray tolerance must be finite, non-negative, and safely squarable"));
    }
    let maximum = match params.max_distance {
        None => f64::INFINITY,
        Some(value) if value.is_finite() && value >= min_distance => value,
        Some(_) => {
            return Err(fail(
                "invalid_distance",
                "maximum distance must be finite and at least the minimum distance",
            ));
        }
    };
    if method == Method::RayMinimumDistance && maximum != f64::INFINITY && maximum > f64::MAX.sqrt() {
        return Err(fail("invalid_distance", "maximum closest distance is too large to square"));
    }
    for (label, name) in [
        ("distance attribute", &params.distance_attribute),
        ("primitive attribute", &params.primitive_attribute),
        ("source vertex numbers attribute", &params.source_vertex_numbers_attribute),
        ("source vertex weights attribute", &params.source_vertex_weights_attribute),
        ("normal attribute", &params.normal_attribute),
    ] {
        validate_name(label, name)?;
    }
    if let Some(name) = &params.hit_group {
        if blank(name) {
            return Err(fail("invalid_name", "hit group name must not be empty"));
        }
    }
    if params.match_groups
        && params.point_pattern.is_none()
        && params.vertex_pattern.is_none()
        && params.primitive_pattern.is_none()
    {
        return Err(fail("invalid_pattern", "group interpolation requires a point, vertex, or primitive pattern"));
    }
    if params.source_vertex_numbers_attribute.is_some() != params.source_vertex_weights_attribute.is_some() {
        return Err(fail("invalid_name", "source vertex number and weight outputs must be requested together"));
    }
    let output_names: Vec<String> = [
        &params.distance_attribute,
        &params.primitive_attribute,
        &params.source_vertex_numbers_attribute,
        &params.source_vertex_weights_attribute,
        &params.normal_attribute,
    ]
    .into_iter()
    .filter_map(|name| name.clone())
    .collect();
    {
        let mut sorted = output_names.clone();
        sorted.sort();
        sorted.dedup();
        if sorted.len() != output_names.len() {
            return Err(fail("invalid_name", "ray output attribute names must be distinct"));
        }
    }
    let point_count = geometry::point_count(source) as usize;
    let selection = point_selection(grain, source, params.selection.as_ref(), cancel_ref)?;
    let surface = surface_index::create(cancel_ref, Some(grain), params.collision_primitives.as_ref(), None, None, collision)?;
    let mut normal_planes: Option<Planes3> = match &params.normal_attribute {
        None if lift == 0.0 => None,
        None => Some((vec![0.0; point_count], vec![0.0; point_count], vec![0.0; point_count])),
        Some(name) => Some(existing_point_float3(name, point_count, source)?),
    };
    let mut primitives = vec![-1i64; point_count];
    let mut triangles = vec![-1i64; point_count];
    let mut barycentric_a = vec![0.0f64; point_count];
    let mut barycentric_b = vec![0.0f64; point_count];
    let mut barycentric_c = vec![0.0f64; point_count];
    let mut distances = vec![f64::INFINITY; point_count];
    let mut direction_signs = vec![0i64; point_count];
    let mut hit_counts = vec![0i64; point_count];
    let directions = match method {
        Method::RayMinimumDistance => None,
        Method::RayProject => Some(direction_planes(cancel_ref, grain, source, &params.direction)?),
    };
    match method {
        Method::RayMinimumDistance => {
            let maximum_squared = if maximum == f64::INFINITY { f64::INFINITY } else { maximum * maximum };
            surface_index::closest_many_into(
                cancel_ref,
                selection.as_ref(),
                None,
                grain,
                &surface,
                geometry::positions(source),
                maximum_squared,
                &mut primitives,
                &mut triangles,
                &mut barycentric_a,
                &mut barycentric_b,
                &mut barycentric_c,
                &mut distances,
            )?;
            for point in 0..point_count {
                if primitives[point] >= 0 {
                    distances[point] = distances[point].sqrt();
                }
            }
        }
        Method::RayProject => {
            let directions = directions.as_ref().ok_or_else(|| fail("invalid_direction", "missing ray directions"))?;
            if samples == 1 {
                surface_index::View::raycast_many_into(
                    cancel_ref,
                    selection.as_ref(),
                    grain,
                    &surface,
                    geometry::positions(source),
                    directions,
                    min_distance,
                    maximum,
                    tolerance,
                    params.direction_mode,
                    params.surface_hit,
                    &mut primitives,
                    &mut triangles,
                    &mut barycentric_a,
                    &mut barycentric_b,
                    &mut barycentric_c,
                    &mut distances,
                )?;
            } else {
                let normals = normal_planes
                    .as_mut()
                    .map(|(x, y, z)| (x.as_mut_slice(), y.as_mut_slice(), z.as_mut_slice()));
                let mut outputs = SampleOutputs {
                    primitives: &mut primitives,
                    triangles: &mut triangles,
                    barycentric_a: &mut barycentric_a,
                    barycentric_b: &mut barycentric_b,
                    barycentric_c: &mut barycentric_c,
                    distances: &mut distances,
                    direction_signs: &mut direction_signs,
                    hit_counts: &mut hit_counts,
                };
                raycast_samples_into(
                    cancel_ref,
                    selection.as_ref(),
                    grain,
                    &surface,
                    geometry::positions(source),
                    directions,
                    min_distance,
                    maximum,
                    tolerance,
                    params.direction_mode,
                    params.surface_hit,
                    samples,
                    params.jitter_scale,
                    params.seed,
                    combine,
                    &mut outputs,
                    normals,
                )?;
            }
        }
    }
    let source_positions = packed::Float3::View::view(geometry::positions(source))?;
    let collision_positions = packed::Float3::View::view(geometry::positions(collision))?;
    let collision_topology = topology::View::view(geometry::topology(collision))?;
    let mut px = source_positions.x.to_vec();
    let mut py = source_positions.y.to_vec();
    let mut pz = source_positions.z.to_vec();
    let range_count = if point_count == 0 { 0 } else { point_count.div_ceil(grain as usize) };
    let mut errors = vec![-1i64; range_count];
    let mut changed = vec![false; range_count];
    for range in 0..range_count {
        let first = range * grain as usize;
        let last = point_count.min(first + grain as usize);
        let mut normal = [0.0f64; 3];
        for point in first..last {
            cancel::poll(cancel_ref, point as i64, 4095)?;
            let triangle = triangles[point];
            if triangle < 0 {
                continue;
            }
            let va = surface_index::View::triangle_vertex(&surface, triangle, 0) as usize;
            let vb = surface_index::View::triangle_vertex(&surface, triangle, 1) as usize;
            let vc = surface_index::View::triangle_vertex(&surface, triangle, 2) as usize;
            let a = collision_topology.vertex_points[va] as usize;
            let b = collision_topology.vertex_points[vb] as usize;
            let c = collision_topology.vertex_points[vc] as usize;
            let (wa, wb, wc) = (barycentric_a[point], barycentric_b[point], barycentric_c[point]);
            let m = |p: usize| {
                float_max(
                    collision_positions.x[p].abs(),
                    float_max(collision_positions.y[p].abs(), collision_positions.z[p].abs()),
                )
            };
            let coordinate_scale = float_max(m(a), float_max(m(b), m(c)));
            let coordinate_scale = if coordinate_scale == 0.0 { 1.0 } else { coordinate_scale };
            let cx_ = |p: usize| collision_positions.x[p] / coordinate_scale;
            let cy_ = |p: usize| collision_positions.y[p] / coordinate_scale;
            let cz_ = |p: usize| collision_positions.z[p] / coordinate_scale;
            let (hx, hy, hz) = if samples == 1 {
                (
                    coordinate_scale * (wa * cx_(a) + wb * cx_(b) + wc * cx_(c)),
                    coordinate_scale * (wa * cy_(a) + wb * cy_(b) + wc * cy_(c)),
                    coordinate_scale * (wa * cz_(a) + wb * cz_(b) + wc * cz_(c)),
                )
            } else {
                let directions = directions.as_ref().ok_or_else(|| fail("invalid_direction", "missing ray directions"))?;
                let (vx, vy, vz) = match directions {
                    RayDirections::ConstantDirection { x, y, z } => (*x, *y, *z),
                    RayDirections::PerQueryDirections(values) => (values.x[point], values.y[point], values.z[point]),
                };
                let direction_scale = float_max(vx.abs(), float_max(vy.abs(), vz.abs()));
                let (dx, dy, dz) = (vx / direction_scale, vy / direction_scale, vz / direction_scale);
                let inverse = 1.0 / (dx * dx + dy * dy + dz * dz).sqrt();
                let signed_distance = direction_signs[point] as f64 * distances[point] * inverse;
                (
                    source_positions.x[point] + signed_distance * dx,
                    source_positions.y[point] + signed_distance * dy,
                    source_positions.z[point] + signed_distance * dz,
                )
            };
            normal = [0.0; 3];
            if let Some((output_x, output_y, output_z)) = normal_planes.as_mut() {
                if samples > 1 {
                    normal = [output_x[point], output_y[point], output_z[point]];
                    let length = (normal[0] * normal[0] + normal[1] * normal[1] + normal[2] * normal[2]).sqrt();
                    if length > 0.0 {
                        normal[0] /= length;
                        normal[1] /= length;
                        normal[2] /= length;
                    }
                } else {
                    let (ax, ay, az) = (cx_(a), cy_(a), cz_(a));
                    let (bx, by, bz) = (cx_(b), cy_(b), cz_(b));
                    let (qx, qy, qz) = (cx_(c), cy_(c), cz_(c));
                    let (abx, aby, abz) = (bx - ax, by - ay, bz - az);
                    let (acx, acy, acz) = (qx - ax, qy - ay, qz - az);
                    normal[0] = aby * acz - abz * acy;
                    normal[1] = abz * acx - abx * acz;
                    normal[2] = abx * acy - aby * acx;
                    let inverse = 1.0 / (normal[0] * normal[0] + normal[1] * normal[1] + normal[2] * normal[2]).sqrt();
                    normal[0] *= inverse;
                    normal[1] *= inverse;
                    normal[2] *= inverse;
                    output_x[point] = normal[0];
                    output_y[point] = normal[1];
                    output_z[point] = normal[2];
                }
            }
            let x = source_positions.x[point] + scale * (hx - source_positions.x[point]) + lift * normal[0];
            let y = source_positions.y[point] + scale * (hy - source_positions.y[point]) + lift * normal[1];
            let z = source_positions.z[point] + scale * (hz - source_positions.z[point]) + lift * normal[2];
            if !(x.is_finite() && y.is_finite() && z.is_finite()) {
                errors[range] = point as i64;
            } else {
                px[point] = x;
                py[point] = y;
                pz[point] = z;
                if x != source_positions.x[point] || y != source_positions.y[point] || z != source_positions.z[point] {
                    changed[range] = true;
                }
            }
        }
        let _ = normal;
    }
    if let Some(point) = errors.iter().find(|&&point| point >= 0) {
        return Err(fail(
            "non_finite_output",
            &format!("ray transform produced a non-finite position at point {point}"),
        ));
    }
    let positions = packed::Float3::View::of_owned_exn(px, py, pz)?;
    let needs_interpolation = params.point_pattern.is_some()
        || params.vertex_pattern.is_some()
        || params.primitive_pattern.is_some()
        || params.detail_pattern.is_some()
        || params.match_groups;
    let keep_drivers = params.source_vertex_numbers_attribute.is_some();
    let need_drivers = needs_interpolation || keep_drivers;
    let mut reserved: Vec<String> = vec!["P".to_owned()];
    reserved.extend(output_names.iter().cloned());
    let numbers_name = match &params.source_vertex_numbers_attribute {
        Some(name) => name.clone(),
        None => unique_attribute_name(source, collision, &reserved, "__rays_ray_vertex_numbers"),
    };
    let weights_name = match &params.source_vertex_weights_attribute {
        Some(name) => name.clone(),
        None => {
            let mut taken = vec![numbers_name.clone()];
            taken.extend(reserved.iter().cloned());
            unique_attribute_name(source, collision, &taken, "__rays_ray_vertex_weights")
        }
    };
    // Most recent first, as the OCaml lists are consed.
    let mut attributes: Vec<attribute::T> = Vec::new();
    let mut protected_attributes: Vec<attribute::T> = Vec::new();
    let imported_point_attribute = |name: &str| -> bool {
        match &params.point_pattern {
            None => false,
            Some(pattern) => match attribute_pattern::compile(pattern) {
                Err(_) => false,
                Ok(pattern) => {
                    attribute_pattern::matches(&pattern, name)
                        && geometry::find_attribute(attribute::Owner::Point, name, collision).is_some()
                }
            },
        }
    };
    let selected_point = |point: usize| match &selection {
        None => true,
        Some(item) => group::mem(point as i64, item),
    };
    if let Some(name) = &params.distance_attribute {
        let mut values = existing_point_float(name, point_count, source, -1.0)?;
        for point in 0..point_count {
            if selected_point(point) {
                values[point] = if primitives[point] < 0 { -1.0 } else { distances[point] };
            }
        }
        let item = attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::Float(values))?;
        attributes.insert(0, item.clone());
        protected_attributes.insert(0, item);
    }
    if let Some(name) = &params.primitive_attribute {
        let mut values = existing_point_int(name, point_count, source, -1)?;
        for point in 0..point_count {
            if selected_point(point) {
                values[point] = primitives[point];
            }
        }
        let item = attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::Int(values))?;
        attributes.insert(0, item.clone());
        protected_attributes.insert(0, item);
    }
    if let (Some(name), Some((x, y, z))) = (&params.normal_attribute, &normal_planes) {
        let item = attribute::create_owned(
            name,
            attribute::Owner::Point,
            attribute::Storage::Float3(packed::Float3::View::of_owned_exn(x.clone(), y.clone(), z.clone())?),
        )?;
        attributes.insert(0, item.clone());
        if !imported_point_attribute(name) {
            protected_attributes.insert(0, item);
        }
    }
    if need_drivers {
        let average_drivers = samples > 1 && combine == Combine::RayAverage;
        let mut offsets = vec![0i64; point_count + 1];
        for point in 0..point_count {
            cancel::poll(cancel_ref, point as i64, 16_383)?;
            let hits = if average_drivers {
                hit_counts[point]
            } else if triangles[point] >= 0 {
                1
            } else {
                0
            };
            offsets[point + 1] = offsets[point] + hits * 3;
        }
        let value_count = offsets[point_count] as usize;
        let mut numbers = vec![0i64; value_count];
        let mut weights = vec![0.0f64; value_count];
        if average_drivers {
            let directions = directions.as_ref().ok_or_else(|| fail("invalid_direction", "missing ray directions"))?;
            raycast_average_drivers_into(
                cancel_ref,
                selection.as_ref(),
                grain,
                &surface,
                geometry::positions(source),
                directions,
                min_distance,
                maximum,
                tolerance,
                params.surface_hit,
                samples,
                params.jitter_scale,
                params.seed,
                &direction_signs,
                &hit_counts,
                &offsets,
                &mut numbers,
                &mut weights,
            )?;
        } else {
            for point in 0..point_count {
                let triangle = triangles[point];
                if triangle >= 0 {
                    let output = offsets[point] as usize;
                    numbers[output] = surface_index::View::triangle_vertex(&surface, triangle, 0);
                    numbers[output + 1] = surface_index::View::triangle_vertex(&surface, triangle, 1);
                    numbers[output + 2] = surface_index::View::triangle_vertex(&surface, triangle, 2);
                    weights[output] = barycentric_a[point];
                    weights[output + 1] = barycentric_b[point];
                    weights[output + 2] = barycentric_c[point];
                }
            }
        }
        let numbers_attribute = attribute::create_owned(
            &numbers_name,
            attribute::Owner::Point,
            attribute::Storage::IntArray(packed::IntArray::View::create_validated_owned(offsets.clone(), numbers)),
        )?;
        let weights_attribute = attribute::create_owned(
            &weights_name,
            attribute::Owner::Point,
            attribute::Storage::FloatArray(packed::FloatArray::View::create_validated_owned(offsets, weights)),
        )?;
        attributes.insert(0, weights_attribute.clone());
        attributes.insert(0, numbers_attribute.clone());
        if keep_drivers {
            protected_attributes.insert(0, weights_attribute);
            protected_attributes.insert(0, numbers_attribute);
        }
    }
    let hit_selection = if needs_interpolation || params.hit_group.is_some() {
        let name = params.hit_group.clone().unwrap_or_else(|| "__ray_hits".to_owned());
        Some(group::init(Some(grain), group::Owner::Point, &name, point_count as i64, &|point| {
            triangles[point as usize] >= 0
        })?)
    } else {
        None
    };
    let groups: Vec<group::T> = match (&params.hit_group, &hit_selection) {
        (Some(_), Some(item)) => vec![item.clone()],
        _ => Vec::new(),
    };
    let changed_positions = changed.iter().any(|&flag| flag);
    let target = if changed_positions {
        let t = geometry::without_attribute(attribute::Owner::Point, "N", source);
        geometry::without_attribute(attribute::Owner::Vertex, "N", &t)
    } else {
        source.clone()
    };
    attributes.reverse();
    let target = geometry::View::with_merged_owned(Some(positions.clone()), attributes, groups, &target)?;
    let output = if !needs_interpolation {
        target
    } else {
        let hit = hit_selection.clone().ok_or_else(|| fail("invalid_output", "missing hit selection"))?;
        attribute_ops::interpolate(&attribute_ops::Params {
            cancel: None,
            grain: Some(grain),
            selection: Some(hit),
            driver: Some(attribute_ops::InterpolateDriver::VertexWeights {
                numbers_attribute: numbers_name.clone(),
                weights_attribute: weights_name.clone(),
            }),
            compute_weights: None,
            point_pattern: params.point_pattern.clone(),
            vertex_pattern: params.vertex_pattern.clone(),
            primitive_pattern: params.primitive_pattern.clone(),
            detail_pattern: params.detail_pattern.clone(),
            match_groups: params.match_groups,
            pre_scale: None,
            normalize_weights: None,
            threshold: None,
            blend: None,
            unmatched: Unmatched::KeepTarget,
            target_owner: attribute::Owner::Point,
            attributes: Vec::new(),
            source: collision.clone(),
            target,
        })?
    };
    let output = if need_drivers && !keep_drivers {
        let t = geometry::without_attribute(attribute::Owner::Point, &numbers_name, &output);
        geometry::without_attribute(attribute::Owner::Point, &weights_name, &t)
    } else {
        output
    };
    let mut output = geometry::with_positions(positions, &output)?;
    for item in protected_attributes {
        output = geometry::with_attribute(item, &output)?;
    }
    if let (Some(_), Some(item)) = (&params.hit_group, &hit_selection) {
        output = geometry::with_group(item.clone(), &output)?;
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plane_clip::test_util::*;
    use rdk_core::topology::PrimitiveKind;

    fn vec3(x: f64, y: f64, z: f64) -> rays_math::vec3::T {
        rays_math::vec3::create(x, y, z)
    }

    /// Two triangles over x, z in [-size/2, size/2] at height y, facing +y.
    fn plane(size: f64, y: f64) -> geometry::T {
        let h = size / 2.0;
        polygons(
            vec![-h, h, -h, h],
            vec![y; 4],
            vec![-h, -h, h, h],
            &[vec![0, 2, 3], vec![0, 3, 1]],
        )
    }

    fn source_points(values: &[(f64, f64, f64)]) -> geometry::T {
        geom(
            values.iter().map(|v| v.0).collect(),
            values.iter().map(|v| v.1).collect(),
            values.iter().map(|v| v.2).collect(),
            &[],
        )
    }

    fn base() -> Params {
        Params {
            grain: 16_384,
            selection: None,
            collision_primitives: None,
            method: Method::RayProject,
            direction: Direction::RayNormal,
            direction_mode: RayDirectionMode::RayForward,
            surface_hit: RaySurfaceHit::RayFirstSurface,
            samples: 1,
            jitter_scale: 1.0,
            seed: 0,
            combine: Combine::RayAverage,
            min_distance: 0.0,
            max_distance: None,
            tolerance: 0.0,
            scale: 1.0,
            lift: 0.0,
            distance_attribute: None,
            primitive_attribute: None,
            source_vertex_numbers_attribute: None,
            source_vertex_weights_attribute: None,
            hit_group: None,
            normal_attribute: None,
            point_pattern: None,
            vertex_pattern: None,
            primitive_pattern: None,
            detail_pattern: None,
            match_groups: false,
        }
    }

    fn ray(p: &Params, source: &geometry::T, collision: &geometry::T) -> Result<geometry::T, error::T> {
        run(None, p, source, collision)
    }

    fn down() -> Direction {
        Direction::RayVector(vec3(0.0, -1.0, 0.0))
    }

    fn up() -> Direction {
        Direction::RayVector(vec3(0.0, 1.0, 0.0))
    }

    fn point_float(source: &geometry::T, name: &str) -> Vec<f64> {
        match attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, name, source).unwrap()).unwrap() {
            attribute::Storage::Float(values) => values.clone(),
            _ => panic!("{name} has unexpected storage"),
        }
    }

    fn point_float3(source: &geometry::T, name: &str) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
        match attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, name, source).unwrap()).unwrap() {
            attribute::Storage::Float3(values) => {
                let v = packed::Float3::View::view(values).unwrap();
                (v.x.to_vec(), v.y.to_vec(), v.z.to_vec())
            }
            _ => panic!("{name} has unexpected storage"),
        }
    }

    fn near(a: f64, b: f64) -> bool {
        (a - b).abs() <= 1e-10
    }

    #[test]
    fn basic_projection() {
        let source = source_points(&[(-1.0, 2.0, -1.0), (0.5, 3.0, 0.25), (4.0, 2.0, 0.0)]);
        let collision = plane(4.0, 0.0);
        let mut p = base();
        p.direction = Direction::RayVector(vec3(0.0, -5.0, 0.0));
        p.distance_attribute = Some("dist".to_owned());
        p.primitive_attribute = Some("hit_prim".to_owned());
        p.source_vertex_numbers_attribute = Some("hit_vertices".to_owned());
        p.source_vertex_weights_attribute = Some("hit_weights".to_owned());
        p.hit_group = Some("hits".to_owned());
        p.normal_attribute = Some("hit_N".to_owned());
        let projected = ray(&p, &source, &collision).unwrap();
        let (_, py, pz) = xyz(&projected);
        let (px, _, _) = xyz(&projected);
        let distances = point_float(&projected, "dist");
        let primitives = int_attribute(attribute::Owner::Point, "hit_prim", &projected);
        let normals = point_float3(&projected, "hit_N");
        let (numbers, weights) = match (
            attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, "hit_vertices", &projected).unwrap()).unwrap(),
            attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, "hit_weights", &projected).unwrap()).unwrap(),
        ) {
            (attribute::Storage::IntArray(n), attribute::Storage::FloatArray(w)) => {
                (packed::IntArray::View::view(n), packed::FloatArray::View::view(w))
            }
            _ => panic!("provenance storage"),
        };
        assert!(near(py[0], 0.0) && near(py[1], 0.0) && near(py[2], 2.0), "Ray basic projection positions");
        assert!(near(distances[0], 2.0) && near(distances[1], 3.0) && distances[2] == -1.0);
        assert!(primitives[0] >= 0 && primitives[1] >= 0 && primitives[2] == -1);
        assert!(numbers.offsets.len() == 4 && numbers.offsets == weights.offsets && numbers.offsets[3] == 6);
        let (cx, cy, cz) = xyz(&collision);
        let collision_topology = topo(&collision);
        for point in 0..2usize {
            let first = numbers.offsets[point] as usize;
            let total = weights.values[first] + weights.values[first + 1] + weights.values[first + 2];
            let reconstruct = |coordinate: &[f64]| -> f64 {
                (0..3)
                    .map(|local| {
                        let vertex = numbers.values[first + local] as usize;
                        let source_point = collision_topology.vertex_points[vertex] as usize;
                        weights.values[first + local] * coordinate[source_point]
                    })
                    .sum()
            };
            assert!(near(total, 1.0) && near(normals.1[point].abs(), 1.0), "barycentric/normal output");
            assert!(
                near(reconstruct(&cx), px[point]) && near(reconstruct(&cy), py[point]) && near(reconstruct(&cz), pz[point]),
                "exact source-vertex provenance reconstruction"
            );
        }
        let hits = geometry::find_group(group::Owner::Point, "hits", &projected).unwrap();
        assert!(group::cardinality(&hits) == 2 && group::mem(0, &hits) && group::mem(1, &hits) && !group::mem(2, &hits));
        let mut p = base();
        p.method = Method::RayMinimumDistance;
        p.scale = 0.5;
        p.lift = 0.1;
        p.normal_attribute = Some("Nhit".to_owned());
        let scaled = ray(&p, &source, &collision).unwrap();
        let (_, sy, _) = xyz(&scaled);
        assert!(near(sy[0], 1.1) && near(sy[1], 1.6), "scale/lift transform");
    }

    #[test]
    fn direction_policies() {
        let source = source_points(&[(0.0, 0.0, 0.0)]);
        let collision = merge(&[plane(4.0, 1.0), plane(4.0, 3.0), plane(4.0, -2.0)]);
        let project = |mode: RayDirectionMode, surface: RaySurfaceHit| -> f64 {
            let mut p = base();
            p.direction = up();
            p.direction_mode = mode;
            p.surface_hit = surface;
            xyz(&ray(&p, &source, &collision).unwrap()).1[0]
        };
        assert!(near(project(RayDirectionMode::RayForward, RaySurfaceHit::RayFirstSurface), 1.0));
        assert!(near(project(RayDirectionMode::RayForward, RaySurfaceHit::RayLastSurface), 3.0));
        assert!(near(project(RayDirectionMode::RayReverse, RaySurfaceHit::RayFirstSurface), -2.0));
        assert!(near(project(RayDirectionMode::RayBidirectionalClosest, RaySurfaceHit::RayFirstSurface), 1.0));
        assert!(near(project(RayDirectionMode::RayBidirectionalFarthest, RaySurfaceHit::RayFirstSurface), -2.0));
        let mut p = base();
        p.direction = up();
        p.min_distance = 1.5;
        p.max_distance = Some(2.5);
        assert_eq!(xyz(&ray(&p, &source, &collision).unwrap()).1[0], 0.0, "distance bounds did not reject hits");
        let near_edge = source_points(&[(2.0001, 1.0, 0.0)]);
        let mut p = base();
        p.direction = down();
        let missed = ray(&p, &near_edge, &plane(4.0, 0.0)).unwrap();
        p.tolerance = 0.001;
        let tolerated = ray(&p, &near_edge, &plane(4.0, 0.0)).unwrap();
        assert!(xyz(&missed).1[0] == 1.0 && near(xyz(&tolerated).1[0], 0.0), "world-space edge tolerance");
        let upper = group::init(None, group::Owner::Primitive, "upper", geometry::primitive_count(&collision), &|p| {
            (2..4).contains(&p)
        })
        .unwrap();
        let mut p = base();
        p.collision_primitives = Some(upper);
        p.direction = up();
        assert!(near(xyz(&ray(&p, &source, &collision).unwrap()).1[0], 3.0), "collision restriction");
    }

    #[test]
    fn multi_samples() {
        let source = source_points(&[(0.0, 2.0, 0.0)]);
        let collision = plane(20.0, 0.0);
        let project = |combine: Combine, jitter: f64, seed: i64| -> geometry::T {
            let mut p = base();
            p.samples = 7;
            p.jitter_scale = jitter;
            p.seed = seed;
            p.combine = combine;
            p.direction = down();
            p.distance_attribute = Some("dist".to_owned());
            p.normal_attribute = Some("hit_N".to_owned());
            ray(&p, &source, &collision).unwrap()
        };
        let shortest = project(Combine::RayShortest, 0.35, 71);
        let longest = project(Combine::RayLongest, 0.35, 71);
        let average = project(Combine::RayAverage, 0.35, 71);
        let median = project(Combine::RayMedian, 0.35, 71);
        let (sd, ld, ad, md) = (
            point_float(&shortest, "dist")[0],
            point_float(&longest, "dist")[0],
            point_float(&average, "dist")[0],
            point_float(&median, "dist")[0],
        );
        assert!(near(sd, 2.0), "multi-sample shortest did not retain the unjittered ray");
        assert!(ld > ad && ad > 2.0 && md >= 2.0 && md <= ld, "multi-sample combine ordering");
        assert!(near(xyz(&average).1[0], 2.0 - ad), "average did not move along the unjittered direction");
        assert!(near(point_float3(&average, "hit_N").1[0].abs(), 1.0), "averaged hit normal");
        assert!(same_geometry(&average, &project(Combine::RayAverage, 0.35, 71)), "seeded jitter is not deterministic");
        let (cx, _, _) = xyz(&collision);
        let signal = attribute::create_owned("signal", attribute::Owner::Point, attribute::Storage::Float(cx.clone())).unwrap();
        let collision_with_signal = geometry::with_attribute(signal, &collision).unwrap();
        let mut p = base();
        p.samples = 4;
        p.jitter_scale = 0.0;
        p.seed = 9;
        p.combine = Combine::RayAverage;
        p.direction = down();
        p.source_vertex_numbers_attribute = Some("vertices".to_owned());
        p.source_vertex_weights_attribute = Some("weights".to_owned());
        p.point_pattern = Some("signal".to_owned());
        let drivers = ray(&p, &source, &collision_with_signal).unwrap();
        let (numbers, weights) = match (
            attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, "vertices", &drivers).unwrap()).unwrap(),
            attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, "weights", &drivers).unwrap()).unwrap(),
        ) {
            (attribute::Storage::IntArray(n), attribute::Storage::FloatArray(w)) => {
                (packed::IntArray::View::view(n), packed::FloatArray::View::view(w))
            }
            _ => panic!("provenance storage"),
        };
        assert!(numbers.offsets.to_vec() == vec![0, 12] && weights.offsets == numbers.offsets, "average provenance");
        assert!(near(weights.values.iter().sum::<f64>(), 1.0), "average provenance weights do not sum to one");
        let collision_topology = topo(&collision_with_signal);
        let mut reconstructed = 0.0;
        for at in numbers.offsets[0]..numbers.offsets[1] {
            let point = collision_topology.vertex_points[numbers.values[at as usize] as usize] as usize;
            reconstructed += weights.values[at as usize] * cx[point];
        }
        assert!(near(reconstructed, point_float(&drivers, "signal")[0]), "average attribute import ignored sample drivers");
        let mut p = base();
        p.samples = 4;
        p.jitter_scale = 0.0;
        p.seed = 9;
        p.combine = Combine::RayMedian;
        p.direction = down();
        p.source_vertex_numbers_attribute = Some("vertices".to_owned());
        p.source_vertex_weights_attribute = Some("weights".to_owned());
        let selected = ray(&p, &source, &collision).unwrap();
        match attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, "vertices", &selected).unwrap()).unwrap() {
            attribute::Storage::IntArray(n) => {
                assert_eq!(packed::IntArray::View::view(n).offsets.to_vec(), vec![0, 3]);
            }
            _ => panic!("storage"),
        }
        for combine in [Combine::RayAverage, Combine::RayMedian, Combine::RayShortest, Combine::RayLongest] {
            assert!(near(point_float(&project(combine, 0.0, 11), "dist")[0], 2.0), "zero-jitter combiner changed an identical hit");
        }
    }

    #[test]
    fn selection_and_directions() {
        let source = source_points(&[(-1.0, 2.0, 0.0), (0.0, 2.0, 0.0), (1.0, 2.0, 0.0)]);
        let selection = group::init(None, group::Owner::Point, "middle", 3, &|p| p == 1).unwrap();
        let mut p = base();
        p.selection = Some(element_selection::T::SelectedPoints(selection));
        p.direction = down();
        assert_eq!(xyz(&ray(&p, &source, &plane(4.0, 0.0)).unwrap()).1, vec![2.0, 0.0, 2.0], "point restriction");
        let directions = packed::Float3::View::of_owned_exn(vec![0.0; 3], vec![-1.0, -2.0, -3.0], vec![0.0; 3]).unwrap();
        let direction_attribute =
            attribute::create_owned("ray_dir", attribute::Owner::Point, attribute::Storage::Float3(directions)).unwrap();
        let with_direction = geometry::with_attribute(direction_attribute, &source).unwrap();
        let mut p = base();
        p.direction = Direction::RayAttribute("ray_dir".to_owned());
        assert_eq!(xyz(&ray(&p, &with_direction, &plane(4.0, 0.0)).unwrap()).1, vec![0.0, 0.0, 0.0], "attribute directions");
        // A 3 x 3 point triangle grid facing +y, one unit across, two above the plane.
        let (mut x, mut z) = (Vec::new(), Vec::new());
        for r in 0..3 {
            for c in 0..3 {
                x.push(c as f64 * 0.5 - 0.5);
                z.push(r as f64 * 0.5 - 0.5);
            }
        }
        let mut polys = Vec::new();
        for r in 0..2i64 {
            for c in 0..2i64 {
                let q = r * 3 + c;
                polys.push(vec![q, q + 3, q + 4]);
                polys.push(vec![q, q + 4, q + 1]);
            }
        }
        let normal_source = polygons(x, vec![2.0; 9], z, &polys);
        let mut p = base();
        p.direction = Direction::RayNormal;
        p.direction_mode = RayDirectionMode::RayReverse;
        let projected = ray(&p, &normal_source, &plane(4.0, 0.0)).unwrap();
        assert!(xyz(&projected).1.iter().all(|&y| near(y, 0.0)), "authored/computed normal direction");
    }

    #[test]
    fn imports() {
        let collision = plane(4.0, 0.0);
        let (points, vertices, primitives) = (
            geometry::point_count(&collision),
            geometry::vertex_count(&collision),
            geometry::primitive_count(&collision),
        );
        let add = |name: &str, owner: attribute::Owner, storage: attribute::Storage, g: &geometry::T| {
            geometry::with_attribute(attribute::create_owned(name, owner, storage).unwrap(), g).unwrap()
        };
        let collision = add(
            "temperature",
            attribute::Owner::Point,
            attribute::Storage::Float((0..points).map(|p| p as f64).collect()),
            &collision,
        );
        let collision = add(
            "corner",
            attribute::Owner::Vertex,
            attribute::Storage::Float((0..vertices).map(|v| 10.0 + v as f64).collect()),
            &collision,
        );
        let collision = add(
            "material",
            attribute::Owner::Primitive,
            attribute::Storage::Text((0..primitives).map(|p| format!("m{p}")).collect()),
            &collision,
        );
        let collision = add("revision", attribute::Owner::Detail, attribute::Storage::Int(vec![7]), &collision);
        let collision = geometry::with_group(
            group::init(None, group::Owner::Primitive, "marked", primitives, &|_| true).unwrap(),
            &collision,
        )
        .unwrap();
        let source = source_points(&[(0.5, 2.0, 0.25), (5.0, 2.0, 0.0)]);
        let mut p = base();
        p.direction = down();
        p.point_pattern = Some("temperature".to_owned());
        p.vertex_pattern = Some("corner".to_owned());
        p.primitive_pattern = Some("material marked".to_owned());
        p.detail_pattern = Some("revision".to_owned());
        p.match_groups = true;
        let output = ray(&p, &source, &collision).unwrap();
        // The hit (0.5, 0, 0.25) lies in the second triangle, points 0, 3, 1 and vertices 3, 4, 5,
        // at barycentric weights 0.375, 0.5625, 0.0625.
        assert!(
            near(point_float(&output, "temperature")[0], 3.0 * 0.5625 + 0.0625)
                && near(point_float(&output, "corner")[0], 13.0 * 0.375 + 14.0 * 0.5625 + 15.0 * 0.0625),
            "did not interpolate imported numeric fields at the hit"
        );
        match attribute::View::storage(&geometry::find_attribute(attribute::Owner::Point, "material", &output).unwrap()).unwrap() {
            attribute::Storage::Text(values) => assert_eq!(values[0], "m1"),
            _ => panic!("material output storage"),
        }
        assert_eq!(int_attribute(attribute::Owner::Point, "revision", &output)[0], 7);
        let marked = geometry::find_group(group::Owner::Point, "marked", &output).expect("imported group missing");
        assert!(group::mem(0, &marked));
        let normals = packed::Float3::View::of_owned_exn(vec![1.0; points as usize], vec![0.0; points as usize], vec![0.0; points as usize]).unwrap();
        let collision = add("N", attribute::Owner::Point, attribute::Storage::Float3(normals), &collision);
        let mut p = base();
        p.direction = down();
        p.normal_attribute = Some("N".to_owned());
        p.point_pattern = Some("N".to_owned());
        let imported = ray(&p, &source, &collision).unwrap();
        let (nx, ny, _) = point_float3(&imported, "N");
        assert!(near(nx[0], 1.0) && near(ny[0], 0.0), "geometric normal overrode explicitly imported collision N");
    }

    #[test]
    fn validation() {
        let source = source_points(&[(0.0, 2.0, 0.0)]);
        let collision = plane(4.0, 0.0);
        let check = |p: Params, code: &str| expect_code(ray(&p, &source, &collision), code);
        let mut p = base();
        p.grain = 0;
        check(p, "invalid_parameter");
        let mut p = base();
        p.max_distance = Some(-1.0);
        check(p, "invalid_distance");
        let mut p = base();
        p.direction = Direction::RayVector(vec3(0.0, 0.0, 0.0));
        check(p, "invalid_direction");
        let mut p = base();
        p.direction = Direction::RayAttribute("missing".to_owned());
        check(p, "invalid_direction");
        let mut p = base();
        p.distance_attribute = Some("P".to_owned());
        check(p, "invalid_name");
        let mut p = base();
        p.distance_attribute = Some("same".to_owned());
        p.primitive_attribute = Some("same".to_owned());
        check(p, "invalid_name");
        let mut p = base();
        p.source_vertex_numbers_attribute = Some("vertices".to_owned());
        check(p, "invalid_name");
        let mut p = base();
        p.match_groups = true;
        check(p, "invalid_pattern");
        let mut p = base();
        p.samples = 0;
        check(p, "invalid_parameter");
        let mut p = base();
        p.samples = 1025;
        check(p, "invalid_parameter");
        let mut p = base();
        p.jitter_scale = -1.0;
        check(p, "invalid_parameter");
        let mut p = base();
        p.samples = 2;
        p.jitter_scale = f64::MAX;
        p.direction = down();
        let extreme = ray(&p, &source, &collision).unwrap();
        let (x, y, z) = xyz(&extreme);
        assert!(x.iter().chain(y.iter()).chain(z.iter()).all(|v| v.is_finite()), "extreme jitter produced non-finite positions");
        let mut p = base();
        p.method = Method::RayMinimumDistance;
        p.samples = 2;
        check(p, "invalid_parameter");
        let wrong = group::init(None, group::Owner::Point, "wrong", geometry::point_count(&collision), &|_| true).unwrap();
        let mut p = base();
        p.collision_primitives = Some(wrong);
        check(p, "invalid_group");
        let curve = geom(vec![-1.0, 1.0], vec![0.0; 2], vec![0.0; 2], &[(PrimitiveKind::OpenPolyline, vec![0, 1])]);
        expect_code(ray(&base(), &source, &curve), "invalid_surface");
        let bad = packed::Float3::View::of_owned_exn(vec![0.0], vec![f64::NAN], vec![0.0]).unwrap();
        let bad_source = geometry::with_attribute(
            attribute::create_owned("bad", attribute::Owner::Point, attribute::Storage::Float3(bad)).unwrap(),
            &source,
        )
        .unwrap();
        let mut p = base();
        p.direction = Direction::RayAttribute("bad".to_owned());
        expect_code(ray(&p, &bad_source, &collision), "invalid_direction");
        let token = cancel::create();
        cancel::cancel(&token);
        expect_code(run(Some(&token), &base(), &source, &collision), "cancelled");
    }
}
