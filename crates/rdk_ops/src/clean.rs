use std::sync::Arc;

use rays_math::hash;
use crate::subdivide_refine::{ABytes, AInts};
use rdk_core::{attribute_pattern, cancel, error, geometry, group, packed, support, topology};
use rdk_geom::point_grid::float_max;

use crate::attribute_lifecycle;
use crate::compact_points;
use crate::deletion;
use crate::fuse_grid;
use crate::group_ops;
use crate::reverse_faces;

const OCAML_MAX_INT: i64 = (1i64 << 62) - 1;
const MAX_ARRAY_LENGTH: i64 = (1i64 << 54) - 1;

fn make(operation: &str, code: &str, message: String) -> error::T {
    error::make(operation.to_owned(), code.to_owned(), message, None).unwrap_or_else(|e| e)
}

fn message_error(message: &str) -> error::T {
    make("Rdk_mesh.Clean", "invalid_parameter", message.to_owned())
}

fn clean_length(dx: f64, dy: f64, dz: f64) -> f64 {
    let scale = float_max(dx.abs(), float_max(dy.abs(), dz.abs()));
    if scale == 0.0 {
        0.0
    } else if !scale.is_finite() {
        f64::INFINITY
    } else {
        let (x, y, z) = (dx / scale, dy / scale, dz / scale);
        scale * ((x * x) + (y * y) + (z * z)).sqrt()
    }
}

fn clean_point_distance(positions: &packed::Float3::View::View, a: usize, b: usize) -> f64 {
    let scale = float_max(
        float_max(positions.x[a].abs(), positions.x[b].abs()),
        float_max(
            float_max(positions.y[a].abs(), positions.y[b].abs()),
            float_max(positions.z[a].abs(), positions.z[b].abs()),
        ),
    );
    if scale == 0.0 {
        0.0
    } else if !scale.is_finite() {
        f64::INFINITY
    } else {
        clean_length(
            (positions.x[b] / scale) - (positions.x[a] / scale),
            (positions.y[b] / scale) - (positions.y[a] / scale),
            (positions.z[b] / scale) - (positions.z[a] / scale),
        ) * scale
    }
}

fn clean_triangle_is_degenerate(
    epsilon: f64,
    positions: &packed::Float3::View::View,
    a: usize,
    b: usize,
    c: usize,
) -> bool {
    let (ax, ay, az) = (positions.x[a], positions.y[a], positions.z[a]);
    let (bx, by, bz) = (positions.x[b], positions.y[b], positions.z[b]);
    let (cx, cy, cz) = (positions.x[c], positions.y[c], positions.z[c]);
    let all = [ax, ay, az, bx, by, bz, cx, cy, cz];
    if !all.iter().all(|v| v.is_finite()) {
        return true;
    }
    let (ux, uy, uz) = (bx - ax, by - ay, bz - az);
    let (vx, vy, vz) = (cx - ax, cy - ay, cz - az);
    let nx = (uy * vz) - (uz * vy);
    let ny = (uz * vx) - (ux * vz);
    let nz = (ux * vy) - (uy * vx);
    if nx.is_finite() && ny.is_finite() && nz.is_finite() {
        0.5 * clean_length(nx, ny, nz) <= epsilon * epsilon
    } else {
        let scale = float_max(
            float_max(float_max(ax.abs(), ay.abs()), float_max(az.abs(), bx.abs())),
            float_max(
                float_max(by.abs(), bz.abs()),
                float_max(cx.abs(), float_max(cy.abs(), cz.abs())),
            ),
        );
        if scale == 0.0 {
            true
        } else {
            let (ax, ay, az) = (ax / scale, ay / scale, az / scale);
            let (ux, uy, uz) = ((bx / scale) - ax, (by / scale) - ay, (bz / scale) - az);
            let (vx, vy, vz) = ((cx / scale) - ax, (cy / scale) - ay, (cz / scale) - az);
            let nx = (uy * vz) - (uz * vy);
            let ny = (uz * vx) - (ux * vz);
            let nz = (ux * vy) - (uy * vx);
            let ratio = epsilon / scale;
            0.5 * clean_length(nx, ny, nz) <= ratio * ratio
        }
    }
}

pub struct DeleteDegenerateParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub primitives: Option<group::T>,
    pub epsilon: f64,
}

fn delete_flagged(
    cancel_value: &Option<Arc<cancel::T>>,
    grain: i64,
    owner: group::Owner,
    name: &str,
    count: i64,
    flags: &[u8],
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let selection = group::init(Some(grain), owner, name, count, &|index| flags[index as usize] != 0)?;
    deletion::delete(
        &deletion::DeleteParams {
            cancel: cancel_value.clone(),
            grain: Some(grain),
            selected: true,
            compact_points: false,
        },
        &selection,
        geometry_value,
    )
}

pub fn delete_degenerate(
    params: &DeleteDegenerateParams,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let epsilon = params.epsilon;
    let primitive_count = geometry::primitive_count(geometry_value);
    if let Some(selection) = &params.primitives {
        if group::owner(selection) != group::Owner::Primitive || group::length(selection) != primitive_count {
            return Err(message_error("Facet selection must be a matching primitive group"));
        }
    }
    if primitive_count == 0 {
        return Ok(geometry_value.clone());
    }
    let selected = |primitive: usize| match &params.primitives {
        None => true,
        Some(selection) => group::mem(primitive as i64, selection),
    };
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let topology_value = geometry::topology(geometry_value);
    let view = topology::View::view(topology_value)?;
    let flags = ABytes::new(primitive_count, 0);
    if topology::all_triangles(topology_value)? {
        support::parallel_for(cancel_value, params.grain, primitive_count, &|primitive| {
            let primitive = primitive as usize;
            let first = view.primitive_offsets[primitive] as usize;
            if selected(primitive)
                && clean_triangle_is_degenerate(
                    epsilon,
                    &positions,
                    view.vertex_points[first] as usize,
                    view.vertex_points[first + 1] as usize,
                    view.vertex_points[first + 2] as usize,
                )
            {
                flags.set(primitive as i64, 1);
            }
            Ok(())
        })?;
    } else {
        support::parallel_for(cancel_value, params.grain, primitive_count, &|primitive| {
            let primitive = primitive as usize;
            let first = view.primitive_offsets[primitive];
            let last = view.primitive_offsets[primitive + 1];
            let kind = view.primitive_kinds[primitive];
            if !selected(primitive) {
                return Ok(());
            }
            if kind == 0 {
                let (mut sb, mut sc, mut sd) = (0.0f64, 0.0f64, 0.0f64);
                let mut flag = 0u8;
                let origin = view.vertex_points[first as usize] as usize;
                let (ox, oy, oz) = (positions.x[origin], positions.y[origin], positions.z[origin]);
                for vertex in first..last {
                    let next = if vertex + 1 < last { vertex + 1 } else { first };
                    let a = view.vertex_points[vertex as usize] as usize;
                    let b = view.vertex_points[next as usize] as usize;
                    let (ax, ay, az) = (positions.x[a] - ox, positions.y[a] - oy, positions.z[a] - oz);
                    let (bx, by, bz) = (positions.x[b] - ox, positions.y[b] - oy, positions.z[b] - oz);
                    if !(positions.x[a].is_finite() && positions.y[a].is_finite() && positions.z[a].is_finite()) {
                        flag = 1;
                    } else if flag != 1 {
                        let nx = sb + ((ay * bz) - (az * by));
                        let ny = sc + ((az * bx) - (ax * bz));
                        let nz = sd + ((ax * by) - (ay * bx));
                        sb = nx;
                        sc = ny;
                        sd = nz;
                        if !(nx.is_finite() && ny.is_finite() && nz.is_finite()) {
                            flag = 2;
                        }
                    }
                }
                if flag == 0 {
                    let area = 0.5 * clean_length(sb, sc, sd);
                    if area <= epsilon * epsilon {
                        flag = 1;
                    }
                } else if flag == 2 {
                    flag = 0;
                    sb = 0.0;
                    sc = 0.0;
                    sd = 0.0;
                    let mut scale = 0.0f64;
                    for vertex in first..last {
                        let point = view.vertex_points[vertex as usize] as usize;
                        scale = float_max(scale, positions.x[point].abs());
                        scale = float_max(scale, positions.y[point].abs());
                        scale = float_max(scale, positions.z[point].abs());
                    }
                    let (ox, oy, oz) = (positions.x[origin] / scale, positions.y[origin] / scale, positions.z[origin] / scale);
                    for vertex in first..last {
                        let next = if vertex + 1 < last { vertex + 1 } else { first };
                        let a = view.vertex_points[vertex as usize] as usize;
                        let b = view.vertex_points[next as usize] as usize;
                        let (ax, ay, az) = ((positions.x[a] / scale) - ox, (positions.y[a] / scale) - oy, (positions.z[a] / scale) - oz);
                        let (bx, by, bz) = ((positions.x[b] / scale) - ox, (positions.y[b] / scale) - oy, (positions.z[b] / scale) - oz);
                        sb += (ay * bz) - (az * by);
                        sc += (az * bx) - (ax * bz);
                        sd += (ax * by) - (ay * bx);
                    }
                    let area = 0.5 * clean_length(sb, sc, sd);
                    let ratio = epsilon / scale;
                    if area <= ratio * ratio {
                        flag = 1;
                    }
                }
                flags.set(primitive as i64, flag);
            } else {
                let size = last - first;
                let edges = size - 1 + if kind == 1 { 0 } else { 1 };
                let mut length = 0.0f64;
                for edge in 0..edges {
                    let a = view.vertex_points[(first + (edge % size)) as usize] as usize;
                    let b = view.vertex_points[(first + ((edge + 1) % size)) as usize] as usize;
                    length += clean_point_distance(&positions, a, b);
                }
                if !length.is_finite() || length <= epsilon {
                    flags.set(primitive as i64, 1);
                }
            }
            Ok(())
        })?;
    }
    let flags = flags.into_vec();
    if flags.iter().all(|&flag| flag == 0) {
        return Ok(geometry_value.clone());
    }
    delete_flagged(
        &params.cancel,
        params.grain,
        group::Owner::Primitive,
        "__clean_degenerate",
        primitive_count,
        &flags,
        geometry_value,
    )
}

fn delete_nan_points(
    cancel_value: &Option<Arc<cancel::T>>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let point_count = geometry::point_count(geometry_value);
    if point_count == 0 {
        return Ok(geometry_value.clone());
    }
    let flags = ABytes::new(point_count, 0);
    support::parallel_for(cancel_value.as_deref(), grain, point_count, &|point| {
        let point = point as usize;
        if positions.x[point].is_nan() || positions.y[point].is_nan() || positions.z[point].is_nan() {
            flags.set(point as i64, 1);
        }
        Ok(())
    })?;
    let flags = flags.into_vec();
    if flags.iter().all(|&flag| flag == 0) {
        return Ok(geometry_value.clone());
    }
    delete_flagged(cancel_value, grain, group::Owner::Point, "__clean_nan_points", point_count, &flags, geometry_value)
}

fn clean_cycle_point(view: &topology::View::View, first: i64, size: i64, start: i64, direction: i64, offset: i64) -> i64 {
    let local = (start + direction * offset) % size;
    view.vertex_points[(first + if local < 0 { local + size } else { local }) as usize]
}

fn clean_minimal_rotation(view: &topology::View::View, first: i64, size: i64, direction: i64) -> i64 {
    let mut left = 0i64;
    let mut right = 1i64;
    let mut offset = 0i64;
    while left < size && right < size && offset < size {
        let a = clean_cycle_point(view, first, size, left, direction, offset);
        let b = clean_cycle_point(view, first, size, right, direction, offset);
        if a == b {
            offset += 1;
        } else {
            if a > b {
                left += offset + 1;
                if left == right {
                    left += 1;
                }
            } else {
                right += offset + 1;
                if right == left {
                    right += 1;
                }
            }
            offset = 0;
        }
    }
    left.min(right) % size
}

fn table_capacity(count: i64) -> usize {
    let mut capacity = 8usize;
    while (capacity as i64) < count * 2 {
        capacity <<= 1;
    }
    capacity
}

fn delete_overlaps_general(
    cancel_value: &Option<Arc<cancel::T>>,
    grain: i64,
    delete_pairs: bool,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let view = topology::View::view(geometry::topology(geometry_value))?;
    let primitive_count = geometry::primitive_count(geometry_value) as usize;
    let sizes = AInts::new(primitive_count as i64, 0);
    let starts = AInts::new(primitive_count as i64, 0);
    let directions = AInts::new(primitive_count as i64, 1);
    let hashes = AInts::new(primitive_count as i64, 0);
    let polygon_count = (0..primitive_count).filter(|&p| view.primitive_kinds[p] == 0).count() as i64;
    if polygon_count == 0 {
        return Ok(geometry_value.clone());
    }
    if polygon_count > MAX_ARRAY_LENGTH / 4 {
        return Err(message_error("Rdk_mesh.Clean.clean: overlap table exceeds array limits"));
    }
    support::parallel_for(cancel_value.as_deref(), grain, primitive_count as i64, &|primitive| {
        let primitive = primitive as usize;
        if view.primitive_kinds[primitive] != 0 {
            return Ok(());
        }
        let first = view.primitive_offsets[primitive];
        let size = view.primitive_offsets[primitive + 1] - first;
        let mut start = clean_minimal_rotation(&view, first, size, 1);
        let mut direction = 1i64;
        let reverse_start = clean_minimal_rotation(&view, first, size, -1);
        let mut offset = 0i64;
        let mut comparison = 0i64;
        while offset < size && comparison == 0 {
            let a = clean_cycle_point(&view, first, size, start, 1, offset);
            let b = clean_cycle_point(&view, first, size, reverse_start, -1, offset);
            comparison = if a < b { -1 } else if a > b { 1 } else { 0 };
            offset += 1;
        }
        if comparison > 0 {
            start = reverse_start;
            direction = -1;
        }
        let mut hash_value = 17 ^ size;
        for offset in 0..size {
            hash_value = hash::mix(
                hash_value,
                clean_cycle_point(&view, first, size, start, direction, offset),
            ) & OCAML_MAX_INT;
        }
        starts.set(primitive as i64, start);
        directions.set(primitive as i64, direction);
        hashes.set(primitive as i64, hash_value);
        sizes.set(primitive as i64, size);
        Ok(())
    })?;
    let (sizes, starts, directions, hashes) = (sizes.into_vec(), starts.into_vec(), directions.into_vec(), hashes.into_vec());
    let capacity = table_capacity(polygon_count);
    let mut table = vec![-1i64; capacity];
    let mask = (capacity - 1) as i64;
    let mut flags = vec![0u8; primitive_count];
    let point = |primitive: usize, offset: i64| -> i64 {
        let first = view.primitive_offsets[primitive];
        let size = sizes[primitive];
        let local = (starts[primitive] + (directions[primitive] * offset)) % size;
        view.vertex_points[(first + if local < 0 { local + size } else { local }) as usize]
    };
    let equal = |left: usize, right: usize| -> bool {
        sizes[left] == sizes[right] && (0..sizes[left]).all(|offset| point(left, offset) == point(right, offset))
    };
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value.as_deref(), primitive as i64, 4_095)?;
        if sizes[primitive] > 0 {
            let mut slot = hashes[primitive] & mask;
            let mut found = -1i64;
            loop {
                let candidate = table[slot as usize];
                if candidate < 0 {
                    break;
                }
                if hashes[candidate as usize] == hashes[primitive] && equal(candidate as usize, primitive) {
                    found = candidate;
                    break;
                }
                slot = (slot + 1) & mask;
            }
            if found < 0 {
                table[slot as usize] = primitive as i64;
            } else {
                flags[primitive] = 1;
                if delete_pairs {
                    flags[found as usize] = 1;
                }
            }
        }
    }
    if flags.iter().all(|&flag| flag == 0) {
        return Ok(geometry_value.clone());
    }
    delete_flagged(
        cancel_value,
        grain,
        group::Owner::Primitive,
        "__clean_overlaps",
        primitive_count as i64,
        &flags,
        geometry_value,
    )
}

fn delete_triangle_overlaps(
    cancel_value: &Option<Arc<cancel::T>>,
    grain: i64,
    delete_pairs: bool,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let view = topology::View::view(geometry::topology(geometry_value))?;
    let primitive_count = geometry::primitive_count(geometry_value);
    if primitive_count == 0 {
        return Ok(geometry_value.clone());
    }
    if primitive_count > MAX_ARRAY_LENGTH / 4 {
        return Err(message_error("Rdk_mesh.Clean.clean: overlap table exceeds array limits"));
    }
    let count = primitive_count as usize;
    let first_points = AInts::new(count as i64, 0);
    let second_points = AInts::new(count as i64, 0);
    let third_points = AInts::new(count as i64, 0);
    let hashes = AInts::new(count as i64, 0);
    support::parallel_for(cancel_value.as_deref(), grain, count as i64, &|primitive| {
        let primitive = primitive as usize;
        let first = view.primitive_offsets[primitive] as usize;
        let (p0, p1, p2) = (view.vertex_points[first], view.vertex_points[first + 1], view.vertex_points[first + 2]);
        let (low, high) = if p0 <= p1 { (p0, p1) } else { (p1, p0) };
        let (a, b, c) = if p2 <= low {
            (p2, low, high)
        } else if p2 >= high {
            (low, high, p2)
        } else {
            (low, p2, high)
        };
        first_points.set(primitive as i64, a);
        second_points.set(primitive as i64, b);
        third_points.set(primitive as i64, c);
        hashes.set(primitive as i64, hash::mix(hash::mix(hash::mix(17 ^ 3, a), b), c) & OCAML_MAX_INT);
        Ok(())
    })?;
    let (first_points, second_points, third_points, hashes) =
        (first_points.into_vec(), second_points.into_vec(), third_points.into_vec(), hashes.into_vec());
    let capacity = table_capacity(primitive_count);
    let mut table = vec![-1i64; capacity];
    let mask = (capacity - 1) as i64;
    let mut flags = vec![0u8; count];
    for primitive in 0..count {
        cancel::poll(cancel_value.as_deref(), primitive as i64, 4_095)?;
        let mut slot = hashes[primitive] & mask;
        let mut found = -1i64;
        loop {
            let candidate = table[slot as usize];
            if candidate < 0 {
                break;
            }
            let c = candidate as usize;
            if hashes[c] == hashes[primitive]
                && first_points[c] == first_points[primitive]
                && second_points[c] == second_points[primitive]
                && third_points[c] == third_points[primitive]
            {
                found = candidate;
                break;
            }
            slot = (slot + 1) & mask;
        }
        if found < 0 {
            table[slot as usize] = primitive as i64;
        } else {
            flags[primitive] = 1;
            if delete_pairs {
                flags[found as usize] = 1;
            }
        }
    }
    if flags.iter().all(|&flag| flag == 0) {
        return Ok(geometry_value.clone());
    }
    delete_flagged(cancel_value, grain, group::Owner::Primitive, "__clean_overlaps", primitive_count, &flags, geometry_value)
}

fn delete_overlaps(
    cancel_value: &Option<Arc<cancel::T>>,
    grain: i64,
    delete_pairs: bool,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if topology::all_triangles(geometry::topology(geometry_value))? {
        delete_triangle_overlaps(cancel_value, grain, delete_pairs, geometry_value)
    } else {
        delete_overlaps_general(cancel_value, grain, delete_pairs, geometry_value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OverlapPolicy {
    KeepFirstOverlap,
    DeleteOverlapPairs,
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub epsilon: f64,
    pub remove_degenerate: bool,
    pub consolidate_distance: Option<f64>,
    pub overlaps: Option<OverlapPolicy>,
    pub reverse_winding: bool,
    pub remove_nan_points: bool,
    pub remove_unused_points: bool,
    pub delete_unused_groups: bool,
    pub point_attributes: Option<String>,
    pub vertex_attributes: Option<String>,
    pub primitive_attributes: Option<String>,
    pub detail_attributes: Option<String>,
    pub point_groups: Option<String>,
    pub vertex_groups: Option<String>,
    pub primitive_groups: Option<String>,
    pub edge_groups: Option<String>,
}

fn clean(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = &params.cancel;
    let grain = params.grain;
    if grain <= 0 {
        return Err(make(
            "Rdk_mesh.Clean",
            "invalid_argument",
            "Rdk_mesh.Clean.clean: grain must be positive".to_owned(),
        ));
    }
    if !params.epsilon.is_finite() || params.epsilon < 0.0 {
        return Err(message_error("Rdk_mesh.Clean.clean: epsilon must be finite and non-negative"));
    }
    if let Some(value) = params.consolidate_distance {
        if !value.is_finite() || value < 0.0 {
            return Err(message_error(
                "Rdk_mesh.Clean.clean: consolidate distance must be finite and non-negative",
            ));
        }
    }
    for pattern in [
        &params.point_attributes,
        &params.vertex_attributes,
        &params.primitive_attributes,
        &params.detail_attributes,
        &params.point_groups,
        &params.vertex_groups,
        &params.primitive_groups,
        &params.edge_groups,
    ] {
        if let Some(pattern) = pattern {
            if !pattern.trim().is_empty() {
                attribute_pattern::compile(pattern)?;
            }
        }
    }
    let mut result = geometry_value.clone();
    if params.remove_nan_points {
        result = delete_nan_points(cancel_value, grain, &result)?;
    }
    if let Some(tolerance) = params.consolidate_distance {
        result = fuse_grid::weld(
            fuse_grid::WeldParams { cancel: cancel_value.clone(), grain, selection: None, tolerance },
            &result,
        )?;
    }
    if params.remove_degenerate {
        result = delete_degenerate(
            &DeleteDegenerateParams {
                cancel: cancel_value.clone(),
                grain,
                primitives: None,
                epsilon: params.epsilon,
            },
            &result,
        )?;
    }
    if let Some(policy) = params.overlaps {
        result = delete_overlaps(cancel_value, grain, policy == OverlapPolicy::DeleteOverlapPairs, &result)?;
    }
    if params.reverse_winding {
        result = reverse_faces::run(cancel_value.as_deref(), &result)?;
    }
    if params.remove_unused_points {
        result = compact_points::run(cancel_value.as_deref(), Some(grain), &result)?;
    }
    result = attribute_lifecycle::delete(
        &attribute_lifecycle::DeleteParams {
            cancel: cancel_value.clone(),
            reference: None,
            delete_non_selected: false,
            point_pattern: params.point_attributes.clone(),
            vertex_pattern: params.vertex_attributes.clone(),
            primitive_pattern: params.primitive_attributes.clone(),
            detail_pattern: params.detail_attributes.clone(),
        },
        &result,
    )?;
    let mut rules = Vec::new();
    for (owner, pattern) in [
        (group_ops::Owner::GroupPoints, &params.point_groups),
        (group_ops::Owner::GroupVertices, &params.vertex_groups),
        (group_ops::Owner::GroupPrimitives, &params.primitive_groups),
        (group_ops::Owner::GroupEdges, &params.edge_groups),
    ] {
        match pattern {
            Some(delete_pattern) if !delete_pattern.trim().is_empty() => rules.push(group_ops::DeleteRule {
                delete_owner: Some(owner),
                delete_pattern: delete_pattern.clone(),
            }),
            _ => {}
        }
    }
    if rules.is_empty() && !params.delete_unused_groups {
        return Ok(result);
    }
    group_ops::delete(
        &group_ops::DeleteParams { rules, delete_unused: params.delete_unused_groups },
        &result,
    )
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    clean(params, geometry_value).map_err(|failure| {
        if error::code(&failure) == "cancelled" {
            make("clean", "cancelled", "geometry operation was cancelled".to_owned())
        } else {
            make("clean", "invalid_geometry", error::message(&failure).to_owned())
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{self, Owner, Storage};
    use rdk_core::topology::PrimitiveKind;

    fn base() -> Params {
        Params {
            cancel: None,
            grain: 16_384,
            epsilon: 1e-12,
            remove_degenerate: true,
            consolidate_distance: None,
            overlaps: None,
            reverse_winding: false,
            remove_nan_points: false,
            remove_unused_points: false,
            delete_unused_groups: false,
            point_attributes: None,
            vertex_attributes: None,
            primitive_attributes: None,
            detail_attributes: None,
            point_groups: None,
            vertex_groups: None,
            primitive_groups: None,
            edge_groups: None,
        }
    }

    #[test]
    fn degenerate_tolerance_and_extreme_coordinates() {
        let source = geom(
            &[0., 0.1, 0., -1e308, 1e308, 0., 2., 2.],
            &[0., 0., 0.1, 0., 0., 1e308, 0., 0.],
            &[0.; 8],
            &[0, 1, 2, 3, 4, 5, 6, 7],
            &[0, 3, 6, 8],
            Some(vec![PrimitiveKind::Polygon, PrimitiveKind::Polygon, PrimitiveKind::OpenPolyline]),
        );
        let coarse = run(&Params { grain: 1, epsilon: 0.1, ..base() }, &source).unwrap();
        let fine = run(&Params { grain: 1, epsilon: 0.05, ..base() }, &source).unwrap();
        assert_eq!(
            (geometry::primitive_count(&coarse), geometry::primitive_count(&fine)),
            (1, 2),
            "edge-length degeneracy tolerance or extreme-coordinate robustness"
        );
    }

    #[test]
    fn overlap_policies_keep_ancestry() {
        let source = geom(
            &[0., 1., 1., 0.],
            &[0., 0., 1., 1.],
            &[0.; 4],
            &[0, 1, 2, 1, 2, 0, 2, 1, 0, 0, 2, 3, 0, 1, 2],
            &[0, 3, 6, 9, 12, 15],
            Some(vec![
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::Polygon,
                PrimitiveKind::OpenPolyline,
            ]),
        );
        let source = with_attr(&source, Owner::Primitive, "primitive_id", Storage::Int(vec![10, 20, 30, 40, 50]));
        let before = topo(&source);
        for (policy, expected_ids, expected_vertices) in [
            (OverlapPolicy::KeepFirstOverlap, vec![10, 40, 50], vec![0, 1, 2, 0, 2, 3, 0, 1, 2]),
            (OverlapPolicy::DeleteOverlapPairs, vec![40, 50], vec![0, 2, 3, 0, 1, 2]),
        ] {
            let out = run(
                &Params { grain: 1, remove_degenerate: false, overlaps: Some(policy), ..base() },
                &source,
            )
            .unwrap();
            let ids = ints(Owner::Primitive, "primitive_id", &out);
            let (vp, offsets, _) = topo(&out);
            assert_eq!(ids, expected_ids);
            assert_eq!(vp, expected_vertices);
            assert_eq!(offsets, (0..=ids.len() as i64).map(|i| i * 3).collect::<Vec<_>>());
        }
        assert_eq!(topo(&source), before, "overlap repair mutated its source");
    }

    #[test]
    fn triangle_overlaps_use_the_sorted_key_path() {
        let g = grid(8, 6, 4.0, false);
        let (vp, offsets, _) = topo(&g);
        let primitives = offsets.len() - 1;
        let mut dense_vp = Vec::new();
        for primitive in 0..primitives {
            let s = offsets[primitive] as usize;
            let (a, b, c) = (vp[s], vp[s + 1], vp[s + 2]);
            dense_vp.extend_from_slice(&[a, b, c, b, c, a, c, b, a]);
        }
        let (x, y, z) = positions(&g);
        let dense_offsets: Vec<i64> = (0..=(primitives * 3) as i64).map(|p| p * 3).collect();
        let dense = geom(&x, &y, &z, &dense_vp, &dense_offsets, None);
        let ids: Vec<i64> = (0..primitives as i64 * 3).map(|p| p / 3).collect();
        let dense = with_attr(&dense, Owner::Primitive, "source_id", Storage::Int(ids));
        let out = run(
            &Params {
                grain: 257,
                remove_degenerate: false,
                overlaps: Some(OverlapPolicy::KeepFirstOverlap),
                ..base()
            },
            &dense,
        )
        .unwrap();
        assert_eq!(geometry::primitive_count(&out), primitives as i64);
        assert_eq!(geometry::vertex_count(&out), geometry::vertex_count(&g));
        assert_eq!(ints(Owner::Primitive, "source_id", &out), (0..primitives as i64).collect::<Vec<_>>());
    }

    #[test]
    fn consolidation_and_nan_points() {
        let duplicates = geom(&[0., 0., 1.], &[0.; 3], &[0.; 3], &[], &[0], None);
        let out = run(
            &Params { remove_degenerate: false, consolidate_distance: Some(0.0), ..base() },
            &duplicates,
        )
        .unwrap();
        assert_eq!(geometry::point_count(&out), 2);
        let nan = geom(
            &[0., f64::NAN, 1., 0.],
            &[0., 0., 0., 1.],
            &[0.; 4],
            &[0, 2, 3, 1, 2, 3],
            &[0, 3, 6],
            None,
        );
        let out = run(&Params { remove_nan_points: true, remove_degenerate: false, ..base() }, &nan).unwrap();
        assert_eq!(geometry::point_count(&out), 3);
        assert_eq!(geometry::primitive_count(&out), 1);
    }

    #[test]
    fn attribute_and_group_cleanup() {
        let g = geom(&[0., 1., 0.], &[0., 0., 1.], &[0.; 3], &[0, 1, 2], &[0, 3], None);
        let g = with_attr(&g, Owner::Point, "temp_point", Storage::Int(vec![0, 1, 2]));
        let g = with_attr(&g, Owner::Vertex, "keep_vertex", Storage::Int(vec![0, 1, 2]));
        let g = with_attr(&g, Owner::Vertex, "temp_vertex", Storage::Float(vec![1., 2., 3.]));
        let g = with_attr(&g, Owner::Primitive, "temp_primitive", Storage::Int(vec![4]));
        let g = with_attr(&g, Owner::Detail, "temp_detail", Storage::Text(vec![b"drop".to_vec()]));
        let g = geometry::with_group(group::init(None, group::Owner::Point, "drop_points", 3, &|p| p == 0).unwrap(), &g).unwrap();
        let g = geometry::with_group(group::init(None, group::Owner::Vertex, "empty_vertices", 3, &|_| false).unwrap(), &g).unwrap();
        let g = geometry::with_group(group::init(None, group::Owner::Primitive, "keep_faces", 1, &|_| true).unwrap(), &g).unwrap();
        let out = run(
            &Params {
                remove_degenerate: false,
                reverse_winding: true,
                delete_unused_groups: true,
                point_attributes: Some("temp*".to_owned()),
                vertex_attributes: Some("temp*".to_owned()),
                primitive_attributes: Some("temp*".to_owned()),
                detail_attributes: Some("temp*".to_owned()),
                point_groups: Some("drop*".to_owned()),
                ..base()
            },
            &g,
        )
        .unwrap();
        let names: Vec<String> = geometry::attributes(&out).iter().map(|a| attribute::name(a).to_owned()).collect();
        assert_eq!(names, vec!["keep_vertex".to_owned()]);
        assert!(geometry::find_group(group::Owner::Point, "drop_points", &out).is_none());
        assert!(geometry::find_group(group::Owner::Vertex, "empty_vertices", &out).is_none());
        assert!(geometry::find_group(group::Owner::Primitive, "keep_faces", &out).is_some());
        assert_eq!(ints(Owner::Vertex, "keep_vertex", &out), vec![2, 1, 0]);
    }

    #[test]
    fn identity_diagnostics_and_cancellation() {
        let valid = grid(2, 2, 2.0, false);
        let out = run(&base(), &valid).unwrap();
        assert_eq!(geometry::data_id(&out), geometry::data_id(&valid));
        let negative = err(run(&Params { consolidate_distance: Some(-1.0), ..base() }, &valid));
        assert_eq!(error::code(&negative), "invalid_geometry");
        let pattern = err(run(&Params { point_attributes: Some("[".to_owned()), ..base() }, &valid));
        assert_eq!(error::code(&pattern), "invalid_geometry");
        let token = std::sync::Arc::new(cancel::create());
        cancel::cancel(&token);
        let cancelled = err(run(&Params { cancel: Some(token), ..base() }, &valid));
        assert_eq!(error::code(&cancelled), "cancelled");
        assert_eq!(error::code(&err(run(&Params { epsilon: -1.0, ..base() }, &valid))), "invalid_geometry");
    }

    #[test]
    fn delete_degenerate_respects_primitive_selection() {
        // A zero-area triangle and a unit triangle; only the first is selected.
        let g = geom(
            &[0., 1., 2., 0., 1., 0.],
            &[0., 0., 0., 1., 1., 2.],
            &[0.; 6],
            &[0, 1, 2, 3, 4, 5],
            &[0, 3, 6],
            None,
        );
        let selection = group::init(None, group::Owner::Primitive, "pick", 2, &|p| p == 1).unwrap();
        let params = DeleteDegenerateParams { cancel: None, grain: 1, primitives: Some(selection), epsilon: 1e-9 };
        let out = delete_degenerate(&params, &g).unwrap();
        assert_eq!(geometry::primitive_count(&out), 2, "the unselected degenerate triangle stays");
        let all = DeleteDegenerateParams { cancel: None, grain: 1, primitives: None, epsilon: 1e-9 };
        let out = delete_degenerate(&all, &g).unwrap();
        assert_eq!(geometry::primitive_count(&out), 1);
        let wrong = group::init(None, group::Owner::Point, "pick", 6, &|_| true).unwrap();
        let bad = DeleteDegenerateParams { cancel: None, grain: 1, primitives: Some(wrong), epsilon: 1e-9 };
        assert!(delete_degenerate(&bad, &g).is_err());
    }

    #[test]
    fn parallel_exact() {
        for quads in [false, true] {
            let g = grid(120, 120, 2.0, quads);
            let (x, y, z) = positions(&g);
            let (vp, offsets, _) = topo(&g);
            let mut x = x;
            for p in (0..x.len()).step_by(997) {
                x[p] = f64::NAN;
            }
            let (mut new_vp, mut new_offsets) = (Vec::new(), vec![0i64]);
            for prim in 0..offsets.len() - 1 {
                let corners = &vp[offsets[prim] as usize..offsets[prim + 1] as usize];
                new_vp.extend_from_slice(corners);
                new_offsets.push(new_vp.len() as i64);
                if prim % 7 == 0 {
                    let mut rotated = corners.to_vec();
                    rotated.rotate_left(1);
                    new_vp.extend(rotated);
                    new_offsets.push(new_vp.len() as i64);
                }
                if prim % 11 == 0 {
                    let (a, b) = (corners[0], corners[1]);
                    new_vp.extend(if quads { vec![a, b, a, b] } else { vec![a, a, b] });
                    new_offsets.push(new_vp.len() as i64);
                }
            }
            let source = geom(&x, &y, &z, &new_vp, &new_offsets, None);
            for policy in [OverlapPolicy::KeepFirstOverlap, OverlapPolicy::DeleteOverlapPairs] {
                let params = Params {
                    grain: 512,
                    remove_nan_points: true,
                    overlaps: Some(policy),
                    reverse_winding: true,
                    ..base()
                };
                let out = exact_at_one_and_four(&|| run(&params, &source).unwrap());
                assert!(geometry::primitive_count(&out) < geometry::primitive_count(&source));
            }
        }
    }
}
