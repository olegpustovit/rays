//! Point snapping plan: one target point per source point, or -1.
//!
//! OCaml returns `(int array, string) result`; here a failure is an `error::T` with code
//! `invalid_parameter` and the OCaml message (prefix included), a cancellation has code `cancelled`.

use std::sync::Arc;

use rdk_core::{attribute, cancel, error, geometry, group, packed, support};
use rdk_core::error::Code;

use crate::point_clusters::Metric;
use crate::point_grid::{self, float_max, float_min};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Using {
    LeastTargetPoint,
    ClosestTargetPoint,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MatchCondition {
    EqualAttributeValues,
    UnequalAttributeValues,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Targeting {
    NearPoints,
    SpecifiedPoints(String),
}

const PREFIX: &str = "Rdk_spatial.Point_snap.fuse: ";

fn fail(message: String) -> error::T {
    error::new("Rdk_spatial.Point_snap.fuse", Code::InvalidParameter, format!("{PREFIX}{message}"))
}

fn validate_group(label: &str, owner_count: i64, value: Option<&group::T>) -> Result<(), error::T> {
    match value {
        None => Ok(()),
        Some(value) if group::owner(value) == group::Owner::Point && group::length(value) == owner_count => Ok(()),
        Some(_) => Err(fail(format!("{label} must be a matching point group"))),
    }
}

/// Ascending members of a group (`Group.iter` order).
fn members(value: &group::T) -> Vec<i64> {
    (0..group::length(value)).filter(|&index| group::mem(index, value)).collect()
}

enum Scalar {
    Float(Arc<Vec<f64>>),
    Int(Arc<Vec<i64>>),
    Text(Arc<rays_math::text_array::TextArray>),
}

fn scalar_attribute(label: &str, name: &str, geometry_value: &geometry::T) -> Result<Scalar, error::T> {
    let Some(found) = geometry::find_attribute(attribute::Owner::Point, name, geometry_value) else {
        return Err(fail(format!("{label} point attribute {name:?} is missing")));
    };
    match attribute::view::storage(&found)? {
        attribute::Storage::Float(values) => Ok(Scalar::Float(values.clone())),
        attribute::Storage::Int(values) => Ok(Scalar::Int(values.clone())),
        attribute::Storage::Text(values) => Ok(Scalar::Text(values.clone())),
        _ => Err(fail(format!(
            "{label} point attribute {name:?} must be scalar float, integer, or text"
        ))),
    }
}

fn radius_view(label: &str, name: &str, geometry_value: &geometry::T) -> Result<(Scalar, f64), error::T> {
    let values = scalar_attribute(label, name, geometry_value)?;
    if matches!(values, Scalar::Text(_)) {
        return Err(fail(format!("{label} point attribute {name:?} must be scalar float or integer")));
    }
    let count = geometry::point_count(geometry_value);
    let mut maximum = 0.0_f64;
    for point in 0..count as usize {
        let value = match &values {
            Scalar::Float(values) => values[point],
            Scalar::Int(values) => values[point] as f64,
            Scalar::Text(_) => 0.0,
        };
        if !value.is_finite() || value < 0.0 {
            return Err(fail(format!(
                "{label} radius attribute {name:?} has a non-finite or negative value at point {point}"
            )));
        }
        if value > maximum {
            maximum = value;
        }
    }
    Ok((values, maximum))
}

fn radius_at(values: &Option<(Scalar, f64)>, point: usize) -> f64 {
    match values {
        None => 0.0,
        Some((Scalar::Float(values), _)) => values[point],
        Some((Scalar::Int(values), _)) => values[point] as f64,
        Some((Scalar::Text(_), _)) => 0.0,
    }
}

enum MatchViews {
    Float(Arc<Vec<f64>>, Arc<Vec<f64>>),
    Int(Arc<Vec<i64>>, Arc<Vec<i64>>),
    Text(Arc<rays_math::text_array::TextArray>, Arc<rays_math::text_array::TextArray>),
}

fn match_views(name: &str, source: &geometry::T, target: &geometry::T) -> Result<MatchViews, error::T> {
    let query = scalar_attribute("query", name, source)?;
    let target = scalar_attribute("target", name, target)?;
    match (query, target) {
        (Scalar::Float(query), Scalar::Float(target)) => Ok(MatchViews::Float(query, target)),
        (Scalar::Int(query), Scalar::Int(target)) => Ok(MatchViews::Int(query, target)),
        (Scalar::Text(query), Scalar::Text(target)) => Ok(MatchViews::Text(query, target)),
        _ => Err(fail(format!(
            "match attribute {name:?} must have identical scalar storage on query and target geometry"
        ))),
    }
}

fn compatible(condition: MatchCondition, tolerance: f64, views: &Option<MatchViews>, query: usize, target: usize) -> bool {
    let equal = match views {
        None => true,
        Some(MatchViews::Float(queries, targets)) => (queries[query] - targets[target]).abs() <= tolerance,
        Some(MatchViews::Int(queries, targets)) => queries[query] == targets[target],
        Some(MatchViews::Text(queries, targets)) => queries[query] == targets[target],
    };
    match condition {
        MatchCondition::EqualAttributeValues => equal,
        MatchCondition::UnequalAttributeValues => !equal,
    }
}

fn validate_positions(
    cancel_value: Option<&cancel::T>,
    label: &str,
    geometry_value: &geometry::T,
) -> Result<packed::float3::view::View, error::T> {
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    for point in 0..geometry::point_count(geometry_value) as usize {
        cancel::poll(cancel_value, point as i64, 4095)?;
        if !(positions.x[point].is_finite() && positions.y[point].is_finite() && positions.z[point].is_finite()) {
            return Err(fail(format!("{label} point {point} has a non-finite position")));
        }
    }
    Ok(positions)
}

fn specified(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    queries: Option<&group::T>,
    targets: Option<&group::T>,
    attribute_name: &str,
    source: &geometry::T,
    target: &geometry::T,
) -> Result<Vec<i64>, error::T> {
    let source_count = geometry::point_count(source);
    let target_count = geometry::point_count(target);
    validate_positions(cancel_value, "query", source)?;
    validate_positions(cancel_value, "target", target)?;
    let values = match geometry::find_attribute(attribute::Owner::Point, attribute_name, source) {
        Some(found) => match attribute::view::storage(&found)? {
            attribute::Storage::Int(values) => values.clone(),
            _ => {
                return Err(fail(format!(
                    "specified target attribute {attribute_name:?} must be a point integer"
                )));
            }
        },
        None => {
            return Err(fail(format!(
                "specified target point attribute {attribute_name:?} is missing"
            )));
        }
    };
    let same = geometry::data_id(source) == geometry::data_id(target);
    let mut output = vec![-1_i64; source_count as usize];
    support::par_fill(cancel_value, grain, &mut output, &|query| {
        if queries.is_none_or(|selected| group::mem(query, selected)) {
            let destination = values[query as usize];
            if destination >= 0
                && destination < target_count
                && targets.is_none_or(|selected| group::mem(destination, selected))
                && (!same || destination != query)
            {
                return Ok(destination);
            }
        }
        Ok(-1)
    })?;
    Ok(output)
}

#[allow(clippy::too_many_arguments)]
fn near(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    queries: Option<&group::T>,
    targets: Option<&group::T>,
    using: Using,
    tolerance: f64,
    metric: Metric,
    inclusive: bool,
    radius_attribute: Option<&str>,
    match_attribute: Option<&str>,
    match_condition: MatchCondition,
    match_tolerance: f64,
    source: &geometry::T,
    target: &geometry::T,
) -> Result<Vec<i64>, error::T> {
    let source_count = geometry::point_count(source);
    let target_count = geometry::point_count(target);
    let query_positions = validate_positions(cancel_value, "query", source)?;
    let target_positions = validate_positions(cancel_value, "target", target)?;
    let (query_radius, target_radius) = match radius_attribute {
        None => (None, None),
        Some(name) => (
            Some(radius_view("query", name, source)?),
            Some(radius_view("target", name, target)?),
        ),
    };
    let matches = match match_attribute {
        Some(name) => Some(match_views(name, source, target)?),
        None => None,
    };
    let target_members = targets.map(members);
    let target_slots = match &target_members {
        None => target_count,
        Some(list) => list.len() as i64,
    };
    if target_slots == 0 || source_count == 0 {
        return Ok(vec![-1; source_count as usize]);
    }
    let mut min = [f64::INFINITY; 3];
    let mut max = [f64::NEG_INFINITY; 3];
    let planes = [&target_positions.x, &target_positions.y, &target_positions.z];
    let mut visit = |point: usize| {
        for axis in 0..3 {
            min[axis] = float_min(min[axis], planes[axis][point]);
            max[axis] = float_max(max[axis], planes[axis][point]);
        }
    };
    match &target_members {
        None => (0..target_count as usize).for_each(&mut visit),
        Some(list) => list.iter().for_each(|&point| visit(point as usize)),
    }
    let max_query_radius = query_radius.as_ref().map_or(0.0, |(_, v)| *v);
    let max_target_radius = target_radius.as_ref().map_or(0.0, |(_, v)| *v);
    let cell_size = tolerance + max_query_radius + max_target_radius;
    if !cell_size.is_finite() {
        return Err(fail("tolerance plus point radii exceeds the finite range".to_owned()));
    }
    let exact = cell_size == 0.0;
    let scaled_delta = |left: f64, right: f64| -> f64 {
        let delta = right - left;
        if delta.is_finite() {
            delta / cell_size
        } else {
            (right / cell_size) - (left / cell_size)
        }
    };
    let span = |axis: usize| if exact { 0.0 } else { scaled_delta(min[axis], max[axis]) };
    let (span_x, span_y, span_z) = (span(0), span(1), span(2));
    if !exact {
        let largest = float_max(span_x, float_max(span_y, span_z));
        if !largest.is_finite() || largest > ((i64::MAX >> 1) / 4) as f64 {
            return Err(fail("snap distance is too small for the target geometry extent".to_owned()));
        }
    }
    let mut grid = point_grid::create(target_slots, target_count);
    let cell_coord = |value: f64, minimum: f64| -> i64 {
        if exact {
            point_grid::float_cell(value)
        } else {
            scaled_delta(minimum, value).floor() as i64
        }
    };
    let mut insert_target = |point: usize| {
        let cx = cell_coord(target_positions.x[point], min[0]);
        let cy = cell_coord(target_positions.y[point], min[1]);
        let cz = cell_coord(target_positions.z[point], min[2]);
        point_grid::insert(&mut grid, cx, cy, cz, point as i64);
    };
    match &target_members {
        None => (0..target_count as usize).for_each(&mut insert_target),
        Some(list) => list.iter().for_each(|&point| insert_target(point as usize)),
    }
    let same = geometry::data_id(source) == geometry::data_id(target);
    let neighbor_delta: i64 = if exact { 0 } else { 1 };
    let mut output = vec![-1_i64; source_count as usize];
    support::par_fill(cancel_value, grain, &mut output, &|query| {
        if let Some(selected) = queries {
            if !group::mem(query, selected) {
                return Ok(-1);
            }
        }
        let query_index = query as usize;
        let (qx, qy, qz) = (query_positions.x[query_index], query_positions.y[query_index], query_positions.z[query_index]);
        let query_cell = if exact {
            Some((cell_coord(qx, min[0]), cell_coord(qy, min[1]), cell_coord(qz, min[2])))
        } else {
            let sx = scaled_delta(min[0], qx);
            let sy = scaled_delta(min[1], qy);
            let sz = scaled_delta(min[2], qz);
            if !(sx.is_finite() && sy.is_finite() && sz.is_finite())
                || sx < -1.0
                || sy < -1.0
                || sz < -1.0
                || sx > span_x + 1.0
                || sy > span_y + 1.0
                || sz > span_z + 1.0
            {
                None
            } else {
                Some((sx.floor() as i64, sy.floor() as i64, sz.floor() as i64))
            }
        };
        let Some((cx, cy, cz)) = query_cell else {
            return Ok(-1);
        };
        let mut best = -1_i64;
        let mut best_distance = f64::INFINITY;
        for dx in -neighbor_delta..=neighbor_delta {
            for dy in -neighbor_delta..=neighbor_delta {
                for dz in -neighbor_delta..=neighbor_delta {
                    let cell = point_grid::find(&grid, cx + dx, cy + dy, cz + dz);
                    if cell < 0 {
                        continue;
                    }
                    let mut candidate = grid.cell_head[cell as usize];
                    while candidate >= 0 {
                        let target_point = candidate;
                        let t = target_point as usize;
                        if (!same || query != target_point)
                            && compatible(match_condition, match_tolerance, &matches, query_index, t)
                        {
                            let px = qx - target_positions.x[t];
                            let py = qy - target_positions.y[t];
                            let pz = qz - target_positions.z[t];
                            let threshold = tolerance + radius_at(&query_radius, query_index) + radius_at(&target_radius, t);
                            let (position_matches, distance) = if threshold == 0.0 {
                                (px == 0.0 && py == 0.0 && pz == 0.0, 0.0)
                            } else {
                                match metric {
                                    Metric::Euclidean => {
                                        let scale = float_max(px.abs(), float_max(py.abs(), pz.abs()));
                                        if scale == 0.0 {
                                            (true, 0.0)
                                        } else if scale > threshold {
                                            (false, f64::INFINITY)
                                        } else {
                                            let nx = px / scale;
                                            let ny = py / scale;
                                            let nz = pz / scale;
                                            let ratio = scale / threshold;
                                            let normalized = ratio * ratio * ((nx * nx) + (ny * ny) + (nz * nz));
                                            (
                                                if inclusive { normalized <= 1.0 } else { normalized < 1.0 },
                                                scale * ((nx * nx) + (ny * ny) + (nz * nz)).sqrt(),
                                            )
                                        }
                                    }
                                    Metric::Componentwise => {
                                        let maximum = float_max(px.abs(), float_max(py.abs(), pz.abs()));
                                        (
                                            if inclusive { maximum <= threshold } else { maximum < threshold },
                                            maximum,
                                        )
                                    }
                                }
                            };
                            if position_matches {
                                match using {
                                    Using::LeastTargetPoint => {
                                        if best < 0 || target_point < best {
                                            best = target_point;
                                        }
                                    }
                                    Using::ClosestTargetPoint => {
                                        if distance < best_distance
                                            || (distance == best_distance && (best < 0 || target_point < best))
                                        {
                                            best = target_point;
                                            best_distance = distance;
                                        }
                                    }
                                }
                            }
                        }
                        candidate = grid.point_next[t];
                    }
                }
            }
        }
        Ok(best)
    })?;
    Ok(output)
}

/// Return one target point number per source point, or `-1` for no snap. Only selected queries are
/// evaluated and only selected targets are eligible.
#[allow(clippy::too_many_arguments)]
pub fn plan(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    queries: Option<&group::T>,
    targets: Option<&group::T>,
    targeting: &Targeting,
    using: Using,
    tolerance: f64,
    metric: Metric,
    inclusive: bool,
    radius_attribute: Option<&str>,
    match_attribute: Option<&str>,
    match_condition: MatchCondition,
    match_tolerance: f64,
    source: &geometry::T,
    target: &geometry::T,
) -> Result<Vec<i64>, error::T> {
    if grain <= 0 {
        return Err(fail("grain must be positive".to_owned()));
    }
    validate_group("query selection", geometry::point_count(source), queries)?;
    validate_group("target selection", geometry::point_count(target), targets)?;
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(fail("snap distance must be finite and non-negative".to_owned()));
    }
    if !match_tolerance.is_finite() || match_tolerance < 0.0 {
        return Err(fail("match tolerance must be finite and non-negative".to_owned()));
    }
    if radius_attribute.is_some_and(|name| name.trim().is_empty()) {
        return Err(fail("radius attribute name must not be empty".to_owned()));
    }
    if match_attribute.is_some_and(|name| name.trim().is_empty()) {
        return Err(fail("match attribute name must not be empty".to_owned()));
    }
    if match_attribute.is_none() {
        if match_condition != MatchCondition::EqualAttributeValues {
            return Err(fail("unequal match condition requires a match attribute".to_owned()));
        }
        if match_tolerance != 0.0 {
            return Err(fail("match tolerance requires a match attribute".to_owned()));
        }
    }
    cancel::check_opt(cancel_value)?;
    match targeting {
        Targeting::SpecifiedPoints(attribute_name) => {
            if radius_attribute.is_some() || match_attribute.is_some() {
                return Err(fail(
                    "specified-point targeting does not accept radius or match attributes".to_owned(),
                ));
            }
            if attribute_name.trim().is_empty() {
                return Err(fail("specified target attribute name must not be empty".to_owned()));
            }
            specified(cancel_value, grain, queries, targets, attribute_name, source, target)
        }
        Targeting::NearPoints => near(
            cancel_value,
            grain,
            queries,
            targets,
            using,
            tolerance,
            metric,
            inclusive,
            radius_attribute,
            match_attribute,
            match_condition,
            match_tolerance,
            source,
            target,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cloud(points: &[[f64; 3]]) -> geometry::T {
        let x = points.iter().map(|p| p[0]).collect();
        let y = points.iter().map(|p| p[1]).collect();
        let z = points.iter().map(|p| p[2]).collect();
        let positions = packed::float3::view::of_owned_exn(x, y, z).unwrap();
        geometry::create(positions, rdk_core::topology::empty(points.len() as i64).unwrap(), None, None, None).unwrap()
    }

    #[test]
    fn near_plan_picks_least_and_closest_target() {
        let source = cloud(&[[0.0, 0.0, 0.0], [10.0, 0.0, 0.0]]);
        let target = cloud(&[[0.5, 0.0, 0.0], [0.1, 0.0, 0.0], [10.0, 0.0, 0.0]]);
        let run = |using| {
            plan(
                None, 4, None, None, &Targeting::NearPoints, using, 1.0, Metric::Euclidean, true, None, None,
                MatchCondition::EqualAttributeValues, 0.0, &source, &target,
            )
            .unwrap()
        };
        assert_eq!(run(Using::LeastTargetPoint), vec![0, 2]);
        assert_eq!(run(Using::ClosestTargetPoint), vec![1, 2]);
    }
}
