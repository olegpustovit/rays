//! Port of lib/rdk/geom/spatial/spatial_index.ml: immutable balanced index over packed 3D points.
//!
//! Construction uses an in-place median partition, skips globally flat axes, and stores one packed
//! point-index permutation plus at most three active axis codes. Queries are deterministic: equal
//! distances are ordered by the original point index.
use std::cmp::Ordering;
use std::sync::Mutex;

use rdk_core::{cancel, error, group, packed, support};
use rdk_core::error::Code;

use crate::point_grid::{float_max, float_min};

const DEFAULT_GRAIN: i64 = 16_384;

pub struct T {
    positions: packed::Float3::View::View,
    order: Vec<i64>,
    axes: Vec<i64>,
}

fn fail(code: Code, message: &str) -> error::T {
    error::new("spatial_index", code, message.to_owned())
}

fn invalid(operation: &str, message: &str) -> error::T {
    error::new(operation, Code::InvalidArgument, message.to_owned())
}

fn cancelled() -> error::T {
    fail(Code::Cancelled, "spatial-index construction was cancelled")
}

fn cancel_check(cancel_value: Option<&cancel::T>) -> Result<(), error::T> {
    cancel::check_opt(cancel_value).map_err(|_| cancelled())
}

fn pool_error(message: String) -> error::T {
    fail(Code::Internal, &message)
}

fn length(value: &T) -> i64 {
    value.order.len() as i64
}

/// Builds the index. `grain` defaults to 16384; disjoint tree ranges at or above it build in parallel.
pub fn create(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    points: Option<&group::T>,
    source: &packed::Float3::T,
) -> Result<T, error::T> {
    let grain = grain.unwrap_or(DEFAULT_GRAIN);
    if grain <= 0 {
        return Err(fail(Code::InvalidParameter, "grain must be positive"));
    }
    let positions = packed::Float3::View::view(source)?;
    let count = packed::Float3::length(source);
    let mut order: Vec<i64> = match points {
        None => (0..count).collect(),
        Some(selection) => {
            if !matches!(group::owner(selection), group::Owner::Point) {
                return Err(fail(Code::InvalidGroup, "source selection must be a point group"));
            }
            if group::length(selection) != count {
                return Err(fail(
                    Code::InvalidGroup,
                    "source point group length does not match positions",
                ));
            }
            (0..count).filter(|&point| group::mem(point, selection)).collect()
        }
    };

    let mut min = [f64::INFINITY; 3];
    let mut max = [f64::NEG_INFINITY; 3];
    for slot in 0..order.len() {
        cancel::poll(cancel_value, slot as i64, 16_383).map_err(|_| cancelled())?;
        let point = order[slot] as usize;
        let (x, y, z) = (positions.x[point], positions.y[point], positions.z[point]);
        if !(x.is_finite() && y.is_finite() && z.is_finite()) {
            return Err(fail(Code::InvalidPosition, "source positions must be finite"));
        }
        for (axis, value) in [x, y, z].into_iter().enumerate() {
            min[axis] = float_min(min[axis], value);
            max[axis] = float_max(max[axis], value);
        }
    }

    // Widest axis first; ties by axis number. Only axes with a positive span are active.
    let mut ranked = [(max[0] - min[0], 0_i64), (max[1] - min[1], 1), (max[2] - min[2], 2)];
    ranked.sort_by(|left, right| {
        right.0.partial_cmp(&left.0).unwrap_or(Ordering::Equal).then(left.1.cmp(&right.1))
    });
    let active = ranked.iter().filter(|(span, _)| *span > 0.0).count();
    let axes: Vec<i64> = if active == 0 {
        vec![0]
    } else {
        ranked[..active].iter().map(|(_, axis)| *axis).collect()
    };

    partition(cancel_value, &positions, &axes, grain, &mut order, 0)?;
    Ok(T {
        positions,
        order,
        axes,
    })
}

/// Median-partitions `order` (a slice of the whole permutation) and recurses on both halves.
fn partition(
    cancel_value: Option<&cancel::T>,
    positions: &packed::Float3::View::View,
    axes: &[i64],
    grain: i64,
    order: &mut [i64],
    depth: usize,
) -> Result<(), error::T> {
    let count = order.len();
    if count < 2 {
        return Ok(());
    }
    cancel_check(cancel_value)?;
    let middle = (count - 1) / 2;
    let keys: &[f64] = match axes[depth % axes.len()] {
        0 => &positions.x[..],
        1 => &positions.y[..],
        _ => &positions.z[..],
    };
    support::select(cancel_value, keys, order, 0, count as i64 - 1, middle as i64)?;
    let (left, rest) = order.split_at_mut(middle);
    let right = &mut rest[1..];
    if count as i64 >= grain {
        let left_slot = Mutex::new(left);
        let right_slot = Mutex::new(right);
        let failure: Mutex<Option<error::T>> = Mutex::new(None);
        let record = |result: Result<(), error::T>| -> Result<(), String> {
            if let Err(error) = result {
                let mut slot = failure.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                if slot.is_none() {
                    *slot = Some(error);
                }
            }
            Ok(())
        };
        parallel::both_unit(
            &|| {
                let mut guard = left_slot.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                record(partition(cancel_value, positions, axes, grain, &mut **guard, depth + 1))
            },
            &|| {
                let mut guard = right_slot.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                record(partition(cancel_value, positions, axes, grain, &mut **guard, depth + 1))
            },
        )
        .map_err(pool_error)?;
        if let Some(error) =
            failure.lock().unwrap_or_else(std::sync::PoisonError::into_inner).take()
        {
            return Err(error);
        }
    } else {
        partition(cancel_value, positions, axes, grain, left, depth + 1)?;
        partition(cancel_value, positions, axes, grain, right, depth + 1)?;
    }
    Ok(())
}

fn selected(points: Option<&group::T>, query: i64) -> bool {
    match points {
        None => true,
        Some(group_value) => group::mem(query, group_value),
    }
}

fn check_selection(
    operation: &str,
    points: Option<&group::T>,
    query_count: i64,
) -> Result<(), error::T> {
    match points {
        Some(group_value)
            if !matches!(group::owner(group_value), group::Owner::Point)
                || group::length(group_value) != query_count =>
        {
            Err(invalid(operation, "query selection must be a matching point group"))
        }
        _ => Ok(()),
    }
}

/// Query entry points. Each fills disjoint per-query output slices in parallel.
#[allow(non_snake_case)]
pub mod View {
    use std::sync::Mutex;

    use rdk_core::{cancel, error, group, packed, support};

    use super::{T, check_selection, invalid, length, selected};
    use crate::point_grid::float_min;

    /// One query's fixed-width slot in the caller's output: `offset` is relative to the chunk.
    #[derive(Clone, Copy)]
    struct KQuery {
        query: usize,
        offset: usize,
        capacity: i64,
        maximum_squared: f64,
    }

    /// Search radius squared: the k-th best distance once the slot is full, else the max distance.
    fn bound(q: &KQuery, found: i64, distances: &[f64]) -> f64 {
        if found < q.capacity {
            q.maximum_squared
        } else {
            let worst = distances[q.offset + q.capacity as usize - 1];
            if q.maximum_squared < worst { q.maximum_squared } else { worst }
        }
    }

    /// Inserts the k nearest points under the subtree `[first, last]`; returns the new fill count.
    #[allow(clippy::too_many_arguments, clippy::neg_cmp_op_on_partial_ord)]
    fn visit(
        tree: &T,
        queries: &packed::Float3::View::View,
        q: &KQuery,
        indices: &mut [i64],
        distances: &mut [f64],
        first: i64,
        last: i64,
        depth: usize,
        found: i64,
    ) -> i64 {
        if first > last {
            return found;
        }
        let positions = &tree.positions;
        let middle = first + (last - first) / 2;
        let point = tree.order[middle as usize];
        let p = point as usize;
        let query = q.query;
        let dx = positions.x[p] - queries.x[query];
        let dy = positions.y[p] - queries.y[query];
        let dz = positions.z[p] - queries.z[query];
        let distance = (dx * dx) + (dy * dy) + (dz * dz);
        let capacity = q.capacity;
        let cap = capacity as usize;
        let mut found = found;
        // NaN distances fall through to insertion, as in the OCaml comparison.
        if !(distance > q.maximum_squared) {
            let last_slot = q.offset + cap - 1;
            let full_and_worse = found >= capacity
                && !(distance < distances[last_slot]
                    || (distance == distances[last_slot] && point < indices[last_slot]));
            if !full_and_worse {
                let mut position = found.min(capacity - 1);
                while position > 0 {
                    let before = q.offset + position as usize - 1;
                    if distance < distances[before]
                        || (distance == distances[before] && point < indices[before])
                    {
                        indices[before + 1] = indices[before];
                        distances[before + 1] = distances[before];
                        position -= 1;
                    } else {
                        break;
                    }
                }
                indices[q.offset + position as usize] = point;
                distances[q.offset + position as usize] = distance;
                found = capacity.min(found + 1);
            }
        }
        let axis = tree.axes[depth % tree.axes.len()];
        let delta = match axis {
            0 => queries.x[query] - positions.x[p],
            1 => queries.y[query] - positions.y[p],
            _ => queries.z[query] - positions.z[p],
        };
        if delta <= 0.0 {
            let found = visit(tree, queries, q, indices, distances, first, middle - 1, depth + 1, found);
            if delta * delta <= bound(q, found, distances) {
                visit(tree, queries, q, indices, distances, middle + 1, last, depth + 1, found)
            } else {
                found
            }
        } else {
            let found = visit(tree, queries, q, indices, distances, middle + 1, last, depth + 1, found);
            if delta * delta <= bound(q, found, distances) {
                visit(tree, queries, q, indices, distances, first, middle - 1, depth + 1, found)
            } else {
                found
            }
        }
    }

    /// Query every packed point into fixed-width disjoint slices. `indices` and `distances_squared`
    /// need `query_count * capacity` elements and `counts` needs `query_count`. Unselected queries
    /// leave their slots untouched.
    #[allow(clippy::too_many_arguments)]
    pub fn nearest_k_many_into(
        cancel_value: Option<&cancel::T>,
        points: Option<&group::T>,
        grain: i64,
        value: &T,
        queries: &packed::Float3::T,
        max_distance_squared: f64,
        capacity: i64,
        indices: &mut [i64],
        distances_squared: &mut [f64],
        counts: &mut [i64],
    ) -> Result<(), error::T> {
        const OP: &str = "Spatial_index.View.nearest_k_many_into";
        if grain <= 0 {
            return Err(invalid(OP, "grain must be positive"));
        }
        let query_count = packed::Float3::length(queries);
        check_selection(OP, points, query_count)?;
        let len = |slice_len: usize| i64::try_from(slice_len).unwrap_or(i64::MAX);
        let fits = capacity > 0
            && query_count.checked_mul(capacity).is_some_and(|needed| {
                len(indices.len()) >= needed && len(distances_squared.len()) >= needed
            })
            && len(counts.len()) >= query_count;
        if !fits {
            return Err(invalid(OP, "invalid output storage"));
        }
        let needed = (query_count * capacity) as usize;
        let positions = packed::Float3::View::view(queries)?;
        if query_count > 0 {
            let cap = capacity as usize;
            let stride = (grain as usize).saturating_mul(cap);
            let slots: Vec<Mutex<(&mut [i64], &mut [i64], &mut [f64])>> = counts
                [..query_count as usize]
                .chunks_mut(grain as usize)
                .zip(indices[..needed].chunks_mut(stride))
                .zip(distances_squared[..needed].chunks_mut(stride))
                .map(|((counts, indices), distances)| Mutex::new((counts, indices, distances)))
                .collect();
            let tree_length = length(value);
            support::run_indexed_ranges(
                grain,
                query_count,
                &|range: i64, first: i64, last: i64| -> Result<(), error::T> {
                    let mut guard =
                        slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                    let (counts, indices, distances) = &mut *guard;
                    for query in first..last {
                        cancel::poll(cancel_value, query, 4095)?;
                        if selected(points, query) {
                            let local = (query - first) as usize;
                            counts[local] = if tree_length == 0 {
                                0
                            } else {
                                let context = KQuery {
                                    query: query as usize,
                                    offset: local * cap,
                                    capacity,
                                    maximum_squared: max_distance_squared,
                                };
                                visit(
                                    value,
                                    &positions,
                                    &context,
                                    &mut **indices,
                                    &mut **distances,
                                    0,
                                    tree_length - 1,
                                    0,
                                    0,
                                )
                            };
                        }
                    }
                    Ok(())
                },
            )?;
        }
        Ok(())
    }

    /// Squared-distance-only nearest search (no provenance). Misses keep `infinity`; unselected
    /// queries keep their slot.
    fn visit_distance_squared(
        tree: &T,
        queries: &packed::Float3::View::View,
        query: usize,
        maximum_squared: f64,
        best: &mut f64,
        first: i64,
        last: i64,
        depth: usize,
    ) {
        if first > last {
            return;
        }
        let positions = &tree.positions;
        let middle = first + (last - first) / 2;
        let point = tree.order[middle as usize] as usize;
        let dx = positions.x[point] - queries.x[query];
        let dy = positions.y[point] - queries.y[query];
        let dz = positions.z[point] - queries.z[query];
        let distance = (dx * dx) + (dy * dy) + (dz * dz);
        if distance <= maximum_squared && distance < *best {
            *best = distance;
        }
        let delta = match tree.axes[depth % tree.axes.len()] {
            0 => queries.x[query] - positions.x[point],
            1 => queries.y[query] - positions.y[point],
            _ => queries.z[query] - positions.z[point],
        };
        if delta <= 0.0 {
            visit_distance_squared(tree, queries, query, maximum_squared, best, first, middle - 1, depth + 1);
            if delta * delta <= float_min(maximum_squared, *best) {
                visit_distance_squared(tree, queries, query, maximum_squared, best, middle + 1, last, depth + 1);
            }
        } else {
            visit_distance_squared(tree, queries, query, maximum_squared, best, middle + 1, last, depth + 1);
            if delta * delta <= float_min(maximum_squared, *best) {
                visit_distance_squared(tree, queries, query, maximum_squared, best, first, middle - 1, depth + 1);
            }
        }
    }

    /// Fill only nearest squared distance, retaining `infinity` for misses and unselected queries.
    pub fn nearest_distances_many_into(
        cancel_value: Option<&cancel::T>,
        points: Option<&group::T>,
        grain: i64,
        value: &T,
        queries: &packed::Float3::T,
        max_distance_squared: f64,
        distances_squared: &mut [f64],
    ) -> Result<(), error::T> {
        const OP: &str = "Spatial_index.View.nearest_distances_many_into";
        if grain <= 0 {
            return Err(invalid(OP, "grain must be positive"));
        }
        let query_count = packed::Float3::length(queries);
        check_selection(OP, points, query_count)?;
        if (distances_squared.len() as i64) < query_count {
            return Err(invalid(OP, "output array is too short"));
        }
        let positions = packed::Float3::View::view(queries)?;
        if query_count > 0 {
            let chunks: Vec<Mutex<&mut [f64]>> = distances_squared[..query_count as usize]
                .chunks_mut(grain as usize)
                .map(Mutex::new)
                .collect();
            let tree_length = length(value);
            support::run_indexed_ranges(
                grain,
                query_count,
                &|range: i64, first: i64, last: i64| -> Result<(), error::T> {
                    let mut guard =
                        chunks[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                    let out: &mut [f64] = &mut **guard;
                    for query in first..last {
                        cancel::poll(cancel_value, query, 4095)?;
                        if selected(points, query) {
                            let local = (query - first) as usize;
                            out[local] = f64::INFINITY;
                            if tree_length > 0 {
                                visit_distance_squared(
                                    value,
                                    &positions,
                                    query as usize,
                                    max_distance_squared,
                                    &mut out[local],
                                    0,
                                    tree_length - 1,
                                    0,
                                );
                            }
                        }
                    }
                    Ok(())
                },
            )?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    fn points(x: &[f64], y: &[f64], z: &[f64]) -> packed::Float3::T {
        packed::Float3::of_owned(x.to_vec(), y.to_vec(), z.to_vec()).unwrap()
    }

    /// Builds with `domains` workers, as in the OCaml `Parallel.run ~domains`.
    fn build(domains: i64, source: &packed::Float3::T, grain: i64) -> T {
        let slot: Mutex<Option<T>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            let built = create(None, Some(grain), None, source).map_err(|e| error::to_string(&e))?;
            *slot.lock().unwrap() = Some(built);
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }

    #[test]
    fn rejects_bad_grain_and_non_finite_positions() {
        let good = points(&[0.0, 1.0], &[0.0, 1.0], &[0.0, 1.0]);
        let zero = create(None, Some(0), None, &good).err().unwrap();
        assert_eq!(error::code(&zero), "invalid_parameter");
        let bad = points(&[f64::NAN], &[0.0], &[0.0]);
        let non_finite = create(None, None, None, &bad).err().unwrap();
        assert_eq!(error::code(&non_finite), "invalid_position");
    }

    #[test]
    fn distance_only_search_matches_provenance_and_misses_are_infinite() {
        // Port of test_distance_from_geometry.ml test_distance_only_indexes.
        let queries = points(&[0.0, 1.0, 4.0], &[0.0, 2.0, 0.0], &[0.0, 0.0, 0.0]);
        let source = points(&[0.0, 3.0], &[1.0, 0.0], &[0.0, 0.0]);
        let index = create(None, Some(1), None, &source).unwrap();
        let mut indices = [-1_i64; 3];
        let mut generic = [f64::INFINITY; 3];
        let mut counts = [0_i64; 3];
        let mut specialized = [f64::INFINITY; 3];
        View::nearest_k_many_into(
            None, None, 1, &index, &queries, f64::INFINITY, 1, &mut indices, &mut generic,
            &mut counts,
        )
        .unwrap();
        View::nearest_distances_many_into(
            None, None, 1, &index, &queries, f64::INFINITY, &mut specialized,
        )
        .unwrap();
        assert_eq!(generic, specialized);
        let mut bounded = [0.0_f64; 3];
        View::nearest_distances_many_into(None, None, 1, &index, &queries, 0.01, &mut bounded)
            .unwrap();
        assert!(bounded.iter().all(|&value| value == f64::INFINITY));
    }

    #[test]
    fn parallel_build_and_queries_match_sequential_and_brute_force() {
        // Port of test_rdk_attributes.ml's spatial exactness check, plus 1-vs-4 identity.
        let count = 257_usize;
        let (mut xs, mut ys, mut zs) = (Vec::new(), Vec::new(), Vec::new());
        for point in 0..count {
            let value = point as f64;
            xs.push((value * 0.73).sin() * 7.0);
            ys.push((value * 1.17).cos() * 5.0);
            zs.push((value * 0.19).sin() * 3.0);
        }
        let source = points(&xs, &ys, &zs);
        let one = build(1, &source, 7);
        let four = build(4, &source, 7);
        assert_eq!(one.order, four.order);
        assert_eq!(one.axes, four.axes);

        let queries_count = 128_usize;
        let (mut qx, mut qy, mut qz) = (Vec::new(), Vec::new(), Vec::new());
        for query in 0..queries_count {
            let value = query as f64;
            qx.push((value * 0.31).sin() * 6.0);
            qy.push((value * 0.43).cos() * 4.0);
            qz.push((value * 0.61).sin() * 2.0);
        }
        let queries = points(&qx, &qy, &qz);
        let mut indices = vec![0_i64; queries_count];
        let mut distances = vec![0.0_f64; queries_count];
        let mut counts = vec![0_i64; queries_count];
        let mut indices_four = indices.clone();
        let mut distances_four = distances.clone();
        let mut counts_four = counts.clone();
        View::nearest_k_many_into(
            None, None, 3, &one, &queries, f64::INFINITY, 1, &mut indices, &mut distances,
            &mut counts,
        )
        .unwrap();
        View::nearest_k_many_into(
            None, None, 3, &four, &queries, f64::INFINITY, 1, &mut indices_four,
            &mut distances_four, &mut counts_four,
        )
        .unwrap();
        assert_eq!(indices, indices_four);
        assert_eq!(distances, distances_four);
        assert_eq!(counts, counts_four);
        for query in 0..queries_count {
            let mut expected = 0_i64;
            let mut expected_distance = f64::INFINITY;
            for point in 0..count {
                let dx = xs[point] - qx[query];
                let dy = ys[point] - qy[query];
                let dz = zs[point] - qz[query];
                let distance = (dx * dx) + (dy * dy) + (dz * dz);
                if distance < expected_distance
                    || (distance == expected_distance && (point as i64) < expected)
                {
                    expected = point as i64;
                    expected_distance = distance;
                }
            }
            assert_eq!(counts[query], 1);
            assert_eq!(indices[query], expected);
            assert_eq!(distances[query], expected_distance);
        }
    }

    #[test]
    fn source_selection_restricts_members() {
        let source = points(&[0.0, 5.0, 1.0], &[0.0, 0.0, 0.0], &[0.0, 0.0, 0.0]);
        let mut builder = group::Builder::create(group::Owner::Point, "sel", 3).unwrap();
        group::Builder::set(&mut builder, 0, true).unwrap();
        group::Builder::set(&mut builder, 2, true).unwrap();
        let selection = group::Builder::freeze(&mut builder).unwrap();
        let index = create(None, None, Some(&selection), &source).unwrap();
        assert_eq!(index.order, vec![0, 2]);
        let queries = points(&[0.0, 5.0, 0.0], &[0.0; 3], &[0.0; 3]);
        let mut out = [-7.0_f64; 3];
        View::nearest_distances_many_into(None, None, 2, &index, &queries, f64::INFINITY, &mut out)
            .unwrap();
        assert_eq!(out[0], 0.0);
        // Member points are x = 0 and x = 1, so the query at x = 5 is 16 away from x = 1.
        assert_eq!(out[1], 16.0);
        assert_eq!(out[2], 0.0);
    }
}
