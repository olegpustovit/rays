//! Deterministic point clustering and representative order.
//!
//! OCaml returns `(t, string) result` and lets `Cancel.Cancelled` escape; here both are an
//! `error::T`: a plain failure has code `invalid_parameter` and the OCaml message, a cancellation
//! has code `cancelled`.
use rdk_core::support::DisjointSet;
use rdk_core::{cancel, error, geometry, group, packed};
use rdk_core::error::Code;

use crate::point_grid;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Metric {
    Euclidean,
    Componentwise,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Clusters {
    pub count: i64,
    pub of_point: Vec<i64>,
    pub offsets: Vec<i64>,
    pub members: Vec<i64>,
    pub representatives: Vec<i64>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum T {
    Identity,
    Clusters(Clusters),
}

fn failure(operation: &str, message: String) -> error::T {
    error::new(operation, Code::InvalidParameter, message)
}


/// Packs `cluster_of[point]` (already numbered 0..cluster_count in first-seen order) into clusters.
fn pack(cluster_of: Vec<i64>, cluster_count: usize) -> Clusters {
    let count = cluster_of.len();
    let mut offsets = vec![0_i64; cluster_count + 1];
    for &cluster in &cluster_of {
        offsets[cluster as usize + 1] += 1;
    }
    for cluster in 0..cluster_count {
        offsets[cluster + 1] += offsets[cluster];
    }
    let mut members = vec![0_i64; count];
    let mut cursors = offsets.clone();
    for (point, &cluster) in cluster_of.iter().enumerate() {
        let slot = cursors[cluster as usize];
        members[slot as usize] = point as i64;
        cursors[cluster as usize] = slot + 1;
    }
    let representatives = (0..cluster_count).map(|cluster| members[offsets[cluster] as usize]).collect();
    Clusters {
        count: cluster_count as i64,
        of_point: cluster_of,
        offsets,
        members,
        representatives,
    }
}

pub fn of_links(
    cancel_value: Option<&cancel::T>,
    operation: &str,
    destinations: &[i64],
) -> Result<T, error::T> {
    let count = destinations.len();
    let mut parent = DisjointSet::new(count);
    for point in 0..count {
        cancel::poll(cancel_value, point as i64, 4095)?;
        let destination = destinations[point];
        if destination < -1 || destination >= count as i64 {
            return Err(failure(
                operation,
                format!("{operation} target point {destination} at query {point} is out of range"),
            ));
        }
        if destination >= 0 && destination != point as i64 {
            parent.union_min(point, destination as usize);
        }
    }
    let mut changed = false;
    for point in 0..count {
        let representative = parent.find(point);
        parent.parent[point] = representative as i64;
        if representative != point {
            changed = true;
        }
    }
    if !changed {
        return Ok(T::Identity);
    }
    let mut representative_to_cluster = vec![-1_i64; count];
    let mut cluster_count = 0_usize;
    for point in 0..count {
        let representative = parent.parent[point] as usize;
        if representative_to_cluster[representative] < 0 {
            representative_to_cluster[representative] = cluster_count as i64;
            cluster_count += 1;
        }
        parent.parent[point] = representative_to_cluster[representative];
    }
    Ok(T::Clusters(pack(parent.parent, cluster_count)))
}

#[allow(clippy::too_many_arguments)]
pub fn create(
    cancel_value: Option<&cancel::T>,
    selection: Option<&group::T>,
    metric: Metric,
    inclusive: bool,
    operation: &str,
    tolerance: f64,
    compatible: &dyn Fn(i64, i64) -> bool,
    geometry_value: &geometry::T,
) -> Result<T, error::T> {
    let count = geometry::point_count(geometry_value);
    if let Some(group_value) = selection {
        if group::owner(group_value) != group::Owner::Point || group::length(group_value) != count {
            return Err(failure(operation, format!("{operation} selection must be a matching point group")));
        }
    }
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(failure(
            operation,
            format!("{operation} tolerance must be finite and non-negative"),
        ));
    }
    cancel::check_opt(cancel_value)?;
    if count == 0 {
        return Ok(T::Identity);
    }
    // ponytail: OCaml also rejects count > Sys.max_array_length / 2; unreachable for a Vec index here.
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    let (px_all, py_all, pz_all) = (&positions.x, &positions.y, &positions.z);
    let count_usize = count as usize;
    let mut min = [f64::INFINITY; 3];
    let mut max = [f64::NEG_INFINITY; 3];
    let mut non_finite = -1_i64;
    for point in 0..count_usize {
        cancel::poll(cancel_value, point as i64, 4095)?;
        let p = [px_all[point], py_all[point], pz_all[point]];
        if !(p[0].is_finite() && p[1].is_finite() && p[2].is_finite()) {
            non_finite = point as i64;
        } else {
            for axis in 0..3 {
                if p[axis] < min[axis] {
                    min[axis] = p[axis];
                }
                if p[axis] > max[axis] {
                    max[axis] = p[axis];
                }
            }
        }
    }
    if non_finite >= 0 {
        return Err(failure(
            operation,
            format!("{operation} point {non_finite} has a non-finite position"),
        ));
    }
    let exact = tolerance == 0.0;
    let scaled_delta = |left: f64, right: f64| -> f64 {
        let delta = right - left;
        if delta.is_finite() {
            delta / tolerance
        } else {
            (right / tolerance) - (left / tolerance)
        }
    };
    let largest_cell = if exact {
        0.0
    } else {
        let x = scaled_delta(min[0], max[0]);
        let y = scaled_delta(min[1], max[1]);
        let z = scaled_delta(min[2], max[2]);
        if x >= y {
            if x >= z { x } else { z }
        } else if y >= z {
            y
        } else {
            z
        }
    };
    if !largest_cell.is_finite() || largest_cell > ((i64::MAX >> 1) / 4) as f64 {
        return Err(failure(
            operation,
            format!("{operation} tolerance is too small for the geometry extent"),
        ));
    }
    let mut grid = point_grid::create(count, count);
    let mut representative: Vec<i64> = (0..count).collect();
    let neighbor_delta: i64 = if exact { 0 } else { 1 };
    let cell_coord = |value: f64, minimum: f64| -> i64 {
        if exact {
            point_grid::float_cell(value)
        } else {
            scaled_delta(minimum, value).floor() as i64
        }
    };
    for point in 0..count_usize {
        cancel::poll(cancel_value, point as i64, 4095)?;
        if let Some(group_value) = selection {
            if !group::mem(point as i64, group_value) {
                continue;
            }
        }
        let (x, y, z) = (px_all[point], py_all[point], pz_all[point]);
        let cx = cell_coord(x, min[0]);
        let cy = cell_coord(y, min[1]);
        let cz = cell_coord(z, min[2]);
        let mut nearest = -1_i64;
        for dx in -neighbor_delta..=neighbor_delta {
            for dy in -neighbor_delta..=neighbor_delta {
                for dz in -neighbor_delta..=neighbor_delta {
                    let cell = point_grid::find(&grid, cx + dx, cy + dy, cz + dz);
                    if cell < 0 {
                        continue;
                    }
                    let mut candidate = grid.cell_head[cell as usize];
                    while candidate >= 0 {
                        let other = candidate;
                        let o = other as usize;
                        let ddx = x - px_all[o];
                        let ddy = y - py_all[o];
                        let ddz = z - pz_all[o];
                        let position_matches = if exact {
                            ddx == 0.0 && ddy == 0.0 && ddz == 0.0
                        } else {
                            match metric {
                                Metric::Euclidean => {
                                    let scale = ddx.abs().max(ddy.abs().max(ddz.abs()));
                                    if scale == 0.0 {
                                        true
                                    } else if scale > tolerance {
                                        false
                                    } else {
                                        let nx = ddx / scale;
                                        let ny = ddy / scale;
                                        let nz = ddz / scale;
                                        let ratio = scale / tolerance;
                                        let normalized = ratio * ratio * ((nx * nx) + (ny * ny) + (nz * nz));
                                        if inclusive { normalized <= 1.0 } else { normalized < 1.0 }
                                    }
                                }
                                Metric::Componentwise => {
                                    if inclusive {
                                        ddx.abs() <= tolerance && ddy.abs() <= tolerance && ddz.abs() <= tolerance
                                    } else {
                                        ddx.abs() < tolerance && ddy.abs() < tolerance && ddz.abs() < tolerance
                                    }
                                }
                            }
                        };
                        if position_matches && compatible(point as i64, other) && (nearest < 0 || other < nearest) {
                            nearest = other;
                        }
                        candidate = grid.point_next[o];
                    }
                }
            }
        }
        if nearest >= 0 {
            representative[point] = nearest;
        } else {
            point_grid::insert(&mut grid, cx, cy, cz, point as i64);
        }
    }
    let mut representative_to_cluster = vec![-1_i64; count_usize];
    let mut cluster_count = 0_usize;
    for point in 0..count_usize {
        let root_point = representative[point] as usize;
        if representative_to_cluster[root_point] < 0 {
            representative_to_cluster[root_point] = cluster_count as i64;
            cluster_count += 1;
        }
        representative[point] = representative_to_cluster[root_point];
    }
    if cluster_count == count_usize {
        return Ok(T::Identity);
    }
    Ok(T::Clusters(pack(representative, cluster_count)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn links_merge_to_the_smaller_root_and_number_clusters_by_first_member() {
        assert_eq!(of_links(None, "op", &[-1, 1, -1]).unwrap(), T::Identity);
        let result = of_links(None, "op", &[-1, 0, -1, 2, 1]).unwrap();
        let T::Clusters(clusters) = result else { panic!("expected clusters") };
        assert_eq!(clusters.count, 2);
        assert_eq!(clusters.of_point, vec![0, 0, 1, 1, 0]);
        assert_eq!(clusters.offsets, vec![0, 3, 5]);
        assert_eq!(clusters.members, vec![0, 1, 4, 2, 3]);
        assert_eq!(clusters.representatives, vec![0, 2]);
        let bad = of_links(None, "op", &[5]).unwrap_err();
        assert_eq!(error::message(&bad), "op target point 5 at query 0 is out of range");
    }
}
