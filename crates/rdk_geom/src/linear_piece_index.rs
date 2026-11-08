//! Packed BVH over the linear pieces accepted by Intersection Analysis.

use std::sync::atomic::{AtomicI64, Ordering};

use rdk_core::{cancel, error, geometry, group, packed, support, topology};
use rdk_core::error::Code;

use crate::bounds3_index;
use crate::point_grid::{float_max, float_min};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Triangle,
    Segment,
}

pub struct T {
    positions: packed::Float3::View::View,
    topology: topology::View::View,
    kinds: Vec<u8>,
    primitives: Vec<i64>,
    locals: Vec<i64>,
    vertex_a: Vec<i64>,
    vertex_b: Vec<i64>,
    vertex_c: Vec<i64>,
    u0: Vec<f64>,
    u1: Vec<f64>,
    piece_min_x: Vec<f64>,
    piece_min_y: Vec<f64>,
    piece_min_z: Vec<f64>,
    piece_max_x: Vec<f64>,
    piece_max_y: Vec<f64>,
    piece_max_z: Vec<f64>,
    order: Vec<i64>,
    min_x: Vec<f64>,
    min_y: Vec<f64>,
    min_z: Vec<f64>,
    max_x: Vec<f64>,
    max_y: Vec<f64>,
    max_z: Vec<f64>,
    left: Vec<i64>,
    right: Vec<i64>,
    first: Vec<i64>,
    count: Vec<i64>,
}

const OPERATION: &str = "linear_piece_index";

fn fail(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

/// A cancellation takes this operation's name; any other failure passes through.
pub(crate) fn remap(failure: error::T, operation: &str) -> error::T {
    if error::is_cancelled(&failure) { error::cancelled(operation) } else { failure }
}

/// Ascending members of a group (`Group.iter` order).
pub(crate) fn members(value: &group::T) -> Vec<i64> {
    (0..group::length(value)).filter(|&index| group::mem(index, value)).collect()
}

/// Concatenates per-range `(left, right)` pair buffers in range order.
pub(crate) fn join_pairs(parts: Vec<(Vec<i64>, Vec<i64>)>) -> (Vec<i64>, Vec<i64>) {
    let total = parts.iter().map(|(left, _)| left.len()).sum();
    let mut left = Vec::with_capacity(total);
    let mut right = Vec::with_capacity(total);
    for (range_left, range_right) in parts {
        left.extend(range_left);
        right.extend(range_right);
    }
    (left, right)
}

pub(crate) fn plane_windows<'a>(planes: &'a mut [Vec<f64>], grain: i64) -> Vec<support::Windows<'a, f64>> {
    planes.iter_mut().map(|plane| support::Windows::even(plane, grain)).collect()
}

pub(crate) fn take_planes<'a>(windows: &[support::Windows<'a, f64>], range: i64) -> Vec<&'a mut [f64]> {
    windows.iter().map(|window| window.take(range)).collect()
}

struct Pieces {
    kinds: Vec<u8>,
    primitives: Vec<i64>,
    locals: Vec<i64>,
    vertex_a: Vec<i64>,
    vertex_b: Vec<i64>,
    vertex_c: Vec<i64>,
    u0: Vec<f64>,
    u1: Vec<f64>,
}

struct Bounds {
    centroid: [Vec<f64>; 3],
    min: [Vec<f64>; 3],
    max: [Vec<f64>; 3],
}

fn create_inner(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives_group: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<T, error::T> {
    if grain <= 0 {
        return Err(fail(Code::InvalidParameter, "Linear_piece_index: grain must be positive".to_owned()));
    }
    cancel::check_opt(cancel_value)?;
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let topology_view = topology::View::view(geometry::topology(geometry_value))?;
    let primitive_count = topology_view.primitive_kinds.len() as i64;
    let selected: Vec<i64> = match primitives_group {
        None => (0..primitive_count).collect(),
        Some(selection) => {
            if group::owner(selection) != group::Owner::Primitive {
                return Err(fail(Code::InvalidGroup, "selection must be a primitive group".to_owned()));
            }
            if group::length(selection) != primitive_count {
                return Err(fail(Code::InvalidGroup, "primitive group length does not match geometry".to_owned()));
            }
            members(selection)
        }
    };
    let selected_count = selected.len();
    let mut piece_offsets = vec![0_i64; selected_count + 1];
    for slot in 0..selected_count {
        cancel::poll(cancel_value, slot as i64, 4095)?;
        let primitive = selected[slot];
        let first = topology_view.primitive_offsets[primitive as usize];
        let last = topology_view.primitive_offsets[primitive as usize + 1];
        let size = last - first;
        let pieces = match topology_view.primitive_kinds[primitive as usize] {
            0 => {
                if size != 3 {
                    return Err(fail(
                        Code::InvalidSurface,
                        format!("primitive {primitive} is a polygon with {size} corners, not a triangle"),
                    ));
                }
                1
            }
            1 => size - 1,
            2 => size,
            _ => return Err(fail(Code::InvalidSurface, format!("primitive {primitive} has an invalid kind"))),
        };
        piece_offsets[slot + 1] = piece_offsets[slot] + pieces;
    }
    let piece_count = piece_offsets[selected_count];
    let average = if selected_count == 0 {
        1
    } else {
        support::ceiling_div(piece_count, selected_count as i64)?.max(1)
    };
    let chunk = (grain / average).max(1);
    let total = piece_count as usize;
    let mut pieces = Pieces {
        kinds: vec![0; total],
        primitives: vec![0; total],
        locals: vec![0; total],
        vertex_a: vec![0; total],
        vertex_b: vec![0; total],
        vertex_c: vec![-1; total],
        u0: vec![0.0; total],
        u1: vec![0.0; total],
    };
    let selected_ranges = support::range_count(selected_count as i64, chunk);
    let selected_bound = |range: i64| piece_offsets[(range.saturating_mul(chunk) as usize).min(selected_count)] as usize;
    let Pieces { kinds, primitives, locals, vertex_a, vertex_b, vertex_c, u0, u1 } = &mut pieces;
    let kind_windows = support::Windows::new(kinds, selected_ranges, &selected_bound);
    let primitive_windows = support::Windows::new(primitives, selected_ranges, &selected_bound);
    let local_windows = support::Windows::new(locals, selected_ranges, &selected_bound);
    let a_windows = support::Windows::new(vertex_a, selected_ranges, &selected_bound);
    let b_windows = support::Windows::new(vertex_b, selected_ranges, &selected_bound);
    let c_windows = support::Windows::new(vertex_c, selected_ranges, &selected_bound);
    let u0_windows = support::Windows::new(u0, selected_ranges, &selected_bound);
    let u1_windows = support::Windows::new(u1, selected_ranges, &selected_bound);
    support::run_indexed_ranges(chunk, selected_count as i64, &|range, first_selected, last_selected| {
        let (kinds, primitives, locals) =
            (kind_windows.take(range), primitive_windows.take(range), local_windows.take(range));
        let (vertex_a, vertex_b, vertex_c) = (a_windows.take(range), b_windows.take(range), c_windows.take(range));
        let (u0, u1) = (u0_windows.take(range), u1_windows.take(range));
        let base = piece_offsets[first_selected as usize];
        for slot in first_selected..last_selected {
            cancel::poll(cancel_value, slot, 4095)?;
            let primitive = selected[slot as usize];
            let output = (piece_offsets[slot as usize] - base) as usize;
            let first = topology_view.primitive_offsets[primitive as usize];
            let last = topology_view.primitive_offsets[primitive as usize + 1];
            let size = last - first;
            let code = topology_view.primitive_kinds[primitive as usize];
            if code == 0 {
                primitives[output] = primitive;
                vertex_a[output] = first;
                vertex_b[output] = first + 1;
                vertex_c[output] = first + 2;
            } else {
                let segment_count = if code == 1 { size - 1 } else { size };
                for at in 0..segment_count {
                    let piece = output + at as usize;
                    kinds[piece] = 1;
                    primitives[piece] = primitive;
                    locals[piece] = at;
                    vertex_a[piece] = first + at;
                    vertex_b[piece] = if at + 1 < size { first + at + 1 } else { first };
                    u0[piece] = at as f64 / segment_count as f64;
                    u1[piece] = (at + 1) as f64 / segment_count as f64;
                }
            }
        }
        Ok(())
    })?;
    let mut bounds = Bounds {
        centroid: std::array::from_fn(|_| vec![0.0; total]),
        min: std::array::from_fn(|_| vec![0.0; total]),
        max: std::array::from_fn(|_| vec![0.0; total]),
    };
    let mut failures: Vec<Option<String>> = vec![None; support::range_count(piece_count, grain) as usize];
    let vertex_points = &topology_view.vertex_points;
    let Bounds { centroid, min, max } = &mut bounds;
    let (centroid_windows, min_windows, max_windows) =
        (plane_windows(centroid, grain), plane_windows(min, grain), plane_windows(max, grain));
    let failure_windows = support::Windows::even(&mut failures, 1);
    support::run_indexed_ranges(grain, piece_count, &|range, first_piece, last_piece| {
        let (mut centroid, mut min, mut max) = (
            take_planes(&centroid_windows, range),
            take_planes(&min_windows, range),
            take_planes(&max_windows, range),
        );
        let failure = &mut failure_windows.take(range)[0];
        for piece in first_piece..last_piece {
            cancel::poll(cancel_value, piece, 4095)?;
            if failure.is_some() {
                continue;
            }
            let p = piece as usize;
            let at = p - first_piece as usize;
            let primitive = pieces.primitives[p];
            let pa = vertex_points[pieces.vertex_a[p] as usize] as usize;
            let pb = vertex_points[pieces.vertex_b[p] as usize] as usize;
            let (ax, ay, az) = (positions.x[pa], positions.y[pa], positions.z[pa]);
            let (bx, by, bz) = (positions.x[pb], positions.y[pb], positions.z[pb]);
            if !(ax.is_finite() && ay.is_finite() && az.is_finite() && bx.is_finite() && by.is_finite() && bz.is_finite()) {
                *failure = Some(format!("primitive {primitive} has a non-finite position"));
            } else if pieces.kinds[p] == 1 {
                if ax == bx && ay == by && az == bz {
                    *failure = Some(format!(
                        "primitive {primitive} segment {} is degenerate",
                        pieces.locals[p]
                    ));
                } else {
                    centroid[0][at] = (ax * 0.5) + (bx * 0.5);
                    centroid[1][at] = (ay * 0.5) + (by * 0.5);
                    centroid[2][at] = (az * 0.5) + (bz * 0.5);
                    min[0][at] = float_min(ax, bx);
                    min[1][at] = float_min(ay, by);
                    min[2][at] = float_min(az, bz);
                    max[0][at] = float_max(ax, bx);
                    max[1][at] = float_max(ay, by);
                    max[2][at] = float_max(az, bz);
                }
            } else {
                let pc = vertex_points[pieces.vertex_c[p] as usize] as usize;
                let (cx, cy, cz) = (positions.x[pc], positions.y[pc], positions.z[pc]);
                if !(cx.is_finite() && cy.is_finite() && cz.is_finite()) {
                    *failure = Some(format!("primitive {primitive} has a non-finite position"));
                } else {
                    let abx = bx - ax;
                    let aby = by - ay;
                    let abz = bz - az;
                    let acx = cx - ax;
                    let acy = cy - ay;
                    let acz = cz - az;
                    let nx = (aby * acz) - (abz * acy);
                    let ny = (abz * acx) - (abx * acz);
                    let nz = (abx * acy) - (aby * acx);
                    let area2 = (nx * nx) + (ny * ny) + (nz * nz);
                    if !area2.is_finite() || area2 <= 1e-30 {
                        *failure = Some(format!("primitive {primitive} is degenerate"));
                    } else {
                        centroid[0][at] = (ax + bx + cx) / 3.0;
                        centroid[1][at] = (ay + by + cy) / 3.0;
                        centroid[2][at] = (az + bz + cz) / 3.0;
                        min[0][at] = float_min(ax, float_min(bx, cx));
                        min[1][at] = float_min(ay, float_min(by, cy));
                        min[2][at] = float_min(az, float_min(bz, cz));
                        max[0][at] = float_max(ax, float_max(bx, cx));
                        max[1][at] = float_max(ay, float_max(by, cy));
                        max[2][at] = float_max(az, float_max(bz, cz));
                    }
                }
            }
        }
        Ok(())
    })?;
    if let Some(message) = failures.into_iter().flatten().next() {
        return Err(fail(Code::InvalidSurface, message));
    }
    let tree = bounds3_index::create(
        cancel_value,
        None,
        grain,
        &bounds.centroid[0],
        &bounds.centroid[1],
        &bounds.centroid[2],
        &bounds.min[0],
        &bounds.min[1],
        &bounds.min[2],
        &bounds.max[0],
        &bounds.max[1],
        &bounds.max[2],
    )?;
    let [min_x, min_y, min_z] = bounds.min;
    let [max_x, max_y, max_z] = bounds.max;
    Ok(T {
        positions,
        topology: topology_view,
        kinds: pieces.kinds,
        primitives: pieces.primitives,
        locals: pieces.locals,
        vertex_a: pieces.vertex_a,
        vertex_b: pieces.vertex_b,
        vertex_c: pieces.vertex_c,
        u0: pieces.u0,
        u1: pieces.u1,
        piece_min_x: min_x,
        piece_min_y: min_y,
        piece_min_z: min_z,
        piece_max_x: max_x,
        piece_max_y: max_y,
        piece_max_z: max_z,
        order: tree.order,
        min_x: tree.min_x,
        min_y: tree.min_y,
        min_z: tree.min_z,
        max_x: tree.max_x,
        max_y: tree.max_y,
        max_z: tree.max_z,
        left: tree.left,
        right: tree.right,
        first: tree.first,
        count: tree.count,
    })
}

/// Selected polygon primitives must be triangles. Open and closed polygon curves are expanded into
/// stable segment pieces without materializing new geometry. Referenced positions must be finite,
/// triangles non-degenerate, and segments non-zero. `grain` defaults to 16384 in OCaml.
pub fn create(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    primitives: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<T, error::T> {
    create_inner(cancel_value, grain.unwrap_or(16_384), primitives, geometry_value)
        .map_err(|failure| remap(failure, OPERATION))
}

pub fn piece_count(value: &T) -> i64 {
    value.primitives.len() as i64
}

#[inline]
fn bounds_overlap(tolerance: f64, left: &T, right_index: &T, node: usize, left_piece: usize) -> bool {
    left.piece_min_x[left_piece] <= right_index.max_x[node] + tolerance
        && left.piece_max_x[left_piece] >= right_index.min_x[node] - tolerance
        && left.piece_min_y[left_piece] <= right_index.max_y[node] + tolerance
        && left.piece_max_y[left_piece] >= right_index.min_y[node] - tolerance
        && left.piece_min_z[left_piece] <= right_index.max_z[node] + tolerance
        && left.piece_max_z[left_piece] >= right_index.min_z[node] - tolerance
}

#[inline]
fn piece_bounds_overlap(tolerance: f64, left: &T, left_piece: usize, right: &T, right_piece: usize) -> bool {
    left.piece_min_x[left_piece] <= right.piece_max_x[right_piece] + tolerance
        && left.piece_max_x[left_piece] >= right.piece_min_x[right_piece] - tolerance
        && left.piece_min_y[left_piece] <= right.piece_max_y[right_piece] + tolerance
        && left.piece_max_y[left_piece] >= right.piece_min_y[right_piece] - tolerance
        && left.piece_min_z[left_piece] <= right.piece_max_z[right_piece] + tolerance
        && left.piece_max_z[left_piece] >= right.piece_min_z[right_piece] - tolerance
}

fn invalid_argument(message: &str) -> error::T {
    error::new(OPERATION, Code::InvalidParameter, message.to_owned())
}

fn pairs_of(
    cancel_value: Option<&cancel::T>,
    is_self: bool,
    grain: i64,
    tolerance: f64,
    left_index: &T,
    right_index: &T,
) -> Result<(Vec<i64>, Vec<i64>), error::T> {
    if grain <= 0 {
        return Err(invalid_argument("Linear_piece_index.overlapping_pairs: grain must be positive"));
    }
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(invalid_argument(
            "Linear_piece_index.overlapping_pairs: tolerance must be finite and non-negative",
        ));
    }
    let left_count = piece_count(left_index);
    if left_count == 0 || piece_count(right_index) == 0 {
        return Ok((Vec::new(), Vec::new()));
    }
    let ranges = support::range_count(left_count, grain) as usize;
    // ponytail: OCaml counts then fills (two traversals) to size output exactly; one pass into
    // per-range buffers joined in range order yields the same bytes.
    let mut parts = vec![(Vec::new(), Vec::new()); ranges];
    let part_windows = support::Windows::even(&mut parts, 1);
    support::run_indexed_ranges(grain, left_count, &|range, first, last| {
        let mut stack = [0_i64; 65];
        let mut out_left: Vec<i64> = Vec::new();
        let mut out_right: Vec<i64> = Vec::new();
        for piece in first..last {
            cancel::poll(cancel_value, piece, 4095)?;
            let left_piece = piece as usize;
            let mut top = 1_usize;
            stack[0] = 0;
            while top > 0 {
                top -= 1;
                let node = stack[top] as usize;
                if bounds_overlap(tolerance, left_index, right_index, node, left_piece) {
                    if right_index.count[node] > 0 {
                        let start = right_index.first[node];
                        let last_slot = start + right_index.count[node];
                        for slot in start..last_slot {
                            let right_piece = right_index.order[slot as usize];
                            if (!is_self || right_piece > piece)
                                && piece_bounds_overlap(tolerance, left_index, left_piece, right_index, right_piece as usize)
                            {
                                out_left.push(piece);
                                out_right.push(right_piece);
                            }
                        }
                    } else {
                        stack[top] = right_index.right[node];
                        stack[top + 1] = right_index.left[node];
                        top += 2;
                    }
                }
            }
        }
        part_windows.take(range)[0] = (out_left, out_right);
        Ok(())
    })?;
    Ok(join_pairs(parts))
}

fn self_pair_of(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    tolerance: f64,
    index: &T,
    predicate: &(dyn Fn(i64, i64) -> bool + Sync),
) -> Result<Option<(i64, i64)>, error::T> {
    if grain <= 0 {
        return Err(invalid_argument("Linear_piece_index.find_overlapping_self_pair: grain must be positive"));
    }
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(invalid_argument(
            "Linear_piece_index.find_overlapping_self_pair: tolerance must be finite and non-negative",
        ));
    }
    let pieces = piece_count(index);
    if pieces == 0 {
        return Ok(None);
    }
    let ranges = support::ceiling_div(pieces, grain)?;
    let found_left: Vec<AtomicI64> = (0..ranges).map(|_| AtomicI64::new(-1)).collect();
    let found_right: Vec<AtomicI64> = (0..ranges).map(|_| AtomicI64::new(-1)).collect();
    support::parallel_for(cancel_value, 1, ranges, &|range| {
        let mut stack = [0_i64; 65];
        let last_piece = pieces.min((range + 1) * grain);
        let mut left_piece = range * grain;
        while found_left[range as usize].load(Ordering::Relaxed) < 0 && left_piece < last_piece {
            cancel::poll(cancel_value, left_piece, 4095)?;
            let mut top = 1_usize;
            let mut best_right = -1_i64;
            stack[0] = 0;
            while top > 0 {
                top -= 1;
                let node = stack[top];
                cancel::poll(cancel_value, node, 4095)?;
                let node = node as usize;
                if bounds_overlap(tolerance, index, index, node, left_piece as usize) {
                    if index.count[node] > 0 {
                        let start = index.first[node];
                        let last_slot = start + index.count[node];
                        for slot in start..last_slot {
                            let right_piece = index.order[slot as usize];
                            if right_piece > left_piece
                                && (right_piece < best_right || best_right < 0)
                                && piece_bounds_overlap(tolerance, index, left_piece as usize, index, right_piece as usize)
                                && predicate(left_piece, right_piece)
                            {
                                best_right = right_piece;
                            }
                        }
                    } else {
                        stack[top] = index.right[node];
                        stack[top + 1] = index.left[node];
                        top += 2;
                    }
                }
            }
            if best_right >= 0 {
                found_left[range as usize].store(left_piece, Ordering::Relaxed);
                found_right[range as usize].store(best_right, Ordering::Relaxed);
            }
            left_piece += 1;
        }
        Ok(())
    })?;
    for range in 0..ranges as usize {
        let left = found_left[range].load(Ordering::Relaxed);
        if left >= 0 {
            return Ok(Some((left, found_right[range].load(Ordering::Relaxed))));
        }
    }
    Ok(None)
}

#[allow(non_snake_case)]
pub mod view {
    use super::*;

    pub fn kind(value: &T, piece: i64) -> Kind {
        if value.kinds[piece as usize] == 0 { Kind::Triangle } else { Kind::Segment }
    }

    pub fn primitive(value: &T, piece: i64) -> i64 {
        value.primitives[piece as usize]
    }

    pub fn local(value: &T, piece: i64) -> i64 {
        value.locals[piece as usize]
    }

    /// Source vertex of local corner 0, 1 or 2 (2 only for triangles).
    pub fn vertex(value: &T, piece: i64, corner: i64) -> Result<i64, error::T> {
        let p = piece as usize;
        match corner {
            0 => Ok(value.vertex_a[p]),
            1 => Ok(value.vertex_b[p]),
            2 if value.kinds[p] == 0 => Ok(value.vertex_c[p]),
            _ => Err(invalid_argument("Linear_piece_index.vertex: invalid local corner")),
        }
    }

    /// Original source point number for a piece corner.
    pub fn point(value: &T, piece: i64, corner: i64) -> Result<i64, error::T> {
        Ok(value.topology.vertex_points[vertex(value, piece, corner)? as usize])
    }

    /// Global polygon-curve parameter at segment endpoint 0 or 1.
    pub fn curve_u(value: &T, piece: i64, endpoint: i64) -> Result<f64, error::T> {
        match endpoint {
            0 => Ok(value.u0[piece as usize]),
            1 => Ok(value.u1[piece as usize]),
            _ => Err(invalid_argument("Linear_piece_index.curve_u: invalid endpoint")),
        }
    }

    pub fn positions(value: &T) -> &packed::Float3::View::View {
        &value.positions
    }

    pub fn overlapping_pairs(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        tolerance: f64,
        left: &T,
        right: &T,
    ) -> Result<(Vec<i64>, Vec<i64>), error::T> {
        pairs_of(cancel_value, false, grain, tolerance, left, right)
    }

    /// Each unordered piece pair is returned once. Different pieces from one curve primitive remain
    /// eligible because they can genuinely self-intersect.
    pub fn overlapping_self_pairs(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        tolerance: f64,
        value: &T,
    ) -> Result<(Vec<i64>, Vec<i64>), error::T> {
        pairs_of(cancel_value, true, grain, tolerance, value, value)
    }

    /// The lexicographically first unordered AABB-overlapping pair for which the pure predicate
    /// returns true; domain-count independent.
    pub fn find_overlapping_self_pair(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        tolerance: f64,
        value: &T,
        predicate: &(dyn Fn(i64, i64) -> bool + Sync),
    ) -> Result<Option<(i64, i64)>, error::T> {
        self_pair_of(cancel_value, grain, tolerance, value, predicate)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn square_and_polyline() -> geometry::T {
        // triangle (0,1,2) and an open polyline (3,4,5) crossing it
        let x = vec![0.0, 4.0, 0.0, 1.0, 1.0, 1.0];
        let y = vec![0.0, 0.0, 4.0, 1.0, 1.0, 1.0];
        let z = vec![0.0, 0.0, 0.0, -1.0, 1.0, 3.0];
        let positions = packed::Float3::View::of_owned_exn(x, y, z).unwrap();
        let topo = topology::View::create_validated_owned(6, vec![0, 1, 2, 3, 4, 5], vec![0, 3, 6], vec![0, 1]);
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    #[test]
    fn pieces_pairs_and_first_self_pair() {
        let geom = square_and_polyline();
        let index = create(None, Some(1), None, &geom).unwrap();
        assert_eq!(piece_count(&index), 3);
        assert_eq!(view::kind(&index, 0), Kind::Triangle);
        assert_eq!(view::kind(&index, 1), Kind::Segment);
        assert_eq!(view::primitive(&index, 2), 1);
        assert_eq!(view::local(&index, 2), 1);
        assert_eq!(view::curve_u(&index, 2, 0).unwrap(), 0.5);
        assert_eq!(view::point(&index, 2, 1).unwrap(), 5);
        assert!(view::vertex(&index, 1, 2).is_err());
        let (left, right) = view::overlapping_self_pairs(None, 1, 0.0, &index).unwrap();
        assert_eq!((left.clone(), right.clone()), (vec![0, 1], vec![1, 2]));
        let (sequential, _) = view::overlapping_self_pairs(None, 100, 0.0, &index).unwrap();
        assert_eq!(sequential, left);
        let found = view::find_overlapping_self_pair(None, 1, 0.0, &index, &|a, b| a == 1 && b == 2).unwrap();
        assert_eq!(found, Some((1, 2)));
        let none = view::find_overlapping_self_pair(None, 1, 0.0, &index, &|_, _| false).unwrap();
        assert_eq!(none, None);
    }

    #[test]
    fn degenerate_segment_is_reported() {
        let positions = packed::Float3::View::of_owned_exn(vec![0.0, 0.0], vec![0.0, 0.0], vec![0.0, 0.0]).unwrap();
        let topo = topology::View::create_validated_owned(2, vec![0, 1], vec![0, 2], vec![1]);
        let geom = geometry::create(positions, topo, None, None, None).unwrap();
        let failure = create(None, None, None, &geom).err().unwrap();
        assert_eq!(error::code(&failure), Code::InvalidSurface);
        assert_eq!(error::message(&failure), "primitive 0 segment 0 is degenerate");
    }
}
