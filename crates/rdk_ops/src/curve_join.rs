//! Curve Join: welds selected open polyline curves into chains.
//!
//! OCaml returns `(Geometry.t, string) result` and lets `Cancel.Cancelled` escape; here a failure is
//! an `error::T` with operation `Curve_join.join`, code `invalid_parameter` and the OCaml message,
//! and a cancellation keeps code `cancelled`.
use std::cmp::Ordering;
use std::sync::Arc;

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, support, topology,
    topology_index, topology_remap,
};
use rdk_core::error::Code;

const OPEN_POLYLINE: u8 = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CurveJoinEnd {
    JoinCurveStart,
    JoinCurveEnd,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CurveJoinPick {
    pub primitive: i64,
    pub end_: CurveJoinEnd,
}

fn fail(message: String) -> error::T {
    error::new("Curve_join.join", Code::InvalidParameter, message)
}

fn validate_selection(
    operation: &str,
    selection: Option<&group::T>,
    primitive_count: i64,
) -> Result<(), error::T> {
    if let Some(value) = selection {
        if group::owner(value) != group::Owner::Primitive {
            return Err(fail(format!("{operation} selection must own primitives")));
        }
        if group::length(value) != primitive_count {
            return Err(fail(format!(
                "{operation} selection length does not match primitive count"
            )));
        }
    }
    Ok(())
}

fn remap_attribute(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    mapping: &[i64],
    value: &attribute::T,
) -> Result<attribute::T, error::T> {
    topology_remap::attribute(cancel_value, grain.unwrap_or(16_384), None, mapping, value)
}

/// Shared packed linear interpolation for point/vertex topology generators. Integer, text, and
/// ragged payload use the nearest endpoint.
pub fn interpolate_attribute(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    point_left: &[i64],
    point_right: &[i64],
    point_weight: &[f64],
    vertex_left: &[i64],
    vertex_right: &[i64],
    vertex_weight: &[f64],
    value: &attribute::T,
) -> Result<attribute::T, error::T> {
    let (left, right, weight) = match attribute::owner(value) {
        attribute::Owner::Point => (point_left, point_right, point_weight),
        attribute::Owner::Vertex => (vertex_left, vertex_right, vertex_weight),
        attribute::Owner::Primitive | attribute::Owner::Detail => return Ok(value.clone()),
    };
    let grain = support::resolve(grain).map_err(|_| fail("grain must be positive".to_owned()))?;
    topology_remap::blend_attribute(cancel_value, grain, left, right, weight, value)
}

struct EndpointTree {
    endpoint_ids: Vec<i64>,
    endpoint_node: Vec<i64>,
    endpoint_x: Vec<f64>,
    endpoint_y: Vec<f64>,
    endpoint_z: Vec<f64>,
    left: Vec<i64>,
    right: Vec<i64>,
    parent: Vec<i64>,
    axis: Vec<u8>,
    active: Vec<bool>,
    active_count: Vec<i64>,
    nearest_endpoint: i64,
    nearest_distance: f64,
    root: i64,
}

fn robust_distance_xyz(ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64) -> f64 {
    let (dx, dy, dz) = (ax - bx, ay - by, az - bz);
    dx.hypot(dy).hypot(dz)
}

/// OCaml `Float.compare`: NaN equals itself and sorts below every other float.
fn float_compare(a: f64, b: f64) -> Ordering {
    if a < b {
        Ordering::Less
    } else if a > b {
        Ordering::Greater
    } else if a == b {
        Ordering::Equal
    } else if a.is_nan() {
        if b.is_nan() { Ordering::Equal } else { Ordering::Less }
    } else {
        Ordering::Greater
    }
}

impl EndpointTree {
    fn compare(&self, axis: u8, left: i64, right: i64) -> Ordering {
        let (l, r) = (left as usize, right as usize);
        let order = match axis {
            0 => float_compare(self.endpoint_x[l], self.endpoint_x[r]),
            1 => float_compare(self.endpoint_y[l], self.endpoint_y[r]),
            _ => float_compare(self.endpoint_z[l], self.endpoint_z[r]),
        };
        if order != Ordering::Equal { order } else { left.cmp(&right) }
    }

    fn swap(&mut self, left: i64, right: i64) {
        self.endpoint_ids.swap(left as usize, right as usize);
    }

    fn median_of_three(&self, axis: u8, first: i64, middle: i64, last: i64) -> i64 {
        let (a, b, c) = (
            self.endpoint_ids[first as usize],
            self.endpoint_ids[middle as usize],
            self.endpoint_ids[last as usize],
        );
        if self.compare(axis, a, b) != Ordering::Greater {
            if self.compare(axis, b, c) != Ordering::Greater {
                middle
            } else if self.compare(axis, a, c) != Ordering::Greater {
                last
            } else {
                first
            }
        } else if self.compare(axis, a, c) != Ordering::Greater {
            first
        } else if self.compare(axis, b, c) != Ordering::Greater {
            last
        } else {
            middle
        }
    }

    fn partition(
        &mut self,
        cancel_value: Option<&cancel::T>,
        axis: u8,
        first: i64,
        last: i64,
        pivot_index: i64,
    ) -> Result<i64, error::T> {
        let pivot = self.endpoint_ids[pivot_index as usize];
        self.swap(pivot_index, last);
        let mut store = first;
        for index in first..last {
            cancel::poll(cancel_value, index, 16_383)?;
            if self.compare(axis, self.endpoint_ids[index as usize], pivot) == Ordering::Less {
                self.swap(index, store);
                store += 1;
            }
        }
        self.swap(store, last);
        Ok(store)
    }

    fn select(
        &mut self,
        cancel_value: Option<&cancel::T>,
        axis: u8,
        first: i64,
        last: i64,
        target: i64,
    ) -> Result<(), error::T> {
        let (mut first, mut last) = (first, last);
        while first < last {
            cancel::check_opt(cancel_value)?;
            let middle = first + ((last - first) >> 1);
            let pivot = self.median_of_three(axis, first, middle, last);
            let pivot = self.partition(cancel_value, axis, first, last, pivot)?;
            if target < pivot {
                last = pivot - 1;
            } else if target > pivot {
                first = pivot + 1;
            } else {
                first = target;
                last = target;
            }
        }
        Ok(())
    }

    fn choose_axis(&self, cancel_value: Option<&cancel::T>, first: i64, last: i64) -> Result<u8, error::T> {
        let endpoint = self.endpoint_ids[first as usize] as usize;
        let (mut min_x, mut max_x) = (self.endpoint_x[endpoint], self.endpoint_x[endpoint]);
        let (mut min_y, mut max_y) = (self.endpoint_y[endpoint], self.endpoint_y[endpoint]);
        let (mut min_z, mut max_z) = (self.endpoint_z[endpoint], self.endpoint_z[endpoint]);
        for index in first + 1..last {
            cancel::poll(cancel_value, index, 16_383)?;
            let endpoint = self.endpoint_ids[index as usize] as usize;
            let (x, y, z) = (self.endpoint_x[endpoint], self.endpoint_y[endpoint], self.endpoint_z[endpoint]);
            if x < min_x {
                min_x = x;
            }
            if x > max_x {
                max_x = x;
            }
            if y < min_y {
                min_y = y;
            }
            if y > max_y {
                max_y = y;
            }
            if z < min_z {
                min_z = z;
            }
            if z > max_z {
                max_z = z;
            }
        }
        let (x_span, y_span, z_span) = (max_x - min_x, max_y - min_y, max_z - min_z);
        Ok(if y_span > x_span {
            if z_span > y_span { 2 } else { 1 }
        } else if z_span > x_span {
            2
        } else {
            0
        })
    }

    fn build(
        &mut self,
        cancel_value: Option<&cancel::T>,
        first: i64,
        last: i64,
        parent_node: i64,
    ) -> Result<i64, error::T> {
        if first >= last {
            return Ok(-1);
        }
        let axis_value = self.choose_axis(cancel_value, first, last)?;
        let middle = first + ((last - first) >> 1);
        self.select(cancel_value, axis_value, first, last - 1, middle)?;
        let m = middle as usize;
        self.parent[m] = parent_node;
        self.axis[m] = axis_value;
        self.endpoint_node[self.endpoint_ids[m] as usize] = middle;
        let left_node = self.build(cancel_value, first, middle, middle)?;
        let right_node = self.build(cancel_value, middle + 1, last, middle)?;
        self.left[m] = left_node;
        self.right[m] = right_node;
        self.active_count[m] = 1
            + if left_node < 0 { 0 } else { self.active_count[left_node as usize] }
            + if right_node < 0 { 0 } else { self.active_count[right_node as usize] };
        Ok(middle)
    }

    fn create(
        cancel_value: Option<&cancel::T>,
        endpoint_x: Vec<f64>,
        endpoint_y: Vec<f64>,
        endpoint_z: Vec<f64>,
    ) -> Result<EndpointTree, error::T> {
        let count = endpoint_x.len();
        if endpoint_y.len() != count || endpoint_z.len() != count {
            return Err(fail("Curve Join endpoint coordinate length mismatch".to_owned()));
        }
        let mut tree = EndpointTree {
            endpoint_ids: (0..count as i64).collect(),
            endpoint_node: vec![-1; count],
            endpoint_x,
            endpoint_y,
            endpoint_z,
            left: vec![-1; count],
            right: vec![-1; count],
            parent: vec![-1; count],
            axis: vec![0; count],
            active: vec![true; count],
            active_count: vec![0; count],
            nearest_endpoint: -1,
            nearest_distance: f64::INFINITY,
            root: -1,
        };
        tree.root = tree.build(cancel_value, 0, count as i64, -1)?;
        Ok(tree)
    }

    fn remove(&mut self, endpoint: i64) {
        if self.active[endpoint as usize] {
            self.active[endpoint as usize] = false;
            let mut node = self.endpoint_node[endpoint as usize];
            while node >= 0 {
                self.active_count[node as usize] -= 1;
                node = self.parent[node as usize];
            }
        }
    }

    fn visit(
        &mut self,
        cancel_value: Option<&cancel::T>,
        query: (f64, f64, f64),
        node: i64,
        visited: &mut i64,
    ) -> Result<(), error::T> {
        if node >= 0 && self.active_count[node as usize] > 0 {
            *visited += 1;
            cancel::poll(cancel_value, *visited, 16_383)?;
            let endpoint = self.endpoint_ids[node as usize] as usize;
            let axis = self.axis[node as usize];
            let (qx, qy, qz) = query;
            let delta = match axis {
                0 => qx - self.endpoint_x[endpoint],
                1 => qy - self.endpoint_y[endpoint],
                _ => qz - self.endpoint_z[endpoint],
            };
            let (near, far) = if delta <= 0.0 {
                (self.left[node as usize], self.right[node as usize])
            } else {
                (self.right[node as usize], self.left[node as usize])
            };
            self.visit(cancel_value, query, near, visited)?;
            if self.active[endpoint] {
                let distance = robust_distance_xyz(
                    qx,
                    qy,
                    qz,
                    self.endpoint_x[endpoint],
                    self.endpoint_y[endpoint],
                    self.endpoint_z[endpoint],
                );
                if distance < self.nearest_distance
                    || (distance == self.nearest_distance
                        && (self.nearest_endpoint < 0 || (endpoint as i64) < self.nearest_endpoint))
                {
                    self.nearest_endpoint = endpoint as i64;
                    self.nearest_distance = distance;
                }
            }
            if delta.abs() <= self.nearest_distance {
                self.visit(cancel_value, query, far, visited)?;
            }
        }
        Ok(())
    }

    fn nearest(&mut self, cancel_value: Option<&cancel::T>, query_endpoint: i64) -> Result<(), error::T> {
        let q = query_endpoint as usize;
        let query = (self.endpoint_x[q], self.endpoint_y[q], self.endpoint_z[q]);
        self.nearest_endpoint = -1;
        self.nearest_distance = f64::INFINITY;
        let mut visited = 0;
        let root = self.root;
        self.visit(cancel_value, query, root, &mut visited)
    }
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub primitives: Option<group::T>,
    pub picked_ends: Option<Vec<CurveJoinPick>>,
    pub orient_closest: bool,
    pub connect_closest_ends: bool,
    pub only_connected: bool,
    pub group_size: Option<i64>,
    pub keep_originals: bool,
    pub tolerance: f64,
    pub wrap: bool,
}

/// Welds the selected open polyline curves of `geometry` into chains.
pub fn join(params: &Params, value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain;
    let tolerance = params.tolerance;
    let group_size = params.group_size;
    let picked_ends = params.picked_ends.as_deref();
    let primitives = params.primitives.as_ref();
    let (orient_closest, connect_closest_ends, only_connected, keep_originals, wrap) = (
        params.orient_closest,
        params.connect_closest_ends,
        params.only_connected,
        params.keep_originals,
        params.wrap,
    );
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(fail("Curve Join tolerance must be finite and non-negative".to_owned()));
    }
    if let Some(size) = group_size {
        if size <= 0 {
            return Err(fail("Curve Join group size must be positive".to_owned()));
        }
    }
    if primitives.is_some() && picked_ends.is_some() {
        return Err(fail(
            "Curve Join primitive selection and picked ends are mutually exclusive".to_owned(),
        ));
    }
    if picked_ends.is_some() && connect_closest_ends {
        return Err(fail(
            "Curve Join picked ends and Connect Closest Ends are mutually exclusive".to_owned(),
        ));
    }
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(value);
    let source = topology::view::view(topology_value)?;
    let positions = packed::float3::view::view(geometry::positions(value))?;
    let primitive_count = topology::primitive_count(topology_value);
    validate_selection("Curve Join", primitives, primitive_count)?;
    let selected_count = match picked_ends {
        Some(picks) => picks.len() as i64,
        None => match primitives {
            Some(selection) => group::cardinality(selection),
            None => primitive_count,
        },
    };
    if selected_count == 0 {
        return Ok(value.clone());
    }
    let sc = selected_count as usize;
    let mut selected_primitives = vec![0_i64; sc];
    let mut selected_index = vec![-1_i64; primitive_count as usize];
    match picked_ends {
        Some(picks) => {
            for (index, pick) in picks.iter().enumerate() {
                let primitive = pick.primitive;
                if primitive < 0 || primitive >= primitive_count {
                    return Err(fail(format!(
                        "Curve Join picked primitive {primitive} is out of bounds"
                    )));
                }
                if selected_index[primitive as usize] >= 0 {
                    return Err(fail(format!(
                        "Curve Join picked primitive {primitive} more than once"
                    )));
                }
                selected_index[primitive as usize] = index as i64;
                selected_primitives[index] = primitive;
            }
        }
        None => match primitives {
            Some(selection) => match group::view::order_view(selection) {
                Some(order) => selected_primitives[..sc].copy_from_slice(&order[..sc]),
                None => {
                    let mut at = 0;
                    for primitive in 0..group::length(selection) {
                        if group::mem(primitive, selection) {
                            selected_primitives[at] = primitive;
                            at += 1;
                        }
                    }
                }
            },
            None => {
                for (index, slot) in selected_primitives.iter_mut().enumerate() {
                    *slot = index as i64;
                }
            }
        },
    }
    for &primitive in &selected_primitives {
        if source.primitive_kinds[primitive as usize] != OPEN_POLYLINE {
            return Err(fail(format!(
                "Curve Join primitive {primitive} is not an open polygon curve"
            )));
        }
    }
    let mut reversed = vec![false; sc];
    let mut welded = vec![false; sc];
    let mut selected_chain = vec![0_i64; sc];
    let mut chain_count = 1_i64;
    let starts_subgroup = |index: i64| match group_size {
        Some(size) => index > 0 && index % size == 0,
        None => false,
    };
    let endpoint = |primitive: i64, reverse: bool, last: bool| -> i64 {
        let first = source.primitive_offsets[primitive as usize];
        let stop = source.primitive_offsets[primitive as usize + 1];
        let vertex = if last != reverse { stop - 1 } else { first };
        source.vertex_points[vertex as usize]
    };
    let distance = |left: i64, right: i64| -> Result<f64, error::T> {
        let (l, r) = (left as usize, right as usize);
        if !(positions.x[l].is_finite()
            && positions.y[l].is_finite()
            && positions.z[l].is_finite()
            && positions.x[r].is_finite()
            && positions.y[r].is_finite()
            && positions.z[r].is_finite())
        {
            return Err(fail("Curve Join requires finite selected endpoint positions".to_owned()));
        }
        Ok(robust_distance_xyz(
            positions.x[l],
            positions.y[l],
            positions.z[l],
            positions.x[r],
            positions.y[r],
            positions.z[r],
        ))
    };
    if let Some(picks) = picked_ends {
        let picked_reverse = |index: i64| -> bool {
            let subgroup = starts_subgroup(index);
            match picks[index as usize].end_ {
                CurveJoinEnd::JoinCurveStart => index == 0 || subgroup,
                CurveJoinEnd::JoinCurveEnd => index > 0 && !subgroup,
            }
        };
        if only_connected {
            for index in 0..selected_count {
                cancel::poll(cancel_value, index, 4095)?;
                let subgroup = starts_subgroup(index);
                let reverse = picked_reverse(index);
                if reverse {
                    reversed[index as usize] = true;
                }
                if index > 0 {
                    let previous = selected_primitives[index as usize - 1];
                    let primitive = selected_primitives[index as usize];
                    let previous_reversed = picked_reverse(index - 1);
                    let current_end = endpoint(previous, previous_reversed, true);
                    let next_start = endpoint(primitive, reverse, false);
                    let join_distance = distance(current_end, next_start)?;
                    if subgroup || join_distance > tolerance {
                        selected_chain[index as usize] = chain_count;
                        chain_count += 1;
                    } else {
                        selected_chain[index as usize] = chain_count - 1;
                        welded[index as usize] = true;
                    }
                }
            }
        } else {
            let fixed_chain_count = match group_size {
                None => 1,
                Some(size) => 1 + (selected_count - 1) / size,
            };
            chain_count = fixed_chain_count;
            let chunk = grain.unwrap_or(16_384);
            let reversed_windows = support::Windows::even(&mut reversed, chunk);
            let welded_windows = support::Windows::even(&mut welded, chunk);
            let chain_windows = support::Windows::even(&mut selected_chain, chunk);
            let selected_ref = &selected_primitives;
            support::run_indexed_ranges(chunk, selected_count, &|range, first, last| {
                cancel::check_opt(cancel_value)?;
                let local_reversed = reversed_windows.take(range);
                let local_welded = welded_windows.take(range);
                let local_chain = chain_windows.take(range);
                for index in first..last {
                    let subgroup = starts_subgroup(index);
                    let reverse = picked_reverse(index);
                    let at = (index - first) as usize;
                    local_reversed[at] = reverse;
                    local_chain[at] = match group_size {
                        None => 0,
                        Some(size) => index / size,
                    };
                    let mut weld = false;
                    if index > 0 && !subgroup {
                        let previous = selected_ref[index as usize - 1];
                        let primitive = selected_ref[index as usize];
                        let current_end = endpoint(previous, picked_reverse(index - 1), true);
                        let next_start = endpoint(primitive, reverse, false);
                        if distance(current_end, next_start)? <= tolerance {
                            weld = true;
                        }
                    }
                    local_welded[at] = weld;
                }
                Ok(())
            })?;
        }
    } else if connect_closest_ends {
        if selected_count > i64::MAX / 2 {
            return Err(fail("Curve Join endpoint count exceeds OCaml array limits".to_owned()));
        }
        let input_primitives = selected_primitives.clone();
        let endpoint_count = selected_count * 2;
        let chunk = grain.unwrap_or(16_384);
        let mut endpoint_x = vec![0.0; endpoint_count as usize];
        let mut endpoint_y = vec![0.0; endpoint_count as usize];
        let mut endpoint_z = vec![0.0; endpoint_count as usize];
        {
            let xs = support::Windows::even(&mut endpoint_x, chunk);
            let ys = support::Windows::even(&mut endpoint_y, chunk);
            let zs = support::Windows::even(&mut endpoint_z, chunk);
            support::run_indexed_ranges(chunk, endpoint_count, &|range, first, last| {
                let xs = xs.take(range);
                let ys = ys.take(range);
                let zs = zs.take(range);
                for endpoint_slot in first..last {
                    cancel::poll(cancel_value, endpoint_slot, 16_383)?;
                    let primitive = input_primitives[(endpoint_slot >> 1) as usize];
                    let point = endpoint(primitive, false, endpoint_slot & 1 == 1) as usize;
                    let (x, y, z) = (positions.x[point], positions.y[point], positions.z[point]);
                    if !(x.is_finite() && y.is_finite() && z.is_finite()) {
                        return Err(fail(
                            "Curve Join requires finite selected endpoint positions".to_owned(),
                        ));
                    }
                    let at = (endpoint_slot - first) as usize;
                    xs[at] = x;
                    ys[at] = y;
                    zs[at] = z;
                }
                Ok(())
            })?;
        }
        let mut tree = EndpointTree::create(cancel_value, endpoint_x, endpoint_y, endpoint_z)?;
        let mut used = vec![false; sc];
        fn remove(tree: &mut EndpointTree, used: &mut [bool], primitive_slot: i64) {
            used[primitive_slot as usize] = true;
            tree.remove(primitive_slot * 2);
            tree.remove(primitive_slot * 2 + 1);
        }
        fn orient_root(
            tree: &mut EndpointTree,
            cancel_value: Option<&cancel::T>,
            primitive_slot: i64,
        ) -> Result<bool, error::T> {
            if tree.active_count[tree.root as usize] == 0 {
                Ok(false)
            } else {
                tree.nearest(cancel_value, primitive_slot * 2)?;
                let from_start = tree.nearest_distance;
                tree.nearest(cancel_value, primitive_slot * 2 + 1)?;
                Ok(from_start < tree.nearest_distance)
            }
        }
        remove(&mut tree, &mut used, 0);
        let mut current_endpoint = 1_i64;
        let mut next_root = 1_i64;
        for index in 1..selected_count {
            cancel::poll(cancel_value, index, 4095)?;
            tree.nearest(cancel_value, current_endpoint)?;
            let nearest = tree.nearest_endpoint;
            let nearest_distance = tree.nearest_distance;
            if nearest < 0 {
                return Err(fail("Curve Join endpoint search exhausted early".to_owned()));
            }
            let subgroup = starts_subgroup(index);
            let disconnected = only_connected && nearest_distance > tolerance;
            let new_chain = subgroup || disconnected;
            let mut removed = false;
            let (primitive_slot, reverse, join_distance) = if new_chain {
                let primitive_slot = if subgroup {
                    nearest >> 1
                } else {
                    while next_root < selected_count && used[next_root as usize] {
                        next_root += 1;
                    }
                    if next_root >= selected_count {
                        return Err(fail(
                            "Curve Join connected-component planning exhausted early".to_owned(),
                        ));
                    }
                    next_root
                };
                remove(&mut tree, &mut used, primitive_slot);
                removed = true;
                let reverse = orient_root(&mut tree, cancel_value, primitive_slot)?;
                (primitive_slot, reverse, f64::INFINITY)
            } else {
                (nearest >> 1, nearest & 1 == 1, nearest_distance)
            };
            selected_primitives[index as usize] = input_primitives[primitive_slot as usize];
            if new_chain {
                selected_chain[index as usize] = chain_count;
                chain_count += 1;
            } else {
                selected_chain[index as usize] = chain_count - 1;
                if reverse {
                    reversed[index as usize] = true;
                }
                if join_distance <= tolerance {
                    welded[index as usize] = true;
                }
            }
            if !removed {
                remove(&mut tree, &mut used, primitive_slot);
            }
            current_endpoint = primitive_slot * 2 + if reverse { 0 } else { 1 };
        }
    } else {
        for index in 1..selected_count {
            let iu = index as usize;
            let previous = selected_primitives[iu - 1];
            let primitive = selected_primitives[iu];
            let previous_reversed = reversed[iu - 1];
            let current_end = endpoint(previous, previous_reversed, true);
            let next_start = endpoint(primitive, false, false);
            let next_end = endpoint(primitive, false, true);
            let start_distance = distance(current_end, next_start)?;
            let end_distance = distance(current_end, next_end)?;
            let subgroup = starts_subgroup(index);
            let (reverse, dist) = if subgroup {
                let reverse = if orient_closest && index + 1 < selected_count && !starts_subgroup(index + 1) {
                    let following = selected_primitives[iu + 1];
                    let following_start = endpoint(following, false, false);
                    let following_end = endpoint(following, false, true);
                    let root_start = endpoint(primitive, false, false);
                    let root_end = endpoint(primitive, false, true);
                    let from_start = distance(root_start, following_start)?.min(distance(root_start, following_end)?);
                    let from_end = distance(root_end, following_start)?.min(distance(root_end, following_end)?);
                    from_start < from_end
                } else {
                    false
                };
                (reverse, f64::INFINITY)
            } else {
                let reverse = orient_closest && end_distance < start_distance;
                (reverse, if reverse { end_distance } else { start_distance })
            };
            if subgroup || (only_connected && dist > tolerance) {
                selected_chain[iu] = chain_count;
                if reverse {
                    reversed[iu] = true;
                }
                chain_count += 1;
            } else {
                selected_chain[iu] = chain_count - 1;
                if reverse {
                    reversed[iu] = true;
                }
                if dist <= tolerance {
                    welded[iu] = true;
                }
            }
        }
    }
    selected_index.fill(-1);
    for (index, &primitive) in selected_primitives.iter().enumerate() {
        selected_index[primitive as usize] = index as i64;
    }
    let cc = chain_count as usize;
    let mut chain_first = vec![-1_i64; cc];
    let mut chain_last = vec![-1_i64; cc];
    let mut chain_size = vec![0_i64; cc];
    for index in 0..sc {
        let chain = selected_chain[index] as usize;
        if chain_first[chain] < 0 {
            chain_first[chain] = index as i64;
        }
        chain_last[chain] = index as i64;
        let primitive = selected_primitives[index] as usize;
        let size = source.primitive_offsets[primitive + 1] - source.primitive_offsets[primitive];
        chain_size[chain] += size - if index as i64 > chain_first[chain] && welded[index] { 1 } else { 0 };
    }
    if wrap {
        for (chain, &size) in chain_size.iter().enumerate() {
            if size < 3 {
                return Err(fail(format!(
                    "Curve Join cannot wrap chain {chain} with fewer than three vertices"
                )));
            }
        }
    }
    let retained_primitive_count = if keep_originals {
        primitive_count
    } else {
        primitive_count - selected_count
    };
    if retained_primitive_count > i64::MAX - chain_count {
        return Err(fail("Curve Join primitive cardinality exceeds OCaml array limits".to_owned()));
    }
    let output_primitive_count = retained_primitive_count + chain_count;
    let opc = output_primitive_count as usize;
    // (retained source primitive or -1, chain or -1), one per output primitive.
    let mut plan: Vec<(i64, i64)> = Vec::with_capacity(opc);
    if keep_originals {
        for primitive in 0..primitive_count {
            plan.push((primitive, -1));
        }
        for chain in 0..chain_count {
            plan.push((-1, chain));
        }
    } else {
        for primitive in 0..primitive_count {
            let index = selected_index[primitive as usize];
            if index < 0 {
                plan.push((primitive, -1));
            } else {
                let chain = selected_chain[index as usize];
                if chain_first[chain as usize] == index {
                    plan.push((-1, chain));
                }
            }
        }
    }
    let mut descriptor_primitive = vec![-1_i64; opc];
    let mut descriptor_chain = vec![-1_i64; opc];
    let mut primitive_map = vec![0_i64; opc];
    let mut output_sizes = vec![0_i64; opc];
    let mut output_kinds = vec![topology::PrimitiveKind::OpenPolyline; opc];
    for (output, &(primitive, chain)) in plan.iter().enumerate() {
        if primitive >= 0 {
            descriptor_primitive[output] = primitive;
            primitive_map[output] = primitive;
            output_sizes[output] = source.primitive_offsets[primitive as usize + 1]
                - source.primitive_offsets[primitive as usize];
            output_kinds[output] = match source.primitive_kinds[primitive as usize] {
                0 => topology::PrimitiveKind::Polygon,
                1 => topology::PrimitiveKind::OpenPolyline,
                _ => topology::PrimitiveKind::ClosedPolyline,
            };
        } else {
            descriptor_chain[output] = chain;
            primitive_map[output] = selected_primitives[chain_first[chain as usize] as usize];
            output_sizes[output] = chain_size[chain as usize];
            output_kinds[output] = if wrap {
                topology::PrimitiveKind::ClosedPolyline
            } else {
                topology::PrimitiveKind::OpenPolyline
            };
        }
    }
    let mut primitive_offsets = vec![0_i64; opc + 1];
    for primitive in 0..opc {
        if primitive_offsets[primitive] > i64::MAX - output_sizes[primitive] {
            return Err(fail("Curve Join output cardinality exceeds OCaml array limits".to_owned()));
        }
        primitive_offsets[primitive + 1] = primitive_offsets[primitive] + output_sizes[primitive];
    }
    let output_vertex_count = primitive_offsets[opc];
    let mut vertex_map = vec![0; output_vertex_count as usize];
    let mut vertex_points = vec![0; output_vertex_count as usize];
    let mut edge_source_corner = vec![-1; output_vertex_count as usize];
    {
        let chunk = grain.unwrap_or(16_384);
        let offsets = &primitive_offsets;
        let ranges = support::range_count(output_primitive_count, chunk);
        let bound = |range: i64| offsets[(range.saturating_mul(chunk)).min(output_primitive_count) as usize] as usize;
        let map = support::Windows::new(&mut vertex_map, ranges, bound);
        let points = support::Windows::new(&mut vertex_points, ranges, bound);
        let corner = support::Windows::new(&mut edge_source_corner, ranges, bound);
        let (descriptor_primitive, descriptor_chain) = (&descriptor_primitive, &descriptor_chain);
        let (selected_primitives, reversed, welded) = (&selected_primitives, &reversed, &welded);
        let (chain_first, chain_last) = (&chain_first, &chain_last);
        support::run_indexed_ranges(chunk, output_primitive_count, &|range, first_output, last_output| {
            cancel::check_opt(cancel_value)?;
            let base = offsets[first_output as usize];
            let mut buf = FillBuf {
                map: map.take(range),
                points: points.take(range),
                corner: corner.take(range),
            };
            for output_primitive in first_output..last_output {
                let mut output_at = (offsets[output_primitive as usize] - base) as usize;
                let mut previous = -1_i64;
                let single = descriptor_primitive[output_primitive as usize];
                if single >= 0 {
                    let source_first = source.primitive_offsets[single as usize];
                    let source_size = source.primitive_offsets[single as usize + 1] - source_first;
                    fill_source(
                        &mut buf,
                        &source.vertex_points,
                        &mut output_at,
                        &mut previous,
                        (source_first, source_size),
                        (false, false),
                    );
                    if source.primitive_kinds[single as usize] != OPEN_POLYLINE {
                        buf.corner[previous as usize] = source_first + source_size - 1;
                    }
                } else {
                    let chain = descriptor_chain[output_primitive as usize] as usize;
                    for index in chain_first[chain]..=chain_last[chain] {
                        let primitive = selected_primitives[index as usize] as usize;
                        let source_first = source.primitive_offsets[primitive];
                        let source_size = source.primitive_offsets[primitive + 1] - source_first;
                        fill_source(
                            &mut buf,
                            &source.vertex_points,
                            &mut output_at,
                            &mut previous,
                            (source_first, source_size),
                            (reversed[index as usize], index > chain_first[chain] && welded[index as usize]),
                        );
                    }
                    buf.corner[previous as usize] = -1;
                }
            }
            Ok(())
        })?;
    }
    let output_topology = topology::create_owned(
        source.point_count,
        vertex_points,
        primitive_offsets.clone(),
        output_kinds.clone(),
    )?;
    let mut attributes = Vec::new();
    for item in geometry::attributes(value) {
        attributes.push(match attribute::owner(&item) {
            attribute::Owner::Point | attribute::Owner::Detail => item,
            attribute::Owner::Vertex => remap_attribute(cancel_value, grain, &vertex_map, &item)?,
            attribute::Owner::Primitive => remap_attribute(cancel_value, grain, &primitive_map, &item)?,
        });
    }
    let mut groups = Vec::new();
    for item in geometry::groups(value) {
        groups.push(match group::owner(&item) {
            group::Owner::Point => item,
            group::Owner::Vertex => {
                let target = group::init(
                    grain,
                    group::Owner::Vertex,
                    group::name(&item),
                    output_vertex_count,
                    &|vertex| group::mem(vertex_map[vertex as usize], &item),
                )?;
                group::view::remap_order(&item, &vertex_map, &target)?
            }
            group::Owner::Primitive => {
                let target = group::init(
                    grain,
                    group::Owner::Primitive,
                    group::name(&item),
                    output_primitive_count,
                    &|output| {
                        let single = descriptor_primitive[output as usize];
                        if single >= 0 {
                            group::mem(single, &item)
                        } else {
                            let chain = descriptor_chain[output as usize] as usize;
                            let mut member = false;
                            for index in chain_first[chain]..=chain_last[chain] {
                                if group::mem(selected_primitives[index as usize], &item) {
                                    member = true;
                                }
                            }
                            member
                        }
                    },
                )?;
                group::view::remap_order(&item, &primitive_map, &target)?
            }
        });
    }
    let source_groups = geometry::edge_groups(value);
    let mut edge_groups = Vec::new();
    if !source_groups.is_empty() {
        let mut seen = vec![false; source.point_count as usize];
        let mut unique_points = true;
        for &point in source.vertex_points.iter() {
            if seen[point as usize] {
                unique_points = false;
            } else {
                seen[point as usize] = true;
            }
        }
        if unique_points && !keep_originals {
            let edge_count = |kind: topology::PrimitiveKind, size: i64| -> i64 {
                size - 1
                    + match kind {
                        topology::PrimitiveKind::Polygon | topology::PrimitiveKind::ClosedPolyline => 1,
                        topology::PrimitiveKind::OpenPolyline => 0,
                    }
            };
            let mut source_corner_edge = vec![-1_i64; source.vertex_points.len()];
            let mut source_edge_at = 0_i64;
            for primitive in 0..primitive_count as usize {
                let first = source.primitive_offsets[primitive];
                let size = source.primitive_offsets[primitive + 1] - first;
                let kind = match source.primitive_kinds[primitive] {
                    0 => topology::PrimitiveKind::Polygon,
                    1 => topology::PrimitiveKind::OpenPolyline,
                    _ => topology::PrimitiveKind::ClosedPolyline,
                };
                for local in 0..edge_count(kind, size) {
                    source_corner_edge[(first + local) as usize] = source_edge_at;
                    source_edge_at += 1;
                }
            }
            let mut target_edge_offsets = vec![0_i64; opc + 1];
            for primitive in 0..opc {
                target_edge_offsets[primitive + 1] =
                    target_edge_offsets[primitive] + edge_count(output_kinds[primitive], output_sizes[primitive]);
            }
            let target_edge_count = target_edge_offsets[opc];
            for source_group in &source_groups {
                let mut bits = vec![0_u8; ((target_edge_count + 7) / 8) as usize];
                for primitive in 0..opc {
                    cancel::poll(cancel_value, primitive as i64, 4095)?;
                    let edges = target_edge_offsets[primitive + 1] - target_edge_offsets[primitive];
                    let first = primitive_offsets[primitive];
                    for local in 0..edges {
                        let source_corner = edge_source_corner[(first + local) as usize];
                        if source_corner >= 0 {
                            let source_edge = source_corner_edge[source_corner as usize];
                            if source_edge >= 0 && edge_group::mem(source_edge, source_group) {
                                let edge = (target_edge_offsets[primitive] + local) as usize;
                                bits[edge >> 3] |= 1 << (edge & 7);
                            }
                        }
                    }
                }
                edge_groups.push(edge_group::view::of_owned_bits(
                    &output_topology,
                    target_edge_count,
                    edge_group::name(source_group),
                    bits,
                )?);
            }
        } else {
            let source_index = topology_index::create(cancel_value, topology_value)?;
            let target_index = topology_index::create(cancel_value, &output_topology)?;
            let target_view = topology_index::view::view(&target_index);
            for source_group in &source_groups {
                let mut builder =
                    edge_group::builder::create(&output_topology, &target_index, edge_group::name(source_group))?;
                for vertex in 0..output_vertex_count {
                    cancel::poll(cancel_value, vertex, 16_383)?;
                    let source_corner = edge_source_corner[vertex as usize];
                    if source_corner >= 0 {
                        let source_edge = topology_index::edge_of_vertex(&source_index, source_corner)?;
                        let target_edge = target_view.edge_of_vertex[vertex as usize];
                        if source_edge >= 0 && target_edge >= 0 && edge_group::mem(source_edge, source_group) {
                            edge_group::builder::set(&mut builder, target_edge, true)?;
                        }
                    }
                }
                edge_groups.push(edge_group::builder::freeze(&mut builder)?);
            }
        }
    }
    geometry::create(
        geometry::positions(value).clone(),
        output_topology,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )
}

/// Disjoint final output of the vertex fill; `corner` is the source corner each output edge follows.
struct FillBuf<'a> {
    map: &'a mut [i64],
    points: &'a mut [i64],
    corner: &'a mut [i64],
}

/// Appends one source piece to the output primitive being filled; `piece` is
/// `(source_first, source_size)` and `flags` is `(reverse, skip_first)`.
fn fill_source(
    buf: &mut FillBuf<'_>,
    source_vertex_points: &[i64],
    output_at: &mut usize,
    previous: &mut i64,
    piece: (i64, i64),
    flags: (bool, bool),
) {
    let (source_first, source_size) = piece;
    let (reverse, skip_first) = flags;
    let first_step = if skip_first { 1 } else { 0 };
    for step in first_step..source_size {
        let local = if reverse { source_size - 1 - step } else { step };
        let source_vertex = source_first + local;
        let output = *output_at;
        buf.map[output] = source_vertex;
        buf.points[output] = source_vertex_points[source_vertex as usize];
        if *previous >= 0 {
            buf.corner[*previous as usize] = if step == first_step && first_step == 0 {
                -1
            } else if reverse {
                source_first + local
            } else {
                source_first + local - 1
            };
        }
        *previous = output as i64;
        *output_at += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn curves(points: &[(f64, f64, f64)], rows: &[&[i64]]) -> geometry::T {
        let positions = packed::float3::of_owned(
            points.iter().map(|p| p.0).collect(),
            points.iter().map(|p| p.1).collect(),
            points.iter().map(|p| p.2).collect(),
        )
        .unwrap();
        let mut builder = topology::builder::create(points.len() as i64, None, None).unwrap();
        for row in rows {
            topology::builder::add_open_polyline(&mut builder, row).unwrap();
        }
        let topo = topology::builder::freeze(&mut builder).unwrap();
        geometry::create(positions, topo, None, None, None).unwrap()
    }

    fn params() -> Params {
        Params {
            cancel: None,
            grain: None,
            primitives: None,
            picked_ends: None,
            orient_closest: true,
            connect_closest_ends: false,
            only_connected: false,
            group_size: None,
            keep_originals: false,
            tolerance: 0.0,
            wrap: false,
        }
    }

    fn rows(value: &geometry::T) -> Vec<Vec<i64>> {
        let view = topology::view::view(geometry::topology(value)).unwrap();
        (0..view.primitive_offsets.len() - 1)
            .map(|p| view.vertex_points[view.primitive_offsets[p] as usize..view.primitive_offsets[p + 1] as usize].to_vec())
            .collect()
    }

    fn message(result: Result<geometry::T, error::T>) -> String {
        match result {
            Ok(_) => String::new(),
            Err(failure) => error::message(&failure).to_owned(),
        }
    }

    const LINE: [(f64, f64, f64); 6] =
        [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.0, 0.0, 0.0), (3.0, 0.0, 0.0), (4.0, 0.0, 0.0), (5.0, 0.0, 0.0)];

    #[test]
    fn joins_consecutive_curves_and_welds_shared_endpoints() {
        let input = curves(&LINE, &[&[0, 1, 2], &[2, 3, 4]]);
        let joined = join(&params(), &input).unwrap();
        assert_eq!(rows(&joined), vec![vec![0, 1, 2, 3, 4]]);
        let separate = curves(&LINE, &[&[0, 1], &[3, 4]]);
        let gap = join(&params(), &separate).unwrap();
        assert_eq!(rows(&gap), vec![vec![0, 1, 3, 4]]);
        let only = Params { only_connected: true, ..params() };
        assert_eq!(rows(&join(&only, &separate).unwrap()), vec![vec![0, 1], vec![3, 4]]);
    }

    #[test]
    fn orients_closest_ends_and_keeps_originals_and_wraps() {
        let input = curves(&LINE, &[&[0, 1, 2], &[4, 3, 2]]);
        assert_eq!(rows(&join(&params(), &input).unwrap()), vec![vec![0, 1, 2, 3, 4]]);
        let kept = Params { keep_originals: true, wrap: true, ..params() };
        assert_eq!(
            rows(&join(&kept, &input).unwrap()),
            vec![vec![0, 1, 2], vec![4, 3, 2], vec![0, 1, 2, 3, 4]]
        );
        let view = topology::view::view(geometry::topology(&join(&kept, &input).unwrap())).unwrap();
        assert_eq!(&view.primitive_kinds[..], &[1, 1, 2]);
    }

    #[test]
    fn connect_closest_ends_chains_by_proximity() {
        let input = curves(&LINE, &[&[0, 1], &[4, 5], &[2, 3]]);
        let connect = Params { connect_closest_ends: true, ..params() };
        assert_eq!(rows(&join(&connect, &input).unwrap()), vec![vec![0, 1, 2, 3, 4, 5]]);
        let picked = Params {
            picked_ends: Some(vec![
                CurveJoinPick { primitive: 0, end_: CurveJoinEnd::JoinCurveStart },
                CurveJoinPick { primitive: 2, end_: CurveJoinEnd::JoinCurveStart },
            ]),
            ..params()
        };
        assert_eq!(rows(&join(&picked, &input).unwrap()), vec![vec![1, 0, 2, 3], vec![4, 5]]);
    }

    #[test]
    fn group_size_splits_chains_and_failures_are_reported() {
        let input = curves(&LINE, &[&[0, 1], &[1, 2], &[2, 3], &[3, 4]]);
        let sized = Params { group_size: Some(2), ..params() };
        assert_eq!(rows(&join(&sized, &input).unwrap()), vec![vec![0, 1, 2], vec![2, 3, 4]]);
        assert_eq!(
            message(join(&Params { tolerance: -1.0, ..params() }, &input)),
            "Curve Join tolerance must be finite and non-negative"
        );
        assert_eq!(
            message(join(&Params { group_size: Some(0), ..params() }, &input)),
            "Curve Join group size must be positive"
        );
        let two = Params { wrap: true, ..params() };
        let short = curves(&LINE, &[&[0, 1]]);
        assert_eq!(
            message(join(&two, &short)),
            "Curve Join cannot wrap chain 0 with fewer than three vertices"
        );
        let cancelled = Arc::new(cancel::create());
        cancel::cancel(&cancelled);
        let Err(failure) = join(&Params { cancel: Some(cancelled), ..params() }, &input) else {
            panic!("expected cancellation")
        };
        assert_eq!(error::code(&failure), Code::Cancelled);
    }

    #[test]
    fn final_windows_preserve_parallel_curve_layouts() {
        use crate::attribute_ops::{fixtures, fixtures2};
        let mut points = Vec::new();
        let mut rows = Vec::new();
        let mut start = 0.0;
        for i in 0..96 {
            let row: Vec<_> = (0..2 + i % 4).map(|j| {
                let index = points.len() as i64;
                points.push((start + j as f64, 0.0, 0.0));
                index
            }).collect();
            start += row.len() as f64 - 1.0;
            rows.push(row);
        }
        let refs: Vec<_> = rows.iter().map(Vec::as_slice).collect();
        let input = curves(&points, &refs);
        let input = fixtures2::add_ints(attribute::Owner::Vertex, "v", (0..geometry::vertex_count(&input)).collect(), &input);
        let input = fixtures2::add_ints(attribute::Owner::Primitive, "p", (0..96).collect(), &input);
        for connect in [false, true] {
            let options = Params { orient_closest: false, connect_closest_ends: connect, group_size: Some(7), keep_originals: true, ..params() };
            let expected = fixtures::on_domains(1, &|| join(&Params { grain: Some(8192), ..options.clone() }, &input).unwrap());
            let actual = fixtures::on_domains(8, &|| join(&Params { grain: Some(3), ..options.clone() }, &input).unwrap());
            assert_eq!(fixtures2::signature(&expected), fixtures2::signature(&actual));
        }
    }

    #[test]
    fn edge_groups_follow_surviving_edges_and_small_grains_agree() {
        let base = curves(&LINE, &[&[0, 1, 2], &[3, 4, 5]]);
        let index = topology_index::create(None, geometry::topology(&base)).unwrap();
        let group_value = edge_group::init(None, geometry::topology(&base), &index, "g", &|edge| edge == 1 || edge == 3)
            .unwrap();
        let input = geometry::with_edge_group(group_value, &base).unwrap();
        let sequential = join(&params(), &input).unwrap();
        let tiny = join(&Params { grain: Some(1), ..params() }, &input).unwrap();
        assert_eq!(rows(&sequential), rows(&tiny));
        let kept = geometry::find_edge_group("g", &sequential).unwrap();
        let tiny_kept = geometry::find_edge_group("g", &tiny).unwrap();
        for edge in 0..5 {
            assert_eq!(edge_group::mem(edge, &kept), edge_group::mem(edge, &tiny_kept));
        }
        assert_eq!(edge_group::cardinality(&kept), 2);
    }

    #[test]
    fn interpolate_attribute_leaves_primitive_payload_alone() {
        let value = attribute::create_owned("a", attribute::Owner::Primitive, attribute::Storage::Float(Arc::new(vec![1.0]))).unwrap();
        let same = interpolate_attribute(None, None, &[], &[], &[], &[], &[], &[], &value).unwrap();
        assert_eq!(attribute::length(&same), 1);
        let point = attribute::create_owned("p", attribute::Owner::Point, attribute::Storage::Float(Arc::new(vec![0.0, 10.0]))).unwrap();
        let blended = interpolate_attribute(None, None, &[0], &[1], &[0.25], &[], &[], &[], &point).unwrap();
        match attribute::view::storage(&blended).unwrap() {
            attribute::Storage::Float(values) => assert_eq!(**values, vec![2.5]),
            _ => panic!("float"),
        }
        let Err(failure) = interpolate_attribute(None, Some(0), &[0], &[1], &[0.25], &[], &[], &[], &point) else {
            panic!("expected failure")
        };
        assert_eq!(error::message(&failure), "grain must be positive");
    }
}
