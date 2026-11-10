// Deterministic source order breaks ties between equal exact coordinates.
//! Shared exact-predicate planar constraint-recovery kernel.
//!
//! Its ear clip is not `Polygon_triangulation.find_ear`: it uses exact orientation predicates and
//! takes the lowest-point ear, not the first.
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::sync::Arc;

use rdk_core::{cancel, scratch};

use crate::predicates::Sign;


pub struct T {
    triangle_points: Arc<[i64]>,
    constraint_points: Arc<[i64]>,
}

fn orient(cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64, c: i64) -> Sign {
    (cb.0)(a, b, c)
}

fn incircle(cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64, c: i64, d: i64) -> Sign {
    (cb.1)(a, b, c, d)
}

fn bounds_overlap(cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64, c: i64, d: i64) -> bool {
    (cb.2)(a, b, c, d)
}

fn err_message(message: &str) -> String {
    message.to_owned()
}

fn cancelled() -> String {
    "Planar CDT was cancelled".to_owned()
}

fn check(cancel_value: Option<&cancel::T>) -> Result<(), String> {
    cancel::check_opt(cancel_value).map_err(|_| cancelled())
}

fn poll(cancel_value: Option<&cancel::T>, index: i64, mask: i64) -> Result<(), String> {
    cancel::poll(cancel_value, index, mask).map_err(|_| cancelled())
}

fn u(value: i64) -> usize {
    value as usize
}

fn edge_key(point_count: i64, key_stride: i64, first: i64, second: i64) -> Result<i64, String> {
    let (first, second) = if first < second {
        (first, second)
    } else {
        (second, first)
    };
    if first < 0 || second < 0 || second >= point_count {
        return Err(err_message("Planar CDT edge endpoint is out of range"));
    }
    if key_stride <= 0 { return Err(err_message("Planar CDT edge key exceeds integer range")); }
    first.checked_mul(key_stride).and_then(|key| key.checked_add(second))
        .ok_or_else(|| err_message("Planar CDT edge key exceeds integer range"))
}

const EMPTY: i64 = -1;
const TOMBSTONE: i64 = -2;

struct Table {
    keys: Vec<i64>,
    first_triangles: Vec<i64>,
    first_opposites: Vec<i64>,
    second_triangles: Vec<i64>,
    second_opposites: Vec<i64>,
    constrained: Vec<u8>,
    edge_count: i64,
    tombstone_count: i64,
}

fn table_capacity_for(expected: i64) -> Result<usize, String> {
    if expected < 0 {
        return Err(err_message("Planar CDT edge cardinality exceeds array limits"));
    }
    let needed = std::cmp::max(16, expected.saturating_mul(2));
    let mut capacity: i64 = 16;
    while capacity < needed {
        capacity = capacity
            .checked_mul(2)
            .ok_or_else(|| err_message("Planar CDT edge table exceeds array limits"))?;
    }
    usize::try_from(capacity).map_err(|_| err_message("Planar CDT edge table exceeds array limits"))
}

fn table_with_capacity(capacity: usize) -> Table {
    Table {
        keys: vec![EMPTY; capacity],
        first_triangles: vec![-1; capacity],
        first_opposites: vec![-1; capacity],
        second_triangles: vec![-1; capacity],
        second_opposites: vec![-1; capacity],
        constrained: vec![0; capacity],
        edge_count: 0,
        tombstone_count: 0,
    }
}

/// An allocation-free stand-in left behind while a workspace's real table is checked out.
fn table_empty() -> Table {
    Table { keys: Vec::new(), first_triangles: Vec::new(), first_opposites: Vec::new(), second_triangles: Vec::new(), second_opposites: Vec::new(), constrained: Vec::new(), edge_count: 0, tombstone_count: 0 }
}

fn table_create(expected: i64) -> Result<Table, String> {
    Ok(table_with_capacity(table_capacity_for(expected)?))
}

fn table_reset(table: &mut Table) {
    table.keys.iter_mut().for_each(|k| *k = EMPTY);
    table.edge_count = 0;
    table.tombstone_count = 0;
}

fn table_clear_constraints(table: &mut Table) {
    table.constrained.iter_mut().for_each(|k| *k = 0);
}

fn hash(key: i64) -> i64 { rays_math::hash::mix(0, key) }

fn find_slot(table: &Table, key: i64) -> usize {
    let mask = (table.keys.len() - 1) as i64;
    let mut slot = hash(key) & mask;
    let mut first_tombstone: i64 = -1;
    loop {
        let present = table.keys[slot as usize];
        if present == key {
            return slot as usize;
        } else if present == EMPTY {
            return if first_tombstone >= 0 {
                first_tombstone as usize
            } else {
                slot as usize
            };
        } else {
            if present == TOMBSTONE && first_tombstone < 0 {
                first_tombstone = slot;
            }
            slot = (slot + 1) & mask;
        }
    }
}

/// Slot of `key`, or -1.
fn table_find(table: &Table, key: i64) -> i64 {
    let slot = find_slot(table, key);
    if table.keys[slot] == key {
        slot as i64
    } else {
        -1
    }
}

fn table_add_raw(
    table: &mut Table,
    key: i64,
    triangle: i64,
    opposite: i64,
    is_constrained: bool,
) -> Result<(), String> {
    let slot = find_slot(table, key);
    if table.keys[slot] != key {
        if table.keys[slot] == TOMBSTONE {
            table.tombstone_count -= 1;
        }
        table.keys[slot] = key;
        table.first_triangles[slot] = triangle;
        table.first_opposites[slot] = opposite;
        table.second_triangles[slot] = -1;
        table.second_opposites[slot] = -1;
        table.constrained[slot] = if is_constrained { 1 } else { 0 };
        table.edge_count += 1;
    } else {
        if is_constrained {
            table.constrained[slot] = 1;
        }
        let first = table.first_triangles[slot];
        let second = table.second_triangles[slot];
        if triangle == first || triangle == second {
            return Err(err_message("Planar CDT duplicated one triangle edge incidence"));
        } else if second >= 0 {
            return Err(err_message("Planar CDT input contains a non-manifold edge"));
        } else if triangle < first {
            table.second_triangles[slot] = first;
            table.second_opposites[slot] = table.first_opposites[slot];
            table.first_triangles[slot] = triangle;
            table.first_opposites[slot] = opposite;
        } else {
            table.second_triangles[slot] = triangle;
            table.second_opposites[slot] = opposite;
        }
    }
    Ok(())
}

fn table_rehash(table: &mut Table, capacity: usize) -> Result<(), String> {
    let old = std::mem::replace(table, table_with_capacity(capacity));
    for slot in 0..old.keys.len() {
        if old.keys[slot] >= 0 {
            let fixed = old.constrained[slot] != 0;
            table_add_raw(
                table,
                old.keys[slot],
                old.first_triangles[slot],
                old.first_opposites[slot],
                fixed,
            )?;
            if old.second_triangles[slot] >= 0 {
                table_add_raw(
                    table,
                    old.keys[slot],
                    old.second_triangles[slot],
                    old.second_opposites[slot],
                    fixed,
                )?;
            }
        }
    }
    Ok(())
}

fn table_prepare_insert(table: &mut Table) -> Result<(), String> {
    let capacity = table.keys.len() as i64;
    let used = table.edge_count + table.tombstone_count + 1;
    if used >= capacity - capacity / 3 {
        let next = capacity
            .checked_mul(2)
            .ok_or_else(|| err_message("Planar CDT edge table exceeds array limits"))? as usize;
        table_rehash(table, next)?;
    } else if table.tombstone_count > table.edge_count && table.tombstone_count > 64 {
        table_rehash(table, capacity as usize)?;
    }
    Ok(())
}

fn table_add(table: &mut Table, key: i64, triangle: i64, opposite: i64) -> Result<(), String> {
    table_prepare_insert(table)?;
    table_add_raw(table, key, triangle, opposite, false)
}

fn table_remove(table: &mut Table, key: i64, triangle: i64) -> Result<(), String> {
    let slot = table_find(table, key);
    if slot < 0 {
        return Err(err_message("Planar CDT lost an indexed edge"));
    }
    let slot = u(slot);
    if table.first_triangles[slot] == triangle {
        if table.second_triangles[slot] >= 0 {
            table.first_triangles[slot] = table.second_triangles[slot];
            table.first_opposites[slot] = table.second_opposites[slot];
            table.second_triangles[slot] = -1;
            table.second_opposites[slot] = -1;
        } else {
            table.keys[slot] = TOMBSTONE;
            table.first_triangles[slot] = -1;
            table.first_opposites[slot] = -1;
            table.constrained[slot] = 0;
            table.edge_count -= 1;
            table.tombstone_count += 1;
        }
    } else if table.second_triangles[slot] == triangle {
        table.second_triangles[slot] = -1;
        table.second_opposites[slot] = -1;
    } else {
        return Err(err_message("Planar CDT lost one edge incidence"));
    }
    Ok(())
}

fn table_constrain(table: &mut Table, key: i64) -> Result<(), String> {
    let slot = table_find(table, key);
    if slot < 0 {
        return Err(err_message("Planar CDT cannot mark a missing edge"));
    }
    table.constrained[u(slot)] = 1;
    Ok(())
}

fn table_unconstrain(table: &mut Table, key: i64) {
    let slot = table_find(table, key);
    if slot >= 0 {
        table.constrained[u(slot)] = 0;
    }
}

pub struct Workspace {
    triangle_a: Vec<i64>,
    triangle_b: Vec<i64>,
    triangle_c: Vec<i64>,
    point_triangle: Vec<i64>,
    key_stride: i64,
    point_count: i64,
    triangle_count: i64,
    last_output: Option<Arc<[i64]>>,
    constraints: Vec<i64>,
    valid: bool,
    full_build_count: i64,
    incremental_build_count: i64,
    edge_table: Table,
}

fn checked_edge_capacity(triangle_capacity: i64) -> Result<i64, String> {
    triangle_capacity.checked_mul(2).and_then(|count| count.checked_add(3))
        .filter(|_| triangle_capacity >= 0)
        .ok_or_else(|| err_message("Planar CDT triangle cardinality exceeds edge-table limits"))
}

pub fn create_workspace(point_capacity: Option<i64>, triangle_capacity: i64) -> Result<Workspace, String> {
    if triangle_capacity < 0 {
        return Err(err_message("Planar CDT workspace triangle capacity exceeds array limits"));
    }
    let capacity = triangle_capacity.max(1);
    let edge_capacity = checked_edge_capacity(capacity)?;
    let point_capacity = match point_capacity {
        Some(count) => count,
        None => triangle_capacity.checked_add(2).ok_or_else(|| err_message("Planar CDT workspace point capacity exceeds array limits"))?,
    };
    if point_capacity <= 0 {
        return Err(err_message("Planar CDT workspace point capacity exceeds array limits"));
    }
    Ok(Workspace {
        triangle_a: vec![0; u(capacity)],
        triangle_b: vec![0; u(capacity)],
        triangle_c: vec![0; u(capacity)],
        point_triangle: vec![-1; u(point_capacity)],
        key_stride: point_capacity,
        point_count: 0,
        triangle_count: 0,
        last_output: None,
        constraints: Vec::new(),
        valid: false,
        full_build_count: 0,
        incremental_build_count: 0,
        edge_table: table_create(edge_capacity)?,
    })
}

fn ensure_workspace(workspace: &mut Workspace, required: i64) {
    let have = workspace.triangle_a.len() as i64;
    if required > have {
        let mut capacity = have.max(1);
        while capacity < required {
            capacity = capacity.checked_mul(2).unwrap_or(required);
        }
        capacity = capacity.max(required);
        workspace.triangle_a.resize(u(capacity), 0);
        workspace.triangle_b.resize(u(capacity), 0);
        workspace.triangle_c.resize(u(capacity), 0);
    }
}

fn prepare_workspace_points(workspace: &mut Workspace, required: i64) {
    if required > workspace.key_stride {
        let mut capacity = workspace.key_stride.max(1);
        while capacity < required {
            capacity = capacity.checked_mul(2).unwrap_or(required);
        }
        capacity = capacity.max(required);
        workspace.point_triangle = vec![-1; u(capacity)];
        workspace.key_stride = capacity;
    }
}

#[allow(non_snake_case)]
pub mod view {
    use super::T;
    use std::sync::Arc;

    pub type Workspace = super::Workspace;

    #[derive(Clone, Debug)]
    pub struct View {
        pub triangle_points: Arc<[i64]>,
        pub constraint_points: Arc<[i64]>,
    }

    pub fn view(value: &T) -> View {
        View {
            triangle_points: Arc::clone(&value.triangle_points),
            constraint_points: Arc::clone(&value.constraint_points),
        }
    }

    pub fn create_workspace(
        point_capacity: Option<i64>,
        triangle_capacity: i64,
    ) -> Result<Workspace, String> {
        super::create_workspace(point_capacity, triangle_capacity)
    }
}

fn opposite_sign(left: Sign, right: Sign) -> bool {
    matches!(
        (left, right),
        (Sign::Negative, Sign::Positive) | (Sign::Positive, Sign::Negative)
    )
}

fn squared_limit(count: i64) -> i64 {
    if count <= 0 { 0 } else { count.checked_mul(count).unwrap_or(i64::MAX) }
}

fn canonicalize(count: usize, triangle_a: &[i64], triangle_b: &[i64], triangle_c: &[i64]) -> Vec<i64> {
    let mut packed = scratch::filled(count * 3, 0i64);
    for triangle in 0..count {
        let (a, b, c) = (triangle_a[triangle], triangle_b[triangle], triangle_c[triangle]);
        let at = triangle * 3;
        if a <= b && a <= c {
            packed[at] = a;
            packed[at + 1] = b;
            packed[at + 2] = c;
        } else if b <= c {
            packed[at] = b;
            packed[at + 1] = c;
            packed[at + 2] = a;
        } else {
            packed[at] = c;
            packed[at + 1] = a;
            packed[at + 2] = b;
        }
    }
    let mut order = scratch::empty::<usize>();
    order.extend(0..count);
    // Equal packed triples are identical rows, so an unstable sort gives the same bytes.
    order.sort_unstable_by(|&left, &right| {
        let (l, r) = (left * 3, right * 3);
        packed[l]
            .cmp(&packed[r])
            .then(packed[l + 1].cmp(&packed[r + 1]))
            .then(packed[l + 2].cmp(&packed[r + 2]))
    });
    let mut output = vec![0i64; count * 3];
    for (target, &source) in order.iter().enumerate() {
        output[target * 3..target * 3 + 3].copy_from_slice(&packed[source * 3..source * 3 + 3]);
    }
    output
}

fn canonical_constraints(
    point_count: i64,
    key_stride: i64,
    constraints: &[i64],
    winding: Option<&[i64]>,
) -> Result<(Vec<i64>, Vec<i64>), String> {
    if constraints.len() % 2 != 0 {
        return Err(err_message("Planar CDT constraint endpoint array must contain pairs"));
    }
    let count = constraints.len() / 2;
    let winding: Vec<i64> = match winding {
        None => vec![0; count],
        Some(values) if values.len() == count => values.to_vec(),
        Some(_) => {
            return Err(err_message("Planar CDT constraint winding cardinality must match constraint count"))
        }
    };
    let mut pairs = vec![0i64; count];
    let mut weights = vec![0i64; count];
    for constraint_index in 0..count {
        let first = constraints[constraint_index * 2];
        let second = constraints[constraint_index * 2 + 1];
        if first == second {
            return Err(err_message("Planar CDT constraint has equal endpoints"));
        }
        pairs[constraint_index] = edge_key(point_count, key_stride, first, second)?;
        let weight = winding[constraint_index];
        weights[constraint_index] = if first < second {
            weight
        } else {
            weight.checked_neg().ok_or_else(|| err_message("Planar CDT constraint winding exceeds integer range"))?
        };
    }
    // Stable equal-key order makes integer weight overflow reporting deterministic.
    let mut order: Vec<usize> = (0..count).collect();
    order.sort_by(|&l, &r| pairs[l].cmp(&pairs[r]));
    let mut unique_pairs: Vec<i64> = Vec::with_capacity(count);
    let mut unique_weights: Vec<i64> = Vec::with_capacity(count);
    for index in 0..count {
        let source = order[index];
        if index == 0 || pairs[source] != pairs[order[index - 1]] {
            unique_pairs.push(pairs[source]);
            unique_weights.push(weights[source]);
        } else {
            let last = unique_weights.len() - 1;
            let previous = unique_weights[last];
            let value = weights[source];
            unique_weights[last] =
                previous.checked_add(value).ok_or_else(|| err_message("Planar CDT constraint winding exceeds integer range"))?;
        }
    }
    let mut output = vec![0i64; unique_pairs.len() * 2];
    for index in 0..unique_pairs.len() {
        output[index * 2] = unique_pairs[index] / key_stride;
        output[index * 2 + 1] = unique_pairs[index] % key_stride;
    }
    Ok((output, unique_weights))
}

/// Returns (removed, added) endpoint pairs between two sorted constraint lists.
fn constraint_delta(previous: &[i64], current: &[i64]) -> (Vec<i64>, Vec<i64>) {
    let previous_count = previous.len() / 2;
    let current_count = current.len() / 2;
    let mut removed = Vec::new();
    let mut added = Vec::new();
    let (mut p, mut c) = (0usize, 0usize);
    while p < previous_count || c < current_count {
        if p == previous_count {
            added.extend_from_slice(&current[c * 2..c * 2 + 2]);
            c += 1;
        } else if c == current_count {
            removed.extend_from_slice(&previous[p * 2..p * 2 + 2]);
            p += 1;
        } else {
            let compared = previous[p * 2]
                .cmp(&current[c * 2])
                .then(previous[p * 2 + 1].cmp(&current[c * 2 + 1]));
            match compared {
                std::cmp::Ordering::Equal => {
                    p += 1;
                    c += 1;
                }
                std::cmp::Ordering::Less => {
                    removed.extend_from_slice(&previous[p * 2..p * 2 + 2]);
                    p += 1;
                }
                std::cmp::Ordering::Greater => {
                    added.extend_from_slice(&current[c * 2..c * 2 + 2]);
                    c += 1;
                }
            }
        }
    }
    (removed, added)
}

struct St {
    point_count: i64,
    key_stride: i64,
    incremental: bool,
    has_constraints: bool,
    triangle_capacity: i64,
    triangle_count: i64,
    triangle_a: Vec<i64>,
    triangle_b: Vec<i64>,
    triangle_c: Vec<i64>,
    point_triangle: Vec<i64>,
    table: Table,
    constraints: Vec<i64>,
    constraint_winding: Vec<i64>,
    walk_start: i64,
    insertion_queue: BinaryHeap<Reverse<i64>>,
    strip_marks: scratch::Buf<u8>,
    boundary_stamps: scratch::Buf<i64>,
    boundary_edges: HashMap<i64, i64>,
    boundary_generation: i64,
}

fn ek(st: &St, first: i64, second: i64) -> Result<i64, String> {
    edge_key(st.point_count, st.key_stride, first, second)
}

fn constraint_index(st: &St, key: i64) -> Result<i64, String> {
    let mut first: i64 = 0;
    let mut last = (st.constraints.len() / 2) as i64;
    while first < last {
        let middle = first + (last - first) / 2;
        let present = ek(
            st,
            st.constraints[u(middle) * 2],
            st.constraints[u(middle) * 2 + 1],
        )?;
        if present < key {
            first = middle + 1;
        } else {
            last = middle;
        }
    }
    if first < (st.constraints.len() / 2) as i64
        && ek(
            st,
            st.constraints[u(first) * 2],
            st.constraints[u(first) * 2 + 1],
        )? == key
    {
        Ok(first)
    } else {
        Ok(-1)
    }
}

fn constraint_key(st: &St, key: i64) -> Result<bool, String> {
    Ok(constraint_index(st, key)? >= 0)
}

fn constraint_weight(st: &St, key: i64) -> Result<i64, String> {
    let index = constraint_index(st, key)?;
    Ok(if index < 0 {
        0
    } else {
        st.constraint_winding[u(index)]
    })
}

fn add_edge(st: &mut St, key: i64, triangle: i64, opposite: i64) -> Result<(), String> {
    table_add(&mut st.table, key, triangle, opposite)?;
    if st.incremental && st.has_constraints && constraint_key(st, key)? {
        table_constrain(&mut st.table, key)?;
    }
    Ok(())
}

fn tri(st: &St, triangle: i64) -> (i64, i64, i64) {
    let t = u(triangle);
    (st.triangle_a[t], st.triangle_b[t], st.triangle_c[t])
}

fn add_triangle(st: &mut St, triangle: i64) -> Result<(), String> {
    let (a, b, c) = tri(st, triangle);
    let k = ek(st, a, b)?;
    add_edge(st, k, triangle, c)?;
    let k = ek(st, b, c)?;
    add_edge(st, k, triangle, a)?;
    let k = ek(st, c, a)?;
    add_edge(st, k, triangle, b)
}

fn remove_triangle(st: &mut St, triangle: i64) -> Result<(), String> {
    let (a, b, c) = tri(st, triangle);
    let k = ek(st, a, b)?;
    table_remove(&mut st.table, k, triangle)?;
    let k = ek(st, b, c)?;
    table_remove(&mut st.table, k, triangle)?;
    let k = ek(st, c, a)?;
    table_remove(&mut st.table, k, triangle)
}

fn seed_triangle(st: &mut St, triangle: i64) {
    let (a, b, c) = tri(st, triangle);
    st.point_triangle[u(a)] = triangle;
    st.point_triangle[u(b)] = triangle;
    st.point_triangle[u(c)] = triangle;
}

fn set_triangle(st: &mut St, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), triangle: i64, a: i64, b: i64, c: i64) -> Result<(), String> {
    let t = u(triangle);
    match orient(cb, a, b, c) {
        Sign::Positive => {
            st.triangle_a[t] = a;
            st.triangle_b[t] = b;
            st.triangle_c[t] = c;
        }
        Sign::Negative => {
            st.triangle_a[t] = a;
            st.triangle_b[t] = c;
            st.triangle_c[t] = b;
        }
        Sign::Zero => return Err(err_message("Planar CDT flip produced a degenerate triangle")),
    }
    Ok(())
}

fn flip(
    st: &mut St,
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    left: i64,
    right: i64,
    left_opposite: i64,
    right_opposite: i64,
    edge_a: i64,
    edge_b: i64,
) -> Result<(), String> {
    remove_triangle(st, left)?;
    remove_triangle(st, right)?;
    set_triangle(st, cb, left, left_opposite, right_opposite, edge_a)?;
    set_triangle(st, cb, right, right_opposite, left_opposite, edge_b)?;
    add_triangle(st, left)?;
    add_triangle(st, right)?;
    seed_triangle(st, left);
    seed_triangle(st, right);
    Ok(())
}

fn append_triangle(st: &mut St, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64, c: i64) -> Result<i64, String> {
    if st.triangle_count >= st.triangle_capacity {
        return Err(err_message("Planar CDT inserted-point triangle bound was exceeded"));
    }
    let triangle = st.triangle_count;
    st.triangle_count += 1;
    set_triangle(st, cb, triangle, a, b, c)?;
    Ok(triangle)
}

fn adjacent_across(st: &St, triangle: i64, first: i64, second: i64) -> Result<i64, String> {
    let slot = table_find(&st.table, ek(st, first, second)?);
    if slot < 0 {
        return Ok(-1);
    }
    let left = st.table.first_triangles[u(slot)];
    let right = st.table.second_triangles[u(slot)];
    Ok(if left == triangle {
        right
    } else if right == triangle {
        left
    } else {
        -1
    })
}

type Edge = Option<(i64, i64, i64, i64)>;

fn choose(st: &St, edge: &mut Edge, first: i64, second: i64, opposite: i64, sign: Sign) -> Result<(), String> {
    if sign == Sign::Zero {
        let key = ek(st, first, second)?;
        match *edge {
            None => *edge = Some((key, first, second, opposite)),
            Some((present, _, _, _)) if key < present => {
                *edge = Some((key, first, second, opposite))
            }
            Some(_) => {}
        }
    }
    Ok(())
}

fn locate(st: &St, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), point: i64) -> Result<(i64, Edge), String> {
    if st.triangle_count == 0 {
        return Err(err_message("Planar CDT cannot insert a point into empty topology"));
    }
    let mut current = std::cmp::min(st.walk_start, st.triangle_count - 1);
    let mut steps: i64 = 0;
    let mut result: Option<(i64, Edge)> = None;
    while result.is_none() && steps <= st.triangle_count {
        let triangle = current;
        if triangle < 0 || triangle >= st.triangle_count {
            steps = st.triangle_count + 1;
        } else {
            let (a, b, c) = tri(st, triangle);
            let ab = orient(cb, a, b, point);
            if ab == Sign::Negative {
                current = adjacent_across(st, triangle, a, b)?;
            } else {
                let bc = orient(cb, b, c, point);
                if bc == Sign::Negative {
                    current = adjacent_across(st, triangle, b, c)?;
                } else {
                    let ca = orient(cb, c, a, point);
                    if ca == Sign::Negative {
                        current = adjacent_across(st, triangle, c, a)?;
                    } else {
                        let mut edge: Edge = None;
                        choose(st, &mut edge, a, b, c, ab)?;
                        choose(st, &mut edge, b, c, a, bc)?;
                        choose(st, &mut edge, c, a, b, ca)?;
                        result = Some((triangle, edge));
                    }
                }
            }
        }
        steps += 1;
    }
    if let Some(value) = result {
        return Ok(value);
    }
    // Audited compatibility oracle for a stale walk seed or a point exactly on a high-valence fan.
    let mut triangle: i64 = 0;
    while triangle < st.triangle_count {
        let (a, b, c) = tri(st, triangle);
        let ab = orient(cb, a, b, point);
        let bc = orient(cb, b, c, point);
        let ca = orient(cb, c, a, point);
        if ab != Sign::Negative && bc != Sign::Negative && ca != Sign::Negative {
            let mut edge: Edge = None;
            choose(st, &mut edge, a, b, c, ab)?;
            choose(st, &mut edge, b, c, a, bc)?;
            choose(st, &mut edge, c, a, b, ca)?;
            return Ok((triangle, edge));
        }
        triangle += 1;
    }
    Err(err_message("Planar CDT inserted point lies outside seed triangulation"))
}

fn enqueue_opposite(st: &mut St, point: i64, triangle: i64) -> Result<(), String> {
    let (a, b, c) = tri(st, triangle);
    if a == point {
        let k = ek(st, b, c)?;
        st.insertion_queue.push(Reverse(k));
    } else if b == point {
        let k = ek(st, c, a)?;
        st.insertion_queue.push(Reverse(k));
    } else if c == point {
        let k = ek(st, a, b)?;
        st.insertion_queue.push(Reverse(k));
    }
    Ok(())
}

fn is_illegal(orientation: Sign, circle: Sign, a_key: i64, key: i64) -> bool {
    match (orientation, circle) {
        (Sign::Positive, Sign::Positive) | (Sign::Negative, Sign::Negative) => true,
        (_, Sign::Zero) => a_key < key,
        _ => false,
    }
}

fn legalize_inserted(st: &mut St, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), point: i64, triangles: &[i64]) -> Result<(), String> {
    st.insertion_queue.clear();
    for &triangle in triangles {
        enqueue_opposite(st, point, triangle)?;
    }
    let mut flips: i64 = 0;
    while let Some(Reverse(key)) = st.insertion_queue.pop() {
        let slot = table_find(&st.table, key);
        if slot >= 0 && st.table.second_triangles[u(slot)] >= 0 {
            let s = u(slot);
            let first = st.table.first_triangles[s];
            let second = st.table.second_triangles[s];
            let first_opposite = st.table.first_opposites[s];
            let second_opposite = st.table.second_opposites[s];
            let edge_a = key / st.key_stride;
            let edge_b = key % st.key_stride;
            if first_opposite == point || second_opposite == point {
                let (inserted, opposite, left, right) = if first_opposite == point {
                    (first_opposite, second_opposite, first, second)
                } else {
                    (second_opposite, first_opposite, second, first)
                };
                let circle = incircle(cb, edge_a, edge_b, inserted, opposite);
                let orientation = orient(cb, edge_a, edge_b, inserted);
                let illegal = is_illegal(orientation, circle, ek(st, inserted, opposite)?, key);
                if illegal
                    && opposite_sign(
                        orient(cb, inserted, opposite, edge_a),
                        orient(cb, inserted, opposite, edge_b),
                    )
                {
                    flips += 1;
                    if flips > squared_limit(st.triangle_count) {
                        return Err(err_message("Planar CDT inserted-point legalization did not converge"));
                    }
                    flip(st, cb, left, right, inserted, opposite, edge_a, edge_b)?;
                    enqueue_opposite(st, point, left)?;
                    enqueue_opposite(st, point, right)?;
                }
            }
        }
    }
    Ok(())
}

fn properly_crosses(cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64, u_: i64, v: i64) -> bool {
    u_ != a
        && u_ != b
        && v != a
        && v != b
        && bounds_overlap(cb, a, b, u_, v)
        && opposite_sign(orient(cb, a, b, u_), orient(cb, a, b, v))
        && opposite_sign(orient(cb, u_, v, a), orient(cb, u_, v, b))
}

fn triangle_has(st: &St, triangle: i64, point: i64) -> bool {
    let (a, b, c) = tri(st, triangle);
    a == point || b == point || c == point
}

fn crossing_key(st: &St, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64, triangle: i64, entry: i64) -> Result<i64, String> {
    let mut found: i64 = -1;
    let (ta, tb, tc) = tri(st, triangle);
    for (p, q) in [(ta, tb), (tb, tc), (tc, ta)] {
        let key = ek(st, p, q)?;
        if key != entry && properly_crosses(cb, a, b, p, q) && (found < 0 || key < found) {
            found = key;
        }
    }
    Ok(found)
}

fn adjacent(st: &St, triangle: i64, p: i64, q: i64) -> Result<i64, String> {
    let slot = table_find(&st.table, ek(st, p, q)?);
    if slot < 0 {
        return Ok(-1);
    }
    let first = st.table.first_triangles[u(slot)];
    let second = st.table.second_triangles[u(slot)];
    Ok(if first == triangle {
        second
    } else if second == triangle {
        first
    } else {
        -1
    })
}

fn other(st: &St, triangle: i64, p: i64, q: i64) -> i64 {
    let (a, b, c) = tri(st, triangle);
    if a != p && a != q {
        a
    } else if b != p && b != q {
        b
    } else {
        c
    }
}

fn search_fan(st: &St, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64, seed: i64, entry_vertex: i64) -> Result<i64, String> {
    let mut current = adjacent(st, seed, a, entry_vertex)?;
    let mut entry = entry_vertex;
    let mut steps: i64 = 0;
    let mut found: i64 = -1;
    while found < 0 && current >= 0 && current != seed && steps <= st.triangle_count {
        if crossing_key(st, cb, a, b, current, -1)? >= 0 {
            found = current;
        } else {
            let exit = other(st, current, a, entry);
            let next = adjacent(st, current, a, exit)?;
            entry = exit;
            current = next;
        }
        steps += 1;
    }
    Ok(found)
}

fn incident_start(st: &St, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), a: i64, b: i64) -> Result<i64, String> {
    let seed = st.point_triangle[u(a)];
    if seed < 0 || !triangle_has(st, seed, a) {
        return Ok(-1);
    }
    if crossing_key(st, cb, a, b, seed, -1)? >= 0 {
        return Ok(seed);
    }
    let (ta, tb, tc) = tri(st, seed);
    let (first, second) = if ta == a {
        (tb, tc)
    } else if tb == a {
        (tc, ta)
    } else {
        (ta, tb)
    };
    let found = search_fan(st, cb, a, b, seed, first)?;
    if found >= 0 {
        Ok(found)
    } else {
        search_fan(st, cb, a, b, seed, second)
    }
}

/// Planar CDT's exact-predicate ear clip: the lowest-point ear, not the first.
fn ear_clip(
    cancel_value: Option<&cancel::T>,
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    a: i64,
    b: i64,
    path: &[i64],
    emitted: &mut Vec<i64>,
    emitted_count: &mut i64,
    strip_count: i64,
) -> Result<(), String> {
    let size = path.len();
    if size >= 3 {
        let mut remaining: Vec<usize> = (0..size).collect();
        let mut active = size;
        while active > 3 {
            check(cancel_value)?;
            let mut best_slot: i64 = -1;
            let mut best_point = i64::MAX;
            for slot in 0..active {
                let previous = remaining[(slot + active - 1) % active];
                let current = remaining[slot];
                let next = remaining[(slot + 1) % active];
                let (pa, pb, pc) = (path[previous], path[current], path[next]);
                if orient(cb, pa, pb, pc) == Sign::Positive {
                    let mut blocked = false;
                    let mut other = 0usize;
                    while !blocked && other < active {
                        let candidate = remaining[other];
                        if candidate != previous && candidate != current && candidate != next {
                            let p = path[candidate];
                            blocked = p != pa
                                && p != pb
                                && p != pc
                                && orient(cb, pa, pb, p) != Sign::Negative
                                && orient(cb, pb, pc, p) != Sign::Negative
                                && orient(cb, pc, pa, p) != Sign::Negative;
                        }
                        other += 1;
                    }
                    if !blocked && pb < best_point {
                        best_point = pb;
                        best_slot = slot as i64;
                    }
                }
            }
            if best_slot < 0 {
                let rendered: Vec<String> = path.iter().map(|p| p.to_string()).collect();
                return Err(format!(
                    "Planar CDT constraint cavity is non-simple or degenerate (constraint={},{} path={})",
                    a,
                    b,
                    rendered.join(":")
                ));
            }
            let best_slot = best_slot as usize;
            let previous = remaining[(best_slot + active - 1) % active];
            let current = remaining[best_slot];
            let next = remaining[(best_slot + 1) % active];
            emit(
                cb,
                emitted,
                emitted_count,
                strip_count,
                path[previous],
                path[current],
                path[next],
            )?;
            remaining.copy_within(best_slot + 1..active, best_slot);
            active -= 1;
        }
        emit(
            cb,
            emitted,
            emitted_count,
            strip_count,
            path[remaining[0]],
            path[remaining[1]],
            path[remaining[2]],
        )?;
    }
    Ok(())
}

fn write_emitted(cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool), emitted: &mut [i64], triangle: i64, a: i64, b: i64, c: i64) -> Result<(), String> {
    let at = u(triangle) * 3;
    match orient(cb, a, b, c) {
        Sign::Positive => {
            emitted[at] = a;
            emitted[at + 1] = b;
            emitted[at + 2] = c;
        }
        Sign::Negative => {
            emitted[at] = a;
            emitted[at + 1] = c;
            emitted[at + 2] = b;
        }
        Sign::Zero => {
            return Err(err_message("Planar CDT constraint cavity emitted a degenerate triangle"));
        }
    }
    Ok(())
}

fn emit(
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    emitted: &mut Vec<i64>,
    emitted_count: &mut i64,
    strip_count: i64,
    a: i64,
    b: i64,
    c: i64,
) -> Result<(), String> {
    if *emitted_count >= strip_count {
        return Err(err_message("Planar CDT constraint cavity exceeded its triangle budget"));
    }
    write_emitted(cb, emitted, *emitted_count, a, b, c)?;
    *emitted_count += 1;
    Ok(())
}

fn point_index(point: i64, cycle: &[i64]) -> i64 {
    for (index, &value) in cycle.iter().enumerate() {
        if value == point {
            return index as i64;
        }
    }
    -1
}

fn bridge_cycle(
    st: &mut St,
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    a: i64,
    b: i64,
    strip: &[i64],
    outer: &[i64],
    hole: &[i64],
) -> Result<Vec<i64>, String> {
    let point_count = u(st.point_count);
    let mut target = vec![0u8; point_count];
    for &point in hole {
        target[u(point)] = 1;
    }
    let mut parent = vec![-2i64; point_count];
    let mut queue = vec![0i64; point_count];
    let mut first = 0usize;
    let mut last = 0usize;
    for &point in outer {
        if parent[u(point)] == -2 {
            parent[u(point)] = -1;
            queue[last] = point;
            last += 1;
        }
    }
    let mut reached: i64 = -1;
    while reached < 0 && first < last {
        let point = queue[first];
        first += 1;
        if target[u(point)] != 0 {
            reached = point;
        } else {
            for &triangle in strip {
                let (ta, tb, tc) = tri(st, triangle);
                let neighbors = if ta == point {
                    Some((tb, tc))
                } else if tb == point {
                    Some((ta, tc))
                } else if tc == point {
                    Some((ta, tb))
                } else {
                    None
                };
                if let Some((n1, n2)) = neighbors {
                    for neighbor in [n1, n2] {
                        if parent[u(neighbor)] == -2 && !properly_crosses(cb, a, b, point, neighbor)
                        {
                            parent[u(neighbor)] = point;
                            queue[last] = neighbor;
                            last += 1;
                        }
                    }
                }
            }
        }
    }
    if reached < 0 {
        return Err(err_message("Planar CDT could not bridge a constraint-strip hole"));
    }
    let mut reverse_path: Vec<i64> = Vec::new();
    let mut point = reached;
    while point >= 0 {
        reverse_path.push(point);
        point = parent[u(point)];
    }
    let path: Vec<i64> = reverse_path.iter().rev().copied().collect();
    for &point in &path {
        st.boundary_stamps[u(point)] = st.boundary_generation;
    }
    let outer_index = point_index(path[0], outer);
    let hole_index = point_index(path[path.len() - 1], hole);
    if outer_index < 0 || hole_index < 0 {
        return Err(err_message("Planar CDT hole bridge endpoints are not on their cycles"));
    }
    let (outer_index, hole_index) = (u(outer_index), u(hole_index));
    let outer_count = outer.len();
    let hole_count = hole.len();
    let path_count = path.len();
    let output_count = outer_count + hole_count + 2 * (path_count - 1);
    let mut output: Vec<i64> = Vec::with_capacity(output_count);
    output.push(outer[outer_index]);
    for index in 1..path_count {
        output.push(path[index]);
    }
    for offset in 1..hole_count {
        output.push(hole[(hole_index + offset) % hole_count]);
    }
    output.push(hole[hole_index]);
    for index in (0..path_count - 1).rev() {
        output.push(path[index]);
    }
    for offset in 1..outer_count {
        output.push(outer[(outer_index + offset) % outer_count]);
    }
    if output.len() != output_count {
        return Err(err_message("Planar CDT hole bridge cardinality is inconsistent"));
    }
    Ok(output)
}

fn cyclic_path(cycle: &[i64], first: usize, last: usize) -> Vec<i64> {
    let size = cycle.len();
    let count = (last + size - first) % size + 1;
    (0..count).map(|index| cycle[(first + index) % size]).collect()
}

fn triangle_has_edge(emitted: &[i64], triangle: usize, p: i64, q: i64) -> bool {
    let at = triangle * 3;
    let (a, b, c) = (emitted[at], emitted[at + 1], emitted[at + 2]);
    (a == p || b == p || c == p) && (a == q || b == q || c == q)
}

fn split_triangle_edge(
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    emitted: &mut Vec<i64>,
    emitted_count: &mut i64,
    strip_count: i64,
    triangle: i64,
    p: i64,
    q: i64,
    point: i64,
) -> Result<(), String> {
    let at = u(triangle) * 3;
    let (a, b, c) = (emitted[at], emitted[at + 1], emitted[at + 2]);
    let opposite = if a != p && a != q {
        a
    } else if b != p && b != q {
        b
    } else {
        c
    };
    write_emitted(cb, emitted, triangle, p, point, opposite)?;
    emit(cb, emitted, emitted_count, strip_count, point, q, opposite)
}

fn recover_constraint(
    st: &mut St,
    cancel_value: Option<&cancel::T>,
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    a: i64,
    b: i64,
) -> Result<(), String> {
    let mut strip = scratch::empty::<i64>();
    let mut current = incident_start(st, cb, a, b)?;
    let mut entry_key: i64 = -1;
    let mut steps: i64 = 0;
    let mut complete = false;
    while !complete && current >= 0 && steps <= st.triangle_count {
        if st.strip_marks[u(current)] != 0 {
            return Err(err_message("Planar CDT constraint trace entered a cycle"));
        }
        st.strip_marks[u(current)] = 1;
        strip.push(current);
        if triangle_has(st, current, b) {
            complete = true;
        } else {
            let key = crossing_key(st, cb, a, b, current, entry_key)?;
            let slot = if key < 0 { -1 } else { table_find(&st.table, key) };
            if slot < 0 || st.table.second_triangles[u(slot)] < 0 {
                current = -1;
            } else if constraint_key(st, key)? {
                return Err(err_message("Planar CDT constraints cross; crossing splitting is required"));
            } else {
                let left = st.table.first_triangles[u(slot)];
                let right = st.table.second_triangles[u(slot)];
                let next = if left == current {
                    right
                } else if right == current {
                    left
                } else {
                    -1
                };
                entry_key = key;
                current = next;
            }
        }
        steps += 1;
    }
    if !complete {
        return Err(err_message("Planar CDT constraint trace failed; an open constraint segment contains an unsplit point"));
    }
    let strip_count = strip.len() as i64;
    let mut boundary_u = scratch::empty::<i64>();
    let mut boundary_v = scratch::empty::<i64>();
    let mut boundary_triangle = scratch::empty::<i64>();
    for index in 0..strip.len() {
        let triangle = strip[index];
        let (ta, tb, tc) = tri(st, triangle);
        for (first, second) in [(ta, tb), (tb, tc), (tc, ta)] {
            let slot = table_find(&st.table, ek(st, first, second)?);
            if slot < 0 {
                return Err(err_message("Planar CDT strip lost a boundary edge"));
            }
            let left = st.table.first_triangles[u(slot)];
            let right = st.table.second_triangles[u(slot)];
            let outside = if left == triangle {
                right
            } else if right == triangle {
                left
            } else {
                return Err(err_message("Planar CDT strip edge incidence is inconsistent"));
            };
            if outside < 0 || st.strip_marks[u(outside)] == 0 {
                boundary_u.push(first);
                boundary_v.push(second);
                boundary_triangle.push(triangle);
            }
        }
    }
    let boundary_count = boundary_u.len();
    st.boundary_generation += 1;
    if st.boundary_generation == i64::MAX {
        st.boundary_stamps.iter_mut().for_each(|s| *s = 0);
        st.boundary_generation = 1;
    }
    for edge in 0..boundary_count {
        st.boundary_stamps[u(boundary_u[edge])] = st.boundary_generation;
        st.boundary_stamps[u(boundary_v[edge])] = st.boundary_generation;
    }
    if st.boundary_stamps[u(a)] != st.boundary_generation
        || st.boundary_stamps[u(b)] != st.boundary_generation
    {
        return Err(err_message("Planar CDT constraint endpoints are absent from the strip boundary"));
    }
    let point_count = st.point_count;
    let directed_key = |first: i64, second: i64| first * point_count + second;
    st.boundary_edges.clear();
    for edge in 0..boundary_count {
        st.boundary_edges
            .insert(directed_key(boundary_u[edge], boundary_v[edge]), edge as i64);
    }
    let next_boundary = |st: &St, edge: usize| -> Result<usize, String> {
        let mut current_triangle = boundary_triangle[edge];
        let mut first = boundary_u[edge];
        let second = boundary_v[edge];
        let mut next: i64 = -1;
        let mut steps: i64 = 0;
        while next < 0 && steps <= strip_count {
            let (ta, tb, tc) = tri(st, current_triangle);
            let third = if ta == first && tb == second {
                tc
            } else if tb == first && tc == second {
                ta
            } else if tc == first && ta == second {
                tb
            } else {
                return Err(err_message("Planar CDT boundary walk lost its directed triangle edge"));
            };
            let slot = table_find(&st.table, ek(st, second, third)?);
            if slot < 0 {
                return Err(err_message("Planar CDT boundary walk lost an edge"));
            }
            let left = st.table.first_triangles[u(slot)];
            let right = st.table.second_triangles[u(slot)];
            let adjacent = if left == current_triangle {
                right
            } else if right == current_triangle {
                left
            } else {
                return Err(err_message("Planar CDT boundary walk found inconsistent incidence"));
            };
            if adjacent < 0 || st.strip_marks[u(adjacent)] == 0 {
                next = match st.boundary_edges.get(&directed_key(second, third)) {
                    Some(&value) => value,
                    None => return Err(err_message("Planar CDT boundary walk did not find its next edge")),
                };
            } else {
                current_triangle = adjacent;
                first = third;
            }
            steps += 1;
        }
        if next < 0 {
            return Err(err_message("Planar CDT boundary walk did not terminate"));
        }
        Ok(next as usize)
    };
    let mut visited_boundary = scratch::filled(boundary_count, 0u8);
    let mut cycles: Vec<Vec<i64>> = Vec::new();
    for start in 0..boundary_count {
        if visited_boundary[start] == 0 {
            let mut values: Vec<i64> = Vec::new();
            let mut current = start;
            while visited_boundary[current] == 0 {
                visited_boundary[current] = 1;
                values.push(boundary_u[current]);
                current = next_boundary(st, current)?;
            }
            if current != start {
                return Err(err_message("Planar CDT strip boundary cycles merge"));
            }
            cycles.push(values);
        }
    }
    let main_cycles: Vec<usize> = (0..cycles.len())
        .filter(|&i| cycles[i].contains(&a) && cycles[i].contains(&b))
        .collect();
    let initial_main = if main_cycles.len() == 1 {
        main_cycles[0]
    } else if main_cycles.is_empty() {
        return Err(err_message("Planar CDT constraint strip did not reach its endpoint"));
    } else {
        return Err(err_message("Planar CDT constraint belongs to multiple strip cycles"));
    };
    let mut main_cycle = cycles[initial_main].clone();
    for index in 0..cycles.len() {
        if index != initial_main {
            main_cycle = bridge_cycle(st, cb, a, b, &strip, &main_cycle, &cycles[index])?;
        }
    }
    let mut emitted = scratch::filled(strip.len() * 3, 0i64);
    let mut emitted_count: i64 = 0;
    {
        let a_index = point_index(a, &main_cycle);
        let b_index = point_index(b, &main_cycle);
        if a_index < 0 || b_index < 0 {
            return Err(err_message("Planar CDT constraint endpoints are absent from main strip cycle"));
        }
        let (a_index, b_index) = (u(a_index), u(b_index));
        let path = cyclic_path(&main_cycle, a_index, b_index);
        ear_clip(
            cancel_value,
            cb,
            a,
            b,
            &path,
            &mut emitted,
            &mut emitted_count,
            strip_count,
        )?;
        let path = cyclic_path(&main_cycle, b_index, a_index);
        ear_clip(
            cancel_value,
            cb,
            a,
            b,
            &path,
            &mut emitted,
            &mut emitted_count,
            strip_count,
        )?;
    }
    let mut interior = scratch::empty::<i64>();
    let mut interior_seen = scratch::filled(u(st.point_count), 0u8);
    for index in 0..strip.len() {
        let (ta, tb, tc) = tri(st, strip[index]);
        for point in [ta, tb, tc] {
            if st.boundary_stamps[u(point)] != st.boundary_generation
                && interior_seen[u(point)] == 0
            {
                interior_seen[u(point)] = 1;
                interior.push(point);
            }
        }
    }
    interior.sort();
    let interior_count = interior.len();
    for &point in &interior {
        check(cancel_value)?;
        let mut containing: i64 = -1;
        let mut edge_u: i64 = -1;
        let mut edge_v: i64 = -1;
        let mut triangle: i64 = 0;
        while containing < 0 && triangle < emitted_count {
            let at = u(triangle) * 3;
            let (ea, eb, ec) = (emitted[at], emitted[at + 1], emitted[at + 2]);
            let ab = orient(cb, ea, eb, point);
            let bc = orient(cb, eb, ec, point);
            let ca = orient(cb, ec, ea, point);
            if ab != Sign::Negative && bc != Sign::Negative && ca != Sign::Negative {
                containing = triangle;
                if ab == Sign::Zero {
                    edge_u = ea;
                    edge_v = eb;
                } else if bc == Sign::Zero {
                    edge_u = eb;
                    edge_v = ec;
                } else if ca == Sign::Zero {
                    edge_u = ec;
                    edge_v = ea;
                }
            }
            triangle += 1;
        }
        if containing < 0 {
            return Err(err_message("Planar CDT could not reinsert a constraint-cavity point"));
        }
        if edge_u < 0 {
            let at = u(containing) * 3;
            let (ea, eb, ec) = (emitted[at], emitted[at + 1], emitted[at + 2]);
            write_emitted(cb, &mut emitted, containing, ea, eb, point)?;
            emit(cb, &mut emitted, &mut emitted_count, strip_count, eb, ec, point)?;
            emit(cb, &mut emitted, &mut emitted_count, strip_count, ec, ea, point)?;
        } else {
            let mut adjacent_triangle: i64 = -1;
            for candidate in 0..emitted_count {
                if candidate != containing && triangle_has_edge(&emitted, u(candidate), edge_u, edge_v)
                {
                    adjacent_triangle = candidate;
                }
            }
            if adjacent_triangle < 0 {
                return Err(err_message("Planar CDT cavity point lies on its unsplit boundary"));
            }
            let first = containing;
            let second = adjacent_triangle;
            split_triangle_edge(
                cb,
                &mut emitted,
                &mut emitted_count,
                strip_count,
                first,
                edge_u,
                edge_v,
                point,
            )?;
            split_triangle_edge(
                cb,
                &mut emitted,
                &mut emitted_count,
                strip_count,
                second,
                edge_u,
                edge_v,
                point,
            )?;
        }
    }
    if emitted_count != strip_count {
        return Err(format!(
            "Planar CDT constraint cavity changed triangle cardinality ({} strip triangles, {} boundary edges, {} cycles, {} interior points, {} emitted)",
            strip_count, boundary_count, 1, interior_count, emitted_count
        ));
    }
    for index in 0..strip.len() {
        remove_triangle(st, strip[index])?;
    }
    for index in 0..strip.len() {
        let at = index * 3;
        set_triangle(
            st,
            cb,
            strip[index],
            emitted[at],
            emitted[at + 1],
            emitted[at + 2],
        )?;
    }
    for index in 0..strip.len() {
        add_triangle(st, strip[index])?;
        seed_triangle(st, strip[index]);
        st.strip_marks[u(strip[index])] = 0;
    }
    Ok(())
}

fn recover_constraints(
    st: &mut St,
    cancel_value: Option<&cancel::T>,
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    values: &[i64],
) -> Result<(), String> {
    for constraint_index in 0..values.len() / 2 {
        poll(cancel_value, constraint_index as i64, 255)?;
        let a = values[constraint_index * 2];
        let b = values[constraint_index * 2 + 1];
        let key = ek(st, a, b)?;
        if table_find(&st.table, key) < 0 {
            recover_constraint(st, cancel_value, cb, a, b)?;
        }
        if table_find(&st.table, key) < 0 {
            return Err(err_message("Planar CDT cavity retriangulation did not recover its constraint"));
        }
        table_constrain(&mut st.table, key)?;
    }
    Ok(())
}

fn checked_add(left: i64, right: i64) -> Result<i64, String> {
    left.checked_add(right).ok_or_else(|| err_message("Planar CDT polygon winding exceeds integer range"))
}

fn checked_negate(value: i64) -> Result<i64, String> {
    value.checked_neg().ok_or_else(|| err_message("Planar CDT polygon winding exceeds integer range"))
}

struct Outcome {
    triangle_a: Vec<i64>,
    triangle_b: Vec<i64>,
    triangle_c: Vec<i64>,
    count: i64,
    constraints: Vec<i64>,
}

fn filter_outside(
    st: &St,
    cancel_value: Option<&cancel::T>,
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    flood_from_hull_boundary: bool,
    remove_outside_constraint_polygons: bool,
) -> Result<Outcome, String> {
    let triangle_count = st.triangle_count;
    let n = u(triangle_count);
    let table = &st.table;
    let mut outside = if flood_from_hull_boundary { vec![0u8; n] } else { Vec::new() };
    let mut queue = if flood_from_hull_boundary { vec![0i64; n] } else { Vec::new() };
    let mut read = 0usize;
    let mut write = 0usize;
    if flood_from_hull_boundary {
        let mark = |triangle: i64, outside: &mut Vec<u8>, queue: &mut Vec<i64>, write: &mut usize| {
            if triangle >= 0 && outside[u(triangle)] == 0 {
                outside[u(triangle)] = 1;
                queue[*write] = triangle;
                *write += 1;
            }
        };
        for slot in 0..table.keys.len() {
            if table.keys[slot] >= 0 && table.second_triangles[slot] < 0 && table.constrained[slot] == 0 {
                mark(table.first_triangles[slot], &mut outside, &mut queue, &mut write);
            }
        }
        while read < write {
            poll(cancel_value, read as i64, 4095)?;
            let triangle = queue[read];
            read += 1;
            let (a, b, c) = tri(st, triangle);
            for (p, q) in [(a, b), (b, c), (c, a)] {
                let slot = table_find(table, ek(st, p, q)?);
                if slot < 0 {
                    return Err(err_message("Planar CDT outside flood lost an edge"));
                }
                if table.constrained[u(slot)] == 0 {
                    let first = table.first_triangles[u(slot)];
                    let second = table.second_triangles[u(slot)];
                    mark(if first == triangle { second } else { first }, &mut outside, &mut queue, &mut write);
                }
            }
        }
    }
    let mut winding = if remove_outside_constraint_polygons { vec![0i64; n] } else { Vec::new() };
    let mut winding_known = if remove_outside_constraint_polygons { vec![0u8; n] } else { Vec::new() };
    let mut winding_queue = if remove_outside_constraint_polygons { vec![0i64; n] } else { Vec::new() };
    let mut winding_read = 0usize;
    let mut winding_write = 0usize;
    if remove_outside_constraint_polygons {
        let assign = |triangle: i64,
                      value: i64,
                      winding: &mut Vec<i64>,
                      known: &mut Vec<u8>,
                      queue: &mut Vec<i64>,
                      write: &mut usize|
         -> Result<(), String> {
            if known[u(triangle)] == 0 {
                known[u(triangle)] = 1;
                winding[u(triangle)] = value;
                queue[*write] = triangle;
                *write += 1;
            } else if winding[u(triangle)] != value {
                return Err(err_message("Planar CDT polygon constraints have inconsistent winding"));
            }
            Ok(())
        };
        for slot in 0..table.keys.len() {
            let key = table.keys[slot];
            if key >= 0 && table.second_triangles[slot] < 0 {
                let triangle = table.first_triangles[slot];
                let opposite = table.first_opposites[slot];
                let (pu, pv) = (key / st.key_stride, key % st.key_stride);
                let weight = constraint_weight(st, key)?;
                let value = if weight == 0 {
                    0
                } else {
                    match orient(cb, pu, pv, opposite) {
                        Sign::Positive => weight,
                        Sign::Negative => checked_negate(weight)?,
                        Sign::Zero => return Err(err_message("Planar CDT hull contains a degenerate edge")),
                    }
                };
                assign(
                    triangle,
                    value,
                    &mut winding,
                    &mut winding_known,
                    &mut winding_queue,
                    &mut winding_write,
                )?;
            }
        }
        while winding_read < winding_write {
            poll(cancel_value, winding_read as i64, 4095)?;
            let triangle = winding_queue[winding_read];
            winding_read += 1;
            let (a, b, c) = tri(st, triangle);
            for (pu, pv, opposite) in [(a, b, c), (b, c, a), (c, a, b)] {
                let key = ek(st, pu, pv)?;
                let slot = table_find(table, key);
                if slot < 0 {
                    return Err(err_message("Planar CDT winding propagation lost an edge"));
                }
                let first = table.first_triangles[u(slot)];
                let second = table.second_triangles[u(slot)];
                let neighbor = if first == triangle { second } else { first };
                if neighbor >= 0 {
                    let canonical_u = key / st.key_stride;
                    let canonical_v = key % st.key_stride;
                    let weight = constraint_weight(st, key)?;
                    let delta = if weight == 0 {
                        0
                    } else {
                        match orient(cb, canonical_u, canonical_v, opposite) {
                            Sign::Positive => checked_negate(weight)?,
                            Sign::Negative => weight,
                            Sign::Zero => {
                                return Err(err_message("Planar CDT triangle contains a degenerate edge"))
                            }
                        }
                    };
                    let value = checked_add(winding[u(triangle)], delta)?;
                    assign(
                        neighbor,
                        value,
                        &mut winding,
                        &mut winding_known,
                        &mut winding_queue,
                        &mut winding_write,
                    )?;
                }
            }
        }
        for triangle in 0..n {
            if winding_known[triangle] == 0 {
                return Err(err_message("Planar CDT could not classify polygon winding"));
            }
        }
    }
    let mut removed = vec![0u8; n];
    let mut removed_count = 0usize;
    for triangle in 0..n {
        if (flood_from_hull_boundary && outside[triangle] != 0)
            || (remove_outside_constraint_polygons && winding[triangle] == 0)
        {
            removed[triangle] = 1;
            removed_count += 1;
        }
    }
    let kept = n - removed_count;
    let mut output_a = Vec::with_capacity(kept);
    let mut output_b = Vec::with_capacity(kept);
    let mut output_c = Vec::with_capacity(kept);
    for triangle in 0..n {
        if removed[triangle] == 0 {
            output_a.push(st.triangle_a[triangle]);
            output_b.push(st.triangle_b[triangle]);
            output_c.push(st.triangle_c[triangle]);
        }
    }
    let mut kept_constraints: Vec<i64> = Vec::new();
    for constraint_index in 0..st.constraints.len() / 2 {
        let a = st.constraints[constraint_index * 2];
        let b = st.constraints[constraint_index * 2 + 1];
        let slot = table_find(table, ek(st, a, b)?);
        if slot < 0 {
            return Err(err_message("Planar CDT outside flood lost a constrained edge"));
        }
        let first = table.first_triangles[u(slot)];
        let second = table.second_triangles[u(slot)];
        if (first >= 0 && removed[u(first)] == 0) || (second >= 0 && removed[u(second)] == 0) {
            kept_constraints.push(a);
            kept_constraints.push(b);
        }
    }
    Ok(Outcome {
        triangle_a: output_a,
        triangle_b: output_b,
        triangle_c: output_c,
        count: kept as i64,
        constraints: kept_constraints,
    })
}

fn delaunay_repair(st: &mut St, cancel_value: Option<&cancel::T>, cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool)) -> Result<(), String> {
    let mut queue = BinaryHeap::new();
    for slot in 0..st.table.keys.len() {
        if st.table.keys[slot] >= 0
            && st.table.second_triangles[slot] >= 0
            && st.table.constrained[slot] == 0
        {
            queue.push(Reverse(st.table.keys[slot]));
        }
    }
    let mut flips: i64 = 0;
    while let Some(Reverse(key)) = queue.pop() {
        poll(cancel_value, flips, 255)?;
        let slot = table_find(&st.table, key);
        if slot >= 0
            && st.table.second_triangles[u(slot)] >= 0
            && st.table.constrained[u(slot)] == 0
        {
            let s = u(slot);
            let left = st.table.first_triangles[s];
            let right = st.table.second_triangles[s];
            let lo = st.table.first_opposites[s];
            let ro = st.table.second_opposites[s];
            let ea = key / st.key_stride;
            let eb = key % st.key_stride;
            let circle = incircle(cb, ea, eb, lo, ro);
            let orientation = orient(cb, ea, eb, lo);
            let illegal = is_illegal(orientation, circle, ek(st, lo, ro)?, key);
            if illegal && opposite_sign(orient(cb, lo, ro, ea), orient(cb, lo, ro, eb)) {
                flips += 1;
                if flips > squared_limit(st.triangle_count) {
                    return Err(err_message("Planar CDT Delaunay repair did not converge"));
                }
                flip(st, cb, left, right, lo, ro, ea, eb)?;
                for triangle in [left, right] {
                    let (a, b, c) = tri(st, triangle);
                    queue.push(Reverse(ek(st, a, b)?));
                    queue.push(Reverse(ek(st, b, c)?));
                    queue.push(Reverse(ek(st, c, a)?));
                }
            }
        }
    }
    Ok(())
}

struct Request<'a> {
    incremental: bool,
    point_count: i64,
    initial_triangle_count: usize,
    triangle_points: &'a [i64],
    insert_points: &'a [i64],
    flood: bool,
    remove_outside: bool,
    constraint_points: &'a [i64],
    constraint_winding: Option<&'a [i64]>,
}

fn run(
    st: &mut St,
    cb: &(&dyn Fn(i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> Sign, &dyn Fn(i64, i64, i64, i64) -> bool),
    cancel_value: Option<&cancel::T>,
    req: &Request<'_>,
    previous_constraints: &[i64],
) -> Result<T, String> {
    let point_count = req.point_count;
    let initial_triangle_count = req.initial_triangle_count;
    let insert_points = &req.insert_points[..];
    let triangle_points = &req.triangle_points[..];
    let incremental = req.incremental;
    if !incremental {
        let mut referenced = scratch::filled(u(point_count), 0u8);
        for triangle in 0..initial_triangle_count {
            let a = triangle_points[triangle * 3];
            let b = triangle_points[triangle * 3 + 1];
            let c = triangle_points[triangle * 3 + 2];
            if a < 0 || a >= point_count || b < 0 || b >= point_count || c < 0 || c >= point_count {
                return Err(err_message("Planar CDT triangle point is out of range"));
            }
            match orient(cb, a, b, c) {
                Sign::Positive => {
                    st.triangle_a[triangle] = a;
                    st.triangle_b[triangle] = b;
                    st.triangle_c[triangle] = c;
                }
                Sign::Negative => {
                    st.triangle_a[triangle] = a;
                    st.triangle_b[triangle] = c;
                    st.triangle_c[triangle] = b;
                }
                Sign::Zero => return Err(err_message("Planar CDT input contains a degenerate triangle")),
            }
            referenced[u(a)] = 1;
            referenced[u(b)] = 1;
            referenced[u(c)] = 1;
        }
        for &point in insert_points {
            if point < 0 || point >= point_count {
                return Err(err_message("Planar CDT inserted point is out of range"));
            }
            if referenced[u(point)] != 0 {
                return Err(err_message("Planar CDT inserted point is duplicated or already referenced"));
            }
            referenced[u(point)] = 1;
        }
    } else {
        for triangle in 0..initial_triangle_count {
            let (a, b, c) = (
                st.triangle_a[triangle],
                st.triangle_b[triangle],
                st.triangle_c[triangle],
            );
            match orient(cb, a, b, c) {
                Sign::Positive => {}
                Sign::Negative => {
                    st.triangle_b[triangle] = c;
                    st.triangle_c[triangle] = b;
                }
                Sign::Zero => return Err(err_message("Planar CDT input contains a degenerate triangle")),
            }
        }
    }
    let (constraints, constraint_winding) = canonical_constraints(
        point_count,
        st.key_stride,
        &req.constraint_points,
        req.constraint_winding,
    )?;
    let (_removed_constraints, added_constraints) = if incremental {
        constraint_delta(previous_constraints, &constraints)
    } else {
        (Vec::new(), Vec::new())
    };
    let removed_constraints = _removed_constraints;
    st.constraints = constraints;
    st.constraint_winding = constraint_winding;
    st.has_constraints = !st.constraints.is_empty();
    if !incremental {
        for triangle in 0..initial_triangle_count as i64 {
            add_triangle(st, triangle)?;
        }
        for triangle in 0..initial_triangle_count as i64 {
            seed_triangle(st, triangle);
        }
    } else {
        for constraint_index in 0..removed_constraints.len() / 2 {
            let key = ek(
                st,
                removed_constraints[constraint_index * 2],
                removed_constraints[constraint_index * 2 + 1],
            )?;
            table_unconstrain(&mut st.table, key);
        }
    }
    for (insertion, &point) in insert_points.iter().enumerate() {
        poll(cancel_value, insertion as i64, 255)?;
        let (containing, edge) = locate(st, cb, point)?;
        let (inserted_triangles, inserted_count): ([i64; 4], usize) = match edge {
            None => {
                let (a, b, c) = tri(st, containing);
                remove_triangle(st, containing)?;
                set_triangle(st, cb, containing, a, b, point)?;
                let second = append_triangle(st, cb, b, c, point)?;
                let third = append_triangle(st, cb, c, a, point)?;
                add_triangle(st, containing)?;
                add_triangle(st, second)?;
                add_triangle(st, third)?;
                seed_triangle(st, containing);
                seed_triangle(st, second);
                seed_triangle(st, third);
                ([containing, second, third, 0], 3)
            }
            Some((_, pu, pv, opposite)) => {
                let slot = table_find(&st.table, ek(st, pu, pv)?);
                if slot < 0 {
                    return Err(err_message("Planar CDT lost an inserted-point split edge"));
                }
                let left = st.table.first_triangles[u(slot)];
                let right = st.table.second_triangles[u(slot)];
                let adjacent_triangle = if left == containing { right } else { left };
                let adjacent_opposite = if adjacent_triangle < 0 {
                    -1
                } else if left == adjacent_triangle {
                    st.table.first_opposites[u(slot)]
                } else {
                    st.table.second_opposites[u(slot)]
                };
                remove_triangle(st, containing)?;
                if adjacent_triangle >= 0 {
                    remove_triangle(st, adjacent_triangle)?;
                }
                set_triangle(st, cb, containing, pu, point, opposite)?;
                let containing_second = append_triangle(st, cb, point, pv, opposite)?;
                add_triangle(st, containing)?;
                add_triangle(st, containing_second)?;
                seed_triangle(st, containing);
                seed_triangle(st, containing_second);
                if adjacent_triangle >= 0 {
                    set_triangle(st, cb, adjacent_triangle, pv, point, adjacent_opposite)?;
                    let adjacent_second = append_triangle(st, cb, point, pu, adjacent_opposite)?;
                    add_triangle(st, adjacent_triangle)?;
                    add_triangle(st, adjacent_second)?;
                    seed_triangle(st, adjacent_triangle);
                    seed_triangle(st, adjacent_second);
                    ([containing, containing_second, adjacent_triangle, adjacent_second], 4)
                } else {
                    ([containing, containing_second, 0, 0], 2)
                }
            }
        };
        legalize_inserted(st, cb, point, &inserted_triangles[..inserted_count])?;
        st.walk_start = containing;
    }
    if incremental {
        recover_constraints(st, cancel_value, cb, &added_constraints)?;
    } else {
        table_clear_constraints(&mut st.table);
        let all = st.constraints.clone();
        recover_constraints(st, cancel_value, cb, &all)?;
    }
    delaunay_repair(st, cancel_value, cb)?;
    let (triangle_a, triangle_b, triangle_c, output_triangle_count, constraints) =
        if !req.flood && !req.remove_outside {
            (
                st.triangle_a.clone(),
                st.triangle_b.clone(),
                st.triangle_c.clone(),
                st.triangle_count,
                st.constraints.clone(),
            )
        } else {
            let o = filter_outside(st, cancel_value, cb, req.flood, req.remove_outside)?;
            (o.triangle_a, o.triangle_b, o.triangle_c, o.count, o.constraints)
        };
    let triangle_points = canonicalize(u(output_triangle_count), &triangle_a, &triangle_b, &triangle_c);
    Ok(T {
        triangle_points: Arc::from(triangle_points),
        constraint_points: Arc::from(constraints),
    })
}

/// Packed triangle triples and endpoint pairs report typed triangulation/cancellation failures.
pub fn build(
    cancel_value: Option<&cancel::T>,
    workspace: Option<&mut Workspace>,
    point_count: i64,
    orient_fn: &dyn Fn(i64, i64, i64) -> Sign,
    incircle_fn: &dyn Fn(i64, i64, i64, i64) -> Sign,
    bounds_overlap_fn: Option<&dyn Fn(i64, i64, i64, i64) -> bool>,
    triangle_points: &[i64],
    insert_points: Option<&[i64]>,
    flood_from_hull_boundary: bool,
    constraint_points: &[i64],
    constraint_winding: Option<&[i64]>,
    remove_outside_constraint_polygons: bool,
) -> Result<T, String> {
    check(cancel_value)?;
    if point_count <= 0 {
        return Err(err_message("Planar CDT point count must be positive"));
    }
    if triangle_points.len() % 3 != 0 {
        return Err(err_message("Planar CDT triangle array must contain triples"));
    }
    let insert_points = insert_points.unwrap_or(&[]);
    let initial_triangle_count = triangle_points.len() / 3;
    let all_true = |_: i64, _: i64, _: i64, _: i64| true;
    let overlap: &dyn Fn(i64, i64, i64, i64) -> bool = match bounds_overlap_fn {
        Some(f) => f,
        None => &all_true,
    };
    let cb = (orient_fn, incircle_fn, overlap);
    let mut incremental = false;
    if let Some(w) = workspace.as_deref() {
        if w.valid
            && !flood_from_hull_boundary
            && !remove_outside_constraint_polygons
            && w.key_stride >= point_count
            && w.point_count + insert_points.len() as i64 == point_count
            && w.triangle_count == initial_triangle_count as i64
            && match &w.last_output {
                Some(previous) => {
                    previous.len() == triangle_points.len()
                        && (previous.is_empty() || previous.as_ptr() == triangle_points.as_ptr())
                }
                None => false,
            }
        {
            incremental = true;
            for (index, &p) in insert_points.iter().enumerate() {
                if p != w.point_count + index as i64 {
                    incremental = false;
                }
            }
        }
    }
    let triangle_capacity = initial_triangle_count as i64 + 2 * insert_points.len() as i64;
    let edge_capacity = checked_edge_capacity(triangle_capacity)?;
    let mut workspace = workspace;
    let mut st;
    let mut previous_constraints: Vec<i64> = Vec::new();
    match workspace.as_deref_mut() {
        None => {
            st = St {
                point_count,
                key_stride: point_count,
                incremental,
                has_constraints: false,
                triangle_capacity,
                triangle_count: initial_triangle_count as i64,
                triangle_a: vec![0; u(triangle_capacity)],
                triangle_b: vec![0; u(triangle_capacity)],
                triangle_c: vec![0; u(triangle_capacity)],
                point_triangle: vec![-1; u(point_count)],
                table: table_create(edge_capacity)?,
                constraints: Vec::new(),
                constraint_winding: Vec::new(),
                walk_start: 0,
                insertion_queue: BinaryHeap::new(),
                strip_marks: scratch::filled(u(triangle_capacity), 0u8),
                boundary_stamps: scratch::filled(u(point_count), 0i64),
                boundary_edges: HashMap::new(),
                boundary_generation: 0,
            };
        }
        Some(w) => {
            let triangle_count = if incremental {
                w.triangle_count
            } else {
                initial_triangle_count as i64
            };
            if !incremental {
                prepare_workspace_points(w, point_count);
            }
            ensure_workspace(w, triangle_capacity);
            w.valid = false;
            w.last_output = None;
            previous_constraints = std::mem::take(&mut w.constraints);
            if incremental {
                let start = u(w.point_count);
                let end = u(point_count);
                w.point_triangle[start..end].iter_mut().for_each(|p| *p = -1);
            } else {
                w.point_triangle[..u(point_count)]
                    .iter_mut()
                    .for_each(|p| *p = -1);
            }
            let mut table = std::mem::replace(&mut w.edge_table, table_empty());
            if !incremental {
                table_reset(&mut table);
            }
            st = St {
                point_count,
                key_stride: w.key_stride,
                incremental,
                has_constraints: false,
                triangle_capacity,
                triangle_count,
                triangle_a: std::mem::take(&mut w.triangle_a),
                triangle_b: std::mem::take(&mut w.triangle_b),
                triangle_c: std::mem::take(&mut w.triangle_c),
                point_triangle: std::mem::take(&mut w.point_triangle),
                table,
                constraints: Vec::new(),
                constraint_winding: Vec::new(),
                walk_start: 0,
                insertion_queue: BinaryHeap::new(),
                strip_marks: scratch::filled(u(triangle_capacity), 0u8),
                boundary_stamps: scratch::filled(u(point_count), 0i64),
                boundary_edges: HashMap::new(),
                boundary_generation: 0,
            };
        }
    }
    let req = Request {
        incremental,
        point_count,
        initial_triangle_count,
        triangle_points,
        insert_points,
        flood: flood_from_hull_boundary,
        remove_outside: remove_outside_constraint_polygons,
        constraint_points,
        constraint_winding,
    };
    let result = run(&mut st, &cb, cancel_value, &req, &previous_constraints);
    if let Some(w) = workspace {
        let output_triangle_count = st.triangle_count;
        let kept_constraints = std::mem::take(&mut st.constraints);
        w.triangle_a = std::mem::take(&mut st.triangle_a);
        w.triangle_b = std::mem::take(&mut st.triangle_b);
        w.triangle_c = std::mem::take(&mut st.triangle_c);
        w.point_triangle = std::mem::take(&mut st.point_triangle);
        w.edge_table = std::mem::replace(&mut st.table, table_empty());
        if let Ok(value) = &result {
            if !flood_from_hull_boundary && !remove_outside_constraint_polygons {
                w.point_count = point_count;
                w.triangle_count = output_triangle_count;
                w.last_output = Some(Arc::clone(&value.triangle_points));
                w.constraints = kept_constraints;
                w.valid = true;
            }
            if incremental {
                w.incremental_build_count += 1;
            } else {
                w.full_build_count += 1;
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{delaunay2, predicates};

    #[test]
    fn cardinality_guards_are_native_i64() {
        assert_eq!(checked_edge_capacity(1 << 61), Ok((1 << 62) + 3));
        assert!(checked_edge_capacity(-1).is_err() && checked_edge_capacity(i64::MAX).is_err());
        assert_eq!(edge_key(i64::MAX, 2, 1, 1 << 62), Ok((1 << 62) + 2));
        assert!(edge_key(4, 0, 1, 2).is_err() && edge_key(4, i64::MAX, 1, 2).is_err());
        assert_eq!(squared_limit(-1), 0);
        assert_eq!(squared_limit(3_000_000_000), 9_000_000_000_000_000_000);
        assert_eq!(squared_limit(i64::MAX), i64::MAX);
        assert!(create_workspace(None, i64::MAX).is_err());
    }

    #[test]
    fn winding_guards_are_native_i64_overflow() {
        assert!(checked_negate(i64::MIN).is_err() && checked_negate(1 << 62) == Ok(-(1 << 62)));
        assert!(checked_add(i64::MAX, 1).is_err() && checked_add(1 << 62, 5) == Ok((1 << 62) + 5));
    }

    fn cdt(x: &[f64], y: &[f64], constraints: &[i64]) -> Result<T, String> {
        let seed = delaunay2::build(None, Some(17), x, y).unwrap();
        let view = delaunay2::view::view(&seed);
        let o = |a: i64, b: i64, c: i64| predicates::orient2d_packed(x, y, a, b, c);
        let ic = |a: i64, b: i64, c: i64, d: i64| predicates::incircle_packed(x, y, a, b, c, d);
        let bo = |a: i64, b: i64, u: i64, v: i64| {
            let (a, b, u, v) = (a as usize, b as usize, u as usize, v as usize);
            x[a].min(x[b]).max(x[u].min(x[v])) <= x[a].max(x[b]).min(x[u].max(x[v]))
                && y[a].min(y[b]).max(y[u].min(y[v])) <= y[a].max(y[b]).min(y[u].max(y[v]))
        };
        build(
            None,
            None,
            x.len() as i64,
            &o,
            &ic,
            Some(&bo),
            &view.triangle_points,
            None,
            false,
            constraints,
            None,
            false,
        )
    }

    #[test]
    fn crossing_rejected() {
        let x = [0., 1., 1., 0.];
        let y = [0., 0., 1., 1.];
        assert!(cdt(&x, &y, &[0, 2, 1, 3]).is_err());
    }

    #[test]
    fn single_diagonal_constraint_is_recovered() {
        let x = [0., 4., 4., 0., 1.];
        let y = [0., 0., 4., 4., 2.];
        let r = cdt(&x, &y, &[1, 3]).unwrap();
        assert_eq!(&r.constraint_points[..], &[1, 3]);
        assert_eq!(r.triangle_points.len() % 3, 0);
    }
}
