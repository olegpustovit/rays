//! Port of lib/rdk/boolean/boolean_complex.ml: the conforming two-complex assembled from refined
//! Boolean faces.
#![allow(non_snake_case)]

use std::collections::HashMap;
use std::sync::Arc;

use rdk_core::{cancel, error, geometry, support};
use rdk_core::error::Code;
use rdk_geom::implicit_point::{self, Construction};

use crate::boolean_constraints::{self as constraints_mod, Side};
use crate::boolean_face_cdt as cdt_mod;
use crate::boolean_refinement as refinement_mod;

pub const OPERATION: &str = "boolean_complex";

pub struct T {
    constraints: Arc<constraints_mod::T>,
    vertices: Vec<implicit_point::T>,
    facet_a: Vec<i64>,
    facet_b: Vec<i64>,
    facet_c: Vec<i64>,
    member_offsets: Vec<i64>,
    member_sides: Vec<u8>,
    member_faces: Vec<i64>,
    member_triangles: Vec<i64>,
    member_windings: Vec<u8>,
    edge_first: Vec<i64>,
    edge_second: Vec<i64>,
    edge_offsets: Vec<i64>,
    edge_facets: Vec<i64>,
    edge_locals: Vec<u8>,
}

fn fail(code: Code, message: &str) -> error::T {
    error::new(OPERATION, code, message.to_owned())
}

/// OCaml `Invalid_argument message` surfaces as `invalid_refinement`.
fn invalid(message: &str) -> error::T {
    fail(Code::InvalidRefinement, message)
}

pub fn vertex_count(value: &T) -> i64 {
    value.vertices.len() as i64
}
pub fn approximate_vertex(value: &T, vertex: i64) -> (f64, f64, f64) {
    implicit_point::approximate(&value.vertices[vertex as usize])
}
pub fn facet_count(value: &T) -> i64 {
    value.facet_a.len() as i64
}

/// `Invalid_argument` in OCaml for a corner other than 0, 1 or 2: a panic here.
pub fn facet_vertex(value: &T, facet: i64, local: i64) -> i64 {
    match local {
        0 => value.facet_a[facet as usize],
        1 => value.facet_b[facet as usize],
        2 => value.facet_c[facet as usize],
        _ => unreachable!("Boolean complex facet corner must be 0, 1, or 2"),
    }
}

pub fn facet_member_range(value: &T, facet: i64) -> (i64, i64) {
    (value.member_offsets[facet as usize], value.member_offsets[facet as usize + 1])
}
pub fn member_side(value: &T, member: i64) -> Side {
    if value.member_sides[member as usize] == 0 { Side::Left } else { Side::Right }
}
pub fn member_face(value: &T, member: i64) -> i64 {
    value.member_faces[member as usize]
}
pub fn member_triangle(value: &T, member: i64) -> i64 {
    value.member_triangles[member as usize]
}
pub fn member_winding(value: &T, member: i64) -> i64 {
    if value.member_windings[member as usize] == 0 { 1 } else { -1 }
}
pub fn edge_count(value: &T) -> i64 {
    value.edge_first.len() as i64
}
pub fn edge_first(value: &T, edge: i64) -> i64 {
    value.edge_first[edge as usize]
}
pub fn edge_second(value: &T, edge: i64) -> i64 {
    value.edge_second[edge as usize]
}
pub fn edge_incident_range(value: &T, edge: i64) -> (i64, i64) {
    (value.edge_offsets[edge as usize], value.edge_offsets[edge as usize + 1])
}
pub fn edge_incident_facet(value: &T, incident: i64) -> i64 {
    value.edge_facets[incident as usize]
}
pub fn edge_incident_local(value: &T, incident: i64) -> i64 {
    value.edge_locals[incident as usize] as i64
}

pub mod view {
    use super::*;

    pub fn constraints(value: &T) -> &Arc<constraints_mod::T> {
        &value.constraints
    }
    pub fn vertex(value: &T, vertex: i64) -> &implicit_point::T {
        &value.vertices[vertex as usize]
    }
}

fn compare_points(points: &[implicit_point::T], left: i64, right: i64) -> std::cmp::Ordering {
    let (l, r) = (&points[left as usize], &points[right as usize]);
    let mut comparison = implicit_point::compare_x(l, r);
    if comparison == 0 {
        comparison = implicit_point::compare_y(l, r);
    }
    if comparison == 0 {
        comparison = implicit_point::compare_z(l, r);
    }
    if comparison != 0 { comparison.cmp(&0) } else { left.cmp(&right) }
}

fn sorted3(a: i64, b: i64, c: i64) -> (i64, i64, i64) {
    if a <= b {
        if b <= c {
            (a, b, c)
        } else if a <= c {
            (a, c, b)
        } else {
            (c, a, b)
        }
    } else if a <= c {
        (b, a, c)
    } else if b <= c {
        (b, c, a)
    } else {
        (c, b, a)
    }
}

fn ordered_pair(first: i64, second: i64) -> (i64, i64) {
    if first <= second { (first, second) } else { (second, first) }
}

type Triple = (i64, i64, i64);

fn ordered_triple(first: Triple, second: Triple, third: Triple) -> (Triple, Triple, Triple) {
    if first <= second {
        if second <= third {
            (first, second, third)
        } else if first <= third {
            (first, third, second)
        } else {
            (third, first, second)
        }
    } else if first <= third {
        (second, first, third)
    } else if second <= third {
        (second, third, first)
    } else {
        (third, second, first)
    }
}

/// One stable counting-sort pass over `values` keyed by `keys[value]`.
fn stable_counting_pass(
    cancel_value: Option<&cancel::T>,
    key_count: usize,
    keys: &[i64],
    values: &mut [i64],
    scratch: &mut [i64],
    counts: &mut [i64],
) -> Result<(), error::T> {
    for count in counts[..key_count].iter_mut() {
        *count = 0;
    }
    for slot in 0..values.len() {
        poll(cancel_value, slot as i64, 16383)?;
        let key = keys[values[slot] as usize];
        if key < 0 || key as usize >= key_count {
            return Err(invalid("Boolean complex counting-sort key is out of range"));
        }
        counts[key as usize] += 1;
    }
    let mut total = 0i64;
    for key in 0..key_count {
        let count = counts[key];
        counts[key] = total;
        total += count;
    }
    for slot in 0..values.len() {
        let value = values[slot];
        let key = keys[value as usize] as usize;
        scratch[counts[key] as usize] = value;
        counts[key] += 1;
    }
    values.copy_from_slice(&scratch[..values.len()]);
    Ok(())
}

fn stable_counting_sort3(
    cancel_value: Option<&cancel::T>,
    key_count: usize,
    key_a: &[i64],
    key_b: &[i64],
    key_c: &[i64],
    values: &mut [i64],
) -> Result<(), error::T> {
    let mut scratch = vec![0i64; values.len()];
    let mut counts = vec![0i64; key_count];
    stable_counting_pass(cancel_value, key_count, key_c, values, &mut scratch, &mut counts)?;
    stable_counting_pass(cancel_value, key_count, key_b, values, &mut scratch, &mut counts)?;
    stable_counting_pass(cancel_value, key_count, key_a, values, &mut scratch, &mut counts)
}

fn stable_counting_sort2(
    cancel_value: Option<&cancel::T>,
    key_count: usize,
    key_a: &[i64],
    key_b: &[i64],
    values: &mut [i64],
) -> Result<(), error::T> {
    let mut scratch = vec![0i64; values.len()];
    let mut counts = vec![0i64; key_count];
    stable_counting_pass(cancel_value, key_count, key_b, values, &mut scratch, &mut counts)?;
    stable_counting_pass(cancel_value, key_count, key_a, values, &mut scratch, &mut counts)
}

fn face_of(refinement: &refinement_mod::T, side: Side, face: i64) -> Option<&cdt_mod::T> {
    match side {
        Side::Left => refinement_mod::left_face(refinement, face),
        Side::Right => refinement_mod::right_face(refinement, face),
    }
}

fn poll(cancel_value: Option<&cancel::T>, index: i64, mask: i64) -> Result<(), error::T> {
    cancel::poll(cancel_value, index, mask).map_err(|_| error::cancelled(OPERATION))
}

/// Sorts `values` by `compare_points`: sorted runs of 2048 in parallel, then pairwise merges of
/// doubling width, as the OCaml `parallel_sort_indices`. The comparator is total, so the result is
/// the same as any other sort.
fn parallel_sort_indices(
    cancel_value: Option<&cancel::T>,
    token_points: &[implicit_point::T],
    values: Vec<i64>,
) -> Result<Vec<i64>, error::T> {
    let count = values.len();
    let run_size = 2_048usize;
    let mut values = values;
    if count < run_size * 2 {
        values.sort_by(|&l, &r| compare_points(token_points, l, r));
        return Ok(values);
    }
    let mut source = values;
    let mut destination = vec![0_i64; count];
    {
        let windows = support::Windows::even(&mut source, run_size as i64);
        support::run_indexed_ranges(run_size as i64, count as i64, &|range, _, _| {
            cancel::check_opt(cancel_value)?;
            windows.take(range).sort_by(|&l, &r| compare_points(token_points, l, r));
            Ok(())
        })?;
    }
    let mut width = run_size;
    while width < count {
        let span = width * 2;
        {
            let sorted = &source;
            let windows = support::Windows::even(&mut destination, span as i64);
            support::run_indexed_ranges(span as i64, count as i64, &|merge, _, _| {
                cancel::check_opt(cancel_value)?;
                let output = windows.take(merge);
                let first = merge as usize * span;
                let middle = count.min(first + width);
                let last = count.min(first + span);
                let (mut left, mut right, mut at) = (first, middle, 0);
                while left < middle && right < last {
                    let (a, b) = (sorted[left], sorted[right]);
                    if compare_points(token_points, a, b) != std::cmp::Ordering::Greater {
                        output[at] = a;
                        left += 1;
                    } else {
                        output[at] = b;
                        right += 1;
                    }
                    at += 1;
                }
                output[at..at + (middle - left)].copy_from_slice(&sorted[left..middle]);
                at += middle - left;
                output[at..at + (last - right)].copy_from_slice(&sorted[right..last]);
                Ok(())
            })?;
        }
        std::mem::swap(&mut source, &mut destination);
        width = span;
    }
    Ok(source)
}

fn merge_orders(token_points: &[implicit_point::T], first: &[i64], second: &[i64]) -> Vec<i64> {
    let mut output = Vec::with_capacity(first.len() + second.len());
    let (mut left, mut right) = (0usize, 0usize);
    while left < first.len() && right < second.len() {
        if compare_points(token_points, first[left], second[right]) != std::cmp::Ordering::Greater {
            output.push(first[left]);
            left += 1;
        } else {
            output.push(second[right]);
            right += 1;
        }
    }
    output.extend_from_slice(&first[left..]);
    output.extend_from_slice(&second[right..]);
    output
}

type Key = [i64; 10];

struct Assembly {
    token_points: Vec<implicit_point::T>,
    next_token: i64,
    construction_tokens: HashMap<Key, i64>,
    raw_point_tokens: Vec<i64>,
    raw_sides: Vec<u8>,
    raw_faces: Vec<i64>,
    raw_triangles: Vec<i64>,
    raw_orientations: Vec<u8>,
}

impl Assembly {
    fn allocate_local(&mut self, point: implicit_point::T) -> i64 {
        let token = self.next_token;
        self.next_token += 1;
        self.token_points[token as usize] = point;
        token
    }

    fn intern_key(&mut self, key: Key, point: implicit_point::T) -> i64 {
        if let Some(&present) = self.construction_tokens.get(&key) {
            return present;
        }
        let token = self.allocate_local(point);
        self.construction_tokens.insert(key, token);
        token
    }

    fn intern_local_point(&mut self, point: &implicit_point::T) -> i64 {
        let key: Key = match implicit_point::construction(point) {
            Construction::LinePlane { line_start, line_end, plane_a, plane_b, plane_c } => {
                let (line_start, line_end) = ordered_pair(*line_start, *line_end);
                let (a, b, c) = sorted3(*plane_a, *plane_b, *plane_c);
                [1, line_start, line_end, a, b, c, -1, -1, -1, -1]
            }
            Construction::LineLine { first_start, first_end, second_start, second_end, .. } => {
                let first = ordered_pair(*first_start, *first_end);
                let second = ordered_pair(*second_start, *second_end);
                let (first, second) = if first <= second { (first, second) } else { (second, first) };
                [2, first.0, first.1, second.0, second.1, -1, -1, -1, -1, -1]
            }
            Construction::TriplePlane {
                first_a,
                first_b,
                first_c,
                second_a,
                second_b,
                second_c,
                third_a,
                third_b,
                third_c,
            } => {
                let first = sorted3(*first_a, *first_b, *first_c);
                let second = sorted3(*second_a, *second_b, *second_c);
                let third = sorted3(*third_a, *third_b, *third_c);
                let (f, s, t) = ordered_triple(first, second, third);
                [3, f.0, f.1, f.2, s.0, s.1, s.2, t.0, t.1, t.2]
            }
            Construction::Explicit(_)
            | Construction::Rounded { .. }
            | Construction::Midpoint { .. }
            | Construction::Centroid3 { .. }
            | Construction::Circumcenter2Xy { .. } => return self.allocate_local(point.clone()),
        };
        self.intern_key(key, point.clone())
    }

    fn append(&mut self, side: Side, face: i64, triangle_index: i64, tokens: [i64; 3], orientation: i64) {
        if side == Side::Right {
            self.raw_sides.push(1);
        } else {
            self.raw_sides.push(0);
        }
        self.raw_faces.push(face);
        self.raw_triangles.push(triangle_index);
        self.raw_orientations.push(if orientation < 0 { 1 } else { 0 });
        self.raw_point_tokens.extend_from_slice(&tokens);
    }
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    constraints: &Arc<constraints_mod::T>,
    refinement: &refinement_mod::T,
) -> Result<T, error::T> {
    poll(cancel_value, 0, 0)?;
    if !Arc::ptr_eq(refinement_mod::view::constraints(refinement), constraints) {
        return Err(invalid("refinement belongs to a different constraint plan"));
    }
    let constraints_ref: &constraints_mod::T = constraints;
    let left_faces = constraints_mod::left_triangle_count(constraints_ref);
    let right_faces = constraints_mod::right_triangle_count(constraints_ref);
    let source_point_count = (geometry::point_count(constraints_mod::view::left_geometry(constraints_ref))
        + geometry::point_count(constraints_mod::view::right_geometry(constraints_ref))) as usize;
    let global_point_count = source_point_count + constraints_mod::point_count(constraints_ref) as usize;
    let mut raw_count = 0usize;
    let mut token_count = global_point_count;
    let mut add_faces = |count: i64, side: Side| {
        for face in 0..count {
            let facets = match face_of(refinement, side, face) {
                None => 1,
                Some(value) => {
                    let mut extra_points = 0usize;
                    for point in 3..cdt_mod::point_count(value) {
                        if cdt_mod::view::global_point_token(value, point) < 0 {
                            extra_points += 1;
                        }
                    }
                    token_count += extra_points;
                    cdt_mod::triangle_count(value) as usize
                }
            };
            raw_count += facets;
        }
    };
    add_faces(left_faces, Side::Left);
    add_faces(right_faces, Side::Right);
    let fallback = if left_faces > 0 {
        constraints_mod::view::source_point(
            constraints_ref,
            constraints_mod::view::left_triangle_point(constraints_ref, 0, 0),
        )
        .clone()
    } else if right_faces > 0 {
        constraints_mod::view::source_point(
            constraints_ref,
            constraints_mod::view::right_triangle_point(constraints_ref, 0, 0),
        )
        .clone()
    } else {
        return Err(invalid("Boolean complex has no source faces"));
    };
    let mut token_points = vec![fallback; token_count];
    for point in 0..source_point_count {
        token_points[point] = constraints_mod::view::source_point(constraints_ref, point as i64).clone();
    }
    for point in 0..constraints_mod::point_count(constraints_ref) as usize {
        token_points[source_point_count + point] = constraints_mod::view::point(constraints_ref, point as i64).clone();
    }
    let mut assembly = Assembly {
        token_points,
        next_token: global_point_count as i64,
        construction_tokens: HashMap::new(),
        raw_point_tokens: Vec::with_capacity(raw_count * 3),
        raw_sides: Vec::with_capacity(raw_count),
        raw_faces: Vec::with_capacity(raw_count),
        raw_triangles: Vec::with_capacity(raw_count),
        raw_orientations: Vec::with_capacity(raw_count),
    };
    let mut append_side = |side: Side, face_count: i64| -> Result<(), error::T> {
        let triangle_point = |face: i64, local: i64| constraints_mod::view::triangle_point(constraints_ref, side, face, local);
        for face in 0..face_count {
            poll(cancel_value, face, 255)?;
            match face_of(refinement, side, face) {
                None => {
                    let tokens = [triangle_point(face, 0), triangle_point(face, 1), triangle_point(face, 2)];
                    assembly.append(side, face, -1, tokens, 1);
                }
                Some(value) => {
                    let orientation = cdt_mod::view::source_winding(value);
                    let point_count = cdt_mod::point_count(value);
                    let mut point_tokens = vec![0i64; point_count as usize];
                    for local in 0..std::cmp::min(2, point_count - 1) + 1 {
                        point_tokens[local as usize] = triangle_point(face, local);
                    }
                    for point in 3..point_count {
                        let global = cdt_mod::view::global_point_token(value, point);
                        point_tokens[point as usize] = if global >= 0 {
                            global
                        } else {
                            assembly.intern_local_point(cdt_mod::view::point(value, point))
                        };
                    }
                    for triangle in 0..cdt_mod::triangle_count(value) {
                        let tokens = [
                            point_tokens[cdt_mod::triangle_point(value, triangle, 0) as usize],
                            point_tokens[cdt_mod::triangle_point(value, triangle, 1) as usize],
                            point_tokens[cdt_mod::triangle_point(value, triangle, 2) as usize],
                        ];
                        assembly.append(side, face, triangle, tokens, orientation);
                    }
                }
            }
        }
        Ok(())
    };
    append_side(Side::Left, left_faces)?;
    append_side(Side::Right, right_faces)?;
    let Assembly {
        token_points,
        next_token,
        raw_point_tokens,
        raw_sides,
        raw_faces,
        raw_triangles,
        raw_orientations,
        ..
    } = assembly;
    let token_count = next_token as usize;
    let raw_count = raw_sides.len();
    // Constraint points are already exact-coordinate sorted and unique by the constraint planner.
    // Sort only the explicit-source prefix and the face-local constructions, then merge the runs.
    // The comparator is total (index tie-break), so any sort yields the same order.
    let source_order = parallel_sort_indices(cancel_value, &token_points, (0..source_point_count as i64).collect())?;
    let constraint_order: Vec<i64> = (source_point_count as i64..global_point_count as i64).collect();
    let local_order = parallel_sort_indices(cancel_value, &token_points, (global_point_count as i64..token_count as i64).collect())?;
    let token_order = merge_orders(&token_points, &merge_orders(&token_points, &source_order, &constraint_order), &local_order);
    let mut vertex_of_token = vec![0i64; token_count];
    let mut vertices: Vec<implicit_point::T> = Vec::new();
    let mut previous: i64 = -1;
    for &token in &token_order {
        if previous < 0
            || !implicit_point::equal(&token_points[token as usize], &token_points[previous as usize])
        {
            previous = token;
            vertices.push(token_points[token as usize].clone());
        }
        vertex_of_token[token as usize] = vertices.len() as i64 - 1;
    }
    let unique_count = vertices.len();
    let mut raw_a = vec![0i64; raw_count];
    let mut raw_b = vec![0i64; raw_count];
    let mut raw_c = vec![0i64; raw_count];
    let mut key_a = vec![0i64; raw_count];
    let mut key_b = vec![0i64; raw_count];
    let mut key_c = vec![0i64; raw_count];
    for facet in 0..raw_count {
        let a = vertex_of_token[raw_point_tokens[facet * 3] as usize];
        let b = vertex_of_token[raw_point_tokens[facet * 3 + 1] as usize];
        let c = vertex_of_token[raw_point_tokens[facet * 3 + 2] as usize];
        if a == b || b == c || c == a {
            return Err(invalid("Boolean complex contains a degenerate refined facet"));
        }
        raw_a[facet] = a;
        raw_b[facet] = b;
        raw_c[facet] = c;
        let (a, b, c) = sorted3(a, b, c);
        key_a[facet] = a;
        key_b[facet] = b;
        key_c[facet] = c;
    }
    let mut facet_order: Vec<i64> = (0..raw_count as i64).collect();
    stable_counting_sort3(cancel_value, unique_count, &key_a, &key_b, &key_c, &mut facet_order)?;
    let same_key = |facet: usize, other: usize| key_a[facet] == key_a[other] && key_b[facet] == key_b[other] && key_c[facet] == key_c[other];
    let mut unique_facets = 0usize;
    let mut previous_facet: i64 = -1;
    for &facet in &facet_order {
        if previous_facet < 0 || !same_key(facet as usize, previous_facet as usize) {
            unique_facets += 1;
            previous_facet = facet;
        }
    }
    let mut facet_a = vec![0i64; unique_facets];
    let mut facet_b = vec![0i64; unique_facets];
    let mut facet_c = vec![0i64; unique_facets];
    let mut member_offsets = vec![0i64; unique_facets + 1];
    let mut group_of_raw = vec![0usize; raw_count];
    let mut roots = vec![0usize; unique_facets];
    let mut group: i64 = -1;
    previous_facet = -1;
    for &facet in &facet_order {
        let f = facet as usize;
        if previous_facet < 0 || !same_key(f, previous_facet as usize) {
            group += 1;
            previous_facet = facet;
            roots[group as usize] = f;
            facet_a[group as usize] = raw_a[f];
            facet_b[group as usize] = raw_b[f];
            facet_c[group as usize] = raw_c[f];
        }
        group_of_raw[f] = group as usize;
        member_offsets[group as usize + 1] += 1;
    }
    for facet in 0..unique_facets {
        member_offsets[facet + 1] += member_offsets[facet];
    }
    let mut member_sides = vec![0u8; raw_count];
    let mut member_faces = vec![0i64; raw_count];
    let mut member_triangles = vec![0i64; raw_count];
    let mut member_windings = vec![0u8; raw_count];
    let mut cursor = member_offsets.clone();
    for raw_facet in 0..raw_count {
        let group = group_of_raw[raw_facet];
        let root = roots[group];
        let member = cursor[group] as usize;
        cursor[group] = member as i64 + 1;
        member_sides[member] = raw_sides[raw_facet];
        member_faces[member] = raw_faces[raw_facet];
        member_triangles[member] = raw_triangles[raw_facet];
        let position = |value: i64| if value == raw_a[root] { 0 } else if value == raw_b[root] { 1 } else { 2 };
        let p0 = position(raw_a[raw_facet]);
        let p1 = position(raw_b[raw_facet]);
        let p2 = position(raw_c[raw_facet]);
        let odd = (p0 > p1) as i64 + (p0 > p2) as i64 + (p1 > p2) as i64;
        let sign = (if odd & 1 == 0 { 1 } else { -1 }) * if raw_orientations[raw_facet] == 0 { 1 } else { -1 };
        if sign < 0 {
            member_windings[member] = 1;
        }
    }
    let edge_entries = unique_facets * 3;
    let mut entry_first = vec![0i64; edge_entries];
    let mut entry_second = vec![0i64; edge_entries];
    let mut entry_facet = vec![0i64; edge_entries];
    let mut entry_local = vec![0u8; edge_entries];
    for facet in 0..unique_facets {
        let (a, b, c) = (facet_a[facet], facet_b[facet], facet_c[facet]);
        let entry = facet * 3;
        entry_first[entry] = a.min(b);
        entry_second[entry] = a.max(b);
        entry_facet[entry] = facet as i64;
        entry_first[entry + 1] = b.min(c);
        entry_second[entry + 1] = b.max(c);
        entry_facet[entry + 1] = facet as i64;
        entry_local[entry + 1] = 1;
        entry_first[entry + 2] = c.min(a);
        entry_second[entry + 2] = c.max(a);
        entry_facet[entry + 2] = facet as i64;
        entry_local[entry + 2] = 2;
    }
    let mut edge_order: Vec<i64> = (0..edge_entries as i64).collect();
    stable_counting_sort2(cancel_value, unique_count, &entry_first, &entry_second, &mut edge_order)?;
    let same_edge = |entry: usize, other: usize| entry_first[entry] == entry_first[other] && entry_second[entry] == entry_second[other];
    let mut edges = 0usize;
    let mut previous_entry: i64 = -1;
    for &entry in &edge_order {
        if previous_entry < 0 || !same_edge(entry as usize, previous_entry as usize) {
            edges += 1;
            previous_entry = entry;
        }
    }
    let mut edge_first = vec![0i64; edges];
    let mut edge_second = vec![0i64; edges];
    let mut edge_offsets = vec![0i64; edges + 1];
    let mut edge_facets = vec![0i64; edge_entries];
    let mut edge_locals = vec![0u8; edge_entries];
    let mut edge: i64 = -1;
    let mut incident = 0usize;
    previous_entry = -1;
    for &entry in &edge_order {
        let e = entry as usize;
        if previous_entry < 0 || !same_edge(e, previous_entry as usize) {
            if edge >= 0 {
                edge_offsets[edge as usize + 1] = incident as i64;
            }
            edge += 1;
            previous_entry = entry;
            edge_first[edge as usize] = entry_first[e];
            edge_second[edge as usize] = entry_second[e];
        }
        edge_facets[incident] = entry_facet[e];
        edge_locals[incident] = entry_local[e];
        incident += 1;
    }
    if edges > 0 {
        edge_offsets[edges] = incident as i64;
    }
    Ok(T {
        constraints: Arc::clone(constraints),
        vertices,
        facet_a,
        facet_b,
        facet_c,
        member_offsets,
        member_sides,
        member_faces,
        member_triangles,
        member_windings,
        edge_first,
        edge_second,
        edge_offsets,
        edge_facets,
        edge_locals,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boolean_coplanar as coplanar_mod;
    use rdk_core::topology::PrimitiveKind;
    use rdk_core::{packed, topology};
    use std::sync::Mutex;

    type P = (f64, f64, f64);

    fn close(l: f64, r: f64) -> bool {
        (l - r).abs() <= 1e-11
    }

    fn geom(points: &[P], triangles: &[i64]) -> geometry::T {
        let x: Vec<f64> = points.iter().map(|p| p.0).collect();
        let y: Vec<f64> = points.iter().map(|p| p.1).collect();
        let z: Vec<f64> = points.iter().map(|p| p.2).collect();
        let positions = packed::Float3::View::of_owned_exn(x, y, z).unwrap();
        let offsets: Vec<i64> = (0..=(triangles.len() as i64 / 3)).map(|p| p * 3).collect();
        let kinds = vec![PrimitiveKind::Polygon; offsets.len() - 1];
        let topology = topology::create_owned(points.len() as i64, triangles.to_vec(), offsets, kinds).unwrap();
        geometry::create(positions, topology, None, None, None).unwrap()
    }

    fn triangle(points: &[P]) -> geometry::T {
        geom(points, &[0, 1, 2])
    }

    fn pipeline(left: &geometry::T, right: &geometry::T) -> (Arc<constraints_mod::T>, refinement_mod::T, T) {
        let constraints = Arc::new(constraints_mod::build(&constraints_mod::params(1), left, right).unwrap());
        let coplanar = coplanar_mod::build(None, 1, &constraints).unwrap();
        let refinement = refinement_mod::build(None, Some(&coplanar), 1, &constraints).unwrap();
        let value = build(None, &constraints, &refinement).unwrap();
        (constraints, refinement, value)
    }

    #[test]
    fn disjoint_closed_surfaces() {
        let tetra = |offset: f64| {
            geom(
                &[(offset, 0., 0.), (offset + 1., 0., 0.), (offset, 1., 0.), (offset, 0., 1.)],
                &[0, 2, 1, 0, 1, 3, 1, 2, 3, 2, 0, 3],
            )
        };
        let (_, _, value) = pipeline(&tetra(0.), &tetra(10.));
        assert_eq!(vertex_count(&value), 8, "disjoint tetrahedra lost vertices");
        assert_eq!(facet_count(&value), 8, "disjoint tetrahedra lost facets");
        assert_eq!(edge_count(&value), 12, "disjoint tetrahedra edge table is wrong");
        for e in 0..edge_count(&value) {
            let (first, last) = edge_incident_range(&value, e);
            assert_eq!(last - first, 2, "closed tetrahedron edge is not two-manifold");
        }
    }

    #[test]
    fn identical_facet_ownership() {
        let left = triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]);
        let right = triangle(&[(0., 2., 0.), (2., 0., 0.), (0., 0., 0.)]);
        let (_, _, value) = pipeline(&left, &right);
        assert!(vertex_count(&value) == 3 && facet_count(&value) == 1, "identical facets were not merged");
        let (first, last) = facet_member_range(&value, 0);
        assert_eq!(last - first, 2, "identical facet lost an owner");
        assert!(
            member_side(&value, first) == Side::Left
                && member_side(&value, first + 1) == Side::Right
                && member_winding(&value, first) == 1
                && member_winding(&value, first + 1) == -1,
            "identical facet winding vector is wrong"
        );
    }

    #[test]
    fn partial_coplanar_overlap() {
        let left = triangle(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)]);
        let right = triangle(&[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.)]);
        let (_, _, value) = pipeline(&left, &right);
        assert_eq!(vertex_count(&value), 12, "coplanar overlay did not globally deduplicate exact vertices");
        assert_eq!(facet_count(&value), 10, "coplanar overlay did not merge four shared refined facets");
        let mut shared = 0;
        for facet in 0..facet_count(&value) {
            let (first, last) = facet_member_range(&value, facet);
            if last - first == 2 {
                shared += 1;
            } else {
                assert_eq!(last - first, 1, "facet has an unexpected ownership count");
            }
        }
        assert_eq!(shared, 4, "partial overlap has the wrong coincident facet count");
    }

    #[test]
    fn transverse_radial_bundle() {
        let left = triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]);
        let right = triangle(&[(0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)]);
        let (_, _, value) = pipeline(&left, &right);
        let mut seam = -1;
        for e in 0..edge_count(&value) {
            let (ax, ay, az) = approximate_vertex(&value, edge_first(&value, e));
            let (bx, by, bz) = approximate_vertex(&value, edge_second(&value, e));
            let matches = |x0: f64, y0: f64, z0: f64, x1: f64, y1: f64, z1: f64| {
                close(ax, x0) && close(ay, y0) && close(az, z0) && close(bx, x1) && close(by, y1) && close(bz, z1)
            };
            if matches(0.5, 0.5, 0., 0.5, 1.5, 0.) || matches(0.5, 1.5, 0., 0.5, 0.5, 0.) {
                seam = e;
            }
        }
        assert!(seam >= 0, "transverse intersection is not a complex edge");
        let (first, last) = edge_incident_range(&value, seam);
        assert_eq!(last - first, 4, "transverse seam does not retain its four incident surface charts");
    }

    type Signature = (Vec<P>, Vec<(i64, i64, i64, (i64, i64))>, Vec<(i64, i64, (i64, i64))>);

    fn signature(value: &T) -> Signature {
        (
            (0..vertex_count(value)).map(|v| approximate_vertex(value, v)).collect(),
            (0..facet_count(value))
                .map(|f| (facet_vertex(value, f, 0), facet_vertex(value, f, 1), facet_vertex(value, f, 2), facet_member_range(value, f)))
                .collect(),
            (0..edge_count(value)).map(|e| (edge_first(value, e), edge_second(value, e), edge_incident_range(value, e))).collect(),
        )
    }

    #[test]
    fn domain_exactness() {
        let left = triangle(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)]);
        let right = triangle(&[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.)]);
        let run = |domains: i64| {
            let slot: Mutex<Option<Signature>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                let (_, _, value) = pipeline(&left, &right);
                *slot.lock().unwrap() = Some(signature(&value));
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        assert!(run(1) == run(4), "Boolean complex differs between domain counts");
    }

    #[test]
    fn cancellation() {
        let left = triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]);
        let right = triangle(&[(10., 0., 0.), (12., 0., 0.), (10., 2., 0.)]);
        let (constraints, refinement, _) = pipeline(&left, &right);
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), &constraints, &refinement) {
            Err(e) => assert_eq!(error::code(&e), Code::Cancelled),
            Ok(_) => panic!("cancelled Boolean complex assembly completed"),
        }
    }

    #[test]
    fn parallel_sort_matches_sequential_sort() {
        // 9000 points with many exact duplicates: more than four 2048-runs and several merge passes.
        let n = 9_000usize;
        let coordinate = |k: usize, salt: usize| ((k * 7919 + salt) % 211) as f64;
        let source = implicit_point::source(
            (0..n).map(|k| coordinate(k, 1)).collect(),
            (0..n).map(|k| coordinate(k, 2)).collect(),
            (0..n).map(|k| coordinate(k, 3)).collect(),
        )
        .unwrap();
        let points: Vec<implicit_point::T> = (0..n as i64).map(|k| implicit_point::explicit(&source, k).unwrap()).collect();
        let mut expected: Vec<i64> = (0..n as i64).collect();
        expected.sort_by(|&l, &r| compare_points(&points, l, r));
        for domains in [1, 4] {
            let slot: Mutex<Option<Vec<i64>>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                *slot.lock().unwrap() = Some(parallel_sort_indices(None, &points, (0..n as i64).collect()).unwrap());
                Ok(())
            })
            .unwrap();
            assert!(slot.into_inner().unwrap().unwrap() == expected, "parallel sort differs at {domains} domains");
        }
    }
}
