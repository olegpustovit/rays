//! Port of lib/rdk/boolean/boolean_materialization.ml: post-rounding Boolean materialization policy.
//!
//! OCaml exceptions (`Cancelled`, `Invalid_argument`) become `Err` values: `inv` marks an
//! `Invalid_argument` with the reserved code `invalid_argument` and `guard` turns it (and every
//! cancellation) into the stable diagnostic each OCaml `try ... with` produced.
use rdk_core::support::DisjointSet;
use rays_math::float::{min as omin, max as omax, compare as float_compare};
use std::cmp::Ordering;
use std::sync::atomic::{AtomicI64, AtomicU8, Ordering as AtomicOrdering};
use std::sync::{Arc, Mutex};

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, support, topology,
    topology_index, topology_remap,
};
use rdk_core::error::Code;
use rdk_geom::implicit_point;
use rdk_geom::predicates::{self, Sign};
use rdk_geom::surface_index;
use rdk_ops::{deletion, dissolve, edge_collapse, fuse_reduce};

use crate::boolean_constraints::{self, Side};
use crate::boolean_coplanar;
use crate::boolean_extract;
use crate::boolean_seam;

const OPERATION: &str = "boolean_materialization";
const MAX_ARRAY_LENGTH: i64 = 18_014_398_509_481_983;

type Positions = packed::float3::view::View;
type TopologyView = topology::view::View;
type IndexView = topology_index::view::View;

fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

fn fail_with(code: Code, message: &str) -> error::T {
    make(code, message.to_owned())
}

/// OCaml `invalid_arg`.
fn inv(message: &str) -> error::T {
    make(Code::InvalidArgument, message.to_owned())
}

/// A failure of an OCaml call that raised instead of returning a result.
fn raised(failure: error::T) -> error::T {
    if error::is_cancelled(&failure) {
        failure
    } else {
        inv(error::message(&failure))
    }
}

/// The OCaml `try ... with Cancelled -> ... | Invalid_argument m -> error invalid_code m`.
fn guard_fail(invalid_code: Code, failure: error::T) -> error::T {
    match error::code(&failure) {
        Code::Cancelled => error::cancelled(OPERATION),
        Code::InvalidArgument => make(invalid_code, error::message(&failure).to_owned()),
        _ => failure,
    }
}

fn cx(cancel: Option<&Arc<cancel::T>>) -> Option<&cancel::T> {
    cancel.map(|value| value.as_ref())
}

fn u(index: i64) -> usize {
    index as usize
}

fn is_nonzero(sign: Sign) -> bool {
    sign != Sign::Zero
}

fn edge_length(positions: &Positions, a: i64, b: i64) -> f64 {
    let (a, b) = (u(a), u(b));
    (positions.x[a] - positions.x[b])
        .hypot((positions.y[a] - positions.y[b]).hypot(positions.z[a] - positions.z[b]))
}

fn members(group: &edge_group::T) -> Vec<i64> {
    (0..edge_group::length(group)).filter(|edge| edge_group::mem(*edge, group)).collect()
}

fn bits_len(count: i64) -> usize {
    ((count + 7) / 8) as usize
}

fn positions_view(geometry_value: &geometry::T) -> Result<Positions, error::T> {
    packed::float3::view::view(geometry::positions(geometry_value)).map_err(raised)
}

fn topology_view(topology_value: &topology::T) -> Result<TopologyView, error::T> {
    topology::view::view(topology_value).map_err(raised)
}

fn primitive_min_edge_length(positions: &Positions, topology: &TopologyView, primitive: i64) -> f64 {
    let first = topology.primitive_offsets[u(primitive)];
    let last = topology.primitive_offsets[u(primitive + 1)];
    let mut minimum = f64::INFINITY;
    for vertex in first..last {
        let next = if vertex + 1 == last { first } else { vertex + 1 };
        minimum = omin(
            minimum,
            edge_length(positions, topology.vertex_points[u(vertex)], topology.vertex_points[u(next)]),
        );
    }
    minimum
}

fn primitive_is_triangle(topology: &TopologyView, primitive: i64) -> bool {
    topology.primitive_kinds[u(primitive)] == 0
        && topology.primitive_offsets[u(primitive + 1)] - topology.primitive_offsets[u(primitive)] == 3
}

fn link_condition(
    index: &IndexView,
    topology: &TopologyView,
    stamps: &mut [i64],
    stamp: i64,
    edge: i64,
    a: i64,
    b: i64,
) -> bool {
    let first = index.edge_offsets[u(edge)];
    let v0 = index.edge_vertices[u(first)];
    let v1 = index.edge_vertices[u(first + 1)];
    let opposite = |vertex: i64| {
        let next = index.next_vertex[u(vertex)];
        topology.vertex_points[u(index.next_vertex[u(next)])]
    };
    let c = opposite(v0);
    let d = opposite(v1);
    for local in index.point_edge_offsets[u(a)]..index.point_edge_offsets[u(a + 1)] {
        let candidate = index.point_edges[u(local)];
        let x = index.edge_a[u(candidate)];
        let y = index.edge_b[u(candidate)];
        let neighbor = if x == a { y } else { x };
        if neighbor != a {
            stamps[u(neighbor)] = stamp;
        }
    }
    let mut common = 0;
    let mut valid = true;
    for local in index.point_edge_offsets[u(b)]..index.point_edge_offsets[u(b + 1)] {
        let candidate = index.point_edges[u(local)];
        let x = index.edge_a[u(candidate)];
        let y = index.edge_b[u(candidate)];
        let neighbor = if x == b { y } else { x };
        if neighbor != b && stamps[u(neighbor)] == stamp {
            common += 1;
            if neighbor != c && neighbor != d {
                valid = false;
            }
        }
    }
    valid && common == if c == d { 1 } else { 2 }
}

fn same_or_zero(first: Sign, second: Sign) -> bool {
    first == Sign::Zero || second == Sign::Zero || first == second
}

fn contraction_preserves_orientation(
    positions: &Positions,
    topology: &TopologyView,
    primitive: i64,
    keep: i64,
    remove: i64,
) -> bool {
    let first = topology.primitive_offsets[u(primitive)];
    let original = |local: i64| topology.vertex_points[u(first + local)];
    let contracted = |local: i64| {
        let point = original(local);
        if point == remove { keep } else { point }
    };
    let (oa, ob, oc) = (original(0), original(1), original(2));
    let (na, nb, nc) = (contracted(0), contracted(1), contracted(2));
    if na == nb || nb == nc || nc == na {
        return false;
    }
    let (x, y, z) = (&positions.x, &positions.y, &positions.z);
    let oxy = predicates::orient2d_packed(x, y, oa, ob, oc);
    let nxy = predicates::orient2d_packed(x, y, na, nb, nc);
    let oyz = predicates::orient2d_packed(y, z, oa, ob, oc);
    let nyz = predicates::orient2d_packed(y, z, na, nb, nc);
    let ozx = predicates::orient2d_packed(z, x, oa, ob, oc);
    let nzx = predicates::orient2d_packed(z, x, na, nb, nc);
    same_or_zero(oxy, nxy)
        && same_or_zero(oyz, nyz)
        && same_or_zero(ozx, nzx)
        && ((is_nonzero(oxy) && oxy == nxy) || (is_nonzero(oyz) && oyz == nyz) || (is_nonzero(ozx) && ozx == nzx))
}

fn sorted_candidates(candidates: &edge_group::T, lengths: &mut [f64], positions: &Positions, index: &IndexView) -> Vec<i64> {
    let ordered = members(candidates);
    for edge in &ordered {
        lengths[u(*edge)] = edge_length(positions, index.edge_a[u(*edge)], index.edge_b[u(*edge)]);
    }
    let mut ordered = ordered;
    ordered.sort_by(|left, right| {
        float_compare(lengths[u(*left)], lengths[u(*right)]).then(left.cmp(right))
    });
    ordered
}

fn reserve_primitives(index: &IndexView, used_primitives: &mut [u8], point: i64) {
    for local in index.point_offsets[u(point)]..index.point_offsets[u(point + 1)] {
        support::bits::set(used_primitives, index.primitive_of_vertex[u(index.point_vertices[u(local)])]);
    }
}

pub fn safe_independent_edges(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    keep_greatest: Option<bool>,
    candidates: &edge_group::T,
    geometry_value: &geometry::T,
) -> Result<edge_group::T, error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    let keep_greatest = keep_greatest.unwrap_or(false);
    (|| {
            cancel::check_opt(cx(cancel))?;
            let topology_value = geometry::topology(geometry_value);
            let index_value = topology_index::create(cx(cancel), topology_value)?;
            let index = topology_index::view::view(&index_value);
            let topology = topology_view(topology_value)?;
            let positions = positions_view(geometry_value)?;
            let edge_count = index.edge_a.len() as i64;
            let point_count = geometry::point_count(geometry_value);
            let primitive_count = geometry::primitive_count(geometry_value);
            if edge_group::topology_data_id(candidates) != topology::data_id(topology_value)
                || edge_group::length(candidates) != edge_count
            {
                return Err(inv("Boolean edge candidates belong to a different topology"));
            }
            let mut lengths = vec![f64::INFINITY; u(edge_count)];
            let ordered = sorted_candidates(candidates, &mut lengths, &positions, &index);
            let mut chosen = vec![0u8; bits_len(edge_count)];
            let mut used_points = vec![0u8; bits_len(point_count)];
            let mut used_primitives = vec![0u8; bits_len(primitive_count)];
            let mut neighbor_stamps = vec![-1i64; u(point_count)];
            for (stamp, edge) in ordered.iter().enumerate() {
                let stamp = stamp as i64;
                let edge = *edge;
                cancel::poll(cx(cancel), stamp, 4095)?;
                let incidence_first = index.edge_offsets[u(edge)];
                let incidence_last = index.edge_offsets[u(edge + 1)];
                if incidence_last - incidence_first != 2 {
                    continue;
                }
                let a = index.edge_a[u(edge)];
                let b = index.edge_b[u(edge)];
                let keep = if keep_greatest { a.max(b) } else { a.min(b) };
                let remove = if keep_greatest { a.min(b) } else { a.max(b) };
                let primitive_a = index.primitive_of_vertex[u(index.edge_vertices[u(incidence_first)])];
                let primitive_b = index.primitive_of_vertex[u(index.edge_vertices[u(incidence_first + 1)])];
                if primitive_a != primitive_b
                    && primitive_is_triangle(&topology, primitive_a)
                    && primitive_is_triangle(&topology, primitive_b)
                    && !(support::bits::mem(&used_points, a)
                        || support::bits::mem(&used_points, b)
                        || support::bits::mem(&used_primitives, primitive_a)
                        || support::bits::mem(&used_primitives, primitive_b))
                    && link_condition(&index, &topology, &mut neighbor_stamps, stamp, edge, a, b)
                {
                    let mut valid = true;
                    let first = index.point_offsets[u(remove)];
                    let last = index.point_offsets[u(remove + 1)];
                    let mut local = first;
                    while valid && local < last {
                        let primitive = index.primitive_of_vertex[u(index.point_vertices[u(local)])];
                        if primitive != primitive_a
                            && primitive != primitive_b
                            && (support::bits::mem(&used_primitives, primitive)
                                || !primitive_is_triangle(&topology, primitive)
                                || !contraction_preserves_orientation(&positions, &topology, primitive, keep, remove))
                        {
                            valid = false;
                        }
                        local += 1;
                    }
                    if valid {
                        support::bits::set(&mut chosen, edge);
                        support::bits::set(&mut used_points, a);
                        support::bits::set(&mut used_points, b);
                        reserve_primitives(&index, &mut used_primitives, a);
                        reserve_primitives(&index, &mut used_primitives, b);
                    }
                }
            }
            edge_group::view::of_owned_bits(topology_value, edge_count, "__rdk_boolean_safe_tiny_seam", chosen)
                .map_err(raised)
        })().map_err(|f| guard_fail(Code::InvalidInput, f))
}

pub fn speculative_independent_edges(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    candidates: &edge_group::T,
    geometry_value: &geometry::T,
) -> Result<edge_group::T, error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    (|| {
            cancel::check_opt(cx(cancel))?;
            let topology_value = geometry::topology(geometry_value);
            let index_value = topology_index::create(cx(cancel), topology_value)?;
            let index = topology_index::view::view(&index_value);
            let positions = positions_view(geometry_value)?;
            let edge_count = index.edge_a.len() as i64;
            if edge_group::topology_data_id(candidates) != topology::data_id(topology_value)
                || edge_group::length(candidates) != edge_count
            {
                return Err(inv("Boolean speculative candidates belong to a different topology"));
            }
            let mut lengths = vec![f64::INFINITY; u(edge_count)];
            let ordered = sorted_candidates(candidates, &mut lengths, &positions, &index);
            let mut chosen = vec![0u8; bits_len(edge_count)];
            let mut used_primitives = vec![0u8; bits_len(geometry::primitive_count(geometry_value))];
            for edge in ordered {
                let a = index.edge_a[u(edge)];
                let b = index.edge_b[u(edge)];
                let mut available = true;
                for point in [a, b] {
                    for slot in index.point_offsets[u(point)]..index.point_offsets[u(point + 1)] {
                        let primitive = index.primitive_of_vertex[u(index.point_vertices[u(slot)])];
                        if support::bits::mem(&used_primitives, primitive) {
                            available = false;
                        }
                    }
                }
                if available {
                    support::bits::set(&mut chosen, edge);
                    reserve_primitives(&index, &mut used_primitives, a);
                    reserve_primitives(&index, &mut used_primitives, b);
                }
            }
            edge_group::view::of_owned_bits(
                topology_value,
                edge_count,
                "__rdk_boolean_speculative_independent",
                chosen,
            )
            .map_err(raised)
        })().map_err(|f| guard_fail(Code::InvalidInput, f))
}

pub fn surface_seam_edges(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    ancestry: &boolean_extract::Ancestry,
    seam: &boolean_seam::T,
) -> Result<edge_group::T, error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    (|| {
            cancel::check_opt(cx(cancel))?;
            let complex = boolean_extract::view::complex(ancestry);
            if !Arc::ptr_eq(boolean_seam::view::complex(seam), complex) {
                return Err(inv("Boolean seam and extraction belong to different exact complexes"));
            }
            let extracted = boolean_extract::geometry(ancestry);
            let topology_value = geometry::topology(&extracted);
            let index = topology_index::create(cx(cancel), topology_value)?;
            let mut builder = edge_group::builder::create(topology_value, &index, "__rdk_boolean_exact_seam")
                .map_err(raised)?;
            for complex_edge in 0..crate::boolean_complex::edge_count(&complex) {
                cancel::poll(cx(cancel), complex_edge, 4095)?;
                if boolean_seam::view::is_seam_edge(seam, complex_edge) {
                    let a = boolean_extract::view::complex_output_point(
                        ancestry,
                        crate::boolean_complex::edge_first(&complex, complex_edge),
                    );
                    let b = boolean_extract::view::complex_output_point(
                        ancestry,
                        crate::boolean_complex::edge_second(&complex, complex_edge),
                    );
                    if a >= 0 && b >= 0 {
                        let edge = topology_index::find_edge_index(&index, a, b);
                        if edge >= 0 {
                            edge_group::builder::set(&mut builder, edge, true).map_err(raised)?;
                        }
                    }
                }
            }
            edge_group::builder::freeze(&mut builder).map_err(raised)
        })().map_err(|f| guard_fail(Code::InvalidInput, f))
}

fn empty_geometry() -> Result<geometry::T, error::T> {
    let positions = packed::float3::view::of_owned_exn(Vec::new(), Vec::new(), Vec::new()).map_err(raised)?;
    let topology_value = topology::empty(0).map_err(raised)?;
    geometry::create(positions, topology_value, None, None, None).map_err(raised)
}

fn opposite_duplicate(topology: &TopologyView, first: i64, second: i64) -> bool {
    let first_offset = topology.primitive_offsets[u(first)];
    let second_offset = topology.primitive_offsets[u(second)];
    if topology.primitive_offsets[u(first + 1)] - first_offset != 3
        || topology.primitive_offsets[u(second + 1)] - second_offset != 3
    {
        return false;
    }
    let point = |offset: i64| topology.vertex_points[u(offset)];
    let (a, b, c) = (point(first_offset), point(first_offset + 1), point(first_offset + 2));
    let (x, y, z) = (point(second_offset), point(second_offset + 1), point(second_offset + 2));
    (a == x && b == z && c == y) || (a == y && b == x && c == z) || (a == z && b == y && c == x)
}

fn triangle_point_key(topology: &TopologyView, primitive: i64) -> (i64, i64, i64) {
    let first = topology.primitive_offsets[u(primitive)];
    let a = topology.vertex_points[u(first)];
    let b = topology.vertex_points[u(first + 1)];
    let c = topology.vertex_points[u(first + 2)];
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

fn stable_counting_pass(
    key_count: i64,
    keys: &[i64],
    values: &mut [i64],
    scratch: &mut [i64],
    counts: &mut [i64],
) -> Result<(), error::T> {
    for count in counts.iter_mut().take(u(key_count)) {
        *count = 0;
    }
    for slot in 0..values.len() {
        let key = keys[u(values[slot])];
        if key < 0 || key >= key_count {
            return Err(inv("Boolean duplicate-facet point key is out of range"));
        }
        counts[u(key)] += 1;
    }
    let mut total = 0;
    for key in 0..u(key_count) {
        let count = counts[key];
        counts[key] = total;
        total += count;
    }
    for slot in 0..values.len() {
        let primitive = values[slot];
        let key = keys[u(primitive)];
        scratch[u(counts[u(key)])] = primitive;
        counts[u(key)] += 1;
    }
    values.copy_from_slice(&scratch[..values.len()]);
    Ok(())
}

fn verify_duplicate_triangles(allow_opposite_duplicates: bool, topology: &TopologyView) -> Result<(), error::T> {
    let primitives = topology.primitive_kinds.len();
    let mut key_a = vec![0i64; primitives];
    let mut key_b = vec![0i64; primitives];
    let mut key_c = vec![0i64; primitives];
    for primitive in 0..primitives {
        let (a, b, c) = triangle_point_key(topology, primitive as i64);
        key_a[primitive] = a;
        key_b[primitive] = b;
        key_c[primitive] = c;
    }
    let mut ordered: Vec<i64> = (0..primitives as i64).collect();
    let mut scratch = vec![0i64; primitives];
    let mut counts = vec![0i64; u(topology.point_count)];
    stable_counting_pass(topology.point_count, &key_c, &mut ordered, &mut scratch, &mut counts)?;
    stable_counting_pass(topology.point_count, &key_b, &mut ordered, &mut scratch, &mut counts)?;
    stable_counting_pass(topology.point_count, &key_a, &mut ordered, &mut scratch, &mut counts)?;
    let same = |first: i64, second: i64| {
        key_a[u(first)] == key_a[u(second)]
            && key_b[u(first)] == key_b[u(second)]
            && key_c[u(first)] == key_c[u(second)]
    };
    let mut failure: Option<(i64, i64)> = None;
    let mut first = 0usize;
    while failure.is_none() && first < primitives {
        let mut last = first + 1;
        while last < primitives && same(ordered[first], ordered[last]) {
            last += 1;
        }
        let count = last - first;
        if count > 1 {
            let a = ordered[first];
            let b = ordered[first + 1];
            if count != 2 || !allow_opposite_duplicates || !opposite_duplicate(topology, a, b) {
                failure = Some((a, b));
            }
        }
        first = last;
    }
    match failure {
        None => Ok(()),
        Some((first, second)) => Err(fail_with(Code::SurfaceSelfIntersection, &format!("rounded Boolean triangles {first} and {second} duplicate a surface facet"))),
    }
}

fn verify_extracted_contacts(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    positions: &Positions,
    topology: &TopologyView,
    geometry_value: &geometry::T,
) -> Result<(), error::T> {
    let surface = surface_index::view::create_validated_triangles(cx(cancel), grain, geometry_value)?;
    let (first_candidates, second_candidates) =
        surface_index::view::self_pairs_disjoint_topology(cx(cancel), grain, 0., &surface)?;
    let candidate_count = first_candidates.len() as i64;
    let ranges = (candidate_count + grain - 1) / grain;
    let failures: Vec<AtomicI64> = (0..ranges).map(|_| AtomicI64::new(-1)).collect();
    let failure_kinds: Vec<AtomicU8> = (0..ranges).map(|_| AtomicU8::new(0)).collect();
    support::run_indexed_ranges(grain, candidate_count, &|range, first, last| {
        let mut scratch = [0i64; 4];
        let mut candidate = first;
        while failures[u(range)].load(AtomicOrdering::Relaxed) < 0 && candidate < last {
            cancel::poll(cx(cancel), candidate, 4095)?;
            let left = first_candidates[u(candidate)];
            let right = second_candidates[u(candidate)];
            let left_a = surface_index::view::triangle_point(&surface, left, 0);
            let left_b = surface_index::view::triangle_point(&surface, left, 1);
            let left_c = surface_index::view::triangle_point(&surface, left, 2);
            let right_a = surface_index::view::triangle_point(&surface, right, 0);
            let right_b = surface_index::view::triangle_point(&surface, right, 1);
            let right_c = surface_index::view::triangle_point(&surface, right, 2);
            let count = predicates::view::triangle_triangle_features_into(
                &positions.x, &positions.y, &positions.z, left_a, left_b, left_c, right_a, right_b, right_c,
                &mut scratch,
            );
            if count == -2 {
                failures[u(range)].store(candidate, AtomicOrdering::Relaxed);
                failure_kinds[u(range)].store(1, AtomicOrdering::Relaxed);
            } else if count > 0 {
                failures[u(range)].store(candidate, AtomicOrdering::Relaxed);
                failure_kinds[u(range)].store(2, AtomicOrdering::Relaxed);
            } else if count == -1
                && predicates::view::coplanar_triangles_contact_packed(
                    &positions.x, &positions.y, &positions.z, left_a, left_b, left_c, right_a, right_b, right_c,
                )
            {
                failures[u(range)].store(candidate, AtomicOrdering::Relaxed);
                failure_kinds[u(range)].store(3, AtomicOrdering::Relaxed);
            }
            candidate += 1;
        }
        Ok(())
    })?;
    let mut failure_range = -1i64;
    let mut range = 0;
    while failure_range < 0 && range < ranges {
        if failures[u(range)].load(AtomicOrdering::Relaxed) >= 0 {
            failure_range = range;
        }
        range += 1;
    }
    if failure_range < 0 {
        return Ok(());
    }
    let candidate = failures[u(failure_range)].load(AtomicOrdering::Relaxed);
    let first_triangle = first_candidates[u(candidate)];
    let second_triangle = second_candidates[u(candidate)];
    let first = surface_index::view::triangle_primitive(&surface, first_triangle);
    let second = surface_index::view::triangle_primitive(&surface, second_triangle);
    match failure_kinds[u(failure_range)].load(AtomicOrdering::Relaxed) {
        1 => Err(fail_with(Code::RoundingDegenerate, "rounded Boolean surface contains a degenerate triangle pair")),
        2 => {
            let first_min = primitive_min_edge_length(positions, topology, first);
            let second_min = primitive_min_edge_length(positions, topology, second);
            Err(fail_with(Code::SurfaceSelfIntersection, &format!(
                    "rounded Boolean triangles {first} and {second} intersect away from ordinary shared topology (minimum edge lengths {} and {})",
                    cfmt::g(first_min, 17),
                    cfmt::g(second_min, 17)
                )))
        }
        _ => Err(fail_with(Code::SurfaceSelfIntersection, &format!("rounded Boolean triangles {first} and {second} have non-adjacent coplanar contact"))),
    }
}

fn verify_constraint_contacts(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    allow_opposite_duplicates: bool,
    positions: &Positions,
    topology: &TopologyView,
    constraints: &Arc<boolean_constraints::T>,
) -> Result<(), error::T> {
    if boolean_constraints::degenerate_pair_count(constraints) > 0 {
        return Err(fail_with(Code::RoundingDegenerate, "rounded Boolean surface contains a degenerate triangle pair"));
    }
    if boolean_constraints::constraint_count(constraints) > 0 {
        let first = boolean_constraints::constraint_first_triangle(constraints, 0);
        let second = boolean_constraints::constraint_second_triangle(constraints, 0);
        let first_min = primitive_min_edge_length(positions, topology, first);
        let second_min = primitive_min_edge_length(positions, topology, second);
        return Err(fail_with(Code::SurfaceSelfIntersection, &format!(
                "rounded Boolean triangles {first} and {second} intersect away from ordinary shared topology (minimum edge lengths {} and {})",
                cfmt::g(first_min, 17),
                cfmt::g(second_min, 17)
            )));
    }
    if boolean_constraints::coplanar_pair_count(constraints) == 0 {
        return Ok(());
    }
    let coplanar = boolean_coplanar::build(cx(cancel), grain, constraints)?;
    let mut contact = -1;
    let mut pair = 0;
    while contact < 0 && pair < boolean_coplanar::pair_count(&coplanar) {
        if boolean_coplanar::kind(&coplanar, pair) != boolean_coplanar::OverlapKind::Empty {
            let first = boolean_coplanar::first_triangle(&coplanar, pair);
            let second = boolean_coplanar::second_triangle(&coplanar, pair);
            if !(allow_opposite_duplicates && opposite_duplicate(topology, first, second)) {
                contact = pair;
            }
        }
        pair += 1;
    }
    if contact < 0 {
        return Ok(());
    }
    let first = boolean_coplanar::first_triangle(&coplanar, contact);
    let second = boolean_coplanar::second_triangle(&coplanar, contact);
    Err(fail_with(Code::SurfaceSelfIntersection, &format!("rounded Boolean triangles {first} and {second} have non-adjacent coplanar contact")))
}

fn verify_extracted_orientations(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    ancestry: &boolean_extract::Ancestry,
    geometry_value: &geometry::T,
) -> Result<(), error::T> {
    let positions = positions_view(geometry_value)?;
    let topology = topology_view(geometry::topology(geometry_value))?;
    let view = boolean_extract::view::ancestry_view(ancestry);
    let complex = boolean_extract::view::complex(ancestry);
    let primitives = geometry::primitive_count(geometry_value);
    if view.corner_complex_vertices.len() as i64 != primitives * 3
        || view.primitive_sides.len() as i64 != primitives
        || view.primitive_triangles.len() as i64 != primitives
    {
        return Err(fail_with(Code::GeometryMismatch, "Boolean extraction corner ancestry has the wrong cardinality"));
    }
    let constraints = crate::boolean_complex::view::constraints(&complex);
    let source = boolean_constraints::view::source(&constraints);
    let ranges = (primitives + grain - 1) / grain;
    let failures: Vec<AtomicI64> = (0..ranges).map(|_| AtomicI64::new(-1)).collect();
    support::run_indexed_ranges(grain, primitives, &|range, first, last| {
        let mut primitive = first;
        while failures[u(range)].load(AtomicOrdering::Relaxed) < 0 && primitive < last {
            cancel::poll(cx(cancel), primitive, 4095)?;
            let offset = primitive * 3;
            let a = topology.vertex_points[u(offset)];
            let b = topology.vertex_points[u(offset + 1)];
            let c = topology.vertex_points[u(offset + 2)];
            let ea = crate::boolean_complex::view::vertex(&complex, view.corner_complex_vertices[u(offset)]);
            let eb = crate::boolean_complex::view::vertex(&complex, view.corner_complex_vertices[u(offset + 1)]);
            let ec = crate::boolean_complex::view::vertex(&complex, view.corner_complex_vertices[u(offset + 2)]);
            let triangle = view.primitive_triangles[u(primitive)];
            let side = if view.primitive_sides[u(primitive)] == 0 { Side::Left } else { Side::Right };
            let triangle_point = |local: i64| match side {
                Side::Left => boolean_constraints::view::left_triangle_point(&constraints, triangle, local),
                Side::Right => boolean_constraints::view::right_triangle_point(&constraints, triangle, local),
            };
            let (sa, sb, sc) = (triangle_point(0), triangle_point(1), triangle_point(2));
            let (exact, rounded) = match implicit_point::source_triangle_projection(&source, sa, sb, sc) {
                implicit_point::Projection::XY => (
                    implicit_point::orient2d_xy(&ea, &eb, &ec),
                    predicates::orient2d_packed(&positions.x, &positions.y, a, b, c),
                ),
                implicit_point::Projection::YZ => (
                    implicit_point::orient2d_yz(&ea, &eb, &ec),
                    predicates::orient2d_packed(&positions.y, &positions.z, a, b, c),
                ),
                implicit_point::Projection::ZX => (
                    implicit_point::orient2d_zx(&ea, &eb, &ec),
                    predicates::orient2d_packed(&positions.z, &positions.x, a, b, c),
                ),
            };
            if exact == Sign::Zero || exact != rounded {
                failures[u(range)].store(primitive, AtomicOrdering::Relaxed);
            }
            primitive += 1;
        }
        Ok(())
    })?;
    let mut failure = -1;
    for range in 0..ranges {
        let value = failures[u(range)].load(AtomicOrdering::Relaxed);
        if failure < 0 && value >= 0 {
            failure = value;
        }
    }
    if failure < 0 {
        Ok(())
    } else {
        Err(fail_with(Code::RoundingOrientation, &format!("rounded Boolean triangle {failure} changed its exact extraction orientation")))
    }
}

fn validate_materialized(positions: &Positions, topology: &TopologyView) -> Result<(), error::T> {
    boolean_extract::view::validate_materialized(
        false,
        &positions.x,
        &positions.y,
        &positions.z,
        &topology.vertex_points,
    )
    .map_err(|failure| {
        make(
            error::code(&failure),
            format!("rounded surface verification failed: {}", error::to_string(&failure)),
        )
    })
}

fn verify_extracted_surface(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    require_closed: bool,
    allow_opposite_duplicates: bool,
    ancestry: &boolean_extract::Ancestry,
    geometry_value: &geometry::T,
) -> Result<(), error::T> {
    let extracted = boolean_extract::geometry(ancestry);
    if packed::float3::data_id(geometry::positions(geometry_value))
        != packed::float3::data_id(geometry::positions(&extracted))
        || topology::data_id(geometry::topology(geometry_value)) != topology::data_id(geometry::topology(&extracted))
    {
        return Err(fail_with(Code::GeometryMismatch, "fast Boolean verification requires unchanged extraction geometry"));
    }
    (|| {
            cancel::check_opt(cx(cancel))?;
            let positions = positions_view(geometry_value)?;
            let topology_value = geometry::topology(geometry_value);
            let topology = topology_view(topology_value)?;
            validate_materialized(&positions, &topology)?;
            if require_closed {
                boolean_extract::view::validate_closed_topology(topology_value)?;
            }
            verify_extracted_orientations(cancel, grain, ancestry, geometry_value)?;
            verify_duplicate_triangles(allow_opposite_duplicates, &topology)?;
            verify_extracted_contacts(cancel, grain, &positions, &topology, geometry_value)
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

fn verify_surface(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    require_closed: bool,
    allow_opposite_duplicates: bool,
    geometry_value: &geometry::T,
) -> Result<(), error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    (|| {
            cancel::check_opt(cx(cancel))?;
            let positions = positions_view(geometry_value)?;
            let topology_value = geometry::topology(geometry_value);
            let view = topology_view(topology_value)?;
            validate_materialized(&positions, &view)?;
            if require_closed {
                boolean_extract::view::validate_closed_topology(topology_value)?;
            }
            let mut constraint_params = boolean_constraints::params(grain);
            constraint_params.cancel = cancel.cloned();
            constraint_params.resolve_left_self_intersections = true;
            constraint_params.skip_opposite_self_pairs = allow_opposite_duplicates;
            let constraints = Arc::new(boolean_constraints::build(&constraint_params, geometry_value, &empty_geometry()?)?);
            verify_constraint_contacts(cancel, grain, allow_opposite_duplicates, &positions, &view, &constraints)
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

#[derive(Clone)]
pub struct Cleanup {
    cleanup_geometry: geometry::T,
    cleanup_candidate_count: i64,
    cleanup_collapsed_count: i64,
    cleanup_batch_count: i64,
    cleanup_rounding_repaired_count: i64,
    cleanup_point_sources: Vec<i64>,
    cleanup_vertex_sources: Vec<i64>,
    cleanup_primitive_sources: Vec<i64>,
    cleanup_seam_edges: edge_group::T,
}

pub fn cleanup_geometry(value: &Cleanup) -> &geometry::T {
    &value.cleanup_geometry
}

/// Number of rounded zero-area facets removed or ULP-scale contact edges contracted by the
/// mandatory certified representability repair.
pub fn cleanup_primitive_source(value: &Cleanup, primitive: i64) -> i64 {
    value.cleanup_primitive_sources[u(primitive)]
}

fn verify_materialized_seam_curves(cancel: Option<&Arc<cancel::T>>, grain: i64, seam: &boolean_seam::T) -> Result<(), error::T> {
    if boolean_seam::view::curves_materialized(seam) {
        boolean_seam::view::verify_curves(cx(cancel), grain, &boolean_seam::curves(seam))
    } else {
        Ok(())
    }
}

fn fresh_attribute_name(geometry_value: &geometry::T, base: &str) -> String {
    let used = |name: &str| {
        geometry::find_attribute(attribute::Owner::Point, name, geometry_value).is_some()
            || geometry::find_attribute(attribute::Owner::Vertex, name, geometry_value).is_some()
            || geometry::find_attribute(attribute::Owner::Primitive, name, geometry_value).is_some()
            || geometry::find_attribute(attribute::Owner::Detail, name, geometry_value).is_some()
    };
    let mut suffix = 0;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}{suffix}") };
        if !used(&name) {
            return name;
        }
        suffix += 1;
    }
}

fn fresh_edge_name(geometry_value: &geometry::T, base: &str) -> String {
    let mut suffix = 0;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}{suffix}") };
        if geometry::find_edge_group(&name, geometry_value).is_none() {
            return name;
        }
        suffix += 1;
    }
}

fn install_source_ids(geometry_value: &geometry::T, name: &str) -> Result<geometry::T, error::T> {
    let add = |owner: attribute::Owner, count: i64, value: &geometry::T| -> Result<geometry::T, error::T> {
        let attribute_value = attribute::create_owned(name, owner, attribute::Storage::Int(Arc::new((0..count).collect())))
            .map_err(raised)?;
        geometry::with_attribute(attribute_value, value).map_err(raised)
    };
    let tagged = add(attribute::Owner::Point, geometry::point_count(geometry_value), geometry_value)?;
    let tagged = add(attribute::Owner::Vertex, geometry::vertex_count(geometry_value), &tagged)?;
    add(attribute::Owner::Primitive, geometry::primitive_count(geometry_value), &tagged)
}

fn source_ids(owner: attribute::Owner, name: &str, geometry_value: &geometry::T) -> Result<Arc<Vec<i64>>, error::T> {
    match geometry::find_attribute(owner, name, geometry_value) {
        Some(item) => match attribute::storage(&item).map_err(raised)? {
            attribute::Storage::Int(values) => Ok(values),
            _ => Err(inv("Boolean source-ID scratch has the wrong storage")),
        },
        None => Err(inv("Boolean source-ID scratch was lost")),
    }
}

fn remove_source_ids(name: &str, geometry_value: &geometry::T) -> geometry::T {
    let value = geometry::without_attribute(attribute::Owner::Point, name, geometry_value);
    let value = geometry::without_attribute(attribute::Owner::Vertex, name, &value);
    geometry::without_attribute(attribute::Owner::Primitive, name, &value)
}

/// Tag points, vertices, primitives and the exact seam edges so an operation's output can be
/// mapped back to the cleanup's ancestry, then compose the mapping.
struct Tagged {
    source_name: String,
    seam_name: String,
    geometry: geometry::T,
}

fn tag(cleanup: &Cleanup) -> Result<Tagged, error::T> {
    let geometry_value = &cleanup.cleanup_geometry;
    let source_name = fresh_attribute_name(geometry_value, "__rdk_boolean_source_id");
    let seam_name = fresh_edge_name(geometry_value, "__rdk_boolean_exact_seam");
    let tagged = install_source_ids(geometry_value, &source_name)?;
    let tagged_seams = edge_group::with_name(&seam_name, &cleanup.cleanup_seam_edges).map_err(raised)?;
    let tagged = geometry::with_edge_group(tagged_seams, &tagged).map_err(raised)?;
    Ok(Tagged { source_name, seam_name, geometry: tagged })
}

struct Composed {
    geometry: geometry::T,
    points: Vec<i64>,
    vertices: Vec<i64>,
    primitives: Vec<i64>,
    seams: edge_group::T,
}

fn compose_output(
    tagged: &Tagged,
    cleanup: &Cleanup,
    output: &geometry::T,
    remap_message: &str,
    lost_message: &str,
) -> Result<Composed, error::T> {
    let point_current = source_ids(attribute::Owner::Point, &tagged.source_name, output)?;
    let vertex_current = source_ids(attribute::Owner::Vertex, &tagged.source_name, output)?;
    let primitive_current = source_ids(attribute::Owner::Primitive, &tagged.source_name, output)?;
    let seams = match geometry::find_edge_group(&tagged.seam_name, output) {
        Some(found) => found,
        None => return Err(inv(lost_message)),
    };
    let compose = |source: &[i64], mapping: &[i64]| -> Result<Vec<i64>, error::T> {
        let mut result = Vec::with_capacity(mapping.len());
        for current in mapping {
            if *current < 0 || *current >= source.len() as i64 {
                return Err(inv(remap_message));
            }
            result.push(source[u(*current)]);
        }
        Ok(result)
    };
    let points = compose(&cleanup.cleanup_point_sources, &point_current)?;
    let vertices = compose(&cleanup.cleanup_vertex_sources, &vertex_current)?;
    let primitives = compose(&cleanup.cleanup_primitive_sources, &primitive_current)?;
    let stripped = remove_source_ids(&tagged.source_name, output);
    let stripped = geometry::without_edge_group(&tagged.seam_name, &stripped);
    Ok(Composed { geometry: stripped, points, vertices, primitives, seams })
}

fn rounded_triangle_degenerate(positions: &Positions, topology: &TopologyView, primitive: i64) -> bool {
    if !primitive_is_triangle(topology, primitive) {
        return false;
    }
    let first = topology.primitive_offsets[u(primitive)];
    let a = topology.vertex_points[u(first)];
    let b = topology.vertex_points[u(first + 1)];
    let c = topology.vertex_points[u(first + 2)];
    a == b
        || b == c
        || c == a
        || (predicates::orient2d_packed(&positions.x, &positions.y, a, b, c) == Sign::Zero
            && predicates::orient2d_packed(&positions.y, &positions.z, a, b, c) == Sign::Zero
            && predicates::orient2d_packed(&positions.z, &positions.x, a, b, c) == Sign::Zero)
}

fn rounded_sliver_edges(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<(i64, edge_group::T), error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    (|| {
            cancel::check_opt(cx(cancel))?;
            let topology_value = geometry::topology(geometry_value);
            let topology = topology_view(topology_value)?;
            let positions = positions_view(geometry_value)?;
            let index_value = topology_index::create(cx(cancel), topology_value)?;
            let index = topology_index::view::view(&index_value);
            let edge_count = topology_index::edge_count(&index_value);
            let mut bits = vec![0u8; bits_len(edge_count)];
            let mut degenerate_count = 0;
            for primitive in 0..geometry::primitive_count(geometry_value) {
                cancel::poll(cx(cancel), primitive, 4095)?;
                if rounded_triangle_degenerate(&positions, &topology, primitive) {
                    degenerate_count += 1;
                    let first = topology.primitive_offsets[u(primitive)];
                    for local in 0..3 {
                        let edge = index.edge_of_vertex[u(first + local)];
                        if edge >= 0 {
                            support::bits::set(&mut bits, edge);
                        }
                    }
                }
            }
            let group = edge_group::view::of_owned_bits(topology_value, edge_count, "__rdk_boolean_rounded_sliver", bits)
                .map_err(raised)?;
            Ok((degenerate_count, group))
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

/// Fast exact verification for geometry that still physically shares its extraction positions and
/// topology. `false` requests the ordinary bounded rounding-repair path; no weakened result is
/// published.
pub fn verify_unchanged_extraction(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    require_closed: bool,
    allow_opposite_duplicates: bool,
    ancestry: &boolean_extract::Ancestry,
    geometry_value: &geometry::T,
) -> Result<bool, error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    match verify_extracted_surface(cancel, grain, require_closed, allow_opposite_duplicates, ancestry, geometry_value) {
        Ok(()) => Ok(true),
        Err(failure)
            if matches!(error::code(&failure), Code::Cancelled | Code::GeometryMismatch | Code::InvalidParameter) =>
        {
            Err(failure)
        }
        Err(_) => Ok(false),
    }
}

fn coordinate_ulp(value: f64) -> f64 {
    if !value.is_finite() {
        return f64::INFINITY;
    }
    omax((value.next_up() - value).abs(), (value - value.next_down()).abs())
}

fn primitive_ulp_scale(positions: &Positions, topology: &TopologyView, primitive: i64) -> f64 {
    let first = topology.primitive_offsets[u(primitive)];
    let last = topology.primitive_offsets[u(primitive + 1)];
    let mut scale = 0.;
    for vertex in first..last {
        let point = u(topology.vertex_points[u(vertex)]);
        scale = omax(scale, coordinate_ulp(positions.x[point]));
        scale = omax(scale, coordinate_ulp(positions.y[point]));
        scale = omax(scale, coordinate_ulp(positions.z[point]));
    }
    scale
}

fn rounded_self_contact_edges(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<(i64, i64, edge_group::T), error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    let mut constraint_params = boolean_constraints::params(grain);
    constraint_params.cancel = cancel.cloned();
    constraint_params.resolve_left_self_intersections = true;
    let constraints = boolean_constraints::build(&constraint_params, geometry_value, &empty_geometry()?)?;
    (|| {
            let contact_count = boolean_constraints::constraint_count(&constraints);
            let topology_value = geometry::topology(geometry_value);
            let topology = topology_view(topology_value)?;
            let positions = positions_view(geometry_value)?;
            let index_value = topology_index::create(cx(cancel), topology_value)?;
            let index = topology_index::view::view(&index_value);
            let surface = boolean_constraints::view::left_surface(&constraints);
            let edge_count = topology_index::edge_count(&index_value);
            let mut bits = vec![0u8; bits_len(edge_count)];
            let mut repairable = 0;
            let mut mark = |triangle: i64| {
                let primitive = surface_index::view::triangle_primitive(&surface, triangle);
                let minimum = primitive_min_edge_length(&positions, &topology, primitive);
                let ulp = primitive_ulp_scale(&positions, &topology, primitive);
                if minimum <= 8. * ulp {
                    repairable += 1;
                    let first = topology.primitive_offsets[u(primitive)];
                    for local in 0..3 {
                        let edge = index.edge_of_vertex[u(first + local)];
                        if edge >= 0 {
                            let length = edge_length(&positions, index.edge_a[u(edge)], index.edge_b[u(edge)]);
                            if length <= minimum {
                                support::bits::set(&mut bits, edge);
                            }
                        }
                    }
                }
            };
            for contact in 0..contact_count {
                cancel::poll(cx(cancel), contact, 4095)?;
                mark(boolean_constraints::constraint_first_triangle(&constraints, contact));
                mark(boolean_constraints::constraint_second_triangle(&constraints, contact));
            }
            let group = edge_group::view::of_owned_bits(
                topology_value,
                edge_count,
                "__rdk_boolean_rounded_self_contact",
                bits,
            )
            .map_err(raised)?;
            Ok((contact_count, repairable, group))
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

/// OCaml's `Array.sort` (ternary heap sort) specialised to `((i64, i64, i64), i64)` keyed by the
/// first component only: equal keys land where OCaml puts them, which decides which sliver of an
/// odd group survives.
fn ocaml_sort_keyed(a: &mut [((i64, i64, i64), i64)]) {
    let cmp = |x: &((i64, i64, i64), i64), y: &((i64, i64, i64), i64)| x.0.cmp(&y.0);
    fn maxson(a: &[((i64, i64, i64), i64)], l: usize, i: usize, cmp: &dyn Fn(&((i64, i64, i64), i64), &((i64, i64, i64), i64)) -> Ordering) -> Option<usize> {
        let i31 = i + i + i + 1;
        let mut x = i31;
        if i31 + 2 < l {
            if cmp(&a[i31], &a[i31 + 1]) == Ordering::Less {
                x = i31 + 1;
            }
            if cmp(&a[x], &a[i31 + 2]) == Ordering::Less {
                x = i31 + 2;
            }
            Some(x)
        } else if i31 + 1 < l && cmp(&a[i31], &a[i31 + 1]) == Ordering::Less {
            Some(i31 + 1)
        } else if i31 < l {
            Some(i31)
        } else {
            None
        }
    }
    let l = a.len();
    let trickle = |a: &mut [((i64, i64, i64), i64)], l: usize, mut i: usize, e: ((i64, i64, i64), i64)| loop {
        match maxson(a, l, i, &cmp) {
            None => {
                a[i] = e;
                break;
            }
            Some(j) => {
                if cmp(&a[j], &e) == Ordering::Greater {
                    a[i] = a[j];
                    i = j;
                } else {
                    a[i] = e;
                    break;
                }
            }
        }
    };
    let bubble = |a: &mut [((i64, i64, i64), i64)], l: usize, mut i: usize| -> usize {
        loop {
            match maxson(a, l, i, &cmp) {
                None => return i,
                Some(j) => {
                    a[i] = a[j];
                    i = j;
                }
            }
        }
    };
    let trickleup = |a: &mut [((i64, i64, i64), i64)], mut i: usize, e: ((i64, i64, i64), i64)| loop {
        let father = (i - 1) / 3;
        if cmp(&a[father], &e) == Ordering::Less {
            a[i] = a[father];
            if father > 0 {
                i = father;
            } else {
                a[0] = e;
                break;
            }
        } else {
            a[i] = e;
            break;
        }
    };
    if l == 0 {
        return;
    }
    let mut i = ((l + 1) / 3) as i64 - 1;
    while i >= 0 {
        let e = a[i as usize];
        trickle(a, l, i as usize, e);
        i -= 1;
    }
    let mut i = l as i64 - 1;
    while i >= 2 {
        let e = a[i as usize];
        a[i as usize] = a[0];
        let hole = bubble(a, i as usize, 0);
        trickleup(a, hole, e);
        i -= 1;
    }
    if l > 1 {
        let e = a[1];
        a[1] = a[0];
        a[0] = e;
    }
}

fn paired_rounded_slivers(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<(i64, group::T), error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    (|| {
            cancel::check_opt(cx(cancel))?;
            let topology = topology_view(geometry::topology(geometry_value))?;
            let positions = positions_view(geometry_value)?;
            let primitive_count = geometry::primitive_count(geometry_value);
            let mut keyed: Vec<((i64, i64, i64), i64)> = Vec::new();
            for primitive in 0..primitive_count {
                cancel::poll(cx(cancel), primitive, 4095)?;
                if rounded_triangle_degenerate(&positions, &topology, primitive) {
                    keyed.push((triangle_point_key(&topology, primitive), primitive));
                }
            }
            ocaml_sort_keyed(&mut keyed);
            let mut bits = vec![0u8; bits_len(primitive_count)];
            let mut removed = 0;
            let mut first = 0;
            while first < keyed.len() {
                let mut last = first + 1;
                while last < keyed.len() && keyed[last].0 == keyed[first].0 {
                    last += 1;
                }
                let paired_last = first + (last - first) / 2 * 2;
                for slot in first..paired_last {
                    support::bits::set(&mut bits, keyed[slot].1);
                    removed += 1;
                }
                first = last;
            }
            let selected = group::view::of_owned_bits(
                group::Owner::Primitive,
                "__rdk_boolean_paired_rounded_sliver",
                primitive_count,
                bits,
            )
            .map_err(raised)?;
            Ok((removed, selected))
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

fn closed_rounded_slivers(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<(bool, i64, group::T), error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    (|| {
            cancel::check_opt(cx(cancel))?;
            let topology_value = geometry::topology(geometry_value);
            let topology = topology_view(topology_value)?;
            let positions = positions_view(geometry_value)?;
            let index_value = topology_index::create(cx(cancel), topology_value)?;
            let index = topology_index::view::view(&index_value);
            let primitive_count = geometry::primitive_count(geometry_value);
            let mut selected = vec![0u8; bits_len(primitive_count)];
            let mut selected_count = 0;
            for primitive in 0..primitive_count {
                cancel::poll(cx(cancel), primitive, 4095)?;
                if rounded_triangle_degenerate(&positions, &topology, primitive) {
                    support::bits::set(&mut selected, primitive);
                    selected_count += 1;
                }
            }
            let mut valid = selected_count > 0;
            let mut edge = 0usize;
            while valid && edge < index.edge_a.len() {
                let first = index.edge_offsets[edge];
                let last = index.edge_offsets[edge + 1];
                let mut removed = 0;
                for slot in first..last {
                    let primitive = index.primitive_of_vertex[u(index.edge_vertices[u(slot)])];
                    if support::bits::mem(&selected, primitive) {
                        removed += 1;
                    }
                }
                let remaining = last - first - removed;
                if remaining != 0 && (remaining < 2 || remaining & 1 != 0) {
                    valid = false;
                }
                edge += 1;
            }
            let selection = group::view::of_owned_bits(
                group::Owner::Primitive,
                "__rdk_boolean_closed_rounded_sliver",
                primitive_count,
                selected,
            )
            .map_err(raised)?;
            Ok((valid, selected_count, selection))
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

fn delete_rounding_pairs(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    selected: &group::T,
    cleanup: &Cleanup,
) -> Result<Cleanup, error::T> {
    if group::cardinality(selected) == 0 {
        return Ok(cleanup.clone());
    }
    (|| {
            let tagged = tag(cleanup)?;
            let output = deletion::delete_primitives(
                deletion::DeletePrimitivesParams {
                    cancel: cancel.cloned(),
                    grain: Some(grain),
                    compact_points: true,
                },
                selected,
                &tagged.geometry,
            )?;
            let composed = compose_output(
                &tagged,
                cleanup,
                &output,
                "Boolean rounding-pair remap is out of range",
                "Boolean seam-edge scratch was lost",
            )?;
            Ok(Cleanup {
                cleanup_geometry: composed.geometry,
                cleanup_rounding_repaired_count: cleanup.cleanup_rounding_repaired_count + group::cardinality(selected),
                cleanup_point_sources: composed.points,
                cleanup_vertex_sources: composed.vertices,
                cleanup_primitive_sources: composed.primitives,
                cleanup_seam_edges: composed.seams,
                ..cleanup.clone()
            })
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

fn run_edge_collapse(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    selected: &edge_group::T,
    position: fuse_reduce::Position,
    tagged: &Tagged,
) -> Result<geometry::T, error::T> {
    edge_collapse::run(
        &edge_collapse::Params {
            cancel: cancel.cloned(),
            grain: Some(grain),
            edges: Some(selected.clone()),
            connectivity_attribute: None,
            position: Some(position),
            remove_degenerate_primitives: Some(true),
            recompute_point_normals: Some(true),
        },
        &tagged.geometry,
    )
}

fn collapse_rounding_batch(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    keep_greatest: bool,
    selected: &edge_group::T,
    cleanup: &Cleanup,
) -> Result<Cleanup, error::T> {
    if edge_group::cardinality(selected) == 0 {
        return Ok(cleanup.clone());
    }
    (|| {
            let tagged = tag(cleanup)?;
            let position = if keep_greatest {
                fuse_reduce::Position::GreatestPoint
            } else {
                fuse_reduce::Position::LeastPoint
            };
            let output = run_edge_collapse(cancel, grain, selected, position, &tagged)?;
            let composed = compose_output(
                &tagged,
                cleanup,
                &output,
                "Boolean rounding-repair remap is out of range",
                "Boolean seam-edge scratch was lost",
            )?;
            Ok(Cleanup {
                cleanup_geometry: composed.geometry,
                cleanup_rounding_repaired_count: cleanup.cleanup_rounding_repaired_count
                    + edge_group::cardinality(selected),
                cleanup_point_sources: composed.points,
                cleanup_vertex_sources: composed.vertices,
                cleanup_primitive_sources: composed.primitives,
                cleanup_seam_edges: composed.seams,
                ..cleanup.clone()
            })
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

enum Step {
    Done(Cleanup),
    Next(Cleanup),
}

fn next_attempt(slot: i64, keep_greatest: bool) -> (i64, bool) {
    if keep_greatest { (slot + 1, false) } else { (slot, true) }
}

/// The speculative one-edge-at-a-time contraction search of the self-contact branch.
fn attempt_contacts(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    contacts: i64,
    repairable: i64,
    candidates: &edge_group::T,
    cleanup: &Cleanup,
) -> Result<Step, error::T> {
    let topology = geometry::topology(&cleanup.cleanup_geometry);
    let index = topology_index::create(cx(cancel), topology).map_err(raised)?;
    let positions = positions_view(&cleanup.cleanup_geometry)?;
    let mut ordered = members(candidates);
    let point_lengths = |edge: i64| -> Result<f64, error::T> {
        let (a, b) = topology_index::edge_points(&index, edge).map_err(raised)?;
        Ok(edge_length(&positions, a, b))
    };
    let mut keyed = Vec::with_capacity(ordered.len());
    for edge in &ordered {
        keyed.push((point_lengths(*edge)?, *edge));
    }
    keyed.sort_by(|first, second| float_compare(first.0, second.0).then(first.1.cmp(&second.1)));
    ordered = keyed.into_iter().map(|(_, edge)| edge).collect();
    let limit = std::cmp::min(64, ordered.len() as i64);
    let edge_count = topology_index::edge_count(&index);
    let mut slot = 0i64;
    let mut keep_greatest = false;
    let mut last_error: Option<error::T> = None;
    loop {
        if slot >= limit {
            return match last_error {
                Some(failure) => Err(failure),
                None => Err(fail_with(Code::UnresolvedRoundingContacts, &format!(
                        "{contacts} rounded self-contact(s), {repairable} ULP-scale incident triangle(s), and no certified contraction"
                    ))),
            };
        }
        let edge = ordered[u(slot)];
        let mut bits = vec![0u8; bits_len(edge_count)];
        support::bits::set(&mut bits, edge);
        let selected = edge_group::view::of_owned_bits(
            topology,
            edge_count,
            "__rdk_boolean_speculative_rounding_contact",
            bits,
        )
        .map_err(raised)?;
        let candidate = match collapse_rounding_batch(cancel, grain, keep_greatest, &selected, cleanup) {
            Err(failure) if error::is_cancelled(&failure) => return Err(failure),
            Err(failure) => {
                (slot, keep_greatest) = next_attempt(slot, keep_greatest);
                last_error = Some(failure);
                continue;
            }
            Ok(candidate) => candidate,
        };
        match rounded_sliver_edges(cancel, grain, &candidate.cleanup_geometry) {
            Err(failure) if error::is_cancelled(&failure) => return Err(failure),
            Err(failure) => {
                (slot, keep_greatest) = next_attempt(slot, keep_greatest);
                last_error = Some(failure);
                continue;
            }
            Ok((slivers, _)) if slivers != 0 => {
                (slot, keep_greatest) = next_attempt(slot, keep_greatest);
                continue;
            }
            Ok(_) => {}
        }
        match rounded_self_contact_edges(cancel, grain, &candidate.cleanup_geometry) {
            Err(failure) if error::is_cancelled(&failure) => return Err(failure),
            Ok((next_contacts, _, _)) if next_contacts < contacts => return Ok(Step::Next(candidate)),
            Err(failure) => {
                (slot, keep_greatest) = next_attempt(slot, keep_greatest);
                last_error = Some(failure);
            }
            Ok(_) => {
                (slot, keep_greatest) = next_attempt(slot, keep_greatest);
            }
        }
    }
}

/// One certified batch: least-point contraction, then greatest-point.
fn certified_batch(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    candidates: &edge_group::T,
    cleanup: &Cleanup,
) -> Result<Option<Cleanup>, error::T> {
    let least = safe_independent_edges(cancel, grain, None, candidates, &cleanup.cleanup_geometry)?;
    if edge_group::cardinality(&least) > 0 {
        return collapse_rounding_batch(cancel, grain, false, &least, cleanup).map(Some);
    }
    let greatest = safe_independent_edges(cancel, grain, Some(true), candidates, &cleanup.cleanup_geometry)?;
    if edge_group::cardinality(&greatest) > 0 {
        return collapse_rounding_batch(cancel, grain, true, &greatest, cleanup).map(Some);
    }
    Ok(None)
}

#[allow(clippy::too_many_arguments)]
fn repair_step(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    require_closed: bool,
    allow_opposite_duplicates: bool,
    max_batches: i64,
    ancestry: &boolean_extract::Ancestry,
    seam: &boolean_seam::T,
    batch: i64,
    cleanup: &Cleanup,
) -> Result<Step, error::T> {
    let (remaining, candidates) = rounded_sliver_edges(cancel, grain, &cleanup.cleanup_geometry)?;
    if remaining == 0 {
        let geometry_value = &cleanup.cleanup_geometry;
        let extracted = boolean_extract::geometry(ancestry);
        let verified = if packed::float3::data_id(geometry::positions(geometry_value))
            == packed::float3::data_id(geometry::positions(&extracted))
            && topology::data_id(geometry::topology(geometry_value)) == topology::data_id(geometry::topology(&extracted))
        {
            verify_extracted_surface(cancel, grain, require_closed, allow_opposite_duplicates, ancestry, geometry_value)
        } else {
            verify_surface(cancel, grain, require_closed, allow_opposite_duplicates, geometry_value)
        };
        return match verified {
            Ok(()) => {
                verify_materialized_seam_curves(cancel, grain, seam)?;
                Ok(Step::Done(cleanup.clone()))
            }
            Err(initial) if error::code(&initial) != Code::SurfaceSelfIntersection => Err(initial),
            Err(initial) => {
                let (contacts, repairable, candidates) =
                    rounded_self_contact_edges(cancel, grain, &cleanup.cleanup_geometry)?;
                if contacts > 0 && batch >= max_batches {
                    return Err(fail_with(Code::UnresolvedRoundingContacts, &format!("{contacts} rounded self-contact(s) remain after {max_batches} certified batch(es)")));
                }
                if contacts <= 0 {
                    return Err(initial);
                }
                if let Some(next) = certified_batch(cancel, grain, &candidates, cleanup)? {
                    return Ok(Step::Next(next));
                }
                let speculative =
                    speculative_independent_edges(cancel, grain, &candidates, &cleanup.cleanup_geometry)?;
                if edge_group::cardinality(&speculative) > 1 {
                    return collapse_rounding_batch(cancel, grain, false, &speculative, cleanup).map(Step::Next);
                }
                attempt_contacts(cancel, grain, contacts, repairable, &candidates, cleanup)
            }
        };
    }
    if batch >= max_batches {
        return Err(fail_with(Code::UnresolvedRoundingSlivers, &format!("{remaining} rounded degenerate triangle(s) remain after {max_batches} certified batch(es)")));
    }
    let (closed_valid, _, closed) = closed_rounded_slivers(cancel, grain, &cleanup.cleanup_geometry)?;
    if closed_valid {
        return delete_rounding_pairs(cancel, grain, &closed, cleanup).map(Step::Next);
    }
    let (removed, paired) = paired_rounded_slivers(cancel, grain, &cleanup.cleanup_geometry)?;
    if removed > 0 {
        return delete_rounding_pairs(cancel, grain, &paired, cleanup).map(Step::Next);
    }
    if let Some(next) = certified_batch(cancel, grain, &candidates, cleanup)? {
        return Ok(Step::Next(next));
    }
    let topology = geometry::topology(&cleanup.cleanup_geometry);
    let index = topology_index::create(cx(cancel), topology).map_err(raised)?;
    let mut two_sided = 0;
    let mut other = 0;
    for edge in members(&candidates) {
        if topology_index::edge_incidence_count(&index, edge).map_err(raised)? == 2 {
            two_sided += 1;
        } else {
            other += 1;
        }
    }
    Err(fail_with(Code::UnresolvedRoundingSlivers, &format!(
            "{remaining} rounded degenerate triangle(s) have no certified edge contraction ({two_sided} two-sided and {other} other candidate edges)"
        )))
}

fn repair_rounded_slivers(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    require_closed: bool,
    allow_opposite_duplicates: bool,
    max_batches: i64,
    ancestry: &boolean_extract::Ancestry,
    seam: &boolean_seam::T,
    geometry_value: &geometry::T,
) -> Result<Cleanup, error::T> {
    let exact_seams = surface_seam_edges(cancel, grain, ancestry, seam)?;
    let mut cleanup = Cleanup {
        cleanup_geometry: geometry_value.clone(),
        cleanup_candidate_count: 0,
        cleanup_collapsed_count: 0,
        cleanup_batch_count: 0,
        cleanup_rounding_repaired_count: 0,
        cleanup_point_sources: (0..geometry::point_count(geometry_value)).collect(),
        cleanup_vertex_sources: (0..geometry::vertex_count(geometry_value)).collect(),
        cleanup_primitive_sources: (0..geometry::primitive_count(geometry_value)).collect(),
        cleanup_seam_edges: exact_seams,
    };
    let mut batch = 0;
    loop {
        match repair_step(
            cancel,
            grain,
            require_closed,
            allow_opposite_duplicates,
            max_batches,
            ancestry,
            seam,
            batch,
            &cleanup,
        )? {
            Step::Done(done) => return Ok(done),
            Step::Next(next) => {
                cleanup = next;
                batch += 1;
            }
        }
    }
}

fn tiny_cleanup_edges(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    threshold: f64,
    ancestry: &boolean_extract::Ancestry,
    seam: &boolean_seam::T,
    cleanup: &Cleanup,
) -> Result<edge_group::T, error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    if !threshold.is_finite() || threshold < 0. {
        return Err(fail_with(Code::InvalidParameter, "threshold must be finite and non-negative"));
    }
    (|| {
            cancel::check_opt(cx(cancel))?;
            let complex = boolean_extract::view::complex(ancestry);
            if !Arc::ptr_eq(boolean_seam::view::complex(seam), complex) {
                return Err(inv("Boolean seam and extraction belong to different exact complexes"));
            }
            let extraction_primitives = geometry::primitive_count(&boolean_extract::geometry(ancestry));
            let geometry_value = &cleanup.cleanup_geometry;
            let mut seam_facets = vec![0u8; u(crate::boolean_complex::facet_count(&complex))];
            for complex_edge in 0..crate::boolean_complex::edge_count(&complex) {
                cancel::poll(cx(cancel), complex_edge, 4095)?;
                if boolean_seam::view::is_seam_edge(seam, complex_edge) {
                    let (first, last) = crate::boolean_complex::edge_incident_range(&complex, complex_edge);
                    for incident in first..last {
                        seam_facets[u(crate::boolean_complex::edge_incident_facet(&complex, incident))] = 1;
                    }
                }
            }
            let topology = geometry::topology(geometry_value);
            let index_value = topology_index::create(cx(cancel), topology)?;
            let index = topology_index::view::view(&index_value);
            let positions = positions_view(geometry_value)?;
            let failure: Mutex<Option<error::T>> = Mutex::new(None);
            let group = edge_group::init(
                Some(grain),
                topology,
                &index_value,
                "__rdk_boolean_tiny_seam_continuation",
                &|edge| {
                    let outcome = (|| -> Result<bool, error::T> {
                        cancel::poll(cx(cancel), edge, 4095)?;
                        let first = index.edge_offsets[u(edge)];
                        let last = index.edge_offsets[u(edge + 1)];
                        let mut adjacent = false;
                        let mut incident = first;
                        while !adjacent && incident < last {
                            let primitive = index.primitive_of_vertex[u(index.edge_vertices[u(incident)])];
                            let extraction = cleanup.cleanup_primitive_sources[u(primitive)];
                            if extraction < 0 || extraction >= extraction_primitives {
                                return Err(inv("Boolean cleanup primitive ancestry is out of range"));
                            }
                            let facet = boolean_extract::view::primitive_complex_facet(ancestry, extraction);
                            adjacent = seam_facets[u(facet)] != 0;
                            incident += 1;
                        }
                        Ok(adjacent
                            && edge_length(&positions, index.edge_a[u(edge)], index.edge_b[u(edge)]) <= threshold)
                    })();
                    match outcome {
                        Ok(value) => value,
                        Err(problem) => {
                            let mut slot = failure.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                            if slot.is_none() {
                                *slot = Some(problem);
                            }
                            false
                        }
                    }
                },
            )
            .map_err(raised)?;
            if let Some(problem) = failure.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
                return Err(problem);
            }
            Ok(group)
        })().map_err(|f| guard_fail(Code::InvalidInput, f))
}

fn collapse_cleanup_batch(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    require_closed: bool,
    allow_opposite_duplicates: bool,
    seam: &boolean_seam::T,
    selected: &edge_group::T,
    cleanup: &Cleanup,
) -> Result<Cleanup, error::T> {
    if edge_group::cardinality(selected) == 0 {
        return Ok(cleanup.clone());
    }
    (|| {
            let tagged = tag(cleanup)?;
            let output = run_edge_collapse(cancel, grain, selected, fuse_reduce::Position::First, &tagged)?;
            let composed = compose_output(
                &tagged,
                cleanup,
                &output,
                "Boolean cleanup continuation remap is out of range",
                "Boolean seam-edge scratch was lost",
            )?;
            verify_materialized_seam_curves(cancel, grain, seam)?;
            verify_surface(cancel, grain, require_closed, allow_opposite_duplicates, &composed.geometry)?;
            Ok(Cleanup {
                cleanup_geometry: composed.geometry,
                cleanup_candidate_count: cleanup.cleanup_candidate_count,
                cleanup_collapsed_count: cleanup.cleanup_collapsed_count + edge_group::cardinality(selected),
                cleanup_batch_count: cleanup.cleanup_batch_count + 1,
                cleanup_rounding_repaired_count: cleanup.cleanup_rounding_repaired_count,
                cleanup_point_sources: composed.points,
                cleanup_vertex_sources: composed.vertices,
                cleanup_primitive_sources: composed.primitives,
                cleanup_seam_edges: composed.seams,
            })
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

/// Continue deterministic independent contraction batches until no tiny seam-adjacent candidate
/// remains, the explicit batch bound is reached, or no candidate has a certified contraction.
/// Strict mode returns the stable `unresolved_cleanup` diagnostic instead of publishing a partial
/// cleanup. Before optional threshold cleanup, a separately bounded mandatory pass removes only
/// closed/pair-canceling zero-area subcomplexes and contracts only ULP-scale edges.
#[allow(clippy::too_many_arguments)]
pub fn collapse_tiny_seams(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    threshold: f64,
    require_closed: bool,
    allow_opposite_duplicates: Option<bool>,
    max_batches: Option<i64>,
    strict: Option<bool>,
    ancestry: &boolean_extract::Ancestry,
    seam: &boolean_seam::T,
    geometry_value: &geometry::T,
) -> Result<Cleanup, error::T> {
    let allow_opposite_duplicates = allow_opposite_duplicates.unwrap_or(false);
    let max_batches = max_batches.unwrap_or(8);
    let strict = strict.unwrap_or(true);
    if max_batches <= 0 {
        return Err(fail_with(Code::InvalidParameter, "max_batches must be positive"));
    }
    let mut cleanup = repair_rounded_slivers(
        cancel,
        grain,
        require_closed,
        allow_opposite_duplicates,
        64,
        ancestry,
        seam,
        geometry_value,
    )?;
    loop {
        let candidates = tiny_cleanup_edges(cancel, grain, threshold, ancestry, seam, &cleanup)?;
        let remaining = edge_group::cardinality(&candidates);
        if cleanup.cleanup_batch_count == 0 {
            cleanup.cleanup_candidate_count = remaining;
        }
        if remaining == 0 {
            return Ok(cleanup);
        }
        if cleanup.cleanup_batch_count >= max_batches {
            return if strict {
                Err(fail_with(Code::UnresolvedCleanup, &format!(
                        "{remaining} tiny seam-adjacent edge candidate(s) remain after {max_batches} batch(es)"
                    )))
            } else {
                Ok(cleanup)
            };
        }
        let selected = safe_independent_edges(cancel, grain, None, &candidates, &cleanup.cleanup_geometry)?;
        if edge_group::cardinality(&selected) == 0 {
            return if strict {
                Err(fail_with(Code::UnresolvedCleanup, &format!("{remaining} tiny seam-adjacent edge candidate(s) have no certified contraction")))
            } else {
                Ok(cleanup)
            };
        }
        cleanup = collapse_cleanup_batch(
            cancel,
            grain,
            require_closed,
            allow_opposite_duplicates,
            seam,
            &selected,
            &cleanup,
        )?;
    }
}

/// Duplicate each seam point once per primitive component obtained after removing exact seam
/// edges. Corner/primitive order is unchanged; point payload and native edge groups follow exact
/// packed ancestry.
pub fn split_seam_points(cancel: Option<&Arc<cancel::T>>, grain: i64, cleanup: &Cleanup) -> Result<Cleanup, error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    if edge_group::cardinality(&cleanup.cleanup_seam_edges) == 0 {
        return Ok(cleanup.clone());
    }
    (|| {
            cancel::check_opt(cx(cancel))?;
            let geometry_value = &cleanup.cleanup_geometry;
            let topology_value = geometry::topology(geometry_value);
            let topology = topology_view(topology_value)?;
            let index_value = topology_index::create(cx(cancel), topology_value)?;
            let index = topology_index::view::view(&index_value);
            let primitive_count = u(geometry::primitive_count(geometry_value));
            let point_count = u(geometry::point_count(geometry_value));
            let vertex_count = u(geometry::vertex_count(geometry_value));
            if edge_group::topology_data_id(&cleanup.cleanup_seam_edges) != topology::data_id(topology_value)
                || edge_group::length(&cleanup.cleanup_seam_edges) != index.edge_a.len() as i64
            {
                return Err(inv("Boolean seam split group belongs to a different topology"));
            }
            let mut parent = DisjointSet::new(primitive_count);
            for edge in 0..index.edge_a.len() {
                cancel::poll(cx(cancel), edge as i64, 4095)?;
                if !edge_group::mem(edge as i64, &cleanup.cleanup_seam_edges) {
                    let first = index.edge_offsets[edge];
                    let last = index.edge_offsets[edge + 1];
                    if first < last {
                        let base = u(index.primitive_of_vertex[u(index.edge_vertices[u(first)])]);
                        for incident in first + 1..last {
                            let other = u(index.primitive_of_vertex[u(index.edge_vertices[u(incident)])]);
                            parent.union_rank(base, other, false);
                        }
                    }
                }
            }
            let mut root_component = vec![-1i64; primitive_count];
            let mut primitive_component = vec![0i64; primitive_count];
            let mut component_count = 0i64;
            for primitive in 0..primitive_count {
                let root = parent.find(primitive);
                if root_component[root] < 0 {
                    root_component[root] = component_count;
                    component_count += 1;
                }
                primitive_component[primitive] = root_component[root];
            }
            let mut component_stamps = vec![-1i64; u(component_count)];
            let mut point_offsets = vec![0i64; point_count + 1];
            for point in 0..point_count {
                cancel::poll(cx(cancel), point as i64, 4095)?;
                let first = index.point_offsets[point];
                let last = index.point_offsets[point + 1];
                let mut count = 0i64;
                for incident in first..last {
                    let primitive = index.primitive_of_vertex[u(index.point_vertices[u(incident)])];
                    let component = primitive_component[u(primitive)];
                    if component_stamps[u(component)] != point as i64 {
                        component_stamps[u(component)] = point as i64;
                        count += 1;
                    }
                }
                let count = if count == 0 { 1 } else { count };
                if point_offsets[point] > MAX_ARRAY_LENGTH - count {
                    return Err(inv("Boolean seam point cardinality exceeds array limits"));
                }
                point_offsets[point + 1] = point_offsets[point] + count;
            }
            let output_points = point_offsets[point_count];
            let mut point_map = vec![0i64; u(output_points)];
            let mut vertex_points = vec![0i64; vertex_count];
            let mut component_output = vec![-1i64; u(component_count)];
            component_stamps.iter_mut().for_each(|stamp| *stamp = -1);
            for point in 0..point_count {
                let first = index.point_offsets[point];
                let last = index.point_offsets[point + 1];
                let mut next = point_offsets[point];
                if first == last {
                    point_map[u(next)] = point as i64;
                    next += 1;
                } else {
                    for incident in first..last {
                        let vertex = index.point_vertices[u(incident)];
                        let primitive = index.primitive_of_vertex[u(vertex)];
                        let component = primitive_component[u(primitive)];
                        if component_stamps[u(component)] != point as i64 {
                            component_stamps[u(component)] = point as i64;
                            component_output[u(component)] = next;
                            point_map[u(next)] = point as i64;
                            next += 1;
                        }
                        vertex_points[u(vertex)] = component_output[u(component)];
                    }
                }
                if next != point_offsets[point + 1] {
                    return Err(inv("Boolean seam point plan did not match its prefix"));
                }
            }
            let target_topology = topology::view::create_validated_owned(
                output_points,
                vertex_points,
                topology.primitive_offsets.to_vec(),
                topology.primitive_kinds.to_vec(),
            );
            let source_positions = positions_view(geometry_value)?;
            let select = |source: &[f64]| -> Result<Vec<f64>, error::T> {
                let mut values = vec![0.0_f64; point_map.len()];
                support::par_fill(cx(cancel), grain, &mut values, &|output| Ok(source[u(point_map[output as usize])]))?;
                Ok(values)
            };
            let positions = packed::float3::view::of_owned_exn(
                select(&source_positions.x)?,
                select(&source_positions.y)?,
                select(&source_positions.z)?,
            )
            .map_err(raised)?;
            let identity_vertices: Vec<i64> = (0..vertex_count as i64).collect();
            let identity_primitives: Vec<i64> = (0..primitive_count as i64).collect();
            let attributes = topology_remap::attributes(
                cx(cancel),
                grain,
                &point_map,
                &identity_vertices,
                &identity_primitives,
                geometry_value,
            )
            .map_err(raised)?;
            let groups = topology_remap::groups(
                cx(cancel),
                grain,
                &point_map,
                &identity_vertices,
                &identity_primitives,
                geometry_value,
            )
            .map_err(raised)?;
            let edge_groups = topology_remap::split_point_edge_groups(
                cx(cancel),
                grain,
                &index_value,
                &target_topology,
                &geometry::edge_groups(geometry_value),
            )
            .map_err(|failure| inv(error::message(&failure)))?;
            let seam_edges = match topology_remap::split_point_edge_groups(
                cx(cancel),
                grain,
                &index_value,
                &target_topology,
                std::slice::from_ref(&cleanup.cleanup_seam_edges),
            )
            .map_err(|failure| inv(error::message(&failure)))?
            .as_slice()
            {
                [group] => group.clone(),
                _ => return Err(inv("Boolean seam split produced invalid edge ancestry")),
            };
            let split = geometry::create(positions, target_topology, Some(attributes), Some(groups), Some(edge_groups))
                .map_err(|failure| inv(error::message(&failure)))?;
            Ok(Cleanup {
                cleanup_geometry: split,
                cleanup_point_sources: point_map.iter().map(|point| cleanup.cleanup_point_sources[u(*point)]).collect(),
                cleanup_seam_edges: seam_edges,
                ..cleanup.clone()
            })
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Detriangulation {
    Triangles,
    AllPolygons,
    UnchangedPolygons,
}

/// Merge adjacent triangles only when their exact extraction ancestry names the same source
/// polygon and the edge is not a Boolean seam.
pub fn detriangulate(
    cancel: Option<&Arc<cancel::T>>,
    grain: i64,
    assume_flat: bool,
    mode: Detriangulation,
    ancestry: &boolean_extract::Ancestry,
    cleanup: &Cleanup,
) -> Result<Cleanup, error::T> {
    support::check_positive(grain).map_err(|_| fail_with(Code::InvalidParameter, "grain must be positive"))?;
    if mode == Detriangulation::Triangles {
        return Ok(cleanup.clone());
    }
    (|| {
            cancel::check_opt(cx(cancel))?;
            let geometry_value = &cleanup.cleanup_geometry;
            let topology_value = geometry::topology(geometry_value);
            let topology = topology_view(topology_value)?;
            let index_value = topology_index::create(cx(cancel), topology_value)?;
            let index = topology_index::view::view(&index_value);
            let primitive_count = u(geometry::primitive_count(geometry_value));
            if cleanup.cleanup_primitive_sources.len() != primitive_count {
                return Err(inv("Boolean detriangulation primitive ancestry is inconsistent"));
            }
            let extraction_primitives = geometry::primitive_count(&boolean_extract::geometry(ancestry));
            let left_count = u(geometry::primitive_count(&boolean_extract::view::left_geometry(ancestry)));
            let right_count = u(geometry::primitive_count(&boolean_extract::view::right_geometry(ancestry)));
            let mut left_cut = vec![0u8; left_count];
            let mut right_cut = vec![0u8; right_count];
            for primitive in 0..extraction_primitives {
                if boolean_extract::primitive_refined_triangle(ancestry, primitive) >= 0 {
                    let face = u(boolean_extract::primitive_face(ancestry, primitive));
                    match boolean_extract::primitive_side(ancestry, primitive) {
                        Side::Left => left_cut[face] = 1,
                        Side::Right => right_cut[face] = 1,
                    }
                }
            }
            let source = |primitive: i64| -> Result<(Side, i64), error::T> {
                let extraction = cleanup.cleanup_primitive_sources[u(primitive)];
                if extraction < 0 || extraction >= extraction_primitives {
                    return Err(inv("Boolean detriangulation source primitive is out of range"));
                }
                Ok((
                    boolean_extract::primitive_side(ancestry, extraction),
                    boolean_extract::primitive_face(ancestry, extraction),
                ))
            };
            let unchanged = |side: Side, face: i64| match side {
                Side::Left => left_cut[u(face)] == 0,
                Side::Right => right_cut[u(face)] == 0,
            };
            let positions = positions_view(geometry_value)?;
            let coplanar = |edge: i64| {
                let first = index.edge_offsets[u(edge)];
                let first_vertex = index.edge_vertices[u(first)];
                let second_vertex = index.edge_vertices[u(first + 1)];
                let opposite = |vertex: i64| {
                    topology.vertex_points[u(index.next_vertex[u(index.next_vertex[u(vertex)])])]
                };
                let a = index.edge_a[u(edge)];
                let b = index.edge_b[u(edge)];
                let c = opposite(first_vertex);
                let d = opposite(second_vertex);
                predicates::orient3d_packed(&positions.x, &positions.y, &positions.z, a, b, c, d) == Sign::Zero
            };
            let failure: Mutex<Option<error::T>> = Mutex::new(None);
            let selected = edge_group::init(
                Some(grain),
                topology_value,
                &index_value,
                "__rdk_boolean_detriangulate",
                &|edge| {
                    let outcome = (|| -> Result<bool, error::T> {
                        cancel::poll(cx(cancel), edge, 4095)?;
                        let first = index.edge_offsets[u(edge)];
                        let last = index.edge_offsets[u(edge + 1)];
                        if last - first != 2 || edge_group::mem(edge, &cleanup.cleanup_seam_edges) {
                            return Ok(false);
                        }
                        let first_primitive = index.primitive_of_vertex[u(index.edge_vertices[u(first)])];
                        let second_primitive = index.primitive_of_vertex[u(index.edge_vertices[u(first + 1)])];
                        if topology.primitive_offsets[u(first_primitive + 1)]
                            - topology.primitive_offsets[u(first_primitive)]
                            != 3
                            || topology.primitive_offsets[u(second_primitive + 1)]
                                - topology.primitive_offsets[u(second_primitive)]
                                != 3
                        {
                            return Ok(false);
                        }
                        let (first_side, first_face) = source(first_primitive)?;
                        let (second_side, second_face) = source(second_primitive)?;
                        Ok(first_side == second_side
                            && first_face == second_face
                            && (mode == Detriangulation::AllPolygons || unchanged(first_side, first_face))
                            && (assume_flat || coplanar(edge)))
                    })();
                    match outcome {
                        Ok(value) => value,
                        Err(problem) => {
                            let mut slot = failure.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                            if slot.is_none() {
                                *slot = Some(problem);
                            }
                            false
                        }
                    }
                },
            )
            .map_err(raised)?;
            if let Some(problem) = failure.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner) {
                return Err(problem);
            }
            // Dissolving every diagonal of a cut source face is valid only when each selected
            // triangle component has one simple boundary cycle. A component with a hole would
            // otherwise be encoded by Dissolve as a repeated bridge walk, which is deliberately not
            // a valid RDK polygon. Keep such a component triangulated rather than publishing
            // non-simple topology.
            let selected = if mode == Detriangulation::UnchangedPolygons || edge_group::cardinality(&selected) == 0 {
                selected
            } else {
                let selected_edges = members(&selected);
                let mut parent = DisjointSet::new(primitive_count);
                for edge in &selected_edges {
                    let first = index.edge_offsets[u(*edge)];
                    let a = u(index.primitive_of_vertex[u(index.edge_vertices[u(first)])]);
                    let b = u(index.primitive_of_vertex[u(index.edge_vertices[u(first + 1)])]);
                    parent.union_min(a, b);
                }
                for primitive in 0..primitive_count {
                    let root = parent.find(primitive);
                    parent.parent[primitive] = root as i64;
                }
                let mut component_selected = vec![0u8; primitive_count];
                let mut head = vec![-1i64; primitive_count];
                let mut next = vec![-1i64; primitive_count];
                for primitive in (0..primitive_count).rev() {
                    let root = parent.parent[primitive] as usize;
                    next[primitive] = head[root];
                    head[root] = primitive as i64;
                }
                for edge in &selected_edges {
                    let first = index.edge_offsets[u(*edge)];
                    let primitive = u(index.primitive_of_vertex[u(index.edge_vertices[u(first)])]);
                    component_selected[parent.parent[primitive] as usize] = 1;
                }
                let points = u(geometry::point_count(geometry_value));
                let mut stamp = vec![-1i64; points];
                let mut degree = vec![0i64; points];
                let mut boundary_parent = DisjointSet::new(points);
                let mut touched = vec![0usize; points];
                let mut safe = vec![0u8; primitive_count];
                for root in 0..primitive_count {
                    if component_selected[root] == 0 {
                        continue;
                    }
                    let mut touched_count = 0usize;
                    let mut valid = true;
                    let mut primitive = head[root];
                    while primitive >= 0 {
                        let first = topology.primitive_offsets[u(primitive)];
                        let last = topology.primitive_offsets[u(primitive + 1)];
                        for vertex in first..last {
                            let edge = index.edge_of_vertex[u(vertex)];
                            if edge >= 0 && !edge_group::mem(edge, &selected) {
                                let mut internal = false;
                                for incident in index.edge_offsets[u(edge)]..index.edge_offsets[u(edge + 1)] {
                                    let other = index.primitive_of_vertex[u(index.edge_vertices[u(incident)])];
                                    if other != primitive && parent.parent[u(other)] == root as i64 {
                                        internal = true;
                                    }
                                }
                                if internal {
                                    valid = false;
                                } else {
                                    let a = u(index.edge_a[u(edge)]);
                                    let b = u(index.edge_b[u(edge)]);
                                    for point in [a, b] {
                                        if stamp[point] != root as i64 {
                                            stamp[point] = root as i64;
                                            degree[point] = 0;
                                            boundary_parent.parent[point] = point as i64;
                                            touched[touched_count] = point;
                                            touched_count += 1;
                                        }
                                    }
                                    degree[a] += 1;
                                    degree[b] += 1;
                                    let a_root = boundary_parent.find(a);
                                    let b_root = boundary_parent.find(b);
                                    if a_root != b_root {
                                        boundary_parent.parent[b_root] = a_root as i64;
                                    }
                                }
                            }
                        }
                        primitive = next[u(primitive)];
                    }
                    let mut cycles = 0;
                    for slot in 0..touched_count {
                        let point = touched[slot];
                        if degree[point] != 2 {
                            valid = false;
                        }
                        if boundary_parent.find(point) == point {
                            cycles += 1;
                        }
                    }
                    if valid && cycles == 1 {
                        safe[root] = 1;
                    }
                }
                edge_group::init(
                    Some(grain),
                    topology_value,
                    &index_value,
                    "__rdk_boolean_detriangulate_safe",
                    &|edge| {
                        if !edge_group::mem(edge, &selected) {
                            false
                        } else {
                            let first = index.edge_offsets[u(edge)];
                            let primitive = u(index.primitive_of_vertex[u(index.edge_vertices[u(first)])]);
                            safe[parent.parent[primitive] as usize] != 0
                        }
                    },
                )
                .map_err(raised)?
            };
            if edge_group::cardinality(&selected) == 0 {
                return Ok(cleanup.clone());
            }
            let tagged = tag(cleanup)?;
            let output = dissolve::run(
                &dissolve::Params {
                    cancel: cancel.cloned(),
                    grain: Some(grain),
                    edges: Some(selected),
                    operation: dissolve::Operation::DissolveSelected,
                    bridge_policy: dissolve::BridgePolicy::CreateBridgedPolygons,
                    remove_inline_points: false,
                    collinearity_tolerance: 0.,
                    remove_unused_points: false,
                    create_boundary_curves: false,
                    recompute_normals: false,
                },
                &tagged.geometry,
            )?;
            let composed = compose_output(
                &tagged,
                cleanup,
                &output,
                "Boolean detriangulation remap is out of range",
                "Boolean detriangulation seam ancestry was lost",
            )?;
            Ok(Cleanup {
                cleanup_geometry: composed.geometry,
                cleanup_point_sources: composed.points,
                cleanup_vertex_sources: composed.vertices,
                cleanup_primitive_sources: composed.primitives,
                cleanup_seam_edges: composed.seams,
                ..cleanup.clone()
            })
        })().map_err(|f| guard_fail(Code::InvalidOutput, f))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cfmt_g17_matches_c() {
        assert_eq!(cfmt::g(0.1, 17), "0.10000000000000001");
        assert_eq!(cfmt::g(1.0, 17), "1");
        assert_eq!(cfmt::g(0.0, 17), "0");
        assert_eq!(cfmt::g(1.5e-10, 17), "1.5e-10");
        assert_eq!(cfmt::g(2.5e20, 17), "2.5e+20");
        assert_eq!(cfmt::g(f64::INFINITY, 17), "inf");
    }

    #[test]
    fn ocaml_heap_sort_sorts_by_key_only() {
        let mut state = 12345u64;
        for length in [0usize, 1, 2, 3, 4, 7, 20, 101] {
            let mut items: Vec<((i64, i64, i64), i64)> = (0..length)
                .map(|slot| {
                    state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                    (((state >> 33) as i64 % 5, (state >> 40) as i64 % 3, 0), slot as i64)
                })
                .collect();
            let mut expected: Vec<i64> = items.iter().map(|item| item.1).collect();
            ocaml_sort_keyed(&mut items);
            assert!(items.windows(2).all(|pair| pair[0].0 <= pair[1].0));
            let mut seen: Vec<i64> = items.iter().map(|item| item.1).collect();
            seen.sort();
            expected.sort();
            assert_eq!(seen, expected);
        }
    }
}
