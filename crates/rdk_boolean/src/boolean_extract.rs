//! Port of lib/rdk/boolean/boolean_extract.ml: typed expression extraction from classified Weiler
//! cells. OCaml exceptions (`Invalid_argument`, `Cancel.Cancelled`, `Error.Rdk`) travel as the
//! private `Fail` enum and are mapped to the same typed errors the OCaml `try` blocks produce.
//! Ancestry planes that `View.ancestry_view` exposes are shared `Arc` slices.
use std::sync::Arc;

use rdk_core::packed;
use rdk_core::{cancel, error, geometry, topology, topology_index};
use rdk_core::error::Code;
use rdk_geom::implicit_point::{self, Construction};
use rdk_geom::predicates::{self, Sign};
use rdk_geom::surface_index;

use crate::boolean_cells as cells;
use crate::boolean_complex as complex;
use crate::boolean_constraints::{self as constraints, Side};
use crate::boolean_weiler as weiler;

const OPERATION: &str = "boolean_extract";

#[derive(Clone, Debug)]
pub enum Expression {
    Left,
    Right,
    Not(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Xor(Box<Expression>, Box<Expression>),
}

pub fn union() -> Expression {
    Expression::Or(Box::new(Expression::Left), Box::new(Expression::Right))
}

pub fn intersection() -> Expression {
    Expression::And(Box::new(Expression::Left), Box::new(Expression::Right))
}

pub fn difference() -> Expression {
    Expression::And(
        Box::new(Expression::Left),
        Box::new(Expression::Not(Box::new(Expression::Right))),
    )
}

pub fn reverse_difference() -> Expression {
    Expression::And(
        Box::new(Expression::Right),
        Box::new(Expression::Not(Box::new(Expression::Left))),
    )
}

pub fn xor() -> Expression {
    Expression::Xor(Box::new(Expression::Left), Box::new(Expression::Right))
}

fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

enum Fail {
    Cancelled,
    Invalid(String),
    Rdk(error::T),
}

impl From<error::T> for Fail {
    fn from(value: error::T) -> Fail {
        if error::is_cancelled(&value) { Fail::Cancelled } else { Fail::Rdk(value) }
    }
}

fn invalid_fail(message: &str) -> Fail {
    Fail::Invalid(message.to_owned())
}

fn finish_fail(failure: Fail) -> error::T {
    match failure {
        Fail::Cancelled => error::cancelled(OPERATION),
        Fail::Invalid(message) => make(Code::InvalidOutput, message),
        Fail::Rdk(failure) => failure,
    }
}

#[derive(Clone)]
pub struct Ancestry {
    complex: Arc<complex::T>,
    geometry: geometry::T,
    left_geometry: geometry::T,
    right_geometry: geometry::T,
    complex_output_points: Vec<i64>,
    corner_complex_vertices: Arc<[i64]>,
    primitive_complex_facets: Vec<i64>,
    primitive_sides: Arc<[u8]>,
    primitive_faces: Vec<i64>,
    primitive_triangles: Arc<[i64]>,
    primitive_refined_triangles: Vec<i64>,
    primitive_windings: Vec<u8>,
    primitive_source_points: Arc<[i64]>,
    primitive_source_vertices: Arc<[i64]>,
    barycentric_a: Arc<Vec<f64>>,
    barycentric_b: Arc<Vec<f64>>,
    barycentric_c: Arc<Vec<f64>>,
    edge_source_offsets: Arc<[i64]>,
    edge_source_sides: Arc<[u8]>,
    edge_source_edges: Arc<[i64]>,
}

fn mix(bits: u64) -> u64 {
    let bits = bits ^ (bits >> 30);
    let bits = bits.wrapping_mul(0x1e35a7bd);
    let bits = bits ^ (bits >> 27);
    let bits = bits.wrapping_mul(0x17a1465b);
    bits ^ (bits >> 31)
}

fn hash_float(value: f64) -> u64 {
    mix(if value == 0.0 { 0 } else { value.to_bits() })
}

fn hash_point(x: f64, y: f64, z: f64) -> u64 {
    hash_float(x) ^ hash_float(y).rotate_left(21) ^ hash_float(z).rotate_left(42)
}

fn table_capacity(points: usize, message: &str) -> Result<usize, String> {
    if points > (i64::MAX as usize) / 4 {
        return Err(message.to_owned());
    }
    let needed = std::cmp::max(2, points * 2);
    let mut capacity = 2;
    while capacity < needed {
        capacity *= 2;
    }
    Ok(capacity)
}

/// Merge output points with bit-identical binary64 coordinates (`-0` equals `0`). The planes are
/// compacted in place, in first-seen order. Returns `(x, y, z, complex_vertices, old_to_new)`.
#[allow(clippy::type_complexity)]
pub fn coalesce_positions(
    complex_vertices: &[i64],
    mut x: Vec<f64>,
    mut y: Vec<f64>,
    mut z: Vec<f64>,
) -> Result<(Vec<f64>, Vec<f64>, Vec<f64>, Vec<i64>, Vec<i64>), String> {
    let points = x.len();
    if y.len() != points || z.len() != points || complex_vertices.len() != points {
        return Err("Boolean rounded-point coalescing cardinality mismatch".to_owned());
    }
    let capacity = table_capacity(points, "Boolean rounded-point table exceeds array limits")?;
    let mut slots = vec![-1_i64; capacity];
    let mask = capacity - 1;
    let mut old_to_new = vec![-1_i64; points];
    let mut new_complex = vec![-1_i64; points];
    let mut count = 0_usize;
    for point in 0..points {
        let hash = hash_point(x[point], y[point], z[point]);
        let mut slot = (hash as usize) & mask;
        loop {
            let other = slots[slot];
            if other < 0 {
                slots[slot] = count as i64;
                old_to_new[point] = count as i64;
                x[count] = x[point];
                y[count] = y[point];
                z[count] = z[point];
                new_complex[count] = complex_vertices[point];
                count += 1;
                break;
            }
            let o = other as usize;
            if x[o] == x[point] && y[o] == y[point] && z[o] == z[point] {
                old_to_new[point] = other;
                break;
            }
            slot = (slot + 1) & mask;
        }
    }
    x.truncate(count);
    y.truncate(count);
    z.truncate(count);
    new_complex.truncate(count);
    Ok((x, y, z, new_complex, old_to_new))
}

pub fn geometry(value: &Ancestry) -> &geometry::T {
    &value.geometry
}

pub fn primitive_side(value: &Ancestry, primitive: i64) -> Side {
    if value.primitive_sides[primitive as usize] == 0 { Side::Left } else { Side::Right }
}

pub fn primitive_face(value: &Ancestry, primitive: i64) -> i64 {
    value.primitive_faces[primitive as usize]
}

pub fn primitive_refined_triangle(value: &Ancestry, primitive: i64) -> i64 {
    value.primitive_refined_triangles[primitive as usize]
}

pub fn primitive_winding(value: &Ancestry, primitive: i64) -> i64 {
    if value.primitive_windings[primitive as usize] == 0 { 1 } else { -1 }
}

/// Dense stable component IDs for materialized output primitives.
pub fn piece_ids(
    weiler_value: &weiler::T,
    ancestry: &Ancestry,
    source: &dyn Fn(i64) -> i64,
    primitive_count: i64,
) -> Result<Vec<i64>, error::T> {
    let c = &ancestry.complex;
    let mut shell_to_piece = vec![-1_i64; weiler::shell_count(weiler_value) as usize];
    let mut next_piece = 0_i64;
    let mut output = Vec::with_capacity(primitive_count as usize);
    for primitive in 0..primitive_count {
        let source_primitive = source(primitive);
        let facet = ancestry.primitive_complex_facets[source_primitive as usize];
        let second = ancestry.corner_complex_vertices[(source_primitive * 3 + 1) as usize];
        let canonical_second = complex::facet_vertex(c, facet, 1);
        let reversed_second = complex::facet_vertex(c, facet, 2);
        let half = if second == canonical_second {
            weiler::Side::Negative
        } else if second == reversed_second {
            weiler::Side::Positive
        } else {
            return Err(make(
                Code::InvalidOutput,
                "Boolean output ancestry lost facet orientation".to_owned(),
            ));
        };
        let shell = weiler::half_facet_shell(weiler_value, weiler::half_facet(facet, half));
        let piece = shell_to_piece[shell as usize];
        if piece >= 0 {
            output.push(piece);
        } else {
            shell_to_piece[shell as usize] = next_piece;
            output.push(next_piece);
            next_piece += 1;
        }
    }
    Ok(output)
}

fn evaluate(expression: &Expression, left: bool, right: bool) -> bool {
    match expression {
        Expression::Left => left,
        Expression::Right => right,
        Expression::Not(value) => !evaluate(value, left, right),
        Expression::And(first, second) => {
            evaluate(first, left, right) && evaluate(second, left, right)
        }
        Expression::Or(first, second) => {
            evaluate(first, left, right) || evaluate(second, left, right)
        }
        Expression::Xor(first, second) => evaluate(first, left, right) != evaluate(second, left, right),
    }
}

fn edge_key(point_count: i64, first: i64, second: i64) -> Result<i64, Fail> {
    let (first, second) = if first < second { (first, second) } else { (second, first) };
    if first > (i64::MAX - second) / point_count {
        return Err(invalid_fail("Boolean output edge key exceeds integer range"));
    }
    Ok(first * point_count + second)
}

pub fn validate_positions(x: &[f64], y: &[f64], z: &[f64]) -> Result<(), error::T> {
    let points = x.len();
    if y.len() != points || z.len() != points {
        return Err(make(
            Code::RoundingCardinality,
            "materialized coordinate planes have different lengths".to_owned(),
        ));
    }
    let capacity = table_capacity(points, "Boolean materialization coordinate table exceeds array limits")
        .map_err(|message| make(Code::InvalidOutput, message))?;
    let mut slots = vec![-1_i64; capacity];
    let mask = capacity - 1;
    for point in 0..points {
        let hash = hash_point(x[point], y[point], z[point]);
        let mut slot = (hash as usize) & mask;
        loop {
            let other = slots[slot];
            if other < 0 {
                slots[slot] = point as i64;
                break;
            }
            let o = other as usize;
            if x[o] == x[point] && y[o] == y[point] && z[o] == z[point] {
                return Err(make(
                    Code::RoundingCollision,
                    format!(
                        "exact output vertices {other} and {point} round to the same binary64 coordinate"
                    ),
                ));
            }
            slot = (slot + 1) & mask;
        }
    }
    Ok(())
}

pub fn validate_materialized(
    allow_degenerate: bool,
    x: &[f64],
    y: &[f64],
    z: &[f64],
    vertex_points: &[i64],
) -> Result<(), error::T> {
    let points = x.len() as i64;
    if vertex_points.len() % 3 != 0 {
        return Err(make(
            Code::RoundingCardinality,
            "materialized triangle corners are not divisible by three".to_owned(),
        ));
    }
    validate_positions(x, y, z)?;
    let projected = |projection: i64, a: i64, b: i64, c: i64| -> Sign {
        match projection {
            0 => predicates::orient2d_packed(x, y, a, b, c),
            1 => predicates::orient2d_packed(y, z, a, b, c),
            _ => predicates::orient2d_packed(z, x, a, b, c),
        }
    };
    for triangle in 0..vertex_points.len() / 3 {
        let offset = triangle * 3;
        let a = vertex_points[offset];
        let b = vertex_points[offset + 1];
        let c = vertex_points[offset + 2];
        if a < 0 || a >= points || b < 0 || b >= points || c < 0 || c >= points {
            return Err(make(
                Code::RoundingCardinality,
                format!("materialized triangle {triangle} references an invalid point"),
            ));
        }
        let (au, bu, cu) = (a as usize, b as usize, c as usize);
        let abx = x[bu] - x[au];
        let aby = y[bu] - y[au];
        let abz = z[bu] - z[au];
        let acx = x[cu] - x[au];
        let acy = y[cu] - y[au];
        let acz = z[cu] - z[au];
        let nx = aby * acz - abz * acy;
        let ny = abz * acx - abx * acz;
        let nz = abx * acy - aby * acx;
        let (first, second, third) = if nx.is_finite() && ny.is_finite() && nz.is_finite() {
            let (ax, ay, az) = (nx.abs(), ny.abs(), nz.abs());
            if az >= ax && az >= ay {
                (0, 1, 2)
            } else if ax >= ay {
                (1, 2, 0)
            } else {
                (2, 0, 1)
            }
        } else {
            (0, 1, 2)
        };
        if !allow_degenerate
            && projected(first, a, b, c) == Sign::Zero
            && projected(second, a, b, c) == Sign::Zero
            && projected(third, a, b, c) == Sign::Zero
        {
            return Err(make(
                Code::RoundingDegenerate,
                format!("exact output triangle {triangle} becomes collinear in binary64"),
            ));
        }
    }
    Ok(())
}

pub fn validate_closed_topology(topology_value: &topology::T) -> Result<(), error::T> {
    validate_closed_inner(topology_value).map_err(|f| finish_fail(f))
}

fn validate_closed_inner(topology_value: &topology::T) -> Result<(), Fail> {
    let view = topology::View::view(topology_value)?;
    let primitive_count = view.primitive_kinds.len();
    let mut triangle_only = true;
    for primitive in 0..primitive_count {
        if !(view.primitive_kinds[primitive] == 0
            && view.primitive_offsets[primitive + 1] - view.primitive_offsets[primitive] == 3)
        {
            triangle_only = false;
            break;
        }
    }
    if !triangle_only {
        return Err(Fail::Rdk(make(Code::InvalidOutput, "solid Boolean output is not triangle-only".to_owned())));
    }
    let edge_count = primitive_count * 3;
    let mut keys = vec![0_i64; edge_count];
    let point_count = view.point_count;
    for triangle in 0..primitive_count {
        let offset = triangle * 3;
        let a = view.vertex_points[offset];
        let b = view.vertex_points[offset + 1];
        let c = view.vertex_points[offset + 2];
        keys[offset] = edge_key(point_count, a, b)?;
        keys[offset + 1] = edge_key(point_count, b, c)?;
        keys[offset + 2] = edge_key(point_count, c, a)?;
    }
    keys.sort_unstable();
    let mut first = 0;
    while first < edge_count {
        let mut last = first + 1;
        while last < edge_count && keys[last] == keys[first] {
            last += 1;
        }
        let incidence = last - first;
        if incidence < 2 || incidence & 1 != 0 {
            return Err(Fail::Rdk(make(
                Code::OpenOutput,
                format!(
                    "solid Boolean output edge key {} has invalid incidence {}",
                    keys[first], incidence
                ),
            )));
        }
        first = last;
    }
    Ok(())
}

fn native_edge(view: &topology_index::View::View, primitive: i64, first: i64, second: i64) -> i64 {
    let (f, s) = (first as usize, second as usize);
    if view.primitive_of_vertex[f] == primitive && view.next_vertex[f] == second {
        view.edge_of_vertex[f]
    } else if view.primitive_of_vertex[s] == primitive && view.next_vertex[s] == first {
        view.edge_of_vertex[s]
    } else {
        -1
    }
}

fn explicit_id(point: &implicit_point::T) -> i64 {
    match implicit_point::construction(point) {
        Construction::Explicit(id) => *id,
        _ => -1,
    }
}

fn same_edge(first: i64, second: i64, a: i64, b: i64) -> bool {
    (first == a && second == b) || (first == b && second == a)
}

/// `edges`/`pairs` are the triangle's three native edges and their `(b,c)`, `(c,a)`, `(a,b)`
/// source-point pairs.
fn explicit_source_edge(first_id: i64, second_id: i64, edges: [i64; 3], pairs: [(i64, i64); 3]) -> i64 {
    if first_id >= 0 && second_id >= 0 {
        for index in 0..3 {
            if edges[index] >= 0 && same_edge(first_id, second_id, pairs[index].0, pairs[index].1) {
                return edges[index];
            }
        }
    }
    -1
}

pub struct BarycentricCache {
    triangle_keys: Vec<i64>,
    point_keys: Vec<i64>,
    weight_a: Vec<f64>,
    weight_b: Vec<f64>,
    weight_c: Vec<f64>,
    mask: usize,
}

fn barycentric_cache_create(capacity: i64) -> BarycentricCache {
    let needed = std::cmp::max(2, capacity.max(0) as usize * 2);
    let mut size = 2;
    while size < needed {
        size *= 2;
    }
    BarycentricCache {
        triangle_keys: vec![-1; size],
        point_keys: vec![-1; size],
        weight_a: vec![0.0; size],
        weight_b: vec![0.0; size],
        weight_c: vec![0.0; size],
        mask: size - 1,
    }
}

#[allow(clippy::too_many_arguments)]
fn cached_barycentric(
    cache: &mut BarycentricCache,
    source: &implicit_point::Source,
    triangle: i64,
    point: i64,
    a: i64,
    b: i64,
    c: i64,
    implicit: &implicit_point::T,
) -> (f64, f64, f64) {
    let value = (triangle.wrapping_mul(0x1e35a7bd)) ^ (point.wrapping_mul(0x17a1465b));
    let mut slot = ((value ^ ((value as u64) >> 16) as i64) as usize) & cache.mask;
    loop {
        let stored = cache.triangle_keys[slot];
        if stored < 0 {
            let (wa, wb, wc) = implicit_point::barycentric_source_triangle(source, a, b, c, implicit);
            cache.triangle_keys[slot] = triangle;
            cache.point_keys[slot] = point;
            cache.weight_a[slot] = wa;
            cache.weight_b[slot] = wb;
            cache.weight_c[slot] = wc;
            return (wa, wb, wc);
        }
        if stored == triangle && cache.point_keys[slot] == point {
            return (cache.weight_a[slot], cache.weight_b[slot], cache.weight_c[slot]);
        }
        slot = (slot + 1) & cache.mask;
    }
}

/// OCaml's optional arguments of `build_with_ancestry`.
#[derive(Clone, Copy)]
pub struct Options {
    pub require_closed: bool,
    pub defer_rounded_slivers: bool,
    pub corner_payload: bool,
}

pub fn options() -> Options {
    Options { require_closed: true, defer_rounded_slivers: false, corner_payload: true }
}

struct Internal {
    require_closed: bool,
    defer_rounded_slivers: bool,
    preferred_side: Option<Side>,
    with_corner_payload: bool,
}

fn side_triangle_point(c: &constraints::T, side: Side, triangle: i64, local: i64) -> i64 {
    match side {
        Side::Left => constraints::View::left_triangle_point(c, triangle, local),
        Side::Right => constraints::View::right_triangle_point(c, triangle, local),
    }
}

fn side_surface(c: &constraints::T, side: Side) -> &surface_index::T {
    match side {
        Side::Left => constraints::View::left_surface(c),
        Side::Right => constraints::View::right_surface(c),
    }
}

fn build_internal(
    cancel_value: Option<&cancel::T>,
    facet_selection: Option<&[u8]>,
    internal: &Internal,
    expression: &Expression,
    c: &Arc<complex::T>,
    weiler_value: &Arc<weiler::T>,
    cells_value: &cells::T,
    shared_barycentrics: Option<&mut BarycentricCache>,
) -> Result<Ancestry, Fail> {
    cancel::check_opt(cancel_value)?;
    if !Arc::ptr_eq(weiler::View::complex(weiler_value), c) {
        return Err(invalid_fail("Weiler graph belongs to a different Boolean complex"));
    }
    if !Arc::ptr_eq(cells::View::weiler(cells_value), weiler_value) {
        return Err(invalid_fail("cell labels belong to a different Weiler graph"));
    }
    if cells::shell_count(cells_value) != weiler::shell_count(weiler_value) {
        return Err(invalid_fail("cell labels do not match the Weiler shell cardinality"));
    }
    let mut table = [false; 4];
    for state in 0..4_usize {
        table[state] = evaluate(expression, state & 2 != 0, state & 1 != 0);
    }
    let inside = |shell: i64| -> bool {
        let state = (if cells::left_winding(cells_value, shell) != 0 { 2 } else { 0 })
            + (if cells::right_winding(cells_value, shell) != 0 { 1 } else { 0 });
        table[state]
    };
    let facets = complex::facet_count(c);
    if let Some(values) = facet_selection {
        if values.len() as i64 != facets {
            return Err(invalid_fail("Boolean facet selection cardinality mismatch"));
        }
        for &value in values {
            if value != 0 && value != 1 && value != 2 {
                return Err(invalid_fail("Boolean facet selection contains an invalid orientation"));
            }
        }
    }
    let mut selected = vec![0_u8; facets as usize];
    let mut reversed = vec![0_u8; facets as usize];
    let mut selected_count = 0_i64;
    for facet in 0..facets {
        cancel::poll(cancel_value, facet, 4095)?;
        match facet_selection {
            Some(values) => {
                let choice = values[facet as usize];
                if choice != 0 {
                    selected[facet as usize] = 1;
                    if choice == 2 {
                        reversed[facet as usize] = 1;
                    }
                    selected_count += 1;
                }
            }
            None => {
                let negative = weiler::half_facet_shell(
                    weiler_value,
                    weiler::half_facet(facet, weiler::Side::Negative),
                );
                let positive = weiler::half_facet_shell(
                    weiler_value,
                    weiler::half_facet(facet, weiler::Side::Positive),
                );
                let negative_inside = inside(negative);
                let positive_inside = inside(positive);
                if negative_inside != positive_inside {
                    selected[facet as usize] = 1;
                    if positive_inside {
                        reversed[facet as usize] = 1;
                    }
                    selected_count += 1;
                }
            }
        }
    }
    let vertex_count = complex::vertex_count(c);
    let mut used = vec![0_u8; vertex_count as usize];
    for facet in 0..facets {
        if selected[facet as usize] != 0 {
            for local in 0..3 {
                used[complex::facet_vertex(c, facet, local) as usize] = 1;
            }
        }
    }
    let mut point_map = vec![-1_i64; vertex_count as usize];
    let mut point_count = 0_i64;
    for point in 0..vertex_count as usize {
        if used[point] != 0 {
            point_map[point] = point_count;
            point_count += 1;
        }
    }
    let n = point_count as usize;
    let mut x = vec![0.0_f64; n];
    let mut y = vec![0.0_f64; n];
    let mut z = vec![0.0_f64; n];
    let mut materialized_complex_vertices = vec![-1_i64; n];
    for point in 0..vertex_count {
        let output = point_map[point as usize];
        if output >= 0 {
            materialized_complex_vertices[output as usize] = point;
            let (px, py, pz) = complex::approximate_vertex(c, point);
            x[output as usize] = px;
            y[output as usize] = py;
            z[output as usize] = pz;
        }
    }
    let (x, y, z, _materialized, old_to_new) =
        coalesce_positions(&materialized_complex_vertices, x, y, z).map_err(Fail::Invalid)?;
    if let Err(failure) = validate_positions(&x, &y, &z) {
        return Err(Fail::Invalid(format!(
            "Boolean rounded-point coalescing invariant failed: {}",
            error::to_string(&failure)
        )));
    }
    for point in 0..point_map.len() {
        let output = point_map[point];
        if output >= 0 {
            point_map[point] = old_to_new[output as usize];
        }
    }
    let point_count = x.len() as i64;
    let complex_output_points = point_map.clone();
    let constraints_value = complex::View::constraints(c);
    let left_geometry = constraints::View::left_geometry(constraints_value).clone();
    let right_geometry = constraints::View::right_geometry(constraints_value).clone();
    let edge_index = |geometry_value: &geometry::T| -> Result<Option<topology_index::View::View>, Fail> {
        if internal.with_corner_payload && !geometry::edge_groups(geometry_value).is_empty() {
            let index = topology_index::create(cancel_value, geometry::topology(geometry_value))?;
            Ok(Some(topology_index::View::view(&index)))
        } else {
            Ok(None)
        }
    };
    let left_edge_index = edge_index(&left_geometry)?;
    let right_edge_index = edge_index(&right_geometry)?;
    let has_edge_sources = left_edge_index.is_some() || right_edge_index.is_some();
    let mut selected_members = 0_i64;
    let mut maximum_facet_members = 0_i64;
    if has_edge_sources {
        for facet in 0..facets {
            if selected[facet as usize] != 0 {
                let (first, last) = complex::facet_member_range(c, facet);
                let count = last - first;
                selected_members += count;
                maximum_facet_members = maximum_facet_members.max(count);
            }
        }
    }
    let edge_source_capacity = (if has_edge_sources { selected_members * 3 } else { 0 }) as usize;
    let sc = selected_count as usize;
    let mut vertex_points = vec![0_i64; sc * 3];
    let mut corner_complex_vertices = vec![-1_i64; sc * 3];
    let mut primitive_complex_facets = vec![-1_i64; sc];
    let mut primitive_sides = vec![0_u8; sc];
    let mut primitive_faces = vec![0_i64; sc];
    let mut primitive_triangles = vec![-1_i64; sc];
    let mut primitive_refined_triangles = vec![-1_i64; sc];
    let mut primitive_windings = vec![0_u8; sc];
    let mut primitive_source_points = vec![0_i64; sc * 3];
    let mut primitive_source_vertices = vec![0_i64; sc * 3];
    let mut barycentric_a = vec![0.0_f64; sc * 3];
    let mut barycentric_b = vec![0.0_f64; sc * 3];
    let mut barycentric_c = vec![0.0_f64; sc * 3];
    let mut edge_source_offsets = vec![0_i64; sc * 3 + 1];
    let mut edge_source_sides = vec![0_u8; edge_source_capacity];
    let mut edge_source_edges = vec![-1_i64; edge_source_capacity];
    let mut edge_source_count = 0_usize;
    let mut edge_source_scratch = vec![-1_i64; maximum_facet_members as usize * 3];
    let mut edge_side_scratch = vec![0_u8; maximum_facet_members as usize];
    let mut output = 0_usize;
    let mut local_barycentrics;
    let barycentrics: &mut BarycentricCache = match shared_barycentrics {
        Some(cache) => cache,
        None => {
            local_barycentrics = barycentric_cache_create(if internal.with_corner_payload {
                selected_count * 3
            } else {
                0
            });
            &mut local_barycentrics
        }
    };
    let source = constraints::View::source(constraints_value);
    for facet in 0..facets {
        cancel::poll(cancel_value, facet, 4095)?;
        if selected[facet as usize] == 0 {
            continue;
        }
        let a = point_map[complex::facet_vertex(c, facet, 0) as usize];
        let b = point_map[complex::facet_vertex(c, facet, 1) as usize];
        let cc = point_map[complex::facet_vertex(c, facet, 2) as usize];
        let offset = output * 3;
        vertex_points[offset] = a;
        let is_reversed = reversed[facet as usize] != 0;
        if !is_reversed {
            vertex_points[offset + 1] = b;
            vertex_points[offset + 2] = cc;
            corner_complex_vertices[offset] = complex::facet_vertex(c, facet, 0);
            corner_complex_vertices[offset + 1] = complex::facet_vertex(c, facet, 1);
            corner_complex_vertices[offset + 2] = complex::facet_vertex(c, facet, 2);
        } else {
            vertex_points[offset + 1] = cc;
            vertex_points[offset + 2] = b;
            corner_complex_vertices[offset] = complex::facet_vertex(c, facet, 0);
            corner_complex_vertices[offset + 1] = complex::facet_vertex(c, facet, 2);
            corner_complex_vertices[offset + 2] = complex::facet_vertex(c, facet, 1);
        }
        primitive_complex_facets[output] = facet;
        let (first_member, last_member) = complex::facet_member_range(c, facet);
        if first_member == last_member {
            return Err(invalid_fail("Boolean output facet has no source member"));
        }
        let desired: i64 = if !is_reversed { 1 } else { -1 };
        let side_matches = |member: i64| -> bool {
            match internal.preferred_side {
                None => true,
                Some(side) => complex::member_side(c, member) == side,
            }
        };
        let mut preferred = -1_i64;
        let mut member = first_member;
        while preferred < 0 && member < last_member {
            if side_matches(member) && complex::member_winding(c, member) == desired {
                preferred = member;
            }
            member += 1;
        }
        if preferred < 0 {
            member = first_member;
            while preferred < 0 && member < last_member {
                if side_matches(member) {
                    preferred = member;
                }
                member += 1;
            }
        }
        if preferred < 0 {
            return Err(invalid_fail("selected Boolean facet has no member on its requested side"));
        }
        let member = preferred;
        let member_side = complex::member_side(c, member);
        if member_side == Side::Right {
            primitive_sides[output] = 1;
        }
        let source_triangle = complex::member_face(c, member);
        let surface = side_surface(constraints_value, member_side);
        primitive_faces[output] = surface_index::View::triangle_primitive(surface, source_triangle);
        primitive_triangles[output] = source_triangle;
        primitive_refined_triangles[output] = complex::member_triangle(c, member);
        let source_a = side_triangle_point(constraints_value, member_side, source_triangle, 0);
        let source_b = side_triangle_point(constraints_value, member_side, source_triangle, 1);
        let source_c = side_triangle_point(constraints_value, member_side, source_triangle, 2);
        let triangle_key = source_triangle
            + match member_side {
                Side::Left => 0,
                Side::Right => constraints::left_triangle_count(constraints_value),
            };
        if internal.with_corner_payload {
            for local in 0..3_i64 {
                primitive_source_points[output * 3 + local as usize] =
                    surface_index::View::triangle_point(surface, source_triangle, local);
                primitive_source_vertices[output * 3 + local as usize] =
                    surface_index::View::triangle_vertex(surface, source_triangle, local);
            }
            for local in 0..3_i64 {
                let facet_local = if local == 0 || desired > 0 { local } else { 3 - local };
                let complex_vertex = complex::facet_vertex(c, facet, facet_local);
                let point = complex::View::vertex(c, complex_vertex);
                let (wa, wb, wc) = cached_barycentric(
                    barycentrics,
                    source,
                    triangle_key,
                    complex_vertex,
                    source_a,
                    source_b,
                    source_c,
                    &point,
                );
                let corner = output * 3 + local as usize;
                barycentric_a[corner] = wa;
                barycentric_b[corner] = wb;
                barycentric_c[corner] = wc;
            }
        }
        if edge_source_capacity > 0 {
            let facet_local = |value: i64| -> i64 { if value == 0 || desired > 0 { value } else { 3 - value } };
            let output_point =
                |local: i64| complex::View::vertex(c, complex::facet_vertex(c, facet, facet_local(local)));
            let output0 = output_point(0);
            let output1 = output_point(1);
            let output2 = output_point(2);
            let output0_id = explicit_id(&output0);
            let output1_id = explicit_id(&output1);
            let output2_id = explicit_id(&output2);
            let mut relative = 0_usize;
            let mut candidate = first_member;
            while candidate < last_member {
                let side = complex::member_side(c, candidate);
                edge_side_scratch[relative] = if side == Side::Left { 0 } else { 1 };
                let index = match side {
                    Side::Left => left_edge_index.as_ref(),
                    Side::Right => right_edge_index.as_ref(),
                };
                match index {
                    None => {
                        edge_source_scratch[relative * 3] = -1;
                        edge_source_scratch[relative * 3 + 1] = -1;
                        edge_source_scratch[relative * 3 + 2] = -1;
                    }
                    Some(index) => {
                        let triangle = complex::member_face(c, candidate);
                        let candidate_surface = side_surface(constraints_value, side);
                        let primitive =
                            surface_index::View::triangle_primitive(candidate_surface, triangle);
                        let a_id = side_triangle_point(constraints_value, side, triangle, 0);
                        let b_id = side_triangle_point(constraints_value, side, triangle, 1);
                        let c_id = side_triangle_point(constraints_value, side, triangle, 2);
                        let va = surface_index::View::triangle_vertex(candidate_surface, triangle, 0);
                        let vb = surface_index::View::triangle_vertex(candidate_surface, triangle, 1);
                        let vc = surface_index::View::triangle_vertex(candidate_surface, triangle, 2);
                        let edge0 = native_edge(index, primitive, vb, vc);
                        let edge1 = native_edge(index, primitive, vc, va);
                        let edge2 = native_edge(index, primitive, va, vb);
                        let base = relative * 3;
                        let edges = [edge0, edge1, edge2];
                        let pairs = [(b_id, c_id), (c_id, a_id), (a_id, b_id)];
                        let direct0 = explicit_source_edge(output0_id, output1_id, edges, pairs);
                        let direct1 = explicit_source_edge(output1_id, output2_id, edges, pairs);
                        let direct2 = explicit_source_edge(output2_id, output0_id, edges, pairs);
                        if direct0 >= 0 && direct1 >= 0 && direct2 >= 0 {
                            edge_source_scratch[base] = direct0;
                            edge_source_scratch[base + 1] = direct1;
                            edge_source_scratch[base + 2] = direct2;
                        } else {
                            let projection =
                                implicit_point::source_triangle_projection(source, a_id, b_id, c_id);
                            let resolve = |direct: i64,
                                           first: &implicit_point::T,
                                           second: &implicit_point::T|
                             -> i64 {
                                if direct >= 0 {
                                    return direct;
                                }
                                let contains = |edge: i64, source_first: i64, source_second: i64| -> bool {
                                    edge >= 0
                                        && implicit_point::source_segment_contains(
                                            source, projection, source_first, source_second, first,
                                        )
                                        && implicit_point::source_segment_contains(
                                            source, projection, source_first, source_second, second,
                                        )
                                };
                                if contains(edge0, b_id, c_id) {
                                    edge0
                                } else if contains(edge1, c_id, a_id) {
                                    edge1
                                } else if contains(edge2, a_id, b_id) {
                                    edge2
                                } else {
                                    -1
                                }
                            };
                            edge_source_scratch[base] = resolve(direct0, &output0, &output1);
                            edge_source_scratch[base + 1] = resolve(direct1, &output1, &output2);
                            edge_source_scratch[base + 2] = resolve(direct2, &output2, &output0);
                        }
                    }
                }
                candidate += 1;
                relative += 1;
            }
            let member_count = (last_member - first_member) as usize;
            for local in 0..3_usize {
                let corner = output * 3 + local;
                edge_source_offsets[corner] = edge_source_count as i64;
                for member_index in 0..member_count {
                    let edge = edge_source_scratch[member_index * 3 + local];
                    if edge >= 0 {
                        if edge_source_count >= edge_source_capacity {
                            return Err(invalid_fail("Boolean edge ancestry exceeded its proven capacity"));
                        }
                        edge_source_sides[edge_source_count] = edge_side_scratch[member_index];
                        edge_source_edges[edge_source_count] = edge;
                        edge_source_count += 1;
                    }
                }
            }
        } else {
            for local in 0..3 {
                edge_source_offsets[output * 3 + local] = edge_source_count as i64;
            }
        }
        let relative_winding = desired * complex::member_winding(c, member);
        if relative_winding < 0 {
            primitive_windings[output] = 1;
        }
        output += 1;
    }
    edge_source_offsets[sc * 3] = edge_source_count as i64;
    if let Err(failure) =
        validate_materialized(internal.defer_rounded_slivers, &x, &y, &z, &vertex_points)
    {
        return Err(Fail::Rdk(make(
            error::kind(&failure),
            format!("post-coalescing extraction validation failed: {}", error::to_string(&failure)),
        )));
    }
    let primitive_offsets: Vec<i64> = (0..=selected_count).map(|triangle| triangle * 3).collect();
    let topology_value =
        match topology::polygons_owned(point_count, vertex_points, primitive_offsets) {
            Ok(value) => value,
            Err(failure) => {
                return Err(Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())));
            }
        };
    if internal.require_closed {
        validate_closed_topology(&topology_value).map_err(Fail::Rdk)?;
    }
    let positions = match packed::Float3::View::of_owned_exn(x, y, z) {
        Ok(value) => value,
        Err(failure) => return Err(Fail::Invalid(error::message(&failure).to_owned())),
    };
    match geometry::create(positions, topology_value, None, None, None) {
        Ok(geometry_value) => Ok(Ancestry {
            complex: Arc::clone(c),
            geometry: geometry_value,
            left_geometry,
            right_geometry,
            complex_output_points,
            corner_complex_vertices: corner_complex_vertices.into(),
            primitive_complex_facets,
            primitive_sides: primitive_sides.into(),
            primitive_faces,
            primitive_triangles: primitive_triangles.into(),
            primitive_refined_triangles,
            primitive_windings,
            primitive_source_points: primitive_source_points.into(),
            primitive_source_vertices: primitive_source_vertices.into(),
            barycentric_a: barycentric_a.into(),
            barycentric_b: barycentric_b.into(),
            barycentric_c: barycentric_c.into(),
            edge_source_offsets: edge_source_offsets.into(),
            edge_source_sides: edge_source_sides.into(),
            edge_source_edges: edge_source_edges.into(),
        }),
        Err(failure) => Err(Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned()))),
    }
}

pub fn build_with_ancestry(
    cancel_value: Option<&cancel::T>,
    options: &Options,
    expression: &Expression,
    c: &Arc<complex::T>,
    weiler_value: &Arc<weiler::T>,
    cells_value: &cells::T,
) -> Result<Ancestry, error::T> {
    let internal = Internal {
        require_closed: options.require_closed,
        defer_rounded_slivers: options.defer_rounded_slivers,
        preferred_side: None,
        with_corner_payload: options.corner_payload,
    };
    build_internal(cancel_value, None, &internal, expression, c, weiler_value, cells_value, None).map_err(|f| finish_fail(f))
}

/// Materialize the exact facets selected by packed orientation bytes: zero omits, one keeps
/// canonical orientation, and two reverses it. The requested side supplies payload ancestry.
#[allow(clippy::too_many_arguments)]
pub fn build_selected_with_ancestry(
    cancel_value: Option<&cancel::T>,
    defer_rounded_slivers: bool,
    corner_payload: bool,
    barycentric_cache: Option<&mut BarycentricCache>,
    selection: &[u8],
    side: Side,
    c: &Arc<complex::T>,
    weiler_value: &Arc<weiler::T>,
    cells_value: &cells::T,
) -> Result<Ancestry, error::T> {
    let internal = Internal {
        require_closed: false,
        defer_rounded_slivers,
        preferred_side: Some(side),
        with_corner_payload: corner_payload,
    };
    build_internal(cancel_value, Some(selection), &internal, &Expression::Left, c, weiler_value, cells_value, barycentric_cache).map_err(|f| finish_fail(f))
}

#[allow(non_snake_case)]
pub mod View {
    use super::*;

    pub use super::BarycentricCache;

    /// Shared planes of an ancestry (cloning shares the storage).
    #[derive(Clone)]
    pub struct AncestryView {
        pub corner_complex_vertices: Arc<[i64]>,
        pub primitive_sides: Arc<[u8]>,
        pub primitive_triangles: Arc<[i64]>,
        pub primitive_source_points: Arc<[i64]>,
        pub primitive_source_vertices: Arc<[i64]>,
        pub barycentric_a: Arc<Vec<f64>>,
        pub barycentric_b: Arc<Vec<f64>>,
        pub barycentric_c: Arc<Vec<f64>>,
        pub edge_source_offsets: Arc<[i64]>,
        pub edge_source_sides: Arc<[u8]>,
        pub edge_source_edges: Arc<[i64]>,
    }

    pub fn ancestry_view(value: &Ancestry) -> AncestryView {
        AncestryView {
            corner_complex_vertices: Arc::clone(&value.corner_complex_vertices),
            primitive_sides: Arc::clone(&value.primitive_sides),
            primitive_triangles: Arc::clone(&value.primitive_triangles),
            primitive_source_points: Arc::clone(&value.primitive_source_points),
            primitive_source_vertices: Arc::clone(&value.primitive_source_vertices),
            barycentric_a: Arc::clone(&value.barycentric_a),
            barycentric_b: Arc::clone(&value.barycentric_b),
            barycentric_c: Arc::clone(&value.barycentric_c),
            edge_source_offsets: Arc::clone(&value.edge_source_offsets),
            edge_source_sides: Arc::clone(&value.edge_source_sides),
            edge_source_edges: Arc::clone(&value.edge_source_edges),
        }
    }

    pub fn complex(value: &Ancestry) -> &Arc<complex::T> {
        &value.complex
    }

    /// Output point for a selected exact-complex vertex, or `-1`.
    pub fn complex_output_point(value: &Ancestry, point: i64) -> i64 {
        value.complex_output_points[point as usize]
    }

    pub fn primitive_complex_facet(value: &Ancestry, primitive: i64) -> i64 {
        value.primitive_complex_facets[primitive as usize]
    }

    pub fn left_geometry(value: &Ancestry) -> &geometry::T {
        &value.left_geometry
    }

    pub fn right_geometry(value: &Ancestry) -> &geometry::T {
        &value.right_geometry
    }

    pub fn validate_positions(x: &[f64], y: &[f64], z: &[f64]) -> Result<(), error::T> {
        super::validate_positions(x, y, z)
    }

    pub fn validate_materialized(
        allow_degenerate: bool,
        x: &[f64],
        y: &[f64],
        z: &[f64],
        vertex_points: &[i64],
    ) -> Result<(), error::T> {
        super::validate_materialized(allow_degenerate, x, y, z, vertex_points)
    }

    #[allow(clippy::type_complexity)]
    pub fn coalesce_positions(
        complex_vertices: &[i64],
        x: Vec<f64>,
        y: Vec<f64>,
        z: Vec<f64>,
    ) -> Result<(Vec<f64>, Vec<f64>, Vec<f64>, Vec<i64>, Vec<i64>), String> {
        super::coalesce_positions(complex_vertices, x, y, z)
    }

    pub fn validate_closed_topology(topology_value: &topology::T) -> Result<(), error::T> {
        super::validate_closed_topology(topology_value)
    }

    pub fn barycentric_cache(capacity: i64) -> BarycentricCache {
        super::barycentric_cache_create(capacity)
    }

    pub fn reverse_ancestry(
        cancel_value: Option<&cancel::T>,
        value: &Ancestry,
    ) -> Result<Ancestry, error::T> {
        super::reverse_ancestry(cancel_value, value)
    }

    pub fn concatenate_ancestries(
        cancel_value: Option<&cancel::T>,
        values: &[Ancestry],
    ) -> Result<Ancestry, error::T> {
        super::concatenate_ancestries(cancel_value, values)
    }
}

fn reverse_ancestry(cancel_value: Option<&cancel::T>, value: &Ancestry) -> Result<Ancestry, error::T> {
    reverse_inner(cancel_value, value).map_err(|f| finish_fail(f))
}

fn reverse_inner(cancel_value: Option<&cancel::T>, value: &Ancestry) -> Result<Ancestry, Fail> {
    cancel::check_opt(cancel_value)?;
    let primitives = geometry::primitive_count(&value.geometry) as usize;
    let corners = primitives * 3;
    let source_topology = topology::View::view(geometry::topology(&value.geometry))?;
    if source_topology.vertex_points.len() != corners {
        return Err(invalid_fail("Boolean ancestry reversal requires triangular topology"));
    }
    let mut vertex_points = vec![0_i64; corners];
    let mut corner_complex_vertices = vec![0_i64; corners];
    let mut primitive_windings = value.primitive_windings.clone();
    let mut barycentric_a = vec![0.0_f64; corners];
    let mut barycentric_b = vec![0.0_f64; corners];
    let mut barycentric_c = vec![0.0_f64; corners];
    let mut edge_source_offsets = vec![0_i64; corners + 1];
    let edge_source_count = value.edge_source_offsets[corners] as usize;
    let mut edge_source_sides = vec![0_u8; edge_source_count];
    let mut edge_source_edges = vec![0_i64; edge_source_count];
    let mut next_edge_source = 0_usize;
    let old_local = |local: usize| -> usize {
        match local {
            0 => 2,
            1 => 1,
            _ => 0,
        }
    };
    for primitive in 0..primitives {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        let offset = primitive * 3;
        vertex_points[offset] = source_topology.vertex_points[offset];
        vertex_points[offset + 1] = source_topology.vertex_points[offset + 2];
        vertex_points[offset + 2] = source_topology.vertex_points[offset + 1];
        corner_complex_vertices[offset] = value.corner_complex_vertices[offset];
        corner_complex_vertices[offset + 1] = value.corner_complex_vertices[offset + 2];
        corner_complex_vertices[offset + 2] = value.corner_complex_vertices[offset + 1];
        for (plane, from) in [
            (&mut barycentric_a, &value.barycentric_a),
            (&mut barycentric_b, &value.barycentric_b),
            (&mut barycentric_c, &value.barycentric_c),
        ] {
            plane[offset] = from[offset];
            plane[offset + 1] = from[offset + 2];
            plane[offset + 2] = from[offset + 1];
        }
        primitive_windings[primitive] = if primitive_windings[primitive] == 0 { 1 } else { 0 };
        for local in 0..3 {
            let target = offset + local;
            edge_source_offsets[target] = next_edge_source as i64;
            let source = offset + old_local(local);
            let first = value.edge_source_offsets[source] as usize;
            let last = value.edge_source_offsets[source + 1] as usize;
            let count = last - first;
            edge_source_sides[next_edge_source..next_edge_source + count]
                .copy_from_slice(&value.edge_source_sides[first..last]);
            edge_source_edges[next_edge_source..next_edge_source + count]
                .copy_from_slice(&value.edge_source_edges[first..last]);
            next_edge_source += count;
        }
    }
    edge_source_offsets[corners] = next_edge_source as i64;
    if next_edge_source != edge_source_count {
        return Err(invalid_fail("Boolean reversed edge ancestry changed cardinality"));
    }
    let primitive_offsets: Vec<i64> = (0..=primitives as i64).map(|primitive| primitive * 3).collect();
    let topology_value = topology::polygons_owned(
        geometry::point_count(&value.geometry),
        vertex_points,
        primitive_offsets,
    )
    .map_err(|failure| Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())))?;
    let geometry_value =
        geometry::create(geometry::positions(&value.geometry).clone(), topology_value, None, None, None)
            .map_err(|failure| Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())))?;
    let mut result = value.clone();
    result.geometry = geometry_value;
    result.corner_complex_vertices = corner_complex_vertices.into();
    result.primitive_windings = primitive_windings;
    result.barycentric_a = barycentric_a.into();
    result.barycentric_b = barycentric_b.into();
    result.barycentric_c = barycentric_c.into();
    result.edge_source_offsets = edge_source_offsets.into();
    result.edge_source_sides = edge_source_sides.into();
    result.edge_source_edges = edge_source_edges.into();
    Ok(result)
}

/// Concatenate products from one exact complex while sharing equal exact complex vertices and
/// preserving every packed ancestry plane.
fn concatenate_ancestries(
    cancel_value: Option<&cancel::T>,
    values: &[Ancestry],
) -> Result<Ancestry, error::T> {
    concatenate_inner(cancel_value, values).map_err(|f| finish_fail(f))
}

fn concatenate_inner(cancel_value: Option<&cancel::T>, values: &[Ancestry]) -> Result<Ancestry, Fail> {
    cancel::check_opt(cancel_value)?;
    if values.is_empty() {
        return Err(invalid_fail("Boolean ancestry concatenation requires at least one product"));
    }
    let first = &values[0];
    let c = &first.complex;
    let mut total_primitives = 0_usize;
    let mut total_edge_sources = 0_usize;
    for (index, value) in values.iter().enumerate() {
        if !Arc::ptr_eq(&value.complex, c) {
            return Err(invalid_fail("Boolean products belong to different exact complexes"));
        }
        total_primitives += geometry::primitive_count(&value.geometry) as usize;
        total_edge_sources += value.edge_source_offsets[value.edge_source_offsets.len() - 1] as usize;
        cancel::poll(cancel_value, index as i64, 255)?;
    }
    let vertex_count = complex::vertex_count(c) as usize;
    let mut used = vec![0_u8; vertex_count];
    for value in values {
        for (point, &output) in value.complex_output_points.iter().enumerate() {
            if output >= 0 {
                used[point] = 1;
            }
        }
    }
    let mut complex_to_output = vec![-1_i64; vertex_count];
    let mut point_count = 0_usize;
    for point in 0..vertex_count {
        if used[point] != 0 {
            complex_to_output[point] = point_count as i64;
            point_count += 1;
        }
    }
    let mut x = vec![0.0_f64; point_count];
    let mut y = vec![0.0_f64; point_count];
    let mut z = vec![0.0_f64; point_count];
    let mut materialized_complex_vertices = vec![0_i64; point_count];
    for point in 0..vertex_count {
        let output = complex_to_output[point];
        if output >= 0 {
            materialized_complex_vertices[output as usize] = point as i64;
            let (px, py, pz) = complex::approximate_vertex(c, point as i64);
            x[output as usize] = px;
            y[output as usize] = py;
            z[output as usize] = pz;
        }
    }
    let (x, y, z, _vertices, old_to_new) =
        coalesce_positions(&materialized_complex_vertices, x, y, z).map_err(Fail::Invalid)?;
    for point in 0..vertex_count {
        let output = complex_to_output[point];
        if output >= 0 {
            complex_to_output[point] = old_to_new[output as usize];
        }
    }
    let point_count = x.len() as i64;
    let complex_output_points = complex_to_output.clone();
    let corners = total_primitives * 3;
    let mut vertex_points = vec![0_i64; corners];
    let mut corner_complex_vertices = vec![0_i64; corners];
    let mut primitive_complex_facets = vec![0_i64; total_primitives];
    let mut primitive_sides = vec![0_u8; total_primitives];
    let mut primitive_faces = vec![0_i64; total_primitives];
    let mut primitive_triangles = vec![0_i64; total_primitives];
    let mut primitive_refined_triangles = vec![0_i64; total_primitives];
    let mut primitive_windings = vec![0_u8; total_primitives];
    let mut primitive_source_points = vec![0_i64; corners];
    let mut primitive_source_vertices = vec![0_i64; corners];
    let mut barycentric_a = vec![0.0_f64; corners];
    let mut barycentric_b = vec![0.0_f64; corners];
    let mut barycentric_c = vec![0.0_f64; corners];
    let mut edge_source_offsets = vec![0_i64; corners + 1];
    let mut edge_source_sides = vec![0_u8; total_edge_sources];
    let mut edge_source_edges = vec![0_i64; total_edge_sources];
    let mut primitive_base = 0_usize;
    let mut edge_source_base = 0_usize;
    for value in values {
        let primitives = geometry::primitive_count(&value.geometry) as usize;
        let corner_base = primitive_base * 3;
        let part_corners = primitives * 3;
        for corner in 0..part_corners {
            vertex_points[corner_base + corner] =
                complex_to_output[value.corner_complex_vertices[corner] as usize];
            corner_complex_vertices[corner_base + corner] = value.corner_complex_vertices[corner];
        }
        let end = primitive_base + primitives;
        primitive_complex_facets[primitive_base..end].copy_from_slice(&value.primitive_complex_facets);
        primitive_sides[primitive_base..end].copy_from_slice(&value.primitive_sides);
        primitive_faces[primitive_base..end].copy_from_slice(&value.primitive_faces);
        primitive_triangles[primitive_base..end].copy_from_slice(&value.primitive_triangles);
        primitive_refined_triangles[primitive_base..end]
            .copy_from_slice(&value.primitive_refined_triangles);
        primitive_windings[primitive_base..end].copy_from_slice(&value.primitive_windings);
        let corner_end = corner_base + part_corners;
        primitive_source_points[corner_base..corner_end].copy_from_slice(&value.primitive_source_points);
        primitive_source_vertices[corner_base..corner_end]
            .copy_from_slice(&value.primitive_source_vertices);
        barycentric_a[corner_base..corner_end].copy_from_slice(&value.barycentric_a);
        barycentric_b[corner_base..corner_end].copy_from_slice(&value.barycentric_b);
        barycentric_c[corner_base..corner_end].copy_from_slice(&value.barycentric_c);
        for corner in 0..part_corners {
            edge_source_offsets[corner_base + corner] =
                edge_source_base as i64 + value.edge_source_offsets[corner];
        }
        let source_count = value.edge_source_offsets[value.edge_source_offsets.len() - 1] as usize;
        edge_source_sides[edge_source_base..edge_source_base + source_count]
            .copy_from_slice(&value.edge_source_sides[..source_count]);
        edge_source_edges[edge_source_base..edge_source_base + source_count]
            .copy_from_slice(&value.edge_source_edges[..source_count]);
        primitive_base += primitives;
        edge_source_base += source_count;
        cancel::check_opt(cancel_value)?;
    }
    edge_source_offsets[corners] = total_edge_sources as i64;
    let primitive_offsets: Vec<i64> =
        (0..=total_primitives as i64).map(|primitive| primitive * 3).collect();
    let topology_value = topology::polygons_owned(point_count, vertex_points, primitive_offsets)
        .map_err(|failure| Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())))?;
    let positions = packed::Float3::View::of_owned_exn(x, y, z)
        .map_err(|failure| Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())))?;
    let geometry_value = geometry::create(positions, topology_value, None, None, None)
        .map_err(|failure| Fail::Rdk(make(Code::InvalidOutput, error::message(&failure).to_owned())))?;
    Ok(Ancestry {
        complex: Arc::clone(c),
        geometry: geometry_value,
        left_geometry: first.left_geometry.clone(),
        right_geometry: first.right_geometry.clone(),
        complex_output_points,
        corner_complex_vertices: corner_complex_vertices.into(),
        primitive_complex_facets,
        primitive_sides: primitive_sides.into(),
        primitive_faces,
        primitive_triangles: primitive_triangles.into(),
        primitive_refined_triangles,
        primitive_windings,
        primitive_source_points: primitive_source_points.into(),
        primitive_source_vertices: primitive_source_vertices.into(),
        barycentric_a: barycentric_a.into(),
        barycentric_b: barycentric_b.into(),
        barycentric_c: barycentric_c.into(),
        edge_source_offsets: edge_source_offsets.into(),
        edge_source_sides: edge_source_sides.into(),
        edge_source_edges: edge_source_edges.into(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // test_materialization_validation
    #[test]
    fn materialization_validation() {
        validate_materialized(false, &[0., 1., 0.], &[0., 0., 1.], &[0., 0., 0.], &[0, 1, 2])
            .expect("valid rounded triangle failed");
        match validate_materialized(false, &[0., 0.], &[0., 0.], &[0., 0.], &[]) {
            Err(failure) if error::kind(&failure) == Code::RoundingCollision => {}
            Err(failure) => panic!("unexpected rounding collision error: {}", error::to_string(&failure)),
            Ok(()) => panic!("rounded exact-point collision was accepted"),
        }
        match validate_materialized(false, &[0., 1., 2.], &[0., 0., 0.], &[0., 0., 0.], &[0, 1, 2]) {
            Err(failure) if error::kind(&failure) == Code::RoundingDegenerate => {}
            Err(failure) => panic!("unexpected rounding degeneracy error: {}", error::to_string(&failure)),
            Ok(()) => panic!("rounded collinear output triangle was accepted"),
        }
        validate_materialized(true, &[0., 1., 2.], &[0., 0., 0.], &[0., 0., 0.], &[0, 1, 2])
            .expect("explicit deferred rounding repair was rejected");
        let (x, y, z, representatives, map) = coalesce_positions(
            &[10, 11, 12, 13, 14],
            vec![0., 1., 1., 2., 1.],
            vec![0.; 5],
            vec![0.; 5],
        )
        .unwrap();
        assert!(
            x == vec![0., 1., 2.]
                && y == vec![0.; 3]
                && z == vec![0.; 3]
                && representatives == vec![10, 11, 13]
                && map == vec![0, 1, 1, 2, 1],
            "rounded-point coalescing lost stable representatives after duplicate gaps"
        );
    }

    #[test]
    fn closed_topology_and_expressions() {
        let tetra =
            topology::polygons_owned(4, vec![0, 2, 1, 0, 1, 3, 1, 2, 3, 2, 0, 3], vec![0, 3, 6, 9, 12]).unwrap();
        validate_closed_topology(&tetra).expect("closed tetrahedron rejected");
        let open = topology::polygons_owned(3, vec![0, 1, 2], vec![0, 3]).unwrap();
        assert_eq!(error::kind(&validate_closed_topology(&open).unwrap_err()), Code::OpenOutput);
        assert!(evaluate(&difference(), true, false) && !evaluate(&difference(), true, true));
        assert!(evaluate(&xor(), true, false) && !evaluate(&xor(), true, true));
        assert!(evaluate(&union(), false, true) && !evaluate(&intersection(), false, true));
        assert!(evaluate(&reverse_difference(), false, true));
    }
}
