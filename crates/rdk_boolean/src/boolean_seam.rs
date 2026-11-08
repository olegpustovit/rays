//! Port of lib/rdk/boolean/boolean_seam.ml: exact seam and coincident-surface product from a
//! Boolean complex. Classification and chaining are sequential (each edge writes only its own
//! slot, so the result is the parallel path's by construction); `grain` still feeds the exact
//! curve verification and `parallel_cutoff` is only validated.
use std::sync::Arc;

use rdk_core::packed;
use rdk_core::topology::PrimitiveKind;
use rdk_core::{cancel, error, geometry, support, topology, topology_index};
use rdk_core::error::Code;
use rdk_geom::implicit_point;
use rdk_geom::linear_piece_index;
use rdk_geom::predicates::{self, SegmentSegment};
use rdk_geom::surface_index;

use crate::boolean_complex as complex;
use crate::boolean_constraints::{self as constraints, Side};
use crate::boolean_extract::View as extract_view;

const OPERATION: &str = "boolean_seam";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    LeftSelf,
    Between,
    RightSelf,
}

pub struct T {
    complex: Arc<complex::T>,
    curves_materialized: bool,
    curves: geometry::T,
    coincident: geometry::T,
    edge_kinds: Vec<u8>,
    curve_kinds: Vec<u8>,
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

pub fn curves(value: &T) -> &geometry::T {
    &value.curves
}

pub fn coincident(value: &T) -> &geometry::T {
    &value.coincident
}

fn kind_code(kind: Kind) -> u8 {
    match kind {
        Kind::LeftSelf => 0,
        Kind::Between => 1,
        Kind::RightSelf => 2,
    }
}

pub fn curve_kind(value: &T, curve: i64) -> Kind {
    match value.curve_kinds[curve as usize] {
        0 => Kind::LeftSelf,
        1 => Kind::Between,
        2 => Kind::RightSelf,
        _ => unreachable!("Boolean seam kind is invalid"),
    }
}

fn side_bit(side: Side) -> i64 {
    match side {
        Side::Left => 1,
        Side::Right => 2,
    }
}

fn facet_mask(c: &complex::T, facet: i64) -> i64 {
    let (first, last) = complex::facet_member_range(c, facet);
    let mut mask = 0;
    for member in first..last {
        mask |= side_bit(complex::member_side(c, member));
    }
    mask | if last - first > 1 { 4 } else { 0 }
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

fn side_surface(c: &constraints::T, side: Side) -> &surface_index::T {
    match side {
        Side::Left => constraints::View::left_surface(c),
        Side::Right => constraints::View::right_surface(c),
    }
}

fn side_triangle_point(c: &constraints::T, side: Side, triangle: i64, local: i64) -> i64 {
    match side {
        Side::Left => constraints::View::left_triangle_point(c, triangle, local),
        Side::Right => constraints::View::right_triangle_point(c, triangle, local),
    }
}

fn source_native_edge(
    c: &constraints::T,
    index: &topology_index::View::View,
    side: Side,
    triangle: i64,
    first: &implicit_point::T,
    second: &implicit_point::T,
) -> i64 {
    let surface = side_surface(c, side);
    let source = constraints::View::source(c);
    let first_id = side_triangle_point(c, side, triangle, 0);
    let second_id = side_triangle_point(c, side, triangle, 1);
    let third_id = side_triangle_point(c, side, triangle, 2);
    let projection = implicit_point::source_triangle_projection(source, first_id, second_id, third_id);
    let primitive = surface_index::View::triangle_primitive(surface, triangle);
    for local in 0..3_i64 {
        let next = (local + 1) % 3;
        let source_first = side_triangle_point(c, side, triangle, local);
        let source_second = side_triangle_point(c, side, triangle, next);
        if implicit_point::source_segment_contains(source, projection, source_first, source_second, first)
            && implicit_point::source_segment_contains(source, projection, source_first, source_second, second)
        {
            return native_edge(
                index,
                primitive,
                surface_index::View::triangle_vertex(surface, triangle, local),
                surface_index::View::triangle_vertex(surface, triangle, next),
            );
        }
    }
    -1
}

fn is_self_edge(
    c: &complex::T,
    constraints_value: &constraints::T,
    index: &topology_index::View::View,
    side: Side,
    edge: i64,
) -> bool {
    let first_point = complex::View::vertex(c, complex::edge_first(c, edge));
    let second_point = complex::View::vertex(c, complex::edge_second(c, edge));
    let (first_incident, last_incident) = complex::edge_incident_range(c, edge);
    let mut common = -2_i64;
    let mut self_edge = false;
    for incident in first_incident..last_incident {
        let facet = complex::edge_incident_facet(c, incident);
        let (first, last) = complex::facet_member_range(c, facet);
        for member in first..last {
            if complex::member_side(c, member) == side {
                let source_edge = source_native_edge(
                    constraints_value,
                    index,
                    side,
                    complex::member_face(c, member),
                    &first_point,
                    &second_point,
                );
                if source_edge < 0 {
                    self_edge = true;
                } else if common == -2 {
                    common = source_edge;
                } else if common != source_edge {
                    self_edge = true;
                }
            }
        }
    }
    self_edge
}

/// Output coordinates of the used complex vertices after certified coalescing.
fn coordinates(
    c: &complex::T,
    used: &[u8],
) -> Result<(Vec<i64>, Vec<f64>, Vec<f64>, Vec<f64>), Fail> {
    let vertex_count = complex::vertex_count(c) as usize;
    let mut map = vec![-1_i64; vertex_count];
    let mut count = 0_usize;
    for point in 0..used.len() {
        if used[point] != 0 {
            map[point] = count as i64;
            count += 1;
        }
    }
    let mut x = vec![0.0_f64; count];
    let mut y = vec![0.0_f64; count];
    let mut z = vec![0.0_f64; count];
    let mut complex_vertices = vec![-1_i64; count];
    for point in 0..used.len() {
        let output = map[point];
        if output >= 0 {
            let (px, py, pz) = complex::approximate_vertex(c, point as i64);
            x[output as usize] = px;
            y[output as usize] = py;
            z[output as usize] = pz;
            complex_vertices[output as usize] = point as i64;
        }
    }
    let (x, y, z, _, old_to_new) =
        extract_view::coalesce_positions(&complex_vertices, x, y, z).map_err(Fail::Invalid)?;
    for point in 0..map.len() {
        let output = map[point];
        if output >= 0 {
            map[point] = old_to_new[output as usize];
        }
    }
    Ok((map, x, y, z))
}

fn make_geometry(
    x: Vec<f64>,
    y: Vec<f64>,
    z: Vec<f64>,
    topology_value: topology::T,
) -> Result<geometry::T, Fail> {
    let positions = packed::Float3::View::of_owned_exn(x, y, z)
        .map_err(|failure| Fail::Invalid(error::message(&failure).to_owned()))?;
    geometry::create(positions, topology_value, None, None, None)
        .map_err(|failure| Fail::Invalid(error::message(&failure).to_owned()))
}

fn empty_geometry() -> Result<geometry::T, Fail> {
    make_geometry(Vec::new(), Vec::new(), Vec::new(), topology::empty(0)?)
}

/// Exact post-rounding self-intersection verification over packed curve segments.
pub fn verify_curves(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    curves_value: &geometry::T,
) -> Result<(), error::T> {
    if grain <= 0 {
        return Err(make(Code::InvalidParameter, "grain must be positive".to_owned()));
    }
    let cancelled = || error::cancelled(OPERATION);
    let topology_value = geometry::topology(curves_value);
    for primitive in 0..topology::primitive_count(topology_value) {
        cancel::poll(cancel_value, primitive, 4095).map_err(|_| cancelled())?;
        let size = topology::primitive_size(topology_value, primitive)?;
        let message = match topology::primitive_kind(topology_value, primitive)? {
            PrimitiveKind::Polygon => {
                Some(format!("primitive {primitive} is a polygon, not a seam curve"))
            }
            PrimitiveKind::OpenPolyline if size < 2 => {
                Some(format!("open seam curve {primitive} has fewer than two points"))
            }
            PrimitiveKind::ClosedPolyline if size < 3 => {
                Some(format!("closed seam curve {primitive} has fewer than three points"))
            }
            _ => None,
        };
        if let Some(message) = message {
            return Err(make(Code::InvalidOutput, message));
        }
    }
    let index = match linear_piece_index::create(cancel_value, Some(grain), None, curves_value) {
        Ok(index) => index,
        Err(failure) if error::is_cancelled(&failure) => return Err(cancelled()),
        Err(failure) => {
            return Err(make(
                Code::InvalidOutput,
                format!("Boolean seam curves could not be indexed for verification ({})", error::to_string(&failure)),
            ));
        }
    };
    let positions = linear_piece_index::View::positions(&index);
    let invalid_pair = |left: i64, right: i64| -> bool {
        let point = |piece: i64, corner: i64| -> i64 {
            linear_piece_index::View::point(&index, piece, corner).expect("segment corner")
        };
        let (left_a, left_b) = (point(left, 0), point(left, 1));
        let (right_a, right_b) = (point(right, 0), point(right, 1));
        let shared = left_a == right_a || left_a == right_b || left_b == right_a || left_b == right_b;
        match predicates::segment_segment_packed(
            &positions.x,
            &positions.y,
            &positions.z,
            left_a,
            left_b,
            right_a,
            right_b,
        ) {
            SegmentSegment::Disjoint => false,
            SegmentSegment::Point => !shared,
            SegmentSegment::Overlap | SegmentSegment::Degenerate => true,
        }
    };
    let found = linear_piece_index::View::find_overlapping_self_pair(
        cancel_value,
        grain,
        0.0,
        &index,
        &invalid_pair,
    )
    .map_err(|failure| {
        if error::is_cancelled(&failure) { cancelled() } else { failure }
    })?;
    match found {
        None => Ok(()),
        Some((left, right)) => {
            let left_primitive = linear_piece_index::View::primitive(&index, left);
            let right_primitive = linear_piece_index::View::primitive(&index, right);
            let left_local = linear_piece_index::View::local(&index, left);
            let right_local = linear_piece_index::View::local(&index, right);
            Err(make(
                Code::SeamSelfIntersection,
                format!(
                    "rounded seam curve {left_primitive} segment {left_local} intersects curve {right_primitive} segment {right_local} (reduce the cleanup tolerance or inspect the reported curve segments)"
                ),
            ))
        }
    }
}

fn validate_curve_ancestry(
    cancel_value: Option<&cancel::T>,
    complex_edge_count: i64,
    selected: i64,
    curves_value: &geometry::T,
    curve_kinds: &[u8],
    curve_edge_offsets: &[i64],
    curve_edges: &[i64],
) -> Result<(), Fail> {
    let topology_value = geometry::topology(curves_value);
    let curve_count = geometry::primitive_count(curves_value);
    if curve_kinds.len() as i64 != curve_count
        || curve_edge_offsets.len() as i64 != curve_count + 1
        || curve_edges.len() as i64 != selected
        || curve_edge_offsets[0] != 0
        || curve_edge_offsets[curve_count as usize] != selected
    {
        return Err(invalid_fail("Boolean seam curve ancestry has inconsistent cardinality"));
    }
    let mut seen = vec![0_u8; complex_edge_count as usize];
    for curve in 0..curve_count {
        cancel::poll(cancel_value, curve, 4095)?;
        let first = curve_edge_offsets[curve as usize];
        let last = curve_edge_offsets[curve as usize + 1];
        if first > last {
            return Err(invalid_fail("Boolean seam curve ancestry offsets decrease"));
        }
        let size = topology::primitive_size(topology_value, curve)?;
        let expected = match topology::primitive_kind(topology_value, curve)? {
            PrimitiveKind::OpenPolyline => size - 1,
            PrimitiveKind::ClosedPolyline => size,
            PrimitiveKind::Polygon => return Err(invalid_fail("Boolean seam ancestry references a polygon")),
        };
        if last - first != expected {
            return Err(invalid_fail("Boolean seam curve incidence differs from edge ancestry"));
        }
        for slot in first..last {
            let edge = curve_edges[slot as usize];
            if edge < 0 || edge >= complex_edge_count {
                return Err(invalid_fail("Boolean seam ancestry edge is out of bounds"));
            }
            if seen[edge as usize] != 0 {
                return Err(invalid_fail("Boolean seam ancestry repeats a complex edge"));
            }
            seen[edge as usize] = 1;
        }
    }
    Ok(())
}

fn materialize_coincident(
    cancel_value: Option<&cancel::T>,
    c: &complex::T,
    facet_masks: &[i64],
) -> Result<geometry::T, Fail> {
    let selected = facet_masks.iter().filter(|&&mask| mask & 4 != 0).count();
    let mut used = vec![0_u8; complex::vertex_count(c) as usize];
    for facet in 0..complex::facet_count(c) {
        cancel::poll(cancel_value, facet, 4095)?;
        if facet_masks[facet as usize] & 4 != 0 {
            for local in 0..3 {
                used[complex::facet_vertex(c, facet, local) as usize] = 1;
            }
        }
    }
    let (map, x, y, z) = coordinates(c, &used)?;
    let mut vertices = vec![0_i64; selected * 3];
    let mut output = 0_usize;
    for facet in 0..complex::facet_count(c) {
        if facet_masks[facet as usize] & 4 != 0 {
            for local in 0..3_usize {
                vertices[output * 3 + local] = map[complex::facet_vertex(c, facet, local as i64) as usize];
            }
            output += 1;
        }
    }
    extract_view::validate_materialized(false, &x, &y, &z, &vertices).map_err(Fail::Rdk)?;
    let primitive_offsets: Vec<i64> = (0..=selected as i64).map(|index| index * 3).collect();
    let topology_value = topology::polygons_owned(x.len() as i64, vertices, primitive_offsets)?;
    make_geometry(x, y, z, topology_value)
}

struct Chaining {
    vertices: Vec<i64>,
    primitive_offsets: Vec<i64>,
    primitive_kinds: Vec<u8>,
    curve_kinds: Vec<u8>,
    curve_edge_offsets: Vec<i64>,
    curve_edges: Vec<i64>,
    visited: Vec<u8>,
    vertex_output: usize,
    curve_output: usize,
    edge_output: usize,
}

struct Curves {
    geometry: geometry::T,
    kinds: Vec<u8>,
    edge_offsets: Vec<i64>,
    edges: Vec<i64>,
    selected: i64,
}

fn materialize_curves(
    cancel_value: Option<&cancel::T>,
    c: &complex::T,
    edge_kinds: &[u8],
) -> Result<Curves, Fail> {
    let vertex_count = complex::vertex_count(c) as usize;
    let edge_count = complex::edge_count(c);
    let mut used = vec![0_u8; vertex_count];
    for edge in 0..edge_count {
        cancel::poll(cancel_value, edge, 4095)?;
        if edge_kinds[edge as usize] < 3 {
            used[complex::edge_first(c, edge) as usize] = 1;
            used[complex::edge_second(c, edge) as usize] = 1;
        }
    }
    let (map, x, y, z) = coordinates(c, &used)?;
    let mut degree = vec![0_i64; vertex_count * 3];
    let mut adjacency_counts = vec![0_i64; vertex_count];
    let mut materialized_selected = 0_usize;
    for edge in 0..edge_count {
        let code = edge_kinds[edge as usize] as usize;
        if code < 3 {
            let first = complex::edge_first(c, edge) as usize;
            let second = complex::edge_second(c, edge) as usize;
            if map[first] != map[second] {
                degree[first * 3 + code] += 1;
                degree[second * 3 + code] += 1;
                adjacency_counts[first] += 1;
                adjacency_counts[second] += 1;
                materialized_selected += 1;
            }
        }
    }
    let mut offsets = vec![0_i64; vertex_count + 1];
    for vertex in 0..vertex_count {
        offsets[vertex + 1] = offsets[vertex] + adjacency_counts[vertex];
    }
    let mut adjacency = vec![0_i64; materialized_selected * 2];
    let mut cursors = offsets.clone();
    for edge in 0..edge_count {
        if edge_kinds[edge as usize] < 3 {
            let first = complex::edge_first(c, edge) as usize;
            let second = complex::edge_second(c, edge) as usize;
            if map[first] != map[second] {
                adjacency[cursors[first] as usize] = edge;
                cursors[first] += 1;
                adjacency[cursors[second] as usize] = edge;
                cursors[second] += 1;
            }
        }
    }
    extract_view::validate_positions(&x, &y, &z).map_err(Fail::Rdk)?;
    let mut state = Chaining {
        vertices: vec![0; materialized_selected * 2],
        primitive_offsets: vec![0; materialized_selected + 1],
        primitive_kinds: vec![1; materialized_selected],
        curve_kinds: vec![0; materialized_selected],
        curve_edge_offsets: vec![0; materialized_selected + 1],
        curve_edges: vec![0; materialized_selected],
        visited: vec![0; edge_count as usize],
        vertex_output: 0,
        curve_output: 0,
        edge_output: 0,
    };
    let other = |edge: i64, vertex: i64| -> Result<i64, Fail> {
        let first = complex::edge_first(c, edge);
        let second = complex::edge_second(c, edge);
        if first == vertex {
            Ok(second)
        } else if second == vertex {
            Ok(first)
        } else {
            Err(invalid_fail("Boolean seam adjacency is inconsistent"))
        }
    };
    let next_edge = |state: &Chaining, vertex: i64, code: u8| -> i64 {
        let v = vertex as usize;
        for slot in offsets[v]..offsets[v + 1] {
            let edge = adjacency[slot as usize];
            if edge_kinds[edge as usize] == code && state.visited[edge as usize] == 0 {
                return edge;
            }
        }
        -1
    };
    let emit = |state: &mut Chaining, start: i64, first_edge: i64, code: u8| -> Result<(), Fail> {
        let curve = state.curve_output;
        state.primitive_offsets[curve] = state.vertex_output as i64;
        state.curve_edge_offsets[curve] = state.edge_output as i64;
        state.curve_kinds[curve] = code;
        state.vertices[state.vertex_output] = map[start as usize];
        state.vertex_output += 1;
        let mut current_vertex = start;
        let mut current_edge = first_edge;
        let mut closed = false;
        loop {
            if state.edge_output >= materialized_selected
                || state.vertex_output >= materialized_selected * 2
            {
                return Err(invalid_fail("Boolean seam chaining exceeded proven cardinality"));
            }
            state.visited[current_edge as usize] = 1;
            state.curve_edges[state.edge_output] = current_edge;
            state.edge_output += 1;
            let next = other(current_edge, current_vertex)?;
            if next == start {
                closed = true;
                break;
            }
            state.vertices[state.vertex_output] = map[next as usize];
            state.vertex_output += 1;
            if degree[next as usize * 3 + code as usize] != 2 {
                break;
            }
            let following = next_edge(state, next, code);
            if following < 0 {
                break;
            }
            current_vertex = next;
            current_edge = following;
        }
        if closed {
            state.primitive_kinds[curve] = 2;
        }
        state.curve_output += 1;
        Ok(())
    };
    for vertex in 0..vertex_count as i64 {
        for code in 0..3_u8 {
            let d = degree[vertex as usize * 3 + code as usize];
            if d != 0 && d != 2 {
                let mut edge = next_edge(&state, vertex, code);
                while edge >= 0 {
                    emit(&mut state, vertex, edge, code)?;
                    edge = next_edge(&state, vertex, code);
                }
            }
        }
    }
    for edge in 0..edge_count {
        let code = edge_kinds[edge as usize];
        let first = complex::edge_first(c, edge);
        let second = complex::edge_second(c, edge);
        if code < 3 && map[first as usize] != map[second as usize] && state.visited[edge as usize] == 0 {
            emit(&mut state, first.min(second), edge, code)?;
        }
    }
    state.primitive_offsets[state.curve_output] = state.vertex_output as i64;
    state.curve_edge_offsets[state.curve_output] = state.edge_output as i64;
    if state.edge_output != materialized_selected {
        return Err(invalid_fail("Boolean seam chaining did not consume every selected edge"));
    }
    let curve_output = state.curve_output;
    state.vertices.truncate(state.vertex_output);
    state.primitive_offsets.truncate(curve_output + 1);
    state.primitive_kinds.truncate(curve_output);
    state.curve_kinds.truncate(curve_output);
    state.curve_edge_offsets.truncate(curve_output + 1);
    let topology_value = topology::View::create_validated_owned(
        x.len() as i64,
        state.vertices,
        state.primitive_offsets,
        state.primitive_kinds,
    );
    Ok(Curves {
        geometry: make_geometry(x, y, z, topology_value)?,
        kinds: state.curve_kinds,
        edge_offsets: state.curve_edge_offsets,
        edges: state.curve_edges,
        selected: materialized_selected as i64,
    })
}

/// OCaml's optional arguments of `build`.
#[derive(Clone, Copy)]
pub struct Params {
    pub grain: i64,
    pub parallel_cutoff: i64,
    pub materialize: bool,
}

pub fn params() -> Params {
    Params { grain: 16_384, parallel_cutoff: 200_000, materialize: true }
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    options: &Params,
    c: &Arc<complex::T>,
) -> Result<T, error::T> {
    if options.grain <= 0 {
        return Err(make(Code::InvalidParameter, "grain must be positive".to_owned()));
    }
    if options.parallel_cutoff <= 0 {
        return Err(make(Code::InvalidParameter, "parallel_cutoff must be positive".to_owned()));
    }
    build_inner(cancel_value, options, c).map_err(|failure| match failure {
        Fail::Cancelled => error::cancelled(OPERATION),
        Fail::Invalid(message) => make(Code::InvalidOutput, message),
        Fail::Rdk(failure) => failure,
    })
}

fn build_inner(
    cancel_value: Option<&cancel::T>,
    options: &Params,
    c: &Arc<complex::T>,
) -> Result<T, Fail> {
    cancel::check_opt(cancel_value)?;
    let facets = complex::facet_count(c);
    let facet_masks: Vec<i64> = if facets < options.parallel_cutoff {
        (0..facets).map(|facet| facet_mask(c, facet)).collect()
    } else {
        let mut masks = vec![0_i64; facets as usize];
        support::par_fill(cancel_value, options.grain, &mut masks, &|facet| Ok(facet_mask(c, facet)))?;
        masks
    };
    let constraints_value = complex::View::constraints(c);
    let index_of = |geometry_value: &geometry::T| -> Result<topology_index::View::View, Fail> {
        let index = topology_index::create(cancel_value, geometry::topology(geometry_value))?;
        Ok(topology_index::View::view(&index))
    };
    let left_index = if constraints::View::resolve_left_self_intersections(constraints_value) {
        Some(index_of(constraints::View::left_geometry(constraints_value))?)
    } else {
        None
    };
    let right_index = if constraints::View::resolve_right_self_intersections(constraints_value) {
        Some(index_of(constraints::View::right_geometry(constraints_value))?)
    } else {
        None
    };
    let edge_count = complex::edge_count(c);
    let mut edge_kinds = vec![255_u8; edge_count as usize];
    for edge in 0..edge_count {
        cancel::poll(cancel_value, edge, 4095)?;
        let (first, last) = complex::edge_incident_range(c, edge);
        let mut mask = 0_i64;
        let mut has_unshared = false;
        for incident in first..last {
            let current = facet_masks[complex::edge_incident_facet(c, incident) as usize] & 3;
            mask |= current;
            if current != 3 {
                has_unshared = true;
            }
        }
        let kind = if mask == 3 && has_unshared {
            Some(Kind::Between)
        } else if mask == 1
            && last - first > 2
            && left_index
                .as_ref()
                .is_some_and(|index| is_self_edge(c, constraints_value, index, Side::Left, edge))
        {
            Some(Kind::LeftSelf)
        } else if mask == 2
            && last - first > 2
            && right_index
                .as_ref()
                .is_some_and(|index| is_self_edge(c, constraints_value, index, Side::Right, edge))
        {
            Some(Kind::RightSelf)
        } else {
            None
        };
        if let Some(kind) = kind {
            edge_kinds[edge as usize] = kind_code(kind);
        }
    }
    let (curves_value, curve_kinds, coincident_value) = if options.materialize {
        let curves = materialize_curves(cancel_value, c, &edge_kinds)?;
        validate_curve_ancestry(
            cancel_value,
            edge_count,
            curves.selected,
            &curves.geometry,
            &curves.kinds,
            &curves.edge_offsets,
            &curves.edges,
        )?;
        verify_curves(cancel_value, options.grain, &curves.geometry).map_err(Fail::Rdk)?;
        let coincident = materialize_coincident(cancel_value, c, &facet_masks)?;
        (curves.geometry, curves.kinds, coincident)
    } else {
        (empty_geometry()?, Vec::new(), empty_geometry()?)
    };
    Ok(T {
        complex: Arc::clone(c),
        curves_materialized: options.materialize,
        curves: curves_value,
        coincident: coincident_value,
        edge_kinds,
        curve_kinds,
    })
}

#[allow(non_snake_case)]
pub mod View {
    use super::*;

    pub fn complex(value: &T) -> &Arc<complex::T> {
        &value.complex
    }

    pub fn curves_materialized(value: &T) -> bool {
        value.curves_materialized
    }

    pub fn is_seam_edge(value: &T, edge: i64) -> bool {
        if edge < 0 || edge >= value.edge_kinds.len() as i64 {
            unreachable!("Boolean seam edge is out of bounds");
        }
        value.edge_kinds[edge as usize] < 3
    }

    pub fn verify_curves(
        cancel_value: Option<&cancel::T>,
        grain: i64,
        curves_value: &geometry::T,
    ) -> Result<(), error::T> {
        super::verify_curves(cancel_value, grain, curves_value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{boolean_constraints, boolean_coplanar, boolean_refinement};
    use std::sync::Mutex;

    type P = (f64, f64, f64);

    fn geom(points: &[P], triangles: &[i64]) -> geometry::T {
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let offsets: Vec<i64> = (0..=(triangles.len() as i64 / 3)).map(|p| p * 3).collect();
        let topo = topology::polygons_owned(points.len() as i64, triangles.to_vec(), offsets).unwrap();
        geometry::create(packed::Float3::View::of_owned_exn(x, y, z).unwrap(), topo, None, None, None)
            .unwrap()
    }

    fn triangle(points: &[P]) -> geometry::T {
        geom(points, &[0, 1, 2])
    }

    fn curve_geometry(points: &[P], primitives: &[Vec<i64>], kinds: &[PrimitiveKind]) -> geometry::T {
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let mut offsets = vec![0_i64];
        let mut vertices = Vec::new();
        for primitive in primitives {
            vertices.extend(primitive.iter().copied());
            offsets.push(vertices.len() as i64);
        }
        let topo =
            topology::create_owned(points.len() as i64, vertices, offsets, kinds.to_vec()).unwrap();
        geometry::create(packed::Float3::View::of_owned_exn(x, y, z).unwrap(), topo, None, None, None)
            .unwrap()
    }

    fn pipeline(left: &geometry::T, right: &geometry::T, resolve_left: bool, resolve_right: bool) -> Arc<complex::T> {
        let mut params = boolean_constraints::params(1);
        params.resolve_left_self_intersections = resolve_left;
        params.resolve_right_self_intersections = resolve_right;
        let constraints = Arc::new(boolean_constraints::build(&params, left, right).unwrap());
        let coplanar = boolean_coplanar::build(None, 1, &constraints).unwrap();
        let refinement = boolean_refinement::build(None, Some(&coplanar), 1, &constraints).unwrap();
        Arc::new(complex::build(None, &constraints, &refinement).unwrap())
    }

    fn seam(left: &geometry::T, right: &geometry::T, resolve_left: bool, resolve_right: bool) -> T {
        let value = pipeline(left, right, resolve_left, resolve_right);
        build(None, &params(), &value).unwrap()
    }

    fn count(value: &geometry::T) -> i64 {
        geometry::primitive_count(value)
    }

    #[test]
    fn coincident_products() {
        let identical = seam(
            &triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]),
            &triangle(&[(0., 2., 0.), (2., 0., 0.), (0., 0., 0.)]),
            false,
            false,
        );
        assert!(count(curves(&identical)) == 0, "fully coincident triangles produced a false boundary seam");
        assert!(count(coincident(&identical)) == 1, "fully coincident triangles were not emitted once");
        let partial = seam(
            &triangle(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)]),
            &triangle(&[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.)]),
            false,
            false,
        );
        assert!(count(coincident(&partial)) == 4, "partial coplanar overlap lost refined coincident facets");
        assert!(count(curves(&partial)) > 0, "partial coplanar overlap lost its area-boundary seam");
        for curve in 0..count(curves(&partial)) {
            assert!(
                curve_kind(&partial, curve) == Kind::Between,
                "partial overlap boundary has a non-between kind"
            );
        }
    }

    fn far_triangle() -> geometry::T {
        triangle(&[(10., 10., 0.), (11., 10., 0.), (10., 11., 0.)])
    }

    fn transverse_pair() -> geometry::T {
        geom(
            &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)],
            &[0, 1, 2, 3, 4, 5],
        )
    }

    #[test]
    fn self_kinds() {
        let left = seam(&transverse_pair(), &far_triangle(), true, false);
        assert!(
            count(curves(&left)) == 1 && curve_kind(&left, 0) == Kind::LeftSelf,
            "left same-operand intersection did not produce a left-self seam"
        );
        let right = seam(&far_triangle(), &transverse_pair(), false, true);
        assert!(
            count(curves(&right)) == 1 && curve_kind(&right, 0) == Kind::RightSelf,
            "right same-operand intersection did not produce a right-self seam"
        );
    }

    #[test]
    fn same_operand_coincidence() {
        let duplicate = geom(
            &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (0., 0., 0.), (2., 0., 0.), (0., 2., 0.)],
            &[0, 1, 2, 3, 4, 5],
        );
        let left = seam(&duplicate, &far_triangle(), true, false);
        assert!(
            count(coincident(&left)) == 1 && count(curves(&left)) == 0,
            "left same-operand coincident facets were not emitted once"
        );
        let right = seam(&far_triangle(), &duplicate, false, true);
        assert!(
            count(coincident(&right)) == 1 && count(curves(&right)) == 0,
            "right same-operand coincident facets were not emitted once"
        );
    }

    #[test]
    fn native_nonmanifold_is_not_self_intersection() {
        let native = geom(
            &[(0., 0., 0.), (2., 0., 0.), (1., 1., 0.), (1., 0., 1.), (1., -1., 0.)],
            &[0, 1, 2, 1, 0, 3, 0, 1, 4],
        );
        let value = seam(&native, &far_triangle(), true, false);
        assert!(
            count(curves(&value)) == 0,
            "ordinary source-native non-manifold adjacency became a self seam"
        );
    }

    fn disjoint_complex() -> Arc<complex::T> {
        pipeline(
            &triangle(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)]),
            &triangle(&[(10., 0., 0.), (12., 0., 0.), (10., 2., 0.)]),
            false,
            false,
        )
    }

    #[test]
    fn cancellation() {
        let value = disjoint_complex();
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), &params(), &value) {
            Err(failure) => assert_eq!(error::code(&failure), "cancelled"),
            Ok(_) => panic!("cancelled Boolean seam extraction completed"),
        }
    }

    #[test]
    fn invalid_grain() {
        let value = disjoint_complex();
        let grain = Params { grain: 0, ..params() };
        match build(None, &grain, &value) {
            Err(failure) => assert_eq!(error::code(&failure), "invalid_parameter"),
            Ok(_) => panic!("zero Boolean seam grain was accepted"),
        }
        let cutoff = Params { parallel_cutoff: 0, ..params() };
        match build(None, &cutoff, &value) {
            Err(failure) => assert_eq!(error::code(&failure), "invalid_parameter"),
            Ok(_) => panic!("zero Boolean seam parallel cutoff was accepted"),
        }
    }

    #[test]
    fn edge_classification_retention() {
        let complex_value = disjoint_complex();
        let value = build(None, &params(), &complex_value).unwrap();
        for edge in 0..complex::edge_count(&complex_value) {
            assert!(
                !View::is_seam_edge(&value, edge),
                "disjoint source boundary was retained as a Boolean seam edge"
            );
        }
    }

    #[test]
    #[should_panic(expected = "Boolean seam edge is out of bounds")]
    fn edge_classification_rejects_negative() {
        let complex_value = disjoint_complex();
        let value = build(None, &params(), &complex_value).unwrap();
        View::is_seam_edge(&value, -1);
    }

    #[test]
    #[should_panic(expected = "Boolean seam edge is out of bounds")]
    fn edge_classification_rejects_past_end() {
        let complex_value = disjoint_complex();
        let value = build(None, &params(), &complex_value).unwrap();
        View::is_seam_edge(&value, complex::edge_count(&complex_value));
    }

    #[test]
    fn post_rounding_verification() {
        let open = PrimitiveKind::OpenPolyline;
        let verify = |geometry_value: &geometry::T| verify_curves(None, 1, geometry_value);
        let crossing = curve_geometry(
            &[(-1., 0., 0.), (1., 0., 0.), (0., -1., 0.), (0., 1., 0.)],
            &[vec![0, 1], vec![2, 3]],
            &[open, open],
        );
        let crossing_error = match verify(&crossing) {
            Err(failure) if error::code(&failure) == "seam_self_intersection" => error::to_string(&failure),
            Err(failure) => panic!("unexpected seam verification error: {}", error::to_string(&failure)),
            Ok(()) => panic!("crossing rounded seam curves passed verification"),
        };
        assert!(
            crossing_error.contains("0") && crossing_error.contains("1"),
            "seam crossing diagnostic lost stable curve IDs"
        );
        let branch = curve_geometry(
            &[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)],
            &[vec![0, 1], vec![0, 2]],
            &[open, open],
        );
        verify(&branch).unwrap();
        let overlap = curve_geometry(
            &[(0., 0., 0.), (1., 0., 0.), (2., 0., 0.)],
            &[vec![0, 2], vec![0, 1]],
            &[open, open],
        );
        match verify(&overlap) {
            Err(failure) if error::code(&failure) == "seam_self_intersection" => {}
            Err(failure) => panic!("unexpected overlap error: {}", error::to_string(&failure)),
            Ok(()) => panic!("overlapping rounded seam segments passed verification"),
        }
        let bow_tie = curve_geometry(
            &[(-1., -1., 0.), (1., 1., 0.), (-1., 1., 0.), (1., -1., 0.)],
            &[vec![0, 1, 2, 3]],
            &[PrimitiveKind::ClosedPolyline],
        );
        match verify(&bow_tie) {
            Err(failure) if error::code(&failure) == "seam_self_intersection" => {}
            Err(failure) => panic!("unexpected closed-curve error: {}", error::to_string(&failure)),
            Ok(()) => panic!("self-crossing closed seam passed verification"),
        }
        let skew = curve_geometry(
            &[(-1., 0., 0.), (1., 0., 0.), (0., -1., 1.), (0., 1., 1.)],
            &[vec![0, 1], vec![2, 3]],
            &[open, open],
        );
        verify(&skew).unwrap();
        let tiny = f64::from_bits(1);
        let subnormal = curve_geometry(&[(0., 0., 0.), (tiny, 0., 0.)], &[vec![0, 1]], &[open]);
        verify(&subnormal).unwrap();
        match verify(&triangle(&[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)])) {
            Err(failure) if error::code(&failure) == "invalid_output" => {}
            Err(failure) => panic!("unexpected polygon verification error: {}", error::to_string(&failure)),
            Ok(()) => panic!("polygon entered seam curve verification"),
        }
        let token = cancel::create();
        cancel::cancel(&token);
        match verify_curves(Some(&token), 1, &branch) {
            Err(failure) if error::is_cancelled(&failure) => {}
            Err(failure) => panic!("unexpected seam verification cancellation: {}", error::to_string(&failure)),
            Ok(()) => panic!("cancelled seam verification completed"),
        }
        let run = |domains: i64| {
            let slot: Mutex<Option<Result<(), String>>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                *slot.lock().unwrap() = Some(verify(&crossing).map_err(|failure| error::to_string(&failure)));
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        assert!(run(1) == run(4), "seam verification diagnostic differs between domain counts");
    }

    #[test]
    fn verification_scale() {
        let count = 4096_usize;
        let points: Vec<P> =
            (0..count * 2).map(|point| ((point / 2) as f64 * 4., (point & 1) as f64, 0.)).collect();
        let primitives: Vec<Vec<i64>> =
            (0..count).map(|segment| vec![segment as i64 * 2, segment as i64 * 2 + 1]).collect();
        let kinds = vec![PrimitiveKind::OpenPolyline; count];
        let curves_value = curve_geometry(&points, &primitives, &kinds);
        let run = |domains: i64| {
            let slot: Mutex<Option<bool>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                *slot.lock().unwrap() = Some(verify_curves(None, 256, &curves_value).is_ok());
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        assert!(run(1) && run(4), "scale seam verification failed or differs across domain counts");
    }

    #[test]
    fn facet_mask_parallel_path_matches_serial() {
        let value = pipeline(
            &triangle(&[(0., 0., 0.), (4., 0., 0.), (2., 4., 0.)]),
            &triangle(&[(0., 3., 0.), (4., 3., 0.), (2., -1., 0.)]),
            false,
            false,
        );
        let signature = |built: &T| -> Vec<u64> {
            let mut bits = Vec::new();
            for part in [curves(built), coincident(built)] {
                let positions = packed::Float3::View::view(geometry::positions(part)).unwrap();
                bits.extend(positions.x.iter().chain(positions.y.iter()).chain(positions.z.iter()).map(|v| v.to_bits()));
                bits.push(geometry::primitive_count(part) as u64);
            }
            bits
        };
        let run = |domains: i64, cutoff: i64| {
            let slot: Mutex<Option<Vec<u64>>> = Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                let options = Params { grain: 1, parallel_cutoff: cutoff, ..params() };
                *slot.lock().unwrap() = Some(signature(&build(None, &options, &value).unwrap()));
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        let serial = run(1, 1_000_000);
        assert!(serial == run(1, 1) && serial == run(4, 1), "parallel facet masks differ from the serial path");
    }
}
