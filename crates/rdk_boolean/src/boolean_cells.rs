//! Port of lib/rdk/boolean/boolean_cells.ml: exact-axis shell winding classification for a
//! Weiler complex. Operand component builds share the worker pool; each BVH query owns its scratch.
use rdk_core::support::DisjointSet;
use std::sync::{Arc, Mutex};

use rdk_core::{cancel, error};
use rdk_core::error::Code;
use rdk_geom::bounds3_index;
use rdk_geom::implicit_point::{self, AxisRayTriangle, SymbolicRayTriangle};
use rdk_geom::point_grid::{float_max, float_min};
use rdk_geom::predicates::Sign;

use crate::boolean_complex as complex;
use crate::boolean_constraints::{self as constraints, Side};
use crate::boolean_weiler as weiler;

const OPERATION: &str = "boolean_cells";

pub struct T {
    weiler: Arc<weiler::T>,
    left: Vec<i64>,
    right: Vec<i64>,
}

#[derive(Clone, Copy)]
enum Axis {
    X,
    Y,
    Z,
}

struct Components {
    offsets: Vec<i64>,
    triangles: Vec<i64>,
    min_x: Vec<f64>,
    min_y: Vec<f64>,
    min_z: Vec<f64>,
    max_x: Vec<f64>,
    max_y: Vec<f64>,
    max_z: Vec<f64>,
    tree: bounds3_index::T,
}

enum Fail {
    Cancelled,
    Invalid(String),
    Ambiguous(i64, String),
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

fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

pub fn shell_count(value: &T) -> i64 {
    value.left.len() as i64
}

pub fn left_winding(value: &T, shell: i64) -> i64 {
    value.left[shell as usize]
}

pub fn right_winding(value: &T, shell: i64) -> i64 {
    value.right[shell as usize]
}

pub mod view {
    use super::*;

    pub fn weiler(value: &T) -> &Arc<weiler::T> {
        &value.weiler
    }
}

fn orient2(axis: Axis, a: &implicit_point::T, b: &implicit_point::T, c: &implicit_point::T) -> Sign {
    match axis {
        Axis::X => implicit_point::orient2d_yz(a, b, c),
        Axis::Y => implicit_point::orient2d_zx(a, b, c),
        Axis::Z => implicit_point::orient2d_xy(a, b, c),
    }
}

fn axis_index(axis: Axis) -> i64 {
    match axis {
        Axis::X => 0,
        Axis::Y => 1,
        Axis::Z => 2,
    }
}

fn checked_add(left: i64, right: i64) -> Result<i64, Fail> {
    match left.checked_add(right) {
        Some(value) => Ok(value),
        None => Err(invalid_fail("Boolean winding integer overflow")),
    }
}

fn center(lower: f64, upper: f64) -> f64 {
    lower * 0.5 + upper * 0.5
}

fn build_component_tree(
    cancel_value: Option<&cancel::T>,
    bounds: &[&[f64]; 6],
) -> Result<bounds3_index::T, Fail> {
    cancel::check_opt(cancel_value)?;
    let count = bounds[0].len();
    let mut centroid: [Vec<f64>; 3] = std::array::from_fn(|_| Vec::with_capacity(count));
    for component in 0..count {
        cancel::poll(cancel_value, component as i64, 4095)?;
        for axis in 0..3 {
            centroid[axis].push(center(bounds[axis][component], bounds[axis + 3][component]));
        }
    }
    let tree = bounds3_index::create(
        cancel_value,
        None,
        16_384,
        &centroid[0],
        &centroid[1],
        &centroid[2],
        bounds[0],
        bounds[1],
        bounds[2],
        bounds[3],
        bounds[4],
        bounds[5],
    )?;
    cancel::check_opt(cancel_value)?;
    Ok(tree)
}

fn triangle_point(c: &constraints::T, side: Side, triangle: i64, local: i64) -> i64 {
    match side {
        Side::Left => constraints::view::left_triangle_point(c, triangle, local),
        Side::Right => constraints::view::right_triangle_point(c, triangle, local),
    }
}

fn build_components(
    cancel_value: Option<&cancel::T>,
    c: &constraints::T,
    side: Side,
) -> Result<Components, Fail> {
    let triangle_count = match side {
        Side::Left => constraints::left_triangle_count(c),
        Side::Right => constraints::right_triangle_count(c),
    };
    let mut parent = DisjointSet::new(triangle_count as usize);
    let source_points = constraints::view::source_point_count(c);
    let mut first_triangle = vec![-1_i64; source_points as usize];
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        for local in 0..3 {
            let point = triangle_point(c, side, triangle, local);
            if point < 0 || point >= source_points {
                return Err(invalid_fail("Boolean component source point is out of range"));
            }
            let other = first_triangle[point as usize];
            if other < 0 {
                first_triangle[point as usize] = triangle;
            } else {
                parent.union_min(triangle as usize, other as usize);
            }
        }
    }
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        let root = parent.find(triangle as usize) as i64;
        parent.parent[triangle as usize] = root;
    }
    let mut component_of_root = vec![-1_i64; triangle_count as usize];
    let mut count = 0_i64;
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        if parent.parent[triangle as usize] == triangle {
            component_of_root[triangle as usize] = count;
            count += 1;
        }
    }
    let mut component_of_triangle = Vec::with_capacity(triangle_count as usize);
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        component_of_triangle.push(component_of_root[parent.parent[triangle as usize] as usize]);
    }
    let n = count as usize;
    let mut counts = vec![0_i64; n];
    for &component in &component_of_triangle {
        counts[component as usize] += 1;
    }
    let mut offsets = vec![0_i64; n + 1];
    for component in 0..n {
        offsets[component + 1] = offsets[component] + counts[component];
    }
    let mut triangles = vec![0_i64; triangle_count as usize];
    let mut cursor = offsets.clone();
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        let component = component_of_triangle[triangle as usize] as usize;
        triangles[cursor[component] as usize] = triangle;
        cursor[component] += 1;
    }
    let mut min_x = vec![f64::INFINITY; n];
    let mut min_y = vec![f64::INFINITY; n];
    let mut min_z = vec![f64::INFINITY; n];
    let mut max_x = vec![f64::NEG_INFINITY; n];
    let mut max_y = vec![f64::NEG_INFINITY; n];
    let mut max_z = vec![f64::NEG_INFINITY; n];
    let source = constraints::view::source(c);
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        let component = component_of_triangle[triangle as usize] as usize;
        for local in 0..3 {
            let (x, y, z) =
                implicit_point::source_coordinate(source, triangle_point(c, side, triangle, local));
            min_x[component] = float_min(min_x[component], x);
            min_y[component] = float_min(min_y[component], y);
            min_z[component] = float_min(min_z[component], z);
            max_x[component] = float_max(max_x[component], x);
            max_y[component] = float_max(max_y[component], y);
            max_z[component] = float_max(max_z[component], z);
        }
    }
    let tree = build_component_tree(
        cancel_value,
        &[&min_x[..], &min_y[..], &min_z[..], &max_x[..], &max_y[..], &max_z[..]],
    )?;
    Ok(Components {
        offsets,
        triangles,
        min_x,
        min_y,
        min_z,
        max_x,
        max_y,
        max_z,
        tree,
    })
}

/// Candidate component IDs whose boxes meet the query's certified bounds, in ascending order.
fn query_components_indexed(
    cancel_value: Option<&cancel::T>,
    components: &Components,
    query: &implicit_point::T,
) -> Result<Vec<i64>, Fail> {
    cancel::check_opt(cancel_value)?;
    let ((x_lower, x_upper), (y_lower, y_upper), (z_lower, z_upper)) =
        implicit_point::bounds(query);
    let tree = &components.tree;
    let mut found = Vec::new();
    // Median splits bound pending siblings by the bit width of the item count.
    let mut stack = [0usize; usize::BITS as usize];
    let mut pending = usize::from(!tree.order.is_empty());
    let mut visited = 0;
    while pending > 0 {
        pending -= 1;
        let node = stack[pending];
        cancel::poll(cancel_value, visited, 4095)?;
        visited += 1;
        if !(tree.max_x[node] >= x_lower
            && tree.min_x[node] <= x_upper
            && tree.max_y[node] >= y_lower
            && tree.min_y[node] <= y_upper
            && tree.max_z[node] >= z_lower
            && tree.min_z[node] <= z_upper)
        {
            continue;
        }
        if tree.count[node] == 0 {
            stack[pending] = tree.right[node] as usize;
            stack[pending + 1] = tree.left[node] as usize;
            pending += 2;
        } else {
            for slot in tree.first[node]..tree.first[node] + tree.count[node] {
                let component = tree.order[slot as usize];
                let k = component as usize;
                if components.max_x[k] >= x_lower
                    && components.min_x[k] <= x_upper
                    && components.max_y[k] >= y_lower
                    && components.min_y[k] <= y_upper
                    && components.max_z[k] >= z_lower
                    && components.min_z[k] <= z_upper
                {
                    found.push(component);
                }
            }
        }
    }
    found.sort_unstable();
    cancel::check_opt(cancel_value)?;
    Ok(found)
}

#[allow(clippy::too_many_arguments)]
fn classify_operand(
    cancel_value: Option<&cancel::T>,
    c: &constraints::T,
    components: &Components,
    side: Side,
    query: &implicit_point::T,
    axis: Axis,
    positive: bool,
    skip: &dyn Fn(i64) -> bool,
) -> Result<Option<i64>, Fail> {
    let source = constraints::view::source(c);
    let axis = axis_index(axis);
    let mut winding = 0_i64;
    let mut ambiguous = false;
    for component in query_components_indexed(cancel_value, components, query)? {
        let component = component as usize;
        for slot in components.offsets[component]..components.offsets[component + 1] {
            let face = components.triangles[slot as usize];
            cancel::poll(cancel_value, face, 4095)?;
            if !skip(face) {
                match implicit_point::axis_ray_source_triangle(
                    source,
                    triangle_point(c, side, face, 0),
                    triangle_point(c, side, face, 1),
                    triangle_point(c, side, face, 2),
                    axis,
                    positive,
                    query,
                ) {
                    AxisRayTriangle::AxisMiss | AxisRayTriangle::AxisParallel => {}
                    AxisRayTriangle::AxisBoundary => ambiguous = true,
                    AxisRayTriangle::AxisHit(normal) => {
                        winding =
                            checked_add(winding, if normal == Sign::Positive { 1 } else { -1 })?;
                    }
                }
            }
        }
    }
    Ok(if ambiguous { None } else { Some(winding) })
}

enum Symbolic {
    Winding(i64),
    SeedBoundary(i64),
    SourceDegenerate(i64),
}

fn classify_operand_symbolic(
    cancel_value: Option<&cancel::T>,
    c: &constraints::T,
    components: &Components,
    side: Side,
    query: &implicit_point::T,
    skip: &dyn Fn(i64) -> bool,
) -> Result<Symbolic, Fail> {
    let source = constraints::view::source(c);
    let mut winding = 0_i64;
    let mut boundary = -1_i64;
    let mut degenerate = -1_i64;
    for component in query_components_indexed(cancel_value, components, query)? {
        let component = component as usize;
        for slot in components.offsets[component]..components.offsets[component + 1] {
            let face = components.triangles[slot as usize];
            cancel::poll(cancel_value, face, 4095)?;
            if !skip(face) {
                match implicit_point::symbolic_ray_source_triangle(
                    source,
                    triangle_point(c, side, face, 0),
                    triangle_point(c, side, face, 1),
                    triangle_point(c, side, face, 2),
                    query,
                ) {
                    SymbolicRayTriangle::SymbolicMiss => {}
                    SymbolicRayTriangle::SymbolicHit(normal) => {
                        winding =
                            checked_add(winding, if normal == Sign::Positive { 1 } else { -1 })?;
                    }
                    SymbolicRayTriangle::SymbolicOriginBoundary => {
                        if boundary < 0 {
                            boundary = face;
                        }
                    }
                    SymbolicRayTriangle::SymbolicDegenerate => {
                        if degenerate < 0 {
                            degenerate = face;
                        }
                    }
                }
            }
        }
    }
    Ok(if degenerate >= 0 {
        Symbolic::SourceDegenerate(degenerate)
    } else if boundary >= 0 {
        Symbolic::SeedBoundary(boundary)
    } else {
        Symbolic::Winding(winding)
    })
}

fn finish_fail(failure: Fail) -> error::T {
    match failure {
        Fail::Cancelled => error::cancelled(OPERATION),
        Fail::Ambiguous(facet, reason) => make(
            Code::AmbiguousClassification,
            format!("could not classify refined seed facet {facet}: {reason}"),
        ),
        Fail::Invalid(message) => make(Code::InvalidComplex, message),
        Fail::Rdk(failure) => failure,
    }
}

/// OCaml's `?track_left ?track_right`.
#[derive(Clone, Copy)]
pub struct Params {
    pub track_left: bool,
    pub track_right: bool,
}

pub fn params() -> Params {
    Params { track_left: true, track_right: true }
}

const UNKNOWN: i64 = i64::MIN;

struct State {
    left: Vec<i64>,
    right: Vec<i64>,
    queue: Vec<i64>,
}

fn assign(state: &mut State, shell: i64, left_value: i64, right_value: i64) -> Result<(), Fail> {
    let s = shell as usize;
    if state.left[s] == UNKNOWN {
        state.left[s] = left_value;
        state.right[s] = right_value;
        state.queue.push(shell);
        Ok(())
    } else if state.left[s] != left_value || state.right[s] != right_value {
        Err(invalid_fail("inconsistent winding propagation across Weiler cells"))
    } else {
        Ok(())
    }
}

fn shells_of(weiler_value: &weiler::T, facet: i64) -> (i64, i64) {
    (
        weiler::half_facet_shell(weiler_value, weiler::half_facet(facet, weiler::Side::Negative)),
        weiler::half_facet_shell(weiler_value, weiler::half_facet(facet, weiler::Side::Positive)),
    )
}

pub fn build(
    cancel_value: Option<&cancel::T>,
    options: &Params,
    c: &Arc<complex::T>,
    weiler_value: &Arc<weiler::T>,
) -> Result<T, error::T> {
    build_inner(cancel_value, options, c, weiler_value).map_err(|f| finish_fail(f))
}

struct Context {
    track_left: bool,
    track_right: bool,
    left_components: Option<Components>,
    right_components: Option<Components>,
    offsets: Vec<i64>,
    incidences: Vec<i64>,
}

fn propagate(weiler: &weiler::T, context: &Context, state: &mut State) -> Result<(), Fail> {
    let mut queue_first = 0;
    while queue_first < state.queue.len() {
        let shell = state.queue[queue_first];
        queue_first += 1;
        let s = shell as usize;
        for slot in context.offsets[s]..context.offsets[s + 1] {
            let incidence = context.incidences[slot as usize];
            let facet = incidence >> 1;
            let current_positive = incidence & 1 == 1;
            let (negative, positive) = shells_of(weiler, facet);
            let delta_left =
                if context.track_left { weiler::facet_left_winding(weiler, facet) } else { 0 };
            let delta_right = if context.track_right {
                weiler::facet_right_winding(weiler, facet)
            } else {
                0
            };
            if current_positive {
                let l = checked_add(state.left[s], delta_left)?;
                let r = checked_add(state.right[s], delta_right)?;
                assign(state, negative, l, r)?;
            } else {
                let l = checked_add(state.left[s], -delta_left)?;
                let r = checked_add(state.right[s], -delta_right)?;
                assign(state, positive, l, r)?;
            }
        }
    }
    Ok(())
}

fn seed_component(cancel_value: Option<&cancel::T>, complex: &complex::T, weiler: &weiler::T, context: &Context, state: &mut State, shell: i64) -> Result<(), Fail> {
    let c = complex;
    let constraints_value = complex::view::constraints(c);
    let incidence = context.incidences[context.offsets[shell as usize] as usize];
    let facet = incidence >> 1;
    let a = complex::view::vertex(c, complex::facet_vertex(c, facet, 0));
    let b = complex::view::vertex(c, complex::facet_vertex(c, facet, 1));
    let cc = complex::view::vertex(c, complex::facet_vertex(c, facet, 2));
    let query = match implicit_point::centroid3(&a, &b, &cc) {
        Ok(point) => point,
        Err(_) => return Err(invalid_fail("could not construct exact facet centroid")),
    };
    let (first_member, last_member) = complex::facet_member_range(c, facet);
    let skip = |side: Side, face: i64| -> bool {
        let mut member = first_member;
        while member < last_member {
            if complex::member_side(c, member) == side && complex::member_face(c, member) == face {
                return true;
            }
            member += 1;
        }
        false
    };
    let skip_left = |face: i64| skip(Side::Left, face);
    let skip_right = |face: i64| skip(Side::Right, face);
    let choices = [
        (Axis::X, false),
        (Axis::Y, false),
        (Axis::Z, false),
        (Axis::X, true),
        (Axis::Y, true),
        (Axis::Z, true),
    ];
    for (axis, positive) in choices {
        let normal = orient2(axis, &a, &b, &cc);
        if normal == Sign::Zero {
            continue;
        }
        let left_value = if context.track_left {
            classify_operand(
                cancel_value,
                constraints_value,
                context.left_components.as_ref().expect("left components"),
                Side::Left,
                &query,
                axis,
                positive,
                &skip_left,
            )?
        } else {
            Some(0)
        };
        let right_value = if context.track_right {
            classify_operand(
                cancel_value,
                constraints_value,
                context.right_components.as_ref().expect("right components"),
                Side::Right,
                &query,
                axis,
                positive,
                &skip_right,
            )?
        } else {
            Some(0)
        };
        if let (Some(left_value), Some(right_value)) = (left_value, right_value) {
            let side = if (normal == Sign::Positive) == positive {
                weiler::Side::Positive
            } else {
                weiler::Side::Negative
            };
            let seed_shell =
                weiler::half_facet_shell(weiler, weiler::half_facet(facet, side));
            return assign(state, seed_shell, left_value, right_value);
        }
    }
    let left_value = if context.track_left {
        classify_operand_symbolic(
            cancel_value,
            constraints_value,
            context.left_components.as_ref().expect("left components"),
            Side::Left,
            &query,
            &skip_left,
        )?
    } else {
        Symbolic::Winding(0)
    };
    let right_value = if context.track_right {
        classify_operand_symbolic(
            cancel_value,
            constraints_value,
            context.right_components.as_ref().expect("right components"),
            Side::Right,
            &query,
            &skip_right,
        )?
    } else {
        Symbolic::Winding(0)
    };
    match (left_value, right_value) {
        (Symbolic::Winding(left_value), Symbolic::Winding(right_value)) => {
            let normal = implicit_point::normal_dot_symbolic(&a, &b, &cc);
            if normal == Sign::Zero {
                Err(Fail::Ambiguous(facet, "the refined seed facet is exactly degenerate".to_owned()))
            } else {
                let side = if normal == Sign::Positive {
                    weiler::Side::Positive
                } else {
                    weiler::Side::Negative
                };
                let seed_shell =
                    weiler::half_facet_shell(weiler, weiler::half_facet(facet, side));
                assign(state, seed_shell, left_value, right_value)
            }
        }
        (Symbolic::SeedBoundary(face), _) => Err(Fail::Ambiguous(
            facet,
            format!("left source triangle {face} contains the seed but is not a refined-facet member"),
        )),
        (_, Symbolic::SeedBoundary(face)) => Err(Fail::Ambiguous(
            facet,
            format!("right source triangle {face} contains the seed but is not a refined-facet member"),
        )),
        (Symbolic::SourceDegenerate(face), _) => Err(Fail::Ambiguous(
            facet,
            format!("left source triangle {face} is exactly degenerate"),
        )),
        (_, Symbolic::SourceDegenerate(face)) => Err(Fail::Ambiguous(
            facet,
            format!("right source triangle {face} is exactly degenerate"),
        )),
    }
}

fn build_inner(
    cancel_value: Option<&cancel::T>,
    options: &Params,
    c: &Arc<complex::T>,
    weiler_value: &Arc<weiler::T>,
) -> Result<T, Fail> {
    cancel::check_opt(cancel_value)?;
    if !Arc::ptr_eq(weiler::view::complex(weiler_value), c) {
        return Err(invalid_fail("Weiler graph belongs to a different Boolean complex"));
    }
    let constraints_value = complex::view::constraints(c);
    let shells = weiler::shell_count(weiler_value);
    let facets = complex::facet_count(c);
    let left_slot: Mutex<Option<Result<Option<Components>, Fail>>> = Mutex::new(None);
    let right_slot: Mutex<Option<Result<Option<Components>, Fail>>> = Mutex::new(None);
    let build_side = |track: bool, side: Side, slot: &Mutex<Option<Result<Option<Components>, Fail>>>| {
        let built = if track { build_components(cancel_value, constraints_value, side).map(Some) } else { Ok(None) };
        *slot.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some(built);
        Ok(())
    };
    parallel::both_unit(
        &|| build_side(options.track_left, Side::Left, &left_slot),
        &|| build_side(options.track_right, Side::Right, &right_slot),
    )
    .map_err(|message| Fail::Invalid(message))?;
    let take = |slot: Mutex<Option<Result<Option<Components>, Fail>>>| {
        slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner).expect("both_unit ran both sides")
    };
    let left_components = take(left_slot)?;
    let right_components = take(right_slot)?;
    let mut counts = vec![0_i64; shells as usize];
    for facet in 0..facets {
        let (negative, positive) = shells_of(weiler_value, facet);
        counts[negative as usize] += 1;
        counts[positive as usize] += 1;
    }
    let mut offsets = vec![0_i64; shells as usize + 1];
    for shell in 0..shells as usize {
        offsets[shell + 1] = offsets[shell] + counts[shell];
    }
    let mut incidences = vec![0_i64; offsets[shells as usize] as usize];
    let mut cursor = offsets.clone();
    for facet in 0..facets {
        let (negative, positive) = shells_of(weiler_value, facet);
        incidences[cursor[negative as usize] as usize] = facet * 2;
        cursor[negative as usize] += 1;
        incidences[cursor[positive as usize] as usize] = facet * 2 + 1;
        cursor[positive as usize] += 1;
    }
    let context = Context {
        track_left: options.track_left,
        track_right: options.track_right,
        left_components,
        right_components,
        offsets,
        incidences,
    };
    let mut state = State {
        left: vec![UNKNOWN; shells as usize],
        right: vec![UNKNOWN; shells as usize],
        queue: Vec::new(),
    };
    for shell in 0..shells {
        if state.left[shell as usize] == UNKNOWN {
            state.queue.clear();
            seed_component(cancel_value, c, weiler_value, &context, &mut state, shell)?;
            propagate(weiler_value, &context, &mut state)?;
        }
    }
    Ok(T { weiler: Arc::clone(weiler_value), left: state.left, right: state.right })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        boolean_constraints, boolean_coplanar, boolean_radial, boolean_refinement,
    };
    use rdk_core::{geometry, packed, topology};
    use std::sync::Mutex;

    type P = (f64, f64, f64);

    fn geom(points: &[P], triangles: &[i64]) -> geometry::T {
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let offsets: Vec<i64> = (0..=(triangles.len() as i64 / 3)).map(|p| p * 3).collect();
        let topo = topology::polygons_owned(points.len() as i64, triangles.to_vec(), offsets).unwrap();
        geometry::create(packed::float3::view::of_owned_exn(x, y, z).unwrap(), topo, None, None, None)
            .unwrap()
    }

    const LOCAL: [i64; 12] = [0, 2, 1, 0, 1, 3, 1, 2, 3, 2, 0, 3];

    fn tetra(origin: P, size: f64) -> geometry::T {
        let (ox, oy, oz) = origin;
        geom(
            &[(ox, oy, oz), (ox + size, oy, oz), (ox, oy + size, oz), (ox, oy, oz + size)],
            &LOCAL,
        )
    }

    fn tetra_batch(count: usize, shift: f64) -> geometry::T {
        let mut points = Vec::new();
        let mut triangles = Vec::new();
        for item in 0..count {
            let point = (item * 4) as i64;
            let x = item as f64 * 6. + shift;
            points.extend([(x, 0., 0.), (x + 1., 0., 0.), (x, 1., 0.), (x, 0., 1.)]);
            triangles.extend(LOCAL.iter().map(|corner| point + corner));
        }
        geom(&points, &triangles)
    }

    fn pipeline(left: &geometry::T, right: &geometry::T) -> (Arc<complex::T>, Arc<weiler::T>, T) {
        let constraints = Arc::new(
            boolean_constraints::build(&boolean_constraints::params(1), left, right).unwrap(),
        );
        let coplanar = boolean_coplanar::build(None, 1, &constraints).unwrap();
        let refinement = boolean_refinement::build(None, Some(&coplanar), 1, &constraints).unwrap();
        let value = Arc::new(complex::build(None, &constraints, &refinement).unwrap());
        let radial = boolean_radial::build(None, &value).unwrap();
        let graph = Arc::new(weiler::build(None, &value, &radial).unwrap());
        let cells = build(None, &params(), &value, &graph).unwrap();
        (value, graph, cells)
    }

    fn shell_values(cells: &T) -> Vec<(i64, i64)> {
        let mut values: Vec<(i64, i64)> =
            (0..shell_count(cells)).map(|shell| (left_winding(cells, shell), right_winding(cells, shell))).collect();
        values.sort();
        values
    }

    fn validate_relations(value: &complex::T, graph: &weiler::T, cells: &T) {
        for facet in 0..complex::facet_count(value) {
            let (negative, positive) = shells_of(graph, facet);
            assert!(
                left_winding(cells, positive)
                    == left_winding(cells, negative) - weiler::facet_left_winding(graph, facet),
                "left winding propagation relation is broken"
            );
            assert!(
                right_winding(cells, positive)
                    == right_winding(cells, negative) - weiler::facet_right_winding(graph, facet),
                "right winding propagation relation is broken"
            );
        }
    }

    type Signature = (Vec<(i64, i64)>, Vec<(i64, i64)>);

    fn signature(value: &complex::T, graph: &weiler::T, cells: &T) -> Signature {
        (
            shell_values(cells),
            (0..complex::facet_count(value))
                .map(|facet| (weiler::facet_left_winding(graph, facet), weiler::facet_right_winding(graph, facet)))
                .collect(),
        )
    }

    fn signature_at(domains: i64, left: &geometry::T, right: &geometry::T) -> Signature {
        let slot: Mutex<Option<Signature>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            let (value, graph, cells) = pipeline(left, right);
            *slot.lock().unwrap() = Some(signature(&value, &graph, &cells));
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }

    #[test]
    fn disjoint_solids() {
        let (value, graph, cells) = pipeline(&tetra((0., 0., 0.), 1.), &tetra((10., 0., 0.), 1.));
        assert!(
            shell_values(&cells) == vec![(0, 0), (0, 0), (0, 1), (1, 0)],
            "disjoint solid winding cells are wrong"
        );
        validate_relations(&value, &graph, &cells);
    }

    #[test]
    fn nested_solids() {
        let (value, graph, cells) = pipeline(&tetra((0., 0., 0.), 4.), &tetra((1., 1., 1.), 0.5));
        assert!(
            shell_values(&cells) == vec![(0, 0), (1, 0), (1, 0), (1, 1)],
            "nested solid winding cells are wrong"
        );
        validate_relations(&value, &graph, &cells);
    }

    #[test]
    fn domain_exactness() {
        let left = tetra((0., 0., 0.), 4.);
        let right = tetra((1., 1., 1.), 0.5);
        assert!(
            signature_at(1, &left, &right) == signature_at(4, &left, &right),
            "cell classification differs between domain counts"
        );
    }

    #[test]
    fn component_index_domains() {
        let fixtures = [
            (tetra((0., 0., 0.), 1.), tetra((10., 0., 0.), 1.)),
            (tetra((0., 0., 0.), 4.), tetra((1., 1., 1.), 0.5)),
            (tetra((-2., -2., -2.), 4.), tetra((-1., -1., -1.), 4.)),
        ];
        for (fixture, (left, right)) in fixtures.iter().enumerate() {
            assert!(
                signature_at(1, left, right) == signature_at(4, left, right),
                "component index differs between domain counts for fixture {fixture}"
            );
        }
    }

    #[test]
    fn component_index_scale() {
        let left = tetra_batch(64, 0.);
        let right = tetra_batch(64, 3.);
        assert!(
            signature_at(1, &left, &right) == signature_at(4, &left, &right),
            "component index scale result differs between domain counts"
        );
    }

    #[test]
    fn component_index_matches_linear_scan() {
        let mut points = Vec::new();
        let mut faces = Vec::new();
        for item in 0..64 {
            let first = points.len() as i64;
            let (x, y, z) = (
                ((item * 17) % 64) as f64,
                (item % 3) as f64,
                (item % 5) as f64,
            );
            let size = 4. + (item % 3) as f64;
            points.extend([
                (x, y, z),
                (x + size, y, z),
                (x, y + size, z),
                (x, y, z + size),
            ]);
            faces.extend(LOCAL.iter().map(|corner| first + corner));
        }
        let constraints = boolean_constraints::build(
            &boolean_constraints::params(1),
            &geom(&points, &faces),
            &geom(&[], &[]),
        )
        .unwrap();
        let components = build_components(None, &constraints, Side::Left)
            .map_err(finish_fail)
            .unwrap();
        assert_eq!(components.offsets.len(), 65);
        let mut queries = vec![(-1., 0., 0.), (0., 0., 0.), (4., 0., 0.), (100., 0., 0.)];
        queries.extend((0..128).map(|i| ((i * 37 % 70) as f64, (i % 7) as f64, (i % 9) as f64)));
        for (x, y, z) in queries {
            let source = implicit_point::source(vec![x, x.next_up()], vec![y; 2], vec![z; 2]).unwrap();
            let a = implicit_point::explicit(&source, 0).unwrap();
            let b = implicit_point::explicit(&source, 1).unwrap();
            for query in [a.clone(), implicit_point::midpoint(&a, &b).unwrap()] {
                let ((xl, xu), (yl, yu), (zl, zu)) = implicit_point::bounds(&query);
                let expected: Vec<_> = (0..64)
                    .filter(|&k| {
                        components.max_x[k] >= xl
                            && components.min_x[k] <= xu
                            && components.max_y[k] >= yl
                            && components.min_y[k] <= yu
                            && components.max_z[k] >= zl
                            && components.min_z[k] <= zu
                    })
                    .map(|k| k as i64)
                    .collect();
                let indexed = query_components_indexed(None, &components, &query)
                    .map_err(finish_fail)
                    .unwrap();
                assert_eq!(
                    indexed, expected,
                    "inclusive certified bounds match a linear scan"
                );
            }
        }
        let token = cancel::create();
        cancel::cancel(&token);
        let empty_components = build_components(None, &constraints, Side::Right)
            .map_err(finish_fail)
            .unwrap();
        let source = implicit_point::source(vec![100.], vec![0.], vec![0.]).unwrap();
        let query = implicit_point::explicit(&source, 0).unwrap();
        for index in [&components, &empty_components] {
            let failure = query_components_indexed(Some(&token), index, &query)
                .map_err(finish_fail)
                .unwrap_err();
            assert_eq!(error::code(&failure), Code::Cancelled);
        }
        let empty: &[f64] = &[];
        for bounds in [
            [empty; 6],
            [
                &components.min_x[..],
                &components.min_y,
                &components.min_z,
                &components.max_x,
                &components.max_y,
                &components.max_z,
            ],
        ] {
            match build_component_tree(Some(&token), &bounds) {
                Err(failure) => assert_eq!(error::code(&finish_fail(failure)), Code::Cancelled),
                Ok(_) => panic!("cancelled component index built"),
            }
        }
    }

    #[test]
    fn untracked_operand_contributes_nothing() {
        let (value, graph, _) = pipeline(&tetra((0., 0., 0.), 1.), &tetra((10., 0., 0.), 1.));
        let only_left = Params { track_left: true, track_right: false };
        let cells = build(None, &only_left, &value, &graph).unwrap();
        assert!((0..shell_count(&cells)).all(|shell| right_winding(&cells, shell) == 0));
    }

    #[test]
    fn cancellation() {
        let (value, graph, _) = pipeline(&tetra((0., 0., 0.), 1.), &tetra((10., 0., 0.), 1.));
        let token = cancel::create();
        cancel::cancel(&token);
        match build(Some(&token), &params(), &value, &graph) {
            Err(failure) => assert_eq!(error::code(&failure), Code::Cancelled),
            Ok(_) => panic!("cancelled cell classification completed"),
        }
    }
}
