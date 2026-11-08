//! Port of lib/rdk/boolean/boolean_cells.ml: exact-axis shell winding classification for a
//! Weiler complex. `Parallel.both` of the two component builds runs sequentially (the result is
//! identical); BVH query scratch is local to each query so queries share no mutable state.
use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::sync::{Arc, Mutex};

use rdk_core::{cancel, error};
use rdk_core::error::Code;
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
    order: Vec<i64>,
    tree_min_x: Vec<f64>,
    tree_min_y: Vec<f64>,
    tree_min_z: Vec<f64>,
    tree_max_x: Vec<f64>,
    tree_max_y: Vec<f64>,
    tree_max_z: Vec<f64>,
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

#[allow(non_snake_case)]
pub mod View {
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

/// OCaml `Float.compare` (NaN equals itself and sorts first; -0 equals +0).
fn float_compare(left: f64, right: f64) -> Ordering {
    if left < right {
        Ordering::Less
    } else if left > right {
        Ordering::Greater
    } else if left == right || (left.is_nan() && right.is_nan()) {
        Ordering::Equal
    } else if left.is_nan() {
        Ordering::Less
    } else {
        Ordering::Greater
    }
}

fn center(lower: f64, upper: f64) -> f64 {
    lower * 0.5 + upper * 0.5
}

struct Tree {
    order: Vec<i64>,
    min_x: Vec<f64>,
    min_y: Vec<f64>,
    min_z: Vec<f64>,
    max_x: Vec<f64>,
    max_y: Vec<f64>,
    max_z: Vec<f64>,
}

fn include_node(tree: &mut Tree, parent: usize, child: usize) {
    tree.min_x[parent] = float_min(tree.min_x[parent], tree.min_x[child]);
    tree.min_y[parent] = float_min(tree.min_y[parent], tree.min_y[child]);
    tree.min_z[parent] = float_min(tree.min_z[parent], tree.min_z[child]);
    tree.max_x[parent] = float_max(tree.max_x[parent], tree.max_x[child]);
    tree.max_y[parent] = float_max(tree.max_y[parent], tree.max_y[child]);
    tree.max_z[parent] = float_max(tree.max_z[parent], tree.max_z[child]);
}

fn build_subtree(
    tree: &mut Tree,
    built: &mut i64,
    cancel_value: Option<&cancel::T>,
    bounds: &[&[f64]; 6],
    first: i64,
    last: i64,
) -> Result<(), Fail> {
    if first > last {
        return Ok(());
    }
    *built += 1;
    cancel::poll(cancel_value, *built, 4095)?;
    let middle = first + (last - first) / 2;
    let m = middle as usize;
    let component = tree.order[m] as usize;
    tree.min_x[m] = bounds[0][component];
    tree.min_y[m] = bounds[1][component];
    tree.min_z[m] = bounds[2][component];
    tree.max_x[m] = bounds[3][component];
    tree.max_y[m] = bounds[4][component];
    tree.max_z[m] = bounds[5][component];
    if first < middle {
        build_subtree(tree, built, cancel_value, bounds, first, middle - 1)?;
        include_node(tree, m, (first + (middle - 1 - first) / 2) as usize);
    }
    if middle < last {
        build_subtree(tree, built, cancel_value, bounds, middle + 1, last)?;
        include_node(tree, m, (middle + 1 + (last - (middle + 1)) / 2) as usize);
    }
    Ok(())
}

fn build_component_tree(
    cancel_value: Option<&cancel::T>,
    bounds: &[&[f64]; 6],
) -> Result<Tree, Fail> {
    let [min_x, min_y, min_z, max_x, max_y, max_z] = *bounds;
    let count = min_x.len();
    let mut order: Vec<i64> = (0..count as i64).collect();
    if count > 1 {
        let span = |lower: &[f64], upper: &[f64]| -> Result<f64, Fail> {
            let mut minimum = f64::INFINITY;
            let mut maximum = f64::NEG_INFINITY;
            for component in 0..count {
                cancel::poll(cancel_value, component as i64, 4095)?;
                let value = center(lower[component], upper[component]);
                minimum = float_min(minimum, value);
                maximum = float_max(maximum, value);
            }
            Ok(maximum - minimum)
        };
        let mut spans =
            [(span(min_x, max_x)?, 0_i64), (span(min_y, max_y)?, 1), (span(min_z, max_z)?, 2)];
        spans.sort_by(|left, right| float_compare(right.0, left.0).then(left.1.cmp(&right.1)));
        let axis = spans[0].1;
        let (lower, upper) = if axis == 0 {
            (min_x, max_x)
        } else if axis == 1 {
            (min_y, max_y)
        } else {
            (min_z, max_z)
        };
        let comparisons = Cell::new(0_i64);
        let failure: RefCell<Option<error::T>> = RefCell::new(None);
        order.sort_by(|&left, &right| {
            comparisons.set(comparisons.get() + 1);
            if let Err(bad) = cancel::poll(cancel_value, comparisons.get(), 4095) {
                failure.borrow_mut().get_or_insert(bad);
            }
            float_compare(
                center(lower[left as usize], upper[left as usize]),
                center(lower[right as usize], upper[right as usize]),
            )
            .then(left.cmp(&right))
        });
        if let Some(bad) = failure.into_inner() {
            return Err(bad.into());
        }
    }
    let mut tree = Tree {
        order,
        min_x: vec![f64::INFINITY; count],
        min_y: vec![f64::INFINITY; count],
        min_z: vec![f64::INFINITY; count],
        max_x: vec![f64::NEG_INFINITY; count],
        max_y: vec![f64::NEG_INFINITY; count],
        max_z: vec![f64::NEG_INFINITY; count],
    };
    let mut built = 0_i64;
    build_subtree(&mut tree, &mut built, cancel_value, bounds, 0, count as i64 - 1)?;
    cancel::check_opt(cancel_value)?;
    Ok(tree)
}

fn triangle_point(c: &constraints::T, side: Side, triangle: i64, local: i64) -> i64 {
    match side {
        Side::Left => constraints::View::left_triangle_point(c, triangle, local),
        Side::Right => constraints::View::right_triangle_point(c, triangle, local),
    }
}

fn find(parent: &mut [i64], value: i64) -> i64 {
    let mut root = value;
    while parent[root as usize] != root {
        root = parent[root as usize];
    }
    let mut cursor = value;
    while parent[cursor as usize] != cursor {
        let next = parent[cursor as usize];
        parent[cursor as usize] = root;
        cursor = next;
    }
    root
}

fn unite(parent: &mut [i64], left: i64, right: i64) {
    let left = find(parent, left);
    let right = find(parent, right);
    if left != right {
        if left < right {
            parent[right as usize] = left;
        } else {
            parent[left as usize] = right;
        }
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
    let mut parent: Vec<i64> = (0..triangle_count).collect();
    let source_points = constraints::View::source_point_count(c);
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
                unite(&mut parent, triangle, other);
            }
        }
    }
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        let root = find(&mut parent, triangle);
        parent[triangle as usize] = root;
    }
    let mut component_of_root = vec![-1_i64; triangle_count as usize];
    let mut count = 0_i64;
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        if parent[triangle as usize] == triangle {
            component_of_root[triangle as usize] = count;
            count += 1;
        }
    }
    let mut component_of_triangle = Vec::with_capacity(triangle_count as usize);
    for triangle in 0..triangle_count {
        cancel::poll(cancel_value, triangle, 4095)?;
        component_of_triangle.push(component_of_root[parent[triangle as usize] as usize]);
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
    let source = constraints::View::source(c);
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
        order: tree.order,
        tree_min_x: tree.min_x,
        tree_min_y: tree.min_y,
        tree_min_z: tree.min_z,
        tree_max_x: tree.max_x,
        tree_max_y: tree.max_y,
        tree_max_z: tree.max_z,
    })
}

/// Candidate components whose box meets the query's certified bounds, in OCaml's traversal order.
fn query_components_indexed(components: &Components, query: &implicit_point::T) -> Vec<i64> {
    let ((x_lower, x_upper), (y_lower, y_upper), (z_lower, z_upper)) =
        implicit_point::bounds(query);
    let mut found = Vec::new();
    let mut stack: Vec<(i64, i64)> = Vec::new();
    if !components.order.is_empty() {
        stack.push((0, components.order.len() as i64 - 1));
    }
    while let Some((first, last)) = stack.pop() {
        let middle = first + (last - first) / 2;
        let m = middle as usize;
        if components.tree_max_x[m] >= x_lower
            && components.tree_min_x[m] <= x_upper
            && components.tree_max_y[m] >= y_lower
            && components.tree_min_y[m] <= y_upper
            && components.tree_max_z[m] >= z_lower
            && components.tree_min_z[m] <= z_upper
        {
            let component = components.order[m];
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
            if middle < last {
                stack.push((middle + 1, last));
            }
            if first < middle {
                stack.push((first, middle - 1));
            }
        }
    }
    found
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
    let source = constraints::View::source(c);
    let axis = axis_index(axis);
    let mut winding = 0_i64;
    let mut ambiguous = false;
    for component in query_components_indexed(components, query) {
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
    let source = constraints::View::source(c);
    let mut winding = 0_i64;
    let mut boundary = -1_i64;
    let mut degenerate = -1_i64;
    for component in query_components_indexed(components, query) {
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
    let constraints_value = complex::View::constraints(c);
    let incidence = context.incidences[context.offsets[shell as usize] as usize];
    let facet = incidence >> 1;
    let a = complex::View::vertex(c, complex::facet_vertex(c, facet, 0));
    let b = complex::View::vertex(c, complex::facet_vertex(c, facet, 1));
    let cc = complex::View::vertex(c, complex::facet_vertex(c, facet, 2));
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
    if !Arc::ptr_eq(weiler::View::complex(weiler_value), c) {
        return Err(invalid_fail("Weiler graph belongs to a different Boolean complex"));
    }
    let constraints_value = complex::View::constraints(c);
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
        geometry::create(packed::Float3::View::of_owned_exn(x, y, z).unwrap(), topo, None, None, None)
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
            Err(failure) => assert_eq!(error::kind(&failure), Code::Cancelled),
            Ok(_) => panic!("cancelled cell classification completed"),
        }
    }
}
