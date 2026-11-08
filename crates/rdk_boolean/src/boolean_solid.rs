//! Port of lib/rdk/boolean/boolean_solid.ml: build-once exact solid Boolean arrangement.
use std::sync::Arc;

use rdk_core::{cancel, error, geometry};
use rdk_core::error::Code;

use crate::boolean_cells;
use crate::boolean_complex;
use crate::boolean_constraints::{self, Side};
use crate::boolean_coplanar;
use crate::boolean_extract;
use crate::boolean_radial;
use crate::boolean_refinement;
use crate::boolean_seam;
use crate::boolean_weiler::{self, Side as WeilerSide};

const OPERATION: &str = "boolean_solid";


fn cx(cancel: Option<&Arc<cancel::T>>) -> Option<&cancel::T> {
    cancel.map(|value| value.as_ref())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Treatment {
    Solid,
    Surface,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
    Union,
    Intersection,
    Difference,
    ReverseDifference,
    Xor,
    Shatter,
}

pub struct T {
    complex: Arc<boolean_complex::T>,
    weiler: Arc<boolean_weiler::T>,
    cells: boolean_cells::T,
    left_treatment: Treatment,
    right_treatment: Treatment,
}

#[allow(non_snake_case)]
pub mod View {
    use std::sync::Arc;

    use super::{boolean_weiler, T};

    pub fn weiler(value: &T) -> &Arc<boolean_weiler::T> {
        &value.weiler
    }
}

fn shatter_error() -> error::T {
    // OCaml `invalid_arg`: an exception outside every `result`.
    error::new(OPERATION, Code::InvalidArgument, "Shatter has no single product expression".to_owned())
}

/// Construct and classify the shared two-operand arrangement once. Degenerate source triangle
/// pairs are rejected until their repair policy is explicit. Self-intersection resolution is
/// opt-in per operand so known-clean solids do not pay for self broad phases. `Surface` operands
/// remain exact arrangement constraints and seam contributors but add zero to volumetric winding.
#[allow(clippy::too_many_arguments)]
pub fn prepare(
    cancel: Option<&Arc<cancel::T>>,
    resolve_left_self_intersections: Option<bool>,
    resolve_right_self_intersections: Option<bool>,
    left_treatment: Option<Treatment>,
    right_treatment: Option<Treatment>,
    grain: i64,
    left: &geometry::T,
    right: &geometry::T,
) -> Result<T, error::T> {
    let left_treatment = left_treatment.unwrap_or(Treatment::Solid);
    let right_treatment = right_treatment.unwrap_or(Treatment::Solid);
    let mut params = boolean_constraints::params(grain);
    params.cancel = cancel.cloned();
    params.resolve_left_self_intersections = resolve_left_self_intersections.unwrap_or(false);
    params.resolve_right_self_intersections = resolve_right_self_intersections.unwrap_or(false);
    let constraints = Arc::new(boolean_constraints::build(&params, left, right)?);
    let degenerate_pairs = boolean_constraints::degenerate_pair_count(&constraints);
    if degenerate_pairs != 0 {
        return Err(error::new(
            OPERATION,
            Code::DegenerateTriangle,
            format!(
                "{degenerate_pairs} candidate triangle pair(s) contain a degenerate source triangle (remove or repair zero-area source triangles before solid Boolean)"
            ),
        ));
    }
    let coplanar = boolean_coplanar::build(cx(cancel), grain, &constraints)?;
    let refinement = boolean_refinement::build(cx(cancel), Some(&coplanar), grain, &constraints)?;
    let complex = Arc::new(boolean_complex::build(cx(cancel), &constraints, &refinement)?);
    let radial = boolean_radial::build(cx(cancel), &complex)?;
    let weiler = Arc::new(boolean_weiler::build(cx(cancel), &complex, &radial)?);
    let cells = boolean_cells::build(
        cx(cancel),
        &boolean_cells::Params {
            track_left: left_treatment == Treatment::Solid,
            track_right: right_treatment == Treatment::Solid,
        },
        &complex,
        &weiler,
    )?;
    Ok(T { complex, weiler, cells, left_treatment, right_treatment })
}

fn extract_with_ancestry(
    cancel: Option<&Arc<cancel::T>>,
    options: &boolean_extract::Options,
    expression: &boolean_extract::Expression,
    value: &T,
) -> Result<boolean_extract::Ancestry, error::T> {
    boolean_extract::build_with_ancestry(cx(cancel), options, expression, &value.complex, &value.weiler, &value.cells)
}

fn operation_expression(operation: Operation) -> Result<boolean_extract::Expression, error::T> {
    match operation {
        Operation::Union => Ok(boolean_extract::union()),
        Operation::Intersection => Ok(boolean_extract::intersection()),
        Operation::Difference => Ok(boolean_extract::difference()),
        Operation::ReverseDifference => Ok(boolean_extract::reverse_difference()),
        Operation::Xor => Ok(boolean_extract::xor()),
        Operation::Shatter => Err(shatter_error()),
    }
}

fn first_member_on_side(complex: &boolean_complex::T, facet: i64, side: Side) -> i64 {
    let (first, last) = boolean_complex::facet_member_range(complex, facet);
    let mut member = first;
    while member < last {
        if boolean_complex::member_side(complex, member) == side {
            return member;
        }
        member += 1;
    }
    -1
}

fn source_oriented_selection(complex: &boolean_complex::T, side: Side, keep: &dyn Fn(i64) -> bool) -> Vec<u8> {
    let facets = boolean_complex::facet_count(complex);
    let mut selection = vec![0u8; facets as usize];
    for facet in 0..facets {
        let member = first_member_on_side(complex, facet, side);
        if member >= 0 && keep(facet) {
            selection[facet as usize] = if boolean_complex::member_winding(complex, member) > 0 { 1 } else { 2 };
        }
    }
    selection
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Location {
    Outside,
    Inside,
    Boundary,
}

fn shell_windings(cells: &boolean_cells::T, weiler: &boolean_weiler::T, facet: i64, side: Side) -> (i64, i64) {
    let negative = boolean_weiler::half_facet_shell(weiler, boolean_weiler::half_facet(facet, WeilerSide::Negative));
    let positive = boolean_weiler::half_facet_shell(weiler, boolean_weiler::half_facet(facet, WeilerSide::Positive));
    match side {
        Side::Left => (boolean_cells::left_winding(cells, negative), boolean_cells::left_winding(cells, positive)),
        Side::Right => (boolean_cells::right_winding(cells, negative), boolean_cells::right_winding(cells, positive)),
    }
}

fn relative_location(cells: &boolean_cells::T, weiler: &boolean_weiler::T, facet: i64, side: Side) -> Location {
    let (negative_winding, positive_winding) = shell_windings(cells, weiler, facet, side);
    if negative_winding == 0 && positive_winding == 0 {
        Location::Outside
    } else if negative_winding != 0 && positive_winding != 0 {
        Location::Inside
    } else {
        Location::Boundary
    }
}

#[allow(clippy::too_many_arguments)]
fn extract_selection(
    cancel: Option<&Arc<cancel::T>>,
    defer_rounded_slivers: bool,
    barycentric_cache: Option<&mut boolean_extract::BarycentricCache>,
    corner_payload: bool,
    value: &T,
    side: Side,
    selection: &[u8],
) -> Result<boolean_extract::Ancestry, error::T> {
    boolean_extract::build_selected_with_ancestry(
        cx(cancel),
        defer_rounded_slivers,
        corner_payload,
        barycentric_cache,
        selection,
        side,
        &value.complex,
        &value.weiler,
        &value.cells,
    )
}

fn selected_count(selection: &[u8]) -> i64 {
    selection.iter().filter(|byte| **byte != 0).count() as i64
}

fn extract_mixed_with_ancestry(
    cancel: Option<&Arc<cancel::T>>,
    defer_rounded_slivers: bool,
    corner_payload: bool,
    operation: Operation,
    solid_side: Side,
    surface_side: Side,
    value: &T,
) -> Result<boolean_extract::Ancestry, error::T> {
    let surface_location = |facet: i64| relative_location(&value.cells, &value.weiler, facet, solid_side);
    let surface_is_left = surface_side == Side::Left;
    let keep_surface = |facet: i64| match (operation, surface_location(facet)) {
        (Operation::Union, Location::Outside) => true,
        (Operation::Intersection, Location::Inside | Location::Boundary) => true,
        (Operation::Difference, Location::Outside) if surface_is_left => true,
        (Operation::ReverseDifference, Location::Outside) if !surface_is_left => true,
        (Operation::Xor, Location::Outside) => true,
        _ => false,
    };
    let double_surface = |facet: i64| match (operation, surface_location(facet)) {
        (Operation::Difference, Location::Inside) if !surface_is_left => true,
        (Operation::ReverseDifference, Location::Inside) if surface_is_left => true,
        (Operation::Xor, Location::Inside) => true,
        _ => false,
    };
    let volume_present = matches!(
        (operation, solid_side),
        (Operation::Union | Operation::Xor, _)
            | (Operation::Difference, Side::Left)
            | (Operation::ReverseDifference, Side::Right)
    );
    let facet_count = boolean_complex::facet_count(&value.complex);
    let mut volume_selection = vec![0u8; facet_count as usize];
    if volume_present {
        for facet in 0..facet_count {
            if first_member_on_side(&value.complex, facet, solid_side) >= 0 {
                let (negative, positive) = shell_windings(&value.cells, &value.weiler, facet, solid_side);
                let negative_inside = negative != 0;
                let positive_inside = positive != 0;
                if negative_inside != positive_inside {
                    volume_selection[facet as usize] = if positive_inside { 2 } else { 1 };
                }
            }
        }
    }
    let surface_selection = source_oriented_selection(&value.complex, surface_side, &keep_surface);
    let double_selection = source_oriented_selection(&value.complex, surface_side, &double_surface);
    let mut cache = boolean_extract::View::barycentric_cache(if corner_payload {
        3 * (selected_count(&volume_selection) + selected_count(&surface_selection) + selected_count(&double_selection))
    } else {
        0
    });
    let volume = extract_selection(
        cancel, defer_rounded_slivers, Some(&mut cache), corner_payload, value, solid_side, &volume_selection,
    )?;
    let surface = extract_selection(
        cancel, defer_rounded_slivers, Some(&mut cache), corner_payload, value, surface_side, &surface_selection,
    )?;
    let wall = extract_selection(
        cancel, defer_rounded_slivers, Some(&mut cache), corner_payload, value, surface_side, &double_selection,
    )?;
    let reverse_wall = boolean_extract::View::reverse_ancestry(cx(cancel), &wall)?;
    boolean_extract::View::concatenate_ancestries(cx(cancel), &[volume, surface, wall, reverse_wall])
}

fn extract_surface_pair_with_ancestry(
    cancel: Option<&Arc<cancel::T>>,
    defer_rounded_slivers: bool,
    corner_payload: bool,
    operation: Operation,
    value: &T,
) -> Result<boolean_extract::Ancestry, error::T> {
    if operation == Operation::Shatter {
        return Err(shatter_error());
    }
    let has = |facet: i64, side: Side| first_member_on_side(&value.complex, facet, side) >= 0;
    let choose_left = |facet: i64| match operation {
        Operation::Union => has(facet, Side::Left),
        Operation::Intersection => has(facet, Side::Left) && has(facet, Side::Right),
        Operation::Difference => has(facet, Side::Left) && !has(facet, Side::Right),
        Operation::ReverseDifference | Operation::Shatter => false,
        Operation::Xor => has(facet, Side::Left) != has(facet, Side::Right),
    };
    let choose_right = |facet: i64| match operation {
        Operation::Union | Operation::Xor => !has(facet, Side::Left) && has(facet, Side::Right),
        Operation::Intersection | Operation::Difference | Operation::Shatter => false,
        Operation::ReverseDifference => has(facet, Side::Right) && !has(facet, Side::Left),
    };
    let left = source_oriented_selection(&value.complex, Side::Left, &choose_left);
    let right = source_oriented_selection(&value.complex, Side::Right, &choose_right);
    let mut cache = boolean_extract::View::barycentric_cache(if corner_payload {
        3 * (selected_count(&left) + selected_count(&right))
    } else {
        0
    });
    let left =
        extract_selection(cancel, defer_rounded_slivers, Some(&mut cache), corner_payload, value, Side::Left, &left)?;
    let right =
        extract_selection(cancel, defer_rounded_slivers, Some(&mut cache), corner_payload, value, Side::Right, &right)?;
    boolean_extract::View::concatenate_ancestries(cx(cancel), &[left, right])
}

/// Treatment-aware solid/surface product extraction. Sheet facets remain zero-volume arrangement
/// constraints and are emitted explicitly, including paired opposite-facing walls for a
/// solid-minus-surface cut.
pub fn extract_product_with_ancestry(
    cancel: Option<&Arc<cancel::T>>,
    require_closed: Option<bool>,
    defer_rounded_slivers: Option<bool>,
    corner_payload: Option<bool>,
    operation: Operation,
    value: &T,
) -> Result<boolean_extract::Ancestry, error::T> {
    let corner_payload = corner_payload.unwrap_or(true);
    let defer = defer_rounded_slivers.unwrap_or(false);
    if operation == Operation::Shatter {
        return Err(shatter_error());
    }
    let ancestry = match (value.left_treatment, value.right_treatment) {
        (Treatment::Solid, Treatment::Solid) => extract_with_ancestry(
            cancel,
            &boolean_extract::Options {
                require_closed: require_closed.unwrap_or(true),
                defer_rounded_slivers: defer,
                corner_payload,
            },
            &operation_expression(operation)?,
            value,
        )?,
        (Treatment::Solid, Treatment::Surface) => {
            extract_mixed_with_ancestry(cancel, defer, corner_payload, operation, Side::Left, Side::Right, value)?
        }
        (Treatment::Surface, Treatment::Solid) => {
            extract_mixed_with_ancestry(cancel, defer, corner_payload, operation, Side::Right, Side::Left, value)?
        }
        (Treatment::Surface, Treatment::Surface) => {
            extract_surface_pair_with_ancestry(cancel, defer, corner_payload, operation, value)?
        }
    };
    if require_closed == Some(true)
        && (value.left_treatment == Treatment::Surface || value.right_treatment == Treatment::Surface)
    {
        boolean_extract::View::validate_closed_topology(geometry::topology(boolean_extract::geometry(&ancestry)))?;
    }
    Ok(ancestry)
}

/// Materialize deterministic self/intersection curves and coincident facets from the already
/// prepared exact complex without repeating arrangement.
pub fn seams(
    cancel: Option<&Arc<cancel::T>>,
    grain: Option<i64>,
    materialize: Option<bool>,
    value: &T,
) -> Result<boolean_seam::T, error::T> {
    let mut options = boolean_seam::params();
    if let Some(grain) = grain {
        options.grain = grain;
    }
    if let Some(materialize) = materialize {
        options.materialize = materialize;
    }
    boolean_seam::build(cx(cancel), &options, &value.complex)
}

/// Extract stable A-only, overlap, and B-only closed region boundaries from one prepared
/// arrangement. Shared walls are intentionally duplicated between products.
pub fn shatter_with_ancestry(
    cancel: Option<&Arc<cancel::T>>,
    require_closed: Option<bool>,
    defer_rounded_slivers: Option<bool>,
    corner_payload: Option<bool>,
    value: &T,
) -> Result<Vec<boolean_extract::Ancestry>, error::T> {
    let options = boolean_extract::Options {
        require_closed: require_closed.unwrap_or(true),
        defer_rounded_slivers: defer_rounded_slivers.unwrap_or(false),
        corner_payload: corner_payload.unwrap_or(true),
    };
    let left_only = extract_with_ancestry(cancel, &options, &boolean_extract::difference(), value)?;
    let overlap = extract_with_ancestry(cancel, &options, &boolean_extract::intersection(), value)?;
    let right_only = extract_with_ancestry(cancel, &options, &boolean_extract::reverse_difference(), value)?;
    Ok(vec![left_only, overlap, right_only])
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::{packed, topology};

    fn geometry_of(points: &[(f64, f64, f64)], triangles: Vec<i64>) -> geometry::T {
        let x: Vec<f64> = points.iter().map(|p| p.0).collect();
        let y: Vec<f64> = points.iter().map(|p| p.1).collect();
        let z: Vec<f64> = points.iter().map(|p| p.2).collect();
        let offsets: Vec<i64> = (0..=(triangles.len() / 3) as i64).map(|primitive| primitive * 3).collect();
        let topology_value = topology::polygons_owned(points.len() as i64, triangles, offsets).unwrap();
        geometry::create(packed::Float3::View::of_owned_exn(x, y, z).unwrap(), topology_value, None, None, None)
            .unwrap()
    }

    fn tetra(origin: (f64, f64, f64), size: f64) -> geometry::T {
        let (ox, oy, oz) = origin;
        geometry_of(
            &[(ox, oy, oz), (ox + size, oy, oz), (ox, oy + size, oz), (ox, oy, oz + size)],
            vec![0, 2, 1, 0, 1, 3, 1, 2, 3, 2, 0, 3],
        )
    }

    // test_degenerate_rejection
    #[test]
    fn degenerate_rejection() {
        let degenerate = geometry_of(&[(0., 0., 0.), (0.5, 0., 0.), (1., 0., 0.)], vec![0, 1, 2]);
        let solid = tetra((0., 0., 0.), 1.);
        match prepare(None, None, None, None, None, 1, &degenerate, &solid) {
            Err(failure) if matches!(error::kind(&failure), Code::DegenerateTriangle | Code::InvalidSurface) => {}
            Err(failure) => panic!("unexpected degenerate error: {}", error::to_string(&failure)),
            Ok(_) => panic!("degenerate source triangle entered the solid Boolean pipeline"),
        }
    }

    // test_cancellation
    #[test]
    fn cancellation() {
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        match prepare(
            Some(&token),
            None,
            None,
            None,
            None,
            1,
            &tetra((0., 0., 0.), 1.),
            &tetra((0.2, 0.2, 0.2), 1.),
        ) {
            Err(failure) if error::is_cancelled(&failure) => {}
            Err(failure) => panic!("unexpected cancellation error: {}", error::to_string(&failure)),
            Ok(_) => panic!("cancelled Boolean preparation completed"),
        }
    }
}
