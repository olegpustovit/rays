//! Port of lib/rdk/ops/mesh/subdivide.ml: polygon and curve subdivision with local selections,
//! crack closure, creases and holes. Includes the refinement and stitch modules, as the OCaml does.
use std::sync::Arc;

use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::{cancel, edge_group, error, geometry, group, support, topology, topology_index};
use rdk_core::error::Code;

use crate::curve_subdivide;
use crate::deform;
use crate::deletion;
use crate::ordering;
pub use crate::subdivide_refine::*;
pub use crate::subdivide_stitch::*;

use BoundaryInterpolation::*;
use CreasingMethod::*;
use FaceVaryingInterpolation::*;
use TrianglePolicy::*;

fn delete_part(
    cancel_arc: &Option<Arc<cancel::T>>,
    grain: Option<i64>,
    selected: bool,
    compact_points: bool,
    selection: &group::T,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    deletion::delete(
        &deletion::DeleteParams {
            cancel: cancel_arc.clone(),
            grain,
            selected,
            compact_points,
        },
        selection,
        geometry_value,
    )
}

pub fn extract_primitive_part(
    cancel_arc: &Option<Arc<cancel::T>>,
    grain: Option<i64>,
    selected: bool,
    primitives: &group::T,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let part = delete_part(cancel_arc, grain, !selected, true, primitives, geometry_value)?;
    if selected {
        split_disconnected_point_fans(cancel_arc.as_deref(), grain, &part)
    } else {
        Ok(part)
    }
}

pub fn free_point_part(
    cancel_arc: &Option<Arc<cancel::T>>,
    grain: Option<i64>,
    geometry_value: &geometry::T,
) -> Result<Option<geometry::T>, error::T> {
    let topology_view = topology::view::view(geometry::topology(geometry_value))?;
    let mut used = vec![0u8; topology_view.point_count as usize];
    for (vertex, &point) in topology_view.vertex_points.iter().enumerate() {
        cancel::poll(cancel_arc.as_deref(), vertex as i64, 16_383)?;
        used[point as usize] = 1;
    }
    let free_count = used.iter().filter(|&&value| value == 0).count();
    if free_count == 0 {
        return Ok(None);
    }
    cancel::check_opt(cancel_arc.as_deref())?;
    let referenced = group::init(
        grain,
        group::Owner::Point,
        "__rdk_subdivide_referenced_points",
        topology_view.point_count,
        &|point| used[point as usize] != 0,
    )?;
    Ok(Some(delete_part(cancel_arc, grain, true, false, &referenced, geometry_value)?))
}

pub fn iterate(
    cancel_token: Option<&cancel::T>,
    options: &OnceOptions,
    scheme: Scheme,
    iterations: i64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let mut remaining = iterations;
    let mut edge_crease_override = options.edge_crease_override;
    let mut current = geometry_value.clone();
    loop {
        cancel::check_opt(cancel_token)?;
        if remaining == 0 {
            return Ok(current);
        }
        let last = remaining == 1;
        let step = OnceOptions {
            edge_crease_override,
            generate_resulting_creases: !last || options.generate_resulting_creases,
            resulting_crease_group_name: if last {
                options.resulting_crease_group_name.clone()
            } else {
                None
            },
            remove_holes: last && options.remove_holes,
            ..options.clone()
        };
        current = once(cancel_token, &step, scheme, &current)?;
        remaining -= 1;
        edge_crease_override = None;
    }
}

pub fn with_empty_edge_group(
    cancel_token: Option<&cancel::T>,
    name: &str,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let index = topology_index::create(cancel_token, topology_value)?;
    let empty = edge_group::init(None, topology_value, &index, name, &|_| false)?;
    geometry::with_edge_group(empty, geometry_value)
}

pub fn without_resulting_creases(geometry_value: &geometry::T) -> geometry::T {
    let stripped = geometry::without_attribute(Owner::Vertex, "creaseweight", geometry_value);
    let stripped = geometry::without_attribute(Owner::Primitive, "creaseweight", &stripped);
    geometry::without_attribute(Owner::Point, "cornerweight", &stripped)
}

pub fn automatic_boundary_holes(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    geometry_value: &geometry::T,
) -> Result<Option<group::T>, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let topology_view = topology::view::view(topology_value)?;
    let index_value = topology_index::create(cancel_token, topology_value)?;
    let index = topology_index::view::view(&index_value);
    let point_count = topology_view.point_count;
    let point_byte_count = (point_count >> 3) + if point_count & 7 == 0 { 0 } else { 1 };
    let grain = grain.unwrap_or(16_384);
    if grain <= 0 {
        return Err(invalid_arg("Rdk_mesh.Subdivide.subdivide: grain must be positive".to_owned()));
    }
    let mut boundary_points = vec![0_u8; point_byte_count as usize];
    support::par_fill(cancel_token, grain, &mut boundary_points, &|byte| {
        let base = byte << 3;
        let mut value = 0_u8;
        for bit in 0..=(7.min(point_count - base - 1)) {
            let point = base + bit;
            let mut at = gi(&index.point_edge_offsets, point);
            let last = gi(&index.point_edge_offsets, point + 1);
            let mut boundary = false;
            while at < last && !boundary {
                let edge = gi(&index.point_edges, at);
                if gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge) == 1 {
                    boundary = true;
                }
                at += 1;
            }
            if boundary {
                value |= 1 << bit;
            }
        }
        Ok(value)
    })?;
    let primitive_count = topology_view.primitive_kinds.len() as i64;
    let byte_count = (primitive_count >> 3) + if primitive_count & 7 == 0 { 0 } else { 1 };
    let mut hole_bits = vec![0_u8; byte_count as usize];
    support::par_fill(cancel_token, grain, &mut hole_bits, &|byte| {
        let base = byte << 3;
        let mut value = 0_u8;
        for bit in 0..=(7.min(primitive_count - base - 1)) {
            let primitive = base + bit;
            let first = gi(&topology_view.primitive_offsets, primitive);
            let last = gi(&topology_view.primitive_offsets, primitive + 1);
            let mut vertex = first;
            let mut boundary = false;
            while vertex < last && !boundary {
                cancel::poll(cancel_token, vertex, 4095)?;
                let point = gi(&topology_view.vertex_points, vertex);
                if gu(&boundary_points, point >> 3) & (1 << (point & 7)) != 0 {
                    boundary = true;
                }
                vertex += 1;
            }
            if boundary {
                value |= 1 << bit;
            }
        }
        Ok(value)
    })?;
    let holes = group::view::of_owned_bits(
        group::Owner::Primitive,
        "subdivision_hole",
        primitive_count,
        hole_bits,
    )?;
    Ok(if group::cardinality(&holes) == 0 { None } else { Some(holes) })
}

#[derive(Clone, Default)]
pub struct DetailOverrides {
    pub osd_scheme: Option<attribute::T>,
    pub osd_vtxboundaryinterpolation: Option<attribute::T>,
    pub osd_fvarlinearinterpolation: Option<attribute::T>,
    pub osd_creasingmethod: Option<attribute::T>,
    pub osd_trianglesubdiv: Option<attribute::T>,
}

pub fn detail_overrides(geometry_value: &geometry::T) -> DetailOverrides {
    let mut found = DetailOverrides::default();
    for item in geometry::view::attributes(geometry_value) {
        if attribute::owner(item) == Owner::Detail {
            match attribute::name(item) {
                "osd_scheme" => found.osd_scheme = Some(item.clone()),
                "osd_vtxboundaryinterpolation" => {
                    found.osd_vtxboundaryinterpolation = Some(item.clone())
                }
                "osd_fvarlinearinterpolation" => {
                    found.osd_fvarlinearinterpolation = Some(item.clone())
                }
                "osd_creasingmethod" => found.osd_creasingmethod = Some(item.clone()),
                "osd_trianglesubdiv" => found.osd_trianglesubdiv = Some(item.clone()),
                _ => {}
            }
        }
    }
    found
}

fn detail_int(name: &str, item: &attribute::T) -> Result<i64, error::T> {
    match attribute::view::storage(item)? {
        Storage::Int(values) if values.len() == 1 => Ok(values[0]),
        Storage::Int(_) => {
            Err(fail(format!("detail attribute {name} must contain exactly one value")))
        }
        _ => Err(fail(format!(
            "detail attribute {name} must use integer storage, not {}",
            attribute::kind_name(item)
        ))),
    }
}

/// OCaml `%S`: a double-quoted string with `String.escaped` escapes.
fn ocaml_quoted(value: &str) -> String {
    let mut out = String::from("\"");
    for byte in value.bytes() {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            8 => out.push_str("\\b"),
            32..=126 => out.push(byte as char),
            other => out.push_str(&format!("\\{other:03}")),
        }
    }
    out.push('"');
    out
}

pub fn resolve_scheme(default: Scheme, found: Option<&attribute::T>) -> Result<Scheme, error::T> {
    let Some(item) = found else {
        return Ok(default);
    };
    match attribute::view::storage(item)? {
        Storage::Int(values) if values.len() == 1 => match values[0] {
            0 => Ok(Scheme::CatmullClark),
            1 => Ok(Scheme::Loop),
            2 => Ok(Scheme::Bilinear),
            value => Err(fail(format!(
                "detail attribute osd_scheme value {value} is outside [0, 2]"
            ))),
        },
        Storage::Int(_) | Storage::Text(_) if attribute::length(item) != 1 => {
            Err(failm("detail attribute osd_scheme must contain exactly one value"))
        }
        Storage::Text(values) if values.len() == 1 => match std::str::from_utf8(&values[0]).map_err(|_| failm("detail attribute osd_scheme must contain UTF-8 text"))? {
            "catmull-clark" => Ok(Scheme::CatmullClark),
            "loop" => Ok(Scheme::Loop),
            "bilinear" => Ok(Scheme::Bilinear),
            value => Err(fail(format!(
                "detail attribute osd_scheme has unsupported value {}; expected \"catmull-clark\", \"loop\", or \"bilinear\"",
                ocaml_quoted(value)
            ))),
        },
        _ => Err(fail(format!(
            "detail attribute osd_scheme must use integer or text storage, not {}",
            attribute::kind_name(item)
        ))),
    }
}

pub fn resolve_boundary_interpolation(
    default: BoundaryInterpolation,
    found: Option<&attribute::T>,
) -> Result<BoundaryInterpolation, error::T> {
    let Some(item) = found else {
        return Ok(default);
    };
    match detail_int("osd_vtxboundaryinterpolation", item)? {
        0 => Ok(SubdivideBoundaryNone),
        1 => Ok(SubdivideBoundaryEdgeOnly),
        2 => Ok(SubdivideBoundaryEdgeAndCorner),
        value => Err(fail(format!(
            "detail attribute osd_vtxboundaryinterpolation value {value} is outside [0, 2]"
        ))),
    }
}

pub fn resolve_face_varying_interpolation(
    default: FaceVaryingInterpolation,
    found: Option<&attribute::T>,
) -> Result<FaceVaryingInterpolation, error::T> {
    let Some(item) = found else {
        return Ok(default);
    };
    match detail_int("osd_fvarlinearinterpolation", item)? {
        0 => Ok(SubdivideFvarNone),
        1 => Ok(SubdivideFvarCornersOnly),
        2 => Ok(SubdivideFvarCornersPlus1),
        3 => Ok(SubdivideFvarCornersPlus2),
        4 => Ok(SubdivideFvarBoundaries),
        5 => Ok(SubdivideFvarAll),
        value => Err(fail(format!(
            "detail attribute osd_fvarlinearinterpolation value {value} is outside [0, 5]"
        ))),
    }
}

pub fn resolve_creasing_method(
    default: CreasingMethod,
    found: Option<&attribute::T>,
) -> Result<CreasingMethod, error::T> {
    let Some(item) = found else {
        return Ok(default);
    };
    match detail_int("osd_creasingmethod", item)? {
        0 => Ok(SubdivideCreasingUniform),
        1 => Ok(SubdivideCreasingChaikin),
        value => Err(fail(format!(
            "detail attribute osd_creasingmethod value {value} is outside [0, 1]"
        ))),
    }
}

pub fn resolve_triangle_subdivision(
    default: TrianglePolicy,
    found: Option<&attribute::T>,
) -> Result<TrianglePolicy, error::T> {
    let Some(item) = found else {
        return Ok(default);
    };
    match detail_int("osd_trianglesubdiv", item)? {
        0 => Ok(SubdivideTrianglesCatmullClark),
        1 => Ok(SubdivideTrianglesSmooth),
        value => Err(fail(format!(
            "detail attribute osd_trianglesubdiv value {value} is outside [0, 1]"
        ))),
    }
}

/// The optional arguments of `subdivide_polygons`.
#[derive(Clone)]
pub struct PolygonOptions {
    pub scheme: Scheme,
    pub iterations: i64,
    pub primitives: Option<group::T>,
    pub cracks: CrackPolicy,
    pub consistent_topology: bool,
    pub creases: Option<geometry::T>,
    pub crease_primitives: Option<group::T>,
    pub crease_weight: Option<f64>,
    pub generate_resulting_creases: bool,
    pub resulting_crease_group: Option<String>,
    pub hole_primitives: Option<group::T>,
    pub remove_holes: bool,
    pub boundary_interpolation: BoundaryInterpolation,
    pub face_varying_interpolation: FaceVaryingInterpolation,
    pub triangle_subdivision: TrianglePolicy,
    pub creasing_method: CreasingMethod,
}

impl Default for PolygonOptions {
    fn default() -> PolygonOptions {
        PolygonOptions {
            scheme: Scheme::CatmullClark,
            iterations: 1,
            primitives: None,
            cracks: CrackPolicy::SubdivideDoNotClose,
            consistent_topology: false,
            creases: None,
            crease_primitives: None,
            crease_weight: None,
            generate_resulting_creases: true,
            resulting_crease_group: None,
            hole_primitives: None,
            remove_holes: true,
            boundary_interpolation: SubdivideBoundaryEdgeOnly,
            face_varying_interpolation: SubdivideFvarAll,
            triangle_subdivision: SubdivideTrianglesCatmullClark,
            creasing_method: SubdivideCreasingUniform,
        }
    }
}

fn validate_iterations_and_bias(iterations: i64, cracks: CrackPolicy) -> Result<(), error::T> {
    if iterations < 0 {
        return Err(invalid_arg(
            "Rdk_mesh.Subdivide.subdivide: iterations must be non-negative".to_owned(),
        ));
    }
    if let CrackPolicy::SubdividePullDivideEdges(bias) | CrackPolicy::SubdividePullTriangulate(bias) =
        cracks
    {
        if !bias.is_finite() || !(0.0..=1.0).contains(&bias) {
            return Err(invalid_arg(
                "Rdk_mesh.Subdivide.subdivide: Pull Divide bias must be finite and in [0, 1]"
                    .to_owned(),
            ));
        }
    }
    Ok(())
}

fn validate_crease_arguments(
    creases: &Option<geometry::T>,
    crease_primitives: &Option<group::T>,
    crease_weight: Option<f64>,
    generate_resulting_creases: bool,
    resulting_crease_group: &Option<String>,
) -> Result<(), error::T> {
    if creases.is_none() && crease_primitives.is_some() {
        return Err(failm("crease primitive selection requires a crease input"));
    }
    if let Some(value) = crease_weight {
        if !value.is_finite() || value < 0.0 {
            return Err(failm("crease override must be finite and non-negative"));
        }
    }
    if let Some(name) = resulting_crease_group {
        if rdk_core::support::ocaml_trim_is_empty(name) {
            return Err(failm("resulting crease group name must not be empty"));
        }
        if !generate_resulting_creases {
            return Err(failm("resulting crease group requires generated resulting creases"));
        }
    }
    Ok(())
}

pub fn subdivide_polygons(
    cancel_arc: &Option<Arc<cancel::T>>,
    grain: Option<i64>,
    options: &PolygonOptions,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_token = cancel_arc.as_deref();
    validate_iterations_and_bias(options.iterations, options.cracks)?;
    cancel::check_opt(cancel_token)?;
    let overrides = detail_overrides(geometry_value);
    let scheme = resolve_scheme(options.scheme, overrides.osd_scheme.as_ref())?;
    let boundary_interpolation = resolve_boundary_interpolation(
        options.boundary_interpolation,
        overrides.osd_vtxboundaryinterpolation.as_ref(),
    )?;
    let face_varying_interpolation = resolve_face_varying_interpolation(
        options.face_varying_interpolation,
        overrides.osd_fvarlinearinterpolation.as_ref(),
    )?;
    let triangle_subdivision = resolve_triangle_subdivision(
        options.triangle_subdivision,
        overrides.osd_trianglesubdiv.as_ref(),
    )?;
    let creasing_method =
        resolve_creasing_method(options.creasing_method, overrides.osd_creasingmethod.as_ref())?;
    validate_crease_arguments(
        &options.creases,
        &options.crease_primitives,
        options.crease_weight,
        options.generate_resulting_creases,
        &options.resulting_crease_group,
    )?;
    let iterations = options.iterations;
    let cracks = options.cracks;
    let consistent_topology = options.consistent_topology;
    let generate_resulting_creases = options.generate_resulting_creases;
    let prepared = match &options.creases {
        None => geometry_value.clone(),
        Some(creases) => apply_crease_input(
            cancel_token,
            grain,
            options.crease_primitives.as_ref(),
            options.crease_weight,
            geometry_value,
            creases,
        )?,
    };
    let initial_edge_crease_override =
        if options.creases.is_none() { options.crease_weight } else { None };
    let prepared = match &options.hole_primitives {
        None => prepared,
        Some(holes) => {
            validate_hole_group(holes, &prepared)?;
            geometry::with_group(group::with_name("subdivision_hole", holes)?, &prepared)?
        }
    };
    let prepared = match (boundary_interpolation, scheme) {
        (SubdivideBoundaryNone, Scheme::CatmullClark | Scheme::Loop) => {
            match automatic_boundary_holes(cancel_token, grain, &prepared)? {
                None => prepared,
                Some(boundary_holes) => {
                    let holes = match geometry::find_group(
                        group::Owner::Primitive,
                        "subdivision_hole",
                        &prepared,
                    ) {
                        None => boundary_holes,
                        Some(existing) => group::union(&existing, &boundary_holes)?,
                    };
                    geometry::with_group(holes, &prepared)?
                }
            }
        }
        _ => prepared,
    };
    let prepared = match &options.resulting_crease_group {
        Some(name) if iterations > 0 => with_empty_edge_group(cancel_token, name, &prepared)?,
        _ => prepared,
    };
    let hole_group = if options.remove_holes {
        geometry::find_group(group::Owner::Primitive, "subdivision_hole", &prepared)
    } else {
        None
    };
    let primitives = match (&options.primitives, hole_group) {
        (Some(selection), Some(holes)) => Some(group::union(selection, &holes)?),
        (Some(selection), None) => Some(selection.clone()),
        (None, _) => None,
    };
    let iterate_options = OnceOptions {
        grain,
        edge_ancestry_attribute: None,
        edge_crease_override: initial_edge_crease_override,
        generate_resulting_creases,
        resulting_crease_group_name: options.resulting_crease_group.clone(),
        remove_holes: options.remove_holes,
        boundary_interpolation,
        face_varying_interpolation,
        triangle_subdivision,
        creasing_method,
    };
    let Some(selection) = primitives else {
        if iterations == 0 {
            return Ok(geometry_value.clone());
        }
        return iterate(cancel_token, &iterate_options, scheme, iterations, &prepared);
    };
    validate_primitive_selection(&selection, &prepared)?;
    if iterations == 0 || group::cardinality(&selection) == 0 {
        return Ok(geometry_value.clone());
    }
    if group::cardinality(&selection) == group::length(&selection) {
        return iterate(cancel_token, &iterate_options, scheme, iterations, &prepared);
    }
    let closing = !matches!(cracks, CrackPolicy::SubdivideDoNotClose);
    let (selected_source, pull_plan) = if closing {
        annotate_pull_interfaces(cancel_token, &selection, &prepared)?
    } else {
        (prepared.clone(), None)
    };
    let selected =
        extract_primitive_part(cancel_arc, grain, true, &selection, &selected_source)?;
    let unselected = match cracks {
        CrackPolicy::SubdividePullDivideEdges(_)
        | CrackPolicy::SubdividePullTriangulate(_)
        | CrackPolicy::SubdivideStitchDivideEdges
        | CrackPolicy::SubdivideStitchTriangulate => None,
        _ => Some(extract_primitive_part(cancel_arc, grain, false, &selection, &prepared)?),
    };
    let refined_with_ancestry = iterate(
        cancel_token,
        &OnceOptions {
            edge_ancestry_attribute: pull_plan.as_ref().map(|plan| plan.ancestry_attribute.clone()),
            ..iterate_options.clone()
        },
        scheme,
        iterations,
        &selected,
    )?;
    let dividing = matches!(
        cracks,
        CrackPolicy::SubdividePullDivideEdges(_)
            | CrackPolicy::SubdividePullTriangulate(_)
            | CrackPolicy::SubdivideStitchDivideEdges
            | CrackPolicy::SubdivideStitchTriangulate
    );
    let chains = match (&pull_plan, dividing) {
        (Some(plan), true) => Some(interface_chains(cancel_token, plan, &refined_with_ancestry)?),
        _ => None,
    };
    let divided = match (&pull_plan, &chains) {
        (Some(plan), Some(chains)) => {
            let divided = divide_unselected_edges(cancel_token, grain, plan, chains, &prepared)?;
            Some(match cracks {
                CrackPolicy::SubdividePullTriangulate(bias) => triangulate_divided_boundary(
                    cancel_token,
                    grain,
                    Some(bias),
                    consistent_topology,
                    plan,
                    chains,
                    &divided,
                    &refined_with_ancestry,
                )?,
                CrackPolicy::SubdivideStitchTriangulate => triangulate_divided_boundary(
                    cancel_token,
                    grain,
                    None,
                    consistent_topology,
                    plan,
                    chains,
                    &divided,
                    &refined_with_ancestry,
                )?,
                _ => divided,
            })
        }
        _ => None,
    };
    let refined = match (cracks, &pull_plan) {
        (CrackPolicy::SubdividePullNoEdgeDivision, Some(plan)) => {
            pull_boundary_no_edge_division(cancel_token, grain, plan, &refined_with_ancestry)?
        }
        (_, Some(plan)) => geometry::without_attribute(
            Owner::Vertex,
            &plan.ancestry_attribute,
            &refined_with_ancestry,
        ),
        (_, None) => refined_with_ancestry.clone(),
    };
    let free = free_point_part(cancel_arc, grain, &prepared)?;
    let free_points = free.as_ref().map_or(0, geometry::point_count);
    let free_vertices = free.as_ref().map_or(0, geometry::vertex_count);
    let coarse = match (&divided, &unselected) {
        (Some(divided), _) => divided.geometry.clone(),
        (None, Some(unselected)) => unselected.clone(),
        (None, None) => return Err(failm("coarse region is missing")),
    };
    let parts: Vec<geometry::T> = match free {
        None => vec![coarse, refined.clone()],
        Some(free) => vec![coarse, free, refined.clone()],
    };
    let append = match (cracks, &pull_plan, &chains, &divided) {
        (CrackPolicy::SubdivideStitchNoEdgeDivision, Some(plan), _, _) => {
            let Some(unselected) = &unselected else {
                return Err(failm("coarse region is missing"));
            };
            Some(stitch_boundary_no_edge_division(
                cancel_token,
                plan,
                unselected,
                free_points,
                &refined_with_ancestry,
            )?)
        }
        (
            CrackPolicy::SubdivideStitchDivideEdges | CrackPolicy::SubdivideStitchTriangulate,
            Some(plan),
            Some(chains),
            Some(divided),
        ) => Some(stitch_boundary_divide_edges(
            cancel_token,
            consistent_topology,
            plan,
            chains,
            divided,
            free_points,
            free_vertices,
            &refined,
        )?),
        _ => None,
    };
    let point_counts: Vec<i64> = parts.iter().map(geometry::point_count).collect();
    let total_points = checked_total("combined point count", &point_counts)?;
    let weld = match (cracks, &chains, &divided) {
        (
            CrackPolicy::SubdividePullDivideEdges(bias) | CrackPolicy::SubdividePullTriangulate(bias),
            Some(chains),
            Some(divided),
        ) => Some(make_boundary_weld(
            cancel_token,
            bias,
            false,
            chains,
            divided,
            free_points,
            total_points,
            &refined,
        )?),
        (
            CrackPolicy::SubdivideStitchDivideEdges | CrackPolicy::SubdivideStitchTriangulate,
            Some(chains),
            Some(divided),
        ) => {
            if consistent_topology {
                None
            } else {
                Some(make_boundary_weld(
                    cancel_token,
                    0.0,
                    true,
                    chains,
                    divided,
                    free_points,
                    total_points,
                    &refined,
                )?)
            }
        }
        _ => None,
    };
    let output = combine_parts(
        cancel_token,
        grain,
        append.as_ref(),
        weld.as_ref(),
        &prepared,
        &parts,
    )?;
    Ok(if generate_resulting_creases { output } else { without_resulting_creases(&output) })
}

pub fn primitive_kind_counts(geometry_value: &geometry::T) -> Result<(i64, i64), error::T> {
    let topology_view = topology::view::view(geometry::topology(geometry_value))?;
    let mut polygons = 0_i64;
    let mut curves = 0_i64;
    for &kind in topology_view.primitive_kinds.iter() {
        if kind == 0 {
            polygons += 1;
        } else {
            curves += 1;
        }
    }
    Ok((polygons, curves))
}

pub fn internal_attribute_name(geometry_value: &geometry::T, base: &str) -> String {
    let mut suffix = 0_i64;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}_{suffix}") };
        if geometry::find_attribute(Owner::Primitive, &name, geometry_value).is_none() {
            return name;
        }
        suffix += 1;
    }
}

pub fn primitive_kind_group(
    grain: Option<i64>,
    polygon: bool,
    geometry_value: &geometry::T,
) -> Result<group::T, error::T> {
    let topology_view = topology::view::view(geometry::topology(geometry_value))?;
    group::init(
        grain,
        group::Owner::Primitive,
        "__rdk_subdivide_primitive_kind",
        topology_view.primitive_kinds.len() as i64,
        &|primitive| (gu(&topology_view.primitive_kinds, primitive) == 0) == polygon,
    )
}

pub fn extract_kind(
    cancel_arc: &Option<Arc<cancel::T>>,
    grain: Option<i64>,
    polygon: bool,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let selection = primitive_kind_group(grain, polygon, geometry_value)?;
    delete_part(cancel_arc, grain, false, true, &selection, geometry_value)
}

pub fn selection_for_kind(
    grain: Option<i64>,
    polygon: bool,
    selection: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<Option<group::T>, error::T> {
    let Some(selection) = selection else {
        return Ok(None);
    };
    let topology_view = topology::view::view(geometry::topology(geometry_value))?;
    let source_of_output: Vec<i64> = (0..topology_view.primitive_kinds.len() as i64)
        .filter(|&primitive| (gu(&topology_view.primitive_kinds, primitive) == 0) == polygon)
        .collect();
    Ok(Some(group::init(
        grain,
        group::Owner::Primitive,
        "__rdk_subdivide_kind_selection",
        source_of_output.len() as i64,
        &|output| group::mem(gi(&source_of_output, output), selection),
    )?))
}

pub fn refine_curves(
    cancel_arc: &Option<Arc<cancel::T>>,
    grain: Option<i64>,
    selection: Option<&group::T>,
    independent: bool,
    scheme: Scheme,
    iterations: i64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_token = cancel_arc.as_deref();
    let refine = |source: &geometry::T| -> Result<geometry::T, error::T> {
        let curve_scheme = match scheme {
            Scheme::CatmullClark => curve_subdivide::Scheme::CatmullClark,
            Scheme::Bilinear => curve_subdivide::Scheme::Bilinear,
            Scheme::Loop => {
                return Err(failm(
                    "Loop subdivision requires triangle polygons and cannot refine polygon curves",
                ));
            }
        };
        curve_subdivide::iterate(cancel_token, grain, independent, curve_scheme, iterations, source)
    };
    let Some(selection) = selection else {
        return refine(geometry_value);
    };
    validate_primitive_selection(selection, geometry_value)?;
    let selected = group::cardinality(selection);
    if selected == 0 {
        Ok(geometry_value.clone())
    } else if selected == group::length(selection) {
        refine(geometry_value)
    } else {
        let refined_source = delete_part(cancel_arc, grain, false, true, selection, geometry_value)?;
        let retained = delete_part(cancel_arc, grain, true, true, selection, geometry_value)?;
        combine_parts(
            cancel_token,
            grain,
            None,
            None,
            geometry_value,
            &[retained, refine(&refined_source)?],
        )
    }
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub scheme: Scheme,
    pub iterations: i64,
    pub primitives: Option<group::T>,
    pub cracks: CrackPolicy,
    pub consistent_topology: bool,
    pub creases: Option<geometry::T>,
    pub crease_primitives: Option<group::T>,
    pub crease_weight: Option<f64>,
    pub generate_resulting_creases: bool,
    pub resulting_crease_group: Option<String>,
    pub hole_primitives: Option<group::T>,
    pub remove_holes: bool,
    pub boundary_interpolation: BoundaryInterpolation,
    pub face_varying_interpolation: FaceVaryingInterpolation,
    pub triangle_policy: TrianglePolicy,
    pub creasing_method: CreasingMethod,
    pub treat_curves_as_independent: bool,
    pub recompute_point_normals: bool,
}

/// OCaml's `Invalid_argument` caught inside the curve path becomes a plain failure message.
fn invalid_argument_to_failure(result: Result<geometry::T, error::T>) -> Result<geometry::T, error::T> {
    match result {
        Err(failure) if error::code(&failure) == Code::InvalidArgument => {
            Err(fail(error::message(&failure).to_owned()))
        }
        other => other,
    }
}

fn mixed_subdivide(
    params: &Params,
    polygon_count: i64,
    curve_count: i64,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_arc = &params.cancel;
    let cancel_token = cancel_arc.as_deref();
    let grain = params.grain;
    let iterations = params.iterations;
    validate_iterations_and_bias(iterations, params.cracks)?;
    cancel::check_opt(cancel_token)?;
    if let Some(selection) = &params.primitives {
        validate_primitive_selection(selection, geometry_value)?;
    }
    validate_crease_arguments(
        &params.creases,
        &params.crease_primitives,
        params.crease_weight,
        params.generate_resulting_creases,
        &params.resulting_crease_group,
    )?;
    let overrides = detail_overrides(geometry_value);
    let scheme = resolve_scheme(params.scheme, overrides.osd_scheme.as_ref())?;
    let boundary_interpolation = resolve_boundary_interpolation(
        params.boundary_interpolation,
        overrides.osd_vtxboundaryinterpolation.as_ref(),
    )?;
    let face_varying_interpolation = resolve_face_varying_interpolation(
        params.face_varying_interpolation,
        overrides.osd_fvarlinearinterpolation.as_ref(),
    )?;
    let triangle_subdivision =
        resolve_triangle_subdivision(params.triangle_policy, overrides.osd_trianglesubdiv.as_ref())?;
    let creasing_method =
        resolve_creasing_method(params.creasing_method, overrides.osd_creasingmethod.as_ref())?;
    let selected_curve_count = match &params.primitives {
        None => curve_count,
        Some(selection) => {
            let topology_view = topology::view::view(geometry::topology(geometry_value))?;
            let mut count = 0_i64;
            for primitive in 0..topology_view.primitive_kinds.len() as i64 {
                if gu(&topology_view.primitive_kinds, primitive) != 0
                    && group::mem(primitive, selection)
                {
                    count += 1;
                }
            }
            count
        }
    };
    if scheme == Scheme::Loop && selected_curve_count > 0 && iterations > 0 {
        return Err(failm(
            "Loop subdivision requires triangle polygons and cannot refine polygon curves",
        ));
    }
    let nothing_selected = match &params.primitives {
        Some(selection) => group::cardinality(selection) == 0,
        None => false,
    };
    if iterations == 0 || nothing_selected {
        return Ok(geometry_value.clone());
    }
    let prepared = match &params.creases {
        None => geometry_value.clone(),
        Some(creases) => apply_crease_input(
            cancel_token,
            grain,
            params.crease_primitives.as_ref(),
            params.crease_weight,
            geometry_value,
            creases,
        )?,
    };
    let prepared = match &params.hole_primitives {
        None => prepared,
        Some(holes) => {
            validate_hole_group(holes, &prepared)?;
            geometry::with_group(group::with_name("subdivision_hole", holes)?, &prepared)?
        }
    };
    let prepared = match &params.resulting_crease_group {
        Some(name) => with_empty_edge_group(cancel_token, name, &prepared)?,
        None => prepared,
    };
    let all_curves_selected = polygon_count == 0
        && match &params.primitives {
            None => true,
            Some(selection) => group::cardinality(selection) == group::length(selection),
        };
    if all_curves_selected {
        let refine = |source: &geometry::T| {
            refine_curves(
                cancel_arc,
                grain,
                None,
                params.treat_curves_as_independent,
                scheme,
                iterations,
                source,
            )
        };
        let output = if !params.treat_curves_as_independent {
            refine(&prepared)?
        } else {
            match free_point_part(cancel_arc, grain, &prepared)? {
                None => refine(&prepared)?,
                Some(free) => {
                    let curves = extract_kind(cancel_arc, grain, false, &prepared)?;
                    combine_parts(
                        cancel_token,
                        grain,
                        None,
                        None,
                        &prepared,
                        &[refine(&curves)?, free],
                    )?
                }
            }
        };
        return Ok(if params.generate_resulting_creases {
            output
        } else {
            without_resulting_creases(&output)
        });
    }
    let source_order_name = internal_attribute_name(&prepared, "__rdk_subdivide_source_primitive");
    let source_order = attribute::create_owned(
        &source_order_name,
        Owner::Primitive,
        Storage::Int(Arc::new((0..geometry::primitive_count(&prepared)).collect())),
    )?;
    let prepared = geometry::with_attribute(source_order, &prepared)?;
    let mut parts: Vec<geometry::T> = Vec::new();
    if polygon_count > 0 {
        let surface = extract_kind(cancel_arc, grain, true, &prepared)?;
        let surface_selection =
            selection_for_kind(grain, true, params.primitives.as_ref(), &prepared)?;
        let surface_crease_weight =
            if params.creases.is_none() { params.crease_weight } else { None };
        let refined = subdivide_polygons(
            cancel_arc,
            grain,
            &PolygonOptions {
                scheme,
                iterations,
                primitives: surface_selection,
                cracks: params.cracks,
                consistent_topology: params.consistent_topology,
                creases: None,
                crease_primitives: None,
                crease_weight: surface_crease_weight,
                generate_resulting_creases: params.generate_resulting_creases,
                resulting_crease_group: params.resulting_crease_group.clone(),
                hole_primitives: None,
                remove_holes: params.remove_holes,
                boundary_interpolation,
                face_varying_interpolation,
                triangle_subdivision,
                creasing_method,
            },
            &surface,
        )?;
        parts.push(refined);
    }
    let curves = extract_kind(cancel_arc, grain, false, &prepared)?;
    let curve_selection = selection_for_kind(grain, false, params.primitives.as_ref(), &prepared)?;
    let curves = refine_curves(
        cancel_arc,
        grain,
        curve_selection.as_ref(),
        params.treat_curves_as_independent,
        scheme,
        iterations,
        &curves,
    )?;
    parts.push(curves);
    if let Some(free) = free_point_part(cancel_arc, grain, &prepared)? {
        parts.push(free);
    }
    let combined = combine_parts(cancel_token, grain, None, None, &prepared, &parts)?;
    let ordered = ordering::sort_primitives(cancel_token, grain, &source_order_name, &combined)?;
    let output = geometry::without_attribute(Owner::Primitive, &source_order_name, &ordered);
    Ok(if params.generate_resulting_creases { output } else { without_resulting_creases(&output) })
}

/// The `Error.guard ~operation:"subdivide" ~code:"invalid_topology"` boundary.
fn guard(result: Result<geometry::T, error::T>) -> Result<geometry::T, error::T> {
    match result {
        Ok(value) => Ok(value),
        // OCaml lets Invalid_argument escape the guard as an exception.
        Err(failure) if error::code(&failure) == Code::InvalidArgument => {
            Err(error::new("subdivide", Code::InvalidArgument, error::message(&failure).to_owned()))
        }
        Err(failure) => Err(error::guard(failure, "subdivide", Code::InvalidTopology)),
    }
}

pub fn subdivide(
    params: &Params,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let work = || -> Result<geometry::T, error::T> {
        let had_point_normals =
            geometry::find_attribute(Owner::Point, "N", geometry_value).is_some();
        let (polygon_count, curve_count) = primitive_kind_counts(geometry_value)?;
        let result = if curve_count == 0 {
            subdivide_polygons(
                &params.cancel,
                params.grain,
                &PolygonOptions {
                    scheme: params.scheme,
                    iterations: params.iterations,
                    primitives: params.primitives.clone(),
                    cracks: params.cracks,
                    consistent_topology: params.consistent_topology,
                    creases: params.creases.clone(),
                    crease_primitives: params.crease_primitives.clone(),
                    crease_weight: params.crease_weight,
                    generate_resulting_creases: params.generate_resulting_creases,
                    resulting_crease_group: params.resulting_crease_group.clone(),
                    hole_primitives: params.hole_primitives.clone(),
                    remove_holes: params.remove_holes,
                    boundary_interpolation: params.boundary_interpolation,
                    face_varying_interpolation: params.face_varying_interpolation,
                    triangle_subdivision: params.triangle_policy,
                    creasing_method: params.creasing_method,
                },
                geometry_value,
            )?
        } else {
            match invalid_argument_to_failure(mixed_subdivide(
                params,
                polygon_count,
                curve_count,
                geometry_value,
            )) {
                Ok(value) => value,
                Err(failure) => return Err(failure),
            }
        };
        if !params.recompute_point_normals || !had_point_normals {
            return Ok(result);
        }
        deform::normals(
            deform::Params {
                cancel: params.cancel.clone(),
                grain: params.grain.unwrap_or(16_384),
                primitives: None,
            },
            &result,
        )
        .map_err(|failure| fail(error::message(&failure).to_owned()))
    };
    guard(work())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::packed;
    use std::sync::Mutex;

    fn defaults() -> Params {
        Params {
            cancel: None,
            grain: None,
            scheme: Scheme::CatmullClark,
            iterations: 1,
            primitives: None,
            cracks: CrackPolicy::SubdivideDoNotClose,
            consistent_topology: false,
            creases: None,
            crease_primitives: None,
            crease_weight: None,
            generate_resulting_creases: true,
            resulting_crease_group: None,
            hole_primitives: None,
            remove_holes: true,
            boundary_interpolation: SubdivideBoundaryEdgeOnly,
            face_varying_interpolation: SubdivideFvarAll,
            triangle_policy: SubdivideTrianglesCatmullClark,
            creasing_method: SubdivideCreasingUniform,
            treat_curves_as_independent: false,
            recompute_point_normals: false,
        }
    }

    fn sub(source: &geometry::T, configure: &dyn Fn(&mut Params)) -> Result<geometry::T, error::T> {
        let mut params = defaults();
        configure(&mut params);
        subdivide(&params, source)
    }

    fn ok(source: &geometry::T, configure: &dyn Fn(&mut Params)) -> geometry::T {
        sub(source, configure).unwrap_or_else(|failure| panic!("{}", error::to_string(&failure)))
    }

    fn with_domains(domains: i64, work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
        let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            *slot.lock().unwrap() = Some(work());
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }

    fn polygons(point_count: i64, vertex_points: Vec<i64>, offsets: Vec<i64>) -> topology::T {
        topology::polygons_owned(point_count, vertex_points, offsets).unwrap()
    }

    fn make(
        x: Vec<f64>,
        y: Vec<f64>,
        z: Vec<f64>,
        topology_value: topology::T,
        attributes: Vec<attribute::T>,
    ) -> geometry::T {
        geometry::create(
            packed::float3::view::of_owned_exn(x, y, z).unwrap(),
            topology_value,
            Some(attributes),
            None,
            None,
        )
        .unwrap()
    }

    fn attr(owner: Owner, name: &str, storage: Storage) -> attribute::T {
        attribute::create_owned(name, owner, storage).unwrap()
    }

    fn float_attr(owner: Owner, name: &str, values: Vec<f64>) -> attribute::T {
        attr(owner, name, Storage::Float(Arc::new(values)))
    }

    fn quad(
        vertex_weights: Option<Vec<f64>>,
        primitive_weights: Option<Vec<f64>>,
        corner_weights: Option<Vec<f64>>,
    ) -> geometry::T {
        let mut attributes = Vec::new();
        if let Some(values) = vertex_weights {
            attributes.push(float_attr(Owner::Vertex, "creaseweight", values));
        }
        if let Some(values) = primitive_weights {
            attributes.push(float_attr(Owner::Primitive, "creaseweight", values));
        }
        if let Some(values) = corner_weights {
            attributes.push(float_attr(Owner::Point, "cornerweight", values));
        }
        make(
            vec![0.0, 2.0, 2.0, 0.0],
            vec![0.0, 0.0, 2.0, 2.0],
            vec![0.0; 4],
            polygons(4, vec![0, 1, 2, 3], vec![0, 4]),
            attributes,
        )
    }

    /// A flat grid of `columns` x `rows` cells in the xz plane, row-major points.
    fn grid(columns: i64, rows: i64, size: f64, triangles: bool) -> geometry::T {
        let mut x = Vec::new();
        let mut z = Vec::new();
        for row in 0..=rows {
            for column in 0..=columns {
                x.push(-size / 2.0 + size * (column as f64) / (columns as f64));
                z.push(-size / 2.0 + size * (row as f64) / (rows as f64));
            }
        }
        let stride = columns + 1;
        let mut vertex_points = Vec::new();
        let mut offsets = vec![0];
        for row in 0..rows {
            for column in 0..columns {
                let p00 = row * stride + column;
                let p01 = p00 + 1;
                let p10 = p00 + stride;
                let p11 = p10 + 1;
                if triangles {
                    vertex_points.extend([p00, p01, p11, p00, p11, p10]);
                    offsets.push(vertex_points.len() as i64 - 3);
                    offsets.push(vertex_points.len() as i64);
                } else {
                    vertex_points.extend([p00, p01, p11, p10]);
                    offsets.push(vertex_points.len() as i64);
                }
            }
        }
        let points = x.len();
        make(
            x,
            vec![0.0; points],
            z,
            polygons(points as i64, vertex_points, offsets),
            Vec::new(),
        )
    }

    fn floats(owner: Owner, name: &str, geometry_value: &geometry::T) -> Vec<f64> {
        let found = geometry::find_attribute(owner, name, geometry_value)
            .unwrap_or_else(|| panic!("missing float attribute {name}"));
        match attribute::view::storage(&found).unwrap() {
            Storage::Float(values) => values.to_vec(),
            _ => panic!("wrong float storage for {name}"),
        }
    }

    fn ints(owner: Owner, name: &str, geometry_value: &geometry::T) -> Vec<i64> {
        let found = geometry::find_attribute(owner, name, geometry_value)
            .unwrap_or_else(|| panic!("missing integer attribute {name}"));
        match attribute::view::storage(&found).unwrap() {
            Storage::Int(values) => values.to_vec(),
            _ => panic!("wrong integer storage for {name}"),
        }
    }

    fn positions(geometry_value: &geometry::T) -> packed::float3::view::View {
        packed::float3::view::view(geometry::positions(geometry_value)).unwrap()
    }

    fn bits_eq(left: &[f64], right: &[f64]) -> bool {
        left.len() == right.len() && left.iter().zip(right).all(|(a, b)| a.to_bits() == b.to_bits())
    }

    fn storage_eq(left: &Storage, right: &Storage) -> bool {
        match (left, right) {
            (Storage::Float(a), Storage::Float(b)) => bits_eq(a, b),
            (Storage::Int(a), Storage::Int(b)) => a == b,
            (Storage::Text(a), Storage::Text(b)) => a == b,
            (Storage::Float2(a), Storage::Float2(b)) => {
                let (a, b) = (packed::float2::view::view(a), packed::float2::view::view(b));
                bits_eq(&a.x, &b.x) && bits_eq(&a.y, &b.y)
            }
            (Storage::Float3(a), Storage::Float3(b)) => {
                let (a, b) = (
                    packed::float3::view::view(a).unwrap(),
                    packed::float3::view::view(b).unwrap(),
                );
                bits_eq(&a.x, &b.x) && bits_eq(&a.y, &b.y) && bits_eq(&a.z, &b.z)
            }
            (Storage::Float4(a), Storage::Float4(b)) => {
                let (a, b) = (packed::float4::view::view(a), packed::float4::view::view(b));
                bits_eq(&a.x, &b.x) && bits_eq(&a.y, &b.y) && bits_eq(&a.z, &b.z) && bits_eq(&a.w, &b.w)
            }
            (Storage::IntArray(a), Storage::IntArray(b)) => {
                let (a, b) = (packed::int_array::view::view(a), packed::int_array::view::view(b));
                a.offsets == b.offsets && a.values == b.values
            }
            (Storage::FloatArray(a), Storage::FloatArray(b)) => {
                let (a, b) = (packed::float_array::view::view(a), packed::float_array::view::view(b));
                a.offsets == b.offsets && bits_eq(&a.values, &b.values)
            }
            _ => false,
        }
    }

    fn equal_geometry(left: &geometry::T, right: &geometry::T) -> bool {
        let (lt, rt) = (
            topology::view::view(geometry::topology(left)).unwrap(),
            topology::view::view(geometry::topology(right)).unwrap(),
        );
        if lt.point_count != rt.point_count
            || lt.vertex_points != rt.vertex_points
            || lt.primitive_offsets != rt.primitive_offsets
            || lt.primitive_kinds != rt.primitive_kinds
        {
            return false;
        }
        let (lp, rp) = (positions(left), positions(right));
        if !(bits_eq(&lp.x, &rp.x) && bits_eq(&lp.y, &rp.y) && bits_eq(&lp.z, &rp.z)) {
            return false;
        }
        let (la, ra) = (geometry::attributes(left), geometry::attributes(right));
        if la.len() != ra.len() {
            return false;
        }
        for (a, b) in la.iter().zip(ra.iter()) {
            if attribute::name(a) != attribute::name(b)
                || attribute::owner(a) != attribute::owner(b)
                || !storage_eq(attribute::view::storage(a).unwrap(), attribute::view::storage(b).unwrap())
            {
                return false;
            }
        }
        let (lg, rg) = (geometry::groups(left), geometry::groups(right));
        if lg.len() != rg.len() {
            return false;
        }
        for (a, b) in lg.iter().zip(rg.iter()) {
            if group::name(a) != group::name(b)
                || group::owner(a) != group::owner(b)
                || group::view::bits_view(a) != group::view::bits_view(b)
                || group::ordered_elements(a) != group::ordered_elements(b)
            {
                return false;
            }
        }
        let (le, re) = (geometry::edge_groups(left), geometry::edge_groups(right));
        le.len() == re.len()
            && le.iter().zip(re.iter()).all(|(a, b)| {
                edge_group::name(a) == edge_group::name(b)
                    && edge_group::length(a) == edge_group::length(b)
                    && collect_edge_members(a) == collect_edge_members(b)
            })
    }

    fn close(left: f64, right: f64) -> bool {
        (left - right).abs() <= 1e-12
    }

    fn edge_weight(geometry_value: &geometry::T, a: i64, b: i64) -> f64 {
        let Some(found) = geometry::find_attribute(Owner::Vertex, "creaseweight", geometry_value)
        else {
            return 0.0;
        };
        let Storage::Float(values) = attribute::view::storage(&found).unwrap() else {
            panic!("creaseweight storage changed");
        };
        let index = topology_index::create(None, geometry::topology(geometry_value)).unwrap();
        let view = topology_index::view::view(&index);
        let edge = topology_index::find_edge(&index, a, b).unwrap();
        let mut result = 0.0_f64;
        for at in view.edge_offsets[edge as usize]..view.edge_offsets[edge as usize + 1] {
            result = fmax(result, values[view.edge_vertices[at as usize] as usize]);
        }
        result
    }

    fn primitive_group(name: &str, length: i64, members: &[i64]) -> group::T {
        group::ordered(group::Owner::Primitive, name, length, members).unwrap()
    }

    #[test]
    fn cardinality_guards_are_native_i64() {
        for (a, b, expected) in [
            (1 << 62, 1, Some((1 << 62) + 1)), (i64::MAX, 0, Some(i64::MAX)),
            (i64::MAX, 1, None), (i64::MIN, -1, None), (-1, 1, None), (1, -1, None),
        ] { assert_eq!(checked_add("count", a, b).ok(), expected); }
        for (a, b, expected) in [
            (1 << 61, 2, Some(1 << 62)), (i64::MAX, 0, Some(0)),
            (i64::MAX, 2, None), (-1, 0, None), (0, -1, None),
        ] { assert_eq!(checked_mul("count", a, b).ok(), expected); }
    }

    #[test]
    fn checked_loop_quad_error() {
        match sub(&quad(None, None, None), &|p| p.scheme = Scheme::Loop) {
            Err(failure) if error::code(&failure) == Code::InvalidTopology => {}
            _ => panic!("Loop Subdivide accepted a quad"),
        }
    }

    #[test]
    fn selected_loop_validation_scope() {
        let source = make(
            vec![0.0, 1.0, 0.0, 3.0, 4.0, 4.0, 3.0],
            vec![0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0],
            vec![0.0; 7],
            polygons(7, vec![0, 1, 2, 3, 4, 5, 6], vec![0, 3, 7]),
            Vec::new(),
        );
        let triangle = group::init(None, group::Owner::Primitive, "triangle", 2, &|p| p == 0).unwrap();
        let output = ok(&source, &|p| {
            p.scheme = Scheme::Loop;
            p.primitives = Some(triangle.clone());
        });
        assert!(
            geometry::primitive_count(&output) == 5
                && topology::primitive_size(geometry::topology(&output), 0).unwrap() == 4,
            "local Loop validated or changed an unselected quad"
        );
    }

    #[test]
    fn disconnected_selected_fans() {
        let source = make(
            vec![0.0, 1.0, 2.0, 0.0, 1.0, 2.0, 0.0, 1.0, 2.0],
            vec![0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0],
            vec![0.0; 9],
            polygons(
                9,
                vec![0, 3, 4, 1, 1, 4, 5, 2, 3, 6, 7, 4, 4, 7, 8, 5],
                vec![0, 4, 8, 12, 16],
            ),
            vec![attr(Owner::Point, "point_id", Storage::Int(Arc::new((0..9).map(|p| 100 + p).collect())))],
        );
        let diagonal = primitive_group("diagonal", 4, &[0, 3]);
        let run = |domains: i64| {
            with_domains(domains, &|| {
                ok(&source, &|p| {
                    p.grain = Some(1);
                    p.primitives = Some(diagonal.clone());
                })
            })
        };
        let one = run(1);
        let four = run(4);
        assert!(equal_geometry(&one, &four), "disconnected selected-fan splitting differs across domains");
        assert!(
            geometry::point_count(&one) == 25
                && geometry::vertex_count(&one) == 40
                && geometry::primitive_count(&one) == 10,
            "disconnected selected-fan splitting cardinality"
        );
        let ids = ints(Owner::Point, "point_id", &one);
        let center_copies = ids[..15].iter().filter(|&&value| value == 104).count();
        assert_eq!(center_copies, 3, "disconnected selected-fan splitting lost point ancestry");
    }

    #[test]
    fn chaikin_creasing() {
        let weights = [(1_i64, 4.0_f64), (3, 2.0), (5, 8.0), (7, 0.5)];
        let make_input = |triangles: bool| {
            let base = grid(2, 2, 2.0, triangles);
            let index = topology_index::view::view(
                &topology_index::create(None, geometry::topology(&base)).unwrap(),
            );
            let creaseweight: Vec<f64> = (0..geometry::vertex_count(&base))
                .map(|vertex| {
                    let edge = index.edge_of_vertex[vertex as usize];
                    if edge < 0 {
                        return 0.0;
                    }
                    let a = index.edge_a[edge as usize];
                    let b = index.edge_b[edge as usize];
                    let neighbor = if a == 4 {
                        Some(b)
                    } else if b == 4 {
                        Some(a)
                    } else {
                        None
                    };
                    match neighbor {
                        None => 0.0,
                        Some(neighbor) => weights
                            .iter()
                            .find(|(point, _)| *point == neighbor)
                            .map_or(0.0, |(_, weight)| *weight),
                    }
                })
                .collect();
            geometry::with_attribute(float_attr(Owner::Vertex, "creaseweight", creaseweight), &base)
                .unwrap()
        };
        let input = make_input(false);
        let source_index = topology_index::create(None, geometry::topology(&input)).unwrap();
        let edge = |neighbor: i64| topology_index::find_edge(&source_index, 4, neighbor).unwrap();
        let uniform = ok(&input, &|p| {
            p.grain = Some(1);
            p.creasing_method = SubdivideCreasingUniform;
            p.resulting_crease_group = Some("remaining".to_owned());
        });
        let chaikin = ok(&input, &|p| {
            p.grain = Some(1);
            p.creasing_method = SubdivideCreasingChaikin;
            p.resulting_crease_group = Some("remaining".to_owned());
        });
        assert!(
            equal_geometry(
                &uniform,
                &ok(&input, &|p| {
                    p.grain = Some(1);
                    p.resulting_crease_group = Some("remaining".to_owned());
                })
            ),
            "uniform creasing is not the compatibility default"
        );
        let midpoint = |neighbor: i64| geometry::point_count(&input) + edge(neighbor);
        assert!(
            close(edge_weight(&uniform, 4, midpoint(1)), 3.0)
                && close(edge_weight(&uniform, midpoint(1), 1), 3.0),
            "uniform creasing did not decrement both child edges equally"
        );
        assert!(
            close(edge_weight(&chaikin, 4, midpoint(1)), 2.875)
                && close(edge_weight(&chaikin, midpoint(1), 1), 3.0)
                && close(edge_weight(&chaikin, 4, midpoint(3)), 37.0 / 24.0)
                && close(edge_weight(&chaikin, midpoint(3), 3), 1.0)
                && close(edge_weight(&chaikin, 4, midpoint(5)), 133.0 / 24.0)
                && close(edge_weight(&chaikin, midpoint(5), 5), 7.0)
                && close(edge_weight(&chaikin, 4, midpoint(7)), 13.0 / 24.0)
                && close(edge_weight(&chaikin, midpoint(7), 7), 0.0),
            "Chaikin child sharpness did not use the exact endpoint neighborhood mean"
        );
        let uniform_group = geometry::find_edge_group("remaining", &uniform).unwrap();
        let chaikin_group = geometry::find_edge_group("remaining", &chaikin).unwrap();
        assert!(
            edge_group::cardinality(&uniform_group) == 6
                && edge_group::cardinality(&chaikin_group) == 7,
            "resulting crease group did not classify endpoint-dependent Chaikin children"
        );
        let chaikin_index = topology_index::create(None, geometry::topology(&chaikin)).unwrap();
        let lifted = topology_index::find_edge(&chaikin_index, 4, midpoint(7)).unwrap();
        let decayed = topology_index::find_edge(&chaikin_index, midpoint(7), 7).unwrap();
        assert!(
            edge_group::mem(lifted, &chaikin_group) && !edge_group::mem(decayed, &chaikin_group),
            "Chaikin resulting group lost its asymmetric child-edge membership"
        );

        let mask_base = grid(3, 2, 3.0, false);
        let mask_z: Vec<f64> = (0..geometry::point_count(&mask_base))
            .map(|point| (((point * point) + (3 * point)) % 11) as f64 * 0.17)
            .collect();
        let mask_positions = positions(&mask_base);
        let mask_base = geometry::with_positions(
            packed::float3::view::of_owned_exn(
                mask_positions.x.to_vec(),
                mask_positions.y.to_vec(),
                mask_z,
            )
            .unwrap(),
            &mask_base,
        )
        .unwrap();
        let mask_index_value = topology_index::create(None, geometry::topology(&mask_base)).unwrap();
        let mask_index = topology_index::view::view(&mask_index_value);
        let target_edge = topology_index::find_edge(&mask_index_value, 5, 6).unwrap();
        let high_left = topology_index::find_edge(&mask_index_value, 1, 5).unwrap();
        let high_right = topology_index::find_edge(&mask_index_value, 6, 10).unwrap();
        let mask_weights: Vec<f64> = (0..geometry::vertex_count(&mask_base))
            .map(|vertex| {
                let edge = mask_index.edge_of_vertex[vertex as usize];
                if edge == target_edge {
                    0.5
                } else if edge == high_left || edge == high_right {
                    8.0
                } else {
                    0.0
                }
            })
            .collect();
        let mask_input = geometry::with_attribute(
            float_attr(Owner::Vertex, "creaseweight", mask_weights),
            &mask_base,
        )
        .unwrap();
        let mask_topology = topology::view::view(geometry::topology(&mask_input)).unwrap();
        let mask_z_values = positions(&mask_input).z.to_vec();
        let mask_fvar = float_attr(
            Owner::Vertex,
            "fvar",
            mask_topology.vertex_points.iter().map(|&point| mask_z_values[point as usize]).collect(),
        );
        let mask_input = geometry::with_attribute(mask_fvar, &mask_input).unwrap();
        let mask_uniform = ok(&mask_input, &|p| {
            p.grain = Some(1);
            p.creasing_method = SubdivideCreasingUniform;
            p.face_varying_interpolation = SubdivideFvarNone;
        });
        let mask_chaikin = ok(&mask_input, &|p| {
            p.grain = Some(1);
            p.creasing_method = SubdivideCreasingChaikin;
            p.face_varying_interpolation = SubdivideFvarNone;
        });
        let source_positions = positions(&mask_input);
        let uniform_positions = positions(&mask_uniform);
        let chaikin_positions = positions(&mask_chaikin);
        let target_point = geometry::point_count(&mask_input) + target_edge;
        let midpoint_z = (source_positions.z[5] + source_positions.z[6]) * 0.5;
        assert!(
            close(chaikin_positions.z[target_point as usize], midpoint_z)
                && !close(uniform_positions.z[target_point as usize], midpoint_z),
            "Chaikin did not keep a sub-unit edge fully creased when both children survive"
        );
        let fvar_at = |geometry_value: &geometry::T, point: i64| {
            let topology_view = topology::view::view(geometry::topology(geometry_value)).unwrap();
            let values = floats(Owner::Vertex, "fvar", geometry_value);
            let vertex = topology_view.vertex_points.iter().position(|&p| p == point).unwrap();
            values[vertex]
        };
        assert!(
            close(fvar_at(&mask_chaikin, target_point), midpoint_z)
                && !close(fvar_at(&mask_uniform, target_point), midpoint_z),
            "Chaikin edge rules did not refine continuous face-varying data"
        );
        let crease_vertex_z = (source_positions.z[5] * 0.75)
            + ((source_positions.z[1] + source_positions.z[6]) * 0.125);
        assert!(
            close(chaikin_positions.z[5], crease_vertex_z)
                && !close(uniform_positions.z[5], crease_vertex_z),
            "Chaikin vertex rule did not follow its surviving child crease neighborhood"
        );

        let exact = |domains: i64, method: CreasingMethod| {
            with_domains(domains, &|| {
                ok(&make_input(false), &|p| {
                    p.grain = Some(1);
                    p.iterations = 2;
                    p.creasing_method = method;
                    p.resulting_crease_group = Some("remaining".to_owned());
                })
            })
        };
        let exact_one = exact(1, SubdivideCreasingChaikin);
        let exact_four = exact(4, SubdivideCreasingChaikin);
        assert!(
            equal_geometry(&exact_one, &exact_four),
            "recursive Chaikin creasing differs across one and four domains"
        );
        assert!(
            !equal_geometry(&exact_one, &exact(1, SubdivideCreasingUniform)),
            "recursive Chaikin creasing collapsed to uniform decay"
        );

        let clean = grid(2, 2, 2.0, false);
        let second_input = ok(&clean, &|p| {
            p.grain = Some(1);
            p.creases = Some(input.clone());
            p.creasing_method = SubdivideCreasingChaikin;
            p.resulting_crease_group = Some("remaining".to_owned());
        });
        assert!(
            equal_geometry(&chaikin, &second_input),
            "second-input Chaikin creases differ from source-attribute creases"
        );

        let selection = primitive_group("refine", geometry::primitive_count(&input), &[0, 1]);
        let local_input = geometry::with_group(selection.clone(), &input).unwrap();
        let local = |domains: i64| {
            with_domains(domains, &|| {
                ok(&local_input, &|p| {
                    p.grain = Some(1);
                    p.primitives = Some(selection.clone());
                    p.creasing_method = SubdivideCreasingChaikin;
                })
            })
        };
        assert!(equal_geometry(&local(1), &local(4)), "local Chaikin creasing differs across one and four domains");

        let loop_run = |domains: i64| {
            with_domains(domains, &|| {
                ok(&make_input(true), &|p| {
                    p.grain = Some(1);
                    p.scheme = Scheme::Loop;
                    p.creasing_method = SubdivideCreasingChaikin;
                })
            })
        };
        assert!(equal_geometry(&loop_run(1), &loop_run(4)), "Loop Chaikin creasing differs across one and four domains");

        let corner_only = quad(None, None, Some(vec![2.0, 0.0, 0.0, 0.0]));
        let corner_uniform = ok(&corner_only, &|p| p.creasing_method = SubdivideCreasingUniform);
        let corner_chaikin = ok(&corner_only, &|p| p.creasing_method = SubdivideCreasingChaikin);
        assert!(
            equal_geometry(&corner_uniform, &corner_chaikin),
            "Chaikin edge policy changed uniform corner sharpness decay"
        );
    }

    #[test]
    fn point_boundary_interpolation() {
        let input = geometry::with_attribute(
            float_attr(Owner::Point, "sample", vec![0.0, 2.0, 4.0, 6.0]),
            &quad(None, None, None),
        )
        .unwrap();
        let edge_only = ok(&input, &|p| p.boundary_interpolation = SubdivideBoundaryEdgeOnly);
        let edge_and_corner =
            ok(&input, &|p| p.boundary_interpolation = SubdivideBoundaryEdgeAndCorner);
        let (ep, cp) = (positions(&edge_only), positions(&edge_and_corner));
        assert!(ep.x[0] == 0.25 && ep.y[0] == 0.25, "Edge Only did not apply the smooth boundary-curve stencil");
        assert!(cp.x[0] == 0.0 && cp.y[0] == 0.0, "Edge and Corner did not pin a one-face boundary vertex");
        assert!(floats(Owner::Point, "sample", &edge_only)[0] == 1.0);
        assert!(floats(Owner::Point, "sample", &edge_and_corner)[0] == 0.0);

        let none = ok(&input, &|p| p.boundary_interpolation = SubdivideBoundaryNone);
        assert!(
            geometry::point_count(&none) == 9
                && geometry::vertex_count(&none) == 0
                && geometry::primitive_count(&none) == 0,
            "None did not turn an open single-face boundary into a hole"
        );
        let holes = geometry::find_group(group::Owner::Primitive, "subdivision_hole", &none).unwrap();
        assert!(group::length(&holes) == 0 && group::cardinality(&holes) == 0);
        let retained = ok(&input, &|p| {
            p.remove_holes = false;
            p.boundary_interpolation = SubdivideBoundaryNone;
        });
        let retained_holes =
            geometry::find_group(group::Owner::Primitive, "subdivision_hole", &retained).unwrap();
        assert!(
            geometry::primitive_count(&retained) == 4 && group::cardinality(&retained_holes) == 4,
            "None with Remove Holes disabled did not retain propagated boundary holes"
        );

        for policy in [SubdivideBoundaryNone, SubdivideBoundaryEdgeOnly, SubdivideBoundaryEdgeAndCorner] {
            let run = |domains: i64| {
                with_domains(domains, &|| {
                    ok(&grid(3, 3, 3.0, false), &|p| {
                        p.grain = Some(1);
                        p.boundary_interpolation = policy;
                    })
                })
            };
            assert!(equal_geometry(&run(1), &run(4)), "point-boundary interpolation differs between one and four domains");
        }
        let center_only = ok(&grid(3, 3, 3.0, false), &|p| {
            p.grain = Some(1);
            p.boundary_interpolation = SubdivideBoundaryNone;
        });
        assert_eq!(geometry::primitive_count(&center_only), 4);
        let recursive = ok(&grid(3, 3, 3.0, false), &|p| {
            p.iterations = 2;
            p.boundary_interpolation = SubdivideBoundaryNone;
        });
        assert_eq!(geometry::primitive_count(&recursive), 16);
        let with_center_hole =
            geometry::with_group(primitive_group("subdivision_hole", 9, &[4]), &grid(3, 3, 3.0, false))
                .unwrap();
        let all_holes = ok(&with_center_hole, &|p| p.boundary_interpolation = SubdivideBoundaryNone);
        assert_eq!(geometry::primitive_count(&all_holes), 0);
        let local_source =
            geometry::with_group(primitive_group("center", 9, &[4]), &grid(3, 3, 3.0, false)).unwrap();
        let center = geometry::find_group(group::Owner::Primitive, "center", &local_source).unwrap();
        let local = ok(&local_source, &|p| {
            p.grain = Some(1);
            p.primitives = Some(center.clone());
            p.boundary_interpolation = SubdivideBoundaryNone;
        });
        assert_eq!(geometry::primitive_count(&local), 4);

        let strip = grid(2, 1, 2.0, false);
        let strip_edge = ok(&strip, &|p| p.boundary_interpolation = SubdivideBoundaryEdgeOnly);
        let strip_corners = ok(&strip, &|p| p.boundary_interpolation = SubdivideBoundaryEdgeAndCorner);
        let (sp, sc) = (positions(&strip_edge), positions(&strip_corners));
        assert!(
            sp.x[1] == sc.x[1] && sp.y[1] == sc.y[1] && sp.z[1] == sc.z[1],
            "Edge and Corner pinned a two-face boundary vertex"
        );

        let triangle = make(
            vec![0.0, 2.0, 0.0],
            vec![0.0, 0.0, 2.0],
            vec![0.0; 3],
            polygons(3, vec![0, 1, 2], vec![0, 3]),
            Vec::new(),
        );
        let loop_edge = ok(&triangle, &|p| {
            p.scheme = Scheme::Loop;
            p.boundary_interpolation = SubdivideBoundaryEdgeOnly;
        });
        let loop_corners = ok(&triangle, &|p| {
            p.scheme = Scheme::Loop;
            p.boundary_interpolation = SubdivideBoundaryEdgeAndCorner;
        });
        let loop_none = ok(&triangle, &|p| {
            p.scheme = Scheme::Loop;
            p.boundary_interpolation = SubdivideBoundaryNone;
        });
        let (le, lc) = (positions(&loop_edge), positions(&loop_corners));
        assert!(
            le.x[0] == 0.25 && le.y[0] == 0.25 && lc.x[0] == 0.0 && lc.y[0] == 0.0,
            "Loop point-boundary policies did not use their documented stencils"
        );
        assert!(
            geometry::point_count(&loop_none) == 6 && geometry::primitive_count(&loop_none) == 0,
            "Loop None did not classify the open triangle as a hole"
        );

        // A closed cube is untouched by the point-boundary policies.
        let mut cx = Vec::new();
        let mut cy = Vec::new();
        let mut cz = Vec::new();
        for point in 0..8_i64 {
            cx.push(if point & 1 == 0 { -1.0 } else { 1.0 });
            cy.push(if point & 2 == 0 { -1.0 } else { 1.0 });
            cz.push(if point & 4 == 0 { -1.0 } else { 1.0 });
        }
        let cube = make(
            cx,
            cy,
            cz,
            polygons(
                8,
                vec![
                    0, 2, 3, 1, 4, 5, 7, 6, 0, 1, 5, 4, 2, 6, 7, 3, 0, 4, 6, 2, 1, 3, 7, 5,
                ],
                vec![0, 4, 8, 12, 16, 20, 24],
            ),
            Vec::new(),
        );
        let closed_edge = ok(&cube, &|p| p.boundary_interpolation = SubdivideBoundaryEdgeOnly);
        let closed_corner = ok(&cube, &|p| p.boundary_interpolation = SubdivideBoundaryEdgeAndCorner);
        let closed_none = ok(&cube, &|p| p.boundary_interpolation = SubdivideBoundaryNone);
        assert!(
            equal_geometry(&closed_edge, &closed_corner) && equal_geometry(&closed_edge, &closed_none),
            "point-boundary policy changed a closed manifold"
        );
        let bilinear_none = ok(&input, &|p| {
            p.scheme = Scheme::Bilinear;
            p.boundary_interpolation = SubdivideBoundaryNone;
        });
        let bilinear_edge = ok(&input, &|p| {
            p.scheme = Scheme::Bilinear;
            p.boundary_interpolation = SubdivideBoundaryEdgeOnly;
        });
        assert!(equal_geometry(&bilinear_none, &bilinear_edge));

        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        match sub(&grid(3, 3, 3.0, false), &|p| {
            p.cancel = Some(token.clone());
            p.boundary_interpolation = SubdivideBoundaryNone;
        }) {
            Err(failure) if error::is_cancelled(&failure) => {}
            _ => panic!("cancelled boundary-hole classification published geometry"),
        }
    }

    #[test]
    fn triangle_subdivision_policy() {
        let build = |vertex_points: Vec<i64>, offsets: Vec<i64>, values: Vec<f64>| {
            let count = values.len();
            let topology_value = polygons(count as i64, vertex_points, offsets);
            let view = topology::view::view(&topology_value).unwrap();
            let fvar: Vec<f64> = view.vertex_points.iter().map(|&p| values[p as usize]).collect();
            make(
                values.clone(),
                vec![0.0; count],
                vec![0.0; count],
                topology_value,
                vec![float_attr(Owner::Point, "value", values), float_attr(Owner::Vertex, "fvar", fvar)],
            )
        };
        let edge_point = |geometry_value: &geometry::T, a: i64, b: i64| {
            let index = topology_index::create(None, geometry::topology(geometry_value)).unwrap();
            geometry::point_count(geometry_value) + topology_index::find_edge(&index, a, b).unwrap()
        };
        let triangles = build(vec![0, 1, 2, 1, 0, 3], vec![0, 3, 6], vec![2.0, 6.0, 10.0, 14.0]);
        let shared = edge_point(&triangles, 0, 1);
        let standard = ok(&triangles, &|p| {
            p.grain = Some(1);
            p.face_varying_interpolation = SubdivideFvarNone;
            p.triangle_policy = SubdivideTrianglesCatmullClark;
        });
        let smooth = ok(&triangles, &|p| {
            p.grain = Some(1);
            p.face_varying_interpolation = SubdivideFvarNone;
            p.triangle_policy = SubdivideTrianglesSmooth;
        });
        let face0 = (2.0 + 6.0 + 10.0) / 3.0;
        let face1 = (6.0 + 2.0 + 14.0) / 3.0;
        let expected_standard = 0.25 * (2.0 + 6.0 + face0 + face1);
        let expected_smooth = (0.03 * (2.0 + 6.0)) + (0.470 * (face0 + face1));
        let standard_x = positions(&standard).x[shared as usize];
        let smooth_x = positions(&smooth).x[shared as usize];
        assert!(
            close(standard_x, expected_standard) && close(smooth_x, expected_smooth),
            "Smooth Triangles did not apply Pixar's two-triangle edge mask"
        );
        assert!(
            close(floats(Owner::Point, "value", &standard)[shared as usize], expected_standard)
                && close(floats(Owner::Point, "value", &smooth)[shared as usize], expected_smooth)
        );
        let fvar_at_shared = |geometry_value: &geometry::T| {
            let topology_view = topology::view::view(geometry::topology(geometry_value)).unwrap();
            let values = floats(Owner::Vertex, "fvar", geometry_value);
            values[topology_view.vertex_points.iter().position(|&p| p == shared).unwrap()]
        };
        assert!(
            close(fvar_at_shared(&standard), expected_standard)
                && close(fvar_at_shared(&smooth), expected_smooth),
            "Smooth Triangles did not apply to smoothly refined face-varying data"
        );
        assert!(equal_geometry(
            &standard,
            &ok(&triangles, &|p| {
                p.grain = Some(1);
                p.face_varying_interpolation = SubdivideFvarNone;
            })
        ));

        let mixed = build(
            vec![0, 1, 2, 1, 0, 3, 4],
            vec![0, 3, 7],
            vec![2.0, 6.0, 10.0, 14.0, 18.0],
        );
        let mixed_shared = edge_point(&mixed, 0, 1);
        let mixed_smooth = ok(&mixed, &|p| {
            p.grain = Some(1);
            p.triangle_policy = SubdivideTrianglesSmooth;
        });
        let triangle_face = (2.0 + 6.0 + 10.0) / 3.0;
        let quad_face = (6.0 + 2.0 + 14.0 + 18.0) / 4.0;
        let expected_mixed = (0.14 * (2.0 + 6.0)) + (0.36 * (triangle_face + quad_face));
        assert!(
            close(positions(&mixed_smooth).x[mixed_shared as usize], expected_mixed),
            "Smooth Triangles did not interpolate the mixed triangle/quad edge mask"
        );

        let source_index = topology_index::view::view(
            &topology_index::create(None, geometry::topology(&triangles)).unwrap(),
        );
        let source_edge = shared - geometry::point_count(&triangles);
        let creaseweights: Vec<f64> = (0..geometry::vertex_count(&triangles))
            .map(|vertex| if source_index.edge_of_vertex[vertex as usize] == source_edge { 1.0 } else { 0.0 })
            .collect();
        let creased = geometry::with_attribute(
            float_attr(Owner::Vertex, "creaseweight", creaseweights),
            &triangles,
        )
        .unwrap();
        let creased_standard = ok(&creased, &|p| {
            p.grain = Some(1);
            p.triangle_policy = SubdivideTrianglesCatmullClark;
        });
        let creased_smooth = ok(&creased, &|p| {
            p.grain = Some(1);
            p.triangle_policy = SubdivideTrianglesSmooth;
        });
        assert!(
            equal_geometry(&creased_standard, &creased_smooth)
                && close(positions(&creased_smooth).x[shared as usize], 4.0),
            "fully sharp edge did not take precedence over Smooth Triangles"
        );
        for scheme in [Scheme::Loop, Scheme::Bilinear] {
            let ordinary = ok(&triangles, &|p| {
                p.grain = Some(1);
                p.scheme = scheme;
                p.triangle_policy = SubdivideTrianglesCatmullClark;
            });
            let alternate = ok(&triangles, &|p| {
                p.grain = Some(1);
                p.scheme = scheme;
                p.triangle_policy = SubdivideTrianglesSmooth;
            });
            assert!(equal_geometry(&ordinary, &alternate), "triangle policy changed a non-Catmull-Clark scheme");
        }
        let dense = grid(4, 3, 4.0, true);
        let exact = |domains: i64| {
            with_domains(domains, &|| {
                ok(&dense, &|p| {
                    p.grain = Some(1);
                    p.iterations = 2;
                    p.triangle_policy = SubdivideTrianglesSmooth;
                })
            })
        };
        assert!(equal_geometry(&exact(1), &exact(4)), "recursive Smooth Triangles differs across one and four domains");
        let selection = primitive_group("refine", geometry::primitive_count(&dense), &[0, 1, 2, 3]);
        let local_input = geometry::with_group(selection.clone(), &dense).unwrap();
        let local = |domains: i64, policy: TrianglePolicy| {
            with_domains(domains, &|| {
                ok(&local_input, &|p| {
                    p.grain = Some(1);
                    p.primitives = Some(selection.clone());
                    p.triangle_policy = policy;
                })
            })
        };
        let local_one = local(1, SubdivideTrianglesSmooth);
        assert!(equal_geometry(&local_one, &local(4, SubdivideTrianglesSmooth)));
        assert!(
            !equal_geometry(&local_one, &local(1, SubdivideTrianglesCatmullClark)),
            "local Subdivide ignored its Smooth Triangles policy"
        );
    }

    fn with_vertex_values(
        geometry_value: &geometry::T,
        make_value: &dyn Fn(i64, i64, i64) -> f64,
    ) -> geometry::T {
        let view = topology::view::view(geometry::topology(geometry_value)).unwrap();
        let index = topology_index::view::view(
            &topology_index::create(None, geometry::topology(geometry_value)).unwrap(),
        );
        let values: Vec<f64> = (0..geometry::vertex_count(geometry_value))
            .map(|vertex| {
                make_value(vertex, index.primitive_of_vertex[vertex as usize], view.vertex_points[vertex as usize])
            })
            .collect();
        geometry::with_attribute(float_attr(Owner::Vertex, "fvar", values), geometry_value).unwrap()
    }

    fn source_corner(geometry_value: &geometry::T, primitive: i64, point: i64) -> i64 {
        let view = topology::view::view(geometry::topology(geometry_value)).unwrap();
        let mut result = -1;
        for vertex in view.primitive_offsets[primitive as usize]..view.primitive_offsets[primitive as usize + 1] {
            if view.vertex_points[vertex as usize] == point {
                result = vertex;
            }
        }
        assert!(result >= 0, "test face does not reference the requested point");
        result
    }

    #[test]
    fn face_varying_interpolation() {
        let run_fvar = |source: &geometry::T, iterations: i64, mode: FaceVaryingInterpolation| {
            ok(source, &|p| {
                p.grain = Some(1);
                p.iterations = iterations;
                p.face_varying_interpolation = mode;
            })
        };
        let continuous = || {
            with_vertex_values(&grid(3, 3, 3.0, false), &|_, _, point| (point as f64) * (point as f64))
        };
        let input = continuous();
        let center_output = source_corner(&input, 0, 5) * 4;
        let boundary_output = source_corner(&input, 0, 1) * 4;
        let outer_output = source_corner(&input, 0, 0) * 4;
        let none = run_fvar(&input, 1, SubdivideFvarNone);
        let corners = run_fvar(&input, 1, SubdivideFvarCornersOnly);
        let plus1 = run_fvar(&input, 1, SubdivideFvarCornersPlus1);
        let plus2 = run_fvar(&input, 1, SubdivideFvarCornersPlus2);
        let boundaries = run_fvar(&input, 1, SubdivideFvarBoundaries);
        let all = run_fvar(&input, 1, SubdivideFvarAll);
        let values = |value: &geometry::T| floats(Owner::Vertex, "fvar", value);
        let at = |value: &geometry::T, index: i64| values(value)[index as usize];
        assert!(at(&all, center_output) == 25.0, "Linear All did not pin an interior face-varying value");
        assert!(at(&none, center_output) != 25.0, "Linear None did not smooth a continuous interior value");
        assert!(
            at(&none, outer_output) != 0.0 && at(&corners, outer_output) == 0.0,
            "Corners Only did not pin exactly a one-face region corner"
        );
        assert!(
            at(&corners, boundary_output) != 1.0 && at(&boundaries, boundary_output) == 1.0,
            "Boundaries did not linearly constrain a multi-face region boundary"
        );
        assert!(
            values(&corners) == values(&plus1) && values(&corners) == values(&plus2),
            "Plus policies changed a continuous region without a plus feature"
        );
        let source_values = values(&input);
        let tupled = geometry::with_attribute(
            attr(
                Owner::Vertex,
                "fvar2",
                Storage::Float2(
                    packed::float2::of_owned(
                        source_values.clone(),
                        source_values.iter().map(|v| 2.0 * v + 1.0).collect(),
                    )
                    .unwrap(),
                ),
            ),
            &input,
        )
        .unwrap();
        let tupled = geometry::with_attribute(
            attr(
                Owner::Vertex,
                "fvar3",
                Storage::Float3(
                    packed::float3::view::of_owned_exn(
                        source_values.clone(),
                        source_values.iter().map(|v| v + 3.0).collect(),
                        source_values.iter().map(|v| -2.0 * v).collect(),
                    )
                    .unwrap(),
                ),
            ),
            &tupled,
        )
        .unwrap();
        let tupled = geometry::with_attribute(
            attr(
                Owner::Vertex,
                "fvar4",
                Storage::Float4(
                    packed::float4::of_owned(
                        source_values.clone(),
                        source_values.iter().map(|v| v + 5.0).collect(),
                        source_values.iter().map(|v| v * 3.0).collect(),
                        source_values.iter().map(|v| 7.0 - v).collect(),
                    )
                    .unwrap(),
                ),
            ),
            &tupled,
        )
        .unwrap();
        let tupled = run_fvar(&tupled, 1, SubdivideFvarNone);
        let scalar = at(&tupled, center_output);
        let found = |name: &str| geometry::find_attribute(Owner::Vertex, name, &tupled).unwrap();
        let c = center_output as usize;
        let (a2, a3, a4) = (found("fvar2"), found("fvar3"), found("fvar4"));
        let Storage::Float2(f2) = attribute::view::storage(&a2).unwrap() else { panic!() };
        let Storage::Float3(f3) = attribute::view::storage(&a3).unwrap() else { panic!() };
        let Storage::Float4(f4) = attribute::view::storage(&a4).unwrap() else { panic!() };
        let (f2, f3, f4) = (
            packed::float2::view::view(f2),
            packed::float3::view::view(f3).unwrap(),
            packed::float4::view::view(f4),
        );
        assert!(
            f2.x[c] == scalar
                && f2.y[c] == 2.0 * scalar + 1.0
                && f3.y[c] == scalar + 3.0
                && f3.z[c] == -2.0 * scalar
                && f4.y[c] == scalar + 5.0
                && f4.z[c] == scalar * 3.0
                && f4.w[c] == 7.0 - scalar,
            "tuple face-varying components did not share one interpolation plan"
        );

        let center_fixture = |center_values: [f64; 4]| {
            with_vertex_values(&grid(2, 2, 2.0, false), &|_, primitive, point| {
                if point == 4 { center_values[primitive as usize] } else { (point * point) as f64 }
            })
        };
        let junction = center_fixture([10.0, 10.0, 20.0, 30.0]);
        let junction_corner = source_corner(&junction, 0, 4) * 4;
        let junction_corners = run_fvar(&junction, 1, SubdivideFvarCornersOnly);
        let junction_plus1 = run_fvar(&junction, 1, SubdivideFvarCornersPlus1);
        assert!(
            at(&junction_corners, junction_corner) != 10.0 && at(&junction_plus1, junction_corner) == 10.0,
            "Corners Plus 1 did not pin a junction of three face-varying regions"
        );
        let junction_topology = topology::view::view(geometry::topology(&junction_plus1)).unwrap();
        let plus1_values = values(&junction_plus1);
        let mut distinct: Vec<u64> = Vec::new();
        for (vertex, &point) in junction_topology.vertex_points.iter().enumerate() {
            if point == 4 && !distinct.contains(&plus1_values[vertex].to_bits()) {
                distinct.push(plus1_values[vertex].to_bits());
            }
        }
        assert_eq!(distinct.len(), 3, "smoothed face-varying interpolation collapsed an authored seam");
        let junction_scalar = values(&junction);
        let tuple_junction = run_fvar(
            &geometry::with_attribute(
                attr(
                    Owner::Vertex,
                    "tuple_seam",
                    Storage::Float2(
                        packed::float2::of_owned(vec![0.0; junction_scalar.len()], junction_scalar.clone())
                            .unwrap(),
                    ),
                ),
                &junction,
            )
            .unwrap(),
            1,
            SubdivideFvarCornersPlus1,
        );
        let found = geometry::find_attribute(Owner::Vertex, "tuple_seam", &tuple_junction).unwrap();
        let Storage::Float2(tuple_values) = attribute::view::storage(&found).unwrap() else { panic!() };
        let tuple_values = packed::float2::view::view(tuple_values);
        let tuple_topology = topology::view::view(geometry::topology(&tuple_junction)).unwrap();
        let mut tuple_distinct: Vec<u64> = Vec::new();
        for (vertex, &point) in tuple_topology.vertex_points.iter().enumerate() {
            if point == 4 && !tuple_distinct.contains(&tuple_values.y[vertex].to_bits()) {
                tuple_distinct.push(tuple_values.y[vertex].to_bits());
            }
        }
        assert_eq!(tuple_distinct.len(), 3, "face-varying seam classification ignored a non-leading tuple component");

        let concave = center_fixture([20.0, 10.0, 10.0, 10.0]);
        let concave_corner = source_corner(&concave, 1, 4) * 4;
        let concave_plus1 = run_fvar(&concave, 1, SubdivideFvarCornersPlus1);
        let concave_plus2 = run_fvar(&concave, 1, SubdivideFvarCornersPlus2);
        assert!(
            at(&concave_plus1, concave_corner) != 10.0 && at(&concave_plus2, concave_corner) == 10.0,
            "Corners Plus 2 did not propagate a concave one-face corner"
        );

        let dart = with_vertex_values(&grid(2, 2, 2.0, false), &|_, primitive, point| {
            if point == 4 {
                10.0
            } else if point == 1 && primitive == 0 {
                1.0
            } else if point == 1 && primitive == 1 {
                2.0
            } else {
                (point * point) as f64
            }
        });
        let dart_corner = source_corner(&dart, 0, 4) * 4;
        let dart_plus1 = run_fvar(&dart, 1, SubdivideFvarCornersPlus1);
        let dart_plus2 = run_fvar(&dart, 1, SubdivideFvarCornersPlus2);
        assert!(
            at(&dart_plus1, dart_corner) != 10.0 && at(&dart_plus2, dart_corner) == 10.0,
            "Corners Plus 2 did not pin a face-varying dart"
        );

        let corner_weight = float_attr(
            Owner::Point,
            "cornerweight",
            (0..geometry::point_count(&input)).map(|point| if point == 5 { 1.0 } else { 0.0 }).collect(),
        );
        let corner_input = geometry::with_attribute(corner_weight, &input).unwrap();
        let corner_output = run_fvar(&corner_input, 1, SubdivideFvarNone);
        assert!(
            at(&corner_output, center_output) == 25.0,
            "geometry corner sharpness did not take precedence over FVar None"
        );
        let source_index_value = topology_index::create(None, geometry::topology(&input)).unwrap();
        let crease_edge = topology_index::find_edge(&source_index_value, 5, 6).unwrap();
        let source_index = topology_index::view::view(&source_index_value);
        let crease_weights: Vec<f64> = (0..geometry::vertex_count(&input))
            .map(|vertex| if source_index.edge_of_vertex[vertex as usize] == crease_edge { 1.0 } else { 0.0 })
            .collect();
        let creased_input =
            geometry::with_attribute(float_attr(Owner::Vertex, "creaseweight", crease_weights), &input).unwrap();
        let smooth_output = run_fvar(&input, 1, SubdivideFvarNone);
        let creased_output = run_fvar(&creased_input, 1, SubdivideFvarNone);
        let directed = source_index.edge_vertices[source_index.edge_offsets[crease_edge as usize] as usize];
        let edge_output = directed * 4 + 1;
        assert!(
            at(&smooth_output, edge_output) != at(&creased_output, edge_output),
            "geometry edge sharpness did not take precedence over FVar None"
        );

        let recursive = |domains: i64| {
            with_domains(domains, &|| run_fvar(&continuous(), 2, SubdivideFvarNone))
        };
        let recursive_one = recursive(1);
        assert!(
            equal_geometry(&recursive_one, &recursive(4)),
            "recursive smooth face-varying refinement differs across domains"
        );
        assert_eq!(geometry::vertex_count(&recursive_one), 9 * 16 * 4);

        let loop_input = with_vertex_values(&grid(3, 2, 3.0, true), &|_, primitive, point| {
            ((point * point) + (primitive / 4)) as f64
        });
        for mode in [
            SubdivideFvarNone,
            SubdivideFvarCornersOnly,
            SubdivideFvarCornersPlus1,
            SubdivideFvarCornersPlus2,
            SubdivideFvarBoundaries,
            SubdivideFvarAll,
        ] {
            let refine = |domains: i64| {
                with_domains(domains, &|| {
                    ok(&loop_input, &|p| {
                        p.grain = Some(1);
                        p.scheme = Scheme::Loop;
                        p.face_varying_interpolation = mode;
                    })
                })
            };
            assert!(
                equal_geometry(&refine(1), &refine(4)),
                "Loop face-varying policy differs across one and four domains"
            );
        }
        let loop_none = ok(&loop_input, &|p| {
            p.grain = Some(1);
            p.scheme = Scheme::Loop;
            p.face_varying_interpolation = SubdivideFvarNone;
        });
        let loop_all = ok(&loop_input, &|p| {
            p.grain = Some(1);
            p.scheme = Scheme::Loop;
            p.face_varying_interpolation = SubdivideFvarAll;
        });
        assert!(
            floats(Owner::Vertex, "fvar", &loop_none) != floats(Owner::Vertex, "fvar", &loop_all),
            "Loop FVar None did not smooth values relative to Linear All"
        );

        let local_selection = primitive_group("refine", geometry::primitive_count(&input), &[0, 1, 3, 4]);
        let local_input = geometry::with_group(local_selection.clone(), &input).unwrap();
        let local = |mode: FaceVaryingInterpolation, domains: i64| {
            with_domains(domains, &|| {
                ok(&local_input, &|p| {
                    p.grain = Some(1);
                    p.primitives = Some(local_selection.clone());
                    p.face_varying_interpolation = mode;
                })
            })
        };
        let local_none_one = local(SubdivideFvarNone, 1);
        assert!(
            equal_geometry(&local_none_one, &local(SubdivideFvarNone, 4)),
            "local face-varying refinement differs across one and four domains"
        );
        assert!(
            floats(Owner::Vertex, "fvar", &local_none_one)
                != floats(Owner::Vertex, "fvar", &local(SubdivideFvarAll, 1)),
            "local Subdivide ignored its face-varying interpolation policy"
        );

        let bilinear = |mode: FaceVaryingInterpolation| {
            ok(&input, &|p| {
                p.scheme = Scheme::Bilinear;
                p.face_varying_interpolation = mode;
            })
        };
        assert!(
            equal_geometry(&bilinear(SubdivideFvarNone), &bilinear(SubdivideFvarAll)),
            "face-varying policy changed bilinear interpolation"
        );
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        match sub(&input, &|p| {
            p.cancel = Some(token.clone());
            p.face_varying_interpolation = SubdivideFvarNone;
        }) {
            Err(failure) if error::is_cancelled(&failure) => {}
            _ => panic!("cancelled face-varying refinement published geometry"),
        }
    }

    fn curve_network() -> geometry::T {
        let topology_value = topology::create_owned(
            3,
            vec![0, 1, 1, 2],
            vec![0, 2, 4],
            vec![topology::PrimitiveKind::OpenPolyline, topology::PrimitiveKind::OpenPolyline],
        )
        .unwrap();
        let attributes = vec![
            float_attr(Owner::Point, "mass", vec![0.0, 8.0, 16.0]),
            attr(Owner::Point, "id", Storage::Int(Arc::new(vec![10, 11, 12]))),
            attr(
                Owner::Point,
                "rows",
                Storage::IntArray(
                    packed::int_array::create_owned(vec![0, 1, 3, 4], vec![10, 11, 111, 12]).unwrap(),
                ),
            ),
            attr(
                Owner::Point,
                "N",
                Storage::Float3(
                    packed::float3::view::of_owned_exn(vec![0.0; 3], vec![0.0; 3], vec![1.0; 3]).unwrap(),
                ),
            ),
            float_attr(Owner::Vertex, "u", vec![0.0, 2.0, 10.0, 14.0]),
            attr(
                Owner::Vertex,
                "label",
                Storage::Text(Arc::new(["a", "b", "c", "d"].iter().map(|s| s.as_bytes()).collect())),
            ),
            attr(Owner::Primitive, "material", Storage::Int(Arc::new(vec![7, 9]))),
            attr(Owner::Detail, "tag", Storage::Text(std::sync::Arc::new(vec![b"curves".to_vec()].into()))),
        ];
        let point_group = group::ordered(group::Owner::Point, "picked_points", 3, &[1, 0]).unwrap();
        let vertex_group = group::ordered(group::Owner::Vertex, "picked_vertices", 4, &[1, 0]).unwrap();
        let primitive_group = group::ordered(group::Owner::Primitive, "picked_primitives", 2, &[1]).unwrap();
        let edge_index = topology_index::create(None, &topology_value).unwrap();
        let mut builder = edge_group::builder::create(&topology_value, &edge_index, "first_edge").unwrap();
        edge_group::builder::set(
            &mut builder,
            topology_index::find_edge(&edge_index, 0, 1).unwrap(),
            true,
        )
        .unwrap();
        geometry::create(
            packed::float3::view::of_owned_exn(vec![0.0, 1.0, 4.0], vec![0.0, 2.0, 0.0], vec![0.0; 3]).unwrap(),
            topology_value,
            Some(attributes),
            Some(vec![point_group, vertex_group, primitive_group]),
            Some(vec![edge_group::builder::freeze(&mut builder).unwrap()]),
        )
        .unwrap()
    }

    fn with_detail(value: &geometry::T, name: &str, storage: Storage) -> geometry::T {
        geometry::with_attribute(attr(Owner::Detail, name, storage), value).unwrap()
    }

    #[test]
    fn polygon_curve_subdivision() {
        let run_curves = |domains: i64, independent: bool, iterations: i64, scheme: Scheme| {
            with_domains(domains, &|| {
                ok(&curve_network(), &|p| {
                    p.grain = Some(1);
                    p.iterations = iterations;
                    p.scheme = scheme;
                    p.treat_curves_as_independent = independent;
                })
            })
        };
        let cc = Scheme::CatmullClark;
        let shared = run_curves(1, false, 1, cc);
        assert!(
            equal_geometry(&shared, &run_curves(4, false, 1, cc)),
            "shared polygon-curve subdivision differs across one and four domains"
        );
        assert!(
            geometry::point_count(&shared) == 5
                && geometry::vertex_count(&shared) == 6
                && geometry::primitive_count(&shared) == 2,
            "shared polygon-curve subdivision cardinality"
        );
        let view = topology::view::view(geometry::topology(&shared)).unwrap();
        assert!(
            view.vertex_points.to_vec() == vec![0, 3, 1, 1, 4, 2] && view.primitive_kinds.to_vec() == vec![1, 1],
            "shared polygon-curve subdivision topology"
        );
        let shared_positions = positions(&shared);
        assert!(
            shared_positions.x.to_vec() == vec![0.0, 1.25, 4.0, 0.5, 2.5]
                && shared_positions.y.to_vec() == vec![0.0, 1.5, 0.0, 1.0, 1.0],
            "shared polygon-curve cubic stencil"
        );
        assert_eq!(floats(Owner::Point, "mass", &shared), vec![0.0, 8.0, 16.0, 4.0, 12.0]);
        assert_eq!(ints(Owner::Point, "id", &shared), vec![10, 11, 12, 10, 11]);
        assert_eq!(floats(Owner::Vertex, "u", &shared), vec![0.0, 1.0, 2.0, 10.0, 12.0, 14.0]);
        assert_eq!(ints(Owner::Primitive, "material", &shared), vec![7, 9]);
        let curve_n = match attribute::view::storage(
            &geometry::find_attribute(Owner::Point, "N", &shared).unwrap(),
        )
        .unwrap()
        {
            Storage::Float3(values) => packed::float3::view::view(values).unwrap(),
            _ => panic!("N storage changed"),
        };
        assert!(
            curve_n.x.iter().all(|&v| v == 0.0)
                && curve_n.y.iter().all(|&v| v == 0.0)
                && curve_n.z.iter().all(|&v| v == 1.0),
            "polygon-curve subdivision did not interpolate point normals"
        );
        let picked = geometry::find_group(group::Owner::Point, "picked_points", &shared).unwrap();
        assert_eq!(group::ordered_elements(&picked), Some(vec![1, 0, 3]));
        assert!(group::mem(3, &picked) && !group::mem(4, &picked));
        let picked_vertices = geometry::find_group(group::Owner::Vertex, "picked_vertices", &shared).unwrap();
        assert_eq!(group::ordered_elements(&picked_vertices), Some(vec![2, 0, 1]));
        let first_edges = geometry::find_edge_group("first_edge", &shared).unwrap();
        assert_eq!(edge_group::cardinality(&first_edges), 2);

        let independent = run_curves(1, true, 1, cc);
        assert!(equal_geometry(&independent, &run_curves(4, true, 1, cc)));
        assert!(
            geometry::point_count(&independent) == 6 && geometry::vertex_count(&independent) == 6,
            "independent polygon-curve subdivision did not duplicate shared corners"
        );
        let ip = positions(&independent);
        assert!(
            ip.x.to_vec() == vec![0.0, 0.5, 1.0, 1.0, 2.5, 4.0] && ip.y.to_vec() == vec![0.0, 1.0, 2.0, 2.0, 1.0, 0.0],
            "independent polygon-curve endpoint rule"
        );
        let recursive = run_curves(1, false, 2, cc);
        assert!(
            equal_geometry(&recursive, &run_curves(4, false, 2, cc))
                && geometry::point_count(&recursive) == 9
                && geometry::vertex_count(&recursive) == 10,
            "recursive shared polygon-curve refinement"
        );
        let recursive_independent = run_curves(1, true, 2, cc);
        assert!(
            geometry::point_count(&recursive_independent) == 10
                && geometry::vertex_count(&recursive_independent) == 10
        );
        let bilinear = run_curves(1, false, 1, Scheme::Bilinear);
        let bp = positions(&bilinear);
        assert!(bp.x[1] == 1.0 && bp.y[1] == 2.0, "bilinear polygon-curve refinement moved an old point");
        let overridden = ok(
            &with_detail(&curve_network(), "osd_scheme", Storage::Text(std::sync::Arc::new(vec![b"bilinear".to_vec()].into()))),
            &|p| p.scheme = Scheme::CatmullClark,
        );
        let overridden = geometry::without_attribute(Owner::Detail, "osd_scheme", &overridden);
        assert!(
            equal_geometry(&overridden, &bilinear),
            "polygon-curve subdivision ignored its osd_scheme detail override"
        );

        let local_selection = primitive_group("local_curve", 2, &[0]);
        let local = |domains: i64| {
            with_domains(domains, &|| {
                ok(&curve_network(), &|p| {
                    p.grain = Some(1);
                    p.primitives = Some(local_selection.clone());
                })
            })
        };
        let local_one = local(1);
        assert!(
            equal_geometry(&local_one, &local(4))
                && geometry::primitive_count(&local_one) == 2
                && geometry::vertex_count(&local_one) == 5,
            "local polygon-curve subdivision exactness/cardinality"
        );
        let local_topology = topology::view::view(geometry::topology(&local_one)).unwrap();
        assert_eq!(local_topology.primitive_offsets.to_vec(), vec![0, 3, 5]);

        let closed_source = geometry::create(
            packed::float3::view::of_owned_exn(
                vec![0.0, 2.0, 2.0, 0.0],
                vec![0.0, 0.0, 2.0, 2.0],
                vec![0.0; 4],
            )
            .unwrap(),
            topology::create_owned(
                4,
                vec![0, 1, 2, 3],
                vec![0, 4],
                vec![topology::PrimitiveKind::ClosedPolyline],
            )
            .unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        let closed = |domains: i64| {
            with_domains(domains, &|| ok(&closed_source, &|p| p.grain = Some(1)))
        };
        let closed_one = closed(1);
        assert!(
            equal_geometry(&closed_one, &closed(4))
                && geometry::point_count(&closed_one) == 8
                && geometry::vertex_count(&closed_one) == 8
                && topology::primitive_kind(geometry::topology(&closed_one), 0).unwrap()
                    == topology::PrimitiveKind::ClosedPolyline,
            "closed polygon-curve subdivision exactness/topology"
        );
        let closed_output = positions(&closed_one);
        assert!(closed_output.x[0] == 0.25 && closed_output.y[0] == 0.25);

        let free_source = geometry::create(
            packed::float3::view::of_owned_exn(
                vec![0.0, 1.0, 9.0],
                vec![0.0, 0.0, 9.0],
                vec![0.0, 0.0, 9.0],
            )
            .unwrap(),
            topology::create_owned(3, vec![0, 1], vec![0, 2], vec![topology::PrimitiveKind::OpenPolyline])
                .unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        for independent in [false, true] {
            let output = ok(&free_source, &|p| p.treat_curves_as_independent = independent);
            let fp = positions(&output);
            assert!(
                geometry::point_count(&output) == 4
                    && fp.x.contains(&9.0)
                    && fp.y.contains(&9.0)
                    && fp.z.contains(&9.0),
                "polygon-curve subdivision did not retain a free point exactly once"
            );
        }
        let invalid = geometry::create(
            packed::float3::view::of_owned_exn(vec![0.0, 1.0], vec![0.0, 0.0], vec![0.0, 0.0]).unwrap(),
            topology::create_owned(2, vec![0, 0], vec![0, 2], vec![topology::PrimitiveKind::OpenPolyline])
                .unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        match sub(&invalid, &|_| {}) {
            Ok(_) => panic!("Subdivide accepted a zero-length curve topology edge"),
            Err(failure) => assert!(
                error::message(&failure).contains("zero-length"),
                "Subdivide returned the wrong zero-length curve diagnostic"
            ),
        }
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        match sub(&curve_network(), &|p| p.cancel = Some(token.clone())) {
            Ok(_) => panic!("polygon-curve Subdivide ignored cancellation"),
            Err(failure) => assert_eq!(error::code(&failure), Code::Cancelled),
        }
        match sub(&curve_network(), &|p| p.scheme = Scheme::Loop) {
            Ok(_) => panic!("Subdivide accepted polygon curves under Loop"),
            Err(failure) => assert!(
                error::message(&failure).contains("cannot refine polygon curves"),
                "Subdivide returned the wrong Loop/curve diagnostic"
            ),
        }
    }

    fn mixed_surface_curves() -> geometry::T {
        let attributes = vec![
            float_attr(Owner::Point, "weight", vec![0.0, 8.0, 16.0, 20.0, 21.0, 22.0, 23.0]),
            attr(
                Owner::Point,
                "N",
                Storage::Float3(
                    packed::float3::view::of_owned_exn(vec![0.0; 7], vec![0.0; 7], vec![1.0; 7]).unwrap(),
                ),
            ),
            float_attr(Owner::Vertex, "u", vec![0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 10.0, 14.0]),
            attr(Owner::Primitive, "source_kind", Storage::Int(Arc::new(vec![1, 0, 1]))),
            attr(Owner::Detail, "tag", Storage::Text(std::sync::Arc::new(vec![b"mixed".to_vec()].into()))),
        ];
        geometry::create(
            packed::float3::view::of_owned_exn(
                vec![0.0, 1.0, 4.0, 0.0, 2.0, 2.0, 0.0],
                vec![0.0, 2.0, 0.0, 4.0, 4.0, 6.0, 6.0],
                vec![0.0; 7],
            )
            .unwrap(),
            topology::create_owned(
                7,
                vec![0, 1, 3, 4, 5, 6, 1, 2],
                vec![0, 2, 6, 8],
                vec![
                    topology::PrimitiveKind::OpenPolyline,
                    topology::PrimitiveKind::Polygon,
                    topology::PrimitiveKind::OpenPolyline,
                ],
            )
            .unwrap(),
            Some(attributes),
            None,
            None,
        )
        .unwrap()
    }

    #[test]
    fn mixed_surface_curve_subdivision() {
        let run_mixed = |domains: i64, selection: Option<&group::T>, independent: bool, recompute: bool| {
            with_domains(domains, &|| {
                ok(&mixed_surface_curves(), &|p| {
                    p.grain = Some(1);
                    p.primitives = selection.cloned();
                    p.treat_curves_as_independent = independent;
                    p.recompute_point_normals = recompute;
                })
            })
        };
        let one = run_mixed(1, None, false, false);
        assert!(
            equal_geometry(&one, &run_mixed(4, None, false, false)),
            "mixed surface/curve subdivision differs across one and four domains"
        );
        assert_eq!(geometry::primitive_count(&one), 6);
        let view = topology::view::view(geometry::topology(&one)).unwrap();
        assert_eq!(
            view.primitive_kinds.to_vec(),
            vec![1, 0, 0, 0, 0, 1],
            "mixed subdivision did not restore source primitive order"
        );
        assert_eq!(ints(Owner::Primitive, "source_kind", &one), vec![1, 0, 0, 0, 0, 1]);
        let tag = geometry::find_attribute(Owner::Detail, "tag", &one).unwrap();
        assert!(matches!(
            attribute::view::storage(&tag).unwrap(),
            Storage::Text(values) if **values == vec![b"mixed".to_vec()]
        ));
        let selection = primitive_group("local", 3, &[0, 1]);
        let local_one = run_mixed(1, Some(&selection), false, false);
        assert!(equal_geometry(&local_one, &run_mixed(4, Some(&selection), false, false)));
        assert_eq!(geometry::primitive_count(&local_one), 6);
        let local_topology = topology::view::view(geometry::topology(&local_one)).unwrap();
        assert_eq!(local_topology.primitive_kinds.to_vec(), vec![1, 0, 0, 0, 0, 1]);
        let independent = run_mixed(1, None, true, false);
        assert_eq!(geometry::point_count(&independent), geometry::point_count(&one) + 1);
        let recomputed = run_mixed(1, None, false, true);
        assert!(equal_geometry(&recomputed, &run_mixed(4, None, false, true)));
        let view = topology::view::view(geometry::topology(&recomputed)).unwrap();
        let normals = match attribute::view::storage(
            &geometry::find_attribute(Owner::Point, "N", &recomputed).unwrap(),
        )
        .unwrap()
        {
            Storage::Float3(values) => packed::float3::view::view(values).unwrap(),
            _ => panic!("N storage changed"),
        };
        let mut surface = vec![false; view.point_count as usize];
        let mut curve = vec![false; view.point_count as usize];
        for primitive in 0..view.primitive_kinds.len() {
            for vertex in view.primitive_offsets[primitive]..view.primitive_offsets[primitive + 1] {
                let point = view.vertex_points[vertex as usize] as usize;
                if view.primitive_kinds[primitive] == 0 {
                    surface[point] = true;
                } else {
                    curve[point] = true;
                }
            }
        }
        for point in 0..view.point_count as usize {
            if surface[point] {
                assert!(
                    normals.x[point] == 0.0 && normals.y[point] == 0.0 && normals.z[point].abs() == 1.0,
                    "mixed Subdivide surface normal was not normalized"
                );
            } else if curve[point] {
                assert!(
                    normals.x[point] == 0.0 && normals.y[point] == 0.0 && normals.z[point] == 0.0,
                    "mixed Subdivide curve normal was not zero"
                );
            }
        }
    }

    fn boundary_edge_count(value: &geometry::T) -> usize {
        let index = topology_index::view::view(
            &topology_index::create(None, geometry::topology(value)).unwrap(),
        );
        (0..index.edge_a.len())
            .filter(|&edge| index.edge_offsets[edge + 1] - index.edge_offsets[edge] == 1)
            .count()
    }

    #[test]
    fn crack_closure_policies_close_the_selected_block() {
        let base = grid(4, 4, 4.0, false);
        let selection = primitive_group("center", 16, &[5, 6, 9, 10]);
        let source = geometry::with_group(selection.clone(), &base).unwrap();
        let run_cracks = |cracks: CrackPolicy, domains: i64, consistent: bool| {
            with_domains(domains, &|| {
                ok(&source, &|p| {
                    p.grain = Some(1);
                    p.primitives = Some(selection.clone());
                    p.cracks = cracks;
                    p.consistent_topology = consistent;
                })
            })
        };
        let open = run_cracks(CrackPolicy::SubdivideDoNotClose, 1, false);
        assert!(boundary_edge_count(&open) > 16, "an unclosed local subdivision should keep its crack");
        let closing = [
            CrackPolicy::SubdividePullNoEdgeDivision,
            CrackPolicy::SubdividePullDivideEdges(0.5),
            CrackPolicy::SubdividePullTriangulate(0.5),
            CrackPolicy::SubdivideStitchNoEdgeDivision,
            CrackPolicy::SubdivideStitchDivideEdges,
            CrackPolicy::SubdivideStitchTriangulate,
        ];
        for cracks in closing {
            for consistent in [false, true] {
                let one = run_cracks(cracks, 1, consistent);
                assert!(
                    equal_geometry(&one, &run_cracks(cracks, 4, consistent)),
                    "crack closure {cracks:?} differs across one and four domains"
                );
                assert_eq!(
                    geometry::primitive_count(&one) >= 16,
                    true,
                    "crack closure {cracks:?} lost faces"
                );
                let welded = matches!(
                    cracks,
                    CrackPolicy::SubdividePullDivideEdges(_) | CrackPolicy::SubdividePullTriangulate(_)
                ) || (!consistent
                    && matches!(
                        cracks,
                        CrackPolicy::SubdivideStitchDivideEdges | CrackPolicy::SubdivideStitchTriangulate
                    ));
                if welded {
                    assert_eq!(
                        boundary_edge_count(&one),
                        16,
                        "crack closure {cracks:?} (consistent {consistent}) left the interface open"
                    );
                }
            }
        }
    }
}
