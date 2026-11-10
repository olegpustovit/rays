//! Port of lib/sop/sop_topology.ml: the topology and Boolean nodes, declared once (metadata,
//! fields, parameters, operator). One `pub mod` per OCaml module, in the OCaml order.
use std::sync::Arc;

use param::{Kind, Value};
use rays_diagnostic::Diagnostic;
use rdk_core::{edge_group, geometry, group};

use crate::context;
use crate::decl::{self, Meta, Spec};
use crate::edit_graph::Factory;
use crate::node::{self, build, CookMode, GeometryCooked};
use crate::support::{self, Choice};

// ---------------------------------------------------------------- shared helpers

type Cook = dyn Fn(&context::T, &[geometry::T]) -> Result<GeometryCooked, Diagnostic> + Send + Sync;

/// `Node.Build.make_geometry ?label ~operation ~version ~parameters:"" ~cook_mode`, static
/// dependencies, no expand.
fn make(label: &str, operation: &str, version: i64, cook_mode: CookMode, inputs: Vec<node::T>, cook: Arc<Cook>) -> node::T {
    build::make_geometry(
        build::Args { label: Some(label.to_owned()), version, cook_mode, inputs, ..build::args(operation) },
        None,
        Arc::new(move |_, context, inputs| cook(context, inputs)),
    )
}

fn grain(context: &context::T) -> i64 {
    context::grain(context)
}

fn cancel(context: &context::T) -> Option<Arc<rdk_core::cancel::T>> {
    Some(Arc::clone(context::cancel_token(context)))
}

/// Result of an rdk call as a cooked geometry.
fn done(result: Result<geometry::T, rdk_core::error::T>) -> Result<GeometryCooked, Diagnostic> {
    match result {
        Ok(geometry) => support::cooked(geometry),
        Err(error) => Err(support::structured_rdk_error(&error)),
    }
}

fn missing(code: &str, what: &str, name: &str) -> Diagnostic {
    support::diagnostic(code, &format!("{what} {}", node::ocaml_quote(name)))
}

/// A named group of `owner` looked up in `geometry`, or the diagnostic `code`: "`what` "name"".
fn find_group(
    owner: group::Owner,
    name: &Option<String>,
    geometry: &geometry::T,
    code: &str,
    what: &str,
) -> Result<Option<group::T>, Diagnostic> {
    match name {
        None => Ok(None),
        Some(name) => geometry::find_group(owner, name, geometry).map(Some).ok_or_else(|| missing(code, what, name)),
    }
}

fn find_edges(
    name: &Option<String>,
    geometry: &geometry::T,
    code: &str,
    what: &str,
) -> Result<Option<edge_group::T>, Diagnostic> {
    match name {
        None => Ok(None),
        Some(name) => geometry::find_edge_group(name, geometry).map(Some).ok_or_else(|| missing(code, what, name)),
    }
}

fn find_primitives(name: &Option<String>, geometry: &geometry::T, code: &str, what: &str) -> Result<Option<group::T>, Diagnostic> {
    find_group(group::Owner::Primitive, name, geometry, code, what)
}

fn first_input(inputs: &[Option<node::T>]) -> node::T {
    inputs[0].clone().expect("required input")
}

fn vb(values: &[Value], index: usize) -> bool {
    match &values[index] {
        Value::Bool(value) => *value,
        _ => unreachable!("parameter {index} is not a toggle"),
    }
}

fn vf(values: &[Value], index: usize) -> f64 {
    match &values[index] {
        Value::Float(value) => *value,
        _ => unreachable!("parameter {index} is not a float"),
    }
}

fn vi(values: &[Value], index: usize) -> i64 {
    match &values[index] {
        Value::Int(value) => *value,
        _ => unreachable!("parameter {index} is not an int"),
    }
}

fn vs(values: &[Value], index: usize) -> String {
    match &values[index] {
        Value::Text(value) => String::from_utf8_lossy(value).into_owned(),
        _ => unreachable!("parameter {index} is not text"),
    }
}

/// OCaml `String.trim t = ""`.
fn blank(text: &str) -> bool {
    text.trim().is_empty()
}

/// `List.length l <> List.length (List.sort_uniq compare l)`.
fn has_duplicates(names: &[String]) -> bool {
    let mut sorted = names.to_vec();
    sorted.sort();
    sorted.dedup();
    sorted.len() != names.len()
}

fn refuse(node: &str, message: &str) -> Result<(), String> {
    Err(format!("sop/{node}: {message}"))
}

// ---- field constructors (one `Spec` each; descriptions and vec3 by struct update)

fn text(name: &'static str, label: &'static str, folder: &'static str, default: &str) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::text_kind(), Value::Text(default.as_bytes().into())) }
}

fn toggle(name: &'static str, label: &'static str, folder: &'static str, default: bool) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::toggle_kind(), Value::Bool(default)) }
}

fn float(
    name: &'static str,
    label: &'static str,
    folder: &'static str,
    default: f64,
    range: (f64, f64),
    hard: (Option<f64>, Option<f64>),
) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::float_kind(range.0, range.1, hard.0, hard.1), Value::Float(default)) }
}

fn int(
    name: &'static str,
    label: &'static str,
    folder: &'static str,
    default: i64,
    range: (i64, i64),
    hard_min: Option<i64>,
) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::int_kind(range.0, range.1, hard_min, None), Value::Int(default)) }
}

fn choice(name: &'static str, label: &'static str, folder: &'static str, kind: Kind, default: Value) -> Spec {
    Spec { label, folder, ..decl::spec(name, kind, default) }
}

// ---------------------------------------------------------------- choice tables

use rdk_ops::fuse_reduce::Position;

const POSITION: Choice<Position> = Choice(&[
    ("First", Position::First),
    ("Least point", Position::LeastPoint),
    ("Greatest point", Position::GreatestPoint),
    ("Average", Position::Average),
    ("Minimum", Position::Minimum),
    ("Maximum", Position::Maximum),
    ("Mode", Position::Mode),
    ("Median", Position::Median),
    ("Sum", Position::Sum),
    ("Sum squares", Position::SumSquares),
    ("Root mean square", Position::RootMeanSquare),
    ("Weighted average", Position::WeightedAverage),
    ("Weighted sum", Position::WeightedSum),
    ("Minimum weight", Position::MinimumWeight),
    ("Maximum weight", Position::MaximumWeight),
]);

// ---------------------------------------------------------------- Edge_collapse

pub mod edge_collapse {
    use super::*;

    pub const META: Meta = Meta {
        key: "edge_collapse",
        label: "Edge Collapse",
        category: "Topology/Edge",
        inputs: 1,
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Edge group", "", ""),
            text("connectivity_attribute", "Connectivity attribute", "", ""),
            choice("position", "Position", "", POSITION.kind(), POSITION.value(&Position::Average)),
            toggle("remove_degenerate_primitives", "Remove degenerate primitives", "Cleanup", true),
            toggle("recompute_point_normals", "Recompute point normals", "Cleanup", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_connectivity_attribute = vs(values, 1);
        let p_position = POSITION.of(&values[2]);
        let p_remove_degenerate_primitives = vb(values, 3);
        let p_recompute_point_normals = vb(values, 4);
        let group = support::optional_text(&p_group);
        let connectivity_attribute = support::optional_text(&p_connectivity_attribute);
        Ok(make(
            label,
            "edge_collapse",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let edges = find_edges(&group, geometry, "missing_group", "edge_collapse could not find native edge group")?;
                done(rdk_ops::edge_collapse::run(
                    &rdk_ops::edge_collapse::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        edges,
                        connectivity_attribute: connectivity_attribute.clone(),
                        position: Some(p_position),
                        remove_degenerate_primitives: Some(p_remove_degenerate_primitives),
                        recompute_point_normals: Some(p_recompute_point_normals),
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Dissolve

pub mod dissolve {
    use super::*;
    use rdk_ops::dissolve::{BridgePolicy, Operation};

    const OPERATION: Choice<Operation> = Choice(&[
        ("Selected", Operation::DissolveSelected),
        ("Non-selected", Operation::DissolveNonSelected),
    ]);
    const BRIDGE: Choice<BridgePolicy> = Choice(&[
        ("Create bridged polygons", BridgePolicy::CreateBridgedPolygons),
        ("Create disjoint polygons", BridgePolicy::CreateDisjointPolygons),
        ("Delete bridge polygons", BridgePolicy::DeleteBridgePolygons),
    ]);

    pub const META: Meta = Meta { key: "dissolve", label: "Dissolve", category: "Topology", inputs: 1, ..decl::META };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Edge group", "", ""),
            choice("operation", "Operation", "", OPERATION.kind(), OPERATION.at(0)),
            choice("bridge_policy", "Bridge loops", "", BRIDGE.kind(), BRIDGE.at(0)),
            toggle("remove_inline_points", "Remove inline points", "Cleanup", true),
            float("collinearity_tolerance", "Collinearity tolerance", "Cleanup", 1e-6, (0., 0.1), (Some(0.), None)),
            toggle("remove_unused_points", "Remove unused points", "Cleanup", true),
            toggle("create_boundary_curves", "Create boundary curves", "", false),
            toggle("recompute_normals", "Recompute normals", "Cleanup", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_operation = OPERATION.of(&values[1]);
        let p_bridge_policy = BRIDGE.of(&values[2]);
        let p_remove_inline_points = vb(values, 3);
        let p_collinearity_tolerance = vf(values, 4);
        let p_remove_unused_points = vb(values, 5);
        let p_create_boundary_curves = vb(values, 6);
        let p_recompute_normals = vb(values, 7);
        let group = support::optional_text(&p_group);
        Ok(make(
            label,
            "dissolve",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let edges = find_edges(&group, &inputs[0], "missing_edge_group", "dissolve could not find edge group")?;
                done(rdk_ops::dissolve::run(
                    &rdk_ops::dissolve::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        edges,
                        operation: p_operation,
                        bridge_policy: p_bridge_policy,
                        remove_inline_points: p_remove_inline_points,
                        collinearity_tolerance: p_collinearity_tolerance,
                        remove_unused_points: p_remove_unused_points,
                        create_boundary_curves: p_create_boundary_curves,
                        recompute_normals: p_recompute_normals,
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Triangulate

pub mod triangulate {
    use super::*;

    pub const META: Meta = Meta { key: "triangulate", label: "Triangulate", category: "Topology", inputs: 1, ..decl::META };

    pub fn specs() -> Vec<Spec> {
        vec![text("group", "Primitive group", "", "")]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let group = support::optional_text(&vs(values, 0));
        Ok(make(
            label,
            "triangulate",
            2,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let primitives = find_primitives(&group, geometry, "missing_group", "triangulate could not find primitive group")?;
                done(rdk_ops::triangulate::run(
                    &rdk_ops::triangulate::Params { cancel: cancel(context), grain: Some(grain(context)), primitives },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Poly_extrude

pub mod poly_extrude {
    use super::*;
    use rdk_ops::poly_extrude::Divide;

    const DIVIDE: Choice<Divide> = Choice(&[
        ("Individual elements", Divide::ExtrudeIndividual),
        ("Connected components", Divide::ExtrudeConnectedComponents),
    ]);

    pub const META: Meta = Meta {
        key: "poly_extrude",
        label: "Poly Extrude",
        category: "Topology/Polygon",
        inputs: 1,
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Primitive group", "", ""),
            text("split_edges", "Split edge group", "", ""),
            float("distance", "Distance", "", 0.1, (-10., 10.), (None, None)),
            choice("divide", "Divide into", "", DIVIDE.kind(), DIVIDE.at(0)),
            int("divisions", "Divisions", "", 1, (1, 64), Some(1)),
            toggle("output_front", "Output front", "Output", true),
            toggle("output_back", "Output back", "Output", true),
            toggle("output_side", "Output side", "Output", true),
            text("front_group", "Front group", "Groups", ""),
            text("back_group", "Back group", "Groups", ""),
            text("side_group", "Side group", "Groups", ""),
            text("front_boundary_group", "Front boundary group", "Groups", ""),
            text("back_boundary_group", "Back boundary group", "Groups", ""),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_split_edges = vs(values, 1);
        let p_distance = vf(values, 2);
        let p_divide = DIVIDE.of(&values[3]);
        let p_divisions = vi(values, 4);
        let p_output_front = vb(values, 5);
        let p_output_back = vb(values, 6);
        let p_output_side = vb(values, 7);
        let p_front_group = vs(values, 8);
        let p_back_group = vs(values, 9);
        let p_side_group = vs(values, 10);
        let p_front_boundary_group = vs(values, 11);
        let p_back_boundary_group = vs(values, 12);
        let group = support::optional_text(&p_group);
        let split_edges = support::optional_text(&p_split_edges);
        let (front_group, back_group, side_group) = (
            support::optional_text(&p_front_group),
            support::optional_text(&p_back_group),
            support::optional_text(&p_side_group),
        );
        let (front_boundary_group, back_boundary_group) =
            (support::optional_text(&p_front_boundary_group), support::optional_text(&p_back_boundary_group));
        Ok(make(
            label,
            "poly_extrude",
            2,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let primitives = find_primitives(&group, geometry, "missing_group", "poly_extrude could not find primitive group");
                let split = find_edges(&split_edges, geometry, "missing_group", "poly_extrude could not find edge split group");
                let (primitives, split_edges) = (primitives?, split?);
                done(rdk_ops::poly_extrude::run(
                    &rdk_ops::poly_extrude::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        primitives,
                        split_edges,
                        divide: p_divide,
                        divisions: p_divisions,
                        output_front: p_output_front,
                        output_back: p_output_back,
                        output_side: p_output_side,
                        front_group: front_group.clone(),
                        back_group: back_group.clone(),
                        side_group: side_group.clone(),
                        front_boundary_group: front_boundary_group.clone(),
                        back_boundary_group: back_boundary_group.clone(),
                        distance: p_distance,
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Poly_fill

pub mod poly_fill {
    use super::*;
    use rdk_ops::poly_fill::Mode;

    const MODE: Choice<Mode> = Choice(&[
        ("Single polygon", Mode::FillSinglePolygon),
        ("Triangles", Mode::FillTriangles),
        ("Triangle fan", Mode::FillTriangleFan),
    ]);

    pub const META: Meta = Meta {
        key: "poly_fill",
        label: "Poly Fill",
        category: "Topology/Polygon",
        inputs: 1,
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("boundary_group", "Boundary group", "", ""),
            choice("mode", "Fill mode", "", MODE.kind(), MODE.at(1)),
            toggle("reverse_patches", "Reverse patches", "", false),
            toggle("unique_points", "Unique points", "", false),
            toggle("update_point_normals", "Update point normals", "", false),
            text("patch_group", "Patch group", "", ""),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_boundary_group = vs(values, 0);
        let p_mode = MODE.of(&values[1]);
        let p_reverse_patches = vb(values, 2);
        let p_unique_points = vb(values, 3);
        let p_update_point_normals = vb(values, 4);
        let p_patch_group = vs(values, 5);
        let boundary_group = support::optional_text(&p_boundary_group);
        let patch_group = support::optional_text(&p_patch_group);
        Ok(make(
            label,
            "poly_fill",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let boundary = find_edges(&boundary_group, geometry, "missing_group", "poly_fill could not find boundary edge group")?;
                done(rdk_ops::poly_fill::run(
                    &rdk_ops::poly_fill::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        boundary,
                        mode: p_mode,
                        reverse_patches: p_reverse_patches,
                        unique_points: p_unique_points,
                        update_point_normals: p_update_point_normals,
                        patch_group: patch_group.clone(),
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Remesh

pub mod remesh {
    use super::*;

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_target_length = vf(values, 0);
        let p_smoothing = vf(values, 2);
        let p_preserve_uv_seams = vb(values, 8);
        let p_uv_attribute = vs(values, 9);
        if p_target_length <= 0. {
            return refuse("remesh", "target length must be finite and positive");
        }
        if p_smoothing < 0. || p_smoothing > 1. {
            return refuse("remesh", "smoothing must be finite and within [0,1]");
        }
        if p_preserve_uv_seams && blank(&p_uv_attribute) {
            return refuse("remesh", "empty UV attribute name");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "remesh",
        label: "Remesh",
        category: "Topology/Remesh",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            float("target_length", "Target length", "", 0.1, (0.0001, 10.), (Some(0.), None)),
            int("iterations", "Iterations", "", 3, (1, 32), Some(0)),
            float("smoothing", "Smoothing", "", 0.5, (0., 1.), (None, None)),
            toggle("project", "Project to surface", "", true),
            toggle("use_input_points_only", "Use input points only", "", false),
            text("hard_point_group", "Hard points", "Constraints", ""),
            text("hard_edge_group", "Hard edges", "Constraints", ""),
            text("target_size_attribute", "Target size attribute", "Adaptivity", ""),
            toggle("preserve_uv_seams", "Preserve UV seams", "UV", true),
            text("uv_attribute", "UV attribute", "UV", "uv"),
            text("output_hard_edges", "Output hard edges", "Output", ""),
            text("output_mesh_size", "Output mesh size", "Output", ""),
            text("output_quality", "Output quality", "Output", ""),
            toggle("recompute_point_normals", "Recompute point normals", "Output", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_target_length = vf(values, 0);
        let p_iterations = vi(values, 1);
        let p_smoothing = vf(values, 2);
        let p_project = vb(values, 3);
        let p_use_input_points_only = vb(values, 4);
        let p_hard_point_group = vs(values, 5);
        let p_hard_edge_group = vs(values, 6);
        let p_target_size_attribute = vs(values, 7);
        let p_preserve_uv_seams = vb(values, 8);
        let p_uv_attribute = vs(values, 9);
        let p_output_hard_edges = vs(values, 10);
        let p_output_mesh_size = vs(values, 11);
        let p_output_quality = vs(values, 12);
        let p_recompute_point_normals = vb(values, 13);
        let hard_point_group = support::optional_text(&p_hard_point_group);
        let hard_edge_group = support::optional_text(&p_hard_edge_group);
        let target_size_attribute = support::optional_text(&p_target_size_attribute);
        let output_hard_edges = support::optional_text(&p_output_hard_edges);
        let output_mesh_size = support::optional_text(&p_output_mesh_size);
        let output_quality = support::optional_text(&p_output_quality);
        Ok(make(
            label,
            "remesh",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let hard_points = support::resolve_optional_point_group("remesh", &hard_point_group, geometry)?;
                let hard_edges = support::resolve_optional_edge_group("remesh", &hard_edge_group, geometry)?;
                done(rdk_ops::remesh::run(
                    &rdk_ops::remesh::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        iterations: p_iterations,
                        smoothing: p_smoothing,
                        project: p_project,
                        use_input_points_only: p_use_input_points_only,
                        hard_points,
                        hard_edges,
                        target_size_attribute: target_size_attribute.clone(),
                        preserve_uv_seams: p_preserve_uv_seams,
                        uv_attribute: p_uv_attribute.clone(),
                        output_hard_edges: output_hard_edges.clone(),
                        output_mesh_size: output_mesh_size.clone(),
                        output_quality: output_quality.clone(),
                        recompute_point_normals: p_recompute_point_normals,
                        target_length: p_target_length,
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Convex_hull

pub mod convex_hull {
    use super::*;
    use rdk_ops::group_ops::Owner;

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_source_point_attribute = vs(values, 3);
        if p_source_point_attribute.trim() == "P" {
            return refuse("convex_hull", "source point attribute must not be P");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "convex_hull",
        label: "Convex Hull",
        category: "Topology",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            choice("group_owner", "Group type", "", support::ELEMENT_OWNER.kind(), support::ELEMENT_OWNER.value(&Owner::GroupPoints)),
            text("group", "Group", "", ""),
            toggle("preserve_point_payload", "Preserve point payload", "", true),
            text("source_point_attribute", "Source point attribute", "Output", "sourcepoint"),
            text("hull_group", "Hull group", "Output", "hull"),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group_owner = support::ELEMENT_OWNER.of(&values[0]);
        let p_group = vs(values, 1);
        let p_preserve_point_payload = vb(values, 2);
        let p_source_point_attribute = vs(values, 3);
        let p_hull_group = vs(values, 4);
        let selection = support::optional_element_group(p_group_owner, &p_group);
        let source_point_attribute = support::optional_text(&p_source_point_attribute);
        let hull_group = support::optional_text(&p_hull_group);
        Ok(make(
            label,
            "convex_hull",
            1,
            CookMode::Generic,
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let selection = support::resolve_element_group("convex_hull", &selection, &inputs[0])?;
                done(rdk_ops::convex_hull::run(
                    &rdk_ops::convex_hull::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        selection,
                        preserve_point_payload: p_preserve_point_payload,
                        source_point_attribute: source_point_attribute.clone(),
                        hull_group: hull_group.clone(),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Clean

pub mod clean {
    use super::*;
    use support::{CleanOverlap, KernelMode};

    const OVERLAPS: Choice<CleanOverlap> = Choice(&[
        ("Keep first", CleanOverlap::KeepFirst),
        ("Delete pairs", CleanOverlap::DeletePairs),
        ("Auto", CleanOverlap::Auto),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_epsilon = vf(values, 1);
        let p_consolidate_distance = vf(values, 4);
        let p_point_attributes = vs(values, 10);
        let p_vertex_attributes = vs(values, 11);
        let p_primitive_attributes = vs(values, 12);
        let p_detail_attributes = vs(values, 13);
        let p_point_groups = vs(values, 14);
        let p_vertex_groups = vs(values, 15);
        let p_primitive_groups = vs(values, 16);
        let p_edge_groups = vs(values, 17);
        if !p_epsilon.is_finite() || p_epsilon < 0. || !p_consolidate_distance.is_finite() || p_consolidate_distance < 0. {
            return refuse("clean", "epsilon and consolidate distance must be finite and nonnegative");
        }
        for pattern in [
            &p_point_attributes,
            &p_vertex_attributes,
            &p_primitive_attributes,
            &p_detail_attributes,
            &p_point_groups,
            &p_vertex_groups,
            &p_primitive_groups,
            &p_edge_groups,
        ] {
            if !blank(pattern) {
                support::validate_parameter_pattern("sop/clean", pattern)?;
            }
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "clean",
        label: "Clean",
        category: "Topology/Cleanup",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        let explicit = || support::KERNEL_MODE.value(&KernelMode::Explicit);
        vec![
            choice("epsilon_mode", "Epsilon mode", "Robustness", support::KERNEL_MODE.kind(), explicit()),
            float("epsilon", "Epsilon", "Robustness", 1e-9, (0., 0.001), (Some(0.), None)),
            toggle("remove_degenerate", "Remove degenerate primitives", "", true),
            choice("consolidate_mode", "Consolidate mode", "", support::KERNEL_MODE.kind(), explicit()),
            float("consolidate_distance", "Consolidate distance", "", 0., (0., 0.1), (Some(0.), None)),
            choice("overlaps", "Overlaps", "", OVERLAPS.kind(), OVERLAPS.at(0)),
            toggle("reverse_winding", "Reverse winding", "", false),
            toggle("remove_nan_points", "Remove non-finite points", "", true),
            toggle("remove_unused_points", "Remove unused points", "", true),
            toggle("delete_unused_groups", "Delete unused groups", "", true),
            text("point_attributes", "Point attributes", "Delete attributes", ""),
            text("vertex_attributes", "Vertex attributes", "Delete attributes", ""),
            text("primitive_attributes", "Primitive attributes", "Delete attributes", ""),
            text("detail_attributes", "Detail attributes", "Delete attributes", ""),
            text("point_groups", "Point groups", "Delete groups", ""),
            text("vertex_groups", "Vertex groups", "Delete groups", ""),
            text("primitive_groups", "Primitive groups", "Delete groups", ""),
            text("edge_groups", "Edge groups", "Delete groups", ""),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_epsilon_mode = support::KERNEL_MODE.of(&values[0]);
        let p_epsilon = vf(values, 1);
        let p_remove_degenerate = vb(values, 2);
        let p_consolidate_mode = support::KERNEL_MODE.of(&values[3]);
        let p_consolidate_distance = vf(values, 4);
        let p_overlaps = OVERLAPS.of(&values[5]);
        let p_reverse_winding = vb(values, 6);
        let p_remove_nan_points = vb(values, 7);
        let p_remove_unused_points = vb(values, 8);
        let p_delete_unused_groups = vb(values, 9);
        let p_point_attributes = vs(values, 10);
        let p_vertex_attributes = vs(values, 11);
        let p_primitive_attributes = vs(values, 12);
        let p_detail_attributes = vs(values, 13);
        let p_point_groups = vs(values, 14);
        let p_vertex_groups = vs(values, 15);
        let p_primitive_groups = vs(values, 16);
        let p_edge_groups = vs(values, 17);
        let epsilon = if p_epsilon_mode == KernelMode::Auto { 1e-12 } else { p_epsilon };
        let consolidate_distance = if p_consolidate_mode == KernelMode::Auto { None } else { Some(p_consolidate_distance) };
        let overlaps = match p_overlaps {
            CleanOverlap::Auto => None,
            CleanOverlap::KeepFirst => Some(rdk_ops::clean::OverlapPolicy::KeepFirstOverlap),
            CleanOverlap::DeletePairs => Some(rdk_ops::clean::OverlapPolicy::DeleteOverlapPairs),
        };
        let (point_attributes, vertex_attributes, primitive_attributes, detail_attributes) = (
            support::optional_text(&p_point_attributes),
            support::optional_text(&p_vertex_attributes),
            support::optional_text(&p_primitive_attributes),
            support::optional_text(&p_detail_attributes),
        );
        let (point_groups, vertex_groups, primitive_groups, edge_groups) = (
            support::optional_text(&p_point_groups),
            support::optional_text(&p_vertex_groups),
            support::optional_text(&p_primitive_groups),
            support::optional_text(&p_edge_groups),
        );
        Ok(make(
            label,
            "clean",
            2,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                done(rdk_ops::clean::run(
                    &rdk_ops::clean::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        epsilon,
                        remove_degenerate: p_remove_degenerate,
                        consolidate_distance,
                        overlaps,
                        reverse_winding: p_reverse_winding,
                        remove_nan_points: p_remove_nan_points,
                        remove_unused_points: p_remove_unused_points,
                        delete_unused_groups: p_delete_unused_groups,
                        point_attributes: point_attributes.clone(),
                        vertex_attributes: vertex_attributes.clone(),
                        primitive_attributes: primitive_attributes.clone(),
                        detail_attributes: detail_attributes.clone(),
                        point_groups: point_groups.clone(),
                        vertex_groups: vertex_groups.clone(),
                        primitive_groups: primitive_groups.clone(),
                        edge_groups: edge_groups.clone(),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Join_curves

pub mod join_curves {
    use super::*;

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_group = vs(values, 0);
        let p_picked_ends = vs(values, 1);
        let p_connect_closest_ends = vb(values, 3);
        let p_group_size = vi(values, 6);
        let p_tolerance = vf(values, 8);
        if p_group_size < 1 {
            return refuse("join_curves", "group size must be positive");
        }
        if !p_tolerance.is_finite() || p_tolerance < 0. {
            return refuse("join_curves", "tolerance must be finite and nonnegative");
        }
        if support::decode_curve_join_picks(&p_picked_ends)?.is_some() && (!blank(&p_group) || p_connect_closest_ends) {
            return refuse("join_curves", "picked ends are mutually exclusive with group and closest ordering");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "join_curves",
        label: "Join Curves",
        category: "Topology/Curve",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Primitive group", "", ""),
            Spec {
                description: Some("Comma-separated primitive:start or primitive:end pairs, in traversal order; [] selects no curves."),
                ..text("picked_ends", "Picked ends", "", "")
            },
            toggle("orient_closest", "Orient closest ends", "", true),
            toggle("connect_closest_ends", "Connect closest ends", "", false),
            toggle("only_connected", "Only connected", "", false),
            toggle("use_group_size", "Use group size", "", false),
            int("group_size", "Group size", "", 2, (1, 1024), Some(1)),
            toggle("keep_originals", "Keep originals", "", false),
            float("tolerance", "Tolerance", "", 0., (0., 1.), (Some(0.), None)),
            toggle("wrap", "Wrap", "", false),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_picked_ends = vs(values, 1);
        let p_orient_closest = vb(values, 2);
        let p_connect_closest_ends = vb(values, 3);
        let p_only_connected = vb(values, 4);
        let p_use_group_size = vb(values, 5);
        let p_group_size = vi(values, 6);
        let p_keep_originals = vb(values, 7);
        let p_tolerance = vf(values, 8);
        let p_wrap = vb(values, 9);
        let group = support::optional_text(&p_group);
        let picked_ends = support::decode_curve_join_picks(&p_picked_ends).expect("picked ends were validated");
        let group_size = if p_use_group_size { Some(p_group_size) } else { None };
        Ok(make(
            label,
            "join_curves",
            4,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let primitives = match &group {
                    None => None,
                    Some(name) => match geometry::find_group(rdk_core::group::Owner::Primitive, name, &inputs[0]) {
                        Some(found) => Some(found),
                        None => {
                            return Err(support::diagnostic_with(
                                "missing_group",
                                &format!("join_curves could not find primitive group {}", node::ocaml_quote(name)),
                                None,
                                vec!["Create the primitive group before Curve Join".to_owned()],
                            ));
                        }
                    },
                };
                done(rdk_ops::curve_topology::join_curves(
                    &rdk_ops::curve_topology::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        primitives,
                        picked_ends: picked_ends.clone(),
                        orient_closest: Some(p_orient_closest),
                        connect_closest_ends: Some(p_connect_closest_ends),
                        only_connected: Some(p_only_connected),
                        group_size,
                        keep_originals: Some(p_keep_originals),
                        tolerance: Some(p_tolerance),
                        wrap: Some(p_wrap),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Facet

pub mod facet {
    use super::*;
    use rdk_ops::group_ops::Owner;
    use support::{FacetConsolidation, KernelMode};

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_consolidate_distance = vf(values, 6);
        let p_consolidate_normals_distance = vf(values, 7);
        let p_inline_distance = vf(values, 9);
        let p_cusp_angle = vf(values, 12);
        if ![p_consolidate_distance, p_consolidate_normals_distance, p_inline_distance, p_cusp_angle].iter().all(|v| v.is_finite()) {
            return refuse("facet", "distance and angle controls must be finite");
        }
        if p_consolidate_distance < 0. || p_consolidate_normals_distance < 0. || p_inline_distance < 0. {
            return refuse("facet", "distances must be nonnegative");
        }
        if p_cusp_angle < 0. || p_cusp_angle > std::f64::consts::PI {
            return refuse("facet", "cusp angle must be within [0, pi]");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "facet",
        label: "Facet",
        category: "Topology/Cleanup",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        let pi = std::f64::consts::PI;
        vec![
            text("group", "Primitive group", "", ""),
            choice("group_owner", "Group owner", "", support::ELEMENT_OWNER.kind(), support::ELEMENT_OWNER.value(&Owner::GroupPrimitives)),
            choice(
                "consolidation",
                "Consolidation",
                "Consolidate",
                support::FACET_CONSOLIDATION.kind(),
                support::FACET_CONSOLIDATION.at(0),
            ),
            toggle("pre_compute_normals", "Pre-compute normals", "Normals", false),
            toggle("make_normals_unit_length", "Make normals unit length", "Normals", false),
            toggle("unique_points", "Unique points", "", false),
            float("consolidate_distance", "Consolidate distance", "Consolidate", 0., (0., 0.1), (Some(0.), None)),
            float("consolidate_normals_distance", "Normal distance", "Consolidate", 0., (0., 0.1), (Some(0.), None)),
            toggle("remove_inline_points", "Remove inline points", "", false),
            float("inline_distance", "Inline distance", "", 1e-6, (0., 0.1), (Some(0.), None)),
            toggle("orient_polygons", "Orient polygons", "", false),
            choice("cusp_mode", "Cusp mode", "Normals", support::KERNEL_MODE.kind(), support::KERNEL_MODE.value(&KernelMode::Explicit)),
            float("cusp_angle", "Cusp angle", "Normals", pi, (0., pi), (Some(0.), Some(pi))),
            toggle("remove_degenerate", "Remove degenerate primitives", "", false),
            toggle("make_planar", "Make planar", "", false),
            toggle("post_compute_normals", "Post-compute normals", "Normals", false),
            toggle("reverse_normals", "Reverse normals", "Normals", false),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_group_owner = support::ELEMENT_OWNER.of(&values[1]);
        let p_consolidation = support::FACET_CONSOLIDATION.of(&values[2]);
        let p_pre_compute_normals = vb(values, 3);
        let p_make_normals_unit_length = vb(values, 4);
        let p_unique_points = vb(values, 5);
        let p_consolidate_distance = vf(values, 6);
        let p_consolidate_normals_distance = vf(values, 7);
        let p_remove_inline_points = vb(values, 8);
        let p_inline_distance = vf(values, 9);
        let p_orient_polygons = vb(values, 10);
        let p_cusp_mode = support::KERNEL_MODE.of(&values[11]);
        let p_cusp_angle = vf(values, 12);
        let p_remove_degenerate = vb(values, 13);
        let p_make_planar = vb(values, 14);
        let p_post_compute_normals = vb(values, 15);
        let p_reverse_normals = vb(values, 16);
        let selection = support::optional_element_group(p_group_owner, &p_group);
        let consolidate_distance = (p_consolidation == FacetConsolidation::Points).then_some(p_consolidate_distance);
        let consolidate_normals_distance =
            (p_consolidation == FacetConsolidation::Normals).then_some(p_consolidate_normals_distance);
        let cusp_angle = match p_cusp_mode {
            KernelMode::Explicit => Some(p_cusp_angle),
            KernelMode::Auto => None,
        };
        Ok(make(
            label,
            "facet",
            4,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let selection = support::resolve_element_group("facet", &selection, geometry)?;
                done(rdk_ops::facet::run(
                    &rdk_ops::facet::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        selection,
                        pre_compute_normals: Some(p_pre_compute_normals),
                        make_normals_unit_length: Some(p_make_normals_unit_length),
                        unique_points: Some(p_unique_points),
                        consolidate_distance,
                        consolidate_normals_distance,
                        remove_inline_points: Some(p_remove_inline_points),
                        inline_distance: Some(p_inline_distance),
                        orient_polygons: Some(p_orient_polygons),
                        cusp_angle,
                        remove_degenerate: Some(p_remove_degenerate),
                        make_planar: Some(p_make_planar),
                        post_compute_normals: Some(p_post_compute_normals),
                        reverse_normals: Some(p_reverse_normals),
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Poly_cut

pub mod poly_cut {
    use super::*;
    use rdk_ops::poly_cut::{Detection, Element, Strategy};
    use support::PolyCutDetection;

    const ELEMENT: Choice<Element> = Choice(&[
        ("Points", Element::PolyCutPoints),
        ("Edges", Element::PolyCutEdges),
    ]);
    const STRATEGY: Choice<Strategy> = Choice(&[
        ("Remove", Strategy::PolyCutRemove),
        ("Cut", Strategy::PolyCutCut),
    ]);
    const DETECTION: Choice<PolyCutDetection> = Choice(&[
        ("All selected", PolyCutDetection::All),
        ("Attribute crossing", PolyCutDetection::Crossing),
        ("Attribute change", PolyCutDetection::Change),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_strategy = STRATEGY.of(&values[3]);
        let p_detection = DETECTION.of(&values[4]);
        let p_attribute = vs(values, 5);
        let p_value = vf(values, 6);
        let p_threshold = vf(values, 7);
        if !(p_value.is_finite() && p_threshold.is_finite()) {
            return refuse("poly_cut", "detection controls must be finite");
        }
        if p_threshold < 0. {
            return refuse("poly_cut", "threshold must be nonnegative");
        }
        if p_detection != PolyCutDetection::All && blank(&p_attribute) {
            return refuse("poly_cut", "attribute must be nonblank");
        }
        if p_detection == PolyCutDetection::Crossing && p_attribute == "P" {
            return refuse("poly_cut", "crossing requires scalar storage");
        }
        if p_detection == PolyCutDetection::Change && p_strategy == Strategy::PolyCutCut && p_threshold == 0. {
            return refuse("poly_cut", "cut-at-change requires a positive threshold");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "poly_cut",
        label: "Poly Cut",
        category: "Topology/Curve",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Primitive group", "", ""),
            text("cut_group", "Cut group", "", ""),
            choice("element", "Cut elements", "", ELEMENT.kind(), ELEMENT.at(0)),
            choice("strategy", "Strategy", "", STRATEGY.kind(), STRATEGY.at(0)),
            choice("detection", "Detection", "Detection", DETECTION.kind(), DETECTION.at(0)),
            text("attribute", "Attribute", "Detection", "cut"),
            float("value", "Crossing value", "Detection", 0.5, (-10., 10.), (None, None)),
            float("threshold", "Change threshold", "Detection", 0., (0., 10.), (Some(0.), None)),
            toggle("keep_closed", "Keep closed", "", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_cut_group = vs(values, 1);
        let p_element = ELEMENT.of(&values[2]);
        let p_strategy = STRATEGY.of(&values[3]);
        let p_detection = DETECTION.of(&values[4]);
        let p_attribute = vs(values, 5);
        let p_value = vf(values, 6);
        let p_threshold = vf(values, 7);
        let p_keep_closed = vb(values, 8);
        let group = support::optional_text(&p_group);
        let cut_group = support::optional_text(&p_cut_group);
        let detection = match p_detection {
            PolyCutDetection::All => Detection::PolyCutAll,
            PolyCutDetection::Crossing => Detection::PolyCutCrossing { attribute: p_attribute.clone(), value: p_value },
            PolyCutDetection::Change => Detection::PolyCutChange { attribute: p_attribute.clone(), threshold: p_threshold },
        };
        Ok(make(
            label,
            "poly_cut",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let primitives = support::resolve_optional_primitive_group("poly_cut", &group, geometry)?;
                let (cut_points, cut_edges) = match p_element {
                    Element::PolyCutPoints => {
                        (support::resolve_optional_point_group("poly_cut", &cut_group, geometry)?, None)
                    }
                    Element::PolyCutEdges => {
                        (None, support::resolve_optional_edge_group("poly_cut", &cut_group, geometry)?)
                    }
                };
                done(rdk_ops::poly_cut::cut(
                    &rdk_ops::poly_cut::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        primitives,
                        cut_points,
                        cut_edges,
                        element: p_element,
                        strategy: p_strategy,
                        detection: detection.clone(),
                        keep_closed: p_keep_closed,
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Poly_reduce

pub mod poly_reduce {
    use super::*;
    use rdk_ops::poly_reduce::Target;
    use support::PolyReduceTarget;

    const TARGET: Choice<PolyReduceTarget> = Choice(&[
        ("Percentage", PolyReduceTarget::Ratio),
        ("Primitive count", PolyReduceTarget::PrimitiveCount),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_ratio = vf(values, 4);
        let p_primitive_count = vi(values, 5);
        let p_equalize_lengths = vf(values, 8);
        let p_max_normal_deviation = vf(values, 10);
        if !(p_ratio.is_finite() && p_equalize_lengths.is_finite() && p_max_normal_deviation.is_finite()) {
            return refuse("poly_reduce", "controls must be finite");
        }
        if p_ratio < 0. || p_ratio > 1. {
            return refuse("poly_reduce", "ratio must be in [0, 1]");
        }
        if p_primitive_count < 0 {
            return refuse("poly_reduce", "primitive count must be nonnegative");
        }
        if p_equalize_lengths < 0. {
            return refuse("poly_reduce", "equalize lengths must be nonnegative");
        }
        if p_max_normal_deviation < 0. || p_max_normal_deviation > std::f64::consts::PI {
            return refuse("poly_reduce", "normal deviation must be in [0, pi]");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "poly_reduce",
        label: "PolyReduce",
        category: "Topology/Remesh",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Primitive group", "", ""),
            text("hard_point_group", "Hard point group", "Constraints", ""),
            text("hard_edge_group", "Hard edge group", "Constraints", ""),
            choice("target_mode", "Target", "", TARGET.kind(), TARGET.at(0)),
            float("ratio", "Percentage", "", 0.5, (0., 1.), (Some(0.), Some(1.))),
            int("primitive_count", "Primitive count", "", 100, (0, 1_000_000), Some(0)),
            toggle("preserve_boundary", "Preserve boundary", "", true),
            toggle("only_original_positions", "Only original positions", "", false),
            float("equalize_lengths", "Equalize edge lengths", "", 0.0000000001, (0., 1.), (Some(0.), None)),
            toggle("limit_normal_deviation", "Limit normal deviation", "", false),
            float("max_normal_deviation", "Maximum normal deviation", "", 0.5, (0., std::f64::consts::PI), (Some(0.), None)),
            text("output_group", "Output group", "", ""),
            toggle("recompute_point_normals", "Recompute point normals", "", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_hard_point_group = vs(values, 1);
        let p_hard_edge_group = vs(values, 2);
        let p_target_mode = TARGET.of(&values[3]);
        let p_ratio = vf(values, 4);
        let p_primitive_count = vi(values, 5);
        let p_preserve_boundary = vb(values, 6);
        let p_only_original_positions = vb(values, 7);
        let p_equalize_lengths = vf(values, 8);
        let p_limit_normal_deviation = vb(values, 9);
        let p_max_normal_deviation = vf(values, 10);
        let p_output_group = vs(values, 11);
        let p_recompute_point_normals = vb(values, 12);
        let group = support::optional_text(&p_group);
        let hard_point_group = support::optional_text(&p_hard_point_group);
        let hard_edge_group = support::optional_text(&p_hard_edge_group);
        let output_group = support::optional_text(&p_output_group);
        let target = match p_target_mode {
            PolyReduceTarget::Ratio => Target::ReduceRatio(p_ratio),
            PolyReduceTarget::PrimitiveCount => Target::ReducePrimitiveCount(p_primitive_count),
        };
        let max_normal_deviation = p_limit_normal_deviation.then_some(p_max_normal_deviation);
        Ok(make(
            label,
            "poly_reduce",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let primitives = find_primitives(&group, geometry, "missing_group", "poly_reduce could not find primitive group");
                let hard_points = find_group(
                    rdk_core::group::Owner::Point,
                    &hard_point_group,
                    geometry,
                    "missing_group",
                    "poly_reduce could not find hard point group",
                );
                let hard_edges = find_edges(&hard_edge_group, geometry, "missing_group", "poly_reduce could not find hard edge group");
                let (primitives, hard_points, hard_edges) = (primitives?, hard_points?, hard_edges?);
                done(rdk_ops::poly_reduce::run(
                    &rdk_ops::poly_reduce::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        target,
                        primitives,
                        hard_points,
                        hard_edges,
                        preserve_boundary: p_preserve_boundary,
                        only_original_positions: p_only_original_positions,
                        equalize_lengths: p_equalize_lengths,
                        max_normal_deviation,
                        output_group: output_group.clone(),
                        recompute_point_normals: p_recompute_point_normals,
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- shared Boolean tables

use rdk_boolean::boolean_materialization::Detriangulation;
use rdk_boolean::boolean_payload::PointConflict;
use rdk_boolean::boolean_solid::Treatment;

const TREATMENT: Choice<Treatment> = Choice(&[
    ("Solid", Treatment::Solid),
    ("Surface", Treatment::Surface),
]);
const CONFLICT: Choice<PointConflict> = Choice(&[
    ("Reject conflict", PointConflict::Reject),
    ("Promote to vertex", PointConflict::PromoteToVertex),
]);
const DETRIANGULATION: Choice<Detriangulation> = Choice(&[
    ("Triangles", Detriangulation::Triangles),
    ("Unchanged polygons", Detriangulation::UnchangedPolygons),
    ("All polygons", Detriangulation::AllPolygons),
]);

// ---------------------------------------------------------------- Boolean_seam

pub mod boolean_seam {
    use super::*;
    use rdk_boolean::boolean::SeamOutput;

    const OUTPUT: Choice<SeamOutput> = Choice(&[
        ("Seam curves", SeamOutput::SeamCurves),
        ("Coincident patches", SeamOutput::CoincidentPatches),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_output = OUTPUT.of(&values[0]);
        let p_left_self_group = vs(values, 5);
        let p_between_group = vs(values, 6);
        let p_right_self_group = vs(values, 7);
        let p_coincident_group = vs(values, 8);
        let names = match p_output {
            SeamOutput::SeamCurves => vec![p_left_self_group, p_between_group, p_right_self_group],
            SeamOutput::CoincidentPatches => vec![p_coincident_group],
        };
        let names: Vec<String> = names.iter().filter_map(|name| support::optional_text(name)).collect();
        if has_duplicates(&names) {
            return refuse("boolean_seam", "output group names must be distinct");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "boolean_seam",
        label: "Boolean Seam",
        category: "Boolean/Analysis",
        inputs: 2,
        slots: Some("left, right"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            choice("output", "Output", "", OUTPUT.kind(), OUTPUT.at(0)),
            choice("left_treatment", "A treatment", "Operands", TREATMENT.kind(), TREATMENT.at(0)),
            choice("right_treatment", "B treatment", "Operands", TREATMENT.kind(), TREATMENT.at(0)),
            toggle("resolve_left_self_intersections", "Resolve A self-intersections", "Operands", false),
            toggle("resolve_right_self_intersections", "Resolve B self-intersections", "Operands", false),
            text("left_self_group", "A self group", "Groups", "boolean_left_self_seam"),
            text("between_group", "Between group", "Groups", "boolean_seam"),
            text("right_self_group", "B self group", "Groups", "boolean_right_self_seam"),
            text("coincident_group", "Coincident group", "Groups", "boolean_coincident"),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_output = OUTPUT.of(&values[0]);
        let p_left_treatment = TREATMENT.of(&values[1]);
        let p_right_treatment = TREATMENT.of(&values[2]);
        let p_resolve_left_self_intersections = vb(values, 3);
        let p_resolve_right_self_intersections = vb(values, 4);
        let p_left_self_group = vs(values, 5);
        let p_between_group = vs(values, 6);
        let p_right_self_group = vs(values, 7);
        let p_coincident_group = vs(values, 8);
        let (left_self_group, between_group, right_self_group, coincident_group) = (
            support::optional_text(&p_left_self_group),
            support::optional_text(&p_between_group),
            support::optional_text(&p_right_self_group),
            support::optional_text(&p_coincident_group),
        );
        Ok(make(
            label,
            "boolean_seam",
            1,
            CookMode::Generic,
            vec![first_input(inputs), inputs[1].clone().expect("required input")],
            Arc::new(move |context, inputs| {
                done(rdk_boolean::boolean::seam(
                    &rdk_boolean::boolean::SeamParams {
                        cancel: cancel(context),
                        grain: grain(context),
                        output: p_output,
                        left_treatment: p_left_treatment,
                        right_treatment: p_right_treatment,
                        resolve_left_self_intersections: p_resolve_left_self_intersections,
                        resolve_right_self_intersections: p_resolve_right_self_intersections,
                        left_self_group: left_self_group.clone(),
                        between_group: between_group.clone(),
                        right_self_group: right_self_group.clone(),
                        coincident_group: coincident_group.clone(),
                        right: inputs[1].clone(),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Poly_bevel

pub mod poly_bevel {
    use super::*;
    use rdk_ops::poly_bevel::Shape;
    use support::PolyBevelShape;

    const SHAPE: Choice<PolyBevelShape> = Choice(&[
        ("Chamfer", PolyBevelShape::Chamfer),
        ("Round", PolyBevelShape::Round),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_convexity = vf(values, 2);
        let p_distance = vf(values, 3);
        let p_divisions = vi(values, 4);
        let p_ignore_flat_angle = vf(values, 6);
        if ![p_convexity, p_distance, p_ignore_flat_angle].iter().all(|v| v.is_finite()) {
            return refuse("poly_bevel", "numeric controls must be finite");
        }
        if p_distance < 0. {
            return refuse("poly_bevel", "distance must be nonnegative");
        }
        if p_divisions < 1 {
            return refuse("poly_bevel", "divisions must be positive");
        }
        if p_convexity < -1. || p_convexity > 1. {
            return refuse("poly_bevel", "convexity must be in [-1, 1]");
        }
        if p_ignore_flat_angle < 0. || p_ignore_flat_angle > std::f64::consts::PI {
            return refuse("poly_bevel", "flatness angle must be in [0, pi]");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "poly_bevel",
        label: "Poly Bevel",
        category: "Topology/Polygon",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Edge group", "", ""),
            choice("shape", "Shape", "", SHAPE.kind(), SHAPE.at(0)),
            float("convexity", "Convexity", "Round", 0.5, (0., 1.), (None, None)),
            float("distance", "Distance", "", 0.1, (0., 2.), (Some(0.), None)),
            int("divisions", "Divisions", "", 1, (1, 32), Some(1)),
            text("point_scale_attribute", "Point scale attribute", "Attributes", ""),
            float("ignore_flat_angle", "Ignore flat angle (0: bevel every edge)", "Robustness", 0., (0., 3.14159), (Some(0.), None)),
            toggle("clamp_overlap", "Clamp overlap", "Robustness", true),
            text("edge_group", "Edge group", "Output groups", ""),
            text("corner_group", "Corner group", "Output groups", ""),
            text("offset_group", "Offset group", "Output groups", ""),
            toggle("recompute_point_normals", "Recompute point normals", "", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_shape = SHAPE.of(&values[1]);
        let p_convexity = vf(values, 2);
        let p_distance = vf(values, 3);
        let p_divisions = vi(values, 4);
        let p_point_scale_attribute = vs(values, 5);
        let p_ignore_flat_angle = vf(values, 6);
        let p_clamp_overlap = vb(values, 7);
        let p_edge_group = vs(values, 8);
        let p_corner_group = vs(values, 9);
        let p_offset_group = vs(values, 10);
        let p_recompute_point_normals = vb(values, 11);
        let shape = match p_shape {
            PolyBevelShape::Chamfer => Shape::BevelChamfer,
            PolyBevelShape::Round => Shape::BevelRound { convexity: p_convexity },
        };
        let group = support::optional_text(&p_group);
        let point_scale_attribute = support::optional_text(&p_point_scale_attribute);
        let (edge_group, corner_group, offset_group) = (
            support::optional_text(&p_edge_group),
            support::optional_text(&p_corner_group),
            support::optional_text(&p_offset_group),
        );
        let ignore_flat_angle = (p_ignore_flat_angle > 0.).then_some(p_ignore_flat_angle);
        Ok(make(
            label,
            "poly_bevel",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let edges = find_edges(&group, geometry, "missing_edge_group", "poly_bevel could not find edge group")?;
                done(rdk_ops::poly_bevel::run(
                    &rdk_ops::poly_bevel::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        edges,
                        shape,
                        divisions: p_divisions,
                        point_scale_attribute: point_scale_attribute.clone(),
                        ignore_flat_angle,
                        clamp_overlap: p_clamp_overlap,
                        edge_group: edge_group.clone(),
                        corner_group: corner_group.clone(),
                        offset_group: offset_group.clone(),
                        recompute_point_normals: p_recompute_point_normals,
                        distance: p_distance,
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Boolean_fracture

pub mod boolean_fracture {
    use super::*;
    use rdk_boolean::boolean_solid::Operation;

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_piece_attribute = vs(values, 5);
        let p_point_tolerance = vf(values, 6);
        let p_tiny_seam_threshold = vf(values, 7);
        let p_cleanup_max_batches = vi(values, 8);
        if !(p_point_tolerance.is_finite() && p_tiny_seam_threshold.is_finite())
            || p_point_tolerance < 0.
            || p_tiny_seam_threshold < 0.
        {
            return refuse("boolean_fracture", "tolerances must be finite and nonnegative");
        }
        if p_cleanup_max_batches < 1 {
            return refuse("boolean_fracture", "cleanup batches must be positive");
        }
        if blank(&p_piece_attribute) {
            return refuse("boolean_fracture", "piece attribute must be nonblank");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "boolean_fracture",
        label: "Boolean Fracture",
        category: "Boolean",
        inputs: 2,
        slots: Some("source, cutters"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            choice("point_conflict", "Point attribute conflicts", "Attributes", CONFLICT.kind(), CONFLICT.at(1)),
            toggle("assume_flat", "Assume flat", "Output", false),
            toggle("resolve_cutter_self_intersections", "Resolve cutter self-intersections", "", false),
            choice("detriangulation", "Polygons", "", DETRIANGULATION.kind(), DETRIANGULATION.at(0)),
            toggle("require_closed", "Require closed", "", true),
            text("piece_attribute", "Piece attribute", "", "piece"),
            float("point_tolerance", "Point tolerance", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            float("tiny_seam_threshold", "Tiny seam threshold", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            int("cleanup_max_batches", "Cleanup batches", "Robustness", 8, (1, 32), Some(1)),
            toggle("strict_cleanup", "Strict cleanup", "Robustness", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_point_conflict = CONFLICT.of(&values[0]);
        let p_assume_flat = vb(values, 1);
        let p_resolve_cutter_self_intersections = vb(values, 2);
        let p_detriangulation = DETRIANGULATION.of(&values[3]);
        let p_require_closed = vb(values, 4);
        let p_piece_attribute = vs(values, 5);
        let p_point_tolerance = vf(values, 6);
        let p_tiny_seam_threshold = vf(values, 7);
        let p_cleanup_max_batches = vi(values, 8);
        let p_strict_cleanup = vb(values, 9);
        Ok(make(
            label,
            "boolean_fracture",
            1,
            CookMode::Generic,
            vec![first_input(inputs), inputs[1].clone().expect("required input")],
            Arc::new(move |context, inputs| {
                done(rdk_boolean::boolean::run(
                    &rdk_boolean::boolean::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        operation: Operation::Difference,
                        left_treatment: Treatment::Solid,
                        right_treatment: Treatment::Surface,
                        resolve_left_self_intersections: false,
                        resolve_right_self_intersections: p_resolve_cutter_self_intersections,
                        point_conflict: p_point_conflict,
                        point_tolerance: p_point_tolerance,
                        tiny_seam_threshold: p_tiny_seam_threshold,
                        cleanup_max_batches: p_cleanup_max_batches,
                        strict_cleanup: p_strict_cleanup,
                        seam_points: rdk_boolean::boolean::SeamPoints::SharedSeamPoints,
                        detriangulation: p_detriangulation,
                        assume_flat: p_assume_flat,
                        require_closed: Some(p_require_closed),
                        piece_attribute: Some(p_piece_attribute.clone()),
                        left_piece_group: Some("boolean_left".to_owned()),
                        overlap_piece_group: Some("boolean_overlap".to_owned()),
                        right_piece_group: Some("boolean_right".to_owned()),
                        right: inputs[1].clone(),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Intersection_analysis

pub mod intersection_analysis {
    use super::*;

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_tolerance = vf(values, 2);
        let p_input_attribute = vs(values, 4);
        let p_primitive_attribute = vs(values, 5);
        let p_primitive_uvw_attribute = vs(values, 6);
        let p_point_attribute = vs(values, 7);
        if !p_tolerance.is_finite() || p_tolerance < 0. {
            return refuse("intersection_analysis", "tolerance must be finite and nonnegative");
        }
        let names: Vec<String> =
            [&p_input_attribute, &p_primitive_attribute, &p_primitive_uvw_attribute, &p_point_attribute]
                .iter()
                .filter_map(|name| support::optional_text(name))
                .collect();
        if names.iter().any(|name| name == "P") {
            return refuse("intersection_analysis", "output attributes must not replace P");
        }
        if has_duplicates(&names) {
            return refuse("intersection_analysis", "output attribute names must be distinct");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "intersection_analysis",
        label: "Intersection Analysis",
        category: "Boolean/Analysis",
        inputs: 2,
        slots: Some("input, collision"),
        optional: &[1],
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("source_group", "Source group", "Inputs", ""),
            text("collision_group", "Collision group", "Inputs", ""),
            float("tolerance", "Tolerance", "", 0., (0., 0.001), (Some(0.), None)),
            toggle("include_coplanar", "Include coplanar overlap", "", true),
            text("input_attribute", "Input attribute", "Output attributes", "sourceinput"),
            text("primitive_attribute", "Primitive attribute", "Output attributes", "sourceprim"),
            text("primitive_uvw_attribute", "Primitive UVW attribute", "Output attributes", "sourceprimuv"),
            text("point_attribute", "Point attribute", "Output attributes", "sourcepoint"),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_source_group = vs(values, 0);
        let p_collision_group = vs(values, 1);
        let p_tolerance = vf(values, 2);
        let p_include_coplanar = vb(values, 3);
        let p_input_attribute = vs(values, 4);
        let p_primitive_attribute = vs(values, 5);
        let p_primitive_uvw_attribute = vs(values, 6);
        let p_point_attribute = vs(values, 7);
        let collision = inputs[1].clone();
        let source_group = support::optional_text(&p_source_group);
        let collision_group = if collision.is_some() { support::optional_text(&p_collision_group) } else { None };
        let (input_attribute, primitive_attribute, primitive_uvw_attribute, point_attribute) = (
            support::optional_text(&p_input_attribute),
            support::optional_text(&p_primitive_attribute),
            support::optional_text(&p_primitive_uvw_attribute),
            support::optional_text(&p_point_attribute),
        );
        let mut nodes = vec![first_input(inputs)];
        nodes.extend(collision);
        Ok(make(
            label,
            "intersection_analysis",
            1,
            CookMode::Generic,
            nodes,
            Arc::new(move |context, inputs| {
                let source_primitives = support::resolve_optional_primitive_group("intersection_analysis", &source_group, &inputs[0])?;
                let collision_geometry = inputs.get(1).cloned();
                let group_geometry = collision_geometry.as_ref().unwrap_or(&inputs[0]);
                let collision_primitives = support::resolve_optional_primitive_group(
                    "intersection_analysis collision",
                    &collision_group,
                    group_geometry,
                )?;
                done(rdk_ops::intersection_analysis::run(
                    &rdk_ops::intersection_analysis::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        source_primitives,
                        collision_primitives,
                        tolerance: p_tolerance,
                        include_coplanar: p_include_coplanar,
                        input_attribute: input_attribute.clone(),
                        primitive_attribute: primitive_attribute.clone(),
                        primitive_uvw_attribute: primitive_uvw_attribute.clone(),
                        point_attribute: point_attribute.clone(),
                        collision: collision_geometry.clone(),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Fuse

pub mod fuse {
    use super::*;
    use rdk_ops::fuse_grid::FuseMetric;
    use rdk_ops::fuse_rules::DefaultAttributes;
    use rdk_geom::point_snap::{MatchCondition, Targeting, Using};
    use support::FuseTargeting;

    const TARGETING: Choice<FuseTargeting> = Choice(&[
        ("Near points", FuseTargeting::NearPoints),
        ("Specified points", FuseTargeting::SpecifiedPoints),
    ]);
    const USING: Choice<Using> = Choice(&[
        ("Least target point", Using::LeastTargetPoint),
        ("Closest target point", Using::ClosestTargetPoint),
    ]);
    const ATTRIBUTES: Choice<DefaultAttributes> = Choice(&[
        ("Keep first", DefaultAttributes::KeepFirst),
        ("Average numeric", DefaultAttributes::AverageNumeric),
    ]);
    const METRIC: Choice<FuseMetric> = Choice(&[
        ("Euclidean", FuseMetric::Euclidean),
        ("Componentwise", FuseMetric::Componentwise),
    ]);
    const CONDITION: Choice<MatchCondition> = Choice(&[
        ("Equal", MatchCondition::EqualAttributeValues),
        ("Unequal", MatchCondition::UnequalAttributeValues),
    ]);

    fn bytes(values: &[Value], index: usize) -> Vec<u8> {
        match &values[index] {
            Value::Text(value) => value.to_vec(),
            _ => unreachable!("parameter {index} is not text"),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_targeting = TARGETING.of(&values[2]);
        let p_target_attribute = vs(values, 3);
        let p_tolerance = vf(values, 5);
        let p_position = POSITION.of(&values[6]);
        let p_weight_attribute = vs(values, 7);
        let p_attribute_rules = bytes(values, 9);
        let p_group_rules = bytes(values, 10);
        let p_radius_attribute = vs(values, 14);
        let p_match_attribute = vs(values, 15);
        let p_match_condition = CONDITION.of(&values[16]);
        let p_match_tolerance = vf(values, 17);
        let p_fuse_points = vb(values, 19);
        let p_keep_fused_points = vb(values, 20);
        if !(p_tolerance.is_finite() && p_match_tolerance.is_finite()) || p_tolerance < 0. || p_match_tolerance < 0. {
            return refuse("fuse", "tolerances must be finite and nonnegative");
        }
        let specified = p_targeting == FuseTargeting::SpecifiedPoints;
        if specified && blank(&p_target_attribute) {
            return refuse("fuse", "specified targets require an attribute name");
        }
        if blank(&p_match_attribute) && (p_match_condition != MatchCondition::EqualAttributeValues || p_match_tolerance != 0.) {
            return refuse("fuse", "match condition and tolerance require a match attribute");
        }
        if specified && (!blank(&p_radius_attribute) || !blank(&p_match_attribute)) {
            return refuse("fuse", "specified targets do not accept radius or match attributes");
        }
        if p_keep_fused_points && !p_fuse_points {
            return refuse("fuse", "keeping fused points requires fusing points");
        }
        if matches!(
            p_position,
            Position::WeightedAverage | Position::WeightedSum | Position::MinimumWeight | Position::MaximumWeight
        ) && blank(&p_weight_attribute)
        {
            return refuse("fuse", "weighted position reduction requires a weight attribute");
        }
        support::decode_fuse_attribute_rules("sop/fuse", &p_attribute_rules)?;
        support::decode_fuse_group_rules("sop/fuse", &p_group_rules)?;
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "fuse",
        label: "Fuse",
        category: "Topology/Cleanup",
        inputs: 2,
        slots: Some("input, target"),
        optional: &[1],
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Point group", "", ""),
            text("target_group", "Target group", "", ""),
            choice("targeting", "Targeting", "", TARGETING.kind(), TARGETING.at(0)),
            text("target_attribute", "Target point attribute", "", "targetpoint"),
            choice("using", "Use target", "", USING.kind(), USING.at(0)),
            float("tolerance", "Snap distance", "", 0.001, (0., 10.), (Some(0.), None)),
            choice("position", "Position", "Fuse", POSITION.kind(), POSITION.value(&Position::Average)),
            text("weight_attribute", "Weight attribute", "Fuse", ""),
            choice("attributes", "Attributes", "Fuse", ATTRIBUTES.kind(), ATTRIBUTES.at(0)),
            Spec {
                description: Some(
                    "Escaped tab-separated rows: pattern, reduction method, weight attribute. Weighted methods require a weight name.",
                ),
                ..text("attribute_rules", "Attribute rules", "Fuse", "")
            },
            Spec {
                description: Some("Escaped tab-separated rows: group pattern, reduction method."),
                ..text("group_rules", "Group rules", "Fuse", "")
            },
            choice("metric", "Metric", "Matching", METRIC.kind(), METRIC.at(0)),
            toggle("inclusive", "Inclusive distance", "Matching", true),
            toggle("match_attributes", "Match attributes", "Matching", false),
            text("radius_attribute", "Radius attribute", "Matching", ""),
            text("match_attribute", "Match attribute", "Matching", ""),
            choice("match_condition", "Match condition", "Matching", CONDITION.kind(), CONDITION.at(0)),
            float("match_tolerance", "Match tolerance", "Matching", 0., (0., 1.), (Some(0.), None)),
            toggle("modify_target", "Modify target", "", false),
            toggle("fuse_points", "Fuse points", "", true),
            toggle("keep_fused_points", "Keep fused points", "", false),
            text("snapped_group", "Snapped group", "Output", ""),
            text("snapped_destination_attribute", "Destination attribute", "Output", ""),
            toggle("remove_degenerate_primitives", "Remove degenerate primitives", "Cleanup", true),
            toggle("remove_unused_points_from_degenerate_primitives", "Remove newly unused points", "Cleanup", true),
            toggle("remove_all_unused_points", "Remove all unused points", "Cleanup", false),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_target_group = vs(values, 1);
        let p_targeting = TARGETING.of(&values[2]);
        let p_target_attribute = vs(values, 3);
        let p_using = USING.of(&values[4]);
        let p_tolerance = vf(values, 5);
        let p_position = POSITION.of(&values[6]);
        let p_weight_attribute = vs(values, 7);
        let p_attributes = ATTRIBUTES.of(&values[8]);
        let p_attribute_rules = bytes(values, 9);
        let p_group_rules = bytes(values, 10);
        let p_metric = METRIC.of(&values[11]);
        let p_inclusive = vb(values, 12);
        let p_match_attributes = vb(values, 13);
        let p_radius_attribute = vs(values, 14);
        let p_match_attribute = vs(values, 15);
        let p_match_condition = CONDITION.of(&values[16]);
        let p_match_tolerance = vf(values, 17);
        let p_modify_target = vb(values, 18);
        let p_fuse_points = vb(values, 19);
        let p_keep_fused_points = vb(values, 20);
        let p_snapped_group = vs(values, 21);
        let p_snapped_destination_attribute = vs(values, 22);
        let p_remove_degenerate_primitives = vb(values, 23);
        let p_remove_unused_points_from_degenerate_primitives = vb(values, 24);
        let p_remove_all_unused_points = vb(values, 25);
        let target = inputs[1].clone();
        if p_modify_target && target.is_some() {
            return Err("sop/fuse: modify target requires a disconnected target input".to_owned());
        }
        let targeting = match p_targeting {
            FuseTargeting::NearPoints => Targeting::NearPoints,
            FuseTargeting::SpecifiedPoints => Targeting::SpecifiedPoints(p_target_attribute.clone()),
        };
        let group = support::optional_text(&p_group);
        let target_group = support::optional_text(&p_target_group);
        let weight_attribute = support::optional_text(&p_weight_attribute);
        let radius_attribute = support::optional_text(&p_radius_attribute);
        let match_attribute = support::optional_text(&p_match_attribute);
        let snapped_group = support::optional_text(&p_snapped_group);
        let snapped_destination_attribute = support::optional_text(&p_snapped_destination_attribute);
        let attribute_rules = support::decode_fuse_attribute_rules("sop/fuse", &p_attribute_rules).expect("rules were validated");
        let group_rules = support::decode_fuse_group_rules("sop/fuse", &p_group_rules).expect("rules were validated");
        let mut nodes = vec![first_input(inputs)];
        nodes.extend(target);
        Ok(make(
            label,
            "fuse",
            6,
            CookMode::DuplicateInput(0),
            nodes,
            Arc::new(move |context, inputs| {
                let selection = support::resolve_optional_point_group("fuse", &group, &inputs[0])?;
                let target_geometry = if inputs.len() == 2 { &inputs[1] } else { &inputs[0] };
                let target_selection = support::resolve_optional_point_group("fuse target", &target_group, target_geometry)?;
                done(rdk_ops::fuse_grid::fuse(
                    &rdk_ops::fuse_grid::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        selection,
                        target_selection,
                        targeting: targeting.clone(),
                        using: p_using,
                        tolerance: p_tolerance,
                        position: p_position,
                        weight_attribute: weight_attribute.clone(),
                        attributes: p_attributes,
                        attribute_rules: attribute_rules.clone(),
                        group_rules: group_rules.clone(),
                        metric: p_metric,
                        inclusive: p_inclusive,
                        match_attributes: p_match_attributes,
                        radius_attribute: radius_attribute.clone(),
                        match_attribute: match_attribute.clone(),
                        match_condition: p_match_condition,
                        match_tolerance: p_match_tolerance,
                        modify_target: p_modify_target,
                        fuse_points: p_fuse_points,
                        keep_fused_points: p_keep_fused_points,
                        snapped_group: snapped_group.clone(),
                        snapped_destination_attribute: snapped_destination_attribute.clone(),
                        remove_degenerate_primitives: p_remove_degenerate_primitives,
                        remove_unused_points_from_degenerate_primitives: p_remove_unused_points_from_degenerate_primitives,
                        remove_all_unused_points: p_remove_all_unused_points,
                        target: if inputs.len() == 2 { Some(target_geometry.clone()) } else { None },
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Subdivide_cracks

pub mod subdivide_cracks {
    use super::*;
    use rdk_ops::subdivide::{
        BoundaryInterpolation, CrackPolicy, CreasingMethod, FaceVaryingInterpolation, Scheme, TrianglePolicy,
    };
    use support::{KernelMode, SubdivisionCracks};

    const SCHEME: Choice<Scheme> = Choice(&[
        ("Catmull-Clark", Scheme::CatmullClark),
        ("Loop", Scheme::Loop),
        ("Bilinear", Scheme::Bilinear),
    ]);
    const CRACKS: Choice<SubdivisionCracks> = Choice(&[
        ("Do not close", SubdivisionCracks::DoNotClose),
        ("Pull, no edge division", SubdivisionCracks::PullNoDivision),
        ("Pull, divide edges", SubdivisionCracks::PullDivide),
        ("Pull, triangulate", SubdivisionCracks::PullTriangulate),
        ("Stitch, no edge division", SubdivisionCracks::StitchNoDivision),
        ("Stitch, divide edges", SubdivisionCracks::StitchDivide),
        ("Stitch, triangulate", SubdivisionCracks::StitchTriangulate),
    ]);
    const BOUNDARY: Choice<BoundaryInterpolation> = Choice(&[
        ("None", BoundaryInterpolation::SubdivideBoundaryNone),
        ("Edge only", BoundaryInterpolation::SubdivideBoundaryEdgeOnly),
        ("Edge and corner", BoundaryInterpolation::SubdivideBoundaryEdgeAndCorner),
    ]);
    const FVAR: Choice<FaceVaryingInterpolation> = Choice(&[
        ("None", FaceVaryingInterpolation::SubdivideFvarNone),
        ("Corners only", FaceVaryingInterpolation::SubdivideFvarCornersOnly),
        ("Corners plus 1", FaceVaryingInterpolation::SubdivideFvarCornersPlus1),
        ("Corners plus 2", FaceVaryingInterpolation::SubdivideFvarCornersPlus2),
        ("Boundaries", FaceVaryingInterpolation::SubdivideFvarBoundaries),
        ("All", FaceVaryingInterpolation::SubdivideFvarAll),
    ]);
    const TRIANGLE: Choice<TrianglePolicy> = Choice(&[
        ("Catmull-Clark", TrianglePolicy::SubdivideTrianglesCatmullClark),
        ("Smooth", TrianglePolicy::SubdivideTrianglesSmooth),
    ]);
    const CREASING: Choice<CreasingMethod> = Choice(&[
        ("Uniform", CreasingMethod::SubdivideCreasingUniform),
        ("Chaikin", CreasingMethod::SubdivideCreasingChaikin),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_iterations = vi(values, 2);
        let p_crack_bias = vf(values, 4);
        let p_crease_weight = vf(values, 8);
        let p_generate_resulting_creases = vb(values, 9);
        let p_resulting_crease_group = vs(values, 10);
        if p_iterations < 1 {
            return refuse("subdivide_cracks", "iterations must be positive");
        }
        if !p_crack_bias.is_finite() || p_crack_bias < 0. || p_crack_bias > 1. {
            return refuse("subdivide_cracks", "crack bias must be finite and in [0, 1]");
        }
        if !p_crease_weight.is_finite() || p_crease_weight < 0. {
            return refuse("subdivide_cracks", "crease weight must be finite and nonnegative");
        }
        if !p_generate_resulting_creases && !blank(&p_resulting_crease_group) {
            return refuse("subdivide_cracks", "resulting crease group requires generated creases");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "subdivide_cracks",
        label: "Subdivide Cracks",
        category: "Topology",
        inputs: 2,
        slots: Some("input, creases"),
        optional: &[1],
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Primitive group", "", ""),
            choice("scheme", "Scheme", "", SCHEME.kind(), SCHEME.at(0)),
            int("iterations", "Depth", "", 1, (1, 6), Some(1)),
            choice("cracks", "Close cracks", "", CRACKS.kind(), CRACKS.at(0)),
            float("crack_bias", "Pull bias", "Cracks", 0.5, (0., 1.), (None, None)),
            toggle("consistent_topology", "Consistent topology", "Cracks", false),
            text("crease_group", "Crease group", "Creases", ""),
            choice("crease_weight_mode", "Crease weight mode", "Creases", support::KERNEL_MODE.kind(), support::KERNEL_MODE.value(&KernelMode::Explicit)),
            float("crease_weight", "Crease weight", "Creases", 1., (0., 10.), (Some(0.), None)),
            toggle("generate_resulting_creases", "Generate resulting creases", "Creases", false),
            text("resulting_crease_group", "Resulting crease group", "Creases", ""),
            text("hole_group", "Hole group", "Holes", "subdivision_hole"),
            toggle("remove_holes", "Remove holes", "Holes", false),
            choice("boundary_interpolation", "Point boundaries", "Interpolation", BOUNDARY.kind(), BOUNDARY.at(2)),
            choice("face_varying_interpolation", "Vertex boundaries", "Interpolation", FVAR.kind(), FVAR.at(4)),
            choice("triangle_policy", "Triangles", "Interpolation", TRIANGLE.kind(), TRIANGLE.at(0)),
            choice("creasing_method", "Creasing method", "Creases", CREASING.kind(), CREASING.at(0)),
            toggle("treat_curves_as_independent", "Treat curves independently", "", false),
            toggle("recompute_point_normals", "Recompute point normals", "", false),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_group = vs(values, 0);
        let p_scheme = SCHEME.of(&values[1]);
        let p_iterations = vi(values, 2);
        let p_cracks = CRACKS.of(&values[3]);
        let p_crack_bias = vf(values, 4);
        let p_consistent_topology = vb(values, 5);
        let p_crease_group = vs(values, 6);
        let p_crease_weight_mode = support::KERNEL_MODE.of(&values[7]);
        let p_crease_weight = vf(values, 8);
        let p_generate_resulting_creases = vb(values, 9);
        let p_resulting_crease_group = vs(values, 10);
        let p_hole_group = vs(values, 11);
        let p_remove_holes = vb(values, 12);
        let p_boundary_interpolation = BOUNDARY.of(&values[13]);
        let p_face_varying_interpolation = FVAR.of(&values[14]);
        let p_triangle_policy = TRIANGLE.of(&values[15]);
        let p_creasing_method = CREASING.of(&values[16]);
        let p_treat_curves_as_independent = vb(values, 17);
        let p_recompute_point_normals = vb(values, 18);
        let creases = inputs[1].clone();
        if creases.is_none() && !blank(&p_crease_group) {
            return Err("sop/subdivide_cracks: crease group requires a crease input".to_owned());
        }
        let cracks = match p_cracks {
            SubdivisionCracks::DoNotClose => CrackPolicy::SubdivideDoNotClose,
            SubdivisionCracks::PullNoDivision => CrackPolicy::SubdividePullNoEdgeDivision,
            SubdivisionCracks::PullDivide => CrackPolicy::SubdividePullDivideEdges(p_crack_bias),
            SubdivisionCracks::PullTriangulate => CrackPolicy::SubdividePullTriangulate(p_crack_bias),
            SubdivisionCracks::StitchNoDivision => CrackPolicy::SubdivideStitchNoEdgeDivision,
            SubdivisionCracks::StitchDivide => CrackPolicy::SubdivideStitchDivideEdges,
            SubdivisionCracks::StitchTriangulate => CrackPolicy::SubdivideStitchTriangulate,
        };
        let group = support::optional_text(&p_group);
        let crease_group = support::optional_text(&p_crease_group);
        let resulting_crease_group = support::optional_text(&p_resulting_crease_group);
        let hole_group = support::optional_text(&p_hole_group);
        let crease_weight = match p_crease_weight_mode {
            KernelMode::Auto => None,
            KernelMode::Explicit => Some(p_crease_weight),
        };
        let has_creases = creases.is_some();
        let cook_mode = if has_creases { CookMode::Generic } else { CookMode::DuplicateInput(0) };
        let mut nodes = vec![first_input(inputs)];
        nodes.extend(creases);
        Ok(make(
            label,
            "subdivide_cracks",
            13,
            cook_mode,
            nodes,
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let selection = find_primitives(&group, geometry, "missing_group", "subdivide could not find primitive group");
                let crease_selection = match (has_creases, &crease_group) {
                    (true, Some(_)) => find_primitives(
                        &crease_group,
                        &inputs[1],
                        "missing_group",
                        "subdivide could not find crease primitive group",
                    ),
                    _ => Ok(None),
                };
                let hole_selection =
                    find_primitives(&hole_group, geometry, "missing_group", "subdivide could not find hole primitive group");
                let (primitives, crease_primitives, hole_primitives) = (selection?, crease_selection?, hole_selection?);
                done(rdk_ops::subdivide::subdivide(
                    &rdk_ops::subdivide::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        scheme: p_scheme,
                        iterations: p_iterations,
                        primitives,
                        cracks,
                        consistent_topology: p_consistent_topology,
                        creases: if has_creases { Some(inputs[1].clone()) } else { None },
                        crease_primitives,
                        crease_weight,
                        generate_resulting_creases: p_generate_resulting_creases,
                        resulting_crease_group: resulting_crease_group.clone(),
                        hole_primitives,
                        remove_holes: p_remove_holes,
                        boundary_interpolation: p_boundary_interpolation,
                        face_varying_interpolation: p_face_varying_interpolation,
                        triangle_policy: p_triangle_policy,
                        creasing_method: p_creasing_method,
                        treat_curves_as_independent: p_treat_curves_as_independent,
                        recompute_point_normals: p_recompute_point_normals,
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Boolean

pub mod boolean {
    use super::*;
    use rdk_boolean::boolean::SeamPoints;
    use rdk_boolean::boolean_solid::Operation;
    use support::BooleanClosedPolicy;

    const OPERATION: Choice<Operation> = Choice(&[
        ("Union", Operation::Union),
        ("Intersection", Operation::Intersection),
        ("Subtract B from A", Operation::Difference),
        ("Subtract A from B", Operation::ReverseDifference),
        ("Exclusive or", Operation::Xor),
        ("Shatter", Operation::Shatter),
    ]);
    const SEAM_POINTS: Choice<SeamPoints> = Choice(&[
        ("Shared", SeamPoints::SharedSeamPoints),
        ("Split", SeamPoints::SplitSeamPoints),
    ]);
    const CLOSED: Choice<BooleanClosedPolicy> = Choice(&[
        ("Operation default", BooleanClosedPolicy::Default),
        ("Require closed", BooleanClosedPolicy::Required),
        ("Allow open", BooleanClosedPolicy::NotRequired),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_operation = OPERATION.of(&values[0]);
        let p_left_treatment = TREATMENT.of(&values[1]);
        let p_right_treatment = TREATMENT.of(&values[2]);
        let p_point_tolerance = vf(values, 6);
        let p_tiny_seam_threshold = vf(values, 7);
        let p_cleanup_max_batches = vi(values, 8);
        let p_left_piece_group = vs(values, 15);
        let p_overlap_piece_group = vs(values, 16);
        let p_right_piece_group = vs(values, 17);
        if !(p_point_tolerance.is_finite() && p_tiny_seam_threshold.is_finite())
            || p_point_tolerance < 0.
            || p_tiny_seam_threshold < 0.
        {
            return refuse("boolean", "tolerances must be finite and nonnegative");
        }
        if p_cleanup_max_batches < 1 {
            return refuse("boolean", "cleanup batches must be positive");
        }
        if p_operation == Operation::Shatter {
            if p_left_treatment != Treatment::Solid || p_right_treatment != Treatment::Solid {
                return refuse("boolean", "shatter requires two solid operands");
            }
            let names: Vec<String> = [&p_left_piece_group, &p_overlap_piece_group, &p_right_piece_group]
                .iter()
                .filter_map(|name| support::optional_text(name))
                .collect();
            if has_duplicates(&names) {
                return refuse("boolean", "shatter group names must be distinct");
            }
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "boolean",
        label: "Boolean",
        category: "Boolean",
        inputs: 2,
        slots: Some("left, right"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            choice("operation", "Operation", "", OPERATION.kind(), OPERATION.at(0)),
            choice("left_treatment", "A treatment", "Operands", TREATMENT.kind(), TREATMENT.at(0)),
            choice("right_treatment", "B treatment", "Operands", TREATMENT.kind(), TREATMENT.at(0)),
            toggle("resolve_left_self_intersections", "Resolve A self-intersections", "Operands", false),
            toggle("resolve_right_self_intersections", "Resolve B self-intersections", "Operands", false),
            choice("point_conflict", "Point attribute conflicts", "Attributes", CONFLICT.kind(), CONFLICT.at(1)),
            float("point_tolerance", "Point tolerance", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            float("tiny_seam_threshold", "Tiny seam threshold", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            int("cleanup_max_batches", "Cleanup batches", "Robustness", 8, (1, 32), Some(1)),
            toggle("strict_cleanup", "Strict cleanup", "Robustness", true),
            choice("seam_points", "Seam points", "Output", SEAM_POINTS.kind(), SEAM_POINTS.at(0)),
            choice("detriangulation", "Polygons", "Output", DETRIANGULATION.kind(), DETRIANGULATION.at(0)),
            toggle("assume_flat", "Assume flat", "Output", false),
            choice("require_closed", "Closed output", "Output", CLOSED.kind(), CLOSED.at(0)),
            text("piece_attribute", "Piece attribute", "Output", ""),
            text("left_piece_group", "A-only group", "Shatter groups", "boolean_left"),
            text("overlap_piece_group", "Overlap group", "Shatter groups", "boolean_overlap"),
            text("right_piece_group", "B-only group", "Shatter groups", "boolean_right"),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_operation = OPERATION.of(&values[0]);
        let p_left_treatment = TREATMENT.of(&values[1]);
        let p_right_treatment = TREATMENT.of(&values[2]);
        let p_resolve_left_self_intersections = vb(values, 3);
        let p_resolve_right_self_intersections = vb(values, 4);
        let p_point_conflict = CONFLICT.of(&values[5]);
        let p_point_tolerance = vf(values, 6);
        let p_tiny_seam_threshold = vf(values, 7);
        let p_cleanup_max_batches = vi(values, 8);
        let p_strict_cleanup = vb(values, 9);
        let p_seam_points = SEAM_POINTS.of(&values[10]);
        let p_detriangulation = DETRIANGULATION.of(&values[11]);
        let p_assume_flat = vb(values, 12);
        let p_require_closed = CLOSED.of(&values[13]);
        let p_piece_attribute = vs(values, 14);
        let p_left_piece_group = vs(values, 15);
        let p_overlap_piece_group = vs(values, 16);
        let p_right_piece_group = vs(values, 17);
        let require_closed = match p_require_closed {
            BooleanClosedPolicy::Default => None,
            BooleanClosedPolicy::Required => Some(true),
            BooleanClosedPolicy::NotRequired => Some(false),
        };
        let piece_attribute = support::optional_text(&p_piece_attribute);
        let left_piece_group = support::optional_text(&p_left_piece_group);
        let overlap_piece_group = support::optional_text(&p_overlap_piece_group);
        let right_piece_group = support::optional_text(&p_right_piece_group);
        Ok(make(
            label,
            "boolean",
            1,
            CookMode::Generic,
            vec![first_input(inputs), inputs[1].clone().expect("required input")],
            Arc::new(move |context, inputs| {
                done(rdk_boolean::boolean::run(
                    &rdk_boolean::boolean::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        operation: p_operation,
                        left_treatment: p_left_treatment,
                        right_treatment: p_right_treatment,
                        resolve_left_self_intersections: p_resolve_left_self_intersections,
                        resolve_right_self_intersections: p_resolve_right_self_intersections,
                        point_conflict: p_point_conflict,
                        point_tolerance: p_point_tolerance,
                        tiny_seam_threshold: p_tiny_seam_threshold,
                        cleanup_max_batches: p_cleanup_max_batches,
                        strict_cleanup: p_strict_cleanup,
                        seam_points: p_seam_points,
                        detriangulation: p_detriangulation,
                        assume_flat: p_assume_flat,
                        require_closed,
                        piece_attribute: piece_attribute.clone(),
                        left_piece_group: left_piece_group.clone(),
                        overlap_piece_group: overlap_piece_group.clone(),
                        right_piece_group: right_piece_group.clone(),
                        right: inputs[1].clone(),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Boolean_detect

pub mod boolean_detect {
    use super::*;

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_tolerance = vf(values, 2);
        if !p_tolerance.is_finite() || p_tolerance < 0. {
            return refuse("boolean_detect", "tolerance must be finite and nonnegative");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "boolean_detect",
        label: "Boolean Detect",
        category: "Boolean/Analysis",
        inputs: 2,
        slots: Some("input, collision"),
        optional: &[1],
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("source_group", "Source group", "Inputs", ""),
            text("collision_group", "Collision group", "Inputs", ""),
            float("tolerance", "Tolerance", "", 0., (0., 0.001), (Some(0.), None)),
            toggle("include_coplanar", "Include coplanar overlap", "", true),
            text("intersecting_group", "Intersecting group", "A/B outputs", "boolean_intersections"),
            text("intersections_attribute", "Intersections attribute", "A/B outputs", ""),
            text("count_attribute", "Count attribute", "A/B outputs", ""),
            text("self_intersecting_group", "Self-intersecting group", "Self outputs", "boolean_self_intersections"),
            text("self_intersections_attribute", "Self intersections attribute", "Self outputs", ""),
            text("self_count_attribute", "Self count attribute", "Self outputs", ""),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_source_group = vs(values, 0);
        let p_collision_group = vs(values, 1);
        let p_tolerance = vf(values, 2);
        let p_include_coplanar = vb(values, 3);
        let p_intersecting_group = vs(values, 4);
        let p_intersections_attribute = vs(values, 5);
        let p_count_attribute = vs(values, 6);
        let p_self_intersecting_group = vs(values, 7);
        let p_self_intersections_attribute = vs(values, 8);
        let p_self_count_attribute = vs(values, 9);
        let collision = inputs[1].clone();
        let has_collision = collision.is_some();
        let cross = |name: &str| if has_collision { support::optional_text(name) } else { None };
        let source_group = support::optional_text(&p_source_group);
        let collision_group = cross(&p_collision_group);
        let intersecting_group = cross(&p_intersecting_group);
        let intersections_attribute = cross(&p_intersections_attribute);
        let count_attribute = cross(&p_count_attribute);
        let self_intersecting_group = support::optional_text(&p_self_intersecting_group);
        let self_intersections_attribute = support::optional_text(&p_self_intersections_attribute);
        let self_count_attribute = support::optional_text(&p_self_count_attribute);
        let attributes: Vec<String> = [
            &intersections_attribute,
            &count_attribute,
            &self_intersections_attribute,
            &self_count_attribute,
        ]
        .into_iter()
        .flatten()
        .cloned()
        .collect();
        let groups: Vec<String> =
            [&intersecting_group, &self_intersecting_group].into_iter().flatten().cloned().collect();
        if attributes.is_empty() && groups.is_empty() {
            return Err("sop/boolean_detect: at least one output must be requested".to_owned());
        }
        if has_duplicates(&attributes) {
            return Err("sop/boolean_detect: attribute outputs must have distinct names".to_owned());
        }
        if has_duplicates(&groups) {
            return Err("sop/boolean_detect: group outputs must have distinct names".to_owned());
        }
        let mut nodes = vec![first_input(inputs)];
        nodes.extend(collision);
        Ok(make(
            label,
            "boolean_detect",
            1,
            CookMode::DuplicateInput(0),
            nodes,
            Arc::new(move |context, inputs| {
                let source_primitives = support::resolve_optional_primitive_group("boolean_detect", &source_group, &inputs[0])?;
                let collision_geometry = if inputs.len() == 1 { &inputs[0] } else { &inputs[1] };
                let collision_primitives = support::resolve_optional_primitive_group(
                    "boolean_detect collision",
                    &collision_group,
                    collision_geometry,
                )?;
                done(rdk_ops::boolean_detect::run_checked(
                    &rdk_ops::boolean_detect::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        source_primitives,
                        collision_primitives,
                        tolerance: p_tolerance,
                        include_coplanar: p_include_coplanar,
                        intersecting_group: intersecting_group.clone(),
                        intersections_attribute: intersections_attribute.clone(),
                        count_attribute: count_attribute.clone(),
                        self_intersecting_group: self_intersecting_group.clone(),
                        self_intersections_attribute: self_intersections_attribute.clone(),
                        self_count_attribute: self_count_attribute.clone(),
                    },
                    collision_geometry,
                    &inputs[0],
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- Triangulate_2d

pub mod triangulate_2d {
    use super::*;
    use rdk_ops::triangulate2d::Projection;
    use support::Triangulate2dProjection;

    const PROJECTION: Choice<Triangulate2dProjection> = Choice(&[
        ("Best fit", Triangulate2dProjection::BestFit),
        ("XY", Triangulate2dProjection::Xy),
        ("YZ", Triangulate2dProjection::Yz),
        ("ZX", Triangulate2dProjection::Zx),
        ("Custom plane", Triangulate2dProjection::Plane),
        ("Point attribute", Triangulate2dProjection::Attribute),
    ]);

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p_projection = PROJECTION.of(&values[3]);
        let p_plane_origin_x = vf(values, 4);
        let p_plane_origin_y = vf(values, 5);
        let p_plane_origin_z = vf(values, 6);
        let p_plane_normal_x = vf(values, 7);
        let p_plane_normal_y = vf(values, 8);
        let p_plane_normal_z = vf(values, 9);
        let p_point_attribute = vs(values, 10);
        let p_refine = vb(values, 19);
        let p_minimum_angle = vf(values, 21);
        let p_use_maximum_area = vb(values, 22);
        let p_maximum_area = vf(values, 23);
        let p_use_target_edge_length = vb(values, 24);
        let p_target_edge_length = vf(values, 25);
        let p_minimum_edge_length = vf(values, 26);
        let p_maximum_new_points = vi(values, 27);
        let p_regularization_steps = vi(values, 28);
        let numbers = [
            p_plane_origin_x,
            p_plane_origin_y,
            p_plane_origin_z,
            p_plane_normal_x,
            p_plane_normal_y,
            p_plane_normal_z,
            p_minimum_angle,
            p_maximum_area,
            p_target_edge_length,
            p_minimum_edge_length,
        ];
        if !numbers.iter().all(|v| v.is_finite()) {
            return refuse("triangulate_2d", "numeric values must be finite");
        }
        if p_minimum_angle < 0.
            || p_minimum_edge_length < 0.
            || p_maximum_area < 0.
            || p_target_edge_length < 0.
            || p_maximum_new_points < 0
            || p_regularization_steps < 0
        {
            return refuse("triangulate_2d", "refinement bounds must be nonnegative");
        }
        if p_refine && (p_minimum_angle <= 0. || p_minimum_angle >= std::f64::consts::PI / 3.) {
            return refuse("triangulate_2d", "minimum angle must lie between zero and pi/3");
        }
        if p_use_maximum_area && p_maximum_area <= 0. {
            return refuse("triangulate_2d", "maximum area must be positive");
        }
        if p_use_target_edge_length && p_target_edge_length <= 0. {
            return refuse("triangulate_2d", "target edge length must be positive");
        }
        if p_projection == Triangulate2dProjection::Plane
            && p_plane_normal_x == 0.
            && p_plane_normal_y == 0.
            && p_plane_normal_z == 0.
        {
            return refuse("triangulate_2d", "plane normal must be nonzero");
        }
        if p_projection == Triangulate2dProjection::Attribute && blank(&p_point_attribute) {
            return refuse("triangulate_2d", "point attribute must be nonblank");
        }
        Ok(())
    }

    pub const META: Meta = Meta {
        key: "triangulate_2d",
        label: "Triangulate 2D",
        category: "Topology/Triangulate",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    fn vec3(name: &'static str, label: &'static str, group: &'static str, component: i64, default: f64, bound: f64) -> Spec {
        Spec {
            vec3: Some((group, component)),
            ..float(name, label, "Projection/Plane", default, (-bound, bound), (None, None))
        }
    }

    pub fn specs() -> Vec<Spec> {
        let refinement = |name, label, default, max| float(name, label, "Refinement", default, (0., max), (Some(0.), None));
        vec![
            text("point_group", "Point group", "Input", ""),
            text("constraint_edge_group", "Constraint edge group", "Input", ""),
            text("constraint_primitive_group", "Constraint primitive group", "Input", ""),
            choice("projection", "Projection", "Projection", PROJECTION.kind(), PROJECTION.at(0)),
            vec3("plane_origin_x", "Origin X", "plane_origin", 0, 0., 10.),
            vec3("plane_origin_y", "Origin Y", "plane_origin", 1, 0., 10.),
            vec3("plane_origin_z", "Origin Z", "plane_origin", 2, 0., 10.),
            vec3("plane_normal_x", "Normal X", "plane_normal", 0, 0., 1.),
            vec3("plane_normal_y", "Normal Y", "plane_normal", 1, 0., 1.),
            vec3("plane_normal_z", "Normal Z", "plane_normal", 2, 1., 1.),
            text("point_attribute", "Point attribute", "Projection", "uv"),
            int("seed", "Seed", "Triangulation", 0, (0, 9999), None),
            toggle("split_crossing_constraints", "Split crossing constraints", "Constraints", false),
            toggle("flood_from_hull_boundary", "Flood from hull boundary", "Constraints", false),
            toggle("remove_outside_constraint_polygons", "Remove outside constraints", "Constraints", false),
            toggle("silhouette_constraints", "Silhouette constraints", "Constraints", false),
            toggle("remove_outside_silhouette", "Remove outside silhouette", "Constraints", false),
            toggle("ignore_non_constraint_points", "Ignore non-constraint points", "Constraints", false),
            toggle("remove_duplicate_points", "Remove duplicate points", "Cleanup", false),
            toggle("refine", "Refine", "Refinement", false),
            toggle("allow_constraint_splitting", "Allow constraint splitting", "Refinement", true),
            refinement("minimum_angle", "Minimum angle", 0.3490658503988659, 1.0471975511965976),
            toggle("use_maximum_area", "Maximum area", "Refinement", false),
            refinement("maximum_area", "Area", 1., 100.),
            toggle("use_target_edge_length", "Target edge length", "Refinement", false),
            refinement("target_edge_length", "Edge length", 1., 100.),
            refinement("minimum_edge_length", "Minimum edge length", 0., 100.),
            int("maximum_new_points", "Maximum new points", "Refinement/Limits", 100000, (0, 1_000_000), Some(0)),
            int("regularization_steps", "Regularization steps", "Refinement", 0, (0, 100), Some(0)),
            toggle("allow_movement_of_interior_input_points", "Move interior input points", "Refinement", false),
            toggle("preserve_point_payload", "Preserve point payload", "Payload", true),
            toggle("restore_original_point_positions", "Restore original positions", "Payload", true),
            toggle("keep_primitives", "Keep primitives", "Output", false),
            toggle("remove_unused_points", "Remove unused points", "Cleanup", false),
            toggle("recompute_point_normals", "Recompute point normals", "Output", false),
            text("split_point_group", "Split point group", "Output", ""),
            text("refinement_point_group", "Refinement point group", "Output", ""),
            text("triangle_group", "Triangle group", "Output", ""),
            text("constraint_group", "Constraint group", "Output", ""),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_point_group = vs(values, 0);
        let p_constraint_edge_group = vs(values, 1);
        let p_constraint_primitive_group = vs(values, 2);
        let p_projection = PROJECTION.of(&values[3]);
        let p_plane_origin_x = vf(values, 4);
        let p_plane_origin_y = vf(values, 5);
        let p_plane_origin_z = vf(values, 6);
        let p_plane_normal_x = vf(values, 7);
        let p_plane_normal_y = vf(values, 8);
        let p_plane_normal_z = vf(values, 9);
        let p_point_attribute = vs(values, 10);
        let p_seed = vi(values, 11);
        let p_split_crossing_constraints = vb(values, 12);
        let p_flood_from_hull_boundary = vb(values, 13);
        let p_remove_outside_constraint_polygons = vb(values, 14);
        let p_silhouette_constraints = vb(values, 15);
        let p_remove_outside_silhouette = vb(values, 16);
        let p_ignore_non_constraint_points = vb(values, 17);
        let p_remove_duplicate_points = vb(values, 18);
        let p_refine = vb(values, 19);
        let p_allow_constraint_splitting = vb(values, 20);
        let p_minimum_angle = vf(values, 21);
        let p_use_maximum_area = vb(values, 22);
        let p_maximum_area = vf(values, 23);
        let p_use_target_edge_length = vb(values, 24);
        let p_target_edge_length = vf(values, 25);
        let p_minimum_edge_length = vf(values, 26);
        let p_maximum_new_points = vi(values, 27);
        let p_regularization_steps = vi(values, 28);
        let p_allow_movement_of_interior_input_points = vb(values, 29);
        let p_preserve_point_payload = vb(values, 30);
        let p_restore_original_point_positions = vb(values, 31);
        let p_keep_primitives = vb(values, 32);
        let p_remove_unused_points = vb(values, 33);
        let p_recompute_point_normals = vb(values, 34);
        let p_split_point_group = vs(values, 35);
        let p_refinement_point_group = vs(values, 36);
        let p_triangle_group = vs(values, 37);
        let p_constraint_group = vs(values, 38);
        let projection_of = move || -> Projection {
            match p_projection {
                Triangulate2dProjection::BestFit => Projection::BestFit,
                Triangulate2dProjection::Xy => Projection::PlaneXy,
                Triangulate2dProjection::Yz => Projection::PlaneYz,
                Triangulate2dProjection::Zx => Projection::PlaneZx,
                Triangulate2dProjection::Plane => Projection::Plane {
                    origin: rays_math::vec3::create(p_plane_origin_x, p_plane_origin_y, p_plane_origin_z),
                    normal: rays_math::vec3::create(p_plane_normal_x, p_plane_normal_y, p_plane_normal_z),
                },
                Triangulate2dProjection::Attribute => Projection::PointAttribute(p_point_attribute.clone()),
            }
        };
        let maximum_area = p_use_maximum_area.then_some(p_maximum_area);
        let target_edge_length = p_use_target_edge_length.then_some(p_target_edge_length);
        let point_group = support::optional_text(&p_point_group);
        let constraint_edge_group = support::optional_text(&p_constraint_edge_group);
        let constraint_primitive_group = support::optional_text(&p_constraint_primitive_group);
        let split_point_group = support::optional_text(&p_split_point_group);
        let refinement_point_group = support::optional_text(&p_refinement_point_group);
        let triangle_group = support::optional_text(&p_triangle_group);
        let constraint_group = support::optional_text(&p_constraint_group);
        Ok(make(
            label,
            "triangulate_2d",
            12,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let selection = find_group(
                    rdk_core::group::Owner::Point,
                    &point_group,
                    geometry,
                    "missing_group",
                    "triangulate_2d could not find point group",
                )
                .map(|found| found.map(rdk_core::element_selection::T::SelectedPoints));
                let constraint_edges = find_edges(
                    &constraint_edge_group,
                    geometry,
                    "missing_group",
                    "triangulate_2d could not find native edge group",
                );
                let constraint_primitives = find_primitives(
                    &constraint_primitive_group,
                    geometry,
                    "missing_group",
                    "triangulate_2d could not find primitive group",
                );
                let (selection, constraint_edges, constraint_primitives) =
                    (selection?, constraint_edges?, constraint_primitives?);
                done(rdk_ops::triangulate2d::run(
                    Some(context::cancel_token(context).as_ref()),
                    &rdk_ops::triangulate2d::Params {
                        grain: grain(context),
                        selection,
                        constraint_edges,
                        constraint_primitives,
                        projection: projection_of(),
                        seed: p_seed,
                        split_crossing_constraints: p_split_crossing_constraints,
                        flood_from_hull_boundary: p_flood_from_hull_boundary,
                        remove_outside_constraint_polygons: p_remove_outside_constraint_polygons,
                        silhouette_constraints: p_silhouette_constraints,
                        remove_outside_silhouette: p_remove_outside_silhouette,
                        ignore_non_constraint_points: p_ignore_non_constraint_points,
                        remove_duplicate_points: p_remove_duplicate_points,
                        refine: p_refine,
                        allow_constraint_splitting: p_allow_constraint_splitting,
                        minimum_angle: p_minimum_angle,
                        maximum_area,
                        target_edge_length,
                        minimum_edge_length: p_minimum_edge_length,
                        maximum_new_points: p_maximum_new_points,
                        regularization_steps: p_regularization_steps,
                        allow_movement_of_interior_input_points: p_allow_movement_of_interior_input_points,
                        preserve_point_payload: p_preserve_point_payload,
                        restore_original_point_positions: p_restore_original_point_positions,
                        keep_primitives: p_keep_primitives,
                        split_point_group: split_point_group.clone(),
                        refinement_point_group: refinement_point_group.clone(),
                        triangle_group: triangle_group.clone(),
                        constraint_group: constraint_group.clone(),
                        remove_unused_points: p_remove_unused_points,
                        recompute_point_normals: p_recompute_point_normals,
                    },
                    geometry,
                ))
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edit_graph;
    use crate::session;
    use crate::test_support::*;

    fn ring() -> geometry::T {
        rdk_ops::line_geometry::polyline(
            &rdk_ops::line_geometry::Params { closed: true },
            &[(0., 0., 0.), (1., 0., 0.), (3., 2., 0.), (0., 2., 0.)],
        )
        .unwrap()
    }

    fn node_of(factory: &Factory, inputs: &[geometry::T]) -> node::T {
        let mut slots: Vec<Option<node::T>> = inputs.iter().map(|g| Some(snapshot(g))).collect();
        slots.resize(edit_graph::factory_inputs(factory).len(), None);
        edit_graph::instantiate_optional(factory, &slots).unwrap()
    }

    fn cook_code(n: &node::T) -> String {
        let mut s = session(8);
        let code = match session::cook(&mut s, None, &context(4), n) {
            Ok(_) => panic!("cook unexpectedly succeeded"),
            Err(error) => error.code.clone(),
        };
        session::close(&mut s);
        code
    }

    fn counts(g: &geometry::T) -> (i64, i64, i64) {
        (geometry::point_count(g), geometry::vertex_count(g), geometry::primitive_count(g))
    }

    // test_sop.ml test_edge_collapse_contract (the parts that need no sop/group_edges).
    #[test]
    fn edge_collapse_contract() {
        let factory = edge_collapse::factory();
        let source = ring();
        let n = node_of(&factory, std::slice::from_ref(&source));
        assert_eq!(node::operation(&n), "edge_collapse");
        assert_eq!(node::version(&n), 1);
        // omitted group: every edge is selected and the ring collapses away
        let mut s = session(8);
        let whole = geometry_of(&cook(&mut s, 4, &n));
        assert_eq!((geometry::point_count(&whole), geometry::primitive_count(&whole)), (0, 0));
        // a missing group, then a missing connectivity attribute
        let (missing, _) = node::apply_parameters(&n, &[("group".to_owned(), Value::Text(b"absent".as_slice().into()))]).unwrap();
        assert_eq!(cook_code(&missing), "missing_group");
        let (absent, _) =
            node::apply_parameters(&n, &[("connectivity_attribute".to_owned(), Value::Text(b"absent".as_slice().into()))]).unwrap();
        assert_eq!(cook_code(&absent), "invalid_topology");
        // blank group and attribute are accepted and mean none
        let blank = [("group".to_owned(), Value::Text(b" ".as_slice().into())), ("connectivity_attribute".to_owned(), Value::Text(b" ".as_slice().into()))];
        let (blank, _) = node::apply_parameters(&n, &blank).unwrap();
        assert_eq!(geometry::point_count(&geometry_of(&cook(&mut s, 4, &blank))), 0);
        session::close(&mut s);
    }

    // test_sop.ml test_poly_fill_contract on the fixture grid instead of a Lisp tube.
    #[test]
    fn poly_fill_contract() {
        let factory = poly_fill::factory();
        let n = node_of(&factory, &[fixture()]);
        let (n, effects) = node::apply_parameters(
            &n,
            &[
                ("mode".to_owned(), Value::Choice("Triangle fan".to_owned())),
                ("patch_group".to_owned(), Value::Text(b"patch".as_slice().into())),
            ],
        )
        .unwrap();
        assert!(effects.cook);
        let mut s = session(8);
        let out = geometry_of(&cook(&mut s, 4, &n));
        // the grid's single 8-edge boundary loop becomes one fan of eight triangles
        let patch = geometry::find_group(group::Owner::Primitive, "patch", &out).expect("patch group");
        assert_eq!(rdk_core::group::cardinality(&patch), 8);
        assert_eq!(geometry::primitive_count(&out), 12);
        session::close(&mut s);
        let (missing, _) = node::apply_parameters(&n, &[("boundary_group".to_owned(), Value::Text(b"missing".as_slice().into()))]).unwrap();
        assert_eq!(cook_code(&missing), "missing_group");
    }

    #[test]
    fn mesh_nodes_cook_on_the_grid_and_match_across_domains() {
        let cases: [(Factory, &str, (i64, i64, i64)); 4] = [
            (triangulate::factory(), "triangulate", (9, 24, 8)),
            (facet::factory(), "facet", (9, 16, 4)),
            (clean::factory(), "clean", (9, 16, 4)),
            (fuse::factory(), "fuse", (9, 16, 4)),
        ];
        for (factory, key, expected) in cases {
            let n = node_of(&factory, &[fixture()]);
            let mut s = session(8);
            let one = geometry_of(&cook(&mut s, 1, &n));
            let many = geometry_of(&cook(&mut s, 4, &n));
            assert_eq!(counts(&one), expected, "{key}");
            assert!(same_geometry(&one, &many) || counts(&one) == counts(&many), "{key}: domains differ");
            let (a, b) = (geometry::positions(&one), geometry::positions(&many));
            assert!(rdk_core::packed::float3::view::view(a).unwrap().x == rdk_core::packed::float3::view::view(b).unwrap().x, "{key}");
            session::close(&mut s);
        }
    }

    #[test]
    fn validators_refuse_like_the_lisp_nodes() {
        let refused = |factory: Factory, input_count: usize, edits: Vec<(&str, Value)>, message: &str| {
            let inputs: Vec<geometry::T> = (0..input_count).map(|_| fixture()).collect();
            let n = node_of(&factory, &inputs);
            match node::apply_parameters(&n, &edits.into_iter().map(|(k, v)| (k.to_owned(), v)).collect::<Vec<_>>()) {
                Ok(_) => panic!("{message}: accepted"),
                Err(error) => assert!(error.contains(message), "{error} does not contain {message}"),
            }
        };
        refused(remesh::factory(), 1, vec![("target_length", Value::Float(0.))], "sop/remesh: target length must be finite and positive");
        refused(
            convex_hull::factory(),
            1,
            vec![("source_point_attribute", Value::Text(b" P ".as_slice().into()))],
            "sop/convex_hull: source point attribute must not be P",
        );
        refused(
            poly_cut::factory(),
            1,
            vec![("detection", Value::Choice("Attribute change".to_owned())), ("strategy", Value::Choice("Cut".to_owned()))],
            "sop/poly_cut: cut-at-change requires a positive threshold",
        );
    }

    #[test]
    fn wiring_refusals() {
        // boolean_detect with nothing to output; fuse modify_target with a connected target
        let detect = node_of(&boolean_detect::factory(), &[fixture()]);
        let cleared = [
            ("intersecting_group", ""),
            ("self_intersecting_group", ""),
        ]
        .map(|(name, value)| (name.to_owned(), Value::Text(value.as_bytes().into())));
        match node::apply_parameters(&detect, &cleared) {
            Ok(_) => panic!("boolean_detect accepted no outputs"),
            Err(error) => assert!(error.contains("sop/boolean_detect: at least one output must be requested"), "{error}"),
        }
        let fused = node_of(&fuse::factory(), &[fixture(), fixture()]);
        match node::apply_parameters(&fused, &[("modify_target".to_owned(), Value::Bool(true))]) {
            Ok(_) => panic!("fuse accepted modify_target with a target"),
            Err(error) => assert!(error.contains("modify target requires a disconnected target input"), "{error}"),
        }
        let creases = [("crease_group".to_owned(), Value::Text(b"c".as_slice().into()))];
        let sub = node_of(&subdivide_cracks::factory(), &[fixture()]);
        match node::apply_parameters(&sub, &creases) {
            Ok(_) => panic!("subdivide accepted a crease group without creases"),
            Err(error) => assert!(error.contains("crease group requires a crease input"), "{error}"),
        }
    }
}
