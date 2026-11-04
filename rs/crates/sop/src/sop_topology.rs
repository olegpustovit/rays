//! Port of lib/sop/sop_topology.ml: the topology and Boolean nodes, declared once (metadata,
//! fields, parameters, operator). One `pub mod` per OCaml module, in the OCaml order.
use std::sync::Arc;

use param::{Kind, Value};
use rays_diagnostic::Diagnostic;
use rdk::{edge_group, geometry, group};

use crate::context;
use crate::decl::{self, Decl, Meta, Spec};
use crate::edit_graph::Factory;
use crate::node::{self, build, CookMode, GeometryCooked};
use crate::support;

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

fn cancel(context: &context::T) -> Option<Arc<rdk::cancel::T>> {
    Some(Arc::clone(context::cancel_token(context)))
}

/// Result of an rdk call as a cooked geometry.
fn done(result: Result<geometry::T, rdk::error::T>) -> Result<GeometryCooked, Diagnostic> {
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
        _ => panic!("parameter {index} is not a toggle"),
    }
}

fn vf(values: &[Value], index: usize) -> f64 {
    match &values[index] {
        Value::Float(value) => *value,
        _ => panic!("parameter {index} is not a float"),
    }
}

fn vi(values: &[Value], index: usize) -> i64 {
    match &values[index] {
        Value::Int(value) => *value,
        _ => panic!("parameter {index} is not an int"),
    }
}

fn vs(values: &[Value], index: usize) -> String {
    match &values[index] {
        Value::Text(value) => String::from_utf8_lossy(value).into_owned(),
        _ => panic!("parameter {index} is not text"),
    }
}

/// OCaml `String.trim t = ""`.
fn blank(text: &str) -> bool {
    text.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c')).is_empty()
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

/// A `Param.choice` table: the value at the label's position.
fn pick<T: Clone>(labels: &[&str], table: &[T], value: &Value) -> T {
    table[support::choice_index(labels, value)].clone()
}

// ---- field constructors (one `Spec` each; descriptions and vec3 by struct update)

fn text(name: &'static str, label: &'static str, folder: &'static str, default: &str) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::text_kind(), Value::Text(default.as_bytes().to_vec())) }
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

/// `decl` and `factory` of a node module (`parameters_factory build`).
macro_rules! declare {
    () => {
        pub fn decl() -> Decl {
            Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
        }

        pub fn factory() -> Factory {
            decl::factory(&decl())
        }
    };
}

// ---------------------------------------------------------------- choice tables

use rdk::fuse_reduce::Position;

const POSITION_LABELS: [&str; 15] = [
    "First",
    "Least point",
    "Greatest point",
    "Average",
    "Minimum",
    "Maximum",
    "Mode",
    "Median",
    "Sum",
    "Sum squares",
    "Root mean square",
    "Weighted average",
    "Weighted sum",
    "Minimum weight",
    "Maximum weight",
];
const POSITION: [Position; 15] = [
    Position::First,
    Position::LeastPoint,
    Position::GreatestPoint,
    Position::Average,
    Position::Minimum,
    Position::Maximum,
    Position::Mode,
    Position::Median,
    Position::Sum,
    Position::SumSquares,
    Position::RootMeanSquare,
    Position::WeightedAverage,
    Position::WeightedSum,
    Position::MinimumWeight,
    Position::MaximumWeight,
];
const POSITION_AVERAGE: usize = 3;

fn position_kind() -> Kind {
    support::choice_kind(&POSITION_LABELS)
}

fn position_of_value(value: &Value) -> Position {
    pick(&POSITION_LABELS, &POSITION, value)
}

// ---------------------------------------------------------------- Edge_collapse

pub mod edge_collapse {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub connectivity_attribute: String,
        pub position: Position,
        pub remove_degenerate_primitives: bool,
        pub recompute_point_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            connectivity_attribute: vs(values, 1),
            position: position_of_value(&values[2]),
            remove_degenerate_primitives: vb(values, 3),
            recompute_point_normals: vb(values, 4),
        }
    }

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
            choice("position", "Position", "", position_kind(), support::choice_value(&POSITION_LABELS, POSITION_AVERAGE)),
            toggle("remove_degenerate_primitives", "Remove degenerate primitives", "Cleanup", true),
            toggle("recompute_point_normals", "Recompute point normals", "Cleanup", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let group = support::optional_text(&p.group);
        let connectivity_attribute = support::optional_text(&p.connectivity_attribute);
        Ok(make(
            label,
            "edge_collapse",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let edges = find_edges(&group, geometry, "missing_group", "edge_collapse could not find native edge group")?;
                done(rdk::edge_collapse::run(
                    &rdk::edge_collapse::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        edges,
                        connectivity_attribute: connectivity_attribute.clone(),
                        position: Some(p.position),
                        remove_degenerate_primitives: Some(p.remove_degenerate_primitives),
                        recompute_point_normals: Some(p.recompute_point_normals),
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Dissolve

pub mod dissolve {
    use super::*;
    use rdk::dissolve::{BridgePolicy, Operation};

    const OPERATION_LABELS: [&str; 2] = ["Selected", "Non-selected"];
    const OPERATION: [Operation; 2] = [Operation::DissolveSelected, Operation::DissolveNonSelected];
    const BRIDGE_LABELS: [&str; 3] = ["Create bridged polygons", "Create disjoint polygons", "Delete bridge polygons"];
    const BRIDGE: [BridgePolicy; 3] =
        [BridgePolicy::CreateBridgedPolygons, BridgePolicy::CreateDisjointPolygons, BridgePolicy::DeleteBridgePolygons];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub operation: Operation,
        pub bridge_policy: BridgePolicy,
        pub remove_inline_points: bool,
        pub collinearity_tolerance: f64,
        pub remove_unused_points: bool,
        pub create_boundary_curves: bool,
        pub recompute_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            operation: pick(&OPERATION_LABELS, &OPERATION, &values[1]),
            bridge_policy: pick(&BRIDGE_LABELS, &BRIDGE, &values[2]),
            remove_inline_points: vb(values, 3),
            collinearity_tolerance: vf(values, 4),
            remove_unused_points: vb(values, 5),
            create_boundary_curves: vb(values, 6),
            recompute_normals: vb(values, 7),
        }
    }

    pub const META: Meta = Meta { key: "dissolve", label: "Dissolve", category: "Topology", inputs: 1, ..decl::META };

    pub fn specs() -> Vec<Spec> {
        vec![
            text("group", "Edge group", "", ""),
            choice("operation", "Operation", "", support::choice_kind(&OPERATION_LABELS), support::choice_value(&OPERATION_LABELS, 0)),
            choice("bridge_policy", "Bridge loops", "", support::choice_kind(&BRIDGE_LABELS), support::choice_value(&BRIDGE_LABELS, 0)),
            toggle("remove_inline_points", "Remove inline points", "Cleanup", true),
            float("collinearity_tolerance", "Collinearity tolerance", "Cleanup", 1e-6, (0., 0.1), (Some(0.), None)),
            toggle("remove_unused_points", "Remove unused points", "Cleanup", true),
            toggle("create_boundary_curves", "Create boundary curves", "", false),
            toggle("recompute_normals", "Recompute normals", "Cleanup", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let group = support::optional_text(&p.group);
        Ok(make(
            label,
            "dissolve",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let edges = find_edges(&group, &inputs[0], "missing_edge_group", "dissolve could not find edge group")?;
                done(rdk::dissolve::run(
                    &rdk::dissolve::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        edges,
                        operation: p.operation,
                        bridge_policy: p.bridge_policy,
                        remove_inline_points: p.remove_inline_points,
                        collinearity_tolerance: p.collinearity_tolerance,
                        remove_unused_points: p.remove_unused_points,
                        create_boundary_curves: p.create_boundary_curves,
                        recompute_normals: p.recompute_normals,
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    declare!();
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
                done(rdk::triangulate::run(
                    &rdk::triangulate::Params { cancel: cancel(context), grain: Some(grain(context)), primitives },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Poly_extrude

pub mod poly_extrude {
    use super::*;
    use rdk::poly_extrude::Divide;

    const DIVIDE_LABELS: [&str; 2] = ["Individual elements", "Connected components"];
    const DIVIDE: [Divide; 2] = [Divide::ExtrudeIndividual, Divide::ExtrudeConnectedComponents];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub split_edges: String,
        pub distance: f64,
        pub divide: Divide,
        pub divisions: i64,
        pub output_front: bool,
        pub output_back: bool,
        pub output_side: bool,
        pub front_group: String,
        pub back_group: String,
        pub side_group: String,
        pub front_boundary_group: String,
        pub back_boundary_group: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            split_edges: vs(values, 1),
            distance: vf(values, 2),
            divide: pick(&DIVIDE_LABELS, &DIVIDE, &values[3]),
            divisions: vi(values, 4),
            output_front: vb(values, 5),
            output_back: vb(values, 6),
            output_side: vb(values, 7),
            front_group: vs(values, 8),
            back_group: vs(values, 9),
            side_group: vs(values, 10),
            front_boundary_group: vs(values, 11),
            back_boundary_group: vs(values, 12),
        }
    }

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
            choice("divide", "Divide into", "", support::choice_kind(&DIVIDE_LABELS), support::choice_value(&DIVIDE_LABELS, 0)),
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
        let p = from_values(values);
        let group = support::optional_text(&p.group);
        let split_edges = support::optional_text(&p.split_edges);
        let (front_group, back_group, side_group) = (
            support::optional_text(&p.front_group),
            support::optional_text(&p.back_group),
            support::optional_text(&p.side_group),
        );
        let (front_boundary_group, back_boundary_group) =
            (support::optional_text(&p.front_boundary_group), support::optional_text(&p.back_boundary_group));
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
                done(rdk::poly_extrude::run(
                    &rdk::poly_extrude::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        primitives,
                        split_edges,
                        divide: p.divide,
                        divisions: p.divisions,
                        output_front: p.output_front,
                        output_back: p.output_back,
                        output_side: p.output_side,
                        front_group: front_group.clone(),
                        back_group: back_group.clone(),
                        side_group: side_group.clone(),
                        front_boundary_group: front_boundary_group.clone(),
                        back_boundary_group: back_boundary_group.clone(),
                        distance: p.distance,
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Poly_fill

pub mod poly_fill {
    use super::*;
    use rdk::poly_fill::Mode;

    const MODE_LABELS: [&str; 3] = ["Single polygon", "Triangles", "Triangle fan"];
    const MODE: [Mode; 3] = [Mode::FillSinglePolygon, Mode::FillTriangles, Mode::FillTriangleFan];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub boundary_group: String,
        pub mode: Mode,
        pub reverse_patches: bool,
        pub unique_points: bool,
        pub update_point_normals: bool,
        pub patch_group: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            boundary_group: vs(values, 0),
            mode: pick(&MODE_LABELS, &MODE, &values[1]),
            reverse_patches: vb(values, 2),
            unique_points: vb(values, 3),
            update_point_normals: vb(values, 4),
            patch_group: vs(values, 5),
        }
    }

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
            choice("mode", "Fill mode", "", support::choice_kind(&MODE_LABELS), support::choice_value(&MODE_LABELS, 1)),
            toggle("reverse_patches", "Reverse patches", "", false),
            toggle("unique_points", "Unique points", "", false),
            toggle("update_point_normals", "Update point normals", "", false),
            text("patch_group", "Patch group", "", ""),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let boundary_group = support::optional_text(&p.boundary_group);
        let patch_group = support::optional_text(&p.patch_group);
        Ok(make(
            label,
            "poly_fill",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let boundary = find_edges(&boundary_group, geometry, "missing_group", "poly_fill could not find boundary edge group")?;
                done(rdk::poly_fill::run(
                    &rdk::poly_fill::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        boundary,
                        mode: p.mode,
                        reverse_patches: p.reverse_patches,
                        unique_points: p.unique_points,
                        update_point_normals: p.update_point_normals,
                        patch_group: patch_group.clone(),
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Remesh

pub mod remesh {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub target_length: f64,
        pub iterations: i64,
        pub smoothing: f64,
        pub project: bool,
        pub use_input_points_only: bool,
        pub hard_point_group: String,
        pub hard_edge_group: String,
        pub target_size_attribute: String,
        pub preserve_uv_seams: bool,
        pub uv_attribute: String,
        pub output_hard_edges: String,
        pub output_mesh_size: String,
        pub output_quality: String,
        pub recompute_point_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            target_length: vf(values, 0),
            iterations: vi(values, 1),
            smoothing: vf(values, 2),
            project: vb(values, 3),
            use_input_points_only: vb(values, 4),
            hard_point_group: vs(values, 5),
            hard_edge_group: vs(values, 6),
            target_size_attribute: vs(values, 7),
            preserve_uv_seams: vb(values, 8),
            uv_attribute: vs(values, 9),
            output_hard_edges: vs(values, 10),
            output_mesh_size: vs(values, 11),
            output_quality: vs(values, 12),
            recompute_point_normals: vb(values, 13),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if p.target_length <= 0. {
            return refuse("remesh", "target length must be finite and positive");
        }
        if p.smoothing < 0. || p.smoothing > 1. {
            return refuse("remesh", "smoothing must be finite and within [0,1]");
        }
        if p.preserve_uv_seams && blank(&p.uv_attribute) {
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
        let p = from_values(values);
        let hard_point_group = support::optional_text(&p.hard_point_group);
        let hard_edge_group = support::optional_text(&p.hard_edge_group);
        let target_size_attribute = support::optional_text(&p.target_size_attribute);
        let output_hard_edges = support::optional_text(&p.output_hard_edges);
        let output_mesh_size = support::optional_text(&p.output_mesh_size);
        let output_quality = support::optional_text(&p.output_quality);
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
                done(rdk::remesh::run(
                    &rdk::remesh::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        iterations: p.iterations,
                        smoothing: p.smoothing,
                        project: p.project,
                        use_input_points_only: p.use_input_points_only,
                        hard_points,
                        hard_edges,
                        target_size_attribute: target_size_attribute.clone(),
                        preserve_uv_seams: p.preserve_uv_seams,
                        uv_attribute: p.uv_attribute.clone(),
                        output_hard_edges: output_hard_edges.clone(),
                        output_mesh_size: output_mesh_size.clone(),
                        output_quality: output_quality.clone(),
                        recompute_point_normals: p.recompute_point_normals,
                        target_length: p.target_length,
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Convex_hull

pub mod convex_hull {
    use super::*;
    use rdk::group_ops::Owner;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group_owner: Owner,
        pub group: String,
        pub preserve_point_payload: bool,
        pub source_point_attribute: String,
        pub hull_group: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group_owner: support::element_owner_of_value(&values[0]),
            group: vs(values, 1),
            preserve_point_payload: vb(values, 2),
            source_point_attribute: vs(values, 3),
            hull_group: vs(values, 4),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        if ocaml_trim(&from_values(values).source_point_attribute) == "P" {
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
            choice("group_owner", "Group type", "", support::element_owner_kind(), support::element_owner_value(Owner::GroupPoints)),
            text("group", "Group", "", ""),
            toggle("preserve_point_payload", "Preserve point payload", "", true),
            text("source_point_attribute", "Source point attribute", "Output", "sourcepoint"),
            text("hull_group", "Hull group", "Output", "hull"),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let selection = support::optional_element_group(p.group_owner, &p.group);
        let source_point_attribute = support::optional_text(&p.source_point_attribute);
        let hull_group = support::optional_text(&p.hull_group);
        Ok(make(
            label,
            "convex_hull",
            1,
            CookMode::Generic,
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let selection = support::resolve_element_group("convex_hull", &selection, &inputs[0])?;
                done(rdk::convex_hull::run(
                    &rdk::convex_hull::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        selection,
                        preserve_point_payload: p.preserve_point_payload,
                        source_point_attribute: source_point_attribute.clone(),
                        hull_group: hull_group.clone(),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    declare!();
}

/// OCaml `String.trim`: ASCII whitespace only.
fn ocaml_trim(text: &str) -> &str {
    text.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c'))
}

// ---------------------------------------------------------------- Clean

pub mod clean {
    use super::*;
    use support::{CleanOverlap, KernelMode};

    const OVERLAPS_LABELS: [&str; 3] = ["Keep first", "Delete pairs", "Auto"];
    const OVERLAPS: [CleanOverlap; 3] = [CleanOverlap::KeepFirst, CleanOverlap::DeletePairs, CleanOverlap::Auto];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub epsilon_mode: KernelMode,
        pub epsilon: f64,
        pub remove_degenerate: bool,
        pub consolidate_mode: KernelMode,
        pub consolidate_distance: f64,
        pub overlaps: CleanOverlap,
        pub reverse_winding: bool,
        pub remove_nan_points: bool,
        pub remove_unused_points: bool,
        pub delete_unused_groups: bool,
        pub point_attributes: String,
        pub vertex_attributes: String,
        pub primitive_attributes: String,
        pub detail_attributes: String,
        pub point_groups: String,
        pub vertex_groups: String,
        pub primitive_groups: String,
        pub edge_groups: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            epsilon_mode: support::kernel_mode_of_value(&values[0]),
            epsilon: vf(values, 1),
            remove_degenerate: vb(values, 2),
            consolidate_mode: support::kernel_mode_of_value(&values[3]),
            consolidate_distance: vf(values, 4),
            overlaps: pick(&OVERLAPS_LABELS, &OVERLAPS, &values[5]),
            reverse_winding: vb(values, 6),
            remove_nan_points: vb(values, 7),
            remove_unused_points: vb(values, 8),
            delete_unused_groups: vb(values, 9),
            point_attributes: vs(values, 10),
            vertex_attributes: vs(values, 11),
            primitive_attributes: vs(values, 12),
            detail_attributes: vs(values, 13),
            point_groups: vs(values, 14),
            vertex_groups: vs(values, 15),
            primitive_groups: vs(values, 16),
            edge_groups: vs(values, 17),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !p.epsilon.is_finite() || p.epsilon < 0. || !p.consolidate_distance.is_finite() || p.consolidate_distance < 0. {
            return refuse("clean", "epsilon and consolidate distance must be finite and nonnegative");
        }
        for pattern in [
            &p.point_attributes,
            &p.vertex_attributes,
            &p.primitive_attributes,
            &p.detail_attributes,
            &p.point_groups,
            &p.vertex_groups,
            &p.primitive_groups,
            &p.edge_groups,
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
        let explicit = || support::kernel_mode_value(KernelMode::Explicit);
        vec![
            choice("epsilon_mode", "Epsilon mode", "Robustness", support::kernel_mode_kind(), explicit()),
            float("epsilon", "Epsilon", "Robustness", 1e-9, (0., 0.001), (Some(0.), None)),
            toggle("remove_degenerate", "Remove degenerate primitives", "", true),
            choice("consolidate_mode", "Consolidate mode", "", support::kernel_mode_kind(), explicit()),
            float("consolidate_distance", "Consolidate distance", "", 0., (0., 0.1), (Some(0.), None)),
            choice("overlaps", "Overlaps", "", support::choice_kind(&OVERLAPS_LABELS), support::choice_value(&OVERLAPS_LABELS, 0)),
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
        let p = from_values(values);
        let epsilon = if p.epsilon_mode == KernelMode::Auto { 1e-12 } else { p.epsilon };
        let consolidate_distance = if p.consolidate_mode == KernelMode::Auto { None } else { Some(p.consolidate_distance) };
        let overlaps = match p.overlaps {
            CleanOverlap::Auto => None,
            CleanOverlap::KeepFirst => Some(rdk::clean::OverlapPolicy::KeepFirstOverlap),
            CleanOverlap::DeletePairs => Some(rdk::clean::OverlapPolicy::DeleteOverlapPairs),
        };
        let (point_attributes, vertex_attributes, primitive_attributes, detail_attributes) = (
            support::optional_text(&p.point_attributes),
            support::optional_text(&p.vertex_attributes),
            support::optional_text(&p.primitive_attributes),
            support::optional_text(&p.detail_attributes),
        );
        let (point_groups, vertex_groups, primitive_groups, edge_groups) = (
            support::optional_text(&p.point_groups),
            support::optional_text(&p.vertex_groups),
            support::optional_text(&p.primitive_groups),
            support::optional_text(&p.edge_groups),
        );
        Ok(make(
            label,
            "clean",
            2,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                done(rdk::clean::run(
                    &rdk::clean::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        epsilon,
                        remove_degenerate: p.remove_degenerate,
                        consolidate_distance,
                        overlaps,
                        reverse_winding: p.reverse_winding,
                        remove_nan_points: p.remove_nan_points,
                        remove_unused_points: p.remove_unused_points,
                        delete_unused_groups: p.delete_unused_groups,
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

    declare!();
}

// ---------------------------------------------------------------- Join_curves

pub mod join_curves {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub picked_ends: String,
        pub orient_closest: bool,
        pub connect_closest_ends: bool,
        pub only_connected: bool,
        pub use_group_size: bool,
        pub group_size: i64,
        pub keep_originals: bool,
        pub tolerance: f64,
        pub wrap: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            picked_ends: vs(values, 1),
            orient_closest: vb(values, 2),
            connect_closest_ends: vb(values, 3),
            only_connected: vb(values, 4),
            use_group_size: vb(values, 5),
            group_size: vi(values, 6),
            keep_originals: vb(values, 7),
            tolerance: vf(values, 8),
            wrap: vb(values, 9),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if p.group_size < 1 {
            return refuse("join_curves", "group size must be positive");
        }
        if !p.tolerance.is_finite() || p.tolerance < 0. {
            return refuse("join_curves", "tolerance must be finite and nonnegative");
        }
        if support::decode_curve_join_picks(&p.picked_ends)?.is_some() && (!blank(&p.group) || p.connect_closest_ends) {
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
        let p = from_values(values);
        let group = support::optional_text(&p.group);
        let picked_ends = support::decode_curve_join_picks(&p.picked_ends).expect("picked ends were validated");
        let group_size = if p.use_group_size { Some(p.group_size) } else { None };
        Ok(make(
            label,
            "join_curves",
            4,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let primitives = match &group {
                    None => None,
                    Some(name) => match geometry::find_group(rdk::group::Owner::Primitive, name, &inputs[0]) {
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
                done(rdk::curve_topology::join_curves(
                    &rdk::curve_topology::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        primitives,
                        picked_ends: picked_ends.clone(),
                        orient_closest: Some(p.orient_closest),
                        connect_closest_ends: Some(p.connect_closest_ends),
                        only_connected: Some(p.only_connected),
                        group_size,
                        keep_originals: Some(p.keep_originals),
                        tolerance: Some(p.tolerance),
                        wrap: Some(p.wrap),
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Facet

pub mod facet {
    use super::*;
    use rdk::group_ops::Owner;
    use support::{FacetConsolidation, KernelMode};

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub group_owner: Owner,
        pub consolidation: FacetConsolidation,
        pub pre_compute_normals: bool,
        pub make_normals_unit_length: bool,
        pub unique_points: bool,
        pub consolidate_distance: f64,
        pub consolidate_normals_distance: f64,
        pub remove_inline_points: bool,
        pub inline_distance: f64,
        pub orient_polygons: bool,
        pub cusp_mode: KernelMode,
        pub cusp_angle: f64,
        pub remove_degenerate: bool,
        pub make_planar: bool,
        pub post_compute_normals: bool,
        pub reverse_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            group_owner: support::element_owner_of_value(&values[1]),
            consolidation: support::facet_consolidation_of_value(&values[2]),
            pre_compute_normals: vb(values, 3),
            make_normals_unit_length: vb(values, 4),
            unique_points: vb(values, 5),
            consolidate_distance: vf(values, 6),
            consolidate_normals_distance: vf(values, 7),
            remove_inline_points: vb(values, 8),
            inline_distance: vf(values, 9),
            orient_polygons: vb(values, 10),
            cusp_mode: support::kernel_mode_of_value(&values[11]),
            cusp_angle: vf(values, 12),
            remove_degenerate: vb(values, 13),
            make_planar: vb(values, 14),
            post_compute_normals: vb(values, 15),
            reverse_normals: vb(values, 16),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if ![p.consolidate_distance, p.consolidate_normals_distance, p.inline_distance, p.cusp_angle].iter().all(|v| v.is_finite()) {
            return refuse("facet", "distance and angle controls must be finite");
        }
        if p.consolidate_distance < 0. || p.consolidate_normals_distance < 0. || p.inline_distance < 0. {
            return refuse("facet", "distances must be nonnegative");
        }
        if p.cusp_angle < 0. || p.cusp_angle > std::f64::consts::PI {
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
            choice("group_owner", "Group owner", "", support::element_owner_kind(), support::element_owner_value(Owner::GroupPrimitives)),
            choice(
                "consolidation",
                "Consolidation",
                "Consolidate",
                support::facet_consolidation_kind(),
                support::choice_value(&support::FACET_CONSOLIDATION_LABELS, 0),
            ),
            toggle("pre_compute_normals", "Pre-compute normals", "Normals", false),
            toggle("make_normals_unit_length", "Make normals unit length", "Normals", false),
            toggle("unique_points", "Unique points", "", false),
            float("consolidate_distance", "Consolidate distance", "Consolidate", 0., (0., 0.1), (Some(0.), None)),
            float("consolidate_normals_distance", "Normal distance", "Consolidate", 0., (0., 0.1), (Some(0.), None)),
            toggle("remove_inline_points", "Remove inline points", "", false),
            float("inline_distance", "Inline distance", "", 1e-6, (0., 0.1), (Some(0.), None)),
            toggle("orient_polygons", "Orient polygons", "", false),
            choice("cusp_mode", "Cusp mode", "Normals", support::kernel_mode_kind(), support::kernel_mode_value(KernelMode::Explicit)),
            float("cusp_angle", "Cusp angle", "Normals", pi, (0., pi), (Some(0.), Some(pi))),
            toggle("remove_degenerate", "Remove degenerate primitives", "", false),
            toggle("make_planar", "Make planar", "", false),
            toggle("post_compute_normals", "Post-compute normals", "Normals", false),
            toggle("reverse_normals", "Reverse normals", "Normals", false),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let selection = support::optional_element_group(p.group_owner, &p.group);
        let consolidate_distance = (p.consolidation == FacetConsolidation::Points).then_some(p.consolidate_distance);
        let consolidate_normals_distance =
            (p.consolidation == FacetConsolidation::Normals).then_some(p.consolidate_normals_distance);
        let cusp_angle = match p.cusp_mode {
            KernelMode::Explicit => Some(p.cusp_angle),
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
                done(rdk::facet::run(
                    &rdk::facet::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        selection,
                        pre_compute_normals: Some(p.pre_compute_normals),
                        make_normals_unit_length: Some(p.make_normals_unit_length),
                        unique_points: Some(p.unique_points),
                        consolidate_distance,
                        consolidate_normals_distance,
                        remove_inline_points: Some(p.remove_inline_points),
                        inline_distance: Some(p.inline_distance),
                        orient_polygons: Some(p.orient_polygons),
                        cusp_angle,
                        remove_degenerate: Some(p.remove_degenerate),
                        make_planar: Some(p.make_planar),
                        post_compute_normals: Some(p.post_compute_normals),
                        reverse_normals: Some(p.reverse_normals),
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Poly_cut

pub mod poly_cut {
    use super::*;
    use rdk::poly_cut::{Detection, Element, Strategy};
    use support::PolyCutDetection;

    const ELEMENT_LABELS: [&str; 2] = ["Points", "Edges"];
    const ELEMENT: [Element; 2] = [Element::PolyCutPoints, Element::PolyCutEdges];
    const STRATEGY_LABELS: [&str; 2] = ["Remove", "Cut"];
    const STRATEGY: [Strategy; 2] = [Strategy::PolyCutRemove, Strategy::PolyCutCut];
    const DETECTION_LABELS: [&str; 3] = ["All selected", "Attribute crossing", "Attribute change"];
    const DETECTION: [PolyCutDetection; 3] = [PolyCutDetection::All, PolyCutDetection::Crossing, PolyCutDetection::Change];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub cut_group: String,
        pub element: Element,
        pub strategy: Strategy,
        pub detection: PolyCutDetection,
        pub attribute: String,
        pub value: f64,
        pub threshold: f64,
        pub keep_closed: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            cut_group: vs(values, 1),
            element: pick(&ELEMENT_LABELS, &ELEMENT, &values[2]),
            strategy: pick(&STRATEGY_LABELS, &STRATEGY, &values[3]),
            detection: pick(&DETECTION_LABELS, &DETECTION, &values[4]),
            attribute: vs(values, 5),
            value: vf(values, 6),
            threshold: vf(values, 7),
            keep_closed: vb(values, 8),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !(p.value.is_finite() && p.threshold.is_finite()) {
            return refuse("poly_cut", "detection controls must be finite");
        }
        if p.threshold < 0. {
            return refuse("poly_cut", "threshold must be nonnegative");
        }
        if p.detection != PolyCutDetection::All && blank(&p.attribute) {
            return refuse("poly_cut", "attribute must be nonblank");
        }
        if p.detection == PolyCutDetection::Crossing && p.attribute == "P" {
            return refuse("poly_cut", "crossing requires scalar storage");
        }
        if p.detection == PolyCutDetection::Change && p.strategy == Strategy::PolyCutCut && p.threshold == 0. {
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
            choice("element", "Cut elements", "", support::choice_kind(&ELEMENT_LABELS), support::choice_value(&ELEMENT_LABELS, 0)),
            choice("strategy", "Strategy", "", support::choice_kind(&STRATEGY_LABELS), support::choice_value(&STRATEGY_LABELS, 0)),
            choice("detection", "Detection", "Detection", support::choice_kind(&DETECTION_LABELS), support::choice_value(&DETECTION_LABELS, 0)),
            text("attribute", "Attribute", "Detection", "cut"),
            float("value", "Crossing value", "Detection", 0.5, (-10., 10.), (None, None)),
            float("threshold", "Change threshold", "Detection", 0., (0., 10.), (Some(0.), None)),
            toggle("keep_closed", "Keep closed", "", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let group = support::optional_text(&p.group);
        let cut_group = support::optional_text(&p.cut_group);
        let detection = match p.detection {
            PolyCutDetection::All => Detection::PolyCutAll,
            PolyCutDetection::Crossing => Detection::PolyCutCrossing { attribute: p.attribute.clone(), value: p.value },
            PolyCutDetection::Change => Detection::PolyCutChange { attribute: p.attribute.clone(), threshold: p.threshold },
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
                let (cut_points, cut_edges) = match p.element {
                    Element::PolyCutPoints => {
                        (support::resolve_optional_point_group("poly_cut", &cut_group, geometry)?, None)
                    }
                    Element::PolyCutEdges => {
                        (None, support::resolve_optional_edge_group("poly_cut", &cut_group, geometry)?)
                    }
                };
                done(rdk::poly_cut::cut(
                    &rdk::poly_cut::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        primitives,
                        cut_points,
                        cut_edges,
                        element: p.element,
                        strategy: p.strategy,
                        detection: detection.clone(),
                        keep_closed: p.keep_closed,
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Poly_reduce

pub mod poly_reduce {
    use super::*;
    use rdk::poly_reduce::Target;
    use support::PolyReduceTarget;

    const TARGET_LABELS: [&str; 2] = ["Percentage", "Primitive count"];
    const TARGET: [PolyReduceTarget; 2] = [PolyReduceTarget::Ratio, PolyReduceTarget::PrimitiveCount];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub hard_point_group: String,
        pub hard_edge_group: String,
        pub target_mode: PolyReduceTarget,
        pub ratio: f64,
        pub primitive_count: i64,
        pub preserve_boundary: bool,
        pub only_original_positions: bool,
        pub equalize_lengths: f64,
        pub limit_normal_deviation: bool,
        pub max_normal_deviation: f64,
        pub output_group: String,
        pub recompute_point_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            hard_point_group: vs(values, 1),
            hard_edge_group: vs(values, 2),
            target_mode: pick(&TARGET_LABELS, &TARGET, &values[3]),
            ratio: vf(values, 4),
            primitive_count: vi(values, 5),
            preserve_boundary: vb(values, 6),
            only_original_positions: vb(values, 7),
            equalize_lengths: vf(values, 8),
            limit_normal_deviation: vb(values, 9),
            max_normal_deviation: vf(values, 10),
            output_group: vs(values, 11),
            recompute_point_normals: vb(values, 12),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !(p.ratio.is_finite() && p.equalize_lengths.is_finite() && p.max_normal_deviation.is_finite()) {
            return refuse("poly_reduce", "controls must be finite");
        }
        if p.ratio < 0. || p.ratio > 1. {
            return refuse("poly_reduce", "ratio must be in [0, 1]");
        }
        if p.primitive_count < 0 {
            return refuse("poly_reduce", "primitive count must be nonnegative");
        }
        if p.equalize_lengths < 0. {
            return refuse("poly_reduce", "equalize lengths must be nonnegative");
        }
        if p.max_normal_deviation < 0. || p.max_normal_deviation > std::f64::consts::PI {
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
            choice("target_mode", "Target", "", support::choice_kind(&TARGET_LABELS), support::choice_value(&TARGET_LABELS, 0)),
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
        let p = from_values(values);
        let group = support::optional_text(&p.group);
        let hard_point_group = support::optional_text(&p.hard_point_group);
        let hard_edge_group = support::optional_text(&p.hard_edge_group);
        let output_group = support::optional_text(&p.output_group);
        let target = match p.target_mode {
            PolyReduceTarget::Ratio => Target::ReduceRatio(p.ratio),
            PolyReduceTarget::PrimitiveCount => Target::ReducePrimitiveCount(p.primitive_count),
        };
        let max_normal_deviation = p.limit_normal_deviation.then_some(p.max_normal_deviation);
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
                    rdk::group::Owner::Point,
                    &hard_point_group,
                    geometry,
                    "missing_group",
                    "poly_reduce could not find hard point group",
                );
                let hard_edges = find_edges(&hard_edge_group, geometry, "missing_group", "poly_reduce could not find hard edge group");
                let (primitives, hard_points, hard_edges) = (primitives?, hard_points?, hard_edges?);
                done(rdk::poly_reduce::run(
                    &rdk::poly_reduce::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        target,
                        primitives,
                        hard_points,
                        hard_edges,
                        preserve_boundary: p.preserve_boundary,
                        only_original_positions: p.only_original_positions,
                        equalize_lengths: p.equalize_lengths,
                        max_normal_deviation,
                        output_group: output_group.clone(),
                        recompute_point_normals: p.recompute_point_normals,
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- shared Boolean tables

use rdk::boolean_materialization::Detriangulation;
use rdk::boolean_payload::PointConflict;
use rdk::boolean_solid::Treatment;

const TREATMENT_LABELS: [&str; 2] = ["Solid", "Surface"];
const TREATMENT: [Treatment; 2] = [Treatment::Solid, Treatment::Surface];
const CONFLICT_LABELS: [&str; 2] = ["Reject conflict", "Promote to vertex"];
const CONFLICT: [PointConflict; 2] = [PointConflict::Reject, PointConflict::PromoteToVertex];
const DETRIANGULATION_LABELS: [&str; 3] = ["Triangles", "Unchanged polygons", "All polygons"];
const DETRIANGULATION: [Detriangulation; 3] =
    [Detriangulation::Triangles, Detriangulation::UnchangedPolygons, Detriangulation::AllPolygons];

fn treatment_kind() -> Kind {
    support::choice_kind(&TREATMENT_LABELS)
}

fn conflict_kind() -> Kind {
    support::choice_kind(&CONFLICT_LABELS)
}

fn detriangulation_kind() -> Kind {
    support::choice_kind(&DETRIANGULATION_LABELS)
}

// ---------------------------------------------------------------- Boolean_seam

pub mod boolean_seam {
    use super::*;
    use rdk::boolean::SeamOutput;

    const OUTPUT_LABELS: [&str; 2] = ["Seam curves", "Coincident patches"];
    const OUTPUT: [SeamOutput; 2] = [SeamOutput::SeamCurves, SeamOutput::CoincidentPatches];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub output: SeamOutput,
        pub left_treatment: Treatment,
        pub right_treatment: Treatment,
        pub resolve_left_self_intersections: bool,
        pub resolve_right_self_intersections: bool,
        pub left_self_group: String,
        pub between_group: String,
        pub right_self_group: String,
        pub coincident_group: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            output: pick(&OUTPUT_LABELS, &OUTPUT, &values[0]),
            left_treatment: pick(&TREATMENT_LABELS, &TREATMENT, &values[1]),
            right_treatment: pick(&TREATMENT_LABELS, &TREATMENT, &values[2]),
            resolve_left_self_intersections: vb(values, 3),
            resolve_right_self_intersections: vb(values, 4),
            left_self_group: vs(values, 5),
            between_group: vs(values, 6),
            right_self_group: vs(values, 7),
            coincident_group: vs(values, 8),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let names = match p.output {
            SeamOutput::SeamCurves => vec![p.left_self_group, p.between_group, p.right_self_group],
            SeamOutput::CoincidentPatches => vec![p.coincident_group],
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
            choice("output", "Output", "", support::choice_kind(&OUTPUT_LABELS), support::choice_value(&OUTPUT_LABELS, 0)),
            choice("left_treatment", "A treatment", "Operands", treatment_kind(), support::choice_value(&TREATMENT_LABELS, 0)),
            choice("right_treatment", "B treatment", "Operands", treatment_kind(), support::choice_value(&TREATMENT_LABELS, 0)),
            toggle("resolve_left_self_intersections", "Resolve A self-intersections", "Operands", false),
            toggle("resolve_right_self_intersections", "Resolve B self-intersections", "Operands", false),
            text("left_self_group", "A self group", "Groups", "boolean_left_self_seam"),
            text("between_group", "Between group", "Groups", "boolean_seam"),
            text("right_self_group", "B self group", "Groups", "boolean_right_self_seam"),
            text("coincident_group", "Coincident group", "Groups", "boolean_coincident"),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let (left_self_group, between_group, right_self_group, coincident_group) = (
            support::optional_text(&p.left_self_group),
            support::optional_text(&p.between_group),
            support::optional_text(&p.right_self_group),
            support::optional_text(&p.coincident_group),
        );
        Ok(make(
            label,
            "boolean_seam",
            1,
            CookMode::Generic,
            vec![first_input(inputs), inputs[1].clone().expect("required input")],
            Arc::new(move |context, inputs| {
                done(rdk::boolean::seam(
                    &rdk::boolean::SeamParams {
                        cancel: cancel(context),
                        grain: grain(context),
                        output: p.output,
                        left_treatment: p.left_treatment,
                        right_treatment: p.right_treatment,
                        resolve_left_self_intersections: p.resolve_left_self_intersections,
                        resolve_right_self_intersections: p.resolve_right_self_intersections,
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

    declare!();
}

// ---------------------------------------------------------------- Poly_bevel

pub mod poly_bevel {
    use super::*;
    use rdk::poly_bevel::Shape;
    use support::PolyBevelShape;

    const SHAPE_LABELS: [&str; 2] = ["Chamfer", "Round"];
    const SHAPE: [PolyBevelShape; 2] = [PolyBevelShape::Chamfer, PolyBevelShape::Round];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub shape: PolyBevelShape,
        pub convexity: f64,
        pub distance: f64,
        pub divisions: i64,
        pub point_scale_attribute: String,
        pub ignore_flat_angle: f64,
        pub clamp_overlap: bool,
        pub edge_group: String,
        pub corner_group: String,
        pub offset_group: String,
        pub recompute_point_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            shape: pick(&SHAPE_LABELS, &SHAPE, &values[1]),
            convexity: vf(values, 2),
            distance: vf(values, 3),
            divisions: vi(values, 4),
            point_scale_attribute: vs(values, 5),
            ignore_flat_angle: vf(values, 6),
            clamp_overlap: vb(values, 7),
            edge_group: vs(values, 8),
            corner_group: vs(values, 9),
            offset_group: vs(values, 10),
            recompute_point_normals: vb(values, 11),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if ![p.convexity, p.distance, p.ignore_flat_angle].iter().all(|v| v.is_finite()) {
            return refuse("poly_bevel", "numeric controls must be finite");
        }
        if p.distance < 0. {
            return refuse("poly_bevel", "distance must be nonnegative");
        }
        if p.divisions < 1 {
            return refuse("poly_bevel", "divisions must be positive");
        }
        if p.convexity < -1. || p.convexity > 1. {
            return refuse("poly_bevel", "convexity must be in [-1, 1]");
        }
        if p.ignore_flat_angle < 0. || p.ignore_flat_angle > std::f64::consts::PI {
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
            choice("shape", "Shape", "", support::choice_kind(&SHAPE_LABELS), support::choice_value(&SHAPE_LABELS, 0)),
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
        let p = from_values(values);
        let shape = match p.shape {
            PolyBevelShape::Chamfer => Shape::BevelChamfer,
            PolyBevelShape::Round => Shape::BevelRound { convexity: p.convexity },
        };
        let group = support::optional_text(&p.group);
        let point_scale_attribute = support::optional_text(&p.point_scale_attribute);
        let (edge_group, corner_group, offset_group) = (
            support::optional_text(&p.edge_group),
            support::optional_text(&p.corner_group),
            support::optional_text(&p.offset_group),
        );
        let ignore_flat_angle = (p.ignore_flat_angle > 0.).then_some(p.ignore_flat_angle);
        Ok(make(
            label,
            "poly_bevel",
            1,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let edges = find_edges(&group, geometry, "missing_edge_group", "poly_bevel could not find edge group")?;
                done(rdk::poly_bevel::run(
                    &rdk::poly_bevel::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        edges,
                        shape,
                        divisions: p.divisions,
                        point_scale_attribute: point_scale_attribute.clone(),
                        ignore_flat_angle,
                        clamp_overlap: p.clamp_overlap,
                        edge_group: edge_group.clone(),
                        corner_group: corner_group.clone(),
                        offset_group: offset_group.clone(),
                        recompute_point_normals: p.recompute_point_normals,
                        distance: p.distance,
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Boolean_fracture

pub mod boolean_fracture {
    use super::*;
    use rdk::boolean_solid::Operation;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub point_conflict: PointConflict,
        pub assume_flat: bool,
        pub resolve_cutter_self_intersections: bool,
        pub detriangulation: Detriangulation,
        pub require_closed: bool,
        pub piece_attribute: String,
        pub point_tolerance: f64,
        pub tiny_seam_threshold: f64,
        pub cleanup_max_batches: i64,
        pub strict_cleanup: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            point_conflict: pick(&CONFLICT_LABELS, &CONFLICT, &values[0]),
            assume_flat: vb(values, 1),
            resolve_cutter_self_intersections: vb(values, 2),
            detriangulation: pick(&DETRIANGULATION_LABELS, &DETRIANGULATION, &values[3]),
            require_closed: vb(values, 4),
            piece_attribute: vs(values, 5),
            point_tolerance: vf(values, 6),
            tiny_seam_threshold: vf(values, 7),
            cleanup_max_batches: vi(values, 8),
            strict_cleanup: vb(values, 9),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !(p.point_tolerance.is_finite() && p.tiny_seam_threshold.is_finite())
            || p.point_tolerance < 0.
            || p.tiny_seam_threshold < 0.
        {
            return refuse("boolean_fracture", "tolerances must be finite and nonnegative");
        }
        if p.cleanup_max_batches < 1 {
            return refuse("boolean_fracture", "cleanup batches must be positive");
        }
        if blank(&p.piece_attribute) {
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
            choice("point_conflict", "Point attribute conflicts", "Attributes", conflict_kind(), support::choice_value(&CONFLICT_LABELS, 1)),
            toggle("assume_flat", "Assume flat", "Output", false),
            toggle("resolve_cutter_self_intersections", "Resolve cutter self-intersections", "", false),
            choice("detriangulation", "Polygons", "", detriangulation_kind(), support::choice_value(&DETRIANGULATION_LABELS, 0)),
            toggle("require_closed", "Require closed", "", true),
            text("piece_attribute", "Piece attribute", "", "piece"),
            float("point_tolerance", "Point tolerance", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            float("tiny_seam_threshold", "Tiny seam threshold", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            int("cleanup_max_batches", "Cleanup batches", "Robustness", 8, (1, 32), Some(1)),
            toggle("strict_cleanup", "Strict cleanup", "Robustness", true),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        Ok(make(
            label,
            "boolean_fracture",
            1,
            CookMode::Generic,
            vec![first_input(inputs), inputs[1].clone().expect("required input")],
            Arc::new(move |context, inputs| {
                done(rdk::boolean::run(
                    &rdk::boolean::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        operation: Operation::Difference,
                        left_treatment: Treatment::Solid,
                        right_treatment: Treatment::Surface,
                        resolve_left_self_intersections: false,
                        resolve_right_self_intersections: p.resolve_cutter_self_intersections,
                        point_conflict: p.point_conflict,
                        point_tolerance: p.point_tolerance,
                        tiny_seam_threshold: p.tiny_seam_threshold,
                        cleanup_max_batches: p.cleanup_max_batches,
                        strict_cleanup: p.strict_cleanup,
                        seam_points: rdk::boolean::SeamPoints::SharedSeamPoints,
                        detriangulation: p.detriangulation,
                        assume_flat: p.assume_flat,
                        require_closed: Some(p.require_closed),
                        piece_attribute: Some(p.piece_attribute.clone()),
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

    declare!();
}

// ---------------------------------------------------------------- Intersection_analysis

pub mod intersection_analysis {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub source_group: String,
        pub collision_group: String,
        pub tolerance: f64,
        pub include_coplanar: bool,
        pub input_attribute: String,
        pub primitive_attribute: String,
        pub primitive_uvw_attribute: String,
        pub point_attribute: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            source_group: vs(values, 0),
            collision_group: vs(values, 1),
            tolerance: vf(values, 2),
            include_coplanar: vb(values, 3),
            input_attribute: vs(values, 4),
            primitive_attribute: vs(values, 5),
            primitive_uvw_attribute: vs(values, 6),
            point_attribute: vs(values, 7),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !p.tolerance.is_finite() || p.tolerance < 0. {
            return refuse("intersection_analysis", "tolerance must be finite and nonnegative");
        }
        let names: Vec<String> =
            [&p.input_attribute, &p.primitive_attribute, &p.primitive_uvw_attribute, &p.point_attribute]
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
        let p = from_values(values);
        let collision = inputs[1].clone();
        let source_group = support::optional_text(&p.source_group);
        let collision_group = if collision.is_some() { support::optional_text(&p.collision_group) } else { None };
        let (input_attribute, primitive_attribute, primitive_uvw_attribute, point_attribute) = (
            support::optional_text(&p.input_attribute),
            support::optional_text(&p.primitive_attribute),
            support::optional_text(&p.primitive_uvw_attribute),
            support::optional_text(&p.point_attribute),
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
                done(rdk::intersection_analysis::run(
                    &rdk::intersection_analysis::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        source_primitives,
                        collision_primitives,
                        tolerance: p.tolerance,
                        include_coplanar: p.include_coplanar,
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

    declare!();
}

// ---------------------------------------------------------------- Fuse

pub mod fuse {
    use super::*;
    use rdk::fuse_grid::FuseMetric;
    use rdk::fuse_rules::DefaultAttributes;
    use rdk::point_snap::{MatchCondition, Targeting, Using};
    use support::FuseTargeting;

    const TARGETING_LABELS: [&str; 2] = ["Near points", "Specified points"];
    const TARGETING: [FuseTargeting; 2] = [FuseTargeting::NearPoints, FuseTargeting::SpecifiedPoints];
    const USING_LABELS: [&str; 2] = ["Least target point", "Closest target point"];
    const USING: [Using; 2] = [Using::LeastTargetPoint, Using::ClosestTargetPoint];
    const ATTRIBUTES_LABELS: [&str; 2] = ["Keep first", "Average numeric"];
    const ATTRIBUTES: [DefaultAttributes; 2] = [DefaultAttributes::KeepFirst, DefaultAttributes::AverageNumeric];
    const METRIC_LABELS: [&str; 2] = ["Euclidean", "Componentwise"];
    const METRIC: [FuseMetric; 2] = [FuseMetric::Euclidean, FuseMetric::Componentwise];
    const CONDITION_LABELS: [&str; 2] = ["Equal", "Unequal"];
    const CONDITION: [MatchCondition; 2] = [MatchCondition::EqualAttributeValues, MatchCondition::UnequalAttributeValues];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub target_group: String,
        pub targeting: FuseTargeting,
        pub target_attribute: String,
        pub using: Using,
        pub tolerance: f64,
        pub position: Position,
        pub weight_attribute: String,
        pub attributes: DefaultAttributes,
        pub attribute_rules: Vec<u8>,
        pub group_rules: Vec<u8>,
        pub metric: FuseMetric,
        pub inclusive: bool,
        pub match_attributes: bool,
        pub radius_attribute: String,
        pub match_attribute: String,
        pub match_condition: MatchCondition,
        pub match_tolerance: f64,
        pub modify_target: bool,
        pub fuse_points: bool,
        pub keep_fused_points: bool,
        pub snapped_group: String,
        pub snapped_destination_attribute: String,
        pub remove_degenerate_primitives: bool,
        pub remove_unused_points_from_degenerate_primitives: bool,
        pub remove_all_unused_points: bool,
    }

    fn bytes(values: &[Value], index: usize) -> Vec<u8> {
        match &values[index] {
            Value::Text(value) => value.clone(),
            _ => panic!("parameter {index} is not text"),
        }
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            target_group: vs(values, 1),
            targeting: pick(&TARGETING_LABELS, &TARGETING, &values[2]),
            target_attribute: vs(values, 3),
            using: pick(&USING_LABELS, &USING, &values[4]),
            tolerance: vf(values, 5),
            position: position_of_value(&values[6]),
            weight_attribute: vs(values, 7),
            attributes: pick(&ATTRIBUTES_LABELS, &ATTRIBUTES, &values[8]),
            attribute_rules: bytes(values, 9),
            group_rules: bytes(values, 10),
            metric: pick(&METRIC_LABELS, &METRIC, &values[11]),
            inclusive: vb(values, 12),
            match_attributes: vb(values, 13),
            radius_attribute: vs(values, 14),
            match_attribute: vs(values, 15),
            match_condition: pick(&CONDITION_LABELS, &CONDITION, &values[16]),
            match_tolerance: vf(values, 17),
            modify_target: vb(values, 18),
            fuse_points: vb(values, 19),
            keep_fused_points: vb(values, 20),
            snapped_group: vs(values, 21),
            snapped_destination_attribute: vs(values, 22),
            remove_degenerate_primitives: vb(values, 23),
            remove_unused_points_from_degenerate_primitives: vb(values, 24),
            remove_all_unused_points: vb(values, 25),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !(p.tolerance.is_finite() && p.match_tolerance.is_finite()) || p.tolerance < 0. || p.match_tolerance < 0. {
            return refuse("fuse", "tolerances must be finite and nonnegative");
        }
        let specified = p.targeting == FuseTargeting::SpecifiedPoints;
        if specified && blank(&p.target_attribute) {
            return refuse("fuse", "specified targets require an attribute name");
        }
        if blank(&p.match_attribute) && (p.match_condition != MatchCondition::EqualAttributeValues || p.match_tolerance != 0.) {
            return refuse("fuse", "match condition and tolerance require a match attribute");
        }
        if specified && (!blank(&p.radius_attribute) || !blank(&p.match_attribute)) {
            return refuse("fuse", "specified targets do not accept radius or match attributes");
        }
        if p.keep_fused_points && !p.fuse_points {
            return refuse("fuse", "keeping fused points requires fusing points");
        }
        if matches!(
            p.position,
            Position::WeightedAverage | Position::WeightedSum | Position::MinimumWeight | Position::MaximumWeight
        ) && blank(&p.weight_attribute)
        {
            return refuse("fuse", "weighted position reduction requires a weight attribute");
        }
        support::decode_fuse_attribute_rules("sop/fuse", &p.attribute_rules)?;
        support::decode_fuse_group_rules("sop/fuse", &p.group_rules)?;
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
            choice("targeting", "Targeting", "", support::choice_kind(&TARGETING_LABELS), support::choice_value(&TARGETING_LABELS, 0)),
            text("target_attribute", "Target point attribute", "", "targetpoint"),
            choice("using", "Use target", "", support::choice_kind(&USING_LABELS), support::choice_value(&USING_LABELS, 0)),
            float("tolerance", "Snap distance", "", 0.001, (0., 10.), (Some(0.), None)),
            choice("position", "Position", "Fuse", position_kind(), support::choice_value(&POSITION_LABELS, POSITION_AVERAGE)),
            text("weight_attribute", "Weight attribute", "Fuse", ""),
            choice("attributes", "Attributes", "Fuse", support::choice_kind(&ATTRIBUTES_LABELS), support::choice_value(&ATTRIBUTES_LABELS, 0)),
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
            choice("metric", "Metric", "Matching", support::choice_kind(&METRIC_LABELS), support::choice_value(&METRIC_LABELS, 0)),
            toggle("inclusive", "Inclusive distance", "Matching", true),
            toggle("match_attributes", "Match attributes", "Matching", false),
            text("radius_attribute", "Radius attribute", "Matching", ""),
            text("match_attribute", "Match attribute", "Matching", ""),
            choice("match_condition", "Match condition", "Matching", support::choice_kind(&CONDITION_LABELS), support::choice_value(&CONDITION_LABELS, 0)),
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
        let p = from_values(values);
        let target = inputs[1].clone();
        if p.modify_target && target.is_some() {
            return Err("sop/fuse: modify target requires a disconnected target input".to_owned());
        }
        let targeting = match p.targeting {
            FuseTargeting::NearPoints => Targeting::NearPoints,
            FuseTargeting::SpecifiedPoints => Targeting::SpecifiedPoints(p.target_attribute.clone()),
        };
        let group = support::optional_text(&p.group);
        let target_group = support::optional_text(&p.target_group);
        let weight_attribute = support::optional_text(&p.weight_attribute);
        let radius_attribute = support::optional_text(&p.radius_attribute);
        let match_attribute = support::optional_text(&p.match_attribute);
        let snapped_group = support::optional_text(&p.snapped_group);
        let snapped_destination_attribute = support::optional_text(&p.snapped_destination_attribute);
        let attribute_rules = support::decode_fuse_attribute_rules("sop/fuse", &p.attribute_rules).expect("rules were validated");
        let group_rules = support::decode_fuse_group_rules("sop/fuse", &p.group_rules).expect("rules were validated");
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
                done(rdk::fuse_grid::fuse(
                    &rdk::fuse_grid::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        selection,
                        target_selection,
                        targeting: targeting.clone(),
                        using: p.using,
                        tolerance: p.tolerance,
                        position: p.position,
                        weight_attribute: weight_attribute.clone(),
                        attributes: p.attributes,
                        attribute_rules: attribute_rules.clone(),
                        group_rules: group_rules.clone(),
                        metric: p.metric,
                        inclusive: p.inclusive,
                        match_attributes: p.match_attributes,
                        radius_attribute: radius_attribute.clone(),
                        match_attribute: match_attribute.clone(),
                        match_condition: p.match_condition,
                        match_tolerance: p.match_tolerance,
                        modify_target: p.modify_target,
                        fuse_points: p.fuse_points,
                        keep_fused_points: p.keep_fused_points,
                        snapped_group: snapped_group.clone(),
                        snapped_destination_attribute: snapped_destination_attribute.clone(),
                        remove_degenerate_primitives: p.remove_degenerate_primitives,
                        remove_unused_points_from_degenerate_primitives: p.remove_unused_points_from_degenerate_primitives,
                        remove_all_unused_points: p.remove_all_unused_points,
                        target: if inputs.len() == 2 { Some(target_geometry.clone()) } else { None },
                    },
                    &inputs[0],
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Subdivide_cracks

pub mod subdivide_cracks {
    use super::*;
    use rdk::subdivide::{
        BoundaryInterpolation, CrackPolicy, CreasingMethod, FaceVaryingInterpolation, Scheme, TrianglePolicy,
    };
    use support::{KernelMode, SubdivisionCracks};

    const SCHEME_LABELS: [&str; 3] = ["Catmull-Clark", "Loop", "Bilinear"];
    const SCHEME: [Scheme; 3] = [Scheme::CatmullClark, Scheme::Loop, Scheme::Bilinear];
    const CRACKS_LABELS: [&str; 7] = [
        "Do not close",
        "Pull, no edge division",
        "Pull, divide edges",
        "Pull, triangulate",
        "Stitch, no edge division",
        "Stitch, divide edges",
        "Stitch, triangulate",
    ];
    const CRACKS: [SubdivisionCracks; 7] = [
        SubdivisionCracks::DoNotClose,
        SubdivisionCracks::PullNoDivision,
        SubdivisionCracks::PullDivide,
        SubdivisionCracks::PullTriangulate,
        SubdivisionCracks::StitchNoDivision,
        SubdivisionCracks::StitchDivide,
        SubdivisionCracks::StitchTriangulate,
    ];
    const BOUNDARY_LABELS: [&str; 3] = ["None", "Edge only", "Edge and corner"];
    const BOUNDARY: [BoundaryInterpolation; 3] = [
        BoundaryInterpolation::SubdivideBoundaryNone,
        BoundaryInterpolation::SubdivideBoundaryEdgeOnly,
        BoundaryInterpolation::SubdivideBoundaryEdgeAndCorner,
    ];
    const FVAR_LABELS: [&str; 6] = ["None", "Corners only", "Corners plus 1", "Corners plus 2", "Boundaries", "All"];
    const FVAR: [FaceVaryingInterpolation; 6] = [
        FaceVaryingInterpolation::SubdivideFvarNone,
        FaceVaryingInterpolation::SubdivideFvarCornersOnly,
        FaceVaryingInterpolation::SubdivideFvarCornersPlus1,
        FaceVaryingInterpolation::SubdivideFvarCornersPlus2,
        FaceVaryingInterpolation::SubdivideFvarBoundaries,
        FaceVaryingInterpolation::SubdivideFvarAll,
    ];
    const TRIANGLE_LABELS: [&str; 2] = ["Catmull-Clark", "Smooth"];
    const TRIANGLE: [TrianglePolicy; 2] =
        [TrianglePolicy::SubdivideTrianglesCatmullClark, TrianglePolicy::SubdivideTrianglesSmooth];
    const CREASING_LABELS: [&str; 2] = ["Uniform", "Chaikin"];
    const CREASING: [CreasingMethod; 2] = [CreasingMethod::SubdivideCreasingUniform, CreasingMethod::SubdivideCreasingChaikin];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group: String,
        pub scheme: Scheme,
        pub iterations: i64,
        pub cracks: SubdivisionCracks,
        pub crack_bias: f64,
        pub consistent_topology: bool,
        pub crease_group: String,
        pub crease_weight_mode: KernelMode,
        pub crease_weight: f64,
        pub generate_resulting_creases: bool,
        pub resulting_crease_group: String,
        pub hole_group: String,
        pub remove_holes: bool,
        pub boundary_interpolation: BoundaryInterpolation,
        pub face_varying_interpolation: FaceVaryingInterpolation,
        pub triangle_policy: TrianglePolicy,
        pub creasing_method: CreasingMethod,
        pub treat_curves_as_independent: bool,
        pub recompute_point_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            group: vs(values, 0),
            scheme: pick(&SCHEME_LABELS, &SCHEME, &values[1]),
            iterations: vi(values, 2),
            cracks: pick(&CRACKS_LABELS, &CRACKS, &values[3]),
            crack_bias: vf(values, 4),
            consistent_topology: vb(values, 5),
            crease_group: vs(values, 6),
            crease_weight_mode: support::kernel_mode_of_value(&values[7]),
            crease_weight: vf(values, 8),
            generate_resulting_creases: vb(values, 9),
            resulting_crease_group: vs(values, 10),
            hole_group: vs(values, 11),
            remove_holes: vb(values, 12),
            boundary_interpolation: pick(&BOUNDARY_LABELS, &BOUNDARY, &values[13]),
            face_varying_interpolation: pick(&FVAR_LABELS, &FVAR, &values[14]),
            triangle_policy: pick(&TRIANGLE_LABELS, &TRIANGLE, &values[15]),
            creasing_method: pick(&CREASING_LABELS, &CREASING, &values[16]),
            treat_curves_as_independent: vb(values, 17),
            recompute_point_normals: vb(values, 18),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if p.iterations < 1 {
            return refuse("subdivide_cracks", "iterations must be positive");
        }
        if !p.crack_bias.is_finite() || p.crack_bias < 0. || p.crack_bias > 1. {
            return refuse("subdivide_cracks", "crack bias must be finite and in [0, 1]");
        }
        if !p.crease_weight.is_finite() || p.crease_weight < 0. {
            return refuse("subdivide_cracks", "crease weight must be finite and nonnegative");
        }
        if !p.generate_resulting_creases && !blank(&p.resulting_crease_group) {
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
            choice("scheme", "Scheme", "", support::choice_kind(&SCHEME_LABELS), support::choice_value(&SCHEME_LABELS, 0)),
            int("iterations", "Depth", "", 1, (1, 6), Some(1)),
            choice("cracks", "Close cracks", "", support::choice_kind(&CRACKS_LABELS), support::choice_value(&CRACKS_LABELS, 0)),
            float("crack_bias", "Pull bias", "Cracks", 0.5, (0., 1.), (None, None)),
            toggle("consistent_topology", "Consistent topology", "Cracks", false),
            text("crease_group", "Crease group", "Creases", ""),
            choice("crease_weight_mode", "Crease weight mode", "Creases", support::kernel_mode_kind(), support::kernel_mode_value(KernelMode::Explicit)),
            float("crease_weight", "Crease weight", "Creases", 1., (0., 10.), (Some(0.), None)),
            toggle("generate_resulting_creases", "Generate resulting creases", "Creases", false),
            text("resulting_crease_group", "Resulting crease group", "Creases", ""),
            text("hole_group", "Hole group", "Holes", "subdivision_hole"),
            toggle("remove_holes", "Remove holes", "Holes", false),
            choice("boundary_interpolation", "Point boundaries", "Interpolation", support::choice_kind(&BOUNDARY_LABELS), support::choice_value(&BOUNDARY_LABELS, 2)),
            choice("face_varying_interpolation", "Vertex boundaries", "Interpolation", support::choice_kind(&FVAR_LABELS), support::choice_value(&FVAR_LABELS, 4)),
            choice("triangle_policy", "Triangles", "Interpolation", support::choice_kind(&TRIANGLE_LABELS), support::choice_value(&TRIANGLE_LABELS, 0)),
            choice("creasing_method", "Creasing method", "Creases", support::choice_kind(&CREASING_LABELS), support::choice_value(&CREASING_LABELS, 0)),
            toggle("treat_curves_as_independent", "Treat curves independently", "", false),
            toggle("recompute_point_normals", "Recompute point normals", "", false),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let creases = inputs[1].clone();
        if creases.is_none() && !blank(&p.crease_group) {
            return Err("sop/subdivide_cracks: crease group requires a crease input".to_owned());
        }
        let cracks = match p.cracks {
            SubdivisionCracks::DoNotClose => CrackPolicy::SubdivideDoNotClose,
            SubdivisionCracks::PullNoDivision => CrackPolicy::SubdividePullNoEdgeDivision,
            SubdivisionCracks::PullDivide => CrackPolicy::SubdividePullDivideEdges(p.crack_bias),
            SubdivisionCracks::PullTriangulate => CrackPolicy::SubdividePullTriangulate(p.crack_bias),
            SubdivisionCracks::StitchNoDivision => CrackPolicy::SubdivideStitchNoEdgeDivision,
            SubdivisionCracks::StitchDivide => CrackPolicy::SubdivideStitchDivideEdges,
            SubdivisionCracks::StitchTriangulate => CrackPolicy::SubdivideStitchTriangulate,
        };
        let group = support::optional_text(&p.group);
        let crease_group = support::optional_text(&p.crease_group);
        let resulting_crease_group = support::optional_text(&p.resulting_crease_group);
        let hole_group = support::optional_text(&p.hole_group);
        let crease_weight = match p.crease_weight_mode {
            KernelMode::Auto => None,
            KernelMode::Explicit => Some(p.crease_weight),
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
                done(rdk::subdivide::subdivide(
                    &rdk::subdivide::Params {
                        cancel: cancel(context),
                        grain: Some(grain(context)),
                        scheme: p.scheme,
                        iterations: p.iterations,
                        primitives,
                        cracks,
                        consistent_topology: p.consistent_topology,
                        creases: if has_creases { Some(inputs[1].clone()) } else { None },
                        crease_primitives,
                        crease_weight,
                        generate_resulting_creases: p.generate_resulting_creases,
                        resulting_crease_group: resulting_crease_group.clone(),
                        hole_primitives,
                        remove_holes: p.remove_holes,
                        boundary_interpolation: p.boundary_interpolation,
                        face_varying_interpolation: p.face_varying_interpolation,
                        triangle_policy: p.triangle_policy,
                        creasing_method: p.creasing_method,
                        treat_curves_as_independent: p.treat_curves_as_independent,
                        recompute_point_normals: p.recompute_point_normals,
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- Boolean

pub mod boolean {
    use super::*;
    use rdk::boolean::SeamPoints;
    use rdk::boolean_solid::Operation;
    use support::BooleanClosedPolicy;

    const OPERATION_LABELS: [&str; 6] =
        ["Union", "Intersection", "Subtract B from A", "Subtract A from B", "Exclusive or", "Shatter"];
    const OPERATION: [Operation; 6] = [
        Operation::Union,
        Operation::Intersection,
        Operation::Difference,
        Operation::ReverseDifference,
        Operation::Xor,
        Operation::Shatter,
    ];
    const SEAM_POINTS_LABELS: [&str; 2] = ["Shared", "Split"];
    const SEAM_POINTS: [SeamPoints; 2] = [SeamPoints::SharedSeamPoints, SeamPoints::SplitSeamPoints];
    const CLOSED_LABELS: [&str; 3] = ["Operation default", "Require closed", "Allow open"];
    const CLOSED: [BooleanClosedPolicy; 3] =
        [BooleanClosedPolicy::Default, BooleanClosedPolicy::Required, BooleanClosedPolicy::NotRequired];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub operation: Operation,
        pub left_treatment: Treatment,
        pub right_treatment: Treatment,
        pub resolve_left_self_intersections: bool,
        pub resolve_right_self_intersections: bool,
        pub point_conflict: PointConflict,
        pub point_tolerance: f64,
        pub tiny_seam_threshold: f64,
        pub cleanup_max_batches: i64,
        pub strict_cleanup: bool,
        pub seam_points: SeamPoints,
        pub detriangulation: Detriangulation,
        pub assume_flat: bool,
        pub require_closed: BooleanClosedPolicy,
        pub piece_attribute: String,
        pub left_piece_group: String,
        pub overlap_piece_group: String,
        pub right_piece_group: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            operation: pick(&OPERATION_LABELS, &OPERATION, &values[0]),
            left_treatment: pick(&TREATMENT_LABELS, &TREATMENT, &values[1]),
            right_treatment: pick(&TREATMENT_LABELS, &TREATMENT, &values[2]),
            resolve_left_self_intersections: vb(values, 3),
            resolve_right_self_intersections: vb(values, 4),
            point_conflict: pick(&CONFLICT_LABELS, &CONFLICT, &values[5]),
            point_tolerance: vf(values, 6),
            tiny_seam_threshold: vf(values, 7),
            cleanup_max_batches: vi(values, 8),
            strict_cleanup: vb(values, 9),
            seam_points: pick(&SEAM_POINTS_LABELS, &SEAM_POINTS, &values[10]),
            detriangulation: pick(&DETRIANGULATION_LABELS, &DETRIANGULATION, &values[11]),
            assume_flat: vb(values, 12),
            require_closed: pick(&CLOSED_LABELS, &CLOSED, &values[13]),
            piece_attribute: vs(values, 14),
            left_piece_group: vs(values, 15),
            overlap_piece_group: vs(values, 16),
            right_piece_group: vs(values, 17),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !(p.point_tolerance.is_finite() && p.tiny_seam_threshold.is_finite())
            || p.point_tolerance < 0.
            || p.tiny_seam_threshold < 0.
        {
            return refuse("boolean", "tolerances must be finite and nonnegative");
        }
        if p.cleanup_max_batches < 1 {
            return refuse("boolean", "cleanup batches must be positive");
        }
        if p.operation == Operation::Shatter {
            if p.left_treatment != Treatment::Solid || p.right_treatment != Treatment::Solid {
                return refuse("boolean", "shatter requires two solid operands");
            }
            let names: Vec<String> = [&p.left_piece_group, &p.overlap_piece_group, &p.right_piece_group]
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
            choice("operation", "Operation", "", support::choice_kind(&OPERATION_LABELS), support::choice_value(&OPERATION_LABELS, 0)),
            choice("left_treatment", "A treatment", "Operands", treatment_kind(), support::choice_value(&TREATMENT_LABELS, 0)),
            choice("right_treatment", "B treatment", "Operands", treatment_kind(), support::choice_value(&TREATMENT_LABELS, 0)),
            toggle("resolve_left_self_intersections", "Resolve A self-intersections", "Operands", false),
            toggle("resolve_right_self_intersections", "Resolve B self-intersections", "Operands", false),
            choice("point_conflict", "Point attribute conflicts", "Attributes", conflict_kind(), support::choice_value(&CONFLICT_LABELS, 1)),
            float("point_tolerance", "Point tolerance", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            float("tiny_seam_threshold", "Tiny seam threshold", "Robustness", 0., (0., 0.001), (Some(0.), None)),
            int("cleanup_max_batches", "Cleanup batches", "Robustness", 8, (1, 32), Some(1)),
            toggle("strict_cleanup", "Strict cleanup", "Robustness", true),
            choice("seam_points", "Seam points", "Output", support::choice_kind(&SEAM_POINTS_LABELS), support::choice_value(&SEAM_POINTS_LABELS, 0)),
            choice("detriangulation", "Polygons", "Output", detriangulation_kind(), support::choice_value(&DETRIANGULATION_LABELS, 0)),
            toggle("assume_flat", "Assume flat", "Output", false),
            choice("require_closed", "Closed output", "Output", support::choice_kind(&CLOSED_LABELS), support::choice_value(&CLOSED_LABELS, 0)),
            text("piece_attribute", "Piece attribute", "Output", ""),
            text("left_piece_group", "A-only group", "Shatter groups", "boolean_left"),
            text("overlap_piece_group", "Overlap group", "Shatter groups", "boolean_overlap"),
            text("right_piece_group", "B-only group", "Shatter groups", "boolean_right"),
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let require_closed = match p.require_closed {
            BooleanClosedPolicy::Default => None,
            BooleanClosedPolicy::Required => Some(true),
            BooleanClosedPolicy::NotRequired => Some(false),
        };
        let piece_attribute = support::optional_text(&p.piece_attribute);
        let left_piece_group = support::optional_text(&p.left_piece_group);
        let overlap_piece_group = support::optional_text(&p.overlap_piece_group);
        let right_piece_group = support::optional_text(&p.right_piece_group);
        Ok(make(
            label,
            "boolean",
            1,
            CookMode::Generic,
            vec![first_input(inputs), inputs[1].clone().expect("required input")],
            Arc::new(move |context, inputs| {
                done(rdk::boolean::run(
                    &rdk::boolean::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        operation: p.operation,
                        left_treatment: p.left_treatment,
                        right_treatment: p.right_treatment,
                        resolve_left_self_intersections: p.resolve_left_self_intersections,
                        resolve_right_self_intersections: p.resolve_right_self_intersections,
                        point_conflict: p.point_conflict,
                        point_tolerance: p.point_tolerance,
                        tiny_seam_threshold: p.tiny_seam_threshold,
                        cleanup_max_batches: p.cleanup_max_batches,
                        strict_cleanup: p.strict_cleanup,
                        seam_points: p.seam_points,
                        detriangulation: p.detriangulation,
                        assume_flat: p.assume_flat,
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

    declare!();
}

// ---------------------------------------------------------------- Boolean_detect

pub mod boolean_detect {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub source_group: String,
        pub collision_group: String,
        pub tolerance: f64,
        pub include_coplanar: bool,
        pub intersecting_group: String,
        pub intersections_attribute: String,
        pub count_attribute: String,
        pub self_intersecting_group: String,
        pub self_intersections_attribute: String,
        pub self_count_attribute: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            source_group: vs(values, 0),
            collision_group: vs(values, 1),
            tolerance: vf(values, 2),
            include_coplanar: vb(values, 3),
            intersecting_group: vs(values, 4),
            intersections_attribute: vs(values, 5),
            count_attribute: vs(values, 6),
            self_intersecting_group: vs(values, 7),
            self_intersections_attribute: vs(values, 8),
            self_count_attribute: vs(values, 9),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !p.tolerance.is_finite() || p.tolerance < 0. {
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
        let p = from_values(values);
        let collision = inputs[1].clone();
        let has_collision = collision.is_some();
        let cross = |name: &str| if has_collision { support::optional_text(name) } else { None };
        let source_group = support::optional_text(&p.source_group);
        let collision_group = cross(&p.collision_group);
        let intersecting_group = cross(&p.intersecting_group);
        let intersections_attribute = cross(&p.intersections_attribute);
        let count_attribute = cross(&p.count_attribute);
        let self_intersecting_group = support::optional_text(&p.self_intersecting_group);
        let self_intersections_attribute = support::optional_text(&p.self_intersections_attribute);
        let self_count_attribute = support::optional_text(&p.self_count_attribute);
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
                done(rdk::boolean_detect::run_checked(
                    &rdk::boolean_detect::Params {
                        cancel: cancel(context),
                        grain: grain(context),
                        source_primitives,
                        collision_primitives,
                        tolerance: p.tolerance,
                        include_coplanar: p.include_coplanar,
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

    declare!();
}

// ---------------------------------------------------------------- Triangulate_2d

pub mod triangulate_2d {
    use super::*;
    use rdk::triangulate2d::Projection;
    use support::Triangulate2dProjection;

    const PROJECTION_LABELS: [&str; 6] = ["Best fit", "XY", "YZ", "ZX", "Custom plane", "Point attribute"];
    const PROJECTION: [Triangulate2dProjection; 6] = [
        Triangulate2dProjection::BestFit,
        Triangulate2dProjection::Xy,
        Triangulate2dProjection::Yz,
        Triangulate2dProjection::Zx,
        Triangulate2dProjection::Plane,
        Triangulate2dProjection::Attribute,
    ];
    /// `Sys.max_array_length` on a 64-bit OCaml.
    const MAX_ARRAY_LENGTH: i64 = (1 << 54) - 1;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub point_group: String,
        pub constraint_edge_group: String,
        pub constraint_primitive_group: String,
        pub projection: Triangulate2dProjection,
        pub plane_origin_x: f64,
        pub plane_origin_y: f64,
        pub plane_origin_z: f64,
        pub plane_normal_x: f64,
        pub plane_normal_y: f64,
        pub plane_normal_z: f64,
        pub point_attribute: String,
        pub seed: i64,
        pub split_crossing_constraints: bool,
        pub flood_from_hull_boundary: bool,
        pub remove_outside_constraint_polygons: bool,
        pub silhouette_constraints: bool,
        pub remove_outside_silhouette: bool,
        pub ignore_non_constraint_points: bool,
        pub remove_duplicate_points: bool,
        pub refine: bool,
        pub allow_constraint_splitting: bool,
        pub minimum_angle: f64,
        pub use_maximum_area: bool,
        pub maximum_area: f64,
        pub use_target_edge_length: bool,
        pub target_edge_length: f64,
        pub minimum_edge_length: f64,
        pub maximum_new_points: i64,
        pub regularization_steps: i64,
        pub allow_movement_of_interior_input_points: bool,
        pub preserve_point_payload: bool,
        pub restore_original_point_positions: bool,
        pub keep_primitives: bool,
        pub remove_unused_points: bool,
        pub recompute_point_normals: bool,
        pub split_point_group: String,
        pub refinement_point_group: String,
        pub triangle_group: String,
        pub constraint_group: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            point_group: vs(values, 0),
            constraint_edge_group: vs(values, 1),
            constraint_primitive_group: vs(values, 2),
            projection: pick(&PROJECTION_LABELS, &PROJECTION, &values[3]),
            plane_origin_x: vf(values, 4),
            plane_origin_y: vf(values, 5),
            plane_origin_z: vf(values, 6),
            plane_normal_x: vf(values, 7),
            plane_normal_y: vf(values, 8),
            plane_normal_z: vf(values, 9),
            point_attribute: vs(values, 10),
            seed: vi(values, 11),
            split_crossing_constraints: vb(values, 12),
            flood_from_hull_boundary: vb(values, 13),
            remove_outside_constraint_polygons: vb(values, 14),
            silhouette_constraints: vb(values, 15),
            remove_outside_silhouette: vb(values, 16),
            ignore_non_constraint_points: vb(values, 17),
            remove_duplicate_points: vb(values, 18),
            refine: vb(values, 19),
            allow_constraint_splitting: vb(values, 20),
            minimum_angle: vf(values, 21),
            use_maximum_area: vb(values, 22),
            maximum_area: vf(values, 23),
            use_target_edge_length: vb(values, 24),
            target_edge_length: vf(values, 25),
            minimum_edge_length: vf(values, 26),
            maximum_new_points: vi(values, 27),
            regularization_steps: vi(values, 28),
            allow_movement_of_interior_input_points: vb(values, 29),
            preserve_point_payload: vb(values, 30),
            restore_original_point_positions: vb(values, 31),
            keep_primitives: vb(values, 32),
            remove_unused_points: vb(values, 33),
            recompute_point_normals: vb(values, 34),
            split_point_group: vs(values, 35),
            refinement_point_group: vs(values, 36),
            triangle_group: vs(values, 37),
            constraint_group: vs(values, 38),
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let numbers = [
            p.plane_origin_x,
            p.plane_origin_y,
            p.plane_origin_z,
            p.plane_normal_x,
            p.plane_normal_y,
            p.plane_normal_z,
            p.minimum_angle,
            p.maximum_area,
            p.target_edge_length,
            p.minimum_edge_length,
        ];
        if !numbers.iter().all(|v| v.is_finite()) {
            return refuse("triangulate_2d", "numeric values must be finite");
        }
        if p.minimum_angle < 0.
            || p.minimum_edge_length < 0.
            || p.maximum_area < 0.
            || p.target_edge_length < 0.
            || p.maximum_new_points < 0
            || p.maximum_new_points >= MAX_ARRAY_LENGTH
            || p.regularization_steps < 0
        {
            return refuse("triangulate_2d", "refinement bounds must be nonnegative");
        }
        if p.refine && (p.minimum_angle <= 0. || p.minimum_angle >= std::f64::consts::PI / 3.) {
            return refuse("triangulate_2d", "minimum angle must lie between zero and pi/3");
        }
        if p.use_maximum_area && p.maximum_area <= 0. {
            return refuse("triangulate_2d", "maximum area must be positive");
        }
        if p.use_target_edge_length && p.target_edge_length <= 0. {
            return refuse("triangulate_2d", "target edge length must be positive");
        }
        if p.projection == Triangulate2dProjection::Plane
            && p.plane_normal_x == 0.
            && p.plane_normal_y == 0.
            && p.plane_normal_z == 0.
        {
            return refuse("triangulate_2d", "plane normal must be nonzero");
        }
        if p.projection == Triangulate2dProjection::Attribute && blank(&p.point_attribute) {
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
            choice("projection", "Projection", "Projection", support::choice_kind(&PROJECTION_LABELS), support::choice_value(&PROJECTION_LABELS, 0)),
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
        let p = from_values(values);
        fn projection_of(p: &Parameters) -> Projection {
        match p.projection {
            Triangulate2dProjection::BestFit => Projection::BestFit,
            Triangulate2dProjection::Xy => Projection::PlaneXy,
            Triangulate2dProjection::Yz => Projection::PlaneYz,
            Triangulate2dProjection::Zx => Projection::PlaneZx,
            Triangulate2dProjection::Plane => Projection::Plane {
                origin: rays_math::vec3::create(p.plane_origin_x, p.plane_origin_y, p.plane_origin_z),
                normal: rays_math::vec3::create(p.plane_normal_x, p.plane_normal_y, p.plane_normal_z),
            },
            Triangulate2dProjection::Attribute => Projection::PointAttribute(p.point_attribute.clone()),
        }
        }
        let maximum_area = p.use_maximum_area.then_some(p.maximum_area);
        let target_edge_length = p.use_target_edge_length.then_some(p.target_edge_length);
        let point_group = support::optional_text(&p.point_group);
        let constraint_edge_group = support::optional_text(&p.constraint_edge_group);
        let constraint_primitive_group = support::optional_text(&p.constraint_primitive_group);
        let split_point_group = support::optional_text(&p.split_point_group);
        let refinement_point_group = support::optional_text(&p.refinement_point_group);
        let triangle_group = support::optional_text(&p.triangle_group);
        let constraint_group = support::optional_text(&p.constraint_group);
        Ok(make(
            label,
            "triangulate_2d",
            12,
            CookMode::DuplicateInput(0),
            vec![first_input(inputs)],
            Arc::new(move |context, inputs| {
                let geometry = &inputs[0];
                let selection = find_group(
                    rdk::group::Owner::Point,
                    &point_group,
                    geometry,
                    "missing_group",
                    "triangulate_2d could not find point group",
                )
                .map(|found| found.map(rdk::element_selection::T::SelectedPoints));
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
                done(rdk::triangulate2d::run(
                    Some(context::cancel_token(context).as_ref()),
                    &rdk::triangulate2d::Params {
                        grain: grain(context),
                        selection,
                        constraint_edges,
                        constraint_primitives,
                        projection: projection_of(&p),
                        seed: p.seed,
                        split_crossing_constraints: p.split_crossing_constraints,
                        flood_from_hull_boundary: p.flood_from_hull_boundary,
                        remove_outside_constraint_polygons: p.remove_outside_constraint_polygons,
                        silhouette_constraints: p.silhouette_constraints,
                        remove_outside_silhouette: p.remove_outside_silhouette,
                        ignore_non_constraint_points: p.ignore_non_constraint_points,
                        remove_duplicate_points: p.remove_duplicate_points,
                        refine: p.refine,
                        allow_constraint_splitting: p.allow_constraint_splitting,
                        minimum_angle: p.minimum_angle,
                        maximum_area,
                        target_edge_length,
                        minimum_edge_length: p.minimum_edge_length,
                        maximum_new_points: p.maximum_new_points,
                        regularization_steps: p.regularization_steps,
                        allow_movement_of_interior_input_points: p.allow_movement_of_interior_input_points,
                        preserve_point_payload: p.preserve_point_payload,
                        restore_original_point_positions: p.restore_original_point_positions,
                        keep_primitives: p.keep_primitives,
                        split_point_group: split_point_group.clone(),
                        refinement_point_group: refinement_point_group.clone(),
                        triangle_group: triangle_group.clone(),
                        constraint_group: constraint_group.clone(),
                        remove_unused_points: p.remove_unused_points,
                        recompute_point_normals: p.recompute_point_normals,
                    },
                    geometry,
                ))
            }),
        ))
    }

    declare!();
}

// ---------------------------------------------------------------- tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::edit_graph;
    use crate::session;
    use crate::test_support::*;

    fn ring() -> geometry::T {
        rdk::line_geometry::polyline(
            &rdk::line_geometry::Params { closed: true },
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
        let (missing, _) = node::apply_parameters(&n, &[("group".to_owned(), Value::Text(b"absent".to_vec()))]).unwrap();
        assert_eq!(cook_code(&missing), "missing_group");
        let (absent, _) =
            node::apply_parameters(&n, &[("connectivity_attribute".to_owned(), Value::Text(b"absent".to_vec()))]).unwrap();
        assert_eq!(cook_code(&absent), "invalid_topology");
        // blank group and attribute are accepted and mean none
        let blank = [("group".to_owned(), Value::Text(b" ".to_vec())), ("connectivity_attribute".to_owned(), Value::Text(b" ".to_vec()))];
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
                ("patch_group".to_owned(), Value::Text(b"patch".to_vec())),
            ],
        )
        .unwrap();
        assert!(effects.cook);
        let mut s = session(8);
        let out = geometry_of(&cook(&mut s, 4, &n));
        // the grid's single 8-edge boundary loop becomes one fan of eight triangles
        let patch = geometry::find_group(group::Owner::Primitive, "patch", &out).expect("patch group");
        assert_eq!(rdk::group::cardinality(&patch), 8);
        assert_eq!(geometry::primitive_count(&out), 12);
        session::close(&mut s);
        let (missing, _) = node::apply_parameters(&n, &[("boundary_group".to_owned(), Value::Text(b"missing".to_vec()))]).unwrap();
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
            assert!(rdk::packed::Float3::View::view(a).unwrap().x == rdk::packed::Float3::View::view(b).unwrap().x, "{key}");
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
            vec![("source_point_attribute", Value::Text(b" P ".to_vec()))],
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
        .map(|(name, value)| (name.to_owned(), Value::Text(value.as_bytes().to_vec())));
        match node::apply_parameters(&detect, &cleared) {
            Ok(_) => panic!("boolean_detect accepted no outputs"),
            Err(error) => assert!(error.contains("sop/boolean_detect: at least one output must be requested"), "{error}"),
        }
        let fused = node_of(&fuse::factory(), &[fixture(), fixture()]);
        match node::apply_parameters(&fused, &[("modify_target".to_owned(), Value::Bool(true))]) {
            Ok(_) => panic!("fuse accepted modify_target with a target"),
            Err(error) => assert!(error.contains("modify target requires a disconnected target input"), "{error}"),
        }
        let creases = [("crease_group".to_owned(), Value::Text(b"c".to_vec()))];
        let sub = node_of(&subdivide_cracks::factory(), &[fixture()]);
        match node::apply_parameters(&sub, &creases) {
            Ok(_) => panic!("subdivide accepted a crease group without creases"),
            Err(error) => assert!(error.contains("crease group requires a crease input"), "{error}"),
        }
    }
}
