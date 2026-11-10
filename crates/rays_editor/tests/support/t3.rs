//! Test support shared by suite t3: ports of lisp_sop.ml, Test_support.Workspace_cook, Editor_prepare and
//! test_text.ml (copied from flow_sop/src/lisp_sop.rs and lisp_sop/support.rs, which are cfg(test) there).
#![allow(dead_code)]
// Test support: a SOP graph written as Rays Lisp text,
// compiled to a `sop::node::T` by the same lowering the editor uses.
use flow_sop::{catalog, lower, manifest, operators};
use flow_check::op;
use flow_syntax::context as fctx;
use flow_syntax::syntax;
use sop::edit_graph::{self as edit, Factory};
use sop::node::{self, build, CookMode};
use sop::payload;
use std::collections::HashMap;
use rdk_core::geometry as geo;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::session;
use std::sync::Arc;

pub fn factories() -> Vec<Factory> {
    sop::nodes::factories()
}

/// A generator retains already-built geometry through with_.
pub fn snapshot(geometry: &rdk_core::geometry::T) -> node::T {
    let parameters = format!("data_id={};bytes={}", rdk_core::geometry::data_id(geometry), rdk_core::geometry::payload_bytes(geometry));
    let geometry = geometry.clone();
    build::make_geometry(
        build::Args { parameters: parameters.into_bytes(), cook_mode: CookMode::Generator, ..build::args("snapshot") },
        None,
        Arc::new(move |_, _, _| Ok(node::GeometryCooked { geometry: geometry.clone(), instances: None })),
    )
}

/// An external node may itself have inputs, but a factory node cannot: the lowering sees an
/// input-free marker per external node and the real nodes are put back afterwards.
fn marker(name: &str) -> node::T {
    build::make_geometry(
        build::Args { parameters: name.as_bytes().to_vec(), cook_mode: CookMode::Generator, ..build::args("lisp_sop_ext") },
        None,
        Arc::new(|_, _, _| panic!("Lisp_sop: marker cooked")),
    )
}

fn restore(with_: &[(String, node::T)], root: &node::T) -> node::T {
    // a DAG (an inlined body reads its input many times): each node is rebuilt once
    fn go(with_: &[(String, node::T)], restored: &mut HashMap<i64, node::T>, n: &node::T) -> node::T {
        if let Some(found) = restored.get(&node::id(n)) {
            return found.clone();
        }
        let result = if node::operation(n) == "lisp_sop_ext" {
            let name = String::from_utf8_lossy(node::parameters(n)).into_owned();
            with_.iter().find(|(k, _)| *k == name).map(|(_, v)| v.clone()).expect("an external node")
        } else {
            let inputs = node::inputs(n);
            if inputs.is_empty() {
                n.clone()
            } else {
                let rebuilt: Vec<node::T> = inputs.iter().map(|i| go(with_, restored, i)).collect();
                if inputs.iter().zip(rebuilt.iter()).all(|(a, b)| node::ptr_eq(a, b)) { n.clone() } else { build::rebuild_with_inputs(n, &rebuilt) }
            }
        };
        restored.insert(node::id(n), result.clone());
        result
    }
    go(with_, &mut HashMap::new(), root)
}

/// A graph's result: a card's factory node (`T.card_node`) when it is a Lisp call, otherwise its
/// compiled node.
pub fn root(lowered: &lower::T, graph: &lower::Graph) -> Result<node::T, String> {
    let root = graph.root.expect("a graph with a result");
    let card = lowered.compiled.iter().any(|(id, cid)| *cid == root && lowered.plan.nodes[*id as usize].kind == flow_eval::card_kind());
    if card {
        match (lowered.card_node)(root) {
            Ok(n) => return Ok(n),
            Err(_) => {}
        }
    }
    edit::compile_node(&graph.network.geometry, root)
}

/// Compile a complete workspace and return the first default SOP graph's result. External nodes
/// make `(sop/ext_NAME)` stand for the existing node; `image/NAME` gives `(image/ext_NAME)`.
pub fn workspace_node_result(with_: &[(String, node::T)], source: &str) -> Result<node::T, String> {
    let image_name = |name: &str| name.strip_prefix("image/").map(str::to_owned);
    let factory_key = |name: &str| match image_name(name) {
        Some(n) => format!("lisp_sop_image_ext_{n}"),
        None => format!("ext_{name}"),
    };
    let external_factories: Vec<Factory> = with_
        .iter()
        .map(|(name, _)| {
            let m = marker(name);
            edit::factory(edit::factory_args(&factory_key(name), &format!("Ext {name}"), &["Test"]), 0, Arc::new(move |_| Ok(m.clone())))
        })
        .collect();
    let prototype = op::find(&[], "image/noise", &fctx::value()).expect("image/noise");
    let mut image_ops: Vec<op::Op> = with_
        .iter()
        .filter_map(|(name, _)| {
            image_name(name).map(|n| {
                let key = format!("sop/{}", factory_key(name));
                op::Op {
                    name: format!("image/ext_{n}"),
                    signature: op::Signature { pos: Vec::new(), opt: Vec::new(), rest: None, kw: Vec::new() },
                    check: Arc::new(|_| Ok(())),
                    packed_extension: None,
                    body: Arc::new(move |_live, node, _| node(&key, &[])),
                    ..prototype.clone()
                }
            })
        })
        .collect();
    image_ops.extend(operators::all().iter().cloned());
    let mut all = external_factories;
    all.extend(factories());
    let forms = syntax::parse(source).map_err(|d| rays_diagnostic::to_string(&d))?;
    // the catalog of a set of external names is built once per thread (a marker only carries its name)
    thread_local! { static CATALOGS: std::cell::RefCell<HashMap<Vec<String>, flow_check::catalog::Catalog>> = std::cell::RefCell::new(HashMap::new()); }
    let names: Vec<String> = with_.iter().map(|(n, _)| n.clone()).collect();
    let cached = CATALOGS.with(|c| c.borrow().get(&names).cloned());
    let catalog_result = match cached {
        Some(c) => Ok(c),
        None => catalog::of_factories(manifest::VERSION, &[], None, &all).inspect(|c| {
            CATALOGS.with(|m| m.borrow_mut().insert(names.clone(), c.clone()));
        }),
    };
    let lowered = catalog_result.and_then(|cat| match catalog::check_workspace(&image_ops, false, None, &cat, &forms) {
        (Some(checked), _) => lower::of_checked(Some(all.clone()), lower::Options::default(), &checked),
        (None, diagnostics) => Err(diagnostics
            .into_iter()
            .find(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity)
            .unwrap_or_else(|| rays_diagnostic::error("E_LOWER".into(), "Workspace did not check".into(), None, None, None, Vec::new(), Vec::new()))),
    });
    match lowered {
        Err(d) => Err(rays_diagnostic::to_string(&d)),
        Ok(lowered) => match lowered.graphs.iter().find(|g| g.default) {
            None => Err("the workspace has no SOP graph".to_owned()),
            Some(graph) if graph.root.is_none() => Err("the graph has no result".to_owned()),
            Some(graph) => root(&lowered, graph).map(|n| restore(with_, &n)),
        },
    }
}

pub fn workspace_node(with_: &[(String, node::T)], text: &str) -> node::T {
    match workspace_node_result(with_, text) {
        Ok(n) => n,
        Err(message) => panic!("Lisp_sop: {message}\n{text}"),
    }
}

/// `text` lowers as the result of a `sop` graph: a Lisp call as its factory node (not inlined).
pub fn node_result(with_: &[(String, node::T)], text: &str) -> Result<node::T, String> {
    workspace_node_result(with_, &format!("(workspace w (graph g :context sop {text}))"))
}

pub fn node(with_: &[(String, node::T)], text: &str) -> node::T {
    match node_result(with_, text) {
        Ok(n) => n,
        Err(message) => panic!("Lisp_sop: {message}\n{text}"),
    }
}

/// A float as a Lisp literal that reads back as the same float.
pub fn float(value: f64) -> String {
    flow_syntax::lisp::float(value)
}

pub fn vec3(v: &rays_math::vec3::T) -> String {
    format!("[{} {} {}]", float(v.x), float(v.y), float(v.z))
}

/// `[[x y z] ...]` for `(sop/curve ...)`.
pub fn curve_points(points: &[(f64, f64, f64)]) -> String {
    format!("[{}]", points.iter().map(|(x, y, z)| format!("[{} {} {}]", float(*x), float(*y), float(*z))).collect::<Vec<_>>().join(" "))
}


pub fn parse(text: &str) -> Vec<syntax::Syntax> {
    syntax::parse(text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

/// The first default graph of a workspace, lowered, with its compiled root node.
pub fn lower_text(options: lower::Options, text: &str) -> (lower::T, node::T) {
    let lowered = lower::workspace(None, Vec::new(), options, &parse(text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let graph = lowered.graphs.iter().find(|g| g.default).expect("a default graph").clone();
    let root = edit::compile_node(&graph.network.geometry, graph.root.expect("a graph result")).expect("compile");
    (lowered, root)
}

pub fn new_session(entries: i64, bytes: i64) -> session::T {
    session::create(entries, bytes).expect("session")
}

pub fn context(domains: i64, grain: i64) -> sctx::T {
    sctx::create(ContextOptions { domains: Some(domains), grain: Some(grain), ..ContextOptions::default() }).expect("context")
}

pub fn try_cook(session: &mut session::T, context: &sctx::T, n: &node::T) -> Result<geo::T, rays_diagnostic::Diagnostic> {
    let out = session::cook(session, None, context, n)?;
    payload::geometry(&out.payload)
}

pub fn cook(session: &mut session::T, context: &sctx::T, n: &node::T) -> geo::T {
    try_cook(session, context, n).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)))
}

/// One cook in a fresh session.
pub fn cook_once(domains: i64, n: &node::T) -> geo::T {
    let mut session = new_session(64, 64 * 1024 * 1024);
    let g = cook(&mut session, &context(domains, 257), n);
    session::close(&mut session);
    g
}

/// A byte image of a geometry: positions, topology and every attribute, to compare exactly.
pub fn geometry_bytes(g: &geo::T) -> Vec<u8> {
    let mut out = String::new();
    let positions = geo::positions(g);
    for i in 0..rdk_core::packed::float3::length(positions) {
        let (x, y, z) = rdk_core::packed::float3::get(positions, i).expect("position");
        out.push_str(&format!("{:016x}{:016x}{:016x};", x.to_bits(), y.to_bits(), z.to_bits()));
    }
    if let Ok(view) = rdk_core::topology::view::view(geo::topology(g)) {
        out.push_str(&format!("T{}|{:?}|{:?}|{:?};", view.point_count, view.vertex_points, view.primitive_offsets, view.primitive_kinds));
    }
    for a in geo::attributes(g) {
        out.push_str(&format!("A{:?}:{}:{};", rdk_core::attribute::owner(&a), rdk_core::attribute::name(&a), rdk_core::attribute::kind_name(&a)));
        match rdk_core::attribute::storage(&a) {
            Ok(rdk_core::attribute::Storage::Float(v)) => out.push_str(&v.iter().map(|x| format!("{:016x}", x.to_bits())).collect::<String>()),
            Ok(rdk_core::attribute::Storage::Int(v)) => out.push_str(&format!("{v:?}")),
            Ok(rdk_core::attribute::Storage::Text(v)) => out.push_str(&format!("{v:?}")),
            Ok(rdk_core::attribute::Storage::Float2(v)) => {
                for i in 0..rdk_core::packed::float2::length(&v) {
                    out.push_str(&format!("{:?}", rdk_core::packed::float2::get(&v, i).map(|(x, y)| (x.to_bits(), y.to_bits()))));
                }
            }
            Ok(rdk_core::attribute::Storage::Float3(v)) => {
                for i in 0..rdk_core::packed::float3::length(&v) {
                    out.push_str(&format!("{:?}", rdk_core::packed::float3::get(&v, i).map(|(x, y, z)| (x.to_bits(), y.to_bits(), z.to_bits()))));
                }
            }
            Ok(rdk_core::attribute::Storage::Float4(v)) => {
                for i in 0..rdk_core::packed::float4::length(&v) {
                    out.push_str(&format!("{:?}", rdk_core::packed::float4::get(&v, i).map(|(a, b, c, d)| (a.to_bits(), b.to_bits(), c.to_bits(), d.to_bits()))));
                }
            }
            Ok(_) => out.push_str("array"),
            Err(e) => out.push_str(&format!("{e:?}")),
        }
        out.push(';');
    }
    for group in geo::groups(g) {
        out.push_str(&format!("G{:?}:{}:", rdk_core::group::owner(&group), rdk_core::group::name(&group)));
        for i in 0..rdk_core::group::length(&group) {
            out.push(if rdk_core::group::mem(i, &group) { '1' } else { '0' });
        }
        out.push(';');
    }
    out.into_bytes()
}

/// Read specification cases directly; bloom/variations are standalone fixtures and remaining sources live in sketches/ws_NAME.rays.
pub fn case_text(name: &str) -> String {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let path = if name == "bloom" || name == "variations" {
        root.join("specification/workspace/cases").join(format!("{name}.lisp"))
    } else {
        root.join("sketches").join(format!("ws_{name}.rays"))
    };
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

// ---- Editor_prepare ----------------------------------------------------------------------------

use rays_editor::cook;

/// `Editor_prepare.surface`: the cooked geometry, as the app prepares it.
pub fn prepare_surface() -> cook::Prepare_fn {
    std::rc::Rc::new(|s, o| cook::prepare(s, o))
}

/// A prepared that is the single point (x, 0, 0): a test marks what a prepare saw with it.
pub fn prepared_at(x: f64) -> Result<cook::prepared, String> {
    prepared_of(vec![x], vec![0.], vec![0.], 1)
}

fn prepared_of(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>, points: i64) -> Result<cook::prepared, String> {
    let positions = rdk_core::packed::float3::of_owned(x, y, z).map_err(|e| format!("{e:?}"))?;
    let topology = rdk_core::topology::empty(points).map_err(|e| format!("{e:?}"))?;
    let geometry = geo::create(positions, topology, None, None, None).map_err(|e| format!("{e:?}"))?;
    let surface = rdk_rays::surface::of_geometry(None, &geometry)?;
    Ok(cook::prepared { geometry, surface })
}

pub fn empty_prepared() -> Result<cook::prepared, String> {
    prepared_of(Vec::new(), Vec::new(), Vec::new(), 0)
}

/// `Editor_prepare.empty`.
pub fn prepare_empty() -> cook::Prepare_fn {
    std::rc::Rc::new(|_, _| empty_prepared())
}

pub fn first_point(p: &cook::prepared) -> (f64, f64, f64) {
    rdk_core::packed::float3::get(geo::positions(&p.geometry), 0).expect("a first point")
}

pub fn x_of(p: &cook::prepared) -> f64 {
    first_point(p).0
}

/// Poll until true within the existing 60-second deadline.
pub fn await_until(mut predicate: impl FnMut() -> bool) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
    loop {
        if predicate() {
            return;
        }
        assert!(std::time::Instant::now() < deadline, "editor cook barrier timeout");
        std::thread::sleep(std::time::Duration::from_micros(100));
    }
}

/// Debug trace to a file (`%TEMP%/t3log.txt`): `cargo t` hides the output of a killed case.
pub fn log(s: &str) {
    use std::io::Write;
    thread_local! { static T0: std::time::Instant = std::time::Instant::now(); }
    let ms = T0.with(|t| t.elapsed().as_millis());
    let s = format!("[{ms} ms] {s}");
    if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(std::env::temp_dir().join("t3log.txt")) {
        let _ = writeln!(f, "{s}");
    }
}

// ---- Test_support.Projection / Graph_pane over the editor's catalog -------------------------------

use flow_check::catalog::Catalog;
use flow_check::workspace::Workspace;

/// `Editor_document.Kinds.catalog ~version:1 Sop.Nodes.factories` (the Lisp SOP nodes included).
pub fn catalog() -> Catalog {
    (*rays_editor::workspace_catalog(None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))).clone()
}

pub fn checked(text: &str) -> Workspace {
    match rays_editor::workspace::load(None, Vec::new(), None, text) {
        Ok(doc) => (*doc.checked).clone(),
        Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

/// Read standalone bloom/variations fixtures and shared sketches/ws_NAME.rays sources directly.
pub fn read_case(name: &str) -> String {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let path = if name == "bloom" || name == "variations" { root.join("specification/workspace/cases").join(format!("{name}.lisp")) } else { root.join("sketches").join(format!("ws_{name}.rays")) };
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}
