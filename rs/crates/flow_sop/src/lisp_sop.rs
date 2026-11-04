//! Port of lib/flow_sop/lisp_sop/lisp_sop.ml. Test support: a SOP graph written as Rays Lisp text,
//! compiled to a `sop::node::T` by the same lowering the editor uses.
use crate::{catalog, lower, manifest, operators};
use flow_check::op;
use flow_syntax::context as fctx;
use flow_syntax::syntax;
use sop::edit_graph::{self as edit, Factory};
use sop::node::{self, build, CookMode};
use sop::payload;
use std::collections::HashMap;
use std::sync::Arc;

pub fn factories() -> Vec<Factory> {
    sop::nodes::factories()
}

/// A generator node holding an already built geometry: the one OCaml-only source, used through
/// `with_`.
pub fn snapshot(geometry: &rdk::geometry::T) -> node::T {
    let parameters = format!("data_id={};bytes={}", rdk::geometry::data_id(geometry), rdk::geometry::payload_bytes(geometry));
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
    let lowered = catalog::of_factories(manifest::VERSION, &[], None, &all).and_then(|cat| match catalog::check_workspace(&image_ops, false, None, &cat, &forms) {
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

#[allow(dead_code)]
fn _unused(_: &payload::T) {}

#[cfg(test)]
mod support;
#[cfg(test)]
mod test_concat;
#[cfg(test)]
mod test_planes;
#[cfg(test)]
mod test_iterate;
#[cfg(test)]
mod test_settle;
#[cfg(test)]
mod test_grid_bulk;
#[cfg(test)]
mod test_lisp_inline;
#[cfg(test)]
mod test_lisp_node;
#[cfg(test)]
mod test_node_contracts;
#[cfg(test)]
mod test_lower_core;
#[cfg(test)]
mod test_lisp_fusion;
#[cfg(test)]
mod test_node_facts;
#[cfg(test)]
mod test_node_sources;
#[cfg(test)]
mod test_graph;
#[cfg(test)]
mod test_workspace_cook;
#[cfg(test)]
mod test_workspace_edit;
#[cfg(test)]
mod test_workspace_zone;
#[cfg(test)]
mod test_perf_lower;
#[cfg(test)]
mod test_projection;
#[cfg(test)]
mod test_lisp_projection;
#[cfg(test)]
mod test_metadata_kernel;
#[cfg(test)]
mod test_group_kernel;
#[cfg(test)]
mod test_remap;
#[cfg(test)]
mod test_mesh_resident;
#[cfg(test)]
mod test_trace;

#[cfg(test)]
mod tests {
    use super::*;

    fn cook(n: &node::T) -> rdk::geometry::T {
        let context = sop::context::create(sop::context::Options::default()).unwrap();
        let mut session = sop::session::create(8, 1_000_000).unwrap();
        let out = sop::session::cook(&mut session, None, &context, n).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
        sop::session::close(&mut session);
        payload::geometry(&out.payload).unwrap()
    }

    // lib/flow_sop/lisp_sop/test_workspace_node.ml
    #[test]
    #[ignore = "blocked upstream: grid lowering needs the flow_check error_rows shape [rows mapper :policy p] and then hits the irfix flow_ir deadlock"]
    fn workspace_node_helpers() {
        let workspace = "(workspace shared\n    (defn size :context value [] 3)\n    (graph g :context sop (sop/grid :columns (size) :rows 2)))";
        assert_eq!(rdk::geometry::point_count(&cook(&workspace_node(&[], workspace))), 12, "workspace definitions were not available while lowering");
        let w = "(workspace shared\n    (graph g :context sop (sop/ext_src)))";
        let source = node(&[], "(sop/grid :columns 3 :rows 2)");
        assert!(node::ptr_eq(&workspace_node(&[("src".to_owned(), source.clone())], w), &source), "workspace external marker was not restored");
        assert_eq!(rdk::geometry::point_count(&cook(&node(&[], "(sop/grid :columns 3 :rows 2)"))), 12, "expression helper changed");
    }

    #[test]
    fn external_marker_is_restored_and_a_line_cooks() {
        let line = rdk::line_geometry::points(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]).unwrap();
        let source = snapshot(&line);
        let w = "(workspace shared
    (graph g :context sop (sop/ext_src)))";
        assert!(node::ptr_eq(&workspace_node(&[("src".to_owned(), source.clone())], w), &source), "workspace external marker was not restored");
        assert_eq!(rdk::geometry::point_count(&cook(&node(&[], "(sop/line :points 4)"))), 4, "expression helper changed");
    }

    #[test]
    fn invalid_workspaces_fail_with_a_diagnostic() {
        for text in ["(", "(workspace empty (graph g :context sop nil))", "(workspace value_only (graph g :context value 1))", "(workspace bad (graph g :context sop (sop/not_a_node)))"] {
            assert!(workspace_node_result(&[], text).is_err(), "invalid workspace did not fail with a lowering diagnostic: {text}");
        }
    }
}
