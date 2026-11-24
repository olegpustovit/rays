//! A layout edit checks and lowers only the editor graph (and graphs nothing else references) over the
//! last whole check: on every sketch with an editor graph, each layout gesture gives the document a whole
//! rebuild gives, and undoing it restores the text it had.
use crate::document::{self, Origin};
use crate::{document_edit, layout_edit as le, lowering, preset, settings, workspace_doc};
use flow_check::workspace as w;
use flow_syntax::syntax as s;

fn load(path: &std::path::Path) -> Option<document::T> {
    preset::load(None, path.to_str()?, None, &settings::none()).ok().map(|loaded| loaded.doc)
}

fn same_checked(label: &str, a: &w::Workspace, b: &w::Workspace) {
    assert_eq!(a.graphs.len(), b.graphs.len(), "{label}: graphs");
    for (x, y) in a.graphs.iter().zip(b.graphs.iter()) {
        assert!(x == y, "{label}: graph {} differs from a whole check", x.name);
    }
    assert!(a.live == b.live, "{label}: live");
    assert!(a.invariant == b.invariant, "{label}: invariant");
    assert!(a.packed == b.packed, "{label}: packed");
    assert!(a.approx_reasons == b.approx_reasons, "{label}: approx reasons");
    assert!(a.packed_roots == b.packed_roots, "{label}: packed roots");
    assert!(a.kind_fns == b.kind_fns && a.defs == b.defs && a.nodes.len() == b.nodes.len() && a.macros.len() == b.macros.len(), "{label}: definitions");
}

fn same_doc(label: &str, a: &document::T, b: &document::T) {
    assert_eq!(workspace_doc::to_text(&a.workspace.ws).unwrap(), workspace_doc::to_text(&b.workspace.ws).unwrap(), "{label}: text");
    same_checked(label, &a.workspace.ws.checked, &b.workspace.ws.checked);
    assert!(a.workspace.ws.layout == b.workspace.ws.layout, "{label}: layout keys");
    match (&a.shell, &b.shell) {
        (Some(x), Some(y)) => {
            // objects a viewport makes anew take fresh ids on each lowering: their keys and counts compare
            let counts = |v: &[(String, Vec<i64>)]| v.iter().map(|(k, ids)| (k.clone(), ids.len())).collect::<Vec<_>>();
            assert_eq!(counts(&x.views), counts(&y.views), "{label}: views");
            let y = document::Shell { views: x.views.clone(), ..(**y).clone() };
            assert!(lowering::same_shell(x, &y), "{label}: shell");
        }
        (None, None) => {}
        _ => panic!("{label}: shell presence"),
    }
    let objects = |g: &[crate::kind_graph::Node]| g.iter().map(|n| (n.kind.clone(), n.label.clone())).collect::<Vec<_>>();
    assert_eq!(objects(&a.scene), objects(&b.scene), "{label}: scene");
    assert!(a.networks.len() == b.networks.len() && a.layers.len() == b.layers.len(), "{label}: networks");
    assert!(settings::same_values(&a.settings, &b.settings), "{label}: settings");
    assert_eq!(a.workspace.lowered.plan.instances.len(), b.workspace.lowered.plan.instances.len(), "{label}: instances");
    assert_eq!(a.workspace.lowered.plan.nodes.len(), b.workspace.lowered.plan.nodes.len(), "{label}: plan nodes");
    // separate evaluations number their functions apart: the editor's value compares as shown
    let editors: Vec<&str> = a.workspace.ws.checked.graphs.iter().filter(|g| g.context == flow_syntax::context::editor()).map(|g| g.name.as_str()).collect();
    for (x, y) in a.workspace.lowered.plan.instances.iter().zip(&b.workspace.lowered.plan.instances) {
        assert!(x.graph == y.graph && x.default == y.default, "{label}: instance {}", x.graph);
        if editors.contains(&x.graph.as_str()) {
            assert_eq!(flow_eval::show(&x.result), flow_eval::show(&y.result), "{label}: instance {}", x.graph);
        }
    }
}

fn gestures(doc: &document::T) -> Vec<le::Op> {
    let Some(graph) = workspace_doc::editor_graph(&doc.workspace.ws).map(|g| g.name) else { return Vec::new() };
    let panel = |t: &str| s::parse(t).unwrap().remove(0);
    let mut ops = Vec::new();
    let bound: Vec<String> = doc.shell.iter().flat_map(|sh| sh.origins.iter()).filter_map(|(_, o)| match o { Origin::Bound(n) => Some(n.clone()), _ => None }).collect();
    if let Some(name) = bound.first() {
        let node: w::Path = vec![graph.clone().into(), name.clone().into()];
        ops.push(le::Op::SplitPanel { node: node.clone(), axis: le::Axis::H, ratio: 0.5, panel: panel("(ui/inspector)") });
        ops.push(le::Op::SetPanelKind { node: node.clone(), panel: panel("(ui/lisp)") });
    }
    if bound.len() > 1 {
        ops.push(le::Op::ClosePanel { node: vec![graph.clone().into(), bound[1].clone().into()] });
    }
    ops.push(le::Op::LayoutNew { graph: graph.clone() });
    if doc.shell.as_ref().is_some_and(|sh| sh.switch.as_ref().is_some_and(|sw| sw.layouts.len() > 1)) {
        ops.push(le::Op::SetLayout { graph, index: 1 });
    }
    ops
}

#[test]
fn layout_gestures_match_a_whole_rebuild_on_every_editor_sketch() {
    // a sketch per shape of editor graph: views and a canvas, a switch, a viewport over another scene
    // instance, a layout written after other graphs
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../sketches");
    let mut checked = 0;
    for name in ["isogrid.rays", "basic.rays", "particles.rays", "ws_variations.rays", "pxui.rays"] {
        let path = dir.join(name);
        let Some(doc) = load(&path) else { continue };
        for op in gestures(&doc) {
            let label = format!("{} {}", path.file_name().unwrap().to_string_lossy(), le::label(&op));
            let fast = document_edit::layout_op(None, &doc, &op);
            w::forget_retained();
            let whole = document_edit::layout_op(None, &doc, &op);
            match (fast, whole) {
                (Ok(fast), Ok(whole)) => {
                    same_doc(&label, &fast, &whole);
                    // undo: the document before the gesture, lowered again on top of the one after it
                    let back = lowering::restore(None, &fast, &document::snapshot(&doc)).expect("restore");
                    same_doc(&format!("{label} undone"), &back, &doc);
                    checked += 1;
                }
                (Err(a), Err(b)) => assert_eq!(a, b, "{label}: refusals"),
                (a, b) => panic!("{label}: {:?} / {:?}", a.err(), b.err()),
            }
        }
    }
    assert!(checked >= 11, "only {checked} gestures compared");
}
