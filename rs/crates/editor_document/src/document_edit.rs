//! Port of lib/editor_document/document_edit.ml: the document reducers. Every edit of a
//! `document::T` is rewritten, checked and lowered atomically, or refused and changes nothing.
use crate::document::{self, Origin};
use crate::layout_by_path as lbp;
use crate::layout_edit;
use crate::lowering;
use crate::kinds;
use crate::scene_sync;
use crate::workspace_doc::{self, TextOptions};
use editor_core::panels;
use flow_check::catalog::Catalog;
use flow_graph::flow_edit::{arg_text, ArgKey, Op};
use flow_syntax::syntax::Node;
use param::Effects;
use rays_diagnostic::Diagnostic;
use sop::edit_graph::Factory;

type R<T> = Result<T, Diagnostic>;

pub fn cook_effects() -> Effects {
    param::add_impact(param::Impact::Cook, param::NO_EFFECTS)
}

fn factories_of(factories: Option<&[Factory]>) -> Vec<Factory> {
    factories.map(|f| f.to_vec()).unwrap_or_else(sop::nodes::factories)
}

fn catalog_of(factories: Option<&[Factory]>) -> R<Catalog> {
    kinds::catalog(flow_sop::manifest::VERSION, &factories_of(factories))
}

/// One gesture on a workspace document: rewrite and re-check the source, then lower it into the
/// document's objects. Atomic: an error changes nothing.
fn edit_result(apply: &dyn Fn(&Catalog, &workspace_doc::T) -> R<workspace_doc::T>, factories: Option<&[Factory]>, doc: &document::T) -> R<document::T> {
    let missing = |d: &document::T| !d.workspace.ws.checked.graphs.iter().any(|g| g.name == "scene");
    let run = |d: &document::T| -> R<document::T> {
        let catalog = catalog_of(factories)?;
        let workspace = apply(&catalog, &d.workspace.ws)?;
        lowering::of_workspace(&lowering::Options { factories: Some(factories_of(factories)), previous: Some(d.clone()), ..Default::default() }, &workspace)
    };
    // a gesture on the scene the host composes (the text has no such graph yet, so the edit finds
    // nothing to change): the host's own objects are written out first, and the gesture is tried
    // on that text. Kept only when it then checks.
    match run(doc) {
        Ok(done) => Ok(done),
        Err(refused) => {
            if missing(doc) {
                let adopted = scene_sync::adopt(factories, doc)
                    .map_err(|m| rays_diagnostic::error("E_EDIT".to_owned(), m, None, None, None, Vec::new(), Vec::new()))
                    .and_then(|d| run(&d));
                if let Ok(d) = adopted {
                    return Ok(d);
                }
            }
            Err(refused)
        }
    }
}

pub fn syntax_edit_result(factories: Option<&[Factory]>, doc: &document::T, op: &Op) -> R<document::T> {
    edit_result(&|catalog, ws| workspace_doc::edit(catalog, ws, op), factories, doc)
}

/// A layout gesture is applied like a text one, and is atomic the same way.
pub fn layout_op_result(factories: Option<&[Factory]>, doc: &document::T, op: &layout_edit::Op) -> R<document::T> {
    edit_result(&|catalog, ws| workspace_doc::edit_layout(catalog, ws, op), factories, doc)
}

pub fn layout_op(factories: Option<&[Factory]>, doc: &document::T, op: &layout_edit::Op) -> Result<document::T, String> {
    layout_op_result(factories, doc, op).map_err(|d| rays_diagnostic::to_string(&d))
}

/// One gesture on the text.
pub fn syntax_edit(factories: Option<&[Factory]>, doc: &document::T, op: &Op) -> Result<document::T, String> {
    syntax_edit_result(factories, doc, op).map_err(|d| rays_diagnostic::to_string(&d))
}

/// Several rewrites, one gesture: all or none. The host's own objects are written out first when
/// the batch adds to a scene graph the document does not have yet.
pub fn syntax_batch(factories: Option<&[Factory]>, doc: &document::T, ops: &[Op]) -> Result<document::T, String> {
    let adds_to_scene = ops.iter().any(|op| matches!(op, Op::AddNode { scope, .. } if scope.len() == 1 && scope[0] == "scene"));
    let missing = !doc.workspace.ws.checked.graphs.iter().any(|g| g.name == "scene");
    let mut current = if adds_to_scene && missing { scene_sync::adopt(factories, doc)? } else { doc.clone() };
    for op in ops {
        current = syntax_edit(factories, &current, op)?;
    }
    Ok(current)
}

/// The whole workspace text, edited: parsed and checked as a document, lowered, atomic.
pub fn text_edit(imports: Option<&[(String, String)]>, factories: Option<&[Factory]>, doc: &document::T, text: &str) -> Result<document::T, Vec<Diagnostic>> {
    let workspace = &doc.workspace.ws;
    let catalog = catalog_of(factories).map_err(|d| vec![d])?;
    let options = TextOptions {
        ops: Some((*workspace.checked.ops).clone()),
        imports: imports.map(|i| i.to_vec()).unwrap_or_else(|| workspace_doc::import_texts(workspace)),
        inputs: (*workspace.inputs).clone(),
        settings: Some(workspace.settings.clone()),
        layout: Some(workspace.layout.clone()),
    };
    let edited = workspace_doc::of_text(&options, &catalog, text)?;
    lowering::of_workspace(&lowering::Options { factories: Some(factories_of(factories)), previous: Some(doc.clone()), ..Default::default() }, &edited).map_err(|d| vec![d])
}

/// A change to the workspace's layout keys (a moved item, a collapsed zone): the source and the
/// lowering are untouched, so nothing recooks.
pub fn layout_edit(doc: &document::T, f: &dyn Fn(&lbp::T) -> lbp::T) -> document::T {
    let mut next = doc.clone();
    next.workspace.ws.layout = f(&doc.workspace.ws.layout);
    next
}

pub fn select_layout(factories: Option<&[Factory]>, doc: &document::T, name: &str) -> Result<document::T, String> {
    let mut workspace = doc.workspace.ws.clone();
    workspace.layout.editor = Some(name.to_owned());
    match workspace_doc::editor_graph(&workspace) {
        None => Err(format!("Unknown editor layout {name}.")),
        Some(_) => lowering::of_workspace(&lowering::Options { factories: Some(factories_of(factories)), previous: Some(doc.clone()), ..Default::default() }, &workspace)
            .map_err(|d| rays_diagnostic::to_string(&d)),
    }
}

/// A derived edit (an object's field, a reparent, a World layer) written to the text.
pub fn reconcile(factories: Option<&[Factory]>, adopt: Option<bool>, before: &document::T, after: &document::T) -> Result<document::T, String> {
    scene_sync::reconcile(factories, adopt, before, after)
}

/// The binding of the panel at tree `path` (an assignable node for a gesture): a named panel is
/// its binding, one written in place is bound first (the call holding it unfolded).
pub fn panel_node(factories: Option<&[Factory]>, loop_message: Option<&str>, doc: &document::T, path: &panels::Path) -> Result<(document::T, Vec<String>), String> {
    let loop_message = loop_message.unwrap_or("These panels are copies made by a loop: edit the loop in the editor graph.");
    let graph = workspace_doc::editor_graph(&doc.workspace.ws).map(|g| g.name);
    let origin = doc.shell.as_ref().and_then(|s| s.origins.iter().find(|(p, _)| p == path).map(|(_, o)| o.clone()));
    match (graph, origin) {
        (Some(graph), Some(Origin::Bound(name))) => Ok((doc.clone(), vec![graph, name])),
        (_, Some(Origin::Inline(home, key))) => {
            let (doc, node) = scene_sync::bind_home(factories, doc, &home)?;
            let doc = syntax_edit(factories, &doc, &Op::Unfold { node: node.clone(), key: key.clone(), sub: Vec::new() })?;
            match arg_text(&doc.workspace.ws.source, &node, &key) {
                Some(f) => match f.node {
                    Node::Sym(name) => {
                        let mut path = node[..node.len() - 1].to_vec();
                        path.push(name);
                        Ok((doc, path))
                    }
                    _ => Err("The panel could not be named.".to_owned()),
                },
                None => Err("The panel could not be named.".to_owned()),
            }
        }
        (_, Some(Origin::Loop(_, _))) => Err(loop_message.to_owned()),
        _ => Err("This panel is not part of an editor layout.".to_owned()),
    }
}

#[allow(dead_code)]
fn _key_unused(_: ArgKey) {}
