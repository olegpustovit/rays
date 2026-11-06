//! Carry: what putting a payload at a place means.
//!
//! A payload is a Flow value held as text (kind "material" or "sop", value "(ref cobalt)"). A place
//! is a target when the edit that writes the value there passes the checker, so there is no list of
//! what a widget accepts: `put` runs the edit (the one the inspector or the graph already writes) and
//! the document's own check, and its result is the preview, the refusal's reason and the write. See
//! specification/flow.md, "Carry".
#![allow(non_camel_case_types)]
use editor_core::panels::{self, Panel};
use editor_document::document::{self, T as Document};
use editor_document::{document_edit, scene_sync};
use flow_check::catalog::Catalog;
use flow_check::workspace as w;
use flow_graph::flow_edit::{self as e, ArgKey, Op};
use flow_graph::projection as p;
use flow_syntax::context;
use flow_syntax::syntax::{self as s, Node, Syntax};
use sop::edit_graph::Factory;

pub use pxui::ui::Payload;

use crate::text_pane;

#[derive(Clone, Debug, PartialEq)]
pub enum Place {
    /// a node of a graph pane, or of a text tile
    Node(w::Path),
    /// the body of a graph: its empty canvas
    Graph(String),
    /// a geometry object of the scene
    Object(i64),
    /// the surface of a geometry object a viewport shows under the pointer, with the material graph
    /// its primitive reads
    Surface { object_: i64, material: Option<String> },
    /// a viewport panel under the pointer, before the host's pick says which surface
    Viewport(String),
    /// a byte of the text pane's shown text, where `(ref name)` would be inserted
    Text { target: Text_target, text: String, byte: i64 },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Text_target {
    /// the Document tab: the whole workspace text
    Whole,
    /// the Graph tab of this graph
    Graph_text(String),
    /// the Selection tab: the closure of this binding
    Selection_text(w::Path),
    /// the tab holds an unapplied draft, whose bytes are not the document's
    Drafted,
}

/// what a put plans: ops over a document, or the whole text rewritten
pub enum Edit {
    Edits(Document, Vec<Op>, String),
    Rewrite(String, String),
}

type R<T> = Result<T, String>;
type Factories<'a> = Option<&'a [Factory]>;

fn name_of(expr: &Syntax) -> Option<String> {
    match expr.node {
        Node::List(items) if expr.range(items).len() == 2 => {
            let items = expr.range(items);
            match (expr.arena().row(items[0]).node, expr.arena().row(items[1]).node) {
                (Node::Sym(head), Node::Sym(name)) if expr.name(head) == "ref" => Some(expr.name(name).to_owned()),
                _ => None,
            }
        },
        _ => None,
    }
}

/// the payload as an expression, and the graph it names
pub fn parse(payload: &Payload) -> R<(Syntax, String)> {
    match s::parse(&payload.value) {
        Ok(forms) if forms.len() == 1 => {
            let expr = forms[0].clone();
            if payload.kind == "camera" {
                match &expr.node {
                    Node::Sym(name) => {
                        let name = expr.name(*name).to_owned();
                        Ok((expr, name))
                    }
                    _ => Err("A carried camera is the name of its object".to_string()),
                }
            } else {
                match name_of(&expr) {
                    Some(name) => Ok((expr, name)),
                    None => Err("A carried value is a graph reference".to_string()),
                }
            }
        }
        _ => Err("A carried value is one form".to_string()),
    }
}

fn find_graph<'a>(doc: &'a Document, name: &str) -> Option<&'a w::Graph> {
    doc.workspace.ws.checked.graphs.iter().find(|g| g.name == name)
}

/// the lowered SOP graph a geometry object instantiates, by its network
pub fn graph_of_object(doc: &Document, id: i64) -> Option<String> {
    document::object_graph(doc, id)
}

pub fn geometry_objects(doc: &Document) -> Vec<(i64, String)> {
    document::scene_graph(doc).iter().filter(|n| n.kind == "geometry").map(|n| (n.id, n.label.clone())).collect()
}

fn readers(doc: &Document, graph: &str) -> usize {
    geometry_objects(doc).iter().filter(|(id, _)| graph_of_object(doc, *id).as_deref() == Some(graph)).count()
}

fn shared(doc: &Document, graph: &str) -> String {
    match readers(doc, graph) {
        n if n > 1 => format!(" \u{b7} {n} objects read it"),
        _ => String::new(),
    }
}

fn scope_of(catalog: &Catalog, doc: &Document, graph: &str) -> Option<p::Scope> {
    let checked = &doc.workspace.ws.checked;
    p::find_graph(checked, graph)?;
    p::of_graph(catalog, checked, graph).ok()
}

/// a `sop/material` that has no group: it paints everything that reaches it
fn whole_material(n: &p::Node) -> bool {
    n.head == "sop/material" && !n.rows.iter().any(|r| r.key == ArgKey::Kw("group".into()) && r.expr.is_some())
}

fn reads_material(n: &p::Node, graph: &str) -> bool {
    n.head == "sop/material"
        && n.rows
            .iter()
            .any(|r| r.key == ArgKey::Kw("material".into()) && r.expr.as_ref().and_then(name_of).as_deref() == Some(graph))
}

fn with_argument(head: &str, input: &str, key: &str, expr: &Syntax) -> Syntax {
    use flow_syntax::arena::{Draft, SyntaxBuilder};
    let mut builder = SyntaxBuilder::new();
    let mut items = Vec::with_capacity(4);
    for draft in [Draft::Sym(head.to_owned()), Draft::Sym(input.to_owned()), Draft::Kw(key.to_owned())] {
        items.push(builder.push_draft(0, draft, Default::default(), None));
    }
    items.push(builder.import(expr));
    let id = builder.push_draft(0, Draft::List(items), Default::default(), None);
    builder.freeze().handle(id)
}

/// A material put on a SOP graph as a whole: its result node when that is a material node, else a
/// new one after it that becomes the result.
fn material_on_graph(catalog: &Catalog, doc: &Document, graph: &str, expr: &Syntax, material: &str) -> R<(Vec<Op>, String)> {
    let Some(g) = find_graph(doc, graph) else { return Err(format!("There is no graph {graph}")) };
    if g.context != context::sop() {
        return Err(format!("A {} graph has no surface", w::context_name(&g.context)));
    }
    let Some(scope) = scope_of(catalog, doc, graph) else { return Err(format!("There is no graph {graph}")) };
    match &scope.result {
        p::ScopeResult::Link(name) => match p::find(&scope, &[graph.to_string(), name.clone()]) {
            Some(n) if whole_material(&n) => Ok((
                vec![Op::SetArg { node: n.path.clone(), key: ArgKey::Kw("material".into()), sub: vec![], value: expr.clone() }],
                format!(":material (ref {material}) in graph {graph}{}", shared(doc, graph)),
            )),
            _ => {
                let fresh = e::fresh_name(&doc.workspace.ws.source, graph, "material");
                Ok((
                    vec![
                        Op::AddNode {
                            scope: vec![graph.to_string()],
                            name: fresh.clone(),
                            expr: with_argument("sop/material", &name, "material", expr),
                        },
                        Op::Connect { node: vec![graph.to_string(), "@result".to_string()], key: ArgKey::Whole, src: fresh, iter: false },
                    ],
                    format!("a sop/material node and :material (ref {material}) in graph {graph}{}", shared(doc, graph)),
                ))
            }
        },
        p::ScopeResult::Node(_) | p::ScopeResult::Literal(_) => Err(format!("The result of {graph} is not a node a material can follow")),
    }
}

/// `text` with `value` inserted at `byte`, spaced from what it touches
pub fn insert_at(text: &str, byte: usize, value: &str) -> String {
    let mut byte = byte.min(text.len());
    while !text.is_char_boundary(byte) {
        byte -= 1;
    }
    let (before, after) = text.split_at(byte);
    let opens = |c: char| " \n\t([{".contains(c);
    let closes = |c: char| " \n\t)]}".contains(c);
    let lead = match before.chars().last() {
        None => "",
        Some(c) if opens(c) => "",
        Some(_) => " ",
    };
    let trail = match after.chars().next() {
        None => "",
        Some(c) if closes(c) => "",
        Some(_) => " ",
    };
    format!("{before}{lead}{value}{trail}{after}")
}

// ---- viewport panels: a scene graph or a camera put on one ----

/// the viewport panels of the active layout, in tree order: key and tree path
pub fn viewports(doc: &Document) -> Vec<(String, panels::Path)> {
    match &doc.shell {
        None => Vec::new(),
        Some(shell) => panels::leaves(&shell.tree)
            .into_iter()
            .filter_map(|(path, panel)| match panel {
                Panel::View(key) => Some((key, path)),
                _ => None,
            })
            .collect(),
    }
}

/// A scene graph on a viewport panel re-points the panel's `(ui/viewport (ref ...))`; a camera makes
/// the `:camera` of the root of the scene the panel shows (a root is written first for a part).
/// Either way the panel is bound first when it is written in place.
fn on_viewport(
    factories: Factories,
    catalog: &Catalog,
    doc: &Document,
    payload: &Payload,
    expr: &Syntax,
    name: &str,
    key: &str,
) -> R<(Document, Vec<Op>, String)> {
    let path = match viewports(doc).into_iter().find(|(k, _)| k == key) {
        Some((_, path)) => path,
        None => return Err("That viewport is not part of the editor graph's layout".to_string()),
    };
    let (bound, node) = document_edit::panel_node(factories, None, doc, &path)?;
    let source = &bound.workspace.ws.source;
    let shown = e::arg_text(source, &node, &ArgKey::Pos(0)).as_ref().and_then(name_of);
    if payload.kind == "scene" {
        match find_graph(doc, name) {
            Some(g) if g.context == context::scene() && shown.as_deref() == Some(name) => Err(format!("That viewport already shows {name}")),
            Some(g) if g.context == context::scene() => Ok((
                bound,
                vec![Op::SetArg { node, key: ArgKey::Pos(0), sub: vec![], value: expr.clone() }],
                format!("(ui/viewport (ref {name}))"),
            )),
            Some(g) => Err(format!("A {} graph is not a scene", w::context_name(&g.context))),
            None => Err(format!("There is no graph {name}")),
        }
    } else {
        let Some(scene) = shown else { return Err("That viewport shows no scene graph".to_string()) };
        let Some(scope) = scope_of(catalog, &bound, &scene) else { return Err(format!("There is no scene graph {scene}")) };
        let on_root = |n: &p::Node, bound: Document| -> R<(Document, Vec<Op>, String)> {
            Ok((
                bound,
                vec![Op::SetArg { node: n.path.clone(), key: ArgKey::Kw("camera".into()), sub: vec![], value: expr.clone() }],
                format!(":camera {name} on {} in graph {scene}", if n.synthetic { "its result" } else { n.name.as_str() }),
            ))
        };
        match &scope.result {
            p::ScopeResult::Node(path) => match p::find(&scope, path) {
                Some(n) if n.head == "scene/root" => on_root(&n, bound),
                _ => Err(format!("{scene} returns a scene written in place: name it first, then put the camera")),
            },
            p::ScopeResult::Link(result) => match p::find(&scope, &[scene.clone(), result.clone()]) {
                Some(n) if n.head == "scene/root" => on_root(&n, bound),
                _ => {
                    let fresh = e::fresh_name(&bound.workspace.ws.source, &scene, "root");
                    let ops = vec![
                        Op::AddNode {
                            scope: vec![scene.clone()],
                            name: fresh.clone(),
                            expr: with_argument("scene/root", &result, "camera", expr),
                        },
                        Op::Connect { node: vec![scene.clone(), "@result".to_string()], key: ArgKey::Whole, src: fresh, iter: false },
                    ];
                    Ok((bound, ops, format!("a scene/root with :camera {name} in graph {scene}")))
                }
            },
            p::ScopeResult::Literal(_) => Err(format!("The result of {scene} is not a scene a root can follow")),
        }
    }
}

/// The edit of a payload at a place: the document it applies to (an object's home is bound first),
/// the ops of one gesture, and the words of the text that say what they write.
pub fn plan(factories: Factories, catalog: &Catalog, doc: &Document, payload: &Payload, place: &Place) -> R<Edit> {
    let (expr, name) = parse(payload)?;
    let material = payload.kind == "material";
    let ok = |on: &Document, ops: Vec<Op>, what: String| -> R<Edit> { Ok(Edit::Edits(on.clone(), ops, what)) };
    let geometry_in_scene = |existing: &str| -> R<Edit> {
        ok(doc, scene_sync::add_geometry(doc, Some(existing)), format!("a scene/geometry (ref {existing}) binding and a merge input"))
    };
    let object_graph = |id: i64| -> R<String> { graph_of_object(doc, id).ok_or_else(|| "That object has no SOP graph".to_string()) };
    let label = |id: i64| -> String { geometry_objects(doc).into_iter().find(|(i, _)| *i == id).map(|(_, l)| l).unwrap_or_else(|| "object".to_string()) };
    let with_doc = |(ops, what): (Vec<Op>, String)| -> R<Edit> { ok(doc, ops, what) };
    let repoint = |id: i64| -> R<Edit> {
        let current = object_graph(id)?;
        if current == name {
            return Err(format!("{} already shows {name}", label(id)));
        }
        match doc.homes.objects.iter().find(|(i, _)| *i == id) {
            None => Err("That object is not in the text".to_string()),
            Some((_, home)) => {
                let (bound, path) = scene_sync::bind_home(factories, doc, home)?;
                ok(
                    &bound,
                    vec![Op::SetArg { node: path, key: ArgKey::Pos(0), sub: vec![], value: expr.clone() }],
                    format!("{} (scene/geometry (ref {name}))", label(id)),
                )
            }
        }
    };
    let is_view_payload = payload.kind == "scene" || payload.kind == "camera";
    match place {
        Place::Viewport(key) if is_view_payload => {
            on_viewport(factories, catalog, doc, payload, &expr, &name, key).map(|(on, ops, what)| Edit::Edits(on, ops, what))
        }
        _ if is_view_payload => Err(format!("A {} goes on a viewport panel", payload.kind)),
        Place::Viewport(_) => Err("Move over a surface".to_string()),
        Place::Text { target, text, byte } => {
            let byte = (*byte).clamp(0, text.len() as i64) as usize;
            if payload.kind == "camera" {
                return Err("A camera is put on a viewport panel".to_string());
            }
            let upto = {
                let mut b = byte;
                while !text.is_char_boundary(b) {
                    b -= 1;
                }
                &text[..b]
            };
            let line = 1 + upto.bytes().filter(|c| *c == b'\n').count();
            let what = format!("{} at line {line} of the text", payload.value);
            let inserted = insert_at(text, byte, &payload.value);
            let source = &doc.workspace.ws.source;
            match target {
                Text_target::Drafted => Err("The text has an unapplied draft: apply or discard it first".to_string()),
                Text_target::Whole => Ok(Edit::Rewrite(inserted, what)),
                Text_target::Graph_text(graph) => {
                    let op = text_pane::graph_op(source, graph, None, &inserted).map_err(|d| d.message)?;
                    ok(doc, vec![op], what)
                }
                Text_target::Selection_text(path) => {
                    let op = text_pane::graph_op(source, &path[0], Some(path.as_slice()), &inserted).map_err(|d| d.message)?;
                    ok(doc, vec![op], what)
                }
            }
        }
        Place::Graph(graph) => match find_graph(doc, graph) {
            Some(g) if g.context == context::scene() && !material => geometry_in_scene(&name),
            Some(g) if g.context == context::sop() && material => material_on_graph(catalog, doc, graph, &expr, &name).and_then(with_doc),
            Some(g) => Err(format!("A {} graph takes no {}", w::context_name(&g.context), payload.kind)),
            None => Err(format!("There is no graph {graph}")),
        },
        Place::Surface { object_, material: surface } if material => {
            let graph = object_graph(*object_)?;
            let on_node = surface.as_ref().and_then(|reading| {
                scope_of(catalog, doc, &graph).and_then(|scope| scope.nodes.iter().find(|n| reads_material(n, reading)).cloned())
            });
            match on_node {
                Some(n) => ok(
                    doc,
                    vec![Op::SetArg { node: n.path.clone(), key: ArgKey::Kw("material".into()), sub: vec![], value: expr.clone() }],
                    format!(":material (ref {name}) on {} in graph {graph}{}", n.name, shared(doc, &graph)),
                ),
                None => material_on_graph(catalog, doc, &graph, &expr, &name).and_then(with_doc),
            }
        }
        Place::Object(id) if material => {
            let graph = object_graph(*id)?;
            material_on_graph(catalog, doc, &graph, &expr, &name).and_then(with_doc)
        }
        Place::Surface { object_: id, .. } | Place::Object(id) => repoint(*id),
        Place::Node(path) if path.is_empty() => Err("Nothing is there".to_string()),
        Place::Node(path) => {
            let root = &path[0];
            let node = scope_of(catalog, doc, root).and_then(|scope| p::find(&scope, path));
            let head = node.as_ref().map(|n| n.head.clone()).unwrap_or_default();
            let here = path.join("/");
            // a scene/geometry node: the SOP graph of the object it makes
            let shown = node
                .as_ref()
                .and_then(|n| n.rows.iter().find_map(|r| if r.key == ArgKey::Pos(0) { r.expr.clone() } else { None }))
                .as_ref()
                .and_then(name_of);
            let assign = |key: ArgKey, what: String| -> R<Edit> {
                ok(doc, vec![Op::SetArg { node: path.clone(), key, sub: vec![], value: expr.clone() }], what)
            };
            if material {
                match (head.as_str(), &shown) {
                    ("scene/geometry", Some(graph)) => material_on_graph(catalog, doc, graph, &expr, &name).and_then(with_doc),
                    _ => assign(ArgKey::Kw("material".into()), format!(":material (ref {name}) on {here}")),
                }
            } else {
                match (head.as_str(), &shown) {
                    ("scene/merge", _) => geometry_in_scene(&name),
                    ("scene/geometry", Some(current)) if *current == name => Err(format!("{here} already shows {name}")),
                    ("scene/geometry", _) => assign(ArgKey::Pos(0), format!("{here} (scene/geometry (ref {name}))")),
                    _ => assign(ArgKey::Pos(0), format!("(ref {name}) as the first input of {here}")),
                }
            }
        }
    }
}

/// The put: the document with the payload at the place and what was written, or the checker's
/// reason.
pub fn put(factories: Factories, catalog: &Catalog, doc: &Document, payload: &Payload, place: &Place) -> R<(Document, String)> {
    match plan(factories, catalog, doc, payload, place)? {
        Edit::Edits(on, ops, what) => {
            let edited = document_edit::syntax_batch(factories, &on, &ops)?;
            Ok((edited, what))
        }
        Edit::Rewrite(text, what) => {
            let edited = document_edit::text_edit(None, factories, doc, &text)
                .map_err(|ds| ds.first().map(|d| d.message.clone()).unwrap_or_else(|| "The text does not check".to_string()))?;
            Ok((edited, what))
        }
    }
}

/// The places a payload can be put, for the key route: the open graph, the scene, then each
/// geometry object, those whose put passes the checker, with the words of their label.
pub fn targets(factories: Factories, catalog: &Catalog, limit: Option<usize>, doc: &Document, payload: &Payload, graph: Option<&str>) -> Vec<(Place, String)> {
    let limit = limit.unwrap_or(9);
    let candidates: Vec<(Place, String)> = if payload.kind == "scene" || payload.kind == "camera" {
        viewports(doc).into_iter().enumerate().map(|(i, (key, _))| (Place::Viewport(key), format!("viewport {}", i + 1))).collect()
    } else {
        let mut out: Vec<(Place, String)> = Vec::new();
        if let Some(g) = graph.and_then(|g| find_graph(doc, g)) {
            if g.context == context::sop() && payload.kind == "material" {
                out.push((Place::Graph(g.name.clone()), "this graph".to_string()));
            } else if g.context == context::scene() && payload.kind == "sop" {
                out.push((Place::Graph(g.name.clone()), "the scene".to_string()));
            }
        }
        if let Some(g) = find_graph(doc, "scene") {
            if g.context == context::scene() && payload.kind == "sop" && graph != Some("scene") {
                out.push((Place::Graph("scene".to_string()), "the scene".to_string()));
            }
        }
        out.extend(geometry_objects(doc).into_iter().map(|(id, label)| (Place::Object(id), label)));
        out
    };
    let mut found = Vec::new();
    for (place, label) in candidates {
        if found.len() >= limit {
            break;
        }
        if put(factories, catalog, doc, payload, &place).is_ok() {
            found.push((place, label));
        }
    }
    found
}
