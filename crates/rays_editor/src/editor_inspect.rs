//! the levels of the model (kinds, objects, network,
//! displayed node), node ownership, new graphs and materials, and the workspace inspector (the
//! sheet of the node selected in the graph pane). Bundle C; Editor_model and Editor_shell come
//! from bundle D.

use flow_syntax::symbol::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use editor_document::document::{self as doc_mod};
use editor_document::kind_graph;
use editor_document::layout_edit as le;
use flow_check::catalog as fcat;
use flow_graph::flow_edit::{self as fe, ArgKey};
use flow_graph::probe::{self, Probe, Summary};
use flow_graph::projection as p;
use flow_sop::catalog as scat;
use flow_sop::port;
use flow_syntax::context;
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node as SNode, Syntax};
use flow_syntax::ty::{self, Ty};
use param::{FieldView, FloatRange, Impact, IntRange, KindView, Value};
use pxui::ui::{HeadAction, Ui, UiBox};
use pxui_graph::scope_state::Change as ScopeChange;
use pxui_shell::inspector as insp;
use sop::edit_graph;
use sop::node;

use crate::cook;
use crate::editor_model::{Change, flow_catalog, Preview, ScopeKey, EditorModel as Model, ModelView};
use crate::navigator;
use crate::text_pane;

type S = Syntax;
pub type Path = flow_check::workspace::Path;
pub use editor_document::document::Level;

fn sym(name: &str) -> S {
    s::make(s::Draft::Sym(name.to_owned()))
}
fn num(text: &str) -> S {
    s::make(s::Draft::Num(text.to_owned()))
}
fn string(text: &str) -> S {
    s::make(s::Draft::Str(text.as_bytes().to_vec()))
}
fn text_value(t: &str) -> Value {
    Value::Text(t.as_bytes().into())
}
fn text_of(v: &[u8]) -> String {
    String::from_utf8_lossy(v).into_owned()
}
fn float_of(t: &str) -> Option<f64> {
    t.parse::<f64>().ok()
}
fn int_of(t: &str) -> Option<i64> {
    t.parse::<i64>().ok()
}

// ---- levels ----

/// The scene's objects.
pub fn scene(value: &Model) -> &kind_graph::T {
    &value.doc.scene
}

/// Which objects a view draws: all, the primary view (those no named view lists), or one view.
#[derive(Clone, Debug, PartialEq)]
pub enum View {
    All,
    Primary,
    Only(String),
}

/// Membership is shared by drawing, lighting, picking and framing.
pub fn view_wants(value: &Model, view: &View, id: i64) -> bool {
    let empty = Vec::new();
    let views = match &value.doc.shell {
        Some(sh) => &sh.views,
        None => &empty,
    };
    match view {
        View::All => true,
        View::Primary => !views.iter().any(|(_, ids)| ids.contains(&id)),
        View::Only(key) => match views.iter().find(|(k, _)| k == key) {
            Some((_, ids)) => ids.contains(&id),
            None => false,
        },
    }
}

pub fn kind(value: &Model, id: i64) -> Option<String> {
    scene(value).iter().find(|n| n.id == id).map(|n| n.kind.clone())
}

pub fn level_key(level: Level) -> String {
    match level {
        Level::Scene => "scene".to_owned(),
        Level::Inside(id) => format!("object:{id}"),
    }
}

/// The kinds the node menu offers where the pane shows a graph of `context`: the object kinds of a
/// scene, the layers of a World, the SOP factories plus the library nodes this document does not
/// use yet (a workspace lowers only the library definitions it reaches).
pub fn kinds(value: &Model, ctx: &context::Context) -> Vec<scat::Descriptor> {
    let generated = |keep: &dyn Fn(&str) -> bool| -> Vec<scat::Descriptor> {
        editor_document::kinds::descriptors().into_iter().filter(|d| keep(&d.qualified)).collect()
    };
    if *ctx == context::scene() {
        generated(&|q| ["scene/geometry", "scene/light", "scene/camera"].contains(&q))
    } else if *ctx == context::world() {
        generated(&|q| q.starts_with("world/"))
    } else if *ctx == context::sop() {
        let mut all: Vec<_> = value.factories.iter().cloned().collect();
        all.extend(value.doc.workspace.lowered.factories.iter().cloned());
        scat::with_library(all.iter().map(scat::descriptor).collect())
    } else {
        Vec::new()
    }
}

/// The open level's objects or layers (the scene's if the level is gone: every path resolves the
/// level first); none inside a geometry object.
pub fn objects<'a>(value: &ModelView<'a>) -> std::borrow::Cow<'a, [kind_graph::Node]> {
    let doc: &'a std::sync::Arc<doc_mod::T> = value.doc;
    std::borrow::Cow::Borrowed(match crate::editor_model::pane_level(value) {
        Level::Scene => doc.scene.as_slice(),
        Level::Inside(id) => doc_mod::layers(doc, id).map_or(&[][..], |layers| layers.as_slice()),
    })
}

/// The SOP network of the open geometry object.
pub fn network<'a>(value: &ModelView<'a>) -> Option<&'a doc_mod::Network> {
    match crate::editor_model::pane_level(value) {
        Level::Inside(id) => doc_mod::network(&value.model.doc, id),
        Level::Scene => None,
    }
}

/// The open level lists a node of this id: an object, a layer, a node of a geometry network.
pub fn lists<'a>(value: &ModelView<'a>, id: i64) -> bool {
    match network(value) {
        Some(n) => edit_graph::find(&n.graph.geometry, id).is_some(),
        None => objects(value).iter().any(|n| n.id == id),
    }
}

/// The node the open level shows as its view: a geometry network's display node, the scene's first
/// object, the World's top layer.
pub fn displayed<'a>(value: &ModelView<'a>) -> Option<i64> {
    match (crate::editor_model::pane_level(value), network(value)) {
        (Level::Inside(_), Some(n)) => n.displayed,
        (Level::Scene, _) => objects(value).first().map(|n| n.id),
        (Level::Inside(_), None) => kind_graph::stack(&objects(value)).first().map(|n| n.id),
    }
}

pub fn viewed_path<'a>(value: &ModelView<'a>, scope: &p::Scope) -> Option<Path> {
    let valid = |pv: &Preview| -> Option<Path> {
        match (pv.path.first(), scope.path.first()) {
            (Some(graph), Some(root)) if graph == root => Some(pv.path.clone()),
            _ => None,
        }
    };
    let level = crate::editor_model::pane_level(value);
    let mut found = match level {
        Level::Inside(id) if doc_mod::object_graph(&value.doc, id).as_deref() == scope.path.first().map(|x| x.as_str()) => {
            value.previews.get(&id).and_then(&valid)
        }
        _ => value.previews.values().find_map(&valid),
    };
    if found.is_none() {
        if let Some(canvas) = &value.canvas_preview {
            found = valid(canvas);
        }
    }
    if found.is_some() {
        return found;
    }
    match &scope.result {
        p::ScopeResult::Link(target) => Some(vec![scope.path[0], target.clone().into()]),
        p::ScopeResult::Node(path) => Some(path.clone()),
        p::ScopeResult::Literal(_) => None,
    }
}

/// A named pane or Navigator can show a SOP graph while the scene list is open. Its compiled nodes
/// belong to an object's network, independent of the list's level.
pub fn node_owner_in(level: Level, networks: &doc_mod::IntMap<doc_mod::Network>, node_id: i64) -> Option<i64> {
    let owns = |n: &doc_mod::Network| edit_graph::find(&n.graph.geometry, node_id).is_some();
    if let Level::Inside(id) = level {
        if networks.get(&id).map_or(false, owns) {
            return Some(id);
        }
    }
    // ponytail: scan owners on this fallback; add an ownership index if large scenes need it.
    networks.iter().find_map(|(id, n)| if owns(n) { Some(*id) } else { None })
}

/// What a closure kept past this frame asks: the owner from the level and networks alone, never
/// the model.
pub fn node_owner_of<'a>(value: &ModelView<'a>) -> std::sync::Arc<dyn Fn(i64) -> Option<i64> + Send + Sync> {
    let level = crate::editor_model::pane_level(value);
    let networks = value.doc.networks.clone();
    std::sync::Arc::new(move |node_id| node_owner_in(level, &networks, node_id))
}

pub fn node_owner<'a>(value: &ModelView<'a>, node_id: i64) -> Option<i64> {
    node_owner_in(crate::editor_model::pane_level(value), &value.doc.networks, node_id)
}

pub fn compiled_node<'a>(value: &ModelView<'a>, node_id: i64) -> Option<node::T> {
    let owned = node_owner(value, node_id).and_then(|id| {
        let graph = match cook::applied(&value.cook, id) {
            Some(resolved) => resolved.geometry.clone(),
            None => value.doc.networks.get(&id).expect("owner").graph.geometry.clone(),
        };
        edit_graph::find(&graph, node_id)
    });
    if owned.is_some() {
        return owned;
    }
    value.doc.workspace.lowered.graphs.iter().find_map(|g| edit_graph::find(&g.network.geometry, node_id))
}

fn graph_taken(value: &Model, n: &str) -> bool {
    let ws = &value.doc.workspace.ws;
    ws.checked.graphs.iter().chain(ws.checked.defs.iter()).any(|g| g.name == n)
}

/// A new material graph: a free name and its Set_graph (`Material` in the add menu, "new material"
/// in the inspector).
pub fn new_material(value: &Model) -> (String, fe::Op) {
    let mut i = 1;
    let name = loop {
        let n = if i == 1 { "material".to_owned() } else { format!("material_{i}") };
        if !graph_taken(value, &n) {
            break n;
        }
        i += 1;
    };
    use s::Draft;
    let mut builder = s::SyntaxBuilder::new();
    let body: Vec<_> = [
        Draft::Sym("material/standard".into()), Draft::Kw("name".into()), Draft::Str(name.as_bytes().to_vec()),
        Draft::Kw("color".into()), Draft::Str(b"#cccccc".to_vec()), Draft::Kw("roughness".into()), Draft::Num("0.4".into()),
    ].into_iter().map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
    let body = builder.push_draft(0, Draft::List(body), Default::default(), None);
    let mut items: Vec<_> = [Draft::Sym("graph".into()), Draft::Sym(name.clone()), Draft::Kw("context".into()), Draft::Sym("material".into())]
        .into_iter().map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
    items.push(body);
    let id = builder.push_draft(0, Draft::List(items), Default::default(), None);
    let form = builder.freeze().handle(id);
    (name.clone(), fe::Op::SetGraph { name, form })
}

/// A new graph of a context, from the Navigator's context menu or the add menu: a fresh name (the
/// context's, then `_2` ...) and a body that checks and cooks. A scene reads the first SOP graph,
/// if any, and has a camera and a light, so it renders in a viewport.
pub fn new_graph(value: &Model, context: &str) -> (String, fe::Op) {
    let mut i = 1;
    let name = loop {
        let n = if i == 1 { context.to_owned() } else { format!("{context}_{i}") };
        if !graph_taken(value, &n) {
            break n;
        }
        i += 1;
    };
    use s::Draft;
    let mut builder = s::SyntaxBuilder::new();
    let body = match context {
        "value" => builder.push_draft(0, Draft::Num("1".into()), Default::default(), None),
        "sop" | "draw" | "image" | "material" => {
            let atoms = match context {
                "sop" => vec![Draft::Sym("sop/box".into())],
                "draw" => vec![Draft::Sym("draw/background".into()), Draft::Str(b"#111318".to_vec())],
                "image" => vec![Draft::Sym("image/noise".into()), Draft::Kw("width".into()), Draft::Num("256".into()), Draft::Kw("height".into()), Draft::Num("256".into()), Draft::Kw("frequency".into()), Draft::Num("0.03".into()), Draft::Kw("seed".into()), Draft::Num("1".into())],
                _ => vec![Draft::Sym("material/standard".into()), Draft::Kw("name".into()), Draft::Str(name.as_bytes().to_vec()), Draft::Kw("color".into()), Draft::Str(b"#cccccc".to_vec()), Draft::Kw("roughness".into()), Draft::Num("0.4".into())],
            };
            let items = atoms.into_iter().map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
            builder.push_draft(0, Draft::List(items), Default::default(), None)
        },
        _ => {
            let ws = &value.doc.workspace.ws;
            let mut bindings = Vec::new();
            let mut names = Vec::new();
            if let Some(g) = ws.checked.graphs.iter().find(|g| g.context == context::sop()) {
                let reference = [Draft::Sym("ref".into()), Draft::Sym(g.name.clone())].into_iter().map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
                let reference = builder.push_draft(0, Draft::List(reference), Default::default(), None);
                let head = builder.push_draft(0, Draft::Sym("scene/geometry".into()), Default::default(), None);
                let geometry = builder.push_draft(0, Draft::List(vec![head, reference]), Default::default(), None);
                let name = builder.push_draft(0, Draft::Sym("geometry".into()), Default::default(), None);
                bindings.extend([name, geometry]);
                names.push(name);
            }
            for (name, head, vectors, intensity) in [
                ("camera", "scene/camera", &[("eye", ["4", "5", "5"]), ("target", ["0", "0", "0"])][..], None),
                ("light", "scene/light", &[("translate", ["0", "5", "3"])][..], Some("50")),
            ] {
                let mut args = vec![builder.push_draft(0, Draft::Sym(head.into()), Default::default(), None)];
                for &(key, values) in vectors {
                    args.push(builder.push_draft(0, Draft::Kw(key.into()), Default::default(), None));
                    let values = values.into_iter().map(|text| builder.push_draft(0, Draft::Num(text.into()), Default::default(), None)).collect();
                    args.push(builder.push_draft(0, Draft::Vec(values), Default::default(), None));
                }
                if let Some(text) = intensity {
                    args.push(builder.push_draft(0, Draft::Kw("intensity".into()), Default::default(), None));
                    args.push(builder.push_draft(0, Draft::Num(text.into()), Default::default(), None));
                }
                let expr = builder.push_draft(0, Draft::List(args), Default::default(), None);
                let name = builder.push_draft(0, Draft::Sym(name.into()), Default::default(), None);
                bindings.extend([name, expr]);
                names.push(name);
            }
            let merge = builder.push_draft(0, Draft::Sym("scene/merge".into()), Default::default(), None);
            let mut merge_args = vec![merge];
            merge_args.extend(names);
            let merge = builder.push_draft(0, Draft::List(merge_args), Default::default(), None);
            let mut root: Vec<_> = [Draft::Sym("scene/root".into()), Draft::Kw("camera".into()), Draft::Sym("camera".into())].into_iter().map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
            root.insert(1, merge);
            let root = builder.push_draft(0, Draft::List(root), Default::default(), None);
            let head = builder.push_draft(0, Draft::Sym("let*".into()), Default::default(), None);
            let bindings = builder.push_draft(0, Draft::Vec(bindings), Default::default(), None);
            builder.push_draft(0, Draft::List(vec![head, bindings, root]), Default::default(), None)
        },
    };
    let mut items: Vec<_> = [Draft::Sym("graph".into()), Draft::Sym(name.clone()), Draft::Kw("context".into()), Draft::Sym(context.into())]
        .into_iter().map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
    items.push(body);
    let id = builder.push_draft(0, Draft::List(items), Default::default(), None);
    let form = builder.freeze().handle(id);
    (name.clone(), fe::Op::SetGraph { name, form })
}

/// "=(* 2 t)" typed in a row: the expression after the "=", any Lisp expression.
pub fn expression_text(text: &str) -> Result<S, String> {
    match s::parse(&text[1..]) {
        Ok(mut v) if v.len() == 1 => Ok(v.remove(0)),
        Ok(_) => Err("Write one expression after the =".to_owned()),
        Err(d) => Err(format!("Expression: {}", d.message)),
    }
}

/// 1204 as "1 204".
pub fn group_digits(n: i64) -> String {
    let text = n.unsigned_abs().to_string();
    let len = text.len();
    let mut b = String::with_capacity(len + 4);
    if n < 0 {
        b.push('-');
    }
    for (i, c) in text.chars().enumerate() {
        if i > 0 && (len - i) % 3 == 0 {
            b.push(' ');
        }
        b.push(c);
    }
    b
}

/// The section of a kind's arguments that have no folder: its own name ("sop/copy_to_points" is
/// Copy to points).
pub fn kind_label(head: &str) -> String {
    let leaf = match head.rfind('/') {
        Some(i) => &head[i + 1..],
        None => head,
    };
    let mut chars = leaf.chars().map(|c| if c == '_' { ' ' } else { c });
    match chars.next() {
        Some(first) => first.to_ascii_uppercase().to_string() + &chars.collect::<String>(),
        None => String::new(),
    }
}

fn plain_field(name: &str, label: &str, folder: Vec<String>, unit: Option<String>, vec3: Option<(String, i64)>, kind: KindView, default: Value, current: Value) -> FieldView {
    FieldView { name: name.to_owned(), label: label.to_owned(), description: None, folder, impact: Impact::Cook, primary: false, unit, vec3, kind, default, current }
}

/// The arguments of a kind the editor has no compiled node for (a scene or material node), as the
/// fields a node would give: the checked kind's parameters, valued by what is written in `authored`.
pub fn kind_fields(value: &Model, graph: &str, head: &str, authored: &dyn Fn(&str) -> Option<S>) -> Option<Vec<FieldView>> {
    let ws = &value.doc.workspace.ws;
    let ctx = ws.checked.graphs.iter().chain(ws.checked.defs.iter()).find(|g| g.name == graph).map(|g| g.context.clone());
    let declaration = ws.checked.nodes.iter().find(|n| n.kind.qualified == head);
    let number = |e: &S| -> Option<f64> {
        match &e.node {
            SNode::Num(t) => float_of(e.name(*t)),
            _ => None,
        }
    };
    if head == "material/standard" {
        // a built-in of the workspace, not a catalog kind: its four keywords
        let field = |name: &str, default: Value, view: KindView| -> FieldView {
            let current = match (authored(name), &default) {
                (Some(e), Value::Text(_)) if matches!(e.node, SNode::Str(_)) => match e.node { SNode::Str(t) => Value::Text(e.shared_bytes(t)), _ => unreachable!() },
                (Some(e), Value::Float(_)) => match number(&e) {
                    Some(f) => Value::Float(f),
                    None => default.clone(),
                },
                _ => default.clone(),
            };
            plain_field(name, name, vec![], None, None, view, default, current)
        };
        return Some(vec![
            field("name", text_value(""), KindView::Text),
            field("color", text_value("#cccccc"), KindView::Text),
            field("roughness", Value::Float(0.5), KindView::Floating(FloatRange { soft_min: 0.0, soft_max: 1.0, hard_min: Some(0.0), hard_max: Some(1.0) })),
            field("emission", text_value("#000000"), KindView::Text),
        ]);
    }
    let catalog = flow_catalog(value)?;
    let context = ctx?;
    let kind: fcat::Kind = match declaration {
        Some(n) if n.kind.context == context => n.kind.clone(),
        _ => fcat::resolve_kind(&catalog, &context, head).ok()?,
    };
    let mut out = Vec::new();
    for pm in &kind.parameters {
        let written = authored(&pm.name);
        let three = pm.fields.len() == 3 && pm.ty == Some(flow_check::port_type::PortType::Vec3);
        for (i, (name, view, default)) in pm.fields.iter().enumerate() {
            let current = match (&written, default) {
                (Some(e), Value::Float(_)) if three && matches!(e.node, SNode::Vec(v) if e.range(v).len() == 3) => match number(&e.child(e.children()[i])) {
                    Some(f) => Value::Float(f),
                    None => default.clone(),
                },
                (Some(e), Value::Float(_)) if !three => match number(e) {
                    Some(f) => Value::Float(f),
                    None => default.clone(),
                },
                (Some(e), Value::Int(_)) if matches!(e.node, SNode::Num(_)) => match e.node {
                    SNode::Num(t) => int_of(e.name(t)).map(Value::Int).unwrap_or_else(|| default.clone()),
                    _ => unreachable!(),
                },
                (Some(e), Value::Choice(_)) if matches!(e.node, SNode::Str(_)) => match e.node {
                    SNode::Str(t) => Value::Choice(text_of(e.bytes(t))),
                    _ => unreachable!(),
                },
                (Some(e), Value::Text(_)) if matches!(e.node, SNode::Str(_)) => match e.node {
                    SNode::Str(t) => Value::Text(e.shared_bytes(t)),
                    _ => unreachable!(),
                },
                (Some(e), Value::Bool(_)) if matches!(e.node, SNode::Sym(b) if matches!(e.name(b), "true" | "false")) => match e.node {
                    SNode::Sym(b) => Value::Bool(e.name(b) == "true"),
                    _ => unreachable!(),
                },
                _ => default.clone(),
            };
            let field = FieldView {
                name: name.clone(),
                label: if three { name.clone() } else { pm.label.clone() },
                description: None,
                folder: pm.folder.clone(),
                impact: Impact::Cook,
                primary: false,
                unit: if three { None } else { pm.unit.clone() },
                vec3: if three { Some((pm.name.clone(), i as i64)) } else { None },
                kind: view.clone(),
                default: default.clone(),
                current: current.clone(),
            };
            out.push(match declaration.and_then(|n| n.fields.iter().find(|f| f.name == *name)) {
                Some(f) => FieldView { current, ..f.clone() },
                None => field,
            });
        }
    }
    Some(out)
}

/// The iterations the zones around a node of the pane's graph probe, outermost first.
/// `chains` is `probe::chains` of the scope, made once by the caller (it walks the whole scope).
pub fn probes_of(value: &Model, chains: &HashMap<Path, Vec<Path>>, path: &[Symbol]) -> Vec<i64> {
    chains
        .get(path)
        .map(|zones| zones.iter().map(|zone| value.probes.get(zone).copied().unwrap_or(0)).collect())
        .unwrap_or_default()
}

/// The compiled node a node of the pane's graph lowers to at its probes.
pub fn compiled_at(value: &Model, chains: &HashMap<Path, Vec<Path>>, records: &Rc<RefCell<Probe>>, path: &[Symbol]) -> Option<i64> {
    let probes = probes_of(value, chains, path);
    let plan = records.borrow().plan_node(None, path, &probes)?;
    value.doc.workspace.lowered.compiled.get(&plan).copied()
}

/// A nested operator call in a wired row (the call `* 0.2 (sin ...)` in row b): one sub-row per
/// leaf of its tree, addressed by `Set_arg`'s `sub` (child indices; the head is child 0). Depth 4,
/// 12 rows.
pub const ARG_PREFIX: &str = "@arg:";

pub fn operator_call(e: &S) -> bool {
    e.children().len() >= 2 && s::head(e).is_some_and(|head| !head.contains('/'))
}

/// An entry of the inspector: the row's base name, the projection row, its sub-path and its sheet row.
pub type Entry = (String, p::Row, Vec<i64>, insp::FlowRow);

fn num_range(f: f64) -> FloatRange {
    let m = 1.0f64.max(f.abs() * 2.0);
    FloatRange { soft_min: -m, soft_max: m, hard_min: None, hard_max: None }
}

fn int_range(i: i64) -> IntRange {
    IntRange { soft_min: 0.min(2 * i), soft_max: 10.max(2 * i), hard_min: None, hard_max: None }
}

fn all_numbers(form: &S, l: &[s::SyntaxId]) -> bool {
    l.iter().all(|&id| matches!(form.arena().row(id).node, SNode::Num(t) if float_of(form.name(t)).is_some()))
}

const VEC_SUFFIXES: [&str; 4] = [".x", ".y", ".z", ".w"];

/// Literals in operator rows and nested operator args share one field builder (vectors 2–4 wide, all numeric).
fn literal_fields(
    syntax: &S,
    base: &str,
    row_label: &str,
    ty: Option<&Ty>,
    float_range: &dyn Fn(f64) -> FloatRange,
) -> Option<(Vec<FieldView>, Option<String>)> {
    let field = |vec3: Option<(String, i64)>, suffix: &str, kind: KindView, current: Value| -> FieldView {
        FieldView {
            name: format!("{base}{suffix}"),
            label: if vec3.is_none() { row_label.to_owned() } else { suffix.to_owned() },
            description: None,
            folder: vec![],
            impact: Impact::Cook,
            primary: false,
            unit: None,
            vec3,
            kind,
            default: current.clone(),
            current,
        }
    };
    match &syntax.node {
        SNode::Num(t) => {
            let name = syntax.name(*t);
            if matches!(ty, Some(Ty::Int)) {
                if let Some(i) = int_of(name) {
                    return Some((vec![field(None, "", KindView::Integer(int_range(i)), Value::Int(i))], None));
                }
            } else if ty.is_none() {
                if let Some(i) = int_of(name) {
                    return Some((vec![field(None, "", KindView::Integer(int_range(i)), Value::Int(i))], None));
                }
            }
            float_of(name).map(|f| (vec![field(None, "", KindView::Floating(float_range(f)), Value::Float(f))], None))
        }
        SNode::Str(t) => Some((vec![field(None, "", KindView::Text, Value::Text(syntax.shared_bytes(*t)))], None)),
        SNode::Sym(b) if matches!(syntax.name(*b), "true" | "false") => Some((
            vec![field(
                None,
                "",
                KindView::Toggle,
                Value::Bool(syntax.name(*b) == "true"),
            )],
            None,
        )),
        SNode::Vec(l) if (2..=4).contains(&syntax.range(*l).len()) && all_numbers(syntax, syntax.range(*l)) => {
            let range = syntax.range(*l);
            let fields = range
                .iter()
                .enumerate()
                .map(|(i, &id)| {
                    let f = match syntax.arena().row(id).node {
                        SNode::Num(t) => float_of(syntax.name(t)).unwrap_or(0.0),
                        _ => 0.0,
                    };
                    field(
                        Some((base.to_owned(), i as i64)),
                        VEC_SUFFIXES[i],
                        KindView::Floating(float_range(f)),
                        Value::Float(f),
                    )
                })
                .collect();
            Some((fields, None))
        }
        SNode::Sym(sy) => Some((vec![field(None, "", KindView::Text, text_value(""))], Some(syntax.name(*sy).to_owned()))),
        _ => Some((vec![field(None, "", KindView::Text, text_value(""))], Some(format!("={}", lisp::flat(syntax))))),
    }
}

#[allow(clippy::too_many_arguments)]
fn walk_sub(row: &p::Row, shown: bool, chain: &[Symbol], depth: i64, sub: &[i64], e: &S, count: &mut i64, out: &mut Vec<Entry>) {
    if depth >= 4 { return; }
    let Some(h) = s::head(e).map(str::to_owned) else { return; };
    for (i, &id) in e.children().iter().enumerate() {
        let c = e.child(id);
        if i == 0 {
            continue;
        }
        let mut sub2 = sub.to_vec();
        sub2.push(i as i64);
        if operator_call(&c) {
            let mut chain2 = chain.to_vec();
            chain2.push(h.clone().into());
            walk_sub(row, shown, &chain2, depth + 1, &sub2, &c, count, out);
            continue;
        }
        if *count >= 12 {
            continue;
        }
        *count += 1;
        let label = format!("{}.{}", row.label, sub2.iter().map(|k| (k - 1).to_string()).collect::<Vec<_>>().join("."));
        let base = format!("{}{}@{}", ARG_PREFIX, row.label, sub2.iter().map(|k| k.to_string()).collect::<Vec<_>>().join("."));
        if let Some((fields, drive)) = literal_fields(&c, &base, &label, None, &num_range) {
            out.push((
                base.clone(),
                row.clone(),
                sub2.clone(),
                insp::FlowRow { path: base.clone(), fields, shown, locked: false, drive, live: None },
            ));
        }
    }
}

pub fn sub_entries(shown: bool, r: &p::Row) -> Vec<Entry> {
    match &r.expr {
        Some(e) if operator_call(e) => {
            let mut out = Vec::new();
            let mut count = 0;
            walk_sub(r, shown, &[], 0, &[], e, &mut count, &mut out);
            out
        }
        _ => Vec::new(),
    }
}

/// The inspector's subject among the pane's selected paths: a card selected together with one of
/// its rows (the pane selects both on a click on an expression chip) is the row alone.
pub fn inspector_paths(paths: Vec<Path>) -> Vec<Path> {
    let holder = |pth: &Path| -> Option<Path> {
        match pth.split_last() {
            Some((last, rest)) if !rest.is_empty() && last.starts_with(':') => Some(rest.to_vec()),
            _ => None,
        }
    };
    if let [a, b] = paths.as_slice() {
        if holder(b).as_ref() == Some(a) {
            return vec![b.clone()];
        }
        if holder(a).as_ref() == Some(b) {
            return vec![a.clone()];
        }
    }
    paths
}

/// The row and sub-path of a field named `name` among `entries` (a vector's cell is its row's `.x` ...).
pub fn arg_row_in(entries: &[Entry], name: &str) -> Option<(p::Row, Vec<i64>)> {
    let mut base = name.to_owned();
    for suffix in [".x", ".y", ".z", ".w"] {
        if base.ends_with(suffix) {
            base.truncate(base.len() - 2);
        }
    }
    entries.iter().find_map(|(b, r, sub, _)| if *b == base { Some((r.clone(), sub.clone())) } else { None })
}

/// The request of an edit of a sub-row or of an operator row.
pub fn follow_row(source: &[S], n: &p::Node, r: &p::Row) -> Option<Change> {
    p::sources(r).into_iter().find_map(|name| text_pane::reference_target(source, &n.path, &name).map(Change::FollowSource))
}

pub fn arg_request(source: &[S], n: &p::Node, arg_row: &dyn Fn(&str) -> Option<(p::Row, Vec<i64>)>, ev: &insp::FlowChange) -> Option<Change> {
    match ev {
        insp::FlowChange::Follow(path) => arg_row(path).and_then(|(r, sub)| {
            let mut expr = r.expr.clone();
            for i in &sub {
                expr = expr.and_then(|e| e.children().get(*i as usize).map(|&id| e.child(id)));
            }
            let chip = if sub.is_empty() { r.chip.clone() } else { p::Chip::Const };
            follow_row(source, n, &p::Row { expr, chip, ..r })
        }),
        insp::FlowChange::Edited(name, edited) => arg_row(name).and_then(|(r, sub)| {
            let component = if name.ends_with(".x") {
                Some(0)
            } else if name.ends_with(".y") {
                Some(1)
            } else if name.ends_with(".z") {
                Some(2)
            } else {
                None
            };
            let syntax = match edited {
                Value::Float(f) => Some(num(&lisp::float(*f))),
                Value::Int(i) => Some(num(&i.to_string())),
                Value::Bool(b) => Some(sym(if *b { "true" } else { "false" })),
                Value::Text(t) => Some(s::make(s::Draft::Str(t.to_vec()))),
                Value::Choice(t) => Some(string(t)),
            };
            syntax.map(|value| {
                let mut sub2 = sub.clone();
                if let Some(i) = component {
                    sub2.push(i);
                }
                Change::SyntaxEdit(fe::Op::SetArg { node: n.path.clone(), key: r.key.clone(), sub: sub2, value })
            })
        }),
        insp::FlowChange::Expression(path, text) => arg_row(path).map(|(r, sub)| match expression_text(text) {
            Ok(value) => Change::SyntaxEdit(fe::Op::SetArg { node: n.path.clone(), key: r.key.clone(), sub, value }),
            Err(message) => Change::Declined(message),
        }),
        insp::FlowChange::Reset(path) => arg_row(path).and_then(|(r, sub)| {
            // the x on a row that reads a name (a wire, a context symbol such as `t`) writes what
            // the wire's own x writes in the graph: the schema default, else a value of the row's type
            if !sub.is_empty() {
                return None;
            }
            let fallback = if r.kind == p::RowKind::Rest {
                None
            } else if matches!(r.key, ArgKey::Kw(_) | ArgKey::Field(_)) {
                r.default.as_ref().and_then(|d| match s::parse(d) {
                    Ok(mut v) if v.len() == 1 => Some(v.remove(0)),
                    _ => None,
                })
            } else {
                r.ty.as_ref().and_then(|ty| fe::default_for(ty, &r.label, None))
            };
            Some(Change::SyntaxEdit(fe::Op::Disconnect { node: n.path.clone(), key: r.key.clone(), fallback }))
        }),
        _ => None,
    }
}

/// The geometry source of the node selected in the graph pane (the spreadsheet's subject).
pub fn spreadsheet_source<'a>(value: &ModelView<'a>) -> Option<&'a rdk_rays::geometry_view::T> {
    let laid = crate::editor_model::pane_laid(value)?;
    let records = laid.records.as_ref()?;
    let path = pxui_graph::scope_state::selected(&crate::editor_model::pane_view(value)).into_iter().next()?;
    let chains = probe::chains(&laid.scope);
    let node_id = compiled_at(value, &chains, records, &path)?;
    let object_id = node_owner(value, node_id)?;
    cook::source(&value.model.cook, object_id, node_id)
}

/// The iterations of the loop the node sits in, one button each.
pub fn iteration_picks(ui: &mut Ui, footer: &probe::Footer, chain: &[Path], probes: &[i64], n: &p::Node, records: &Rc<RefCell<Probe>>) -> Vec<ScopeChange> {
    let iterations = records.borrow_mut().iterations(n, probes);
    let zone = chain.last();
    match zone {
        Some(z) if !iterations.is_empty() => {
            ui.inspector_message(
                "ws-runs",
                &format!(
                    "Inside {}: runs {} times. {}",
                    z.last().map_or("", |name| name.as_str()),
                    footer.runs.unwrap_or(iterations.len() as i64),
                    if n.invariant { "The same every time, so it can move out of the loop." } else { "It changes with the loop variable." }
                ),
            );
            let current = probes.last().copied().unwrap_or(0);
            let mut picks = Vec::new();
            for k in 0..iterations.len().min(64) {
                let label = format!("{}{}  {}", if k as i64 == current { "\u{25ba} " } else { "  " }, k + 1, iterations[k]);
                if ui.inspector_button(&format!("ws-iter-{k}"), &label) {
                    picks.push(ScopeChange::ProbeSet { zone: z.clone(), index: k as i64 });
                }
            }
            picks
        }
        _ => Vec::new(),
    }
}

/// The note above the binding in the Lisp: one line here, typing is one history entry; the last
/// section, closed.
pub fn note_section(ui: &mut Ui, width: f64, n: &p::Node) -> Vec<Change> {
    if n.synthetic || p::anonymous(n) {
        return Vec::new();
    }
    let current = n.note.clone().unwrap_or_default();
    let mut out: Vec<Change> = Vec::new();
    ui.inspector_section("ws-note-section", false, None, "Note", &mut |ui| {
        if current.contains('\n') {
            ui.inspector_message("ws-note-lines", "A note of several lines is edited in the text pane.");
            return;
        }
        let (row, cx, cy, cw) = ui.inspector_row(Some(width), None, "ws-note", "note");
        let mut text = current.clone();
        ui.within(&row, &mut |ui| {
            let o = pxui::ui::ValueFieldOpts { left: true, ..Default::default() };
            text = ui.value_field((cx, cy), cw, 20.0, &o, &|_| true, "ws-note-field", &current).0;
        });
        if text != current {
            out.push(Change::SyntaxEdit(fe::Op::SetNote { node: n.path.clone(), text }));
        }
    });
    out
}

/// What the cook made of a geometry node: read-out rows, as the sheet's Output section.
pub fn output_section(ui: &mut Ui, width: f64, geo: Option<&probe::Geometry>) {
    if let Some(g) = geo {
        ui.inspector_section("ws-output", true, None, "Output", &mut |ui| {
            ui.inspector_readout(Some(width), "ws-output-points", "points", &group_digits(g.points));
            ui.inspector_readout(Some(width), "ws-output-prims", "prims", &group_digits(g.prims));
            if let Some((x, y, z)) = g.extent {
                ui.inspector_readout(
                    Some(width),
                    "ws-output-bounds",
                    "bounds",
                    &format!("{} \u{d7} {} \u{d7} {}", cfmt::f(x, 1), cfmt::f(y, 1), cfmt::f(z, 1)),
                );
            }
            if !g.groups.is_empty() {
                ui.inspector_readout(Some(width), "ws-output-groups", "groups", &g.groups.join(", "));
            }
            for (i, (owner, name, kind, length)) in g.attributes.iter().enumerate() {
                ui.inspector_readout(
                    Some(width),
                    &format!("ws-output-attr-{i}"),
                    &format!("{owner} {name}"),
                    &format!("{kind} \u{b7} {}", group_digits(*length)),
                );
            }
        });
    }
}

fn syntax_of_value(v: &Value) -> S {
    match v {
        Value::Float(f) => num(&lisp::float(*f)),
        Value::Int(i) => num(&i.to_string()),
        Value::Bool(b) => sym(if *b { "true" } else { "false" }),
        Value::Text(t) => s::make(s::Draft::Str(t.to_vec())),
        Value::Choice(t) => string(t),
    }
}

fn ref_form(name: &str) -> S {
    let mut builder = s::SyntaxBuilder::new();
    let items = [s::Draft::Sym("ref".into()), s::Draft::Sym(name.into())].into_iter()
        .map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
    let id = builder.push_draft(0, s::Draft::List(items), Default::default(), None);
    builder.freeze().handle(id)
}

fn is_literal(e: &S) -> bool {
    match &e.node {
        SNode::Num(_) | SNode::Str(_) => true,
        SNode::Sym(b) => matches!(e.name(*b), "true" | "false"),
        SNode::Vec(l) => e.range(*l).iter().all(|&id| matches!(e.arena().row(id).node, SNode::Num(_))),
        _ => false,
    }
}

/// The layout switch of a node, when it is one: the names of its layouts and the active index.
pub type LayoutNames = Option<(Vec<String>, i64)>;

/// What a drag of a splitter gives: the size to write.
pub type Resized<'a> = &'a dyn Fn(&[Symbol], pxui_shell::layout::Way) -> Option<le::Size>;

/// The intents the rows' widgets returned, as document changes.
#[allow(clippy::too_many_arguments)]
pub fn field_edits<'a>(
    ui: &mut Ui,
    on_choice: Option<&dyn Fn(&str, &UiBox)>,
    width: f64,
    expanded: &[String],
    parameters: &[port::Parameter],
    ref_rows: &[(p::Row, insp::FlowRow)],
    row_of: &dyn Fn(&str) -> Option<p::Row>,
    arg_row: &dyn Fn(&str) -> Option<(p::Row, Vec<i64>)>,
    value: &ModelView<'a>,
    size_names: &[&str],
    n: &p::Node,
    resized: Resized,
    graph: &str,
    layout_names: &LayoutNames,
    rows: &[insp::FlowRow],
) -> Vec<Change> {
    if rows.is_empty() {
        return Vec::new();
    }
    let chips = match crate::editor_model::pane_laid(value) {
        Some(k) => match &k.evaluated {
            Some(ev) => navigator::chips(ev),
            None => Vec::new(),
        },
        None => Vec::new(),
    };
    let opts = insp::FlowOpts { expanded: expanded.to_vec(), width: Some(width), pins: true, pin_click: true, chips, kind_label: Some(kind_label(&n.head)) };
    let events = insp::flow_fields(ui, &opts, on_choice, rows);
    let ws = &value.doc.workspace.ws;
    let source: &[S] = &ws.source;
    let mut out = Vec::new();
    for ev in events {
        let name_of = |ev: &insp::FlowChange| -> String {
            match ev {
                insp::FlowChange::Edited(n, _) | insp::FlowChange::Expression(n, _) | insp::FlowChange::Follow(n) | insp::FlowChange::Reset(n) | insp::FlowChange::Pinned(n, _) => n.clone(),
            }
        };
        let name = name_of(&ev);
        // the layout switch and the split size
        if let insp::FlowChange::Edited(nm, Value::Choice(chosen)) = &ev {
            if nm == "@layout" {
                if let Some((names, _)) = layout_names {
                    if let Some(index) = names.iter().position(|x| x == chosen) {
                        out.push(Change::LayoutEdit(le::Op::SetLayout { graph: graph.to_owned(), index: index as i64 }));
                    }
                }
                continue;
            }
            if nm == "@size" {
                if let Some(index) = size_names.iter().position(|x| x == chosen) {
                    let how = [pxui_shell::layout::Way::Ratio, pxui_shell::layout::Way::First, pxui_shell::layout::Way::Second][index];
                    let size = resized(&n.path, how).unwrap_or(match how {
                        pxui_shell::layout::Way::Ratio => le::Size::Ratio(0.5),
                        pxui_shell::layout::Way::First => le::Size::First(240),
                        pxui_shell::layout::Way::Second => le::Size::Second(240),
                    });
                    out.push(Change::LayoutEdit(le::Op::SetLayoutSize { node: n.path.clone(), size }));
                }
                continue;
            }
        }
        if name.starts_with(ARG_PREFIX) {
            if let Some(c) = arg_request(source, n, arg_row, &ev) {
                out.push(c);
            }
            continue;
        }
        match &ev {
            insp::FlowChange::Follow(path) => {
                let row = row_of(path).or_else(|| {
                    [".x", ".y", ".z", ".w"].iter().find_map(|suffix| if path.ends_with(suffix) { row_of(&path[..path.len() - 2]) } else { None })
                });
                if let Some(c) = row.and_then(|r| follow_row(source, n, &r)) {
                    out.push(c);
                }
            }
            insp::FlowChange::Edited(name, edited) => {
                if let Some((r, _)) = ref_rows.iter().find(|(r, _)| format!("@ref:{}", r.label) == *name) {
                    match edited {
                        Value::Choice(c) if c == "new material" => {
                            let (mname, make) = new_material(value);
                            out.push(Change::SyntaxBatch(
                                "New material".to_owned(),
                                vec![make, fe::Op::SetArg { node: n.path.clone(), key: r.key.clone(), sub: vec![], value: ref_form(&mname) }],
                            ));
                        }
                        Value::Choice(c) if c != "(none)" && !c.is_empty() => {
                            out.push(Change::SyntaxEdit(fe::Op::SetArg { node: n.path.clone(), key: r.key.clone(), sub: vec![], value: ref_form(c) }));
                        }
                        _ => {}
                    }
                } else {
                    for parameter in parameters {
                        let Some(index) = parameter.fields.iter().position(|f| f.name == *name) else { continue };
                        let syntax = syntax_of_value(edited);
                        let syntax = if parameter.fields.len() != 3 {
                            syntax
                        } else {
                            let mut builder = s::SyntaxBuilder::new();
                            let items = parameter.fields.iter().enumerate().map(|(i, f)| {
                                if i == index {
                                    builder.import(&syntax)
                                } else {
                                    let text = match &f.current {
                                        Value::Float(x) => lisp::float(*x),
                                        Value::Int(x) => lisp::float(*x as f64),
                                        _ => "0.0".to_owned(),
                                    };
                                    builder.push_draft(0, s::Draft::Num(text), Default::default(), None)
                                }
                            }).collect();
                            let id = builder.push_draft(0, s::Draft::Vec(items), Default::default(), None);
                            builder.freeze().handle(id)
                        };
                        out.push(Change::SyntaxEdit(fe::Op::SetArg { node: n.path.clone(), key: ArgKey::Kw(parameter.path.clone()), sub: vec![], value: syntax }));
                        break;
                    }
                }
            }
            insp::FlowChange::Pinned(path, want) => {
                if let Some(r) = row_of(path) {
                    out.push(Change::PinRow { node: n.path.clone(), label: r.label.clone(), pin: if *want == p::row_shown(None, &r) { None } else { Some(*want) } });
                }
            }
            insp::FlowChange::Expression(path, text) => {
                let (key, sub) = match path.find('.') {
                    Some(i) => {
                        let axis = &path[i + 1..];
                        (path[..i].to_owned(), ["x", "y", "z"].iter().position(|a| a == &axis).map(|k| vec![k as i64]).unwrap_or_default())
                    }
                    None => (path.clone(), vec![]),
                };
                match expression_text(text) {
                    Ok(v) => out.push(Change::SyntaxEdit(fe::Op::SetArg { node: n.path.clone(), key: ArgKey::Kw(key), sub, value: v })),
                    Err(message) => out.push(Change::Declined(message)),
                }
            }
            insp::FlowChange::Reset(path) => {
                if let Some((r, _)) = ref_rows.iter().find(|(r, _)| format!("@ref:{}", r.label) == *path) {
                    out.push(Change::SyntaxEdit(fe::Op::SetArg { node: n.path.clone(), key: r.key.clone(), sub: vec![], value: sym("nil") }));
                } else {
                    let key = path.split('.').next().unwrap_or("").to_owned();
                    out.push(Change::SyntaxEdit(fe::Op::Disconnect { node: n.path.clone(), key: ArgKey::Kw(key), fallback: None }));
                }
            }
        }
    }
    out
}

fn sheet_row(path: &str, fields: Vec<FieldView>, shown: bool, drive: Option<String>, live: Option<String>) -> insp::FlowRow {
    insp::FlowRow { path: path.to_owned(), fields, shown, locked: false, drive, live }
}

/// A value operator's inputs as rows: a value operator (+, *, sin ...) or a nested call has no
/// catalog parameters: its inputs are the card's rows, so the inspector shows those. A literal is a
/// field, a wire or an expression a readout that typing replaces; edits are `SetArg` on the row's key.
pub fn operator_rows(n: &p::Node, pin_of: &dyn Fn(&p::Row) -> Option<bool>, size_row: bool, layout_row: bool, ref_rows: bool, parameters: bool) -> Vec<(p::Row, insp::FlowRow)> {
    if parameters || ref_rows || layout_row || size_row {
        return Vec::new();
    }
    let mut out = Vec::new();
    for r in &n.rows {
        let e = match (&r.kind, &r.expr) {
            (p::RowKind::Arg | p::RowKind::Rest | p::RowKind::Hole, Some(e)) if !r.head => e,
            _ => continue,
        };
        let name = format!("{ARG_PREFIX}{}", r.label);
        let float_range = |f: f64| -> FloatRange {
            match r.control {
                p::Control::Range(lo, hi) => FloatRange { soft_min: lo, soft_max: hi, hard_min: None, hard_max: None },
                _ => num_range(f),
            }
        };
        let shown = p::row_shown(pin_of(r), r);
        let row = |fields: Vec<FieldView>, drive: Option<String>| (r.clone(), sheet_row(&name, fields, shown, drive, None));
        let wired = || {
            row(
                vec![FieldView {
                    name: name.clone(),
                    label: r.label.clone(),
                    description: None,
                    folder: vec![],
                    impact: Impact::Cook,
                    primary: false,
                    unit: None,
                    vec3: None,
                    kind: KindView::Text,
                    default: text_value(""),
                    current: text_value(""),
                }],
                Some(match &e.node {
                    SNode::Sym(sy) => e.name(*sy).to_owned(),
                    _ => format!("={}", lisp::flat(e)),
                }),
            )
        };
        let made = literal_fields(e, &name, &r.label, r.ty.as_ref(), &float_range)
            .map(|(fields, drive)| row(fields, drive))
            .unwrap_or_else(wired);
        out.push(made);
    }
    out
}

/// A switch's layouts and a split's size, as choice rows.
pub fn layout_and_size_rows(value: &Model, n: &p::Node) -> (LayoutNames, Option<insp::FlowRow>, [&'static str; 3], Option<insp::FlowRow>) {
    // a switch's :active is a choice among its layouts, named from their panels
    let layout_names: LayoutNames = match (n.head.as_str(), &value.doc.shell) {
        ("ui/switch", Some(sh)) => match &sh.switch {
            Some(sw) if sw.layouts.len() == n.rows.iter().filter(|r| matches!(r.key, ArgKey::Pos(_)) && r.kind != p::RowKind::Add).count() => {
                Some((editor_core::panels::labels(&sw.layouts), sw.active))
            }
            _ => None,
        },
        _ => None,
    };
    let choice_field = |name: &str, label: &str, description: &str, names: Vec<String>, current: usize| FieldView {
        name: name.to_owned(),
        label: label.to_owned(),
        description: Some(description.to_owned()),
        folder: vec![],
        impact: Impact::View,
        primary: true,
        unit: None,
        vec3: None,
        default: Value::Choice(names[0].clone()),
        current: Value::Choice(names[current].clone()),
        kind: KindView::Choice(names),
    };
    let layout_row = layout_names.as_ref().map(|(names, active)| {
        sheet_row("@layout", vec![choice_field("@layout", "Layout", "The layout shown", names.clone(), *active as usize)], true, None, None)
    });
    // a split is sized by ratio or with one side fixed: the choice converts it, keeping the sizes
    // it shows (flow.md 11.11)
    let size_names = ["By ratio", "First side fixed", "Second side fixed"];
    let size_row = if n.head != "ui/split" && n.head != "ui/split_at" {
        None
    } else {
        let given = |key: &str| n.rows.iter().any(|r| r.key == ArgKey::Kw(key.to_owned()) && r.expr.is_some());
        let now = if given("first_size") {
            1
        } else if given("second_size") {
            2
        } else {
            0
        };
        Some(sheet_row("@size", vec![choice_field("@size", "Size", "What the split is sized by", size_names.iter().map(|x| x.to_string()).collect(), now)], true, None, None))
    };
    (layout_names, layout_row, size_names, size_row)
}

fn ref_target(e: &Option<S>) -> Option<String> {
    let form = e.as_ref()?;
    if s::head(form) != Some("ref") { return None; }
    match form.children() {
        [_, id] => match form.arena().row(*id).node {
            SNode::Sym(name) => Some(form.name(name).to_owned()),
            _ => None,
        },
        _ => None,
    }
}

/// The rows that name a graph: a choice among the graphs.
pub fn reference_rows<'a>(value: &ModelView<'a>, n: &p::Node) -> Vec<(p::Row, insp::FlowRow)> {
    let is_ref_slot = |r: &p::Row| -> bool {
        ref_target(&r.expr).is_some() || r.ty == Some(ty::material()) || (r.ty == Some(ty::geometry()) && n.head.starts_with("scene/"))
    };
    let doc = &value.doc.workspace.ws;
    let sop_graphs: Vec<String> = doc.checked.graphs.iter().filter(|g| g.context == context::sop()).map(|g| g.name.clone()).collect();
    let material_graphs: Vec<String> = doc.checked.graphs.iter().filter(|g| g.context == context::material()).map(|g| g.name.clone()).collect();
    let mut out = Vec::new();
    for r in &n.rows {
        if !is_ref_slot(r) {
            continue;
        }
        let is_material = r.ty == Some(ty::material());
        let target = ref_target(&r.expr).unwrap_or_default();
        let gs = if is_material { &material_graphs } else { &sop_graphs };
        let choices: Vec<String> = if gs.is_empty() && !is_material {
            vec!["(none)".to_owned()]
        } else {
            let mut l = gs.clone();
            if !target.is_empty() && !gs.contains(&target) {
                l.insert(0, target.clone());
            }
            if is_material {
                l.push("new material".to_owned());
            }
            l
        };
        let field_name = format!("@ref:{}", r.label);
        let current = if !target.is_empty() { target.clone() } else { choices[0].clone() };
        let label = {
            let mut c = r.label.chars();
            match c.next() {
                Some(f) => f.to_ascii_uppercase().to_string() + c.as_str(),
                None => String::new(),
            }
        };
        let field = FieldView {
            name: field_name.clone(),
            label,
            description: Some(if is_material { "Material graph reference" } else { "SOP graph reference" }.to_owned()),
            folder: vec![],
            impact: Impact::Cook,
            primary: false,
            unit: None,
            vec3: None,
            default: Value::Choice(choices[0].clone()),
            current: Value::Choice(current),
            kind: KindView::Choice(choices),
        };
        out.push((r.clone(), sheet_row(&field_name, vec![field], true, None, None)));
    }
    out
}

/// The readouts a node shows under its head.
pub fn readout_rows(ui: &mut Ui, n: &p::Node, value: &Model, footer: &probe::Footer, readouts: &[(String, String)], width: f64, zone_text: bool) {
    // the head's detail line says it for a node; a zone keeps its readout rows
    if zone_text {
        for (label, text) in readouts {
            ui.inspector_readout(Some(width), &format!("ws-{label}"), label, text);
        }
    }
    if !zone_text && footer.execution.as_ref().map_or(false, |e| e.tier != "Cooked") {
        for (label, text) in readouts.iter().filter(|(l, _)| ["tier", "execution group", "group time"].contains(&l.as_str())) {
            ui.inspector_readout(Some(width), &format!("ws-{label}"), label, text);
        }
    }
    let checked = &value.doc.workspace.ws.checked;
    if checked.approx.contains(&n.path) {
        ui.inspector_readout(Some(width), "ws-precision", "precision", "approximable");
    } else if let Some((_, reasons)) = checked.approx_reasons.iter().find(|(p_, _)| *p_ == n.path) {
        for (i, reason) in reasons.iter().enumerate() {
            ui.inspector_readout(Some(width), &format!("ws-precision-{i}"), "precision", &reason.message);
        }
    }
}

/// A loop or scope says what it is, then the rows of its rail.
pub fn zone_says(ui: &mut Ui, width: f64, zone_text: &Option<(&'static str, fn(&str) -> String)>, n: &p::Node) {
    if let (Some(z), Some((_, sentence))) = (&n.zone, zone_text) {
        let var = z.rail.iter().find(|r| r.role == p::Role::Var).map(|r| r.name.clone()).unwrap_or_else(|| "item".to_owned());
        ui.inspector_message("ws-zone-says", &sentence(&var));
        for r in &z.rail {
            let label = match r.role {
                p::Role::Var => format!("{} in", r.name),
                p::Role::Acc => format!("{} (next)", r.name),
                p::Role::Param => format!("{} (input)", r.name),
                p::Role::Capture => r.name.clone(),
            };
            ui.inspector_readout(Some(width), &format!("ws-rail-{}", r.name), &label, &match &r.expr {
                Some(e) => lisp::flat(e),
                None => "same for all".to_owned(),
            });
        }
    }
}

/// A graph input: its type, and its default as one Lisp form.
pub fn input_inspector(ui: &mut Ui, graph: &str, width: f64, scope: &p::Scope, path: &[Symbol]) -> (Vec<Change>, Vec<ScopeChange>) {
    let Some(input) = scope.inputs.iter().find(|i| i.path == path) else { return (vec![], vec![]) };
    ui.inspector_header("ws-input", Some("input"), None, None, None, &[], None, false, &input.name, &ty::to_string(&input.ty));
    let shown = input.default.as_ref().map(lisp::flat).unwrap_or_default();
    let mut text = shown.clone();
    ui.inspector_body(&mut |ui| {
        let (row, cx, cy, cw) = ui.inspector_row(Some(width), None, "ws-input-default", "default");
        ui.within(&row, &mut |ui| match &input.default {
            Some(form) if matches!(form.node, SNode::Num(_)) => {
                // a number: the pane's own field, so it drags here as it does on the card
                // the input's type decides the step: a float written [1] scrubs by fractions
                let kind = match input.ty {
                    Ty::Int => Some(param::number::Kind::Int),
                    Ty::Float => Some(param::number::Kind::Float),
                    _ => None,
                };
                text = pxui_graph::scope_frame::num_field(ui, (cx, cy), cw, 20.0, None, kind, None, false, "ws-input-default-field", &shown).unwrap_or_else(|| shown.clone());
            }
            _ => {
                let o = pxui::ui::ValueFieldOpts { left: float_of(&shown).is_none(), ..Default::default() };
                text = ui.value_field((cx, cy), cw, 20.0, &o, &|t| !t.trim().is_empty(), "ws-input-default-field", &shown).0;
            }
        });
    });
    if text == shown {
        return (vec![], vec![]);
    }
    match s::parse(&text) {
        Ok(mut v) if v.len() == 1 => (vec![Change::SyntaxEdit(fe::Op::SetInputDefault { form: graph.to_owned(), input: input.name.clone(), value: v.remove(0) })], vec![]),
        Ok(v) if v.is_empty() => (vec![], vec![]),
        Ok(_) => (vec![Change::Declined("A default is one Lisp form".to_owned())], vec![]),
        Err(d) => (vec![Change::Declined(format!("Default: {}", d.message))], vec![]),
    }
}

/// An expression chip clicked on a card: the row is the subject.
pub fn expression_inspector(ui: &mut Ui, width: f64, value: &Model, holder: &p::Node, r: &p::Row) -> (Vec<Change>, Vec<ScopeChange>) {
    let e = r.expr.clone().expect("expression row");
    let op = s::head(&e).unwrap_or_default().to_owned();
    ui.inspector_header("ws-header", Some(&op), None, None, None, &[], None, false, &format!("{} \u{b7} {}", p::title(holder), r.label), &lisp::flat(&e));
    let entries = sub_entries(true, r);
    let mut requests: Vec<Change> = Vec::new();
    let source: &[S] = &value.doc.workspace.ws.source;
    ui.inspector_body(&mut |ui| {
        if ui.inspector_button(&format!("ws-unfold-{}", r.label), &format!("Unfold {}", r.label)) {
            requests.push(Change::SyntaxEdit(fe::Op::Unfold { node: holder.path.clone(), key: r.key.clone(), sub: vec![] }));
        }
        if !entries.is_empty() {
            let opts = insp::FlowOpts { width: Some(width), kind_label: Some(kind_label(&op)), ..Default::default() };
            let rows: Vec<insp::FlowRow> = entries.iter().map(|(_, _, _, ir)| ir.clone()).collect();
            let events = insp::flow_fields(ui, &opts, None, &rows);
            let find = |name: &str| arg_row_in(&entries, name);
            for ev in events {
                if let Some(c) = arg_request(source, holder, &find, &ev) {
                    requests.push(c);
                }
            }
        }
    });
    (requests, vec![])
}

/// What the Inspector hands back for a frame: graph requests and the probe moves.
pub type Inspected = (Vec<Change>, Vec<ScopeChange>);

/// The optional arguments of `workspace_inspector`.
pub struct InspectorOpts<'a> {
    pub image: &'a dyn Fn(i64) -> Option<rays::image::Image>,
    pub laid: Option<ScopeKey>,
    pub window: bool,
    pub on_choice: Option<&'a dyn Fn(&str, &UiBox)>,
    pub resized: Resized<'a>,
    pub follows: &'a dyn Fn(&[Symbol]) -> Option<String>,
}

#[derive(Clone)]
enum Pressed {
    View,
    Bypass,
    Activate(Path),
    Enter,
}

fn zone_words(kind: p::ZoneKind) -> (&'static str, fn(&str) -> String) {
    match kind {
        p::ZoneKind::For => ("repeat \u{b7} collect a list", |v| format!("Runs its body once for every {v} and collects the results into a list.")),
        p::ZoneKind::Fold => ("iterate \u{b7} feed back", |v| format!("Runs its body for every {v} and hands the result to the next run.")),
        p::ZoneKind::Scan => ("scan \u{b7} keep every step", |v| format!("Runs its body for every {v} and keeps each step's result in a list.")),
        p::ZoneKind::Sum => ("sum \u{b7} add up", |v| format!("Runs its body for every {v} and adds the results.")),
        p::ZoneKind::Let => ("scope \u{b7} names for its result", |_| "Names shared by its result; it runs once.".to_owned()),
        p::ZoneKind::State => ("state \u{b7} fold frames", |_| "Reads the previous frame's value and stores this frame's result.".to_owned()),
        p::ZoneKind::Fn => ("function \u{b7} runs per call", |_| "A function: its body runs each time it is called.".to_owned()),
        p::ZoneKind::Branch => ("conditional arm", |_| "Runs when its condition is selected.".to_owned()),
    }
}

/// The inspector of the node selected in the workspace pane: its value at the probe, whether it
/// recooks every frame, the list of its iterations (a click moves the zone's probe), and the
/// catalog parameters of the lowered node at that iteration. An edit is a `SetArg` on the
/// authored argument; an argument that is not a literal shows its expression and is locked.
/// Returns the graph requests and the probe moves.
pub fn workspace_inspector<'a>(value: &ModelView<'a>, ui: &mut Ui, width: f64, path: &[Symbol], o: &InspectorOpts) -> Inspected {
    let laid = match &o.laid {
        Some(k) => Some(k.clone()),
        None => crate::editor_model::pane_laid(value).cloned(),
    };
    let Some(laid) = laid else { return (vec![], vec![]) };
    let Some(records) = laid.records.clone() else { return (vec![], vec![]) };
    let scope = laid.scope.clone();
    let graph = laid.graph.clone();
    // `holder @ [":" ^ label]`: a row of a node that is an operator call (the pane's expression chip)
    let row_subject: Option<(p::Node, p::Row)> = match path.split_last() {
        Some((last, rev_holder)) if last.starts_with(':') && !rev_holder.is_empty() && !scope.inputs.iter().any(|i| i.path == path) => {
            let label = &last[1..];
            p::find(&scope, rev_holder).and_then(|h| {
                let r = h.rows.iter().find(|r| r.label == label && r.expr.as_ref().map_or(false, operator_call)).cloned();
                r.map(|r| (h, r))
            })
        }
        _ => None,
    };
    let found = p::find(&scope, path);
    let n = match found {
        None => {
            return match row_subject {
                Some((h, r)) => expression_inspector(ui, width, value, &h, &r),
                None => input_inspector(ui, &graph, width, &scope, path),
            };
        }
        Some(n) => n,
    };
    let chains = probe::chains(&scope);
    let chain: Vec<Path> = chains.get(&n.path).cloned().unwrap_or_default();
    let probes = probes_of(value, &chains, &n.path);
    let footer = records.borrow_mut().footer(&n, &probes);
    let preview = match records.borrow_mut().at(&n.path, &probes) {
        Some(Summary::Value(flow_syntax::value::Value::Deferred(Ty::Named(name), id))) if name == "image" => (o.image)(id),
        Some(Summary::Image(im)) => (o.image)(im.node),
        _ => None,
    };
    if let Some(image) = preview {
        let (w, h) = rays::image::dimensions(&image);
        ui.inspector_readout(Some(width), "image-size", "image", &format!("{w} \u{d7} {h}"));
        let height = 160.0f64.min(width * h as f64 / w as f64);
        let bx = ui.box_("image-thumbnail", pxui::ui::BoxOpts::default().w(pxui::ui::Size::Px(width)).h(pxui::ui::Size::Px(height)));
        let image = image.clone();
        ui.draw(&bx, move |paint, (x, y, bw, bh)| {
            let scale = (bw / w as f64).min(bh / h as f64);
            paint.image(x, y, w as f64 * scale, h as f64 * scale, &image);
        });
    }
    // a loop or scope says what it is, then the rows of its rail
    let zone_text: Option<(&'static str, fn(&str) -> String)> = n.zone.as_ref().map(|z| zone_words(z.kind));
    // a bypassed node passes its input through: the plan node is the upstream one, so it has no
    // number, cook or arguments of its own to show here
    let node_id = if n.bypass { None } else { compiled_at(value, &chains, &records, &n.path) };
    let node = node_id.and_then(|id| compiled_node(value, id));
    let authored_row = |key: &str| -> Option<S> {
        n.rows.iter().find(|r| r.key == ArgKey::Kw(key.to_owned())).and_then(|r| r.expr.clone())
    };
    let authored = |parameter: &port::Parameter| authored_row(&parameter.path);
    // the arguments: a compiled node's own fields, else (a scene or material node, which has none)
    // the checked kind's parameters, valued by what is written
    let fields: Vec<FieldView> = match &node {
        Some(nd) => node::parameter_fields(nd),
        None => kind_fields(value, &graph, &n.head, &authored_row).unwrap_or_default(),
    };
    let parameters: Vec<port::Parameter> = port::parameters(false, &fields).unwrap_or_default();
    // the docked column under ~340 points (workspace.html) has no number and no bar; the window
    // follows windows.html (no bar); the 380 column (inspector.html) has both
    let narrow = width < 340.0;
    let window = o.window;
    let doc_layout = &value.doc.workspace.ws.layout;
    let pin_of = |r: &p::Row| -> Option<bool> { doc_layout.rows.get(&n.path).and_then(|m| m.get(&r.label)).copied() };
    let row_of = |pth: &str| -> Option<p::Row> { n.rows.iter().find(|r| r.key == ArgKey::Kw(pth.to_owned())).cloned() };
    let on_card_row = |pth: &str| -> bool { row_of(pth).map_or(false, |r| p::row_shown(pin_of(&r), &r)) };
    let geo: Option<probe::Geometry> = node_id.and_then(|id| node_owner(value, id).and_then(|oid| cook::geometry(&value.cook, oid, id).cloned()));
    let displayed_path = viewed_path(value, &scope);
    let readouts = records.borrow_mut().readouts(&n, &probes);
    // the head: the kind, VIEW while displayed, the node's number; the name edited in place; what
    // the probe and the cook know; the flags the card has as buttons
    let follow = (o.follows)(&n.path);
    // the active camera: the one the scene's root names as its :camera
    let root_node = scope.nodes.iter().find(|m| m.head == "scene/root").cloned();
    let last_name = n.path.last().map_or("", |name| name.as_str());
    let active_camera = n.head == "scene/camera"
        && match &root_node {
            Some(root) => root.rows.iter().any(|r| r.key == ArgKey::Kw("camera".to_owned()) && r.expr.as_ref().is_some_and(|form| matches!(form.node, SNode::Sym(name) if form.name(name) == last_name))),
            None => false,
        };
    // a scene or material node says its state: the active camera is `active`, the rest how it cooks
    let scene_state: Option<String> = if !(n.head.starts_with("scene/") || n.head.starts_with("material/")) {
        None
    } else {
        Some(if active_camera { "active".to_owned() } else { readouts.iter().find(|(k, _)| k == "cook").map(|(_, v)| v.clone()).unwrap_or_else(|| "cached".to_owned()) })
    };
    let mut buttons: Vec<(HeadAction, Pressed)> = Vec::new();
    if [ty::geometry(), ty::drawing(), ty::image()].contains(&n.ty) {
        buttons.push((HeadAction { caption: "View".into(), keycap: "V".into(), active: displayed_path.as_ref() == Some(&n.path), usable: true }, Pressed::View));
    }
    if p::bypassable(&n) {
        buttons.push((HeadAction { caption: "Bypass".into(), keycap: "B".into(), active: n.bypass, usable: true }, Pressed::Bypass));
    }
    if let Some(root) = &root_node {
        if n.head == "scene/camera" {
            buttons.push((HeadAction { caption: "Active".into(), keycap: String::new(), active: active_camera, usable: !active_camera }, Pressed::Activate(root.path.clone())));
        }
    }
    if follow.is_some() {
        buttons.push((HeadAction { caption: "Enter".into(), keycap: "I".into(), active: false, usable: true }, Pressed::Enter));
    }
    let detail = match (&zone_text, &geo) {
        (Some((kind, _)), _) => format!("{kind} \u{b7} {}", ty::to_string(&n.ty)),
        (None, Some(g)) => {
            // what the cook knows of the node: its points and primitives
            let count = |k: i64, word: &str| format!("{} {word}", group_digits(k));
            let mut parts = vec![count(g.points, if g.points == 1 { "point" } else { "points" }), count(g.prims, if g.prims == 1 { "prim" } else { "prims" })];
            if let Some(sec) = g.seconds {
                parts.push(if sec == 0.0 { "cached".to_owned() } else { format!("host {} s", cfmt::f(sec, 3)) });
            }
            parts.join(" \u{b7} ")
        }
        (None, None) => match &scene_state {
            Some(state) => format!("{} \u{b7} {}", n.head, state),
            None => {
                let text = readouts.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>().join(" \u{b7} ");
                if text.is_empty() { ty::to_string(&n.ty) } else { text }
            }
        },
    };
    let actions: Vec<HeadAction> = buttons.iter().map(|(a, _)| a.clone()).collect();
    let index = if narrow || window { None } else { node_id.map(|id| format!("NO. {id:04}")) };
    let rename: Option<Rc<dyn Fn(&str) -> bool>> = if n.synthetic { None } else { Some(Rc::new(flow_syntax::symbol::valid_name)) };
    let reset_enabled = parameters.iter().any(|pm| authored(pm).is_some());
    let head = ui.inspector_header(
        "ws-header",
        Some(&n.head),
        if displayed_path.as_ref() == Some(&n.path) { Some("view") } else { None },
        index.as_deref(),
        rename,
        &actions,
        Some("Reset all"),
        reset_enabled,
        &p::title(&n),
        &detail,
    );
    let pressed = head.chosen.map(|i| buttons[i as usize].1.clone());
    // Reset all: every written argument of the node removed, one undoable entry
    let reset_all: Vec<Change> = if !head.reset_pressed {
        vec![]
    } else {
        let ops: Vec<fe::Op> = parameters
            .iter()
            .filter(|pm| authored(pm).is_some())
            .map(|pm| fe::Op::Disconnect { node: n.path.clone(), key: ArgKey::Kw(pm.path.clone()), fallback: None })
            .collect();
        match ops.len() {
            0 => vec![],
            1 => vec![Change::SyntaxEdit(ops.into_iter().next().unwrap())],
            _ => vec![Change::SyntaxBatch("Reset all".to_owned(), ops)],
        }
    };
    // the name: editing it is the pane's Rename (the text's binding name); a nested node has no
    // name yet, giving it one binds it
    let rename_edit: Vec<Change> = if n.synthetic || head.renamed == p::title(&n) {
        vec![]
    } else {
        vec![Change::SyntaxEdit(fe::Op::Rename { node: n.path.clone(), to_: head.renamed.clone() })]
    };
    let flags: Inspected = match pressed {
        Some(Pressed::Bypass) => (vec![Change::SyntaxEdit(fe::Op::ToggleBypass { node: n.path.clone() })], vec![]),
        Some(Pressed::View) => (vec![], vec![ScopeChange::DisplaySet(n.path.clone())]),
        Some(Pressed::Enter) => (vec![], vec![ScopeChange::Activated(n.path.clone())]),
        Some(Pressed::Activate(root)) => (
            vec![Change::SyntaxEdit(fe::Op::SetArg { node: root, key: ArgKey::Kw("camera".to_owned()), sub: vec![], value: sym(&last_name) })],
            vec![],
        ),
        None => (vec![], vec![]),
    };
    let mut on_card = 0i64;
    let mut requests: Vec<Change> = Vec::new();
    let mut picks: Vec<ScopeChange> = Vec::new();
    ui.inspector_body(&mut |ui| {
        zone_says(ui, width, &zone_text, &n);
        readout_rows(ui, &n, value, &footer, &readouts, width, zone_text.is_some());
        let hoist: Vec<Change> = if footer.invariant && ui.inspector_button("ws-hoist", "Move out of the loop") {
            vec![Change::SyntaxEdit(fe::Op::Hoist { node: n.path.clone() })]
        } else {
            vec![]
        };
        // a macro call: what it is, and the request the pane's lens button makes
        let mut macro_edit: Vec<Change> = vec![];
        if let Some(name) = &n.macro_ {
            ui.inspector_message("ws-macro", &format!("Macro {name}: its rows are the holes."));
            if ui.inspector_button("ws-inline", "Replace call with expansion") {
                macro_edit.push(Change::SyntaxEdit(fe::Op::InlineMacro { node: n.path.clone() }));
            }
        }
        // the items of a list or a string: each can move up a place
        let mut movers: Vec<Change> = vec![];
        if !n.synthetic && p::reorderable(&n) {
            for r in &n.rows {
                if let ArgKey::Pos(k) = r.key {
                    if k >= 1 && k < 24 && ui.inspector_button(&format!("ws-move-{k}"), &format!("Move item {} up", k + 1)) {
                        movers.push(Change::SyntaxEdit(fe::Op::MoveItem { node: n.path.clone(), pos: k }));
                    }
                }
            }
        }
        if n.head == "settings/config" {
            ui.inspector_message("ws-startup-settings", "Title, size, frame rate and seed apply on restart.");
        }
        let ref_rows = reference_rows(value, &n);
        let (layout_names, layout_row, size_names, size_row) = layout_and_size_rows(value, &n);
        let op_rows = operator_rows(&n, &pin_of, size_row.is_some(), layout_row.is_some(), !ref_rows.is_empty(), !parameters.is_empty());
        let mut entries: Vec<Entry> = Vec::new();
        for (r, ir) in &op_rows {
            entries.push((ir.path.clone(), r.clone(), vec![], ir.clone()));
            entries.extend(sub_entries(ir.shown, r));
        }
        // the operator rows' Unfold: the nested call becomes its own binding and card
        let mut unfold_nested: Vec<Change> = vec![];
        for (r, _) in &op_rows {
            if r.expr.as_ref().map_or(false, operator_call) && ui.inspector_button(&format!("ws-unfold-{}", r.label), &format!("Unfold {}", r.label)) {
                unfold_nested.push(Change::SyntaxEdit(fe::Op::Unfold { node: n.path.clone(), key: r.key.clone(), sub: vec![] }));
            }
        }
        let arg_row = |name: &str| arg_row_in(&entries, name);
        let mut rows: Vec<insp::FlowRow> = Vec::new();
        rows.extend(layout_row.clone());
        rows.extend(size_row.clone());
        rows.extend(ref_rows.iter().map(|(_, ir)| ir.clone()));
        rows.extend(entries.iter().map(|(_, _, _, ir)| ir.clone()));
        for pm in &parameters {
            let wired = match authored(pm) {
                Some(e) => !is_literal(&e),
                None => false,
            };
            // a computed argument reads as its expression, "=" first: typing another one, or a
            // number, replaces it; the cross removes it. A plain name is a link, read as "<- name";
            // anything else is an expression
            let drive = match authored(pm) {
                Some(e) if wired && matches!(e.node, SNode::Sym(_)) => match e.node { SNode::Sym(name) => Some(e.name(name).to_owned()), _ => unreachable!() },
                Some(e) if wired => Some(format!("={}", lisp::flat(&e))),
                _ => None,
            };
            // what the drive gives now: the node's value at the probe
            let live = match pm.fields.as_slice() {
                [FieldView { current: Value::Int(i), .. }] => Some(i.to_string()),
                [FieldView { current: Value::Float(f), .. }] => Some(param::number::show(*f)),
                [FieldView { current: Value::Bool(b), .. }] => Some(b.to_string()),
                _ => None,
            };
            rows.push(insp::FlowRow { path: pm.path.clone(), fields: pm.fields.clone(), shown: on_card_row(&pm.path), locked: false, drive, live });
        }
        let mut expanded: Vec<String> = fields.iter().filter_map(|f| f.folder.first().cloned()).collect();
        expanded.sort();
        expanded.dedup();
        let edits = field_edits(ui, o.on_choice, width, &expanded, &parameters, &ref_rows, &row_of, &arg_row, value, &size_names, &n, o.resized, &graph, &layout_names, &rows);
        output_section(ui, width, geo.as_ref());
        let note = note_section(ui, width, &n);
        picks = iteration_picks(ui, &footer, &chain, &probes, &n, &records);
        on_card = rows.iter().filter(|r| r.shown && !r.path.starts_with('@')).count() as i64;
        requests = [hoist, macro_edit, note, movers, unfold_nested, edits].concat();
    });
    // the sheet's bar: what a field takes, and how many rows are on the card
    if !(window || narrow) {
        ui.inspector_bar(&[("s".to_owned(), "pin row to card".to_owned()), ("\u{2325} click".to_owned(), "type a value".to_owned())], on_card);
    }
    let (flag_changes, flag_picks) = flags;
    let mut all = flag_changes;
    all.extend(rename_edit);
    all.extend(reset_all);
    all.extend(requests);
    let mut moves = flag_picks;
    moves.extend(picks);
    (all, moves)
}

/// The node of the graph pane that a lowered node of the open object was made by, at the
/// iterations the selectors probe.
pub fn node_path<'a>(value: &ModelView<'a>, node_id: i64) -> Option<Path> {
    let laid = crate::editor_model::pane_laid(value)?;
    let records = laid.records.as_ref()?;
    let chains = probe::chains(&laid.scope);
    fn search(value: &Model, chains: &HashMap<Path, Vec<Path>>, records: &Rc<RefCell<Probe>>, node_id: i64, sc: &p::Scope) -> Option<Path> {
        sc.nodes.iter().find_map(|n| {
            if compiled_at(value, chains, records, &n.path) == Some(node_id) {
                Some(n.path.clone())
            } else {
                n.zone.as_ref().and_then(|z| search(value, chains, records, node_id, &z.scope))
            }
        })
    }
    search(value, &chains, records, node_id, &laid.scope)
}

/// The same lowering, physically: its recording evaluation is the identity.
pub fn same_lowered(a: &flow_sop::lower::T, b: &flow_sop::lower::T) -> bool {
    std::sync::Arc::ptr_eq(&a.evaluated, &b.evaluated)
}
