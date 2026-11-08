//! The open level's list: its objects, the focused geometry object and its cook, and the list
//! rows and intents. Port of lib/rays_editor/editor_list.ml.
#![allow(non_camel_case_types)]
use std::collections::HashMap;
use std::rc::Rc;

use editor_document::document::Level;
use editor_document::kind_graph as kg;
use editor_document::objects;
use editor_document::scene_sync;
use flow_syntax::{context, ty};
use flow_sop::network as net;
use param::Value;
use pxui_shell::tree::{self, DropAt, Intent, Row};
use rays_math::color::{self, T as Color};
use rays_math::mat4;
use sop::custom;
use sop::edit_graph;
use sop::node::{self, CookMode};

use crate::cook;
use crate::editor_inspect::{kind, network, node_owner, objects as level_objects, scene};
use crate::editor_model::*;
use crate::editor_panes::scope_node;
use crate::selection;

pub fn enterable(value: &EditorModel, id: i64) -> bool {
    match kind(value, id).as_deref() {
        Some("geometry") => value.doc.networks.contains_key(&id),
        Some("world") => value.doc.layers.contains_key(&id),
        Some(_) | None => false,
    }
}

/// Switch the open level: nothing is selected there, and the list starts again.
pub fn open_level(value: &mut EditorModel, level: Level) {
    if level == pane_level(&*value) {
        return;
    }
    value.ui.borrow_mut().unfocus();
    let world = match level {
        Level::Inside(id) => kind(&*value, id).as_deref() == Some("world"),
        Level::Scene => false,
    };
    value.map_view = world;
    value.tree = tree::create();
    with_pane(value, &|pane| graph_pane { at: level, shown: None, picked: selection::empty(), ..pane.clone() });
}

/// The geometry objects to cook: visible ones, each with its network.
pub fn geometry_objects(value: &EditorModel) -> Vec<(i64, &net::Network, i64)> {
    // a workspace without a scene graph shows each graph the lowering made a network of; an image
    // graph is one (the canvas previews its nodes) but its payload is not geometry, so the geometry
    // cook never takes it (Flow_native.Workspace_images cooks images)
    let ws = &value.doc.workspace.ws;
    let image_graph = |node: &kg::Node| ws.checked.graphs.iter().any(|g| g.name == node.label && context::result(g.context) == ty::image());
    let scene_graph = scene(value);
    let mut found = Vec::new();
    for id in objects::ids("geometry", &scene_graph) {
        if let (Some(node), Some(network)) = (kg::find(&scene_graph, id), value.doc.networks.get(&id)) {
            if objects::visible(&node) && !image_graph(&node) {
                match value.viewed.get(&id) {
                    Some((graph, displayed)) => found.push((id, graph, *displayed)),
                    None => {
                        if let Some(displayed) = network.displayed {
                            found.push((id, &network.graph, displayed));
                        }
                    }
                }
            }
        }
    }
    found
}

/// The object the sketch-facing single-object accessors describe: the open one, else the first
/// visible geometry object.
pub fn focus_object<'a>(value: impl Into<ModelView<'a>>) -> Option<i64> {
    let value = &value.into();
    match pane_level(value) {
        Level::Inside(id) if kind(value, id).as_deref() == Some("geometry") => Some(id),
        Level::Inside(_) | Level::Scene => geometry_objects(value).first().map(|(id, _, _)| *id),
    }
}

pub fn pieces(value: &EditorModel) -> &[cook::piece] {
    &value.cook.pieces
}

pub fn piece<'a>(value: impl Into<ModelView<'a>>) -> Option<&'a cook::piece> {
    let value = &value.into();
    let id = focus_object(value)?;
    pieces(value.model).iter().find(|piece| piece.id == id)
}

pub fn prepared<'a>(value: impl Into<ModelView<'a>>) -> Option<&'a cook::Prepared_ref> {
    let value = &value.into();
    piece(value).map(|piece| &piece.prepared)
}

/// The object in focus (the selected scene object, or the one whose network is open): its world
/// transform, its cooked bounds in its own space, and its name.
pub fn selected_bounds<'a>(value: impl Into<ModelView<'a>>) -> Option<(mat4::T, cook::bounds, String)> {
    let value = &value.into();
    let piece = piece(value)?;
    let bounds = cook::piece_bounds(piece)?;
    let scene_graph = scene(value);
    let node = kg::find(&scene_graph, piece.id)?;
    Some((objects::world(&scene_graph, piece.id), bounds, node.label))
}

pub fn no_graph() -> node::T {
    let mut args = custom::plain_args("points");
    args.parameters = b"count=0".to_vec();
    args.cook_mode = CookMode::Generator;
    custom::plain(args, Vec::new(), std::sync::Arc::new(|_, _| rdk_rays::geometry_view::points_of_nothing().map_err(|e| format!("{e:?}"))))
}

pub fn graph<'a>(value: impl Into<ModelView<'a>>) -> node::T {
    let value = &value.into();
    let by_focus = focus_object(value).and_then(|id| value.cook.graphs.iter().find(|(i, _)| *i == id).map(|(_, g)| g.clone()));
    match (by_focus, piece(value)) {
        (Some(graph), _) => graph,
        (None, Some(piece)) => piece.graph.clone(),
        (None, None) => no_graph(),
    }
}

pub fn displayed_node<'a>(value: impl Into<ModelView<'a>>) -> node::T {
    let value = &value.into();
    graph(value)
}

pub fn settings(value: &EditorModel) -> editor_document::settings::T {
    value.doc.settings.clone()
}

pub fn timeline(value: &EditorModel) -> crate::timeline::Timeline {
    value.timeline
}

pub fn selected_node<'a>(value: impl Into<ModelView<'a>>) -> Option<picked> {
    let value = &value.into();
    match scope_node(value) {
        Some((_, node)) => Some(of_sop(&node)),
        None => {
            let id = selection::selected(pane_selection(value))?;
            kg::find(&level_objects(value), id).map(|n| of_object(&n))
        }
    }
}

/// Scoped SOP handles use their owning object's transform. Navigation selection uses the open
/// object or the scene object's parent chain.
pub fn space<'a>(value: impl Into<ModelView<'a>>) -> mat4::T {
    let value = &value.into();
    let owner = scope_node(value).and_then(|(_, node)| node_owner(value, node::id(&node)));
    if let Some(owner) = owner {
        return objects::world(&scene(value), owner);
    }
    match pane_level(value) {
        Level::Inside(id) if kind(value, id).as_deref() == Some("geometry") => objects::world(&scene(value), id),
        Level::Inside(_) => mat4::identity(),
        Level::Scene => match selection::selected(pane_selection(value)) {
            Some(id) => objects::parent_world(&scene(value), id),
            None => mat4::identity(),
        },
    }
}

// ---- the graph as a list ----

fn hex(text: &str) -> Color {
    color::hex_exn(text).unwrap_or_else(|_| color::hex_exn("#000000").expect("black"))
}

/// A row's kind letter and colour (the prototype's palette).
pub fn badge(operation: &str) -> (String, Color) {
    match operation {
        "geometry" => ("G".into(), hex("#3f7a52")),
        "light" => ("L".into(), hex("#b07a1c")),
        "camera" => ("C".into(), hex("#5a4f86")),
        "world" => ("W".into(), hex("#285f77")),
        "sun" => ("\u{2726}".into(), hex("#b07a1c")),
        "gradient" | "sky" | "shape" | "scatter" | "room" => ("\u{b7}".into(), hex("#6d8a95")),
        _ => ("S".into(), hex("#566463")),
    }
}

fn emit_object(children: &HashMap<Option<i64>, Vec<kg::Node>>, active: Option<i64>, depth: i64, node: &kg::Node, rows: &mut Vec<Row>) {
    let flags: Vec<bool> = ["visible", "render"].iter().filter(|name| objects::has_flag(name, node)).map(|name| objects::flag(name, node)).collect();
    rows.push(Row {
        id: node.id,
        depth,
        label: node.label.clone(),
        detail: if active == Some(node.id) { "* render cam".to_string() } else { node.kind.clone() },
        badge: badge(&node.kind),
        link: false,
        ghost: false,
        flags,
    });
    for child in sorted(children, Some(node.id)) {
        emit_object(children, active, depth + 1, &child, rows);
    }
}

fn sorted(children: &HashMap<Option<i64>, Vec<kg::Node>>, parent: Option<i64>) -> Vec<kg::Node> {
    let mut nodes = children.get(&parent).cloned().unwrap_or_default();
    nodes.sort_by_key(|n| n.id);
    nodes
}

/// The objects as a tree by parent, siblings in id order.
pub fn scene_rows(active: Option<i64>, objects: &[kg::Node]) -> Vec<Row> {
    let mut children: HashMap<Option<i64>, Vec<kg::Node>> = HashMap::new();
    for node in objects {
        children.entry(node.input).or_default().insert(0, node.clone());
    }
    let mut rows = Vec::new();
    for node in sorted(&children, None) {
        emit_object(&children, active, 0, &node, &mut rows);
    }
    rows
}

/// The World's layers, bottom first.
pub fn layer_rows(layers: &[kg::Node]) -> Vec<Row> {
    let mut stack = kg::stack(layers);
    stack.reverse();
    stack
        .iter()
        .map(|node| Row {
            id: node.id,
            depth: 0,
            link: false,
            ghost: false,
            label: node.label.clone(),
            detail: node.kind.clone(),
            badge: badge(&node.kind),
            flags: if objects::has_flag("visible", node) { vec![objects::flag("visible", node)] } else { Vec::new() },
        })
        .collect()
}

struct Trunk {
    inputs: HashMap<i64, Vec<Option<i64>>>,
    emitted: std::collections::HashSet<i64>,
    rows: Vec<(i64, i64, bool)>,
}

impl Trunk {
    fn first(&self, id: i64) -> Option<i64> {
        self.inputs.get(&id).and_then(|slots| slots.iter().find_map(|s| *s))
    }

    fn emit_trunk(&mut self, id: i64, depth: i64) {
        // The chain up the first inputs, stopping at a node already listed.
        let mut shared = None;
        let mut nodes = Vec::new();
        let mut cur = id;
        loop {
            if self.emitted.contains(&cur) {
                shared = Some(cur);
                break;
            }
            match self.first(cur) {
                Some(source) if self.inputs.contains_key(&source) => {
                    nodes.push(cur);
                    cur = source;
                }
                _ => {
                    nodes.push(cur);
                    break;
                }
            }
        }
        nodes.reverse();
        if let Some(id) = shared {
            self.rows.push((id, depth, true));
        }
        for id in nodes {
            self.emitted.insert(id);
            self.rows.push((id, depth, false));
            let mut primary_seen = false;
            let slots = self.inputs.get(&id).cloned().unwrap_or_default();
            for slot in slots {
                if let Some(side) = slot {
                    if self.inputs.contains_key(&side) {
                        if !primary_seen {
                            primary_seen = true;
                        } else if self.emitted.contains(&side) {
                            self.rows.push((side, depth + 1, true));
                        } else {
                            self.emit_trunk(side, depth + 1);
                        }
                    }
                }
            }
        }
    }
}

/// The network as list rows `(id, depth, link)`, sources first: rows follow each node's first
/// input, other inputs nest one level under the node that consumes them, and a node reached twice
/// repeats as a `link` row.
pub fn trunk(network: &net::Network) -> Vec<(i64, i64, bool)> {
    let infos = edit_graph::inspect(&network.geometry);
    let mut consumed = std::collections::HashSet::new();
    let mut inputs = HashMap::new();
    for info in &infos {
        inputs.insert(info.id, info.inputs.clone());
        for source in info.inputs.iter().flatten() {
            consumed.insert(*source);
        }
    }
    let ids: Vec<i64> = infos.iter().map(|info| info.id).collect();
    let mut st = Trunk { inputs, emitted: std::collections::HashSet::new(), rows: Vec::new() };
    for id in ids {
        if !consumed.contains(&id) {
            st.emit_trunk(id, 0);
        }
    }
    st.rows
}

pub fn trunk_rows(network: &net::Network, viewed: Option<i64>, flags: &dyn Fn(i64, Option<&node::T>) -> Vec<bool>) -> Vec<Row> {
    trunk(network)
        .into_iter()
        .map(|(id, depth, link)| {
            let node = edit_graph::find(&network.geometry, id);
            let label = node.as_ref().map(|n| node::label(n).to_string()).unwrap_or_default();
            let operation = node.as_ref().map(|n| node::operation(n).to_string()).unwrap_or_default();
            let mut details = vec![operation.clone()];
            if viewed == Some(id) {
                details.push("VIEW".to_string());
            }
            Row { id, depth, link, ghost: false, label, detail: details.join(" \u{b7} "), badge: badge(&operation), flags: flags(id, node.as_ref()) }
        })
        .collect()
}

/// The rows a level's list shows, and its flag columns, built when asked.
pub type lazy_rows = Rc<dyn Fn() -> (Vec<Row>, Vec<String>)>;

/// The open level's list: what it is made from, and its rows and flag columns when asked.
pub fn rows<'a>(value: impl Into<ModelView<'a>>) -> (row_source, lazy_rows) {
    let value = &value.into();
    match (pane_level(value), network(value)) {
        (Level::Scene, _) => {
            let scene_graph = scene(value).clone();
            let active = value.doc.active_camera;
            (row_source::Objects(scene_graph.clone()), Rc::new(move || (scene_rows(active, &scene_graph), vec!["v".to_string(), "r".to_string()])))
        }
        (Level::Inside(_), Some(network)) => {
            let graph = network.graph.clone();
            let displayed = network.displayed;
            (
                row_source::Network(network.graph.clone()),
                Rc::new(move || {
                    (
                        trunk_rows(&graph, displayed, &|node_id, node| match node {
                            Some(_) => vec![Some(node_id) == displayed],
                            None => Vec::new(),
                        }),
                        vec!["d".to_string()],
                    )
                }),
            )
        }
        (Level::Inside(_), None) => {
            let layers = level_objects(value);
            (row_source::Objects(layers.clone()), Rc::new(move || (layer_rows(&layers), vec!["v".to_string()])))
        }
    }
}

/// A layer chain in stack order, bottom first.
pub fn restack(layers: &[kg::Node], order: &[i64]) -> kg::T {
    let mut below: Option<i64> = None;
    let mut layers = layers.to_vec();
    for id in order {
        layers = kg::update(&layers, *id, &|mut node| {
            node.input = below;
            node
        });
        below = Some(*id);
    }
    layers
}

pub fn apply_parameter(objects_: &[kg::Node], id: i64, name: &str, value: Value) -> kg::T {
    match kg::find(objects_, id) {
        Some(node) if objects::has_flag(name, &node) => match kg::set_fields(objects_, id, &[(name.to_string(), value)]) {
            Ok((objects_, _)) => objects_,
            Err(_) => objects_.to_vec(),
        },
        Some(_) | None => objects_.to_vec(),
    }
}

/// The layer stack a World list's move asks for, in the rows' order.
pub fn restacked(rows: &[Row], intent: &Intent) -> Option<Vec<i64>> {
    let order: Vec<i64> = rows.iter().filter(|row| !row.link).map(|row| row.id).collect();
    let move_in = |ids: &[i64], target: i64, drop: DropAt| -> Vec<i64> {
        let rest: Vec<i64> = order.iter().copied().filter(|id| !ids.contains(id)).collect();
        let mut out = Vec::new();
        for id in rest {
            if id != target {
                out.push(id);
            } else {
                match drop {
                    DropAt::Before => {
                        out.extend_from_slice(ids);
                        out.push(id);
                    }
                    DropAt::After | DropAt::Inside => {
                        out.push(id);
                        out.extend_from_slice(ids);
                    }
                }
            }
        }
        out
    };
    match intent {
        Intent::Move { ids, target, drop } => Some(move_in(ids, *target, *drop)),
        Intent::Reorder { ids, delta } => {
            let index = order.iter().position(|id| ids.contains(id))?;
            let others: Vec<i64> = order.iter().copied().filter(|id| !ids.contains(id)).collect();
            let at = (index as i64 + delta).max(0) as usize;
            let target = *others.get(at)?;
            Some(move_in(ids, target, if *delta < 0 { DropAt::Before } else { DropAt::After }))
        }
        _ => None,
    }
}

/// What a scene list's move asks: the objects and the parent they go under (none: the root).
pub fn reparented(document: &[kg::Node], rows: &[Row], intent: &Intent) -> Option<(Vec<i64>, Option<i64>)> {
    match intent {
        Intent::Move { ids, target, drop } => Some((
            ids.clone(),
            match drop {
                DropAt::Inside => Some(*target),
                DropAt::Before | DropAt::After => objects::parent(document, *target),
            },
        )),
        Intent::Indent(ids) if !ids.is_empty() => {
            // Under the previous sibling row.
            let id = ids[0];
            let k = rows.iter().position(|row| row.id == id)?;
            let depth = rows[k].depth;
            let mut found: Option<i64> = None;
            for candidate in (0..k).rev() {
                if found.is_none() && rows[candidate].depth == depth {
                    found = Some(rows[candidate].id);
                } else if rows[candidate].depth < depth {
                    found = Some(-1);
                }
            }
            match found {
                Some(id) if id >= 0 => Some((ids.clone(), Some(id))),
                _ => None,
            }
        }
        _ => None,
    }
}

/// The state `apply_tree` folds: the open level's graph, the selection, the list's view state, an
/// entry request, the history label and the rows the intents refer to.
#[derive(Clone)]
pub struct tree_state {
    pub document: kg::T,
    pub selection: selection::Selection,
    pub tree: tree::Tree,
    pub opened: Option<i64>,
    pub label: Option<String>,
    pub rows: Vec<Row>,
}

/// Tree intents become selection, an entry request, and edits of the derived network for the
/// objects only the host made (reconcile adopts them): what the text has is edited as text
/// (`tree_edit`).
pub fn apply_tree<'a>(value: impl Into<ModelView<'a>>, state: tree_state, intent: &Intent) -> tree_state {
    let value = &value.into();
    let level = pane_level(value);
    let in_text = |id: i64| scene_sync::in_text(&value.doc, level, id);
    let hosted = |ids: &[i64]| -> Vec<i64> { ids.iter().copied().filter(|id| !in_text(*id)).collect() };
    let world = matches!(level, Level::Inside(id) if kind(value, id).as_deref() == Some("world"));
    // the nodes of a geometry object are the lowering of its graph: its list only selects
    let geometry = matches!(level, Level::Inside(id) if kind(value, id).as_deref() == Some("geometry"));
    let mut s = state;
    match intent {
        Intent::Select(ids) => {
            s.selection = if ids.is_empty() { selection::clear() } else { selection::select_nodes(ids) };
        }
        // the display node of a workspace graph is its result
        Intent::Flag { .. } if geometry => {}
        Intent::Flag { ids, column, value: on } => {
            let names: &[&str] = if world { &["visible"] } else { &["visible", "render"] };
            let name = names[*column as usize];
            for id in hosted(ids) {
                s.document = apply_parameter(&s.document, id, name, Value::Bool(*on));
            }
            s.label = Some(if *on { "Show" } else { "Hide" }.to_string());
        }
        Intent::Move { .. } | Intent::Reorder { .. } if world => {
            if let Some(order) = restacked(&s.rows, intent) {
                if !order.iter().all(|id| in_text(*id)) {
                    s.document = restack(&s.document, &order);
                }
                s.label = Some("Reorder layers".to_string());
            }
        }
        Intent::Move { .. } | Intent::Indent(_) if level == Level::Scene => {
            if let Some((ids, parent)) = reparented(&s.document, &s.rows, intent) {
                s.document = objects::reparent(&s.document, &hosted(&ids), parent);
                s.label = Some("Reparent".to_string());
            }
        }
        Intent::Outdent(ids) if level == Level::Scene => {
            s.document = objects::outdent(&s.document, &hosted(ids));
            s.label = Some("Reparent".to_string());
        }
        Intent::Rename(id, _) if in_text(*id) => {
            s.label = Some("Rename".to_string());
        }
        Intent::Rename(id, name) if !geometry => {
            if kg::find(&s.document, *id).is_some() {
                s.document = kg::update(&s.document, *id, &|mut node| {
                    node.label = name.clone();
                    node
                });
                s.label = Some("Rename".to_string());
            }
        }
        Intent::Activate(_) if geometry => {}
        Intent::Activate(id) => {
            s.opened = Some(*id);
        }
        Intent::Delete(ids) if !geometry => {
            s.document = kg::remove(&hosted(ids), &s.document);
            s.selection = selection::clear();
            s.label = Some("Delete".to_string());
        }
        Intent::Rename(..) | Intent::Delete(_) | Intent::Move { .. } | Intent::Indent(_) | Intent::Outdent(_) | Intent::Reorder { .. } => {}
    }
    s
}

/// A list intent as an edit of the text, for the objects the text has (`Scene_sync::write`).
pub fn tree_edit<'a>(value: impl Into<ModelView<'a>>, rows: &[Row], intent: &Intent) -> Option<scene_sync::Edit> {
    let value = &value.into();
    let level = pane_level(value);
    let in_text = |id: i64| scene_sync::in_text(&value.doc, level, id);
    let some = |ids: Vec<i64>, make: &dyn Fn(Vec<i64>) -> scene_sync::Edit| -> Option<scene_sync::Edit> {
        let ids: Vec<i64> = ids.into_iter().filter(|id| in_text(*id)).collect();
        if ids.is_empty() { None } else { Some(make(ids)) }
    };
    let world = matches!(level, Level::Inside(id) if kind(value, id).as_deref() == Some("world"));
    let lowered = matches!(level, Level::Inside(id) if kind(value, id).as_deref() == Some("geometry"));
    let graph = level_objects(value);
    if lowered {
        return None;
    }
    match intent {
        Intent::Flag { ids, column, value: on } => {
            let names: &[&str] = if world { &["visible"] } else { &["visible", "render"] };
            let name = names[*column as usize].to_string();
            let ids: Vec<i64> = ids.iter().copied().filter(|id| kg::find(&graph, *id).is_some_and(|node| objects::has_flag(&name, &node))).collect();
            some(ids, &|ids| scene_sync::Edit::Fields(ids.into_iter().map(|id| (id, vec![(name.clone(), Value::Bool(*on))])).collect()))
        }
        Intent::Rename(id, name) => some(vec![*id], &|_| scene_sync::Edit::Rename(*id, name.clone())),
        Intent::Delete(ids) => some(ids.clone(), &|ids| scene_sync::Edit::Delete(ids)),
        Intent::Move { .. } | Intent::Reorder { .. } if world => {
            let order = restacked(rows, intent)?;
            if order.iter().all(|id| in_text(*id)) { Some(scene_sync::Edit::Restack(order)) } else { None }
        }
        Intent::Move { .. } | Intent::Indent(_) if level == Level::Scene => {
            let (ids, parent) = reparented(&graph, rows, intent)?;
            some(ids, &|ids| scene_sync::Edit::Reparent(ids, parent))
        }
        Intent::Outdent(ids) if level == Level::Scene => some(ids.clone(), &|ids| scene_sync::Edit::Outdent(ids)),
        _ => None,
    }
}
