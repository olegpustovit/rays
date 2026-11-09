//! The model's shell functions: the layouts, panel keys and states, the geometry, graph panels and
//! the inspector panel. Port of lib/rays_editor/editor_shell.ml.
//!
//! Functions that return an updated model take it by value (OCaml `{ value with .. }`).
#![allow(non_camel_case_types)]
use std::borrow::Cow;
use editor_core::panels::{self, Panel};
use editor_document::document::{Level, Origin, T as Document};
use editor_document::workspace_doc;
use flow_syntax::context;
use pxui::theme::Theme;
use pxui::ui::{PanelOpts, Ui};
use pxui_graph::scope_state as scope;
use pxui_shell::inspector::FlowRow;
use pxui_shell::layout;
use rays::frame;
use rays_diagnostic::Diagnostic;

use crate::editor_model::*;
use crate::lisp_text;
use crate::panel_map;
use crate::selection;
use crate::text_pane;

// ---- the shell ----

/// The layouts / [ switches among: a switch's, named from their panels; else the editor graphs of
/// an older file (several named layouts), the current one marked.
pub fn layouts(value: &EditorModel) -> Vec<(String, bool)> {
    if let Some(sw) = value.doc.shell.as_ref().and_then(|s| s.switch.as_ref()) {
        return panels::labels(&sw.layouts).into_iter().enumerate().map(|(i, name)| (name, i as i64 == sw.active)).collect();
    }
    let doc = &value.doc.workspace.ws;
    let current = workspace_doc::editor_graph_ref(doc).map(|g| &g.name);
    let gs: Vec<_> = doc.checked.graphs.iter().filter(|g| g.context == context::editor()).collect();
    if gs.len() <= 1 {
        return Vec::new();
    }
    gs.iter().map(|g| (g.name.clone(), current == Some(&g.name))).collect()
}

/// What the text pane completes beyond its own text: the graphs, the materials, the cameras of the
/// first scene graph and the layouts.
pub fn completion_names(value: &EditorModel) -> lisp_text::Names {
    let ws = &value.doc.workspace.ws;
    let graphs = &ws.checked.graphs;
    let named = |ctx: context::Context| -> Vec<String> { graphs.iter().filter(|g| g.context == ctx).map(|g| g.name.clone()).collect() };
    let scenes = named(context::scene());
    lisp_text::Names {
        graphs: graphs.iter().map(|g| g.name.clone()).collect(),
        materials: named(context::material()),
        cameras: match scenes.first() {
            Some(scene) => text_pane::cameras(&ws.source, scene),
            None => Vec::new(),
        },
        layouts: layouts(value).into_iter().map(|(name, _)| name).collect(),
    }
}

/// The tree drawn now: the editor graph's (the host's when the document has none, or after
/// "Restore layout"), with the split being dragged at its live ratio.
pub fn shell_tree<'a>(value: &'a EditorModel, shell: &'a shell) -> Cow<'a, panels::Panels> {
    let base = match &value.doc.shell {
        Some(s) if !shell.restored => &s.tree,
        _ => &shell.tree,
    };
    match &shell.live {
        Some((node, size)) => Cow::Owned(panels::set_size(node, *size, base.clone())),
        None => Cow::Borrowed(base),
    }
}

pub fn visible(shell: &shell, panel: &Panel) -> bool {
    !shell.hidden.contains(panel)
}

/// A panel key, borrowed: the editor graph, then a binding name or the leaf's place.
enum KeyTail<'a> { Name(&'a str), Place(&'a [usize]) }

fn panel_key_ref<'a>(doc: &'a Document, path: &'a [usize]) -> (&'a str, KeyTail<'a>) {
    let graph = workspace_doc::editor_graph_ref(&doc.workspace.ws).map_or("@default", |g| g.name.as_str());
    let origin = |path: &[usize]| -> Option<&'a Origin> { doc.shell.as_ref().and_then(|s| s.origins.iter().find(|(p, _)| p == path).map(|(_, o)| o)) };
    // a binding several leaves are made from names none of them: each is keyed by its place
    let once = |name: &str| !doc.shell.as_ref().is_some_and(|s| s.repeated.iter().any(|r| r == name));
    if let Some(Origin::Bound(name)) = origin(path) {
        if once(name) {
            return (graph, KeyTail::Name(name));
        }
    }
    // [name (ui/floating (ui/inspector))]: the window is the float's binding
    if let Some((0, up)) = path.split_last().map(|(last, up)| (*last, up)) {
        let is_float = doc.shell.as_ref().and_then(|s| panels::at_ref(up, &s.tree)).is_some_and(|t| matches!(t, panels::Panels::Float(_)));
        if is_float {
            if let Some(Origin::Bound(name)) = origin(up) {
                return (graph, KeyTail::Name(name));
            }
        }
    }
    (graph, KeyTail::Place(path))
}

pub fn panel_key(doc: &Document, path: &[usize]) -> Vec<String> {
    match panel_key_ref(doc, path) {
        (graph, KeyTail::Name(name)) => vec![graph.to_string(), name.to_string()],
        (graph, KeyTail::Place(path)) => [graph.to_string(), "@panel".to_string()].into_iter().chain(path.iter().map(|i| path_index_text(*i))).collect(),
    }
}

/// `key == panel_key(doc, path)` without building the key (layout reads it many times a frame).
fn key_is(key: &[String], (graph, tail): &(&str, KeyTail)) -> bool {
    let index_is = |text: &str, i: usize| {
        let mut buf = [0u8; 20];
        let mut w = &mut buf[..];
        let _ = std::io::Write::write_fmt(&mut w, format_args!("{i}"));
        let n = 20 - w.len();
        if i == HOST_BAR { text == "-1" } else { text.as_bytes() == &buf[..n] }
    };
    match (key, tail) {
        ([g, name], KeyTail::Name(n)) => g == graph && name == n,
        ([g, panel, rest @ ..], KeyTail::Place(path)) => g == graph && panel == "@panel" && rest.len() == path.len() && rest.iter().zip(path.iter()).all(|(t, i)| index_is(t, *i)),
        _ => false,
    }
}

pub fn panel_state<'a>(value: &ModelView<'a>, path: &[usize]) -> panels::State {
    // ponytail: linear scan of the saved panel states (a handful); key the map by borrowed parts if it grows
    let saved = |key: &(&str, KeyTail)| value.doc.workspace.ws.layout.panels.iter().find(|(k, _)| key_is(k, key)).map(|(_, s)| *s);
    let state = if value.workspace.restored {
        panels::default_state()
    } else {
        let key = panel_key_ref(&value.doc, path);
        match saved(&key) {
            Some(state) => state,
            None => {
                // a leaf of a binding used twice starts from the binding's entry (flow.md 11.11)
                let origin = value.doc.shell.as_ref().and_then(|s| s.origins.iter().find(|(p, _)| p == path).map(|(_, o)| o));
                let by_name = match (origin, &key) {
                    (Some(Origin::Bound(name)), (graph, KeyTail::Place(_))) => saved(&(graph, KeyTail::Name(name))),
                    _ => None,
                };
                by_name.unwrap_or(panels::State { collapsed: path == [HOST_BAR], ..panels::default_state() })
            }
        }
    };
    match &value.workspace.window_live {
        Some((p, window)) if p == path => panels::State { window: Some(*window), ..state },
        _ => state,
    }
}

/// An edit that changes the editor graph's tree ends "Restore layout".
pub fn unrestore(before: &Document, after: &Document, mut shell: shell) -> shell {
    let tree = |doc: &Document| doc.shell.as_ref().map(|s| s.tree.clone());
    if tree(before) != tree(after) {
        shell.restored = false;
        shell.window_live = None;
    }
    shell
}

/// the host bar above the panels: a document with an editor graph has one
pub fn shell_hidden<'a>(value: &ModelView<'a>, shell: &shell) -> Vec<Panel> {
    if shell.restored {
        shell.hidden.clone()
    } else if panel_state(value, &[HOST_BAR]).collapsed && !panels::each_leaf(&shell_tree(value, shell), &mut |_, p| *p == Panel::Timeline) {
        vec![Panel::Timeline]
    } else {
        Vec::new()
    }
}

pub fn geometry<'a>(value: &ModelView<'a>, shell: &shell, frame: &frame::Frame) -> layout::Geometry {
    let state = |path: &[usize]| panel_state(value, path);
    let hidden = shell_hidden(value, shell);
    layout::geometry(Some(&state), Some(&hidden), &shell_tree(value, shell), frame)
}

pub fn has_panel<'a>(value: &ModelView<'a>, panel: &Panel) -> bool {
    visible(&value.workspace, panel)
        && panels::each_leaf(&shell_tree(value, &value.workspace), &mut |path, p| p == panel && !panel_state(value, path).collapsed)
}

/// The viewport that keyboard focus and the camera follow: the focused one, else the first.
pub fn active_view<'a>(value: &ModelView<'a>, g: &layout::Geometry) -> Option<layout::Leaf> {
    match &value.focus {
        Panel::View(_) if layout::find(g, &value.focus).is_some() => layout::find(g, &value.focus),
        _ => layout::first_view(g),
    }
}

pub fn panes<'a>(value: &ModelView<'a>, frame: &frame::Frame) -> layout::Panes {
    panes_in(value, &geometry(value, &value.workspace, frame))
}

/// `panes` of a geometry already laid out.
pub fn panes_in<'a>(value: &ModelView<'a>, g: &layout::Geometry) -> layout::Panes {
    let panes = layout::panes(g);
    match active_view(value, g) {
        Some(leaf) => layout::Panes { view: leaf.body, ..panes },
        None => panes,
    }
}

/// The viewports on screen and the bodies they are drawn in.
pub fn view_bodies<'a>(value: &ModelView<'a>, frame: &frame::Frame) -> Vec<(String, layout::Bounds)> {
    view_bodies_in(value, &geometry(value, &value.workspace, frame))
}

/// `view_bodies` of a geometry already laid out.
pub fn view_bodies_in<'a>(value: &ModelView<'a>, g: &layout::Geometry) -> Vec<(String, layout::Bounds)> {
    g.leaves
        .iter()
        .filter_map(|leaf| match &leaf.panel {
            Panel::View(key) if !panel_state(value, &leaf.path).collapsed => Some((key.clone(), leaf.body)),
            _ => None,
        })
        .collect()
}

pub fn view_visible<'a>(value: &ModelView<'a>) -> bool {
    panels::each_leaf(&shell_tree(value, &value.workspace), &mut |path, p| match p {
        Panel::View(_) => visible(&value.workspace, p) && !panel_state(value, path).collapsed,
        _ => false,
    })
}

// ---- graph panels: each [ui/graph] leaf is a pane of its own ----

/// The graph leaves of the tree shown, by panel key: walked once a frame (`follow_graph`).
pub fn graph_leaves<'a>(value: &ModelView<'a>) -> Vec<(Vec<String>, layout::Path)> {
    panels::leaves(&shell_tree(value, &value.workspace)).into_iter().filter(|(_, p)| *p == Panel::Graph).map(|(path, _)| (panel_key(&value.doc, &path), path)).collect()
}

/// The leaf of the pane in use: where its panel was last seen when that is still its leaf (no
/// walk: this is read many times a frame), else found again, else the first graph leaf.
pub fn graph_path<'a>(value: &ModelView<'a>) -> Option<layout::Path> {
    if value.key == panel_map::key_ref(&value.model.panes) {
        if let (Some(key), Some(path)) = (value.key, panel_map::leaf(&value.model.panes)) {
            let tree = shell_tree(&value, &value.workspace);
            if matches!(panels::at_ref(&path, &tree), Some(panels::Panels::Leaf(Panel::Graph))) && key_is(key, &panel_key_ref(&value.doc, &path)) {
                return Some(path);
            }
        }
    }
    let leaves = graph_leaves(value);
    value.key.and_then(|key| leaves.iter().find(|(k, _)| k == key).map(|(_, path)| path.clone()))
        .or_else(|| leaves.into_iter().next().map(|(_, path)| path))
}

/// What a graph leaf's own keywords open it with: its `:view`, else the canvas when it pins a graph.
pub fn start_views(doc: &Document, level: Level, path: &[usize]) -> Level_map {
    match &doc.shell {
        None => level_map_empty(),
        Some(s) => match s.start.graph_views.iter().find(|(p, _)| p == path).map(|(_, v)| v.as_str()) {
            Some("list") => level_map_singleton(level, projection::List_view),
            Some("text") => level_map_singleton(level, projection::Text_view),
            Some(_) => level_map_singleton(level, projection::Graph_view),
            None => {
                if s.named.iter().any(|(p, _)| p == path) {
                    level_map_singleton(level, projection::Graph_view)
                } else {
                    level_map_empty()
                }
            }
        },
    }
}

fn new_pane(doc: &Document, at: Level, path: &[usize]) -> graph_pane {
    graph_pane { view: scope::create(None, None), laid: None, shown: None, route: Vec::new(), views: start_views(doc, at, path), at, picked: selection::empty() }
}

/// The pane's graph state, borrowed unless it is new or its level disappeared.
pub fn pane_at<'a>(value: &ModelView<'a>, (key, path): (&Vec<String>, &layout::Path)) -> Cow<'a, graph_pane> {
    if value.key == Some(key) { return Cow::Borrowed(value.pane); }
    let mut pane = match panel_map::find(&value.model.panes, key) {
        Some(pane) => Cow::Borrowed(pane),
        None => Cow::Owned(new_pane(&value.doc, pane_level(value), path)),
    };
    let at = editor_document::document::resolve_level(&value.doc, pane.at);
    if at != pane.at {
        let pane = pane.to_mut();
        pane.picked = selection::empty();
        pane.at = at;
    }
    pane
}

/// The graph panel's synchronized state, borrowed from the frame's model.
pub fn as_pane<'a>(value: &ModelView<'a>, (key, _path): (&Vec<String>, &layout::Path)) -> ModelView<'a> {
    if value.key == Some(key) { return *value; }
    let (key, pane) = panel_map::entry(&value.model.panes, key).expect("graph panes are synchronized before frame construction");
    ModelView { pane, key: Some(key), selection: &pane.picked, ..*value }
}

/// `as_pane` of a model owned: no copy.
pub fn with_pane_in_use(value: &mut EditorModel, (key, path): (&Vec<String>, &layout::Path)) {
    if panel_map::key(&value.panes).as_ref() == Some(key) {
        return;
    }
    let level = pane_level(&(&*value).into());
    let doc = value.doc.clone();
    panel_map::activate(&mut value.panes, key, &|| new_pane(&doc, level, path));
    panel_map::set_leaf(&mut value.panes, Some(path.clone()));
    let pane = panel_map::active_mut(&mut value.panes);
    let at = editor_document::document::resolve_level(&doc, pane.at);
    if at != pane.at {
        pane.picked = selection::empty();
        pane.at = at;
    }
}

/// The graph leaf an inspector, list or lisp leaf is tied to by `:of`.
pub fn tied<'a>(value: &ModelView<'a>, path: &[usize]) -> Option<(Vec<String>, layout::Path)> {
    if value.workspace.restored {
        return None;
    }
    value.doc.shell.as_ref().and_then(|s| s.follows.iter().find(|(p, _)| p == path).map(|(_, target)| (panel_key(&value.doc, target), target.clone())))
}

/// The pane in use follows the focus: the focused graph leaf, else the one it was, else the first
/// one open.  The pane left keeps its canvas, selection, graph and route.
pub fn follow_graph(value: &mut EditorModel, focus: &Panel, focus_path: Option<&[usize]>) {
    let all = graph_leaves(&(&*value).into());
    let at = panel_map::key(&value.panes).and_then(|key| all.iter().find(|(k, _)| *k == key).map(|(_, p)| p.clone()));
    panel_map::set_leaf(&mut value.panes, at);
    let leaves: Vec<(Vec<String>, layout::Path)> = all.iter().filter(|(_, path)| !panel_state(&(&*value).into(), path).collapsed).cloned().collect();
    // the focused graph leaf, or the one the focused leaf is tied to: its edits land there
    let focused = focus_path.and_then(|path| {
        let path = if *focus == Panel::Graph { Some(path.to_vec()) } else { tied(&(&*value).into(), path).map(|(_, p)| p) };
        path.and_then(|path| leaves.iter().find(|(_, p)| *p == path).cloned())
    });
    let current = panel_map::key(&value.panes);
    let target = match (&focused, &current) {
        (Some(leaf), _) => Some(leaf.clone()),
        (None, Some(key)) if leaves.iter().any(|(k, _)| k == key) => None,
        (None, _) => leaves.first().cloned(),
    };
    match (target, current) {
        (None, _) => {},
        (Some((key, _)), Some(current)) if key == current => {},
        (Some((key, path)), None) => {
            // the first pane: the state so far is its
            panel_map::adopt(&mut value.panes, &key);
            panel_map::set_leaf(&mut value.panes, Some(path));
        }
        (Some((key, path)), Some(_)) => {
            // the pane left keeps its canvas, selection, graph and route in the map
            with_pane_in_use(value, (&key, &path));
            let keys: Vec<Vec<String>> = all.iter().map(|(k, _)| k.clone()).collect();
            panel_map::retain(&mut value.panes, &|key| keys.contains(key));
        }
    }
}

// The inspector column's kit panel.
// A window is the input sheet: its panes paint their ground with the theme's panel colour, so they
// are built with the sheet as that colour.
pub fn sheet_theme(theme: &Theme) -> Theme {
    let mut sheet = theme.clone();
    sheet.panel = theme.input;
    sheet.ink_ground = Some(theme.ink_ground.unwrap_or(theme.panel));
    sheet
}

pub fn on_sheet(ui: &mut Ui, floating: bool, build: &mut dyn FnMut(&mut Ui)) {
    if !floating {
        build(ui);
        return;
    }
    let theme = ui.theme();
    ui.set_theme(sheet_theme(&theme));
    build(ui);
    ui.set_theme(theme);
}

pub fn inspector_panel(window: bool, ui: &mut Ui, bounds: layout::Bounds, build: &mut dyn FnMut(&mut Ui)) {
    let (x, y, width, height) = bounds;
    ui.panel(
        PanelOpts { window, x: x as f64, y: y as f64, width: width as f64, height: Some(height as f64), padding: 0, ..PanelOpts::default() },
        "workspace-inspector-panel",
        build,
    );
}

/// The inspector rows of a scene object or a World layer: its fields, a vec3 in one row.
pub fn object_rows(locked: bool, fields: &[param::FieldView]) -> Result<Vec<FlowRow>, Diagnostic> {
    flow_sop::port::parameters(true, fields).map(|ps| {
        ps.into_iter().map(|parameter| FlowRow { path: parameter.path, fields: parameter.fields, shown: false, locked, drive: None, live: None }).collect()
    })
}
