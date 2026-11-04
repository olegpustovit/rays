//! The model's shell functions: the layouts, panel keys and states, the geometry, graph panels and
//! the inspector panel. Port of lib/rays_editor/editor_shell.ml.
//!
//! Functions that return an updated model take it by value (OCaml `{ value with .. }`).
#![allow(non_camel_case_types)]
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
pub fn layouts(value: &t) -> Vec<(String, bool)> {
    if let Some(sw) = value.doc.shell.as_ref().and_then(|s| s.switch.as_ref()) {
        return panels::labels(&sw.layouts).into_iter().enumerate().map(|(i, name)| (name, i as i64 == sw.active)).collect();
    }
    let doc = &value.doc.workspace.ws;
    let current = workspace_doc::editor_graph(doc).map(|g| g.name);
    let gs: Vec<_> = doc.checked.graphs.iter().filter(|g| g.context == context::editor()).collect();
    if gs.len() <= 1 {
        return Vec::new();
    }
    gs.iter().map(|g| (g.name.clone(), Some(&g.name) == current.as_ref())).collect()
}

/// What the text pane completes beyond its own text: the graphs, the materials, the cameras of the
/// first scene graph and the layouts.
pub fn completion_names(value: &t) -> lisp_text::Names {
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
pub fn shell_tree(value: &t, shell: &shell) -> panels::t {
    let base = if shell.restored {
        shell.tree.clone()
    } else {
        match &value.doc.shell {
            Some(s) => s.tree.clone(),
            None => shell.tree.clone(),
        }
    };
    match &shell.live {
        Some((node, size)) => panels::set_size(node, *size, base),
        None => base,
    }
}

pub fn visible(shell: &shell, panel: &Panel) -> bool {
    !shell.hidden.contains(panel)
}

pub fn panel_key(doc: &Document, path: &[usize]) -> Vec<String> {
    let graph = workspace_doc::editor_graph(&doc.workspace.ws).map(|g| g.name).unwrap_or_else(|| "@default".to_string());
    let origin = |path: &[usize]| -> Option<&Origin> { doc.shell.as_ref().and_then(|s| s.origins.iter().find(|(p, _)| p == path).map(|(_, o)| o)) };
    // a binding several leaves are made from names none of them: each is keyed by its place
    let once = |name: &str| !doc.shell.as_ref().is_some_and(|s| s.repeated.iter().any(|r| r == name));
    if let Some(Origin::Bound(name)) = origin(path) {
        if once(name) {
            return vec![graph, name.clone()];
        }
    }
    // [name (ui/floating (ui/inspector))]: the window is the float's binding
    if let Some((0, up)) = path.split_last().map(|(last, up)| (*last, up)) {
        let is_float = doc.shell.as_ref().and_then(|s| panels::at(up, &s.tree)).is_some_and(|t| matches!(t, panels::t::Float(_)));
        if is_float {
            if let Some(Origin::Bound(name)) = origin(up) {
                return vec![graph, name.clone()];
            }
        }
    }
    let mut key = vec![graph, "@panel".to_string()];
    key.extend(path.iter().map(|i| path_index_text(*i)));
    key
}

pub fn panel_state(value: &t, path: &[usize]) -> panels::State {
    let saved = |key: &Vec<String>| value.doc.workspace.ws.layout.panels.get(key).copied();
    let state = if value.workspace.restored {
        panels::default_state()
    } else {
        match saved(&panel_key(&value.doc, path)) {
            Some(state) => state,
            None => {
                // a leaf of a binding used twice starts from the binding's entry (flow.md 11.11)
                let origin = value.doc.shell.as_ref().and_then(|s| s.origins.iter().find(|(p, _)| p == path).map(|(_, o)| o));
                let key = panel_key(&value.doc, path);
                let by_name = match (origin, key.as_slice()) {
                    (Some(Origin::Bound(name)), [graph, second, ..]) if second == "@panel" => saved(&vec![graph.clone(), name.clone()]),
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
pub fn shell_hidden(value: &t, shell: &shell) -> Vec<Panel> {
    if shell.restored {
        shell.hidden.clone()
    } else if panel_state(value, &[HOST_BAR]).collapsed && !panels::leaves(&shell_tree(value, shell)).iter().any(|(_, p)| *p == Panel::Timeline) {
        vec![Panel::Timeline]
    } else {
        Vec::new()
    }
}

pub fn geometry(value: &t, shell: &shell, frame: &frame::t) -> layout::Geometry {
    let state = |path: &[usize]| panel_state(value, path);
    let hidden = shell_hidden(value, shell);
    layout::geometry(Some(&state), Some(&hidden), &shell_tree(value, shell), frame)
}

pub fn has_panel(value: &t, panel: &Panel) -> bool {
    visible(&value.workspace, panel)
        && panels::leaves(&shell_tree(value, &value.workspace)).iter().any(|(path, p)| p == panel && !panel_state(value, path).collapsed)
}

/// The viewport that keyboard focus and the camera follow: the focused one, else the first.
pub fn active_view(value: &t, g: &layout::Geometry) -> Option<layout::Leaf> {
    match &value.focus {
        Panel::View(_) if layout::find(g, &value.focus).is_some() => layout::find(g, &value.focus),
        _ => layout::first_view(g),
    }
}

pub fn panes(value: &t, frame: &frame::t) -> layout::Panes {
    let g = geometry(value, &value.workspace, frame);
    let panes = layout::panes(&g);
    match active_view(value, &g) {
        Some(leaf) => layout::Panes { view: leaf.body, ..panes },
        None => panes,
    }
}

/// The viewports on screen and the bodies they are drawn in.
pub fn view_bodies(value: &t, frame: &frame::t) -> Vec<(String, layout::Bounds)> {
    geometry(value, &value.workspace, frame)
        .leaves
        .iter()
        .filter_map(|leaf| match &leaf.panel {
            Panel::View(key) if !panel_state(value, &leaf.path).collapsed => Some((key.clone(), leaf.body)),
            _ => None,
        })
        .collect()
}

pub fn view_visible(value: &t) -> bool {
    panels::leaves(&shell_tree(value, &value.workspace)).iter().any(|(path, p)| match p {
        Panel::View(_) => visible(&value.workspace, p) && !panel_state(value, path).collapsed,
        _ => false,
    })
}

// ---- graph panels: each [ui/graph] leaf is a pane of its own ----

/// The graph leaves of the tree shown, by panel key: walked once a frame (`follow_graph`).
pub fn graph_leaves(value: &t) -> Vec<(Vec<String>, layout::Path)> {
    panels::leaves(&shell_tree(value, &value.workspace)).into_iter().filter(|(_, p)| *p == Panel::Graph).map(|(path, _)| (panel_key(&value.doc, &path), path)).collect()
}

/// The leaf of the pane in use: where its panel was last seen when that is still its leaf (no
/// walk: this is read many times a frame), else found again, else the first graph leaf.
pub fn graph_path(value: &t) -> Option<layout::Path> {
    if let (Some(key), Some(path)) = (panel_map::key(&value.panes), panel_map::leaf(&value.panes)) {
        let tree = shell_tree(value, &value.workspace);
        if matches!(panels::at(&path, &tree), Some(panels::t::Leaf(Panel::Graph))) && panel_key(&value.doc, &path) == key {
            return Some(path);
        }
    }
    let leaves = graph_leaves(value);
    match panel_map::key(&value.panes).and_then(|key| leaves.iter().find(|(k, _)| *k == key).map(|(_, p)| p.clone())) {
        Some(path) => Some(path),
        None => leaves.into_iter().next().map(|(_, path)| path),
    }
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

/// The editor as the graph panel `key` at `path` has it: its own pane is the one in use.  A pane
/// made now starts at the level of the one it replaces.  The document may have changed under a
/// pane that waited (an undo from another panel): its level is resolved again, and a level that is
/// gone takes its selection with it.
pub fn as_pane(value: &t, (key, path): (&Vec<String>, &layout::Path)) -> t {
    let mut value = value.clone();
    if panel_map::key(&value.panes).as_ref() == Some(key) {
        return value;
    }
    let level = pane_level(&value);
    let views = start_views(&value.doc, level, path);
    let create = move || graph_pane { view: scope::create(None, None), laid: None, shown: None, route: Vec::new(), views: views.clone(), at: level, picked: selection::empty() };
    let doc = value.doc.clone();
    let panes = panel_map::activate(key, &create, value.panes);
    let panes = panel_map::set_leaf(Some(path.clone()), panes);
    let panes = panel_map::update(
        &|pane: &graph_pane| {
            let at = editor_document::document::resolve_level(&doc, pane.at);
            graph_pane { at, picked: if at == pane.at { pane.picked.clone() } else { selection::empty() }, ..pane.clone() }
        },
        panes,
    );
    value.panes = panes;
    value
}

/// The graph leaf an inspector, list or lisp leaf is tied to by `:of`.
pub fn tied(value: &t, path: &[usize]) -> Option<(Vec<String>, layout::Path)> {
    if value.workspace.restored {
        return None;
    }
    value.doc.shell.as_ref().and_then(|s| s.follows.iter().find(|(p, _)| p == path).map(|(_, target)| (panel_key(&value.doc, target), target.clone())))
}

/// The pane in use follows the focus: the focused graph leaf, else the one it was, else the first
/// one open.  The pane left keeps its canvas, selection, graph and route.
pub fn follow_graph(mut value: t, focus: &Panel, focus_path: Option<&[usize]>) -> t {
    let all = graph_leaves(&value);
    let at = panel_map::key(&value.panes).and_then(|key| all.iter().find(|(k, _)| *k == key).map(|(_, p)| p.clone()));
    value.panes = panel_map::set_leaf(at, value.panes);
    let leaves: Vec<(Vec<String>, layout::Path)> = all.iter().filter(|(_, path)| !panel_state(&value, path).collapsed).cloned().collect();
    // the focused graph leaf, or the one the focused leaf is tied to: its edits land there
    let focused = focus_path.and_then(|path| {
        let path = if *focus == Panel::Graph { Some(path.to_vec()) } else { tied(&value, path).map(|(_, p)| p) };
        path.and_then(|path| leaves.iter().find(|(_, p)| *p == path).cloned())
    });
    let current = panel_map::key(&value.panes);
    let target = match (&focused, &current) {
        (Some(leaf), _) => Some(leaf.clone()),
        (None, Some(key)) if leaves.iter().any(|(k, _)| k == key) => None,
        (None, _) => leaves.first().cloned(),
    };
    match (target, current) {
        (None, _) => value,
        (Some((key, _)), Some(current)) if key == current => value,
        (Some((key, path)), None) => {
            // the first pane: the state so far is its
            let panes = panel_map::adopt(&key, value.panes);
            value.panes = panel_map::set_leaf(Some(path), panes);
            value
        }
        (Some((key, path)), Some(_)) => {
            // the pane left keeps its canvas, selection, graph and route in the map
            let mut value = as_pane(&value, (&key, &path));
            let keys: Vec<Vec<String>> = all.iter().map(|(k, _)| k.clone()).collect();
            value.panes = panel_map::retain(&|key| keys.contains(key), value.panes);
            value
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
