//! Shell helpers return updated owned models for layouts, panel state, geometry and inspectors.

use flow_syntax::symbol::Symbol;
use std::borrow::Cow;
use editor_core::panels::{self, Panel};
use editor_document::document::{Level, Origin, T as Document};
use editor_document::workspace_doc;
use flow_syntax::context;
use pxui::theme::Tokens as Theme;
use pxui::ui::{Ui};
use rays_graph::scope_state as scope;
use rays_shell::inspector::FlowRow;
use rays_shell::layout;
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

/// The authored tree, or the host tree when the document has none or is restored.
pub fn shell_tree<'a>(value: &'a EditorModel, shell: &'a Shell) -> Cow<'a, panels::Panels> {
    Cow::Borrowed(match &value.doc.shell {
        Some(s) if !shell.restored => &s.tree,
        _ => &shell.tree,
    })
}

pub fn visible(shell: &Shell, panel: &Panel) -> bool {
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

pub fn panel_key(doc: &Document, path: &[usize]) -> flow_check::workspace::Path {
    match panel_key_ref(doc, path) {
        (graph, KeyTail::Name(name)) => vec![graph.into(), name.into()],
        (graph, KeyTail::Place(path)) => [graph.into(), "@panel".into()].into_iter().chain(path.iter().map(|i| path_index_text(*i).into())).collect(),
    }
}

/// `key == panel_key(doc, path)` without building the key (layout reads it many times a frame).
fn key_is(key: &[Symbol], (graph, tail): &(&str, KeyTail)) -> bool {
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
pub fn unrestore(before: &Document, after: &Document, mut shell: Shell) -> Shell {
    fn tree(doc: &Document) -> Option<&panels::Panels> { doc.shell.as_ref().map(|s| &s.tree) }
    if tree(before) != tree(after) {
        shell.restored = false;
        shell.window_live = None;
    }
    shell
}

/// the host bar above the panels: a document with an editor graph has one
pub fn shell_hidden<'a>(value: &ModelView<'_>, shell: &'a Shell) -> &'a [Panel] {
    if shell.restored {
        &shell.hidden
    } else if panel_state(value, &[HOST_BAR]).collapsed && !shell_leaves(value, shell).iter().any(|(_, panel)| *panel == Panel::Timeline) {
        &[Panel::Timeline]
    } else {
        &[]
    }
}

/// What `geometry` read: a frame lays the shell out many times, so the last answer is kept
/// while every input compares equal.
struct GeoKey {
    size: (i64, i64),
    shell_doc: Option<std::sync::Arc<editor_document::document::Shell>>,
    states: std::collections::BTreeMap<flow_check::workspace::Path, panels::State>,
    graph: String,
    workspace: Shell,
}

thread_local! { static GEOMETRY: std::cell::RefCell<Option<(GeoKey, std::rc::Rc<layout::Geometry>)>> = const { std::cell::RefCell::new(None) }; }

impl GeoKey {
    fn matches(&self, value: &ModelView<'_>, shell: &Shell, graph: &str) -> bool {
        self.graph == graph
            && match (&self.shell_doc, &value.doc.shell) { (Some(a), Some(b)) => std::sync::Arc::ptr_eq(a, b), (None, None) => true, _ => false }
            && self.states == value.doc.workspace.ws.layout.panels
            && self.workspace.restored == shell.restored
            && self.workspace.tree == shell.tree
            && self.workspace.hidden == shell.hidden
            && self.workspace.window_live == shell.window_live
    }
}

/// The grip signal belongs to the previous layout. Its split extent can be read even if this
/// frame will change the live size or the window dimensions.
// ponytail: reuse the one-slot memo. Keep a previous layout per editor if a UI thread starts
// alternating workspaces during a capture.
pub fn last_geometry(value: &ModelView<'_>) -> Option<std::rc::Rc<layout::Geometry>> {
    let graph = workspace_doc::editor_graph_ref(&value.doc.workspace.ws).map_or("@default", |g| g.name.as_str());
    GEOMETRY.with(|cache| cache.borrow().as_ref().filter(|(k, _)| k.matches(value, value.workspace, graph)).map(|(_, g)| g.clone()))
}

pub fn geometry<'a>(value: &ModelView<'a>, shell: &Shell, frame: &frame::Frame) -> std::rc::Rc<layout::Geometry> {
    let ws = &value.doc.workspace.ws;
    let graph = workspace_doc::editor_graph_ref(ws).map_or("@default", |g| g.name.as_str());
    let hit = GEOMETRY.with(|cache| cache.borrow().as_ref().and_then(|(k, g)| {
        (k.size == frame.input.size && k.workspace.live == shell.live && k.matches(value, shell, graph)).then(|| g.clone())
    }));
    if let Some(g) = hit {
        return g;
    }
    let view = ModelView { workspace: shell, ..*value };
    let state = |path: &[usize]| panel_state(&view, path);
    let hidden = shell_hidden(&view, shell);
    let g = std::rc::Rc::new(layout::geometry_with_resize(Some(&state), Some(hidden), &shell_tree(value, shell), frame, shell.live.as_ref()));
    let key = GeoKey { size: frame.input.size, shell_doc: value.doc.shell.clone(), states: ws.layout.panels.clone(), graph: graph.to_owned(), workspace: shell.clone() };
    GEOMETRY.with(|cache| *cache.borrow_mut() = Some((key, g.clone())));
    g
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

type Leaves = std::rc::Rc<Vec<(layout::Path, Panel)>>;
type GraphLeaves = std::rc::Rc<Vec<(panel_map::Key, layout::Path)>>;

thread_local! {
    static LEAVES: std::cell::RefCell<Option<(panels::Panels, Leaves)>> = const { std::cell::RefCell::new(None) };
    static GRAPH_LEAVES: std::cell::RefCell<Option<(Leaves, Option<std::sync::Arc<editor_document::document::Shell>>, String, GraphLeaves)>> = const { std::cell::RefCell::new(None) };
}

/// `panels::leaves` of the tree shown, kept while the tree is the same: a frame reads it many times.
pub fn shell_leaves(value: &EditorModel, shell: &Shell) -> Leaves {
    let tree = shell_tree(value, shell);
    if let Some(found) = LEAVES.with(|cache| cache.borrow().as_ref().filter(|(seen, _)| *seen == *tree).map(|(_, leaves)| leaves.clone())) {
        return found;
    }
    let leaves = std::rc::Rc::new(panels::leaves(&tree));
    LEAVES.with(|cache| *cache.borrow_mut() = Some((tree.into_owned(), leaves.clone())));
    leaves
}

/// The graph leaves, with the panel keys retained while the layout gives them the same identity.
pub fn graph_leaves(value: &ModelView<'_>) -> GraphLeaves {
    let leaves = shell_leaves(value, value.workspace);
    let shell = &value.doc.shell;
    let graph = workspace_doc::editor_graph_ref(&value.doc.workspace.ws).map_or("@default", |g| g.name.as_str());
    GRAPH_LEAVES.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some((previous_leaves, previous_shell, previous_graph, keys)) = cache.as_ref() {
            let same_shell = match (previous_shell, shell) {
                (Some(a), Some(b)) => std::sync::Arc::ptr_eq(a, b),
                (None, None) => true,
                _ => false,
            };
            if std::rc::Rc::ptr_eq(previous_leaves, &leaves) && same_shell && previous_graph == graph {
                return keys.clone();
            }
        }
        let keys: GraphLeaves = std::rc::Rc::new(leaves.iter().filter(|(_, panel)| *panel == Panel::Graph)
            .map(|(path, _)| (panel_key(&value.doc, path), path.clone())).collect());
        *cache = Some((leaves, shell.clone(), graph.to_owned(), keys.clone()));
        keys
    })
}

/// The leaf of the pane in use: where its panel was last seen when that is still its leaf (no
/// walk: this is read many times a frame), else found again, else the first graph leaf.
pub fn graph_path<'a>(value: &ModelView<'a>) -> Option<layout::Path> {
    if let Some(path) = graph_path_known(value) {
        return Some(path.clone());
    }
    let leaves = graph_leaves(value);
    value.key.and_then(|key| leaves.iter().find(|(k, _)| k == key).map(|(_, path)| path.clone()))
        .or_else(|| leaves.first().map(|(_, path)| path.clone()))
}

/// `graph_path` when the pane's remembered leaf still holds it: borrowed, no walk.
pub fn graph_path_known<'a>(value: &ModelView<'a>) -> Option<&'a layout::Path> {
    if value.key == panel_map::key_ref(&value.model.panes) {
        if let (Some(key), Some(path)) = (value.key, panel_map::leaf(&value.model.panes)) {
            let tree = shell_tree(&value, &value.workspace);
            if matches!(panels::at_ref(path, &tree), Some(panels::Panels::Leaf(Panel::Graph))) && key_is(key, &panel_key_ref(&value.doc, path)) {
                return Some(path);
            }
        }
    }
    None
}

/// What a graph leaf's own keywords open it with: its `:view`, else the canvas when it pins a graph.
pub fn start_views(doc: &Document, level: Level, path: &[usize]) -> LevelMap {
    match &doc.shell {
        None => level_map_empty(),
        Some(s) => match s.start.graph_views.iter().find(|(p, _)| p == path).map(|(_, v)| v.as_str()) {
            Some("list") => level_map_singleton(level, Projection::ListView),
            Some("text") => level_map_singleton(level, Projection::TextView),
            Some(_) => level_map_singleton(level, Projection::GraphView),
            None => {
                if s.named.iter().any(|(p, _)| p == path) {
                    level_map_singleton(level, Projection::GraphView)
                } else {
                    level_map_empty()
                }
            }
        },
    }
}

fn new_pane(doc: &Document, at: Level, path: &[usize]) -> GraphPane {
    GraphPane { view: scope::create(None, None), laid: None, shown: None, route: Vec::new(), views: start_views(doc, at, path), at, picked: selection::empty() }
}

/// The pane's graph state, borrowed unless it is new or its level disappeared.
pub fn pane_at<'a>(value: &ModelView<'a>, (key, path): (&flow_check::workspace::Path, &layout::Path)) -> Cow<'a, GraphPane> {
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
pub fn as_pane<'a>(value: &ModelView<'a>, (key, _path): (&flow_check::workspace::Path, &layout::Path)) -> ModelView<'a> {
    if value.key == Some(key) { return *value; }
    let (key, pane) = panel_map::entry(&value.model.panes, key).expect("graph panes are synchronized before frame construction");
    ModelView { pane, key: Some(key), selection: &pane.picked, ..*value }
}

/// `as_pane` of a model owned: no copy.
pub fn with_pane_in_use(value: &mut EditorModel, (key, path): (&flow_check::workspace::Path, &layout::Path)) {
    if panel_map::key_ref(&value.panes) == Some(key) {
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
pub fn tied(value: &ModelView<'_>, path: &[usize]) -> Option<(flow_check::workspace::Path, layout::Path)> {
    tied_path(value, path).map(|target| (panel_key(&value.doc, target), target.clone()))
}

fn tied_path<'a>(value: &ModelView<'a>, path: &[usize]) -> Option<&'a layout::Path> {
    if value.workspace.restored {
        return None;
    }
    value.doc.shell.as_ref().and_then(|s| s.follows.iter().find(|(p, _)| p == path).map(|(_, target)| target))
}

/// The pane in use follows the focus: the focused graph leaf, else the one it was, else the first
/// one open. The pane left keeps its canvas, selection, graph and route.
pub fn follow_graph(value: &mut EditorModel, focus: &Panel, focus_path: Option<&[usize]>) {
    let all = graph_leaves(&(&*value).into());
    let current = panel_map::key_ref(&value.panes);
    let current_leaf = current.and_then(|key| all.iter().find(|(k, _)| k == key).map(|(_, path)| path));
    if panel_map::leaf(&value.panes) != current_leaf {
        panel_map::set_leaf(&mut value.panes, current_leaf.cloned());
    }
    let vw = ModelView::from(&*value);
    let focused = focus_path.and_then(|path| {
        if *focus == Panel::Graph { Some(path) } else { tied_path(&vw, path).map(Vec::as_slice) }
    });
    let mut open = all.iter().filter(|(_, path)| !panel_state(&vw, path).collapsed);
    let target = focused.and_then(|path| open.clone().find(|(_, p)| p == path)).or_else(|| {
        if panel_map::key_ref(&value.panes).is_some_and(|key| open.clone().any(|(k, _)| k == key)) { None } else { open.next() }
    });
    if let Some((key, path)) = target {
        match panel_map::key_ref(&value.panes) {
            None => {
                panel_map::adopt(&mut value.panes, key);
                panel_map::set_leaf(&mut value.panes, Some(path.clone()));
            }
            Some(current) if current == key => {}
            Some(_) => with_pane_in_use(value, (key, path)),
        }
    }
}

fn same_panel_kind(a: &Panel, b: &Panel) -> bool {
    match (a, b) {
        (Panel::View(_), Panel::View(_)) | (Panel::Canvas(_), Panel::Canvas(_)) => true,
        _ => a == b,
    }
}

/// Panel key renames caused by a same-layout edit (split, unsplit/undo, default-shell
/// materialization, float): `(old_key, new_key, old_path, new_path)`. Returns empty on a layout
/// switch or when the shell is unchanged.
pub fn panel_renames(
    before: &Document,
    before_tree: &panels::Panels,
    after: &Document,
    after_tree: &panels::Panels,
) -> Vec<(panel_map::Key, panel_map::Key, layout::Path, layout::Path)> {
    if match (&before.shell, &after.shell) {
        (Some(a), Some(b)) => std::sync::Arc::ptr_eq(a, b),
        (None, None) => true,
        _ => false,
    } {
        return Vec::new();
    }
    // Switching between editor graphs or switch layouts is not a key rename (B6 keeps each layout's panes).
    let before_graph = workspace_doc::editor_graph_ref(&before.workspace.ws).map(|g| g.name.as_str());
    let after_graph = workspace_doc::editor_graph_ref(&after.workspace.ws).map(|g| g.name.as_str());
    if let (Some(a), Some(b)) = (before_graph, after_graph) {
        if a != b {
            return Vec::new();
        }
    }
    if let (Some(a), Some(b)) = (
        before.shell.as_ref().and_then(|s| s.switch.as_ref()),
        after.shell.as_ref().and_then(|s| s.switch.as_ref()),
    ) {
        if a.active != b.active {
            return Vec::new();
        }
    }
    let old_leaves: Vec<(layout::Path, Panel, panel_map::Key)> = panels::leaves(before_tree)
        .into_iter()
        .map(|(p, panel)| {
            let k = panel_key(before, &p);
            (p, panel, k)
        })
        .collect();
    let new_leaves: Vec<(layout::Path, Panel, panel_map::Key)> = panels::leaves(after_tree)
        .into_iter()
        .map(|(p, panel)| {
            let k = panel_key(after, &p);
            (p, panel, k)
        })
        .collect();
    let disappeared: Vec<usize> = (0..old_leaves.len())
        .filter(|&i| !new_leaves.iter().any(|(_, _, nk)| *nk == old_leaves[i].2))
        .collect();
    let appeared: Vec<usize> = (0..new_leaves.len())
        .filter(|&j| !old_leaves.iter().any(|(_, _, ok)| *ok == new_leaves[j].2))
        .collect();
    if disappeared.is_empty() || appeared.is_empty() {
        return Vec::new();
    }
    let mut used_old = vec![false; old_leaves.len()];
    let mut used_new = vec![false; new_leaves.len()];
    let mut out = Vec::new();
    let mut match_pass = |pred: &dyn Fn(&[usize], &[usize]) -> bool| {
        for &oi in &disappeared {
            if used_old[oi] {
                continue;
            }
            let (ref op, ref opanel, ref okey) = old_leaves[oi];
            if let Some(&ni) = appeared.iter().find(|&&j| !used_new[j] && same_panel_kind(opanel, &new_leaves[j].1) && pred(op, &new_leaves[j].0)) {
                used_old[oi] = true;
                used_new[ni] = true;
                let (ref np, _, ref nkey) = new_leaves[ni];
                out.push((okey.clone(), nkey.clone(), op.clone(), np.clone()));
            }
        }
    };
    // 1. Same path (default-shell materialization or in-place binding rename).
    match_pass(&|op, np| op == np);
    // 2. Split: an inline leaf at `op` moved to `op + [0]` and got a binding.
    match_pass(&|op, np| np.len() == op.len() + 1 && np[..op.len()] == *op && np[op.len()] == 0);
    // 3. Unsplit / undo of split / sibling closed: `op` was a direct child of `np`.
    match_pass(&|op, np| op.len() == np.len() + 1 && op[..np.len()] == *np);
    // 4. Float / unfloat / dock where a unique disappeared leaf of a kind matches one appeared leaf.
    for &oi in &disappeared {
        if used_old[oi] {
            continue;
        }
        let (ref op, ref opanel, ref okey) = old_leaves[oi];
        let old_of_kind = disappeared.iter().filter(|&&i| !used_old[i] && same_panel_kind(&old_leaves[i].1, opanel)).count();
        let mut new_of_kind = appeared.iter().copied().filter(|&j| !used_new[j] && same_panel_kind(&new_leaves[j].1, opanel));
        if old_of_kind == 1 {
            if let (Some(ni), None) = (new_of_kind.next(), new_of_kind.next()) {
                used_old[oi] = true;
                used_new[ni] = true;
                let (ref np, _, ref nkey) = new_leaves[ni];
                out.push((okey.clone(), nkey.clone(), op.clone(), np.clone()));
            }
        }
    }
    out
}

/// Carry graph panes, panel locals, active host keys, focus path and start-keyword memory across
/// panel key renames produced by a same-layout edit (B5).
pub fn remap_panels(value: &mut EditorModel, result: &mut FrameResult, before: &Document, before_tree: &panels::Panels) {
    let after_tree = shell_tree(value, &value.workspace).into_owned();
    let renames = panel_renames(before, before_tree, &value.doc, &after_tree);
    let focus = result.focus_path.as_ref().and_then(|path| match panels::at_ref(path, before_tree) {
        Some(panels::Panels::Leaf(panel)) => Some((panel, panel_key_ref(before, path))),
        _ => None,
    });
    // a pane key that appears (a split reusing the name a closed half freed) starts with fresh state
    let old_keys: Vec<panel_map::Key> = panels::leaves(before_tree).into_iter().map(|(p, _)| panel_key(before, &p)).collect();
    let mut focused = None;
    for (p, panel) in panels::leaves(&after_tree) {
        let key = panel_key(&value.doc, &p);
        if focus.as_ref().is_some_and(|(old, old_key)| same_panel_kind(old, &panel) && key_is(&key, old_key)) {
            focused = Some(p);
        }
        if !old_keys.contains(&key) && !renames.iter().any(|(_, new, _, _)| *new == key) {
            value.locals.retain(|(k, _)| *k != key);
            result.locals.retain(|(k, _)| *k != key);
        }
    }
    if let Some(path) = focused {
        result.focus_path = Some(path);
    }
    if renames.is_empty() {
        return;
    }
    let rename_opt = |slot: &mut Option<panel_map::Key>, old: &panel_map::Key, new: &panel_map::Key| {
        if slot.as_ref() == Some(old) {
            *slot = Some(new.clone());
        }
    };
    let rename_locals = |locals: &mut Vec<(panel_map::Key, Local)>, old: &panel_map::Key, new: &panel_map::Key| {
        if let Some(pos) = locals.iter().position(|(k, _)| k == old) {
            let (_, local) = locals.remove(pos);
            locals.retain(|(k, _)| k != new);
            locals.insert(pos.min(locals.len()), (new.clone(), local));
        }
    };
    for (old_key, new_key, old_path, new_path) in renames {
        panel_map::rename(&mut value.panes, &old_key, &new_key, Some(new_path.clone()));
        for (k, _) in &mut result.graph_panes {
            if *k == old_key {
                *k = new_key.clone();
            }
        }
        rename_locals(&mut value.locals, &old_key, &new_key);
        rename_locals(&mut result.locals, &old_key, &new_key);
        rename_opt(&mut value.list_at, &old_key, &new_key);
        rename_opt(&mut value.text_at, &old_key, &new_key);
        rename_opt(&mut value.outline_at, &old_key, &new_key);
        if result.focus_path.as_ref() == Some(&old_path) {
            result.focus_path = Some(new_path);
        }
        let started = std::rc::Rc::make_mut(&mut value.started);
        rename_opt(&mut started.on, &old_key, &new_key);
        for (k, _) in &mut started.views {
            if *k == old_key {
                *k = new_key.clone();
            }
        }
        for (k, _) in &mut started.tabs {
            if *k == old_key {
                *k = new_key.clone();
            }
        }
    }
}

// The inspector column's kit panel.
// A window is the input sheet: its panes paint their ground with the theme's panel colour, so they
// are built with the sheet as that colour.
pub fn sheet_theme(theme: &Theme) -> Theme {
    let mut sheet = theme.clone();
    sheet.panel = theme.input;
    sheet.ink_ground = Some(theme.ink_ground.get().unwrap_or(theme.panel)).into();
    sheet
}

pub fn on_sheet(ui: &mut Ui, floating: bool, build: &mut dyn FnMut(&mut Ui)) {
    if !floating {
        build(ui);
        return;
    }
    // the sheet theme is derived once per token change, not per call
    thread_local! { static SHEET: std::cell::RefCell<Option<(Theme, std::rc::Rc<pxui::ThemeHandle>)>> = const { std::cell::RefCell::new(None) }; }
    let tokens = ui.tokens();
    let sheet = SHEET.with(|cell| {
        let mut cell = cell.borrow_mut();
        match cell.as_ref() {
            Some((from, sheet)) if *from == tokens => sheet.clone(),
            _ => {
                let sheet = std::rc::Rc::new(pxui::ThemeHandle::from_tokens(sheet_theme(&tokens)));
                *cell = Some((tokens, sheet.clone()));
                sheet
            }
        }
    });
    ui.themed(&sheet, build);
}

pub fn inspector_panel(window: bool, ui: &mut Ui, bounds: layout::Bounds, build: &mut dyn FnMut(&mut Ui)) {
    let (x, y, width, height) = bounds;
    let overlay = pxui::Overlay::DEFAULT.at(pxui::Point::new(x as f32, y as f32))
        .w_px(width as f32).h_px(height as f32).padding(0.0);
    let overlay = if window { overlay.window() } else { overlay };
    ui.panel("workspace-inspector-panel", &overlay, build);
}

/// The inspector rows of a scene object or a World layer: its fields, a vec3 in one row.
pub fn object_rows(locked: bool, fields: &[param::FieldView]) -> Result<Vec<FlowRow>, Diagnostic> {
    flow_sop::port::parameters(true, fields).map(|ps| {
        ps.into_iter().map(|parameter| FlowRow { path: parameter.path, fields: parameter.fields, shown: false, locked, drive: None, live: None }).collect()
    })
}
