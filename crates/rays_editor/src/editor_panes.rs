// Preserve the existing pane intent priority when dispatching edits.
//! Graph projections and panel instances draw list/text/outline contents and return per-panel intents after construction.


use flow_syntax::symbol::Symbol;
use std::borrow::Cow;
use std::rc::Rc;

use editor_core::panels::{self as panels, Panel};
use editor_document::document::{self as doc_mod, Level};
use editor_document::kind_graph;
use flow_graph::flow_edit::{self as fe, ArgKey};
use flow_graph::probe;
use flow_graph::projection as p;
use flow_sop::lower;
use flow_syntax::context;
use flow_syntax::syntax::{Node as SNode, Syntax};
use pxui::ui::Ui;
use pxui::ui::UiBox;
use rays_graph::node_menu;


use rays_graph::scope_state as scope;
use rays_shell::kit::{self, ButtonOpts};
use rays_shell::layout::{self, Leaf};
use rays_shell::timeline_bar;
use rays_shell::tree;
use rays::frame;

use crate::bars;
use crate::cook;
use crate::editor_commands::Action;
use crate::editor_inspect::{compiled_at, compiled_node, kind, probes_of, spreadsheet_source};
use crate::editor_model::*;
use crate::editor_shell::*;
use crate::navigator;
use crate::panel_map;
use crate::pick;
use crate::selection;
use crate::spreadsheet;
use crate::text_pane;
use crate::timeline;

type S = Syntax;
pub type Key = flow_check::workspace::Path;

pub fn stored_projection<'a>(value: &ModelView<'a>) -> Projection {
    projection_in(value, pane(value))
}

fn projection_in(value: &EditorModel, pane: &GraphPane) -> Projection {
    match level_map_find(&pane.views, pane.at) {
        Some(p) => p,
        None => match pane.at {
            Level::Scene => ListView,
            Level::Inside(id) if kind(value, id).as_deref() == Some("geometry") => GraphView,
            Level::Inside(_) => ListView,
        },
    }
}

/// Any of the panels that hold the graph pane's views is on screen.
pub fn graph_family<'a>(value: &ModelView<'a>) -> bool {
    has_panel(value, &Panel::Graph) || has_panel(value, &Panel::List) || has_panel(value, &Panel::Lisp)
}

/// What the graph panel shows: the view it says, whatever other panels the layout has.
pub fn projection_of<'a>(value: &ModelView<'a>) -> Projection {
    stored_projection(value)
}

/// Which keys the focused panel takes: the list's and the text pane's own panels, else the graph
/// panel's projection.
pub fn listing<'a>(value: &ModelView<'a>) -> bool {
    *value.focus == Panel::List || (*value.focus != Panel::Lisp && projection_of(value) == ListView)
}
pub fn texting<'a>(value: &ModelView<'a>) -> bool {
    *value.focus == Panel::Lisp || (*value.focus != Panel::List && projection_of(value) == TextView)
}

// ---- panel instances: every leaf draws, with its own list, text pane or outline ----

pub fn fresh_local() -> Local {
    Local { rows: tree::create(), code: text_pane::initial(), nav: navigator::initial(), sheet_owner: 0 }
}

pub fn local_of(locals: &[(Key, Local)], key: &Key) -> Local {
    match locals.iter().find(|(k, _)| k == key) {
        Some((_, l)) => l.clone(),
        None => fresh_local(),
    }
}

/// The panels' own states are kept for the 64 panels last drawn or changed (the latest first): a
/// bound on what layouts that came and went leave behind.
pub const LOCALS_CAPACITY: usize = 64;

pub fn put_local(key: &Key, f: &dyn Fn(Local) -> Local, locals: &[(Key, Local)]) -> Vec<(Key, Local)> {
    let current = match locals.iter().find(|(k, _)| k == key) {
        Some((_, l)) => l.clone(),
        None => fresh_local(),
    };
    let mut out = vec![(key.clone(), f(current))];
    out.extend(locals.iter().filter(|(k, _)| k != key).take(LOCALS_CAPACITY - 1).cloned());
    out
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostKind {
    List,
    Text,
    Outline,
}

/// A panel that holds a list, a text pane or an outline: its key, leaf path, panel and what it holds.
pub type Host = (Key, layout::Path, Panel, HostKind);

/// The open panels that hold a list, a text pane or an outline: a graph panel holds the one its
/// view shows.
pub fn panel_hosts<'a>(value: &ModelView<'a>) -> Vec<Host> {
    let mut out = Vec::new();
    for (path, panel) in shell_leaves(value, &value.workspace).iter() {
        if !visible(&value.workspace, panel) || panel_state(value, path).collapsed {
            continue;
        }
        let key = panel_key(&value.doc, &path);
        let kind = match &panel {
            Panel::List => Some(HostKind::List),
            Panel::Lisp => Some(HostKind::Text),
            Panel::Outline => Some(HostKind::Outline),
            Panel::Graph => match projection_in(value, &pane_at(value, (&key, &path))) {
                ListView => Some(HostKind::List),
                TextView => Some(HostKind::Text),
                GraphView => None,
            },
            Panel::View(_) | Panel::Canvas(_) | Panel::Inspector | Panel::Spreadsheet | Panel::Timeline => None,
        };
        if let Some(kind) = kind {
            out.push((key, path.clone(), panel.clone(), kind));
        }
    }
    out
}

/// The graph state a leaf reads, with its resolved graph path, without copying the editor model.
pub fn shown_pane<'a>(value: &ModelView<'a>, (key, path, panel): (&Key, &layout::Path, &Panel)) -> (Cow<'a, GraphPane>, Option<layout::Path>) {
    let at = |key: &Key, path: &layout::Path| {
        let resolved = value.doc.shell.as_ref().and_then(|_| {
            if value.key == Some(key) { graph_path(value) } else { Some(path.clone()) }
        });
        (pane_at(value, (key, path)), resolved)
    };
    if *panel == Panel::Graph { at(key, path) } else {
        match tied(value, path) {
            Some((key, path)) => at(&key, &path),
            None => (Cow::Borrowed(value.pane), value.doc.shell.as_ref().and_then(|_| graph_path(value))),
        }
    }
}

/// The synchronized graph pane a leaf follows, borrowed with its shared model.
pub fn shown_as<'a>(value: &ModelView<'a>, (key, path, panel): (&Key, &layout::Path, &Panel)) -> ModelView<'a> {
    if *panel == Panel::Graph { as_pane(value, (key, path)) } else {
        match tied(value, path) {
            Some((key, path)) => as_pane(value, (&key, &path)),
            None => *value,
        }
    }
}

/// `tree`, `text` and `outline` follow the focus like the graph pane does: each belongs to the
/// focused panel of its kind, else to the one it was, else to the first one open; the panel left
/// keeps its own in `locals`.
pub fn follow_hosts(value: &mut EditorModel, focus_path: Option<&layout::Path>, hosts: &[Host]) {
    let focused = focus_path.map(|p| panel_key(&value.doc, p));
    for kind in [HostKind::List, HostKind::Text, HostKind::Outline] {
        let current = match kind {
            HostKind::List => value.list_at.clone(),
            HostKind::Text => value.text_at.clone(),
            HostKind::Outline => value.outline_at.clone(),
        };
        let keys: Vec<&Key> = hosts.iter().filter(|(_, _, _, k)| *k == kind).map(|(key, ..)| key).collect();
        let target = match (&focused, &current) {
            (Some(key), _) if keys.contains(&key) => Some(key.clone()),
            (_, Some(key)) if keys.contains(&key) => current.clone(),
            _ => keys.first().map(|k| (*k).clone()),
        };
        let Some(key) = target else { continue };
        if current.as_ref() == Some(&key) { continue; }
        if let Some(old) = &current {
            value.locals = put_local(old, &|l| match kind {
                    HostKind::List => Local { rows: value.tree.clone(), ..l },
                    HostKind::Text => Local { code: value.text.clone(), ..l },
                    HostKind::Outline => Local { nav: value.outline.clone(), ..l },
            }, &value.locals);
        }
        match kind {
            HostKind::List => {
                value.list_at = Some(key.clone());
                if current.is_some() { value.tree = local_of(&value.locals, &key).rows; }
            }
            HostKind::Text => {
                value.text_at = Some(key.clone());
                if current.is_some() { value.text = local_of(&value.locals, &key).code; }
            }
            HostKind::Outline => {
                value.outline_at = Some(key.clone());
                if current.is_some() { value.outline = local_of(&value.locals, &key).nav; }
            }
        }
    }
}

/// The start keywords of the document, by panel key.
pub fn start_of(doc: &doc_mod::T) -> Started {
    match &doc.shell {
        None => Started { on: None, views: vec![], tabs: vec![] },
        Some(s) => {
            let keyed = |l: &Vec<(panels::Path, String)>| -> Vec<(Key, String)> { l.iter().map(|(path, v)| (panel_key(doc, path), v.clone())).collect() };
            Started { on: s.start.focus.as_ref().map(|p| panel_key(doc, p)), views: keyed(&s.start.graph_views), tabs: keyed(&s.start.tabs) }
        }
    }
}

/// The editor follows a start keyword when its value in the text changed (flow.md 11.11): on
/// opening, a reload, a preset, an edit, undo. What did not change is left as the user has it.
pub fn follow_start(value: &mut EditorModel) {
    let now = start_of(&value.doc);
    let was = value.started.clone();
    if now == *was {
        return;
    }
    let leaves = shell_leaves(&*value, &value.workspace);
    let path_of = |value: &EditorModel, key: &Key| -> Option<(layout::Path, Panel)> {
        leaves.iter().find(|(path, _)| panel_key(&value.doc, path) == *key).cloned()
    };
    let changed = |now: &[(Key, String)], was: &[(Key, String)]| -> Vec<(Key, String)> {
        now.iter().filter(|(key, v)| was.iter().find(|(k, _)| k == key).map(|(_, w)| w) != Some(v)).cloned().collect()
    };
    if let Some(key) = &now.on {
        if now.on != was.on {
            if let Some((path, panel)) = path_of(&*value, key) {
                value.focus = panel;
                value.focus_path = Some(path);
            }
        }
    }
    for (key, view) in changed(&now.views, &was.views) {
        let view = match view.as_str() {
            "list" => ListView,
            "text" => TextView,
            _ => GraphView,
        };
        if panel_map::key(&value.panes).as_ref() == Some(&key) {
            { let pane = pane_mut(value); pane.views = level_map_add(pane.views.clone(), pane.at, view); };
        } else if let Some((path, _)) = path_of(&*value, &key) {
            let pane = pane_at(&(&*value).into(), (&key, &path)).into_owned();
            let pane = GraphPane { views: level_map_add(pane.views.clone(), pane.at, view), ..pane };
            panel_map::set(&mut value.panes, &key, pane);
        }
    }
    for (key, tab) in changed(&now.tabs, &was.tabs) {
        let tab = match tab.as_str() {
            "graph" => text_pane::Tab::Graph,
            "document" => text_pane::Tab::Document,
            _ => text_pane::Tab::Selection,
        };
        if value.text_at.as_ref() == Some(&key) || value.text_at.is_none() {
            value.text.tab = tab;
        } else {
            value.locals = put_local(&key, &|l| Local { code: text_pane::State { tab, ..l.code.clone() }, ..l }, &value.locals);
        }
    }
    value.started = Rc::new(now);
}

/// The workspace graph the pane shows with zones and selectors: the one a `(ui/graph "name")` panel
/// names, else the one the outline picked, else the graph of the geometry object opened (the one
/// its network was lowered from, else the `sop` graph its label names), of the World opened, or the
/// scene graph at the scene level. A level without such a graph shows its list only.
/// The lowered sop graph a geometry object instantiates, by its network.
pub fn graph_of_object(value: &EditorModel, id: i64) -> Option<String> {
    doc_mod::object_graph(&value.doc, id)
}

pub fn graph_name<'a>(value: &ModelView<'a>) -> Option<String> {
    let owned;
    let path = match value.doc.shell.as_ref().map(|_| graph_path_known(value)) {
        Some(None) => { owned = graph_path(value); owned.as_deref() }
        Some(Some(path)) => Some(path.as_slice()),
        None => None,
    };
    graph_name_at(&value.doc, pane_level(value), pane_shown(value).map(String::as_str), path)
}

pub fn graph_name_at(doc: &doc_mod::T, level: Level, shown: Option<&str>, path: Option<&[usize]>) -> Option<String> {
    let ws = &doc.workspace.ws;
    let exists = |name: &str| -> bool {
        ws.checked.graphs.iter().any(|g| g.name == name)
            || (name.len() > 4 && &name[..4] == "def:" && ws.checked.defs.iter().any(|g| format!("def:{}", g.name) == name))
    };
    // the graph the pane's own leaf pins
    let named = || doc.shell.as_ref().and_then(|s| path.and_then(|path| s.named.iter().find(|(p, _)| p.as_slice() == path).map(|(_, n)| n.as_str())));
    let chosen = shown.or_else(named);
    let kind = |id| doc_mod::scene_graph(doc).into_iter().find(|node| node.id == id).map(|node| node.kind);
    match (chosen, level) {
        (Some(name), _) if exists(name) => Some(name.to_owned()),
        (_, Level::Inside(id)) if kind(id).as_deref() == Some("geometry") => match doc_mod::object_graph(doc, id) {
            Some(name) => Some(name),
            None => kind_graph::find(&doc_mod::scene_graph(doc), id).and_then(|node| if exists(&node.label) { Some(node.label.clone()) } else { None }),
        },
        (_, Level::Inside(id)) if kind(id).as_deref() == Some("world") => {
            ws.checked.graphs.iter().find_map(|g| if g.context == context::world() { Some(g.name.clone()) } else { None })
        }
        (_, Level::Scene) => ws.checked.graphs.iter().find_map(|g| if g.context == context::scene() { Some(g.name.clone()) } else { None }),
        _ => None,
    }
}

/// For the crash report and tests: the graph of every graph panel, and the floating windows.
pub fn graph_panels<'a>(value: &ModelView<'a>) -> String {
    graph_leaves(value)
        .iter()
        .map(|(key, path)| {
            let pane = pane_at(value, (key, path));
            let path = if value.key == Some(key) { graph_path(value) } else { Some(path.clone()) };
            graph_name_at(&value.doc, pane.at, pane.shown.as_deref(), path.as_deref()).unwrap_or_else(|| "-".to_owned())
        })
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn windows<'a>(value: &ModelView<'a>) -> String {
    shell_leaves(value, &value.workspace)
        .iter()
        .filter_map(|(path, panel)| panel_state(value, path).window.map(|(x, y, w, h)| format!("{} {} {} {} {}", panels::name(panel).to_lowercase(), x, y, w, h)))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Every panel by its key's last name, the focused one starred, with what is its own: a text
/// pane's tab and draft, a list's focus, folds and filter, an outline's search.
pub fn panels_line<'a>(value: &ModelView<'a>) -> String {
    let hosts = panel_hosts(value);
    shell_leaves(value, &value.workspace)
        .iter()
        .map(|(path, panel)| {
            let key = panel_key(&value.doc, path);
            let name = format!(
                "{} {}{}",
                panels::name(panel).to_lowercase(),
                key.last().map_or("", |name| name.as_str()),
                if *value.focus == *panel && value.focus_path.as_ref() == Some(path) { "*" } else { "" }
            );
            let own = hosts.iter().find_map(|(k, p_, _, kind)| if *k == key && p_ == path { Some(*kind) } else { None });
            match own {
                Some(HostKind::Text) => format!("{}: {}", name, text_pane::summary(&if Some(&key) == value.text_at.as_ref() { value.text.clone() } else { local_of(&value.locals, &key).code })),
                Some(HostKind::List) => format!("{}: {}", name, tree::summary(&if Some(&key) == value.list_at.as_ref() { value.tree.clone() } else { local_of(&value.locals, &key).rows })),
                Some(HostKind::Outline) => format!(
                    "{}: search {:?}",
                    name,
                    navigator::query(&if Some(&key) == value.outline_at.as_ref() { value.outline.clone() } else { local_of(&value.locals, &key).nav })
                ),
                None => name,
            }
        })
        .collect::<Vec<_>>()
        .join("; ")
}

/// Follow a reference: the pane shows `graph`, and `u` comes back to what it showed.
pub fn remember(value: &mut EditorModel) {
    {
        let pane = pane_mut(value);
        pane.route.truncate(31);
        pane.route.insert(0, (pane.at, pane.shown.clone()));
    };
}

pub fn go(value: &mut EditorModel, graph: &str) {
    if graph_name(&(&*value).into()).as_deref() != Some(graph) {
        remember(value);
        panel_map::active_mut(&mut value.panes).shown = Some(graph.to_owned());
    }
}

fn reference(ws_graphs: &[flow_check::workspace::Graph], e: &S) -> Option<String> {
    if let SNode::List(items) = e.node {
        if let [head, name] = e.range(items) {
            if let (SNode::Sym(r), SNode::Sym(name)) = (e.arena().row(*head).node, e.arena().row(*name).node) {
                let name = e.name(name);
                if e.name(r) == "ref" && ws_graphs.iter().any(|g| g.name == name) {
                    return Some(name.to_owned());
                }
            }
        }
    }
    None
}

/// What the selected node of the graph pane references: the graph its `(ref name)` names, the
/// `:material` of a sop/material first (a material node follows to its surface).
pub fn follow_target<'a>(value: &ModelView<'a>, path: Option<&[Symbol]>) -> Option<String> {
    let ws = &value.doc.workspace.ws;
    let graphs: &[flow_check::workspace::Graph] = &ws.checked.graphs;
    let selected: Vec<Key> = match path {
        Some(p_) => vec![p_.to_vec()],
        None => scope::selected(pane_view(value)),
    };
    match selected.as_slice() {
        [path] if graph_name(value).is_some() => {
            let arg = |key: ArgKey| fe::arg_text(&ws.source, path, &key).and_then(|e| reference(graphs, &e));
            if let Some(found) = arg(ArgKey::Kw("material".to_owned())) {
                return Some(found);
            }
            match fe::arg_text(&ws.source, path, &ArgKey::Whole) {
                Some(form) if matches!(form.node, SNode::List(_)) => {
                    if let Some(head) = flow_syntax::syntax::head(&form) {
                        if ws.checked.defs.iter().any(|d| d.name == head) {
                            return Some(format!("def:{head}"));
                        }
                    }
                    form.children().get(1..).and_then(|args| args.iter().find_map(|&id| reference(graphs, &form.child(id))))
                },
                _ => None,
            }
        }
        _ => None,
    }
}

/// `follow_target`, else the graph of the object selected in the list.
pub fn peek_target<'a>(value: &ModelView<'a>) -> Option<String> {
    if let Some(found) = follow_target(value, None) {
        return Some(found);
    }
    selection::selected(pane_selection(value)).and_then(|id| if kind(value, id).as_deref() == Some("geometry") { graph_of_object(value, id) } else { None })
}

/// `i` in a viewport with nothing selected follows to the scene it shows: the graph its
/// `(ref scene)` names, else the first scene graph.
pub fn viewport_target<'a>(value: &ModelView<'a>) -> Option<String> {
    let ws = &value.doc.workspace.ws;
    match &value.focus {
        Panel::View(key) if selection::selected(pane_selection(value)).is_none() => {
            let named = value.doc.shell.as_ref().and_then(|shell| {
                shell.preview_sources.iter().find(|(k, _)| k == key).and_then(|(_, src)| match &src.scene_ref {
                    Some(form) if flow_syntax::syntax::head(form) == Some("ref") => {
                        match form.children() {
                            [_, name] => match form.arena().row(*name).node {
                                SNode::Sym(name) => Some(form.name(name).to_owned()),
                                _ => None,
                            },
                            _ => None,
                        }
                    },
                    _ => None,
                })
            });
            named.or_else(|| ws.checked.graphs.iter().find_map(|g| if g.context == context::scene() { Some(g.name.clone()) } else { None }))
        }
        _ => None,
    }
}

/// The route taken, as the graph header shows it: scene > shards > cobalt (the last three).
pub fn route<'a>(value: &ModelView<'a>) -> String {
    let path = value.doc.shell.as_ref().and_then(|_| graph_path(value));
    route_in(value, pane(value), path.as_deref())
}

pub fn route_in(value: &EditorModel, pane: &GraphPane, path: Option<&[usize]>) -> String {
    let mut names: Vec<String> = pane.route
        .iter()
        .rev()
        .filter_map(|(at, shown)| graph_name_at(&value.doc, *at, shown.as_deref(), path))
        .collect();
    names.extend(graph_name_at(&value.doc, pane.at, pane.shown.as_deref(), path));
    let skip = names.len().saturating_sub(3);
    names.into_iter().skip(skip).collect::<Vec<_>>().join(" > ")
}

pub fn scope_name<'a>(value: &ModelView<'a>) -> Option<String> {
    if projection_of(value) == GraphView { graph_name(value) } else { None }
}

/// The lowered node of the node selected in the graph pane, at the iteration its zones probe.
pub fn scope_node<'a>(value: &ModelView<'a>) -> Option<(Key, sop::node::T)> {
    let laid = pane_laid(value)?;
    let records = laid.records.as_ref()?;
    let selected = scope::selected(pane_view(value));
    let [path] = selected.as_slice() else { return None };
    scope_name(value)?;
    let n = p::find(&laid.scope, path)?;
    let node_id = compiled_at(value, &probe::chains(&laid.scope), records, &n.path)?;
    compiled_node(value, node_id).map(|node| (n.path.clone(), node))
}

/// The tags the selected node highlights at the probes, with the cache to keep.
pub fn lit_tags<'a>(value: &ModelView<'a>) -> (pick::Set, Option<LitCache>) {
    let lowered = &value.doc.workspace.lowered;
    let selected = scope::selected(pane_view(value));
    match (pane_laid(value), selected.as_slice()) {
        (Some(laid), [site]) if scope_name(value).is_some() => {
            if let Some(c) = &value.lit {
                if c.site == *site && c.at == value.probes && std::sync::Arc::ptr_eq(&c.evaluated, &lowered.evaluated) && c.scope == *laid.scope {
                    return (c.tags.clone(), value.lit.clone());
                }
            }
            let iter = probes_of(value, &probe::chains(&laid.scope), site);
            let tags: pick::Set = lower::tags(lowered, site, &iter).into_iter().collect();
            (tags.clone(), Some(LitCache { site: site.clone(), at: value.probes.clone(), evaluated: lowered.evaluated.clone(), scope: (*laid.scope).clone(), tags }))
        }
        _ => (pick::Set::new(), None),
    }
}

/// The nodes of the pane's graph that the last failed cook names, each with its diagnostic's code
/// (the pane draws the sheet's failed state on them).
pub fn failed_nodes<'a>(value: &ModelView<'a>) -> Vec<(Key, String)> {
    let failed = cook::failed_node(&value.cook);
    match (failed, pane_laid(value)) {
        (Some((code, node_id)), Some(laid)) if scope_name(value).is_some() && laid.records.is_some() => {
            fn nodes(s: &p::Scope, out: &mut Vec<p::Node>) {
                for n in &s.nodes {
                    out.push(n.clone());
                    if let Some(z) = &n.zone {
                        nodes(&z.scope, out);
                    }
                }
            }
            let mut all = Vec::new();
            nodes(&laid.scope, &mut all);
            let chains = probe::chains(&laid.scope);
            let records = laid.records.as_ref().unwrap();
            all.iter().filter(|n| compiled_at(value, &chains, records, &n.path) == Some(node_id)).map(|n| (n.path.clone(), code.clone())).collect()
        }
        _ => Vec::new(),
    }
}

/// The lists of every list host: the in-use one draws the live rows, the others their cached ones.
/// Returns the tree, the intents and the locals.
#[allow(clippy::too_many_arguments)]
pub fn draw_list_panes<'a>(
    ui: &mut Ui,
    shortcut_frame: &frame::Frame,
    value: &ModelView<'a>,
    hosted: &[Host],
    root_at: &dyn Fn(&layout::Path) -> Option<(Leaf, UiBox)>,
    pane_of: &dyn Fn(&Host) -> ModelView<'a>,
    in_use: &dyn Fn(&ModelView<'a>) -> bool,
    title_of: &dyn Fn(&ModelView<'a>) -> String,
    rows: &[tree::Row],
    columns: &[String],
    host_rows: &[(Key, (Vec<tree::Row>, Vec<String>))],
    selected: &dyn Fn() -> Vec<i64>,
    tree_state: tree::Tree,
    intents: Vec<tree::Intent>,
    locals: Vec<(Key, Local)>,
) -> (tree::Tree, Vec<tree::Intent>, Vec<(Key, Local)>) {
    let (mut tree_state, mut intents, mut locals) = (tree_state, intents, locals);
    for host in hosted {
        let (key, path, _, _) = host;
        let Some((leaf, root)) = root_at(path) else { continue };
        let pane = pane_of(host);
        let mine = in_use(&pane);
        let (rows_now, columns_now): (Vec<tree::Row>, Vec<String>) = if mine {
            (rows.to_vec(), columns.to_vec())
        } else {
            host_rows.iter().find(|(k, _)| k == key).map(|(_, v)| v.clone()).expect("host rows")
        };
        let selected_now: Vec<i64> = if mine { selected() } else { selection::selected_nodes(pane_selection(&pane)).to_vec() };
        let title = title_of(&pane);
        let draw = |ui: &mut Ui, mut state: tree::Tree| -> (tree::Tree, Vec<tree::Intent>) {
            let mut emitted = Vec::new();
            on_sheet(ui, leaf.floating, &mut |ui| {
                ui.within(&root, &mut |ui| {
                    tree::update(&mut state, ui, shortcut_frame, leaf.body, &title, &columns_now, &rows_now, &selected_now, &mut emitted);
                });
            });
            (state, emitted)
        };
        if Some(key) == value.list_at.as_ref() {
            let (t_, emitted) = draw(ui, tree_state.clone());
            tree_state = t_;
            if mine {
                intents.extend(emitted);
            }
        } else {
            let (state, emitted) = draw(ui, local_of(&value.locals, key).rows);
            if mine {
                intents.extend(emitted);
            }
            locals = put_local(key, &|l| Local { rows: state.clone(), ..l }, &locals);
        }
    }
    (tree_state, intents, locals)
}

/// The text panes: the one in use and the others, each with what it showed.
#[allow(clippy::too_many_arguments)]
pub fn draw_text_panes(
    ui: &mut Ui,
    frame_: &frame::Frame,
    value: &EditorModel,
    root_at: &dyn Fn(&layout::Path) -> Option<(Leaf, UiBox)>,
    header_slots: &dyn Fn(&Leaf) -> Vec<Option<f32>>,
    text_host: &Option<Host>,
    text_shown: &Option<text_pane::Shown>,
    text: text_pane::State,
    other_texts: Vec<(Key, layout::Path, text_pane::State, text_pane::Shown)>,
    locals: Vec<(Key, Local)>,
) -> (text_pane::State, Vec<text_pane::Intent>, Vec<(Key, Local)>, Vec<(Key, text_pane::Shown, Vec<text_pane::Intent>)>) {
    let names = completion_names(value);
    let vocab = value.lisp_vocab.force();
    let view_text = |ui: &mut Ui, path: &layout::Path, state: &text_pane::State, shown: &text_pane::Shown| -> Vec<text_pane::Intent> {
        match root_at(path) {
            Some((leaf, root)) => {
                let tabs_right = header_slots(&leaf).last().cloned().flatten();
                let mut out = Vec::new();
                ui.within(&root, &mut |ui| {
                    out = text_pane::view(ui, leaf.body, tabs_right, vocab, &names, frame_.input.t, state, shown);
                });
                out
            }
            None => Vec::new(),
        }
    };
    let text_intents = match (text_shown, text_host) {
        (Some(shown), Some((_, path, _, _))) => view_text(ui, path, &text, shown),
        _ => Vec::new(),
    };
    let mut locals = locals;
    let mut texts = Vec::new();
    for (key, path, state, shown) in other_texts {
        locals = put_local(&key, &|l| Local { code: state.clone(), ..l }, &locals);
        let intents = view_text(ui, &path, &state, &shown);
        // Dispatch intents from the latest drawn pane first.
        texts.insert(0, (key, shown, intents));
    }
    (text, text_intents, locals, texts)
}

/// The outline panels.
pub fn draw_outline_panes(
    ui: &mut Ui,
    value: &EditorModel,
    hosted: &[Host],
    root_at: &dyn Fn(&layout::Path) -> Option<(Leaf, UiBox)>,
    params_of: &dyn Fn(&EditorModel) -> navigator::Params,
    outline: navigator::State,
    locals: Vec<(Key, Local)>,
) -> (navigator::State, Vec<navigator::Intent>, Vec<(Key, Local)>) {
    let (mut outline, mut intents, mut locals) = (outline, Vec::new(), locals);
    for (key, path, _, _) in hosted {
        let Some((leaf, root)) = root_at(path) else { continue };
        let params = params_of(value);
        let draw = |ui: &mut Ui, state: &navigator::State| -> (navigator::State, Vec<navigator::Intent>) {
            let mut result = None;
            on_sheet(ui, leaf.floating, &mut |ui| {
                ui.within(&root, &mut |ui| {
                    result = Some(navigator::view(state, ui, leaf.body, &params));
                });
            });
            result.expect("outline drawn")
        };
        if Some(key) == value.outline_at.as_ref() {
            let (o, emitted) = draw(ui, &outline);
            outline = o;
            intents.extend(emitted);
        } else {
            let (state, emitted) = draw(ui, &local_of(&value.locals, key).nav);
            intents.extend(emitted);
            locals = put_local(key, &|l| Local { nav: state.clone(), ..l }, &locals);
        }
    }
    (outline, intents, locals)
}

/// The spreadsheet panels: each remembers which object's sheet it shows.
pub fn draw_spreadsheet_panes<'a>(ui: &mut Ui, value: &ModelView<'a>, panes: &[(Leaf, UiBox)], locals: Vec<(Key, Local)>) -> Vec<(Key, Local)> {
    let mut locals = locals;
    for (leaf, root) in panes {
        let key = panel_key(&value.doc, &leaf.path);
        let pane = shown_as(value, (&key, &leaf.path, &leaf.panel));
        let owner = local_of(&locals, &key).sheet_owner;
        let mut changed = None;
        ui.within(root, &mut |ui| {
            changed = spreadsheet::view(ui, leaf.body, owner, spreadsheet_source(&pane));
        });
        if let Some(sheet_owner) = changed {
            locals = put_local(&key, &|local_| Local { sheet_owner, ..local_ }, &locals);
        }
    }
    locals
}

/// The keys down this frame that were not down in the last: the edges panes act on.
pub fn pressed_keys(value: &EditorModel, frame_: &frame::Frame) -> Vec<input::Key> {
    frame_.input.keys.iter().filter(|k| !value.held_keys.contains(k)).cloned().collect()
}

/// The graph panes that are not the one in use: those showing a graph update their canvas.
pub fn draw_other_graph_panes<'a>(
    ui: &mut Ui,
    shortcut_frame: &frame::Frame,
    value: &ModelView<'a>,
    other_panes: &[(Key, layout::Path)],
    roots: &[(Leaf, UiBox)],
) -> Vec<(Key, GraphPane)> {
    let mut out = Vec::new();
    for (key, path) in other_panes {
        let pane = as_pane(value, (key, path));
        let mut active = pane.pane.clone();
        match roots.iter().find(|(leaf, _)| leaf.path == *path) {
            Some((leaf, hosted)) if leaf.body.3 > 0 => {
                let (x, y, width, height) = leaf.body;
                if stored_projection(&pane) == GraphView && graph_name(&pane).is_some() {
                    ui.within(hosted, &mut |ui| {
                        let theme = ui.tokens();
                        let theme = if leaf.floating { sheet_theme(&theme) } else { theme };
                        scope::with_guide(false, &mut active.view);
                        scope::with_theme(theme, &mut active.view);
                        scope::with_bounds(x, y, width.max(1), height.max(1), &mut active.view);
                        rays_graph::scope_pane::update(&pressed_keys(value, shortcut_frame), &mut active.view, ui, shortcut_frame, &mut Vec::new());
                    });
                }
                out.push((key.clone(), active));
            }
            _ => out.push((key.clone(), active)),
        }
    }
    out
}

/// What the graph header's tools and views ask this frame.
pub type HeaderAsks = (Option<Action>, Option<Action>, Option<Projection>);

/// The graph header: its toolbar, the Graph / List / Text views, and a viewport's render tabs.
pub fn draw_graph_header<'a>(
    ui: &mut Ui,
    value: &ModelView<'a>,
    g: &layout::Geometry,
    graph_host: &Option<Leaf>,
    header_slots: &dyn Fn(&Leaf) -> Vec<Option<f32>>,
    tools_from: &dyn Fn(&Leaf) -> f32,
) -> HeaderAsks {
    let tool_action = match graph_host {
        Some(leaf) if projection_of(value) == GraphView && has_panel(value, &Panel::Graph) => {
            let tools_scope = scope_name(value).is_some();
            let has_selection = !scope::selected(pane_view(value)).is_empty();
            let enabled = move |tool: bars::Tool| tool == bars::Tool::Outline || (tools_scope && (tool == bars::Tool::Add || has_selection));
            match bars::graph_tools(!has_panel(value, &Panel::Outline), ui, leaf.header, tools_from(leaf), &enabled) {
                Some(bars::Tool::Add) => Some(Action::AddNode),
                Some(bars::Tool::Repeat) => Some(Action::ScopeCommand(scope::Command::WrapRepeat)),
                Some(bars::Tool::Iterate) => Some(Action::ScopeCommand(scope::Command::WrapIterate)),
                Some(bars::Tool::Fn) => Some(Action::ScopeCommand(scope::Command::MakeFn)),
                Some(bars::Tool::Macro) => Some(Action::ScopeCommand(scope::Command::MakeMacro)),
                Some(bars::Tool::Defn) => Some(Action::ScopeCommand(scope::Command::MakeDefn)),
                Some(bars::Tool::Outline) => Some(Action::PanelSplitAt(leaf.path.clone(), panels::Axis::H, 0.7, Panel::Outline)),
                None => None,
            }
        }
        _ => None,
    };
    let view_pick = match graph_host {
        Some(leaf) if has_panel(value, &Panel::Graph) => {
            let (_, hy, _, hh) = leaf.header;
            let active = match projection_of(value) {
                GraphView => 0,
                ListView => 1,
                TextView => 2,
            };
            let slot = header_slots(leaf).first().cloned().flatten();
            slot.and_then(|right| {
                kit::segments(ui, "workspace-view", right, hy as f32 + ((hh - 20) as f32 / 2.0), &["Graph".to_owned(), "List".to_owned(), "Text".to_owned()], active).0
            })
            .map(|i| [GraphView, ListView, TextView][i as usize])
        }
        _ => None,
    };
    let view_action = match (active_view(value, g), &value.view_tools) {
        (Some(leaf), Some((looks, mode))) if leaf.header.3 > 0 => {
            let (hx, hy, hw, hh) = leaf.header;
            let from = hx as f32 + tools_from(&leaf);
            let modes = ["Solid".to_owned(), "Wire".to_owned(), "Traced".to_owned()];
            let seg_w = modes.iter().fold(24.0, |w, label| w + ui.text_width(None, label));
            let look_w = kit::button_width(ui, Some("C"), false, "Look through");
            let ty = hy as f32 + ((hh - 20) as f32 / 2.0);
            let frame_w = kit::button_width(ui, Some("F"), false, "Frame");
            if from + seg_w + 8.0 + look_w + 8.0 + frame_w < (hx + hw - 36) as f32 {
                bars::rule(ui, "workspace-view-rule", leaf.header, from - hx as f32);
                let render = match kit::segments(ui, "workspace-render", from + seg_w, ty, &modes, *mode).0 {
                    Some(i) if i != *mode => Some(Action::RenderMode(i)),
                    _ => None,
                };
                let look = kit::button(ui, "workspace-look", (from + seg_w + 8.0, ty), look_w, &ButtonOpts { active: *looks, hint: Some("C".into()), ..Default::default() }, "Look through");
                let framed = kit::button(ui, "workspace-frame", (from + seg_w + 8.0 + look_w + 8.0, ty), frame_w, &ButtonOpts { hint: Some("F".into()), ..Default::default() }, "Frame");
                if framed {
                    Some(Action::FrameCamera)
                } else if look {
                    Some(Action::LookThrough)
                } else {
                    render
                }
            } else {
                None
            }
        }
        _ => None,
    };
    (tool_action, view_action, view_pick)
}

/// What the graph pane gave back: its body bounds, the menu and the pick.
pub type GraphPaneResult = (layout::Bounds, Option<node_menu::Menu>, Option<String>);

/// Draw the graph pane and its node menu. `open_menu` opens the menu at a point, for the pane's view.
#[allow(clippy::too_many_arguments)]
pub fn draw_graph_pane<'a>(
    ui: &mut Ui,
    frame_: &frame::Frame,
    value: &ModelView<'a>,
    shortcut_frame: &frame::Frame,
    scope_view: &mut scope::Pane,
    scope_active: bool,
    guide: bool,
    carry_lit: Vec<(Key, String)>,
    carry_hot: Option<(Key, bool)>,
    graph_root: &UiBox,
    graph_host: &Option<Leaf>,
    workspace: &Shell,
    menu: Option<node_menu::Menu>,
    open_menu: &dyn Fn(Option<&scope::Pane>, &ModelView<'a>, (i64, i64)) -> Option<node_menu::Menu>,
    scope_frame_changes: &mut Vec<scope::Change>,
) -> GraphPaneResult {
    let graph_body = match graph_host {
        Some(leaf) => leaf.body,
        None => (0, 0, 0, 0),
    };
    let (gx, gy, gw, gh) = graph_body;
    let first = scope_frame_changes.len();
    if !scope_active {
        scope::suspend(scope_view);
    } else {
        let failed = failed_nodes(value);
        let pressed = pressed_keys(value, shortcut_frame);
        ui.within(graph_root, &mut |ui| {
            let theme = ui.tokens();
            let theme = match graph_host {
                Some(leaf) if leaf.floating => sheet_theme(&theme),
                _ => theme,
            };
            scope::with_guide(guide, scope_view);
            scope::with_failed(failed.clone(), scope_view);
            scope::with_theme(theme, scope_view);
            scope::with_bounds(gx, gy, gw.max(1), gh.max(1), scope_view);
            scope::with_carry(carry_lit.clone(), carry_hot.clone(), scope_view);
            rays_graph::scope_pane::update(&pressed, scope_view, ui, shortcut_frame, scope_frame_changes);
        });
    }
    let mut menu = menu;
    if menu.is_none() {
        let at = scope_frame_changes[first..].iter().find_map(|c| match c {
            scope::Change::MenuRequested(x, y) => Some((*x as i64, *y as i64)),
            _ => None,
        });
        if let Some(at) = at {
            menu = open_menu(Some(scope_view), value, at);
        }
    }
    let mut menu_pick = None;
    if let Some(m) = menu.take() {
        let status_y = geometry(value, workspace, frame_).status_at.1;
        let mut result = None;
        ui.within(graph_root, &mut |ui| {
            result = Some(node_menu::update(m.clone(), ui, (0, 0, frame_.input.size.0, status_y)));
        });
        let (next, picked) = result.expect("menu updated");
        menu = next;
        menu_pick = picked;
    }
    (graph_body, menu, menu_pick)
}

/// The timeline panes' bars and the strip's context menu: the playback intents and the layout ones.
pub fn draw_timeline_pane(
    ui: &mut Ui,
    timeline_: &timeline::Timeline,
    max_frame: i64,
    slots: &[(layout::Bounds, UiBox, bool)],
    timeline_root: &Option<(Panel, UiBox)>,
    strip_bounds: layout::Bounds,
) -> (Vec<TimelineIntent>, Vec<rays_shell::chrome::Intent>) {
    let mut intents = Vec::new();
    for (bounds, boxed, edge) in slots {
        if bounds.3 <= 0 {
            continue;
        }
        let playing = timeline::mode(timeline_) == timeline::Mode::Playing;
        ui.within(boxed, &mut |ui| {
            intents.extend(timeline_bar::draw(ui, *bounds, *edge, playing, timeline::frame(timeline_), timeline::time(timeline_), max_frame));
        });
    }
    let mut strip_playback: Vec<TimelineIntent> = Vec::new();
    let mut strip_layout: Vec<rays_shell::chrome::Intent> = Vec::new();
    if let Some((_, boxed)) = timeline_root {
        if strip_bounds.3 > 0 {
            ui.within(boxed, &mut |ui| {
                if let Some((x, y)) = ui.context_at(boxed) {
                    ui.set_text_state(boxed, Some(format!("{} {}", cfmt::g(x as f64, 6), cfmt::g(y as f64, 6))));
                }
                let state = ui.text_state(boxed).map(|s| s.split(' ').map(|x| x.to_owned()).collect::<Vec<_>>());
                if let Some(parts) = state {
                    if let [x, y] = parts.as_slice() {
                        if let (Ok(x), Ok(y)) = (x.parse::<f64>(), y.parse::<f64>()) {
                            let playing = timeline::mode(timeline_) == timeline::Mode::Playing;
                            let items = vec![(if playing { "Pause" } else { "Play" }.to_owned(), true), ("Stop".to_owned(), true), ("Hide timeline".to_owned(), true)];
                            let result = ui.context_menu((x as f32, y as f32), &Default::default(), "timeline-context", &items);
                            if !matches!(result, pxui::ui::MenuResult::Open) {
                                ui.set_text_state(boxed, None);
                                match result {
                                    pxui::ui::MenuResult::Pick(0) => strip_playback.push(TimelineIntent::PauseToggle),
                                    pxui::ui::MenuResult::Pick(1) => strip_playback.push(TimelineIntent::StopPlayback),
                                    pxui::ui::MenuResult::Pick(2) => strip_layout.push(rays_shell::chrome::Intent::Toggle(vec![usize::MAX])),
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            });
        }
    }
    intents.extend(strip_playback);
    (intents, strip_layout)
}
