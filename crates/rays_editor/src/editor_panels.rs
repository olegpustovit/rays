//! Panels: titles, the navigator's inputs, the focused leaf, layout gestures, docking, and the
//! clipboard commands. #![allow(non_camel_case_types)]
use flow_syntax::symbol::Symbol;
use std::rc::Rc;

use editor_core::panels::{self, Axis, Panel};
use editor_document::document::{self, Origin, T as Document};
use editor_document::kind_graph as kg;
use editor_document::layout_by_path::PathMap;
use editor_document::layout_edit as le;
use editor_document::workspace_doc;
use editor_document::{document_edit, objects, panel_kinds, preset};
use flow_graph::flow_edit::{self as fe, ArgKey, Op};
use flow_graph::projection as fproj;
use flow_syntax::{context, lisp, syntax as s, ty::Ty};
use param::Value;
use pxui_graph::scope_state as scope;
use pxui_shell::chrome::{self, Side};
use pxui_shell::layout::{self, Leaf};
use sop::edit_graph::Factory;

use crate::bars;
use crate::editor_commands::{self, action};
use crate::editor_inspect::{inspector_paths, scene};
use crate::editor_list::scene_rows;
use crate::editor_model::*;
use crate::editor_panes::{graph_name, graph_of_object, peek_target, route_in, shown_pane, graph_name_at};
use crate::editor_shell::{layouts, panel_key, panel_state, shell_tree};
use crate::editor_status::level_name_at;
use crate::editor_add::add_target;
use crate::navigator;
use crate::selection;
use crate::text_pane;

/// A panel header's title: its type, and where a looped panel comes from (register E1).
pub fn panel_title<'a>(value: &ModelView<'a>, leaf: &Leaf) -> String {
    // a graph panel's title is its own pane's
    let (pane, path) = shown_pane(value, (&panel_key(&value.doc, &leaf.path), &leaf.path, &leaf.panel));
    let graph = graph_name_at(&value.doc, pane.at, pane.shown.as_deref(), path.as_deref()).unwrap_or_default();
    let (name, sub): (&str, String) = match &leaf.panel {
        Panel::Canvas(_) => ("Canvas", String::new()),
        Panel::View(_) => {
            // the scene it shows and its render camera: "scene / camera"
            let scene_name = value.doc.workspace.ws.checked.graphs.iter().find(|g| g.context == context::scene()).map(|g| g.name.clone()).unwrap_or_default();
            let camera = value.doc.active_camera.and_then(|id| kg::find(&scene(&value), id).map(|n| n.label));
            let parts: Vec<String> = [scene_name, camera.unwrap_or_default()].into_iter().filter(|s| !s.is_empty()).collect();
            ("Viewport", parts.join(" / "))
        }
        Panel::Graph => (
            "Graph",
            if graph.is_empty() {
                String::new()
            } else {
                let ctx = value.doc.workspace.ws.checked.graphs.iter().find(|g| g.name == graph).map(|g| flow_check::workspace::context_name(&g.context));
                let head = if pane.route.is_empty() {
                    graph.clone()
                } else {
                    route_in(value, &pane, path.as_deref()).split('>').map(|p| p.trim().to_string()).collect::<Vec<_>>().join(" / ")
                };
                head + &ctx.map(|c| format!(" / {c}")).unwrap_or_default()
            },
        ),
        Panel::List => ("List", level_name_at(value, pane.at)),
        Panel::Lisp => (
            "Lisp",
            if leaf.frame.2 < 400 {
                String::new() // the narrow sheet has the kind alone
            } else if graph.is_empty() {
                value.file.clone()
            } else if value.file.is_empty() {
                graph.clone()
            } else {
                format!("{} / {}", value.file, graph)
            },
        ),
        Panel::Inspector => {
            let paths = if pane.laid.is_none() { Vec::new() } else { inspector_paths(scope::selected(&pane.view)) };
            let sub = match paths.as_slice() {
                [path] if !graph.is_empty() => {
                    // the node's title (a result is `result`, not `@result`); the narrow column has the node alone
                    let title = match pane.laid.as_deref().and_then(|k| fproj::find(&k.scope, path)) {
                        Some(n) => fproj::title(&n),
                        None => {
                            let last = path.last().map_or_else(String::new, ToString::to_string);
                            match last.strip_prefix(':') {
                                Some(rest) => rest.to_string(),
                                None => last,
                            }
                        }
                    };
                    if leaf.frame.2 < 340 { title } else { format!("{graph} / {title}") }
                }
                _ => graph.clone(),
            };
            ("Inspector", sub)
        }
        Panel::Outline => ("Outline", if leaf.frame.2 < 300 { String::new() } else { value.file.clone() }),
        Panel::Spreadsheet => ("Spreadsheet", graph.clone()),
        Panel::Timeline => (
            "Timeline",
            // the real step of the sketch: the same step every frame is a fixed one
            if value.steady >= 3 && value.last_dt > 0.0 { format!("fixed dt 1/{}", (1.0 / value.last_dt).round() as i64) } else { "realtime".to_string() },
        ),
    };
    let name = if sub.is_empty() { name.to_string() } else { format!("{name}\t{sub}") };
    let origin = value.doc.shell.as_ref().and_then(|s| if value.workspace.restored { None } else { s.origins.iter().find(|(p, _)| *p == leaf.path).map(|(_, o)| o.clone()) });
    match origin {
        Some(Origin::Loop(from, _)) => format!("{name} \u{b7} from loop {}", document::describe(&value.doc.workspace.ws.source, &from)),
        _ => name,
    }
}

fn ty_text(ty: &Ty) -> Option<String> {
    match ty {
        Ty::Named(n) if n == "geometry" => Some("geometry".to_string()),
        Ty::Float => Some("float".to_string()),
        Ty::Int => Some("int".to_string()),
        Ty::Bool => Some("bool".to_string()),
        Ty::Vec3 => Some("vec3".to_string()),
        Ty::Text => Some("text".to_string()),
        Ty::Fn(_) => Some("fn".to_string()),
        Ty::List(inner) => ty_text(inner).map(|s| format!("(list {s})")),
        _ => None,
    }
}

fn find_ty(name: &str, sc: &fproj::Scope) -> Option<Ty> {
    if let Some(i) = sc.inputs.iter().find(|i| i.name == name) {
        return Some(i.ty.clone());
    }
    sc.nodes.iter().find_map(|n| if n.binds.iter().any(|b| b == name) { Some(n.ty.clone()) } else { n.zone.as_ref().and_then(|z| find_ty(name, &z.scope)) })
}

/// Make a reusable function from the selected nodes: the names they read from outside are typed
/// from the graph pane's projection, the context is the result's (a geometry is `sop`, anything
/// else `value`).
pub fn defn_change<'a>(value: &ModelView<'a>, paths: &[flow_check::workspace::Path]) -> change {
    let ws = &value.doc.workspace.ws;
    let scope_opt = pane_laid(value).map(|k| k.scope.clone());
    match (fe::defn_draft(&ws.source, paths), scope_opt) {
        (Err(d), _) => change::Declined(d.message),
        (Ok(_), None) => change::Declined("Open a graph to make a function from its nodes".to_string()),
        (Ok(draft), Some(scope)) => {
            let typed: Vec<(String, Option<String>)> = draft.free.iter().map(|n| (n.clone(), find_ty(n, &scope).and_then(|t| ty_text(&t)))).collect();
            if let Some((n, _)) = typed.iter().find(|(_, t)| t.is_none()) {
                return change::Declined(format!("The function reads {n}, whose type a function cannot take."));
            }
            let mut result_ty = None;
            for path in paths {
                if let Some(n) = fproj::find(&scope, path) {
                    result_ty = Some(n.ty);
                }
            }
            change::Syntax_edit(Op::MakeDefn {
                nodes: paths.to_vec(),
                name: draft.name,
                context: if result_ty == Some(flow_syntax::ty::geometry()) { "sop" } else { "value" }.to_string(),
                params: typed.into_iter().map(|(n, t)| (n, t.unwrap_or_default())).collect(),
            })
        }
    }
}

/// The Navigator: what it shows of the document (see `navigator`).
/// The scene's objects as the Outline lists them: the list panel's rows, with what each is and the
/// binding that places it.
pub fn outline_objects<'a>(value: &ModelView<'a>) -> Vec<navigator::Obj> {
    let document = scene(value);
    let selected: Vec<i64> = if pane_level(value) == document::Level::Scene { selection::selected_nodes(pane_selection(value)).to_vec() } else { Vec::new() };
    let rows: Vec<(i64, navigator::Obj)> = scene_rows(None, &document)
        .iter()
        .map(|row| {
            let node = kg::find(&document, row.id);
            let flag = |name: &str| node.as_ref().and_then(|n| if objects::has_flag(name, n) { Some(objects::flag(name, n)) } else { None });
            let lead = value.doc.active_camera == Some(row.id);
            // a light says its shape, the render camera that it is active
            let choice = node.as_ref().and_then(|n| {
                kg::fields(n).iter().find_map(|f| match (f.name.as_str(), &f.current) {
                    ("shape" | "type", Value::Choice(v)) => Some(v.to_lowercase()),
                    _ => None,
                })
            });
            let operation = node.as_ref().map(|n| n.kind.clone());
            let field = |name: &str| {
                node.as_ref().and_then(|n| {
                    kg::fields(n).iter().find_map(|f| match (f.name == name, &f.current) {
                        (true, Value::Float(v)) => Some(*v),
                        _ => None,
                    })
                })
            };
            // the sheet's words: a camera its focal length, a geometry object the graph it
            // references, the World a reference to its graph; a light its shape
            let (name, detail) = match operation.as_deref() {
                Some("camera") => (
                    row.label.clone(),
                    match field("fov") {
                        Some(fov) if fov > 0.0 && fov < 180.0 => format!("{} mm", cfmt::f(12.0 / (fov * std::f64::consts::PI / 360.0).tan(), 0)),
                        _ => row.detail.clone(),
                    },
                ),
                Some("geometry") => (
                    row.label.clone(),
                    match graph_of_object(value, row.id) {
                        Some(graph) => format!("ref {graph}"),
                        None => row.detail.clone(),
                    },
                ),
                Some("world") => ("world".to_string(), format!("ref {}", row.label)),
                _ => (row.label.clone(), choice.unwrap_or_else(|| row.detail.clone())),
            };
            // the render camera's mark is its flag (the accent); the World renders while it is in
            // the scene; neither is a field a press can write
            let (inert, render) = match operation.as_deref() {
                Some("camera") => (true, Some(lead)),
                Some("world") => (true, Some(true)),
                _ => (false, flag("render")),
            };
            // the render camera has both flags, its visible one only showing
            let visible = match operation.as_deref() {
                Some("camera") => Some(true),
                _ => flag("visible"),
            };
            let rank = match operation.as_deref() {
                Some("camera") => 0,
                Some("light") => 1,
                Some("world") => 4,
                _ => 2,
            };
            let home = match value.doc.homes.objects.iter().find(|(id, _)| *id == row.id) {
                Some((_, document::Home::BoundAt(path))) => Some(path.clone()),
                _ => None,
            };
            (
                rank,
                navigator::Obj { depth: row.depth, letter: row.badge.0.clone(), name, detail, visible, render, lead, inert, chosen: selected.contains(&row.id), home },
            )
        })
        .collect();
    // the sheet's order: the camera, the lights, the geometry, the World; a nested scene keeps its tree
    if rows.iter().all(|(_, o)| o.depth == 0) {
        let mut rows = rows;
        rows.sort_by_key(|(rank, _)| *rank);
        rows.into_iter().map(|(_, o)| o).collect()
    } else {
        rows.into_iter().map(|(_, o)| o).collect()
    }
}

pub fn navigator_params<'a>(value: &ModelView<'a>) -> navigator::Params {
    let ws = &value.doc.workspace.ws;
    let active = graph_name(value);
    let here = match (pane_laid(value), &active) {
        (Some(k), Some(name)) if k.graph == *name => Some(k),
        _ => None,
    };
    let evaluated = pane_laid(value).and_then(|k| k.evaluated.clone());
    let probes = value.probes.clone();
    navigator::Params {
        workspace: ws.checked.clone(),
        active,
        scope: here.map(|k| k.scope.clone()),
        records: here.and_then(|k| k.records.clone()),
        probes: Rc::new(move |p: &[Symbol]| probes.get(p).copied().unwrap_or(0)),
        selected: scope::selected(pane_view(value)),
        chips: evaluated.as_ref().map(|ev| navigator::chips(ev)).unwrap_or_default(),
        notes: evaluated.as_ref().map(|ev| navigator::notes(ev)).unwrap_or_default(),
        objects: outline_objects(value),
        root_detail: format!("{} spp", objects::root::render(&value.doc.root).max_spp),
        layouts: value.doc.shell.as_ref().and_then(|s| s.switch.as_ref()).map(|sw| (sw.layouts.iter().map(panels::summary).collect(), sw.active)),
    }
}

/// The leaf the graph pane in use draws in, when it is open.
pub fn graph_leaf<'a>(value: &ModelView<'a>, g: &layout::Geometry) -> Option<Leaf> {
    let path = crate::editor_shell::graph_path(value)?;
    g.leaves.iter().find(|l| l.path == path && l.panel == Panel::Graph && l.body.3 > 0).cloned()
}

fn under(prefix: &[usize], path: &[usize]) -> bool {
    path.len() >= prefix.len() && prefix.iter().zip(path).all(|(a, b)| a == b)
}

fn shared(a: &[usize], b: &[usize]) -> i64 {
    a.iter().zip(b).take_while(|(x, y)| x == y).count() as i64
}

/// Chrome emits document edits; splitter drafts stay local until release. A refused structural
/// edit returns a notice and preserves the document.
/// The focused leaf: the one clicked when its kind is shown twice, else the first of its kind.
pub fn focused_leaf(g: &layout::Geometry, focus: &Panel, path: Option<&[usize]>) -> Option<Leaf> {
    if let Some(leaf) = g.leaves.iter().find(|l| Some(l.path.as_slice()) == path && l.panel == *focus) {
        return Some(leaf.clone());
    }
    // the tree changed under the focus: a split moves the leaf a level down (and a viewport's key
    // follows its path), a retype leaves another kind at the path.  The leaf of that kind under
    // the old path, else the leaf now at the path, else the first of the kind.
    let kind = |l: &Leaf| editor_commands::scope(&l.panel) == editor_commands::scope(focus);
    let first = |f: &dyn Fn(&Leaf) -> bool| g.leaves.iter().find(|l| f(l)).cloned();
    path.and_then(|p| first(&|l| kind(l) && under(p, &l.path)))
        .or_else(|| first(&|l| Some(l.path.as_slice()) == path))
        .or_else(|| layout::find(g, focus))
        .or_else(|| first(&kind))
        // its panel was closed and no other is of its kind: the leaf that shares the most of its
        // path, the neighbour that took its place
        .or_else(|| {
            path.and_then(|p| {
                let mut best: Option<&Leaf> = None;
                for l in &g.leaves {
                    best = match best {
                        Some(b) if shared(p, &b.path) >= shared(p, &l.path) => Some(b),
                        _ => Some(l),
                    };
                }
                best.cloned()
            })
        })
}

pub fn panel_kind(panel: &Panel) -> &'static str {
    match panel {
        Panel::Canvas(_) => "canvas",
        Panel::View(_) => "viewport",
        Panel::Graph => "graph",
        Panel::List => "list",
        Panel::Lisp => "lisp",
        Panel::Inspector => "inspector",
        Panel::Outline => "outline",
        Panel::Timeline => "timeline",
        Panel::Spreadsheet => "spreadsheet",
    }
}

fn scene_name(value: &EditorModel) -> String {
    value.doc.workspace.ws.checked.graphs.iter().find(|g| g.context == context::scene()).map(|g| g.name.clone()).unwrap_or_else(|| "scene".to_string())
}

fn set_editor_graph(form: s::Syntax) -> change {
    change::Syntax_edit(Op::SetGraph { name: "editor".to_string(), form })
}

/// / [ and / n: the layouts of the switch and the floating windows, each one edit of the editor
/// graph (written from the layout shown first, when the document has none).
pub fn layout_actions<'a>(value: &ModelView<'a>, workspace: &shell, leaf: Option<&Leaf>, actions: &[action]) -> Vec<change> {
    let graph = workspace_doc::editor_graph(&value.doc.workspace.ws).map(|g| g.name);
    let has_switch = value.doc.shell.as_ref().and_then(|s| s.switch.as_ref()).is_some();
    let edit = |make: &dyn Fn(String) -> le::Op| -> Vec<change> {
        match &graph {
            _ if workspace.restored => vec![change::Declined("The default layout is showing. / z returns to the editor graph.".to_string())],
            Some(graph) => vec![change::Layout_edit(make(graph.clone()))],
            None => {
                let scene = scene_name(value);
                let (text, _) = bars::tree_text("editor", &scene, &shell_tree(value, &shell { live: None, ..workspace.clone() }));
                match s::parse(&text) {
                    Ok(forms) if forms.len() == 1 => vec![set_editor_graph(forms[0].clone()), change::Layout_edit(make("editor".to_string()))],
                    _ => vec![change::Declined("The layout could not be written as an editor graph.".to_string())],
                }
            }
        }
    };
    let window = |kind: &str| -> Vec<change> {
        match panel_kinds::expr(&value.doc.workspace.ws.source, kind) {
            Ok(panel) => edit(&|graph| le::Op::LayoutWindow { graph, panel: panel.clone() }),
            Err(message) => vec![change::Declined(message)],
        }
    };
    let mut out = Vec::new();
    for a in actions {
        out.extend(match a {
            action::Layout_switch(index) if has_switch => edit(&|graph| le::Op::SetLayout { graph, index: *index }),
            action::Layout_switch(index) => {
                // an older file: several editor graphs
                match layouts(value).get(*index as usize) {
                    Some((name, _)) => vec![change::Select_layout(name.clone())],
                    None => Vec::new(),
                }
            }
            action::Layout_new if !has_switch && !layouts(value).is_empty() => {
                // an older file with several editor graphs: they become the layouts of one switch first
                match &graph {
                    Some(graph) => vec![change::Layout_edit(le::Op::MergeThenNew { graph: graph.clone() })],
                    None => Vec::new(),
                }
            }
            action::Layout_new => edit(&|graph| le::Op::LayoutNew { graph }),
            action::Layout_remove => edit(&|graph| le::Op::LayoutRemove { graph }),
            action::Window_new(panel) => window(panel_kind(panel)),
            action::Peek => match peek_target(value) {
                Some(target) => window(&format!("graph:{target}")),
                None => vec![change::Declined("Nothing selected to peek at".to_string())],
            },
            action::Float_toggle => match leaf {
                Some(leaf) => edit(&|graph| le::Op::LayoutFloat { graph, at: leaf.path.iter().map(|i| *i as i64).collect() }),
                None => Vec::new(),
            },
            _ => Vec::new(),
        });
    }
    out
}

type Maker = Rc<dyn Fn(&flow_check::workspace::Path) -> le::Op>;

pub fn layout_intents<'a>(value: &ModelView<'a>, workspace: &shell, intents: &[chrome::Intent]) -> (shell, Vec<change>) {
    if intents.is_empty() {
        return (workspace.clone(), Vec::new());
    }
    let base = shell_tree(value, &shell { live: None, ..workspace.clone() }).into_owned();
    let editor = workspace_doc::editor_graph(&value.doc.workspace.ws).map(|g| g.name);
    let shell_doc = if workspace.restored { None } else { value.doc.shell.as_ref() };
    let edit = |path: &layout::Path, make: Maker| -> Vec<change> {
        match (&editor, shell_doc) {
            (Some(graph), Some(s)) => match s.origins.iter().find(|(p, _)| p == path).map(|(_, o)| o) {
                Some(Origin::Bound(name)) => vec![change::Layout_edit(make(&vec![graph.clone().into(), name.clone().into()]))],
                Some(Origin::Inline(home, key)) => {
                    let make = make.clone();
                    vec![change::Layout_inline { home: home.clone(), key: key.clone(), make: Rc::new(move |p: &flow_check::workspace::Path| make(p)) }]
                }
                Some(Origin::Loop(home, key)) => {
                    // a loop's panels are copies of its one template: retyping edits the template
                    match make(&Vec::new()) {
                        le::Op::SetPanelKind { panel, .. } => vec![change::Layout_inline {
                            home: home.clone(),
                            key: key.clone(),
                            make: Rc::new(move |p: &flow_check::workspace::Path| {
                                let mut node = p.clone();
                                node.push("@result".into());
                                le::Op::SetPanelKind { node, panel: panel.clone() }
                            }),
                        }],
                        _ => vec![change::Declined(format!(
                            "These panels are copies made by a loop in {}: retype them (/ o), or edit the loop in the editor graph.",
                            document::describe(&value.doc.workspace.ws.source, home)
                        ))],
                    }
                }
                None => vec![change::Declined("This panel is not part of the editor graph's tree.".to_string())],
            },
            _ if workspace.restored => vec![change::Declined("The default layout is showing. / z returns to the editor graph.".to_string())],
            _ => {
                // no editor graph: the layout shown is written as one first, then edited
                let scene = scene_name(value);
                let (text, name_of) = bars::tree_text("editor", &scene, &base);
                match (s::parse(&text), name_of(path)) {
                    (Ok(forms), Some(leaf)) if forms.len() == 1 => vec![set_editor_graph(forms[0].clone()), change::Layout_edit(make(&vec!["editor".into(), leaf.into()]))],
                    _ => vec![change::Declined("This panel is not part of the layout.".to_string())],
                }
            }
        }
    };
    // a panel of `kind` for `make`: the form the editor's panel table writes, or why there is none
    let with_panel = |kind: &str, path: &layout::Path, make: Rc<dyn Fn(s::Syntax, &flow_check::workspace::Path) -> le::Op>| -> Vec<change> {
        match panel_kinds::expr(&value.doc.workspace.ws.source, kind) {
            Ok(panel) => edit(path, Rc::new(move |node: &flow_check::workspace::Path| make(panel.clone(), node))),
            Err(message) => vec![change::Declined(message)],
        }
    };
    let beside = |path: &layout::Path| -> &'static str {
        match panels::at_ref(path, &base) {
            Some(panels::Panels::Leaf(panel)) => panel_kinds::beside(panel_kind(panel)),
            _ => panel_kinds::beside(""),
        }
    };
    let save_state = |path: &layout::Path, state: panels::State| -> Vec<change> {
        let mut prefix = Vec::new();
        if editor.is_none() {
            let scene = scene_name(value);
            let (text, _) = bars::tree_text("editor", &scene, &base);
            if let Ok(forms) = s::parse(&text) {
                if forms.len() == 1 {
                    prefix.push(set_editor_graph(forms[0].clone()));
                }
            }
        }
        prefix.push(change::Panel_state(path.clone(), state));
        prefix
    };
    let mut w = workspace.clone();
    let mut changes: Vec<change> = Vec::new();
    for intent in intents {
        match intent {
            chrome::Intent::Toggle(path) => {
                let state = panel_state(value, path);
                changes.extend(save_state(path, panels::State { collapsed: !state.collapsed, ..state }));
            }
            chrome::Intent::Window(path, window) => changes.extend(save_state(path, panels::State { window: *window, ..panel_state(value, path) })),
            chrome::Intent::Window_drag(path, window, released) => {
                if *released {
                    w.window_live = None;
                    changes.extend(save_state(path, panels::State { window: Some(*window), ..panel_state(value, path) }));
                } else {
                    w.window_live = Some((path.clone(), *window));
                }
            }
            chrome::Intent::Dragging(..) | chrome::Intent::Crumb(..) => {}
            chrome::Intent::Dock_panel(source, target, side) => changes.push(change::Dock_panels(source.clone(), target.clone(), *side)),
            chrome::Intent::Resize { node, size } => w.live = Some((node.clone(), *size)),
            chrome::Intent::Settled => {
                if let Some((node, size)) = w.live.take() {
                    changes.extend(edit(&node, Rc::new(move |node: &flow_check::workspace::Path| le::Op::SetLayoutSize { node: node.clone(), size })));
                }
            }
            chrome::Intent::Split_panel(path, axis) => {
                let axis = *axis;
                changes.extend(with_panel(beside(path), path, Rc::new(move |panel, node| le::Op::SplitPanel { node: node.clone(), axis, ratio: 0.5, panel })));
            }
            chrome::Intent::Split_panel_at(path, axis, ratio, kind) => {
                let (axis, ratio) = (*axis, *ratio);
                changes.extend(with_panel(panel_kind(kind), path, Rc::new(move |panel, node| le::Op::SplitPanel { node: node.clone(), axis, ratio, panel })));
            }
            chrome::Intent::Close_panel(path) => changes.extend(edit(path, Rc::new(|node: &flow_check::workspace::Path| le::Op::ClosePanel { node: node.clone() }))),
            chrome::Intent::Retype_panel(path, kind) => {
                changes.extend(with_panel(panel_kind(kind), path, Rc::new(|panel, node| le::Op::SetPanelKind { node: node.clone(), panel })));
            }
        }
    }
    (w, changes)
}

pub fn dock_panels(factories: &[Factory], doc: &Document, source: &layout::Path, target: &layout::Path, side: Side) -> Result<Document, String> {
    let states = &doc.workspace.ws.layout.panels;
    let source_key = panel_key(doc, source);
    let target_key = panel_key(doc, target);
    let saved = |key: &flow_check::workspace::Path| states.get(key).copied().unwrap_or_else(panels::default_state);
    let loop_message = "These panels are copies made by a loop: move their tile in the editor graph.";
    let (doc, node) = document_edit::panel_node(Some(factories), Some(loop_message), doc, source)?;
    let (doc, target_node) = document_edit::panel_node(Some(factories), Some(loop_message), &doc, target)?;
    let doc = document_edit::layout_op(Some(factories), &doc, &le::Op::DockPanel { node: node.clone(), target: target_node.clone(), side })?;
    let copy_pos = if matches!(side, Side::Left | Side::Top) { 3 } else { 2 };
    let copy: flow_check::workspace::Path = match fe::arg_text(&doc.workspace.ws.source, &target_node, &ArgKey::Pos(copy_pos)) {
        Some(e) => match &e.node {
            s::Node::Sym(name) => {
                let mut c = target_node.clone();
                c.pop();
                c.push(e.name(*name).into());
                c
            }
            _ => target_node.clone(),
        },
        None => target_node.clone(),
    };
    let (source_state, target_state) = (saved(&source_key), saved(&target_key));
    Ok(document_edit::layout_edit(&doc, &|layout| {
        let mut panels: PathMap<panels::State> = layout.panels.clone();
        panels.remove(&source_key);
        panels.remove(&target_key);
        panels.insert(node.clone(), panels::State { window: None, ..source_state });
        panels.insert(copy.clone(), target_state);
        let mut next = layout.clone();
        next.panels = panels;
        next
    }))
}

/// A pane's request applied to the open level's objects or layers: a parameter or name of one the
/// host made.  Text gestures (`Syntax_edit`) are applied to the whole document afterwards.
pub fn apply_change(state: (kg::T, Option<String>, param::Effects), c: &change) -> (kg::T, Option<String>, param::Effects) {
    let (document, error, effects) = state;
    match c {
        change::Set_parameter { node, path, value } => match kg::set_fields(&document, *node, &[(path.clone(), value.clone())]) {
            Err(message) => (document, Some(message), effects),
            Ok((document, changed)) => (document, None, param::union_effects(effects, changed)),
        },
        change::Rename { node, label } => match kg::find(&document, *node) {
            None => {
                let message = format!("no object #{node}");
                (document, Some(message), effects)
            }
            Some(_) => (
                kg::update(&document, *node, &|mut n| {
                    n.label = label.clone();
                    n
                }),
                None,
                effects,
            ),
        },
        _ => (document, error, effects),
    }
}

/// Command-C / X: the selected bindings as Lisp pairs ("name expr" per line) on the clipboard, the
/// text a let* vector or the Lisp pane takes.
pub fn copy_bindings(value: &EditorModel, paths: &[flow_check::workspace::Path]) -> change {
    let ws = &value.doc.workspace.ws;
    let mut lines = Vec::new();
    for path in paths {
        if let Some((_, form)) = text_pane::binding(&ws.source, path) {
            let (text, _) = match lisp::print(&[form]) {
                Ok(printed) => printed,
                Err(error) => return change::Declined(rays_diagnostic::to_string(&error)),
            };
            lines.push(format!("{} {}", path.last().map_or("", |name| name.as_str()), text.trim()));
        }
    }
    if lines.is_empty() {
        return change::Declined("Nothing to copy".to_string());
    }
    let text = lines.join("\n");
    if let Err(error) = rays_diagnostic::check_source_len(text.len()) {
        return change::Declined(rays_diagnostic::to_string(&error));
    }
    match rays::clipboard::set_text(&text) {
        Ok(()) => change::Notice(format!("Copied {} binding{}", lines.len(), if lines.len() == 1 { "" } else { "s" })),
        Err(message) => change::Declined(format!("Clipboard: {message}")),
    }
}

/// The palette's "Copy workspace as Lisp": the text Command-S writes, on the clipboard.
pub fn copy_workspace(value: &EditorModel) -> (notice_kind, String) {
    match preset::text(&value.doc).and_then(|text| rays::clipboard::set_text(&text)) {
        Ok(()) => (notice_kind::Info, "Copied the workspace as Lisp".to_string()),
        Err(message) => (notice_kind::Refusal, format!("Clipboard: {message}")),
    }
}

/// Command-V: the clipboard's "name expr" pairs (or bare expressions, named by their head) become
/// `Add_node`s in the selected scope (`Text_pane::paste_ops`), one history entry.
pub fn paste_bindings<'a>(value: &ModelView<'a>) -> Vec<change> {
    let Some((graph, _)) = add_target(value) else { return vec![change::Declined("Open a graph to paste into".to_string())] };
    let text = match rays::clipboard::get_text() {
        Ok(text) => text,
        Err(message) => return vec![change::Declined(format!("Clipboard: {message}"))],
    };
    let ws = &value.doc.workspace.ws;
    let selected = scope::selected(pane_view(value));
    let scope_path: flow_check::workspace::Path = match selected.as_slice() {
        [path] if path.len() >= 2 => path[..path.len() - 1].to_vec(),
        _ => vec![graph.clone().into()],
    };
    // one gesture: every binding lands or none does
    match text_pane::paste_ops(&ws.source, &graph, &scope_path, &text) {
        Err(message) => vec![change::Declined(message)],
        Ok(ops) if ops.len() == 1 => vec![change::Syntax_edit(ops[0].clone())],
        Ok(ops) => vec![change::Syntax_batch("Paste".to_string(), ops)],
    }
}

/// The one node the inspector shows for the pane's selection, if exactly one (a test hook)
pub fn inspector_subject<'a>(value: &ModelView<'a>) -> Option<flow_check::workspace::Path> {
    if pane_laid(value).is_none() {
        return None;
    }
    let paths = inspector_paths(scope::selected(pane_view(value)));
    if paths.len() == 1 { Some(paths[0].clone()) } else { None }
}
