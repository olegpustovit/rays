//! The one reduction phase of a frame: what the panes asked for (`frame_result`) and the keys
//! (`actions`) become the next model. Nothing here builds UI: the panes have finished constructing
//! their boxes, and a test can run it on a frame result it made itself.
//!
//! The reducer owns the model and keeps only the previous identities needed for comparisons;
//! `Frame_input.view_state` is an `Rc<dyn Fn>`; optional arguments (`?gpu ?pump`) are trailing
//! `Option`s of `reduce`.

use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::Arc;

use editor_core::panels::Panel;
use editor_core::router;
use editor_document::document::{self, Home, Level, T as Document};
use editor_document::document_edit;
use editor_document::kind_graph as kg;
use editor_document::layout_by_path::{self as lbp, PathMap};
use editor_document::layout_edit;
use editor_document::lowering;
use editor_document::objects;
use editor_document::preset;
use editor_document::scene_sync::{self as sync, Edit};
use editor_document::settings;
use flow_graph::flow_edit::{self as fe, ArgKey, Op};
use flow_syntax::syntax::{Node as SNode, Syntax};
use input::{Event, MouseButton};
use param::{Effects, Value};
use rays_graph::scope_commands;
use rays_graph::scope_state as scope;
use rays_shell::{prompt as shell_prompt, tree};
use rays::frame;
use sop::edit_graph::Factory;

use crate::carry as carry_mod;
use crate::cook;
use crate::echo;
use crate::editor_actions::{in_world, intent_label, world_keys};
use crate::editor_carry::carry_payload;
use crate::editor_commands::Action;
use crate::editor_history::{self as history, Merge};
use crate::editor_inspect::{kind, lists, level_key, node_path, objects as level_objects, view_wants, View};
use crate::editor_list::{apply_tree, enterable, geometry_objects, open_level, tree_edit, TreeState};
use crate::editor_model::*;
use crate::editor_panels::{apply_change, dock_panels};
use crate::editor_panes::{follow_target, fresh_local, go, graph_name, lit_tags, remember, scope_name, viewport_target};
use crate::editor_scope::{sync_scope, view_node};
use crate::editor_setup::{browse, save_guide, state_directory, state_file};
use crate::editor_shell::{active_view, geometry, has_panel, panel_key, unrestore};
use crate::editor_text::{adopt, apply_text, apply_text_at, commit, parameter_gesture, select_source};
use crate::panel_map;
use crate::selection;
use crate::text_pane;
use crate::timeline;

type Path = flow_check::workspace::Path;

fn basename(path: &str) -> String {
    path.rsplit(['/', '\\']).next().unwrap_or(path).to_string()
}
/// A row of the Navigator opens its graph in the graph panel, selecting a node there.
pub fn outline_step(created: &Option<String>, result: &FrameResult, v: &mut EditorModel) -> Option<(String, Option<Path>)> {
    let mut outlined: Option<(String, Option<Path>)> = result.outline_intents.iter().find_map(|i| match i {
        crate::navigator::Intent::Open { graph, node } => Some((graph.clone(), node.clone())),
        _ => None,
    });
    // / j, or a material just made, shows its graph like a row of the outline
    if outlined.is_none() {
        if let Some(PromptIntent::Go(graph)) = &result.prompt_intent {
            outlined = Some((graph.clone(), None));
        } else if let Some(graph) = created {
            outlined = Some((graph.clone(), None));
        }
    }
    v.outline = result.outline.clone();
    if let Some((_, Some(node))) = &outlined {
        v.select_later = vec![node.clone()];
    }
    if result.outline_intents.iter().any(|i| matches!(i, crate::navigator::Intent::Macro(_))) {
        v.text.tab = text_pane::Tab::Document;
    }
    // a node of the graph already shown is selected now; of another graph, once the pane shows it
    if let Some((graph, Some(node))) = &outlined {
        if graph_name(&(&*v).into()).as_deref() == Some(graph.as_str()) && pane_laid(&(&*v).into()).is_some_and(|k| &k.graph == graph) {
            v.select_later = Vec::new();
            let node = node.clone();
            {
                let view = &mut pane_mut(v).view;
                scope::select(vec![node], view);
                scope_commands::run_command(view, scope::Command::FrameSelection, &mut Vec::new());
            };
        }
    }
    if let Some((graph, _)) = &outlined {
        go(v, graph);
    }
    outlined
}

/// Entering and leaving levels: the graph a reference follows, the level i / u / list activation
/// open.
pub fn level_target(followed: Option<String>, has_route: bool, value2: &EditorModel, level: Level, doc: &Document, selection: &selection::Selection, actions: &[Action], result: &FrameResult) -> (Option<String>, Option<Level>) {
    let top = actions.iter().find(|a| matches!(a, Action::Enter | Action::Up | Action::GoWorld));
    let target = if followed.is_some() {
        None
    } else {
        match (top, &result.opened) {
            (Some(Action::Up), _) if has_route => None,
            (Some(Action::Up), _) => match level {
                Level::Inside(_) => Some(Level::Scene),
                Level::Scene => None,
            },
            (Some(Action::GoWorld), _) => objects::ids("world", &doc.scene).into_iter().find(|id| enterable(value2, *id)).map(Level::Inside),
            (Some(Action::Enter), _) | (_, Some(_)) => {
                let candidate = match result.opened {
                    Some(id) => Some(id),
                    None => selection::selected(selection),
                };
                match (level, candidate) {
                    (Level::Scene, Some(id)) if enterable(value2, id) => Some(Level::Inside(id)),
                    _ => None,
                }
            }
            _ => None,
        }
    };
    (followed, target)
}

/// The history key of a gesture still in progress, if the frame's change belongs to one.
pub fn gesture_key<'a>(value: &ModelView<'a>, frame_: &frame::Frame, loaded: bool, world_added: bool, world_label: &Option<String>, result: &FrameResult) -> Option<String> {
    let dragging = frame::mouse_down(MouseButton::LeftButton, frame_) || frame::has_event(&|e| matches!(e, Event::PointerReleased(MouseButton::LeftButton, _)), frame_);
    if !dragging || loaded || world_added || world_label.is_some() {
        return None;
    }
    let found = result.changes.iter().find_map(|c| match c {
        Change::SetParameter { node, path, .. } => Some(format!("graph.scrub:{}:{}:{}", level_key(pane_level(value)), node, path)),
        Change::SyntaxEdit(op) => fe::gesture(op),
        Change::LayoutEdit(op) => layout_edit::gesture(op).map(|g| g.to_string()),
        Change::PanelState(path, _) => Some(format!("panel:{}", flow_syntax::symbol::join_path(&panel_key(&value.doc, path), "/"))),
        Change::DockPanels(path, _, _) => Some(format!("panel:{}", flow_syntax::symbol::join_path(&panel_key(&value.doc, path), "/"))),
        _ => None,
    });
    if found.is_some() {
        return found;
    }
    let graph_empty = result.changes.iter().all(|c| intent_label(c).is_none());
    if !graph_empty {
        return None;
    }
    match (&result.tree_intents[..], &result.handle_changes, &result.settings_changes[..]) {
        ([], Some((id, changes)), []) => Some(parameter_gesture("handle", pane_level(value), *id, changes)),
        ([], None, changes) if !changes.is_empty() => Some(parameter_gesture("settings", pane_level(value), -1, changes)),
        (intents, None, []) if matches!(intents.first(), Some(tree::Intent::Flag { .. })) && intents.iter().all(|i| matches!(i, tree::Intent::Flag { .. })) => {
            // a stroke down a flag column of the list is one entry
            match intents.first() {
                Some(tree::Intent::Flag { column, .. }) => Some(format!("list.flag:{}:{}", level_key(pane_level(value)), column)),
                _ => None,
            }
        }
        _ => None,
    }
}

/// The graph pane's layout gestures: moving, collapsing, levels, frames, probes.
pub fn layout_gestures(value: &EditorModel, loaded: bool, added: &[Path], mut next: Arc<Document>, mut result: FrameResult, scope_changes: &[scope::Change]) -> (Arc<Document>, FrameResult, PathMap<i64>) {
    let mut probes = value.probes.clone();
    if loaded {
        return (next, result, probes);
    }
    for c in scope_changes {
        match c {
            scope::Change::ZoneCollapsed { zone, collapsed } => {
                let default = zone.last().is_some_and(|leaf| fe::nested(leaf));
                let desired = if *collapsed || default { Some(*collapsed) } else { None };
                if next.workspace.ws.layout.collapsed.get(zone).copied() != desired {
                    let layout = &mut Arc::make_mut(&mut next).workspace.ws.layout;
                    match desired {
                        Some(v) => { layout.collapsed.insert(zone.clone(), v); }
                        None => { layout.collapsed.remove(zone); }
                    }
                }
                result.label = if *collapsed { "Collapse zone" } else { "Expand zone" }.to_string();
            }
            scope::Change::Moved(placed) => {
                if placed.iter().any(|(path, x, y)| next.workspace.ws.layout.at.get(path) != Some(&(*x, *y))) {
                    let layout = &mut Arc::make_mut(&mut next).workspace.ws.layout;
                    for (path, x, y) in placed { layout.at.insert(path.clone(), (*x, *y)); }
                }
                if added.is_empty() { result.label = "Move".to_string(); }
            }
            scope::Change::LevelSet(changes) => {
                if changes.iter().any(|(path, level, pinned)| next.workspace.ws.layout.level.get(path) != level.as_ref() || next.workspace.ws.layout.pinned.get(path).copied() != if *pinned { Some(true) } else { None }) {
                    let layout = &mut Arc::make_mut(&mut next).workspace.ws.layout;
                    for (path, level, pinned) in changes {
                        match level {
                            Some(v) => { layout.level.insert(path.clone(), v.clone()); }
                            None => { layout.level.remove(path); }
                        }
                        if *pinned { layout.pinned.insert(path.clone(), true); } else { layout.pinned.remove(path); }
                    }
                }
                result.label = "Detail level".to_string();
            }
            scope::Change::ProbeSet { zone, index } => { probes.insert(zone.clone(), *index); }
            scope::Change::FramesSet { scope: sc, frames } => {
                let frames: Vec<_> = frames.iter().map(|(title, at, size)| lbp::Frame { title: title.clone(), at: *at, size: *size }).collect();
                let desired = if frames.is_empty() { None } else { Some(&frames) };
                if next.workspace.ws.layout.frames.get(sc) != desired {
                    let layout = &mut Arc::make_mut(&mut next).workspace.ws.layout;
                    if frames.is_empty() { layout.frames.remove(sc); } else { layout.frames.insert(sc.clone(), frames); }
                }
                result.label = "Frame".to_string();
            }
            _ => {}
        }
    }
    (next, result, probes)
}

/// The scene and World edits act on derived objects: each difference is written to the text (a
/// refused one changes nothing).
pub fn reconciled(value: &EditorModel, before: &Arc<Document>, next: Arc<Document>, result: FrameResult) -> (Arc<Document>, FrameResult) {
    if Arc::ptr_eq(before, &next) { return (next, result); }
    let mut result = result;
    match document_edit::reconcile(Some(&value.factories), None, before, &next) {
        Ok(doc) => (Arc::new(doc), result),
        Err(message) => {
            result.edit_error = Some(message);
            (before.clone(), result)
        }
    }
}

/// What a frame's gestures leave besides the document.
#[derive(Clone, Default)]
pub struct GestureLog {
    pub added: Vec<Path>,
    pub created: Option<String>,
    pub renamed: Option<(String, String)>,
    pub refused: Option<String>,
    pub note: Option<String>,
}

fn first_note(g: &mut GestureLog, note: Option<String>) {
    if g.note.is_none() {
        g.note = note;
    }
}

/// Bind what is written in place (the call holding it, then the expression), then the gesture on
/// its name. `apply` gives the document and the label.
fn inline_gesture(value: &EditorModel, next: &Document, home: &Home, key: &ArgKey, apply: &dyn Fn(&Document, &Path) -> Result<(Document, String), String>) -> Result<(Document, String), String> {
    let (doc, node) = sync::bind_home(Some(&value.factories), next, home)?;
    let doc = document_edit::syntax_edit(Some(&value.factories), &doc, &Op::Unfold { node: node.clone(), key: key.clone(), sub: Vec::new() })?;
    match fe::arg_text(&doc.workspace.ws.source, &node, key) {
        Some(syntax) => match &syntax.node {
            SNode::Sym(name) => {
                let mut path = node[..node.len() - 1].to_vec();
                path.push(syntax.name(*name).into());
                apply(&doc, &path)
            }
            _ => Err("The panel could not be named.".to_string()),
        },
        None => Err("The panel could not be named.".to_string()),
    }
}

type Written = (Arc<Document>, FrameResult, Vec<Path>, Option<String>, Option<(String, String)>, Option<String>);

/// The frame's changes and gestures written to the document, text first: the first refusal leaves
/// none applied. Returns the document, the result, the nodes added, a graph made, a graph renamed
/// and the note the echo shows.
pub fn write_gestures<'a>(value: &ModelView<'a>, loaded: bool, present: &Arc<Document>, tree_edits: &[Edit], in_text: &dyn Fn(i64) -> bool, node_effects: &dyn Fn(Effects) -> Effects, next: Arc<Document>, result: FrameResult) -> Written {
    let (mut next, mut result) = if loaded { (next, result) } else { reconciled(value, present, next, result) };
    // Workspace gestures: one rewrite of the source per gesture, lowered into the document, one
    // history entry named by the op. A frame's gestures land together: the first one refused leaves
    // none of them applied, and a later one cannot hide its message.
    let gestures = (!loaded && (!tree_edits.is_empty() || result.handle_changes.is_some() || !result.changes.is_empty())).then(|| (next.clone(), result.clone()));
    let mut g = GestureLog::default();
    let factories: &[Factory] = &value.factories;
    let level = pane_level(value);
    let set_fields = |next: &mut Arc<Document>, result: &mut FrameResult, g: &mut GestureLog, node: i64, values: &[(String, Value)]| {
        match sync::set_fields(Some(factories), next, level, node, values) {
            Ok(Some((doc, effects, home))) => {
                let note = sync::template_note(next, &home);
                *next = Arc::new(doc);
                result.edit_error = None;
                result.effects = param::union_effects(result.effects, node_effects(effects));
                first_note(g, note);
            }
            Ok(None) => {}
            Err(message) => g.refused = Some(message),
        }
    };
    if !loaded {
        for edit in tree_edits {
            if g.refused.is_some() {
                continue;
            }
            match sync::write(Some(factories), &next, level, edit) {
                Ok(Some((doc, note))) => {
                    next = Arc::new(doc);
                    result.edit_error = None;
                    first_note(&mut g, note);
                }
                Ok(None) => {}
                Err(message) => g.refused = Some(message),
            }
        }
    }
    // a viewport handle's values
    if !loaded {
        if let Some((id, values)) = result.handle_changes.clone() {
            if in_text(id) {
                set_fields(&mut next, &mut result, &mut g, id, &values);
            }
        }
    }
    if !loaded {
        for c in result.changes.clone() {
            if g.refused.is_some() {
                continue;
            }
            match &c {
                Change::SetParameter { node, path, value: field } if in_text(*node) => {
                    set_fields(&mut next, &mut result, &mut g, *node, &[(path.clone(), field.clone())]);
                }
                Change::DockPanels(source, target, side) => match dock_panels(factories, &next, source, target, *side) {
                    Ok(doc) => {
                        next = Arc::new(doc);
                        result.label = "Arrange panel".to_string();
                        result.edit_error = None;
                    }
                    Err(message) => g.refused = Some(message),
                },
                Change::PanelState(path, state) => {
                    let key = panel_key(&next, path);
                    let state = *state;
                    if next.workspace.ws.layout.panels.get(&key) != Some(&state) {
                        Arc::make_mut(&mut next).workspace.ws.layout.panels.insert(key, state);
                    }
                    result.label = if state.collapsed { "Collapse panel" } else { "Arrange panel" }.to_string();
                    result.edit_error = None;
                }
                Change::SelectLayout(name) if editor_document::workspace_doc::editor_graph(&next.workspace.ws).map(|g| g.name).as_ref() == Some(name) => {}
                Change::SelectLayout(name) => match document_edit::select_layout(Some(factories), &next, name) {
                    Ok(doc) => {
                        next = Arc::new(doc);
                        result.label = "Switch layout".to_string();
                        result.edit_error = None;
                    }
                    Err(message) => g.refused = Some(message),
                },
                Change::SyntaxEdit(op) => match document_edit::syntax_edit(Some(factories), &next, op) {
                    Ok(doc) => {
                        // a math node a wire folded into its reader opens as a card, where it was
                        let doc = match op {
                            Op::Connect { node, key, src, .. } if !node.is_empty() => {
                                let sp = node[..node.len() - 1].to_vec();
                                let leaf = fe::nested_leaf(&node[node.len() - 1], key);
                                match (leaf, fe::arg_text(&doc.workspace.ws.source, node, key)) {
                                    (Ok(leaf), Some(e)) if fe::value_call(&e) => {
                                        let mut from = sp.clone();
                                        from.push(src.clone().into());
                                        let at = next.workspace.ws.layout.at.get(&from).copied();
                                        let mut to = sp.clone();
                                        to.push(leaf.into());
                                        document_edit::layout_edit(&doc, &|l| {
                                            let mut l = l.clone();
                                            l.collapsed.insert(to.clone(), false);
                                            if let Some(p) = at {
                                                l.at.insert(to.clone(), p);
                                            }
                                            l
                                        })
                                    }
                                    _ => doc,
                                }
                            }
                            _ => doc,
                        };
                        match op {
                            Op::AddNode { scope: sc, name, .. } => {
                                let mut p = sc.clone();
                                p.push(name.clone().into());
                                g.added = vec![p];
                            }
                            Op::RenameGraph { name, to_ } => g.renamed = Some((name.clone(), to_.clone())),
                            Op::Duplicate { nodes } => g.added = fe::duplicated(&next.workspace.ws.source, nodes),
                            _ => {}
                        }
                        next = Arc::new(doc);
                        result.label = fe::label(op).to_string();
                        result.edit_error = None;
                        result.effects = param::union_effects(result.effects, document_edit::cook_effects());
                        first_note(&mut g, echo::words(op).map(|w| format!("Wrote {w}")));
                    }
                    Err(message) => g.refused = Some(message),
                },
                Change::LayoutEdit(op) => match document_edit::layout_op(Some(factories), &next, op) {
                    Ok(doc) => {
                        next = Arc::new(doc);
                        result.label = layout_edit::label(op).to_string();
                        result.edit_error = None;
                        result.effects = param::union_effects(result.effects, document_edit::cook_effects());
                    }
                    Err(message) => g.refused = Some(message),
                },
                Change::SyntaxBatch(label, ops) => match document_edit::syntax_batch(Some(factories), &next, ops) {
                    Ok(doc) => {
                        for op in ops {
                            match op {
                                Op::AddNode { scope: sc, name, .. } => {
                                    let mut p = sc.clone();
                                    p.push(name.clone().into());
                                    g.added = vec![p];
                                }
                                Op::SetGraph { name, .. } if label == "New material" || label == "New graph" => g.created = Some(name.clone()),
                                _ => {}
                            }
                        }
                        next = Arc::new(doc);
                        result.label = label.clone();
                        result.edit_error = None;
                        result.effects = param::union_effects(result.effects, document_edit::cook_effects());
                        first_note(&mut g, echo::batch(ops).map(|w| format!("Wrote {w}")));
                    }
                    Err(message) => g.refused = Some(message),
                },
                Change::PinRow { node, label, pin } => {
                    let pin = *pin;
                    let current = next.workspace.ws.layout.rows.get(node).and_then(|rows| rows.get(label)).copied();
                    if current != pin || next.workspace.ws.layout.rows.get(node).is_some_and(|rows| rows.is_empty()) {
                        let rows = &mut Arc::make_mut(&mut next).workspace.ws.layout.rows;
                        match pin {
                            Some(v) => { rows.entry(node.clone()).or_default().insert(label.clone(), v); }
                            None => {
                                if let Some(pinned) = rows.get_mut(node) {
                                    pinned.remove(label);
                                    if pinned.is_empty() { rows.remove(node); }
                                }
                            }
                        }
                    }
                    result.label = if pin == Some(false) { "Unpin row from card" } else { "Pin row to card" }.to_string();
                    result.edit_error = None;
                }
                Change::ObjectArg { node, key, sub, expr } => {
                    // the expression typed in a row is the argument of the call that holds the
                    // object (of a loop's template: every copy changes)
                    let home = match level {
                        Level::Scene => next.homes.objects.iter().find(|(n, _)| n == node).map(|(_, h)| h.clone()),
                        Level::Inside(_) => next.homes.layers.iter().find(|(n, _)| n == node).map(|(_, h)| h.clone()),
                    };
                    match home {
                        None => g.refused = Some("That object is not in the text yet: change one of its values, which writes it.".to_string()),
                        Some(home) => {
                            let applied = sync::bind_home(Some(factories), &next, &home).and_then(|(doc, path)| {
                                document_edit::syntax_edit(Some(factories), &doc, &Op::SetArg { node: path, key: ArgKey::Kw(key.clone()), sub: sub.clone(), value: expr.clone() })
                            });
                            match applied {
                                Ok(doc) => {
                                    let note = sync::template_note(&next, &home);
                                    next = Arc::new(doc);
                                    result.label = "Edit expression".to_string();
                                    result.effects = param::union_effects(result.effects, document_edit::cook_effects());
                                    g.note = note;
                                }
                                Err(message) => g.refused = Some(message),
                            }
                        }
                    }
                }
                Change::SyntaxInline { home, key, make } => {
                    let make = make.clone();
                    let applied = inline_gesture(value, &next, home, key, &|doc, path| {
                        let op = make(path);
                        document_edit::syntax_edit(Some(factories), doc, &op).map(|d| (d, fe::label(&op).to_string()))
                    });
                    match applied {
                        Ok((doc, label)) => {
                            next = Arc::new(doc);
                            result.label = label;
                            result.effects = param::union_effects(result.effects, document_edit::cook_effects());
                        }
                        Err(message) => g.refused = Some(message),
                    }
                }
                Change::LayoutInline { home, key, make } => {
                    let make = make.clone();
                    let applied = inline_gesture(value, &next, home, key, &|doc, path| {
                        let op = make(path);
                        document_edit::layout_op(Some(factories), doc, &op).map(|d| (d, layout_edit::label(&op).to_string()))
                    });
                    match applied {
                        Ok((doc, label)) => {
                            next = Arc::new(doc);
                            result.label = label;
                            result.effects = param::union_effects(result.effects, document_edit::cook_effects());
                        }
                        Err(message) => g.refused = Some(message),
                    }
                }
                _ => {}
            }
        }
    }
    if let Some(edit_error) = g.refused.clone() {
        let (doc, mut res) = gestures.expect("a refused gesture has a rollback snapshot");
        res.edit_error = Some(edit_error);
        g.added = Vec::new();
        g.created = None;
        g.renamed = None;
        return (doc, res, g.added, g.created, g.renamed, g.note);
    }
    (next, result, g.added, g.created, g.renamed, g.note)
}

/// The prompt a frame leaves open, the notice, and a preset it loaded.
pub fn prompt_step(value: &EditorModel, view_state: &dyn Fn(Option<&PanelResult>) -> Syntax, result: &FrameResult) -> (Option<Prompt>, Option<(NoticeKind, String)>, Option<preset::Loaded>) {
    let load = |intent: &PromptIntent| -> (Option<Prompt>, Option<(NoticeKind, String)>, Option<preset::Loaded>) {
        let last = matches!(intent, PromptIntent::LoadLastState);
        let (path, restored) = match intent {
            PromptIntent::LoadPresetFile(name) => (preset::path(&value.presets, name), format!("Loaded preset {name}")),
            _ => (state_file(value), "Restored last edited state".to_string()),
        };
        let rejected = if last { "Last edited state rejected: " } else { "Preset rejected: " };
        let ops = (*value.doc.workspace.ws.checked.ops).clone();
        match preset::load_with_ops(Some(value.gpu.clone()), ops, &path, Some(&value.factories), &value.doc.settings) {
            Ok(loaded) => (result.prompt.clone(), Some((NoticeKind::Info, restored)), Some(loaded)),
            Err(message) => (result.prompt.clone(), Some((NoticeKind::Refusal, format!("{rejected}{message}"))), None),
        }
    };
    let (prompt, notice, loaded) = match &result.prompt_intent {
        None => {
            let found = result.changes.iter().find_map(|c| match c {
                Change::Notice(message) => Some((NoticeKind::Info, message.clone())),
                Change::Declined(message) => Some((NoticeKind::Refusal, message.clone())),
                _ => None,
            });
            (result.prompt.clone(), found.or_else(|| value.notice.clone()), None)
        }
        Some(PromptIntent::RunAction(_) | PromptIntent::EditSource(_) | PromptIntent::Go(_) | PromptIntent::FindNode(_)) => (result.prompt.clone(), value.notice.clone(), None),
        Some(PromptIntent::SavePresetFile(name)) => {
            let notice = match preset::save(&value.presets, name, &value.doc, &view_state(result.panel.as_ref())) {
                Ok(path) => (NoticeKind::Info, format!("Saved preset {}", basename(&path))),
                Err(message) => (NoticeKind::Refusal, format!("Preset not saved: {message}")),
            };
            (result.prompt.clone(), Some(notice), None)
        }
        Some(intent @ (PromptIntent::LoadPresetFile(_) | PromptIntent::LoadLastState)) => load(intent),
        Some(intent @ (PromptIntent::DeletePresetFile { .. } | PromptIntent::DeleteLastState(_))) => {
            let (directory, name, query, description) = match intent {
                PromptIntent::DeletePresetFile { name, query } => (value.presets.clone(), name.clone(), query.clone(), format!("preset {name}")),
                PromptIntent::DeleteLastState(query) => (state_directory(value), value.state_name.clone(), query.clone(), "last edited state".to_string()),
                _ => unreachable!(),
            };
            let notice = match preset::delete(&directory, &name) {
                Ok(()) => (NoticeKind::Info, format!("Deleted {description}")),
                Err(message) => (NoticeKind::Refusal, format!("Preset not deleted: {message}")),
            };
            (browse(value, &query), Some(notice), None)
        }
    };
    // [m] in the workspace pane: the make-macro dialog over the selection, its first two literals
    // ticked as in the study.
    let macro_nodes = result.scope_changes.iter().find_map(|c| match c {
        scope::Change::MacroRequested(nodes) => Some(nodes.clone()),
        _ => None,
    });
    let (prompt, notice) = match macro_nodes {
        Some(nodes) => match fe::macro_draft(&value.doc.workspace.ws.source, &nodes) {
            Ok(draft) => {
                let holes = draft.literals.iter().enumerate().map(|(i, _)| (i < 2, format!("p{}", i + 1))).collect();
                let state = shell_prompt::Macro { name: draft.name.clone(), holes };
                (Some(Prompt::MakingMacro { nodes, draft, state }), notice)
            }
            Err(d) => (prompt, Some((NoticeKind::Refusal, d.message.clone()))),
        },
        None => (prompt, notice),
    };
    let referenced = result.text_intents.iter().find_map(|i| match i {
        text_pane::Intent::ReferencesOf(name) => Some(name.clone()),
        _ => None,
    });
    let prompt = match referenced {
        Some(name) => Some(Prompt::Referencing(name, String::new())),
        None => prompt,
    };
    (prompt, notice, loaded)
}

/// A payload in flight: what the panes saw under the pointer becomes the carry's report, and
/// nothing else edits the document until the put (navigation and selection only).
pub fn carry_report_of(value: &EditorModel, carrying: bool, text: &text_pane::State, text_shown: Option<&text_pane::Shown>, result: &FrameResult) -> Option<CarryReport> {
    if !carrying {
        return None;
    }
    let place_of_path = |path: &Path| -> carry_mod::Place {
        if path.len() == 1 { carry_mod::Place::Graph(path[0].to_string()) } else { carry_mod::Place::Node(path.clone()) }
    };
    let mut found: Vec<(carry_mod::Place, bool)> = Vec::new();
    for c in &result.scope_changes {
        match c {
            scope::Change::DropOver { path, .. } => found.push((place_of_path(path), false)),
            scope::Change::Dropped { path, .. } => found.push((place_of_path(path), true)),
            _ => {}
        }
    }
    let text_drops = |text: &text_pane::State, shown: &text_pane::Shown, intents: &[text_pane::Intent]| -> Vec<(carry_mod::Place, bool)> {
        intents
            .iter()
            .filter_map(|i| match i {
                text_pane::Intent::CarryOver(byte, dropped) => {
                    let drafted = match text.tab {
                        text_pane::Tab::Document => text.draft.is_some(),
                        text_pane::Tab::Graph => text_pane::graph_slot(text, &shown.graph).is_some(),
                        text_pane::Tab::Selection => text.binding_draft.as_ref().is_some_and(|(p, _)| *p == shown.key),
                    };
                    let target = if drafted {
                        carry_mod::TextTarget::Drafted
                    } else {
                        match text.tab {
                            text_pane::Tab::Document => carry_mod::TextTarget::Whole,
                            text_pane::Tab::Graph => carry_mod::TextTarget::GraphText(shown.graph.clone()),
                            text_pane::Tab::Selection => carry_mod::TextTarget::SelectionText(shown.key.clone()),
                        }
                    };
                    Some((carry_mod::Place::Text { target, text: shown.text.clone(), byte: *byte }, *dropped))
                }
                _ => None,
            })
            .collect()
    };
    if let Some(shown) = text_shown {
        found.extend(text_drops(text, shown, &result.text_intents));
    }
    for (key, shown, intents) in &result.other_texts {
        let code = result.locals.iter().find(|(k, _)| k == key).map(|(_, l)| l.code.clone()).unwrap_or_else(|| fresh_local().code);
        found.extend(text_drops(&code, shown, intents));
    }
    found.extend(result.drops.iter().cloned());
    let _ = value;
    found.into_iter().next().map(|(over, dropped)| CarryReport { over, dropped })
}

/// `[y]`: the payload is held by the handle (no pointer capture); a press or Enter puts it.
pub fn pick_up(value2: &EditorModel, actions: &[Action], frame_: &frame::Frame, report: Option<CarryReport>) -> (Option<Carry>, Option<String>) {
    match &value2.carry {
        Some(c) => (Some(Carry { report, ..c.clone() }), None),
        None if actions.contains(&Action::PickUp) => match carry_payload(&value2.into()) {
            Ok(payload) => {
                value2.ui.borrow_mut().carry(None, &payload.kind, &payload.value);
                (
                    Some(Carry {
                        payload,
                        via: Via::Keys,
                        original: value2.doc.clone(),
                        settled: value2.cook.pieces.clone(),
                        report: None,
                        targets: None,
                        chosen: None,
                        anchor: Some(frame_.input.pointer),
                        preview: None,
                        resting: None,
                        hint: None,
                    }),
                    None,
                )
            }
            Err(message) => (None, Some(message)),
        },
        None => (None, None),
    }
}

/// What the camera is asked to frame: the object a `F` in a graph names, or every cooked object.
pub fn framing_step(value2: &EditorModel, doc: &Document, cooked: &cook::UpdateResult, frame_request: Option<(i64, i64)>, actions: &[Action], frame_: &frame::Frame, result: &FrameResult) -> (Option<i64>, Option<Option<Bounds>>) {
    // Framing: local bounds move into the world with their object; at the scene level [F] frames
    // every cooked object.
    let world_bounds = |id: i64, b: &Bounds| cook::transformed(&objects::world(&doc.scene, id), b);
    // a framing cook may finish frames after its request: the object is remembered until it does
    let framing = match frame_request {
        Some((id, _)) => Some(id),
        None => value2.framing,
    };
    let framed = match (&cooked.framed, framing) {
        (Some(Some(b)), Some(id)) => Some(Some(world_bounds(id, b))),
        (framed, _) if actions.contains(&Action::FrameCamera) && pane_level(&value2.into()) == Level::Scene => {
            let _ = framed;
            let view = match active_view(&value2.into(), &geometry(&value2.into(), &result.workspace, frame_)) {
                Some(leaf) => match &leaf.panel {
                    Panel::View(key) if doc.shell.as_ref().is_some_and(|sh| sh.views.iter().any(|(k, _)| k == key)) => View::Only(key.clone()),
                    _ => View::Primary,
                },
                None => View::Primary,
            };
            let mut union: Option<Bounds> = None;
            for piece in cooked.cook.pieces.iter() {
                if !view_wants(value2, &view, piece.id) {
                    continue;
                }
                if let Some(b) = cook::piece_bounds(piece) {
                    let b = world_bounds(piece.id, &b);
                    union = Some(match union {
                        None => b,
                        Some(box_) => cook::union(&box_, &b),
                    });
                }
            }
            Some(union)
        }
        (framed, _) => framed.clone(),
    };
    (framing, framed)
}

/// The graph panel's own projection: list and text views are panels of their own now (`/ l`
/// retypes), so opening a graph is the only thing that changes it.
pub fn projections_of(outlined: &Option<(String, Option<Path>)>, actions: &[Action], result: &FrameResult, value2: &EditorModel) -> LevelMap {
    let level = pane_level(&value2.into());
    let views = pane_views(&value2.into()).clone();
    if result.open_graph.is_some() || outlined.is_some() {
        return level_map_add(views, level, GraphView);
    }
    match result.view_pick {
        Some(view) if view != TextView || graph_name(&value2.into()).is_some() => level_map_add(views, level, view),
        Some(_) => views,
        None => {
            let retype = actions.iter().find_map(|a| match a {
                Action::PanelRetype(p @ (Panel::Graph | Panel::List | Panel::Lisp)) if value2.focus == Panel::Graph => Some(p.clone()),
                _ => None,
            });
            match retype {
                Some(Panel::Graph) => level_map_add(views, level, GraphView),
                Some(Panel::List) => level_map_add(views, level, ListView),
                Some(_) if graph_name(&value2.into()).is_some() => level_map_add(views, level, TextView),
                _ => views,
            }
        }
    }
}

/// Command-Z / Shift-Command-Z: the history step, its notice and the document it leaves.
pub fn undo_step(value: &EditorModel, carrying: bool, actions: &[Action], notice: Option<(NoticeKind, String)>, h: history::EditorHistory) -> (history::EditorHistory, Arc<Document>, bool, Option<(NoticeKind, String)>) {
    // an entry holds no lowering: stepping there lowers its text again, unless it is the last present
    let current = history::present_arc(&h);
    let factories = value.factories.clone();
    let restore = move |saved: &document::Snapshot| lowering::restore(Some(&factories), &current, saved);
    let redo = actions.contains(&Action::Redo);
    let undo = actions.contains(&Action::Undo);
    let stepped = if redo {
        history::redo(&restore, &h)
    } else if undo {
        history::undo(&restore, &h)
    } else {
        None
    };
    let refused = match &stepped {
        Some(Err(diagnostic)) => Some((NoticeKind::Refusal, format!("Undo refused: {}", rays_diagnostic::to_string(diagnostic)))),
        _ => None,
    };
    let was_step = refused.is_some();
    let stepped = match stepped {
        Some(Ok(next)) => Some(next),
        _ => None,
    };
    let notice = match &stepped {
        None if was_step => refused,
        None if redo => Some((NoticeKind::Refusal, "Redo: nothing to redo".to_string())),
        None if undo => Some((NoticeKind::Refusal, "Undo: nothing to undo".to_string())),
        None => notice,
        Some(_) if redo => history::redo_label(&h).map(|label| (NoticeKind::Info, format!("Redo {label}"))),
        Some(_) => Some((NoticeKind::Info, format!("Undo {}", history::label(&h)))),
    };
    match stepped {
        Some(next) => {
            let doc = history::present_arc(&next);
            (next, doc, true, notice)
        }
        None => {
            let doc = if carrying { value.doc.clone() } else { history::present_arc(&h) };
            (h, doc, false, notice)
        }
    }
}

/// The pointer and keys of the sketch go to the pane in focus, else the first viewer.
pub fn view_input(value: &EditorModel, result: &FrameResult) -> frame::Frame {
    let is_view = |p: &Panel| matches!(p, Panel::View(_));
    let owner = match result.pane_keys.iter().find(|(_, (panel, _))| *panel == result.focus && is_view(panel)) {
        Some((key, _)) => Some(*key),
        None => result.pane_keys.iter().find_map(|(key, (panel, _))| if is_view(panel) { Some(*key) } else { None }),
    };
    value.ui.borrow().input(owner)
}

pub fn timeline_step(value: &EditorModel, timeline: timeline::Timeline, timeline_changes: Vec<timeline::Change>, result: &FrameResult) -> (timeline::Timeline, Vec<timeline::Change>, i64) {
    let mut timeline = timeline;
    let mut changes = timeline_changes;
    for intent in &result.timeline_intents {
        let (next, emitted) = match intent {
            TimelineIntent::PauseToggle => timeline::toggle_pause(&timeline),
            TimelineIntent::StopPlayback => timeline::stop(&timeline),
            TimelineIntent::ResetPlayback => timeline::reset(&timeline),
            TimelineIntent::SeekPlayback(frame_) => timeline::seek(&timeline, *frame_),
            TimelineIntent::SetEnd(_) => (timeline.clone(), Vec::new()),
        };
        timeline = next;
        changes.extend(emitted);
    }
    // the End field of a tall timeline: the scrub range (view state, like the playhead)
    let mut frames = value.timeline_frames;
    for intent in &result.timeline_intents {
        if let TimelineIntent::SetEnd(last) = intent {
            frames = (*last).max(1);
        }
    }
    (timeline, changes, frames)
}

/// What the frame around the panes hands the reducer.
#[derive(Clone)]
pub struct FrameInput<'a> {
    pub carry_changed: bool,
    pub all_ui_visible: bool,
    pub view_state: Rc<dyn Fn(Option<&PanelResult>) -> Syntax>,
    pub carrying: bool,
    pub held_keys: Vec<input::Key>,
    pub leader: router::State,
    pub frame: &'a frame::Frame,
    pub hud: Option<(String, f64)>,
    pub actions: Vec<Action>,
    pub guide: bool,
    pub copied: Option<(NoticeKind, String)>,
    pub steady: i64,
    pub status_fps: Option<i64>,
    pub status_fps_at: f64,
    pub timeline: timeline::Timeline,
    pub timeline_changes: Vec<timeline::Change>,
    pub graph_shown: bool,
    pub text: text_pane::State,
    pub text_shown: Option<text_pane::Shown>,
    pub row_sets: Vec<(RowSource, (Option<i64>, Option<i64>), (Vec<tree::Row>, Vec<String>))>,
    pub rows: Vec<tree::Row>,
}

fn same_workspace_all(a: &document::Workspace, b: &document::Workspace) -> bool {
    same_workspace(&a.ws, &b.ws) && crate::editor_inspect::same_lowered(&a.lowered, &b.lowered)
}

fn same_pieces(a: &[cook::Piece], b: &[cook::Piece]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(x, y)| Rc::ptr_eq(&x.bounds, &y.bounds))
}

pub fn reduce(gpu: Option<&flow_ir::flow_ir::gpu::Config>, pump: Option<&dyn Fn()>, input: FrameInput<'_>, owned: &mut EditorModel, result: FrameResult) -> Update {
    let value = &*owned;
    let _timer = flow_syntax::phase_timer::measure("reduce");
    let FrameInput {
        carry_changed,
        all_ui_visible,
        view_state,
        carrying,
        held_keys,
        leader,
        frame: frame_,
        hud,
        actions,
        guide,
        copied,
        steady,
        status_fps,
        status_fps_at,
        timeline,
        timeline_changes,
        graph_shown,
        text,
        text_shown,
        row_sets,
        rows,
    } = input;
    let mut result = result;
    if let Some(PromptIntent::EditSource(op)) = &result.prompt_intent {
        result.changes.push(Change::SyntaxEdit(op.clone()));
    }
    // i on a collapsed function or loop opens it in place, as a double click does
    let mut actions = actions;
    if let [zone] = scope::selected(pane_view(&value.into())).as_slice() {
        if actions.contains(&Action::Enter) && scope::collapsed_zone(pane_view(&value.into()), zone) {
            result.scope_changes.push(scope::Change::ZoneCollapsed { zone: zone.clone(), collapsed: false });
            actions.retain(|a| *a != Action::Enter);
        }
    }
    let carry_report = carry_report_of(value, carrying, &text, text_shown.as_ref(), &result);
    if carrying {
        result.changes = Vec::new();
        result.handle_changes = None;
        result.settings_changes = Vec::new();
        result.text_intents = Vec::new();
        result.other_texts = Vec::new();
        result.menu_pick = None;
        result.tree_intents.retain(|i| matches!(i, tree::Intent::Select(_)));
        result.scope_changes.retain(|c| matches!(c, scope::Change::Selected(_) | scope::Change::Activated(_) | scope::Change::DropOver { .. } | scope::Change::Dropped { .. } | scope::Change::Notice(_)));
        result.prompt_intent = match result.prompt_intent.take() {
            Some(intent @ PromptIntent::Go(_)) => Some(intent),
            _ => None,
        };
    }
    let guide_error = if guide == value.guide {
        None
    } else {
        match save_guide(&value.preferences, guide) {
            Ok(()) => None,
            Err(message) => Some((NoticeKind::Refusal, format!("Guide preference not saved: {message}"))),
        }
    };
    // The one reduction phase: panes have finished constructing their boxes.
    // the fields of an object the text declares are written to the text (below, with the frame's
    // gestures); only an object the host made is edited as a derived node, for reconcile to adopt
    let level0 = pane_level(&value.into());
    let in_text = |node: i64| sync::in_text(&value.doc, level0, node);
    let host_changes: Vec<Change> = result
        .changes
        .iter()
        .filter(|c| match c {
            Change::SetParameter { node, .. } | Change::Rename { node, .. } => !in_text(*node),
            _ => true,
        })
        .cloned()
        .collect();
    let (document, edit_error, editor_effects) = host_changes.iter().fold((level_objects(&value.into()).into_owned(), value.edit_error.clone(), Effects::default()), |state, c| apply_change(state, c));
    // what a list or the inspector's name row asks of an object the text has is written to the text
    // (below); the derived network is edited only for the others
    let mut tree_edits: Vec<Edit> = result.tree_intents.iter().filter_map(|i| tree_edit(&value.into(), &rows, i)).collect();
    for c in &result.changes {
        if let Change::Rename { node, label } = c {
            if in_text(*node) {
                tree_edits.push(Edit::Rename(*node, label.clone()));
            }
        }
    }
    let applied = result.tree_intents.iter().fold(
        TreeState { document, selection: result.selection.clone(), tree: result.tree.clone(), opened: result.opened, label: None, rows: rows.clone() },
        |state, intent| apply_tree(&value.into(), state, intent),
    );
    let TreeState { document, selection, tree: tree_value, opened, label: tree_label, .. } = applied;
    let mut edit_error = edit_error;
    let mut document = document;
    let mut parameter_effects = Effects::default();
    let handle = match &result.handle_changes {
        Some((id, _)) if in_text(*id) => None,
        other => other.clone(),
    };
    if let Some((node_id, values)) = handle {
        match kg::set_fields(&document, node_id, &values) {
            Ok((doc, changed)) => {
                document = doc;
                parameter_effects = param::union_effects(parameter_effects, changed);
            }
            Err(message) => edit_error = Some(message),
        }
    }
    let (new_settings, settings_effects) = if result.settings_changes.is_empty() {
        (value.doc.settings.clone(), Effects::default())
    } else {
        match settings::apply(&value.doc.settings, &result.settings_changes) {
            Ok((st, effects)) => (st, effects),
            Err(message) => {
                edit_error = Some(message);
                (value.doc.settings.clone(), Effects::default())
            }
        }
    };
    let node_effects = |effects: Effects| -> Effects {
        match pane_level(&value.into()) {
            Level::Inside(id) if kind(value, id).as_deref() == Some("geometry") => effects,
            _ => Effects { cook: false, ..effects },
        }
    };
    let settings_changed = !settings::same_values(&new_settings, &value.doc.settings);
    let label = match (result.changes.iter().find_map(intent_label), tree_label) {
        (Some(label), _) | (None, Some(label)) => label,
        (None, None) if settings_changed => "Settings".to_string(),
        (None, None) => match &result.handle_changes {
            Some((id, _)) => format!("Edit {}", kg::find(&document, *id).map(|n| n.label).unwrap_or_else(|| id.to_string())),
            None => "Edit".to_string(),
        },
    };
    let result_document_unchanged = kg::same(&document, &level_objects(&value.into()));
    result.document = document;
    result.selection = selection;
    result.tree = tree_value;
    result.opened = opened;
    result.edit_error = edit_error;
    result.effects = param::union_effects(node_effects(param::union_effects(editor_effects, parameter_effects)), settings_effects);
    result.settings = new_settings;
    result.label = label;
    let (timeline, timeline_changes, timeline_frames) = timeline_step(value, timeline, timeline_changes, &result);
    let (prompt, notice, loaded) = prompt_step(value, &*view_state, &result);
    let loaded_flag = loaded.is_some();
    // Shared undo stack: every document change (a parameter, a workspace gesture, an inspector
    // commit) becomes one history entry; Command/Ctrl-Z undoes, Shift-Command/Ctrl-Z or Ctrl-Y redoes.
    let present: Arc<Document> = value.doc.clone();
    let (mut next, loaded_view) = match loaded {
        Some(preset) => (Arc::new(preset.doc), Some(preset.view)),
        None => ( {
            // the host's objects or layers as this frame's derived edits leave them
            if result_document_unchanged || network_present(&value.into()) {
                present.clone()
            } else {
                let doc = document::with_kinds(&present, pane_level(&value.into()), result.document.clone());
                Arc::new(if pane_level(&value.into()) == Level::Scene { document::prune(&doc) } else { doc })
            }
        }, None)
    };
    // Enter and row activation share camera selection and look-through.
    let entered_camera: Option<i64> = if pane_level(&value.into()) != Level::Scene || loaded_flag || carrying {
        None
    } else {
        let candidate = match result.opened {
            Some(id) => Some(id),
            None if actions.contains(&Action::Enter) => selection::selected(&result.selection),
            None => None,
        };
        match candidate {
            Some(id) if kind(value, id).as_deref() == Some("camera") && view_wants(value, &View::Primary, id) => Some(id),
            _ => None,
        }
    };
    if !settings::same_values(&result.settings, &value.doc.settings) {
        tree_edits.push(Edit::Settings(result.settings.clone()));
    }
    if let Some(id) = entered_camera {
        if next.active_camera != Some(id) {
            tree_edits.push(Edit::Camera(Some(id)));
        }
    }
    let (written, written_result, added, created, renamed, edit_note) = write_gestures(&value.into(), loaded_flag, &present, &tree_edits, &in_text, &node_effects, next, result);
    next = written;
    result = written_result;
    // The workspace pane's layout gestures: moving an item and collapsing a zone edit the layout
    // keys (one history entry each); the probe is view state and never reaches history.
    // a node added from the menu lands where the menu was opened
    let mut scope_changes = result.scope_changes.clone();
    if let ([path], Some(_), Some(menu)) = (&added[..], &result.menu_pick, &value.menu) {
        let sc = path[..path.len().saturating_sub(1)].to_vec();
        let (mx, my) = rays_graph::node_menu::position(menu);
        if let Some((x, y)) = scope::scope_point(pane_view(&value.into()), &sc, mx as f64, my as f64) {
            scope_changes.insert(0, scope::Change::Moved(vec![(path.clone(), x, y)]));
        }
    }
    let (laid_out, laid_result, probes) = layout_gestures(value, loaded_flag, &added, next, result, &scope_changes);
    next = laid_out;
    result = laid_result;
    let before_world = next.clone();
    // a World key is written to the text (below); only a World the host made is edited here
    let (mut world_label, mut world_edits) = (None, Vec::new());
    if in_world(&value.into()) {
        let (doc, label, edits) = world_keys(&value.into(), &next, &result.selection, &actions);
        next = doc;
        world_label = label;
        world_edits = edits;
    }
    // / e opens the World, writing one (a [scene/world] member and its world graph) on first use.
    let mut world_added = false;
    if objects::ids("world", &next.scene).is_empty() && actions.contains(&Action::GoWorld) {
        if let Ok(ops) = sync::add_world(&next) {
            if let Ok(doc) = document_edit::syntax_batch(Some(&value.factories), &next, &ops) {
                next = Arc::new(doc);
                world_added = true;
            }
        }
    }
    if !loaded_flag {
        let (doc, res) = reconciled(value, &before_world, next, result);
        next = doc;
        result = res;
        for (level, edit) in &world_edits {
            if result.edit_error.is_some() {
                continue;
            }
            match sync::write(Some(&value.factories), &next, *level, edit) {
                Ok(Some((doc, _))) => next = Arc::new(doc),
                Ok(None) => {}
                Err(message) => result.edit_error = Some(message),
            }
        }
    }
    let input_frame = view_input(value, &result);
    let gesture = gesture_key(&value.into(), &frame_, loaded_flag, world_added, &world_label, &result);
    let history_after = if Arc::ptr_eq(&next, &present) || same_document(&next, &present) {
        value.history.clone()
    } else {
        let label = if matches!(result.prompt_intent, Some(PromptIntent::LoadLastState)) && loaded_flag {
            "Restore last edited state".to_string()
        } else if loaded_flag {
            "Load preset".to_string()
        } else if world_added {
            "Add World".to_string()
        } else if let Some(l) = &world_label {
            l.clone()
        } else {
            result.label.clone()
        };
        let merge = if result.changes.iter().any(|c| matches!(c, Change::LayoutEdit(layout_edit::Op::SetLayout { .. }))) {
            // keys: no drag seals it
            Merge::Burst { key: "layout".to_string(), at: frame_.input.t, window: 1.5 }
        } else {
            match gesture {
                Some(key) => Merge::Gesture(key),
                None => Merge::Step,
            }
        };
        let arc = adopt(&value.history, next);
        commit(&arc, Some(&label), Some(merge), value.history.clone())
    };
    let (history2, doc, undone, notice) = undo_step(value, carrying, &actions, notice, history_after);
    let effects = if undone || loaded_flag || carry_changed { param::union_effects(result.effects, document_edit::cook_effects()) } else { result.effects };
    // The open level must still exist after undo or a preset load.
    let level = document::resolve_level(&doc, if loaded_flag { Level::Scene } else { pane_level(&value.into()) });
    let previous_doc = value.doc.clone();
    let previous_cook = value.cook.clone();
    let previous_level = pane_level(&value.into());
    let had_route = !pane_route(&value.into()).is_empty();
    let previous_viewed: Vec<_> = value.viewed.iter().map(|(id, (network, root))| (*id, flow_sop::network::identity(network), *root)).collect();
    let scope_selection_changed = scope::selected(&result.scope_view) != scope::selected(pane_view(&value.into()));
    let vocab = vocabulary_for(value, &doc);
    let workspace = unrestore(&value.doc, &doc, result.workspace.clone());
    // i also follows a reference of the node selected in the pane (a material, an object's graph)
    let activated = result.scope_changes.iter().find_map(|c| match c {
        scope::Change::Activated(path) => Some(path.clone()),
        _ => None,
    });
    let followed = match activated {
        Some(path) => follow_target(&value.into(), Some(&path)),
        None if actions.contains(&Action::Enter) => follow_target(&value.into(), None).or_else(|| viewport_target(&value.into())),
        None => None,
    };
    let picked_row = result.tree_intents.iter().find_map(|i| match i {
        tree::Intent::Select(ids) if ids.len() == 1 => Some(ids[0]),
        _ => None,
    });
    let mut picked_view = result.scope_view.clone();
    if added.is_empty() {
        // A row of a geometry object's list selects its source node in the old pane.
        match picked_row {
            Some(id) if matches!(pane_level(&value.into()), Level::Inside(oid) if kind(value, oid).as_deref() == Some("geometry")) => match node_path(&value.into(), id) {
                Some(path) => scope::select(vec![path], &mut picked_view),
                None => {},
            },
            _ => {},
        }
    } else {
        scope::select(added.clone(), &mut picked_view)
    };
    let value2 = owned;
    value2.timeline_frames = timeline_frames;
    value2.set_doc(doc.clone());
    value2.lisp_vocab = vocab;
    value2.workspace = workspace;
    pane_mut(value2).at = level;
    // the selection keeps the nodes that still exist in the open network
    let mut selection = if level != previous_level || loaded_flag {
        selection::empty()
    } else {
        let current = selection::selected_nodes(&result.selection).to_vec();
        let ids: Vec<i64> = current.iter().copied().filter(|id| lists(&(&*value2).into(), *id)).collect();
        if ids == current { result.selection.clone() } else { selection::select_nodes(&ids) }
    };
    let (followed, target) = level_target(followed, had_route, &*value2, level, &doc, &selection, &actions, &result);
    if let Some(id) = result.open_graph {
        if lists(&(&*value2).into(), id) {
            selection = selection::select(id);
        }
    }
    // / e selects the scene's World, then enters it
    if let Some(Level::Inside(id)) = target {
        if actions.contains(&Action::GoWorld) && level == Level::Scene {
            selection = selection::select(id);
        }
    }
    value2.menu = result.menu.clone();
    value2.tree = result.tree.clone();
    if value2.probes != probes {
        value2.revs.views += 1;
        value2.probes = probes;
    }
    value2.open_import = scope_changes
        .iter()
        .find_map(|c| match c {
            scope::Change::OpenImport(file) => Some(file.clone()),
            _ => None,
        })
        .or_else(|| value2.open_import.clone());
    {
        let pane = pane_mut(value2);
        pane.picked = selection.clone();
        pane.view = picked_view;
    }
    if let Some(level) = target {
        if level != pane_level(&(&*value2).into()) {
            remember(value2);
        }
        open_level(value2, level);
    }
    // u goes back along the route taken
    let pops = actions.iter().fold(0i64, |n, a| match a {
        Action::Up => n.max(1),
        Action::Back(k) => n.max(*k),
        _ => n,
    });
    let route = pane_route(&(&*value2).into()).to_vec();
    if pops > 0 && target.is_none() && followed.is_none() {
        if let Some(((back_level, pane_graph), rest)) = route.get((pops - 1).max(0) as usize..).and_then(|r| r.split_first()) {
            let back_level = document::resolve_level(&doc, *back_level);
            let rest = rest.to_vec();
            let pane_graph = pane_graph.clone();
            pane_mut(value2).route = rest.clone();
            open_level(value2, back_level);
            pane_mut(value2).shown = pane_graph.clone();
        }
    }
    if let Some(graph) = &followed {
        go(value2, graph);
    }
    // In the World, / m flips the view pane to the lat-long map.
    let map_view = if actions.contains(&Action::ToggleMap) && in_world(&(&*value2).into()) { !value2.map_view } else { value2.map_view && in_world(&(&*value2).into()) };
    value2.map_view = map_view;
    // a renamed graph keeps being the one shown, and the one [u] returns to
    if let Some((old, now)) = &renamed {
        let name = |g: &Option<String>| g.as_ref().map(|g| if g == old { now.clone() } else { g.clone() });
        { let pane = pane_mut(value2); pane.shown = name(&pane.shown); pane.route = pane.route.iter().map(|(level, g)| (*level, name(g))).collect(); };
    }
    let outlined = outline_step(&created, &result, value2);
    if let Some(PromptIntent::FindNode(path)) = &result.prompt_intent {
        select_source(value2, path);
    }
    let projections = projections_of(&outlined, &actions, &result, &*value2);
    let selection_changed = scope_selection_changed || outlined.is_some();
    let mut text = text;
    if selection_changed {
        text.target = None;
    }
    let locals: Vec<(crate::panel_map::Key, Local)> = if selection_changed {
        result
            .locals
            .iter()
            .map(|(key, l)| {
                let mut l = l.clone();
                l.code.target = None;
                (key.clone(), l)
            })
            .collect()
    } else {
        result.locals.clone()
    };
    value2.text = text;
    value2.rows = Rc::new(row_sets);
    value2.live_cook = result.live_cook;
    pane_mut(value2).views = projections;
    value2.locals = locals;
    for c in &result.changes {
        if let Change::FollowSource(path) = c {
            select_source(value2, path);
        }
    }
    value2.timeline = timeline.clone();
    value2.history = history2;
    apply_text(value2, &result.text_intents);
    // each other text pane's intents fold into its own state
    for (key, _, intents) in &result.other_texts {
        apply_text_at(value2, key, intents);
    }
    sync_scope(value2);
    for c in &scope_changes {
        if let scope::Change::DisplaySet(path) = c {
            view_node(value2, path);
        }
    }
    sync_scope(value2);
    let doc = value2.doc.clone();
    let frame_request: Option<(i64, i64)> = match (result.frame_request, pane_level(&(&*value2).into())) {
        (Some(node), Level::Inside(id)) if kind(&*value2, id).as_deref() == Some("geometry") => Some((id, node)),
        _ => None,
    };
    if !same_workspace_all(&doc.workspace, &previous_doc.workspace)
        || !value2.viewed.iter().map(|(id, (network, root))| (*id, flow_sop::network::identity(network), *root)).eq(previous_viewed) {
        let roots: BTreeSet<i64> = value2.viewed.values().map(|(_, root)| *root).collect();
        let volatile = flow_sop::lower::is_volatile(&doc.workspace.lowered);
        cook::set_volatile(&previous_cook, Arc::new(move |id| volatile(id) || roots.contains(&id)));
    }
    let probes_wanted: Vec<(i64, i64)> = match pane_laid(&(&*value2).into()) {
        Some(k) if scope_name(&(&*value2).into()).is_some() => {
            // only the footers drawn and the selection are counted, not every node of the graph
            let view = pane_view(&(&*value2).into());
            let mut shown = rays_graph::scope_frame::inspect::footer_paths(view);
            shown.extend(scope::selected(view));
            k.targets.iter().filter_map(|(path, target)| if shown.contains(path) { Some(*target) } else { None }).collect()
        }
        _ => Vec::new(),
    };
    let (lit, lit_cache) = lit_tags(&(&*value2).into());
    let default_pump = || std::thread::sleep(std::time::Duration::from_micros(500));
    let pump_ref: &dyn Fn() = match pump {
        Some(p) => p,
        None => &default_pump,
    };
    let gpu_config = gpu.cloned().unwrap_or_else(|| value2.gpu.clone());
    let cooked = cook::update(
        Some(&gpu_config),
        Some(value2.live_frame.clone()),
        result.live_cook,
        &probes_wanted,
        &lit,
        Some(pump_ref),
        &previous_cook,
        &doc.settings,
        &geometry_objects(&*value2),
        result.edit_error.clone(),
        effects,
        &timeline_changes,
        &timeline,
        &frame_,
        frame_request,
    );
    let (framing, framed) = framing_step(&*value2, &doc, &cooked, frame_request, &actions, &frame_, &result);
    let document_changed = !Arc::ptr_eq(&doc, &previous_doc);
    let (carry_value, pick_error) = pick_up(&*value2, &actions, &frame_, carry_report);
    // a cook that gave back every piece it had (the text changed, the objects did not) leaves
    // nothing to recompose, and no accumulating render to restart
    let prepared_changed = cooked.prepared_changed && !same_pieces(cook::pieces(&cooked.cook), cook::pieces(&previous_cook));
    let new_notice = if guide_error.is_some() {
        guide_error
    } else if let Some(m) = pick_error {
        Some((NoticeKind::Refusal, m))
    } else if copied.is_some() {
        copied
    } else if document_changed && !loaded_flag && !undone {
        edit_note.map(|m| (NoticeKind::Info, m))
    } else {
        notice
    };
    let notice_changed = match (&new_notice, &value2.notice) {
        (Some((ka, ta)), Some((kb, tb))) => ka != kb || ta != tb,
        (None, None) => false,
        _ => true,
    };
    let queued = match &result.prompt_intent {
        Some(PromptIntent::RunAction(a)) => vec![a.clone()],
        _ if all_ui_visible && actions.contains(&Action::AddNode) && !graph_shown && has_panel(&(&*value2).into(), &Panel::Graph) => vec![Action::AddNode],
        _ => Vec::new(),
    };
    let notice_at = value2.notice_at;
    let core = value2;
    core.timeline = timeline;
    core.cook = std::rc::Rc::new(cooked.cook);
    core.lit = lit_cache;
    core.edit_error = cooked.edit_error.clone();
    core.status_fps = status_fps;
    core.status_fps_at = status_fps_at;
    core.last_dt = frame_.input.dt;
    core.steady = steady;
    core.guide = guide;
    core.hud = hud;
    core.focus = result.focus.clone();
    core.focus_path = result.focus_path.clone();
    core.pane_keys = Rc::new(result.pane_keys.clone());
    panel_map::with_others(&mut core.panes, result.graph_panes.clone());
    core.leader = leader;
    core.held_keys = held_keys;
    core.prompt = prompt;
    core.queued = queued;
    core.carry = carry_value;
    core.framing = if cooked.framed.is_some() { None } else { framing };
    core.notice = new_notice;
    core.notice_at = if notice_changed { frame_.input.t } else { notice_at };
    let scene_changed = carry_changed || !kg::same(&doc.scene, &previous_doc.scene) || prepared_changed;
    let mut out_actions = actions.clone();
    if entered_camera.is_some() && entered_camera == doc.active_camera {
        out_actions.push(Action::LookThroughCamera);
    }
    Update {
        effects,
        prepared_changed,
        scene_changed,
        framed,
        loaded_view,
        actions: out_actions,
        panel: result.panel.clone(),
        input: input_frame,
    }
}

fn network_present<'a>(value: &ModelView<'a>) -> bool {
    crate::editor_inspect::network(value).is_some()
}

/// One reduction with no panes built, for tests of the reducer: the keys are `actions`, the list's
/// selection is `select`, and `preview` puts a payload in flight whose hot target shows that edit
/// on a scratch document (as `carry_step` leaves the model while a target is hot).
pub fn reduce_idle(select: Option<&[i64]>, preview: Option<&Op>, value: &EditorModel, actions: &[Action], frame_: &frame::Frame) -> Result<EditorModel, String> {
    let mut value = value.clone();
    value.live_frame = flow_native::live_frame::of_frame(frame_);
    if let Some(op) = preview {
        let doc = document_edit::syntax_edit(Some(&value.factories), &value.doc, op)?;
        let doc = Arc::new(doc);
        value.lisp_vocab = vocabulary_for(&value, &doc);
        value.carry = Some(Carry {
            payload: carry_mod::Payload { kind: "sop".to_string(), value: String::new() },
            via: Via::Keys,
            original: value.doc.clone(),
            settled: value.cook.pieces.clone(),
            report: None,
            targets: None,
            chosen: None,
            anchor: None,
            resting: None,
            hint: None,
            preview: Some(CarryPreview::Showing { place: carry_mod::Place::Graph(String::new()), doc: doc.clone(), what: String::new() }),
        });
        value.set_doc(doc);
    }
    let selection = match select {
        Some(ids) => selection::select_nodes(ids),
        None => pane_selection(&(&value).into()).clone(),
    };
    let input = idle_input(&(&value).into(), actions, frame_);
    let result = idle_result(&(&value).into(), selection);
    reduce(None, None, input, &mut value, result);
    Ok(value)
}

/// The frame result of a frame in which no pane asked for anything.
pub fn idle_result<'a>(value: &ModelView<'a>, selection: selection::Selection) -> FrameResult {
    FrameResult {
        workspace: value.workspace.clone(),
        focus: value.focus.clone(),
        focus_path: value.focus_path.clone(),
        pane_keys: (*value.pane_keys).clone(),
        scope_view: pane_view(value).clone(),
        scope_changes: Vec::new(),
        selection,
        menu: value.menu.clone(),
        menu_pick: None,
        tree: value.tree.clone(),
        outline: value.outline.clone(),
        outline_intents: Vec::new(),
        document: level_objects(value).into_owned(),
        edit_error: value.edit_error.clone(),
        effects: Effects::default(),
        timeline_intents: Vec::new(),
        frame_request: None,
        prompt: value.prompt.clone(),
        prompt_intent: None,
        panel: None,
        grab: false,
        settings: value.doc.settings.clone(),
        opened: None,
        live_cook: value.live_cook,
        label: "Edit".to_string(),
        changes: Vec::new(),
        tree_intents: Vec::new(),
        text_intents: Vec::new(),
        open_graph: None,
        settings_changes: Vec::new(),
        graph_panes: panel_map::others(&value.panes),
        locals: value.locals.clone(),
        other_texts: Vec::new(),
        handle_changes: None,
        bar_action: None,
        view_pick: None,
        drops: Vec::new(),
    }
}

/// The reducer's input for a frame without panes.
pub fn idle_input<'a, 'f>(value: &ModelView<'a>, actions: &[Action], frame_: &'f frame::Frame) -> FrameInput<'f> {
    FrameInput {
        carry_changed: false,
        all_ui_visible: false,
        view_state: Rc::new(|_| {
            let mut builder = flow_syntax::arena::SyntaxBuilder::new();
            let fields = builder.range(&[]);
            let id = builder.push(0, SNode::Map(fields), Default::default(), None);
            builder.freeze().handle(id)
        }),
        carrying: value.carry.is_some(),
        held_keys: value.held_keys.clone(),
        leader: value.leader.clone(),
        frame: frame_,
        hud: value.hud.clone(),
        actions: actions.to_vec(),
        guide: value.guide,
        copied: None,
        steady: value.steady,
        status_fps: value.status_fps,
        status_fps_at: value.status_fps_at,
        timeline: value.timeline.clone(),
        timeline_changes: Vec::new(),
        graph_shown: false,
        text: value.text.clone(),
        text_shown: None,
        row_sets: Vec::new(),
        rows: Vec::new(),
    }
}

