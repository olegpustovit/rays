//! The text pane's applies: drafts, merges, scrubs and the save-time bookkeeping of unsaved work.
//! Port of lib/rays_editor/editor_text.ml.
//!
//! Optional arguments of the OCaml functions are trailing `Option`s.
#![allow(non_camel_case_types)]
use flow_syntax::symbol::Symbol;
use std::sync::Arc;

use editor_document::document::{self, Level, T as Document};
use editor_document::document_edit;
use editor_document::settings;
use editor_document::workspace_doc::{self, T as WorkspaceDoc};
use flow_graph::flow_edit::Op;
use flow_graph::projection as fproj;
use flow_syntax::lisp;
use param::Value;
use pxui_graph::scope_commands;
use pxui_graph::scope_state as scope;
use rays_diagnostic::{self as diag, Diagnostic, Position};

use crate::cook;
use crate::editor_history::{self as history, Merge};
use crate::editor_inspect::level_key;
use crate::editor_model::*;
use crate::editor_panes::{go, local_of, put_local};
use crate::editor_scope::sync_scope;
use crate::editor_shell::unrestore;
use crate::text_pane::{self, Checked, Intent, Slot, Tab};

type Candidate = Result<Document, Vec<Diagnostic>>;

/// Every editor path records the same immutable document. No-op/rejected edits leave both the
/// present and the merge state untouched.
pub fn commit(doc: &Arc<Document>, label: Option<&str>, merge: Option<Merge>, history: history::EditorHistory) -> history::EditorHistory {
    if Arc::ptr_eq(doc, &history::present_arc(&history)) {
        return history;
    }
    history::record(merge.unwrap_or(Merge::Step), label.unwrap_or("Edit"), doc.clone(), history)
}

/// The document as the history's present, keeping the present's identity when nothing changed.
pub fn adopt(history: &history::EditorHistory, doc: Arc<Document>) -> Arc<Document> {
    let present = history::present_arc(history);
    if Arc::ptr_eq(&present, &doc) || same_document(&present, &doc) { present } else { doc }
}

pub fn parameter_gesture(operation: &str, level: Level, id: i64, values: &[(String, Value)]) -> String {
    let mut names: Vec<&str> = values.iter().map(|(k, _)| k.as_str()).collect();
    names.sort();
    names.dedup();
    format!("{operation}/{}/{id}/{}", level_key(level), names.join("/"))
}

/// A workspace edit installed as the present: the source and its lowering are one history entry,
/// the open level is re-resolved and the cook told about the new volatile nodes.
pub fn install(value: &mut EditorModel, doc: Document, label: &str, merge: Merge) {
    let doc = adopt(&value.history, Arc::new(doc));
    let level = document::resolve_level(&doc, pane_level(&(&*value).into()));
    value.lisp_vocab = vocabulary_for(value, &doc);
    value.workspace = unrestore(&value.doc, &doc, value.workspace.clone());
    value.doc = doc.clone();
    cook::set_volatile(&value.cook, flow_sop::lower::is_volatile(&doc.workspace.lowered));
    value.history = commit(&doc, Some(label), Some(merge), value.history.clone());
    let pane = crate::panel_map::active_mut(&mut value.panes);
    if level != pane.at {
        pane.at = level;
        pane.picked = crate::selection::empty();
    }
}


fn at_line_1(mut d: Diagnostic) -> Diagnostic {
    d.position = Some(Position { line: 1, col: 0 });
    d.span = None;
    d
}

/// Selection patches root bindings into one candidate graph, checked and installed once. Omitted
/// bindings stay and the shown result cannot change. Without a closure the tab shows the graph
/// form, replaced whole.
fn binding_document(value: &EditorModel, path: &[Symbol], text: &str) -> Candidate {
    let ws = &value.doc.workspace.ws;
    match text_pane::graph_op(&ws.source, &path[0], Some(path), text) {
        Err(d) => Err(vec![if d.position.is_none() { at_line_1(d) } else { d }]),
        Ok(op) => match document_edit::syntax_edit_result(Some(&value.factories), &value.doc, &op) {
            Ok(doc) => Ok(doc),
            Err(d) => Err(vec![at_line_1(d)]),
        },
    }
}

/// The Graph tab's apply: the draft must be the one graph (or function) form, which replaces the
/// graph's; atomic like every apply.
fn graph_document(value: &EditorModel, name: &str, text: &str) -> Candidate {
    let ws = &value.doc.workspace.ws;
    match text_pane::graph_op(&ws.source, name, None, text) {
        Err(d) => Err(vec![if d.position.is_none() { Diagnostic { position: Some(Position { line: 1, col: 0 }), ..d } } else { d }]),
        Ok(op) => match document_edit::syntax_edit_result(Some(&value.factories), &value.doc, &op) {
            Ok(doc) => Ok(doc),
            Err(d) => Err(vec![at_line_1(d)]),
        },
    }
}

/// Fold the pane's intents: drafts live in `value.text` (view state); a refused apply keeps the
/// draft and its errors and changes nothing else.
pub fn scrub_merge() -> Merge {
    Merge::Gesture("text-scrub".to_string())
}

pub fn select_source(value: &mut EditorModel, path: &[Symbol]) {
    let Some(graph) = path.first() else { return };
    if graph.starts_with("macro:") {
        return;
    }
    // A missing nested path is a no-op. Keep only the pane and caches touched by navigation.
    let previous = (
        pane(&(&*value).into()).clone(), value.previews.clone(), value.viewed.clone(),
        value.canvas_preview.clone(), value.canvas_viewed.clone(), value.view_key.clone(),
        value.select_later.clone(),
    );
    go(value, graph);
    sync_scope(value);
    let found = pane_laid(&(&*value).into()).is_some_and(|k| k.graph == graph.as_str() && (fproj::find(&k.scope, path).is_some() || k.scope.inputs.iter().any(|input| input.path == path)));
    if !found && path.len() > 1 {
        *crate::panel_map::active_mut(&mut value.panes) = previous.0;
        value.previews = previous.1;
        value.viewed = previous.2;
        value.canvas_preview = previous.3;
        value.canvas_viewed = previous.4;
        value.view_key = previous.5;
        value.select_later = previous.6;
        return;
    }
    value.text.target = None;
    for (_, local) in &mut value.locals {
        local.code.target = None;
    }
    if found {
        if scope::selected(pane_view(&(&*value).into())) != vec![path.to_vec()] {
            value.select_later.clear();
            {
                let view = &mut pane_mut(value).view;
                scope::select(vec![path.to_vec()], view);
                scope_commands::run_command(view, scope::Command::Frame_selection, &mut Vec::new());
            };
        }
    } else {
        scope::select(Vec::new(), &mut pane_mut(value).view);
    }
}

fn conflict() -> Diagnostic {
    diag::error(
        "E_DRAFT_CONFLICT".to_string(),
        "The document changed in the same place as this draft. Your draft is kept; discard it and reapply your edits to the current text.".to_string(),
        None,
        Some(Position { line: 1, col: 0 }),
        None,
        Vec::new(),
        Vec::new(),
    )
}

fn put(mut text: text_pane::State, graph: &str, slot: Option<Slot>) -> text_pane::State {
    text.graphs.retain(|(g, _)| g != graph);
    if let Some(s) = slot {
        text.graphs.insert(0, (graph.to_string(), s));
    }
    text
}

fn of_graph(graph: &str, ws: &WorkspaceDoc) -> Result<String, Diagnostic> {
    text_pane::make_shown(&ws.source, graph, None, Tab::Graph).map(|shown| shown.text)
}

fn one_intent(value: &mut EditorModel, intent: &Intent) {
    let (intent, literal): (&Intent, Option<(Arc<Vec<flow_syntax::syntax::Syntax>>, Op, Position)>) = match intent {
        Intent::LiteralScrub { source, op, position, fallback } => (fallback.as_ref(), Some((source.clone(), op.clone(), position.clone()))),
        other => (other, None),
    };
    let mut text = match intent {
        Intent::DocScrub(..) | Intent::GraphScrub(..) | Intent::BindingScrub(..) => value.text.clone(),
        _ => {
            let mut t0 = value.text.clone();
            t0.scrubbing = false;
            t0.cache = if value.text.scrubbing { None } else { value.text.cache.clone() };
            t0
        }
    };
    let doc = value.doc.clone();
    let workspace: &WorkspaceDoc = &doc.workspace.ws;
    let base = |old: Option<WorkspaceDoc>| -> Option<WorkspaceDoc> { Some(old.unwrap_or_else(|| workspace.clone())) };
    let slot_of = |text: &text_pane::State, graph: &str| -> Option<Slot> { text_pane::graph_slot(text, graph).cloned() };
    let failed = |text: &text_pane::State, graph: &str, draft: &str, errors: Vec<Diagnostic>| -> Option<Slot> {
        Some(Slot { gtext: draft.to_string(), gbase: base(slot_of(text, graph).and_then(|s| s.gbase)), gerrors: Checked { at: draft.to_string(), found: errors } })
    };
    // a draft begun on an older document: merged with what changed since on the syntax tree
    // (`Text_pane::merge3`); only a value both changed keeps the draft as a conflict
    let checked = |metadata: bool, shown: &dyn Fn(&WorkspaceDoc) -> Result<String, Diagnostic>, draft: &str, old: Option<&WorkspaceDoc>, apply: &dyn Fn(&str) -> Candidate| -> Candidate {
        match old {
            Some(previous) if !Arc::ptr_eq(&previous.checked, &workspace.checked) || (metadata && (previous.layout != workspace.layout || !settings::same_values(&previous.settings, &workspace.settings))) => {
                let previous = shown(previous).map_err(|error| vec![error])?;
                let current = shown(workspace).map_err(|error| vec![error])?;
                match text_pane::merge3(&previous, draft, &current) {
                    Some(merged) => apply(&merged),
                    None => Err(vec![conflict()]),
                }
            }
            _ => apply(draft),
        }
    };
    let whole = |ws: &WorkspaceDoc| workspace_doc::to_text(ws);
    let checked_binding = |path: &[Symbol], draft: &str, old: Option<&WorkspaceDoc>, apply: &dyn Fn(&str) -> Candidate| -> Candidate {
        match old {
            Some(previous) if !Arc::ptr_eq(&previous.checked, &workspace.checked) => {
                // Selection can patch a root binding outside the printed upstream closure. Merge
                // the complete candidate graph so a concurrent edit of that binding cannot
                // disappear just because it was outside the printed closure.
                match text_pane::graph_op(&previous.source, &path[0], Some(path), draft) {
                    Err(d) => Err(vec![d]),
                    Ok(Op::SetGraph { name, form }) => {
                        let mine = lisp::print(&[form]).map_err(|error| vec![error])?.0;
                        let previous = of_graph(&name, previous).map_err(|error| vec![error])?;
                        let current = of_graph(&name, workspace).map_err(|error| vec![error])?;
                        match text_pane::merge3(&previous, &mine, &current) {
                            Some(merged) => graph_document(&*value, &name, &merged),
                            None => Err(vec![conflict()]),
                        }
                    }
                    Ok(_) => apply(draft),
                }
            }
            _ => apply(draft),
        }
    };
    let scrub = |apply: &dyn Fn(&str) -> Candidate, draft: &str| -> Candidate {
        match &literal {
            Some((source, op, position)) if **source == workspace.source => document_edit::syntax_edit_result(Some(&value.factories), &value.doc, op)
                .map_err(|d| vec![Diagnostic { position: Some(position.clone()), span: None, ..d }]),
            _ => apply(draft),
        }
    };
    let finished = |done: bool, draft: &str, previous: Option<&WorkspaceDoc>, current: Option<&str>| -> bool {
        done && current == Some(draft) && previous.is_some_and(|p| same_workspace(p, workspace))
    };
    let scrubbed = |done: bool| -> bool { !done && literal.is_some() };

    match intent {
        Intent::PrintError(error) => {
            let message = rays_diagnostic::to_string(error);
            value.edit_error = Some(message.clone());
            value.notice = Some((notice_kind::Refusal, message));
        }
        Intent::Tab(tab) => {
            text.tab = *tab;
            value.text = text
        }
        Intent::Menu(menu) => {
            text.menu = *menu;
            value.text = text
        }
        Intent::ToggleWrap => {
            text.wrap = !text.wrap;
            value.text = text
        }
        Intent::ToggleParinfer => {
            text.parinfer = !text.parinfer;
            value.text = text
        }
        Intent::Picker(picker) => {
            text.picker = *picker;
            value.text = text
        }
        Intent::OpenGraph(graph) => go(value, graph),
        Intent::SelectBinding(path) => {
            value.text = text_pane::State { target: None, ..text };
            select_source(value, path)
        }
        Intent::JumpDefinition(path) => {
            select_source(value, path);
            value.text = text_pane::State { target: Some(path.clone()), nonce: text.nonce + 1, ..text };
        }
        // opens the references prompt, see `Editor_reduce`
        Intent::ReferencesOf(_) => {},
        // read by the carry's report, not an edit
        Intent::CarryOver(..) => {},
        Intent::FindSet(find, reveal) => {
            text.find = find.clone();
            text.nonce += if *reveal { 1 } else { 0 };
            value.text = text
        }
        Intent::RequestSet(request) => {
            text.request = request.clone();
            value.text = text
        }
        Intent::Resolved => {
            text.pending = Vec::new();
            value.text = text
        }
        Intent::DocDraft(draft) => {
            text.draft = Some(draft.clone());
            text.doc_base = base(text.doc_base.clone());
            text.scrubbing = false;
            value.text = text
        }
        Intent::DocDiscard => {
            text.draft = None;
            text.doc_base = None;
            text.doc_errors = text_pane::no_errors();
            value.text = text
        }
        Intent::DocApply(draft) => match checked(true, &whole, draft, text.doc_base.as_ref(), &|d| document_edit::text_edit(None, Some(&value.factories), &value.doc, d)) {
            Ok(doc) => {
                install(value, doc, "Edit text", Merge::Step);
                value.text = text_pane::State { draft: None, doc_base: None, doc_errors: text_pane::no_errors(), ..text };
            }
            Err(found) => {
                text.draft = Some(draft.clone());
                text.doc_base = base(text.doc_base.clone());
                text.doc_errors = Checked { at: draft.clone(), found };
                value.text = text
            }
        },
        Intent::BindingDraft(path, draft) => {
            let kept = match &text.binding_draft {
                Some((old, _)) if old == path => text.binding_base.clone(),
                _ => None,
            };
            text.binding_base = base(kept);
            text.binding_draft = Some((path.clone(), draft.clone()));
            value.text = text
        }
        Intent::BindingDiscard => {
            text.binding_draft = None;
            text.binding_base = None;
            text.binding_errors = text_pane::no_errors();
            value.text = text
        }
        Intent::GraphDraft(graph, draft) => {
            let old = slot_of(&text, graph);
            let slot = Slot {
                gtext: draft.clone(),
                gbase: base(old.as_ref().and_then(|s| s.gbase.clone())),
                gerrors: old.as_ref().map(|s| s.gerrors.clone()).unwrap_or_else(text_pane::no_errors),
            };
            value.text = put(text, graph, Some(slot))
        }
        Intent::GraphDiscard(graph) => value.text = put(text, graph, None),
        Intent::Live(live) => {
            text.live = Some(live.clone());
            value.text = text
        }
        Intent::GraphApply(graph, draft) => {
            let gbase = slot_of(&text, graph).and_then(|s| s.gbase);
            match checked(false, &|ws| of_graph(graph, ws), draft, gbase.as_ref(), &|d| graph_document(&*value, graph, d)) {
                Ok(doc) => {
                    install(value, doc, "Edit text", Merge::Step);
                    value.text = put(text, graph, None);
                }
                Err(found) => {
                    let slot = failed(&text, graph, draft, found);
                    value.text = put(text, graph, slot)
                }
            }
        }
        Intent::BindingApply(path, draft) => match checked_binding(path, draft, text.binding_base.as_ref(), &|d| binding_document(&*value, path, d)) {
            Ok(doc) => {
                install(value, doc, "Edit text", Merge::Step);
                value.text = text_pane::State { binding_draft: None, binding_base: None, binding_errors: text_pane::no_errors(), ..text };
            }
            Err(found) => {
                text.binding_draft = Some((path.clone(), draft.clone()));
                text.binding_base = base(text.binding_base.clone());
                text.binding_errors = Checked { at: draft.clone(), found };
                value.text = text
            }
        },
        // a dragged number: the text applies on every frame of the drag as one "Edit text" entry
        // (the gesture seals on release); the draft stays until the drag ends so the editor keeps
        // the text it is dragging in
        Intent::DocScrub(draft, done) => {
            let result = if finished(*done, draft, text.doc_base.as_ref(), text.draft.as_deref()) {
                Ok(None)
            } else {
                checked(true, &whole, draft, text.doc_base.as_ref(), &|d| scrub(&|d2| document_edit::text_edit(None, Some(&value.factories), &value.doc, d2), d)).map(Some)
            };
            match result {
                Ok(doc) => {
                    if let Some(doc) = doc {
                        install(value, doc, "Edit text", scrub_merge());
                    }
                    let now = value.doc.workspace.ws.clone();
                    value.text = text_pane::State {
                        draft: if *done { None } else { Some(draft.clone()) },
                        doc_base: if *done { None } else { Some(now) },
                        doc_errors: text_pane::no_errors(),
                        scrubbing: scrubbed(*done),
                        cache: if *done { None } else { text.cache.clone() },
                        ..text
                    };
                }
                Err(found) => {
                    text.draft = Some(draft.clone());
                    text.doc_base = base(text.doc_base.clone());
                    text.doc_errors = Checked { at: draft.clone(), found };
                    text.scrubbing = false;
                    value.text = text
                }
            }
        }
        Intent::GraphScrub(graph, draft, done) => {
            let gbase = slot_of(&text, graph).and_then(|s| s.gbase);
            let current = slot_of(&text, graph).map(|s| s.gtext);
            let result = if finished(*done, draft, gbase.as_ref(), current.as_deref()) {
                Ok(None)
            } else {
                checked(false, &|ws| of_graph(graph, ws), draft, gbase.as_ref(), &|d| scrub(&|d2| graph_document(&*value, graph, d2), d)).map(Some)
            };
            match result {
                Ok(doc) => {
                    if let Some(doc) = doc {
                        install(value, doc, "Edit text", scrub_merge());
                    }
                    let kept = if *done {
                        None
                    } else {
                        Some(Slot { gtext: draft.clone(), gbase: Some(value.doc.workspace.ws.clone()), gerrors: text_pane::no_errors() })
                    };
                    let mut next = put(text.clone(), graph, kept);
                    next.scrubbing = scrubbed(*done);
                    next.cache = if *done { None } else { text.cache.clone() };
                    value.text = next;
                }
                Err(found) => {
                    let slot = failed(&text, graph, draft, found);
                    let mut next = put(text, graph, slot);
                    next.scrubbing = false;
                    value.text = next
                }
            }
        }
        Intent::BindingScrub(path, draft, done) => {
            let current = match &text.binding_draft {
                Some((p, d)) if p == path => Some(d.clone()),
                _ => None,
            };
            let result = if finished(*done, draft, text.binding_base.as_ref(), current.as_deref()) {
                Ok(None)
            } else {
                checked_binding(path, draft, text.binding_base.as_ref(), &|d| scrub(&|d2| binding_document(&*value, path, d2), d)).map(Some)
            };
            match result {
                Ok(doc) => {
                    if let Some(doc) = doc {
                        install(value, doc, "Edit text", scrub_merge());
                    }
                    let now = value.doc.workspace.ws.clone();
                    value.text = text_pane::State {
                        binding_draft: if *done { None } else { Some((path.clone(), draft.clone())) },
                        binding_base: if *done { None } else { Some(now) },
                        binding_errors: text_pane::no_errors(),
                        scrubbing: scrubbed(*done),
                        cache: if *done { None } else { text.cache.clone() },
                        ..text
                    };
                }
                Err(found) => {
                    text.binding_draft = Some((path.clone(), draft.clone()));
                    text.binding_base = base(text.binding_base.clone());
                    text.binding_errors = Checked { at: draft.clone(), found };
                    text.scrubbing = false;
                    value.text = text
                }
            }
        }
        Intent::LiteralScrub { .. } => {},
    }
}

pub fn apply_text(value: &mut EditorModel, intents: &[Intent]) {
    for intent in intents {
        one_intent(value, intent);
    }
}

/// The intents of a text pane that is not the one in use fold into its own state.
pub fn apply_text_at(value: &mut EditorModel, key: &flow_check::workspace::Path, intents: &[Intent]) {
    if intents.is_empty() {
        return;
    }
    let code = local_of(&value.locals, key).code;
    let mut mine = std::mem::replace(&mut value.text, code);
    apply_text(value, intents);
    if intents.iter().any(|i| matches!(i, Intent::SelectBinding(_) | Intent::JumpDefinition(_))) {
        mine.target = None;
    }
    let code = std::mem::replace(&mut value.text, mine);
    value.locals = put_local(key, &|l| local { code: code.clone(), ..l }, &value.locals);
}

/// The unapplied drafts of a text pane, as the applies its Check & apply would make.
pub fn draft_applies(except: Option<&str>, text: &text_pane::State) -> Vec<Intent> {
    let mut out = Vec::new();
    if let Some(d) = &text.draft {
        if Some(d.as_str()) != except {
            out.push(Intent::DocApply(d.clone()));
        }
    }
    out.extend(text.graphs.iter().map(|(g, s)| Intent::GraphApply(g.clone(), s.gtext.clone())));
    if let Some((p, d)) = &text.binding_draft {
        out.push(Intent::BindingApply(p.clone(), d.clone()));
    }
    out
}

/// Some text pane holds text that is not in the document (`except`: the text of a refused reload,
/// which is the file's and not the user's).
pub fn has_draft(except: Option<&str>, value: &EditorModel) -> bool {
    !draft_applies(except, &value.text).is_empty() || value.locals.iter().any(|(_, l)| !draft_applies(except, &l.code).is_empty())
}

/// What Command-S writes: the workspace text and, without a settings graph, the settings.
pub fn same_text(a: &Document, b: &document::Text) -> bool {
    same_workspace(&a.workspace.ws, &b.text_ws) && settings::same_values(&a.settings, &b.text_settings)
}

/// Work a reload of the source file would lose: edits since the file's text, or a draft.
pub fn unsaved(except: Option<&str>, value: &EditorModel) -> bool {
    !same_text(&value.doc, &value.filed) || has_draft(except, value)
}

/// The document is the file's text now (saved over it).
pub fn filed(mut value: EditorModel) -> EditorModel {
    value.filed = document::text(&value.doc);
    value
}

/// Command-S with a draft: every draft is applied first (each one its "Edit text" entry); a draft
/// that does not check stays with its errors, and `Err` says the save must wait.
pub fn apply_drafts(except: Option<&str>, value: &mut EditorModel) -> bool {
    let applies = draft_applies(except, &value.text);
    apply_text(value, &applies);
    let locals = value.locals.clone();
    for (key, l) in locals {
        apply_text_at(value, &key, &draft_applies(except, &l.code));
    }
    !has_draft(except, value)
}
