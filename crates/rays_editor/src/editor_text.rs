//! The text pane's applies: drafts, merges, scrubs and the save-time bookkeeping of unsaved work.
//! Port of lib/rays_editor/editor_text.ml.
//!
//! Optional arguments of the OCaml functions are trailing `Option`s.
#![allow(non_camel_case_types)]
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

type Applied = Result<t, Vec<Diagnostic>>;

/// Every editor path records the same immutable document. No-op/rejected edits leave both the
/// present and the merge state untouched.
pub fn commit(doc: &Arc<Document>, label: Option<&str>, merge: Option<Merge>, history: history::t) -> history::t {
    if Arc::ptr_eq(doc, &history::present_arc(&history)) {
        return history;
    }
    history::record(merge.unwrap_or(Merge::Step), label.unwrap_or("Edit"), doc.clone(), history)
}

/// The document as the history's present, keeping the present's identity when nothing changed.
pub fn adopt(history: &history::t, doc: Document) -> Arc<Document> {
    let present = history::present_arc(history);
    if same_document(&present, &doc) { present } else { Arc::new(doc) }
}

pub fn parameter_gesture(operation: &str, level: Level, id: i64, values: &[(String, Value)]) -> String {
    let mut names: Vec<&str> = values.iter().map(|(k, _)| k.as_str()).collect();
    names.sort();
    names.dedup();
    format!("{operation}/{}/{id}/{}", level_key(level), names.join("/"))
}

/// A workspace edit installed as the present: the source and its lowering are one history entry,
/// the open level is re-resolved and the cook told about the new volatile nodes.
pub fn install(value: &t, doc: Document, label: &str, merge: Merge) -> t {
    let doc = adopt(&value.history, doc);
    let level = document::resolve_level(&doc, pane_level(value));
    let mut next = value.clone();
    next.lisp_vocab = vocabulary_for(value, &doc);
    next.workspace = unrestore(&value.doc, &doc, value.workspace.clone());
    next.doc = doc.clone();
    cook::set_volatile(&value.cook, flow_sop::lower::is_volatile(&doc.workspace.lowered));
    next.history = commit(&doc, Some(label), Some(merge), value.history.clone());
    with_pane(next, &|pane| graph_pane { at: level, picked: if level == pane.at { pane.picked.clone() } else { crate::selection::empty() }, ..pane.clone() })
}

/// The text pane's applies: the whole workspace text, or one binding's expression; atomic, one
/// history entry "Edit text".
pub fn text_edit(value: &t, text: &str, imports: Option<&[(String, String)]>, label: Option<&str>, merge: Option<Merge>) -> Applied {
    let doc = document_edit::text_edit(imports, Some(&value.factories), &value.doc, text)?;
    Ok(install(value, doc, label.unwrap_or("Edit text"), merge.unwrap_or(Merge::Step)))
}

fn at_line_1(mut d: Diagnostic) -> Diagnostic {
    d.position = Some(Position { line: 1, col: 0 });
    d.span = None;
    d
}

/// Selection patches root bindings into one candidate graph, checked and installed once. Omitted
/// bindings stay and the shown result cannot change. Without a closure the tab shows the graph
/// form, replaced whole.
pub fn binding_edit(value: &t, path: &[String], text: &str, merge: Option<Merge>) -> Applied {
    let ws = &value.doc.workspace.ws;
    match text_pane::graph_op(&ws.source, &path[0], Some(path), text) {
        Err(d) => Err(vec![if d.position.is_none() { at_line_1(d) } else { d }]),
        Ok(op) => match document_edit::syntax_edit_result(Some(&value.factories), &value.doc, &op) {
            Ok(doc) => Ok(install(value, doc, "Edit text", merge.unwrap_or(Merge::Step))),
            Err(d) => Err(vec![at_line_1(d)]),
        },
    }
}

/// The Graph tab's apply: the draft must be the one graph (or function) form, which replaces the
/// graph's; atomic like every apply.
pub fn graph_edit(value: &t, name: &str, text: &str, merge: Option<Merge>) -> Applied {
    let ws = &value.doc.workspace.ws;
    match text_pane::graph_op(&ws.source, name, None, text) {
        Err(d) => Err(vec![if d.position.is_none() { Diagnostic { position: Some(Position { line: 1, col: 0 }), ..d } } else { d }]),
        Ok(op) => match document_edit::syntax_edit_result(Some(&value.factories), &value.doc, &op) {
            Ok(doc) => Ok(install(value, doc, "Edit text", merge.unwrap_or(Merge::Step))),
            Err(d) => Err(vec![at_line_1(d)]),
        },
    }
}

/// Fold the pane's intents: drafts live in `value.text` (view state); a refused apply keeps the
/// draft and its errors and changes nothing else.
pub fn scrub_merge() -> Merge {
    Merge::Gesture("text-scrub".to_string())
}

pub fn select_source(value: t, path: &[String]) -> t {
    let Some(graph) = path.first() else { return value };
    if graph.starts_with("macro:") {
        return value;
    }
    let mut value = value;
    let locals: Vec<(Vec<String>, local)> = value.locals.iter().map(|(key, l)| (key.clone(), local { code: text_pane::State { target: None, ..l.code.clone() }, ..l.clone() })).collect();
    let original = value.clone();
    value.text = text_pane::State { target: None, ..value.text.clone() };
    value.locals = locals;
    let mut next = sync_scope(go(value, graph));
    let found = pane_laid(&next).is_some_and(|k| k.graph == *graph && (fproj::find(&k.scope, path).is_some() || k.scope.inputs.iter().any(|input| input.path == path)));
    if found {
        if scope::selected(pane_view(&next)) == vec![path.to_vec()] {
            next
        } else {
            next.select_later = Vec::new();
            with_pane(next, &|pane| {
                let selected = scope::select(vec![path.to_vec()], pane.view.clone());
                graph_pane { view: scope_commands::run_command(selected, scope::Command::Frame_selection).0, ..pane.clone() }
            })
        }
    } else if path.len() == 1 {
        with_pane(next, &|pane| graph_pane { view: scope::select(Vec::new(), pane.view.clone()), ..pane.clone() })
    } else {
        original
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

fn one_intent(value: t, intent: &Intent) -> t {
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
    let with_text = |mut value: t, text: text_pane::State| -> t {
        value.text = text;
        value
    };
    let base = |old: Option<WorkspaceDoc>| -> Option<WorkspaceDoc> { Some(old.unwrap_or_else(|| workspace.clone())) };
    let slot_of = |text: &text_pane::State, graph: &str| -> Option<Slot> { text_pane::graph_slot(text, graph).cloned() };
    let failed = |text: &text_pane::State, graph: &str, draft: &str, errors: Vec<Diagnostic>| -> Option<Slot> {
        Some(Slot { gtext: draft.to_string(), gbase: base(slot_of(text, graph).and_then(|s| s.gbase)), gerrors: Checked { at: draft.to_string(), found: errors } })
    };
    // a draft begun on an older document: merged with what changed since on the syntax tree
    // (`Text_pane::merge3`); only a value both changed keeps the draft as a conflict
    let checked = |metadata: bool, shown: &dyn Fn(&WorkspaceDoc) -> Result<String, Diagnostic>, draft: &str, old: Option<&WorkspaceDoc>, apply: &dyn Fn(&str) -> Applied| -> Applied {
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
    let checked_binding = |merge: Option<Merge>, path: &[String], draft: &str, old: Option<&WorkspaceDoc>, apply: &dyn Fn(&str) -> Applied| -> Applied {
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
                            Some(merged) => graph_edit(&value, &name, &merged, merge),
                            None => Err(vec![conflict()]),
                        }
                    }
                    Ok(_) => apply(draft),
                }
            }
            _ => apply(draft),
        }
    };
    let scrub = |apply: &dyn Fn(&str) -> Applied, draft: &str| -> Applied {
        match &literal {
            Some((source, op, position)) if **source == workspace.source => document_edit::syntax_edit_result(Some(&value.factories), &value.doc, op)
                .map(|doc| install(&value, doc, "Edit text", scrub_merge()))
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
            let mut next = value;
            let message = rays_diagnostic::to_string(error);
            next.edit_error = Some(message.clone());
            next.notice = Some((notice_kind::Refusal, message));
            next
        }
        Intent::Tab(tab) => {
            text.tab = *tab;
            with_text(value, text)
        }
        Intent::Menu(menu) => {
            text.menu = *menu;
            with_text(value, text)
        }
        Intent::ToggleWrap => {
            text.wrap = !text.wrap;
            with_text(value, text)
        }
        Intent::ToggleParinfer => {
            text.parinfer = !text.parinfer;
            with_text(value, text)
        }
        Intent::Picker(picker) => {
            text.picker = *picker;
            with_text(value, text)
        }
        Intent::OpenGraph(graph) => go(value, graph),
        Intent::SelectBinding(path) => {
            let mut v = value;
            v.text = text_pane::State { target: None, ..text };
            select_source(v, path)
        }
        Intent::JumpDefinition(path) => {
            let next = select_source(value, path);
            let mut next = next;
            next.text = text_pane::State { target: Some(path.clone()), nonce: text.nonce + 1, ..text };
            next
        }
        // opens the references prompt, see `Editor_reduce`
        Intent::ReferencesOf(_) => value,
        // read by the carry's report, not an edit
        Intent::CarryOver(..) => value,
        Intent::FindSet(find, reveal) => {
            text.find = find.clone();
            text.nonce += if *reveal { 1 } else { 0 };
            with_text(value, text)
        }
        Intent::RequestSet(request) => {
            text.request = request.clone();
            with_text(value, text)
        }
        Intent::Resolved => {
            text.pending = Vec::new();
            with_text(value, text)
        }
        Intent::DocDraft(draft) => {
            text.draft = Some(draft.clone());
            text.doc_base = base(text.doc_base.clone());
            text.scrubbing = false;
            with_text(value, text)
        }
        Intent::DocDiscard => {
            text.draft = None;
            text.doc_base = None;
            text.doc_errors = text_pane::no_errors();
            with_text(value, text)
        }
        Intent::DocApply(draft) => match checked(true, &whole, draft, text.doc_base.as_ref(), &|d| text_edit(&value, d, None, None, None)) {
            Ok(mut v) => {
                v.text = text_pane::State { draft: None, doc_base: None, doc_errors: text_pane::no_errors(), ..text };
                v
            }
            Err(found) => {
                text.draft = Some(draft.clone());
                text.doc_base = base(text.doc_base.clone());
                text.doc_errors = Checked { at: draft.clone(), found };
                with_text(value, text)
            }
        },
        Intent::BindingDraft(path, draft) => {
            let kept = match &text.binding_draft {
                Some((old, _)) if old == path => text.binding_base.clone(),
                _ => None,
            };
            text.binding_base = base(kept);
            text.binding_draft = Some((path.clone(), draft.clone()));
            with_text(value, text)
        }
        Intent::BindingDiscard => {
            text.binding_draft = None;
            text.binding_base = None;
            text.binding_errors = text_pane::no_errors();
            with_text(value, text)
        }
        Intent::GraphDraft(graph, draft) => {
            let old = slot_of(&text, graph);
            let slot = Slot {
                gtext: draft.clone(),
                gbase: base(old.as_ref().and_then(|s| s.gbase.clone())),
                gerrors: old.as_ref().map(|s| s.gerrors.clone()).unwrap_or_else(text_pane::no_errors),
            };
            with_text(value, put(text, graph, Some(slot)))
        }
        Intent::GraphDiscard(graph) => with_text(value, put(text, graph, None)),
        Intent::Live(live) => {
            text.live = Some(live.clone());
            with_text(value, text)
        }
        Intent::GraphApply(graph, draft) => {
            let gbase = slot_of(&text, graph).and_then(|s| s.gbase);
            match checked(false, &|ws| of_graph(graph, ws), draft, gbase.as_ref(), &|d| graph_edit(&value, graph, d, None)) {
                Ok(mut v) => {
                    v.text = put(text, graph, None);
                    v
                }
                Err(found) => {
                    let slot = failed(&text, graph, draft, found);
                    with_text(value, put(text, graph, slot))
                }
            }
        }
        Intent::BindingApply(path, draft) => match checked_binding(None, path, draft, text.binding_base.as_ref(), &|d| binding_edit(&value, path, d, None)) {
            Ok(mut v) => {
                v.text = text_pane::State { binding_draft: None, binding_base: None, binding_errors: text_pane::no_errors(), ..text };
                v
            }
            Err(found) => {
                text.binding_draft = Some((path.clone(), draft.clone()));
                text.binding_base = base(text.binding_base.clone());
                text.binding_errors = Checked { at: draft.clone(), found };
                with_text(value, text)
            }
        },
        // a dragged number: the text applies on every frame of the drag as one "Edit text" entry
        // (the gesture seals on release); the draft stays until the drag ends so the editor keeps
        // the text it is dragging in
        Intent::DocScrub(draft, done) => {
            let result = if finished(*done, draft, text.doc_base.as_ref(), text.draft.as_deref()) {
                Ok(value.clone())
            } else {
                checked(true, &whole, draft, text.doc_base.as_ref(), &|d| scrub(&|d2| text_edit(&value, d2, None, None, Some(scrub_merge())), d))
            };
            match result {
                Ok(mut v) => {
                    let now = v.doc.workspace.ws.clone();
                    v.text = text_pane::State {
                        draft: if *done { None } else { Some(draft.clone()) },
                        doc_base: if *done { None } else { Some(now) },
                        doc_errors: text_pane::no_errors(),
                        scrubbing: scrubbed(*done),
                        cache: if *done { None } else { text.cache.clone() },
                        ..text
                    };
                    v
                }
                Err(found) => {
                    text.draft = Some(draft.clone());
                    text.doc_base = base(text.doc_base.clone());
                    text.doc_errors = Checked { at: draft.clone(), found };
                    text.scrubbing = false;
                    with_text(value, text)
                }
            }
        }
        Intent::GraphScrub(graph, draft, done) => {
            let gbase = slot_of(&text, graph).and_then(|s| s.gbase);
            let current = slot_of(&text, graph).map(|s| s.gtext);
            let result = if finished(*done, draft, gbase.as_ref(), current.as_deref()) {
                Ok(value.clone())
            } else {
                checked(false, &|ws| of_graph(graph, ws), draft, gbase.as_ref(), &|d| scrub(&|d2| graph_edit(&value, graph, d2, Some(scrub_merge())), d))
            };
            match result {
                Ok(mut v) => {
                    let kept = if *done {
                        None
                    } else {
                        Some(Slot { gtext: draft.clone(), gbase: Some(v.doc.workspace.ws.clone()), gerrors: text_pane::no_errors() })
                    };
                    let mut next = put(text.clone(), graph, kept);
                    next.scrubbing = scrubbed(*done);
                    next.cache = if *done { None } else { text.cache.clone() };
                    v.text = next;
                    v
                }
                Err(found) => {
                    let slot = failed(&text, graph, draft, found);
                    let mut next = put(text, graph, slot);
                    next.scrubbing = false;
                    with_text(value, next)
                }
            }
        }
        Intent::BindingScrub(path, draft, done) => {
            let current = match &text.binding_draft {
                Some((p, d)) if p == path => Some(d.clone()),
                _ => None,
            };
            let result = if finished(*done, draft, text.binding_base.as_ref(), current.as_deref()) {
                Ok(value.clone())
            } else {
                checked_binding(Some(scrub_merge()), path, draft, text.binding_base.as_ref(), &|d| scrub(&|d2| binding_edit(&value, path, d2, Some(scrub_merge())), d))
            };
            match result {
                Ok(mut v) => {
                    let now = v.doc.workspace.ws.clone();
                    v.text = text_pane::State {
                        binding_draft: if *done { None } else { Some((path.clone(), draft.clone())) },
                        binding_base: if *done { None } else { Some(now) },
                        binding_errors: text_pane::no_errors(),
                        scrubbing: scrubbed(*done),
                        cache: if *done { None } else { text.cache.clone() },
                        ..text
                    };
                    v
                }
                Err(found) => {
                    text.binding_draft = Some((path.clone(), draft.clone()));
                    text.binding_base = base(text.binding_base.clone());
                    text.binding_errors = Checked { at: draft.clone(), found };
                    text.scrubbing = false;
                    with_text(value, text)
                }
            }
        }
        Intent::LiteralScrub { .. } => value,
    }
}

pub fn apply_text(value: t, intents: &[Intent]) -> t {
    intents.iter().fold(value, |value, intent| one_intent(value, intent))
}

/// The intents of a text pane that is not the one in use fold into its own state.
pub fn apply_text_at(mut value: t, key: &Vec<String>, intents: &[Intent]) -> t {
    if intents.is_empty() {
        return value;
    }
    let code = local_of(&value.locals, key).code;
    let mut mine = std::mem::replace(&mut value.text, code);
    let mut value = apply_text(value, intents);
    if intents.iter().any(|i| matches!(i, Intent::SelectBinding(_) | Intent::JumpDefinition(_))) {
        mine.target = None;
    }
    let code = std::mem::replace(&mut value.text, mine);
    value.locals = put_local(key, &|l| local { code: code.clone(), ..l }, &value.locals);
    value
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
pub fn has_draft(except: Option<&str>, value: &t) -> bool {
    !draft_applies(except, &value.text).is_empty() || value.locals.iter().any(|(_, l)| !draft_applies(except, &l.code).is_empty())
}

/// What Command-S writes: the workspace text and, without a settings graph, the settings.
pub fn same_text(a: &Document, b: &document::Text) -> bool {
    same_workspace(&a.workspace.ws, &b.text_ws) && settings::same_values(&a.settings, &b.text_settings)
}

/// Work a reload of the source file would lose: edits since the file's text, or a draft.
pub fn unsaved(except: Option<&str>, value: &t) -> bool {
    !same_text(&value.doc, &value.filed) || has_draft(except, value)
}

/// The document is the file's text now (saved over it).
pub fn filed(mut value: t) -> t {
    value.filed = document::text(&value.doc);
    value
}

/// Command-S with a draft: every draft is applied first (each one its "Edit text" entry); a draft
/// that does not check stays with its errors, and `Err` says the save must wait.
pub fn apply_drafts(except: Option<&str>, value: t) -> Result<t, t> {
    let applies = draft_applies(except, &value.text);
    let mut value = apply_text(value, &applies);
    let locals = value.locals.clone();
    for (key, l) in locals {
        value = apply_text_at(value, &key, &draft_applies(except, &l.code));
    }
    if has_draft(except, &value) { Err(value) } else { Ok(value) }
}
