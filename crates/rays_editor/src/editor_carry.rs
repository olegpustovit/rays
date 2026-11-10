//! The carry's turn of a frame: the key route, the preview, the put and the cancel.

use std::borrow::Cow;
use std::sync::Arc;

use editor_core::router;
use editor_document::document::{self, Home, T as Document};
use flow_check::workspace as w;
use input::{Event, Key};
use pxui_graph::scope_state as scope;
use rays::frame;

use crate::carry::{self as carry_mod, Place};
use crate::cook;
use crate::editor_commands::Action;
use crate::editor_inspect::kind;
use crate::editor_model::*;
use crate::editor_panes::{follow_target, go, graph_name, graph_of_object};
use crate::editor_shell::unrestore;
use crate::editor_text::install;
use crate::editor_history::Merge;
use crate::selection;

// ---- carry ----

pub const CARRY_LETTERS: &str = "asdfghjkl";
/// seconds the pointer rests on a node before the carry enters it
pub const CARRY_REST: f64 = 0.6;

/// While a payload is carried the document only navigates: the keys that walk, follow, go back and
/// jump work; every edit waits for the put.
pub fn carry_allowed(a: &Action) -> bool {
    match a {
        Action::Enter | Action::Up | Action::Back(_) | Action::Jump | Action::GuideToggle | Action::GuideKeys | Action::FrameTile | Action::PickUp => true,
        Action::ScopeCommand(scope::Command::Walk(_) | scope::Command::FrameAll | scope::Command::FrameSelection) => true,
        _ => false,
    }
}

pub fn carry_place(p: &CarryPreview) -> &Place {
    match p {
        CarryPreview::Showing { place, .. } | CarryPreview::HeldBack { place, .. } | CarryPreview::Refused { place, .. } => place,
    }
}

/// the document the panels read: the scratch copy while a target is hot and its picture is on
/// screen, else the one that was there
pub fn carry_shown(c: &Carry) -> Arc<Document> {
    match &c.preview {
        Some(CarryPreview::Showing { doc, .. }) => doc.clone(),
        Some(CarryPreview::HeldBack { .. } | CarryPreview::Refused { .. }) | None => c.original.clone(),
    }
}

/// The put at `place`, run on the original: the checker's answer is the preview, the refusal's
/// reason and the write.  A put that takes longer than the budget (or onto a cook that does) is
/// said and not shown.
pub fn carry_try(value: &EditorModel, c: &Carry, place: &Place) -> CarryPreview {
    let Some(catalog) = flow_catalog(value) else {
        return CarryPreview::Refused { place: place.clone(), reason: "The catalog is unavailable".to_string() };
    };
    let start = (value.clock)();
    let result = carry_mod::put(Some(&value.factories), &catalog, &c.original, &c.payload, place);
    let spent = (value.clock)() - start;
    match result {
        Err(reason) => CarryPreview::Refused { place: place.clone(), reason },
        Ok((doc, what)) => {
            let doc = Arc::new(doc);
            match value.cook.seconds {
                _ if spent >= value.carry_budget => {
                    CarryPreview::HeldBack { place: place.clone(), doc, what, reason: format!("applying takes {} ms", (spent * 1000.0) as i64) }
                }
                Some(cook_seconds) if cook_seconds >= value.carry_budget => {
                    CarryPreview::HeldBack { place: place.clone(), doc, what, reason: format!("a cook takes {} ms", (cook_seconds * 1000.0) as i64) }
                }
                _ => CarryPreview::Showing { place: place.clone(), doc, what },
            }
        }
    }
}

/// The key route's letters for the graph the pane shows now (they follow navigation).
pub fn carry_targets<'a>(value: &ModelView<'a>, c: Carry) -> Carry {
    let graph = graph_name(value);
    if let Some((for_graph, _)) = &c.targets {
        if *for_graph == graph {
            return c;
        }
    }
    let found = match flow_catalog(value) {
        None => Vec::new(),
        Some(catalog) => carry_mod::targets(Some(&value.factories), &catalog, None, &c.original, &c.payload, graph.as_deref()),
    };
    let letters: Vec<char> = CARRY_LETTERS.chars().collect();
    let targets = found.into_iter().enumerate().map(|(i, (place, label))| (letters[i].to_string(), place, label)).collect();
    Carry { targets: Some((graph, targets)), ..c }
}

fn referenced_payload(ws: &w::Workspace, name: &str) -> Option<Result<carry_mod::Payload, String>> {
    let ctx = ws.graphs.iter().find(|g| g.name == name).map(|g| g.context)?;
    let payload = |kind: &str| Ok(carry_mod::Payload { kind: kind.to_string(), value: format!("(ref {name})") });
    if ctx == flow_syntax::context::material() {
        Some(payload("material"))
    } else if ctx == flow_syntax::context::sop() {
        Some(payload("sop"))
    } else {
        None
    }
}

/// What `y` picks up: the open material or SOP graph; else the graph of the selected geometry
/// object; else the selected camera object (by its binding name); else the graph the pane's
/// selected node references; else the open scene graph (a viewport shows it).
pub fn carry_payload<'a>(value: &ModelView<'a>) -> Result<carry_mod::Payload, String> {
    let ws = &value.doc.workspace.ws.checked;
    let referenced = |name: &str| referenced_payload(ws, name);
    if let Some(result) = graph_name(value).and_then(|name| referenced(&name)) {
        return result;
    }
    let object_graph = selection::selected(pane_selection(value)).and_then(|id| if kind(value, id).as_deref() == Some("geometry") { graph_of_object(value, id) } else { None });
    if let Some(result) = object_graph.and_then(|g| referenced(&g)) {
        return result;
    }
    // a camera object: the one selected in the list, else the scene node selected in the pane
    let camera_named = |id: i64| -> Option<carry_mod::Payload> {
        if kind(value, id).as_deref() != Some("camera") {
            return None;
        }
        match value.doc.homes.objects.iter().find(|(i, _)| *i == id) {
            Some((_, Home::BoundAt(path))) if !path.is_empty() => Some(carry_mod::Payload { kind: "camera".to_string(), value: path[path.len() - 1].to_string() }),
            _ => None,
        }
    };
    let camera = match selection::selected(pane_selection(value)).and_then(camera_named) {
        Some(found) => Some(found),
        None => {
            let selected = scope::selected(pane_view(value));
            match selected.as_slice() {
                [path] => value.doc.homes.objects.iter().find_map(|(id, home)| if *home == Home::BoundAt(path.clone()) { Some(*id) } else { None }).and_then(camera_named),
                _ => None,
            }
        }
    };
    if let Some(payload) = camera {
        return Ok(payload);
    }
    if let Some(result) = follow_target(value, None).and_then(|t| referenced(&t)) {
        return result;
    }
    let scene = graph_name(value).and_then(|name| {
        let ctx = ws.graphs.iter().find(|g| g.name == name).map(|g| g.context);
        if ctx == Some(flow_syntax::context::scene()) { Some(carry_mod::Payload { kind: "scene".to_string(), value: format!("(ref {name})") }) } else { None }
    });
    scene.ok_or_else(|| "Nothing to pick up \u{b7} open a material, SOP or scene graph, or select a geometry or camera object".to_string())
}

pub fn carry_cancel(value: &mut EditorModel, c: &Carry, notice: (NoticeKind, String)) {
    value.ui.borrow_mut().cancel_carry();
    let doc = c.original.clone();
    let level = document::resolve_level(&doc, pane_level(&(&*value).into()));
    if !Arc::ptr_eq(&doc, &value.doc) {
        cook::set_volatile(&value.cook, flow_sop::lower::is_volatile(&doc.workspace.lowered));
    }
    value.carry = None;
    value.workspace = unrestore(&value.doc, &doc, value.workspace.clone());
    value.doc = doc;
    value.notice = Some(notice);
    pane_mut(value).at = level;
}

pub fn carry_commit(value: &mut EditorModel, c: &Carry, doc: &Arc<Document>, what: &str) {
    value.ui.borrow_mut().cancel_carry();
    value.carry = None;
    install(value, (**doc).clone(), "Put", Merge::Step);
    value.notice = Some((NoticeKind::Info, format!("Put {} \u{b7} wrote {what} \u{b7} one undo entry", c.payload.value)));
}

fn first_char(letter: &str) -> char {
    letter.chars().next().unwrap_or(' ')
}

/// The carry's turn of a frame, before the panes build: the keys of the route and the ends of the
/// gesture, then the preview (the scratch document the panels read), or the put or the restore.
/// Returns the frame without the keys the carry took, and whether the document shown changed.
pub fn carry_step<'a>(value: &mut EditorModel, text_focus: bool, frame_: &'a frame::Frame) -> (Cow<'a, frame::Frame>, bool) {
    let c = match &value.carry {
        None => {
            let carrying = value.ui.borrow().carrying();
            return match carrying {
                Some(payload) => {
                    value.carry = Some(Carry {
                        payload,
                        via: Via::Pointer,
                        original: crate::editor_history::present_arc(&value.history),
                        settled: value.cook.pieces.clone(),
                        report: None,
                        targets: None,
                        chosen: None,
                        anchor: None,
                        preview: None,
                        resting: None,
                        hint: None,
                    });
                    (Cow::Borrowed(frame_), false)
                }
                None => (Cow::Borrowed(frame_), false),
            };
        }
        Some(c) => c.clone(),
    };
    let pressed: Vec<Key> = frame_.input.events.iter().filter_map(|e| if let Event::KeyPressed(k) = e { Some(k.clone()) } else { None }).collect();
    let idle = value.leader == router::State::Idle;
    let escape = idle && pressed.contains(&Key::Escape);
    let lost = frame_.input.events.iter().any(|e| matches!(e, Event::FocusLost));
    let typing = !text_focus && idle && value.prompt.is_none() && value.menu.is_none();
    let c = if c.via == Via::Keys { carry_targets(&(&*value).into(), c) } else { c };
    let letters: Vec<(String, Place, String)> = match &c.targets {
        Some((_, targets)) if c.via == Via::Keys && typing => targets.clone(),
        _ => Vec::new(),
    };
    let mut letter: Option<(String, Place, String)> = None;
    for key in &pressed {
        if let Key::KeyChar(ch) = key {
            if let Some(hit) = letters.iter().find(|(l, _, _)| *l == ch.to_string()) {
                letter = Some(hit.clone());
            }
        }
    }
    let enter = c.via == Via::Keys && typing && pressed.contains(&Key::Enter);
    let consumed = |key: &Key| -> bool { (idle && *key == Key::Escape) || (c.via == Via::Keys && typing && (*key == Key::Enter || letters.iter().any(|(l, _, _)| Key::KeyChar(first_char(l)) == *key))) };
    let taken_event = |event: &Event| matches!(event, Event::KeyPressed(key) | Event::KeyReleased(key) if consumed(key));
    let mut taken = Cow::Borrowed(frame_);
    if frame_.input.events.iter().any(taken_event) || frame_.input.keys.iter().any(consumed) {
        let taken = taken.to_mut();
        taken.input.events.retain(|event| !taken_event(event));
        taken.input.keys.retain(|key| !consumed(key));
    }
    let mut c = c;
    if let Some((_, place, _)) = &letter {
        c.chosen = Some(place.clone());
        c.anchor = Some(frame_.input.pointer);
        c.hint = None;
    }
    let dropped: Option<Place> = match &c.report {
        Some(r) if r.dropped => Some(r.over.clone()),
        _ => None,
    };
    let hovering: Option<Place> = match &c.report {
        Some(r) if !r.dropped => Some(r.over.clone()),
        _ => None,
    };
    let pointer_wins = match c.anchor {
        Some(at) => frame_.input.pointer != at,
        None => true,
    };
    let desired: Option<Place> = match (&dropped, &hovering) {
        (Some(_), _) => dropped.clone(),
        (None, Some(_)) if pointer_wins => hovering.clone(),
        _ => c.chosen.clone(),
    };
    let still_carrying = value.ui.borrow().carrying().is_some();
    if escape || lost || (!still_carrying && dropped.is_none()) {
        let message = if lost { "Carry dropped \u{b7} the window lost focus \u{b7} nothing written" } else { "Carry dropped \u{b7} nothing written" };
        carry_cancel(value, &c, (NoticeKind::Info, message.to_string()));
        return (taken, true);
    }
    let preview = if desired.as_ref() == c.preview.as_ref().map(carry_place) { c.preview.clone() } else { desired.as_ref().map(|d| carry_try(&*value, &c, d)) };
    c.preview = preview.clone();
    if dropped.is_some() || enter {
        match preview {
            Some(CarryPreview::Showing { doc, what, .. } | CarryPreview::HeldBack { doc, what, .. }) => carry_commit(value, &c, &doc, &what),
            Some(CarryPreview::Refused { reason, .. }) => carry_cancel(value, &c, (NoticeKind::Refusal, format!("Refused \u{b7} {reason} \u{b7} nothing written"))),
            None if dropped.is_some() => carry_cancel(value, &c, (NoticeKind::Info, "Carry dropped \u{b7} nothing written".to_string())),
            None => {
                value.carry = Some(Carry { hint: Some("Pick a target letter first, or Esc".to_string()), ..c });
                return (taken, false);
            }
        }
        return (taken, true);
    }
    // the pointer resting on a node enters what it references, after a moment
    let resting: Option<(Place, f64)> = match &hovering {
        Some(place @ Place::Node(_)) => match &c.resting {
            Some((q, since)) if q == place => Some((q.clone(), *since)),
            _ => Some((place.clone(), frame_.input.t)),
        },
        _ => None,
    };
    let entered: Option<String> = match &resting {
        Some((Place::Node(path), since)) if frame_.input.t - *since >= CARRY_REST => {
            // what the original document references, not the preview
            let original = ModelView { doc: &c.original, ..ModelView::from(&*value) };
            follow_target(&original, Some(path.as_slice()))
        }
        _ => None,
    };
    c.resting = if entered.is_some() { None } else { resting };
    value.carry = Some(c.clone());
    if let Some(graph) = entered {
        go(value, &graph);
    }
    let shown = carry_shown(&c);
    if Arc::ptr_eq(&shown, &value.doc) {
        return (taken, false);
    }
    cook::set_volatile(&value.cook, flow_sop::lower::is_volatile(&shown.workspace.lowered));
    let level = document::resolve_level(&shown, pane_level(&(&*value).into()));
    value.lisp_vocab = vocabulary_for(value, &shown);
    value.workspace = unrestore(&value.doc, &shown, value.workspace.clone());
    value.doc = shown;
    pane_mut(value).at = level;
    (taken, true)
}
