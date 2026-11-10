//! The status strip: the line it says, the open dialog's name for a crash report, the probe
//! caption. #![allow(non_camel_case_types)]
use flow_syntax::symbol::Symbol;
use editor_core::guide_context;
use editor_core::keymap;
use editor_core::panels::Panel;
use editor_core::router;
use flow_graph::probe;
use flow_graph::projection::{self as fproj, ZoneKind};
use flow_syntax::phase_timer;
use pxui::ui::Ui;
use pxui_graph::scope_state as scope;
use pxui_shell::status_bar::{self, Opts, State};
use rays::frame;
use sop::async_cook::Status;

use crate::editor_commands;
use crate::editor_inspect::{inspector_paths, kind, scene};
use crate::editor_model::*;
use crate::editor_panes::{graph_name, stored_projection};
use crate::editor_shell::geometry;
use crate::text_pane;
use crate::timeline;

/// the open dialog, for the crash report
pub fn prompt_name(value: &EditorModel) -> &'static str {
    match &value.prompt {
        None => "-",
        Some(prompt::Keys) => "keys",
        Some(prompt::Saving(_)) => "save preset",
        Some(prompt::Palette(_)) => "commands",
        Some(prompt::Jumping(_)) => "jump",
        Some(prompt::Symbols(_)) => "go to symbol",
        Some(prompt::Referencing(..)) => "references",
        Some(prompt::Going(_)) => "go to line",
        Some(prompt::Picking { .. }) => "panel kind",
        Some(prompt::Splitting { .. }) => "split",
        Some(prompt::Browsing { .. }) => "presets",
        Some(prompt::Making_macro { .. }) => "make macro",
    }
}

pub fn level_name<'a>(value: &ModelView<'a>) -> String {
    level_name_at(value, pane_level(value))
}

pub fn level_name_at(value: &EditorModel, level: editor_document::document::Level) -> String {
    match level {
        editor_document::document::Level::Scene => "scene".to_string(),
        editor_document::document::Level::Inside(id) => {
            format!("scene \u{203a} {}", editor_document::kind_graph::find(&scene(value), id).map(|n| n.label).unwrap_or_else(|| "?".to_string()))
        }
    }
}

/// The strip while a payload is carried: what it is, then (a target hot) what putting it writes in
/// the words of the text, or why the place refuses it.
pub fn carry_line(c: &carry) -> String {
    let held = &c.payload.value;
    let put = match c.via {
        Via::Keys => "Enter",
        Via::Pointer => "release",
    };
    match &c.preview {
        Some(carry_preview::Showing { what, .. }) => format!("Preview \u{b7} {put} writes {what} \u{b7} Esc drops"),
        Some(carry_preview::Held_back { what, reason, .. }) => format!("Would write {what} \u{b7} no preview, {reason} \u{b7} {put} writes it"),
        Some(carry_preview::Refused { reason, .. }) => format!("Refused \u{b7} {reason}"),
        None => {
            let lead = match &c.hint {
                Some(hint) => format!("{hint} \u{b7} "),
                None => String::new(),
            };
            let rest = match (c.via, &c.targets) {
                (Via::Pointer, _) => format!("Carrying {held} \u{b7} put it on a place \u{b7} Esc drops"),
                (Via::Keys, Some((_, targets))) if !targets.is_empty() => {
                    let list: Vec<String> = targets.iter().map(|(letter, _, label)| format!("{letter} {label}")).collect();
                    format!("Carrying {held} \u{b7} put on {} \u{b7} Enter writes \u{b7} Esc drops", list.join(" \u{b7} "))
                }
                (Via::Keys, _) => format!("Carrying {held} \u{b7} nothing here takes it \u{b7} u, i or / j go elsewhere \u{b7} Esc drops"),
            };
            lead + &rest
        }
    }
}

pub fn status_text<'a>(value: &ModelView<'a>, brief: bool) -> String {
    if let Some(c) = &value.carry {
        return carry_line(c);
    }
    let playing = timeline::mode(&value.timeline) == timeline::Mode::Playing;
    let idle = || -> String {
        let text_error = if stored_projection(value) == Text_view { text_pane::first_error(&value.text) } else { None };
        if let Some(d) = text_error {
            return format!("Text rejected: {}", rays_diagnostic::to_string(&d));
        }
        match (&value.edit_error, &value.cook.error, value.cook.seconds) {
            (Some(error), _, _) => format!("Graph edit rejected: {error}"),
            (None, Some(error), _) => format!("Cook rejected: {error}"),
            (None, None, _) if value.notice.is_some() => value.notice.as_ref().map(|(_, s)| s.clone()).unwrap_or_default(),
            (None, None, Some(seconds)) => match flow_sop::lower::status(&value.doc.workspace.lowered, seconds) {
                Some(text) => text,
                None => format!("checked \u{b7} frame cook {} ms", cfmt::f(seconds * 1000.0, 0)),
            },
            (None, None, None) => "Waiting for first cook".to_string(),
        }
    };
    let cook = match crate::cook::status(&value.cook) {
        // playing and a newer frame is already waiting: frames are skipped
        Status::Cooking { seconds, queued: true, .. } if playing => format!("frame cook {} ms \u{b7} skipping frames", (seconds * 1000.0) as i64),
        // a frame-driven graph recooks every frame, so one is nearly always in flight: say what
        // the last one took, not how long the current one has run
        Status::Cooking { .. } if playing && flow_sop::lower::counts(&value.doc.workspace.lowered).0 > 0 && value.cook.seconds.is_some() => idle(),
        Status::Cooking { seconds, queued, .. } => format!("Cooking\u{2026} {}s{}", cfmt::f(seconds, 1), if queued { " \u{b7} latest queued" } else { "" }),
        Status::Idle => idle(),
    };
    let cook = match flow_ir::fallback::summary() {
        Some(off) => format!("{cook} \u{b7} {off}"),
        None => cook,
    };
    let cook = match mem::status_line() {
        Some(line) => format!("{cook} \u{b7} {line}"),
        None => cook,
    };
    let phases = phase_timer::summary(&value.edit_phases);
    let cook = if phases.is_empty() { cook } else { format!("{cook} \u{b7} {phases}") };
    // What the open level's keys do, so the World and the menu are findable.
    let hint = match pane_level(value) {
        editor_document::document::Level::Scene => "i/double-click enter \u{b7} / a add \u{b7} / e World".to_string(),
        editor_document::document::Level::Inside(id) if kind(value, id).as_deref() == Some("world") => {
            "u up \u{b7} drag map: move layer/sun \u{b7} t dome/light \u{b7} n reseed \u{b7} d day cycle \u{b7} [ ] time \u{b7} / m 3D/map".to_string()
        }
        editor_document::document::Level::Inside(_) => "u up \u{b7} / a add \u{b7} / l panel kind".to_string(),
    };
    let lead = if value.workspace.restored { "Default layout \u{b7} / z returns to the editor graph \u{b7} " } else { "" };
    let tail = if brief { String::new() } else { format!(" \u{b7} {} \u{b7} {hint}", level_name(value)) };
    format!("{lead}{cook}{tail}")
}

/// "ring · iteration 1 of 12": which iteration the viewport's highlight and the inspector show for
/// the node selected in the graph pane (the innermost loop around it, or itself when it is one).
pub fn probe_caption<'a>(value: &ModelView<'a>) -> Option<String> {
    let laid = pane_laid(value)?;
    let selected = scope::selected(pane_view(value));
    if selected.len() != 1 {
        return None;
    }
    let path = &selected[0];
    let records = laid.records.as_ref()?;
    if graph_name(value).as_ref() != Some(&laid.graph) {
        return None;
    }
    let probe_of = |p: &[Symbol]| value.probes.get(p).copied().unwrap_or(0);
    let zones: Vec<fproj::Node> = fproj::zones(&laid.scope).into_iter().filter(|n| n.zone.as_ref().is_some_and(|z| z.kind != ZoneKind::Let)).collect();
    let own = zones.iter().find(|n| n.path == *path);
    let zone = match own {
        Some(n) => Some(n.path.clone()),
        None => match probe::chains(&laid.scope).get(path) {
            Some(chain) if !chain.is_empty() => chain.last().cloned(),
            _ => None,
        },
    }?;
    let counts = records.borrow_mut().counts(&laid.scope, &probe_of);
    let count = counts.iter().find(|(z, _)| *z == zone).map(|(_, c)| *c)?;
    Some(format!("{} \u{b7} iteration {} of {}", zone.last().map_or("", |name| name.as_str()), probe_of(&zone) + 1, count))
}

/// The status strip under the view: kit text on the ground, under a hairline.
pub fn status_box<'a>(value: &ModelView<'a>, ui: &mut Ui, frame: &frame::Frame, render_status: Option<&str>, error_status: Option<&str>, context: guide_context::GuideContext) {
    let g = geometry(value, &value.workspace, frame);
    // the pane really in use: the focus when a leaf of its kind is open, else the first leaf
    let focus: Panel = {
        let open = g.leaves.iter().any(|l| editor_commands::scope(&l.panel) == editor_commands::scope(&value.focus));
        match (open, g.leaves.first()) {
            (false, Some(l)) => l.panel.clone(),
            _ => value.focus.clone(),
        }
    };
    let (x, y, width, height) = g.status_at;
    // what every strip says first: the workspace, a dot for its state, then the layout in use
    let file = value.file.clone();
    let state = if error_status.is_some() || value.edit_error.is_some() || value.cook.error.is_some() {
        State::Error
    } else {
        match crate::cook::status(&value.cook) {
            Status::Cooking { .. } => State::Busy,
            Status::Idle => State::Ok,
        }
    };
    let layout_text = match value.doc.shell.as_ref().and_then(|s| s.switch.as_ref()) {
        Some(sw) if sw.active < sw.layouts.len() as i64 => format!("layout {} \u{b7} {}", sw.active, editor_core::panels::summary(&sw.layouts[sw.active as usize])),
        _ => String::new(),
    };
    let line = match error_status {
        Some(error) => error.to_string(),
        None => {
            let base = status_text(value, true);
            match render_status {
                None => base,
                Some(status) => format!("{base} \u{b7} {status}"),
            }
        }
    };
    // the focused pane's kind and what is selected: in the graph pane its node, in a viewport the
    // object the brackets stand round
    let kind_text = match &focus {
        Panel::View(_) => "Viewport".to_string(),
        panel => editor_core::panels::name(panel).to_string(),
    };
    let selection = match (&focus, &value.selected_box) {
        (Panel::View(_), Some((_, _, name))) => Some(name.clone()),
        _ => {
            let paths = inspector_paths(scope::selected(pane_view(value)));
            match paths.len() {
                1 => paths[0].last().map(ToString::to_string),
                0 => None,
                n => Some(format!("{n} selected")),
            }
        }
    };
    // a panel being carried: its name in the accent, what the drag does, and how many windows float
    let moving = value.workspace.window_live.as_ref().and_then(|(path, _)| g.leaves.iter().find(|l| l.path == *path).map(|l| editor_core::panels::name(&l.panel).to_string()));
    let floating = g.leaves.iter().filter(|l| l.floating).count();
    let graphs = value.doc.workspace.ws.checked.graphs.len();
    // the end follows the sheet's context: floating windows give their count alone; a graph that is
    // the only docked pane (graph.html) gives its counts and zoom in place of layout and fps; any
    // other layout keeps the layout summary and the frame rate
    let docked = g.leaves.iter().filter(|l| !l.floating).count();
    let alone = floating == 0 && focus == Panel::Graph && docked == 1;
    let readout = if !alone {
        String::new()
    } else {
        let nodes = scope::stats(pane_view(value)).nodes;
        let selected = inspector_paths(scope::selected(pane_view(value))).len();
        format!("{nodes} node{}{}", if nodes == 1 { "" } else { "s" }, if selected == 0 { String::new() } else { format!(" \u{b7} {selected} selected") })
    };
    let notes: Vec<String> = if floating > 0 {
        vec![format!("{floating} floating")]
    } else if alone {
        vec![format!("zoom {}%", (100.0 * scope::zoom(pane_view(value))).round() as i64)]
    } else {
        match focus {
            Panel::Outline => vec![format!("{graphs} graph{}", if graphs == 1 { "" } else { "s" })],
            _ => Vec::new(),
        }
    };
    let (layout_text, status_fps) = if floating > 0 || alone { (String::new(), None) } else { (layout_text, value.status_fps) };
    // the keys each pane's sheet lists, as short labels; Lisp, Timeline and Inspector have none
    let jump = editor_commands::keymap().iter().find_map(|c| {
        if c.id == "scene.jump" {
            let platform = ui.platform();
            c.trigger.as_ref().map(|t| (keymap::label(platform, t), "jump".to_string()))
        } else {
            None
        }
    });
    let pair = |a: &str, b: &str| (a.to_string(), b.to_string());
    let extra: Vec<(String, String)> = match &focus {
        Panel::Outline => {
            let mut e = vec![pair("s", "filter"), pair("i", "enter")];
            e.extend(jump);
            e
        }
        Panel::Graph => vec![
            pair("Tab", "add after"),
            pair("o", "open"),
            pair("v", "view"),
            pair("b", "bypass"),
            pair("i", "enter"),
            pair("f", "frame"),
            pair("w", "hints"),
            pair("/", "leader"),
        ],
        Panel::View(_) => vec![pair("w", "move"), pair("e", "rotate"), pair("r", "scale"), pair("i", "enter object"), pair("\u{2325} drag", "orbit")],
        _ => Vec::new(),
    };
    let bounds = (x, y, width, height);
    let base = Opts { file, state, layout: layout_text, text: line, fps: status_fps, notes, readout, ..Opts::default() };
    if height <= 0 {
    } else if let router::State::Pending(p) = &value.leader {
        // an open leader: the pending prefix in the accent and what the strip waits for
        let prefix = if !p.is_empty() { format!("/ {p}") } else { "/".to_string() };
        status_bar::guide(ui, bounds, &Opts { leader: Some(prefix), ..base }, guide_context::GuideContext::Leader);
    } else if let Some(moving) = moving {
        status_bar::guide(
            ui,
            bounds,
            &Opts {
                accent: true,
                kind: Some(format!("moving {moving}")),
                extra: vec![pair("drag to an edge", "dock"), pair("/ o f", "float or dock"), pair("/ n", "new window")],
                ..base
            },
            guide_context::GuideContext::Canvas,
        );
    } else if state != State::Error && value.carry.is_none() && value.guide {
        status_bar::guide(ui, bounds, &Opts { extra, kind: Some(kind_text), selection, ..base }, context);
    } else {
        // a refusal has the strip to itself: the whole message, up to the layout and frame rate
        let (kind_opt, selection) = if state == State::Error { (None, None) } else { (Some(kind_text), selection) };
        status_bar::draw(ui, bounds, &Opts { kind: kind_opt, selection, ..base });
    }
}
