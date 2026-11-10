//! One frame of the editor: keys, panes, prompts, then the reducer. Port of
//! lib/rays_editor/editor_frame.ml.
//!
//! Conventions: the model `ui` is behind a `RefCell`, so nothing called inside `Ui::frame` touches
//! `value.ui` (the platform is read before the frame); optional arguments (`?gpu ?pump ?image
//! ?host_events`) are the trailing parameters of `update`; callbacks that need the `ui` while the
//! chrome holds it (`groups`, `tools_from`) are answered from tables computed just before.
#![allow(non_camel_case_types)]
use flow_syntax::symbol::Symbol;
use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;

use editor_core::guide_context;
use editor_core::keymap::{self, Trigger};
use editor_core::panels::{self, Axis, Panel};
use editor_core::router;
use editor_document::document::{Home, Level, Origin};
use editor_document::kind_graph as kg;
use editor_document::layout_edit;
use editor_document::scene_sync;
use editor_document::workspace_doc;
use flow_graph::flow_edit::{self as fe, ArgKey, Op};
use flow_graph::projection as fproj;
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node as SNode, Syntax};
use input::{Event, Key, MouseButton};
use param::{Effects, FieldView, Value};
use pxui::ui::{BoxOpts, MenuOpts, MenuResult, Size, Ui, UiBox, CLICKABLE};
use pxui_graph::node_menu;
use pxui_graph::scope_state as scope;
use pxui_shell::chrome;
use pxui_shell::layout::{self, Leaf};
use pxui_shell::prompt as shell_prompt;
use pxui_shell::status_bar::{self, TipKind};
use pxui_shell::which_key;
use pxui_shell::tree;
use rays::frame;

use crate::carry as carry_mod;
use crate::editor_actions::{apply_action, routed};
use crate::editor_carry::{carry_allowed, carry_place, carry_step};
use crate::editor_commands::{self, action, action_of, Command};
use crate::editor_inspect::{displayed, kind, new_graph, objects as level_objects};
use crate::editor_list::{rows, space};
use crate::editor_menu::open_menu;
use crate::editor_model::*;
use crate::editor_panels::{copy_bindings, copy_workspace, defn_change, focused_leaf, graph_leaf, layout_actions, layout_intents, navigator_params, panel_title, paste_bindings};
use crate::editor_panes::*;
use crate::editor_reduce::{reduce, Frame_input};
use crate::editor_scope::sync_scope;
use crate::editor_setup::browse;
use crate::editor_shell::*;
use crate::editor_status::{level_name, probe_caption, status_box};
use crate::editor_text::has_draft;
use crate::editor_viewport_pane::draw_viewport_pane;
use crate::editor_add::scope_add;
use crate::editor_inspector_pane::inspect_panel;
use crate::navigator;
use crate::panel_map;
use crate::selection;
use crate::text_pane;
use crate::timeline;

type Path = flow_check::workspace::Path;
type Key_ = flow_check::workspace::Path;

/// a viewer or a canvas: the panels a sketch's picture is drawn in
pub fn is_picture(panel: &Panel) -> bool {
    matches!(panel, Panel::View(_) | Panel::Canvas(_))
}

/// The viewport's handles: given the selected node, the space and the pane's bounds, the edits
/// (field values), whether a handle holds the pointer and the object picked.
pub type ViewHandles = dyn Fn(&mut Ui, Option<&picked>, rays_math::mat4::T, layout::Bounds) -> (Vec<(String, Value)>, bool, Option<i64>);

/// UTC month, day, hour and minute of a Unix time.
// ponytail: UTC, not local time (std has no time zone), like `Preset.default_name`; add the offset
// when a caller needs it.
pub fn month_day_hour_minute(time: f64) -> (i64, i64, i64, i64) {
    let secs = time.floor() as i64;
    let days = secs.div_euclid(86400);
    let rem = secs.rem_euclid(86400);
    let z = days + 719468;
    let era = z.div_euclid(146097);
    let doe = z.rem_euclid(146097);
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = doy - (153 * mp + 2) / 5 + 1;
    let month = if mp < 10 { mp + 3 } else { mp - 9 };
    let _ = era;
    (month, day, rem / 3600, (rem % 3600) / 60)
}

type Prompted = (Option<prompt>, Option<prompt_intent>);

/// A search prompt over `all` rows of (name, detail, intent): fuzzy-filtered, a pick is its
/// intent, any other query change keeps the prompt open as `again query`.
#[allow(clippy::too_many_arguments)]
fn search_prompt(ui: &mut Ui, key: &str, title: &str, label: &str, query: &str, all: Vec<(String, String, prompt_intent)>, again: &dyn Fn(String) -> prompt) -> Prompted {
    let matches = |query: &str| -> Vec<(String, String, prompt_intent)> { all.iter().filter(|(name, _, _)| pxui::ui::fuzzy_match(query, name)).cloned().collect() };
    let rows = |query: &str| -> Vec<(String, String)> { matches(query).into_iter().map(|(name, detail, _)| (name, detail)).collect() };
    match shell_prompt::search(ui, key, title, label, query, &rows) {
        None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
        Some((query, pxui::ui::Pick::Pick(index))) => {
            let found = matches(&query);
            (None, found.get(index as usize).map(|(_, _, intent)| intent.clone()))
        }
        Some((query, _)) => (Some(again(query)), None),
    }
}

/// The open prompt of a frame: draws it and returns (next prompt, intent).
#[allow(clippy::too_many_arguments)]
pub fn prompt_panel<'a>(value: &ModelView<'a>, focus: &Panel, keymap: &[Command], frame_: &frame::Frame, workspace: &shell, text: &text_pane::State, text_shown: Option<&text_pane::Shown>, ui: &mut Ui, prompt_: Option<prompt>) -> Result<Prompted, rays_diagnostic::Diagnostic> {
    let next: Prompted = match prompt_.clone() {
        None => (None, None),
        Some(prompt::Keys) => {
            let commands: Vec<Command> = editor_commands::keymap().iter().filter(|c| !matches!(action_of(c), action::List_command(_) | action::Frame_tile)).cloned().collect();
            let context = format!("{} focused", editor_commands::pane_name(focus).to_lowercase());
            let text_keys: Vec<(String, Trigger)> = editor_commands::text_keys().into_iter().map(|(l, t)| (l.to_string(), t)).collect();
            let category = |c: &Command| editor_commands::sheet_group(c).to_string();
            if which_key::sheet(ui, &context, Some(&category), &text_keys, &commands) { (Some(prompt::Keys), None) } else { (None, None) }
        }
        Some(prompt::Saving(name)) => match shell_prompt::name(ui, "preset-save", "Save preset", "Name for the current state of the document", "Preset name", &name) {
            None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
            Some((name, pxui::ui::Pick::Submit)) => (None, Some(prompt_intent::Save_preset_file(name))),
            Some((name, _)) => (Some(prompt::Saving(name)), None),
        },
        Some(prompt::Making_macro { nodes, draft, state }) => {
            let literals: Vec<String> = draft.literals.iter().map(|(_, e)| lisp::flat(e)).collect();
            match shell_prompt::macro_(ui, "make-macro", "Make a macro from the selection", &literals, &draft.free, &state) {
                None => (None, None),
                Some((state, shell_prompt::MacroResult::Submit)) => (None, Some(prompt_intent::Edit_source(fe::macro_op(&draft, &nodes, &state.name, &state.holes)))),
                Some((state, shell_prompt::MacroResult::None)) => (Some(prompt::Making_macro { nodes, draft, state }), None),
            }
        }
        Some(prompt::Browsing { query, presets, last_state }) => {
            let entries = |query: &str| -> Vec<(String, f64, bool)> {
                let mut all: Vec<(String, f64, bool)> = last_state.iter().map(|time| ("Last edited state".to_string(), *time, true)).collect();
                all.extend(presets.iter().map(|(name, time)| (name.clone(), *time, false)));
                all.into_iter().filter(|(name, _, _)| pxui::ui::fuzzy_match(query, name)).collect()
            };
            let rows = |query: &str| -> Vec<(String, String)> {
                entries(query)
                    .into_iter()
                    .map(|(name, time, recovery)| {
                        let (mon, day, hour, minute) = month_day_hour_minute(time);
                        (name, format!("{}{:02}-{:02} {:02}:{:02}", if recovery { "Autosave \u{b7} " } else { "" }, mon, day, hour, minute))
                    })
                    .collect()
            };
            let title = format!("Presets \u{b7} {}", entries("").len());
            let same = |query: String| prompt::Browsing { query, presets: presets.clone(), last_state };
            match shell_prompt::search(ui, "preset-browse", &title, "Search presets", &query, &rows) {
                None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
                Some((query, pxui::ui::Pick::Pick(index))) => match entries(&query).get(index as usize) {
                    Some((name, _, recovery)) => (None, Some(if *recovery { prompt_intent::Load_last_state } else { prompt_intent::Load_preset_file(name.clone()) })),
                    None => (None, None),
                },
                Some((query, pxui::ui::Pick::Delete(index))) => match entries(&query).get(index as usize) {
                    Some((name, _, recovery)) => {
                        let intent = if *recovery { prompt_intent::Delete_last_state(query.clone()) } else { prompt_intent::Delete_preset_file { name: name.clone(), query: query.clone() } };
                        (Some(same(query)), Some(intent))
                    }
                    None => (Some(same(query)), None),
                },
                Some((query, _)) => (Some(same(query)), None),
            }
        }
        Some(prompt::Jumping(query)) => {
            // every graph, grouped like the outline
            let mut all: Vec<(String, String, prompt_intent)> = navigator::jump_rows(&value.doc.workspace.ws.checked).into_iter().map(|(name, group)| (name.clone(), group, prompt_intent::Go(name))).collect();
            // and every node of the graph the pane shows: picking one selects it and frames it
            fn nodes(s: &fproj::Scope, out: &mut Vec<(String, String, prompt_intent)>) {
                for n in &s.nodes {
                    if !n.synthetic {
                        out.push((fproj::title(n), format!("node \u{b7} {}", n.head), prompt_intent::Find_node(n.path.clone())));
                    }
                    if let Some(z) = &n.zone {
                        nodes(&z.scope, out);
                    }
                }
            }
            if let Some(k) = pane_laid(value) {
                nodes(&k.scope, &mut all);
            }
            search_prompt(ui, "graph-jump", "Jump to graph or node", "Search graphs and nodes", &query, all, &prompt::Jumping)
        }
        Some(prompt::Symbols(query)) => {
            // every graph, function, macro and [let*] name of the text, by position
            let all: Vec<(String, String, prompt_intent)> = match text_shown {
                Some(shown) => {
                    let source = text_pane::current_text(text, shown)?;
                    text_pane::symbols(&source)
                        .into_iter()
                        .map(|(name, kind_, byte)| {
                            let line = rays_diagnostic::position_of_offset(&source, byte).line;
                            (name, format!("{kind_} \u{b7} line {line}"), prompt_intent::Run_action(action::Text_command(text_pane::Command::Select(byte, byte))))
                        })
                        .collect()
                }
                None => Vec::new(),
            };
            search_prompt(ui, "symbol-jump", "Go to symbol", "Search bindings", &query, all, &prompt::Symbols)
        }
        Some(prompt::Referencing(name, query)) => {
            let all: Vec<(String, String, prompt_intent)> = match text_shown {
                Some(shown) => {
                    let source = text_pane::current_text(text, shown)?;
                    text_pane::references(&source, &name)
                        .into_iter()
                        .map(|(byte, line)| {
                            let n = rays_diagnostic::position_of_offset(&source, byte).line;
                            (line, format!("line {n}"), prompt_intent::Run_action(action::Text_command(text_pane::Command::Select(byte, byte))))
                        })
                        .collect()
                }
                None => Vec::new(),
            };
            let held = name.clone();
            search_prompt(ui, "references", &format!("References to {name}"), "Search uses", &query, all, &move |q| prompt::Referencing(held.clone(), q))
        }
        Some(prompt::Going(query)) => match shell_prompt::name(ui, "goto-line", "Go to line", "A line, or line:column", "Line", &query) {
            None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
            Some((query, pxui::ui::Pick::Submit)) => {
                let target = match text_shown {
                    Some(shown) => text_pane::line_target(&text_pane::current_text(text, shown)?, &query),
                    None => None,
                };
                match target {
                    Some(byte) => (None, Some(prompt_intent::Run_action(action::Text_command(text_pane::Command::Select(byte, byte))))),
                    None => (Some(prompt::Going(query)), None),
                }
            }
            Some((query, _)) => (Some(prompt::Going(query)), None),
        },
        Some(open_prompt @ prompt::Picking { .. }) => {
            let prompt::Picking { split, at } = open_prompt.clone() else { unreachable!() };
            let kinds = [Panel::Graph, Panel::List, Panel::Lisp, Panel::Inspector, Panel::Outline, Panel::Timeline, Panel::View(String::new()), Panel::Canvas(String::new())];
            let digit = frame_.input.events.iter().find_map(|e| match e {
                Event::Key_pressed(Key::KeyChar(c)) if *c >= '1' && (*c as u32) < ('1' as u32 + kinds.len() as u32) => Some((*c as u32 - '1' as u32) as usize),
                _ => None,
            });
            let pick = |i: usize| -> Prompted {
                let panel = kinds[i].clone();
                if split { (Some(prompt::Splitting { panel, axis: Axis::H }), None) } else { (None, Some(prompt_intent::Run_action(action::Window_new(panel)))) }
            };
            match digit {
                Some(i) => pick(i),
                None => {
                    let keys: Vec<String> = (0..kinds.len()).map(|i| (i + 1).to_string()).collect();
                    let items: Vec<(String, bool)> = kinds.iter().map(|p| (panels::name(p).to_string(), true)).collect();
                    let opts = MenuOpts { width: Some(200.0), keys, ..Default::default() };
                    match ui.context_menu(at, &opts, if split { "split-kind" } else { "window-kind" }, &items) {
                        MenuResult::Open => (Some(open_prompt), None),
                        MenuResult::Dismiss => (None, None),
                        MenuResult::Pick(i) => pick(i as usize),
                    }
                }
            }
        }
        Some(prompt::Splitting { panel, axis }) => {
            // the split follows the pointer over the panel under it; Tab turns it, a click places it
            let pressed = |key: Key| frame_.input.events.iter().any(|e| matches!(e, Event::Key_pressed(k) if *k == key));
            let axis = if pressed(Key::Tab) { if axis == Axis::H { Axis::V } else { Axis::H } } else { axis };
            let (mx, my) = frame_.input.pointer;
            let leaf = geometry(value, workspace, frame_).leaves.iter().find(|l| {
                let (x, y, w, h) = l.frame;
                !l.floating && pxui::ui::contains((x as f64, y as f64, w as f64, h as f64), (mx, my))
            }).cloned();
            let ratio = match &leaf {
                Some(l) => {
                    let (x, y, w, h) = l.frame;
                    let r = if axis == Axis::H { (mx - x as f64) / (w.max(1) as f64) } else { (my - y as f64) / (h.max(1) as f64) };
                    r.min(0.9).max(0.1)
                }
                None => 0.5,
            };
            let b = ui.box_("split-pending", BoxOpts::default().flags(CLICKABLE).w(Size::Px(frame_.input.size.0 as f64)).h(Size::Px(frame_.input.size.1 as f64)).at((0.0, 0.0)));
            ui.to_front(900, &b);
            let theme = ui.theme();
            let painted = leaf.clone();
            let name = panels::name(&panel).to_uppercase();
            ui.draw_over(&b, move |paint, _| {
                let Some(l) = &painted else { return };
                let (x, y, w, h) = l.frame;
                let (x, y, w, h) = (x as f64, y as f64, w as f64, h as f64);
                let cut = if axis == Axis::H { x + w * ratio } else { y + h * ratio };
                let (nx, ny, nw, nh) = if axis == Axis::H { (cut, y, x + w - cut, h) } else { (x, cut, w, y + h - cut) };
                // the panel being split is tinted, the room of the new one more
                let tint = |alpha: i64| rays_math::color::with_alpha(theme.accent, alpha);
                paint.fill(x, y, w, h, tint(22));
                paint.fill(nx, ny, nw, nh, tint(56));
                // the new panel's room: accent brackets 6 inside its corners; the panel kept stays as it is
                paint.brackets(nx + 6.0, ny + 6.0, nw - 12.0, nh - 12.0, 0.0, 12.0, 1.0, theme.accent);
                // the cut: a hairline through the panel with a short accent cap at each end
                let (a, b2, (ux, uy)) = if axis == Axis::H { ((cut, y), (cut, y + h), (0.0, 1.0)) } else { ((x, cut), (x + w, cut), (1.0, 0.0)) };
                paint.line_w(a, b2, 1.0, theme.accent);
                for (px, py, sgn) in [(a.0, a.1, 1.0), (b2.0, b2.1, -1.0)] {
                    let (px, py) = (px + sgn * ux, py + sgn * uy);
                    paint.line_w((px - 5.0 * uy, py - 5.0 * ux), (px + 5.0 * uy, py + 5.0 * ux), 3.0, theme.accent);
                }
                // what is being made, in the new panel's room, and the keys that steer it
                let (cx, cy) = (nx + nw / 2.0, ny + nh / 2.0);
                if nw > 90.0 && nh > 40.0 {
                    let cw = paint.cap_width(&name);
                    paint.cap((cx - cw / 2.0, cy - 14.0), Some(theme.accent), &name);
                    let hint = "TAB  TURN   ESC  CANCEL";
                    let hw = paint.cap_width(hint);
                    paint.cap((cx - hw / 2.0, cy + 4.0), Some(pxui::theme::ink_3(&theme)), hint);
                }
                paint.ratio((mx + 12.0, my + 12.0), (ratio * 100.0) as i64, 100 - (ratio * 100.0) as i64);
            });
            let signal = ui.signal(&b);
            let cancel = pressed(Key::Escape) || frame_.input.events.iter().any(|e| matches!(e, Event::Pointer_pressed(MouseButton::RightButton, _)));
            if cancel {
                (None, None)
            } else {
                match (signal.clicked, &leaf) {
                    (true, Some(l)) => (None, Some(prompt_intent::Run_action(action::Panel_split_at(l.path.clone(), axis, ratio, panel)))),
                    _ => (Some(prompt::Splitting { panel, axis }), None),
                }
            }
        }
        Some(prompt::Palette(query)) => {
            // Every keymap command once per id (undo has several chords).
            let mut commands: Vec<&Command> = Vec::new();
            for command in keymap {
                if command.scope.is_some() && command.scope != Some(editor_commands::scope(focus)) {
                    continue;
                }
                if commands.iter().any(|c| c.id == command.id) {
                    continue;
                }
                commands.push(command);
            }
            let all: Vec<(String, String, prompt_intent)> = commands.iter().map(|c| (c.label.clone(), String::new(), prompt_intent::Run_action(action_of(c).clone()))).collect();
            search_prompt(ui, "command-palette", "Commands", "Search commands", &query, all, &prompt::Palette)
        }
    };
    // A closed prompt must not keep keyboard focus into the next frame.
    if next.0.is_none() && prompt_.is_some() {
        ui.dismiss_popup();
    }
    Ok(next)
}

/// The gutters, last over every pane's hit area, and the layout the frame asks for through them.
#[allow(clippy::too_many_arguments)]
pub fn settle_layout<'a>(value: &ModelView<'a>, vw: &ModelView<'a>, workspace: shell, frame_: &frame::Frame, keymap: &[Command], shortcut_frame: &frame::Frame, g: &layout::Geometry, intents: &[chrome::Intent], strip_layout: Vec<chrome::Intent>, timeline_leaves: &[(Leaf, UiBox)], platform: input::Platform, ui: &mut Ui) -> (shell, Vec<change>) {
    // The gutters last, on top of every pane's hit area: a drag applies from the next frame.
    let state = |path: &[usize]| panel_state(value, path);
    let grips = chrome::splitters(Some(&state), Some(&workspace.hidden), &shell_tree(value, &workspace), ui, shortcut_frame);
    let dragging = intents.iter().find_map(|i| match i {
        chrome::Intent::Dragging(path, released) => Some((path.clone(), *released)),
        _ => None,
    });
    let drops = chrome::drop_targets(ui, dragging, g, &state);
    let key_of = |id: &str| -> String {
        match keymap.iter().find(|c| c.id == id) {
            Some(Command { trigger: Some(trigger), .. }) => keymap::label(platform, trigger),
            _ => String::new(),
        }
    };
    let mut timeline_menus: Vec<chrome::Intent> = Vec::new();
    for (leaf, boxed) in timeline_leaves {
        if leaf.panel != Panel::Timeline || leaf.body == (0, 0, 0, 0) {
            continue;
        }
        ui.within(boxed, &mut |ui| {
            let at = ui.context_at(boxed);
            timeline_menus.extend(chrome::panel_menu(Some(&state), Some(&key_of), ui, leaf, boxed, at));
        });
    }
    let mut all: Vec<chrome::Intent> = grips.clone();
    all.extend(drops);
    all.extend(timeline_menus);
    all.extend(strip_layout);
    let (mut workspace, drag_changes) = layout_intents(vw, &workspace, &all);
    if workspace.window_live.is_some() && !intents.iter().chain(grips.iter()).any(|i| matches!(i, chrome::Intent::Window_drag(..))) {
        workspace.window_live = None;
    }
    // a gutter drag that ended without its release (the window lost the focus, the pointer was
    // cancelled) leaves no size behind for the next click on a gutter to commit
    if workspace.live.is_some()
        && !grips.iter().any(|i| matches!(i, chrome::Intent::Resize { .. } | chrome::Intent::Settled))
        && (!frame::mouse_down(MouseButton::LeftButton, frame_) || frame::has_event(&|e| matches!(e, Event::Focus_lost | Event::Pointer_cancelled(MouseButton::LeftButton)), frame_))
    {
        workspace.live = None;
    }
    (workspace, drag_changes)
}

type Target = (Panel, Option<layout::Path>);

/// The pane the last press landed in takes the focus; the panes' PXUI keys name them.
pub fn follow_press<'a>(value: &ModelView<'a>, workspace: &shell, frame_: &frame::Frame, roots: &[(Leaf, UiBox)], timeline_root: &Option<(Panel, UiBox)>, focus: Panel, focus_path: Option<layout::Path>, ui: &mut Ui) -> (Panel, Option<layout::Path>, Vec<(i64, Target)>) {
    let mut pane_roots: Vec<(Target, UiBox)> = roots.iter().map(|(l, b)| ((l.panel.clone(), Some(l.path.clone())), b.clone())).collect();
    if let Some((panel, b)) = timeline_root {
        pane_roots.push(((panel.clone(), None), b.clone()));
    }
    let mut latest = -1i64;
    let mut found: Target = (focus, focus_path);
    for (target, b) in &pane_roots {
        if let Some(index) = ui.signal(b).subtree_press {
            if index >= latest {
                latest = index;
                found = target.clone();
            }
        }
    }
    let (mut focus, mut focus_path) = found;
    // a focus whose leaf an edit split, retyped or moved follows it, so the next panel key acts
    if focus_path.is_some() {
        if let Some(leaf) = focused_leaf(&geometry(value, workspace, frame_), &focus, focus_path.as_deref()) {
            focus = leaf.panel.clone();
            focus_path = Some(leaf.path.clone());
        }
    }
    let pane_keys = pane_roots.iter().map(|(target, b)| (pxui::ui_core::key(b), target.clone())).collect();
    (focus, focus_path, pane_keys)
}

#[allow(clippy::too_many_arguments)]
pub fn draw_notice<'a>(value: &ModelView<'a>, frame_: &frame::Frame, roots: &[(Leaf, UiBox)], header_focus: &Option<layout::Path>, graph_body: layout::Bounds, scope_view: &scope::Pane, scope_active: bool, ui: &mut Ui) {
    // echo, the sheet's [08]: messages only (saved, undo and redo results, refusals), never a key
    // press; the last one fades after 3 seconds.  It stands at the bottom-left of the focused pane,
    // above the strip.
    if let Some((kind_, text)) = &value.notice {
        if frame_.input.t - value.notice_at < 3.0 {
            let bounds = match roots.iter().find(|(l, _)| Some(&l.path) == header_focus.as_ref()) {
                Some((l, _)) => l.body,
                None => graph_body,
            };
            fn cards(view: &scope::Pane, s: &fproj::Scope, out: &mut Vec<(f64, f64, f64, f64)>) {
                for n in &s.nodes {
                    out.extend(pxui_graph::scope_frame::inspect::box_of(view, &n.path));
                    if let Some(z) = &n.zone {
                        cards(view, &z.scope, out);
                    }
                }
            }
            let mut avoid = Vec::new();
            if let (Some(k), true) = (pane_laid(value), scope_active) {
                cards(scope_view, &k.scope, &mut avoid);
            }
            let tip = match kind_ {
                notice_kind::Info => TipKind::Info,
                notice_kind::Refusal => TipKind::Refusal,
            };
            status_bar::tips(ui, bounds, &avoid, &[(text.clone(), tip)]);
        }
    }
}

/// The viewport handles: their edits, a pick, and the changes they write.
#[allow(clippy::too_many_arguments)]
pub fn draw_handles<'a>(value: &ModelView<'a>, workspace: &shell, selection: selection::Selection, selected: &Option<picked>, lowered_level: bool, scope_active: bool, active: &Option<Leaf>, view_root: &UiBox, view_handles: &ViewHandles, ui: &mut Ui) -> (selection::Selection, bool, Vec<change>, Option<(i64, Vec<(String, Value)>)>) {
    // a lowered node is edited through its text: the handles of the node selected in the graph pane
    // write its arguments, the list's rows of a geometry object have none
    let scope_target = if scope_active { scope_node(value) } else { None };
    let handle_target: Option<picked> = match &scope_target {
        Some((_, node)) => Some(of_sop(node)),
        None => {
            if lowered_level { None } else { selected.clone() }
        }
    };
    let (handle_edits, grab, picked_id) = match active {
        Some(leaf) if visible(workspace, &leaf.panel) => {
            let mut out = (Vec::new(), false, None);
            // ponytail: space reads the pane scope and scene, so reuse the current model until selection affects that operation.
            ui.within(view_root, &mut |ui| {
                out = view_handles(ui, handle_target.as_ref(), space(value), leaf.body);
            });
            out
        }
        _ => (Vec::new(), false, None),
    };
    let selection = match picked_id {
        Some(id) if pane_level(value) == Level::Scene => selection::select(id),
        _ => selection,
    };
    let (handle_ops, handle_changes) = match (&scope_target, &handle_target, handle_edits.is_empty()) {
        (Some((path, node)), _, false) => {
            let parameters = flow_sop::port::parameters(false, &sop::node::parameter_fields(node)).unwrap_or_default();
            let ops = parameters
                .iter()
                .filter_map(|p| {
                    if p.fields.iter().any(|f| handle_edits.iter().any(|(n, _)| *n == f.name)) {
                        let fields: Vec<FieldView> = p
                            .fields
                            .iter()
                            .map(|f| match handle_edits.iter().find(|(n, _)| *n == f.name) {
                                Some((_, current)) => FieldView { current: current.clone(), ..f.clone() },
                                None => f.clone(),
                            })
                            .collect();
                        Some(change::Syntax_edit(Op::SetArg { node: path.clone(), key: ArgKey::Kw(p.path.clone()), sub: Vec::new(), value: scene_sync::value_syntax(&fields) }))
                    } else {
                        None
                    }
                })
                .collect();
            (ops, None)
        }
        (_, Some(node), false) => (Vec::new(), Some((node.id, handle_edits))),
        _ => (Vec::new(), None),
    };
    (selection, grab, handle_ops, handle_changes)
}

/// What the graph pane and the outline asked for, as document changes.
pub fn pane_changes<'a>(value: &ModelView<'a>, vw: &ModelView<'a>, command_changes: Vec<change>, scope_changes: &[scope::Change], menu_pick: &Option<String>, outline_intents: &[navigator::Intent]) -> Vec<change> {
    let mut changes = command_changes;
    for c in scope_changes {
        match c {
            scope::Change::Syntax_edit(op) => changes.push(change::Syntax_edit(op.clone())),
            scope::Change::Layout_active { graph, index } => changes.push(change::Layout_edit(layout_edit::Op::SetLayout { graph: graph.clone(), index: *index })),
            // the pane's notices are all refusals
            scope::Change::Notice(message) => changes.push(change::Declined(message.clone())),
            scope::Change::Defn_requested(paths) => changes.push(defn_change(vw, paths)),
            scope::Change::Copy_requested(paths) => changes.push(copy_bindings(value, paths)),
            scope::Change::Paste_requested => changes.extend(paste_bindings(value)),
            _ => {}
        }
    }
    if let Some(key) = menu_pick {
        changes.extend(scope_add(value, key));
    }
    for i in outline_intents {
        match i {
            navigator::Intent::SetDefault { graph, input, value: v, integer } => {
                let text = if *integer { format!("{}", v.round() as i64) } else { lisp::float(*v) };
                let mut builder = flow_syntax::arena::SyntaxBuilder::new();
                let name = builder.intern(&text);
                let id = builder.push(0, SNode::Num(name), Default::default(), None);
                changes.push(change::Syntax_edit(Op::SetInputDefault { form: graph.clone(), input: input.clone(), value: builder.freeze().handle(id) }));
            }
            navigator::Intent::Rename { graph, to } => changes.push(change::Syntax_edit(Op::RenameGraph { name: graph.clone(), to_: to.clone() })),
            navigator::Intent::Remove(graph) => changes.push(change::Syntax_edit(Op::RemoveGraph { name: graph.clone() })),
            navigator::Intent::NewGraph(context) => changes.push(change::Syntax_batch("New graph".to_string(), vec![new_graph(value, context).1])),
            navigator::Intent::Flag { node, name, value: on } => {
                let mut builder = flow_syntax::arena::SyntaxBuilder::new();
                let flag = builder.intern(if *on { "true" } else { "false" });
                let id = builder.push(0, SNode::Sym(flag), Default::default(), None);
                changes.push(change::Syntax_edit(Op::SetArg {
                    node: node.clone(),
                    key: ArgKey::Kw(name.clone()),
                    sub: Vec::new(),
                    value: builder.freeze().handle(id),
                }));
            },
            navigator::Intent::Open { .. } | navigator::Intent::Macro(_) | navigator::Intent::Layout(_) | navigator::Intent::Add => {}
        }
    }
    changes
}

type Inspected = (Option<Panel_result>, Vec<change>, Vec<(String, Value)>, bool, Vec<scope::Change>, Vec<(carry_mod::Place, bool)>);

/// Every inspector leaf: the one in use over the frame's state, the others over their own pane.
#[allow(clippy::too_many_arguments)]
pub fn draw_inspectors<'a>(
    value: &ModelView<'a>,
    workspace: &shell,
    image: &dyn Fn(i64) -> Option<rays::image::Image>,
    camera_panel: &dyn Fn(&mut Ui) -> Panel_result,
    inspector_visible: bool,
    unchanged: &editor_document::settings::T,
    g: &layout::Geometry,
    scope_view: &scope::Pane,
    selection: &selection::Selection,
    scope_active: bool,
    graph_shown: bool,
    focus: &Panel,
    focus_path: &Option<layout::Path>,
    pane_of: &dyn Fn(&Host) -> ModelView<'a>,
    in_use: &dyn Fn(&ModelView<'a>) -> bool,
    roots: &[(Leaf, UiBox)],
    ui: &mut Ui,
) -> (Option<Panel_result>, Vec<change>, Vec<(String, Value)>, Vec<scope::Change>, Vec<(carry_mod::Place, bool)>, bool) {
    // the split a card of the editor graph is, sized another way from what it shows now
    let resized = |node: &[Symbol], how: layout::Way| -> Option<layout_edit::Size> {
        match (node, &value.doc.shell) {
            ([graph, name], Some(sh)) if !workspace.restored && workspace_doc::editor_graph(&value.doc.workspace.ws).map(|g| g.name).as_deref() == Some(graph.as_str()) => {
                let path = sh.origins.iter().find_map(|(path, origin)| if *origin == Origin::Bound(name.to_string()) { Some(path.clone()) } else { None })?;
                let splitter = g.splitters.iter().find(|s| s.node.as_ref() == Some(&path))?;
                Some(match layout::resized(splitter, how) {
                    panels::Size::Ratio(r) => layout_edit::Size::Ratio(r),
                    panels::Size::First(n) => layout_edit::Size::First(n),
                    panels::Size::Second(n) => layout_edit::Size::Second(n),
                })
            }
            _ => None,
        }
    };
    let mut inspectors: Vec<(Leaf, Inspected)> = Vec::new();
    for (leaf, root) in roots {
        let host: Host = (panel_key(&value.doc, &leaf.path), leaf.path.clone(), leaf.panel.clone(), Host_kind::List);
        let pane = pane_of(&host);
        if in_use(&pane) {
            let r = inspect_panel(image, camera_panel, inspector_visible, unchanged, &resized, g, ui, (*value, scope_view, selection, scope_active), root, leaf.body);
            inspectors.push((leaf.clone(), r));
        } else {
            let shown = graph_shown && stored_projection(&pane) == Graph_view && graph_name(&pane).is_some();
            let view = pane_view(&pane).clone();
            let picked_selection = pane_selection(&pane).clone();
            let (panel, _, _, _, moves, drops) = inspect_panel(image, camera_panel, inspector_visible, unchanged, &resized, g, ui, (pane, &view, &picked_selection, shown), root, leaf.body);
            inspectors.push((leaf.clone(), (panel, Vec::new(), Vec::new(), value.live_cook, moves, drops)));
        }
    }
    let focused = inspectors.iter().find(|(l, _)| *focus == Panel::Inspector && Some(&l.path) == focus_path.as_ref());
    let panel = match (focused, inspectors.first()) {
        (Some((_, r)), _) | (None, Some((_, r))) => r.0.clone(),
        (None, None) => None,
    };
    let inspector_changes = inspectors.iter().flat_map(|(_, r)| r.1.clone()).collect();
    let settings_changes = inspectors.iter().flat_map(|(_, r)| r.2.clone()).collect();
    let workspace_moves = inspectors.iter().flat_map(|(_, r)| r.4.clone()).collect();
    let inspector_drops = inspectors.iter().flat_map(|(_, r)| r.5.clone()).collect();
    let live_cook = inspectors.iter().find_map(|(_, r)| if r.3 != value.live_cook { Some(r.3) } else { None }).unwrap_or(value.live_cook);
    (panel, inspector_changes, settings_changes, workspace_moves, inspector_drops, live_cook)
}

type Marks = (Vec<(Path, String)>, Option<(Path, bool)>);

/// What the graph pane marks while a payload is carried: the places that take it (the key route's
/// letters) and the one under the pointer.
pub fn carry_marks(value: &EditorModel) -> Marks {
    let Some(c) = &value.carry else { return (Vec::new(), None) };
    let path_of = |place: &carry_mod::Place| -> Option<Path> {
        match place {
            carry_mod::Place::Node(p) => Some(p.clone()),
            carry_mod::Place::Graph(g) => Some(vec![g.clone().into()]),
            carry_mod::Place::Object(id) | carry_mod::Place::Surface { object_: id, .. } => match value.doc.homes.objects.iter().find(|(n, _)| n == id) {
                Some((_, Home::BoundAt(p))) => Some(p.clone()),
                _ => None,
            },
            carry_mod::Place::Viewport(_) | carry_mod::Place::Text { .. } => None,
        }
    };
    let lit = match &c.targets {
        Some((_, targets)) if c.via == Via::Keys => targets.iter().filter_map(|(letter, place, _)| path_of(place).map(|p| (p, letter.clone()))).collect(),
        _ => Vec::new(),
    };
    let hot = c.preview.as_ref().and_then(|preview| path_of(carry_place(preview)).map(|p| (p, matches!(preview, carry_preview::Showing { .. }))));
    (lit, hot)
}

type RowSet = (row_source, (Option<i64>, Option<i64>), (Vec<tree::Row>, Vec<String>));

/// The rows of a level's list and of every list that draws, by panel key.
pub fn list_rows<'a>(value: &ModelView<'a>, listing_: bool, list_drawn: bool, pane_of: &dyn Fn(&Host) -> ModelView<'a>, list_hosts: &[Host]) -> (Vec<RowSet>, Vec<tree::Row>, Vec<String>, Vec<(Key_, (Vec<tree::Row>, Vec<String>))>) {
    // the rows of a level's list, made once per network and kept while a list shows them
    let rows_of = |sets: &mut Vec<RowSet>, pane: ModelView<'a>| -> (Vec<tree::Row>, Vec<String>) {
        let (source, made) = rows(&pane);
        let key = (displayed(&pane), pane.doc.active_camera);
        if let Some((_, _, r)) = sets.iter().find(|(s, k, _)| same_source(s, &source) && *k == key) {
            return r.clone();
        }
        let entry = match value.rows.iter().find(|(s, k, _)| same_source(s, &source) && *k == key) {
            Some(entry) => entry.clone(),
            None => (source, key, made()),
        };
        let r = entry.2.clone();
        sets.insert(0, entry);
        r
    };
    let mut sets: Vec<RowSet> = Vec::new();
    let (rows_, columns) = if listing_ || list_drawn { rows_of(&mut sets, *value) } else { (Vec::new(), Vec::new()) };
    // the rows of every list that draws, by panel key: made here, before the frame builds
    let mut host_rows = Vec::new();
    for host in list_hosts {
        let pane = pane_of(host);
        let r = rows_of(&mut sets, pane);
        host_rows.insert(0, (host.0.clone(), r));
    }
    (sets, rows_, columns, host_rows)
}

/// `/ a`: the node menu of the graph the pane shows.
pub fn node_menu_step<'a>(value: &ModelView<'a>, workspace: &shell, frame_: &frame::Frame, actions: &[action], graph_shown: bool) -> (Option<node_menu::Menu>, Vec<change>) {
    if actions.contains(&action::Add_node) && graph_shown {
        let (gx, gy, gw, gh) = match graph_leaf(value, &geometry(value, workspace, frame_)) {
            Some(leaf) => leaf.body,
            None => (0, 0, 0, 0),
        };
        let (mx, my) = frame_.input.pointer;
        let at = if pxui::ui::contains((gx as f64, gy as f64, gw as f64, gh as f64), (mx, my)) { (mx as i64, my as i64) } else { (gx + gw / 3, gy + gh / 3) };
        match open_menu(None, value, at) {
            Some(menu) => (Some(menu), Vec::new()),
            None => (value.menu.clone(), vec![change::Declined("Open a graph to add a node to it".to_string())]),
        }
    } else {
        (value.menu.clone(), Vec::new())
    }
}

type TextState = (Option<Host>, text_pane::State, Option<text_pane::Shown>, Vec<(Key_, layout::Path, text_pane::State, text_pane::Shown)>);

/// What each text pane shows: its state and what it is showing.
pub fn text_state<'a>(value: &ModelView<'a>, actions: &[action], pane_of: &dyn Fn(&Host) -> ModelView<'a>, text_hosts: &[Host]) -> Result<TextState, rays_diagnostic::Diagnostic> {
    let ws = &value.doc.workspace.ws;
    let text_of = |state: &text_pane::State, host: &Host| -> Result<Option<(text_pane::State, text_pane::Shown)>, rays_diagnostic::Diagnostic> {
        let pane = pane_of(host);
        let graph = match graph_name(&pane) {
            None if host.2 == Panel::Lisp => ws.checked.graphs.first().map(|g| g.name.clone()),
            graph => graph,
        };
        let Some(graph) = graph else { return Ok(None) };
        let selected = match scope::selected(pane_view(&pane)).as_slice() {
            [path] => Some(path.clone()),
            _ => None,
        };
        // while a payload is carried the text is the document it was picked up from: the preview is
        // not drawn here, so the byte under the pointer does not move when a put is previewed
        let shown_ws = match &value.carry {
            Some(c) => &c.original.workspace.ws,
            None => ws,
        };
        Ok(Some(text_pane::shown(Some(shown_ws), state, &shown_ws.source, &graph, selected.as_ref())?))
    };
    let text_host = text_hosts.iter().find(|(key, _, _, _)| Some(key) == value.text_at.as_ref()).cloned();
    let current = match text_host.as_ref() { Some(host) => text_of(&value.text, host)?, None => None };
    let (mut text, text_shown) = match current {
        Some((text, shown)) => (text, Some(shown)),
        None => (value.text.clone(), None),
    };
    // the keys of the text (find, go to) wait in the pane's state for its view; nothing without one
    if text_shown.is_some() {
        for a in actions {
            if let action::Text_command(command) = a {
                text = text_pane::command(&text, command.clone());
            }
        }
    }
    let mut other_texts = Vec::new();
    for host in text_hosts {
        if Some(&host.0) == value.text_at.as_ref() {
            continue;
        }
        if let Some((state, shown)) = text_of(&local_of(&value.locals, &host.0).code, host)? {
            other_texts.push((host.0.clone(), host.1.clone(), state, shown));
        }
    }
    Ok((text_host, text, text_shown, other_texts))
}

/// Command +/-/0: the kit text of the panels (the graph pane zooms, viewports have no text).
pub fn set_text_size(value: &EditorModel, actions: &[action]) {
    for a in actions {
        if let action::Ui_scale(delta) = a {
            let sizes = [8i64, 9, 10, 11, 12, 13, 14, 16, 18];
            let default = sizes.iter().position(|s| *s == value.text_size).unwrap_or(5);
            let here = sizes.iter().position(|s| *s == value.ui.borrow().font_size()).unwrap_or(default);
            let index = if *delta == 0 { default } else { (here as i64 + delta).clamp(0, sizes.len() as i64 - 1) as usize };
            value.ui.borrow_mut().set_font_size(sizes[index]);
        }
    }
}

/// What the hosted sketch sees of this frame, and the window focus it tracks.
pub fn gated(value: &mut EditorModel, frame_: &frame::Frame, text_focus: bool, host_events: Vec<Event>) {
    let mut window_focus = value.window_focus;
    for e in &frame_.input.events {
        match e {
            Event::Focus_lost => window_focus = -1,
            Event::Focus_gained => window_focus = 0,
            _ => {}
        }
    }
    window_focus = if window_focus >= 0 { (window_focus + 1).min(2) } else { -1 };
    let roots: Vec<i64> = value.pane_keys.iter().map(|(k, _)| *k).collect();
    let pane_is_picture = match value.ui.borrow().root_at(frame_.input.pointer, &roots) {
        None => true,
        Some(key) => match value.pane_keys.iter().find(|(k, _)| *k == key) {
            Some((_, (panel, _))) => is_picture(panel),
            None => false,
        },
    };
    let gate = input::gate::Gate {
        prompt: value.prompt.is_some(),
        menu: value.menu.is_some(),
        leader: value.leader != router::State::Idle,
        text_focus,
        focus_on_picture: is_picture(&value.focus),
        pointer_on_picture: pane_is_picture,
        window_key: window_focus >= 2,
        pressed: frame_.input.events.iter().any(|e| matches!(e, Event::Pointer_pressed(..))),
        previous: value.live_frame.clone(),
        blocked: value.sketch_blocked.clone(),
    };
    let (mut live_frame, sketch_blocked) = input::gate::gate(&gate, &flow_native::live_frame::of_frame(frame_));
    if !host_events.is_empty() {
        let mut events = host_events;
        events.extend(live_frame.events.clone());
        live_frame.events = events;
    }
    value.live_frame = live_frame;
    value.sketch_blocked = sketch_blocked;
    value.window_focus = window_focus;
}

/// The header intents the frame's actions stand for: panel toggles, and the focused panel's split,
/// close and retype.
pub fn panel_intents<'a>(value: &ModelView<'a>, leaves: &[(layout::Path, Panel)], workspace: &shell, frame_: &frame::Frame, focus: &Panel, focus_path: Option<&[usize]>, actions: &[action]) -> Vec<chrome::Intent> {
    let panel_paths = |panel: &Panel| -> Vec<layout::Path> { leaves.iter().filter(|(_, p)| p == panel).map(|(path, _)| path.clone()).collect() };
    let toggle = |panel: &Panel| -> Vec<chrome::Intent> { panel_paths(panel).into_iter().map(chrome::Intent::Toggle).collect() };
    let expand = |panel: &Panel| -> Vec<chrome::Intent> { panel_paths(panel).into_iter().filter(|path| panel_state(value, path).collapsed).map(chrome::Intent::Toggle).collect() };
    let mut intents = Vec::new();
    for a in actions {
        match a {
            action::Toggle_graph => intents.extend(toggle(&Panel::Graph)),
            action::Toggle_inspector => intents.extend(toggle(&Panel::Inspector)),
            action::Toggle_timeline => {
                if panel_paths(&Panel::Timeline).is_empty() {
                    intents.push(chrome::Intent::Toggle(vec![HOST_BAR]));
                } else {
                    intents.extend(toggle(&Panel::Timeline));
                }
            }
            action::Open_camera => intents.extend(expand(&Panel::Inspector)),
            action::Add_node => intents.extend(expand(&Panel::Graph)),
            _ => {}
        }
    }
    // / o ...: the focused panel's split, close and retype, as its header menu; the geometry only
    // when one was asked for
    let on_focused = actions.iter().any(|a| matches!(a, action::Panel_split(_) | action::Panel_close | action::Panel_retype(_)));
    if let Some(leaf) = on_focused.then(|| focused_leaf(&geometry(value, workspace, frame_), focus, focus_path)).flatten() {
        for a in actions {
            match a {
                action::Panel_split(axis) => intents.push(chrome::Intent::Split_panel(leaf.path.clone(), *axis)),
                action::Panel_close => intents.push(chrome::Intent::Close_panel(leaf.path.clone())),
                // a view switch, below
                action::Panel_retype(Panel::Graph | Panel::List | Panel::Lisp) if leaf.panel == Panel::Graph => {}
                action::Panel_retype(panel) => intents.push(chrome::Intent::Retype_panel(leaf.path.clone(), panel.clone())),
                _ => {}
            }
        }
    }
    for a in actions {
        if let action::Panel_split_at(path, axis, ratio, panel) = a {
            intents.push(chrome::Intent::Split_panel_at(path.clone(), *axis, *ratio, panel.clone()));
        }
    }
    intents
}

/// The status bar's fps, sampled once a second.
pub fn fps_sample<'a>(value: &ModelView<'a>, frame_: &frame::Frame) -> (Option<i64>, f64) {
    let sample = frame_.input.t < value.status_fps_at || frame_.input.t - value.status_fps_at >= 1.0;
    if !sample {
        (value.status_fps, value.status_fps_at)
    } else {
        (if frame_.fps > 0.0 && frame_.fps.is_finite() { Some(frame_.fps.round() as i64) } else { None }, frame_.input.t)
    }
}

/// Command-Z in a text whose own undo stack is empty (a dragged number applies as it goes) reaches
/// the document history, unless a prompt, menu or rename owns the keys, or a text pane holds an
/// unapplied draft: an extra Command-Z after the draft is undone must not change the document
/// under it.
pub fn undo_passed(value: &EditorModel, text_focus: bool, modal: bool) -> Vec<action> {
    if text_focus && !modal && !has_draft(None, value) {
        match value.ui.borrow().passed_undo() {
            Some(pxui::ui::UndoKind::Undo) => vec![action::Undo],
            Some(pxui::ui::UndoKind::Redo) => vec![action::Redo],
            None => Vec::new(),
        }
    } else {
        Vec::new()
    }
}

/// The last command's label, or the leader's pending prefix, for 1.5 seconds.
pub fn hud_of<'a>(value: &ModelView<'a>, leader: &router::State, frame_: &frame::Frame, commands: &[Command]) -> Option<(String, f64)> {
    match commands.last() {
        Some(command) => {
            let platform = value.ui.borrow().platform();
            Some((format!("{} \u{b7} {}", keymap::label(platform, command.trigger.as_ref().expect("a routed command has a trigger")), command.label), frame_.input.t + 1.5))
        }
        None => match leader {
            router::State::Pending(prefix) if leader != value.leader => Some((format!("{} \u{b7} leader", if prefix.is_empty() { "Space".to_string() } else { format!("/ {prefix}") }), frame_.input.t + 1.5)),
            _ => value.hud.clone(),
        },
    }
}

/// A hidden pane set leaves the list and frame-camera keys out of the keymap.
pub fn live_keymap(all_ui_visible: bool, keymap: std::rc::Rc<Vec<Command>>) -> std::rc::Rc<Vec<Command>> {
    if all_ui_visible {
        keymap
    } else {
        std::rc::Rc::new(keymap.iter().filter(|c| !matches!(action_of(c), action::List_command(_) | action::Frame_camera)).cloned().collect())
    }
}

fn ui_platform(value: &EditorModel) -> input::Platform {
    value.ui.borrow().platform()
}

#[allow(clippy::too_many_arguments)]
pub fn update_frame(
    value: &mut EditorModel,
    carry_changed: bool,
    all_ui_visible: bool,
    text_focus: bool,
    camera_panel: &dyn Fn(&mut Ui) -> Panel_result,
    view_handles: &ViewHandles,
    render_status: Option<String>,
    error_status: Option<String>,
    view_state: Rc<dyn Fn(Option<&Panel_result>) -> Syntax>,
    frame_: &frame::Frame,
    gpu: Option<&flow_ir::flow_ir::gpu::Config>,
    pump: Option<&dyn Fn()>,
    image: &dyn Fn(i64) -> Option<rays::image::Image>,
    host_events: Vec<Event>,
) -> update {
    let carrying = value.carry.is_some();
    gated(value, frame_, text_focus, host_events);
    value.workspace.hidden = shell_hidden(&(&*value).into(), &value.workspace);
    follow_start(value);
    let (focus, focus_path) = if all_ui_visible {
        let roots: Vec<i64> = value.pane_keys.iter().map(|(k, _)| *k).collect();
        let pressed = value.ui.borrow().last_press_within(frame_, &roots);
        match pressed {
            Some(key) => value.pane_keys.iter().find(|(k, _)| *k == key).map(|(_, t)| t.clone()).unwrap_or((value.focus.clone(), value.focus_path.clone())),
            None => (value.focus.clone(), value.focus_path.clone()),
        }
    } else {
        (value.focus.clone(), value.focus_path.clone())
    };
    // a press in another graph panel makes it the pane in use before anything reads the pane
    follow_graph(value, &focus, focus_path.as_deref());
    // the lists, text panes and outlines open, and the ones the focus makes the ones in use
    let hosts: Vec<Host> = if all_ui_visible { panel_hosts(&(&*value).into()) } else { Vec::new() };
    follow_hosts(value, focus_path.as_ref(), &hosts);
    // a field of the graph pane owns the keys only while the pane is drawn: one left open when the
    // panel turned to its list or text view must not lock the keyboard
    let modal = value.prompt.is_some()
        || value.menu.is_some()
        || (all_ui_visible && graph_family(&(&*value).into()) && scope_name(&(&*value).into()).is_some() && scope::editing(pane_view(&(&*value).into())))
        || tree::editing(&value.tree);
    let text_focus = text_focus || modal;
    // routed as the new focus has it, without a copy of the model
    let held_focus = std::mem::replace(&mut value.focus, focus.clone());
    let keymap = live_keymap(all_ui_visible, routed(&(&*value).into()));
    value.focus = held_focus;
    let held_keys = frame_.input.keys.clone();
    let platform = ui_platform(&*value);
    let keymap_refs: Vec<&Command> = keymap.iter().collect();
    let (leader, commands, routed) = router::step(
        &value.held_keys,
        platform,
        &keymap_refs,
        &editor_commands::scope(&focus),
        text_focus,
        frame_,
        value.leader.clone(),
    );
    let frame_ = routed.as_ref();
    let hud = hud_of(&(&*value).into(), &leader, frame_, &commands);
    let mut actions: Vec<action> = commands.iter().map(|c| action_of(c).clone()).collect();
    let hud = match hud {
        Some((_, until)) if frame_.input.t >= until => None,
        other => other,
    };
    let guide = actions.iter().fold(value.guide, |guide, a| if *a == action::Guide_toggle { !guide } else { guide });
    let passed = undo_passed(&*value, text_focus, modal);
    let mut all_actions = passed;
    all_actions.extend(value.queued.clone());
    all_actions.extend(actions.drain(..));
    let mut actions = all_actions;
    if carrying {
        actions.retain(carry_allowed);
    }
    let copied = if actions.contains(&action::Copy_lisp) { Some(copy_workspace(&*value)) } else { None };
    set_text_size(&*value, &actions);
    let steady = if frame_.input.dt > 0.0 && frame_.input.dt == value.last_dt { value.steady + 1 } else { 0 };
    let (status_fps, status_fps_at) = fps_sample(&(&*value).into(), frame_);
    let keyboard_event = |event: &Event| matches!(event, Event::Key_pressed(_) | Event::Key_released(_) | Event::Text_input(_) | Event::Text_editing { .. });
    let shortcut_frame = if text_focus && frame_.input.events.iter().any(keyboard_event) {
        let mut filtered = frame_.clone();
        filtered.input.events.retain(|event| !keyboard_event(event));
        Cow::Owned(filtered)
    } else {
        Cow::Borrowed(frame_)
    };
    let (timeline_now, timeline_changes) = timeline::update(&value.timeline, &shortcut_frame);
    let (workspace, selection, tree_value, timeline_now, timeline_changes) = actions.iter().fold((value.workspace.clone(), pane_selection(&(&*value).into()).clone(), value.tree.clone(), timeline_now, timeline_changes), |state, a| apply_action(&*value, state, a));
    value.timeline = timeline_now;
    sync_scope(value);
    let mut other_panes = Vec::new();
    if all_ui_visible {
        let mut leaves = graph_leaves(&(&*value).into());
        leaves.sort_by(|a, b| a.0.cmp(&b.0));
        leaves.dedup_by(|a, b| a.0 == b.0);
        let active = panel_map::key(&value.panes);
        let active_leaf = panel_map::leaf(&value.panes);
        // An inactive projection may consume its own deferred selection, not the active model's.
        let select_later = value.select_later.clone();
        for (key, path) in leaves {
            if active.as_ref() == Some(&key) { continue; }
            with_pane_in_use(value, (&key, &path));
            sync_scope(value);
            value.select_later = select_later.clone();
            other_panes.push((key, path));
        }
        panel_map::activate(&mut value.panes, &active.unwrap_or_default(), &|| unreachable!("the active pane exists"));
        panel_map::set_leaf(&mut value.panes, active_leaf);
    }
    let mut vw = ModelView::from(&*value);
    vw.workspace = &workspace;
    vw.focus = &focus;
    vw.selection = &selection;
    let graph_shown = all_ui_visible && graph_family(&vw);
    let listing_ = listing(&vw);
    let hosted = |k: Host_kind| -> Vec<Host> { hosts.iter().filter(|h| h.3 == k).cloned().collect() };
    let list_hosts = hosted(Host_kind::List);
    let list_drawn = !list_hosts.is_empty();
    // What a text pane shows: a graph panel's own graph and selection, a lisp panel the ones of the
    // graph pane in use (the first graph of the workspace when none is open).
    // a panel's pane, laid out: the one in use, or one of the others
    let pane_of = |host: &Host| shown_as(&(&*value).into(), (&host.0, &host.1, &host.2));
    let in_use = |pane: &ModelView<'_>| pane.key == panel_map::key_ref(&value.panes);
    let ((text_host, text, text_shown, other_texts), text_error) =
        match text_state(&(&*value).into(), &actions, &pane_of, &hosted(Host_kind::Text)) {
            Ok(state) => (state, None),
            Err(error) => ((None, value.text.clone(), None, Vec::new()), Some(rays_diagnostic::to_string(&error))),
        };
    let scope_active = graph_shown && scope_name(&(&*value).into()).is_some();
    let mut scope_view = pane_view(&(&*value).into()).clone();
    let mut scope_command_changes: Vec<scope::Change> = Vec::new();
    for a in &actions {
        match a {
            action::Scope_command(command) if scope_active => {
                pxui_graph::scope_commands::run_command(&mut scope_view, command.clone(), &mut scope_command_changes);
            }
            action::Open_camera => scope::clear_selection(&mut scope_view),
            _ => {}
        }
    }
    let (menu, command_changes) = node_menu_step(&(&*value).into(), &workspace, frame_, &actions, graph_shown);
    let shown_node = match pane_level(&(&*value).into()) {
        Level::Inside(id) => match value.viewed.get(&id) {
            Some((_, d)) => Some(*d),
            None => displayed(&(&*value).into()),
        },
        Level::Scene => displayed(&(&*value).into()),
    };
    let (row_sets, rows_, columns, host_rows) = list_rows(&(&*value).into(), listing_, list_drawn, &pane_of, &list_hosts);
    let selected = || -> Vec<i64> {
        let mut ids = selection::selected_nodes(&selection).to_vec();
        let primary = selection::selected(&selection);
        ids.sort_by(|a, b| {
            if Some(*a) == primary {
                std::cmp::Ordering::Less
            } else if Some(*b) == primary {
                std::cmp::Ordering::Greater
            } else {
                a.cmp(b)
            }
        });
        ids
    };
    let mut tree_value = tree_value;
    let mut list_intents: Vec<tree::Intent> = Vec::new();
    for a in &actions {
        match a {
            action::List_command(tree::Command::Activate_row) => {}
            action::List_command(command) if graph_shown => {
                tree::run_command(&mut tree_value, &rows_, &selected(), command.clone(), &mut list_intents);
            }
            _ => {}
        }
    }
    let open_graph = if !graph_shown {
        None
    } else {
        actions.iter().find_map(|a| match a {
            action::List_command(tree::Command::Activate_row) if listing_ => match tree::focused(&tree_value) {
                Some(id) => Some(id),
                None => selection::selected(&selection),
            },
            _ => None,
        })
    };
    let initial_frame_request = if !actions.contains(&action::Frame_camera) { None } else { shown_node };
    let (carry_lit, carry_hot) = carry_marks(&*value);
    let unchanged = value.doc.settings.clone();
    // Presets: / s names and saves the document, / b browses, loads (Enter), and deletes (Delete
    // twice). A load replaces the document below as one undo entry.
    let mut initial_prompt = value.prompt.clone();
    for a in &actions {
        match a {
            action::Save_preset => initial_prompt = Some(prompt::Saving(editor_document::preset::default_name((value.clock)()))),
            action::Browse_presets => initial_prompt = browse(&*value, ""),
            action::Guide_keys => initial_prompt = Some(prompt::Keys),
            action::Command_palette => initial_prompt = Some(prompt::Palette(String::new())),
            action::Jump => initial_prompt = Some(prompt::Jumping(String::new())),
            action::Goto_symbol if text_shown.is_some() => initial_prompt = Some(prompt::Symbols(String::new())),
            action::Goto_line if text_shown.is_some() => initial_prompt = Some(prompt::Going(String::new())),
            action::Panel_pick => initial_prompt = Some(prompt::Picking { split: true, at: frame_.input.pointer }),
            action::Window_pick => initial_prompt = Some(prompt::Picking { split: false, at: frame_.input.pointer }),
            _ => {}
        }
    }
    let leader_panel = match &leader {
        router::State::Pending(prefix) => Some(prefix.clone()),
        router::State::Idle => None,
    };

    let mut built: Option<frame_result> = None;
    {
        let mut build_and_prompt = |ui: &mut Ui| {
            let mut vw = vw;
            if let Some(prefix) = &leader_panel {
                let category = |c: &Command| editor_commands::group(c).to_string();
                let describe = |p: &str| editor_commands::describe_prefix(p);
                let order = |sec: &str| editor_commands::order(sec);
                which_key::panel(ui, Some(&category), Some(&describe), Some(&order), &keymap, prefix, &focus, editor_commands::pane_name(&focus));
            }
            if !all_ui_visible {
                return;
            }
            let (prompt_next, prompt_intent_, prompt_error) = match prompt_panel(&(&*value).into(), &focus, &keymap, frame_, &workspace, &text, text_shown.as_ref(), ui, initial_prompt.clone()) {
                Ok((prompt, intent)) => (prompt, intent, None),
                Err(error) => (initial_prompt.clone(), None, Some(rays_diagnostic::to_string(&error))),
            };
            // ---- build ----
            // Chrome first: panel backgrounds, splitters and headers.  Its intents fold into the
            // shell now (a fold, a drag) or become one editor-graph edit after the frame.
            let header_focus: Option<layout::Path> = match &focus_path {
                Some(_) => focus_path.clone(),
                None => panels::leaves(&shell_tree(&*value, &workspace)).into_iter().find_map(|(path, p)| if p == focus { Some(path) } else { None }),
            };
            // the tabs standing at a header's end, outermost first: a graph panel's Graph / List /
            // Text, then a text view's Selection / Graph / Document.  `Chrome.header_slots` places
            // them and cuts the title, so a narrow header drops the text tabs, then the views, and
            // never draws one over another
            let geo0 = geometry(&(&*value).into(), &workspace, frame_);
            let mut group_table: HashMap<layout::Path, Vec<f64>> = HashMap::new();
            let mut tools_table: HashMap<layout::Path, f64> = HashMap::new();
            let platform_ui = ui.platform();
            let _ = platform_ui;
            for leaf in &geo0.leaves {
                let text_ = match (&text_host, &text_shown) {
                    (Some((_, path, _, _)), Some(shown)) if *path == leaf.path => Some((text.clone(), shown.clone())),
                    _ => other_texts.iter().find_map(|(_, path, state, shown)| if *path == leaf.path { Some((state.clone(), shown.clone())) } else { None }),
                };
                let mut widths = Vec::new();
                if leaf.panel == Panel::Graph {
                    widths.push(ui.text_width(None, "GraphListText") + 24.0);
                }
                if let Some((state, shown)) = text_ {
                    widths.push(text_pane::tabs_width(ui, leaf.body.2, &state, &shown));
                }
                group_table.insert(leaf.path.clone(), widths);
                tools_table.insert(leaf.path.clone(), chrome::tools_start(ui, header_focus.as_ref() == Some(&leaf.path), leaf.floating, false, &panel_title(&vw, leaf)));
            }
            let header_groups = |leaf: &Leaf| -> Vec<f64> { group_table.get(&leaf.path).cloned().unwrap_or_default() };
            let header_slots = |leaf: &Leaf| -> Vec<Option<f64>> { chrome::header_slots(leaf, &header_groups(leaf)).0 };
            let tools_from = |leaf: &Leaf| -> f64 { tools_table.get(&leaf.path).copied().unwrap_or(0.0) };
            let key_of = |id: &str| -> String {
                match keymap.iter().find(|c| c.id == id) {
                    Some(Command { trigger: Some(trigger), .. }) => keymap::label(platform, trigger),
                    _ => String::new(),
                }
            };
            let state_fn = |path: &[usize]| panel_state(&(&*value).into(), path);
            let title_fn = |leaf: &Leaf| panel_title(&vw, leaf);
            let mut intents = chrome::update(Some(&state_fn), Some(&workspace.hidden), Some(&title_fn), Some(&header_groups), Some(&key_of), header_focus.as_ref(), &shell_tree(&*value, &workspace), ui, &shortcut_frame);
            let leaves = panels::leaves(&shell_tree(&*value, &workspace));
            intents.extend(panel_intents(&(&*value).into(), &leaves, &workspace, frame_, &focus, focus_path.as_deref(), &actions));
            let layout_leaf = focused_leaf(&geo0, &focus, focus_path.as_deref());
            let (workspace, mut layout_changes) = layout_intents(&vw, &workspace, &intents);
            layout_changes.extend(layout_actions(&(&*value).into(), &workspace, layout_leaf.as_ref(), &actions));
            vw.workspace = &workspace;
            let g = geometry(&(&*value).into(), &workspace, frame_);
            // the graph panel's toolbar; a click is one command or edit
            let graph_host = graph_leaf(&vw, &g);
            let (tool_action, view_action, view_pick) = draw_graph_header(ui, &vw, &g, &graph_host, &header_slots, &tools_from);
            let mut roots: Vec<(Leaf, UiBox)> = Vec::new();
            for (order, leaf) in g.leaves.iter().enumerate() {
                let boxed = chrome::pane_root(ui, frame_, leaf.body, &format!("workspace-pane-{}{}", editor_commands::pane_name(&leaf.panel), chrome::key(&leaf.path)));
                if leaf.floating {
                    ui.to_front(order as i64, &boxed);
                }
                roots.push((leaf.clone(), boxed));
            }
            let viewport_drops: Vec<(carry_mod::Place, bool)> = if !carrying {
                Vec::new()
            } else {
                roots
                    .iter()
                    .filter_map(|(leaf, boxed)| match &leaf.panel {
                        Panel::View(key) => ui.drop_target(boxed).map(|d| (carry_mod::Place::Viewport(key.clone()), matches!(d, pxui::ui::Drop::Dropped(_)))),
                        _ => None,
                    })
                    .collect()
            };
            let timeline_root: Option<(Panel, UiBox)> = if layout::find(&g, &Panel::Timeline).is_none() {
                Some((Panel::Timeline, chrome::pane_root(ui, frame_, g.timeline_at, "workspace-pane-Timeline")))
            } else {
                None
            };
            // every leaf draws in its own root: its PXUI keys are seeded by it
            let root_at = |path: &layout::Path| -> Option<(Leaf, UiBox)> { roots.iter().find(|(l, _)| l.path == *path && l.body.3 > 0).cloned() };
            let of_kind = |panel: &Panel| -> Vec<(Leaf, UiBox)> { roots.iter().filter(|(l, _)| l.panel == *panel && visible(&workspace, panel) && l.body.3 > 0).cloned().collect() };
            let none_root = |ui: &mut Ui| chrome::pane_root(ui, frame_, (0, 0, 0, 0), "workspace-pane-none");
            let graph_root = match graph_host.as_ref().and_then(|l| root_at(&l.path)) {
                Some((_, b)) => b,
                None => none_root(ui),
            };
            let active = active_view(&vw, &g);
            let view_root = draw_viewport_pane(ui, &(&*value).into(), probe_caption(&vw), &roots, active.as_ref(), &none_root);
            let mut scope_view2 = scope_view.clone();
            let mut scope_changes = scope_command_changes.clone();
            let (graph_body, menu2, menu_pick) = draw_graph_pane(ui, frame_, &(&*value).into(), &shortcut_frame, &mut scope_view2, scope_active, guide, carry_lit.clone(), carry_hot.clone(), &graph_root, &graph_host, &workspace, menu.clone(), &open_menu, &mut scope_changes);
            // Each pane builder returns only its frame intents and updated pane-local state.
            let (tree2, list_intents2, locals) = draw_list_panes(ui, &shortcut_frame, &(&*value).into(), &list_hosts, &root_at, &pane_of, &in_use, &|pane| level_name(pane), &rows_, &columns, &host_rows, &selected, tree_value.clone(), list_intents.clone(), value.locals.clone());
            let (text2, text_intents, locals, other_texts2) = draw_text_panes(ui, frame_, &*value, &root_at, &header_slots, &text_host, &text_shown, text.clone(), other_texts.clone(), locals);
            let (outline, outline_intents, locals) = draw_outline_panes(ui, &*value, &hosted(Host_kind::Outline), &root_at, &|pane| navigator_params(&pane.into()), value.outline.clone(), locals);
            let locals = draw_spreadsheet_panes(ui, &(&*value).into(), &of_kind(&Panel::Spreadsheet), locals);
            let graph_panes = draw_other_graph_panes(ui, &shortcut_frame, &(&*value).into(), &other_panes, &roots);
            let _ = text2;
            let mut changes = pane_changes(&(&*value).into(), &vw, command_changes.clone(), &scope_changes, &menu_pick, &outline_intents);
            // a layout row and the add button are commands: they run like a toolbar click
            let mut outline_action: Option<action> = None;
            for i in &outline_intents {
                match i {
                    navigator::Intent::Layout(index) => outline_action = Some(action::Layout_switch(*index)),
                    navigator::Intent::Add => outline_action = Some(action::Add_node),
                    _ => {}
                }
            }
            // a click on an earlier part of the graph header's route goes back that far
            let crumb_action = intents.iter().find_map(|i| match i {
                chrome::Intent::Crumb(path, index) if !pane_route(&(&*value).into()).is_empty() && leaves.iter().any(|(p, panel)| p == path && *panel == Panel::Graph) => {
                    let steps = (pane_route(&(&*value).into()).len() as i64 + 1).min(3) - 1 - index;
                    if steps > 0 { Some(action::Back(steps)) } else { None }
                }
                _ => None,
            });
            // the latest of the four is the command, as when each overwrote the one before
            let bar_action = [tool_action, view_action, outline_action, crumb_action].into_iter().fold(None, |latest, a| if a.is_none() { latest } else { a });
            // Selection is view state; inspection intents retain the selected stable id. Topology and
            // parameter application wait until Ui.frame finishes.
            let mut selection = selection.clone();
            for i in &list_intents2 {
                match i {
                    tree::Intent::Select(ids) if ids.is_empty() => selection = selection::clear(),
                    tree::Intent::Select(ids) => selection = selection::select_nodes(ids),
                    _ => {}
                }
            }
            let inspector_visible = has_panel(&vw, &Panel::Inspector);
            let selected_ids = selection::selected_nodes(&selection).to_vec();
            let selected_id = if selected_ids.len() == 1 { selection::selected(&selection) } else { None };
            let selected_obj: Option<picked> = selected_id.and_then(|id| kg::find(&level_objects(&(&*value).into()), id).map(|n| of_object(&n)));
            let lowered_level = match pane_level(&(&*value).into()) {
                Level::Inside(id) => kind(&*value, id).as_deref() == Some("geometry"),
                Level::Scene => false,
            };
            let (panel, inspector_changes, settings_changes, workspace_moves, inspector_drops, live_cook) = draw_inspectors(
                &(&*value).into(),
                &workspace,
                image,
                camera_panel,
                inspector_visible,
                &unchanged,
                &g,
                &scope_view2,
                &selection,
                scope_active,
                graph_shown,
                &focus,
                &focus_path,
                &pane_of,
                &in_use,
                &of_kind(&Panel::Inspector),
                ui,
            );
            changes.extend(inspector_changes);
            changes.extend(layout_changes);
            scope_changes.extend(workspace_moves);
            // every timeline leaf, else the strip under the tree
            let timeline_leaves = of_kind(&Panel::Timeline);
            let timeline_slots: Vec<(layout::Bounds, UiBox, bool)> = if timeline_leaves.is_empty() {
                timeline_root.iter().map(|(_, b)| (g.timeline_at, b.clone(), true)).collect()
            } else {
                timeline_leaves.iter().map(|(l, b)| (l.body, b.clone(), false)).collect()
            };
            let (timeline_intents, strip_layout) = draw_timeline_pane(ui, &value.timeline, value.timeline_frames, &timeline_slots, &timeline_root, g.timeline_at);
            let (selection, grab, handle_ops, handle_changes) = draw_handles(&(&*value).into(), &workspace, selection, &selected_obj, lowered_level, scope_active, &active, &view_root, view_handles, ui);
            changes.extend(handle_ops);
            let context = match &leader {
                router::State::Pending(_) => guide_context::GuideContext::Leader,
                router::State::Idle if value.prompt.is_some() || menu2.is_some() || scope::editing(&scope_view2) => guide_context::GuideContext::Search,
                router::State::Idle if listing_ => guide_context::GuideContext::List,
                router::State::Idle => {
                    let count = if scope_active { scope::selected(&scope_view2).len() } else { selection::selected_nodes(&selection).len() };
                    match count {
                        0 => guide_context::GuideContext::Canvas,
                        1 => guide_context::GuideContext::Node,
                        _ => guide_context::GuideContext::Multi,
                    }
                }
            };
            {
                vw.status_fps = status_fps;
                vw.guide = guide;
                vw.leader = &leader;
                vw.selection = &selection;
                status_box(&vw, ui, frame_, render_status.as_deref(), text_error.as_deref().or(prompt_error.as_deref()).or(error_status.as_deref()), context);
            }
            draw_notice(&(&*value).into(), frame_, &roots, &header_focus, graph_body, &scope_view2, scope_active, ui);
            let (focus2, focus_path2, pane_keys) = follow_press(&(&*value).into(), &workspace, frame_, &roots, &timeline_root, focus.clone(), focus_path.clone(), ui);
            let (workspace, drag_changes) = settle_layout(&(&*value).into(), &vw, workspace.clone(), frame_, &keymap, &shortcut_frame, &g, &intents, strip_layout, &timeline_leaves, platform, ui);
            changes.extend(drag_changes);
            let mut drops = inspector_drops;
            drops.extend(viewport_drops);
            let result = frame_result {
                workspace,
                focus: focus2,
                focus_path: focus_path2,
                pane_keys,
                outline,
                outline_intents,
                selection,
                menu: menu2,
                menu_pick,
                scope_view: scope_view2,
                scope_changes,
                tree: tree2,
                document: level_objects(&(&*value).into()),
                edit_error: text_error.clone().or(prompt_error.clone()).or_else(|| value.edit_error.clone()),
                effects: Effects::default(),
                timeline_intents,
                frame_request: initial_frame_request,
                prompt: None,
                prompt_intent: None,
                panel,
                grab,
                settings: unchanged.clone(),
                opened: None,
                live_cook,
                label: "Edit".to_string(),
                changes,
                tree_intents: list_intents2,
                text_intents,
                open_graph,
                settings_changes,
                graph_panes,
                locals,
                other_texts: other_texts2,
                handle_changes,
                bar_action: bar_action.clone(),
                view_pick,
                drops,
            };
            let prompt_intent_ = match (prompt_intent_, &result.bar_action) {
                (None, Some(a)) => Some(prompt_intent::Run_action(a.clone())),
                (intent, _) => intent,
            };
            built = Some(frame_result { prompt: prompt_next, prompt_intent: prompt_intent_, ..result });
        };
        let ui_handle = value.ui.clone();
        ui_handle.borrow_mut().frame(frame_, &mut build_and_prompt);
    }
    let result = match built {
        Some(result) => result,
        None => frame_result {
            workspace: workspace.clone(),
            focus: focus.clone(),
            focus_path: focus_path.clone(),
            pane_keys: Vec::new(),
            graph_panes: panel_map::others(&value.panes),
            locals: value.locals.clone(),
            other_texts: Vec::new(),
            outline: value.outline.clone(),
            outline_intents: Vec::new(),
            selection: selection.clone(),
            menu: menu.clone(),
            menu_pick: None,
            scope_view: scope_view.clone(),
            scope_changes: scope_command_changes.clone(),
            tree: tree_value.clone(),
            document: level_objects(&(&*value).into()),
            edit_error: text_error.clone().or_else(|| value.edit_error.clone()),
            effects: Effects::default(),
            timeline_intents: Vec::new(),
            frame_request: initial_frame_request,
            prompt: initial_prompt.clone(),
            prompt_intent: None,
            panel: None,
            grab: false,
            settings: value.doc.settings.clone(),
            opened: None,
            live_cook: value.live_cook,
            label: "Edit".to_string(),
            changes: command_changes.clone(),
            tree_intents: list_intents.clone(),
            text_intents: Vec::new(),
            open_graph,
            settings_changes: Vec::new(),
            handle_changes: None,
            bar_action: None,
            view_pick: None,
            drops: Vec::new(),
        },
    };
    let timeline_for_reduce = value.timeline.clone();
    reduce(
        gpu,
        pump,
        Frame_input {
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
            timeline: timeline_for_reduce,
            timeline_changes,
            graph_shown,
            text,
            text_shown,
            row_sets,
            rows: rows_,
        },
        value,
        result,
    )
}

/// One frame of the editor. A carry takes its turn first (its keys, the ends of the gesture, the
/// preview document the panels then read) and the frame the rest of the editor sees has the keys it
/// took removed.
#[allow(clippy::too_many_arguments)]
pub fn update(
    value: &mut EditorModel,
    all_ui_visible: bool,
    text_focus: bool,
    camera_panel: &dyn Fn(&mut Ui) -> Panel_result,
    view_handles: &ViewHandles,
    render_status: Option<String>,
    error_status: Option<String>,
    view_state: Rc<dyn Fn(Option<&Panel_result>) -> Syntax>,
    frame_: &frame::Frame,
    gpu: Option<&flow_ir::flow_ir::gpu::Config>,
    image: Option<&dyn Fn(i64) -> Option<rays::image::Image>>,
    host_events: Vec<Event>,
    pump: Option<&dyn Fn()>,
) -> update {
    let clock = value.clock.clone();
    let guard = flow_syntax::phase_timer::SampleGuard::start_with_clock(Box::new(move || clock()));
    let before = value.doc.clone();
    let (frame_after, carry_changed) = carry_step(value, text_focus, frame_);
    let no_image = |_: i64| None;
    let image: &dyn Fn(i64) -> Option<rays::image::Image> = match image {
        Some(i) => i,
        None => &no_image,
    };
    let out = update_frame(value, carry_changed, all_ui_visible, text_focus, camera_panel, view_handles, render_status, error_status, view_state, &frame_after, gpu, pump, image, host_events);
    let phases = guard.finish();
    if !std::sync::Arc::ptr_eq(&value.doc, &before) {
        value.edit_phases = phases;
    }
    out
}
