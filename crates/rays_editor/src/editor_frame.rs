//! One frame of the editor: keys, panes, prompts, then the reducer.
//!
//! Conventions: the model `ui` is behind a `RefCell`, so nothing called inside `Ui::frame` touches
//! `value.ui` (the platform is read before the frame); optional arguments (`?gpu ?pump ?image
//! ?host_events`) are the trailing parameters of `update`.

use flow_syntax::symbol::Symbol;
use std::borrow::Cow;
use std::cell::Cell;
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
use flow_syntax::syntax::{Node as SNode, Syntax};
use input::{Event, Key, MouseButton};
use param::{Effects, FieldView, Value};
use crate::p32;
use pxui::ui::{Size, Ui};
use rays_graph::node_menu;
use rays_graph::scope_state as scope;
use rays_shell::chrome;
use rays_shell::layout::{self, Leaf};
use rays_shell::prompt as shell_prompt;
use rays_shell::status_bar::{self, TipKind};
use rays_shell::which_key;
use rays_shell::tree;
use rays::frame;

use crate::carry as carry_mod;
use crate::editor_actions::{apply_action, routed};
use crate::editor_carry::{carry_allowed, carry_place, carry_step};
use crate::editor_commands::{self, Action, action_of, Command};
use crate::editor_inspect::{displayed, kind, new_graph, objects as level_objects};
use crate::editor_list::{rows, space, sync_list_selection};
use crate::editor_menu::open_menu;
use crate::editor_model::*;
use crate::editor_panels::{copy_bindings, copy_workspace, defn_change, focused_leaf, graph_leaf, layout_actions, layout_intents_cow, paste_bindings, write_panel_title};
use crate::editor_panes::*;
use crate::editor_reduce::{reduce, FrameInput};
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
pub type ViewHandles = dyn Fn(&mut Ui, Option<&Picked>, rays_math::mat4::T, layout::Bounds) -> (Vec<(String, Value)>, bool, Option<i64>);

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

type Prompted = (Option<Prompt>, Option<PromptIntent>);

/// A search prompt over `all` rows of (name, detail, intent): fuzzy-filtered, a pick is its
/// intent, any other query change keeps the prompt open as `again query`.
#[allow(clippy::too_many_arguments)]
fn search_prompt(ui: &mut Ui, key: &str, title: &str, label: &str, query: &str, all: &[(impl AsRef<str>, impl AsRef<str>, PromptIntent)], again: &dyn Fn(String) -> Prompt) -> Prompted {
    let matches = |query: &str, name: &str| query.is_empty() || pxui::ui::fuzzy_match(query, name);
    let mut fill = |query: &str, rows: &mut pxui::PickRows| {
        let mut count = 0;
        for (name, detail, _) in all.iter().filter(|(name, _, _)| matches(query, name.as_ref())) {
            rows.push(pxui::PickRow::new(name.as_ref()).detail(detail.as_ref()));
            count += 1;
        }
        count
    };
    match shell_prompt::search(ui, key, title, label, query, all.len(), &mut fill) {
        None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
        Some((query, pxui::ui::Pick::Pick(index))) => {
            let found = all.iter().filter(|(name, _, _)| matches(&query, name.as_ref())).nth(index as usize);
            (None, found.map(|(_, _, intent)| intent.clone()))
        }
        Some((query, _)) => (Some(again(query)), None),
    }
}

/// The open prompt of a frame: draws it and returns (next prompt, intent).
#[allow(clippy::too_many_arguments)]
pub fn prompt_panel<'a>(value: &ModelView<'a>, focus: &Panel, keymap: &[Command], frame_: &frame::Frame, workspace: &Shell, text: &text_pane::State, text_shown: Option<&text_pane::Shown>, ui: &mut Ui, prompt_: Option<Prompt>) -> Result<Prompted, rays_diagnostic::Diagnostic> {
    let next: Prompted = match prompt_.clone() {
        None => (None, None),
        Some(Prompt::Keys) => {
            let commands: Vec<Command> = editor_commands::keymap().iter().filter(|c| !matches!(action_of(c), Action::ListCommand(_) | Action::FrameTile)).cloned().collect();
            let context = format!("{} focused", editor_commands::pane_name(focus).to_lowercase());
            let text_keys: Vec<(String, Trigger)> = editor_commands::text_keys().into_iter().map(|(l, t)| (l.to_string(), t)).collect();
            let category = |c: &Command| editor_commands::sheet_group(c).to_string();
            if which_key::sheet(ui, &context, Some(&category), &text_keys, &commands) { (Some(Prompt::Keys), None) } else { (None, None) }
        }
        Some(Prompt::Confirming { title, message, accept, cancel, action, accepting }) => {
            let (accepting, answer) = shell_prompt::confirm(ui, "confirm-action", title, &message, accept, cancel, accepting);
            match answer {
                pxui::ui::Pick::Cancel => (None, None),
                pxui::ui::Pick::Submit => (None, Some(PromptIntent::RunAction(action))),
                _ => (Some(Prompt::Confirming { title, message, accept, cancel, action, accepting }), None),
            }
        }
        Some(Prompt::Saving(name)) => match shell_prompt::name(ui, "preset-save", "Save preset", "Name for the current state of the document", "Preset name", &name) {
            None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
            Some((name, pxui::ui::Pick::Submit)) => (None, Some(PromptIntent::SavePresetFile(name))),
            Some((name, _)) => (Some(Prompt::Saving(name)), None),
        },
        Some(Prompt::MakingMacro { nodes, draft, state }) => {
            let literals: Vec<String> = draft.literals.iter().map(|(_, e)| lisp::flat(e)).collect();
            match shell_prompt::macro_(ui, "make-macro", "Make a macro from the selection", &literals, &draft.free, &state) {
                None => (None, None),
                Some((state, shell_prompt::MacroResult::Submit)) => (None, Some(PromptIntent::EditSource(fe::macro_op(&draft, &nodes, &state.name, &state.holes)))),
                Some((state, shell_prompt::MacroResult::None)) => (Some(Prompt::MakingMacro { nodes, draft, state }), None),
            }
        }
        Some(Prompt::Browsing { query, presets, last_state }) => {
            let entries = |query: &str| -> Vec<(String, f64, bool)> {
                let mut all: Vec<(String, f64, bool)> = last_state.iter().map(|time| ("Last edited state".to_string(), *time, true)).collect();
                all.extend(presets.iter().map(|(name, time)| (name.clone(), *time, false)));
                all.into_iter().filter(|(name, _, _)| pxui::ui::fuzzy_match(query, name)).collect()
            };
            let mut fill = |query: &str, rows: &mut pxui::PickRows| {
                let entries = entries(query);
                for (name, time, recovery) in &entries {
                    let (mon, day, hour, minute) = month_day_hour_minute(*time);
                    let detail = format!("{}{:02}-{:02} {:02}:{:02}", if *recovery { "Autosave \u{b7} " } else { "" }, mon, day, hour, minute);
                    rows.push(pxui::PickRow::new(name).detail(&detail));
                }
                entries.len()
            };
            let total = presets.len() + usize::from(last_state.is_some());
            let title = format!("Presets \u{b7} {total}");
            let same = |query: String| Prompt::Browsing { query, presets: presets.clone(), last_state };
            match shell_prompt::search(ui, "preset-browse", &title, "Search presets", &query, total, &mut fill) {
                None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
                Some((query, pxui::ui::Pick::Pick(index))) => match entries(&query).get(index as usize) {
                    Some((name, _, recovery)) => (None, Some(if *recovery { PromptIntent::LoadLastState } else { PromptIntent::LoadPresetFile(name.clone()) })),
                    None => (None, None),
                },
                Some((query, pxui::ui::Pick::Delete(index))) => match entries(&query).get(index as usize) {
                    Some((name, _, recovery)) => {
                        let intent = if *recovery { PromptIntent::DeleteLastState(query.clone()) } else { PromptIntent::DeletePresetFile { name: name.clone(), query: query.clone() } };
                        (Some(same(query)), Some(intent))
                    }
                    None => (Some(same(query)), None),
                },
                Some((query, _)) => (Some(same(query)), None),
            }
        }
        Some(Prompt::Jumping(query)) => {
            // every graph, grouped like the outline
            let mut all: Vec<(String, String, PromptIntent)> = navigator::jump_rows(&value.doc.workspace.ws.checked).into_iter().map(|(name, group)| (name.clone(), group, PromptIntent::Go(name))).collect();
            // and every node of the graph the pane shows: picking one selects it and frames it
            fn nodes(s: &fproj::Scope, out: &mut Vec<(String, String, PromptIntent)>) {
                for n in &s.nodes {
                    if !n.synthetic {
                        out.push((fproj::title(n), format!("node \u{b7} {}", n.head), PromptIntent::FindNode(n.path.clone())));
                    }
                    if let Some(z) = &n.zone {
                        nodes(&z.scope, out);
                    }
                }
            }
            if let Some(k) = pane_laid(value) {
                nodes(&k.scope, &mut all);
            }
            search_prompt(ui, "graph-jump", "Jump to graph or node", "Search graphs and nodes", &query, &all, &Prompt::Jumping)
        }
        Some(Prompt::Symbols(query)) => {
            // every graph, function, macro and [let*] name of the text, by position
            let all: Vec<(String, String, PromptIntent)> = match text_shown {
                Some(shown) => {
                    let source = text_pane::current_text(text, shown)?;
                    text_pane::symbols(&source)
                        .into_iter()
                        .map(|(name, kind_, byte)| {
                            let line = rays_diagnostic::position_of_offset(&source, byte).line;
                            (name, format!("{kind_} \u{b7} line {line}"), PromptIntent::RunAction(Action::TextCommand(text_pane::Command::Select(byte, byte))))
                        })
                        .collect()
                }
                None => Vec::new(),
            };
            search_prompt(ui, "symbol-jump", "Go to symbol", "Search bindings", &query, &all, &Prompt::Symbols)
        }
        Some(Prompt::Referencing(name, query)) => {
            let all: Vec<(String, String, PromptIntent)> = match text_shown {
                Some(shown) => {
                    let source = text_pane::current_text(text, shown)?;
                    text_pane::references(&source, &name)
                        .into_iter()
                        .map(|(byte, line)| {
                            let n = rays_diagnostic::position_of_offset(&source, byte).line;
                            (line, format!("line {n}"), PromptIntent::RunAction(Action::TextCommand(text_pane::Command::Select(byte, byte))))
                        })
                        .collect()
                }
                None => Vec::new(),
            };
            let held = name.clone();
            search_prompt(ui, "references", &format!("References to {name}"), "Search uses", &query, &all, &move |q| Prompt::Referencing(held.clone(), q))
        }
        Some(Prompt::Going(query)) => match shell_prompt::name(ui, "goto-line", "Go to line", "A line, or line:column", "Line", &query) {
            None | Some((_, pxui::ui::Pick::Cancel)) => (None, None),
            Some((query, pxui::ui::Pick::Submit)) => {
                let target = match text_shown {
                    Some(shown) => text_pane::line_target(&text_pane::current_text(text, shown)?, &query),
                    None => None,
                };
                match target {
                    Some(byte) => (None, Some(PromptIntent::RunAction(Action::TextCommand(text_pane::Command::Select(byte, byte))))),
                    None => (Some(Prompt::Going(query)), None),
                }
            }
            Some((query, _)) => (Some(Prompt::Going(query)), None),
        },
        Some(open_prompt @ Prompt::Picking { .. }) => {
            let Prompt::Picking { split, at } = open_prompt.clone() else { unreachable!() };
            let kinds: Vec<_> = panels::PANELS.iter().map(|entry| entry.panel.clone()).collect();
            let digit = frame_.input.events.iter().find_map(|e| match e {
                Event::KeyPressed(Key::KeyChar(c)) if *c >= '1' && (*c as u32) < ('1' as u32 + kinds.len() as u32) => Some((*c as u32 - '1' as u32) as usize),
                _ => None,
            });
            let pick = |i: usize| -> Prompted {
                let panel = kinds[i].clone();
                if split { (Some(Prompt::Splitting { panel, axis: Axis::H }), None) } else { (None, Some(PromptIntent::RunAction(Action::WindowNew(panel)))) }
            };
            match digit {
                Some(i) => pick(i),
                None => {
                    let keys: Vec<String> = (0..kinds.len()).map(|i| (i + 1).to_string()).collect();
                    let items: Vec<(String, bool)> = kinds.iter().map(|p| (panels::name(p).to_string(), true)).collect();
                    let rows: Vec<_> = items.iter().zip(&keys).map(|((label, enabled), shortcut)|
                        pxui::MenuItem::new(label).disabled(!enabled).shortcut(shortcut)).collect();
                    let got = ui.menu(if split { "split-kind" } else { "window-kind" },
                        pxui::Point::new(at.0 as f32, at.1 as f32), &rows, &pxui::Menu::DEFAULT.w_px(200.0));
                    match (got.chosen, got.dismissed) {
                        (None, false) => (Some(open_prompt), None),
                        (None, true) => (None, None),
                        (Some(i), _) => pick(i),
                    }
                }
            }
        }
        Some(Prompt::Splitting { panel, axis }) => {
            // the split follows the pointer over the panel under it; Tab turns it, a click places it
            let pressed = |key: Key| frame_.input.events.iter().any(|e| matches!(e, Event::KeyPressed(k) if *k == key));
            let axis = if pressed(Key::Tab) { if axis == Axis::H { Axis::V } else { Axis::H } } else { axis };
            let (mx, my) = frame_.input.pointer;
            let leaf = geometry(value, workspace, frame_).leaves.iter().find(|l| {
                let (x, y, w, h) = l.frame;
                !l.floating && pxui::ui::Rect::new(x as f32, y as f32, w as f32, h as f32).contains(p32((mx, my)))
            }).cloned();
            let ratio = match &leaf {
                Some(l) => {
                    let (x, y, w, h) = l.frame;
                    let r = if axis == Axis::H { (mx - x as f64) / (w.max(1) as f64) } else { (my - y as f64) / (h.max(1) as f64) };
                    r.min(0.9).max(0.1)
                }
                None => 0.5,
            };
            let b = ui.node("split-pending", pxui::Node::DEFAULT.flags(pxui::Flags::CLICKABLE).w(Size::Px((frame_.input.size.0 as f64) as f32)).h(Size::Px((frame_.input.size.1 as f64) as f32)).at(0.0, 0.0));
            ui.to_front(900, &b);
            let theme = ui.tokens();
            let painted = leaf.clone();
            let name = panels::name(&panel).to_uppercase();
            ui.canvas_over(&b, move |paint, _| {
                let Some(l) = &painted else { return };
                let (x, y, w, h) = l.frame;
                let (x, y, w, h) = (x as f64, y as f64, w as f64, h as f64);
                let cut = if axis == Axis::H { x + w * ratio } else { y + h * ratio };
                let (nx, ny, nw, nh) = if axis == Axis::H { (cut, y, x + w - cut, h) } else { (x, cut, w, y + h - cut) };
                // the panel being split is tinted, the room of the new one more
                let tint = |alpha: i64| theme.accent.with_alpha(alpha as u8);
                paint.fill(pxui::Rect::new(x as f32, y as f32, w as f32, h as f32), pxui::Fill::new(tint(22)));
                paint.fill(pxui::Rect::new(nx as f32, ny as f32, nw as f32, nh as f32), pxui::Fill::new(tint(56)));
                // the new panel's room: accent brackets 6 inside its corners; the panel kept stays as it is
                paint.brackets(pxui::Rect::new((nx + 6.0) as f32, (ny + 6.0) as f32, (nw - 12.0) as f32, (nh - 12.0) as f32), 0.0, 12.0, pxui::Stroke::new(theme.accent).width(1.0));
                // the cut: a hairline through the panel with a short accent cap at each end
                let (a, b2, (ux, uy)) = if axis == Axis::H { ((cut, y), (cut, y + h), (0.0, 1.0)) } else { ((x, cut), (x + w, cut), (1.0, 0.0)) };
                paint.line(p32(a), p32(b2), pxui::Stroke::new(theme.accent).width(1.0));
                for (px, py, sgn) in [(a.0, a.1, 1.0), (b2.0, b2.1, -1.0)] {
                    let (px, py) = (px + sgn * ux, py + sgn * uy);
                    paint.line(((px - 5.0 * uy) as f32, (py - 5.0 * ux) as f32), ((px + 5.0 * uy) as f32, (py + 5.0 * ux) as f32), pxui::Stroke::new(theme.accent).width(3.0));
                }
                // what is being made, in the new panel's room, and the keys that steer it
                let (cx, cy) = (nx + nw / 2.0, ny + nh / 2.0);
                if nw > 90.0 && nh > 40.0 {
                    let cw = f64::from({ let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(&name, style, None) }.unwrap_or(0.0));
                    { let text = &name; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.accent); let _ = paint.text_spans(pxui::Point::new((cx - cw / 2.0) as f32, (cy - 14.0) as f32), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), 0.0, text, &[span]); };
                    let hint = "TAB  TURN   ESC  CANCEL";
                    let hw = f64::from({ let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(hint, style, None) }.unwrap_or(0.0));
                    { let text = hint; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.521)); let _ = paint.text_spans(pxui::Point::new((cx - hw / 2.0) as f32, (cy + 4.0) as f32), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), 0.0, text, &[span]); };
                }
                let first = (ratio * 100.0) as i64;
                let label = format!("{first} / {}", 100 - first);
                let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).tracking(0.08);
                paint.text_chars(pxui::Point::new((mx + 12.0) as f32, (my + 12.0) as f32), style, &label);
            });
            let signal = ui.signal(&b);
            let cancel = pressed(Key::Escape) || frame_.input.events.iter().any(|e| matches!(e, Event::PointerPressed(MouseButton::RightButton, _)));
            if cancel {
                (None, None)
            } else {
                match (signal.clicked, &leaf) {
                    (true, Some(l)) => (None, Some(PromptIntent::RunAction(Action::PanelSplitAt(l.path.clone(), axis, ratio, panel)))),
                    _ => (Some(Prompt::Splitting { panel, axis }), None),
                }
            }
        }
        Some(Prompt::Palette(query)) => {
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
            let all: Vec<_> = commands.iter().map(|c| (c.label.as_str(), "", PromptIntent::RunAction(action_of(c).clone()))).collect();
            search_prompt(ui, "command-palette", "Commands", "Search commands", &query, &all, &Prompt::Palette)
        }
    };
    // A closed prompt must not keep keyboard focus into the next frame.
    if next.0.is_none() && prompt_.is_some() {
        ui.dismiss_popup();
    }
    Ok(next)
}

/// Apply the grip, drop and timeline intents after drawing the panes.
#[allow(clippy::too_many_arguments)]
pub fn settle_layout<'a>(value: &ModelView<'a>, vw: &ModelView<'a>, workspace: &'a Shell, keymap: &[Command], g: &layout::Geometry, intents: &[chrome::Intent], mut grips: Vec<chrome::Intent>, strip_layout: Vec<chrome::Intent>, timeline_leaves: &[&(Leaf, pxui::Handle)], platform: input::Platform, ui: &mut Ui) -> (Cow<'a, Shell>, Vec<Change>) {
    let state = |path: &[usize]| panel_state(value, path);
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
    for &(leaf, boxed) in timeline_leaves {
        if leaf.panel != Panel::Timeline || leaf.body == (0, 0, 0, 0) {
            continue;
        }
        ui.within(boxed, &mut |ui| {
            let at = ui.context_at(boxed).map(|p| (p.x, p.y));
            timeline_menus.extend(chrome::panel_menu(Some(&state), Some(&key_of), ui, leaf, boxed, at));
        });
    }
    let window_dragged = intents.iter().chain(&grips).any(|i| matches!(i, chrome::Intent::WindowDrag(..)));
    grips.extend(drops);
    grips.extend(timeline_menus);
    grips.extend(strip_layout);
    let (mut workspace, drag_changes) = layout_intents_cow(vw, workspace, &grips);
    if !window_dragged && workspace.window_live.is_some() {
        workspace.to_mut().window_live = None;
    }
    (workspace, drag_changes)
}

type Target = (Panel, Option<layout::Path>);

/// The pane the last press landed in takes the focus; the panes' PXUI keys name them.
/// Idle frames reuse `prev` (Rc clone) when box keys hold and nothing pressed — no Path clones.
pub fn follow_press(
    g: &layout::Geometry,
    roots: &[(Leaf, pxui::Handle)],
    timeline_root: &Option<(Panel, pxui::Handle)>,
    focus: Panel,
    focus_path: Option<layout::Path>,
    prev: &std::rc::Rc<Vec<(i64, Target)>>,
    ui: &mut Ui,
) -> (Panel, Option<layout::Path>, std::rc::Rc<Vec<(i64, Target)>>) {
    let n = roots.len() + usize::from(timeline_root.is_some());
    let mut latest = -1i64;
    let mut found: Target = (focus.clone(), focus_path.clone());
    let mut can_reuse = prev.len() == n;
    let panes = roots.iter().map(|(leaf, b)| (&leaf.panel, Some(&leaf.path), b));
    let timeline = timeline_root.iter().map(|(panel, b)| (panel, None, b));
    let mut i = 0usize;
    for (panel, path, b) in panes.chain(timeline) {
        if can_reuse {
            can_reuse = prev.get(i).is_some_and(|(k, _)| *k == b.box_key);
        }
        if let Some(index) = ui.signal(b).subtree_press {
            if index >= latest {
                latest = index;
                found = (panel.clone(), path.cloned());
            }
        }
        i += 1;
    }
    // Box keys can churn under PXUI renew; idle still reuses when shape holds (same pane count).
    if latest < 0 && prev.len() == n {
        let _ = can_reuse;
        return (focus, focus_path, prev.clone());
    }
    let mut pane_keys = Vec::with_capacity(n);
    let panes = roots.iter().map(|(leaf, b)| (&leaf.panel, Some(&leaf.path), b));
    let timeline = timeline_root.iter().map(|(panel, b)| (panel, None, b));
    for (panel, path, b) in panes.chain(timeline) {
        pane_keys.push((b.box_key, (panel.clone(), path.cloned())));
    }
    let (mut focus, mut focus_path) = found;
    // a focus whose leaf an edit split, retyped or moved follows it, so the next panel key acts
    if focus_path.is_some() {
        if let Some(leaf) = focused_leaf(g, &focus, focus_path.as_deref()) {
            focus = leaf.panel.clone();
            focus_path = Some(leaf.path.clone());
        }
    }
    (focus, focus_path, std::rc::Rc::new(pane_keys))
}

#[allow(clippy::too_many_arguments)]
pub fn draw_notice<'a>(value: &ModelView<'a>, frame_: &frame::Frame, roots: &[(Leaf, pxui::Handle)], header_focus: Option<&layout::Path>, graph_body: layout::Bounds, scope_view: &scope::Pane, scope_active: bool, ui: &mut Ui) {
    // echo, the sheet's [08]: messages only (saved, undo and redo results, refusals), never a key
    // press; the last one fades after 3 seconds.  It stands at the bottom-left of the focused pane,
    // above the strip.
    if let Some((kind_, text)) = &value.notice {
        if frame_.input.t - value.notice_at < 3.0 {
            let bounds = match roots.iter().find(|(l, _)| Some(&l.path) == header_focus) {
                Some((l, _)) => l.body,
                None => graph_body,
            };
            fn cards(view: &scope::Pane, s: &fproj::Scope, out: &mut Vec<(f64, f64, f64, f64)>) {
                for n in &s.nodes {
                    out.extend(rays_graph::scope_frame::inspect::box_of(view, &n.path));
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
                NoticeKind::Info => TipKind::Info,
                NoticeKind::Refusal => TipKind::Refusal,
            };
            let avoid: Vec<(f32, f32, f32, f32)> = avoid.iter().map(|r| (r.0 as f32, r.1 as f32, r.2 as f32, r.3 as f32)).collect();
            status_bar::tips(ui, bounds, &avoid, &[(text.clone(), tip)]);
        }
    }
}

/// The viewport handles: their edits, a pick, and the changes they write.
#[allow(clippy::too_many_arguments)]
pub fn draw_handles<'a>(value: &ModelView<'a>, frame_: &frame::Frame, workspace: &Shell, selection: selection::Selection, selected: &Option<Picked>, lowered_level: bool, scope_active: bool, active: Option<&Leaf>, view_root: &pxui::Handle, view_handles: &ViewHandles, ui: &mut Ui) -> (selection::Selection, bool, Vec<Change>, Option<(i64, Vec<(String, Value)>)>) {
    // a lowered node is edited through its text: the handles of the node selected in the graph pane
    // write its arguments, the list's rows of a geometry object have none
    let scope_target = if scope_active { scope_node(value) } else { None };
    let handle_target: Option<Picked> = match &scope_target {
        Some((_, node)) => Some(of_sop(node)),
        None => {
            if lowered_level { None } else { selected.clone() }
        }
    };
    let (handle_edits, grab, picked_id) = match active {
        Some(leaf) if visible(workspace, &leaf.panel) => {
            let mut out = (Vec::new(), false, None);
            // ponytail: space reads the pane scope and scene, so reuse the current model until selection affects that operation.
            chrome::within_pane(ui, view_root, frame_, leaf.body, &mut |ui| {
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
                        Some(Change::SyntaxEdit(Op::SetArg { node: path.clone(), key: ArgKey::Kw(p.path.clone()), sub: Vec::new(), value: scene_sync::value_syntax(&fields) }))
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
pub fn pane_changes<'a>(value: &ModelView<'a>, vw: &ModelView<'a>, command_changes: Vec<Change>, scope_changes: &[scope::Change], menu_pick: &Option<String>, outline_intents: &[navigator::Intent]) -> Vec<Change> {
    let mut changes = command_changes;
    for c in scope_changes {
        match c {
            scope::Change::SyntaxEdit(op) => changes.push(Change::SyntaxEdit(op.clone())),
            scope::Change::LayoutActive { graph, index } => changes.push(Change::LayoutEdit(layout_edit::Op::SetLayout { graph: graph.clone(), index: *index })),
            // the pane's notices are all refusals
            scope::Change::Notice(message) => changes.push(Change::Declined(message.clone())),
            scope::Change::DefnRequested(paths) => changes.push(defn_change(vw, paths)),
            scope::Change::CopyRequested(paths) => changes.push(copy_bindings(value, paths)),
            scope::Change::PasteRequested => changes.extend(paste_bindings(value)),
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
                changes.push(Change::SyntaxEdit(Op::SetInputDefault { form: graph.clone(), input: input.clone(), value: builder.freeze().handle(id) }));
            }
            navigator::Intent::Rename { graph, to } => changes.push(Change::SyntaxEdit(Op::RenameGraph { name: graph.clone(), to_: to.clone() })),
            navigator::Intent::Remove(graph) => changes.push(Change::SyntaxEdit(Op::RemoveGraph { name: graph.clone() })),
            navigator::Intent::NewGraph(context) => changes.push(Change::SyntaxBatch("New graph".to_string(), vec![new_graph(value, context).1])),
            navigator::Intent::Flag { node, name, value: on } => {
                let mut builder = flow_syntax::arena::SyntaxBuilder::new();
                let flag = builder.intern(if *on { "true" } else { "false" });
                let id = builder.push(0, SNode::Sym(flag), Default::default(), None);
                changes.push(Change::SyntaxEdit(Op::SetArg {
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


/// Every inspector leaf: the one in use over the frame's state, the others over their own pane.
#[allow(clippy::too_many_arguments)]
pub fn draw_inspectors<'a>(
    frame_: &frame::Frame,
    value: &ModelView<'a>,
    workspace: &Shell,
    image: &dyn Fn(i64) -> Option<rays::image::Image>,
    camera_panel: &dyn Fn(&mut Ui) -> PanelResult,
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
    roots: &[&(Leaf, pxui::Handle)],
    ui: &mut Ui,
) -> (Option<PanelResult>, Vec<Change>, Vec<(String, Value)>, Vec<scope::Change>, Vec<(carry_mod::Place, bool)>, bool) {
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
    let mut panel = None;
    let mut inspector_changes = Vec::new();
    let mut settings_changes = Vec::new();
    let mut workspace_moves = Vec::new();
    let mut inspector_drops = Vec::new();
    let mut live_cook = value.live_cook;
    for (index, (leaf, root)) in roots.iter().copied().enumerate() {
        let host: Host = (panel_key(&value.doc, &leaf.path), leaf.path.clone(), leaf.panel.clone(), HostKind::List);
        let pane = pane_of(&host);
        let mine = in_use(&pane);
        let input = if mine {
            (*value, scope_view, selection, scope_active)
        } else {
            let shown = graph_shown && stored_projection(&pane) == GraphView && graph_name(&pane).is_some();
            (pane, pane_view(&pane), pane_selection(&pane), shown)
        };
        let (result, changes, settings, live, moves, drops) = inspect_panel(
            frame_, image, camera_panel, inspector_visible, unchanged, &resized, g, ui, input, root, leaf.body,
        );
        if index == 0 || (*focus == Panel::Inspector && Some(&leaf.path) == focus_path.as_ref()) {
            panel = result;
        }
        if mine {
            inspector_changes.extend(changes);
            settings_changes.extend(settings);
            if live != value.live_cook { live_cook = live; }
        }
        workspace_moves.extend(moves);
        inspector_drops.extend(drops);
    }
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
    let hot = c.preview.as_ref().and_then(|preview| path_of(carry_place(preview)).map(|p| (p, matches!(preview, CarryPreview::Showing { .. }))));
    (lit, hot)
}

/// The rows of a level's list and of every list that draws, by panel key.
pub fn list_rows<'a>(value: &ModelView<'a>, listing_: bool, list_drawn: bool, pane_of: &dyn Fn(&Host) -> ModelView<'a>, list_hosts: &[Host]) -> (Vec<Rc<RowSet>>, Option<Rc<ListRows>>, Vec<(Key_, Rc<ListRows>)>) {
    // the rows of a level's list, made once per network and kept while a list shows them
    let rows_of = |sets: &mut Vec<Rc<RowSet>>, pane: ModelView<'a>| -> Rc<ListRows> {
        let (source, made) = rows(&pane);
        let key = (displayed(&pane), pane.doc.active_camera);
        if let Some(entry) = sets.iter().find(|entry| same_source(&entry.0, &source) && entry.1 == key) {
            return entry.2.clone();
        }
        let entry = match value.rows.iter().find(|entry| same_source(&entry.0, &source) && entry.1 == key) {
            Some(entry) => entry.clone(),
            None => Rc::new((source.into_owned(), key, Rc::new(made()))),
        };
        let r = entry.2.clone();
        sets.insert(0, entry);
        r
    };
    let mut sets: Vec<Rc<RowSet>> = Vec::new();
    let rows = (listing_ || list_drawn).then(|| rows_of(&mut sets, *value));
    // the rows of every list that draws, by panel key: made here, before the frame builds
    let mut host_rows = Vec::new();
    for host in list_hosts {
        let pane = pane_of(host);
        let r = rows_of(&mut sets, pane);
        host_rows.insert(0, (host.0.clone(), r));
    }
    (sets, rows, host_rows)
}

/// `/ a`: the node menu of the graph the pane shows.
pub fn node_menu_step<'a>(value: &ModelView<'a>, workspace: &Shell, frame_: &frame::Frame, actions: &[Action], graph_shown: bool) -> (Option<node_menu::Menu>, Vec<Change>) {
    if actions.contains(&Action::AddNode) && graph_shown {
        let (gx, gy, gw, gh) = match graph_leaf(value, &geometry(value, workspace, frame_)) {
            Some(leaf) => leaf.body,
            None => (0, 0, 0, 0),
        };
        let (mx, my) = frame_.input.pointer;
        let at = if pxui::Rect::new(gx as f32, gy as f32, gw as f32, gh as f32).contains(p32((mx, my))) { (mx as i64, my as i64) } else { (gx + gw / 3, gy + gh / 3) };
        match open_menu(None, value, at) {
            Some(menu) => (Some(menu), Vec::new()),
            None => (value.menu.clone(), vec![Change::Declined("Open a graph to add a node to it".to_string())]),
        }
    } else {
        (value.menu.clone(), Vec::new())
    }
}

type TextState = (Option<Host>, text_pane::State, Option<text_pane::Shown>, Vec<(Key_, layout::Path, text_pane::State, text_pane::Shown)>);

/// What each text pane shows: its state and what it is showing.
pub fn text_state<'a>(value: &ModelView<'a>, actions: &[Action], pane_of: &dyn Fn(&Host) -> ModelView<'a>, text_hosts: &[Host], pane_text: text_pane::State) -> Result<TextState, rays_diagnostic::Diagnostic> {
    let ws = &value.doc.workspace.ws;
    let text_of = |state: text_pane::State, host: &Host| -> Result<Option<(text_pane::State, text_pane::Shown)>, rays_diagnostic::Diagnostic> {
        let pane = pane_of(host);
        let graph = match graph_name(&pane) {
            None if host.2 == Panel::Lisp => ws.checked.graphs.first().map(|g| g.name.as_str()),
            graph => graph,
        };
        let Some(graph) = graph else { return Ok(None) };
        let graph = graph.to_owned();
        let selected = match scope::selected(pane_view(&pane)) {
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
    let (mut text, text_shown) = match text_host.as_ref() {
        Some(host) => match text_of(pane_text, host)? {
            Some((text, shown)) => (text, Some(shown)),
            None => (text_pane::initial(), None),
        },
        None => (pane_text, None),
    };
    // the keys of the text (find, go to) wait in the pane's state for its view; nothing without one
    if text_shown.is_some() {
        for a in actions {
            if let Action::TextCommand(command) = a {
                text = text_pane::command(&text, command.clone());
            }
        }
    }
    let mut other_texts = Vec::new();
    for host in text_hosts {
        if Some(&host.0) == value.text_at.as_ref() {
            continue;
        }
        if let Some((state, shown)) = text_of(local_of(&value.locals, &host.0).code.clone(), host)? {
            other_texts.push((host.0.clone(), host.1.clone(), state, shown));
        }
    }
    Ok((text_host, text, text_shown, other_texts))
}

/// Command +/-/0: the kit text of the panels (the graph pane zooms, viewports have no text).
pub fn set_text_size(value: &EditorModel, actions: &[Action]) {
    for a in actions {
        if let Action::UiScale(delta) = a {
            let sizes = [8i64, 9, 10, 11, 12, 13, 14, 16, 18];
            let default = sizes.iter().position(|s| *s == value.text_size).unwrap_or(5);
            let here = sizes.iter().position(|s| *s == value.ui.borrow().font_size().round() as i64).unwrap_or(default);
            let index = if *delta == 0 { default } else { (here as i64 + delta).clamp(0, sizes.len() as i64 - 1) as usize };
            value.ui.borrow_mut().set_font_size(sizes[index] as f32);
        }
    }
}

/// What the hosted sketch sees of this frame, and the window focus it tracks.
pub fn gated(value: &mut EditorModel, frame_: &frame::Frame, text_focus: bool, host_events: Vec<Event>) {
    let mut window_focus = value.window_focus;
    for e in &frame_.input.events {
        match e {
            Event::FocusLost => window_focus = -1,
            Event::FocusGained => window_focus = 0,
            _ => {}
        }
    }
    window_focus = if window_focus >= 0 { (window_focus + 1).min(2) } else { -1 };
    let mut roots = PANE_KEYS.take();
    roots.clear();
    roots.extend(value.pane_keys.iter().map(|(k, _)| pxui::Key::Resolved(*k)));
    let hit = value.ui.borrow().root_at(pxui::Point::new(frame_.input.pointer.0 as f32, frame_.input.pointer.1 as f32), &roots);
    PANE_KEYS.set(roots);
    let pane_is_picture = match hit {
        None => true,
        Some(pxui::Key::Resolved(key)) => match value.pane_keys.iter().find(|(k, _)| *k == key) {
            Some((_, (panel, _))) => is_picture(panel),
            None => false,
        },
        _ => false,
    };
    let gate = input::gate::Gate {
        prompt: value.prompt.is_some(),
        menu: value.menu.is_some(),
        leader: value.leader != router::State::Idle,
        text_focus,
        focus_on_picture: is_picture(&value.focus),
        pointer_on_picture: pane_is_picture,
        window_key: window_focus >= 2,
        pressed: frame_.input.events.iter().any(|e| matches!(e, Event::PointerPressed(..))),
        previous: std::mem::replace(&mut value.live_frame, input::at_time(frame_.input.t)),
        blocked: std::mem::take(&mut value.sketch_blocked),
    };
    let (mut live_frame, sketch_blocked) = input::gate::gate(&gate, flow_native::live_frame::of_frame(frame_));
    if !host_events.is_empty() {
        let mut events = host_events;
        events.append(&mut live_frame.events);
        live_frame.events = events;
    }
    value.live_frame = live_frame;
    value.sketch_blocked = sketch_blocked;
    value.window_focus = window_focus;
}

/// The header intents the frame's actions stand for: panel toggles, and the focused panel's split,
/// close and retype.
pub fn panel_intents<'a>(value: &ModelView<'a>, leaves: &[(layout::Path, Panel)], workspace: &Shell, frame_: &frame::Frame, focus: &Panel, focus_path: Option<&[usize]>, actions: &[Action]) -> Vec<chrome::Intent> {
    let panel_paths = |panel: &Panel| -> Vec<layout::Path> { leaves.iter().filter(|(_, p)| p == panel).map(|(path, _)| path.clone()).collect() };
    let toggle = |panel: &Panel| -> Vec<chrome::Intent> {
        let paths = panel_paths(panel);
        let collapsed = paths.iter().filter(|path| panel_state(value, path).collapsed).count();
        let target_collapsed = paths.len() - collapsed >= collapsed;
        paths.into_iter().filter(|path| panel_state(value, path).collapsed != target_collapsed).map(chrome::Intent::Toggle).collect()
    };
    let expand = |panel: &Panel| -> Vec<chrome::Intent> { panel_paths(panel).into_iter().filter(|path| panel_state(value, path).collapsed).map(chrome::Intent::Toggle).collect() };
    let mut intents = Vec::new();
    for a in actions {
        match a {
            Action::ToggleGraph => intents.extend(toggle(&Panel::Graph)),
            Action::ToggleInspector => intents.extend(toggle(&Panel::Inspector)),
            Action::ToggleTimeline => {
                if panel_paths(&Panel::Timeline).is_empty() {
                    intents.push(chrome::Intent::Toggle(vec![HOST_BAR]));
                } else {
                    intents.extend(toggle(&Panel::Timeline));
                }
            }
            Action::OpenCamera => intents.extend(expand(&Panel::Inspector)),
            Action::AddNode => intents.extend(expand(&Panel::Graph)),
            _ => {}
        }
    }
    // / o ...: the focused panel's split, close, float and retype, as its header menu; the geometry
    // only when one was asked for
    let on_focused = actions.iter().any(|a| matches!(a, Action::PanelSplit(_) | Action::PanelClose | Action::FloatToggle | Action::PanelRetype(_)));
    if let Some(leaf) = on_focused.then(|| focused_leaf(&geometry(value, workspace, frame_), focus, focus_path)).flatten() {
        for a in actions {
            match a {
                Action::PanelSplit(axis) => intents.push(chrome::Intent::SplitPanel(leaf.path.clone(), *axis)),
                Action::PanelClose => intents.push(chrome::Intent::ClosePanel(leaf.path.clone())),
                Action::FloatToggle => intents.push(chrome::Intent::FloatPanel(leaf.path.clone())),
                // a view switch, below
                Action::PanelRetype(Panel::Graph | Panel::List | Panel::Lisp) if leaf.panel == Panel::Graph => {}
                Action::PanelRetype(panel) => intents.push(chrome::Intent::RetypePanel(leaf.path.clone(), panel.clone())),
                _ => {}
            }
        }
    }
    for a in actions {
        if let Action::PanelSplitAt(path, axis, ratio, panel) = a {
            intents.push(chrome::Intent::SplitPanelAt(path.clone(), *axis, *ratio, panel.clone()));
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
pub fn undo_passed(value: &EditorModel, text_focus: bool, modal: bool) -> Vec<Action> {
    if text_focus && !modal && !has_draft(None, value) {
        match value.ui.borrow().passed_undo() {
            Some(pxui::ui::UndoKind::Undo) => vec![Action::Undo],
            Some(pxui::ui::UndoKind::Redo) => vec![Action::Redo],
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
            let platform = rays_shell::host::platform(&value.ui.borrow());
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
        std::rc::Rc::new(keymap.iter().filter(|c| !matches!(action_of(c), Action::ListCommand(_) | Action::FrameCamera)).cloned().collect())
    }
}

fn ui_platform(value: &EditorModel) -> input::Platform {
    rays_shell::host::platform(&value.ui.borrow())
}

thread_local! {
    // Pane builders take owned leaves. Retain their storage between builds; handles are replaced
    // before the roots are read, even when another editor uses this thread.
    static PANE_KEYS: Cell<Vec<pxui::Key>> = const { Cell::new(Vec::new()) };
    static PANE_ROOTS: Cell<Vec<(Leaf, pxui::Handle)>> = const { Cell::new(Vec::new()) };
    static HEADER_POSITIONS: Cell<Vec<([Option<f32>; 2], f32)>> = const { Cell::new(Vec::new()) };
}

#[allow(clippy::too_many_arguments)]
pub fn update_frame(
    value: &mut EditorModel,
    carry_changed: bool,
    all_ui_visible: bool,
    text_focus: bool,
    camera_panel: &dyn Fn(&mut Ui) -> PanelResult,
    view_handles: &ViewHandles,
    render_status: Option<String>,
    error_status: Option<String>,
    view_state: Rc<dyn Fn(Option<&PanelResult>) -> Syntax>,
    frame_: &frame::Frame,
    gpu: Option<&flow_ir::flow_ir::gpu::Config>,
    pump: Option<&dyn Fn()>,
    image: &dyn Fn(i64) -> Option<rays::image::Image>,
    host_events: Vec<Event>,
) -> Update {
    let carrying = value.carry.is_some();
    gated(value, frame_, text_focus, host_events);
    let hidden = shell_hidden(&(&*value).into(), &value.workspace);
    if hidden != value.workspace.hidden {
        value.workspace.hidden = hidden.to_vec();
    }
    follow_start(value);
    let (focus, focus_path) = if all_ui_visible && frame::has_event(&|e| matches!(e, Event::PointerPressed(..)), frame_) {
        let roots: Vec<_> = value.pane_keys.iter().map(|(k, _)| pxui::Key::Resolved(*k)).collect();
        let pressed = rays_shell::host::last_press_within(&value.ui.borrow(), frame_, &roots);
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
    // Pointer-only frames never match a chord; skip building the keymap ref vec (hover path).
    let need_keymap = frame_.input.events.iter().any(|e| {
        matches!(
            e,
            Event::KeyPressed(_)
                | Event::KeyReleased(_)
                | Event::TextInput(_)
                | Event::TextEditing { .. }
        )
    });
    let keymap_refs: Vec<&Command> = if need_keymap { keymap.iter().collect() } else { Vec::new() };
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
    let mut actions: Vec<Action> = commands.iter().map(|c| action_of(c).clone()).collect();
    let hud = match hud {
        Some((_, until)) if frame_.input.t >= until => None,
        other => other,
    };
    let guide = actions.iter().fold(value.guide, |guide, a| if *a == Action::GuideToggle { !guide } else { guide });
    let passed = undo_passed(&*value, text_focus, modal);
    let mut all_actions = passed;
    all_actions.extend(value.queued.clone());
    all_actions.extend(actions.drain(..));
    let mut actions = all_actions;
    if carrying {
        actions.retain(carry_allowed);
    }
    let copied = if actions.contains(&Action::CopyLisp) { Some(copy_workspace(&*value)) } else { None };
    set_text_size(&*value, &actions);
    let steady = if frame_.input.dt > 0.0 && frame_.input.dt == value.last_dt { value.steady + 1 } else { 0 };
    let (status_fps, status_fps_at) = fps_sample(&(&*value).into(), frame_);
    let keyboard_event = |event: &Event| matches!(event, Event::KeyPressed(_) | Event::KeyReleased(_) | Event::TextInput(_) | Event::TextEditing { .. });
    let shortcut_frame = if text_focus && frame_.input.events.iter().any(keyboard_event) {
        let mut filtered = frame_.clone();
        filtered.input.events.retain(|event| !keyboard_event(event));
        Cow::Owned(filtered)
    } else {
        Cow::Borrowed(frame_)
    };
    let (timeline_now, timeline_changes) = timeline::update(&value.timeline, &shortcut_frame);
    let (applied, timeline_now, timeline_changes) = if actions.is_empty() {
        (None, timeline_now, timeline_changes)
    } else {
        let start = (value.workspace.clone(), pane_selection(&(&*value).into()).clone(), value.tree.clone(), timeline_now, timeline_changes.into_owned());
        let (workspace, selection, tree, timeline, changes) = actions.iter().fold(start, |state, action| apply_action(value, state, action));
        (Some((workspace, selection, tree)), timeline, std::borrow::Cow::Owned(changes))
    };
    value.timeline = timeline_now;
    sync_scope(value);
    let mut other_panes = Vec::new();
    if all_ui_visible {
        let leaves = graph_leaves(&(&*value).into());
        let active = panel_map::key_ref(&value.panes);
        other_panes.extend(leaves.iter().filter(|(key, _)| Some(key) != active).cloned());
        if !other_panes.is_empty() {
            other_panes.sort_by(|a, b| a.0.cmp(&b.0));
            other_panes.dedup_by(|a, b| a.0 == b.0);
            let active = active.cloned().unwrap_or_default();
            let active_leaf = panel_map::leaf(&value.panes).cloned();
            // An inactive projection may consume its own deferred selection.
            let select_later = value.select_later.clone();
            for (key, path) in &other_panes {
                with_pane_in_use(value, (key, path));
                sync_scope(value);
                value.select_later = select_later.clone();
            }
            panel_map::activate(&mut value.panes, &active, &|| unreachable!("the active pane exists"));
            panel_map::set_leaf(&mut value.panes, active_leaf);
        }
    }
    // take pane text before Cow borrows into value (selection/workspace)
    let taken_text = std::mem::replace(&mut value.text, text_pane::initial());
    let (workspace, selection, tree_value) = match applied {
        Some((workspace, selection, tree)) => (Cow::Owned(workspace), Cow::Owned(selection), Cow::Owned(tree)),
        None => (Cow::Borrowed(&value.workspace), Cow::Borrowed(pane_selection(&(&*value).into())), Cow::Borrowed(&value.tree)),
    };
    let mut vw = ModelView::from(&*value);
    vw.workspace = &workspace;
    vw.focus = &focus;
    vw.selection = &selection;
    let graph_shown = all_ui_visible && graph_family(&vw);
    let listing_ = listing(&vw);
    let hosted = |k: HostKind| -> Vec<Host> { hosts.iter().filter(|h| h.3 == k).cloned().collect() };
    let list_hosts = hosted(HostKind::List);
    let list_drawn = !list_hosts.is_empty();
    // What a text pane shows: a graph panel's own graph and selection, a lisp panel the ones of the
    // graph pane in use (the first graph of the workspace when none is open).
    // a panel's pane, laid out: the one in use, or one of the others
    let pane_of = |host: &Host| shown_as(&(&*value).into(), (&host.0, &host.1, &host.2));
    let ((text_host, text, text_shown, other_texts), text_error) = {
        let mv = (&*value).into();
        match text_state(&mv, &actions, &pane_of, &hosted(HostKind::Text), taken_text) {
            Ok(state) => (state, None),
            Err(error) => ((None, text_pane::initial(), None, Vec::new()), Some(rays_diagnostic::to_string(&error))),
        }
    };
    let scope_active = graph_shown && scope_name(&(&*value).into()).is_some();
    // the frame's copy of the pane, its commands run: made once, and again only for a build PXUI repeats
    // (the first build took the copy; the result hands it to reduce)
    let frame_pane = |value: &EditorModel| -> (scope::Pane, Vec<scope::Change>) {
        let mut view = pane_view(&value.into()).clone();
        let mut changes = Vec::new();
        for a in &actions {
            match a {
                Action::ScopeCommand(command) if scope_active => rays_graph::scope_commands::run_command(&mut view, command.clone(), &mut changes),
                Action::OpenCamera => scope::clear_selection(&mut view),
                _ => {}
            }
        }
        (view, changes)
    };
    let (view, scope_command_changes) = frame_pane(value);
    let mut scope_view = Some(view);
    let (menu, command_changes) = node_menu_step(&(&*value).into(), &workspace, frame_, &actions, graph_shown);
    let shown_node = match pane_level(&(&*value).into()) {
        Level::Inside(id) => match value.viewed.get(&id) {
            Some((_, d)) => Some(*d),
            None => displayed(&(&*value).into()),
        },
        Level::Scene => displayed(&(&*value).into()),
    };
    let (row_sets, rows, host_rows) = list_rows(&(&*value).into(), listing_, list_drawn, &pane_of, &list_hosts);
    let selected = |selection: &selection::Selection| -> Vec<i64> {
        let mut ids = selection::selected_nodes(selection).to_vec();
        let primary = selection::selected(selection);
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
            Action::ListCommand(tree::Command::ActivateRow) => {}
            Action::ListCommand(command) if graph_shown => {
                tree::run_command(tree_value.to_mut(), rows.as_ref().map_or(&[][..], |pair| pair.0.as_slice()), &selected(&selection), command.clone(), &mut list_intents);
            }
            _ => {}
        }
    }
    let open_graph = if !graph_shown {
        None
    } else {
        actions.iter().find_map(|a| match a {
            Action::ListCommand(tree::Command::ActivateRow) if listing_ => match tree::focused(&tree_value) {
                Some(id) => Some(id),
                None => selection::selected(&selection),
            },
            _ => None,
        })
    };
    let initial_frame_request = if !actions.contains(&Action::FrameCamera) { None } else { shown_node };
    let (carry_lit, carry_hot) = carry_marks(&*value);
    let unchanged = value.doc.settings.clone();
    // Presets: / s names and saves the document, / b browses, loads (Enter), and deletes (Delete
    // twice). A load replaces the document below as one undo entry.
    let mut initial_prompt = value.prompt.clone();
    for a in &actions {
        match a {
            Action::SavePreset => initial_prompt = Some(Prompt::Saving(editor_document::preset::default_name((value.clock)()))),
            Action::BrowsePresets => initial_prompt = browse(&*value, ""),
            Action::GuideKeys => initial_prompt = Some(Prompt::Keys),
            Action::CommandPalette => initial_prompt = Some(Prompt::Palette(String::new())),
            Action::Jump => initial_prompt = Some(Prompt::Jumping(String::new())),
            Action::GotoSymbol if text_shown.is_some() => initial_prompt = Some(Prompt::Symbols(String::new())),
            Action::GotoLine if text_shown.is_some() => initial_prompt = Some(Prompt::Going(String::new())),
            Action::PanelPick => initial_prompt = Some(Prompt::Picking { split: true, at: frame_.input.pointer }),
            Action::WindowPick => initial_prompt = Some(Prompt::Picking { split: false, at: frame_.input.pointer }),
            _ => {}
        }
    }
    let leader_panel = match &leader {
        router::State::Pending(prefix) => Some(prefix.clone()),
        router::State::Idle => None,
    };

    let workspace_override = match workspace { Cow::Owned(v) => Some(v), Cow::Borrowed(_) => None };
    let selection_override = match selection { Cow::Owned(v) => Some(v), Cow::Borrowed(_) => None };
    let tree_override = match tree_value { Cow::Owned(v) => Some(v), Cow::Borrowed(_) => None };
    let ui_handle = value.ui.clone();
    let mut built: Option<FrameResult> = None;
    {
        // PXUI may rebuild after layout without replaying pointer events. Keep the draft and
        // release from the first build so the final build publishes the same geometry.
        let mut splitter_workspace = None;
        let mut splitter_released = false;
        let mut pending_prompt = initial_prompt.clone();
        let mut pending_menu = menu.clone();
        let mut build_and_prompt = |ui: &mut Ui| {
            let workspace = Cow::Borrowed(workspace_override.as_ref().unwrap_or(&value.workspace));
            let selection = selection_override.as_ref().unwrap_or_else(|| &panel_map::active(&value.panes).picked);
            let tree_value = tree_override.as_ref().unwrap_or(&value.tree);
            let pane_of = |host: &Host| shown_as(&(&*value).into(), (&host.0, &host.1, &host.2));
            let in_use = |pane: &ModelView<'_>| pane.key == panel_map::key_ref(&value.panes);
            let mut vw = ModelView::from(&*value);
            vw.workspace = &workspace;
            vw.focus = &focus;
            vw.selection = selection;
            if let Some(prefix) = &leader_panel {
                let category = |c: &Command| editor_commands::group(c).to_string();
                let describe = |p: &str| editor_commands::describe_prefix(p);
                let order = |sec: &str| editor_commands::order(sec);
                which_key::panel(ui, Some(&category), Some(&describe), Some(&order), &keymap, prefix, &focus, editor_commands::pane_name(&focus));
            }
            if !all_ui_visible {
                return;
            }
            if ui.captured() || frame::has_event(&|e| matches!(e, Event::PointerReleased(..)), frame_) {
                if let Some((draft, released)) = last_geometry(&vw).and_then(|g| chrome::splitter_drag(ui, &g)) {
                    if draft.is_some() {
                        splitter_workspace.get_or_insert_with(|| workspace.as_ref().clone()).live = draft;
                    }
                    splitter_released |= released;
                }
            }
            if !splitter_released && workspace.live.is_some()
                && (!frame::mouse_down(MouseButton::LeftButton, frame_) || frame::has_event(&|e| matches!(e, Event::FocusLost | Event::PointerCancelled(MouseButton::LeftButton)), frame_))
            {
                splitter_workspace.get_or_insert_with(|| workspace.as_ref().clone()).live = None;
            }
            let workspace = splitter_workspace.as_ref().unwrap_or(&workspace);
            vw.workspace = &workspace;
            let g = geometry(&vw, &workspace, frame_);
            let state_fn = |path: &[usize]| panel_state(&vw, path);
            let mut grips = chrome::splitters_with_geometry(Some(&state_fn), &g, ui);
            if splitter_released {
                grips.push(chrome::Intent::Settled);
            }
            // PXUI can settle layout with another build in the same frame. Keep the first
            // build's accepted query or intent once its keyboard events have been consumed.
            let (prompt_intent_, prompt_error) = match prompt_panel(&(&*value).into(), &focus, &keymap, frame_, &workspace, &text, text_shown.as_ref(), ui, pending_prompt.clone()) {
                Ok((prompt, intent)) => {
                    pending_prompt = prompt;
                    (intent, None)
                }
                Err(error) => (None, Some(rays_diagnostic::to_string(&error))),
            };
            let prompt_next = pending_prompt.clone();
            // ---- build ----
            // Chrome first: panel backgrounds, splitters and headers.  Its intents fold into the
            // shell now (a fold, a drag) or become one editor-graph edit after the frame.
            let leaves = shell_leaves(value, workspace);
            let header_focus = focus_path.as_ref().or_else(|| leaves.iter().find_map(|(path, panel)| (*panel == focus).then_some(path)));
            // Keep just the positions pane builders use. The widths are measured once, alongside
            // the title, in the order chrome visits the leaves.
            let mut header_positions = HEADER_POSITIONS.take();
            header_positions.clear();
            header_positions.resize(g.leaves.len(), ([None, None], 0.0));
            let mut describe_header = |ui: &mut Ui, index: usize, leaf: &Leaf, title: &mut String| {
                write_panel_title(&vw, leaf, title);
                let (slots, tools) = &mut header_positions[index];
                if matches!(leaf.panel, Panel::Graph | Panel::View(_)) {
                    *tools = chrome::tools_start(ui, header_focus == Some(&leaf.path), leaf.floating, false, title);
                }
                let text_ = match (&text_host, &text_shown) {
                    (Some((_, path, _, _)), Some(shown)) if *path == leaf.path => Some((&text, shown)),
                    _ => other_texts.iter().find_map(|(_, path, state, shown)| (*path == leaf.path).then_some((state, shown))),
                };
                let mut widths = [0.0; 2];
                let graph = usize::from(leaf.panel == Panel::Graph);
                if graph > 0 {
                    widths[0] = rays_shell::kit::text_width(ui, "GraphListText") + 24.0;
                }
                let n = if let Some((state, shown)) = text_ {
                    widths[graph] = text_pane::tabs_width(ui, leaf.body.2, state, shown);
                    graph + 1
                } else {
                    graph
                };
                let mut positions = [None; 2];
                let limit = chrome::header_slots_into(leaf, &widths[..n], &mut positions);
                *slots = [if graph > 0 { positions[0] } else { None }, text_.and(positions[graph])];
                limit
            };
            let key_of = |id: &str| -> String {
                match keymap.iter().find(|c| c.id == id) {
                    Some(Command { trigger: Some(trigger), .. }) => keymap::label(platform, trigger),
                    _ => String::new(),
                }
            };
            let mut intents = chrome::update_with_geometry(Some(&state_fn), Some(&workspace.hidden), &mut describe_header, Some(&key_of), header_focus, &g, ui, &shortcut_frame);
            let positions = |leaf: &Leaf| g.leaves.iter().position(|l| l.path == leaf.path).map(|i| header_positions[i]).unwrap_or_default();
            let tab_at = |leaf: &Leaf| positions(leaf).0;
            let tools_from = |leaf: &Leaf| positions(leaf).1;
            intents.extend(panel_intents(&vw, &leaves, &workspace, frame_, &focus, focus_path.as_deref(), &actions));
            let layout_leaf = focused_leaf(&g, &focus, focus_path.as_deref());
            let (workspace, mut layout_changes) = layout_intents_cow(&vw, workspace, &intents);
            layout_changes.extend(layout_actions(&(&*value).into(), &workspace, layout_leaf.as_ref(), &actions));
            vw.workspace = &workspace;
            // the graph panel's toolbar; a click is one command or edit
            let graph_host = graph_leaf(&vw, &g);
            let (tool_action, view_action, view_pick) = draw_graph_header(ui, &vw, &g, &graph_host, &tab_at, &tools_from);
            let mut roots = PANE_ROOTS.take();
            for (order, leaf) in g.leaves.iter().enumerate() {
                let boxed = chrome::pane_root_fmt(ui, frame_, leaf.body, format_args!("workspace-pane-{}{}", editor_commands::pane_name(&leaf.panel), chrome::key(&leaf.path)));
                if leaf.floating {
                    ui.to_front((order * 2) as i64, &boxed);
                }
                if let Some((previous, root)) = roots.get_mut(order) {
                    if previous != leaf {
                        previous.clone_from(leaf);
                    }
                    *root = boxed;
                } else {
                    roots.push((leaf.clone(), boxed));
                }
            }
            roots.truncate(g.leaves.len());
            let viewport_drops: Vec<(carry_mod::Place, bool)> = if !carrying {
                Vec::new()
            } else {
                roots
                    .iter()
                    .filter_map(|(leaf, boxed)| match &leaf.panel {
                        Panel::View(key) => ui.drop_target(boxed).map(|d| (carry_mod::Place::Viewport(key.clone()), matches!(d, pxui::DropRef::Dropped(_)))),
                        _ => None,
                    })
                    .collect()
            };
            let timeline_root: Option<(Panel, pxui::Handle)> = if layout::find(&g, &Panel::Timeline).is_none() {
                Some((Panel::Timeline, chrome::pane_root(ui, frame_, g.timeline_at, "workspace-pane-Timeline")))
            } else {
                None
            };
            // every leaf draws in its own root: its PXUI keys are seeded by it
            let root_at = |path: &layout::Path| roots.iter().find(|(l, _)| l.path == *path && l.body.3 > 0);
            let of_kind = |panel: &Panel| -> Vec<&(Leaf, pxui::Handle)> { roots.iter().filter(|(l, _)| l.panel == *panel && visible(&workspace, panel) && l.body.3 > 0).collect() };
            let none_root = |ui: &mut Ui| chrome::pane_root(ui, frame_, (0, 0, 0, 0), "workspace-pane-none");
            let graph_root = match graph_host.as_ref().and_then(|l| root_at(&l.path)) {
                Some((_, b)) => *b,
                None => none_root(ui),
            };
            let active = active_view(&vw, &g);
            let view_root = draw_viewport_pane(ui, &(&*value).into(), probe_caption(&vw), &roots, active, &none_root);
            let mut scope_view2 = scope_view.take().unwrap_or_else(|| frame_pane(value).0);
            let mut scope_changes = scope_command_changes.clone();
            let (graph_body, menu2, menu_pick) = draw_graph_pane(ui, frame_, &(&*value).into(), &shortcut_frame, &mut scope_view2, scope_active, guide, carry_lit.clone(), carry_hot.clone(), &graph_root, &graph_host, &workspace, pending_menu.take(), &open_menu, &mut scope_changes);
            pending_menu = menu2.clone();
            // Each pane builder returns only its frame intents and updated pane-local state.
            let (tree2, list_intents2, locals) = draw_list_panes(ui, &shortcut_frame, &(&*value).into(), &list_hosts, &root_at, &pane_of, &in_use, &|pane| level_name(pane), rows.as_ref(), &host_rows, &|| selected(selection), tree_value.clone(), list_intents.clone(), value.locals.clone());
            let (text2, text_intents, locals, other_texts2) = draw_text_panes(ui, frame_, &*value, &root_at, &tab_at, &text_host, &text_shown, text.clone(), other_texts.clone(), locals);
            let (outline, outline_intents, locals) = draw_outline_panes(ui, frame_, &*value, &hosted(HostKind::Outline), &root_at, value.outline.clone(), locals);
            let locals = draw_spreadsheet_panes(ui, frame_, &(&*value).into(), &of_kind(&Panel::Spreadsheet), locals);
            let _ = text2;
            let mut changes = pane_changes(&(&*value).into(), &vw, command_changes.clone(), &scope_changes, &menu_pick, &outline_intents);
            // a layout row and the add button are commands: they run like a toolbar click
            let mut outline_action: Option<Action> = None;
            for i in &outline_intents {
                match i {
                    navigator::Intent::Layout(index) => outline_action = Some(Action::LayoutSwitch(*index)),
                    navigator::Intent::Add => outline_action = Some(Action::AddNode),
                    _ => {}
                }
            }
            // a click on an earlier part of the graph header's route goes back that far
            let crumb_action = intents.iter().find_map(|i| match i {
                chrome::Intent::Crumb(path, index) if !pane_route(&(&*value).into()).is_empty() && leaves.iter().any(|(p, panel)| p == path && *panel == Panel::Graph) => {
                    let steps = (pane_route(&(&*value).into()).len() as i64 + 1).min(3) - 1 - index;
                    if steps > 0 { Some(Action::Back(steps)) } else { None }
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
            sync_list_selection(&(&*value).into(), &list_intents2, &mut scope_view2);
            let inspector_visible = has_panel(&vw, &Panel::Inspector);
            let selected_ids = selection::selected_nodes(&selection);
            let selected_id = if selected_ids.len() == 1 { selection::selected(&selection) } else { None };
            let selected_obj: Option<Picked> = selected_id.and_then(|id| kg::find(&level_objects(&(&*value).into()), id).map(|n| of_object(&n)));
            let lowered_level = match pane_level(&(&*value).into()) {
                Level::Inside(id) => kind(&*value, id).as_deref() == Some("geometry"),
                Level::Scene => false,
            };
            let (panel, inspector_changes, settings_changes, workspace_moves, inspector_drops, live_cook) = draw_inspectors(
                frame_,
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
            let timeline_slots: Vec<(layout::Bounds, pxui::Handle, bool)> = if timeline_leaves.is_empty() {
                timeline_root.iter().map(|(_, b)| (g.timeline_at, *b, true)).collect()
            } else {
                timeline_leaves.iter().map(|&&(ref l, b)| (l.body, b, false)).collect()
            };
            let (timeline_intents, strip_layout) = draw_timeline_pane(ui, frame_, &value.timeline, value.timeline_frames, &timeline_slots, &timeline_root, g.timeline_at);
            let (selection, grab, handle_ops, handle_changes) = draw_handles(&(&*value).into(), frame_, &workspace, selection, &selected_obj, lowered_level, scope_active, active, &view_root, view_handles, ui);
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
            draw_notice(&(&*value).into(), frame_, &roots, header_focus, graph_body, &scope_view2, scope_active, ui);
            let (focus2, focus_path2, pane_keys) = follow_press(&g, &roots, &timeline_root, focus.clone(), focus_path.clone(), &value.pane_keys, ui);
            let (workspace, drag_changes) = settle_layout(&(&*value).into(), &vw, &workspace, &keymap, &g, &intents, grips, strip_layout, &timeline_leaves, platform, ui);
            HEADER_POSITIONS.set(header_positions);
            changes.extend(drag_changes);
            let mut drops = inspector_drops;
            drops.extend(viewport_drops);
            let result = FrameResult {
                workspace: workspace.into_owned(),
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
                list_rows: rows.clone(),
                edit_error: text_error.clone().or(prompt_error.clone()).or_else(|| value.edit_error.clone()),
                effects: Effects::default(),
                timeline_intents,
                frame_request: initial_frame_request,
                prompt: prompt_next,
                prompt_intent: prompt_intent_,
                panel,
                grab,
                settings: unchanged.clone(),
                opened: None,
                live_cook,
                label: String::new(),
                changes,
                tree_intents: list_intents2,
                text_intents,
                open_graph,
                settings_changes,
                graph_panes: Vec::new(),
                locals,
                other_texts: other_texts2,
                handle_changes,
                bar_action,
                view_pick,
                drops,
            };
            // The remaining graph views only change pane-local state. All model readers in this
            // build have finished, so the panes can be lent to the drawer in place.
            draw_other_graph_panes(ui, &shortcut_frame, value, &other_panes, &roots);
            PANE_ROOTS.set(roots);
            // Layout settling rebuilds the picture with quiet signals. Reduce the intents from
            // the first build, which received the frame's input.
            if built.is_none() {
                built = Some(result);
            }
        };
        rays_shell::host::frame(&mut ui_handle.borrow_mut(), frame_, &mut build_and_prompt);
    }
    let result = match built {
        Some(result) => result,
        None => FrameResult {
            workspace: workspace_override.unwrap_or_else(|| value.workspace.clone()),
            focus: focus.clone(),
            focus_path: focus_path.clone(),
            pane_keys: std::rc::Rc::new(Vec::new()),
            graph_panes: Vec::new(),
            locals: value.locals.clone(),
            other_texts: Vec::new(),
            outline: value.outline.clone(),
            outline_intents: Vec::new(),
            selection: selection_override.unwrap_or_else(|| pane_selection(&(&*value).into()).clone()),
            menu: menu.clone(),
            menu_pick: None,
            scope_view: scope_view.take().unwrap_or_else(|| frame_pane(value).0),
            scope_changes: scope_command_changes.clone(),
            tree: tree_override.unwrap_or_else(|| value.tree.clone()),
            list_rows: rows.clone(),
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
            label: String::new(),
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
        FrameInput {
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
    camera_panel: &dyn Fn(&mut Ui) -> PanelResult,
    view_handles: &ViewHandles,
    render_status: Option<String>,
    error_status: Option<String>,
    view_state: Rc<dyn Fn(Option<&PanelResult>) -> Syntax>,
    frame_: &frame::Frame,
    gpu: Option<&flow_ir::flow_ir::gpu::Config>,
    image: Option<&dyn Fn(i64) -> Option<rays::image::Image>>,
    host_events: Vec<Event>,
    pump: Option<&dyn Fn()>,
) -> Update {
    let guard = flow_syntax::phase_timer::SampleGuard::start_arc(value.clock.clone());
    let before = value.doc.clone();
    let (frame_after, carry_changed) = carry_step(value, text_focus, frame_);
    let no_image = |_: i64| None;
    let image: &dyn Fn(i64) -> Option<rays::image::Image> = match image {
        Some(i) => i,
        None => &no_image,
    };
    let out = update_frame(value, carry_changed, all_ui_visible, text_focus, camera_panel, view_handles, render_status, error_status, view_state, &frame_after, gpu, pump, image, host_events);
    if !std::sync::Arc::ptr_eq(&value.doc, &before) {
        value.edit_phases = guard.finish();
    } else {
        // Idle: park the Sample shell; do not build a PhaseTimer Vec.
        guard.discard();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cook;
    use crate::editor_host;
    use crate::editor_panels::panel_title;
    use crate::editor_setup::{self, CreateOpts};
    use editor_document::workspace_doc::TextOptions;

    fn big_stack(body: fn()) {
        std::thread::Builder::new().stack_size(256 << 20).spawn(body).unwrap().join().unwrap();
    }

    fn model_of(text: &str) -> EditorModel {
        let catalog = editor_document::kinds::catalog(flow_sop::manifest::VERSION, &sop::nodes::factories()).expect("catalog");
        let ws = workspace_doc::of_text(&TextOptions::default(), &catalog, text).expect("workspace");
        let opts = CreateOpts {
            preferences: Some(std::env::temp_dir().join("rays-editor-test-no-preferences.rays").to_string_lossy().to_string()),
            presets: Some(std::env::temp_dir().join("rays-editor-test-presets").to_string_lossy().to_string()),
            ui_text_size: 11,
            await_: Some(true),
            ..CreateOpts::default()
        };
        editor_setup::create(opts, &ws, Rc::new(cook::prepare)).expect("model")
    }

    fn idle_frame() -> frame::Frame {
        let mut input = input::at_time(0.0);
        input.size = (900, 640);
        frame::Frame { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
    }

    fn step_model(value: &mut EditorModel, tick: &mut i64, pointer: (f64, f64), events: Vec<Event>) {
        step_model_buttons(value, tick, &[], pointer, events);
    }

    fn step_model_buttons(value: &mut EditorModel, tick: &mut i64, buttons: &[MouseButton], pointer: (f64, f64), events: Vec<Event>) {
        *tick += 1;
        let t = *tick;
        let mut input = input::at_time(t as f64 / 60.0);
        input.dt = 1.0 / 60.0;
        input.frame = t;
        input.tick = t;
        input.size = (900, 640);
        input.pointer = pointer;
        input.buttons = buttons.to_vec();
        input.events = events;
        let f = frame::Frame { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) };
        let no_cam = |_: &mut Ui| -> PanelResult { Rc::new(()) };
        let no_handles = |_: &mut Ui, _: Option<&Picked>, _: rays_math::mat4::T, _: layout::Bounds| (Vec::new(), false, None);
        let no_view_state: Rc<dyn Fn(Option<&PanelResult>) -> Syntax> = Rc::new(|_| {
            let mut b = flow_syntax::arena::SyntaxBuilder::new();
            let fields = b.range(&[]);
            let id = b.push(0, SNode::Map(fields), Default::default(), None);
            b.freeze().handle(id)
        });
        let no_image = |_: i64| None;
        let _ = update_frame(value, false, true, false, &no_cam, &no_handles, None, None, no_view_state, &f, None, None, &no_image, Vec::new());
    }

    #[test]
    fn b5_split_preserves_graph_pane_and_locals() {
        big_stack(|| {
            let mut value = model_of(
                r#"(workspace w
                    (graph a :context sop (sop/box))
                    (graph b :context sop (let* [box1 (sop/box)] box1))
                    (graph editor :context editor
                      (let* [net (ui/graph "a" :focus true) code (ui/lisp)]
                        (ui/workspace (ui/split_at "horizontal" 0.6 net code)))))"#,
            );
            let mut tick = 0;
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            // Navigate to graph b (building a route), settle its initial layout, then pan and select.
            go(&mut value, "b");
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            {
                let p = pane_mut(&mut value);
                p.view.pan_x = 123.0;
                p.view.pan_y = -45.0;
                scope::select(vec![vec!["b".into(), "box1".into()]], &mut p.view);
            }
            // Give the Lisp panel its own non-default tab state in `text` / `locals`.
            value.text.tab = text_pane::Tab::Document;
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            // Split the focused graph panel with /ov.
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('o')), Event::KeyPressed(Key::KeyChar('v'))]);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            let printed = workspace_doc::print(&value.doc.workspace.ws).unwrap().0;
            assert!(printed.contains("(ui/split_at \"horizontal\" 0.5 net net_b)"), "B5 setup: /ov did not split net: {printed}");
            assert_eq!(pane_shown(&(&value).into()).map(String::as_str), Some("b"), "B5: split reset pane.shown");
            assert!(!pane_route(&(&value).into()).is_empty(), "B5: split cleared pane.route");
            assert_eq!(pane_view(&(&value).into()).pan_x, 123.0, "B5: split reset pan_x");
            assert_eq!(pane_view(&(&value).into()).pan_y, -45.0, "B5: split reset pan_y");
            assert_eq!(scope::selected(pane_view(&(&value).into())), vec![vec![Symbol::from("b"), Symbol::from("box1")]], "B5: split cleared selection");
            // Also split the Lisp panel: its local state (Tab::Document) must stay attached.
            value.focus = Panel::Lisp;
            value.focus_path = Some(vec![1]);
            step_model(&mut value, &mut tick, (700.0, 200.0), vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('o')), Event::KeyPressed(Key::KeyChar('v'))]);
            step_model(&mut value, &mut tick, (700.0, 200.0), vec![]);
            assert_eq!(value.text.tab, text_pane::Tab::Document, "B5: splitting Lisp panel orphaned its local tab");
            editor_host::close(&value);
        });
    }

    #[test]
    fn a_split_reusing_a_closed_panes_name_starts_fresh() {
        big_stack(|| {
            let mut value = model_of(
                r#"(workspace w
                    (graph a :context sop (sop/box))
                    (graph editor :context editor
                      (let* [net (ui/graph "a" :focus true) code (ui/lisp)]
                        (ui/workspace (ui/split_at "horizontal" 0.6 net code)))))"#,
            );
            let mut tick = 0;
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            // what a closed `net_b` left: split/close pairs reuse the names they free
            let stale: panel_map::Key = vec!["editor".into(), "net_b".into()];
            value.locals = crate::editor_panes::put_local(&stale, &|l| Local { code: text_pane::State { tab: text_pane::Tab::Document, ..l.code }, ..l }, &value.locals);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('o')), Event::KeyPressed(Key::KeyChar('v'))]);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            let printed = workspace_doc::print(&value.doc.workspace.ws).unwrap().0;
            assert!(printed.contains("net_b"), "setup: /ov did not split net: {printed}");
            assert!(!value.locals.iter().any(|(k, l)| *k == stale && l.code.tab == text_pane::Tab::Document), "the new pane took a closed pane's state");
            editor_host::close(&value);
        });
    }

    #[test]
    fn b6_layout_switch_preserves_inactive_graph_panes() {
        big_stack(|| {
            let mut value = model_of(
                r#"(workspace w
                    (graph a :context sop (sop/box))
                    (graph b :context sop (sop/box))
                    (graph editor :context editor
                      (let* [g0 (ui/graph "a") g1 (ui/graph "a")
                             l0 g0 l1 (ui/split_at "horizontal" 0.5 g1 (ui/inspector))]
                        (ui/workspace (ui/switch l0 l1 :active 0)))))"#,
            );
            let mut tick = 0;
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            // Switch to layout 1 (/[1).
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('[')), Event::KeyPressed(Key::KeyChar('1'))]);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            // Navigate on layout 1's graph panel g1, settle layout, then pan.
            go(&mut value, "b");
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            {
                let p = pane_mut(&mut value);
                p.view.pan_x = 77.0;
                p.view.pan_y = -33.0;
            }
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            // Switch back to layout 0 (/[0), settle, then switch back to layout 1 (/[1).
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('[')), Event::KeyPressed(Key::KeyChar('0'))]);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('[')), Event::KeyPressed(Key::KeyChar('1'))]);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            assert_eq!(pane_shown(&(&value).into()).map(String::as_str), Some("b"), "B6: layout switch dropped g1's shown graph");
            assert!(!pane_route(&(&value).into()).is_empty(), "B6: layout switch dropped g1's route");
            assert_eq!(pane_view(&(&value).into()).pan_x, 77.0, "B6: layout switch dropped g1's pan_x");
            assert_eq!(pane_view(&(&value).into()).pan_y, -33.0, "B6: layout switch dropped g1's pan_y");
            editor_host::close(&value);
        });
    }

    #[test]
    fn b11_pick_split_and_outline_apply_same_frame_and_keep_bar_action() {
        big_stack(|| {
            // 1. /x pick-split applies on the click frame itself (no queued lag).
            let mut value = model_of(
                r#"(workspace w
                    (graph g :context sop (sop/box))
                    (graph editor :context editor (ui/workspace (ui/graph "g" :focus true))))"#,
            );
            let mut tick = 0;
            step_model(&mut value, &mut tick, (400.0, 300.0), vec![]);
            step_model(&mut value, &mut tick, (400.0, 300.0), vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('x'))]);
            step_model(&mut value, &mut tick, (400.0, 300.0), vec![Event::KeyPressed(Key::KeyChar('4'))]);
            assert!(matches!(value.prompt, Some(Prompt::Splitting { .. })), "B11 setup: expected Prompt::Splitting");
            // Hover then click to commit the split: must land on THIS frame!
            step_model(&mut value, &mut tick, (400.0, 300.0), vec![Event::PointerMoved((400.0, 300.0))]);
            step_model(
                &mut value,
                &mut tick,
                (400.0, 300.0),
                vec![Event::PointerPressed(MouseButton::LeftButton, (400.0, 300.0)), Event::PointerReleased(MouseButton::LeftButton, (400.0, 300.0))],
            );
            let printed = workspace_doc::print(&value.doc.workspace.ws).unwrap().0;
            assert!(printed.contains("(ui/inspector)"), "B11: /x pick-split did not apply on the click frame: {printed}");
            assert!(value.queued.is_empty(), "B11: /x pick-split left a queued action: {:?}", value.queued);
            editor_host::close(&value);

            // 2. Graph toolbar Outline button applies on the click frame itself.
            let mut value = model_of(
                r#"(workspace o
                    (graph g :context value (let* [a 1.0] a))
                    (graph editor :context editor (ui/workspace (ui/graph "g" :focus true))))"#,
            );
            let mut tick = 0;
            step_model(&mut value, &mut tick, (200.0, 300.0), vec![]);
            step_model(&mut value, &mut tick, (200.0, 300.0), vec![]);
            let f0 = idle_frame();
            let geom = geometry(&(&value).into(), &value.workspace, &f0);
            let leaf = layout::find(&geom, &Panel::Graph).expect("graph leaf");
            let from = {
                let mut ui = value.ui.borrow_mut();
                chrome::tools_start(&mut ui, true, false, false, &panel_title(&(&value).into(), leaf))
            };
            let (ox, oy) = {
                let mut ui = value.ui.borrow_mut();
                let (x, y, w, h) = crate::bars::tool_rect(true, &mut ui, leaf.header, from, crate::bars::Tool::Outline).expect("Outline tool rect");
                (f64::from(x + w / 2.0), f64::from(y + h / 2.0))
            };
            step_model(&mut value, &mut tick, (ox, oy), vec![Event::PointerMoved((ox, oy))]);
            step_model(
                &mut value,
                &mut tick,
                (ox, oy),
                vec![Event::PointerPressed(MouseButton::LeftButton, (ox, oy)), Event::PointerReleased(MouseButton::LeftButton, (ox, oy))],
            );
            let printed = workspace_doc::print(&value.doc.workspace.ws).unwrap().0;
            assert!(printed.contains("(ui/outline)"), "B11: Outline toolbar click did not apply on the click frame: {printed}");
            assert!(value.queued.is_empty(), "B11: Outline toolbar click left a queued action: {:?}", value.queued);
            editor_host::close(&value);

            // 3. A bar_action is not dropped when a prompt_intent arrives on the same frame.
            let mut value = model_of(
                r#"(workspace w
                    (graph a :context sop (sop/box))
                    (graph b :context sop (sop/box))
                    (graph editor :context editor
                      (let* [l0 (ui/graph "a") l1 (ui/split_at "horizontal" 0.5 (ui/graph "a") (ui/inspector))]
                        (ui/workspace (ui/switch l0 l1 :active 0)))))"#,
            );
            let mut tick = 0;
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            let f = idle_frame();
            let input = crate::editor_reduce::idle_input(&(&value).into(), &[], &f);
            let mut res = crate::editor_reduce::idle_result(&(&value).into(), selection::empty());
            res.prompt_intent = Some(PromptIntent::Go("b".to_string()));
            res.bar_action = Some(Action::LayoutSwitch(1));
            reduce(None, None, input, &mut value, res);
            let printed = workspace_doc::print(&value.doc.workspace.ws).unwrap().0;
            assert_eq!(pane_shown(&(&value).into()).map(String::as_str), Some("b"), "B11: prompt_intent Go(\"b\") did not apply");
            assert!(printed.contains(":active 1"), "B11: bar_action LayoutSwitch(1) was dropped when prompt_intent arrived on the same frame: {printed}");
            editor_host::close(&value);
        });
    }

    #[test]
    fn b12_splitter_drag_updates_panes_on_same_frame() {
        big_stack(|| {
            let mut value = model_of(
                r#"(workspace w
                    (graph g :context sop (sop/box))
                    (graph editor :context editor
                      (ui/workspace (ui/split_at "horizontal" 0.5 (ui/graph "g") (ui/inspector)))))"#,
            );
            let mut tick = 0;
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            step_model(&mut value, &mut tick, (200.0, 200.0), vec![]);
            let f0 = idle_frame();
            let splitter = geometry(&(&value).into(), &value.workspace, &f0).splitters[0].clone();
            let sx = splitter.bounds.0 as f64;
            let sy = (splitter.bounds.1 + splitter.bounds.3 / 2) as f64;
            let before_w = pane_view(&(&value).into()).width;
            step_model(&mut value, &mut tick, (sx, sy), vec![Event::PointerMoved((sx, sy))]);
            step_model_buttons(&mut value, &mut tick, &[MouseButton::LeftButton], (sx, sy), vec![Event::PointerPressed(MouseButton::LeftButton, (sx, sy))]);
            // Drag 150 points right: the graph pane's drawn width must update on THIS frame, not lag by 1 frame.
            let target_x = sx + 150.0;
            step_model_buttons(&mut value, &mut tick, &[MouseButton::LeftButton], (target_x, sy), vec![Event::PointerMoved((target_x, sy))]);
            let dragged_w = pane_view(&(&value).into()).width;
            assert!(
                (dragged_w - (before_w + 150)).abs() <= 4,
                "B12: graph pane width lagged behind splitter drag on the drag frame (before={before_w}, dragged={dragged_w}, expected ~{})",
                before_w + 150
            );
            {
                let ui = value.ui.borrow();
                let seed = pxui::ui::key_of(pxui::ui::ROOT_KEY, "workspace-grips");
                let grip = ui.box_of_key(pxui::Key::Resolved(pxui::ui::key_of(seed, "workspace-grip"))).expect("splitter grip");
                assert!((ui.rect(&grip).x as f64 - target_x).abs() <= 1.0);
            }
            step_model(&mut value, &mut tick, (target_x, sy), vec![Event::PointerReleased(MouseButton::LeftButton, (target_x, sy))]);
            assert!(value.workspace.live.is_none(), "the release should commit the draft");
            assert_eq!(pane_view(&(&value).into()).width, dragged_w);
            editor_host::close(&value);
        });
    }

    #[test]
    fn b9_toggle_graph_sets_all_to_one_state() {
        big_stack(|| {
            // Two graph panels: g1 open, g2 collapsed -> /g must bring both to one state.
            let mut value = model_of(
                r#"(workspace w
                    (graph g :context sop (sop/box))
                    (graph editor :context editor
                      (let* [g1 (ui/graph "g") g2 (ui/graph "g")]
                        (ui/workspace (ui/split_at "horizontal" 0.5 g1 g2)))))"#,
            );
            let k2 = panel_key(&value.doc, &[1]);
            std::sync::Arc::make_mut(&mut value.doc).workspace.ws.layout.panels.insert(k2, panels::State { collapsed: true, window: None });
            let leaves = shell_leaves(&value, &value.workspace);
            let f = idle_frame();
            let intents = panel_intents(&(&value).into(), &leaves, &value.workspace, &f, &Panel::Graph, None, &[Action::ToggleGraph]);
            let toggled = |p: &[usize]| intents.iter().any(|i| matches!(i, chrome::Intent::Toggle(path) if path == p));
            let after0 = panel_state(&(&value).into(), &[0]).collapsed ^ toggled(&[0]);
            let after1 = panel_state(&(&value).into(), &[1]).collapsed ^ toggled(&[1]);
            assert_eq!(after0, after1, "B9: /g flipped two graph panels into opposite states ({after0} vs {after1})");
            assert!(after0, "B9: 1 open / 1 collapsed tie should collapse all");

            // Three graph panels: 1 open, 2 collapsed -> majority is collapsed, so /g expands all.
            let mut value3 = model_of(
                r#"(workspace w
                    (graph g :context sop (sop/box))
                    (graph editor :context editor
                      (let* [g1 (ui/graph "g") g2 (ui/graph "g") g3 (ui/graph "g")]
                        (ui/workspace (ui/split_at "horizontal" 0.5 g1 (ui/split_at "horizontal" 0.5 g2 g3))))))"#,
            );
            let k2 = panel_key(&value3.doc, &[1, 0]);
            let k3 = panel_key(&value3.doc, &[1, 1]);
            {
                let map = &mut std::sync::Arc::make_mut(&mut value3.doc).workspace.ws.layout.panels;
                map.insert(k2, panels::State { collapsed: true, window: None });
                map.insert(k3, panels::State { collapsed: true, window: None });
            }
            let leaves3 = shell_leaves(&value3, &value3.workspace);
            let intents3 = panel_intents(&(&value3).into(), &leaves3, &value3.workspace, &f, &Panel::Graph, None, &[Action::ToggleGraph]);
            let toggled3 = |p: &[usize]| intents3.iter().any(|i| matches!(i, chrome::Intent::Toggle(path) if path == p));
            let states: Vec<bool> = [vec![0], vec![1, 0], vec![1, 1]]
                .iter()
                .map(|p| panel_state(&(&value3).into(), p).collapsed ^ toggled3(p))
                .collect();
            assert_eq!(states, vec![false, false, false], "B9: 1 open / 2 collapsed should expand all to open");
            editor_host::close(&value);
            editor_host::close(&value3);
        });
    }
}
