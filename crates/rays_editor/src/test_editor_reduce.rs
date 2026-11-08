//! Checks of bundle E that need a model but not the whole Editor: the leader actions and the
//! routed keymap (`editor_actions`), the reducer (`editor_reduce`: gestures, history, undo,
//! prompts, layout) and the frame's helpers (`editor_frame`).
use std::rc::Rc;
use std::sync::Arc;

use editor_core::panels::Panel;
use editor_core::router;
use editor_document::document::Level;
use editor_document::workspace_doc::{self, TextOptions};
use flow_graph::flow_edit::Op;
use flow_syntax::syntax;
use input::{Event, MouseButton};
use param::Value;
use pxui_graph::scope_state as scope;
use rays::frame;

use crate::cook;
use crate::editor_actions::{apply_action, in_world, intent_label, routed, world_keys};
use crate::editor_commands::{self, action, action_of};
use crate::editor_frame;
use crate::editor_history as history;
use crate::editor_model::*;
use crate::editor_reduce::{self, gesture_key, idle_input, idle_result, reduce, reduce_idle, Frame_input};
use crate::editor_setup::{self, create_opts};
use crate::selection;
use crate::timeline;

const TEXT: &str = "(workspace w (graph g :context sop (sop/box)))";

fn model() -> t {
    let catalog = editor_document::kinds::catalog(flow_sop::manifest::VERSION, &sop::nodes::factories()).expect("catalog");
    let ws = workspace_doc::of_text(&TextOptions::default(), &catalog, TEXT).expect("workspace");
    let opts = create_opts {
        preferences: Some(std::env::temp_dir().join("rays-editor-test-no-preferences.rays").to_string_lossy().to_string()),
        presets: Some(std::env::temp_dir().join("rays-editor-test-presets").to_string_lossy().to_string()),
        ui_text_size: 11,
        await_: Some(true),
        ..create_opts::default()
    };
    editor_setup::create(opts, &ws, Rc::new(cook::prepare)).expect("model")
}

fn frame_at(time: f64, buttons: Vec<MouseButton>, events: Vec<Event>) -> frame::t {
    let mut input = input::at_time(time);
    input.size = (800, 600);
    input.dt = 1.0 / 60.0;
    input.buttons = buttons;
    input.events = events;
    frame::t { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// The debug build's deep recursion wants a big stack (as `test_editor_model` does).
fn big_stack(body: fn()) {
    std::thread::Builder::new().stack_size(256 << 20).spawn(body).unwrap().join().unwrap();
}

#[test]
fn actions_routing_and_labels() {
    big_stack(|| {
        let value = model();
        // not in the World: its keys are not routed; no layouts: the digit commands are left out
        assert!(!in_world(&value));
        let keymap = routed(&value);
        assert!(keymap.iter().all(|c| !matches!(action_of(c), action::World_emit | action::World_reseed | action::World_time(_) | action::World_play | action::World_preset(_) | action::Layout_switch(_))));
        // the graph panel is in the default tree: the node menu key is routed, and so is frame-camera
        assert!(keymap.iter().any(|c| matches!(action_of(c), action::Add_node)));
        assert!(keymap.iter().any(|c| matches!(action_of(c), action::Frame_camera)));

        // undo labels of graph intents
        assert_eq!(intent_label(&change::Select_layout("x".into())).as_deref(), Some("Switch layout"));
        assert_eq!(intent_label(&change::Set_parameter { node: 1, path: "size".into(), value: Value::Float(1.0) }).as_deref(), Some("Set size"));
        assert_eq!(intent_label(&change::Pin_row { node: vec![], label: "a".into(), pin: Some(false) }).as_deref(), Some("Unpin row from card"));
        assert_eq!(intent_label(&change::Pin_row { node: vec![], label: "a".into(), pin: None }).as_deref(), Some("Pin row to card"));
        assert_eq!(intent_label(&change::Notice("x".into())), None);

        // leader actions: restore layout flips, open-camera clears the selection and shows the
        // inspector, play/pause steps the timeline
        let state = (value.workspace.clone(), selection::select(3), value.tree.clone(), value.timeline.clone(), Vec::new());
        let (workspace, selection, _, _, _) = apply_action(&value, state.clone(), &action::Restore_layout);
        assert!(workspace.restored);
        assert_eq!(workspace.hidden, vec![Panel::Timeline]);
        let again = apply_action(&value, (workspace, selection, value.tree.clone(), value.timeline.clone(), Vec::new()), &action::Restore_layout);
        assert!(!again.0.restored);
        let (_, selection, _, _, _) = apply_action(&value, state.clone(), &action::Open_camera);
        assert!(selection::selected(&selection).is_none());
        let (_, _, _, stepped, _) = apply_action(&value, state.clone(), &action::Play_pause);
        assert_ne!(timeline::mode(&stepped), timeline::mode(&value.timeline));

        // World keys outside a World edit nothing
        let (doc, label, edits) = world_keys(&value, &value.doc, &selection::empty(), &[action::World_emit]);
        assert!(label.is_none() && edits.is_empty());
        assert!(editor_document::document::scene_graph(&doc).len() == editor_document::document::scene_graph(&value.doc).len());
        editor_host_close(&value);
    });
}

fn editor_host_close(value: &t) {
    crate::editor_host::close(value);
}

#[test]
fn reducer_idle_undo_and_gestures() {
    big_stack(|| {
        let value = model();
        let frame0 = frame_at(0.0, vec![], vec![]);
        // an idle reduction changes nothing: same document, no history entry, no notice
        prime(&value, &frame0);
        let idle = reduce_idle(None, None, &value, &[], &frame0).expect("idle");
        assert!(Arc::ptr_eq(&idle.doc, &value.doc));
        assert_eq!(history::depth(&idle.history), 0);
        assert!(idle.notice.is_none());
        // undo and redo with nothing to step say so
        let undone = reduce_idle(None, None, &value, &[action::Undo], &frame0).expect("undo");
        assert_eq!(undone.notice, Some((notice_kind::Refusal, "Undo: nothing to undo".to_string())));
        let redone = reduce_idle(None, None, &value, &[action::Redo], &frame0).expect("redo");
        assert_eq!(redone.notice, Some((notice_kind::Refusal, "Redo: nothing to redo".to_string())));

        // a syntax gesture is written to the text, one history entry named by the op
        let expr = syntax::parse("(sop/box)").expect("parse").remove(0);
        let op = Op::AddNode { scope: vec!["g".to_string()], name: "second".to_string(), expr };
        let mut result = idle_result(&value, selection::empty());
        result.changes = vec![change::Syntax_edit(op)];
        let out = reduce(None, None, idle_input(&value, &[], &frame0), &value, result);
        assert!(!Arc::ptr_eq(&out.core.doc, &value.doc));
        assert_eq!(history::depth(&out.core.history), 1);
        assert_eq!(history::label(&out.core.history), "Add node");
        assert!(out.core.doc.workspace.ws.source.len() == value.doc.workspace.ws.source.len());
        assert!(editor_document::preset::text(&out.core.doc).unwrap().contains("second"));
        assert!(out.effects.cook);
        assert!(out.core.edit_error.is_none());
        let wrote = out.core.notice.clone().expect("a note of what was written");
        assert_eq!(wrote.0, notice_kind::Info);
        assert!(wrote.1.starts_with("Wrote "), "{}", wrote.1);

        // undo restores the text
        let back = reduce_idle(None, None, &out.core, &[action::Undo], &frame0).expect("undo");
        assert!(!editor_document::preset::text(&back.doc).unwrap().contains("second"));
        assert_eq!(history::depth(&back.history), 0);
        assert_eq!(back.notice, Some((notice_kind::Info, "Undo Add node".to_string())));
        // and redo brings it back
        let forth = reduce_idle(None, None, &back, &[action::Redo], &frame0).expect("redo");
        assert!(editor_document::preset::text(&forth.doc).unwrap().contains("second"));

        // a refused gesture leaves the document and says why
        let mut result = idle_result(&value, selection::empty());
        result.changes = vec![change::Syntax_edit(Op::RemoveGraph { name: "missing".to_string() })];
        let refused = reduce(None, None, idle_input(&value, &[], &frame0), &value, result);
        assert!(Arc::ptr_eq(&refused.core.doc, &value.doc));
        assert!(refused.core.edit_error.is_some());
        editor_host_close(&value);
    });
}

#[test]
fn reducer_gesture_keys_layout_and_prompts() {
    big_stack(|| {
        let value = model();
        let held = frame_at(0.0, vec![MouseButton::LeftButton], vec![]);
        let free = frame_at(0.0, vec![], vec![]);
        let mut result = idle_result(&value, selection::empty());
        // a scrub is one gesture key while the button is down, none when it is not
        result.changes = vec![change::Set_parameter { node: 4, path: "size".to_string(), value: Value::Float(2.0) }];
        assert_eq!(gesture_key(&value, &held, false, false, &None, &result).as_deref(), Some("graph.scrub:scene:4:size"));
        assert_eq!(gesture_key(&value, &free, false, false, &None, &result), None);
        assert_eq!(gesture_key(&value, &held, true, false, &None, &result), None);
        // a settings stroke without a graph change is keyed by its names
        result.changes = Vec::new();
        result.settings_changes = vec![("fov".to_string(), Value::Float(1.0))];
        assert_eq!(gesture_key(&value, &held, false, false, &None, &result).as_deref(), Some("settings/scene/-1/fov"));

        // a moved card is a layout edit with the label of the gesture
        let mut result = idle_result(&value, selection::empty());
        result.scope_changes = vec![scope::Change::Moved(vec![(vec!["g".to_string(), "a".to_string()], 10.0, 20.0)])];
        let (next, result, _) = editor_reduce::layout_gestures(&value, false, &[], (*value.doc).clone(), result.clone(), &result.scope_changes.clone());
        assert_eq!(result.label, "Move");
        assert_eq!(next.workspace.ws.layout.at.get(&vec!["g".to_string(), "a".to_string()]), Some(&(10.0, 20.0)));
        // a zone collapsed is stored, a probe is view state only
        let mut result = idle_result(&value, selection::empty());
        result.scope_changes = vec![scope::Change::Zone_collapsed { zone: vec!["g".to_string(), "z".to_string()], collapsed: true }, scope::Change::Probe_set { zone: vec!["g".to_string(), "z".to_string()], index: 3 }];
        let (next, result, probes) = editor_reduce::layout_gestures(&value, false, &[], (*value.doc).clone(), result.clone(), &result.scope_changes.clone());
        assert_eq!(result.label, "Collapse zone");
        assert_eq!(next.workspace.ws.layout.collapsed.get(&vec!["g".to_string(), "z".to_string()]), Some(&true));
        assert_eq!(probes.get(&vec!["g".to_string(), "z".to_string()]), Some(&3));

        // prompts: saving a preset writes the file and says so; loading a missing one is refused
        let dir = std::env::temp_dir().join(format!("rays-editor-test-presets-{}", std::process::id()));
        let mut value = value;
        value.presets = dir.to_string_lossy().to_string();
        let view = |_: Option<&Panel_result>| syntax::make(syntax::Draft::Map(Vec::new()));
        let mut result = idle_result(&value, selection::empty());
        result.prompt_intent = Some(prompt_intent::Save_preset_file("probe".to_string()));
        let (_, notice, loaded) = editor_reduce::prompt_step(&value, &view, &result);
        assert!(loaded.is_none());
        let (kind, text) = notice.expect("a notice");
        assert_eq!(kind, notice_kind::Info);
        assert!(text.starts_with("Saved preset probe"), "{text}");
        result.prompt_intent = Some(prompt_intent::Load_preset_file("probe".to_string()));
        let (_, notice, loaded) = editor_reduce::prompt_step(&value, &view, &result);
        assert!(loaded.is_some(), "{:?}", notice);
        assert_eq!(notice, Some((notice_kind::Info, "Loaded preset probe".to_string())));
        result.prompt_intent = Some(prompt_intent::Load_preset_file("nothing-here".to_string()));
        let (_, notice, loaded) = editor_reduce::prompt_step(&value, &view, &result);
        assert!(loaded.is_none());
        assert_eq!(notice.expect("refusal").0, notice_kind::Refusal);
        let _ = std::fs::remove_dir_all(&dir);
        editor_host_close(&value);
    });
}

#[test]
fn frame_helpers() {
    big_stack(|| {
        // the browse list's time: UTC month, day, hour, minute
        assert_eq!(editor_frame::month_day_hour_minute(0.0), (1, 1, 0, 0));
        assert_eq!(editor_frame::month_day_hour_minute(86400.0 * 59.0 + 3661.0), (3, 1, 1, 1));
        let value = model();
        // fps is sampled once a second
        let mut value = value;
        value.status_fps_at = 0.0;
        let early = frame_at(0.5, vec![], vec![]);
        assert_eq!(editor_frame::fps_sample(&value, &early), (value.status_fps, value.status_fps_at));
        let later = frame_at(1.5, vec![], vec![]);
        assert_eq!(editor_frame::fps_sample(&value, &later), (Some(60), 1.5));
        // a hidden pane set leaves the list and frame-camera keys out
        let all = editor_commands::keymap().to_vec();
        let live = editor_frame::live_keymap(false, std::rc::Rc::new(all.clone()));
        assert!(live.len() < all.len());
        assert!(live.iter().all(|c| !matches!(action_of(c), action::List_command(_) | action::Frame_camera)));
        assert_eq!(editor_frame::live_keymap(true, std::rc::Rc::new(all.clone())).len(), all.len());
        // no carry: no marks
        assert_eq!(editor_frame::carry_marks(&value), (Vec::new(), None));
        // the hud shows the leader's pending prefix
        let pending = router::State::Pending("o".to_string());
        let hud = editor_frame::hud_of(&value, &pending, &early, &[]).expect("hud");
        assert_eq!(hud.0, "/ o \u{b7} leader");
        assert_eq!(hud.1, 2.0);
        // Level is still the scene
        assert_eq!(pane_level(&value), Level::Scene);
        // the reducer's idle input mirrors the model
        let input: Frame_input = idle_input(&value, &[action::Undo], &early);
        assert_eq!(input.actions, vec![action::Undo]);
        assert!(!input.carrying);
        editor_host_close(&value);
    });
}

/// `Ui.input` needs a completed PXUI frame: an empty one stands for the panes.
fn prime(value: &t, frame: &frame::t) {
    value.ui.borrow_mut().frame(frame, &mut |_| {});
}

#[test]
fn whole_frame_update() {
    big_stack(|| {
        let value = model();
        let frame0 = frame_at(0.0, vec![], vec![]);
        let camera = |_: &mut pxui::ui::Ui| -> Panel_result { Rc::new(()) };
        let handles = |_: &mut pxui::ui::Ui, _: Option<&picked>, _: rays_math::mat4::T, _: pxui_shell::layout::Bounds| (Vec::new(), false, None);
        let view_state: Rc<dyn Fn(Option<&Panel_result>) -> syntax::Syntax> = Rc::new(|_| syntax::make(syntax::Draft::Map(Vec::new())));
        // the panes hidden: only the keys and the reducer run
        let out = editor_frame::update(value.clone(), false, false, &camera, &handles, None, None, view_state.clone(), &frame0, None, None, Vec::new(), None);
        assert!(Arc::ptr_eq(&out.core.doc, &value.doc));
        assert!(out.actions.is_empty());
        assert!(out.core.pane_keys.is_empty());
        // every pane drawn headless: the shell's panels get roots, the focus stays, nothing edits
        let frame1 = frame_at(1.0 / 60.0, vec![], vec![]);
        let out = editor_frame::update(out.core, true, false, &camera, &handles, None, None, view_state, &frame1, None, None, Vec::new(), None);
        assert!(Arc::ptr_eq(&out.core.doc, &value.doc));
        assert!(!out.core.pane_keys.is_empty());
        assert_eq!(out.core.focus, value.focus);
        assert_eq!(history::depth(&out.core.history), 0);
        assert!(out.core.prompt.is_none());
        editor_host_close(&value);
    });
}

#[test]
fn inactive_text_intents_reuse_the_owned_model() {
    big_stack(|| {
        use crate::{editor_panes, editor_text, text_pane};
        let mut value = model();
        value.preferences = "owned model identity".repeat(100);
        let identity = value.preferences.as_ptr();
        value.text.draft = Some("active draft".into());
        value.text.target = Some(vec!["g".into()]);
        let key = vec!["other-text".into()];
        let doc = value.doc.clone();
        let value = editor_text::apply_text_at(value, &key, &[text_pane::Intent::DocDraft("other draft".into())]);
        assert_eq!(value.preferences.as_ptr(), identity, "text intents copied the whole owned model");
        assert!(Arc::ptr_eq(&value.doc, &doc));
        assert_eq!(value.text.draft.as_deref(), Some("active draft"));
        assert_eq!(value.text.target, Some(vec!["g".into()]));
        let local = editor_panes::local_of(&value.locals, &key);
        assert_eq!(local.code.draft.as_deref(), Some("other draft"));
        assert!(local.code.doc_base.is_some());
        let value = editor_text::apply_text_at(value, &key, &[text_pane::Intent::DocApply("(workspace broken".into())]);
        assert!(Arc::ptr_eq(&value.doc, &doc));
        assert_eq!(value.text.draft.as_deref(), Some("active draft"));
        let local = editor_panes::local_of(&value.locals, &key);
        assert_eq!(local.code.draft.as_deref(), Some("(workspace broken"));
        assert!(!local.code.doc_errors.found.is_empty());
        let value = editor_text::apply_text_at(value, &key, &[text_pane::Intent::DocDiscard]);
        assert_eq!(value.preferences.as_ptr(), identity);
        assert!(editor_panes::local_of(&value.locals, &key).code.draft.is_none());
        assert_eq!(value.text.draft.as_deref(), Some("active draft"));
        editor_host_close(&value);
    });
}
