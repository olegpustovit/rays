//! The editor-driven sections of lib/rays_editor/test/test_text_pane.ml (those that need the whole
//! `Editor`). Ported here: editor_graph_drafts and editor_fn_frames. The others (editor_text,
//! editor_binding, editor_w9, editor_active_scrub, editor_command_click, editor_literal_scrub,
//! editor_text_drop, editor_text_find) lower the 240-seed sunflower/rosette sketches and drive
//! pixel-exact clicks; see the report.
use super::t3c::*;
use input::{Event, Key, MouseButton};
use rays_editor::editor;

fn check(condition: bool, message: &str) {
    if !condition {
        panic!("test_text_pane: {message}");
    }
}

/// The centre of a tab in the header over a text pane's body: the tabs end at `right`, 12 points
/// apart, in the test face's 7-point glyphs.
pub fn tab_at(narrow: bool, right: i64, top: i64, index: usize) -> (f64, f64) {
    let offsets: [i64; 3] = if narrow { [66, 127, 168] } else { [31, 92, 150] };
    ((right - 178 + offsets[index]) as f64, (top - 12) as f64)
}

fn start_env(text: &str) -> Driver {
    let presets = scratch_dir("rays-text-presets");
    let ui_font = std::env::var("RAYS_UI_FONT").ok();
    Driver::new(create(editor::Create { domains: Some(1), presets: Some(presets), ui_font, ..options() }, &load(text)))
}

pub fn graph_drafts() {
    let text = r#"(workspace drafts
  (graph a :context sop (sop/box))
  (graph b :context sop (sop/merge (sop/box)))
  (graph scene :context scene (scene/merge (scene/geometry (ref b))))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))"#;
    let mut d = start_env(text);
    let mouse = std::cell::Cell::new((450., 320.));
    let step = |d: &mut Driver, keys: &[Key], events: Vec<Event>| d.step_full(&[], keys, mouse.get(), events, 1);
    let at = |d: &mut Driver, point: (f64, f64), events: Vec<Event>| {
        mouse.set(point);
        step(d, &[], events)
    };
    let jump = |d: &mut Driver, name: &str| {
        step(d, &[], vec![key(Key::Escape)]);
        step(d, &[], vec![key(Key::KeyChar('/')), ch('j')]);
        step(d, &[], vec![Event::Text_input(name.into())]);
        step(d, &[], vec![key(Key::Enter)]);
        step(d, &[], vec![]);
    };
    let summary = |d: &Driver| dump_line(d.e(), "text");
    for _ in 0..4 {
        step(&mut d, &[], vec![]);
    }
    let (gx, gy, gw, gh) = editor::panes(d.e(), &frame_at((450., 320.), vec![], 0)).graph;
    let top = gy + gh + 1 + 28;
    let click = |d: &mut Driver, point: (f64, f64)| {
        at(d, point, vec![Event::Pointer_moved(point)]);
        at(d, point, vec![Event::Pointer_pressed(MouseButton::LeftButton, point), Event::Pointer_released(MouseButton::LeftButton, point)]);
        step(d, &[], vec![]);
    };
    let area = (gx as f64 + 120., top as f64 + 40.);
    let draft_in = |d: &mut Driver, name: &str| {
        jump(d, name);
        click(d, tab_at(gw < 400, gx + gw - 36, top, 1));
        click(d, area);
        step(d, &[Key::Meta], vec![ch('a')]);
        step(d, &[], vec![Event::Text_input(format!("(sop/box) ; {name}"))]);
    };
    draft_in(&mut d, "a");
    check(has(&summary(&d), "graph drafts: a"), &format!("a draft was not kept: {}", summary(&d)));
    draft_in(&mut d, "b");
    check(has(&summary(&d), "graph drafts: a b"), &format!("the second graph's draft replaced the first's: {}", summary(&d)));
    jump(&mut d, "a");
    check(has(&summary(&d), "graph drafts: a b"), &format!("leaving a graph dropped its draft: {}", summary(&d)));
    d.close();
}

/// Every fn frame starts open, its body in view, however many the graph has (c folds one).
pub fn fn_frames() {
    let height = |maps: &str| -> i64 {
        let text = format!(
            r#"(workspace fz
  (graph g :context value (let* [{maps}] 1.0))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))"#
        );
        let mut d = start_env(&text);
        for _ in 0..4 {
            d.step_at((450., 320.), vec![]);
        }
        d.step_at((450., 320.), vec![Event::Key_pressed(Key::Escape)]);
        d.step_at((450., 320.), vec![Event::Key_pressed(Key::KeyChar('/')), Event::Key_pressed(Key::KeyChar('j'))]);
        d.step_at((450., 320.), vec![Event::Text_input("g".into())]);
        d.step_at((450., 320.), vec![Event::Key_pressed(Key::Enter)]);
        for _ in 0..4 {
            d.step_at((450., 320.), vec![]);
        }
        let box_ = editor::node_box(d.e(), &["g".into(), "a".into()]);
        d.close();
        box_.map(|(_, _, _, h)| h).unwrap_or_else(|| panic!("the fn frame has no box"))
    };
    let one = height("a (fn [k] (* k 2.0))");
    let two = height("a (fn [k] (* k 2.0)) b (fn [k] (+ k 1.0))");
    check(two == one, &format!("a second fn frame folded the first ({one}, {two})"));
}
