//! L4 regressions: B14 Backspace and B15 a click inside a multi-selection.
use crate::support::*;
use flow_graph::flow_edit::Op;
use input::MouseButton;
use rays_graph::scope_pane as scope;
use rays_graph::scope_state::{Change, Command, ScopeOpts};

const TEXT: &str = "(workspace m (graph g :context sop (let* [a (sop/box) b (sop/box) m (sop/merge a b)] m)))";

fn b14_backspace_deletes_the_selection_not_the_hovered_wire() {
    let mut ui = new_ui();
    let (view, _) = g::scope_view(&checked(TEXT), "g");
    let view = g::settled(&mut ui, view);
    let (a, m) = (path(&["g", "a"]), path(&["g", "m"]));
    let mut view = view;
    scope::select(vec![a.clone()], &mut view);
    let (rx, ry) = scope::inspect::row_center(&view, &m, 1).expect("m's first input row");
    let at = (rx as i64, ry as i64);
    let (view, _) = scope_step(&mut ui, view, &g::frame_ev(at, vec![mouse_move(at)]));
    let mut changes = Vec::new();
    let mut view = view;
    scope::run_command(&mut view, Command::Delete, &mut changes);
    check(changes == vec![g::syntax(Op::DeleteNodes { nodes: vec![a] })], "Backspace did not delete the selected node");
}

fn b15_click_in_a_multi_selection_narrows_it_on_release() {
    let mut ui = new_ui();
    let (view, _) = g::scope_view(&checked(TEXT), "g");
    let mut view = g::settled(&mut ui, view);
    let (a, b) = (path(&["g", "a"]), path(&["g", "b"]));
    scope::select(vec![a.clone(), b.clone()], &mut view);
    let (x, y, w, h) = g::center_of(&view, &a);
    let at = ((x + w / 2.0) as i64, (y + h / 2.0) as i64);
    let (view, _) = scope_step(&mut ui, view, &g::frame(at, Vec::new(), vec![mouse_move(at), mouse_press(MouseButton::LeftButton, at)]));
    check(scope::selected(&view).len() == 2, "the press alone narrowed the selection (a drag would lose the others)");
    let (view, changes) = scope_step(&mut ui, view, &g::frame(at, Vec::new(), vec![mouse_release(MouseButton::LeftButton, at)]));
    check(scope::selected(&view) == vec![a] && changes.contains(&Change::Selected), "the release did not narrow the selection to the clicked card");
}

fn b16_a_relaid_pane_has_no_hovered_row_for_a_command() {
    let mut ui = new_ui();
    let (view, _) = g::scope_view(&checked(TEXT), "g");
    let view = g::settled(&mut ui, view);
    let mut view = view;
    // the pointer rested on m's first input row in the frame before the geometry moved
    view.hovered_at = Some((*view.geo.slot.get(&path(&["g", "m"])).expect("m's slot"), 1));
    check(scope::hovered_row(&view).is_some(), "the fixture did not hover a row");
    scope::relayout(Some(true), &mut view);
    let mut changes = Vec::new();
    scope::run_command(&mut view, Command::Delete, &mut changes);
    check(changes.is_empty(), "Backspace acted on a row hovered before the geometry moved");
}

fn b19_back_to_a_graph_restores_its_camera() {
    let mut ui = new_ui();
    let w = checked("(workspace w (graph g :context sop (let* [a (sop/box)] a)) (graph h :context sop (let* [b (sop/box)] b)))");
    let (mut view, _) = g::scope_view(&w, "g");
    view = g::settled(&mut ui, view);
    (view.pan_x, view.pan_y, view.zoom) = (77.0, 55.0, 0.5);
    for name in ["h", "g"] {
        let sc = std::rc::Rc::new(flow_graph::projection::of_graph(&scope_catalog(), &w, name).expect("graph"));
        scope::with_scope(ScopeOpts::default(), name, sc, &mut view);
        view = g::settled(&mut ui, view);
    }
    check((view.pan_x, view.pan_y, view.zoom) == (77.0, 55.0, 0.5), "returning to a graph lost its pan and zoom");
}

fn b20_the_text_width_cache_is_bounded() {
    let mut ui = new_ui();
    let (view, _) = g::scope_view(&checked(TEXT), "g");
    let view = g::settled(&mut ui, view);
    for i in 0..9000u64 {
        view.measured.borrow_mut().insert((11, i), 1.0);
    }
    let (view, _) = scope_step(&mut ui, view, &g::frame0());
    let n = view.measured.borrow().len();
    check(n <= 4096, &format!("the width cache kept {n} entries"));
}

pub const CASES: &[(&str, fn())] = &[
    ("b14_backspace_deletes_the_selection_not_the_hovered_wire", b14_backspace_deletes_the_selection_not_the_hovered_wire),
    ("b16_a_relaid_pane_has_no_hovered_row_for_a_command", b16_a_relaid_pane_has_no_hovered_row_for_a_command),
    ("b19_back_to_a_graph_restores_its_camera", b19_back_to_a_graph_restores_its_camera),
    ("b20_the_text_width_cache_is_bounded", b20_the_text_width_cache_is_bounded),
    ("b15_click_in_a_multi_selection_narrows_it_on_release", b15_click_in_a_multi_selection_narrows_it_on_release),
];
