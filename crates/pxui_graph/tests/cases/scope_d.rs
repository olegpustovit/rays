//! Conditional arms, iteration selection, macro lens and fixture counts have independently budgeted gesture cases.
use flow_syntax::symbol::Symbol;
use crate::support::*;
use flow_eval::Value;
use flow_graph::edit_tree::ArgKey;
use flow_graph::flow_edit::Op;
use flow_graph::probe::{self, Execution};
use flow_graph::projection as p;
use input::{Event, MouseButton};
use pxui::ui::Ui;
use pxui_graph::scope_pane as scope;
use pxui_graph::scope_state::{Change, Command, Pane, ScopeOpts};
use std::cell::RefCell;
use std::rc::Rc;

fn ipt(q: (f64, f64)) -> (i64, i64) {
    (q.0 as i64, q.1 as i64)
}

fn right_click(at: (i64, i64)) -> Vec<Event> {
    vec![mouse_move(at), mouse_press(MouseButton::RightButton, at), mouse_release(MouseButton::RightButton, at)]
}

type Inputs = Vec<(String, Vec<(Symbol, Value)>)>;

/// `scope_view ?inputs ?probe ?at ?collapsed workspace graph` with the inputs of a recording.
fn view_with(opts: ScopeOpts, inputs: Option<Inputs>, w: &flow_check::workspace::Workspace, graph: &str) -> (Pane, Rc<p::Scope>) {
    let sc = Rc::new(p::of_graph(&scope_catalog(), w, graph).expect("graph"));
    let eval = flow_eval::r#static(w, true, inputs.as_deref()).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let rec = Rc::new(RefCell::new(probe::make(&eval, probe::Options::default())));
    let view = { let mut r8_view = scope::create(Some(1000), Some(700)); scope::with_scope(opts, graph, sc.clone(), &mut r8_view); r8_view };
    ({ let mut r8_view = view; scope::with_records(rec, &mut r8_view); r8_view }, sc)
}

fn conditional_arms() {
    let mut ui = new_ui();
    let conditional = checked("(workspace branches (graph g :context value (let* [x (cond false (+ 1 2) true (* 3 4) :else 5)] x)))");
    // the arm's formula opened, so it is a card of its own
    let sc = Rc::new(p::of_graph_with(&|_| false, &scope_catalog(), &conditional, "g").expect("g"));
    let view = { let mut r8_view = scope::create(Some(1000), Some(700)); scope::with_scope(ScopeOpts::default(), "g", sc.clone(), &mut r8_view); r8_view };
    let view = { let mut r8_view = view; scope::with_records(recorded(&conditional), &mut r8_view); r8_view };
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let arm = path(&["g", "x#then~2"]);
    let x = path(&["g", "x"]);
    check(scope::inspect::selector(&view, &arm).is_none(), "conditional arm incorrectly has an iteration selector");
    let (_, changes) = { let mut r8_view = { let mut r8_view = view.clone(); scope::select(vec![arm.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Add_arm, &mut r8_changes); (r8_view, r8_changes) };
    check(changes.contains(&Change::Syntax_edit(Op::AddArm { node: x.clone(), after: 1 })), "arm Add uses wrong holder/index");
    let (_, changes) = { let mut r8_view = { let mut r8_view = view.clone(); scope::select(vec![arm.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Delete_arm, &mut r8_changes); (r8_view, r8_changes) };
    check(changes.contains(&Change::Syntax_edit(Op::DeleteArm { node: x.clone(), index: 1 })), "arm Delete uses wrong index");
    let (_, changes) = { let mut r8_view = { let mut r8_view = view.clone(); scope::select(vec![arm.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Unfold, &mut r8_changes); (r8_view, r8_changes) };
    check(changes.contains(&Change::Syntax_edit(Op::Unfold { node: x.clone(), key: ArgKey::Arm(1), sub: Vec::new() })), "arm unfold did not address conditional body");
    let (view, _) = scope_step(&mut ui, scope_view(ScopeOpts::default(), &conditional, "g").0, &frame0());
    let (bx, by, _, _) = scope::inspect::box_of(&view, &x).expect("box");
    let at = (bx as i64 + 20, by as i64 + 8);
    let (menu, _) = scope_step(&mut ui, view.clone(), &frame_ev(at, right_click(at)));
    let (menu, _) = scope_step(&mut ui, menu, &frame_ev(at, Vec::new()));
    let add_arm = Change::Syntax_edit(Op::AddArm { node: x.clone(), after: -1 });
    let mut found = false;
    for row in 0..20 {
        let point = (at.0 + 30, at.1 + 3 + (24 * row) + 12);
        if scope_step(&mut ui, menu.clone(), &frame_ev(point, click(point))).1.contains(&add_arm) {
            found = true;
            break;
        }
    }
    check(found, "conditional right-click Add arm did not emit edit");
    let imported = { let mut r8_view = view; scope::with_scope(saved_opts(TestSaved { imported: Some("lib.rays".to_owned()), ..Default::default() }), "import", Rc::new(p::of_graph(&scope_catalog(), &conditional, "g").expect("g")), &mut r8_view); r8_view };
    let (imported, _) = scope_step(&mut ui, imported, &frame0());
    let (menu, _) = scope_step(&mut ui, imported, &frame_ev(at, right_click(at)));
    let (menu, _) = scope_step(&mut ui, menu, &frame_ev(at, Vec::new()));
    let point = (at.0 + 30, at.1 + 15);
    let (_, changes) = scope_step(&mut ui, menu, &frame_ev(point, click(point)));
    check(changes.contains(&Change::Open_import("lib.rays".to_owned())), "import context menu did not emit Open file");
}

fn fixture_counts() {
    let mut ui = new_ui();
    // every fixture's graphs draw: the pane's counts are the projection's
    for (name, graph, nodes, zones, rows) in [
        ("bloom", "flower", 13, 1, 86),
        ("sunflower", "sunflower", 9, 1, 36),
        ("orrery", "orrery", 21, 1, 117),
        ("facade", "facade", 18, 2, 97),
        ("kit", "kit", 17, 2, 114),
        ("tree", "tree", 9, 1, 184),
        ("garland", "garland", 23, 5, 98),
        ("wave", "wave", 7, 2, 46),
        ("tiles", "tiles", 17, 1, 84),
    ] {
        let w = load_workspace(name);
        let (view, _) = scope_view(ScopeOpts::default(), &w, graph);
        let (view, _) = scope_step(&mut ui, view, &frame0());
        let s = scope::stats(&view);
        check(s.nodes == nodes && s.zones == zones && s.rows == rows, &format!("{}/{}: pane counts {}/{}/{}", name, graph, s.nodes, s.zones, s.rows));
        check(s.drawn_items > 0 && s.drawn_zones <= s.zones, &format!("{}: nothing drawn", name));
    }
}

fn selector_steps() {
    let mut ui = new_ui();
    // the iteration selector: buttons and track are hit-tested boxes
    let w = load_workspace("sunflower");
    let zone = path(&["sunflower", "seeds_each"]);
    let (view, _) = scope_view(saved_opts(TestSaved { probe: vec![(zone.clone(), 5)], ..Default::default() }), &w, "sunflower");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let (prev, track, next) = scope::inspect::selector(&view, &zone).expect("selector");
    let (_, changes) = scope_click(&mut ui, view.clone(), rect_center(next));
    check(changes.contains(&Change::Probe_set { zone: zone.clone(), index: 6 }), "the next button did not step the probe");
    let (_, changes) = scope_click(&mut ui, view.clone(), rect_center(prev));
    check(changes.contains(&Change::Probe_set { zone: zone.clone(), index: 4 }), "the previous button did not step the probe");
    // one stepper pins the iteration of every zone when nothing is selected
    let (_, pinned) = { let mut r8_view = view.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Probe_step(1), &mut r8_changes); (r8_view, r8_changes) };
    check(pinned.contains(&Change::Probe_set { zone: zone.clone(), index: 6 }), "[ ] with nothing selected did not step every zone");
    let inline = checked("(workspace w (graph g :context value (let* [ys (map (fn [(x : float)] (+ x 2.0)) (array/float 10003 1.0))] (array/count ys))))");
    let (inline_view, _) = scope_view(saved_opts(TestSaved { probe: vec![(path(&["g", "ys#0"]), 9999)], ..Default::default() }), &inline, "g");
    let (inline_view, _) = scope_step(&mut ui, inline_view, &frame0());
    let inline_zone = path(&["g", "ys#0"]);
    let (_, inline_keys) = { let mut r8_view = { let mut r8_view = inline_view.clone(); scope::select(vec![inline_zone.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Probe_step(1), &mut r8_changes); (r8_view, r8_changes) };
    check(inline_keys.contains(&Change::Probe_set { zone: inline_zone.clone(), index: 10000 }), "inline function keyboard selector did not step beyond the recording cap");
    let (_, _, inline_next) = scope::inspect::selector(&inline_view, &inline_zone).expect("selector");
    let (ix, iy, iw, ih) = inline_next;
    check(ix >= 0.0 && iy >= 0.0 && ix + iw <= 1000.0 && iy + ih <= 700.0, &format!("inline selector outside viewport: {:.1} {:.1} {:.1} {:.1}", ix, iy, iw, ih));
    let (_, changes) = scope_click(&mut ui, inline_view, rect_center(inline_next));
    check(
        changes.contains(&Change::Probe_set { zone: inline_zone, index: 10000 }),
        &format!("an inline function's selector did not step beyond the recording cap: {:.1} {:.1} {:.1} {:.1} {:?}", ix, iy, iw, ih, changes),
    );
    let (x, y, tw, th) = track;
    let (_, changes) = scope_click(&mut ui, view.clone(), ((x + (tw * 0.75)) as i64, (y + (th / 2.0)) as i64));
    // a click at 75 % of the 57.5-point track reads 180 of 240; the one slack is the test's whole-point
    // click (up to 1 point is 4.2 iterations): 175 to 181
    check(
        changes.iter().any(|c| matches!(c, Change::Probe_set { zone: z, index } if *z == zone && *index >= 175 && *index <= 181)),
        "the track did not map the pointer to an iteration",
    );
    let (_, changes) = scope_click(&mut ui, view.clone(), (10, 690));
    check(changes.iter().all(|c| matches!(c, Change::Selected(_))), "a blank click made an edit");
    // the toggle collapses the zone; a collapsed zone has no selector
    let (tx, ty, _, _) = scope::inspect::box_of(&view, &zone).expect("box");
    let (_, changes) = scope_click(&mut ui, view, (tx as i64 + 10, ty as i64 + 10));
    check(changes.contains(&Change::Zone_collapsed { zone, collapsed: true }), "the toggle did not collapse the zone");
}

fn drawn_iterations() {
    let mut ui = new_ui();
    let w = load_workspace("sunflower");
    let zone = path(&["sunflower", "seeds_each"]);
    // only visible zones draw: 1,000 iterations draw what 3 do
    let inputs = |n: i64| -> Inputs { vec![("sunflower".to_owned(), vec![("seeds".into(), Value::Int(n))])] };
    let draw_with = |ui: &mut Ui, n: i64| {
        let (view, _) = view_with(ScopeOpts::default(), Some(inputs(n)), &w, "sunflower");
        let (view, _) = scope_step(ui, view, &frame0());
        (scope::stats(&view).drawn_items, scope_paint(ui, view))
    };
    let (items3, paint3) = draw_with(&mut ui, 3);
    let (items1000, paint1000) = draw_with(&mut ui, 1000);
    check(items3 == items1000, "iterations changed how many tiles are built");
    // what grows is the sparklines: at most 16 segments each, whatever the count
    check(paint1000 < paint3 + (30 * items3), "a 1,000-iteration zone painted much more than a 3-iteration one");
    let (base, sc) = view_with(ScopeOpts::default(), Some(inputs(3)), &w, "sunflower");
    let evaluated = flow_eval::r#static(&w, true, Some(&inputs(3))).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let zone2 = zone.clone();
    let execution = move |q: &[flow_syntax::symbol::Symbol]| Some(Execution { tier: "CPU".to_owned(), group: zone2.clone(), seconds: if q == zone2.as_slice() { Some(0.002) } else { None } });
    let ex2 = execution.clone();
    let reports = Rc::new(RefCell::new(probe::make(&evaluated, probe::Options { execution: Some(Rc::new(move |q: &[flow_syntax::symbol::Symbol], _: &[i64]| ex2(q))), ..probe::Options::default() })));
    let zone_node = p::find(&sc, &zone).expect("zone");
    let footer = reports.borrow_mut().footer(&zone_node, &[]);
    check(footer.execution == execution(&zone), "the footer lost its execution report");
    check(reports.borrow_mut().readouts(&zone_node, &[]).contains(&("tier".to_owned(), "CPU".to_owned())), "the inspector lost its execution tier");
    let paint_hash = |ui: &mut Ui, view: Pane| -> Vec<u8> {
        let _ = scope_step(ui, view, &frame0());
        match rays::scene::layer::stage_native(1000, 700, &ui.scene(None)) {
            Err(m) => fail(&m),
            Ok(staged) => staged
                .layers
                .iter()
                .filter_map(|l| match l {
                    rays::scene::layer::native_layer::Ui_layer(batch, _) => Some(batch.instances().to_vec()),
                    _ => None,
                })
                .flatten()
                .collect(),
        }
    };
    let a = paint_hash(&mut ui, base.clone());
    let b = paint_hash(&mut ui, { let mut r8_view = base; scope::with_records(reports, &mut r8_view); r8_view });
    check(a != b, "execution badges did not change the graph's paint");
    let z3 = zone.clone();
    let (collapsed_view, _) = scope_view(saved_opts(TestSaved { collapsed: vec![z3.clone()], ..Default::default() }), &w, "sunflower");
    let (collapsed_view, _) = scope_step(&mut ui, collapsed_view, &frame0());
    check(scope::stats(&collapsed_view).drawn_zones == 0, "a collapsed zone drew its body");
}

fn wire_and_delete() {
    let mut ui = new_ui();
    // gestures become requests: scrub, wire, delete
    let ws = load_workspace("bloom");
    let (view, sc) = scope_view(ScopeOpts::default(), &ws, "flower");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let heart = path(&["flower", "heart"]);
    // wire: a node's output socket dropped on another node's geometry row
    let source = path(&["flower", "bloom"]);
    let target = path(&["flower", "result"]);
    let (sx, sy) = scope::inspect::output_socket(&view, &source).expect("socket");
    let start = ipt((sx, sy));
    let tnode = p::find(&sc, &target).expect("target");
    let row = tnode.rows.iter().position(|r| r.socket && r.kind == p::RowKind::Rest).expect("row");
    let stop = ipt(scope::inspect::row_center(&view, &target, row).expect("c"));
    let (view, _) = scope_step(&mut ui, view, &frame((start.0, start.1), Vec::new(), vec![mouse_move(start), mouse_press(MouseButton::LeftButton, start)]));
    let (view, _) = scope_step(&mut ui, view, &frame_ev(stop, vec![mouse_move(stop)]));
    let (_, changes) = scope_step(&mut ui, view.clone(), &frame_ev(stop, vec![mouse_release(MouseButton::LeftButton, stop)]));
    check(
        changes.iter().any(|c| matches!(c, Change::Syntax_edit(Op::Connect { node, src, iter: false, .. }) if *node == target && src == "bloom")),
        "a wire dropped on a row did not become Connect",
    );
    // delete: the selected node
    let (view, _) = scope_click(&mut ui, view, start);
    let view = { let mut r8_view = view; scope::select(vec![heart.clone()], &mut r8_view); r8_view };
    let (_, changes) = { let mut r8_view = view.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Delete, &mut r8_changes); (r8_view, r8_changes) };
    check(changes == vec![Change::Syntax_edit(Op::DeleteNodes { nodes: vec![heart.clone()] })], "Delete did not become Delete_nodes");
    // wire selection and deletion
    check(scope::inspect::wire_count(&view) > 0, "there are wires in the graph");
    let view0 = { let mut r8_view = view; scope::select(vec![heart.clone()], &mut r8_view); r8_view };
    check(scope::selected(&view0) == vec![heart.clone()], "heart is selected");
    // some wire of the graph (not under a card) is hit at its longest segment's middle
    let mut view = view0.clone();
    for i in 0..scope::inspect::wire_count(&view0) {
        if scope::selected_wire(&view).is_some() || scope::inspect::wire_target(&view0, i).is_none() {
            continue;
        }
        let wpt = scope::inspect::wire_midpoint(&view0, i).expect("mid");
        view = scope_click(&mut ui, view0.clone(), ipt(wpt)).0;
    }
    check(scope::selected_wire(&view).is_some(), "clicking wire selected it");
    check(scope::selected(&view).is_empty(), "clicking wire cleared node selection");
    let (_, changes) = { let mut r8_view = view.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Delete, &mut r8_changes); (r8_view, r8_changes) };
    check(matches!(changes.as_slice(), [Change::Syntax_edit(Op::Disconnect { .. })]), "Delete on selected wire emitted Disconnect");
    // a right-click on a node makes the nodes the selection again: Delete then removes the node
    let (hx, hy, _, _) = scope::inspect::box_of(&view, &heart).expect("box");
    let at = (hx as i64 + 60, hy as i64 + 8);
    let (menu, changes) = scope_step(&mut ui, view.clone(), &frame_ev(at, right_click(at)));
    check(changes.contains(&Change::Selected(vec![heart.clone()])), "a right-click on a node did not emit Selected");
    check(
        { let mut r8_view = menu; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Delete, &mut r8_changes); (r8_view, r8_changes) }.1.contains(&Change::Syntax_edit(Op::DeleteNodes { nodes: vec![heart.clone()] })),
        "Delete after a right-click on a node removed the wire selected before",
    );
    // keys and menu: every key command maps to a request
    let bindings = scope::bindings();
    for name in ["delete", "fold", "unfold", "hoist", "bypass", "repeat", "iterate", "function", "collapse", "probe-prev", "probe-next", "frame-all", "walk.left"] {
        check(bindings.iter().any(|(c, _)| c.id == format!("scope.{}", name)), &format!("no key for {}", name));
    }
    let (_, changes) = { let mut r8_view = { let mut r8_view = view; scope::select(vec![heart.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Bypass, &mut r8_changes); (r8_view, r8_changes) };
    check(changes == vec![Change::Syntax_edit(Op::ToggleBypass { node: heart })], "Bypass");
}

fn loop_wire() {
    let mut ui = new_ui();
    // a wire inside a loop's body is hovered and selected like any other: its boxes lie over the zone's tile
    let lw = checked("(workspace l (graph g :context sop (let* [ring (for [i (range 3)] (let* [u (sop/uv_sphere) v (sop/transform u)] v)) m (sop/merge ring)] m)))");
    let (lview, _) = scope_view(saved_opts(TestSaved { at: vec![(path(&["g", "ring", "u"]), (0.0, 0.0)), (path(&["g", "ring", "v"]), (480.0, 240.0))], ..Default::default() }), &lw, "g");
    let (lview, _) = scope_step(&mut ui, lview, &frame0());
    let inner = path(&["g", "ring", "v"]);
    let wi = (0..scope::inspect::wire_count(&lview)).find(|i| matches!(scope::inspect::wire_target(&lview, *i), Some((q, _)) if q == inner)).expect("wire");
    let mid = ipt(scope::inspect::wire_midpoint(&lview, wi).expect("mid"));
    let (hovered, _) = scope_step(&mut ui, lview.clone(), &frame_ev(mid, vec![mouse_move(mid)]));
    check(scope::inspect::highlighted_connections(&hovered).len() == 1, "a wire inside a loop is not hovered");
    let (picked, _) = scope_click(&mut ui, lview, mid);
    check(
        matches!({ let mut r8_view = picked; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Delete, &mut r8_changes); (r8_view, r8_changes) }.1.first(), Some(Change::Syntax_edit(Op::Disconnect { node, .. })) if *node == inner),
        "a wire inside a loop is not selected by a click",
    );
}

fn macro_lens() {
    let mut ui = new_ui();
    // W9: the macro lens, the bypass flag, the make-macro key
    let w = load_workspace("rosette");
    let (view, sc) = scope_view(ScopeOpts::default(), &w, "rosette");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let outer = path(&["rosette", "outer"]);
    let soft = path(&["rosette", "soft"]);
    let node = p::find(&sc, &outer).expect("outer");
    let lens = node.lens.clone().expect("lens");
    check(
        lens.steps.len() >= 2 && lens.error.is_none() && lens.steps[0].starts_with("(radial") && lens.steps[1].starts_with("(sop/merge"),
        "the lens steps are the call, then its expansions",
    );
    check(scope::inspect::macro_step(&view, &outer).is_none(), "the lens starts closed");
    let toggle = scope::inspect::lens_toggle(&view, &outer).expect("toggle");
    let (view, _) = scope_click(&mut ui, view, ipt(toggle));
    check(scope::inspect::macro_step(&view, &outer) == Some(lens.steps.len() as i64 - 1), "the toggle opens the last step");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let button = |view: &Pane, i: i64| ipt(scope::inspect::lens_step_button(view, &outer, i).expect("button"));
    let b0 = button(&view, 0);
    let (view, changes) = scope_click(&mut ui, view, b0);
    check(scope::inspect::macro_step(&view, &outer) == Some(0) && changes.is_empty(), "the call button shows the call, no edit");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let b1 = button(&view, 1);
    let (view, _) = scope_click(&mut ui, view, b1);
    check(scope::inspect::macro_step(&view, &outer) == Some(1), "a step button chooses the step");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    // the Template button shows the macro's definition, one step past the expansions
    check(lens.template.split('\n').any(|l| l.starts_with("(defmacro radial")), "the lens carries the macro's template");
    let bt = button(&view, lens.steps.len() as i64);
    let (view, changes) = scope_click(&mut ui, view, bt);
    check(scope::inspect::macro_step(&view, &outer) == Some(lens.steps.len() as i64) && changes.is_empty(), "the Template button shows the definition, no edit");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let b1 = button(&view, 1);
    let (view, _) = scope_click(&mut ui, view, b1);
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let (_, _, bw, bh) = scope::inspect::box_of(&view, &outer).expect("box");
    let h0 = scope::inspect::box_of(&scope_view(ScopeOpts::default(), &w, "rosette").0, &outer).expect("box").3;
    check(bh > h0 && bw >= (p::lens_width() * scope::zoom(&view)) - 1.0, "an open panel grows its card");
    let r = scope::inspect::lens_replace(&view, &outer).expect("replace");
    let (_, changes) = scope_click(&mut ui, view.clone(), ipt(r));
    check(changes.contains(&Change::Syntax_edit(Op::InlineMacro { node: outer.clone() })), "the replace button did not become Inline_macro");
    let t = scope::inspect::lens_toggle(&view, &outer).expect("toggle");
    let (view, _) = scope_click(&mut ui, view, ipt(t));
    check(scope::inspect::macro_step(&view, &outer).is_none(), "the toggle closes the panel");
    // bypass is the b key (and the context menu): the same request, and a card with nothing to pass through has none
    let (_, changes) = { let mut r8_view = { let mut r8_view = view.clone(); scope::select(vec![soft.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Bypass, &mut r8_changes); (r8_view, r8_changes) };
    check(changes.contains(&Change::Syntax_edit(Op::ToggleBypass { node: soft.clone() })), "the b key did not become Toggle_bypass");
    check(p::find(&sc, &soft).expect("soft").bypass, "soft is authored bypassed");
    // m asks the host for the make-macro dialog over the selection
    let (_, changes) = { let mut r8_view = { let mut r8_view = view.clone(); scope::select(vec![soft.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Make_macro, &mut r8_changes); (r8_view, r8_changes) };
    check(changes == vec![Change::Macro_requested(vec![soft])], "m did not request the dialog");
    let (_, changes) = { let mut r8_view = { let mut r8_view = view; scope::select(Vec::new(), &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Make_macro, &mut r8_changes); (r8_view, r8_changes) };
    check(matches!(changes.as_slice(), [Change::Notice(_)]), "m with nothing selected");
}

pub const CASES: &[(&str, fn())] = &[
    ("run_scope_conditional_arms", conditional_arms),
    ("run_scope_fixture_counts", fixture_counts),
    ("run_scope_selector_steps", selector_steps),
    ("run_scope_drawn_iterations", drawn_iterations),
    ("run_scope_wire_and_delete", wire_and_delete),
    ("run_scope_loop_wire", loop_wire),
    ("run_scope_macro_lens", macro_lens),
];
