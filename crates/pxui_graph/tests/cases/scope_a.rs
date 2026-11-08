//! The OCaml test_pxui_graph.ml lines 218-899: zoom geometry, levels, hover, idle frame, suspend,
//! pinch, carry, ghost values.
use crate::support::*;
use editor_core::keymap::Trigger;
use flow_graph::flow_edit::Op;
use flow_graph::projection::{self as p, Level};
use flow_check::workspace::Path;
use flow_graph::edit_tree::ArgKey;
use input::{Event, Key, MouseButton};
use pxui::ui::*;
use pxui_graph::scope_commands;
use pxui_graph::scope_pane as scope;
use pxui_graph::scope_state::{Change, Command, Pane, ScopeOpts};
use std::rc::Rc;

fn at_of(pairs: Vec<(Path, (f64, f64))>) -> ScopeOpts {
    saved_opts(TestSaved { at: pairs, ..Default::default() })
}

fn menu_requested(changes: &[Change]) -> bool {
    changes.iter().any(|c| matches!(c, Change::Menu_requested(..)))
}

// ---------------------------------------------------------------- zoom geometry

/// Zoomed out, a node is its shown level's geometry: at every zoom, and with one card pinned full among
/// points, each wire end is a port of a node.
fn scope_zoom_geometry() {
    let mut ui = new_ui();
    let ws = checked("(workspace z (graph g :context sop [(n : int 3)] (let* [a (sop/box) b (sop/scatter a :count n) c (sop/copy_to_points a b) d (sop/transform c)] (sop/merge c d))))");
    let sc = p::of_graph(&scope_catalog(), &ws, "g").unwrap();
    let mut paths: Vec<Path> = sc.nodes.iter().map(|n| n.path.clone()).collect();
    paths.extend(sc.inputs.iter().map(|i| i.path.clone()));
    let cases: Vec<(bool, f64, fn(f64) -> bool)> = vec![
        (false, 1.0, |z| z >= 0.5),
        (false, 0.45, |z| z < 0.5 && z >= 0.34),
        (false, 0.3, |z| z < 0.34),
        (true, 0.3, |z| z < 0.34),
        (true, 0.45, |z| z < 0.5),
    ];
    for (pinned, factor, expect) in cases {
        let o = saved_opts(TestSaved { level: if pinned { vec![(path(&["g", "b"]), (Level::Full, true))] } else { Vec::new() }, ..Default::default() });
        let (view, _) = scope_view(o, &ws, "g");
        let (view, _) = scope_step(&mut ui, view, &frame0());
        let mouse = (500, 350);
        let z0 = scope::zoom(&view);
        let (view, _) = scope_step(&mut ui, view, &frame_ev(mouse, vec![mouse_move(mouse), Event::Pinched(factor / z0)]));
        let (view, _) = scope_step(&mut ui, view, &frame_ev(mouse, vec![]));
        let ports: Vec<(f64, f64)> = paths.iter().flat_map(|q| scope::Inspect::ports(&view, q)).collect();
        let at = |x: f64, y: f64| ports.iter().any(|(px, py)| (px - x).abs() < 0.01 && (py - y).abs() < 0.01);
        let count = scope::Inspect::wire_count(&view);
        check(count > 0, "no wires to check");
        for i in 0..count {
            let pts = scope::Inspect::wire_points(&view, i);
            match pts.first() {
                Some(first) => {
                    let last = pts[pts.len() - 1];
                    check(at(first.0, first.1) && at(last.0, last.1), &format!("zoom {:.2}{}: wire {} does not end on a port", scope::zoom(&view), if pinned { " (b full)" } else { "" }, i));
                }
                None => fail("an empty wire"),
            }
        }
        check(expect(scope::zoom(&view)), "the pinch did not reach the zoom under test");
        // the zoom never reduces a card: b has a written row at every zoom
        let (_, _, _, bh) = scope::Inspect::box_of(&view, &path(&["g", "b"])).unwrap();
        check(bh > ((p::HEAD_HEIGHT + p::ROW_HEIGHT) * scope::zoom(&view)) - 0.01, &format!("zoom {:.2}: a card lost its rows", scope::zoom(&view)));
        // footers are drawn (so counted) only for cards in view and not too small to read
        let fp = scope::Inspect::footer_paths(&view);
        check((scope::zoom(&view) < 0.4 && fp.is_empty()) || fp.contains(&path(&["g", "b"])), &format!("zoom {:.2}: footer paths", scope::zoom(&view)));
        // a click on a node's box, at whatever level it is shown, selects it
        if scope::zoom(&view) < 0.5 {
            for q in &paths {
                let (x, y, w, h) = scope::Inspect::box_of(&view, q).unwrap();
                let (view2, _) = scope_click(&mut ui, view.clone(), ((x + w / 2.0) as i64, (y + (h / 2.0).min(11.0 * scope::zoom(&view))) as i64));
                check(scope::selected(&view2) == vec![q.clone()], &format!("a click on {} did not select it", q.join("/")));
            }
        }
    }
}

// ---------------------------------------------------------------- levels

const LEVELS_TEXT: &str = "(workspace levels (graph g :context sop (let* [a (sop/box) b (sop/box) c (sop/transform a) d (sop/transform b)] (sop/merge c d))))";

fn levels_fixture(ui: &mut Ui) -> (flow_check::workspace::Workspace, Pane, Rc<p::Scope>) {
    let ws = checked(LEVELS_TEXT);
    let (view, sc) = scope_view(
        at_of(vec![(path(&["g", "a"]), (24.0, 24.0)), (path(&["g", "b"]), (336.0, 24.0)), (path(&["g", "c"]), (648.0, 24.0)), (path(&["g", "d"]), (336.0, 360.0)), (path(&["g", "@result"]), (960.0, 360.0))]),
        &ws,
        "g",
    );
    let (view, _) = scope_step(ui, view, &frame0());
    (ws, view, sc)
}

fn scope_levels_a() {
    let mut ui = new_ui();
    let (ws, view, sc) = levels_fixture(&mut ui);
    let (a, b, c) = (path(&["g", "a"]), path(&["g", "b"]), path(&["g", "c"]));
    // the first geometry slot is the header's in-port, never a row
    let node_c = p::find(&sc, &c).unwrap();
    check(node_c.rows.iter().any(|r| r.head), "the first geometry slot is not the header's");
    // o opens the selection one level and pins it; p points it, again goes back
    let sel = scope::select(vec![a.clone()], view.clone());
    let (_, changes) = scope::run_command(sel.clone(), Command::Open_level);
    check(changes == vec![Change::Level_set(vec![(a.clone(), Some(Level::Full), true)])], "o is not Level_set full, pinned");
    let (_pointed, changes) = scope::run_command(sel, Command::Point_level);
    check(changes == vec![Change::Level_set(vec![(a.clone(), Some(Level::Point), false)])], "p is not Level_set point");
    let a2 = a.clone();
    let o = saved_opts(TestSaved { at: vec![(a.clone(), (24.0, 24.0))], level: vec![(a2, (Level::Point, false))], ..Default::default() });
    let (pview, _) = scope_view(o, &ws, "g");
    let (pview, _) = scope_step(&mut ui, pview, &frame0());
    let (_, _, pw, ph) = scope::Inspect::box_of(&pview, &a).unwrap();
    check(ph == p::POINT_SIZE * scope::zoom(&pview) && pw < 100.0 * scope::zoom(&pview), "a point is the 14-point disc and its name");
    let (_back, changes) = scope::run_command(scope::select(vec![a.clone()], pview), Command::Point_level);
    check(matches!(changes.as_slice(), [Change::Level_set(l)] if l.len() == 1 && l[0].0 == a && l[0].1.is_none() && !l[0].2), "p on a point does not go back to the card");
    // the header's in-port takes a wire: dropped anywhere on the header it is Connect on the first slot
    let (sx, sy) = scope::Inspect::output_socket(&view, &b).unwrap();
    let (tx, ty) = scope::Inspect::row_center(&view, &c, 0).unwrap();
    let start = (sx as i64, sy as i64);
    let stop = (tx as i64, ty as i64);
    let (v, _) = scope_step(&mut ui, view.clone(), &frame_ev(start, vec![mouse_move(start), mouse_press(MouseButton::LeftButton, start)]));
    let (v, _) = scope_step(&mut ui, v, &frame_ev(stop, vec![mouse_move(stop)]));
    let (_, changes) = scope_step(&mut ui, v, &frame_ev(stop, vec![mouse_release(MouseButton::LeftButton, stop)]));
    check(
        changes.iter().any(|ch| matches!(ch, Change::Syntax_edit(Op::Connect { node, key: ArgKey::Pos(0), src, .. }) if *node == c && src == "b")),
        "a wire dropped on the header did not connect the first slot",
    );
    // a wire let go on empty canvas selects its source and asks for the add menu there
    let empty = (2900, 1900);
    let (v, _) = scope_step(&mut ui, view.clone(), &frame_ev(start, vec![mouse_move(start), mouse_press(MouseButton::LeftButton, start)]));
    let (v, _) = scope_step(&mut ui, v, &frame_ev(empty, vec![mouse_move(empty)]));
    let (v, changes) = scope_step(&mut ui, v, &frame_ev(empty, vec![mouse_release(MouseButton::LeftButton, empty)]));
    check(changes.contains(&Change::Selected(vec![b.clone()])) && menu_requested(&changes) && scope::selected(&v) == vec![b.clone()], "a wire let go on empty canvas did not ask for the node menu after its source");
    // a wire itself pulled into empty canvas does the same, and stays selected: the host inserts on it
    let i = (0..scope::Inspect::wire_count(&view)).find(|i| matches!(scope::Inspect::wire_target(&view, *i), Some((n, _)) if n == c));
    let i = match i {
        None => fail("no wire into c"),
        Some(i) => i,
    };
    let (ax, _, aw, _) = scope::Inspect::box_of(&view, &a).unwrap();
    let (bx, _, _, _) = scope::Inspect::box_of(&view, &b).unwrap();
    let (mx, my) = scope::Inspect::wire_points(&view, i).into_iter().find(|(x, _)| *x > ax + aw + 4.0 && *x < bx - 4.0).expect("wire point");
    let mid = (mx as i64, my as i64);
    let (v, _) = scope_step(&mut ui, view.clone(), &frame_ev(mid, vec![mouse_move(mid), mouse_press(MouseButton::LeftButton, mid)]));
    let (v, _) = scope_step(&mut ui, v, &frame_ev(empty, vec![mouse_move(empty)]));
    let (v, changes) = scope_step(&mut ui, v, &frame_ev(empty, vec![mouse_release(MouseButton::LeftButton, empty)]));
    check(
        changes.contains(&Change::Selected(vec![a.clone()])) && menu_requested(&changes) && scope::selected(&v) == vec![a.clone()] && scope::selected_wire(&v).map(|w| w.0) == Some(c.clone()),
        "a wire pulled into empty canvas did not ask for the node menu with the wire selected",
    );
    // a short press-release on the wire is still just a click
    let (v, _) = scope_step(&mut ui, view.clone(), &frame_ev(mid, vec![mouse_move(mid), mouse_press(MouseButton::LeftButton, mid)]));
    let (_, changes) = scope_step(&mut ui, v, &frame_ev(mid, vec![mouse_release(MouseButton::LeftButton, mid)]));
    check(!menu_requested(&changes), "a click on a wire opened the menu");
    // Alt: the wire is lifted off its input; empty canvas disconnects it, no menu
    let (v, _) = scope_step(&mut ui, view.clone(), &frame(mid, vec![Key::Alt], vec![mouse_move(mid), mouse_press(MouseButton::LeftButton, mid)]));
    let (v, _) = scope_step(&mut ui, v, &frame(empty, vec![Key::Alt], vec![mouse_move(empty)]));
    let (_, changes) = scope_step(&mut ui, v, &frame_ev(empty, vec![mouse_release(MouseButton::LeftButton, empty)]));
    check(changes.iter().any(|ch| matches!(ch, Change::Syntax_edit(Op::Disconnect { node, .. }) if *node == c)) && !menu_requested(&changes), "an Alt-drag of a wire into empty canvas did not disconnect it");
    // Alt on b's out-port lifts its wire too
    let (v, _) = scope_step(&mut ui, view, &frame(start, vec![Key::Alt], vec![mouse_move(start), mouse_press(MouseButton::LeftButton, start)]));
    let (v, _) = scope_step(&mut ui, v, &frame(empty, vec![Key::Alt], vec![mouse_move(empty)]));
    let (_, changes) = scope_step(&mut ui, v, &frame_ev(empty, vec![mouse_release(MouseButton::LeftButton, empty)]));
    check(changes.iter().any(|ch| matches!(ch, Change::Syntax_edit(Op::Disconnect { .. }))), "an Alt-drag of an out-port into empty canvas did not take its wires off");
}

fn scope_levels_b() {
    let mut ui = new_ui();
    // a frame's own variable leaves its port on the begin bar (20 points inside the left edge) straight into
    // its body: the wire stays inside the frame, below the label row
    let mw = checked("(workspace m (graph g :context value (let* [boxes (array/vec3 8) ys (map (fn [i] (let* [p (array/nth boxes (int i))] [p.x p.y 0])) (array/range 4))] (array/count ys))))");
    let (mview, _) = scope_view(ScopeOpts::default(), &mw, "g");
    let (mview, _) = scope_step(&mut ui, mview, &frame0());
    let (zx, zy, zw, _) = scope::Inspect::box_of(&mview, &path(&["g", "ys#0"])).unwrap();
    let z = scope::zoom(&mview);
    let from_i: Vec<i64> = (0..scope::Inspect::wire_count(&mview)).filter(|i| matches!(scope::Inspect::wire_points(&mview, *i).first(), Some((x, _)) if *x > zx && *x <= zx + 24.0 * z)).collect();
    check(!from_i.is_empty(), "the frame's variable has no wire from its begin bar");
    for i in from_i {
        for (x, y) in scope::Inspect::wire_points(&mview, i) {
            check(x >= zx && x <= zx + zw - 3.0 * z && y > zy + p::HEAD_HEIGHT * z, "a frame's variable wire left the frame or crossed its label row");
        }
    }
}

fn one_frame(ui: &mut Ui, key: Key, v: Pane) -> (Pane, Vec<Change>) {
    let f = frame((0, 0), vec![key.clone()], vec![]);
    let mut cell = Some(v);
    let mut out = None;
    ui.frame(&f, &mut |ui: &mut Ui| {
        out = Some(scope::update(&[key.clone()], cell.take().expect("view"), ui, &f));
    });
    out.expect("frame")
}

fn scope_levels_c() {
    let mut ui = new_ui();
    let (_, view, sc) = levels_fixture(&mut ui);
    let b = path(&["g", "b"]);
    // f: letter hints; a letter connects the selected output
    let (hint_view, notices) = scope::run_command(scope::select(vec![b.clone()], view), Command::Show_hints);
    check(notices.is_empty() && scope::editing(&hint_view), "f did not start the hints");
    let (hint_view, _) = scope_step(&mut ui, hint_view, &frame0());
    // one frame with the key down: scope_step's settle frame would take the press
    let (v, changes) = one_frame(&mut ui, Key::KeyChar('a'), hint_view.clone());
    let connects = changes.iter().filter(|c| matches!(c, Change::Syntax_edit(Op::Connect { src, .. }) if src == "b")).count();
    check(connects > 0 && !scope::editing(&v), "a hint letter did not connect and end the hints");
    let (esc, _) = one_frame(&mut ui, Key::Escape, hint_view.clone());
    check(!scope::editing(&esc), "Escape did not end the hints");
    // a reloaded document ends them: their targets name nodes of the scope they were made over
    check(scope::editing(&scope::with_scope(ScopeOpts::default(), "g", sc, hint_view.clone())), "the same scope ended the hints");
    let reloaded = checked("(workspace w (graph g :context sop (sop/box)))");
    let rs = Rc::new(p::of_graph(&scope_catalog(), &reloaded, "g").unwrap());
    check(!scope::editing(&scope::with_scope(ScopeOpts::default(), "g", rs, hint_view)), "a new scope kept the letter hints");
    // f frames the selection (all with none); the hints are on w
    let key_of = |action: Command| scope_commands::bindings().into_iter().find(|c| c.1 == action).and_then(|c| c.0.trigger);
    check(matches!(key_of(Command::Frame_selection), Some(Trigger::Chord(Key::KeyChar('f'), k)) if k.is_empty()), "f does not frame the selection");
    check(matches!(key_of(Command::Show_hints), Some(Trigger::Chord(Key::KeyChar('w'), k)) if k.is_empty()), "w does not start the hints");
    // a node named as the graph it refers to is not wired to itself
    let sw = checked("(workspace s (graph g :context sop (sop/box)) (graph scene :context scene (let* [g (scene/geometry (ref g))] (scene/merge g))))");
    let (sview, _) = scope_view(ScopeOpts::default(), &sw, "scene");
    let (sview, _) = scope_step(&mut ui, sview, &frame0());
    check((0..scope::Inspect::wire_count(&sview)).all(|i| !matches!(scope::Inspect::wire_target(&sview, i), Some((q, _)) if q == path(&["scene", "g"]))), "a node that refers to a graph of its own name is wired to itself");
    // v on a node inside a loop asks for it too: the host shows it at the probed iteration
    let lw = checked("(workspace l (graph g :context sop [(n : int 3)] (let* [k 0.4 a (sop/uv_sphere :radius k :segments n) copies (for [i (range 2)] (sop/transform a :translate [i 0 0]))] (sop/merge copies))))");
    let (lview, lscope) = scope_view(ScopeOpts::default(), &lw, "g");
    let (lview, _) = scope_step(&mut ui, lview, &frame0());
    let inner = path(&["g", "copies", "@result"]);
    check(scope::run_command(scope::select(vec![inner.clone()], lview.clone()), Command::Display).1 == vec![Change::Display_set(inner)], "v refused a node inside a loop");
    // a row wired from a node nothing else reads has a fold button; a click folds that node into the row.
    // A row wired from a graph input has none.
    let sphere = path(&["g", "a"]);
    let rows = p::find(&lscope, &sphere).unwrap().rows;
    let row = |label: &str| rows.iter().position(|r| r.label == label).unwrap();
    check(scope::Inspect::fold_button(&lview, &sphere, row("segments")).is_none(), "a row wired from an input has a fold button");
    let (fx, fy) = match scope::Inspect::fold_button(&lview, &sphere, row("radius")) {
        Some(at) => at,
        None => fail("a row wired from a single-use node has no fold button"),
    };
    let (_, changes) = scope_click(&mut ui, lview.clone(), (fx as i64, fy as i64));
    check(changes.contains(&Change::Syntax_edit(Op::FoldInto { node: path(&["g", "k"]) })), "a click on the fold button did not fold the source into the row");
    // taking the wire off a named argument writes the value it carried (k is 0.4): the row stays on the card
    let (rx, ry) = scope::Inspect::row_center(&lview, &sphere, row("radius")).unwrap();
    let at = (rx as i64, ry as i64);
    let (hovered, _) = scope_step(&mut ui, lview, &frame_ev(at, vec![mouse_move(at)]));
    match scope::run_command(hovered, Command::Delete).1.as_slice() {
        [Change::Syntax_edit(Op::Disconnect { node, key: ArgKey::Kw(k), fallback: Some(f) })] if k == "radius" => {
            let flat = flow_syntax::lisp::flat(f);
            check(*node == sphere && flat == "0.4", &format!("the wire taken off radius did not keep its last value: {flat}"));
        }
        _ => fail("Delete over a wired row is not a Disconnect with the last value"),
    }
}

// ---------------------------------------------------------------- connection hover

fn scope_connection_hover() {
    let mut ui = new_ui();
    let ws = checked("(workspace hover (graph g :context sop (let* [a (sop/box) b (sop/transform a) c (sop/transform a)] (sop/merge b c))))");
    let (view, _) = scope_view(at_of(vec![(path(&["g", "a"]), (0.0, 0.0)), (path(&["g", "b"]), (400.0, 0.0)), (path(&["g", "c"]), (400.0, 240.0)), (path(&["g", "@result"]), (800.0, 600.0))]), &ws, "g");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let highlight = |ui: &mut Ui, point: (f64, f64)| {
        let point = (point.0 as i64, point.1 as i64);
        let (hovered, changes) = scope_step(ui, view.clone(), &frame_ev(point, vec![mouse_move(point)]));
        check(changes.is_empty(), "hover edited the graph");
        scope::Inspect::highlighted_connections(&hovered)
    };
    let source = scope::Inspect::output_socket(&view, &path(&["g", "a"])).unwrap();
    let fanout = highlight(&mut ui, source);
    check(fanout.len() == 2, "an output hover did not highlight its two connections");
    let (ax, ay) = source;
    let (bx, _, _, _) = scope::Inspect::box_of(&view, &path(&["g", "c"])).unwrap();
    let (_, by) = scope::Inspect::row_center(&view, &path(&["g", "c"]), 0).unwrap();
    let wire_hover = highlight(&mut ui, ((ax + bx) / 2.0, (ay + by) / 2.0));
    check(wire_hover.len() == 1, &format!("hover on a wire did not isolate its connection: {} at {:.1} {:.1}", wire_hover.len(), (ax + bx) / 2.0, (ay + by) / 2.0));
    check(highlight(&mut ui, (bx, by)).len() == 1, "an input hover did not isolate its connection");
    check(highlight(&mut ui, (999.0, 699.0)).is_empty(), "connections stayed highlighted after leaving them");
    let midpoint = (((ax + bx) / 2.0) as i64, ((ay + by) / 2.0) as i64);
    let (selected, _) = scope_click(&mut ui, view, midpoint);
    check(
        matches!(scope::run_command(selected, Command::Delete).1.as_slice(), [Change::Syntax_edit(Op::Disconnect { node, key: ArgKey::Pos(0), .. })] if *node == path(&["g", "c"])),
        "a press on the wire did not preserve its disconnect target",
    );
    let ws = checked("(workspace capture (graph g :context sop (let* [a (sop/box) copies (for [i (range 2)] (sop/transform a :translate [i 0 0]))] (sop/merge copies))))");
    let (view, _) = scope_view(ScopeOpts::default(), &ws, "g");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    // a captured value is wired straight across the zone's edge from the node that makes it
    let (ax, ay) = scope::Inspect::output_socket(&view, &path(&["g", "a"])).unwrap();
    let point = (ax as i64, ay as i64);
    let (hovered, changes) = scope_step(&mut ui, view, &frame_ev(point, vec![mouse_move(point)]));
    check(changes.is_empty() && scope::Inspect::highlighted_connections(&hovered).len() == 1, "hover on a captured value's socket missed its outer connection");
}

// ---------------------------------------------------------------- idle frame

/// Frame work does not scale with what is out of view: an idle frame of a pane that shows no card of a
/// 2,001-node graph allocates a small constant, and the level keys answer for every node at once.
fn scope_idle_frame() {
    let mut ui = new_ui();
    let mut cur = Some(scope::with_scope(ScopeOpts::default(), "g", Rc::new(big_scope(2001)), scope::create(Some(16), Some(16))));
    let step = |ui: &mut Ui, view: &mut Option<Pane>| {
        let v = view.take().unwrap();
        *view = Some(in_frame(ui, &frame0(), v).0);
    };
    for _ in 0..3 {
        step(&mut ui, &mut cur);
    }
    let before = mem_sys::alloc_bytes_total();
    for _ in 0..10 {
        step(&mut ui, &mut cur);
    }
    let per_frame = (mem_sys::alloc_bytes_total() - before) as f64 / 10.0;
    let view = cur.unwrap();
    check(scope::stats(&view).nodes >= 2001, "the big graph is not laid out");
    check(per_frame < 500_000.0, &format!("an idle frame with nothing in view allocates {:.0} bytes", per_frame));
    check(
        matches!(scope::run_command(view, Command::Point_all).1.as_slice(), [Change::Level_set(l)] if l.len() == 2001 && l.iter().all(|(_, level, _)| *level == Some(Level::Point))),
        "Point_all did not point every node",
    );
}

// ---------------------------------------------------------------- suspend

/// A pane that stops being drawn keeps nothing open: a pan gives the pointer back and a text field closes.
fn scope_suspend() {
    let mut ui = new_ui();
    let w = load_workspace("bloom");
    let (view, _) = scope_view(ScopeOpts::default(), &w, "flower");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let empty = (990, 690);
    let mut held = frame_ev(empty, vec![mouse_move(empty), mouse_press(MouseButton::RightButton, empty)]);
    held.input.buttons = vec![MouseButton::RightButton];
    let (panning, _) = scope_step(&mut ui, view, &held);
    check(scope::Inspect::grabbed(&panning), "a right-button press on the canvas did not begin a pan");
    let (naming, _) = scope::run_command(scope::select(vec![path(&["flower", "heart"])], panning), Command::Edit_name);
    check(scope::editing(&naming) && scope::Inspect::grabbed(&naming), "the name field did not open during the pan");
    let idle = scope::suspend(naming);
    check(!scope::Inspect::grabbed(&idle) && !scope::editing(&idle), "suspend left the grab or the field");
    // suspending an idle pane makes no new value: the sharing (layout) and the open state are unchanged
    let again = scope::suspend(idle.clone());
    check(Rc::ptr_eq(&again.layout, &idle.layout) && again.drag.is_none() && again.context.is_none() && again.highlighted.is_empty(), "suspending an idle pane made a new value");
    // drawn again as the button comes up: no grab, no field, no edit
    let (after, mut changes) = scope_step(&mut ui, idle, &frame_ev(empty, vec![mouse_release(MouseButton::RightButton, empty)]));
    let (after, later) = scope_step(&mut ui, after, &frame_ev(empty, vec![]));
    changes.extend(later);
    check(!scope::Inspect::grabbed(&after) && !scope::editing(&after) && !changes.iter().any(|c| matches!(c, Change::Syntax_edit(_) | Change::Moved(_))), "the pane woke with a grab, a field or an edit");
}

// ---------------------------------------------------------------- pinch

/// A trackpad pinch zooms the graph where the wheel does: at the pointer, the frame's pinch factors
/// multiplied, clamped like the wheel.
fn scope_pinch() {
    let mut ui = new_ui();
    let w = load_workspace("bloom");
    let (view, _) = scope_view(ScopeOpts::default(), &w, "flower");
    let view = scope_step(&mut ui, view, &frame0()).0;
    let before = scope::zoom(&view);
    let point = (500, 350);
    let zoomed = |ui: &mut Ui, events: Vec<Event>| {
        let mut evs = vec![mouse_move(point)];
        evs.extend(events);
        scope::zoom(&scope_step(ui, view.clone(), &frame_ev(point, evs)).0)
    };
    let near = |a: f64, b: f64| (a - b).abs() < 1e-9;
    check(before > 0.3 && before < 2.0, "the bloom framing leaves no room to zoom both ways");
    check(near(zoomed(&mut ui, vec![Event::Pinched(1.25)]), before * 1.25), "a pinch did not zoom the graph by its factor");
    check(near(zoomed(&mut ui, vec![Event::Pinched(0.8)]), before * 0.8), "a pinch below one did not zoom the graph out");
    check(near(zoomed(&mut ui, vec![Event::Pinched(1.1), Event::Pinched(1.1)]), before * 1.21), "two pinches in a frame did not multiply");
    check(zoomed(&mut ui, vec![Event::Pinched(100.0)]) == 2.5, "a huge pinch escaped the zoom clamp");
    check(zoomed(&mut ui, vec![Event::Pinched(0.001)]) == 0.25, "a tiny pinch escaped the zoom clamp");
    check(near(zoomed(&mut ui, vec![Event::Scrolled((0.0, 1.0))]), before * 1.1), "the wheel no longer zooms by a tenth a step");
    // the point under the pointer stays put, as with the wheel
    let anchored = zoomed(&mut ui, vec![Event::Pinched(1.5)]);
    check(anchored > before, "pinch-out did not enlarge");
    // a pinch outside the pane is not the pane's
    let outside = (3000, 3000);
    let unmoved = scope_step(&mut ui, view.clone(), &frame_ev(outside, vec![mouse_move(outside), Event::Pinched(1.5)])).0;
    check(scope::zoom(&unmoved) == before, "a pinch away from the pane zoomed it");
}

// ---------------------------------------------------------------- carry

#[derive(Debug, PartialEq)]
enum Drops {
    Over,
    Dropped,
}

fn drops(changes: &[Change]) -> Vec<(Drops, Path, String, String)> {
    changes
        .iter()
        .filter_map(|c| match c {
            Change::Drop_over { path, kind, value } => Some((Drops::Over, path.clone(), kind.clone(), value.clone())),
            Change::Dropped { path, kind, value } => Some((Drops::Dropped, path.clone(), kind.clone(), value.clone())),
            _ => None,
        })
        .collect()
}

/// A carried payload: the pane reports the node under the pointer (the path), or its empty canvas (the
/// graph's own one-segment path), hovering and then released; it never edits.
fn scope_carry() {
    use crate::support::g::{frame_ev, frame0};
    let mut ui = new_ui();
    let w = load_workspace("bloom");
    let (view, _) = crate::support::g::scope_view(&w, "flower");
    let heart = path(&["flower", "heart"]);
    let view = scope_step(&mut ui, view, &frame0()).0;
    let (x, y, bw, bh) = scope::Inspect::box_of(&view, &heart).unwrap();
    let over = ((x + bw / 2.0) as i64, (y + bh - 4.0) as i64);
    let nothing = (2900, 1900);
    // idle frames report nothing
    let (view, changes) = scope_step(&mut ui, view, &frame_ev(over, vec![mouse_move(over)]));
    check(drops(&changes).is_empty(), "a pane with nothing carried reported a drop");
    ui.carry(None, "material", "(ref cobalt)");
    let (view, changes) = scope_step(&mut ui, view, &frame_ev(over, vec![mouse_move(over)]));
    check(drops(&changes) == vec![(Drops::Over, heart.clone(), "material".to_owned(), "(ref cobalt)".to_owned())], "a carry over a node did not report the node's path");
    let (view, changes) = scope_step(&mut ui, view, &frame_ev(nothing, vec![mouse_move(nothing)]));
    check(drops(&changes) == vec![(Drops::Over, path(&["flower"]), "material".to_owned(), "(ref cobalt)".to_owned())], "a carry over the empty canvas did not report the graph");
    let (view, changes) = scope_step(&mut ui, view, &frame_ev(over, vec![mouse_move(over)]));
    check(drops(&changes).len() == 1, "one place under the pointer");
    let (view, changes) = scope_step(&mut ui, view, &frame_ev(over, vec![mouse_press(MouseButton::LeftButton, over)]));
    check(drops(&changes) == vec![(Drops::Dropped, heart.clone(), "material".to_owned(), "(ref cobalt)".to_owned())], "a put over a node did not emit Dropped with its path");
    check(!changes.iter().any(|c| matches!(c, Change::Syntax_edit(_) | Change::Selected(_) | Change::Moved(_))), "the pane edited, selected or moved on a put");
    check(ui.carrying().is_none(), "the payload outlived its put");
    let (view, changes) = scope_step(&mut ui, view, &frame_ev(over, vec![mouse_release(MouseButton::LeftButton, over)]));
    check(drops(&changes).is_empty(), "the release after a put reported a drop");
    // the marks of a carry paint without changing what the pane reports
    let marked = scope::with_carry(vec![(heart.clone(), "a".to_owned())], Some((heart, true)), view);
    let _ = scope_step(&mut ui, marked, &frame0());
}

// ---------------------------------------------------------------- ghost values

/// A card below Full has no footer; the value of its out-port at the pinned iteration is drawn beside it.
fn scope_ghost_values() {
    let mut ui = new_ui();
    let w = checked("(workspace w (graph g :context value (let* [k 6.0 ys (for [i (range 4)] (let* [a (* k (float i)) b (+ a 1.0)] b)) n (reduce (fn [acc x] (+ acc x)) 0.0 ys)] n)))");
    let sc = Rc::new(p::of_graph(&scope_catalog(), &w, "g").unwrap());
    let view = |index: i64| {
        let o = saved_opts(TestSaved { probe: sc.nodes.iter().filter(|n| n.zone.is_some()).map(|n| (n.path.clone(), index)).collect(), ..Default::default() });
        let v = scope::with_scope(o, "g", sc.clone(), scope::create(Some(1000), Some(700)));
        scope::with_records(recorded(&w), v)
    };
    let ghost = |ui: &mut Ui, index: i64, q: &[&str]| scope::Inspect::ghost(&scope_step(ui, view(index), &frame0()).0, &path(q));
    check(ghost(&mut ui, 0, &["g", "k"]) == Some("6".to_owned()), "a card below Full showed no value beside its out-port");
    check(ghost(&mut ui, 0, &["g", "ys", "b"]) == Some("1".to_owned()) && ghost(&mut ui, 3, &["g", "ys", "b"]) == Some("19".to_owned()), "the ghost value did not follow the pinned iteration");
    let bare = scope::with_scope(ScopeOpts::default(), "g", sc.clone(), scope::create(Some(1000), Some(700)));
    check(scope::Inspect::ghost(&scope_step(&mut ui, bare, &frame0()).0, &path(&["g", "k"])).is_none(), "a ghost value without records");
}

pub const CASES: &[(&str, fn())] = &[
    ("scope_zoom_geometry", scope_zoom_geometry),
    ("scope_levels_a", scope_levels_a),
    ("scope_levels_b", scope_levels_b),
    ("scope_levels_c", scope_levels_c),
    ("scope_connection_hover", scope_connection_hover),
    ("scope_idle_frame", scope_idle_frame),
    ("scope_suspend", scope_suspend),
    ("scope_pinch", scope_pinch),
    ("scope_carry", scope_carry),
    ("scope_ghost_values", scope_ghost_values),
];
