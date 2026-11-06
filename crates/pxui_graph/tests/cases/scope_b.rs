//! test_pxui_graph.ml lines 900-1556: curves, formulas, elbows, spines, footer paths and the first two
//! gesture groups.
use crate::support::*;
use flow_graph::flow_edit as e;
use flow_graph::projection as p;
use flow_syntax::syntax::{self as s, Node};
use input::{Event, Key, MouseButton};
use pxui::ui::Ui;
use pxui_graph::scope_pane as scope;
use pxui_graph::scope_state::{Change, Command, Pane, ScopeOpts};
use std::rc::Rc;

fn syn(op: e::Op) -> Change {
    Change::Syntax_edit(op)
}

fn wires(view: &Pane) -> Vec<Vec<(f64, f64)>> {
    (0..scope::Inspect::wire_count(view)).map(|i| scope::Inspect::wire_points(view, i)).collect()
}

fn settle(ui: &mut Ui, view: Pane) -> Pane {
    scope_step(ui, view, &frame0()).0
}

fn view_of(ui: &mut Ui, text: &str) -> Pane {
    settle(ui, scope_view(ScopeOpts::default(), &checked(text), "g").0)
}

fn down(pts: &[(f64, f64)]) -> bool {
    pts.len() >= 2 && (pts[1].0 - pts[0].0).abs() < 0.5 && pts[1].1 > pts[0].1
}

fn scope_curve() {
    let mut ui = new_ui();
    let w = checked("(workspace c (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [0 3 0])] b)))");
    let view = settle(&mut ui, scope_view(ScopeOpts::default(), &w, "g").0);
    let pts = scope::Inspect::wire_points(&view, 0);
    let level = |v: &[(f64, f64)]| v.len() >= 2 && (v[1].1 - v[0].1).abs() < 0.5;
    let rev: Vec<_> = pts.iter().rev().cloned().collect();
    check(pts.len() > 2 && level(&pts) && level(&rev), "a curve wire does not leave and enter level");
    // a loop variable's wire leaves its begin-bar port downward, clear of the name beside the port
    let view = view_of(&mut ui, "(workspace f (graph g :context value (let* [total (fold [acc 0.0] [k (range 4)] (+ acc (* k 2.0)))] total)))");
    check(wires(&view).iter().any(|w| down(w)), "no begin-bar wire leaves its port downward");
    // a function's parameter wires are ordinary curves: they leave level, not down the bar
    let view = view_of(&mut ui, "(workspace f (graph g :context value (let* [f (fn [(p : float)] (+ p 1.0)) m (map f (list 1.0 2.0))] (sum [x m] x))))");
    check(!wires(&view).iter().any(|w| down(w)), "a function parameter's wire leaves down the begin bar");
    // a call in a vector's cell is a card wired to the row, the wire ending at that cell
    let w = checked("(workspace f (graph g :context host (let* [a (+ [0.0 (* 2.0 3.0)] [1.0 1.0])] a)))");
    let (view, sc) = scope_view(ScopeOpts::default(), &w, "g");
    let view = settle(&mut ui, view);
    check(p::find(&sc, &path(&["g", "a#0#c1"])).is_some(), "the call in a vector cell is not a card");
    check(
        (0..scope::Inspect::wire_count(&view)).any(|i| scope::Inspect::wire_target(&view, i) == Some((path(&["g", "a#0"]), e::ArgKey::Cell(1)))),
        "no wire ends at the vector's cell",
    );
    // taking the wire off from the row's own port disconnects just that cell
    let a_node = p::find(&sc, &path(&["g", "a"])).expect("a");
    let k = a_node.rows.iter().position(|r| !r.cells.is_empty()).expect("cell row");
    let (rx, ry) = scope::Inspect::row_center(&view, &path(&["g", "a"]), k).expect("row center");
    let at = (rx as i64, ry as i64);
    let (hovered, _) = scope_step(&mut ui, view, &frame_ev(at, vec![mouse_move(at)]));
    let (_, ch) = scope::run_command(hovered, Command::Delete);
    check(
        matches!(ch.as_slice(), [Change::Syntax_edit(e::Op::Disconnect { node, key: e::ArgKey::Cell(1), fallback: Some(_) })] if *node == path(&["g", "a#0"])),
        "Delete over a vector row with a wired cell is not a Disconnect of that cell",
    );
    // every card inside a function's zone lies inside the zone's rectangle, cell cards included
    let w = checked("(workspace f (graph g :context host (let* [f (fn [(p : vec3) (h : float)] (+ p [0.0 (* 2.0 h) 0.0])) m (map f (list [0.0 0.0 0.0]) (list 1.0))] m)))");
    let (view, sc) = scope_view(ScopeOpts::default(), &w, "g");
    let view = settle(&mut ui, view);
    let (zx, zy, zw, zh) = scope::Inspect::box_of(&view, &path(&["g", "f"])).expect("zone");
    let inner: Vec<p::Node> = p::zones(&sc).iter().flat_map(|z| z.zone.as_ref().expect("zone").scope.nodes.clone()).filter(|n| n.path.len() == 3).collect();
    check(inner.len() >= 2, "the function's body has its cards");
    for n in &inner {
        let (x, y, w, h) = scope::Inspect::box_of(&view, &n.path).expect("box");
        check(x >= zx - 0.5 && y >= zy - 0.5 && x + w <= zx + zw + 0.5 && y + h <= zy + zh + 0.5, &format!("{} leaves its zone", n.path.join("/")));
    }
    // a wire does not run over a card it neither leaves nor enters
    let cards: Vec<(f64, f64, f64, f64)> = sc.nodes.iter().chain(inner.iter()).filter(|n| n.zone.is_none()).filter_map(|n| scope::Inspect::box_of(&view, &n.path)).collect();
    let inside = |(x, y): (f64, f64), (bx, by, bw, bh): (f64, f64, f64, f64)| x > bx - 0.5 && x < bx + bw + 0.5 && y > by - 0.5 && y < by + bh + 0.5;
    let shrunk = |(bx, by, bw, bh): (f64, f64, f64, f64)| (bx + 3.0, by + 3.0, bw - 6.0, bh - 6.0);
    for pts in wires(&view) {
        let ends = [pts[0], *pts.last().expect("pts")];
        for seg in pts.windows(2) {
            let ((x0, y0), (x1, y1)) = (seg[0], seg[1]);
            for b in &cards {
                if !ends.iter().any(|en| inside(*en, *b)) {
                    for k in 0..=20 {
                        let t = k as f64 / 20.0;
                        check(!inside((x0 + (x1 - x0) * t, y0 + (y1 - y0) * t), shrunk(*b)), "a wire runs over a card it does not enter");
                    }
                }
            }
        }
    }
}

fn scope_field_formula() {
    let mut ui = new_ui();
    let w = checked("(workspace r (graph g :context value (let* [a 2.0 r {:x (+ a 1.0)}] r.x)))");
    let view = settle(&mut ui, scope_view(ScopeOpts::default(), &w, "g").0);
    let pth = path(&["g", "r"]);
    let sc = p::of_graph(&scope_catalog(), &w, "g").expect("scope");
    let row = p::find(&sc, &pth).expect("r").rows.iter().position(|r| r.key == e::ArgKey::Field("x".into())).expect("row");
    let (x, y) = scope::Inspect::expr_chip(&view, &pth, row).expect("chip");
    // a double click on the formula opens it, as its glyph does
    let at = (x as i64, y as i64);
    let (v, _) = scope_click(&mut ui, view, at);
    let (_, changes) = scope_click(&mut ui, v, at);
    let unfolded = changes.iter().any(|c| matches!(c, Change::Syntax_edit(e::Op::Unfold { key: e::ArgKey::Field(f), .. }) if f == "x"));
    check(unfolded, "the formula glyph of a record field did not unfold it");
}

fn scope_elbow() {
    let mut ui = new_ui();
    let w = checked("(workspace s (graph g :context value (let* [a (array/range 3) b (map (fn [i] (* i 2.0)) a) c (array/nth b 0)] c)))");
    // c sits 40 points left of its column and b 30 to the right: the elbow still leaves and enters cleanly
    let mut o = ScopeOpts::default();
    o.at = Rc::new(|q: &[String]| {
        if q == ["g", "c"] {
            Some((-40.0, 250.0))
        } else if q == ["g", "b"] {
            Some((30.0, 100.0))
        } else if q == ["g", "a"] {
            Some((0.0, 0.0))
        } else {
            None
        }
    });
    let view = settle(&mut ui, scope_view(o, &w, "g").0);
    let ws = wires(&view);
    check(!ws.is_empty(), "no wires");
    for pts in ws {
        if pts.len() >= 2 {
            let (x0, x1) = (pts[0].0, pts[1].0);
            let xe = pts.last().expect("pts").0;
            check(x1 >= x0, "an elbow leaves its port leftwards");
            check(pts.iter().all(|(x, _)| *x >= x0.min(xe) - 13.0), "an elbow hooks left of its cards");
        }
    }
}

fn scope_spine() {
    let mut ui = new_ui();
    let view = view_of(&mut ui, "(workspace s (graph g :context value (let* [ys (map (fn [i] (* i 2.0)) (array/range 3))] (array/nth ys 0))))");
    let into_f: Vec<Vec<(f64, f64)>> =
        (0..scope::Inspect::wire_count(&view)).filter(|i| scope::Inspect::wire_target(&view, *i) == Some((path(&["g", "ys"]), e::ArgKey::Pos(0)))).map(|i| scope::Inspect::wire_points(&view, i)).collect();
    // the wire leaves the function zone's out-port on its right edge and enters map's f on its left edge
    let (zx, zy, zw, zh) = scope::Inspect::box_of(&view, &path(&["g", "ys#0"])).expect("zone");
    let (mx, my, _, _) = scope::Inspect::box_of(&view, &path(&["g", "ys"])).expect("map");
    check(
        match into_f.as_slice() {
            [pts] => {
                let (x0, y0) = pts[0];
                let (x1, y1) = *pts.last().expect("pts");
                (x0 - (zx + zw)).abs() < 1.0 && y0 >= zy && y0 <= zy + zh && (x1 - mx).abs() < 1.0 && y1 > my
            }
            _ => false,
        },
        "a map's stacked function is not wired from its out-port into map's f socket",
    );
    // zones are obstacles for a wire that does not enter them: [k] feeds [r], right of the zone
    let w = checked("(workspace s (graph g :context value (let* [k 2.0 ys (map (fn [i] (* i k)) (array/range 3)) r (+ (array/nth ys 0) k)] r)))");
    let mut o = ScopeOpts::default();
    o.at = Rc::new(|q: &[String]| match q.iter().map(|s| s.as_str()).collect::<Vec<_>>().as_slice() {
        ["g", "k"] => Some((0.0, 0.0)),
        ["g", "ys#0"] => Some((300.0, 96.0)),
        ["g", "ys"] => Some((300.0, 336.0)),
        ["g", "r"] => Some((1000.0, 360.0)),
        _ => None,
    });
    let view = settle(&mut ui, scope_view(o, &w, "g").0);
    let (zx, zy, zw, zh) = scope::Inspect::box_of(&view, &path(&["g", "ys#0"])).expect("zone");
    let (kx, ky, kw, _) = scope::Inspect::box_of(&view, &path(&["g", "k"])).expect("k");
    let (rx, ry, _, _) = scope::Inspect::box_of(&view, &path(&["g", "r"])).expect("r");
    // the straight line between the two ends passes through the zone
    let line = |x: f64| ky + 12.0 + ((ry - ky) * (x - (kx + kw)) / (rx - kx - kw));
    check(line(zx + zw / 2.0) > zy && line(zx + zw / 2.0) < zy + zh, "the zone is off the line");
    for i in 0..scope::Inspect::wire_count(&view) {
        if let Some((t, _)) = scope::Inspect::wire_target(&view, i) {
            if t == path(&["g", "r"]) {
                for (x, y) in scope::Inspect::wire_points(&view, i) {
                    check(!(x > zx + 1.0 && x < zx + zw - 1.0 && y > zy + 1.0 && y < zy + zh - 1.0), "a wire that does not enter a zone runs through it");
                }
            }
        }
    }
}

fn scope_zone_footer_paths() {
    let mut ui = new_ui();
    let view = view_of(&mut ui, "(workspace z (graph g :context sop (let* [a (sop/box) b (sop/with_attr a :P (exact (map (fn [(p : vec3)] p) (sop/attr a :P))))] b)))");
    let mouse = (990, 350);
    let f = scope::zoom(&view);
    let (view, _) = scope_step(&mut ui, view, &frame_ev(mouse, vec![mouse_move(mouse), Event::Pinched(2.5 / f)]));
    let (view, _) = scope_step(&mut ui, view, &frame_ev(mouse, vec![]));
    let (ax, _, aw, _) = scope::Inspect::box_of(&view, &path(&["g", "a"])).expect("a");
    check(ax + aw < 0.0, "the geometry node is meant to be off screen");
    check(scope::Inspect::footer_paths(&view).contains(&path(&["g", "a"])), "a zone in view lost its geometry");
}

// ---------------------------------------------------------------- gestures 1

fn num(n: &str) -> s::Syntax {
    s::make(s::Draft::Num(n.to_owned()))
}

fn set_args(changes: &[Change]) -> Vec<e::Op> {
    changes.iter().filter_map(|c| if let Change::Syntax_edit(op @ e::Op::SetArg { .. }) = c { Some(op.clone()) } else { None }).collect()
}

fn written(ops: &[e::Op]) -> Vec<String> {
    ops.iter().filter_map(|op| if let e::Op::SetArg { value, .. } = op { if let Node::Num(n) = value.node { Some(value.name(n).to_owned()) } else { None } } else { None }).collect()
}

fn scrub(ui: &mut Ui, (view, from): (Pane, (i64, i64)), by: i64) -> Vec<e::Op> {
    let (view, _) = scope_step(ui, view, &g::frame(from, vec![], vec![mouse_move(from), mouse_press(MouseButton::LeftButton, from)]));
    let towards = (from.0 + by, from.1);
    let (view, moved) = if by == 0 { (view, vec![]) } else { scope_step(ui, view, &g::frame(towards, vec![], vec![mouse_move(towards)])) };
    let (_, released) = scope_step(ui, view, &g::frame(towards, vec![], vec![mouse_release(MouseButton::LeftButton, towards)]));
    let mut all = moved;
    all.extend(released);
    set_args(&all)
}

fn scope_gestures_1() {
    let mut ui = new_ui();
    // rename: a double-click on the title opens a field, Enter commits Rename; F2 does the same
    let w = load_workspace("bloom");
    let heart = g::heart();
    let view = g::settled(&mut ui, g::scope_view(&w, "flower").0);
    let (x, y, _, _) = g::center_of(&view, &heart);
    let (view, _) = g::double_click(&mut ui, view, (x as i64 + 100, y as i64 + 8));
    check(scope::editing(&view), "a double-click on a title did not open the name field");
    let view = g::settled(&mut ui, view);
    let (view, changes) = g::type_text(&mut ui, view, "core");
    check(changes.contains(&syn(e::Op::Rename { node: heart.clone(), to_: "core".into() })), "typing a name did not become Rename");
    check(!scope::editing(&view), "the name field stayed open after Enter");
    let (view, changes) = scope::run_command(scope::select(vec![heart.clone()], view), Command::Edit_name);
    check(changes.is_empty() && scope::editing(&view), "F2 did not open the name field");
    let view = g::settled(&mut ui, view);
    let (view, changes) = scope_step(&mut ui, view, &g::frame((0, 0), vec![], vec![Event::Text_input("two words".into()), Event::Key_pressed(Key::Enter)]));
    check(!changes.iter().any(|c| matches!(c, Change::Syntax_edit(e::Op::Rename { .. }))), "a name with a space was accepted");
    let view = scope_step(&mut ui, view, &g::frame((0, 0), vec![], vec![Event::Key_pressed(Key::Escape)])).0;
    check(!scope::editing(&view), "Escape left the name field open");
    let (_, changes) = scope::run_command(scope::select(vec![], view.clone()), Command::Edit_name);
    check(matches!(changes.as_slice(), [Change::Notice(_)]), "F2 with nothing selected");
    // a graph input's default: double-click its tile, type a form
    let petals = path(&["flower", ":petals"]);
    let view = g::settled(&mut ui, scope::clear_selection(view));
    let (x, y, _, _) = g::center_of(&view, &petals);
    let (view, _) = g::double_click(&mut ui, view, (x as i64 + 60, y as i64 + 12));
    check(scope::editing(&view), "a double-click on a graph input did not open its default");
    let view = g::settled(&mut ui, view);
    let (_, changes) = g::type_text(&mut ui, view, "9");
    check(
        changes.iter().any(|c| matches!(c, Change::Syntax_edit(e::Op::SetInputDefault { form, input, value }) if form == "flower" && input == "petals" && matches!(value.node, Node::Num(n) if value.name(n) == "9"))),
        "typing an input default did not become Set_input_default",
    );
    // scrub: a drag on a number field is Set_arg
    let view = g::settled(&mut ui, g::scope_view(&w, "flower").0);
    let node = p::find(&p::of_graph(&scope_catalog(), &w, "flower").expect("scope"), &heart).expect("heart");
    let i = node.rows.iter().position(|r| r.label == "radius").expect("radius");
    let (fx, fy) = scope::Inspect::row_center(&view, &heart, i).expect("row");
    let from = (fx as i64, fy as i64);
    let (view, _) = scope_step(&mut ui, view, &g::frame(from, vec![], vec![mouse_move(from), mouse_press(MouseButton::LeftButton, from)]));
    let towards = (from.0 + 40, from.1);
    let (view, moved) = scope_step(&mut ui, view, &g::frame(towards, vec![], vec![mouse_move(towards)]));
    let (_, released) = scope_step(&mut ui, view, &g::frame(towards, vec![], vec![mouse_release(MouseButton::LeftButton, towards)]));
    let mut all = moved;
    all.extend(released);
    check(all.iter().any(|c| matches!(c, Change::Syntax_edit(e::Op::SetArg { node, sub, .. }) if sub.len() == 1 && *node == heart)), "scrubbing a vector field did not become Set_arg");
}

fn scope_gestures_1b() {
    let mut ui = new_ui();
    // A float parameter written as an integer scrubs as a float (the kind is the row's type), a long
    // value keeps its digits, a click writes nothing and a drag is one history entry
    let nw = checked("(workspace n (graph g :context sop (let* [a (sop/uv_sphere :radius 1) b (sop/transform a :translate [1234567.89 2 3])] b)))");
    let field = |ui: &mut Ui, pth: &[&str], label: &str, inset: f64| -> (Pane, (i64, i64)) {
        let pth = path(pth);
        let view = g::settled(ui, g::scope_view(&nw, "g").0);
        let node = p::find(&p::of_graph(&scope_catalog(), &nw, "g").expect("scope"), &pth).expect("node");
        let i = node.rows.iter().position(|r| r.label == label).expect("row");
        let (bx, _, bw, _) = scope::Inspect::box_of(&view, &pth).expect("box");
        let ry = scope::Inspect::row_center(&view, &pth, i).expect("row").1;
        (view, ((bx + bw - inset) as i64, ry as i64))
    };
    // at two places: a second press at one would be a double-click, which types
    let f = field(&mut ui, &["g", "a"], "radius", 20.0);
    check(scrub(&mut ui, f, 0).is_empty(), "a click on a number field wrote a value");
    let f = field(&mut ui, &["g", "a"], "radius", 60.0);
    let radius = scrub(&mut ui, f, 10);
    check(
        match written(&radius).first() {
            Some(n) => n.contains('.') && n.parse::<f64>().map(|v| v.fract() != 0.0).unwrap_or(false),
            None => false,
        },
        &format!("a float written 1 scrubbed as an integer: {}", written(&radius).join(" ")),
    );
    check(radius.iter().all(|op| e::gesture(op).is_some() && e::gesture(op) == e::gesture(&radius[0])), "a scrub is not one history entry");
    let f = field(&mut ui, &["g", "b"], "translate", 110.0);
    let cell = scrub(&mut ui, f, 2);
    check(written(&cell).contains(&"1234567.99".to_owned()), &format!("a scrub of 1234567.89 lost its digits: {}", written(&cell).join(" ")));
    // Explicit non-finite values are editable on value cards, while ranged catalog fields keep their
    // finite-value boundary.
    let source = s::parse("(workspace literals (graph g :context value (let* [x 0.5] x)))").expect("parse");
    let check_ws = |source: &[s::Syntax]| match flow_check::workspace::check(&[], false, false, None, &scope_catalog(), source) {
        (Some(w), ds) if ds.is_empty() => w,
        _ => fail("literal workspace did not check"),
    };
    let value_field = |ui: &mut Ui, source: &[s::Syntax]| -> (Pane, (i64, i64)) {
        let view = g::settled(ui, g::scope_view(&check_ws(source), "g").0);
        let (x, y, w, _) = g::center_of(&view, &path(&["g", "x"]));
        (view, ((x + w - 40.0) as i64, (y + 12.0) as i64))
    };
    let edit = |ui: &mut Ui, (view, point): (Pane, (i64, i64)), text: &str| -> Vec<e::Op> {
        let mut events = vec![mouse_move(point)];
        events.extend(click(point));
        let (view, _) = scope_step(ui, view, &g::frame(point, vec![Key::Alt], events));
        let (view, changes) = g::type_text(ui, view, text);
        let _ = scope_step(ui, view, &g::frame((0, 0), vec![], vec![Event::Key_pressed(Key::Escape)]));
        set_args(&changes)
    };
    for text in ["NaN", "Infinity", "-Infinity"] {
        let vf = value_field(&mut ui, &source);
        let ops = edit(&mut ui, vf, text);
        check(written(&ops) == vec![text.to_owned()], &format!("value card refused {text}"));
        let (edited, workspace) = e::apply_checked(&scope_catalog(), &source, &ops[0]).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        let ev = flow_eval::run(&workspace, false, None, None, None, 0.0).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        let actual = ev.results.iter().find(|(n, _)| n == "g").expect("g").1.clone();
        check(matches!(actual, flow_eval::Value::Float(f) if flow_syntax::lisp::float(f) == text), &format!("value card did not evaluate {text}"));
        let vf = value_field(&mut ui, &edited);
        check(scrub(&mut ui, vf, 10).is_empty(), &format!("scrubbing {text} changed its value"));
        let vf = value_field(&mut ui, &edited);
        check(written(&edit(&mut ui, vf, "7.5")) == vec!["7.5".to_owned()], &format!("value card could not replace {text}"));
    }
    let f = field(&mut ui, &["g", "a"], "radius", 60.0);
    let rejected = written(&edit(&mut ui, f, "NaN"));
    check(rejected.is_empty(), &format!("a ranged radius field accepted NaN: {}", rejected.join(",")));
    let _ = num;
}

// ---------------------------------------------------------------- gestures 2

fn scope_gestures_2() {
    let mut ui = new_ui();
    let w = load_workspace("bloom");
    let heart = g::heart();
    // the selection commands and their requests
    let view = g::settled(&mut ui, scope::select(vec![heart.clone()], g::scope_view(&w, "flower").0));
    let only = |c: Command| scope::run_command(view.clone(), c).1;
    let h = || vec![heart.clone()];
    check(only(Command::Fold_into) == vec![syn(e::Op::FoldInto { node: heart.clone() })], "f: Fold_into");
    check(only(Command::Hoist) == vec![syn(e::Op::Hoist { node: heart.clone() })], "h: Hoist");
    check(only(Command::Wrap_repeat) == vec![syn(e::Op::Wrap { nodes: h(), loop_: e::Loop::For })], "r: Wrap For");
    check(only(Command::Wrap_iterate) == vec![syn(e::Op::Wrap { nodes: h(), loop_: e::Loop::Fold })], "Shift-r: Wrap Fold");
    check(only(Command::Wrap_map) == vec![syn(e::Op::Wrap { nodes: h(), loop_: e::Loop::Map })], "Shift-m: Wrap Map");
    check(only(Command::Wrap_filter) == vec![syn(e::Op::Wrap { nodes: h(), loop_: e::Loop::Filter })], "Shift-l: Wrap Filter");
    check(only(Command::Inline_fn) == vec![syn(e::Op::InlineDefn { node: heart.clone() })], "Alt-Shift-i: Inline_defn");
    check(only(Command::Item_up) == vec![syn(e::Op::MoveBinding { node: heart.clone(), by: -1 })], "Alt-Up off a list: Move_binding");
    check(only(Command::Item_down) == vec![syn(e::Op::MoveBinding { node: heart.clone(), by: 1 })], "Alt-Down off a list: Move_binding");
    check(only(Command::Make_fn) == vec![syn(e::Op::MakeLocalFn { nodes: h() })], "l: Make_local_fn");
    check(only(Command::Duplicate) == vec![syn(e::Op::Duplicate { nodes: h() })], "Command-D: Duplicate");
    check(only(Command::Copy) == vec![Change::Copy_requested(h())], "Command-C: Copy_requested");
    check(only(Command::Cut) == vec![Change::Copy_requested(h()), syn(e::Op::DeleteNodes { nodes: h() })], "Command-X: copy, then delete");
    check(only(Command::Paste) == vec![Change::Paste_requested], "Command-V: Paste_requested");
    // a screen point as a grid-snapped position in a scope: further right on screen is further right in the scope
    {
        let (x, y, _, _) = g::center_of(&view, &heart);
        match (scope::scope_point(&view, &path(&["flower"]), x, y), scope::scope_point(&view, &path(&["flower"]), x + 60.0, y)) {
            (Some((ax, ay)), Some((bx, _))) => check(ax % 12.0 == 0.0 && ay % 12.0 == 0.0 && bx > ax, "scope_point snaps to 12 and follows the pointer"),
            _ => fail("scope_point found no scope"),
        }
    }
    check(scope::scope_point(&view, &path(&["nosuch"]), 0.0, 0.0).is_none(), "an unknown scope has no point");
    // fold and unfold: the keys, and the right-click menu's rows
    let result = path(&["flower", "result"]);
    match scope::run_command(scope::select(vec![result.clone()], view.clone()), Command::Unfold).1.as_slice() {
        [Change::Syntax_edit(e::Op::Unfold { node, sub, .. })] if *node == result && sub.is_empty() => {}
        _ => fail("Shift-U on a call with an inline call did not become Unfold"),
    }
    let (hx, hy, _, _) = g::center_of(&view, &heart);
    let at = (hx as i64 + 20, hy as i64 + 8);
    let (menu, _) = scope_step(&mut ui, view.clone(), &g::frame(at, vec![], vec![mouse_move(at), mouse_press(MouseButton::RightButton, at), mouse_release(MouseButton::RightButton, at)]));
    let (menu, _) = scope_step(&mut ui, menu, &g::frame(at, vec![], vec![]));
    let item = |i: i64| (at.0 + 30, at.1 + 3 + 24 * i + 12);
    let (_, picked) = scope_step(&mut ui, menu, &g::frame(item(0), vec![], vec![mouse_move(item(0)), mouse_press(MouseButton::LeftButton, item(0)), mouse_release(MouseButton::LeftButton, item(0))]));
    check(picked.contains(&syn(e::Op::FoldInto { node: heart.clone() })), "the right-click menu's first row did not become Fold_into");
    // a right-click on empty canvas asks the host for its add menu at that point
    let empty = (2900, 1900);
    let (_, requested) = scope_step(&mut ui, view.clone(), &g::frame(empty, vec![], vec![mouse_move(empty), mouse_press(MouseButton::RightButton, empty), mouse_release(MouseButton::RightButton, empty)]));
    check(requested.contains(&Change::Menu_requested(2900.0, 1900.0)), "a right-click on empty canvas did not ask for the add menu");
    check(only(Command::Display) == vec![Change::Display_set(heart.clone())], "v: a geometry node is shown in the viewport");
    match scope::run_command(scope::select(vec![path(&["flower", "petals"])], view.clone()), Command::Display).1.as_slice() {
        [Change::Notice(_)] => {}
        _ => fail("v on a number did not say only geometry can be viewed"),
    }
    check(matches!(scope::run_command(scope::select(vec![], view.clone()), Command::Duplicate).1.as_slice(), [Change::Notice(_)]), "Command-D with nothing selected did not say so");
    // the viewed node carries the VIEW mark; the key bindings are the Command table's
    let mut o = ScopeOpts::default();
    o.display = Some(heart.clone());
    let sc = Rc::new(p::of_graph(&scope_catalog(), &w, "flower").expect("scope"));
    let _viewed = scope::with_scope(o, "flower", sc, view.clone());
    for (id, modifier) in [("scope.duplicate", Key::Meta), ("scope.duplicate", Key::Ctrl), ("scope.display", Key::Shift)] {
        let mods = if id == "scope.display" { vec![] } else { vec![modifier] };
        let ch = if id == "scope.display" { 'v' } else { 'd' };
        check(
            scope::bindings().iter().any(|(c, _)| c.id == id && c.trigger == Some(editor_core::editor_core::keymap::Trigger::Chord(Key::KeyChar(ch), mods.clone()))),
            &format!("{id} is not bound"),
        );
    }
}

pub const CASES: &[(&str, fn())] = &[
    ("scope_curve", scope_curve),
    ("scope_field_formula", scope_field_formula),
    ("scope_elbow", scope_elbow),
    ("scope_spine", scope_spine),
    ("scope_zone_footer_paths", scope_zone_footer_paths),
    ("scope_gestures_1", scope_gestures_1),
    ("scope_gestures_1b", scope_gestures_1b),
    ("scope_gestures_2", scope_gestures_2),
];
