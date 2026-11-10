//! List/record/expression-chip gestures and scope framing/marquee contracts.
use crate::support::*;
use flow_graph::edit_tree::ArgKey;
use flow_graph::flow_edit::Op;
use flow_graph::projection as p;
use flow_syntax::syntax as s;
use input::{Event, Key, MouseButton};
use pxui_graph::scope_pane as scope;
use pxui_graph::scope_state::{Change, Command, ScopeOpts};
use std::rc::Rc;

fn syn(op: Op) -> Change {
    g::syntax(op)
}

fn ipt(p: (f64, f64)) -> (i64, i64) {
    (p.0 as i64, p.1 as i64)
}

fn find_node(nodes: &[p::Node], path: &[flow_syntax::symbol::Symbol]) -> Option<p::Node> {
    for n in nodes {
        if n.path == path {
            return Some(n.clone());
        }
        if let Some(z) = &n.zone {
            if let Some(f) = find_node(&z.scope.nodes, path) {
                return Some(f);
            }
        }
    }
    None
}

fn records(nodes: &[p::Node]) -> Vec<p::Node> {
    let mut out = Vec::new();
    for n in nodes {
        if n.head == "record" && n.zone.is_none() {
            out.push(n.clone());
        }
        if let Some(z) = &n.zone {
            out.extend(records(&z.scope.nodes));
        }
    }
    out
}

fn add_row(view: &scope::Pane, sc: &p::Scope, path: &[flow_syntax::symbol::Symbol]) -> (i64, i64) {
    let n = find_node(&sc.nodes, path).expect("node");
    let i = n.rows.iter().position(|r| r.kind == p::RowKind::Add).expect("add row");
    ipt(scope::inspect::row_center(view, path, i).expect("row center"))
}

fn scope_gestures_3() {
    let mut ui = new_ui();
    // list rows: the + item row, the up arrow and Alt-Up move an item, a record's + field row
    let kw = load_workspace("kit");
    let kit_scope = p::of_graph(&scope_catalog(), &kw, "kit").expect("kit");
    let (view, _) = g::scope_view(&kw, "kit");
    let view = g::settled(&mut ui, view);
    let widths = path(&["kit", "widths"]);
    let node = p::find(&kit_scope, &widths).expect("widths");
    let add = node.rows.iter().position(|r| r.kind == p::RowKind::Add).expect("add");
    let (ax, ay) = scope::inspect::row_center(&view, &widths, add).expect("c");
    let (_, changes) = g::scope_click(&mut ui, view.clone(), (ax as i64, ay as i64));
    check(changes.contains(&syn(Op::AddItem { node: widths.clone() })), "the + item row did not become Add_item");
    let (bx, _by, bw, _) = g::center_of(&view, &widths);
    let second = node.rows.iter().position(|r| r.key == ArgKey::Pos(1)).expect("second");
    let ux = bx + bw - (14.0 * scope::zoom(&view));
    let uy = scope::inspect::row_center(&view, &widths, second).expect("c").1;
    let (_, changes) = g::scope_click(&mut ui, view.clone(), (ux as i64, uy as i64));
    check(changes.contains(&syn(Op::MoveItem { node: widths.clone(), pos: 1 })), "the row arrow did not become Move_item");
    let hover = (scope::inspect::row_center(&view, &widths, second).expect("c").0 as i64, uy as i64);
    let (hovered, _) = scope_step(&mut ui, view.clone(), &g::frame(hover, Vec::new(), vec![mouse_move(hover)]));
    check({ let mut r8_view = hovered.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Item_up, &mut r8_changes); (r8_view, r8_changes) }.1 == vec![syn(Op::MoveItem { node: widths.clone(), pos: 1 })], "Alt-Up: Move_item");
    check({ let mut r8_view = hovered.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Item_down, &mut r8_changes); (r8_view, r8_changes) }.1 == vec![syn(Op::MoveItem { node: widths.clone(), pos: 2 })], "Alt-Down: Move_item");
    // Delete over an item of a variadic input removes the item, whatever it holds
    check(
        { let mut r8_view = hovered.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Delete, &mut r8_changes); (r8_view, r8_changes) }.1 == vec![syn(Op::Disconnect { node: widths.clone(), key: ArgKey::Pos(1), fallback: None })],
        "Delete on a hovered list item did not remove it",
    );
    check(matches!({ let mut r8_view = view.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Item_up, &mut r8_changes); (r8_view, r8_changes) }.1.as_slice(), [Change::Notice(_)]), "Alt-Up with no hovered item");
    match records(&kit_scope.nodes).first() {
        Some(record) => {
            let add = record.rows.iter().position(|r| r.kind == p::RowKind::Add).expect("add");
            if let Some((rx, ry)) = scope::inspect::row_center(&view, &record.path, add) {
                let (_, changes) = g::scope_click(&mut ui, view.clone(), (rx as i64, ry as i64));
                check(
                    changes.iter().any(|c| matches!(c, Change::Syntax_edit(Op::AddField { node, .. }) if *node == record.path)),
                    "the + field row did not become Add_field",
                );
            }
        }
        None => fail("kit has no record node"),
    }
    // the new field takes the first name the record does not have
    let rw = checked("(workspace r (graph g :context sop (let* [r {:f2 5} a (sop/uv_sphere :radius r.f2)] a)))");
    let (rview, rscope) = g::scope_view(&rw, "g");
    let (rview, _) = scope_step(&mut ui, rview, &frame0());
    let rn = p::find(&rscope, &path(&["g", "r"])).expect("r");
    let ri = rn.rows.iter().position(|r| r.kind == p::RowKind::Add).expect("add");
    let (rx, ry) = scope::inspect::row_center(&rview, &path(&["g", "r"]), ri).expect("c");
    let (_, changes) = g::scope_click(&mut ui, rview, (rx as i64, ry as i64));
    check(changes.iter().any(|c| matches!(c, Change::Syntax_edit(Op::AddField { name, .. }) if name == "f1")), "the + field row did not pick the first unused name");
    // a wire taken off a variadic input takes the item with it: no nil is left behind
    let mw = checked("(workspace m (graph g :context sop (let* [a (sop/box) b (sop/box) m (sop/merge a b)] m)))");
    let mview = g::settled(&mut ui, g::scope_view(&mw, "g").0);
    let target = Some((path(&["g", "m"]), ArgKey::Pos(1)));
    let wi = (0..scope::inspect::wire_count(&mview)).find(|i| scope::inspect::wire_target(&mview, *i) == target).expect("wire");
    let (wx, wy) = scope::inspect::wire_midpoint(&mview, wi).expect("mid");
    let (picked, _) = g::scope_click(&mut ui, mview, (wx as i64, wy as i64));
    check(
        { let mut r8_view = picked; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Delete, &mut r8_changes); (r8_view, r8_changes) }.1 == vec![syn(Op::Disconnect { node: path(&["g", "m"]), key: ArgKey::Pos(1), fallback: None })],
        "a wire off a variadic input did not remove the item",
    );
    // + output names the new output first; a row that only a wire can fill says so
    let (oview, oscope) = g::scope_view(&kw, "def:window");
    let oview = g::settled(&mut ui, oview);
    let values = oscope.nodes.iter().find(|n| n.head == "values").expect("values").path.clone();
    let at = add_row(&oview, &oscope, &values);
    let (naming, changes) = g::scope_click(&mut ui, oview, at);
    check(changes.is_empty() && scope::editing(&naming), "the + output row did not open a name field");
    let (_, changes) = scope_step(&mut ui, naming, &g::frame((0, 0), Vec::new(), vec![Event::Text_input("depth".into()), Event::Key_pressed(Key::Enter)]));
    check(
        changes.contains(&syn(Op::AddField { node: values.clone(), name: "depth".into(), value: s::make(s::Draft::Num("0".into())) })),
        "a named output did not become Add_field",
    );
    let cw = checked("(workspace c (graph g :context value (let* [xs (list 1 2) ys (concat xs xs)] (count ys))))");
    let (cview, cscope) = g::scope_view(&cw, "g");
    let cview = g::settled(&mut ui, cview);
    let at = add_row(&cview, &cscope, &path(&["g", "ys"]));
    check(matches!(g::scope_click(&mut ui, cview.clone(), at).1.as_slice(), [Change::Notice(_)]), "the + list row of a concat did not ask for a wire");
    // a click on the name a row reads selects that binding, so the inspector follows it
    let ys = p::find(&cscope, &path(&["g", "ys"])).expect("ys");
    let ri = ys.rows.iter().position(|r| r.chip == p::Chip::Name("xs".into())).expect("xs row");
    let (x, y) = scope::inspect::ref_chip(&cview, &path(&["g", "ys"]), ri).expect("chip");
    let (_, changes) = g::scope_click(&mut ui, cview, (x as i64, y as i64));
    check(changes.contains(&Change::Selected(vec![path(&["g", "xs"])])), "a click on a read name did not select its binding");
    // a click on an expression chip selects its card and the row, [holder @ [":" ^ label]]
    let ew = checked("(workspace e (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a))] (+ a b))))");
    let (eview, escope) = g::scope_view(&ew, "g");
    let eview = g::settled(&mut ui, eview);
    let eb = p::find(&escope, &path(&["g", "b"])).expect("b");
    let ei = eb.rows.iter().position(|r| matches!(r.chip, p::Chip::Inline { .. })).expect("inline");
    let label = eb.rows[ei].label.clone();
    let (x, y) = scope::inspect::expr_chip(&eview, &path(&["g", "b"]), ei).expect("chip");
    let (eview, changes) = g::scope_click(&mut ui, eview, (x as i64, y as i64));
    let want = vec![path(&["g", "b"]), vec!["g".into(), "b".into(), format!(":{}", label).into()]];
    check(changes.contains(&Change::Selected(want.clone())), "a click on an expression chip did not select its row");
    check(scope::selected(&eview) == want, "the pane did not keep the row selected beside its card");
}

/// Drag presses at a, moves to b and releases; return move/release changes in order.
fn drag(ui: &mut pxui::ui::Ui, view: scope::Pane, shift: bool, a: (i64, i64), b: (i64, i64)) -> (scope::Pane, Vec<Change>) {
    let mut evs = Vec::new();
    if shift {
        evs.push(Event::Key_pressed(Key::Shift));
    }
    evs.push(mouse_move(a));
    evs.push(mouse_press(MouseButton::LeftButton, a));
    let (view, _) = scope_step(ui, view, &g::frame(a, Vec::new(), evs));
    let (view, moved) = scope_step(ui, view, &g::frame(b, Vec::new(), vec![mouse_move(b)]));
    let (view, released) = scope_step(ui, view, &g::frame(b, Vec::new(), vec![mouse_release(MouseButton::LeftButton, b)]));
    let mut all = moved;
    all.extend(released);
    (view, all)
}

fn scope_gestures_4() {
    let mut ui = new_ui();
    let w = load_workspace("bloom");
    let heart = g::heart();
    let bloom = path(&["flower", "bloom"]);
    // frames: Shift-G makes one around the selection, the corner resizes it, the cross deletes it
    let view = g::settled(&mut ui, { let mut r8_view = g::scope_view(&w, "flower").0; scope::select(vec![heart.clone(), bloom.clone()], &mut r8_view); r8_view });
    let (view, changes) = { let mut r8_view = view; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Make_frame, &mut r8_changes); (r8_view, r8_changes) };
    let made = match changes.as_slice() {
        [Change::Frames_set { scope: sc, frames }] if *sc == path(&["flower"]) && frames.len() == 1 => frames[0].clone(),
        _ => fail("Shift-G did not make one frame"),
    };
    let (title, at, (fw, fh)) = made.clone();
    let made2 = made.clone();
    let framed = { let mut r8_view = view; scope::with_scope(saved_opts(TestSaved { frames: vec![(path(&["flower"]), vec![made2.clone()])], ..Default::default() }), "flower", Rc::new(p::of_graph(&scope_catalog(), &w, "flower").expect("flower")), &mut r8_view); r8_view };
    let framed = g::settled(&mut ui, framed);
    let (fx0, fy0) = at;
    // the frame's screen position: its scope's origin is the pane's, so use the pan through a tile
    let (tile_x, tile_y, _, _) = g::center_of(&framed, &heart);
    let mut lo = p::LayoutOptions::new();
    lo.foot = true;
    let heart_rel = p::layout(&lo, &p::of_graph(&scope_catalog(), &w, "flower").expect("flower"));
    let hp = heart_rel.placed.iter().find(|q| q.path == heart).expect("placed");
    let z = scope::zoom(&framed);
    let sx0 = tile_x - (hp.x * z);
    let sy0 = tile_y - (hp.y * z);
    let corner = ((sx0 + ((fx0 + fw) * z) - 6.0) as i64, (sy0 + ((fy0 + fh) * z) - 6.0) as i64);
    let drag_to = (corner.0 + 30, corner.1 + 20);
    let (sized, _) = scope_step(&mut ui, framed.clone(), &g::frame(corner, Vec::new(), vec![mouse_move(corner), mouse_press(MouseButton::LeftButton, corner)]));
    let (sized, _) = scope_step(&mut ui, sized, &g::frame(drag_to, Vec::new(), vec![mouse_move(drag_to)]));
    let (_, changes) = scope_step(&mut ui, sized, &g::frame(drag_to, Vec::new(), vec![mouse_release(MouseButton::LeftButton, drag_to)]));
    let grown = changes.iter().find_map(|c| match c {
        Change::Frames_set { frames, .. } if frames.len() == 1 => Some((frames[0].2 .0, frames[0].2 .1)),
        _ => None,
    });
    match grown {
        Some((w2, h2)) => check(w2 > fw && h2 > fh, "dragging the corner did not grow the frame"),
        None => fail("dragging the corner did not emit Frames_set"),
    }
    let cross = ((sx0 + ((fx0 + fw) * z) - 10.0) as i64, (sy0 + (fy0 * z) + 10.0) as i64);
    let (_, changes) = g::scope_click(&mut ui, framed.clone(), cross);
    check(changes.iter().any(|c| matches!(c, Change::Frames_set { frames, .. } if frames.is_empty())), "the cross did not delete the frame");
    let title_at = ((sx0 + (fx0 * z) + 20.0) as i64, (sy0 + (fy0 * z) + 8.0) as i64);
    let (retitled, _) = g::double_click(&mut ui, framed.clone(), title_at);
    check(scope::editing(&retitled), "a double-click on a frame title did not open its field");
    let retitled = g::settled(&mut ui, retitled);
    let (_, changes) = g::type_text(&mut ui, retitled, "legs");
    check(
        changes.iter().any(|c| matches!(c, Change::Frames_set { frames, .. } if frames.len() == 1 && frames[0].0 == "legs" && frames[0].0 != title)),
        "typing a frame title did not emit Frames_set",
    );
    // a drag on the title carries the frame with the nodes inside it, as one Moved and one Frames_set
    let towards = (title_at.0 + 40, title_at.1 + 30);
    let (carried, _) = scope_step(&mut ui, framed.clone(), &g::frame(title_at, Vec::new(), vec![mouse_move(title_at), mouse_press(MouseButton::LeftButton, title_at)]));
    let (carried, _) = scope_step(&mut ui, carried, &g::frame(towards, Vec::new(), vec![mouse_move(towards)]));
    let (carried, _) = scope_step(&mut ui, carried, &g::frame(towards, Vec::new(), Vec::new()));
    let moved_box = scope::inspect::box_of(&carried, &heart);
    let still_box = scope::inspect::box_of(&framed, &heart);
    check(matches!((moved_box, still_box), (Some(m), Some(s0)) if m.0 > s0.0 + 10.0), "the nodes did not travel with the frame while it was dragged");
    let (_, changes) = scope_step(&mut ui, carried, &g::frame(towards, Vec::new(), vec![mouse_release(MouseButton::LeftButton, towards)]));
    let placed: Vec<&Vec<(flow_check::workspace::Path, f64, f64)>> = changes.iter().filter_map(|c| if let Change::Moved(m) = c { Some(m) } else { None }).collect();
    let fr_at: Vec<(f64, f64)> = changes
        .iter()
        .filter_map(|c| match c {
            Change::Frames_set { frames, .. } if frames.len() == 1 => Some(frames[0].1),
            _ => None,
        })
        .collect();
    if placed.len() == 1 && fr_at.len() == 1 {
        let (x, y) = fr_at[0];
        check(placed[0].iter().any(|(q, _, _)| *q == heart) && placed[0].iter().any(|(q, _, _)| *q == bloom), "the frame did not move the nodes inside it");
        check((x - (fx0 + (40.0 / z))).abs() < 2.0 && (y - (fy0 + (30.0 / z))).abs() < 2.0, "the frame did not move with the pointer");
    } else {
        fail("dragging the frame title did not emit one Moved and one Frames_set");
    }
    // f frames the selection, with nothing selected all of it
    let far = { let mut r8_view = framed.clone(); scope::with_bounds(0, 0, 400, 300, &mut r8_view); r8_view };
    let zoomed = { let mut r8_view = { let mut r8_view = far.clone(); scope::select(vec![heart.clone()], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Frame_selection, &mut r8_changes); (r8_view, r8_changes) }.0;
    match scope::inspect::box_of(&zoomed, &heart) {
        Some((x, y, w2, h2)) => check(x >= 0.0 && y >= 0.0 && x + w2 <= 400.0 && y + h2 <= 300.0 && w2 * h2 > 0.0, "framing the selection left the node out of view"),
        None => fail("the framed node has no box"),
    }
    check(
        scope::zoom(&{ let mut r8_view = { let mut r8_view = far.clone(); scope::select(Vec::new(), &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Frame_selection, &mut r8_changes); (r8_view, r8_changes) }.0) == scope::zoom(&{ let mut r8_view = far; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, Command::Frame_all, &mut r8_changes); (r8_view, r8_changes) }.0),
        "framing an empty selection is framing all",
    );
    // marquee: a drag on empty canvas selects the nodes of one scope it covers; Shift adds
    let view = g::settled(&mut ui, g::scope_view(&w, "flower").0);
    let sc = p::of_graph(&scope_catalog(), &w, "flower").expect("flower");
    let boxes: Vec<(f64, f64, f64, f64)> = sc.nodes.iter().map(|n| scope::inspect::box_of(&view, &n.path).expect("box")).collect();
    let x0 = boxes.iter().fold(f64::INFINITY, |a, b| a.min(b.0)) - 8.0;
    let y0 = boxes.iter().fold(f64::INFINITY, |a, b| a.min(b.1)) - 8.0;
    let x1 = boxes.iter().fold(f64::NEG_INFINITY, |a, b| a.max(b.0 + b.2)) + 8.0;
    let y1 = boxes.iter().fold(f64::NEG_INFINITY, |a, b| a.max(b.1 + b.3)) + 8.0;
    let a = (x0 as i64, y0 as i64);
    let b = (x1 as i64, y1 as i64);
    let (view, changes) = drag(&mut ui, view, false, a, b);
    let sel = scope::selected(&view);
    let covered: Vec<flow_check::workspace::Path> = sc.nodes.iter().map(|n| n.path.clone()).filter(|q| sel.contains(q)).collect();
    check(covered.len() >= 4 && covered.contains(&heart), "a marquee over the graph did not select its nodes");
    check(changes.iter().any(|c| matches!(c, Change::Selected(l) if l.contains(&heart))), "a marquee did not emit Selected");
    let view = g::settled(&mut ui, { let mut r8_view = g::scope_view(&w, "flower").0; scope::select(vec![heart.clone()], &mut r8_view); r8_view });
    let (rx, ry, rw, rh) = scope::inspect::box_of(&view, &path(&["flower", "result"])).expect("result");
    let ra = ((rx - 4.0) as i64, (ry - 4.0) as i64);
    let rb = ((rx + rw + 4.0) as i64, (ry + rh + 4.0) as i64);
    let (view, _) = drag(&mut ui, view, true, ra, rb);
    let sel = scope::selected(&view);
    check(sel.contains(&heart) && sel.contains(&path(&["flower", "result"])), "Shift marquee did not add to the selection");
    let fresh = g::settled(&mut ui, { let mut r8_view = g::scope_view(&w, "flower").0; scope::select(vec![heart.clone()], &mut r8_view); r8_view });
    let (view, _) = drag(&mut ui, fresh, false, ra, rb);
    check(!scope::selected(&view).contains(&heart), "a plain marquee kept the old selection");
    // A narrow crossing must select the unnamed result too, without enclosing its card.
    let workspace = checked("(workspace crossing (graph g :context value (let* [a 1.0] (+ a 2.0))))");
    let sc = Rc::new(p::of_graph(&scope_catalog(), &workspace, "g").expect("g"));
    let result = path(&["g", "@result"]);
    let upstream = path(&["g", "a"]);
    check(p::find(&sc, &result).expect("result").synthetic, "crossing fixture has no unnamed result");
    for level in [p::Level::Card, p::Level::Chip, p::Level::Point] {
        struct Every(p::Level);
        impl pxui_graph::scope_state::Saved for Every {
            fn level(&self, _: &[flow_syntax::symbol::Symbol]) -> Option<(p::Level, bool)> {
                Some((self.0, true))
            }
        }
        let opts = ScopeOpts { saved: Rc::new(Every(level)), ..ScopeOpts::default() };
        let view = { let mut r8_view = scope::create(Some(3000), Some(2000)); scope::with_scope(opts, "g", sc.clone(), &mut r8_view); r8_view };
        let view = g::settled(&mut ui, view);
        let (x, y, wd, h) = scope::inspect::box_of(&view, &result).expect("box");
        let a = ((x + (wd / 2.0) - 6.0) as i64, (y - 12.0) as i64);
        let b = ((x + (wd / 2.0) + 6.0) as i64, (y + h + 12.0) as i64);
        for (a, b) in [(a, b), (b, a)] {
            let (selected, changes) = drag(&mut ui, view.clone(), false, a, b);
            check(scope::selected(&selected) == vec![result.clone()], "a crossing marquee skipped the unnamed result");
            check(changes.iter().any(|c| matches!(c, Change::Selected(l) if l.len() == 1 && l[0] == result)), "a crossing marquee did not report the result selection");
        }
        let (selected, _) = drag(&mut ui, { let mut r8_view = view.clone(); scope::select(vec![upstream.clone()], &mut r8_view); r8_view }, true, a, b);
        let sel = scope::selected(&selected);
        check(sel.contains(&upstream) && sel.contains(&result), "Shift crossing did not keep the old selection and add the unnamed result");
    }
}

pub const CASES: &[(&str, fn())] = &[("scope_gestures_3", scope_gestures_3), ("scope_gestures_4", scope_gestures_4)];
