//! , part 7: run_undo_and_input, run_carry_reaches_no_history,
//! run_atomic_frame, run_undo_under_pane, run_spreadsheet, run_op_inspector, run_nested_inspector.
use super::support::*;
use editor_document::workspace_doc;
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::syntax::{self as s, Draft as Node};
use input::{Event, Key, MouseButton};
use pxui_shell::layout;
use rays_editor::{editor, reduce};

const LB: MouseButton = MouseButton::LeftButton;
const M: MouseButton = MouseButton::MiddleButton;

fn count_of(text: &str, piece: &str) -> usize {
    text.matches(piece).count()
}
fn press(c: char) -> Event {
    key(Key::KeyChar(if c == ' ' { '/' } else { c }))
}
fn session(text: &str, at: (f64, f64)) -> Run {
    let mut r = Run::new(editor(text));
    for _ in 0..4 {
        r.step(at, vec![]);
    }
    r
}
fn set_arg(r: &mut Run, node: &[&str], k: &str, value: Node) {
    r.e = match editor::edit(r.e.clone(), &Op::SetArg { node: node.iter().map(|s| flow_syntax::symbol::Symbol::from(*s)).collect(), key: ArgKey::Kw(k.into()), sub: vec![], value: s::make(value) }) {
        Ok(e) => e,
        Err(m) => fail(&m),
    };
}

pub fn run_undo_and_input() {
    if !gpu() {
        return;
    }
    let plain = "(workspace studio (graph g :context sop (let* [a (sop/box)] a)))";
    // E12: navigation is view state. It leaves no entry, does not swallow the next undo and does not drop what redo restores.
    let at = (200., 300.);
    let mut r = session(plain, at);
    let orbit = |r: &mut Run| {
        r.step(at, vec![Event::PointerMoved(at), Event::Scrolled((0., -2.))]);
        for _ in 0..20 {
            r.step(at, vec![]);
        }
    };
    let eye = |r: &Run| rays::camera::position(&rays::easy_camera::camera(&editor::camera(&r.e)).expect("camera"));
    let start = eye(&r);
    orbit(&mut r);
    check(eye(&r) != start, "the wheel did not move the viewport");
    check(!editor::can_undo(&r.e), &format!("navigating made an undo entry: {:?}", r.undo_label()));
    set_arg(&mut r, &["g", "a"], "uniform_scale", Node::Num("2.0".into()));
    r.step(at, vec![]);
    let label = r.undo_label();
    orbit(&mut r);
    check(r.undo_label() == label, "navigating after an edit made an undo entry");
    r.step_full(&[], &[Key::Meta], at, vec![press('z')]);
    check(!has(&r.source(), "uniform_scale") && editor::can_redo(&r.e), "the first undo after navigating was swallowed");
    orbit(&mut r);
    check(editor::can_redo(&r.e), "navigating dropped what redo restores");
    r.close();
    // E13: a stroke down the visibility column is one undo entry
    let lights = r#"(workspace lights (graph scene :context scene
    (scene/merge (scene/light :name "a") (scene/light :name "b") (scene/light :name "c"))))"#;
    let mut r = session(lights, at);
    let (gx, gy, gw, _) = r.panes().graph;
    let flag = |row: i64| ((gx + gw - 38) as f64, (gy + 24 + 24 * row + 12) as f64);
    r.count += 30;
    r.step(flag(0), vec![Event::PointerMoved(flag(0))]);
    r.stepb(&[LB], flag(0), vec![Event::PointerPressed(LB, flag(0))]);
    r.stepb(&[LB], flag(1), vec![Event::PointerMoved(flag(1))]);
    r.stepb(&[LB], flag(2), vec![Event::PointerMoved(flag(2))]);
    r.step(flag(2), vec![Event::PointerReleased(LB, flag(2))]);
    r.step(at, vec![]);
    check(count_of(&r.source(), ":visible false") == 3, &format!("the stroke did not hide three rows: {}", r.source()));
    r.step_full(&[], &[Key::Meta], at, vec![press('z')]);
    check(count_of(&r.source(), ":visible false") == 0, &format!("one undo did not restore the stroke: {}", r.source()));
    r.close();
    // E14: a gutter drag cut short by a focus loss leaves no size for a later click to commit
    let m = (450., 300.);
    let mut r = session(&case("variations"), m);
    let (gutter, away) = ((169.5, 300.), (300., 300.));
    r.step(gutter, vec![Event::PointerMoved(gutter)]);
    r.stepb(&[LB], gutter, vec![Event::PointerPressed(LB, gutter)]);
    r.stepb(&[LB], away, vec![Event::PointerMoved(away)]);
    r.step(away, vec![Event::FocusLost]);
    r.step(away, vec![]);
    let before = r.source();
    r.step(gutter, vec![Event::PointerMoved(gutter)]);
    r.stepb(&[LB], gutter, vec![Event::PointerPressed(LB, gutter)]);
    r.step(gutter, vec![Event::PointerReleased(LB, gutter)]);
    r.step(m, vec![]);
    check(r.source() == before && r.undo_label().is_none(), &format!("a click on a gutter committed the size of an interrupted drag: {:?}", r.undo_label()));
    r.close();
    // E19: the focus of a closed panel goes to its neighbour, so the next panel key acts
    let inspector = (800., 300.);
    let mut r = session(plain, inspector);
    r.step(inspector, vec![Event::PointerMoved(inspector)]);
    r.step(inspector, vec![Event::PointerPressed(LB, inspector), Event::PointerReleased(LB, inspector)]);
    let keys = |r: &mut Run, at: (f64, f64), cs: &[char]| {
        for c in cs {
            r.step(at, vec![press(*c)]);
            r.step(at, vec![]);
        }
        r.step(at, vec![]);
    };
    keys(&mut r, inspector, &[' ', 'o', 'x']);
    check(r.undo_label() == Some("Close panel".into()), &format!("the inspector was not closed: {:?}", r.undo_label()));
    keys(&mut r, inspector, &[' ', 'o', 'v']);
    check(r.undo_label() == Some("Split panel".into()), "panel keys were dead after the focused panel closed");
    r.close();
    // E21: fly mode ends when the view stops navigating (a fixed camera, looked through)
    let mut r = session(plain, at);
    r.step(at, vec![Event::PointerMoved(at)]);
    r.step(at, vec![Event::PointerPressed(LB, at), Event::PointerReleased(LB, at)]);
    for c in [' ', 'v'] {
        r.step(at, vec![press(c)]);
        r.step(at, vec![]);
    }
    for c in [' ', 'w'] {
        r.step(at, vec![press(c)]);
        r.step(at, vec![]);
    }
    check(editor::flying(&r.e) && editor::look_through(&r.e), "the view is not flying through its camera");
    set_arg(&mut r, &["scene", "camera1"], "follow_viewport", Node::Sym("false".into()));
    r.step(at, vec![]);
    r.step(at, vec![]);
    check(!editor::flying(&r.e), "fly mode outlived the navigation it belongs to");
    r.close();
    // E20: a name field left open in the graph pane does not hold the keyboard once the panel shows its list
    let mut r = session(plain, m);
    let (gx, gy, gw, _) = r.panes().graph;
    let row = ((gx + 60) as f64, (gy + 24 + 12) as f64);
    let click = |r: &mut Run, p: (f64, f64)| {
        r.step(p, vec![Event::PointerMoved(p)]);
        r.step(p, vec![Event::PointerPressed(LB, p), Event::PointerReleased(LB, p)]);
        r.step(p, vec![]);
    };
    click(&mut r, row);
    r.step(m, vec![press('i')]);
    r.step(m, vec![]);
    r.count += 30;
    let (x, y, w, h) = editor::node_box(&r.e, &["g".into(), "a".into()]).expect("a box");
    click(&mut r, ((x + w / 2) as f64, (y + 10.min(h / 2)) as f64));
    r.step(m, vec![key(Key::F2)]);
    r.step(m, vec![]);
    r.step(m, vec![press('u')]);
    r.step(m, vec![]);
    check(editor::level(&r.e).is_some(), "F2 opened no name field: u left the object");
    // the header's List tab: found by trying, the tabs end 36 points from the header's right edge
    let mut listed = false;
    for dx in [40i64, 46, 52, 58, 64, 70] {
        if r.dump("projection") == "list" {
            listed = true;
            break;
        }
        r.count += 30;
        click(&mut r, ((gx + gw - 36 - dx) as f64, (gy - 12) as f64));
        if r.dump("projection") == "list" {
            listed = true;
            break;
        }
    }
    check(listed, &format!("the header's List tab was not found: {}", r.dump("projection")));
    for c in [' ', 'l', 'g'] {
        r.step(m, vec![press(c)]);
        r.step(m, vec![]);
    }
    r.step(m, vec![]);
    check(r.dump("projection") == "graph", "a name field open in the graph pane kept the keyboard after the panel turned to its list");
    r.close();
    // E20: a pan in progress gives the pointer back when the panel turns to its list
    let mut r = session(plain, m);
    let (gx, gy, gw, gh) = r.panes().graph;
    let row = ((gx + 60) as f64, (gy + 24 + 12) as f64);
    r.step(row, vec![Event::PointerMoved(row)]);
    r.step(row, vec![Event::PointerPressed(LB, row), Event::PointerReleased(LB, row)]);
    r.step(row, vec![press('i')]);
    r.step(m, vec![]);
    r.count += 30;
    let canvas = ((gx + gw - 40) as f64, (gy + gh - 40) as f64);
    let away = ((gx + gw - 80) as f64, (gy + gh - 60) as f64);
    r.step(canvas, vec![Event::PointerMoved(canvas)]);
    r.stepb(&[M], canvas, vec![Event::PointerPressed(M, canvas)]);
    r.stepb(&[M], away, vec![Event::PointerMoved(away)]);
    check(r.dump("graph pan") == "true", "a middle drag on the canvas is not a pan");
    for c in [' ', 'l', 'l'] {
        r.stepb(&[M], away, vec![press(c)]);
        r.stepb(&[M], away, vec![]);
    }
    check(r.dump("projection") == "list" && r.dump("graph pan") == "false", &format!("a pan in progress kept the pointer after the panel turned to its list: {}", r.dump("projection")));
    r.close();
}

pub fn run_carry_reaches_no_history() {
    if !gpu() {
        return;
    }
    let e = editor(
        r#"(workspace cams
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [cam (scene/camera :name "cam")
           side (scene/camera :name "side" :eye [6 2 0])
           all (scene/merge (scene/geometry (ref g)) cam side)]
      (scene/root all :camera cam))))"#,
    );
    let m = (450., 300.);
    let e = editor::update(editor::update(e, &frame(&[], m, vec![], 1)), &frame(&[], m, vec![], 2));
    let side = editor::scene_document(&e).iter().find(|i| i.label == "side").expect("side camera").id;
    let preview = Op::SetArg { node: vec!["scene".into(), "side".into()], key: ArgKey::Kw("fov".into()), sub: vec![], value: s::make(Node::Num("0.6981317007977318".into())) };
    let before = source(&e);
    let enter = rays_editor::editor_commands::Action::Enter;
    let stepped = reduce::step(e.clone(), Some(&[side]), Some(&preview), &[enter.clone()], &frame(&[], m, vec![], 3)).expect("step");
    check(editor::carrying(&stepped).is_some(), "the reducer dropped the carry");
    check(!editor::can_undo(&stepped), &format!("entering a camera during a carry reached the history: {:?}", editor::undo_label(&stepped)));
    // without the carry the same keys do make the camera the render camera: the test reaches the path
    let entered = reduce::step(e.clone(), Some(&[side]), None, &[enter], &frame(&[], m, vec![], 3)).expect("step");
    check(editor::can_undo(&entered) && has(&source(&entered), ":camera side") && source(&e) == before, &format!("Enter on a camera did not make it the render camera: {}", source(&entered)));
    editor::close(&e);
}

pub fn run_atomic_frame() {
    if !gpu() {
        return;
    }
    let at = (200., 300.);
    let mut r = Run::new(editor("(workspace solo (graph g :context sop (sop/box)) (graph editor :context editor (ui/workspace (ui/graph))))"));
    for _ in 0..4 {
        r.step(at, vec![]);
    }
    r.step(at, vec![Event::PointerMoved(at)]);
    r.step(at, vec![Event::PointerPressed(LB, at), Event::PointerReleased(LB, at)]);
    let before = r.source();
    let label = r.undo_label();
    for c in [' ', 'o', 'x'] {
        r.step(at, vec![press(c)]);
        r.step(at, vec![]);
    }
    r.step(at, vec![]);
    check(r.dump("edit error") != "-", &format!("closing the only panel was not refused: {}", r.source()));
    check(r.source() == before && r.undo_label() == label, &format!("a refused gesture left the frame's earlier gesture applied: {}", r.source()));
    r.close();
}

pub fn run_undo_under_pane() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace two
  (graph g :context sop (sop/box))
  (graph h :context sop (sop/box :size [2 2 2]))
  (graph scene :context scene (let* [a (scene/geometry (ref g))] (scene/merge a)))
  (graph editor :context editor
    (ui/workspace (ui/split "horizontal" (ui/graph :focus true) (ui/graph)))))"#;
    let mut r = Run::new(editor(text));
    let click = |r: &mut Run, p: (f64, f64)| {
        r.step(p, vec![Event::PointerMoved(p)]);
        r.step(p, vec![Event::PointerPressed(LB, p), Event::PointerReleased(LB, p)]);
        r.step(p, vec![]);
    };
    for _ in 0..4 {
        r.step((200., 500.), vec![]);
    }
    let expr = match flow_syntax::syntax::parse("(scene/geometry (ref h))") {
        Ok(v) if v.len() == 1 => v[0].clone(),
        _ => fail("bad form"),
    };
    r.e = match editor::edit(r.e.clone(), &Op::AddNode { scope: vec!["scene".into()], name: "b".into(), expr }) {
        Ok(e) => e,
        Err(m) => fail(&m),
    };
    r.step((200., 500.), vec![]);
    // the second panel lists the scene: enter the object made from h, its second row
    r.count += 30;
    let row = (700., (24 + 24 + 12 + 24) as f64);
    click(&mut r, row);
    r.step(row, vec![press('i')]);
    r.step(row, vec![]);
    check(has(&r.dump("graph panels"), "h"), &format!("the second graph panel did not enter the object: {}", r.dump("graph panels")));
    // the first panel takes the focus and undoes the object
    r.count += 30;
    click(&mut r, (200., 500.));
    r.step_full(&[], &[Key::Meta], (200., 500.), vec![press('z')]);
    r.step((200., 500.), vec![]);
    r.step((200., 500.), vec![]);
    check(!has(&r.source(), "(ref h)") && !has(&r.dump("graph panels"), "h"), &format!("undo left the other panel inside a removed object: {}", r.dump("graph panels")));
    r.close();
}

pub fn run_spreadsheet() {
    if !gpu() {
        return;
    }
    use rays_editor::spreadsheet as sheet;
    let text = r#"(workspace sheet
    (graph g :context sop (sop/grid :counts "Point counts" :rows 2 :columns 3))
    (graph h :context sop (sop/box))
    (graph editor :context editor (let* [a (ui/graph "g" :focus true) b (ui/graph "h")
      sa (ui/spreadsheet :of a) sb (ui/spreadsheet :of b)]
      (ui/workspace (ui/split "vertical" (ui/split "horizontal" a b) (ui/split "horizontal" sa sb))))))"#;
    let doc = of_text(text);
    check(workspace_doc::to_text(&of_text(&workspace_doc::to_text(&doc).unwrap())).unwrap() == workspace_doc::to_text(&doc).unwrap(), "spreadsheet layout did not round trip");
    let shell = shell_of(&build_ok(&doc));
    check(shell.follows == vec![(vec![1usize, 1], vec![0usize, 1]), (vec![1, 0], vec![0, 0])], "spreadsheet :of did not follow its graph panel");
    let mut r = Run::new(editor(text));
    r.e = reduce::select_path(r.e.clone(), &["g".into(), "@result".into()]);
    r.step((0., 0.), vec![]);
    r.step((0., 0.), vec![]);
    let (owner, geometry) = reduce::spreadsheet(&r.e, &[1, 0]);
    let geometry = geometry.unwrap_or_else(|| fail("selected cooked node absent from spreadsheet"));
    let counts: Vec<i64> = (0..4).map(|o| sheet::count(o, &geometry)).collect();
    check(owner == 0 && counts == [6, 12, 4, 1], &format!("spreadsheet owner row counts: {counts:?}; owner={owner}"));
    check(reduce::spreadsheet(&r.e, &[1, 1]).1.is_none(), "spreadsheet followed the other graph selection");
    let layouts = layout::geometry(None, None, &shell.tree, &idle_frame(r.count));
    let leaf = layouts.leaves.iter().find(|l| l.path == vec![1usize, 0]).expect("leaf");
    let (x, y, w, _) = leaf.body;
    let mut ui = pxui::ui::Ui::create(input::Platform::Apple, None, None);
    let detail_width = ui.text_width(None, "Detail");
    ui.destroy();
    let point = ((x + w) as f64 - 8. - detail_width / 2., y as f64 + 12.);
    r.step(point, vec![]);
    r.step(point, vec![Event::PointerPressed(LB, point), Event::PointerReleased(LB, point)]);
    check(reduce::spreadsheet(&r.e, &[1, 0]).0 == 3, "spreadsheet Detail tab did not route shared input");
    check(reduce::spreadsheet(&r.e, &[1, 1]).0 == 0, "spreadsheet owner state leaked to another pane");
    r.e = editor::edit(r.e.clone(), &Op::SetArg { node: vec!["g".into(), "@result".into()], key: ArgKey::Kw("columns".into()), sub: vec![], value: s::make(Node::Num("4".into())) }).expect("edit");
    r.step((0., 0.), vec![]);
    r.step((0., 0.), vec![]);
    let geometry = reduce::spreadsheet(&r.e, &[1, 0]).1.expect("geometry");
    check(rdk_core::geometry::point_count(&geometry) == 8 && reduce::spreadsheet(&r.e, &[1, 0]).0 == 3, "spreadsheet recook kept stale geometry or reset owner");
    let columns = sheet::columns(0, &geometry);
    check(columns.len() >= 4 && (0..4).map(|i| columns[i].0.clone()).collect::<Vec<_>>() == ["index", "P.x", "P.y", "P.z"], "canonical position tuple columns");
    let attr = rdk_core::attribute::create_owned("label", rdk_core::attribute::Owner::Point, rdk_core::attribute::Storage::Text(std::sync::Arc::new(vec![b"sample".to_vec(); 8].into()))).expect("attribute");
    let geometry = rdk_core::geometry::with_attribute(attr, &geometry).expect("with_attribute");
    let group = rdk_core::group::init(None, rdk_core::group::Owner::Point, "odd", 8, &|i| Ok(i % 2 == 1)).expect("group");
    let geometry = rdk_core::geometry::with_group(group, &geometry).expect("with_group");
    let columns = sheet::columns(0, &geometry);
    let col = |name: &str| &columns.iter().find(|(n, _)| n == name).unwrap_or_else(|| fail(&format!("no column {name}"))).1;
    check(col("label")(7) == "sample" && col("group:odd")(0) == "0" && col("group:odd")(7) == "1", "text values/group membership columns");
    r.close();
}

pub fn run_op_inspector() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace ops
    (graph g :context value (let* [a 0.6 b (* 3.0 a)] (+ a b)))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector)))))"#;
    let mut r = Run::new(editor(text));
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    let click = |r: &mut Run, p: (f64, f64)| {
        r.step(p, vec![Event::PointerMoved(p)]);
        r.step(p, vec![Event::PointerPressed(LB, p), Event::PointerReleased(LB, p)]);
    };
    let (bx, by, _, _) = editor::node_box(&r.e, &["g".into(), "b".into()]).expect("b");
    click(&mut r, ((bx + 60) as f64, (by + 12) as f64));
    r.step(m, vec![]);
    check(r.dump("scope selected") == "g/b", &format!("op node not selected: {}", r.dump("scope selected")));
    let before = r.source();
    // the first row (a, 3.0) is a slider 186 points under the panel body's top
    let (ix, iy, _, _) = r.panes().inspector;
    let at = |x: i64| ((ix + 200 + x) as f64, (iy + 186) as f64);
    r.stepb(&[LB], at(0), vec![Event::PointerPressed(LB, at(0))]);
    for x in [20, 40, 60, 80] {
        r.stepb(&[LB], at(x), vec![]);
    }
    r.step(at(90), vec![Event::PointerReleased(LB, at(90))]);
    r.step(m, vec![]);
    check(r.source() != before && !has(&r.source(), "(* 3.0 a)") && has(&r.source(), "(* "), &format!("the inspector showed no editable input row of a * node: {}", r.source()));
    let label = r.undo_label();
    let authored = r.source();
    click(&mut r, ((ix + 210) as f64, (iy + 210) as f64));
    r.step(m, vec![]);
    check(r.dump("scope selected") == "g/a" && editor::private::inspector_subject(&r.e) == Some(vec!["g".into(), "a".into()]), &format!("a wired inspector value did not select its source: {}", r.dump("scope selected")));
    check(r.source() == authored && r.undo_label() == label, "following a wire edited the document");
    r.close();
}

pub fn run_nested_inspector() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace ops
    (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a))] (+ a b)))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector)))))"#;
    let mut r = Run::new(editor(text));
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    let click = |r: &mut Run, p: (f64, f64)| {
        r.step(p, vec![Event::PointerMoved(p)]);
        r.step(p, vec![Event::PointerPressed(LB, p), Event::PointerReleased(LB, p)]);
    };
    let (bx, by, _, _) = editor::node_box(&r.e, &["g".into(), "b".into()]).expect("b");
    click(&mut r, ((bx + 60) as f64, (by + 12) as f64));
    r.step(m, vec![]);
    check(r.dump("scope selected") == "g/b", &format!("op node not selected: {}", r.dump("scope selected")));
    let before = r.source();
    let (ix, iy, _, _) = r.panes().inspector;
    let at = |dy: i64, x: i64| ((ix + 200 + x) as f64, (iy + dy) as f64);
    r.stepb(&[LB], at(281, 0), vec![Event::PointerPressed(LB, at(281, 0))]);
    for x in [20, 40, 60, 80] {
        r.stepb(&[LB], at(281, x), vec![]);
    }
    r.step(at(281, 90), vec![Event::PointerReleased(LB, at(281, 90))]);
    r.step(m, vec![]);
    let after = r.source();
    check(after != before && has(&after, "(+ 2.0 (* ") && has(&after, "a 1.0") && has(&after, " a))") && !has(&after, "(* 3.0 a)"), &format!("the nested * input was not editable from the inspector: {after}"));
    let b_line = after.lines().find(|l| has(l, "b (+")).expect("b line").to_string();
    check(has(&b_line, "(+ 2.0 (* ") && !has(&b_line, "(* 3.0 a)") && has(&b_line, " a))") && r.undo_label() == Some("Edit value".into()), &format!("only the nested 3.0 may change: {b_line}"));
    click(&mut r, at(154, 0));
    r.step(m, vec![]);
    let unfolded = r.source();
    check(has(&unfolded, "product (* ") && has(&unfolded, "b (+ 2.0 product)"), &format!("Unfold did not make the nested call its own binding: {unfolded}"));
    r.close();
}
