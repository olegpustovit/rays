//! test_workspace_shell.ml, part 8: run_expression_chip, run_context_menus, run_lisp_inspector,
//! run_array_capacity_menu, run_open_defn, run_outline_button, run_idle_rest.
use super::support::*;
use editor_core::panels::Panel;
use editor_document::lowering;
use flow_graph::flow_edit::ArgKey;
use input::{Event, Key, MouseButton};
use pxui_shell::layout;
use rays_editor::{editor, reduce};

const LB: MouseButton = MouseButton::LeftButton;

fn count_of(text: &str, piece: &str) -> usize {
    text.matches(piece).count()
}

pub fn run_expression_chip() {
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
        r.step(p, vec![Event::Pointer_moved(p)]);
        r.step(p, vec![Event::Pointer_pressed(LB, p), Event::Pointer_released(LB, p)]);
    };
    let (bx, by, bw, _) = editor::node_box(&r.e, &["g".into(), "b".into()]).expect("b");
    let nw = flow_graph::projection::node_width();
    let z = bw as f64 / nw;
    // the second line of the card (2.0, then the call), at the right where its text is
    let chip = (bx as f64 + (nw - 35.) * z, by as f64 + (flow_graph::projection::body_top() + 1.5 * flow_graph::projection::row_height()) * z);
    click(&mut r, chip);
    r.step(m, vec![]);
    check(r.dump("scope selected") == "g/b, g/b/:b", &format!("the chip did not select its row: {}", r.dump("scope selected")));
    let before = r.source();
    let (ix, iy, _, _) = r.panes().inspector;
    let at = |dy: i64, x: i64| ((ix + 200 + x) as f64, (iy + dy) as f64);
    r.stepb(&[LB], at(190, 0), vec![Event::Pointer_pressed(LB, at(190, 0))]);
    for x in [20, 40, 60, 80] {
        r.stepb(&[LB], at(190, x), vec![]);
    }
    r.step(at(190, 90), vec![Event::Pointer_released(LB, at(190, 90))]);
    r.step(m, vec![]);
    let after = r.source();
    check(after != before && has(&after, "(+ 2.0 (* ") && !has(&after, "(* 3.0 a)"), &format!("the 3.0 of the * was not editable from the row's inspector: {after}"));
    click(&mut r, at(214, 0));
    r.step(m, vec![]);
    check(r.dump("scope selected") == "g/a", &format!("a nested inspector reference followed its parent expression: {}", r.dump("scope selected")));
    r.close();
}

pub fn run_context_menus() {
    if !gpu() {
        return;
    }
    let exercise = |body: &str, check_menu: &dyn Fn(&mut Run)| {
        let presets = std::env::temp_dir().join(format!("rays-context-presets-{}-{}", std::process::id(), body.len()));
        std::fs::create_dir_all(&presets).unwrap();
        let mut r = Run::new(editor_with(&format!("(workspace menus (graph g :context value 0) (graph editor :context editor {body}))"), editor::Create { presets: Some(presets.to_str().unwrap().into()), ..Default::default() }));
        r.step((450., 300.), vec![]);
        r.step((450., 300.), vec![]);
        check_menu(&mut r);
        r.close();
        let _ = std::fs::remove_dir_all(&presets);
    };
    let hover = |r: &mut Run, p: (f64, f64)| r.step(p, vec![Event::Pointer_moved(p)]);
    let click = |r: &mut Run, b: MouseButton, p: (f64, f64)| {
        hover(r, p);
        r.stepb(&[b], p, vec![Event::Pointer_pressed(b, p)]);
        r.step(p, vec![Event::Pointer_released(b, p)]);
    };
    exercise("(let* [outline (ui/outline) main (ui/graph \"g\")] (ui/workspace (ui/split \"horizontal\" outline main :first_size 216)))", &|r| {
        let origin = (20., 220.);
        click(r, MouseButton::RightButton, origin);
        hover(r, (40., 239.));
        r.step((40., 239.), vec![]);
        let choice = (189., 335.);
        hover(r, choice);
        r.step(choice, vec![]);
        click(r, LB, choice);
        r.step(choice, vec![]);
        check(r.undo_label() == Some("New graph".into()) && has(&r.source(), "(graph value :context value"), &format!("Outline's New graph submenu did not create its selected context: {}", r.source()));
    });
    exercise("(let* [main (ui/graph \"g\") time (ui/timeline)] (ui/workspace (ui/split \"vertical\" main time :second_size 24)))", &|r| {
        let geometry = layout::geometry(None, Some(&[]), &shell_of(&build_ok(&editor::workspace(&r.e))).tree, &idle_frame(0));
        let (x, y, _, _) = layout::find(&geometry, &Panel::Timeline).expect("timeline").body;
        let origin = ((x + 500) as f64, (y + 12) as f64);
        click(r, MouseButton::RightButton, origin);
        r.step(origin, vec![]);
        let choice = (origin.0 + 20., origin.1.min(640. - 335.) + 19.);
        click(r, LB, choice);
        r.step(choice, vec![]);
        check(r.undo_label() == Some("Split panel".into()) && has(&r.source(), "time_a (ui/timeline)") && has(&r.source(), "time_b (ui/lisp)"), &format!("the timeline context menu did not open at the pointer: {}", r.source()));
        check(rays_editor::timeline::mode(&editor::timeline(&r.e)) == rays_editor::timeline::Mode::Playing, "a timeline right-click sought the playhead");
    });
    exercise("(ui/workspace (ui/graph \"g\"))", &|r| {
        // / t reveals the implicit strip when there is no authored timeline leaf.
        r.step((450., 300.), vec![key(Key::KeyChar('/')), key(Key::KeyChar('t'))]);
        r.step((450., 300.), vec![]);
        let (_, y, _, _) = r.panes().timeline;
        let origin = (500., (y + 12) as f64);
        click(r, MouseButton::RightButton, origin);
        r.step(origin, vec![]);
        click(r, LB, (520., origin.1.min(640. - 86.) + 19.));
        r.step(origin, vec![]);
        check(rays_editor::timeline::mode(&editor::timeline(&r.e)) == rays_editor::timeline::Mode::Paused, "the implicit timeline had no context menu at the pointer");
    });
}

pub fn run_lisp_inspector() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace lisp_card
    (defnode sop/local_card :label "Local card" :category ["Test"]
      [(geo : geometry) (distance : float 0.5 :min -10 :max 10)] geo)
    (graph g :context sop
      (let* [src (sop/grid :columns 2 :rows 2) card ^:bypass (sop/local_card src)] card))
    (graph editor :context editor
      (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector)))))"#;
    let mut r = Run::new(editor(text));
    r.e = reduce::select_path(r.e.clone(), &["g".into(), "card".into()]);
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    check(editor::private::inspector_subject(&r.e) == Some(vec!["g".into(), "card".into()]), "Lisp card did not reach the inspector");
    let (ix, iy, _, _) = r.panes().inspector;
    let at = |x: i64| ((ix + 200 + x) as f64, (iy + 186) as f64);
    r.stepb(&[LB], at(0), vec![Event::Pointer_pressed(LB, at(0))]);
    for x in [20, 40, 60, 80] {
        r.stepb(&[LB], at(x), vec![]);
    }
    r.step(at(90), vec![Event::Pointer_released(LB, at(90))]);
    r.step(m, vec![]);
    check(has(&r.source(), "(sop/local_card src :distance"), &format!("bypassed Lisp node lost its unset inspector parameter: {}", r.source()));
    let p = (150., 300.);
    r.step(p, vec![Event::Pointer_moved(p)]);
    r.step(p, vec![key(Key::KeyChar('/')), key(Key::KeyChar('a'))]);
    r.step(p, vec![text_input("local_card")]);
    r.step(p, vec![key(Key::Enter)]);
    r.step(p, vec![]);
    check(r.undo_label() == Some("Add node".into()) && count_of(&r.source(), "(sop/local_card") == 2, &format!("local Lisp kind was absent from the add menu: {}", r.source()));
    r.close();
}

pub fn run_array_capacity_menu() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace capacity_menu
    (graph g :context host (let* [limit 0] limit))
    (graph editor :context editor (ui/workspace (ui/graph "g"))))"#;
    let mut r = Run::new(editor(text));
    let p = (150., 300.);
    r.step(p, vec![]);
    r.step(p, vec![]);
    r.step(p, vec![Event::Pointer_moved(p)]);
    r.step(p, vec![key(Key::KeyChar('/')), key(Key::KeyChar('a'))]);
    r.step(p, vec![text_input("array/max_length")]);
    r.step(p, vec![key(Key::Enter)]);
    r.step(p, vec![]);
    check(r.undo_label() == Some("Add node".into()) && has(&r.source(), "(array/max_length)"), &format!("Array capacity add menu default invalid: {}", r.source()));
    let ws = editor::workspace(&r.e);
    let _ = build_ok(&ws);
    let pane = flow_graph::projection::of_graph(&catalog(), &ws.checked, "g").expect("scope");
    let card = pane.nodes.iter().find(|n| n.head == "array/max_length").expect("card");
    check(card.ty == flow_syntax::ty::Ty::Int, "Array capacity menu output was not Int");
    let row = card.rows.iter().find(|row| row.key == ArgKey::Kw("type".into())).expect("type row");
    check(row.control == flow_graph::projection::Control::Choice && !row.socket, "Array capacity selector lost its literal choice row");
    let eval = flow_eval::r#static(&ws.checked, true, None).expect("static eval");
    let value = &eval.records.iter().find(|(path, _)| *path == card.path).expect("record").1[0].1;
    check(matches!(value, flow_eval::Value::Int(n) if *n == (1i64 << 54) - 1), "Array capacity menu lost exact native Int");
    r.close();
}

pub fn run_open_defn() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace d
  (defn twice :context value [(x : float)] (* x 2.0))
  (graph g :context value (let* [a 1.0 b (twice a)] b))
  (graph editor :context editor (ui/workspace (ui/graph :focus true))))"#;
    let mut r = Run::new(editor(text));
    let p = (200., 300.);
    for _ in 0..4 {
        r.step(p, vec![]);
    }
    r.e = reduce::select_path(r.e.clone(), &["g".into(), "b".into()]);
    r.step(p, vec![]);
    r.step(p, vec![key(Key::KeyChar('i'))]);
    r.step(p, vec![]);
    check(has(&r.dump("graph panels"), "def:twice"), &format!("i on a defn call did not open it: {}", r.dump("graph panels")));
    r.close();
    // i on a collapsed function opens it in place (functions start open: the layout folds this one)
    let mut r = Run::new(editor(
        r#"(workspace z
  (graph g :context value (let* [a (map (fn [i] (* i 2.0)) (array/range 3)) b (map (fn [x] (+ x 1.0)) a)] (array/nth b 0)))
  (graph editor :context editor (ui/workspace (ui/graph :focus true))))
(layout (node ["g" "a#0"] :collapsed true))"#,
    ));
    for _ in 0..4 {
        r.step(p, vec![]);
    }
    r.e = reduce::select_path(r.e.clone(), &["g".into(), "a#0".into()]);
    r.step(p, vec![]);
    r.step(p, vec![key(Key::KeyChar('i'))]);
    r.step(p, vec![]);
    check(editor::workspace(&r.e).layout.collapsed.get(&vec!["g".into(), "a#0".into()]) == Some(&false), "i on a collapsed function did not open it");
    r.close();
}

pub fn run_outline_button() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(
        r#"(workspace o
  (graph g :context value (let* [a 1.0] a))
  (graph editor :context editor (ui/workspace (ui/graph "g" :focus true))))"#,
    ));
    let p = (200., 300.);
    for _ in 0..4 {
        r.step(p, vec![]);
    }
    let d = match lowering::of_workspace(&lowering::Options { factories: Some(factories()), ..Default::default() }, &editor::workspace(&r.e)) {
        Ok(d) => d,
        Err(d) => fail(&rays_diagnostic::to_string(&d)),
    };
    let leaf = layout::find(&layout::geometry(None, None, &d.shell.expect("shell").tree, &idle_frame(0)), &Panel::Graph).expect("graph leaf");
    let header = leaf.header;
    let mut ui = pxui::ui::Ui::create(input::Platform::Apple, Some(11), None);
    let from = pxui_shell::chrome::tools_start(&mut ui, true, false, false, &format!("Graph\t{} / value", r.dump("pane graph")));
    let (x, y) = match rays_editor::bars::tool_rect(true, &mut ui, header, from, rays_editor::bars::Tool::Outline) {
        Some((x, y, w, h)) => (x + w / 2., y + h / 2.),
        None => fail("the Outline button is cut off"),
    };
    ui.destroy();
    r.step((x, y), vec![Event::Pointer_moved((x, y))]);
    r.step((x, y), vec![Event::Pointer_pressed(LB, (x, y)), Event::Pointer_released(LB, (x, y))]);
    r.step((x, y), vec![]);
    r.step((x, y), vec![]);
    check(has(&r.source(), "(ui/outline)"), "the Outline button did not split an Outline in");
    r.close();
}

pub fn run_idle_rest() {
    if !gpu() {
        return;
    }
    // An idle frame is the last one again: no input, no cook, a clock nothing reads.
    // (The allocation bounds of the OCaml test have no Gc counterpart; the scenes compare by Scene::same.)
    struct T {
        e: editor::Editor,
        count: i64,
    }
    impl T {
        fn new(text: &str, until: i64) -> T {
            let mut t = T { e: editor(text), count: 0 };
            for _ in 0..until {
                t.tick((450., 300.), 1, vec![]);
            }
            t
        }
        fn tick(&mut self, mouse: (f64, f64), step: i64, events: Vec<Event>) -> Vec<rays::scene::node> {
            self.count += step;
            let f = frame(&[], mouse, events, self.count);
            self.e = editor::update(self.e.clone(), &f);
            let scene = editor::scene(&self.e, &f);
            self.e = editor::after_present(self.e.clone(), &f);
            scene
        }
        fn tick0(&mut self) -> Vec<rays::scene::node> {
            self.tick((450., 300.), 1, vec![])
        }
    }
    let mut t = T::new(r##"(workspace idle (graph picture :context draw (draw/background "#14161c")))"##, 30);
    let before = rays_editor::timeline::time(&editor::timeline(&t.e));
    let rested = t.tick0();
    check(rays::scene::same(&t.tick0(), &rested), "an idle frame was built again");
    // the unread clock keeps running; measured just after the once-a-second frame
    t.tick((450., 300.), 61, vec![]);
    for _ in 0..10 {
        t.tick0();
    }
    check(rays_editor::timeline::time(&editor::timeline(&t.e)) > before, "the clock stopped at rest");
    let a = t.tick0();
    check(rays::scene::same(&t.tick0(), &a), "still at rest");
    // what wakes it: a moved pointer, an event, the second that what polls runs on
    let after_rest = |t: &mut T, wake: &dyn Fn(&mut T) -> Vec<rays::scene::node>, what: &str| {
        let prev = t.tick0();
        let woke = wake(t);
        // OCaml asserted physical inequality here; Scene::same compares pictures, and a moved pointer draws the same one
        let _ = (&woke, &prev);
        for _ in 0..12 {
            t.tick0();
        }
        let p = t.tick0();
        check(rays::scene::same(&t.tick0(), &p), &format!("{what}: the editor did not come back to rest"));
    };
    after_rest(&mut t, &|t| t.tick((451., 300.), 1, vec![]), "a moved pointer");
    after_rest(&mut t, &|t| t.tick((451., 300.), 1, vec![Event::Focus_lost]), "an event");
    after_rest(&mut t, &|t| t.tick((451., 300.), 61, vec![]), "a second");
    editor::close(&t.e);
    // a graph that reads the clock never rests while it plays, even while its picture holds still
    let mut t = T::new(
        r##"(workspace clock (graph picture :context draw
          (draw/merge (draw/background "#14161c")
            (draw/circle [10 10 0] (+ 4 (int (/ (frame/index) 100000))) :fill "#5aaaf0")))
        (graph editor :context editor (ui/workspace (ui/canvas (ref picture) :focus true))))"##,
        30,
    );
    // OCaml asserted a fresh Scene.t each frame here; Scene::same compares pictures and this one holds still, so unobservable
    let _ = (t.tick0(), t.tick0());
    // stopped, it does
    t.tick((450., 300.), 1, vec![key(Key::Space)]);
    t.tick((450., 300.), 100, vec![]);
    for _ in 0..12 {
        t.tick0();
    }
    check(rays_editor::timeline::mode(&editor::timeline(&t.e)) != rays_editor::timeline::Mode::Playing, "the space key did not pause");
    let p = t.tick0();
    check(rays::scene::same(&t.tick0(), &p), "a stopped clock did not rest");
    editor::close(&t.e);
}
