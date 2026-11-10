//! test_workspace_shell.ml, part 5: run_root_section, run_idle_follow, run_hide_and_order,
//! run_copy_lisp, run_studio.
use super::support::*;
use editor_core::panels::{self, Panel};
use editor_document::layout_edit::{Op as L, Size as LSize};
use editor_document::workspace_doc;
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::syntax::{self as s, Draft as Node};
use input::{Event, Key, MouseButton};
use pxui_shell::layout;
use rays_editor::editor;
use std::cell::RefCell;
use std::rc::Rc;

const LB: MouseButton = MouseButton::LeftButton;

pub fn run_root_section() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(
        r#"(workspace studio
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           all (scene/merge body)]
      (scene/root all :renderer "Raster" :width 800 :height 600 :max_spp 64))))"#,
    ));
    for _ in 0..6 {
        r.step((450., 300.), vec![]);
    }
    let before = r.source();
    check(r.undo_label().is_none() && has(&r.source(), ":width 800"), "the root is in the text, nothing to undo");
    let (ix, iy, iw, ih) = editor::panes(&r.e, &idle_frame(r.count)).inspector;
    check(iw > 0 && ih > 0, "the inspector panel is showing");
    // the Renderer row of the Scene root section: a press opens its menu under the field; the second row is Wireframe.
    let row = iy + 90 + 24 + 4 + 24 + 40 + 96 + 40 + 24 + 40;
    let press = |r: &mut Run, at: (f64, f64)| {
        r.step(at, vec![Event::Pointer_moved(at)]);
        r.step(at, vec![Event::Pointer_pressed(LB, at), Event::Pointer_released(LB, at)]);
        r.step(at, vec![]);
    };
    let x = (ix + iw * 3 / 4) as f64;
    press(&mut r, (x, (row + 12) as f64));
    press(&mut r, (x, (row + 23 + 6 + 24 + 12) as f64));
    check(r.undo_label() == Some("Render settings".into()), "no control of the inspector wrote the root");
    check(r.source() != before && has(&r.source(), "(scene/root all"), "the root stayed in the text, edited");
    r.step_full(&[], &[Key::Meta], (450., 300.), vec![ch('z')]);
    check(r.source() == before, "one undo returns the root");
    r.close();
}

pub fn run_idle_follow() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(
        r#"(workspace sea
  (graph sea :context sop [(side : int 8)]
    (sop/transform (sop/grid :counts "Point counts" :connectivity "Triangles" :columns side :rows side)
      :shear_xy (* 0.1 (sin t)) :recompute_normals true))
  (graph scene :context scene
    (let* [s (scene/geometry (ref sea) :name "Sea")
           camera (scene/camera :eye [7.0 4.0 7.0] :target [0 0 0] :fov 40 :follow_viewport true)
           key (scene/light :name "Key" :translate [4 8 2] :intensity (+ 40.0 (* 10.0 (sin t))))]
      (scene/root (scene/merge s camera key) :width 960 :height 640 :renderer "Path traced" :max_spp 64)))
  (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "scene") (ui/inspector)))))"#,
    ));
    let m = (450., 300.);
    for _ in 0..6 {
        r.step(m, vec![]);
    }
    r.step_full(&[], &[Key::Space], m, vec![key(Key::Space)]);
    for _ in 0..6 {
        r.step(m, vec![]);
    }
    let doc = r.source();
    let prepared = editor::prepared(&r.e).map(|g| rdk_core::geometry::point_count(&g));
    let status = editor::private::status_text(&r.e);
    let scene = editor::private::resolved_scene(&r.e);
    let sig = |k: &editor_document::kind_graph::T| k.iter().map(|n| (n.id, n.kind.clone(), n.label.clone(), editor_document::kind_graph::field(n, "translate_x"))).collect::<Vec<_>>();
    // the graph reads only [t]: a roaming pointer over the picture is no reason to cook
    for i in 1..=20 {
        r.step((450. + i as f64, 300. - i as f64), vec![]);
    }
    check(sig(&editor::private::resolved_scene(&r.e)) == sig(&scene), "a roaming pointer re-resolved the scene");
    check(r.source() == doc, "an idle frame wrote the document");
    check(editor::prepared(&r.e).map(|g| rdk_core::geometry::point_count(&g)) == prepared, "an idle frame cooked a new displayed mesh");
    check(editor::private::status_text(&r.e) == status, &format!("the status moved while idle: {status} -> {}", editor::private::status_text(&r.e)));
    r.close();
}

pub fn run_hide_and_order() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace hide
    (graph g1 :context sop (sop/box))
    (graph scene :context scene
      (let* [a (scene/geometry (ref g1) :name "a")
             b (scene/geometry (ref g1) :translate [3 0 0] :name "b")
             all (scene/merge a b)]
        (scene/root all)))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "scene") (ui/inspector)))))"#;
    let mut r = Run::new(editor(text));
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    let click = |r: &mut Run, path: &[&str]| {
        let (bx, by, bw, bh) = editor::node_box(&r.e, &path.iter().map(|s| flow_syntax::symbol::Symbol::from(*s)).collect::<Vec<_>>()).expect("node box");
        let at = (bx as f64 + bw as f64 / 2., by as f64 + bh as f64 - 6.);
        r.step(at, vec![Event::Pointer_moved(at)]);
        r.step(at, vec![Event::Pointer_pressed(LB, at)]);
        r.step(at, vec![Event::Pointer_released(LB, at)]);
        r.step(at, vec![]);
    };
    click(&mut r, &["scene", "a"]);
    r.step(m, vec![ch('b')]);
    r.step(m, vec![]);
    check(has(&r.source(), "(scene/geometry (ref g1) :name \"a\" :visible false)") && r.undo_label() == Some("Edit value".into()), &format!("b wrote :visible false on the object: {}", r.source()));
    check(has(&r.dump("cook"), "Wrote :visible false on scene/a"), &format!("the strip says what was written: {}", r.dump("cook")));
    r.step(m, vec![ch('b')]);
    r.step(m, vec![]);
    check(has(&r.source(), ":visible true"), &format!("b again shows it: {}", r.source()));
    r.step_full(&[], &[Key::Meta], m, vec![ch('z')]);
    r.step_full(&[], &[Key::Meta], m, vec![ch('z')]);
    r.step(m, vec![]);
    check(!has(&r.source(), ":visible"), "two undos give the text back");
    // Alt-Down on a hovered merge input swaps it with the next
    let (bx, by, bw, bh) = editor::node_box(&r.e, &["scene".into(), "all".into()]).expect("all");
    click(&mut r, &["scene", "all"]);
    let mut moved = false;
    let mut y = by + 4;
    while !moved && y < by + bh {
        let at = ((bx + bw / 2) as f64, y as f64);
        r.step(at, vec![Event::Pointer_moved(at)]);
        r.step_full(&[], &[Key::Alt], at, vec![key(Key::ArrowDown)]);
        r.step(at, vec![]);
        if has(&r.source(), "(scene/merge b a)") {
            moved = true;
        }
        y += 3;
    }
    check(moved, &format!("Alt-Down moved a merge input: {}", r.source()));
    check(r.undo_label() == Some("Move item".into()), "one entry named Move item");
    r.close();
}

pub fn run_copy_lisp() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(
        r#"(workspace copied
  (graph g :context sop (sop/box))
  (graph scene :context scene (scene/merge (scene/geometry (ref g)))))"#,
    ));
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    // whether this run has a clipboard is asked first, so each case has one outcome that passes
    let available = rays::clipboard::set_text("probe").is_ok();
    r.step(m, vec![key(Key::KeyChar('/')), key(Key::KeyChar('/'))]);
    r.step(m, vec![]);
    r.step(m, vec![text_input("copy workspace")]);
    r.step(m, vec![key(Key::Enter)]);
    r.step(m, vec![]);
    let note = r.dump("cook");
    if available {
        check(has(&note, "Copied the workspace as Lisp"), &format!("the palette row did not copy: {note}"));
        match rays::clipboard::get_text() {
            Ok(clip) => check(has(&clip, "(workspace copied") && has(&clip, "(scene/geometry (ref g))"), &format!("the clipboard does not hold the workspace text: {clip}")),
            Err(message) => fail(&format!("the clipboard took the text and gives none back: {message}")),
        }
    } else {
        check(has(&note, "Clipboard: "), &format!("without a clipboard the palette row did not say so: {note}"));
    }
    check(r.undo_label().is_none(), "copying changes nothing");
    r.close();
}

pub fn run_studio() {
    if !gpu() {
        return;
    }
    let text = studio_text();
    let ws = of_text(&text);
    let doc = build_ok(&ws);
    let shell = shell_of(&doc);
    check(panels::to_string(&shell.tree) == "(h outline (h (v (v view timeline) graph) (v inspector lisp)))", &format!("the studio tree: {}", panels::to_string(&shell.tree)));
    // the target at three window sizes: every fixed side keeps its points
    for (w, h) in [(1440i64, 900i64), (1920, 1080), (1100, 700)] {
        let mut sample = idle_frame(0);
        sample.input.size = (w, h);
        let g = layout::geometry(None, Some(&[]), &shell.tree, &sample);
        let whole = |pick: &dyn Fn(&Panel) -> bool| match g.leaves.iter().find(|l| pick(&l.panel)) {
            Some(l) => l.frame,
            None => fail("the studio lost a panel"),
        };
        let expect = |what: &str, got: (i64, i64, i64, i64), want: (i64, i64, i64, i64)| check(got == want, &format!("studio at {w}x{h}: {what}"));
        let tree = h - 25;
        let middle = w - 216 - 320 - 2;
        expect("outline", whole(&|p| *p == Panel::Outline), (0, 0, 216, tree));
        expect("viewport", whole(&|p| matches!(p, Panel::View(_))), (217, 0, middle, tree - 336 - 24 - 2));
        expect("timeline", whole(&|p| *p == Panel::Timeline), (217, tree - 336 - 24 - 1, middle, 24));
        expect("graph", whole(&|p| *p == Panel::Graph), (217, tree - 336, middle, 336));
        expect("inspector", whole(&|p| *p == Panel::Inspector), (w - 320, 0, 320, tree - 300 - 1));
        expect("lisp", whole(&|p| *p == Panel::Lisp), (w - 320, tree - 300, 320, 300));
        expect("status", g.status_at, (0, tree, w, 25));
    }
    // what the panels' keywords open the editor with
    let preview = match shell.preview_sources.iter().map(|(k, _)| k.clone()).collect::<Vec<_>>().as_slice() {
        [k] => k.clone(),
        _ => fail("one viewport"),
    };
    check(
        shell.named == vec![(vec![1usize, 0, 1], "garden".to_string())] && shell.start.focus == Some(vec![1, 0, 1]) && shell.start.looking == vec![preview] && shell.start.tabs == vec![(vec![1usize, 1, 1], "graph".to_string())],
        "the start state of the studio's panels",
    );
    // print and re-read
    let again = shell_of(&build_ok(&of_text(&workspace_doc::to_text(&ws).unwrap())));
    check(again.tree == shell.tree && again.named == shell.named && again.start == shell.start && again.switch == shell.switch, "the studio did not round-trip through its printed text");
    let apply = |ws: &workspace_doc::T, op: L| match workspace_doc::edit_layout(&catalog(), ws, &op) {
        Ok(ws) => ws,
        Err(d) => fail(&format!("{}: {}", editor_document::layout_edit::label(&op), rays_diagnostic::to_string(&d))),
    };
    let text_of = |ws: &workspace_doc::T| workspace_doc::print(ws).unwrap().0;
    // a gutter drag writes what the split is sized by
    let sized = apply(&ws, L::SetLayoutSize { node: vec!["editor".into(), "studio".into()], size: LSize::First(260) });
    check(has(&text_of(&sized), "(ui/split \"horizontal\" outline work :first_size 260)"), "a fixed side is written in points");
    let ratio = apply(&ws, L::SetLayoutSize { node: vec!["editor".into(), "stage".into()], size: LSize::Ratio(0.61803) });
    check(has(&text_of(&ratio), "(ui/split-at \"vertical\" 0.618 preview timeline)"), "a ratio keeps four decimals");
    match workspace_doc::edit(&catalog(), &ws, &Op::SetArg { node: vec!["editor".into(), "studio".into()], key: ArgKey::Kw("second_size".into()), sub: vec![], value: s::make(Node::Num("100".into())) }) {
        Err(d) => check(has(&rays_diagnostic::to_string(&d), "one side"), &format!("two fixed sides: {}", rays_diagnostic::to_string(&d))),
        Ok(_) => fail("two fixed sides was accepted"),
    }
    // the keyword rows of the cards are the graph's view of it
    let scope = flow_graph::projection::of_graph(&catalog(), &ws.checked, "editor").expect("scope");
    let rows = |name: &str| match flow_graph::projection::find(&scope, &["editor".into(), name.into()]) {
        Some(n) => n.rows.iter().map(|r| r.label.clone()).collect::<Vec<_>>(),
        None => fail(&format!("no card {name}")),
    };
    let has_row = |card: &str, row: &str| rows(card).iter().any(|r| r == row);
    check(has_row("studio", "first_size") && has_row("studio", "second_size") && has_row("preview", "look_through") && has_row("network", "focus") && has_row("network", "view") && has_row("code", "tab"), "the layout keywords are not rows of their cards");
    // the layout commands take a graph whose result is a binding of the workspace
    let mut laid = ws.clone();
    for op in [
        L::LayoutNew { graph: "editor".into() },
        L::LayoutWindow { graph: "editor".into(), panel: editor_document::panel_kinds::expr(&[], "inspector").expect("inspector") },
        L::LayoutFloat { graph: "editor".into(), at: vec![0, 0] },
        L::SetLayout { graph: "editor".into(), index: 0 },
        L::LayoutRemove { graph: "editor".into() },
    ] {
        laid = apply(&laid, op);
    }
    check(has(&text_of(&laid), "shell (ui/workspace"), "the workspace binding was not kept");
    check(!has(&text_of(&laid), "(-> "), "a layout was printed as a thread");
    let _ = build_ok(&of_text(&workspace_doc::to_text(&laid).unwrap()));
    // diagnostics
    let bad = |name: &str, body: &str, needle: &str| expect_error(name, &with_editor(&format!("    {body}")), needle);
    bad("an unknown graph", "(ui/workspace (ui/graph \"nope\"))", "E_UNKNOWN_GRAPH");
    bad("a fixed side of nothing", "(ui/workspace (ui/split \"vertical\" (ui/graph) (ui/lisp) :first_size 0))", "1 point");
    bad("an unknown view", "(ui/workspace (ui/graph :view \"tree\"))", "view");
    // through the editor: the start state, the points a drag writes, two graph panels
    let mut r = Run::new(editor(&text));
    r.step((450., 100.), vec![]);
    r.step((450., 100.), vec![]);
    check(r.dump("focus") == "Graph" && r.dump("pane graph") == "garden", "the focus did not start in the graph panel");
    check(r.dump("text").starts_with("graph tab"), &format!("the lisp panel's tab: {}", r.dump("text")));
    check(editor::look_through(&r.e), "the viewport did not start looking through the render camera");
    let g0 = (216.5, 100.);
    r.step(g0, vec![Event::Pointer_moved(g0)]);
    r.stepb(&[LB], g0, vec![Event::Pointer_pressed(LB, g0)]);
    r.stepb(&[LB], (180.2, 100.), vec![Event::Pointer_moved((180.2, 100.))]);
    r.step((180.2, 100.), vec![Event::Pointer_released(LB, (180.2, 100.))]);
    r.step((180.2, 100.), vec![]);
    check(has(&r.source(), "(ui/split \"horizontal\" outline work :first_size 180)") && r.undo_label() == Some("Resize panel".into()), &format!("the gutter of a fixed split did not write points: {}", r.source()));
    // the second layout: a floating graph on the material beside the docked graph, each its own pane
    r.e = match editor::edit_layout(r.e.clone(), &L::SetLayout { graph: "editor".into(), index: 1 }) {
        Ok(e) => e,
        Err(m) => fail(&m),
    };
    r.step((450., 100.), vec![]);
    r.step((450., 100.), vec![]);
    check(r.dump("graph panels") == "garden, clay", &format!("the graph panels: {}", r.dump("graph panels")));
    check(r.dump("windows") == "graph 560 90 520 360, view 250 470 380 260, inspector 1090 60 320 420, lisp 640 500 420 300", &format!("the windows: {}", r.dump("windows")));
    r.click((500., 200.));
    check(r.dump("pane graph") == "clay", "a press in the floating graph did not make it the pane in use");
    r.click((230., 420.));
    check(r.dump("pane graph") == "garden" && r.dump("graph panels") == "garden, clay", "the docked graph did not keep its own graph");
    r.close();
    // every panel is an instance (flow.md 11.11): at 1440x900 the second layout has an inspector and a
    // text pane floating beside the docked ones; each draws, scrolls and types on its own
    let mut e = editor(&replace(&text, ":active 0", ":active 1"));
    let mut count = 0i64;
    let probes: Rc<RefCell<Vec<f64>>> = Rc::new(RefCell::new(vec![]));
    let mut step = |e: &mut editor::Editor, buttons: &[MouseButton], mouse: (f64, f64), events: Vec<Event>| {
        count += 1;
        probes.borrow_mut().clear();
        let mut f = frame(buttons, mouse, events, count);
        f.input.size = (1440, 900);
        let sink = probes.clone();
        // the sketch's own rows end every inspector: a tall filler whose place says where that inspector has scrolled to
        let (next, _) = editor::update_with(e.clone(), &f, &move |ui| {
            let filler = ui.box_("filler", pxui::ui::BoxOpts::default().h(pxui::ui::Size::Px(1500.)));
            let (_, y, _, _) = ui.rect(&filler);
            sink.borrow_mut().push(y);
            Rc::new(y)
        });
        *e = next;
    };
    let click = |step: &mut dyn FnMut(&mut editor::Editor, &[MouseButton], (f64, f64), Vec<Event>), e: &mut editor::Editor, p: (f64, f64)| {
        step(e, &[], p, vec![Event::Pointer_moved(p)]);
        step(e, &[LB], p, vec![Event::Pointer_pressed(LB, p)]);
        step(e, &[], p, vec![Event::Pointer_released(LB, p)]);
        step(e, &[], p, vec![]);
    };
    let rest = |step: &mut dyn FnMut(&mut editor::Editor, &[MouseButton], (f64, f64), Vec<Event>), e: &mut editor::Editor, p: (f64, f64)| {
        for _ in 0..3 {
            step(e, &[], p, vec![]);
        }
    };
    // the floating inspector's labels run to x 1199 (window 1090 + 109): a point left of that is no field
    let (docked, floating) = ((1300., 530.), (1110., 300.));
    rest(&mut step, &mut e, (700., 200.));
    let at_rest = match probes.borrow().as_slice() {
        [a, b] => (*a, *b),
        l => fail(&format!("{} inspectors drew their rows, not 2", l.len())),
    };
    let scroll = |step: &mut dyn FnMut(&mut editor::Editor, &[MouseButton], (f64, f64), Vec<Event>), e: &mut editor::Editor, p: (f64, f64)| {
        step(e, &[], p, vec![Event::Pointer_moved(p)]);
        for _ in 0..6 {
            step(e, &[], p, vec![Event::Scrolled((0., -8.))]);
        }
        for _ in 0..3 {
            step(e, &[], p, vec![]);
        }
        let g = probes.borrow();
        match g.as_slice() {
            [a, b] => (*a, *b),
            _ => fail("an inspector stopped drawing"),
        }
    };
    let (d1, f1) = scroll(&mut step, &mut e, floating);
    check(d1 == at_rest.0 && f1 != at_rest.1, &format!("the floating inspector did not scroll on its own: {} {}, then {d1} {f1}", at_rest.0, at_rest.1));
    let (d2, f2) = scroll(&mut step, &mut e, docked);
    check(d2 != d1 && f2 == f1, "the docked inspector did not scroll on its own");
    // a click in a floating panel focuses it
    click(&mut step, &mut e, floating);
    check(has(&dump_line(&e, "panels"), "inspector details*"), &format!("a click did not focus the floating inspector: {}", dump_line(&e, "panels")));
    // typing in the floating text pane drafts there, not in the docked one
    let pane = |e: &editor::Editor, name: &str| -> String {
        let prefix = format!("lisp {name}");
        dump_line(e, "panels").split(';').map(|l| l.trim().to_string()).find(|l| l.starts_with(&prefix)).unwrap_or_else(|| fail(&format!("no pane {name}")))
    };
    check(has(&pane(&e, "code"), "graph tab, draft no") && has(&pane(&e, "notes"), "document tab, draft no"), &format!("each text pane opens on its own tab: {}", dump_line(&e, "panels")));
    click(&mut step, &mut e, (800., 660.));
    step(&mut e, &[], (800., 660.), vec![text_input(" ")]);
    rest(&mut step, &mut e, (800., 660.));
    check(has(&pane(&e, "notes"), "lisp notes*: document tab, draft yes") && has(&pane(&e, "code"), "graph tab, draft no"), &format!("typing in one text pane reached the other: {}", dump_line(&e, "panels")));
    click(&mut step, &mut e, (1280., 720.));
    step(&mut e, &[], (1280., 720.), vec![text_input(" ")]);
    rest(&mut step, &mut e, (1280., 720.));
    check(has(&pane(&e, "code"), "lisp code*: graph tab, draft yes") && has(&pane(&e, "notes"), "document tab, draft yes"), &format!("the docked text pane does not draft on its own: {}", dump_line(&e, "panels")));
    editor::close(&e);
}
