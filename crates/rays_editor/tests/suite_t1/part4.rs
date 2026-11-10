//! test_workspace_shell.ml, part 4: run_loop_copies, run_loop_expression, run_panel_states,
//! run_ref_picker, run_compose, run_layouts.
use super::support::*;
use editor_core::panels::{self, Panel};
use editor_document::document::Origin;
use editor_document::kind_graph as kg;
use editor_document::layout_edit::Op as L;
use editor_document::workspace_doc;
use input::{Event, Key, MouseButton};
use param::Value;
use pxui_shell::layout;
use rays_editor::editor;

const LB: MouseButton = MouseButton::LeftButton;

fn count_of(text: &str, piece: &str) -> usize {
    text.matches(piece).count()
}
fn objects(e: &editor::Editor) -> kg::T {
    editor::scene_document(e)
}
fn field_of(i: &kg::Node, name: &str) -> Option<Value> {
    kg::field(i, name)
}
fn f(v: f64) -> Option<Value> {
    Some(Value::Float(v))
}

/// The list's rows, walked with the arrows: the row under the focus is the selection.
fn select_row(r: &mut Run, k: usize) {
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    let (gx, gy, _, _) = r.panes().graph;
    let p = ((gx + 40) as f64, (gy + 400) as f64);
    r.step(p, vec![Event::Pointer_pressed(LB, p), Event::Pointer_released(LB, p)]);
    r.step(m, vec![key(Key::Home)]);
    for _ in 0..k {
        r.step(m, vec![key(Key::ArrowDown)]);
    }
    r.step(m, vec![]);
}

/// A drag across the inspector's slider at `row` points under the panel's top.
fn drag_slider(r: &mut Run, row: i64) {
    let (ix, iy, _, _) = r.panes().inspector;
    // the first field of a vector row starts at the control column: 12 + 6 + 8 + 56 + 8 = 90 in a
    // 180-wide panel; a point 10 inside it
    let at = |x: i64| ((ix + 100 + x) as f64, (iy + row) as f64);
    r.stepb(&[LB], at(0), vec![Event::Pointer_pressed(LB, at(0))]);
    for x in [4, 8, 12, 16] {
        r.stepb(&[LB], at(x), vec![]);
    }
    r.step(at(20), vec![Event::Pointer_released(LB, at(20))]);
    r.step((450., 300.), vec![]);
}

fn undo(r: &mut Run) {
    r.count += 1;
    let f = frame_full(&[], &[Key::Meta], (0., 0.), (450., 300.), vec![ch('z')], r.count);
    r.e = editor::update(r.e.clone(), &f);
}

fn lights(e: &editor::Editor) -> Vec<kg::Node> {
    objects(e).into_iter().filter(|i| i.kind == "light").collect()
}

pub fn run_loop_copies() {
    if !gpu() {
        return;
    }
    // copies that differ only by the graph they place: every field of the template is a literal
    let text = r#"(workspace copies
    (graph g :context sop [(seed : int 1)] (sop/box :size [seed 1 1]))
    (graph scene :context scene
      (let* [row (for [i (range 3)] (scene/geometry (ref g :seed (+ i 1))))]
        (scene/merge row))))"#;
    let mut r = Run::new(editor(text));
    select_row(&mut r, 1);
    let x = |r: &Run| objects(&r.e).iter().map(|i| field_of(i, "translate_x")).collect::<Vec<_>>();
    check(objects(&r.e).len() == 3 && x(&r).iter().all(|v| *v == f(0.)), "the loop did not make three objects at the origin");
    // the head is 118 tall at the test's 11 points, then Inputs (4 + 24, 24), Geometry (16 + 24, three switch
    // rows since 55482c94 added Cull) and Transform (16 + 24): the translate row is 118 + 52 + 112 + 40 = 322
    // down, its middle 12 more
    drag_slider(&mut r, 334);
    let moved = x(&r);
    check(moved.iter().all(|v| *v == moved[0] && *v != f(0.)), "dragging a literal field of one copy did not move all three");
    check(count_of(&r.source(), ":translate [") == 1, &format!("the template was not edited once: {}", r.source()));
    check(r.undo_label() == Some("Set translate_x".into()), "the drag is not one history entry named for the field");
    check(has(&r.dump("cook"), "loop template") && has(&r.dump("cook"), "3 copies"), &format!("the status does not say the template changed: {}", r.dump("cook")));
    let saved = r.source();
    r.close();
    let mut r = Run::new(editor(&saved));
    check(x(&r) == moved, "the saved text does not reopen with the same copies");
    // one undo gives them all back
    undo(&mut r);
    r.close();
    // a rename from the list is the template's :name: every copy takes it
    let mut r = Run::new(editor(text));
    select_row(&mut r, 2);
    let m = (450., 300.);
    r.step(m, vec![key(Key::F2)]);
    r.step(m, vec![text_input("spot")]);
    r.step(m, vec![key(Key::Enter)]);
    r.step(m, vec![]);
    let labels: Vec<String> = objects(&r.e).iter().map(|i| i.label.clone()).collect();
    check(labels.iter().all(|l| l == "gspot") && count_of(&r.source(), ":name \"gspot\"") == 1 && r.undo_label() == Some("Rename".into()), &format!("a rename of one copy did not rename the template: {}{}", r.source(), labels.join(",")));
    r.close();
    // a copy computed from its index has no value of its own
    let text = r#"(workspace lamps
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [row (for [i (range 3)] (scene/light :name "lamp" :translate [i 2 0]))]
        (scene/merge (scene/geometry (ref g)) row))))"#;
    let mut r = Run::new(editor(text));
    select_row(&mut r, 2);
    let before = r.source();
    // the head 118, Inputs (4 + 24, 24), Light (16 + 24, five rows) and Transform (16 + 24), then 12 into
    // the translate row: 118 + 52 + 160 + 40 + 12 = 382
    drag_slider(&mut r, 382);
    check(r.source() == before && r.undo_label().is_none(), "a computed field of a copy was written");
    check(has(&r.dump("edit error"), "computed by the loop"), &format!("no reason for the refused edit: {}", r.dump("edit error")));
    // deleting one copy skips its iteration: the others stay where they are
    select_row(&mut r, 2);
    r.step(m, vec![key(Key::Delete)]);
    check(lights(&r.e).len() == 2 && has(&r.source(), ":skip [1]") && r.undo_label() == Some("Delete".into()), &format!("deleting a copy did not skip it in the loop: {}", r.source()));
    check(lights(&r.e).iter().map(|i| field_of(i, "translate_x")).collect::<Vec<_>>() == vec![f(0.), f(2.)], "the copies after a deleted one moved");
    r.close();
    // a loop with two clauses: the same, one entry, and one undo gives the copy back
    let text = r#"(workspace grid
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [row (for [i (range 2) j (range 3)] (scene/light :translate [i j 0]))]
        (scene/merge (scene/geometry (ref g)) row))))"#;
    let mut r = Run::new(editor(text));
    select_row(&mut r, 2);
    let before = r.source();
    r.step(m, vec![key(Key::Delete)]);
    check(has(&r.source(), ":skip [1]") && objects(&r.e).len() == 6 && r.undo_label() == Some("Delete".into()) && r.dump("prompt") == "-", &format!("a copy of a two-clause loop was not deleted exactly: {}", r.source()));
    let saved = r.source();
    undo(&mut r);
    check(r.source() == before && objects(&r.e).len() == 7, "one undo did not give the deleted copy back");
    r.close();
    let r = Run::new(editor(&saved));
    check(objects(&r.e).len() == 6 && r.source() == saved, "the saved text does not reopen without the copy");
    r.close();
}

pub fn run_loop_expression() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace lamps
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [row (for [i (range 3)] (scene/light :name "lamp" :translate [i 2 0]))]
        (scene/merge (scene/geometry (ref g)) row))))"#;
    let mut r = Run::new(editor(text));
    select_row(&mut r, 2);
    let (ix, iy, _, _) = r.panes().inspector;
    // Option-click on the translate X field of the lamp
    let label = ((ix + 100) as f64, (iy + 118 + 28 + 24 + 40 + 120 + 40 + 12) as f64);
    r.step(label, vec![Event::Pointer_moved(label)]);
    r.step_full(&[LB], &[Key::Alt], label, vec![Event::Pointer_pressed(LB, label)]);
    r.step_full(&[], &[Key::Alt], label, vec![Event::Pointer_released(LB, label)]);
    r.step(label, vec![key(Key::KeyChar('a'))]);
    r.step(label, vec![text_input("=(* i 3)")]);
    r.step(label, vec![key(Key::Enter)]);
    r.step(label, vec![]);
    check(has(&r.source(), ":translate [(* i 3) 2 0]"), &format!("the expression did not reach the template: {}", r.source()));
    let xs = |r: &Run| lights(&r.e).iter().map(|i| field_of(i, "translate_x")).collect::<Vec<_>>();
    check(xs(&r) == vec![f(0.), f(3.), f(6.)], "the copies did not follow the expression");
    check(r.undo_label() == Some("Edit expression".into()) && r.dump("edit error") == "-" && has(&r.dump("cook"), "loop template"), "the expression edit is not one entry with its status");
    let saved = r.source();
    r.close();
    let r = Run::new(editor(&saved));
    check(xs(&r) == vec![f(0.), f(3.), f(6.)], "the saved text does not reopen with the same copies");
    r.close();
}

pub fn run_panel_states() {
    if !gpu() {
        return;
    }
    let directory = std::env::temp_dir().join(format!("rays-panel-state-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let mut r = Run::new(editor_with("(workspace panels (graph g :context sop (sop/box)))", editor::Create { presets: Some(directory.to_str().unwrap().into()), ..Default::default() }));
    let m = (450., 300.);
    let click = |r: &mut Run, p: (f64, f64)| {
        r.step(p, vec![Event::Pointer_moved(p)]);
        r.step(p, vec![Event::Pointer_pressed(LB, p), Event::Pointer_released(LB, p)]);
        r.step(p, vec![]);
    };
    r.step(m, vec![]);
    r.step(m, vec![]);
    let geometry = layout::geometry(None, Some(&[Panel::Timeline]), &layout::default(), &idle_frame(0));
    let splitter = geometry.splitters.iter().find(|s| s.node == Some(vec![])).expect("root splitter").clone();
    let (sx, sy, sw, sh) = splitter.bounds;
    let grip = ((sx + sw / 2) as f64, (sy + sh / 2) as f64);
    let moved = (grip.0 + 30., grip.1);
    let ws_text = |r: &Run| workspace_doc::to_text(&editor::workspace(&r.e)).unwrap();
    let before_split = ws_text(&r);
    r.step(grip, vec![Event::Pointer_moved(grip)]);
    r.stepb(&[LB], grip, vec![Event::Pointer_pressed(LB, grip)]);
    r.stepb(&[LB], moved, vec![Event::Pointer_moved(moved)]);
    check(ws_text(&r) == before_split && r.undo_label().is_none(), "a held splitter drag committed authored state before release");
    r.step(moved, vec![Event::Pointer_released(LB, moved)]);
    r.step(moved, vec![]);
    check(editor::workspace(&r.e).layout.editor.is_none() && has(&r.source(), "ui/split-at") && r.undo_label() == Some("Resize panel".into()), "resizing the default shell did not write its editor graph");
    let saved_shell = build_ok(&editor::workspace(&r.e)).shell.map(|s| panels::to_string(&s.tree));
    check(build_ok(&of_text(&ws_text(&r))).shell.map(|s| panels::to_string(&s.tree)) == saved_shell, "the first splitter edit did not round-trip through Lisp");
    r.step_full(&[], &[Key::Meta], moved, vec![ch('z')]);
    check(build_ok(&editor::workspace(&r.e)).shell.is_none(), "undoing the first splitter edit did not restore the default shell");
    click(&mut r, (889., 11.));
    let state = |r: &Run, key: &str| editor::workspace(&r.e).layout.panels.get(&vec!["editor".into(), key.into()]).copied().unwrap_or_else(panels::default_state);
    check(state(&r, "inspector").collapsed && r.undo_label() == Some("Collapse panel".into()) && r.panes().inspector.2 == 0, "collapsing a panel did not persist or remove its body");
    let saved = ws_text(&r);
    check(of_text(&saved).layout.panels == editor::workspace(&r.e).layout.panels, "panel state did not round-trip through Lisp");
    click(&mut r, (889., 11.));
    check(!state(&r, "inspector").collapsed, "the collapsed header could not expand the panel");
    let (gx, gy, _, _) = r.panes().graph;
    let start = ((gx + 8) as f64, (gy - 11) as f64);
    let moved = (start.0 + 30., start.1 + 70.);
    r.step(start, vec![Event::Pointer_moved(start)]);
    r.stepb(&[LB], start, vec![Event::Pointer_pressed(LB, start)]);
    r.stepb(&[LB], moved, vec![Event::Pointer_moved(moved)]);
    r.stepb(&[LB], (moved.0 + 10., moved.1), vec![Event::Pointer_moved((moved.0 + 10., moved.1))]);
    r.step(moved, vec![Event::Pointer_released(LB, moved)]);
    r.step(moved, vec![]);
    check(state(&r, "network").window.is_some() && r.undo_label() == Some("Arrange panel".into()), "the panel drag handle did not undock the panel");
    let window = state(&r, "network").window.unwrap();
    let (x, y, w, h) = window;
    let grip = ((x + 8) as f64, (y + 11) as f64);
    let to = (grip.0 + 35., grip.1 + 30.);
    let before_move = ws_text(&r);
    let before_bounds = r.panes().graph;
    r.step(grip, vec![Event::Pointer_moved(grip)]);
    r.stepb(&[LB], grip, vec![Event::Pointer_pressed(LB, grip)]);
    r.stepb(&[LB], to, vec![Event::Pointer_moved(to)]);
    check(ws_text(&r) == before_move, "a held floating-window drag committed authored state before release");
    r.step(to, vec![Event::Pointer_cancelled(LB)]);
    check(ws_text(&r) == before_move && r.panes().graph == before_bounds, "cancelling a floating-window drag retained draft bounds or changed authored state");
    r.step(grip, vec![Event::Pointer_moved(grip)]);
    r.stepb(&[LB], grip, vec![Event::Pointer_pressed(LB, grip)]);
    r.stepb(&[LB], to, vec![Event::Pointer_moved(to)]);
    r.step(to, vec![Event::Pointer_released(LB, to)]);
    r.step(to, vec![]);
    check(state(&r, "network").window == Some((x + 35, y + 30, w, h)), "an underlying pane covered the floating drag handle");
    r.step_full(&[], &[Key::Meta], to, vec![ch('z')]);
    check(state(&r, "network").window == Some(window), "a second window drag merged with the previous gesture");
    let visible_h = h.min(640 - y);
    let corner = ((x + w - 4) as f64, (y + visible_h - 4) as f64);
    let to = (corner.0 - 35., corner.1 - 40.);
    let before_resize = ws_text(&r);
    r.step(corner, vec![Event::Pointer_moved(corner)]);
    r.stepb(&[LB], corner, vec![Event::Pointer_pressed(LB, corner)]);
    r.stepb(&[LB], to, vec![Event::Pointer_moved(to)]);
    check(ws_text(&r) == before_resize, "a held floating-window resize committed authored state before release");
    r.step(to, vec![Event::Pointer_released(LB, to)]);
    r.step(to, vec![]);
    check(state(&r, "network").window == Some((x, y, w - 35, visible_h - 40)), "the floating window resize handle did not persist its size");
    r.step_full(&[], &[Key::Meta], to, vec![ch('z')]);
    r.step_full(&[], &[Key::Meta], moved, vec![ch('z')]);
    check(state(&r, "network").window.is_none() && !state(&r, "inspector").collapsed, "one undo did not revert the whole window drag");
    // Drop onto the viewport's lower edge: a docked split, preserved panel types, one undo.
    let before = ws_text(&r);
    let (gx, gy, _, _) = r.panes().graph;
    let start = ((gx + 8) as f64, (gy - 11) as f64);
    let target = (180., 580.);
    r.step(start, vec![Event::Pointer_moved(start)]);
    r.stepb(&[LB], start, vec![Event::Pointer_pressed(LB, start)]);
    r.stepb(&[LB], target, vec![Event::Pointer_moved(target)]);
    r.stepb(&[LB], target, vec![]);
    r.step(target, vec![Event::Pointer_released(LB, target)]);
    r.step(target, vec![]);
    check(has(&r.source(), "preview network)") && state(&r, "network").window.is_none() && r.dump("edit error") == "-", &format!("the edge drop did not dock the panel: {}\n{}", r.dump("edit error"), r.source()));
    r.step_full(&[], &[Key::Meta], target, vec![ch('z')]);
    check(ws_text(&r) == before, "one undo did not revert the docking gesture");
    r.step(target, vec![key(Key::KeyChar('/')), key(Key::KeyChar('g'))]);
    r.step(target, vec![]);
    check(state(&r, "network").collapsed, "the leader graph toggle was not saved");
    r.step(target, vec![key(Key::KeyChar('/')), key(Key::KeyChar('a'))]);
    check(!state(&r, "network").collapsed, "Add did not expand the saved collapsed graph");
    r.step(target, vec![]);
    r.step(target, vec![key(Key::Escape)]);
    r.step(target, vec![]);
    r.step(target, vec![key(Key::KeyChar('/')), key(Key::KeyChar('t'))]);
    r.step(target, vec![]);
    check(editor::workspace(&r.e).layout.panels.get(&vec!["editor".into(), "@panel".into(), "-1".into()]).is_some_and(|s| !s.collapsed), "the leader timeline toggle was not saved");
    let prepared = editor::prepared(&r.e);
    let same = |r: &Run| match (&editor::prepared(&r.e), &prepared) {
        // physical equality has no Rust identity (Editor::prepared clones): compare the geometry by size
        (Some(a), Some(b)) => rdk_core::geometry::point_count(a) == rdk_core::geometry::point_count(b),
        (None, None) => true,
        _ => false,
    };
    r.e = editor::set_renderer(r.e.clone(), rays_editor::editor_renderer::Mode::Wireframe);
    r.step(target, vec![]);
    check(editor::renderer(&r.e) == rays_editor::editor_renderer::Mode::Wireframe && same(&r), "the shared wireframe choice recooked the geometry");
    r.e = editor::set_renderer(r.e.clone(), rays_editor::editor_renderer::Mode::Raster);
    r.step(target, vec![]);
    check(editor::renderer(&r.e) == rays_editor::editor_renderer::Mode::Raster && same(&r), "the shared raster choice recooked the geometry");
    r.close();
    let _ = std::fs::remove_dir_all(&directory);
}

pub fn run_ref_picker() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace ref_pick
    (graph scene :context scene
      (let* [geom (scene/geometry (ref g1))]
        (scene/merge geom)))
    (graph g1 :context sop (sop/box))
    (graph g2 :context sop (sop/grid))
    (graph editor :context editor
      (ui/workspace (ui/split "horizontal" (ui/graph "scene") (ui/inspector)))))"#;
    let mut r = Run::new(editor(text));
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    let (ix, iy, _, _) = r.panes().inspector;
    let click = |r: &mut Run, p: (f64, f64)| {
        r.step(p, vec![Event::Pointer_moved(p)]);
        r.step(p, vec![Event::Pointer_pressed(LB, p), Event::Pointer_released(LB, p)]);
    };
    let (bx, by, _, _) = editor::node_box(&r.e, &["scene".into(), "geom".into()]).expect("geom box");
    click(&mut r, ((bx + 60) as f64, (by + 12) as f64));
    r.step(m, vec![]);
    check(r.dump("scope selected") == "scene/geom", &format!("the scene geometry node was not selected, got: {}", r.dump("scope selected")));
    click(&mut r, ((ix + 180) as f64, (iy + 190) as f64));
    r.step(m, vec![]);
    click(&mut r, ((ix + 180) as f64, (iy + 245) as f64));
    r.step(m, vec![]);
    check(has(&r.source(), "(ref g2)"), &format!("inspector ref picker did not change ref to g2: {}", r.source()));
    undo(&mut r);
    r.step(m, vec![]);
    check(has(&r.source(), "(ref g1)"), &format!("undo did not restore ref to g1: {}", r.source()));
    r.close();
}

pub fn run_compose() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace compose
    (graph g1 :context sop (sop/box))
    (graph scene :context scene
      (let* [a (scene/geometry (ref g1))
             b (scene/geometry (ref g1) :translate [3 0 0] :name "b")]
        (scene/root (scene/merge a b))))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "scene") (ui/inspector)))))"#;
    let m = (450., 300.);
    let pick = |typed: &str| {
        let mut r = Run::new(editor(text));
        r.step(m, vec![]);
        r.step(m, vec![]);
        let before = r.source();
        r.step(m, vec![key(Key::KeyChar('/')), ch('a')]);
        r.step(m, vec![text_input(typed)]);
        r.step(m, vec![key(Key::Enter)]);
        r.step(m, vec![]);
        let after = r.source();
        let label = r.undo_label();
        r.step_full(&[], &[Key::Meta], m, vec![ch('z')]);
        r.step(m, vec![]);
        let undone = r.source();
        r.close();
        (before, after, label, undone)
    };
    let flat = |t: &str| t.split_whitespace().collect::<Vec<_>>().join(" ");
    let (before, after, label, undone) = pick("World");
    check(has(&flat(&after), "(scene/world (ref sky))") && has(&flat(&after), "(graph sky :context world") && label == Some("Add World".into()) && undone == before, &format!("/ a World: {after}"));
    let (before, after, label, undone) = pick("Geometry");
    check(has(&flat(&after), "(graph shape :context sop") && has(&flat(&after), "(scene/geometry (ref shape))") && label == Some("Add geometry".into()) && undone == before, &format!("/ a Geometry: {after}"));
    let (before, after, label, undone) = pick("Merge");
    check(has(&flat(&after), "(scene/merge)") && undone == before && label.is_some(), &format!("/ a Merge: {after}"));
    // / e selects the scene's World, a merge member, and enters it
    let mut r = Run::new(editor(
        r#"(workspace sky
    (graph g1 :context sop (sop/box))
    (graph sky :context world (world/sun (world/sky)))
    (graph scene :context scene
      (scene/root (scene/merge (scene/geometry (ref g1)) (scene/world (ref sky) :name "Dome")))))"#,
    ));
    r.step(m, vec![]);
    r.step(m, vec![]);
    r.step(m, vec![key(Key::KeyChar('/')), ch('e')]);
    r.step(m, vec![]);
    check(editor::level(&r.e) == Some("Dome".into()) && !has(&r.source(), "graph world"), "/ e did not enter the World of a scene/world member");
    r.close();
    // / e on a scene with a root and no World makes one
    let mut r = Run::new(editor(
        r#"(workspace plain
    (graph g1 :context sop (sop/box))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g1))))))"#,
    ));
    r.step(m, vec![]);
    r.step(m, vec![]);
    r.step(m, vec![key(Key::KeyChar('/')), ch('e')]);
    r.step(m, vec![]);
    check(has(&flat(&r.source()), "(scene/world (ref sky)") && has(&r.source(), "graph sky :context world") && has(&r.source(), "scene/root") && r.undo_label() == Some("Add World".into()), &format!("/ e did not write the new World: {}", r.source()));
    r.close();
}

const SWITCH_TEXT: &str = r#"
  (workspace sw
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/camera :eye [0 0 6])))
    (graph editor :context editor
      (let* [preview (ui/viewport (ref scene))
             network (ui/graph)
             inspector (ui/inspector)
             code (ui/lisp)
             build (ui/split-at "horizontal" 0.4 preview (ui/split-at "horizontal" 0.62 network inspector))
             write (ui/split-at "horizontal" 0.46 preview (ui/split-at "vertical" 0.5 network code))
             look preview]
        (ui/workspace (ui/switch build write look :active 0)))))
"#;

pub fn run_layouts() {
    if !gpu() {
        return;
    }
    let labels = |text: &str| match shell_of(&build_ok(&of_text(text))).switch {
        Some(sw) => (panels::labels(&sw.layouts), sw.active),
        None => fail("no switch"),
    };
    let sl: Vec<String> = ["View | Graph | Inspector", "View | Graph / Lisp", "View"].iter().map(|s| s.to_string()).collect();
    check(labels(SWITCH_TEXT) == (sl, 0), "the layouts are named from their panels");
    let shell = shell_of(&build_ok(&of_text(SWITCH_TEXT)));
    check(panels::to_string(&shell.tree) == "(h view (h graph inspector))" && !shell.origins.is_empty() && shell.origins.iter().find(|(p, _)| p.is_empty()).map(|(_, o)| o.clone()) == Some(Origin::Bound("build".into())), "the active layout is the tree, its origin the binding the switch names");
    let shell = shell_of(&build_ok(&of_text(&replace(SWITCH_TEXT, ":active 0", ":active 1"))));
    check(panels::to_string(&shell.tree) == "(h view (v graph lisp))", "the switch is transparent: :active picks the tree");
    expect_error("an active layout out of range", &replace(SWITCH_TEXT, ":active 0", ":active 3"), "active layout");
    // edits
    let ws = of_text(SWITCH_TEXT);
    let edit = |ws: &workspace_doc::T, op: L| match workspace_doc::edit_layout(&catalog(), ws, &op) {
        Ok(ws) => ws,
        Err(d) => fail(&rays_diagnostic::to_string(&d)),
    };
    let text = |ws: &workspace_doc::T| workspace_doc::print(ws).unwrap().0;
    let switched = edit(&ws, L::SetLayout { graph: "editor".into(), index: 2 });
    check(has(&text(&switched), ":active 2"), "Set_layout writes :active");
    let added = edit(&switched, L::LayoutNew { graph: "editor".into() });
    check(has(&text(&added), "(ui/switch build write look layout :active 3)") && has(&text(&added), "layout preview"), &format!("a new layout copies the active one: {}", text(&added)));
    let labelled = labels(&text(&added));
    let a: Vec<String> = ["View | Graph | Inspector", "View | Graph / Lisp", "View", "View \u{b7} View 100% (1)"].iter().map(|s| s.to_string()).collect();
    let b: Vec<String> = ["View | Graph | Inspector", "View | Graph / Lisp", "View \u{b7} View 100%", "View \u{b7} View 100% (2)"].iter().map(|s| s.to_string()).collect();
    check(labelled == (a, 3) || labelled.0 == b, &format!("the copy is named like its twin: {}", labelled.0.join(" / ")));
    let removed = edit(&added, L::LayoutRemove { graph: "editor".into() });
    check(has(&text(&removed), "(ui/switch build write look :active") && !has(&text(&removed), "layout preview"), &format!("removing the current layout drops its input and binding: {}", text(&removed)));
    let one = edit(&edit(&edit(&removed, L::SetLayout { graph: "editor".into(), index: 0 }), L::LayoutRemove { graph: "editor".into() }), L::LayoutRemove { graph: "editor".into() });
    match workspace_doc::edit_layout(&catalog(), &one, &L::LayoutRemove { graph: "editor".into() }) {
        Err(d) => check(has(&d.message, "last layout"), "the last layout stays"),
        Ok(_) => fail("removed the last layout"),
    }
    // a window joins the active layout only; it floats and docks
    let floated = edit(&ws, L::LayoutFloat { graph: "editor".into(), at: vec![1, 0] });
    check(panels::label(&shell_of(&build_ok(&floated)).tree) == "View | Inspector + Graph", &format!("a panel floats: {}", text(&floated)));
    let docked = edit(&floated, L::LayoutFloat { graph: "editor".into(), at: vec![1, 0] });
    check(panels::label(&shell_of(&build_ok(&docked)).tree) == "View | Inspector | Graph", &format!("a window docks: {}", text(&docked)));
    // a switch inside a split edits in place
    let nested = of_text(&with_editor("    (ui/workspace (ui/split-at \"horizontal\" 0.5 (ui/graph) (ui/switch (ui/lisp) (ui/inspector) :active 1)))"));
    let n_set = edit(&nested, L::SetLayout { graph: "editor".into(), index: 0 });
    check(has(&text(&n_set), "(ui/switch (ui/lisp) (ui/inspector) :active 0)") || has(&text(&n_set), ":active 0"), &format!("Set_layout reaches a switch inside a split: {}", text(&n_set)));
    check(panels::to_string(&shell_of(&build_ok(&n_set)).tree) == "(h graph lisp)", "and the column swaps");
    // an older file: several editor graphs become the layouts of one switch
    let old_text = {
        let t = with_editor("    (let* [a (ui/graph) b (ui/lisp) s (ui/split \"horizontal\" a b)] (ui/workspace s))");
        format!("{}\n  (graph second :context editor\n    (let* [a (ui/lisp) b (ui/inspector) s (ui/split \"vertical\" a b)] (ui/workspace s))))\n", &t[..t.len() - 2])
    };
    let old = of_text(&old_text);
    let merged = edit(&old, L::MergeLayouts { graph: "editor".into() });
    check(!has(&text(&merged), "(graph second") && has(&text(&merged), "second_a") && has(&text(&merged), "(ui/switch"), &format!("the other editor graph joins the switch: {}", text(&merged)));
    match shell_of(&build_ok(&merged)).switch {
        Some(sw) => check(sw.layouts.len() == 2 && sw.layouts.iter().map(panels::label).collect::<Vec<_>>() == ["Graph | Lisp", "Lisp / Inspector"], "the merged layouts"),
        None => fail("no switch after merging"),
    }
    if workspace_doc::edit_layout(&catalog(), &merged, &L::MergeLayouts { graph: "editor".into() }).is_ok() {
        fail("merged with nothing to merge");
    }
    // a workspace without a switch gets one on the first new layout
    let plain = of_text(&replace(SWITCH_TEXT, "(ui/switch build write look :active 0)", "build"));
    let wrapped = edit(&plain, L::LayoutNew { graph: "editor".into() });
    check(has(&text(&wrapped), "(ui/workspace switch)") && has(&text(&wrapped), "(ui/switch build layout :active 1)"), &format!("a switch is written around the tree: {}", text(&wrapped)));
    // one merged undo entry, by keys
    let mut r = Run::new(editor(SWITCH_TEXT));
    let m = (300., 150.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    r.step(m, vec![key(Key::KeyChar('/')), ch('[')]);
    r.step(m, vec![]);
    // which-key lists the layouts
    r.step(m, vec![ch('1')]);
    r.step(m, vec![]);
    check(has(&r.source(), ":active 1") && r.undo_label() == Some("Layout".into()), "/ [ 1 switches the layout");
    r.step(m, vec![key(Key::KeyChar('/')), ch('['), ch('2')]);
    r.step(m, vec![]);
    check(has(&r.source(), ":active 2"), "/ [ 2 switches again");
    r.step_full(&[], &[Key::Meta], m, vec![ch('z')]);
    r.step(m, vec![]);
    check(has(&r.source(), ":active 0"), "one undo returns past both switches");
    r.step(m, vec![key(Key::KeyChar('/')), ch('['), ch('n')]);
    r.step(m, vec![]);
    check(has(&r.source(), ":active 3") && r.undo_label() == Some("New layout".into()), "/ [ n copies the layout");
    r.step(m, vec![key(Key::KeyChar('/')), ch('['), ch('x')]);
    r.step(m, vec![]);
    check(r.undo_label() == Some("Remove layout".into()) && !has(&r.source(), "layout preview"), "/ [ x removes it");
    r.step(m, vec![key(Key::KeyChar('/')), ch('n')]);
    r.step(m, vec![ch('4')]);
    r.step(m, vec![]);
    check(has(&r.source(), "(ui/floating (ui/inspector))") && r.undo_label() == Some("New window".into()), "/ n 4 opens an inspector window");
    // the layout's one docked panel cannot float: refused, nothing written, the entry stays
    let before = r.source();
    r.step(m, vec![key(Key::KeyChar('/')), ch('o'), ch('f')]);
    r.step(m, vec![]);
    check(r.undo_label() == Some("New window".into()) && r.source() == before, &format!("/ o f on a layout's only docked panel is refused: {:?}", r.undo_label()));
    // in a layout of several panels the focused one floats, as one entry named Float panel
    r.step(m, vec![key(Key::KeyChar('/')), ch('['), ch('0')]);
    r.step(m, vec![]);
    let (gx, gy, gw, gh) = editor::panes(&r.e, &frame(&[], (0., 0.), vec![], r.count)).graph;
    let at = (gx as f64 + gw as f64 / 2., gy as f64 + gh as f64 / 2.);
    r.step(at, vec![Event::Pointer_moved(at)]);
    r.step(at, vec![Event::Pointer_pressed(LB, at)]);
    r.step(at, vec![Event::Pointer_released(LB, at)]);
    r.step(at, vec![]);
    r.step(at, vec![key(Key::KeyChar('/')), ch('o'), ch('f')]);
    r.step(at, vec![]);
    check(r.undo_label() == Some("Float panel".into()) && has(&r.source(), "(ui/floating"), &format!("/ o f floats the focused panel: {:?}\n{}", r.undo_label(), r.source()));
    r.close();
}
