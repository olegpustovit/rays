//! test_workspace_shell.ml, part 3: run_ops, run_restore, run_camera_zoom, run_cameras,
//! run_instances, run_views.
use super::support::*;
use editor_core::panels::{self, Panel, Size};
use editor_document::layout_edit::{Axis, Op as L, Side, Size as LSize};
use editor_document::panel_kinds;
use editor_document::workspace_doc;
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::syntax::{self as s, Draft as Node};
use input::{Event, Key, MouseButton};
use pxui_shell::layout;
use rays_editor::Editor;

const LB: MouseButton = MouseButton::LeftButton;

/// The layout sections need the panels of variations, not its sphere lowerings.
pub fn cheap_variations() -> String {
    let text = case("variations");
    let text = if text.contains("(sop/uv_sphere :radius 0.06 :segments 8 :rings 4)") { replace(&text, "(sop/uv_sphere :radius 0.06 :segments 8 :rings 4)", "(sop/box)") } else { text };
    replace(&text, "(range 4)", "(range 1)")
}

fn panes_graph(e: &Editor::t) -> i64 {
    Editor::panes(e, &idle_frame(0)).graph.2
}

fn p(a: &[&str]) -> Vec<String> {
    a.iter().map(|s| s.to_string()).collect()
}

pub fn run_ops() {
    if !gpu() {
        return;
    }
    let e = editor(&cheap_variations());
    let ok = |label: &str, op: L| -> Editor::t {
        match Editor::edit_layout(e.clone(), &op) {
            Ok(e2) => {
                check(Editor::undo_label(&e2) == Some(label.into()), &format!("label {label}: {:?}", Editor::undo_label(&e2)));
                e2
            }
            Err(m) => fail(&format!("{label}: {m}")),
        }
    };
    let tree = |e: &Editor::t| shell_of(&build_ok(&Editor::workspace(e))).tree;
    let sh = tree(&e);
    // resize
    let r = ok("Resize panel", L::SetLayoutSize { node: p(&["editor", "left"]), size: LSize::Ratio(0.3) });
    check(has(&source(&r), "(ui/split-at \"vertical\" 0.3 network code)"), "Set_layout_ratio");
    let clamped = tree(&ok("Resize panel", L::SetLayoutSize { node: p(&["editor", "left"]), size: LSize::Ratio(5.) }));
    check(
        match &clamped {
            panels::t::Split { b, .. } => match &**b {
                panels::t::Split { a, .. } => matches!(&**a, panels::t::Split { size: Size::Ratio(x), .. } if *x == 0.9),
                _ => false,
            },
            _ => false,
        },
        "a ratio clamps to 0.9",
    );
    // a ui/split (no ratio) becomes ui/split-at
    let plain = editor(&with_editor("    (let* [a (ui/graph) b (ui/lisp) s (ui/split \"horizontal\" a b)] (ui/workspace s))"));
    match Editor::edit_layout(plain.clone(), &L::SetLayoutSize { node: p(&["editor", "s"]), size: LSize::Ratio(0.25) }) {
        Ok(q) => check(has(&source(&q), "(ui/split-at \"horizontal\" 0.25 a b)"), "ui/split gets a ratio"),
        Err(m) => fail(&m),
    }
    // split, close, retype: the editor's panel table writes the panel forms
    let panel = |kind: &str| panel_kinds::expr(&Editor::workspace(&e).source, kind).expect("panel kind");
    let sp = ok("Split panel", L::SplitPanel { node: p(&["editor", "network"]), axis: Axis::V, ratio: 0.5, panel: panel("lisp") });
    check(panels::to_string(&tree(&sp)) == "(h outline (h (v (v graph lisp) lisp) (tile view)))", &format!("split: {}", panels::to_string(&tree(&sp))));
    check(has(&source(&sp), "(ui/split-at \"vertical\" 0.5 network_a network_b)"), "the new split");
    let c = ok("Close panel", L::ClosePanel { node: p(&["editor", "network"]) });
    check(panels::to_string(&tree(&c)) == "(h outline (h lisp (tile view)))", &format!("close: {}", panels::to_string(&tree(&c))));
    let t = ok("Retype panel", L::SetPanelKind { node: p(&["editor", "network"]), panel: panel("list") });
    check(panels::to_string(&tree(&t)) == "(h outline (h (v list lisp) (tile view)))", &format!("retype: {}", panels::to_string(&tree(&t))));
    let sheet = ok("Retype panel", L::SetPanelKind { node: p(&["editor", "network"]), panel: panel("spreadsheet") });
    check(panels::to_string(&tree(&sheet)) == "(h outline (h (v spreadsheet lisp) (tile view)))", "retype spreadsheet");
    let saved = workspace_doc::to_text(&Editor::workspace(&sheet)).unwrap();
    check(workspace_doc::to_text(&of_text(&saved)).unwrap() == saved, "retyped spreadsheet did not round trip");
    let v = ok("Retype panel", L::SetPanelKind { node: p(&["editor", "outline"]), panel: panel("viewport") });
    check(has(&source(&v), "outline (ui/viewport (ref scene))"), "a viewport is over the first scene graph");
    let docked = ok("Dock panel", L::DockPanel { node: p(&["editor", "network"]), target: p(&["editor", "outline"]), side: Side::Right });
    check(panels::to_string(&tree(&docked)) == "(h (h outline graph) (h lisp (tile view)))", &format!("docking duplicated a panel or left a gap: {}", panels::to_string(&tree(&docked))));
    check(has(&source(&docked), "(ui/split-at \"horizontal\" 0.5 outline network)"), "docking was not reflected in the editor Lisp");
    // undo restores the tree with the source
    check(panels::to_string(&tree(&e)) == panels::to_string(&sh), "undo target");
    // refusals leave everything alone
    let refuse = |name: &str, op: L, needle: &str| match Editor::edit_layout(e.clone(), &op) {
        Err(m) => check(has(&m, needle), &format!("{name}: {m}")),
        Ok(_) => fail(&format!("{name} was accepted")),
    };
    refuse("close the workspace", L::ClosePanel { node: p(&["editor", "shell"]) }, "Keep at least one");
    check(panel_kinds::expr(&Editor::workspace(&e).source, "dashboard").err() == Some("Unknown panel type dashboard.".to_string()), "an unknown panel type");
    refuse("resize a panel", L::SetLayoutSize { node: p(&["editor", "network"]), size: LSize::Ratio(0.4) }, "not a split");
    refuse("no such binding", L::SplitPanel { node: p(&["editor", "nothing"]), axis: Axis::H, ratio: 0.5, panel: panel("lisp") }, "no longer exists");
    let floating = editor(&with_editor("(let* [main (ui/graph) outline (ui/outline) window (ui/floating outline) tiles (ui/tile main window)] (ui/workspace tiles))"));
    match Editor::edit_layout(floating.clone(), &L::ClosePanel { node: p(&["editor", "outline"]) }) {
        Ok(closed) => {
            check(panels::to_string(&tree(&closed)) == "(tile graph)", "closing a floating panel left its wrapper");
            check(!has(&source(&closed), "ui/floating"), "empty floating wrapper remained in Lisp");
            match Editor::edit_layout(closed, &L::ClosePanel { node: p(&["editor", "main"]) }) {
                Err(message) => check(has(&message, "Keep at least one"), "last panel refusal"),
                Ok(_) => fail("closing the last panel was accepted"),
            }
        }
        Err(message) => fail(&message),
    }
    let switched = editor(&with_editor("(let* [main (ui/graph) outline (ui/outline) window (ui/floating outline)] (ui/workspace (ui/switch (ui/split-at \"horizontal\" 0.5 main window) (ui/lisp) :active 0)))"));
    match Editor::edit_layout(switched.clone(), &L::ClosePanel { node: p(&["editor", "outline"]) }) {
        Ok(closed) => {
            check(!has(&source(&closed), "ui/floating") && has(&source(&closed), "(ui/switch main (ui/lisp)"), "closing a floating panel did not clean its switch branch");
            match Editor::edit_layout(closed, &L::ClosePanel { node: p(&["editor", "main"]) }) {
                Err(message) => check(has(&message, "every layout"), "empty switch branch refusal"),
                Ok(_) => fail("closing the last panel in a switch branch was accepted"),
            }
        }
        Err(message) => fail(&message),
    }
    // a panel that follows a graph panel keeps following it when a window docks on that panel or is closed
    let followed = editor(&with_editor("(let* [network (ui/graph) inspector (ui/inspector :of network)] (ui/workspace (ui/split-at \"horizontal\" 0.5 network inspector)))"));
    match Editor::edit_layout(followed.clone(), &L::LayoutWindow { graph: "editor".into(), panel: panel_kinds::expr(&[], "outline").expect("outline") }) {
        Err(message) => fail(&format!("new window: {message}")),
        Ok(windowed) => match Editor::edit_layout(windowed, &L::DockPanel { node: p(&["editor", "inspector"]), target: p(&["editor", "network"]), side: Side::Left }) {
            Ok(docked) => check(has(&source(&docked), ":of network)"), &format!("docking on a followed panel renamed :of: {}", source(&docked))),
            Err(message) => fail(&format!("dock on a followed panel: {message}")),
        },
    }
    Editor::close(&followed);
    Editor::close(&switched);
    Editor::close(&floating);
    Editor::close(&plain);
    Editor::close(&e);
}

pub fn run_restore() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(&cheap_variations()));
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    check(panes_graph(&r.e) > 0, "the shell starts with a graph panel");
    // an invalid editor graph is refused whole: the layout and the document stay
    let before = r.source();
    match Editor::edit(r.e.clone(), &Op::SetArg { node: p(&["editor", "panels"]), key: ArgKey::Pos(1), sub: vec![], value: s::make(Node::Num("0.95".into())) }) {
        Err(_) => {}
        Ok(_) => fail("a ratio of 0.95 was accepted"),
    }
    check(r.source() == before && panes_graph(&r.e) > 0, "a refused edit keeps the layout");
    // a valid one that leaves only an outline: nothing to edit the layout with
    let mut builder = s::SyntaxBuilder::new();
    let head = builder.push_draft(0, Node::Sym("ui/outline".into()), Default::default(), None);
    let id = builder.push_draft(0, Node::List(vec![head]), Default::default(), None);
    let bricked = Editor::edit(r.e.clone(), &Op::SetArg { node: p(&["editor", "shell"]), key: ArgKey::Pos(0), sub: vec![], value: builder.freeze().handle(id) }).expect("bricked");
    r.e = bricked.clone();
    r.step(m, vec![]);
    check(panes_graph(&r.e) == 0, "the bricked shell has no graph panel");
    r.step(m, vec![key(Key::KeyChar('/')), key(Key::KeyChar('z'))]);
    r.step(m, vec![]);
    check(panes_graph(&r.e) > 0, "/ z restores the default layout");
    check(has(&r.dump("cook"), "Default layout"), &format!("the status says so: {}", r.dump("cook")));
    check(r.source() == source(&bricked), "the document is untouched by Restore");
    // the editor graph is repaired from the restored shell: the layout edit ends the restore
    let repaired = Editor::edit(r.e.clone(), &Op::SetArg { node: p(&["editor", "shell"]), key: ArgKey::Pos(0), sub: vec![], value: s::make(Node::Sym("panels".into())) }).expect("repaired");
    r.e = repaired;
    r.step(m, vec![]);
    check(panes_graph(&r.e) > 0 && !has(&r.dump("cook"), "Default layout"), "editing the editor graph ends the restore");
    // pressing it twice returns to the editor graph's layout
    r.e = bricked;
    r.step(m, vec![]);
    r.step(m, vec![key(Key::KeyChar('/')), key(Key::KeyChar('z'))]);
    r.step(m, vec![]);
    r.step(m, vec![key(Key::KeyChar('/')), key(Key::KeyChar('z'))]);
    r.step(m, vec![]);
    check(panes_graph(&r.e) == 0, "the second / z returns to the editor graph");
    r.close();
}

pub fn run_camera_zoom() {
    if !gpu() {
        return;
    }
    let camera = rays::easy_camera::create(None, None, None, None, None, Some(false)).expect("camera");
    let mut e = editor_with(
        "(workspace zoom (graph g :context sop (sop/box)) (graph scene :context scene (scene/merge (scene/geometry (ref g)) (scene/camera :eye [0 0 12345.123456789] :follow_viewport true :active true))))",
        Editor::Create { camera: Some(camera), ..Default::default() },
    );
    let mut step = |e: &mut Editor::t, events: Vec<Event>, count: i64| *e = Editor::update(e.clone(), &frame(&[], (200., 200.), events, count));
    step(&mut e, vec![], 0);
    step(&mut e, vec![], 1);
    step(&mut e, vec![Event::Pointer_moved((200., 200.)), Event::Pointer_pressed(LB, (200., 200.)), Event::Pointer_released(LB, (200., 200.))], 2);
    for count in 3..=15 {
        step(&mut e, vec![Event::Scrolled((0., -1.))], 2 * count);
        let viewport = rays::easy_camera::camera(&Editor::camera(&e)).expect("camera");
        let saved = Editor::render_camera(&e);
        check(rays_math::vec3::nearly_equal(rays::camera::position(&viewport), rays::camera::position(&saved), 1e-6), "zooming out made the following camera lag behind the viewport");
        let eye = rays::camera::position(&viewport);
        step(&mut e, vec![], 2 * count + 1);
        check(rays_math::vec3::nearly_equal(eye, rays::camera::position(&rays::easy_camera::camera(&Editor::camera(&e)).expect("camera")), 1e-6), "an idle frame snapped the viewport to rounded camera coordinates");
    }
    Editor::close(&e);
}

pub fn run_cameras() {
    if !gpu() {
        return;
    }
    let camera = rays::easy_camera::with_inertia(false, &rays::easy_camera::create(Some(rays_math::vec3::zero()), Some(7.), None, None, None, None).expect("camera"));
    let mut r = Run::new(editor_with(&case("variations"), Editor::Create { camera: Some(camera), ..Default::default() }));
    let stepd = |r: &mut Run, buttons: &[MouseButton], delta: (f64, f64), at: (f64, f64), events: Vec<Event>| {
        r.count += 1;
        r.e = Editor::update(r.e.clone(), &frame_full(buttons, &[], delta, at, events, r.count));
    };
    for _ in 0..8 {
        r.step((450., 300.), vec![]);
    }
    let geometry = |r: &Run| layout::geometry(None, Some(&[Panel::Timeline]), &shell_of(&build_ok(&Editor::workspace(&r.e))).tree, &idle_frame(0));
    let center = |r: &Run, key: &str| {
        let leaf = layout::find(&geometry(r), &Panel::View(key.into())).expect("leaf");
        let (x, y, w, h) = leaf.body;
        ((x + w / 2) as f64, (y + h / 2) as f64)
    };
    let position = |r: &Run, key: &str| rays::camera::position(&Editor::viewport_camera(&r.e, key));
    let keys = ["v1.1.0", "v1.1.1", "v1.1.2", "v1.1.3"];
    let click = |r: &mut Run, key: &str| {
        let pt = center(r, key);
        r.step(pt, vec![Event::Pointer_moved(pt)]);
        r.stepb(&[LB], pt, vec![Event::Pointer_pressed(LB, pt)]);
        r.step(pt, vec![Event::Pointer_released(LB, pt)]);
        r.step(pt, vec![]);
    };
    // a press at the point of the click just before would be a double click
    let orbit = |r: &mut Run, key: &str, delta: (f64, f64)| {
        let (x, y) = center(r, key);
        let y = y + 40.;
        r.step((x, y), vec![Event::Pointer_moved((x, y))]);
        r.stepb(&[LB], (x, y), vec![Event::Pointer_pressed(LB, (x, y))]);
        for k in 1..=6 {
            let pt = (x + k as f64 * delta.0, y + k as f64 * delta.1);
            stepd(r, &[LB], delta, pt, vec![Event::Pointer_moved(pt)]);
        }
        let last = (x + 6. * delta.0, y + 6. * delta.1);
        r.step(last, vec![Event::Pointer_released(LB, last)]);
        r.step(last, vec![]);
    };
    let same = |a, b| rays_math::vec3::nearly_equal(a, b, 1e-9);
    click(&mut r, "v1.1.1");
    let start: Vec<(&str, _)> = keys.iter().map(|k| (*k, position(&r, k))).collect();
    let at = |k: &str| start.iter().find(|(n, _)| *n == k).unwrap().1;
    check(start.iter().all(|(_, pos)| same(*pos, start[0].1)), "viewports start from one orbit");
    orbit(&mut r, "v1.1.1", (20., 0.));
    check(!same(position(&r, "v1.1.1"), at("v1.1.1")), "a drag did not orbit the focused viewport");
    for k in ["v1.1.0", "v1.1.2", "v1.1.3"] {
        check(same(position(&r, k), at(k)), &format!("{k} moved with another viewport's drag"));
    }
    let first = position(&r, "v1.1.1");
    click(&mut r, "v1.1.2");
    check(same(position(&r, "v1.1.1"), first), "focusing another viewport moved the one left");
    orbit(&mut r, "v1.1.2", (-20., 10.));
    check(!same(position(&r, "v1.1.2"), at("v1.1.2")) && !same(position(&r, "v1.1.2"), first), "the second viewport did not orbit on its own");
    check(same(position(&r, "v1.1.1"), first) && same(position(&r, "v1.1.0"), at("v1.1.0")), "the second drag disturbed the first viewport");
    click(&mut r, "v1.1.1");
    check(same(position(&r, "v1.1.1"), first), "returning to a viewport lost its orbit");
    // a click on the geometry of a viewport over another scene instance selects the node that made it
    let graph_pane = {
        let (x, y, _, _) = layout::find(&geometry(&r), &Panel::Graph).expect("graph leaf").body;
        ((x + 20) as f64, (y + 200) as f64)
    };
    r.step(graph_pane, vec![Event::Pointer_moved(graph_pane)]);
    r.stepb(&[LB], graph_pane, vec![Event::Pointer_pressed(LB, graph_pane)]);
    r.step(graph_pane, vec![Event::Pointer_released(LB, graph_pane)]);
    r.step(graph_pane, vec![key(Key::KeyChar('/')), ch('l'), ch('g')]);
    r.step(graph_pane, vec![]);
    check(has(&r.dump("projection"), "graph"), &format!("the pane is not showing a graph: {}", r.dump("projection")));
    click(&mut r, "v1.1.2");
    check(r.dump("pane graph") == "garden" && r.dump("scope selected") != "-", &format!("a click in another instance's viewport selected nothing (pane {}, selected {})", r.dump("pane graph"), r.dump("scope selected")));
    r.close();
}

pub fn run_instances() {
    if !gpu() {
        return;
    }
    let text = r#"(workspace days
  (graph g :context sop (sop/box))
  (graph noon :context world (world/sun (world/sky :name "noon") :name "sun"))
  (graph dusk :context world (world/sun (world/sky :name "dusk") :name "sun"))
  (graph set :context scene
    (scene/merge (scene/geometry (ref g) :name "body") (scene/camera :name "hero")))
  (graph day :context scene
    (scene/root (scene/merge (ref set) (scene/world (ref noon) :name "Noon")) :renderer "Raster"))
  (graph night :context scene
    (scene/root (scene/merge (ref set) (scene/world (ref dusk) :name "Dusk" :exposure -2))
                :renderer "Path traced" :max_spp 1024 :bounces 12))
  (graph editor :context editor
    (ui/workspace (ui/split-at "horizontal" 0.5 (ui/viewport (ref day)) (ui/viewport (ref night))))))"#;
    let mut e = editor(text);
    for c in 1..=12 {
        e = Editor::update(e, &frame(&[], (450., 300.), vec![], c));
    }
    check(!Editor::objects(&e).is_empty() && Editor::prepared(&e).is_some(), &format!("the editor did not cook (cook {})", dump_line(&e, "cook")));
    check(!has(&dump_line(&e, "edit error"), "Failure"), &format!("an error composing the views: {}", dump_line(&e, "edit error")));
    Editor::close(&e);
}

pub fn run_views() {
    if !gpu() {
        return;
    }
    let mut e = editor(&case("variations"));
    for c in 1..=12 {
        e = Editor::update(e, &frame(&[], (450., 300.), vec![], c));
    }
    check(!Editor::objects(&e).is_empty() && Editor::prepared(&e).is_some(), &format!("the editor did not cook ({} objects, prepared {}, cook {})", Editor::objects(&e).len(), Editor::prepared(&e).is_some(), dump_line(&e, "cook")));
    for c in 200..=230 {
        e = Editor::update(e, &frame(&[], (450., 300.), vec![], c));
    }
    let scene = Editor::scene(&e, &frame(&[], (450., 300.), vec![], 231));
    let staged = rays::scene::Layer::stage_native(900, 640, &scene);
    Editor::close(&e);
    match staged {
        Err(m) => fail(&m),
        Ok(staged) => {
            let viewports = staged.layers.iter().filter(|l| matches!(l, rays::scene::Layer::native_layer::Scene3_layer(_))).count();
            check(viewports == 4, &format!("four viewports draw four 3D layers, got {viewports}"));
        }
    }
}
