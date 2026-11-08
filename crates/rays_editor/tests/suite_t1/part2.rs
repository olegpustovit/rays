//! test_workspace_shell.ml, part 2: run_values, run_duplicate_and_view, run_loop_view,
//! run_result_view, run_geometry_view, run_movers.
use super::support::*;
use flow_graph::flow_edit::{ArgKey, Op};
use input::{Event, Key, MouseButton};
use rays_editor::editor;

const L: MouseButton = MouseButton::LeftButton;

/// Editor_prepare.centroid of the prepared geometry: the x of its mesh centre.
fn centre_x(e: &editor::Editor) -> Option<f64> {
    let geometry = editor::prepared(e)?;
    let mesh = rdk_rays::rays_mesh::to_mesh(&geometry).ok()?;
    rays::mesh::centroid(&mesh).ok().flatten().map(|p| p.x)
}

fn click_at(r: &mut Run, p: (f64, f64)) {
    r.step(p, vec![Event::Pointer_moved(p)]);
    r.step(p, vec![Event::Pointer_pressed(L, p), Event::Pointer_released(L, p)]);
}

pub fn run_values() {
    if !gpu() {
        return;
    }
    let started = |text: Option<&str>| {
        let opening = text.is_none();
        let text = text.unwrap_or("(workspace w (graph g :context sop (sop/box)))");
        let mut r = Run::new(editor(text));
        r.step((450., 300.), vec![]);
        r.step((450., 300.), vec![]);
        let (gx, gy, _, _) = r.panes().graph;
        let p = ((gx + 40) as f64, (gy + 40) as f64);
        r.step(p, vec![Event::Pointer_moved(p)]);
        r.step(p, vec![Event::Pointer_pressed(L, p)]);
        r.step(p, vec![Event::Pointer_released(L, p)]);
        if opening {
            r.step(p, vec![key(Key::Home)]);
            r.step(p, vec![key(Key::KeyChar('i'))]);
        }
        r.step(p, vec![]);
        (r, p)
    };
    let add = |workspace: Option<&str>, pick: &dyn Fn(&editor::Editor) -> usize, text: &str| -> editor::Editor {
        let (mut r, p) = started(workspace);
        if workspace.is_none() {
            check(editor::level(&r.e) == Some("g".into()), "the box graph is open");
        }
        r.step(p, vec![key(Key::KeyChar('/')), ch('a')]);
        r.step(p, vec![text_input(text)]);
        for _ in 0..pick(&r.e) {
            r.step(p, vec![key(Key::ArrowDown)]);
        }
        r.step(p, vec![key(Key::Enter)]);
        r.step(p, vec![]);
        r.e
    };
    let add0 = |text: &str| add(None, &|_| 0, text);
    let e = add0("number");
    check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), "value 1.0"), &format!("a number binding: {}", source(&e)));
    editor::close(&e);
    let e = add0("time");
    check(has(&source(&e), "time t"), &format!("a time binding: {}", source(&e)));
    editor::close(&e);
    let e = add0("sin");
    check(has(&source(&e), "(sin 0.5)"), &format!("an operator binding with typed defaults: {}", source(&e)));
    editor::close(&e);
    let e = add0("str");
    check(has(&source(&e), "(str \"text\")"), &format!("a str binding: {}", source(&e)));
    editor::close(&e);
    let facs = factories();
    check(!facs.iter().any(|f| sop::edit_graph::factory_key(f) == "line"), "Line production menu still uses an OCaml registration");
    let e = add0("line");
    check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), "(sop/line)"), &format!("Line actual zero-input menu insertion: {}", source(&e)));
    let ws = editor::workspace(&e);
    let _ = build_ok(&ws);
    let pane = flow_graph::projection::of_graph(&catalog(), &ws.checked, "g").expect("scope");
    check(pane.nodes.iter().any(|c| c.head == "sop/line"), "Line menu insertion has no geometry card");
    editor::close(&e);
    check(!facs.iter().any(|f| sop::edit_graph::factory_key(f) == "points"), "Points production menu still uses an OCaml registration");
    let pick_points = |editor: &editor::Editor| -> usize {
        let doc = build_ok(&editor::workspace(editor));
        let lowered = &doc.workspace.lowered;
        let mut all = factories();
        all.extend(lowered.factories.iter().cloned());
        let entries: Vec<pxui_graph::node_menu::Entry> = flow_sop::catalog::with_library(all.iter().map(flow_sop::catalog::descriptor).collect())
            .into_iter()
            .map(|d| pxui_graph::node_menu::Entry {
                key: d.key.clone(),
                label: d.label.clone(),
                category: d.category.clone(),
                arity: d.slots.len() as i64 - d.keyword_inputs.len() as i64,
                context: "sop".into(),
                output: flow_syntax::ty::geometry(),
                off: None,
            })
            .collect();
        let menu = pxui_graph::node_menu::create(None, 0, 0, entries);
        pxui_graph::node_menu::keys(&menu, "points").iter().position(|k| k == "points").expect("points in the menu")
    };
    let e = add(None, &pick_points, "points");
    check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), "(sop/points)"), &format!("Points actual zero-input menu insertion: {}", source(&e)));
    let ws = editor::workspace(&e);
    let _ = build_ok(&ws);
    let pane = flow_graph::projection::of_graph(&catalog(), &ws.checked, "g").expect("scope");
    check(pane.nodes.iter().any(|c| c.head == "sop/points"), "Points menu insertion has no geometry card");
    editor::close(&e);
    for (name, expression) in [("array/compact", "(array/compact (array/int 0))"), ("array/scan_sum", "(array/scan_sum (array/int 0))")] {
        let e = add0(name);
        check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), expression), &format!("ordered integer array menu insertion: {}", source(&e)));
        editor::close(&e);
    }
    for (name, expression) in [("array/grade", "(array/grade (array/float 4))"), ("array/index_of", "(array/index_of (array/int 0) (array/int 0))")] {
        let e = add0(name);
        check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), expression), &format!("grade/index menu insertion: {}", source(&e)));
        editor::close(&e);
    }
    let e = add0("array/scatter_add");
    check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), "(array/scatter_add (array/int 0) (array/float 0) 0)"), &format!("empty ordered scatter menu insertion: {}", source(&e)));
    editor::close(&e);
    let e = add0("sop/mesh");
    let s = source(&e);
    check(
        editor::undo_label(&e) == Some("Add node".into()) && has(&s, "(sop/mesh") && has(&s, ":points (array/vec3 0)") && has(&s, ":vertex_points (array/int 0)") && has(&s, ":primitive_offsets (array/int 1)") && has(&s, ":kinds (array/int 0)"),
        &format!("required mesh keyword arrays were absent from production menu insertion: {s}"),
    );
    editor::close(&e);
    let e = add0("sop/mesh_planes");
    let s = source(&e);
    check(
        editor::undo_label(&e) == Some("Add node".into()) && has(&s, "(sop/mesh_planes") && has(&s, ":points {:x (array/float 0)") && has(&s, ":y (array/float 0)") && has(&s, ":z (array/float 0)}") && has(&s, ":primitive_offsets (array/int 1)"),
        &format!("required typed mesh planes were absent from production menu insertion: {s}"),
    );
    editor::close(&e);
    let e = add0("sop/with_attr");
    let s = source(&e);
    check(
        editor::undo_label(&e) == Some("Add node".into()) && has(&s, "(sop/with_attr (sop/mesh") && has(&s, "\"attribute\"") && has(&s, "(array/vec3 0)"),
        &format!("ordinary full attribute menu starter lost typed legacy array/geometry defaults: {s}"),
    );
    {
        let saved = of_text(&s);
        let doc = build_ok(&saved);
        let graph = doc.workspace.lowered.graphs.iter().find(|g| g.name == "g").expect("graph g").clone();
        let info = sop::edit_graph::inspect(&graph.network.geometry).into_iter().find(|i| i.operation == "flow.with_attr").expect("with_attr node");
        let node = sop::edit_graph::compile_node(&graph.network.geometry, info.id).expect("compile");
        let mut session = sop::session::create(32, 1_000_000).expect("session");
        let output = sop::session::cook(&mut session, None, &sop::context::create(Default::default()).expect("context"), &node).expect("cook");
        let geo = sop::payload::geometry(&output.payload).expect("geometry");
        check(rdk_core::geometry::point_count(&geo) == 0, "ordinary attribute menu starter did not cook actual empty geometry");
        let attr = rdk_core::geometry::find_attribute(rdk_core::attribute::Owner::Point, "attribute", &geo).expect("attribute");
        check(matches!(rdk_core::attribute::view::storage(&attr), Ok(rdk_core::attribute::Storage::Float3(data)) if rdk_core::packed::float3::length(data) == 0), "ordinary attribute menu starter lost vec3 storage");
        sop::session::close(&mut session);
    }
    editor::close(&e);
    let e = add0("sop/remap");
    let s = source(&e);
    check(
        editor::undo_label(&e) == Some("Add node".into()) && has(&s, "(sop/remap (sop/mesh") && has(&s, ":points (array/int 0)") && has(&s, ":vertices (array/int 0)") && has(&s, ":primitives (array/int 0)"),
        &format!("required remap maps and cooked empty geometry defaults were absent from production menu insertion: {s}"),
    );
    editor::close(&e);
    let e = add0("attr_from_image");
    let s = source(&e);
    check(editor::undo_label(&e) == Some("Add node".into()) && has(&s, "sop/attr_from_image") && has(&s, ":image (image/noise :width 1 :height 1)"), &format!("a mixed SOP has a real image default through the add menu: {s}"));
    editor::close(&e);
    for op in ["frame/dt", "array/count", "array/nth", "array/sum"] {
        let e = add0(op);
        check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), &format!("({op}")), &format!("a frame/array operator has checked menu defaults: {}", source(&e)));
        editor::close(&e);
    }
    let workspace = "(workspace w (graph g :context draw (draw/merge)) (graph editor :context editor (ui/workspace (ui/graph \"g\" :focus true))))";
    for op in flow_check::op::all() {
        if op.ctx == flow_syntax::context::draw() {
            let e = add(Some(workspace), &|_| 0, &op.name);
            check(editor::undo_label(&e) == Some("Add node".into()) && has(&source(&e), &format!("({}", op.name)), &format!("a drawing operator is added through the menu: {}", source(&e)));
            editor::close(&e);
        }
    }
}

pub fn run_duplicate_and_view() {
    if !gpu() {
        return;
    }
    for text in [
        "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [20 0 0])] b)))",
        "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [20 0 0])] b))\n       (graph scene :context scene (scene/merge (scene/geometry (ref g))\n         (scene/camera :eye [0 0 6] :follow_viewport true))))",
    ] {
        let mut r = Run::new(editor(text));
        let m = (450., 300.);
        r.step(m, vec![]);
        r.step(m, vec![]);
        check(centre_x(&r.e) == Some(20.), "the initial viewport did not show the graph result");
        r.step((150., 300.), vec![Event::Pointer_moved((150., 300.))]);
        r.step((150., 300.), vec![Event::Scrolled((0., -1.))]);
        r.step(m, vec![]);
        let (gx, gy, _, _) = r.panes().graph;
        let p = ((gx + 40) as f64, (gy + 40) as f64);
        r.step(p, vec![Event::Pointer_moved(p)]);
        r.step(p, vec![Event::Pointer_pressed(L, p)]);
        r.step(p, vec![Event::Pointer_released(L, p)]);
        r.step(p, vec![key(Key::Home)]);
        r.step(p, vec![key(Key::KeyChar('i'))]);
        r.step(p, vec![]);
        r.step(p, vec![key(Key::ArrowRight)]);
        check(r.dump("scope selected") != "-", "a walk key selected a node");
        let node = r.dump("scope selected");
        let before = r.source();
        let label = r.undo_label();
        r.step(p, vec![key(Key::KeyChar('v'))]);
        check(r.source() == before && r.undo_label() == label, "v changed the workspace or history");
        let layout_text = editor_document::layout_by_path::to_syntax(&editor::workspace(&r.e).layout);
        check(!has(&flow_syntax::lisp::print(&[layout_text]).unwrap().0, "(display"), "the viewed node is not saved in the layout");
        r.step(p, vec![]);
        check(sop::node::label(&editor::displayed_node(&r.e)) == "box" && centre_x(&r.e) == Some(0.), &format!("v marked the node but did not show it in the viewport: {node}"));
        let (x, y, w, _) = editor::node_box(&r.e, &["g".into(), "b".into()]).expect("box b");
        let target = ((x + w / 2) as f64, (y + 3) as f64);
        r.step(target, vec![Event::Pointer_moved(target)]);
        r.step(target, vec![Event::Pointer_pressed(L, target), Event::Pointer_released(L, target)]);
        r.step(target, vec![key(Key::KeyChar('v'))]);
        r.step(p, vec![]);
        check(centre_x(&r.e) == Some(20.), "viewing the result did not restore its geometry");
        r.step_full(&[], &[Key::Meta], p, vec![key(Key::KeyChar('d'))]);
        check(r.undo_label() == Some("Duplicate".into()) && has(&r.source(), "_2"), &format!("Command-D made no copy: {}", r.source()));
        check(r.dump("scope selected") != node && r.dump("scope selected") != "-", "the copy is the selection");
        r.close();
    }
}

pub fn run_loop_view() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(
        r#"(workspace loop_view
    (graph g :context sop
      (let* [a (sop/box)
             rows (for [i (range 3)] (sop/transform a :translate [(+ 5 (* i 10)) 0 0]))]
        (sop/merge rows)))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :eye [0 0 6] :follow_viewport true))))"#,
    ));
    let m = (450., 300.);
    let centre = |r: &Run, what: &str, expected: f64| {
        let actual = centre_x(&r.e);
        check(actual.is_some_and(|x| (x - expected).abs() < 1e-4), &format!("{what}: expected the geometry at {expected:.1}, got {actual:?}"));
    };
    r.step(m, vec![]);
    r.step(m, vec![]);
    centre(&r, "the result", 15.);
    let (gx, gy, _, _) = r.panes().graph;
    click_at(&mut r, ((gx + 40) as f64, (gy + 40) as f64));
    r.step(m, vec![key(Key::Home)]);
    r.step(m, vec![key(Key::KeyChar('i'))]);
    r.step(m, vec![]);
    let inner: Vec<String> = ["g", "rows", "@result"].iter().map(|s| s.to_string()).collect();
    let (x, y, w, _) = editor::node_box(&r.e, &inner).expect("loop body box");
    click_at(&mut r, ((x + w / 2) as f64, (y + 3) as f64));
    check(r.dump("scope selected") == "g/rows/@result", "the node inside the loop could not be selected");
    let before = r.source();
    let label = r.undo_label();
    r.step(m, vec![key(Key::KeyChar('v'))]);
    r.step(m, vec![]);
    check(r.source() == before && r.undo_label() == label, "viewing a loop body changed the document or history");
    centre(&r, "iteration zero", 5.);
    r.e = editor::set_probe(r.e.clone(), &["g".into(), "rows".into()], 1);
    r.step(m, vec![]);
    centre(&r, "iteration one", 15.);
    r.e = editor::set_probe(r.e.clone(), &["g".into(), "rows".into()], 2);
    r.step(m, vec![]);
    centre(&r, "iteration two", 25.);
    r.close();
}

pub fn run_result_view() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(
        r#"(workspace result_view
    (graph g :context sop
      (let* [a (sop/box) b (sop/transform a :translate [5 0 0])
             result (sop/transform b :translate [15 0 0])]
        result))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :eye [0 0 6] :follow_viewport true))))"#,
    ));
    let m = (450., 300.);
    let centre = |r: &Run, expected: f64| {
        let actual = centre_x(&r.e);
        check(actual == Some(expected), &format!("VIEW kept stale geometry: expected {expected:.1}, got {actual:?}; cook error: {:?}", r.e.core.cook.error));
    };
    let select = |r: &mut Run, name: &str| {
        let (x, y, w, _) = editor::node_box(&r.e, &["g".into(), name.into()]).expect("box");
        // a different point each time: two presses on one are a double-click, which renames
        click_at(r, ((x + w * (20 + 35 * (r.count % 4)) / 196) as f64, (y + 3) as f64));
        check(r.dump("scope selected") == format!("g/{name}"), "the result node could not be selected");
    };
    let edit = |r: &mut Run, op: Op| {
        r.e = match editor::edit(r.e.clone(), &op) {
            Ok(e) => e,
            Err(m) => fail(&m),
        };
        r.step(m, vec![]);
    };
    let parsed = |text: &str| match flow_syntax::syntax::parse(text) {
        Ok(v) if v.len() == 1 => v[0].clone(),
        _ => fail("bad result-view expression"),
    };
    r.step(m, vec![]);
    r.step(m, vec![]);
    centre(&r, 20.);
    let (gx, gy, _, _) = r.panes().graph;
    click_at(&mut r, ((gx + 40) as f64, (gy + 40) as f64));
    r.step(m, vec![key(Key::Home)]);
    r.step(m, vec![key(Key::KeyChar('i'))]);
    r.step(m, vec![]);
    select(&mut r, "b");
    r.step(m, vec![key(Key::KeyChar('v'))]);
    r.step(m, vec![]);
    centre(&r, 5.);
    select(&mut r, "result");
    r.step(m, vec![key(Key::KeyChar('v'))]);
    r.step(m, vec![]);
    centre(&r, 20.);
    edit(&mut r, Op::SetArg { node: vec!["g".into(), "b".into()], key: ArgKey::Kw("translate".into()), sub: vec![], value: parsed("[9 0 0]") });
    centre(&r, 24.);
    edit(&mut r, Op::SetArg { node: vec!["g".into(), "result".into()], key: ArgKey::Kw("translate".into()), sub: vec![], value: parsed("[30 0 0]") });
    centre(&r, 39.);
    select(&mut r, "b");
    r.step(m, vec![key(Key::KeyChar('v'))]);
    r.step(m, vec![]);
    centre(&r, 9.);
    edit(&mut r, Op::SetArg { node: vec!["g".into(), "b".into()], key: ArgKey::Whole, sub: vec![], value: parsed("(sop/transform (sop/transform a :translate [2 0 0]) :translate [9 0 0])") });
    centre(&r, 11.);
    let (vx, vy, _, _) = r.panes().view;
    click_at(&mut r, ((vx + 10) as f64, (vy + 10) as f64));
    r.step(m, vec![key(Key::KeyChar('f'))]);
    r.step(m, vec![]);
    check((rays::easy_camera::target(&editor::camera(&r.e)).x - 11.).abs() < 1e-6, "viewport F framed the graph result instead of the viewed node");
    let (gx, gy, _, _) = r.panes().graph;
    click_at(&mut r, ((gx + 10) as f64, (gy + 10) as f64));
    r.step(m, vec![key(Key::KeyChar('u'))]);
    r.step(m, vec![]);
    check(r.dump("level") == "scene", "u did not leave the geometry network");
    centre(&r, 11.);
    click_at(&mut r, ((gx + 40) as f64, (gy + 40) as f64));
    r.step(m, vec![key(Key::KeyChar('i'))]);
    r.step(m, vec![]);
    select(&mut r, "result");
    r.step(m, vec![key(Key::KeyChar('v'))]);
    r.step(m, vec![]);
    centre(&r, 41.);
    r.close();
}

pub fn run_geometry_view() {
    if !gpu() {
        return;
    }
    let mut r = Run::new(editor(
        r#"(workspace geometry_view
    (graph g :context sop
      (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0]))
             dots (for [p (sop/point_list field)]
               (let* [moved (sop/transform (sop/box) :translate p)]
                 (sop/transform moved :translate [0 2 0]))) ]
        (sop/merge dots)))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :eye [0 0 6] :follow_viewport true))))"#,
    ));
    let m = (450., 300.);
    let centre = |r: &Run, expected: f64| check(centre_x(&r.e) == Some(expected), "the geometry template did not preview the selected element");
    r.step(m, vec![]);
    r.step(m, vec![]);
    centre(&r, 10.);
    let (gx, gy, _, _) = r.panes().graph;
    click_at(&mut r, ((gx + 40) as f64, (gy + 40) as f64));
    r.step(m, vec![key(Key::Home)]);
    r.step(m, vec![key(Key::KeyChar('i'))]);
    r.step(m, vec![]);
    let (x, y, w, _) = editor::node_box(&r.e, &["g".into(), "dots".into(), "moved".into()]).expect("moved");
    click_at(&mut r, ((x + w / 2) as f64, (y + 3) as f64));
    let before = r.source();
    let label = r.undo_label();
    r.step(m, vec![key(Key::KeyChar('v'))]);
    r.step(m, vec![]);
    centre(&r, 0.);
    r.e = editor::set_probe(r.e.clone(), &["g".into(), "dots".into()], 2);
    r.step(m, vec![]);
    centre(&r, 20.);
    check(r.source() == before && r.undo_label() == label, "geometry preview and its selector changed the source or history");
    let (vx, vy, _, _) = r.panes().view;
    click_at(&mut r, ((vx + 10) as f64, (vy + 10) as f64));
    r.step(m, vec![key(Key::KeyChar('f'))]);
    r.step(m, vec![]);
    check((rays::easy_camera::target(&editor::camera(&r.e)).x - 20.).abs() < 1e-6, "viewport F did not frame the geometry preview");
    let panes = r.panes();
    let point = rays::camera::world_to_screen(panes.view, &editor::view_camera(&r.e), rays_math::vec3::create(20., 0., 0.)).expect("projection").expect("on screen");
    click_at(&mut r, (point.x, point.y));
    r.step(m, vec![]);
    check(r.dump("scope selected") == "g/dots/moved" && editor::probe(&r.e, &["g".into(), "dots".into()]) == Some(2), "picking a preview selected the authored result or changed its iteration");
    r.close();
}

pub fn run_movers() {
    if !gpu() {
        return;
    }
    let text = "(workspace w (graph g :context sop (let* [widths (list 1 2 3) r (for [i (range 2)] (sop/box :size [(nth widths i) 1 1]))] (sop/merge r))))";
    let mut r = Run::new(editor(text));
    let m = (450., 300.);
    r.step(m, vec![]);
    r.step(m, vec![]);
    let (gx, gy, _, _) = r.panes().graph;
    let p = ((gx + 40) as f64, (gy + 40) as f64);
    r.step(p, vec![Event::Pointer_moved(p)]);
    r.step(p, vec![Event::Pointer_pressed(L, p)]);
    r.step(p, vec![Event::Pointer_released(L, p)]);
    r.step(p, vec![key(Key::Home)]);
    r.step(p, vec![key(Key::KeyChar('i'))]);
    r.step(p, vec![]);
    let (bx, by, bw, _) = editor::node_box(&r.e, &["g".into(), "widths".into()]).expect("widths");
    let c = ((bx + bw / 2) as f64, (by + 8) as f64);
    r.step(c, vec![Event::Pointer_moved(c)]);
    r.step(c, vec![Event::Pointer_pressed(L, c)]);
    r.step(c, vec![Event::Pointer_released(L, c)]);
    r.step(c, vec![]);
    let (ix, _, iw, _) = r.panes().inspector;
    let q = ((ix + iw / 2) as f64, 210.);
    r.step(q, vec![Event::Pointer_moved(q)]);
    r.step(q, vec![Event::Pointer_pressed(L, q)]);
    r.step(q, vec![Event::Pointer_released(L, q)]);
    r.step(q, vec![]);
    let label = r.undo_label();
    let moved = has(&r.source(), "(list 1 3 2") || has(&r.source(), "(list 2 1 3");
    r.close();
    check(label == Some("Move item".into()) && moved, "the inspector's Move item button did not move a list item up");
}
