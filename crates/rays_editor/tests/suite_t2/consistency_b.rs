//! test_editor_consistency.ml, second half: viewport interaction, renderers, live-light failure,
//! camera menus and the compiled probe panels.
use crate::common;
use super::consistency::*;
use super::support::*;
use editor_core::panels::Panel;
use editor_document::workspace_doc as wd;
use flow_graph::flow_edit::Op;
use input::{Event, Key, MouseButton};
use pxui_shell::layout;
use rays_editor::{editor, editor_renderer};

pub const CASES: &[(&str, fn())] = &[
    ("consistency_root_viewport", root_viewport),
    ("consistency_overlapping_renderers", overlapping_renderers),
    ("consistency_no_viewport_click", no_viewport_click),
    ("consistency_hidden_preview", hidden_preview),
    ("consistency_focused_framing", focused_framing),
    ("consistency_preview_orbits", preview_orbits),
    ("consistency_preview_camera_world", preview_camera_world),
    ("consistency_live_light_failure", live_light_failure),
    ("consistency_live_light_determinism", live_light_determinism),
    ("consistency_live_light_failure_isolation", live_light_failure_isolation),
    ("consistency_lisp_apply", lisp_apply),
    ("consistency_enter_camera", enter_camera),
    ("consistency_named_inspector", named_inspector),
    ("consistency_stale_inspector_draft", stale_inspector_draft),
    ("consistency_named_handles", named_handles),
];

pub const SKIPPED: &[(&str, &str)] = &[
    ("consistency_render_mode_click", "needs sketches/gpu_sea.rays lowering: slow lowering: perf3"),
    ("consistency_run_native", "Sketch.run_state window runs (native render_mode_click, failed_renderer_modes): no Rust Sketch loop in the suite"),
    ("consistency_compiled_probe_panels", "slow lowering: perf3 (10000-element folds with exact precision; the allocation budget uses Gc.allocated_bytes, which has no Rust counterpart)"),
];

fn center(e: &Ed, key: &str) -> (f64, f64) {
    let doc = build(&editor::workspace(e));
    let g = layout::geometry(None, Some(&[Panel::Timeline]), &shell(&doc).tree, &frame(1));
    let (x, y, w, h) = layout::find(&g, &Panel::View(key.to_owned())).expect("view leaf").body;
    ((x + w / 2) as f64, (y + h / 2) as f64)
}

fn leaf_body(e: &Ed, panel: Panel) -> (i64, i64, i64, i64) {
    let doc = build(&editor::workspace(e));
    let g = layout::geometry(None, Some(&[Panel::Timeline]), &shell(&doc).tree, &frame(1));
    layout::find(&g, &panel).expect("leaf").body
}

fn click_at(e: &mut Ed, p: (f64, f64), n: i64) {
    *e = editor::update(e.clone(), &frame_at(p, click_events(p), n));
}

fn position(e: &Ed, key: &str) -> rays_math::vec3::T {
    rays::camera::position(&editor::viewport_camera(e, key))
}

fn same(a: rays_math::vec3::T, b: rays_math::vec3::T) -> bool {
    rays_math::vec3::nearly_equal(a, b, 1e-9)
}

fn root_viewport() {
    let ws = workspace(
        r#"(workspace rooted
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/root (scene/merge (scene/geometry (ref g)))
        :renderer "Wireframe" :width 800 :height 600 :max_spp 32)))"#,
    );
    with_editor(&ws, None, |e| {
        upd(e, |e| step(e, 2));
        assert!(editor::renderer(e) == editor_renderer::Mode::Wireframe, "the viewport did not take the root's renderer");
        let settings = editor::render_settings(e);
        assert!(settings.width == 800 && settings.height == 600 && settings.max_spp == 32, "the render settings are not the root's");
        upd(e, |e| step(editor::set_renderer(e, editor_renderer::Mode::Raster), 3));
        assert!(editor::renderer(e) == editor_renderer::Mode::Wireframe, "a viewport request overrode the root's renderer");
    });
    with_editor(&workspace(PLAIN), None, |e| {
        upd(e, |e| step(editor::set_renderer(e, editor_renderer::Mode::Wireframe), 2));
        assert!(editor::renderer(e) == editor_renderer::Mode::Wireframe, "without a root the viewport request stands");
    });
}

fn overlapping_renderers() {
    let ws = workspace(
        r#"(workspace overlap
    (graph g :context sop [(size : float 1)] (sop/box :size [size size size]))
    (graph scene :context scene [(size : float 1)] (scene/geometry (ref g :size size)))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene))
             one (ui/viewport (ref scene :size 2))
             two (ui/viewport (ref scene :size 3))]
        (ui/workspace (ui/tile main one two)))))
    (layout (panel ["editor" "one"] :window [150 120 320 280])
            (panel ["editor" "two"] :window [150 120 320 280]))"#,
    );
    with_editor(&ws, None, |e| {
        upd(e, |e| step(editor::set_renderer(e, editor_renderer::Mode::Wireframe), 2));
        let pictures = layers(e);
        assert_eq!(pictures.len(), 3, "an overlapping viewport did not render");
        assert!(pictures[1] != pictures[2], "coincident floating bounds made two renderers paint the same slot");
    });
}

fn no_viewport_click() {
    let ws = workspace(
        r#"
(workspace no_vp
  (graph g :context sop (sop/box))
  (graph editor :context editor
    (let* [o (ui/outline)
           g (ui/graph)
           i (ui/inspector)]
      (ui/workspace (ui/tile o g i)))))
"#,
    );
    with_editor(&ws, None, |e| {
        let mut f = frame(2);
        f.input.events = click_events((15., 15.));
        f.input.pointer = (15., 15.);
        upd(e, |e| editor::update(e, &f));
    });
}

fn hidden_preview() {
    with_editor(&workspace(PREVIEW_TEXT), None, |e| {
        // Focus the authored empty preview, then hide the UI. The fullscreen image remains empty.
        let p = center(e, "v:\"editor\"/\"blank\"");
        upd(e, |e| editor::update(e, &frame_at(p, click_events(p), 2)));
        upd(e, |e| editor::update(e, &frame_at(p, vec![Event::Key_pressed(Key::KeyChar('/')), Event::Key_pressed(Key::KeyChar('h'))], 3)));
        assert_eq!(layers(e).iter().map(|l| l.len()).collect::<Vec<_>>(), vec![0], "hiding the UI switched the focused preview to the primary scene");
    });
}

fn focused_framing() {
    with_editor(&workspace(PREVIEW_TEXT), None, |e| {
        let count = std::cell::Cell::new(1);
        let mut frame_view = |e: &mut Ed, key: &str| {
            let p = center(e, key);
            count.set(count.get() + 1);
            let c = count.get();
            upd(e, |e| editor::update(e, &frame_at(p, click_events(p), c)));
            let before = position(e, key);
            count.set(count.get() + 1);
            let c = count.get();
            upd(e, |e| editor::update(e, &frame_at(p, vec![Event::Key_pressed(Key::KeyChar('f'))], c)));
            (before, position(e, key))
        };
        let (before, after) = frame_view(e, "v:\"editor\"/\"main\"");
        assert!(!same(before, after), "viewport-only layout could not frame its populated scene");
        let (before, after) = frame_view(e, "v:\"editor\"/\"blank\"");
        assert!(same(before, after), "framing an empty focused viewport used another scene's bounds");
    });
}

fn preview_orbits() {
    let ws = workspace(PREVIEW_TEXT);
    let camera = rays::easy_camera::with_inertia(false, &rays::easy_camera::create(None, Some(7.), None, None, None, None).unwrap());
    with_editor(&ws, Some(camera), |e| {
        let count = std::cell::Cell::new(1);
        let update = |e: &mut Ed, buttons: &[MouseButton], delta: (f64, f64), point: (f64, f64), events: Vec<Event>| {
            count.set(count.get() + 1);
            let c = count.get();
            upd(e, |e| editor::update(e, &frame_full(buttons, &[], delta, point, events, c)));
        };
        let focus = |e: &mut Ed, update: &dyn Fn(&mut Ed, &[MouseButton], (f64, f64), (f64, f64), Vec<Event>), key: &str| {
            let point = center(e, key);
            update(e, &[], (0., 0.), point, vec![Event::Pointer_moved(point)]);
            update(e, &[], (0., 0.), point, click_events(point));
        };
        let main = "v:\"editor\"/\"main\"";
        let comparison = "v:\"editor\"/\"comparison\"";
        focus(e, &update, comparison);
        let initial = position(e, main);
        let (x, y) = center(e, comparison);
        let point = (x, y + 40.);
        update(e, &[MouseButton::LeftButton], (0., 0.), point, vec![Event::Pointer_pressed(MouseButton::LeftButton, point)]);
        for k in 1..=6 {
            let point = (x + k as f64 * 15., y + 40. + k as f64 * 2.);
            update(e, &[MouseButton::LeftButton], (15., 2.), point, vec![Event::Pointer_moved(point)]);
        }
        let point = (x + 90., y + 52.);
        update(e, &[], (0., 0.), point, vec![Event::Pointer_released(MouseButton::LeftButton, point)]);
        let orbited = position(e, comparison);
        assert!(!same(initial, orbited) && same(position(e, main), initial), "viewport orbit fixture did not produce isolated cameras");
        let preserved = |e: &Ed| assert!(same(position(e, main), initial) && same(position(e, comparison), orbited), "layout change transferred or reset named viewport orbits");
        let do_edit = |e: &mut Ed, op: &Op| {
            upd(e, |e| edit(e, op));
            update(e, &[], (0., 0.), (0., 0.), vec![]);
        };
        do_edit(e, &set_pos(&["editor", "@result"], "(ui/tile blank comparison main)"));
        preserved(e);
        upd(e, |e| editor::edit_layout(e, &editor_document::layout_edit::Op::DockPanel { node: s(&["editor", "comparison"]), target: s(&["editor", "main"]), side: editor_document::layout_edit::Side::Right }).unwrap());
        update(e, &[], (0., 0.), (0., 0.), vec![]);
        preserved(e);
        let shown = rays_editor::text_pane::make_shown(&editor::workspace(e).source, "editor", None, rays_editor::text_pane::Tab::Graph).unwrap();
        do_edit(e, &Op::SetGraph { name: "editor".into(), form: parse(&shown.text) });
        preserved(e);
        focus(e, &update, main);
        upd(e, |e| editor::edit_layout(e, &editor_document::layout_edit::Op::ClosePanel { node: s(&["editor", "comparison"]) }).unwrap());
        update(e, &[], (0., 0.), (0., 0.), vec![]);
        let original = rays_editor::text_pane::make_shown(&ws.source, "editor", None, rays_editor::text_pane::Tab::Graph).unwrap();
        do_edit(e, &Op::SetGraph { name: "editor".into(), form: parse(&original.text) });
        focus(e, &update, comparison);
        assert!(same(position(e, comparison), initial) && !same(position(e, comparison), orbited), "closing a preview left an orphan orbit that reappeared on re-add");
    });
}

fn preview_camera_world() {
    let ws = workspace(
        r#"(workspace cameras
    (graph g :context sop (sop/box))
    (graph scene :context scene [(eye : vec3 [0 0 6])]
      (scene/merge (scene/geometry (ref g))
        (scene/camera :name "camera" :eye eye :active true)
        (scene/world (ref world) :exposure 0.5)))
    (graph world :context world (world/sky))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene))
             comparison (ui/viewport (ref scene :eye [6 0 0]))]
        (ui/workspace (ui/tile main comparison)))))"#,
    );
    with_editor(&ws, None, |e| {
        let before = rays::camera::position(&editor::render_camera(e));
        let world = editor::world(e).expect("a world");
        assert!(rays_math::vec3::nearly_equal(before, rays_math::vec3::create(0., 0., 6.), 1e-6), "primary camera/World policy did not resolve authored values");
        let key = "v:\"editor\"/\"comparison\"";
        let doc = build(&editor::workspace(e));
        let ids = shell(&doc).views.iter().find(|(k, _)| k == key).unwrap().1.clone();
        let comparison = ids
            .iter()
            .find_map(|id| editor_document::kind_graph::find(&doc.scene, *id).and_then(|node| editor_document::objects::camera::of_node(&node).map(|(c, _)| c)))
            .expect("comparison camera");
        assert!(rays_math::vec3::nearly_equal(rays::camera::position(&comparison), rays_math::vec3::create(6., 0., 0.), 1e-6), "the comparison instance lost its independently authored camera");
        let point = center(e, key);
        upd(e, |e| editor::update(e, &frame_at(point, click_events(point), 2)));
        upd(e, |e| edit(e, &set_pos(&["editor", "comparison"], "(ref scene :eye [8 0 0])")));
        upd(e, |e| step(e, 3));
        let unchanged = |e: &Ed| rays::camera::position(&editor::render_camera(e)) == before && format!("{:?}", editor::world(e).unwrap()) == format!("{:?}", world);
        assert!(unchanged(e), "focusing/editing a comparison camera changed the global primary camera or World");
        upd(e, |e| editor::update(e, &undo_frame(point, 4)));
        assert!(unchanged(e), "undoing a comparison camera edit changed the primary camera or World");
    });
}

fn report(e: &Ed, dir: &str) -> String {
    editor::crash_dump(e, dir).expect("crash dump");
    std::fs::read_to_string(format!("{dir}/editor.txt")).expect("editor.txt")
}

fn live_light_failure() {
    let ws = workspace(
        r#"(workspace failure
    (graph g :context sop (sop/box))
    (graph scene :context scene (scene/merge (scene/geometry (ref g))
      (scene/light :intensity (pow 10 (* 400 (sin t)))))))"#,
    );
    with_editor(&ws, None, |e| {
        let source = editor::workspace(e);
        let good = layers(e);
        let mut live = frame(2);
        live.input.dt = 1.;
        upd(e, |e| editor::update(e, &live));
        assert!(same_ws(&editor::workspace(e), &source) && editor::undo_label(e).is_none(), "a failed live light expression changed authored state");
        assert!(layers(e) == good, "a failed live light expression replaced the last successful scene");
        with_dir(|dir| {
            let r = report(e, dir);
            assert!(r.contains("E_CONTEXT_LIVE") && r.contains("intensity"), "a live light failure lacked a field diagnostic");
            let mut recovered = frame(3);
            recovered.input.dt = 4.;
            upd(e, |e| editor::update(e, &recovered));
            assert!(layers(e) != good && !report(e, dir).contains("E_CONTEXT_LIVE"), "a recovered live light expression retained stale output or its diagnostic");
        });
    });
}

fn live_light_determinism() {
    if !common::gpu() {
        return;
    }
    let ws = workspace(
        r#"(workspace seeded
    (graph g :context sop (sop/noise_displace
      (sop/uv_sphere :segments 16 :rings 8) :seed 7 :amplitude 0.1 :frequency 1))
    (graph scene :context scene (scene/merge (scene/geometry (ref g))
      (scene/light :intensity (+ 1 t) :color [(+ 0.1 (* 0.1 t)) 0.2 0.3]))))"#,
    );
    let replay = |domains: i64| {
        with_dir(|presets| {
            let prepares = std::rc::Rc::new(std::cell::Cell::new(0));
            let drawings = std::rc::Rc::new(std::cell::Cell::new(0));
            let (p, d) = (prepares.clone(), drawings.clone());
            let options = editor::Create { platform: Some(input::Platform::Apple), ui_text_size: Some(11), presets: Some(presets.to_owned()), await_: Some(true), domains: Some(domains), seed: Some(42), ..Default::default() };
            let prepare: rays_editor::cook::Prepare_fn = std::rc::Rc::new(move |s, o| {
                p.set(p.get() + 1);
                rays_editor::cook::prepare(s, o)
            });
            let draw: editor::Draw_fn = std::rc::Rc::new(move |n, pr| {
                d.set(d.get() + 1);
                rays_editor::cook::draw(n, pr)
            });
            let mut e = editor::create(options, &ws, None, prepare, draw).expect("editor");
            let mut samples = Vec::new();
            for (i, dt) in [0., 1. / 60., 0.5, 2.].iter().enumerate() {
                let mut f = frame(i as i64 + 1);
                f.input.dt = *dt;
                e = editor::update(e, &f);
                let g = editor::prepared(&e).unwrap();
                let v = rdk_core::packed::float3::view::view(rdk_core::geometry::positions(&g)).unwrap();
                samples.push((format!("{:?}{:?}{:?}", v.x, v.y, v.z), format!("{:?}", editor::lights(&e))));
            }
            editor::close(&e);
            assert_eq!(prepares.get(), 1, "live lighting recooked or re-prepared unchanged seeded geometry");
            assert_eq!(drawings.get(), 1, "live lighting rebuilt unchanged geometry drawings");
            samples
        })
    };
    let sequential = replay(1);
    let parallel = replay(3);
    assert!(sequential == parallel, "live context playback changed seeded output between one and three domains");
    assert!(sequential[0].1 != sequential[3].1, "determinism fixture did not vary its live lighting");
}

fn live_light_failure_isolation() {
    let ws = workspace(
        r#"(workspace isolated
    (graph g :context sop (sop/box))
    (graph scene :context scene [(bad : bool false)]
      (scene/merge (scene/geometry (ref g))
        (scene/light :intensity (if bad (pow 10 (* 400 (sin t))) (+ 1 t)))))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene))
             comparison (ui/viewport (ref scene :bad true))]
        (ui/workspace (ui/tile main comparison)))))"#,
    );
    with_editor(&ws, None, |e| {
        let before = layers(e);
        let mut live = frame(2);
        live.input.dt = 1.;
        upd(e, |e| editor::update(e, &live));
        let failed = layers(e);
        assert!(failed[0] != before[0] && failed[1] == before[1], "one failed preview light froze a healthy sibling or discarded its own last output");
        with_dir(|dir| {
            let r = report(e, dir);
            assert!(r.contains("E_CONTEXT_LIVE") && r.contains("comparison"), "a failed preview light lacked viewport attribution");
        });
        let mut recovered = frame(3);
        recovered.input.dt = 4.;
        upd(e, |e| editor::update(e, &recovered));
        assert!(layers(e)[1] != failed[1], "a recovered preview light retained stale output");
    });
}

fn replace(text: &str, from: &str, by: &str) -> String {
    assert!(text.contains(from), "no {from}");
    text.replacen(from, by, 1)
}

/// Test_text_pane.tab_at
fn tab_at(right: i64, top: i64, index: usize) -> (f64, f64) {
    ((right - 178 + [31, 92, 150][index]) as f64, (top - 12) as f64)
}

fn lisp_apply() {
    let text = replace(SCENE_TEXT, "(ui/viewport (ref scene))", "(ui/split \"horizontal\" (ui/lisp) (ui/viewport (ref scene)))");
    let ws = workspace_with(Some(settings_of(3., "kept")), &format!("{text}\n(layout (node [\"g\"] :at [24 48]))"));
    with_editor(&ws, None, |e| {
        let (x, y, w, h) = {
            let doc = build(&ws);
            let g = layout::geometry(None, Some(&[Panel::Timeline]), &shell(&doc).tree, &frame(1));
            layout::find(&g, &Panel::Lisp).expect("lisp leaf").body
        };
        let count = std::cell::Cell::new(1);
        let run = |e: &mut Ed, mouse: (f64, f64), keys: &[Key], events: Vec<Event>| {
            count.set(count.get() + 1);
            let c = count.get();
            upd(e, |e| editor::update(e, &frame_full(&[], keys, (0., 0.), mouse, events, c)));
        };
        let click = |e: &mut Ed, run: &dyn Fn(&mut Ed, (f64, f64), &[Key], Vec<Event>), p: (f64, f64)| {
            run(e, p, &[], vec![Event::Pointer_moved(p)]);
            run(e, p, &[], click_events(p));
        };
        click(e, &run, tab_at(x + w - 32, y, 2));
        click(e, &run, ((x + 180) as f64, (y + 80) as f64));
        run(e, (450., 300.), &[Key::Meta], vec![Event::Key_pressed(Key::KeyChar('a'))]);
        run(e, (450., 300.), &[], vec![Event::Text_input(replace(&wd::to_text(&ws).unwrap(), "(sop/box)", "(sop/box :size [2 2 2])"))]);
        let before = format!("{:?}", editor::prepared(e).map(|g| {
            let v = rdk_core::packed::float3::view::view(rdk_core::geometry::positions(&g)).unwrap();
            (v.x.clone(), v.y.clone(), v.z.clone())
        }));
        let picture = layers(e);
        click(e, &run, ((x + 32) as f64, (y + h - 40) as f64));
        assert_eq!(editor::undo_label(e).as_deref(), Some("Edit text"), "Lisp apply made no document edit");
        let after = format!("{:?}", editor::prepared(e).map(|g| {
            let v = rdk_core::packed::float3::view::view(rdk_core::geometry::positions(&g)).unwrap();
            (v.x.clone(), v.y.clone(), v.z.clone())
        }));
        assert!(after != before && layers(e) != picture, "Lisp applied after the frame's cook or composition");
        assert!(editor::workspace(e).layout.at == ws.layout.at && got(&editor::workspace(e).settings) == vec![param::Value::Float(3.), param::Value::Text(b"kept".as_slice().into())], "Lisp apply lost layout or sketch settings");
    });
}

fn enter_camera() {
    let ws = workspace(
        r#"(workspace cameras
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :name "near" :eye [2 3 6])
        (scene/camera :name "telephoto" :eye [13 8.8 14.8] :fov 20))))"#,
    );
    with_editor(&ws, None, |e| {
        let count = std::cell::Cell::new(1);
        let step_ = |e: &mut Ed, mouse: (f64, f64), events: Vec<Event>| {
            count.set(count.get() + 1);
            let c = count.get();
            upd(e, |e| editor::update(e, &frame_at(mouse, events, c)));
        };
        let click = |button: MouseButton, p: (f64, f64)| vec![Event::Pointer_pressed(button, p), Event::Pointer_released(button, p)];
        let (gx, gy, _, _) = editor::panes(e, &frame(0)).graph;
        let in_list = ((gx + 60) as f64, (gy + 350) as f64);
        step_(e, in_list, click(MouseButton::LeftButton, in_list));
        step_(e, (450., 300.), vec![Event::Key_pressed(Key::Home)]);
        let mut index = 0;
        loop {
            if editor::selected_node(e).map(|p| p.label) == Some("telephoto".to_owned()) {
                break;
            }
            assert!(index < 4, "no camera row telephoto");
            step_(e, (450., 300.), vec![Event::Key_pressed(Key::ArrowDown)]);
            index += 1;
        }
        let row = index;
        step_(e, (450., 300.), vec![Event::Key_pressed(Key::KeyChar('i'))]);
        let v = |e: &Ed| format!("{:?}", editor::view_camera(e)) == format!("{:?}", editor::render_camera(e));
        assert!(editor::look_through(e) && v(e) && rays::camera::position(&editor::render_camera(e)) == rays_math::vec3::create(13., 8.8, 14.8), "camera Enter did not choose and look through the selected camera");
        step_(e, (450., 300.), vec![Event::Key_pressed(Key::KeyChar('i'))]);
        assert!(editor::look_through(e), "repeated camera Enter disabled look-through");
        let in_view = (100., 300.);
        step_(e, in_view, click(MouseButton::LeftButton, in_view));
        step_(e, in_view, vec![Event::Key_pressed(Key::KeyChar('/')), Event::Key_pressed(Key::KeyChar('v'))]);
        assert!(!editor::look_through(e), "camera fixture did not leave look-through");
        let at = ((gx + 60) as f64, (gy + 24 + row * 24 + 12) as f64);
        step_(e, at, click(MouseButton::RightButton, at));
        step_(e, at, vec![]);
        let item = (at.0 + 20., at.1 + 12.);
        step_(e, item, click(MouseButton::LeftButton, item));
        assert!(editor::look_through(e) && v(e), "right-click Enter on the camera did not enable look-through");
    });
}

fn with_factory_editor(ws_text: &str, f: impl FnOnce(&mut Ed, &wd::T, &[sop::edit_graph::Factory])) {
    if !common::gpu() {
        return;
    }
    let mut factories = vec![test_factory()];
    factories.extend(sop::nodes::factories());
    let ws = workspace_factories(&factories, ws_text);
    with_dir(|presets| {
        let options = editor::Create { platform: Some(input::Platform::Apple), ui_text_size: Some(11), presets: Some(presets.to_owned()), await_: Some(true), domains: Some(1), factories: Some(factories.clone()), ..Default::default() };
        let mut e = editor::create(options, &ws, None, editor::prepare_fn(), editor::draw_fn()).expect("editor");
        e = step(e, 1);
        let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&mut e, &ws, &factories)));
        editor::close(&e);
        if let Err(p) = r {
            std::panic::resume_unwind(p);
        }
    });
}

fn lower_with(factories: &[sop::edit_graph::Factory], ws: &wd::T) -> editor_document::document::T {
    editor_document::lowering::of_workspace(&editor_document::lowering::Options { factories: Some(factories.to_vec()), ..Default::default() }, ws).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn inspector_drag_from(body: (i64, i64, i64, i64)) -> ((f64, f64), (f64, f64)) {
    let (ix, iy, iw, _) = body;
    let from = (ix as f64 + 96f64.max(150f64.min(iw as f64 * 0.34)) + 10., (iy + 190) as f64);
    (from, (from.0 + 50., from.1))
}

fn named_inspector() {
    with_factory_editor(
        r#"(workspace named
    (graph g :context sop (let* [a (sop/test_source :value 1)] a))
    (graph editor :context editor
      (ui/workspace (ui/split-at "horizontal" 0.6 (ui/graph "g") (ui/inspector)))))"#,
        |e, _ws, factories| {
            let (x, y, w, _) = editor::node_box(e, &s(&["g", "a"])).expect("node box");
            let p = ((x + w / 2) as f64, (y + 10) as f64);
            upd(e, |e| editor::update(e, &frame_at(p, click_events(p), 2)));
            upd(e, |e| step(e, 3));
            assert!(editor::level(e).is_none(), "fixture left the scene level");
            let doc = lower_with(factories, &editor::workspace(e));
            let g = layout::geometry(None, Some(&[Panel::Timeline]), &shell(&doc).tree, &frame(1));
            let (from, to) = inspector_drag_from(layout::find(&g, &Panel::Inspector).expect("inspector").body);
            upd(e, |e| editor::update(e, &frame_full(&[MouseButton::LeftButton], &[], (0., 0.), from, vec![Event::Pointer_pressed(MouseButton::LeftButton, from)], 4)));
            upd(e, |e| editor::update(e, &frame_full(&[MouseButton::LeftButton], &[], (0., 0.), to, vec![Event::Pointer_moved(to)], 5)));
            upd(e, |e| editor::update(e, &frame_at(to, vec![Event::Pointer_released(MouseButton::LeftButton, to)], 6)));
            assert_eq!(editor::undo_label(e).as_deref(), Some("Edit value"), "a named SOP pane at the scene level had no editable inspector fields");
        },
    );
}

fn stale_inspector_draft() {
    with_factory_editor(
        r#"(workspace drafts
    (graph g :context sop (let* [a (sop/test_source :value 1)] a))
    (graph editor :context editor (ui/workspace
      (ui/split-at "horizontal" 0.6
        (ui/split-at "vertical" 0.5 (ui/graph "g") (ui/lisp)) (ui/inspector)))))"#,
        |e, _ws, factories| {
            let count = std::cell::Cell::new(0);
            let update = |e: &mut Ed, keys: &[Key], buttons: &[MouseButton], p: (f64, f64), events: Vec<Event>| {
                count.set(count.get() + 1);
                let c = count.get();
                upd(e, |e| editor::update(e, &frame_full(buttons, keys, (0., 0.), p, events, c)));
            };
            let (x, y, w, _) = editor::node_box(e, &s(&["g", "a"])).expect("node box");
            let p = ((x + w / 2) as f64, (y + 10) as f64);
            update(e, &[], &[], p, click_events(p));
            let doc = lower_with(factories, &editor::workspace(e));
            let panes = layout::geometry(None, Some(&[Panel::Timeline]), &shell(&doc).tree, &frame(1));
            let (lx, ly, _, lh) = layout::find(&panes, &Panel::Lisp).expect("lisp").body;
            let area = ((lx + 150) as f64, (ly + 70) as f64);
            update(e, &[], &[], area, click_events(area));
            update(e, &[Key::Meta], &[], area, vec![Event::Key_pressed(Key::KeyChar('a'))]);
            update(e, &[], &[], area, vec![Event::Text_input("(let* [a (sop/test_source :value 2)] a)".into())]);
            let (from, to) = inspector_drag_from(layout::find(&panes, &Panel::Inspector).expect("inspector").body);
            let before = editor::workspace(e);
            update(e, &[], &[MouseButton::LeftButton], from, vec![Event::Pointer_pressed(MouseButton::LeftButton, from)]);
            update(e, &[], &[MouseButton::LeftButton], to, vec![Event::Pointer_moved(to)]);
            update(e, &[], &[], to, vec![Event::Pointer_released(MouseButton::LeftButton, to)]);
            let edited = editor::workspace(e);
            let label = editor::undo_label(e);
            assert!(!same_ws(&edited, &before) && label.as_deref() == Some("Edit value"), "fixture inspector did not change the source");
            let apply = ((lx + 60) as f64, (ly + lh - 36) as f64);
            update(e, &[], &[], apply, click_events(apply));
            assert!(same_ws(&editor::workspace(e), &edited) && editor::undo_label(e) == label, "an old Selection draft overwrote an inspector edit");
            with_dir(|dir| {
                let r = report(e, dir);
                assert!(r.contains("draft yes") && r.contains("E_DRAFT_CONFLICT"), "inspector conflict did not retain the draft and explain the refusal");
            });
        },
    );
}

fn named_handles() {
    let ws = workspace(
        r#"(workspace handles
    (graph g :context sop (let* [a (sop/box :center [0 0 0])] a))
    (graph scene :context scene (scene/geometry (ref g) :translate [1 0 0]))
    (graph editor :context editor (ui/workspace
      (ui/split-at "horizontal" 0.5 (ui/viewport (ref scene)) (ui/graph "g")))))"#,
    );
    let camera = rays::easy_camera::with_inertia(false, &rays::easy_camera::create(None, Some(7.), None, None, None, None).unwrap());
    with_editor(&ws, Some(camera), |e| {
        let count = std::cell::Cell::new(1);
        let update = |e: &mut Ed, buttons: &[MouseButton], delta: (f64, f64), p: (f64, f64), events: Vec<Event>| {
            count.set(count.get() + 1);
            let c = count.get();
            upd(e, |e| editor::update(e, &frame_full(buttons, &[], delta, p, events, c)));
        };
        let (x, y, width, _) = editor::node_box(e, &s(&["g", "a"])).expect("node box");
        let p = ((x + width / 2) as f64, (y + 10) as f64);
        update(e, &[], (0., 0.), p, click_events(p));
        update(e, &[], (0., 0.), p, vec![]);
        assert!(editor::level(e).is_none() && editor::selected_node(e).is_some(), "named handle selection did not resolve a SOP node independently of scene navigation");
        let bounds = editor::panes(e, &frame(count.get())).view;
        let camera = editor::viewport_camera(e, "v0");
        let screen = |x: f64| rays::camera::world_to_screen(bounds, &camera, rays_math::vec3::create(x, 0., 0.)).unwrap().expect("on screen");
        let base = screen(1.);
        let unit = screen(2.);
        let (dx, dy) = (unit.x - base.x, unit.y - base.y);
        let length = dx.hypot(dy);
        let tip = (base.x + dx / length * 60., base.y + dy / length * 60.);
        let target = (tip.0 + dx / length * 30., tip.1 + dy / length * 30.);
        let before = editor::workspace(e);
        update(e, &[MouseButton::LeftButton], (0., 0.), tip, vec![Event::Pointer_pressed(MouseButton::LeftButton, tip)]);
        update(e, &[MouseButton::LeftButton], (target.0 - tip.0, target.1 - tip.1), target, vec![Event::Pointer_moved(target)]);
        update(e, &[], (0., 0.), target, vec![Event::Pointer_released(MouseButton::LeftButton, target)]);
        let after = editor::workspace(e);
        let binding = |w: &wd::T| rays_editor::text_pane::binding(&w.source, &s(&["g", "a"])).map(|(_, v)| flat(&v));
        assert!(!same_ws(&after, &before) && binding(&after) != binding(&before), "named SOP handle did not write its owning graph argument");
        let r = flow_eval::run(&after.checked, false, None, None, None, 0.).unwrap();
        let scene = r.results.iter().find(|(n, _)| n == "scene").map(|(_, v)| format!("{v:?}")).unwrap();
        assert!(scene.contains("1.0, 0.0, 0.0") || scene.contains("Vec3(1.0, 0.0, 0.0)") || scene.contains("[1.0, 0.0, 0.0]"), "SOP handle accidentally edited the scene object's placement: {scene}");
    });
}
