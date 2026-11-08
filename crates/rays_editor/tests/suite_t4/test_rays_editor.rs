//! test_rays_editor.ml: the workspace editor end to end (layout panes, camera nodes, sketch
//! settings, presets and look-through, the full workspace walk). `native_smoke` opens a Sketch
//! window and has no Rust counterpart here (skipped, see SKIPPED).
use super::t4::*;
use editor_document::workspace_doc;
use flow_graph::flow_edit as fe;
use input::{Event, Key, MouseButton::*};
use param::Value;
use rays::frame;
use rays_editor::editor;
use rays_math::vec3;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

type Rect = (i64, i64, i64, i64);

fn pointer((x, y): (i64, i64)) -> (f64, f64) {
    (x as f64, y as f64)
}
fn press(b: input::MouseButton, p: (i64, i64)) -> Event {
    Event::Pointer_pressed(b, pointer(p))
}
fn release(b: input::MouseButton, p: (i64, i64)) -> Event {
    Event::Pointer_released(b, pointer(p))
}
fn mv(p: (i64, i64)) -> Event {
    Event::Pointer_moved(pointer(p))
}

fn frame_ex(width: i64, height: i64, mouse: Option<(i64, i64)>, events: Vec<Event>, count: i64) -> frame::Frame {
    let (x, y) = mouse.unwrap_or((width / 2, height / 2));
    frame::Frame {
        input: input::Frame { dt: 1. / 60., frame: count, tick: count, size: (width, height), pointer: (x as f64, y as f64), events, ..input::at_time(count as f64 / 60.) },
        pixel_scale: (1., 1.),
        fps: 60.,
        mouse_delta: (0., 0.),
    }
}
fn fr(count: i64) -> frame::Frame {
    frame_ex(900, 640, None, vec![], count)
}
fn fre(count: i64, events: Vec<Event>) -> frame::Frame {
    frame_ex(900, 640, None, events, count)
}
fn frm(count: i64, mouse: (i64, i64), events: Vec<Event>) -> frame::Frame {
    frame_ex(900, 640, Some(mouse), events, count)
}
fn width(r: Rect) -> i64 {
    r.2
}
fn card_header((x, y, w, _): Rect) -> (i64, i64) {
    (x + w / 2, y + 3)
}
fn card_row((x, y, w, _): Rect) -> (i64, i64) {
    (x + w * 20 / 196, y + w * 36 / 196)
}

/// The packed PXUI instances of a composed scene, for exact comparisons.
fn ui_bytes(scene: &[rays::scene::node]) -> Vec<u8> {
    let staged = rays::scene::Layer::stage_native_render(900, 640, scene).unwrap_or_else(|m| panic!("{m}"));
    let mut out = Vec::new();
    for layer in &staged.layers {
        if let rays::scene::Layer::native_layer::Ui_layer(batch, _) = layer {
            out.extend_from_slice(batch.instances());
        }
    }
    out
}

const TEXT: &str = r#"(workspace inspectable
  (graph geo1 :context sop
    (let* [source (sop/box :size [2 2 2])
           output (sop/transform source :translate [5 0 0])]
      output)))"#;

fn fixture() -> workspace_doc::T {
    of_text(None, TEXT)
}

fn report_line(ed: &editor::Editor, name: &str) -> String {
    let dir = temp_dir("report");
    editor::crash_dump(ed, &dir).unwrap();
    let all = format!("{}{}", std::fs::read_to_string(format!("{dir}/editor.txt")).unwrap(), std::fs::read_to_string(format!("{dir}/document.txt")).unwrap());
    let _ = std::fs::remove_dir_all(&dir);
    all.split('\n').find(|l| l.starts_with(&format!("{name}: ")) || l.starts_with(&format!("{name} "))).unwrap_or_else(|| panic!("no {name} line in the report")).to_string()
}

fn text_of(ed: &editor::Editor) -> String {
    workspace_doc::to_text(&editor::workspace(ed)).unwrap()
}
fn has(text: &str, piece: &str) -> bool {
    text.contains(piece)
}
fn cameras(ed: &editor::Editor) -> Vec<editor_document::kind_graph::Node> {
    editor::scene_document(ed).into_iter().filter(|n| n.kind == "camera").collect()
}
/// the id of the ACTIVE camera, from the crash report's "camera N" line
fn active(ed: &editor::Editor) -> Vec<i64> {
    let line = report_line(ed, "camera");
    let parts: Vec<&str> = line.split(' ').collect();
    match parts.as_slice() {
        [_, "-"] => vec![],
        [_, id] => vec![id.parse().unwrap()],
        _ => panic!("unreadable camera line"),
    }
}
fn near(a: vec3::T, b: vec3::T) -> bool {
    vec3::nearly_equal(a, b, 1e-6)
}
fn eye(ed: &editor::Editor) -> vec3::T {
    rays::camera::position(&editor::render_camera(ed))
}
fn viewport_eye(ed: &editor::Editor) -> vec3::T {
    rays::camera::position(&rays::easy_camera::camera(&editor::camera(ed)).unwrap())
}
fn up(ed: editor::Editor, f: frame::Frame) -> editor::Editor {
    if std::env::var("T4_TRACE").is_ok() {
        eprintln!("[up {}]", f.input.frame);
    }
    editor::update(ed, &f)
}
fn wait(count: i64, mut ed: editor::Editor) -> editor::Editor {
    for i in 0..2000 {
        ed = up(ed, fr(count + i));
        if editor::prepared(&ed).is_some() {
            return ed;
        }
    }
    panic!("sketch environment did not publish its initial async cook");
}
fn k(c: char) -> Event {
    key(Key::KeyChar(c))
}
fn with_keys(mut f: frame::Frame, keys: Vec<Key>) -> frame::Frame {
    f.input.keys = keys;
    f
}
fn with_buttons(mut f: frame::Frame, buttons: Vec<input::MouseButton>) -> frame::Frame {
    f.input.buttons = buttons;
    f
}
fn node_box(ed: &editor::Editor, path: &[&str]) -> Rect {
    let p: Vec<String> = path.iter().map(|s| s.to_string()).collect();
    editor::node_box(ed, &p).expect("the node has no box in the graph pane")
}

fn options(distance: Option<f64>) -> editor::Create {
    editor::Create {
        ui_text_size: Some(11),
        platform: Some(input::Platform::Apple),
        await_: Some(true),
        max_entries: Some(4),
        max_payload_bytes: Some(16 * 1024 * 1024),
        camera: distance.map(|d| rays::easy_camera::create(None, Some(d), None, None, None, Some(false)).unwrap()),
        ..Default::default()
    }
}

/// The scene opens as a list: a click on the geo1 row selects it and [i] enters its graph.
fn enter_geo1(ed: editor::Editor, count: i64) -> editor::Editor {
    let (gx, gy, _, _) = editor::panes(&ed, &fr(count)).graph;
    let point = (gx + 60, gy + 24 + 12);
    let ed = up(ed, frm(count + 1, point, vec![]));
    let ed = up(ed, frm(count + 2, point, vec![press(LeftButton, point), release(LeftButton, point)]));
    up(ed, fre(count + 3, vec![k('i')]))
}

pub fn layout_panes() {
    if !crate::common::gpu() {
        return;
    }
    use editor_core::panels;
    use pxui_shell::{chrome, layout as L};
    for (graph, _) in pxui_graph::scope_commands::bindings() {
        assert!(
            rays_editor::editor_commands::keymap().iter().any(|c| c.id == graph.id && c.trigger == graph.trigger && true),
            "graph command missing from the host keymap"
        );
    }
    for (graph, action) in pxui_graph::scope_commands::bindings() {
        assert!(
            rays_editor::editor_commands::keymap().iter().any(|c| c.id == graph.id && c.trigger == graph.trigger && *rays_editor::editor_commands::action_of(c) == rays_editor::editor_commands::action::Scope_command(action.clone())),
            "graph command missing from the host keymap"
        );
    }
    let camera_binding = |key: &str, action: rays_editor::editor_commands::action| rays_editor::editor_commands::keymap().iter().any(|c| c.trigger == Some(editor_core::keymap::Trigger::Leader(key.into())) && *rays_editor::editor_commands::action_of(c) == action);
    assert!(camera_binding("h", rays_editor::editor_commands::action::Hide_ui) && camera_binding("c", rays_editor::editor_commands::action::Open_camera), "camera visibility commands missing from the host keymap");
    let workspace = L::default();
    let panes = |hidden: Option<&[panels::Panel]>, tree: &L::Panels, f: &frame::Frame| L::panes(&L::geometry(None, hidden, tree, f));
    let initial = panes(None, &workspace, &frame_ex(1000, 640, None, vec![], 0));
    let header = L::find(&L::geometry(None, None, &workspace, &frame_ex(1000, 640, None, vec![], 0)), &panels::main()).unwrap().header;
    // a header is one 24-point row under a 4-point margin
    assert!(header == (0, 4, width(initial.view), 24), "workspace header is not a compact single line");
    assert!((width(initial.view) - 449).abs() <= 1 && (width(initial.graph) - 349).abs() <= 1 && (width(initial.inspector) - 200).abs() <= 1, "workspace defaults are not 45/35/20 after splitter space");
    let splitter_x = width(initial.view) + 2;
    let mut ui = pxui::ui::Ui::create(input::Platform::Apple, Some(11), None);
    let mut step = |f: &frame::Frame| {
        let mut out = Vec::new();
        ui.frame(f, &mut |ui| {
            out = chrome::update(None, None, None, None, None, None, &workspace, ui, f);
            out.extend(chrome::splitters(None, None, &workspace, ui, f));
        });
        out
    };
    step(&frame_ex(1000, 640, None, vec![], 0));
    let dragged = step(&frame_ex(1000, 640, None, vec![press(LeftButton, (splitter_x, 200)), mv((splitter_x + 80, 200)), release(LeftButton, (splitter_x + 80, 200))], 1));
    let mut resized = workspace.clone();
    for intent in &dragged {
        if let chrome::Intent::Resize { node, size } = intent {
            resized = panels::set_size(node, size.clone(), resized);
        }
    }
    let resized_panes = panes(None, &resized, &frame_ex(1000, 640, None, vec![], 2));
    assert!(dragged.contains(&chrome::Intent::Settled) && resized != workspace && width(resized_panes.view) > width(initial.view) && width(resized_panes.graph) < width(initial.graph), "workspace splitter did not resize its adjacent columns");
    let wider = panes(None, &resized, &frame_ex(1200, 640, None, vec![], 3));
    assert!(width(wider.view) > width(resized_panes.view), "workspace splitter ratio did not survive a window resize");
    ui.destroy();
    let collapsed = panes(Some(&[panels::Panel::Timeline, panels::Panel::Inspector]), &resized, &fr(5));
    assert!(width(collapsed.inspector) == 0, "inspector toggle did not collapse the third column");
}

pub fn camera_nodes() {
    if !crate::common::gpu() {
        return;
    }
    // Camera nodes: a default camera following the viewport joins a document without one, exactly
    // one is ACTIVE, deleting the last re-adds it inside the same undo entry, and a viewport drag is
    // one coalesced undo entry.
    let ed = editor::create(options(Some(6.)), &fixture(), None, editor::prepare_fn(), editor::draw_fn()).expect("opens");
    assert!(cameras(&ed).len() == 1 && active(&ed).len() == 1, "Editor did not add one ACTIVE default camera");
    // Cameras are scene objects: rows of the scene list.
    let mut ed = ed;
    for c in 0..3 {
        ed = up(ed, fr(c));
    }
    assert!(!editor::can_undo(&ed) && near(eye(&ed), viewport_eye(&ed)), "an idle following camera wrote undo entries or drifted from the viewport");
    // The default camera object is a pinhole; the view camera is the viewport with that lens while
    // nothing looks through.
    let pin = rays::camera::pinhole().aperture;
    assert!(rays::camera::lens(&editor::render_camera(&ed)).aperture == pin && rays::camera::lens(&editor::view_camera(&ed)).aperture == pin && near(rays::camera::position(&editor::view_camera(&ed)), viewport_eye(&ed)), "the default camera object did not carry its lens into the view camera");
    let before_idle = editor::view_camera(&ed);
    let idle = up(ed.clone(), fr(3));
    assert!(editor::view_camera(&idle) == before_idle, "an idle frame rebuilt the view camera (a sketch tracer would restart each frame)");
    let start_eye = eye(&ed);
    let (vx, vy, vw, vh) = editor::panes(&ed, &fr(3)).view;
    let (px, py) = (vx + vw / 2, vy + vh / 2);
    for (count, ev) in [(4, press(LeftButton, (px, py))), (5, mv((px + 30, py))), (6, mv((px + 60, py + 10))), (7, mv((px + 90, py + 20)))] {
        ed = up(ed, with_buttons(fre(count, vec![ev]), vec![LeftButton]));
    }
    ed = up(ed, fre(8, vec![release(LeftButton, (px + 90, py + 20))]));
    assert!(!near(eye(&ed), start_eye) && near(eye(&ed), viewport_eye(&ed)), "the following camera did not track a viewport orbit");
    let command = |c: char, count: i64| with_keys(fre(count, vec![k(c)]), vec![Key::Meta]);
    // navigation is view state (E12): the orbit leaves no undo entry, and an undo with nothing to
    // undo leaves the view where it is
    assert!(!editor::can_undo(&ed), "a viewport orbit made an undo entry");
    let orbited = eye(&ed);
    ed = up(ed, command('z', 9));
    ed = up(ed, fr(10));
    assert!(near(eye(&ed), orbited) && near(viewport_eye(&ed), orbited) && !editor::can_undo(&ed), "an undo with nothing to undo moved the viewport");
    ed = up(ed, fre(10, vec![key(Key::Home)]));
    ed = up(ed, fr(10));
    let camera_id = active(&ed)[0];
    // the camera is the second row of the scene list (geo1 is the first)
    let (gx, gy, _, _) = editor::panes(&ed, &fr(10)).graph;
    let at = (gx + 60, gy + 24 + 24 + 12);
    ed = up(ed, frm(11, at, vec![]));
    ed = up(ed, frm(12, at, vec![press(LeftButton, at), release(LeftButton, at)]));
    ed = up(ed, fre(16, vec![key(Key::Delete)]));
    // the host's camera is an object like any: deleting the last one is written (a scene graph that
    // declares no camera), not re-seeded, and one undo gives it back
    assert!(cameras(&ed).is_empty() && active(&ed).is_empty() && has(&text_of(&ed), "graph scene") && !has(&text_of(&ed), "scene/camera"), "deleting the last camera was not written as no camera");
    ed = up(ed, command('z', 17));
    assert!(cameras(&ed).iter().map(|n| n.id).collect::<Vec<_>>() == vec![camera_id], "undo after deleting the camera did not restore the original in one step");
    // Fly: / w (view focused) flies, held W moves forward, Escape exits; Space exits and arms the leader in the same frame.
    let fly_view = |ed: &editor::Editor| {
        let (vx, vy, _, _) = editor::panes(ed, &fr(18)).view;
        (vx + 20, vy + 20)
    };
    let p = fly_view(&ed);
    ed = up(ed, fre(18, vec![press(RightButton, p), release(RightButton, p)]));
    ed = up(ed, fre(18, vec![k('/'), k('w')]));
    assert!(editor::flying(&ed), "/ w did not enter fly mode");
    let before = viewport_eye(&ed);
    ed = up(ed, with_keys(fre(18, vec![k('w')]), vec![Key::KeyChar('w')]));
    assert!(!near(viewport_eye(&ed), before) && editor::selected_node(&ed).is_none(), "held W did not fly the viewport");
    ed = up(ed, fre(18, vec![key(Key::Escape)]));
    assert!(!editor::flying(&ed), "Escape did not exit fly mode");
    ed = up(ed, fre(18, vec![k('/'), k('w')]));
    ed = up(ed, fre(18, vec![k('/')]));
    ed = up(ed, fre(18, vec![k('g')]));
    assert!(!editor::flying(&ed) && width(editor::panes(&ed, &fr(18)).graph) == 0, "Space did not exit fly mode into the leader");
    ed = up(ed, fre(18, vec![k('/'), k('g')]));
    // F frames the displayed geometry in the viewport even when another node is selected.
    ed = enter_geo1(ed, 400);
    ed = up(ed, fr(404));
    let source_at = card_header(node_box(&ed, &["geo1", "source"]));
    ed = up(ed, frm(405, source_at, vec![]));
    ed = up(ed, frm(406, source_at, vec![press(LeftButton, source_at), release(LeftButton, source_at)]));
    let target = |ed: &editor::Editor| rays::easy_camera::target(&editor::camera(ed));
    let view_at = fly_view(&ed);
    ed = up(ed, fre(407, vec![press(LeftButton, view_at), release(LeftButton, view_at)]));
    ed = up(ed, fre(408, vec![k('f')]));
    // Framing waits for bounds when the display cook is still running.
    let mut count = 409;
    while !near(target(&ed), vec3::create(5., 0., 0.)) && count < 409 + 2000 {
        ed = up(ed, fr(count));
        count += 1;
    }
    assert!(near(target(&ed), vec3::create(5., 0., 0.)), "viewport-focused F did not focus on the displayed node's cached bounds");
    editor::close(&ed);
}

pub fn sketch_settings() {
    if !crate::common::gpu() {
        return;
    }
    // Sketch settings live in the document: [prepare] reads them, a change recooks, and undo
    // restores the previous value (voxel_wall's renderer).
    let kind = param::integer(0, 3, None, None).unwrap();
    let field = param::field("mode".into(), kind, Value::Int(0)).unwrap();
    let schema = Arc::new(param::schema("mode".into(), vec![field]).unwrap());
    let seen = Arc::new(AtomicI64::new(-1));
    let (s2, seen2) = (schema.clone(), seen.clone());
    let prepare: rays_editor::cook::Prepare_fn = std::rc::Rc::new(move |settings, output| {
        if let Ok(Value::Int(v)) = Editor_settings_get(&s2, settings) {
            seen2.store(v, Ordering::SeqCst);
        }
        editor::prepare(settings, output)
    });
    let mut o = options(None);
    o.settings = Some(rays_editor::Settings::make(&schema, vec![Value::Int(0)]));
    let mut ed = editor::create(o, &fixture(), None, prepare, editor::draw_fn()).expect("opens");
    let wait_mode = |expected: i64, start: i64, mut ed: editor::Editor| -> editor::Editor {
        for i in 0..2000 {
            ed = up(ed, fr(start + i));
            if seen.load(Ordering::SeqCst) == expected && editor::prepared(&ed).is_some() {
                return ed;
            }
        }
        panic!("a settings change did not re-run prepare");
    };
    ed = wait_mode(0, 0, ed);
    let set = editor::set_settings(ed, &rays_editor::Settings::make(&schema, vec![Value::Int(1)]));
    ed = wait_mode(1, 100, set);
    let undo = with_keys(fre(199, vec![k('z')]), vec![Key::Meta]);
    let after = up(ed, undo);
    ed = wait_mode(0, 200, after);
    assert!(matches!(Editor_settings_get(&schema, &editor::settings(&ed)), Ok(Value::Int(0))), "undo did not restore the sketch settings");
    editor::close(&ed);
}

#[allow(non_snake_case)]
fn Editor_settings_get(schema: &param::Schema, settings: &rays_editor::Settings::T) -> Result<Value, String> {
    rays_editor::Settings::get(schema, settings).map(|v| v[0].clone())
}

pub fn preset_and_look_through() {
    if !crate::common::gpu() {
        return;
    }
    // Every document is a workspace, so every document saves: the prompt owns the keyboard while it
    // is open, Enter writes the workspace text, and / b loads it back.
    use editor_document::preset;
    let presets = temp_dir("presets");
    let mut o = options(Some(6.));
    o.presets = Some(presets.clone());
    let ed = editor::create(o, &fixture(), None, editor::prepare_fn(), editor::draw_fn()).expect("opens");
    let mut ed = wait(0, ed);
    ed = up(ed, fre(50, vec![k('/'), k('s')]));
    let graph_width = width(editor::panes(&ed, &fr(50)).graph);
    ed = up(ed, fre(50, vec![k('/'), k('g')]));
    assert!(width(editor::panes(&ed, &fr(50)).graph) == graph_width, "open preset prompt let a workspace shortcut toggle the graph");
    ed = up(ed, fre(51, vec![key(Key::Enter)]));
    let saved = preset::list(&presets);
    assert!(saved.len() == 1, "/ s did not save a preset of the document");
    let original = text_of(&ed);
    assert!(has(&std::fs::read_to_string(preset::path(&presets, &saved[0].0)).unwrap(), &original), "the preset file is not the workspace text");
    ed = editor::edit(ed, &fe::Op::Rename { node: vec!["geo1".into(), "output".into()], to_: "moved".into() }).unwrap_or_else(|m| panic!("{m}"));
    assert!(text_of(&ed) != original, "the rename did not change the document");
    let phases = editor::edit_phases(&ed);
    use flow_syntax::phase_timer as pt;
    assert!(pt::calls(&phases, "check") == 1 && pt::calls(&phases, "evaluate") == 1 && pt::calls(&phases, "lower") == 1 && phases.entries.iter().all(|e| e.seconds >= 0.), "the editor edit did not perform exactly one check/evaluation/lowering");
    assert!(has(&report_line(&ed, "phases"), "check"), "the crash report omitted the last edit's phases");
    ed = up(ed, fre(52, vec![k('/'), k('b')]));
    ed = up(ed, fre(53, vec![key(Key::Enter)]));
    assert!(text_of(&ed) == original && editor::undo_label(&ed) == Some("Load preset".to_string()), "/ b did not load the saved preset as one history entry");
    // Looking through the camera fits the film to its aspect (the render resolution); otherwise the whole pane.
    ed = up(ed, fre(52, vec![k('/'), k('v')]));
    ed = up(ed, fr(53));
    assert!(editor::look_through(&ed) && near(rays::camera::position(&editor::view_camera(&ed)), eye(&ed)), "/ v did not look through the render camera");
    let settings = editor::render_settings(&ed);
    let (_, _, pane_w, pane_h) = editor::panes(&ed, &fr(54)).view;
    let (fx, fy, fw, fh) = editor::film(&ed, &fr(54));
    assert!(settings.width == 1920 && settings.height == 1080 && settings.max_spp == 256, "the scene root did not carry default render settings");
    // the frame keeps clear of the label row above it (40), the readouts' row under it (36) and 12
    // at each side, the film's aspect inside what is left, centred there. A body under 1000 wide
    // follows the workspace sheet: 14.63 percent at each side, 32 above, 93 below
    let narrow = pane_w < 1000;
    let side = if narrow { (0.1463 * pane_w as f64).round() as i64 } else { 12 };
    let (top, bottom) = if narrow { (32, 93) } else { (40, 36) };
    let (area_w, area_h) = (pane_w - 2 * side, pane_h - top - bottom);
    assert!(fw <= area_w && fh <= area_h && (fw == area_w || fh == area_h) && (fw * 1080 - fh * 1920).abs() <= 1920 && fx == side + (area_w - fw) / 2 && fy == if narrow { top } else { top + (area_h - fh) / 2 }, "look-through did not letterbox the film to the camera's aspect");
    // / v toggles look-through off: the view camera is the free viewport
    ed = up(ed, fre(54, vec![k('/'), k('v')]));
    assert!(!editor::look_through(&ed) && near(rays::camera::position(&editor::view_camera(&ed)), viewport_eye(&ed)), "with look-through off the view camera did not follow the free viewport");
    assert!(editor::film(&ed, &fr(54)) == (0, 0, pane_w, pane_h), "without look-through the film did not fill the view pane");
    ed = up(ed, fre(54, vec![k('/'), k('v')]));
    assert!(editor::look_through(&ed), "/ v did not restore look-through");
    editor::close(&ed);
    let _ = std::fs::remove_dir_all(&presets);
}

pub fn workspace_walk() {
    if !crate::common::gpu() {
        return;
    }
    let mut o = options(None);
    o.await_ = Some(true);
    let ed = editor::create(o, &fixture(), None, editor::prepare_fn(), editor::draw_fn()).expect("opens");
    let mut ed = wait(0, ed);
    ed = enter_geo1(ed, 0);
    assert!(editor::level(&ed) == Some("geo1".to_string()), "entering geo1 from the scene list did not open its SOP network");
    ed = up(ed, fre(1, vec![k('/'), k('h')]));
    let hidden_scene = editor::scene(&ed, &fr(1));
    assert!(rays::scene::same(&editor::scene(&ed, &fr(2)), &hidden_scene), "unchanged hidden 3D scene composition was rebuilt");
    ed = up(ed, fre(2, vec![k('/'), k('h')]));
    // Graph focus shows the Flow guide; the ordinary status/FPS strip remains under View focus.
    ed = up(ed, fr(3));
    ed = up(ed, frm(4, (100, 500), vec![press(LeftButton, (100, 500)), release(LeftButton, (100, 500))]));
    let at = |count: i64, fps: f64| {
        let mut f = fr(count);
        f.fps = fps;
        f
    };
    let status = |ed: &editor::Editor, f: &frame::Frame| ui_bytes(&editor::scene(ed, f));
    ed = up(ed, at(1_000, 60.));
    let fps_initial = status(&ed, &at(1_000, 60.));
    ed = up(ed, at(1_030, 120.));
    let fps_before_deadline = status(&ed, &at(1_030, 120.));
    ed = up(ed, at(1_061, 120.));
    let fps_after_deadline = status(&ed, &at(1_061, 120.));
    assert!(fps_before_deadline == fps_initial && fps_after_deadline != fps_initial, "FPS status text ignored its one-second sampling deadline");
    assert!(editor::selected_node(&ed).is_none(), "camera/render controls should own an unselected inspector");
    let current_frame = fr(10);
    let panes = editor::panes(&ed, &current_frame);
    assert!(rays::easy_camera::control_area(&editor::camera(&ed)) == Some(panes.view), "3D camera gestures are not confined to the view column");
    let (ix, iy, _, _) = panes.inspector;
    let camera_header = (ix + 32, iy + 15);
    let collapsed_scene = ui_bytes(&editor::scene(&ed, &current_frame));
    let blank_inspector = (ix + 4, iy + 200);
    ed = up(ed, fre(10, vec![press(LeftButton, blank_inspector), release(LeftButton, blank_inspector), k('/'), k('w')]));
    assert!(!editor::flying(&ed), "same-frame inspector click routed a view-only shortcut");
    ed = up(ed, fre(11, vec![press(LeftButton, camera_header)]));
    ed = up(ed, fre(12, vec![release(LeftButton, camera_header)]));
    ed = up(ed, fre(13, vec![mv((10, 100))]));
    let expanded_scene = ui_bytes(&editor::scene(&ed, &fr(13)));
    assert!(expanded_scene != collapsed_scene, "workspace camera accordion lost its armed press before the release frame");
    ed = up(ed, fre(14, vec![press(LeftButton, camera_header)]));
    ed = up(ed, fre(15, vec![release(LeftButton, camera_header)]));
    ed = up(ed, fre(16, vec![mv((10, 100))]));
    let render_header = (ix + 32, iy + 39);
    ed = up(ed, fre(17, vec![press(LeftButton, render_header)]));
    ed = up(ed, fre(18, vec![release(LeftButton, render_header)]));
    ed = up(ed, fre(19, vec![mv((10, 100))]));
    assert!(ui_bytes(&editor::scene(&ed, &fr(19))) != collapsed_scene, "workspace render accordion lost its armed press before the release frame");
    ed = up(ed, fre(19, vec![k('/'), k('w')]));
    assert!(!editor::flying(&ed), "inspector child press did not focus its pane");
    // the header: the middle of the card is a row (or the [+ N more] row, which opens it)
    let point = {
        let (x, y, w, _) = node_box(&ed, &["geo1", "source"]);
        (x + w / 2, y + 12)
    };
    ed = up(ed, frm(20, point, vec![press(LeftButton, point), release(LeftButton, point)]));
    assert!(report_line(&ed, "scope selected") == "scope selected: geo1/source", "graph selection did not select the clicked node");
    ed = up(ed, fre(20, vec![key(Key::Home)]));
    ed = up(ed, fr(20));
    ed = up(ed, fre(21, vec![k('/'), k('c')]));
    assert!(report_line(&ed, "scope selected") == "scope selected: -", "/ c did not restore the camera/render inspector");
    // Shared undo stack: an added node is one entry; Command-Z removes it, Shift-Command-Z brings it back.
    let source_point = card_row(node_box(&ed, &["geo1", "source"]));
    ed = up(ed, frm(23, source_point, vec![press(LeftButton, source_point), release(LeftButton, source_point)]));
    let chord = |shift: bool, c: char, count: i64| {
        let mut keys = vec![Key::Meta];
        if shift {
            keys.push(Key::Shift);
        }
        with_keys(fre(count, vec![k(c)]), keys)
    };
    ed = up(ed, frm(24, source_point, vec![k('/'), k('a')]));
    ed = up(ed, frm(25, source_point, vec![Event::Text_input("null".into()), key(Key::Enter)]));
    assert!(has(&text_of(&ed), "(sop/null source)"), "the leader add-node menu did not add a node wired to the selection");
    assert!(editor::can_undo(&ed), "the added node did not enter the undo stack");
    ed = up(ed, chord(false, 'z', 26));
    assert!(!has(&text_of(&ed), "sop/null"), "Command-Z did not undo the added node");
    assert!(editor::can_redo(&ed), "undo did not leave a redo entry");
    ed = up(ed, chord(true, 'z', 27));
    assert!(has(&text_of(&ed), "(sop/null source)"), "Shift-Command-Z did not redo the added node");
    ed = up(ed, chord(false, 'z', 28));
    assert!(!has(&text_of(&ed), "sop/null"), "second undo failed after redo");
    // Tile positions are document state: one drag is one undo entry.
    let tile_x = |ed: &editor::Editor, path: &[&str]| node_box(ed, path).0;
    let x0 = tile_x(&ed, &["geo1", "source"]);
    let (sx, sy) = source_point;
    for (count, mouse, events) in [(30, (sx, sy), vec![]), (31, (sx, sy), vec![press(LeftButton, (sx, sy))]), (32, (sx + 20, sy), vec![mv((sx + 20, sy))]), (33, (sx + 40, sy), vec![mv((sx + 40, sy))])] {
        ed = up(ed, with_buttons(frm(count, mouse, events), if count > 30 { vec![LeftButton] } else { vec![] }));
    }
    ed = up(ed, frm(34, (sx + 40, sy), vec![release(LeftButton, (sx + 40, sy))]));
    assert!(tile_x(&ed, &["geo1", "source"]) != x0, "dragging a node did not move it");
    ed = up(ed, chord(false, 'z', 35));
    ed = up(ed, fr(36));
    assert!(tile_x(&ed, &["geo1", "source"]) == x0, "undo did not restore the dragged node position");
    // A multi-node drag moves every selected node, and undo and redo restore every one.
    let paths: [&[&str]; 2] = [&["geo1", "source"], &["geo1", "output"]];
    let xs = |ed: &editor::Editor| paths.iter().map(|p| tile_x(ed, p)).collect::<Vec<_>>();
    let xs0 = xs(&ed);
    let (ox, oy) = card_header(node_box(&ed, &["geo1", "output"]));
    let held = vec![LeftButton];
    let steps: Vec<(i64, (i64, i64), Vec<input::MouseButton>, Vec<Key>, Vec<Event>)> = vec![
        // well past the double-click interval of the earlier presses on these nodes
        (140, (ox, oy), vec![], vec![], vec![]),
        (141, (ox, oy), held.clone(), vec![Key::Shift], vec![press(LeftButton, (ox, oy))]),
        (142, (ox, oy), vec![], vec![Key::Shift], vec![release(LeftButton, (ox, oy))]),
        (143, (sx, sy), vec![], vec![], vec![]),
        (144, (sx, sy), held.clone(), vec![], vec![press(LeftButton, (sx, sy))]),
        (145, (sx + 30, sy), held.clone(), vec![], vec![mv((sx + 30, sy))]),
        (146, (sx + 30, sy), vec![], vec![], vec![release(LeftButton, (sx + 30, sy))]),
    ];
    for (count, mouse, buttons, keys, events) in steps {
        ed = up(ed, with_keys(with_buttons(frm(count, mouse, events), buttons), keys));
    }
    let xs1 = xs(&ed);
    assert!(xs0.iter().zip(&xs1).all(|(a, b)| a != b), "dragging a multi-node selection did not move every node");
    ed = up(ed, chord(false, 'z', 147));
    ed = up(ed, fr(148));
    assert!(xs(&ed) == xs0, "undo did not restore every dragged node");
    ed = up(ed, chord(true, 'z', 149));
    ed = up(ed, fr(150));
    assert!(xs(&ed) == xs1, "redo did not restore every dragged node");
    ed = up(ed, chord(false, 'z', 151));
    // / c clears the selection the drag made, as before this check.
    ed = up(ed, fre(152, vec![k('/'), k('c')]));
    let centered = |ed: &editor::Editor| -> bool {
        editor::prepared(ed)
            .and_then(|g| rdk_rays::rays_mesh::to_mesh(&g).ok())
            .and_then(|m| rays::mesh::centroid(&m).ok().flatten())
            .is_some_and(|c| (c.x - 5.).abs() < 1.)
    };
    let mut count = 153;
    while !centered(&ed) && count < 153 + 2000 {
        ed = up(ed, fr(count));
        count += 1;
    }
    assert!(centered(&ed), "display-node cook did not publish the graph's result");
    assert!(!editor::scene(&ed, &current_frame).is_empty(), "sketch environment produced an empty composed scene");
    // The whole workspace paints in batch groups, not one draw per label.
    let staged = rays::scene::Layer::stage_native_render(900, 640, &editor::scene(&ed, &current_frame)).unwrap_or_else(|m| panic!("{m}"));
    let mut batches = 0;
    for layer in &staged.layers {
        if let rays::scene::Layer::native_layer::Ui_layer(batch, _) = layer {
            batches += batch.batches().len();
        }
    }
    assert!(batches > 0 && batches <= 32, "workspace UI draws were not batched: {batches}");
    let (graph_x, graph_y, graph_width, graph_height) = editor::panes(&ed, &fr(229)).graph;
    let menu_point = (graph_x + graph_width / 2, graph_y + graph_height / 2);
    let add = |ed: editor::Editor, count: i64| {
        let ed = up(ed, frm(count, menu_point, vec![k('/'), k('a')]));
        up(ed, frm(count + 1, menu_point, vec![Event::Text_input("null".into()), key(Key::Enter)]))
    };
    ed = add(ed, 230);
    assert!(has(&text_of(&ed), "(sop/null output)") && sop::node::label(&editor::displayed_node(&ed)) == "transform", "workspace could not add a node reading the result without moving the display: {}\n{}", sop::node::operation(&editor::displayed_node(&ed)), text_of(&ed));
    assert!(report_line(&ed, "scope selected") == "scope selected: geo1/null", "the added node was not selected");
    ed = up(ed, fre(232, vec![key(Key::Backspace)]));
    assert!(!has(&text_of(&ed), "sop/null"), "Backspace did not delete the added node");
    ed = add(ed, 234);
    ed = up(ed, fre(236, vec![k('/'), k('g')]));
    ed = up(ed, fre(237, vec![key(Key::Backspace)]));
    assert!(has(&text_of(&ed), "sop/null"), "hidden graph still accepted a delete shortcut");
    ed = up(ed, fre(238, vec![k('/'), k('g')]));
    // / t shows the timeline bar; dragging its scrub slider seeks and pauses the shared clock.
    ed = up(ed, fre(240, vec![k('/'), k('t')]));
    let (tx, ty, tw, th) = editor::panes(&ed, &fr(241)).timeline;
    assert!(th > 0 && tw == 900, "/ t did not show the full-width timeline bar");
    ed = up(ed, fr(241));
    let (scrub_y, scrub_x) = (ty + th / 2, tx + tw - 60);
    ed = up(ed, fre(242, vec![press(LeftButton, (scrub_x, scrub_y))]));
    ed = up(ed, fre(243, vec![mv((tx + tw - 10, scrub_y)), release(LeftButton, (tx + tw - 10, scrub_y))]));
    let clock = editor::timeline(&ed);
    assert!(rays_editor::Timeline::mode(&clock) == rays_editor::Timeline::Mode::Paused && rays_editor::Timeline::frame(&clock) >= 200, "dragging the timeline scrub did not seek and pause");
    ed = up(ed, fre(244, vec![k('/')]));
    ed = up(ed, fre(245, vec![key(Key::Escape)]));
    editor::close(&ed);
}
