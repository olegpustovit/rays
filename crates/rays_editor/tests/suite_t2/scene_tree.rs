//! test_scene_tree.ml: the scene tree, window-free: list keys, enter/up, lights, reparent, hide,
//! World keys, the three projections, host camera deletion, typed draw errors.
use std::cell::Cell;
use std::rc::Rc;

use crate::common;
use super::support::*;
use editor_document::kind_graph as kg;
use editor_document::workspace_doc as wd;
use input::{Event, Key, MouseButton};
use param::Value;
use rays_editor::editor;

pub const CASES: &[(&str, fn())] = &[
    ("scene_tree_a_list", run_a),
    ("scene_tree_b_reparent", run_b),
    ("scene_tree_c_slider", run_c),
    ("scene_tree_d_world", run_d),
    ("scene_tree_d2_world_keys", run_d2),
    ("scene_tree_e_projections", run_e),
    ("scene_tree_domains_agree", domains_agree),
    ("scene_tree_run_host", run_host),
    ("scene_tree_run_surface_errors", run_surface_errors),
];

pub const SKIPPED: &[(&str, &str)] = &[];

const TEXT: &str = r#"(workspace scene_tree
  (graph geo :context sop (sop/box))
  (graph scene :context scene
    (let* [body (scene/geometry (ref geo) :name "geo1")
           key (scene/light :name "key")
           fill (scene/light :name "fill" :translate [-3 4 2])
           cam (scene/camera :name "camera1" :aperture 0.3)
           world (scene/world (ref sky) :name "world")]
      (scene/merge body key fill cam world)))
  (graph sky :context world
    (world/scatter (world/shape (world/sky :name "sky") :name "softbox")
                   :name "constellation")))"#;

fn tframe(mouse: (i64, i64), events: Vec<Event>, keys: &[Key], buttons: &[MouseButton], count: i64) -> rays::frame::Frame {
    frame_full(buttons, keys, (0., 0.), (mouse.0 as f64, mouse.1 as f64), events, count)
}

fn source(e: &Ed) -> String {
    wd::to_text(&editor::workspace(e)).unwrap()
}

type Labelled = Vec<(String, String, Option<String>, Vec<(String, Value)>)>;

fn labelled(e: &Ed) -> Labelled {
    let graph = editor::scene_document(e);
    let mut v: Labelled = graph
        .iter()
        .map(|info| {
            (
                info.label.clone(),
                info.kind.clone(),
                info.input.and_then(|id| kg::find(&graph, id)).map(|n| n.label),
                kg::fields(info).into_iter().map(|f| (f.name, f.current)).collect(),
            )
        })
        .collect();
    v.sort_by(|a, b| format!("{a:?}").cmp(&format!("{b:?}")));
    v
}

fn open(text: &str, counter: Option<Rc<Cell<i64>>>) -> Ed {
    let ws = workspace(text);
    let prepare: rays_editor::cook::Prepare_fn = Rc::new(move |s, o| {
        if let Some(c) = &counter {
            c.set(c.get() + 1);
        }
        rays_editor::cook::prepare(s, o)
    });
    let options = editor::Create { platform: Some(input::Platform::Apple), ui_text_size: Some(11), await_: Some(true), domains: Some(1), max_entries: Some(4), max_payload_bytes: Some(16 * 1024 * 1024), ..Default::default() };
    editor::create(options, &ws, None, prepare, editor::draw_fn()).expect("the editor opens")
}

fn domains_agree() {
    if !common::gpu() {
        return;
    }
    let digest = |domains: i64| {
        let ws = workspace(
            r#"(workspace mountain
      (graph g :context sop
        (sop/mountain (sop/grid :columns 12 :rows 12 :size 4 :width 4 :height 4)
                      :seed 3 :height 0.4 :frequency [1 1 1] :octaves 3 :lacunarity 2
                      :roughness 0.5 :recompute_normals true)))"#,
        );
        let options = editor::Create { platform: Some(input::Platform::Apple), ui_text_size: Some(11), await_: Some(true), domains: Some(domains), grain: Some(16), max_entries: Some(4), max_payload_bytes: Some(16 * 1024 * 1024), ..Default::default() };
        let e = editor::create(options, &ws, None, editor::prepare_fn(), editor::draw_fn()).expect("editor");
        let e = editor::update(e, &tframe((450, 320), vec![], &[], &[], 0));
        let g = editor::prepared(&e).expect("the domain-count cook did not finish");
        editor::close(&e);
        let v = rdk_core::packed::Float3::View::view(rdk_core::geometry::positions(&g)).unwrap();
        (0..v.x.len()).map(|i| format!("{:x} {:x} {:x}", v.x[i].to_bits(), v.y[i].to_bits(), v.z[i].to_bits())).collect::<Vec<_>>().join(",")
    };
    assert!(digest(1) == digest(4), "one domain and four domains cooked different geometry");
}

struct Run {
    e: Ed,
    count: i64,
}

impl Run {
    fn step_with(&mut self, mouse: (i64, i64), keys: &[Key], buttons: &[MouseButton], events: Vec<Event>) {
        
        self.count += 1;
        let started = std::time::Instant::now();
        let shown = format!("{events:?}");
        self.e = editor::update(self.e.clone(), &tframe(mouse, events, keys, buttons, self.count));
        if started.elapsed().as_secs_f64() > 1.0 { eprintln!("SLOW step {} {:.1}s {}", self.count, started.elapsed().as_secs_f64(), shown); }
    }
    fn step(&mut self, events: Vec<Event>) {
        self.step_with((450, 320), &[], &[], events);
    }
    fn key(&mut self, k: Key) {
        self.step(vec![Event::Key_pressed(k)]);
    }
    fn chars(&mut self, cs: &[char]) {
        self.step(cs.iter().map(|c| Event::Key_pressed(Key::KeyChar(*c))).collect());
    }
    fn label(&self) -> Option<String> {
        editor::selected_node(&self.e).map(|p| p.label)
    }
    fn click_at(&mut self, p: (i64, i64)) {
        let f = (p.0 as f64, p.1 as f64);
        self.step_with(p, &[], &[], vec![Event::Pointer_pressed(MouseButton::LeftButton, f), Event::Pointer_released(MouseButton::LeftButton, f)]);
    }
    fn names(&self) -> String {
        editor::document(&self.e).iter().map(|i| i.label.clone()).collect::<Vec<_>>().join(",")
    }
    fn select(&mut self, name: &str, tries: i64) {
        let mut tries = tries;
        while self.label().as_deref() != Some(name) {
            assert!(tries > 0, "no list row named {name} among {} at level {}", self.names(), editor::level(&self.e).unwrap_or("scene".into()));
            self.key(Key::ArrowDown);
            tries -= 1;
        }
    }
    fn select_up(&mut self, name: &str, tries: i64) {
        let mut tries = tries;
        while self.label().as_deref() != Some(name) {
            assert!(tries > 0, "no list row named {name}");
            self.key(Key::ArrowUp);
            tries -= 1;
        }
    }
    fn projection(&self) -> String {
        with_dir(|dir| {
            editor::crash_dump(&self.e, dir).expect("dump");
            std::fs::read_to_string(format!("{dir}/editor.txt")).unwrap().lines().nth(1).unwrap_or("").to_owned()
        })
    }
    fn graph_pane(&self) -> (i64, i64, i64, i64) {
        editor::panes(&self.e, &tframe((450, 320), vec![], &[], &[], 0)).graph
    }
}

/// The monolithic OCaml `run` is cut in five sections, each from a fresh editor (a 144-frame
/// chain would be over the 5 s case budget in a debug build).
fn section(body: fn(&mut Run, &Ctx, &Rc<Cell<i64>>)) {
    if !common::gpu() {
        return;
    }
    let cooks = Rc::new(Cell::new(0i64));
    let mut r = Run { e: open(TEXT, Some(cooks.clone())), count: 0 };
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let cx = prefix(&mut r, &cooks);
        body(&mut r, &cx, &cooks)
    }));
    editor::close(&r.e);
    if let Err(p) = result {
        std::panic::resume_unwind(p);
    }
}

fn run_a() {
    section(sec_a)
}
fn run_b() {
    section(sec_b)
}
fn run_c() {
    section(sec_c)
}
fn run_d() {
    section(sec_d)
}
fn run_d2() {
    section(sec_d2)
}
fn run_e() {
    section(sec_e)
}

struct Ctx {
    in_list: (i64, i64),
    gx: i64,
    gy: i64,
}

fn press(p: (f64, f64)) -> Vec<Event> {
    vec![Event::Pointer_pressed(MouseButton::LeftButton, p), Event::Pointer_released(MouseButton::LeftButton, p)]
}

fn at(p: (i64, i64)) -> (f64, f64) {
    (p.0 as f64, p.1 as f64)
}

fn prefix(r: &mut Run, cooks: &Rc<Cell<i64>>) -> Ctx {
    r.step(vec![]);
    assert!(editor::prepared(&r.e).is_some() && cooks.get() > 0, "the scene did not cook geo1");
    let (gx, gy, _, _) = r.graph_pane();
    let in_list = (gx + 40, gy + 400);
    r.step_with(in_list, &[], &[], press((40., 1.)));
    r.step_with(in_list, &[], &[], press(at(in_list)));
    Ctx { in_list, gx, gy }
}

fn row_of(r: &mut Run, cx: &Ctx, name: &str) -> ((i64, i64), Vec<Event>) {
    for index in 0..=5 {
        let point = (cx.gx + 60, cx.gy + 24 + 24 * index + 12);
        let click = press(at(point));
        r.step_with(point, &[], &[], click.clone());
        if r.label().as_deref() == Some(name) {
            return (point, click);
        }
    }
    panic!("{name} row not found among {} at level {}", r.names(), editor::level(&r.e).unwrap_or("scene".into()));
}

fn slash(r: &mut Run, c: char) {
    r.step(vec![Event::Key_pressed(Key::KeyChar('/')), Event::Key_pressed(Key::KeyChar(c))]);
}

fn slash_l(r: &mut Run, c: char) {
    r.step(vec![Event::Key_pressed(Key::KeyChar('/')), Event::Key_pressed(Key::KeyChar('l')), Event::Key_pressed(Key::KeyChar(c))]);
}

/// List keys, camera, enter/up, the Flow list projection, double-click enter.
fn sec_a(r: &mut Run, cx: &Ctx, _cooks: &Rc<Cell<i64>>) {
    r.key(Key::Home);
    let first = editor::selected_node(&r.e).map(|p| p.id);
    r.key(Key::ArrowDown);
    assert!(editor::selected_node(&r.e).map(|p| p.id) != first, "the list's down arrow did not move to the next row");
    r.key(Key::ArrowUp);
    assert!(editor::selected_node(&r.e).map(|p| p.id) == first, "the list's up arrow did not return to the previous row");
    r.select("camera1", 6);
    assert!(rays::camera::lens(&editor::render_camera(&r.e)).aperture == 0.3, "the ACTIVE camera did not carry its text lens");
    r.chars(&['i']);
    let same_cam = |e: &Ed| format!("{:?}", editor::view_camera(e)) == format!("{:?}", editor::render_camera(e));
    assert!(editor::look_through(&r.e) && same_cam(&r.e) && editor::level(&r.e).is_none(), "entering the camera did not look through it at the scene level");
    r.chars(&['i']);
    assert!(editor::look_through(&r.e), "entering the camera again toggled look-through off");
    r.key(Key::Home);
    r.select("geo1", 6);
    assert!(editor::level(&r.e).is_none(), "the editor did not open at the scene level");
    r.chars(&['i']);
    assert!(editor::level(&r.e).as_deref() == Some("geo1"), "i did not enter the focused geometry object");
    assert!(editor::network(&r.e).map(|n| sop::edit_graph::inspect(&n).iter().any(|i| i.label == "box")).unwrap_or(false), "geo1's network is not the sketch's SOP graph");
    slash_l(r, 'l');
    r.step(vec![]);
    assert_eq!(r.projection(), "projection: list", "/ l l did not reach Flow list");
    let (gx, gy, _, _) = r.graph_pane();
    let row = (gx + 40, gy + 24 + 12);
    r.step_with(row, &[], &[], vec![]);
    r.step_with(row, &[], &[], press(at(row)));
    r.key(Key::Enter);
    assert_eq!(r.projection(), "projection: graph", "list Enter did not open the selected node in graph");
    r.chars(&['u']);
    assert!(editor::level(&r.e).is_none(), "u did not go back to the scene");
    r.step(vec![]);
    // A double-click on a list row enters it too.
    let (point, click) = row_of(r, cx, "geo1");
    r.step_with(point, &[], &[], click);
    assert!(editor::level(&r.e).as_deref() == Some("geo1"), "double-clicking the geo1 row did not enter it");
    r.chars(&['u']);
}

/// Scene edits never re-cook: reparent, hide, undo.
fn sec_b(r: &mut Run, _cx: &Ctx, cooks: &Rc<Cell<i64>>) {
    r.key(Key::Home);
    let lights_before = editor::lights(&r.e).len();
    let cooked = cooks.get();
    let position = |e: &Ed| match editor::lights(e).first().map(|l| l.kind.clone()) {
        Some(rays::light::kind::Area { position, .. }) => position,
        _ => panic!("the first light is not an area light"),
    };
    let before = position(&r.e);
    r.select("key", 6);
    r.key(Key::Tab);
    let parent = |e: &Ed| editor::scene_document(e).into_iter().find(|i| i.label == "key").unwrap().input;
    assert!(parent(&r.e).is_some(), "Tab did not reparent the light under the row above");
    assert!(source(&r.e).contains(":parent \"geo1\""), "Tab did not write the parent to the text");
    assert_eq!(editor::undo_label(&r.e).as_deref(), Some("Reparent"), "reparenting is not one 'Reparent' entry");
    assert!(rays_math::vec3::nearly_equal(position(&r.e), before, 1e-6), "reparenting moved the light in the world");
    r.step_with((450, 320), &[Key::Shift], &[], vec![Event::Key_pressed(Key::Tab)]);
    assert!(parent(&r.e).is_none(), "Shift-Tab did not move the light back to the scene root");
    assert!(!source(&r.e).contains(":parent"), "Shift-Tab left the parent in the text");
    r.chars(&['h']);
    assert!(editor::lights(&r.e).len() == lights_before - 1, "h did not hide the light");
    assert!(source(&r.e).contains(":visible false"), "h did not write the hidden flag to the text");
    assert_eq!(editor::undo_label(&r.e).as_deref(), Some("Hide"), "hiding is not one 'Hide' entry");
    for _ in 0..3 {
        r.step(vec![]);
    }
    assert!(cooks.get() == cooked, "a scene-level edit re-cooked SOPs");
    // Undo walks back through the scene edits.
    r.step_with((450, 320), &[Key::Meta], &[], vec![Event::Key_pressed(Key::KeyChar('z'))]);
    assert!(editor::lights(&r.e).len() == lights_before, "undo did not show the light again");
    assert!(!source(&r.e).contains(":visible false"), "undo left the hidden flag in the text");
}

/// Dragging an object transform slider in the inspector does not re-cook.
fn sec_c(r: &mut Run, _cx: &Ctx, cooks: &Rc<Cell<i64>>) {
    r.key(Key::Home);
    let cooked = cooks.get();
    r.select("geo1", 8);
    r.step(vec![]);
    let (ix, iy, _, _) = editor::panes(&r.e, &tframe((450, 320), vec![], &[], &[], 0)).inspector;
    let slider = |x: i64| (ix + 100 + x, iy + 346);
    let translate_x = |e: &Ed| editor::selected_node(e).and_then(|n| n.fields.into_iter().find(|f| f.name == "translate_x").map(|f| f.current));
    let before = translate_x(&r.e);
    r.step_with(slider(0), &[], &[MouseButton::LeftButton], vec![Event::Pointer_pressed(MouseButton::LeftButton, at(slider(0)))]);
    for x in [4, 8, 12, 16] {
        r.step_with(slider(x), &[], &[MouseButton::LeftButton], vec![]);
    }
    r.step_with(slider(20), &[], &[], vec![Event::Pointer_released(MouseButton::LeftButton, at(slider(20)))]);
    assert!(before.is_some() && translate_x(&r.e) != before, "dragging the Translate X slider did not edit geo1");
    assert!(source(&r.e).contains(":translate ["), "the Translate X drag did not reach the text");
    assert_eq!(editor::undo_label(&r.e).as_deref(), Some("Edit value"), "the slider drag is not one \"Edit value\" entry");
    assert!(editor::objects(&r.e).iter().map(|(m, _)| m.clone()).collect::<Vec<_>>() != vec![rays_math::mat4::identity()], "the transform did not reach objects");
    for _ in 0..4 {
        r.step(vec![]);
    }
    assert!(cooks.get() == cooked, "an object transform edit re-cooked SOPs");
}

fn parameters(e: &Ed, id: i64) -> Vec<(String, Value)> {
    kg::fields(&kg::find(&editor::document(e), id).unwrap()).into_iter().map(|f| (f.name, f.current)).collect()
}

/// The World: / e bakes, a map-layer drag clamps and undoes, World keys, the preset.
fn sec_d(r: &mut Run, cx: &Ctx, _cooks: &Rc<Cell<i64>>) {
    slash(r, 'e');
    assert!(editor::level(&r.e).as_deref() == Some("world"), "/ e did not open the World");
    r.step(vec![]);
    assert!(editor::world(&r.e).is_some(), "the World did not bake");
    r.step_with(cx.in_list, &[], &[], press(at(cx.in_list)));
    r.select("softbox", 16);
    let selected = editor::selected_node(&r.e).unwrap().id;
    let original = parameters(&r.e, selected);
    let others: Vec<(i64, Vec<(String, Value)>)> = editor::document(&r.e).iter().filter(|i| i.id != selected).map(|i| (i.id, parameters(&r.e, i.id))).collect();
    let press_p = (100., 300.);
    let release = (800., 1000.);
    let camera = editor::camera(&r.e);
    r.step_with((100, 300), &[], &[MouseButton::LeftButton], vec![Event::Pointer_pressed(MouseButton::LeftButton, press_p)]);
    r.step_with((800, 1000), &[], &[MouseButton::LeftButton], vec![Event::Pointer_moved(release)]);
    let moved = parameters(&r.e, selected);
    assert!(moved != original && moved.iter().find(|(n, _)| n == "elevation").map(|(_, v)| v.clone()) == Some(Value::Float(-90.)), "captured map-layer movement did not clamp outside the map");
    assert!(source(&r.e).contains(":elevation -90.0"), "the map-layer drag did not reach the text");
    assert!(others.iter().all(|(id, values)| &parameters(&r.e, *id) == values), "a map-layer gesture edited another stable node");
    r.step_with((800, 1000), &[], &[], vec![Event::Pointer_released(MouseButton::LeftButton, release)]);
    assert!(
        rays::camera::position(&rays::easy_camera::camera(&editor::camera(&r.e)).unwrap()) == rays::camera::position(&rays::easy_camera::camera(&camera).unwrap()),
        "map-layer movement also navigated the 3D camera"
    );
    r.step_with((450, 320), &[Key::Meta], &[], vec![Event::Key_pressed(Key::KeyChar('z'))]);
    assert!(parameters(&r.e, selected) == original, "map-layer drag did not undo in one entry");
    assert!(!source(&r.e).contains(":elevation -90.0"), "undoing the map-layer drag left the text edited");
}

/// World keys on a fresh World level.
fn sec_d2(r: &mut Run, cx: &Ctx, _cooks: &Rc<Cell<i64>>) {
    slash(r, 'e');
    r.step(vec![]);
    r.step_with(cx.in_list, &[], &[], press(at(cx.in_list)));
    r.key(Key::Home);
    r.select("softbox", 16);
    // World keys: t flips the selected emitter to a real light, ] moves the time of day, 3 loads
    // the white room preset.
    r.chars(&['t']);
    r.step(vec![]);
    assert!(editor::world(&r.e).map(|b| !b.lights.is_empty()).unwrap_or(false), "t did not promote the selected shape to a light");
    assert!(source(&r.e).contains(":emit \"Light\""), "World t did not write the emitter to the text");
    r.select("constellation", 16);
    let scatter = editor::selected_node(&r.e).unwrap().id;
    let seed = |e: &Ed| parameters(e, scatter).into_iter().find(|(n, _)| n == "seed").unwrap().1;
    let before = seed(&r.e);
    r.chars(&['n']);
    assert!(matches!((before, seed(&r.e)), (Value::Int(a), Value::Int(b)) if b == a + 1), "World n did not reseed the selected scatter layer");
    r.chars(&['d']);
    let world_field = |e: &Ed, name: &str| editor::scene_document(e).iter().find(|i| i.kind == "world").and_then(|i| kg::field(i, name)).unwrap();
    assert!(world_field(&r.e, "day_cycle") == Value::Float(1.), "World d did not start its day cycle");
    assert!(source(&r.e).contains(":day_cycle 1.0") && source(&r.e).contains(":seed"), "World n and d did not reach the text");
    let hours = |e: &Ed| match world_field(e, "time_of_day") {
        Value::Float(h) => h,
        _ => panic!("time_of_day"),
    };
    let before = hours(&r.e);
    r.chars(&[']']);
    assert!((hours(&r.e) - before - 0.5).abs() < 1e-9, "] did not step the time of day");
    r.chars(&['3']);
    assert!(editor::document(&r.e).iter().any(|i| i.kind == "room"), "3 did not load the white room preset");
    assert!(source(&r.e).contains("world/room") && editor::undo_label(&r.e).as_deref() == Some("Preset white room"), "the white room preset did not replace the World graph in the text");
    r.chars(&['u']);
    assert!(editor::level(&r.e).is_none(), "u did not leave the World");
    let saved = labelled(&r.e);
    let baked = editor::world(&r.e);
    r.chars(&['t', 'n', 'd']);
    let eager = |b: &Option<rays::world::baked>| b.as_ref().map(|b| format!("{:?}", (&b.camera, &b.lighting, &b.lights, &b.sun, &b.background, b.exposure)));
    assert!(labelled(&r.e) == saved && eager(&editor::world(&r.e)) == eager(&baked), "World keys edited the scene level");
}

/// The graph/text projections, deleting the display object, reopening the text, right-click Enter.
fn sec_e(r: &mut Run, cx: &Ctx, _cooks: &Rc<Cell<i64>>) {
    slash_l(r, 't');
    r.step(vec![]);
    assert_eq!(r.projection(), "projection: text", "/ l t did not reach the scene graph's text");
    slash_l(r, 'g');
    r.step(vec![]);
    assert!(r.projection() == "projection: graph" && editor::level(&r.e).is_none(), "/ l g did not reach the scene graph");
    // Deleting a network's display node never leaves a dangling display.
    r.chars(&['u']);
    slash_l(r, 'l');
    r.step(vec![]);
    r.key(Key::Home);
    r.select("geo1", 8);
    r.key(Key::Delete);
    slash_l(r, 't');
    slash_l(r, 'g');
    slash_l(r, 'l');
    slash(r, 'e');
    r.chars(&['u']);
    assert!(editor::level(&r.e).is_none(), "the scene broke after deleting its display object");
    assert!(!source(&r.e).contains("(scene/geometry"), "deleting geo1 left it in the text");
    // what was written opens as the same scene
    let reopened = open(&source(&r.e), None);
    assert!(labelled(&reopened) == labelled(&r.e), "the saved text does not open as the edited scene");
    editor::close(&reopened);
    // The list's right-click menu enters the row under the pointer.
    r.step(vec![]);
    let (point, _) = row_of(r, cx, "world");
    let f = at(point);
    r.step_with(point, &[], &[], vec![Event::Pointer_pressed(MouseButton::RightButton, f), Event::Pointer_released(MouseButton::RightButton, f)]);
    r.step_with(point, &[], &[], vec![]);
    let item = (point.0 + 20, point.1 + 3 + 12);
    r.step_with(item, &[], &[], vec![]);
    r.step_with(item, &[], &[], press(at(item)));
    assert!(editor::level(&r.e).as_deref() == Some("world"), "the list's right-click Enter did not open the World");
}

fn run_host() {
    if !common::gpu() {
        return;
    }
    let mut r = Run { e: open("(workspace host (graph g :context sop (sop/box)))", None), count: 0 };
    let names = |e: &Ed| {
        let mut v: Vec<String> = editor::scene_document(e).iter().map(|i| i.label.clone()).collect();
        v.sort();
        v
    };
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        r.step(vec![]);
        assert!(names(&r.e) == vec!["camera1".to_owned(), "g".to_owned()], "the host did not seed its camera: {}", names(&r.e).join(","));
        let env = r.e.clone();
        let (gx, gy, _, _) = r.graph_pane();
        let at = ((gx + 40) as f64, (gy + 400) as f64);
        r.step(vec![Event::Pointer_pressed(MouseButton::LeftButton, at), Event::Pointer_released(MouseButton::LeftButton, at)]);
        r.key(Key::Home);
        let mut tries = 8;
        while r.label().as_deref() != Some("camera1") {
            assert!(tries > 0, "no list row named camera1 among {}", names(&r.e).join(","));
            r.key(Key::ArrowDown);
            tries -= 1;
        }
        r.key(Key::Delete);
        let no_camera = r.e.clone();
        assert!(!names(&no_camera).contains(&"camera1".to_owned()), "Delete did not remove the host camera");
        assert_eq!(editor::undo_label(&no_camera).as_deref(), Some("Delete"), "deleting the host camera is not one 'Delete' entry");
        assert!(source(&no_camera).contains("graph scene") && !source(&no_camera).contains("scene/camera"), "a deleted host camera is not in the text");
        let reopened = open(&source(&no_camera), None);
        let reopened = editor::update(reopened, &tframe((450, 320), vec![], &[], &[], 100));
        assert!(names(&reopened).contains(&"g".to_owned()) && !names(&reopened).contains(&"camera1".to_owned()), "the deleted camera came back on reload");
        editor::close(&reopened);
        r.step_with((450, 320), &[Key::Meta], &[], vec![Event::Key_pressed(Key::KeyChar('z'))]);
        let undone = r.e.clone();
        assert!(names(&undone).contains(&"camera1".to_owned()) && !source(&undone).contains("graph scene"), "undo did not give the host camera back");
        editor::close(&env);
        editor::close(&no_camera);
    }));
    editor::close(&r.e);
    if let Err(p) = result {
        std::panic::resume_unwind(p);
    }
}

fn run_surface_errors() {
    if !common::gpu() {
        return;
    }
    let ws = workspace(
        r#"(workspace surface_error
      (graph geo :context sop (let* [box (sop/box :size [1 1 1])] box))
      (graph scene :context scene (scene/root (scene/geometry (ref geo)))))"#,
    );
    let options = editor::Create { platform: Some(input::Platform::Apple), ui_text_size: Some(11), await_: Some(true), domains: Some(1), ..Default::default() };
    let mut current = editor::create(options, &ws, None, editor::prepare_fn(), editor::draw_fn()).expect("editor");
    let calls = Rc::new(Cell::new(0));
    let message = "surface conversion refused at draw time";
    let c = calls.clone();
    current.draw = Rc::new(move |_node, prepared| {
        c.set(c.get() + 1);
        let (lo, hi) = rdk_rays::geometry_view::bounds(&prepared.geometry).expect("bounds");
        if hi.x - lo.x < 1.5 {
            return Err(rdk_rays::surface::ConversionError(message.to_owned()));
        }
        rdk_rays::surface::scene3(None, None, &prepared.surface)
    });
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        current = editor::update(current.clone(), &tframe((450, 320), vec![], &[], &[], 0));
        assert!(current.context_error.as_deref().map(|s| s.contains(message)).unwrap_or(false), "draw-time conversion error was hidden");
        assert!(matches!(&current.resolved, Some((_, _, _, None))), "a draw error polluted the cached scene-resolution diagnostic");
        assert!(current.drawn.values().any(|d| d.error.as_deref() == Some(message)), "the failed drawing did not retain its conversion cause");
        current.rendered = None;
        current.context_error = None;
        current = editor::update(current.clone(), &tframe((450, 320), vec![], &[], &[], 1));
        current = editor::update(current.clone(), &tframe((450, 320), vec![], &[], &[], 2));
        assert_eq!(calls.get(), 1, "cached conversion error repeated the draw");
        assert!(current.context_error.as_deref().map(|s| s.contains(message)).unwrap_or(false), "cached drawing lost its conversion error");
        current = editor::edit(current.clone(), &Op_size()).expect("edit");
        current = editor::update(current.clone(), &tframe((450, 320), vec![], &[], &[], 3));
        assert!(calls.get() == 2 && current.context_error.is_none(), "a corrected prepared object kept its old conversion error");
    }));
    editor::close(&current);
    if let Err(p) = result {
        std::panic::resume_unwind(p);
    }
}

#[allow(non_snake_case)]
fn Op_size() -> flow_graph::flow_edit::Op {
    set_arg(&["geo", "box"], "size", "[2 2 2]")
}
