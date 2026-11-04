//! test_2d_ports.ml: each 2D sketch loads to its own text, Command-S saves it unchanged, and its
//! drawing commands are identical at one and eight domains. The native PNG export modes
//! (Sketch.export_state vs Workspace.export) are SKIPPED: no Sketch.export_state counterpart here.
use super::t4::*;
use editor_document::workspace_doc as Doc;
use input::{Event, Key, MouseButton::LeftButton};
use rays_editor::{Editor, Source, Workspace};
use std::cell::RefCell;

const NAMES: [&str; 8] = ["basic", "generative", "noise", "recursive_rectangles", "drawing", "audio", "file_dialog", "pxui"];

fn dimensions(name: &str) -> (i64, i64) {
    match name {
        "generative" => (500, 500),
        "noise" => (800, 450),
        "recursive_rectangles" => (720, 720),
        "drawing" => (800, 500),
        "file_dialog" => (720, 420),
        _ => (640, 360),
    }
}

fn frame_of(synthetic: bool, name: &str, count: i64) -> rays::frame::t {
    let (width, height) = dimensions(name);
    let events = if !synthetic {
        vec![]
    } else {
        match (name, count) {
            ("drawing", 1) => vec![Event::Pointer_pressed(LeftButton, (100., 140.)), Event::Pointer_moved((120., 160.))],
            ("drawing", 2) => vec![Event::Pointer_moved((140., 180.))],
            ("drawing", 3) => vec![Event::Pointer_released(LeftButton, (160., 190.))],
            ("recursive_rectangles", 1) => vec![Event::Pointer_pressed(LeftButton, (100., 140.))],
            ("audio", 1) => vec![Event::Key_pressed(Key::KeyChar('3'))],
            ("audio", 3) => vec![Event::Pointer_pressed(LeftButton, (400., 180.))],
            ("file_dialog", 1) => vec![Event::File_dropped("sample.png".into()), Event::File_dialog { id: 1, result: Ok(vec!["a.png".into(), "b.png".into()]) }],
            ("file_dialog", 2) => vec![Event::File_drag_moved((90., 140.)), Event::Pinched(1.25)],
            ("file_dialog", 3) => vec![Event::File_drag_ended, Event::File_dialog { id: 2, result: Ok(vec![]) }],
            _ => vec![],
        }
    };
    let mouse = if !synthetic {
        (0., 0.)
    } else if name == "recursive_rectangles" {
        (100., 140.)
    } else {
        (-100., -100.)
    };
    rays::frame::t { input: input::Frame { dt: 1. / 60., frame: count, tick: count, size: (width, height), pointer: mouse, events, ..input::at_time(count as f64 / 60.) }, pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) }
}

fn sketch_text(name: &str) -> String {
    std::fs::read_to_string(format!("{}/../../../sketches/{name}.rays", env!("CARGO_MANIFEST_DIR"))).unwrap()
}

fn load(name: &str) -> Doc::T {
    let source = sketch_text(name);
    let ws = Workspace::load(None, vec![], None, &source).unwrap_or_else(|ds| panic!("{name}: {ds:?}"));
    assert!(Doc::to_text(&ws) == source, "{name}: load does not print back to its source");
    ws
}

fn save_roundtrip(name: &str) {
    let workspace = load(name);
    let source = sketch_text(name);
    let dir = temp_dir(&format!("port-save-{name}"));
    let file = format!("{dir}/sketch.rays");
    std::fs::write(&file, &source).unwrap();
    let o = Editor::Create { ui_text_size: Some(11), platform: Some(input::Platform::Apple), await_: Some(true), domains: Some(1), presets: Some(format!("{dir}/presets")), ..Default::default() };
    let ed = Editor::create(o, &workspace, Some(Source::at(&file, &editor_document::lowering::sha256(&source))), Editor::prepare_fn(), Editor::draw_fn()).unwrap();
    let mut ed = Editor::update(ed, &frame_of(false, name, 0));
    let mut save = frame_of(false, name, 1);
    save.input.keys = vec![Key::Meta];
    save.input.events = vec![Event::Key_pressed(Key::KeyChar('s'))];
    ed = Editor::update(ed, &save);
    ed = Editor::update(ed, &frame_of(false, name, 2));
    // (the OCaml inode check is POSIX-only: an atomic replace keeps the text and comments)
    assert!(std::fs::read_to_string(&file).unwrap() == source, "{name}: Command-S changed the source file");
    assert!(Doc::to_text(&Editor::workspace(&ed)) == source, "{name}: the document changed on save");
    Editor::close(&ed);
    let _ = std::fs::remove_dir_all(&dir);
}


fn serialize(scene: &rays::scene::t) -> Vec<u8> {
    let commands = rays::scene::Layer::commands(None, scene).unwrap();
    // ponytail: scene_command is not a dependency of rays_editor; the commands compare by their Debug text
    format!("{commands:?}").into_bytes()
}

fn pictures(name: &str, workspace: &Doc::T, domains: i64, count: i64) -> Vec<Vec<u8>> {
    let out = RefCell::new(Vec::new());
    parallel::run_unit(Some(domains), &|| {
        let evaluated = flow_eval::r#static(&workspace.checked, false, Some(&workspace.inputs)).map_err(|d| rays_diagnostic::to_string(&d))?;
        let root = evaluated.results.iter().find(|(n, _)| n == "picture").map(|(_, v)| v.clone()).expect("a picture graph");
        let prepared = flow_native::drawing::prepare(&flow_native::drawing::PrepareOptions { states: evaluated.states.clone(), ..Default::default() }, &evaluated.plan, &root).map_err(|d| rays_diagnostic::to_string(&d))?;
        let state = flow_eval::create_state();
        let mut scenes = Vec::new();
        for c in 0..count {
            let f = frame_of(true, name, c);
            let live = flow_native::live_frame::of_frame(&f);
            let scene = flow_native::drawing::render_prepared(&prepared, &live, f.input.size, &flow_native::drawing::RenderOptions { state: Some(state.clone()), ..Default::default() }).map_err(|d| rays_diagnostic::to_string(&d))?;
            scenes.push(serialize(&scene));
        }
        *out.borrow_mut() = scenes;
        Ok(())
    })
    .unwrap_or_else(|m| panic!("{name}: {m}"));
    out.into_inner()
}

/// One text cache for both runs, as Sketch.run_state installs: a fresh cache re-rasterizes text under a new image identity.
fn parity(name: &str, workspace: &Doc::T) -> Vec<Vec<u8>> {
    let fonts = rays::font::Raster::create_cache_context();
    let mut result = Vec::new();
    rays::font::Raster::with_cache_context(&fonts, &mut || {
        let expected = pictures(name, workspace, 1, 4);
        let eight = pictures(name, workspace, 8, 4);
        assert!(eight == expected, "{name}: 1/8-domain command parity");
        result = expected;
    });
    rays::font::Raster::clear_cache_context(&fonts);
    result
}


pub fn moving_circles() {
    if !super::common::gpu() {
        return;
    }
    let text = r##"(workspace moving
 (graph picture :context draw (let* [phase (state [p 0.0] (+ p (frame/dt)))
  positions (map (fn [i] [(+ (mod i 640) (* 5 (sin phase))) (mod (int/div i 640) 360) 0]) (array/range 10000))]
  (draw/merge (draw/background "#111827") (draw/circles positions :radius 2.0 :fill "#22d3ee"))))
 (graph window :context settings (settings/config :width 640 :height 360)))"##;
    let workspace = Workspace::load(None, vec![], None, text).unwrap_or_else(|ds| panic!("{ds:?}"));
    let expected = parity("moving_circles", &workspace);
    assert!(expected[0] != expected[3], "the circles do not move");
}

macro_rules! per_sketch {
    ($($id:ident => $name:expr),*) => {
        $(pub mod $id {
            pub fn save() {
                if !super::super::common::gpu() {
                    return;
                }
                super::save_roundtrip($name);
            }
            pub fn parity() {
                if !super::super::common::gpu() {
                    return;
                }
                super::parity($name, &super::load($name));
            }
        })*
    };
}
per_sketch!(basic => "basic", generative => "generative", noise => "noise", recursive_rectangles => "recursive_rectangles", drawing => "drawing", audio => "audio", file_dialog => "file_dialog", pxui => "pxui");
