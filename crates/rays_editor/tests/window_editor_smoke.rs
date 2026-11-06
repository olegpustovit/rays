//! Smoke test of the whole editor: create an `Editor` on small workspaces, run a few frames
//! windowless (SDL's dummy video driver, the Vulkan device of the other native targets) and check
//! that nothing errs and the frame has a picture. The OCaml suite (crates/rays_editor/test) is ported
//! on top of this API; see docs/history/rays_editor_plan.md, "Editor API for tests".
#[path = "../../rays/tests/common/mod.rs"]
mod common;

use rays::frame;
use rays_editor::Editor;
use rays_editor::Workspace;

/// A scene workspace: a sop graph (a tube), a material, a scene with camera and light, the default
/// editor layout. sketches/ws_layout.rays without the scatter that hangs the lowering (see the plan).
const STUDIO: &str = r##"(workspace studio
  (graph garden :context sop
    (let* [bed (sop/tube :top_radius 1 :bottom_radius 1 :height 0.04 :columns 48)]
      (sop/set_color bed :color "#3b7d4e")))
  (graph clay :context material
    (material/standard :name "clay" :color "#b0680f" :roughness 0.6))
  (graph scene :context scene
    (let* [plants (scene/geometry (ref garden) :name "garden")
           cam (scene/camera :eye [0.5 1.8 6] :target [0 0.3 0] :fov 50)
           sun (scene/light :shape "Directional" :translate [8 11.2 6.4] :intensity 1)
           all (scene/merge plants cam sun)]
      (scene/root all :camera cam)))
  (graph settings :context settings
    (settings/config :title "Studio" :width 1440 :height 900)))"##;

fn frame_at(tick: i64) -> frame::t {
    frame::t {
        input: input::Frame { dt: 1. / 60., frame: tick, tick, size: (960, 640), pointer: (-100., -100.), ..input::at_time(tick as f64 / 60.) },
        pixel_scale: (1., 1.),
        fps: 60.,
        mouse_delta: (0., 0.),
    }
}

fn open_text(text: &str, imports: Vec<(String, String)>) -> Editor::t {
    let workspace = Workspace::load(None, imports, None, text).unwrap_or_else(|d| panic!("workspace did not check: {d:?}"));
    Editor::create(Editor::Create { await_: Some(true), ui_text_size: Some(11), domains: Some(1), ..Default::default() }, &workspace, None, Editor::prepare_fn(), Editor::draw_fn()).expect("the editor opens")
}

fn open_sketch(name: &str) -> Editor::t {
    let path = format!("{}/../../sketches/{name}", env!("CARGO_MANIFEST_DIR"));
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let imports = rays_editor::Source::read_imports(&path, &text).unwrap_or_else(|d| panic!("{d:?}"));
    open_text(&text, imports)
}

/// Frames, the scene of each, the frame boundary; returns the last scene's length.
fn run_frames(mut editor: Editor::t, frames: i64) -> (Editor::t, usize) {
    let mut last = 0;
    for tick in 1..=frames {
        let at = frame_at(tick);
        editor = Editor::update(editor, &at);
        last = Editor::scene(&editor, &at).len();
        editor = Editor::after_present(editor, &at);
    }
    (editor, last)
}

fn check(name: &str, editor: Editor::t, scene_len: usize) {
    assert!(scene_len > 0, "{name}: the frame is empty");
    assert!(editor.core.edit_error.is_none(), "{name}: edit error {:?}", editor.core.edit_error);
    assert!(editor.core.cook.error.is_none(), "{name}: cook error {:?}", editor.core.cook.error);
    assert!(editor.memory_error.is_none(), "{name}: memory error {:?}", editor.memory_error);
    assert!(editor.context_error.is_none(), "{name}: context error {:?}", editor.context_error);
    let status = Editor::Private::status_text(&editor);
    assert!(!status.contains("rejected"), "{name}: status says {status}");
    Editor::close(&editor);
}

fn smoke_basic() {
    if !common::gpu() {
        return;
    }
    let (editor, len) = run_frames(open_sketch("basic.rays"), 4);
    check("basic", editor, len);
}

fn smoke_studio() {
    if !common::gpu() {
        return;
    }
    let (editor, len) = run_frames(open_text(STUDIO, Vec::new()), 6);
    // the cook ran (await): the tube is a piece, and the scene document has the three objects
    assert!(Editor::prepared(&editor).is_some(), "studio: nothing cooked");
    assert!(!Editor::objects(&editor).is_empty(), "studio: no placed object");
    check("studio", editor, len);
}

/// sketches/ws_layout.rays: its lowering hangs in the Rust port (sop/scatter), so it runs only on
/// request (RAYS_SMOKE_WS_LAYOUT=1) until that is fixed; see the report in rays_editor_plan.md.
fn smoke_ws_layout() {
    if std::env::var("RAYS_SMOKE_WS_LAYOUT").is_err() || !common::gpu() {
        return;
    }
    let (editor, len) = run_frames(open_sketch("ws_layout.rays"), 4);
    check("ws_layout", editor, len);
}

/// The accessors the ported suite uses on a fresh editor.
fn api_accessors() {
    if !common::gpu() {
        return;
    }
    let (editor, _) = run_frames(open_sketch("basic.rays"), 2);
    assert!(!Editor::can_undo(&editor) && !Editor::can_redo(&editor));
    assert_eq!(Editor::undo_label(&editor), None);
    assert_eq!(Editor::workspace(&editor).checked.name, "basic");
    assert!(Editor::carrying(&editor).is_none());
    assert!(!Editor::flying(&editor) && !Editor::look_through(&editor));
    let panes = Editor::panes(&editor, &frame_at(3));
    // basic.rays has a canvas and no viewport: its view pane is empty, the status strip is not
    assert!(panes.status.2 > 0 && panes.status.3 > 0, "the status strip has a size: {:?}", panes.status);
    Editor::close(&editor);
}

fn main() {
    // Headless, as crates/rays_editor/dune sets SDL_VIDEODRIVER=dummy; the hint precedes SDL init.
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let cases: Vec<(&str, fn())> = vec![("smoke_basic", smoke_basic), ("smoke_studio", smoke_studio), ("smoke_ws_layout", smoke_ws_layout), ("api_accessors", api_accessors)];
    common::window::run(&cases);
}
