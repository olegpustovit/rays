//! Ported OCaml editor tests, group t5: the editor-driven sections of test_text_pane.ml, the carry tests of
//! test_materials.ml and the editor-driven part of test_scene_sync.ml.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "suite_t5/common.rs"]
mod t5c;
#[path = "suite_t5/textpane.rs"]
mod textpane;
#[path = "suite_t5/carry.rs"]
mod carry_t;
#[path = "suite_t5/scene_sync.rs"]
mod scene_sync_t;

macro_rules! gpu_case {
    ($name:ident, $path:path) => {
        fn $name() {
            if !common::gpu() {
                return;
            }
            $path();
        }
    };
}

/// Skipped unless `RAYS_SLOW=1`, with the reason printed.
#[allow(unused_macros)]
macro_rules! slow_case {
    ($name:ident, $path:path, $reason:expr) => {
        fn $name() {
            if std::env::var("RAYS_SLOW").map_or(true, |v| v != "1") {
                println!("SKIPPED {}: {}", stringify!($name), $reason);
                return;
            }
            if !common::gpu() {
                return;
            }
            $path();
        }
    };
}

slow_case!(text_editor_text, textpane::editor_text, "22 s in a debug build (every edit re-lowers the sunflower: perf3); passes under RAYS_SLOW=1");
slow_case!(text_editor_binding, textpane::editor_binding, "7 s in a debug build (edits re-lower the sunflower: perf3); passes under RAYS_SLOW=1");
slow_case!(text_editor_w9, textpane::editor_w9, "131 s in a debug build (eight scenarios of 19 frames over the rosette: perf3); passes under RAYS_SLOW=1");
gpu_case!(text_active_scrub, textpane::editor_active_scrub);
gpu_case!(text_literal_scrub, textpane::editor_literal_scrub);
slow_case!(text_text_drop, textpane::editor_text_drop, "9.7 s in a debug build (graph jumps and cooks); passes under RAYS_SLOW=1");
gpu_case!(text_command_click, textpane::editor_command_click);
gpu_case!(text_text_find, textpane::editor_text_find);
gpu_case!(scene_sync_add_by_key, scene_sync_t::add_by_key);
slow_case!(materials_carry, carry_t::carry_tests, "21 s in a debug build (about 60 editor frames with preview re-cooks: perf3); passes under RAYS_SLOW=1");
slow_case!(materials_viewport_carry, carry_t::viewport_carry_tests, "5.8 s in a debug build (surface-cooking editor, 25 frames); passes under RAYS_SLOW=1");

fn main() {
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let cases: Vec<(&str, fn())> = vec![
        ("text_editor_text", text_editor_text),
        ("text_editor_binding", text_editor_binding),
        ("text_editor_w9", text_editor_w9),
        ("text_active_scrub", text_active_scrub),
        ("text_literal_scrub", text_literal_scrub),
        ("text_text_drop", text_text_drop),
        ("text_command_click", text_command_click),
        ("text_text_find", text_text_find),
        ("scene_sync_add_by_key", scene_sync_add_by_key),
        ("materials_carry", materials_carry),
        ("materials_viewport_carry", materials_viewport_carry),
    ];
    common::window::run(&cases);
}
