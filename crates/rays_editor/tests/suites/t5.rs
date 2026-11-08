//! Editor suite t5: text pane, carry, scene sync.
#[path = "../suite_t5/common.rs"]
mod t5c;
#[path = "../suite_t5/textpane.rs"]
mod textpane;
#[path = "../suite_t5/carry.rs"]
mod carry_t;
#[path = "../suite_t5/scene_sync.rs"]
mod scene_sync_t;

macro_rules! gpu_case {
    ($name:ident, $path:path) => {
        fn $name() {
            if !crate::common::gpu() {
                return;
            }
            $path();
        }
    };
}

gpu_case!(text_editor_text, textpane::editor_text);
gpu_case!(text_editor_binding, textpane::editor_binding);
gpu_case!(text_editor_w9, textpane::editor_w9);
gpu_case!(text_active_scrub, textpane::editor_active_scrub);
gpu_case!(text_literal_scrub, textpane::editor_literal_scrub);
gpu_case!(text_text_drop, textpane::editor_text_drop);
gpu_case!(text_command_click, textpane::editor_command_click);
gpu_case!(text_text_find, textpane::editor_text_find);
gpu_case!(scene_sync_add_by_key, scene_sync_t::add_by_key);
gpu_case!(materials_carry, carry_t::carry_tests);
gpu_case!(materials_viewport_carry, carry_t::viewport_carry_tests);

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
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
    ]
}
