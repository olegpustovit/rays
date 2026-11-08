//! Editor suite t4: document, input, transactions, source, ports, fast path, …
#[path = "../suite_t4/mod.rs"]
mod t4;
#[path = "../suite_t4/test_document.rs"]
mod test_document;
#[path = "../suite_t4/test_input.rs"]
mod test_input;
#[path = "../suite_t4/test_transactions.rs"]
mod test_transactions;
#[path = "../suite_t4/test_rays_editor.rs"]
mod test_rays_editor;
#[path = "../suite_t4/test_source.rs"]
mod test_source;
#[path = "../suite_t4/test_small.rs"]
mod test_small;
#[path = "../suite_t4/test_history.rs"]
mod test_history;
#[path = "../suite_t4/test_ports.rs"]
mod test_ports;
#[path = "../suite_t4/test_fast.rs"]
mod test_fast;

pub fn cases() -> Vec<(&'static str, fn())> {
    let mut cases: Vec<(&str, fn())> = vec![
        ("editor_document", test_document::run),
        ("editor_transactions", test_transactions::run),
        ("rays_editor_layout_panes", test_rays_editor::layout_panes),
        ("rays_editor_sketch_settings", test_rays_editor::sketch_settings),
        ("source_files", test_source::files),
        ("source_find", test_source::find),
        ("source_external_over_dirty", test_source::external_over_dirty),
        ("source_start_keywords", test_source::start_keywords),
        ("source_imports", test_source::imports),
        ("source_library", test_source::library),
        ("source_embedded_nodes", test_source::embedded_nodes),
        ("source_library_navigation", test_source::library_navigation),
        ("source_vocabulary_refresh", test_source::vocabulary_refresh),
        ("history_undo_relower", test_history::history),
        ("history_budget", test_history::budget),
        ("ports_basic_save", test_ports::basic::save),
        ("ports_generative_save", test_ports::generative::save),
        ("ports_pxui_save", test_ports::pxui::save),
        ("ports_basic_parity", test_ports::basic::parity),
        ("ports_generative_parity", test_ports::generative::parity),
        ("ports_pxui_parity", test_ports::pxui::parity),
        ("ports_moving_circles", test_ports::moving_circles),
        ("host_effects_export", test_small::host_effects_export),
        ("host_effects_edge", test_small::host_effects_edge),
        ("threading", test_small::threading),
        ("workspace_start_errors", test_small::workspace_start_errors),
    ];
    cases.extend::<Vec<(&str, fn())>>(vec![
        ("editor_input", test_input::run),
        ("rays_editor_camera_nodes", test_rays_editor::camera_nodes),
        ("rays_editor_preset_look_through", test_rays_editor::preset_and_look_through),
        ("rays_editor_workspace_walk", test_rays_editor::workspace_walk),
        ("source_editor", test_source::editor_flow),
        ("source_autosave", test_source::autosave),
        ("ports_noise_save", test_ports::noise::save),
        ("ports_recursive_rectangles_save", test_ports::recursive_rectangles::save),
        ("ports_drawing_save", test_ports::drawing::save),
        ("ports_audio_save", test_ports::audio::save),
        ("ports_file_dialog_save", test_ports::file_dialog::save),
        ("ports_noise_parity", test_ports::noise::parity),
        ("ports_recursive_rectangles_parity", test_ports::recursive_rectangles::parity),
        ("ports_drawing_parity", test_ports::drawing::parity),
        ("ports_audio_parity", test_ports::audio::parity),
        ("ports_file_dialog_parity", test_ports::file_dialog::parity),
    ]);
    cases.push(("fast_path_cameras", test_fast::cameras));
    cases
}
