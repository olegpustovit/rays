//! Ported OCaml editor tests, group t3 (see docs/history/rays_editor_plan.md): test_editor_commands,
//! test_editor_cook, test_probe, test_workspace_live, test_viewport_pick, test_sop_ui,
//! test_packed_pieces, test_drawing, test_materials and the editor-driven sections of
//! test_text_pane. Cases run on the main thread via the shared window runner.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "suite_t3/common.rs"]
mod t3c;
#[path = "suite_t3/commands.rs"]
mod commands;
#[path = "suite_t3/support.rs"]
mod support;
#[path = "suite_t3/cook.rs"]
mod cook_t;
#[path = "../../pxui_graph/tests/cases/support.rs"]
mod pg;
#[path = "suite_t3/probe.rs"]
mod probe_t;
#[path = "suite_t3/live.rs"]
mod live_t;
#[path = "suite_t3/pick.rs"]
mod pick_t;
#[path = "suite_t3/sop_ui.rs"]
mod sop_ui_t;
#[path = "suite_t3/packed.rs"]
mod packed_t;
#[path = "suite_t3/drawing.rs"]
mod drawing_t;
#[path = "suite_t3/materials.rs"]
mod materials_t;
#[path = "suite_t3/textpane.rs"]
mod textpane_t;

/// A case that needs the device: the lease is taken inside the case (see the plan).
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


gpu_case!(commands_canvas_preview, commands::canvas_preview);
gpu_case!(commands_canvas_frame_size, commands::canvas_frame_size);
gpu_case!(commands_menu_forms, commands::menu_forms);
gpu_case!(commands_world_command, commands::world_command);
gpu_case!(commands_world_drag, commands::world_drag);
gpu_case!(commands_image_cook, commands::image_cook);
gpu_case!(commands_new_graphs, commands::new_graphs);
gpu_case!(commands_keys_and_guide, commands::keys_and_guide);
gpu_case!(commands_guide_preference, commands::guide_preference);

gpu_case!(cook_consumers, cook_t::consumers);
gpu_case!(cook_reuse, cook_t::reuse);
gpu_case!(cook_context_provenance, cook_t::context_provenance);
gpu_case!(cook_live_drive, cook_t::live_drive);
gpu_case!(cook_barriers, cook_t::barriers);
gpu_case!(cook_close_joins, cook_t::close_joins);

gpu_case!(probe_off_display, probe_t::off_display);
gpu_case!(probe_budget, probe_t::budget);
gpu_case!(probe_compiled_inputs, probe_t::compiled_inputs);
gpu_case!(probe_live, probe_t::live);
gpu_case!(probe_branches_garland, probe_t::branches_garland);

gpu_case!(live_value_lanes, live_t::value_lanes);
gpu_case!(live_equals_static_sunflower, live_t::live_equals_static_sunflower);
gpu_case!(live_equals_static_orrery, live_t::live_equals_static_orrery);
gpu_case!(live_orrery_playback, live_t::orrery_playback);
gpu_case!(live_orrery_fixed_step, live_t::orrery_fixed_step);
gpu_case!(live_realtime_edit, live_t::realtime_edit);
gpu_case!(live_editor_cook_path, live_t::editor_cook_path);

gpu_case!(pick_sunflower, pick_t::pick_sunflower);
gpu_case!(pick_bloom, pick_t::pick_bloom);
gpu_case!(pick_flat_boxes, pick_t::flat_boxes);
gpu_case!(pick_flat_loops, pick_t::flat_loops);

gpu_case!(packed_exploded_view, packed_t::exploded_view);
gpu_case!(packed_boolean_fracture, packed_t::boolean_fracture);
gpu_case!(materials_face_materials, materials_t::face_materials_preserve_explosion);
gpu_case!(materials_refs, materials_t::material_refs);
gpu_case!(materials_follow, materials_t::follow_tests);
gpu_case!(textpane_graph_drafts, textpane_t::graph_drafts);
gpu_case!(textpane_fn_frames, textpane_t::fn_frames);

fn main() {
    // Headless, as crates/rays_editor/dune sets SDL_VIDEODRIVER=dummy; the hint precedes SDL init.
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let cases: Vec<(&str, fn())> = vec![
        ("commands_canvas_preview", commands_canvas_preview),
        ("commands_canvas_frame_size", commands_canvas_frame_size),
        ("commands_menu_forms", commands_menu_forms),
        ("commands_world_command", commands_world_command),
        ("commands_world_drag", commands_world_drag),
        ("commands_image_cook", commands_image_cook),
        ("commands_new_graphs", commands_new_graphs),
        ("commands_keys_and_guide", commands_keys_and_guide),
        ("commands_guide_preference", commands_guide_preference),
        ("cook_consumers", cook_consumers),
        ("cook_reuse", cook_reuse),
        ("cook_context_provenance", cook_context_provenance),
        ("cook_live_drive", cook_live_drive),
        ("cook_barriers", cook_barriers),
        ("cook_close_joins", cook_close_joins),
        ("probe_bounds", probe_t::bounds),
        ("probe_sunflower", probe_t::sunflower),
        ("probe_tree", probe_t::tree),
        ("probe_branches_tiles", probe_t::branches_tiles),
        ("probe_branches_garland", probe_branches_garland),
        ("probe_live", probe_live),
        ("probe_nested", probe_t::nested),
        ("probe_hoist", probe_t::hoist),
        ("probe_selection", probe_t::selection),
        ("probe_geometry_zone", probe_t::geometry_zone),
        ("probe_off_display", probe_off_display),
        ("probe_skips", probe_t::skips),
        ("probe_budget", probe_budget),
        ("probe_compiled_inputs", probe_compiled_inputs),
        ("probe_run", probe_t::run),
        ("live_value_lanes", live_value_lanes),
        ("live_equals_static_sunflower", live_equals_static_sunflower),
        ("live_equals_static_orrery", live_equals_static_orrery),
        ("live_orrery_playback", live_orrery_playback),
        ("live_orrery_fixed_step", live_orrery_fixed_step),
        ("live_realtime_edit", live_realtime_edit),
        ("live_editor_cook_path", live_editor_cook_path),
        ("pick_sunflower", pick_sunflower),
        ("pick_bloom", pick_bloom),
        ("pick_flat_boxes", pick_flat_boxes),
        ("pick_flat_loops", pick_flat_loops),
        ("sop_ui_reset_context_symbol", sop_ui_t::reset_context_symbol),
        ("sop_ui_wire_color", sop_ui_t::wire_color),
        ("sop_ui_inspector_rows", sop_ui_t::inspector_rows),
        ("sop_ui_flow_rows", sop_ui_t::flow_rows),
        ("packed_exploded_view", packed_exploded_view),
        ("packed_boolean_fracture", packed_boolean_fracture),
        ("drawing_all_draws_and_errors", drawing_t::all_draws_and_errors),
        ("drawing_particles", drawing_t::particles),
        ("drawing_editable_folds", drawing_t::editable_folds),
        ("drawing_host_inputs", drawing_t::host_inputs),
        ("materials_face_materials", materials_face_materials),
        ("materials_refs", materials_refs),
        ("materials_follow", materials_follow),
        ("textpane_graph_drafts", textpane_graph_drafts),
        ("textpane_fn_frames", textpane_fn_frames),
    ];
    common::window::run(&cases);
}
