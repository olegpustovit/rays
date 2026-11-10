//! Editor suite t1:
mod support {
    pub use crate::support::shell::*;
}
#[path = "../suite_t1/part1.rs"]
pub mod part1;
#[path = "../suite_t1/part2.rs"]
pub mod part2;
#[path = "../suite_t1/part3.rs"]
pub mod part3;
#[path = "../suite_t1/part4.rs"]
pub mod part4;
#[path = "../suite_t1/part5.rs"]
pub mod part5;
#[path = "../suite_t1/part6.rs"]
pub mod part6;
#[path = "../suite_t1/part7.rs"]
pub mod part7;
#[path = "../suite_t1/part8.rs"]
pub mod part8;
#[path = "../suite_t1/part9.rs"]
pub mod part9;

macro_rules! cases {
    ($($name:ident => $path:path;)*) => {
        pub fn cases() -> Vec<(&'static str, fn())> {
            vec![$((stringify!($name), $path as fn())),*]
        }
    };
}

cases! {
    run_lowering => part1::run_lowering;
    run_editor => part1::run_editor;
    run_frame_key => part1::run_frame_key;
    run_panel_keys => part1::run_panel_keys;
    run_unbound_panels => part1::run_unbound_panels;
    run_wire_gestures => part1::run_wire_gestures;
    run_values => part2::run_values;
    run_duplicate_and_view => part2::run_duplicate_and_view;
    run_loop_view => part2::run_loop_view;
    run_result_view => part2::run_result_view;
    run_geometry_view => part2::run_geometry_view;
    run_movers => part2::run_movers;
    run_ops => part3::run_ops;
    run_restore => part3::run_restore;
    run_camera_zoom => part3::run_camera_zoom;
    run_cameras => part3::run_cameras;
    run_instances => part3::run_instances;
    run_views => part3::run_views;
    run_loop_copies => part4::run_loop_copies;
    run_loop_expression => part4::run_loop_expression;
    run_panel_states => part4::run_panel_states;
    run_ref_picker => part4::run_ref_picker;
    run_compose => part4::run_compose;
    run_layouts => part4::run_layouts;
    run_root_section => part5::run_root_section;
    run_idle_follow => part5::run_idle_follow;
    run_hide_and_order => part5::run_hide_and_order;
    run_copy_lisp => part5::run_copy_lisp;
    run_studio => part5::run_studio;
    run_studio_keywords => part6::run_studio_keywords;
    run_studio_tail => part6::run_studio_tail;
    run_panels => part6::run_panels;
    run_host_scene_edit => part6::run_host_scene_edit;
    run_panel_chain => part6::run_panel_chain;
    run_undo_and_input => part7::run_undo_and_input;
    run_carry_reaches_no_history => part7::run_carry_reaches_no_history;
    run_atomic_frame => part7::run_atomic_frame;
    run_undo_under_pane => part7::run_undo_under_pane;
    run_spreadsheet => part7::run_spreadsheet;
    run_op_inspector => part7::run_op_inspector;
    run_nested_inspector => part7::run_nested_inspector;
    run_expression_chip => part8::run_expression_chip;
    run_context_menus => part8::run_context_menus;
    run_lisp_inspector => part8::run_lisp_inspector;
    run_array_capacity_menu => part8::run_array_capacity_menu;
    run_open_defn => part8::run_open_defn;
    run_outline_button => part8::run_outline_button;
    run_idle_rest => part8::run_idle_rest;
    run_roots => part9::run_roots;
}
