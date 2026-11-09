mod common;
#[path = "cases/gpu_marks_native.rs"]
mod gpu_marks_native;
#[path = "cases/gpu_offscreen.rs"]
mod gpu_offscreen;
#[path = "cases/gpu_dense_scene2.rs"]
mod gpu_dense_scene2;
#[path = "cases/gpu_draw.rs"]
mod gpu_draw;
#[path = "cases/gpu_probe.rs"]
mod gpu_probe;
#[path = "cases/gpu_batch_merge.rs"]
mod gpu_batch_merge;
#[path = "cases/gpu_scene3_instances.rs"]
mod gpu_scene3_instances;
#[path = "cases/gpu_scene2_argument.rs"]
mod gpu_scene2_argument;
#[path = "cases/gpu_scene2_coordinates.rs"]
mod gpu_scene2_coordinates;
#[path = "cases/gpu_shapes_lowering.rs"]
mod gpu_shapes_lowering;
#[path = "cases/gpu_ui_pipeline.rs"]
mod gpu_ui_pipeline;
#[path = "cases/scene2_argument_body/mod.rs"]
mod scene2_argument_body;
#[path = "cases/window_runtime_render.rs"]
mod window_runtime_render;
#[path = "cases/window_scene2_argument.rs"]
mod window_scene2_argument;
#[path = "cases/window_scene3_lighting.rs"]
mod window_scene3_lighting;

fn main() {
    let groups: &[&[(&str, fn())]] = &[
        gpu_marks_native::CASES,
        gpu_offscreen::CASES,
        gpu_dense_scene2::CASES,
        gpu_draw::CASES,
        gpu_probe::CASES,
        gpu_batch_merge::CASES,
        gpu_scene3_instances::CASES,
        gpu_scene2_argument::CASES,
        gpu_scene2_coordinates::CASES,
        gpu_shapes_lowering::CASES,
        gpu_ui_pipeline::CASES,
        window_runtime_render::CASES,
        window_scene2_argument::CASES,
        window_scene3_lighting::CASES,
    ];
    let mut cases = groups.concat();
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") {
        cases.extend_from_slice(gpu_offscreen::QUALIFICATION_CASES);
    }
    common::window::run(&cases);
}
