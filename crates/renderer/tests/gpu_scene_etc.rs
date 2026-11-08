//! One process, one shared Vulkan device (tests/common keeps a lease) for a slice of the renderer's cases.
//! The cargo-t runner runs the gpu_* targets in parallel and the window_* target alone.
#[path = "common/mod.rs"]
mod common;
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

fn main() {
    let groups: &[&[(&str, fn())]] = &[gpu_scene3_instances::CASES, gpu_scene2_argument::CASES, gpu_scene2_coordinates::CASES, gpu_shapes_lowering::CASES, gpu_ui_pipeline::CASES, ];
    let cases: Vec<(&str, fn())> = groups.concat().into_iter().filter(|_c| true).collect();
    common::window::run(&cases);
}
