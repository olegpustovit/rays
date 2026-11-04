//! One process, one shared Vulkan device (tests/common keeps a lease) for a slice of the renderer's cases.
//! The cargo-t runner runs the gpu_* targets in parallel and the window_* target alone.
#[path = "common/mod.rs"]
mod common;
#[path = "cases/window_runtime_render.rs"]
mod window_runtime_render;
#[path = "cases/window_scene2_argument.rs"]
mod window_scene2_argument;
#[path = "cases/window_scene3_lighting.rs"]
mod window_scene3_lighting;
#[path = "cases/scene2_argument_body/mod.rs"]
mod scene2_argument_body;

fn main() {
    let groups: &[&[(&str, fn())]] = &[window_runtime_render::CASES, window_scene2_argument::CASES, window_scene3_lighting::CASES, ];
    let cases: Vec<(&str, fn())> = groups.concat().into_iter().filter(|c| true).collect();
    common::window::run(&cases);
}
