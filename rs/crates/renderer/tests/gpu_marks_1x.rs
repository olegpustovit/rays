//! One process, one shared Vulkan device (tests/common keeps a lease) for a slice of the renderer's cases.
//! The cargo-t runner runs the gpu_* targets in parallel and the window_* target alone.
#[path = "common/mod.rs"]
mod common;
#[path = "cases/gpu_marks_native.rs"]
mod gpu_marks_native;

fn main() {
    let groups: &[&[(&str, fn())]] = &[gpu_marks_native::CASES, ];
    let cases: Vec<(&str, fn())> = groups.concat().into_iter().filter(|c| c.0.ends_with("_1x")).collect();
    common::window::run(&cases);
}
