//! One process, one shared Vulkan device (tests/common keeps a lease) for a slice of the renderer's cases.
//! The cargo-t runner runs the gpu_* targets in parallel and the window_* target alone.
#[path = "common/mod.rs"]
mod common;
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

fn main() {
    let groups: &[&[(&str, fn())]] = &[gpu_offscreen::CASES, gpu_dense_scene2::CASES, gpu_draw::CASES, gpu_probe::CASES, gpu_batch_merge::CASES, ];
    let cases: Vec<(&str, fn())> = groups.concat().into_iter().filter(|c| true).collect();
    common::window::run(&cases);
}
