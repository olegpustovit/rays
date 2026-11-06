//! One process, one shared Vulkan device (tests/common keeps a lease) for the offscreen-GPU cases
//! of lib/rays. The cargo-t runner runs the gpu_* targets in parallel and window_* targets alone.
#[path = "common/mod.rs"]
mod common;
#[path = "cases/gpu_rays.rs"]
mod gpu_rays;

fn main() {
    common::window::run(gpu_rays::CASES);
}
