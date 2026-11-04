//! The kit widgets, overlays and zone tokens compared pixel for pixel with the goldens of
//! lib/pxui/fixtures (read in place). One process, one shared Vulkan device.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "cases/gpu_parity.rs"]
mod gpu_parity;

fn main() {
    common::window::run(gpu_parity::CASES);
}
