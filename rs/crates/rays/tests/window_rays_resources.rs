//! SDL main-thread cases of lib/rays that need no GPU (images, fonts, display-list images).
//! The cargo-t runner runs window_* targets alone.
#[path = "common/mod.rs"]
mod common;
#[path = "cases/window_resources.rs"]
mod window_resources;

fn main() {
    common::window::run(window_resources::CASES);
}
