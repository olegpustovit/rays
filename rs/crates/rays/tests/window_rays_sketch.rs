//! Opens a native window and runs `Sketch.run_state` for a few frames. The cargo-t runner runs
//! window_* targets alone.
#[path = "common/mod.rs"]
mod common;
#[path = "cases/window_sketch.rs"]
mod window_sketch;

fn main() {
    common::window::run(window_sketch::CASES);
}
