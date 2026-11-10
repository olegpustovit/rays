//! Render pixel goldens using the native display driver and its actual density.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "cases/gpu_parity.rs"]
mod gpu_parity;

fn main() {
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    common::window::run(gpu_parity::CASES);
}
