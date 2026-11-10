//! Pixel goldens rendered headless into an offscreen texture: no SDL, no window, any display.
#[path = "cases/gpu.rs"]
mod gpu;
#[path = "cases/gpu_parity.rs"]
mod gpu_parity;
#[path = "../../pxui_render/tests/png.rs"]
mod png;
#[path = "cases/run.rs"]
mod run;
#[path = "cases/support.rs"]
#[allow(dead_code)]
mod support;

fn main() {
    run::run(gpu_parity::CASES);
}
