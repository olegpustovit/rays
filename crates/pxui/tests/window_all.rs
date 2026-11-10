//! Run kit interaction cases on the SDL main thread so native fonts and input follow production ownership.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "cases/gpu_parity.rs"]
mod gpu_parity;
#[path = "cases/small.rs"]
mod small;
#[path = "cases/steps.rs"]
mod steps;
#[path = "cases/support.rs"]
mod support;
#[path = "cases/ui_a.rs"]
mod ui_a;
#[path = "cases/ui_b.rs"]
mod ui_b;
#[path = "cases/ui_c.rs"]
mod ui_c;

fn main() {
    // Headless, as crates/pxui/dune sets SDL_VIDEODRIVER=dummy: the clipboard cases must not reach the
    // shared OS clipboard. The hint has to precede SDL initialisation.
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend_from_slice(small::CASES);
    cases.extend_from_slice(ui_a::CASES);
    cases.extend_from_slice(ui_b::CASES);
    cases.extend_from_slice(ui_c::CASES);
    cases.extend_from_slice(gpu_parity::CASES);
    common::window::run(&cases);
}
