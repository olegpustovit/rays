//! Graph pane and node-menu cases run on the SDL main thread and read shared specification fixtures directly.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "cases/support.rs"]
mod support;
#[path = "cases/menu.rs"]
mod menu;
#[path = "cases/scope_a.rs"]
mod scope_a;
#[path = "cases/scope_b.rs"]
mod scope_b;
#[path = "cases/scope_c.rs"]
mod scope_c;
#[path = "cases/scope_d.rs"]
mod scope_d;
#[path = "cases/scope_e.rs"]
mod scope_e;

fn main() {
    // Headless, as the pxui window targets: the hint has to precede SDL initialisation.
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend_from_slice(menu::CASES);
    cases.extend_from_slice(scope_a::CASES);
    cases.extend_from_slice(scope_b::CASES);
    cases.extend_from_slice(scope_c::CASES);
    cases.extend_from_slice(scope_d::CASES);
    cases.extend_from_slice(scope_e::CASES);
    common::window::run(&cases);
}
