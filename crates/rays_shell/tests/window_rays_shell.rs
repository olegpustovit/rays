//! Editor chrome interactions run on the SDL main thread; stage_native packet counts validate painted strip geometry.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "cases/blur.rs"]
mod blur;
#[path = "cases/host.rs"]
mod host;
#[path = "cases/shell_a.rs"]
mod shell_a;
#[path = "cases/shell_b.rs"]
mod shell_b;

fn main() {
    pxui::ui::Ui::verify_new_views(cfg!(debug_assertions));
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend_from_slice(blur::CASES);
    cases.extend_from_slice(host::CASES);
    cases.extend_from_slice(shell_a::CASES);
    cases.extend_from_slice(shell_b::CASES);
    common::window::run(&cases);
}
