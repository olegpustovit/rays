//! The interaction contract of the editor chrome on the SDL main thread (fonts): the OCaml
//! lib/pxui_shell test_shell. There are no pixel goldens in lib/pxui_shell; the OCaml suite counts
//! the instances a strip draws, which these cases do through `Layer::stage_native`.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "cases/shell_a.rs"]
mod shell_a;
#[path = "cases/shell_b.rs"]
mod shell_b;

fn main() {
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend_from_slice(shell_a::CASES);
    cases.extend_from_slice(shell_b::CASES);
    if std::env::var("RAYS_SHELL_PARAM_BUG").is_ok() {
        cases.extend_from_slice(shell_b::PARAM_BUG);
    }
    common::window::run(&cases);
}
