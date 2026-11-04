//! Ported OCaml editor tests, group t4 (see rs/port/rays_editor_plan.md). Cases run on the main thread
//! via the shared window runner; add cases to CASES. Placeholder until the group's agent fills it.
#[path = "../../rays/tests/common/mod.rs"]
mod common;

fn main() {
    common::window::run(&[]);
}
