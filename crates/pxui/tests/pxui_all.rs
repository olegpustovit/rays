//! Kit interaction, text and retained-state cases: headless, no SDL, no window.
use mem_sys as _; // the counting global allocator
#[path = "cases/nodes.rs"]
mod nodes;
#[path = "cases/run.rs"]
mod run;
#[path = "cases/small.rs"]
mod small;
#[path = "cases/styling.rs"]
mod styling;
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
#[path = "cases/views.rs"]
mod views;

fn main() {
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend_from_slice(small::CASES);
    cases.extend_from_slice(ui_a::CASES);
    cases.extend_from_slice(ui_b::CASES);
    cases.extend_from_slice(ui_c::CASES);
    cases.extend_from_slice(nodes::CASES);
    cases.extend_from_slice(views::CASES);
    cases.extend_from_slice(styling::CASES);
    run::run(&cases);
}
