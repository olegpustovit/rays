//! Merged offscreen editor GPU suite (B3): groups t1–t6 and native qualification.
#![allow(clippy::all)]
#[path = "../../rays/tests/common/mod.rs"]
mod common;

#[path = "support/mod.rs"]
mod support;

#[path = "suites/t1.rs"]
mod suite_t1;
#[path = "suites/t2.rs"]
mod suite_t2;
#[path = "suites/t3.rs"]
mod suite_t3;
#[path = "suites/t4.rs"]
mod suite_t4;
#[path = "suites/t5.rs"]
mod suite_t5;
#[path = "suites/t6.rs"]
mod suite_t6;
#[path = "suites/natq.rs"]
mod natq;

fn main() {
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    if natq::scrub_child() {
        return;
    }
    suite_t2::print_skipped();
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(suite_t1::cases());
    cases.extend(suite_t2::cases());
    cases.extend(suite_t3::cases());
    cases.extend(suite_t4::cases());
    cases.extend(suite_t5::cases());
    cases.extend(suite_t6::cases());
    cases.extend(natq::cases());
    common::window::run(&cases);
}
