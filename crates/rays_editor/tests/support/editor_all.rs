// Merged editor suite (B3): groups t1–t6 and native qualification, included by `gpu_editor` and `editor`.
// `pure_cases.txt` names the device-free cases the runner parallelizes in `editor`.
// Native cases use `gpu_editor`, where the full SOP inventories also share their checked factories.
#[path = "../../../rays/tests/common/mod.rs"]
mod native;

mod common {
    pub use super::native::{release_keepalive, window};
    use std::cell::Cell;

    thread_local! { pub static DEVICE_FREE: Cell<bool> = const { Cell::new(false) }; }

    pub fn gpu() -> bool {
        // The device-free cases use the same editor helpers with no native backend installed.
        DEVICE_FREE.get() || super::native::gpu()
    }
}

#[path = "../support/mod.rs"]
mod support;

#[path = "../suites/t1.rs"]
mod suite_t1;
#[path = "../suites/t2.rs"]
mod suite_t2;
#[path = "../suites/t3.rs"]
mod suite_t3;
#[path = "../suites/t4.rs"]
mod suite_t4;
#[path = "../suites/t5.rs"]
mod suite_t5;
#[path = "../suites/t6.rs"]
mod suite_t6;
#[path = "../suites/natq.rs"]
mod natq;

fn run_split(pure: bool) {
    // a cached view whose revision misses an input panics, naming it (debug builds)
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
    // ponytail: keep the device-free names in one list; use per-case markers if it churns.
    let pure_names: std::collections::HashSet<&str> = include_str!("../pure_cases.txt").lines().collect();
    cases.retain(|(name, _)| pure_names.contains(name) == pure);
    common::DEVICE_FREE.set(pure);
    common::window::run(&cases);
    if pure {
        assert!(!ogpu_core::impl_::is_selected(), "a device-free editor case selected a GPU backend");
    }
}
