// Merged editor suite (B3): groups t1–t6 and native qualification, included by `gpu_editor` and `editor`.
// `pure_cases.txt` names the cases that never acquire a GPU device (probed); they run in `editor`,
// which the runner parallelizes. Every other case runs in the serialized `gpu_editor`.
#[path = "../../../rays/tests/common/mod.rs"]
mod common;

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
    // ponytail: a case that starts needing a device must move out of pure_cases.txt (it would still pass,
    // but contend with parallel cases); upgrade to a per-case marker if the list churns.
    let pure_names: std::collections::HashSet<&str> = include_str!("../pure_cases.txt").lines().collect();
    cases.retain(|(name, _)| pure_names.contains(name) == pure);
    common::window::run(&cases);
}
