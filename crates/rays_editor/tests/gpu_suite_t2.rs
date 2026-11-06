//! Ported OCaml editor tests, group t2: test_editor_consistency, test_scene_sync (editor-driven
//! parts), test_scene_tree, test_workspace_doc (editor-driven parts), test_bloom_studio,
//! test_isogrid_deferred (see docs/history/rays_editor_plan.md). Cases run on the main thread via the
//! shared window runner.
#![allow(clippy::all)]
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "suite_t2/support.rs"]
mod support;
#[path = "suite_t2/consistency.rs"]
mod consistency;
#[path = "suite_t2/consistency_b.rs"]
mod consistency_b;
#[path = "suite_t2/scene_tree.rs"]
mod scene_tree;
#[path = "suite_t2/workspace_doc.rs"]
mod workspace_doc;
#[path = "suite_t2/bloom_studio.rs"]
mod bloom_studio;

fn main() {
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(consistency::CASES);
    cases.extend(consistency_b::CASES);
    cases.extend(scene_tree::CASES);
    cases.extend(workspace_doc::CASES);
    cases.extend(bloom_studio::CASES);
    for s in [consistency::SKIPPED, consistency_b::SKIPPED, scene_tree::SKIPPED, workspace_doc::SKIPPED, bloom_studio::SKIPPED] {
        for (name, why) in s {
            println!("SKIPPED {name}: {why}");
        }
    }
    common::window::run(&cases);
}
