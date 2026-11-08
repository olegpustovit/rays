//! Editor suite t2: consistency, scene tree, workspace doc, bloom studio.
#![allow(clippy::all)]
mod support {
    pub use crate::support::consistency::*;
}
#[path = "../suite_t2/consistency.rs"]
mod consistency;
#[path = "../suite_t2/consistency_b.rs"]
mod consistency_b;
#[path = "../suite_t2/scene_tree.rs"]
mod scene_tree;
#[path = "../suite_t2/workspace_doc.rs"]
mod workspace_doc;
#[path = "../suite_t2/bloom_studio.rs"]
mod bloom_studio;

pub fn cases() -> Vec<(&'static str, fn())> {
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(consistency::CASES);
    cases.extend(consistency_b::CASES);
    cases.extend(scene_tree::CASES);
    cases.extend(workspace_doc::CASES);
    cases.extend(bloom_studio::CASES);
    cases
}

pub fn print_skipped() {
    for s in [
        consistency::SKIPPED,
        consistency_b::SKIPPED,
        scene_tree::SKIPPED,
        workspace_doc::SKIPPED,
        bloom_studio::SKIPPED,
    ] {
        for (name, why) in s {
            println!("SKIPPED {name}: {why}");
        }
    }
}
