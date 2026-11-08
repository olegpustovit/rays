//! Native editor qualification (offscreen Vulkan/Metal device).
#[path = "../native_qualification/workspace.rs"]
mod workspace;
#[path = "../native_qualification/placement.rs"]
mod placement;
#[path = "../native_qualification/isogrid.rs"]
mod isogrid;
#[path = "../native_qualification/chains.rs"]
mod chains;
#[path = "../native_qualification/live_mesh.rs"]
mod live_mesh;
#[path = "../native_qualification/marks_scene.rs"]
mod marks_scene;
#[path = "../native_qualification/smoke.rs"]
mod smoke;

pub fn scrub_child() -> bool {
    chains::scrub_child()
}

pub fn cases() -> Vec<(&'static str, fn())> {
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(workspace::cases());
    cases.extend(placement::cases());
    cases.extend(isogrid::cases());
    cases.extend(chains::cases());
    cases.extend(live_mesh::cases());
    cases.extend(marks_scene::cases());
    cases.extend(smoke::cases());
    cases
}
