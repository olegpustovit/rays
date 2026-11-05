//! Native editor qualification ports, using the offscreen Vulkan/Metal test device.
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "native_qualification/workspace.rs"]
mod workspace;
#[path = "native_qualification/placement.rs"]
mod placement;
#[path = "native_qualification/isogrid.rs"]
mod isogrid;
#[path = "native_qualification/chains.rs"]
mod chains;
#[path = "native_qualification/live_mesh.rs"]
mod live_mesh;
#[path = "native_qualification/marks_scene.rs"]
mod marks_scene;
#[path = "native_qualification/smoke.rs"]
mod smoke;

fn main() {
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    if chains::scrub_child() {
        return;
    }
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(workspace::cases());
    cases.extend(placement::cases());
    cases.extend(isogrid::cases());
    cases.extend(chains::cases());
    cases.extend(live_mesh::cases());
    cases.extend(marks_scene::cases());
    cases.extend(smoke::cases());
    common::window::run(&cases);
}
