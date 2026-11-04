//! Ported OCaml editor tests, group t6: the remaining rays_editor/test files (flow_shader, custom_sop, open_domain,
//! gallery_shapes, image_sop, shattered_studio, terminal_boolean_normals, workspace_images[_native], workspace_ir,
//! sop_catalog, isogrid_deferred).
#[path = "../../rays/tests/common/mod.rs"]
mod common;
#[path = "suite_t6/support.rs"]
mod support;
#[path = "suite_t6/sopcat.rs"]
mod sopcat;
#[path = "suite_t6/images.rs"]
mod images;
#[path = "suite_t6/misc.rs"]
mod misc;

fn main() {
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(misc::cases());
    cases.extend(sopcat::cases());
    cases.extend(images::cases());
    common::window::run(&cases);
}
