//! lib/renderer/test_scene2_argument.ml on an offscreen renderer (the window run is
//! window_scene2_argument.rs); both share tests/scene2_argument_body.
mod common;
mod scene2_argument_body;
use common::get;
use renderer::renderer;

fn gpu_scene2_argument() {
    if !common::gpu() {
        return;
    }
    let before = get(ogpu::impl_::live_handles());
    let runtime = get(renderer::create_offscreen(&renderer::configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, ..renderer::default_configuration() }));
    scene2_argument_body::run(&runtime);
    get(renderer::destroy(&runtime));
    assert_eq!(get(ogpu::impl_::live_handles()), before, "scene2 retained argument live-handle delta");
}

fn main() {
    common::window::run(&[("gpu_scene2_argument", gpu_scene2_argument)]);
}
