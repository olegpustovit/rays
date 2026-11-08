//! lib/renderer/test_scene2_argument.ml on an offscreen renderer (the window run is
//! window_scene2_argument.rs); both share tests/scene2_argument_body.
use crate::common;
use crate::scene2_argument_body;
use renderer::renderer;

fn gpu_scene2_argument() {
    if !common::gpu() {
        return;
    }
    let before = ogpu_core::impl_::live_handles().unwrap();
    let runtime = renderer::create_offscreen(&renderer::Configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, ..renderer::default_configuration() }).unwrap();
    scene2_argument_body::run(&runtime, || {});
    renderer::destroy(&runtime).unwrap();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), before, "scene2 retained argument live-handle delta");
}

pub const CASES: &[(&str, fn())] = &[("gpu_scene2_argument", gpu_scene2_argument)];
