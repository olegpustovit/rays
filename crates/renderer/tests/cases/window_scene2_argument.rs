//!  on a real window renderer (skipped without a display); the
//! offscreen run is gpu_scene2_argument.rs; both share tests/scene2_argument_body.
use crate::common;
use crate::scene2_argument_body;
use renderer::renderer;

fn window_scene2_argument() {
    if !common::gpu() {
        return;
    }
    let before = ogpu_core::impl_::live_handles().unwrap();
    // The retained-argument frame sweep measures rendering, not display pacing.
    let runtime = match renderer::create(&renderer::Configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, vsync: false, ..renderer::default_configuration() }) {
        Ok(runtime) => runtime,
        Err(e) => {
            println!("RAYS_TEST_SKIP: runtime-next scene2 retained argument: skipped ({})", ogpu_core::error::to_string(&e));
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
            return;
        }
    };
    // AppKit presentation needs the same event pumping as the real application loop.
    let mut input = runtime::runtime_input::create(4096, 4, 4).unwrap();
    scene2_argument_body::run(&runtime, &scene2_argument_body::WINDOW, || {
        runtime::runtime_input::begin_frame(&mut input);
        runtime::runtime_input_sdl3::pump(&mut input).unwrap();
        runtime::runtime_input::drain(&mut input);
    });
    renderer::destroy(&runtime).unwrap();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), before, "scene2 retained argument live-handle delta");
}

pub const CASES: &[(&str, fn())] = &[("window_scene2_argument", window_scene2_argument)];
