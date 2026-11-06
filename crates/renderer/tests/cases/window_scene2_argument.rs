//! lib/renderer/test_scene2_argument.ml on a real window renderer (skipped without a display); the
//! offscreen run is gpu_scene2_argument.rs; both share tests/scene2_argument_body.
use crate::common;
use crate::scene2_argument_body;
use renderer::renderer;

fn window_scene2_argument() {
    if !common::gpu() {
        return;
    }
    let before = ogpu::impl_::live_handles().unwrap();
    // The retained-argument frame sweep measures rendering, not display pacing.
    let runtime = match renderer::create(&renderer::configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, vsync: false, ..renderer::default_configuration() }) {
        Ok(runtime) => runtime,
        Err(e) => {
            println!("runtime-next scene2 retained argument: skipped ({})", ogpu::error::to_string(&e));
            return;
        }
    };
    // AppKit presentation needs the same event pumping as the real application loop.
    let mut input = runtime_input::create(4096, 4, 4).unwrap();
    scene2_argument_body::run(&runtime, || {
        runtime_input::begin_frame(&mut input);
        runtime_input::runtime_input_sdl3::pump(&mut input).unwrap();
        runtime_input::drain(&mut input);
    });
    renderer::destroy(&runtime).unwrap();
    assert_eq!(ogpu::impl_::live_handles().unwrap(), before, "scene2 retained argument live-handle delta");
}

pub const CASES: &[(&str, fn())] = &[("window_scene2_argument", window_scene2_argument)];
