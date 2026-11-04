//! lib/renderer/test_scene2_argument.ml on a real window renderer (skipped without a display); the
//! offscreen run is gpu_scene2_argument.rs; both share tests/scene2_argument_body.
use crate::common;
use crate::scene2_argument_body;
use crate::common::get;
use renderer::renderer;

fn window_scene2_argument() {
    if !common::gpu() {
        return;
    }
    let before = get(ogpu::impl_::live_handles());
    let runtime = match renderer::create(&renderer::configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, ..renderer::default_configuration() }) {
        Ok(runtime) => runtime,
        Err(e) => {
            println!("runtime-next scene2 retained argument: skipped ({})", ogpu::error::to_string(&e));
            return;
        }
    };
    scene2_argument_body::run(&runtime);
    get(renderer::destroy(&runtime));
    assert_eq!(get(ogpu::impl_::live_handles()), before, "scene2 retained argument live-handle delta");
}

pub const CASES: &[(&str, fn())] = &[("window_scene2_argument", window_scene2_argument)];
