use crate::common;
use renderer::renderer;

fn gpu_probe() {
    if !common::gpu() {
        return;
    }
    let cfg = renderer::configuration { logical_width: 16, logical_height: 8, drawable_width: 16, drawable_height: 8, title: "probe".into(), vsync: false, high_density: true };
    let value = common::get(renderer::create_offscreen(&cfg));
    common::get(renderer::step(Some((1.0, 0.0, 0.0, 1.0)), &value, &[]));
    let pixels = common::get(renderer::read_pixels(&value, 64));
    assert_eq!(pixels[0], 255);
    common::get(renderer::destroy(&value));
}

pub const CASES: &[(&str, fn())] = &[("gpu_probe", gpu_probe)];
