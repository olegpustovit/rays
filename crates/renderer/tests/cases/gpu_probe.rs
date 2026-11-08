use crate::common;
use renderer::renderer;

fn gpu_probe() {
    if !common::gpu() {
        return;
    }
    let cfg = renderer::Configuration { logical_width: 16, logical_height: 8, drawable_width: 16, drawable_height: 8, title: "probe".into(), vsync: false, high_density: true };
    let value = renderer::create_offscreen(&cfg).unwrap();
    renderer::step(Some((1.0, 0.0, 0.0, 1.0)), &value, &[]).unwrap();
    let pixels = renderer::read_pixels(&value, 64).unwrap();
    assert_eq!(pixels[0], 255);
    renderer::destroy(&value).unwrap();
}

pub const CASES: &[(&str, fn())] = &[("gpu_probe", gpu_probe)];
