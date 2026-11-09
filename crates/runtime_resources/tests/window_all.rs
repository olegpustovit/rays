mod common;
mod window_gpu_image;
mod window_runtime_resources;

fn main() {
    let groups: &[(&str, &[(&str, fn())])] = &[
        ("window_gpu_image", window_gpu_image::CASES),
        ("window_runtime_resources", window_runtime_resources::CASES),
    ];
    common::window::run_groups(groups);
}
