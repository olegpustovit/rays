mod common;
mod gpu_ogpu_metal;
mod gpu_ogpu_metal_native;

fn main() {
    if !cfg!(target_os = "macos") { return; }
    let groups: &[(&str, &[(&str, fn())])] = &[
        ("gpu_ogpu_metal", gpu_ogpu_metal::CASES),
        ("gpu_ogpu_metal_native", gpu_ogpu_metal_native::CASES),
    ];
    common::window::run_groups(groups);
}
