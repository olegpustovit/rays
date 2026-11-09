mod common;
mod gpu_metal;
mod native_reflection_snapshot;
mod native_shader_type;
mod gpu_ogpu_metal;
mod gpu_ogpu_metal_native;

fn main() {
    if !cfg!(target_os = "macos") { return; }
    let groups: &[(&str, &[(&str, fn())])] = &[
        ("gpu_metal", gpu_metal::CASES),
        ("gpu_ogpu_metal", gpu_ogpu_metal::CASES),
        ("gpu_ogpu_metal_native", gpu_ogpu_metal_native::CASES),
    ];
    common::window::run_groups(groups);
}
