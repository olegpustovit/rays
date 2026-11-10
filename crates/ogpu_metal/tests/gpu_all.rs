#[cfg(target_os = "macos")]
mod common;
#[cfg(target_os = "macos")]
mod gpu_metal;
#[cfg(target_os = "macos")]
mod native_reflection_snapshot;
#[cfg(target_os = "macos")]
mod native_shader_type;
#[cfg(target_os = "macos")]
mod gpu_ogpu_metal;
#[cfg(target_os = "macos")]
mod gpu_ogpu_metal_native;

#[cfg(target_os = "macos")]
fn main() {
    let groups: &[(&str, &[(&str, fn())])] = &[
        ("gpu_metal", gpu_metal::CASES),
        ("gpu_ogpu_metal", gpu_ogpu_metal::CASES),
        ("gpu_ogpu_metal_native", gpu_ogpu_metal_native::CASES),
    ];
    common::window::run_groups(groups);
}

#[cfg(not(target_os = "macos"))]
fn main() {}
