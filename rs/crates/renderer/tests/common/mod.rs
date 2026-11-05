//! Shared by native renderer tests: Vulkan with fatal validation on Windows, Metal on macOS.
//! Supported-platform qualification requires a real device; failed creation must not look passed.
#![allow(dead_code)]

use ogpu::error;

thread_local! {
    static KEEP: std::cell::RefCell<Option<::renderer::renderer::gpu>> = const { std::cell::RefCell::new(None) };
}

pub fn release_keepalive() {
    if let Some(lease) = KEEP.with(|k| k.borrow_mut().take()) {
        ::renderer::renderer::release_gpu(&lease);
    }
}

#[cfg(any(windows, target_os = "macos"))]
pub fn gpu() -> bool {
    if KEEP.with(|k| k.borrow().is_some()) {
        return true;
    }
    #[cfg(windows)]
    use ogpu::impl_::Impl;
    #[cfg(windows)]
    ogpu::impl_::select(Impl {
        backend: ogpu_vulkan::impl_::BACKEND.to_owned(),
        create_driver: Box::new(|| ogpu_vulkan::backend::create_with_validation((true, true))),
        live_handles: Box::new(ogpu_vulkan::impl_::live_handles),
    });
    #[cfg(target_os = "macos")]
    ogpu::impl_::select(ogpu_metal::impl_::implementation());
    // The lease keeps the shared headless device alive across the target's cases (released by window::run).
    match ::renderer::renderer::acquire_gpu() {
        Ok(lease) => {
            KEEP.with(|k| *k.borrow_mut() = Some(lease));
            true
        }
        Err(e) => panic!("{}", error::to_string(&e)),
    }
}

#[cfg(not(any(windows, target_os = "macos")))]
pub fn gpu() -> bool {
    println!("skipped: native renderer integration requires Windows Vulkan or macOS Metal");
    false
}

pub fn words(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

pub fn floats(values: &[f32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}
pub mod window;
