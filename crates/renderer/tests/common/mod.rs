//! Shared by native renderer tests: Vulkan with fatal validation on Windows, Metal on macOS.
//! Supported-platform qualification requires a real device; failed creation must not look passed.
#![allow(dead_code)]

use ogpu_core::error;

thread_local! {
    static KEEP: std::cell::RefCell<Option<::renderer::renderer::Gpu>> = const { std::cell::RefCell::new(None) };
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
    use ogpu_core::impl_::Impl;
    #[cfg(windows)]
    ogpu_core::impl_::select(Impl {
        backend: ogpu_vulkan::impl_::BACKEND,
        create_driver: || ogpu_vulkan::backend::create_with_validation((true, true)),
        live_handles: ogpu_vulkan::impl_::live_handles,
    });
    #[cfg(target_os = "macos")]
    ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
    // The lease keeps the shared headless device alive across the target's cases (released by window::run).
    match ::renderer::renderer::acquire_gpu() {
        Ok(lease) => {
            KEEP.with(|k| *k.borrow_mut() = Some(lease));
            true
        }
        Err(e) if e.kind == error::ErrorKind::NoAdapter => {
            println!("RAYS_TEST_SKIP: no device ({})", error::to_string(&e));
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "RAYS_ALLOW_SKIP=1 explicitly permits skipping missing GPU coverage");
            false
        }
        Err(e) => panic!("{}", error::to_string(&e)),
    }
}

#[cfg(not(any(windows, target_os = "macos")))]
pub fn gpu() -> bool {
    println!("RAYS_TEST_SKIP: skipped: native renderer integration requires Windows Vulkan or macOS Metal");
    assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
    false
}

pub fn words(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

pub fn floats(values: &[f32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}
#[path = "../../../../tools/t/window_harness.rs"]
pub mod window;
