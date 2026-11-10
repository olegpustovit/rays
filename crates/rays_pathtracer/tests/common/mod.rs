//! Native test helpers select Metal or fatal-validation Vulkan. Tests report device absence explicitly.
#![allow(dead_code)]
#[path = "../../../renderer/tests/common/window.rs"]
pub mod window;

use ogpu_core::error::{self, ErrorKind};

thread_local! {
    static KEEP: std::cell::RefCell<Option<::renderer::renderer::Gpu>> = const { std::cell::RefCell::new(None) };
}

pub fn release_keepalive() {
    flow_gpu::host::clear_thread_state();
    if let Some(lease) = KEEP.with(|k| k.borrow_mut().take()) {
        ::renderer::renderer::release_gpu(&lease);
    }
}

/// The shared lease `gpu()` took.
pub fn lease() -> ::renderer::renderer::Gpu {
    KEEP.with(|k| k.borrow().clone()).expect("gpu() first")
}

fn select() {
    #[cfg(not(target_os = "macos"))]
    use ogpu_core::impl_::Impl;

    #[cfg(windows)]
    ogpu_core::impl_::select(Impl {
        backend: ogpu_vulkan::impl_::BACKEND,
        create_driver: || ogpu_vulkan::backend::create_with_validation((true, true)),
        live_handles: ogpu_vulkan::impl_::live_handles,
    });
    #[cfg(target_os = "macos")]
    ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
    #[cfg(not(any(windows, target_os = "macos")))]
    let _ = Impl { backend: "", create_driver: || panic!("no implementation"), live_handles: || 0 };
}

pub fn gpu() -> bool {
    if KEEP.with(|k| k.borrow().is_some()) {
        return true;
    }
    if !cfg!(any(windows, target_os = "macos")) {
        println!("RAYS_TEST_SKIP: skipped: no native OGPU implementation on this platform");
        assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
        return false;
    }
    select();
    match ::renderer::renderer::acquire_gpu() {
        Ok(lease) => {
            KEEP.with(|k| *k.borrow_mut() = Some(lease));
            true
        }
        Err(e) if e.kind == ErrorKind::NoAdapter => {
            println!("RAYS_TEST_SKIP: skipped: no device ({})", error::to_string(&e));
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
            false
        }
        Err(e) => panic!("{}", error::to_string(&e)),
    }
}
pub mod meshes;
pub mod pt;
