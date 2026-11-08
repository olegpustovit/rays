//! Shared by flow_gpu's native tests. `gpu()` selects the platform's OGPU implementation (Vulkan
//! with validation fatal on Windows, Metal on macOS) and reports whether a device exists; a test
//! with none prints "skipped" and returns, as the OCaml suite skips without a display or GPU.
#![allow(dead_code)]
pub mod window;

use ogpu::error::{self, ErrorKind};

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
    use ogpu::impl_::Impl;
    #[cfg(windows)]
    ogpu::impl_::select(Impl {
        backend: ogpu_vulkan::impl_::BACKEND.to_owned(),
        create_driver: Box::new(|| ogpu_vulkan::backend::create_with_validation((true, true))),
        live_handles: Box::new(ogpu_vulkan::impl_::live_handles),
    });
    #[cfg(target_os = "macos")]
    ogpu::impl_::select(ogpu_metal::impl_::implementation());
    #[cfg(not(any(windows, target_os = "macos")))]
    let _ = Impl { backend: String::new(), create_driver: Box::new(|| panic!("no implementation")), live_handles: Box::new(|| 0) };
}

pub fn gpu() -> bool {
    if KEEP.with(|k| k.borrow().is_some()) {
        return true;
    }
    if !cfg!(any(windows, target_os = "macos")) {
        println!("skipped: no native OGPU implementation on this platform");
        return false;
    }
    select();
    match ::renderer::renderer::acquire_gpu() {
        Ok(lease) => {
            KEEP.with(|k| *k.borrow_mut() = Some(lease));
            true
        }
        Err(e) if e.kind == ErrorKind::NoAdapter => {
            println!("skipped: no device ({})", error::to_string(&e));
            false
        }
        Err(e) => panic!("{}", error::to_string(&e)),
    }
}
