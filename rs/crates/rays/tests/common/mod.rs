//! Shared by the renderer's native tests. `gpu()` selects the Vulkan implementation (validation
//! fatal, as `packaging/README.md` says tests run) and reports whether a device exists; a test
//! with none prints "skipped" and returns, as the OCaml suite skips without a display or GPU.
#![allow(dead_code)]

use ogpu::error::{self, ErrorKind};

thread_local! {
    static KEEP: std::cell::RefCell<Option<::renderer::renderer::gpu>> = const { std::cell::RefCell::new(None) };
}

pub fn release_keepalive() {
    if let Some(lease) = KEEP.with(|k| k.borrow_mut().take()) {
        ::renderer::renderer::release_gpu(&lease);
    }
}

#[cfg(windows)]
pub fn gpu() -> bool {
    if KEEP.with(|k| k.borrow().is_some()) {
        return true;
    }
    use ogpu::impl_::Impl;
    ogpu::impl_::select(Impl {
        backend: ogpu_vulkan::impl_::BACKEND.to_owned(),
        create_driver: Box::new(|| ogpu_vulkan::backend::create_with_validation((true, true))),
        live_handles: Box::new(ogpu_vulkan::impl_::live_handles),
    });
    // The lease keeps the shared headless device alive across the target's cases (released by window::run).
    match ::renderer::renderer::acquire_gpu() {
        Ok(lease) => {
            KEEP.with(|k| *k.borrow_mut() = Some(lease));
            true
        }
        Err(e) if e.kind == ErrorKind::NoAdapter => {
            println!("skipped: no Vulkan device ({})", error::to_string(&e));
            false
        }
        Err(e) => panic!("{}", error::to_string(&e)),
    }
}

#[cfg(not(windows))]
pub fn gpu() -> bool {
    println!("skipped: the Vulkan implementation is Windows only");
    false
}

pub fn words(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

pub fn floats(values: &[f32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}
pub mod window;
