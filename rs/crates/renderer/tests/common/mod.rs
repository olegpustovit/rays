//! Shared by the renderer's native tests. `gpu()` selects the Vulkan implementation (validation
//! fatal, as `packaging/README.md` says tests run) and reports whether a device exists; a test
//! with none prints "skipped" and returns, as the OCaml suite skips without a display or GPU.
#![allow(dead_code)]

use ogpu::error::{self, Error, ErrorKind};

#[cfg(windows)]
pub fn gpu() -> bool {
    use ogpu::impl_::Impl;
    ogpu::impl_::select(Impl {
        backend: ogpu_vulkan::impl_::BACKEND.to_owned(),
        create_driver: Box::new(|| ogpu_vulkan::backend::create_with_validation((true, true))),
        live_handles: Box::new(ogpu_vulkan::impl_::live_handles),
    });
    let probe = ogpu::backend::create_device(ogpu::impl_::create_driver().expect("selected"));
    match probe {
        Ok(device) => {
            let _ = ogpu::backend::destroy_device(&device);
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

pub fn get<A>(result: Result<A, Error>) -> A {
    result.unwrap_or_else(|e| panic!("{}", error::to_string(&e)))
}

pub fn words(values: &[i32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

pub fn floats(values: &[f32]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}
pub mod window;
