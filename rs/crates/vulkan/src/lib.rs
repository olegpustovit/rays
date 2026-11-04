#![forbid(unsafe_code)]

pub mod reflect;
#[cfg(windows)]
mod vulkan;
#[cfg(windows)]
pub use vulkan::*;
