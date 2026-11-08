//! OGPU's Vulkan implementation: the driver traits over the safe `vulkan` module (Windows only).
pub mod reflect;
#[cfg(windows)]
pub mod vulkan;
#[cfg(windows)]
pub mod backend;
#[cfg(windows)]
pub mod impl_;

#[cfg(all(test, windows))]
mod tests;
