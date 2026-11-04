//! OGPU's Vulkan implementation: the driver records over the `vulkan` crate (Windows only).
#[cfg(windows)]
pub mod backend;
#[cfg(windows)]
pub mod impl_;

#[cfg(all(test, windows))]
mod tests;
