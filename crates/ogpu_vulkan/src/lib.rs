//! OGPU's Vulkan implementation: the driver traits over the safe `vulkan` module (Windows and Linux).
pub mod reflect;
#[cfg(any(windows, target_os = "linux"))]
pub mod vulkan;
#[cfg(any(windows, target_os = "linux"))]
pub mod backend;
#[cfg(any(windows, target_os = "linux"))]
pub mod impl_;

#[cfg(all(test, any(windows, target_os = "linux")))]
mod tests;
