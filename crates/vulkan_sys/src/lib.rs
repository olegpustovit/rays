#[cfg(any(windows, target_os = "linux"))]
mod native;
#[cfg(any(windows, target_os = "linux"))]
pub use native::*;
