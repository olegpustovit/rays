//! The render target Rays hands the renderer: a host window (`pxui_wayland` on Linux, `pxui_sdl` elsewhere) or
//! an offscreen extent, plus the portable per-frame input source. Window hosting itself lives in the host crate.
mod host;
mod runtime;
pub use runtime::*;

/// The portable input source (events, relative mode, per-frame snapshot) and its native pump.
pub mod runtime_input;
pub mod runtime_input_sdl3;

/// Native binary startup policy; does not acquire SDL subsystems or change their ownership.
pub use host::configure_native_process;
#[cfg(test)]
mod test_runtime_input;
#[cfg(test)]
mod test_runtime_visibility;
