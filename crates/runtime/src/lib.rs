//! The render target Rays hands the renderer: a `pxui_sdl` window or an offscreen extent, plus the
//! portable per-frame input source. Window hosting itself lives in `pxui_sdl`.
mod runtime;
pub use runtime::*;

/// The portable input source (events, relative mode, per-frame snapshot) and its SDL3 pump.
pub mod runtime_input;
pub mod runtime_input_sdl3;

/// Native binary startup policy; does not acquire SDL subsystems or change their ownership.
pub use pxui_sdl::configure_native_process;
#[cfg(test)]
mod test_runtime_input;
#[cfg(test)]
mod test_runtime_visibility;
