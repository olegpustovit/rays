mod runtime;
pub use runtime::*;

/// The portable input source (events, relative mode, per-frame snapshot) and its SDL3 pump.
pub mod runtime_input;
pub mod runtime_input_sdl3;

/// Native binary startup policy; does not acquire SDL subsystems or change their ownership.
pub use sdl3::init::configure_native_process;
#[cfg(test)]
mod test_runtime_input;
#[cfg(test)]
mod test_runtime_visibility;
