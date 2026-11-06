mod runtime;
pub use runtime::*;

/// Native binary startup policy; does not acquire SDL subsystems or change their ownership.
pub use sdl3::init::configure_native_process;
#[cfg(test)]
mod test_runtime_visibility;
