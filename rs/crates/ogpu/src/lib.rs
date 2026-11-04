//! The virtual public API: ogpu_core's modules plus `impl_`, the backend selected at startup.
pub use ogpu_core::{
    acceleration, backend, binding, caps, error, handle, pipeline, render_pass, shader, surface, types,
    validation,
};

pub mod impl_;

#[cfg(test)]
mod test_ogpu_acceleration;
#[cfg(test)]
mod test_ogpu_binding;
#[cfg(test)]
mod test_ogpu_foundation;
#[cfg(test)]
mod test_ogpu_pipeline;
#[cfg(test)]
mod test_ogpu_shader;
#[cfg(test)]
mod test_ogpu_surface;
#[cfg(test)]
mod test_ogpu_validation;
