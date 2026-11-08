//! OGPU: the portable device/buffer/pipeline API and its validation (`backend`), plus `impl_`, the
//! backend selected at startup. Metal, Vulkan and mock implement the `backend::Driver*` traits.
pub mod acceleration;
pub mod backend;
pub mod binding;
pub mod caps;
pub mod error;
pub mod handle;
pub mod impl_;
pub mod pipeline;
pub mod render_pass;
pub mod shader;
pub mod surface;
pub mod types;
pub mod validation;

#[cfg(test)]
mod tests;
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
