#![allow(clippy::too_many_arguments, clippy::type_complexity)]

pub mod renderer_shaders;
pub mod side_lru;
pub mod scene_attachment_pool;
pub mod scene_execution;
pub mod renderer_pipeline;
pub mod renderer_leases;
pub mod renderer_retained_plans;
pub mod renderer_lowering;
pub mod renderer;

#[cfg(test)]
mod test_mock_backend;
#[cfg(test)]
mod test_renderer_scaling;
