#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals, clippy::all)]

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
