use crate::error::{self, Error, ErrorKind};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Feature {
    Buffer,
    Texture,
    Sampler,
    ComputePipeline,
    RenderPipeline,
    Queue,
    Surface,
    RayTracing,
    RayTracingCurves,
    RayTracingMotion,
    RayTracingRefit,
    MemorylessAttachments,
    Unknown(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Limits {
    pub max_buffer_size: i64,
    pub max_texture_dimension_2d: i64,
    pub max_bind_groups: i64,
    pub max_sample_count: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Caps {
    pub limits: Limits,
    pub compute_pipeline: bool,
    pub render_pipeline: bool,
    pub ray_tracing: bool,
    pub ray_tracing_curves: bool,
    pub ray_tracing_motion: bool,
    pub ray_tracing_refit: bool,
    pub memoryless_attachments: bool,
    pub unified_memory: bool,
}

pub fn minimum_m1() -> Caps {
    Caps {
        limits: Limits {
            max_buffer_size: 268_435_456,
            max_texture_dimension_2d: 16_384,
            max_bind_groups: 4,
            max_sample_count: 4,
        },
        compute_pipeline: true,
        render_pipeline: true,
        ray_tracing: false,
        ray_tracing_curves: false,
        ray_tracing_motion: false,
        ray_tracing_refit: false,
        memoryless_attachments: false,
        unified_memory: false,
    }
}

pub fn validate(caps: &Caps) -> Result<(), Error> {
    let limits = caps.limits;
    if limits.max_buffer_size <= 0
        || limits.max_texture_dimension_2d <= 0
        || limits.max_bind_groups <= 0
        || limits.max_sample_count <= 0
    {
        Err(error::make(
            "Ogpu.Caps.validate",
            ErrorKind::InvalidArgument,
            "limits must be positive",
        ))
    } else {
        Ok(())
    }
}

pub fn has(caps: &Caps, feature: &Feature) -> bool {
    match feature {
        Feature::Buffer | Feature::Texture | Feature::Sampler | Feature::Queue | Feature::Surface => true,
        Feature::ComputePipeline => caps.compute_pipeline,
        Feature::RenderPipeline => caps.render_pipeline,
        Feature::RayTracing => caps.ray_tracing,
        Feature::RayTracingCurves => caps.ray_tracing_curves,
        Feature::RayTracingMotion => caps.ray_tracing_motion,
        Feature::RayTracingRefit => caps.ray_tracing_refit,
        Feature::MemorylessAttachments => caps.memoryless_attachments,
        Feature::Unknown(_) => false,
    }
}

pub fn require(operation: Option<&str>, caps: &Caps, feature: &Feature) -> Result<(), Error> {
    if has(caps, feature) {
        Ok(())
    } else {
        Err(error::make(
            operation.unwrap_or("Ogpu.Caps.require"),
            ErrorKind::Unsupported,
            "operation is unavailable on this adapter profile",
        ))
    }
}
