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
    pub max_texture_dimension_1d: i64,
    pub max_texture_dimension_2d: i64,
    pub max_texture_dimension_3d: i64,
    pub max_texture_dimension_cube: i64,
    pub max_texture_array_layers: i64,
    pub max_bind_groups: i64,
    /// Actual supported color/depth attachment counts; bit values are the sample counts.
    pub sample_counts: u32,
    pub color_sample_counts: u32,
    pub depth_sample_counts: u32,
}

impl Limits {
    pub fn supports_samples(self, count: i64) -> bool {
        count > 0 && count <= 64 && count & (count - 1) == 0 && self.sample_counts & count as u32 != 0
    }
    pub fn supports_texture_samples(self, format: crate::types::TextureFormat, count: i64) -> bool {
        let mask = if format == crate::types::TextureFormat::Depth32Float { self.depth_sample_counts } else { self.color_sample_counts };
        count > 0 && count <= 64 && count & (count - 1) == 0 && mask & count as u32 != 0
    }
    pub fn max_sample_count(self) -> i64 {
        self.sample_counts.checked_ilog2().map_or(0, |n| 1i64 << n)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Caps {
    pub limits: Limits,
    pub compute_pipeline: bool,
    pub render_pipeline: bool,
    pub render_requires_fragment: bool,
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
            max_texture_dimension_1d: 16_384,
            max_texture_dimension_2d: 16_384,
            max_texture_dimension_3d: 2_048,
            max_texture_dimension_cube: 16_384,
            max_texture_array_layers: 2_048,
            max_bind_groups: 4,
            sample_counts: 1 | 2 | 4, color_sample_counts: 1 | 2 | 4, depth_sample_counts: 1 | 2 | 4,
        },
        compute_pipeline: true,
        render_pipeline: true,
        render_requires_fragment: false,
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
        || limits.max_texture_dimension_1d <= 0
        || limits.max_texture_dimension_2d <= 0
        || limits.max_texture_dimension_3d <= 0
        || limits.max_texture_dimension_cube <= 0
        || limits.max_texture_array_layers <= 0
        || limits.max_bind_groups <= 0
        || limits.sample_counts & 1 == 0
        || limits.sample_counts & !127 != 0
        || limits.color_sample_counts & 1 == 0 || limits.depth_sample_counts & 1 == 0
        || (limits.color_sample_counts | limits.depth_sample_counts) & !127 != 0
        || limits.sample_counts != limits.color_sample_counts & limits.depth_sample_counts
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
