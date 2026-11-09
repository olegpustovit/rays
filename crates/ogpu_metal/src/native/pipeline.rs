use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::native::resource::{error, sys_error, enter_main, Buffer, CommandBuffer, Device, DeviceFamily, Error, ErrorKind, Lifetime, Texture, TextureKind, TextureRegion};
use ogpu_core::types::TextureFormat as Format;
use crate::native::{descriptors::{DepthStencil, RenderPassDescriptor, Sampler}, library::Library};

pub use ogpu_core::render_pass::Primitive as PrimitiveTopology;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BlendState { Disabled, Enabled }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BlendFactor {
    Zero, One, SourceColor, OneMinusSourceColor, SourceAlpha, OneMinusSourceAlpha,
    DestinationColor, OneMinusDestinationColor, DestinationAlpha, OneMinusDestinationAlpha,
    SourceAlphaSaturated, Color, OneMinusColor, Alpha, OneMinusAlpha,
    Source1Color, OneMinusSource1Color, Source1Alpha, OneMinusSource1Alpha,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BlendOperation { Add, Subtract, ReverseSubtract, Min, Max }
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ColorWrite { Red, Green, Blue, Alpha }
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ColorAttachment {
    pub format: Format,
    pub blending: BlendState,
    pub source_rgb: BlendFactor,
    pub destination_rgb: BlendFactor,
    pub rgb_operation: BlendOperation,
    pub source_alpha: BlendFactor,
    pub destination_alpha: BlendFactor,
    pub alpha_operation: BlendOperation,
    pub write_mask: Vec<ColorWrite>,
}
impl ColorAttachment {
    pub fn new(format: Format) -> Self {
        Self {
            format, blending: BlendState::Disabled, source_rgb: BlendFactor::One,
            destination_rgb: BlendFactor::Zero, rgb_operation: BlendOperation::Add,
            source_alpha: BlendFactor::One, destination_alpha: BlendFactor::Zero,
            alpha_operation: BlendOperation::Add,
            write_mask: vec![ColorWrite::Red, ColorWrite::Green, ColorWrite::Blue, ColorWrite::Alpha],
        }
    }
}

#[derive(Clone, Debug)]
pub struct RenderPipelineOptions {
    pub label: Option<String>,
    pub fragment: Option<String>,
    pub reflection: bool,
    pub raster_sample_count: usize,
    pub color_formats: Option<Vec<Format>>,
    pub color_attachments: Option<Vec<ColorAttachment>>,
    pub primitive_topology: PrimitiveTopology,
    pub support_indirect_command_buffers: bool,
}
impl Default for RenderPipelineOptions {
    fn default() -> Self {
        Self {
            label: None, fragment: None, reflection: false, raster_sample_count: 1,
            color_formats: None, color_attachments: None, primitive_topology: PrimitiveTopology::TriangleList,
            support_indirect_command_buffers: false,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BindingAccess { ReadOnly, ReadWrite, WriteOnly, Unknown(i32) }
#[derive(Clone, Debug, PartialEq)]
pub enum BindingKind {
    Buffer { alignment: i64, data_size: i64, data_type: i32 },
    ThreadgroupMemory { alignment: i64, data_size: i64 },
    Texture { texture_kind: i32, data_type: i32, depth: bool, array_length: i64 },
    Sampler,
    ImageblockData,
    Imageblock,
    VisibleFunctionTable,
    PrimitiveAccelerationStructure,
    InstanceAccelerationStructure,
    IntersectionFunctionTable,
    ObjectPayload { alignment: i64, data_size: i64 },
    Tensor,
    Unknown(i32),
}
#[derive(Clone, Debug, PartialEq)]
pub struct ShaderBinding {
    pub name: String,
    pub index: u64,
    pub access: BindingAccess,
    pub used: bool,
    pub argument: bool,
    pub kind: BindingKind,
    pub reflection: Option<metal_sys::native::reflection::ReflectedType>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct RenderPipelineReflection {
    pub vertex_bindings: Vec<ShaderBinding>,
    pub fragment_bindings: Vec<ShaderBinding>,
}

pub(crate) fn map_binding(binding: metal_sys::native::pipeline::Binding) -> ShaderBinding {
    let kind = match binding.binding_type {
        0 => BindingKind::Buffer { alignment: binding.buffer_alignment, data_size: binding.buffer_data_size, data_type: binding.buffer_data_type },
        1 => BindingKind::ThreadgroupMemory { alignment: binding.threadgroup_alignment, data_size: binding.threadgroup_data_size },
        2 => BindingKind::Texture { texture_kind: binding.texture_type, data_type: binding.texture_data_type, depth: binding.depth, array_length: binding.array_length },
        3 => BindingKind::Sampler,
        16 => BindingKind::ImageblockData,
        17 => BindingKind::Imageblock,
        24 => BindingKind::VisibleFunctionTable,
        25 => BindingKind::PrimitiveAccelerationStructure,
        26 => BindingKind::InstanceAccelerationStructure,
        27 => BindingKind::IntersectionFunctionTable,
        34 => BindingKind::ObjectPayload { alignment: binding.object_alignment, data_size: binding.object_data_size },
        37 => BindingKind::Tensor,
        value => BindingKind::Unknown(value),
    };
    ShaderBinding {
        name: binding.name, index: binding.index,
        access: match binding.access { 0 => BindingAccess::ReadOnly, 1 => BindingAccess::ReadWrite, 2 => BindingAccess::WriteOnly, value => BindingAccess::Unknown(value) },
        used: binding.used, argument: binding.argument, kind,
        reflection: binding.reflected_type,
    }
}
fn map_reflection((vertex, fragment): (Vec<metal_sys::native::pipeline::Binding>, Vec<metal_sys::native::pipeline::Binding>)) -> RenderPipelineReflection {
    RenderPipelineReflection { vertex_bindings: vertex.into_iter().map(map_binding).collect(), fragment_bindings: fragment.into_iter().map(map_binding).collect() }
}

struct CompilerInner { raw: metal_sys::Handle, lifetime: Lifetime, device: Device, device_attached: AtomicBool }
impl Drop for CompilerInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.detach_child(); }
    }
}
#[derive(Clone)]
pub struct Compiler(Arc<CompilerInner>);
impl Compiler {
    pub fn create(device: &Device) -> Result<Self, Error> {
        const OP: &str = "Metal.Compiler.create";
        enter_main(OP)?;
        device.ensure_live(OP)?;
        if !device.supports_family(DeviceFamily::Metal4)? { return Err(error(OP, ErrorKind::Unsupported, "the Metal device does not support Metal 4")); }
        let raw = metal_sys::native::pipeline::create_compiler(device.raw()).map_err(|failure| sys_error(OP, failure))?;
        device.attach_child(OP)?;
        Ok(Self(Arc::new(CompilerInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true) })))
    }

    pub fn create_render_pipeline(&self, library: &Library, vertex: &str, options: &RenderPipelineOptions) -> Result<RenderPipeline, Error> {
        const OP: &str = "Metal.Compiler.create_render_pipeline";
        enter_main(OP)?;
        self.0.lifetime.ensure_live(OP)?;
        library.lifetime().ensure_live(OP)?;
        if !self.0.device.same(library.device()) { return Err(error(OP, ErrorKind::CrossDevice, "compiler and library belong to different devices")); }
        if vertex.is_empty() || vertex.as_bytes().contains(&0) { return Err(error(OP, ErrorKind::InvalidArgument, "vertex function name must be nonempty and contain no NUL byte")); }
        if let Some(fragment) = &options.fragment {
            if fragment.is_empty() || fragment.as_bytes().contains(&0) { return Err(error(OP, ErrorKind::InvalidArgument, "fragment function name must be nonempty and contain no NUL byte")); }
        }
        if options.label.as_ref().is_some_and(|label| label.as_bytes().contains(&0)) { return Err(error(OP, ErrorKind::InvalidArgument, "render-pipeline label contains a NUL byte")); }
        let names = library.function_names()?;
        if !names.iter().any(|name| name == vertex) { return Err(error(OP, ErrorKind::InvalidArgument, "vertex function is absent from the source library")); }
        if options.fragment.as_ref().is_some_and(|fragment| !names.iter().any(|name| name == fragment)) { return Err(error(OP, ErrorKind::InvalidArgument, "fragment function is absent from the source library")); }
        let mut color_attachments = match (&options.color_formats, &options.color_attachments) {
            (Some(_), Some(_)) => return Err(error(OP, ErrorKind::InvalidArgument, "color_formats and color_attachments are mutually exclusive")),
            (Some(formats), None) => formats.iter().copied().map(ColorAttachment::new).collect(),
            (None, Some(attachments)) => attachments.clone(),
            (None, None) => vec![ColorAttachment::new(Format::Bgra8Unorm)],
        };
        // Rasterization is always on: it needs exactly one fragment function and one to eight colours.
        if options.fragment.is_none() { return Err(error(OP, ErrorKind::InvalidArgument, "rasterization requires exactly one fragment function")); }
        if color_attachments.is_empty() || color_attachments.len() > 8 {
            return Err(error(OP, ErrorKind::InvalidArgument, "render color attachments must match rasterization and not exceed eight"));
        }
        if color_attachments.iter().any(|attachment| attachment.format == Format::Depth32Float) { return Err(error(OP, ErrorKind::InvalidArgument, "render color attachments require color pixel formats")); }
        if color_attachments.iter().any(|attachment| attachment.write_mask.iter().copied().collect::<std::collections::HashSet<_>>().len() != attachment.write_mask.len()) { return Err(error(OP, ErrorKind::InvalidArgument, "render color-write masks contain duplicate channels")); }
        if options.raster_sample_count == 0 { return Err(error(OP, ErrorKind::InvalidArgument, "render raster sample count must be positive")); }
        if !self.0.device.supports_texture_sample_count(options.raster_sample_count)? { return Err(error(OP, ErrorKind::Unsupported, "the Metal device does not support the render sample count")); }
        let sample_count = u64::try_from(options.raster_sample_count).map_err(|_| error(OP, ErrorKind::InvalidArgument, "render sample count exceeds native bounds"))?;
        let native_sample_count = i64::try_from(sample_count).map_err(|_| error(OP, ErrorKind::InvalidArgument, "render sample count exceeds descriptor bounds"))?;
        let raw_colors = color_attachments.drain(..).map(|attachment| {
            metal_sys::native::pipeline::ColorAttachment {
                pixel_format: crate::native::resource::pixel_format_code(attachment.format) as i32,
                blending_state: match attachment.blending { BlendState::Disabled => 0, BlendState::Enabled => 1 },
                source_rgb_blend_factor: blend_factor_code(attachment.source_rgb),
                destination_rgb_blend_factor: blend_factor_code(attachment.destination_rgb),
                rgb_blend_operation: blend_operation_code(attachment.rgb_operation),
                source_alpha_blend_factor: blend_factor_code(attachment.source_alpha),
                destination_alpha_blend_factor: blend_factor_code(attachment.destination_alpha),
                alpha_blend_operation: blend_operation_code(attachment.alpha_operation),
                write_mask: attachment.write_mask.iter().copied().fold(0, |bits, value| bits | color_write_bit(value)),
            }
        }).collect::<Vec<_>>();
        let topology = match options.primitive_topology { PrimitiveTopology::PointList => 1, PrimitiveTopology::LineList | PrimitiveTopology::LineStrip => 2, PrimitiveTopology::TriangleList | PrimitiveTopology::TriangleStrip => 3 };
        let (raw, reflection) = metal_sys::native::pipeline::create_render_pipeline(
            &self.0.raw, library.raw(), options.label.as_deref(), vertex,
            options.fragment.as_deref(), options.reflection, native_sample_count as u64,
            &raw_colors, topology, options.support_indirect_command_buffers,
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.device.attach_child(OP)?;
        Ok(RenderPipeline(Arc::new(RenderPipelineInner {
            raw, lifetime: Lifetime::new(), device: self.0.device.clone(), reflection: options.reflection.then(|| map_reflection(reflection)),
            device_attached: AtomicBool::new(true),
        })))
    }

    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Compiler.destroy")?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.detach_child(); }
        Ok(())
    }
}

fn blend_factor_code(value: BlendFactor) -> i32 {
    match value {
        BlendFactor::Zero => 0, BlendFactor::One => 1, BlendFactor::SourceColor => 2, BlendFactor::OneMinusSourceColor => 3,
        BlendFactor::SourceAlpha => 4, BlendFactor::OneMinusSourceAlpha => 5, BlendFactor::DestinationColor => 6,
        BlendFactor::OneMinusDestinationColor => 7, BlendFactor::DestinationAlpha => 8, BlendFactor::OneMinusDestinationAlpha => 9,
        BlendFactor::SourceAlphaSaturated => 10, BlendFactor::Color => 11, BlendFactor::OneMinusColor => 12, BlendFactor::Alpha => 13,
        BlendFactor::OneMinusAlpha => 14, BlendFactor::Source1Color => 15, BlendFactor::OneMinusSource1Color => 16,
        BlendFactor::Source1Alpha => 17, BlendFactor::OneMinusSource1Alpha => 18,
    }
}
fn blend_operation_code(value: BlendOperation) -> i32 {
    match value { BlendOperation::Add => 0, BlendOperation::Subtract => 1, BlendOperation::ReverseSubtract => 2, BlendOperation::Min => 3, BlendOperation::Max => 4 }
}
fn color_write_bit(value: ColorWrite) -> i32 {
    match value { ColorWrite::Red => 8, ColorWrite::Green => 4, ColorWrite::Blue => 2, ColorWrite::Alpha => 1 }
}

struct RenderPipelineInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    device: Device,
    reflection: Option<RenderPipelineReflection>,
    device_attached: AtomicBool,
}
impl Drop for RenderPipelineInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.detach_child(); }
    }
}
#[derive(Clone)]
pub struct RenderPipeline(Arc<RenderPipelineInner>);
impl RenderPipeline {
    pub fn supports_indirect_command_buffers(&self) -> Result<bool, Error> {
        const OP: &str = "Metal.RenderPipeline.supports_indirect_command_buffers";
        self.0.lifetime.ensure_live(OP)?;
        metal_sys::registry::render_pipeline_state_support_indirect_command_buffers(&self.0.raw).map_err(|failure| sys_error(OP, failure))
    }
    pub fn reflection(&self) -> Option<&RenderPipelineReflection> { self.0.reflection.as_ref() }
    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.RenderPipeline.destroy")?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.detach_child(); }
        Ok(())
    }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Viewport { pub x: f64, pub y: f64, pub width: f64, pub height: f64, pub znear: f64, pub zfar: f64 }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Scissor { pub x: usize, pub y: usize, pub width: usize, pub height: usize }
pub use ogpu_core::render_pass::Cull as CullMode;
pub use ogpu_core::backend::Winding;
pub use ogpu_core::render_pass::Primitive;
pub use ogpu_core::render_pass::IndexType;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResourceUsage { Read, Write, Sample }
pub use ogpu_core::backend::ShaderStage as RenderStage;
#[derive(Clone)]
pub enum RenderResource { Buffer(Buffer), Texture(Texture) }

struct RenderEncoderInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    command_buffer: CommandBuffer,
    pass: RenderPassDescriptor,
    target_width: u64,
    target_height: u64,
    ended: AtomicBool,
}
impl RenderEncoderInner {
    fn finish(&self, end_native: bool) {
        if !self.ended.swap(true, Ordering::AcqRel) {
            if end_native { let _ = metal_sys::registry::render_encoder_end(&self.raw); }
            self.lifetime.destroyed.store(true, Ordering::Release);
            self.command_buffer.lifetime().detach();
            self.pass.lifetime().detach();
        }
    }
}
impl Drop for RenderEncoderInner { fn drop(&mut self) { self.finish(true); } }
#[derive(Clone)]
pub struct RenderEncoder(Arc<RenderEncoderInner>);
impl RenderEncoder {
    pub fn create(command_buffer: &CommandBuffer, pass: &RenderPassDescriptor) -> Result<Self, Error> {
        const OP: &str = "Metal.RenderEncoder.create";
        enter_main(OP)?;
        command_buffer.ensure_recording(OP)?;
        pass.ensure_live(OP)?;
        if !command_buffer.device().same(pass.device()) { return Err(error(OP, ErrorKind::CrossDevice, "render pass belongs to another device")); }
        let color_target = pass.color_attachment();
        let depth_target = pass.depth_attachment();
        let stencil_target = pass.stencil_attachment();
        let raw = metal_sys::registry::command_buffer_render_encoder_from_pass(command_buffer.raw(), pass.raw()).map_err(|failure| sys_error(OP, failure))?;
        command_buffer.lifetime().attach();
        pass.lifetime().attach();
        for texture in [color_target.clone(), depth_target, stencil_target, pass.resolve_texture()].into_iter().flatten() {
            command_buffer.retain_texture(&texture);
        }
        Ok(Self(Arc::new(RenderEncoderInner {
            raw, lifetime: Lifetime::new(), command_buffer: command_buffer.clone(), pass: pass.clone(),
            target_width: pass.width(), target_height: pass.height(), ended: AtomicBool::new(false),
        })))
    }
    fn ensure_open(&self, operation: &str) -> Result<(), Error> { self.0.lifetime.ensure_live(operation) }
    pub fn set_pipeline(&self, pipeline: &RenderPipeline) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_pipeline";
        metal_sys::registry::render_encoder_set_pipeline(&self.0.raw, pipeline.raw()).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_render_pipeline(pipeline);
        Ok(())
    }
    pub fn set_vertex_buffer(&self, index: usize, offset: u64, buffer: &Buffer) -> Result<(), Error> { self.set_buffer("Metal.RenderEncoder.set_vertex_buffer", index, offset, buffer, true) }
    pub fn set_fragment_buffer(&self, index: usize, offset: u64, buffer: &Buffer) -> Result<(), Error> { self.set_buffer("Metal.RenderEncoder.set_fragment_buffer", index, offset, buffer, false) }
    fn set_buffer(&self, operation: &str, index: usize, offset: u64, buffer: &Buffer, vertex: bool) -> Result<(), Error> {
        let index = u64::try_from(index).map_err(|_| error(operation, ErrorKind::InvalidArgument, "buffer index exceeds native bounds"))?;
        let call = if vertex { metal_sys::registry::render_encoder_set_vertex_buffer(&self.0.raw, buffer.raw(), offset, index) } else { metal_sys::registry::render_encoder_set_fragment_buffer(&self.0.raw, buffer.raw(), offset, index) };
        call.map_err(|failure| sys_error(operation, failure))?; self.0.command_buffer.retain_buffer(buffer); Ok(())
    }
    pub fn set_vertex_texture(&self, index: usize, texture: &Texture) -> Result<(), Error> { self.set_texture("Metal.RenderEncoder.set_vertex_texture", index, texture, true) }
    pub fn set_fragment_texture(&self, index: usize, texture: &Texture) -> Result<(), Error> { self.set_texture("Metal.RenderEncoder.set_fragment_texture", index, texture, false) }
    fn set_texture(&self, operation: &str, index: usize, texture: &Texture, vertex: bool) -> Result<(), Error> {
        let index = u64::try_from(index).map_err(|_| error(operation, ErrorKind::InvalidArgument, "texture index exceeds native bounds"))?;
        let call = if vertex { metal_sys::registry::render_encoder_set_vertex_texture(&self.0.raw, texture.raw(), index) } else { metal_sys::registry::render_encoder_set_fragment_texture(&self.0.raw, texture.raw(), index) };
        call.map_err(|failure| sys_error(operation, failure))?; self.0.command_buffer.retain_texture(texture); Ok(())
    }
    pub fn set_vertex_sampler(&self, index: usize, sampler: &Sampler) -> Result<(), Error> { self.set_sampler("Metal.RenderEncoder.set_vertex_sampler", index, sampler, true) }
    pub fn set_fragment_sampler(&self, index: usize, sampler: &Sampler) -> Result<(), Error> { self.set_sampler("Metal.RenderEncoder.set_fragment_sampler", index, sampler, false) }
    fn set_sampler(&self, operation: &str, index: usize, sampler: &Sampler, vertex: bool) -> Result<(), Error> {
        let index = u64::try_from(index).map_err(|_| error(operation, ErrorKind::InvalidArgument, "sampler index exceeds native bounds"))?;
        let call = if vertex { metal_sys::registry::render_encoder_set_vertex_sampler(&self.0.raw, sampler.raw(), index) } else { metal_sys::registry::render_encoder_set_fragment_sampler(&self.0.raw, sampler.raw(), index) };
        call.map_err(|failure| sys_error(operation, failure))?; self.0.command_buffer.retain_sampler(sampler); Ok(())
    }
    pub fn set_depth_stencil_state(&self, state: Option<&DepthStencil>) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_depth_stencil_state";
        if let Some(state) = state {
            state.lifetime().ensure_live(OP)?;
        }
        metal_sys::registry::render_depth_stencil(&self.0.raw, state.map(DepthStencil::raw)).map_err(|failure| sys_error(OP, failure))?;
        if let Some(state) = state { self.0.command_buffer.retain_depth_stencil(state); }
        Ok(())
    }
    pub fn set_viewport(&self, viewport: Viewport) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_viewport";
        self.ensure_open(OP)?;
        if ![viewport.x, viewport.y, viewport.width, viewport.height, viewport.znear, viewport.zfar].iter().all(|value| value.is_finite()) || viewport.width <= 0.0 || viewport.height <= 0.0 || viewport.znear < 0.0 || viewport.znear > 1.0 || viewport.zfar < 0.0 || viewport.zfar > 1.0 || viewport.znear > viewport.zfar {
            return Err(error(OP, ErrorKind::InvalidArgument, "viewport is non-finite, empty, or outside the depth range"));
        }
        metal_sys::registry::render_encoder_set_viewport(&self.0.raw, metal_sys::registry::record::Mtl_viewport { originX: viewport.x, originY: viewport.y, width: viewport.width, height: viewport.height, znear: viewport.znear, zfar: viewport.zfar }).map_err(|failure| sys_error(OP, failure))
    }
    pub fn set_scissor(&self, scissor: Scissor) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_scissor";
        self.ensure_open(OP)?;
        let (x, y, width, height) = (
            u64::try_from(scissor.x).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
            u64::try_from(scissor.y).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
            u64::try_from(scissor.width).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
            u64::try_from(scissor.height).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
        );
        if width == 0 || height == 0 || x > self.0.target_width || width > self.0.target_width - x || y > self.0.target_height || height > self.0.target_height - y {
            return Err(error(OP, ErrorKind::InvalidArgument, "scissor rectangle is outside the render pass"));
        }
        let record = metal_sys::registry::record::Mtl_scissor_rect { x, y, width, height };
        metal_sys::registry::render_encoder_set_scissor(&self.0.raw, record).map_err(|failure| sys_error(OP, failure))
    }
    pub fn set_cull_mode(&self, mode: CullMode) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_cull_mode";
        let code = match mode { CullMode::CullNone => 0, CullMode::CullFront => 1, CullMode::CullBack => 2 }; metal_sys::registry::render_encoder_set_cull_mode(&self.0.raw, code).map_err(|failure| sys_error(OP, failure))
    }
    pub fn set_winding(&self, winding: Winding) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_winding";
        let code = match winding { Winding::Clockwise => 0, Winding::CounterClockwise => 1 }; metal_sys::registry::render_encoder_set_winding(&self.0.raw, code).map_err(|failure| sys_error(OP, failure))
    }
    pub fn use_resources(&self, resources: &[RenderResource], usage: &[ResourceUsage], stages: &[RenderStage]) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.use_resources";
        self.ensure_open(OP)?;
        if resources.is_empty() || usage.is_empty() || stages.is_empty() {
            return Err(error(OP, ErrorKind::InvalidArgument, "resources, usage, and stages must be nonempty"));
        }
        let device = self.0.command_buffer.device();
        let mut raw_resources = Vec::with_capacity(resources.len());
        for resource in resources {
            let (kind, raw, lifetime, resource_device) = match resource {
                RenderResource::Buffer(value) => (0, value.raw(), value.lifetime(), value.device()),
                RenderResource::Texture(value) => (1, value.raw(), value.lifetime(), value.device()),
            };
            lifetime.ensure_live(OP)?;
            if !device.same(resource_device) { return Err(error(OP, ErrorKind::CrossDevice, "render resource belongs to another device")); }
            if raw_resources.iter().any(|(_, previous): &(i32, &metal_sys::Handle)| raw.same(previous)) {
                return Err(error(OP, ErrorKind::InvalidArgument, "render resources contain duplicates"));
            }
            raw_resources.push((kind, raw));
        }
        let usage = usage.iter().fold(0, |bits, value| bits | match value { ResourceUsage::Read => 1, ResourceUsage::Write => 2, ResourceUsage::Sample => 4 });
        let stages = stages.iter().fold(0, |bits, value| bits | match value { RenderStage::Vertex => 1, RenderStage::Fragment => 2 });
        metal_sys::native::pipeline::render_encoder_use_resources(&self.0.raw, &raw_resources, usage, stages).map_err(|failure| sys_error(OP, failure))?;
        for resource in resources {
            match resource {
                RenderResource::Buffer(value) => self.0.command_buffer.retain_buffer(value),
                RenderResource::Texture(value) => self.0.command_buffer.retain_texture(value),
            }
        }
        Ok(())
    }
    pub fn draw_primitives(&self, primitive: Primitive, first: usize, count: usize, instances: usize) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.draw_primitives";
        let first = u64::try_from(first).map_err(|_| error(OP, ErrorKind::InvalidArgument, "draw start exceeds native bounds"))?;
        let count = u64::try_from(count).map_err(|_| error(OP, ErrorKind::InvalidArgument, "draw count exceeds native bounds"))?;
        let instances = u64::try_from(instances).map_err(|_| error(OP, ErrorKind::InvalidArgument, "instance count exceeds native bounds"))?;
        let primitive = primitive_code(primitive);
        metal_sys::registry::render_encoder_draw_primitives(&self.0.raw, primitive, first, count, instances).map_err(|failure| sys_error(OP, failure))
    }
    pub fn draw_triangles(&self, first: usize, count: usize) -> Result<(), Error> { self.draw_primitives(Primitive::TriangleList, first, count, 1) }
    pub fn draw_indexed(&self, primitive: Primitive, index_type: IndexType, index_buffer: &Buffer, index_offset: u64, index_count: u64, instance_count: Option<u64>) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.draw_indexed";
        let instances = instance_count.unwrap_or(1);
        let primitive = primitive_code(primitive);
        let index_type = match index_type { IndexType::Uint16 => 0, IndexType::Uint32 => 1 };
        let call = if instance_count.is_some() {
            metal_sys::registry::render_draw_indexed_instances(&self.0.raw, primitive, index_count, index_type, index_buffer.raw(), index_offset, instances)
        } else {
            metal_sys::registry::render_draw_indexed_basic(&self.0.raw, primitive, index_count, index_type, index_buffer.raw(), index_offset)
        };
        call.map_err(|failure| sys_error(OP, failure))?; self.0.command_buffer.retain_buffer(index_buffer); Ok(())
    }
    pub fn draw_indexed_indirect(&self, primitive: Primitive, index_type: IndexType, index_buffer: &Buffer, index_offset: u64, arguments: &Buffer, arguments_offset: u64) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.draw_indexed_indirect";
        let index_type = match index_type { IndexType::Uint16 => 0, IndexType::Uint32 => 1 };
        metal_sys::registry::render_draw_indexed_indirect(
            &self.0.raw, primitive_code(primitive), index_type, index_buffer.raw(), index_offset, arguments.raw(), arguments_offset,
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_buffer(index_buffer);
        self.0.command_buffer.retain_buffer(arguments);
        Ok(())
    }
    pub fn end_encoding(&self) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.end_encoding";
        metal_sys::registry::render_encoder_end(&self.0.raw).map_err(|failure| sys_error(OP, failure))?;
        self.0.finish(false);
        Ok(())
    }
    pub fn destroyed(&self) -> bool { self.0.lifetime.destroyed.load(Ordering::Acquire) }
}
pub(crate) fn primitive_code(value: Primitive) -> i64 {
    match value { Primitive::PointList => 0, Primitive::LineList => 1, Primitive::LineStrip => 2, Primitive::TriangleList => 3, Primitive::TriangleStrip => 4 }
}

struct BlitEncoderInner { raw: metal_sys::Handle, lifetime: Lifetime, command_buffer: CommandBuffer, ended: AtomicBool }
impl BlitEncoderInner {
    fn finish(&self, end_native: bool) {
        if !self.ended.swap(true, Ordering::AcqRel) {
            if end_native { let _ = metal_sys::registry::blit_encoder_end(&self.raw); }
            self.lifetime.destroyed.store(true, Ordering::Release);
            self.command_buffer.lifetime().detach();
        }
    }
}
impl Drop for BlitEncoderInner { fn drop(&mut self) { self.finish(true); } }
#[derive(Clone)]
pub struct BlitEncoder(Arc<BlitEncoderInner>);
impl BlitEncoder {
    pub fn create(command_buffer: &CommandBuffer) -> Result<Self, Error> {
        const OP: &str = "Metal.BlitEncoder.create";
        enter_main(OP)?;
        command_buffer.ensure_recording(OP)?;
        let raw = metal_sys::registry::command_buffer_blit_encoder(command_buffer.raw()).map_err(|failure| sys_error(OP, failure))?;
        command_buffer.lifetime().attach();
        Ok(Self(Arc::new(BlitEncoderInner { raw, lifetime: Lifetime::new(), command_buffer: command_buffer.clone(), ended: AtomicBool::new(false) })))
    }
    pub fn copy_buffer_to_texture(&self, source: &Buffer, source_offset: u64, source_bytes_per_row: usize, source_bytes_per_image: usize, destination: &Texture, destination_slice: usize, destination_level: usize, region: TextureRegion) -> Result<(), Error> {
        const OP: &str = "Metal.BlitEncoder.copy_buffer_to_texture";
        self.0.lifetime.ensure_live(OP)?; source.lifetime().ensure_live(OP)?; destination.lifetime().ensure_live(OP)?;
        let descriptor = destination.descriptor();
        if descriptor.sample_count != 1 { return Err(error(OP, ErrorKind::Unsupported, "multisample textures do not accept buffer blits")); }
        let slice_count = texture_slices(descriptor).ok_or_else(|| error(OP, ErrorKind::InvalidArgument, "texture slice count overflows"))?;
        if destination_level >= descriptor.mip_levels { return Err(error(OP, ErrorKind::InvalidArgument, "blit destination mip level is outside the texture")); }
        if destination_slice >= slice_count { return Err(error(OP, ErrorKind::InvalidArgument, "blit destination slice is outside the texture")); }
        if region.width == 0 || region.height == 0 || region.depth == 0 { return Err(error(OP, ErrorKind::InvalidArgument, "blit destination region dimensions must be positive")); }
        let mip = |size: usize| (size >> destination_level.min(usize::BITS as usize - 1)).max(1);
        let (width, height, depth) = (mip(descriptor.width), mip(descriptor.height), mip(descriptor.depth));
        if region.x > width || region.width > width - region.x || region.y > height || region.height > height - region.y || region.z > depth || region.depth > depth - region.z {
            return Err(error(OP, ErrorKind::InvalidArgument, "blit destination region exceeds the selected mip level"));
        }
        let texel_bytes = ogpu_core::types::texel_bytes(descriptor.format) as usize;
        let minimum_row = region.width.checked_mul(texel_bytes).ok_or_else(|| error(OP, ErrorKind::InvalidArgument, "blit row size overflows"))?;
        if source_bytes_per_row < minimum_row || source_bytes_per_row % texel_bytes != 0 || source_bytes_per_image == 0 {
            return Err(error(OP, ErrorKind::InvalidArgument, "blit source row pitch is too small, zero, or not block-aligned"));
        }
        let minimum_image = source_bytes_per_row.checked_mul(region.height).ok_or_else(|| error(OP, ErrorKind::InvalidArgument, "blit image size overflows"))?;
        if source_bytes_per_image < minimum_image { return Err(error(OP, ErrorKind::InvalidArgument, "blit source image pitch is smaller than its rows")); }
        let total = (source_bytes_per_image as u128) * (region.depth as u128);
        if total > u64::MAX as u128 || source_offset > source.length() || total as u64 > source.length() - source_offset {
            return Err(error(OP, ErrorKind::InvalidArgument, "blit source range exceeds the buffer"));
        }
        if !self.0.command_buffer.device().same(source.device()) || !self.0.command_buffer.device().same(destination.device()) {
            return Err(error(OP, ErrorKind::CrossDevice, "blit resources belong to another device"));
        }
        let usize_to_u64 = |value: usize| u64::try_from(value).map_err(|_| error(OP, ErrorKind::InvalidArgument, "blit argument exceeds native bounds"));
        let origin = (usize_to_u64(region.x)?, usize_to_u64(region.y)?, usize_to_u64(region.z)?);
        let size = (usize_to_u64(region.width)?, usize_to_u64(region.height)?, usize_to_u64(region.depth)?);
        metal_sys::native::pipeline::copy_buffer_to_texture(
            &self.0.raw, source.raw(), source_offset, usize_to_u64(source_bytes_per_row)?, usize_to_u64(source_bytes_per_image)?,
            destination.raw(), usize_to_u64(destination_slice)?, usize_to_u64(destination_level)?, origin, size,
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_buffer(source);
        self.0.command_buffer.retain_texture(destination);
        Ok(())
    }
    pub fn end_encoding(&self) -> Result<(), Error> {
        const OP: &str = "Metal.BlitEncoder.end_encoding";
        self.0.lifetime.ensure_live(OP)?; metal_sys::registry::blit_encoder_end(&self.0.raw).map_err(|failure| sys_error(OP, failure))?; self.0.finish(false); Ok(())
    }
}

fn texture_slices(descriptor: &crate::native::resource::TextureDescriptor) -> Option<usize> {
    match descriptor.kind {
        TextureKind::Texture1dArray | TextureKind::Texture2dArray | TextureKind::Texture2dMultisampleArray => Some(descriptor.array_length),
        TextureKind::TextureCube => Some(6),
        TextureKind::TextureCubeArray => descriptor.array_length.checked_mul(6),
        _ => Some(1),
    }
}

// ogpu_metal: executing a range of an indirect command buffer.
impl RenderEncoder {
    pub fn execute_indirect_commands(&self, commands: &crate::native::indirect::IndirectCommandBuffer, location: usize, length: usize) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.execute_indirect_commands";
        metal_sys::native::indirect::execute_icb_range(&self.0.raw, commands.raw(), location as u64, length as u64).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_indirect(commands);
        Ok(())
    }
}
