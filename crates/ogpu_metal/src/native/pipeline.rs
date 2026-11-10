use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::native::resource::{error, sys_error, enter_main, Buffer, CommandBuffer, Device, Error, ErrorKind, Lifetime, Texture, TextureRegion};
use crate::native::{descriptors::{DepthStencil, RenderPassDescriptor, Sampler}, library::Library};

pub use ogpu_core::render_pass::Primitive as PrimitiveTopology;
pub use ogpu_core::pipeline::{BlendState, BlendFactor, BlendOperation, ColorWrite, ColorAttachment,
    RenderPipelineOptions, AdmittedRenderPipeline};

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
        let raw = metal_sys::native::pipeline::create_compiler(device.raw()).map_err(|failure| sys_error(OP, failure))?;
        device.attach_child(OP)?;
        Ok(Self(Arc::new(CompilerInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true) })))
    }

    pub fn create_render_pipeline(&self, library: &Library, profile: &AdmittedRenderPipeline) -> Result<RenderPipeline, Error> {
        const OP: &str = "Metal.Compiler.create_render_pipeline";
        enter_main(OP)?;
        self.0.lifetime.ensure_live(OP)?;
        library.lifetime().ensure_live(OP)?;
        if !self.0.device.same(library.device()) { return Err(error(OP, ErrorKind::CrossDevice, "compiler and library belong to different devices")); }
        if !profile.admitted_for(&self.0.device.capabilities()?) {
            return Err(error(OP, ErrorKind::CrossDevice, "pipeline was admitted for different device capabilities"));
        }
        let options = profile.options();
        let (raw, reflection) = metal_sys::native::pipeline::create_render_pipeline(
            &self.0.raw, library.raw(), profile,
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
}

pub use ogpu_core::render_pass::Viewport;
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
        let color_target = pass.color_attachment();
        let depth_target = pass.depth_attachment();
        let stencil_target = pass.stencil_attachment();
        let raw = metal_sys::registry::command_buffer_render_encoder_from_pass(command_buffer.raw(), pass.raw()).map_err(|failure| sys_error(OP, failure))?;
        command_buffer.lifetime().attach();
        pass.lifetime().attach();
        for texture in [color_target, depth_target, stencil_target, pass.resolve_texture()].into_iter().flatten() {
            command_buffer.retain_texture(&texture);
        }
        Ok(Self(Arc::new(RenderEncoderInner {
            raw, lifetime: Lifetime::new(), command_buffer: command_buffer.clone(), pass: pass.clone(),
            ended: AtomicBool::new(false),
        })))
    }
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
    pub fn set_viewport(&self, viewport: ogpu_core::render_pass::AdmittedViewport) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_viewport";
        metal_sys::registry::render_encoder_set_viewport(&self.0.raw, viewport).map_err(|failure| sys_error(OP, failure))
    }
    pub fn set_scissor(&self, scissor: Scissor) -> Result<(), Error> {
        const OP: &str = "Metal.RenderEncoder.set_scissor";
        let (x, y, width, height) = (
            u64::try_from(scissor.x).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
            u64::try_from(scissor.y).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
            u64::try_from(scissor.width).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
            u64::try_from(scissor.height).map_err(|_| error(OP, ErrorKind::InvalidArgument, "scissor exceeds native bounds"))?,
        );
        let record = metal_sys::registry::record::MtlScissorRect { x, y, width, height };
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
        let usage = usage.iter().fold(0, |bits, value| bits | match value { ResourceUsage::Read => 1, ResourceUsage::Write => 2, ResourceUsage::Sample => 4 });
        let stages = stages.iter().fold(0, |bits, value| bits | match value { RenderStage::Vertex => 1, RenderStage::Fragment => 2 });
        fn handle(resource: &RenderResource) -> (i32, &metal_sys::Handle) {
            match resource {
                RenderResource::Buffer(value) => (0, value.raw()),
                RenderResource::Texture(value) => (1, value.raw()),
            }
        }
        if resources.is_empty() {
            // validation rejects it
            metal_sys::native::pipeline::render_encoder_use_resources(&self.0.raw, &[], usage, stages).map_err(|failure| sys_error(OP, failure))?;
        }
        // chunks on the stack: a pass declares its resources every frame
        for chunk in resources.chunks(32) {
            let first = handle(&chunk[0]);
            let mut raw = [first; 32];
            for (slot, resource) in raw.iter_mut().zip(chunk) {
                *slot = handle(resource);
            }
            metal_sys::native::pipeline::render_encoder_use_resources(&self.0.raw, &raw[..chunk.len()], usage, stages).map_err(|failure| sys_error(OP, failure))?;
        }
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
        let raw = metal_sys::registry::command_buffer_blit_encoder(command_buffer.raw()).map_err(|failure| sys_error(OP, failure))?;
        command_buffer.lifetime().attach();
        Ok(Self(Arc::new(BlitEncoderInner { raw, lifetime: Lifetime::new(), command_buffer: command_buffer.clone(), ended: AtomicBool::new(false) })))
    }
    pub fn copy_buffer_to_texture(&self, source: &Buffer, source_offset: u64, source_bytes_per_row: usize, source_bytes_per_image: usize, destination: &Texture, destination_slice: usize, destination_level: usize, region: TextureRegion) -> Result<(), Error> {
        const OP: &str = "Metal.BlitEncoder.copy_buffer_to_texture";
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
        metal_sys::registry::blit_encoder_end(&self.0.raw).map_err(|failure| sys_error(OP, failure))?; self.0.finish(false); Ok(())
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
