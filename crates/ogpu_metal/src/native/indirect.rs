use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::native::resource::{error, sys_error, enter_main, Buffer, Device, Error, ErrorKind, Lifetime};
use crate::native::pipeline::RenderPipeline;
use metal_sys;

const PRIVATE_TRACKED_RESOURCE_OPTIONS: i64 = (2 << 4) | (2 << 8);
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommandType { IndirectDraw, IndirectDrawIndexed, IndirectConcurrentDispatch, IndirectConcurrentDispatchThreads }
impl CommandType {
    fn bit(self) -> i64 {
        match self { Self::IndirectDraw => 1, Self::IndirectDrawIndexed => 2, Self::IndirectConcurrentDispatch => 32, Self::IndirectConcurrentDispatchThreads => 64 }
    }
}

#[derive(Clone, Debug)]
pub struct Descriptor {
    pub command_types: Vec<CommandType>,
    pub inherit_buffers: bool,
    pub inherit_pipeline_state: bool,
    pub max_vertex_buffer_bind_count: usize,
    pub max_fragment_buffer_bind_count: usize,
    pub max_kernel_buffer_bind_count: usize,
    pub support_ray_tracing: bool,
    pub support_dynamic_attribute_stride: bool,
    pub max_kernel_threadgroup_memory_bind_count: usize,
    pub max_object_buffer_bind_count: usize,
    pub max_mesh_buffer_bind_count: usize,
    pub max_object_threadgroup_memory_bind_count: usize,
    pub inherit_depth_stencil_state: bool,
    pub inherit_depth_bias: bool,
    pub inherit_depth_clip_mode: bool,
    pub inherit_cull_mode: bool,
    pub inherit_front_facing_winding: bool,
    pub inherit_triangle_fill_mode: bool,
    pub support_color_attachment_mapping: bool,
}
impl Descriptor {
    pub fn new(command_types: Vec<CommandType>) -> Self {
        Self {
            command_types,
            inherit_buffers: false,
            inherit_pipeline_state: false,
            max_vertex_buffer_bind_count: 0,
            max_fragment_buffer_bind_count: 0,
            max_kernel_buffer_bind_count: 0,
            support_ray_tracing: false,
            support_dynamic_attribute_stride: false,
            max_kernel_threadgroup_memory_bind_count: 0,
            max_object_buffer_bind_count: 0,
            max_mesh_buffer_bind_count: 0,
            max_object_threadgroup_memory_bind_count: 0,
            inherit_depth_stencil_state: true,
            inherit_depth_bias: true,
            inherit_depth_clip_mode: true,
            inherit_cull_mode: true,
            inherit_front_facing_winding: true,
            inherit_triangle_fill_mode: true,
            support_color_attachment_mapping: false,
        }
    }
}

struct Retained(RetainedResource);
enum RetainedResource { Buffer(Buffer), Pipeline(RenderPipeline) }
impl RetainedResource {
    fn lifetime(&self) -> &Lifetime { match self { Self::Buffer(value) => value.lifetime(), Self::Pipeline(value) => value.lifetime() } }
}
impl Drop for Retained {
    fn drop(&mut self) { self.0.lifetime().detach(); }
}

struct IndirectCommandBufferInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    device: Device,
    max_command_count: usize,
    command_types: Vec<CommandType>,
    retained: RefCell<Vec<Retained>>,
    device_attached: AtomicBool,
}
impl Drop for IndirectCommandBufferInner {
    fn drop(&mut self) {
        
        self.retained.get_mut().clear();
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.detach_child(); }
    }
}

#[derive(Clone)]
pub struct IndirectCommandBuffer(Arc<IndirectCommandBufferInner>);
impl IndirectCommandBuffer {
    pub fn descriptor(command_types: Vec<CommandType>) -> Descriptor { Descriptor::new(command_types) }
    pub fn create(device: &Device, max_command_count: usize, descriptor: &Descriptor) -> Result<Self, Error> {
        const OP: &str = "Metal.Indirect_command_buffer.create";
        enter_main(OP)?;
        device.ensure_live(OP)?;
        if descriptor.command_types.is_empty() {
            return Err(error(OP, ErrorKind::InvalidArgument, "at least one indirect command type is required"));
        }
        if descriptor.command_types.windows(2).any(|pair| pair[0] as u8 >= pair[1] as u8) {
            return Err(error(OP, ErrorKind::InvalidArgument, "indirect command types must be unique and ordered"));
        }
        if max_command_count == 0 { return Err(error(OP, ErrorKind::InvalidArgument, "maximum command count must be positive")); }
        let counts = [
            descriptor.max_vertex_buffer_bind_count,
            descriptor.max_fragment_buffer_bind_count,
            descriptor.max_kernel_buffer_bind_count,
            descriptor.max_kernel_threadgroup_memory_bind_count,
            descriptor.max_object_buffer_bind_count,
            descriptor.max_mesh_buffer_bind_count,
            descriptor.max_object_threadgroup_memory_bind_count,
        ];
        if counts.iter().any(|&count| count > 31) {
            return Err(error(OP, ErrorKind::InvalidArgument, "indirect binding counts must be in [0, 31]"));
        }
        let command_types = descriptor.command_types.iter().fold(0, |bits, command| bits | command.bit());
        let raw_descriptor = metal_sys::native::IndirectCommandBufferDescriptor {
            command_types,
            inherit_buffers: u8::from(descriptor.inherit_buffers),
            inherit_pipeline_state: u8::from(descriptor.inherit_pipeline_state),
            max_vertex_buffer_bind_count: counts[0] as i64,
            max_fragment_buffer_bind_count: counts[1] as i64,
            max_kernel_buffer_bind_count: counts[2] as i64,
            support_ray_tracing: u8::from(descriptor.support_ray_tracing),
            support_dynamic_attribute_stride: u8::from(descriptor.support_dynamic_attribute_stride),
            max_kernel_threadgroup_memory_bind_count: counts[3] as i64,
            max_object_buffer_bind_count: counts[4] as i64,
            max_mesh_buffer_bind_count: counts[5] as i64,
            max_object_threadgroup_memory_bind_count: counts[6] as i64,
            inherit_depth_stencil_state: u8::from(descriptor.inherit_depth_stencil_state),
            inherit_depth_bias: u8::from(descriptor.inherit_depth_bias),
            inherit_depth_clip_mode: u8::from(descriptor.inherit_depth_clip_mode),
            inherit_cull_mode: u8::from(descriptor.inherit_cull_mode),
            inherit_front_facing_winding: u8::from(descriptor.inherit_front_facing_winding),
            inherit_triangle_fill_mode: u8::from(descriptor.inherit_triangle_fill_mode),
            support_color_attachment_mapping: u8::from(descriptor.support_color_attachment_mapping),
        };
        let max = u64::try_from(max_command_count).map_err(|_| error(OP, ErrorKind::InvalidArgument, "maximum command count exceeds native bounds"))?;
        let raw = metal_sys::native::create_indirect_command_buffer(device.raw(), &raw_descriptor, max, PRIVATE_TRACKED_RESOURCE_OPTIONS).map_err(|failure| sys_error(OP, failure))?;
        device.attach_child(OP)?;
        Ok(Self(Arc::new(IndirectCommandBufferInner {
            raw, lifetime: Lifetime::new(), device: device.clone(), max_command_count,
            command_types: descriptor.command_types.clone(), retained: RefCell::new(Vec::new()),
            device_attached: AtomicBool::new(true),
        })))
    }

    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Indirect_command_buffer.destroy")?;
        self.0.retained.borrow_mut().clear();
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.detach_child(); }
        Ok(())
    }

    fn retain(&self, resource: RetainedResource) {
        resource.lifetime().attach();
        self.0.retained.borrow_mut().push(Retained(resource));
    }

    fn validate_range(&self, operation: &str, location: usize, length: usize) -> Result<(), Error> {
        if location > self.0.max_command_count || length > self.0.max_command_count - location {
            Err(error(operation, ErrorKind::InvalidArgument, "range exceeds the indirect command buffer"))
        } else { Ok(()) }
    }

    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
    pub(crate) fn retain_buffer(&self, buffer: &Buffer) { self.retain(RetainedResource::Buffer(buffer.clone())); }
    pub(crate) fn retain_pipeline(&self, pipeline: &RenderPipeline) { self.retain(RetainedResource::Pipeline(pipeline.clone())); }

}

struct RenderCommandInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    parent: IndirectCommandBuffer,
    parent_attached: AtomicBool,
}
impl Drop for RenderCommandInner {
    fn drop(&mut self) {
        
        if self.parent_attached.swap(false, Ordering::AcqRel) { self.parent.lifetime().detach(); }
    }
}

pub use ogpu_core::render_pass::Primitive;
pub use ogpu_core::render_pass::IndexType;

#[derive(Clone)]
pub struct RenderCommand(Arc<RenderCommandInner>);
impl RenderCommand {
    pub fn at(buffer: &IndirectCommandBuffer, index: usize) -> Result<Self, Error> {
        const OP: &str = "Metal.Indirect_command_buffer.Render_command.at";
        enter_main(OP)?;
        buffer.0.lifetime.ensure_live(OP)?;
        if !buffer.0.command_types.contains(&CommandType::IndirectDraw) && !buffer.0.command_types.contains(&CommandType::IndirectDrawIndexed) {
            return Err(error(OP, ErrorKind::InvalidState, "descriptor does not enable render commands"));
        }
        buffer.validate_range(OP, index, 1)?;
        let raw = metal_sys::registry::indirect_render_command(&buffer.0.raw, index as u64).map_err(|failure| sys_error(OP, failure))?;
        buffer.0.lifetime.attach();
        Ok(Self(Arc::new(RenderCommandInner { raw, lifetime: Lifetime::new(), parent: buffer.clone(), parent_attached: AtomicBool::new(true) })))
    }
    pub fn set_pipeline(&self, pipeline: &RenderPipeline) -> Result<(), Error> {
        const OP: &str = "Metal.Indirect_command_buffer.Render_command.set_pipeline";
        metal_sys::registry::indirect_render_command_set_pipeline(&self.0.raw, pipeline.raw()).map_err(|failure| sys_error(OP, failure))?;
        self.0.parent.retain_pipeline(pipeline);
        Ok(())
    }

    fn set_buffer(&self, operation: &'static str, vertex: bool, index: usize, offset: u64, buffer: &Buffer) -> Result<(), Error> {
        if vertex {
            metal_sys::registry::indirect_render_command_set_vertex_buffer(&self.0.raw, buffer.raw(), offset, index).map_err(|failure| sys_error(operation, failure))?;
        } else {
            metal_sys::registry::indirect_render_command_set_fragment_buffer(&self.0.raw, buffer.raw(), offset, index).map_err(|failure| sys_error(operation, failure))?;
        }
        self.0.parent.retain_buffer(buffer);
        Ok(())
    }

    pub fn set_vertex_buffer(&self, index: usize, offset: u64, buffer: &Buffer) -> Result<(), Error> {
        self.set_buffer("Metal.Indirect_command_buffer.Render_command.set_vertex_buffer", true, index, offset, buffer)
    }
    pub fn set_fragment_buffer(&self, index: usize, offset: u64, buffer: &Buffer) -> Result<(), Error> {
        self.set_buffer("Metal.Indirect_command_buffer.Render_command.set_fragment_buffer", false, index, offset, buffer)
    }

    pub fn draw_indexed(&self, primitive: Primitive, index_type: IndexType, index_buffer: &Buffer, index_offset: u64, index_count: u64, instance_count: Option<u64>) -> Result<(), Error> {
        const OP: &str = "Metal.Indirect_command_buffer.Render_command.draw_indexed";
        let instance_count = instance_count.unwrap_or(1);
        let type_code = match index_type { IndexType::Uint16 => 0, IndexType::Uint32 => 1 };
        metal_sys::native::draw_indexed(
            &self.0.raw, crate::native::pipeline::primitive_code(primitive) as i32, index_count, type_code, index_buffer.raw(), index_offset,
            instance_count, 0, 0, self.0.parent.device().registry_id(),
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.parent.retain_buffer(index_buffer);
        Ok(())
    }

    pub fn draw_primitives(&self, primitive: Primitive, vertex_start: usize, vertex_count: usize, instance_count: Option<usize>) -> Result<(), Error> {
        const OP: &str = "Metal.Indirect_command_buffer.Render_command.draw_primitives";
        let instance_count = instance_count.unwrap_or(1);
        let start = u64::try_from(vertex_start).map_err(|_| error(OP, ErrorKind::InvalidArgument, "vertex start exceeds native bounds"))?;
        let count = u64::try_from(vertex_count).map_err(|_| error(OP, ErrorKind::InvalidArgument, "vertex count exceeds native bounds"))?;
        let instances = u64::try_from(instance_count).map_err(|_| error(OP, ErrorKind::InvalidArgument, "instance count exceeds native bounds"))?;
        metal_sys::registry::indirect_render_command_draw_primitives(&self.0.raw, crate::native::pipeline::primitive_code(primitive), start, count, instances, 0).map_err(|failure| sys_error(OP, failure))
    }

    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Indirect_command_buffer.Render_command.destroy")?;
        if self.0.parent_attached.swap(false, Ordering::AcqRel) { self.0.parent.lifetime().detach(); }
        Ok(())
    }
}
