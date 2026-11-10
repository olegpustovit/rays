use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::native::resource::{error, sys_error, enter_main, Buffer, Device, Error, ErrorKind, Lifetime};
use crate::native::pipeline::RenderPipeline;
use metal_sys;

pub use ogpu_core::validation::{IcbCommandType as CommandType, IcbDescriptor as Descriptor, AdmittedIcb};

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
    pub fn create(device: &Device, profile: &AdmittedIcb) -> Result<Self, Error> {
        const OP: &str = "Metal.Indirect_command_buffer.create";
        enter_main(OP)?;
        device.ensure_live(OP)?;
        let max_command_count = profile.capacity();
        let descriptor = profile.descriptor();
        let raw = metal_sys::native::create_indirect_command_buffer(device.raw(), profile).map_err(|failure| sys_error(OP, failure))?;
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

    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn retain_buffer(&self, buffer: &Buffer) { self.retain(RetainedResource::Buffer(buffer.clone())); }
    pub(crate) fn retain_pipeline(&self, pipeline: &RenderPipeline) { self.retain(RetainedResource::Pipeline(pipeline.clone())); }

}

struct RenderCommandInner {
    raw: metal_sys::registry::BoundRenderCommand,
    parent: IndirectCommandBuffer,
    // Commands have no dependents; attachment also records their destroyed state.
    parent_attached: AtomicBool,
}
#[cfg(target_pointer_width="64")]
const _:()=assert!(std::mem::size_of::<RenderCommandInner>()==40);
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
    pub fn at(buffer: &IndirectCommandBuffer, index: ogpu_core::validation::AdmittedIcbIndex) -> Result<Self, Error> {
        const OP: &str = "Metal.Indirect_command_buffer.Render_command.at";
        enter_main(OP)?;
        buffer.0.lifetime.ensure_live(OP)?;
        ogpu_core::validation::validate_render_command_types(OP,&buffer.0.command_types)?;
        if !index.belongs_to_capacity(buffer.0.max_command_count) {
            return Err(error(OP, ErrorKind::InvalidArgument, "index was admitted for another indirect command capacity"));
        }
        let raw = metal_sys::registry::indirect_render_command(&buffer.0.raw, index.index() as u64).map_err(|failure| sys_error(OP, failure))?;
        buffer.0.lifetime.attach();
        Ok(Self(Arc::new(RenderCommandInner { raw, parent: buffer.clone(), parent_attached: AtomicBool::new(true) })))
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

    pub fn draw_indexed(&self, primitive: Primitive, index_buffer: &Buffer, range:&ogpu_core::validation::AdmittedIndexRange) -> Result<(), Error> {
        const OP: &str = "Metal.Indirect_command_buffer.Render_command.draw_indexed";
        if !self.0.parent_attached.load(Ordering::Acquire) {return Err(error(OP,ErrorKind::StaleHandle,"indirect render command is destroyed"));}
        metal_sys::native::draw_indexed(&self.0.raw,primitive,index_buffer.raw(),range,0,0)
            .map_err(|failure|sys_error(OP,failure))?;
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
        if self.0.parent_attached.swap(false, Ordering::AcqRel) { self.0.parent.lifetime().detach(); }
        Ok(())
    }
}
