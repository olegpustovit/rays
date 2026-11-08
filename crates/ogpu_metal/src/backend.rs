//! Ogpu_metal.Backend: OGPU's driver traits over the Metal modules. Tokens name native objects;
//! the checks live in `ogpu_core::backend`, as for Vulkan.
use crate::{
    acceleration::{self, Acceleration},
    blit_encoder, buffer,
    buffer::Buffer,
    command_control::{CommandContext, IcbEntry, OpenEncoder, Resource, Tables},
    compute_encoder,
    device::{fail, of_metal_error, Device},
    library::{self, Library},
    pipeline::{self, Native, Pipeline},
    queue::{self, Cleanup, Queue},
    render_encoder, sampler,
    surface::{self, Acquire, Frame, PendingPresentation, Surface},
    texture::{self, Texture},
};
use ogpu_core::{
    backend::{
        AccelSizes, Driver, DriverAccel, DriverAccelDescriptor, DriverAccelEncoder, DriverAcquire, DriverArgument,
        DriverBlitEncoder, DriverCommands, DriverComputeEncoder, DriverDevice, DriverFrame, DriverIcb,
        DriverLibrary, DriverMapped, DriverMapping, DriverPipeline, DriverQueue, DriverRenderEncoder, DriverRenderTarget,
        DriverResource, DriverSampler, DriverSurface, Floats, GpuTiming, InstanceKind, OwnedReadback, Receipt,
        RenderPipelineOptions, ShaderStage, Token,
    },
    caps::Caps,
    error::{Error, ErrorKind},
    handle,
    pipeline::RenderDescriptor,
    render_pass::{IndexType, Primitive},
    shader::{Binding, ConstantValue, Shader},
    surface::Configuration,
    types::{BufferDescriptor, Memory, SamplerDescriptor, TextureDescriptor, TextureFormat},
};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

const PENDING_PRESENTATION_CAPACITY: usize = 3;

/// One device's driver state: the token tables and everything the driver objects share.
struct Dev {
    device: Device,
    t: Rc<Tables>,
    libraries: RefCell<HashMap<i64, Library>>,
    active_queues: RefCell<HashMap<i64, Rc<Vec<PendingPresentation>>>>,
    acquired_frames: RefCell<HashMap<i64, (Surface, Frame)>>,
    cleanup_error: RefCell<Option<Error>>,
    device_live: Rc<Cell<bool>>,
}

impl Dev {
    fn take_cleanup_error(&self) -> Option<Error> {
        self.cleanup_error.borrow_mut().take()
    }
    fn record_cleanup(&self, failure: Option<metal::Error>) {
        if let Some(e) = failure {
            let mut slot = self.cleanup_error.borrow_mut();
            if slot.is_none() {
                *slot = Some(of_metal_error("Ogpu_metal.Backend.cleanup", &e));
            }
        }
    }
    /// A completion result, or else the cleanup error recorded since.
    fn commit_completed(&self, completion: Result<(), Error>) -> Result<(), Error> {
        let cleanup = self.take_cleanup_error();
        match (completion, cleanup) {
            (Err(e), _) => Err(e),
            (Ok(()), Some(e)) => Err(e),
            (Ok(()), None) => Ok(()),
        }
    }
}

fn buffer_memory(memory: Memory) -> Result<buffer::Memory, Error> {
    match memory {
        Memory::Shared => Ok(buffer::Memory::Shared),
        Memory::DeviceLocal => Ok(buffer::Memory::DeviceLocal),
        Memory::Memoryless => Err(fail("Ogpu_metal.Backend.create_buffer", ErrorKind::InvalidArgument, "memoryless buffers do not exist")),
    }
}

/// The host view a mapped buffer made on first ask; its buffer clears it on destroy.
type View = Rc<RefCell<Option<Floats>>>;

struct MetalBuffer {
    dev: Rc<Dev>,
    value: Buffer,
    id: i64,
    view: Option<View>,
}

impl DriverResource for MetalBuffer {
    fn token(&self) -> Token {
        self.id
    }
    fn write(&self, offset: i64, bytes: &[u8]) -> Result<(), Error> {
        self.value.write_bytes(&self.dev.device, offset, bytes)
    }
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, Error> {
        self.value.read_bytes(&self.dev.device, offset, length as i64)
    }
    fn read_into(&self, offset: i64, destination: &mut [u8], destination_offset: usize, length: usize) -> Result<(), Error> {
        let bytes = self.value.read_bytes(&self.dev.device, offset, length as i64)?;
        if length > destination.len().saturating_sub(destination_offset) || destination_offset > destination.len() {
            return Err(fail("Ogpu_metal.Backend.buffer.read_into", ErrorKind::InvalidArgument, "destination range is invalid"));
        }
        destination[destination_offset..destination_offset + length].copy_from_slice(&bytes);
        Ok(())
    }
    fn destroy(&self) -> Result<(), Error> {
        if let Some(view) = &self.view {
            *view.borrow_mut() = None;
        }
        self.value.destroy()?;
        self.dev.t.resources.borrow_mut().remove(&self.id);
        Ok(())
    }
}

fn register_buffer(dev: &Rc<Dev>, created: Result<Buffer, Error>) -> Result<MetalBuffer, Error> {
    let value = created?;
    let id = dev.t.token();
    dev.t.resources.borrow_mut().insert(id, Resource::Buffer(value.clone()));
    Ok(MetalBuffer { dev: dev.clone(), value, id, view: None })
}

fn create_buffer(dev: &Rc<Dev>, memory: Memory, descriptor: &BufferDescriptor) -> Result<Box<dyn DriverResource>, Error> {
    Ok(Box::new(register_buffer(dev, buffer_memory(memory).and_then(|m| buffer::create(&dev.device, m, descriptor)))?))
}

fn floats_to_bytes(floats: &[f32], length: usize) -> Vec<u8> {
    let mut bytes: Vec<u8> = floats.iter().flat_map(|value| value.to_le_bytes()).collect();
    bytes.truncate(length);
    bytes
}

const MAPPED_OP: &str = "Ogpu_metal.Backend.create_mapped_buffer";

struct MetalMapping {
    value: Buffer,
    device: Device,
    view: View,
    readback: OwnedReadback,
    count: usize,
}

impl MetalMapping {
    fn make_view(&self) -> Result<Floats, Error> {
        let mut slot = self.view.borrow_mut();
        if let Some(view) = slot.as_ref() {
            return Ok(view.clone());
        }
        let bytes = self.value.read_bytes(&self.device, 0, (self.count * 4) as i64)?;
        let floats: Vec<f32> = bytes.chunks_exact(4).map(|c| f32::from_le_bytes([c[0], c[1], c[2], c[3]])).collect();
        let view: Floats = Rc::new(RefCell::new(floats));
        *slot = Some(view.clone());
        Ok(view)
    }
}

impl DriverMapping for MetalMapping {
    fn readback(&self) -> OwnedReadback {
        self.readback.clone()
    }
    fn view(&self) -> Option<Floats> {
        self.make_view().ok()
    }
    // shared storage on unified memory: the one view stays (as lib/ogpu_metal does) until the buffer is destroyed
    fn release_view(&self) {}
    fn flush(&self, length: usize) -> Result<(), Error> {
        let Some(floats) = self.view.borrow().clone() else {
            return Err(fail(MAPPED_OP, ErrorKind::InvalidArgument, "no host view was written"));
        };
        let bytes = floats_to_bytes(&floats.borrow(), length);
        self.value.write_bytes(&self.device, 0, &bytes)
    }
    fn invalidate(&self, length: usize) -> Result<(), Error> {
        let floats = self.make_view()?;
        if length > floats.borrow().len() * 4 {
            return Err(fail(MAPPED_OP, ErrorKind::InvalidArgument, "range exceeds the buffer"));
        }
        let bytes = self.value.read_bytes(&self.device, 0, length as i64)?;
        for (slot, chunk) in floats.borrow_mut().iter_mut().zip(bytes.chunks_exact(4)) {
            *slot = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        }
        Ok(())
    }
}

/// A mapped buffer: shared storage, with the host view a copy that `flush` and `invalidate` move
/// across (the Rust `Floats` is a vector, not the native bytes). ponytail: a true alias needs
/// `Floats` to be a native view; the copy is made on the first ask and kept until destroy.
fn create_mapped_buffer(dev: &Rc<Dev>, descriptor: &BufferDescriptor) -> Result<DriverMapped, Error> {
    let value = buffer::create(&dev.device, buffer::Memory::Shared, descriptor)?;
    let mut resource = register_buffer(dev, Ok(value.clone()))?;
    let native_readback = value.metal().owned_readback().map_err(|error| of_metal_error(MAPPED_OP, &error))?;
    let readback: OwnedReadback = std::sync::Arc::new(move |offset, length| {
        native_readback(offset, length).map_err(|error| of_metal_error(MAPPED_OP, &error))
    });
    let view: View = Rc::new(RefCell::new(None));
    resource.view = Some(view.clone());
    Ok(DriverMapped {
        mapped_resource: Box::new(resource),
        mapping: Box::new(MetalMapping { value, device: dev.device.clone(), view, readback, count: (descriptor.size / 4) as usize }),
    })
}

fn texture_memory(host_read: bool, descriptor: &TextureDescriptor) -> Memory {
    if descriptor.sample_count == 1 && host_read { Memory::Shared } else { Memory::DeviceLocal }
}

struct MetalTexture {
    dev: Rc<Dev>,
    value: Texture,
    id: i64,
    host_read: bool,
    height: i64,
}

impl DriverResource for MetalTexture {
    fn token(&self) -> Token {
        self.id
    }
    fn write(&self, _offset: i64, _bytes: &[u8]) -> Result<(), Error> {
        Err(fail("Ogpu_metal.Backend.texture.write", ErrorKind::Unsupported, "use a transfer pass for textures"))
    }
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, Error> {
        let height = self.height;
        if !self.host_read {
            Err(fail("Ogpu_metal.Backend.depth.read", ErrorKind::Unsupported, "depth textures are not host readable"))
        } else if offset != 0 || height <= 0 || (length as i64) % height != 0 {
            Err(fail("Ogpu_metal.Backend.texture.read", ErrorKind::InvalidArgument, "texture read range is invalid"))
        } else {
            self.value.read_bytes(&self.dev.device, 0, length as i64 / height)
        }
    }
    fn read_into(&self, offset: i64, destination: &mut [u8], destination_offset: usize, length: usize) -> Result<(), Error> {
        let height = self.height;
        if !self.host_read {
            Err(fail("Ogpu_metal.Backend.depth.read_into", ErrorKind::Unsupported, "depth textures are not host readable"))
        } else if offset != 0 || destination_offset != 0 || length != destination.len() || height <= 0 || (length as i64) % height != 0 {
            Err(fail("Ogpu_metal.Backend.texture.read_into", ErrorKind::InvalidArgument, "destination must exactly match the texture read range"))
        } else {
            self.value.read_bytes_into(&self.dev.device, 0, length as i64 / height, destination)
        }
    }
    fn destroy(&self) -> Result<(), Error> {
        self.value.destroy()?;
        self.dev.t.resources.borrow_mut().remove(&self.id);
        Ok(())
    }
}

fn register_texture(dev: &Rc<Dev>, host_read: bool, descriptor: &TextureDescriptor, created: Result<Texture, Error>) -> Result<Box<dyn DriverResource>, Error> {
    let value = created?;
    let id = dev.t.token();
    dev.t.resources.borrow_mut().insert(id, Resource::Texture(value.clone()));
    Ok(Box::new(MetalTexture { dev: dev.clone(), value, id, host_read, height: descriptor.height }))
}

fn create_texture_format(dev: &Rc<Dev>, format: TextureFormat, host_read: bool, descriptor: &TextureDescriptor) -> Result<Box<dyn DriverResource>, Error> {
    register_texture(dev, host_read, descriptor, texture::create(&dev.device, texture_memory(host_read, descriptor), format, descriptor))
}

fn max_threads(pipeline: &Pipeline) -> i64 {
    match pipeline.native() {
        Native::Compute(native) => native.max_total_threads() as i64,
        Native::Render(_) => 0,
    }
}

struct MetalPipeline {
    dev: Rc<Dev>,
    pipeline: Pipeline,
    id: i64,
    max: i64,
}

impl DriverPipeline for MetalPipeline {
    fn pipeline_token(&self) -> Token {
        self.id
    }
    fn max_threads(&self) -> i64 {
        self.max
    }
    fn destroy_pipeline(&self) -> Result<(), Error> {
        self.pipeline.destroy()?;
        self.dev.t.pipelines.borrow_mut().remove(&self.id);
        Ok(())
    }
}

fn own_pipeline(dev: &Rc<Dev>, pipeline: Pipeline) -> Result<Box<dyn DriverPipeline>, Error> {
    let id = dev.t.token();
    dev.t.pipelines.borrow_mut().insert(id, pipeline.clone());
    let max = max_threads(&pipeline);
    Ok(Box::new(MetalPipeline { dev: dev.clone(), pipeline, id, max }))
}

struct MetalLibrary {
    dev: Rc<Dev>,
    value: Library,
    id: i64,
}

impl DriverLibrary for MetalLibrary {
    fn create_compute_pipeline_in(&self, entry: &str, constants: &[(String, ConstantValue)], interface: &[Binding]) -> Result<Box<dyn DriverPipeline>, Error> {
        let pipeline = pipeline::create_compute_from_library(&self.dev.device, &self.value, entry, constants, interface)?;
        own_pipeline(&self.dev, pipeline)
    }
    fn destroy_library(&self) -> Result<(), Error> {
        self.value.destroy()?;
        self.dev.libraries.borrow_mut().remove(&self.id);
        Ok(())
    }
}

fn create_library(dev: &Rc<Dev>, shader: &Shader) -> Result<Box<dyn DriverLibrary>, Error> {
    let value = library::create(&dev.device, shader)?;
    let id = dev.t.token();
    dev.libraries.borrow_mut().insert(id, value.clone());
    Ok(Box::new(MetalLibrary { dev: dev.clone(), value, id }))
}

fn warm_library(dev: &Rc<Dev>, shader: &Shader) -> Option<ogpu_core::backend::Warm> {
    let source = std::str::from_utf8(ogpu_core::shader::bytes(shader)).ok()?;
    let entry = &ogpu_core::shader::entry_points(shader).first()?.name;
    let warm = metal::Library::warm(dev.device.metal(), source, entry).ok()?;
    Some(Box::new(move || warm.state()))
}

struct MetalAccel {
    dev: Rc<Dev>,
    structure: Acceleration,
    id: i64,
    sizes: AccelSizes,
}

impl DriverAccel for MetalAccel {
    fn accel_token(&self) -> Token {
        self.id
    }
    fn accel_sizes(&self) -> AccelSizes {
        self.sizes
    }
    fn destroy_accel(&self) -> Result<(), Error> {
        self.structure.destroy()?;
        self.dev.t.accels.borrow_mut().remove(&self.id);
        Ok(())
    }
}

fn create_accel(dev: &Rc<Dev>, descriptor: &DriverAccelDescriptor) -> Result<Box<dyn DriverAccel>, Error> {
    const OP: &str = "Ogpu_metal.Backend.create_accel";
    let resolve_buffer = |token: Token| -> Result<Buffer, Error> {
        match dev.t.resource(token) {
            Some(Resource::Buffer(b)) => Ok(b),
            _ => Err(fail(OP, ErrorKind::InvalidArgument, "acceleration buffer graph is incomplete")),
        }
    };
    let resolve_structure = |token: Token| -> Result<Acceleration, Error> {
        match dev.t.accels.borrow().get(&token) {
            Some(s) => Ok(s.clone()),
            None => Err(fail(OP, ErrorKind::InvalidArgument, "acceleration structure graph is incomplete")),
        }
    };
    let structure = acceleration::create(&dev.device, descriptor, &resolve_buffer, &resolve_structure)?;
    let id = dev.t.token();
    dev.t.accels.borrow_mut().insert(id, structure.clone());
    let sizes = structure.sizes();
    Ok(Box::new(MetalAccel {
        dev: dev.clone(),
        structure,
        id,
        sizes: AccelSizes {
            structure_size: sizes.acceleration_structure_size as i64,
            build_scratch_size: sizes.build_scratch_buffer_size as i64,
            refit_scratch_size: sizes.refit_scratch_buffer_size as i64,
        },
    }))
}

fn instance_layout(kind: InstanceKind) -> Vec<i64> {
    let kind = match kind {
        InstanceKind::UserIdInstances => metal::acceleration::InstanceKind::UserIdInstances,
        InstanceKind::MotionInstances => metal::acceleration::InstanceKind::MotionInstances,
    };
    let layout = metal::acceleration::BuildDescriptor::instance_layout(kind).expect("Metal instance layout");
    vec![
        layout.size as i64,
        layout.transform,
        layout.options,
        layout.mask,
        layout.structure_index,
        layout.user_id,
        layout.transforms_start,
        layout.transforms_count,
        layout.start_border_offset,
        layout.end_border_offset,
        layout.start_time_offset,
        layout.end_time_offset,
    ]
}

struct MetalSampler {
    dev: Rc<Dev>,
    value: sampler::Sampler,
    id: i64,
}

impl DriverSampler for MetalSampler {
    fn sampler_token(&self) -> Token {
        self.id
    }
    fn destroy_sampler(&self) -> Result<(), Error> {
        self.value.destroy()?;
        self.dev.t.samplers.borrow_mut().remove(&self.id);
        Ok(())
    }
}

fn create_sampler(dev: &Rc<Dev>, descriptor: &SamplerDescriptor) -> Result<Box<dyn DriverSampler>, Error> {
    let value = sampler::create(&dev.device, descriptor)?;
    let id = dev.t.token();
    dev.t.samplers.borrow_mut().insert(id, value.clone());
    Ok(Box::new(MetalSampler { dev: dev.clone(), value, id }))
}

fn create_render_pipeline(dev: &Rc<Dev>, options: RenderPipelineOptions, descriptor: &RenderDescriptor) -> Result<Box<dyn DriverPipeline>, Error> {
    let topology = match options.topology {
        Primitive::PointList => metal::pipeline::PrimitiveTopology::Point,
        _ => metal::pipeline::PrimitiveTopology::Triangle,
    };
    let pipeline = pipeline::create_render_owned(options.indirect, topology, options.blend, &dev.device, descriptor)?;
    own_pipeline(dev, pipeline)
}

fn icb_primitive(value: Primitive) -> metal::indirect::Primitive {
    match value {
        Primitive::PointList => metal::indirect::Primitive::Point,
        Primitive::LineList => metal::indirect::Primitive::Line,
        Primitive::TriangleList => metal::indirect::Primitive::Triangle,
        Primitive::TriangleStrip => metal::indirect::Primitive::TriangleStrip,
    }
}

const ICB_OP: &str = "Ogpu_metal.Backend.create_icb";

struct MetalIcb {
    dev: Rc<Dev>,
    entry: Rc<IcbEntry>,
    id: i64,
}

impl MetalIcb {
    fn command(&self, index: i64) -> Result<metal::RenderCommand, Error> {
        let index = index as usize;
        if let Some(Some(c)) = self.entry.commands.borrow().get(index) {
            return Ok(c.clone());
        }
        let c = metal::RenderCommand::at(&self.entry.icb, index).map_err(|value| of_metal_error(ICB_OP, &value))?;
        self.entry.commands.borrow_mut()[index] = Some(c.clone());
        Ok(c)
    }

    fn find_buffer(&self, token: Token) -> Result<Buffer, Error> {
        match self.dev.t.resource(token) {
            Some(Resource::Buffer(b)) => Ok(b),
            _ => Err(fail(ICB_OP, ErrorKind::InvalidArgument, "buffer graph is incomplete")),
        }
    }
}

impl DriverIcb for MetalIcb {
    fn icb_token(&self) -> Token {
        self.id
    }
    fn icb_set_pipeline(&self, index: i64, token: Token) -> Result<(), Error> {
        let Some(pipeline) = self.dev.t.pipelines.borrow().get(&token).cloned() else {
            return Err(fail(ICB_OP, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
        };
        match pipeline.native() {
            Native::Compute(_) => Err(fail(ICB_OP, ErrorKind::InvalidArgument, "compute pipeline cannot draw")),
            Native::Render(native) => self.command(index)?.set_pipeline(native).map_err(|value| of_metal_error(ICB_OP, &value)),
        }
    }
    fn icb_set_buffer(&self, index: i64, stage: ShaderStage, slot: i64, offset: i64, token: Token) -> Result<(), Error> {
        let command = self.command(index)?;
        let buffer = self.find_buffer(token)?;
        match stage {
            ShaderStage::Vertex => command.set_vertex_buffer(slot as usize, offset as u64, buffer.metal()),
            ShaderStage::Fragment => command.set_fragment_buffer(slot as usize, offset as u64, buffer.metal()),
        }.map_err(|value| of_metal_error(ICB_OP, &value))
    }
    fn icb_draw(&self, index: i64, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.command(index)?.draw_primitives(icb_primitive(primitive), first as usize, count as usize, Some(instances as usize)).map_err(|value| of_metal_error(ICB_OP, &value))
    }
    fn icb_draw_indexed(&self, index: i64, primitive: Primitive, index_type: IndexType, token: Token, offset: i64, count: i64, instances: i64) -> Result<(), Error> {
        let command = self.command(index)?;
        let index_buffer = self.find_buffer(token)?;
        command.draw_indexed(
            icb_primitive(primitive),
            match index_type {
                IndexType::Uint16 => metal::indirect::IndexType::Uint16,
                IndexType::Uint32 => metal::indirect::IndexType::Uint32,
            },
            index_buffer.metal(),
            offset as u64,
            count as u64,
            Some(instances as u64),
        ).map_err(|value| of_metal_error(ICB_OP, &value))
    }
    fn destroy_icb(&self) -> Result<(), Error> {
        for slot in self.entry.commands.borrow_mut().iter_mut() {
            if let Some(command) = slot.take() {
                let _ = command.destroy();
            }
        }
        self.entry.icb.destroy().map_err(|value| of_metal_error(ICB_OP, &value))?;
        self.dev.t.icbs.borrow_mut().remove(&self.id);
        Ok(())
    }
}

fn create_icb(dev: &Rc<Dev>, max_commands: i64) -> Result<Box<dyn DriverIcb>, Error> {
    let mut descriptor = metal::IndirectCommandBuffer::descriptor(vec![metal::indirect::CommandType::IndirectDraw, metal::indirect::CommandType::IndirectDrawIndexed]);
    descriptor.inherit_buffers = false;
    descriptor.inherit_pipeline_state = false;
    descriptor.max_vertex_buffer_bind_count = 8;
    descriptor.max_fragment_buffer_bind_count = 8;
    let icb = metal::IndirectCommandBuffer::create(dev.device.metal(), max_commands as usize, &descriptor).map_err(|value| of_metal_error(ICB_OP, &value))?;
    let id = dev.t.token();
    let entry = Rc::new(IcbEntry { icb, commands: RefCell::new(vec![None; max_commands as usize]) });
    dev.t.icbs.borrow_mut().insert(id, entry.clone());
    Ok(Box::new(MetalIcb { dev: dev.clone(), entry, id }))
}

const ARGUMENT_OP: &str = "Ogpu_metal.Backend.create_argument";

struct MetalArgument {
    dev: Rc<Dev>,
    pipeline: Pipeline,
    index: i64,
    encoder: RefCell<metal::ShaderArgumentEncoder>,
    length: i64,
    alignment: i64,
}

impl MetalArgument {
    /// A Metal argument encoder keeps its last-set resources alive; encode each slice through a
    /// short-lived encoder so evicted textures can be released.
    fn encode(&self, buffer: Token, offset: i64, resource: metal::ShaderArgumentResource, slot: i64) -> Result<(), Error> {
        let Some(Resource::Buffer(buffer)) = self.dev.t.resource(buffer) else {
            return Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "buffer graph is incomplete"));
        };
        let mut scratch = self.pipeline.argument_encoder(self.index as u64)?;
        let done = scratch
            .set_argument_buffer(buffer.metal(), offset as u64)
            .and_then(|()| scratch.set(slot as u64, resource));
        let _ = scratch.destroy();
        done.map_err(|value| of_metal_error(ARGUMENT_OP, &value))
    }
}

impl DriverArgument for MetalArgument {
    fn argument_length(&self) -> i64 {
        self.length
    }
    fn argument_alignment(&self) -> i64 {
        self.alignment
    }
    fn argument_texture(&self, buffer: Token, offset: i64, slot: i64, texture: Token) -> Result<(), Error> {
        match self.dev.t.resource(texture) {
            Some(Resource::Texture(texture)) => self.encode(buffer, offset, metal::ShaderArgumentResource::Texture(texture.metal().clone()), slot),
            _ => Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "texture graph is incomplete")),
        }
    }
    fn argument_sampler(&self, buffer: Token, offset: i64, slot: i64, sampler: Token) -> Result<(), Error> {
        match self.dev.t.samplers.borrow().get(&sampler) {
            Some(sampler) => self.encode(buffer, offset, metal::ShaderArgumentResource::Sampler(sampler.metal().clone()), slot),
            None => Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "sampler graph is incomplete")),
        }
    }
    fn destroy_argument(&self) -> Result<(), Error> {
        self.encoder.borrow_mut().destroy().map_err(|value| of_metal_error(ARGUMENT_OP, &value))
    }
}

fn create_argument(dev: &Rc<Dev>, pipeline: Token, stage: ShaderStage, index: i64) -> Result<Box<dyn DriverArgument>, Error> {
    let Some(pipeline) = dev.t.pipelines.borrow().get(&pipeline).cloned() else {
        return Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
    };
    if stage == ShaderStage::Vertex {
        return Err(fail(ARGUMENT_OP, ErrorKind::Unsupported, "argument encoders are exposed for fragment functions"));
    }
    let encoder = RefCell::new(pipeline.argument_encoder(index as u64)?);
    let (length, alignment) = (encoder.borrow().encoded_length() as i64, encoder.borrow().alignment() as i64);
    Ok(Box::new(MetalArgument { dev: dev.clone(), pipeline, index, encoder, length, alignment }))
}

fn run_all(retained: &RefCell<Vec<Cleanup>>) {
    for release in std::mem::take(&mut *retained.borrow_mut()) {
        release();
    }
}

struct AccelRecorder {
    context: Rc<CommandContext>,
    encoder: metal::AccelerationEncoder,
}

fn accel_encoder(context: &Rc<CommandContext>) -> Result<Box<dyn DriverAccelEncoder>, Error> {
    context.recording()?;
    let encoder = metal::AccelerationEncoder::create(&context.native).map_err(|value| of_metal_error(context.operation, &value))?;
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Accel(encoder.clone()));
    Ok(Box::new(AccelRecorder { context: context.clone(), encoder }))
}

impl DriverAccelEncoder for AccelRecorder {
    fn build(&self, token: Token, scratch: Token, scratch_offset: i64) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(token)?;
        let scratch = c.find_buffer(scratch)?;
        c.keep_accel(&structure)?;
        c.keep_buffer(&scratch)?;
        structure.encode_build(&self.encoder, &c.device, &scratch, scratch_offset)
    }
    fn compact(&self, src: Token, dst: Token) -> Result<(), Error> {
        let c = &self.context;
        let (src, dst) = (c.find_accel(src)?, c.find_accel(dst)?);
        c.keep_accel(&src)?;
        c.keep_accel(&dst)?;
        Acceleration::encode_compact(&self.encoder, &c.device, &src, &dst)
    }
    fn write_compacted_size(&self, token: Token, dst: Token, offset: i64) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(token)?;
        let destination = c.find_buffer(dst)?;
        c.keep_accel(&structure)?;
        c.keep_buffer(&destination)?;
        structure.encode_compacted_size(&self.encoder, &c.device, &destination, offset)
    }
    fn refit(&self, token: Token, descriptor: &DriverAccelDescriptor, scratch: Token, scratch_offset: i64) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(token)?;
        let scratch = c.find_buffer(scratch)?;
        c.keep_accel(&structure)?;
        c.keep_buffer(&scratch)?;
        let resolve = |token: Token| -> Result<Buffer, Error> {
            let b = c.find_buffer(token)?;
            c.keep_buffer(&b)?;
            Ok(b)
        };
        structure.encode_refit(&self.encoder, &c.device, descriptor, &resolve, &scratch, scratch_offset)
    }
    fn end_accel(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        self.encoder.end_encoding().map_err(|value| of_metal_error(self.context.operation, &value))
    }
}

struct MetalCommands {
    dev: Rc<Dev>,
    queue: Queue,
    active: Rc<Vec<PendingPresentation>>,
    context: Rc<CommandContext>,
}

impl DriverCommands for MetalCommands {
    fn compute_encoder(&self) -> Result<Box<dyn DriverComputeEncoder>, Error> {
        compute_encoder::create(&self.context)
    }
    fn accel_encoder(&self) -> Result<Box<dyn DriverAccelEncoder>, Error> {
        accel_encoder(&self.context)
    }
    fn blit_encoder(&self) -> Result<Box<dyn DriverBlitEncoder>, Error> {
        blit_encoder::create(&self.context)
    }
    fn render_encoder(&self, target: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error> {
        render_encoder::create(&self.context, target)
    }
    fn commit(&self) -> Result<Receipt, Error> {
        let context = &self.context;
        context.recording()?;
        let mut retained = std::mem::take(&mut *context.retained.borrow_mut());
        match self.queue.submit_native(&context.native, &mut retained) {
            Err(e) => {
                *context.retained.borrow_mut() = retained;
                Err(e)
            }
            Ok(receipt) => {
                context.finished.set(true);
                Ok(Receipt { epoch: receipt.epoch })
            }
        }
    }
    fn commit_present(&self, source: Token, frame: DriverFrame) -> Result<Receipt, Error> {
        let context = &self.context;
        context.recording()?;
        let source = context.find_texture(source)?;
        let Some((surface, native_frame)) = self.dev.acquired_frames.borrow().get(&frame.frame_token).cloned() else {
            return Err(fail(context.operation, ErrorKind::InvalidState, "frame token is stale"));
        };
        let Some(pending) = self.active.iter().find(|p| p.available()) else {
            return Err(fail(context.operation, ErrorKind::Capacity, "bounded presentation slots are all in flight"));
        };
        pending.prepare_present(&surface, &native_frame, &source)?;
        if let Err(e) = pending.presentation_encoder(&context.native) {
            pending.rollback_present();
            return Err(e);
        }
        let mut retained = std::mem::take(&mut *context.retained.borrow_mut());
        match self.queue.submit_native(&context.native, &mut retained) {
            Err(e) => {
                *context.retained.borrow_mut() = retained;
                pending.rollback_present();
                Err(e)
            }
            Ok(receipt) => {
                context.finished.set(true);
                pending.commit_present(receipt.epoch);
                self.dev.acquired_frames.borrow_mut().remove(&frame.frame_token);
                Ok(Receipt { epoch: receipt.epoch })
            }
        }
    }
    fn abandon(&self) -> Result<(), Error> {
        let context = &self.context;
        context.recording()?;
        let open = context.open_encoder.borrow_mut().take();
        if let Some(open) = open {
            open.end();
        }
        context.finished.set(true);
        run_all(&context.retained);
        context.native.destroy().map_err(|value| of_metal_error(context.operation, &value))
    }
}

fn begin_commands(dev: &Rc<Dev>, queue: &Queue, active: &Rc<Vec<PendingPresentation>>) -> Result<Box<dyn DriverCommands>, Error> {
    let operation = "Ogpu_metal.Backend.begin_commands";
    let native = metal::CommandBuffer::create(queue.metal()).map_err(|value| of_metal_error(operation, &value))?;
    let context = Rc::new(CommandContext {
        operation,
        device: dev.device.clone(),
        native,
        tables: dev.t.clone(),
        commands_token: dev.t.token(),
        open_encoder: RefCell::new(None),
        finished: Cell::new(false),
        retained: RefCell::new(Vec::new()),
    });
    Ok(Box::new(MetalCommands { dev: dev.clone(), queue: queue.clone(), active: active.clone(), context }))
}

struct MetalQueue {
    dev: Rc<Dev>,
    queue: Queue,
    active: Rc<Vec<PendingPresentation>>,
    token: i64,
}

impl DriverQueue for MetalQueue {
    fn complete_through(&self, epoch: i64) -> Result<(), Error> {
        let waited = self.queue.wait_through(epoch);
        surface::complete_presentations_through(&self.active, self.queue.completed_epoch());
        self.dev.commit_completed(waited)
    }
    fn poll_through(&self, epoch: i64) -> Result<bool, Error> {
        let polled = self.queue.poll_through(epoch);
        let completed = self.queue.completed_epoch();
        if epoch > 0 && completed >= epoch {
            surface::complete_presentations_through(&self.active, completed);
            self.dev.commit_completed(polled.map(|_| ())).map(|()| true)
        } else {
            polled
        }
    }
    fn completed_epoch(&self) -> i64 {
        self.queue.completed_epoch()
    }
    fn begin_commands(&self) -> Result<Box<dyn DriverCommands>, Error> {
        begin_commands(&self.dev, &self.queue, &self.active)
    }
    fn gpu_duration(&self, epoch: i64) -> Option<f64> {
        self.queue.gpu_duration(epoch)
    }
    fn gpu_timing(&self) -> GpuTiming {
        GpuTiming { gpu_seconds: queue::gpu_timing_for_device(&self.dev.device).duration_seconds }
    }
    fn destroy_queue(&self) -> Result<(), Error> {
        if !self.active.iter().all(|p| p.available()) {
            return Err(fail("Ogpu_metal.Backend.destroy_queue", ErrorKind::InvalidState, "queue has presentations in flight"));
        }
        self.queue.destroy()?;
        surface::clear_pending_presentations(&self.active);
        self.dev.active_queues.borrow_mut().remove(&self.token);
        match self.dev.take_cleanup_error() {
            None => Ok(()),
            Some(e) => Err(e),
        }
    }
}

fn create_queue(dev: &Rc<Dev>) -> Result<Box<dyn DriverQueue>, Error> {
    let queue = queue::create(&dev.device)?;
    let token = dev.t.token();
    let active: Rc<Vec<PendingPresentation>> = Rc::new((0..PENDING_PRESENTATION_CAPACITY).map(|_| surface::create_pending_presentation()).collect());
    dev.active_queues.borrow_mut().insert(token, active.clone());
    Ok(Box::new(MetalQueue { dev: dev.clone(), queue, active, token }))
}

struct MetalSurface {
    dev: Rc<Dev>,
    value: Surface,
}

impl DriverSurface for MetalSurface {
    fn configure(&self, configuration: &Configuration) -> Result<(), Error> {
        self.value.configure(configuration)
    }
    fn acquire_sync(&self) -> Result<DriverAcquire, Error> {
        match self.value.acquire()? {
            Acquire::Timeout => Ok(DriverAcquire::Timeout),
            Acquire::Occluded => Ok(DriverAcquire::Occluded),
            Acquire::DeviceLost => Ok(DriverAcquire::DeviceLost),
            Acquire::Acquired(frame) => {
                let frame_token = frame.id();
                self.dev.acquired_frames.borrow_mut().insert(frame_token, (self.value.clone(), frame));
                Ok(DriverAcquire::Acquired(DriverFrame { frame_token }))
            }
        }
    }
    fn discard(&self, frame: DriverFrame) -> Result<(), Error> {
        let Some((_, native)) = self.dev.acquired_frames.borrow().get(&frame.frame_token).cloned() else {
            return Err(fail("Ogpu_metal.Backend.surface", ErrorKind::InvalidState, "frame token is stale"));
        };
        self.value.discard(&native)?;
        self.dev.acquired_frames.borrow_mut().remove(&frame.frame_token);
        Ok(())
    }
    fn destroy_surface(&self) -> Result<(), Error> {
        let frames = self.dev.acquired_frames.borrow().values().filter(|(owner, _)| owner.same(&self.value)).count();
        if frames != 0 {
            Err(fail("Ogpu_metal.Backend.destroy_surface", ErrorKind::InvalidState, "surface has outstanding frames"))
        } else if self.value.in_flight_presentations() != 0 {
            Err(fail("Ogpu_metal.Backend.destroy_surface", ErrorKind::InvalidState, "surface has presentations in flight"))
        } else {
            self.value.destroy();
            self.value.release_layer();
            Ok(())
        }
    }
}

fn create_surface(dev: &Rc<Dev>, configuration: &Configuration) -> Result<Box<dyn DriverSurface>, Error> {
    const OP: &str = "Ogpu_metal.Backend.create_surface";
    let Some(token) = &configuration.layer else {
        return Err(fail(OP, ErrorKind::Unsupported, "surface needs a native layer token"));
    };
    let layer = metal::Surface::adopt_borrowed(
            dev.device.metal(),
            token,
            token.owner_id(),
            token.generation(),
            metal::surface::Configuration::default(configuration.physical_width as usize, configuration.physical_height as usize),
        ).map_err(|value| of_metal_error(OP, &value))?;
    let value = surface::create(&dev.device, layer, configuration)?;
    Ok(Box::new(MetalSurface { dev: dev.clone(), value }))
}

fn destroy_device(dev: &Rc<Dev>) -> Result<(), Error> {
    if !dev.active_queues.borrow().is_empty() {
        return Err(fail("Ogpu_metal.Backend.destroy_device", ErrorKind::InvalidState, "device has active queues"));
    }
    for (_, state) in std::mem::take(&mut *dev.t.depth_states.borrow_mut()) {
        dev.record_cleanup(state.destroy().err());
    }
    let destroyed = if dev.device_live.get() { dev.device.destroy() } else { Ok(()) };
    if destroyed.is_ok() {
        dev.device_live.set(false);
    }
    let cleanup = dev.take_cleanup_error();
    match (destroyed, cleanup) {
        (Err(e), _) => Err(e),
        (Ok(()), Some(e)) => Err(e),
        (Ok(()), None) => Ok(()),
    }
}

fn metal_size(device: &Device, project: fn(&metal::DeviceInfo) -> u64) -> i64 {
    match device.metal().info() {
        Ok(info) => project(&info) as i64,
        Err(_) => 0,
    }
}

struct MetalDevice {
    dev: Rc<Dev>,
    handle: handle::Device,
    caps: Caps,
}

impl DriverDevice for MetalDevice {
    fn device_handle(&self) -> handle::Device {
        self.handle.clone()
    }
    fn capabilities(&self) -> Caps {
        self.caps
    }
    fn allocated_size(&self) -> i64 {
        metal_size(&self.dev.device, |i| i.current_allocated_size)
    }
    fn recommended_working_set(&self) -> i64 {
        metal_size(&self.dev.device, |i| i.recommended_max_working_set_size)
    }
    fn create_buffer(&self, memory: Memory, descriptor: &BufferDescriptor) -> Result<Box<dyn DriverResource>, Error> {
        create_buffer(&self.dev, memory, descriptor)
    }
    fn create_mapped_buffer(&self, descriptor: &BufferDescriptor) -> Result<DriverMapped, Error> {
        create_mapped_buffer(&self.dev, descriptor)
    }
    fn create_texture(&self, descriptor: &TextureDescriptor) -> Result<Box<dyn DriverResource>, Error> {
        create_texture_format(&self.dev, descriptor.format, true, descriptor)
    }
    fn create_depth_texture(&self, descriptor: &TextureDescriptor) -> Result<Box<dyn DriverResource>, Error> {
        create_texture_format(&self.dev, TextureFormat::Depth32Float, false, descriptor)
    }
    fn create_memoryless_texture(&self, depth: bool, descriptor: &TextureDescriptor) -> Result<Box<dyn DriverResource>, Error> {
        register_texture(
            &self.dev,
            false,
            descriptor,
            texture::create(&self.dev.device, Memory::Memoryless, if depth { TextureFormat::Depth32Float } else { descriptor.format }, descriptor),
        )
    }
    fn create_library(&self, shader: &Shader) -> Result<Box<dyn DriverLibrary>, Error> {
        create_library(&self.dev, shader)
    }
    fn warm_library(&self, shader: &Shader) -> Option<ogpu_core::backend::Warm> {
        warm_library(&self.dev, shader)
    }
    fn create_accel(&self, descriptor: &DriverAccelDescriptor) -> Result<Box<dyn DriverAccel>, Error> {
        create_accel(&self.dev, descriptor)
    }
    fn instance_layout(&self, kind: InstanceKind) -> Vec<i64> {
        instance_layout(kind)
    }
    fn create_sampler(&self, descriptor: &SamplerDescriptor) -> Result<Box<dyn DriverSampler>, Error> {
        create_sampler(&self.dev, descriptor)
    }
    fn create_render_pipeline(&self, options: RenderPipelineOptions, descriptor: &RenderDescriptor) -> Result<Box<dyn DriverPipeline>, Error> {
        create_render_pipeline(&self.dev, options, descriptor)
    }
    fn create_icb(&self, max_commands: i64) -> Result<Box<dyn DriverIcb>, Error> {
        create_icb(&self.dev, max_commands)
    }
    fn create_argument(&self, pipeline: Token, stage: ShaderStage, index: i64) -> Result<Box<dyn DriverArgument>, Error> {
        create_argument(&self.dev, pipeline, stage, index)
    }
    fn create_queue(&self) -> Result<Box<dyn DriverQueue>, Error> {
        create_queue(&self.dev)
    }
    fn create_surface(&self, configuration: &Configuration) -> Result<Box<dyn DriverSurface>, Error> {
        create_surface(&self.dev, configuration)
    }
    fn destroy_device(&self) -> Result<(), Error> {
        destroy_device(&self.dev)
    }
}

struct MetalDriver {
    device_live: Rc<Cell<bool>>,
}

impl Driver for MetalDriver {
    fn create_device(&self) -> Result<Box<dyn DriverDevice>, Error> {
        let device_live = &self.device_live;
        if device_live.get() {
            return Err(fail("Ogpu_metal.Backend.create_device", ErrorKind::InvalidState, "adapter already owns a live device"));
        }
        let device = Device::system_default()?;
        device_live.set(true);
        let handle = device.handle().clone();
        let caps = device.capabilities();
        let dev = Rc::new(Dev {
            device,
            t: Rc::new(Tables::new()),
            libraries: RefCell::new(HashMap::new()),
            active_queues: RefCell::new(HashMap::new()),
            acquired_frames: RefCell::new(HashMap::new()),
            cleanup_error: RefCell::new(None),
            device_live: device_live.clone(),
        });
        Ok(Box::new(MetalDevice { dev, handle, caps }))
    }
}

pub fn create() -> Box<dyn Driver> {
    Box::new(MetalDriver { device_live: Rc::new(Cell::new(false)) })
}

/// Native handles still alive on this thread (the release queue drained first).
pub fn live_handles() -> i64 {
    match (metal::release_queue::drain(), metal::release_queue::stats()) {
        (Ok(_), Ok(stats)) => stats.live_handles as i64,
        _ => unreachable!("the release queue answers on the thread that owns the Metal handles"),
    }
}

