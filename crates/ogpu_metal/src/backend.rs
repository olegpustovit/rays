//! Ogpu_metal.Backend: OGPU's driver traits over the Metal modules. Tokens name native objects;
//! the checks live in `ogpu_core::backend`, as for Vulkan.
use ogpu_core::validation::{AdmittedBuffer, AdmittedTexture, TextureRead, TextureReadInto};
use crate::native;
use crate::{
    acceleration::{self, Acceleration},
    blit_encoder, buffer,
    buffer::Buffer,
    command_control::{CommandContext, IcbEntry, OpenEncoder, Tables},
    compute_encoder,
    device::{fail, Device},
    library::{self, Library},
    pipeline::{self, Native, Pipeline},
    queue::{self, Queue},
    render_encoder, sampler,
    surface::{self, Acquire, Frame, PendingPresentation, Surface},
    texture::{self, Texture},
};
use ogpu_core::{
    backend::{
        AccelSizes, Driver, DriverAccel, DriverAccelDescriptor, DriverAccelEncoder, DriverAcquire, DriverArgument,
        DriverBlitEncoder, DriverCommands, DriverComputeEncoder, DriverDevice, DriverFrame, DriverIcb,
        DriverLibrary, DriverMapped, DriverMapping, DriverPipeline, DriverQueue, DriverRenderEncoder, DriverRenderTarget,
        DriverResource, DriverSampler, DriverSurface, InstanceKind, OwnedReadback, Receipt,
        RenderPipelineOptions, ShaderStage, Token,
    },
    caps::Caps,
    error::{Error, ErrorKind},
    handle,
    pipeline::RenderDescriptor,
    render_pass::Primitive,
    shader::{Binding, Shader},
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
    fn record_cleanup(&self, failure: Option<native::Error>) {
        if let Some(e) = failure {
            let mut slot = self.cleanup_error.borrow_mut();
            if slot.is_none() {
                *slot = Some(e);
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

/// The host view a mapped buffer made on first ask; its buffer clears it on destroy.

struct MetalBuffer {
    dev: Rc<Dev>,
    value: Buffer,
    id: i64,
}

impl DriverResource for MetalBuffer {
    fn token(&self) -> Token {
        self.id
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
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
        self.value.destroy()
    }
}

/// The Metal buffer behind a portable resource.
pub(crate) fn metal_buffer(resource: &dyn DriverResource) -> Option<&Buffer> {
    resource.as_any().downcast_ref::<MetalBuffer>().map(|b| &b.value)
}

/// The Metal texture behind a portable resource.
pub(crate) fn metal_texture(resource: &dyn DriverResource) -> Option<&Texture> {
    resource.as_any().downcast_ref::<MetalTexture>().map(|t| &t.value)
}

fn register_buffer(dev: &Rc<Dev>, created: Result<Buffer, Error>) -> Result<MetalBuffer, Error> {
    let value = created?;
    let id = dev.t.token();
    Ok(MetalBuffer { dev: dev.clone(), value, id })
}

fn create_buffer(dev: &Rc<Dev>, descriptor: &AdmittedBuffer) -> Result<Box<dyn DriverResource>, Error> {
    Ok(Box::new(register_buffer(dev, buffer::create(&dev.device, descriptor))?))
}


struct MetalMapping {
    readback: OwnedReadback,
}

impl DriverMapping for MetalMapping {
    fn readback(&self) -> OwnedReadback {
        self.readback.clone()
    }
}

fn create_mapped_buffer(dev: &Rc<Dev>, descriptor: &AdmittedBuffer) -> Result<DriverMapped, Error> {
    let value = buffer::create(&dev.device, descriptor)?;
    let resource = register_buffer(dev, Ok(value.clone()))?;
    let native_readback = value.metal().owned_readback()?;
    let readback: OwnedReadback = std::sync::Arc::new(move |offset, length| {
        native_readback(offset, length)
    });
    Ok(DriverMapped {
        mapped_resource: Box::new(resource),
        mapping: Box::new(MetalMapping { readback }),
    })
}

struct MetalTexture { value: Texture, id: i64 }
impl DriverResource for MetalTexture {
    fn token(&self) -> Token { self.id }
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn write(&self, _: i64, _: &[u8]) -> Result<(), Error> {
        Err(fail("Ogpu_metal.Backend.texture.write", ErrorKind::Unsupported, "use a transfer pass for textures"))
    }
    fn read(&self, _: i64, _: usize) -> Result<Vec<u8>, Error> {
        Err(fail("Ogpu_metal.Backend.texture.read", ErrorKind::InvalidArgument, "texture reads require an admitted layout"))
    }
    fn read_into(&self, _: i64, _: &mut [u8], _: usize, _: usize) -> Result<(), Error> {
        Err(fail("Ogpu_metal.Backend.texture.read_into", ErrorKind::InvalidArgument, "texture reads require an admitted layout"))
    }
    fn read_texture(&self, read: &TextureRead) -> Result<Vec<u8>, Error> { self.value.read_bytes(read) }
    fn read_texture_into(&self, read: &TextureReadInto, destination: &mut [u8]) -> Result<(), Error> {
        self.value.read_bytes_into(read, destination)
    }
    fn destroy(&self) -> Result<(), Error> {
        self.value.destroy()
    }
}
fn create_texture(dev: &Rc<Dev>, descriptor: &AdmittedTexture) -> Result<Box<dyn DriverResource>, Error> {
    let value = texture::create(&dev.device, descriptor)?;
    let id = dev.t.token();
    Ok(Box::new(MetalTexture { value, id }))
}

fn max_threads(pipeline: &Pipeline) -> i64 {
    match pipeline.native() {
        Native::Compute(native) => native.max_total_threads() as i64,
        Native::Render(_) => 0,
    }
}

pub(crate) struct MetalPipeline {
    pub(crate) pipeline: Pipeline,
    max: i64,
}

impl DriverPipeline for MetalPipeline {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn max_threads(&self) -> i64 {
        self.max
    }
    fn destroy_pipeline(&self) -> Result<(), Error> {
        self.pipeline.destroy()
    }
}

fn own_pipeline(pipeline: Pipeline) -> Result<Box<dyn DriverPipeline>, Error> {
    let max = max_threads(&pipeline);
    Ok(Box::new(MetalPipeline { pipeline, max }))
}

struct MetalLibrary {
    dev: Rc<Dev>,
    value: Library,
    id: i64,
}

impl DriverLibrary for MetalLibrary {
    fn create_compute_pipeline_in(&self, function: &ogpu_core::shader::AdmittedFunction, interface: &[Binding]) -> Result<Box<dyn DriverPipeline>, Error> {
        let pipeline = pipeline::create_compute_from_library(&self.dev.device, &self.value, function, interface)?;
        own_pipeline(pipeline)
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
    let warm = native::Library::warm(dev.device.metal(), shader).ok()?;
    Some(Box::new(move || warm.state()))
}

struct MetalAccel {
    structure: Acceleration,
    sizes: AccelSizes,
}

/// The Metal structure behind a portable one.
pub(crate) fn metal_accel(accel: &dyn DriverAccel) -> Option<&Acceleration> {
    accel.as_any().downcast_ref::<MetalAccel>().map(|a| &a.structure)
}

impl DriverAccel for MetalAccel {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn accel_sizes(&self) -> AccelSizes {
        self.sizes
    }
    fn destroy_accel(&self) -> Result<(), Error> {
        self.structure.destroy()
    }
}

fn create_accel(dev: &Rc<Dev>, descriptor: &DriverAccelDescriptor) -> Result<Box<dyn DriverAccel>, Error> {
    const OP: &str = "Ogpu_metal.Backend.create_accel";
    let resolve_buffer = |resource: &dyn DriverResource| -> Result<Buffer, Error> {
        metal_buffer(resource).cloned().ok_or_else(|| fail(OP, ErrorKind::InvalidArgument, "acceleration buffer graph is incomplete"))
    };
    let resolve_structure = |accel: &dyn DriverAccel| -> Result<Acceleration, Error> {
        metal_accel(accel).cloned().ok_or_else(|| fail(OP, ErrorKind::InvalidArgument, "acceleration structure graph is incomplete"))
    };
    let structure = acceleration::create(&dev.device, descriptor, &resolve_buffer, &resolve_structure)?;
    let sizes = structure.sizes();
    Ok(Box::new(MetalAccel {
        structure,
        sizes: AccelSizes {
            structure_size: sizes.acceleration_structure_size as i64,
            build_scratch_size: sizes.build_scratch_buffer_size as i64,
            refit_scratch_size: sizes.refit_scratch_buffer_size as i64,
        },
    }))
}

fn instance_layout(kind: InstanceKind) -> Vec<i64> {
    let kind = match kind { InstanceKind::UserIdInstances => 1, InstanceKind::MotionInstances => 2 };
    metal_sys::native::acceleration::instance_layout(kind).expect("Metal instance layout").to_vec()
}

pub(crate) struct MetalSampler {
    pub(crate) value: sampler::Sampler,
}

impl DriverSampler for MetalSampler {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn destroy_sampler(&self) -> Result<(), Error> {
        self.value.destroy()?;
        Ok(())
    }
}

fn create_sampler(dev: &Rc<Dev>, descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn DriverSampler>, Error> {
    let value = sampler::create(&dev.device, descriptor)?;
    Ok(Box::new(MetalSampler { value }))
}

fn create_render_pipeline(dev: &Rc<Dev>, _options: RenderPipelineOptions, descriptor: &RenderDescriptor, admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Box<dyn DriverPipeline>, Error> {
    let pipeline = pipeline::create_render_owned(&dev.device, descriptor, admitted)?;
    own_pipeline(pipeline)
}

const ICB_OP: &str = "Ogpu_metal.Backend.create_icb";

pub(crate) struct MetalIcb {
    pub(crate) entry: Rc<IcbEntry>,
}

impl MetalIcb {
    fn command(&self, index: ogpu_core::validation::AdmittedIcbIndex) -> Result<native::RenderCommand, Error> {
        let at = index.index();
        if let Some(Some(c)) = self.entry.commands.borrow().get(at) {
            return Ok(c.clone());
        }
        let c = native::RenderCommand::at(&self.entry.icb, index)?;
        self.entry.commands.borrow_mut()[at] = Some(c.clone());
        Ok(c)
    }

    fn find_buffer<'a>(&self, resource: &'a dyn DriverResource) -> Result<&'a Buffer, Error> {
        metal_buffer(resource).ok_or_else(|| fail(ICB_OP, ErrorKind::InvalidArgument, "buffer graph is incomplete"))
    }
}

impl DriverIcb for MetalIcb {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn icb_set_pipeline(&self, index: ogpu_core::validation::AdmittedIcbIndex, pipeline: &dyn DriverPipeline) -> Result<(), Error> {
        let Some(pipeline) = metal_pipeline(pipeline) else {
            return Err(fail(ICB_OP, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
        };
        match pipeline.native() {
            Native::Compute(_) => Err(fail(ICB_OP, ErrorKind::InvalidArgument, "compute pipeline cannot draw")),
            Native::Render(native) => self.command(index)?.set_pipeline(native),
        }
    }
    fn icb_set_buffer(&self, index: ogpu_core::validation::AdmittedIcbIndex, stage: ShaderStage, slot: i64, offset: i64, resource: &dyn DriverResource) -> Result<(), Error> {
        let command = self.command(index)?;
        let buffer = self.find_buffer(resource)?;
        match stage {
            ShaderStage::Vertex => command.set_vertex_buffer(slot as usize, offset as u64, buffer.metal()),
            ShaderStage::Fragment => command.set_fragment_buffer(slot as usize, offset as u64, buffer.metal()),
        }
    }
    fn icb_draw(&self, index: ogpu_core::validation::AdmittedIcbIndex, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.command(index)?.draw_primitives(primitive, first as usize, count as usize, Some(instances as usize))
    }
    fn icb_draw_indexed(&self, index: ogpu_core::validation::AdmittedIcbIndex, primitive: Primitive, resource: &dyn DriverResource, range:&ogpu_core::validation::AdmittedIndexRange) -> Result<(), Error> {
        let command = self.command(index)?;
        let index_buffer = self.find_buffer(resource)?;
        command.draw_indexed(primitive,index_buffer.metal(),range)
    }
    fn destroy_icb(&self) -> Result<(), Error> {
        for slot in self.entry.commands.borrow_mut().iter_mut() {
            if let Some(command) = slot.take() {
                let _ = command.destroy();
            }
        }
        self.entry.icb.destroy()
    }
}

fn create_icb(dev: &Rc<Dev>, profile: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn DriverIcb>, Error> {
    let max_commands = profile.capacity();
    let icb = native::IndirectCommandBuffer::create(dev.device.metal(), profile)?;
    let entry = Rc::new(IcbEntry { icb, commands: RefCell::new(vec![None; max_commands as usize]) });
    Ok(Box::new(MetalIcb { entry }))
}

const ARGUMENT_OP: &str = "Ogpu_metal.Backend.create_argument";

struct MetalArgument {
    pipeline: Pipeline,
    index: ogpu_core::validation::AdmittedSlot,
    encoder: RefCell<native::ShaderArgumentEncoder>,
    length: i64,
    alignment: i64,
}

impl MetalArgument {
    /// A Metal argument encoder keeps its last-set resources alive; encode each slice through a
    /// short-lived encoder so evicted textures can be released.
    fn encode(&self, buffer: &dyn DriverResource, offset: i64, resource: native::ShaderArgumentResource, slot: i64) -> Result<(), Error> {
        let Some(buffer) = metal_buffer(buffer) else {
            return Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "buffer graph is incomplete"));
        };
        let mut scratch = self.pipeline.argument_encoder(self.index)?;
        let done = scratch
            .set_argument_buffer(buffer.metal(), offset as u64)
            .and_then(|()| scratch.set(slot as u64, resource));
        let _ = scratch.destroy();
        done
    }
}

impl DriverArgument for MetalArgument {
    fn argument_length(&self) -> i64 {
        self.length
    }
    fn argument_alignment(&self) -> i64 {
        self.alignment
    }
    fn argument_texture(&self, buffer: &dyn DriverResource, offset: i64, slot: i64, texture: &dyn DriverResource) -> Result<(), Error> {
        match metal_texture(texture) {
            Some(texture) => self.encode(buffer, offset, native::ShaderArgumentResource::Texture(texture.metal().clone()), slot),
            _ => Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "texture graph is incomplete")),
        }
    }
    fn argument_sampler(&self, buffer: &dyn DriverResource, offset: i64, slot: i64, sampler: &dyn DriverSampler) -> Result<(), Error> {
        match sampler.as_any().downcast_ref::<MetalSampler>() {
            Some(sampler) => self.encode(buffer, offset, native::ShaderArgumentResource::Sampler(sampler.value.metal().clone()), slot),
            None => Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "sampler graph is incomplete")),
        }
    }
    fn destroy_argument(&self) -> Result<(), Error> {
        self.encoder.borrow_mut().destroy()
    }
}

/// The Metal pipeline behind a portable one.
pub(crate) fn metal_pipeline(pipeline: &dyn DriverPipeline) -> Option<&Pipeline> {
    pipeline.as_any().downcast_ref::<MetalPipeline>().map(|p| &p.pipeline)
}

fn create_argument(pipeline: &dyn DriverPipeline, stage: ShaderStage, index: ogpu_core::validation::AdmittedSlot) -> Result<Box<dyn DriverArgument>, Error> {
    let Some(pipeline) = metal_pipeline(pipeline).cloned() else {
        return Err(fail(ARGUMENT_OP, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
    };
    if stage == ShaderStage::Vertex {
        return Err(fail(ARGUMENT_OP, ErrorKind::Unsupported, "argument encoders are exposed for fragment functions"));
    }
    let encoder = RefCell::new(pipeline.argument_encoder(index)?);
    let (length, alignment) = (encoder.borrow().encoded_length() as i64, encoder.borrow().alignment() as i64);
    Ok(Box::new(MetalArgument { pipeline, index, encoder, length, alignment }))
}

struct AccelRecorder {
    context: Rc<CommandContext>,
    encoder: native::AccelerationEncoder,
}

fn accel_encoder(context: &Rc<CommandContext>) -> Result<Box<dyn DriverAccelEncoder>, Error> {
    let encoder = native::AccelerationEncoder::create(&context.native)?;
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Accel(encoder.clone()));
    Ok(Box::new(AccelRecorder { context: context.clone(), encoder }))
}

impl DriverAccelEncoder for AccelRecorder {
    fn build(&self, accel: &dyn DriverAccel, scratch: &dyn DriverResource, scratch_offset: i64) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(accel)?;
        let scratch = c.find_buffer(scratch)?;
        c.keep_accel(structure)?;
        c.keep_buffer(scratch)?;
        structure.encode_build(&self.encoder, scratch, scratch_offset)
    }
    fn compact(&self, src: &dyn DriverAccel, dst: &dyn DriverAccel) -> Result<(), Error> {
        let c = &self.context;
        let (src, dst) = (c.find_accel(src)?, c.find_accel(dst)?);
        c.keep_accel(src)?;
        c.keep_accel(dst)?;
        Acceleration::encode_compact(&self.encoder, src, dst)
    }
    fn write_compacted_size(&self, accel: &dyn DriverAccel, dst: &dyn DriverResource, output: ogpu_core::validation::AdmittedCompactedSize) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(accel)?;
        let destination = c.find_buffer(dst)?;
        c.keep_accel(structure)?;
        c.keep_buffer(destination)?;
        structure.encode_compacted_size(&self.encoder, destination, output)
    }
    fn refit(&self, accel: &dyn DriverAccel, descriptor: &DriverAccelDescriptor, scratch: &dyn DriverResource, scratch_offset: i64) -> Result<(), Error> {
        let c = &self.context;
        let structure = c.find_accel(accel)?;
        let scratch = c.find_buffer(scratch)?;
        c.keep_accel(structure)?;
        c.keep_buffer(scratch)?;
        let resolve = |resource: &dyn DriverResource| -> Result<Buffer, Error> {
            let b = c.find_buffer(resource)?;
            c.keep_buffer(b)?;
            Ok(b.clone())
        };
        structure.encode_refit(&self.encoder, &c.device, descriptor, &resolve, scratch, scratch_offset)
    }
    fn end_accel(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        self.encoder.end_encoding()
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
        let mut retained = std::mem::take(&mut *context.retained.borrow_mut());
        match self.queue.submit_native(&context.native, &mut retained) {
            Err(e) => {
                *context.retained.borrow_mut() = retained;
                Err(e)
            }
            Ok(receipt) => Ok(Receipt { epoch: receipt.epoch }),
        }
    }
    fn commit_present(&self, source: &dyn DriverResource, frame: DriverFrame) -> Result<Receipt, Error> {
        let context = &self.context;
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
                pending.commit_present(receipt.epoch);
                self.dev.acquired_frames.borrow_mut().remove(&frame.frame_token);
                Ok(Receipt { epoch: receipt.epoch })
            }
        }
    }
    fn abandon(&self) -> Result<(), Error> {
        let context = &self.context;
        let open = context.open_encoder.borrow_mut().take();
        if let Some(open) = open {
            open.end();
        }
        context.retained.borrow_mut().clear();
        context.native.destroy()
    }
}

fn begin_commands(dev: &Rc<Dev>, queue: &Queue, active: &Rc<Vec<PendingPresentation>>) -> Result<Box<dyn DriverCommands>, Error> {
    let operation = "Ogpu_metal.Backend.begin_commands";
    let native = native::CommandBuffer::create(queue.metal())?;
    let context = Rc::new(CommandContext {
        operation,
        device: dev.device.clone(),
        native,
        tables: dev.t.clone(),
        commands_token: dev.t.token(),
        open_encoder: RefCell::new(None),
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
        surface::complete_presentations_through(&self.active, epoch);
        self.dev.commit_completed(waited)
    }
    fn poll_through(&self, epoch: i64) -> Result<bool, Error> {
        if self.queue.poll_through(epoch)? {
            surface::complete_presentations_through(&self.active, epoch);
            self.dev.commit_completed(Ok(())).map(|()| true)
        } else {
            Ok(false)
        }
    }
    fn begin_commands(&self) -> Result<Box<dyn DriverCommands>, Error> {
        begin_commands(&self.dev, &self.queue, &self.active)
    }
    fn gpu_duration(&self, epoch: i64) -> Option<f64> {
        self.queue.gpu_duration(epoch)
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
    fn configure(&self, configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<(), Error> {
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
        if self.value.in_flight_presentations() != 0 {
            Err(fail("Ogpu_metal.Backend.destroy_surface", ErrorKind::InvalidState, "surface has presentations in flight"))
        } else {
            self.value.destroy();
            self.value.release_layer();
            Ok(())
        }
    }
}

fn create_surface(dev: &Rc<Dev>, configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn DriverSurface>, Error> {
    const OP: &str = "Ogpu_metal.Backend.create_surface";
    let Some(token) = &configuration.layer else {
        return Err(fail(OP, ErrorKind::Unsupported, "surface needs a native layer token"));
    };
    let layer = native::Surface::adopt_borrowed(
            dev.device.metal(),
            token,
            token.owner_id(),
            token.generation(),
            configuration.layer_profile(true)?,
        )?;
    let value = surface::create(&dev.device, layer)?;
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

fn metal_size(device: &Device, project: fn(&native::DeviceInfo) -> u64) -> i64 {
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
    fn create_buffer(&self, descriptor: &AdmittedBuffer) -> Result<Box<dyn DriverResource>, Error> {
        create_buffer(&self.dev, descriptor)
    }
    fn create_mapped_buffer(&self, descriptor: &AdmittedBuffer) -> Result<DriverMapped, Error> {
        create_mapped_buffer(&self.dev, descriptor)
    }
    fn create_texture(&self, descriptor: &AdmittedTexture) -> Result<Box<dyn DriverResource>, Error> {
        create_texture(&self.dev, descriptor)
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
    fn create_sampler(&self, descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn DriverSampler>, Error> {
        create_sampler(&self.dev, descriptor)
    }
    fn create_render_pipeline(&self, options: RenderPipelineOptions, descriptor: &RenderDescriptor, admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Box<dyn DriverPipeline>, Error> {
        create_render_pipeline(&self.dev, options, descriptor, admitted)
    }
    fn create_icb(&self, profile: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn DriverIcb>, Error> {
        create_icb(&self.dev, profile)
    }
    fn create_argument(&self, pipeline: &dyn DriverPipeline, stage: ShaderStage, index: ogpu_core::validation::AdmittedSlot) -> Result<Box<dyn DriverArgument>, Error> {
        create_argument(pipeline, stage, index)
    }
    fn create_queue(&self) -> Result<Box<dyn DriverQueue>, Error> {
        create_queue(&self.dev)
    }
    fn create_surface(&self, configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn DriverSurface>, Error> {
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
        if let Err(e) = dev.t.create_depth_states(&dev.device) {
            let _ = destroy_device(&dev);
            return Err(e);
        }
        Ok(Box::new(MetalDevice { dev, handle, caps }))
    }
}

pub fn create() -> Box<dyn Driver> {
    Box::new(MetalDriver { device_live: Rc::new(Cell::new(false)) })
}

/// Native handles still alive on this thread (the release queue drained first).
pub fn live_handles() -> i64 {
    match (native::release_queue::drain(), native::release_queue::stats()) {
        (Ok(_), Ok(stats)) => stats.live_handles as i64,
        _ => unreachable!("the release queue answers on the thread that owns the Metal handles"),
    }
}
