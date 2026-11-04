//! Ogpu_metal.Backend: OGPU's driver records over the Metal modules. Tokens name native objects;
//! the checks live in `ogpu_core::backend`, as for Vulkan.
use crate::{
    acceleration::{self, Acceleration},
    blit_encoder, buffer,
    buffer::Buffer,
    command_control::{CommandContext, IcbEntry, Resource, Tables},
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
        DriverLibrary, DriverMapped, DriverPipeline, DriverQueue, DriverRenderEncoder, DriverResource,
        DriverSampler, DriverSurface, Floats, GpuTiming, InstanceKind, Receipt, RenderPipelineOptions,
        ShaderStage, Token,
    },
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

/// One device's driver state: the token tables and everything the closures share.
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
}

fn buffer_memory(memory: Memory) -> Result<buffer::Memory, Error> {
    match memory {
        Memory::Shared => Ok(buffer::Memory::Shared),
        Memory::DeviceLocal => Ok(buffer::Memory::DeviceLocal),
        Memory::Memoryless => Err(fail("Ogpu_metal.Backend.create_buffer", ErrorKind::InvalidArgument, "memoryless buffers do not exist")),
    }
}

fn register_buffer(dev: &Rc<Dev>, created: Result<Buffer, Error>) -> Result<DriverResource, Error> {
    let value = created?;
    let id = dev.t.token();
    dev.t.resources.borrow_mut().insert(id, Resource::Buffer(value.clone()));
    let device = dev.device.clone();
    let (b_write, b_read, b_read_into, b_destroy) = (value.clone(), value.clone(), value.clone(), value);
    let (d_write, d_read, d_read_into) = (device.clone(), device.clone(), device);
    let dev = dev.clone();
    Ok(DriverResource {
        token: id,
        write: Box::new(move |offset, bytes| b_write.write_bytes(&d_write, offset, bytes)),
        read: Box::new(move |offset, length| b_read.read_bytes(&d_read, offset, length as i64)),
        read_into: Box::new(move |offset, destination, destination_offset, length| {
            let bytes = b_read_into.read_bytes(&d_read_into, offset, length as i64)?;
            if length > destination.len().saturating_sub(destination_offset) || destination_offset > destination.len() {
                return Err(fail("Ogpu_metal.Backend.buffer.read_into", ErrorKind::InvalidArgument, "destination range is invalid"));
            }
            destination[destination_offset..destination_offset + length].copy_from_slice(&bytes);
            Ok(())
        }),
        destroy: Box::new(move || {
            b_destroy.destroy()?;
            dev.t.resources.borrow_mut().remove(&id);
            Ok(())
        }),
    })
}

fn create_buffer(dev: &Rc<Dev>, memory: Memory, descriptor: &BufferDescriptor) -> Result<DriverResource, Error> {
    register_buffer(dev, buffer_memory(memory).and_then(|m| buffer::create(&dev.device, m, descriptor)))
}

fn floats_to_bytes(floats: &[f32], length: usize) -> Vec<u8> {
    let mut bytes: Vec<u8> = floats.iter().flat_map(|value| value.to_le_bytes()).collect();
    bytes.truncate(length);
    bytes
}

/// A mapped buffer: shared storage, with the host view a copy that `flush` and `invalidate` move
/// across (the Rust `Floats` is a vector, not the native bytes). ponytail: a true alias needs
/// `Floats` to be a native view; the copy is made on the first ask and kept until destroy.
fn create_mapped_buffer(dev: &Rc<Dev>, descriptor: &BufferDescriptor) -> Result<DriverMapped, Error> {
    const OP: &str = "Ogpu_metal.Backend.create_mapped_buffer";
    let value = buffer::create(&dev.device, buffer::Memory::Shared, descriptor)?;
    let resource = register_buffer(dev, Ok(value.clone()))?;
    let cell: Rc<RefCell<Option<Floats>>> = Rc::new(RefCell::new(None));
    let count = (descriptor.size / 4) as usize;
    let make_view = {
        let cell = cell.clone();
        let (value, device) = (value.clone(), dev.device.clone());
        move || -> Result<Floats, Error> {
            let mut slot = cell.borrow_mut();
            if let Some(view) = slot.as_ref() {
                return Ok(view.clone());
            }
            let bytes = value.read_bytes(&device, 0, (count * 4) as i64)?;
            let floats: Vec<f32> = bytes.chunks_exact(4).map(|c| f32::from_le_bytes([c[0], c[1], c[2], c[3]])).collect();
            let view: Floats = Rc::new(RefCell::new(floats));
            *slot = Some(view.clone());
            Ok(view)
        }
    };
    let DriverResource { token, write, read, read_into, destroy } = resource;
    let (destroy_cell, flush_cell) = (cell.clone(), cell);
    let (flush_value, flush_device) = (value.clone(), dev.device.clone());
    let (invalidate_value, invalidate_device) = (value, dev.device.clone());
    let (view, invalidate_view) = (make_view.clone(), make_view);
    Ok(DriverMapped {
        mapped_resource: DriverResource {
            token,
            write,
            read,
            read_into,
            destroy: Box::new(move || {
                *destroy_cell.borrow_mut() = None;
                destroy()
            }),
        },
        view: Box::new(move || view().ok()),
        // shared storage on unified memory: the one view stays (as lib/ogpu_metal does) until the buffer is destroyed
        release_view: Box::new(|| {}),
        flush: Box::new(move |length| {
            let Some(floats) = flush_cell.borrow().clone() else {
                return Err(fail(OP, ErrorKind::InvalidArgument, "no host view was written"));
            };
            let bytes = floats_to_bytes(&floats.borrow(), length);
            flush_value.write_bytes(&flush_device, 0, &bytes)
        }),
        invalidate: Box::new(move |length| {
            let floats = invalidate_view()?;
            if length > floats.borrow().len() * 4 {
                return Err(fail(OP, ErrorKind::InvalidArgument, "range exceeds the buffer"));
            }
            let bytes = invalidate_value.read_bytes(&invalidate_device, 0, length as i64)?;
            for (slot, chunk) in floats.borrow_mut().iter_mut().zip(bytes.chunks_exact(4)) {
                *slot = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            }
            Ok(())
        }),
    })
}

fn texture_memory(host_read: bool, descriptor: &TextureDescriptor) -> Memory {
    if descriptor.sample_count == 1 && host_read { Memory::Shared } else { Memory::DeviceLocal }
}

fn register_texture(dev: &Rc<Dev>, host_read: bool, descriptor: &TextureDescriptor, created: Result<Texture, Error>) -> Result<DriverResource, Error> {
    let value = created?;
    let id = dev.t.token();
    dev.t.resources.borrow_mut().insert(id, Resource::Texture(value.clone()));
    let height = descriptor.height;
    let (t_read, t_read_into, t_destroy) = (value.clone(), value.clone(), value);
    let (d_read, d_read_into) = (dev.device.clone(), dev.device.clone());
    let dev = dev.clone();
    Ok(DriverResource {
        token: id,
        write: Box::new(|_, _| Err(fail("Ogpu_metal.Backend.texture.write", ErrorKind::Unsupported, "use a transfer pass for textures"))),
        read: Box::new(move |offset, length| {
            if !host_read {
                Err(fail("Ogpu_metal.Backend.depth.read", ErrorKind::Unsupported, "depth textures are not host readable"))
            } else if offset != 0 || height <= 0 || (length as i64) % height != 0 {
                Err(fail("Ogpu_metal.Backend.texture.read", ErrorKind::InvalidArgument, "texture read range is invalid"))
            } else {
                t_read.read_bytes(&d_read, 0, length as i64 / height)
            }
        }),
        read_into: Box::new(move |offset, destination, destination_offset, length| {
            if !host_read {
                Err(fail("Ogpu_metal.Backend.depth.read_into", ErrorKind::Unsupported, "depth textures are not host readable"))
            } else if offset != 0 || destination_offset != 0 || length != destination.len() || height <= 0 || (length as i64) % height != 0 {
                Err(fail("Ogpu_metal.Backend.texture.read_into", ErrorKind::InvalidArgument, "destination must exactly match the texture read range"))
            } else {
                t_read_into.read_bytes_into(&d_read_into, 0, length as i64 / height, destination)
            }
        }),
        destroy: Box::new(move || {
            t_destroy.destroy()?;
            dev.t.resources.borrow_mut().remove(&id);
            Ok(())
        }),
    })
}

fn create_texture_format(dev: &Rc<Dev>, format: TextureFormat, host_read: bool, descriptor: &TextureDescriptor) -> Result<DriverResource, Error> {
    register_texture(dev, host_read, descriptor, texture::create(&dev.device, texture_memory(host_read, descriptor), format, descriptor))
}

fn max_threads(pipeline: &Pipeline) -> i64 {
    match pipeline.native() {
        Native::Compute(native) => native.max_total_threads() as i64,
        Native::Render(_) => 0,
    }
}

fn own_pipeline(dev: &Rc<Dev>, pipeline: Pipeline) -> Result<DriverPipeline, Error> {
    let id = dev.t.token();
    dev.t.pipelines.borrow_mut().insert(id, pipeline.clone());
    let max = max_threads(&pipeline);
    let dev = dev.clone();
    Ok(DriverPipeline {
        pipeline_token: id,
        max_threads: max,
        destroy_pipeline: Box::new(move || {
            pipeline.destroy()?;
            dev.t.pipelines.borrow_mut().remove(&id);
            Ok(())
        }),
    })
}

fn create_library(dev: &Rc<Dev>, shader: &Shader) -> Result<DriverLibrary, Error> {
    let value = library::create(&dev.device, shader)?;
    let id = dev.t.token();
    dev.libraries.borrow_mut().insert(id, value.clone());
    let (l_create, l_destroy) = (value.clone(), value);
    let (d_create, d_destroy) = (dev.clone(), dev.clone());
    Ok(DriverLibrary {
        create_compute_pipeline_in: Box::new(move |entry: &str, constants: &[(String, ConstantValue)], interface: &[Binding]| {
            let pipeline = pipeline::create_compute_from_library(&d_create.device, &l_create, entry, constants, interface)?;
            own_pipeline(&d_create, pipeline)
        }),
        destroy_library: Box::new(move || {
            l_destroy.destroy()?;
            d_destroy.libraries.borrow_mut().remove(&id);
            Ok(())
        }),
    })
}

fn create_accel(dev: &Rc<Dev>, descriptor: &DriverAccelDescriptor) -> Result<DriverAccel, Error> {
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
    let dev = dev.clone();
    Ok(DriverAccel {
        accel_token: id,
        accel_sizes: AccelSizes {
            structure_size: sizes.acceleration_structure_size as i64,
            build_scratch_size: sizes.build_scratch_buffer_size as i64,
            refit_scratch_size: sizes.refit_scratch_buffer_size as i64,
        },
        destroy_accel: Box::new(move || {
            structure.destroy()?;
            dev.t.accels.borrow_mut().remove(&id);
            Ok(())
        }),
    })
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

fn create_sampler(dev: &Rc<Dev>, descriptor: &SamplerDescriptor) -> Result<DriverSampler, Error> {
    let value = sampler::create(&dev.device, descriptor)?;
    let id = dev.t.token();
    dev.t.samplers.borrow_mut().insert(id, value.clone());
    let dev = dev.clone();
    Ok(DriverSampler {
        sampler_token: id,
        destroy_sampler: Box::new(move || {
            value.destroy()?;
            dev.t.samplers.borrow_mut().remove(&id);
            Ok(())
        }),
    })
}

fn create_render_pipeline(dev: &Rc<Dev>, options: RenderPipelineOptions, descriptor: &RenderDescriptor) -> Result<DriverPipeline, Error> {
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

fn create_icb(dev: &Rc<Dev>, max_commands: i64) -> Result<DriverIcb, Error> {
    const OP: &str = "Ogpu_metal.Backend.create_icb";
    let mut descriptor = metal::IndirectCommandBuffer::descriptor(vec![metal::indirect::CommandType::IndirectDraw, metal::indirect::CommandType::IndirectDrawIndexed]);
    descriptor.inherit_buffers = false;
    descriptor.inherit_pipeline_state = false;
    descriptor.max_vertex_buffer_bind_count = 8;
    descriptor.max_fragment_buffer_bind_count = 8;
    let icb = metal::IndirectCommandBuffer::create(dev.device.metal(), max_commands as usize, &descriptor).map_err(|value| of_metal_error(OP, &value))?;
    let id = dev.t.token();
    let entry = Rc::new(IcbEntry { icb, commands: RefCell::new(vec![None; max_commands as usize]) });
    dev.t.icbs.borrow_mut().insert(id, entry.clone());
    let command = {
        let entry = entry.clone();
        move |index: i64| -> Result<metal::RenderCommand, Error> {
            let index = index as usize;
            if let Some(Some(c)) = entry.commands.borrow().get(index) {
                return Ok(c.clone());
            }
            let c = metal::RenderCommand::at(&entry.icb, index).map_err(|value| of_metal_error(OP, &value))?;
            entry.commands.borrow_mut()[index] = Some(c.clone());
            Ok(c)
        }
    };
    let find_buffer = {
        let dev = dev.clone();
        move |token: Token| -> Result<Buffer, Error> {
            match dev.t.resource(token) {
                Some(Resource::Buffer(b)) => Ok(b),
                _ => Err(fail(OP, ErrorKind::InvalidArgument, "buffer graph is incomplete")),
            }
        }
    };
    let (c_pipeline, d_pipeline) = (command.clone(), dev.clone());
    let (c_buffer, f_buffer) = (command.clone(), find_buffer.clone());
    let c_draw = command.clone();
    let (c_indexed, f_indexed) = (command, find_buffer);
    let dev = dev.clone();
    Ok(DriverIcb {
        icb_token: id,
        icb_set_pipeline: Box::new(move |index, token| {
            let Some(pipeline) = d_pipeline.t.pipelines.borrow().get(&token).cloned() else {
                return Err(fail(OP, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
            };
            match pipeline.native() {
                Native::Compute(_) => Err(fail(OP, ErrorKind::InvalidArgument, "compute pipeline cannot draw")),
                Native::Render(native) => c_pipeline(index)?.set_pipeline(native).map_err(|value| of_metal_error(OP, &value)),
            }
        }),
        icb_set_buffer: Box::new(move |index, stage, slot, offset, token| {
            let command = c_buffer(index)?;
            let buffer = f_buffer(token)?;
            match stage {
                    ShaderStage::Vertex => command.set_vertex_buffer(slot as usize, offset as u64, buffer.metal()),
                    ShaderStage::Fragment => command.set_fragment_buffer(slot as usize, offset as u64, buffer.metal()),
                }.map_err(|value| of_metal_error(OP, &value))
            
        }),
        icb_draw: Box::new(move |index, primitive, first, count, instances| {
            c_draw(index)?.draw_primitives(icb_primitive(primitive), first as usize, count as usize, Some(instances as usize)).map_err(|value| of_metal_error(OP, &value))
        }),
        icb_draw_indexed: Box::new(move |index, primitive, index_type, token, offset, count, instances| {
            let command = c_indexed(index)?;
            let index_buffer = f_indexed(token)?;
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
                ).map_err(|value| of_metal_error(OP, &value))
            
        }),
        destroy_icb: Box::new(move || {
            for slot in entry.commands.borrow_mut().iter_mut() {
                if let Some(command) = slot.take() {
                    let _ = command.destroy();
                }
            }
            entry.icb.destroy().map_err(|value| of_metal_error(OP, &value))?;
            dev.t.icbs.borrow_mut().remove(&id);
            Ok(())
        }),
    })
}

fn create_argument(dev: &Rc<Dev>, pipeline: Token, stage: ShaderStage, index: i64) -> Result<DriverArgument, Error> {
    const OP: &str = "Ogpu_metal.Backend.create_argument";
    let Some(pipeline) = dev.t.pipelines.borrow().get(&pipeline).cloned() else {
        return Err(fail(OP, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
    };
    if stage == ShaderStage::Vertex {
        return Err(fail(OP, ErrorKind::Unsupported, "argument encoders are exposed for fragment functions"));
    }
    let encoder = RefCell::new(pipeline.argument_encoder(index as u64)?);
    let (length, alignment) = (encoder.borrow().encoded_length() as i64, encoder.borrow().alignment() as i64);
    // A Metal argument encoder keeps its last-set resources alive; encode each slice through a
    // short-lived encoder so evicted textures can be released.
    let encode = {
        let (dev, pipeline) = (dev.clone(), pipeline);
        Rc::new(move |buffer: Token, offset: i64, resource: metal::ShaderArgumentResource, slot: i64| -> Result<(), Error> {
            let Some(Resource::Buffer(buffer)) = dev.t.resource(buffer) else {
                return Err(fail(OP, ErrorKind::InvalidArgument, "buffer graph is incomplete"));
            };
            let mut scratch = pipeline.argument_encoder(index as u64)?;
            let done = scratch
                .set_argument_buffer(buffer.metal(), offset as u64)
                .and_then(|()| scratch.set(slot as u64, resource));
            let _ = scratch.destroy();
            done.map_err(|value| of_metal_error(OP, &value))
        })
    };
    let (e_texture, e_sampler) = (encode.clone(), encode);
    let (d_texture, d_sampler) = (dev.clone(), dev.clone());
    Ok(DriverArgument {
        argument_length: length,
        argument_alignment: alignment,
        argument_texture: Box::new(move |buffer, offset, slot, texture| match d_texture.t.resource(texture) {
            Some(Resource::Texture(texture)) => e_texture(buffer, offset, metal::ShaderArgumentResource::Texture(texture.metal().clone()), slot),
            _ => Err(fail(OP, ErrorKind::InvalidArgument, "texture graph is incomplete")),
        }),
        argument_sampler: Box::new(move |buffer, offset, slot, sampler| match d_sampler.t.samplers.borrow().get(&sampler) {
            Some(sampler) => e_sampler(buffer, offset, metal::ShaderArgumentResource::Sampler(sampler.metal().clone()), slot),
            None => Err(fail(OP, ErrorKind::InvalidArgument, "sampler graph is incomplete")),
        }),
        destroy_argument: Box::new(move || encoder.borrow_mut().destroy().map_err(|value| of_metal_error(OP, &value))),
    })
}

fn run_all(retained: &RefCell<Vec<Cleanup>>) {
    for release in std::mem::take(&mut *retained.borrow_mut()) {
        release();
    }
}

fn accel_encoder(context: &Rc<CommandContext>) -> Result<DriverAccelEncoder, Error> {
    context.recording()?;
    let encoder = metal::AccelerationEncoder::create(&context.native).map_err(|value| of_metal_error(context.operation, &value))?;
    let closing = encoder.clone();
    *context.open_encoder.borrow_mut() = Some(Box::new(move || {
        let _ = closing.end_encoding();
    }));
    let (c_build, e_build) = (context.clone(), encoder.clone());
    let (c_compact, e_compact) = (context.clone(), encoder.clone());
    let (c_size, e_size) = (context.clone(), encoder.clone());
    let (c_refit, e_refit) = (context.clone(), encoder.clone());
    let (c_end, e_end) = (context.clone(), encoder);
    Ok(DriverAccelEncoder {
        build: Box::new(move |token, scratch, scratch_offset| {
            let structure = c_build.find_accel(token)?;
            let scratch = c_build.find_buffer(scratch)?;
            c_build.keep_accel(&structure)?;
            c_build.keep_buffer(&scratch)?;
            structure.encode_build(&e_build, &c_build.device, &scratch, scratch_offset)
        }),
        compact: Box::new(move |src, dst| {
            let (src, dst) = (c_compact.find_accel(src)?, c_compact.find_accel(dst)?);
            c_compact.keep_accel(&src)?;
            c_compact.keep_accel(&dst)?;
            Acceleration::encode_compact(&e_compact, &c_compact.device, &src, &dst)
        }),
        write_compacted_size: Box::new(move |token, dst, offset| {
            let structure = c_size.find_accel(token)?;
            let destination = c_size.find_buffer(dst)?;
            c_size.keep_accel(&structure)?;
            c_size.keep_buffer(&destination)?;
            structure.encode_compacted_size(&e_size, &c_size.device, &destination, offset)
        }),
        refit: Box::new(move |token, descriptor, scratch, scratch_offset| {
            let structure = c_refit.find_accel(token)?;
            let scratch = c_refit.find_buffer(scratch)?;
            c_refit.keep_accel(&structure)?;
            c_refit.keep_buffer(&scratch)?;
            let resolve = |token: Token| -> Result<Buffer, Error> {
                let b = c_refit.find_buffer(token)?;
                c_refit.keep_buffer(&b)?;
                Ok(b)
            };
            structure.encode_refit(&e_refit, &c_refit.device, descriptor, &resolve, &scratch, scratch_offset)
        }),
        end_accel: Box::new(move || {
            *c_end.open_encoder.borrow_mut() = None;
            e_end.end_encoding().map_err(|value| of_metal_error(c_end.operation, &value))
        }),
    })
}

fn begin_commands(dev: &Rc<Dev>, queue: &Queue, active: &Rc<Vec<PendingPresentation>>) -> Result<DriverCommands, Error> {
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
    let (c_compute, c_blit, c_render, c_accel) = (context.clone(), context.clone(), context.clone(), context.clone());
    let (c_commit, q_commit) = (context.clone(), queue.clone());
    let (c_present, q_present, a_present, d_present) = (context.clone(), queue.clone(), active.clone(), dev.clone());
    let c_abandon = context;
    Ok(DriverCommands {
        compute_encoder: Box::new(move || -> Result<DriverComputeEncoder, Error> { compute_encoder::create(&c_compute) }),
        accel_encoder: Box::new(move || accel_encoder(&c_accel)),
        blit_encoder: Box::new(move || -> Result<DriverBlitEncoder, Error> { blit_encoder::create(&c_blit) }),
        render_encoder: Box::new(move |target| -> Result<DriverRenderEncoder, Error> { render_encoder::create(&c_render, target) }),
        commit: Box::new(move || {
            c_commit.recording()?;
            let mut retained = std::mem::take(&mut *c_commit.retained.borrow_mut());
            match q_commit.submit_native(&c_commit.native, &mut retained) {
                Err(e) => {
                    *c_commit.retained.borrow_mut() = retained;
                    Err(e)
                }
                Ok(receipt) => {
                    c_commit.finished.set(true);
                    Ok(Receipt { epoch: receipt.epoch })
                }
            }
        }),
        commit_present: Box::new(move |source, frame: DriverFrame| {
            let context = &c_present;
            context.recording()?;
            let source = context.find_texture(source)?;
            let Some((surface, native_frame)) = d_present.acquired_frames.borrow().get(&frame.frame_token).cloned() else {
                return Err(fail(operation, ErrorKind::InvalidState, "frame token is stale"));
            };
            let Some(pending) = a_present.iter().find(|p| p.available()) else {
                return Err(fail(operation, ErrorKind::Capacity, "bounded presentation slots are all in flight"));
            };
            pending.prepare_present(&surface, &native_frame, &source)?;
            if let Err(e) = pending.presentation_encoder(&context.native) {
                pending.rollback_present();
                return Err(e);
            }
            let mut retained = std::mem::take(&mut *context.retained.borrow_mut());
            match q_present.submit_native(&context.native, &mut retained) {
                Err(e) => {
                    *context.retained.borrow_mut() = retained;
                    pending.rollback_present();
                    Err(e)
                }
                Ok(receipt) => {
                    context.finished.set(true);
                    pending.commit_present(receipt.epoch);
                    d_present.acquired_frames.borrow_mut().remove(&frame.frame_token);
                    Ok(Receipt { epoch: receipt.epoch })
                }
            }
        }),
        abandon: Box::new(move || {
            c_abandon.recording()?;
            if let Some(end_open) = c_abandon.open_encoder.borrow_mut().take() {
                end_open();
            }
            c_abandon.finished.set(true);
            run_all(&c_abandon.retained);
            c_abandon.native.destroy().map_err(|value| of_metal_error(c_abandon.operation, &value))
        }),
    })
}

fn create_queue(dev: &Rc<Dev>) -> Result<DriverQueue, Error> {
    let queue = queue::create(&dev.device)?;
    let queue_token = dev.t.token();
    let active: Rc<Vec<PendingPresentation>> = Rc::new((0..PENDING_PRESENTATION_CAPACITY).map(|_| surface::create_pending_presentation()).collect());
    dev.active_queues.borrow_mut().insert(queue_token, active.clone());
    let commit_completed = {
        let dev = dev.clone();
        move |completion: Result<(), Error>| -> Result<(), Error> {
            let cleanup = dev.take_cleanup_error();
            match (completion, cleanup) {
                (Err(e), _) => Err(e),
                (Ok(()), Some(e)) => Err(e),
                (Ok(()), None) => Ok(()),
            }
        }
    };
    let (q_complete, a_complete, cc_complete) = (queue.clone(), active.clone(), commit_completed.clone());
    let (q_poll, a_poll, cc_poll) = (queue.clone(), active.clone(), commit_completed);
    let q_epoch = queue.clone();
    let (q_begin, a_begin, d_begin) = (queue.clone(), active.clone(), dev.clone());
    let q_duration = queue.clone();
    let device_timing = dev.device.clone();
    let (q_destroy, a_destroy, d_destroy) = (queue, active, dev.clone());
    Ok(DriverQueue {
        complete_through: Box::new(move |epoch| {
            let waited = q_complete.wait_through(epoch);
            surface::complete_presentations_through(&a_complete, q_complete.completed_epoch());
            cc_complete(waited)
        }),
        poll_through: Box::new(move |epoch| {
            let polled = q_poll.poll_through(epoch);
            let completed = q_poll.completed_epoch();
            if epoch > 0 && completed >= epoch {
                surface::complete_presentations_through(&a_poll, completed);
                cc_poll(polled.map(|_| ())).map(|()| true)
            } else {
                polled
            }
        }),
        completed_epoch: Box::new(move || q_epoch.completed_epoch()),
        begin_commands: Box::new(move || begin_commands(&d_begin, &q_begin, &a_begin)),
        gpu_duration: Box::new(move |epoch| q_duration.gpu_duration(epoch)),
        gpu_timing: Box::new(move || GpuTiming { gpu_seconds: queue::gpu_timing_for_device(&device_timing).duration_seconds }),
        destroy_queue: Box::new(move || {
            if !a_destroy.iter().all(|p| p.available()) {
                return Err(fail("Ogpu_metal.Backend.destroy_queue", ErrorKind::InvalidState, "queue has presentations in flight"));
            }
            q_destroy.destroy()?;
            surface::clear_pending_presentations(&a_destroy);
            d_destroy.active_queues.borrow_mut().remove(&queue_token);
            match d_destroy.take_cleanup_error() {
                None => Ok(()),
                Some(e) => Err(e),
            }
        }),
    })
}


fn create_surface(dev: &Rc<Dev>, configuration: &Configuration) -> Result<DriverSurface, Error> {
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
    let frames_of_surface = {
        let (dev, value) = (dev.clone(), value.clone());
        move || dev.acquired_frames.borrow().values().filter(|(owner, _)| owner.same(&value)).count()
    };
    let (s_configure, s_acquire, d_acquire) = (value.clone(), value.clone(), dev.clone());
    let (s_discard, d_discard) = (value.clone(), dev.clone());
    let s_destroy = value;
    Ok(DriverSurface {
        configure: Box::new(move |c: &Configuration| s_configure.configure(c)),
        acquire_sync: Box::new(move || match s_acquire.acquire()? {
            Acquire::Timeout => Ok(DriverAcquire::Timeout),
            Acquire::Occluded => Ok(DriverAcquire::Occluded),
            Acquire::DeviceLost => Ok(DriverAcquire::DeviceLost),
            Acquire::Acquired(frame) => {
                let frame_token = frame.id();
                d_acquire.acquired_frames.borrow_mut().insert(frame_token, (s_acquire.clone(), frame));
                Ok(DriverAcquire::Acquired(DriverFrame { frame_token }))
            }
        }),
        discard: Box::new(move |frame: DriverFrame| {
            let Some((_, native)) = d_discard.acquired_frames.borrow().get(&frame.frame_token).cloned() else {
                return Err(fail("Ogpu_metal.Backend.surface", ErrorKind::InvalidState, "frame token is stale"));
            };
            s_discard.discard(&native)?;
            d_discard.acquired_frames.borrow_mut().remove(&frame.frame_token);
            Ok(())
        }),
        destroy_surface: Box::new(move || {
            if frames_of_surface() != 0 {
                Err(fail("Ogpu_metal.Backend.destroy_surface", ErrorKind::InvalidState, "surface has outstanding frames"))
            } else if s_destroy.in_flight_presentations() != 0 {
                Err(fail("Ogpu_metal.Backend.destroy_surface", ErrorKind::InvalidState, "surface has presentations in flight"))
            } else {
                s_destroy.destroy();
                s_destroy.release_layer();
                Ok(())
            }
        }),
    })
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

fn create_device(device_live: &Rc<Cell<bool>>) -> Result<DriverDevice, Error> {
    if device_live.get() {
        return Err(fail("Ogpu_metal.Backend.create_device", ErrorKind::InvalidState, "adapter already owns a live device"));
    }
    let device = Device::system_default()?;
    device_live.set(true);
    let dev = Rc::new(Dev {
        device: device.clone(),
        t: Rc::new(Tables::new()),
        libraries: RefCell::new(HashMap::new()),
        active_queues: RefCell::new(HashMap::new()),
        acquired_frames: RefCell::new(HashMap::new()),
        cleanup_error: RefCell::new(None),
        device_live: device_live.clone(),
    });
    let handle_copy: handle::Device = device.handle().clone();
    let (allocated, working) = (device.clone(), device.clone());
    let d_buffer = dev.clone();
    let d_mapped = dev.clone();
    let d_texture = dev.clone();
    let d_depth = dev.clone();
    let d_memoryless = dev.clone();
    let d_library = dev.clone();
    let d_accel = dev.clone();
    let d_sampler = dev.clone();
    let d_render = dev.clone();
    let d_icb = dev.clone();
    let d_argument = dev.clone();
    let d_queue = dev.clone();
    let d_surface = dev.clone();
    let d_destroy = dev.clone();
    Ok(DriverDevice {
        device_handle: handle_copy,
        capabilities: device.capabilities(),
        allocated_size: Box::new(move || metal_size(&allocated, |i| i.current_allocated_size)),
        recommended_working_set: Box::new(move || metal_size(&working, |i| i.recommended_max_working_set_size)),
        create_buffer: Box::new(move |memory, descriptor| create_buffer(&d_buffer, memory, descriptor)),
        create_mapped_buffer: Box::new(move |descriptor| create_mapped_buffer(&d_mapped, descriptor)),
        create_texture: Box::new(move |descriptor| create_texture_format(&d_texture, descriptor.format, true, descriptor)),
        create_depth_texture: Box::new(move |descriptor| create_texture_format(&d_depth, TextureFormat::Depth32Float, false, descriptor)),
        create_memoryless_texture: Box::new(move |depth, descriptor| {
            register_texture(
                &d_memoryless,
                false,
                descriptor,
                texture::create(&d_memoryless.device, Memory::Memoryless, if depth { TextureFormat::Depth32Float } else { descriptor.format }, descriptor),
            )
        }),
        create_library: Box::new(move |shader| create_library(&d_library, shader)),
        create_accel: Box::new(move |descriptor| create_accel(&d_accel, descriptor)),
        instance_layout: Box::new(instance_layout),
        create_sampler: Box::new(move |descriptor| create_sampler(&d_sampler, descriptor)),
        create_render_pipeline: Box::new(move |options, descriptor| create_render_pipeline(&d_render, options, descriptor)),
        create_icb: Box::new(move |max_commands| create_icb(&d_icb, max_commands)),
        create_argument: Box::new(move |pipeline, stage, index| create_argument(&d_argument, pipeline, stage, index)),
        create_queue: Box::new(move || create_queue(&d_queue)),
        create_surface: Box::new(move |configuration| create_surface(&d_surface, configuration)),
        destroy_device: Box::new(move || destroy_device(&d_destroy)),
    })
}

pub fn create() -> Driver {
    let device_live = Rc::new(Cell::new(false));
    Driver { create_device: Box::new(move || create_device(&device_live)) }
}

/// Native handles still alive on this thread (the release queue drained first).
pub fn live_handles() -> i64 {
    match (metal::ReleaseQueue::drain(), metal::ReleaseQueue::stats()) {
        (Ok(_), Ok(stats)) => stats.live_handles as i64,
        _ => unreachable!("the release queue answers on the thread that owns the Metal handles"),
    }
}

