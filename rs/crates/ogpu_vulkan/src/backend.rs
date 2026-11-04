//! OGPU's driver records over the Vulkan bridge. Tokens name native objects; the checks live in
//! `ogpu_core::backend`, as for Metal.
use ogpu_core::{
    backend::{
        Driver, DriverAccel, DriverAccelDescriptor, DriverAccelEncoder, DriverAcquire, DriverArgument,
        DriverBlitEncoder, DriverCommands, DriverComputeEncoder, DriverDevice, DriverFrame,
        DriverGeometry, DriverIcb, DriverLibrary, DriverMapped, DriverPipeline, DriverQueue,
        DriverRenderEncoder, DriverRenderTarget, DriverResource, DriverSampler, DriverSurface, Floats,
        GpuTiming, InstanceKind, Receipt, RenderPipelineOptions, ShaderStage, Token, Winding,
    },
    caps::{Caps, Limits},
    error::{self, Error, ErrorKind},
    handle,
    pipeline::{Blend, ColorFormat, DepthFormat, RenderDescriptor},
    render_pass::{Comparison, Cull, IndexType, Load, Primitive, Store},
    shader::{self, Binding as ShaderBinding, BindingKind, ConstantValue, Shader},
    surface::{Configuration, PresentMode},
    types::{
        self, AddressMode, BufferDescriptor, BufferUsage, Memory, MipFilter, SamplerDescriptor,
        SamplerFilter, TextureDescriptor, TextureFormat, TextureUsage,
    },
};
use std::{
    cell::{Cell, Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};
use vulkan as vk;

thread_local! {
    static LIVE: Cell<i64> = const { Cell::new(0) };
}

fn live_add(n: i64) {
    LIVE.with(|live| live.set(live.get() + n));
}

pub fn live_handles() -> i64 {
    LIVE.with(Cell::get)
}

fn fail(op: &str, kind: ErrorKind, text: &str) -> Error {
    error::make(op, kind, text)
}

/// Every Vulkan failure becomes an Ogpu error here, and nowhere else.
fn of_vk(op: &str, failure: vk::Error) -> Error {
    match failure.kind {
        vk::ErrorKind::NoAdapter => error::make(op, ErrorKind::NoAdapter, "no discrete Vulkan 1.3 device"),
        vk::ErrorKind::Unsupported => error::make(op, ErrorKind::Unsupported, "unsupported by the Vulkan bridge"),
        vk::ErrorKind::DeviceLost => error::make(op, ErrorKind::DeviceLost, "VK_ERROR_DEVICE_LOST"),
        vk::ErrorKind::OutOfDeviceMemory => error::make(op, ErrorKind::Capacity, "VK_ERROR_OUT_OF_DEVICE_MEMORY"),
        vk::ErrorKind::OutOfHostMemory => error::make(op, ErrorKind::Capacity, "VK_ERROR_OUT_OF_HOST_MEMORY"),
        vk::ErrorKind::InvalidInput(text) => error::make(op, ErrorKind::InvalidArgument, text),
        vk::ErrorKind::InvalidHandle => error::make(op, ErrorKind::InvalidState, "invalid Vulkan handle"),
        vk::ErrorKind::WrongDevice => error::make(op, ErrorKind::CrossDevice, "resource belongs to another device"),
        vk::ErrorKind::Vulkan(code) => error::make(op, ErrorKind::Io, &format!("VkResult {code}")),
    }
}

struct BufferEntry {
    buffer: RefCell<vk::Buffer>,
    size: i64,
}

struct TextureEntry {
    texture: vk::Texture,
    descriptor: TextureDescriptor,
}

struct PipelineEntry {
    pipeline: vk::Pipeline,
    local: (i64, i64, i64),
}

/// ponytail: an indirect command buffer is its recorded calls, replayed into the encoder; secondary
/// command buffers if replay shows in a frame's CPU time.
#[derive(Clone)]
enum IcbDraw {
    Draw { primitive: Primitive, first: i64, count: i64, instances: i64 },
    Indexed { primitive: Primitive, index_type: IndexType, buffer: Token, offset: i64, count: i64, instances: i64 },
}

#[derive(Clone, Default)]
struct IcbCommand {
    pipeline: Option<Token>,
    buffers: Vec<(i64, i64, Token)>,
    draw: Option<IcbDraw>,
}

/// One acceleration geometry as the bridge takes it.
#[derive(Clone)]
struct GeomRecord {
    kind: vk::GeometryKind,
    buffer: Rc<BufferEntry>,
    offset: u64,
    stride: u64,
    count: u32,
    opaque: bool,
    duplicate: bool,
}

/// An argument slice is a (texture, sampler) pair: binding the buffer at its offset binds those at
/// the slot and the next one, the shape SPIR-V's fragments declare.
type ArgumentCell = (Option<Rc<TextureEntry>>, Option<Rc<vk::Sampler>>);

fn format_index(format: TextureFormat) -> i32 {
    match format {
        TextureFormat::R8Unorm => 0,
        TextureFormat::Rgba8Unorm => 1,
        TextureFormat::Bgra8Unorm => 2,
        TextureFormat::Bgra8UnormSrgb => 3,
        TextureFormat::Rgba16Float => 4,
        TextureFormat::Rgba32Float => 5,
        TextureFormat::Depth32Float => 6,
    }
}

fn primitive_index(primitive: Primitive) -> i32 {
    match primitive {
        Primitive::PointList => 0,
        Primitive::LineList => 1,
        Primitive::TriangleList => 2,
        Primitive::TriangleStrip => 3,
    }
}

fn index_kind(index_type: IndexType) -> i32 {
    match index_type {
        IndexType::Uint16 => 0,
        IndexType::Uint32 => 1,
    }
}

fn load_index(load: Load) -> i32 {
    match load {
        Load::Load => 0,
        Load::Clear => 1,
        Load::DontCare => 2,
    }
}

fn store_index(store: Store) -> i32 {
    match store {
        Store::Store => 0,
        Store::Discard => 1,
        Store::Resolve => 2,
    }
}

fn compare_index(comparison: Comparison) -> i32 {
    match comparison {
        Comparison::Never => 0,
        Comparison::Less => 1,
        Comparison::Equal => 2,
        Comparison::LessEqual => 3,
        Comparison::Greater => 4,
        Comparison::NotEqual => 5,
        Comparison::GreaterEqual => 6,
        Comparison::Always => 7,
    }
}

fn blend_index(blend: Blend) -> i32 {
    match blend {
        Blend::Replace => 0,
        Blend::Alpha => 1,
        Blend::Add => 2,
        Blend::Multiply => 3,
        Blend::Screen => 4,
        Blend::Subtract => 5,
    }
}

fn validation_mode() -> (bool, bool) {
    match std::env::var("RAYS_VULKAN_VALIDATION").as_deref() {
        Ok("fatal") => (true, true),
        Ok("on") => (true, false),
        _ => (false, false),
    }
}

/// Stdlib.min / Stdlib.max on floats (unspecified for nan, as there).
fn omin(a: f64, b: f64) -> f64 {
    if a <= b { a } else { b }
}
fn omax(a: f64, b: f64) -> f64 {
    if a >= b { a } else { b }
}

struct Dev {
    device: RefCell<Option<Rc<vk::Device>>>,
    next: Cell<i64>,
    buffers: RefCell<HashMap<i64, Rc<BufferEntry>>>,
    textures: RefCell<HashMap<i64, Rc<TextureEntry>>>,
    pipelines: RefCell<HashMap<i64, Rc<PipelineEntry>>>,
    samplers: RefCell<HashMap<i64, Rc<vk::Sampler>>>,
    icbs: RefCell<HashMap<i64, Rc<RefCell<Vec<IcbCommand>>>>>,
    arguments: RefCell<HashMap<(i64, i64), ArgumentCell>>,
    frames: RefCell<HashMap<i64, Rc<RefCell<Option<vk::Surface>>>>>,
    accels: RefCell<HashMap<i64, Rc<vk::AccelerationStructure>>>,
    accel_resources: RefCell<HashMap<i64, Vec<DriverResource>>>,
    accel_geometries: RefCell<HashMap<i64, Vec<GeomRecord>>>,
    queues: Cell<i64>,
    max_invocations: i64,
}

impl Dev {
    fn token(&self) -> i64 {
        let x = self.next.get();
        self.next.set(x + 1);
        x
    }
    fn device(&self, op: &str) -> Result<Rc<vk::Device>, Error> {
        match self.device.borrow().as_ref() {
            Some(device) => Ok(device.clone()),
            None => Err(fail(op, ErrorKind::StaleHandle, "device is destroyed")),
        }
    }
    fn find_buffer(&self, op: &str, id: Token) -> Result<Rc<BufferEntry>, Error> {
        match self.buffers.borrow().get(&id) {
            Some(buffer) => Ok(buffer.clone()),
            None => Err(fail(op, ErrorKind::InvalidArgument, "buffer graph is incomplete")),
        }
    }
    fn find_texture(&self, op: &str, id: Token) -> Result<Rc<TextureEntry>, Error> {
        match self.textures.borrow().get(&id) {
            Some(texture) => Ok(texture.clone()),
            None => Err(fail(op, ErrorKind::InvalidArgument, "texture graph is incomplete")),
        }
    }
    fn find_pipeline(&self, op: &str, id: Token) -> Result<Rc<PipelineEntry>, Error> {
        match self.pipelines.borrow().get(&id) {
            Some(pipeline) => Ok(pipeline.clone()),
            None => Err(fail(op, ErrorKind::InvalidArgument, "pipeline graph is incomplete")),
        }
    }
    fn find_sampler(&self, op: &str, id: Token) -> Result<Rc<vk::Sampler>, Error> {
        match self.samplers.borrow().get(&id) {
            Some(sampler) => Ok(sampler.clone()),
            None => Err(fail(op, ErrorKind::InvalidArgument, "sampler graph is incomplete")),
        }
    }
    fn find_accel(&self, op: &str, id: Token) -> Result<Rc<vk::AccelerationStructure>, Error> {
        match self.accels.borrow().get(&id) {
            Some(accel) => Ok(accel.clone()),
            None => Err(fail(op, ErrorKind::InvalidArgument, "acceleration structure graph is incomplete")),
        }
    }
}

fn in_range(size: i64, offset: i64, length: usize) -> bool {
    offset >= 0 && i64::try_from(length).is_ok_and(|length| offset.checked_add(length).is_some_and(|end| end <= size))
}

fn buffer_read_into(op: &str, entry: &BufferEntry, offset: i64, destination: &mut [u8], at: usize, length: usize) -> Result<(), Error> {
    if at > destination.len() || length > destination.len() - at || !in_range(entry.size, offset, length) {
        return Err(fail(op, ErrorKind::InvalidArgument, "read range is invalid"));
    }
    entry.buffer.borrow().read(offset as u64, &mut destination[at..at + length]).map_err(|failure| of_vk(op, failure))
}

fn register_buffer(dev: &Rc<Dev>, op: &'static str, created: Result<vk::Buffer, vk::Error>, size: i64) -> Result<DriverResource, Error> {
    let native = created.map_err(|failure| of_vk(op, failure))?;
    live_add(1);
    let id = dev.token();
    dev.buffers.borrow_mut().insert(id, Rc::new(BufferEntry { buffer: RefCell::new(native), size }));
    let (write_dev, read_dev, into_dev, destroy_dev) = (dev.clone(), dev.clone(), dev.clone(), dev.clone());
    Ok(DriverResource {
        token: id,
        write: Box::new(move |offset, bytes| {
            let entry = write_dev.find_buffer(op, id)?;
            if !in_range(entry.size, offset, bytes.len()) {
                return Err(fail(op, ErrorKind::InvalidArgument, "write range is invalid"));
            }
            entry.buffer.borrow_mut().write(offset as u64, bytes).map_err(|failure| of_vk(op, failure))
        }),
        read: Box::new(move |offset, length| {
            let entry = read_dev.find_buffer(op, id)?;
            let mut bytes = vec![0u8; length];
            buffer_read_into(op, &entry, offset, &mut bytes, 0, length)?;
            Ok(bytes)
        }),
        read_into: Box::new(move |offset, destination, at, length| {
            let entry = into_dev.find_buffer(op, id)?;
            buffer_read_into(op, &entry, offset, destination, at, length)
        }),
        destroy: Box::new(move || {
            destroy_dev.buffers.borrow_mut().remove(&id);
            live_add(-1);
            Ok(())
        }),
    })
}

/// Both kinds are device-local: a Shared buffer in host memory is read by kernels over PCIe, 100x
/// slower than VRAM (isogrid_city's triangle kernel: 160 ms to 4 ms). Host writes stage into the next
/// submit and reads go through the transfer queue; Shared reads as zeros, so it is filled like a mapped
/// buffer is. ponytail: a host-visible tier for a buffer the host rewrites every frame and the GPU reads
/// once, if a measurement ever shows the staging copy.
fn create_buffer(dev: &Rc<Dev>, memory: Memory, descriptor: &BufferDescriptor) -> Result<DriverResource, Error> {
    const OP: &str = "Ogpu_vulkan.create_buffer";
    let device = dev.device(OP)?;
    let resource = register_buffer(dev, OP, device.buffer(descriptor.size as u64, vk::BufferKind::DeviceLocal), descriptor.size)?;
    if memory == Memory::Shared {
        let zeroed = dev.find_buffer(OP, resource.token).and_then(|entry| entry.buffer.borrow().zero().map_err(|failure| of_vk(OP, failure)));
        if let Err(failure) = zeroed {
            let _ = (resource.destroy)();
            return Err(failure);
        }
    }
    Ok(resource)
}

fn floats_to_bytes(floats: &[f32], length: usize) -> Vec<u8> {
    let mut bytes: Vec<u8> = floats.iter().flat_map(|value| value.to_le_bytes()).collect();
    bytes.truncate(length);
    bytes
}

fn create_mapped_buffer(dev: &Rc<Dev>, descriptor: &BufferDescriptor) -> Result<DriverMapped, Error> {
    const OP: &str = "Ogpu_vulkan.create_mapped_buffer";
    let device = dev.device(OP)?;
    let size = descriptor.size;
    let resource = register_buffer(dev, OP, device.buffer(size as u64, vk::BufferKind::DeviceLocal), size)?;
    let id = resource.token;
    if let Err(failure) = dev.find_buffer(OP, id).and_then(|entry| entry.buffer.borrow().zero().map_err(|failure| of_vk(OP, failure))) {
        let _ = (resource.destroy)();
        return Err(failure);
    }
    // The host copy exists only while the host uses it: made on its first ask, dropped on release
    // and destroy.
    let cell: Rc<RefCell<Option<Floats>>> = Rc::new(RefCell::new(None));
    let count = (size / 4) as usize;
    let make_view = {
        let cell = cell.clone();
        move || -> Floats {
            let mut slot = cell.borrow_mut();
            slot.get_or_insert_with(|| Rc::new(RefCell::new(vec![0f32; count]))).clone()
        }
    };
    let DriverResource { token, write, read, read_into, destroy } = resource;
    let destroy_cell = cell.clone();
    let release_cell = cell.clone();
    let flush_cell = cell;
    let (flush_dev, invalidate_dev) = (dev.clone(), dev.clone());
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
        view: Box::new(move || Some(view())),
        release_view: Box::new(move || *release_cell.borrow_mut() = None),
        flush: Box::new(move |length| {
            let Some(floats) = flush_cell.borrow().clone() else {
                return Err(fail(OP, ErrorKind::InvalidArgument, "no host view was written"));
            };
            let entry = flush_dev.find_buffer(OP, id)?;
            let bytes = floats_to_bytes(&floats.borrow(), length);
            entry.buffer.borrow().flush(&bytes).map_err(|failure| of_vk(OP, failure))
        }),
        invalidate: Box::new(move |length| {
            let floats = invalidate_view();
            let entry = invalidate_dev.find_buffer(OP, id)?;
            let all = floats.borrow().len() * 4;
            let mut bytes = floats_to_bytes(&floats.borrow(), all);
            if length > bytes.len() {
                return Err(fail(OP, ErrorKind::InvalidArgument, "range exceeds the buffer"));
            }
            entry.buffer.borrow().invalidate(&mut bytes[..length]).map_err(|failure| of_vk(OP, failure))?;
            for (value, chunk) in floats.borrow_mut().iter_mut().zip(bytes.chunks_exact(4)) {
                *value = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            }
            Ok(())
        }),
    })
}

fn register_texture(dev: &Rc<Dev>, format: TextureFormat, descriptor: &TextureDescriptor) -> Result<DriverResource, Error> {
    const OP: &str = "Ogpu_vulkan.create_texture";
    let device = dev.device(OP)?;
    let created = device.texture(vk::TextureDescriptor {
        width: descriptor.width as u32,
        height: descriptor.height as u32,
        depth: descriptor.depth.max(1) as u32,
        mip_levels: descriptor.mip_levels.max(1) as u32,
        samples: descriptor.sample_count.max(1) as u32,
        format: format_index(format),
        storage: descriptor.usage.contains(&TextureUsage::StorageBinding),
        attachment: descriptor.usage.contains(&TextureUsage::RenderAttachment),
    });
    let native = created.map_err(|failure| of_vk(OP, failure))?;
    live_add(1);
    let id = dev.token();
    let stored = TextureDescriptor { format, ..descriptor.clone() };
    dev.textures.borrow_mut().insert(id, Rc::new(TextureEntry { texture: native, descriptor: stored }));
    let (width, height) = (descriptor.width, descriptor.height);
    let (into_dev, read_dev) = (dev.clone(), dev.clone());
    let read_into = move |dev: &Rc<Dev>, offset: i64, destination: &mut [u8], at: usize, length: usize| -> Result<(), Error> {
        if offset != 0 || at != 0 || length != destination.len() || height <= 0 || length as i64 % height != 0 {
            return Err(fail(OP, ErrorKind::InvalidArgument, "destination must match the texture read range"));
        }
        let row = length as i64 / height;
        let texel = types::texel_bytes(format);
        if row < width * texel || row % texel != 0 {
            return Err(fail(OP, ErrorKind::InvalidArgument, "row pitch is invalid"));
        }
        let entry = dev.find_texture(OP, id)?;
        entry.texture.read(destination, row as u64, texel as u32).map_err(|failure| of_vk(OP, failure))
    };
    let read_again = read_into.clone();
    let destroy_dev = dev.clone();
    Ok(DriverResource {
        token: id,
        write: Box::new(|_, _| Err(fail(OP, ErrorKind::Unsupported, "use a transfer pass for textures"))),
        read: Box::new(move |offset, length| {
            let mut bytes = vec![0u8; length];
            read_again(&read_dev, offset, &mut bytes, 0, length)?;
            Ok(bytes)
        }),
        read_into: Box::new(move |offset, destination, at, length| read_into(&into_dev, offset, destination, at, length)),
        destroy: Box::new(move || {
            destroy_dev.textures.borrow_mut().remove(&id);
            live_add(-1);
            Ok(())
        }),
    })
}

fn own_pipeline(dev: &Rc<Dev>, pipeline: vk::Pipeline, local: (i64, i64, i64), max_threads: i64) -> DriverPipeline {
    live_add(1);
    let id = dev.token();
    dev.pipelines.borrow_mut().insert(id, Rc::new(PipelineEntry { pipeline, local }));
    let dev = dev.clone();
    DriverPipeline {
        pipeline_token: id,
        max_threads,
        destroy_pipeline: Box::new(move || {
            dev.pipelines.borrow_mut().remove(&id);
            live_add(-1);
            Ok(())
        }),
    }
}

fn kernel_space(kind: BindingKind) -> u32 {
    match kind {
        BindingKind::UniformBuffer | BindingKind::StorageBuffer => 0,
        BindingKind::SampledTexture | BindingKind::StorageTexture => 1,
        BindingKind::Sampler => 2,
        BindingKind::AccelerationStructure => 3,
    }
}

fn reflected_space(kind: u32) -> u32 {
    match kind {
        0 | 1 => 0,
        2 | 3 => 1,
        4 => 2,
        _ => 3,
    }
}

fn create_library(dev: &Rc<Dev>, shader: &Shader) -> Result<DriverLibrary, Error> {
    const OP: &str = "Ogpu_vulkan.create_library";
    let bytes = shader::bytes(shader).to_vec();
    let device = dev.device(OP)?;
    let module = device.shader(&bytes).map_err(|failure| of_vk(OP, failure))?;
    live_add(1);
    let module: Rc<RefCell<Option<vk::ShaderModule>>> = Rc::new(RefCell::new(Some(module)));
    let (pipeline_dev, pipeline_module) = (dev.clone(), module.clone());
    Ok(DriverLibrary {
        create_compute_pipeline_in: Box::new(move |entry: &str, constants: &[(String, ConstantValue)], declared: &[ShaderBinding]| {
            if !constants.is_empty() {
                return Err(fail(OP, ErrorKind::Unsupported, "SPIR-V kernels take no named function constants"));
            }
            let reflected = match vk::reflect::reflect_entry(&bytes, entry) {
                Ok(Some(reflected)) => reflected,
                Ok(None) | Err(_) => return Err(fail(OP, ErrorKind::InvalidArgument, "the module has no such entry point")),
            };
            let mut want: Vec<(u32, u32)> = declared.iter().map(|b| (b.binding as u32, kernel_space(b.kind))).collect();
            let mut have: Vec<(u32, u32)> = reflected.bindings.iter().map(|&(b, k)| (b, reflected_space(k))).collect();
            want.sort_unstable();
            have.sort_unstable();
            if want != have {
                return Err(fail(OP, ErrorKind::InvalidArgument, "SPIR-V reflection does not match the interface"));
            }
            let device = pipeline_dev.device(OP)?;
            let natives: Vec<vk::Binding> =
                reflected.bindings.iter().map(|&(binding, kind)| vk::Binding { binding, kind: kind as i32 }).collect();
            let module = pipeline_module.borrow();
            let Some(module) = module.as_ref() else {
                return Err(fail(OP, ErrorKind::StaleHandle, "library is destroyed"));
            };
            let pipeline = vk::Pipeline::compute(&device, module, entry, &natives).map_err(|failure| of_vk(OP, failure))?;
            let local = (reflected.local_size.0 as i64, reflected.local_size.1 as i64, reflected.local_size.2 as i64);
            Ok(own_pipeline(&pipeline_dev, pipeline, local, pipeline_dev.max_invocations))
        }),
        destroy_library: Box::new(move || {
            *module.borrow_mut() = None;
            live_add(-1);
            Ok(())
        }),
    })
}

fn create_render_pipeline(dev: &Rc<Dev>, options: RenderPipelineOptions, descriptor: &RenderDescriptor) -> Result<DriverPipeline, Error> {
    const OP: &str = "Ogpu_vulkan.create_render_pipeline";
    let device = dev.device(OP)?;
    let vertex_bytes = shader::bytes(&descriptor.vertex);
    let vertex = device.shader(vertex_bytes).map_err(|failure| of_vk(OP, failure))?;
    let fragment = match &descriptor.fragment {
        None => None,
        Some(shader) => Some(device.shader(shader::bytes(shader)).map_err(|failure| of_vk(OP, failure))?),
    };
    let reflect = |bytes: &[u8]| -> Result<Vec<(u32, u32)>, Error> {
        vk::reflect::reflect(bytes).or_else(|_| Err(fail(OP, ErrorKind::InvalidArgument, "invalid SPIR-V module")))
    };
    let mut interface = reflect(vertex_bytes)?;
    if let Some(shader) = &descriptor.fragment {
        interface.extend(reflect(shader::bytes(shader))?);
    }
    interface.sort_unstable();
    interface.dedup();
    let bindings: Vec<vk::Binding> = interface.iter().map(|&(binding, kind)| vk::Binding { binding, kind: kind as i32 }).collect();
    let topology = match options.topology {
        Primitive::PointList => 0,
        Primitive::LineList => 1,
        Primitive::TriangleList | Primitive::TriangleStrip => 2,
    };
    let color_format = match descriptor.color_format {
        ColorFormat::Rgba8Unorm => 1,
        ColorFormat::Bgra8Unorm => 2,
    };
    let has_depth = descriptor.depth_format != DepthFormat::NoDepth;
    let pipeline = vk::Pipeline::render(
            &device,
            &vertex,
            fragment.as_ref(),
            &bindings,
            vk::PipelineDescriptor {
                blend: blend_index(options.blend),
                topology,
                color_format,
                has_depth,
                samples: descriptor.sample_count.max(1) as i32,
            },
        )
    .map_err(|failure| of_vk(OP, failure))?;
    Ok(own_pipeline(dev, pipeline, (1, 1, 1), 0))
}

fn create_sampler(dev: &Rc<Dev>, descriptor: &SamplerDescriptor) -> Result<DriverSampler, Error> {
    const OP: &str = "Ogpu_vulkan.create_sampler";
    let filter = |filter: SamplerFilter| match filter {
        SamplerFilter::Nearest => 0,
        SamplerFilter::Linear => 1,
    };
    let address = |mode: AddressMode| match mode {
        AddressMode::ClampToEdge => 0,
        AddressMode::Repeat => 1,
        AddressMode::MirrorRepeat => 2,
    };
    let mip = match descriptor.mip_filter {
        MipFilter::NoMip => 0,
        MipFilter::NearestMip => 1,
        MipFilter::LinearMip => 2,
    };
    let device = dev.device(OP)?;
    let native = device.sampler(vk::SamplerDescriptor {
            min_filter: filter(descriptor.min_filter),
            mag_filter: filter(descriptor.mag_filter),
            mip_filter: mip,
            address_u: address(descriptor.address_u),
            address_v: address(descriptor.address_v),
            anisotropy: descriptor.max_anisotropy as i32,
            lod_min: descriptor.lod_min,
            lod_max: descriptor.lod_max,
        })
    .map_err(|failure| of_vk(OP, failure))?;
    live_add(1);
    let id = dev.token();
    dev.samplers.borrow_mut().insert(id, Rc::new(native));
    let dev = dev.clone();
    Ok(DriverSampler {
        sampler_token: id,
        destroy_sampler: Box::new(move || {
            dev.samplers.borrow_mut().remove(&id);
            live_add(-1);
            Ok(())
        }),
    })
}

fn create_icb(dev: &Rc<Dev>, max_commands: i64) -> Result<DriverIcb, Error> {
    const OP: &str = "Ogpu_vulkan.icb";
    let id = dev.token();
    live_add(1);
    let commands = Rc::new(RefCell::new(vec![IcbCommand::default(); max_commands.max(0) as usize]));
    dev.icbs.borrow_mut().insert(id, commands.clone());
    let at = move |index: i64, edit: &mut dyn FnMut(&mut IcbCommand)| -> Result<(), Error> {
        let mut commands = commands.borrow_mut();
        match usize::try_from(index).ok().and_then(|index| commands.get_mut(index)) {
            Some(command) => {
                edit(command);
                Ok(())
            }
            None => Err(fail(OP, ErrorKind::InvalidArgument, "indirect command index is out of range")),
        }
    };
    let (a1, a2, a3, a4) = (at.clone(), at.clone(), at.clone(), at);
    let destroy_dev = dev.clone();
    Ok(DriverIcb {
        icb_token: id,
        icb_set_pipeline: Box::new(move |index, pipeline| a1(index, &mut |c| c.pipeline = Some(pipeline))),
        icb_set_buffer: Box::new(move |index, _stage, slot, offset, buffer| {
            a2(index, &mut |c| c.buffers.insert(0, (slot, offset, buffer)))
        }),
        icb_draw: Box::new(move |index, primitive, first, count, instances| {
            a3(index, &mut |c| c.draw = Some(IcbDraw::Draw { primitive, first, count, instances }))
        }),
        icb_draw_indexed: Box::new(move |index, primitive, index_type, buffer, offset, count, instances| {
            a4(index, &mut |c| c.draw = Some(IcbDraw::Indexed { primitive, index_type, buffer, offset, count, instances }))
        }),
        destroy_icb: Box::new(move || {
            destroy_dev.icbs.borrow_mut().remove(&id);
            live_add(-1);
            Ok(())
        }),
    })
}

fn create_argument(dev: &Rc<Dev>) -> Result<DriverArgument, Error> {
    const OP: &str = "Ogpu_vulkan.argument";
    let (texture_dev, sampler_dev) = (dev.clone(), dev.clone());
    Ok(DriverArgument {
        argument_length: 16,
        argument_alignment: 16,
        argument_texture: Box::new(move |buffer, offset, _slot, texture| {
            let texture = texture_dev.find_texture(OP, texture)?;
            texture_dev.arguments.borrow_mut().entry((buffer, offset)).or_default().0 = Some(texture);
            Ok(())
        }),
        argument_sampler: Box::new(move |buffer, offset, _slot, sampler| {
            let sampler = sampler_dev.find_sampler(OP, sampler)?;
            sampler_dev.arguments.borrow_mut().entry((buffer, offset)).or_default().1 = Some(sampler);
            Ok(())
        }),
        destroy_argument: Box::new(|| Ok(())),
    })
}

fn read_bytes(dev: &Dev, op: &str, token: Token, offset: i64, length: i64) -> Result<Vec<u8>, Error> {
    let buffer = dev.find_buffer(op, token)?;
    if offset < 0 || length < 0 || offset > buffer.size || length > buffer.size - offset {
        return Err(fail(op, ErrorKind::InvalidArgument, "read range is invalid"));
    }
    let mut bytes = vec![0u8; length as usize];
    buffer_read_into(op, &buffer, offset, &mut bytes, 0, length as usize)?;
    Ok(bytes)
}

fn generated_buffer(dev: &Rc<Dev>, generated: &mut Vec<DriverResource>, bytes: &[u8]) -> Result<Token, Error> {
    let resource = create_buffer(
        dev,
        Memory::DeviceLocal,
        &BufferDescriptor {
            label: Some("Vulkan ray bounds".to_owned()),
            size: bytes.len() as i64,
            usage: vec![BufferUsage::Storage, BufferUsage::CopyDst],
        },
    )?;
    match (resource.write)(0, bytes) {
        Ok(()) => {
            let token = resource.token;
            generated.push(resource);
            Ok(token)
        }
        Err(failure) => {
            let _ = (resource.destroy)();
            Err(failure)
        }
    }
}

fn put_float(bytes: &mut [u8], at: usize, value: f64) {
    bytes[at..at + 4].copy_from_slice(&(value as f32).to_le_bytes());
}

fn put_i32(bytes: &mut [u8], at: usize, value: i64) {
    bytes[at..at + 4].copy_from_slice(&(value as i32).to_le_bytes());
}

fn read_float(bytes: &[u8], at: usize) -> f64 {
    f32::from_le_bytes([bytes[at], bytes[at + 1], bytes[at + 2], bytes[at + 3]]) as f64
}

fn read_u32(bytes: &[u8], at: usize) -> i64 {
    i32::from_le_bytes([bytes[at], bytes[at + 1], bytes[at + 2], bytes[at + 3]]) as i64
}

fn padded_bounds(minimum: &[f64; 3], maximum: &[f64; 3]) -> [(f64, f64); 3] {
    std::array::from_fn(|axis| {
        let epsilon = 1e-5 * omax(1., omax(minimum[axis].abs(), maximum[axis].abs()));
        (minimum[axis] - epsilon, maximum[axis] + epsilon)
    })
}

fn box_geometry(entry: Rc<BufferEntry>, count: i64) -> GeomRecord {
    GeomRecord {
        kind: vk::GeometryKind::Boxes,
        buffer: entry,
        offset: 0,
        stride: 24,
        count: count as u32,
        opaque: false,
        duplicate: false,
    }
}

fn motion_triangle_boxes(
    dev: &Rc<Dev>,
    op: &str,
    generated: &mut Vec<DriverResource>,
    keyframes: &[(Token, i64)],
    stride: i64,
    vertices: i64,
) -> Result<GeomRecord, Error> {
    let length = (vertices - 1) * stride + 12;
    let mut frames = Vec::new();
    for &(token, offset) in keyframes {
        frames.push(read_bytes(dev, op, token, offset, length)?);
    }
    let triangles = (vertices / 3).max(0) as usize;
    let mut boxes = vec![0u8; triangles * 24];
    for triangle in 0..triangles {
        let mut minimum = [f64::INFINITY; 3];
        let mut maximum = [f64::NEG_INFINITY; 3];
        for frame in &frames {
            for vertex in 0..3 {
                for axis in 0..3 {
                    let at = (triangle * 3 + vertex) * stride as usize + axis * 4;
                    let value = read_float(frame, at);
                    minimum[axis] = omin(minimum[axis], value);
                    maximum[axis] = omax(maximum[axis], value);
                }
            }
        }
        let padded = padded_bounds(&minimum, &maximum);
        for (axis, (low, high)) in padded.iter().enumerate() {
            put_float(&mut boxes, triangle * 24 + axis * 4, *low);
            put_float(&mut boxes, triangle * 24 + 12 + axis * 4, *high);
        }
    }
    let token = generated_buffer(dev, generated, &boxes)?;
    let entry = dev.find_buffer(op, token)?;
    Ok(box_geometry(entry, vertices / 3))
}

fn geometry_records(dev: &Rc<Dev>, generated: &mut Vec<DriverResource>, geometries: &[DriverGeometry]) -> Result<Vec<GeomRecord>, Error> {
    const OP: &str = "Ogpu_vulkan.accel";
    let mut records = Vec::new();
    for geometry in geometries {
        records.push(match geometry {
            DriverGeometry::Triangles { vertices, offset, vertex_stride, vertex_count, .. } => GeomRecord {
                kind: vk::GeometryKind::Triangles,
                buffer: dev.find_buffer(OP, *vertices)?,
                offset: *offset as u64,
                stride: *vertex_stride as u64,
                count: *vertex_count as u32,
                opaque: true,
                duplicate: false,
            },
            DriverGeometry::MotionTriangles { keyframes, vertex_stride, vertex_count } => {
                motion_triangle_boxes(dev, OP, generated, keyframes, *vertex_stride, *vertex_count)?
            }
            DriverGeometry::Boxes { boxes, stride, count, opaque, duplicate } if boxes.len() == 1 => GeomRecord {
                kind: vk::GeometryKind::Boxes,
                buffer: dev.find_buffer(OP, boxes[0].0)?,
                offset: boxes[0].1 as u64,
                stride: *stride as u64,
                count: *count as u32,
                opaque: *opaque,
                duplicate: *duplicate,
            },
            DriverGeometry::Boxes { .. } | DriverGeometry::Curves { .. } => {
                return Err(fail(OP, ErrorKind::Unsupported, "Vulkan ray queries do not expose native curves or multi-keyframe boxes"));
            }
        });
    }
    Ok(records)
}

/// The bridge's geometries over records (each buffer handle is a cheap shared clone).
fn native_geometries(records: &[GeomRecord]) -> Vec<vk::Geometry> {
    records
        .iter()
        .map(|record| vk::Geometry {
            kind: record.kind,
            buffer: record.buffer.buffer.borrow().clone(),
            offset: record.offset,
            stride: record.stride,
            count: record.count,
            opaque: record.opaque,
            duplicate_any_hit: record.duplicate,
        })
        .collect()
}

fn local_bounds(op: &str, geometry: &GeomRecord) -> Result<([f64; 3], [f64; 3]), Error> {
    if geometry.kind != vk::GeometryKind::Triangles {
        return Err(fail(op, ErrorKind::Unsupported, "motion instances require triangle prototypes"));
    }
    if geometry.count == 0 {
        return Err(fail(op, ErrorKind::InvalidArgument, "motion instance prototype has no vertices"));
    }
    let (stride, vertices) = (geometry.stride as usize, geometry.count as usize);
    let mut bytes = vec![0u8; (vertices - 1) * stride + 12];
    geometry.buffer.buffer.borrow().read(geometry.offset, &mut bytes).map_err(|failure| of_vk(op, failure))?;
    let mut minimum = [f64::INFINITY; 3];
    let mut maximum = [f64::NEG_INFINITY; 3];
    for vertex in 0..vertices {
        for axis in 0..3 {
            let value = read_float(&bytes, vertex * stride + axis * 4);
            minimum[axis] = omin(minimum[axis], value);
            maximum[axis] = omax(maximum[axis], value);
        }
    }
    Ok((minimum, maximum))
}

fn transform_point(data: &[u8], key: i64, point: &[f64; 3]) -> [f64; 3] {
    let at = key as usize * 48;
    std::array::from_fn(|row| {
        read_float(data, at + row * 4) * point[0]
            + read_float(data, at + (3 + row) * 4) * point[1]
            + read_float(data, at + (6 + row) * 4) * point[2]
            + read_float(data, at + (9 + row) * 4)
    })
}

/// One proxy box geometry for each motion instance: the prototype's local bounds swept through
/// its transform keyframes, and the 68-byte native instance records that name them.
fn motion_instance_geometries(
    dev: &Rc<Dev>,
    op: &str,
    generated: &mut Vec<DriverResource>,
    instances: Token,
    offset: i64,
    count: i64,
    structures: &[Token],
    transforms: (Token, i64, i64),
) -> Result<(Vec<GeomRecord>, Vec<u8>), Error> {
    let records = read_bytes(dev, op, instances, offset, count * 44)?;
    let (transform_token, transform_offset, transform_count) = transforms;
    let transform_data = read_bytes(dev, op, transform_token, transform_offset, transform_count * 48)?;
    let mut built = Vec::new();
    for index in 0..count {
        let base = index as usize * 44;
        let mask = read_u32(&records, base);
        let structure = read_u32(&records, base + 4);
        let user_id = read_u32(&records, base + 8);
        let start = read_u32(&records, base + 12);
        let keys = read_u32(&records, base + 16);
        if structure < 0 || structure as usize >= structures.len() {
            return Err(fail(op, ErrorKind::InvalidArgument, "motion instance structure index is out of range"));
        }
        if keys < 2 || start < 0 || keys > transform_count || start > transform_count - keys {
            return Err(fail(op, ErrorKind::InvalidArgument, "motion instance transform range is invalid"));
        }
        let prototype = dev.accel_geometries.borrow().get(&structures[structure as usize]).cloned();
        let triangle = match prototype {
            None => return Err(fail(op, ErrorKind::InvalidArgument, "motion instance prototype is not a triangle BLAS")),
            Some(geometries) if geometries.len() == 1 => geometries[0].clone(),
            Some(_) => return Err(fail(op, ErrorKind::Unsupported, "motion instances require one triangle geometry per prototype")),
        };
        let (minimum, maximum) = local_bounds(op, &triangle)?;
        let mut bytes = vec![0u8; (keys as usize - 1) * 24];
        for interval in 0..keys - 1 {
            let mut world_min = [f64::INFINITY; 3];
            let mut world_max = [f64::NEG_INFINITY; 3];
            for key in 0..2 {
                for corner in 0..8 {
                    let point: [f64; 3] =
                        std::array::from_fn(|axis| if corner & (1 << axis) == 0 { minimum[axis] } else { maximum[axis] });
                    let world = transform_point(&transform_data, start + interval + key, &point);
                    for axis in 0..3 {
                        world_min[axis] = omin(world_min[axis], world[axis]);
                        world_max[axis] = omax(world_max[axis], world[axis]);
                    }
                }
            }
            let padded = padded_bounds(&world_min, &world_max);
            let box_at = interval as usize * 24;
            for (axis, (low, high)) in padded.iter().enumerate() {
                put_float(&mut bytes, box_at + axis * 4, *low);
                put_float(&mut bytes, box_at + 12 + axis * 4, *high);
            }
        }
        let token = generated_buffer(dev, generated, &bytes)?;
        let entry = dev.find_buffer(op, token)?;
        built.push((box_geometry(entry, keys - 1), structure, mask, user_id));
    }
    let mut native = vec![0u8; count as usize * 68];
    for (index, (_, structure, mask, user_id)) in built.iter().enumerate() {
        let base = index * 68;
        for at in [0, 16, 32] {
            put_float(&mut native, base + at, 1.);
        }
        put_i32(&mut native, base + 48, *user_id);
        put_i32(&mut native, base + 52, *mask);
        put_i32(&mut native, base + 56, index as i64);
        put_i32(&mut native, base + 60, *structure);
    }
    Ok((built.into_iter().map(|(geometry, ..)| geometry).collect(), native))
}

fn destroy_all(resources: Vec<DriverResource>) {
    for resource in resources {
        let _ = (resource.destroy)();
    }
}

fn build_accel(
    dev: &Rc<Dev>,
    op: &str,
    descriptor: &DriverAccelDescriptor,
    generated: &mut Vec<DriverResource>,
    stored: &mut Option<Vec<GeomRecord>>,
) -> Result<vk::AccelerationStructure, Error> {
    let device = dev.device(op)?;
    match descriptor {
        DriverAccelDescriptor::Blas { geometries, refit, .. } => {
            let records = geometry_records(dev, generated, geometries)?;
            let created = device.acceleration_blas(&native_geometries(&records), *refit);
            *stored = Some(records);
            created.map_err(|failure| of_vk(op, failure))
        }
        DriverAccelDescriptor::Tlas {
            instances,
            offset,
            instance_count,
            kind: InstanceKind::MotionInstances,
            structures,
            motion_transforms: Some(transforms),
        } => {
            let (proxies, instance_records) =
                motion_instance_geometries(dev, op, generated, *instances, *offset, *instance_count, structures, *transforms)?;
            let instance_token = generated_buffer(dev, generated, &instance_records)?;
            let instance_buffer = dev.find_buffer(op, instance_token)?;
            let mut children = Vec::new();
            for proxy in &proxies {
                let created = device.acceleration_blas(&native_geometries(std::slice::from_ref(proxy)), false);
                children.push(created.map_err(|failure| of_vk(op, failure))?);
            }
            let refs = children;
            let buffer = instance_buffer.buffer.borrow();
            device.acceleration_tlas(&buffer, 0, *instance_count as u32, &refs, true).map_err(|failure| of_vk(op, failure))
        }
        DriverAccelDescriptor::Tlas { kind: InstanceKind::MotionInstances, .. } => {
            Err(fail(op, ErrorKind::InvalidArgument, "motion instances require transform keyframes"))
        }
        DriverAccelDescriptor::Tlas { instances, offset, instance_count, structures, .. } => {
            let buffer = dev.find_buffer(op, *instances)?;
            let mut handles = Vec::new();
            for id in structures {
                handles.push(dev.find_accel(op, *id)?);
            }
            let refs: Vec<vk::AccelerationStructure> = handles.iter().map(|handle| (**handle).clone()).collect();
            let buffer = buffer.buffer.borrow();
            device.acceleration_tlas(&buffer, *offset as u64, *instance_count as u32, &refs, false).map_err(|failure| of_vk(op, failure))
        }
        DriverAccelDescriptor::Sized { size, template } => {
            let template = dev.find_accel(op, *template)?;
            device.acceleration_sized(*size as u64, &template).map_err(|failure| of_vk(op, failure))
        }
    }
}

fn create_accel(dev: &Rc<Dev>, descriptor: &DriverAccelDescriptor) -> Result<DriverAccel, Error> {
    const OP: &str = "Ogpu_vulkan.create_accel";
    let mut generated = Vec::new();
    let mut stored = None;
    let native = match build_accel(dev, OP, descriptor, &mut generated, &mut stored) {
        Ok(native) => native,
        Err(failure) => {
            destroy_all(generated);
            return Err(failure);
        }
    };
    live_add(1);
    let id = dev.token();
    let sizes = native.sizes();
    dev.accels.borrow_mut().insert(id, Rc::new(native));
    dev.accel_resources.borrow_mut().insert(id, generated);
    if let Some(records) = stored {
        dev.accel_geometries.borrow_mut().insert(id, records);
    }
    let dev = dev.clone();
    Ok(DriverAccel {
        accel_token: id,
        accel_sizes: ogpu_core::backend::AccelSizes {
            structure_size: sizes.structure_bytes as i64,
            build_scratch_size: sizes.build_scratch_bytes as i64,
            refit_scratch_size: sizes.update_scratch_bytes as i64,
        },
        destroy_accel: Box::new(move || {
            dev.accels.borrow_mut().remove(&id);
            dev.accel_geometries.borrow_mut().remove(&id);
            let resources = dev.accel_resources.borrow_mut().remove(&id).unwrap_or_default();
            destroy_all(resources);
            live_add(-1);
            Ok(())
        }),
    })
}

/// Epochs the queue has submitted and seen complete.
struct QueueState {
    queue: RefCell<Option<vk::Queue>>,
    next_epoch: Cell<i64>,
    completed: Cell<i64>,
}

impl QueueState {
    fn active(&self) -> i64 {
        self.next_epoch.get() - 1 - self.completed.get()
    }
    fn native(&self, op: &str) -> Result<Ref<'_, vk::Queue>, Error> {
        Ref::filter_map(self.queue.borrow(), Option::as_ref)
            .or_else(|_| Err(fail(op, ErrorKind::StaleHandle, "queue is destroyed")))
    }
    fn bound_epoch(&self, op: &str, epoch: i64) -> Result<i64, Error> {
        if epoch <= 0 || epoch >= self.next_epoch.get() {
            Err(fail(op, ErrorKind::InvalidArgument, "epoch was not submitted"))
        } else {
            Ok(epoch)
        }
    }
}

/// One recording command buffer, shared by its encoders.
struct Cmds {
    dev: Rc<Dev>,
    queue: Rc<QueueState>,
    commands: RefCell<Option<vk::Commands>>,
    finished: Cell<bool>,
}

impl Cmds {
    fn run(&self, op: &str, call: &mut dyn FnMut(&mut vk::Commands) -> Result<(), vk::Error>) -> Result<(), Error> {
        match self.commands.borrow_mut().as_mut() {
            Some(commands) => call(commands).map_err(|failure| of_vk(op, failure)),
            None => Err(fail(op, ErrorKind::InvalidState, "commands are finished")),
        }
    }

    fn commit_with(&self, op: &str, submit: &mut dyn FnMut(&mut Option<vk::Commands>) -> Result<u64, vk::Error>) -> Result<Receipt, Error> {
        if self.finished.get() {
            Err(fail(op, ErrorKind::InvalidState, "commands are finished"))
        } else if self.queue.active() >= 3 {
            Err(fail(op, ErrorKind::Capacity, "frames-in-flight capacity reached"))
        } else {
            let epoch = submit(&mut self.commands.borrow_mut()).map_err(|failure| of_vk(op, failure))? as i64;
            self.finished.set(true);
            self.queue.next_epoch.set(epoch + 1);
            Ok(Receipt { epoch })
        }
    }
}

fn compute_encoder(cmds: &Rc<Cmds>) -> Result<DriverComputeEncoder, Error> {
    const OP: &str = "Ogpu_vulkan.queue";
    cmds.run(OP, &mut |c| {
        c.compute_begin();
        Ok(())
    })?;
    // A kernel runs in groups of its own local size: the threadgroup a caller names is Metal's, and
    // SPIR-V cannot change it.
    let local = Rc::new(Cell::new((1i64, 1i64, 1i64)));
    let (c1, c2, c3, c4, c5, c6, c7, c8) =
        (cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone());
    let local_set = local.clone();
    Ok(DriverComputeEncoder {
        set_pipeline: Box::new(move |id| {
            let entry = c1.dev.find_pipeline(OP, id)?;
            local_set.set(entry.local);
            c1.run(OP, &mut |c| c.set_pipeline(&entry.pipeline))
        }),
        set_buffer: Box::new(move |index, offset, id| {
            let entry = c2.dev.find_buffer(OP, id)?;
            c2.run(OP, &mut |c| c.set_buffer(index as u32, offset as u64, &entry.buffer.borrow()))
        }),
        set_bytes: Box::new(move |index, bytes| c3.run(OP, &mut |c| c.set_bytes(index as u32, bytes))),
        set_texture: Box::new(move |index, id| {
            let entry = c4.dev.find_texture(OP, id)?;
            c4.run(OP, &mut |c| c.set_texture(index as u32, &entry.texture))
        }),
        set_accel: Box::new(move |index, id| {
            let accel = c5.dev.find_accel(OP, id)?;
            c5.run(OP, &mut |c| c.set_acceleration(index as u32, &accel))
        }),
        dispatch_threads: Box::new(move |(x, y, z), _threadgroup| {
            let (gx, gy, gz) = local.get();
            let groups = |threads: i64, group: i64| ((threads + group - 1) / group).max(0) as u32;
            c6.run(OP, &mut |c| {
                c.dispatch(groups(x, gx), groups(y, gy), groups(z, gz));
                Ok(())
            })
        }),
        dispatch_indirect: Box::new(move |id, offset, _threadgroup| {
            let entry = c7.dev.find_buffer(OP, id)?;
            c7.run(OP, &mut |c| c.dispatch_indirect(&entry.buffer.borrow(), offset as u64))
        }),
        compute_use_accels: Box::new(|_| Ok(())),
        end_compute: Box::new(move || {
            c8.run(OP, &mut |c| {
                c.encoder_end();
                Ok(())
            })
        }),
    })
}

fn accel_encoder(cmds: &Rc<Cmds>) -> Result<DriverAccelEncoder, Error> {
    const OP: &str = "Ogpu_vulkan.queue";
    let (c1, c2, c3, c4, c5) = (cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone());
    Ok(DriverAccelEncoder {
        build: Box::new(move |id, scratch, scratch_offset| {
            let accel = c1.dev.find_accel(OP, id)?;
            let scratch = c1.dev.find_buffer(OP, scratch)?;
            c1.run(OP, &mut |c| c.build_acceleration(&accel, &scratch.buffer.borrow(), scratch_offset as u64))
        }),
        compact: Box::new(move |src, dst| {
            let source = c2.dev.find_accel(OP, src)?;
            let destination = c2.dev.find_accel(OP, dst)?;
            c2.run(OP, &mut |c| c.compact_acceleration(&source, &destination))
        }),
        write_compacted_size: Box::new(move |id, dst, offset| {
            let accel = c3.dev.find_accel(OP, id)?;
            let buffer = c3.dev.find_buffer(OP, dst)?;
            c3.run(OP, &mut |c| c.write_compacted_size(&accel, &buffer.buffer.borrow(), offset as u64))
        }),
        refit: Box::new(move |id, descriptor, scratch, scratch_offset| {
            let accel = c4.dev.find_accel(OP, id)?;
            let scratch = c4.dev.find_buffer(OP, scratch)?;
            let DriverAccelDescriptor::Blas { geometries, .. } = descriptor else {
                return Err(fail(OP, ErrorKind::InvalidArgument, "Vulkan refit requires a BLAS descriptor"));
            };
            let mut generated = Vec::new();
            let records = match geometry_records(&c4.dev, &mut generated, geometries) {
                Ok(records) => records,
                Err(failure) => {
                    destroy_all(generated);
                    return Err(failure);
                }
            };
            let refit = c4.run(OP, &mut |c| {
                c.refit_acceleration(&accel, &native_geometries(&records), &scratch.buffer.borrow(), scratch_offset as u64)
            });
            if let Err(failure) = refit {
                destroy_all(generated);
                return Err(failure);
            }
            let old = c4.dev.accel_resources.borrow_mut().insert(id, generated).unwrap_or_default();
            destroy_all(old);
            c4.dev.accel_geometries.borrow_mut().insert(id, records);
            Ok(())
        }),
        end_accel: Box::new(move || {
            c5.run(OP, &mut |c| {
                c.encoder_end();
                Ok(())
            })
        }),
    })
}

fn blit_encoder(cmds: &Rc<Cmds>) -> Result<DriverBlitEncoder, Error> {
    const OP: &str = "Ogpu_vulkan.queue";
    let (c1, c2) = (cmds.clone(), cmds.clone());
    Ok(DriverBlitEncoder {
        buffer_to_texture: Box::new(move |src, offset, bytes_per_row, _bytes_per_image, dst, mip, origin, extent| {
            let buffer = c1.dev.find_buffer(OP, src)?;
            let texture = c1.dev.find_texture(OP, dst)?;
            let texel = types::texel_bytes(texture.descriptor.format);
            let args = vk::BlitDescriptor {
                buffer_offset: offset as u64,
                bytes_per_row: bytes_per_row as u32,
                texel_bytes: texel as u32,
                mip_level: mip as u32,
                x: origin.x as i32,
                y: origin.y as i32,
                z: origin.z as i32,
                width: extent.width as u32,
                height: extent.height as u32,
                depth: extent.depth as u32,
            };
            c1.run(OP, &mut |c| c.blit_buffer_to_texture(&buffer.buffer.borrow(), &texture.texture, args))
        }),
        end_blit: Box::new(move || {
            c2.run(OP, &mut |c| {
                c.encoder_end();
                Ok(())
            })
        }),
    })
}

/// A render pass's recording state: what a draw batch and an indirect replay re-enter.
struct Render {
    cmds: Rc<Cmds>,
}

impl Render {
    const OP: &'static str = "Ogpu_vulkan.queue";

    fn set_pipeline(&self, id: Token) -> Result<(), Error> {
        let entry = self.cmds.dev.find_pipeline(Self::OP, id)?;
        self.cmds.run(Self::OP, &mut |c| c.set_pipeline(&entry.pipeline))
    }

    fn set_stage_buffer(&self, index: i64, offset: i64, id: Token) -> Result<(), Error> {
        let argument = self.cmds.dev.arguments.borrow().get(&(id, offset)).cloned();
        match argument {
            Some((texture, sampler)) => {
                let (Some(texture), Some(sampler)) = (texture, sampler) else {
                    return Err(fail(Self::OP, ErrorKind::InvalidArgument, "argument slice is incomplete"));
                };
                self.cmds.run(Self::OP, &mut |c| c.set_texture(index as u32, &texture.texture))?;
                self.cmds.run(Self::OP, &mut |c| c.set_sampler(index as u32 + 1, &sampler))
            }
            None => {
                let entry = self.cmds.dev.find_buffer(Self::OP, id)?;
                self.cmds.run(Self::OP, &mut |c| c.set_buffer(index as u32, offset as u64, &entry.buffer.borrow()))
            }
        }
    }

    fn draw(&self, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.cmds.run(Self::OP, &mut |c| c.draw(primitive_index(primitive), first as u32, count as u32, instances as u32))
    }

    fn draw_indexed(&self, primitive: Primitive, index_type: IndexType, id: Token, offset: i64, count: i64, instances: i64) -> Result<(), Error> {
        let entry = self.cmds.dev.find_buffer(Self::OP, id)?;
        self.cmds.run(Self::OP, &mut |c| {
            c.draw_indexed(
                &entry.buffer.borrow(),
                primitive_index(primitive),
                index_kind(index_type),
                offset as u64,
                count as u32,
                instances as u32,
            )
        })
    }
}

fn render_encoder(cmds: &Rc<Cmds>, target: &DriverRenderTarget) -> Result<DriverRenderEncoder, Error> {
    const OP: &str = "Ogpu_vulkan.queue";
    let dev = &cmds.dev;
    let mut colors = Vec::new();
    for color in &target.target_colors {
        let texture = dev.find_texture(OP, color.color)?;
        let resolve = match color.color_resolve {
            None => None,
            Some(id) => Some(dev.find_texture(OP, id)?),
        };
        colors.push((texture, resolve, color.color_load, color.color_store, color.color_clear));
    }
    let depth = match &target.target_depth {
        None => None,
        Some(depth) => Some((dev.find_texture(OP, depth.depth)?, depth.depth_load, depth.depth_store, depth.depth_clear)),
    };
    let attachments: Vec<vk::ColorAttachment> = colors
        .iter()
        .map(|(texture, resolve, load, store, (r, g, b, a))| vk::ColorAttachment {
            texture: texture.texture.clone(),
            resolve: resolve.as_ref().map(|t| t.texture.clone()),
            load: load_index(*load),
            store: store_index(*store),
            clear: [*r, *g, *b, *a],
        })
        .collect();
    let depth_attachment = depth.as_ref().map(|(texture, load, store, clear)| vk::DepthAttachment {
        texture: texture.texture.clone(),
        load: load_index(*load),
        store: store_index(*store),
        clear: *clear,
    });
    let size = vk::Size { width: target.target_width as u32, height: target.target_height as u32 };
    let mut depth_attachment = depth_attachment;
    cmds.run(OP, &mut |c| c.render_begin(&attachments, depth_attachment.take(), size))?;
    let render = Rc::new(Render { cmds: cmds.clone() });
    let r = |render: &Rc<Render>| render.clone();
    let (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15) = (
        r(&render), r(&render), r(&render), r(&render), r(&render), r(&render), r(&render), r(&render),
        r(&render), r(&render), r(&render), r(&render), r(&render), r(&render), r(&render),
    );
    Ok(DriverRenderEncoder {
        render_set_pipeline: Box::new(move |id| r1.set_pipeline(id)),
        set_stage_buffer: Box::new(move |_stage, index, offset, id| r2.set_stage_buffer(index, offset, id)),
        set_stage_texture: Box::new(move |_stage, index, id| {
            let entry = r3.cmds.dev.find_texture(OP, id)?;
            r3.cmds.run(OP, &mut |c| c.set_texture(index as u32, &entry.texture))
        }),
        set_stage_sampler: Box::new(move |_stage, index, id| {
            let sampler = r4.cmds.dev.find_sampler(OP, id)?;
            r4.cmds.run(OP, &mut |c| c.set_sampler(index as u32, &sampler))
        }),
        set_viewport: Box::new(move |rect| {
            r5.cmds.run(OP, &mut |c| {
                c.viewport(vk::Rect { x: rect.x as i32, y: rect.y as i32, width: rect.width as i32, height: rect.height as i32 });
                Ok(())
            })
        }),
        set_scissor: Box::new(move |rect| {
            r6.cmds.run(OP, &mut |c| {
                c.scissor(vk::Rect { x: rect.x as i32, y: rect.y as i32, width: rect.width as i32, height: rect.height as i32 })
            })
        }),
        set_cull: Box::new(move |cull| {
            let mode = match cull {
                Cull::CullNone => 0,
                Cull::CullFront => 1,
                Cull::CullBack => 2,
            };
            r7.cmds.run(OP, &mut |c| c.cull(mode))
        }),
        set_winding: Box::new(move |winding| {
            r8.cmds.run(OP, &mut |c| {
                c.winding(winding == Winding::Clockwise);
                Ok(())
            })
        }),
        set_depth_state: Box::new(move |state| {
            r9.cmds.run(OP, &mut |c| match state {
                None => c.depth(-1, false),
                Some(state) => c.depth(compare_index(state.depth_compare), state.depth_write),
            })
        }),
        draw: Box::new(move |primitive, first, count, instances| r10.draw(primitive, first, count, instances)),
        draw_indexed: Box::new(move |primitive, index_type, id, offset, count, instances| {
            r11.draw_indexed(primitive, index_type, id, offset, count, instances)
        }),
        draw_indexed_indirect: Box::new(move |primitive, index_type, id, offset, arguments_buffer, arguments_offset| {
            let entry = r12.cmds.dev.find_buffer(OP, id)?;
            let arguments = r12.cmds.dev.find_buffer(OP, arguments_buffer)?;
            r12.cmds.run(OP, &mut |c| {
                c.draw_indexed_indirect(
                    &entry.buffer.borrow(),
                    &arguments.buffer.borrow(),
                    primitive_index(primitive),
                    index_kind(index_type),
                    offset as u64,
                    arguments_offset as u64,
                )
            })
        }),
        draw_batch: Box::new(move |draws| {
            for draw in draws {
                r13.set_pipeline(draw.batch_pipeline)?;
                for &(_stage, index, buffer, offset) in &draw.batch_buffers {
                    r13.set_stage_buffer(index, offset, buffer)?;
                }
                match draw.batch_index {
                    Some((index_type, buffer, offset, count)) => {
                        r13.draw_indexed(draw.batch_primitive, index_type, buffer, offset, count, draw.batch_instances)?
                    }
                    None => r13.draw(draw.batch_primitive, draw.batch_vertex_start, draw.batch_vertex_count, draw.batch_instances)?,
                }
            }
            Ok(())
        }),
        use_resources: Box::new(|_| Ok(())),
        execute_icb: Box::new(move |id, location, length| {
            let recorded = r14.cmds.dev.icbs.borrow().get(&id).cloned();
            let Some(recorded) = recorded else {
                return Err(fail(OP, ErrorKind::InvalidArgument, "indirect commands are unknown"));
            };
            for i in location..location + length {
                let command = usize::try_from(i).ok().and_then(|i| recorded.borrow().get(i).cloned());
                let Some(command) = command else {
                    return Err(fail(OP, ErrorKind::InvalidArgument, "indirect command index is out of range"));
                };
                if let Some(pipeline) = command.pipeline {
                    r14.set_pipeline(pipeline)?;
                }
                for &(slot, offset, buffer) in &command.buffers {
                    r14.set_stage_buffer(slot, offset, buffer)?;
                }
                match command.draw {
                    Some(IcbDraw::Draw { primitive, first, count, instances }) => r14.draw(primitive, first, count, instances)?,
                    Some(IcbDraw::Indexed { primitive, index_type, buffer, offset, count, instances }) => {
                        r14.draw_indexed(primitive, index_type, buffer, offset, count, instances)?
                    }
                    None => (),
                }
            }
            Ok(())
        }),
        end_render: Box::new(move || {
            r15.cmds.run(OP, &mut |c| {
                c.render_end();
                Ok(())
            })
        }),
    })
}

fn begin_commands(dev: &Rc<Dev>, queue: &Rc<QueueState>) -> Result<DriverCommands, Error> {
    const OP: &str = "Ogpu_vulkan.queue";
    let native = queue.native(OP)?.begin().map_err(|failure| of_vk(OP, failure))?;
    let cmds = Rc::new(Cmds { dev: dev.clone(), queue: queue.clone(), commands: RefCell::new(Some(native)), finished: Cell::new(false) });
    let (c1, c2, c3, c4, c5, c6, c7) =
        (cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone(), cmds.clone());
    Ok(DriverCommands {
        compute_encoder: Box::new(move || compute_encoder(&c1)),
        accel_encoder: Box::new(move || accel_encoder(&c2)),
        blit_encoder: Box::new(move || blit_encoder(&c3)),
        render_encoder: Box::new(move |target| render_encoder(&c4, target)),
        commit: Box::new(move || {
            c5.commit_with(OP, &mut |slot| match slot.take() {
                Some(commands) => commands.commit(),
                None => Err(vk::Error { operation: "commands_commit", kind: vk::ErrorKind::InvalidHandle }),
            })
        }),
        commit_present: Box::new(move |source, frame| {
            let source = c6.dev.find_texture(OP, source)?;
            let surface = c6.dev.frames.borrow().get(&frame.frame_token).cloned();
            let Some(surface) = surface else {
                return Err(fail(OP, ErrorKind::InvalidState, "frame token is stale"));
            };
            let result = c6.commit_with(OP, &mut |slot| {
                let handle_error = || vk::Error { operation: "commit_present", kind: vk::ErrorKind::InvalidHandle };
                let commands = slot.as_mut().ok_or_else(handle_error)?;
                let mut surface = surface.borrow_mut();
                let epoch = surface.as_mut().ok_or_else(handle_error)?.present(commands, &source.texture)?;
                *slot = None;
                Ok(epoch)
            });
            if result.is_ok() {
                c6.dev.frames.borrow_mut().remove(&frame.frame_token);
            }
            result
        }),
        abandon: Box::new(move || {
            if c7.finished.get() {
                Err(fail(OP, ErrorKind::InvalidState, "commands are finished"))
            } else {
                c7.finished.set(true);
                *c7.commands.borrow_mut() = None;
                Ok(())
            }
        }),
    })
}

fn create_queue(dev: &Rc<Dev>) -> Result<DriverQueue, Error> {
    const OP: &str = "Ogpu_vulkan.queue";
    let device = dev.device(OP)?;
    let native = device.queue().map_err(|failure| of_vk(OP, failure))?;
    live_add(1);
    dev.queues.set(dev.queues.get() + 1);
    let state = Rc::new(QueueState { queue: RefCell::new(Some(native)), next_epoch: Cell::new(1), completed: Cell::new(0) });
    let (s1, s2, s3, s4, s5, s6, s7) =
        (state.clone(), state.clone(), state.clone(), state.clone(), state.clone(), state.clone(), state.clone());
    let (begin_dev, destroy_dev) = (dev.clone(), dev.clone());
    Ok(DriverQueue {
        complete_through: Box::new(move |epoch| {
            let epoch = s1.bound_epoch(OP, epoch)?;
            if epoch <= s1.completed.get() {
                return Ok(());
            }
            s1.native(OP)?.wait(epoch as u64).map_err(|failure| of_vk(OP, failure))?;
            s1.completed.set(epoch);
            Ok(())
        }),
        poll_through: Box::new(move |epoch| {
            let epoch = s2.bound_epoch(OP, epoch)?;
            if epoch <= s2.completed.get() {
                return Ok(true);
            }
            let queue = s2.native(OP)?;
            let done = queue.completed().map_err(|failure| of_vk(OP, failure))? as i64;
            if done >= epoch {
                queue.poll();
                s2.completed.set(epoch);
                Ok(true)
            } else {
                Ok(false)
            }
        }),
        completed_epoch: Box::new(move || s3.completed.get()),
        begin_commands: Box::new(move || begin_commands(&begin_dev, &s4)),
        gpu_duration: Box::new(move |epoch| s5.native(OP).ok().and_then(|queue| queue.duration(epoch as u64))),
        gpu_timing: Box::new(move || GpuTiming { gpu_seconds: s6.native(OP).map_or(0., |queue| queue.total_seconds()) }),
        destroy_queue: Box::new(move || {
            let completed = s7.native(OP)?.completed().map_or(-1, |done| done as i64);
            if s7.active() > 0 && completed < s7.next_epoch.get() - 1 {
                Err(fail(OP, ErrorKind::InvalidState, "queue has commands in flight"))
            } else {
                destroy_dev.queues.set(destroy_dev.queues.get() - 1);
                live_add(-1);
                *s7.queue.borrow_mut() = None;
                Ok(())
            }
        }),
    })
}

fn create_surface(dev: &Rc<Dev>, configuration: &Configuration) -> Result<DriverSurface, Error> {
    const OP: &str = "Ogpu_vulkan.surface";
    let immediate = |configuration: &Configuration| configuration.present_mode == PresentMode::Immediate;
    let Some(layer) = &configuration.layer else {
        return Err(fail(OP, ErrorKind::Unsupported, "surface needs a native layer token"));
    };
    let device = dev.device(OP)?;
    let native = device.surface(layer, layer.owner_id(), layer.generation(), immediate(configuration)).map_err(|failure| of_vk(OP, failure))?;
    live_add(1);
    let cell = Rc::new(RefCell::new(Some(native)));
    let (configure_cell, acquire_cell, discard_cell, destroy_cell) = (cell.clone(), cell.clone(), cell.clone(), cell);
    let (acquire_dev, discard_dev, destroy_dev) = (dev.clone(), dev.clone(), dev.clone());
    let stale = || error::make(OP, ErrorKind::StaleHandle, "surface is destroyed");
    Ok(DriverSurface {
        configure: Box::new(move |configuration| match configure_cell.borrow_mut().as_mut() {
            Some(surface) => surface.configure(immediate(configuration)).map_err(|failure| of_vk(OP, failure)),
            None => Err(stale()),
        }),
        acquire_sync: Box::new(move || {
            let acquired = match acquire_cell.borrow_mut().as_mut() {
                Some(surface) => surface.acquire(),
                None => return Err(stale()),
            };
            match acquired {
                Ok(vk::Acquire::Image(_)) => {
                    let frame_token = acquire_dev.token();
                    acquire_dev.frames.borrow_mut().insert(frame_token, acquire_cell.clone());
                    Ok(DriverAcquire::Acquired(DriverFrame { frame_token }))
                }
                Ok(vk::Acquire::TimedOut) => Ok(DriverAcquire::Timeout),
                Ok(vk::Acquire::Occluded) => Ok(DriverAcquire::Occluded),
                Err(failure) if failure.kind == vk::ErrorKind::DeviceLost => Ok(DriverAcquire::DeviceLost),
                Err(failure) => Err(of_vk(OP, failure)),
            }
        }),
        discard: Box::new(move |frame| {
            if discard_dev.frames.borrow_mut().remove(&frame.frame_token).is_some() {
                if let Some(surface) = discard_cell.borrow_mut().as_mut() {
                    surface.discard();
                }
                Ok(())
            } else {
                Err(fail(OP, ErrorKind::InvalidState, "frame token is stale"))
            }
        }),
        destroy_surface: Box::new(move || {
            destroy_dev.frames.borrow_mut().retain(|_, surface| !Rc::ptr_eq(surface, &destroy_cell));
            live_add(-1);
            *destroy_cell.borrow_mut() = None;
            Ok(())
        }),
    })
}

fn create_device(device_live: &Rc<Cell<bool>>, validation: (bool, bool)) -> Result<DriverDevice, Error> {
    const OP: &str = "Ogpu_vulkan.create_device";
    if device_live.get() {
        return Err(fail(OP, ErrorKind::InvalidState, "adapter already owns a live device"));
    }
    let device = vk::Device::create(validation.0, validation.1).map_err(|failure| of_vk(OP, failure))?;
    device_live.set(true);
    let facts = match device.facts().map_err(|failure| of_vk(OP, failure)) {
        Ok(facts) => facts,
        Err(failure) => {
            device_live.set(false);
            return Err(failure);
        }
    };
    let dev = Rc::new(Dev {
        device: RefCell::new(Some(Rc::new(device))),
        next: Cell::new(1),
        buffers: RefCell::default(),
        textures: RefCell::default(),
        pipelines: RefCell::default(),
        samplers: RefCell::default(),
        icbs: RefCell::default(),
        arguments: RefCell::default(),
        frames: RefCell::default(),
        accels: RefCell::default(),
        accel_resources: RefCell::default(),
        accel_geometries: RefCell::default(),
        queues: Cell::new(0),
        max_invocations: facts.max_invocations as i64,
    });
    let handle_id = handle::create_device();
    let capabilities = Caps {
        limits: Limits {
            max_buffer_size: facts.max_buffer_bytes as i64,
            max_texture_dimension_2d: facts.max_texture_dimension as i64,
            max_bind_groups: 4,
            max_sample_count: facts.max_samples as i64,
        },
        compute_pipeline: true,
        render_pipeline: true,
        ray_tracing: true,
        ray_tracing_curves: false,
        // only after device_create enables rayQuery and accelerationStructure
        ray_tracing_motion: true,
        ray_tracing_refit: true,
        memoryless_attachments: false,
        unified_memory: false,
    };
    let memory = |dev: &Rc<Dev>| dev.device("Ogpu_vulkan.memory").ok().and_then(|device| device.memory().ok());
    let (allocated_dev, working_dev) = (dev.clone(), dev.clone());
    let (d_buffer, d_mapped, d_texture, d_depth, d_library, d_accel, d_sampler, d_render, d_icb, d_queue, d_surface, d_destroy) = (
        dev.clone(), dev.clone(), dev.clone(), dev.clone(), dev.clone(), dev.clone(), dev.clone(), dev.clone(),
        dev.clone(), dev.clone(), dev.clone(), dev.clone(),
    );
    let d_argument = dev.clone();
    let destroy_handle = handle_id.clone();
    let device_live = device_live.clone();
    Ok(DriverDevice {
        device_handle: handle_id,
        capabilities,
        allocated_size: Box::new(move || memory(&allocated_dev).map_or(0, |m| m.live_bytes as i64)),
        recommended_working_set: Box::new(move || memory(&working_dev).map_or(0, |m| m.device_local_budget as i64)),
        create_buffer: Box::new(move |memory, descriptor| create_buffer(&d_buffer, memory, descriptor)),
        create_mapped_buffer: Box::new(move |descriptor| create_mapped_buffer(&d_mapped, descriptor)),
        create_texture: Box::new(move |descriptor| register_texture(&d_texture, descriptor.format, descriptor)),
        create_depth_texture: Box::new(move |descriptor| register_texture(&d_depth, TextureFormat::Depth32Float, descriptor)),
        create_memoryless_texture: Box::new(|_depth, _descriptor| {
            Err(fail("Ogpu_vulkan.create_memoryless_texture", ErrorKind::Unsupported, "this device has no tile memory"))
        }),
        create_library: Box::new(move |shader| create_library(&d_library, shader)),
        create_accel: Box::new(move |descriptor| create_accel(&d_accel, descriptor)),
        // Vulkan stores a 24-bit user id and 8-bit mask together; motion records are the portable
        // 44-byte keyframe layout consumed when swept instance AABBs are built.
        instance_layout: Box::new(|kind| match kind {
            InstanceKind::UserIdInstances => vec![68, 0, -1, 52, 56, 48, -1, -1, -1, -1, -1, -1],
            InstanceKind::MotionInstances => vec![44, -1, -1, 0, 4, 8, 12, 16, 20, 24, 28, 32],
        }),
        create_sampler: Box::new(move |descriptor| create_sampler(&d_sampler, descriptor)),
        create_render_pipeline: Box::new(move |options, descriptor| create_render_pipeline(&d_render, options, descriptor)),
        create_icb: Box::new(move |max_commands| create_icb(&d_icb, max_commands)),
        create_argument: Box::new(move |_pipeline, _stage: ShaderStage, _index| create_argument(&d_argument)),
        create_queue: Box::new(move || create_queue(&d_queue)),
        create_surface: Box::new(move |configuration| create_surface(&d_surface, configuration)),
        destroy_device: Box::new(move || {
            if d_destroy.queues.get() != 0 {
                return Err(fail("Ogpu_vulkan.destroy_device", ErrorKind::InvalidState, "device has active queues"));
            }
            device_live.set(false);
            // Natives leaked past their owners go with the device.
            d_destroy.buffers.borrow_mut().clear();
            d_destroy.textures.borrow_mut().clear();
            d_destroy.pipelines.borrow_mut().clear();
            d_destroy.samplers.borrow_mut().clear();
            d_destroy.icbs.borrow_mut().clear();
            d_destroy.arguments.borrow_mut().clear();
            d_destroy.frames.borrow_mut().clear();
            d_destroy.accels.borrow_mut().clear();
            d_destroy.accel_geometries.borrow_mut().clear();
            d_destroy.accel_resources.borrow_mut().clear();
            *d_destroy.device.borrow_mut() = None;
            handle::destroy_device(&destroy_handle);
            Ok(())
        }),
    })
}

/// The driver: one live device at a time, validation as `RAYS_VULKAN_VALIDATION` says (`fatal`, `on`).
pub fn create() -> Driver {
    create_with_validation(validation_mode())
}

/// The same with validation named: (enabled, fatal).
pub fn create_with_validation(validation: (bool, bool)) -> Driver {
    let device_live = Rc::new(Cell::new(false));
    Driver { create_device: Box::new(move || create_device(&device_live, validation)) }
}
