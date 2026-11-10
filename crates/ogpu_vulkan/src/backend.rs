//! OGPU's driver records over the Vulkan bridge. Tokens name native objects; the checks live in
//! `ogpu_core::backend`, as for Metal.
use ogpu_core::validation::{AdmittedBuffer, AdmittedTexture, TextureRead, TextureReadInto, StorageMode};
use rays_math::float::{min as omin, max as omax};
use ogpu_core::{
    backend::{
        DepthState, Driver, DriverAccel, DriverAccelDescriptor, DriverAccelEncoder, DriverAcquire, DriverArgument,
        DriverBatchDraw, DriverBlitEncoder, DriverCommands, DriverComputeEncoder, DriverDevice, DriverFrame,
        DriverGeometry, DriverIcb, DriverLibrary, DriverMapped, DriverMapping, DriverPipeline, DriverQueue,
        DriverRenderEncoder, DriverRenderTarget, DriverResource, DriverSampler, DriverSurface, Floats,
        GpuTiming, InstanceKind, Receipt, RenderPipelineOptions, ShaderStage, Token, Warm, Winding,
    },
    caps::{Caps, Limits},
    error::{self, Error, ErrorKind},
    handle,
    pipeline::{Blend, ColorFormat, DepthFormat, RenderDescriptor},
    render_pass::{Comparison, Cull, IndexType, Load, Primitive, Rect, Store},
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
use crate::vulkan as vk;

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
pub(crate) fn of_vk(op: &str, failure: vk::Error) -> Error {
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
    descriptor: AdmittedTexture,
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
        Primitive::LineStrip => 4,
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
    accel_resources: RefCell<HashMap<i64, Vec<Box<dyn DriverResource>>>>,
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

struct BufferResource {
    dev: Rc<Dev>,
    id: Token,
    op: &'static str,
}

impl DriverResource for BufferResource {
    fn token(&self) -> Token {
        self.id
    }
    fn write(&self, offset: i64, bytes: &[u8]) -> Result<(), Error> {
        let entry = self.dev.find_buffer(self.op, self.id)?;
        if !in_range(entry.size, offset, bytes.len()) {
            return Err(fail(self.op, ErrorKind::InvalidArgument, "write range is invalid"));
        }
        entry.buffer.borrow_mut().write(offset as u64, bytes).map_err(|failure| of_vk(self.op, failure))
    }
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, Error> {
        let entry = self.dev.find_buffer(self.op, self.id)?;
        let mut bytes = vec![0u8; length];
        buffer_read_into(self.op, &entry, offset, &mut bytes, 0, length)?;
        Ok(bytes)
    }
    fn read_into(&self, offset: i64, destination: &mut [u8], at: usize, length: usize) -> Result<(), Error> {
        let entry = self.dev.find_buffer(self.op, self.id)?;
        buffer_read_into(self.op, &entry, offset, destination, at, length)
    }
    fn destroy(&self) -> Result<(), Error> {
        self.dev.buffers.borrow_mut().remove(&self.id);
        live_add(-1);
        Ok(())
    }
}

fn register_buffer(dev: &Rc<Dev>, op: &'static str, created: Result<vk::Buffer, vk::Error>, size: i64) -> Result<BufferResource, Error> {
    let native = created.map_err(|failure| of_vk(op, failure))?;
    live_add(1);
    let id = dev.token();
    dev.buffers.borrow_mut().insert(id, Rc::new(BufferEntry { buffer: RefCell::new(native), size }));
    Ok(BufferResource { dev: dev.clone(), id, op })
}

/// Both kinds are device-local: a Shared buffer in host memory is read by kernels over PCIe, 100x
/// slower than VRAM (isogrid_city's triangle kernel: 160 ms to 4 ms). Host writes stage into the next
/// submit and reads go through the transfer queue; Shared reads as zeros, so it is filled like a mapped
/// buffer is. ponytail: a host-visible tier for a buffer the host rewrites every frame and the GPU reads
/// once, if a measurement ever shows the staging copy.
fn create_buffer(dev: &Rc<Dev>, memory: Memory, descriptor: &BufferDescriptor) -> Result<Box<dyn DriverResource>, Error> {
    const OP: &str = "Ogpu_vulkan.create_buffer";
    let device = dev.device(OP)?;
    let resource = register_buffer(dev, OP, device.buffer(descriptor.size as u64, vk::BufferKind::DeviceLocal), descriptor.size)?;
    if memory == Memory::Shared {
        let zeroed = dev.find_buffer(OP, resource.id).and_then(|entry| entry.buffer.borrow().zero().map_err(|failure| of_vk(OP, failure)));
        if let Err(failure) = zeroed {
            let _ = resource.destroy();
            return Err(failure);
        }
    }
    Ok(Box::new(resource))
}

const MAPPED_OP: &str = "Ogpu_vulkan.create_mapped_buffer";

struct Mapping {
    readback: ogpu_core::backend::OwnedReadback,
}

impl DriverMapping for Mapping {
    fn readback(&self) -> ogpu_core::backend::OwnedReadback {
        self.readback.clone()
    }
}

fn create_mapped_buffer(dev: &Rc<Dev>, descriptor: &BufferDescriptor) -> Result<DriverMapped, Error> {
    let device = dev.device(MAPPED_OP)?;
    let size = descriptor.size;
    let resource = register_buffer(dev, MAPPED_OP, device.buffer(size as u64, vk::BufferKind::DeviceLocal), size)?;
    let id = resource.id;
    if let Err(failure) = dev.find_buffer(MAPPED_OP, id).and_then(|entry| entry.buffer.borrow().zero().map_err(|failure| of_vk(MAPPED_OP, failure))) {
        let _ = resource.destroy();
        return Err(failure);
    }
  // Native staged reads serialize under the device qlock; readback retains the allocation only.
    let readback_buffer = dev.find_buffer(MAPPED_OP, id)?.buffer.borrow().clone();
    let readback: ogpu_core::backend::OwnedReadback = std::sync::Arc::new(move |offset, length| {
        let offset = u64::try_from(offset).map_err(|_| fail(MAPPED_OP, ErrorKind::InvalidArgument, "negative read offset"))?;
        if offset > readback_buffer.size() || length as u64 > readback_buffer.size() - offset {
            return Err(fail(MAPPED_OP, ErrorKind::InvalidArgument, "range exceeds the buffer"));
        }
        let mut bytes = vec![0; length];
        readback_buffer.read(offset, &mut bytes).map_err(|failure| of_vk(MAPPED_OP, failure))?;
        Ok(bytes)
    });
    Ok(DriverMapped {
        mapped_resource: Box::new(resource),
        mapping: Box::new(Mapping { readback }),
    })
}

const TEXTURE_OP: &str = "Ogpu_vulkan.create_texture";

struct TextureResource {
    dev: Rc<Dev>,
    id: Token,
    profile: AdmittedTexture,
}

impl DriverResource for TextureResource {
    fn token(&self) -> Token {
        self.id
    }
    fn write(&self, _offset: i64, _bytes: &[u8]) -> Result<(), Error> {
        Err(fail(TEXTURE_OP, ErrorKind::Unsupported, "use a transfer pass for textures"))
    }
    fn read(&self, _: i64, _: usize) -> Result<Vec<u8>, Error> {
        Err(fail(TEXTURE_OP, ErrorKind::InvalidArgument, "texture reads require an admitted layout"))
    }
    fn read_into(&self, _: i64, _: &mut [u8], _: usize, _: usize) -> Result<(), Error> {
        Err(fail(TEXTURE_OP, ErrorKind::InvalidArgument, "texture reads require an admitted layout"))
    }
    fn read_texture(&self, read: &TextureRead) -> Result<Vec<u8>, Error> {
        let mut bytes = vec![0; read.length()];
        let entry = self.dev.find_texture(TEXTURE_OP, self.id)?;
        if !read.belongs_to(&self.profile) { return Err(fail(TEXTURE_OP, ErrorKind::InvalidArgument, "transfer belongs to another admitted texture")); }
        entry.texture.read(&mut bytes, read.bytes_per_row() as u64, types::texel_bytes(self.profile.format) as u32)
            .map_err(|failure| of_vk(TEXTURE_OP, failure))?;
        Ok(bytes)
    }
    fn read_texture_into(&self, read: &mut TextureReadInto<'_>) -> Result<(), Error> {
        let (read, destination) = read.parts();
        if !read.belongs_to(&self.profile) {
            return Err(fail(TEXTURE_OP, ErrorKind::InvalidArgument, "transfer belongs to another admitted texture"));
        }
        let entry = self.dev.find_texture(TEXTURE_OP, self.id)?;
        entry.texture.read(destination, read.bytes_per_row() as u64, types::texel_bytes(self.profile.format) as u32)
            .map_err(|failure| of_vk(TEXTURE_OP, failure))
    }
    fn destroy(&self) -> Result<(), Error> {
        self.dev.textures.borrow_mut().remove(&self.id);
        live_add(-1);
        Ok(())
    }
}

fn register_texture(dev: &Rc<Dev>, descriptor: &AdmittedTexture) -> Result<Box<dyn DriverResource>, Error> {
    let device = dev.device(TEXTURE_OP)?;
    let created = device.texture(vk::TextureDescriptor {
        width: descriptor.width as u32,
        height: descriptor.height as u32,
        depth: descriptor.depth.max(1) as u32,
        mip_levels: descriptor.mip_levels.max(1) as u32,
        samples: descriptor.sample_count.max(1) as u32,
        format: format_index(descriptor.format),
        storage: descriptor.usage.contains(&TextureUsage::StorageBinding),
        attachment: descriptor.usage.contains(&TextureUsage::RenderAttachment),
    });
    let native = created.map_err(|failure| of_vk(TEXTURE_OP, failure))?;
    live_add(1);
    let id = dev.token();
    dev.textures.borrow_mut().insert(id, Rc::new(TextureEntry { texture: native, descriptor: descriptor.clone() }));
    Ok(Box::new(TextureResource { dev: dev.clone(), id, profile: descriptor.clone() }))
}

struct OwnedPipeline {
    dev: Rc<Dev>,
    id: Token,
    max_threads: i64,
}

impl DriverPipeline for OwnedPipeline {
    fn pipeline_token(&self) -> Token {
        self.id
    }
    fn max_threads(&self) -> i64 {
        self.max_threads
    }
    fn destroy_pipeline(&self) -> Result<(), Error> {
        self.dev.pipelines.borrow_mut().remove(&self.id);
        live_add(-1);
        Ok(())
    }
}

fn own_pipeline(dev: &Rc<Dev>, pipeline: vk::Pipeline, local: (i64, i64, i64), max_threads: i64) -> Box<dyn DriverPipeline> {
    live_add(1);
    let id = dev.token();
    dev.pipelines.borrow_mut().insert(id, Rc::new(PipelineEntry { pipeline, local }));
    Box::new(OwnedPipeline { dev: dev.clone(), id, max_threads })
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

const LIBRARY_OP: &str = "Ogpu_vulkan.create_library";

struct ShaderLibrary {
    dev: Rc<Dev>,
    bytes: Vec<u8>,
    module: RefCell<Option<vk::ShaderModule>>,
}

impl DriverLibrary for ShaderLibrary {
    fn create_compute_pipeline_in(&self, function: &ogpu_core::shader::AdmittedFunction, declared: &[ShaderBinding]) -> Result<Box<dyn DriverPipeline>, Error> {
        let entry = function.name();
        let constants = function.constants();
        if !constants.is_empty() {
            return Err(fail(LIBRARY_OP, ErrorKind::Unsupported, "SPIR-V kernels take no named function constants"));
        }
        let reflected = match crate::reflect::reflect_entry(&self.bytes, entry) {
            Ok(Some(reflected)) => reflected,
            Ok(None) | Err(_) => return Err(fail(LIBRARY_OP, ErrorKind::InvalidArgument, "the module has no such entry point")),
        };
        let mut want: Vec<(u32, u32)> = declared.iter().map(|b| (b.binding as u32, kernel_space(b.kind))).collect();
        let mut have: Vec<(u32, u32)> = reflected.bindings.iter().map(|&(b, k)| (b, reflected_space(k))).collect();
        want.sort_unstable();
        have.sort_unstable();
        if want != have {
            return Err(fail(LIBRARY_OP, ErrorKind::InvalidArgument, "SPIR-V reflection does not match the interface"));
        }
        let device = self.dev.device(LIBRARY_OP)?;
        let natives: Vec<vk::Binding> =
            reflected.bindings.iter().map(|&(binding, kind)| vk::Binding { binding, kind: kind as i32 }).collect();
        let module = self.module.borrow();
        let Some(module) = module.as_ref() else {
            return Err(fail(LIBRARY_OP, ErrorKind::StaleHandle, "library is destroyed"));
        };
        let pipeline = vk::Pipeline::compute(&device, module, entry, &natives).map_err(|failure| of_vk(LIBRARY_OP, failure))?;
        let local = (reflected.local_size.0 as i64, reflected.local_size.1 as i64, reflected.local_size.2 as i64);
        Ok(own_pipeline(&self.dev, pipeline, local, self.dev.max_invocations))
    }
    fn destroy_library(&self) -> Result<(), Error> {
        *self.module.borrow_mut() = None;
        live_add(-1);
        Ok(())
    }
}

fn create_library(dev: &Rc<Dev>, shader: &Shader) -> Result<Box<dyn DriverLibrary>, Error> {
    let bytes = shader::bytes(shader).to_vec();
    let device = dev.device(LIBRARY_OP)?;
    let module = device.shader(&bytes).map_err(|failure| of_vk(LIBRARY_OP, failure))?;
    live_add(1);
    Ok(Box::new(ShaderLibrary { dev: dev.clone(), bytes, module: RefCell::new(Some(module)) }))
}

fn create_render_pipeline(dev: &Rc<Dev>, options: RenderPipelineOptions, descriptor: &RenderDescriptor) -> Result<Box<dyn DriverPipeline>, Error> {
    const OP: &str = "Ogpu_vulkan.create_render_pipeline";
    let device = dev.device(OP)?;
    let vertex_bytes = shader::bytes(&descriptor.vertex);
    let vertex = device.shader(vertex_bytes).map_err(|failure| of_vk(OP, failure))?;
    let fragment = match &descriptor.fragment {
        None => None,
        Some(shader) => Some(device.shader(shader::bytes(shader)).map_err(|failure| of_vk(OP, failure))?),
    };
    let reflect = |bytes: &[u8]| -> Result<Vec<(u32, u32)>, Error> {
        crate::reflect::reflect(bytes).or_else(|_| Err(fail(OP, ErrorKind::InvalidArgument, "invalid SPIR-V module")))
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
        Primitive::LineList | Primitive::LineStrip => 1,
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

fn create_sampler(dev: &Rc<Dev>, descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn DriverSampler>, Error> {
    const OP: &str = "Ogpu_vulkan.create_sampler";
    if !descriptor.normalized_coordinates || descriptor.address_r != AddressMode::ClampToEdge
        || descriptor.lod_average || descriptor.lod_bias != 0. || descriptor.compare_function != Comparison::Never
        || descriptor.reduction_mode != ogpu_core::validation::ReductionMode::WeightedAverage
        || descriptor.border_color != ogpu_core::validation::BorderColor::TransparentBlack {
        return Err(fail(OP, ErrorKind::Unsupported, "advanced sampler profile is unavailable in Vulkan"));
    }

    let filter = |filter: SamplerFilter| match filter {
        SamplerFilter::Nearest => 0,
        SamplerFilter::Linear => 1,
    };
    let address = |mode: AddressMode| match mode {
        AddressMode::ClampToEdge => Ok(0), AddressMode::Repeat => Ok(1), AddressMode::MirrorRepeat => Ok(2),
        _ => Err(fail(OP, ErrorKind::Unsupported, "advanced sampler addressing is unavailable in Vulkan")),
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
            address_u: address(descriptor.address_s)?,
            address_v: address(descriptor.address_t)?,
            anisotropy: descriptor.max_anisotropy as i32,
            lod_min: descriptor.lod_min,
            lod_max: descriptor.lod_max,
        })
    .map_err(|failure| of_vk(OP, failure))?;
    live_add(1);
    let id = dev.token();
    dev.samplers.borrow_mut().insert(id, Rc::new(native));
    Ok(Box::new(OwnedSampler { dev: dev.clone(), id }))
}

struct OwnedSampler {
    dev: Rc<Dev>,
    id: Token,
}

impl DriverSampler for OwnedSampler {
    fn sampler_token(&self) -> Token {
        self.id
    }
    fn destroy_sampler(&self) -> Result<(), Error> {
        self.dev.samplers.borrow_mut().remove(&self.id);
        live_add(-1);
        Ok(())
    }
}

const ICB_OP: &str = "Ogpu_vulkan.icb";

struct IndirectCommands {
    dev: Rc<Dev>,
    id: Token,
    commands: Rc<RefCell<Vec<IcbCommand>>>,
}

impl IndirectCommands {
    fn at(&self, index: ogpu_core::validation::AdmittedIcbIndex, edit: &mut dyn FnMut(&mut IcbCommand)) -> Result<(), Error> {
        let mut commands = self.commands.borrow_mut();
        if !index.belongs_to_capacity(commands.len()) {
            return Err(fail(ICB_OP, ErrorKind::InvalidArgument, "index was admitted for another indirect command capacity"));
        }
        edit(&mut commands[index.index()]);
        Ok(())
    }
}

impl DriverIcb for IndirectCommands {
    fn icb_token(&self) -> Token {
        self.id
    }
    fn icb_set_pipeline(&self, index: ogpu_core::validation::AdmittedIcbIndex, pipeline: Token) -> Result<(), Error> {
        self.at(index, &mut |c| c.pipeline = Some(pipeline))
    }
    fn icb_set_buffer(&self, index: ogpu_core::validation::AdmittedIcbIndex, _stage: ShaderStage, slot: i64, offset: i64, buffer: Token) -> Result<(), Error> {
        self.at(index, &mut |c| c.buffers.insert(0, (slot, offset, buffer)))
    }
    fn icb_draw(&self, index: ogpu_core::validation::AdmittedIcbIndex, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.at(index, &mut |c| c.draw = Some(IcbDraw::Draw { primitive, first, count, instances }))
    }
    fn icb_draw_indexed(&self, index: ogpu_core::validation::AdmittedIcbIndex, primitive: Primitive, index_type: IndexType, buffer: Token, offset: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.at(index, &mut |c| c.draw = Some(IcbDraw::Indexed { primitive, index_type, buffer, offset, count, instances }))
    }
    fn destroy_icb(&self) -> Result<(), Error> {
        self.dev.icbs.borrow_mut().remove(&self.id);
        live_add(-1);
        Ok(())
    }
}

fn create_icb(dev: &Rc<Dev>, profile: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn DriverIcb>, Error> {
    let id = dev.token();
    live_add(1);
    let commands = Rc::new(RefCell::new(vec![IcbCommand::default(); profile.capacity()]));
    dev.icbs.borrow_mut().insert(id, commands.clone());
    Ok(Box::new(IndirectCommands { dev: dev.clone(), id, commands }))
}

const ARGUMENT_OP: &str = "Ogpu_vulkan.argument";

struct ArgumentSlices {
    dev: Rc<Dev>,
}

impl DriverArgument for ArgumentSlices {
    fn argument_length(&self) -> i64 {
        16
    }
    fn argument_alignment(&self) -> i64 {
        16
    }
    fn argument_texture(&self, buffer: Token, offset: i64, _slot: i64, texture: Token) -> Result<(), Error> {
        let texture = self.dev.find_texture(ARGUMENT_OP, texture)?;
        self.dev.arguments.borrow_mut().entry((buffer, offset)).or_default().0 = Some(texture);
        Ok(())
    }
    fn argument_sampler(&self, buffer: Token, offset: i64, _slot: i64, sampler: Token) -> Result<(), Error> {
        let sampler = self.dev.find_sampler(ARGUMENT_OP, sampler)?;
        self.dev.arguments.borrow_mut().entry((buffer, offset)).or_default().1 = Some(sampler);
        Ok(())
    }
    fn destroy_argument(&self) -> Result<(), Error> {
        Ok(())
    }
}

fn create_argument(dev: &Rc<Dev>) -> Result<Box<dyn DriverArgument>, Error> {
    Ok(Box::new(ArgumentSlices { dev: dev.clone() }))
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

fn generated_buffer(dev: &Rc<Dev>, generated: &mut Vec<Box<dyn DriverResource>>, bytes: &[u8]) -> Result<Token, Error> {
    let resource = create_buffer(
        dev,
        Memory::DeviceLocal,
        &BufferDescriptor {
            label: Some("Vulkan ray bounds".to_owned()),
            size: bytes.len() as i64,
            usage: vec![BufferUsage::Storage, BufferUsage::CopyDst],
        },
    )?;
    match resource.write(0, bytes) {
        Ok(()) => {
            let token = resource.token();
            generated.push(resource);
            Ok(token)
        }
        Err(failure) => {
            let _ = resource.destroy();
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
    generated: &mut Vec<Box<dyn DriverResource>>,
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

fn geometry_records(dev: &Rc<Dev>, generated: &mut Vec<Box<dyn DriverResource>>, geometries: &[DriverGeometry]) -> Result<Vec<GeomRecord>, Error> {
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
    generated: &mut Vec<Box<dyn DriverResource>>,
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

fn destroy_all(resources: Vec<Box<dyn DriverResource>>) {
    for resource in resources {
        let _ = resource.destroy();
    }
}

fn build_accel(
    dev: &Rc<Dev>,
    op: &str,
    descriptor: &DriverAccelDescriptor,
    generated: &mut Vec<Box<dyn DriverResource>>,
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
            device.acceleration_sized(*size, &template).map_err(|failure| of_vk(op, failure))
        }
    }
}

struct OwnedAccel {
    dev: Rc<Dev>,
    id: Token,
    sizes: ogpu_core::backend::AccelSizes,
}

impl DriverAccel for OwnedAccel {
    fn accel_token(&self) -> Token {
        self.id
    }
    fn accel_sizes(&self) -> ogpu_core::backend::AccelSizes {
        self.sizes
    }
    fn destroy_accel(&self) -> Result<(), Error> {
        self.dev.accels.borrow_mut().remove(&self.id);
        self.dev.accel_geometries.borrow_mut().remove(&self.id);
        let resources = self.dev.accel_resources.borrow_mut().remove(&self.id).unwrap_or_default();
        destroy_all(resources);
        live_add(-1);
        Ok(())
    }
}

fn create_accel(dev: &Rc<Dev>, descriptor: &DriverAccelDescriptor) -> Result<Box<dyn DriverAccel>, Error> {
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
    Ok(Box::new(OwnedAccel {
        dev: dev.clone(),
        id,
        sizes: ogpu_core::backend::AccelSizes {
            structure_size: sizes.structure_bytes as i64,
            build_scratch_size: sizes.build_scratch_bytes as i64,
            refit_scratch_size: sizes.update_scratch_bytes as i64,
        },
    }))
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
}

impl Cmds {
    fn run(&self, op: &str, call: &mut dyn FnMut(&mut vk::Commands) -> Result<(), vk::Error>) -> Result<(), Error> {
        match self.commands.borrow_mut().as_mut() {
            Some(commands) => call(commands).map_err(|failure| of_vk(op, failure)),
            None => Err(fail(op, ErrorKind::InvalidState, "commands are finished")),
        }
    }

    fn commit_with(&self, op: &str, submit: &mut dyn FnMut(&mut Option<vk::Commands>) -> Result<u64, vk::Error>) -> Result<Receipt, Error> {
        if self.queue.active() >= 3 {
            Err(fail(op, ErrorKind::Capacity, "frames-in-flight capacity reached"))
        } else {
            let epoch = submit(&mut self.commands.borrow_mut()).map_err(|failure| of_vk(op, failure))? as i64;
            self.queue.next_epoch.set(epoch + 1);
            Ok(Receipt { epoch })
        }
    }
}

const QUEUE_OP: &str = "Ogpu_vulkan.queue";

/// A kernel runs in groups of its own local size: the threadgroup a caller names is Metal's, and
/// SPIR-V cannot change it.
struct ComputePass {
    cmds: Rc<Cmds>,
    local: Cell<(i64, i64, i64)>,
}

impl DriverComputeEncoder for ComputePass {
    fn set_pipeline(&self, id: Token) -> Result<(), Error> {
        let entry = self.cmds.dev.find_pipeline(QUEUE_OP, id)?;
        self.local.set(entry.local);
        self.cmds.run(QUEUE_OP, &mut |c| c.set_pipeline(&entry.pipeline))
    }
    fn set_buffer(&self, index: i64, offset: i64, id: Token) -> Result<(), Error> {
        let entry = self.cmds.dev.find_buffer(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.set_buffer(index as u32, offset as u64, &entry.buffer.borrow()))
    }
    fn set_bytes(&self, index: i64, bytes: ogpu_core::validation::AdmittedBytes<'_>) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| c.set_bytes(index as u32, bytes.bytes()))
    }
    fn set_texture(&self, index: i64, id: Token) -> Result<(), Error> {
        let entry = self.cmds.dev.find_texture(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.set_texture(index as u32, &entry.texture))
    }
    fn set_accel(&self, index: i64, id: Token) -> Result<(), Error> {
        let accel = self.cmds.dev.find_accel(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.set_acceleration(index as u32, &accel))
    }
    fn dispatch_threads(&self, (x, y, z): (i64, i64, i64), _threadgroup: (i64, i64, i64)) -> Result<(), Error> {
        let (gx, gy, gz) = self.local.get();
        let groups = |threads: i64, group: i64| ((threads + group - 1) / group).max(0) as u32;
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.dispatch(groups(x, gx), groups(y, gy), groups(z, gz));
            Ok(())
        })
    }
    fn dispatch_indirect(&self, id: Token, offset: i64, _threadgroup: (i64, i64, i64)) -> Result<(), Error> {
        let entry = self.cmds.dev.find_buffer(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.dispatch_indirect(&entry.buffer.borrow(), offset as u64))
    }
    fn compute_use_accels(&self, _accels: &[Token]) -> Result<(), Error> {
        Ok(())
    }
    fn end_compute(&self) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.encoder_end();
            Ok(())
        })
    }
}

fn compute_encoder(cmds: &Rc<Cmds>) -> Result<Box<dyn DriverComputeEncoder>, Error> {
    cmds.run(QUEUE_OP, &mut |c| {
        c.compute_begin();
        Ok(())
    })?;
    Ok(Box::new(ComputePass { cmds: cmds.clone(), local: Cell::new((1, 1, 1)) }))
}

struct AccelPass {
    cmds: Rc<Cmds>,
}

impl DriverAccelEncoder for AccelPass {
    fn build(&self, id: Token, scratch: Token, scratch_offset: i64) -> Result<(), Error> {
        let accel = self.cmds.dev.find_accel(QUEUE_OP, id)?;
        let scratch = self.cmds.dev.find_buffer(QUEUE_OP, scratch)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.build_acceleration(&accel, &scratch.buffer.borrow(), scratch_offset as u64))
    }
    fn compact(&self, src: Token, dst: Token) -> Result<(), Error> {
        let source = self.cmds.dev.find_accel(QUEUE_OP, src)?;
        let destination = self.cmds.dev.find_accel(QUEUE_OP, dst)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.compact_acceleration(&source, &destination))?;
        // Motion proxies need the prototype's vertices even after its original BLAS is released.
        let records = self.cmds.dev.accel_geometries.borrow().get(&src).cloned();
        let mut geometries = self.cmds.dev.accel_geometries.borrow_mut();
        if let Some(records) = records {
            geometries.insert(dst, records);
        } else {
            geometries.remove(&dst);
        }
        Ok(())
    }
    fn write_compacted_size(&self, id: Token, dst: Token, offset: i64) -> Result<(), Error> {
        let accel = self.cmds.dev.find_accel(QUEUE_OP, id)?;
        let buffer = self.cmds.dev.find_buffer(QUEUE_OP, dst)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.write_compacted_size(&accel, &buffer.buffer.borrow(), offset as u64))
    }
    fn refit(&self, id: Token, descriptor: &DriverAccelDescriptor, scratch: Token, scratch_offset: i64) -> Result<(), Error> {
        let accel = self.cmds.dev.find_accel(QUEUE_OP, id)?;
        let scratch = self.cmds.dev.find_buffer(QUEUE_OP, scratch)?;
        let DriverAccelDescriptor::Blas { geometries, .. } = descriptor else {
            return Err(fail(QUEUE_OP, ErrorKind::InvalidArgument, "Vulkan refit requires a BLAS descriptor"));
        };
        let mut generated = Vec::new();
        let records = match geometry_records(&self.cmds.dev, &mut generated, geometries) {
            Ok(records) => records,
            Err(failure) => {
                destroy_all(generated);
                return Err(failure);
            }
        };
        let refit = self.cmds.run(QUEUE_OP, &mut |c| {
            c.refit_acceleration(&accel, &native_geometries(&records), &scratch.buffer.borrow(), scratch_offset as u64)
        });
        if let Err(failure) = refit {
            destroy_all(generated);
            return Err(failure);
        }
        let old = self.cmds.dev.accel_resources.borrow_mut().insert(id, generated).unwrap_or_default();
        destroy_all(old);
        self.cmds.dev.accel_geometries.borrow_mut().insert(id, records);
        Ok(())
    }
    fn end_accel(&self) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.encoder_end();
            Ok(())
        })
    }
}

fn accel_encoder(cmds: &Rc<Cmds>) -> Result<Box<dyn DriverAccelEncoder>, Error> {
    Ok(Box::new(AccelPass { cmds: cmds.clone() }))
}

struct BlitPass {
    cmds: Rc<Cmds>,
}

impl DriverBlitEncoder for BlitPass {
    fn buffer_to_texture(
        &self,
        src: Token,
        offset: i64,
        bytes_per_row: i64,
        _bytes_per_image: i64,
        dst: Token,
        mip: i64,
        origin: types::Origin,
        extent: types::Extent,
    ) -> Result<(), Error> {
        let buffer = self.cmds.dev.find_buffer(QUEUE_OP, src)?;
        let texture = self.cmds.dev.find_texture(QUEUE_OP, dst)?;
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
        self.cmds.run(QUEUE_OP, &mut |c| c.blit_buffer_to_texture(&buffer.buffer.borrow(), &texture.texture, args))
    }
    fn end_blit(&self) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.encoder_end();
            Ok(())
        })
    }
}

fn blit_encoder(cmds: &Rc<Cmds>) -> Result<Box<dyn DriverBlitEncoder>, Error> {
    Ok(Box::new(BlitPass { cmds: cmds.clone() }))
}

/// A render pass's recording state; a draw batch and an indirect replay re-enter its own methods.
struct Render {
    cmds: Rc<Cmds>,
}

impl DriverRenderEncoder for Render {
    fn render_set_pipeline(&self, id: Token) -> Result<(), Error> {
        let entry = self.cmds.dev.find_pipeline(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.set_pipeline(&entry.pipeline))
    }
    fn set_stage_buffer(&self, _stage: ShaderStage, index: i64, offset: i64, id: Token) -> Result<(), Error> {
        let argument = self.cmds.dev.arguments.borrow().get(&(id, offset)).cloned();
        match argument {
            Some((texture, sampler)) => {
                let (Some(texture), Some(sampler)) = (texture, sampler) else {
                    return Err(fail(QUEUE_OP, ErrorKind::InvalidArgument, "argument slice is incomplete"));
                };
                self.cmds.run(QUEUE_OP, &mut |c| c.set_texture(index as u32, &texture.texture))?;
                self.cmds.run(QUEUE_OP, &mut |c| c.set_sampler(index as u32 + 1, &sampler))
            }
            None => {
                let entry = self.cmds.dev.find_buffer(QUEUE_OP, id)?;
                self.cmds.run(QUEUE_OP, &mut |c| c.set_buffer(index as u32, offset as u64, &entry.buffer.borrow()))
            }
        }
    }
    fn set_stage_texture(&self, _stage: ShaderStage, index: i64, id: Token) -> Result<(), Error> {
        let entry = self.cmds.dev.find_texture(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.set_texture(index as u32, &entry.texture))
    }
    fn set_stage_sampler(&self, _stage: ShaderStage, index: i64, id: Token) -> Result<(), Error> {
        let sampler = self.cmds.dev.find_sampler(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| c.set_sampler(index as u32, &sampler))
    }
    fn set_viewport(&self, rect: ogpu_core::render_pass::AdmittedViewport) -> Result<(), Error> {
        let rect=rect.points().ok_or_else(||fail(QUEUE_OP,ErrorKind::Unsupported,"fractional native viewport is unavailable in Vulkan"))?;
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.viewport(vk::Rect { x: rect.x as i32, y: rect.y as i32, width: rect.width as i32, height: rect.height as i32 });
            Ok(())
        })
    }
    fn set_scissor(&self, rect: Rect) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.scissor(vk::Rect { x: rect.x as i32, y: rect.y as i32, width: rect.width as i32, height: rect.height as i32 })
        })
    }
    fn set_cull(&self, cull: Cull) -> Result<(), Error> {
        let mode = match cull {
            Cull::CullNone => 0,
            Cull::CullFront => 1,
            Cull::CullBack => 2,
        };
        self.cmds.run(QUEUE_OP, &mut |c| c.cull(mode))
    }
    fn set_winding(&self, winding: Winding) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.winding(winding == Winding::Clockwise);
            Ok(())
        })
    }
    fn set_depth_state(&self, state: Option<DepthState>) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| match state {
            None => c.depth(-1, false),
            Some(state) => c.depth(compare_index(state.depth_compare), state.depth_write),
        })
    }
    fn draw(&self, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| c.draw(primitive_index(primitive), first as u32, count as u32, instances as u32))
    }
    fn draw_indexed(&self, primitive: Primitive, index_type: IndexType, id: Token, offset: i64, count: i64, instances: i64) -> Result<(), Error> {
        let entry = self.cmds.dev.find_buffer(QUEUE_OP, id)?;
        self.cmds.run(QUEUE_OP, &mut |c| {
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
    fn draw_indexed_indirect(
        &self,
        primitive: Primitive,
        index_type: IndexType,
        id: Token,
        offset: i64,
        arguments_buffer: Token,
        arguments_offset: i64,
    ) -> Result<(), Error> {
        let entry = self.cmds.dev.find_buffer(QUEUE_OP, id)?;
        let arguments = self.cmds.dev.find_buffer(QUEUE_OP, arguments_buffer)?;
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.draw_indexed_indirect(
                &entry.buffer.borrow(),
                &arguments.buffer.borrow(),
                primitive_index(primitive),
                index_kind(index_type),
                offset as u64,
                arguments_offset as u64,
            )
        })
    }
    fn draw_batch(&self, draws: &[DriverBatchDraw]) -> Result<(), Error> {
        for draw in draws {
            self.render_set_pipeline(draw.batch_pipeline)?;
            for &(stage, index, buffer, offset) in &draw.batch_buffers {
                self.set_stage_buffer(stage, index, offset, buffer)?;
            }
            match draw.batch_index {
                Some((index_type, buffer, offset, count)) => {
                    self.draw_indexed(draw.batch_primitive, index_type, buffer, offset, count, draw.batch_instances)?
                }
                None => self.draw(draw.batch_primitive, draw.batch_vertex_start, draw.batch_vertex_count, draw.batch_instances)?,
            }
        }
        Ok(())
    }
    fn use_resources(&self, _resources: &[Token]) -> Result<(), Error> {
        Ok(())
    }
    fn execute_icb(&self, id: Token, location: i64, length: i64) -> Result<(), Error> {
        let recorded = self.cmds.dev.icbs.borrow().get(&id).cloned();
        let Some(recorded) = recorded else {
            return Err(fail(QUEUE_OP, ErrorKind::InvalidArgument, "indirect commands are unknown"));
        };
        for i in location..location + length {
            let command = usize::try_from(i).ok().and_then(|i| recorded.borrow().get(i).cloned());
            let Some(command) = command else {
                return Err(fail(QUEUE_OP, ErrorKind::InvalidArgument, "indirect command index is out of range"));
            };
            if let Some(pipeline) = command.pipeline {
                self.render_set_pipeline(pipeline)?;
            }
            for &(slot, offset, buffer) in &command.buffers {
                self.set_stage_buffer(ShaderStage::Vertex, slot, offset, buffer)?;
            }
            match command.draw {
                Some(IcbDraw::Draw { primitive, first, count, instances }) => self.draw(primitive, first, count, instances)?,
                Some(IcbDraw::Indexed { primitive, index_type, buffer, offset, count, instances }) => {
                    self.draw_indexed(primitive, index_type, buffer, offset, count, instances)?
                }
                None => (),
            }
        }
        Ok(())
    }
    fn end_render(&self) -> Result<(), Error> {
        self.cmds.run(QUEUE_OP, &mut |c| {
            c.render_end();
            Ok(())
        })
    }
}

fn render_encoder(cmds: &Rc<Cmds>, target: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error> {
    const OP: &str = QUEUE_OP;
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
    Ok(Box::new(Render { cmds: cmds.clone() }))
}

struct CommandsHandle {
    cmds: Rc<Cmds>,
}

impl DriverCommands for CommandsHandle {
    fn compute_encoder(&self) -> Result<Box<dyn DriverComputeEncoder>, Error> {
        compute_encoder(&self.cmds)
    }
    fn accel_encoder(&self) -> Result<Box<dyn DriverAccelEncoder>, Error> {
        accel_encoder(&self.cmds)
    }
    fn blit_encoder(&self) -> Result<Box<dyn DriverBlitEncoder>, Error> {
        blit_encoder(&self.cmds)
    }
    fn render_encoder(&self, target: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error> {
        render_encoder(&self.cmds, target)
    }
    fn commit(&self) -> Result<Receipt, Error> {
        self.cmds.commit_with(QUEUE_OP, &mut |slot| match slot.take() {
            Some(commands) => commands.commit(),
            None => Err(vk::Error { operation: "commands_commit", kind: vk::ErrorKind::InvalidHandle }),
        })
    }
    fn commit_present(&self, source: Token, frame: DriverFrame) -> Result<Receipt, Error> {
        let source = self.cmds.dev.find_texture(QUEUE_OP, source)?;
        let surface = self.cmds.dev.frames.borrow().get(&frame.frame_token).cloned();
        let Some(surface) = surface else {
            return Err(fail(QUEUE_OP, ErrorKind::InvalidState, "frame token is stale"));
        };
        let result = self.cmds.commit_with(QUEUE_OP, &mut |slot| {
            let handle_error = || vk::Error { operation: "commit_present", kind: vk::ErrorKind::InvalidHandle };
            let commands = slot.as_mut().ok_or_else(handle_error)?;
            let mut surface = surface.borrow_mut();
            let epoch = surface.as_mut().ok_or_else(handle_error)?.present(commands, &source.texture)?;
            *slot = None;
            Ok(epoch)
        });
        if result.is_ok() {
            self.cmds.dev.frames.borrow_mut().remove(&frame.frame_token);
        }
        result
    }
    fn abandon(&self) -> Result<(), Error> {
        *self.cmds.commands.borrow_mut() = None;
        Ok(())
    }
}

fn begin_commands(dev: &Rc<Dev>, queue: &Rc<QueueState>) -> Result<Box<dyn DriverCommands>, Error> {
    let native = queue.native(QUEUE_OP)?.begin().map_err(|failure| of_vk(QUEUE_OP, failure))?;
    let cmds = Rc::new(Cmds { dev: dev.clone(), queue: queue.clone(), commands: RefCell::new(Some(native)) });
    Ok(Box::new(CommandsHandle { cmds }))
}

struct NativeQueue {
    dev: Rc<Dev>,
    state: Rc<QueueState>,
}

impl DriverQueue for NativeQueue {
    fn complete_through(&self, epoch: i64) -> Result<(), Error> {
        let epoch = self.state.bound_epoch(QUEUE_OP, epoch)?;
        if epoch <= self.state.completed.get() {
            return Ok(());
        }
        self.state.native(QUEUE_OP)?.wait(epoch as u64).map_err(|failure| of_vk(QUEUE_OP, failure))?;
        self.state.completed.set(epoch);
        Ok(())
    }
    fn poll_through(&self, epoch: i64) -> Result<bool, Error> {
        let epoch = self.state.bound_epoch(QUEUE_OP, epoch)?;
        if epoch <= self.state.completed.get() {
            return Ok(true);
        }
        let queue = self.state.native(QUEUE_OP)?;
        let done = queue.completed().map_err(|failure| of_vk(QUEUE_OP, failure))? as i64;
        if done >= epoch {
            queue.poll();
            self.state.completed.set(epoch);
            Ok(true)
        } else {
            Ok(false)
        }
    }
    fn completed_epoch(&self) -> i64 {
        self.state.completed.get()
    }
    fn begin_commands(&self) -> Result<Box<dyn DriverCommands>, Error> {
        begin_commands(&self.dev, &self.state)
    }
    fn gpu_duration(&self, epoch: i64) -> Option<f64> {
        self.state.native(QUEUE_OP).ok().and_then(|queue| queue.duration(epoch as u64))
    }
    fn gpu_timing(&self) -> GpuTiming {
        GpuTiming { gpu_seconds: self.state.native(QUEUE_OP).map_or(0., |queue| queue.total_seconds()) }
    }
    fn destroy_queue(&self) -> Result<(), Error> {
        let completed = self.state.native(QUEUE_OP)?.completed().map_or(-1, |done| done as i64);
        if self.state.active() > 0 && completed < self.state.next_epoch.get() - 1 {
            Err(fail(QUEUE_OP, ErrorKind::InvalidState, "queue has commands in flight"))
        } else {
            self.dev.queues.set(self.dev.queues.get() - 1);
            live_add(-1);
            *self.state.queue.borrow_mut() = None;
            Ok(())
        }
    }
}

fn create_queue(dev: &Rc<Dev>) -> Result<Box<dyn DriverQueue>, Error> {
    let device = dev.device(QUEUE_OP)?;
    let native = device.queue().map_err(|failure| of_vk(QUEUE_OP, failure))?;
    live_add(1);
    dev.queues.set(dev.queues.get() + 1);
    let state = Rc::new(QueueState { queue: RefCell::new(Some(native)), next_epoch: Cell::new(1), completed: Cell::new(0) });
    Ok(Box::new(NativeQueue { dev: dev.clone(), state }))
}

const SURFACE_OP: &str = "Ogpu_vulkan.surface";

fn immediate(configuration: &Configuration) -> bool {
    configuration.present_mode == PresentMode::Immediate
}

fn surface_stale() -> Error {
    error::make(SURFACE_OP, ErrorKind::StaleHandle, "surface is destroyed")
}

struct SurfaceHandle {
    dev: Rc<Dev>,
    cell: Rc<RefCell<Option<vk::Surface>>>,
}

impl DriverSurface for SurfaceHandle {
    fn configure(&self, configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<(), Error> {
        match self.cell.borrow_mut().as_mut() {
            Some(surface) => surface.configure(immediate(configuration)).map_err(|failure| of_vk(SURFACE_OP, failure)),
            None => Err(surface_stale()),
        }
    }
    fn acquire_sync(&self) -> Result<DriverAcquire, Error> {
        let acquired = match self.cell.borrow_mut().as_mut() {
            Some(surface) => surface.acquire(),
            None => return Err(surface_stale()),
        };
        match acquired {
            Ok(vk::Acquire::Image(_)) => {
                let frame_token = self.dev.token();
                self.dev.frames.borrow_mut().insert(frame_token, self.cell.clone());
                Ok(DriverAcquire::Acquired(DriverFrame { frame_token }))
            }
            Ok(vk::Acquire::TimedOut) => Ok(DriverAcquire::Timeout),
            Ok(vk::Acquire::Occluded) => Ok(DriverAcquire::Occluded),
            Err(failure) if failure.kind == vk::ErrorKind::DeviceLost => Ok(DriverAcquire::DeviceLost),
            Err(failure) => Err(of_vk(SURFACE_OP, failure)),
        }
    }
    fn discard(&self, frame: DriverFrame) -> Result<(), Error> {
        if self.dev.frames.borrow_mut().remove(&frame.frame_token).is_some() {
            if let Some(surface) = self.cell.borrow_mut().as_mut() {
                surface.discard();
            }
            Ok(())
        } else {
            Err(fail(SURFACE_OP, ErrorKind::InvalidState, "frame token is stale"))
        }
    }
    fn destroy_surface(&self) -> Result<(), Error> {
        self.dev.frames.borrow_mut().retain(|_, surface| !Rc::ptr_eq(surface, &self.cell));
        live_add(-1);
        *self.cell.borrow_mut() = None;
        Ok(())
    }
}

fn create_surface(dev: &Rc<Dev>, configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn DriverSurface>, Error> {
    let Some(layer) = &configuration.layer else {
        return Err(fail(SURFACE_OP, ErrorKind::Unsupported, "surface needs a native layer token"));
    };
    let device = dev.device(SURFACE_OP)?;
    let native = device.surface(layer, layer.owner_id(), layer.generation(), immediate(configuration)).map_err(|failure| of_vk(SURFACE_OP, failure))?;
    live_add(1);
    Ok(Box::new(SurfaceHandle { dev: dev.clone(), cell: Rc::new(RefCell::new(Some(native))) }))
}

struct VulkanDevice {
    dev: Rc<Dev>,
    handle: handle::Device,
    capabilities: Caps,
    device_live: Rc<Cell<bool>>,
}

impl DriverDevice for VulkanDevice {
    fn device_handle(&self) -> handle::Device {
        self.handle.clone()
    }
    fn capabilities(&self) -> Caps {
        self.capabilities
    }
    fn allocated_size(&self) -> i64 {
        let memory = self.dev.device("Ogpu_vulkan.memory").ok().and_then(|device| device.memory().ok());
        memory.map_or(0, |m| m.live_bytes as i64)
    }
    fn recommended_working_set(&self) -> i64 {
        let memory = self.dev.device("Ogpu_vulkan.memory").ok().and_then(|device| device.memory().ok());
        memory.map_or(0, |m| m.device_local_budget as i64)
    }
    fn create_buffer(&self, descriptor: &AdmittedBuffer) -> Result<Box<dyn DriverResource>, Error> {
        if !descriptor.admitted_for(&self.capabilities) {
            return Err(fail("Ogpu_vulkan.create_buffer", ErrorKind::CrossDevice, "descriptor was admitted for different device capabilities"));
        }
        let memory = match descriptor.storage { StorageMode::Shared => Memory::Shared, StorageMode::Private => Memory::DeviceLocal,
            _ => return Err(fail("Ogpu_vulkan.create_buffer", ErrorKind::Unsupported, "native storage mode is unavailable in Vulkan")) };
        create_buffer(&self.dev, memory, &descriptor.descriptor)
    }
    fn create_mapped_buffer(&self, descriptor: &AdmittedBuffer) -> Result<DriverMapped, Error> {
        if !descriptor.admitted_for(&self.capabilities) {
            return Err(fail("Ogpu_vulkan.create_mapped_buffer", ErrorKind::CrossDevice, "descriptor was admitted for different device capabilities"));
        }
        create_mapped_buffer(&self.dev, &descriptor.descriptor)
    }
    fn create_texture(&self, descriptor: &AdmittedTexture) -> Result<Box<dyn DriverResource>, Error> {
        if !descriptor.admitted_for(&self.capabilities) {
            return Err(fail("Ogpu_vulkan.create_texture", ErrorKind::CrossDevice, "descriptor was admitted for different device capabilities"));
        }
        if descriptor.storage == StorageMode::Memoryless {
            return Err(fail("Ogpu_vulkan.create_memoryless_texture", ErrorKind::Unsupported, "this device has no tile memory"));
        }
        register_texture(&self.dev, descriptor)
    }
    fn create_library(&self, shader: &Shader) -> Result<Box<dyn DriverLibrary>, Error> {
        create_library(&self.dev, shader)
    }
    fn warm_library(&self, _shader: &Shader) -> Option<Warm> {
        None
    }
    fn create_accel(&self, descriptor: &DriverAccelDescriptor) -> Result<Box<dyn DriverAccel>, Error> {
        create_accel(&self.dev, descriptor)
    }
    // Vulkan stores a 24-bit user id and 8-bit mask together; motion records are the portable
    // 44-byte keyframe layout consumed when swept instance AABBs are built.
    fn instance_layout(&self, kind: InstanceKind) -> Vec<i64> {
        match kind {
            InstanceKind::UserIdInstances => vec![68, 0, -1, 52, 56, 48, -1, -1, -1, -1, -1, -1],
            InstanceKind::MotionInstances => vec![44, -1, -1, 0, 4, 8, 12, 16, 20, 24, 28, 32],
        }
    }
    fn create_sampler(&self, descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn DriverSampler>, Error> {
        create_sampler(&self.dev, descriptor)
    }
    fn create_render_pipeline(&self, options: RenderPipelineOptions, descriptor: &RenderDescriptor, _admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Box<dyn DriverPipeline>, Error> {
        if !_admitted.admitted_for(&self.capabilities) {
            return Err(fail("Ogpu_vulkan.create_render_pipeline", ErrorKind::CrossDevice, "descriptor was admitted for different device capabilities"));
        }
        create_render_pipeline(&self.dev, options, descriptor)
    }
    fn create_icb(&self, profile: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn DriverIcb>, Error> {
        create_icb(&self.dev, profile)
    }
    fn create_argument(&self, _pipeline: Token, _stage: ShaderStage, _index: ogpu_core::validation::AdmittedSlot) -> Result<Box<dyn DriverArgument>, Error> {
        create_argument(&self.dev)
    }
    fn create_queue(&self) -> Result<Box<dyn DriverQueue>, Error> {
        create_queue(&self.dev)
    }
    fn create_surface(&self, configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn DriverSurface>, Error> {
        create_surface(&self.dev, configuration)
    }
    fn destroy_device(&self) -> Result<(), Error> {
        if self.dev.queues.get() != 0 {
            return Err(fail("Ogpu_vulkan.destroy_device", ErrorKind::InvalidState, "device has active queues"));
        }
        self.device_live.set(false);
        // Natives leaked past their owners go with the device.
        self.dev.buffers.borrow_mut().clear();
        self.dev.textures.borrow_mut().clear();
        self.dev.pipelines.borrow_mut().clear();
        self.dev.samplers.borrow_mut().clear();
        self.dev.icbs.borrow_mut().clear();
        self.dev.arguments.borrow_mut().clear();
        self.dev.frames.borrow_mut().clear();
        self.dev.accels.borrow_mut().clear();
        self.dev.accel_geometries.borrow_mut().clear();
        self.dev.accel_resources.borrow_mut().clear();
        *self.dev.device.borrow_mut() = None;
        handle::destroy_device(&self.handle);
        Ok(())
    }
}

fn create_device(device_live: &Rc<Cell<bool>>, validation: (bool, bool)) -> Result<Box<dyn DriverDevice>, Error> {
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
    let handle = handle::create_device();
    let capabilities = Caps {
        limits: Limits {
            max_buffer_size: facts.max_buffer_bytes as i64,
            max_texture_dimension_1d: facts.max_texture_dimension_1d as i64,
            max_texture_dimension_2d: facts.max_texture_dimension_2d as i64,
            max_texture_dimension_3d: facts.max_texture_dimension_3d as i64,
            max_texture_dimension_cube: facts.max_texture_dimension_cube as i64,
            max_texture_array_layers: facts.max_texture_array_layers as i64,
            max_bind_groups: 4,
            sample_counts: facts.sample_counts, color_sample_counts: facts.color_sample_counts, depth_sample_counts: facts.depth_sample_counts,
        },
        compute_pipeline: true,
        render_pipeline: true,
        render_requires_fragment: false,
        ray_tracing: true,
        ray_tracing_curves: false,
        // only after device_create enables rayQuery and accelerationStructure
        ray_tracing_motion: true,
        ray_tracing_refit: true,
        memoryless_attachments: false,
        unified_memory: false,
    };
    Ok(Box::new(VulkanDevice { dev, handle, capabilities, device_live: device_live.clone() }))
}

struct VulkanDriver {
    device_live: Rc<Cell<bool>>,
    validation: (bool, bool),
}

impl Driver for VulkanDriver {
    fn create_device(&self) -> Result<Box<dyn DriverDevice>, Error> {
        create_device(&self.device_live, self.validation)
    }
}

/// The driver: one live device at a time, validation as `RAYS_VULKAN_VALIDATION` says (`fatal`, `on`).
pub fn create() -> Box<dyn Driver> {
    create_with_validation(validation_mode())
}

/// The same with validation named: (enabled, fatal).
pub fn create_with_validation(validation: (bool, bool)) -> Box<dyn Driver> {
    Box::new(VulkanDriver { device_live: Rc::new(Cell::new(false)), validation })
}
