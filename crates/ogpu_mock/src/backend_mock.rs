//! The portable mock: host memory behind the driver record. It never executes compute, ray
//! tracing or rasterization (those are `Unsupported`); it stores buffers and textures, replays
//! recorded buffer-to-texture blits at commit, and counts what is alive.
use ogpu_core::validation::{AdmittedBuffer, AdmittedTexture, StorageMode};
use ogpu_core::{
    backend::{
        Driver, DriverAccel, DriverAccelDescriptor, DriverAccelEncoder, DriverAcquire, DriverArgument,
        DriverBlitEncoder, DriverCommands, DriverComputeEncoder, DriverDevice, DriverFrame, DriverIcb,
        DriverLibrary, DriverMapped, DriverMapping, DriverPipeline, DriverQueue, DriverRenderEncoder,
        DriverRenderTarget, DriverResource, DriverSampler, DriverSurface, InstanceKind,
        OwnedReadback, Receipt, RenderPipelineOptions, ShaderStage, Warm,
    },
    caps,
    error::{self, Error, ErrorKind},
    handle,
    pipeline::RenderDescriptor,
    shader::{Binding, Shader},
    types::{self, BufferDescriptor, Extent, Origin, TextureDescriptor, TextureFormat},
};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
    sync::{Arc, Mutex},
};

type Bytes = Arc<Mutex<Vec<u8>>>;

struct TextureStorage {
    descriptor: TextureDescriptor,
    levels: Vec<Bytes>,
}

struct State {
    next: i64,
    memory: HashMap<i64, Bytes>,
    texture_storage: HashMap<i64, TextureStorage>,
    lost: bool,
    fail_submission: bool,
    fail_completion: bool,
    fail_texture_after: Option<i64>,
    fail_depth_after: Option<i64>,
    fail_configure: bool,
    trace: Vec<String>,
    buffers: i64,
    textures: i64,
    pipelines: i64,
    queues: i64,
    surfaces: i64,
    max_allocations: Option<i64>,
}

/// The test-side view of one driver's counters.
#[derive(Clone)]
pub struct Control(Rc<RefCell<State>>);

type Shared = Rc<RefCell<State>>;

fn fail(op: &str, kind: ErrorKind, text: &str) -> Error {
    error::make(op, kind, text)
}

fn add(c: &Shared, text: String) {
    c.borrow_mut().trace.push(text);
}

fn token(c: &Shared) -> i64 {
    let mut state = c.borrow_mut();
    let value = state.next;
    state.next += 1;
    value
}

fn mip_extent(value: i64, mip: i64) -> i64 {
    // Mip levels deeper than the machine shift width still have extent one.
    std::cmp::max(1, if mip >= 63 { 0 } else { value >> mip })
}

/// Zeroed host memory, or None when the allocation cannot be made.
fn zeroed(size: i64) -> Option<Vec<u8>> {
    let size = usize::try_from(size).ok()?;
    let mut bytes = Vec::new();
    bytes.try_reserve_exact(size).ok()?;
    bytes.resize(size, 0);
    Some(bytes)
}

fn texture_level_size(d: &TextureDescriptor, mip: i64) -> Option<i64> {
    let texels = (mip_extent(d.width, mip) as i128)
        .checked_mul(mip_extent(d.height, mip) as i128)?
        .checked_mul(mip_extent(d.depth, mip) as i128)?;
    let texel = types::texel_bytes(d.format) as i128;
    i64::try_from(texels.checked_mul(texel)?).ok()
}

/// A blit recorded by the mock encoder and replayed at commit.
#[derive(Clone, Copy)]
struct BufferToTexture {
    src: i64,
    offset: i64,
    row: i64,
    image: i64,
    dst: i64,
    mip: i64,
    origin: Origin,
    extent: Extent,
}

fn in_range(len: usize, offset: i64, length: i64) -> bool {
    let len = len as i64;
    offset >= 0 && length >= 0 && offset <= len && length <= len - offset
}

fn product(a: i64, b: i64) -> Option<i64> {
    if a < 0 || b < 0 { None } else { a.checked_mul(b) }
}

fn sum(a: i64, b: i64) -> Option<i64> {
    if a < 0 || b < 0 { None } else { a.checked_add(b) }
}

fn buffer_region(texel: i64, len: usize, offset: i64, row: i64, image: i64, extent: &Extent) -> bool {
    let (width, height, depth) = (extent.width, extent.height, extent.depth);
    if width <= 0 || height <= 0 || depth <= 0 {
        return false;
    }
    let (Some(pixels), Some(minimum_image)) = (product(width, texel), product(row, height)) else {
        return false;
    };
    if row < pixels || image < minimum_image {
        return false;
    }
    let (Some(slices), Some(rows)) = (product(image, depth - 1), product(row, height - 1)) else {
        return false;
    };
    match sum(slices, rows).and_then(|span| sum(span, pixels)) {
        None => false,
        Some(span) => in_range(len, offset, span),
    }
}

fn valid_region(storage: &TextureStorage, mip: i64, origin: &Origin, extent: &Extent) -> bool {
    if mip < 0 || mip >= storage.levels.len() as i64 {
        return false;
    }
    let width = mip_extent(storage.descriptor.width, mip);
    let height = mip_extent(storage.descriptor.height, mip);
    let depth = mip_extent(storage.descriptor.depth, mip);
    origin.x >= 0
        && origin.y >= 0
        && origin.z >= 0
        && extent.width > 0
        && extent.height > 0
        && extent.depth > 0
        && origin.x <= width - extent.width
        && origin.y <= height - extent.height
        && origin.z <= depth - extent.depth
}

fn execute_transfer(c: &Shared, t: &BufferToTexture) -> Result<(), Error> {
    let op = "Backend_mock.transfer";
    let (source, target) = {
        let state = c.borrow();
        let Some(source) = state.memory.get(&t.src).cloned() else {
            return Err(fail(op, ErrorKind::StaleHandle, "command resource is destroyed"));
        };
        if !state.memory.contains_key(&t.dst) {
            return Err(fail(op, ErrorKind::StaleHandle, "command resource is destroyed"));
        }
        let Some(storage) = state.texture_storage.get(&t.dst) else {
            return Err(fail(op, ErrorKind::InvalidArgument, "resource is not a color texture"));
        };
        let texel = types::texel_bytes(storage.descriptor.format);
        let ok = valid_region(storage, t.mip, &t.origin, &t.extent)
            && buffer_region(texel, source.lock().unwrap().len(), t.offset, t.row, t.image, &t.extent);
        if !ok {
            return Err(fail(op, ErrorKind::InvalidArgument, "texture upload range is invalid"));
        }
        let width = mip_extent(storage.descriptor.width, t.mip);
        let height = mip_extent(storage.descriptor.height, t.mip);
        let target_offset = (((t.origin.z * height) + t.origin.y) * width + t.origin.x) * texel;
        (source, (storage.levels[t.mip as usize].clone(), texel, width, height, target_offset))
    };
    let (level, texel, width, height, target_offset) = target;
    for z in 0..t.extent.depth {
        for y in 0..t.extent.height {
            let from = (t.offset + z * t.image + y * t.row) as usize;
            let to = (target_offset + z * width * height * texel + y * width * texel) as usize;
            let n = (t.extent.width * texel) as usize;
            // a texture's level 0 can also be the source: copy through a temporary
            let row = source.lock().unwrap()[from..from + n].to_vec();
            level.lock().unwrap()[to..to + n].copy_from_slice(&row);
        }
    }
    Ok(())
}

fn should_fail(field: Option<i64>) -> bool {
    field == Some(0)
}

fn advance(field: Option<i64>) -> Option<i64> {
    match field {
        None | Some(0) => None,
        Some(remaining) => Some(remaining - 1),
    }
}

struct MockResource {
    c: Shared,
    id: i64,
    kind: &'static str,
    bytes: Bytes,
}

impl DriverResource for MockResource {
    fn token(&self) -> i64 { self.id }
    fn write(&self, offset: i64, source: &[u8]) -> Result<(), Error> {
        let mut data = self.bytes.lock().unwrap();
        if !in_range(data.len(), offset, source.len() as i64) {
            return Err(fail("Backend_mock.write", ErrorKind::InvalidArgument, "range"));
        }
        data[offset as usize..offset as usize + source.len()].copy_from_slice(source);
        Ok(())
    }
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, Error> {
        let data = self.bytes.lock().unwrap();
        if !in_range(data.len(), offset, length as i64) {
            return Err(fail("Backend_mock.read", ErrorKind::InvalidArgument, "range"));
        }
        Ok(data[offset as usize..offset as usize + length].to_vec())
    }
    fn read_into(&self, offset: i64, destination: &mut [u8], destination_offset: usize, length: usize) -> Result<(), Error> {
        let data = self.bytes.lock().unwrap();
        if !in_range(data.len(), offset, length as i64)
            || destination_offset > destination.len()
            || length > destination.len() - destination_offset
        {
            return Err(fail("Backend_mock.read_into", ErrorKind::InvalidArgument, "range"));
        }
        destination[destination_offset..destination_offset + length]
            .copy_from_slice(&data[offset as usize..offset as usize + length]);
        Ok(())
    }
    fn destroy(&self) -> Result<(), Error> {
        let mut state = self.c.borrow_mut();
        state.memory.remove(&self.id);
        state.texture_storage.remove(&self.id);
        if self.kind == "buffer" { state.buffers -= 1 } else { state.textures -= 1 }
        state.trace.push(format!("destroy-{}:{}", self.kind, self.id));
        Ok(())
    }
}

/// kind is "buffer" or "texture".
fn resource(c: &Shared, kind: &'static str, size: i64) -> Result<MockResource, Error> {
    let over = {
        let state = c.borrow();
        matches!(state.max_allocations, Some(limit) if state.buffers + state.textures >= limit)
    };
    if over {
        return Err(fail("Backend_mock.allocate", ErrorKind::Capacity, "allocation count limit"));
    }
    let Some(initial) = zeroed(size) else {
        return Err(fail("Backend_mock.allocate", ErrorKind::Capacity, "allocation exceeds mock byte capacity"));
    };
    let id = token(c);
    let bytes: Bytes = Arc::new(Mutex::new(initial));
    {
        let mut state = c.borrow_mut();
        state.memory.insert(id, bytes.clone());
        if kind == "buffer" { state.buffers += 1 } else { state.textures += 1 }
        state.trace.push(format!("create-{kind}:{id}"));
    }
    Ok(MockResource { c: c.clone(), id, kind, bytes })
}

struct MockMapping {
    device: Bytes,
}

impl DriverMapping for MockMapping {
    fn readback(&self) -> OwnedReadback {
        let device = self.device.clone();
        Arc::new(move |offset, length| {
            let data = device.lock().unwrap();
            let offset = usize::try_from(offset).map_err(|_| fail("Backend_mock.readback", ErrorKind::InvalidArgument, "range"))?;
            let end = offset.checked_add(length).filter(|end| *end <= data.len())
                .ok_or_else(|| fail("Backend_mock.readback", ErrorKind::InvalidArgument, "range"))?;
            Ok(data[offset..end].to_vec())
        })
    }
}

fn create_mapped_buffer(c: &Shared, d: &BufferDescriptor) -> Result<DriverMapped, Error> {
    let raw = resource(c, "buffer", d.size)?;
    let mapping = MockMapping { device: raw.bytes.clone() };
    Ok(DriverMapped { mapped_resource: Box::new(raw), mapping: Box::new(mapping) })
}

fn create_texture(c: &Shared, d: &TextureDescriptor) -> Result<MockResource, Error> {
    let operation = "Backend_mock.create_texture";
    {
        let mut state = c.borrow_mut();
        if should_fail(state.fail_texture_after) {
            state.fail_texture_after = None;
            return Err(fail(operation, ErrorKind::Capacity, "injected texture allocation failure"));
        }
        state.fail_texture_after = advance(state.fail_texture_after);
    }
    let Some(size) = texture_level_size(d, 0) else {
        return Err(fail(operation, ErrorKind::Capacity, "texture exceeds mock byte capacity"));
    };
    let raw = resource(c, "texture", size)?;
    let mut levels: Vec<Bytes> = Vec::new();
    for mip in 0..d.mip_levels {
        if mip == 0 {
            levels.push(raw.bytes.clone());
        } else {
            match texture_level_size(d, mip).and_then(zeroed) {
                Some(bytes) => levels.push(Arc::new(Mutex::new(bytes))),
                None => {
                    let _ = raw.destroy();
                    return Err(fail(operation, ErrorKind::Capacity, "texture exceeds mock byte capacity"));
                }
            }
        }
    }
    c.borrow_mut().texture_storage.insert(raw.id, TextureStorage { descriptor: d.clone(), levels });
    Ok(raw)
}

struct MockDepthTexture { c: Shared, id: i64 }

impl DriverResource for MockDepthTexture {
    fn token(&self) -> i64 { self.id }
    fn write(&self, _: i64, _: &[u8]) -> Result<(), Error> {
        Err(fail("Backend_mock.depth.write", ErrorKind::Unsupported, "depth textures are not host writable"))
    }
    fn read(&self, _: i64, _: usize) -> Result<Vec<u8>, Error> {
        Err(fail("Backend_mock.depth.read", ErrorKind::Unsupported, "depth textures are not host readable"))
    }
    fn read_into(&self, _: i64, _: &mut [u8], _: usize, _: usize) -> Result<(), Error> {
        Err(fail("Backend_mock.depth.read_into", ErrorKind::Unsupported, "depth textures are not host readable"))
    }
    fn destroy(&self) -> Result<(), Error> {
        let mut state = self.c.borrow_mut();
        state.textures -= 1;
        state.trace.push(format!("destroy-depth-texture:{}", self.id));
        Ok(())
    }
}

fn create_depth_texture(c: &Shared) -> Result<MockDepthTexture, Error> {
    {
        let mut state = c.borrow_mut();
        if should_fail(state.fail_depth_after) {
            state.fail_depth_after = None;
            return Err(fail("Backend_mock.create_depth_texture", ErrorKind::Capacity, "injected depth allocation failure"));
        }
        state.fail_depth_after = advance(state.fail_depth_after);
    }
    let id = token(c);
    {
        let mut state = c.borrow_mut();
        state.textures += 1;
        state.trace.push(format!("create-depth-texture:{id}"));
    }
    Ok(MockDepthTexture { c: c.clone(), id })
}

struct MockQueue {
    c: Shared,
    epoch: Rc<Cell<i64>>,
}

impl DriverQueue for MockQueue {
    fn complete_through(&self, e: i64) -> Result<(), Error> {
        add(&self.c, format!("complete:{e}"));
        let injected = std::mem::replace(&mut self.c.borrow_mut().fail_completion, false);
        if injected {
            Err(fail("Backend_mock.complete", ErrorKind::DeviceLost, "injected terminal completion failure"))
        } else {
            Ok(())
        }
    }
    fn poll_through(&self, e: i64) -> Result<bool, Error> {
        self.complete_through(e).map(|()| true)
    }
    fn begin_commands(&self) -> Result<Box<dyn DriverCommands>, Error> {
        Ok(Box::new(MockCommands {
            id: token(&self.c),
            c: self.c.clone(),
            epoch: self.epoch.clone(),
            ops: Rc::new(RefCell::new(Vec::new())),
        }))
    }
    fn gpu_duration(&self, _: i64) -> Option<f64> { None }
    fn destroy_queue(&self) -> Result<(), Error> {
        self.c.borrow_mut().queues -= 1;
        Ok(())
    }
}

fn create_queue(c: &Shared) -> Result<Box<dyn DriverQueue>, Error> {
    token(c);
    c.borrow_mut().queues += 1;
    Ok(Box::new(MockQueue { c: c.clone(), epoch: Rc::new(Cell::new(0)) }))
}

struct MockCommands {
    c: Shared,
    id: i64,
    epoch: Rc<Cell<i64>>,
    ops: Rc<RefCell<Vec<BufferToTexture>>>,
}

impl MockCommands {
    // Preserve deterministic injected-failure/device-loss precedence.
    fn submit(&self, operation: &str) -> Result<(), Error> {
        let injected = std::mem::replace(&mut self.c.borrow_mut().fail_submission, false);
        if injected {
            Err(fail(operation, ErrorKind::InvalidState, "injected submission failure"))
        } else if self.c.borrow().lost {
            Err(fail(operation, ErrorKind::DeviceLost, "injected device loss"))
        } else {
            Ok(())
        }
    }
    fn run(&self) -> Result<(), Error> {
        self.ops.borrow().iter().try_for_each(|t| execute_transfer(&self.c, t))
    }
}

struct MockBlit(Rc<RefCell<Vec<BufferToTexture>>>);

impl DriverBlitEncoder for MockBlit {
    fn buffer_to_texture(&self, src: i64, offset: i64, row: i64, image: i64, dst: i64, mip: i64, origin: Origin, extent: Extent) -> Result<(), Error> {
        self.0.borrow_mut().push(BufferToTexture { src, offset, row, image, dst, mip, origin, extent });
        Ok(())
    }
    fn end_blit(&self) -> Result<(), Error> { Ok(()) }
}

impl DriverCommands for MockCommands {
    fn compute_encoder(&self) -> Result<Box<dyn DriverComputeEncoder>, Error> {
        Err(fail("Backend_mock.compute_encoder", ErrorKind::Unsupported, "mock backend does not execute compute shaders"))
    }
    fn accel_encoder(&self) -> Result<Box<dyn DriverAccelEncoder>, Error> {
        Err(fail("Backend_mock.accel_encoder", ErrorKind::Unsupported, "mock backend has no ray tracing"))
    }
    fn blit_encoder(&self) -> Result<Box<dyn DriverBlitEncoder>, Error> {
        Ok(Box::new(MockBlit(self.ops.clone())))
    }
    fn render_encoder(&self, _: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error> {
        Err(fail("Backend_mock.render_encoder", ErrorKind::Unsupported, "mock backend does not rasterize"))
    }
    fn commit(&self) -> Result<Receipt, Error> {
        self.submit("Backend_mock.commit")?;
        self.run()?;
        self.epoch.set(self.epoch.get() + 1);
        add(&self.c, format!("commit:{}:{}", self.id, self.epoch.get()));
        Ok(Receipt { epoch: self.epoch.get() })
    }
    fn commit_present(&self, source: i64, frame: DriverFrame) -> Result<Receipt, Error> {
        self.submit("Backend_mock.commit_present")?;
        self.run()?;
        self.epoch.set(self.epoch.get() + 1);
        add(&self.c, format!("commit-present:{}:{source}:{}:{}", self.id, frame.frame_token, self.epoch.get()));
        Ok(Receipt { epoch: self.epoch.get() })
    }
    fn abandon(&self) -> Result<(), Error> {
        self.ops.borrow_mut().clear();
        Ok(())
    }
}

struct MockSurface { c: Shared, frame: Cell<i64> }

impl DriverSurface for MockSurface {
    fn configure(&self, _: &ogpu_core::surface::AdmittedConfiguration) -> Result<(), Error> {
        let injected = std::mem::replace(&mut self.c.borrow_mut().fail_configure, false);
        if injected {
            Err(fail("Backend_mock.configure", ErrorKind::InvalidState, "injected configure failure"))
        } else {
            Ok(())
        }
    }
    fn acquire_sync(&self) -> Result<DriverAcquire, Error> {
        if self.c.borrow().lost {
            Ok(DriverAcquire::DeviceLost)
        } else {
            self.frame.set(self.frame.get() + 1);
            Ok(DriverAcquire::Acquired(DriverFrame { frame_token: self.frame.get() }))
        }
    }
    fn discard(&self, _: DriverFrame) -> Result<(), Error> {
        add(&self.c, "discard".to_owned());
        Ok(())
    }
    fn destroy_surface(&self) -> Result<(), Error> {
        self.c.borrow_mut().surfaces -= 1;
        Ok(())
    }
}

fn create_surface(c: &Shared) -> Result<Box<dyn DriverSurface>, Error> {
    token(c);
    c.borrow_mut().surfaces += 1;
    Ok(Box::new(MockSurface { c: c.clone(), frame: Cell::new(0) }))
}

struct MockLibrary { c: Shared, id: i64 }

impl DriverLibrary for MockLibrary {
    fn create_compute_pipeline_in(&self, _: &ogpu_core::shader::AdmittedFunction, _: &[Binding]) -> Result<Box<dyn DriverPipeline>, Error> {
        Err(fail(
            "Backend_mock.create_compute_pipeline_in",
            ErrorKind::Unsupported,
            "mock backend does not execute compute shaders",
        ))
    }
    fn destroy_library(&self) -> Result<(), Error> {
        self.c.borrow_mut().pipelines -= 1;
        add(&self.c, format!("destroy-library:{}", self.id));
        Ok(())
    }
}

struct MockSampler { c: Shared, id: i64 }

impl DriverSampler for MockSampler {
    fn sampler_token(&self) -> i64 { self.id }
    fn destroy_sampler(&self) -> Result<(), Error> {
        self.c.borrow_mut().pipelines -= 1;
        add(&self.c, format!("destroy-sampler:{}", self.id));
        Ok(())
    }
}

struct MockDevice { c: Shared, handle: handle::Device, capabilities: caps::Caps }

impl DriverDevice for MockDevice {
    fn device_handle(&self) -> handle::Device { self.handle.clone() }
    fn capabilities(&self) -> caps::Caps { self.capabilities.clone() }
    fn allocated_size(&self) -> i64 {
        self.c.borrow().memory.values().map(|bytes| bytes.lock().unwrap().len() as i64).sum()
    }
    fn recommended_working_set(&self) -> i64 { 8 * 1024 * 1024 * 1024 }
    fn create_buffer(&self, d: &AdmittedBuffer) -> Result<Box<dyn DriverResource>, Error> {
        Ok(Box::new(resource(&self.c, "buffer", d.size)?))
    }
    fn create_mapped_buffer(&self, d: &AdmittedBuffer) -> Result<DriverMapped, Error> {
        create_mapped_buffer(&self.c, &d.descriptor)
    }
    fn create_texture(&self, d: &AdmittedTexture) -> Result<Box<dyn DriverResource>, Error> {
        if d.storage == StorageMode::Memoryless {
            return Err(fail("Backend_mock.create_memoryless_texture", ErrorKind::Unsupported, "the mock has no memoryless attachments"));
        }
        if d.format == TextureFormat::Depth32Float { Ok(Box::new(create_depth_texture(&self.c)?)) }
        else { Ok(Box::new(create_texture(&self.c, &d.descriptor)?)) }
    }
    fn create_library(&self, _: &Shader) -> Result<Box<dyn DriverLibrary>, Error> {
        let id = token(&self.c);
        self.c.borrow_mut().pipelines += 1;
        add(&self.c, format!("create-library:{id}"));
        Ok(Box::new(MockLibrary { c: self.c.clone(), id }))
    }
    fn warm_library(&self, _: &Shader) -> Option<Warm> { None }
    fn create_accel(&self, _: &DriverAccelDescriptor) -> Result<Box<dyn DriverAccel>, Error> {
        Err(fail("Backend_mock.create_accel", ErrorKind::Unsupported, "mock backend has no ray tracing"))
    }
    // The mock mirrors Metal's documented instance record layouts.
    fn instance_layout(&self, kind: InstanceKind) -> Vec<i64> {
        match kind {
            InstanceKind::UserIdInstances => vec![68, 0, 48, 52, 60, 64, -1, -1, -1, -1, -1, -1],
            InstanceKind::MotionInstances => vec![44, -1, 8, 12, 20, 24, 0, 4, 28, 32, 36, 40],
        }
    }
    fn create_sampler(&self, _: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn DriverSampler>, Error> {
        let id = token(&self.c);
        self.c.borrow_mut().pipelines += 1;
        add(&self.c, format!("create-sampler:{id}"));
        Ok(Box::new(MockSampler { c: self.c.clone(), id }))
    }
    fn create_render_pipeline(&self, _: RenderPipelineOptions, _: &RenderDescriptor, _: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Box<dyn DriverPipeline>, Error> {
        Err(fail("Backend_mock.create_render_pipeline", ErrorKind::Unsupported, "mock backend does not rasterize"))
    }
    fn create_icb(&self, _: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn DriverIcb>, Error> {
        Err(fail("Backend_mock.create_icb", ErrorKind::Unsupported, "mock backend does not rasterize"))
    }
    fn create_argument(&self, _: i64, _: ShaderStage, _: ogpu_core::validation::AdmittedSlot) -> Result<Box<dyn DriverArgument>, Error> {
        Err(fail("Backend_mock.create_argument", ErrorKind::Unsupported, "mock backend does not rasterize"))
    }
    fn create_queue(&self) -> Result<Box<dyn DriverQueue>, Error> { create_queue(&self.c) }
    fn create_surface(&self, _: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn DriverSurface>, Error> { create_surface(&self.c) }
    fn destroy_device(&self) -> Result<(), Error> {
        handle::destroy_device(&self.handle);
        add(&self.c, "destroy-device".to_owned());
        Ok(())
    }
}

struct MockDriver { c: Shared, capabilities: caps::Caps }

impl Driver for MockDriver {
    fn create_device(&self) -> Result<Box<dyn DriverDevice>, Error> {
        token(&self.c);
        Ok(Box::new(MockDevice { c: self.c.clone(), handle: handle::create_device(), capabilities: self.capabilities.clone() }))
    }
}

pub fn create() -> (Box<dyn Driver>, Control) {
    let capabilities = caps::Caps {
        compute_pipeline: false,
        render_pipeline: false,
        ray_tracing: false,
        ray_tracing_curves: false,
        ray_tracing_motion: false,
        ray_tracing_refit: false,
        ..caps::minimum_m1()
    };
    let c: Shared = Rc::new(RefCell::new(State {
        next: 1,
        memory: HashMap::new(),
        texture_storage: HashMap::new(),
        lost: false,
        fail_submission: false,
        fail_completion: false,
        fail_texture_after: None,
        fail_depth_after: None,
        fail_configure: false,
        trace: Vec::new(),
        buffers: 0,
        textures: 0,
        pipelines: 0,
        queues: 0,
        surfaces: 0,
        // a discrete device's driver caps live allocations (Vulkan maxMemoryAllocationCount is 4096
        // on common drivers); Metal has no such limit
        max_allocations: Some(4096),
    }));
    (Box::new(MockDriver { c: c.clone(), capabilities }), Control(c))
}

/// (buffers, textures, pipelines, queues, surfaces)
pub fn live_counts(control: &Control) -> (i64, i64, i64, i64, i64) {
    let s = control.0.borrow();
    (s.buffers, s.textures, s.pipelines, s.queues, s.surfaces)
}

/// Recorded operations are returned oldest first.
pub fn trace(control: &Control) -> Vec<String> {
    control.0.borrow().trace.clone()
}
