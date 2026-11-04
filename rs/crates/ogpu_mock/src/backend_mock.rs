//! The portable mock: host memory behind the driver record. It never executes compute, ray
//! tracing or rasterization (those are `Unsupported`); it stores buffers and textures, replays
//! recorded buffer-to-texture blits at commit, and counts what is alive.
use ogpu_core::{
    backend::{
        Driver, DriverAcquire, DriverBlitEncoder, DriverCommands, DriverDevice, DriverFrame,
        DriverLibrary, DriverMapped, DriverQueue, DriverResource, DriverSampler, DriverSurface, Floats,
        GpuTiming, InstanceKind, Receipt,
    },
    caps,
    error::{self, Error, ErrorKind},
    handle,
    types::{self, BufferDescriptor, Extent, Origin, TextureDescriptor},
};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

type Bytes = Rc<RefCell<Vec<u8>>>;

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

fn fail<T>(op: &str, kind: ErrorKind, text: &str) -> Result<T, Error> {
    Err(error::make(op, kind, text))
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
    // OCaml `lsr` by 64 or more is unspecified; a level that deep is 1 anyway.
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
        * (mip_extent(d.height, mip) as i128)
        * (mip_extent(d.depth, mip) as i128);
    let texel = types::texel_bytes(d.format) as i128;
    // Sys.max_string_length on 64-bit OCaml.
    let max_string_length: i128 = (1 << 57) - 9;
    if texels > max_string_length / texel { None } else { i64::try_from(texels * texel).ok() }
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
            return fail(op, ErrorKind::StaleHandle, "command resource is destroyed");
        };
        if !state.memory.contains_key(&t.dst) {
            return fail(op, ErrorKind::StaleHandle, "command resource is destroyed");
        }
        let Some(storage) = state.texture_storage.get(&t.dst) else {
            return fail(op, ErrorKind::InvalidArgument, "resource is not a color texture");
        };
        let texel = types::texel_bytes(storage.descriptor.format);
        let ok = valid_region(storage, t.mip, &t.origin, &t.extent)
            && buffer_region(texel, source.borrow().len(), t.offset, t.row, t.image, &t.extent);
        if !ok {
            return fail(op, ErrorKind::InvalidArgument, "texture upload range is invalid");
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
            let row = source.borrow()[from..from + n].to_vec();
            level.borrow_mut()[to..to + n].copy_from_slice(&row);
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

/// kind is "buffer" or "texture".
fn resource(c: &Shared, kind: &'static str, size: i64) -> Result<DriverResource, Error> {
    let over = {
        let state = c.borrow();
        matches!(state.max_allocations, Some(limit) if state.buffers + state.textures >= limit)
    };
    if over {
        return fail("Backend_mock.allocate", ErrorKind::Capacity, "allocation count limit");
    }
    let Some(initial) = zeroed(size) else {
        return fail("Backend_mock.allocate", ErrorKind::Capacity, "allocation exceeds mock byte capacity");
    };
    let id = token(c);
    let bytes: Bytes = Rc::new(RefCell::new(initial));
    {
        let mut state = c.borrow_mut();
        state.memory.insert(id, bytes.clone());
        if kind == "buffer" { state.buffers += 1 } else { state.textures += 1 }
        state.trace.push(format!("create-{kind}:{id}"));
    }
    let b_write = bytes.clone();
    let b_read = bytes.clone();
    let b_into = bytes;
    let c_destroy = c.clone();
    Ok(DriverResource {
        token: id,
        write: Box::new(move |offset, source| {
            let mut data = b_write.borrow_mut();
            if !in_range(data.len(), offset, source.len() as i64) {
                return fail("Backend_mock.write", ErrorKind::InvalidArgument, "range");
            }
            data[offset as usize..offset as usize + source.len()].copy_from_slice(source);
            Ok(())
        }),
        read: Box::new(move |offset, length| {
            let data = b_read.borrow();
            if !in_range(data.len(), offset, length as i64) {
                return fail("Backend_mock.read", ErrorKind::InvalidArgument, "range");
            }
            Ok(data[offset as usize..offset as usize + length].to_vec())
        }),
        read_into: Box::new(move |offset, destination, destination_offset, length| {
            let data = b_into.borrow();
            if !in_range(data.len(), offset, length as i64)
                || destination_offset > destination.len()
                || length > destination.len() - destination_offset
            {
                return fail("Backend_mock.read_into", ErrorKind::InvalidArgument, "range");
            }
            destination[destination_offset..destination_offset + length]
                .copy_from_slice(&data[offset as usize..offset as usize + length]);
            Ok(())
        }),
        destroy: Box::new(move || {
            let mut state = c_destroy.borrow_mut();
            state.memory.remove(&id);
            state.texture_storage.remove(&id);
            if kind == "buffer" { state.buffers -= 1 } else { state.textures -= 1 }
            state.trace.push(format!("destroy-{kind}:{id}"));
            Ok(())
        }),
    })
}

fn create_mapped_buffer(c: &Shared, d: &BufferDescriptor) -> Result<DriverMapped, Error> {
    // A discrete device: the view is a separate host copy, made when asked for and synchronised
    // only by flush and invalidate, so a caller that skips one reads stale data; a released view
    // turns NaN and its successor starts NaN, so a caller that keeps one, or reads a new one
    // before an invalidate, reads NaN.
    let size = d.size;
    let raw = resource(c, "buffer", size)?;
    let device = c.borrow().memory.get(&raw.token).cloned().expect("buffer was just registered");
    let cell: Rc<RefCell<Option<Floats>>> = Rc::new(RefCell::new(None));
    let released = Rc::new(Cell::new(false));
    let view = {
        let (cell, released) = (cell.clone(), released.clone());
        move || -> Option<Floats> {
            let mut slot = cell.borrow_mut();
            if slot.is_none() {
                let fill = if released.get() { f32::NAN } else { 0.0 };
                *slot = Some(Rc::new(RefCell::new(vec![fill; (size / 4) as usize])));
            }
            slot.clone()
        }
    };
    let invalidate_view = view.clone();
    let invalidate_device = device.clone();
    let flush_cell = cell.clone();
    Ok(DriverMapped {
        mapped_resource: raw,
        view: Box::new(view),
        release_view: Box::new(move || {
            released.set(true);
            if let Some(floats) = cell.borrow_mut().take() {
                floats.borrow_mut().fill(f32::NAN);
            }
        }),
        flush: Box::new(move |n| match flush_cell.borrow().as_ref() {
            None => fail("Backend_mock.flush", ErrorKind::InvalidArgument, "no host view was written"),
            Some(floats) => {
                let floats = floats.borrow();
                let mut device = device.borrow_mut();
                for i in 0..n / 4 {
                    device[4 * i..4 * i + 4].copy_from_slice(&floats[i].to_bits().to_le_bytes());
                }
                Ok(())
            }
        }),
        invalidate: Box::new(move |n| {
            let floats = invalidate_view().expect("view");
            let device = invalidate_device.borrow();
            let mut floats = floats.borrow_mut();
            for i in 0..n / 4 {
                floats[i] = f32::from_bits(u32::from_le_bytes(device[4 * i..4 * i + 4].try_into().unwrap()));
            }
            Ok(())
        }),
    })
}

fn create_texture(c: &Shared, d: &TextureDescriptor) -> Result<DriverResource, Error> {
    let operation = "Backend_mock.create_texture";
    {
        let mut state = c.borrow_mut();
        if should_fail(state.fail_texture_after) {
            state.fail_texture_after = None;
            return fail(operation, ErrorKind::Capacity, "injected texture allocation failure");
        }
        state.fail_texture_after = advance(state.fail_texture_after);
    }
    let Some(size) = texture_level_size(d, 0) else {
        return fail(operation, ErrorKind::Capacity, "texture exceeds mock byte capacity");
    };
    let raw = resource(c, "texture", size)?;
    let mut levels: Vec<Bytes> = Vec::new();
    for mip in 0..d.mip_levels {
        if mip == 0 {
            levels.push(c.borrow().memory.get(&raw.token).cloned().expect("texture was just registered"));
        } else {
            match texture_level_size(d, mip).and_then(zeroed) {
                Some(bytes) => levels.push(Rc::new(RefCell::new(bytes))),
                None => {
                    let _ = (raw.destroy)();
                    return fail(operation, ErrorKind::Capacity, "texture exceeds mock byte capacity");
                }
            }
        }
    }
    c.borrow_mut().texture_storage.insert(raw.token, TextureStorage { descriptor: d.clone(), levels });
    Ok(raw)
}

fn create_depth_texture(c: &Shared) -> Result<DriverResource, Error> {
    {
        let mut state = c.borrow_mut();
        if should_fail(state.fail_depth_after) {
            state.fail_depth_after = None;
            return fail("Backend_mock.create_depth_texture", ErrorKind::Capacity, "injected depth allocation failure");
        }
        state.fail_depth_after = advance(state.fail_depth_after);
    }
    let id = token(c);
    {
        let mut state = c.borrow_mut();
        state.textures += 1;
        state.trace.push(format!("create-depth-texture:{id}"));
    }
    let c = c.clone();
    Ok(DriverResource {
        token: id,
        write: Box::new(|_, _| {
            fail("Backend_mock.depth.write", ErrorKind::Unsupported, "depth textures are not host writable")
        }),
        read: Box::new(|_, _| {
            fail("Backend_mock.depth.read", ErrorKind::Unsupported, "depth textures are not host readable")
        }),
        read_into: Box::new(|_, _, _, _| {
            fail("Backend_mock.depth.read_into", ErrorKind::Unsupported, "depth textures are not host readable")
        }),
        destroy: Box::new(move || {
            let mut state = c.borrow_mut();
            state.textures -= 1;
            state.trace.push(format!("destroy-depth-texture:{id}"));
            Ok(())
        }),
    })
}

fn create_queue(c: &Shared) -> Result<DriverQueue, Error> {
    token(c);
    let epoch = Rc::new(Cell::new(0i64));
    let completed = Rc::new(Cell::new(0i64));
    c.borrow_mut().queues += 1;
    let complete_through = {
        let (c, epoch, completed) = (c.clone(), epoch.clone(), completed.clone());
        Rc::new(move |e: i64| -> Result<(), Error> {
            if e <= completed.get() || e > epoch.get() {
                return fail("Backend_mock.complete", ErrorKind::InvalidArgument, "epoch is invalid");
            }
            completed.set(e);
            add(&c, format!("complete:{e}"));
            let injected = std::mem::replace(&mut c.borrow_mut().fail_completion, false);
            if injected {
                fail("Backend_mock.complete", ErrorKind::DeviceLost, "injected terminal completion failure")
            } else {
                Ok(())
            }
        })
    };
    let poll_through = {
        let (completed, complete_through) = (completed.clone(), complete_through.clone());
        move |e: i64| -> Result<bool, Error> {
            if e <= 0 {
                fail("Backend_mock.poll", ErrorKind::InvalidArgument, "epoch is invalid")
            } else if e <= completed.get() {
                Ok(true)
            } else {
                complete_through(e).map(|()| true)
            }
        }
    };
    let begin_commands = {
        let (c, epoch) = (c.clone(), epoch.clone());
        move || -> Result<DriverCommands, Error> { Ok(create_commands(&c, &epoch)) }
    };
    let (c_destroy, completed_read) = (c.clone(), completed);
    Ok(DriverQueue {
        complete_through: Box::new(move |e| complete_through(e)),
        poll_through: Box::new(poll_through),
        completed_epoch: Box::new(move || completed_read.get()),
        begin_commands: Box::new(begin_commands),
        gpu_duration: Box::new(|_| None),
        gpu_timing: Box::new(|| GpuTiming { gpu_seconds: 0.0 }),
        destroy_queue: Box::new(move || {
            c_destroy.borrow_mut().queues -= 1;
            Ok(())
        }),
    })
}

fn create_commands(c: &Shared, epoch: &Rc<Cell<i64>>) -> DriverCommands {
    let commands = token(c);
    let ops: Rc<RefCell<Vec<BufferToTexture>>> = Rc::new(RefCell::new(Vec::new()));
    let run = {
        let (c, ops) = (c.clone(), ops.clone());
        move || -> Result<(), Error> {
            let recorded = ops.borrow().clone();
            recorded.iter().try_for_each(|t| execute_transfer(&c, t))
        }
    };
    // injected failures and loss are checked in the order of the OCaml mock
    let submit = {
        let c = c.clone();
        move |operation: &str| -> Result<(), Error> {
            let injected = std::mem::replace(&mut c.borrow_mut().fail_submission, false);
            if injected {
                fail(operation, ErrorKind::InvalidState, "injected submission failure")
            } else if c.borrow().lost {
                fail(operation, ErrorKind::DeviceLost, "injected device loss")
            } else {
                Ok(())
            }
        }
    };
    let blit_encoder = {
        let ops = ops.clone();
        move || -> Result<DriverBlitEncoder, Error> {
            let ops = ops.clone();
            Ok(DriverBlitEncoder {
                buffer_to_texture: Box::new(move |src, offset, row, image, dst, mip, origin, extent| {
                    ops.borrow_mut().push(BufferToTexture { src, offset, row, image, dst, mip, origin, extent });
                    Ok(())
                }),
                end_blit: Box::new(|| Ok(())),
            })
        }
    };
    let commit_present = {
        let (c, epoch, run, submit) = (c.clone(), epoch.clone(), run.clone(), submit.clone());
        move |source: i64, frame: DriverFrame| -> Result<Receipt, Error> {
            submit("Backend_mock.commit_present")?;
            run()?;
            epoch.set(epoch.get() + 1);
            add(&c, format!("commit-present:{commands}:{source}:{}:{}", frame.frame_token, epoch.get()));
            Ok(Receipt { epoch: epoch.get() })
        }
    };
    let commit = {
        let (c, epoch) = (c.clone(), epoch.clone());
        move || -> Result<Receipt, Error> {
            submit("Backend_mock.commit")?;
            run()?;
            epoch.set(epoch.get() + 1);
            add(&c, format!("commit:{commands}:{}", epoch.get()));
            Ok(Receipt { epoch: epoch.get() })
        }
    };
    DriverCommands {
        compute_encoder: Box::new(|| {
            fail("Backend_mock.compute_encoder", ErrorKind::Unsupported, "mock backend does not execute compute shaders")
        }),
        accel_encoder: Box::new(|| {
            fail("Backend_mock.accel_encoder", ErrorKind::Unsupported, "mock backend has no ray tracing")
        }),
        blit_encoder: Box::new(blit_encoder),
        render_encoder: Box::new(|_| {
            fail("Backend_mock.render_encoder", ErrorKind::Unsupported, "mock backend does not rasterize")
        }),
        commit: Box::new(commit),
        commit_present: Box::new(commit_present),
        abandon: Box::new(move || {
            ops.borrow_mut().clear();
            Ok(())
        }),
    }
}

fn create_surface(c: &Shared) -> Result<DriverSurface, Error> {
    token(c);
    let frame = Rc::new(Cell::new(0i64));
    c.borrow_mut().surfaces += 1;
    let (c_configure, c_acquire, c_discard, c_destroy) = (c.clone(), c.clone(), c.clone(), c.clone());
    Ok(DriverSurface {
        configure: Box::new(move |_| {
            let injected = std::mem::replace(&mut c_configure.borrow_mut().fail_configure, false);
            if injected {
                fail("Backend_mock.configure", ErrorKind::InvalidState, "injected configure failure")
            } else {
                Ok(())
            }
        }),
        acquire_sync: Box::new(move || {
            if c_acquire.borrow().lost {
                Ok(DriverAcquire::DeviceLost)
            } else {
                frame.set(frame.get() + 1);
                Ok(DriverAcquire::Acquired(DriverFrame { frame_token: frame.get() }))
            }
        }),
        discard: Box::new(move |_| {
            add(&c_discard, "discard".to_owned());
            Ok(())
        }),
        destroy_surface: Box::new(move || {
            c_destroy.borrow_mut().surfaces -= 1;
            Ok(())
        }),
    })
}

fn create_library(c: &Shared) -> Result<DriverLibrary, Error> {
    let id = token(c);
    c.borrow_mut().pipelines += 1;
    add(c, format!("create-library:{id}"));
    let c = c.clone();
    Ok(DriverLibrary {
        create_compute_pipeline_in: Box::new(|_, _, _| {
            fail(
                "Backend_mock.create_compute_pipeline_in",
                ErrorKind::Unsupported,
                "mock backend does not execute compute shaders",
            )
        }),
        destroy_library: Box::new(move || {
            c.borrow_mut().pipelines -= 1;
            add(&c, format!("destroy-library:{id}"));
            Ok(())
        }),
    })
}

fn create_sampler(c: &Shared) -> Result<DriverSampler, Error> {
    let id = token(c);
    c.borrow_mut().pipelines += 1;
    add(c, format!("create-sampler:{id}"));
    let c = c.clone();
    Ok(DriverSampler {
        sampler_token: id,
        destroy_sampler: Box::new(move || {
            c.borrow_mut().pipelines -= 1;
            add(&c, format!("destroy-sampler:{id}"));
            Ok(())
        }),
    })
}

fn create_device(c: &Shared, capabilities: caps::Caps) -> Result<DriverDevice, Error> {
    token(c);
    let device_handle = handle::create_device();
    let c_size = c.clone();
    let c_buffer = c.clone();
    let c_mapped = c.clone();
    let c_texture = c.clone();
    let c_depth = c.clone();
    let c_library = c.clone();
    let c_sampler = c.clone();
    let c_queue = c.clone();
    let c_surface = c.clone();
    let c_destroy = c.clone();
    let destroyed = device_handle.clone();
    Ok(DriverDevice {
        device_handle,
        capabilities,
        allocated_size: Box::new(move || {
            c_size.borrow().memory.values().map(|bytes| bytes.borrow().len() as i64).sum()
        }),
        recommended_working_set: Box::new(|| 8 * 1024 * 1024 * 1024),
        create_buffer: Box::new(move |_memory, d| resource(&c_buffer, "buffer", d.size)),
        create_mapped_buffer: Box::new(move |d| create_mapped_buffer(&c_mapped, d)),
        create_texture: Box::new(move |d| create_texture(&c_texture, d)),
        create_depth_texture: Box::new(move |_| create_depth_texture(&c_depth)),
        create_memoryless_texture: Box::new(|_, _| {
            fail(
                "Backend_mock.create_memoryless_texture",
                ErrorKind::Unsupported,
                "the mock has no memoryless attachments",
            )
        }),
        create_library: Box::new(move |_| create_library(&c_library)),
        create_accel: Box::new(|_| {
            fail("Backend_mock.create_accel", ErrorKind::Unsupported, "mock backend has no ray tracing")
        }),
        // The mock mirrors Metal's documented instance record layouts.
        instance_layout: Box::new(|kind| match kind {
            InstanceKind::UserIdInstances => vec![68, 0, 48, 52, 60, 64, -1, -1, -1, -1, -1, -1],
            InstanceKind::MotionInstances => vec![44, -1, 8, 12, 20, 24, 0, 4, 28, 32, 36, 40],
        }),
        create_sampler: Box::new(move |_| create_sampler(&c_sampler)),
        create_render_pipeline: Box::new(|_, _| {
            fail("Backend_mock.create_render_pipeline", ErrorKind::Unsupported, "mock backend does not rasterize")
        }),
        create_icb: Box::new(|_| {
            fail("Backend_mock.create_icb", ErrorKind::Unsupported, "mock backend does not rasterize")
        }),
        create_argument: Box::new(|_, _, _| {
            fail("Backend_mock.create_argument", ErrorKind::Unsupported, "mock backend does not rasterize")
        }),
        create_queue: Box::new(move || create_queue(&c_queue)),
        create_surface: Box::new(move |_| create_surface(&c_surface)),
        destroy_device: Box::new(move || {
            handle::destroy_device(&destroyed);
            add(&c_destroy, "destroy-device".to_owned());
            Ok(())
        }),
    })
}

pub fn create() -> (Driver, Control) {
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
    let driver = Driver { create_device: Box::new({ let c = c.clone(); move || create_device(&c, capabilities) }) };
    (driver, Control(c))
}

/// (buffers, textures, pipelines, queues, surfaces)
pub fn live_counts(control: &Control) -> (i64, i64, i64, i64, i64) {
    let s = control.0.borrow();
    (s.buffers, s.textures, s.pipelines, s.queues, s.surfaces)
}

/// Recorded operations, oldest first (the OCaml control keeps them newest first, unread).
pub fn trace(control: &Control) -> Vec<String> {
    control.0.borrow().trace.clone()
}
