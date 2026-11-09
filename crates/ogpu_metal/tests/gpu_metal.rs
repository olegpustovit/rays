// Port of crates/metal/test_metal_{device_safe,device_queue_safe,device_library_safe,command_buffer_safe,
// blit_safe,compute_pipeline_safe}.ml on the real Metal device. Each case runs in its own process
// (rays-t passes --exact), as the OCaml suite does, since they count process-wide handles.
use ogpu_metal::native as m;
use ogpu_core::error::{self, Error, ErrorKind};
use m::Format;
use m::pipeline::{IndexType, Primitive};
use crate::native_shader_type::{shader_data_type, ScalarType, ShaderDataType};
use m::*;

fn die(e: Error) -> ! { panic!("{}", error::to_string(&e)) }
fn reject(kind: ErrorKind, result: Option<Error>) {
    match result {
        Some(e) if e.kind == kind => {}
        Some(e) => panic!("expected {kind:?}, got {}", error::to_string(&e)),
        None => panic!("operation unexpectedly succeeded, expected {kind:?}"),
    }
}
fn stats() -> release_queue::Stats { release_queue::stats().unwrap_or_else(|failure| die(failure)) }
fn i32_bytes(values: &[i32]) -> Vec<u8> { values.iter().flat_map(|v| v.to_le_bytes()).collect() }
fn shared(device: &Device, length: u64) -> Buffer { Buffer::create(device, length, StorageMode::Shared, CpuCacheMode::DefaultCache, None).unwrap_or_else(|failure| die(failure)) }
fn settle(expected_live: usize) -> release_queue::Stats {
    for _ in 0..8 {
        release_queue::drain().unwrap_or_else(|failure| die(failure));
        let s = stats();
        if s.pending == 0 && s.live_handles == expected_live { return s; }
    }
    let s = stats();
    panic!("Metal handles did not settle ({} pending, {} live; expected {expected_live})", s.pending, s.live_handles);
}
// Every rejection in `operation` is decided before Metal is asked for an object.
fn without_handles(name: &str, operation: &mut dyn FnMut()) {
    release_queue::drain().unwrap_or_else(|failure| die(failure));
    let before = stats();
    operation();
    let after = stats();
    assert!(after.total_created == before.total_created && after.live_handles == before.live_handles, "{name} allocated a native handle");
}
const SOURCE_K: &str = "#include <metal_stdlib>\nusing namespace metal; kernel void k(uint i [[thread_position_in_grid]]) {}";

// The OCaml wrong-domain cases are prevented at the Rust ownership boundary:
// neither moving a handle nor sharing a reference may enter a foreign thread.
fn gpu_owner_only_handles() {
    trait AmbiguousIfSend<A> { fn check() {} }
    impl<T: ?Sized> AmbiguousIfSend<()> for T {}
    struct IsSend;
    impl<T: ?Sized + Send> AmbiguousIfSend<IsSend> for T {}
    trait AmbiguousIfSync<A> { fn check() {} }
    impl<T: ?Sized> AmbiguousIfSync<()> for T {}
    struct IsSync;
    impl<T: ?Sized + Sync> AmbiguousIfSync<IsSync> for T {}
    macro_rules! owner_only {
        ($($handle:ty),+ $(,)?) => {$(
            let _ = <$handle as AmbiguousIfSend<_>>::check;
            let _ = <$handle as AmbiguousIfSync<_>>::check;
        )+};
    }
    owner_only!(Device, Buffer, MappedBuffer, Texture, Sampler, Library, Function, Compiler,
        CommandQueue, CommandBuffer, ComputePipeline, ComputeEncoder, BlitEncoder,
        RenderPassDescriptor, RenderPipeline, RenderEncoder, ShaderArgumentEncoder,
        AccelerationStructure, AccelerationEncoder, metal_sys::Handle);
}

fn gpu_device_safe() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let before = stats();
    let info = device.info().unwrap_or_else(|failure| die(failure));
    assert_eq!(stats().total_created, before.total_created, "Device.info allocated a native handle");
    assert!(info.max_threadgroup_memory_length > 0, "device maximum threadgroup-memory length is not positive");
    if info.name == "Apple M1" { assert_eq!(info.max_threadgroup_memory_length, 32_768); }
    assert!(!info.name.is_empty() && info.registry_id != 0, "default device identity is incomplete");
    assert!(info.max_buffer_length >= 16 && device.same(&device));
    // a buffer dropped without destroy is released by collection: one created, one released
    let c0 = stats();
    drop(shared(&device, 16));
    let s = settle(c0.live_handles);
    assert!(s.total_created - c0.total_created == 1 && s.total_released - c0.total_released == 1, "buffer finalization");
    // other threads are refused: the calls need the platform main thread
    let off = std::thread::spawn(|| Device::system_default().map(|_| ())).join().unwrap();
    reject(ErrorKind::WrongDomain, off.err());
    let buffer = Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::DefaultCache, Some("Metal conformance values")).unwrap_or_else(|failure| die(failure));
    assert_eq!(buffer.length(), 16);
    let values = i32_bytes(&[1, 41, 99, -2]);
    buffer.write_bytes(0, &values).unwrap_or_else(|failure| die(failure));
    assert_eq!(buffer.read_bytes(0, 16).unwrap_or_else(|failure| die(failure)), values);
    assert_eq!(buffer.read_bytes(4, 4).unwrap_or_else(|failure| die(failure)), i32_bytes(&[41]), "a buffer range read the wrong offset");
    reject(ErrorKind::InvalidState, device.destroy().err());
    without_handles("invalid buffer labels", &mut || {
        reject(ErrorKind::InvalidArgument, Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::DefaultCache, Some("invalid\0label")).err());
    });
    let private = Buffer::create(&device, 16, StorageMode::Private, CpuCacheMode::DefaultCache, None).unwrap_or_else(|failure| die(failure));
    private.destroy().unwrap_or_else(|failure| die(failure));
    let configured = Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::WriteCombined, None).unwrap_or_else(|failure| die(failure));
    configured.write_bytes(0, &values).unwrap_or_else(|failure| die(failure));
    assert_eq!(configured.read_bytes(0, 16).unwrap_or_else(|failure| die(failure)), values, "write-combined buffer bytes did not round-trip");
    configured.destroy().unwrap_or_else(|failure| die(failure));
    // a mapped buffer: the host view is the memory the buffer reads
    let (mapped, mut floats) = Buffer::create_mapped(&device, 16, None).unwrap_or_else(|failure| die(failure));
    assert!(floats.len() == 4 && mapped.length() >= 16);
    floats.as_mut_slice()[1] = 2.5;
    assert_eq!(mapped.read_bytes(4, 4).unwrap_or_else(|failure| die(failure)), 2.5f32.to_bits().to_le_bytes());
    mapped.write_bytes(8, &7f32.to_bits().to_le_bytes()).unwrap_or_else(|failure| die(failure));
    assert_eq!(floats.as_slice()[2], 7.0, "mapped buffer write is not in its view");
    mapped.destroy().unwrap_or_else(|failure| die(failure));
    assert_eq!(floats.as_slice()[1], 2.5, "mapped view changed after destroy");
    reject(ErrorKind::InvalidArgument, Buffer::create_mapped(&device, 6, None).err());
    reject(ErrorKind::InvalidArgument, device.supports_texture_sample_count(0).err());
    assert!(device.supports_texture_sample_count(1).unwrap_or_else(|failure| die(failure)), "device refuses single-sample textures");
    buffer.destroy().unwrap_or_else(|failure| die(failure));
    buffer.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::StaleHandle, device.info().err());
    reject(ErrorKind::StaleHandle, Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::DefaultCache, None).err());
    drop((floats, mapped, buffer, private, configured, device));
    let fin = settle(0);
    assert!(fin.pending == 0 && fin.external_deallocation_mismatches == 0);
}

fn gpu_device_queue() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_device_library() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidArgument, Library::compile_source(&device, "", None).err());
    reject(ErrorKind::InvalidArgument, Library::compile_source(&device, "kernel void k() {}\0", None).err());
    reject(ErrorKind::InvalidArgument, Library::compile_source(&device, "kernel void k() {}", Some("a\0b")).err());
    reject(ErrorKind::DeviceLost, Library::compile_source(&device, "this is not Metal", None).err());
    let library = Library::compile_source(&device, SOURCE_K, Some("source_library")).unwrap_or_else(|failure| die(failure));
    assert!(Function::find(&library, "missing").is_err(), "a function the library does not define was found");
    let function = Function::find(&library, "k").unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, library.destroy().err());
    function.destroy().unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_command_buffer() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let command = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let blit = BlitEncoder::create(&command).unwrap_or_else(|failure| die(failure));
    blit.end_encoding().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, queue.destroy().err());
    command.commit().unwrap_or_else(|failure| die(failure));
    command.wait_until_completed().unwrap_or_else(|failure| die(failure));
    command.destroy().unwrap_or_else(|failure| die(failure));
    release_queue::drain().unwrap_or_else(|failure| die(failure));
    let before = stats().live_handles;
    {
        let command = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
        command.commit().unwrap_or_else(|failure| die(failure));
        command.wait_until_completed().unwrap_or_else(|failure| die(failure));
    }
    release_queue::drain().unwrap_or_else(|failure| die(failure));
    assert_eq!(stats().live_handles, before, "abandoned command buffer retained a native handle");
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_blit() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let commands = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let blit = BlitEncoder::create(&commands).unwrap_or_else(|failure| die(failure));
    let (source, destination) = (shared(&device, 64), shared(&device, 64));
    blit.end_encoding().unwrap_or_else(|failure| die(failure));
    commands.commit().unwrap_or_else(|failure| die(failure));
    commands.wait_until_completed().unwrap_or_else(|failure| die(failure));
    commands.destroy().unwrap_or_else(|failure| die(failure));
    destination.destroy().unwrap_or_else(|failure| die(failure));
    source.destroy().unwrap_or_else(|failure| die(failure));
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_compute_pipeline() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let source = "#include <metal_stdlib>\nusing namespace metal; kernel void increment_output(device uint *out [[buffer(0)]], uint i [[thread_position_in_grid]]) { out[i] = i + 7; }";
    let library = Library::compile_source(&device, source, None).unwrap_or_else(|failure| die(failure));
    let function = Function::find(&library, "increment_output").unwrap_or_else(|failure| die(failure));
    let pipeline = ComputePipeline::create(&function, None, true).unwrap_or_else(|failure| die(failure));
    assert!(pipeline.bindings().is_some_and(|b| !b.is_empty()), "missing immutable reflection snapshot");
    let plain = ComputePipeline::create(&function, None, false).unwrap_or_else(|failure| die(failure));
    assert!(plain.bindings().is_none(), "reflection was kept without being asked for");
    plain.destroy().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let buffer = shared(&device, 16);
    let command = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = ComputeEncoder::create(&command).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&pipeline).unwrap_or_else(|failure| die(failure));
    encoder.set_buffer(0, 0, &buffer).unwrap_or_else(|failure| die(failure));
    encoder.dispatch_threads((4, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    encoder.end_encoding().unwrap_or_else(|failure| die(failure));
    command.commit().unwrap_or_else(|failure| die(failure));
    command.wait_until_completed().unwrap_or_else(|failure| die(failure));
    assert_eq!(buffer.read_bytes(0, 16).unwrap_or_else(|failure| die(failure)), i32_bytes(&[7, 8, 9, 10]), "compute readback mismatch");
    command.destroy().unwrap_or_else(|failure| die(failure));
    buffer.destroy().unwrap_or_else(|failure| die(failure));
    pipeline.destroy().unwrap_or_else(|failure| die(failure));
    function.destroy().unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_compute_encoder() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let source = "#include <metal_stdlib>\nusing namespace metal;\nkernel void add_step(device uint *out [[buffer(0)]], constant uint &step [[buffer(1)]],\n    uint i [[thread_position_in_grid]]) { out[i] += step; }\n";
    let library = Library::compile_source(&device, source, None).unwrap_or_else(|failure| die(failure));
    let function = Function::find(&library, "add_step").unwrap_or_else(|failure| die(failure));
    let pipeline = ComputePipeline::create(&function, None, false).unwrap_or_else(|failure| die(failure));
    let buffer = shared(&device, 16);
    buffer.write_bytes(0, &i32_bytes(&[0, 10, 20, 30])).unwrap_or_else(|failure| die(failure));
    let arguments = shared(&device, 16);
    arguments.write_bytes(0, &i32_bytes(&[4, 1, 1, 0])).unwrap_or_else(|failure| die(failure));
    let texture = Texture::create(&device, Texture::descriptor_2d(m::Format::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)).unwrap_or_else(|failure| die(failure));
    let command = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = ComputeEncoder::create(&command).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&pipeline).unwrap_or_else(|failure| die(failure));
    encoder.set_buffer(0, 0, &buffer).unwrap_or_else(|failure| die(failure));
    encoder.set_bytes(1, &3i32.to_le_bytes()).unwrap_or_else(|failure| die(failure));
    encoder.set_texture(0, &texture).unwrap_or_else(|failure| die(failure));
    encoder.dispatch_threads((4, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    // the same again, its four threadgroups of one read from a buffer
    encoder.dispatch_threadgroups_indirect(&arguments, 0, (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    encoder.end_encoding().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, buffer.destroy().err());
    reject(ErrorKind::InvalidState, texture.destroy().err());
    command.commit().unwrap_or_else(|failure| die(failure));
    command.wait_until_completed().unwrap_or_else(|failure| die(failure));
    command.destroy().unwrap_or_else(|failure| die(failure));
    assert_eq!(buffer.read_bytes(0, 16).unwrap_or_else(|failure| die(failure)), i32_bytes(&[6, 16, 26, 36]), "compute output mismatch");
    buffer.destroy().unwrap_or_else(|failure| die(failure));
    arguments.destroy().unwrap_or_else(|failure| die(failure));
    texture.destroy().unwrap_or_else(|failure| die(failure));
    pipeline.destroy().unwrap_or_else(|failure| die(failure));
    function.destroy().unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_render_encoder() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let pass = RenderPassDescriptor::create(&device, 4, 4, 1).unwrap_or_else(|failure| die(failure));
    let color = Texture::create(&device, Texture::descriptor_2d(Format::Bgra8Unorm, 4, 4, StorageMode::Private, &[TextureUsage::RenderTarget], None)).unwrap_or_else(|failure| die(failure));
    let depth = Texture::create(&device, Texture::descriptor_2d(Format::Depth32Float, 4, 4, StorageMode::Private, &[TextureUsage::RenderTarget], None)).unwrap_or_else(|failure| die(failure));
    pass.set_attachments(&color, Some(&depth), None, [0.0; 4]).unwrap_or_else(|failure| die(failure));
    let actions = |clear_depth, clear_stencil| pass.set_actions(LoadAction::Clear, StoreAction::Store, LoadAction::Load, StoreAction::Store, clear_depth, LoadAction::Clear, StoreAction::DontCare, clear_stencil);
    actions(0.5, 0).unwrap_or_else(|failure| die(failure));
    without_handles("invalid depth and stencil clears", &mut || {
        reject(ErrorKind::InvalidArgument, actions(1.5, 0).err());
        reject(ErrorKind::InvalidArgument, actions(0.0, 256).err());
        reject(ErrorKind::InvalidArgument, actions(f64::NAN, 0).err());
        reject(ErrorKind::InvalidArgument, actions(-0.1, 0).err());
        reject(ErrorKind::InvalidArgument, pass.set_actions(LoadAction::Clear, StoreAction::Store, LoadAction::Load, StoreAction::MultisampleResolve, 0.5, LoadAction::Clear, StoreAction::Store, 0).err());
        reject(ErrorKind::InvalidArgument, pass.set_actions(LoadAction::Clear, StoreAction::Store, LoadAction::Load, StoreAction::Store, 0.5, LoadAction::Clear, StoreAction::StoreAndMultisampleResolve, 0).err());
    });
    pass.destroy().unwrap_or_else(|failure| die(failure));
    depth.destroy().unwrap_or_else(|failure| die(failure));
    color.destroy().unwrap_or_else(|failure| die(failure));

    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let target = Texture::create(&device, Texture::descriptor_2d(Format::Bgra8Unorm, 8, 8, StorageMode::Shared, &[TextureUsage::RenderTarget], None)).unwrap_or_else(|failure| die(failure));
    let pass = RenderPassDescriptor::create(&device, 8, 8, 1).unwrap_or_else(|failure| die(failure));
    pass.set_attachments(&target, None, None, [0.0; 4]).unwrap_or_else(|failure| die(failure));
    let commands = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = RenderEncoder::create(&commands, &pass).unwrap_or_else(|failure| die(failure));
    let sampled = Texture::create(&device, Texture::descriptor_2d(Format::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)).unwrap_or_else(|failure| die(failure));
    let viewport = Viewport { x: 0.0, y: 0.0, width: 8.0, height: 8.0, znear: 0.0, zfar: 1.0 };
    encoder.set_viewport(Viewport { x: -2.0, width: 11.0, ..viewport }).unwrap_or_else(|failure| die(failure));
    for invalid in [
        Viewport { width: 0.0, ..viewport },
        Viewport { height: -1.0, ..viewport },
        Viewport { x: f64::NAN, ..viewport },
        Viewport { zfar: 1.5, ..viewport },
        Viewport { znear: 0.75, zfar: 0.25, ..viewport },
    ] {
        reject(ErrorKind::InvalidArgument, encoder.set_viewport(invalid).err());
    }
    encoder.end_encoding().unwrap_or_else(|failure| die(failure));
    complete(&commands);
    pass.destroy().unwrap_or_else(|failure| die(failure));
    sampled.destroy().unwrap_or_else(|failure| die(failure));
    target.destroy().unwrap_or_else(|failure| die(failure));
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

// Rejections that are decided before Metal is asked for anything: no handle is created or released.
fn gpu_resource_bounds() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let info = device.info().unwrap_or_else(|failure| die(failure));
    let reject_without_native_work = |name: &str, operation: &dyn Fn() -> Result<(), Error>| {
        release_queue::drain().unwrap_or_else(|failure| die(failure));
        let before = stats();
        match operation() {
            Err(e) if e.kind == ErrorKind::InvalidArgument => {}
            Err(e) => panic!("{name} returned {}, expected InvalidArgument", error::to_string(&e)),
            Ok(()) => panic!("{name} accepted a malformed range"),
        }
        assert_eq!(before, stats(), "{name} crossed the native boundary");
    };
    reject_without_native_work("negative buffer length bits", &|| Buffer::create(&device, u64::MAX, StorageMode::Shared, CpuCacheMode::DefaultCache, None).map(|_| ()));
    reject_without_native_work("oversized buffer length", &|| Buffer::create(&device, info.max_buffer_length + 1, StorageMode::Shared, CpuCacheMode::DefaultCache, None).map(|_| ()));
    let buffer = shared(&device, 32);
    let bytes = [1u8; 16];
    buffer.write_bytes(16, &bytes).unwrap_or_else(|failure| die(failure));
    let mut expected = vec![0u8; 16];
    expected.extend_from_slice(&bytes);
    assert_eq!(buffer.read_bytes(0, 32).unwrap_or_else(|failure| die(failure)), expected, "an admitted write did not land at its offset");
    assert_eq!(buffer.read_bytes(31, 1).unwrap_or_else(|failure| die(failure)), vec![1u8], "the last byte was not readable");
    buffer.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
    release_queue::drain().unwrap_or_else(|failure| die(failure));
}

fn gpu_sampler() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let descriptor = SamplerDescriptor {
        min_filter: Filter::Linear, mag_filter: Filter::Linear, mip_filter: MipFilter::LinearMip, max_anisotropy: 4,
        address_s: AddressMode::Repeat, address_t: AddressMode::Repeat, address_r: AddressMode::Repeat,
        lod_max: 3.0, support_argument_buffers: true, label: Some("Metal linear sampler".to_owned()), ..SamplerDescriptor::default()
    };
    let c0 = stats();
    drop(Sampler::create(&device, descriptor.clone()).unwrap_or_else(|failure| die(failure)));
    let s = settle(c0.live_handles);
    assert!(s.total_created - c0.total_created == 1 && s.total_released - c0.total_released == 1, "sampler finalization");
    let sampler = Sampler::create(&device, descriptor.clone()).unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, device.destroy().err());
    let plain = Sampler::create(&device, SamplerDescriptor::default()).unwrap_or_else(|failure| die(failure));
    plain.destroy().unwrap_or_else(|failure| die(failure));
    // Reduction modes and LOD bias are an Apple10 feature: made there, refused before any allocation elsewhere.
    for (reduction_mode, lod_bias) in [(ReductionMode::Minimum, -16.0), (ReductionMode::Maximum, 15.999), (ReductionMode::Minimum, 0.5), (ReductionMode::WeightedAverage, 0.5)] {
        let advanced = SamplerDescriptor { reduction_mode, lod_bias, label: Some("Metal reduction sampler".to_owned()), ..descriptor.clone() };
        match Sampler::create(&device, advanced.clone()) {
            Ok(made) => {
                assert!(device.supports_family(DeviceFamily::Apple10).unwrap_or_else(|failure| die(failure)), "sampler reduction support escaped its Apple10 hardware gate");
                made.destroy().unwrap_or_else(|failure| die(failure));
            }
            Err(e) if e.kind == ErrorKind::Unsupported => {
                without_handles("unsupported sampler extensions", &mut || reject(ErrorKind::Unsupported, Sampler::create(&device, advanced.clone()).err()));
            }
            Err(e) => panic!("{}", error::to_string(&e)),
        }
    }
    without_handles("invalid sampler inputs", &mut || {
        let d = || descriptor.clone();
        for invalid in [
            SamplerDescriptor { max_anisotropy: 0, ..d() }, SamplerDescriptor { max_anisotropy: 17, ..d() },
            SamplerDescriptor { lod_min: f32::NAN, ..d() }, SamplerDescriptor { lod_min: -1.0, ..d() },
            SamplerDescriptor { lod_bias: f32::NAN, ..d() }, SamplerDescriptor { lod_bias: -16.001, ..d() }, SamplerDescriptor { lod_bias: 16.0, ..d() },
            SamplerDescriptor { normalized_coordinates: false, address_s: AddressMode::Repeat, ..d() },
            SamplerDescriptor { lod_min: 2.0, lod_max: 1.0, ..d() },
            SamplerDescriptor { label: Some("invalid\0label".to_owned()), ..d() },
        ] {
            reject(ErrorKind::InvalidArgument, Sampler::create(&device, invalid).err());
        }
    });
    // Pixel coordinates are accepted with the one addressing they allow.
    let unnormalized = Sampler::create(&device, SamplerDescriptor { normalized_coordinates: false, ..SamplerDescriptor::default() }).unwrap_or_else(|failure| die(failure));
    unnormalized.destroy().unwrap_or_else(|failure| die(failure));
    sampler.destroy().unwrap_or_else(|failure| die(failure));
    sampler.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::StaleHandle, Sampler::create(&device, descriptor).err());
    drop((sampler, plain, unnormalized, device));
    settle(0);
}


// Port of lib/metal/test_metal_texture_safe.ml, one case per former section.
fn full_region(width: usize, height: usize) -> TextureRegion { TextureRegion { x: 0, y: 0, z: 0, width, height, depth: 1 } }
fn complete(commands: &CommandBuffer) {
    commands.commit().unwrap_or_else(|failure| die(failure));
    commands.wait_until_completed().unwrap_or_else(|failure| die(failure));
    assert!(matches!(commands.status().unwrap_or_else(|failure| die(failure)), CommandBufferStatus::Completed), "command buffer did not complete");
    commands.destroy().unwrap_or_else(|failure| die(failure));
}
// Texture contents arrive through a staging buffer and a blit (the one way to write a texture here).
fn upload(device: &Device, queue: &CommandQueue, texture: &Texture, level: usize, region: TextureRegion, bytes_per_row: usize, bytes: &[u8]) {
    let staging = shared(device, bytes.len() as u64);
    staging.write_bytes(0, bytes).unwrap_or_else(|failure| die(failure));
    let commands = CommandBuffer::create(queue).unwrap_or_else(|failure| die(failure));
    let blit = BlitEncoder::create(&commands).unwrap_or_else(|failure| die(failure));
    blit.copy_buffer_to_texture(&staging, 0, bytes_per_row, bytes.len(), texture, 0, level, region).unwrap_or_else(|failure| die(failure));
    blit.end_encoding().unwrap_or_else(|failure| die(failure));
    complete(&commands);
    staging.destroy().unwrap_or_else(|failure| die(failure));
}
const TEXEL_SHADER_SOURCE: &str = r#"
#include <metal_stdlib>
using namespace metal;
kernel void texel_read(texture2d<float, access::read> source [[texture(0)]],
                       device uint *result [[buffer(0)]]) {
  result[0] = uint(round(source.read(uint2(0u, 0u)).r * 255.0f));
}
kernel void read_swizzle(texture2d<float, access::sample> source [[texture(0)]],
                         device uint4 *result [[buffer(0)]]) {
  constexpr sampler nearest_sampler(coord::pixel, filter::nearest);
  result[0] = uint4(round(source.sample(nearest_sampler, float2(0.5f)) * 255.0f));
}
"#;

fn gpu_texture_formats() {
    use m::Format as F;
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let formats = [F::R8Unorm, F::Rgba8Unorm, F::Bgra8Unorm, F::Bgra8UnormSrgb, F::Rgba16Float, F::Rgba32Float, F::Depth32Float];
    let codes: std::collections::HashSet<u32> = formats.iter().map(|format| m::pixel_format_code(*format)).collect();
    assert_eq!(codes.len(), 7, "Metal/OGPU format inventory drift");
    for format in formats {
        match Texture::create(&device, Texture::descriptor_2d(format, 4, 4, StorageMode::Shared, &[TextureUsage::ShaderRead], None)) {
            Ok(texture) => {
                assert_eq!(texture.descriptor().format, format, "Metal changed a retained pixel format");
                texture.destroy().unwrap_or_else(|failure| die(failure));
            }
            Err(e) if matches!(e.kind, ErrorKind::DeviceLost | ErrorKind::Unsupported) => {}
            Err(e) => panic!("{}", error::to_string(&e)),
        }
    }
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_texture_swizzle_compression() {
    use m::Format as F;
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let base_swizzle = Swizzle { red: SwizzleChannel::Red, green: SwizzleChannel::One, blue: SwizzleChannel::One, alpha: SwizzleChannel::Green };
    let swizzled = |storage: StorageMode, usage: &[TextureUsage], width: usize, height: usize| TextureDescriptor {
        swizzle: base_swizzle, ..Texture::descriptor_2d(F::Rgba8Unorm, width, height, storage, usage, None)
    };
    let texture = Texture::create(&device, swizzled(StorageMode::Shared, &[TextureUsage::ShaderRead], 1, 1)).unwrap_or_else(|failure| die(failure));
    assert_eq!(texture.descriptor().swizzle, base_swizzle, "base texture swizzle did not round-trip");
    upload(&device, &queue, &texture, 0, full_region(1, 1), 4, &[10, 20, 30, 40]);
    let plain = Texture::create(&device, Texture::descriptor_2d(F::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)).unwrap_or_else(|failure| die(failure));
    upload(&device, &queue, &plain, 0, full_region(1, 1), 4, &[10, 20, 30, 40]);
    let output = shared(&device, 16);
    let library = Library::compile_source(&device, TEXEL_SHADER_SOURCE, None).unwrap_or_else(|failure| die(failure));
    let function = Function::find(&library, "read_swizzle").unwrap_or_else(|failure| die(failure));
    let pipeline = ComputePipeline::create(&function, None, false).unwrap_or_else(|failure| die(failure));
    for (name, source, expected) in [("identity", &plain, [10i32, 20, 30, 40]), ("base", &texture, [10, 255, 255, 20])] {
        let commands = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
        let encoder = ComputeEncoder::create(&commands).unwrap_or_else(|failure| die(failure));
        encoder.set_pipeline(&pipeline).unwrap_or_else(|failure| die(failure));
        encoder.set_texture(0, source).unwrap_or_else(|failure| die(failure));
        encoder.set_buffer(0, 0, &output).unwrap_or_else(|failure| die(failure));
        encoder.dispatch_threads((1, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
        encoder.end_encoding().unwrap_or_else(|failure| die(failure));
        complete(&commands);
        assert_eq!(output.read_bytes(0, 16).unwrap_or_else(|failure| die(failure)), i32_bytes(&expected), "{name} texture swizzle");
    }
    pipeline.destroy().unwrap_or_else(|failure| die(failure));
    function.destroy().unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    output.destroy().unwrap_or_else(|failure| die(failure));
    plain.destroy().unwrap_or_else(|failure| die(failure));
    texture.destroy().unwrap_or_else(|failure| die(failure));
    // A memoryless texture is a render target, a buffer is not memoryless.
    let memoryless = Texture::descriptor_2d(F::Depth32Float, 4, 4, StorageMode::Memoryless, &[TextureUsage::RenderTarget], None);
    if device.supports_family(DeviceFamily::Apple7).unwrap_or_else(|failure| die(failure)) {
        let made = Texture::create(&device, memoryless.clone()).unwrap_or_else(|failure| die(failure));
        made.destroy().unwrap_or_else(|failure| die(failure));
    }
    without_handles("a memoryless texture is a render target, a buffer is not memoryless", &mut || {
        reject(ErrorKind::InvalidArgument, Texture::create(&device, TextureDescriptor { usage: vec![TextureUsage::ShaderRead], ..memoryless.clone() }).err());
        reject(ErrorKind::InvalidArgument, Buffer::create(&device, 16, StorageMode::Memoryless, CpuCacheMode::DefaultCache, None).err());
    });
    without_handles("invalid writable swizzles", &mut || {
        for usage in [TextureUsage::ShaderWrite, TextureUsage::ShaderAtomic] {
            let descriptor = TextureDescriptor { swizzle: base_swizzle, ..Texture::descriptor_2d(F::Rgba8Unorm, 4, 4, StorageMode::Private, &[usage], None) };
            reject(ErrorKind::InvalidArgument, Texture::create(&device, descriptor).err());
        }
    });
    let lossy = TextureDescriptor {
        compression: CompressionType::Lossy,
        ..Texture::descriptor_2d(F::Rgba8Unorm, 64, 64, StorageMode::Private, &[TextureUsage::RenderTarget], None)
    };
    without_handles("invalid lossy texture descriptors", &mut || {
        let invalid = [
            TextureDescriptor { storage: StorageMode::Shared, ..lossy.clone() },
            TextureDescriptor { allow_gpu_optimized_contents: false, ..lossy.clone() },
            TextureDescriptor { usage: vec![TextureUsage::PixelFormatView], ..lossy.clone() },
            TextureDescriptor { usage: vec![TextureUsage::ShaderWrite], ..lossy.clone() },
            TextureDescriptor { usage: vec![TextureUsage::ShaderAtomic], ..lossy.clone() },
            TextureDescriptor { kind: TextureKind::Texture1d, height: 1, ..lossy.clone() },
        ];
        for descriptor in invalid { reject(ErrorKind::InvalidArgument, Texture::create(&device, descriptor).err()); }
    });
    without_handles("lossy compression is unsupported for retained formats", &mut || {
        reject(ErrorKind::InvalidArgument, Texture::create(&device, lossy.clone()).err());
    });
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

fn gpu_texture_transfer_descriptors() {
    use m::Format as F;
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let base = Texture::descriptor_2d(F::Rgba8Unorm, 4, 4, StorageMode::Shared, &[TextureUsage::ShaderRead, TextureUsage::PixelFormatView], Some("Metal conformance texture"));
    let texture_descriptor = TextureDescriptor { mip_levels: 3, ..base };
    let c0 = stats();
    drop(Texture::create(&device, texture_descriptor.clone()).unwrap_or_else(|failure| die(failure)));
    let s = settle(c0.live_handles);
    assert!(s.total_created - c0.total_created == 1 && s.total_released - c0.total_released == 1, "texture finalization");
    let texture = Texture::create(&device, texture_descriptor.clone()).unwrap_or_else(|failure| die(failure));
    assert_eq!(texture.descriptor().mip_levels, 3, "2D texture mip cardinality is wrong");
    assert_eq!(texture.descriptor().hazard_tracking, HazardTrackingMode::Tracked, "direct texture resource properties are wrong");
    assert_eq!(texture.descriptor().label.as_deref(), Some("Metal conformance texture"), "texture label did not round-trip");
    assert!(texture.device().same(&device) && !texture.is_destroyed(), "texture identity is wrong");
    reject(ErrorKind::InvalidState, device.destroy().err());
    // Rows of sixteen bytes carried at a pitch of twenty: the pad never reaches the texture and reads back as zero.
    let mut texture_bytes = vec![0xeeu8; 80];
    for row in 0..4 {
        for column_byte in 0..16 { texture_bytes[row * 20 + column_byte] = (row * 16 + column_byte) as u8; }
    }
    let full = full_region(4, 4);
    upload(&device, &queue, &texture, 0, full, 20, &texture_bytes);
    let texture_copy = texture.read_bytes(full, 0, 0, 20, 80).unwrap_or_else(|failure| die(failure));
    for row in 0..4 {
        for column_byte in 0..16 {
            let offset = row * 20 + column_byte;
            assert_eq!(texture_copy[offset], texture_bytes[offset], "texture byte transfer changed active pixel data");
        }
        for padding in 16..20 {
            assert_eq!(texture_copy[row * 20 + padding], 0, "texture read exposed uninitialized row padding");
        }
    }
    let mut into = vec![0xaau8; 64];
    texture.read_bytes_into(full, 0, 0, 16, 64, &mut into).unwrap_or_else(|failure| die(failure));
    for row in 0..4 {
        assert!(into[row * 16..row * 16 + 16] == texture_bytes[row * 20..row * 20 + 16], "texture read into caller storage changed pixel data");
    }
    reject(ErrorKind::InvalidArgument, texture.read_bytes_into(full, 0, 0, 16, 64, &mut vec![0u8; 63]).err());
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 0, 15, 60).err());
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 0, 20, 79).err());
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 0, usize::MAX, usize::MAX).err());
    reject(ErrorKind::InvalidArgument, texture.read_bytes(TextureRegion { x: 3, width: 2, ..full }, 0, 0, 8, 32).err());
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 3, 0, 16, 64).err());
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 1, 16, 64).err());
    // A mip level is its own upload and read; the base level stays as it was.
    let mip_region = full_region(2, 2);
    let mip_bytes: Vec<u8> = (0..16).map(|index| (index + 20) as u8).collect();
    upload(&device, &queue, &texture, 1, mip_region, 8, &mip_bytes);
    assert_eq!(texture.read_bytes(mip_region, 1, 0, 8, 16).unwrap_or_else(|failure| die(failure)), mip_bytes, "texture mip transfer did not round-trip");
    assert_eq!(texture.read_bytes(full, 0, 0, 16, 64).unwrap_or_else(|failure| die(failure))[0..16], texture_bytes[0..16], "a mip upload overwrote the base level");
    let private_texture = Texture::create(&device, Texture::descriptor_2d(F::Bgra8Unorm, 4, 4, StorageMode::Private, &[TextureUsage::RenderTarget], None)).unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::Unsupported, private_texture.read_bytes(full, 0, 0, 16, 64).err());
    private_texture.destroy().unwrap_or_else(|failure| die(failure));
    without_handles("invalid texture descriptors", &mut || {
        let invalid = [
            TextureDescriptor { width: 0, ..texture_descriptor.clone() },
            TextureDescriptor { mip_levels: 9, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::Texture1d, height: 2, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { sample_count: 2, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { usage: vec![TextureUsage::ShaderRead, TextureUsage::ShaderRead], ..texture_descriptor.clone() },
            TextureDescriptor { label: Some("invalid\0label".to_owned()), ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::TextureBuffer, height: 1, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::Texture2dArray, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::TextureCube, height: 2, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::Texture3d, array_length: 2, mip_levels: 1, ..texture_descriptor.clone() },
        ];
        for descriptor in invalid { reject(ErrorKind::InvalidArgument, Texture::create(&device, descriptor).err()); }
    });
    // Invalid UTF-8 labels cannot cross the safe API's String boundary.
    if device.supports_texture_sample_count(2).unwrap_or_else(|failure| die(failure)) {
        let multisample = Texture::create(&device, TextureDescriptor {
            kind: TextureKind::Texture2dMultisample, mip_levels: 1, sample_count: 2, storage: StorageMode::Private,
            usage: vec![TextureUsage::RenderTarget], label: None, ..texture_descriptor.clone()
        }).unwrap_or_else(|failure| die(failure));
        reject(ErrorKind::Unsupported, multisample.read_bytes(full, 0, 0, 16, 64).err());
        multisample.destroy().unwrap_or_else(|failure| die(failure));
    }
    // A cube has six slices and no seventh.
    let cube = Texture::create(&device, TextureDescriptor { kind: TextureKind::TextureCube, ..Texture::descriptor_2d(F::Rgba8Unorm, 4, 4, StorageMode::Shared, &[TextureUsage::ShaderRead], None) }).unwrap_or_else(|failure| die(failure));
    cube.read_bytes(full, 0, 5, 16, 64).unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidArgument, cube.read_bytes(full, 0, 6, 16, 64).err());
    cube.destroy().unwrap_or_else(|failure| die(failure));
    texture.destroy().unwrap_or_else(|failure| die(failure));
    texture.destroy().unwrap_or_else(|failure| die(failure));
    assert!(texture.is_destroyed(), "destroyed texture remained live");
    reject(ErrorKind::StaleHandle, texture.read_bytes(full, 0, 0, 16, 64).err());
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

// What the blit refuses, what it holds while encoded, and that a kernel then reads the copied texel.
fn gpu_texture_blit() {
    use m::Format as F;
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let texture = Texture::create(&device, Texture::descriptor_2d(F::R8Unorm, 4, 4, StorageMode::Private, &[TextureUsage::ShaderRead, TextureUsage::ShaderWrite], None)).unwrap_or_else(|failure| die(failure));
    let volume = Texture::create(&device, TextureDescriptor { kind: TextureKind::Texture3d, depth: 2, ..Texture::descriptor_2d(F::R8Unorm, 1, 1, StorageMode::Private, &[TextureUsage::ShaderRead], None) }).unwrap_or_else(|failure| die(failure));
    let mut staging_bytes = vec![0u8; 16];
    staging_bytes[0] = 37;
    let staging = shared(&device, 16);
    staging.write_bytes(0, &staging_bytes).unwrap_or_else(|failure| die(failure));
    let region = full_region(4, 4);
    let commands = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let blit = BlitEncoder::create(&commands).unwrap_or_else(|failure| die(failure));
    let copy = |bytes_per_row: usize, bytes_per_image: usize, destination: &Texture, slice: usize, level: usize, region: TextureRegion| {
        blit.copy_buffer_to_texture(&staging, 0, bytes_per_row, bytes_per_image, destination, slice, level, region)
    };
    without_handles("refused blits", &mut || {
        reject(ErrorKind::InvalidArgument, blit.copy_buffer_to_texture(&staging, u64::MAX, 4, 16, &texture, 0, 0, region).err());
        reject(ErrorKind::InvalidArgument, blit.copy_buffer_to_texture(&staging, 1, 4, 16, &texture, 0, 0, region).err());
        reject(ErrorKind::InvalidArgument, copy(1, usize::MAX, &volume, 0, 0, TextureRegion { depth: 2, ..full_region(1, 1) }).err());
        reject(ErrorKind::InvalidArgument, copy(3, 16, &texture, 0, 0, region).err());
        reject(ErrorKind::InvalidArgument, copy(4, 15, &texture, 0, 0, region).err());
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 0, 1, region).err());
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 1, 0, region).err());
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 0, 0, TextureRegion { x: 1, ..region }).err());
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 0, 0, TextureRegion { width: 0, ..region }).err());
    });
    copy(4, 16, &texture, 0, 0, region).unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, staging.destroy().err());
    reject(ErrorKind::InvalidState, texture.destroy().err());
    blit.end_encoding().unwrap_or_else(|failure| die(failure));
    complete(&commands);
    staging.destroy().unwrap_or_else(|failure| die(failure));
    let library = Library::compile_source(&device, TEXEL_SHADER_SOURCE, None).unwrap_or_else(|failure| die(failure));
    let read_function = Function::find(&library, "texel_read").unwrap_or_else(|failure| die(failure));
    let read_pipeline = ComputePipeline::create(&read_function, None, false).unwrap_or_else(|failure| die(failure));
    let output = shared(&device, 4);
    let commands = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = ComputeEncoder::create(&commands).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&read_pipeline).unwrap_or_else(|failure| die(failure));
    encoder.set_texture(0, &texture).unwrap_or_else(|failure| die(failure));
    encoder.set_buffer(0, 0, &output).unwrap_or_else(|failure| die(failure));
    encoder.dispatch_threads((1, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    encoder.end_encoding().unwrap_or_else(|failure| die(failure));
    complete(&commands);
    let value = i32::from_le_bytes(output.read_bytes(0, 4).unwrap_or_else(|failure| die(failure)).try_into().unwrap());
    assert_eq!(value, 37, "blitted texel read back as {value}, expected 37");
    output.destroy().unwrap_or_else(|failure| die(failure));
    read_pipeline.destroy().unwrap_or_else(|failure| die(failure));
    read_function.destroy().unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    volume.destroy().unwrap_or_else(|failure| die(failure));
    texture.destroy().unwrap_or_else(|failure| die(failure));
    queue.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

// Function constants: typed values, range refusals, reflection on request, and each specialization's own values.
const CONSTANTS_SOURCE: &str = r#"
#include <metal_stdlib>
using namespace metal;

kernel void increment(device uint *values [[buffer(0)]],
                      uint index [[thread_position_in_grid]]) {
  values[index] += 1;
}

constant uint increment_amount [[function_constant(0)]];
constant bool apply_twice [[function_constant(1)]];

kernel void specialized_increment(device uint *values [[buffer(0)]],
                                  uint index [[thread_position_in_grid]]) {
  values[index] += apply_twice ? increment_amount * 2u : increment_amount;
}

constant char scalar_i8 [[function_constant(2)]];
constant uchar scalar_u8 [[function_constant(3)]];
constant short scalar_i16 [[function_constant(4)]];
constant ushort scalar_u16 [[function_constant(5)]];
constant int scalar_i32 [[function_constant(6)]];
constant uint scalar_u32 [[function_constant(7)]];
constant long scalar_i64 [[function_constant(8)]];
constant ulong scalar_u64 [[function_constant(9)]];
constant half scalar_f16 [[function_constant(10)]];
constant float scalar_f32 [[function_constant(11)]];
constant bool scalar_bool [[function_constant(12)]];

kernel void scalar_constant_sum(device uint *result [[buffer(0)]]) {
  result[0] = uint(scalar_i8) + uint(scalar_u8) + uint(scalar_i16)
      + uint(scalar_u16) + uint(scalar_i32) + scalar_u32
      + uint(scalar_i64) + uint(scalar_u64) + uint(scalar_f16)
      + uint(scalar_f32) + uint(scalar_bool);
}

constant int first [[function_constant(13)]];
constant int second [[function_constant(14)]];

kernel void constant_copy(device int *out [[buffer(0)]]) {
  out[0] = first * 100 + second;
}
"#;

fn c(name: &str, value: FunctionConstantValue) -> (String, FunctionConstantValue) { (name.to_owned(), value) }

fn buffer_binding(binding: &ShaderBinding) -> Option<(String, u64, BindingAccess, ShaderDataType)> {
    match &binding.kind {
        BindingKind::Buffer { data_type, .. } => Some((binding.name.clone(), binding.index, binding.access, shader_data_type(i64::from(*data_type)))),
        _ => None,
    }
}
fn buffer_binding_or_fail(binding: &ShaderBinding) -> (String, u64, BindingAccess, ShaderDataType) {
    buffer_binding(binding).unwrap_or_else(|| panic!("render reflection reports {} as something other than a buffer", binding.name))
}

fn run_once(device: &Device, queue: &CommandQueue, function: &Function, initial: i32) -> i32 {
    let pipeline = ComputePipeline::create(function, None, false).unwrap_or_else(|failure| die(failure));
    let output = shared(device, 4);
    output.write_bytes(0, &initial.to_le_bytes()).unwrap_or_else(|failure| die(failure));
    let commands = CommandBuffer::create(queue).unwrap_or_else(|failure| die(failure));
    let encoder = ComputeEncoder::create(&commands).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&pipeline).unwrap_or_else(|failure| die(failure));
    encoder.set_buffer(0, 0, &output).unwrap_or_else(|failure| die(failure));
    encoder.dispatch_threads((1, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    encoder.end_encoding().unwrap_or_else(|failure| die(failure));
    complete(&commands);
    let result = i32::from_le_bytes(output.read_bytes(0, 4).unwrap_or_else(|failure| die(failure)).try_into().unwrap());
    output.destroy().unwrap_or_else(|failure| die(failure));
    pipeline.destroy().unwrap_or_else(|failure| die(failure));
    result
}

fn gpu_function_constants() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    match Library::compile_source(&device, "not a Metal program", Some("invalid shader diagnostic")) {
        Err(e) if e.kind == ErrorKind::DeviceLost && e.message.contains("invalid shader diagnostic") => {}
        Err(e) => panic!("shader compilation lost its full labeled diagnostic: {}", error::to_string(&e)),
        Ok(_) => panic!("an invalid shader compiled"),
    }
    let library = Library::compile_source(&device, CONSTANTS_SOURCE, Some("Metal conformance library")).unwrap_or_else(|failure| die(failure));
    let function_ = Function::find(&library, "increment").unwrap_or_else(|failure| die(failure));
    without_handles("invalid function lookups", &mut || {
        reject(ErrorKind::InvalidArgument, Function::find(&library, "").err());
        reject(ErrorKind::InvalidArgument, Function::find(&library, "bad\0name").err());
    });
    without_handles("invalid function constants", &mut || {
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(1)), c("increment_amount", FunctionConstantValue::Uint32(2))], "specialized_increment").err());
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(0x1_0000_0000))], "specialized_increment").err());
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("", FunctionConstantValue::Uint32(1))], "specialized_increment").err());
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[], "").err());
        for value in [FunctionConstantValue::Int8(128), FunctionConstantValue::Uint8(-1), FunctionConstantValue::Int16(32_768), FunctionConstantValue::Uint16(65_536)] {
            reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("scalar_i8", value)], "scalar_constant_sum").err());
        }
    });
    // A constant of the wrong type is Metal's to refuse.
    reject(ErrorKind::DeviceLost, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Bool(true))], "specialized_increment").err());
    let specialized_function = Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(7)), c("apply_twice", FunctionConstantValue::Bool(true))], "specialized_increment").unwrap_or_else(|failure| die(failure));
    let scalar_constant_function = Function::specialize(&library, &[
        c("scalar_i8", FunctionConstantValue::Int8(-2)), c("scalar_u8", FunctionConstantValue::Uint8(3)),
        c("scalar_i16", FunctionConstantValue::Int16(-4)), c("scalar_u16", FunctionConstantValue::Uint16(5)),
        c("scalar_i32", FunctionConstantValue::Int32(-6)), c("scalar_u32", FunctionConstantValue::Uint32(7)),
        c("scalar_i64", FunctionConstantValue::Int64(-8)), c("scalar_u64", FunctionConstantValue::Uint64Bits(9)),
        c("scalar_f16", FunctionConstantValue::Float16(10.0)), c("scalar_f32", FunctionConstantValue::Float32(11.0)),
        c("scalar_bool", FunctionConstantValue::Bool(true)),
    ], "scalar_constant_sum").unwrap_or_else(|failure| die(failure));
    let stale_function = Function::find(&library, "increment").unwrap_or_else(|failure| die(failure));
    stale_function.destroy().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::StaleHandle, ComputePipeline::create(&stale_function, None, false).err());
    reject(ErrorKind::StaleHandle, stale_function.argument_encoder(0).err());
    without_handles("invalid compute descriptors", &mut || {
        reject(ErrorKind::InvalidArgument, ComputePipeline::create(&specialized_function, Some("invalid\0label"), false).err());
    });
    reject(ErrorKind::InvalidState, library.destroy().err());
    let pipeline = ComputePipeline::create(&function_, None, false).unwrap_or_else(|failure| die(failure));
    let specialized_pipeline = ComputePipeline::create(&specialized_function, Some("specialized reflected pipeline"), true).unwrap_or_else(|failure| die(failure));
    let scalar_constant_pipeline = ComputePipeline::create(&scalar_constant_function, None, false).unwrap_or_else(|failure| die(failure));
    assert!(pipeline.bindings().is_none(), "reflection was kept without being asked for");
    let reflected = specialized_pipeline.bindings().unwrap_or_else(|| panic!("requested compute pipeline reflection is missing"));
    assert_eq!(
        reflected.iter().filter_map(buffer_binding).collect::<Vec<_>>(),
        vec![("values".to_owned(), 0, BindingAccess::ReadWrite, ShaderDataType::Scalar(ScalarType::Uint))],
        "compute reflection does not describe the one read-write uint buffer",
    );
    assert_eq!(reflected.len(), 1, "compute reflection invented a binding");
    let buffer = shared(&device, 16);
    buffer.write_bytes(0, &i32_bytes(&[1, 41, 99, -2])).unwrap_or_else(|failure| die(failure));
    let specialized_buffer = shared(&device, 4);
    specialized_buffer.write_bytes(0, &i32_bytes(&[10])).unwrap_or_else(|failure| die(failure));
    let scalar_constant_buffer = shared(&device, 4);
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let commands = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    assert!(commands.device().same(&device), "command buffer device drift");
    assert!(matches!(commands.status().unwrap_or_else(|failure| die(failure)), CommandBufferStatus::NotEnqueued), "a new command buffer is already enqueued");
    reject(ErrorKind::InvalidState, commands.wait_until_completed().err());
    let encoder = ComputeEncoder::create(&commands).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&pipeline).unwrap_or_else(|failure| die(failure));
    assert!(buffer.read_bytes(0, 16).unwrap_or_else(|failure| die(failure)) == i32_bytes(&[1, 41, 99, -2]), "compute input changed before command encoding");
    encoder.set_buffer(0, 0, &buffer).unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, buffer.destroy().err());
    encoder.dispatch_threads((4, 1, 1), (4, 1, 1)).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&specialized_pipeline).unwrap_or_else(|failure| die(failure));
    encoder.set_buffer(0, 0, &specialized_buffer).unwrap_or_else(|failure| die(failure));
    encoder.dispatch_threads((1, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&scalar_constant_pipeline).unwrap_or_else(|failure| die(failure));
    encoder.set_buffer(0, 0, &scalar_constant_buffer).unwrap_or_else(|failure| die(failure));
    encoder.dispatch_threads((1, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    encoder.end_encoding().unwrap_or_else(|failure| die(failure));
    commands.commit().unwrap_or_else(|failure| die(failure));
    commands.wait_until_completed().unwrap_or_else(|failure| die(failure));
    assert!(matches!(commands.status().unwrap_or_else(|failure| die(failure)), CommandBufferStatus::Completed), "command buffer did not complete");
    let output = buffer.read_bytes(0, 16).unwrap_or_else(|failure| die(failure));
    for (index, expected) in [2i32, 42, 100, -1].into_iter().enumerate() {
        let actual = i32::from_le_bytes(output[index * 4..index * 4 + 4].try_into().unwrap());
        assert_eq!(actual, expected, "compute output {index}: expected {expected}, got {actual}");
    }
    let specialized_value = i32::from_le_bytes(specialized_buffer.read_bytes(0, 4).unwrap_or_else(|failure| die(failure)).try_into().unwrap());
    assert_eq!(specialized_value, 24, "specialized function constants did not execute on the GPU");
    let scalar_value = i32::from_le_bytes(scalar_constant_buffer.read_bytes(0, 4).unwrap_or_else(|failure| die(failure)).try_into().unwrap());
    assert_eq!(scalar_value, 26, "typed scalar function constants did not round-trip to the GPU");
    scalar_constant_buffer.destroy().unwrap_or_else(|failure| die(failure));
    specialized_buffer.destroy().unwrap_or_else(|failure| die(failure));
    buffer.destroy().unwrap_or_else(|failure| die(failure));
    commands.destroy().unwrap_or_else(|failure| die(failure));
    assert!(commands.destroyed(), "destroyed command buffer remained live");
    // The values a constant carries are the ones passed, each call its own: 7 and 11 give 711, and a later specialization does not see them.
    let copy = |first: i32, second: i32| -> i32 {
        let specialized = Function::specialize(&library, &[c("first", FunctionConstantValue::Int32(first)), c("second", FunctionConstantValue::Int32(second))], "constant_copy").unwrap_or_else(|failure| die(failure));
        let result = run_once(&device, &queue, &specialized, -1);
        specialized.destroy().unwrap_or_else(|failure| die(failure));
        result
    };
    assert_eq!(copy(7, 11), 711, "function constants did not copy the original typed values");
    assert_eq!(copy(0, 0), 0, "a later specialization kept stale function constant values");
    assert_eq!(copy(-3, 5), -295, "signed function constants lost their sign");
    queue.destroy().unwrap_or_else(|failure| die(failure));
    scalar_constant_pipeline.destroy().unwrap_or_else(|failure| die(failure));
    specialized_pipeline.destroy().unwrap_or_else(|failure| die(failure));
    pipeline.destroy().unwrap_or_else(|failure| die(failure));
    scalar_constant_function.destroy().unwrap_or_else(|failure| die(failure));
    specialized_function.destroy().unwrap_or_else(|failure| die(failure));
    function_.destroy().unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::StaleHandle, Function::find(&library, "increment").err());
    reject(ErrorKind::StaleHandle, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(1))], "specialized_increment").err());
    device.destroy().unwrap_or_else(|failure| die(failure));
}

// The Metal 4 compiler and the render pipelines it makes: what it refuses before compiling, what Metal refuses,
// the reflection it copies out, and the pixel a compiled pipeline draws.
const RENDER_SOURCE: &str = r#"
#include <metal_stdlib>
using namespace metal;

struct PrismelVertexOut {
  float4 position [[position]];
};

vertex PrismelVertexOut prismel_vertex(
    uint vertex_id [[vertex_id]],
    constant float2 &offset [[buffer(0)]]) {
  constexpr float2 positions[3] = {
    float2(-1.0f, -1.0f),
    float2(3.0f, -1.0f),
    float2(-1.0f, 3.0f)
  };
  PrismelVertexOut result;
  result.position = float4(positions[vertex_id] + offset, 0.0f, 1.0f);
  return result;
}

vertex void prismel_vertex_only(uint vertex_id [[vertex_id]]) {
  (void)vertex_id;
}

fragment float4 prismel_fragment(constant float4 &tint [[buffer(1)]]) {
  return tint;
}

kernel void prismel_kernel(device uint *out [[buffer(0)]]) { out[0] = 113; }
"#;

fn f32_bytes(values: &[f32]) -> Vec<u8> { values.iter().flat_map(|v| v.to_le_bytes()).collect() }

fn gpu_render_pipeline() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    if !device.supports_family(DeviceFamily::Metal4).unwrap_or_else(|failure| die(failure)) {
        without_handles("Metal 4 compiler on an older device", &mut || reject(ErrorKind::Unsupported, Compiler::create(&device).err()));
        device.destroy().unwrap_or_else(|failure| die(failure));
        return;
    }
    let c0 = stats();
    drop(Compiler::create(&device).unwrap_or_else(|failure| die(failure)));
    let s = settle(c0.live_handles);
    assert!(s.total_created - c0.total_created == 1 && s.total_released - c0.total_released == 1, "compiler finalization");
    let compiler = Compiler::create(&device).unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, device.destroy().err());
    let library = Library::compile_source(&device, RENDER_SOURCE, None).unwrap_or_else(|failure| die(failure));
    let fragment_options = || RenderPipelineOptions { fragment: Some("prismel_fragment".to_owned()), ..RenderPipelineOptions::default() };
    without_handles("invalid Metal 4 render inputs", &mut || {
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "", &fragment_options()).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "missing_vertex", &fragment_options()).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { label: Some("invalid\0render".to_owned()), ..fragment_options() }).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { fragment: Some("missing_fragment".to_owned()), ..RenderPipelineOptions::default() }).err());
        // Rasterization needs a fragment function and one to eight colours.
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions::default()).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex_only", &RenderPipelineOptions { color_formats: Some(vec![]), ..RenderPipelineOptions::default() }).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { color_formats: Some(vec![]), ..fragment_options() }).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { raster_sample_count: 0, ..fragment_options() }).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { color_formats: Some(vec![Format::Bgra8Unorm; 9]), ..fragment_options() }).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { color_formats: Some(vec![Format::Depth32Float]), ..fragment_options() }).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
            color_formats: Some(vec![Format::Bgra8Unorm]), color_attachments: Some(vec![ColorAttachment::new(Format::Bgra8Unorm)]), ..fragment_options()
        }).err());
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
            color_attachments: Some(vec![ColorAttachment { write_mask: vec![ColorWrite::Red, ColorWrite::Red], ..ColorAttachment::new(Format::Bgra8Unorm) }]), ..fragment_options()
        }).err());
        // A stage given a function of another stage is Metal's to refuse.
        reject(ErrorKind::DeviceLost, compiler.create_render_pipeline(&library, "prismel_fragment", &fragment_options()).err());
        reject(ErrorKind::DeviceLost, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { fragment: Some("prismel_vertex".to_owned()), ..RenderPipelineOptions::default() }).err());
        reject(ErrorKind::DeviceLost, compiler.create_render_pipeline(&library, "prismel_kernel", &fragment_options()).err());
    });
    let render_pipeline = compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
        label: Some("metal4 reflected render".to_owned()), reflection: true, primitive_topology: PrimitiveTopology::TriangleList,
        support_indirect_command_buffers: true, ..fragment_options()
    }).unwrap_or_else(|failure| die(failure));
    assert!(render_pipeline.supports_indirect_command_buffers().unwrap_or_else(|failure| die(failure)), "a pipeline compiled for indirect command buffers denies it");
    let reflection = render_pipeline.reflection().unwrap_or_else(|| panic!("Metal 4 render reflection is missing"));
    assert_eq!(
        reflection.vertex_bindings.iter().map(buffer_binding_or_fail).collect::<Vec<_>>(),
        vec![("offset".to_owned(), 0, BindingAccess::ReadOnly, ShaderDataType::Vector(ScalarType::Float, 2))],
        "Metal 4 vertex reflection is not the one float2 buffer at index 0",
    );
    assert_eq!(
        reflection.fragment_bindings.iter().map(buffer_binding_or_fail).collect::<Vec<_>>(),
        vec![("tint".to_owned(), 1, BindingAccess::ReadOnly, ShaderDataType::Vector(ScalarType::Float, 4))],
        "Metal 4 fragment reflection is not the one float4 buffer at index 1",
    );
    let plain_pipeline = compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
        color_formats: Some(vec![Format::Rgba8Unorm]), ..fragment_options()
    }).unwrap_or_else(|failure| die(failure));
    assert!(plain_pipeline.reflection().is_none(), "render reflection was kept without being asked for");
    assert!(!plain_pipeline.supports_indirect_command_buffers().unwrap_or_else(|failure| die(failure)), "a pipeline compiled without indirect command buffers claims them");
    let c1 = stats();
    drop(compiler.create_render_pipeline(&library, "prismel_vertex", &fragment_options()).unwrap_or_else(|failure| die(failure)));
    let s = settle(c1.live_handles);
    assert!(s.total_created - c1.total_created == 1 && s.total_released - c1.total_released == 1, "render pipeline finalization");
    // One covering triangle, the tint in a fragment buffer: the pipeline's output is the tint, to the byte.
    let queue = CommandQueue::create(&device).unwrap_or_else(|failure| die(failure));
    let target = Texture::create(&device, Texture::descriptor_2d(Format::Rgba8Unorm, 2, 2, StorageMode::Shared, &[TextureUsage::RenderTarget], None)).unwrap_or_else(|failure| die(failure));
    let offset = shared(&device, 16);
    offset.write_bytes(0, &f32_bytes(&[0.0, 0.0, 0.0, 0.0])).unwrap_or_else(|failure| die(failure));
    let tint = shared(&device, 16);
    tint.write_bytes(0, &f32_bytes(&[0.25, 0.5, 0.75, 1.0])).unwrap_or_else(|failure| die(failure));
    let pass = RenderPassDescriptor::create(&device, 2, 2, 1).unwrap_or_else(|failure| die(failure));
    pass.set_attachments(&target, None, None, [0.0; 4]).unwrap_or_else(|failure| die(failure));
    let commands = CommandBuffer::create(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = RenderEncoder::create(&commands, &pass).unwrap_or_else(|failure| die(failure));
    encoder.set_pipeline(&plain_pipeline).unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::InvalidState, plain_pipeline.destroy().err());
    encoder.set_vertex_buffer(0, 0, &offset).unwrap_or_else(|failure| die(failure));
    encoder.set_fragment_buffer(1, 0, &tint).unwrap_or_else(|failure| die(failure));
    encoder.draw_triangles(0, 3).unwrap_or_else(|failure| die(failure));
    // The same triangle again, its indices and counts read from buffers; a misaligned argument offset is refused before encoding.
    let indices = shared(&device, 12);
    indices.write_bytes(0, &i32_bytes(&[0, 1, 2])).unwrap_or_else(|failure| die(failure));
    let arguments = shared(&device, 24);
    arguments.write_bytes(0, &i32_bytes(&[0, 3, 1, 0, 0, 0])).unwrap_or_else(|failure| die(failure));
    encoder.draw_indexed_indirect(Primitive::TriangleList, IndexType::Uint32, &indices, 0, &arguments, 4).unwrap_or_else(|failure| die(failure));
    encoder.end_encoding().unwrap_or_else(|failure| die(failure));
    complete(&commands);
    arguments.destroy().unwrap_or_else(|failure| die(failure));
    indices.destroy().unwrap_or_else(|failure| die(failure));
    let pixels = target.read_bytes(full_region(2, 2), 0, 0, 8, 16).unwrap_or_else(|failure| die(failure));
    for pixel in 0..4 {
        let bytes = &pixels[pixel * 4..pixel * 4 + 4];
        assert_eq!(bytes, [0x40, 0x80, 0xbf, 0xff], "compiled pipeline drew {bytes:02x?} at pixel {pixel}");
    }
    pass.destroy().unwrap_or_else(|failure| die(failure));
    tint.destroy().unwrap_or_else(|failure| die(failure));
    offset.destroy().unwrap_or_else(|failure| die(failure));
    target.destroy().unwrap_or_else(|failure| die(failure));
    queue.destroy().unwrap_or_else(|failure| die(failure));
    plain_pipeline.destroy().unwrap_or_else(|failure| die(failure));
    render_pipeline.destroy().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::StaleHandle, render_pipeline.supports_indirect_command_buffers().err());
    render_pipeline.destroy().unwrap_or_else(|failure| die(failure));
    // A pipeline outlives the library and compiler that made it.
    let survivor = compiler.create_render_pipeline(&library, "prismel_vertex", &fragment_options()).unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    reject(ErrorKind::StaleHandle, compiler.create_render_pipeline(&library, "prismel_vertex", &fragment_options()).err());
    compiler.destroy().unwrap_or_else(|failure| die(failure));
    compiler.destroy().unwrap_or_else(|failure| die(failure));
    survivor.supports_indirect_command_buffers().unwrap_or_else(|failure| die(failure));
    survivor.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

// Argument-buffer encoding of a fragment function's [[id]] members: a texture and a sampler written into the buffer.
const ICB_SOURCE: &str = r#"
#include <metal_stdlib>
using namespace metal;
struct O { float4 position [[position]]; };
struct A { texture2d<float,access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
vertex O vertex_main(device const float2 *p [[buffer(0)]], uint i [[vertex_id]]) {
  O o; o.position=float4(p[i],0.,1.); return o;
}
fragment float4 fragment_main(O o [[stage_in]], constant A& args [[buffer(1)]]) {
  return args.image.sample(args.sampling,float2(.5));
}
fragment float4 fragment_solid(O o [[stage_in]]) { return float4(.25,.5,.75,1.); }
fragment float4 fragment_direct(O o [[stage_in]],texture2d<float> image [[texture(0)]],sampler sampling [[sampler(1)]]) { return image.sample(sampling,float2(.5)); }
"#;

fn gpu_render_icb_argument() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let library = Library::compile_source(&device, ICB_SOURCE, None).unwrap_or_else(|failure| die(failure));
    let fragment = Function::find(&library, "fragment_main").unwrap_or_else(|failure| die(failure));
    let mut encoder = fragment.argument_encoder(1).unwrap_or_else(|failure| die(failure));
    let sampled = Texture::create(&device, Texture::descriptor_2d(Format::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)).unwrap_or_else(|failure| die(failure));
    let sampler = Sampler::create(&device, SamplerDescriptor { support_argument_buffers: true, ..SamplerDescriptor::default() }).unwrap_or_else(|failure| die(failure));
    let argument_buffer = shared(&device, encoder.encoded_length());
    encoder.set_argument_buffer(&argument_buffer, 0).unwrap_or_else(|failure| die(failure));
    encoder.set(0, ShaderArgumentResource::Texture(sampled.clone())).unwrap_or_else(|failure| die(failure));
    encoder.set(1, ShaderArgumentResource::Sampler(sampler.clone())).unwrap_or_else(|failure| die(failure));
    encoder.destroy().unwrap_or_else(|failure| die(failure));
    argument_buffer.destroy().unwrap_or_else(|failure| die(failure));
    sampler.destroy().unwrap_or_else(|failure| die(failure));
    sampled.destroy().unwrap_or_else(|failure| die(failure));
    fragment.destroy().unwrap_or_else(|failure| die(failure));
    library.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
}

// Argument reflection snapshot over a mock native layer: bounded conversion, and the native handle count
// is the same after success and after each rejection.
mod mock_reflection {
    use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
    use crate::native_reflection_snapshot::{Handle as AnyHandle, Kind, Raw};
    pub static LIVE: AtomicI64 = AtomicI64::new(0);
    pub static FAIL_MEMBER_NAME: AtomicBool = AtomicBool::new(false);
    #[derive(Clone)]
    pub enum Node { Scalar, TextureReference, Array(Box<Node>), Struct(Vec<(String, Node)>), Member(String, Box<Node>) }
    #[derive(Clone)]
    pub struct Handle { node: Node, owned: bool }
    pub fn root(node: Node) -> Handle { Handle { node, owned: false } }
    fn own(node: Node) -> Handle { LIVE.fetch_add(1, Ordering::SeqCst); Handle { node, owned: true } }
    pub struct Mock;
    fn cast(handle: &AnyHandle) -> &Handle { handle.downcast_ref::<Handle>().expect("mock handle") }
    impl Raw for Mock {
        fn destroy(&self, handle: AnyHandle) { let handle = cast(&handle); if handle.owned { LIVE.fetch_sub(1, Ordering::SeqCst); } }
        fn kind(&self, h: &AnyHandle) -> Result<Kind, String> {
            let h = cast(h);
            Ok(match h.node { Node::Scalar | Node::Member(..) => Kind::Scalar, Node::TextureReference => Kind::TextureReference, Node::Array(_) => Kind::Array, Node::Struct(_) => Kind::Struct })
        }
        fn data_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(1) }
        fn array_length(&self, _: &AnyHandle) -> Result<i64, String> { Ok(1) }
        fn array_stride(&self, _: &AnyHandle) -> Result<i64, String> { Ok(4) }
        fn argument_index_stride(&self, _: &AnyHandle) -> Result<i64, String> { Ok(1) }
        fn element(&self, h: &AnyHandle) -> Result<Option<AnyHandle>, String> {
            let h = cast(h);
            Ok(match &h.node { Node::Array(child) => Some(Box::new(own((**child).clone()))), _ => None })
        }
        fn pointer_access(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn alignment(&self, _: &AnyHandle) -> Result<i64, String> { Ok(4) }
        fn data_size(&self, _: &AnyHandle) -> Result<i64, String> { Ok(4) }
        fn element_is_argument_buffer(&self, _: &AnyHandle) -> Result<bool, String> { Ok(false) }
        fn members(&self, h: &AnyHandle) -> Result<Vec<AnyHandle>, String> {
            let h = cast(h);
            Ok(match &h.node {
                Node::Struct(members) => members.iter().map(|(name, node)| Box::new(own(Node::Member(name.clone(), Box::new(node.clone())))) as AnyHandle).collect(),
                _ => Vec::new(),
            })
        }
        fn member_name(&self, h: &AnyHandle) -> Result<String, String> {
            let h = cast(h);
            if FAIL_MEMBER_NAME.load(Ordering::SeqCst) { return Err("injected member-name failure".to_owned()); }
            match &h.node { Node::Member(name, _) => Ok(name.clone()), _ => Err("not member".to_owned()) }
        }
        fn member_offset(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn member_argument_index(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn member_type(&self, h: &AnyHandle) -> Result<Option<AnyHandle>, String> {
            let h = cast(h);
            Ok(match &h.node { Node::Member(_, child) => Some(Box::new(own((**child).clone()))), _ => None })
        }
        fn texture_access(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn texture_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(2) }
        fn is_depth_texture(&self, _: &AnyHandle) -> Result<bool, String> { Ok(false) }
        fn tensor_access(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn tensor_data_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(3) }
        fn tensor_index_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(4) }
        fn tensor_dimensions(&self, _: &AnyHandle) -> Result<Option<Vec<i64>>, String> { Ok(Some(vec![2, 3])) }
        fn argument_name(&self, _: &AnyHandle) -> Result<String, String> { Ok("items".to_owned()) }
        fn argument_index(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn argument_active(&self, _: &AnyHandle) -> Result<bool, String> { Ok(true) }
        fn argument_access(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn argument_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn argument_array_length(&self, _: &AnyHandle) -> Result<i64, String> { Ok(1) }
        fn argument_buffer_alignment(&self, _: &AnyHandle) -> Result<i64, String> { Ok(16) }
        fn argument_buffer_data_size(&self, _: &AnyHandle) -> Result<i64, String> { Ok(16) }
        fn argument_buffer_data_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(1) }
        fn argument_texture_data_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(1) }
        fn argument_texture_type(&self, _: &AnyHandle) -> Result<i64, String> { Ok(2) }
        fn argument_is_depth_texture(&self, _: &AnyHandle) -> Result<bool, String> { Ok(false) }
        fn argument_threadgroup_memory_alignment(&self, _: &AnyHandle) -> Result<i64, String> { Ok(16) }
        fn argument_threadgroup_memory_data_size(&self, _: &AnyHandle) -> Result<i64, String> { Ok(0) }
        fn argument_buffer_pointer_type(&self, _: &AnyHandle) -> Result<Option<AnyHandle>, String> { Ok(Some(Box::new(own(Node::Array(Box::new(Node::Scalar)))))) }
        fn argument_buffer_struct_type(&self, _: &AnyHandle) -> Result<Option<AnyHandle>, String> { Ok(Some(Box::new(own(Node::Struct(vec![("x".to_owned(), Node::Scalar)]))))) }
    }
}

fn gpu_argument_reflection_snapshot() {
    use crate::native_reflection_snapshot::{argument, reflected_type, reflected_type_with_limits, Limits};
    use metal_sys::native::reflection::ReflectedType;
    use crate::native_reflection_snapshot::Handle as AnyHandle;
    use mock_reflection::{root, Mock, Node, FAIL_MEMBER_NAME, LIVE};
    use std::sync::atomic::Ordering;
    let tree: AnyHandle = Box::new(root(Node::Struct(vec![("position".to_owned(), Node::Array(Box::new(Node::Scalar))), ("normal".to_owned(), Node::Scalar)])));
    let before = LIVE.load(Ordering::SeqCst);
    let no_delta = |label: &str| assert_eq!(LIVE.load(Ordering::SeqCst), before, "{label}: native handle delta");
    match reflected_type(&Mock, &tree) {
        Ok(ReflectedType::Struct(members)) => assert_eq!(members.len(), 2, "member snapshot"),
        Ok(_) => panic!("unexpected reflection kind"),
        Err(message) => panic!("{message}"),
    }
    no_delta("success");
    assert!(reflected_type_with_limits(&Mock, &tree, Limits { max_depth: 0, max_members: 8 }).is_err(), "depth limit accepted");
    no_delta("depth rejection");
    assert!(reflected_type_with_limits(&Mock, &tree, Limits { max_depth: 8, max_members: 1 }).is_err(), "member limit accepted");
    no_delta("member rejection");
    FAIL_MEMBER_NAME.store(true, Ordering::SeqCst);
    let failed = reflected_type(&Mock, &tree).is_err();
    FAIL_MEMBER_NAME.store(false, Ordering::SeqCst);
    assert!(failed, "injected failure accepted");
    no_delta("failure unwind");
    match argument(&Mock, &(Box::new(root(Node::Scalar)) as AnyHandle)) {
        Ok(argument) => assert_eq!(argument.name, "items", "argument snapshot"),
        Err(message) => panic!("{message}"),
    }
    no_delta("argument success");
    match reflected_type(&Mock, &(Box::new(root(Node::TextureReference)) as AnyHandle)) {
        Ok(ReflectedType::TextureReference { data_type: 1, access: 0, texture_type: 2, depth: false }) => {}
        Ok(_) => panic!("texture-reference snapshot mismatch"),
        Err(message) => panic!("{message}"),
    }
    no_delta("texture-reference success");
}

fn gpu_handle_release_pool() {
    const CHILD: &str = "RAYS_HANDLE_POOL_CHILD";
    if std::env::var_os(CHILD).is_some() {
        for _ in 0..32 {
            drop(metal_sys::native::descriptors::create_render_pass().expect("render pass"));
            metal_sys::drain_releases().expect("release handles");
        }
        gpu_compute_pipeline();
        return;
    }
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "gpu_handle_release_pool"])
        .env(CHILD, "1").env("OBJC_DEBUG_MISSING_POOLS", "YES")
        .output().expect("autorelease child");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "autorelease child failed: {stderr}");
    assert!(!stderr.contains("MISSING POOLS"), "native handle release autoreleases outside a pool: {stderr}");
}

pub const CASES: &[(&str, fn())] = &[
        ("gpu_handle_release_pool", gpu_handle_release_pool),
        ("gpu_owner_only_handles", gpu_owner_only_handles),
        ("gpu_device_safe", gpu_device_safe),
        ("gpu_device_queue", gpu_device_queue),
        ("gpu_device_library", gpu_device_library),
        ("gpu_command_buffer", gpu_command_buffer),
        ("gpu_blit", gpu_blit),
        ("gpu_compute_pipeline", gpu_compute_pipeline),
        ("gpu_compute_encoder", gpu_compute_encoder),
        ("gpu_render_encoder", gpu_render_encoder),
        ("gpu_resource_bounds", gpu_resource_bounds),
        ("gpu_sampler", gpu_sampler),
        ("gpu_texture_formats", gpu_texture_formats),
        ("gpu_texture_swizzle_compression", gpu_texture_swizzle_compression),
        ("gpu_texture_transfer_descriptors", gpu_texture_transfer_descriptors),
        ("gpu_texture_blit", gpu_texture_blit),
        ("gpu_function_constants", gpu_function_constants),
        ("gpu_render_pipeline", gpu_render_pipeline),
        ("gpu_render_icb_argument", gpu_render_icb_argument),
        ("gpu_argument_reflection_snapshot", gpu_argument_reflection_snapshot),
];
