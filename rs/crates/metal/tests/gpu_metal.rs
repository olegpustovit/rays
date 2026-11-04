// Port of lib/metal/test_metal_{device_safe,device_queue_safe,device_library_safe,command_buffer_safe,
// blit_safe,compute_pipeline_safe}.ml on the real Metal device. Each case runs in its own process
// (rays-t passes --exact), as the OCaml suite does, since they count process-wide handles.
mod common;
extern crate metal as m;
use m::*;

fn get<A>(result: Result<A, Error>) -> A { result.unwrap_or_else(|e| panic!("{e}")) }
fn reject<A>(kind: ErrorKind, result: Result<A, Error>) {
    match result {
        Err(e) if e.kind == kind => {}
        Err(e) => panic!("expected {kind:?}, got {e}"),
        Ok(_) => panic!("operation unexpectedly succeeded, expected {kind:?}"),
    }
}
fn stats() -> ReleaseQueue::Stats { get(ReleaseQueue::stats()) }
fn i32_bytes(values: &[i32]) -> Vec<u8> { values.iter().flat_map(|v| v.to_le_bytes()).collect() }
fn shared(device: &Device, length: u64) -> Buffer { get(Buffer::create(device, length, StorageMode::Shared, CpuCacheMode::DefaultCache, None)) }
fn settle(expected_live: usize) -> ReleaseQueue::Stats {
    for _ in 0..8 {
        get(ReleaseQueue::drain());
        let s = stats();
        if s.pending == 0 && s.live_handles == expected_live { return s; }
    }
    let s = stats();
    panic!("Metal handles did not settle ({} pending, {} live; expected {expected_live})", s.pending, s.live_handles);
}
// Every rejection in `operation` is decided before Metal is asked for an object.
fn without_handles<A>(name: &str, operation: impl FnOnce() -> A) -> A {
    get(ReleaseQueue::drain());
    let before = stats();
    let result = operation();
    let after = stats();
    assert!(after.total_created == before.total_created && after.live_handles == before.live_handles, "{name} allocated a native handle");
    result
}
const SOURCE_K: &str = "#include <metal_stdlib>\nusing namespace metal; kernel void k(uint i [[thread_position_in_grid]]) {}";

fn gpu_device_safe() {
    let device = get(Device::system_default());
    let before = stats();
    let info = get(device.info());
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
    reject(ErrorKind::WrongDomain, off);
    let buffer = get(Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::DefaultCache, Some("Metal conformance values")));
    assert_eq!(buffer.length(), 16);
    let values = i32_bytes(&[1, 41, 99, -2]);
    get(buffer.write_bytes(0, &values));
    assert_eq!(get(buffer.read_bytes(0, 16)), values);
    assert_eq!(get(buffer.read_bytes(4, 4)), i32_bytes(&[41]), "a buffer range read the wrong offset");
    reject(ErrorKind::ParentHasDependents, device.destroy());
    reject(ErrorKind::InvalidArgument, buffer.read_bytes(12, 8));
    reject(ErrorKind::InvalidArgument, buffer.read_bytes(0, usize::MAX >> 1));
    without_handles("invalid buffer labels", || {
        reject(ErrorKind::InvalidArgument, Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::DefaultCache, Some("invalid\0label")));
    });
    let private = get(Buffer::create(&device, 16, StorageMode::Private, CpuCacheMode::DefaultCache, None));
    reject(ErrorKind::Unsupported, private.read_bytes(0, 4));
    reject(ErrorKind::Unsupported, private.write_bytes(0, &values));
    get(private.destroy());
    let configured = get(Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::WriteCombined, None));
    get(configured.write_bytes(0, &values));
    assert_eq!(get(configured.read_bytes(0, 16)), values, "write-combined buffer bytes did not round-trip");
    get(configured.destroy());
    // a mapped buffer: the host view is the memory the buffer reads
    let (mapped, mut floats) = get(Buffer::create_mapped(&device, 16, None));
    assert!(floats.len() == 4 && mapped.length() >= 16);
    floats.as_mut_slice()[1] = 2.5;
    assert_eq!(get(mapped.read_bytes(4, 4)), 2.5f32.to_bits().to_le_bytes());
    get(mapped.write_bytes(8, &7f32.to_bits().to_le_bytes()));
    assert_eq!(floats.as_slice()[2], 7.0, "mapped buffer write is not in its view");
    get(mapped.destroy());
    assert_eq!(floats.as_slice()[1], 2.5, "mapped view changed after destroy");
    reject(ErrorKind::InvalidArgument, Buffer::create_mapped(&device, 6, None));
    reject(ErrorKind::InvalidArgument, device.supports_texture_sample_count(0));
    assert!(get(device.supports_texture_sample_count(1)), "device refuses single-sample textures");
    get(buffer.destroy());
    get(buffer.destroy());
    reject(ErrorKind::Destroyed, buffer.read_bytes(0, 1));
    reject(ErrorKind::Destroyed, buffer.write_bytes(0, &values));
    get(device.destroy());
    reject(ErrorKind::Destroyed, device.info());
    reject(ErrorKind::Destroyed, Buffer::create(&device, 16, StorageMode::Shared, CpuCacheMode::DefaultCache, None));
    drop((floats, mapped, buffer, private, configured, device));
    let fin = settle(0);
    assert!(fin.pending == 0 && fin.external_deallocation_mismatches == 0);
}

fn gpu_device_queue() {
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    get(queue.destroy());
    get(device.destroy());
}

fn gpu_device_library() {
    let device = get(Device::system_default());
    reject(ErrorKind::InvalidArgument, Library::compile_source(&device, "", None));
    reject(ErrorKind::InvalidArgument, Library::compile_source(&device, "kernel void k() {}\0", None));
    reject(ErrorKind::InvalidArgument, Library::compile_source(&device, "kernel void k() {}", Some("a\0b")));
    reject(ErrorKind::NativeError, Library::compile_source(&device, "this is not Metal", None));
    let library = get(Library::compile_source(&device, SOURCE_K, Some("source_library")));
    assert!(Function::find(&library, "missing").is_err(), "a function the library does not define was found");
    let function = get(Function::find(&library, "k"));
    reject(ErrorKind::ParentHasDependents, library.destroy());
    get(function.destroy());
    get(library.destroy());
    get(device.destroy());
}

fn gpu_command_buffer() {
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    let command = get(CommandBuffer::create(&queue));
    let blit = get(BlitEncoder::create(&command));
    get(blit.end_encoding());
    reject(ErrorKind::ParentHasDependents, queue.destroy());
    get(command.commit());
    get(command.wait_until_completed());
    get(command.destroy());
    get(ReleaseQueue::drain());
    let before = stats().live_handles;
    {
        let command = get(CommandBuffer::create(&queue));
        get(command.commit());
        get(command.wait_until_completed());
    }
    get(ReleaseQueue::drain());
    assert_eq!(stats().live_handles, before, "abandoned command buffer retained a native handle");
    get(queue.destroy());
    get(device.destroy());
}

fn gpu_blit() {
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    let commands = get(CommandBuffer::create(&queue));
    let blit = get(BlitEncoder::create(&commands));
    let (source, destination) = (shared(&device, 64), shared(&device, 64));
    get(blit.end_encoding());
    get(commands.commit());
    get(commands.wait_until_completed());
    get(commands.destroy());
    get(destination.destroy());
    get(source.destroy());
    get(queue.destroy());
    get(device.destroy());
}

fn gpu_compute_pipeline() {
    let device = get(Device::system_default());
    let source = "#include <metal_stdlib>\nusing namespace metal; kernel void increment_output(device uint *out [[buffer(0)]], uint i [[thread_position_in_grid]]) { out[i] = i + 7; }";
    let library = get(Library::compile_source(&device, source, None));
    let function = get(Function::find(&library, "increment_output"));
    let pipeline = get(ComputePipeline::create(&function, None, true));
    assert!(pipeline.bindings().is_some_and(|b| !b.is_empty()), "missing immutable reflection snapshot");
    let plain = get(ComputePipeline::create(&function, None, false));
    assert!(plain.bindings().is_none(), "reflection was kept without being asked for");
    get(plain.destroy());
    let queue = get(CommandQueue::create(&device));
    let buffer = shared(&device, 16);
    let command = get(CommandBuffer::create(&queue));
    let encoder = get(ComputeEncoder::create(&command));
    get(encoder.set_pipeline(&pipeline));
    get(encoder.set_buffer(0, 0, &buffer));
    get(encoder.dispatch_threads((4, 1, 1), (1, 1, 1)));
    get(encoder.end_encoding());
    get(command.commit());
    get(command.wait_until_completed());
    assert_eq!(get(buffer.read_bytes(0, 16)), i32_bytes(&[7, 8, 9, 10]), "compute readback mismatch");
    get(command.destroy());
    get(buffer.destroy());
    get(pipeline.destroy());
    get(function.destroy());
    get(library.destroy());
    get(queue.destroy());
    get(device.destroy());
}

fn gpu_compute_encoder() {
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    let source = "#include <metal_stdlib>\nusing namespace metal;\nkernel void add_step(device uint *out [[buffer(0)]], constant uint &step [[buffer(1)]],\n    uint i [[thread_position_in_grid]]) { out[i] += step; }\n";
    let library = get(Library::compile_source(&device, source, None));
    let function = get(Function::find(&library, "add_step"));
    let pipeline = get(ComputePipeline::create(&function, None, false));
    let buffer = shared(&device, 16);
    get(buffer.write_bytes(0, &i32_bytes(&[0, 10, 20, 30])));
    let arguments = shared(&device, 16);
    get(arguments.write_bytes(0, &i32_bytes(&[4, 1, 1, 0])));
    let texture = get(Texture::create(&device, Texture::descriptor_2d(m::metal_format::Format::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)));
    let command = get(CommandBuffer::create(&queue));
    let encoder = get(ComputeEncoder::create(&command));
    reject(ErrorKind::InvalidArgument, encoder.set_buffer(0, 17, &buffer));
    reject(ErrorKind::InvalidArgument, encoder.set_bytes(1, &[]));
    reject(ErrorKind::InvalidState, encoder.dispatch_threads((4, 1, 1), (1, 1, 1)));
    get(encoder.set_pipeline(&pipeline));
    reject(ErrorKind::InvalidArgument, encoder.dispatch_threads((0, 1, 1), (1, 1, 1)));
    get(encoder.set_buffer(0, 0, &buffer));
    get(encoder.set_bytes(1, &3i32.to_le_bytes()));
    get(encoder.set_texture(0, &texture));
    get(encoder.dispatch_threads((4, 1, 1), (1, 1, 1)));
    // the same again, its four threadgroups of one read from a buffer
    reject(ErrorKind::InvalidArgument, encoder.dispatch_threadgroups_indirect(&arguments, 8, (1, 1, 1)));
    get(encoder.dispatch_threadgroups_indirect(&arguments, 0, (1, 1, 1)));
    get(encoder.end_encoding());
    reject(ErrorKind::Destroyed, encoder.dispatch_threads((4, 1, 1), (1, 1, 1)));
    reject(ErrorKind::ParentHasDependents, buffer.destroy());
    reject(ErrorKind::ParentHasDependents, texture.destroy());
    get(command.commit());
    get(command.wait_until_completed());
    get(command.destroy());
    assert_eq!(get(buffer.read_bytes(0, 16)), i32_bytes(&[6, 16, 26, 36]), "compute output mismatch");
    get(buffer.destroy());
    get(arguments.destroy());
    get(texture.destroy());
    get(pipeline.destroy());
    get(function.destroy());
    get(library.destroy());
    get(queue.destroy());
    get(device.destroy());
}

// Rejections that are decided before Metal is asked for anything: no handle is created or released.
fn gpu_resource_bounds() {
    let device = get(Device::system_default());
    let info = get(device.info());
    let reject_without_native_work = |name: &str, operation: &dyn Fn() -> Result<(), Error>| {
        get(ReleaseQueue::drain());
        let before = stats();
        match operation() {
            Err(e) if e.kind == ErrorKind::InvalidArgument => {}
            Err(e) => panic!("{name} returned {e}, expected InvalidArgument"),
            Ok(()) => panic!("{name} accepted a malformed range"),
        }
        assert_eq!(before, stats(), "{name} crossed the native boundary");
    };
    reject_without_native_work("oversized buffer length", &|| Buffer::create(&device, info.max_buffer_length + 1, StorageMode::Shared, CpuCacheMode::DefaultCache, None).map(|_| ()));
    let buffer = shared(&device, 32);
    let bytes = [1u8; 16];
    reject_without_native_work("buffer destination overflow", &|| buffer.write_bytes(24, &bytes));
    reject_without_native_work("buffer destination integer overflow", &|| buffer.write_bytes(i64::MAX as u64, &bytes));
    reject_without_native_work("buffer oversized read", &|| buffer.read_bytes(31, 2).map(|_| ()));
    reject_without_native_work("buffer integer-overflow read", &|| buffer.read_bytes(i64::MAX as u64, usize::MAX >> 1).map(|_| ()));
    get(buffer.write_bytes(16, &bytes));
    let mut expected = vec![0u8; 16];
    expected.extend_from_slice(&bytes);
    assert_eq!(get(buffer.read_bytes(0, 32)), expected, "an admitted write did not land at its offset");
    assert_eq!(get(buffer.read_bytes(31, 1)), vec![1u8], "the last byte was not readable");
    get(buffer.destroy());
    get(device.destroy());
    get(ReleaseQueue::drain());
}

fn gpu_sampler() {
    let device = get(Device::system_default());
    let descriptor = SamplerDescriptor {
        min_filter: Filter::Linear, mag_filter: Filter::Linear, mip_filter: MipFilter::Linear, max_anisotropy: 4,
        address_s: AddressMode::Repeat, address_t: AddressMode::Repeat, address_r: AddressMode::Repeat,
        lod_max: 3.0, support_argument_buffers: true, label: Some("Metal linear sampler".to_owned()), ..SamplerDescriptor::default()
    };
    let c0 = stats();
    drop(get(Sampler::create(&device, descriptor.clone())));
    let s = settle(c0.live_handles);
    assert!(s.total_created - c0.total_created == 1 && s.total_released - c0.total_released == 1, "sampler finalization");
    let sampler = get(Sampler::create(&device, descriptor.clone()));
    reject(ErrorKind::ParentHasDependents, device.destroy());
    let plain = get(Sampler::create(&device, SamplerDescriptor::default()));
    get(plain.destroy());
    // Reduction modes and LOD bias are an Apple10 feature: made there, refused before any allocation elsewhere.
    for (reduction_mode, lod_bias) in [(ReductionMode::Minimum, -16.0), (ReductionMode::Maximum, 15.999), (ReductionMode::Minimum, 0.5), (ReductionMode::WeightedAverage, 0.5)] {
        let advanced = SamplerDescriptor { reduction_mode, lod_bias, label: Some("Metal reduction sampler".to_owned()), ..descriptor.clone() };
        match Sampler::create(&device, advanced.clone()) {
            Ok(made) => {
                assert!(get(device.supports_family(DeviceFamily::Apple10)), "sampler reduction support escaped its Apple10 hardware gate");
                get(made.destroy());
            }
            Err(e) if e.kind == ErrorKind::Unsupported => {
                without_handles("unsupported sampler extensions", || reject(ErrorKind::Unsupported, Sampler::create(&device, advanced.clone())));
            }
            Err(e) => panic!("{e}"),
        }
    }
    without_handles("invalid sampler inputs", || {
        let d = || descriptor.clone();
        for invalid in [
            SamplerDescriptor { max_anisotropy: 0, ..d() }, SamplerDescriptor { max_anisotropy: 17, ..d() },
            SamplerDescriptor { lod_min: f32::NAN, ..d() }, SamplerDescriptor { lod_min: -1.0, ..d() },
            SamplerDescriptor { lod_bias: f32::NAN, ..d() }, SamplerDescriptor { lod_bias: -16.001, ..d() }, SamplerDescriptor { lod_bias: 16.0, ..d() },
            SamplerDescriptor { normalized_coordinates: false, address_s: AddressMode::Repeat, ..d() },
            SamplerDescriptor { lod_min: 2.0, lod_max: 1.0, ..d() },
            SamplerDescriptor { label: Some("invalid\0label".to_owned()), ..d() },
        ] {
            reject(ErrorKind::InvalidArgument, Sampler::create(&device, invalid));
        }
    });
    // Pixel coordinates are accepted with the one addressing they allow.
    let unnormalized = get(Sampler::create(&device, SamplerDescriptor { normalized_coordinates: false, ..SamplerDescriptor::default() }));
    get(unnormalized.destroy());
    get(sampler.destroy());
    get(sampler.destroy());
    get(device.destroy());
    reject(ErrorKind::Destroyed, Sampler::create(&device, descriptor));
    drop((sampler, plain, unnormalized, device));
    settle(0);
}


// Compact port of test_metal_texture_safe.ml: the format matrix, blit upload with CPU readback,
// swizzle through a shader, and the memoryless / writable-swizzle rejections.
fn full_region(width: usize, height: usize) -> TextureRegion { TextureRegion { x: 0, y: 0, z: 0, width, height, depth: 1 } }
fn complete(commands: &CommandBuffer) {
    get(commands.commit());
    get(commands.wait_until_completed());
    assert!(matches!(get(commands.status()), CommandBufferStatus::Completed), "command buffer did not complete");
    get(commands.destroy());
}
fn upload(device: &Device, queue: &CommandQueue, texture: &Texture, region: TextureRegion, bytes_per_row: usize, bytes: &[u8]) {
    let staging = shared(device, bytes.len() as u64);
    get(staging.write_bytes(0, bytes));
    let commands = get(CommandBuffer::create(queue));
    let blit = get(BlitEncoder::create(&commands));
    get(blit.copy_buffer_to_texture(&staging, 0, bytes_per_row, bytes.len(), texture, 0, 0, region));
    get(blit.end_encoding());
    complete(&commands);
    get(staging.destroy());
}

fn gpu_texture() {
    use m::metal_format::Format as F;
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    for format in [F::R8Unorm, F::Rgba8Unorm, F::Bgra8Unorm, F::Bgra8UnormSrgb, F::Rgba16Float, F::Rgba32Float, F::Depth32Float] {
        match Texture::create(&device, Texture::descriptor_2d(format, 4, 4, StorageMode::Shared, &[TextureUsage::ShaderRead], None)) {
            Ok(texture) => {
                assert_eq!(texture.descriptor().format, format, "Metal changed a retained pixel format");
                get(texture.destroy());
            }
            Err(e) if matches!(e.kind, ErrorKind::NativeError | ErrorKind::Unsupported) => {}
            Err(e) => panic!("{e}"),
        }
    }
    // upload by blit, read back on the CPU
    let rgba = get(Texture::create(&device, Texture::descriptor_2d(F::Rgba8Unorm, 2, 2, StorageMode::Shared, &[TextureUsage::ShaderRead], Some("texture readback"))));
    let pixels: Vec<u8> = (0..16).collect();
    upload(&device, &queue, &rgba, full_region(2, 2), 8, &pixels);
    assert_eq!(get(rgba.read_bytes(full_region(2, 2), 0, 0, 8, 16)), pixels, "texture bytes did not round-trip");
    get(rgba.destroy());
    // swizzle: the shader sees the retained channel mapping
    let source = "#include <metal_stdlib>\nusing namespace metal;\nkernel void read_swizzle(texture2d<float, access::sample> source [[texture(0)]], device uint4 *result [[buffer(0)]]) {\n  constexpr sampler nearest_sampler(coord::pixel, filter::nearest);\n  result[0] = uint4(round(source.sample(nearest_sampler, float2(0.5f)) * 255.0f));\n}";
    let swizzle = Swizzle { red: SwizzleChannel::Red, green: SwizzleChannel::One, blue: SwizzleChannel::One, alpha: SwizzleChannel::Green };
    let swizzled = get(Texture::create(&device, TextureDescriptor { swizzle, ..Texture::descriptor_2d(F::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None) }));
    assert_eq!(swizzled.descriptor().swizzle, swizzle, "base texture swizzle did not round-trip");
    let plain = get(Texture::create(&device, Texture::descriptor_2d(F::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)));
    for t in [&swizzled, &plain] { upload(&device, &queue, t, full_region(1, 1), 4, &[10, 20, 30, 40]); }
    let output = shared(&device, 16);
    let library = get(Library::compile_source(&device, source, None));
    let function = get(Function::find(&library, "read_swizzle"));
    let pipeline = get(ComputePipeline::create(&function, None, false));
    for (name, texture, expected) in [("identity", &plain, [10, 20, 30, 40]), ("base", &swizzled, [10, 255, 255, 20])] {
        let commands = get(CommandBuffer::create(&queue));
        let encoder = get(ComputeEncoder::create(&commands));
        get(encoder.set_pipeline(&pipeline));
        get(encoder.set_texture(0, texture));
        get(encoder.set_buffer(0, 0, &output));
        get(encoder.dispatch_threads((1, 1, 1), (1, 1, 1)));
        get(encoder.end_encoding());
        complete(&commands);
        assert_eq!(get(output.read_bytes(0, 16)), i32_bytes(&expected), "{name} texture swizzle");
    }
    get(pipeline.destroy());
    get(function.destroy());
    get(library.destroy());
    get(output.destroy());
    get(plain.destroy());
    get(swizzled.destroy());
    // a memoryless texture is a render target, a buffer is not memoryless; writable textures take no swizzle
    let memoryless = Texture::descriptor_2d(F::Depth32Float, 4, 4, StorageMode::Memoryless, &[TextureUsage::RenderTarget], None);
    if get(device.supports_family(DeviceFamily::Apple7)) { get(get(Texture::create(&device, memoryless.clone())).destroy()); }
    without_handles("memoryless and writable-swizzle rejections", || {
        reject(ErrorKind::InvalidArgument, Texture::create(&device, TextureDescriptor { usage: vec![TextureUsage::ShaderRead], ..memoryless.clone() }));
        reject(ErrorKind::InvalidArgument, Buffer::create(&device, 16, StorageMode::Memoryless, CpuCacheMode::DefaultCache, None));
        for usage in [TextureUsage::ShaderWrite, TextureUsage::ShaderAtomic] {
            let d = TextureDescriptor { swizzle, ..Texture::descriptor_2d(F::Rgba8Unorm, 4, 4, StorageMode::Private, &[usage], None) };
            reject(ErrorKind::InvalidArgument, Texture::create(&device, d));
        }
    });
    get(queue.destroy());
    get(device.destroy());
}

fn main() {
    if !cfg!(target_os = "macos") { return; } // Metal exists only on macOS
    common::run(&[
        ("gpu_device_safe", gpu_device_safe),
        ("gpu_device_queue", gpu_device_queue),
        ("gpu_device_library", gpu_device_library),
        ("gpu_command_buffer", gpu_command_buffer),
        ("gpu_blit", gpu_blit),
        ("gpu_compute_pipeline", gpu_compute_pipeline),
        ("gpu_compute_encoder", gpu_compute_encoder),
        ("gpu_resource_bounds", gpu_resource_bounds),
        ("gpu_sampler", gpu_sampler),
        ("gpu_texture", gpu_texture),
    ]);
}
