// Port of lib/metal/test_metal_{device_safe,device_queue_safe,device_library_safe,command_buffer_safe,
// blit_safe,compute_pipeline_safe}.ml on the real Metal device. Each case runs in its own process
// (rays-t passes --exact), as the OCaml suite does, since they count process-wide handles.
mod common;
extern crate metal as m;
use m::metal_format::Format;
use m::pipeline::{IndexType, Primitive};
use m::reflection::{shader_data_type, ScalarType, ShaderDataType};
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


// Port of lib/metal/test_metal_texture_safe.ml, one case per former section.
fn full_region(width: usize, height: usize) -> TextureRegion { TextureRegion { x: 0, y: 0, z: 0, width, height, depth: 1 } }
fn complete(commands: &CommandBuffer) {
    get(commands.commit());
    get(commands.wait_until_completed());
    assert!(matches!(get(commands.status()), CommandBufferStatus::Completed), "command buffer did not complete");
    get(commands.destroy());
}
// Texture contents arrive through a staging buffer and a blit (the one way to write a texture here).
fn upload(device: &Device, queue: &CommandQueue, texture: &Texture, level: usize, region: TextureRegion, bytes_per_row: usize, bytes: &[u8]) {
    let staging = shared(device, bytes.len() as u64);
    get(staging.write_bytes(0, bytes));
    let commands = get(CommandBuffer::create(queue));
    let blit = get(BlitEncoder::create(&commands));
    get(blit.copy_buffer_to_texture(&staging, 0, bytes_per_row, bytes.len(), texture, 0, level, region));
    get(blit.end_encoding());
    complete(&commands);
    get(staging.destroy());
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
    use m::metal_format::Format as F;
    let device = get(Device::system_default());
    let formats = [F::R8Unorm, F::Rgba8Unorm, F::Bgra8Unorm, F::Bgra8UnormSrgb, F::Rgba16Float, F::Rgba32Float, F::Depth32Float];
    let codes: std::collections::HashSet<u32> = formats.iter().map(|format| format.code()).collect();
    assert_eq!(codes.len(), 7, "Metal/OGPU format inventory drift");
    for format in formats {
        match Texture::create(&device, Texture::descriptor_2d(format, 4, 4, StorageMode::Shared, &[TextureUsage::ShaderRead], None)) {
            Ok(texture) => {
                assert_eq!(texture.descriptor().format, format, "Metal changed a retained pixel format");
                get(texture.destroy());
            }
            Err(e) if matches!(e.kind, ErrorKind::NativeError | ErrorKind::Unsupported) => {}
            Err(e) => panic!("{e}"),
        }
    }
    get(device.destroy());
}

fn gpu_texture_swizzle_compression() {
    use m::metal_format::Format as F;
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    let base_swizzle = Swizzle { red: SwizzleChannel::Red, green: SwizzleChannel::One, blue: SwizzleChannel::One, alpha: SwizzleChannel::Green };
    let swizzled = |storage: StorageMode, usage: &[TextureUsage], width: usize, height: usize| TextureDescriptor {
        swizzle: base_swizzle, ..Texture::descriptor_2d(F::Rgba8Unorm, width, height, storage, usage, None)
    };
    let texture = get(Texture::create(&device, swizzled(StorageMode::Shared, &[TextureUsage::ShaderRead], 1, 1)));
    assert_eq!(texture.descriptor().swizzle, base_swizzle, "base texture swizzle did not round-trip");
    upload(&device, &queue, &texture, 0, full_region(1, 1), 4, &[10, 20, 30, 40]);
    let plain = get(Texture::create(&device, Texture::descriptor_2d(F::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)));
    upload(&device, &queue, &plain, 0, full_region(1, 1), 4, &[10, 20, 30, 40]);
    let output = shared(&device, 16);
    let library = get(Library::compile_source(&device, TEXEL_SHADER_SOURCE, None));
    let function = get(Function::find(&library, "read_swizzle"));
    let pipeline = get(ComputePipeline::create(&function, None, false));
    for (name, source, expected) in [("identity", &plain, [10i32, 20, 30, 40]), ("base", &texture, [10, 255, 255, 20])] {
        let commands = get(CommandBuffer::create(&queue));
        let encoder = get(ComputeEncoder::create(&commands));
        get(encoder.set_pipeline(&pipeline));
        get(encoder.set_texture(0, source));
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
    get(texture.destroy());
    // A memoryless texture is a render target, a buffer is not memoryless.
    let memoryless = Texture::descriptor_2d(F::Depth32Float, 4, 4, StorageMode::Memoryless, &[TextureUsage::RenderTarget], None);
    if get(device.supports_family(DeviceFamily::Apple7)) {
        let made = get(Texture::create(&device, memoryless.clone()));
        get(made.destroy());
    }
    without_handles("a memoryless texture is a render target, a buffer is not memoryless", || {
        reject(ErrorKind::InvalidArgument, Texture::create(&device, TextureDescriptor { usage: vec![TextureUsage::ShaderRead], ..memoryless.clone() }));
        reject(ErrorKind::InvalidArgument, Buffer::create(&device, 16, StorageMode::Memoryless, CpuCacheMode::DefaultCache, None));
    });
    without_handles("invalid writable swizzles", || {
        for usage in [TextureUsage::ShaderWrite, TextureUsage::ShaderAtomic] {
            let descriptor = TextureDescriptor { swizzle: base_swizzle, ..Texture::descriptor_2d(F::Rgba8Unorm, 4, 4, StorageMode::Private, &[usage], None) };
            reject(ErrorKind::InvalidArgument, Texture::create(&device, descriptor));
        }
    });
    let lossy = TextureDescriptor {
        compression: CompressionType::Lossy,
        ..Texture::descriptor_2d(F::Rgba8Unorm, 64, 64, StorageMode::Private, &[TextureUsage::RenderTarget], None)
    };
    without_handles("invalid lossy texture descriptors", || {
        let invalid = [
            TextureDescriptor { storage: StorageMode::Shared, ..lossy.clone() },
            TextureDescriptor { allow_gpu_optimized_contents: false, ..lossy.clone() },
            TextureDescriptor { usage: vec![TextureUsage::PixelFormatView], ..lossy.clone() },
            TextureDescriptor { usage: vec![TextureUsage::ShaderWrite], ..lossy.clone() },
            TextureDescriptor { usage: vec![TextureUsage::ShaderAtomic], ..lossy.clone() },
            TextureDescriptor { kind: TextureKind::Texture1d, height: 1, ..lossy.clone() },
        ];
        for descriptor in invalid { reject(ErrorKind::InvalidArgument, Texture::create(&device, descriptor)); }
    });
    without_handles("lossy compression is unsupported for retained formats", || {
        reject(ErrorKind::InvalidArgument, Texture::create(&device, lossy.clone()));
    });
    get(queue.destroy());
    get(device.destroy());
}

fn gpu_texture_transfer_descriptors() {
    use m::metal_format::Format as F;
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    let base = Texture::descriptor_2d(F::Rgba8Unorm, 4, 4, StorageMode::Shared, &[TextureUsage::ShaderRead, TextureUsage::PixelFormatView], Some("Metal conformance texture"));
    let texture_descriptor = TextureDescriptor { mip_levels: 3, ..base };
    let c0 = stats();
    drop(get(Texture::create(&device, texture_descriptor.clone())));
    let s = settle(c0.live_handles);
    assert!(s.total_created - c0.total_created == 1 && s.total_released - c0.total_released == 1, "texture finalization");
    let texture = get(Texture::create(&device, texture_descriptor.clone()));
    assert_eq!(texture.descriptor().mip_levels, 3, "2D texture mip cardinality is wrong");
    assert_eq!(texture.descriptor().hazard_tracking, HazardTrackingMode::Tracked, "direct texture resource properties are wrong");
    assert_eq!(texture.descriptor().label.as_deref(), Some("Metal conformance texture"), "texture label did not round-trip");
    assert!(texture.device().same(&device) && !texture.is_destroyed(), "texture identity is wrong");
    reject(ErrorKind::ParentHasDependents, device.destroy());
    // Rows of sixteen bytes carried at a pitch of twenty: the pad never reaches the texture and reads back as zero.
    let mut texture_bytes = vec![0xeeu8; 80];
    for row in 0..4 {
        for column_byte in 0..16 { texture_bytes[row * 20 + column_byte] = (row * 16 + column_byte) as u8; }
    }
    let full = full_region(4, 4);
    upload(&device, &queue, &texture, 0, full, 20, &texture_bytes);
    let texture_copy = get(texture.read_bytes(full, 0, 0, 20, 80));
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
    get(texture.read_bytes_into(full, 0, 0, 16, 64, &mut into));
    for row in 0..4 {
        assert!(into[row * 16..row * 16 + 16] == texture_bytes[row * 20..row * 20 + 16], "texture read into caller storage changed pixel data");
    }
    reject(ErrorKind::InvalidArgument, texture.read_bytes_into(full, 0, 0, 16, 64, &mut vec![0u8; 63]));
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 0, 15, 60));
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 0, 20, 79));
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 0, usize::MAX, usize::MAX));
    reject(ErrorKind::InvalidArgument, texture.read_bytes(TextureRegion { x: 3, width: 2, ..full }, 0, 0, 8, 32));
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 3, 0, 16, 64));
    reject(ErrorKind::InvalidArgument, texture.read_bytes(full, 0, 1, 16, 64));
    // A mip level is its own upload and read; the base level stays as it was.
    let mip_region = full_region(2, 2);
    let mip_bytes: Vec<u8> = (0..16).map(|index| (index + 20) as u8).collect();
    upload(&device, &queue, &texture, 1, mip_region, 8, &mip_bytes);
    assert_eq!(get(texture.read_bytes(mip_region, 1, 0, 8, 16)), mip_bytes, "texture mip transfer did not round-trip");
    assert_eq!(get(texture.read_bytes(full, 0, 0, 16, 64))[0..16], texture_bytes[0..16], "a mip upload overwrote the base level");
    let private_texture = get(Texture::create(&device, Texture::descriptor_2d(F::Bgra8Unorm, 4, 4, StorageMode::Private, &[TextureUsage::RenderTarget], None)));
    reject(ErrorKind::Unsupported, private_texture.read_bytes(full, 0, 0, 16, 64));
    get(private_texture.destroy());
    without_handles("invalid texture descriptors", || {
        let invalid = [
            TextureDescriptor { width: 0, ..texture_descriptor.clone() },
            TextureDescriptor { mip_levels: 9, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::Texture1d, height: 2, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { sample_count: 2, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { usage: vec![TextureUsage::ShaderRead, TextureUsage::ShaderRead], ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::TextureBuffer, height: 1, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::Texture2dArray, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::TextureCube, height: 2, mip_levels: 1, ..texture_descriptor.clone() },
            TextureDescriptor { kind: TextureKind::Texture3d, array_length: 2, mip_levels: 1, ..texture_descriptor.clone() },
        ];
        for descriptor in invalid { reject(ErrorKind::InvalidArgument, Texture::create(&device, descriptor)); }
    });
    // Metal refuses an invalid label string (0xFF bytes); a Rust String cannot hold one, so that case is not ported.
    if get(device.supports_texture_sample_count(2)) {
        let multisample = get(Texture::create(&device, TextureDescriptor {
            kind: TextureKind::Texture2dMultisample, mip_levels: 1, sample_count: 2, storage: StorageMode::Private,
            usage: vec![TextureUsage::RenderTarget], label: None, ..texture_descriptor.clone()
        }));
        reject(ErrorKind::Unsupported, multisample.read_bytes(full, 0, 0, 16, 64));
        get(multisample.destroy());
    }
    // A cube has six slices and no seventh.
    let cube = get(Texture::create(&device, TextureDescriptor { kind: TextureKind::TextureCube, ..Texture::descriptor_2d(F::Rgba8Unorm, 4, 4, StorageMode::Shared, &[TextureUsage::ShaderRead], None) }));
    get(cube.read_bytes(full, 0, 5, 16, 64));
    reject(ErrorKind::InvalidArgument, cube.read_bytes(full, 0, 6, 16, 64));
    get(cube.destroy());
    get(texture.destroy());
    get(texture.destroy());
    assert!(texture.is_destroyed(), "destroyed texture remained live");
    reject(ErrorKind::Destroyed, texture.read_bytes(full, 0, 0, 16, 64));
    get(queue.destroy());
    get(device.destroy());
}

// What the blit refuses, what it holds while encoded, and that a kernel then reads the copied texel.
fn gpu_texture_blit() {
    use m::metal_format::Format as F;
    let device = get(Device::system_default());
    let queue = get(CommandQueue::create(&device));
    let texture = get(Texture::create(&device, Texture::descriptor_2d(F::R8Unorm, 4, 4, StorageMode::Private, &[TextureUsage::ShaderRead, TextureUsage::ShaderWrite], None)));
    let volume = get(Texture::create(&device, TextureDescriptor { kind: TextureKind::Texture3d, depth: 2, ..Texture::descriptor_2d(F::R8Unorm, 1, 1, StorageMode::Private, &[TextureUsage::ShaderRead], None) }));
    let mut staging_bytes = vec![0u8; 16];
    staging_bytes[0] = 37;
    let staging = shared(&device, 16);
    get(staging.write_bytes(0, &staging_bytes));
    let region = full_region(4, 4);
    let commands = get(CommandBuffer::create(&queue));
    let blit = get(BlitEncoder::create(&commands));
    reject(ErrorKind::InvalidState, BlitEncoder::create(&commands));
    reject(ErrorKind::InvalidState, commands.commit());
    let copy = |bytes_per_row: usize, bytes_per_image: usize, destination: &Texture, slice: usize, level: usize, region: TextureRegion| {
        blit.copy_buffer_to_texture(&staging, 0, bytes_per_row, bytes_per_image, destination, slice, level, region)
    };
    without_handles("refused blits", || {
        reject(ErrorKind::InvalidArgument, copy(1, usize::MAX, &volume, 0, 0, TextureRegion { depth: 2, ..full_region(1, 1) }));
        reject(ErrorKind::InvalidArgument, copy(3, 16, &texture, 0, 0, region));
        reject(ErrorKind::InvalidArgument, copy(4, 15, &texture, 0, 0, region));
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 0, 1, region));
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 1, 0, region));
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 0, 0, TextureRegion { x: 1, ..region }));
        reject(ErrorKind::InvalidArgument, copy(4, 16, &texture, 0, 0, TextureRegion { width: 0, ..region }));
    });
    // A source offset of -1 is not expressible: source_offset is unsigned in Rust, so that case is not ported.
    get(copy(4, 16, &texture, 0, 0, region));
    reject(ErrorKind::ParentHasDependents, staging.destroy());
    reject(ErrorKind::ParentHasDependents, texture.destroy());
    get(blit.end_encoding());
    reject(ErrorKind::Destroyed, copy(4, 16, &texture, 0, 0, region));
    reject(ErrorKind::Destroyed, blit.end_encoding());
    complete(&commands);
    get(staging.destroy());
    let library = get(Library::compile_source(&device, TEXEL_SHADER_SOURCE, None));
    let read_function = get(Function::find(&library, "texel_read"));
    let read_pipeline = get(ComputePipeline::create(&read_function, None, false));
    let output = shared(&device, 4);
    let commands = get(CommandBuffer::create(&queue));
    let encoder = get(ComputeEncoder::create(&commands));
    get(encoder.set_pipeline(&read_pipeline));
    reject(ErrorKind::InvalidArgument, encoder.set_texture(31, &texture));
    get(encoder.set_texture(0, &texture));
    get(encoder.set_buffer(0, 0, &output));
    get(encoder.dispatch_threads((1, 1, 1), (1, 1, 1)));
    get(encoder.end_encoding());
    complete(&commands);
    let value = i32::from_le_bytes(get(output.read_bytes(0, 4)).try_into().unwrap());
    assert_eq!(value, 37, "blitted texel read back as {value}, expected 37");
    get(output.destroy());
    get(read_pipeline.destroy());
    get(read_function.destroy());
    get(library.destroy());
    get(volume.destroy());
    get(texture.destroy());
    get(queue.destroy());
    get(device.destroy());
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
    let pipeline = get(ComputePipeline::create(function, None, false));
    let output = shared(device, 4);
    get(output.write_bytes(0, &initial.to_le_bytes()));
    let commands = get(CommandBuffer::create(queue));
    let encoder = get(ComputeEncoder::create(&commands));
    get(encoder.set_pipeline(&pipeline));
    get(encoder.set_buffer(0, 0, &output));
    get(encoder.dispatch_threads((1, 1, 1), (1, 1, 1)));
    get(encoder.end_encoding());
    complete(&commands);
    let result = i32::from_le_bytes(get(output.read_bytes(0, 4)).try_into().unwrap());
    get(output.destroy());
    get(pipeline.destroy());
    result
}

fn gpu_function_constants() {
    let device = get(Device::system_default());
    match Library::compile_source(&device, "not a Metal program", Some("invalid shader diagnostic")) {
        Err(e) if e.kind == ErrorKind::NativeError && e.message.contains("invalid shader diagnostic") => {}
        Err(e) => panic!("shader compilation lost its full labeled diagnostic: {e}"),
        Ok(_) => panic!("an invalid shader compiled"),
    }
    let library = get(Library::compile_source(&device, CONSTANTS_SOURCE, Some("Metal conformance library")));
    let function_ = get(Function::find(&library, "increment"));
    without_handles("invalid function lookups", || {
        reject(ErrorKind::InvalidArgument, Function::find(&library, ""));
        reject(ErrorKind::InvalidArgument, Function::find(&library, "bad\0name"));
    });
    without_handles("invalid function constants", || {
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(1)), c("increment_amount", FunctionConstantValue::Uint32(2))], "specialized_increment"));
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(0x1_0000_0000))], "specialized_increment"));
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("", FunctionConstantValue::Uint32(1))], "specialized_increment"));
        reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[], ""));
        for value in [FunctionConstantValue::Int8(128), FunctionConstantValue::Uint8(-1), FunctionConstantValue::Int16(32_768), FunctionConstantValue::Uint16(65_536)] {
            reject(ErrorKind::InvalidArgument, Function::specialize(&library, &[c("scalar_i8", value)], "scalar_constant_sum"));
        }
    });
    // A constant of the wrong type is Metal's to refuse.
    reject(ErrorKind::NativeError, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Bool(true))], "specialized_increment"));
    let specialized_function = get(Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(7)), c("apply_twice", FunctionConstantValue::Bool(true))], "specialized_increment"));
    let scalar_constant_function = get(Function::specialize(&library, &[
        c("scalar_i8", FunctionConstantValue::Int8(-2)), c("scalar_u8", FunctionConstantValue::Uint8(3)),
        c("scalar_i16", FunctionConstantValue::Int16(-4)), c("scalar_u16", FunctionConstantValue::Uint16(5)),
        c("scalar_i32", FunctionConstantValue::Int32(-6)), c("scalar_u32", FunctionConstantValue::Uint32(7)),
        c("scalar_i64", FunctionConstantValue::Int64(-8)), c("scalar_u64", FunctionConstantValue::Uint64Bits(9)),
        c("scalar_f16", FunctionConstantValue::Float16(10.0)), c("scalar_f32", FunctionConstantValue::Float32(11.0)),
        c("scalar_bool", FunctionConstantValue::Bool(true)),
    ], "scalar_constant_sum"));
    let stale_function = get(Function::find(&library, "increment"));
    get(stale_function.destroy());
    reject(ErrorKind::Destroyed, ComputePipeline::create(&stale_function, None, false));
    reject(ErrorKind::Destroyed, stale_function.argument_encoder(0));
    without_handles("invalid compute descriptors", || {
        reject(ErrorKind::InvalidArgument, ComputePipeline::create(&specialized_function, Some("invalid\0label"), false));
    });
    reject(ErrorKind::ParentHasDependents, library.destroy());
    let pipeline = get(ComputePipeline::create(&function_, None, false));
    let specialized_pipeline = get(ComputePipeline::create(&specialized_function, Some("specialized reflected pipeline"), true));
    let scalar_constant_pipeline = get(ComputePipeline::create(&scalar_constant_function, None, false));
    assert!(pipeline.bindings().is_none(), "reflection was kept without being asked for");
    let reflected = specialized_pipeline.bindings().unwrap_or_else(|| panic!("requested compute pipeline reflection is missing"));
    assert_eq!(
        reflected.iter().filter_map(buffer_binding).collect::<Vec<_>>(),
        vec![("values".to_owned(), 0, BindingAccess::ReadWrite, ShaderDataType::Scalar(ScalarType::Uint))],
        "compute reflection does not describe the one read-write uint buffer",
    );
    assert_eq!(reflected.len(), 1, "compute reflection invented a binding");
    let buffer = shared(&device, 16);
    get(buffer.write_bytes(0, &i32_bytes(&[1, 41, 99, -2])));
    let specialized_buffer = shared(&device, 4);
    get(specialized_buffer.write_bytes(0, &i32_bytes(&[10])));
    let scalar_constant_buffer = shared(&device, 4);
    let queue = get(CommandQueue::create(&device));
    let commands = get(CommandBuffer::create(&queue));
    assert!(commands.device().same(&device), "command buffer device drift");
    assert!(matches!(get(commands.status()), CommandBufferStatus::NotEnqueued), "a new command buffer is already enqueued");
    reject(ErrorKind::InvalidState, commands.wait_until_completed());
    let encoder = get(ComputeEncoder::create(&commands));
    reject(ErrorKind::InvalidState, ComputeEncoder::create(&commands));
    get(encoder.set_pipeline(&pipeline));
    assert!(get(buffer.read_bytes(0, 16)) == i32_bytes(&[1, 41, 99, -2]), "compute input changed before command encoding");
    get(encoder.set_buffer(0, 0, &buffer));
    reject(ErrorKind::ParentHasDependents, buffer.destroy());
    reject(ErrorKind::InvalidArgument, encoder.dispatch_threads((4, 1, 1), (usize::MAX, 2, 1)));
    get(encoder.dispatch_threads((4, 1, 1), (4, 1, 1)));
    get(encoder.set_pipeline(&specialized_pipeline));
    get(encoder.set_buffer(0, 0, &specialized_buffer));
    get(encoder.dispatch_threads((1, 1, 1), (1, 1, 1)));
    get(encoder.set_pipeline(&scalar_constant_pipeline));
    get(encoder.set_buffer(0, 0, &scalar_constant_buffer));
    get(encoder.dispatch_threads((1, 1, 1), (1, 1, 1)));
    reject(ErrorKind::InvalidState, commands.commit());
    get(encoder.end_encoding());
    get(commands.commit());
    reject(ErrorKind::InvalidState, commands.commit());
    reject(ErrorKind::InvalidState, ComputeEncoder::create(&commands));
    get(commands.wait_until_completed());
    assert!(matches!(get(commands.status()), CommandBufferStatus::Completed), "command buffer did not complete");
    let output = get(buffer.read_bytes(0, 16));
    for (index, expected) in [2i32, 42, 100, -1].into_iter().enumerate() {
        let actual = i32::from_le_bytes(output[index * 4..index * 4 + 4].try_into().unwrap());
        assert_eq!(actual, expected, "compute output {index}: expected {expected}, got {actual}");
    }
    let specialized_value = i32::from_le_bytes(get(specialized_buffer.read_bytes(0, 4)).try_into().unwrap());
    assert_eq!(specialized_value, 24, "specialized function constants did not execute on the GPU");
    let scalar_value = i32::from_le_bytes(get(scalar_constant_buffer.read_bytes(0, 4)).try_into().unwrap());
    assert_eq!(scalar_value, 26, "typed scalar function constants did not round-trip to the GPU");
    get(scalar_constant_buffer.destroy());
    get(specialized_buffer.destroy());
    get(buffer.destroy());
    get(commands.destroy());
    assert!(commands.destroyed(), "destroyed command buffer remained live");
    // The values a constant carries are the ones passed, each call its own: 7 and 11 give 711, and a later specialization does not see them.
    let copy = |first: i32, second: i32| -> i32 {
        let specialized = get(Function::specialize(&library, &[c("first", FunctionConstantValue::Int32(first)), c("second", FunctionConstantValue::Int32(second))], "constant_copy"));
        let result = run_once(&device, &queue, &specialized, -1);
        get(specialized.destroy());
        result
    };
    assert_eq!(copy(7, 11), 711, "function constants did not copy the original typed values");
    assert_eq!(copy(0, 0), 0, "a later specialization kept stale function constant values");
    assert_eq!(copy(-3, 5), -295, "signed function constants lost their sign");
    get(queue.destroy());
    get(scalar_constant_pipeline.destroy());
    get(specialized_pipeline.destroy());
    get(pipeline.destroy());
    get(scalar_constant_function.destroy());
    get(specialized_function.destroy());
    get(function_.destroy());
    get(library.destroy());
    reject(ErrorKind::Destroyed, Function::find(&library, "increment"));
    reject(ErrorKind::Destroyed, Function::specialize(&library, &[c("increment_amount", FunctionConstantValue::Uint32(1))], "specialized_increment"));
    get(device.destroy());
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
    let device = get(Device::system_default());
    if !get(device.supports_family(DeviceFamily::Metal4)) {
        without_handles("Metal 4 compiler on an older device", || reject(ErrorKind::Unsupported, Compiler::create(&device)));
        get(device.destroy());
        return;
    }
    let c0 = stats();
    drop(get(Compiler::create(&device)));
    let s = settle(c0.live_handles);
    assert!(s.total_created - c0.total_created == 1 && s.total_released - c0.total_released == 1, "compiler finalization");
    let compiler = get(Compiler::create(&device));
    reject(ErrorKind::ParentHasDependents, device.destroy());
    let library = get(Library::compile_source(&device, RENDER_SOURCE, None));
    let fragment_options = || RenderPipelineOptions { fragment: Some("prismel_fragment".to_owned()), ..RenderPipelineOptions::default() };
    without_handles("invalid Metal 4 render inputs", || {
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "", &fragment_options()));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "missing_vertex", &fragment_options()));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { label: Some("invalid\0render".to_owned()), ..fragment_options() }));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { fragment: Some("missing_fragment".to_owned()), ..RenderPipelineOptions::default() }));
        // Rasterization needs a fragment function and one to eight colours.
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions::default()));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex_only", &RenderPipelineOptions { color_formats: Some(vec![]), ..RenderPipelineOptions::default() }));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { color_formats: Some(vec![]), ..fragment_options() }));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { raster_sample_count: 0, ..fragment_options() }));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { color_formats: Some(vec![Format::Bgra8Unorm; 9]), ..fragment_options() }));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { color_formats: Some(vec![Format::Depth32Float]), ..fragment_options() }));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
            color_formats: Some(vec![Format::Bgra8Unorm]), color_attachments: Some(vec![ColorAttachment::new(Format::Bgra8Unorm)]), ..fragment_options()
        }));
        reject(ErrorKind::InvalidArgument, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
            color_attachments: Some(vec![ColorAttachment { write_mask: vec![ColorWrite::Red, ColorWrite::Red], ..ColorAttachment::new(Format::Bgra8Unorm) }]), ..fragment_options()
        }));
        // A stage given a function of another stage is Metal's to refuse.
        reject(ErrorKind::NativeError, compiler.create_render_pipeline(&library, "prismel_fragment", &fragment_options()));
        reject(ErrorKind::NativeError, compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions { fragment: Some("prismel_vertex".to_owned()), ..RenderPipelineOptions::default() }));
        reject(ErrorKind::NativeError, compiler.create_render_pipeline(&library, "prismel_kernel", &fragment_options()));
    });
    let render_pipeline = get(compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
        label: Some("metal4 reflected render".to_owned()), reflection: true, primitive_topology: PrimitiveTopology::Triangle,
        support_indirect_command_buffers: true, ..fragment_options()
    }));
    assert!(get(render_pipeline.supports_indirect_command_buffers()), "a pipeline compiled for indirect command buffers denies it");
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
    let plain_pipeline = get(compiler.create_render_pipeline(&library, "prismel_vertex", &RenderPipelineOptions {
        color_formats: Some(vec![Format::Rgba8Unorm]), ..fragment_options()
    }));
    assert!(plain_pipeline.reflection().is_none(), "render reflection was kept without being asked for");
    assert!(!get(plain_pipeline.supports_indirect_command_buffers()), "a pipeline compiled without indirect command buffers claims them");
    let c1 = stats();
    drop(get(compiler.create_render_pipeline(&library, "prismel_vertex", &fragment_options())));
    let s = settle(c1.live_handles);
    assert!(s.total_created - c1.total_created == 1 && s.total_released - c1.total_released == 1, "render pipeline finalization");
    // One covering triangle, the tint in a fragment buffer: the pipeline's output is the tint, to the byte.
    let queue = get(CommandQueue::create(&device));
    let target = get(Texture::create(&device, Texture::descriptor_2d(Format::Rgba8Unorm, 2, 2, StorageMode::Shared, &[TextureUsage::RenderTarget], None)));
    let offset = shared(&device, 16);
    get(offset.write_bytes(0, &f32_bytes(&[0.0, 0.0, 0.0, 0.0])));
    let tint = shared(&device, 16);
    get(tint.write_bytes(0, &f32_bytes(&[0.25, 0.5, 0.75, 1.0])));
    let pass = get(RenderPassDescriptor::create(&device, 2, 2, 1));
    get(pass.set_attachments(&target, None, None, [0.0; 4]));
    let commands = get(CommandBuffer::create(&queue));
    let encoder = get(RenderEncoder::create(&commands, &pass));
    // The reflected pipeline targets Bgra8, this pass Rgba8.
    reject(ErrorKind::InvalidArgument, encoder.set_pipeline(&render_pipeline));
    get(encoder.set_pipeline(&plain_pipeline));
    reject(ErrorKind::ParentHasDependents, plain_pipeline.destroy());
    get(encoder.set_vertex_buffer(0, 0, &offset));
    get(encoder.set_fragment_buffer(1, 0, &tint));
    get(encoder.draw_triangles(0, 3));
    // The same triangle again, its indices and counts read from buffers; a misaligned argument offset is refused before encoding.
    let indices = shared(&device, 12);
    get(indices.write_bytes(0, &i32_bytes(&[0, 1, 2])));
    let arguments = shared(&device, 24);
    get(arguments.write_bytes(0, &i32_bytes(&[0, 3, 1, 0, 0, 0])));
    reject(ErrorKind::InvalidArgument, encoder.draw_indexed_indirect(Primitive::Triangle, IndexType::Uint32, &indices, 0, &arguments, 2));
    get(encoder.draw_indexed_indirect(Primitive::Triangle, IndexType::Uint32, &indices, 0, &arguments, 4));
    get(encoder.end_encoding());
    complete(&commands);
    get(arguments.destroy());
    get(indices.destroy());
    let pixels = get(target.read_bytes(full_region(2, 2), 0, 0, 8, 16));
    for pixel in 0..4 {
        let bytes = &pixels[pixel * 4..pixel * 4 + 4];
        assert_eq!(bytes, [0x40, 0x80, 0xbf, 0xff], "compiled pipeline drew {bytes:02x?} at pixel {pixel}");
    }
    get(pass.destroy());
    get(tint.destroy());
    get(offset.destroy());
    get(target.destroy());
    get(queue.destroy());
    get(plain_pipeline.destroy());
    get(render_pipeline.destroy());
    reject(ErrorKind::Destroyed, render_pipeline.supports_indirect_command_buffers());
    get(render_pipeline.destroy());
    // A pipeline outlives the library and compiler that made it.
    let survivor = get(compiler.create_render_pipeline(&library, "prismel_vertex", &fragment_options()));
    get(library.destroy());
    reject(ErrorKind::Destroyed, compiler.create_render_pipeline(&library, "prismel_vertex", &fragment_options()));
    get(compiler.destroy());
    get(compiler.destroy());
    get(survivor.supports_indirect_command_buffers());
    get(survivor.destroy());
    get(device.destroy());
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
    let device = get(Device::system_default());
    let library = get(Library::compile_source(&device, ICB_SOURCE, None));
    let fragment = get(Function::find(&library, "fragment_main"));
    let mut encoder = get(fragment.argument_encoder(1));
    let sampled = get(Texture::create(&device, Texture::descriptor_2d(Format::Rgba8Unorm, 1, 1, StorageMode::Shared, &[TextureUsage::ShaderRead], None)));
    let sampler = get(Sampler::create(&device, SamplerDescriptor { support_argument_buffers: true, ..SamplerDescriptor::default() }));
    let argument_buffer = shared(&device, encoder.encoded_length());
    get(encoder.set_argument_buffer(&argument_buffer, 0));
    get(encoder.set(0, ShaderArgumentResource::Texture(sampled.clone())));
    get(encoder.set(1, ShaderArgumentResource::Sampler(sampler.clone())));
    get(encoder.destroy());
    get(argument_buffer.destroy());
    get(sampler.destroy());
    get(sampled.destroy());
    get(fragment.destroy());
    get(library.destroy());
    get(device.destroy());
}

// Argument reflection snapshot over a mock native layer: bounded conversion, and the native handle count
// is the same after success and after each rejection.
mod mock_reflection {
    use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
    use m::metal_argument_reflection_snapshot::{Kind, Raw};
    pub static LIVE: AtomicI64 = AtomicI64::new(0);
    pub static FAIL_MEMBER_NAME: AtomicBool = AtomicBool::new(false);
    #[derive(Clone)]
    pub enum Node { Scalar, TextureReference, Array(Box<Node>), Struct(Vec<(String, Node)>), Member(String, Box<Node>) }
    #[derive(Clone)]
    pub struct Handle { node: Node, owned: bool }
    pub fn root(node: Node) -> Handle { Handle { node, owned: false } }
    fn own(node: Node) -> Handle { LIVE.fetch_add(1, Ordering::SeqCst); Handle { node, owned: true } }
    pub struct Mock;
    impl Raw for Mock {
        type Handle = Handle;
        fn destroy(handle: Handle) { if handle.owned { LIVE.fetch_sub(1, Ordering::SeqCst); } }
        fn kind(h: &Handle) -> Result<Kind, String> {
            Ok(match h.node { Node::Scalar | Node::Member(..) => Kind::Scalar, Node::TextureReference => Kind::TextureReference, Node::Array(_) => Kind::Array, Node::Struct(_) => Kind::Struct })
        }
        fn data_type(_: &Handle) -> Result<i64, String> { Ok(1) }
        fn array_length(_: &Handle) -> Result<i64, String> { Ok(1) }
        fn array_stride(_: &Handle) -> Result<i64, String> { Ok(4) }
        fn argument_index_stride(_: &Handle) -> Result<i64, String> { Ok(1) }
        fn element(h: &Handle) -> Result<Option<Handle>, String> {
            Ok(match &h.node { Node::Array(child) => Some(own((**child).clone())), _ => None })
        }
        fn pointer_access(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn alignment(_: &Handle) -> Result<i64, String> { Ok(4) }
        fn data_size(_: &Handle) -> Result<i64, String> { Ok(4) }
        fn element_is_argument_buffer(_: &Handle) -> Result<bool, String> { Ok(false) }
        fn members(h: &Handle) -> Result<Vec<Handle>, String> {
            Ok(match &h.node {
                Node::Struct(members) => members.iter().map(|(name, node)| own(Node::Member(name.clone(), Box::new(node.clone())))).collect(),
                _ => Vec::new(),
            })
        }
        fn member_name(h: &Handle) -> Result<String, String> {
            if FAIL_MEMBER_NAME.load(Ordering::SeqCst) { return Err("injected member-name failure".to_owned()); }
            match &h.node { Node::Member(name, _) => Ok(name.clone()), _ => Err("not member".to_owned()) }
        }
        fn member_offset(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn member_argument_index(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn member_type(h: &Handle) -> Result<Option<Handle>, String> {
            Ok(match &h.node { Node::Member(_, child) => Some(own((**child).clone())), _ => None })
        }
        fn texture_access(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn texture_type(_: &Handle) -> Result<i64, String> { Ok(2) }
        fn is_depth_texture(_: &Handle) -> Result<bool, String> { Ok(false) }
        fn tensor_access(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn tensor_data_type(_: &Handle) -> Result<i64, String> { Ok(3) }
        fn tensor_index_type(_: &Handle) -> Result<i64, String> { Ok(4) }
        fn tensor_dimensions(_: &Handle) -> Result<Option<Vec<i64>>, String> { Ok(Some(vec![2, 3])) }
        fn argument_name(_: &Handle) -> Result<String, String> { Ok("items".to_owned()) }
        fn argument_index(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn argument_active(_: &Handle) -> Result<bool, String> { Ok(true) }
        fn argument_access(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn argument_type(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn argument_array_length(_: &Handle) -> Result<i64, String> { Ok(1) }
        fn argument_buffer_alignment(_: &Handle) -> Result<i64, String> { Ok(16) }
        fn argument_buffer_data_size(_: &Handle) -> Result<i64, String> { Ok(16) }
        fn argument_buffer_data_type(_: &Handle) -> Result<i64, String> { Ok(1) }
        fn argument_texture_data_type(_: &Handle) -> Result<i64, String> { Ok(1) }
        fn argument_texture_type(_: &Handle) -> Result<i64, String> { Ok(2) }
        fn argument_is_depth_texture(_: &Handle) -> Result<bool, String> { Ok(false) }
        fn argument_threadgroup_memory_alignment(_: &Handle) -> Result<i64, String> { Ok(16) }
        fn argument_threadgroup_memory_data_size(_: &Handle) -> Result<i64, String> { Ok(0) }
        fn argument_buffer_pointer_type(_: &Handle) -> Result<Option<Handle>, String> { Ok(Some(own(Node::Array(Box::new(Node::Scalar))))) }
        fn argument_buffer_struct_type(_: &Handle) -> Result<Option<Handle>, String> { Ok(Some(own(Node::Struct(vec![("x".to_owned(), Node::Scalar)])))) }
    }
}

fn gpu_argument_reflection_snapshot() {
    use m::metal_argument_reflection_snapshot::{argument, reflected_type, reflected_type_with_limits, Limits};
    use m::reflection::ReflectedType;
    use mock_reflection::{root, Mock, Node, FAIL_MEMBER_NAME, LIVE};
    use std::sync::atomic::Ordering;
    let tree = root(Node::Struct(vec![("position".to_owned(), Node::Array(Box::new(Node::Scalar))), ("normal".to_owned(), Node::Scalar)]));
    let before = LIVE.load(Ordering::SeqCst);
    let no_delta = |label: &str| assert_eq!(LIVE.load(Ordering::SeqCst), before, "{label}: native handle delta");
    match reflected_type::<Mock>(&tree) {
        Ok(ReflectedType::Struct(members)) => assert_eq!(members.len(), 2, "member snapshot"),
        Ok(_) => panic!("unexpected reflection kind"),
        Err(message) => panic!("{message}"),
    }
    no_delta("success");
    assert!(reflected_type_with_limits::<Mock>(&tree, Limits { max_depth: 0, max_members: 8 }).is_err(), "depth limit accepted");
    no_delta("depth rejection");
    assert!(reflected_type_with_limits::<Mock>(&tree, Limits { max_depth: 8, max_members: 1 }).is_err(), "member limit accepted");
    no_delta("member rejection");
    FAIL_MEMBER_NAME.store(true, Ordering::SeqCst);
    let failed = reflected_type::<Mock>(&tree).is_err();
    FAIL_MEMBER_NAME.store(false, Ordering::SeqCst);
    assert!(failed, "injected failure accepted");
    no_delta("failure unwind");
    match argument::<Mock>(&root(Node::Scalar)) {
        Ok(argument) => assert_eq!(argument.name, "items", "argument snapshot"),
        Err(message) => panic!("{message}"),
    }
    no_delta("argument success");
    match reflected_type::<Mock>(&root(Node::TextureReference)) {
        Ok(ReflectedType::TextureReference { data_type: 1, access: 0, texture_type: 2, depth: false }) => {}
        Ok(_) => panic!("texture-reference snapshot mismatch"),
        Err(message) => panic!("{message}"),
    }
    no_delta("texture-reference success");
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
        ("gpu_texture_formats", gpu_texture_formats),
        ("gpu_texture_swizzle_compression", gpu_texture_swizzle_compression),
        ("gpu_texture_transfer_descriptors", gpu_texture_transfer_descriptors),
        ("gpu_texture_blit", gpu_texture_blit),
        ("gpu_function_constants", gpu_function_constants),
        ("gpu_render_pipeline", gpu_render_pipeline),
        ("gpu_render_icb_argument", gpu_render_icb_argument),
        ("gpu_argument_reflection_snapshot", gpu_argument_reflection_snapshot),
    ]);
}
