// Port of lib/ogpu_metal/test_device_error.ml, test_ogpu_metal_{foundation,texture,pipeline,acceleration,
// capabilities,sampler_pixels}.ml and test_pipeline_argument_buffer.ml: the native modules and the driver
// record on the real Metal device. Direct native module handles follow their last Rust owner; core
// wrappers release driver payloads at logical destroy, even when dead wrappers remain in scope.
use ogpu_core::{
    backend as be,
    binding::{self, Kind as LayoutKind, LayoutEntry, Stage as LayoutStage},
    caps,
    error::{self, Error, ErrorKind},
    pipeline::{Blend, ColorFormat, DepthFormat, RenderDescriptor},
    render_pass::{Load, Primitive, Store},
    shader::{self, Binding, BindingKind, EntryPoint, Shader, Stage},
    types::{
        AddressMode, BufferDescriptor, BufferUsage, Extent, Memory, MipFilter, SamplerDescriptor, SamplerFilter, TextureDescriptor,
        TextureFormat, TextureUsage,
    },
};
use ogpu_metal::{
    backend,
    buffer::{self, Buffer},
    device::Device,
    impl_, library, pipeline, texture,
};

fn die(failure: Error) -> ! {
    panic!("{}", error::to_string(&failure))
}

fn expect(kind: ErrorKind, result: Option<Error>) {
    match result {
        Some(failure) if failure.kind == kind => (),
        Some(failure) => panic!("wrong rejection: {}", error::to_string(&failure)),
        None => panic!("wrong rejection: accepted"),
    }
}

/// Native handles alive after the release queue drains.
fn live() -> usize {
    ogpu_metal::native::release_queue::drain().expect("drain");
    ogpu_metal::native::release_queue::stats().expect("stats").live_handles
}

fn buffer_descriptor(size: i64, usage: &[BufferUsage]) -> BufferDescriptor {
    BufferDescriptor { label: Some("ogpu-metal-foundation".to_owned()), size, usage: usage.to_vec() }
}

fn put_f32(bytes: &mut [u8], offset: usize, value: f64) {
    bytes[offset..offset + 4].copy_from_slice(&(value as f32).to_le_bytes());
}

fn make_shader(label: &str, source: &str, entries: &[(&str, Stage)], bindings: Vec<Binding>) -> Shader {
    shader::create(shader::Descriptor {
        backend: "metal".to_owned(),
        label: Some(label.to_owned()),
        bytes: source.as_bytes().to_vec(),
        entry_points: entries.iter().map(|(name, stage)| EntryPoint { name: (*name).to_owned(), stage: *stage }).collect(),
        bindings,
    }).unwrap_or_else(|failure| die(failure))
}

fn empty_layout(device: &Device) -> binding::PipelineLayout {
    let group = binding::create_layout(&[]).unwrap_or_else(|failure| die(failure));
    binding::create_pipeline_layout(device.handle(), &device.capabilities(), &[(0, group)]).unwrap_or_else(|failure| die(failure))
}

fn gpu_device_error() {
    let wrong_thread = std::thread::spawn(|| be::create_device(backend::create()).map(|_| ())).join().unwrap();
    expect(ErrorKind::WrongDomain, wrong_thread.err());

}

fn gpu_foundation() {
    assert_eq!(impl_::BACKEND, "metal");
    let mut unsupported = caps::minimum_m1();
    unsupported.limits.max_buffer_size = 0;
    expect(ErrorKind::InvalidArgument, caps::validate(&unsupported).err());
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let other = Device::system_default().unwrap_or_else(|failure| die(failure));
    let before = live();
    expect(ErrorKind::InvalidArgument, buffer::create(&device, buffer::Memory::Shared, &buffer_descriptor(0, &[BufferUsage::CopySrc])).err());
    let values: Vec<Buffer> = [
        (buffer::Memory::DeviceLocal, BufferUsage::Storage),
        (buffer::Memory::Shared, BufferUsage::Uniform),
        (buffer::Memory::Upload, BufferUsage::CopySrc),
        (buffer::Memory::Readback, BufferUsage::CopyDst),
    ]
    .into_iter()
    .map(|(memory, usage)| buffer::create(&device, memory, &buffer_descriptor(64, &[usage])).unwrap_or_else(|failure| die(failure)))
    .collect();
    expect(ErrorKind::InvalidArgument, buffer::create(&device, buffer::Memory::Readback, &buffer_descriptor(64, &[BufferUsage::Uniform])).err());
    for value in &values {
        value.descriptor(&device).unwrap_or_else(|failure| die(failure));
    }
    expect(ErrorKind::CrossDevice, values[0].descriptor(&other).err());
    expect(ErrorKind::InvalidState, device.destroy().err());
    for value in &values {
        value.destroy().unwrap_or_else(|failure| die(failure));
    }
    for value in &values {
        expect(ErrorKind::StaleHandle, value.descriptor(&device).err());
    }
    device.destroy().unwrap_or_else(|failure| die(failure));
    other.destroy().unwrap_or_else(|failure| die(failure));
    drop((values, device, other));
    let after = live();
    assert_eq!(after + 2, before, "Metal live-handle delta");
    println!("ogpu_metal foundation: four memory classes, zero live-handle delta");
}

fn gpu_texture() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let other = Device::system_default().unwrap_or_else(|failure| die(failure));
    let before = live();
    let descriptor = TextureDescriptor {
        label: Some("ogpu-metal-texture".to_owned()),
        width: 4,
        height: 4,
        depth: 1,
        mip_levels: 1,
        sample_count: 1,
        format: TextureFormat::Rgba8Unorm,
        usage: vec![TextureUsage::TextureBinding, TextureUsage::TextureCopySrc, TextureUsage::TextureCopyDst],
    };
    let invalid = TextureDescriptor { mip_levels: 8, ..descriptor.clone() };
    expect(ErrorKind::InvalidArgument, texture::create(&device, Memory::Shared, TextureFormat::Rgba8Unorm, &invalid).err());
    let value = texture::create(&device, Memory::Shared, TextureFormat::Rgba8Unorm, &descriptor).unwrap_or_else(|failure| die(failure));
    let bytes = value.read_bytes(&device, 0, 16).unwrap_or_else(|failure| die(failure));
    assert_eq!(bytes.len(), 64, "texture readback length");
    let mut direct = vec![0xffu8; 64];
    value.read_bytes_into(&device, 0, 16, &mut direct).unwrap_or_else(|failure| die(failure));
    assert_eq!(direct, bytes, "texture direct read-into mismatch");
    expect(ErrorKind::InvalidArgument, value.read_bytes_into(&device, 0, 16, &mut [0u8; 63]).err());
    expect(ErrorKind::CrossDevice, value.descriptor(&other).err());
    expect(ErrorKind::InvalidState, device.destroy().err());
    value.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
    other.destroy().unwrap_or_else(|failure| die(failure));
    drop((value, device, other));
    assert_eq!(live() + 2, before, "texture live-handle delta");
    println!("ogpu_metal texture: both host read paths agree, typed rejections, zero live-handle delta");
}

fn gpu_capabilities() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let info = device.metal().info().expect("info");
    let reported = device.capabilities();
    assert!(
        reported.limits.max_buffer_size == info.max_buffer_length as i64 && reported.ray_tracing == info.raytracing,
        "reported capability differs from Metal probe"
    );
    let mut maximum = 1;
    for sample in [2usize, 4, 8] {
        if device.metal().supports_texture_sample_count(sample).expect("sample count") {
            maximum = sample as i64;
        }
    }
    assert_eq!(reported.limits.max_sample_count, maximum.min(4), "sample limit differs from qualified Metal probes");
    device.destroy().unwrap_or_else(|failure| die(failure));
}

const COMPUTE_SOURCE: &str = "#include <metal_stdlib>\nusing namespace metal;\nkernel void mapped_compute(device uint *values [[buffer(0)]], uint i [[thread_position_in_grid]]) { values[i] = values[i] * 2 + 3; }\n";

const RENDER_SOURCE: &str = "#include <metal_stdlib>\nusing namespace metal;\nstruct V { float4 position [[position]]; };\nvertex V mapped_vertex(uint i [[vertex_id]]) { constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}}; V v; v.position=float4(p[i],0.,1.); return v; }\nfragment float4 mapped_fragment() { return float4(0.125,0.5,0.875,1.); }\n";

fn gpu_destroy_releases_retained_driver_payloads() {
    let before = live();
    let device = be::create_device(backend::create()).unwrap_or_else(|failure| die(failure));
    let device_only = live();
    let queue = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
    let plain = be::create_buffer(&device, buffer_descriptor(8, &[BufferUsage::CopyDst]), None).unwrap_or_else(|failure| die(failure));
    let mapped = be::create_mapped_buffer(&device, buffer_descriptor(8, &[BufferUsage::Storage, BufferUsage::CopyDst])).unwrap_or_else(|failure| die(failure));
    let mapped_clone = mapped.clone();
    let id = be::buffer_id(&mapped);
    be::write_buffer(&mapped, 0, &[0; 8]).unwrap_or_else(|failure| die(failure));
    assert!(be::mapped_view(&mapped).is_some());
    let readback = be::owned_readback(&mapped).unwrap_or_else(|failure| die(failure));
    let texture = be::create_texture(&device, TextureDescriptor {
        label: None, width: 1, height: 1, depth: 1, mip_levels: 1, sample_count: 1,
        format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::TextureBinding],
    }).unwrap_or_else(|failure| die(failure));
    let sampler = be::create_sampler(&device, SamplerDescriptor {
        label: None, min_filter: SamplerFilter::Nearest, mag_filter: SamplerFilter::Nearest,
        mip_filter: MipFilter::NoMip, address_u: AddressMode::ClampToEdge, address_v: AddressMode::ClampToEdge,
        lod_min: 0.0, lod_max: 0.0, max_anisotropy: 1,
    }).unwrap_or_else(|failure| die(failure));
    let binding = Binding { group: 0, binding: 0, kind: BindingKind::StorageBuffer, visibility: vec![Stage::Compute] };
    let shader = make_shader("destroy-ownership", COMPUTE_SOURCE, &[("mapped_compute", Stage::Compute)], vec![binding.clone()]);
    let library = be::create_library(&device, &shader).unwrap_or_else(|failure| die(failure));
    let pipeline = be::create_compute_pipeline_from(&library, "mapped_compute", None, &[binding]).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, be::destroy_library(&library).err());
    expect(ErrorKind::InvalidState, be::destroy_device(&device).err());

    let abandoned = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let blit = be::blit_encoder(&abandoned).unwrap_or_else(|failure| die(failure));
    be::abandon(&abandoned).unwrap_or_else(|failure| die(failure));
    be::abandon(&abandoned).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, be::end_blit(&blit).err());

    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::compute_encoder(&commands).unwrap_or_else(|failure| die(failure));
    be::set_pipeline(&encoder, &pipeline).unwrap_or_else(|failure| die(failure));
    be::set_buffer(&encoder, 0, &mapped).unwrap_or_else(|failure| die(failure));
    be::dispatch_threads(&encoder, (2, 1, 1), (1, 1, 1)).unwrap_or_else(|failure| die(failure));
    be::end_compute(&encoder).unwrap_or_else(|failure| die(failure));
    // Submission cleanup, not these now-dead core wrappers, owns resources until completion.
    be::destroy_buffer(&mapped).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&mapped_clone).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&pipeline).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, be::destroy_library(&library).err());
    let receipt = be::commit(&commands).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, be::destroy_queue(&queue).err());
    be::complete_through(&queue, receipt.epoch).unwrap_or_else(|failure| die(failure));
    assert_eq!(readback(0, 8).unwrap_or_else(|failure| die(failure)), [3, 0, 0, 0, 3, 0, 0, 0]);
    drop(readback);
    be::destroy_library(&library).unwrap_or_else(|failure| die(failure));
    be::destroy_sampler(&sampler).unwrap_or_else(|failure| die(failure));
    be::destroy_texture(&texture).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&plain).unwrap_or_else(|failure| die(failure));
    be::destroy_queue(&queue).unwrap_or_else(|failure| die(failure));
    be::destroy_queue(&queue).unwrap_or_else(|failure| die(failure));
    assert_eq!(be::completed_epoch(&queue), receipt.epoch);
    assert_eq!(be::buffer_id(&mapped_clone), id);
    assert!(be::max_total_threads(&pipeline) > 0);
    expect(ErrorKind::StaleHandle, be::read_buffer(&mapped_clone, 0, 8).err());
    expect(ErrorKind::InvalidState, be::commit(&commands).err());
    assert_eq!(live(), device_only, "dead wrappers retained native driver payloads");
    be::destroy_device(&device).unwrap_or_else(|failure| die(failure));
    be::destroy_device(&device).unwrap_or_else(|failure| die(failure));
    assert_eq!(live(), before, "dead device or child wrapper retained the native device");
}

fn gpu_destroy_preserves_only_escaped_owned_readback() {
    let before = live();
    let device = be::create_device(backend::create()).unwrap_or_else(|failure| die(failure));
    let mapped = be::create_mapped_buffer(&device, buffer_descriptor(8, &[BufferUsage::CopyDst])).unwrap_or_else(|failure| die(failure));
    let clone = mapped.clone();
    be::write_buffer(&mapped, 0, &[2, 3, 4, 5, 6, 7, 8, 9]).unwrap_or_else(|failure| die(failure));
    let readback = be::owned_readback(&mapped).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&mapped).unwrap_or_else(|failure| die(failure));
    be::destroy_device(&device).unwrap_or_else(|failure| die(failure));
    assert_eq!(live(), before + 1, "escaped reader must retain only the buffer allocation");
    expect(ErrorKind::StaleHandle, be::owned_readback(&clone).err());
    assert!(be::mapped_view(&clone).is_none());
    let foreign = readback.clone();
    assert_eq!(std::thread::spawn(move || foreign(2, 4).unwrap()).join().unwrap(), [4, 5, 6, 7]);
    drop(readback);
    assert_eq!(be::buffer_size(&clone), 8);
    assert_eq!(live(), before, "dead mapped wrappers retained their base reader callbacks");
}

fn gpu_pipeline() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let other = Device::system_default().unwrap_or_else(|failure| die(failure));
    let before = live();
    let compute_binding = Binding { group: 0, binding: 0, kind: BindingKind::StorageBuffer, visibility: vec![Stage::Compute] };
    let compute_shader = make_shader("mapped-compute", COMPUTE_SOURCE, &[("mapped_compute", Stage::Compute)], vec![compute_binding.clone()]);
    let library_value = library::create(&device, &compute_shader).unwrap_or_else(|failure| die(failure));
    let compute = pipeline::create_compute_from_library(&device, &library_value, "mapped_compute", &[], &[compute_binding.clone()]).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidArgument, pipeline::create_compute_from_library(&device, &library_value, "mapped_compute", &[], &[Binding { kind: BindingKind::SampledTexture, ..compute_binding.clone() }]).err(),
    );
    assert!(pipeline::create_compute_from_library(&device, &library_value, "absent", &[], &[]).is_err(), "absent entry accepted");
    let invalid_shader = make_shader("invalid-msl", "this is not Metal", &[("bad", Stage::Compute)], vec![]);
    assert!(library::create(&device, &invalid_shader).is_err(), "invalid MSL compiled");
    let render_shader = make_shader("mapped-render", RENDER_SOURCE, &[("mapped_vertex", Stage::Vertex), ("mapped_fragment", Stage::Fragment)], vec![]);
    let descriptor = RenderDescriptor {
        backend: "metal".to_owned(),
        label: Some("mapped-render".to_owned()),
        layout: empty_layout(&device),
        vertex: render_shader.clone(),
        vertex_entry: "mapped_vertex".to_owned(),
        fragment: Some(render_shader),
        fragment_entry: Some("mapped_fragment".to_owned()),
        color_format: ColorFormat::Rgba8Unorm,
        depth_format: DepthFormat::NoDepth,
        sample_count: 1,
    };
    for blend in [Blend::Replace, Blend::Alpha, Blend::Add, Blend::Multiply, Blend::Screen, Blend::Subtract] {
        let value = pipeline::create_render_owned(false, ogpu_metal::native::pipeline::PrimitiveTopology::TriangleList, blend, &device, &descriptor).unwrap_or_else(|failure| die(failure));
        value.destroy().unwrap_or_else(|failure| die(failure));
    }
    compute.destroy().unwrap_or_else(|failure| die(failure));
    library_value.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
    other.destroy().unwrap_or_else(|failure| die(failure));
    drop((compute, library_value, descriptor, device, other));
    assert_eq!(live() + 2, before, "pipeline live-handle delta");
    println!("ogpu_metal pipeline: reflected compute from library, six owned render blends, typed rejections, zero live-handle delta");
}

const ARGUMENT_SOURCE: &str = "#include <metal_stdlib>\nusing namespace metal;\nstruct V { float4 position [[position]]; float2 uv; };\nvertex V scene_vertex(uint i [[vertex_id]]) { constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}}; V v; v.position=float4(p[i],0.,1.); v.uv=(p[i]+1.)*.5; return v; }\nstruct Args { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };\nfragment float4 scene_fragment_argument(V v [[stage_in]], constant Args &args [[buffer(1)]]) { return args.image.sample(args.sampling, v.uv); }\n";

fn gpu_pipeline_argument_buffer() {
    let device = Device::system_default().unwrap_or_else(|failure| die(failure));
    let before = live();
    let vertex = make_shader("argument", ARGUMENT_SOURCE, &[("scene_vertex", Stage::Vertex)], vec![]);
    let fragment = make_shader(
        "argument",
        ARGUMENT_SOURCE,
        &[("scene_fragment_argument", Stage::Fragment)],
        vec![Binding { group: 0, binding: 1, kind: BindingKind::StorageBuffer, visibility: vec![Stage::Fragment] }],
    );
    let group = binding::create_layout(&[LayoutEntry { binding: 1, kind: LayoutKind::Buffer, visibility: vec![LayoutStage::Fragment] }]).unwrap_or_else(|failure| die(failure));
    let layout = binding::create_pipeline_layout(device.handle(), &device.capabilities(), &[(0, group)]).unwrap_or_else(|failure| die(failure));
    let descriptor = RenderDescriptor {
        backend: "metal".to_owned(),
        label: Some("argument".to_owned()),
        layout,
        vertex,
        vertex_entry: "scene_vertex".to_owned(),
        fragment: Some(fragment),
        fragment_entry: Some("scene_fragment_argument".to_owned()),
        color_format: ColorFormat::Rgba8Unorm,
        depth_format: DepthFormat::NoDepth,
        sample_count: 1,
    };
    // Without `indirect` no fragment function is retained for encoders.
    let direct = pipeline::create_render_owned(false, ogpu_metal::native::pipeline::PrimitiveTopology::TriangleList, Blend::Replace, &device, &descriptor).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, direct.argument_encoder(1).err());
    let indirect = pipeline::create_render_owned(true, ogpu_metal::native::pipeline::PrimitiveTopology::TriangleList, Blend::Replace, &device, &descriptor).unwrap_or_else(|failure| die(failure));
    let mut encoder = indirect.argument_encoder(1).unwrap_or_else(|failure| die(failure));
    assert!(encoder.encoded_length() > 0, "argument encoder length");
    expect(ErrorKind::InvalidArgument, indirect.argument_encoder(u64::MAX).err());
    encoder.destroy().expect("encoder destroy");
    indirect.destroy().unwrap_or_else(|failure| die(failure));
    direct.destroy().unwrap_or_else(|failure| die(failure));
    device.destroy().unwrap_or_else(|failure| die(failure));
    drop((encoder, indirect, direct, descriptor, device));
    assert_eq!(live() + 1, before, "argument pipeline live-handle delta");
    println!("pipeline argument buffer: owned indirect pipeline retains its fragment encoder, zero delta");
}

fn gpu_acceleration() {
    let before = live();
    let device = be::create_device(backend::create()).unwrap_or_else(|failure| die(failure));
    let other = be::create_device(backend::create()).unwrap_or_else(|failure| die(failure));
    let vertices = be::create_buffer(&device, buffer_descriptor(36, &[BufferUsage::Storage, BufferUsage::Vertex]), None).unwrap_or_else(|failure| die(failure));
    let mut triangle = vec![0u8; 36];
    for (i, value) in [-1., -1., 0., 1., -1., 0., 0., 1., 0.].iter().enumerate() { put_f32(&mut triangle, i * 4, *value); }
    be::write_buffer(&vertices, 0, &triangle).unwrap_or_else(|failure| die(failure));
    let blas = |vertex_count| be::AccelDescriptor::Blas { geometries: vec![be::Geometry::Triangles { vertices: vertices.clone(), offset: 0, length: 36, vertex_stride: 12, vertex_count }] };
    if be::capabilities(&device).ray_tracing {
        let structure = be::create_accel(&device, blas(3), false).unwrap_or_else(|failure| die(failure));
        expect(ErrorKind::InvalidArgument, be::create_accel(&device, blas(6), false).err());
        let scratch = be::create_buffer(&device, buffer_descriptor(1_048_576, &[BufferUsage::Storage]), Some(Memory::DeviceLocal)).unwrap_or_else(|failure| die(failure));
        let foreign_scratch = be::create_buffer(&other, buffer_descriptor(1_048_576, &[BufferUsage::Storage]), Some(Memory::DeviceLocal)).unwrap_or_else(|failure| die(failure));
        let queue = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
        let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
        let encoder = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
        expect(ErrorKind::CrossDevice, be::build_accel(&encoder, &structure, &foreign_scratch, None).err());
        be::build_accel(&encoder, &structure, &scratch, None).unwrap_or_else(|failure| die(failure));
        let refittable = be::create_accel(&device, blas(3), true).unwrap_or_else(|failure| die(failure));
        be::build_accel(&encoder, &refittable, &scratch, None).unwrap_or_else(|failure| die(failure));
        expect(ErrorKind::InvalidArgument, be::refit_accel(&encoder, &refittable, blas(3), &scratch, Some(1_048_832)).err());
        be::refit_accel(&encoder, &refittable, blas(3), &scratch, None).unwrap_or_else(|failure| die(failure));
        let records = [be::InstanceRecord { instance: be::Instance { transform: vec![1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0.], mask: 0xFFFF_FFFF, structure_index: 0 }, user_id: 7 }];
        let packed = be::pack_instance_records(&device, &records).unwrap_or_else(|failure| die(failure));
        let instances = be::create_buffer(&device, buffer_descriptor(packed.len() as i64, &[BufferUsage::Storage]), None).unwrap_or_else(|failure| die(failure));
        be::write_buffer(&instances, 0, &packed).unwrap_or_else(|failure| die(failure));
        let tlas = |structure: &be::Accel| be::AccelDescriptor::TlasOf { instances: instances.clone(), offset: 0, instance_count: 1, kind: be::InstanceKind::UserIdInstances, structures: vec![structure.clone()], motion_transforms: None };
        let top = be::create_accel(&device, tlas(&structure), false).unwrap_or_else(|failure| die(failure));
        expect(ErrorKind::InvalidArgument, be::create_accel(&device, tlas(&top), false).err());
        be::build_accel(&encoder, &top, &scratch, None).unwrap_or_else(|failure| die(failure));
        let sized = be::create_accel(&device, be::AccelDescriptor::Sized { size: be::accel_sizes(&structure).structure_size, template: structure.clone() }, false).unwrap_or_else(|failure| die(failure));
        expect(ErrorKind::InvalidState, be::build_accel(&encoder, &sized, &scratch, None).err());
        be::compact_accel(&encoder, &structure, &sized).unwrap_or_else(|failure| die(failure));
        be::end_accel(&encoder).unwrap_or_else(|failure| die(failure));
        // Submission callbacks own the built structure graph, including its buffers and children,
        // after logical wrappers are destroyed and before GPU completion.
        be::destroy_accel(&top).unwrap_or_else(|failure| die(failure));
        be::destroy_accel(&sized).unwrap_or_else(|failure| die(failure));
        let receipt = be::commit(&commands).unwrap_or_else(|failure| die(failure));
        be::complete_through(&queue, receipt.epoch).unwrap_or_else(|failure| die(failure));
        be::destroy_accel(&structure).unwrap_or_else(|failure| die(failure));
        be::destroy_accel(&refittable).unwrap_or_else(|failure| die(failure));
        be::destroy_buffer(&instances).unwrap_or_else(|failure| die(failure));
        be::destroy_buffer(&scratch).unwrap_or_else(|failure| die(failure));
        be::destroy_buffer(&foreign_scratch).unwrap_or_else(|failure| die(failure));
        be::destroy_queue(&queue).unwrap_or_else(|failure| die(failure));
    } else {
        expect(ErrorKind::Unsupported, be::create_accel(&device, blas(3), false).err());
    }
    be::destroy_buffer(&vertices).unwrap_or_else(|failure| die(failure));
    be::destroy_device(&device).unwrap_or_else(|failure| die(failure));
    be::destroy_device(&other).unwrap_or_else(|failure| die(failure));
    assert_eq!(live(), before, "acceleration live-handle delta");
    println!("ogpu_metal acceleration: generic primitives and user-id instances, in-place refit, typed core rejections, retained graph until completion, zero delta");
}

const SAMPLER_SOURCE: &str = "#include <metal_stdlib>
using namespace metal;
struct V { float4 position [[position]]; };
vertex V sample_vertex(uint i [[vertex_id]]) {
  constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}};
  V v; v.position=float4(p[i],0.,1.); return v;
}
fragment float4 sample_clamp(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(-0.25,0.25)); }
fragment float4 sample_repeat(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(-0.25,0.25)); }
fragment float4 sample_mirror(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(1.75,0.25)); }
fragment float4 sample_linear(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(0.5,0.5)); }
fragment float4 sample_mip(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(0.5),level(1.0)); }
";

/// Sampler semantics through the portable render encoder: nearest and linear filtering,
/// clamp/repeat/mirror addressing, and an explicit mip level.
fn gpu_sampler_pixels() {
    let driver = backend::create();
    let before = impl_::live_handles();
    let device = be::create_device(driver).unwrap_or_else(|failure| die(failure));
    {
        let sampler_binding = |binding: i64, kind: BindingKind| Binding { group: 0, binding, kind, visibility: vec![Stage::Fragment] };
        let vertex = make_shader("sampler-pixels", SAMPLER_SOURCE, &[("sample_vertex", Stage::Vertex)], vec![]);
        let fragment = |entry: &str| {
            make_shader(
                "sampler-pixels",
                SAMPLER_SOURCE,
                &[(entry, Stage::Fragment)],
                vec![sampler_binding(0, BindingKind::SampledTexture), sampler_binding(1, BindingKind::Sampler)],
            )
        };
        let texture_layout = binding::create_layout(&[
            LayoutEntry { binding: 0, kind: LayoutKind::Texture, visibility: vec![LayoutStage::Fragment] },
            LayoutEntry { binding: 1, kind: LayoutKind::Sampler, visibility: vec![LayoutStage::Fragment] },
        ]).unwrap_or_else(|failure| die(failure));
        let layout = binding::create_pipeline_layout(be::device_handle(&device), &be::capabilities(&device), &[(0, texture_layout)]).unwrap_or_else(|failure| die(failure));
        let pipelines: Vec<(&str, be::Pipeline)> = ["sample_clamp", "sample_repeat", "sample_mirror", "sample_linear", "sample_mip"]
            .into_iter()
            .map(|entry| {
                let descriptor = RenderDescriptor {
                    backend: "metal".to_owned(),
                    label: Some(entry.to_owned()),
                    layout: layout.clone(),
                    vertex: vertex.clone(),
                    vertex_entry: "sample_vertex".to_owned(),
                    fragment: Some(fragment(entry)),
                    fragment_entry: Some(entry.to_owned()),
                    color_format: ColorFormat::Rgba8Unorm,
                    depth_format: DepthFormat::NoDepth,
                    sample_count: 1,
                };
                (entry, be::create_render_pipeline(&device, &descriptor, None, None, None).unwrap_or_else(|failure| die(failure)))
            })
            .collect();
        let texture_of = |label: &str, width: i64, mip_levels: i64, usage: &[TextureUsage]| {
            be::create_texture(
                &device,
                TextureDescriptor { label: Some(label.to_owned()), width, height: width, depth: 1, mip_levels, sample_count: 1, format: TextureFormat::Rgba8Unorm, usage: usage.to_vec() },
            ).unwrap_or_else(|failure| die(failure))
        };
        let sampled = texture_of("sampled", 2, 2, &[TextureUsage::TextureBinding, TextureUsage::TextureCopyDst]);
        let target = texture_of("target", 1, 1, &[TextureUsage::TextureBinding, TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc]);
        let upload = be::create_buffer(&device, BufferDescriptor { label: None, size: 768, usage: vec![BufferUsage::CopySrc, BufferUsage::CopyDst] }, None).unwrap_or_else(|failure| die(failure));
        let queue_value = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
        let mut bytes = vec![0u8; 768];
        bytes[..8].copy_from_slice(&[0xff, 0x00, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff]);
        bytes[256..264].copy_from_slice(&[0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);
        bytes[512..516].copy_from_slice(&[0xff, 0xff, 0x00, 0xff]);
        be::write_buffer(&upload, 0, &bytes).unwrap_or_else(|failure| die(failure));
        let wait = |receipt: be::Receipt| be::complete_through(&queue_value, receipt.epoch).unwrap_or_else(|failure| die(failure));
        let commands = be::begin_commands(&queue_value).unwrap_or_else(|failure| die(failure));
        let blit = be::blit_encoder(&commands).unwrap_or_else(|failure| die(failure));
        be::buffer_to_texture(&blit, &upload, None, 256, 512, &sampled, None, None, Extent { width: 2, height: 2, depth: 1 }).unwrap_or_else(|failure| die(failure));
        be::buffer_to_texture(&blit, &upload, Some(512), 256, 256, &sampled, Some(1), None, Extent { width: 1, height: 1, depth: 1 }).unwrap_or_else(|failure| die(failure));
        be::end_blit(&blit).unwrap_or_else(|failure| die(failure));
        wait(be::commit(&commands).unwrap_or_else(|failure| die(failure)));
        let base = SamplerDescriptor {
            label: None,
            min_filter: SamplerFilter::Nearest,
            mag_filter: SamplerFilter::Nearest,
            mip_filter: MipFilter::NoMip,
            address_u: AddressMode::ClampToEdge,
            address_v: AddressMode::ClampToEdge,
            lod_min: 0.,
            lod_max: 2.,
            max_anisotropy: 1,
        };
        let cases = [
            ("sample_clamp", [255u8, 0, 0, 255], base.clone()),
            ("sample_repeat", [0, 255, 0, 255], SamplerDescriptor { address_u: AddressMode::Repeat, ..base.clone() }),
            ("sample_mirror", [255, 0, 0, 255], SamplerDescriptor { address_u: AddressMode::MirrorRepeat, ..base.clone() }),
            ("sample_linear", [128, 128, 128, 255], SamplerDescriptor { min_filter: SamplerFilter::Linear, mag_filter: SamplerFilter::Linear, ..base.clone() }),
            ("sample_mip", [255, 255, 0, 255], SamplerDescriptor { mip_filter: MipFilter::NearestMip, ..base.clone() }),
        ];
        for (entry, expected, descriptor) in cases {
            let pipeline_value = &pipelines.iter().find(|(name, _)| *name == entry).expect("pipeline").1;
            let sampler = be::create_sampler(&device, descriptor).unwrap_or_else(|failure| die(failure));
            let commands = be::begin_commands(&queue_value).unwrap_or_else(|failure| die(failure));
            let encoder = be::render_encoder(
                &commands,
                &be::RenderTarget {
                    colors: vec![be::ColorAttachment { texture: target.clone(), resolve: None, load: Load::Clear, store: Store::Store, clear: (0., 0., 0., 0.) }],
                    depth: None,
                },
            ).unwrap_or_else(|failure| die(failure));
            be::set_render_pipeline(&encoder, pipeline_value).unwrap_or_else(|failure| die(failure));
            be::set_stage_texture(&encoder, be::ShaderStage::Fragment, 0, &sampled).unwrap_or_else(|failure| die(failure));
            be::set_stage_sampler(&encoder, be::ShaderStage::Fragment, 1, &sampler).unwrap_or_else(|failure| die(failure));
            be::draw(&encoder, Primitive::TriangleList, 0, 3, None).unwrap_or_else(|failure| die(failure));
            be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
            wait(be::commit(&commands).unwrap_or_else(|failure| die(failure)));
            let actual = be::read_texture(&target, 4).unwrap_or_else(|failure| die(failure));
            assert_eq!(actual.len(), expected.len(), "{entry}: sampler pixel byte count");
            assert!(
                expected.iter().zip(actual.iter()).all(|(a, b)| (*a as i32 - *b as i32).abs() <= 1),
                "{entry} sampled {actual:?}, expected {expected:?}"
            );
            be::destroy_sampler(&sampler).unwrap_or_else(|failure| die(failure));
        }
        be::destroy_queue(&queue_value).unwrap_or_else(|failure| die(failure));
        be::destroy_buffer(&upload).unwrap_or_else(|failure| die(failure));
        be::destroy_texture(&sampled).unwrap_or_else(|failure| die(failure));
        be::destroy_texture(&target).unwrap_or_else(|failure| die(failure));
        for (_, value) in &pipelines {
            be::destroy_pipeline(value).unwrap_or_else(|failure| die(failure));
        }
    }
    be::destroy_device(&device).unwrap_or_else(|failure| die(failure));
    drop(device);
    assert_eq!(impl_::live_handles(), before, "sampler pixels live-handle delta");
    println!("ogpu_metal sampler pixels: nearest/linear clamp/repeat/mirror/mip exact, zero delta");
}

pub const CASES: &[(&str, fn())] = &[
        ("gpu_device_error", gpu_device_error),
        ("gpu_foundation", gpu_foundation),
        ("gpu_texture", gpu_texture),
        ("gpu_capabilities", gpu_capabilities),
        ("gpu_pipeline", gpu_pipeline),
        ("gpu_pipeline_argument_buffer", gpu_pipeline_argument_buffer),
        ("gpu_acceleration", gpu_acceleration),
        ("gpu_sampler_pixels", gpu_sampler_pixels),
        ("gpu_destroy_releases_retained_driver_payloads", gpu_destroy_releases_retained_driver_payloads),
        ("gpu_destroy_preserves_only_escaped_owned_readback", gpu_destroy_preserves_only_escaped_owned_readback),
    ];
