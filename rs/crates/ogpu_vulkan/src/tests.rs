//! The Vulkan parts of lib/ogpu/conformance (test_vulkan.ml and conformance.ml), run against a real
//! device. The OCaml suite is one function over one device; here it is three stages and the
//! out-of-memory check, each with its own device, so each stays inside the per-test budget. Every
//! test skips when no Vulkan device exists, and ends with zero live handles and zero validation
//! messages. The SPIR-V comes from lib/ogpu/conformance/vulkan/*.{comp,vert,frag}, built by build.rs.
use crate::{backend::create_with_validation, impl_};
use ogpu_core::{
    acceleration::InstanceKind,
    backend::{
        self as be, Accel, AccelDescriptor, BatchDraw, Border, Buffer, ColorAttachment, CurveBasis,
        CurveCaps, CurveType, DepthAttachment, DepthState, Device, Geometry, Instance,
        InstanceRecord, Keyframe, Motion, MotionInstance, Pipeline, Queue, Receipt, RenderTarget,
        ResourceUse, ShaderStage, Status, Texture, DEFAULT_GEOMETRY_OPTIONS,
    },
    binding::{self, Kind as LayoutKind, LayoutEntry, Stage as LayoutStage},
    caps::{self, Feature},
    error::{self, Error, ErrorKind},
    pipeline::{ColorFormat, DepthFormat, RenderDescriptor},
    render_pass::{Comparison, Cull, IndexType, Load, Primitive, Rect, Store},
    shader::{self, Binding, BindingKind, EntryPoint, Shader, Stage},
    types::{
        self, AddressMode, BufferDescriptor, BufferUsage, Extent, Memory, MipFilter, Origin,
        SamplerDescriptor, SamplerFilter, TextureDescriptor, TextureFormat, TextureUsage,
    },
};
use std::{path::Path, rc::Rc};

type Hit = (i64, i64, i64, f64, f64, f64);

fn get<T>(result: Result<T, Error>) -> T {
    match result {
        Ok(value) => value,
        Err(failure) => panic!("{}", error::to_string(&failure)),
    }
}

fn require(condition: bool, message: &str) {
    assert!(condition, "{message}");
}

/// The result must be an error of this kind.
fn refused<T>(result: Result<T, Error>, kind: ErrorKind, message: &str) {
    match result {
        Err(failure) if failure.kind == kind => (),
        Err(failure) => panic!("{message}: got {}", error::to_string(&failure)),
        Ok(_) => panic!("{message}"),
    }
}

fn relative_close(a: f64, b: f64) -> bool {
    a.is_finite() && b.is_finite() && (a == b || (a - b).abs() <= 1e-5 * a.abs().max(b.abs()))
}

fn check_hits(label: &str, hits: &[Hit], expected: &[Hit]) {
    require(hits.len() == expected.len(), &format!("{label}: ray count differs"));
    for (i, &(hit, primitive, instance, t, u, v)) in expected.iter().enumerate() {
        let (actual_hit, actual_primitive, actual_instance, actual_t, actual_u, actual_v) = hits[i];
        require(
            actual_hit == hit && actual_primitive == primitive && actual_instance == instance,
            &format!("{label} ray {i} ids differ: {:?} against {:?}", hits[i], expected[i]),
        );
        require(
            if hit == 1 {
                relative_close(actual_t, t) && relative_close(actual_u, u) && relative_close(actual_v, v)
            } else {
                actual_t == 0. && actual_u == 0. && actual_v == 0.
            },
            &format!("{label} ray {i} distance/barycentrics differ: {:?} against {:?}", hits[i], expected[i]),
        );
    }
}

const NO_HIT: Hit = (0, 0, 0, 0., 0., 0.);

fn triangle_hit(primitive: i64, instance: i64, t: f64) -> Hit {
    (1, primitive, instance, t, 0.25, 0.5)
}

fn generated_hit(primitive: i64, instance: i64, t: f64) -> Hit {
    (1, primitive, instance, t, 0., 0.)
}

fn shader_bytes(name: &str) -> Vec<u8> {
    let dir = env!("OGPU_VK_SPV_DIR");
    assert!(!dir.is_empty(), "the conformance SPIR-V was not built (glslangValidator and spirv-link are required)");
    std::fs::read(Path::new(dir).join(format!("{name}.spv"))).unwrap_or_else(|failure| panic!("{name}.spv: {failure}"))
}

fn put_f32(bytes: &mut [u8], offset: usize, value: f64) {
    bytes[offset..offset + 4].copy_from_slice(&(value as f32).to_le_bytes());
}

fn put_i32(bytes: &mut [u8], offset: usize, value: i32) {
    bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn i32_at(bytes: &[u8], offset: usize) -> i32 {
    i32::from_le_bytes([bytes[offset], bytes[offset + 1], bytes[offset + 2], bytes[offset + 3]])
}

fn f32_at(bytes: &[u8], offset: usize) -> f64 {
    f32::from_bits(i32_at(bytes, offset) as u32) as f64
}

fn buf(label: &str, size: i64, usage: &[BufferUsage]) -> BufferDescriptor {
    BufferDescriptor { label: Some(label.to_owned()), size, usage: usage.to_vec() }
}

fn make_buffer(device: &Device, label: &str, size: i64, usage: &[BufferUsage]) -> Buffer {
    get(be::create_buffer(device, buf(label, size, usage), None))
}

fn local_buffer(device: &Device, label: &str, size: i64, usage: &[BufferUsage]) -> Buffer {
    get(be::create_buffer(device, buf(label, size, usage), Some(Memory::DeviceLocal)))
}

fn float_buffer(device: &Device, label: &str, values: &[f64]) -> Buffer {
    let mut bytes = vec![0u8; 4 * values.len()];
    for (i, v) in values.iter().enumerate() {
        put_f32(&mut bytes, i * 4, *v);
    }
    let buffer = make_buffer(device, label, bytes.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&buffer, 0, &bytes));
    buffer
}

fn texture_descriptor(label: &str, width: i64, usage: &[TextureUsage]) -> TextureDescriptor {
    TextureDescriptor {
        label: Some(label.to_owned()),
        width,
        height: width,
        depth: 1,
        mip_levels: 1,
        sample_count: 1,
        format: TextureFormat::Rgba8Unorm,
        usage: usage.to_vec(),
    }
}

fn compute_binding(binding: i64, kind: BindingKind) -> Binding {
    Binding { group: 0, binding, kind, visibility: vec![Stage::Compute] }
}

fn make_shader(label: &str, bytes: Vec<u8>, entries: &[(&str, Stage)], bindings: Vec<Binding>) -> Shader {
    get(shader::create(shader::Descriptor {
        backend: "vulkan".to_owned(),
        label: Some(label.to_owned()),
        bytes,
        entry_points: entries.iter().map(|(name, stage)| EntryPoint { name: (*name).to_owned(), stage: *stage }).collect(),
        bindings,
    }))
}

/// Waits for a submitted epoch by polling it, bounded and without a sleep.
fn poll_epoch(queue: &Queue, receipt: Receipt) {
    for _ in 0..2_000_000 {
        if get(be::status(queue, receipt)) == Status::Completed {
            return;
        }
        std::thread::yield_now();
    }
    panic!("submitted epoch did not complete");
}

fn render_target(texture: &Texture, load: Load, store: Store, clear: (f64, f64, f64, f64)) -> RenderTarget {
    RenderTarget {
        colors: vec![ColorAttachment { texture: texture.clone(), resolve: None, load, store, clear }],
        depth: None,
    }
}

/// A device on a fresh driver with fatal validation, or None where there is no Vulkan device.
fn open() -> Option<Device> {
    let driver = create_with_validation((true, true));
    match (driver.create_device)() {
        Err(failure) if failure.kind == ErrorKind::NoAdapter => {
            eprintln!("OGPU conformance (Vulkan): skipped (no device)");
            return None;
        }
        Err(failure) => panic!("{}", error::to_string(&failure)),
        Ok(raw) => get((raw.destroy_device)()),
    }
    Some(get(be::create_device(driver)))
}

fn finish(device: &Device) {
    get(be::destroy_device(device));
    assert_eq!(impl_::live_handles(), 0, "Vulkan backend leaked handles");
    assert_eq!(vulkan::Device::validation_messages(), 0, "validation messages");
}

fn encode_rows(format: TextureFormat, width: usize, texel: &dyn Fn(usize, usize, usize) -> f64) -> Vec<u8> {
    let mut rows = vec![0u8; 1024];
    let size = (types::texel_bytes(format) / 4) as usize;
    for y in 0..width {
        for x in 0..width {
            for c in 0..4 {
                let offset = y * 256 + (x * 4 + c) * size;
                let v = texel(x, y, c);
                match format {
                    TextureFormat::Rgba8Unorm => rows[offset] = (v as i64 & 255) as u8,
                    TextureFormat::Rgba16Float => {
                        rows[offset..offset + 2].copy_from_slice(&(types::half_of_float(v) as u16).to_le_bytes())
                    }
                    TextureFormat::Rgba32Float => put_f32(&mut rows, offset, v),
                    _ => panic!("conformance image writer requires an RGBA color format"),
                }
            }
        }
    }
    rows
}

#[test]
fn gpu_implementation_tag_and_handles() {
    ogpu::impl_::select(impl_::implementation());
    assert_eq!(ogpu::impl_::backend().unwrap(), "vulkan");
    assert_eq!(ogpu::impl_::live_handles().unwrap(), 0);
}

/// Capabilities, buffers, mapped buffers, texture upload and read, pixel formats, LOD sampling, a
/// reusable library with function-constant-free pipelines, indirect dispatch, abandoned commands and
/// independent queues.
#[test]
fn gpu_conformance_transfer_and_compute() {
    let Some(device) = open() else { return };
    let capabilities = be::capabilities(&device);
    get(caps::validate(&capabilities));
    let profile = capabilities;
    require(caps::has(&profile, &Feature::RayTracing) == capabilities.ray_tracing, "ray-tracing capability mismatch");
    refused(
        caps::require(None, &profile, &Feature::Unknown("future".to_owned())),
        ErrorKind::Unsupported,
        "unknown feature was not typed Unsupported",
    );
    let source = make_buffer(&device, "conformance", 16, &[BufferUsage::CopySrc, BufferUsage::CopyDst]);
    let queue = get(be::create_queue(&device));
    require(be::completed_epoch(&queue) == 0, "new queue completed an epoch");
    refused(be::poll_through(&queue, 0), ErrorKind::InvalidArgument, "zero completion epoch was accepted");
    let bytes: Vec<u8> = (0..16).map(|i| ((i * 13) & 255) as u8).collect();
    get(be::write_buffer(&source, 0, &bytes));
    require(get(be::read_buffer(&source, 0, 16)) == bytes, "buffer round trip differs");

    // a mapped buffer: host view and device copy meet only at flush and invalidate; a view outlives
    // the buffer
    let mapped = get(be::create_mapped_buffer(
        &device,
        buf("mapped", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]),
    ));
    let floats = be::mapped_view(&mapped).unwrap();
    require(floats.borrow().len() == 4 && floats.borrow()[3] == 0., "mapped view is not 4 zeroed floats");
    require(Rc::ptr_eq(&be::mapped_view(&mapped).unwrap(), &floats), "mapped view accessor copied storage");
    require(be::mapped_view(&source).is_none(), "unmapped buffer exposed a mapped view");
    floats.borrow_mut()[1] = 1.5;
    get(be::flush_mapped(&mapped, 16));
    require(i32_at(&get(be::read_buffer(&mapped, 4, 4)), 0) == 1.5f32.to_bits() as i32, "a flushed view write is not in the buffer");
    let mut three = [0u8; 4];
    put_f32(&mut three, 0, 3.);
    get(be::write_buffer(&mapped, 8, &three));
    get(be::invalidate_mapped(&mapped, 16));
    require(floats.borrow()[2] == 3. && floats.borrow()[1] == 1.5, "an invalidated view lacks the buffer's contents");
    // released, the host copy of a discrete device goes: a flush has nothing to write, and the next
    // view is new and holds the device copy once invalidated; unified memory keeps its one view
    get(be::release_mapped_view(&mapped));
    if capabilities.unified_memory {
        require(Rc::ptr_eq(&be::mapped_view(&mapped).unwrap(), &floats), "a unified view changed on release");
    } else {
        refused(be::flush_mapped(&mapped, 16), ErrorKind::InvalidArgument, "a released view was flushed");
        get(be::invalidate_mapped(&mapped, 16));
        let fresh = be::mapped_view(&mapped).unwrap();
        require(
            !Rc::ptr_eq(&fresh, &floats) && fresh.borrow()[2] == 3. && fresh.borrow()[1] == 1.5,
            "a view made by invalidate lacks the buffer's contents",
        );
    }
    let floats = be::mapped_view(&mapped).unwrap();
    refused(be::release_mapped_view(&source), ErrorKind::InvalidArgument, "an unmapped buffer's view was released");
    // a buffer only the device touches: no view until the host asks, then the device copy once invalidated
    let device_only = get(be::create_mapped_buffer(
        &device,
        buf("device only", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]),
    ));
    get(be::write_buffer(&device_only, 8, &three));
    if !capabilities.unified_memory {
        refused(be::flush_mapped(&device_only, 16), ErrorKind::InvalidArgument, "a view nobody asked for was flushed");
    }
    get(be::invalidate_mapped(&device_only, 16));
    require(be::mapped_view(&device_only).unwrap().borrow()[2] == 3., "a device-only buffer's first view");
    get(be::destroy_buffer(&device_only));
    refused(be::flush_mapped(&mapped, 20), ErrorKind::InvalidArgument, "a flush past the buffer was accepted");
    refused(be::flush_mapped(&source, 4), ErrorKind::InvalidArgument, "an unmapped buffer was flushed");
    get(be::destroy_buffer(&mapped));
    require(be::mapped_view(&mapped).is_none(), "destroyed buffer exposed a mapped view");
    require(floats.borrow()[1] == 1.5, "mapped view changed after destroy");
    refused(be::invalidate_mapped(&mapped, 4), ErrorKind::StaleHandle, "a destroyed mapped buffer was invalidated");
    refused(
        be::create_mapped_buffer(&device, buf("", 6, &[BufferUsage::Storage])),
        ErrorKind::InvalidArgument,
        "a mapped buffer of 6 bytes was accepted",
    );

    // Blit encoder: texture upload with padded rows, read back by the host, and the same at an inset origin.
    let blit = |encode: &dyn Fn(&be::BlitEncoder)| -> Receipt {
        let commands = get(be::begin_commands(&queue));
        let encoder = get(be::blit_encoder(&commands));
        encode(&encoder);
        get(be::end_blit(&encoder));
        let receipt = get(be::commit(&commands));
        poll_epoch(&queue, receipt);
        receipt
    };
    let upload = make_buffer(&device, "texture-transfer", 512, &[BufferUsage::CopySrc, BufferUsage::CopyDst]);
    let round_trip = texture_descriptor("texture-round-trip", 2, &[TextureUsage::TextureCopySrc, TextureUsage::TextureCopyDst]);
    let first = get(be::create_texture(&device, round_trip.clone()));
    let mut padded = vec![0u8; 512];
    for i in 0..8usize {
        padded[i] = (i * 17 + 3) as u8;
        padded[256 + i] = (i * 19 + 5) as u8;
    }
    get(be::write_buffer(&upload, 0, &padded));
    let extent = Extent { width: 2, height: 2, depth: 1 };
    let receipt = blit(&|encoder| get(be::buffer_to_texture(encoder, &upload, None, 256, 512, &first, None, None, extent)));
    require(be::completed_epoch(&queue) == receipt.epoch, "queue completed epoch differs from receipt");
    require(get(be::poll_through(&queue, receipt.epoch)), "completed epoch did not remain complete");
    let mut compact = vec![0u8; 16];
    compact[..8].copy_from_slice(&padded[..8]);
    compact[8..].copy_from_slice(&padded[256..264]);
    require(get(be::read_texture(&first, 8)) == compact, "uploaded texture differs from its padded rows");
    let inset = get(be::create_texture(&device, TextureDescriptor { width: 4, height: 4, ..round_trip }));
    blit(&|encoder| {
        get(be::buffer_to_texture(encoder, &upload, None, 256, 512, &inset, None, Some(Origin { x: 1, y: 1, z: 0 }), extent))
    });
    let mut expected_image = vec![0u8; 64];
    expected_image[20..28].copy_from_slice(&padded[..8]);
    expected_image[36..44].copy_from_slice(&padded[256..264]);
    require(get(be::read_texture(&inset, 16)) == expected_image, "texture subregion upload changed other pixels");

    // Pixel formats: a 4x4 three-mip texture per format, known texels per level (negative and above one
    // for floats) uploaded with 256-byte rows; level 0 is read back exactly through the host read path.
    let format_rows = make_buffer(&device, "format-rows", 1024, &[BufferUsage::CopySrc, BufferUsage::CopyDst]);
    let format_texture = |width: i64, mip_levels: i64, usage: &[TextureUsage], format: TextureFormat| {
        be::create_texture(
            &device,
            TextureDescriptor {
                label: Some("format".to_owned()),
                width,
                height: width,
                depth: 1,
                mip_levels,
                sample_count: 1,
                format,
                usage: usage.to_vec(),
            },
        )
    };
    let sampled = [TextureUsage::TextureBinding, TextureUsage::TextureCopySrc, TextureUsage::TextureCopyDst];
    for format in [TextureFormat::Rgba8Unorm, TextureFormat::Rgba16Float, TextureFormat::Rgba32Float] {
        let texture = get(format_texture(4, 3, &sampled, format));
        for mip in 0..3usize {
            let width = 4usize >> mip;
            let rows = encode_rows(format, width, &|x, y, c| {
                if format == TextureFormat::Rgba8Unorm {
                    ((x * 7) + (y * 31) + (c * 13) + (mip * 50)) as f64
                } else {
                    (x as f64 - (2 * y) as f64 + (3 * c) as f64) * 1.5 + mip as f64 * 100. - 7.
                }
            });
            get(be::write_buffer(&format_rows, 0, &rows));
            let extent = Extent { width: width as i64, height: width as i64, depth: 1 };
            blit(&|encoder| {
                get(be::buffer_to_texture(encoder, &format_rows, None, 256, 1024, &texture, Some(mip as i64), None, extent))
            });
            if mip == 0 {
                let row = 4 * types::texel_bytes(format) as usize;
                let mut compact = vec![0u8; 4 * row];
                for y in 0..4 {
                    compact[y * row..(y + 1) * row].copy_from_slice(&rows[y * 256..y * 256 + row]);
                }
                require(get(be::read_texture(&texture, row as i64)) == compact, "format host read differs");
            }
        }
        get(be::destroy_texture(&texture));
    }
    refused(
        format_texture(4, 1, &[TextureUsage::TextureBinding, TextureUsage::RenderAttachment], TextureFormat::Rgba16Float),
        ErrorKind::Unsupported,
        "float render attachment was accepted",
    );
    let half = get(format_texture(4, 3, &sampled, TextureFormat::Rgba16Float));
    let wide = get(format_texture(32, 1, &sampled, TextureFormat::Rgba32Float));
    let commands = get(be::begin_commands(&queue));
    let encoder = get(be::blit_encoder(&commands));
    refused(
        be::buffer_to_texture(&encoder, &format_rows, None, 256, 256, &wide, None, None, Extent { width: 32, height: 1, depth: 1 }),
        ErrorKind::InvalidArgument,
        "row pitch below the float texel width was accepted",
    );
    get(be::end_blit(&encoder));
    get(be::abandon(&commands));

    // Linear mip sampling of Rgba16_float at LOD 0, 0.5 and 1 from uniform levels: the exact levels and their average.
    let levels: [[f64; 4]; 3] = [[2., -1., 0.5, 4.], [4., -3., 1.5, 8.], [100., 100., 100., 100.]];
    for mip in 0..3usize {
        let width = 4usize >> mip;
        get(be::write_buffer(&format_rows, 0, &encode_rows(TextureFormat::Rgba16Float, width, &|_, _, c| levels[mip][c])));
        let extent = Extent { width: width as i64, height: width as i64, depth: 1 };
        blit(&|encoder| {
            get(be::buffer_to_texture(encoder, &format_rows, None, 256, 1024, &half, Some(mip as i64), None, extent))
        });
    }
    let interface = vec![compute_binding(0, BindingKind::SampledTexture), compute_binding(1, BindingKind::StorageBuffer)];
    let lod_shader = make_shader("sample-lod", shader_bytes("sample_lod"), &[("sample_lod", Stage::Compute)], interface.clone());
    let library = get(be::create_library(&device, &lod_shader));
    let pipeline = get(be::create_compute_pipeline_from(&library, "sample_lod", None, &interface));
    let out = make_buffer(&device, "sample-lod", 48, &[BufferUsage::Storage, BufferUsage::CopySrc]);
    let commands = get(be::begin_commands(&queue));
    let encoder = get(be::compute_encoder(&commands));
    get(be::set_pipeline(&encoder, &pipeline));
    get(be::set_texture(&encoder, 0, &half));
    get(be::set_buffer(&encoder, 1, &out));
    get(be::dispatch_threads(&encoder, (3, 1, 1), (3, 1, 1)));
    get(be::end_compute(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    let values = get(be::read_buffer(&out, 0, 48));
    let average: Vec<f64> = (0..4).map(|c| (levels[0][c] + levels[1][c]) * 0.5).collect();
    for (i, expected) in [levels[0].to_vec(), average, levels[1].to_vec()].iter().enumerate() {
        for (c, expected) in expected.iter().enumerate() {
            let actual = f32_at(&values, (i * 4 + c) * 4);
            require(
                (actual - expected).abs() <= 1e-3,
                &format!("Rgba16_float sample at LOD {} channel {c}: {actual}, expected {expected}", i as f64 * 0.5),
            );
        }
    }
    get(be::destroy_buffer(&out));
    get(be::destroy_pipeline(&pipeline));
    get(be::destroy_library(&library));
    for texture in [&half, &wide] {
        get(be::destroy_texture(texture));
    }
    get(be::destroy_buffer(&format_rows));

    // Reusable libraries and immediate-mode encoders: one library, two pipelines (SPIR-V has no named
    // function constants: the two variants are two entries), exact output, and status polling.
    let scale = |triple: bool| (if triple { "scale3" } else { "scale2" }).to_owned();
    let library_shader = make_shader(
        "conformance-library",
        shader_bytes("library"),
        &[("fill", Stage::Compute), ("scale2", Stage::Compute), ("scale3", Stage::Compute)],
        vec![],
    );
    let library = get(be::create_library(&device, &library_shader));
    let scale_interface = vec![compute_binding(0, BindingKind::StorageBuffer), compute_binding(1, BindingKind::UniformBuffer)];
    let words = make_buffer(&device, "encoded-words", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]);
    let local = local_buffer(&device, "device-local", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]);
    refused(be::read_buffer(&local, 0, 16), ErrorKind::Unsupported, "device-local buffer was host readable");
    refused(be::write_buffer(&local, 0, &[0u8; 16]), ErrorKind::Unsupported, "device-local buffer was host writable");
    let mut bias = [0u8; 4];
    put_i32(&mut bias, 0, 5);
    let triple = get(be::create_compute_pipeline_from(&library, &scale(true), None, &scale_interface));
    let double = get(be::create_compute_pipeline_from(&library, &scale(false), None, &scale_interface));
    assert!(be::max_total_threads(&double) >= 32);
    refused(
        be::create_compute_pipeline_from(&library, &scale(true), None, &scale_interface[..1]),
        ErrorKind::InvalidArgument,
        "mismatched library interface was accepted",
    );
    require(be::create_compute_pipeline_from(&library, "missing", None, &[]).is_err(), "missing library entry was accepted");
    refused(be::destroy_library(&library), ErrorKind::InvalidState, "library with live pipelines was destroyed");
    let run = |pipeline: &Pipeline, factor: i32| {
        let mut input = [0u8; 16];
        for i in 0..4 {
            put_i32(&mut input, i * 4, i as i32);
        }
        get(be::write_buffer(&words, 0, &input));
        let commands = get(be::begin_commands(&queue));
        let compute = get(be::compute_encoder(&commands));
        refused(
            be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)),
            ErrorKind::InvalidState,
            "dispatch without a pipeline was accepted",
        );
        get(be::set_pipeline(&compute, pipeline));
        get(be::set_buffer(&compute, 0, &words));
        get(be::set_bytes(&compute, 1, &bias));
        refused(be::blit_encoder(&commands), ErrorKind::InvalidState, "second encoder opened while one was recording");
        get(be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)));
        get(be::end_compute(&compute));
        let receipt = get(be::commit(&commands));
        refused(be::commit(&commands), ErrorKind::InvalidState, "commands were committed twice");
        poll_epoch(&queue, receipt);
        let output = get(be::read_buffer(&words, 0, 16));
        for i in 0..4 {
            require(i32_at(&output, i * 4) == i as i32 * factor + 5, "encoded compute output differs");
        }
    };
    run(&triple, 3);
    run(&double, 2);
    run(&triple, 3);
    // an indirect dispatch: one group of two, its count read from a buffer, doubles only the first two
    // words; a range past the buffer or misaligned is refused
    let groups = make_buffer(&device, "indirect-groups", 16, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    let mut args = [0u8; 16];
    for (k, v) in [1, 1, 1].iter().enumerate() {
        put_i32(&mut args, k * 4, *v);
    }
    get(be::write_buffer(&groups, 0, &args));
    get(be::write_buffer(&words, 0, &[0u8; 16]));
    let commands = get(be::begin_commands(&queue));
    let compute = get(be::compute_encoder(&commands));
    get(be::set_pipeline(&compute, &double));
    get(be::set_buffer(&compute, 0, &words));
    get(be::set_bytes(&compute, 1, &bias));
    for offset in [8, 2, -4] {
        refused(
            be::dispatch_threadgroups_indirect(&compute, &groups, offset, (2, 1, 1)),
            ErrorKind::InvalidArgument,
            "invalid indirect arguments range was accepted",
        );
    }
    get(be::dispatch_threadgroups_indirect(&compute, &groups, 0, (2, 1, 1)));
    get(be::end_compute(&compute));
    poll_epoch(&queue, get(be::commit(&commands)));
    let output = get(be::read_buffer(&words, 0, 16));
    for (i, expected) in [5, 5, 0, 0].iter().enumerate() {
        require(i32_at(&output, i * 4) == *expected, "indirect dispatch ran a group count other than its buffer's");
    }
    get(be::destroy_buffer(&groups));
    get(be::destroy_pipeline(&triple));
    get(be::destroy_pipeline(&double));

    // Abandoned commands never execute and cannot be committed.
    get(be::write_buffer(&upload, 0, &[255u8; 512]));
    let abandoned = get(be::begin_commands(&queue));
    let blit_encoder = get(be::blit_encoder(&abandoned));
    get(be::buffer_to_texture(&blit_encoder, &upload, None, 256, 512, &first, None, None, extent));
    get(be::abandon(&abandoned));
    refused(be::commit(&abandoned), ErrorKind::InvalidState, "abandoned commands were committed");
    require(get(be::read_texture(&first, 8)) == compact, "abandoned commands executed");
    get(be::destroy_library(&library));
    get(be::destroy_buffer(&words));
    get(be::destroy_buffer(&local));

    // A second queue's epochs are its own.
    let other_queue = get(be::create_queue(&device));
    require(be::completed_epoch(&other_queue) == 0, "new queue inherited another queue's completion");
    let other_commands = get(be::begin_commands(&other_queue));
    let other_blit = get(be::blit_encoder(&other_commands));
    get(be::buffer_to_texture(&other_blit, &upload, None, 256, 512, &first, None, None, extent));
    get(be::end_blit(&other_blit));
    let other = get(be::commit(&other_commands));
    require(other.epoch == 1, "queue epochs are not independent");
    poll_epoch(&other_queue, other);
    get(be::destroy_buffer(&source));
    refused(be::read_buffer(&source, 0, 1), ErrorKind::StaleHandle, "destroyed buffer remained readable");
    get(be::destroy_buffer(&upload));
    get(be::destroy_texture(&first));
    get(be::destroy_texture(&inset));
    get(be::destroy_queue(&other_queue));
    get(be::destroy_queue(&queue));
    finish(&device);
}

fn vertex_bytes(z: f64) -> Vec<u8> {
    let mut bytes = vec![0u8; 72];
    let values = [-1., -1., z, 1., -1., z, 0., 1., z, 3., -1., z, 5., -1., z, 4., 1., z];
    for (i, v) in values.iter().enumerate() {
        put_f32(&mut bytes, i * 4, *v);
    }
    bytes
}

fn triangles(vertices: &Buffer, length: i64, count: i64) -> AccelDescriptor {
    AccelDescriptor::Blas {
        geometries: vec![Geometry::Triangles { vertices: vertices.clone(), offset: 0, length, vertex_stride: 12, vertex_count: count }],
    }
}

fn instance(transform: &[f64; 12], mask: i64, structure_index: i64, user_id: i64) -> InstanceRecord {
    InstanceRecord { instance: Instance { transform: transform.to_vec(), mask, structure_index }, user_id }
}

/// Everything a ray query pass shares.
struct Rays {
    queue: Queue,
    rays: Buffer,
    hits: Buffer,
}

#[derive(Default)]
struct Extras {
    bytes: Option<Vec<u8>>,
    buffer3: Option<Buffer>,
    buffer4: Option<Buffer>,
    buffer5: Option<Buffer>,
    accel6: Option<Accel>,
}

impl Rays {
    fn read_hits(&self) -> Vec<Hit> {
        let bytes = get(be::read_buffer(&self.hits, 0, 96));
        (0..4)
            .map(|i| {
                (
                    i32_at(&bytes, i * 24) as i64,
                    i32_at(&bytes, i * 24 + 4) as i64,
                    i32_at(&bytes, i * 24 + 8) as i64,
                    f32_at(&bytes, i * 24 + 12),
                    f32_at(&bytes, i * 24 + 16),
                    f32_at(&bytes, i * 24 + 20),
                )
            })
            .collect()
    }

    fn query(&self, pipeline: &Pipeline, structure: &Accel, extras: &Extras) -> Vec<Hit> {
        let commands = get(be::begin_commands(&self.queue));
        let compute = get(be::compute_encoder(&commands));
        get(be::set_pipeline(&compute, pipeline));
        get(be::set_accel(&compute, 0, structure));
        get(be::set_buffer(&compute, 1, &self.rays));
        get(be::set_buffer(&compute, 2, &self.hits));
        if let Some(bytes) = &extras.bytes {
            get(be::set_bytes(&compute, 3, bytes));
        }
        for (index, buffer) in [(3, &extras.buffer3), (4, &extras.buffer4), (5, &extras.buffer5)] {
            if let Some(buffer) = buffer {
                get(be::set_buffer(&compute, index, buffer));
            }
        }
        if let Some(accel) = &extras.accel6 {
            get(be::set_accel(&compute, 6, accel));
        }
        get(be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)));
        get(be::end_compute(&compute));
        poll_epoch(&self.queue, get(be::commit(&commands)));
        self.read_hits()
    }

    fn build_one(&self, structure: &Accel, scratch: &Buffer) {
        let commands = get(be::begin_commands(&self.queue));
        let encoder = get(be::accel_encoder(&commands));
        get(be::build_accel(&encoder, structure, scratch, None));
        get(be::end_accel(&encoder));
        poll_epoch(&self.queue, get(be::commit(&commands)));
    }
}

const IDENTITY: [f64; 12] = [1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0.];
const SHIFTED: [f64; 12] = [1., 0., 0., 8., 0., 1., 0., 0., 0., 0., 1., 0.];
const FAR: [f64; 12] = [1., 0., 0., 20., 0., 1., 0., 0., 0., 0., 1., 0.];

/// Ray tracing: deterministic primitive ids, t and barycentrics; TLAS instance ids; refit; procedural
/// box and capsule candidates; motion keyframes; user-id instances with masks; motion instances;
/// compaction; typed Unsupported for curves.
#[test]
fn gpu_conformance_ray_tracing() {
    let Some(device) = open() else { return };
    let capabilities = be::capabilities(&device);
    let profile = capabilities;
    require(profile.ray_tracing && profile.ray_tracing_motion && profile.ray_tracing_refit, "this device lacks ray queries");
    let queue = get(be::create_queue(&device));
    let ray_interface = vec![
        compute_binding(0, BindingKind::AccelerationStructure),
        compute_binding(1, BindingKind::StorageBuffer),
        compute_binding(2, BindingKind::StorageBuffer),
    ];
    let ray_shader = make_shader(
        "conformance-rays",
        shader_bytes("rays"),
        &[("trace", Stage::Compute), ("trace_instances", Stage::Compute)],
        vec![],
    );
    // Two triangles in z=0 (primitive 0 around x=0, primitive 1 around x=4).
    let vertices = make_buffer(&device, "ray-vertices", 72, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&vertices, 0, &vertex_bytes(0.)));
    // Rays from z=2 toward -z at x=0, x=4, x=8 (miss), x=12 (miss).
    let mut ray_bytes = vec![0u8; 128];
    for (i, x) in [0., 4., 8., 12.].iter().enumerate() {
        for (k, v) in [*x, 0., 2., 0., 0., 0., -1., 0.].iter().enumerate() {
            put_f32(&mut ray_bytes, (i * 8 + k) * 4, *v);
        }
    }
    let rays = make_buffer(&device, "rays", 128, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&rays, 0, &ray_bytes));
    let hits = make_buffer(&device, "hits", 96, &[BufferUsage::Storage, BufferUsage::CopySrc]);
    let rt = Rays { queue: get(be::create_queue(&device)), rays, hits };
    let blas_descriptor = triangles(&vertices, 72, 6);

    let ray_library = get(be::create_library(&device, &ray_shader));
    let trace = get(be::create_compute_pipeline_from(&ray_library, "trace", None, &ray_interface));
    let trace_instances = get(be::create_compute_pipeline_from(&ray_library, "trace_instances", None, &ray_interface));
    let blas = get(be::create_accel(&device, blas_descriptor.clone(), false));
    let sizes = be::accel_sizes(&blas);
    require(sizes.structure_size > 0 && sizes.build_scratch_size >= 0, "BLAS sizes are empty");
    let scratch = local_buffer(&device, "scratch", 256.max(2 * sizes.build_scratch_size), &[BufferUsage::Storage]);
    let instances = get(be::pack_instance_records(&device, &[instance(&IDENTITY, 0xFFFF_FFFF, 0, 0), instance(&SHIFTED, 0xFFFF_FFFF, 0, 1)]));
    let stride = instances.len() / 2;
    let instance_buffer = make_buffer(&device, "instances", 2 * stride as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&instance_buffer, 0, &instances));
    let tlas_of = |buffer: &Buffer, count: i64, structures: Vec<Accel>| AccelDescriptor::TlasOf {
        instances: buffer.clone(),
        offset: 0,
        instance_count: count,
        kind: InstanceKind::UserIdInstances,
        structures,
        motion_transforms: None,
    };
    let tlas = get(be::create_accel(&device, tlas_of(&instance_buffer, 2, vec![blas.clone()]), false));
    refused(
        be::create_accel(&device, tlas_of(&instance_buffer, 3, vec![blas.clone()]), false),
        ErrorKind::InvalidArgument,
        "oversized TLAS instance count was accepted",
    );
    let tlas_scratch = local_buffer(&device, "tlas-scratch", 256.max(be::accel_sizes(&tlas).build_scratch_size), &[BufferUsage::Storage]);
    let commands = get(be::begin_commands(&queue));
    let compute = get(be::compute_encoder(&commands));
    get(be::set_pipeline(&compute, &trace));
    refused(be::set_accel(&compute, 0, &blas), ErrorKind::InvalidState, "unbuilt structure was bound");
    refused(be::compute_use_accels(&compute, &[blas.clone()]), ErrorKind::InvalidState, "unbuilt structure was declared used");
    get(be::compute_use_accels(&compute, &[]));
    get(be::end_compute(&compute));
    let build = get(be::accel_encoder(&commands));
    refused(be::build_accel(&build, &tlas, &tlas_scratch, None), ErrorKind::InvalidState, "TLAS built before its BLAS");
    get(be::build_accel(&build, &blas, &scratch, None));
    refused(be::build_accel(&build, &blas, &scratch, None), ErrorKind::InvalidState, "double build was accepted");
    get(be::build_accel(&build, &tlas, &tlas_scratch, None));
    get(be::end_accel(&build));
    let query_in = |commands: &be::Commands, pipeline: &Pipeline, structure: &Accel| {
        let compute = get(be::compute_encoder(commands));
        get(be::set_pipeline(&compute, pipeline));
        get(be::set_accel(&compute, 0, structure));
        get(be::compute_use_accels(&compute, &[blas.clone()]));
        get(be::set_buffer(&compute, 1, &rt.rays));
        get(be::set_buffer(&compute, 2, &rt.hits));
        get(be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)));
        get(be::end_compute(&compute));
    };
    query_in(&commands, &trace, &blas);
    poll_epoch(&queue, get(be::commit(&commands)));
    check_hits(
        "primitive triangle",
        &rt.read_hits(),
        &[triangle_hit(0, 0, 2.), triangle_hit(1, 0, 2.), NO_HIT, NO_HIT],
    );
    let commands = get(be::begin_commands(&queue));
    query_in(&commands, &trace_instances, &tlas);
    poll_epoch(&queue, get(be::commit(&commands)));
    check_hits(
        "triangle instances",
        &rt.read_hits(),
        &[triangle_hit(0, 0, 2.), triangle_hit(1, 0, 2.), triangle_hit(0, 1, 2.), triangle_hit(1, 1, 2.)],
    );

    // Refit: a refittable structure updated in place from new vertex data of the same shape (the
    // triangles moved to z = 1, so the rays from z = 2 hit at t = 1).
    let refittable = get(be::create_accel(&device, blas_descriptor.clone(), true));
    let moved = make_buffer(&device, "ray-vertices-moved", 72, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&moved, 0, &vertex_bytes(1.)));
    let moved_descriptor = triangles(&moved, 72, 6);
    let refit_scratch = local_buffer(
        &device,
        "refit-scratch",
        256.max(be::accel_sizes(&refittable).build_scratch_size.max(be::accel_sizes(&refittable).refit_scratch_size)),
        &[BufferUsage::Storage],
    );
    let commands = get(be::begin_commands(&queue));
    let encoder = get(be::accel_encoder(&commands));
    refused(
        be::refit_accel(&encoder, &refittable, moved_descriptor.clone(), &refit_scratch, None),
        ErrorKind::InvalidState,
        "an unbuilt structure was refit",
    );
    get(be::build_accel(&encoder, &refittable, &refit_scratch, None));
    refused(
        be::refit_accel(&encoder, &blas, moved_descriptor.clone(), &refit_scratch, None),
        ErrorKind::InvalidState,
        "a structure built without refit was refit",
    );
    refused(
        be::refit_accel(&encoder, &refittable, triangles(&moved, 36, 3), &refit_scratch, None),
        ErrorKind::InvalidArgument,
        "a refit of another shape was accepted",
    );
    get(be::refit_accel(&encoder, &refittable, moved_descriptor, &refit_scratch, None));
    get(be::end_accel(&encoder));
    query_in(&commands, &trace, &refittable);
    poll_epoch(&queue, get(be::commit(&commands)));
    check_hits("refit triangles", &rt.read_hits(), &[triangle_hit(0, 0, 1.), triangle_hit(1, 0, 1.), NO_HIT, NO_HIT]);
    get(be::destroy_accel(&refittable));
    get(be::destroy_buffer(&refit_scratch));
    get(be::destroy_buffer(&moved));

    // Procedural box/capsule candidates, motion keyframes, user-id instances with masks, and compaction.
    let extra_shader = make_shader(
        "conformance-rt-extra",
        shader_bytes("rt_extra"),
        &[
            ("trace_boxes", Stage::Compute),
            ("trace_capsules", Stage::Compute),
            ("trace_masked", Stage::Compute),
            ("trace_motion", Stage::Compute),
            ("trace_motion_instances", Stage::Compute),
        ],
        vec![],
    );
    let extra = get(be::create_library(&device, &extra_shader));
    let with = |extra_bindings: &[Binding]| -> Vec<Binding> {
        let mut all = ray_interface.clone();
        all.extend_from_slice(extra_bindings);
        all
    };
    let boxes_pipeline = get(be::create_compute_pipeline_from(&extra, "trace_boxes", None, &with(&[compute_binding(3, BindingKind::StorageBuffer)])));
    let capsules_pipeline = get(be::create_compute_pipeline_from(&extra, "trace_capsules", None, &with(&[compute_binding(3, BindingKind::StorageBuffer)])));
    let spheres = float_buffer(&device, "spheres", &[0., 0., 0., 0.5, 4., 0., 0., 0.5]);
    let boxes = float_buffer(&device, "boxes", &[-0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 3.5, -0.5, -0.5, 4.5, 0.5, 0.5]);
    let box_geometry = |buffer: &Buffer, count: i64| AccelDescriptor::Blas {
        geometries: vec![Geometry::BoundingBoxes {
            boxes: vec![Keyframe { buffer: buffer.clone(), offset: 0 }],
            stride: 24,
            count,
            options: DEFAULT_GEOMETRY_OPTIONS,
        }],
    };
    let box_blas = get(be::create_accel(&device, box_geometry(&boxes, 2), false));
    let capsule_boxes = float_buffer(&device, "capsule-boxes", &[7.5, -1.5, -0.5, 8.5, 1.5, 0.5]);
    let capsule_data = float_buffer(&device, "capsules", &[8., -1., 0., 0.5, 8., 1., 0., 0.5]);
    let capsule_blas = get(be::create_accel(&device, box_geometry(&capsule_boxes, 1), false));
    let big_scratch = local_buffer(&device, "scratch-extra", 1_048_576, &[BufferUsage::Storage]);
    rt.build_one(&box_blas, &big_scratch);
    rt.build_one(&capsule_blas, &big_scratch);
    let box_hits = rt.query(&boxes_pipeline, &box_blas, &Extras { buffer3: Some(spheres.clone()), ..Extras::default() });
    check_hits(
        "procedural sphere candidates",
        &box_hits,
        &[generated_hit(0, 0, 1.5), generated_hit(1, 0, 1.5), NO_HIT, NO_HIT],
    );
    let capsule_hits = rt.query(&capsules_pipeline, &capsule_blas, &Extras { buffer3: Some(capsule_data.clone()), ..Extras::default() });
    check_hits("procedural capsule candidates", &capsule_hits, &[NO_HIT, NO_HIT, generated_hit(0, 0, 1.5), NO_HIT]);

    // Curves: this device has no native curve intersection, so the descriptor is typed Unsupported.
    let control = float_buffer(&device, "curve-points", &[8., -1., 0., 8., 1., 0.]);
    let radii = float_buffer(&device, "curve-radii", &[0.5, 0.5]);
    let indices = make_buffer(&device, "curve-indices", 4, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&indices, 0, &[0u8; 4]));
    let curve_descriptor = AccelDescriptor::Blas {
        geometries: vec![Geometry::Curves {
            control_points: vec![Keyframe { buffer: control.clone(), offset: 0 }],
            control_stride: 12,
            control_point_count: 2,
            radii: vec![Keyframe { buffer: radii.clone(), offset: 0 }],
            radius_stride: 4,
            indices: indices.clone(),
            index_offset: 0,
            segment_count: 1,
            control_points_per_segment: 2,
            curve_type: CurveType::RoundCurve,
            basis: CurveBasis::LinearBasis,
            caps: CurveCaps::SphereCaps,
        }],
    };
    require(!caps::has(&profile, &Feature::RayTracingCurves), "Vulkan reports native curves");
    refused(
        be::create_accel(&device, curve_descriptor, false),
        ErrorKind::Unsupported,
        "curve geometry without curve support was accepted",
    );

    // Motion: keyframes at z=0 and z=1.5; time selects the interpolated depth.
    let time_bytes = |t: f64| {
        let mut bytes = vec![0u8; 4];
        put_f32(&mut bytes, 0, t);
        bytes
    };
    let motion_bindings = [compute_binding(4, BindingKind::StorageBuffer), compute_binding(5, BindingKind::StorageBuffer)];
    let vertices0 = make_buffer(&device, "motion-0", 72, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    let vertices1 = make_buffer(&device, "motion-1", 72, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&vertices0, 0, &vertex_bytes(0.)));
    get(be::write_buffer(&vertices1, 0, &vertex_bytes(1.5)));
    let motion = Motion { keyframes: 2, start_time: 0., end_time: 1., start_border: Border::Clamp, end_border: Border::Clamp };
    let motion_triangles = |keyframes: Vec<Keyframe>| AccelDescriptor::MotionBlas {
        geometries: vec![Geometry::MotionTriangles { keyframes, vertex_stride: 12, vertex_count: 6 }],
        motion,
    };
    refused(
        be::create_accel(&device, motion_triangles(vec![Keyframe { buffer: vertices0.clone(), offset: 0 }]), false),
        ErrorKind::InvalidArgument,
        "keyframe count mismatch was accepted",
    );
    let motion_blas = get(be::create_accel(
        &device,
        motion_triangles(vec![Keyframe { buffer: vertices0.clone(), offset: 0 }, Keyframe { buffer: vertices1.clone(), offset: 0 }]),
        false,
    ));
    rt.build_one(&motion_blas, &big_scratch);
    let motion_pipeline = get(be::create_compute_pipeline_from(
        &extra,
        "trace_motion",
        None,
        &with(&[compute_binding(3, BindingKind::UniformBuffer), motion_bindings[0].clone(), motion_bindings[1].clone()]),
    ));
    for (time, expected_t) in [(0., 2.), (1., 0.5), (0.5, 1.25)] {
        let motion_hits = rt.query(
            &motion_pipeline,
            &motion_blas,
            &Extras { bytes: Some(time_bytes(time)), buffer4: Some(vertices0.clone()), buffer5: Some(vertices1.clone()), ..Extras::default() },
        );
        check_hits(
            &format!("motion triangles at time {time}"),
            &motion_hits,
            &[triangle_hit(0, 0, expected_t), triangle_hit(1, 0, expected_t), NO_HIT, NO_HIT],
        );
    }

    // User-id instances with masks over the triangle BLAS.
    let records = get(be::pack_instance_records(&device, &[instance(&IDENTITY, 1, 0, 7), instance(&SHIFTED, 2, 0, 9)]));
    require(records.len() == 2 * 68, "user-id record stride differs");
    let record_buffer = make_buffer(&device, "user-id-instances", records.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&record_buffer, 0, &records));
    let masked_tlas = get(be::create_accel(&device, tlas_of(&record_buffer, 2, vec![blas.clone()]), false));
    rt.build_one(&masked_tlas, &big_scratch);
    let masked_pipeline = get(be::create_compute_pipeline_from(
        &extra,
        "trace_masked",
        None,
        &with(&[compute_binding(3, BindingKind::UniformBuffer)]),
    ));
    let mask_bytes = |mask: i32| mask.to_le_bytes().to_vec();
    let masked = rt.query(&masked_pipeline, &masked_tlas, &Extras { bytes: Some(mask_bytes(1)), ..Extras::default() });
    check_hits("mask 1 instances", &masked, &[triangle_hit(0, 7, 2.), triangle_hit(1, 7, 2.), NO_HIT, NO_HIT]);
    let masked = rt.query(&masked_pipeline, &masked_tlas, &Extras { bytes: Some(mask_bytes(2)), ..Extras::default() });
    check_hits("mask 2 instances", &masked, &[NO_HIT, NO_HIT, triangle_hit(0, 9, 2.), triangle_hit(1, 9, 2.)]);

    // The off-ray record 0 and tested record 1 distinguish InstanceId=1, user_id=5, and prototype
    // structure_index=0. Record 1 moves from x=0 to x=8.
    let keyframes = get(be::pack_transforms(&[FAR.to_vec(), FAR.to_vec(), IDENTITY.to_vec(), SHIFTED.to_vec()]));
    let keyframe_buffer = make_buffer(&device, "instance-keyframes", keyframes.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&keyframe_buffer, 0, &keyframes));
    let prototype_vertices = make_buffer(&device, "motion-instance-prototype", 72, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&prototype_vertices, 0, &vertex_bytes(1.5)));
    let prototype_blas = get(be::create_accel(&device, triangles(&prototype_vertices, 72, 6), false));
    rt.build_one(&prototype_blas, &big_scratch);
    let prototype_records = get(be::pack_instance_records(&device, &[instance(&IDENTITY, 1, 0, 0), instance(&IDENTITY, 1, 1, 1)]));
    let prototype_record_buffer =
        make_buffer(&device, "motion-prototypes", prototype_records.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&prototype_record_buffer, 0, &prototype_records));
    let prototype_tlas = get(be::create_accel(&device, tlas_of(&prototype_record_buffer, 2, vec![blas.clone(), prototype_blas.clone()]), false));
    rt.build_one(&prototype_tlas, &big_scratch);
    let motion_instance = |transform: &[f64; 12], structure_index: i64, user_id: i64, start: i64| MotionInstance {
        record: instance(transform, 1, structure_index, user_id),
        transforms_start: start,
        transforms_count: 2,
        start_time: 0.,
        end_time: 1.,
        start_border: Border::Clamp,
        end_border: Border::Clamp,
    };
    let motion_records = get(be::pack_motion_instances(&device, &[motion_instance(&FAR, 0, 3, 0), motion_instance(&IDENTITY, 1, 5, 2)]));
    require(motion_records.len() == 2 * 44, "motion instance record stride differs");
    let motion_record_buffer =
        make_buffer(&device, "motion-instances", motion_records.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    get(be::write_buffer(&motion_record_buffer, 0, &motion_records));
    let motion_tlas_of = |transforms: Option<(Buffer, i64, i64)>| AccelDescriptor::TlasOf {
        instances: motion_record_buffer.clone(),
        offset: 0,
        instance_count: 2,
        kind: InstanceKind::MotionInstances,
        structures: vec![blas.clone(), prototype_blas.clone()],
        motion_transforms: transforms,
    };
    refused(
        be::create_accel(&device, motion_tlas_of(None), false),
        ErrorKind::InvalidArgument,
        "motion instances without keyframe transforms were accepted",
    );
    let motion_tlas = get(be::create_accel(&device, motion_tlas_of(Some((keyframe_buffer.clone(), 0, 4))), false));
    rt.build_one(&motion_tlas, &big_scratch);
    let motion_instances_pipeline = get(be::create_compute_pipeline_from(
        &extra,
        "trace_motion_instances",
        None,
        &with(&[
            compute_binding(3, BindingKind::UniformBuffer),
            motion_bindings[0].clone(),
            motion_bindings[1].clone(),
            compute_binding(6, BindingKind::AccelerationStructure),
        ]),
    ));
    let query_motion_instances = |time: f64| {
        rt.query(
            &motion_instances_pipeline,
            &motion_tlas,
            &Extras {
                bytes: Some(time_bytes(time)),
                buffer4: Some(motion_record_buffer.clone()),
                buffer5: Some(keyframe_buffer.clone()),
                accel6: Some(prototype_tlas.clone()),
                ..Extras::default()
            },
        )
    };
    check_hits("motion instance at time 0", &query_motion_instances(0.), &[triangle_hit(0, 5, 0.5), triangle_hit(1, 5, 0.5), NO_HIT, NO_HIT]);
    check_hits("motion instance at time 1", &query_motion_instances(1.), &[NO_HIT, NO_HIT, triangle_hit(0, 5, 0.5), triangle_hit(1, 5, 0.5)]);

    // Compaction of the triangle BLAS reproduces its hits.
    let size_buffer = make_buffer(&device, "compacted-size", 8, &[BufferUsage::Storage, BufferUsage::CopySrc]);
    let commands = get(be::begin_commands(&queue));
    let encoder = get(be::accel_encoder(&commands));
    refused(
        be::write_compacted_size(&encoder, &blas, &size_buffer, Some(4)),
        ErrorKind::InvalidArgument,
        "unaligned compacted size offset was accepted",
    );
    get(be::write_compacted_size(&encoder, &blas, &size_buffer, None));
    get(be::end_accel(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    let size_bytes = get(be::read_buffer(&size_buffer, 0, 8));
    let compact_size = i64::from_le_bytes(size_bytes[..8].try_into().unwrap());
    require(compact_size > 0 && compact_size <= sizes.structure_size, "compacted size is out of range");
    let compacted = get(be::create_accel(&device, AccelDescriptor::Sized { size: compact_size, template: blas.clone() }, false));
    let commands = get(be::begin_commands(&queue));
    let encoder = get(be::accel_encoder(&commands));
    refused(
        be::build_accel(&encoder, &compacted, &big_scratch, None),
        ErrorKind::InvalidState,
        "sized structure accepted a build",
    );
    get(be::compact_accel(&encoder, &blas, &compacted));
    get(be::end_accel(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    check_hits(
        "compacted triangles",
        &rt.query(&trace, &compacted, &Extras::default()),
        &[triangle_hit(0, 0, 2.), triangle_hit(1, 0, 2.), NO_HIT, NO_HIT],
    );

    for pipeline in [&motion_instances_pipeline, &motion_pipeline, &boxes_pipeline, &capsules_pipeline, &masked_pipeline] {
        get(be::destroy_pipeline(pipeline));
    }
    for accel in [&motion_tlas, &prototype_tlas, &prototype_blas, &motion_blas, &compacted, &masked_tlas, &capsule_blas, &box_blas] {
        get(be::destroy_accel(accel));
    }
    for buffer in [
        &motion_record_buffer, &keyframe_buffer, &prototype_record_buffer, &prototype_vertices, &vertices1, &vertices0, &size_buffer,
        &record_buffer, &indices, &radii, &control, &capsule_data, &capsule_boxes, &big_scratch, &boxes, &spheres,
    ] {
        get(be::destroy_buffer(buffer));
    }
    get(be::destroy_library(&extra));
    get(be::destroy_accel(&blas));
    get(be::destroy_accel(&tlas));
    get(be::destroy_buffer(&instance_buffer));
    get(be::destroy_buffer(&tlas_scratch));
    get(be::destroy_buffer(&scratch));
    get(be::destroy_pipeline(&trace));
    get(be::destroy_pipeline(&trace_instances));
    get(be::destroy_library(&ray_library));
    get(be::destroy_buffer(&vertices));
    get(be::destroy_buffer(&rt.rays));
    get(be::destroy_buffer(&rt.hits));
    get(be::destroy_queue(&rt.queue));
    get(be::destroy_queue(&queue));
    finish(&device);
}

fn color_bytes(r: f64, g: f64, b: f64, a: f64) -> Vec<u8> {
    let mut bytes = vec![0u8; 16];
    for (i, v) in [r, g, b, a].iter().enumerate() {
        put_f32(&mut bytes, i * 4, *v);
    }
    bytes
}

/// The render path: samplers, portable render pipelines, the render encoder with exact pixels,
/// batched and indirect draws, indirect command buffers, argument buffers, direct texture and sampler
/// binding, depth load and clear, multisample resolve, and typed Unsupported memoryless attachments.
#[test]
fn gpu_conformance_render() {
    let Some(device) = open() else { return };
    let capabilities = be::capabilities(&device);
    let profile = capabilities;
    let queue = get(be::create_queue(&device));
    let sampler = get(be::create_sampler(
        &device,
        SamplerDescriptor {
            label: Some("conformance-sampler".to_owned()),
            min_filter: SamplerFilter::Nearest,
            mag_filter: SamplerFilter::Nearest,
            mip_filter: MipFilter::NoMip,
            address_u: AddressMode::ClampToEdge,
            address_v: AddressMode::ClampToEdge,
            lod_min: 0.,
            lod_max: 0.,
            max_anisotropy: 1,
        },
    ));
    let render_shader = |entry: &str, stage: Stage, bindings: Vec<Binding>| {
        make_shader("conformance-render", shader_bytes(entry), &[(entry, stage)], bindings)
    };
    let stage_binding = |binding: i64, kind: BindingKind, stage: Stage| Binding { group: 0, binding, kind, visibility: vec![stage] };
    let vertex = render_shader("flat_vertex", Stage::Vertex, vec![stage_binding(0, BindingKind::StorageBuffer, Stage::Vertex)]);
    let fragment_flat = render_shader("flat_fragment", Stage::Fragment, vec![stage_binding(1, BindingKind::UniformBuffer, Stage::Fragment)]);
    let fragment_argument =
        render_shader("argument_fragment", Stage::Fragment, vec![stage_binding(1, BindingKind::StorageBuffer, Stage::Fragment)]);
    let fragment_textured = render_shader(
        "textured_fragment",
        Stage::Fragment,
        vec![stage_binding(1, BindingKind::SampledTexture, Stage::Fragment), stage_binding(2, BindingKind::Sampler, Stage::Fragment)],
    );
    let render_layout = |entries: &[LayoutEntry]| {
        let group = get(binding::create_layout(entries));
        get(binding::create_pipeline_layout(be::device_handle(&device), &capabilities, &[(0, group)]))
    };
    let vertex_entry = LayoutEntry { binding: 0, kind: LayoutKind::Buffer, visibility: vec![LayoutStage::Vertex] };
    let flat_layout = render_layout(&[
        vertex_entry.clone(),
        LayoutEntry { binding: 1, kind: LayoutKind::Buffer, visibility: vec![LayoutStage::Fragment] },
    ]);
    let textured_layout = render_layout(&[
        vertex_entry,
        LayoutEntry { binding: 1, kind: LayoutKind::Texture, visibility: vec![LayoutStage::Fragment] },
        LayoutEntry { binding: 2, kind: LayoutKind::Sampler, visibility: vec![LayoutStage::Fragment] },
    ]);
    let render_descriptor = |layout: &binding::PipelineLayout, fragment: &Shader, entry: &str| RenderDescriptor {
        backend: "vulkan".to_owned(),
        label: Some("conformance-render".to_owned()),
        layout: layout.clone(),
        vertex: vertex.clone(),
        vertex_entry: "flat_vertex".to_owned(),
        fragment: Some(fragment.clone()),
        fragment_entry: Some(entry.to_owned()),
        color_format: ColorFormat::Rgba8Unorm,
        depth_format: DepthFormat::NoDepth,
        sample_count: 1,
    };
    let target_usage = [TextureUsage::TextureBinding, TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc];
    let target_descriptor = texture_descriptor("render-target", 4, &target_usage);
    let target = get(be::create_texture(&device, target_descriptor.clone()));
    let transient = TextureDescriptor { label: Some("pass".to_owned()), usage: vec![TextureUsage::RenderAttachment], ..target_descriptor.clone() };
    require(!profile.memoryless_attachments, "Vulkan reports memoryless attachments");
    refused(
        be::create_memoryless_texture(&device, true, transient),
        ErrorKind::Unsupported,
        "unavailable memoryless attachments were not typed Unsupported",
    );
    refused(
        be::create_buffer(&device, buf("pass", 16, &[BufferUsage::Storage]), Some(Memory::Memoryless)),
        ErrorKind::InvalidArgument,
        "a memoryless buffer was accepted",
    );
    let offsets = make_buffer(&device, "offsets", 8, &[BufferUsage::Storage]);
    get(be::write_buffer(&offsets, 0, &[0u8; 8]));
    let expect_pixels = |expected: [u8; 4], message: &str| {
        let pixels = get(be::read_texture(&target, 16));
        let pixel = |x: usize, y: usize| pixels[(y * 4 + x) * 4..(y * 4 + x) * 4 + 4].to_vec();
        require(
            pixel(0, 0) == expected && pixel(3, 3) == expected,
            &format!("{message} (pixels (0,0) {:02x?} and (3,3) {:02x?}, expected {expected:02x?})", pixel(0, 0), pixel(3, 3)),
        );
    };
    let clear_target = |r: f64, g: f64, b: f64, a: f64| {
        let commands = get(be::begin_commands(&queue));
        let encoder = get(be::render_encoder(&commands, &render_target(&target, Load::Clear, Store::Store, (r, g, b, a))));
        get(be::end_render(&encoder));
        poll_epoch(&queue, get(be::commit(&commands)));
    };
    let load_pass = |commands: &be::Commands| {
        get(be::render_encoder(commands, &render_target(&target, Load::Load, Store::Store, (0., 0., 0., 0.))))
    };
    let full = Rect { x: 0, y: 0, width: 4, height: 4 };
    require(caps::has(&profile, &Feature::RenderPipeline), "this device cannot render");

    let flat = get(be::create_render_pipeline(&device, &render_descriptor(&flat_layout, &fragment_flat, "flat_fragment"), None, None, None));
    clear_target(0.125, 0.25, 0.5, 1.);
    expect_pixels([0x20, 0x40, 0x80, 0xff], "clear-only render pass pixels differ");
    // One full-screen triangle through the immediate encoder with a fragment uniform buffer.
    let color = make_buffer(&device, "color", 16, &[BufferUsage::Uniform]);
    let set_color = |encoder: &be::RenderEncoder, r: f64, g: f64, b: f64, a: f64| {
        get(be::write_buffer(&color, 0, &color_bytes(r, g, b, a)));
        get(be::set_stage_buffer(encoder, ShaderStage::Fragment, 1, None, &color));
    };
    let commands = get(be::begin_commands(&queue));
    let encoder = load_pass(&commands);
    refused(
        be::draw(&encoder, Primitive::TriangleList, 0, 3, None),
        ErrorKind::InvalidState,
        "draw without a render pipeline was accepted",
    );
    get(be::set_render_pipeline(&encoder, &flat));
    get(be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets));
    set_color(&encoder, 1., 0., 0.5, 1.);
    // A viewport may hang over the target; an empty one is refused.
    refused(
        be::set_viewport(&encoder, Rect { x: 0, y: 0, width: 0, height: 4 }),
        ErrorKind::InvalidArgument,
        "empty viewport was accepted",
    );
    get(be::set_viewport(&encoder, Rect { x: -2, y: -2, width: 8, height: 8 }));
    get(be::set_viewport(&encoder, full));
    get(be::set_scissor(&encoder, full));
    get(be::set_cull(&encoder, Cull::CullNone));
    get(be::draw(&encoder, Primitive::TriangleList, 0, 3, None));
    get(be::end_render(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    expect_pixels([0xff, 0x00, 0x80, 0xff], "immediate triangle pixels differ");

    // The same triangle as one indexed batch draw.
    let indices = make_buffer(&device, "indices", 12, &[BufferUsage::Index]);
    let mut index_bytes = [0u8; 12];
    for (i, v) in [0, 1, 2].iter().enumerate() {
        put_i32(&mut index_bytes, i * 4, *v);
    }
    get(be::write_buffer(&indices, 0, &index_bytes));
    get(be::write_buffer(&color, 0, &color_bytes(0., 1., 0., 1.)));
    clear_target(0., 0., 0., 1.);
    let commands = get(be::begin_commands(&queue));
    let encoder = load_pass(&commands);
    get(be::set_viewport(&encoder, full));
    get(be::draw_batch(
        &encoder,
        &[BatchDraw {
            pipeline: flat.clone(),
            buffers: vec![(ShaderStage::Vertex, 0, offsets.clone(), 0), (ShaderStage::Fragment, 1, color.clone(), 0)],
            primitive: Primitive::TriangleList,
            index: Some((IndexType::Uint32, indices.clone(), 0, 3)),
            vertex_start: 0,
            vertex_count: 3,
            instances: 1,
        }],
    ));
    get(be::end_render(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    expect_pixels([0x00, 0xff, 0x00, 0xff], "batched indexed triangle pixels differ");

    // The same indexed triangle, its counts read from a buffer when the draw runs (index count 3, one
    // instance, at offset 4); a misaligned argument range is refused.
    let arguments = make_buffer(&device, "draw arguments", 24, &[BufferUsage::Storage]);
    let mut argument_bytes = [0u8; 24];
    for (i, v) in [0, 3, 1, 0, 0, 0].iter().enumerate() {
        put_i32(&mut argument_bytes, i * 4, *v);
    }
    get(be::write_buffer(&arguments, 0, &argument_bytes));
    get(be::write_buffer(&color, 0, &color_bytes(1., 1., 0., 1.)));
    clear_target(0., 0., 0., 1.);
    let commands = get(be::begin_commands(&queue));
    let encoder = load_pass(&commands);
    get(be::set_viewport(&encoder, full));
    get(be::set_render_pipeline(&encoder, &flat));
    get(be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets));
    get(be::set_stage_buffer(&encoder, ShaderStage::Fragment, 1, None, &color));
    refused(
        be::draw_indexed_indirect(&encoder, Primitive::TriangleList, IndexType::Uint32, &indices, 0, &arguments, 6),
        ErrorKind::InvalidArgument,
        "misaligned indirect arguments were accepted",
    );
    get(be::draw_indexed_indirect(&encoder, Primitive::TriangleList, IndexType::Uint32, &indices, 0, &arguments, 4));
    get(be::end_render(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    expect_pixels([0xff, 0xff, 0x00, 0xff], "indirect indexed triangle pixels differ");

    // Indirect command buffer: one recorded draw, executed twice.
    let indirect =
        get(be::create_render_pipeline(&device, &render_descriptor(&flat_layout, &fragment_flat, "flat_fragment"), None, None, Some(true)));
    let icb = get(be::create_icb(&device, 2));
    refused(
        be::icb_set_pipeline(&icb, 2, &indirect),
        ErrorKind::InvalidArgument,
        "out-of-range indirect command index was accepted",
    );
    get(be::write_buffer(&color, 0, &color_bytes(0., 0., 1., 1.)));
    get(be::icb_set_pipeline(&icb, 0, &indirect));
    get(be::icb_set_buffer(&icb, 0, ShaderStage::Vertex, 0, None, &offsets));
    get(be::icb_set_buffer(&icb, 0, ShaderStage::Fragment, 1, None, &color));
    get(be::icb_draw_indexed(&icb, 0, Primitive::TriangleList, IndexType::Uint32, &indices, 0, 3, None));
    clear_target(0., 0., 0., 1.);
    for _ in 0..2 {
        let commands = get(be::begin_commands(&queue));
        let encoder = load_pass(&commands);
        get(be::set_viewport(&encoder, full));
        refused(
            be::execute_icb(&encoder, &icb, 0, 1),
            ErrorKind::InvalidState,
            "indirect execution without a pipeline was accepted",
        );
        get(be::set_render_pipeline(&encoder, &indirect));
        get(be::use_resources(
            &encoder,
            &[ResourceUse::Buffer(offsets.clone()), ResourceUse::Buffer(color.clone()), ResourceUse::Buffer(indices.clone())],
        ));
        get(be::execute_icb(&encoder, &icb, 0, 1));
        get(be::end_render(&encoder));
        poll_epoch(&queue, get(be::commit(&commands)));
    }
    expect_pixels([0x00, 0x00, 0xff, 0xff], "indirect command pixels differ");

    // Argument buffer: a 1x1 texture and sampler bound through one buffer.
    let argument_pipeline = get(be::create_render_pipeline(
        &device,
        &render_descriptor(&flat_layout, &fragment_argument, "argument_fragment"),
        None,
        None,
        Some(true),
    ));
    let argument = get(be::create_argument(&argument_pipeline, ShaderStage::Fragment, 1));
    require(be::argument_length(&argument) > 0, "argument length is empty");
    let argument_buffer = make_buffer(&device, "arguments", 256.max(be::argument_length(&argument)), &[BufferUsage::Storage]);
    let texel = get(be::create_texture(
        &device,
        TextureDescriptor {
            label: Some("texel".to_owned()),
            width: 1,
            height: 1,
            ..texture_descriptor("texel", 1, &[TextureUsage::TextureBinding, TextureUsage::TextureCopyDst])
        },
    ));
    let texel_upload = make_buffer(&device, "texel-upload", 256, &[BufferUsage::CopySrc]);
    let mut upload_bytes = vec![0u8; 256];
    upload_bytes[..4].copy_from_slice(&[0x40, 0x80, 0xc0, 0xff]);
    get(be::write_buffer(&texel_upload, 0, &upload_bytes));
    let commands = get(be::begin_commands(&queue));
    let blit = get(be::blit_encoder(&commands));
    get(be::buffer_to_texture(&blit, &texel_upload, None, 256, 256, &texel, None, None, Extent { width: 1, height: 1, depth: 1 }));
    get(be::end_blit(&blit));
    poll_epoch(&queue, get(be::commit(&commands)));
    get(be::argument_texture(&argument, &argument_buffer, 0, 0, &texel));
    get(be::argument_sampler(&argument, &argument_buffer, 0, 1, &sampler));
    clear_target(0., 0., 0., 1.);
    let commands = get(be::begin_commands(&queue));
    let encoder = load_pass(&commands);
    get(be::set_viewport(&encoder, full));
    get(be::set_render_pipeline(&encoder, &argument_pipeline));
    get(be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets));
    get(be::set_stage_buffer(&encoder, ShaderStage::Fragment, 1, None, &argument_buffer));
    get(be::use_resources(&encoder, &[ResourceUse::Texture(texel.clone())]));
    get(be::draw(&encoder, Primitive::TriangleList, 0, 3, None));
    get(be::end_render(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    expect_pixels([0x40, 0x80, 0xc0, 0xff], "argument-buffer sampled pixels differ");

    // Direct texture and sampler binding.
    let textured = get(be::create_render_pipeline(
        &device,
        &render_descriptor(&textured_layout, &fragment_textured, "textured_fragment"),
        None,
        None,
        None,
    ));
    clear_target(0., 0., 0., 1.);
    let commands = get(be::begin_commands(&queue));
    let encoder = load_pass(&commands);
    get(be::set_viewport(&encoder, full));
    get(be::set_render_pipeline(&encoder, &textured));
    get(be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets));
    get(be::set_stage_texture(&encoder, ShaderStage::Fragment, 1, &texel));
    get(be::set_stage_sampler(&encoder, ShaderStage::Fragment, 2, &sampler));
    get(be::draw(&encoder, Primitive::TriangleList, 0, 3, None));
    get(be::end_render(&encoder));
    poll_epoch(&queue, get(be::commit(&commands)));
    expect_pixels([0x40, 0x80, 0xc0, 0xff], "directly sampled pixels differ");

    // Depth: a cleared depth attachment at 0.25 with Less rejects a triangle at z=0.5 when loaded, and
    // admits it after a clear to 1.
    let depth = get(be::create_depth_texture(
        &device,
        texture_descriptor("depth", 4, &[TextureUsage::RenderAttachment]),
    ));
    let depth_shader = render_shader("flat_vertex", Stage::Vertex, vec![stage_binding(0, BindingKind::StorageBuffer, Stage::Vertex)]);
    let depth_pipeline = get(be::create_render_pipeline(
        &device,
        &RenderDescriptor {
            vertex: depth_shader,
            depth_format: DepthFormat::Depth32Float,
            ..render_descriptor(&flat_layout, &fragment_flat, "flat_fragment")
        },
        None,
        None,
        None,
    ));
    let render_depth = |load: Load, clear: f64, expected: [u8; 4], message: &str| {
        clear_target(0., 0., 0., 1.);
        let commands = get(be::begin_commands(&queue));
        let mut pass = render_target(&target, Load::Load, Store::Store, (0., 0., 0., 0.));
        pass.depth = Some(DepthAttachment { depth_texture: depth.clone(), depth_load: load, depth_store: Store::Store, depth_clear: clear });
        let encoder = get(be::render_encoder(&commands, &pass));
        get(be::set_viewport(&encoder, full));
        get(be::set_render_pipeline(&encoder, &depth_pipeline));
        get(be::set_depth_state(&encoder, Some(DepthState { depth_compare: Comparison::Less, depth_write: true })));
        get(be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets));
        set_color(&encoder, 1., 1., 0., 1.);
        get(be::draw(&encoder, Primitive::TriangleList, 0, 3, None));
        get(be::end_render(&encoder));
        poll_epoch(&queue, get(be::commit(&commands)));
        expect_pixels(expected, message);
    };
    render_depth(Load::Clear, 1., [0xff, 0xff, 0x00, 0xff], "depth clear-to-far rejected the triangle");
    render_depth(Load::Clear, 0., [0x00, 0x00, 0x00, 0xff], "depth clear-to-near admitted the triangle");
    render_depth(Load::Load, 1., [0x00, 0x00, 0x00, 0xff], "depth load ignored the retained near depth");

    // Multisampling: Store with a resolve texture resolves and keeps the samples, so a following Load
    // pass resolves the same colour; Discard cannot resolve.
    let msaa = get(be::create_texture(
        &device,
        TextureDescriptor {
            label: Some("msaa".to_owned()),
            sample_count: 4,
            usage: vec![TextureUsage::RenderAttachment],
            ..target_descriptor.clone()
        },
    ));
    let msaa_pass = |load: Load, store: Store, clear: (f64, f64, f64, f64)| {
        let commands = get(be::begin_commands(&queue));
        let pass = RenderTarget {
            colors: vec![ColorAttachment { texture: msaa.clone(), resolve: Some(target.clone()), load, store, clear }],
            depth: None,
        };
        let encoder = get(be::render_encoder(&commands, &pass));
        get(be::end_render(&encoder));
        poll_epoch(&queue, get(be::commit(&commands)));
    };
    msaa_pass(Load::Clear, Store::Store, (0.125, 0.25, 0.5, 1.));
    expect_pixels([0x20, 0x40, 0x80, 0xff], "stored multisample pass did not resolve");
    clear_target(0., 0., 0., 1.);
    msaa_pass(Load::Load, Store::Resolve, (0., 0., 0., 0.));
    expect_pixels([0x20, 0x40, 0x80, 0xff], "Store with a resolve texture did not keep the samples");
    let commands = get(be::begin_commands(&queue));
    refused(
        be::render_encoder(
            &commands,
            &RenderTarget {
                colors: vec![ColorAttachment {
                    texture: msaa.clone(),
                    resolve: Some(target.clone()),
                    load: Load::Clear,
                    store: Store::Discard,
                    clear: (0., 0., 0., 1.),
                }],
                depth: None,
            },
        ),
        ErrorKind::InvalidArgument,
        "a discarded attachment was allowed to resolve",
    );
    get(be::abandon(&commands));
    get(be::destroy_texture(&msaa));
    get(be::destroy_texture(&depth));
    get(be::destroy_pipeline(&depth_pipeline));
    get(be::destroy_pipeline(&textured));
    get(be::destroy_argument(&argument));
    get(be::destroy_buffer(&argument_buffer));
    get(be::destroy_buffer(&texel_upload));
    get(be::destroy_texture(&texel));
    get(be::destroy_pipeline(&argument_pipeline));
    get(be::destroy_icb(&icb));
    get(be::destroy_pipeline(&indirect));
    get(be::destroy_buffer(&color));
    get(be::destroy_buffer(&indices));
    get(be::destroy_buffer(&arguments));
    get(be::destroy_pipeline(&flat));
    get(be::destroy_buffer(&offsets));
    get(be::destroy_texture(&target));
    get(be::destroy_sampler(&sampler));
    get(be::destroy_queue(&queue));
    finish(&device);
}

/// VK_ERROR_OUT_OF_DEVICE_MEMORY is a typed Capacity error, never a crash. One buffer of twice the
/// working set cannot fit any heap, so the driver refuses it at once.
#[test]
fn gpu_out_of_device_memory_is_capacity() {
    let driver = create_with_validation((true, true));
    let raw = match (driver.create_device)() {
        Err(failure) if failure.kind == ErrorKind::NoAdapter => return,
        Err(failure) => panic!("{}", error::to_string(&failure)),
        Ok(raw) => raw,
    };
    let size = 2 * (raw.recommended_working_set)();
    match (raw.create_buffer)(Memory::DeviceLocal, &buf("", size, &[BufferUsage::Storage])) {
        Err(failure) if failure.kind == ErrorKind::Capacity => (),
        Err(failure) => panic!("out of device memory is not Capacity: {}", error::to_string(&failure)),
        Ok(resource) => {
            let _ = (resource.destroy)();
            panic!("a buffer of twice the working set was created");
        }
    }
    get((raw.destroy_device)());
    assert_eq!(impl_::live_handles(), 0, "out-of-memory test leaked handles");
    assert_eq!(vulkan::Device::validation_messages(), 0, "out-of-memory test: validation messages");
}
