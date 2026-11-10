//! Real-device Vulkan cases end with zero live handles and fatal validation errors. Shaders compile from the owned SPIR-V source fixtures.
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

fn die(failure: Error) -> ! {
    panic!("{}", error::to_string(&failure))
}

fn require(condition: bool, message: &str) {
    assert!(condition, "{message}");
}

/// The result must be an error of this kind.
fn refused(result: Option<Error>, kind: ErrorKind, message: &str) {
    match result {
        Some(failure) if failure.kind == kind => (),
        Some(failure) => panic!("{message}: got {}", error::to_string(&failure)),
        None => panic!("{message}"),
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
    be::create_buffer(device, buf(label, size, usage), None).unwrap_or_else(|failure| die(failure))
}

fn local_buffer(device: &Device, label: &str, size: i64, usage: &[BufferUsage]) -> Buffer {
    be::create_buffer(device, buf(label, size, usage), Some(Memory::DeviceLocal)).unwrap_or_else(|failure| die(failure))
}

fn float_buffer(device: &Device, label: &str, values: &[f64]) -> Buffer {
    let mut bytes = vec![0u8; 4 * values.len()];
    for (i, v) in values.iter().enumerate() {
        put_f32(&mut bytes, i * 4, *v);
    }
    let buffer = make_buffer(device, label, bytes.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&buffer, 0, &bytes).unwrap_or_else(|failure| die(failure));
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
    shader::create(shader::Descriptor {
        backend: "vulkan".to_owned(),
        label: Some(label.to_owned()),
        bytes,
        entry_points: entries.iter().map(|(name, stage)| EntryPoint { name: (*name).to_owned(), stage: *stage }).collect(),
        bindings,
    }).unwrap_or_else(|failure| die(failure))
}

/// Waits for a submitted epoch by polling it, bounded and without a sleep.
fn poll_epoch(queue: &Queue, receipt: Receipt) {
    for _ in 0..2_000_000 {
        if be::status(queue, receipt).unwrap_or_else(|failure| die(failure)) == Status::Completed {
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
    match driver.create_device() {
        Err(failure) if failure.kind == ErrorKind::NoAdapter => {
            eprintln!("RAYS_TEST_SKIP: OGPU conformance (Vulkan): skipped (no device)");
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
            return None;
        }
        Err(failure) => panic!("{}", error::to_string(&failure)),
        Ok(raw) => raw.destroy_device().unwrap_or_else(|failure| die(failure)),
    }
    Some(be::create_device(driver).unwrap_or_else(|failure| die(failure)))
}

fn finish(device: &Device) {
    be::destroy_device(device).unwrap_or_else(|failure| die(failure));
    assert_eq!(impl_::live_handles(), 0, "Vulkan backend leaked handles");
    assert_eq!(crate::vulkan::Device::validation_messages(), 0, "validation messages");
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
    ogpu_core::impl_::select(impl_::implementation());
    assert_eq!(ogpu_core::impl_::backend().unwrap(), "vulkan");
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), 0);
}

/// Capabilities, buffers, mapped buffers, texture upload and read, pixel formats, LOD sampling, a
/// reusable library with function-constant-free pipelines, indirect dispatch, abandoned commands and
/// independent queues.
#[test]
fn gpu_conformance_transfer_and_compute() {
    let Some(device) = open() else { return };
    let capabilities = be::capabilities(&device);
    caps::validate(&capabilities).unwrap_or_else(|failure| die(failure));
    let profile = capabilities;
    require(caps::has(&profile, &Feature::RayTracing) == capabilities.ray_tracing, "ray-tracing capability mismatch");
    refused(
        caps::require(None, &profile, &Feature::Unknown("future".to_owned())).err(),
        ErrorKind::Unsupported,
        "unknown feature was not typed Unsupported",
    );
    let source = make_buffer(&device, "conformance", 16, &[BufferUsage::CopySrc, BufferUsage::CopyDst]);
    let queue = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
    require(be::completed_epoch(&queue) == 0, "new queue completed an epoch");
    refused(be::poll_through(&queue, 0).err(), ErrorKind::InvalidArgument, "zero completion epoch was accepted");
    let bytes: Vec<u8> = (0..16).map(|i| ((i * 13) & 255) as u8).collect();
    be::write_buffer(&source, 0, &bytes).unwrap_or_else(|failure| die(failure));
    require(be::read_buffer(&source, 0, 16).unwrap_or_else(|failure| die(failure)) == bytes, "buffer round trip differs");

    // a mapped buffer: host view and device copy meet only at flush and invalidate; a view outlives
    // the buffer
    let mapped = be::create_mapped_buffer(
        &device,
        buf("mapped", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]),
    ).unwrap_or_else(|failure| die(failure));
    let floats = be::mapped_view(&mapped).unwrap();
    require(floats.borrow().len() == 4 && floats.borrow()[3] == 0., "mapped view is not 4 zeroed floats");
    require(Rc::ptr_eq(&be::mapped_view(&mapped).unwrap(), &floats), "mapped view accessor copied storage");
    require(be::mapped_view(&source).is_none(), "unmapped buffer exposed a mapped view");
    floats.borrow_mut()[1] = 1.5;
    be::flush_mapped(&mapped, 16).unwrap_or_else(|failure| die(failure));
    require(i32_at(&be::read_buffer(&mapped, 4, 4).unwrap_or_else(|failure| die(failure)), 0) == 1.5f32.to_bits() as i32, "a flushed view write is not in the buffer");
    let mut three = [0u8; 4];
    put_f32(&mut three, 0, 3.);
    be::write_buffer(&mapped, 8, &three).unwrap_or_else(|failure| die(failure));
    be::invalidate_mapped(&mapped, 16).unwrap_or_else(|failure| die(failure));
    require(floats.borrow()[2] == 3. && floats.borrow()[1] == 1.5, "an invalidated view lacks the buffer's contents");
    // released, the host copy of a discrete device goes: a flush has nothing to write, and the next
    // view is new and holds the device copy once invalidated; unified memory keeps its one view
    be::release_mapped_view(&mapped).unwrap_or_else(|failure| die(failure));
    if capabilities.unified_memory {
        require(Rc::ptr_eq(&be::mapped_view(&mapped).unwrap(), &floats), "a unified view changed on release");
    } else {
        refused(be::flush_mapped(&mapped, 16).err(), ErrorKind::InvalidArgument, "a released view was flushed");
        be::invalidate_mapped(&mapped, 16).unwrap_or_else(|failure| die(failure));
        let fresh = be::mapped_view(&mapped).unwrap();
        require(
            !Rc::ptr_eq(&fresh, &floats) && fresh.borrow()[2] == 3. && fresh.borrow()[1] == 1.5,
            "a view made by invalidate lacks the buffer's contents",
        );
    }
    let floats = be::mapped_view(&mapped).unwrap();
    refused(be::release_mapped_view(&source).err(), ErrorKind::InvalidArgument, "an unmapped buffer's view was released");
    // a buffer only the device touches: no view until the host asks, then the device copy once invalidated
    let device_only = be::create_mapped_buffer(
        &device,
        buf("device only", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]),
    ).unwrap_or_else(|failure| die(failure));
    be::write_buffer(&device_only, 8, &three).unwrap_or_else(|failure| die(failure));
    if !capabilities.unified_memory {
        refused(be::flush_mapped(&device_only, 16).err(), ErrorKind::InvalidArgument, "a view nobody asked for was flushed");
    }
    be::invalidate_mapped(&device_only, 16).unwrap_or_else(|failure| die(failure));
    require(be::mapped_view(&device_only).unwrap().borrow()[2] == 3., "a device-only buffer's first view");
    let owned = be::owned_readback(&device_only).unwrap_or_else(|failure| die(failure));
    refused(owned(-1, 4).err(), ErrorKind::InvalidArgument, "negative owned readback accepted");
    refused(owned(12, 8).err(), ErrorKind::InvalidArgument, "oversized owned readback accepted");
    be::destroy_buffer(&device_only).unwrap_or_else(|failure| die(failure));
    // A settled allocation is retained independently of its owner-only wrapper; no owner pump.
    let bytes = std::thread::spawn(move || owned(8, 4).unwrap()).join().unwrap();
    require(bytes == three, "owned readback did not preserve the native allocation");
    refused(be::owned_readback(&device_only).err(), ErrorKind::StaleHandle, "destroyed owned capture accepted");
    refused(be::flush_mapped(&mapped, 20).err(), ErrorKind::InvalidArgument, "a flush past the buffer was accepted");
    refused(be::flush_mapped(&source, 4).err(), ErrorKind::InvalidArgument, "an unmapped buffer was flushed");
    be::destroy_buffer(&mapped).unwrap_or_else(|failure| die(failure));
    require(be::mapped_view(&mapped).is_none(), "destroyed buffer exposed a mapped view");
    require(floats.borrow()[1] == 1.5, "mapped view changed after destroy");
    refused(be::invalidate_mapped(&mapped, 4).err(), ErrorKind::StaleHandle, "a destroyed mapped buffer was invalidated");
    refused(
        be::create_mapped_buffer(&device, buf("", 6, &[BufferUsage::Storage])).err(),
        ErrorKind::InvalidArgument,
        "a mapped buffer of 6 bytes was accepted",
    );

    // Blit encoder: texture upload with padded rows, read back by the host, and the same at an inset origin.
    let blit = |encode: &dyn Fn(&be::BlitEncoder)| -> Receipt {
        let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
        let encoder = be::blit_encoder(&commands).unwrap_or_else(|failure| die(failure));
        encode(&encoder);
        be::end_blit(&encoder).unwrap_or_else(|failure| die(failure));
        let receipt = be::commit(&commands).unwrap_or_else(|failure| die(failure));
        poll_epoch(&queue, receipt);
        receipt
    };
    let upload = make_buffer(&device, "texture-transfer", 512, &[BufferUsage::CopySrc, BufferUsage::CopyDst]);
    let round_trip = texture_descriptor("texture-round-trip", 2, &[TextureUsage::TextureCopySrc, TextureUsage::TextureCopyDst]);
    let first = be::create_texture(&device, round_trip.clone()).unwrap_or_else(|failure| die(failure));
    let mut padded = vec![0u8; 512];
    for i in 0..8usize {
        padded[i] = (i * 17 + 3) as u8;
        padded[256 + i] = (i * 19 + 5) as u8;
    }
    be::write_buffer(&upload, 0, &padded).unwrap_or_else(|failure| die(failure));
    let extent = Extent { width: 2, height: 2, depth: 1 };
    let receipt = blit(&|encoder| be::buffer_to_texture(encoder, &upload, None, 256, 512, &first, None, None, extent).unwrap_or_else(|failure| die(failure)));
    require(be::completed_epoch(&queue) == receipt.epoch, "queue completed epoch differs from receipt");
    require(be::poll_through(&queue, receipt.epoch).unwrap_or_else(|failure| die(failure)), "completed epoch did not remain complete");
    let mut compact = vec![0u8; 16];
    compact[..8].copy_from_slice(&padded[..8]);
    compact[8..].copy_from_slice(&padded[256..264]);
    require(be::read_texture(&first, 8).unwrap_or_else(|failure| die(failure)) == compact, "uploaded texture differs from its padded rows");
    let inset = be::create_texture(&device, TextureDescriptor { width: 4, height: 4, ..round_trip }).unwrap_or_else(|failure| die(failure));
    blit(&|encoder| {
        be::buffer_to_texture(encoder, &upload, None, 256, 512, &inset, None, Some(Origin { x: 1, y: 1, z: 0 }), extent).unwrap_or_else(|failure| die(failure))
    });
    let mut expected_image = vec![0u8; 64];
    expected_image[20..28].copy_from_slice(&padded[..8]);
    expected_image[36..44].copy_from_slice(&padded[256..264]);
    require(be::read_texture(&inset, 16).unwrap_or_else(|failure| die(failure)) == expected_image, "texture subregion upload changed other pixels");

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
        let texture = format_texture(4, 3, &sampled, format).unwrap_or_else(|failure| die(failure));
        for mip in 0..3usize {
            let width = 4usize >> mip;
            let rows = encode_rows(format, width, &|x, y, c| {
                if format == TextureFormat::Rgba8Unorm {
                    ((x * 7) + (y * 31) + (c * 13) + (mip * 50)) as f64
                } else {
                    (x as f64 - (2 * y) as f64 + (3 * c) as f64) * 1.5 + mip as f64 * 100. - 7.
                }
            });
            be::write_buffer(&format_rows, 0, &rows).unwrap_or_else(|failure| die(failure));
            let extent = Extent { width: width as i64, height: width as i64, depth: 1 };
            blit(&|encoder| {
                be::buffer_to_texture(encoder, &format_rows, None, 256, 1024, &texture, Some(mip as i64), None, extent).unwrap_or_else(|failure| die(failure))
            });
            if mip == 0 {
                let row = 4 * types::texel_bytes(format) as usize;
                let mut compact = vec![0u8; 4 * row];
                for y in 0..4 {
                    compact[y * row..(y + 1) * row].copy_from_slice(&rows[y * 256..y * 256 + row]);
                }
                require(be::read_texture(&texture, row as i64).unwrap_or_else(|failure| die(failure)) == compact, "format host read differs");
            }
        }
        be::destroy_texture(&texture).unwrap_or_else(|failure| die(failure));
    }
    refused(
        format_texture(4, 1, &[TextureUsage::TextureBinding, TextureUsage::RenderAttachment], TextureFormat::Rgba16Float).err(),
        ErrorKind::Unsupported,
        "float render attachment was accepted",
    );
    let half = format_texture(4, 3, &sampled, TextureFormat::Rgba16Float).unwrap_or_else(|failure| die(failure));
    let wide = format_texture(32, 1, &sampled, TextureFormat::Rgba32Float).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::blit_encoder(&commands).unwrap_or_else(|failure| die(failure));
    refused(
        be::buffer_to_texture(&encoder, &format_rows, None, 256, 256, &wide, None, None, Extent { width: 32, height: 1, depth: 1 }).err(),
        ErrorKind::InvalidArgument,
        "row pitch below the float texel width was accepted",
    );
    be::end_blit(&encoder).unwrap_or_else(|failure| die(failure));
    be::abandon(&commands).unwrap_or_else(|failure| die(failure));

    // Linear mip sampling of Rgba16_float at LOD 0, 0.5 and 1 from uniform levels: the exact levels and their average.
    let levels: [[f64; 4]; 3] = [[2., -1., 0.5, 4.], [4., -3., 1.5, 8.], [100., 100., 100., 100.]];
    for mip in 0..3usize {
        let width = 4usize >> mip;
        be::write_buffer(&format_rows, 0, &encode_rows(TextureFormat::Rgba16Float, width, &|_, _, c| levels[mip][c])).unwrap_or_else(|failure| die(failure));
        let extent = Extent { width: width as i64, height: width as i64, depth: 1 };
        blit(&|encoder| {
            be::buffer_to_texture(encoder, &format_rows, None, 256, 1024, &half, Some(mip as i64), None, extent).unwrap_or_else(|failure| die(failure))
        });
    }
    let interface = vec![compute_binding(0, BindingKind::SampledTexture), compute_binding(1, BindingKind::StorageBuffer)];
    let lod_shader = make_shader("sample-lod", shader_bytes("sample_lod"), &[("sample_lod", Stage::Compute)], interface.clone());
    let library = be::create_library(&device, &lod_shader).unwrap_or_else(|failure| die(failure));
    let pipeline = be::create_compute_pipeline_from(&library, "sample_lod", None, &interface).unwrap_or_else(|failure| die(failure));
    let out = make_buffer(&device, "sample-lod", 48, &[BufferUsage::Storage, BufferUsage::CopySrc]);
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::compute_encoder(&commands).unwrap_or_else(|failure| die(failure));
    be::set_pipeline(&encoder, &pipeline).unwrap_or_else(|failure| die(failure));
    be::set_texture(&encoder, 0, &half).unwrap_or_else(|failure| die(failure));
    be::set_buffer(&encoder, 1, &out).unwrap_or_else(|failure| die(failure));
    be::dispatch_threads(&encoder, (3, 1, 1), (3, 1, 1)).unwrap_or_else(|failure| die(failure));
    be::end_compute(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    let values = be::read_buffer(&out, 0, 48).unwrap_or_else(|failure| die(failure));
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
    be::destroy_buffer(&out).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&pipeline).unwrap_or_else(|failure| die(failure));
    be::destroy_library(&library).unwrap_or_else(|failure| die(failure));
    for texture in [&half, &wide] {
        be::destroy_texture(texture).unwrap_or_else(|failure| die(failure));
    }
    be::destroy_buffer(&format_rows).unwrap_or_else(|failure| die(failure));

    // Reusable libraries and immediate-mode encoders: one library, two pipelines (SPIR-V has no named
    // function constants: the two variants are two entries), exact output, and status polling.
    let scale = |triple: bool| (if triple { "scale3" } else { "scale2" }).to_owned();
    let library_shader = make_shader(
        "conformance-library",
        shader_bytes("library"),
        &[("fill", Stage::Compute), ("scale2", Stage::Compute), ("scale3", Stage::Compute)],
        vec![],
    );
    let library = be::create_library(&device, &library_shader).unwrap_or_else(|failure| die(failure));
    let scale_interface = vec![compute_binding(0, BindingKind::StorageBuffer), compute_binding(1, BindingKind::UniformBuffer)];
    let words = make_buffer(&device, "encoded-words", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]);
    let local = local_buffer(&device, "device-local", 16, &[BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst]);
    refused(be::read_buffer(&local, 0, 16).err(), ErrorKind::Unsupported, "device-local buffer was host readable");
    refused(be::write_buffer(&local, 0, &[0u8; 16]).err(), ErrorKind::Unsupported, "device-local buffer was host writable");
    let mut bias = [0u8; 4];
    put_i32(&mut bias, 0, 5);
    let triple = be::create_compute_pipeline_from(&library, &scale(true), None, &scale_interface).unwrap_or_else(|failure| die(failure));
    let double = be::create_compute_pipeline_from(&library, &scale(false), None, &scale_interface).unwrap_or_else(|failure| die(failure));
    assert!(be::max_total_threads(&double) >= 32);
    refused(
        be::create_compute_pipeline_from(&library, &scale(true), None, &scale_interface[..1]).err(),
        ErrorKind::InvalidArgument,
        "mismatched library interface was accepted",
    );
    require(be::create_compute_pipeline_from(&library, "missing", None, &[]).is_err(), "missing library entry was accepted");
    refused(be::destroy_library(&library).err(), ErrorKind::InvalidState, "library with live pipelines was destroyed");
    let run = |pipeline: &Pipeline, factor: i32| {
        let mut input = [0u8; 16];
        for i in 0..4 {
            put_i32(&mut input, i * 4, i as i32);
        }
        be::write_buffer(&words, 0, &input).unwrap_or_else(|failure| die(failure));
        let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
        let compute = be::compute_encoder(&commands).unwrap_or_else(|failure| die(failure));
        refused(
            be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)).err(),
            ErrorKind::InvalidState,
            "dispatch without a pipeline was accepted",
        );
        be::set_pipeline(&compute, pipeline).unwrap_or_else(|failure| die(failure));
        be::set_buffer(&compute, 0, &words).unwrap_or_else(|failure| die(failure));
        be::set_bytes(&compute, 1, &bias).unwrap_or_else(|failure| die(failure));
        refused(be::blit_encoder(&commands).err(), ErrorKind::InvalidState, "second encoder opened while one was recording");
        be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)).unwrap_or_else(|failure| die(failure));
        be::end_compute(&compute).unwrap_or_else(|failure| die(failure));
        let receipt = be::commit(&commands).unwrap_or_else(|failure| die(failure));
        refused(be::commit(&commands).err(), ErrorKind::InvalidState, "commands were committed twice");
        poll_epoch(&queue, receipt);
        let output = be::read_buffer(&words, 0, 16).unwrap_or_else(|failure| die(failure));
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
    be::write_buffer(&groups, 0, &args).unwrap_or_else(|failure| die(failure));
    be::write_buffer(&words, 0, &[0u8; 16]).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let compute = be::compute_encoder(&commands).unwrap_or_else(|failure| die(failure));
    be::set_pipeline(&compute, &double).unwrap_or_else(|failure| die(failure));
    be::set_buffer(&compute, 0, &words).unwrap_or_else(|failure| die(failure));
    be::set_bytes(&compute, 1, &bias).unwrap_or_else(|failure| die(failure));
    for offset in [8, 2, -4] {
        refused(
            be::dispatch_threadgroups_indirect(&compute, &groups, offset, (2, 1, 1)).err(),
            ErrorKind::InvalidArgument,
            "invalid indirect arguments range was accepted",
        );
    }
    be::dispatch_threadgroups_indirect(&compute, &groups, 0, (2, 1, 1)).unwrap_or_else(|failure| die(failure));
    be::end_compute(&compute).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    let output = be::read_buffer(&words, 0, 16).unwrap_or_else(|failure| die(failure));
    for (i, expected) in [5, 5, 0, 0].iter().enumerate() {
        require(i32_at(&output, i * 4) == *expected, "indirect dispatch ran a group count other than its buffer's");
    }
    be::destroy_buffer(&groups).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&triple).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&double).unwrap_or_else(|failure| die(failure));

    // Abandoned commands never execute and cannot be committed.
    be::write_buffer(&upload, 0, &[255u8; 512]).unwrap_or_else(|failure| die(failure));
    let abandoned = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let blit_encoder = be::blit_encoder(&abandoned).unwrap_or_else(|failure| die(failure));
    be::buffer_to_texture(&blit_encoder, &upload, None, 256, 512, &first, None, None, extent).unwrap_or_else(|failure| die(failure));
    be::abandon(&abandoned).unwrap_or_else(|failure| die(failure));
    refused(be::commit(&abandoned).err(), ErrorKind::InvalidState, "abandoned commands were committed");
    require(be::read_texture(&first, 8).unwrap_or_else(|failure| die(failure)) == compact, "abandoned commands executed");
    be::destroy_library(&library).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&words).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&local).unwrap_or_else(|failure| die(failure));

    // A second queue's epochs are its own.
    let other_queue = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
    require(be::completed_epoch(&other_queue) == 0, "new queue inherited another queue's completion");
    let other_commands = be::begin_commands(&other_queue).unwrap_or_else(|failure| die(failure));
    let other_blit = be::blit_encoder(&other_commands).unwrap_or_else(|failure| die(failure));
    be::buffer_to_texture(&other_blit, &upload, None, 256, 512, &first, None, None, extent).unwrap_or_else(|failure| die(failure));
    be::end_blit(&other_blit).unwrap_or_else(|failure| die(failure));
    let other = be::commit(&other_commands).unwrap_or_else(|failure| die(failure));
    require(other.epoch == 1, "queue epochs are not independent");
    poll_epoch(&other_queue, other);
    be::destroy_buffer(&source).unwrap_or_else(|failure| die(failure));
    refused(be::read_buffer(&source, 0, 1).err(), ErrorKind::StaleHandle, "destroyed buffer remained readable");
    be::destroy_buffer(&upload).unwrap_or_else(|failure| die(failure));
    be::destroy_texture(&first).unwrap_or_else(|failure| die(failure));
    be::destroy_texture(&inset).unwrap_or_else(|failure| die(failure));
    be::destroy_queue(&other_queue).unwrap_or_else(|failure| die(failure));
    be::destroy_queue(&queue).unwrap_or_else(|failure| die(failure));
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
        let bytes = be::read_buffer(&self.hits, 0, 96).unwrap_or_else(|failure| die(failure));
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
        let commands = be::begin_commands(&self.queue).unwrap_or_else(|failure| die(failure));
        let compute = be::compute_encoder(&commands).unwrap_or_else(|failure| die(failure));
        be::set_pipeline(&compute, pipeline).unwrap_or_else(|failure| die(failure));
        be::set_accel(&compute, 0, structure).unwrap_or_else(|failure| die(failure));
        be::set_buffer(&compute, 1, &self.rays).unwrap_or_else(|failure| die(failure));
        be::set_buffer(&compute, 2, &self.hits).unwrap_or_else(|failure| die(failure));
        if let Some(bytes) = &extras.bytes {
            be::set_bytes(&compute, 3, bytes).unwrap_or_else(|failure| die(failure));
        }
        for (index, buffer) in [(3, &extras.buffer3), (4, &extras.buffer4), (5, &extras.buffer5)] {
            if let Some(buffer) = buffer {
                be::set_buffer(&compute, index, buffer).unwrap_or_else(|failure| die(failure));
            }
        }
        if let Some(accel) = &extras.accel6 {
            be::set_accel(&compute, 6, accel).unwrap_or_else(|failure| die(failure));
        }
        be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)).unwrap_or_else(|failure| die(failure));
        be::end_compute(&compute).unwrap_or_else(|failure| die(failure));
        poll_epoch(&self.queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
        self.read_hits()
    }

    fn build_one(&self, structure: &Accel, scratch: &Buffer) {
        let commands = be::begin_commands(&self.queue).unwrap_or_else(|failure| die(failure));
        let encoder = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
        be::build_accel(&encoder, structure, scratch, None).unwrap_or_else(|failure| die(failure));
        be::end_accel(&encoder).unwrap_or_else(|failure| die(failure));
        poll_epoch(&self.queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
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
    let queue = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
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
    be::write_buffer(&vertices, 0, &vertex_bytes(0.)).unwrap_or_else(|failure| die(failure));
    // Rays from z=2 toward -z at x=0, x=4, x=8 (miss), x=12 (miss).
    let mut ray_bytes = vec![0u8; 128];
    for (i, x) in [0., 4., 8., 12.].iter().enumerate() {
        for (k, v) in [*x, 0., 2., 0., 0., 0., -1., 0.].iter().enumerate() {
            put_f32(&mut ray_bytes, (i * 8 + k) * 4, *v);
        }
    }
    let rays = make_buffer(&device, "rays", 128, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&rays, 0, &ray_bytes).unwrap_or_else(|failure| die(failure));
    let hits = make_buffer(&device, "hits", 96, &[BufferUsage::Storage, BufferUsage::CopySrc]);
    let rt = Rays { queue: be::create_queue(&device).unwrap_or_else(|failure| die(failure)), rays, hits };
    let blas_descriptor = triangles(&vertices, 72, 6);

    let ray_library = be::create_library(&device, &ray_shader).unwrap_or_else(|failure| die(failure));
    let trace = be::create_compute_pipeline_from(&ray_library, "trace", None, &ray_interface).unwrap_or_else(|failure| die(failure));
    let trace_instances = be::create_compute_pipeline_from(&ray_library, "trace_instances", None, &ray_interface).unwrap_or_else(|failure| die(failure));
    let blas = be::create_accel(&device, blas_descriptor.clone(), false).unwrap_or_else(|failure| die(failure));
    let sizes = be::accel_sizes(&blas);
    require(sizes.structure_size > 0 && sizes.build_scratch_size >= 0, "BLAS sizes are empty");
    let scratch = local_buffer(&device, "scratch", 256.max(2 * sizes.build_scratch_size), &[BufferUsage::Storage]);
    let instances = be::pack_instance_records(&device, &[instance(&IDENTITY, 0xFFFF_FFFF, 0, 0), instance(&SHIFTED, 0xFFFF_FFFF, 0, 1)]).unwrap_or_else(|failure| die(failure));
    let stride = instances.len() / 2;
    let instance_buffer = make_buffer(&device, "instances", 2 * stride as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&instance_buffer, 0, &instances).unwrap_or_else(|failure| die(failure));
    let tlas_of = |buffer: &Buffer, count: i64, structures: Vec<Accel>| AccelDescriptor::TlasOf {
        instances: buffer.clone(),
        offset: 0,
        instance_count: count,
        kind: InstanceKind::UserIdInstances,
        structures,
        motion_transforms: None,
    };
    let tlas = be::create_accel(&device, tlas_of(&instance_buffer, 2, vec![blas.clone()]), false).unwrap_or_else(|failure| die(failure));
    refused(
        be::create_accel(&device, tlas_of(&instance_buffer, 3, vec![blas.clone()]), false).err(),
        ErrorKind::InvalidArgument,
        "oversized TLAS instance count was accepted",
    );
    let tlas_scratch = local_buffer(&device, "tlas-scratch", 256.max(be::accel_sizes(&tlas).build_scratch_size), &[BufferUsage::Storage]);
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let compute = be::compute_encoder(&commands).unwrap_or_else(|failure| die(failure));
    be::set_pipeline(&compute, &trace).unwrap_or_else(|failure| die(failure));
    refused(be::set_accel(&compute, 0, &blas).err(), ErrorKind::InvalidState, "unbuilt structure was bound");
    refused(be::compute_use_accels(&compute, &[blas.clone()]).err(), ErrorKind::InvalidState, "unbuilt structure was declared used");
    be::compute_use_accels(&compute, &[]).unwrap_or_else(|failure| die(failure));
    be::end_compute(&compute).unwrap_or_else(|failure| die(failure));
    let build = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
    refused(be::build_accel(&build, &tlas, &tlas_scratch, None).err(), ErrorKind::InvalidState, "TLAS built before its BLAS");
    be::build_accel(&build, &blas, &scratch, None).unwrap_or_else(|failure| die(failure));
    refused(be::build_accel(&build, &blas, &scratch, None).err(), ErrorKind::InvalidState, "double build was accepted");
    be::build_accel(&build, &tlas, &tlas_scratch, None).unwrap_or_else(|failure| die(failure));
    be::end_accel(&build).unwrap_or_else(|failure| die(failure));
    let query_in = |commands: &be::Commands, pipeline: &Pipeline, structure: &Accel| {
        let compute = be::compute_encoder(commands).unwrap_or_else(|failure| die(failure));
        be::set_pipeline(&compute, pipeline).unwrap_or_else(|failure| die(failure));
        be::set_accel(&compute, 0, structure).unwrap_or_else(|failure| die(failure));
        be::compute_use_accels(&compute, &[blas.clone()]).unwrap_or_else(|failure| die(failure));
        be::set_buffer(&compute, 1, &rt.rays).unwrap_or_else(|failure| die(failure));
        be::set_buffer(&compute, 2, &rt.hits).unwrap_or_else(|failure| die(failure));
        be::dispatch_threads(&compute, (4, 1, 1), (4, 1, 1)).unwrap_or_else(|failure| die(failure));
        be::end_compute(&compute).unwrap_or_else(|failure| die(failure));
    };
    query_in(&commands, &trace, &blas);
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    check_hits(
        "primitive triangle",
        &rt.read_hits(),
        &[triangle_hit(0, 0, 2.), triangle_hit(1, 0, 2.), NO_HIT, NO_HIT],
    );
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    query_in(&commands, &trace_instances, &tlas);
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    check_hits(
        "triangle instances",
        &rt.read_hits(),
        &[triangle_hit(0, 0, 2.), triangle_hit(1, 0, 2.), triangle_hit(0, 1, 2.), triangle_hit(1, 1, 2.)],
    );

    // Refit: a refittable structure updated in place from new vertex data of the same shape (the
    // triangles moved to z = 1, so the rays from z = 2 hit at t = 1).
    let refittable = be::create_accel(&device, blas_descriptor.clone(), true).unwrap_or_else(|failure| die(failure));
    let moved = make_buffer(&device, "ray-vertices-moved", 72, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&moved, 0, &vertex_bytes(1.)).unwrap_or_else(|failure| die(failure));
    let moved_descriptor = triangles(&moved, 72, 6);
    let refit_scratch = local_buffer(
        &device,
        "refit-scratch",
        256.max(be::accel_sizes(&refittable).build_scratch_size.max(be::accel_sizes(&refittable).refit_scratch_size)),
        &[BufferUsage::Storage],
    );
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
    refused(
        be::refit_accel(&encoder, &refittable, moved_descriptor.clone(), &refit_scratch, None).err(),
        ErrorKind::InvalidState,
        "an unbuilt structure was refit",
    );
    be::build_accel(&encoder, &refittable, &refit_scratch, None).unwrap_or_else(|failure| die(failure));
    refused(
        be::refit_accel(&encoder, &blas, moved_descriptor.clone(), &refit_scratch, None).err(),
        ErrorKind::InvalidState,
        "a structure built without refit was refit",
    );
    refused(
        be::refit_accel(&encoder, &refittable, triangles(&moved, 36, 3), &refit_scratch, None).err(),
        ErrorKind::InvalidArgument,
        "a refit of another shape was accepted",
    );
    be::refit_accel(&encoder, &refittable, moved_descriptor, &refit_scratch, None).unwrap_or_else(|failure| die(failure));
    be::end_accel(&encoder).unwrap_or_else(|failure| die(failure));
    query_in(&commands, &trace, &refittable);
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    check_hits("refit triangles", &rt.read_hits(), &[triangle_hit(0, 0, 1.), triangle_hit(1, 0, 1.), NO_HIT, NO_HIT]);
    be::destroy_accel(&refittable).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&refit_scratch).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&moved).unwrap_or_else(|failure| die(failure));

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
    let extra = be::create_library(&device, &extra_shader).unwrap_or_else(|failure| die(failure));
    let with = |extra_bindings: &[Binding]| -> Vec<Binding> {
        let mut all = ray_interface.clone();
        all.extend_from_slice(extra_bindings);
        all
    };
    let boxes_pipeline = be::create_compute_pipeline_from(&extra, "trace_boxes", None, &with(&[compute_binding(3, BindingKind::StorageBuffer)])).unwrap_or_else(|failure| die(failure));
    let capsules_pipeline = be::create_compute_pipeline_from(&extra, "trace_capsules", None, &with(&[compute_binding(3, BindingKind::StorageBuffer)])).unwrap_or_else(|failure| die(failure));
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
    let box_blas = be::create_accel(&device, box_geometry(&boxes, 2), false).unwrap_or_else(|failure| die(failure));
    let capsule_boxes = float_buffer(&device, "capsule-boxes", &[7.5, -1.5, -0.5, 8.5, 1.5, 0.5]);
    let capsule_data = float_buffer(&device, "capsules", &[8., -1., 0., 0.5, 8., 1., 0., 0.5]);
    let capsule_blas = be::create_accel(&device, box_geometry(&capsule_boxes, 1), false).unwrap_or_else(|failure| die(failure));
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
    be::write_buffer(&indices, 0, &[0u8; 4]).unwrap_or_else(|failure| die(failure));
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
        be::create_accel(&device, curve_descriptor, false).err(),
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
    be::write_buffer(&vertices0, 0, &vertex_bytes(0.)).unwrap_or_else(|failure| die(failure));
    be::write_buffer(&vertices1, 0, &vertex_bytes(1.5)).unwrap_or_else(|failure| die(failure));
    let motion = Motion { keyframes: 2, start_time: 0., end_time: 1., start_border: Border::Clamp, end_border: Border::Clamp };
    let motion_triangles = |keyframes: Vec<Keyframe>| AccelDescriptor::MotionBlas {
        geometries: vec![Geometry::MotionTriangles { keyframes, vertex_stride: 12, vertex_count: 6 }],
        motion,
    };
    refused(
        be::create_accel(&device, motion_triangles(vec![Keyframe { buffer: vertices0.clone(), offset: 0 }]), false).err(),
        ErrorKind::InvalidArgument,
        "keyframe count mismatch was accepted",
    );
    let motion_blas = be::create_accel(
        &device,
        motion_triangles(vec![Keyframe { buffer: vertices0.clone(), offset: 0 }, Keyframe { buffer: vertices1.clone(), offset: 0 }]),
        false,
    ).unwrap_or_else(|failure| die(failure));
    rt.build_one(&motion_blas, &big_scratch);
    let motion_pipeline = be::create_compute_pipeline_from(
        &extra,
        "trace_motion",
        None,
        &with(&[compute_binding(3, BindingKind::UniformBuffer), motion_bindings[0].clone(), motion_bindings[1].clone()]),
    ).unwrap_or_else(|failure| die(failure));
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
    let records = be::pack_instance_records(&device, &[instance(&IDENTITY, 1, 0, 7), instance(&SHIFTED, 2, 0, 9)]).unwrap_or_else(|failure| die(failure));
    require(records.len() == 2 * 68, "user-id record stride differs");
    let record_buffer = make_buffer(&device, "user-id-instances", records.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&record_buffer, 0, &records).unwrap_or_else(|failure| die(failure));
    let masked_tlas = be::create_accel(&device, tlas_of(&record_buffer, 2, vec![blas.clone()]), false).unwrap_or_else(|failure| die(failure));
    rt.build_one(&masked_tlas, &big_scratch);
    let masked_pipeline = be::create_compute_pipeline_from(
        &extra,
        "trace_masked",
        None,
        &with(&[compute_binding(3, BindingKind::UniformBuffer)]),
    ).unwrap_or_else(|failure| die(failure));
    let mask_bytes = |mask: i32| mask.to_le_bytes().to_vec();
    let masked = rt.query(&masked_pipeline, &masked_tlas, &Extras { bytes: Some(mask_bytes(1)), ..Extras::default() });
    check_hits("mask 1 instances", &masked, &[triangle_hit(0, 7, 2.), triangle_hit(1, 7, 2.), NO_HIT, NO_HIT]);
    let masked = rt.query(&masked_pipeline, &masked_tlas, &Extras { bytes: Some(mask_bytes(2)), ..Extras::default() });
    check_hits("mask 2 instances", &masked, &[NO_HIT, NO_HIT, triangle_hit(0, 9, 2.), triangle_hit(1, 9, 2.)]);

    // The off-ray record 0 and tested record 1 distinguish InstanceId=1, user_id=5, and prototype
    // structure_index=0. Record 1 moves from x=0 to x=8.
    let keyframes = be::pack_transforms(&[FAR.to_vec(), FAR.to_vec(), IDENTITY.to_vec(), SHIFTED.to_vec()]).unwrap_or_else(|failure| die(failure));
    let keyframe_buffer = make_buffer(&device, "instance-keyframes", keyframes.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&keyframe_buffer, 0, &keyframes).unwrap_or_else(|failure| die(failure));
    let prototype_vertices = make_buffer(&device, "motion-instance-prototype", 72, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&prototype_vertices, 0, &vertex_bytes(1.5)).unwrap_or_else(|failure| die(failure));
    let prototype_blas = be::create_accel(&device, triangles(&prototype_vertices, 72, 6), false).unwrap_or_else(|failure| die(failure));
    rt.build_one(&prototype_blas, &big_scratch);
    // Compact before making either TLAS, then release the original: motion proxy metadata must
    // belong to the compacted prototype, not to its template.
    let prototype_size = make_buffer(&device, "prototype-compacted-size", 8, &[BufferUsage::Storage, BufferUsage::CopySrc]);
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
    be::write_compacted_size(&encoder, &prototype_blas, &prototype_size, None).unwrap_or_else(|failure| die(failure));
    be::end_accel(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    let size_bytes = be::read_buffer(&prototype_size, 0, 8).unwrap_or_else(|failure| die(failure));
    let size = i64::from_le_bytes(size_bytes[..8].try_into().unwrap());
    let compacted_prototype =
        be::create_accel(&device, AccelDescriptor::Sized { size, template: prototype_blas.clone() }, false).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
    be::compact_accel(&encoder, &prototype_blas, &compacted_prototype).unwrap_or_else(|failure| die(failure));
    be::end_accel(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    be::destroy_accel(&prototype_blas).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&prototype_size).unwrap_or_else(|failure| die(failure));
    let prototype_blas = compacted_prototype;
    let prototype_records = be::pack_instance_records(&device, &[instance(&IDENTITY, 1, 0, 0), instance(&IDENTITY, 1, 1, 1)]).unwrap_or_else(|failure| die(failure));
    let prototype_record_buffer =
        make_buffer(&device, "motion-prototypes", prototype_records.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&prototype_record_buffer, 0, &prototype_records).unwrap_or_else(|failure| die(failure));
    let prototype_tlas = be::create_accel(&device, tlas_of(&prototype_record_buffer, 2, vec![blas.clone(), prototype_blas.clone()]), false).unwrap_or_else(|failure| die(failure));
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
    let motion_records = be::pack_motion_instances(&device, &[motion_instance(&FAR, 0, 3, 0), motion_instance(&IDENTITY, 1, 5, 2)]).unwrap_or_else(|failure| die(failure));
    require(motion_records.len() == 2 * 44, "motion instance record stride differs");
    let motion_record_buffer =
        make_buffer(&device, "motion-instances", motion_records.len() as i64, &[BufferUsage::Storage, BufferUsage::CopyDst]);
    be::write_buffer(&motion_record_buffer, 0, &motion_records).unwrap_or_else(|failure| die(failure));
    let motion_tlas_of = |transforms: Option<(Buffer, i64, i64)>| AccelDescriptor::TlasOf {
        instances: motion_record_buffer.clone(),
        offset: 0,
        instance_count: 2,
        kind: InstanceKind::MotionInstances,
        structures: vec![blas.clone(), prototype_blas.clone()],
        motion_transforms: transforms,
    };
    refused(
        be::create_accel(&device, motion_tlas_of(None), false).err(),
        ErrorKind::InvalidArgument,
        "motion instances without keyframe transforms were accepted",
    );
    let motion_tlas = be::create_accel(&device, motion_tlas_of(Some((keyframe_buffer.clone(), 0, 4))), false).unwrap_or_else(|failure| die(failure));
    rt.build_one(&motion_tlas, &big_scratch);
    let motion_instances_pipeline = be::create_compute_pipeline_from(
        &extra,
        "trace_motion_instances",
        None,
        &with(&[
            compute_binding(3, BindingKind::UniformBuffer),
            motion_bindings[0].clone(),
            motion_bindings[1].clone(),
            compute_binding(6, BindingKind::AccelerationStructure),
        ]),
    ).unwrap_or_else(|failure| die(failure));
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
    be::write_buffer(&size_buffer,0,&[0xa5;8]).unwrap_or_else(|failure|die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
    refused(
        be::write_compacted_size(&encoder, &blas, &size_buffer, Some(4)).err(),
        ErrorKind::InvalidArgument,
        "unaligned compacted size offset was accepted",
    );
    be::write_compacted_size(&encoder, &blas, &size_buffer, None).unwrap_or_else(|failure| die(failure));
    be::end_accel(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    let size_bytes = be::read_buffer(&size_buffer, 0, 8).unwrap_or_else(|failure| die(failure));
    let compact_size = i64::from_le_bytes(size_bytes[..8].try_into().unwrap());
    require(compact_size > 0 && compact_size <= sizes.structure_size, "compacted size is out of range");
    let compacted = be::create_accel(&device, AccelDescriptor::Sized { size: compact_size, template: blas.clone() }, false).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::accel_encoder(&commands).unwrap_or_else(|failure| die(failure));
    refused(
        be::build_accel(&encoder, &compacted, &big_scratch, None).err(),
        ErrorKind::InvalidState,
        "sized structure accepted a build",
    );
    be::compact_accel(&encoder, &blas, &compacted).unwrap_or_else(|failure| die(failure));
    be::end_accel(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    check_hits(
        "compacted triangles",
        &rt.query(&trace, &compacted, &Extras::default()),
        &[triangle_hit(0, 0, 2.), triangle_hit(1, 0, 2.), NO_HIT, NO_HIT],
    );

    for pipeline in [&motion_instances_pipeline, &motion_pipeline, &boxes_pipeline, &capsules_pipeline, &masked_pipeline] {
        be::destroy_pipeline(pipeline).unwrap_or_else(|failure| die(failure));
    }
    for accel in [&motion_tlas, &prototype_tlas, &prototype_blas, &motion_blas, &compacted, &masked_tlas, &capsule_blas, &box_blas] {
        be::destroy_accel(accel).unwrap_or_else(|failure| die(failure));
    }
    for buffer in [
        &motion_record_buffer, &keyframe_buffer, &prototype_record_buffer, &prototype_vertices, &vertices1, &vertices0, &size_buffer,
        &record_buffer, &indices, &radii, &control, &capsule_data, &capsule_boxes, &big_scratch, &boxes, &spheres,
    ] {
        be::destroy_buffer(buffer).unwrap_or_else(|failure| die(failure));
    }
    be::destroy_library(&extra).unwrap_or_else(|failure| die(failure));
    be::destroy_accel(&blas).unwrap_or_else(|failure| die(failure));
    be::destroy_accel(&tlas).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&instance_buffer).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&tlas_scratch).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&scratch).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&trace).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&trace_instances).unwrap_or_else(|failure| die(failure));
    be::destroy_library(&ray_library).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&vertices).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&rt.rays).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&rt.hits).unwrap_or_else(|failure| die(failure));
    be::destroy_queue(&rt.queue).unwrap_or_else(|failure| die(failure));
    be::destroy_queue(&queue).unwrap_or_else(|failure| die(failure));
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
    let queue = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
    let sampler = be::create_sampler(
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
    ).unwrap_or_else(|failure| die(failure));
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
        let group = binding::create_layout(entries).unwrap_or_else(|failure| die(failure));
        binding::create_pipeline_layout(be::device_handle(&device), &capabilities, &[(0, group)]).unwrap_or_else(|failure| die(failure))
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
    let target = be::create_texture(&device, target_descriptor.clone()).unwrap_or_else(|failure| die(failure));
    let transient = TextureDescriptor { label: Some("pass".to_owned()), usage: vec![TextureUsage::RenderAttachment], ..target_descriptor.clone() };
    require(!profile.memoryless_attachments, "Vulkan reports memoryless attachments");
    refused(
        be::create_memoryless_texture(&device, true, transient).err(),
        ErrorKind::Unsupported,
        "unavailable memoryless attachments were not typed Unsupported",
    );
    refused(
        be::create_buffer(&device, buf("pass", 16, &[BufferUsage::Storage]), Some(Memory::Memoryless)).err(),
        ErrorKind::InvalidArgument,
        "a memoryless buffer was accepted",
    );
    let offsets = make_buffer(&device, "offsets", 8, &[BufferUsage::Storage]);
    be::write_buffer(&offsets, 0, &[0u8; 8]).unwrap_or_else(|failure| die(failure));
    let expect_pixels = |expected: [u8; 4], message: &str| {
        let pixels = be::read_texture(&target, 16).unwrap_or_else(|failure| die(failure));
        let pixel = |x: usize, y: usize| pixels[(y * 4 + x) * 4..(y * 4 + x) * 4 + 4].to_vec();
        require(
            pixel(0, 0) == expected && pixel(3, 3) == expected,
            &format!("{message} (pixels (0,0) {:02x?} and (3,3) {:02x?}, expected {expected:02x?})", pixel(0, 0), pixel(3, 3)),
        );
    };
    let clear_target = |r: f64, g: f64, b: f64, a: f64| {
        let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
        let encoder = be::render_encoder(&commands, &render_target(&target, Load::Clear, Store::Store, (r, g, b, a))).unwrap_or_else(|failure| die(failure));
        be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
        poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    };
    let load_pass = |commands: &be::Commands| {
        be::render_encoder(commands, &render_target(&target, Load::Load, Store::Store, (0., 0., 0., 0.))).unwrap_or_else(|failure| die(failure))
    };
    let full = Rect { x: 0, y: 0, width: 4, height: 4 };
    require(caps::has(&profile, &Feature::RenderPipeline), "this device cannot render");

    let flat = be::create_render_pipeline(&device, &render_descriptor(&flat_layout, &fragment_flat, "flat_fragment"), None, None, None).unwrap_or_else(|failure| die(failure));
    clear_target(0.125, 0.25, 0.5, 1.);
    expect_pixels([0x20, 0x40, 0x80, 0xff], "clear-only render pass pixels differ");
    // One full-screen triangle through the immediate encoder with a fragment uniform buffer.
    let color = make_buffer(&device, "color", 16, &[BufferUsage::Uniform]);
    let set_color = |encoder: &be::RenderEncoder, r: f64, g: f64, b: f64, a: f64| {
        be::write_buffer(&color, 0, &color_bytes(r, g, b, a)).unwrap_or_else(|failure| die(failure));
        be::set_stage_buffer(encoder, ShaderStage::Fragment, 1, None, &color).unwrap_or_else(|failure| die(failure));
    };
    // Preserve the native line-strip topology when sharing core primitive types.
    let lines = be::create_render_pipeline(&device, &render_descriptor(&flat_layout, &fragment_flat, "flat_fragment"), None, Some(Primitive::LineStrip), None).unwrap_or_else(|failure| die(failure));
    clear_target(0., 0., 0., 1.);
    let mut line_offset = [0u8; 8];
    put_f32(&mut line_offset, 0, -1.);
    put_f32(&mut line_offset, 4, -1.);
    be::write_buffer(&offsets, 0, &line_offset).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = load_pass(&commands);
    be::set_render_pipeline(&encoder, &lines).unwrap_or_else(|failure| die(failure));
    be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets).unwrap_or_else(|failure| die(failure));
    set_color(&encoder, 1., 0., 0., 1.);
    be::draw(&encoder, Primitive::LineStrip, 0, 3, None).unwrap_or_else(|failure| die(failure));
    be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    let pixels = be::read_texture(&target, 16).unwrap_or_else(|failure| die(failure));
    require(pixels.chunks_exact(4).any(|pixel| pixel == [255, 0, 0, 255]), "line strip produced no pixels");
    be::destroy_pipeline(&lines).unwrap_or_else(|failure| die(failure));
    be::write_buffer(&offsets, 0, &[0u8; 8]).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = load_pass(&commands);
    refused(
        be::draw(&encoder, Primitive::TriangleList, 0, 3, None).err(),
        ErrorKind::InvalidState,
        "draw without a render pipeline was accepted",
    );
    be::set_render_pipeline(&encoder, &flat).unwrap_or_else(|failure| die(failure));
    be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets).unwrap_or_else(|failure| die(failure));
    set_color(&encoder, 1., 0., 0.5, 1.);
    // A viewport may hang over the target; an empty one is refused.
    refused(
        be::set_viewport(&encoder, Rect { x: 0, y: 0, width: 0, height: 4 }).err(),
        ErrorKind::InvalidArgument,
        "empty viewport was accepted",
    );
    be::set_viewport(&encoder, Rect { x: -2, y: -2, width: 8, height: 8 }).unwrap_or_else(|failure| die(failure));
    be::set_viewport(&encoder, full).unwrap_or_else(|failure| die(failure));
    be::set_scissor(&encoder, full).unwrap_or_else(|failure| die(failure));
    be::set_cull(&encoder, Cull::CullNone).unwrap_or_else(|failure| die(failure));
    be::draw(&encoder, Primitive::TriangleList, 0, 3, None).unwrap_or_else(|failure| die(failure));
    be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    expect_pixels([0xff, 0x00, 0x80, 0xff], "immediate triangle pixels differ");

    // The same triangle as one indexed batch draw.
    let indices = make_buffer(&device, "indices", 12, &[BufferUsage::Index]);
    let mut index_bytes = [0u8; 12];
    for (i, v) in [0, 1, 2].iter().enumerate() {
        put_i32(&mut index_bytes, i * 4, *v);
    }
    be::write_buffer(&indices, 0, &index_bytes).unwrap_or_else(|failure| die(failure));
    be::write_buffer(&color, 0, &color_bytes(0., 1., 0., 1.)).unwrap_or_else(|failure| die(failure));
    clear_target(0., 0., 0., 1.);
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = load_pass(&commands);
    be::set_viewport(&encoder, full).unwrap_or_else(|failure| die(failure));
    be::draw_batch(
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
    ).unwrap_or_else(|failure| die(failure));
    be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    expect_pixels([0x00, 0xff, 0x00, 0xff], "batched indexed triangle pixels differ");

    // The same indexed triangle, its counts read from a buffer when the draw runs (index count 3, one
    // instance, at offset 4); a misaligned argument range is refused.
    let arguments = make_buffer(&device, "draw arguments", 24, &[BufferUsage::Storage]);
    let mut argument_bytes = [0u8; 24];
    for (i, v) in [0, 3, 1, 0, 0, 0].iter().enumerate() {
        put_i32(&mut argument_bytes, i * 4, *v);
    }
    be::write_buffer(&arguments, 0, &argument_bytes).unwrap_or_else(|failure| die(failure));
    be::write_buffer(&color, 0, &color_bytes(1., 1., 0., 1.)).unwrap_or_else(|failure| die(failure));
    clear_target(0., 0., 0., 1.);
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = load_pass(&commands);
    be::set_viewport(&encoder, full).unwrap_or_else(|failure| die(failure));
    be::set_render_pipeline(&encoder, &flat).unwrap_or_else(|failure| die(failure));
    be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets).unwrap_or_else(|failure| die(failure));
    be::set_stage_buffer(&encoder, ShaderStage::Fragment, 1, None, &color).unwrap_or_else(|failure| die(failure));
    refused(
        be::draw_indexed_indirect(&encoder, Primitive::TriangleList, IndexType::Uint32, &indices, 0, &arguments, 6).err(),
        ErrorKind::InvalidArgument,
        "misaligned indirect arguments were accepted",
    );
    be::draw_indexed_indirect(&encoder, Primitive::TriangleList, IndexType::Uint32, &indices, 0, &arguments, 4).unwrap_or_else(|failure| die(failure));
    be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    expect_pixels([0xff, 0xff, 0x00, 0xff], "indirect indexed triangle pixels differ");

    // Indirect command buffer: one recorded draw, executed twice.
    let indirect =
        be::create_render_pipeline(&device, &render_descriptor(&flat_layout, &fragment_flat, "flat_fragment"), None, None, Some(true)).unwrap_or_else(|failure| die(failure));
    let icb = be::create_icb(&device, 2).unwrap_or_else(|failure| die(failure));
    refused(
        be::icb_set_pipeline(&icb, 2, &indirect).err(),
        ErrorKind::InvalidArgument,
        "out-of-range indirect command index was accepted",
    );
    be::write_buffer(&color, 0, &color_bytes(0., 0., 1., 1.)).unwrap_or_else(|failure| die(failure));
    be::icb_set_pipeline(&icb, 0, &indirect).unwrap_or_else(|failure| die(failure));
    be::icb_set_buffer(&icb, 0, ShaderStage::Vertex, 0, None, &offsets).unwrap_or_else(|failure| die(failure));
    be::icb_set_buffer(&icb, 0, ShaderStage::Fragment, 1, None, &color).unwrap_or_else(|failure| die(failure));
    be::icb_draw_indexed(&icb, 0, Primitive::TriangleList, IndexType::Uint32, &indices, 0, 3, None).unwrap_or_else(|failure| die(failure));
    clear_target(0., 0., 0., 1.);
    for _ in 0..2 {
        let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
        let encoder = load_pass(&commands);
        be::set_viewport(&encoder, full).unwrap_or_else(|failure| die(failure));
        refused(
            be::execute_icb(&encoder, &icb, 0, 1).err(),
            ErrorKind::InvalidState,
            "indirect execution without a pipeline was accepted",
        );
        be::set_render_pipeline(&encoder, &indirect).unwrap_or_else(|failure| die(failure));
        be::use_resources(
            &encoder,
            &[ResourceUse::Buffer(offsets.clone()), ResourceUse::Buffer(color.clone()), ResourceUse::Buffer(indices.clone())],
        ).unwrap_or_else(|failure| die(failure));
        be::execute_icb(&encoder, &icb, 0, 1).unwrap_or_else(|failure| die(failure));
        be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
        poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    }
    expect_pixels([0x00, 0x00, 0xff, 0xff], "indirect command pixels differ");

    // Argument buffer: a 1x1 texture and sampler bound through one buffer.
    let argument_pipeline = be::create_render_pipeline(
        &device,
        &render_descriptor(&flat_layout, &fragment_argument, "argument_fragment"),
        None,
        None,
        Some(true),
    ).unwrap_or_else(|failure| die(failure));
    let argument = be::create_argument(&argument_pipeline, ShaderStage::Fragment, 1).unwrap_or_else(|failure| die(failure));
    require(be::argument_length(&argument) > 0, "argument length is empty");
    let argument_buffer = make_buffer(&device, "arguments", 256.max(be::argument_length(&argument)), &[BufferUsage::Storage]);
    let texel = be::create_texture(
        &device,
        TextureDescriptor {
            label: Some("texel".to_owned()),
            width: 1,
            height: 1,
            ..texture_descriptor("texel", 1, &[TextureUsage::TextureBinding, TextureUsage::TextureCopyDst])
        },
    ).unwrap_or_else(|failure| die(failure));
    let texel_upload = make_buffer(&device, "texel-upload", 256, &[BufferUsage::CopySrc]);
    let mut upload_bytes = vec![0u8; 256];
    upload_bytes[..4].copy_from_slice(&[0x40, 0x80, 0xc0, 0xff]);
    be::write_buffer(&texel_upload, 0, &upload_bytes).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let blit = be::blit_encoder(&commands).unwrap_or_else(|failure| die(failure));
    be::buffer_to_texture(&blit, &texel_upload, None, 256, 256, &texel, None, None, Extent { width: 1, height: 1, depth: 1 }).unwrap_or_else(|failure| die(failure));
    be::end_blit(&blit).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    be::argument_texture(&argument, &argument_buffer, 0, 0, &texel).unwrap_or_else(|failure| die(failure));
    be::argument_sampler(&argument, &argument_buffer, 0, 1, &sampler).unwrap_or_else(|failure| die(failure));
    clear_target(0., 0., 0., 1.);
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = load_pass(&commands);
    be::set_viewport(&encoder, full).unwrap_or_else(|failure| die(failure));
    be::set_render_pipeline(&encoder, &argument_pipeline).unwrap_or_else(|failure| die(failure));
    be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets).unwrap_or_else(|failure| die(failure));
    be::set_stage_buffer(&encoder, ShaderStage::Fragment, 1, None, &argument_buffer).unwrap_or_else(|failure| die(failure));
    be::use_resources(&encoder, &[ResourceUse::Texture(texel.clone())]).unwrap_or_else(|failure| die(failure));
    be::draw(&encoder, Primitive::TriangleList, 0, 3, None).unwrap_or_else(|failure| die(failure));
    be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    expect_pixels([0x40, 0x80, 0xc0, 0xff], "argument-buffer sampled pixels differ");

    // Direct texture and sampler binding.
    let textured = be::create_render_pipeline(
        &device,
        &render_descriptor(&textured_layout, &fragment_textured, "textured_fragment"),
        None,
        None,
        None,
    ).unwrap_or_else(|failure| die(failure));
    clear_target(0., 0., 0., 1.);
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = load_pass(&commands);
    be::set_viewport(&encoder, full).unwrap_or_else(|failure| die(failure));
    be::set_render_pipeline(&encoder, &textured).unwrap_or_else(|failure| die(failure));
    be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets).unwrap_or_else(|failure| die(failure));
    be::set_stage_texture(&encoder, ShaderStage::Fragment, 1, &texel).unwrap_or_else(|failure| die(failure));
    be::set_stage_sampler(&encoder, ShaderStage::Fragment, 2, &sampler).unwrap_or_else(|failure| die(failure));
    be::draw(&encoder, Primitive::TriangleList, 0, 3, None).unwrap_or_else(|failure| die(failure));
    be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    expect_pixels([0x40, 0x80, 0xc0, 0xff], "directly sampled pixels differ");

    // Depth: a cleared depth attachment at 0.25 with Less rejects a triangle at z=0.5 when loaded, and
    // admits it after a clear to 1.
    let depth = be::create_depth_texture(
        &device,
        texture_descriptor("depth", 4, &[TextureUsage::RenderAttachment]),
    ).unwrap_or_else(|failure| die(failure));
    let depth_shader = render_shader("flat_vertex", Stage::Vertex, vec![stage_binding(0, BindingKind::StorageBuffer, Stage::Vertex)]);
    let depth_pipeline = be::create_render_pipeline(
        &device,
        &RenderDescriptor {
            vertex: depth_shader,
            depth_format: DepthFormat::Depth32Float,
            ..render_descriptor(&flat_layout, &fragment_flat, "flat_fragment")
        },
        None,
        None,
        None,
    ).unwrap_or_else(|failure| die(failure));
    let render_depth = |load: Load, clear: f64, expected: [u8; 4], message: &str| {
        clear_target(0., 0., 0., 1.);
        let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
        let mut pass = render_target(&target, Load::Load, Store::Store, (0., 0., 0., 0.));
        pass.depth = Some(DepthAttachment { depth_texture: depth.clone(), depth_load: load, depth_store: Store::Store, depth_clear: clear });
        let encoder = be::render_encoder(&commands, &pass).unwrap_or_else(|failure| die(failure));
        be::set_viewport(&encoder, full).unwrap_or_else(|failure| die(failure));
        be::set_render_pipeline(&encoder, &depth_pipeline).unwrap_or_else(|failure| die(failure));
        be::set_depth_state(&encoder, Some(DepthState { depth_compare: Comparison::Less, depth_write: true })).unwrap_or_else(|failure| die(failure));
        be::set_stage_buffer(&encoder, ShaderStage::Vertex, 0, None, &offsets).unwrap_or_else(|failure| die(failure));
        set_color(&encoder, 1., 1., 0., 1.);
        be::draw(&encoder, Primitive::TriangleList, 0, 3, None).unwrap_or_else(|failure| die(failure));
        be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
        poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
        expect_pixels(expected, message);
    };
    render_depth(Load::Clear, 1., [0xff, 0xff, 0x00, 0xff], "depth clear-to-far rejected the triangle");
    render_depth(Load::Clear, 0., [0x00, 0x00, 0x00, 0xff], "depth clear-to-near admitted the triangle");
    render_depth(Load::Load, 1., [0x00, 0x00, 0x00, 0xff], "depth load ignored the retained near depth");

    // Multisampling: Store with a resolve texture resolves and keeps the samples, so a following Load
    // pass resolves the same colour; Discard cannot resolve.
    let msaa = be::create_texture(
        &device,
        TextureDescriptor {
            label: Some("msaa".to_owned()),
            sample_count: 4,
            usage: vec![TextureUsage::RenderAttachment],
            ..target_descriptor.clone()
        },
    ).unwrap_or_else(|failure| die(failure));
    let msaa_pass = |load: Load, store: Store, clear: (f64, f64, f64, f64)| {
        let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
        let pass = RenderTarget {
            colors: vec![ColorAttachment { texture: msaa.clone(), resolve: Some(target.clone()), load, store, clear }],
            depth: None,
        };
        let encoder = be::render_encoder(&commands, &pass).unwrap_or_else(|failure| die(failure));
        be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
        poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    };
    msaa_pass(Load::Clear, Store::Store, (0.125, 0.25, 0.5, 1.));
    expect_pixels([0x20, 0x40, 0x80, 0xff], "stored multisample pass did not resolve");
    clear_target(0., 0., 0., 1.);
    msaa_pass(Load::Load, Store::Resolve, (0., 0., 0., 0.));
    expect_pixels([0x20, 0x40, 0x80, 0xff], "Store with a resolve texture did not keep the samples");
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
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
        ).err(),
        ErrorKind::InvalidArgument,
        "a discarded attachment was allowed to resolve",
    );
    be::abandon(&commands).unwrap_or_else(|failure| die(failure));
    be::destroy_texture(&msaa).unwrap_or_else(|failure| die(failure));
    be::destroy_texture(&depth).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&depth_pipeline).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&textured).unwrap_or_else(|failure| die(failure));
    be::destroy_argument(&argument).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&argument_buffer).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&texel_upload).unwrap_or_else(|failure| die(failure));
    be::destroy_texture(&texel).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&argument_pipeline).unwrap_or_else(|failure| die(failure));
    be::destroy_icb(&icb).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&indirect).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&color).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&indices).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&arguments).unwrap_or_else(|failure| die(failure));
    be::destroy_pipeline(&flat).unwrap_or_else(|failure| die(failure));
    be::destroy_buffer(&offsets).unwrap_or_else(|failure| die(failure));
    be::destroy_texture(&target).unwrap_or_else(|failure| die(failure));
    be::destroy_sampler(&sampler).unwrap_or_else(|failure| die(failure));
    be::destroy_queue(&queue).unwrap_or_else(|failure| die(failure));
    finish(&device);
}

/// VK_ERROR_OUT_OF_DEVICE_MEMORY is a typed Capacity error, never a crash. One buffer of twice the
/// working set cannot fit any heap, so the driver refuses it at once.
/// A standalone painter's three primitives: offscreen target readback, R8 sub-rectangle write, persistently mapped host buffer.
#[test]
fn gpu_painter_primitives() {
    let Some(device) = open() else { return };
    let queue = be::create_queue(&device).unwrap_or_else(|failure| die(failure));
    let usage = [TextureUsage::TextureBinding, TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc];
    let target = be::create_texture(&device, texture_descriptor("offscreen", 4, &usage)).unwrap_or_else(|failure| die(failure));
    let commands = be::begin_commands(&queue).unwrap_or_else(|failure| die(failure));
    let encoder = be::render_encoder(&commands, &render_target(&target, Load::Clear, Store::Store, (0.125, 0.25, 0.5, 1.))).unwrap_or_else(|failure| die(failure));
    be::end_render(&encoder).unwrap_or_else(|failure| die(failure));
    poll_epoch(&queue, be::commit(&commands).unwrap_or_else(|failure| die(failure)));
    let pixels = be::read_texture(&target, 16).unwrap_or_else(|failure| die(failure));
    require(pixels.chunks_exact(4).all(|p| p == [0x20, 0x40, 0x80, 0xff]), "offscreen clear differs");

    let atlas = be::create_texture(&device, TextureDescriptor { format: TextureFormat::R8Unorm, ..texture_descriptor("atlas", 4, &[TextureUsage::TextureBinding, TextureUsage::TextureCopyDst]) }).unwrap_or_else(|failure| die(failure));
    // A 2x2 glyph with a padded 4-byte row pitch, whose last row carries only its pixels.
    be::write_texture(&atlas, None, Origin { x: 1, y: 1, z: 0 }, Extent { width: 2, height: 2, depth: 1 }, 4, &[1, 2, 0, 0, 3, 4]).unwrap_or_else(|failure| die(failure));
    let mut expected = vec![0u8; 16];
    expected[5..7].copy_from_slice(&[1, 2]);
    expected[9..11].copy_from_slice(&[3, 4]);
    require(be::read_texture(&atlas, 4).unwrap_or_else(|failure| die(failure)) == expected, "R8 sub-rectangle write differs");
    refused(be::write_texture(&atlas, None, Origin { x: 3, y: 3, z: 0 }, Extent { width: 2, height: 2, depth: 1 }, 4, &[0; 6]).err(), ErrorKind::InvalidArgument, "an out-of-bounds rectangle was accepted");

    let ring = be::create_host_buffer(&device, buf("ring", 64, &[BufferUsage::Vertex, BufferUsage::Storage])).unwrap_or_else(|failure| die(failure));
    let (pointer, length) = be::host_ptr(&ring).unwrap_or_else(|failure| die(failure));
    require(length == 64 && be::host_ptr(&ring).unwrap_or_else(|failure| die(failure)).0 == pointer, "the ring pointer is not stable");
    unsafe { std::slice::from_raw_parts_mut(pointer.as_ptr().add(16), 4) }.copy_from_slice(&[9, 8, 7, 6]);
    be::flush_host_range(&ring, 16, 4).unwrap_or_else(|failure| die(failure));
    require(be::read_buffer(&ring, 16, 4).unwrap_or_else(|failure| die(failure)) == [9, 8, 7, 6], "ring write is not visible");
    let local = be::create_buffer(&device, buf("local", 64, &[BufferUsage::Vertex]), Some(Memory::DeviceLocal)).unwrap_or_else(|failure| die(failure));
    refused(be::host_ptr(&local).err(), ErrorKind::Unsupported, "a device-local buffer was host mapped");
    for texture in [&target, &atlas] { be::destroy_texture(texture).unwrap_or_else(|failure| die(failure)); }
    for buffer in [&ring, &local] { be::destroy_buffer(buffer).unwrap_or_else(|failure| die(failure)); }
    be::destroy_queue(&queue).unwrap_or_else(|failure| die(failure));
    finish(&device);
}

#[test]
fn gpu_out_of_device_memory_is_capacity() {
    let raw = match crate::vulkan::Device::create(true,true) {
        Err(failure) if failure.kind == crate::vulkan::ErrorKind::NoAdapter => {
            eprintln!("RAYS_TEST_SKIP: OGPU conformance (Vulkan): skipped (no device)");
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
            return;
        },
        Err(failure) => panic!("{failure}"),
        Ok(raw) => raw,
    };
    let size = 2 * raw.memory().expect("native memory budget").device_local_budget;
    // This probes physical OOM deliberately beyond Core's logical resource limit.
    // The safe native allocation API still uses the owning backend's typed error mapper.
    match raw.buffer(size,crate::vulkan::BufferKind::DeviceLocal) {
        Err(failure) => assert_eq!(crate::backend::of_vk("Ogpu_vulkan.create_buffer",failure).kind,ErrorKind::Capacity),
        Ok(resource) => { drop(resource); panic!("a buffer of twice the working set was created"); }
    }
    drop(raw);
    assert_eq!(impl_::live_handles(), 0, "out-of-memory test leaked handles");
    assert_eq!(crate::vulkan::Device::validation_messages(), 0, "out-of-memory test: validation messages");
}
