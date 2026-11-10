#![allow(non_camel_case_types)]
use crate::common;
use ogpu_core::backend::{self, Buffer};
use ogpu_core::pipeline::Blend;
use ogpu_core::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu_core::types::{BufferDescriptor, BufferUsage};
use ::renderer::renderer::{self, PipelineFamily, Renderer};
use ::renderer::scene_execution::{self, GeometryDraw, Mesh, MeshKey, new_bytes, SampledDraw, State};
use std::{cell::Cell, rc::Rc};

fn config() -> renderer::Configuration {
    renderer::Configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, ..renderer::default_configuration() }
}

fn set(bytes: &mut [u8], index: usize, value: f32) {
    bytes[index * 4..index * 4 + 4].copy_from_slice(&value.to_le_bytes());
}

fn set_u32(bytes: &mut [u8], index: usize, value: u32) {
    bytes[index * 4..index * 4 + 4].copy_from_slice(&value.to_le_bytes());
}

fn uniform(red: bool) -> Vec<u8> {
    let mut bytes = vec![0u8; 5456];
    for matrix in 0..3 {
        for diagonal in 0..4 {
            set(&mut bytes, matrix * 16 + diagonal * 5, 1.0);
        }
    }
    set(&mut bytes, 0, 0.5);
    set(&mut bytes, 3, if red { -0.5 } else { 0.5 });
    set(&mut bytes, 50, -1.0);
    set(&mut bytes, 52, if red { 1.0 } else { 0.0 });
    set(&mut bytes, 53, if red { 0.0 } else { 1.0 });
    set(&mut bytes, 55, 1.0);
    for i in 69..73 {
        set(&mut bytes, i, 1.0);
    }
    bytes
}

struct fixture {
    vertices: Vec<u8>,
    indices: Vec<u8>,
    attributes: Vec<u8>,
    mesh: Mesh,
}

fn fixture() -> fixture {
    let mut indices = vec![0u8; 12];
    let mut vertices = vec![0u8; 24 * 3];
    let mut attributes = vec![0u8; 12 * 3];
    set_u32(&mut indices, 1, 1);
    set_u32(&mut indices, 2, 2);
    for (index, (x, y)) in [(-1.0f32, -1.0f32), (3.0, -1.0), (-1.0, 3.0)].iter().enumerate() {
        let offset = index * 24;
        vertices[offset..offset + 4].copy_from_slice(&x.to_le_bytes());
        vertices[offset + 4..offset + 8].copy_from_slice(&y.to_le_bytes());
        vertices[offset + 20..offset + 24].copy_from_slice(&1.0f32.to_le_bytes());
        attributes[index * 12..index * 12 + 4].copy_from_slice(&0xffffffffu32.to_le_bytes());
    }
    let mesh = Mesh {
        key: MeshKey::Host("scene3-shared-mesh".to_owned()),
        vertices: new_bytes(vertices.clone()),
        shared_vertices: None,
        vertex_count: 3,
        indices: new_bytes(indices.clone()),
        index_count: 3,
        primitive: Primitive::TriangleList,
    };
    fixture { vertices, indices, attributes, mesh }
}

fn entry(f: &fixture, scissor: (i64, i64, i64, i64), uniforms: Vec<u8>) -> SampledDraw {
    SampledDraw {
        family: PipelineFamily::Scene3,
        blend: Blend::Replace,
        texture: None,
        auxiliary: None,
        vertex_attributes: Some(("test-attributes".to_owned(), new_bytes(f.attributes.clone()))),
        samples: 1,
        draw: GeometryDraw {
            mesh: f.mesh.clone(),
            state: State {
                viewport: (0, 0, 4, 4),
                scissor,
                cull: Cull::CullNone,
                depth_compare: Comparison::Always,
                depth_write: false,
                depth_load: Load::Clear,
                depth_clear: 1.0,
                transform_uniforms: Some(new_bytes(uniforms)),
            },
        },
    }
}

fn render(runtime: &Renderer, entries: Vec<SampledDraw>) -> Vec<u8> {
    let draws: Vec<Rc<SampledDraw>> = entries.into_iter().map(Rc::new).collect();
    renderer::draw(None, runtime, &draws).unwrap();
    renderer::read_pixels(runtime, 256).unwrap()
}

fn gpu_scene3_shared_mesh_and_instances() {
    if !common::gpu() {
        return;
    }
    let runtime = renderer::create_offscreen(&config()).unwrap();
    let f = fixture();
    let left = entry(&f, (0, 0, 4, 4), uniform(true));
    let right = entry(&f, (0, 0, 4, 4), uniform(false));
    for _frame in 0..3 {
        let pixels = render(&runtime, vec![left.clone(), right.clone()]);
        assert!(
            pixels[0] == 255 && pixels[1] == 0 && pixels[12] == 0 && pixels[13] == 255,
            "Scene3 draws sharing a mesh lost distinct uniforms: {},{} / {},{}",
            pixels[0],
            pixels[1],
            pixels[12],
            pixels[13]
        );
    }
    let first = uniform(true);
    let mut second = uniform(false);
    set(&mut second, 52, 1.0);
    set(&mut second, 53, 0.0);
    let expected = render(&runtime, vec![entry(&f, (0, 0, 4, 4), first.clone()), entry(&f, (0, 0, 4, 4), second.clone())]);
    let mut instanced = vec![0u8; 5456 + 2 * 192];
    instanced[..5456].copy_from_slice(&first);
    set(&mut instanced, 83, 1.0);
    instanced[5456..5456 + 192].copy_from_slice(&first[..192]);
    instanced[5456 + 192..5456 + 384].copy_from_slice(&second[..192]);
    let actual = render(&runtime, vec![entry(&f, (0, 0, 4, 4), instanced)]);
    assert!(actual == expected, "instanced Scene3 draw differs from two indexed draws");
    let cases: [(Primitive, Vec<(f32, f32)>, &str); 2] = [
        (Primitive::LineList, vec![(-1.0, 0.0), (1.0, 0.0)], "line"),
        (Primitive::PointList, vec![(-0.5, -0.75)], "point"),
    ];
    for (primitive, positions, name) in cases.iter() {
        let count = positions.len();
        let mut vertices = vec![0u8; count * 24];
        let mut attributes = vec![0u8; count * 12];
        let mut indices = vec![0u8; count * 4];
        for (index, (x, y)) in positions.iter().enumerate() {
            let offset = index * 24;
            vertices[offset..offset + 4].copy_from_slice(&x.to_le_bytes());
            vertices[offset + 4..offset + 8].copy_from_slice(&y.to_le_bytes());
            vertices[offset + 20..offset + 24].copy_from_slice(&1.0f32.to_le_bytes());
            attributes[index * 12..index * 12 + 4].copy_from_slice(&0xffffffffu32.to_le_bytes());
            set_u32(&mut indices, index, index as u32);
        }
        let mut e = entry(&f, (0, 0, 4, 4), uniform(true));
        e.family = if *primitive == Primitive::PointList { PipelineFamily::Scene3Points } else { PipelineFamily::Scene3 };
        e.draw.mesh = Mesh {
            vertices: new_bytes(vertices),
            indices: new_bytes(indices),
            vertex_count: count as i64,
            index_count: count as i64,
            primitive: *primitive,
            ..f.mesh.clone()
        };
        e.vertex_attributes = Some(("primitive-attributes".to_owned(), new_bytes(attributes)));
        let pixels = render(&runtime, vec![e]);
        let mut lit = false;
        for row in 0..4 {
            for column in 0..4 {
                if pixels[row * 256 + column * 4] > 0 {
                    lit = true;
                }
            }
        }
        assert!(lit, "Scene3 native draw produced no pixels: {}", name);
    }
    renderer::destroy(&runtime).unwrap();
    println!("Scene3 shared-mesh and instances passed");
}

struct resident {
    runtime: Renderer,
    host: Renderer,
    gpu: renderer::Gpu,
    positions: Buffer,
    device_indices: Buffer,
    key: String,
    alive: Rc<Cell<bool>>,
    unregister: Box<dyn Fn()>,
    f: fixture,
}

fn resident() -> resident {
    let f = fixture();
    let host = renderer::create_offscreen(&config()).unwrap();
    let gpu = renderer::acquire_gpu().unwrap();
    let runtime = renderer::native::create_offscreen_with_gpu(&gpu, &config()).unwrap();
    let device = renderer::gpu_device(&gpu);
    let positions = backend::create_buffer(
        &device,
        BufferDescriptor { label: Some("indexed-test-P".to_owned()), size: 36, usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst] },
        None,
    ).unwrap();
    let device_indices = backend::create_buffer(
        &device,
        BufferDescriptor { label: Some("indexed-test-indices".to_owned()), size: 12, usage: vec![BufferUsage::Index, BufferUsage::Storage, BufferUsage::CopyDst] },
        None,
    ).unwrap();
    let mut position_bytes = vec![0u8; 36];
    for i in 0..3 {
        position_bytes[i * 12..i * 12 + 12].copy_from_slice(&f.vertices[i * 24..i * 24 + 12]);
    }
    backend::write_buffer(&positions, 0, &position_bytes).unwrap();
    backend::write_buffer(&device_indices, 0, &f.indices).unwrap();
    let alive = Rc::new(Cell::new(true));
    let valid = alive.clone();
    let (key, unregister) = renderer::register_gpu_positions(&gpu, &positions, None, Some((device_indices.clone(), 3)), None, 3, Rc::new(move || valid.get())).unwrap();
    resident { runtime, host, gpu, positions, device_indices, key, alive, unregister, f }
}

fn finish(r: resident, color_buffers: Vec<Buffer>) {
    (r.unregister)();
    renderer::destroy(&r.runtime).unwrap();
    for buffer in color_buffers.iter() {
        backend::destroy_buffer(buffer).unwrap();
    }
    backend::destroy_buffer(&r.device_indices).unwrap();
    backend::destroy_buffer(&r.positions).unwrap();
    renderer::release_gpu(&r.gpu);
    renderer::destroy(&r.host).unwrap();
}

fn device_entry(r: &resident, key: String, uniforms: Vec<u8>, attributes: &str, host_bytes: Vec<u8>) -> SampledDraw {
    let mut e = entry(&r.f, (0, 0, 4, 4), uniforms);
    e.vertex_attributes = Some((attributes.to_owned(), new_bytes(host_bytes)));
    e.draw.mesh = Mesh { key: MeshKey::GpuVertices { name: Rc::new(key), indices: None }, vertices: scene_execution::empty_bytes(), indices: scene_execution::empty_bytes(), ..r.f.mesh.clone() };
    e
}

fn gpu_scene3_device_indices_and_white() {
    if !common::gpu() {
        return;
    }
    let r = resident();
    let expected = render(&r.host, vec![entry(&r.f, (0, 0, 4, 4), uniform(true))]);
    let mut device_uniform = uniform(true);
    set(&mut device_uniform, 79, 1.0);
    let e = device_entry(&r, r.key.clone(), device_uniform.clone(), "test-attributes", r.f.attributes.clone());
    for _ in 0..3 {
        let actual = render(&r.runtime, vec![e.clone()]);
        assert!(actual == expected, "borrowed UInt32 Scene3 indices differ from host indices");
    }
    let mut white_uniform = device_uniform.clone();
    set(&mut white_uniform, 81, -1.0);
    let white = device_entry(&r, r.key.clone(), white_uniform, "device-white", Vec::new());
    assert!(render(&r.runtime, vec![white]) == expected, "constant device white differs from the host default attribute record");
    finish(r, Vec::new());
}

fn resident_colors(width: usize) {
    if !common::gpu() {
        return;
    }
    let r = resident();
    let device = renderer::gpu_device(&r.gpu);
    let values: [f32; 12] = [-0.2, 0.25, 0.5, 0.65, 0.8, 1.3, 0.12, 0.25, 0.1, 0.7, 0.9, 1.0];
    let mut color_bytes = vec![0u8; 3 * width * 4];
    let mut host_attributes = r.f.attributes.clone();
    for i in 0..3 {
        for c in 0..width {
            color_bytes[(i * width + c) * 4..(i * width + c) * 4 + 4].copy_from_slice(&values[i * 4 + c].to_le_bytes());
        }
        let channel = |c: usize| values[i * 4 + c] as f64;
        let color = rays_math::color::of_floats(channel(0), channel(1), channel(2), if width == 4 { channel(3) } else { 1.0 });
        let packed = ((color.r as u32) << 24) | ((color.g as u32) << 16) | ((color.b as u32) << 8) | (color.a as u32);
        set_u32(&mut host_attributes, i * 3, packed);
    }
    let color_buffer = backend::create_buffer(
        &device,
        BufferDescriptor { label: Some("indexed-test-Cd".to_owned()), size: color_bytes.len() as i64, usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst] },
        None,
    ).unwrap();
    backend::write_buffer(&color_buffer, 0, &color_bytes).unwrap();
    let (color_key, unregister_color) = renderer::register_gpu_positions(
        &r.gpu,
        &r.positions,
        None,
        Some((r.device_indices.clone(), 3)),
        Some((color_buffer.clone(), width as i64)),
        3,
        Rc::new(|| true),
    ).unwrap();
    let mut white = uniform(true);
    set(&mut white, 53, 1.0);
    set(&mut white, 54, 1.0);
    let mut reference = entry(&r.f, (0, 0, 4, 4), white.clone());
    reference.vertex_attributes = Some((format!("host-colors-{}", width), new_bytes(host_attributes)));
    let expected = render(&r.host, vec![reference]);
    let mut device_uniform = white;
    set(&mut device_uniform, 79, 1.0);
    set(&mut device_uniform, 81, width as f32);
    let colored = device_entry(&r, color_key, device_uniform, "resident-colors", Vec::new());
    for _ in 0..3 {
        let actual = render(&r.runtime, vec![colored.clone()]);
        if actual != expected {
            let at = (0..actual.len()).find(|&i| actual[i] != expected[i]).unwrap_or(actual.len());
            panic!("resident float{} colours differ at byte {}: {} vs {}", width, at, actual[at], expected[at]);
        }
    }
    unregister_color();
    finish(r, vec![color_buffer]);
}

fn gpu_scene3_resident_colors3() {
    resident_colors(3);
}

fn gpu_scene3_resident_colors4() {
    resident_colors(4);
}

fn gpu_scene3_borrowed_stream_guards() {
    if !common::gpu() { return; }
    // Match the source's owned-device lifetime, excluding the unrelated shared probe cache.
    common::release_keepalive();
    use ogpu_core::error::ErrorKind;
    use ::renderer::renderer::native;
    let baseline=ogpu_core::impl_::live_handles().unwrap();
    let r=resident();
    let mut uniforms=uniform(true); set(&mut uniforms,79,1.);
    let e=device_entry(&r,r.key.clone(),uniforms,"indexed-attributes",r.f.attributes.clone());
    let submit=|entry: SampledDraw| {
        let submission=native::begin_submission(&r.runtime)?;
        let batch=native::adopt_draws(&submission,vec![Rc::new(entry)])?;
        native::step(None,Some("borrowed-indexed"),Some(1),&submission,&[batch])
    };
    submit(e.clone()).unwrap();
    let expected=render(&r.host,vec![entry(&r.f,(0,0,4,4),uniform(true))]);
    assert_eq!(renderer::read_pixels(&r.runtime,256).unwrap(),expected);
    let uploaded=renderer::stats(&r.runtime).unwrap().uploaded_bytes;
    assert!(native::replay(None,"borrowed-indexed",1,&r.runtime).unwrap().is_some());
    assert_eq!(renderer::stats(&r.runtime).unwrap().uploaded_bytes,uploaded);
    // Match the source's validity transition while this prepared submission is still retained.
    r.alive.set(false);
    match native::replay(None, "borrowed-indexed", 1, &r.runtime) {
        Err(error) => assert_eq!(error.kind, ErrorKind::StaleHandle),
        Ok(_) => panic!("invalid producer replay accepted"),
    }
    r.alive.set(true);
    // A fresh non-retained submission must validate each malformed stream before uploading.
    let check=|bad: SampledDraw| {
        let before=renderer::stats(&r.runtime).unwrap().uploaded_bytes;
        match renderer::draw(None,&r.runtime,&[Rc::new(bad)]) {
            Err(error) => assert_eq!(error.kind,ErrorKind::InvalidArgument),
            Ok(_) => panic!("malformed borrowed Scene3 accepted"),
        }
        assert_eq!(renderer::stats(&r.runtime).unwrap().uploaded_bytes,before);
    };
    let mut bad=e.clone(); bad.draw.mesh.index_count=6; check(bad);
    let mut bad=e.clone(); bad.draw.mesh.indices=new_bytes(r.f.indices.clone()); check(bad);
    let mut bad=e.clone(); bad.draw.mesh.vertex_count=2; check(bad);
    let mut bad=e.clone(); bad.vertex_attributes=Some(("missing".into(),scene_execution::empty_bytes())); check(bad);
    let mut bad=e.clone(); bad.family=PipelineFamily::Marks; bad.vertex_attributes=None; check(bad);
    let device=renderer::gpu_device(&r.gpu);
    let colors=backend::create_buffer(&device,BufferDescriptor { label:Some("guard-colors".into()),size:36,usage:vec![BufferUsage::Vertex,BufferUsage::Storage,BufferUsage::CopyDst] },None).unwrap();
    backend::write_buffer(&colors,0,&[1f32.to_le_bytes();9].concat()).unwrap();
    let (key,unregister)=renderer::register_gpu_positions(&r.gpu,&r.positions,None,Some((r.device_indices.clone(),3)),Some((colors.clone(),3)),3,Rc::new(|| true)).unwrap();
    let mut uniforms=uniform(true); set(&mut uniforms,79,1.);
    let colored=device_entry(&r,key.clone(),uniforms.clone(),"resident-colors",Vec::new());
    check(colored);
    set(&mut uniforms,81,3.);
    let colored=device_entry(&r,key,uniforms,"resident-colors",Vec::new());
    assert_eq!(render(&r.runtime,vec![colored]),expected);
    unregister(); backend::destroy_buffer(&colors).unwrap();
    (r.unregister)();
    match renderer::draw(None,&r.runtime,&[Rc::new(e.clone())]) {
        Err(error) => assert_eq!(error.kind,ErrorKind::StaleHandle),
        Ok(_) => panic!("closed producer draw accepted"),
    }
    assert_eq!(backend::read_buffer(&r.positions,0,36).unwrap().len(),36,"closing registration destroyed producer storage");
    let foreign=backend::create_device(ogpu_core::impl_::create_driver().unwrap()).unwrap();
    let source=scene_execution::register_gpu_vertices(&native::gpu_vertices_owner(&r.runtime),None,Some(12),None,Some((r.device_indices.clone(),3)),None,&r.key,&foreign,&r.positions,3,Box::new(|| true)).unwrap();
    match renderer::draw(None,&r.runtime,&[Rc::new(e)]) {
        Err(error) => assert_eq!(error.kind,ErrorKind::CrossDevice),
        Ok(_) => panic!("foreign producer device accepted"),
    }
    scene_execution::unregister_gpu_vertices(&source);
    backend::destroy_device(&foreign).unwrap();
    finish(r,Vec::new());
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(),baseline);
}

#[derive(Default)]
struct native_vertex_observation {
    buffers: std::collections::HashMap<String, backend::Token>,
    bindings: Vec<(i64, backend::Token, i64)>,
    mesh_upload_bytes: usize,
}

type Seen = Rc<std::cell::RefCell<native_vertex_observation>>;
type E = ogpu_core::error::Error;
macro_rules! forward {
    ($($name:ident($($arg:ident: $ty:ty),*) -> $ret:ty;)*) => {
        $(fn $name(&self, $($arg: $ty),*) -> $ret { self.inner.$name($($arg),*) })*
    };
}
struct Observed<T: ?Sized> { inner: Box<T>, seen: Seen }
impl backend::DriverResource for Observed<dyn backend::DriverResource> {
    forward! {
        token() -> backend::Token;
        as_any() -> &dyn std::any::Any;
        read(offset: i64, length: usize) -> Result<Vec<u8>, E>;
        read_into(offset: i64, destination: &mut [u8], destination_offset: usize, length: usize) -> Result<(), E>;
        destroy() -> Result<(), E>;
    }
    fn write(&self, offset: i64, bytes: &[u8]) -> Result<(), E> {
        self.inner.write(offset, bytes)?;
        self.seen.borrow_mut().mesh_upload_bytes += bytes.len();
        Ok(())
    }
}
impl backend::DriverRenderEncoder for Observed<dyn backend::DriverRenderEncoder> {
    forward! {
        render_set_pipeline(pipeline: &dyn backend::DriverPipeline) -> Result<(), E>;
        set_stage_texture(stage: backend::ShaderStage, index: i64, texture: &dyn backend::DriverResource) -> Result<(), E>;
        set_stage_sampler(stage: backend::ShaderStage, index: i64, sampler: &dyn backend::DriverSampler) -> Result<(), E>;
        set_viewport(rect: ogpu_core::render_pass::AdmittedViewport) -> Result<(), E>;
        set_scissor(rect: ogpu_core::render_pass::Rect) -> Result<(), E>;
        set_cull(cull: Cull) -> Result<(), E>;
        set_winding(winding: backend::Winding) -> Result<(), E>;
        set_depth_state(state: Option<backend::DepthState>) -> Result<(), E>;
        draw(primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), E>;
        draw_indexed(primitive: Primitive, index_type: ogpu_core::render_pass::IndexType, buffer: &dyn backend::DriverResource, offset: i64, count: i64, instances: i64) -> Result<(), E>;
        draw_indexed_indirect(primitive: Primitive, index_type: ogpu_core::render_pass::IndexType, buffer: &dyn backend::DriverResource, offset: i64, arguments: &dyn backend::DriverResource, arguments_offset: i64) -> Result<(), E>;
        use_resources(resources: &[Rc<dyn backend::DriverResource>]) -> Result<(), E>;
        execute_icb(icb: backend::Token, location: i64, length: i64) -> Result<(), E>;
        end_render() -> Result<(), E>;
    }
    fn set_stage_buffer(&self, stage: backend::ShaderStage, slot: i64, offset: i64, buffer: &dyn backend::DriverResource) -> Result<(), E> {
        self.inner.set_stage_buffer(stage, slot, offset, buffer)?;
        if stage == backend::ShaderStage::Vertex {
            self.seen.borrow_mut().bindings.push((slot, buffer.token(), offset));
        }
        Ok(())
    }
    fn draw_batch(&self, draws: &[backend::DriverBatchDraw]) -> Result<(), E> {
        self.inner.draw_batch(draws)?;
        for draw in draws {
            for (stage, slot, buffer, offset) in &draw.batch_buffers {
                if *stage == backend::ShaderStage::Vertex {
                    self.seen.borrow_mut().bindings.push((*slot, buffer.token(), *offset));
                }
            }
        }
        Ok(())
    }
}
impl backend::DriverCommands for Observed<dyn backend::DriverCommands> {
    forward! {
        compute_encoder() -> Result<Box<dyn backend::DriverComputeEncoder>, E>;
        accel_encoder() -> Result<Box<dyn backend::DriverAccelEncoder>, E>;
        blit_encoder() -> Result<Box<dyn backend::DriverBlitEncoder>, E>;
        commit() -> Result<backend::Receipt, E>;
        commit_present(source: &dyn backend::DriverResource, frame: backend::DriverFrame) -> Result<backend::Receipt, E>;
        abandon() -> Result<(), E>;
    }
    fn render_encoder(&self, target: &backend::DriverRenderTarget) -> Result<Box<dyn backend::DriverRenderEncoder>, E> {
        Ok(Box::new(Observed { inner: self.inner.render_encoder(target)?, seen: self.seen.clone() }))
    }
}
impl backend::DriverQueue for Observed<dyn backend::DriverQueue> {
    forward! {
        complete_through(epoch: i64) -> Result<(), E>;
        poll_through(epoch: i64) -> Result<bool, E>;
        gpu_duration(epoch: i64) -> Option<f64>;
        destroy_queue() -> Result<(), E>;
    }
    fn begin_commands(&self) -> Result<Box<dyn backend::DriverCommands>, E> {
        Ok(Box::new(Observed { inner: self.inner.begin_commands()?, seen: self.seen.clone() }))
    }
}
impl backend::DriverDevice for Observed<dyn backend::DriverDevice> {
    forward! {
        device_handle() -> ogpu_core::handle::Device;
        capabilities() -> ogpu_core::caps::Caps;
        allocated_size() -> i64;
        recommended_working_set() -> i64;
        create_mapped_buffer(descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<backend::DriverMapped, E>;
        create_texture(descriptor: &ogpu_core::validation::AdmittedTexture) -> Result<Box<dyn backend::DriverResource>, E>;
        create_library(shader: &ogpu_core::shader::Shader) -> Result<Box<dyn backend::DriverLibrary>, E>;
        warm_library(shader: &ogpu_core::shader::Shader) -> Option<backend::Warm>;
        create_accel(descriptor: &backend::DriverAccelDescriptor) -> Result<Box<dyn backend::DriverAccel>, E>;
        instance_layout(kind: backend::InstanceKind) -> Vec<i64>;
        create_sampler(descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn backend::DriverSampler>, E>;
        create_render_pipeline(options: backend::RenderPipelineOptions, descriptor: &ogpu_core::pipeline::RenderDescriptor, admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Box<dyn backend::DriverPipeline>, E>;
        create_icb(profile: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn backend::DriverIcb>, E>;
        create_argument(pipeline: &dyn backend::DriverPipeline, stage: backend::ShaderStage, index: ogpu_core::validation::AdmittedSlot) -> Result<Box<dyn backend::DriverArgument>, E>;
        create_surface(configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn backend::DriverSurface>, E>;
        destroy_device() -> Result<(), E>;
    }
    fn create_buffer(&self, descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<Box<dyn backend::DriverResource>, E> {
        let buffer = self.inner.create_buffer(descriptor)?;
        if let Some(label) = &descriptor.label {
            self.seen.borrow_mut().buffers.insert(label.clone(), buffer.token());
        }
        if descriptor.label.as_ref().is_some_and(|label| label.starts_with("scene-mesh-")) {
            Ok(Box::new(Observed { inner: buffer, seen: self.seen.clone() }))
        } else {
            Ok(buffer)
        }
    }
    fn create_queue(&self) -> Result<Box<dyn backend::DriverQueue>, E> {
        Ok(Box::new(Observed { inner: self.inner.create_queue()?, seen: self.seen.clone() }))
    }
}
struct ObservedDriver { base: Box<dyn backend::Driver>, seen: Seen }
impl backend::Driver for ObservedDriver {
    fn create_device(&self) -> Result<Box<dyn backend::DriverDevice>, E> {
        Ok(Box::new(Observed { inner: self.base.create_device()?, seen: self.seen.clone() }))
    }
}

// Observe the real native ABI: wrappers forward every unobserved operation, and each
// intercepted operation delegates to the original driver before recording its successful result.
fn observe_native_vertices(base: Box<dyn backend::Driver>, seen: Seen) -> Box<dyn backend::Driver> {
    Box::new(ObservedDriver { base, seen })
}

// lib/scene_execution/test_gpu_vertices.ml:249-283,291-310,344-370.
fn gpu_borrowed_vertex_encoder_observation() {
    if !common::gpu() { return; }
    use ::renderer::{renderer_pipeline, scene_execution as se};
    use ogpu_core::surface::{Configuration, Format, PresentMode};
    use ogpu_core::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
    let baseline = ogpu_core::impl_::live_handles().unwrap();
    let seen = Rc::new(std::cell::RefCell::new(native_vertex_observation::default()));
    let driver = observe_native_vertices(ogpu_core::impl_::create_driver().unwrap(), seen.clone());
    let cfg = Configuration { logical_width: 4, logical_height: 4, physical_width: 4, physical_height: 4, format: Format::Rgba8Unorm, present_mode: PresentMode::Immediate, max_acquired: 1, layer: None };
    let owner = se::create_gpu_vertices_owner();
    let mut runtime = renderer_pipeline::create_renderer(&ogpu_core::impl_::backend().unwrap(), None, Some(owner.clone()), true, driver, &cfg).unwrap();
    let device = se::device(&runtime);
    let upload = |label: &str, bytes: &[u8]| {
        let buffer = backend::create_buffer(&device, BufferDescriptor {
            label: Some(label.to_owned()), size: bytes.len() as i64,
            usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst],
        }, None).unwrap();
        backend::write_buffer(&buffer, 0, bytes).unwrap();
        buffer
    };
    let mut records = vec![0;128];
    for (record, left, right) in [(0, 0., 2.), (1, 2., 4.)] {
        for (field, value) in [(0,left), (1,0.), (2,right), (3,4.)] {
            set(&mut records, record * 16 + field, value);
        }
        set_u32(&mut records, record * 16 + 8, u32::MAX);
    }
    let producer = upload("observed-ui-producer", &records);
    let source = se::register_gpu_vertices(&owner, None, None, None, None, None, "observed\nui", &device, &producer, 8, Box::new(|| true)).unwrap();
    let indices = new_bytes(common::words(&[0,1,2,0,2,3,4,5,6,4,6,7]));
    let texture = Rc::new(se::SampledTexture {
        key: "observed-white".into(),
        levels: Rc::new(vec![se::TextureLevel { width: 1, height: 1, bytes: new_bytes(vec![255;4]) }]),
        gpu: None,
        sampler: SamplerDescriptor { label: None, min_filter: SamplerFilter::Nearest, mag_filter: SamplerFilter::Nearest, mip_filter: MipFilter::NoMip, address_u: AddressMode::ClampToEdge, address_v: AddressMode::ClampToEdge, lod_min: 0., lod_max: 0., max_anisotropy: 1 },
    });
    let draw = Rc::new(SampledDraw {
        family: PipelineFamily::Ui, blend: Blend::Alpha, texture: Some(texture), auxiliary: None, vertex_attributes: None, samples: 1,
        draw: GeometryDraw {
            mesh: Mesh { key: MeshKey::GpuVertices { name: Rc::new("observed\nui".into()), indices: Some(Rc::new("shared\nhost-indices".into())) }, vertices: se::empty_bytes(), shared_vertices: None, vertex_count: 8, indices, index_count: 12, primitive: Primitive::TriangleList },
            state: State { viewport: (0,0,4,4), scissor: (0,0,4,4), cull: Cull::CullNone, depth_compare: Comparison::Always, depth_write: false, depth_load: Load::Load, depth_clear: 1., transform_uniforms: Some(new_bytes(common::floats(&[0.5,0.,-1.,0.,-0.5,1.]))) },
        },
    });
    let borrowed = se::PreparedScene3 { entries: vec![draw.clone()] };
    assert!(!se::borrows_unregistered(&owner, &borrowed));
    let prepared = || Some(("observed-borrowed-plan".into(),1));
    assert!(se::submit(prepared(), &mut || {}, None, &mut runtime, std::slice::from_ref(&draw)).unwrap());
    let expected = vec![255;4*256];
    // read_pixels uses 256-byte rows: compare every active pixel, not uninitialized padding.
    let check_pixels = |runtime: &mut se::SceneExecution| {
        let pixels = se::read_pixels(runtime,256).unwrap();
        for row in 0..4 { assert_eq!(&pixels[row*256..row*256+16], &expected[row*256..row*256+16]); }
    };
    check_pixels(&mut runtime);
    let producer_token = seen.borrow().buffers["observed-ui-producer"];
    let assert_slot = |slot, token| {
        let seen = seen.borrow();
        let bindings: Vec<_> = seen.bindings.iter().filter(|binding| binding.0 == slot).collect();
        assert!(!bindings.is_empty(), "native vertex slot {slot} was never bound");
        assert!(bindings.iter().all(|binding| binding.1 == token && binding.2 == 0), "native vertex slot {slot} lost its producer token/offset");
    };
    assert_slot(0, producer_token);
    assert_eq!(seen.borrow().mesh_upload_bytes,48);
    let uploaded = se::upload_bytes(&runtime);
    assert!(uploaded > 0);
    assert!(se::submit(prepared(), &mut || {}, None, &mut runtime, std::slice::from_ref(&draw)).unwrap());
    assert_eq!(se::replay_prepared_sampled_resources(None,"observed-borrowed-plan",1,&mut runtime).unwrap(),Some((true,1)));
    assert_eq!(se::upload_bytes(&runtime),uploaded);
    let next_source = se::register_gpu_vertices(&owner, None, None, None, None, None, "observed\nui-next", &device, &producer, 8, Box::new(|| true)).unwrap();
    let mut next = (*draw).clone();
    next.draw.mesh.key = MeshKey::GpuVertices { name: Rc::new("observed\nui-next".into()), indices: match &draw.draw.mesh.key { MeshKey::GpuVertices { indices, .. } => indices.clone(), _ => unreachable!() } };
    assert!(Rc::ptr_eq(&next.draw.mesh.indices,&draw.draw.mesh.indices));
    assert!(se::submit(Some(("observed-borrowed-plan".into(),2)), &mut || {}, None, &mut runtime, &[Rc::new(next)]).unwrap());
    assert_eq!(se::upload_bytes(&runtime),uploaded);
    assert_eq!(seen.borrow().mesh_upload_bytes,48,"registration names must share the host index upload");
    assert_slot(0, producer_token);
    check_pixels(&mut runtime);
    for variant in 0..2 {
        let mut bad = (*draw).clone();
        if variant == 0 {
            bad.draw.mesh.indices = new_bytes(vec![0;47]);
        } else {
            bad.draw.mesh.key = MeshKey::Host("cpu:index-overflow".into());
            bad.draw.mesh.vertices = new_bytes(vec![0;128]);
            bad.draw.mesh.index_count = 1i64 << (isize::BITS-3);
            bad.draw.mesh.indices = se::empty_bytes();
        }
        let mut releases = 0;
        let error = se::submit(None, &mut || releases += 1, None, &mut runtime, &[Rc::new(bad)]).unwrap_err();
        assert_eq!(error.kind,ogpu_core::error::ErrorKind::InvalidArgument,"malformed variant {variant}");
        assert_eq!(releases,1,"malformed variant {variant} release hook");
        assert_eq!(se::upload_bytes(&runtime),uploaded);
        assert_eq!(seen.borrow().mesh_upload_bytes,48);
    }
    // The Marks shader consumes six distinct producer streams at slots 0..5; slot 6 is affine.
    let mut positions = vec![0;128];
    positions[..24].copy_from_slice(&common::floats(&[0.,0.,0.,2.,0.,0.]));
    let contents = [
        positions, common::floats(&[2.,4.,2.,4.]), common::words(&[-1,-1]),
        common::words(&[0]), common::floats(&[1.]), common::words(&[0,3,2,1,0,0,0,0]),
    ];
    let buffers: Vec<_> = contents.iter().enumerate().map(|(slot,bytes)| upload(&format!("observed-marks-{slot}"),bytes)).collect();
    let marks_source = se::register_gpu_vertices(&owner, Some(buffers[1..].to_vec()), None, None, None, None, "observed-marks", &device, &buffers[0], 8, Box::new(|| true)).unwrap();
    let mut marks = (*draw).clone();
    marks.family = PipelineFamily::Marks;
    marks.texture = None;
    marks.draw.mesh.key = MeshKey::GpuVertices { name: Rc::new("observed-marks".into()), indices: None };
    seen.borrow_mut().bindings.clear();
    assert!(se::submit(None, &mut || {}, None, &mut runtime, &[Rc::new(marks)]).unwrap());
    check_pixels(&mut runtime);
    for slot in 0..6 {
        let token = seen.borrow().buffers[&format!("observed-marks-{slot}")];
        assert_slot(slot,token);
    }
    assert!(seen.borrow().bindings.iter().any(|binding| binding.0 == 6),"Marks affine slot 6 missing");
    se::unregister_gpu_vertices(&marks_source);
    se::unregister_gpu_vertices(&next_source);
    se::unregister_gpu_vertices(&source);
    assert!(se::borrows_unregistered(&owner, &borrowed));
    for buffer in buffers.iter().chain(std::iter::once(&producer)) {
        backend::destroy_buffer(buffer).unwrap();
    }
    se::destroy(&mut runtime).unwrap();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(),baseline);
}

pub const CASES: &[(&str, fn())] = &[
        ("gpu_scene3_shared_mesh_and_instances", gpu_scene3_shared_mesh_and_instances),
        ("gpu_scene3_device_indices_and_white", gpu_scene3_device_indices_and_white),
        ("gpu_scene3_resident_colors3", gpu_scene3_resident_colors3),
        ("gpu_scene3_resident_colors4", gpu_scene3_resident_colors4),
        ("gpu_scene3_borrowed_stream_guards", gpu_scene3_borrowed_stream_guards),
        ("gpu_borrowed_vertex_encoder_observation", gpu_borrowed_vertex_encoder_observation),
    ];
