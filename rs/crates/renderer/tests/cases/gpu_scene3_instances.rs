#![allow(non_camel_case_types)]
use crate::common;
use crate::common::get;
use ogpu::backend::{self, Buffer};
use ogpu::error;
use ogpu::pipeline::Blend;
use ogpu::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu::types::{BufferDescriptor, BufferUsage};
use ::renderer::renderer::{self, family, t};
use ::renderer::scene_execution::{self, geometry_draw, mesh, mesh_key, new_bytes, sampled_draw, state};
use std::rc::Rc;

fn native<A>(result: Result<A, error::Error>) -> A {
    result.unwrap_or_else(|e| panic!("{}", error::to_string(&e)))
}

fn config() -> renderer::configuration {
    renderer::configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, ..renderer::default_configuration() }
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
    mesh: mesh,
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
    let mesh = mesh {
        key: mesh_key::Host("scene3-shared-mesh".to_owned()),
        vertices: new_bytes(vertices.clone()),
        vertex_count: 3,
        indices: new_bytes(indices.clone()),
        index_count: 3,
        primitive: Primitive::TriangleList,
    };
    fixture { vertices, indices, attributes, mesh }
}

fn entry(f: &fixture, scissor: (i64, i64, i64, i64), uniforms: Vec<u8>) -> sampled_draw {
    sampled_draw {
        family: family::Scene3,
        blend: Blend::Replace,
        texture: None,
        auxiliary: None,
        vertex_attributes: Some(("test-attributes".to_owned(), new_bytes(f.attributes.clone()))),
        samples: 1,
        draw: geometry_draw {
            mesh: f.mesh.clone(),
            state: state {
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

fn render(runtime: &t, entries: Vec<sampled_draw>) -> Vec<u8> {
    let draws: Vec<Rc<sampled_draw>> = entries.into_iter().map(Rc::new).collect();
    get(renderer::draw(None, runtime, &draws));
    get(renderer::read_pixels(runtime, 256))
}

fn gpu_scene3_shared_mesh_and_instances() {
    if !common::gpu() {
        return;
    }
    let runtime = get(renderer::create_offscreen(&config()));
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
        e.family = if *primitive == Primitive::PointList { family::Scene3_points } else { family::Scene3 };
        e.draw.mesh = mesh {
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
    get(renderer::destroy(&runtime));
    println!("Scene3 shared-mesh and instances passed");
}

struct resident {
    runtime: t,
    host: t,
    gpu: renderer::gpu,
    positions: Buffer,
    device_indices: Buffer,
    key: String,
    unregister: Box<dyn Fn()>,
    f: fixture,
}

fn resident() -> resident {
    let f = fixture();
    let host = get(renderer::create_offscreen(&config()));
    let gpu = get(renderer::acquire_gpu());
    let runtime = get(renderer::Native::create_offscreen_with_gpu(&gpu, &config()));
    let device = renderer::gpu_device(&gpu);
    let positions = native(backend::create_buffer(
        &device,
        BufferDescriptor { label: Some("indexed-test-P".to_owned()), size: 36, usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst] },
        None,
    ));
    let device_indices = native(backend::create_buffer(
        &device,
        BufferDescriptor { label: Some("indexed-test-indices".to_owned()), size: 12, usage: vec![BufferUsage::Index, BufferUsage::Storage, BufferUsage::CopyDst] },
        None,
    ));
    let mut position_bytes = vec![0u8; 36];
    for i in 0..3 {
        position_bytes[i * 12..i * 12 + 12].copy_from_slice(&f.vertices[i * 24..i * 24 + 12]);
    }
    native(backend::write_buffer(&positions, 0, &position_bytes));
    native(backend::write_buffer(&device_indices, 0, &f.indices));
    let (key, unregister) = get(renderer::register_gpu_positions(&gpu, &positions, None, Some((device_indices.clone(), 3)), None, 3, Rc::new(|| true)));
    resident { runtime, host, gpu, positions, device_indices, key, unregister, f }
}

fn finish(r: resident, color_buffers: Vec<Buffer>) {
    (r.unregister)();
    get(renderer::destroy(&r.runtime));
    for buffer in color_buffers.iter() {
        native(backend::destroy_buffer(buffer));
    }
    native(backend::destroy_buffer(&r.device_indices));
    native(backend::destroy_buffer(&r.positions));
    renderer::release_gpu(&r.gpu);
    get(renderer::destroy(&r.host));
}

fn device_entry(r: &resident, key: String, uniforms: Vec<u8>, attributes: &str, host_bytes: Vec<u8>) -> sampled_draw {
    let mut e = entry(&r.f, (0, 0, 4, 4), uniforms);
    e.vertex_attributes = Some((attributes.to_owned(), new_bytes(host_bytes)));
    e.draw.mesh = mesh { key: mesh_key::Gpu_vertices(key), vertices: scene_execution::empty_bytes(), indices: scene_execution::empty_bytes(), ..r.f.mesh.clone() };
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
    let color_buffer = native(backend::create_buffer(
        &device,
        BufferDescriptor { label: Some("indexed-test-Cd".to_owned()), size: color_bytes.len() as i64, usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst] },
        None,
    ));
    native(backend::write_buffer(&color_buffer, 0, &color_bytes));
    let (color_key, unregister_color) = get(renderer::register_gpu_positions(
        &r.gpu,
        &r.positions,
        None,
        Some((r.device_indices.clone(), 3)),
        Some((color_buffer.clone(), width as i64)),
        3,
        Rc::new(|| true),
    ));
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

pub const CASES: &[(&str, fn())] = &[
        ("gpu_scene3_shared_mesh_and_instances", gpu_scene3_shared_mesh_and_instances),
        ("gpu_scene3_device_indices_and_white", gpu_scene3_device_indices_and_white),
        ("gpu_scene3_resident_colors3", gpu_scene3_resident_colors3),
        ("gpu_scene3_resident_colors4", gpu_scene3_resident_colors4),
    ];
