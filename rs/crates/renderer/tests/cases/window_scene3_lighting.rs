#![allow(non_camel_case_types)]
use crate::common;
use ogpu::error::ErrorKind;
use ogpu::pipeline::Blend;
use ogpu::render_pass::{Comparison, Cull, Load, Primitive};
use ::renderer::renderer::{self, family, t};
use ::renderer::scene_execution::{geometry_draw, mesh, mesh_key, new_bytes, sampled_draw, state};
use std::rc::Rc;

fn set(bytes: &mut [u8], index: usize, value: f32) {
    bytes[index * 4..index * 4 + 4].copy_from_slice(&value.to_le_bytes());
}

type rgba = (f64, f64, f64, f64);

fn uniform(ambient: rgba, diffuse: rgba, global: rgba, light: Option<([f64; 3], rgba, f64)>) -> Vec<u8> {
    let mut values = vec![0f64; 1364];
    for matrix in 0..3 {
        for diagonal in 0..4 {
            values[matrix * 16 + diagonal * 5] = 1.0;
        }
    }
    values[50] = 1.0;
    let color = |values: &mut Vec<f64>, offset: usize, c: rgba| {
        values[offset] = c.0;
        values[offset + 1] = c.1;
        values[offset + 2] = c.2;
        values[offset + 3] = c.3;
    };
    color(&mut values, 52, ambient);
    color(&mut values, 56, diffuse);
    color(&mut values, 60, (0.0, 0.0, 0.0, 1.0));
    color(&mut values, 64, (0.0, 0.0, 0.0, 1.0));
    values[68] = 16.0;
    color(&mut values, 69, global);
    if let Some((direction, color_value, intensity)) = light {
        values[73] = 1.0;
        values[85] = direction[0];
        values[86] = direction[1];
        values[87] = direction[2];
        color(&mut values, 88, color_value);
        values[92] = intensity;
    }
    let mut bytes = vec![0u8; 5456];
    for (index, value) in values.iter().enumerate() {
        set(&mut bytes, index, *value as f32);
    }
    bytes
}

fn window_scene3_lighting() {
    if !common::gpu() {
        return;
    }
    let runtime: t = match renderer::create(&renderer::configuration {
        logical_width: 4,
        logical_height: 4,
        drawable_width: 4,
        drawable_height: 4,
        ..renderer::default_configuration()
    }) {
        Ok(r) => r,
        Err(_) => {
            println!("renderer Scene3 lighting: skipped");
            return;
        }
    };
    let mut indices = vec![0u8; 12];
    let mut vertices = vec![0u8; 24 * 3];
    let mut attributes = vec![0u8; 12 * 3];
    indices[4..8].copy_from_slice(&1u32.to_le_bytes());
    indices[8..12].copy_from_slice(&2u32.to_le_bytes());
    for (index, (x, y)) in [(-1.0f32, -1.0f32), (3.0, -1.0), (-1.0, 3.0)].iter().enumerate() {
        let offset = index * 24;
        vertices[offset..offset + 4].copy_from_slice(&x.to_le_bytes());
        vertices[offset + 4..offset + 8].copy_from_slice(&y.to_le_bytes());
        vertices[offset + 20..offset + 24].copy_from_slice(&1.0f32.to_le_bytes());
        attributes[index * 12..index * 12 + 4].copy_from_slice(&0xffffffffu32.to_le_bytes());
    }
    let mesh_ = mesh {
        key: mesh_key::Host("runtime-scene3-light".to_owned()),
        vertices: new_bytes(vertices),
        vertex_count: 3,
        indices: new_bytes(indices),
        index_count: 3,
        primitive: Primitive::TriangleList,
    };
    let entry = |uniforms: Vec<u8>| -> Rc<sampled_draw> {
        Rc::new(sampled_draw {
            family: family::Scene3,
            blend: Blend::Replace,
            texture: None,
            auxiliary: None,
            vertex_attributes: Some(("test-attributes".to_owned(), new_bytes(attributes.clone()))),
            samples: 1,
            draw: geometry_draw {
                mesh: mesh_.clone(),
                state: state {
                    viewport: (0, 0, 4, 4),
                    scissor: (0, 0, 4, 4),
                    cull: Cull::CullNone,
                    depth_compare: Comparison::Always,
                    depth_write: false,
                    depth_load: Load::Clear,
                    depth_clear: 1.0,
                    transform_uniforms: Some(new_bytes(uniforms)),
                },
            },
        })
    };
    let drawable_width = renderer::presentation_facts_of(&runtime).unwrap().drawable_width;
    let check = |name: &str, uniforms: Vec<u8>, expected: (i64, i64, i64)| {
        let mut stable: Option<i64> = None;
        for _frame in [1, 2, 60, 600] {
            renderer::draw(None, &runtime, &[entry(uniforms.clone())]).unwrap();
            let bytes = renderer::read_pixels(&runtime, drawable_width * 4).unwrap();
            let actual = (bytes[0] as i64, bytes[1] as i64, bytes[2] as i64);
            let near = |a: (i64, i64, i64), b: (i64, i64, i64)| (a.0 - b.0).abs() <= 1 && (a.1 - b.1).abs() <= 1 && (a.2 - b.2).abs() <= 1;
            assert!(near(actual, expected), "{} pixel {},{},{}", name, actual.0, actual.1, actual.2);
            let uploaded = renderer::stats(&runtime).unwrap().uploaded_bytes;
            match stable {
                None => stable = Some(uploaded),
                Some(previous) => assert!(previous == uploaded, "{} reuploaded", name),
            }
        }
    };
    check("ambient", uniform((1.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 1.0), (1.0, 1.0, 1.0, 1.0), None), (255, 0, 0));
    check(
        "directional",
        uniform((0.0, 0.0, 0.0, 1.0), (1.0, 1.0, 1.0, 1.0), (0.0, 0.0, 0.0, 1.0), Some(([0.0, 0.0, -1.0], (0.0, 1.0, 0.0, 1.0), 1.0))),
        (0, 255, 0),
    );
    let set64 = |bytes: &mut Vec<u8>, index: usize, value: f64| set(bytes, index, value as f32);
    let positional = |kind: f64, color: rgba| -> Vec<u8> {
        let mut bytes = uniform((0.0, 0.0, 0.0, 1.0), (1.0, 1.0, 1.0, 1.0), (0.0, 0.0, 0.0, 1.0), Some(([0.0, 0.0, -1.0], color, 1.0)));
        set64(&mut bytes, 84, kind);
        set64(&mut bytes, 85, 0.0);
        set64(&mut bytes, 86, 0.0);
        set64(&mut bytes, 87, 1.0);
        bytes
    };
    // Positional lights depend on where pixel (0,0) lands: its centre is at world (x,y,0) with
    // x=-1+1/width; the expected channels follow the shader for that centre with the light at
    // (0,0,1), unit attenuation and normal +z (specular is black).
    let facts = renderer::presentation_facts_of(&runtime).unwrap();
    let x = -1.0 + 1.0 / facts.drawable_width as f64;
    let y = 1.0 - 1.0 / facts.drawable_height as f64;
    let byte = |value: f64| (255.0 * value.max(0.0).min(1.0)).round() as i64;
    let inverse_length = 1.0 / (x * x + y * y + 1.0).sqrt();
    let area_diffuse = {
        // 2x2 samples over a unit square facing -z: u=(-1,0,0), w=(0,1,0).
        let (mut wx, mut wy, mut wz, mut total) = (0.0f64, 0.0f64, 0.0f64, 0.0f64);
        for (a, b) in [(-0.25f64, -0.25f64), (0.25, -0.25), (-0.25, 0.25), (0.25, 0.25)] {
            let (dx, dy, dz) = (-a - x, b - y, 1.0f64);
            let distance = (dx * dx + dy * dy + dz * dz).sqrt();
            let weight = dz / distance / 4.0;
            wx += dx / distance * weight;
            wy += dy / distance * weight;
            wz += dz / distance * weight;
            total += weight;
        }
        wz / (wx * wx + wy * wy + wz * wz).sqrt() * total
    };
    let mut point = positional(1.0, (1.0, 0.0, 0.0, 1.0));
    set64(&mut point, 93, 1.0);
    check("point", point, (byte(inverse_length), 0, 0));
    let mut spot = positional(2.0, (0.0, 0.0, 1.0, 1.0));
    for (i, v) in [(93, 0.0), (94, 0.0), (95, -1.0), (96, 1.0), (97, 0.5), (98, 1.0), (99, 1.0)] {
        set64(&mut spot, i, v);
    }
    check("spot", spot, (0, 0, byte(inverse_length * inverse_length)));
    let mut area = positional(3.0, (1.0, 0.0, 0.0, 1.0));
    for (i, v) in [(93, 0.0), (94, 0.0), (95, -1.0), (96, 1.0), (97, 1.0), (98, 4.0), (99, 1.0)] {
        set64(&mut area, i, v);
    }
    check("area", area, (byte(area_diffuse), 0, 0));
    let mut too_many = uniform((1.0, 1.0, 1.0, 1.0), (0.0, 0.0, 0.0, 1.0), (1.0, 1.0, 1.0, 1.0), None);
    set64(&mut too_many, 73, 65.0);
    let before_reject = renderer::stats(&runtime).unwrap().uploaded_bytes;
    match renderer::draw(None, &runtime, &[entry(too_many)]) {
        Err(e) if e.kind == ErrorKind::InvalidArgument && renderer::stats(&runtime).unwrap().uploaded_bytes == before_reject => {}
        _ => panic!("65-light uniforms were not rejected before upload"),
    }
    renderer::destroy(&runtime).unwrap();
    println!("runtime Scene3 lighting: ambient/directional frames1/2/60/600");
}

pub const CASES: &[(&str, fn())] = &[("window_scene3_lighting", window_scene3_lighting)];
