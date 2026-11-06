//! Mesh generators for the path tracer tests. The OCaml tests build their geometry with
//! rdk_test_support (Box_generator, Uv_sphere) through Rdk_rays.Rays_mesh.to_trace_mesh, whose Rust
//! twin has no test-support port yet: these produce the same shapes as triangle-ready Rays meshes with
//! per-vertex normals (a box with flat faces, a UV sphere with smooth normals).
#![allow(dead_code)]
use rays::mesh::{self, View};
use rays_math::{mat4, vec3};

fn build(positions: &[vec3::T], normals: &[vec3::T], indices: Vec<i64>) -> mesh::t {
    let planes = |list: &[vec3::T]| mesh::Vec3_buffer::of_planes(list.iter().map(|v| v.x).collect(), list.iter().map(|v| v.y).collect(), list.iter().map(|v| v.z).collect());
    View::create_packed_owned(None, Some(indices), Some(planes(normals)), None, None, planes(positions)).expect("mesh")
}

pub fn box_mesh(center: vec3::T, size: vec3::T) -> mesh::t {
    let (hx, hy, hz) = (size.x / 2., size.y / 2., size.z / 2.);
    // face normal, then the face's u and v axes (counter-clockwise seen from outside)
    let faces = [
        (vec3::create(1., 0., 0.), vec3::create(0., 1., 0.), vec3::create(0., 0., 1.)),
        (vec3::create(-1., 0., 0.), vec3::create(0., 0., 1.), vec3::create(0., 1., 0.)),
        (vec3::create(0., 1., 0.), vec3::create(0., 0., 1.), vec3::create(1., 0., 0.)),
        (vec3::create(0., -1., 0.), vec3::create(1., 0., 0.), vec3::create(0., 0., 1.)),
        (vec3::create(0., 0., 1.), vec3::create(1., 0., 0.), vec3::create(0., 1., 0.)),
        (vec3::create(0., 0., -1.), vec3::create(0., 1., 0.), vec3::create(1., 0., 0.)),
    ];
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut indices = Vec::new();
    for (n, u, v) in faces {
        let base = positions.len() as i64;
        for (a, b) in [(-1., -1.), (1., -1.), (1., 1.), (-1., 1.)] {
            positions.push(vec3::create(
                center.x + hx * (n.x + a * u.x + b * v.x),
                center.y + hy * (n.y + a * u.y + b * v.y),
                center.z + hz * (n.z + a * u.z + b * v.z),
            ));
            normals.push(n);
        }
        indices.extend_from_slice(&[base, base + 1, base + 2, base, base + 2, base + 3]);
    }
    build(&positions, &normals, indices)
}

pub fn uv_sphere(center: vec3::T, segments: i64, rings: i64, radius: f64) -> mesh::t {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    for r in 0..=rings {
        let phi = std::f64::consts::PI * r as f64 / rings as f64;
        for s in 0..=segments {
            let theta = 2. * std::f64::consts::PI * s as f64 / segments as f64;
            let n = vec3::create(phi.sin() * theta.cos(), phi.cos(), phi.sin() * theta.sin());
            positions.push(vec3::add(center, vec3::scale(n, radius)));
            normals.push(n);
        }
    }
    let mut indices = Vec::new();
    let row = segments + 1;
    for r in 0..rings {
        for s in 0..segments {
            let a = r * row + s;
            let b = a + row;
            // outward counter-clockwise
            indices.extend_from_slice(&[a, a + 1, b, a + 1, b + 1, b]);
        }
    }
    build(&positions, &normals, indices)
}

/// `Transform_ops_front.transform`: positions by the matrix, normals by its inverse transpose.
pub fn transform(matrix: &mat4::T, source: &mesh::t) -> mesh::t {
    let view = View::packed_view(source).expect("view");
    let normal_matrix = mat4::transpose(&mat4::inverse(matrix).expect("invertible"));
    let positions: Vec<vec3::T> = (0..view.vertices.length() as usize).map(|i| mat4::transform_point(matrix, view.vertices.get(i))).collect();
    let normals: Vec<vec3::T> = (0..view.vertices.length() as usize).map(|i| vec3::normalize(mat4::transform_direction(&normal_matrix, view.normals.as_ref().expect("normals").get(i)))).collect();
    build(&positions, &normals, (*view.indices).clone())
}
