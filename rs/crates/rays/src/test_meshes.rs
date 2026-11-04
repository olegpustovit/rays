//! Meshes the 3D tests draw. The typed generators are gone from the public API (geometry comes
//! from SOP graphs); these build the same shapes through the packed constructor so every test
//! shares one definition.
use crate::mesh;
use rays_math::color;
use rays_math::vec3;

fn planes(values: &[vec3::T]) -> mesh::Vec3_buffer {
    mesh::Vec3_buffer::of_planes(values.iter().map(|p| p.x).collect(), values.iter().map(|p| p.y).collect(), values.iter().map(|p| p.z).collect())
}

pub fn packed(mode: Option<mesh::mode>, colors: Option<Vec<color::T>>, indices: Vec<i64>, normals: &[vec3::T], positions: &[vec3::T]) -> mesh::t {
    mesh::View::create_packed_owned(mode, Some(indices), Some(planes(normals)), colors, None, planes(positions)).expect("packed mesh")
}

/// Every vertex faces +Z.
pub fn facing_z(mode: Option<mesh::mode>, colors: Option<Vec<color::T>>, indices: Vec<i64>, positions: &[vec3::T]) -> mesh::t {
    let normals: Vec<vec3::T> = positions.iter().map(|_| vec3::unit_z()).collect();
    packed(mode, colors, indices, &normals, positions)
}

#[allow(dead_code)]
/// A latitude/longitude sphere about the origin, wound as the other meshes.
pub fn sphere(segments: i64, rings: i64, radius: f64) -> mesh::t {
    let row = segments + 1;
    let normals: Vec<vec3::T> = (0..row * (rings + 1))
        .map(|index| {
            let theta = (index % row) as f64 / segments as f64 * 2. * std::f64::consts::PI;
            let phi = (index / row) as f64 / rings as f64 * std::f64::consts::PI;
            vec3::create(phi.sin() * theta.cos(), phi.cos(), phi.sin() * theta.sin())
        })
        .collect();
    let mut indices = vec![0i64; (segments * rings * 6) as usize];
    for cell in 0..segments * rings {
        let a = (cell / segments * row) + (cell % segments);
        let b = a + 1;
        let d = a + row;
        let c = d + 1;
        indices[(cell * 6) as usize..(cell * 6 + 6) as usize].copy_from_slice(&[a, b, c, a, c, d]);
    }
    let positions: Vec<vec3::T> = normals.iter().map(|n| vec3::scale(*n, radius)).collect();
    packed(None, None, indices, &normals, &positions)
}

/// An axis-aligned box about the origin: four vertices a face, outward normals, counter-clockwise
/// seen from outside.
pub fn box_(width: f64, height: f64, depth: f64) -> mesh::t {
    let half = vec3::create(width / 2., height / 2., depth / 2.);
    let faces = [
        (vec3::unit_x(), vec3::unit_y(), vec3::unit_z()),
        (vec3::neg(vec3::unit_x()), vec3::unit_z(), vec3::unit_y()),
        (vec3::unit_y(), vec3::unit_z(), vec3::unit_x()),
        (vec3::neg(vec3::unit_y()), vec3::unit_x(), vec3::unit_z()),
        (vec3::unit_z(), vec3::unit_x(), vec3::unit_y()),
        (vec3::neg(vec3::unit_z()), vec3::unit_y(), vec3::unit_x()),
    ];
    let corner = |n: vec3::T, u: vec3::T, v: vec3::T, su: f64, sv: f64| {
        let p = vec3::add(n, vec3::add(vec3::scale(u, su), vec3::scale(v, sv)));
        vec3::create(p.x * half.x, p.y * half.y, p.z * half.z)
    };
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut indices = Vec::new();
    for (face, (n, u, v)) in faces.iter().enumerate() {
        positions.extend_from_slice(&[corner(*n, *u, *v, -1., -1.), corner(*n, *u, *v, 1., -1.), corner(*n, *u, *v, 1., 1.), corner(*n, *u, *v, -1., 1.)]);
        normals.extend_from_slice(&[*n; 4]);
        let o = face as i64 * 4;
        indices.extend_from_slice(&[o, o + 1, o + 2, o, o + 2, o + 3]);
    }
    packed(None, None, indices, &normals, &positions)
}
