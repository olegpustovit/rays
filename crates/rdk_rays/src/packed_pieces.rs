//! Disconnected pieces compile immutable membership/center arrays once; explosion only rebuilds render positions and never recooks topology. Invalid controls return Err.
use std::collections::HashMap;
use std::sync::Arc;

use param::{FieldView, Value};
use rays::mesh::{self, Vec3_buffer};
use rays::num::{fmax, fmin};
use rays_math::text_array::TextArray;
use rays_math::{color, noise, vec2, vec3};
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::{error, geometry, topology};
use rdk_ops::triangulate;

use crate::rays_mesh;

#[derive(Clone)]
pub struct T {
    base_mesh: mesh::Mesh,
    base: mesh::view::packed_view,
    piece_of_vertex: Vec<usize>,
    center_x: Vec<f64>,
    center_y: Vec<f64>,
    center_z: Vec<f64>,
    offset_x: Vec<f64>,
    offset_y: Vec<f64>,
    offset_z: Vec<f64>,
}

pub fn piece_count(value: &T) -> i64 {
    value.center_x.len() as i64
}

fn vertex_count(value: &T) -> usize {
    value.piece_of_vertex.len()
}

fn dense_int(values: &[i64]) -> (Vec<usize>, usize) {
    let mut table: HashMap<i64, usize> = HashMap::with_capacity((values.len() / 4).max(16));
    let dense = values
        .iter()
        .map(|value| {
            let next = table.len();
            *table.entry(value.clone()).or_insert(next)
        })
        .collect();
    (dense, table.len())
}

fn dense_text(values: &TextArray) -> (Vec<usize>, usize) {
    let mut table: HashMap<&[u8], usize> = HashMap::with_capacity((values.len() / 4).max(16));
    let dense = values
        .iter()
        .map(|value| {
            let next = table.len();
            *table.entry(value).or_insert(next)
        })
        .collect();
    (dense, table.len())
}

fn primitive_pieces(piece_attribute: &str, g: &geometry::T) -> Result<(Vec<usize>, usize), String> {
    match geometry::find_attribute(Owner::Primitive, piece_attribute, g) {
        None => Err(format!("Packed_pieces: missing primitive piece attribute {piece_attribute:?}")),
        Some(a) => match attribute::view::storage(&a) {
            Ok(Storage::Int(values)) => Ok(dense_int(values)),
            Ok(Storage::Text(values)) => Ok(dense_text(values)),
            _ => Err(format!("Packed_pieces: primitive piece attribute {piece_attribute:?} must be int or text")),
        },
    }
}

fn expand_triangles_preserving_attributes(m: &mesh::Mesh) -> Result<mesh::Mesh, String> {
    let source = mesh::view::packed_view(m)?;
    if source.mode != mesh::mode::Triangles {
        return Err("Packed_pieces: expected a triangulated render mesh".to_owned());
    }
    let count = source.indices.len();
    let select = |plane: &[f64]| -> Vec<f64> { source.indices.iter().map(|&i| plane[i as usize]).collect() };
    let select3 = |values: &Vec3_buffer| Vec3_buffer::of_planes(select(&values.x), select(&values.y), select(&values.z));
    let vertices = select3(&source.vertices);
    let normals = source.normals.as_ref().map(select3);
    let colors: Option<Vec<color::T>> = source.colors.as_ref().map(|c| source.indices.iter().map(|&i| c[i as usize]).collect());
    let tex_coords: Option<Vec<vec2::T>> = source.tex_coords.as_ref().map(|c| source.indices.iter().map(|&i| c[i as usize]).collect());
    mesh::view::create_packed_owned(Some(mesh::mode::Triangles), Some((0..count as i64).collect()), normals, colors, tex_coords, vertices)
}

fn text(e: error::T) -> String {
    error::to_string(&e)
}

pub fn of_geometry(piece_attribute: &str, g: &geometry::T) -> Result<T, String> {
    if piece_attribute.trim().is_empty() {
        return Err("Packed_pieces.of_geometry: empty piece attribute".to_owned());
    }
    let g = if topology::all_triangles(geometry::topology(g)).map_err(text)? {
        g.clone()
    } else {
        triangulate::run(&triangulate::Params { cancel: None, grain: None, primitives: None }, g).map_err(text)?
    };
    let (primitive_piece, pieces) = primitive_pieces(piece_attribute, &g)?;
    if pieces == 0 {
        return Err("Packed_pieces: geometry has no pieces".to_owned());
    }
    let m = rays_mesh::to_mesh(&g).map_err(text)?;
    let expanded = expand_triangles_preserving_attributes(&m)?;
    let base = mesh::view::packed_view(&expanded)?;
    let expected = (geometry::primitive_count(&g) * 3) as usize;
    if base.mode != mesh::mode::Triangles || base.vertices.x.len() != expected {
        return Err("Packed_pieces: triangle render ancestry is inconsistent".to_owned());
    }
    let piece_of_vertex: Vec<usize> = (0..expected).map(|vertex| primitive_piece[vertex / 3]).collect();
    let mut min = [vec![f64::INFINITY; pieces], vec![f64::INFINITY; pieces], vec![f64::INFINITY; pieces]];
    let mut max = [vec![f64::NEG_INFINITY; pieces], vec![f64::NEG_INFINITY; pieces], vec![f64::NEG_INFINITY; pieces]];
    let planes = [&base.vertices.x, &base.vertices.y, &base.vertices.z];
    for (vertex, &piece) in piece_of_vertex.iter().enumerate() {
        for axis in 0..3 {
            let v = planes[axis][vertex];
            min[axis][piece] = fmin(min[axis][piece], v);
            max[axis][piece] = fmax(max[axis][piece], v);
        }
    }
    let center = |axis: usize| -> Vec<f64> { (0..pieces).map(|p| 0.5 * (min[axis][p] + max[axis][p])).collect() };
    let (center_x, center_y, center_z) = (center(0), center(1), center(2));
    let origin = {
        let all_min = |values: &[f64]| values.iter().fold(f64::INFINITY, |a, b| fmin(a, *b));
        let all_max = |values: &[f64]| values.iter().fold(f64::NEG_INFINITY, |a, b| fmax(a, *b));
        vec3::create(
            0.5 * (all_min(&min[0]) + all_max(&max[0])),
            0.5 * (all_min(&min[1]) + all_max(&max[1])),
            0.5 * (all_min(&min[2]) + all_max(&max[2])),
        )
    };
    let offset_x = center_x.iter().map(|v| v - origin.x).collect();
    let offset_y = center_y.iter().map(|v| v - origin.y).collect();
    let offset_z = center_z.iter().map(|v| v - origin.z).collect();
    Ok(T { base_mesh: expanded, base, piece_of_vertex, center_x, center_y, center_z, offset_x, offset_y, offset_z })
}

struct Noise {
    amount: f64,
    frequency: f64,
    seed: i64,
}

/// The mesh with every piece moved by `amount` times its (scaled) offset from the whole's center.
fn mesh_with(value: &T, offsets: [&[f64]; 3], amount: f64, n: &Noise) -> Result<mesh::Mesh, String> {
    if !(amount.is_finite() && n.amount.is_finite() && n.frequency.is_finite()) {
        return Err("Packed_pieces.mesh: controls must be finite".to_owned());
    }
    if amount == 0.0 {
        return Ok(value.base_mesh.clone());
    }
    let generator = noise::create(n.seed);
    let mut scale = Vec::with_capacity(value.center_x.len());
    for piece in 0..value.center_x.len() {
        scale.push(if n.amount == 0.0 {
            1.0
        } else {
            let sample = noise::fbm3(
                Some(3),
                &generator,
                value.center_x[piece] * n.frequency,
                value.center_y[piece] * n.frequency,
                value.center_z[piece] * n.frequency,
            )?;
            f64::max(0.0, 1.0 + n.amount * (2.0 * sample - 1.0))
        });
    }
    let moved = |plane: &[f64], offset: &[f64]| -> Vec<f64> {
        (0..vertex_count(value))
            .map(|vertex| {
                let piece = value.piece_of_vertex[vertex];
                plane[vertex] + amount * scale[piece] * offset[piece]
            })
            .collect()
    };
    let positions = Vec3_buffer::of_planes(
        moved(&value.base.vertices.x, offsets[0]),
        moved(&value.base.vertices.y, offsets[1]),
        moved(&value.base.vertices.z, offsets[2]),
    );
    mesh::view::create_packed_shared(
        Some(value.base.mode),
        Some(Arc::clone(&value.base.indices)),
        value.base.normals.clone(),
        value.base.colors.clone(),
        value.base.tex_coords.clone(),
        positions,
    )
}

#[derive(Clone, Debug, PartialEq)]
pub struct Explosion {
    pub amount: f64,
    pub scale: vec3::T,
    pub piece_attribute: String,
    pub noise_amount: f64,
    pub noise_frequency: f64,
    pub noise_seed: i64,
}

fn explosion_default() -> Explosion {
    Explosion {
        amount: 0.32,
        scale: vec3::create(1.0, 1.0, 1.0),
        piece_attribute: "piece".to_owned(),
        noise_amount: 0.0,
        noise_frequency: 0.8,
        noise_seed: 0,
    }
}

pub fn explosion(operation: &str, fields: &[FieldView]) -> Option<Explosion> {
    if operation != "exploded_view" {
        return None;
    }
    // List.find_map over the fields of that name whose current value has the wanted kind
    let named = |name: &str| fields.iter().filter(move |field| field.name == name).map(|field| &field.current).collect::<Vec<_>>();
    let float = |name: &str, fallback: f64| {
        named(name).into_iter().find_map(|v| if let Value::Float(v) = v { Some(*v) } else { None }).unwrap_or(fallback)
    };
    let int = |name: &str, fallback: i64| {
        named(name).into_iter().find_map(|v| if let Value::Int(v) = v { Some(*v) } else { None }).unwrap_or(fallback)
    };
    let string = |name: &str, fallback: &str| {
        match named(name).into_iter().find_map(|v| if let Value::Text(v) = v { Some(v) } else { None }) {
            Some(bytes) => std::str::from_utf8(bytes).ok().map(str::to_owned),
            None => Some(fallback.to_owned()),
        }
    };
    if !fields.iter().any(|field| field.name == "amount") {
        return None;
    }
    let d = explosion_default();
    Some(Explosion {
        amount: float("amount", d.amount),
        scale: vec3::create(float("scale_x", d.scale.x), float("scale_y", d.scale.y), float("scale_z", d.scale.z)),
        piece_attribute: string("piece_attribute", &d.piece_attribute)?,
        noise_amount: float("noise_amount", d.noise_amount),
        noise_frequency: float("noise_frequency", d.noise_frequency),
        noise_seed: int("noise_seed", d.noise_seed),
    })
}

pub fn mesh_for_explosion(parameters: Option<&Explosion>, value: &T) -> Result<mesh::Mesh, String> {
    let Some(p) = parameters else { return Ok(value.base_mesh.clone()) };
    let scaled = |offset: &[f64], by: f64| -> Vec<f64> { offset.iter().map(|v| by * v).collect() };
    let (ox, oy, oz) = (scaled(&value.offset_x, p.scale.x), scaled(&value.offset_y, p.scale.y), scaled(&value.offset_z, p.scale.z));
    mesh_with(value, [&ox, &oy, &oz], p.amount, &Noise { amount: p.noise_amount, frequency: p.noise_frequency, seed: p.noise_seed })
}
