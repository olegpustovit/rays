//! Port of lib/rays_pathtracer/mesh_data.ml: prepared geometry (flat, instanced or a scene of
//! several placed meshes), pure data that a cook worker may build off the initial domain.
//! The OCaml `'material t` is concrete here: the only instantiation is `material`.
use crate::frame_data::{put_f32, put_u32};
use crate::rays_pathtracer::Material;
use ogpu::backend as b;
use rays_math::mat4;
use std::sync::Arc;

const MAX_INT: i64 = (1i64 << 62) - 1;

fn add_f32(builder: &mut Vec<u8>, value: f64) {
    builder.extend_from_slice(&(value as f32).to_le_bytes());
}

pub fn add_vec3(builder: &mut Vec<u8>, (x, y, z): (f64, f64, f64)) {
    add_f32(builder, x);
    add_f32(builder, y);
    add_f32(builder, z);
}

#[derive(Clone)]
pub struct Instances {
    pub records: Vec<b::InstanceRecord>,
    pub shader_bytes: Vec<u8>,
    pub motion_transforms: Option<Vec<Vec<f64>>>,
    pub matrices: Vec<mat4::T>,
    pub motion_end: Option<Vec<mat4::T>>,
}

/// A scene is a TLAS over several BLASes, one per part and one geometry kind each (Metal cannot mix
/// kinds in one BLAS). A placement is one TLAS instance: its world matrix, its part, and its material
/// (-1: the per-triangle ids). The shader table holds, per instance, the part's first triangle in the
/// shared vertex buffer and that material. A `scene_mesh` layout also keeps, for `move`, each object's
/// placement range (`objects[o]` to `objects[o + 1]`; empty for `mesh`) and each placement's matrix
/// relative to its object.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Part {
    /// first vertex, vertex count
    TrianglePart(i64, i64),
}

#[derive(Clone)]
pub struct Layout {
    pub parts: Vec<Part>,
    pub placements: Vec<(mat4::T, i64, i64)>,
    pub table: Vec<u8>,
    pub objects: Vec<i64>,
    /// each object's world matrix
    pub placed: Vec<mat4::T>,
    pub locals: Vec<mat4::T>,
}

/// Prepared analytic primitives: spheres as (centre, radius) float4s whose materials start at
/// `sphere_material`, strands as linear round curves (control points, radii, each segment's first point
/// and material).
#[derive(Clone)]
pub struct Extras {
    pub sphere_bytes: Vec<u8>,
    pub sphere_material: i64,
    pub sphere_count: i64,
    pub points: Vec<u8>,
    pub radii: Vec<u8>,
    pub indices: Vec<u8>,
    pub ids: Vec<u8>,
    pub point_count: i64,
    pub segments: i64,
}

pub fn no_extras() -> Extras {
    Extras {
        sphere_bytes: vec![],
        sphere_material: 0,
        sphere_count: 0,
        points: vec![],
        radii: vec![],
        indices: vec![],
        ids: vec![],
        point_count: 0,
        segments: 0,
    }
}

/// A device mesh's triangles (`Rays.Mesh.View.create_device`): the registration `key` names its packed
/// float3 positions and normals on the device (and colours, when `colored`), `index_count` its corners:
/// `corners` the host's indices (kept by identity), empty when the device holds them too, `holder` the
/// mesh that keeps them alive. The host arrays of the mesh it is part of stay empty: the tracer gathers
/// the corners on the device.
#[derive(Clone)]
pub struct Device {
    pub key: String,
    pub vertex_count: i64,
    pub index_count: i64,
    pub corners: Arc<Vec<i64>>,
    pub colored: bool,
    pub holder: rays::mesh::t,
}

#[derive(Clone)]
pub struct MeshData {
    pub device: Option<Device>,
    pub positions: Vec<u8>,
    pub normals: Vec<u8>,
    /// a float3 per corner like the normals; empty: no mesh has colours
    pub colors: Vec<u8>,
    pub ids: Vec<u8>,
    pub triangles: i64,
    pub materials: Vec<Material>,
    pub instances: Option<Instances>,
    pub extras: Extras,
    pub scene: Option<Layout>,
}

type FlatParts = (Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>, i64);

fn flatten(extras: &Extras, objects: &[(rays::mesh::t, i64)]) -> Result<FlatParts, String> {
    let mut positions = Vec::with_capacity(4096);
    let mut normals = Vec::with_capacity(4096);
    let mut colors = Vec::with_capacity(4096);
    let mut ids = Vec::with_capacity(1024);
    let mut count = 0i64;
    let mut colored = false;
    for (source, _) in objects {
        if rays::mesh::View::packed_view(source)?.colors.is_some() {
            colored = true;
            break;
        }
    }
    for (source_mesh, material_index) in objects {
        let view = rays::mesh::View::packed_view(source_mesh)?;
        if view.mode != rays::mesh::mode::Triangles {
            return Err("path tracer meshes must be triangle-ready".to_string());
        }
        let normals_view = match &view.normals {
            Some(n) => n.clone(),
            None => return Err("path tracer meshes need prepared vertex normals".to_string()),
        };
        if view.indices.len() % 3 != 0 {
            return Err("path tracer mesh indices must contain complete triangles".to_string());
        }
        let component = |plane: &rays::mesh::vec3_view, index: usize| (plane.x[index], plane.y[index], plane.z[index]);
        let mut id_bytes = vec![0u8; 4];
        put_u32(&mut id_bytes, 0, *material_index);
        for triangle in 0..view.indices.len() / 3 {
            for corner in 0..3 {
                let vertex = view.indices[triangle * 3 + corner] as usize;
                add_vec3(&mut positions, component(&view.vertices, vertex));
                add_vec3(&mut normals, component(&normals_view, vertex));
                if colored {
                    add_vec3(
                        &mut colors,
                        match &view.colors {
                            Some(c) => {
                                let (r, g, b, _) = rays_math::color::to_floats(c[vertex]);
                                (r, g, b)
                            }
                            None => (1., 1., 1.),
                        },
                    );
                }
            }
            ids.extend_from_slice(&id_bytes);
            count += 1;
        }
    }
    if count == 0 && extras.sphere_count == 0 && extras.segments == 0 {
        Err("path tracer scene has no triangles".to_string())
    } else {
        Ok((positions, normals, colors, ids, count))
    }
}

pub fn flat_mesh(extras: Extras, materials: Vec<Material>, source_meshes: &[rays::mesh::t]) -> Result<MeshData, String> {
    let objects: Vec<(rays::mesh::t, i64)> = source_meshes.iter().cloned().enumerate().map(|(i, m)| (m, i as i64)).collect();
    let (mesh_positions, mesh_normals, mesh_colors, mesh_ids, triangles) = flatten(&extras, &objects)?;
    Ok(MeshData { device: None, positions: mesh_positions, normals: mesh_normals, colors: mesh_colors, ids: mesh_ids, triangles, materials, instances: None, extras, scene: None })
}

/// Column-major 3x4 transform plus its 3x3 normal matrix (cofactors / det): seven packed float3
/// columns, 84 bytes.
pub fn transform_columns(matrix: &mat4::T) -> Result<(Vec<f64>, Vec<f64>), String> {
    let values: Vec<f64> = (0..16).map(|index| mat4::get(matrix, index / 4, index % 4).unwrap()).collect();
    let (a, b_, c, d, e, f, g, h, i) = (values[0], values[1], values[2], values[4], values[5], values[6], values[8], values[9], values[10]);
    let cofactors = [e * i - f * h, c * h - b_ * i, b_ * f - c * e, f * g - d * i, a * i - c * g, c * d - a * f, d * h - e * g, b_ * g - a * h, a * e - b_ * d];
    let determinant = a * cofactors[0] + b_ * cofactors[3] + c * cofactors[6];
    let finite = |value: f64| value.is_finite() && value.abs() <= 3.402823466e38;
    if !values.iter().all(|v| finite(*v))
        || values[12].abs() > 1e-6
        || values[13].abs() > 1e-6
        || values[14].abs() > 1e-6
        || (values[15] - 1.).abs() > 1e-6
        || !determinant.is_finite()
        || determinant == 0.
    {
        Err("path tracer instance transform must be finite, affine, and nonsingular".to_string())
    } else {
        let normal: Vec<f64> = cofactors.iter().map(|v| v / determinant).collect();
        if !normal.iter().all(|v| finite(*v)) {
            Err("path tracer instance normal transform exceeds float32 range".to_string())
        } else {
            Ok((values[0..12].to_vec(), normal))
        }
    }
}

pub fn put_transform_record(bytes: &mut [u8], base: usize, transform: &[f64], normal: &[f64]) {
    for column in 0..4 {
        for row in 0..3 {
            put_f32(bytes, base + (column * 3 + row) * 4, transform[row * 4 + column]);
        }
    }
    for column in 0..3 {
        for row in 0..3 {
            put_f32(bytes, base + 48 + (column * 3 + row) * 4, normal[column * 3 + row]);
        }
    }
}

fn blank_record() -> b::InstanceRecord {
    b::InstanceRecord { instance: b::Instance { transform: vec![], mask: 0, structure_index: 0 }, user_id: 0 }
}

/// Packs placements into TLAS records (structure index = part), shader transform records, and the
/// per-instance (first triangle, material) table.
pub fn layout_mesh(objects: Vec<i64>, placed: Vec<mat4::T>, locals: Vec<mat4::T>, base: MeshData, parts: Vec<Part>, placements: Vec<(mat4::T, i64, i64)>) -> Result<MeshData, String> {
    let count = placements.len();
    let mut records = vec![blank_record(); count];
    let mut shader_bytes = vec![0u8; count * 84];
    let mut table = vec![0u8; count * 8];
    for index in 0..count {
        let (matrix, part_index, material) = &placements[index];
        let columns = transform_columns(matrix)?;
        records[index] = b::InstanceRecord { instance: b::Instance { transform: columns.0.clone(), mask: 0xFFFF_FFFF, structure_index: *part_index }, user_id: 0 };
        put_transform_record(&mut shader_bytes, index * 84, &columns.0, &columns.1);
        let Part::TrianglePart(first, _) = parts[*part_index as usize];
        put_u32(&mut table, index * 8, first / 3);
        put_u32(&mut table, index * 8 + 4, *material);
    }
    let matrices = placements.iter().map(|(m, _, _)| m.clone()).collect();
    Ok(MeshData {
        instances: Some(Instances { records, shader_bytes, motion_transforms: None, motion_end: None, matrices }),
        scene: Some(Layout { parts, placements, table, objects, placed, locals }),
        ..base
    })
}

/// Spheres and strands are traced beside the triangle structure (scene/trace), which then needs a
/// triangle: a degenerate one, never hit, stands in when there is none.
pub fn mesh(extras: Extras, materials: Vec<Material>, source_meshes: &[rays::mesh::t]) -> Result<MeshData, String> {
    let flat = flat_mesh(extras, materials, source_meshes)?;
    if flat.triangles == 0 {
        Ok(MeshData { positions: vec![0; 36], normals: vec![0; 36], colors: vec![], ids: vec![0; 4], ..flat })
    } else {
        Ok(flat)
    }
}

pub fn pack_instances(materials: usize, motion: Option<&[mat4::T]>, transforms: &[mat4::T]) -> Result<Instances, String> {
    let count = transforms.len();
    let stride = if motion.is_none() { 84 } else { 168 };
    let mut records = vec![blank_record(); count];
    let mut shader_bytes = vec![0u8; count * stride];
    let mut motion_transforms: Vec<Vec<f64>> = vec![vec![]; 2 * count];
    for instance in 0..count {
        let start = transform_columns(&transforms[instance])?;
        let finish = match motion {
            None => start.clone(),
            Some(ends) => transform_columns(&ends[instance])?,
        };
        records[instance] = b::InstanceRecord {
            instance: b::Instance { transform: start.0.clone(), mask: 0xFFFF_FFFF, structure_index: 0 },
            user_id: if materials == 0 { 0 } else { 1 + instance as i64 },
        };
        put_transform_record(&mut shader_bytes, instance * stride, &start.0, &start.1);
        if motion.is_some() {
            put_transform_record(&mut shader_bytes, instance * stride + 84, &finish.0, &finish.1);
        }
        motion_transforms[2 * instance] = start.0;
        motion_transforms[2 * instance + 1] = finish.0;
    }
    Ok(Instances {
        records,
        shader_bytes,
        matrices: transforms.to_vec(),
        motion_end: motion.map(|m| m.to_vec()),
        motion_transforms: if motion.is_none() { None } else { Some(motion_transforms) },
    })
}

pub fn mesh_instanced(prototype: &MeshData, materials: Option<&[Material]>, motion: Option<&[mat4::T]>, transforms: &[mat4::T]) -> Result<MeshData, String> {
    let materials = materials.unwrap_or(&[]);
    if prototype.scene.is_some() || prototype.instances.is_some() || prototype.device.is_some() || prototype.extras.sphere_count != 0 || prototype.extras.segments != 0 {
        return Err("path tracer instancing needs one prepared triangle mesh".to_string());
    }
    let count = transforms.len() as i64;
    if count == 0 {
        Err("path tracer instancing needs at least one transform".to_string())
    } else if prototype.triangles > MAX_INT / count {
        Err("path tracer instanced triangle count overflows".to_string())
    } else if count > MAX_INT / 168 {
        Err("path tracer transform storage overflows".to_string())
    } else if !materials.is_empty() && materials.len() as i64 != count {
        Err("path tracer instance materials must match the transform count".to_string())
    } else if motion.map(|ends| ends.len() as i64 != count).unwrap_or(false) {
        Err("path tracer motion transforms must match the transform count".to_string())
    } else {
        let packed = pack_instances(materials.len(), motion, transforms)?;
        let mut mesh_materials = prototype.materials.clone();
        mesh_materials.extend(materials.iter().cloned());
        Ok(MeshData { triangles: prototype.triangles * count, materials: mesh_materials, instances: Some(packed), ..prototype.clone() })
    }
}

/// Without `Ray_tracing_motion` a motion mesh is built at the shutter midpoint: the mean of its start
/// and end transforms, as an ordinary instanced mesh.
pub fn shutter_midpoint(m: &MeshData) -> Result<MeshData, String> {
    match &m.instances {
        Some(instances) if instances.motion_end.is_some() => {
            let ends = instances.motion_end.as_ref().unwrap();
            let mid = |a: &mat4::T, b_: &mat4::T| {
                let g = |r: i64, c: i64| (mat4::get(a, r, c).unwrap() + mat4::get(b_, r, c).unwrap()) / 2.;
                let row = |r: i64| (g(r, 0), g(r, 1), g(r, 2), g(r, 3));
                mat4::of_rows(row(0), row(1), row(2), row(3))
            };
            let used = instances.records.iter().any(|r| r.user_id != 0);
            let mids: Vec<mat4::T> = instances.matrices.iter().enumerate().map(|(i, a)| mid(a, &ends[i])).collect();
            let packed = pack_instances(if used { 1 } else { 0 }, None, &mids)?;
            Ok(MeshData { instances: Some(packed), ..m.clone() })
        }
        _ => Ok(m.clone()),
    }
}

type InputEntry = (mat4::T, MeshData, Vec<Part>, Vec<(mat4::T, i64, i64)>);

pub fn scene_mesh(objects: &[(mat4::T, MeshData)]) -> Result<MeshData, String> {
    // Each input as its parts and placements in its own index spaces.
    let mut inputs: Vec<InputEntry> = Vec::new();
    for (matrix, m) in objects {
        let whole = vec![Part::TrianglePart(0, m.positions.len() as i64 / 12)];
        if m.device.is_some() {
            return Err("path tracer scene_mesh objects are host meshes".to_string());
        }
        if m.extras.sphere_count > 0 || m.extras.segments > 0 {
            // ponytail: spheres and strands would need per-instance bases in the intersection function;
            // scene objects are triangle meshes.
            return Err("path tracer scene_mesh objects cannot carry spheres or strands".to_string());
        }
        if matches!(&m.instances, Some(i) if i.motion_transforms.is_some()) {
            return Err("path tracer scene_mesh objects cannot carry motion".to_string());
        }
        let entry = match (&m.scene, &m.instances) {
            (Some(layout), _) => (matrix.clone(), m.clone(), layout.parts.clone(), layout.placements.clone()),
            (None, None) => (matrix.clone(), m.clone(), whole, vec![(mat4::identity(), 0, -1)]),
            (None, Some(instances)) => (
                matrix.clone(),
                m.clone(),
                whole,
                instances.records.iter().enumerate().map(|(i, record)| (instances.matrices[i].clone(), 0, record.user_id)).collect(),
            ),
        };
        inputs.push(entry);
    }
    if inputs.is_empty() {
        return Err("path tracer scene needs at least one object".to_string());
    }
    let mut positions = Vec::with_capacity(4096);
    let mut normals = Vec::with_capacity(4096);
    let mut colors = Vec::with_capacity(4096);
    let colored = inputs.iter().any(|(_, m, _, _)| !m.colors.is_empty());
    let mut ids = Vec::with_capacity(1024);
    let mut parts: Vec<Part> = Vec::new();
    let mut placements: Vec<(mat4::T, i64, i64)> = Vec::new();
    let mut materials: Vec<Material> = Vec::new();
    let mut locals: Vec<mat4::T> = Vec::new();
    let mut object_ranges: Vec<i64> = vec![0];
    let placed: Vec<mat4::T> = inputs.iter().map(|(matrix, _, _, _)| matrix.clone()).collect();
    let (mut vertex_base, mut part_base, mut material_base, mut triangles) = (0i64, 0i64, 0i64, 0i64);
    for (matrix, m, input_parts, input_placements) in &inputs {
        positions.extend_from_slice(&m.positions);
        normals.extend_from_slice(&m.normals);
        if colored {
            if !m.colors.is_empty() {
                colors.extend_from_slice(&m.colors);
            } else {
                for _ in 0..m.normals.len() / 12 {
                    add_vec3(&mut colors, (1., 1., 1.));
                }
            }
        }
        for i in 0..m.ids.len() / 4 {
            let id = i32::from_le_bytes(m.ids[4 * i..4 * i + 4].try_into().unwrap()).wrapping_add(material_base as i32);
            ids.extend_from_slice(&id.to_le_bytes());
        }
        for Part::TrianglePart(first, count) in input_parts {
            parts.push(Part::TrianglePart(first + vertex_base, *count));
        }
        for (instance, part_index, material) in input_placements {
            locals.push(instance.clone());
            placements.push((mat4::mul(matrix, instance), part_index + part_base, if *material < 0 { -1 } else { material + material_base }));
        }
        object_ranges.push(object_ranges[object_ranges.len() - 1] + input_placements.len() as i64);
        materials.extend(m.materials.iter().cloned());
        vertex_base += m.positions.len() as i64 / 12;
        part_base += input_parts.len() as i64;
        material_base += m.materials.len() as i64;
        triangles += m.triangles;
    }
    let base = MeshData {
        device: None,
        positions,
        normals,
        colors,
        ids,
        triangles,
        materials,
        instances: None,
        extras: no_extras(),
        scene: None,
    };
    layout_mesh(object_ranges, placed, locals, base, parts, placements)
}

/// The same scene with object `o`'s placements at `matrices[o] * local`.
pub fn moved_mesh(m: &MeshData, matrices: &[mat4::T]) -> Result<MeshData, String> {
    match (&m.scene, &m.instances) {
        (Some(layout), Some(instances)) if layout.objects.len() > 1 => {
            let objects = &layout.objects;
            if matrices.len() != objects.len() - 1 {
                return Err("path tracer move needs one matrix per scene_mesh object".to_string());
            }
            // Only objects whose matrix changed repack their placements.
            let mut placements = layout.placements.clone();
            let mut records = instances.records.clone();
            let mut shader_bytes = instances.shader_bytes.clone();
            let mut world = instances.matrices.clone();
            let mut o = 0usize;
            let mut i = 0i64;
            while o != matrices.len() {
                if i == objects[o + 1] || mat4::nearly_equal(&matrices[o], &layout.placed[o], 0.) {
                    i = objects[o + 1];
                    o += 1;
                } else {
                    let iu = i as usize;
                    let (_, part_index, material) = placements[iu].clone();
                    let matrix = mat4::mul(&matrices[o], &layout.locals[iu]);
                    let columns = transform_columns(&matrix)?;
                    placements[iu] = (matrix.clone(), part_index, material);
                    world[iu] = matrix;
                    records[iu].instance.transform = columns.0.clone();
                    put_transform_record(&mut shader_bytes, iu * 84, &columns.0, &columns.1);
                    i += 1;
                }
            }
            let mut layout2 = layout.clone();
            layout2.placements = placements;
            layout2.placed = matrices.to_vec();
            Ok(MeshData { instances: Some(Instances { records, shader_bytes, matrices: world, ..instances.clone() }), scene: Some(layout2), ..m.clone() })
        }
        _ => Err("path tracer move needs a scene_mesh scene".to_string()),
    }
}
