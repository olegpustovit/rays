//! scene geometry on the device, built in stages
//! (upload, primitive structure, compaction, instance structure) without waiting.
use crate::frame_data::{buffer, gpu_err, put_f32, storage};
use crate::mesh_data;
use ogpu_core::backend::{self as b, Accel, AccelDescriptor, Buffer, Commands, Device, Geometry, Queue, Receipt};
use ogpu_core::types::{BufferUsage, Memory};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Shape {
    pub instanced: bool,
    pub colored: bool,
    pub lone: bool,
    pub motion: bool,
    pub spheres: bool,
    pub curves: bool,
    pub scene: bool,
}

/// The kernel's views of a mesh (pathtrace.rays): float material ids (per triangle, then per strand
/// segment), four vec3 rows per instance (first triangle, material or -1, then the normal-matrix columns;
/// seven with motion, the shutter end's columns last), scene/trace's instance table (16 floats: first
/// triangle and the world-to-object rows; 32 with motion: first triangle, start and end object-to-world
/// rows), the analytic spheres as (centre, radius) and the strand segments as (end a, radius), (end b,
/// radius).
#[derive(Clone)]
pub struct Tables {
    pub ids: Buffer,
    pub inst: Buffer,
    pub trace_instances: Option<Buffer>,
    pub spheres: Buffer,
    pub strands: Buffer,
    pub id_count: i64,
    pub instance_count: i64,
    pub sphere_count: i64,
    pub sphere_base: i64,
    pub strand_count: i64,
    pub triangle_ids: i64,
    /// some material is metallic, emissive, round: the kernel folds what none is
    pub classes: (bool, bool, bool),
}

/// A device mesh's gathered corners (`positions` and `normals` of its gpu_mesh, device-written): the
/// topology they were made for (`corners` by contents, `material_key` the material bytes), the corners'
/// vertex indices as floats, the refittable structure's scratch, and the device mesh whose buffers the
/// last gather read, kept until the gather's `receipt` completes.
#[derive(Clone)]
pub struct DeviceGpu {
    pub corners: std::sync::Arc<Vec<u32>>,
    pub vertex_count: i64,
    pub material_key: Vec<u8>,
    pub vix: Buffer,
    /// the producer's nonzero stamp of the borrowed index content and its corner count (0: host corners)
    pub topology: (u64, i64),
    /// false: the producer's index stream
    pub owns_vix: bool,
    pub refit_scratch: Buffer,
    pub source: rays::mesh::Mesh,
    pub receipt: Receipt,
}

pub type PrototypeKey = (Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>);

#[derive(Clone)]
pub struct GpuMesh {
    pub device: Option<DeviceGpu>,
    pub positions: Buffer,
    pub normals: Buffer,
    pub colors: Buffer,
    pub materials: Buffer,
    /// strands as native curves, or (no native curves) one capsule box per segment, with their data
    pub curves: Option<(Accel, Vec<Buffer>)>,
    pub native_curves: bool,
    /// Vulkan motion instances: the prototype's one-instance structure the kernel's nested query reads,
    /// with its instance record
    pub prototypes: Option<(Accel, Buffer)>,
    pub structure: Accel,
    pub primitive: Option<Accel>,
    pub instance_buffer: Option<Buffer>,
    pub motion_buffer: Option<Buffer>,
    pub prototype_key: Option<PrototypeKey>,
    pub scene_blas: Vec<Accel>,
    /// what they took before compaction
    pub scene_unpacked: i64,
    /// owned even when the prototype is borrowed
    pub tables: Tables,
    pub owns_prototype: bool,
    pub shape: Shape,
}

/// A replacement build runs in stages: the primitive structure builds and writes its compacted size; then
/// it is compacted into a right-sized structure and the instance structure (if any) builds over the
/// compacted one; then the uncompacted structure is released.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Stage {
    Building(Receipt),
    Compacting(Receipt),
    Ready,
}

pub struct MeshBuild {
    pub gpu: GpuMesh,
    pub scratch: Buffer,
    pub extra_scratch: Option<Buffer>,
    pub size_buffer: Option<Buffer>,
    pub uncompacted: Option<Accel>,
    pub uncompacted_scene: Vec<Accel>,
    pub stage: Stage,
    pub instance_count: i64,
    /// Encoded instances retain their own acceleration structure identity.
    pub instances_built: bool,
    /// the prototype structure's build scratch, until the build finishes
    pub proto_scratch: Option<Buffer>,
}

pub fn shared(device: &Device, label: &str, bytes: &[u8]) -> Result<Buffer, String> {
    let value = storage(device, label, bytes.len() as i64)?;
    match b::write_buffer(&value, 0, bytes).map_err(gpu_err) {
        Ok(()) => Ok(value),
        Err(e) => {
            let _ = b::destroy_buffer(&value);
            Err(e)
        }
    }
}

pub fn table_buffers(t: &Tables) -> Vec<Buffer> {
    let mut v = vec![t.ids.clone(), t.inst.clone(), t.spheres.clone(), t.strands.clone()];
    v.extend(t.trace_instances.iter().cloned());
    v
}

pub fn destroy_gpu(gpu: &GpuMesh) {
    let _ = b::destroy_accel(&gpu.structure);
    for value in table_buffers(&gpu.tables) {
        let _ = b::destroy_buffer(&value);
    }
    if let Some(motion) = &gpu.motion_buffer {
        let _ = b::destroy_buffer(motion);
    }
    if let Some(instances) = &gpu.instance_buffer {
        let _ = b::destroy_buffer(instances);
    }
    if gpu.owns_prototype {
        if let Some(d) = &gpu.device {
            if d.owns_vix {
                let _ = b::destroy_buffer(&d.vix);
            }
            let _ = b::destroy_buffer(&d.refit_scratch);
        }
        if let Some(primitive) = &gpu.primitive {
            let _ = b::destroy_accel(primitive);
        }
        for value in [&gpu.positions, &gpu.normals, &gpu.colors, &gpu.materials] {
            let _ = b::destroy_buffer(value);
        }
        for blas in &gpu.scene_blas {
            let _ = b::destroy_accel(blas);
        }
        if let Some((proto, record)) = &gpu.prototypes {
            let _ = b::destroy_accel(proto);
            let _ = b::destroy_buffer(record);
        }
        if let Some((curves, buffers)) = &gpu.curves {
            let _ = b::destroy_accel(curves);
            for value in buffers {
                let _ = b::destroy_buffer(value);
            }
        }
    }
}

/// Encodes the instance structure build over `build.gpu.primitive`.
pub fn encode_instances(device: &Device, build: &mut MeshBuild, command: &Commands) -> Result<(), String> {
    let structures: Vec<Accel> = match &build.gpu.primitive {
        Some(primitive) => vec![primitive.clone()],
        None => build.gpu.scene_blas.clone(),
    };
    match (&build.gpu.instance_buffer, structures.is_empty()) {
        (Some(instances), false) => {
            let kind = if build.gpu.motion_buffer.is_none() { b::InstanceKind::UserIdInstances } else { b::InstanceKind::MotionInstances };
            // Vulkan has no motion ray queries: the kernel interpolates a moving instance itself and runs a
            // nested query on the prototype's one-instance structure
            let proto = if build.gpu.motion_buffer.is_some() && flow_gpu::pipelines::is_vulkan() && build.gpu.prototypes.is_none() {
                let identity = vec![1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0];
                let record = b::InstanceRecord { instance: b::Instance { transform: identity, mask: 0xFFFF_FFFF, structure_index: 0 }, user_id: 0 };
                let packed = b::pack_instance_records(device, &[record]).map_err(gpu_err)?;
                let record_buffer = shared(device, "pathtracer-prototype-instance", &packed)?;
                let created = b::create_accel(device, AccelDescriptor::TlasOf { instances: record_buffer.clone(), offset: 0, instance_count: 1, kind: b::InstanceKind::UserIdInstances, structures: structures.clone(), motion_transforms: None }, false);
                match created {
                    Ok(accel) => {
                        let scratch = buffer(Memory::DeviceLocal, vec![BufferUsage::Storage], device, "pathtracer-prototype-scratch", b::accel_sizes(&accel).build_scratch_size);
                        match scratch {
                            Ok(scratch) => Some((accel, record_buffer, scratch)),
                            Err(e) => {
                                let _ = b::destroy_accel(&accel);
                                let _ = b::destroy_buffer(&record_buffer);
                                return Err(e);
                            }
                        }
                    }
                    Err(e) => {
                        let _ = b::destroy_buffer(&record_buffer);
                        return Err(gpu_err(e));
                    }
                }
            } else {
                None
            };
            let structure = b::create_accel(
                device,
                AccelDescriptor::TlasOf {
                    instances: instances.clone(),
                    offset: 0,
                    instance_count: build.instance_count,
                    kind,
                    structures,
                    motion_transforms: build.gpu.motion_buffer.as_ref().map(|buf| (buf.clone(), 0, 2 * build.instance_count)),
                },
                false,
            )
            .map_err(gpu_err)?;
            let needed = b::accel_sizes(&structure).build_scratch_size;
            let scratch = if needed <= b::buffer_size(&build.scratch) {
                build.scratch.clone()
            } else {
                let extra = buffer(Memory::DeviceLocal, vec![BufferUsage::Storage], device, "pathtracer-instance-scratch", needed)?;
                build.extra_scratch = Some(extra.clone());
                extra
            };
            let encoded = (|| -> Result<(), String> {
                let encoder = b::accel_encoder(command).map_err(gpu_err)?;
                if let Some((accel, _, proto_scratch)) = &proto {
                    b::build_accel(&encoder, accel, proto_scratch, None).map_err(gpu_err)?;
                }
                b::build_accel(&encoder, &structure, &scratch, None).map_err(gpu_err)?;
                b::end_accel(&encoder).map_err(gpu_err)
            })();
            match encoded {
                Ok(()) => {
                    build.gpu.structure = structure;
                    build.instances_built = true;
                    if let Some((accel, record, proto_scratch)) = proto {
                        build.gpu.prototypes = Some((accel, record));
                        build.proto_scratch = Some(proto_scratch);
                    }
                    Ok(())
                }
                Err(e) => {
                    let _ = b::destroy_accel(&structure);
                    if let Some((accel, record, proto_scratch)) = proto {
                        let _ = b::destroy_accel(&accel);
                        let _ = b::destroy_buffer(&record);
                        let _ = b::destroy_buffer(&proto_scratch);
                    }
                    Err(e)
                }
            }
        }
        _ => Ok(()),
    }
}

pub fn submit_instances(device: &Device, queue: &Queue, build: &mut MeshBuild) -> Result<(), String> {
    let command = b::begin_commands(queue).map_err(gpu_err)?;
    let result = (|| -> Result<Receipt, String> {
        encode_instances(device, build, &command)?;
        b::commit(&command).map_err(gpu_err)
    })();
    match result {
        Ok(receipt) => {
            build.stage = Stage::Compacting(receipt);
            Ok(())
        }
        Err(e) => {
            let _ = b::abandon(&command);
            Err(e)
        }
    }
}

pub fn mesh_shape(mesh: &mesh_data::MeshData) -> Shape {
    Shape {
        instanced: mesh.instances.is_some(),
        colored: !mesh.colors.is_empty() || mesh.device.as_ref().map(|d| d.colored).unwrap_or(false),
        lone: mesh.device.is_some(),
        motion: matches!(&mesh.instances, Some(i) if i.motion_transforms.is_some()),
        spheres: mesh.extras.sphere_count > 0,
        curves: mesh.extras.segments > 0,
        scene: mesh.scene.is_some(),
    }
}

fn floats(values: &[f64]) -> Vec<u8> {
    let mut bytes = vec![0u8; 4 * values.len().max(1)];
    for (i, v) in values.iter().enumerate() {
        put_f32(&mut bytes, 4 * i, *v);
    }
    bytes
}

fn f32_at(bytes: &[u8], offset: usize) -> f64 {
    f32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap()) as f64
}

fn i32_at(bytes: &[u8], offset: usize) -> i64 {
    i32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap()) as i64
}

pub fn upload_tables(material_data: &[u8], device: &Device, mesh: &mesh_data::MeshData) -> Result<Tables, String> {
    let some = |k: &[usize]| {
        for m in 0..material_data.len() / 48 {
            for c in k {
                if f32_at(material_data, 48 * m + 4 * c) != 0. {
                    return true;
                }
            }
        }
        false
    };
    let (records, shader): (&[b::InstanceRecord], &[u8]) = match &mesh.instances {
        Some(i) => (&i.records, &i.shader_bytes),
        None => (&[], &[]),
    };
    let keyframes: Option<&Vec<Vec<f64>>> = mesh.instances.as_ref().and_then(|i| i.motion_transforms.as_ref());
    let n = records.len();
    let stride = if keyframes.is_none() { 84 } else { 168 };
    let first = |i: usize| match &mesh.scene {
        Some(l) => i32_at(&l.table, 8 * i) as f64,
        None => 0.,
    };
    let material = |i: usize| match &mesh.scene {
        Some(l) => i32_at(&l.table, 8 * i + 4) as f64,
        None => {
            let user = records[i].user_id;
            if user == 0 { -1. } else { user as f64 }
        }
    };
    let column = |at: usize, i: usize, k: usize| -> Vec<f64> { (0..3).map(|r| f32_at(shader, stride * i + at + 4 * (3 * k + r))).collect() };
    let mut inst: Vec<f64> = Vec::new();
    for i in 0..n {
        inst.extend_from_slice(&[first(i), material(i), 0.]);
        inst.extend(column(0, i, 4));
        inst.extend(column(0, i, 5));
        inst.extend(column(0, i, 6));
        if keyframes.is_some() {
            inst.extend(column(84, i, 4));
            inst.extend(column(84, i, 5));
            inst.extend(column(84, i, 6));
        }
    }
    // world-to-object row r is normal-matrix column r; its translation is minus that row dotted with t
    let mut trace: Vec<f64> = Vec::new();
    for i in 0..n {
        match keyframes {
            Some(k) => {
                trace.push(first(i));
                trace.extend_from_slice(&k[2 * i]);
                trace.extend_from_slice(&k[2 * i + 1]);
                trace.extend_from_slice(&[0.; 7]);
            }
            None => {
                let t = column(0, i, 3);
                trace.push(first(i));
                for r in 0..3 {
                    let w = column(0, i, 4 + r);
                    trace.extend_from_slice(&[w[0], w[1], w[2], -(w[0] * t[0] + w[1] * t[1] + w[2] * t[2])]);
                }
                trace.extend_from_slice(&[0., 0., 0.]);
            }
        }
    }
    let e = &mesh.extras;
    let spheres: Vec<f64> = (0..4 * e.sphere_count as usize).map(|j| f32_at(&e.sphere_bytes, 4 * j)).collect();
    let strands: Vec<f64> = (0..8 * e.segments as usize)
        .map(|j| {
            let first = i32_at(&e.indices, 4 * (j / 8)) as usize + (j % 8) / 4;
            if j % 4 == 3 { f32_at(&e.radii, 4 * first) } else { f32_at(&e.points, 12 * first + 4 * (j % 4)) }
        })
        .collect();
    let id = |bytes: &[u8]| -> Vec<f64> { (0..bytes.len() / 4).map(|i| i32_at(bytes, 4 * i) as f64).collect() };
    let triangle_ids = (mesh.ids.len() / 4) as i64;
    let mut ids = id(&mesh.ids);
    ids.extend(id(&e.ids));
    let mut owned: Vec<Buffer> = Vec::new();
    let made = (|| -> Result<Tables, String> {
        let mut upload = |label: &str, values: &[f64]| -> Result<Buffer, String> {
            let buf = shared(device, label, &floats(values))?;
            owned.push(buf.clone());
            Ok(buf)
        };
        let ids_buffer = upload("pathtracer-lisp-ids", &ids)?;
        let inst_buffer = upload("pathtracer-lisp-instances", &if n == 0 { vec![0.; 12] } else { inst })?;
        let spheres_buffer = upload("pathtracer-lisp-spheres", &if spheres.is_empty() { vec![0.; 4] } else { spheres })?;
        let strands_buffer = upload("pathtracer-lisp-strands", &if strands.is_empty() { vec![0.; 8] } else { strands })?;
        let trace_instances = if n == 0 { None } else { Some(upload("pathtracer-lisp-trace", &trace)?) };
        Ok(Tables {
            ids: ids_buffer,
            inst: inst_buffer,
            trace_instances,
            spheres: spheres_buffer,
            strands: strands_buffer,
            id_count: ids.len() as i64,
            instance_count: (n as i64).max(1),
            sphere_count: e.sphere_count,
            sphere_base: e.sphere_material,
            strand_count: e.segments,
            triangle_ids,
            classes: (some(&[7]), some(&[4, 5, 6]), some(&[8])),
        })
    })();
    if made.is_err() {
        for value in &owned {
            let _ = b::destroy_buffer(value);
        }
    }
    made
}

/// What a failed `upload_mesh_async` releases.
struct OwnedResources {
    buffers: Vec<Buffer>,
    structures: Vec<Accel>,
    scratch: Option<Buffer>,
    commands: Option<Commands>,
}

fn owned(device: &Device, o: &mut OwnedResources, bytes: &[u8], label: &str) -> Result<Buffer, String> {
    let value = shared(device, label, bytes)?;
    o.buffers.push(value.clone());
    Ok(value)
}

fn accel(device: &Device, o: &mut OwnedResources, descriptor: AccelDescriptor) -> Result<Accel, String> {
    let value = b::create_accel(device, descriptor, false).map_err(gpu_err)?;
    o.structures.push(value.clone());
    Ok(value)
}

/// Stage one of a replacement build: upload the geometry, build the primitive structure, and write its
/// compacted size, all without waiting. A reused prototype skips straight to the instance build.
pub fn upload_mesh_async(reuse: Option<&GpuMesh>, material_data: &[u8], device: &Device, queue: &Queue, mesh: &mesh_data::MeshData) -> Result<MeshBuild, String> {
    let mut o = OwnedResources { buffers: vec![], structures: vec![], scratch: None, commands: None };
    let result = upload_inner(&mut o, reuse, material_data, device, queue, mesh);
    match result {
        Ok(build) => Ok(build),
        Err(e) => {
            if let Some(command) = &o.commands {
                let _ = b::abandon(command);
            }
            if let Some(work) = &o.scratch {
                let _ = b::destroy_buffer(work);
            }
            for built in &o.structures {
                let _ = b::destroy_accel(built);
            }
            for value in &o.buffers {
                let _ = b::destroy_buffer(value);
            }
            Err(e)
        }
    }
}

fn upload_inner(o: &mut OwnedResources, reuse: Option<&GpuMesh>, material_data: &[u8], device: &Device, queue: &Queue, mesh: &mesh_data::MeshData) -> Result<MeshBuild, String> {
    let mesh = if mesh_shape(mesh).motion && !b::capabilities(device).ray_tracing_motion { mesh_data::shutter_midpoint(mesh)? } else { mesh.clone() };
    let mesh = &mesh;
    let shape = mesh_shape(mesh);
    let extras = &mesh.extras;
    let capabilities = b::capabilities(device);
    // without native curves scene/trace tests each strand segment as a capsule
    let native_curves = shape.curves && capabilities.ray_tracing_curves;
    let extra_key = || [extras.sphere_bytes.as_slice(), extras.points.as_slice(), mesh.colors.as_slice()].concat();
    let reused: Option<(&GpuMesh, Accel)> = match (reuse, &mesh.instances) {
        (Some(gpu), Some(_)) if mesh.scene.is_none() => match (&gpu.prototype_key, &gpu.primitive) {
            (Some((positions, normals, ids, materials, key)), Some(primitive))
                if *positions == mesh.positions && *normals == mesh.normals && *ids == mesh.ids && materials.as_slice() == material_data && *key == extra_key() =>
            {
                Some((gpu, primitive.clone()))
            }
            _ => None,
        },
        _ => None,
    };
    let (positions, normals, colors, materials, curves, primitive, scene_blas, build_primitive) = match &reused {
        Some((gpu, primitive)) => (gpu.positions.clone(), gpu.normals.clone(), gpu.colors.clone(), gpu.materials.clone(), gpu.curves.clone(), primitive.clone(), vec![], false),
        None => {
            let positions = owned(device, o, &mesh.positions, "pathtracer-positions")?;
            let normals = owned(device, o, &mesh.normals, "pathtracer-normals")?;
            let colors = owned(device, o, if mesh.colors.is_empty() { &[0u8; 12] } else { mesh.colors.as_slice() }, "pathtracer-colors")?;
            let materials = owned(device, o, material_data, "pathtracer-materials")?;
            let curves = if !shape.curves {
                None
            } else if !native_curves {
                // capsule candidates: one box per segment, the kernel resolves each against the strand records
                let value = |bytes: &[u8], at: usize| f32::from_le_bytes(bytes[at..at + 4].try_into().unwrap()) as f64;
                let segments = extras.segments as usize;
                let mut boxes = vec![0u8; segments * 24];
                for segment in 0..segments {
                    let first = i32::from_le_bytes(extras.indices[4 * segment..4 * segment + 4].try_into().unwrap()) as usize;
                    let radius = value(&extras.radii, 4 * first).abs().max(value(&extras.radii, 4 * (first + 1)).abs());
                    for axis in 0..3 {
                        let a = value(&extras.points, 12 * first + 4 * axis);
                        let b2 = value(&extras.points, 12 * (first + 1) + 4 * axis);
                        let low = a.min(b2) - radius;
                        let high = a.max(b2) + radius;
                        let epsilon = 1e-5 * 1.0f64.max(low.abs().max(high.abs()));
                        boxes[segment * 24 + axis * 4..segment * 24 + axis * 4 + 4].copy_from_slice(&((low - epsilon) as f32).to_le_bytes());
                        boxes[segment * 24 + 12 + axis * 4..segment * 24 + 12 + axis * 4 + 4].copy_from_slice(&((high + epsilon) as f32).to_le_bytes());
                    }
                }
                let boxes = owned(device, o, &boxes, "pathtracer-strand-boxes")?;
                let curves = accel(
                    device,
                    o,
                    AccelDescriptor::Blas { geometries: vec![Geometry::BoundingBoxes { boxes: vec![b::Keyframe { buffer: boxes.clone(), offset: 0 }], stride: 24, count: extras.segments, options: b::DEFAULT_GEOMETRY_OPTIONS }] },
                )?;
                Some((curves, vec![boxes]))
            } else {
                let points = owned(device, o, &extras.points, "pathtracer-strand-points")?;
                let radii = owned(device, o, &extras.radii, "pathtracer-strand-radii")?;
                let indices = owned(device, o, &extras.indices, "pathtracer-strand-indices")?;
                let curves = accel(
                    device,
                    o,
                    AccelDescriptor::Blas {
                        geometries: vec![Geometry::Curves {
                            control_points: vec![b::Keyframe { buffer: points.clone(), offset: 0 }],
                            control_stride: 12,
                            control_point_count: extras.point_count,
                            radii: vec![b::Keyframe { buffer: radii.clone(), offset: 0 }],
                            radius_stride: 4,
                            indices: indices.clone(),
                            index_offset: 0,
                            segment_count: extras.segments,
                            control_points_per_segment: 2,
                            curve_type: b::CurveType::RoundCurve,
                            basis: b::CurveBasis::LinearBasis,
                            caps: b::CurveCaps::SphereCaps,
                        }],
                    },
                )?;
                Some((curves, vec![points, radii, indices]))
            };
            let triangles = |mesh_data::Part::TrianglePart(first, vertex_count): mesh_data::Part| Geometry::Triangles {
                vertices: positions.clone(),
                offset: first * 12,
                length: vertex_count * 12,
                vertex_stride: 12,
                vertex_count,
            };
            let parts: Vec<mesh_data::Part> = match &mesh.scene {
                Some(layout) => layout.parts.clone(),
                None => vec![mesh_data::Part::TrianglePart(0, mesh.positions.len() as i64 / 12)],
            };
            let mut blas = Vec::new();
            for part in parts {
                blas.push(accel(device, o, AccelDescriptor::Blas { geometries: vec![triangles(part)] })?);
            }
            let primitive = blas[0].clone();
            (positions, normals, colors, materials, curves, primitive, if mesh.scene.is_none() { vec![] } else { blas }, true)
        }
    };
    let (instance_buffer, motion_buffer) = match &mesh.instances {
        None => (None, None),
        Some(instances) => {
            let motion_buffer = match &instances.motion_transforms {
                None => None,
                Some(keyframes) => {
                    let packed = b::pack_transforms(keyframes).map_err(gpu_err)?;
                    Some(owned(device, o, &packed, "pathtracer-motion")?)
                }
            };
            let packed = match &instances.motion_transforms {
                None => b::pack_instance_records(device, &instances.records).map_err(gpu_err)?,
                Some(_) => {
                    let list: Vec<b::MotionInstance> = instances
                        .records
                        .iter()
                        .enumerate()
                        .map(|(index, record)| b::MotionInstance {
                            record: record.clone(),
                            transforms_start: 2 * index as i64,
                            transforms_count: 2,
                            start_time: 0.,
                            end_time: 1.,
                            start_border: b::Border::Clamp,
                            end_border: b::Border::Clamp,
                        })
                        .collect();
                    b::pack_motion_instances(device, &list).map_err(gpu_err)?
                }
            };
            let instance_buffer = owned(device, o, &packed, "pathtracer-instances")?;
            (Some(instance_buffer), motion_buffer)
        }
    };
    let tables = upload_tables(material_data, device, mesh)?;
    o.buffers.extend(table_buffers(&tables));
    // Scene BLASes build side by side, each in its own 256-byte-aligned slice of one scratch buffer.
    let mut scratch_offsets = Vec::new();
    let mut offset = 0i64;
    for blas in &scene_blas {
        let size = b::accel_sizes(blas).build_scratch_size;
        scratch_offsets.push(offset);
        offset += (size + 255) / 256 * 256;
    }
    let work = buffer(
        Memory::DeviceLocal,
        vec![BufferUsage::Storage],
        device,
        "pathtracer-scratch",
        if !scene_blas.is_empty() {
            scene_blas.iter().fold(1024, |total, blas| total + (b::accel_sizes(blas).build_scratch_size + 255) / 256 * 256)
        } else {
            1024.max(2 * b::accel_sizes(&primitive).build_scratch_size)
        },
    )?;
    o.scratch = Some(work.clone());
    let gpu_mesh_ = GpuMesh {
        device: None,
        positions,
        normals,
        colors,
        materials,
        curves: curves.clone(),
        native_curves: match &reused {
            Some((gpu, _)) => gpu.native_curves,
            None => native_curves,
        },
        prototypes: reused.as_ref().and_then(|(gpu, _)| gpu.prototypes.clone()),
        structure: primitive.clone(),
        primitive: None,
        instance_buffer,
        motion_buffer,
        prototype_key: match &mesh.instances {
            Some(_) if mesh.scene.is_none() => Some((mesh.positions.clone(), mesh.normals.clone(), mesh.ids.clone(), material_data.to_vec(), extra_key())),
            _ => None,
        },
        scene_blas: scene_blas.clone(),
        scene_unpacked: 0,
        tables,
        owns_prototype: reused.is_none(),
        shape,
    };
    let instance_count = mesh.instances.as_ref().map(|i| i.records.len() as i64).unwrap_or(0);
    let building = |gpu: GpuMesh, extra_scratch: Option<Buffer>, size_buffer: Option<Buffer>, receipt: Receipt| MeshBuild {
        gpu,
        scratch: work.clone(),
        extra_scratch,
        size_buffer,
        uncompacted: None,
        uncompacted_scene: vec![],
        stage: Stage::Building(receipt),
        instance_count,
        instances_built: false,
        proto_scratch: None,
    };
    if !scene_blas.is_empty() {
        // each writes its compacted size, read once they are built
        let size_buffer = buffer(Memory::Shared, vec![BufferUsage::Storage, BufferUsage::CopySrc], device, "pathtracer-compacted-sizes", 8 * scene_blas.len() as i64)?;
        o.buffers.push(size_buffer.clone());
        let command = b::begin_commands(queue).map_err(gpu_err)?;
        o.commands = Some(command.clone());
        let encoder = b::accel_encoder(&command).map_err(gpu_err)?;
        for (blas, offset) in scene_blas.iter().zip(&scratch_offsets) {
            b::build_accel(&encoder, blas, &work, Some(*offset)).map_err(gpu_err)?;
        }
        for (index, blas) in scene_blas.iter().enumerate() {
            b::write_compacted_size(&encoder, blas, &size_buffer, Some(8 * index as i64)).map_err(gpu_err)?;
        }
        b::end_accel(&encoder).map_err(gpu_err)?;
        let receipt = b::commit(&command).map_err(gpu_err)?;
        Ok(building(gpu_mesh_, None, Some(size_buffer), receipt))
    } else if build_primitive {
        let size_buffer = buffer(Memory::Shared, vec![BufferUsage::Storage, BufferUsage::CopySrc], device, "pathtracer-compacted-size", 8)?;
        o.buffers.push(size_buffer.clone());
        let command = b::begin_commands(queue).map_err(gpu_err)?;
        o.commands = Some(command.clone());
        // the curve structure builds beside the triangles, in its own scratch
        let curve_scratch = match &curves {
            None => None,
            Some((c, _)) => Some(buffer(Memory::DeviceLocal, vec![BufferUsage::Storage], device, "pathtracer-curve-scratch", b::accel_sizes(c).build_scratch_size)?),
        };
        let receipt = (|| -> Result<Receipt, String> {
            let encoder = b::accel_encoder(&command).map_err(gpu_err)?;
            if let (Some((c, _)), Some(scratch)) = (&curves, &curve_scratch) {
                b::build_accel(&encoder, c, scratch, None).map_err(gpu_err)?;
            }
            b::build_accel(&encoder, &primitive, &work, None).map_err(gpu_err)?;
            b::write_compacted_size(&encoder, &primitive, &size_buffer, None).map_err(gpu_err)?;
            b::end_accel(&encoder).map_err(gpu_err)?;
            b::commit(&command).map_err(gpu_err)
        })()?;
        Ok(building(gpu_mesh_, curve_scratch, Some(size_buffer), receipt))
    } else {
        // A reused compacted prototype only needs its instance structure.
        let mut build = MeshBuild {
            gpu: GpuMesh { primitive: Some(primitive), ..gpu_mesh_ },
            scratch: work.clone(),
            extra_scratch: None,
            size_buffer: None,
            uncompacted: None,
            uncompacted_scene: vec![],
            stage: Stage::Ready,
            instance_count,
            instances_built: false,
            proto_scratch: None,
        };
        submit_instances(device, queue, &mut build)?;
        Ok(build)
    }
}

/// Polling a later epoch retires earlier ones, so a wait on an already completed receipt is a no-op
/// rather than an invalid-epoch error.
pub fn wait(queue: &Queue, receipt: Receipt) -> Result<(), String> {
    if b::completed_epoch(queue) >= receipt.epoch { Ok(()) } else { b::complete_through(queue, receipt.epoch).map_err(gpu_err) }
}

/// The scene's structures are built: compact them, then build the instances over the compacted ones.
pub fn compact_scene(device: &Device, queue: &Queue, build: &mut MeshBuild) -> Result<(), String> {
    let size_bytes = match &build.size_buffer {
        Some(size_buffer) => b::read_buffer(size_buffer, 0, 8 * build.gpu.scene_blas.len() as i64).map_err(gpu_err)?,
        None => return Err("path tracer build lost its compacted sizes".to_string()),
    };
    let built = build.gpu.scene_blas.clone();
    let mut made: Vec<Accel> = Vec::new();
    for (index, template) in built.iter().enumerate() {
        let size = i64::from_le_bytes(size_bytes[8 * index..8 * index + 8].try_into().unwrap());
        match b::create_accel(device, AccelDescriptor::Sized { size, template: template.clone() }, false) {
            Ok(compacted) => made.push(compacted),
            Err(e) => {
                for blas in &made {
                    let _ = b::destroy_accel(blas);
                }
                return Err(gpu_err(e));
            }
        }
    }
    let compacted = made;
    build.uncompacted_scene = built.clone();
    build.gpu.scene_unpacked = built.iter().fold(0, |total, blas| total + b::accel_sizes(blas).structure_size);
    build.gpu.scene_blas = compacted.clone();
    build.gpu.structure = compacted[0].clone();
    let command = b::begin_commands(queue).map_err(gpu_err)?;
    let encoded = (|| -> Result<Receipt, String> {
        let encoder = b::accel_encoder(&command).map_err(gpu_err)?;
        for (src, dst) in built.iter().zip(&compacted) {
            b::compact_accel(&encoder, src, dst).map_err(gpu_err)?;
        }
        b::end_accel(&encoder).map_err(gpu_err)?;
        encode_instances(device, build, &command)?;
        b::commit(&command).map_err(gpu_err)
    })();
    match encoded {
        Ok(receipt) => {
            build.stage = Stage::Compacting(receipt);
            Ok(())
        }
        Err(e) => {
            let _ = b::abandon(&command);
            Err(e)
        }
    }
}

pub fn advance_build(device: &Device, queue: &Queue, build: &mut MeshBuild) -> Result<(), String> {
    match build.stage {
        Stage::Ready => Ok(()),
        Stage::Building(_) if !build.gpu.scene_blas.is_empty() => compact_scene(device, queue, build),
        Stage::Building(_) => {
            let uncompacted = build.gpu.structure.clone();
            let size_bytes = match &build.size_buffer {
                Some(size_buffer) => b::read_buffer(size_buffer, 0, 8).map_err(gpu_err)?,
                None => return Err("path tracer build lost its compacted size".to_string()),
            };
            let size = i64::from_le_bytes(size_bytes[0..8].try_into().unwrap());
            let compacted = b::create_accel(device, AccelDescriptor::Sized { size, template: uncompacted.clone() }, false).map_err(gpu_err)?;
            build.uncompacted = Some(uncompacted.clone());
            build.gpu.structure = compacted.clone();
            if build.instance_count > 0 {
                build.gpu.primitive = Some(compacted.clone());
            }
            let command = b::begin_commands(queue).map_err(gpu_err)?;
            let encoded = (|| -> Result<Receipt, String> {
                let encoder = b::accel_encoder(&command).map_err(gpu_err)?;
                b::compact_accel(&encoder, &uncompacted, &compacted).map_err(gpu_err)?;
                b::end_accel(&encoder).map_err(gpu_err)?;
                encode_instances(device, build, &command)?;
                b::commit(&command).map_err(gpu_err)
            })();
            match encoded {
                Ok(receipt) => {
                    build.stage = Stage::Compacting(receipt);
                    Ok(())
                }
                Err(e) => {
                    let _ = b::abandon(&command);
                    Err(e)
                }
            }
        }
        Stage::Compacting(_) => {
            if let Some(structure) = build.uncompacted.take() {
                let _ = b::destroy_accel(&structure);
            }
            for structure in build.uncompacted_scene.drain(..) {
                let _ = b::destroy_accel(&structure);
            }
            if let Some(size_buffer) = build.size_buffer.take() {
                let _ = b::destroy_buffer(&size_buffer);
            }
            build.stage = Stage::Ready;
            Ok(())
        }
    }
}

pub fn finish_build(device: &Device, queue: &Queue, mut build: MeshBuild) -> Result<GpuMesh, String> {
    let finished = (|| -> Result<(), String> {
        loop {
            match build.stage {
                Stage::Ready => return Ok(()),
                Stage::Building(receipt) | Stage::Compacting(receipt) => {
                    wait(queue, receipt)?;
                    advance_build(device, queue, &mut build)?;
                }
            }
        }
    })();
    let _ = b::destroy_buffer(&build.scratch);
    if let Some(extra) = &build.extra_scratch {
        let _ = b::destroy_buffer(extra);
    }
    if let Some(extra) = &build.proto_scratch {
        let _ = b::destroy_buffer(extra);
    }
    match finished {
        Ok(()) => Ok(build.gpu),
        Err(e) => {
            if let Some(structure) = &build.uncompacted {
                let _ = b::destroy_accel(structure);
            }
            for structure in &build.uncompacted_scene {
                let _ = b::destroy_accel(structure);
            }
            if let Some(size_buffer) = &build.size_buffer {
                let _ = b::destroy_buffer(size_buffer);
            }
            destroy_gpu(&build.gpu);
            Err(e)
        }
    }
}

pub fn discard_build(queue: &Queue, build: &MeshBuild) {
    match build.stage {
        Stage::Ready => {}
        Stage::Building(receipt) | Stage::Compacting(receipt) => {
            let _ = wait(queue, receipt);
        }
    }
    let _ = b::destroy_buffer(&build.scratch);
    if let Some(extra) = &build.extra_scratch {
        let _ = b::destroy_buffer(extra);
    }
    if let Some(extra) = &build.proto_scratch {
        let _ = b::destroy_buffer(extra);
    }
    if let Some(structure) = &build.uncompacted {
        let _ = b::destroy_accel(structure);
    }
    for structure in &build.uncompacted_scene {
        let _ = b::destroy_accel(structure);
    }
    if let Some(size_buffer) = &build.size_buffer {
        let _ = b::destroy_buffer(size_buffer);
    }
    destroy_gpu(&build.gpu);
}
