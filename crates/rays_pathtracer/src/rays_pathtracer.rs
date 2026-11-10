//! Hardware tracing accumulates spp samples per render and refreshes a borrowed image; camera changes reset accumulation. RDK metallic/roughness materials use a procedural dome or baked World. Kernels are emitted from pathtrace.rays.
use flow_syntax::symbol::Symbol;
use crate::frame_data::{self, allocate_film, camera_fov, color_tuple, destroy_film, Film, gpu_err, put_u32};
use crate::lisp_kernel;
use crate::mesh_build::{advance_build, destroy_gpu, discard_build, finish_build, GpuMesh, MeshBuild, mesh_shape, shared, Stage, submit_instances, table_buffers, upload_tables, wait, DeviceGpu};
use crate::mesh_data;
use ogpu_core::backend::{self as b, Accel, AccelDescriptor, Buffer, Commands, Device, Geometry, Queue, Receipt, Status, Texture};
use ogpu_core::types::{BufferUsage, Memory};
use rays_math::color::linear;
use rays_math::vec3;
use std::rc::Rc;

pub use rays_math::color::linear as Linear_color;

/// Linear floating-point RGB for materials and light. Channels are not quantized or clamped, so dim and HDR
/// values survive GPU upload.
pub type LinearColor = linear::T;

#[derive(Clone, Debug, PartialEq)]
pub struct Material {
    pub albedo: LinearColor,
    /// 0 = mirror, 1 = fully rough
    pub roughness: f64,
    pub metallic: f64,
    pub emission: LinearColor,
    /// Round-corners radius in scene units (0 = off). A render-time shading bevel: the normal rolls over
    /// onto adjacent faces within this distance of an edge, without changing geometry.
    pub round: f64,
}

pub fn material(roughness: Option<f64>, metallic: Option<f64>, emission: Option<LinearColor>, round: Option<f64>, albedo: LinearColor) -> Material {
    Material {
        albedo,
        roughness: roughness.unwrap_or(0.5),
        metallic: metallic.unwrap_or(0.),
        emission: emission.unwrap_or(linear::rgb(0., 0., 0.)),
        round: round.unwrap_or(0.),
    }
}

/// A soft rectangle of light painted on the dome around `direction`. Extents are angular half-sizes in
/// radians; `softness` is the edge falloff.
#[derive(Clone, Debug, PartialEq)]
pub struct Panel {
    pub direction: vec3::T,
    pub width: f64,
    pub height: f64,
    pub softness: f64,
    pub color: LinearColor,
    pub intensity: f64,
}

pub fn panel(softness: Option<f64>, intensity: f64, width: f64, height: f64, direction: vec3::T) -> Panel {
    Panel { direction, width, height, softness: softness.unwrap_or(0.05), color: linear::rgb(1., 1., 1.), intensity }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub sky: LinearColor,
    pub ground: LinearColor,
    pub panels: Vec<Panel>,
}

pub type Camera = rays::camera::Camera;

/// Rectangle area light of `size` (width, height) centred at `at`, facing `target`, two-sided. Sampled with
/// shadow rays (next-event estimation); lights are analytic and, without a World (`set_world`), never appear
/// as visible geometry.
#[derive(Clone, Debug, PartialEq)]
pub struct Light {
    pub at: vec3::T,
    pub target: vec3::T,
    pub size: (f64, f64),
    pub color: LinearColor,
    pub intensity: f64,
}

pub fn rect_light(intensity: f64, size: (f64, f64), target: vec3::T, at: vec3::T) -> Light {
    Light { at, target, size, color: linear::rgb(1., 1., 1.), intensity }
}

/// The rect that lights like a raster `Light.t` with physical 1/d^2 attenuation: a white matte face at
/// distance `d` receives irradiance `pi * intensity * color / d^2` (the raster World path's inverse: a rect
/// of radiance `L` and area `A` is a `Light.t` of color `L * A / pi`). `color` is `diffuse / 255` as linear
/// RGB; the rect's radiance is `pi * intensity / (width * height)`.
pub fn light_of(l: &rays::light::Light) -> Light {
    let c = l.diffuse;
    let color = linear::rgb(c.r as f64 / 255., c.g as f64 / 255., c.b as f64 / 255.);
    let rect = |at: vec3::T, target: vec3::T, distance2: f64, (w, h): (f64, f64)| Light { at, target, size: (w, h), color, intensity: std::f64::consts::PI * l.intensity * distance2 / (w * h) };
    match &l.kind {
        rays::light::kind::Area { position, direction, width, height, .. } => rect(*position, vec3::add(*position, *direction), 1., (*width, *height)),
        rays::light::kind::Point { position, .. } => rect(*position, vec3::sub(*position, vec3::unit_y()), 1., (0.1, 0.1)),
        rays::light::kind::Spot { position, direction, .. } => rect(*position, vec3::add(*position, *direction), 1., (0.1, 0.1)),
        rays::light::kind::Directional { direction } => rect(vec3::scale(vec3::normalize(*direction), -100.), vec3::zero(), 1e4, (20., 20.)),
    }
}

/// An analytic sphere, intersected beside the triangle structure rather than tessellated.
#[derive(Clone, Debug, PartialEq)]
pub struct Sphere {
    pub center: vec3::T,
    pub radius: f64,
    pub sphere_material: Material,
}

/// A round polyline of constant `thickness` (diameter), traced as linear curve segments: native curves where
/// `Ray_tracing_curves` is present, else one capsule test per segment.
#[derive(Clone, Debug, PartialEq)]
pub struct Strand {
    pub points: Vec<vec3::T>,
    pub thickness: f64,
    pub strand_material: Material,
}

/// Scene mesh inputs are already triangle-ready `Rays.Mesh.t` values with per-vertex normals. Geometry
/// triangulation and normal generation happen before entering the path tracer.
#[derive(Clone)]
pub struct Scene {
    pub objects: Vec<(rays::mesh::Mesh, Material)>,
    pub spheres: Vec<Sphere>,
    pub strands: Vec<Strand>,
    pub environment: Environment,
    pub lights: Vec<Light>,
}

/// One in-flight frame: its receipt, which output texture it writes, and whether a camera change made its
/// result obsolete.
#[derive(Clone)]
pub struct Pending {
    pub receipt: Receipt,
    pub slot: usize,
    pub stale: bool,
    pub preview: bool,
    pub camera: Camera,
    pub history_epoch: i64,
    /// the last band of its frame: publishes the film
    pub last: bool,
}

/// Pure prepared geometry: a flat triangle mesh or a prototype plus compact instance transforms.
pub type Mesh = mesh_data::MeshData;

/// The shape of the pipeline a mesh needs.
pub enum Queued {
    Full(Mesh),
    Move(Mesh),
}

pub struct PathTracer {
    pub lease: renderer::renderer::Gpu,
    pub device: Device,
    pub queue: Queue,
    pub flow_pipelines: flow_gpu::pipelines::T,
    /// Flow's RGBA8 film conversion
    pub convert: flow_gpu::image_sink::T,
    /// the Lisp kernels' count word, never read
    pub cell: Buffer,
    /// newest first, at most four
    pub kernels: Vec<(lisp_kernel::Facts, Rc<lisp_kernel::T>)>,
    /// a device mesh's corner gather, made on first use
    pub gather: Option<lisp_kernel::Gather>,
    /// device mesh updates refit in place
    pub refits: i64,
    /// the last frame accumulated, so the film is a preview's history
    pub from_accum: bool,
    pub gpu: GpuMesh,
    pub building: Option<MeshBuild>,
    pub queued_mesh: Option<Queued>,
    /// the newest mesh given, for `move`
    pub latest: Mesh,
    pub panels: Buffer,
    pub lights: Buffer,
    pub light_count: i64,
    pub scene_lights: Vec<Light>,
    pub world: Option<rays::world::baked>,
    pub world_buffer: Buffer,
    /// the World's CDF alone, for the Lisp kernel
    pub cdf_buffer: Buffer,
    pub bsdf_only: bool,
    /// the working film; None once released, `render` makes it again
    pub film: Option<Film>,
    /// the output the image samples
    pub published: Option<usize>,
    /// that output, kept from a released film
    pub shown: Option<Texture>,
    pub shared_film: bool,
    pub next_output_slot: usize,
    pub history_slot: usize,
    pub history_camera: Option<Camera>,
    pub history_epoch: i64,
    /// oldest first, at most two in flight
    pub pending: Vec<Pending>,
    /// freed once no frame is in flight
    pub retired: Vec<Box<dyn FnOnce()>>,
    /// row bands per accumulation frame
    pub bands: i64,
    /// GPU seconds of the frame in progress
    pub frame_gpu: f64,
    pub completed: i64,
    pub resource: runtime_resources::image::Image,
    pub image: rays::image::Image,
    pub width: i64,
    pub height: i64,
    pub spp: i64,
    pub bounces: i64,
    pub exposure: f64,
    pub round_samples: i64,
    pub profile: bool,
    pub panel_count: i64,
    pub sky: LinearColor,
    pub ground: LinearColor,
    pub frame: i64,
    pub camera: Option<Camera>,
    pub moving: bool,
    pub scene_edited: bool,
}

fn add_f32(builder: &mut Vec<u8>, value: f64) {
    builder.extend_from_slice(&(value as f32).to_le_bytes());
}

fn add_vec3(builder: &mut Vec<u8>, (x, y, z): (f64, f64, f64)) {
    add_f32(builder, x);
    add_f32(builder, y);
    add_f32(builder, z);
}

fn add_float4(builder: &mut Vec<u8>, (x, y, z): (f64, f64, f64), w: f64) {
    add_vec3(builder, (x, y, z));
    add_f32(builder, w);
}

fn vec3_tuple(v: &vec3::T) -> (f64, f64, f64) {
    (v.x, v.y, v.z)
}

fn renderer_err(e: &ogpu_core::error::Error) -> String {
    let mut s = String::new();
    renderer::renderer::pp_error(&mut s, e);
    s
}

fn resources_err(e: &runtime_resources::error) -> String {
    ogpu_core::error::to_string(e)
}

/// Analytic primitives share the material index space with the objects: sphere k's is `first_material + k`,
/// and the sphere records are (centre, radius).
pub fn prepare_extras(first_material: i64, spheres: &[Sphere], strands: &[Strand]) -> Result<mesh_data::Extras, String> {
    let mut sphere_bytes = Vec::new();
    let mut strand_points = Vec::new();
    let mut strand_radii = Vec::new();
    let mut strand_indices = Vec::new();
    let mut strand_ids = Vec::new();
    let finite = |value: f64| value.is_finite() && value.abs() <= 3.402823466e38;
    for s in spheres {
        let (x, y, z) = vec3_tuple(&s.center);
        if !(finite(x) && finite(y) && finite(z) && finite(s.radius) && s.radius > 0.) {
            return Err("path tracer sphere needs a finite center and a positive radius".to_string());
        }
        add_float4(&mut sphere_bytes, (x, y, z), s.radius);
    }
    let mut point_count = 0i64;
    let mut segments = 0i64;
    for (index, s) in strands.iter().enumerate() {
        if s.points.len() < 2 {
            return Err("path tracer strand needs at least two points".to_string());
        } else if !(finite(s.thickness) && s.thickness > 0.) {
            return Err("path tracer strand needs a positive finite thickness".to_string());
        } else if s.points.iter().any(|p| {
            let (x, y, z) = vec3_tuple(p);
            !(finite(x) && finite(y) && finite(z))
        }) {
            return Err("path tracer strand points must be finite".to_string());
        }
        let first = point_count;
        for p in &s.points {
            add_vec3(&mut strand_points, vec3_tuple(p));
            add_f32(&mut strand_radii, s.thickness * 0.5);
        }
        point_count += s.points.len() as i64;
        for segment in 0..s.points.len() - 1 {
            strand_indices.extend_from_slice(&((first + segment as i64) as i32).to_le_bytes());
            strand_ids.extend_from_slice(&((first_material + spheres.len() as i64 + index as i64) as i32).to_le_bytes());
            segments += 1;
        }
    }
    Ok(mesh_data::Extras {
        sphere_bytes,
        sphere_material: first_material,
        sphere_count: spheres.len() as i64,
        points: strand_points,
        radii: strand_radii,
        indices: strand_indices,
        ids: strand_ids,
        point_count,
        segments,
    })
}

/// A lone triangle device mesh with normals (`Rays.Mesh.View.create_device`) is traced from the device: its
/// corners are gathered there, never read back.
/// ponytail: float4 colours take the host route; a vec4 gather is the upgrade if a sketch needs them.
pub fn device_of(m: &rays::mesh::Mesh) -> Option<mesh_data::Device> {
    match rays::mesh::view::device(m) {
        Some((key, true)) if rays::mesh::mode(m) == rays::mesh::mode::Triangles => {
            let index_count = rays::mesh::index_count(m);
            let width = rays::mesh::view::device_color_width(m);
            if index_count > 0 && index_count % 3 == 0 && width != 4 {
                Some(mesh_data::Device { key, vertex_count: rays::mesh::vertex_count(m), index_count, corners: rays::mesh::view::unread(m).indices, colored: width == 3, holder: m.clone() })
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Triangles become one primitive structure; spheres and strands are traced beside it. A lone triangle
/// device mesh with normals and host corner indices (below 2^24 vertices) is traced where the device holds
/// it: the tracer gathers its corners from the registered positions and normals on the device into a
/// refittable structure, and a later mesh of the same topology refits that structure in place instead of
/// rebuilding it. Such a mesh cannot join `mesh_instanced` or `scene_mesh`.
pub fn mesh(spheres: Option<&[Sphere]>, strands: Option<&[Strand]>, objects: &[(rays::mesh::Mesh, Material)]) -> Result<Mesh, String> {
    let spheres = spheres.unwrap_or(&[]);
    let strands = strands.unwrap_or(&[]);
    let extras = prepare_extras(objects.len() as i64, spheres, strands)?;
    let mut materials: Vec<Material> = objects.iter().map(|(_, m)| m.clone()).collect();
    materials.extend(spheres.iter().map(|s| s.sphere_material.clone()));
    materials.extend(strands.iter().map(|s| s.strand_material.clone()));
    let devices: Vec<Option<mesh_data::Device>> = objects.iter().map(|(m, _)| device_of(m)).collect();
    if spheres.is_empty() && strands.is_empty() && devices.len() == 1 && devices[0].is_some() {
        let device = devices[0].clone().unwrap();
        Ok(mesh_data::MeshData {
            triangles: device.index_count / 3,
            device: Some(device),
            positions: vec![],
            normals: vec![],
            colors: vec![],
            ids: vec![],
            materials,
            instances: None,
            extras,
            scene: None,
        })
    } else {
        mesh_data::mesh(extras, materials, &objects.iter().map(|(m, _)| m.clone()).collect::<Vec<_>>())
    }
}

/// One prototype and its instance transforms, without duplicated triangles. `materials` (one per instance)
/// overrides the prototype material per instance through the instance user id. `motion` gives each instance
/// a second transform at the end of the shutter; every sample picks a shutter time, so instances blur along
/// their motion.
pub fn mesh_instanced(prototype: &Mesh, materials: Option<&[Material]>, motion: Option<&[rays_math::mat4::T]>, transforms: &[rays_math::mat4::T]) -> Result<Mesh, String> {
    mesh_data::mesh_instanced(prototype, materials, motion, transforms)
}

/// Several prepared meshes placed at their world transforms in one traceable scene: an instance structure
/// over one primitive structure per input.
pub fn scene_mesh(objects: &[(rays_math::mat4::T, Mesh)]) -> Result<Mesh, String> {
    mesh_data::scene_mesh(objects)
}

pub fn moved_mesh(m: &Mesh, matrices: &[rays_math::mat4::T]) -> Result<Mesh, String> {
    mesh_data::moved_mesh(m, matrices)
}

pub fn material_bytes(materials: &[Material]) -> Vec<u8> {
    let mut builder = Vec::with_capacity(256);
    for m in materials {
        add_float4(&mut builder, color_tuple(&m.albedo), m.roughness);
        add_float4(&mut builder, color_tuple(&m.emission), m.metallic);
        add_float4(&mut builder, (m.round, 0., 0.), 0.);
    }
    builder
}

fn upload_mesh_async(reuse: Option<&GpuMesh>, device: &Device, queue: &Queue, m: &Mesh) -> Result<MeshBuild, String> {
    crate::mesh_build::upload_mesh_async(reuse, &material_bytes(&m.materials), device, queue, m)
}

fn upload_mesh(device: &Device, queue: &Queue, m: &Mesh) -> Result<GpuMesh, String> {
    let build = upload_mesh_async(None, device, queue, m)?;
    finish_build(device, queue, build)
}

pub fn panel_bytes(panels: &[Panel]) -> Vec<u8> {
    let mut builder = Vec::with_capacity(256);
    let normalize = |v: &vec3::T| {
        let l = (v.x * v.x + v.y * v.y + v.z * v.z).sqrt();
        let l = if l == 0. { 1. } else { l };
        (v.x / l, v.y / l, v.z / l)
    };
    for p in panels {
        let (r, g, b_) = color_tuple(&p.color);
        add_float4(&mut builder, normalize(&p.direction), 0.);
        add_float4(&mut builder, (r * p.intensity, g * p.intensity, b_ * p.intensity), 0.);
        add_float4(&mut builder, (p.width, p.height, p.softness), 0.);
    }
    if panels.is_empty() {
        builder.extend_from_slice(&[0u8; 48]);
    }
    builder
}

pub fn light_bytes(world_lights: &[rays::world::light], lights: &[Light]) -> Vec<u8> {
    let mut builder = Vec::with_capacity(256);
    let normalize = |v: vec3::T| {
        let l = (v.x * v.x + v.y * v.y + v.z * v.z).sqrt();
        if l == 0. { vec3::create(0., 0., 1.) } else { vec3::create(v.x / l, v.y / l, v.z / l) }
    };
    for l in lights {
        let (w, h) = l.size;
        let forward = normalize(vec3::sub(l.target, l.at));
        let world_up = if forward.y.abs() > 0.999 { vec3::create(0., 0., 1.) } else { vec3::create(0., 1., 0.) };
        let right = normalize(vec3::cross(forward, world_up));
        let up = vec3::cross(right, forward);
        let u = vec3::create(right.x * w, right.y * w, right.z * w);
        let v = vec3::create(up.x * h, up.y * h, up.z * h);
        let origin = vec3::create(l.at.x - (u.x + v.x) / 2., l.at.y - (u.y + v.y) / 2., l.at.z - (u.z + v.z) / 2.);
        let (r, g, b_) = color_tuple(&l.color);
        add_float4(&mut builder, (origin.x, origin.y, origin.z), 0.);
        add_float4(&mut builder, (u.x, u.y, u.z), 0.);
        add_float4(&mut builder, (v.x, v.y, v.z), 0.);
        add_float4(&mut builder, (r * l.intensity, g * l.intensity, b_ * l.intensity), w * h);
    }
    // World lights are one-sided (origin.w = 1): the edges are ordered so cross(u, v) is the emitting normal.
    for l in world_lights {
        let u = vec3::scale(l.u, 2.);
        let v = vec3::scale(l.v, 2.);
        let n = vec3::cross(u, v);
        let (u, v) = if vec3::dot(n, l.normal) < 0. { (v, u) } else { (u, v) };
        let origin = vec3::sub(vec3::sub(l.position, l.u), l.v);
        add_float4(&mut builder, (origin.x, origin.y, origin.z), 1.);
        add_float4(&mut builder, (u.x, u.y, u.z), 0.);
        add_float4(&mut builder, (v.x, v.y, v.z), 0.);
        add_float4(&mut builder, (l.radiance.r, l.radiance.g, l.radiance.b), vec3::length(n));
    }
    if lights.is_empty() && world_lights.is_empty() {
        builder.extend_from_slice(&[0u8; 64]);
    }
    builder
}

/// The baked World as two float32 buffers: the camera then the lighting map, and the CDF marginal then its
/// conditional rows (see pathtrace.rays).
pub fn float32_bytes(parts: &[&[f64]]) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(4 * parts.iter().map(|a| a.len()).sum::<usize>());
    for array in parts {
        for v in array.iter() {
            add_f32(&mut bytes, *v);
        }
    }
    bytes
}

pub fn world_bytes(baked: &rays::world::baked) -> Result<(Vec<u8>, Vec<u8>), String> {
    let (width, height) = (baked.camera.width, baked.camera.height);
    let cdf = &baked.cdf;
    if width < 1
        || height < 1
        || baked.lighting.width != width
        || baked.lighting.height != height
        || baked.camera.pixels.len() as i64 != width * height * 3
        || baked.lighting.pixels.len() as i64 != width * height * 3
        || cdf.marginal.len() as i64 != height + 1
        || cdf.conditional.len() as i64 != height * (width + 1)
    {
        Err("path tracer World maps and CDF do not match in size".to_string())
    } else {
        Ok((float32_bytes(&[&baked.camera.pixels, &baked.lighting.pixels]), float32_bytes(&[&cdf.marginal, &cdf.conditional])))
    }
}

/// Resource helpers on the leased OGPU device. Scene geometry lives in shared memory (Apple silicon is
/// unified); accumulation, history, and build scratch are device-local.
pub fn unavailable(caps: &ogpu_core::caps::Caps) -> Option<String> {
    if ogpu_core::caps::has(caps, &ogpu_core::caps::Feature::RayTracing) { None } else { Some("Path tracing is not available on this GPU.".to_string()) }
}

pub fn available() -> Option<String> {
    match renderer::renderer::acquire_gpu() {
        Err(error) => Some(renderer_err(&error)),
        Ok(lease) => {
            let reason = unavailable(&b::capabilities(&renderer::renderer::gpu_device(&lease)));
            renderer::renderer::release_gpu(&lease);
            reason
        }
    }
}

/// The optional arguments of `create`.
#[derive(Clone, Debug, Default)]
pub struct CreateOptions {
    pub spp: Option<i64>,
    pub bounces: Option<i64>,
    pub exposure: Option<f64>,
    /// the number of probe rays per camera-visible shading point for round-corner materials (default 4);
    /// secondary bounces use a quarter
    pub round_samples: Option<i64>,
    /// disables all light sampling (default false)
    pub bsdf_only: Option<bool>,
    /// logs each GPU band
    pub profile: Option<bool>,
}

fn create_tracer(o: &CreateOptions, width: i64, height: i64, scene_: &Scene) -> Result<PathTracer, String> {
    if width <= 0 || height <= 0 {
        return Err("path tracer size must be positive".to_string());
    }
    let (spp, bounces) = (o.spp.unwrap_or(1), o.bounces.unwrap_or(6));
    if spp <= 0 || bounces <= 0 {
        return Err("path tracer spp and bounces must be positive".to_string());
    }
    let lease = renderer::renderer::acquire_gpu().map_err(|e| renderer_err(&e))?;
    let device = renderer::renderer::gpu_device(&lease);
    let queue = renderer::renderer::gpu_queue(&lease);
    let created = (|| -> Result<PathTracer, String> {
        ogpu_core::caps::require(Some("Rays_pathtracer.create"), &b::capabilities(&device), &ogpu_core::caps::Feature::RayTracing).map_err(gpu_err)?;
        let m = mesh(Some(&scene_.spheres), Some(&scene_.strands), &scene_.objects)?;
        // a device mesh is installed by [replace_mesh] once the tracer exists, over a stand-in
        let gpu_mesh_ = upload_mesh(
            &device,
            &queue,
            &if m.device.is_none() {
                m.clone()
            } else {
                mesh_data::MeshData { device: None, positions: vec![0; 36], normals: vec![0; 36], colors: vec![], ids: vec![0; 4], triangles: 1, ..m.clone() }
            },
        )?;
        let panels = shared(&device, "pathtracer-panels", &panel_bytes(&scene_.environment.panels))?;
        let lights = shared(&device, "pathtracer-lights", &light_bytes(&[], &scene_.lights))?;
        let world_buffer = shared(&device, "pathtracer-world", &[0u8; 16])?;
        let cdf_buffer = shared(&device, "pathtracer-cdf", &[0u8; 16])?;
        let cell = frame_data::buffer(Memory::Shared, vec![BufferUsage::Storage], &device, "pathtracer-count", 24)?;
        let convert = flow_gpu::image_sink::create(&lease).map_err(|d| rays_diagnostic::to_string(&d))?;
        let film_ = allocate_film(&device, width, height)?;
        let pixels = vec![0u8; (width * height * 4) as usize];
        let resource = runtime_resources::image::create(width, height, &pixels).map_err(|e| resources_err(&e))?;
        let image = rays::image::of_resource(resource.clone())?;
        Ok(PathTracer {
            lease: lease.clone(),
            device: device.clone(),
            queue: queue.clone(),
            flow_pipelines: flow_gpu::pipelines::create(&|| 0.0, &device),
            convert,
            cell,
            kernels: vec![],
            gather: None,
            refits: 0,
            from_accum: false,
            gpu: gpu_mesh_,
            building: None,
            queued_mesh: None,
            latest: m,
            panels,
            lights,
            light_count: scene_.lights.len() as i64,
            scene_lights: scene_.lights.clone(),
            world: None,
            world_buffer,
            cdf_buffer,
            bsdf_only: o.bsdf_only.unwrap_or(false),
            film: Some(film_),
            published: None,
            shown: None,
            shared_film: renderer::renderer::gpu_shared(&lease),
            next_output_slot: 0,
            history_slot: 0,
            history_camera: None,
            history_epoch: 0,
            pending: vec![],
            retired: vec![],
            bands: 3,
            frame_gpu: 0.,
            completed: 0,
            resource,
            image,
            width,
            height,
            spp,
            bounces,
            exposure: o.exposure.unwrap_or(1.),
            round_samples: o.round_samples.unwrap_or(4),
            profile: o.profile.unwrap_or(false),
            panel_count: scene_.environment.panels.len() as i64,
            sky: scene_.environment.sky,
            ground: scene_.environment.ground,
            frame: 0,
            camera: None,
            moving: false,
            scene_edited: false,
        })
    })();
    if created.is_err() {
        // ponytail: partial creation failures release the lease and let the device report leaked children;
        // creation is all-or-nothing in tests.
        renderer::renderer::release_gpu(&lease);
    }
    created
}

fn drain(t: &mut PathTracer) {
    if t.pending.is_empty() {
        let retired = std::mem::take(&mut t.retired);
        for free in retired.into_iter().rev() {
            free();
        }
    }
}

fn retire(t: &mut PathTracer, free: Box<dyn FnOnce()>) {
    if t.pending.is_empty() {
        free()
    } else {
        t.retired.push(free)
    }
}

fn reset_samples(t: &mut PathTracer) {
    t.frame = 0;
    t.completed = 0;
}

fn restart(t: &mut PathTracer) {
    reset_samples(t);
    for p in t.pending.iter_mut() {
        p.stale = true;
    }
    t.history_camera = None;
    t.history_epoch += 1;
}

fn live_film(t: &PathTracer) -> &Film {
    t.film.as_ref().expect("film")
}

/// Restarts accumulation; the next frame renders at full quality even right after a scene edit.
pub fn reset(t: &mut PathTracer) {
    restart(t);
    t.scene_edited = false;
}

/// Geometry, lights, or World just became current: the next `render` is a preview frame, like a camera
/// move, so continuous edits stay interactive.
fn edited(t: &mut PathTracer) {
    restart(t);
    t.scene_edited = true;
}

/// A reused prototype or a `move` shares the installed geometry and BLASes; ownership passes to the new mesh.
fn install_gpu(t: &mut PathTracer, mut gpu: GpuMesh) {
    if b::buffer_id(&t.gpu.positions) == b::buffer_id(&gpu.positions) {
        t.gpu.owns_prototype = false;
        gpu.owns_prototype = true;
    }
    let old = std::mem::replace(&mut t.gpu, gpu);
    retire(t, Box::new(move || destroy_gpu(&old)));
    edited(t);
}

/// Accumulated samples per pixel.
pub fn samples(t: &PathTracer) -> i64 {
    t.completed * t.spp
}

pub fn size(t: &PathTracer) -> (i64, i64) {
    (t.width, t.height)
}

/// Borrowed; destroyed by `destroy`. In a native Sketch, Scene samples its completed GPU film directly.
pub fn image(t: &PathTracer) -> rays::image::Image {
    t.image.clone()
}

const BAND_MS: f64 = 6.;

/// Publishes a completed frame's pixels unless a reset made them obsolete.
fn publish(t: &mut PathTracer, p: &Pending) -> Result<(), String> {
    let seconds = b::gpu_duration(&t.queue, p.receipt);
    if t.profile
        && let Some(seconds) = seconds {
            eprintln!("pathtracer gpu_ms {:.4}", seconds * 1000.);
        }
    if !p.preview {
        t.frame_gpu += seconds.unwrap_or(0.);
        if p.last {
            if seconds.is_some() {
                t.bands = 1.max(3.min((t.frame_gpu * 1000. / BAND_MS).ceil() as i64));
            }
            t.frame_gpu = 0.;
        }
    }
    if p.history_epoch == t.history_epoch {
        t.history_camera = Some(p.camera.clone());
    }
    if p.stale || !p.last {
        return Ok(());
    }
    let output = live_film(t).outputs[p.slot].clone();
    if t.shared_film {
        runtime_resources::image::replace_gpu(&t.resource, &output).map_err(|e| resources_err(&e))?;
        t.published = Some(p.slot);
        // the image has left a released film's output
        if let Some(shown) = t.shown.take() {
            let _ = b::destroy_texture(&shown);
        }
    } else {
        let rgba = b::read_texture(&output, t.width * 4).map_err(gpu_err)?;
        runtime_resources::image::replace(&t.resource, t.width, t.height, &rgba).map_err(|e| resources_err(&e))?;
    }
    if !p.preview {
        t.completed += 1;
    }
    Ok(())
}

fn status(t: &PathTracer, receipt: Receipt) -> Result<Status, String> {
    b::status(&t.queue, receipt).map_err(gpu_err)
}

/// A transform-only build: fresh instance records and shader transforms, and a new TLAS over the installed
/// scene's BLASes; geometry, materials, and the table are borrowed (owns_prototype = false) until
/// `install_gpu`.
fn move_async(t: &PathTracer, moved: &Mesh) -> Result<MeshBuild, String> {
    match (&moved.instances, &moved.scene) {
        // ponytail: installed-scene identity is checked only as far as memory safety needs (BLAS and
        // instance counts); `move` after a failed rebuild may place the old geometry.
        (Some(instances), Some(layout)) if t.gpu.scene_blas.len() == layout.parts.len() && t.gpu.tables.instance_count == instances.records.len() as i64 => {
            let mut owned: Vec<Buffer> = Vec::new();
            let mut build_slot: Option<MeshBuild> = None;
            let started = (|| -> Result<(), String> {
                let packed = b::pack_instance_records(&t.device, &instances.records).map_err(gpu_err)?;
                let instance_buffer = shared(&t.device, "pathtracer-instances", &packed)?;
                owned.push(instance_buffer.clone());
                let scratch = frame_data::buffer(Memory::DeviceLocal, vec![BufferUsage::Storage], &t.device, "pathtracer-scratch", 1024)?;
                owned.push(scratch.clone());
                let tables = upload_tables(&material_bytes(&moved.materials), &t.device, moved)?;
                owned.extend(table_buffers(&tables));
                let mut build = MeshBuild {
                    gpu: GpuMesh { instance_buffer: Some(instance_buffer), owns_prototype: false, tables, ..t.gpu.clone() },
                    scratch,
                    extra_scratch: None,
                    size_buffer: None,
                    uncompacted: None,
                    uncompacted_scene: vec![],
                    stage: Stage::Ready,
                    instance_count: instances.records.len() as i64,
                    instances_built: false,
                    proto_scratch: None,
                };
                match submit_instances(&t.device, &t.queue, &mut build) {
                    Ok(()) => {
                        build_slot = Some(build);
                        Ok(())
                    }
                    Err(e) => {
                        if let Some(extra) = &build.extra_scratch {
                            let _ = b::destroy_buffer(extra);
                        }
                        if build.instances_built {
                            let _ = b::destroy_accel(&build.gpu.structure);
                        }
                        Err(e)
                    }
                }
            })();
            match started {
                Ok(()) => Ok(build_slot.expect("build")),
                Err(e) => {
                    for value in &owned {
                        let _ = b::destroy_buffer(value);
                    }
                    Err(e)
                }
            }
        }
        _ => Err("path tracer move does not match the installed scene".to_string()),
    }
}

fn ensure_gather(t: &mut PathTracer) -> Result<(), String> {
    if t.gather.is_none() {
        t.gather = Some(lisp_kernel::gather_kernel(&t.lease, &t.flow_pipelines)?);
    }
    Ok(())
}

/// What a failed device build releases.
struct DeviceOwned {
    owned: Vec<Buffer>,
    structure: Option<Accel>,
    commands: Option<Commands>,
}

fn local(t: &PathTracer, o: &mut DeviceOwned, label: &str, size: i64) -> Result<Buffer, String> {
    let value = frame_data::buffer(Memory::DeviceLocal, vec![BufferUsage::Storage], &t.device, label, size)?;
    o.owned.push(value.clone());
    Ok(value)
}

/// One geometry's gather passes into the tracer's per-corner buffers.
struct GatherInputs {
    positions: Buffer,
    normals: Buffer,
    colors: Option<Buffer>,
    corners: i64,
    vertices: i64,
}

fn encode_gather(t: &PathTracer, commands: &Commands, g: &GatherInputs, vix: &Buffer, into_positions: &Buffer, into_normals: &Buffer, into_colors: &Buffer) -> Result<(), String> {
    let gather = t.gather.as_ref().expect("gather kernel");
    let one = |src: &Buffer, into: &Buffer| lisp_kernel::gather_dispatch(gather, commands, &t.cell, vix, g.corners, src, g.vertices, into);
    one(&g.positions, into_positions)?;
    one(&g.normals, into_normals)?;
    match &g.colors {
        Some(colors) => one(colors, into_colors),
        None => Ok(()),
    }
}

fn corner_triangles(soup: &Buffer, corners: i64) -> AccelDescriptor {
    AccelDescriptor::Blas { geometries: vec![Geometry::Triangles { vertices: soup.clone(), offset: 0, length: 12 * corners, vertex_stride: 12, vertex_count: corners }] }
}

/// A device mesh's update (`Mesh_data.device`): its corners gathered from the device's positions and normals
/// into the tracer's per-corner buffers (one compute pass each), then its structure. The same topology (the
/// corners, vertex count and materials of the installed device mesh) refits the installed structure in place,
/// ordered on the queue before every later frame: `None`, nothing to install. Anything else builds a
/// refittable structure over fresh buffers, installed when done.
/// ponytail: the structure is not compacted (a refit needs the uncompacted one).
fn device_async(t: &mut PathTracer, m: &Mesh, d: &mesh_data::Device) -> Result<Option<MeshBuild>, String> {
    let (positions, normals) = match renderer::renderer::gpu_positions(&t.lease, &d.key) {
        Some((positions, Some(normals), count)) if count == d.vertex_count => (positions, normals),
        _ => return Err("path tracer device mesh is stale or carries no normals".to_string()),
    };
    ensure_gather(t)?;
    let (indices, colors) = renderer::renderer::gpu_streams(&t.lease, &d.key).unwrap_or((None, None));
    let colors = match (d.colored, colors) {
        (true, Some((colors, 3))) => Some(colors),
        (false, _) => None,
        (true, _) => return Err("path tracer device mesh's colours are stale".to_string()),
    };
    let corners = d.index_count;
    let material_key = material_bytes(&m.materials);
    let g = GatherInputs { positions, normals, colors, corners, vertices: d.vertex_count };
    // the producer's stamp of the borrowed index content; host corners carry none
    let topology = (if d.corners.is_empty() { renderer::renderer::gpu_topology(&t.lease, &d.key) } else { 0 }, corners);
    let device_indices = indices.as_ref().filter(|(_, n)| *n == corners).map(|(buffer, _)| buffer.clone());
    let reusable = match &t.gpu.device {
        Some(installed) => {
            (!d.corners.is_empty() || (topology.0 != 0 && topology == installed.topology && device_indices.is_some()))
                && installed.vertex_count == d.vertex_count
                && t.gpu.shape.colored == d.colored
                && installed.material_key == material_key
                // the same topology re-derived hands a fresh array: one host compare instead of a rebuild.
                // ponytail: a producer's topology stamp is the upgrade if a fresh array every frame shows up
                && (std::sync::Arc::ptr_eq(&installed.corners, &d.corners) || *installed.corners == *d.corners)
        }
        None => false,
    };
    if reusable {
        let installed = t.gpu.device.clone().unwrap();
        // the last gather read the previous mesh's buffers: done before that mesh is let go
        wait(&t.queue, installed.receipt)?;
        let commands = b::begin_commands(&t.queue).map_err(gpu_err)?;
        // equal stamps promise equal indices; the producer's current buffer is the live one, the installed may be pooled again
        let vix = if topology.0 != 0 { device_indices.clone().unwrap() } else { installed.vix.clone() };
        let encoded = (|| -> Result<Receipt, String> {
            encode_gather(t, &commands, &g, &vix, &t.gpu.positions, &t.gpu.normals, &t.gpu.colors)?;
            let encoder = b::accel_encoder(&commands).map_err(gpu_err)?;
            b::refit_accel(&encoder, &t.gpu.structure, corner_triangles(&t.gpu.positions, corners), &installed.refit_scratch, None).map_err(gpu_err)?;
            b::end_accel(&encoder).map_err(gpu_err)?;
            b::commit(&commands).map_err(gpu_err)
        })();
        return match encoded {
            Ok(receipt) => {
                let slot = t.gpu.device.as_mut().unwrap();
                slot.source = d.holder.clone();
                slot.corners = d.corners.clone();
                slot.vix = vix;
                slot.receipt = receipt;
                t.refits += 1;
                edited(t);
                Ok(None)
            }
            Err(e) => {
                let _ = b::abandon(&commands);
                Err(e)
            }
        };
    }
    let mut o = DeviceOwned { owned: vec![], structure: None, commands: None };
    let built = device_build(t, &mut o, m, d, &g, indices, &material_key, topology);
    match built {
        Ok(build) => Ok(Some(build)),
        Err(e) => {
            if let Some(c) = &o.commands {
                let _ = b::abandon(c);
            }
            if let Some(blas) = &o.structure {
                let _ = b::destroy_accel(blas);
            }
            for value in &o.owned {
                let _ = b::destroy_buffer(value);
            }
            Err(e)
        }
    }
}

fn device_build(t: &PathTracer, o: &mut DeviceOwned, m: &Mesh, d: &mesh_data::Device, g: &GatherInputs, indices: Option<(Buffer, i64)>, material_key: &[u8], topology: (u64, i64)) -> Result<MeshBuild, String> {
    let corners = g.corners;
    let held = indices.and_then(|(buffer, n)| if n == corners { Some(buffer) } else { None });
    let vix = match &held {
        Some(buffer) => buffer.clone(),
        None => {
            if d.corners.is_empty() {
                return Err("path tracer device mesh's indices are stale".to_string());
            }
            let mut bytes = vec![0u8; 4 * corners as usize];
            for (i, v) in d.corners.iter().enumerate() {
                put_u32(&mut bytes, 4 * i, *v);
            }
            let value = shared(&t.device, "pathtracer-corners", &bytes)?;
            o.owned.push(value.clone());
            value
        }
    };
    let soup_positions = local(t, o, "pathtracer-positions", 12 * corners)?;
    let soup_normals = local(t, o, "pathtracer-normals", 12 * corners)?;
    let soup_colors = local(t, o, "pathtracer-colors", if d.colored { 12 * corners } else { 12 })?;
    let materials = shared(&t.device, "pathtracer-materials", material_key)?;
    o.owned.push(materials.clone());
    let tables = upload_tables(material_key, &t.device, &mesh_data::MeshData { ids: vec![0; 4], ..m.clone() })?;
    o.owned.extend(table_buffers(&tables));
    let blas = b::create_accel(&t.device, corner_triangles(&soup_positions, corners), true).map_err(gpu_err)?;
    o.structure = Some(blas.clone());
    let sizes = b::accel_sizes(&blas);
    let work = local(t, o, "pathtracer-scratch", sizes.build_scratch_size)?;
    let refit_scratch = local(t, o, "pathtracer-refit-scratch", sizes.refit_scratch_size)?;
    let c = b::begin_commands(&t.queue).map_err(gpu_err)?;
    o.commands = Some(c.clone());
    encode_gather(t, &c, g, &vix, &soup_positions, &soup_normals, &soup_colors)?;
    let encoder = b::accel_encoder(&c).map_err(gpu_err)?;
    b::build_accel(&encoder, &blas, &work, None).map_err(gpu_err)?;
    b::end_accel(&encoder).map_err(gpu_err)?;
    let receipt = b::commit(&c).map_err(gpu_err)?;
    o.commands = None;
    Ok(MeshBuild {
        gpu: GpuMesh {
            device: Some(DeviceGpu {
                corners: d.corners.clone(),
                vertex_count: d.vertex_count,
                material_key: material_key.to_vec(),
                vix,
                topology: if held.is_some() { topology } else { (0, corners) },
                owns_vix: held.is_none(),
                refit_scratch,
                source: d.holder.clone(),
                receipt,
            }),
            positions: soup_positions,
            normals: soup_normals,
            colors: soup_colors,
            materials,
            curves: None,
            native_curves: false,
            prototypes: None,
            structure: blas,
            primitive: None,
            instance_buffer: None,
            motion_buffer: None,
            prototype_key: None,
            scene_blas: vec![],
            scene_unpacked: 0,
            tables,
            owns_prototype: true,
            shape: mesh_shape(m),
        },
        scratch: work,
        extra_scratch: None,
        size_buffer: None,
        uncompacted: None,
        uncompacted_scene: vec![],
        stage: Stage::Compacting(receipt),
        instance_count: 0,
        instances_built: false,
        proto_scratch: None,
    })
}

/// The build a queued mesh starts: `None` for a device refit, already in place.
fn build_async(t: &mut PathTracer, m: &Mesh) -> Result<Option<MeshBuild>, String> {
    match &m.device {
        Some(d) => device_async(t, m, d),
        None => upload_mesh_async(Some(&t.gpu), &t.device, &t.queue, m).map(Some),
    }
}

/// A build or move can start only while no frame band holds a queue slot.
fn start_queued(t: &mut PathTracer) -> Result<(), String> {
    match t.queued_mesh.take() {
        Some(Queued::Full(m)) => {
            let next = build_async(t, &m)?;
            t.building = next;
            Ok(())
        }
        Some(Queued::Move(moved)) => {
            let next = move_async(t, &moved)?;
            t.building = Some(next);
            Ok(())
        }
        None => Ok(()),
    }
}

fn poll_build(t: &mut PathTracer) -> Result<(), String> {
    let Some(mut build) = t.building.take() else {
        return if t.pending.is_empty() { start_queued(t) } else { Ok(()) };
    };
    let step = match build.stage {
        Stage::Ready => Ok(true),
        Stage::Building(receipt) | Stage::Compacting(receipt) => match status(t, receipt) {
            Ok(Status::Pending) => Ok(false),
            Ok(Status::Completed) => advance_build(&t.device, &t.queue, &mut build).map(|()| build.stage == Stage::Ready),
            Err(e) => Err(e),
        },
    };
    match step {
        Ok(false) => {
            t.building = Some(build);
            Ok(())
        }
        Err(e) => {
            discard_build(&t.queue, &build);
            Err(e)
        }
        Ok(true) => {
            let built = finish_build(&t.device, &t.queue, build);
            match t.queued_mesh.take() {
                Some(Queued::Full(latest)) => {
                    // Installed, not dropped: a mesh that changes every frame always has a newer one queued.
                    let gpu = built?;
                    install_gpu(t, gpu);
                    let next = build_async(t, &latest)?;
                    t.building = next;
                    Ok(())
                }
                Some(Queued::Move(moved)) => {
                    // The move places the BLASes just built, so they install first.
                    let gpu = built?;
                    install_gpu(t, gpu);
                    let next = move_async(t, &moved)?;
                    t.building = Some(next);
                    Ok(())
                }
                None => {
                    let gpu = built?;
                    install_gpu(t, gpu);
                    Ok(())
                }
            }
        }
    }
}

/// Publishes the in-flight frame and waits for queued geometry; `render` then `flush` is a synchronous
/// render. `render` itself never blocks: while the GPU is still busy with the previous frame it returns
/// without submitting, so the caller's loop keeps its own frame rate.
pub fn flush(t: &mut PathTracer) -> Result<(), String> {
    loop {
        if t.pending.is_empty() {
            drain(t);
            break;
        }
        let p = t.pending.remove(0);
        wait(&t.queue, p.receipt)?;
        publish(t, &p)?;
    }
    poll_build(t)?;
    loop {
        let Some(build) = &t.building else { return Ok(()) };
        let waited = match build.stage {
            Stage::Ready => Ok(()),
            Stage::Building(receipt) | Stage::Compacting(receipt) => wait(&t.queue, receipt),
        };
        match waited {
            Ok(()) => poll_build(t)?,
            Err(e) => {
                if let Some(build) = t.building.take() {
                    discard_build(&t.queue, &build);
                }
                return Err(e);
            }
        }
    }
}

/// Match a new viewport: waits for the in-flight frame, reallocates the film, and restarts accumulation.
/// Scene geometry is kept. No-op at the same size.
pub fn resize(t: &mut PathTracer, width: i64, height: i64) -> Result<(), String> {
    if width <= 0 || height <= 0 {
        Err("path tracer size must be positive".to_string())
    } else if width == t.width && height == t.height {
        Ok(())
    } else {
        restart(t);
        flush(t)?;
        let pixels = vec![0u8; (width * height * 4) as usize];
        // The image stops sampling the old film before that film is destroyed.
        runtime_resources::image::replace(&t.resource, width, height, &pixels).map_err(|e| resources_err(&e))?;
        if let Some(f) = &t.film {
            destroy_film(None, f);
        }
        t.film = None;
        if let Some(shown) = t.shown.take() {
            let _ = b::destroy_texture(&shown);
        }
        t.published = None;
        t.width = width;
        t.height = height;
        Ok(())
    }
}

fn poll(t: &mut PathTracer) -> Result<bool, String> {
    loop {
        if t.pending.is_empty() {
            drain(t);
            return Ok(true);
        }
        let receipt = t.pending[0].receipt;
        match status(t, receipt) {
            Ok(Status::Completed) => {
                let p = t.pending.remove(0);
                publish(t, &p)?;
            }
            Ok(Status::Pending) => return Ok(false),
            Err(message) => {
                t.pending.remove(0);
                return Err(format!("path tracer frame failed: {message}"));
            }
        }
    }
}

/// The tracer is done sampling for now: frees the working film, keeping what `image` shows. The next
/// `render` allocates it again and accumulates from no samples, its preview without history for one frame. A
/// frame still in flight leaves the film in place; call again later.
pub fn release_film(t: &mut PathTracer) -> Result<(), String> {
    let Some(film_) = t.film.clone() else { return Ok(()) };
    let free = poll(t)?;
    if !free {
        return Ok(());
    }
    let keep = t.published;
    let shown = keep.map(|k| film_.outputs[k].clone());
    destroy_film(keep, &film_);
    t.shown = shown;
    t.film = None;
    Ok(())
}

fn restore_film(t: &mut PathTracer) -> Result<(), String> {
    if t.film.is_some() {
        return Ok(());
    }
    let f = allocate_film(&t.device, t.width, t.height)?;
    t.film = Some(f);
    restart(t);
    t.next_output_slot = 0;
    t.history_slot = 0;
    t.from_accum = false;
    Ok(())
}

/// The kernels' scene facts: what the static stage folds.
fn lisp_facts(t: &PathTracer) -> lisp_kernel::Facts {
    let map = |w: &dyn Fn(&rays::world::map) -> i64| match &t.world {
        Some(baked) => w(&baked.camera),
        None => 1,
    };
    lisp_kernel::Facts {
        spp: t.spp,
        bounces: t.bounces,
        round_samples: t.round_samples,
        panels: if t.world.is_none() { t.panel_count } else { 0 },
        lights: t.light_count,
        world: t.world.is_some(),
        background: match &t.world {
            Some(rays::world::baked { background: rays::world::background::Color(_), .. }) => 1,
            Some(rays::world::baked { background: rays::world::background::Transparent, .. }) => 2,
            _ => 0,
        },
        map_w: map(&|m| m.width),
        map_h: map(&|m| m.height),
        sun: matches!(&t.world, Some(rays::world::baked { sun: Some(sun), .. }) if sun.angular_radius > 0.),
        bsdf_only: t.bsdf_only,
        instanced: t.gpu.shape.instanced,
        colored: t.gpu.shape.colored,
        lone: t.gpu.shape.lone,
        classes: t.gpu.tables.classes,
        motion: t.gpu.shape.motion,
        curves: if t.gpu.tables.strand_count == 0 {
            None
        } else if t.gpu.native_curves {
            Some(flow_shader::emit::Curves::Native)
        } else {
            Some(flow_shader::emit::Curves::Loop)
        },
        spheres: t.gpu.tables.sphere_count > 0,
    }
}

/// ponytail: four specializations kept, the oldest closed once no frame reads it; an LRU would keep the
/// shapes an editor toggles between.
fn lisp_kernels(t: &mut PathTracer, facts: &lisp_kernel::Facts) -> Result<Rc<lisp_kernel::T>, String> {
    if let Some((_, k)) = t.kernels.iter().find(|(f, _)| f == facts) {
        return Ok(k.clone());
    }
    let kernels = lisp_kernel::compile(facts)?;
    let keep: Vec<(lisp_kernel::Facts, Rc<lisp_kernel::T>)> = t.kernels.iter().take(3).cloned().collect();
    let old: Vec<Rc<lisp_kernel::T>> = t.kernels.iter().map(|(_, k)| k.clone()).collect();
    for k in old {
        if !keep.iter().any(|(_, kept)| Rc::ptr_eq(kept, &k)) {
            retire(t, Box::new(move || lisp_kernel::close(&k)));
        }
    }
    let mut next = vec![(facts.clone(), kernels.clone())];
    next.extend(keep);
    t.kernels = next;
    Ok(kernels)
}

/// The Lisp kernels' uniforms for a frame whose band covers pixels `[lo, hi)`.
fn lisp_words(t: &PathTracer, camera: &Camera, fov: f64, lo: i64, hi: i64) -> Vec<(&'static str, Vec<f64>)> {
    let v3 = |v: vec3::T| vec![v.x, v.y, v.z];
    let rgb = |c: &LinearColor| vec![c.r, c.g, c.b];
    let zero = vec![0., 0., 0.];
    let aspect = t.width as f64 / t.height as f64;
    let (forward, right, up) = frame_data::axes(camera);
    let previous: Option<(Camera, f64)> = t.history_camera.as_ref().and_then(|p| camera_fov(p).ok().map(|f| (p.clone(), f)));
    let axis = |k: &dyn Fn((vec3::T, vec3::T, vec3::T)) -> vec3::T| match &previous {
        Some((p, _)) => v3(k(frame_data::axes(p))),
        None => zero.clone(),
    };
    let sun = match &t.world {
        Some(rays::world::baked { sun: Some(sun), .. }) if sun.angular_radius > 0. => Some(sun.clone()),
        _ => None,
    };
    // 1 - cos r = 2 sin^2 (r / 2), exact for tiny discs
    let one_minus_cos = match &sun {
        Some(sun) => {
            let s = (sun.angular_radius / 2.).sin();
            2. * s * s
        }
        None => 0.,
    };
    vec![
        ("eye", v3(rays::camera::position(camera))),
        ("fwd", v3(forward)),
        ("rgt", v3(right)),
        ("up", v3(up)),
        ("tanhalf", vec![(fov / 2.).tan()]),
        ("aspect", vec![aspect]),
        ("lens", vec![rays::camera::lens(camera).aperture]),
        ("focus", vec![rays::camera::focus_distance(camera)]),
        ("frame", vec![t.frame as f64]),
        ("lo", vec![lo as f64]),
        ("hi", vec![hi as f64]),
        ("peye", match &previous {
            Some((p, _)) => v3(rays::camera::position(p)),
            None => zero.clone(),
        }),
        ("pfwd", axis(&|a| a.0)),
        ("prgt", axis(&|a| a.1)),
        ("pup", axis(&|a| a.2)),
        ("ptan", vec![match &previous {
            Some((_, f)) => (f / 2.).tan(),
            None => 0.,
        }]),
        ("paspect", vec![aspect]),
        ("pvalid", vec![if previous.is_none() { 0. } else { 1. }]),
        ("pacc", vec![if t.from_accum { 1. } else { 0. }]),
        ("sky", rgb(&t.sky)),
        ("ground", rgb(&t.ground)),
        ("bg", match &t.world {
            Some(rays::world::baked { background: rays::world::background::Color(c), .. }) => vec![c.r, c.g, c.b],
            _ => zero.clone(),
        }),
        ("sundir", match &sun {
            Some(sun) => v3(frame_data::normalize(sun.direction)),
            None => zero.clone(),
        }),
        ("suncos", vec![one_minus_cos]),
        ("sunrad", match &sun {
            Some(sun) => vec![sun.radiance.r, sun.radiance.g, sun.radiance.b],
            None => zero.clone(),
        }),
        ("sunpdf", vec![if sun.is_none() { 0. } else { 1. / (2. * std::f64::consts::PI * one_minus_cos) }]),
        ("exposure", vec![match &t.world {
            Some(baked) => t.exposure * 2f64.powf(baked.exposure),
            None => t.exposure,
        }]),
        ("count", vec![((t.frame + 1) * t.spp) as f64]),
        ("spherebase", vec![t.gpu.tables.sphere_base as f64]),
        ("strandbase", vec![t.gpu.tables.triangle_ids as f64]),
        ("width", vec![t.width as f64]),
        ("height", vec![t.height as f64]),
    ]
}

fn word_lookup(values: &[(&'static str, Vec<f64>)], name: &str) -> Vec<f64> {
    values.iter().find(|(n, _)| *n == name).map(|(_, v)| v.clone()).expect("uniform")
}

/// The buffer and element count behind each of the Lisp kernels' tables.
fn lisp_table(t: &PathTracer, tables: &crate::mesh_build::Tables, geo: &Buffer, pgeo: &Buffer, pcol: &Buffer, name: &str) -> (Buffer, i64) {
    let n = t.width * t.height;
    let (maps, cdf) = match &t.world {
        Some(baked) => {
            let (w, h) = (baked.camera.width, baked.camera.height);
            (2 * w * h, h + 1 + h * (w + 1))
        }
        None => (1, 1),
    };
    let count = |buffer: &Buffer, bytes: i64| b::buffer_size(buffer) / bytes;
    match name {
        "lights" => (t.lights.clone(), 4 * 1.max(t.light_count)),
        "pan" => (t.panels.clone(), 3 * 1.max(t.panel_count)),
        "wmap" => (t.world_buffer.clone(), maps),
        "cdf" => (t.cdf_buffer.clone(), cdf),
        "mats" => (t.gpu.materials.clone(), count(&t.gpu.materials, 16)),
        "ids" => (tables.ids.clone(), tables.id_count),
        "inst" => (tables.inst.clone(), 4 * tables.instance_count),
        "nrm" => (t.gpu.normals.clone(), count(&t.gpu.normals, 12)),
        "cols" => (t.gpu.colors.clone(), count(&t.gpu.colors, 12)),
        "accum" => (live_film(t).accum.clone(), n),
        "geo" => (geo.clone(), n),
        "pgeo" => (pgeo.clone(), n),
        "order" => (live_film(t).order.clone(), n),
        _ => (pcol.clone(), n),
    }
}

/// Flow's RGBA8 conversion of the resolved film into output `slot`.
fn encode_rgba(t: &PathTracer, commands: &Commands, slot: usize) -> Result<(), String> {
    let f = live_film(t);
    flow_gpu::image_sink::encode(&t.convert, commands, &f.resolved, t.width, t.height, &f.outputs[slot]).map_err(|d| rays_diagnostic::to_string(&d))
}

/// The Lisp kernels' frame (pathtrace.rays): a preview traces its primary hits, then one centre sample
/// reprojected onto the history, then the 3x3 resolve; an accumulation band adds its pixels' samples to the
/// film (the first band of frame zero also records the primary hits), and the last band resolves it.
fn encode_lisp(t: &PathTracer, kernels: &lisp_kernel::T, commands: &Commands, camera: &Camera, fov: f64, row_offset: i64, rows: i64, last: bool, slot: usize, previous: usize, next: usize) -> Result<(), String> {
    let tables = &t.gpu.tables;
    let scene_ = flow_gpu::run::Scene {
        accel: t.gpu.structure.clone(),
        vertices: t.gpu.positions.clone(),
        spheres: tables.spheres.clone(),
        sphere_count: tables.sphere_count,
        instances: tables.trace_instances.clone(),
    };
    let extras = flow_gpu::run::Extras {
        strands: if tables.strand_count == 0 { None } else { Some(tables.strands.clone()) },
        strand_count: tables.strand_count,
        curves: t.gpu.curves.as_ref().map(|(c, _)| c.clone()),
        prototypes: t.gpu.prototypes.as_ref().map(|(p, _)| p.clone()),
        ..Default::default()
    };
    let n = t.width * t.height;
    let values = lisp_words(t, camera, fov, row_offset * t.width, n.min((row_offset + rows) * t.width));
    let f = live_film(t);
    let geo = f.history_geometry[next].clone();
    let pgeo = f.history_geometry[previous].clone();
    let history = if t.from_accum { f.accum.clone() } else { f.history_color[previous].clone() };
    let words = |name: &str| word_lookup(&values, name);
    let run = |traced: bool, pcol: &Buffer, name: &str, into: &Buffer| {
        let table = |name: &str| lisp_table(t, tables, &geo, &pgeo, pcol, name);
        let mut resident: Vec<Accel> = t.gpu.primitive.iter().cloned().collect();
        resident.extend(t.gpu.scene_blas.iter().cloned());
        lisp_kernel::dispatch(
            kernels,
            &t.lease,
            &t.flow_pipelines,
            commands,
            n,
            if traced { &f.pixels } else { &f.linear },
            Some(&f.linear),
            &t.cell,
            into,
            if traced { Some(scene_.clone()) } else { None },
            Some(extras.clone()),
            resident,
            &words,
            &table,
            name,
        )
    };
    if t.moving {
        run(true, &history, "preview-geometry", &geo)?;
        run(true, &history, "preview", &f.history_color[next])?;
        run(false, &f.history_color[next], "preview-resolve", &f.resolved)?;
    } else {
        if row_offset == 0 && t.frame == 0 {
            run(true, &history, "geometry", &geo)?;
        }
        run(true, &history, "trace", &f.accum)?;
        if last {
            run(false, &history, "resolve", &f.resolved)?;
        }
    }
    if last { encode_rgba(t, commands, slot) } else { Ok(()) }
}

/// One frame of latency: the frame submitted here is read back by the next `render` (or `flush`), so the CPU
/// never waits on the GPU while the display pipeline runs. One sample per call. An accumulation frame is
/// submitted as row bands of about `band_ms` of GPU time each, one command buffer per band, so the window's
/// own raster pass interleaves between them and the UI stays smooth while the tracer takes the rest of the
/// GPU. The band count follows the measured GPU time of the previous frame; the accumulated image does not
/// depend on it.
///
/// Accepts a perspective camera with no lens offset, forced aspect, or vertical flip. Other projections
/// return an error. A frame whose camera differs from the previous call, or the first frame over newly
/// installed geometry, lights, or World, is an interactive preview.
pub fn render(t: &mut PathTracer, camera: &Camera) -> Result<(), String> {
    let fov = camera_fov(camera)?;
    restore_film(t)?;
    let was_moving = t.moving;
    t.moving = match &t.camera {
        Some(previous) => previous != camera,
        None => false,
    };
    if t.camera.as_ref() != Some(camera) || was_moving {
        reset_samples(t);
    }
    t.camera = Some(camera.clone());
    let free = poll(t)?;
    if !free {
        return Ok(());
    }
    poll_build(t)?;
    if t.scene_edited {
        t.scene_edited = false;
        t.moving = true;
    }
    // ponytail: the queue holds three command buffers, one of which a mesh build may hold and one a device
    // mesh's refit not yet waited for; more bands would need lazy submission from `poll`.
    let refitting = match &t.gpu.device {
        Some(installed) if installed.receipt.epoch > b::completed_epoch(&t.queue) => 1,
        _ => 0,
    };
    let free_slots = 3 - if t.building.is_none() { 0 } else { 1 } - refitting;
    let bands = if t.moving { 1 } else { 1.max(t.bands.min(t.height.min(free_slots))) };
    let rows_per_band = (t.height + bands - 1) / bands;
    let slot = t.next_output_slot;
    let previous = t.history_slot;
    let next = previous ^ 1;
    let facts = lisp_facts(t);
    let kernels = lisp_kernels(t, &facts)?;
    for index in 0..bands {
        let row_offset = index * rows_per_band;
        let rows = 1.max(rows_per_band.min(t.height - row_offset));
        let last = index == bands - 1;
        let commands = b::begin_commands(&t.queue).map_err(gpu_err)?;
        let encoded = encode_lisp(t, &kernels, &commands, camera, fov, row_offset, rows, last, slot, previous, next).and_then(|()| b::commit(&commands).map_err(gpu_err));
        match encoded {
            Err(e) => {
                let _ = b::abandon(&commands);
                return Err(e);
            }
            Ok(receipt) => {
                let p = Pending { receipt, slot, stale: false, preview: t.moving, camera: camera.clone(), history_epoch: t.history_epoch, last };
                t.pending.push(p);
            }
        }
    }
    t.next_output_slot = (slot + 1) % 3;
    // Primary hits are recorded only in previews and the first accumulation frame, and the film itself is
    // the history after accumulating.
    if t.moving || t.frame == 0 {
        t.history_slot = next;
    }
    t.from_accum = !t.moving;
    // A preview frame is shown but never counted: accumulation restarts at frame zero once the camera rests.
    if !t.moving {
        t.frame += 1;
    }
    Ok(())
}

/// Synchronously builds and swaps scene geometry, then restarts accumulation.
pub fn replace_mesh(t: &mut PathTracer, m: &Mesh) -> Result<(), String> {
    for p in t.pending.iter_mut() {
        p.stale = true;
    }
    flush(t)?;
    let build = build_async(t, m)?;
    if let Some(build) = build {
        let gpu = finish_build(&t.device, &t.queue, build)?;
        install_gpu(t, gpu);
    }
    t.latest = m.clone();
    Ok(())
}

#[derive(Clone, Copy)]
enum SlotOf {
    Lights,
    World,
    Cdf,
}

/// Waits for the in-flight frame (it may read the buffer being replaced), swaps the buffer for one of
/// `bytes`, and restarts accumulation.
fn swap_buffer(t: &mut PathTracer, label: &str, bytes: &[u8], which: SlotOf) -> Result<(), String> {
    for p in t.pending.iter_mut() {
        p.stale = true;
    }
    let next = shared(&t.device, label, bytes)?;
    let current = match which {
        SlotOf::Lights => std::mem::replace(&mut t.lights, next),
        SlotOf::World => std::mem::replace(&mut t.world_buffer, next),
        SlotOf::Cdf => std::mem::replace(&mut t.cdf_buffer, next),
    };
    retire(t, Box::new(move || {
        let _ = b::destroy_buffer(&current);
    }));
    edited(t);
    Ok(())
}

fn rebuild_lights(t: &mut PathTracer) -> Result<(), String> {
    let world_lights: Vec<rays::world::light> = match &t.world {
        Some(baked) => baked.lights.clone(),
        None => vec![],
    };
    let bytes = light_bytes(&world_lights, &t.scene_lights);
    swap_buffer(t, "pathtracer-lights", &bytes, SlotOf::Lights)?;
    t.light_count = (t.scene_lights.len() + world_lights.len()) as i64;
    Ok(())
}

/// Replaces the scene's rect lights and restarts accumulation. Rect only: approximate a point or spot light
/// with a small rect facing its target.
pub fn set_lights(t: &mut PathTracer, lights: &[Light]) -> Result<(), String> {
    t.scene_lights = lights.to_vec();
    rebuild_lights(t)
}

/// The same baked World by identity: the shared arrays, and the facts carried beside them.
fn same_world(a: &rays::world::baked, b_: &rays::world::baked) -> bool {
    std::sync::Arc::ptr_eq(&a.camera.pixels, &b_.camera.pixels)
        && std::sync::Arc::ptr_eq(&a.lighting.pixels, &b_.lighting.pixels)
        && std::sync::Arc::ptr_eq(&a.cdf.marginal, &b_.cdf.marginal)
        && std::sync::Arc::ptr_eq(&a.cdf.conditional, &b_.cdf.conditional)
        && a.background == b_.background
        && a.sun == b_.sun
        && a.exposure == b_.exposure
        && a.lights == b_.lights
}

/// Light and frame the scene with a baked World instead of the procedural `environment` (whose sky, ground
/// and panels are then ignored); `None` restores it. Uploads the maps and CDF only when the value changes,
/// then restarts accumulation. The effective exposure is `2 ** baked.exposure` times `create`'s `exposure`.
pub fn set_world(t: &mut PathTracer, baked: Option<&rays::world::baked>) -> Result<(), String> {
    match (&t.world, baked) {
        (Some(current), Some(next)) if same_world(current, next) => Ok(()),
        (None, None) => Ok(()),
        _ => {
            let (maps, cdf) = match baked {
                None => (vec![0u8; 16], vec![0u8; 16]),
                Some(baked) => world_bytes(baked)?,
            };
            swap_buffer(t, "pathtracer-world", &maps, SlotOf::World)?;
            swap_buffer(t, "pathtracer-cdf", &cdf, SlotOf::Cdf)?;
            t.world = baked.cloned();
            rebuild_lights(t)
        }
    }
}

/// Starts a replacement build without waiting for the GPU. `render` continues showing the old scene until
/// the new structure is complete. A newer queued mesh supersedes one still building; at most one build and
/// one queued mesh are retained. `flush` waits for the latest replacement. The queue holds three command
/// buffers: a frame in flight may use all of them, so a build or move arriving then waits for the next
/// `render`.
pub fn queue_mesh(t: &mut PathTracer, m: &Mesh) -> Result<(), String> {
    t.queued_mesh = Some(Queued::Full(m.clone()));
    t.latest = m.clone();
    if t.building.is_none() && t.pending.is_empty() { start_queued(t) } else { Ok(()) }
}

/// Re-places the objects of the newest mesh given to the tracer, which must be a `scene_mesh` of as many
/// objects as matrices, at new world transforms (in input order), without re-preparing or re-uploading
/// geometry: only the instance records and the instance structure are rebuilt, over the existing primitive
/// structures. Like `queue_mesh` it does not wait: the moved scene shows once its build completes, then
/// accumulation restarts; a newer `move` supersedes a pending one.
pub fn r#move(t: &mut PathTracer, matrices: &[rays_math::mat4::T]) -> Result<(), String> {
    let moved = moved_mesh(&t.latest, matrices)?;
    if t.building.is_some() || !t.pending.is_empty() {
        t.queued_mesh = Some(match &t.queued_mesh {
            Some(Queued::Full(_)) => Queued::Full(moved.clone()),
            _ => Queued::Move(moved.clone()),
        });
    } else {
        let build = move_async(t, &moved)?;
        t.building = Some(build);
    }
    t.latest = moved;
    Ok(())
}

pub fn destroy(t: &mut PathTracer) {
    let trace = |step: &str| {
        if std::env::var_os("RAYS_PT_HANDLES").is_some() {
            eprintln!("pt destroy {step}: {} live handles", ogpu_core::impl_::live_handles().unwrap_or(-1));
        }
    };
    trace("start");
    for p in t.pending.iter_mut() {
        p.stale = true;
    }
    let _ = flush(t);
    trace("flushed");
    let _ = rays::image::destroy(&t.image);
    trace("image");
    for (_, k) in &t.kernels {
        lisp_kernel::close(k);
    }
    trace("kernels");
    if let Some(g) = &t.gather {
        lisp_kernel::close_gather(g);
    }
    flow_gpu::pipelines::close(&t.flow_pipelines);
    trace("pipelines");
    flow_gpu::image_sink::close(&t.convert);
    trace("sink");
    destroy_gpu(&t.gpu);
    trace("geometry");
    for value in [&t.panels, &t.lights, &t.world_buffer, &t.cdf_buffer, &t.cell] {
        if let Err(e) = b::destroy_buffer(value)
            && std::env::var_os("RAYS_PT_HANDLES").is_some() {
                eprintln!("pt destroy buffer: {}", ogpu_core::error::to_string(&e));
            }
    }
    if let Some(f) = &t.film {
        destroy_film(None, f);
    }
    if let Some(shown) = &t.shown {
        let _ = b::destroy_texture(shown);
    }
    trace("film");
    renderer::renderer::release_gpu(&t.lease);
    trace("lease");
}

/// Builds the primitive acceleration structure, compiles the kernels, and allocates accumulation and output
/// storage. Fails with a typed message when no ray-tracing device is available.
pub fn create(o: &CreateOptions, width: i64, height: i64, scene_: &Scene) -> Result<PathTracer, String> {
    let mut tracer = create_tracer(o, width, height, scene_)?;
    if tracer.latest.device.is_none() {
        Ok(tracer)
    } else {
        let latest = tracer.latest.clone();
        match replace_mesh(&mut tracer, &latest) {
            Ok(()) => Ok(tracer),
            Err(e) => {
                destroy(&mut tracer);
                Err(e)
            }
        }
    }
}

pub mod private {
    use super::*;
    use flow_syntax::value::Value;

    pub use crate::lisp_kernel::CACHE_DIR as kernel_cache;
    pub use crate::lisp_kernel::forget_memory_cache;

    /// What the tracer's device has allocated, for its tests.
    pub fn device_bytes(t: &PathTracer) -> i64 {
        b::allocated_size(&t.device)
    }

    /// A moving instance blurs over the shutter; otherwise it is built at the shutter midpoint.
    pub fn motion_blurs(t: &PathTracer) -> bool {
        b::capabilities(&t.device).ray_tracing_motion
    }

    /// The scene's bottom-level structures, built and then compacted, in bytes.
    pub fn scene_compaction(t: &PathTracer) -> (i64, i64) {
        (t.gpu.scene_unpacked, t.gpu.scene_blas.iter().fold(0, |total, blas| total + b::accel_sizes(blas).structure_size))
    }

    /// The CPU walker's accumulation frame of the current scene (the device kernel's oracle): `accum` plus
    /// this frame's samples, a vec4 per pixel in row order. Scene traces go to the `Flow.Ray_host` of scene 1.
    pub fn cpu_trace(t: &mut PathTracer, camera: &Camera, accum: &[f64]) -> Result<Vec<f64>, String> {
        let fov = camera_fov(camera)?;
        restore_film(t)?;
        let facts = lisp_facts(t);
        let kernels = lisp_kernels(t, &facts)?;
        let n = t.width * t.height;
        let values = lisp_words(t, camera, fov, 0, n);
        let types: Vec<(&str, flow_syntax::ty::Ty)> = lisp_kernel::tables();
        let film_accum = live_film(t).accum.clone();
        let value = |name: &str| -> Value {
            if name == "accum" {
                return Value::Floats(4, std::sync::Arc::new(accum.to_vec()));
            }
            // the history and order tables are the preview's and the resolve's (device-local on a discrete GPU, and
            // never read by the trace kernel)
            if matches!(name, "geo" | "pgeo" | "pcol") {
                return Value::Floats(4, std::sync::Arc::new(vec![0.; 4]));
            }
            if name == "order" {
                return Value::Int_array(std::sync::Arc::new(vec![0]));
            }
            let ty = &types.iter().find(|(k, _)| *k == name).expect("table").1;
            let (buffer, count) = lisp_table(t, &t.gpu.tables, &film_accum, &film_accum, &film_accum, name);
            let length = (count * lisp_kernel::width(ty)) as usize;
            let bytes = b::read_buffer(&buffer, 0, 4 * length as i64).expect("table read");
            let a: Vec<f64> = (0..length).map(|i| f32::from_le_bytes(bytes[4 * i..4 * i + 4].try_into().unwrap()) as f64).collect();
            match ty {
                flow_syntax::ty::Ty::Array(inner) if **inner == flow_syntax::ty::Ty::Vec4 => Value::Floats(4, std::sync::Arc::new(a)),
                flow_syntax::ty::Ty::Array(inner) if **inner == flow_syntax::ty::Ty::Vec3 => Value::Floats(3, std::sync::Arc::new(a)),
                flow_syntax::ty::Ty::Array(inner) if **inner == flow_syntax::ty::Ty::Int => Value::Int_array(std::sync::Arc::new(a.iter().map(|v| *v as i64).collect())),
                _ => Value::Floats(1, std::sync::Arc::new(a)),
            }
        };
        let words: Vec<(String, Vec<f64>)> = values.iter().map(|(n, w)| (n.to_string(), w.clone())).collect();
        let tables_: Vec<(Symbol, Value)> = types.iter().map(|(n, _)| (flow_syntax::symbol::intern(n), value(n))).collect();
        let pixels = Value::Int_array(std::sync::Arc::new((0..n).collect()));
        match lisp_kernel::cpu(&kernels, words, tables_, pixels, "trace")? {
            Value::Floats(4, v) => Ok((*v).clone()),
            _ => Err("path tracer CPU film is not vec4".to_string()),
        }
    }

    /// A Lisp kernel of the current scene shape ("trace", "preview", ...): its pipeline's thread limit and
    /// its MSL.
    pub fn lisp_kernel(t: &mut PathTracer, name: &str) -> Result<(i64, String), String> {
        let facts = lisp_facts(t);
        let kernels = lisp_kernels(t, &facts)?;
        let threads = lisp_kernel::max_total_threads(&kernels, &t.lease, &t.flow_pipelines, name)?;
        let source = lisp_kernel::source(&kernels, &t.lease, &t.flow_pipelines, name)?;
        Ok((threads, source))
    }

    /// Times the film restarted: each mesh installed is one.
    pub fn edits(t: &PathTracer) -> i64 {
        t.history_epoch
    }

    /// Device mesh updates that refit the installed structure in place (same topology).
    pub fn refits(t: &PathTracer) -> i64 {
        t.refits
    }

    /// Checks pathtrace.rays and its helpers whole; the tracer types them only where its kernels call them.
    pub fn check_library() -> Result<(), String> {
        crate::lisp_kernel::check_library()
    }

    /// Checks the accumulation kernel set of sketches/pathtracer.rays's scene shape (tools/bench_check).
    pub fn check_sketch() -> Result<(), String> {
        crate::lisp_kernel::check_sketch()
    }
}
