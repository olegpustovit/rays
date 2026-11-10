//! Native lowering of `Scene3` views: packed meshes, Blinn-Phong and World uniforms, instance
//! batches and the prepared-layer caches the renderer draws from.
use rays_math::float::compare as float_compare;
use crate::camera;
use crate::idlist::{self, IdList, Item};
use crate::light;
use crate::material;
use crate::mesh;
use crate::num::{fmax, fmin, int_of_float};
use crate::scene3;
use crate::world;
use ogpu_core::pipeline::Blend;
use ogpu_core::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu_core::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
use rays_math::color;
use rays_math::mat4;
use rays_math::vec3;
use renderer::scene_execution as se;
use se::{AuxiliaryResource, Bytes, Draw, GeometryDraw, new_bytes, PipelineFamily, SampledDraw, SampledTexture, TextureLevel};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum Error {
    UnsupportedTexture,
    TooManyLights,
    InvalidMesh,
    InvalidViewport,
    Resource(ogpu_core::error::Error),
}

pub struct Resources {
    pub texture: Box<dyn Fn(&scene3::Texture) -> Result<SampledTexture, Error>>,
}

pub(crate) type Prepared = Rc<se::PreparedScene3>;

type ViewportT = (i64, i64, i64, i64);

fn comparison(c: scene3::Comparison) -> Comparison {
    use scene3::Comparison as s;
    match c {
        s::Never => Comparison::Never,
        s::Less => Comparison::Less,
        s::Equal => Comparison::Equal,
        s::LessEqual => Comparison::LessEqual,
        s::Greater => Comparison::Greater,
        s::NotEqual => Comparison::NotEqual,
        s::GreaterEqual => Comparison::GreaterEqual,
        s::Always => Comparison::Always,
    }
}

fn blend(b: scene3::Blend) -> Blend {
    match b {
        scene3::Blend::Replace => Blend::Replace,
        scene3::Blend::Alpha => Blend::Alpha,
        scene3::Blend::Add => Blend::Add,
        scene3::Blend::Multiply => Blend::Multiply,
        scene3::Blend::Screen => Blend::Screen,
        scene3::Blend::Subtract => Blend::Subtract,
    }
}

fn cull(c: scene3::Cull) -> Cull {
    match c {
        scene3::Cull::CullNone => Cull::CullNone,
        scene3::Cull::CullBack => Cull::CullBack,
        scene3::Cull::CullFront => Cull::CullFront,
    }
}

fn packed_color(c: color::T) -> i32 {
    ((c.r << 24) | (c.g << 16) | (c.b << 8) | c.a) as u32 as i32
}

fn put32(b: &mut [u8], index: usize, value: f64) {
    b[index * 4..index * 4 + 4].copy_from_slice(&(value as f32).to_bits().to_le_bytes());
}

fn get32(b: &[u8], index: usize) -> f64 {
    f32::from_bits(u32::from_le_bytes([b[index * 4], b[index * 4 + 1], b[index * 4 + 2], b[index * 4 + 3]])) as f64
}

fn put_color(b: &mut [u8], index: usize, c: color::T) {
    put32(b, index, c.r as f64 / 255.);
    put32(b, index + 1, c.g as f64 / 255.);
    put32(b, index + 2, c.b as f64 / 255.);
    put32(b, index + 3, c.a as f64 / 255.);
}

fn put_matrix(b: &mut [u8], offset: usize, value: &mat4::T) {
    for index in 0..16 {
        put32(b, offset + index, mat4::get(value, (index / 4) as i64, (index % 4) as i64).unwrap_or(0.));
    }
}

/// Public camera clip depth is [-1,1]; native raster/shadow depth is [0,1].
fn native_clip_projection(projection: &mat4::T) -> mat4::T {
    mat4::mul(
        &mat4::of_rows((1., 0., 0., 0.), (0., 1., 0., 0.), (0., 0., 0.5, 0.5), (0., 0., 0., 1.)),
        projection,
    )
}

fn view_projection(viewport: ViewportT, cam: &camera::Camera) -> Result<mat4::T, Error> {
    camera::view_projection_matrix(viewport, cam).map_err(|_| Error::InvalidViewport)
}

// ---- World lighting (Scene3.with_world, specification/environment.md) ----

/// The one Blinn-Phong to PBR mapping: albedo = diffuse (a `color::T` is sRGB, so the shader
/// decodes it with gamma 2.2, the inverse of the tone map), roughness = sqrt (2 / (shininess +
/// 2)) (Phong exponent vs GGX alpha^2), metallic 0, f0 = 0.04. A material with black diffuse,
/// ambient and specular is unlit: its emissive color is written as is, never tone mapped. Ambient
/// and the scene's ambient color are replaced by the World.
fn pbr_of_material(m: &material::Material) -> (bool, f64, f64) {
    let black = |c: color::T| c.r == 0 && c.g == 0 && c.b == 0;
    (black(m.diffuse) && black(m.ambient) && black(m.specular), fmin(1., (2. / (m.shininess + 2.)).sqrt()), 0.04)
}

struct World {
    baked: world::Baked,
    auxiliary: Rc<AuxiliaryResource>,
    camera_map: Rc<SampledTexture>,
    lights: Vec<world::Light>,
}

struct WorldMaps {
    // keeps the bake (and so the identity its key names) alive
    _baked: world::Baked,
    specular: SampledTexture,
    camera: SampledTexture,
}

const WORLD_CAPACITY: i64 = 2;

struct Caster {
    caster_mesh: mesh::Mesh,
    caster_transform: mat4::T,
    caster_instances: Option<Rc<Vec<mat4::T>>>,
}

struct PackedMesh {
    mesh: mesh::Mesh,
    source: mesh::view::PackedView,
    mode: scene3::RenderMode,
    primitive: Primitive,
    key: se::MeshKey,
    vertices: Bytes,
    indices: Bytes,
    vertex_attributes: (String, Bytes),
    vertex_count: i64,
    index_count: i64,
    /// no vertex color is translucent
    opaque: bool,
    /// Whether the borrowed device stream carries normals.
    gpu_normals: Option<bool>,
}

struct PreparedCacheEntry {
    scene: scene3::Scene3,
    prepared: Prepared,
}

#[derive(Clone)]
struct ContextData {
    world_maps: Rc<RefCell<IdList>>,
    worlds: Rc<RefCell<IdList>>,
    sun_fits: Rc<RefCell<IdList>>,
    instance_frames: Rc<RefCell<IdList>>,
    packed_meshes: Rc<RefCell<IdList>>,
    prepared_cache: Rc<RefCell<IdList>>,
}

#[derive(Clone)]
pub struct CacheContext(ContextData);

thread_local! {
    static CURRENT: RefCell<Option<CacheContext>> = const { RefCell::new(None) };
    static BACKGROUND: (se::Mesh, (String, Bytes)) = (make_background_mesh(), make_background_attributes());
    static WHITE: Rc<SampledTexture> = Rc::new(make_world_white());
    static DEVICE_INDICES: RefCell<Vec<DeviceIndicesEntry>> = const { RefCell::new(Vec::new()) };
}

struct DeviceIndicesEntry {
    corners: Option<Arc<Vec<u32>>>,
    mode: scene3::RenderMode,
    count: i64,
    primitive: Primitive,
    indices: Bytes,
    index_count: i64,
    key: Rc<String>,
}

static NEXT_RESOURCE_ID: AtomicI64 = AtomicI64::new(1);

/// Resource labels survive outside this cache context, so their identity allocator must remain
/// process-unique even when two runs pack the same scene.
fn fresh_resource_id() -> i64 {
    NEXT_RESOURCE_ID.fetch_add(1, Ordering::SeqCst)
}

fn current() -> Option<CacheContext> {
    CURRENT.with(|c| c.borrow().clone())
}

const PREPARED_CACHE_CAPACITY: i64 = 16;
const PACKED_MESH_CAPACITY: i64 = 16;
const RETAINED_PAYLOAD_BYTE_CAPACITY: i64 = 256 * 1024 * 1024;

pub(crate) fn create_cache_context() -> CacheContext {
    CacheContext(ContextData {
        // physically keyed: the bake's specular chain and camera map (a stable address each)
        world_maps: Rc::new(RefCell::new(idlist::create(lru::cache("rays.scene3_world_maps"), WORLD_CAPACITY, Box::new(|a, b| a.downcast_ref::<(usize, usize)>().is_some_and(|x| b.downcast_ref::<(usize, usize)>() == Some(x))), None))),
        worlds: Rc::new(RefCell::new(idlist::create(lru::cache("rays.scene3_worlds"), WORLD_CAPACITY, Box::new(|a, b| a.downcast_ref::<(WorldId, Option<String>)>().is_some_and(|x| b.downcast_ref::<(WorldId, Option<String>)>() == Some(x))), None))),
        sun_fits: Rc::new(RefCell::new(idlist::create(
            lru::cache("rays.scene3_sun"),
            WORLD_CAPACITY,
            Box::new(|a, b| match (a.downcast_ref::<(CasterListKey, vec3::T)>(), b.downcast_ref::<(CasterListKey, vec3::T)>()) {
                (Some(x), Some(y)) => x.1 == y.1 && x.0.0.len() == y.0.0.len() && x.0.0.iter().zip(y.0.0.iter()).all(|(p, q)| same_caster(p, q)),
                _ => false,
            }),
            None,
        ))),
        instance_frames: Rc::new(RefCell::new(idlist::create(
            lru::cache("rays.scene3_instances"),
            8,
            Box::new(|a, b| match (a.downcast_ref::<(mat4::T, Rc<Vec<mat4::T>>)>(), b.downcast_ref::<(mat4::T, Rc<Vec<mat4::T>>)>()) {
                (Some(x), Some(y)) => x.0 == y.0 && Rc::ptr_eq(&x.1, &y.1),
                _ => false,
            }),
            None,
        ))),
        packed_meshes: Rc::new(RefCell::new(idlist::create(
            lru::cache("rays.scene3_packed"),
            PACKED_MESH_CAPACITY,
            Box::new(|a, b| match (a.downcast_ref::<(mesh::Mesh, scene3::RenderMode)>(), b.downcast_ref::<(mesh::Mesh, scene3::RenderMode)>()) {
                (Some(x), Some(y)) => mesh::same(&x.0, &y.0) && x.1 == y.1,
                _ => false,
            }),
            None,
        ))),
        prepared_cache: Rc::new(RefCell::new(idlist::create(
            lru::cache("rays.scene3_prepared"),
            PREPARED_CACHE_CAPACITY,
            Box::new(|a, b| match (a.downcast_ref::<(scene3::Scene3, camera::Camera, ViewportT)>(), b.downcast_ref::<(scene3::Scene3, camera::Camera, ViewportT)>()) {
                (Some(x), Some(y)) => scene3::same(&x.0, &y.0) && x.1 == y.1 && x.2 == y.2,
                _ => false,
            }),
            None,
        ))),
    })
}

struct Restore(Option<CacheContext>);

impl Drop for Restore {
    fn drop(&mut self) {
        let previous = self.0.take();
        CURRENT.with(|c| *c.borrow_mut() = previous);
    }
}

pub(crate) fn with_cache_context(context: &CacheContext, f: &mut dyn FnMut()) {
    let previous = CURRENT.with(|c| c.borrow_mut().replace(context.clone()));
    let _restore = Restore(previous);
    f()
}

#[derive(Clone, PartialEq)]
struct WorldId {
    camera_pixels: usize,
    lighting_pixels: usize,
    exposure: u64,
    background: String,
}

fn world_identity(baked: &world::Baked) -> WorldId {
    WorldId {
        camera_pixels: Arc::as_ptr(&baked.camera.pixels) as usize,
        lighting_pixels: Arc::as_ptr(&baked.lighting.pixels) as usize,
        exposure: baked.exposure.to_bits(),
        background: format!("{:?}", baked.background),
    }
}

struct CasterListKey(Vec<Caster>);

fn half_level(m: &world::Map) -> TextureLevel {
    let mut out = vec![0u8; (m.width * m.height * 8) as usize];
    let one = ogpu_core::types::half_of_float(1.) as u16;
    for i in 0..(m.width * m.height) as usize {
        for c in 0..3 {
            let h = ogpu_core::types::half_of_float(fmin(65504., m.pixels[i * 3 + c])) as u16;
            out[i * 8 + c * 2..i * 8 + c * 2 + 2].copy_from_slice(&h.to_le_bytes());
        }
        out[i * 8 + 6..i * 8 + 8].copy_from_slice(&one.to_le_bytes());
    }
    TextureLevel { width: m.width, height: m.height, bytes: new_bytes(out) }
}

fn world_sampler(mips: i64) -> SamplerDescriptor {
    SamplerDescriptor {
        label: Some("scene3-world".to_string()),
        min_filter: SamplerFilter::Linear,
        mag_filter: SamplerFilter::Linear,
        mip_filter: if mips > 1 { MipFilter::LinearMip } else { MipFilter::NoMip },
        address_u: AddressMode::Repeat,
        address_v: AddressMode::ClampToEdge,
        lod_min: 0.,
        lod_max: (mips - 1) as f64,
        max_anisotropy: 1,
    }
}

fn make_world_white() -> SampledTexture {
    SampledTexture {
        key: "world:white".to_string(),
        levels: Rc::new(vec![TextureLevel { width: 1, height: 1, bytes: new_bytes(vec![255, 255, 255, 255]) }]),
        sampler: SamplerDescriptor {
            label: Some("scene3-world-white".to_string()),
            min_filter: SamplerFilter::Nearest,
            mag_filter: SamplerFilter::Nearest,
            mip_filter: MipFilter::NoMip,
            address_u: AddressMode::ClampToEdge,
            address_v: AddressMode::ClampToEdge,
            lod_min: 0.,
            lod_max: 0.,
            max_anisotropy: 1,
        },
        gpu: None,
    }
}

fn maps_of(baked: &world::Baked) -> Rc<WorldMaps> {
    let specular = baked.specular();
    let cache_key: Item = Rc::new((specular as *const Vec<world::Map> as usize, Arc::as_ptr(&baked.camera.pixels) as usize));
    let cache = current();
    if let Some(context) = &cache
        && let Some(found) = idlist::peek(&mut context.0.world_maps.borrow_mut(), &cache_key)
            && let Ok(maps) = found.downcast::<WorldMaps>() {
                return maps;
            }
    let key = format!("world:{}:", fresh_resource_id());
    let maps = Rc::new(WorldMaps {
        _baked: baked.clone(),
        specular: SampledTexture {
            key: format!("{key}specular"),
            levels: Rc::new(specular.iter().map(half_level).collect()),
            sampler: world_sampler(specular.len() as i64),
            gpu: None,
        },
        camera: SampledTexture {
            key: format!("{key}camera"),
            levels: Rc::new(vec![half_level(&baked.camera)]),
            sampler: world_sampler(1),
            gpu: None,
        },
    });
    if let Some(context) = &cache {
        idlist::add(&mut context.0.world_maps.borrow_mut(), 0, cache_key, maps.clone());
    }
    maps
}

fn casters(scene: &scene3::Scene3) -> Vec<Caster> {
    let mut out = Vec::new();
    scene3::walk::iter_batches(
        &mut |d, transforms| {
            let triangles = matches!(mesh::mode(&d.mesh), mesh::Mode::Triangles | mesh::Mode::TriangleStrip | mesh::Mode::TriangleFan);
            if triangles
                && d.mode == scene3::RenderMode::Faces
                && (d.blend == scene3::Blend::Replace || d.blend == scene3::Blend::Alpha)
                && d.depth.write
            {
                out.push(Caster { caster_mesh: d.mesh.clone(), caster_transform: d.transform.clone(), caster_instances: transforms.cloned() });
            }
        },
        scene,
    );
    out
}

fn same_caster(a: &Caster, b: &Caster) -> bool {
    mesh::same(&a.caster_mesh, &b.caster_mesh)
        && a.caster_transform == b.caster_transform
        && match (&a.caster_instances, &b.caster_instances) {
            (None, None) => true,
            (Some(x), Some(y)) => Rc::ptr_eq(x, y) || x == y,
            _ => false,
        }
}

/// The World sun's shadow map: an orthographic view along the sun fitted to the bounds of the
/// depth-writing Replace or Alpha triangle drawings (the renderer's casters), keyed by their
/// meshes, transforms and instances plus the sun direction, so a scene rebuilt each frame from
/// the same parts re-renders nothing.
fn sun_fit(scene: &scene3::Scene3, sun: &world::Sun) -> Result<Option<(mat4::T, String)>, Error> {
    let list = casters(scene);
    let cache = current();
    let key: Item = Rc::new((CasterListKey(list), sun.direction));
    if let Some(context) = &cache
        && let Some(found) = idlist::peek(&mut context.0.sun_fits.borrow_mut(), &key)
            && let Ok(fit) = found.downcast::<Option<(mat4::T, String)>>() {
                return Ok((*fit).clone());
            }
    let list = &key.downcast_ref::<(CasterListKey, vec3::T)>().expect("sun key").0.0;
    let f = vec3::normalize(vec3::neg(sun.direction));
    let side = vec3::normalize(vec3::cross(f, if f.y.abs() < 0.99 { vec3::unit_y() } else { vec3::unit_x() }));
    let up = vec3::cross(side, f);
    let mut lo = [f64::INFINITY; 3];
    let mut hi = [f64::NEG_INFINITY; 3];
    let mut add = |p: vec3::T| {
        for (axis, v) in [side, up, f].iter().enumerate() {
            let d = vec3::dot(*v, p);
            lo[axis] = fmin(lo[axis], d);
            hi[axis] = fmax(hi[axis], d);
        }
    };
    for c in list {
        // a device mesh's box is reduced where its positions are, its host copy never made
        if let Some((a, b)) = mesh::bounds(&c.caster_mesh).map_err(|_| Error::InvalidMesh)? {
            let mut corners = |m: &mat4::T| {
                for k in 0..8 {
                    add(mat4::transform_point(
                        m,
                        vec3::create(if k & 1 == 0 { a.x } else { b.x }, if k & 2 == 0 { a.y } else { b.y }, if k & 4 == 0 { a.z } else { b.z }),
                    ));
                }
            };
            match &c.caster_instances {
                None => corners(&c.caster_transform),
                Some(transforms) => {
                    for t in transforms.iter() {
                        corners(&mat4::mul(&c.caster_transform, t));
                    }
                }
            }
        }
    }
    let fit = if !lo[0].is_finite() {
        None
    } else {
        // A 2% margin keeps edge texels and the depth extremes inside.
        let range = |axis: usize| fmax(1e-3, (hi[axis] - lo[axis]) * 1.02);
        let middle = |axis: usize| (hi[axis] + lo[axis]) * 0.5;
        let row = |v: vec3::T, scale: f64, offset: f64| (v.x * scale, v.y * scale, v.z * scale, offset);
        let sx = 2. / range(0);
        let sy = 2. / range(1);
        let sz = 1. / range(2);
        let id = fresh_resource_id();
        Some((
            mat4::of_rows(row(side, sx, -middle(0) * sx), row(up, sy, -middle(1) * sy), row(f, sz, 0.5 - (middle(2) * sz)), (0., 0., 0., 1.)),
            format!("sun:{id}"),
        ))
    };
    if let Some(context) = &cache {
        idlist::add(&mut context.0.sun_fits.borrow_mut(), 0, key.clone(), Rc::new(fit.clone()));
    }
    Ok(fit)
}

/// Word layout: see `source_scene3_world` in the runtime.
fn world_of(baked: &world::Baked, sun: Option<(mat4::T, String)>) -> Rc<World> {
    let key: Item = Rc::new((world_identity(baked), sun.as_ref().map(|s| s.1.clone())));
    let cache = current();
    if let Some(context) = &cache
        && let Some(found) = idlist::peek(&mut context.0.worlds.borrow_mut(), &key)
            && let Ok(world) = found.downcast::<World>() {
                return world;
            }
    let maps = maps_of(baked);
    let mut b = vec![0u8; 64 * 4];
    let rgb = |b: &mut Vec<u8>, index: usize, c: &world::Rgb, k: f64| {
        put32(b, index, c.r * k);
        put32(b, index + 1, c.g * k);
        put32(b, index + 2, c.b * k);
    };
    for (k, c) in baked.sh9().iter().enumerate() {
        rgb(&mut b, k * 3, c, 1.);
    }
    put32(&mut b, 27, 2.0_f64.powf(baked.exposure));
    if let Some(s) = &baked.sun {
        put32(&mut b, 28, s.direction.x);
        put32(&mut b, 29, s.direction.y);
        put32(&mut b, 30, s.direction.z);
        put32(&mut b, 31, 1.);
        // Irradiance of a small disc: radiance x solid angle ~ pi r^2.
        rgb(&mut b, 32, &s.radiance, std::f64::consts::PI * s.angular_radius * s.angular_radius);
    }
    if let world::Background::Color(c) = &baked.background {
        put32(&mut b, 35, 1.);
        rgb(&mut b, 36, c, 1.);
    }
    // 2: the renderer's sun map.
    if let Some((view_projection, _)) = &sun {
        put32(&mut b, 39, 2.);
        put_matrix(&mut b, 40, view_projection);
        put32(&mut b, 56, 0.001);
        put32(&mut b, 57, 0.005);
        put32(&mut b, 58, 1.);
        put32(&mut b, 59, 1.);
        put32(&mut b, 60, se::SUN_MAP_SIZE as f64);
        put32(&mut b, 61, se::SUN_MAP_SIZE as f64);
    }
    let power = |l: &world::Light| {
        ((0.2126 * l.radiance.r) + (0.7152 * l.radiance.g) + (0.0722 * l.radiance.b)) * vec3::length(l.u) * vec3::length(l.v)
    };
    let id = fresh_resource_id();
    let mut lights = baked.lights.clone();
    lights.sort_by(|a, b| float_compare(power(b), power(a)));
    let camera_map = Rc::new(maps.camera.clone());
    let w = Rc::new(World {
        baked: baked.clone(),
        auxiliary: Rc::new(AuxiliaryResource {
            key: format!("world:{id}"),
            buffer: new_bytes(b),
            texture: maps.specular.clone(),
            environment: Some(maps.camera.clone()),
            sun_shadow: sun.as_ref().map(|s| s.1.clone()),
        }),
        camera_map,
        lights,
    });
    if let Some(context) = &cache {
        idlist::add(&mut context.0.worlds.borrow_mut(), 0, key, w.clone());
    }
    w
}

/// Uniform words 84..1363 hold 20 words per light, matching the shader loop.
const MAX_LIGHTS: usize = 64;

/// World words 76..78 and the extracted rects after the scene's own lights. ponytail: past 64
/// lights the lowest-power World rects are dropped (the scene's own lights always stay); a
/// clustered light list lifts the cap.
fn world_uniforms(b: &mut Vec<u8>, w: &World, material: &material::Material) {
    let (unlit, roughness, f0) = pbr_of_material(material);
    put32(b, 76, if unlit { 1. } else { 0. });
    put32(b, 77, roughness);
    put32(b, 78, f0);
    let first = int_of_float(get32(b, 73)) as usize;
    let count = std::cmp::min(w.lights.len(), MAX_LIGHTS - first);
    for index in 0..count {
        let l = &w.lights[index];
        let o = 84 + ((first + index) * 20);
        let vector = |b: &mut Vec<u8>, at: usize, v: vec3::T| {
            put32(b, at, v.x);
            put32(b, at + 1, v.y);
            put32(b, at + 2, v.z);
        };
        // The shader scales colors by pi (a `light::Light` intensity 1 lights a white matte face to
        // 1); a rect carries radiance x area / pi.
        let k = 4. * vec3::length(l.u) * vec3::length(l.v) / std::f64::consts::PI;
        put32(b, o, 4.);
        vector(b, o + 1, l.position);
        put32(b, o + 4, l.radiance.r * k);
        put32(b, o + 5, l.radiance.g * k);
        put32(b, o + 6, l.radiance.b * k);
        put32(b, o + 8, 1.);
        vector(b, o + 9, l.u);
        vector(b, o + 12, l.v);
        vector(b, o + 15, l.normal);
    }
    put32(b, 73, (first + count) as f64);
}

/// A clip-space triangle covering the viewport; the shader turns each fragment into a view ray
/// through the inverse view-projection at 84.
fn make_background_mesh() -> se::Mesh {
    let mut vertices = vec![0u8; 3 * 24];
    for (index, (x, y)) in [(-1., -1.), (3., -1.), (-1., 3.)].iter().enumerate() {
        let put = |v: &mut Vec<u8>, at: usize, value: f64| {
            v[index * 24 + at..index * 24 + at + 4].copy_from_slice(&(value as f32).to_bits().to_le_bytes());
        };
        put(&mut vertices, 0, *x);
        put(&mut vertices, 4, *y);
        put(&mut vertices, 8, 0.5);
        put(&mut vertices, 20, 1.);
    }
    let mut indices = vec![0u8; 12];
    indices[4..8].copy_from_slice(&1i32.to_le_bytes());
    indices[8..12].copy_from_slice(&2i32.to_le_bytes());
    se::Mesh {
        key: se::MeshKey::Host("world:background".to_string()),
        vertices: new_bytes(vertices),
        shared_vertices: None,
        vertex_count: 3,
        indices: new_bytes(indices),
        index_count: 3,
        primitive: Primitive::TriangleList,
    }
}

fn make_background_attributes() -> (String, Bytes) {
    let mut b = vec![0u8; 36];
    for i in 0..3 {
        b[i * 12..i * 12 + 4].copy_from_slice(&(-1i32).to_le_bytes());
    }
    ("world:background:attributes".to_string(), new_bytes(b))
}

fn background_uniforms(cam: &camera::Camera, viewport: ViewportT) -> Result<Option<Vec<u8>>, Error> {
    match mat4::inverse(&native_clip_projection(&view_projection(viewport, cam)?)) {
        None => Ok(None),
        Some(inverse) => {
            let mut b = vec![0u8; 5456];
            put_matrix(&mut b, 0, &mat4::identity());
            put_matrix(&mut b, 16, &mat4::identity());
            put_matrix(&mut b, 32, &mat4::identity());
            put32(&mut b, 76, 2.);
            put32(&mut b, 82, 1.);
            put_matrix(&mut b, 84, &inverse);
            Ok(Some(b))
        }
    }
}

fn uniforms_blinn_phong(cam: &camera::Camera, viewport: ViewportT, scene: &scene3::Scene3, d: &scene3::walk::Drawing) -> Result<Vec<u8>, Error> {
    let mut b = vec![0u8; 5456];
    let material = &d.material;
    put32(&mut b, 82, 1.);
    let projection = native_clip_projection(&view_projection(viewport, cam)?);
    put_matrix(&mut b, 0, &mat4::mul(&projection, &d.transform));
    put_matrix(&mut b, 16, &d.transform);
    let normal = match mat4::inverse(&d.transform) {
        None => mat4::identity(),
        Some(value) => mat4::transpose(&value),
    };
    put_matrix(&mut b, 32, &normal);
    let eye = camera::position(cam);
    put32(&mut b, 48, eye.x);
    put32(&mut b, 49, eye.y);
    put32(&mut b, 50, eye.z);
    put_color(&mut b, 52, material.ambient);
    put_color(&mut b, 56, material.diffuse);
    put_color(&mut b, 60, material.specular);
    put_color(&mut b, 64, material.emissive);
    put32(&mut b, 68, material.shininess);
    put_color(&mut b, 69, scene3::walk::ambient(scene));
    let lights = scene3::walk::lights(scene);
    // The native light ABI reserves linear slots; the zero-filled buffer keeps them zero while
    // preserving the shared twenty-word layout.
    put32(&mut b, 73, lights.len() as f64);
    put32(&mut b, 74, if d.cull == scene3::Cull::CullNone { 1. } else { 0. });
    put32(&mut b, 75, if scene3::walk::separate_specular(scene) { 1. } else { 0. });
    for (index, l) in lights.iter().enumerate() {
        let o = 84 + (index * 20);
        let vector = |b: &mut Vec<u8>, p: vec3::T| {
            put32(b, o + 1, p.x);
            put32(b, o + 2, p.y);
            put32(b, o + 3, p.z);
        };
        put_color(&mut b, o + 4, l.diffuse);
        put32(&mut b, o + 8, l.intensity);
        match &l.kind {
            light::Kind::Directional { direction } => {
                put32(&mut b, o, 0.);
                vector(&mut b, *direction);
            }
            light::Kind::Point { position, attenuation } => {
                put32(&mut b, o, 1.);
                vector(&mut b, *position);
                put32(&mut b, o + 9, attenuation.constant);
                put32(&mut b, o + 11, attenuation.quadratic);
            }
            light::Kind::Spot { position, direction, cutoff, concentration, attenuation } => {
                put32(&mut b, o, 2.);
                vector(&mut b, *position);
                put32(&mut b, o + 9, direction.x);
                put32(&mut b, o + 10, direction.y);
                put32(&mut b, o + 11, direction.z);
                put32(&mut b, o + 12, cutoff.cos());
                put32(&mut b, o + 13, cutoff.cos());
                put32(&mut b, o + 14, *concentration);
                put32(&mut b, o + 15, attenuation.constant);
                put32(&mut b, o + 17, attenuation.quadratic);
            }
            light::Kind::Area { position, direction, width, height, samples, attenuation } => {
                put32(&mut b, o, 3.);
                vector(&mut b, *position);
                put32(&mut b, o + 9, direction.x);
                put32(&mut b, o + 10, direction.y);
                put32(&mut b, o + 11, direction.z);
                put32(&mut b, o + 12, *width);
                put32(&mut b, o + 13, *height);
                put32(&mut b, o + 14, *samples as f64);
                put32(&mut b, o + 15, attenuation.constant);
                put32(&mut b, o + 17, attenuation.quadratic);
            }
        }
    }
    Ok(b)
}

fn uniforms(w: Option<&World>, cam: &camera::Camera, viewport: ViewportT, scene: &scene3::Scene3, d: &scene3::walk::Drawing) -> Result<Vec<u8>, Error> {
    let mut b = uniforms_blinn_phong(cam, viewport, scene, d)?;
    if let Some(w) = w {
        world_uniforms(&mut b, w, &d.material);
    }
    Ok(b)
}

/// Each instance's world and normal matrices depend only on its batch, not the camera: kept for
/// the last few batches (by identity) so an orbit only redoes the projection.
fn instance_frames(parent: &mat4::T, transforms: &Rc<Vec<mat4::T>>) -> Rc<(Vec<mat4::T>, Vec<mat4::T>)> {
    let cache = current();
    let key: Item = Rc::new((parent.clone(), transforms.clone()));
    if let Some(context) = &cache
        && let Some(found) = idlist::peek(&mut context.0.instance_frames.borrow_mut(), &key)
            && let Ok(frames) = found.downcast::<(Vec<mat4::T>, Vec<mat4::T>)>() {
                return frames;
            }
    let worlds: Vec<mat4::T> = transforms.iter().map(|t| mat4::mul(parent, t)).collect();
    let normals: Vec<mat4::T> = worlds
        .iter()
        .map(|w| match mat4::inverse(w) {
            None => mat4::identity(),
            Some(inverse) => mat4::transpose(&inverse),
        })
        .collect();
    let frames = Rc::new((worlds, normals));
    if let Some(context) = &cache {
        idlist::add(&mut context.0.instance_frames.borrow_mut(), 0, key, frames.clone());
    }
    frames
}

fn instance_uniforms(
    w: Option<&World>,
    cam: &camera::Camera,
    viewport: ViewportT,
    scene: &scene3::Scene3,
    d: &scene3::walk::Drawing,
    all: &Rc<Vec<mat4::T>>,
    first: usize,
    count: usize,
) -> Result<Vec<u8>, Error> {
    let parent = d.transform.clone();
    let mut drawing = d.clone();
    drawing.transform = mat4::mul(&d.transform, &all[first]);
    let base = uniforms(w, cam, viewport, scene, &drawing)?;
    if count == 1 {
        return Ok(base);
    }
    let mut b = vec![0u8; 5456 + count * 192];
    b[..5456].copy_from_slice(&base);
    // Word 83 marks the packed matrix table that starts after the shared Scene3 material and
    // light block. Each instance owns three 4x4 matrices.
    put32(&mut b, 83, 1.);
    let projection = native_clip_projection(&view_projection(viewport, cam)?);
    let frames = instance_frames(&parent, all);
    let (worlds, normals) = (&frames.0, &frames.1);
    for i in 0..count {
        let world = &worlds[first + i];
        let offset = 1364 + (i * 48);
        put_matrix(&mut b, offset, &mat4::mul(&projection, world));
        put_matrix(&mut b, offset + 16, world);
        put_matrix(&mut b, offset + 32, &normals[first + i]);
    }
    Ok(b)
}

fn prepared_bytes(p: &se::PreparedScene3) -> i64 {
    p.entries
        .iter()
        .map(|entry| {
            let m = &entry.draw.mesh;
            m.vertex_bytes_len() as i64
                + m.indices.len() as i64
                + entry.vertex_attributes.as_ref().map_or(0, |(_, b)| b.len() as i64)
                + entry.draw.state.transform_uniforms.as_ref().map_or(0, |b| b.len() as i64)
        })
        .sum()
}

/// A layer drawing a device's vertices (a live cook's display) is one frame's: the next cook's
/// display is a new mesh, so an older layer is never asked for again, and cached it holds its
/// scene, the geometry and the device outputs under it. A cache keeps only the newest scene's
/// layers (one per viewport); the frame in flight is its pins', not the cache's. Layers of host
/// meshes are bounded only by the cache.
pub(crate) fn borrows_device(p: &se::PreparedScene3) -> bool {
    p.entries.iter().any(|e| matches!(e.draw.mesh.key, se::MeshKey::GpuVertices { .. }))
}

fn trim_prepared_cache(list: &mut IdList) {
    let mut count = 0;
    let mut bytes_ = 0;
    let mut newest_device: Option<scene3::Scene3> = None;
    idlist::filter(list, &mut |_, value| {
        let Some(cached) = value.downcast_ref::<PreparedCacheEntry>() else {
            return false;
        };
        let size = prepared_bytes(&cached.prepared);
        let device = borrows_device(&cached.prepared);
        let keep = count < PREPARED_CACHE_CAPACITY
            && size <= RETAINED_PAYLOAD_BYTE_CAPACITY - bytes_
            && (!device
                || match &newest_device {
                    None => {
                        newest_device = Some(cached.scene.clone());
                        true
                    }
                    Some(scene) => scene3::same(scene, &cached.scene),
                });
        if keep {
            count += 1;
            bytes_ += size;
        }
        keep
    });
}

fn pack_vertices(allow_missing_normals: bool, view: &mesh::view::PackedView) -> Result<Bytes, Error> {
    let count = view.vertices.x.len();
    let normals = match &view.normals {
        Some(values) if values.x.len() == count => Some(Some(values)),
        None if allow_missing_normals => Some(None),
        _ => None,
    };
    let Some(normals) = normals else {
        return Err(Error::InvalidMesh);
    };
    let mut b = vec![0u8; count * 24];
    let mut finite = true;
    let mut put = |b: &mut Vec<u8>, at: usize, value: f64| {
        let single = value as f32;
        if !single.is_finite() {
            finite = false;
        }
        b[at..at + 4].copy_from_slice(&single.to_bits().to_le_bytes());
    };
    for index in 0..count {
        let offset = index * 24;
        put(&mut b, offset, view.vertices.x[index]);
        put(&mut b, offset + 4, view.vertices.y[index]);
        put(&mut b, offset + 8, view.vertices.z[index]);
        match normals {
            None => put(&mut b, offset + 20, 1.),
            Some(n) => {
                put(&mut b, offset + 12, n.x[index]);
                put(&mut b, offset + 16, n.y[index]);
                put(&mut b, offset + 20, n.z[index]);
            }
        }
    }
    if finite { Ok(new_bytes(b)) } else { Err(Error::InvalidMesh) }
}

fn pack_attributes(count: Option<i64>, view: &mesh::view::PackedView) -> Result<Bytes, Error> {
    let count = std::cmp::max(count.unwrap_or(0), view.vertices.x.len() as i64) as usize;
    let mut b = vec![0u8; count * 12];
    let mut finite = true;
    for index in 0..count {
        let c = match &view.colors {
            Some(colors) => packed_color(colors[index]),
            None => -1,
        };
        b[index * 12..index * 12 + 4].copy_from_slice(&c.to_le_bytes());
        if let Some(values) = &view.tex_coords {
            let uv = values[index];
            for (at, value) in [((index * 12) + 4, uv.x), ((index * 12) + 8, uv.y)] {
                let single = value as f32;
                if !single.is_finite() {
                    finite = false;
                }
                b[at..at + 4].copy_from_slice(&single.to_bits().to_le_bytes());
            }
        }
    }
    if finite { Ok(new_bytes(b)) } else { Err(Error::InvalidMesh) }
}

fn pack_indices(values: &[u32]) -> Bytes {
    let mut b = vec![0u8; values.len() * 4];
    for (i, value) in values.iter().enumerate() {
        b[i * 4..i * 4 + 4].copy_from_slice(&value.to_le_bytes());
    }
    new_bytes(b)
}

fn triangle_indices(mode: mesh::Mode, source: &[u32]) -> Option<Vec<u32>> {
    match mode {
        mesh::Mode::Triangles if source.len().is_multiple_of(3) => Some(source.to_vec()),
        mesh::Mode::TriangleStrip if source.len() >= 3 => Some(
            (0..(source.len() - 2) * 3)
                .map(|index| {
                    let triangle = index / 3;
                    let corner = index % 3;
                    if triangle & 1 == 0 {
                        source[triangle + corner]
                    } else {
                        source[triangle + match corner { 0 => 1, 1 => 0, _ => 2 }]
                    }
                })
                .collect(),
        ),
        mesh::Mode::TriangleFan if source.len() >= 3 => Some(
            (0..(source.len() - 2) * 3)
                .map(|index| {
                    let triangle = index / 3;
                    match index % 3 {
                        0 => source[0],
                        1 => source[triangle + 1],
                        _ => source[triangle + 2],
                    }
                })
                .collect(),
        ),
        _ => None,
    }
}

fn line_indices(mode: mesh::Mode, source: &[u32]) -> Option<Vec<u32>> {
    match mode {
        mesh::Mode::Lines if source.len().is_multiple_of(2) => Some(source.to_vec()),
        mesh::Mode::LineStrip if source.len() >= 2 => Some((0..(source.len() - 1) * 2).map(|i| source[(i / 2) + (i % 2)]).collect()),
        mesh::Mode::LineLoop if source.len() >= 2 => Some((0..source.len() * 2).map(|i| source[((i / 2) + (i % 2)) % source.len()]).collect()),
        _ => None,
    }
}

fn unique_triangle_edges(triangles: &[u32]) -> Vec<u32> {
    let mut seen: HashSet<(u32, u32)> = HashSet::with_capacity(triangles.len());
    let mut edges = Vec::new();
    for i in 0..triangles.len() / 3 {
        let a = triangles[i * 3];
        let b = triangles[i * 3 + 1];
        let c = triangles[i * 3 + 2];
        for (a, b) in [(a, b), (b, c), (c, a)] {
            let key = if a < b { (a, b) } else { (b, a) };
            if seen.insert(key) {
                edges.push(a);
                edges.push(b);
            }
        }
    }
    edges
}

fn same_planes(a: &mesh::Vec3Buffer, b: &mesh::Vec3Buffer) -> bool {
    Arc::ptr_eq(&a.x, &b.x) && Arc::ptr_eq(&a.y, &b.y) && Arc::ptr_eq(&a.z, &b.z)
}

fn same_normals(a: &Option<mesh::Vec3Buffer>, b: &Option<mesh::Vec3Buffer>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => same_planes(x, y),
        _ => false,
    }
}

fn same_geometry(a: &mesh::view::PackedView, b: &mesh::view::PackedView) -> bool {
    a.mode == b.mode && same_planes(&a.vertices, &b.vertices) && same_normals(&a.normals, &b.normals) && Arc::ptr_eq(&a.indices, &b.indices)
}

fn same_colors(a: &Option<Arc<Vec<color::T>>>, b: &Option<Arc<Vec<color::T>>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => Arc::ptr_eq(x, y),
        _ => false,
    }
}

fn same_uvs(a: &Option<Arc<Vec<rays_math::vec2::T>>>, b: &Option<Arc<Vec<rays_math::vec2::T>>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => Arc::ptr_eq(x, y),
        _ => false,
    }
}

fn find_packed(predicate: &dyn Fn(&PackedMesh) -> bool) -> Option<Rc<PackedMesh>> {
    let context = current()?;
    let mut found = None;
    idlist::iter(&mut context.0.packed_meshes.borrow_mut(), &mut |_, value| {
        if let Ok(packed) = value.clone().downcast::<PackedMesh>()
            && predicate(&packed) {
                found = Some(packed);
            }
    });
    found
}

/// A device mesh's draw: its indices follow its topology (a triangle mesh's corners, the same
/// array frame after frame while the topology is; a cloud's count) and its plain attribute
/// record its count, so they are packed once, and the renderer keeps the indices under their own
/// key, independently of the borrowed vertex stream.
fn packed_of_device(mode: scene3::RenderMode, m: &mesh::Mesh, device: (String, bool)) -> Result<Rc<PackedMesh>, Error> {
    let (key, normals) = device;
    let key = Rc::new(key);
    let source = mesh::view::unread(m);
    let count = mesh::vertex_count(m);
    let points = mode == scene3::RenderMode::Vertices || source.mode == mesh::Mode::Points;
    let plain = source.colors.is_none() && source.tex_coords.is_none();
    let native: Option<(Primitive, Bytes, i64, Rc<String>)> = if let Some(index_count) = mesh::view::device_index_count(m) {
        Some((Primitive::TriangleList, new_bytes(Vec::new()), index_count, key.clone()))
    } else {
        let found = DEVICE_INDICES.with(|list| {
            list.borrow().iter().take(8).find_map(|e| {
                let corners_match = if points { e.corners.is_none() } else { e.corners.as_ref().is_some_and(|c| Arc::ptr_eq(c, &source.indices)) };
                if e.mode == mode && e.count == count && corners_match {
                    Some((e.primitive, e.indices.clone(), e.index_count, e.key.clone()))
                } else {
                    None
                }
            })
        });
        match found {
            Some(packed) => Some(packed),
            None => {
                let made: Option<(Primitive, Vec<u32>)> = if points {
                    Some((Primitive::PointList, (0..count as u32).collect()))
                } else {
                    triangle_indices(source.mode, &source.indices).map(|indices| {
                        if mode == scene3::RenderMode::Wireframe {
                            (Primitive::LineList, unique_triangle_edges(&indices))
                        } else {
                            (Primitive::TriangleList, indices)
                        }
                    })
                };
                made.map(|(primitive, indices)| {
                    let packed = (primitive, pack_indices(&indices), indices.len() as i64, Rc::new(format!("scene3:device-indices:{}", fresh_resource_id())));
                    DEVICE_INDICES.with(|list| {
                        let mut list = list.borrow_mut();
                        list.insert(
                            0,
                            DeviceIndicesEntry {
                                corners: if points { None } else { Some(source.indices.clone()) },
                                mode,
                                count,
                                primitive: packed.0,
                                indices: packed.1.clone(),
                                index_count: packed.2,
                                key: packed.3.clone(),
                            },
                        );
                        list.truncate(8);
                    });
                    packed
                })
            }
        }
    };
    let attributes = if plain || mesh::view::device_color_width(m) != 0 { Ok(new_bytes(Vec::new())) } else { pack_attributes(Some(count), &source) };
    match (native, attributes) {
        (Some((primitive, indices, index_count, indices_key)), Ok(attributes)) if count > 0 && index_count > 0 => Ok(Rc::new(PackedMesh {
            mesh: m.clone(),
            source: source.clone(),
            mode,
            primitive,
            key: se::MeshKey::GpuVertices { name: key, indices: if mesh::view::device_index_count(m).is_some() { None } else { Some(indices_key) } },
            vertices: new_bytes(Vec::new()),
            indices,
            vertex_attributes: (
                if plain { format!("scene3:device-attributes:{count}") } else { format!("scene3:attributes:{}", fresh_resource_id()) },
                attributes,
            ),
            vertex_count: count,
            index_count,
            opaque: mesh::view::device_color_width(m) != 4
                && source.colors.as_ref().is_none_or(|colors| colors.iter().all(|c| c.a == 255)),
            gpu_normals: Some(normals),
        })),
        _ => Err(Error::InvalidMesh),
    }
}

fn packed_of_mesh(mode: scene3::RenderMode, m: &mesh::Mesh) -> Result<Rc<PackedMesh>, Error> {
    match mesh::view::device(m) {
        Some(_) if mesh::view::device_index_count(m).is_some() && mode != scene3::RenderMode::Faces => packed_of_host(mode, m),
        Some(device) => packed_of_device(mode, m, device),
        None => packed_of_host(mode, m),
    }
}

fn packed_of_host(mode: scene3::RenderMode, m: &mesh::Mesh) -> Result<Rc<PackedMesh>, Error> {
    let source = mesh::view::packed_view(m).map_err(|_| Error::InvalidMesh)?;
    let cache = current();
    let exact = cache
        .as_ref()
        .and_then(|context| {
            let key: Item = Rc::new((m.clone(), mode));
            idlist::peek(&mut context.0.packed_meshes.borrow_mut(), &key)
        })
        .and_then(|found| found.downcast::<PackedMesh>().ok())
        .filter(|packed| {
            same_geometry(&packed.source, &source)
                && same_colors(&packed.source.colors, &source.colors)
                && same_uvs(&packed.source.tex_coords, &source.tex_coords)
        });
    let matching = match exact {
        Some(_) => exact,
        None => find_packed(&|packed| {
            packed.mode == mode
                && same_geometry(&packed.source, &source)
                && same_colors(&packed.source.colors, &source.colors)
                && same_uvs(&packed.source.tex_coords, &source.tex_coords)
        }),
    };
    if let Some(packed) = matching {
        return Ok(packed);
    }
    let reused = find_packed(&|packed| packed.mode == mode && same_geometry(&packed.source, &source));
    let mut view = source.clone();
    // Lit faces need normals; derive them once here (the pack is cached by mesh identity) rather
    // than rejecting procedural meshes without N.
    if mode == scene3::RenderMode::Faces
        && matches!(view.mode, mesh::Mode::Triangles | mesh::Mode::TriangleStrip | mesh::Mode::TriangleFan)
        && view.normals.is_none()
        && reused.is_none()
    {
        view = mesh::view::packed_view(&mesh::recalculate_normals(m)).map_err(|_| Error::InvalidMesh)?;
    }
    let native: Option<(Primitive, Vec<u32>)> = match mode {
        scene3::RenderMode::Vertices => Some((Primitive::PointList, (0..view.vertices.x.len() as u32).collect())),
        scene3::RenderMode::Faces | scene3::RenderMode::Wireframe => match view.mode {
            mesh::Mode::Points => Some((Primitive::PointList, view.indices.to_vec())),
            mesh::Mode::Lines | mesh::Mode::LineStrip | mesh::Mode::LineLoop => {
                line_indices(view.mode, &view.indices).map(|indices| (Primitive::LineList, indices))
            }
            mesh::Mode::Triangles | mesh::Mode::TriangleStrip | mesh::Mode::TriangleFan => {
                triangle_indices(view.mode, &view.indices).map(|indices| {
                    if mode == scene3::RenderMode::Wireframe {
                        (Primitive::LineList, unique_triangle_edges(&indices))
                    } else {
                        (Primitive::TriangleList, indices)
                    }
                })
            }
        },
    };
    let allow_missing_normals = !matches!(native, Some((Primitive::TriangleList, _)));
    let vertices = match &reused {
        Some(packed) => Ok(packed.vertices.clone()),
        None => pack_vertices(allow_missing_normals, &view),
    };
    let attributes = pack_attributes(None, &view);
    match (native, vertices, attributes) {
        (Some((primitive, native_indices)), Ok(vertices), Ok(attributes)) if !native_indices.is_empty() => {
            let id = fresh_resource_id();
            let (key, indices) = match &reused {
                Some(packed) => (packed.key.clone(), packed.indices.clone()),
                None => (se::MeshKey::Host(format!("scene3:{id}")), pack_indices(&native_indices)),
            };
            let packed = Rc::new(PackedMesh {
                mesh: m.clone(),
                source,
                mode,
                primitive,
                key,
                vertices,
                indices,
                vertex_attributes: (format!("scene3:attributes:{id}"), attributes),
                vertex_count: view.vertices.x.len() as i64,
                index_count: native_indices.len() as i64,
                opaque: view.colors.as_ref().is_none_or(|colors| colors.iter().all(|c| c.a == 255)),
                gpu_normals: None,
            });
            if let Some(context) = &cache {
                let key: Item = Rc::new((m.clone(), mode));
                idlist::add(&mut context.0.packed_meshes.borrow_mut(), 0, key, packed.clone());
            }
            Ok(packed)
        }
        _ => Err(Error::InvalidMesh),
    }
}

/// Lower one `Scene3` view into prepared draws.
pub(crate) fn prepare(resources: &Resources, cam: &camera::Camera, viewport: ViewportT, scene: &scene3::Scene3) -> Result<Prepared, Error> {
    let (x, y, width, height) = viewport;
    if width <= 0 || height <= 0 {
        return Err(Error::InvalidViewport);
    }
    let cache = current();
    let cache_key: Item = Rc::new((scene.clone(), cam.clone(), viewport));
    if let Some(context) = &cache
        && let Some(found) = idlist::peek(&mut context.0.prepared_cache.borrow_mut(), &cache_key)
            && let Ok(entry) = found.downcast::<PreparedCacheEntry>() {
                return Ok(entry.prepared.clone());
            }
    if scene3::walk::lights(scene).len() > MAX_LIGHTS {
        return Err(Error::TooManyLights);
    }
    let world = match scene3::walk::world(scene) {
        None => None,
        Some(baked) => {
            let fit = match &baked.sun {
                Some(s) => sun_fit(scene, s)?,
                None => None,
            };
            Some(world_of(&baked, fit))
        }
    };
    let mut cacheable = true;
    let mut failure: Option<Error> = None;
    let mut entries: Vec<Draw> = Vec::new();
    if let Some(w) = &world
        && w.baked.background != world::Background::Transparent
            && let Some(uniforms) = background_uniforms(cam, viewport)? {
                let state = se::State {
                    viewport,
                    scissor: viewport,
                    cull: Cull::CullNone,
                    depth_compare: Comparison::Always,
                    depth_write: false,
                    depth_load: Load::Clear,
                    depth_clear: scene3::walk::depth_clear(scene),
                    transform_uniforms: Some(new_bytes(uniforms)),
                };
                let (mesh_, attributes) = BACKGROUND.with(|b| (b.0.clone(), b.1.clone()));
                entries = vec![Rc::new(SampledDraw { ui: None,
                    family: PipelineFamily::Scene3World,
                    blend: Blend::Replace,
                    texture: Some(w.camera_map.clone()),
                    auxiliary: Some(w.auxiliary.clone()),
                    vertex_attributes: Some(attributes),
                    samples: scene3::walk::samples(scene),
                    draw: GeometryDraw { mesh: mesh_, state },
                })];
            }
    scene3::walk::iter_batches(
        &mut |d, transforms| {
            if d.texture.is_some() {
                cacheable = false;
            }
            let nonempty = match transforms {
                Some(values) => !values.is_empty(),
                None => true,
            };
            if failure.is_some() || !nonempty {
                return;
            }
            let packed = if d.texture.is_some() && mesh::view::device(&d.mesh).is_some() {
                packed_of_host(d.mode, &d.mesh)
            } else {
                packed_of_mesh(d.mode, &d.mesh)
            };
            match packed {
                Ok(packed) => {
                    let texture = match &d.texture {
                        None => Ok(None),
                        Some(value) => (resources.texture)(value).map(|t| Some(Rc::new(t))),
                    };
                    let texture = match texture {
                        Err(e) => {
                            failure = Some(e);
                            return;
                        }
                        Ok(t) => t,
                    };
                    let mesh_ = se::Mesh {
                        key: packed.key.clone(),
                        vertices: packed.vertices.clone(),
                        shared_vertices: None,
                        vertex_count: packed.vertex_count,
                        indices: packed.indices.clone(),
                        index_count: packed.index_count,
                        primitive: packed.primitive,
                    };
                    let family = match (packed.primitive, &texture) {
                        (Primitive::PointList, None) => PipelineFamily::Scene3Points,
                        (_, Some(_)) => PipelineFamily::Scene3Textured,
                        (_, None) => PipelineFamily::Scene3,
                    };
                    // ponytail: point clouds keep the Blinn-Phong points pipeline under a World
                    // (no IBL, no tone map).
                    let world_here = match &world {
                        Some(_) if packed.primitive == Primitive::PointList && texture.is_none() => None,
                        w => w.clone(),
                    };
                    let (family, texture, auxiliary) = match &world_here {
                        Some(w) => (
                            PipelineFamily::Scene3World,
                            Some(texture.unwrap_or_else(|| WHITE.with(|t| t.clone()))),
                            Some(w.auxiliary.clone()),
                        ),
                        None => (family, texture, None),
                    };
                    let add = |mut transform_uniforms: Vec<u8>, entries: &mut Vec<Draw>| {
                        // a device mesh's packed positions (word 79) and normals (word 80)
                        if let Some(normals) = &packed.gpu_normals {
                            put32(&mut transform_uniforms, 79, 1.);
                            put32(&mut transform_uniforms, 80, if *normals { 1. } else { 0. });
                            let width_ = mesh::view::device_color_width(&packed.mesh);
                            put32(
                                &mut transform_uniforms,
                                81,
                                if width_ == 0 && packed.vertex_attributes.1.is_empty() { -1. } else { width_ as f64 },
                            );
                        }
                        let state = se::State {
                            viewport: (x, y, width, height),
                            scissor: (x, y, width, height),
                            cull: cull(d.cull),
                            depth_compare: comparison(d.depth.comparison),
                            depth_write: d.depth.write,
                            depth_load: Load::Clear,
                            depth_clear: scene3::walk::depth_clear(scene),
                            transform_uniforms: Some(new_bytes(transform_uniforms)),
                        };
                        // The default blend is Alpha. A draw whose every fragment has alpha 1
                        // (opaque material and vertex colors, no texture) writes the same pixels
                        // with Replace, and only an unblended pipeline lets a tile-based GPU drop
                        // hidden fragments before shading them: a dense mesh otherwise shades
                        // every layer the depth test lets by.
                        let blend_ = match d.blend {
                            scene3::Blend::Alpha if d.texture.is_none() && d.material.diffuse.a == 255 && packed.opaque => Blend::Replace,
                            other => blend(other),
                        };
                        entries.push(Rc::new(SampledDraw { ui: None,
                            family,
                            blend: blend_,
                            texture: texture.clone(),
                            auxiliary: auxiliary.clone(),
                            vertex_attributes: Some(packed.vertex_attributes.clone()),
                            samples: scene3::walk::samples(scene),
                            draw: GeometryDraw { mesh: mesh_.clone(), state },
                        }));
                    };
                    let w_ref = world_here.as_deref();
                    match transforms {
                        None => match uniforms(w_ref, cam, viewport, scene, d) {
                            Ok(u) => add(u, &mut entries),
                            Err(e) => failure = Some(e),
                        },
                        Some(transforms) => {
                            // ponytail: 8192 instances per draw caps the temporary upload near
                            // 1.6 MB; larger scenes become consecutive draws.
                            let batch = 8192;
                            for start in 0..=(transforms.len() - 1) / batch {
                                let first = start * batch;
                                let count = std::cmp::min(batch, transforms.len() - first);
                                match instance_uniforms(w_ref, cam, viewport, scene, d, transforms, first, count) {
                                    Ok(u) => add(u, &mut entries),
                                    Err(e) => {
                                        failure = Some(e);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                // An empty mesh (a cook that produced no triangles) draws nothing: skip it
                // instead of failing the whole view.
                Err(_)
                    if mesh::vertex_count(&d.mesh) == 0
                        || (mesh::index_count(&d.mesh) < 3 && mesh::mode(&d.mesh) != mesh::Mode::Points) => {}
                Err(_) => failure = Some(Error::InvalidMesh),
            }
        },
        scene,
    );
    if let Some(e) = failure {
        return Err(e);
    }
    match se::prepare_scene3((0., 0., 0., 0.), scene3::walk::depth_clear(scene), &entries) {
        Ok(prepared_) => {
            let prepared_ = Rc::new(prepared_);
            if cacheable
                && let Some(context) = &cache {
                    let entry = Rc::new(PreparedCacheEntry { scene: scene.clone(), prepared: prepared_.clone() });
                    let mut list = context.0.prepared_cache.borrow_mut();
                    idlist::add(&mut list, 0, cache_key, entry);
                    trim_prepared_cache(&mut list);
                }
            Ok(prepared_)
        }
        Err(_) => Err(Error::InvalidMesh),
    }
}

