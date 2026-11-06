use crate::scene_attachment_pool;
use crate::side_lru::{self, Item, TableKey};
use ogpu::backend::{
    self, Argument, BatchDraw, Buffer, ColorAttachment, DepthAttachment, DepthState, Device, Icb,
    Pipeline, Queue, RenderTarget, ResourceUse, Sampler, ShaderStage, Surface, Texture,
};
use ogpu::caps::{self, Feature};
use ogpu::error::{self, Error, ErrorKind};
use ogpu::pipeline::Blend;
use ogpu::render_pass::{Comparison, Cull, IndexType, Load, Primitive, Rect, Store};
use ogpu::surface::Configuration;
use ogpu::types::{
    BufferDescriptor, BufferUsage, Extent, SamplerDescriptor, TextureDescriptor, TextureFormat,
    TextureUsage,
};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, OnceLock};

/// OCaml `bytes`, compared by physical equality where the OCaml code does (`Rc::ptr_eq`).
pub use runtime_resources::bytes;

pub fn new_bytes(value: Vec<u8>) -> bytes {
    Rc::new(RefCell::new(value))
}

pub fn empty_bytes() -> bytes {
    new_bytes(Vec::new())
}

fn blen(value: &bytes) -> usize {
    value.borrow().len()
}

/// OCaml `Sys.max_string_length` on 64-bit.
const MAX_STRING_LENGTH: i64 = 144115188075855863;


fn failure_error(operation: &str, kind: ErrorKind, text: &str) -> Error {
    error::make(operation, kind, text)
}

fn thread_id() -> std::thread::ThreadId {
    std::thread::current().id()
}

// ---------------------------------------------------------------------------------------------
// GPU vertices are borrowed, never uploaded or destroyed by the renderer.
// Registration owns only a bounded name and the producer's lifetime check.

pub struct gpu_vertices_data {
    pub gpu_owner: gpu_vertices_owner,
    pub gpu_key: String,
    pub gpu_device: Device,
    pub gpu_buffer: Buffer,
    pub gpu_extra: Vec<Buffer>,
    pub gpu_vertex_count: i64,
    pub gpu_domain: std::thread::ThreadId,
    /// device-written index count: an indirect draw
    pub gpu_indirect: Option<(Buffer, i64)>,
    pub gpu_indices: Option<(Buffer, i64)>,
    pub gpu_colors: Option<(Buffer, i64)>,
    pub gpu_valid: Box<dyn Fn() -> bool>,
    pub gpu_active: Cell<bool>,
}

pub type gpu_vertices = Rc<gpu_vertices_data>;

pub struct gpu_vertices_owner_data {
    pub gpu_vertex_registry: HashMap<String, gpu_vertices>,
    pub unregistered: i64,
}

pub type gpu_vertices_owner = Rc<RefCell<gpu_vertices_owner_data>>;

pub fn create_gpu_vertices_owner() -> gpu_vertices_owner {
    Rc::new(RefCell::new(gpu_vertices_owner_data { gpu_vertex_registry: HashMap::new(), unregistered: 0 }))
}

const gpu_vertex_capacity: usize = 128;

/// The ids that identify a source's buffers in signatures and fingerprints.
pub fn gpu_identity(source: &gpu_vertices) -> (String, i64, Vec<i64>) {
    let mut ids: Vec<i64> = source.gpu_extra.iter().map(backend::buffer_id).collect();
    ids.extend(source.gpu_indirect.iter().map(|(buffer, _)| backend::buffer_id(buffer)));
    ids.extend(source.gpu_indices.iter().map(|(buffer, _)| backend::buffer_id(buffer)));
    ids.extend(source.gpu_colors.iter().map(|(buffer, _)| backend::buffer_id(buffer)));
    (source.gpu_key.clone(), backend::buffer_id(&source.gpu_buffer), ids)
}

pub fn gpu_vertices_valid(value: &gpu_vertices) -> bool {
    value.gpu_active.get() && value.gpu_domain == thread_id() && (value.gpu_valid)()
}

/// `register_gpu_vertices owner ?extra ?stride ?indirect ?indices ?colors ~key ~device ~buffer
/// ~vertex_count ~valid ()`; `stride` defaults to 16, `extra` to none.
pub fn register_gpu_vertices(
    owner: &gpu_vertices_owner,
    extra: Option<Vec<Buffer>>,
    stride: Option<i64>,
    indirect: Option<(Buffer, i64)>,
    indices: Option<(Buffer, i64)>,
    colors: Option<(Buffer, i64)>,
    key: &str,
    device: &Device,
    buffer: &Buffer,
    vertex_count: i64,
    valid: Box<dyn Fn() -> bool>,
) -> Result<gpu_vertices, Error> {
    let operation = "Scene_execution.register_gpu_vertices";
    let extra = extra.unwrap_or_default();
    let stride = stride.unwrap_or(16);
    let registry_has = owner.borrow().gpu_vertex_registry.contains_key(key);
    let invalid = key.is_empty()
        || vertex_count <= 0
        || vertex_count > 1 << 30
        || (extra.is_empty() && backend::buffer_size(buffer) < vertex_count * stride)
        || std::iter::once(buffer).chain(extra.iter()).any(|b| backend::buffer_size(b) <= 0)
        || match &indirect {
            Some((b, offset)) => *offset < 0 || offset + 20 > backend::buffer_size(b),
            None => false,
        }
        || match &indices {
            Some((b, count)) => {
                *count <= 0
                    || *count > MAX_STRING_LENGTH / 4
                    || backend::buffer_size(b) < count * 4
                    || indirect.is_some()
            }
            None => false,
        }
        || match &colors {
            Some((b, width)) => (*width != 3 && *width != 4) || backend::buffer_size(b) < vertex_count * width * 4,
            None => false,
        }
        || registry_has;
    if invalid {
        return Err(failure_error(operation, ErrorKind::InvalidArgument, "invalid or duplicate borrowed vertex registration"));
    }
    if owner.borrow().gpu_vertex_registry.len() >= gpu_vertex_capacity {
        return Err(failure_error(operation, ErrorKind::Capacity, "borrowed vertex registration capacity reached"));
    }
    let value: gpu_vertices = Rc::new(gpu_vertices_data {
        gpu_owner: owner.clone(),
        gpu_key: key.to_owned(),
        gpu_device: device.clone(),
        gpu_buffer: buffer.clone(),
        gpu_extra: extra,
        gpu_vertex_count: vertex_count,
        gpu_indirect: indirect,
        gpu_indices: indices,
        gpu_colors: colors,
        gpu_domain: thread_id(),
        gpu_valid: valid,
        gpu_active: Cell::new(true),
    });
    owner.borrow_mut().gpu_vertex_registry.insert(key.to_owned(), value.clone());
    Ok(value)
}

pub fn unregister_gpu_vertices(value: &gpu_vertices) {
    if value.gpu_domain != thread_id() {
        unreachable!("GPU vertices closed on another domain");
    }
    if value.gpu_active.get() {
        value.gpu_active.set(false);
        let mut owner = value.gpu_owner.borrow_mut();
        owner.unregistered += 1;
        owner.gpu_vertex_registry.remove(&value.gpu_key);
    }
}

/// A borrowed GPU vertex stream is named apart from every host-owned mesh. A name
/// ["borrow\nindices"] keeps its host payload (the indices) under [indices], the same while its
/// topology is, whatever buffer each frame borrows.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum mesh_key {
    Host(String),
    /// `Gpu_vertices name`: a stream borrowed through `register_gpu_vertices ~key:name`.
    Gpu_vertices(String),
}

fn borrowed(name: &str) -> &str {
    match name.find('\n') {
        Some(i) => &name[..i],
        None => name,
    }
}

pub fn mesh_key_string(key: &mesh_key) -> String {
    match key {
        mesh_key::Host(k) => k.clone(),
        mesh_key::Gpu_vertices(k) => match k.find('\n') {
            Some(i) => format!("gpu:indices:{}", &k[i + 1..]),
            None => format!("gpu:vertices:{}", k),
        },
    }
}

#[derive(Clone)]
pub struct mesh {
    pub key: mesh_key,
    pub vertices: bytes,
    /// Immutable published UI vertices; when present, `vertices` must be empty.
    pub shared_vertices: Option<Arc<Vec<u8>>>,
    pub vertex_count: i64,
    pub indices: bytes,
    pub index_count: i64,
    pub primitive: Primitive,
}

impl mesh {
    pub fn vertex_bytes_len(&self) -> usize {
        self.shared_vertices.as_ref().map_or_else(|| blen(&self.vertices), |bytes| bytes.len())
    }
}

/// Equality is structural, as OCaml's `=` on the record (bytes by content, floats by value).
#[derive(Clone, PartialEq)]
pub struct state {
    pub viewport: (i64, i64, i64, i64),
    pub scissor: (i64, i64, i64, i64),
    pub cull: Cull,
    pub depth_compare: Comparison,
    pub depth_write: bool,
    pub depth_load: Load,
    pub depth_clear: f64,
    pub transform_uniforms: Option<bytes>,
}

#[derive(Clone)]
pub struct geometry_draw {
    pub mesh: mesh,
    pub state: state,
}

/// Exact number of family/blend variants required for each supported sample count.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum pipeline_family {
    Scene2,
    Scene2_textured,
    Scene3,
    Scene3_points,
    Scene3_textured,
    Scene3_world,
    Ui,
    Marks,
}
use pipeline_family::*;

/// Every level holds `width * height` texels of 4, 8 or 16 bytes (the size level 0 implies):
/// `Rgba8_unorm`, `Rgba16_float` or `Rgba32_float`.
#[derive(Clone)]
pub struct texture_level {
    pub width: i64,
    pub height: i64,
    pub bytes: bytes,
}

/// `levels` is an `Rc` so that "the immutable levels this slot was uploaded from" stays a
/// physical-identity test.
#[derive(Clone)]
pub struct sampled_texture {
    pub key: String,
    pub levels: Rc<Vec<texture_level>>,
    pub sampler: SamplerDescriptor,
    pub gpu: Option<Texture>,
}

#[derive(Clone)]
pub struct auxiliary_resource {
    pub key: String,
    pub buffer: bytes,
    pub texture: sampled_texture,
    /// A `Scene3_world` draw's camera map (texture 9, sampler 10).
    pub environment: Option<sampled_texture>,
    /// `Some key`: this World block (sun view-projection at words 40..55) samples the renderer's
    /// sun map at texture 8, rendered from the frame's depth-writing `Replace`/`Alpha` World
    /// triangle draws with the same key whenever the key differs from the last rendered one.
    pub sun_shadow: Option<String>,
}

/// `texture` and `auxiliary` are `Rc`s: draws are merged only when they share the same ones.
#[derive(Clone)]
pub struct sampled_draw {
    pub family: pipeline_family,
    pub blend: Blend,
    pub texture: Option<Rc<sampled_texture>>,
    pub auxiliary: Option<Rc<auxiliary_resource>>,
    /// Independently retained Scene3 color/UV stream (12 bytes per vertex), bound at vertex buffer
    /// 11. Required (and only valid) for the Scene3 families; 2D, UI and Marks draws carry `None`.
    pub vertex_attributes: Option<(String, bytes)>,
    pub samples: i64,
    pub draw: geometry_draw,
}

/// A draw handed around by identity (OCaml's physical equality on the record).
pub type draw = Rc<sampled_draw>;

pub type scene3_entry = draw;

pub struct prepared_scene3 {
    pub entries: Vec<draw>,
}

/// A draw names a borrowed stream that was unregistered: it can never be drawn again.
pub fn borrows_unregistered(owner: &gpu_vertices_owner, prepared: &prepared_scene3) -> bool {
    prepared.entries.iter().any(|entry| match &entry.draw.mesh.key {
        mesh_key::Gpu_vertices(name) => !owner.borrow().gpu_vertex_registry.contains_key(borrowed(name)),
        mesh_key::Host(_) => false,
    })
}

struct cached {
    key: String,
    payload_hash: String,
    trusted_source: Option<mesh>,
    buffer: Buffer,
    index_offset: i64,
    uniform_offset: Option<i64>,
    uniform_copy: Vec<u8>,
    vertex_count: i64,
    index_count: i64,
    primitive: Primitive,
    bytes: i64,
}
type cached_ref = Rc<RefCell<cached>>;

#[derive(Clone)]
struct uniform_slice {
    uniform_buffer: Buffer,
    uniform_offset: i64,
    /// The frame's staging bytes and this slice's window into them, kept for next frame's
    /// unchanged-set comparison without a per-draw copy.
    uniform_staging: Rc<Vec<u8>>,
    uniform_staging_offset: usize,
    uniform_length: usize,
}

struct uniform_page {
    page_buffer: Buffer,
    page_capacity: i64,
}

struct cached_auxiliary {
    auxiliary_copy: Vec<u8>,
    auxiliary_buffer: Buffer,
    /// The bytes this entry was uploaded from: a physically equal source is a hit without comparing
    /// (World blocks can carry a whole shadow map).
    auxiliary_source: bytes,
}
type cached_auxiliary_ref = Rc<RefCell<cached_auxiliary>>;

struct cached_texture {
    texture_key: String,
    texture_hash: String,
    texture_shape: String,
    texture: Texture,
    texture_bytes: i64,
    texture_used: i64,
    texture_levels: Rc<Vec<texture_level>>,
}
type cached_texture_ref = Rc<RefCell<cached_texture>>;

struct texture_upload_scratch {
    scratch_buffer: Buffer,
    scratch_bytes: Vec<u8>,
    scratch_size: i64,
}

struct prepared_run {
    prepared_version: i64,
    prepared_draws: Vec<draw>,
    prepared_bytes: i64,
}

#[derive(Clone)]
struct automatic_signature {
    signature_family: pipeline_family,
    signature_blend: Blend,
    signature_samples: i64,
    signature_state: state,
    signature_texture: Option<(i64, SamplerDescriptor)>,
    signature_auxiliary: Option<(i64, i64, SamplerDescriptor)>,
    signature_environment: Option<(i64, SamplerDescriptor)>,
    signature_buffer: i64,
    signature_index_offset: i64,
    signature_gpu_vertices: Option<(String, i64, Vec<i64>)>,
    signature_vertex_attributes: Option<i64>,
    signature_uniform_buffer: Option<i64>,
    signature_uniform_offset: Option<i64>,
    signature_vertex_count: i64,
    signature_index_count: i64,
}

#[derive(Clone)]
struct plan_draw {
    plan_draw: BatchDraw,
    plan_textures: Vec<(ShaderStage, i64, Texture)>,
    plan_samplers: Vec<(ShaderStage, i64, Sampler)>,
    /// Textures reached only through an argument buffer: Metal needs them made resident with
    /// `use_resources` before any draw of the pass.
    plan_resident: Vec<Texture>,
    /// A borrowed shape whose index count the device wrote: drawn indirect, never batched or recorded
    plan_indirect: Option<(Buffer, i64)>,
}

/// One render pass of a frame: its attachment state and draws. A retained batch replays an
/// indirect command buffer instead of re-encoding.
struct batch_plan {
    batch_family: pipeline_family,
    batch_samples: i64,
    batch_state: state,
    batch_first: bool,
    batch_draws: Vec<plan_draw>,
    /// `batch_draws` as the backend batch, built once with the plan.
    batch_driver_draws: Vec<BatchDraw>,
    batch_resources: Vec<ResourceUse>,
    batch_icb: RefCell<Option<Icb>>,
}

struct replay_plan {
    replay_clear: (f64, f64, f64, f64),
    replay_payloads: Vec<automatic_signature>,
    replay_batches: Vec<Rc<batch_plan>>,
    replay_gpu_vertices: Vec<gpu_vertices>,
}

struct prepared_submission {
    submission_identity: String,
    submission_version: i64,
    submission_draw_count: i64,
    submission_plan: Rc<replay_plan>,
}

struct automatic_candidate {
    candidate_clear: (f64, f64, f64, f64),
    candidate_length: i64,
    candidate_fingerprint: i64,
}

struct prepared_slot {
    slot_family: pipeline_family,
    slot_blend: Blend,
    slot_samples: i64,
    slot_state: Option<state>,
    slot_texture: Option<(Rc<sampled_texture>, cached_texture_ref)>,
    slot_auxiliary: Option<(Rc<auxiliary_resource>, cached_auxiliary_ref, cached_texture_ref)>,
    slot_environment: Option<(sampled_texture, cached_texture_ref)>,
    slot_mesh: Option<cached_ref>,
    slot_uniform: Option<uniform_slice>,
    slot_gpu_vertices: Option<gpu_vertices>,
    slot_vertex_attributes: Option<cached_auxiliary_ref>,
}

struct prepared_scratch {
    scratch_slots: Vec<prepared_slot>,
    scratch_length: usize,
}

struct pipeline_variant {
    family: pipeline_family,
    blend: Blend,
    samples: i64,
    pipeline: Pipeline,
    argument: Option<Argument>,
}

struct icb_stats {
    icb_builds: i64,
    icb_hits: i64,
    icb_misses: i64,
    icb_evictions: i64,
    icb_executions: i64,
    icb_failures: i64,
    icb_last_failure: Option<String>,
}

struct argument_pool {
    argument_buffer: Buffer,
    argument_stride: i64,
    argument_capacity: i64,
    slices: HashMap<(i64, String), i64>,
    argument_next: i64,
}

#[derive(Default)]
struct retired {
    retired_buffers: Vec<Buffer>,
    retired_textures: Vec<Texture>,
}

const cache_byte_capacity: i64 = 256 * 1024 * 1024;
const mesh_cache_entry_capacity: i64 = 256;
const texture_cache_entry_capacity: i64 = 256;
const texture_cache_byte_capacity: i64 = 256 * 1024 * 1024;

fn device_cache(name: &str) -> lru::Owner {
    lru::cache_in(lru::Pool::Device, name)
}

fn mesh_table(retired: &Rc<RefCell<retired>>) -> side_lru::Table {
    let retired = retired.clone();
    side_lru::create(
        device_cache("renderer.mesh"),
        true,
        cache_byte_capacity,
        Box::new(move |_, item| {
            let item = cached_of(&item);
            retired.borrow_mut().retired_buffers.push(item.borrow().buffer.clone());
        }),
        mesh_cache_entry_capacity,
    )
}

fn texture_table(retired: &Rc<RefCell<retired>>) -> side_lru::Table {
    let retired = retired.clone();
    side_lru::create(
        device_cache("renderer.texture"),
        true,
        texture_cache_byte_capacity,
        Box::new(move |_, item| {
            let item = cached_texture_of(&item);
            retired.borrow_mut().retired_textures.push(item.borrow().texture.clone());
        }),
        texture_cache_entry_capacity,
    )
}

fn auxiliary_table(retired: &Rc<RefCell<retired>>) -> side_lru::Table {
    let retired = retired.clone();
    side_lru::create(
        device_cache("renderer.gpu_item"),
        true,
        cache_byte_capacity,
        Box::new(move |_, item| {
            let item = cached_auxiliary_of(&item);
            retired.borrow_mut().retired_buffers.push(item.borrow().auxiliary_buffer.clone());
        }),
        64,
    )
}

fn cached_of(item: &Item) -> cached_ref {
    item.clone().downcast::<RefCell<cached>>().unwrap_or_else(|_| unreachable!("mesh table item"))
}

fn cached_texture_of(item: &Item) -> cached_texture_ref {
    item.clone().downcast::<RefCell<cached_texture>>().unwrap_or_else(|_| unreachable!("texture table item"))
}

fn cached_auxiliary_of(item: &Item) -> cached_auxiliary_ref {
    item.clone().downcast::<RefCell<cached_auxiliary>>().unwrap_or_else(|_| unreachable!("auxiliary table item"))
}

fn prepared_run_of(item: &Item) -> Rc<prepared_run> {
    item.clone().downcast::<prepared_run>().unwrap_or_else(|_| unreachable!("prepared table item"))
}

fn skey(key: &str) -> TableKey {
    TableKey::Str(key.to_owned())
}

/// Evicted GPU resources are destroyed once the frame that evicted them has been submitted.
fn drain_retired(retired: &Rc<RefCell<retired>>, destroy_buffer: &mut dyn FnMut(&Buffer), destroy_texture: &mut dyn FnMut(&Texture)) {
    let (buffers, textures) = {
        let mut r = retired.borrow_mut();
        (std::mem::take(&mut r.retired_buffers), std::mem::take(&mut r.retired_textures))
    };
    for b in &buffers {
        destroy_buffer(b);
    }
    for t in &textures {
        destroy_texture(t);
    }
}

/// The World sun shadow map: packed 24-bit depth in RGBA8 plus the depth attachment its pass
/// tests against, reused across frames (capacity 1).
struct sun_map {
    sun_pipeline: Pipeline,
    sun_color: Texture,
    sun_depth: Texture,
    sun_key: String,
}

pub type sun_factory = Rc<dyn Fn(&Device) -> Result<Pipeline, Error>>;

pub struct t {
    device: Device,
    gpu_vertices: gpu_vertices_owner,
    queue: Queue,
    surface: Option<Surface>,
    target: Texture,
    configuration: Configuration,
    attachments: scene_attachment_pool::t,
    pipelines: Vec<pipeline_variant>,
    pipeline_lookup: Vec<Option<usize>>,
    cache: side_lru::Table,
    uniform_pages: Vec<Option<uniform_page>>,
    next_uniform_page: usize,
    /// frames in a row needing under a quarter of the page
    uniform_low: i64,
    previous_uniforms: Vec<Option<uniform_slice>>,
    prepared_cache: side_lru::Table,
    prepared_submission: Option<prepared_submission>,
    automatic_submission: Option<Rc<replay_plan>>,
    automatic_candidate: Option<automatic_candidate>,
    prepared_scratch: prepared_scratch,
    auxiliary_cache: side_lru::Table,
    texture_cache: side_lru::Table,
    /// Evicted GPU resources wait here until the frame that evicted them has been submitted.
    retired: Rc<RefCell<retired>>,
    frame: i64,
    texture_upload_scratch: Option<texture_upload_scratch>,
    /// the last frame that uploaded a texture
    staged_frame: i64,
    /// and the frame before it did too
    staged_stream: bool,
    samplers: HashMap<String, Sampler>,
    arguments: Option<argument_pool>,
    icb_stats: icb_stats,
    uploaded: i64,
    swept_unregistered: i64,
    sun_factory: Option<sun_factory>,
    sun: Option<sun_map>,
    sun_passes: i64,
    in_flight: Option<i64>,
    dead: bool,
    /// A borrowed device (shared with a window) is never destroyed here.
    owns_device: bool,
}

fn drop_plan(value: &mut t, plan: Option<Rc<replay_plan>>) {
    let Some(plan) = plan else { return };
    for batch in &plan.replay_batches {
        let icb = batch.batch_icb.borrow_mut().take();
        if let Some(icb) = icb {
            value.icb_stats.icb_evictions += 1;
            if let Err(failure) = backend::destroy_icb(&icb) {
                value.icb_stats.icb_failures += 1;
                value.icb_stats.icb_last_failure = Some(format!("destroy: {}", error::to_string(&failure)));
            }
        }
    }
}

fn drop_plans(value: &mut t) {
    let automatic = value.automatic_submission.take();
    drop_plan(value, automatic);
    let prepared = value.prepared_submission.take().map(|s| s.submission_plan);
    drop_plan(value, prepared);
    value.automatic_candidate = None;
}

pub fn device(value: &t) -> Device {
    value.device.clone()
}
pub fn queue(value: &t) -> Queue {
    value.queue.clone()
}
/// The owned RGBA8 texture every submission renders into; a shared-device renderer's callers may
/// sample it directly between completed submissions.
pub fn target(value: &t) -> Texture {
    value.target.clone()
}

fn same_device(a: &Device, b: &Device) -> bool {
    ogpu::handle::device_id(backend::device_handle(a)) == ogpu::handle::device_id(backend::device_handle(b))
}

fn is_scene3_family(family: pipeline_family) -> bool {
    matches!(family, Scene3 | Scene3_points | Scene3_world)
}

fn resolve_gpu_vertices(value: &t, entry: &sampled_draw) -> Result<Option<gpu_vertices>, Error> {
    let mesh = &entry.draw.mesh;
    let name = match &mesh.key {
        mesh_key::Host(_) => return Ok(None),
        mesh_key::Gpu_vertices(name) => name,
    };
    let found = value.gpu_vertices.borrow().gpu_vertex_registry.get(borrowed(name)).cloned();
    match found {
        Some(source) if gpu_vertices_valid(&source) => {
            let color_width = match &entry.draw.state.transform_uniforms {
                Some(bytes_) if blen(bytes_) >= 82 * 4 => {
                    let b = bytes_.borrow();
                    f32::from_bits(u32::from_le_bytes(b[81 * 4..81 * 4 + 4].try_into().unwrap())) as f64
                }
                _ => 0.0,
            };
            let family = entry.family;
            let indices_len = blen(&mesh.indices) as i64;
            let vertices_len = mesh.vertex_bytes_len() as i64;
            if !same_device(&source.gpu_device, &value.device) {
                return Err(failure_error("Renderer.submit", ErrorKind::CrossDevice, "GPU vertices belong to another device"));
            }
            let colors_bad = (source.gpu_colors.is_some() && !is_scene3_family(family))
                || (is_scene3_family(family)
                    && color_width
                        != match (&source.gpu_colors, &entry.vertex_attributes) {
                            (Some((_, width)), _) => *width as f64,
                            (None, Some((_, b))) if blen(b) == 0 => -1.0,
                            (None, _) => 0.0,
                        });
            let indices_bad = match &source.gpu_indices {
                Some((_, count)) => !is_scene3_family(family) || indices_len != 0 || mesh.index_count != *count,
                None => indices_len != mesh.index_count * 4,
            };
            let layout_bad = match (family, source.gpu_extra.len()) {
                (Scene3 | Scene3_points | Scene3_world, 0 | 1) => {
                    // a device's packed float3 positions (and normals)
                    vertices_len != 0 || mesh.vertex_count != source.gpu_vertex_count
                }
                (Ui, 0) | (Marks, 5) => {
                    vertices_len != 0
                        || mesh.vertex_count != source.gpu_vertex_count
                        || mesh.vertex_count % 4 != 0
                        || mesh.index_count != mesh.vertex_count / 4 * 6
                }
                _ => true,
            };
            if colors_bad || indices_bad || layout_bad {
                Err(failure_error("Renderer.submit", ErrorKind::InvalidArgument, "GPU shape layout is malformed"))
            } else {
                Ok(Some(source))
            }
        }
        _ => Err(failure_error("Renderer.submit", ErrorKind::StaleHandle, "GPU vertices are stale or closed")),
    }
}

/// Scene3 draws carry the 12-byte attribute record a vertex; 2D, UI and Marks draws carry none.
fn valid_vertex_attributes(entry: &sampled_draw) -> bool {
    match (entry.family, &entry.vertex_attributes) {
        (Scene2 | Scene2_textured | Ui | Marks, None) => true,
        (Scene2 | Scene2_textured | Ui | Marks, Some(_)) | (Scene3 | Scene3_points | Scene3_textured | Scene3_world, None) => false,
        (_, Some((key, attributes))) => {
            let mesh = &entry.draw.mesh;
            !key.is_empty()
                && mesh.vertex_count >= 0
                && mesh.vertex_count <= MAX_STRING_LENGTH / 24
                && match &mesh.key {
                    mesh_key::Gpu_vertices(_) => mesh.vertex_bytes_len() == 0,
                    mesh_key::Host(_) => mesh.vertex_bytes_len() as i64 == mesh.vertex_count * 24,
                }
                && (blen(attributes) as i64 == mesh.vertex_count * 12
                    || match &mesh.key {
                        mesh_key::Gpu_vertices(_) => blen(attributes) == 0,
                        mesh_key::Host(_) => false,
                    })
        }
    }
}

fn valid_index_length(mesh: &mesh) -> bool {
    mesh.index_count >= 0
        && mesh.index_count <= MAX_STRING_LENGTH / 4
        && (blen(&mesh.indices) as i64 == mesh.index_count * 4
            || match &mesh.key {
                mesh_key::Gpu_vertices(_) => blen(&mesh.indices) == 0,
                mesh_key::Host(_) => false,
            })
}

/// Validates and copies a native Scene3 submission description. Nested byte resources remain
/// borrowed and must be retained by the staging owner.
pub fn prepare_scene3(clear: (f64, f64, f64, f64), clear_depth: f64, entries: &[draw]) -> Result<prepared_scene3, Error> {
    let (r, g, b, a) = clear;
    let valid_extent = |(_, _, width, height): (i64, i64, i64, i64)| width > 0 && height > 0;
    let valid_family = |entry: &sampled_draw| match (entry.family, &entry.texture, &entry.auxiliary) {
        (Scene3, None, None) | (Scene3_points, None, None) => true,
        (Scene3_textured, Some(_), None) => true,
        (Scene3_world, Some(_), Some(auxiliary)) => auxiliary.environment.is_some(),
        _ => false,
    };
    let valid_entry = |entry: &sampled_draw| {
        let mesh = &entry.draw.mesh;
        entry.samples > 0
            && [1, 4, 9, 16].contains(&entry.samples)
            && valid_family(entry)
            && !mesh_key_string(&mesh.key).is_empty()
            && mesh.vertex_count >= 0
            && mesh.index_count >= 0
            && (mesh.shared_vertices.is_none() || blen(&mesh.vertices)==0)
            && ((mesh.primitive == Primitive::PointList) == (entry.family == Scene3_points))
            && match mesh.primitive {
                Primitive::LineList => mesh.index_count % 2 == 0,
                Primitive::TriangleList => mesh.index_count % 3 == 0,
                Primitive::PointList | Primitive::TriangleStrip => true,
            }
            && valid_index_length(mesh)
            && valid_vertex_attributes(entry)
            && valid_extent(entry.draw.state.viewport)
            && valid_extent(entry.draw.state.scissor)
            && entry.draw.state.depth_clear.is_finite()
    };
    let operation = "Scene_execution.prepare_scene3";
    if ![r, g, b, a, clear_depth].iter().all(|v| v.is_finite()) || clear_depth < 0.0 || clear_depth > 1.0 {
        Err(failure_error(operation, ErrorKind::InvalidArgument, "clear state is malformed"))
    } else if !entries.iter().all(|entry| valid_entry(entry)) {
        Err(failure_error(operation, ErrorKind::InvalidArgument, "draw family, resources, samples, mesh, or extent is malformed"))
    } else {
        Ok(prepared_scene3 { entries: entries.to_vec() })
    }
}

fn set_u32_le(target: &mut [u8], offset: usize, value: u32) {
    target[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn texture_descriptor(configuration: &Configuration) -> TextureDescriptor {
    TextureDescriptor {
        label: Some("scene-execution-target".to_owned()),
        width: configuration.physical_width,
        height: configuration.physical_height,
        depth: 1,
        mip_levels: 1,
        sample_count: 1,
        format: TextureFormat::Rgba8Unorm,
        usage: vec![TextureUsage::TextureBinding, TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc],
    }
}

const blends: [Blend; 6] = [Blend::Replace, Blend::Alpha, Blend::Add, Blend::Multiply, Blend::Screen, Blend::Subtract];

const families: [pipeline_family; 8] = [Scene2, Scene2_textured, Scene3, Scene3_points, Scene3_textured, Scene3_world, Ui, Marks];

const blend_count: i64 = 6;
const pipeline_variants_per_sample: i64 = 8 * blend_count;

fn pipeline_slot(family: pipeline_family, blend: Blend, samples: i64) -> i64 {
    let family = match family {
        Scene2 => 0,
        Scene2_textured => 1,
        Scene3 => 2,
        Scene3_points => 3,
        Scene3_textured => 4,
        Ui => 5,
        Scene3_world => 6,
        Marks => 7,
    };
    let blend = match blend {
        Blend::Replace => 0,
        Blend::Alpha => 1,
        Blend::Add => 2,
        Blend::Multiply => 3,
        Blend::Screen => 4,
        Blend::Subtract => 5,
    };
    let samples = match samples {
        1 => 0,
        4 => 1,
        9 => 2,
        16 => 3,
        _ => -1,
    };
    if samples < 0 { -1 } else { ((family * blend_count + blend) * 4) + samples }
}

fn find_pipeline(value: &t, family: pipeline_family, blend: Blend, samples: i64) -> Option<usize> {
    let slot = pipeline_slot(family, blend, samples);
    if slot < 0 { None } else { value.pipeline_lookup[slot as usize] }
}

fn sample_counts(device: &Device) -> Vec<i64> {
    let max = backend::capabilities(device).limits.max_sample_count;
    [1, 4, 9, 16].into_iter().filter(|samples| *samples <= max).collect()
}

fn allocate_target(device: &Device, configuration: &Configuration) -> Result<Texture, Error> {
    backend::create_texture(device, texture_descriptor(configuration))
}

/// Creates a renderer with one pipeline per family, blend, and supported sample count. The factory
/// receives the renderer's device and returns owned OGPU render pipelines (see
/// `backend::create_render_pipeline`); the renderer destroys them.
///
/// With `offscreen` the renderer owns a texture target without creating, acquiring, or presenting a
/// platform surface. With `device`, the renderer borrows that device (the driver is unused) and
/// never destroys it, so a Canvas can share the presenting window's GPU. `sun_depth` makes the
/// pipeline of the World sun map pass (RGBA8 color, Depth32 depth, one sample), created on first
/// use; without it a sun-shadowed World draw is `Unsupported`.
pub fn create(
    device: Option<Device>,
    gpu_vertices: Option<gpu_vertices_owner>,
    sun_depth: Option<sun_factory>,
    offscreen: bool,
    driver: backend::Driver,
    configuration: &Configuration,
    make: &dyn Fn(&Device, pipeline_family, Blend, i64) -> Result<Pipeline, Error>,
) -> Result<t, Error> {
    let owns_device = device.is_none();
    let device = match device {
        Some(device) => device,
        None => backend::create_device(driver)?,
    };
    let cleanup = |device: &Device| {
        if owns_device {
            let _ = backend::destroy_device(device);
        }
    };
    let queue = match backend::create_queue(&device) {
        Ok(queue) => queue,
        Err(e) => {
            cleanup(&device);
            return Err(e);
        }
    };
    let surface = if offscreen {
        None
    } else {
        match backend::create_surface(&device, configuration.clone()) {
            Ok(surface) => Some(surface),
            Err(e) => {
                let _ = backend::destroy_queue(&queue);
                cleanup(&device);
                return Err(e);
            }
        }
    };
    let destroy_surface = |surface: &Option<Surface>| {
        if let Some(surface) = surface {
            let _ = backend::destroy_surface(surface);
        }
    };
    let samples = sample_counts(&device);
    let mut pipelines: Vec<pipeline_variant> = Vec::new();
    for family in families {
        for blend in blends {
            for sample in &samples {
                match make(&device, family, blend, *sample) {
                    Ok(pipeline) => pipelines.push(pipeline_variant { family, blend, samples: *sample, pipeline, argument: None }),
                    Err(e) => {
                        for variant in &pipelines {
                            let _ = backend::destroy_pipeline(&variant.pipeline);
                        }
                        destroy_surface(&surface);
                        let _ = backend::destroy_queue(&queue);
                        cleanup(&device);
                        return Err(e);
                    }
                }
            }
        }
    }
    match allocate_target(&device, configuration) {
        Ok(target) => {
            let attachments = scene_attachment_pool::create(&device, configuration, &samples);
            let mut pipeline_lookup: Vec<Option<usize>> = vec![None; (pipeline_variants_per_sample * 4) as usize];
            for (index, variant) in pipelines.iter().enumerate() {
                let slot = pipeline_slot(variant.family, variant.blend, variant.samples);
                if slot >= 0 {
                    pipeline_lookup[slot as usize] = Some(index);
                }
            }
            let retired = Rc::new(RefCell::new(retired::default()));
            Ok(t {
                device,
                gpu_vertices: gpu_vertices.unwrap_or_else(create_gpu_vertices_owner),
                owns_device,
                queue,
                surface,
                target,
                configuration: configuration.clone(),
                attachments,
                pipelines,
                pipeline_lookup,
                cache: mesh_table(&retired),
                frame: 0,
                // One page: settle waits for the previous frame before the next prepare, so a ring
                // of three never overlaps (memory plan S6).
                uniform_pages: vec![None],
                next_uniform_page: 0,
                uniform_low: 0,
                previous_uniforms: Vec::new(),
                // the meshes' bytes: rays.native_stage holds them
                prepared_cache: side_lru::create(lru::cache("renderer.prepared"), false, cache_byte_capacity, Box::new(|_, _| {}), 64),
                prepared_submission: None,
                automatic_submission: None,
                automatic_candidate: None,
                prepared_scratch: prepared_scratch { scratch_slots: Vec::new(), scratch_length: 0 },
                auxiliary_cache: auxiliary_table(&retired),
                texture_cache: texture_table(&retired),
                retired,
                texture_upload_scratch: None,
                staged_frame: -1,
                staged_stream: false,
                samplers: HashMap::new(),
                arguments: None,
                icb_stats: icb_stats {
                    icb_builds: 0,
                    icb_hits: 0,
                    icb_misses: 0,
                    icb_evictions: 0,
                    icb_executions: 0,
                    icb_failures: 0,
                    icb_last_failure: None,
                },
                uploaded: 0,
                swept_unregistered: 0,
                sun_factory: sun_depth,
                sun: None,
                sun_passes: 0,
                in_flight: None,
                dead: false,
            })
        }
        Err(e) => {
            for variant in &pipelines {
                let _ = backend::destroy_pipeline(&variant.pipeline);
            }
            destroy_surface(&surface);
            let _ = backend::destroy_queue(&queue);
            cleanup(&device);
            Err(e)
        }
    }
}

/// A 128-bit content mix standing in for OCaml's MD5 `Digest`: it only decides whether two byte
/// strings under one cache key are equal, so no digest value is ever compared across runs.
fn digest_hex(data: &[u8]) -> String {
    let (mut a, mut b) = (0xcbf29ce484222325u64, 0x84222325cbf29ce4u64);
    for &x in data {
        a ^= x as u64;
        a = a.wrapping_mul(0x100000001b3);
        b = b.rotate_left(5) ^ (x as u64);
        b = b.wrapping_mul(0x9E3779B97F4A7C15);
    }
    format!("{:016x}{:016x}", a, b)
}

fn source_for_trust(mesh: &mesh, bytes: i64) -> Option<mesh> {
    // Keep extra CPU retention within the already bounded GPU payload budget, even when an indexed
    // mesh contains a large unused vertex plane.
    let (v, i) = (mesh.vertex_bytes_len() as i64, blen(&mesh.indices) as i64);
    if i <= bytes && v <= bytes - i { Some(mesh.clone()) } else { None }
}

fn same_bytes(mesh: &mesh, source: &Option<mesh>) -> bool {
    match source {
        Some(source) => {
            (match (&source.shared_vertices, &mesh.shared_vertices) {
                (Some(a), Some(b)) => Arc::ptr_eq(a,b),
                (None, None) => Rc::ptr_eq(&source.vertices,&mesh.vertices),
                _ => false,
            })
                && Rc::ptr_eq(&source.indices, &mesh.indices)
                && source.vertex_count == mesh.vertex_count
                && source.index_count == mesh.index_count
        }
        None => false,
    }
}

fn vertex_hash(mesh: &mesh) -> String {
    let owned=mesh.vertices.borrow();
    let vertices=mesh.shared_vertices.as_ref().map_or(owned.as_slice(), |bytes| bytes.as_slice());
    digest_hex(vertices)
}

fn content_hash(mesh: &mesh, uniform_bytes: &[u8]) -> String {
    format!("{}:{}:{}", vertex_hash(mesh), digest_hex(&mesh.indices.borrow()), digest_hex(uniform_bytes))
}

fn f32_at(bytes_: &[u8], index: usize) -> f32 {
    f32::from_le_bytes(bytes_[index * 4..index * 4 + 4].try_into().unwrap())
}

fn f64_at(bytes_: &[u8], index: usize) -> f64 {
    f64::from_le_bytes(bytes_[index * 8..index * 8 + 8].try_into().unwrap())
}

fn valid_uniform_bytes(bytes_: &[u8]) -> bool {
    if bytes_.len() == 48 {
        (0..6).all(|index| f64_at(bytes_, index).is_finite())
    } else {
        (bytes_.len() == 24 || bytes_.len() == 208 || bytes_.len() == 5456)
            && (0..bytes_.len() / 4).all(|index| f32_at(bytes_, index).is_finite())
            && {
                let lights = if bytes_.len() == 5456 { f32_at(bytes_, 73) as f64 } else { 0.0 };
                (0.0..=64.0).contains(&lights) && lights.fract() == 0.0
            }
    }
}

fn scratch_mem_mesh(scratch: &prepared_scratch, item: &cached_ref) -> bool {
    scratch.scratch_slots[..scratch.scratch_length]
        .iter()
        .any(|slot| slot.slot_mesh.as_ref().is_some_and(|candidate| Rc::ptr_eq(candidate, item)))
}

/// One entry per key. A hit needs no hashing: vertex-stable keys carry their identity, other draws
/// keep the exact source bytes they were uploaded from and compare them physically. Content hashing
/// happens once per upload and only decides a same-key miss for fresh but equal bytes.
fn prepare(value: &mut t, uniforms: Option<&bytes>, vertex_stable: bool, mesh: &mesh) -> Result<cached_ref, Error> {
    let uniform_bytes: Vec<u8> = uniforms.map(|b| b.borrow().clone()).unwrap_or_default();
    let valid_uniforms = uniforms.is_none_or(|bytes_| valid_uniform_bytes(&bytes_.borrow()));
    let key = mesh_key_string(&mesh.key);
    // a borrowed stream that was unregistered can never be named again: its cached copies go,
    // rather than waiting for the table's byte or entry limit
    let unregistered = value.gpu_vertices.borrow().unregistered;
    if value.swept_unregistered != unregistered {
        value.swept_unregistered = unregistered;
        let prefix = "gpu:vertices:";
        let mut stale: Vec<String> = Vec::new();
        {
            let owner = value.gpu_vertices.borrow();
            side_lru::iter(&mut value.cache, &mut |cached_key, _| {
                if let TableKey::Str(cached_key) = cached_key {
                    if let Some(name) = cached_key.strip_prefix(prefix) {
                        if !owner.gpu_vertex_registry.contains_key(name) {
                            stale.push(cached_key.clone());
                        }
                    }
                }
            });
        }
        for key in stale {
            side_lru::remove(&mut value.cache, &skey(&key));
        }
    }
    let found = side_lru::find(&mut value.cache, &skey(&key)).map(|item| cached_of(&item));
    match found {
        None => prepare_upload(value, uniforms, &uniform_bytes, valid_uniforms, vertex_stable, mesh, &key, None),
        Some(item) if item.borrow().primitive != mesh.primitive => {
            prepare_upload(value, uniforms, &uniform_bytes, valid_uniforms, vertex_stable, mesh, &key, Some(item))
        }
        Some(item) if vertex_stable => {
            let (equal, offset) = {
                let b = item.borrow();
                (uniform_bytes.is_empty() || uniform_bytes == b.uniform_copy, b.uniform_offset)
            };
            if equal {
                return Ok(item);
            }
            match offset {
                None => Ok(item),
                Some(offset) => {
                    backend::write_buffer(&item.borrow().buffer, offset, &uniform_bytes)?;
                    item.borrow_mut().uniform_copy[..uniform_bytes.len()].copy_from_slice(&uniform_bytes);
                    value.uploaded += uniform_bytes.len() as i64;
                    Ok(item)
                }
            }
        }
        Some(item) if same_bytes(mesh, &item.borrow().trusted_source) && uniform_bytes == item.borrow().uniform_copy => Ok(item),
        Some(item) => {
            let hash = item.borrow().payload_hash.clone();
            if !hash.is_empty() && hash == content_hash(mesh, &uniform_bytes) {
                let size = item.borrow().bytes;
                item.borrow_mut().trusted_source = source_for_trust(mesh, size);
                Ok(item)
            } else {
                prepare_upload(value, uniforms, &uniform_bytes, valid_uniforms, vertex_stable, mesh, &key, Some(item))
            }
        }
    }
}

fn prepare_upload(
    value: &mut t,
    uniforms: Option<&bytes>,
    uniform_bytes: &[u8],
    valid_uniforms: bool,
    vertex_stable: bool,
    mesh: &mesh,
    key: &str,
    existing: Option<cached_ref>,
) -> Result<cached_ref, Error> {
    let owned_vertices = mesh.vertices.borrow();
    let vertices = mesh.shared_vertices.as_ref().map_or(owned_vertices.as_slice(), |bytes| bytes.as_slice());
    let indices = mesh.indices.borrow();
    let (vertex_count, index_count) = (mesh.vertex_count, mesh.index_count);
    let total = (vertices.len() + indices.len() + uniform_bytes.len()) as i64;
    if mesh_key_string(&mesh.key).is_empty()
        || vertex_count <= 0
        || index_count <= 0
        || (total == 0 && matches!(mesh.key, mesh_key::Host(_)))
        || !valid_uniforms
    {
        return Err(failure_error("Scene_execution.prepare", ErrorKind::InvalidArgument, "mesh payload or transform uniforms are malformed"));
    }
    let payload_hash = if vertex_stable { String::new() } else { content_hash(mesh, uniform_bytes) };
    let uniform_offset = uniforms.map(|_| (vertices.len() + indices.len()) as i64);
    let uniform_copy: Vec<u8> = uniform_bytes.to_vec();
    let reserved = |item: &cached_ref| scratch_mem_mesh(&value.prepared_scratch, item);
    let reusable: Option<cached_ref> = match &existing {
        Some(item) if item.borrow().bytes == total && !reserved(item) => Some(item.clone()),
        _ if side_lru::length(&mut value.cache) >= mesh_cache_entry_capacity => {
            let scratch = &value.prepared_scratch;
            side_lru::find_first(&mut value.cache, &|_, x| {
                let x = cached_of(x);
                let fits = x.borrow().bytes == total;
                fits && !scratch_mem_mesh(scratch, &x)
            })
            .map(|item| cached_of(&item))
        }
        _ => None,
    };
    match reusable {
        Some(item) => {
            let mut packed = Vec::with_capacity(total as usize);
            packed.extend_from_slice(&vertices);
            packed.extend_from_slice(&indices);
            packed.extend_from_slice(uniform_bytes);
            backend::write_buffer(&item.borrow().buffer, 0, &packed)?;
            let (old_key, size) = {
                let b = item.borrow();
                (b.key.clone(), b.bytes)
            };
            let stolen = old_key != key;
            if stolen {
                side_lru::take(&mut value.cache, &skey(&old_key));
            }
            {
                let mut b = item.borrow_mut();
                b.key = key.to_owned();
                b.payload_hash = payload_hash;
                b.trusted_source = source_for_trust(mesh, size);
                b.index_offset = vertices.len() as i64;
                b.uniform_offset = uniform_offset;
                b.uniform_copy = uniform_copy;
                b.vertex_count = vertex_count;
                b.index_count = index_count;
                b.primitive = mesh.primitive;
            }
            if stolen {
                let item_any: Item = item.clone();
                side_lru::add(&mut value.cache, size, skey(key), item_any);
            }
            value.uploaded += total;
            Ok(item)
        }
        None => {
            let descriptor = BufferDescriptor {
                label: Some(format!("scene-mesh-{}", mesh_key_string(&mesh.key))),
                size: total.max(4),
                usage: vec![BufferUsage::Vertex, BufferUsage::Index, BufferUsage::Storage, BufferUsage::CopyDst],
            };
            let buffer = backend::create_buffer(&value.device, descriptor, None)?;
            let offset = vertices.len() as i64;
            // Indices and uniforms are written apart: joining them first copied every index.
            let write = |offset: i64, data: &[u8]| -> Result<(), Error> {
                backend::write_buffer(&buffer, offset, data).inspect_err(|_| {
                    let _ = backend::destroy_buffer(&buffer);
                })
            };
            write(0, &vertices)?;
            write(offset, &indices)?;
            if !uniform_bytes.is_empty() {
                write(offset + indices.len() as i64, uniform_bytes)?;
            }
            let item: cached_ref = Rc::new(RefCell::new(cached {
                key: key.to_owned(),
                payload_hash,
                trusted_source: source_for_trust(mesh, total),
                buffer,
                index_offset: offset,
                uniform_offset,
                uniform_copy,
                vertex_count,
                index_count,
                primitive: mesh.primitive,
                bytes: total,
            }));
            let item_any: Item = item.clone();
            side_lru::add(&mut value.cache, total, skey(key), item_any);
            value.uploaded += total;
            Ok(item)
        }
    }
}

const uniform_page_byte_capacity: i64 = 256 * 1024 * 1024;

fn align256(value: i64) -> i64 {
    (value + 255) & !255
}

/// A Scene2 mesh's constants (marks.rays, geometry-vertex): its affine (24 bytes of floats or 48 of
/// doubles, none the identity) and the colour flag: each vertex carries its colour word
fn geometry_constants(affine: Option<&bytes>) -> bytes {
    let mut constants = vec![0u8; 32];
    let mut put = |index: usize, value: f32| constants[index * 4..index * 4 + 4].copy_from_slice(&value.to_le_bytes());
    match affine {
        None => {
            put(0, 1.0);
            put(4, 1.0);
        }
        Some(bytes_) if blen(bytes_) == 24 => {
            let b = bytes_.borrow();
            for index in 0..6 {
                put(index, f32_at(&b, index));
            }
        }
        Some(bytes_) => {
            let b = bytes_.borrow();
            for index in 0..6 {
                put(index, f64_at(&b, index) as f32);
            }
        }
    }
    put(6, 1.0);
    new_bytes(constants)
}

// ponytail: a changed transform repacks the frame; use dirty ranges only if sparse-change
// profiles justify tracking them beside P3-2 scene identities.
fn uniform_owner() -> &'static mem::Owner {
    static OWNER: OnceLock<mem::Owner> = OnceLock::new();
    OWNER.get_or_init(|| mem::plan_in(mem::Pool::Device, "renderer.uniform_page"))
}

const uniform_shrink_after_frames: i64 = 240;

/// Deferred destruction: valid through the synchronous submission that evicted it.
enum deferred {
    Buffer(Buffer),
    Texture(Texture),
}

fn prepare_uniforms(value: &mut t, defer: &mut Vec<deferred>, sources: &[Option<bytes>]) -> Result<Vec<Option<uniform_slice>>, Error> {
    let operation = "Scene_execution.prepare_uniforms";
    // one page per plan S6: a frame's needs far under it for long enough bring it back down, also
    // when the uniforms stop changing
    let capacity = value.uniform_pages[value.next_uniform_page].as_ref().map_or(0, |page| page.page_capacity);
    let wanted: i64 = sources.iter().map(|s| s.as_ref().map_or(0, |b| align256(blen(b) as i64))).sum();
    value.uniform_low = if capacity > 65536 && wanted * 4 < capacity { value.uniform_low + 1 } else { 0 };
    let shrink = value.uniform_low >= uniform_shrink_after_frames;
    if shrink {
        value.uniform_low = 0;
    }
    let same = !shrink
        && sources.len() == value.previous_uniforms.len()
        && sources.iter().zip(value.previous_uniforms.iter()).all(|(source, retained)| match (source, retained) {
            (None, None) => true,
            (Some(bytes_), Some(slice)) => {
                let b = bytes_.borrow();
                b.len() == slice.uniform_length
                    && b[..] == slice.uniform_staging[slice.uniform_staging_offset..slice.uniform_staging_offset + slice.uniform_length]
            }
            _ => false,
        });
    // Scene3 blocks (5456 bytes plus 192 per instance) bypass [prepare]'s payload check, so reject
    // non-finite values and light counts outside 0..64 here, before anything is written; unchanged
    // frames skip this.
    let malformed_scene3 = || {
        sources.iter().any(|source| match source {
            Some(bytes_) if blen(bytes_) >= 5456 => {
                let b = bytes_.borrow();
                let finite = (0..b.len() / 4).all(|index| f32_at(&b, index).is_finite());
                let lights = f32_at(&b, 73) as f64;
                !(finite && (0.0..=64.0).contains(&lights) && lights.fract() == 0.0)
            }
            _ => false,
        })
    };
    if same {
        return Ok(value.previous_uniforms.clone());
    }
    if malformed_scene3() {
        return Err(failure_error(operation, ErrorKind::InvalidArgument, "Scene3 transform uniforms are malformed"));
    }
    drop_plans(value);
    value.previous_uniforms = Vec::new();
    let mut needed: i64 = 0;
    let mut valid = true;
    for source in sources.iter().flatten() {
        let length = blen(source) as i64;
        if length > uniform_page_byte_capacity {
            valid = false;
        } else {
            let aligned = align256(length);
            if needed > uniform_page_byte_capacity - aligned {
                valid = false;
            } else {
                needed += aligned;
            }
        }
    }
    if !valid {
        return Err(failure_error(operation, ErrorKind::Capacity, "one submission exceeds the bounded transform-uniform bytes"));
    }
    if needed == 0 {
        return Ok(vec![None; sources.len()]);
    }
    let page_index = value.next_uniform_page;
    let reuse = matches!(&value.uniform_pages[page_index], Some(page) if page.page_capacity >= needed && !shrink);
    if !reuse {
        let mut capacity: i64 = 4096;
        while capacity < needed {
            capacity = uniform_page_byte_capacity.min(capacity * 2);
        }
        let descriptor = BufferDescriptor {
            label: Some("scene-transform-ring".to_owned()),
            size: capacity,
            usage: vec![BufferUsage::Storage, BufferUsage::CopyDst],
        };
        let buffer = backend::create_buffer(&value.device, descriptor, None)?;
        if let Some(old) = value.uniform_pages[page_index].take() {
            mem::release(uniform_owner(), old.page_capacity);
            defer.push(deferred::Buffer(old.page_buffer));
        }
        mem::charge(uniform_owner(), capacity);
        value.uniform_pages[page_index] = Some(uniform_page { page_buffer: buffer, page_capacity: capacity });
    }
    let page_buffer = value.uniform_pages[page_index].as_ref().expect("uniform page").page_buffer.clone();
    value.next_uniform_page = (page_index + 1) % value.uniform_pages.len();
    // Pack every slice into one staging block and upload it once; the page is a bump allocator, not
    // a list of per-draw buffers.
    let mut staging = vec![0u8; needed as usize];
    let mut cursor: i64 = 0;
    let mut payload: i64 = 0;
    let mut offsets: Vec<Option<(i64, usize)>> = vec![None; sources.len()];
    for (index, source) in sources.iter().enumerate() {
        if let Some(bytes_) = source {
            let b = bytes_.borrow();
            staging[cursor as usize..cursor as usize + b.len()].copy_from_slice(&b);
            offsets[index] = Some((cursor, b.len()));
            cursor += align256(b.len() as i64);
            payload += b.len() as i64;
        }
    }
    backend::write_buffer(&page_buffer, 0, &staging)?;
    value.uploaded += payload;
    let staging = Rc::new(staging);
    Ok(offsets
        .into_iter()
        .map(|entry| {
            entry.map(|(offset, length)| uniform_slice {
                uniform_buffer: page_buffer.clone(),
                uniform_offset: offset,
                uniform_staging: staging.clone(),
                uniform_staging_offset: offset as usize,
                uniform_length: length,
            })
        })
        .collect())
}

/// The texel format follows from level 0's byte count: 4, 8 or 16 bytes per texel are
/// `Rgba8_unorm`, `Rgba16_float` and `Rgba32_float`.
fn texel_bytes(source: &sampled_texture) -> i64 {
    let level = &source.levels[0];
    if level.width <= 0 || level.height <= 0 { 0 } else { blen(&level.bytes) as i64 / (level.width * level.height) }
}

fn texel_format(texel: i64) -> TextureFormat {
    match texel {
        8 => TextureFormat::Rgba16Float,
        16 => TextureFormat::Rgba32Float,
        _ => TextureFormat::Rgba8Unorm,
    }
}

fn valid_texture(source: &sampled_texture) -> bool {
    !source.key.is_empty()
        && !source.levels.is_empty()
        && ogpu::validation::validate_sampler(&source.sampler).is_ok()
        && match &source.gpu {
            Some(texture) => {
                let descriptor = backend::texture_descriptor(texture);
                !backend::texture_destroyed(texture)
                    && source.levels.len() == 1
                    && source.levels[0].width == descriptor.width
                    && source.levels[0].height == descriptor.height
                    && descriptor.usage.contains(&TextureUsage::TextureBinding)
                    && blen(&source.levels[0].bytes) == 0
            }
            None => {
                let texel = texel_bytes(source);
                [4, 8, 16].contains(&texel)
                    && source.levels.iter().enumerate().all(|(index, level)| {
                        level.width == (source.levels[0].width >> index).max(1)
                            && level.height == (source.levels[0].height >> index).max(1)
                            && blen(&level.bytes) as i64 == level.width * level.height * texel
                    })
            }
        }
}

fn texture_upload_scratch_for(value: &mut t, total: i64) -> Result<(), Error> {
    if value.staged_frame != value.frame {
        value.staged_stream = value.staged_frame == value.frame - 1;
        value.staged_frame = value.frame;
    }
    if matches!(&value.texture_upload_scratch, Some(scratch) if scratch.scratch_size == total) {
        return Ok(());
    }
    let descriptor = BufferDescriptor { label: Some("scene-texture-staging".to_owned()), size: total, usage: vec![BufferUsage::CopySrc] };
    let scratch_buffer = backend::create_buffer(&value.device, descriptor, None)?;
    let previous = value.texture_upload_scratch.replace(texture_upload_scratch {
        scratch_buffer,
        scratch_bytes: vec![0u8; total as usize],
        scratch_size: total,
    });
    if let Some(old) = previous {
        let _ = backend::destroy_buffer(&old.scratch_buffer);
    }
    Ok(())
}

/// Uploads complete inside their frame, so what they staged through goes at its end: an image
/// uploads once, and two copies of it should not outlive that. A canvas that uploads frame after
/// frame keeps them, or each frame would pay for a new pair.
fn release_texture_staging(value: &mut t) {
    if value.texture_upload_scratch.is_some() && !(value.staged_stream && value.staged_frame == value.frame) {
        if let Some(scratch) = value.texture_upload_scratch.take() {
            let _ = backend::destroy_buffer(&scratch.scratch_buffer);
        }
    }
}

/// Destroys the texture staging a run of uploading frames keeps for the next one; the next upload
/// makes it again.
pub fn release_staging(value: &mut t) {
    value.staged_stream = false;
    release_texture_staging(value);
}

fn image_or_canvas_key(key: &str) -> bool {
    ["canvas:", "image:", "texture:", "text:", "world:"].iter().any(|prefix| key.starts_with(prefix))
}

fn levels_hash(source: &sampled_texture) -> String {
    let parts: Vec<String> = source
        .levels
        .iter()
        .map(|level| format!("{}x{}:{}", level.width, level.height, digest_hex(&level.bytes.borrow())))
        .collect();
    digest_hex(parts.join("|").as_bytes())
}

fn prepare_texture(value: &mut t, defer: &mut Vec<deferred>, source: &sampled_texture) -> Result<cached_texture_ref, Error> {
    if let Some(texture) = &source.gpu {
        let (device_id, _) = backend::texture_driver_token(texture);
        if !valid_texture(source) {
            return Err(failure_error("Scene_execution.prepare_texture", ErrorKind::InvalidArgument, "GPU image is malformed or destroyed"));
        }
        if device_id != ogpu::handle::device_id(backend::device_handle(&value.device)) {
            return Err(failure_error("Scene_execution.prepare_texture", ErrorKind::CrossDevice, "GPU image belongs to another renderer"));
        }
        return Ok(Rc::new(RefCell::new(cached_texture {
            texture_key: source.key.clone(),
            texture_hash: String::new(),
            texture_shape: String::new(),
            texture: texture.clone(),
            texture_bytes: 0,
            texture_used: value.frame,
            texture_levels: Rc::new(Vec::new()),
        })));
    }
    let found = side_lru::find(&mut value.texture_cache, &skey(&source.key)).map(|item| cached_texture_of(&item));
    let key = source.key.as_str();
    match found {
        Some(item)
            if ["image:", "texture:", "shadow:", "text:", "world:"].iter().any(|prefix| key.starts_with(prefix)) =>
        {
            item.borrow_mut().texture_used = value.frame;
            Ok(item)
        }
        Some(item) if key.starts_with("canvas:") => upload_texture(value, defer, source, String::new(), Some(item)),
        // The immutable levels this slot was uploaded from: no hashing.
        Some(item) if Rc::ptr_eq(&item.borrow().texture_levels, &source.levels) => {
            item.borrow_mut().texture_used = value.frame;
            Ok(item)
        }
        Some(item) => {
            let hash = levels_hash(source);
            if item.borrow().texture_hash == hash {
                let mut b = item.borrow_mut();
                b.texture_levels = source.levels.clone();
                b.texture_used = value.frame;
                drop(b);
                Ok(item)
            } else {
                upload_texture(value, defer, source, hash, None)
            }
        }
        // Image, canvas, texture and text keys carry identity and generation, so their bytes are
        // never hashed; other keys are content-addressed.
        None => {
            let hash = if image_or_canvas_key(key) { String::new() } else { levels_hash(source) };
            upload_texture(value, defer, source, hash, None)
        }
    }
}

fn upload_texture(
    value: &mut t,
    defer: &mut Vec<deferred>,
    source: &sampled_texture,
    hash: String,
    reusable: Option<cached_texture_ref>,
) -> Result<cached_texture_ref, Error> {
    if !valid_texture(source) {
        return Err(failure_error("Scene_execution.prepare_texture", ErrorKind::InvalidArgument, "texture or sampler is malformed"));
    }
    let texel = texel_bytes(source);
    let shape = format!(
        "{}{}",
        if texel == 4 { String::new() } else { format!("{}B:", texel) },
        source.levels.iter().map(|level| format!("{}x{}", level.width, level.height)).collect::<Vec<_>>().join("/")
    );
    // Canvas and managed-image identities are unique and lower to one authoritative generation per
    // staged frame, so their same-shape storage can be updated safely between completed
    // submissions. A replacement image of the same shape may steal an unused cached texture; two
    // same-shape images in one submission keep distinct textures because an in-use slot cannot be
    // stolen.
    let reusable = match reusable {
        Some(item) => Some(item),
        None if !image_or_canvas_key(&source.key) => None,
        None => {
            let frame = value.frame;
            side_lru::find_first(&mut value.texture_cache, &|_, item| {
                let item = cached_texture_of(item);
                let b = item.borrow();
                b.texture_shape == shape && image_or_canvas_key(&b.texture_key) && b.texture_used != frame
            })
            .map(|item| cached_texture_of(&item))
        }
    };
    let descriptor = TextureDescriptor {
        label: Some(format!("scene-texture-{}", source.key)),
        width: source.levels[0].width,
        height: source.levels[0].height,
        depth: 1,
        mip_levels: source.levels.len() as i64,
        sample_count: 1,
        format: texel_format(texel),
        usage: vec![TextureUsage::TextureBinding, TextureUsage::TextureCopyDst],
    };
    let rows: Vec<i64> = source.levels.iter().map(|level| align256(level.width * texel)).collect();
    let mut offsets = vec![0i64; source.levels.len()];
    for index in 1..offsets.len() {
        offsets[index] = offsets[index - 1] + rows[index - 1] * source.levels[index - 1].height;
    }
    let last = offsets.len() - 1;
    let total = offsets[last] + rows[last] * source.levels[last].height;
    let cacheable = total <= texture_cache_byte_capacity;
    let (texture, created) = match &reusable {
        Some(item) if cacheable => (item.borrow().texture.clone(), false),
        _ => (backend::create_texture(&value.device, descriptor)?, true),
    };
    let destroy_created = |texture: &Texture| {
        if created {
            let _ = backend::destroy_texture(texture);
        }
    };
    if let Err(e) = texture_upload_scratch_for(value, total) {
        destroy_created(&texture);
        return Err(e);
    }
    let staging;
    {
        let scratch = value.texture_upload_scratch.as_mut().expect("texture staging");
        staging = scratch.scratch_buffer.clone();
        let packed = &mut scratch.scratch_bytes;
        for (level_index, level) in source.levels.iter().enumerate() {
            let pixels = level.bytes.borrow();
            let row_bytes = (level.width * texel) as usize;
            for row in 0..level.height as usize {
                let to = offsets[level_index] as usize + row * rows[level_index] as usize;
                packed[to..to + row_bytes].copy_from_slice(&pixels[row * row_bytes..(row + 1) * row_bytes]);
            }
        }
    }
    let written = {
        let scratch = value.texture_upload_scratch.as_ref().expect("texture staging");
        backend::write_buffer(&staging, 0, &scratch.scratch_bytes)
    };
    if let Err(e) = written {
        destroy_created(&texture);
        return Err(e);
    }
    let uploaded = (|| -> Result<(), Error> {
        let commands = backend::begin_commands(&value.queue)?;
        let encoded = (|| -> Result<backend::Receipt, Error> {
            let blit = backend::blit_encoder(&commands)?;
            for (index, level) in source.levels.iter().enumerate() {
                backend::buffer_to_texture(
                    &blit,
                    &staging,
                    Some(offsets[index]),
                    rows[index],
                    rows[index] * level.height,
                    &texture,
                    Some(index as i64),
                    None,
                    Extent { width: level.width, height: level.height, depth: 1 },
                )?;
            }
            backend::end_blit(&blit)?;
            backend::commit(&commands)
        })();
        match encoded {
            Err(e) => {
                let _ = backend::abandon(&commands);
                Err(e)
            }
            Ok(receipt) => backend::complete_through(&value.queue, receipt.epoch),
        }
    })();
    if let Err(e) = uploaded {
        destroy_created(&texture);
        return Err(e);
    }
    value.uploaded += total;
    match reusable {
        Some(item) if backend::texture_id(&item.borrow().texture) == backend::texture_id(&texture) => {
            let old_key = item.borrow().texture_key.clone();
            if old_key != source.key {
                side_lru::take(&mut value.texture_cache, &skey(&old_key));
                let bytes_ = item.borrow().texture_bytes;
                item.borrow_mut().texture_key = source.key.clone();
                let item_any: Item = item.clone();
                side_lru::add(&mut value.texture_cache, bytes_, skey(&source.key), item_any);
            }
            let mut b = item.borrow_mut();
            b.texture_hash = hash;
            b.texture_levels = source.levels.clone();
            b.texture_used = value.frame;
            drop(b);
            Ok(item)
        }
        _ => {
            let item: cached_texture_ref = Rc::new(RefCell::new(cached_texture {
                texture_key: source.key.clone(),
                texture_hash: hash,
                texture_shape: shape,
                texture: texture.clone(),
                texture_bytes: total,
                texture_used: value.frame,
                texture_levels: source.levels.clone(),
            }));
            if !cacheable {
                defer.push(deferred::Texture(texture));
            } else {
                let item_any: Item = item.clone();
                side_lru::add(&mut value.texture_cache, total, skey(&source.key), item_any);
            }
            Ok(item)
        }
    }
}

fn prepare_storage(value: &mut t, key: &str, bytes_: &bytes) -> Result<cached_auxiliary_ref, Error> {
    let found = side_lru::find(&mut value.auxiliary_cache, &skey(key)).map(|item| cached_auxiliary_of(&item));
    if let Some(item) = found {
        if Rc::ptr_eq(&item.borrow().auxiliary_source, bytes_) {
            return Ok(item);
        }
        if item.borrow().auxiliary_copy == *bytes_.borrow() {
            item.borrow_mut().auxiliary_source = bytes_.clone();
            return Ok(item);
        }
    }
    let data = bytes_.borrow();
    let descriptor = BufferDescriptor {
        label: Some(format!("scene-auxiliary-{}", key)),
        size: data.len() as i64,
        usage: vec![BufferUsage::Storage, BufferUsage::CopyDst],
    };
    let buffer = backend::create_buffer(&value.device, descriptor, None)?;
    if let Err(e) = backend::write_buffer(&buffer, 0, &data) {
        let _ = backend::destroy_buffer(&buffer);
        return Err(e);
    }
    let item: cached_auxiliary_ref = Rc::new(RefCell::new(cached_auxiliary {
        auxiliary_copy: data.clone(),
        auxiliary_buffer: buffer,
        auxiliary_source: bytes_.clone(),
    }));
    let item_any: Item = item.clone();
    side_lru::add(&mut value.auxiliary_cache, data.len() as i64, skey(key), item_any);
    value.uploaded += data.len() as i64;
    Ok(item)
}

fn prepare_auxiliary(value: &mut t, source: &auxiliary_resource) -> Result<cached_auxiliary_ref, Error> {
    prepare_storage(value, &source.key, &source.buffer)
}

fn u32_le(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap())
}

fn primitive_code(primitive: Primitive) -> &'static str {
    match primitive {
        Primitive::PointList => "P",
        Primitive::LineList => "L",
        Primitive::TriangleList => "T",
        Primitive::TriangleStrip => "S",
    }
}

fn mesh_identity(mesh: &mesh) -> String {
    format!(
        "{}:{}:{}:{}",
        mesh_key_string(&mesh.key),
        primitive_code(mesh.primitive),
        vertex_hash(mesh),
        digest_hex(&mesh.indices.borrow())
    )
}

fn combine_meshes(meshes: &[&mesh]) -> Result<mesh, Error> {
    let operation = "Scene_execution.combine_meshes";
    let valid_indices = |mesh: &mesh| {
        let indices = mesh.indices.borrow();
        (0..mesh.index_count as usize).all(|index| {
            let value = u32_le(&indices, index * 4) as i32 as i64;
            value >= 0 && value < mesh.vertex_count
        })
    };
    let (mut vertex_bytes, mut vertex_count, mut index_count) = (0i64, 0i64, 0i64);
    let mut identities: Vec<String> = Vec::new();
    for mesh in meshes {
        let vlen = mesh.vertex_bytes_len() as i64;
        if mesh.vertex_count <= 0
            || vlen % mesh.vertex_count != 0
            || !valid_index_length(mesh)
            || blen(&mesh.indices) as i64 != mesh.index_count * 4
            || !valid_indices(mesh)
        {
            return Err(failure_error(operation, ErrorKind::InvalidArgument, "packed mesh cardinality is inconsistent"));
        }
        if vertex_bytes > MAX_STRING_LENGTH - vlen
            || vertex_count > i32::MAX as i64 - mesh.vertex_count
            || index_count > MAX_STRING_LENGTH / 4 - mesh.index_count
        {
            return Err(failure_error(operation, ErrorKind::Capacity, "combined mesh exceeds packed representation limits"));
        }
        vertex_bytes += vlen;
        vertex_count += mesh.vertex_count;
        index_count += mesh.index_count;
        identities.push(mesh_identity(mesh));
    }
    let mut vertices = vec![0u8; vertex_bytes as usize];
    let mut indices = vec![0u8; (index_count * 4) as usize];
    let (mut vertex_offset, mut vertex_base, mut index_offset) = (0usize, 0i64, 0usize);
    for mesh in meshes {
        let owned = mesh.vertices.borrow();
        let source = mesh.shared_vertices.as_ref().map_or(owned.as_slice(), |bytes| bytes.as_slice());
        vertices[vertex_offset..vertex_offset + source.len()].copy_from_slice(source);
        let source_indices = mesh.indices.borrow();
        for index in 0..mesh.index_count as usize {
            let value = u32_le(&source_indices, index * 4) as i32 as i64;
            set_u32_le(&mut indices, index_offset + index * 4, (value + vertex_base) as i32 as u32);
        }
        vertex_offset += source.len();
        vertex_base += mesh.vertex_count;
        index_offset += mesh.index_count as usize * 4;
    }
    let identity = digest_hex(identities.join("|").as_bytes());
    Ok(mesh {
        key: mesh_key::Host(format!("batch:{}", identity)),
        vertices: new_bytes(vertices),
        shared_vertices: None,
        vertex_count,
        indices: new_bytes(indices),
        index_count,
        primitive: meshes[0].primitive,
    })
}

fn same_optional_texture(a: &Option<Rc<sampled_texture>>, b: &Option<Rc<sampled_texture>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn same_optional_auxiliary(a: &Option<Rc<auxiliary_resource>>, b: &Option<Rc<auxiliary_resource>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn attribute_bytes(entry: &sampled_draw) -> i64 {
    entry.vertex_attributes.as_ref().map_or(0, |(_, b)| blen(b) as i64)
}

fn draw_bytes(entry: &sampled_draw) -> i64 {
    (entry.draw.mesh.vertex_bytes_len() + blen(&entry.draw.mesh.indices)) as i64 + attribute_bytes(entry)
}

fn coalesce_draws(draws: &[draw]) -> Result<Vec<draw>, Error> {
    let stride = |mesh: &mesh| if mesh.vertex_count == 0 { 0 } else { mesh.vertex_bytes_len() as i64 / mesh.vertex_count };
    let compatible = |first: &sampled_draw, next: &sampled_draw| {
        matches!((&first.draw.mesh.key, &next.draw.mesh.key), (mesh_key::Host(_), mesh_key::Host(_)))
            && first.family == next.family
            && first.blend == next.blend
            && first.samples == next.samples
            && first.draw.state == next.draw.state
            && first.draw.mesh.primitive == next.draw.mesh.primitive
            && stride(&first.draw.mesh) == stride(&next.draw.mesh)
            && same_optional_texture(&first.texture, &next.texture)
            && same_optional_auxiliary(&first.auxiliary, &next.auxiliary)
            && first.vertex_attributes.is_some() == next.vertex_attributes.is_some()
    };
    let mut result: Vec<draw> = Vec::new();
    let mut position = 0;
    while position < draws.len() {
        let first = &draws[position];
        let first_bytes = draw_bytes(first);
        if first_bytes > cache_byte_capacity {
            return Err(failure_error("Scene_execution.coalesce", ErrorKind::Capacity, "one mesh exceeds the batch byte capacity"));
        }
        let mut packed_bytes = first_bytes;
        let mut end = position + 1;
        while end < draws.len() && compatible(first, &draws[end]) {
            let bytes_ = draw_bytes(&draws[end]);
            if packed_bytes <= cache_byte_capacity - bytes_ {
                packed_bytes += bytes_;
                end += 1;
            } else {
                break;
            }
        }
        let entries = &draws[position..end];
        position = end;
        // Small runs retain their independent stable identities. Coalescing a changing member into a
        // shared buffer otherwise reuploads every unchanged neighbour. Large runs still coalesce to
        // bound caches and draw payloads.
        if entries.len() <= 64 {
            result.extend(entries.iter().cloned());
        } else {
            let meshes: Vec<&mesh> = entries.iter().map(|entry| &entry.draw.mesh).collect();
            let mesh = combine_meshes(&meshes)?;
            let vertex_attributes = first.vertex_attributes.as_ref().map(|_| {
                let attributes: Vec<&(String, bytes)> = entries.iter().map(|e| e.vertex_attributes.as_ref().expect("attributes")).collect();
                let key = format!(
                    "batch:attributes:{}",
                    digest_hex(attributes.iter().map(|(k, _)| k.as_str()).collect::<Vec<_>>().join("|").as_bytes())
                );
                let mut joined: Vec<u8> = Vec::new();
                for (_, b) in attributes {
                    joined.extend_from_slice(&b.borrow());
                }
                (key, new_bytes(joined))
            });
            let mut merged = (**first).clone();
            merged.vertex_attributes = vertex_attributes;
            merged.draw.mesh = mesh;
            result.push(Rc::new(merged));
        }
    }
    Ok(result)
}

fn prepared_bytes_of(draws: &[draw]) -> i64 {
    draws.iter().map(|entry| draw_bytes(entry)).sum()
}

fn resolve_prepared(value: &mut t, prepared: &Option<(String, i64)>, draws: &[draw]) -> Result<Vec<draw>, Error> {
    match prepared {
        Some((identity, _)) if identity.is_empty() => {
            Err(failure_error("Scene_execution.prepare_run", ErrorKind::InvalidArgument, "prepared identity is empty"))
        }
        Some((identity, version)) => {
            if let Some(item) = side_lru::find(&mut value.prepared_cache, &skey(identity)) {
                let item = prepared_run_of(&item);
                if item.prepared_version == *version {
                    return Ok(item.prepared_draws.clone());
                }
            }
            let prepared_draws = coalesce_draws(draws)?;
            let item = Rc::new(prepared_run {
                prepared_version: *version,
                prepared_bytes: prepared_bytes_of(&prepared_draws),
                prepared_draws: prepared_draws.clone(),
            });
            let bytes_ = item.prepared_bytes;
            side_lru::add(&mut value.prepared_cache, bytes_, skey(identity), item);
            Ok(prepared_draws)
        }
        None => coalesce_draws(draws),
    }
}

fn intersect_extent(bound_w: i64, bound_h: i64, (x, y, w, h): (i64, i64, i64, i64)) -> Option<(i64, i64, i64, i64)> {
    let (x0, y0) = (x.max(0), y.max(0));
    let (x1, y1) = (bound_w.min(x + w), bound_h.min(y + h));
    if x1 <= x0 || y1 <= y0 { None } else { Some((x0, y0, x1 - x0, y1 - y0)) }
}

fn texture_signature(source: &sampled_texture, cached: &cached_texture_ref) -> (i64, SamplerDescriptor) {
    (backend::texture_id(&cached.borrow().texture), source.sampler.clone())
}

fn auxiliary_signature(entry: &(Rc<auxiliary_resource>, cached_auxiliary_ref, cached_texture_ref)) -> (i64, i64, SamplerDescriptor) {
    (
        backend::buffer_id(&entry.1.borrow().auxiliary_buffer),
        backend::texture_id(&entry.2.borrow().texture),
        entry.0.texture.sampler.clone(),
    )
}

/// Submission signatures own affine bytes. Scene values are immutable by contract, but callers may
/// reuse their input buffer after `render` returns; retaining it here would turn later mutation
/// into a false cache hit. Completed ring slices may be replayed when the whole uniform set is
/// unchanged. A changed set gets a different page and offsets.
fn automatic_signature(slot: &prepared_slot) -> automatic_signature {
    let item = slot.slot_mesh.as_ref().expect("slot mesh").borrow();
    let mut signature_state = slot.slot_state.clone().expect("slot state");
    signature_state.transform_uniforms = match &slot.slot_uniform {
        Some(_) => None,
        None => signature_state.transform_uniforms.as_ref().map(|b| new_bytes(b.borrow().clone())),
    };
    automatic_signature {
        signature_family: slot.slot_family,
        signature_blend: slot.slot_blend,
        signature_samples: slot.slot_samples,
        signature_state,
        signature_texture: slot.slot_texture.as_ref().map(|(source, cached)| texture_signature(source, cached)),
        signature_auxiliary: slot.slot_auxiliary.as_ref().map(auxiliary_signature),
        signature_environment: slot.slot_environment.as_ref().map(|(source, cached)| texture_signature(source, cached)),
        signature_buffer: backend::buffer_id(&item.buffer),
        signature_gpu_vertices: slot.slot_gpu_vertices.as_ref().map(gpu_identity),
        signature_vertex_attributes: slot.slot_vertex_attributes.as_ref().map(|a| backend::buffer_id(&a.borrow().auxiliary_buffer)),
        signature_index_offset: item.index_offset,
        signature_uniform_buffer: slot.slot_uniform.as_ref().map(|u| backend::buffer_id(&u.uniform_buffer)),
        signature_uniform_offset: slot.slot_uniform.as_ref().map(|u| u.uniform_offset),
        signature_vertex_count: item.vertex_count,
        signature_index_count: item.index_count,
    }
}

const prepared_scratch_capacity: usize = 65_536;

fn make_prepared_slot() -> prepared_slot {
    prepared_slot {
        slot_family: Scene2,
        slot_blend: Blend::Replace,
        slot_samples: 1,
        slot_state: None,
        slot_texture: None,
        slot_auxiliary: None,
        slot_environment: None,
        slot_mesh: None,
        slot_uniform: None,
        slot_vertex_attributes: None,
        slot_gpu_vertices: None,
    }
}

fn ensure_prepared_scratch(scratch: &mut prepared_scratch, needed: usize) -> Result<(), Error> {
    if needed > prepared_scratch_capacity {
        return Err(failure_error("Renderer.submit", ErrorKind::Capacity, "one submission exceeds the bounded prepared-draw capacity"));
    }
    if needed <= scratch.scratch_slots.len() {
        return Ok(());
    }
    let mut capacity = scratch.scratch_slots.len();
    while capacity < needed {
        capacity = prepared_scratch_capacity.min(16.max(capacity * 2));
    }
    while scratch.scratch_slots.len() < capacity {
        scratch.scratch_slots.push(make_prepared_slot());
    }
    Ok(())
}

fn clear_prepared_scratch(scratch: &mut prepared_scratch) {
    for slot in &mut scratch.scratch_slots[..scratch.scratch_length] {
        slot.slot_state = None;
        slot.slot_texture = None;
        slot.slot_auxiliary = None;
        slot.slot_environment = None;
        slot.slot_gpu_vertices = None;
        slot.slot_mesh = None;
        slot.slot_uniform = None;
        slot.slot_vertex_attributes = None;
    }
    scratch.scratch_length = 0;
}

fn same_automatic_slot(signature: &automatic_signature, slot: &prepared_slot) -> bool {
    let (Some(state), Some(item)) = (&slot.slot_state, &slot.slot_mesh) else { return false };
    let item = item.borrow();
    let left = &signature.signature_state;
    let same_state = left.viewport == state.viewport
        && left.scissor == state.scissor
        && left.cull == state.cull
        && left.depth_compare == state.depth_compare
        && left.depth_write == state.depth_write
        && left.depth_load == state.depth_load
        && left.depth_clear == state.depth_clear
        && (slot.slot_uniform.is_some() || left.transform_uniforms == state.transform_uniforms);
    let same_texture = match (&signature.signature_texture, &slot.slot_texture) {
        (None, None) => true,
        (Some((id, sampler)), Some((source, cached))) => {
            *id == backend::texture_id(&cached.borrow().texture) && *sampler == source.sampler
        }
        _ => false,
    };
    let same_auxiliary = match (&signature.signature_auxiliary, &slot.slot_auxiliary) {
        (None, None) => true,
        (Some((buffer_id, texture_id, sampler)), Some(entry)) => {
            *buffer_id == backend::buffer_id(&entry.1.borrow().auxiliary_buffer)
                && *texture_id == backend::texture_id(&entry.2.borrow().texture)
                && *sampler == entry.0.texture.sampler
        }
        _ => false,
    };
    signature.signature_family == slot.slot_family
        && signature.signature_blend == slot.slot_blend
        && signature.signature_samples == slot.slot_samples
        && signature.signature_environment == slot.slot_environment.as_ref().map(|(source, cached)| texture_signature(source, cached))
        && same_state
        && same_texture
        && same_auxiliary
        && signature.signature_buffer == backend::buffer_id(&item.buffer)
        && signature.signature_gpu_vertices == slot.slot_gpu_vertices.as_ref().map(gpu_identity)
        && signature.signature_vertex_attributes == slot.slot_vertex_attributes.as_ref().map(|a| backend::buffer_id(&a.borrow().auxiliary_buffer))
        && signature.signature_index_offset == item.index_offset
        && signature.signature_uniform_buffer == slot.slot_uniform.as_ref().map(|u| backend::buffer_id(&u.uniform_buffer))
        && signature.signature_uniform_offset == slot.slot_uniform.as_ref().map(|u| u.uniform_offset)
        && signature.signature_vertex_count == item.vertex_count
        && signature.signature_index_count == item.index_count
}

fn same_scratch_payloads(payloads: &[automatic_signature], scratch: &prepared_scratch) -> bool {
    payloads.len() == scratch.scratch_length
        && payloads.iter().zip(scratch.scratch_slots.iter()).all(|(signature, slot)| same_automatic_slot(signature, slot))
}

struct mix(u64);

impl mix {
    fn word(&mut self, value: u64) {
        self.0 = (self.0 ^ value).wrapping_mul(0x9E3779B97F4A7C15).rotate_left(29);
    }
    fn text(&mut self, value: &str) {
        for b in value.bytes() {
            self.word(b as u64);
        }
    }
    fn sampler(&mut self, sampler: &SamplerDescriptor) {
        self.text(sampler.label.as_deref().unwrap_or(""));
        self.word(sampler.min_filter as u64);
        self.word(sampler.mag_filter as u64);
        self.word(sampler.mip_filter as u64);
        self.word(sampler.address_u as u64);
        self.word(sampler.address_v as u64);
        self.word(sampler.lod_min.to_bits());
        self.word(sampler.lod_max.to_bits());
        self.word(sampler.max_anisotropy as u64);
    }
}

/// A compact value that is only an admission hint. Replay still requires `same_scratch_payloads`,
/// so a hash collision cannot submit stale work. (The OCaml hash is `Hashtbl.seeded_hash`; this one
/// mixes the same values, and nothing outside this renderer reads it.)
fn automatic_candidate_fingerprint(scratch: &prepared_scratch) -> i64 {
    let mut h = mix(0x41c64e6d);
    for slot in &scratch.scratch_slots[..scratch.scratch_length] {
        let state = slot.slot_state.as_ref().expect("slot state");
        let mesh = slot.slot_mesh.as_ref().expect("slot mesh").borrow();
        h.word(slot.slot_family as u64);
        h.word(slot.slot_blend as u64);
        h.word(slot.slot_samples as u64);
        for v in [state.viewport, state.scissor] {
            for w in [v.0, v.1, v.2, v.3] {
                h.word(w as u64);
            }
        }
        h.word(state.cull as u64);
        h.word(state.depth_compare as u64);
        h.word(state.depth_write as u64);
        h.word(state.depth_load as u64);
        h.word(state.depth_clear.to_bits());
        if let Some(b) = &state.transform_uniforms {
            for byte in b.borrow().iter() {
                h.word(*byte as u64);
            }
        }
        if let Some((source, cached)) = &slot.slot_texture {
            let (id, sampler) = texture_signature(source, cached);
            h.word(id as u64);
            h.sampler(&sampler);
        }
        if let Some(entry) = &slot.slot_auxiliary {
            let (buffer, texture, sampler) = auxiliary_signature(entry);
            h.word(buffer as u64);
            h.word(texture as u64);
            h.sampler(&sampler);
        }
        if let Some((source, cached)) = &slot.slot_environment {
            let (id, sampler) = texture_signature(source, cached);
            h.word(id as u64);
            h.sampler(&sampler);
        }
        h.word(backend::buffer_id(&mesh.buffer) as u64);
        h.word(mesh.index_offset as u64);
        if let Some(source) = &slot.slot_gpu_vertices {
            let (key, buffer, ids) = gpu_identity(source);
            h.text(&key);
            h.word(buffer as u64);
            for id in ids {
                h.word(id as u64);
            }
        }
        if let Some(a) = &slot.slot_vertex_attributes {
            h.word(backend::buffer_id(&a.borrow().auxiliary_buffer) as u64);
        }
        if let Some(u) = &slot.slot_uniform {
            h.word(backend::buffer_id(&u.uniform_buffer) as u64);
            h.word(u.uniform_offset as u64);
        }
        h.word(mesh.vertex_count as u64);
        h.word(mesh.index_count as u64);
    }
    (h.0 >> 1) as i64
}

fn has_depth(family: pipeline_family) -> bool {
    !matches!(family, Scene2 | Scene2_textured | Ui | Marks)
}

/// Attachments and encoder state for one render pass over `target`.
fn render_target(
    value: &mut t,
    family: pipeline_family,
    samples: i64,
    state: &state,
    load: Load,
    clear: (f64, f64, f64, f64),
    later_color: bool,
    later_depth: bool,
) -> Result<RenderTarget, Error> {
    // an attachment no other pass reads needs no memory: it clears here and only its resolve stays
    let color_passes = load == Load::Clear && !later_color;
    let depth_passes = state.depth_load == Load::Clear && !later_depth;
    let mut keys: Vec<(scene_attachment_pool::kind, i64, bool)> = Vec::new();
    if samples != 1 {
        keys.push((scene_attachment_pool::kind::Color, samples, color_passes));
    }
    if has_depth(family) {
        keys.push((scene_attachment_pool::kind::Depth, samples, depth_passes));
    }
    let pooled = scene_attachment_pool::acquire_many(&mut value.attachments, &keys)?;
    let mut attachments = pooled.into_iter();
    let color = if samples == 1 { value.target.clone() } else { attachments.next().expect("color attachment") };
    let resolve = if samples == 1 { None } else { Some(value.target.clone()) };
    let depth = if has_depth(family) {
        Some(DepthAttachment {
            depth_texture: attachments.next().expect("depth attachment"),
            depth_load: state.depth_load,
            depth_store: if depth_passes { Store::Discard } else { Store::Store },
            depth_clear: state.depth_clear,
        })
    } else {
        None
    };
    Ok(RenderTarget {
        colors: vec![ColorAttachment {
            texture: color,
            resolve,
            load,
            store: if samples == 1 { Store::Store } else { Store::Resolve },
            clear,
        }],
        depth,
    })
}

fn sampler_key(descriptor: &SamplerDescriptor) -> String {
    format!("{:?}", descriptor)
}

fn sampler_for(value: &mut t, descriptor: &SamplerDescriptor) -> Result<Sampler, Error> {
    let key = sampler_key(descriptor);
    if let Some(sampler) = value.samplers.get(&key) {
        return Ok(sampler.clone());
    }
    // ponytail: 64 distinct sampler states per renderer; a scene using more clears the table and
    // drops replay plans that borrowed them.
    if value.samplers.len() >= 64 {
        for sampler in value.samplers.values() {
            let _ = backend::destroy_sampler(sampler);
        }
        value.samplers.clear();
        drop_plans(value);
    }
    let sampler = backend::create_sampler(&value.device, descriptor.clone())?;
    value.samplers.insert(key, sampler.clone());
    Ok(sampler)
}

const argument_page_bytes: i64 = 256 * 1024;

fn argument_encoder(value: &mut t, variant: usize) -> Result<Argument, Error> {
    if let Some(argument) = &value.pipelines[variant].argument {
        return Ok(argument.clone());
    }
    let argument = backend::create_argument(&value.pipelines[variant].pipeline, ShaderStage::Fragment, 7)?;
    value.pipelines[variant].argument = Some(argument.clone());
    Ok(argument)
}

/// Canonical Scene2 binds its texture and sampler through one argument slice per (texture, sampler)
/// pair inside a shared page, so indirect commands can reference textures.
fn argument_slice(value: &mut t, variant: usize, texture: &Texture, sampler_descriptor: &SamplerDescriptor) -> Result<(Buffer, i64), Error> {
    let argument = argument_encoder(value, variant)?;
    if value.arguments.is_none() {
        let length = backend::argument_length(&argument);
        let alignment = backend::argument_alignment(&argument).max(1);
        let stride = 16.max((length + alignment - 1) / alignment * alignment);
        let descriptor = BufferDescriptor {
            label: Some("scene-argument-page".to_owned()),
            size: argument_page_bytes,
            usage: vec![BufferUsage::Storage],
        };
        let argument_buffer = backend::create_buffer(&value.device, descriptor, None)?;
        value.arguments = Some(argument_pool {
            argument_buffer,
            argument_stride: stride,
            argument_capacity: argument_page_bytes / stride,
            slices: HashMap::new(),
            argument_next: 0,
        });
    }
    let (stride, buffer) = {
        let pool = value.arguments.as_ref().expect("argument pool");
        (pool.argument_stride, pool.argument_buffer.clone())
    };
    if backend::argument_length(&argument) > stride {
        return Err(failure_error("Scene_execution.argument_slice", ErrorKind::Capacity, "argument layout exceeds the shared slice stride"));
    }
    let key = (backend::texture_id(texture), sampler_key(sampler_descriptor));
    if let Some(offset) = value.arguments.as_ref().expect("argument pool").slices.get(&key) {
        return Ok((buffer, *offset));
    }
    {
        let pool = value.arguments.as_mut().expect("argument pool");
        if pool.argument_next >= pool.argument_capacity {
            pool.slices.clear();
            pool.argument_next = 0;
            drop_plans(value);
        }
    }
    let offset = value.arguments.as_ref().expect("argument pool").argument_next * stride;
    let sampler = sampler_for(value, sampler_descriptor)?;
    backend::argument_texture(&argument, &buffer, offset, 0, texture)?;
    backend::argument_sampler(&argument, &buffer, offset, 1, &sampler)?;
    let pool = value.arguments.as_mut().expect("argument pool");
    pool.argument_next += 1;
    pool.slices.insert(key, offset);
    Ok((buffer, offset))
}

enum acquired {
    Offscreen,
    Presented(backend::Frame),
}

fn discard(frame: &acquired) {
    if let acquired::Presented(frame) = frame {
        let _ = backend::discard(frame);
    }
}

/// One presented frame stays in flight: the CPU builds the next frame while the GPU draws this one.
/// Waiting here, before any preparation, keeps every resource the previous frame used alive until it
/// completes, so retiring and destroying after the next submission stays safe.
fn settle(value: &mut t) -> Result<(), Error> {
    match value.in_flight.take() {
        None => Ok(()),
        Some(epoch) => backend::complete_through(&value.queue, epoch),
    }
}

enum acquisition {
    Acquired(acquired),
    Skipped,
}

fn acquire(value: &mut t) -> Result<acquisition, Error> {
    settle(value)?;
    match &value.surface {
        None => Ok(acquisition::Acquired(acquired::Offscreen)),
        Some(surface) => match backend::acquire_sync(surface)? {
            backend::AcquireResult::Timeout | backend::AcquireResult::Occluded => Ok(acquisition::Skipped),
            backend::AcquireResult::DeviceLost => Err(failure_error("Renderer.submit", ErrorKind::DeviceLost, "device lost")),
            backend::AcquireResult::Acquired(frame) => Ok(acquisition::Acquired(acquired::Presented(frame))),
        },
    }
}

fn plain_draws(draws: &[plan_draw]) -> bool {
    draws.iter().all(|draw| draw.plan_textures.is_empty() && draw.plan_samplers.is_empty() && draw.plan_indirect.is_none())
}

fn encode_batch(
    value: &mut t,
    commands: &backend::Commands,
    clear: (f64, f64, f64, f64),
    later: &[Rc<batch_plan>],
    batch: &batch_plan,
) -> Result<(), Error> {
    let same_samples = |next: &Rc<batch_plan>| next.batch_samples == batch.batch_samples;
    let target = render_target(
        value,
        batch.batch_family,
        batch.batch_samples,
        &batch.batch_state,
        if batch.batch_first { Load::Clear } else { Load::Load },
        clear,
        later.iter().any(same_samples),
        later.iter().any(|next| same_samples(next) && has_depth(next.batch_family) && next.batch_state.depth_load == Load::Load),
    )?;
    let encoder = backend::render_encoder(commands, &target)?;
    let bound_w = value.configuration.physical_width;
    let bound_h = value.configuration.physical_height;
    // A viewport or scissor with no pixel on the target draws nothing. The viewport is passed
    // whole: it maps clip space, so a view that hangs over the target edge keeps its proportions
    // and only the scissor is clipped.
    let (visible, (x, y, width, height), (sx, sy, sw, sh)) = match (
        intersect_extent(bound_w, bound_h, batch.batch_state.viewport),
        intersect_extent(bound_w, bound_h, batch.batch_state.scissor),
    ) {
        (Some(_), Some(scissor)) => (true, batch.batch_state.viewport, scissor),
        _ => (false, (0, 0, bound_w.max(1), bound_h.max(1)), (0, 0, bound_w.max(1), bound_h.max(1))),
    };
    backend::set_viewport(&encoder, Rect { x, y, width, height })?;
    backend::set_scissor(&encoder, Rect { x: sx, y: sy, width: sw, height: sh })?;
    backend::set_cull(&encoder, batch.batch_state.cull)?;
    // 3D meshes wind counter-clockwise: a fact of the family, never of a bound slot
    if has_depth(batch.batch_family) {
        backend::set_winding(&encoder, backend::Winding::CounterClockwise)?;
        backend::set_depth_state(
            &encoder,
            Some(DepthState { depth_compare: batch.batch_state.depth_compare, depth_write: batch.batch_state.depth_write }),
        )?;
    }
    if !batch.batch_draws.is_empty() && visible {
        if !batch.batch_resources.is_empty() {
            backend::use_resources(&encoder, &batch.batch_resources)?;
        }
        let icb = batch.batch_icb.borrow().clone();
        match icb {
            Some(icb) => {
                backend::set_render_pipeline(&encoder, &batch.batch_draws[0].plan_draw.pipeline)?;
                value.icb_stats.icb_executions += 1;
                backend::execute_icb(&encoder, &icb, 0, batch.batch_draws.len() as i64)?;
            }
            None => {
                if plain_draws(&batch.batch_draws) {
                    backend::draw_batch(&encoder, &batch.batch_driver_draws)?;
                } else {
                    for draw in &batch.batch_draws {
                        let bd = &draw.plan_draw;
                        backend::set_render_pipeline(&encoder, &bd.pipeline)?;
                        for (stage, index, buffer, offset) in &bd.buffers {
                            backend::set_stage_buffer(&encoder, *stage, *index, Some(*offset), buffer)?;
                        }
                        for (stage, index, texture) in &draw.plan_textures {
                            backend::set_stage_texture(&encoder, *stage, *index, texture)?;
                        }
                        for (stage, index, sampler) in &draw.plan_samplers {
                            backend::set_stage_sampler(&encoder, *stage, *index, sampler)?;
                        }
                        match (&bd.index, &draw.plan_indirect) {
                            (Some((index_type, buffer, offset, _)), Some((arguments, arguments_offset))) => {
                                backend::draw_indexed_indirect(&encoder, bd.primitive, *index_type, buffer, *offset, arguments, *arguments_offset)?
                            }
                            (None, _) => {
                                backend::draw(&encoder, bd.primitive, bd.vertex_start, bd.vertex_count, Some(bd.instances))?
                            }
                            (Some((index_type, buffer, offset, count)), None) => backend::draw_indexed(
                                &encoder,
                                bd.primitive,
                                *index_type,
                                buffer,
                                *offset,
                                *count,
                                Some(bd.instances),
                            )?,
                        }
                    }
                }
            }
        }
    }
    backend::end_render(&encoder)
}

fn build_icb(value: &mut t, batch: &batch_plan) -> Result<(), Error> {
    let draws = &batch.batch_draws;
    if draws.is_empty()
        || !plain_draws(draws)
        || batch.batch_icb.borrow().is_some()
        || !draws.iter().all(|draw| backend::pipeline_indirect(&draw.plan_draw.pipeline))
    {
        return Ok(());
    }
    let icb = backend::create_icb(&value.device, draws.len() as i64)?;
    let encoded = (|| -> Result<(), Error> {
        for (index, draw) in draws.iter().enumerate() {
            let index = index as i64;
            let bd = &draw.plan_draw;
            backend::icb_set_pipeline(&icb, index, &bd.pipeline)?;
            for (stage, slot, buffer, offset) in &bd.buffers {
                backend::icb_set_buffer(&icb, index, *stage, *slot, Some(*offset), buffer)?;
            }
            match &bd.index {
                None => backend::icb_draw(&icb, index, bd.primitive, bd.vertex_start, bd.vertex_count, Some(bd.instances))?,
                Some((index_type, buffer, offset, count)) => {
                    backend::icb_draw_indexed(&icb, index, bd.primitive, *index_type, buffer, *offset, *count, Some(bd.instances))?
                }
            }
        }
        Ok(())
    })();
    match encoded {
        Err(e) => {
            let _ = backend::destroy_icb(&icb);
            Err(e)
        }
        Ok(()) => {
            value.icb_stats.icb_builds += 1;
            *batch.batch_icb.borrow_mut() = Some(icb);
            Ok(())
        }
    }
}

fn submit_frame(value: &mut t, frame: &acquired, commands: &backend::Commands) -> Result<(), Error> {
    let committed = match frame {
        acquired::Offscreen => backend::commit(commands),
        acquired::Presented(surface_frame) => backend::commit_present(commands, &value.target, surface_frame),
    };
    match committed {
        Err(e) => {
            let _ = backend::abandon(commands);
            discard(frame);
            Err(e)
        }
        Ok(receipt) => match frame {
            acquired::Presented(_) => {
                value.in_flight = Some(receipt.epoch);
                Ok(())
            }
            acquired::Offscreen => backend::complete_through(&value.queue, receipt.epoch),
        },
    }
}

/// Encodes `batches` into one command buffer and completes it.
fn encode_frame(value: &mut t, frame: &acquired, clear: (f64, f64, f64, f64), batches: &[Rc<batch_plan>]) -> Result<(), Error> {
    let commands = match backend::begin_commands(&value.queue) {
        Ok(commands) => commands,
        Err(e) => {
            discard(frame);
            return Err(e);
        }
    };
    let encoded = (|| -> Result<(), Error> {
        for (index, batch) in batches.iter().enumerate() {
            encode_batch(value, &commands, clear, &batches[index + 1..], batch)?;
        }
        Ok(())
    })();
    match encoded {
        Err(e) => {
            let _ = backend::abandon(&commands);
            discard(frame);
            Err(e)
        }
        Ok(()) => submit_frame(value, frame, &commands),
    }
}

// World sun shadows: a draw whose World block asks for [sun_shadow = Some key] samples the
// renderer's sun map, rendered here from every depth-writing Replace or Alpha World triangle draw
// carrying the same key (the block's words 40..55 hold the sun view-projection) and re-rendered
// only when the key changes.
// ponytail: one 2048^2 map per renderer (32 MB with its depth attachment); a second World in the
// same frame with another key samples the first's map; a translucent depth-writing draw casts an
// opaque shadow. Cascades, per-view maps and alpha-tested casters lift these ceilings.

/// Width and height of the sun map.
pub const sun_map_size: i64 = 2048;

fn sun_key_of(slot: &prepared_slot) -> Option<String> {
    slot.slot_auxiliary.as_ref().and_then(|(source, _, _)| source.sun_shadow.clone())
}

fn sun_descriptor(label: &str, usage: Vec<TextureUsage>) -> TextureDescriptor {
    TextureDescriptor {
        label: Some(label.to_owned()),
        width: sun_map_size,
        height: sun_map_size,
        depth: 1,
        mip_levels: 1,
        sample_count: 1,
        format: TextureFormat::Rgba8Unorm,
        usage,
    }
}

fn render_sun(value: &mut t) -> Result<(), Error> {
    let mut request: Option<String> = None;
    for index in (0..value.prepared_scratch.scratch_length).rev() {
        if let Some(key) = sun_key_of(&value.prepared_scratch.scratch_slots[index]) {
            request = Some(key);
        }
    }
    let Some(key) = request else { return Ok(()) };
    if matches!(&value.sun, Some(sun) if sun.sun_key == key) {
        return Ok(());
    }
    if value.sun.is_none() {
        let Some(factory) = value.sun_factory.clone() else {
            return Err(failure_error("Renderer.submit", ErrorKind::Unsupported, "renderer has no sun shadow pipeline"));
        };
        let pipeline = factory(&value.device)?;
        let color = match backend::create_texture(
            &value.device,
            sun_descriptor("scene-sun-shadow", vec![TextureUsage::TextureBinding, TextureUsage::RenderAttachment]),
        ) {
            Ok(color) => color,
            Err(e) => {
                let _ = backend::destroy_pipeline(&pipeline);
                return Err(e);
            }
        };
        let depth = match backend::create_depth_texture(&value.device, sun_descriptor("scene-sun-depth", vec![TextureUsage::RenderAttachment])) {
            Ok(depth) => depth,
            Err(e) => {
                let _ = backend::destroy_pipeline(&pipeline);
                let _ = backend::destroy_texture(&color);
                return Err(e);
            }
        };
        value.sun = Some(sun_map { sun_pipeline: pipeline, sun_color: color, sun_depth: depth, sun_key: String::new() });
    }
    // Retained plans may have sampled the previous map.
    drop_plans(value);
    value.sun.as_mut().expect("sun map").sun_key = String::new();
    let (sun_pipeline, sun_color, sun_depth) = {
        let sun = value.sun.as_ref().expect("sun map");
        (sun.sun_pipeline.clone(), sun.sun_color.clone(), sun.sun_depth.clone())
    };
    let commands = backend::begin_commands(&value.queue)?;
    let encoded = (|| -> Result<(), Error> {
        let encoder = backend::render_encoder(
            &commands,
            &RenderTarget {
                colors: vec![ColorAttachment {
                    texture: sun_color.clone(),
                    resolve: None,
                    load: Load::Clear,
                    store: Store::Store,
                    clear: (1.0, 1.0, 1.0, 1.0),
                }],
                depth: Some(DepthAttachment {
                    depth_texture: sun_depth.clone(),
                    depth_load: Load::Clear,
                    depth_store: Store::Discard,
                    depth_clear: 1.0,
                }),
            },
        )?;
        backend::set_render_pipeline(&encoder, &sun_pipeline)?;
        backend::set_viewport(&encoder, Rect { x: 0, y: 0, width: sun_map_size, height: sun_map_size })?;
        backend::set_cull(&encoder, Cull::CullNone)?;
        backend::set_depth_state(&encoder, Some(DepthState { depth_compare: Comparison::Less, depth_write: true }))?;
        for index in 0..value.prepared_scratch.scratch_length {
            let slot = &value.prepared_scratch.scratch_slots[index];
            let (Some((_, world, _)), Some(item), Some(state)) = (&slot.slot_auxiliary, &slot.slot_mesh, &slot.slot_state) else {
                continue;
            };
            let item = item.borrow();
            if !(sun_key_of(slot).as_deref() == Some(key.as_str())
                && slot.slot_family == Scene3_world
                && (slot.slot_blend == Blend::Replace || slot.slot_blend == Blend::Alpha)
                && state.depth_write
                && item.primitive == Primitive::TriangleList)
            {
                continue;
            }
            let bytes_ = state.transform_uniforms.as_ref().map_or(0, |b| blen(b) as i64);
            let uniforms: Option<(Buffer, i64, i64)> = match (&slot.slot_uniform, item.uniform_offset) {
                (Some(uniform), _) => Some((
                    uniform.uniform_buffer.clone(),
                    uniform.uniform_offset,
                    if bytes_ > 5456 { uniform.uniform_offset + 5456 } else { uniform.uniform_offset },
                )),
                (None, Some(offset)) => Some((item.buffer.clone(), offset, offset)),
                (None, None) => None,
            };
            let Some((buffer, offset, instances_offset)) = uniforms else { continue };
            // scene.rays sun-vertex: records as floats 0 (slot 1 unused), the World block 2, surface
            // 3, instances 4; a device mesh's borrowed positions at 0
            backend::set_stage_buffer(
                &encoder,
                ShaderStage::Vertex,
                0,
                None,
                match &slot.slot_gpu_vertices {
                    Some(source) => &source.gpu_buffer,
                    None => &item.buffer,
                },
            )?;
            backend::set_stage_buffer(&encoder, ShaderStage::Vertex, 2, None, &world.borrow().auxiliary_buffer)?;
            backend::set_stage_buffer(&encoder, ShaderStage::Vertex, 3, Some(offset), &buffer)?;
            backend::set_stage_buffer(&encoder, ShaderStage::Vertex, 4, Some(instances_offset), &buffer)?;
            let (indices, index_offset, index_count) = match slot.slot_gpu_vertices.as_ref().and_then(|source| source.gpu_indices.clone()) {
                Some((buffer, count)) => (buffer, 0, count),
                None => (item.buffer.clone(), item.index_offset, item.index_count),
            };
            backend::draw_indexed(
                &encoder,
                Primitive::TriangleList,
                IndexType::Uint32,
                &indices,
                index_offset,
                index_count,
                Some(if bytes_ > 5456 { (bytes_ - 5456) / 192 } else { 1 }),
            )?;
        }
        backend::end_render(&encoder)
    })();
    match encoded {
        Err(e) => {
            let _ = backend::abandon(&commands);
            Err(e)
        }
        Ok(()) => {
            let receipt = backend::commit(&commands)?;
            backend::complete_through(&value.queue, receipt.epoch)?;
            value.sun.as_mut().expect("sun map").sun_key = key;
            value.sun_passes += 1;
            Ok(())
        }
    }
}

/// Sun map passes rendered so far (see `auxiliary_resource.sun_shadow`).
pub fn sun_shadow_passes(value: &t) -> i64 {
    value.sun_passes
}

fn replay_plan(value: &mut t, plan: &Rc<replay_plan>) -> Result<bool, Error> {
    if !plan.replay_gpu_vertices.iter().all(gpu_vertices_valid) {
        return Err(failure_error("Scene_execution.replay", ErrorKind::StaleHandle, "GPU vertices are stale or closed"));
    }
    match acquire(value)? {
        acquisition::Skipped => Ok(false),
        acquisition::Acquired(frame) => encode_frame(value, &frame, plan.replay_clear, &plan.replay_batches).map(|()| true),
    }
}

/// 0: Scene2, 1: Scene3-like, 3: Ui and Marks draws bind directly; argument-buffer passes cannot
/// mix them.
fn attachment_class(family: pipeline_family) -> i64 {
    match family {
        Scene2 | Scene2_textured => 0,
        Ui | Marks => 3,
        Scene3 | Scene3_points | Scene3_textured | Scene3_world => 1,
    }
}

fn same_pass_state(class_: i64, a: &state, b: &state) -> bool {
    a.viewport == b.viewport
        && a.scissor == b.scissor
        && a.cull == b.cull
        && (class_ == 0
            || class_ == 3
            || (a.depth_compare == b.depth_compare && a.depth_write == b.depth_write && a.depth_load == b.depth_load && a.depth_clear == b.depth_clear))
}

fn rebind(slots: &[((ShaderStage, i64), Vec<i64>)], stage: ShaderStage, slot: i64) -> Vec<i64> {
    match slots.iter().find(|(key, _)| *key == (stage, slot)) {
        Some((_, s)) => s.clone(),
        None => vec![slot],
    }
}

fn assoc(table: &[(i64, i64)], slot: i64) -> i64 {
    table.iter().find(|(from, _)| *from == slot).map_or(slot, |(_, to)| *to)
}

type plan_entry_result = (plan_draw, Option<automatic_signature>);

/// One draw's plan: the bindings its pipeline's Lisp declaration order expects.
/// `keep`: the plan is retained, and replays when each slot matches its signature.
fn plan_entry(value: &mut t, index: usize, keep: bool) -> Result<plan_entry_result, Error> {
    let slot = &value.prepared_scratch.scratch_slots[index];
    let family = slot.slot_family;
    let blend = slot.slot_blend;
    let samples = slot.slot_samples;
    let item_ref = slot.slot_mesh.clone().expect("slot mesh");
    let slot_texture = slot.slot_texture.clone();
    let slot_auxiliary = slot.slot_auxiliary.clone();
    let slot_environment = slot.slot_environment.clone();
    let slot_uniform = slot.slot_uniform.clone();
    let slot_gpu_vertices = slot.slot_gpu_vertices.clone();
    let slot_vertex_attributes = slot.slot_vertex_attributes.clone();
    let transform_uniforms = slot.slot_state.as_ref().expect("slot state").transform_uniforms.clone();
    let signature = keep.then(|| automatic_signature(slot));
    let sun_requested = sun_key_of(slot).is_some();
    let item = item_ref.borrow();
    if let Some(source) = &slot_gpu_vertices {
        if !gpu_vertices_valid(source) {
            return Err(failure_error("Renderer.submit", ErrorKind::StaleHandle, "GPU vertices are stale or closed"));
        }
    }
    let scene2 = family == Scene2 || family == Scene2_textured;
    let direct = scene2 || family == Ui || family == Marks;
    let variant = find_pipeline(value, family, blend, samples).expect("pipeline variant");
    let vertex_stage = ShaderStage::Vertex;
    let fragment_stage = ShaderStage::Fragment;
    let (mut buffers, mut textures, mut samplers, resident): (
        Vec<(ShaderStage, i64, Buffer, i64)>,
        Vec<(ShaderStage, i64, Texture)>,
        Vec<(ShaderStage, i64, Sampler)>,
        Vec<Texture>,
    ) = match &slot_texture {
        None => (vec![], vec![], vec![], vec![]),
        Some((source, cached)) => {
            let texture = cached.borrow().texture.clone();
            if family == Scene2_textured {
                let (page, offset) = argument_slice(value, variant, &texture, &source.sampler)?;
                (vec![(fragment_stage, 7, page, offset)], vec![], vec![], vec![texture])
            } else {
                let sampler = sampler_for(value, &source.sampler)?;
                (vec![], vec![(fragment_stage, 1, texture)], vec![(fragment_stage, 2, sampler)], vec![])
            }
        }
    };
    let vertices: Buffer = match &slot_gpu_vertices {
        None => item.buffer.clone(),
        Some(source) => source.gpu_buffer.clone(),
    };
    let mut extra: Vec<(ShaderStage, i64, Buffer, i64)> = Vec::new();
    if let Some(source) = &slot_gpu_vertices {
        for (i, b) in source.gpu_extra.iter().enumerate() {
            extra.push((vertex_stage, i as i64 + 1, b.clone(), 0));
        }
        if is_scene3_family(family) && source.gpu_extra.is_empty() {
            extra.push((vertex_stage, 1, source.gpu_buffer.clone(), 0));
        }
    }
    match &slot_auxiliary {
        None => {
            let mut all = vec![(vertex_stage, 0, vertices.clone(), 0)];
            all.extend(extra);
            all.extend(buffers);
            buffers = all;
        }
        Some((source, buffer, texture)) => {
            let sampler = sampler_for(value, &source.texture.sampler)?;
            let mut all = vec![(vertex_stage, 0, vertices.clone(), 0), (fragment_stage, 3, buffer.borrow().auxiliary_buffer.clone(), 0)];
            all.extend(extra);
            all.extend(buffers);
            buffers = all;
            textures.insert(0, (fragment_stage, 4, texture.borrow().texture.clone()));
            samplers.insert(0, (fragment_stage, 5, sampler));
        }
    }
    // World draws bind the sun map at 8 (the camera map stands in when there is none; the block
    // says whether to read it) and the camera map at 9/10.
    if let (Scene3_world, Some((source, cached))) = (family, &slot_environment) {
        let sampler = sampler_for(value, &source.sampler)?;
        let camera = cached.borrow().texture.clone();
        let sun = match (&value.sun, sun_requested) {
            (Some(sun), true) => sun.sun_color.clone(),
            _ => camera.clone(),
        };
        textures.insert(0, (fragment_stage, 9, camera));
        textures.insert(0, (fragment_stage, 8, sun));
        samplers.insert(0, (fragment_stage, 10, sampler));
    }
    if !direct {
        let attributes = match &slot_vertex_attributes {
            Some(attributes) => attributes.borrow().auxiliary_buffer.clone(),
            None => vertices.clone(),
        };
        buffers.insert(0, (vertex_stage, 11, attributes, 0));
    }
    match (&slot_uniform, item.uniform_offset) {
        (Some(uniform), _) => {
            let binding = |stage: ShaderStage| (stage, 6, uniform.uniform_buffer.clone(), uniform.uniform_offset);
            let mut all = vec![binding(vertex_stage)];
            if !direct {
                let offset = match &transform_uniforms {
                    Some(b) if blen(b) > 5456 => uniform.uniform_offset + 5456,
                    _ => uniform.uniform_offset,
                };
                all.push((vertex_stage, 7, uniform.uniform_buffer.clone(), offset));
                all.push(binding(fragment_stage));
            }
            all.extend(buffers);
            buffers = all;
        }
        (None, Some(offset)) => {
            let mut all = vec![(vertex_stage, 6, item.buffer.clone(), offset)];
            if !direct {
                all.push((vertex_stage, 7, item.buffer.clone(), offset));
            }
            all.push((fragment_stage, 6, item.buffer.clone(), offset));
            all.extend(buffers);
            buffers = all;
        }
        (None, None) => {}
    }
    // The generated pipelines bind in their Lisp declaration order (shaders/*.rays): ui.rays its
    // records as floats 0 and words 1, the affine 2, the atlas 3 (sampler 4); scene.rays its records
    // 0 and 1, surface 2, instances 3, attributes 4 and 5, the texture 6 (sampler 7), and for World
    // its block 6, image 7 (8), specular map 9 (10), camera map 11 (12) and sun map 13; marks.rays
    // Scene2's vertex words 0 and constants 1, a quad's constants 0 and its texture's argument
    // buffer 7.
    let regenerated = |buffers: Vec<(ShaderStage, i64, Buffer, i64)>,
                       textures: Vec<(ShaderStage, i64, Texture)>,
                       samplers: Vec<(ShaderStage, i64, Sampler)>,
                       buffers_to: &[((ShaderStage, i64), Vec<i64>)],
                       textures_to: &[(i64, i64)],
                       samplers_to: &[(i64, i64)]| {
        let mut b2 = Vec::new();
        for (stage, slot, b, o) in buffers {
            for s in rebind(buffers_to, stage, slot) {
                b2.push((stage, s, b.clone(), o));
            }
        }
        (
            b2,
            textures.into_iter().map(|(stage, slot, tx)| (stage, assoc(textures_to, slot), tx)).collect::<Vec<_>>(),
            samplers.into_iter().map(|(stage, slot, s)| (stage, assoc(samplers_to, slot), s)).collect::<Vec<_>>(),
        )
    };
    let v = vertex_stage;
    let f = fragment_stage;
    let scene3: Vec<((ShaderStage, i64), Vec<i64>)> =
        vec![((v, 0), vec![0, 1]), ((v, 6), vec![2]), ((f, 6), vec![2]), ((v, 7), vec![3]), ((v, 11), vec![4, 5])];
    let device_scene3 = |scene3: &Vec<((ShaderStage, i64), Vec<i64>)>| {
        // a device's positions at 0 alone, its normals (or the positions again) at 1
        let mut all = vec![((v, 0), vec![0])];
        all.extend(scene3[1..].iter().cloned());
        all
    };
    let (buffers, textures, samplers) = match family {
        Ui => regenerated(buffers, textures, samplers, &[((v, 0), vec![0, 1]), ((v, 6), vec![2])], &[(1, 3)], &[(2, 4)]),
        Scene2 | Scene2_textured => regenerated(buffers, textures, samplers, &[((v, 6), vec![1])], &[], &[]),
        Scene3 | Scene3_points if slot_gpu_vertices.is_some() => {
            regenerated(buffers, textures, samplers, &device_scene3(&scene3), &[(1, 6)], &[(2, 7)])
        }
        Scene3 | Scene3_points | Scene3_textured => regenerated(buffers, textures, samplers, &scene3, &[(1, 6)], &[(2, 7)]),
        Scene3_world => {
            let base = if slot_gpu_vertices.is_some() { device_scene3(&scene3) } else { scene3.clone() };
            let mut table = vec![((f, 3), vec![6])];
            table.extend(base);
            regenerated(buffers, textures, samplers, &table, &[(1, 7), (4, 9), (8, 13), (9, 11)], &[(2, 8), (5, 10), (10, 12)])
        }
        Marks => (buffers, textures, samplers),
    };
    let gpu_indices = slot_gpu_vertices.as_ref().and_then(|source| source.gpu_indices.clone());
    let index = {
        let (buffer, offset, count) = match gpu_indices {
            Some((buffer, count)) => (buffer, 0, count),
            None => (item.buffer.clone(), item.index_offset, item.index_count),
        };
        Some((IndexType::Uint32, buffer, offset, count))
    };
    let buffers = match slot_gpu_vertices.as_ref().and_then(|source| source.gpu_colors.clone()) {
        None => buffers,
        Some((colors, _)) => {
            let has_attributes = slot_vertex_attributes.is_some();
            let mut all = vec![(vertex_stage, 4, colors.clone(), 0)];
            if !has_attributes {
                all.push((vertex_stage, 5, colors, 0));
            }
            all.extend(buffers.into_iter().filter(|(stage, binding, _, _)| {
                *stage != vertex_stage || (*binding != 4 && (*binding != 5 || has_attributes))
            }));
            all
        }
    };
    let instances = match (family, &transform_uniforms) {
        (Scene3 | Scene3_points | Scene3_textured | Scene3_world, Some(b)) if blen(b) > 5456 && (blen(b) - 5456) % 192 == 0 => {
            (blen(b) as i64 - 5456) / 192
        }
        _ => 1,
    };
    let driver_pipeline = value.pipelines[variant].pipeline.clone();
    Ok((
        plan_draw {
            plan_draw: BatchDraw {
                pipeline: driver_pipeline,
                buffers,
                primitive: item.primitive,
                index,
                vertex_start: 0,
                vertex_count: item.vertex_count,
                instances,
            },
            plan_textures: textures,
            plan_samplers: samplers,
            plan_resident: resident,
            plan_indirect: slot_gpu_vertices.as_ref().and_then(|source| source.gpu_indirect.clone()),
        },
        signature,
    ))
}

fn batch_resources_of(value: &t, draws: &[plan_draw]) -> Vec<ResourceUse> {
    let mut seen_buffers: std::collections::HashSet<i64> = std::collections::HashSet::new();
    let mut seen_textures: std::collections::HashSet<i64> = std::collections::HashSet::new();
    let mut resources: Vec<ResourceUse> = Vec::new();
    let mut buffer = |b: &Buffer, resources: &mut Vec<ResourceUse>| {
        if seen_buffers.insert(backend::buffer_id(b)) {
            resources.push(ResourceUse::Buffer(b.clone()));
        }
    };
    let mut texture_seen = |tx: &Texture, resources: &mut Vec<ResourceUse>| {
        if seen_textures.insert(backend::texture_id(tx)) {
            resources.push(ResourceUse::Texture(tx.clone()));
        }
    };
    for draw in draws {
        for (_, _, b, _) in &draw.plan_draw.buffers {
            buffer(b, &mut resources);
        }
        if let Some((_, b, _, _)) = &draw.plan_draw.index {
            buffer(b, &mut resources);
        }
        if let Some((b, _)) = &draw.plan_indirect {
            buffer(b, &mut resources);
        }
        for (_, _, tx) in &draw.plan_textures {
            texture_seen(tx, &mut resources);
        }
        for tx in &draw.plan_resident {
            texture_seen(tx, &mut resources);
        }
    }
    if let Some(pool) = &value.arguments {
        buffer(&pool.argument_buffer, &mut resources);
    }
    resources
}

fn plan_batches(value: &mut t, keep: bool) -> Result<Vec<(Rc<batch_plan>, Vec<automatic_signature>)>, Error> {
    let length = value.prepared_scratch.scratch_length;
    let mut planned: Vec<(Rc<batch_plan>, Vec<automatic_signature>)> = Vec::new();
    let mut first = true;
    let mut index = 0;
    while index < length {
        let (family, samples, state) = {
            let slot = &value.prepared_scratch.scratch_slots[index];
            (slot.slot_family, slot.slot_samples, slot.slot_state.clone().expect("slot state"))
        };
        let class_ = attachment_class(family);
        let mut next = index + 1;
        while next < length && next - (index + 1) < 65_535 {
            let slot = &value.prepared_scratch.scratch_slots[next];
            if attachment_class(slot.slot_family) == class_
                && slot.slot_samples == samples
                && same_pass_state(class_, &state, slot.slot_state.as_ref().expect("slot state"))
            {
                next += 1;
            } else {
                break;
            }
        }
        let mut draws: Vec<plan_draw> = Vec::with_capacity(next - index);
        let mut signatures: Vec<automatic_signature> = Vec::with_capacity(if keep { next - index } else { 0 });
        for slot_index in index..next {
            let (draw, signature) = plan_entry(value, slot_index, keep)?;
            draws.push(draw);
            signatures.extend(signature);
        }
        let batch = batch_plan {
            batch_family: family,
            batch_samples: samples,
            batch_state: state,
            batch_first: first,
            batch_driver_draws: draws.iter().map(|d| d.plan_draw.clone()).collect(),
            batch_resources: batch_resources_of(value, &draws),
            batch_draws: draws,
            batch_icb: RefCell::new(None),
        };
        planned.push((Rc::new(batch), signatures));
        first = false;
        index = next;
    }
    if first {
        let width = value.configuration.physical_width;
        let height = value.configuration.physical_height;
        let state = state {
            viewport: (0, 0, width, height),
            scissor: (0, 0, width, height),
            cull: Cull::CullNone,
            depth_compare: Comparison::Always,
            depth_write: false,
            depth_load: Load::Load,
            depth_clear: 1.0,
            transform_uniforms: None,
        };
        planned.push((
            Rc::new(batch_plan {
                batch_family: Scene2,
                batch_samples: 1,
                batch_state: state,
                batch_first: true,
                batch_draws: vec![],
                batch_driver_draws: vec![],
                batch_resources: vec![],
                batch_icb: RefCell::new(None),
            }),
            vec![],
        ));
    }
    Ok(planned)
}

fn finish_prepared_frame(
    value: &mut t,
    frame: acquired,
    clear: (f64, f64, f64, f64),
    prepared_key: &Option<(String, i64)>,
    draw_count: i64,
    uniform_slices: Vec<Option<uniform_slice>>,
    after_prepare: &mut dyn FnMut(),
) -> Result<bool, Error> {
    let candidate_fingerprint = automatic_candidate_fingerprint(&value.prepared_scratch);
    after_prepare();
    let hit = match &value.automatic_submission {
        Some(plan)
            if plan.replay_clear == clear
                && plan.replay_gpu_vertices.iter().all(gpu_vertices_valid)
                && same_scratch_payloads(&plan.replay_payloads, &value.prepared_scratch) =>
        {
            Some(plan.clone())
        }
        _ => None,
    };
    if let Some(plan) = hit {
        value.automatic_candidate = None;
        value.icb_stats.icb_hits += 1;
        clear_prepared_scratch(&mut value.prepared_scratch);
        // A prepared key claims the identical automatic plan, so its replay works.
        if let Some((identity, version)) = prepared_key {
            value.automatic_submission = None;
            value.prepared_submission = Some(prepared_submission {
                submission_identity: identity.clone(),
                submission_version: *version,
                submission_draw_count: draw_count,
                submission_plan: plan.clone(),
            });
        }
        return encode_frame(value, &frame, clear, &plan.replay_batches).map(|()| true);
    }
    value.icb_stats.icb_misses += 1;
    let payload_count = value.prepared_scratch.scratch_length as i64;
    let admit = match &value.automatic_candidate {
        Some(candidate) => {
            candidate.candidate_clear == clear
                && candidate.candidate_length == payload_count
                && candidate.candidate_fingerprint == candidate_fingerprint
        }
        None => false,
    };
    // a plan nothing keeps (a frame unlike the last, without a prepared key) is encoded only
    let retained = prepared_key.is_some() || admit;
    let planned = match plan_batches(value, retained) {
        Ok(planned) => planned,
        Err(e) => {
            discard(&frame);
            return Err(e);
        }
    };
    let mut plan_batches_: Vec<Rc<batch_plan>> = Vec::with_capacity(planned.len());
    let mut payloads: Vec<automatic_signature> = Vec::with_capacity(if retained { payload_count as usize } else { 0 });
    for (batch, signatures) in planned {
        plan_batches_.push(batch);
        if retained {
            payloads.extend(signatures);
        }
    }
    let gpu_vertices: Vec<gpu_vertices> = if retained {
        value.prepared_scratch.scratch_slots[..value.prepared_scratch.scratch_length].iter().rev().filter_map(|slot| slot.slot_gpu_vertices.clone()).collect()
    } else {
        Vec::new()
    };
    clear_prepared_scratch(&mut value.prepared_scratch);
    encode_frame(value, &frame, clear, &plan_batches_)?;
    value.previous_uniforms = uniform_slices;
    let plan = retained.then(|| {
        Rc::new(replay_plan { replay_clear: clear, replay_payloads: payloads, replay_batches: plan_batches_, replay_gpu_vertices: gpu_vertices })
    });
    if let Some(plan) = &plan {
        for batch in &plan.replay_batches {
            if let Err(e) = build_icb(value, batch) {
                value.icb_stats.icb_failures += 1;
                value.icb_stats.icb_last_failure = Some(error::to_string(&e));
            }
        }
    }
    if let Some((identity, version)) = prepared_key {
        value.prepared_submission = Some(prepared_submission {
            submission_identity: identity.clone(),
            submission_version: *version,
            submission_draw_count: draw_count,
            submission_plan: plan.clone().expect("a prepared key retains its plan"),
        });
    }
    let old = value.automatic_submission.take();
    drop_plan(value, old);
    if admit {
        value.automatic_submission = plan;
        value.automatic_candidate = None;
    } else {
        value.automatic_submission = None;
        value.automatic_candidate =
            Some(automatic_candidate { candidate_clear: clear, candidate_length: payload_count, candidate_fingerprint });
    }
    Ok(true)
}

/// Evicted and oversize resources are valid through this synchronous submission, but not through
/// another replay. Retained plans borrow cache resources; drop both replay paths before releasing
/// any of them, including on preparation/submission failure.
fn finish_submit(value: &mut t, deferred_items: Vec<deferred>, result: Result<bool, Error>) -> Result<bool, Error> {
    let retired_pending = {
        let r = value.retired.borrow();
        !r.retired_buffers.is_empty() || !r.retired_textures.is_empty()
    };
    if !deferred_items.is_empty() || retired_pending {
        drop_plans(value);
    }
    for item in deferred_items.iter().rev() {
        match item {
            deferred::Buffer(b) => {
                let _ = backend::destroy_buffer(b);
            }
            deferred::Texture(tx) => {
                let _ = backend::destroy_texture(tx);
            }
        }
    }
    drain_retired(&value.retired, &mut |b| drop(backend::destroy_buffer(b)), &mut |tx| drop(backend::destroy_texture(tx)));
    release_texture_staging(value);
    result
}

/// `prepared`: an identity and version under which this run is kept; `after_prepare` runs once the
/// draws are prepared (or refused); `clear` defaults to transparent black.
pub fn submit(
    prepared: Option<(String, i64)>,
    after_prepare: &mut dyn FnMut(),
    clear: Option<(f64, f64, f64, f64)>,
    value: &mut t,
    draws: &[draw],
) -> Result<bool, Error> {
    let clear = clear.unwrap_or((0.0, 0.0, 0.0, 0.0));
    if value.dead {
        after_prepare();
        return Err(failure_error("Renderer.submit", ErrorKind::StaleHandle, "renderer is destroyed"));
    }
    if draws.len() > prepared_scratch_capacity {
        after_prepare();
        return Err(failure_error("Renderer.submit", ErrorKind::Capacity, "one submission exceeds the bounded prepared-draw capacity"));
    }
    if let (Some((identity, version)), Some(cached)) = (&prepared, &value.prepared_submission) {
        if cached.submission_identity == *identity && cached.submission_version == *version && cached.submission_plan.replay_clear == clear {
            let plan = cached.submission_plan.clone();
            after_prepare();
            return replay_plan(value, &plan);
        }
    }
    let old = value.prepared_submission.take().map(|s| s.submission_plan);
    drop_plan(value, old);
    let prepared_key = prepared.clone();
    let draws = match resolve_prepared(value, &prepared, draws) {
        Ok(draws) => draws,
        Err(e) => {
            after_prepare();
            return Err(e);
        }
    };
    // An empty mesh (a zero-length line, a zero-radius circle) draws nothing: skip it instead of
    // failing the whole frame.
    let draws: Vec<draw> = draws.into_iter().filter(|entry| entry.draw.mesh.vertex_count > 0 && entry.draw.mesh.index_count > 0).collect();
    let valid_mesh = |mesh: &mesh| {
        !mesh_key_string(&mesh.key).is_empty()
            && mesh.vertex_count > 0
            && mesh.index_count > 0
            && valid_index_length(mesh)
            && (mesh.shared_vertices.is_none() || blen(&mesh.vertices)==0)
            && (mesh.vertex_bytes_len() + blen(&mesh.indices) > 0 || matches!(mesh.key, mesh_key::Gpu_vertices(_)))
    };
    let supported = draws.iter().all(|entry| find_pipeline(value, entry.family, entry.blend, entry.samples).is_some());
    let valid = draws.iter().all(|entry| {
        [1, 4, 9, 16].contains(&entry.samples)
            && valid_mesh(&entry.draw.mesh)
            && entry.texture.as_ref().is_none_or(|texture| valid_texture(texture))
            // a Scene2 mesh is untextured, a Scene2 quad samples its texture
            && match entry.family {
                Scene2 => entry.texture.is_none(),
                Scene2_textured => entry.texture.is_some(),
                _ => true,
            }
            && valid_vertex_attributes(entry)
            && entry
                .auxiliary
                .as_ref()
                .is_none_or(|source| !source.key.is_empty() && blen(&source.buffer) > 0 && valid_texture(&source.texture))
    });
    if !supported {
        after_prepare();
        return Err(failure_error("Renderer.submit", ErrorKind::Unsupported, "pipeline family/blend variant is unavailable"));
    }
    if !valid {
        after_prepare();
        return Err(failure_error("Renderer.submit", ErrorKind::InvalidArgument, "draw resource preflight failed"));
    }
    let mut deferred_items: Vec<deferred> = Vec::new();
    clear_prepared_scratch(&mut value.prepared_scratch);
    value.frame += 1;
    if let Err(e) = ensure_prepared_scratch(&mut value.prepared_scratch, draws.len()) {
        after_prepare();
        return finish_submit(value, deferred_items, Err(e));
    }
    let result = submit_prepared(value, &mut deferred_items, &prepared_key, clear, &draws, after_prepare);
    clear_prepared_scratch(&mut value.prepared_scratch);
    finish_submit(value, deferred_items, result)
}

struct draw_mode {
    scene2: bool,
    affine: bool,
    scene3_transform: bool,
    source: Option<bytes>,
}

fn submit_prepared(
    value: &mut t,
    defer: &mut Vec<deferred>,
    prepared_key: &Option<(String, i64)>,
    clear: (f64, f64, f64, f64),
    draws: &[draw],
    after_prepare: &mut dyn FnMut(),
) -> Result<bool, Error> {
    let modes: Vec<draw_mode> = draws
        .iter()
        .map(|entry| {
            let family = entry.family;
            let draw = &entry.draw;
            let scene2 = family == Scene2 || family == Scene2_textured;
            let affine = (family == Ui || family == Marks)
                && draw.state.transform_uniforms.as_ref().is_some_and(|b| blen(b) == 24 || blen(b) == 48);
            let scene3_transform = !scene2
                && draw.state.transform_uniforms.as_ref().is_some_and(|b| blen(b) >= 5456 && (blen(b) - 5456) % 192 == 0);
            // a Scene2 mesh binds constants; a quad's are its vertices
            let source = if family == Scene2 {
                Some(geometry_constants(draw.state.transform_uniforms.as_ref()))
            } else if scene2 {
                None
            } else if affine || scene3_transform {
                draw.state.transform_uniforms.clone()
            } else {
                None
            };
            draw_mode { scene2, affine, scene3_transform, source }
        })
        .collect();
    let sources: Vec<Option<bytes>> = modes.iter().map(|m| m.source.clone()).collect();
    let frame = match acquire(value) {
        Err(e) => {
            after_prepare();
            return Err(e);
        }
        Ok(acquisition::Skipped) => {
            after_prepare();
            return Ok(false);
        }
        Ok(acquisition::Acquired(frame)) => frame,
    };
    let uniform_slices = match prepare_uniforms(value, defer, &sources) {
        Ok(uniforms) => uniforms,
        Err(e) => {
            after_prepare();
            discard(&frame);
            return Err(e);
        }
    };
    if let Err(e) = prepare_all(value, defer, draws, &modes, &uniform_slices) {
        after_prepare();
        discard(&frame);
        return Err(e);
    }
    if let Err(e) = render_sun(value) {
        after_prepare();
        discard(&frame);
        return Err(e);
    }
    finish_prepared_frame(value, frame, clear, prepared_key, draws.len() as i64, uniform_slices, after_prepare)
}

fn prepare_all(
    value: &mut t,
    defer: &mut Vec<deferred>,
    draws: &[draw],
    modes: &[draw_mode],
    uniform_slices: &[Option<uniform_slice>],
) -> Result<(), Error> {
    for entry in draws {
        let position = value.prepared_scratch.scratch_length;
        let mode = &modes[position];
        let (family, draw) = (entry.family, &entry.draw);
        let gpu_vertices = resolve_gpu_vertices(value, entry)?;
        let cpu_mesh = match &gpu_vertices {
            None => draw.mesh.clone(),
            Some(source) if source.gpu_indices.is_some() => draw.mesh.clone(),
            Some(_) => mesh {
                key: mesh_key::Host(format!(
                    "gpu:indices:{}{}",
                    draw.mesh.vertex_count,
                    match family {
                        Scene3 | Scene3_points => ":scene3",
                        _ => "",
                    }
                )),
                ..draw.mesh.clone()
            },
        };
        let uniforms = if mode.scene2 || mode.affine || mode.scene3_transform { None } else { draw.state.transform_uniforms.as_ref() };
        let mesh_ = prepare(value, uniforms, mode.scene3_transform, &cpu_mesh)?;
        let attributes = match &entry.vertex_attributes {
            None => None,
            Some((_, b)) if blen(b) == 0 => None,
            Some((key, b)) => Some(prepare_storage(value, key, b)?),
        };
        let uniform = uniform_slices[position].clone();
        let texture = match &entry.texture {
            Some(source) => {
                let cached = prepare_texture(value, defer, source)?;
                Some((source.clone(), cached))
            }
            None => None,
        };
        let (auxiliary, environment) = match &entry.auxiliary {
            None => (None, None),
            Some(source) => {
                let buffer = prepare_auxiliary(value, source)?;
                let texture2 = prepare_texture(value, defer, &source.texture)?;
                let environment = match &source.environment {
                    None => None,
                    Some(environment) => Some((environment.clone(), prepare_texture(value, defer, environment)?)),
                };
                (Some((source.clone(), buffer, texture2)), environment)
            }
        };
        let slot = &mut value.prepared_scratch.scratch_slots[position];
        slot.slot_family = family;
        slot.slot_blend = entry.blend;
        slot.slot_samples = entry.samples;
        slot.slot_state = Some(draw.state.clone());
        slot.slot_texture = texture;
        slot.slot_auxiliary = auxiliary;
        slot.slot_environment = environment;
        slot.slot_mesh = Some(mesh_);
        slot.slot_uniform = uniform;
        slot.slot_vertex_attributes = attributes;
        slot.slot_gpu_vertices = gpu_vertices;
        value.prepared_scratch.scratch_length += 1;
    }
    Ok(())
}

pub fn replay_prepared_sampled_resources(
    clear: Option<(f64, f64, f64, f64)>,
    identity: &str,
    version: i64,
    value: &mut t,
) -> Result<Option<(bool, i64)>, Error> {
    let clear = clear.unwrap_or((0.0, 0.0, 0.0, 0.0));
    if value.dead {
        return Err(failure_error("Scene_execution.replay_prepared_sampled_resources", ErrorKind::StaleHandle, "renderer is destroyed"));
    }
    let cached = match &value.prepared_submission {
        Some(cached) if cached.submission_identity == identity && cached.submission_version == version && cached.submission_plan.replay_clear == clear => {
            Some((cached.submission_plan.clone(), cached.submission_draw_count))
        }
        _ => None,
    };
    match cached {
        Some((plan, count)) => replay_plan(value, &plan).map(|presented| Some((presented, count))),
        None => Ok(None),
    }
}

pub fn resize(value: &mut t, configuration: &Configuration) -> Result<(), Error> {
    let _ = settle(value);
    drop_plans(value);
    let mut samples: Vec<i64> = value.pipelines.iter().map(|variant| variant.samples).collect();
    samples.sort();
    samples.dedup();
    let target = allocate_target(&value.device, configuration)?;
    let mut attachments = scene_attachment_pool::create(&value.device, configuration, &samples);
    for (kind_, samples, passing, _) in scene_attachment_pool::allocated(&value.attachments) {
        if let Err(e) = scene_attachment_pool::acquire(&mut attachments, passing, kind_, samples) {
            let _ = backend::destroy_texture(&target);
            scene_attachment_pool::destroy(&mut attachments);
            return Err(e);
        }
    }
    if let Some(surface) = &value.surface {
        if let Err(e) = backend::configure(surface, configuration.clone()) {
            let _ = backend::destroy_texture(&target);
            scene_attachment_pool::destroy(&mut attachments);
            return Err(e);
        }
    }
    let old = std::mem::replace(&mut value.target, target);
    let mut old_attachments = std::mem::replace(&mut value.attachments, attachments);
    value.configuration = configuration.clone();
    scene_attachment_pool::destroy(&mut old_attachments);
    backend::destroy_texture(&old)
}

pub fn upload_bytes(value: &t) -> i64 {
    value.uploaded
}

pub mod Private {
    use super::*;

    pub use super::{create_gpu_vertices_owner, gpu_vertices, gpu_vertices_owner, register_gpu_vertices, unregister_gpu_vertices};

    pub fn uniform_capacity(value: &t) -> i64 {
        value.uniform_pages[0].as_ref().map_or(0, |page| page.page_capacity)
    }

    pub const uniform_shrink_after: i64 = uniform_shrink_after_frames;

    /// What the texture upload scratch holds, on the device and on the host each.
    pub fn staging_bytes(value: &t) -> i64 {
        value.texture_upload_scratch.as_ref().map_or(0, |scratch| scratch.scratch_size)
    }

    /// Whether the device has memoryless attachments, and how many the pool holds that ask for them.
    pub fn memoryless_attachments(value: &t) -> (bool, i64) {
        (
            caps::has(&backend::capabilities(&value.device), &Feature::MemorylessAttachments),
            scene_attachment_pool::allocated(&value.attachments).iter().filter(|(_, _, passing, _)| *passing).count() as i64,
        )
    }

    pub fn gpu_vertices_owner(value: &t) -> super::gpu_vertices_owner {
        value.gpu_vertices.clone()
    }

    /// A live, valid registration's buffer, extra buffers and vertex count (initial domain).
    pub fn find_gpu_vertices(owner: &super::gpu_vertices_owner, key: &str) -> Option<(Buffer, Vec<Buffer>, i64)> {
        let found = owner.borrow().gpu_vertex_registry.get(key).cloned();
        match found {
            Some(source) if gpu_vertices_valid(&source) => Some((source.gpu_buffer.clone(), source.gpu_extra.clone(), source.gpu_vertex_count)),
            _ => None,
        }
    }

    /// A live, valid registration's borrowed indices (count) and colours (width).
    pub fn find_gpu_streams(owner: &super::gpu_vertices_owner, key: &str) -> Option<(Option<(Buffer, i64)>, Option<(Buffer, i64)>)> {
        let found = owner.borrow().gpu_vertex_registry.get(key).cloned();
        match found {
            Some(source) if gpu_vertices_valid(&source) => Some((source.gpu_indices.clone(), source.gpu_colors.clone())),
            _ => None,
        }
    }

    pub fn gpu_vertex_count_for_test(owner: &super::gpu_vertices_owner) -> i64 {
        owner.borrow().gpu_vertex_registry.len() as i64
    }

    pub fn cache_count_for_report(value: &mut t) -> i64 {
        side_lru::length(&mut value.cache)
    }

    /// The number of draws the renderer encodes for these, after merging neighbours; -1 on error.
    pub fn coalesced_draws_for_test(draws: &[draw]) -> i64 {
        coalesce_draws(draws).map_or(-1, |draws| draws.len() as i64)
    }
}

/// Indirect-command replay plans: one automatic plan admitted after two identical frames and one
/// identity-keyed prepared plan.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct retained_stats {
    pub plan_hits: i64,
    pub plan_misses: i64,
}

pub fn retained_stats(value: &t) -> retained_stats {
    retained_stats { plan_hits: value.icb_stats.icb_hits, plan_misses: value.icb_stats.icb_misses }
}

pub fn pipeline_count(value: &t) -> i64 {
    value.pipelines.len() as i64
}

pub fn read_pixels(value: &mut t, bytes_per_row: i64) -> Result<Vec<u8>, Error> {
    settle(value)?;
    backend::read_texture(&value.target, bytes_per_row)
}

fn sorted_samplers(samplers: HashMap<String, Sampler>) -> Vec<(String, Sampler)> {
    let mut entries: Vec<(String, Sampler)> = samplers.into_iter().collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries
}

/// Every owned handle is released; the first failure is reported after the remaining releases and
/// device teardown have run.
pub fn destroy(value: &mut t) -> Result<(), Error> {
    if value.dead {
        return Ok(());
    }
    let _ = settle(value);
    value.dead = true;
    let mut first: Option<Error> = None;
    let record = |result: Result<(), Error>, first: &mut Option<Error>| {
        if let Err(e) = result {
            if first.is_none() {
                *first = Some(e);
            }
        }
    };
    let named = |name: &str, result: Result<(), Error>, first: &mut Option<Error>| {
        if let Err(mut e) = result {
            e.message = format!("{} ({})", e.message, name);
            if first.is_none() {
                *first = Some(e);
            }
        }
    };
    side_lru::clear(&mut value.prepared_cache);
    drop_plans(value);
    clear_prepared_scratch(&mut value.prepared_scratch);
    value.prepared_scratch.scratch_slots = Vec::new();
    for variant in &mut value.pipelines {
        if let Some(argument) = variant.argument.take() {
            record(backend::destroy_argument(&argument), &mut first);
        }
    }
    side_lru::clear(&mut value.cache);
    value.previous_uniforms = Vec::new();
    for page in value.uniform_pages.iter_mut() {
        if let Some(page) = page.take() {
            mem::release(uniform_owner(), page.page_capacity);
            named("uniform page", backend::destroy_buffer(&page.page_buffer), &mut first);
        }
    }
    side_lru::clear(&mut value.auxiliary_cache);
    side_lru::clear(&mut value.texture_cache);
    {
        let mut buffer_results: Vec<Result<(), Error>> = Vec::new();
        let mut texture_results: Vec<Result<(), Error>> = Vec::new();
        drain_retired(
            &value.retired,
            &mut |b| buffer_results.push(backend::destroy_buffer(b)),
            &mut |tx| texture_results.push(backend::destroy_texture(tx)),
        );
        for result in buffer_results {
            named("mesh or auxiliary", result, &mut first);
        }
        for result in texture_results {
            record(result, &mut first);
        }
    }
    if let Some(scratch) = value.texture_upload_scratch.take() {
        record(backend::destroy_buffer(&scratch.scratch_buffer), &mut first);
    }
    if let Some(pool) = value.arguments.take() {
        named("argument page", backend::destroy_buffer(&pool.argument_buffer), &mut first);
    }
    for (_, sampler) in sorted_samplers(std::mem::take(&mut value.samplers)) {
        record(backend::destroy_sampler(&sampler), &mut first);
    }
    if let Some(sun) = value.sun.take() {
        record(backend::destroy_texture(&sun.sun_color), &mut first);
        record(backend::destroy_texture(&sun.sun_depth), &mut first);
        record(backend::destroy_pipeline(&sun.sun_pipeline), &mut first);
    }
    scene_attachment_pool::destroy(&mut value.attachments);
    record(backend::destroy_texture(&value.target), &mut first);
    for variant in &value.pipelines {
        record(backend::destroy_pipeline(&variant.pipeline), &mut first);
    }
    if let Some(surface) = &value.surface {
        record(backend::destroy_surface(surface), &mut first);
    }
    record(backend::destroy_queue(&value.queue), &mut first);
    if value.owns_device {
        record(backend::destroy_device(&value.device), &mut first);
    }
    match first {
        Some(mut e) => {
            e.message = format!(
                "{}{}",
                e.message,
                match &value.icb_stats.icb_last_failure {
                    Some(text) => format!(" [plans: {}]", text),
                    None => String::new(),
                }
            );
            Err(e)
        }
        None => Ok(()),
    }
}

#[cfg(test)]
#[path = "test_prepared_scene3.rs"]
mod test_prepared_scene3;
