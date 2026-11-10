use crate::scene_attachment_pool;
use crate::side_lru::{self, Item, TableKey};
use ogpu_core::backend::{
    self, Argument, BatchDraw, Buffer, ColorAttachment, DepthAttachment, DepthState, Device, Icb,
    Pipeline, Queue, RenderTarget, ResourceUse, Sampler, ShaderStage, Surface, Texture,
};
use ogpu_core::caps::{self, Feature};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::pipeline::Blend;
use ogpu_core::render_pass::{Comparison, Cull, IndexType, Load, Primitive, Rect, Store};
use ogpu_core::surface::Configuration;
use ogpu_core::types::{
    BufferDescriptor, BufferUsage, Extent, SamplerDescriptor, TextureDescriptor, TextureFormat,
    TextureUsage,
};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, OnceLock};

/// Shared byte owners use Rc::ptr_eq where storage identity matters.
pub use runtime_resources::bytes;

pub fn new_bytes(value: Vec<u8>) -> bytes {
    Rc::new(value)
}

pub fn empty_bytes() -> bytes {
    new_bytes(Vec::new())
}

fn blen(value: &bytes) -> usize {
    value.len()
}



fn failure_error(operation: &str, kind: ErrorKind, text: &str) -> Error {
    error::make(operation, kind, text)
}

fn thread_id() -> std::thread::ThreadId {
    std::thread::current().id()
}

// ---------------------------------------------------------------------------------------------
// GPU vertices are borrowed, never uploaded or destroyed by the renderer.
// Registration owns only a bounded name and the producer's lifetime check.

pub struct GpuVerticesData {
    pub owner: GpuVerticesOwner,
    pub key: String,
    pub device: Device,
    pub buffer: Buffer,
    pub extra: Vec<Buffer>,
    pub vertex_count: i64,
    pub domain: std::thread::ThreadId,
    /// device-written index count: an indirect draw
    pub indirect: Option<(Buffer, i64)>,
    pub indices: Option<(Buffer, i64)>,
    pub colors: Option<(Buffer, i64)>,
    pub valid: Box<dyn Fn() -> bool>,
    pub active: Cell<bool>,
}

pub type GpuVertices = Rc<GpuVerticesData>;

pub struct GpuVerticesOwnerData {
    pub gpu_vertex_registry: HashMap<String, GpuVertices>,
    pub unregistered: i64,
}

pub type GpuVerticesOwner = Rc<RefCell<GpuVerticesOwnerData>>;

pub fn create_gpu_vertices_owner() -> GpuVerticesOwner {
    Rc::new(RefCell::new(GpuVerticesOwnerData { gpu_vertex_registry: HashMap::new(), unregistered: 0 }))
}

const GPU_VERTEX_CAPACITY: usize = 128;

/// The ids that identify a source's buffers in signatures and fingerprints.
pub fn gpu_identity(source: &GpuVertices) -> (String, i64, Vec<i64>) {
    let mut ids: Vec<i64> = source.extra.iter().map(backend::buffer_id).collect();
    ids.extend(source.indirect.iter().map(|(buffer, _)| backend::buffer_id(buffer)));
    ids.extend(source.indices.iter().map(|(buffer, _)| backend::buffer_id(buffer)));
    ids.extend(source.colors.iter().map(|(buffer, _)| backend::buffer_id(buffer)));
    (source.key.clone(), backend::buffer_id(&source.buffer), ids)
}

pub fn gpu_vertices_valid(value: &GpuVertices) -> bool {
    value.active.get() && value.domain == thread_id() && (value.valid)()
}

/// `register_gpu_vertices owner ?extra ?stride ?indirect ?indices ?colors ~key ~device ~buffer
/// ~vertex_count ~valid ()`; `stride` defaults to 16, `extra` to none.
pub fn register_gpu_vertices(
    owner: &GpuVerticesOwner,
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
) -> Result<GpuVertices, Error> {
    let operation = "Scene_execution.register_gpu_vertices";
    let extra = extra.unwrap_or_default();
    let stride = stride.unwrap_or(16);
    let registry_has = owner.borrow().gpu_vertex_registry.contains_key(key);
    let invalid = key.is_empty()
        || vertex_count <= 0
        || vertex_count > i32::MAX as i64
        || stride <= 0
        || (extra.is_empty() && vertex_count.checked_mul(stride).is_none_or(|n| backend::buffer_size(buffer) < n))
        || std::iter::once(buffer).chain(extra.iter()).any(|b| backend::buffer_size(b) <= 0)
        || match &indirect {
            Some((b, offset)) => *offset < 0 || offset.checked_add(20).is_none_or(|n| n > backend::buffer_size(b)),
            None => false,
        }
        || match &indices {
            Some((b, count)) => {
                *count <= 0
                    || *count > u32::MAX as i64
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
    if owner.borrow().gpu_vertex_registry.len() >= GPU_VERTEX_CAPACITY {
        return Err(failure_error(operation, ErrorKind::Capacity, "borrowed vertex registration capacity reached"));
    }
    let value: GpuVertices = Rc::new(GpuVerticesData {
        owner: owner.clone(),
        key: key.to_owned(),
        device: device.clone(),
        buffer: buffer.clone(),
        extra,
        vertex_count,
        indirect,
        indices,
        colors,
        domain: thread_id(),
        valid,
        active: Cell::new(true),
    });
    owner.borrow_mut().gpu_vertex_registry.insert(key.to_owned(), value.clone());
    Ok(value)
}

pub fn unregister_gpu_vertices(value: &GpuVertices) {
    if value.domain != thread_id() {
        unreachable!("GPU vertices closed on another domain");
    }
    if value.active.get() {
        value.active.set(false);
        let mut owner = value.owner.borrow_mut();
        owner.unregistered += 1;
        owner.gpu_vertex_registry.remove(&value.key);
    }
}

/// GPU vertices and renderer-owned indices have independent identities.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MeshKey {
    Host(String),
    GpuVertices { name: Rc<String>, indices: Option<Rc<String>> },
}

pub fn mesh_key_string(key: &MeshKey) -> String {
    match key {
        MeshKey::Host(k) => k.clone(),
        MeshKey::GpuVertices { indices: Some(indices), .. } => format!("gpu:indices:{indices}"),
        MeshKey::GpuVertices { name, indices: None } => format!("gpu:vertices:{name}"),
    }
}

#[derive(Clone)]
pub struct Mesh {
    pub key: MeshKey,
    pub vertices: bytes,
    /// Immutable published UI vertices; when present, `vertices` must be empty.
    pub shared_vertices: Option<Arc<Vec<u8>>>,
    pub vertex_count: i64,
    pub indices: bytes,
    pub index_count: i64,
    pub primitive: Primitive,
}

impl Mesh {
    pub fn vertex_bytes_len(&self) -> usize {
        self.shared_vertices.as_ref().map_or_else(|| blen(&self.vertices), |bytes| bytes.len())
    }
    pub fn retained_vertex_bytes(&self) -> i64 {
        (self.vertices.capacity() as i64).saturating_add(self.shared_vertices.as_ref().map_or(0, |bytes| bytes.capacity() as i64))
    }
}

/// Structural scene equality compares bytes by content and floats by value.
#[derive(Clone, PartialEq)]
pub struct State {
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
pub struct GeometryDraw {
    pub mesh: Mesh,
    pub state: State,
}

/// Exact number of family/blend variants required for each supported sample count.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PipelineFamily {
    Scene2,
    Scene2Textured,
    Scene3,
    Scene3Points,
    Scene3Textured,
    Scene3World,
    Ui,
    Marks,
}
use PipelineFamily::*;

/// Every level holds `width * height` texels of 4, 8 or 16 bytes (the size level 0 implies):
/// `Rgba8_unorm`, `Rgba16_float` or `Rgba32_float`.
#[derive(Clone)]
pub struct TextureLevel {
    pub width: i64,
    pub height: i64,
    pub bytes: bytes,
}

/// `levels` is an `Rc` so that "the immutable levels this slot was uploaded from" stays a
/// physical-identity test.
#[derive(Clone)]
pub struct SampledTexture {
    pub key: String,
    pub levels: Rc<Vec<TextureLevel>>,
    pub sampler: SamplerDescriptor,
    pub gpu: Option<Texture>,
}

#[derive(Clone)]
pub struct AuxiliaryResource {
    pub key: String,
    pub buffer: bytes,
    pub texture: SampledTexture,
    /// A `Scene3_world` draw's camera map (texture 9, sampler 10).
    pub environment: Option<SampledTexture>,
    /// `Some key`: this World block (sun view-projection at words 40..55) samples the renderer's
    /// sun map at texture 8, rendered from the frame's depth-writing `Replace`/`Alpha` World
    /// triangle draws with the same key whenever the key differs from the last rendered one.
    pub sun_shadow: Option<String>,
}

/// `texture` and `auxiliary` are `Rc`s: draws are merged only when they share the same ones.
#[derive(Clone)]
pub struct SampledDraw {
    pub family: PipelineFamily,
    pub blend: Blend,
    pub texture: Option<Rc<SampledTexture>>,
    pub auxiliary: Option<Rc<AuxiliaryResource>>,
    /// Independently retained Scene3 color/UV stream (12 bytes per vertex), bound at vertex buffer
    /// 11. Required (and only valid) for the Scene3 families; 2D, UI and Marks draws carry `None`.
    pub vertex_attributes: Option<(String, bytes)>,
    pub samples: i64,
    pub draw: GeometryDraw,
}

/// Retain draws by shared owner identity.
pub type Draw = Rc<SampledDraw>;

pub type Scene3Entry = Draw;

pub struct PreparedScene3 {
    pub entries: Vec<Draw>,
}

/// A draw names a borrowed stream that was unregistered: it can never be drawn again.
pub fn borrows_unregistered(owner: &GpuVerticesOwner, prepared: &PreparedScene3) -> bool {
    prepared.entries.iter().any(|entry| match &entry.draw.mesh.key {
        MeshKey::GpuVertices { name, .. } => !owner.borrow().gpu_vertex_registry.contains_key(name.as_str()),
        MeshKey::Host(_) => false,
    })
}

struct Cached {
    key: String,
    payload_hash: Option<u128>,
    trusted_source: Option<Mesh>,
    buffer: Buffer,
    index_offset: i64,
    uniform_offset: Option<i64>,
    uniform_copy: Vec<u8>,
    vertex_count: i64,
    index_count: i64,
    primitive: Primitive,
    bytes: i64,
}
type CachedRef = Rc<RefCell<Cached>>;

#[derive(Clone)]
struct UniformSlice {
    buffer: Buffer,
    offset: i64,
    /// The frame's staging bytes and this slice's window into them, kept for next frame's
    /// unchanged-set comparison without a per-draw copy.
    staging: Rc<Vec<u8>>,
    staging_offset: usize,
    length: usize,
}

struct UniformPage {
    buffer: Buffer,
    capacity: i64,
}

struct CachedAuxiliary {
    copy: bytes,
    buffer: Buffer,
    /// The bytes this entry was uploaded from: a physically equal source is a hit without comparing
    /// (World blocks can carry a whole shadow map).
    source: bytes,
}
type CachedAuxiliaryRef = Rc<RefCell<CachedAuxiliary>>;

struct CachedTexture {
    key: String,
    hash: Option<u128>,
    shape: String,
    texture: Texture,
    bytes: i64,
    used: i64,
    levels: Rc<Vec<TextureLevel>>,
}
type CachedTextureRef = Rc<RefCell<CachedTexture>>;

struct TextureUploadScratch {
    buffer: Buffer,
    bytes: Vec<u8>,
    size: i64,
}

struct PreparedRun {
    version: i64,
    draws: Vec<Draw>,
    bytes: i64,
}

#[derive(Clone, PartialEq)]
struct AutomaticSignature {
    family: PipelineFamily,
    blend: Blend,
    samples: i64,
    state: State,
    texture: Option<(i64, SamplerDescriptor)>,
    auxiliary: Option<(i64, i64, SamplerDescriptor)>,
    environment: Option<(i64, SamplerDescriptor)>,
    buffer: i64,
    index_offset: i64,
    gpu_vertices: Option<(String, i64, Vec<i64>)>,
    vertex_attributes: Option<i64>,
    uniform_buffer: Option<i64>,
    uniform_offset: Option<i64>,
    vertex_count: i64,
    index_count: i64,
}

#[derive(Clone)]
struct PlanDraw {
    draw: BatchDraw,
    textures: Vec<(ShaderStage, i64, Texture)>,
    samplers: Vec<(ShaderStage, i64, Sampler)>,
    /// Textures reached only through an argument buffer: Metal needs them made resident with
    /// `use_resources` before any draw of the pass.
    resident: Vec<Texture>,
    /// A borrowed shape whose index count the device wrote: drawn indirect, never batched or recorded
    indirect: Option<(Buffer, i64)>,
}

/// One render pass of a frame: its attachment state and draws. A retained batch replays an
/// indirect command buffer instead of re-encoding.
struct BatchPlan {
    family: PipelineFamily,
    samples: i64,
    state: State,
    first: bool,
    draws: Vec<PlanDraw>,
    /// `batch_draws` as the backend batch, built once with the plan.
    driver_draws: Vec<BatchDraw>,
    resources: Vec<ResourceUse>,
    icb: RefCell<Option<Icb>>,
}

struct ReplayPlan {
    clear: (f64, f64, f64, f64),
    payloads: Vec<AutomaticSignature>,
    batches: Vec<Rc<BatchPlan>>,
    gpu_vertices: Vec<GpuVertices>,
}

struct PreparedSubmission {
    identity: String,
    version: i64,
    draw_count: i64,
    plan: Rc<ReplayPlan>,
}

struct AutomaticCandidate {
    clear: (f64, f64, f64, f64),
    payloads: Vec<AutomaticSignature>,
}

struct PreparedSlot {
    family: PipelineFamily,
    blend: Blend,
    samples: i64,
    state: Option<State>,
    texture: Option<(Rc<SampledTexture>, CachedTextureRef)>,
    auxiliary: Option<(Rc<AuxiliaryResource>, CachedAuxiliaryRef, CachedTextureRef)>,
    environment: Option<(SampledTexture, CachedTextureRef)>,
    mesh: Option<CachedRef>,
    uniform: Option<UniformSlice>,
    gpu_vertices: Option<GpuVertices>,
    vertex_attributes: Option<CachedAuxiliaryRef>,
}

struct PreparedScratch {
    slots: Vec<PreparedSlot>,
    length: usize,
}

struct PipelineVariant {
    family: PipelineFamily,
    blend: Blend,
    samples: i64,
    pipeline: Pipeline,
    argument: Option<Argument>,
}

struct IcbStats {
    builds: i64,
    hits: i64,
    misses: i64,
    evictions: i64,
    executions: i64,
    failures: i64,
    last_failure: Option<String>,
}

struct ArgumentPool {
    buffer: Buffer,
    stride: i64,
    capacity: i64,
    slices: HashMap<(i64, u64), i64>,
    next: i64,
}

#[derive(Default)]
struct Retired {
    buffers: Vec<Buffer>,
    textures: Vec<Texture>,
}

const CACHE_BYTE_CAPACITY: i64 = 256 * 1024 * 1024;
const MESH_CACHE_ENTRY_CAPACITY: i64 = 256;
const TEXTURE_CACHE_ENTRY_CAPACITY: i64 = 256;
const TEXTURE_CACHE_BYTE_CAPACITY: i64 = 256 * 1024 * 1024;

fn device_cache(name: &str) -> lru::Owner {
    lru::cache_in(lru::Pool::Device, name)
}

fn mesh_table(retired: &Rc<RefCell<Retired>>) -> side_lru::Table {
    let retired = retired.clone();
    side_lru::create(
        device_cache("renderer.mesh"),
        true,
        CACHE_BYTE_CAPACITY,
        Box::new(move |_, item| {
            let item = cached_of(&item);
            retired.borrow_mut().buffers.push(item.borrow().buffer.clone());
        }),
        MESH_CACHE_ENTRY_CAPACITY,
    )
}

fn texture_table(retired: &Rc<RefCell<Retired>>) -> side_lru::Table {
    let retired = retired.clone();
    side_lru::create(
        device_cache("renderer.texture"),
        true,
        TEXTURE_CACHE_BYTE_CAPACITY,
        Box::new(move |_, item| {
            let item = cached_texture_of(&item);
            retired.borrow_mut().textures.push(item.borrow().texture.clone());
        }),
        TEXTURE_CACHE_ENTRY_CAPACITY,
    )
}

fn auxiliary_table(retired: &Rc<RefCell<Retired>>) -> side_lru::Table {
    let retired = retired.clone();
    side_lru::create(
        device_cache("renderer.gpu_item"),
        true,
        CACHE_BYTE_CAPACITY,
        Box::new(move |_, item| {
            let item = cached_auxiliary_of(&item);
            retired.borrow_mut().buffers.push(item.borrow().buffer.clone());
        }),
        64,
    )
}

fn cached_of(item: &Item) -> CachedRef {
    item.clone().downcast::<RefCell<Cached>>().unwrap_or_else(|_| unreachable!("mesh table item"))
}

fn cached_texture_of(item: &Item) -> CachedTextureRef {
    item.clone().downcast::<RefCell<CachedTexture>>().unwrap_or_else(|_| unreachable!("texture table item"))
}

fn cached_auxiliary_of(item: &Item) -> CachedAuxiliaryRef {
    item.clone().downcast::<RefCell<CachedAuxiliary>>().unwrap_or_else(|_| unreachable!("auxiliary table item"))
}

fn prepared_run_of(item: &Item) -> Rc<PreparedRun> {
    item.clone().downcast::<PreparedRun>().unwrap_or_else(|_| unreachable!("prepared table item"))
}

fn skey(key: &str) -> TableKey {
    TableKey::Str(key.to_owned())
}

/// Evicted GPU resources are destroyed once the frame that evicted them has been submitted.
fn drain_retired(retired: &Rc<RefCell<Retired>>, destroy_buffer: &mut dyn FnMut(&Buffer), destroy_texture: &mut dyn FnMut(&Texture)) {
    let (buffers, textures) = {
        let mut r = retired.borrow_mut();
        (std::mem::take(&mut r.buffers), std::mem::take(&mut r.textures))
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
struct SunMap {
    pipeline: Pipeline,
    color: Texture,
    depth: Texture,
    key: String,
}

pub type SunFactory = Rc<dyn Fn(&Device) -> Result<Pipeline, Error>>;

pub struct SceneExecution {
    device: Device,
    gpu_vertices: GpuVerticesOwner,
    queue: Queue,
    surface: Option<Surface>,
    target: Texture,
    configuration: Configuration,
    attachments: scene_attachment_pool::AttachmentPool,
    pipelines: Vec<PipelineVariant>,
    pipeline_lookup: Vec<Option<usize>>,
    cache: side_lru::Table,
    uniform_pages: Vec<Option<UniformPage>>,
    next_uniform_page: usize,
    /// frames in a row needing under a quarter of the page
    uniform_low: i64,
    previous_uniforms: Vec<Option<UniformSlice>>,
    prepared_cache: side_lru::Table,
    prepared_submission: Option<PreparedSubmission>,
    automatic_submission: Option<Rc<ReplayPlan>>,
    automatic_candidate: Option<AutomaticCandidate>,
    prepared_scratch: PreparedScratch,
    auxiliary_cache: side_lru::Table,
    texture_cache: side_lru::Table,
    /// Evicted GPU resources wait here until the frame that evicted them has been submitted.
    retired: Rc<RefCell<Retired>>,
    frame: i64,
    texture_upload_scratch: Option<TextureUploadScratch>,
    /// the last frame that uploaded a texture
    staged_frame: i64,
    /// and the frame before it did too
    staged_stream: bool,
    samplers: Vec<(SamplerDescriptor, Sampler)>,
    arguments: Option<ArgumentPool>,
    icb_stats: IcbStats,
    uploaded: i64,
    swept_unregistered: i64,
    sun_factory: Option<SunFactory>,
    sun: Option<SunMap>,
    sun_passes: i64,
    in_flight: Option<i64>,
    dead: bool,
    /// A borrowed device (shared with a window) is never destroyed here.
    owns_device: bool,
}

fn drop_plan(value: &mut SceneExecution, plan: Option<Rc<ReplayPlan>>) {
    let Some(plan) = plan else { return };
    for batch in &plan.batches {
        let icb = batch.icb.borrow_mut().take();
        if let Some(icb) = icb {
            value.icb_stats.evictions += 1;
            if let Err(failure) = backend::destroy_icb(&icb) {
                value.icb_stats.failures += 1;
                value.icb_stats.last_failure = Some(format!("destroy: {}", error::to_string(&failure)));
            }
        }
    }
}

fn drop_plans(value: &mut SceneExecution) {
    let automatic = value.automatic_submission.take();
    drop_plan(value, automatic);
    let prepared = value.prepared_submission.take().map(|s| s.plan);
    drop_plan(value, prepared);
    value.automatic_candidate = None;
}

pub fn device(value: &SceneExecution) -> Device {
    value.device.clone()
}
pub fn queue(value: &SceneExecution) -> Queue {
    value.queue.clone()
}
/// The owned RGBA8 texture every submission renders into; a shared-device renderer's callers may
/// sample it directly between completed submissions.
pub fn target(value: &SceneExecution) -> Texture {
    value.target.clone()
}

fn same_device(a: &Device, b: &Device) -> bool {
    ogpu_core::handle::device_id(backend::device_handle(a)) == ogpu_core::handle::device_id(backend::device_handle(b))
}

fn is_scene3_family(family: PipelineFamily) -> bool {
    matches!(family, Scene3 | Scene3Points | Scene3World)
}

fn resolve_gpu_vertices(value: &SceneExecution, entry: &SampledDraw) -> Result<Option<GpuVertices>, Error> {
    let mesh = &entry.draw.mesh;
    let name = match &mesh.key {
        MeshKey::Host(_) => return Ok(None),
        MeshKey::GpuVertices { name, .. } => name,
    };
    let found = value.gpu_vertices.borrow().gpu_vertex_registry.get(name.as_str()).cloned();
    match found {
        Some(source) if gpu_vertices_valid(&source) => {
            let color_width = match &entry.draw.state.transform_uniforms {
                Some(bytes_) if blen(bytes_) >= 82 * 4 => {
                    let b = bytes_;
                    f32::from_bits(u32::from_le_bytes(b[81 * 4..81 * 4 + 4].try_into().unwrap())) as f64
                }
                _ => 0.0,
            };
            let family = entry.family;
            let indices_len = blen(&mesh.indices) as i64;
            let vertices_len = mesh.vertex_bytes_len() as i64;
            if !same_device(&source.device, &value.device) {
                return Err(failure_error("Renderer.submit", ErrorKind::CrossDevice, "GPU vertices belong to another device"));
            }
            let colors_bad = (source.colors.is_some() && !is_scene3_family(family))
                || (is_scene3_family(family)
                    && color_width
                        != match (&source.colors, &entry.vertex_attributes) {
                            (Some((_, width)), _) => *width as f64,
                            (None, Some((_, b))) if blen(b) == 0 => -1.0,
                            (None, _) => 0.0,
                        });
            let indices_bad = match &source.indices {
                Some((_, count)) => !is_scene3_family(family) || indices_len != 0 || mesh.index_count != *count,
                None => indices_len != mesh.index_count * 4,
            };
            let layout_bad = match (family, source.extra.len()) {
                (Scene3 | Scene3Points | Scene3World, 0 | 1) => {
                    // a device's packed float3 positions (and normals)
                    vertices_len != 0 || mesh.vertex_count != source.vertex_count
                }
                (Ui, 0) | (Marks, 5) => {
                    vertices_len != 0
                        || mesh.vertex_count != source.vertex_count
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
fn valid_vertex_attributes(entry: &SampledDraw) -> bool {
    match (entry.family, &entry.vertex_attributes) {
        (Scene2 | Scene2Textured | Ui | Marks, None) => true,
        (Scene2 | Scene2Textured | Ui | Marks, Some(_)) | (Scene3 | Scene3Points | Scene3Textured | Scene3World, None) => false,
        (_, Some((key, attributes))) => {
            let mesh = &entry.draw.mesh;
            !key.is_empty()
                && mesh.vertex_count >= 0
                && mesh.vertex_count.checked_mul(24).is_some()
                && match &mesh.key {
                    MeshKey::GpuVertices { .. } => mesh.vertex_bytes_len() == 0,
                    MeshKey::Host(_) => mesh.vertex_bytes_len() as i64 == mesh.vertex_count * 24,
                }
                && (blen(attributes) as i64 == mesh.vertex_count * 12
                    || match &mesh.key {
                        MeshKey::GpuVertices { .. } => blen(attributes) == 0,
                        MeshKey::Host(_) => false,
                    })
        }
    }
}

fn valid_index_length(mesh: &Mesh) -> bool {
    mesh.index_count >= 0
        && mesh.index_count.checked_mul(4).is_some()
        && (blen(&mesh.indices) as i64 == mesh.index_count * 4
            || match &mesh.key {
                MeshKey::GpuVertices { .. } => blen(&mesh.indices) == 0,
                MeshKey::Host(_) => false,
            })
}

/// Validates and copies a native Scene3 submission description. Nested byte resources remain
/// borrowed and must be retained by the staging owner.
pub fn prepare_scene3(clear: (f64, f64, f64, f64), clear_depth: f64, entries: &[Draw]) -> Result<PreparedScene3, Error> {
    let (r, g, b, a) = clear;
    let valid_extent = |(_, _, width, height): (i64, i64, i64, i64)| width > 0 && height > 0;
    let valid_family = |entry: &SampledDraw| match (entry.family, &entry.texture, &entry.auxiliary) {
        (Scene3, None, None) | (Scene3Points, None, None) => true,
        (Scene3Textured, Some(_), None) => true,
        (Scene3World, Some(_), Some(auxiliary)) => auxiliary.environment.is_some(),
        _ => false,
    };
    let valid_entry = |entry: &SampledDraw| {
        let mesh = &entry.draw.mesh;
        entry.samples > 0
            && [1, 4, 9, 16].contains(&entry.samples)
            && valid_family(entry)
            && !mesh_key_string(&mesh.key).is_empty()
            && mesh.vertex_count >= 0
            && mesh.index_count >= 0
            && (mesh.shared_vertices.is_none() || blen(&mesh.vertices)==0)
            && ((mesh.primitive == Primitive::PointList) == (entry.family == Scene3Points))
            && match mesh.primitive {
                Primitive::LineList => mesh.index_count % 2 == 0,
                Primitive::LineStrip => mesh.index_count >= 2,
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
        Ok(PreparedScene3 { entries: entries.to_vec() })
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

const BLENDS: [Blend; 6] = [Blend::Replace, Blend::Alpha, Blend::Add, Blend::Multiply, Blend::Screen, Blend::Subtract];

const FAMILIES: [PipelineFamily; 8] = [Scene2, Scene2Textured, Scene3, Scene3Points, Scene3Textured, Scene3World, Ui, Marks];

const BLEND_COUNT: i64 = 6;
const PIPELINE_VARIANTS_PER_SAMPLE: i64 = 8 * BLEND_COUNT;

fn pipeline_slot(family: PipelineFamily, blend: Blend, samples: i64) -> i64 {
    let family = match family {
        Scene2 => 0,
        Scene2Textured => 1,
        Scene3 => 2,
        Scene3Points => 3,
        Scene3Textured => 4,
        Ui => 5,
        Scene3World => 6,
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
    if samples < 0 { -1 } else { ((family * BLEND_COUNT + blend) * 4) + samples }
}

fn find_pipeline(value: &SceneExecution, family: PipelineFamily, blend: Blend, samples: i64) -> Option<usize> {
    let slot = pipeline_slot(family, blend, samples);
    if slot < 0 { None } else { value.pipeline_lookup[slot as usize] }
}

fn sample_counts(device: &Device) -> Vec<i64> {
    let limits = backend::capabilities(device).limits;
    [1, 4, 9, 16].into_iter().filter(|samples| limits.supports_samples(*samples)).collect()
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
    gpu_vertices: Option<GpuVerticesOwner>,
    sun_depth: Option<SunFactory>,
    offscreen: bool,
    driver: Box<dyn backend::Driver>,
    configuration: &Configuration,
    make: &dyn Fn(&Device, PipelineFamily, Blend, i64) -> Result<Pipeline, Error>,
) -> Result<SceneExecution, Error> {
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
    let mut pipelines: Vec<PipelineVariant> = Vec::new();
    for family in FAMILIES {
        for blend in BLENDS {
            for sample in &samples {
                match make(&device, family, blend, *sample) {
                    Ok(pipeline) => pipelines.push(PipelineVariant { family, blend, samples: *sample, pipeline, argument: None }),
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
            let mut pipeline_lookup: Vec<Option<usize>> = vec![None; (PIPELINE_VARIANTS_PER_SAMPLE * 4) as usize];
            for (index, variant) in pipelines.iter().enumerate() {
                let slot = pipeline_slot(variant.family, variant.blend, variant.samples);
                if slot >= 0 {
                    pipeline_lookup[slot as usize] = Some(index);
                }
            }
            let retired = Rc::new(RefCell::new(Retired::default()));
            Ok(SceneExecution {
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
                prepared_cache: side_lru::create(lru::cache("renderer.prepared"), false, CACHE_BYTE_CAPACITY, Box::new(|_, _| {}), 64),
                prepared_submission: None,
                automatic_submission: None,
                automatic_candidate: None,
                prepared_scratch: PreparedScratch { slots: Vec::new(), length: 0 },
                auxiliary_cache: auxiliary_table(&retired),
                texture_cache: texture_table(&retired),
                retired,
                texture_upload_scratch: None,
                staged_frame: -1,
                staged_stream: false,
                samplers: Vec::new(),
                arguments: None,
                icb_stats: IcbStats {
                    builds: 0,
                    hits: 0,
                    misses: 0,
                    evictions: 0,
                    executions: 0,
                    failures: 0,
                    last_failure: None,
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

const HASH_SEED: u128 = (rays_math::hash::SEED as u128) << 64 | rays_math::hash::SEED2 as u128;

/// Length-framed parts keep component boundaries distinct without temporary strings.
fn digest(hash: u128, data: &[u8]) -> u128 {
    let a = rays_math::hash::bytes((hash >> 64) as u64, data);
    let b = rays_math::hash::bytes(hash as u64, data);
    (u128::from(a) << 64) | u128::from(b)
}

fn source_for_trust(mesh: &Mesh, bytes: i64) -> Option<Mesh> {
    // Keep extra CPU retention within the already bounded GPU payload budget, even when an indexed
    // mesh contains a large unused vertex plane.
    let (v, i) = (mesh.retained_vertex_bytes(), mesh.indices.capacity() as i64);
    if i <= bytes && v <= bytes - i { Some(mesh.clone()) } else { None }
}

fn same_bytes(mesh: &Mesh, source: &Option<Mesh>) -> bool {
    match source {
        Some(source) => {
            (match (&source.shared_vertices, &mesh.shared_vertices) {
                (Some(a), Some(b)) => Arc::ptr_eq(a,b),
                (None, None) => Rc::ptr_eq(&source.vertices,&mesh.vertices)
                    || (source.vertices.is_empty() && mesh.vertices.is_empty()),
                _ => false,
            })
                && Rc::ptr_eq(&source.indices, &mesh.indices)
                && source.vertex_count == mesh.vertex_count
                && source.index_count == mesh.index_count
        }
        None => false,
    }
}

fn vertex_hash(mesh: &Mesh) -> u128 {
    let owned=&mesh.vertices;
    let vertices=mesh.shared_vertices.as_ref().map_or(&owned[..], |bytes| bytes.as_slice());
    digest(HASH_SEED, vertices)
}

fn content_hash(mesh: &Mesh, uniform_bytes: &[u8]) -> u128 {
    digest(digest(vertex_hash(mesh), &mesh.indices), uniform_bytes)
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

fn scratch_mem_mesh(scratch: &PreparedScratch, item: &CachedRef) -> bool {
    scratch.slots[..scratch.length]
        .iter()
        .any(|slot| slot.mesh.as_ref().is_some_and(|candidate| Rc::ptr_eq(candidate, item)))
}

/// One entry per key. A hit needs no hashing: vertex-stable keys carry their identity, other draws
/// keep the exact source bytes they were uploaded from and compare them physically. Content hashing
/// happens once per upload and only decides a same-key miss for fresh but equal bytes.
fn prepare(value: &mut SceneExecution, uniforms: Option<&bytes>, vertex_stable: bool, mesh: &Mesh) -> Result<CachedRef, Error> {
    let uniform_bytes: Vec<u8> = uniforms.map(|b| b.to_vec()).unwrap_or_default();
    let valid_uniforms = uniforms.is_none_or(|bytes_| valid_uniform_bytes(&bytes_));
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
                if let TableKey::Str(cached_key) = cached_key
                    && let Some(name) = cached_key.strip_prefix(prefix)
                        && !owner.gpu_vertex_registry.contains_key(name) {
                            stale.push(cached_key.clone());
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
            let hash = item.borrow().payload_hash;
            if hash.is_some_and(|hash| hash == content_hash(mesh, &uniform_bytes)) {
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
    value: &mut SceneExecution,
    uniforms: Option<&bytes>,
    uniform_bytes: &[u8],
    valid_uniforms: bool,
    vertex_stable: bool,
    mesh: &Mesh,
    key: &str,
    existing: Option<CachedRef>,
) -> Result<CachedRef, Error> {
    let owned_vertices = &mesh.vertices;
    let vertices = mesh.shared_vertices.as_ref().map_or(&owned_vertices[..], |bytes| bytes.as_slice());
    let indices = &mesh.indices;
    let (vertex_count, index_count) = (mesh.vertex_count, mesh.index_count);
    let operation = "Scene_execution.prepare";
    let vertex_index_bytes = vertices.len().checked_add(indices.len())
        .ok_or_else(|| failure_error(operation, ErrorKind::Capacity, "mesh storage exceeds native address space"))?;
    let total = vertex_index_bytes.checked_add(uniform_bytes.len()).and_then(|n| i64::try_from(n).ok())
        .ok_or_else(|| failure_error(operation, ErrorKind::Capacity, "mesh storage exceeds native address space"))?;
    if mesh_key_string(&mesh.key).is_empty()
        || vertex_count <= 0
        || index_count <= 0
        || (total == 0 && matches!(mesh.key, MeshKey::Host(_)))
        || !valid_uniforms
    {
        return Err(failure_error("Scene_execution.prepare", ErrorKind::InvalidArgument, "mesh payload or transform uniforms are malformed"));
    }
    let payload_hash = if vertex_stable { None } else { Some(content_hash(mesh, uniform_bytes)) };
    let uniform_offset = uniforms.map(|_| vertex_index_bytes as i64);
    let mut uniform_copy = runtime_resources::byte_buffer(operation, uniform_bytes.len())?;
    uniform_copy.extend_from_slice(uniform_bytes);
    let reserved = |item: &CachedRef| scratch_mem_mesh(&value.prepared_scratch, item);
    let reusable: Option<CachedRef> = match &existing {
        Some(item) if item.borrow().bytes == total && !reserved(item) => Some(item.clone()),
        _ if side_lru::length(&mut value.cache) >= MESH_CACHE_ENTRY_CAPACITY => {
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
            let mut packed = runtime_resources::byte_buffer(operation, total as usize)?;
            packed.extend_from_slice(vertices);
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
            write(0, vertices)?;
            write(offset, &indices)?;
            if !uniform_bytes.is_empty() {
                write(offset + indices.len() as i64, uniform_bytes)?;
            }
            let item: CachedRef = Rc::new(RefCell::new(Cached {
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

const UNIFORM_PAGE_BYTE_CAPACITY: i64 = 256 * 1024 * 1024;

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
            let b = bytes_;
            for index in 0..6 {
                put(index, f32_at(&b, index));
            }
        }
        Some(bytes_) => {
            let b = bytes_;
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

const UNIFORM_SHRINK_AFTER_FRAMES: i64 = 240;

/// Deferred destruction: valid through the synchronous submission that evicted it.
enum Deferred {
    Buffer(Buffer),
    Texture(Texture),
}

fn prepare_uniforms(value: &mut SceneExecution, defer: &mut Vec<Deferred>, sources: &[Option<bytes>]) -> Result<Vec<Option<UniformSlice>>, Error> {
    let operation = "Scene_execution.prepare_uniforms";
    // one page per plan S6: a frame's needs far under it for long enough bring it back down, also
    // when the uniforms stop changing
    let capacity = value.uniform_pages[value.next_uniform_page].as_ref().map_or(0, |page| page.capacity);
    let wanted: i64 = sources.iter().map(|s| s.as_ref().map_or(0, |b| align256(blen(b) as i64))).sum();
    value.uniform_low = if capacity > 65536 && wanted * 4 < capacity { value.uniform_low + 1 } else { 0 };
    let shrink = value.uniform_low >= UNIFORM_SHRINK_AFTER_FRAMES;
    if shrink {
        value.uniform_low = 0;
    }
    let same = !shrink
        && sources.len() == value.previous_uniforms.len()
        && sources.iter().zip(value.previous_uniforms.iter()).all(|(source, retained)| match (source, retained) {
            (None, None) => true,
            (Some(bytes_), Some(slice)) => {
                let b = bytes_;
                b.len() == slice.length
                    && b[..] == slice.staging[slice.staging_offset..slice.staging_offset + slice.length]
            }
            _ => false,
        });
    // Scene3 blocks (5456 bytes plus 192 per instance) bypass [prepare]'s payload check, so reject
    // non-finite values and light counts outside 0..64 here, before anything is written; unchanged
    // frames skip this.
    let malformed_scene3 = || {
        sources.iter().any(|source| match source {
            Some(bytes_) if blen(bytes_) >= 5456 => {
                let b = bytes_;
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
        if length > UNIFORM_PAGE_BYTE_CAPACITY {
            valid = false;
        } else {
            let aligned = align256(length);
            if needed > UNIFORM_PAGE_BYTE_CAPACITY - aligned {
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
    let reuse = matches!(&value.uniform_pages[page_index], Some(page) if page.capacity >= needed && !shrink);
    if !reuse {
        let mut capacity: i64 = 4096;
        while capacity < needed {
            capacity = UNIFORM_PAGE_BYTE_CAPACITY.min(capacity * 2);
        }
        let descriptor = BufferDescriptor {
            label: Some("scene-transform-ring".to_owned()),
            size: capacity,
            usage: vec![BufferUsage::Storage, BufferUsage::CopyDst],
        };
        let buffer = backend::create_buffer(&value.device, descriptor, None)?;
        if let Some(old) = value.uniform_pages[page_index].take() {
            mem::release(uniform_owner(), old.capacity);
            defer.push(Deferred::Buffer(old.buffer));
        }
        mem::charge(uniform_owner(), capacity);
        value.uniform_pages[page_index] = Some(UniformPage { buffer, capacity });
    }
    let page_buffer = value.uniform_pages[page_index].as_ref().expect("uniform page").buffer.clone();
    value.next_uniform_page = (page_index + 1) % value.uniform_pages.len();
    // Pack every slice into one staging block and upload it once; the page is a bump allocator, not
    // a list of per-draw buffers.
    let mut staging = vec![0u8; needed as usize];
    let mut cursor: i64 = 0;
    let mut payload: i64 = 0;
    let mut offsets: Vec<Option<(i64, usize)>> = vec![None; sources.len()];
    for (index, source) in sources.iter().enumerate() {
        if let Some(bytes_) = source {
            let b = bytes_;
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
            entry.map(|(offset, length)| UniformSlice {
                buffer: page_buffer.clone(),
                offset,
                staging: staging.clone(),
                staging_offset: offset as usize,
                length,
            })
        })
        .collect())
}

/// The texel format follows from level 0's byte count: 4, 8 or 16 bytes per texel are
/// `Rgba8_unorm`, `Rgba16_float` and `Rgba32_float`.
fn texel_bytes(source: &SampledTexture) -> i64 {
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

fn valid_texture(source: &SampledTexture) -> bool {
    !source.key.is_empty()
        && !source.levels.is_empty()
        && ogpu_core::validation::validate_sampler(&source.sampler).is_ok()
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

fn texture_upload_scratch_for(value: &mut SceneExecution, total: i64) -> Result<(), Error> {
    if value.staged_frame != value.frame {
        value.staged_stream = value.staged_frame == value.frame - 1;
        value.staged_frame = value.frame;
    }
    if matches!(&value.texture_upload_scratch, Some(scratch) if scratch.size == total) {
        return Ok(());
    }
    let descriptor = BufferDescriptor { label: Some("scene-texture-staging".to_owned()), size: total, usage: vec![BufferUsage::CopySrc] };
    let scratch_buffer = backend::create_buffer(&value.device, descriptor, None)?;
    let previous = value.texture_upload_scratch.replace(TextureUploadScratch {
        buffer: scratch_buffer,
        bytes: vec![0u8; total as usize],
        size: total,
    });
    if let Some(old) = previous {
        let _ = backend::destroy_buffer(&old.buffer);
    }
    Ok(())
}

/// Uploads complete inside their frame, so what they staged through goes at its end: an image
/// uploads once, and two copies of it should not outlive that. A canvas that uploads frame after
/// frame keeps them, or each frame would pay for a new pair.
fn release_texture_staging(value: &mut SceneExecution) {
    if value.texture_upload_scratch.is_some() && !(value.staged_stream && value.staged_frame == value.frame)
        && let Some(scratch) = value.texture_upload_scratch.take() {
            let _ = backend::destroy_buffer(&scratch.buffer);
        }
}

/// Destroys the texture staging a run of uploading frames keeps for the next one; the next upload
/// makes it again.
pub fn release_staging(value: &mut SceneExecution) {
    value.staged_stream = false;
    release_texture_staging(value);
}

fn image_or_canvas_key(key: &str) -> bool {
    ["canvas:", "image:", "texture:", "text:", "world:"].iter().any(|prefix| key.starts_with(prefix))
}

fn levels_hash(source: &SampledTexture) -> u128 {
    let mut hash = digest(HASH_SEED, &(source.levels.len() as u64).to_le_bytes());
    for level in source.levels.iter() {
        hash = digest(hash, &level.width.to_le_bytes());
        hash = digest(hash, &level.height.to_le_bytes());
        hash = digest(hash, &level.bytes);
    }
    hash
}

fn prepare_texture(value: &mut SceneExecution, defer: &mut Vec<Deferred>, source: &SampledTexture) -> Result<CachedTextureRef, Error> {
    if let Some(texture) = &source.gpu {
        let (device_id, _) = backend::texture_driver_token(texture);
        if !valid_texture(source) {
            return Err(failure_error("Scene_execution.prepare_texture", ErrorKind::InvalidArgument, "GPU image is malformed or destroyed"));
        }
        if device_id != ogpu_core::handle::device_id(backend::device_handle(&value.device)) {
            return Err(failure_error("Scene_execution.prepare_texture", ErrorKind::CrossDevice, "GPU image belongs to another renderer"));
        }
        return Ok(Rc::new(RefCell::new(CachedTexture {
            key: source.key.clone(),
            hash: None,
            shape: String::new(),
            texture: texture.clone(),
            bytes: 0,
            used: value.frame,
            levels: Rc::new(Vec::new()),
        })));
    }
    let found = side_lru::find(&mut value.texture_cache, &skey(&source.key)).map(|item| cached_texture_of(&item));
    let key = source.key.as_str();
    match found {
        Some(item)
            if ["image:", "texture:", "shadow:", "text:", "world:"].iter().any(|prefix| key.starts_with(prefix)) =>
        {
            item.borrow_mut().used = value.frame;
            Ok(item)
        }
        Some(item) if key.starts_with("canvas:") => upload_texture(value, defer, source, None, Some(item)),
        // The immutable levels this slot was uploaded from: no hashing.
        Some(item) if Rc::ptr_eq(&item.borrow().levels, &source.levels) => {
            item.borrow_mut().used = value.frame;
            Ok(item)
        }
        Some(item) => {
            let hash = levels_hash(source);
            if item.borrow().hash == Some(hash) {
                rebind_texture_source(value, &item, source);
                Ok(item)
            } else {
                upload_texture(value, defer, source, Some(hash), None)
            }
        }
        // Image, canvas, texture and text keys carry identity and generation, so their bytes are
        // never hashed; other keys are content-addressed.
        None => {
            let hash = if image_or_canvas_key(key) { None } else { Some(levels_hash(source)) };
            upload_texture(value, defer, source, hash, None)
        }
    }
}

fn rebind_texture_source(value: &mut SceneExecution, item: &CachedTextureRef, source: &SampledTexture) {
    let (changed, charge) = {
        let b = item.borrow();
        let old_cpu = b.levels.iter().fold(0i64, |n, level| n.saturating_add(level.bytes.capacity() as i64));
        let charge = b.bytes.max(crate::renderer_lowering::sampled_texture_bytes(source));
        (b.key != source.key || b.bytes.max(old_cpu) != charge, charge)
    };
    if changed {
        let old_key = item.borrow().key.clone();
        // Taking preserves the reused native allocation; remove/add would retire it.
        side_lru::take(&mut value.texture_cache, &skey(&old_key));
        item.borrow_mut().key = source.key.clone();
        let item_any: Item = item.clone();
        side_lru::add(&mut value.texture_cache, charge, skey(&source.key), item_any);
    }
    let mut b = item.borrow_mut();
    b.levels = source.levels.clone();
    b.used = value.frame;
}

fn upload_texture(
    value: &mut SceneExecution,
    defer: &mut Vec<Deferred>,
    source: &SampledTexture,
    hash: Option<u128>,
    reusable: Option<CachedTextureRef>,
) -> Result<CachedTextureRef, Error> {
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
                b.shape == shape && image_or_canvas_key(&b.key) && b.used != frame
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
    // Each retained CPU/GPU bank fits this charge; their combined storage may be twice it.
    let charge = total.max(crate::renderer_lowering::sampled_texture_bytes(source));
    let cacheable = charge <= TEXTURE_CACHE_BYTE_CAPACITY;
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
        staging = scratch.buffer.clone();
        let packed = &mut scratch.bytes;
        for (level_index, level) in source.levels.iter().enumerate() {
            let pixels = &level.bytes;
            let row_bytes = (level.width * texel) as usize;
            for row in 0..level.height as usize {
                let to = offsets[level_index] as usize + row * rows[level_index] as usize;
                packed[to..to + row_bytes].copy_from_slice(&pixels[row * row_bytes..(row + 1) * row_bytes]);
            }
        }
    }
    let written = {
        let scratch = value.texture_upload_scratch.as_ref().expect("texture staging");
        backend::write_buffer(&staging, 0, &scratch.bytes)
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
            item.borrow_mut().hash = hash;
            rebind_texture_source(value, &item, source);
            Ok(item)
        }
        _ => {
            let item: CachedTextureRef = Rc::new(RefCell::new(CachedTexture {
                key: source.key.clone(),
                hash,
                shape,
                texture: texture.clone(),
                bytes: total,
                used: value.frame,
                levels: source.levels.clone(),
            }));
            if !cacheable {
                defer.push(Deferred::Texture(texture));
            } else {
                let item_any: Item = item.clone();
                side_lru::add(&mut value.texture_cache, charge, skey(&source.key), item_any);
            }
            Ok(item)
        }
    }
}

fn prepare_storage(value: &mut SceneExecution, key: &str, bytes_: &bytes) -> Result<CachedAuxiliaryRef, Error> {
    let found = side_lru::find(&mut value.auxiliary_cache, &skey(key)).map(|item| cached_auxiliary_of(&item));
    if let Some(item) = found {
        if Rc::ptr_eq(&item.borrow().source, bytes_) {
            return Ok(item);
        }
        if item.borrow().copy == *bytes_ {
            let old_charge = item.borrow().copy.capacity();
            {
                let mut b = item.borrow_mut();
                b.copy = bytes_.clone();
                b.source = bytes_.clone();
            }
            if old_charge != bytes_.capacity() {
                side_lru::take(&mut value.auxiliary_cache, &skey(key));
                let item_any: Item = item.clone();
                side_lru::add(&mut value.auxiliary_cache, bytes_.capacity() as i64, skey(key), item_any);
            }
            return Ok(item);
        }
    }
    let data = bytes_;
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
    let item: CachedAuxiliaryRef = Rc::new(RefCell::new(CachedAuxiliary {
        copy: data.clone(),
        buffer,
        source: bytes_.clone(),
    }));
    let item_any: Item = item.clone();
    side_lru::add(&mut value.auxiliary_cache, data.capacity() as i64, skey(key), item_any);
    value.uploaded += data.len() as i64;
    Ok(item)
}

fn prepare_auxiliary(value: &mut SceneExecution, source: &AuxiliaryResource) -> Result<CachedAuxiliaryRef, Error> {
    prepare_storage(value, &source.key, &source.buffer)
}

fn u32_le(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap())
}

fn primitive_code(primitive: Primitive) -> &'static str {
    match primitive {
        Primitive::PointList => "P",
        Primitive::LineList => "L",
        Primitive::LineStrip => "LS",
        Primitive::TriangleList => "T",
        Primitive::TriangleStrip => "S",
    }
}

fn mesh_identity(mesh: &Mesh) -> u128 {
    let hash = match &mesh.key {
        MeshKey::Host(key) => digest(digest(HASH_SEED, b"host"), key.as_bytes()),
        MeshKey::GpuVertices { indices: Some(indices), .. } => digest(digest(HASH_SEED, b"gpu:indices"), indices.as_bytes()),
        MeshKey::GpuVertices { name, indices: None } => digest(digest(HASH_SEED, b"gpu:vertices"), name.as_bytes()),
    };
    let hash = digest(hash, primitive_code(mesh.primitive).as_bytes());
    digest(digest(hash, &vertex_hash(mesh).to_le_bytes()), &mesh.indices)
}

fn combine_meshes(meshes: &[&Mesh]) -> Result<Mesh, Error> {
    let operation = "Scene_execution.combine_meshes";
    let valid_indices = |mesh: &Mesh| {
        let indices = &mesh.indices;
        (0..mesh.index_count as usize).all(|index| {
            let value = u32_le(&indices, index * 4) as i32 as i64;
            value >= 0 && value < mesh.vertex_count
        })
    };
    let (mut vertex_bytes, mut vertex_count, mut index_count) = (0i64, 0i64, 0i64);
    let mut identity = digest(HASH_SEED, &(meshes.len() as u64).to_le_bytes());
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
        if vertex_bytes.checked_add(vlen).is_none()
            || vertex_count > i32::MAX as i64 - mesh.vertex_count
            || index_count.checked_add(mesh.index_count).is_none_or(|n| n > u32::MAX as i64)
        {
            return Err(failure_error(operation, ErrorKind::Capacity, "combined mesh exceeds packed representation limits"));
        }
        vertex_bytes += vlen;
        vertex_count += mesh.vertex_count;
        index_count += mesh.index_count;
        identity = digest(identity, &mesh_identity(mesh).to_le_bytes());
    }
    let mut vertices = runtime_resources::byte_buffer(operation, usize::try_from(vertex_bytes).map_err(|_| failure_error(operation, ErrorKind::Capacity, "vertex bytes exceed native address space"))?)?;
    vertices.resize(vertex_bytes as usize, 0);
    let mut indices = runtime_resources::byte_buffer(operation, usize::try_from(index_count * 4).map_err(|_| failure_error(operation, ErrorKind::Capacity, "index bytes exceed native address space"))?)?;
    indices.resize((index_count * 4) as usize, 0);
    let (mut vertex_offset, mut vertex_base, mut index_offset) = (0usize, 0i64, 0usize);
    for mesh in meshes {
        let owned = &mesh.vertices;
        let source = mesh.shared_vertices.as_ref().map_or(&owned[..], |bytes| bytes.as_slice());
        vertices[vertex_offset..vertex_offset + source.len()].copy_from_slice(source);
        let source_indices = &mesh.indices;
        for index in 0..mesh.index_count as usize {
            let value = u32_le(&source_indices, index * 4) as i32 as i64;
            set_u32_le(&mut indices, index_offset + index * 4, (value + vertex_base) as i32 as u32);
        }
        vertex_offset += source.len();
        vertex_base += mesh.vertex_count;
        index_offset += mesh.index_count as usize * 4;
    }
    Ok(Mesh {
        key: MeshKey::Host(format!("batch:{identity:032x}")),
        vertices: new_bytes(vertices),
        shared_vertices: None,
        vertex_count,
        indices: new_bytes(indices),
        index_count,
        primitive: meshes[0].primitive,
    })
}

fn same_optional_texture(a: &Option<Rc<SampledTexture>>, b: &Option<Rc<SampledTexture>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn same_optional_auxiliary(a: &Option<Rc<AuxiliaryResource>>, b: &Option<Rc<AuxiliaryResource>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn attribute_bytes(entry: &SampledDraw) -> i64 {
    entry.vertex_attributes.as_ref().map_or(0, |(_, b)| blen(b) as i64)
}

fn draw_bytes(entry: &SampledDraw) -> i64 {
    (entry.draw.mesh.vertex_bytes_len() + blen(&entry.draw.mesh.indices)) as i64 + attribute_bytes(entry)
}

fn coalesce_draws(draws: &[Draw]) -> Result<Vec<Draw>, Error> {
    let stride = |mesh: &Mesh| if mesh.vertex_count == 0 { 0 } else { mesh.vertex_bytes_len() as i64 / mesh.vertex_count };
    let compatible = |first: &SampledDraw, next: &SampledDraw| {
        matches!((&first.draw.mesh.key, &next.draw.mesh.key), (MeshKey::Host(_), MeshKey::Host(_)))
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
    let mut result: Vec<Draw> = Vec::new();
    let mut position = 0;
    while position < draws.len() {
        let first = &draws[position];
        let first_bytes = draw_bytes(first);
        if first_bytes > CACHE_BYTE_CAPACITY {
            return Err(failure_error("Scene_execution.coalesce", ErrorKind::Capacity, "one mesh exceeds the batch byte capacity"));
        }
        let mut packed_bytes = first_bytes;
        let mut end = position + 1;
        while end < draws.len() && compatible(first, &draws[end]) {
            let bytes_ = draw_bytes(&draws[end]);
            if packed_bytes <= CACHE_BYTE_CAPACITY - bytes_ {
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
            let meshes: Vec<&Mesh> = entries.iter().map(|entry| &entry.draw.mesh).collect();
            let mesh = combine_meshes(&meshes)?;
            let vertex_attributes = first.vertex_attributes.as_ref().map(|_| {
                let mut hash = HASH_SEED;
                let mut joined: Vec<u8> = Vec::new();
                for entry in entries {
                    let (key, bytes) = entry.vertex_attributes.as_ref().expect("attributes");
                    hash = digest(hash, key.as_bytes());
                    joined.extend_from_slice(bytes);
                }
                (format!("batch:attributes:{hash:032x}"), new_bytes(joined))
            });
            let mut merged = (**first).clone();
            merged.vertex_attributes = vertex_attributes;
            merged.draw.mesh = mesh;
            result.push(Rc::new(merged));
        }
    }
    Ok(result)
}

fn prepared_bytes_of(draws: &[Draw]) -> i64 {
    draws.iter().fold(0i64, |total, entry| total.saturating_add(retained_draw_bytes(entry)))
}

pub(crate) fn retained_draw_bytes(entry: &SampledDraw) -> i64 {
    let texture_bytes = crate::renderer_lowering::sampled_texture_bytes;
    entry.draw.mesh.retained_vertex_bytes()
        .saturating_add(entry.draw.mesh.indices.capacity() as i64)
        .saturating_add(entry.vertex_attributes.as_ref().map_or(0, |(_, b)| b.capacity() as i64))
        .saturating_add(entry.draw.state.transform_uniforms.as_ref().map_or(0, |b| b.capacity() as i64))
        .saturating_add(entry.texture.as_ref().map_or(0, |texture| texture_bytes(texture)))
        .saturating_add(entry.auxiliary.as_ref().map_or(0, |a| (a.buffer.capacity() as i64)
            .saturating_add(texture_bytes(&a.texture)).saturating_add(a.environment.as_ref().map_or(0, texture_bytes))))
}

fn resolve_prepared(value: &mut SceneExecution, prepared: &Option<(String, i64)>, draws: &[Draw]) -> Result<Vec<Draw>, Error> {
    match prepared {
        Some((identity, _)) if identity.is_empty() => {
            Err(failure_error("Scene_execution.prepare_run", ErrorKind::InvalidArgument, "prepared identity is empty"))
        }
        Some((identity, version)) => {
            if let Some(item) = side_lru::find(&mut value.prepared_cache, &skey(identity)) {
                let item = prepared_run_of(&item);
                if item.version == *version {
                    return Ok(item.draws.clone());
                }
            }
            let prepared_draws = coalesce_draws(draws)?;
            let item = Rc::new(PreparedRun {
                version: *version,
                bytes: prepared_bytes_of(&prepared_draws),
                draws: prepared_draws.clone(),
            });
            let bytes_ = item.bytes;
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

fn texture_signature(source: &SampledTexture, cached: &CachedTextureRef) -> (i64, SamplerDescriptor) {
    (backend::texture_id(&cached.borrow().texture), source.sampler.clone())
}

fn auxiliary_signature(entry: &(Rc<AuxiliaryResource>, CachedAuxiliaryRef, CachedTextureRef)) -> (i64, i64, SamplerDescriptor) {
    (
        backend::buffer_id(&entry.1.borrow().buffer),
        backend::texture_id(&entry.2.borrow().texture),
        entry.0.texture.sampler.clone(),
    )
}

/// Submission signatures share the affine bytes, which are immutable, so a caller reusing its own
/// buffer cannot turn into a false cache hit. Completed ring slices may be replayed when the whole uniform set is
/// unchanged. A changed set gets a different page and offsets.
fn automatic_signature(slot: &PreparedSlot) -> AutomaticSignature {
    let item = slot.mesh.as_ref().expect("slot mesh").borrow();
    let mut signature_state = slot.state.clone().expect("slot state");
    if slot.uniform.is_some() {
        signature_state.transform_uniforms = None;
    }
    AutomaticSignature {
        family: slot.family,
        blend: slot.blend,
        samples: slot.samples,
        state: signature_state,
        texture: slot.texture.as_ref().map(|(source, cached)| texture_signature(source, cached)),
        auxiliary: slot.auxiliary.as_ref().map(auxiliary_signature),
        environment: slot.environment.as_ref().map(|(source, cached)| texture_signature(source, cached)),
        buffer: backend::buffer_id(&item.buffer),
        gpu_vertices: slot.gpu_vertices.as_ref().map(gpu_identity),
        vertex_attributes: slot.vertex_attributes.as_ref().map(|a| backend::buffer_id(&a.borrow().buffer)),
        index_offset: item.index_offset,
        uniform_buffer: slot.uniform.as_ref().map(|u| backend::buffer_id(&u.buffer)),
        uniform_offset: slot.uniform.as_ref().map(|u| u.offset),
        vertex_count: item.vertex_count,
        index_count: item.index_count,
    }
}

const PREPARED_SCRATCH_CAPACITY: usize = 65_536;

fn make_prepared_slot() -> PreparedSlot {
    PreparedSlot {
        family: Scene2,
        blend: Blend::Replace,
        samples: 1,
        state: None,
        texture: None,
        auxiliary: None,
        environment: None,
        mesh: None,
        uniform: None,
        vertex_attributes: None,
        gpu_vertices: None,
    }
}

fn ensure_prepared_scratch(scratch: &mut PreparedScratch, needed: usize) -> Result<(), Error> {
    if needed > PREPARED_SCRATCH_CAPACITY {
        return Err(failure_error("Renderer.submit", ErrorKind::Capacity, "one submission exceeds the bounded prepared-draw capacity"));
    }
    if needed <= scratch.slots.len() {
        return Ok(());
    }
    let mut capacity = scratch.slots.len();
    while capacity < needed {
        capacity = PREPARED_SCRATCH_CAPACITY.min(16.max(capacity * 2));
    }
    while scratch.slots.len() < capacity {
        scratch.slots.push(make_prepared_slot());
    }
    Ok(())
}

fn clear_prepared_scratch(scratch: &mut PreparedScratch) {
    for slot in &mut scratch.slots[..scratch.length] {
        slot.state = None;
        slot.texture = None;
        slot.auxiliary = None;
        slot.environment = None;
        slot.gpu_vertices = None;
        slot.mesh = None;
        slot.uniform = None;
        slot.vertex_attributes = None;
    }
    scratch.length = 0;
}

fn has_depth(family: PipelineFamily) -> bool {
    !matches!(family, Scene2 | Scene2Textured | Ui | Marks)
}

/// Attachments and encoder state for one render pass over `target`.
fn render_target(
    value: &mut SceneExecution,
    family: PipelineFamily,
    samples: i64,
    state: &State,
    load: Load,
    clear: (f64, f64, f64, f64),
    later_color: bool,
    later_depth: bool,
) -> Result<RenderTarget, Error> {
    // an attachment no other pass reads needs no memory: it clears here and only its resolve stays
    let color_passes = load == Load::Clear && !later_color;
    let depth_passes = state.depth_load == Load::Clear && !later_depth;
    let mut keys: Vec<(scene_attachment_pool::Kind, i64, bool)> = Vec::new();
    if samples != 1 {
        keys.push((scene_attachment_pool::Kind::Color, samples, color_passes));
    }
    if has_depth(family) {
        keys.push((scene_attachment_pool::Kind::Depth, samples, depth_passes));
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

fn sampler_for(value: &mut SceneExecution, descriptor: &SamplerDescriptor) -> Result<(usize, Sampler), Error> {
    if let Some((index, (_, sampler))) = value.samplers.iter().enumerate().find(|(_, (stored, _))| stored == descriptor) {
        return Ok((index, sampler.clone()));
    }
    // ponytail: linear scan of at most 64 descriptors; index only if profiling justifies it.
    // A scene using more clears the table and drops replay plans that borrowed its samplers.
    if value.samplers.len() >= 64 {
        for (_, sampler) in &value.samplers {
            let _ = backend::destroy_sampler(sampler);
        }
        value.samplers.clear();
        if let Some(pool) = &mut value.arguments { pool.slices.clear(); pool.next = 0; }
        drop_plans(value);
    }
    let sampler = backend::create_sampler(&value.device, descriptor.clone())?;
    let index = value.samplers.len();
    value.samplers.push((descriptor.clone(), sampler.clone()));
    Ok((index, sampler))
}

const ARGUMENT_PAGE_BYTES: i64 = 256 * 1024;

fn argument_encoder(value: &mut SceneExecution, variant: usize) -> Result<Argument, Error> {
    if let Some(argument) = &value.pipelines[variant].argument {
        return Ok(argument.clone());
    }
    let argument = backend::create_argument(&value.pipelines[variant].pipeline, ShaderStage::Fragment, 7)?;
    value.pipelines[variant].argument = Some(argument.clone());
    Ok(argument)
}

/// Canonical Scene2 binds its texture and sampler through one argument slice per (texture, sampler)
/// pair inside a shared page, so indirect commands can reference textures.
fn argument_slice(value: &mut SceneExecution, variant: usize, texture: &Texture, sampler_descriptor: &SamplerDescriptor) -> Result<(Buffer, i64), Error> {
    let argument = argument_encoder(value, variant)?;
    if value.arguments.is_none() {
        let length = backend::argument_length(&argument);
        let alignment = backend::argument_alignment(&argument).max(1);
        let stride = 16.max((length + alignment - 1) / alignment * alignment);
        let descriptor = BufferDescriptor {
            label: Some("scene-argument-page".to_owned()),
            size: ARGUMENT_PAGE_BYTES,
            usage: vec![BufferUsage::Storage],
        };
        let argument_buffer = backend::create_buffer(&value.device, descriptor, None)?;
        value.arguments = Some(ArgumentPool {
            buffer: argument_buffer,
            stride,
            capacity: ARGUMENT_PAGE_BYTES / stride,
            slices: HashMap::new(),
            next: 0,
        });
    }
    let (stride, buffer) = {
        let pool = value.arguments.as_ref().expect("argument pool");
        (pool.stride, pool.buffer.clone())
    };
    if backend::argument_length(&argument) > stride {
        return Err(failure_error("Scene_execution.argument_slice", ErrorKind::Capacity, "argument layout exceeds the shared slice stride"));
    }
    let (sampler_index, sampler) = sampler_for(value, sampler_descriptor)?;
    let key = (backend::texture_id(texture), sampler_index as u64);
    if let Some(offset) = value.arguments.as_ref().expect("argument pool").slices.get(&key) {
        return Ok((buffer, *offset));
    }
    {
        let pool = value.arguments.as_mut().expect("argument pool");
        if pool.next >= pool.capacity {
            pool.slices.clear();
            pool.next = 0;
            drop_plans(value);
        }
    }
    let offset = value.arguments.as_ref().expect("argument pool").next * stride;
    backend::argument_texture(&argument, &buffer, offset, 0, texture)?;
    backend::argument_sampler(&argument, &buffer, offset, 1, &sampler)?;
    let pool = value.arguments.as_mut().expect("argument pool");
    pool.next += 1;
    pool.slices.insert(key, offset);
    Ok((buffer, offset))
}

enum Acquired {
    Offscreen,
    Presented(backend::Frame),
}

fn discard(frame: &Acquired) {
    if let Acquired::Presented(frame) = frame {
        let _ = backend::discard(frame);
    }
}

/// One presented frame stays in flight: the CPU builds the next frame while the GPU draws this one.
/// Waiting here, before any preparation, keeps every resource the previous frame used alive until it
/// completes, so retiring and destroying after the next submission stays safe.
fn settle(value: &mut SceneExecution) -> Result<(), Error> {
    match value.in_flight.take() {
        None => Ok(()),
        Some(epoch) => backend::complete_through(&value.queue, epoch),
    }
}

enum Acquisition {
    Acquired(Acquired),
    Skipped,
}

fn acquire(value: &mut SceneExecution) -> Result<Acquisition, Error> {
    settle(value)?;
    match &value.surface {
        None => Ok(Acquisition::Acquired(Acquired::Offscreen)),
        Some(surface) => match backend::acquire_sync(surface)? {
            backend::AcquireResult::Timeout | backend::AcquireResult::Occluded => Ok(Acquisition::Skipped),
            backend::AcquireResult::DeviceLost => Err(failure_error("Renderer.submit", ErrorKind::DeviceLost, "device lost")),
            backend::AcquireResult::Acquired(frame) => Ok(Acquisition::Acquired(Acquired::Presented(frame))),
        },
    }
}

fn plain_draws(draws: &[PlanDraw]) -> bool {
    draws.iter().all(|draw| draw.textures.is_empty() && draw.samplers.is_empty() && draw.indirect.is_none())
}

/// A batch that draws straight into the frame's target with no depth: consecutive ones share one
/// render pass (its viewport, scissor and cull are per batch), so a 2D frame is not a pass per clip.
fn joins_pass(batch: &BatchPlan) -> bool {
    batch.samples == 1 && !has_depth(batch.family)
}

fn begin_pass(
    value: &mut SceneExecution,
    commands: &backend::Commands,
    clear: (f64, f64, f64, f64),
    later: &[Rc<BatchPlan>],
    batch: &BatchPlan,
) -> Result<backend::RenderEncoder, Error> {
    let same_samples = |next: &Rc<BatchPlan>| next.samples == batch.samples;
    let target = render_target(
        value,
        batch.family,
        batch.samples,
        &batch.state,
        if batch.first { Load::Clear } else { Load::Load },
        clear,
        later.iter().any(same_samples),
        later.iter().any(|next| same_samples(next) && has_depth(next.family) && next.state.depth_load == Load::Load),
    )?;
    backend::render_encoder(commands, &target)
}

fn encode_batch(value: &mut SceneExecution, encoder: &backend::RenderEncoder, batch: &BatchPlan) -> Result<(), Error> {
    let bound_w = value.configuration.physical_width;
    let bound_h = value.configuration.physical_height;
    // A viewport or scissor with no pixel on the target draws nothing. The viewport is passed
    // whole: it maps clip space, so a view that hangs over the target edge keeps its proportions
    // and only the scissor is clipped.
    let (visible, (x, y, width, height), (sx, sy, sw, sh)) = match (
        intersect_extent(bound_w, bound_h, batch.state.viewport),
        intersect_extent(bound_w, bound_h, batch.state.scissor),
    ) {
        (Some(_), Some(scissor)) => (true, batch.state.viewport, scissor),
        _ => (false, (0, 0, bound_w.max(1), bound_h.max(1)), (0, 0, bound_w.max(1), bound_h.max(1))),
    };
    backend::set_viewport(&encoder, Rect { x, y, width, height })?;
    backend::set_scissor(&encoder, Rect { x: sx, y: sy, width: sw, height: sh })?;
    backend::set_cull(&encoder, batch.state.cull)?;
    // 3D meshes wind counter-clockwise: a fact of the family, never of a bound slot
    if has_depth(batch.family) {
        backend::set_winding(&encoder, backend::Winding::CounterClockwise)?;
        backend::set_depth_state(
            &encoder,
            Some(DepthState { depth_compare: batch.state.depth_compare, depth_write: batch.state.depth_write }),
        )?;
    }
    if !batch.draws.is_empty() && visible {
        if !batch.resources.is_empty() {
            backend::use_resources(&encoder, &batch.resources)?;
        }
        let icb = batch.icb.borrow().clone();
        match icb {
            Some(icb) => {
                backend::set_render_pipeline(&encoder, &batch.draws[0].draw.pipeline)?;
                value.icb_stats.executions += 1;
                backend::execute_icb(&encoder, &icb, 0, batch.draws.len() as i64)?;
            }
            None => {
                if plain_draws(&batch.draws) {
                    backend::draw_batch(&encoder, &batch.driver_draws)?;
                } else {
                    for draw in &batch.draws {
                        let bd = &draw.draw;
                        backend::set_render_pipeline(&encoder, &bd.pipeline)?;
                        for (stage, index, buffer, offset) in &bd.buffers {
                            backend::set_stage_buffer(&encoder, *stage, *index, Some(*offset), buffer)?;
                        }
                        for (stage, index, texture) in &draw.textures {
                            backend::set_stage_texture(&encoder, *stage, *index, texture)?;
                        }
                        for (stage, index, sampler) in &draw.samplers {
                            backend::set_stage_sampler(&encoder, *stage, *index, sampler)?;
                        }
                        match (&bd.index, &draw.indirect) {
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
    Ok(())
}

fn build_icb(value: &mut SceneExecution, batch: &BatchPlan) -> Result<(), Error> {
    let draws = &batch.draws;
    if draws.is_empty()
        || !plain_draws(draws)
        || batch.icb.borrow().is_some()
        || !draws.iter().all(|draw| backend::pipeline_indirect(&draw.draw.pipeline))
    {
        return Ok(());
    }
    let icb = backend::create_icb(&value.device, draws.len() as i64)?;
    let encoded = (|| -> Result<(), Error> {
        for (index, draw) in draws.iter().enumerate() {
            let index = index as i64;
            let bd = &draw.draw;
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
            value.icb_stats.builds += 1;
            *batch.icb.borrow_mut() = Some(icb);
            Ok(())
        }
    }
}

fn submit_frame(value: &mut SceneExecution, frame: &Acquired, commands: &backend::Commands) -> Result<(), Error> {
    let committed = match frame {
        Acquired::Offscreen => backend::commit(commands),
        Acquired::Presented(surface_frame) => backend::commit_present(commands, &value.target, surface_frame),
    };
    match committed {
        Err(e) => {
            let _ = backend::abandon(commands);
            discard(frame);
            Err(e)
        }
        Ok(receipt) => match frame {
            Acquired::Presented(_) => {
                value.in_flight = Some(receipt.epoch);
                Ok(())
            }
            Acquired::Offscreen => backend::complete_through(&value.queue, receipt.epoch),
        },
    }
}

/// Encodes `batches` into one command buffer and completes it.
fn encode_frame(value: &mut SceneExecution, frame: &Acquired, clear: (f64, f64, f64, f64), batches: &[Rc<BatchPlan>]) -> Result<(), Error> {
    let commands = match backend::begin_commands(&value.queue) {
        Ok(commands) => commands,
        Err(e) => {
            discard(frame);
            return Err(e);
        }
    };
    let encoded = (|| -> Result<(), Error> {
        let mut open: Option<backend::RenderEncoder> = None;
        for (index, batch) in batches.iter().enumerate() {
            let later = &batches[index + 1..];
            let encoder = match open.take() {
                Some(encoder) => encoder,
                None => begin_pass(value, &commands, clear, later, batch)?,
            };
            encode_batch(value, &encoder, batch)?;
            if joins_pass(batch) && later.first().is_some_and(|next| joins_pass(next)) {
                open = Some(encoder);
            } else {
                backend::end_render(&encoder)?;
            }
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
pub const SUN_MAP_SIZE: i64 = 2048;

fn sun_key_of(slot: &PreparedSlot) -> Option<String> {
    slot.auxiliary.as_ref().and_then(|(source, _, _)| source.sun_shadow.clone())
}

fn sun_descriptor(label: &str, usage: Vec<TextureUsage>) -> TextureDescriptor {
    TextureDescriptor {
        label: Some(label.to_owned()),
        width: SUN_MAP_SIZE,
        height: SUN_MAP_SIZE,
        depth: 1,
        mip_levels: 1,
        sample_count: 1,
        format: TextureFormat::Rgba8Unorm,
        usage,
    }
}

fn render_sun(value: &mut SceneExecution) -> Result<(), Error> {
    let mut request: Option<String> = None;
    for index in (0..value.prepared_scratch.length).rev() {
        if let Some(key) = sun_key_of(&value.prepared_scratch.slots[index]) {
            request = Some(key);
        }
    }
    let Some(key) = request else { return Ok(()) };
    if matches!(&value.sun, Some(sun) if sun.key == key) {
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
        value.sun = Some(SunMap { pipeline, color, depth, key: String::new() });
    }
    // Retained plans may have sampled the previous map.
    drop_plans(value);
    value.sun.as_mut().expect("sun map").key = String::new();
    let (sun_pipeline, sun_color, sun_depth) = {
        let sun = value.sun.as_ref().expect("sun map");
        (sun.pipeline.clone(), sun.color.clone(), sun.depth.clone())
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
        backend::set_viewport(&encoder, Rect { x: 0, y: 0, width: SUN_MAP_SIZE, height: SUN_MAP_SIZE })?;
        backend::set_cull(&encoder, Cull::CullNone)?;
        backend::set_depth_state(&encoder, Some(DepthState { depth_compare: Comparison::Less, depth_write: true }))?;
        for index in 0..value.prepared_scratch.length {
            let slot = &value.prepared_scratch.slots[index];
            let (Some((_, world, _)), Some(item), Some(state)) = (&slot.auxiliary, &slot.mesh, &slot.state) else {
                continue;
            };
            let item = item.borrow();
            if !(sun_key_of(slot).as_deref() == Some(key.as_str())
                && slot.family == Scene3World
                && (slot.blend == Blend::Replace || slot.blend == Blend::Alpha)
                && state.depth_write
                && item.primitive == Primitive::TriangleList)
            {
                continue;
            }
            let bytes_ = state.transform_uniforms.as_ref().map_or(0, |b| blen(b) as i64);
            let uniforms: Option<(Buffer, i64, i64)> = match (&slot.uniform, item.uniform_offset) {
                (Some(uniform), _) => Some((
                    uniform.buffer.clone(),
                    uniform.offset,
                    if bytes_ > 5456 { uniform.offset + 5456 } else { uniform.offset },
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
                match &slot.gpu_vertices {
                    Some(source) => &source.buffer,
                    None => &item.buffer,
                },
            )?;
            backend::set_stage_buffer(&encoder, ShaderStage::Vertex, 2, None, &world.borrow().buffer)?;
            backend::set_stage_buffer(&encoder, ShaderStage::Vertex, 3, Some(offset), &buffer)?;
            backend::set_stage_buffer(&encoder, ShaderStage::Vertex, 4, Some(instances_offset), &buffer)?;
            let (indices, index_offset, index_count) = match slot.gpu_vertices.as_ref().and_then(|source| source.indices.clone()) {
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
            value.sun.as_mut().expect("sun map").key = key;
            value.sun_passes += 1;
            Ok(())
        }
    }
}

/// Sun map passes rendered so far (see `auxiliary_resource.sun_shadow`).
pub fn sun_shadow_passes(value: &SceneExecution) -> i64 {
    value.sun_passes
}

fn replay_plan(value: &mut SceneExecution, plan: &Rc<ReplayPlan>) -> Result<bool, Error> {
    if !plan.gpu_vertices.iter().all(gpu_vertices_valid) {
        return Err(failure_error("Scene_execution.replay", ErrorKind::StaleHandle, "GPU vertices are stale or closed"));
    }
    match acquire(value)? {
        Acquisition::Skipped => Ok(false),
        Acquisition::Acquired(frame) => encode_frame(value, &frame, plan.clear, &plan.batches).map(|()| true),
    }
}

/// 0: Scene2, 1: Scene3-like, 3: Ui and Marks draws bind directly; argument-buffer passes cannot
/// mix them.
fn attachment_class(family: PipelineFamily) -> i64 {
    match family {
        Scene2 | Scene2Textured => 0,
        Ui | Marks => 3,
        Scene3 | Scene3Points | Scene3Textured | Scene3World => 1,
    }
}

fn same_pass_state(class_: i64, a: &State, b: &State) -> bool {
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

/// One draw's plan: the bindings its pipeline's Lisp declaration order expects.
fn plan_entry(value: &mut SceneExecution, index: usize) -> Result<PlanDraw, Error> {
    let slot = &value.prepared_scratch.slots[index];
    let family = slot.family;
    let blend = slot.blend;
    let samples = slot.samples;
    let item_ref = slot.mesh.clone().expect("slot mesh");
    let slot_texture = slot.texture.clone();
    let slot_auxiliary = slot.auxiliary.clone();
    let slot_environment = slot.environment.clone();
    let slot_uniform = slot.uniform.clone();
    let slot_gpu_vertices = slot.gpu_vertices.clone();
    let slot_vertex_attributes = slot.vertex_attributes.clone();
    let transform_uniforms = slot.state.as_ref().expect("slot state").transform_uniforms.clone();
    let sun_requested = sun_key_of(slot).is_some();
    let item = item_ref.borrow();
    if let Some(source) = &slot_gpu_vertices
        && !gpu_vertices_valid(source) {
            return Err(failure_error("Renderer.submit", ErrorKind::StaleHandle, "GPU vertices are stale or closed"));
        }
    let scene2 = family == Scene2 || family == Scene2Textured;
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
            if family == Scene2Textured {
                let (page, offset) = argument_slice(value, variant, &texture, &source.sampler)?;
                (vec![(fragment_stage, 7, page, offset)], vec![], vec![], vec![texture])
            } else {
                let (_, sampler) = sampler_for(value, &source.sampler)?;
                (vec![], vec![(fragment_stage, 1, texture)], vec![(fragment_stage, 2, sampler)], vec![])
            }
        }
    };
    let vertices: Buffer = match &slot_gpu_vertices {
        None => item.buffer.clone(),
        Some(source) => source.buffer.clone(),
    };
    let mut extra: Vec<(ShaderStage, i64, Buffer, i64)> = Vec::new();
    if let Some(source) = &slot_gpu_vertices {
        for (i, b) in source.extra.iter().enumerate() {
            extra.push((vertex_stage, i as i64 + 1, b.clone(), 0));
        }
        if is_scene3_family(family) && source.extra.is_empty() {
            extra.push((vertex_stage, 1, source.buffer.clone(), 0));
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
            let (_, sampler) = sampler_for(value, &source.texture.sampler)?;
            let mut all = vec![(vertex_stage, 0, vertices.clone(), 0), (fragment_stage, 3, buffer.borrow().buffer.clone(), 0)];
            all.extend(extra);
            all.extend(buffers);
            buffers = all;
            let mut tex = Vec::with_capacity(1 + textures.len());
            tex.push((fragment_stage, 4, texture.borrow().texture.clone()));
            tex.extend(textures);
            textures = tex;
            let mut sam = Vec::with_capacity(1 + samplers.len());
            sam.push((fragment_stage, 5, sampler));
            sam.extend(samplers);
            samplers = sam;
        }
    }
    // World draws bind the sun map at 8 (the camera map stands in when there is none; the block
    // says whether to read it) and the camera map at 9/10.
    if let (Scene3World, Some((source, cached))) = (family, &slot_environment) {
        let (_, sampler) = sampler_for(value, &source.sampler)?;
        let camera = cached.borrow().texture.clone();
        let sun = match (&value.sun, sun_requested) {
            (Some(sun), true) => sun.color.clone(),
            _ => camera.clone(),
        };
        let mut tex = Vec::with_capacity(2 + textures.len());
        tex.push((fragment_stage, 8, sun));
        tex.push((fragment_stage, 9, camera));
        tex.extend(textures);
        textures = tex;
        let mut sam = Vec::with_capacity(1 + samplers.len());
        sam.push((fragment_stage, 10, sampler));
        sam.extend(samplers);
        samplers = sam;
    }
    if !direct {
        let attributes = match &slot_vertex_attributes {
            Some(attributes) => attributes.borrow().buffer.clone(),
            None => vertices,
        };
        let mut buf = Vec::with_capacity(1 + buffers.len());
        buf.push((vertex_stage, 11, attributes, 0));
        buf.extend(buffers);
        buffers = buf;
    }
    match (&slot_uniform, item.uniform_offset) {
        (Some(uniform), _) => {
            let binding = |stage: ShaderStage| (stage, 6, uniform.buffer.clone(), uniform.offset);
            let mut all = vec![binding(vertex_stage)];
            if !direct {
                let offset = match &transform_uniforms {
                    Some(b) if blen(b) > 5456 => uniform.offset + 5456,
                    _ => uniform.offset,
                };
                all.push((vertex_stage, 7, uniform.buffer.clone(), offset));
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
        Scene2 | Scene2Textured => regenerated(buffers, textures, samplers, &[((v, 6), vec![1])], &[], &[]),
        Scene3 | Scene3Points if slot_gpu_vertices.is_some() => {
            regenerated(buffers, textures, samplers, &device_scene3(&scene3), &[(1, 6)], &[(2, 7)])
        }
        Scene3 | Scene3Points | Scene3Textured => regenerated(buffers, textures, samplers, &scene3, &[(1, 6)], &[(2, 7)]),
        Scene3World => {
            let base = if slot_gpu_vertices.is_some() { device_scene3(&scene3) } else { scene3 };
            let mut table = vec![((f, 3), vec![6])];
            table.extend(base);
            regenerated(buffers, textures, samplers, &table, &[(1, 7), (4, 9), (8, 13), (9, 11)], &[(2, 8), (5, 10), (10, 12)])
        }
        Marks => (buffers, textures, samplers),
    };
    let gpu_indices = slot_gpu_vertices.as_ref().and_then(|source| source.indices.clone());
    let index = {
        let (buffer, offset, count) = match gpu_indices {
            Some((buffer, count)) => (buffer, 0, count),
            None => (item.buffer.clone(), item.index_offset, item.index_count),
        };
        Some((IndexType::Uint32, buffer, offset, count))
    };
    let buffers = match slot_gpu_vertices.as_ref().and_then(|source| source.colors.clone()) {
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
        (Scene3 | Scene3Points | Scene3Textured | Scene3World, Some(b)) if blen(b) > 5456 && (blen(b) - 5456).is_multiple_of(192) => {
            (blen(b) as i64 - 5456) / 192
        }
        _ => 1,
    };
    let driver_pipeline = value.pipelines[variant].pipeline.clone();
    Ok(PlanDraw {
        draw: BatchDraw {
            pipeline: driver_pipeline,
            buffers,
            primitive: item.primitive,
            index,
            vertex_start: 0,
            vertex_count: item.vertex_count,
            instances,
        },
        textures,
        samplers,
        resident,
        indirect: slot_gpu_vertices.as_ref().and_then(|source| source.indirect.clone()),
    })
}

fn batch_resources_of(value: &SceneExecution, draws: &[PlanDraw]) -> Vec<ResourceUse> {
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
        for (_, _, b, _) in &draw.draw.buffers {
            buffer(b, &mut resources);
        }
        if let Some((_, b, _, _)) = &draw.draw.index {
            buffer(b, &mut resources);
        }
        if let Some((b, _)) = &draw.indirect {
            buffer(b, &mut resources);
        }
        for (_, _, tx) in &draw.textures {
            texture_seen(tx, &mut resources);
        }
        for tx in &draw.resident {
            texture_seen(tx, &mut resources);
        }
    }
    if let Some(pool) = &value.arguments {
        buffer(&pool.buffer, &mut resources);
    }
    resources
}

fn plan_batches(value: &mut SceneExecution) -> Result<Vec<Rc<BatchPlan>>, Error> {
    let length = value.prepared_scratch.length;
    let mut planned: Vec<Rc<BatchPlan>> = Vec::new();
    let mut first = true;
    let mut index = 0;
    while index < length {
        let (family, samples, state) = {
            let slot = &value.prepared_scratch.slots[index];
            (slot.family, slot.samples, slot.state.clone().expect("slot state"))
        };
        let class_ = attachment_class(family);
        let mut next = index + 1;
        while next < length && next - (index + 1) < 65_535 {
            let slot = &value.prepared_scratch.slots[next];
            if attachment_class(slot.family) == class_
                && slot.samples == samples
                && same_pass_state(class_, &state, slot.state.as_ref().expect("slot state"))
            {
                next += 1;
            } else {
                break;
            }
        }
        let mut draws: Vec<PlanDraw> = Vec::with_capacity(next - index);
        for slot_index in index..next {
            draws.push(plan_entry(value, slot_index)?);
        }
        let batch = BatchPlan {
            family,
            samples,
            state,
            first,
            driver_draws: draws.iter().map(|d| d.draw.clone()).collect(),
            resources: batch_resources_of(value, &draws),
            draws,
            icb: RefCell::new(None),
        };
        planned.push(Rc::new(batch));
        first = false;
        index = next;
    }
    if first {
        let width = value.configuration.physical_width;
        let height = value.configuration.physical_height;
        let state = State {
            viewport: (0, 0, width, height),
            scissor: (0, 0, width, height),
            cull: Cull::CullNone,
            depth_compare: Comparison::Always,
            depth_write: false,
            depth_load: Load::Load,
            depth_clear: 1.0,
            transform_uniforms: None,
        };
        planned.push(Rc::new(BatchPlan {
            family: Scene2,
            samples: 1,
            state,
            first: true,
            draws: vec![],
            driver_draws: vec![],
            resources: vec![],
            icb: RefCell::new(None),
        }));
    }
    Ok(planned)
}

fn finish_prepared_frame(
    value: &mut SceneExecution,
    frame: Acquired,
    clear: (f64, f64, f64, f64),
    prepared_key: &Option<(String, i64)>,
    draw_count: i64,
    uniform_slices: Vec<Option<UniformSlice>>,
    after_prepare: &mut dyn FnMut(),
) -> Result<bool, Error> {
    after_prepare();
    let mut signatures: Vec<AutomaticSignature> =
        value.prepared_scratch.slots[..value.prepared_scratch.length].iter().map(automatic_signature).collect();
    let hit = match &value.automatic_submission {
        Some(plan)
            if plan.clear == clear
                && plan.gpu_vertices.iter().all(gpu_vertices_valid)
                && plan.payloads == signatures =>
        {
            Some(plan.clone())
        }
        _ => None,
    };
    if let Some(plan) = hit {
        value.automatic_candidate = None;
        value.icb_stats.hits += 1;
        clear_prepared_scratch(&mut value.prepared_scratch);
        // A prepared key claims the identical automatic plan, so its replay works.
        if let Some((identity, version)) = prepared_key {
            value.automatic_submission = None;
            value.prepared_submission = Some(PreparedSubmission {
                identity: identity.clone(),
                version: *version,
                draw_count,
                plan: plan.clone(),
            });
        }
        return encode_frame(value, &frame, clear, &plan.batches).map(|()| true);
    }
    value.icb_stats.misses += 1;
    let admit = value.automatic_candidate.as_ref().is_some_and(|c| c.clear == clear && c.payloads == signatures);
    // a plan nothing keeps (a frame unlike the last, without a prepared key) is encoded only
    let retained = prepared_key.is_some() || admit;
    let plan_batches_ = match plan_batches(value) {
        Ok(planned) => planned,
        Err(e) => {
            discard(&frame);
            return Err(e);
        }
    };
    // an admitted plan takes the signatures; otherwise the next frame compares against them
    let payloads = if !retained {
        Vec::new()
    } else if admit {
        std::mem::take(&mut signatures)
    } else {
        signatures.clone()
    };
    let gpu_vertices: Vec<GpuVertices> = if retained {
        value.prepared_scratch.slots[..value.prepared_scratch.length].iter().rev().filter_map(|slot| slot.gpu_vertices.clone()).collect()
    } else {
        Vec::new()
    };
    clear_prepared_scratch(&mut value.prepared_scratch);
    encode_frame(value, &frame, clear, &plan_batches_)?;
    value.previous_uniforms = uniform_slices;
    let plan = retained.then(|| {
        Rc::new(ReplayPlan { clear, payloads, batches: plan_batches_, gpu_vertices })
    });
    if let Some(plan) = &plan {
        for batch in &plan.batches {
            if let Err(e) = build_icb(value, batch) {
                value.icb_stats.failures += 1;
                value.icb_stats.last_failure = Some(error::to_string(&e));
            }
        }
    }
    if let Some((identity, version)) = prepared_key {
        value.prepared_submission = Some(PreparedSubmission {
            identity: identity.clone(),
            version: *version,
            draw_count,
            plan: plan.clone().expect("a prepared key retains its plan"),
        });
    }
    let old = value.automatic_submission.take();
    drop_plan(value, old);
    if admit {
        value.automatic_submission = plan;
        value.automatic_candidate = None;
    } else {
        value.automatic_submission = None;
        value.automatic_candidate = Some(AutomaticCandidate { clear, payloads: signatures });
    }
    Ok(true)
}

/// Evicted and oversize resources are valid through this synchronous submission, but not through
/// another replay. Retained plans borrow cache resources; drop both replay paths before releasing
/// any of them, including on preparation/submission failure.
fn finish_submit(value: &mut SceneExecution, deferred_items: Vec<Deferred>, result: Result<bool, Error>) -> Result<bool, Error> {
    let retired_pending = {
        let r = value.retired.borrow();
        !r.buffers.is_empty() || !r.textures.is_empty()
    };
    if !deferred_items.is_empty() || retired_pending {
        drop_plans(value);
    }
    for item in deferred_items.iter().rev() {
        match item {
            Deferred::Buffer(b) => {
                let _ = backend::destroy_buffer(b);
            }
            Deferred::Texture(tx) => {
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
    value: &mut SceneExecution,
    draws: &[Draw],
) -> Result<bool, Error> {
    let clear = clear.unwrap_or((0.0, 0.0, 0.0, 0.0));
    if value.dead {
        after_prepare();
        return Err(failure_error("Renderer.submit", ErrorKind::StaleHandle, "renderer is destroyed"));
    }
    if draws.len() > PREPARED_SCRATCH_CAPACITY {
        after_prepare();
        return Err(failure_error("Renderer.submit", ErrorKind::Capacity, "one submission exceeds the bounded prepared-draw capacity"));
    }
    if let (Some((identity, version)), Some(cached)) = (&prepared, &value.prepared_submission)
        && cached.identity == *identity && cached.version == *version && cached.plan.clear == clear {
            let plan = cached.plan.clone();
            after_prepare();
            return replay_plan(value, &plan);
        }
    let old = value.prepared_submission.take().map(|s| s.plan);
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
    let draws: Vec<Draw> = draws.into_iter().filter(|entry| entry.draw.mesh.vertex_count > 0 && entry.draw.mesh.index_count > 0).collect();
    let valid_mesh = |mesh: &Mesh| {
        !mesh_key_string(&mesh.key).is_empty()
            && mesh.vertex_count > 0
            && mesh.index_count > 0
            && valid_index_length(mesh)
            && (mesh.shared_vertices.is_none() || blen(&mesh.vertices)==0)
            && (mesh.vertex_bytes_len() + blen(&mesh.indices) > 0 || matches!(mesh.key, MeshKey::GpuVertices { .. }))
    };
    let supported = draws.iter().all(|entry| find_pipeline(value, entry.family, entry.blend, entry.samples).is_some());
    let valid = draws.iter().all(|entry| {
        [1, 4, 9, 16].contains(&entry.samples)
            && valid_mesh(&entry.draw.mesh)
            && entry.texture.as_ref().is_none_or(|texture| valid_texture(texture))
            // a Scene2 mesh is untextured, a Scene2 quad samples its texture
            && match entry.family {
                Scene2 => entry.texture.is_none(),
                Scene2Textured => entry.texture.is_some(),
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
    let mut deferred_items: Vec<Deferred> = Vec::new();
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

struct DrawMode {
    scene2: bool,
    affine: bool,
    scene3_transform: bool,
    source: Option<bytes>,
}

fn submit_prepared(
    value: &mut SceneExecution,
    defer: &mut Vec<Deferred>,
    prepared_key: &Option<(String, i64)>,
    clear: (f64, f64, f64, f64),
    draws: &[Draw],
    after_prepare: &mut dyn FnMut(),
) -> Result<bool, Error> {
    let modes: Vec<DrawMode> = draws
        .iter()
        .map(|entry| {
            let family = entry.family;
            let draw = &entry.draw;
            let scene2 = family == Scene2 || family == Scene2Textured;
            let affine = (family == Ui || family == Marks)
                && draw.state.transform_uniforms.as_ref().is_some_and(|b| blen(b) == 24 || blen(b) == 48);
            let scene3_transform = !scene2
                && draw.state.transform_uniforms.as_ref().is_some_and(|b| blen(b) >= 5456 && (blen(b) - 5456).is_multiple_of(192));
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
            DrawMode { scene2, affine, scene3_transform, source }
        })
        .collect();
    let sources: Vec<Option<bytes>> = modes.iter().map(|m| m.source.clone()).collect();
    let frame = match acquire(value) {
        Err(e) => {
            after_prepare();
            return Err(e);
        }
        Ok(Acquisition::Skipped) => {
            after_prepare();
            return Ok(false);
        }
        Ok(Acquisition::Acquired(frame)) => frame,
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
    value: &mut SceneExecution,
    defer: &mut Vec<Deferred>,
    draws: &[Draw],
    modes: &[DrawMode],
    uniform_slices: &[Option<UniformSlice>],
) -> Result<(), Error> {
    for entry in draws {
        let position = value.prepared_scratch.length;
        let mode = &modes[position];
        let (family, draw) = (entry.family, &entry.draw);
        let gpu_vertices = resolve_gpu_vertices(value, entry)?;
        let cpu_mesh = match &gpu_vertices {
            None => draw.mesh.clone(),
            Some(source) if source.indices.is_some() => draw.mesh.clone(),
            Some(_) => Mesh {
                key: MeshKey::Host(format!(
                    "gpu:indices:{}{}",
                    draw.mesh.vertex_count,
                    match family {
                        Scene3 | Scene3Points => ":scene3",
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
        let slot = &mut value.prepared_scratch.slots[position];
        slot.family = family;
        slot.blend = entry.blend;
        slot.samples = entry.samples;
        slot.state = Some(draw.state.clone());
        slot.texture = texture;
        slot.auxiliary = auxiliary;
        slot.environment = environment;
        slot.mesh = Some(mesh_);
        slot.uniform = uniform;
        slot.vertex_attributes = attributes;
        slot.gpu_vertices = gpu_vertices;
        value.prepared_scratch.length += 1;
    }
    Ok(())
}

pub fn replay_prepared_sampled_resources(
    clear: Option<(f64, f64, f64, f64)>,
    identity: &str,
    version: i64,
    value: &mut SceneExecution,
) -> Result<Option<(bool, i64)>, Error> {
    let clear = clear.unwrap_or((0.0, 0.0, 0.0, 0.0));
    if value.dead {
        return Err(failure_error("Scene_execution.replay_prepared_sampled_resources", ErrorKind::StaleHandle, "renderer is destroyed"));
    }
    let cached = match &value.prepared_submission {
        Some(cached) if cached.identity == identity && cached.version == version && cached.plan.clear == clear => {
            Some((cached.plan.clone(), cached.draw_count))
        }
        _ => None,
    };
    match cached {
        Some((plan, count)) => replay_plan(value, &plan).map(|presented| Some((presented, count))),
        None => Ok(None),
    }
}

pub fn resize(value: &mut SceneExecution, configuration: &Configuration) -> Result<(), Error> {
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
    if let Some(surface) = &value.surface
        && let Err(e) = backend::configure(surface, configuration.clone()) {
            let _ = backend::destroy_texture(&target);
            scene_attachment_pool::destroy(&mut attachments);
            return Err(e);
        }
    let old = std::mem::replace(&mut value.target, target);
    let mut old_attachments = std::mem::replace(&mut value.attachments, attachments);
    value.configuration = configuration.clone();
    scene_attachment_pool::destroy(&mut old_attachments);
    backend::destroy_texture(&old)
}

pub fn upload_bytes(value: &SceneExecution) -> i64 {
    value.uploaded
}

pub mod private {
    use super::*;

    pub use super::{create_gpu_vertices_owner, GpuVertices, GpuVerticesOwner, register_gpu_vertices, unregister_gpu_vertices};

    pub fn uniform_capacity(value: &SceneExecution) -> i64 {
        value.uniform_pages[0].as_ref().map_or(0, |page| page.capacity)
    }

    pub const UNIFORM_SHRINK_AFTER: i64 = UNIFORM_SHRINK_AFTER_FRAMES;

    /// What the texture upload scratch holds, on the device and on the host each.
    pub fn staging_bytes(value: &SceneExecution) -> i64 {
        value.texture_upload_scratch.as_ref().map_or(0, |scratch| scratch.size)
    }

    /// Whether the device has memoryless attachments, and how many the pool holds that ask for them.
    pub fn memoryless_attachments(value: &SceneExecution) -> (bool, i64) {
        (
            caps::has(&backend::capabilities(&value.device), &Feature::MemorylessAttachments),
            scene_attachment_pool::allocated(&value.attachments).iter().filter(|(_, _, passing, _)| *passing).count() as i64,
        )
    }

    pub fn gpu_vertices_owner(value: &SceneExecution) -> super::GpuVerticesOwner {
        value.gpu_vertices.clone()
    }

    /// A live, valid registration's buffer, extra buffers and vertex count (initial domain).
    pub fn find_gpu_vertices(owner: &super::GpuVerticesOwner, key: &str) -> Option<(Buffer, Vec<Buffer>, i64)> {
        let found = owner.borrow().gpu_vertex_registry.get(key).cloned();
        match found {
            Some(source) if gpu_vertices_valid(&source) => Some((source.buffer.clone(), source.extra.clone(), source.vertex_count)),
            _ => None,
        }
    }

    /// A live, valid registration's borrowed indices (count) and colours (width).
    pub fn find_gpu_streams(owner: &super::GpuVerticesOwner, key: &str) -> Option<(Option<(Buffer, i64)>, Option<(Buffer, i64)>)> {
        let found = owner.borrow().gpu_vertex_registry.get(key).cloned();
        match found {
            Some(source) if gpu_vertices_valid(&source) => Some((source.indices.clone(), source.colors.clone())),
            _ => None,
        }
    }

    pub fn gpu_vertex_count_for_test(owner: &super::GpuVerticesOwner) -> i64 {
        owner.borrow().gpu_vertex_registry.len() as i64
    }

    pub fn cache_count_for_report(value: &mut SceneExecution) -> i64 {
        side_lru::length(&mut value.cache)
    }

    /// The number of draws the renderer encodes for these, after merging neighbours; -1 on error.
    pub fn coalesced_draws_for_test(draws: &[Draw]) -> i64 {
        coalesce_draws(draws).map_or(-1, |draws| draws.len() as i64)
    }
}

/// Indirect-command replay plans: one automatic plan admitted after two identical frames and one
/// identity-keyed prepared plan.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RetainedStats {
    pub hits: i64,
    pub misses: i64,
}

pub fn retained_stats(value: &SceneExecution) -> RetainedStats {
    RetainedStats { hits: value.icb_stats.hits, misses: value.icb_stats.misses }
}

pub fn pipeline_count(value: &SceneExecution) -> i64 {
    value.pipelines.len() as i64
}

pub fn read_pixels(value: &mut SceneExecution, bytes_per_row: i64) -> Result<Vec<u8>, Error> {
    settle(value)?;
    backend::read_texture(&value.target, bytes_per_row)
}

/// Every owned handle is released; the first failure is reported after the remaining releases and
/// device teardown have run.
pub fn destroy(value: &mut SceneExecution) -> Result<(), Error> {
    if value.dead {
        return Ok(());
    }
    let _ = settle(value);
    value.dead = true;
    let mut first: Option<Error> = None;
    let record = |result: Result<(), Error>, first: &mut Option<Error>| {
        if let Err(e) = result
            && first.is_none() {
                *first = Some(e);
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
    value.prepared_scratch.slots = Vec::new();
    for variant in &mut value.pipelines {
        if let Some(argument) = variant.argument.take() {
            record(backend::destroy_argument(&argument), &mut first);
        }
    }
    side_lru::clear(&mut value.cache);
    value.previous_uniforms = Vec::new();
    for page in value.uniform_pages.iter_mut() {
        if let Some(page) = page.take() {
            mem::release(uniform_owner(), page.capacity);
            named("uniform page", backend::destroy_buffer(&page.buffer), &mut first);
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
        record(backend::destroy_buffer(&scratch.buffer), &mut first);
    }
    if let Some(pool) = value.arguments.take() {
        named("argument page", backend::destroy_buffer(&pool.buffer), &mut first);
    }
    for (_, sampler) in std::mem::take(&mut value.samplers) {
        record(backend::destroy_sampler(&sampler), &mut first);
    }
    if let Some(sun) = value.sun.take() {
        record(backend::destroy_texture(&sun.color), &mut first);
        record(backend::destroy_texture(&sun.depth), &mut first);
        record(backend::destroy_pipeline(&sun.pipeline), &mut first);
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
                match &value.icb_stats.last_failure {
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
