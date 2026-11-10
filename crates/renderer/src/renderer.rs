// Resource creation and rollback retain their existing dependency order.
//! Isolated native frame coordinator. The public values deliberately contain no SDL or native handles.
use crate::renderer_leases::{self, Policy as lease_policy};
use crate::renderer_lowering;
use crate::renderer_pipeline;
use crate::renderer_retained_plans::{self, Plan, Resource as retained_resource, Segment};
use crate::scene_execution::{self, Bytes, GeometryDraw, new_bytes, PreparedScene3, SampledDraw, SampledTexture, TextureLevel};
use crate::side_lru::{self, Item, TableKey};
use ogpu_core::backend::{self, Buffer, Device, Queue, Texture};
use ogpu_core::error::{self as oerror, Error, ErrorKind};
use ogpu_core::pipeline::Blend;
use ogpu_core::types::{AddressMode, BufferDescriptor, BufferUsage, MipFilter, SamplerDescriptor, SamplerFilter};
use runtime::{FrameFacts, PresentationFacts};
use runtime_resources::{assets, canvas, image, text};
use scene_command::render_ir::{self as ir, Command, RenderIr};
use scene_command::ui_batch::Table as Shapes;
use scene_command::marks;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};

pub use crate::renderer_retained_plans::Resource;
pub use crate::scene_execution::PipelineFamily;
pub use crate::scene_execution::Draw;


pub fn pp_error(formatter: &mut dyn std::fmt::Write, value: &Error) {
    let _ = formatter.write_str(&oerror::to_string(value));
}

fn fail_error(operation: &str, kind: ErrorKind, message: &str) -> Error {
    oerror::make(operation, kind, message)
}

fn resource_error_value(operation: &str, value: &Error) -> Error {
    fail_error(operation, if value.kind == ErrorKind::Capacity { ErrorKind::Capacity } else { ErrorKind::InvalidState }, &oerror::to_string(value))
}

#[derive(Clone, Debug, PartialEq)]
pub struct Configuration {
    pub logical_width: i64,
    pub logical_height: i64,
    pub drawable_width: i64,
    pub drawable_height: i64,
    pub title: String,
    pub vsync: bool,
    /// a drawable at the display's native density (false: the logical size)
    pub high_density: bool,
}

pub fn default_configuration() -> Configuration {
    Configuration {
        logical_width: 640,
        logical_height: 480,
        drawable_width: 640,
        drawable_height: 480,
        title: "Rays".to_owned(),
        vsync: true,
        high_density: true,
    }
}

pub struct GpuData {
    device: Device,
    queue: Queue,
    owner: GpuOwner,
    shared: bool,
    released: Cell<bool>,
}

/// A borrowed GPU: the active window's OGPU device (so Scene samples GPU film textures directly) or,
/// without a window, a lazily created headless device shared by all leases. Every lease owns its own
/// queue. Release it while SDL is alive; the headless device goes away with the last lease.
#[derive(Clone)]
pub struct Gpu(Rc<GpuData>);

pub struct GpuOwnerData {
    scene_vertices: scene_execution::GpuVerticesOwner,
    marks_registry: RefCell<HashMap<i64, GpuMarks>>,
    shared_leases: Cell<i64>,
}

pub type GpuOwner = Rc<GpuOwnerData>;

/// A `Marks`-family source: the attribute buffers of one mark set. A host attribute is uploaded into
/// a buffer the source owns; a device attribute is another producer's output, bound directly and
/// valid only while that producer's output is current.
#[derive(Clone)]
pub enum MarksSource {
    Host(Bytes, i64),
    Device(Rc<dyn Fn() -> Option<Buffer>>, i64),
}

pub struct GpuMarksData {
    id: i64,
    gpu: Gpu,
    domain: std::thread::ThreadId,
    closed: Cell<bool>,
    stamp: Cell<i64>,
    count: Cell<i64>,
    owned: RefCell<Vec<Option<Buffer>>>,
    last: RefCell<Vec<Bytes>>,
    device: RefCell<Vec<(Rc<dyn Fn() -> Option<Buffer>>, Buffer)>>,
    registration: RefCell<Option<scene_execution::GpuVertices>>,
}

#[derive(Clone)]
pub struct GpuMarks(Rc<GpuMarksData>);

const GPU_MARKS_CAPACITY: usize = 64;
static NEXT_GPU_MARKS: AtomicI64 = AtomicI64::new(1);

fn fresh_gpu_marks_id() -> Option<i64> {
    let current = NEXT_GPU_MARKS.fetch_add(1, Ordering::SeqCst);
    if current == i64::MAX { None } else { Some(current) }
}

fn create_gpu_owner() -> GpuOwner {
    Rc::new(GpuOwnerData {
        scene_vertices: scene_execution::private::create_gpu_vertices_owner(),
        marks_registry: RefCell::new(HashMap::new()),
        shared_leases: Cell::new(0),
    })
}

fn same_buffer(a: &Buffer, b: &Buffer) -> bool {
    backend::buffer_id(a) == backend::buffer_id(b)
}

fn valid_gpu_marks(value: &GpuMarks) -> bool {
    let v = &value.0;
    !v.closed.get()
        && !v.gpu.0.released.get()
        && v.domain == std::thread::current().id()
        && v.device.borrow().iter().all(|(source, input)| match source() {
            Some(current) => same_buffer(&current, input),
            None => false,
        })
}

fn gpu_marks_key(value: &GpuMarks) -> String {
    format!("marks:{}:{}", value.0.id, value.0.stamp.get())
}

fn unregister_marks(value: &GpuMarks) {
    let registration = value.0.registration.borrow_mut().take();
    if let Some(registration) = registration {
        scene_execution::private::unregister_gpu_vertices(&registration);
    }
}

pub fn close_gpu_marks(value: &GpuMarks) {
    let v = &value.0;
    if v.domain != std::thread::current().id() {
        unreachable!("GPU marks closed on another domain");
    }
    if !v.closed.get() {
        v.closed.set(true);
        unregister_marks(value);
        v.device.borrow_mut().clear();
        v.gpu.0.owner.marks_registry.borrow_mut().remove(&v.id);
        for owned in v.owned.borrow_mut().iter_mut() {
            if let Some(b) = owned.take() {
                let _ = backend::destroy_buffer(&b);
            }
        }
    }
}


#[derive(Clone, Copy, PartialEq, Eq)]
enum SubmissionState {
    Open,
    Closed,
}

pub use crate::renderer_leases::Policy;

struct SnapshotCacheEntry {
    generation: i64,
    width: i64,
    height: i64,
    texture: Rc<SampledTexture>,
}

const SNAPSHOT_CACHE_CAPACITY: i64 = 256;
const SNAPSHOT_CACHE_BYTE_CAPACITY: i64 = 64 * 1024 * 1024;

// Full-window snapshots change frequently and already have bounded double buffers in [Image].
// Keeping entries below 1 MiB admits UI/text assets but deliberately leaves a 640x480 RGBA frame
// (1,228,800 bytes) transient.
const SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY: i64 = renderer_lowering::SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY;

struct StatsCounters {
    logical_draws: i64,
    logical_passes: i64,
}

fn count_draws(counters: &mut StatsCounters, count: i64) {
    counters.logical_draws += count;
    counters.logical_passes += 1;
}

pub struct ScaledCache {
    input: Vec<Draw>,
    facts: Option<FrameFacts>,
    output: Vec<Draw>,
}

pub fn new_scaled_cache() -> ScaledCache {
    ScaledCache { input: Vec::new(), facts: None, output: Vec::new() }
}

fn scale_required(facts: &FrameFacts) -> bool {
    facts.logical_width != facts.drawable_width || facts.logical_height != facts.drawable_height
}

/// `None` when the draw is unchanged.
fn scale_draw(facts: &FrameFacts, draw_: &GeometryDraw) -> Option<GeometryDraw> {
    let viewport = runtime::map_logical_rect(facts, draw_.state.viewport);
    let scissor = runtime::map_logical_rect(facts, draw_.state.scissor);
    if viewport == draw_.state.viewport && scissor == draw_.state.scissor {
        None
    } else {
        let mut scaled = draw_.clone();
        scaled.state.viewport = viewport;
        scaled.state.scissor = scissor;
        Some(scaled)
    }
}

pub fn scale_draws(facts: &FrameFacts, draws: &[GeometryDraw]) -> Vec<GeometryDraw> {
    if !scale_required(facts) {
        draws.to_vec()
    } else {
        draws.iter().map(|d| scale_draw(facts, d).unwrap_or_else(|| d.clone())).collect()
    }
}

pub fn scale_sampled_resources(facts: &FrameFacts, draws: &[Draw]) -> Vec<Draw> {
    if !scale_required(facts) {
        return draws.to_vec();
    }
    draws
        .iter()
        .map(|entry| match scale_draw(facts, &entry.draw) {
            None => entry.clone(),
            Some(scaled) => {
                let mut copy = (**entry).clone();
                copy.draw = scaled;
                Rc::new(copy)
            }
        })
        .collect()
}

fn same_draw_list(left: &[Draw], right: &[Draw]) -> bool {
    left.len() == right.len() && left.iter().zip(right.iter()).all(|(a, b)| Rc::ptr_eq(a, b))
}

pub fn scale_sampled_cached(cache: &mut ScaledCache, facts: &FrameFacts, draws: &[Draw]) -> Vec<Draw> {
    scale_sampled_ref(cache, facts, draws).to_vec()
}

pub(crate) fn scale_sampled_ref<'a>(cache: &'a mut ScaledCache, facts: &FrameFacts, draws: &'a [Draw]) -> &'a [Draw] {
    if !scale_required(facts) { return draws; }
    if !same_draw_list(draws, &cache.input) || cache.facts.as_ref() != Some(facts) {
        cache.input.clear();
        cache.input.extend_from_slice(draws);
        cache.facts = Some(*facts);
        cache.output.clear();
        cache.output.extend(draws.iter().map(|entry| match scale_draw(facts, &entry.draw) {
            None => entry.clone(),
            Some(scaled) => {
                let mut copy = (**entry).clone();
                copy.draw = scaled;
                Rc::new(copy)
            }
        }));
    }
    &cache.output
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Stats {
    pub logical_draws: i64,
    pub logical_passes: i64,
    pub pipeline_cache_entries: i64,
    pub uploaded_bytes: i64,
    pub gpu_duration_seconds: f64,
    pub retained_plan_hits: i64,
    pub retained_plan_misses: i64,
    pub sun_shadow_passes: i64,
}

pub const ZERO_STATS: Stats = Stats {
    logical_draws: 0,
    logical_passes: 0,
    pipeline_cache_entries: 0,
    uploaded_bytes: 0,
    gpu_duration_seconds: 0.0,
    retained_plan_hits: 0,
    retained_plan_misses: 0,
    sun_shadow_passes: 0,
};

pub struct RendererData {
    /// the target the owner gave us; the owner destroys it, after the renderer
    runtime: runtime::Target,
    shared_device_lease: bool,
    owns_device_lease: bool,
    gpu_owner: GpuOwner,
    renderer: RefCell<scene_execution::SceneExecution>,
    renderer_facts: Cell<FrameFacts>,
    counters: RefCell<StatsCounters>,
    scaled: RefCell<ScaledCache>,
    assets: RefCell<assets::Assets>,
    dead: Cell<bool>,
    snapshots: RefCell<side_lru::Table>,
    lowering: RefCell<renderer_lowering::Lowering>,
    retained_plans: RefCell<renderer_retained_plans::RetainedPlans>,
    retained_scene2_segment_hits: Cell<i64>,
    retained_scene2_segment_misses: Cell<i64>,
    submissions: RefCell<Vec<Submission>>,
    submission_draws: RefCell<Vec<Draw>>,
    last_step_draws: RefCell<Vec<Draw>>,
    last_step_prepared: RefCell<Vec<Draw>>,
    retained_views: RefCell<Vec<Rc<RetainedView>>>,
    /// the sights of a layer before it gets a target: `retain_soon`, or `retain_live` after a target
    /// was dropped before it paid for itself (a layer that changes every few frames)
    retain_after: Rc<Cell<i64>>,
}

/// The renderer is a shared handle with interior-mutable components.
#[derive(Clone)]
pub struct Renderer(Rc<RendererData>);

/// A 3D layer that kept the same prepared identity on consecutive frames: its draws render once into
/// an offscreen target the size of the window and later frames composite that texture, so a frame
/// that only changes the UI never shades the view again. The identity is the key (prepared layers are
/// immutable), the window's drawable size is checked on every use, and the list is capped, so
/// nothing here can outlive or disagree with the scene it stands for.
pub struct RetainedView {
    layer: Rc<PreparedScene3>,
    drawable: (i64, i64),
    target: RefCell<Option<RetainedTarget>>,
    failed: Cell<bool>,
    /// sights before its target, then the frames the target served
    seen: Cell<i64>,
    /// its owner's `retain_after`
    retain_after: Rc<Cell<i64>>,
}

pub struct RetainedTarget {
    exec: Renderer,
    canvas: canvas::Canvas,
    ir: Rc<RenderIr>,
}

pub struct SubmissionData {
    owner: Renderer,
    submission_state: Cell<SubmissionState>,
    image_leases: renderer_leases::Leases,
}

#[derive(Clone)]
pub struct Submission(Rc<SubmissionData>);

#[derive(Clone)]
pub struct Batch {
    owner: Submission,
    draws: Vec<Draw>,
}

const RETAINED_VIEW_CAPACITY: usize = 4;

// A target costs an offscreen execution (its pipelines, its uploads) and pays back by later frames
// compositing it. A live layer, a new one every few frames (a time-driven SOP), never pays: once a
// target is dropped having served fewer than [retain_live] frames, layers wait [retain_live] sights
// for one, until a target serves that many. ponytail: two fixed thresholds, a measured break-even
// if they misjudge.
const RETAIN_SOON: i64 = 2;
const RETAIN_LIVE: i64 = 8;

// ponytail: one active native window; pass an explicit renderer token if sketches start owning
// multiple windows concurrently.
thread_local! {
    static ACTIVE_WINDOW: RefCell<Option<Renderer>> = const { RefCell::new(None) };
    static HEADLESS_GPU: RefCell<Option<(Device, GpuOwner, i64)>> = const { RefCell::new(None) };
}

/// Adopt an already prepared draw without exposing it again. Scene3 values are retained as a
/// distinct family and never misrouted through a Scene2 pipeline.
pub fn prepared_draw(
    family_: PipelineFamily,
    blend_: Option<Blend>,
    texture: Option<Rc<SampledTexture>>,
    auxiliary: Option<Rc<scene_execution::AuxiliaryResource>>,
    vertex_attributes: Option<(String, Bytes)>,
    samples: Option<i64>,
    value: GeometryDraw,
) -> Draw {
    Rc::new(SampledDraw { ui: None,
        family: family_,
        blend: blend_.unwrap_or(Blend::Replace),
        texture,
        auxiliary,
        vertex_attributes,
        samples: samples.unwrap_or(1),
        draw: value,
    })
}

// Device leases: the active window's device when one exists, otherwise one lazily created headless
// device shared by every lease and released with the last one. Offscreen executions (Canvas) and GPU
// film producers both lease.

fn release_headless() {
    let destroy = HEADLESS_GPU.with(|cell| {
        let mut cell = cell.borrow_mut();
        match cell.as_mut() {
            Some((device, _, count)) => {
                *count -= 1;
                if *count <= 0 {
                    let device = device.clone();
                    *cell = None;
                    Some(device)
                } else {
                    None
                }
            }
            None => None,
        }
    });
    if let Some(device) = destroy {
        let _ = backend::destroy_device(&device);
    }
}

fn acquire_device(_operation: &str) -> Result<(Device, bool, GpuOwner), Error> {
    let shared = ACTIVE_WINDOW.with(|cell| {
        cell.borrow().as_ref().and_then(|value| {
            if !value.0.dead.get() && runtime::is_window(&value.0.runtime.borrow()) {
                Some((scene_execution::device(&value.0.renderer.borrow()), value.0.gpu_owner.clone()))
            } else {
                None
            }
        })
    });
    if let Some((device, owner)) = shared {
        owner.shared_leases.set(owner.shared_leases.get() + 1);
        return Ok((device, true, owner));
    }
    let existing = HEADLESS_GPU.with(|cell| {
        cell.borrow_mut().as_mut().map(|(device, owner, count)| {
            *count += 1;
            (device.clone(), owner.clone())
        })
    });
    if let Some((device, owner)) = existing {
        return Ok((device, false, owner));
    }
    let driver = ogpu_core::impl_::create_driver()?;
    let device = backend::create_device(driver)?;
    let owner = create_gpu_owner();
    HEADLESS_GPU.with(|cell| *cell.borrow_mut() = Some((device.clone(), owner.clone(), 1)));
    Ok((device, false, owner))
}

fn release_device(shared: bool, owner: &GpuOwner) {
    if shared {
        owner.shared_leases.set(owner.shared_leases.get() - 1);
    } else {
        release_headless();
    }
}

fn valid_configuration(operation: &str, configuration_: &Configuration) -> Result<(), Error> {
    let positive = |x: i64| x > 0;
    if ![
        configuration_.logical_width,
        configuration_.logical_height,
        configuration_.drawable_width,
        configuration_.drawable_height,
    ]
    .iter()
    .all(|x| positive(*x))
    {
        Err(fail_error(operation, ErrorKind::InvalidArgument, "dimensions must be positive"))
    } else {
        Ok(())
    }
}

fn finish_create(shared_device_lease: bool, owns_device_lease: bool, gpu_owner_: GpuOwner, runtime_: runtime::Target, renderer: scene_execution::SceneExecution) -> Renderer {
    let facts = runtime::frame_facts(&runtime_.borrow());
    Renderer(Rc::new(RendererData {
        runtime: runtime_,
        renderer: RefCell::new(renderer),
        gpu_owner: gpu_owner_,
        renderer_facts: Cell::new(facts),
        counters: RefCell::new(StatsCounters { logical_draws: 0, logical_passes: 0 }),
        scaled: RefCell::new(new_scaled_cache()),
        shared_device_lease,
        owns_device_lease,
        assets: RefCell::new(assets::create()),
        dead: Cell::new(false),
        snapshots: RefCell::new(side_lru::create(
            lru::cache("renderer.snapshot"),
            true,
            SNAPSHOT_CACHE_BYTE_CAPACITY,
            Box::new(|_, _| {}),
            SNAPSHOT_CACHE_CAPACITY,
        )),
        lowering: RefCell::new(renderer_lowering::create()),
        retained_plans: RefCell::new(renderer_retained_plans::create()),
        retained_scene2_segment_hits: Cell::new(0),
        retained_scene2_segment_misses: Cell::new(0),
        submissions: RefCell::new(Vec::new()),
        submission_draws: RefCell::new(Vec::new()),
        last_step_draws: RefCell::new(Vec::new()),
        last_step_prepared: RefCell::new(Vec::new()),
        // count only: views of layers the scene's cache charges
        retained_views: RefCell::new(Vec::new()),
        retain_after: Rc::new(Cell::new(RETAIN_SOON)),
    }))
}

fn backend_name() -> Result<String, Error> {
    ogpu_core::impl_::backend()
}

/// A presenting execution drawing to `target`, a window its owner opened (`runtime::open_window`)
/// and keeps: destroy the renderer first, then the target.
pub fn create(configuration_: &Configuration, target: &runtime::Target) -> Result<Renderer, Error> {
    let operation = "Renderer.create";
    valid_configuration(operation, configuration_)?;
    let surface = runtime::surface_configuration(&mut target.borrow_mut())?;
    let gpu_owner_ = create_gpu_owner();
    let driver = ogpu_core::impl_::create_driver()?;
    let renderer = renderer_pipeline::create_renderer(&backend_name()?, None, Some(gpu_owner_.scene_vertices.clone()), false, driver, &surface)?;
    let value = finish_create(false, false, gpu_owner_, target.clone(), renderer);
    ACTIVE_WINDOW.with(|cell| *cell.borrow_mut() = Some(value.clone()));
    Ok(value)
}

/// A true layerless native target. It owns one long-lived device and pipeline coordinator until
/// `destroy`, without acquiring/presenting a window.
pub fn create_offscreen(configuration_: &Configuration) -> Result<Renderer, Error> {
    let operation = "Renderer.create_offscreen";
    valid_configuration(operation, configuration_)?;
    let (device, lease_shared, gpu_owner_) = acquire_device(operation)?;
    let mut runtime_ = match runtime::create_offscreen(
        configuration_.logical_width,
        configuration_.logical_height,
        configuration_.drawable_width,
        configuration_.drawable_height,
    ) {
        Ok(runtime_) => runtime_,
        Err(e) => {
            release_device(lease_shared, &gpu_owner_);
            return Err(e);
        }
    };
    let surface = match runtime::surface_configuration(&mut runtime_) {
        Ok(surface) => surface,
        Err(e) => {
            let _ = runtime::destroy(&mut runtime_);
            release_device(lease_shared, &gpu_owner_);
            return Err(e);
        }
    };
    let renderer = (|| -> Result<scene_execution::SceneExecution, Error> {
        let driver = ogpu_core::impl_::create_driver()?;
        renderer_pipeline::create_renderer(&backend_name()?, Some(device), Some(gpu_owner_.scene_vertices.clone()), true, driver, &surface)
    })();
    match renderer {
        Err(e) => {
            let _ = runtime::destroy(&mut runtime_);
            release_device(lease_shared, &gpu_owner_);
            Err(e)
        }
        Ok(renderer) => Ok(finish_create(lease_shared, true, gpu_owner_, Rc::new(RefCell::new(runtime_)), renderer)),
    }
}

fn ensure(operation: &str, value: &Renderer) -> Result<(), Error> {
    if value.0.dead.get() { Err(fail_error(operation, ErrorKind::StaleHandle, "renderer is destroyed")) } else { Ok(()) }
}

fn sync_runtime_facts(_operation: &str, value: &Renderer) -> Result<(), Error> {
    let mut rt = value.0.runtime.borrow_mut();
    runtime::sync_facts(&mut rt)?;
    let facts = runtime::frame_facts(&rt);
    if facts == value.0.renderer_facts.get() {
        return Ok(());
    }
    let configuration_ = runtime::surface_configuration(&mut rt)?;
    scene_execution::resize(&mut value.0.renderer.borrow_mut(), &configuration_)?;
    value.0.renderer_facts.set(facts);
    Ok(())
}

fn close_submission(submission_: &Submission) {
    if submission_.0.submission_state.get() == SubmissionState::Open {
        submission_.0.submission_state.set(SubmissionState::Closed);
        renderer_leases::release(&submission_.0.image_leases);
        submission_.0.owner.0.submissions.borrow_mut().retain(|s| !Rc::ptr_eq(&s.0, &submission_.0));
    }
}

pub fn begin_submission(value: &Renderer) -> Result<Submission, Error> {
    ensure("Renderer.Native.begin_submission", value)?;
    let submission_ = Submission(Rc::new(SubmissionData {
        owner: value.clone(),
        submission_state: Cell::new(SubmissionState::Open),
        image_leases: renderer_leases::create(),
    }));
    value.0.submissions.borrow_mut().insert(0, submission_.clone());
    Ok(submission_)
}

fn ensure_submission(operation: &str, submission_: &Submission) -> Result<(), Error> {
    if submission_.0.submission_state.get() == SubmissionState::Closed {
        Err(fail_error(operation, ErrorKind::StaleHandle, "submission is already closed"))
    } else {
        ensure(operation, &submission_.0.owner)
    }
}

/// Releases what only a further frame would reuse: the texture staging that frames which each
/// uploaded a texture keep for the next. For an owner going idle, or a reading that must not depend
/// on whether the last two frames uploaded.
pub fn release_staging(value: &Renderer) -> Result<(), Error> {
    ensure("Renderer.release_staging", value)?;
    scene_execution::release_staging(&mut value.0.renderer.borrow_mut());
    Ok(())
}

pub fn stats(value: &Renderer) -> Result<Stats, Error> {
    ensure("Renderer.stats", value)?;
    let renderer = value.0.renderer.borrow();
    let timing = backend::gpu_timing(&scene_execution::queue(&renderer));
    let retained = scene_execution::retained_stats(&renderer);
    let counters = value.0.counters.borrow();
    Ok(Stats {
        logical_draws: counters.logical_draws,
        logical_passes: counters.logical_passes,
        pipeline_cache_entries: scene_execution::pipeline_count(&renderer),
        uploaded_bytes: scene_execution::upload_bytes(&renderer),
        gpu_duration_seconds: timing.gpu_seconds,
        retained_plan_hits: retained.hits,
        retained_plan_misses: retained.misses,
        sun_shadow_passes: scene_execution::sun_shadow_passes(&renderer),
    })
}

pub fn presentation_facts_of(value: &Renderer) -> Result<PresentationFacts, Error> {
    runtime::presentation_facts(&mut value.0.runtime.borrow_mut())
}

fn scene_image_sampler() -> SamplerDescriptor {
    SamplerDescriptor {
        label: Some("scene-image".to_owned()),
        min_filter: SamplerFilter::Linear,
        mag_filter: SamplerFilter::Linear,
        mip_filter: MipFilter::NoMip,
        address_u: AddressMode::ClampToEdge,
        address_v: AddressMode::ClampToEdge,
        lod_min: 0.0,
        lod_max: 0.0,
        max_anisotropy: 1,
    }
}

fn snapshot_key(key: &str, density: i64) -> TableKey {
    TableKey::Str(format!("{}|{}", key, density))
}

fn snapshot_entry(value: &Renderer, key: &str, density: i64) -> Option<Rc<RefCell<SnapshotCacheEntry>>> {
    side_lru::find(&mut value.0.snapshots.borrow_mut(), &snapshot_key(key, density))
        .map(|item| item.downcast::<RefCell<SnapshotCacheEntry>>().unwrap_or_else(|_| unreachable!("snapshot table item")))
}

fn texture_key(key: &str, density: i64, generation: i64) -> String {
    format!("{}:{}:{}", key, density, generation)
}

fn gpu_texture(key: String, width: i64, height: i64, gpu_: Texture) -> Rc<SampledTexture> {
    Rc::new(SampledTexture {
        key,
        levels: Rc::new(vec![TextureLevel { width, height, bytes: scene_execution::empty_bytes() }]),
        sampler: scene_image_sampler(),
        gpu: Some(gpu_),
    })
}

fn snapshot(value: &Renderer, policy_: &lease_policy, density: i64, source: retained_resource) -> Result<(i64, i64, Rc<SampledTexture>), Error> {
    let operation = "Renderer.lower_scene2";
    if density <= 0 {
        return Err(fail_error(operation, ErrorKind::InvalidArgument, "density must be positive"));
    }
    let find = |key: &str, generation: i64| -> Option<(i64, i64, Rc<SampledTexture>)> {
        let entry = snapshot_entry(value, key, density)?;
        let e = entry.borrow();
        if e.generation == generation {
            Some((e.width, e.height, e.texture.clone()))
        } else {
            None
        }
    };
    let store = |key: &str, generation: i64, width: i64, height: i64, texture: Rc<SampledTexture>, bytes_: i64| {
        let mut table = value.0.snapshots.borrow_mut();
        if bytes_ <= SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY {
            let item: Item = Rc::new(RefCell::new(SnapshotCacheEntry {
                generation,
                width,
                height,
                texture,
            }));
            side_lru::add(&mut table, bytes_, snapshot_key(key, density), item);
        } else {
            side_lru::remove(&mut table, &snapshot_key(key, density));
        }
    };
    let finish = |copy: bool, key: &str, generation: i64, width: i64, height: i64, pixels: &Bytes| -> Result<(i64, i64, Rc<SampledTexture>), Error> {
        if let Some(cached) = find(key, generation) {
            return Ok(cached);
        }
        let same_shape = snapshot_entry(value, key, density).filter(|entry| {
            let e = entry.borrow();
            e.width == width && e.height == height
        });
        let bytes_ = if copy { runtime_resources::copy_bytes(operation, pixels)? } else { pixels.clone() };
        match same_shape {
            Some(entry) => {
                let mut e = entry.borrow_mut();
                e.generation = generation;
                // The execution key carries the generation: same-key bytes are never re-hashed
                // below, and a new generation recycles the same-shape texture with one upload.
                let mut level = e.texture.levels[0].clone();
                level.bytes = bytes_;
                let texture = Rc::new(SampledTexture {
                    key: texture_key(key, density, generation),
                    levels: Rc::new(vec![level]),
                    ..(*e.texture).clone()
                });
                e.texture = texture.clone();
                Ok((width, height, texture))
            }
            None => {
                let texture = Rc::new(SampledTexture {
                    key: texture_key(key, density, generation),
                    levels: Rc::new(vec![TextureLevel { width, height, bytes: bytes_.clone() }]),
                    sampler: scene_image_sampler(),
                    gpu: None,
                });
                let size = bytes_.capacity() as i64;
                store(key, generation, width, height, texture.clone(), size);
                Ok((width, height, texture))
            }
        }
    };
    match source {
        retained_resource::Image(image) => {
            if image::destroyed(&image) {
                return Err(fail_error(operation, ErrorKind::StaleHandle, "image snapshot is destroyed"));
            }
            let key = format!("image:{}", image::identity(&image));
            let generation = image::generation(&image);
            match image::gpu_snapshot(&image) {
                Err(e) => Err(resource_error_value(operation, &e)),
                Ok(Some((width, height, generation, gpu_))) => {
                    Ok((width, height, gpu_texture(texture_key(&key, density, generation), width, height, gpu_)))
                }
                Ok(None) => {
                    if let Some(cached) = find(&key, generation) {
                        return Ok(cached);
                    }
                    match image::borrow_snapshot(&image) {
                        Err(e) => Err(resource_error_value(operation, &e)),
                        Ok((width, height, borrowed_generation, pixels, mut lease)) => {
                            let size = pixels.capacity() as i64;
                            let cacheable = size <= SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY;
                            let bytes_ = if cacheable {
                                // Shared image storage stays immutable through copy-on-write,
                                // including after the bounded snapshot lease is released.
                                image::release_snapshot(&mut lease);
                                pixels
                            } else {
                                match policy_ {
                                    lease_policy::RetainImageSnapshots(leases) => {
                                        renderer_leases::retain(leases, lease);
                                        pixels
                                    }
                                    lease_policy::CopyImageSnapshots => {
                                        let copy = runtime_resources::copy_bytes(operation, &pixels);
                                        image::release_snapshot(&mut lease);
                                        copy?
                                    }
                                }
                            };
                            // The generation is part of the GPU texture key: a replaced image (same
                            // identity, new pixels) then misses the executor's static image fast path
                            // and re-uploads into its same-shape texture.
                            let texture = Rc::new(SampledTexture {
                                key: texture_key(&key, density, borrowed_generation),
                                levels: Rc::new(vec![TextureLevel { width, height, bytes: bytes_ }]),
                                sampler: scene_image_sampler(),
                                gpu: None,
                            });
                            if cacheable {
                                store(&key, borrowed_generation, width, height, texture.clone(), size);
                            }
                            Ok((width, height, texture))
                        }
                    }
                }
            }
        }
        retained_resource::Text(text) => {
            // Text snapshots are immutable and uniquely identified, like images: a hit touches no
            // pixels, a miss copies them once.
            let key = format!("text:{}", text::identity(&text));
            let generation = text::generation(&text);
            if !text::destroyed(&text)
                && let Some(cached) = find(&key, generation) {
                    return Ok(cached);
                }
            match (text::size(&text), text::borrow_pixels(&text)) {
                (Ok((width, height)), Ok(pixels)) => finish(true, &key, generation, width, height, &pixels),
                (Err(e), _) | (_, Err(e)) => Err(resource_error_value(operation, &e)),
            }
        }
        retained_resource::Canvas(canvas) => {
            let key = format!("canvas:{}", canvas::identity(&canvas));
            // A canvas rendered on this window's device is sampled in place.
            let gpu_ = match canvas::gpu_snapshot(&canvas) {
                Some((width, height, generation, texture)) if runtime::is_window(&value.0.runtime.borrow()) => {
                    let (device_id, _) = backend::texture_driver_token(&texture);
                    let device = scene_execution::device(&value.0.renderer.borrow());
                    if ogpu_core::handle::device_id(backend::device_handle(&device)) == device_id {
                        Some((width, height, generation, texture))
                    } else {
                        None
                    }
                }
                _ => None,
            };
            match gpu_ {
                Some((width, height, generation, texture)) => {
                    Ok((width, height, gpu_texture(texture_key(&key, density, generation), width, height, texture)))
                }
                None => match canvas::snapshot(&canvas) {
                    Ok((width, height, generation, pixels)) => finish(false, &key, generation, width, height, &new_bytes(pixels)),
                    Err(e) => Err(resource_error_value(operation, &e)),
                },
            }
        }
    }
}

fn lower_scene2_uncached(
    value: &Renderer,
    policy_: &lease_policy,
    density: i64,
    resolve: &dyn Fn(i64) -> Option<retained_resource>,
    ir_: &Rc<RenderIr>,
) -> Result<Vec<Draw>, Error> {
    ensure("Renderer.lower_scene2", value)?;
    let checkpoint = renderer_leases::checkpoint(policy_);
    let rollback = || renderer_leases::rollback(policy_, checkpoint);
    let resolve_gpu = |shapes: &Shapes| -> Result<Option<String>, String> {
        let Some(token) = shapes.marks() else { return Ok(None) };
        let identity = marks::gpu_identity(token);
        let source = value.0.gpu_owner.marks_registry.borrow().get(&identity).cloned();
        match source {
            Some(source)
                if valid_gpu_marks(&source)
                    && source.0.stamp.get() == marks::gpu_stamp(token)
                    && source.0.count.get() == marks::gpu_count(token) =>
            {
                let device = scene_execution::device(&value.0.renderer.borrow());
                if ogpu_core::handle::device_id(backend::device_handle(&source.0.gpu.0.device)) == ogpu_core::handle::device_id(backend::device_handle(&device)) {
                    Ok(Some(gpu_marks_key(&source)))
                } else {
                    Err("GPU shapes belong to another device".to_owned())
                }
            }
            _ => Err("GPU shapes are stale or closed".to_owned()),
        }
    };
    let facts = runtime::presentation_facts(&mut value.0.runtime.borrow_mut()).expect("presentation facts");
    let mut snapshot_ = |density: i64, source: retained_resource| snapshot(value, policy_, density, source);
    renderer_lowering::lower_scene2(
        &mut value.0.lowering.borrow_mut(),
        &facts,
        density,
        &mut snapshot_,
        &resolve_gpu,
        &rollback,
        resolve,
        ir::commands_readonly(ir_),
    )
}

/// A plan holding a GPU shape token is lowered again every time: the token's source can go stale,
/// and only lowering checks that.
fn has_gpu_shapes(commands: &[Command]) -> bool {
    commands.iter().any(|command| matches!(command, Command::Shapes(shapes) if shapes.marks().is_some()))
}

/// Compare command arrays structurally.
fn commands_equal(left: &[Command], right: &[Command]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right.iter()).all(|(a, b)| match (a, b) {
            (Command::Clear(a), Command::Clear(b)) => a == b,
            (Command::SetBlend(a), Command::SetBlend(b)) => a == b,
            (Command::PushClip(a), Command::PushClip(b)) => a == b,
            (Command::PopClip, Command::PopClip) | (Command::PopTransform, Command::PopTransform) => true,
            (Command::PushTransform(a), Command::PushTransform(b)) => a == b,
            (Command::Geometry(a), Command::Geometry(b)) => a == b,
            (Command::Shapes(a), Command::Shapes(b)) => {
                a.count() == b.count() && a.marks() == b.marks() && a.instances() == b.instances()
            }
            (Command::Image(a), Command::Image(b)) => a == b,
            (Command::Glyphs(a), Command::Glyphs(b)) => a == b,
            _ => false,
        })
}

fn sample_hash(values: &[f64]) -> i64 {
    // A sampled hash selects a candidate bucket; verify the complete retained plan exactly after a hit.
    let mut h = values.len() as i64;
    for v in values.iter().take(16) {
        h = rays_math::hash::mix(h, v.to_bits() as i64);
    }
    h
}

fn lower_scene2_with_policy(
    value: &Renderer,
    policy_: &lease_policy,
    density: i64,
    resolve: &dyn Fn(i64) -> Option<retained_resource>,
    ir_: &Rc<RenderIr>,
) -> Result<Vec<Draw>, Error> {
    if value.0.dead.get() {
        return lower_scene2_uncached(value, policy_, density, resolve, ir_);
    }
    let commands = ir::commands_readonly(ir_);
    let ir_id = ir::identity(ir_);
    let facts = presentation_facts_of(value).expect("presentation facts");
    let extent = (facts.logical_width, facts.logical_height, facts.drawable_width, facts.drawable_height);
    let same_context = |p: &Plan| p.density == density && p.extent == extent;
    let slow = || -> Result<Vec<Draw>, Error> {
        let command_count = commands.len();
        let mut fingerprint = command_count as i64;
        let mut mix = |v: i64| fingerprint = rays_math::hash::mix(fingerprint, v);
        for command in commands {
            match command {
                Command::PushTransform(transform) => {
                    for v in [transform.xx, transform.xy, transform.yx, transform.yy, transform.tx, transform.ty] {
                        mix(v.to_bits() as i64);
                    }
                }
                Command::Geometry(geometry) => {
                    mix(sample_hash(&geometry.vertices));
                    mix(geometry.indices.len() as i64 ^ geometry.indices.iter().take(16).fold(0i64, |h, v| rays_math::hash::mix(h, *v)));
                    mix(geometry.color as i64);
                }
                _ => {}
            }
        }
        let (cacheable, resources) = renderer_retained_plans::resource_stamps(resolve, commands);
        let hit = {
            let found = renderer_retained_plans::find(&mut value.0.retained_plans.borrow_mut(), fingerprint);
            found.filter(|p| {
                let p = p.borrow();
                p.command_count == command_count as i64
                    && same_context(&p)
                    && renderer_retained_plans::same_resources(&p.resources, &resources)
                    && commands_equal(ir::commands_readonly(&p.ir), commands)
            })
        };
        if !cacheable {
            return lower_scene2_uncached(value, policy_, density, resolve, ir_);
        }
        match hit {
            Some(p) => {
                renderer_retained_plans::rebind(&value.0.retained_plans.borrow(), &p, ir_id, ir_);
                let draws = p.borrow().draws.clone();
                Ok(draws)
            }
            None => {
                let draws = lower_scene2_uncached(value, policy_, density, resolve, ir_)?;
                if draws.iter().any(|d| d.texture.as_ref().is_some_and(|tx| renderer_lowering::sampled_texture_bytes(tx) > SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY))
                    || has_gpu_shapes(commands)
                {
                    Ok(draws)
                } else {
                    let p = Rc::new(RefCell::new(Plan {
                        ir_id,
                        command_count: command_count as i64,
                        source_bytes: renderer_retained_plans::plan_source_bytes(commands),
                        density,
                        extent,
                        resources,
                        ir: ir_.clone(),
                        draws: draws.clone(),
                    }));
                    renderer_retained_plans::add(&mut value.0.retained_plans.borrow_mut(), fingerprint, p);
                    Ok(draws)
                }
            }
        }
    };
    let by_ir = renderer_retained_plans::find_ir(&value.0.retained_plans.borrow(), ir_id);
    match by_ir {
        Some(p) if same_context(&p.borrow()) && p.borrow().resources.is_empty() => Ok(p.borrow().draws.clone()),
        Some(p) if same_context(&p.borrow()) => {
            let (cacheable, resources) = renderer_retained_plans::resource_stamps(resolve, commands);
            if cacheable && renderer_retained_plans::same_resources(&p.borrow().resources, &resources) {
                Ok(p.borrow().draws.clone())
            } else {
                slow()
            }
        }
        _ => slow(),
    }
}

pub fn lower_scene2(value: &Renderer, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ir_: &Rc<RenderIr>) -> Result<Vec<Draw>, Error> {
    lower_scene2_with_policy(value, &lease_policy::CopyImageSnapshots, density, resource_, ir_)
}

pub fn replay_step(clear: Option<(f64, f64, f64, f64)>, identity: &str, version: i64, value: &Renderer) -> Result<Option<()>, Error> {
    ensure("Renderer.Native.replay", value)?;
    sync_runtime_facts("Renderer.Native.replay", value)?;
    let replayed = scene_execution::replay_prepared_sampled_resources(clear, identity, version, &mut value.0.renderer.borrow_mut())?;
    match replayed {
        None => Ok(None),
        Some((_presented, draw_count)) => {
            count_draws(&mut value.0.counters.borrow_mut(), draw_count);
            Ok(Some(()))
        }
    }
}

fn step_core(
    after_prepare: &mut dyn FnMut(),
    clear: Option<(f64, f64, f64, f64)>,
    identity: Option<&str>,
    version: Option<i64>,
    value: &Renderer,
    draws: &[Draw],
) -> Result<bool, Error> {
    ensure("Renderer.step", value)?;
    if let Err(e) = sync_runtime_facts("Renderer.step", value) {
        after_prepare();
        return Err(e);
    }
    let f = match presentation_facts_of(value) {
        Ok(f) => f,
        Err(e) => {
            after_prepare();
            return Err(e);
        }
    };
    let replayed = match (identity, version) {
        (Some(identity), Some(version)) => {
            scene_execution::replay_prepared_sampled_resources(clear, identity, version, &mut value.0.renderer.borrow_mut())
        }
        _ => Ok(None),
    };
    match replayed {
        Err(e) => {
            after_prepare();
            Err(e)
        }
        Ok(Some((presented, draw_count))) => {
            after_prepare();
            count_draws(&mut value.0.counters.borrow_mut(), draw_count);
            Ok(presented)
        }
        Ok(None) => {
            if !same_draw_list(draws, &value.0.last_step_draws.borrow()) {
                let mut prepared = value.0.last_step_prepared.borrow_mut();
                prepared.clear();
                prepared.extend(draws.iter().map(|x| {
                        let d = &x.draw;
                        let s = &d.state;
                        let viewport = match s.viewport {
                            (_, _, w, h) if w < 0 || h < 0 => (0, 0, f.logical_width, f.logical_height),
                            v => v,
                        };
                        let scissor = match s.scissor {
                            (_, _, w, h) if w < 0 || h < 0 => (0, 0, f.logical_width, f.logical_height),
                            v => v,
                        };
                        let draw_ = if viewport == s.viewport && scissor == s.scissor {
                            d.clone()
                        } else {
                            let mut copy = d.clone();
                            copy.state.viewport = viewport;
                            copy.state.scissor = scissor;
                            copy
                        };
                        Rc::new(SampledDraw { ui: x.ui.clone(),
                            family: x.family,
                            blend: x.blend,
                            texture: x.texture.clone(),
                            auxiliary: x.auxiliary.clone(),
                            vertex_attributes: x.vertex_attributes.clone(),
                            samples: x.samples,
                            draw: draw_,
                        })
                    }));
                let mut input = value.0.last_step_draws.borrow_mut();
                input.clear();
                input.extend_from_slice(draws);
            }
            let draws = value.0.last_step_prepared.borrow();
            let mut scaled_cache = value.0.scaled.borrow_mut();
            let scaled = scale_sampled_ref(&mut scaled_cache, &value.0.renderer_facts.get(), &draws);
            let result = match (identity, version) {
                (None, None) => scene_execution::submit(None, after_prepare, clear, &mut value.0.renderer.borrow_mut(), &scaled),
                (Some(identity), Some(version)) => {
                    scene_execution::submit(Some((identity.to_owned(), version)), after_prepare, clear, &mut value.0.renderer.borrow_mut(), &scaled)
                }
                _ => {
                    after_prepare();
                    Err(fail_error("Renderer.step", ErrorKind::InvalidArgument, "prepared identity and version must be supplied together"))
                }
            };
            result.inspect(|_presented| {
                count_draws(&mut value.0.counters.borrow_mut(), scaled.len() as i64);
            })
        }
    }
}

pub fn step(clear: Option<(f64, f64, f64, f64)>, value: &Renderer, draws: &[Draw]) -> Result<(), Error> {
    step_core(&mut || {}, clear, None, None, value, draws).map(|_| ())
}

pub fn draw(clear: Option<(f64, f64, f64, f64)>, value: &Renderer, draws: &[Draw]) -> Result<bool, Error> {
    step_core(&mut || {}, clear, None, None, value, draws)
}

fn lower_scene2_submission(submission_: &Submission, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ir_: &Rc<RenderIr>) -> Result<Batch, Error> {
    if let Err(e) = ensure_submission("Renderer.Native.lower_scene2", submission_) {
        close_submission(submission_);
        return Err(e);
    }
    // ponytail: resolver panics unwind; add a panic-to-error boundary only if resolver contracts require recovery.
    match lower_scene2_with_policy(
        &submission_.0.owner,
        &lease_policy::RetainImageSnapshots(submission_.0.image_leases.clone()),
        density,
        resource_,
        ir_,
    ) {
        Ok(draws) => Ok(Batch { owner: submission_.clone(), draws }),
        Err(e) => {
            close_submission(submission_);
            Err(e)
        }
    }
}

fn retained_scene2_segment_bytes(draws: &[Draw]) -> i64 {
    draws.iter().fold(0i64, |total, draw| total.saturating_add(scene_execution::retained_draw_bytes(draw)))
}

fn lower_scene2_segment(
    submission_: &Submission,
    identity: i64,
    version: i64,
    cacheable: bool,
    density: i64,
    resource_: &dyn Fn(i64) -> Option<retained_resource>,
    ir_: &Rc<RenderIr>,
) -> Result<Batch, Error> {
    let operation = "Renderer.Native.lower_scene2_segment";
    if identity <= 0 || version < 0 {
        close_submission(submission_);
        return Err(fail_error(operation, ErrorKind::InvalidArgument, "segment identity/version is invalid"));
    }
    if let Err(e) = ensure_submission(operation, submission_) {
        close_submission(submission_);
        return Err(e);
    }
    let owner = &submission_.0.owner;
    let facts = match presentation_facts_of(owner) {
        Ok(facts) => facts,
        Err(e) => {
            close_submission(submission_);
            return Err(e);
        }
    };
    let found = if cacheable {
        renderer_retained_plans::find_segment(&mut owner.0.retained_plans.borrow_mut(), identity).filter(|entry| {
            entry.version == version
                && entry.density == density
                && entry.width == facts.logical_width
                && entry.height == facts.logical_height
        })
    } else {
        None
    };
    if let Some(entry) = found {
        owner.0.retained_scene2_segment_hits.set(owner.0.retained_scene2_segment_hits.get() + 1);
        return Ok(Batch { owner: submission_.clone(), draws: entry.draws.clone() });
    }
    let lowered = lower_scene2_submission(submission_, density, resource_, ir_)?;
    if cacheable {
        owner.0.retained_scene2_segment_misses.set(owner.0.retained_scene2_segment_misses.get() + 1);
        let bytes_ = retained_scene2_segment_bytes(&lowered.draws);
        renderer_retained_plans::add_segment(
            &mut owner.0.retained_plans.borrow_mut(),
            identity,
            bytes_,
            Segment {
                version,
                density,
                width: facts.logical_width,
                height: facts.logical_height,
                draws: lowered.draws.clone(),
            },
        );
    }
    Ok(lowered)
}

/// A UI layer as one draw its owner paints in place, over the whole frame.
fn ui_draw(submission_: &Submission, ui: scene_execution::UiLayer) -> Result<Batch, Error> {
    if let Err(e) = ensure_submission("Renderer.Native.ui_draw", submission_) {
        close_submission(submission_);
        return Err(e);
    }
    let facts = match presentation_facts_of(&submission_.0.owner) {
        Ok(facts) => facts,
        Err(e) => {
            close_submission(submission_);
            return Err(e);
        }
    };
    let framebuffer = (0, 0, facts.logical_width, facts.logical_height);
    let draw: Draw = Rc::new(SampledDraw {
        ui: Some(ui),
        family: PipelineFamily::Ui,
        blend: Blend::Alpha,
        texture: None,
        auxiliary: None,
        vertex_attributes: None,
        samples: 1,
        draw: GeometryDraw {
            mesh: scene_execution::Mesh {
                key: scene_execution::MeshKey::Host("ui-layer".to_owned()),
                vertices: scene_execution::empty_bytes(),
                shared_vertices: None,
                vertex_count: 0,
                indices: scene_execution::empty_bytes(),
                index_count: 0,
                primitive: ogpu_core::render_pass::Primitive::TriangleList,
            },
            state: renderer_lowering::default_state(framebuffer, framebuffer),
        },
    });
    Ok(Batch { owner: submission_.clone(), draws: vec![draw] })
}

/// Adopts already prepared non-Scene2 draws into this submission.
pub fn adopt_draws(submission_: &Submission, draws: Vec<Draw>) -> Result<Batch, Error> {
    if let Err(e) = ensure_submission("Renderer.Native.adopt_draws", submission_) {
        close_submission(submission_);
        return Err(e);
    }
    Ok(Batch { owner: submission_.clone(), draws })
}

/// Consumes the submission on either success or failure.
fn step_submission(
    clear: Option<(f64, f64, f64, f64)>,
    identity: Option<&str>,
    version: Option<i64>,
    submission_: &Submission,
    batches: &[Batch],
) -> Result<(), Error> {
    if let Err(e) = ensure_submission("Renderer.Native.step", submission_) {
        close_submission(submission_);
        return Err(e);
    }
    let result = if batches.iter().any(|b| !Rc::ptr_eq(&b.owner.0, &submission_.0)) {
        Err(fail_error("Renderer.Native.step", ErrorKind::InvalidArgument, "draw batch belongs to another submission"))
    } else {
        let mut draws = submission_.0.owner.0.submission_draws.borrow_mut();
        draws.clear();
        draws.extend(batches.iter().flat_map(|b| b.draws.iter().cloned()));
        let result = step_core(&mut || close_submission(submission_), clear, identity, version, &submission_.0.owner, &draws).map(|_| ());
        draws.clear();
        result
    };
    close_submission(submission_);
    result
}

/// The completed frame as drawable-sized RGBA bytes.
pub fn capture(value: &Renderer) -> Result<Vec<u8>, Error> {
    ensure("Renderer.capture", value)?;
    let facts = presentation_facts_of(value)?;
    scene_execution::read_pixels(&mut value.0.renderer.borrow_mut(), facts.drawable_width * 4)
}

pub fn read_pixels(value: &Renderer, bytes_per_row: i64) -> Result<Vec<u8>, Error> {
    ensure("Renderer.read_pixels", value)?;
    scene_execution::read_pixels(&mut value.0.renderer.borrow_mut(), bytes_per_row)
}

/// An offscreen execution's completed frame texture, on the device it leased (the presenting
/// window's when one exists). A window execution samples a Canvas published from it directly;
/// readback happens only for CPU access.
pub fn offscreen_target(value: &Renderer) -> Result<Texture, Error> {
    let operation = "Renderer.offscreen_target";
    ensure(operation, value)?;
    if runtime::is_window(&value.0.runtime.borrow()) {
        Err(fail_error(operation, ErrorKind::Unsupported, "operation requires an offscreen execution"))
    } else {
        Ok(scene_execution::target(&value.0.renderer.borrow()))
    }
}

fn release_view(view: &Rc<RetainedView>) {
    let target = view.target.borrow_mut().take();
    if let Some(target) = target {
        view.retain_after.set(if view.seen.get() < RETAIN_LIVE { RETAIN_LIVE } else { RETAIN_SOON });
        let _ = canvas::destroy(&target.canvas);
        let _ = destroy(&target.exec);
    }
}

fn release_views(value: &Renderer) {
    let views = std::mem::take(&mut *value.0.retained_views.borrow_mut());
    for view in views {
        release_view(&view);
    }
}

/// A window execution fails with `InvalidArgument`, destroying nothing, while GPU leases or
/// offscreen executions still share its device.
pub fn destroy(value: &Renderer) -> Result<(), Error> {
    if value.0.dead.get() {
        return Ok(());
    }
    release_views(value);
    if runtime::is_window(&value.0.runtime.borrow()) && value.0.gpu_owner.shared_leases.get() > 0 {
        return Err(fail_error("Renderer.destroy", ErrorKind::InvalidArgument, &format!("{} GPU lease(s) still share this window's device; release them first", value.0.gpu_owner.shared_leases.get())));
    }
    let open: Vec<Submission> = value.0.submissions.borrow().clone();
    for submission_ in &open {
        close_submission(submission_);
    }
    let assets = assets::destroy(&mut value.0.assets.borrow_mut());
    side_lru::clear(&mut value.0.snapshots.borrow_mut());
    renderer_retained_plans::clear(&mut value.0.retained_plans.borrow_mut());
    value.0.submission_draws.borrow_mut().clear();
    value.0.last_step_draws.borrow_mut().clear();
    value.0.last_step_prepared.borrow_mut().clear();
    renderer_lowering::clear(&mut value.0.lowering.borrow_mut());
    value.0.dead.set(true);
    ACTIVE_WINDOW.with(|cell| {
        let current = cell.borrow().clone();
        if let Some(current) = current
            && Rc::ptr_eq(&current.0, &value.0) {
                *cell.borrow_mut() = None;
            }
    });
    let core = scene_execution::destroy(&mut value.0.renderer.borrow_mut());
    if value.0.owns_device_lease {
        release_device(value.0.shared_device_lease, &value.0.gpu_owner);
    }
    match (assets, core) {
        (Err(e), _) => Err(resource_error_value("Renderer.destroy", &e)),
        (Ok(()), Err(e)) => Err(e),
        (Ok(()), Ok(())) => Ok(()),
    }
}

/// The first sight of a layer draws it directly; the second renders it offscreen. `None` when the
/// target cannot be made: the view then stays direct for as long as the layer does.
fn make_retained_target(f: &PresentationFacts, draws: &[Draw]) -> Option<RetainedTarget> {
    let dw = f.drawable_width;
    let dh = f.drawable_height;
    let exec = create_offscreen(&Configuration {
        logical_width: f.logical_width,
        logical_height: f.logical_height,
        drawable_width: dw,
        drawable_height: dh,
        title: "Rays retained view".to_owned(),
        vsync: false,
        high_density: true,
    })
    .ok()?;
    // Prepare canvas, then target, then step, preserving resource validation priority.
    let canvas = canvas::create(dw, dh);
    let target = offscreen_target(&exec);
    let stepped = step(Some((0.0, 0.0, 0.0, 0.0)), &exec, draws);
    let fail_with = |canvas: Option<&canvas::Canvas>| -> Option<RetainedTarget> {
        if let Some(c) = canvas {
            let _ = canvas::destroy(c);
        }
        let _ = destroy(&exec);
        None
    };
    match (stepped, target, canvas) {
        (Ok(()), Ok(texture), Ok(canvas)) => {
            let published = canvas::publish_gpu(&canvas, &texture);
            let command = ir::create(vec![Command::Image(ir::Image {
                resource_id: 1,
                source: ir::Rect { x: 0.0, y: 0.0, width: dw as f64, height: dh as f64 },
                destination: ir::Rect { x: 0.0, y: 0.0, width: f.logical_width as f64, height: f.logical_height as f64 },
            })]);
            match (published, command) {
                (Ok(()), Ok(ir_)) => Some(RetainedTarget { exec: exec.clone(), canvas, ir: Rc::new(ir_) }),
                _ => fail_with(Some(&canvas)),
            }
        }
        (_, _, Ok(canvas)) => fail_with(Some(&canvas)),
        _ => fail_with(None),
    }
}

/// Adopts a 3D layer's draws, retaining the layer's render once it keeps the same `layer` identity
/// on consecutive frames. A window execution only; offscreen ones adopt the draws directly.
pub fn adopt_retained_view(submission_: &Submission, density: i64, layer: &Rc<PreparedScene3>, draws: Vec<Draw>) -> Result<Batch, Error> {
    let value = &submission_.0.owner;
    let direct = |draws: Vec<Draw>| adopt_draws(submission_, draws);
    if !runtime::is_window(&value.0.runtime.borrow()) {
        return direct(draws);
    }
    let f = match presentation_facts_of(value) {
        Ok(f) => f,
        Err(_) => return direct(draws),
    };
    let drawable = (f.drawable_width, f.drawable_height);
    let position = value.0.retained_views.borrow().iter().position(|view| Rc::ptr_eq(&view.layer, layer));
    let current: Option<Rc<RetainedView>> = match position {
        Some(i) => {
            let view = value.0.retained_views.borrow()[i].clone();
            if view.drawable == drawable {
                Some(view)
            } else {
                value.0.retained_views.borrow_mut().remove(i);
                release_view(&view);
                None
            }
        }
        None => None,
    };
    match current {
        None => {
            let view = Rc::new(RetainedView {
                layer: layer.clone(),
                drawable,
                target: RefCell::new(None),
                failed: Cell::new(false),
                seen: Cell::new(1),
                retain_after: value.0.retain_after.clone(),
            });
            let evicted = {
                let mut views = value.0.retained_views.borrow_mut();
                views.insert(0, view);
                if views.len() > RETAINED_VIEW_CAPACITY { views.pop() } else { None }
            };
            if let Some(evicted) = evicted {
                release_view(&evicted);
            }
            direct(draws)
        }
        Some(view) => {
            view.seen.set(view.seen.get() + 1);
            if view.target.borrow().is_none() && !view.failed.get() && view.seen.get() >= value.0.retain_after.get() {
                match make_retained_target(&f, &draws) {
                    Some(target) => {
                        *view.target.borrow_mut() = Some(target);
                        // from here [view_seen] counts the frames the target serves
                        view.seen.set(0);
                    }
                    None => view.failed.set(true),
                }
            }
            let target = view.target.borrow().as_ref().map(|target| (target.canvas.clone(), target.ir.clone()));
            match target {
                None => direct(draws),
                Some((canvas, ir_)) => lower_scene2_submission(
                    submission_,
                    density,
                    &move |id| if id == 1 { Some(retained_resource::Canvas(canvas.clone())) } else { None },
                    &ir_,
                ),
            }
        }
    }
}

// GPU film leases own their own queue, so their frame pacing never couples with presentation.
pub fn acquire_gpu() -> Result<Gpu, Error> {
    let operation = "Renderer.acquire_gpu";
    let (device, gpu_shared, gpu_owner_) = acquire_device(operation)?;
    match backend::create_queue(&device) {
        Err(e) => {
            release_device(gpu_shared, &gpu_owner_);
            Err(e)
        }
        Ok(queue) => Ok(Gpu(Rc::new(GpuData {
            device,
            queue,
            owner: gpu_owner_,
            shared: gpu_shared,
            released: Cell::new(false),
        }))),
    }
}

pub fn gpu_device(value: &Gpu) -> Device {
    value.0.device.clone()
}

pub fn gpu_queue(value: &Gpu) -> Queue {
    value.0.queue.clone()
}

/// True when the device is the presenting window's.
pub fn gpu_shared(value: &Gpu) -> bool {
    value.0.shared
}

pub fn release_gpu(value: &Gpu) {
    if !value.0.released.get() {
        let owned: Vec<GpuMarks> = value
            .0
            .owner
            .marks_registry
            .borrow()
            .values()
            .filter(|marks_| Rc::ptr_eq(&marks_.0.gpu.0, &value.0))
            .cloned()
            .collect();
        for marks_ in &owned {
            close_gpu_marks(marks_);
        }
        value.0.released.set(true);
        let _ = backend::destroy_queue(&value.0.queue);
        release_device(value.0.shared, &value.0.owner);
    }
}

pub mod native {
    use super::*;

    pub use super::{Batch, new_scaled_cache, scale_draws, scale_sampled_cached, scale_sampled_resources, ScaledCache, Submission};
    pub use super::{adopt_draws, adopt_retained_view, begin_submission};

    /// The borrowed SDL/runtime shell; its lifetime remains owned by `t`.
    pub fn runtime_of(value: &Renderer, f: &mut dyn FnMut(&runtime::Runtime)) {
        f(&value.0.runtime.borrow())
    }

    /// The same shell, mutably (an offscreen shell refuses an IME area and a cursor).
    pub fn runtime_of_mut(value: &Renderer, f: &mut dyn FnMut(&mut runtime::Runtime)) {
        f(&mut value.0.runtime.borrow_mut())
    }

    pub fn gpu_vertices_owner(value: &Renderer) -> scene_execution::GpuVerticesOwner {
        value.0.gpu_owner.scene_vertices.clone()
    }

    pub fn create_offscreen_with_gpu(gpu_: &Gpu, configuration_: &Configuration) -> Result<Renderer, Error> {
        let operation = "Renderer.Native.create_offscreen_with_gpu";
        if gpu_.0.released.get() {
            return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU lease is released"));
        }
        valid_configuration(operation, configuration_)?;
        let mut runtime_ = runtime::create_offscreen(
            configuration_.logical_width,
            configuration_.logical_height,
            configuration_.drawable_width,
            configuration_.drawable_height,
        )?;
        let surface = match runtime::surface_configuration(&mut runtime_) {
            Ok(surface) => surface,
            Err(e) => {
                let _ = runtime::destroy(&mut runtime_);
                return Err(e);
            }
        };
        let renderer = (|| -> Result<scene_execution::SceneExecution, Error> {
            let driver = ogpu_core::impl_::create_driver()?;
            renderer_pipeline::create_renderer(
                &backend_name()?,
                Some(gpu_.0.device.clone()),
                Some(gpu_.0.owner.scene_vertices.clone()),
                true,
                driver,
                &surface,
            )
        })();
        match renderer {
            Err(e) => {
                let _ = runtime::destroy(&mut runtime_);
                Err(e)
            }
            Ok(renderer) => Ok(finish_create(false, false, gpu_.0.owner.clone(), Rc::new(RefCell::new(runtime_)), renderer)),
        }
    }

    /// A `Marks`-family source for one mark set (specification/marks.md).
    pub fn create_gpu_marks(gpu_: &Gpu) -> Result<GpuMarks, Error> {
        let operation = "Renderer.create_gpu_marks";
        if gpu_.0.released.get() {
            return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU lease is released"));
        }
        if gpu_.0.owner.marks_registry.borrow().len() >= GPU_MARKS_CAPACITY {
            return Err(fail_error(operation, ErrorKind::Capacity, "GPU marks capacity reached"));
        }
        let Some(identity) = fresh_gpu_marks_id() else {
            return Err(fail_error(operation, ErrorKind::Capacity, "GPU marks identity capacity reached"));
        };
        let value = GpuMarks(Rc::new(GpuMarksData {
            id: identity,
            gpu: gpu_.clone(),
            domain: std::thread::current().id(),
            closed: Cell::new(false),
            stamp: Cell::new(0),
            count: Cell::new(0),
            owned: RefCell::new(vec![None; 6]),
            last: RefCell::new((0..6).map(|_| scene_execution::empty_bytes()).collect()),
            device: RefCell::new(Vec::new()),
            registration: RefCell::new(None),
        }));
        gpu_.0.owner.marks_registry.borrow_mut().insert(identity, value.clone());
        Ok(value)
    }

    /// A token for a `Shapes` command drawing `count` instances through the five attribute buffers
    /// (position, extent, fill, stroke, width; `kind` is the `Ui_batch` kind word). Each is
    /// `Host (bytes, stride)`, borrowed and unchanged, or `Device (source, stride)`, another
    /// producer's buffer. The token stays valid while every device source returns the same buffer;
    /// any change gives a new stamp. With `indirect (source, offset)` the set is drawn by an indexed
    /// indirect draw from five uints at `offset`.
    pub fn gpu_marks_token(
        indirect: Option<(Rc<dyn Fn() -> Option<Buffer>>, i64)>,
        value: &GpuMarks,
        kind: i64,
        count: i64,
        attributes: &[MarksSource],
    ) -> Result<marks::GpuToken, Error> {
        let operation = "Renderer.gpu_marks";
        let v = &value.0;
        if v.closed.get() || v.gpu.0.released.get() {
            return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU marks or lease is closed"));
        }
        if v.domain != std::thread::current().id() {
            return Err(fail_error(operation, ErrorKind::InvalidArgument, "GPU marks used on another domain"));
        }
        if count <= 0 || count > 1_000_000 || attributes.len() != 5 {
            return Err(fail_error(operation, ErrorKind::InvalidArgument, "a mark set has one to 1,000,000 instances and five attributes"));
        }
        let mut changed = v.registration.borrow().is_none() || v.count.get() != count;
        let mut device: Vec<(Rc<dyn Fn() -> Option<Buffer>>, Buffer)> = Vec::new();
        let upload = |index: usize, data: &Bytes, changed: &mut bool| -> Result<Buffer, Error> {
            let owned = v.owned.borrow()[index].clone();
            let length = data.len() as i64;
            let size = 16.max(length);
            let buffer = match owned {
                Some(b) if backend::buffer_size(&b) >= size => b,
                _ => {
                    let capacity = 16.max(2 * length);
                    let b = backend::create_buffer(
                        &v.gpu.0.device,
                        BufferDescriptor {
                            label: Some("Marks attribute".to_owned()),
                            size: capacity,
                            usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst],
                        },
                        None,
                    )?;
                    if let Some(old) = &owned {
                        let _ = backend::destroy_buffer(old);
                    }
                    v.owned.borrow_mut()[index] = Some(b.clone());
                    v.last.borrow_mut()[index] = scene_execution::empty_bytes();
                    *changed = true;
                    b
                }
            };
            let last = v.last.borrow()[index].clone();
            if Rc::ptr_eq(&last, data) || last == *data {
                Ok(buffer)
            } else {
                backend::write_buffer(&buffer, 0, &data)?;
                v.last.borrow_mut()[index] = data.clone();
                *changed = true;
                Ok(buffer)
            }
        };
        let mut strides = [0i64; 5];
        // the device's index count, read by the draw, joins the inputs whose identity the token keeps
        let indirect = match indirect {
            None => None,
            Some((source, offset)) => match source() {
                Some(b) => {
                    device.push((source.clone(), b.clone()));
                    Some((b, offset))
                }
                None => return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU display count is stale or closed")),
            },
        };
        let mut buffers: Vec<Buffer> = Vec::new();
        for index in 0..5 {
            let buffer = match &attributes[index] {
                MarksSource::Host(data, stride) => {
                    strides[index] = *stride;
                    upload(index, data, &mut changed)?
                }
                MarksSource::Device(source, stride) => {
                    strides[index] = *stride;
                    match source() {
                        Some(b) => {
                            device.push((source.clone(), b.clone()));
                            b
                        }
                        None => return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU display output is stale or closed")),
                    }
                }
            };
            buffers.push(buffer);
        }
        // A device colour is a region's float r, g, b (mode 1); a host one is packed words.
        let mode = |index: usize| match attributes[index] {
            MarksSource::Device(..) => 1i64,
            MarksSource::Host(..) => 0,
        };
        let mut params = vec![0u8; 32];
        for (i, word) in [kind, strides[0], strides[1], strides[2], strides[3], strides[4], mode(2), mode(3)].into_iter().enumerate() {
            params[i * 4..i * 4 + 4].copy_from_slice(&(word as i32).to_le_bytes());
        }
        let params_buffer = upload(5, &new_bytes(params), &mut changed)?;
        let token = || marks::gpu_token(v.id, count, v.stamp.get()).map_err(|m| oerror::make(operation, ErrorKind::InvalidArgument, &m));
        let same_device = device.len() == v.device.borrow().len()
            && device.iter().zip(v.device.borrow().iter()).all(|((_, a), (_, b))| same_buffer(a, b));
        if !changed && valid_gpu_marks(value) && same_device {
            return token();
        }
        unregister_marks(value);
        v.stamp.set(v.stamp.get() + 1);
        v.count.set(count);
        *v.device.borrow_mut() = device;
        let mut extra: Vec<Buffer> = buffers[1..].to_vec();
        extra.push(params_buffer);
        let owner_value = value.clone();
        match scene_execution::register_gpu_vertices(
            &v.gpu.0.owner.scene_vertices,
            Some(extra),
            None,
            indirect,
            None,
            None,
            &gpu_marks_key(value),
            &v.gpu.0.device,
            &buffers[0],
            4 * count,
            Box::new(move || valid_gpu_marks(&owner_value)),
        ) {
            Err(e) => {
                v.device.borrow_mut().clear();
                Err(e)
            }
            Ok(registration) => {
                *v.registration.borrow_mut() = Some(registration);
                token()
            }
        }
    }

    pub use super::close_gpu_marks;

    /// Draws the renderer encodes for the batch after neighbours merge (-1 on error).
    pub fn encoded_draws_for_test(batch_: &Batch) -> i64 {
        scene_execution::private::coalesced_draws_for_test(&batch_.draws)
    }

    pub fn snapshot_count_for_test(value: &Renderer) -> i64 {
        side_lru::length(&mut value.0.snapshots.borrow_mut())
    }

    pub fn lower_scene2(submission_: &Submission, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ir_: &Rc<RenderIr>) -> Result<Batch, Error> {
        lower_scene2_submission(submission_, density, resource_, ir_)
    }

    pub fn lower_scene2_segment(
        submission_: &Submission,
        identity: i64,
        version: i64,
        cacheable: bool,
        density: i64,
        resource_: &dyn Fn(i64) -> Option<retained_resource>,
        ir_: &Rc<RenderIr>,
    ) -> Result<Batch, Error> {
        super::lower_scene2_segment(submission_, identity, version, cacheable, density, resource_, ir_)
    }

    pub fn ui_draw(submission_: &Submission, ui: scene_execution::UiLayer) -> Result<Batch, Error> {
        super::ui_draw(submission_, ui)
    }

    pub fn step(
        clear: Option<(f64, f64, f64, f64)>,
        identity: Option<&str>,
        version: Option<i64>,
        submission_: &Submission,
        batches: &[Batch],
    ) -> Result<(), Error> {
        step_submission(clear, identity, version, submission_, batches)
    }

    pub fn replay(clear: Option<(f64, f64, f64, f64)>, identity: &str, version: i64, value: &Renderer) -> Result<Option<()>, Error> {
        replay_step(clear, identity, version, value)
    }

    /// Idempotently releases an unsubmitted transaction.
    pub fn cancel(submission_: &Submission) {
        close_submission(submission_)
    }

    /// What this renderer holds to stage texture uploads through.
    pub fn staging_bytes(value: &Renderer) -> i64 {
        scene_execution::private::staging_bytes(&value.0.renderer.borrow())
    }

    /// Whether the device has memoryless attachments, and how many this renderer's pool holds.
    pub fn memoryless_attachments(value: &Renderer) -> (bool, i64) {
        scene_execution::private::memoryless_attachments(&value.0.renderer.borrow())
    }

    pub fn retained_scene2_segment_stats(value: &Renderer) -> (i64, i64, i64) {
        (
            renderer_retained_plans::segment_count(&mut value.0.retained_plans.borrow_mut()),
            value.0.retained_scene2_segment_hits.get(),
            value.0.retained_scene2_segment_misses.get(),
        )
    }
}

pub fn create_gpu_marks(gpu_: &Gpu) -> Result<GpuMarks, Error> {
    native::create_gpu_marks(gpu_)
}

pub fn gpu_marks_token(
    indirect: Option<(Rc<dyn Fn() -> Option<Buffer>>, i64)>,
    value: &GpuMarks,
    kind: i64,
    count: i64,
    attributes: &[MarksSource],
) -> Result<marks::GpuToken, Error> {
    native::gpu_marks_token(indirect, value, kind, count, attributes)
}

static NEXT_GPU_POSITIONS: AtomicI64 = AtomicI64::new(0);

/// The packed float3 positions, normals (if registered) and count that the valid registration under
/// `key` borrows, for another consumer of the same device (the path tracer); the producer keeps them
/// while the device mesh naming `key` lives. Initial domain.
pub fn gpu_positions(gpu_: &Gpu, key: &str) -> Option<(Buffer, Option<Buffer>, i64)> {
    if gpu_.0.released.get() {
        return None;
    }
    match scene_execution::private::find_gpu_vertices(&gpu_.0.owner.scene_vertices, key) {
        Some((positions, extra, count)) if extra.len() <= 1 => Some((positions, extra.into_iter().next(), count)),
        _ => None,
    }
}

/// The indices (count) and colours (width) borrowed under `key`.
pub fn gpu_streams(gpu_: &Gpu, key: &str) -> Option<(Option<(Buffer, i64)>, Option<(Buffer, i64)>)> {
    if gpu_.0.released.get() {
        return None;
    }
    scene_execution::private::find_gpu_streams(&gpu_.0.owner.scene_vertices, key)
}

/// The producer's stamp of the indices borrowed under `key` (0: none, or the registration is gone).
pub fn gpu_topology(gpu_: &Gpu, key: &str) -> u64 {
    if gpu_.0.released.get() {
        return 0;
    }
    scene_execution::private::find_gpu_topology(&gpu_.0.owner.scene_vertices, key)
}

/// Borrows another producer's `count` packed float3 positions (and normals) for a Scene3 mesh drawn
/// from the device: the key a device mesh names and its unregistration (initial domain). The
/// producer keeps the buffers while `valid`; a draw of a registration no longer valid is refused as
/// stale. Optional `indices` borrows uint32 triangle indices with their settled count; `colors`
/// borrows packed float3/float4 colours with their component width. At most 128 borrowed sources are
/// live. `topology` is the producer's stamp of the index content (0 for none): while it stays equal and
/// nonzero the indices promise the same triangles, whatever buffer holds them.
pub fn register_gpu_positions(
    gpu_: &Gpu,
    positions: &Buffer,
    normals: Option<Buffer>,
    indices: Option<(Buffer, i64)>,
    colors: Option<(Buffer, i64)>,
    count: i64,
    topology: u64,
    valid: Rc<dyn Fn() -> bool>,
) -> Result<(String, Box<dyn Fn()>), Error> {
    let operation = "Renderer.register_gpu_positions";
    if gpu_.0.released.get() {
        return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU lease is released"));
    }
    let key = format!("positions:{}", NEXT_GPU_POSITIONS.fetch_add(1, Ordering::SeqCst));
    let lease = gpu_.clone();
    let registration = scene_execution::register_gpu_vertices(
        &gpu_.0.owner.scene_vertices,
        Some(normals.into_iter().collect()),
        Some(12),
        None,
        indices,
        colors,
        &key,
        &gpu_.0.device,
        positions,
        count,
        Box::new(move || !lease.0.released.get() && valid()),
    )?;
    registration.topology.set(topology);
    Ok((key, Box::new(move || scene_execution::private::unregister_gpu_vertices(&registration))))
}
