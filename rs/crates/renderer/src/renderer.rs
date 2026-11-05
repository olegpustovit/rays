//! Isolated native frame coordinator. The public values deliberately contain no SDL or native handles.
use crate::renderer_leases::{self, policy as lease_policy};
use crate::renderer_lowering::{self, ui_sampler, ui_white_texture};
use crate::renderer_pipeline;
use crate::renderer_retained_plans::{self, plan, resource as retained_resource, segment};
use crate::scene_execution::{self, bytes, geometry_draw, new_bytes, prepared_scene3, sampled_draw, sampled_texture, texture_level};
use crate::side_lru::{self, Item, TableKey};
use ogpu::backend::{self, Buffer, Device, Queue, Texture};
use ogpu::error::{self as oerror, Error, ErrorKind};
use ogpu::types::{AddressMode, BufferDescriptor, BufferUsage, MipFilter, SamplerDescriptor, SamplerFilter};
use runtime::{frame_facts, presentation_facts};
use runtime_resources::{Assets, Canvas, Image, Text};
use scene_command::render_ir::{self as ir, Command, RenderIr};
use scene_command::ui_batch::Table as Shapes;
use scene_command::{marks, ui_batch};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};

pub use crate::renderer_retained_plans::resource;
pub use crate::scene_execution as Scene_execution;
pub use crate::scene_execution::pipeline_family as family;
pub use crate::scene_execution::draw;


pub type error = Error;

pub fn pp_error(formatter: &mut dyn std::fmt::Write, value: &Error) {
    let _ = formatter.write_str(&oerror::to_string(value));
}

fn fail_error(operation: &str, kind: ErrorKind, message: &str) -> Error {
    oerror::make(operation, kind, message)
}

fn resource_error_value(operation: &str, value: &Error) -> Error {
    fail_error(operation, ErrorKind::InvalidState, &oerror::to_string(value))
}

#[derive(Clone, Debug, PartialEq)]
pub struct configuration {
    pub logical_width: i64,
    pub logical_height: i64,
    pub drawable_width: i64,
    pub drawable_height: i64,
    pub title: String,
    pub vsync: bool,
    /// a drawable at the display's native density (false: the logical size)
    pub high_density: bool,
}

pub fn default_configuration() -> configuration {
    configuration {
        logical_width: 640,
        logical_height: 480,
        drawable_width: 640,
        drawable_height: 480,
        title: "Rays".to_owned(),
        vsync: true,
        high_density: true,
    }
}

pub type blend = ogpu::pipeline::Blend;
/// A draw handed around by identity.
pub type draw_ = draw;

pub struct gpu_data {
    gpu_device: Device,
    gpu_queue: Queue,
    gpu_owner: gpu_owner,
    gpu_shared: bool,
    gpu_released: Cell<bool>,
}

/// A borrowed GPU: the active window's OGPU device (so Scene samples GPU film textures directly) or,
/// without a window, a lazily created headless device shared by all leases. Every lease owns its own
/// queue. Release it while SDL is alive; the headless device goes away with the last lease.
#[derive(Clone)]
pub struct gpu(Rc<gpu_data>);

pub struct gpu_owner_data {
    scene_vertices: scene_execution::gpu_vertices_owner,
    marks_registry: RefCell<HashMap<i64, gpu_marks>>,
    shared_leases: Cell<i64>,
}

pub type gpu_owner = Rc<gpu_owner_data>;

/// A `Marks`-family source: the attribute buffers of one mark set. A host attribute is uploaded into
/// a buffer the source owns; a device attribute is another producer's output, bound directly and
/// valid only while that producer's output is current.
#[derive(Clone)]
pub enum marks_source {
    Host(bytes, i64),
    Device(Rc<dyn Fn() -> Option<Buffer>>, i64),
}

pub struct gpu_marks_data {
    marks_id: i64,
    marks_gpu: gpu,
    marks_domain: std::thread::ThreadId,
    marks_closed: Cell<bool>,
    marks_stamp: Cell<i64>,
    marks_count: Cell<i64>,
    marks_owned: RefCell<Vec<Option<Buffer>>>,
    marks_last: RefCell<Vec<bytes>>,
    marks_device: RefCell<Vec<(Rc<dyn Fn() -> Option<Buffer>>, Buffer)>>,
    marks_registration: RefCell<Option<scene_execution::gpu_vertices>>,
}

#[derive(Clone)]
pub struct gpu_marks(Rc<gpu_marks_data>);

const gpu_marks_capacity: usize = 64;
static next_gpu_marks: AtomicI64 = AtomicI64::new(1);

fn fresh_gpu_marks_id() -> Option<i64> {
    let current = next_gpu_marks.fetch_add(1, Ordering::SeqCst);
    if current >= (1i64 << 62) - 1 { None } else { Some(current) }
}

fn create_gpu_owner() -> gpu_owner {
    Rc::new(gpu_owner_data {
        scene_vertices: scene_execution::Private::create_gpu_vertices_owner(),
        marks_registry: RefCell::new(HashMap::new()),
        shared_leases: Cell::new(0),
    })
}

fn same_buffer(a: &Buffer, b: &Buffer) -> bool {
    backend::buffer_id(a) == backend::buffer_id(b)
}

fn valid_gpu_marks(value: &gpu_marks) -> bool {
    let v = &value.0;
    !v.marks_closed.get()
        && !v.marks_gpu.0.gpu_released.get()
        && v.marks_domain == std::thread::current().id()
        && v.marks_device.borrow().iter().all(|(source, input)| match source() {
            Some(current) => same_buffer(&current, input),
            None => false,
        })
}

fn gpu_marks_key(value: &gpu_marks) -> String {
    format!("marks:{}:{}", value.0.marks_id, value.0.marks_stamp.get())
}

fn unregister_marks(value: &gpu_marks) {
    let registration = value.0.marks_registration.borrow_mut().take();
    if let Some(registration) = registration {
        scene_execution::Private::unregister_gpu_vertices(&registration);
    }
}

pub fn close_gpu_marks(value: &gpu_marks) {
    let v = &value.0;
    if v.marks_domain != std::thread::current().id() {
        unreachable!("GPU marks closed on another domain");
    }
    if !v.marks_closed.get() {
        v.marks_closed.set(true);
        unregister_marks(value);
        v.marks_device.borrow_mut().clear();
        v.marks_gpu.0.gpu_owner.marks_registry.borrow_mut().remove(&v.marks_id);
        for owned in v.marks_owned.borrow_mut().iter_mut() {
            if let Some(b) = owned.take() {
                let _ = backend::destroy_buffer(&b);
            }
        }
    }
}

pub type presentation_facts_ = presentation_facts;

#[derive(Clone, Copy, PartialEq, Eq)]
enum submission_state {
    Open,
    Closed,
}

pub use crate::renderer_leases::policy;

struct snapshot_cache_entry {
    snapshot_generation: i64,
    snapshot_width: i64,
    snapshot_height: i64,
    snapshot_texture: Rc<sampled_texture>,
}

const snapshot_cache_capacity: i64 = 256;
const snapshot_cache_byte_capacity: i64 = 64 * 1024 * 1024;

// Full-window snapshots change frequently and already have bounded double buffers in [Image].
// Keeping entries below 1 MiB admits UI/text assets but deliberately leaves a 640x480 RGBA frame
// (1,228,800 bytes) transient.
const snapshot_cache_entry_byte_capacity: i64 = renderer_lowering::snapshot_cache_entry_byte_capacity;

struct stats_counters {
    logical_draws: i64,
    logical_passes: i64,
}

fn count_draws(counters: &mut stats_counters, count: i64) {
    counters.logical_draws += count;
    counters.logical_passes += 1;
}

pub struct scaled_cache {
    scaled_input: Vec<draw>,
    scaled_facts: Option<frame_facts>,
    scaled_output: Vec<draw>,
}

pub fn new_scaled_cache() -> scaled_cache {
    scaled_cache { scaled_input: Vec::new(), scaled_facts: None, scaled_output: Vec::new() }
}

fn scale_required(facts: &frame_facts) -> bool {
    facts.logical_width != facts.drawable_width || facts.logical_height != facts.drawable_height
}

/// `None` when the draw is unchanged.
fn scale_draw(facts: &frame_facts, draw_: &geometry_draw) -> Option<geometry_draw> {
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

pub fn scale_draws(facts: &frame_facts, draws: &[geometry_draw]) -> Vec<geometry_draw> {
    if !scale_required(facts) {
        draws.to_vec()
    } else {
        draws.iter().map(|d| scale_draw(facts, d).unwrap_or_else(|| d.clone())).collect()
    }
}

pub fn scale_sampled_resources(facts: &frame_facts, draws: &[draw]) -> Vec<draw> {
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

fn same_draw_list(left: &[draw], right: &[draw]) -> bool {
    left.len() == right.len() && left.iter().zip(right.iter()).all(|(a, b)| Rc::ptr_eq(a, b))
}

pub fn scale_sampled_cached(cache: &mut scaled_cache, facts: &frame_facts, draws: &[draw]) -> Vec<draw> {
    if !scale_required(facts) {
        draws.to_vec()
    } else if same_draw_list(draws, &cache.scaled_input) && cache.scaled_facts.as_ref() == Some(facts) {
        cache.scaled_output.clone()
    } else {
        let scaled = scale_sampled_resources(facts, draws);
        cache.scaled_input = draws.to_vec();
        cache.scaled_facts = Some(*facts);
        cache.scaled_output = scaled.clone();
        scaled
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct stats {
    pub logical_draws: i64,
    pub logical_passes: i64,
    pub pipeline_cache_entries: i64,
    pub uploaded_bytes: i64,
    pub gpu_duration_seconds: f64,
    pub retained_plan_hits: i64,
    pub retained_plan_misses: i64,
    pub sun_shadow_passes: i64,
}

pub const zero_stats: stats = stats {
    logical_draws: 0,
    logical_passes: 0,
    pipeline_cache_entries: 0,
    uploaded_bytes: 0,
    gpu_duration_seconds: 0.0,
    retained_plan_hits: 0,
    retained_plan_misses: 0,
    sun_shadow_passes: 0,
};

pub struct t_data {
    runtime: RefCell<runtime::t>,
    shared_device_lease: bool,
    owns_device_lease: bool,
    gpu_owner: gpu_owner,
    renderer: RefCell<scene_execution::t>,
    renderer_facts: Cell<frame_facts>,
    counters: RefCell<stats_counters>,
    scaled: RefCell<scaled_cache>,
    assets: RefCell<Assets::t>,
    dead: Cell<bool>,
    snapshots: RefCell<side_lru::Table>,
    lowering: RefCell<renderer_lowering::t>,
    retained_plans: RefCell<renderer_retained_plans::t>,
    retained_scene2_segment_hits: Cell<i64>,
    retained_scene2_segment_misses: Cell<i64>,
    submissions: RefCell<Vec<submission>>,
    last_step_draws: RefCell<Vec<draw>>,
    last_step_prepared: RefCell<Vec<draw>>,
    retained_views: RefCell<Vec<Rc<retained_view>>>,
    /// the sights of a layer before it gets a target: `retain_soon`, or `retain_live` after a target
    /// was dropped before it paid for itself (a layer that changes every few frames)
    retain_after: Rc<Cell<i64>>,
}

/// The renderer: a shared handle (its parts are interior-mutable, as the OCaml record's fields were).
#[derive(Clone)]
pub struct t(Rc<t_data>);

/// A 3D layer that kept the same prepared identity on consecutive frames: its draws render once into
/// an offscreen target the size of the window and later frames composite that texture, so a frame
/// that only changes the UI never shades the view again. The identity is the key (prepared layers are
/// immutable), the window's drawable size is checked on every use, and the list is capped, so
/// nothing here can outlive or disagree with the scene it stands for.
pub struct retained_view {
    view_layer: Rc<prepared_scene3>,
    view_drawable: (i64, i64),
    view_target: RefCell<Option<retained_target>>,
    view_failed: Cell<bool>,
    /// sights before its target, then the frames the target served
    view_seen: Cell<i64>,
    /// its owner's `retain_after`
    view_retain_after: Rc<Cell<i64>>,
}

pub struct retained_target {
    target_exec: t,
    target_canvas: Canvas::t,
    target_ir: Rc<RenderIr>,
}

pub struct submission_data {
    owner: t,
    submission_state: Cell<submission_state>,
    image_leases: renderer_leases::t,
    ui_layers: Cell<i64>,
}

#[derive(Clone)]
pub struct submission(Rc<submission_data>);

#[derive(Clone)]
pub struct batch {
    batch_owner: submission,
    batch_draws: Vec<draw>,
}

const retained_view_capacity: usize = 4;

// A target costs an offscreen execution (its pipelines, its uploads) and pays back by later frames
// compositing it. A live layer, a new one every few frames (a time-driven SOP), never pays: once a
// target is dropped having served fewer than [retain_live] frames, layers wait [retain_live] sights
// for one, until a target serves that many. ponytail: two fixed thresholds, a measured break-even
// if they misjudge.
const retain_soon: i64 = 2;
const retain_live: i64 = 8;

// ponytail: one active native window; pass an explicit renderer token if sketches start owning
// multiple windows concurrently.
thread_local! {
    static ACTIVE_WINDOW: RefCell<Option<t>> = const { RefCell::new(None) };
    static HEADLESS_GPU: RefCell<Option<(Device, gpu_owner, i64)>> = const { RefCell::new(None) };
}

/// Adopt an already prepared draw without exposing it again. Scene3 values are retained as a
/// distinct family and never misrouted through a Scene2 pipeline.
pub fn prepared_draw(
    family_: family,
    blend_: Option<blend>,
    texture: Option<Rc<sampled_texture>>,
    auxiliary: Option<Rc<scene_execution::auxiliary_resource>>,
    vertex_attributes: Option<(String, bytes)>,
    samples: Option<i64>,
    value: geometry_draw,
) -> draw {
    Rc::new(sampled_draw {
        family: family_,
        blend: blend_.unwrap_or(blend::Replace),
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

fn acquire_device(_operation: &str) -> Result<(Device, bool, gpu_owner), Error> {
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
    let driver = ogpu::impl_::create_driver()?;
    let device = backend::create_device(driver)?;
    let owner = create_gpu_owner();
    HEADLESS_GPU.with(|cell| *cell.borrow_mut() = Some((device.clone(), owner.clone(), 1)));
    Ok((device, false, owner))
}

fn release_device(shared: bool, owner: &gpu_owner) {
    if shared {
        owner.shared_leases.set(owner.shared_leases.get() - 1);
    } else {
        release_headless();
    }
}

fn valid_configuration(operation: &str, configuration_: &configuration) -> Result<(), Error> {
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

fn finish_create(shared_device_lease: bool, owns_device_lease: bool, gpu_owner_: gpu_owner, runtime_: runtime::t, renderer: scene_execution::t) -> t {
    let facts = runtime::frame_facts(&runtime_);
    t(Rc::new(t_data {
        runtime: RefCell::new(runtime_),
        renderer: RefCell::new(renderer),
        gpu_owner: gpu_owner_,
        renderer_facts: Cell::new(facts),
        counters: RefCell::new(stats_counters { logical_draws: 0, logical_passes: 0 }),
        scaled: RefCell::new(new_scaled_cache()),
        shared_device_lease,
        owns_device_lease,
        assets: RefCell::new(Assets::create()),
        dead: Cell::new(false),
        snapshots: RefCell::new(side_lru::create(
            lru::cache("renderer.snapshot"),
            true,
            snapshot_cache_byte_capacity,
            Box::new(|_, _| {}),
            snapshot_cache_capacity,
        )),
        lowering: RefCell::new(renderer_lowering::create()),
        retained_plans: RefCell::new(renderer_retained_plans::create()),
        retained_scene2_segment_hits: Cell::new(0),
        retained_scene2_segment_misses: Cell::new(0),
        submissions: RefCell::new(Vec::new()),
        last_step_draws: RefCell::new(Vec::new()),
        last_step_prepared: RefCell::new(Vec::new()),
        // count only: views of layers the scene's cache charges
        retained_views: RefCell::new(Vec::new()),
        retain_after: Rc::new(Cell::new(retain_soon)),
    }))
}

fn backend_name() -> Result<String, Error> {
    ogpu::impl_::backend()
}

pub fn create(configuration_: &configuration) -> Result<t, Error> {
    let operation = "Renderer.create";
    valid_configuration(operation, configuration_)?;
    let mut runtime_ = runtime::create(
        Some(configuration_.vsync),
        Some(false),
        Some(configuration_.high_density),
        Some(&configuration_.title),
        configuration_.logical_width,
        configuration_.logical_height,
    )?;
    if let Err(e) = runtime::set_resizable(&mut runtime_, true) {
        let _ = runtime::destroy(&mut runtime_);
        return Err(e);
    }
    let surface = match runtime::surface_configuration(&mut runtime_) {
        Ok(surface) => surface,
        Err(e) => {
            let _ = runtime::destroy(&mut runtime_);
            return Err(e);
        }
    };
    let gpu_owner_ = create_gpu_owner();
    let renderer = (|| -> Result<scene_execution::t, Error> {
        let driver = ogpu::impl_::create_driver()?;
        renderer_pipeline::create_renderer(&backend_name()?, None, Some(gpu_owner_.scene_vertices.clone()), false, driver, &surface)
    })();
    match renderer {
        Err(e) => {
            let _ = runtime::destroy(&mut runtime_);
            Err(e)
        }
        Ok(renderer) => {
            let value = finish_create(false, false, gpu_owner_, runtime_, renderer);
            ACTIVE_WINDOW.with(|cell| *cell.borrow_mut() = Some(value.clone()));
            Ok(value)
        }
    }
}

/// A true layerless native target. It owns one long-lived device and pipeline coordinator until
/// `destroy`, without acquiring/presenting a window.
pub fn create_offscreen(configuration_: &configuration) -> Result<t, Error> {
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
    let renderer = (|| -> Result<scene_execution::t, Error> {
        let driver = ogpu::impl_::create_driver()?;
        renderer_pipeline::create_renderer(&backend_name()?, Some(device), Some(gpu_owner_.scene_vertices.clone()), true, driver, &surface)
    })();
    match renderer {
        Err(e) => {
            let _ = runtime::destroy(&mut runtime_);
            release_device(lease_shared, &gpu_owner_);
            Err(e)
        }
        Ok(renderer) => Ok(finish_create(lease_shared, true, gpu_owner_, runtime_, renderer)),
    }
}

fn ensure(operation: &str, value: &t) -> Result<(), Error> {
    if value.0.dead.get() { Err(fail_error(operation, ErrorKind::StaleHandle, "renderer is destroyed")) } else { Ok(()) }
}

fn sync_runtime_facts(_operation: &str, value: &t) -> Result<(), Error> {
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

fn close_submission(submission_: &submission) {
    if submission_.0.submission_state.get() == submission_state::Open {
        submission_.0.submission_state.set(submission_state::Closed);
        renderer_leases::release(&submission_.0.image_leases);
        submission_.0.owner.0.submissions.borrow_mut().retain(|s| !Rc::ptr_eq(&s.0, &submission_.0));
    }
}

pub fn begin_submission(value: &t) -> Result<submission, Error> {
    ensure("Renderer.Native.begin_submission", value)?;
    let submission_ = submission(Rc::new(submission_data {
        owner: value.clone(),
        submission_state: Cell::new(submission_state::Open),
        image_leases: renderer_leases::create(),
        ui_layers: Cell::new(0),
    }));
    value.0.submissions.borrow_mut().insert(0, submission_.clone());
    Ok(submission_)
}

fn ensure_submission(operation: &str, submission_: &submission) -> Result<(), Error> {
    if submission_.0.submission_state.get() == submission_state::Closed {
        Err(fail_error(operation, ErrorKind::StaleHandle, "submission is already closed"))
    } else {
        ensure(operation, &submission_.0.owner)
    }
}

/// Releases what only a further frame would reuse: the texture staging that frames which each
/// uploaded a texture keep for the next. For an owner going idle, or a reading that must not depend
/// on whether the last two frames uploaded.
pub fn release_staging(value: &t) -> Result<(), Error> {
    ensure("Renderer.release_staging", value)?;
    scene_execution::release_staging(&mut value.0.renderer.borrow_mut());
    Ok(())
}

pub fn stats(value: &t) -> Result<stats, Error> {
    ensure("Renderer.stats", value)?;
    let renderer = value.0.renderer.borrow();
    let timing = backend::gpu_timing(&scene_execution::queue(&renderer));
    let retained = scene_execution::retained_stats(&renderer);
    let counters = value.0.counters.borrow();
    Ok(stats {
        logical_draws: counters.logical_draws,
        logical_passes: counters.logical_passes,
        pipeline_cache_entries: scene_execution::pipeline_count(&renderer),
        uploaded_bytes: scene_execution::upload_bytes(&renderer),
        gpu_duration_seconds: timing.gpu_seconds,
        retained_plan_hits: retained.plan_hits,
        retained_plan_misses: retained.plan_misses,
        sun_shadow_passes: scene_execution::sun_shadow_passes(&renderer),
    })
}

pub fn presentation_facts_of(value: &t) -> Result<presentation_facts, Error> {
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

fn snapshot_entry(value: &t, key: &str, density: i64) -> Option<Rc<RefCell<snapshot_cache_entry>>> {
    side_lru::find(&mut value.0.snapshots.borrow_mut(), &snapshot_key(key, density))
        .map(|item| item.downcast::<RefCell<snapshot_cache_entry>>().unwrap_or_else(|_| unreachable!("snapshot table item")))
}

fn texture_key(key: &str, density: i64, generation: i64) -> String {
    format!("{}:{}:{}", key, density, generation)
}

fn gpu_texture(key: String, width: i64, height: i64, gpu_: Texture) -> Rc<sampled_texture> {
    Rc::new(sampled_texture {
        key,
        levels: Rc::new(vec![texture_level { width, height, bytes: scene_execution::empty_bytes() }]),
        sampler: scene_image_sampler(),
        gpu: Some(gpu_),
    })
}

fn snapshot(value: &t, policy_: &lease_policy, density: i64, source: retained_resource) -> Result<(i64, i64, Rc<sampled_texture>), Error> {
    let operation = "Renderer.lower_scene2";
    if density <= 0 {
        return Err(fail_error(operation, ErrorKind::InvalidArgument, "density must be positive"));
    }
    let find = |key: &str, generation: i64| -> Option<(i64, i64, Rc<sampled_texture>)> {
        let entry = snapshot_entry(value, key, density)?;
        let e = entry.borrow();
        if e.snapshot_generation == generation {
            Some((e.snapshot_width, e.snapshot_height, e.snapshot_texture.clone()))
        } else {
            None
        }
    };
    let store = |key: &str, generation: i64, width: i64, height: i64, texture: Rc<sampled_texture>, bytes_: i64| {
        let mut table = value.0.snapshots.borrow_mut();
        if bytes_ <= snapshot_cache_entry_byte_capacity {
            let item: Item = Rc::new(RefCell::new(snapshot_cache_entry {
                snapshot_generation: generation,
                snapshot_width: width,
                snapshot_height: height,
                snapshot_texture: texture,
            }));
            side_lru::add(&mut table, bytes_, snapshot_key(key, density), item);
        } else {
            side_lru::remove(&mut table, &snapshot_key(key, density));
        }
    };
    let finish = |copy: bool, key: &str, generation: i64, width: i64, height: i64, pixels: &bytes| -> Result<(i64, i64, Rc<sampled_texture>), Error> {
        if let Some(cached) = find(key, generation) {
            return Ok(cached);
        }
        let same_shape = snapshot_entry(value, key, density).filter(|entry| {
            let e = entry.borrow();
            e.snapshot_width == width && e.snapshot_height == height
        });
        let bytes_ = if copy { new_bytes(pixels.borrow().clone()) } else { pixels.clone() };
        match same_shape {
            Some(entry) => {
                let mut e = entry.borrow_mut();
                e.snapshot_generation = generation;
                // The execution key carries the generation: same-key bytes are never re-hashed
                // below, and a new generation recycles the same-shape texture with one upload.
                let mut level = e.snapshot_texture.levels[0].clone();
                level.bytes = bytes_;
                let texture = Rc::new(sampled_texture {
                    key: texture_key(key, density, generation),
                    levels: Rc::new(vec![level]),
                    ..(*e.snapshot_texture).clone()
                });
                e.snapshot_texture = texture.clone();
                Ok((width, height, texture))
            }
            None => {
                let texture = Rc::new(sampled_texture {
                    key: texture_key(key, density, generation),
                    levels: Rc::new(vec![texture_level { width, height, bytes: bytes_.clone() }]),
                    sampler: scene_image_sampler(),
                    gpu: None,
                });
                let size = bytes_.borrow().len() as i64;
                store(key, generation, width, height, texture.clone(), size);
                Ok((width, height, texture))
            }
        }
    };
    match source {
        retained_resource::Image(image) => {
            if Image::destroyed(&image) {
                return Err(fail_error(operation, ErrorKind::StaleHandle, "image snapshot is destroyed"));
            }
            let key = format!("image:{}", Image::identity(&image));
            let generation = Image::generation(&image);
            match Image::gpu_snapshot(&image) {
                Err(e) => Err(resource_error_value(operation, &e)),
                Ok(Some((width, height, generation, gpu_))) => {
                    Ok((width, height, gpu_texture(texture_key(&key, density, generation), width, height, gpu_)))
                }
                Ok(None) => {
                    if let Some(cached) = find(&key, generation) {
                        return Ok(cached);
                    }
                    match Image::borrow_snapshot(&image) {
                        Err(e) => Err(resource_error_value(operation, &e)),
                        Ok((width, height, borrowed_generation, pixels, mut lease)) => {
                            let size = pixels.borrow().len() as i64;
                            let cacheable = size <= snapshot_cache_entry_byte_capacity;
                            let bytes_ = if cacheable {
                                let copy = new_bytes(pixels.borrow().clone());
                                Image::release_snapshot(&mut lease);
                                copy
                            } else {
                                match policy_ {
                                    lease_policy::Retain_image_snapshots(leases) => {
                                        renderer_leases::retain(leases, lease);
                                        pixels
                                    }
                                    lease_policy::Copy_image_snapshots => {
                                        let copy = new_bytes(pixels.borrow().clone());
                                        Image::release_snapshot(&mut lease);
                                        copy
                                    }
                                }
                            };
                            // The generation is part of the GPU texture key: a replaced image (same
                            // identity, new pixels) then misses the executor's static image fast path
                            // and re-uploads into its same-shape texture.
                            let texture = Rc::new(sampled_texture {
                                key: texture_key(&key, density, borrowed_generation),
                                levels: Rc::new(vec![texture_level { width, height, bytes: bytes_.clone() }]),
                                sampler: scene_image_sampler(),
                                gpu: None,
                            });
                            if cacheable {
                                store(&key, borrowed_generation, width, height, texture.clone(), bytes_.borrow().len() as i64);
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
            let key = format!("text:{}", Text::identity(&text));
            let generation = Text::generation(&text);
            if !Text::destroyed(&text) {
                if let Some(cached) = find(&key, generation) {
                    return Ok(cached);
                }
            }
            match (Text::size(&text), Text::borrow_pixels(&text)) {
                (Ok((width, height)), Ok(pixels)) => finish(true, &key, generation, width, height, &pixels),
                (Err(e), _) | (_, Err(e)) => Err(resource_error_value(operation, &e)),
            }
        }
        retained_resource::Canvas(canvas) => {
            let key = format!("canvas:{}", Canvas::identity(&canvas));
            // A canvas rendered on this window's device is sampled in place.
            let gpu_ = match Canvas::gpu_snapshot(&canvas) {
                Some((width, height, generation, texture)) if runtime::is_window(&value.0.runtime.borrow()) => {
                    let (device_id, _) = backend::texture_driver_token(&texture);
                    let device = scene_execution::device(&value.0.renderer.borrow());
                    if ogpu::handle::device_id(backend::device_handle(&device)) == device_id {
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
                None => match Canvas::snapshot(&canvas) {
                    Ok((width, height, generation, pixels)) => finish(false, &key, generation, width, height, &new_bytes(pixels)),
                    Err(e) => Err(resource_error_value(operation, &e)),
                },
            }
        }
    }
}

fn lower_scene2_uncached(
    value: &t,
    policy_: &lease_policy,
    density: i64,
    resolve: &dyn Fn(i64) -> Option<retained_resource>,
    ir_: &Rc<RenderIr>,
) -> Result<Vec<draw>, Error> {
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
                    && source.0.marks_stamp.get() == marks::gpu_stamp(token)
                    && source.0.marks_count.get() == marks::gpu_count(token) =>
            {
                let device = scene_execution::device(&value.0.renderer.borrow());
                if ogpu::handle::device_id(backend::device_handle(&source.0.marks_gpu.0.gpu_device)) == ogpu::handle::device_id(backend::device_handle(&device)) {
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

/// OCaml's structural equality of two command arrays.
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
    // OCaml's Hashtbl.hash looks at a bounded sample; the plan is verified exactly after a hit.
    let mut h = values.len() as i64;
    for v in values.iter().take(16) {
        h = rays_math::hash::mix(h, v.to_bits() as i64);
    }
    h
}

fn lower_scene2_with_policy(
    value: &t,
    policy_: &lease_policy,
    density: i64,
    resolve: &dyn Fn(i64) -> Option<retained_resource>,
    ir_: &Rc<RenderIr>,
) -> Result<Vec<draw>, Error> {
    if value.0.dead.get() {
        return lower_scene2_uncached(value, policy_, density, resolve, ir_);
    }
    let commands = ir::commands_readonly(ir_);
    let ir_id = ir::identity(ir_);
    let facts = presentation_facts_of(value).expect("presentation facts");
    let extent = (facts.logical_width, facts.logical_height, facts.drawable_width, facts.drawable_height);
    let same_context = |p: &plan| p.plan_density == density && p.plan_extent == extent;
    let slow = || -> Result<Vec<draw>, Error> {
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
                    mix(geometry.indices.len() as i64 ^ geometry.indices.iter().take(16).fold(0i64, |h, v| h.wrapping_mul(31) ^ v));
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
                p.plan_command_count == command_count as i64
                    && same_context(&p)
                    && renderer_retained_plans::same_resources(&p.plan_resources, &resources)
                    && commands_equal(ir::commands_readonly(&p.plan_ir), commands)
            })
        };
        if !cacheable {
            return lower_scene2_uncached(value, policy_, density, resolve, ir_);
        }
        match hit {
            Some(p) => {
                renderer_retained_plans::rebind(&value.0.retained_plans.borrow(), &p, ir_id, ir_);
                let draws = p.borrow().plan_draws.clone();
                Ok(draws)
            }
            None => {
                let draws = lower_scene2_uncached(value, policy_, density, resolve, ir_)?;
                if draws.iter().any(|d| d.texture.as_ref().is_some_and(|tx| renderer_lowering::sampled_texture_bytes(tx) > snapshot_cache_entry_byte_capacity))
                    || has_gpu_shapes(commands)
                {
                    Ok(draws)
                } else {
                    let p = Rc::new(RefCell::new(plan {
                        plan_ir_id: ir_id,
                        plan_command_count: command_count as i64,
                        plan_source_bytes: renderer_retained_plans::plan_source_bytes(commands),
                        plan_density: density,
                        plan_extent: extent,
                        plan_resources: resources,
                        plan_ir: ir_.clone(),
                        plan_draws: draws.clone(),
                    }));
                    renderer_retained_plans::add(&mut value.0.retained_plans.borrow_mut(), fingerprint, p);
                    Ok(draws)
                }
            }
        }
    };
    let by_ir = renderer_retained_plans::find_ir(&value.0.retained_plans.borrow(), ir_id);
    match by_ir {
        Some(p) if same_context(&p.borrow()) && p.borrow().plan_resources.is_empty() => Ok(p.borrow().plan_draws.clone()),
        Some(p) if same_context(&p.borrow()) => {
            let (cacheable, resources) = renderer_retained_plans::resource_stamps(resolve, commands);
            if cacheable && renderer_retained_plans::same_resources(&p.borrow().plan_resources, &resources) {
                Ok(p.borrow().plan_draws.clone())
            } else {
                slow()
            }
        }
        _ => slow(),
    }
}

pub fn lower_scene2(value: &t, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ir_: &Rc<RenderIr>) -> Result<Vec<draw>, Error> {
    lower_scene2_with_policy(value, &lease_policy::Copy_image_snapshots, density, resource_, ir_)
}

pub fn replay_step(clear: Option<(f64, f64, f64, f64)>, identity: &str, version: i64, value: &t) -> Result<Option<()>, Error> {
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
    value: &t,
    draws: &[draw],
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
            let draws: Vec<draw> = if same_draw_list(draws, &value.0.last_step_draws.borrow()) {
                value.0.last_step_prepared.borrow().clone()
            } else {
                let prepared: Vec<draw> = draws
                    .iter()
                    .map(|x| {
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
                        Rc::new(sampled_draw {
                            family: x.family,
                            blend: x.blend,
                            texture: x.texture.clone(),
                            auxiliary: x.auxiliary.clone(),
                            vertex_attributes: x.vertex_attributes.clone(),
                            samples: x.samples,
                            draw: draw_,
                        })
                    })
                    .collect();
                *value.0.last_step_draws.borrow_mut() = draws.to_vec();
                *value.0.last_step_prepared.borrow_mut() = prepared.clone();
                prepared
            };
            let scaled = scale_sampled_cached(&mut value.0.scaled.borrow_mut(), &value.0.renderer_facts.get(), &draws);
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
            result.map(|presented| {
                count_draws(&mut value.0.counters.borrow_mut(), scaled.len() as i64);
                presented
            })
        }
    }
}

pub fn step(clear: Option<(f64, f64, f64, f64)>, value: &t, draws: &[draw]) -> Result<(), Error> {
    step_core(&mut || {}, clear, None, None, value, draws).map(|_| ())
}

pub fn draw(clear: Option<(f64, f64, f64, f64)>, value: &t, draws: &[draw]) -> Result<bool, Error> {
    step_core(&mut || {}, clear, None, None, value, draws)
}

fn lower_scene2_submission(submission_: &submission, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ir_: &Rc<RenderIr>) -> Result<batch, Error> {
    if let Err(e) = ensure_submission("Renderer.Native.lower_scene2", submission_) {
        close_submission(submission_);
        return Err(e);
    }
    // ponytail: the OCaml version turns an exception raised by the resolver into an error; a Rust
    // resolver that panics unwinds through here.
    match lower_scene2_with_policy(
        &submission_.0.owner,
        &lease_policy::Retain_image_snapshots(submission_.0.image_leases.clone()),
        density,
        resource_,
        ir_,
    ) {
        Ok(draws) => Ok(batch { batch_owner: submission_.clone(), batch_draws: draws }),
        Err(e) => {
            close_submission(submission_);
            Err(e)
        }
    }
}

fn retained_scene2_segment_bytes(draws: &[draw]) -> i64 {
    draws
        .iter()
        .map(|d| {
            let value = &d.draw;
            (value.mesh.vertex_bytes_len() + value.mesh.indices.borrow().len()) as i64
                + value.state.transform_uniforms.as_ref().map_or(0, |b| b.borrow().len() as i64)
                + d.texture.as_ref().map_or(0, |tx| renderer_lowering::sampled_texture_bytes(tx))
                + d.auxiliary.as_ref().map_or(0, |a| a.buffer.borrow().len() as i64 + renderer_lowering::sampled_texture_bytes(&a.texture))
        })
        .sum()
}

fn lower_scene2_segment(
    submission_: &submission,
    identity: i64,
    version: i64,
    cacheable: bool,
    density: i64,
    resource_: &dyn Fn(i64) -> Option<retained_resource>,
    ir_: &Rc<RenderIr>,
) -> Result<batch, Error> {
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
            entry.segment_version == version
                && entry.segment_density == density
                && entry.segment_width == facts.logical_width
                && entry.segment_height == facts.logical_height
        })
    } else {
        None
    };
    if let Some(entry) = found {
        owner.0.retained_scene2_segment_hits.set(owner.0.retained_scene2_segment_hits.get() + 1);
        return Ok(batch { batch_owner: submission_.clone(), batch_draws: entry.segment_draws.clone() });
    }
    let lowered = lower_scene2_submission(submission_, density, resource_, ir_)?;
    if cacheable {
        owner.0.retained_scene2_segment_misses.set(owner.0.retained_scene2_segment_misses.get() + 1);
        let bytes_ = retained_scene2_segment_bytes(&lowered.batch_draws);
        renderer_retained_plans::add_segment(
            &mut owner.0.retained_plans.borrow_mut(),
            identity,
            bytes_,
            segment {
                segment_version: version,
                segment_density: density,
                segment_width: facts.logical_width,
                segment_height: facts.logical_height,
                segment_draws: lowered.batch_draws.clone(),
            },
        );
    }
    Ok(lowered)
}

/// Lowers PXUI instances into one `Ui` draw per batch. Texture ids resolve through `resource`; id 0
/// samples a white texel.
fn lower_ui(submission_: &submission, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ui: &Shapes) -> Result<batch, Error> {
    let operation = "Renderer.Native.lower_ui";
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
    let width = facts.logical_width as f64;
    let height = facts.logical_height as f64;
    let framebuffer = (0, 0, facts.logical_width, facts.logical_height);
    let instances = ui.instances();
    let texture = |id: i64| -> Result<Rc<sampled_texture>, Error> {
        if id <= 0 {
            return Ok(ui_white_texture());
        }
        match resource_(id) {
            None => Err(fail_error(operation, ErrorKind::InvalidState, "UI texture resource id is unbound")),
            Some(source) => {
                let policy_ = lease_policy::Retain_image_snapshots(submission_.0.image_leases.clone());
                let (_, _, texture) = snapshot(owner, &policy_, density, source)?;
                let mut copy = (*texture).clone();
                copy.sampler = ui_sampler();
                Ok(Rc::new(copy))
            }
        }
    };
    let layer = submission_.0.ui_layers.get();
    submission_.0.ui_layers.set(layer + 1);
    let mut draws: Vec<draw> = Vec::new();
    let batches = ui.batches();
    let mut failure_: Option<Error> = None;
    for (index, batch_) in batches.iter().enumerate() {
        let (scissor_width, scissor_height) = match &batch_.clip {
            None => (framebuffer.2, framebuffer.3),
            Some(clip) => (clip.width as i64, clip.height as i64),
        };
        if batch_.count == 0 || scissor_width <= 0 || scissor_height <= 0 {
            continue;
        }
        let built = (|| -> Result<draw, Error> {
            let texture = texture(batch_.texture)?;
            let mut lowering = owner.0.lowering.borrow_mut();
            let affine = renderer_lowering::write_ui_affine(
                &lowering,
                &ir::Transform { xx: 2.0 / width, xy: 0.0, yx: 0.0, yy: -2.0 / height, tx: -1.0, ty: 1.0 },
            );
            let first = (batch_.first * ui_batch::instance_bytes()) as usize;
            let length = (batch_.count * ui_batch::instance_bytes()) as usize;
            let slot_key = layer * 65536 + index as i64;
            let slot = renderer_lowering::find_ui_slot(&mut lowering, slot_key);
            let mut s = slot.borrow_mut();
            if s.ui_vertices.borrow().len() != length || !renderer_lowering::same_bytes_range(instances, first, &s.ui_vertices.borrow()) {
                s.ui_vertices = new_bytes(instances[first..first + length].to_vec());
            }
            if s.ui_indices.borrow().len() as i64 != batch_.count * 24 {
                s.ui_indices = renderer_lowering::ui_index_bytes(&mut lowering, batch_.count);
            }
            if s.ui_affine.borrow()[..] != affine.borrow()[..] {
                s.ui_affine = new_bytes(affine.borrow().clone());
            }
            let scissor = match &batch_.clip {
                None => framebuffer,
                Some(clip) => {
                    let x = 0.max(clip.x.floor() as i64);
                    let y = 0.max(clip.y.floor() as i64);
                    let right = facts.logical_width.min((clip.x + clip.width).ceil() as i64);
                    let bottom = facts.logical_height.min((clip.y + clip.height).ceil() as i64);
                    (x, y, 0.max(right - x), 0.max(bottom - y))
                }
            };
            Ok(Rc::new(sampled_draw {
                family: family::Ui,
                blend: blend::Alpha,
                texture: Some(texture),
                auxiliary: None,
                vertex_attributes: None,
                samples: 1,
                draw: geometry_draw {
                    mesh: scene_execution::mesh {
                        key: scene_execution::mesh_key::Host(format!("ui:{}", slot_key)),
                        vertices: s.ui_vertices.clone(),
                        shared_vertices: None,
                        vertex_count: 4 * batch_.count,
                        indices: s.ui_indices.clone(),
                        index_count: 6 * batch_.count,
                        primitive: ogpu::render_pass::Primitive::TriangleList,
                    },
                    state: scene_execution::state {
                        transform_uniforms: Some(s.ui_affine.clone()),
                        ..renderer_lowering::default_state(framebuffer, scissor)
                    },
                },
            }))
        })();
        match built {
            Ok(value) => draws.push(value),
            Err(e) => {
                failure_ = Some(e);
                break;
            }
        }
    }
    match failure_ {
        Some(e) => {
            close_submission(submission_);
            Err(e)
        }
        None => Ok(batch { batch_owner: submission_.clone(), batch_draws: draws }),
    }
}

/// Adopts already prepared non-Scene2 draws into this submission.
pub fn adopt_draws(submission_: &submission, draws: Vec<draw>) -> Result<batch, Error> {
    if let Err(e) = ensure_submission("Renderer.Native.adopt_draws", submission_) {
        close_submission(submission_);
        return Err(e);
    }
    Ok(batch { batch_owner: submission_.clone(), batch_draws: draws })
}

/// Consumes the submission on either success or failure.
fn step_submission(
    clear: Option<(f64, f64, f64, f64)>,
    identity: Option<&str>,
    version: Option<i64>,
    submission_: &submission,
    batches: &[batch],
) -> Result<(), Error> {
    if let Err(e) = ensure_submission("Renderer.Native.step", submission_) {
        close_submission(submission_);
        return Err(e);
    }
    let result = if batches.iter().any(|b| !Rc::ptr_eq(&b.batch_owner.0, &submission_.0)) {
        Err(fail_error("Renderer.Native.step", ErrorKind::InvalidArgument, "draw batch belongs to another submission"))
    } else {
        let draws: Vec<draw> = batches.iter().flat_map(|b| b.batch_draws.iter().cloned()).collect();
        step_core(&mut || close_submission(submission_), clear, identity, version, &submission_.0.owner, &draws).map(|_| ())
    };
    close_submission(submission_);
    result
}

/// The completed frame as drawable-sized RGBA bytes.
pub fn capture(value: &t) -> Result<Vec<u8>, Error> {
    ensure("Renderer.capture", value)?;
    let facts = presentation_facts_of(value)?;
    scene_execution::read_pixels(&mut value.0.renderer.borrow_mut(), facts.drawable_width * 4)
}

pub fn read_pixels(value: &t, bytes_per_row: i64) -> Result<Vec<u8>, Error> {
    ensure("Renderer.read_pixels", value)?;
    scene_execution::read_pixels(&mut value.0.renderer.borrow_mut(), bytes_per_row)
}

/// An offscreen execution's completed frame texture, on the device it leased (the presenting
/// window's when one exists). A window execution samples a Canvas published from it directly;
/// readback happens only for CPU access.
pub fn offscreen_target(value: &t) -> Result<Texture, Error> {
    let operation = "Renderer.offscreen_target";
    ensure(operation, value)?;
    if runtime::is_window(&value.0.runtime.borrow()) {
        Err(fail_error(operation, ErrorKind::Unsupported, "operation requires an offscreen execution"))
    } else {
        Ok(scene_execution::target(&value.0.renderer.borrow()))
    }
}

fn release_view(view: &Rc<retained_view>) {
    let target = view.view_target.borrow_mut().take();
    if let Some(target) = target {
        view.view_retain_after.set(if view.view_seen.get() < retain_live { retain_live } else { retain_soon });
        let _ = Canvas::destroy(&target.target_canvas);
        let _ = destroy(&target.target_exec);
    }
}

fn release_views(value: &t) {
    let views = std::mem::take(&mut *value.0.retained_views.borrow_mut());
    for view in views {
        release_view(&view);
    }
}

/// A window execution fails with `InvalidArgument`, destroying nothing, while GPU leases or
/// offscreen executions still share its device.
pub fn destroy(value: &t) -> Result<(), Error> {
    if value.0.dead.get() {
        return Ok(());
    }
    release_views(value);
    if runtime::is_window(&value.0.runtime.borrow()) && value.0.gpu_owner.shared_leases.get() > 0 {
        return Err(fail_error("Renderer.destroy", ErrorKind::InvalidArgument, &format!("{} GPU lease(s) still share this window's device; release them first", value.0.gpu_owner.shared_leases.get())));
    }
    let open: Vec<submission> = value.0.submissions.borrow().clone();
    for submission_ in &open {
        close_submission(submission_);
    }
    let assets = Assets::destroy(&mut value.0.assets.borrow_mut());
    side_lru::clear(&mut value.0.snapshots.borrow_mut());
    renderer_retained_plans::clear(&mut value.0.retained_plans.borrow_mut());
    value.0.last_step_draws.borrow_mut().clear();
    value.0.last_step_prepared.borrow_mut().clear();
    renderer_lowering::clear(&mut value.0.lowering.borrow_mut());
    value.0.dead.set(true);
    ACTIVE_WINDOW.with(|cell| {
        let current = cell.borrow().clone();
        if let Some(current) = current {
            if Rc::ptr_eq(&current.0, &value.0) {
                *cell.borrow_mut() = None;
            }
        }
    });
    let core = scene_execution::destroy(&mut value.0.renderer.borrow_mut());
    let shell = runtime::destroy(&mut value.0.runtime.borrow_mut());
    if value.0.owns_device_lease {
        release_device(value.0.shared_device_lease, &value.0.gpu_owner);
    }
    match (assets, core, shell) {
        (Err(e), _, _) => Err(resource_error_value("Renderer.destroy", &e)),
        (Ok(()), Err(e), _) => Err(e),
        (Ok(()), Ok(()), Err(e)) => Err(e),
        (Ok(()), Ok(()), Ok(())) => Ok(()),
    }
}

/// The first sight of a layer draws it directly; the second renders it offscreen. `None` when the
/// target cannot be made: the view then stays direct for as long as the layer does.
fn make_retained_target(f: &presentation_facts, draws: &[draw]) -> Option<retained_target> {
    let dw = f.drawable_width;
    let dh = f.drawable_height;
    let exec = create_offscreen(&configuration {
        logical_width: f.logical_width,
        logical_height: f.logical_height,
        drawable_width: dw,
        drawable_height: dh,
        title: "Rays retained view".to_owned(),
        vsync: false,
        high_density: true,
    })
    .ok()?;
    // (OCaml evaluates the three of the tuple right to left: canvas, target, step)
    let canvas = Canvas::create(dw, dh);
    let target = offscreen_target(&exec);
    let stepped = step(Some((0.0, 0.0, 0.0, 0.0)), &exec, draws);
    let fail_with = |canvas: Option<&Canvas::t>| -> Option<retained_target> {
        if let Some(c) = canvas {
            let _ = Canvas::destroy(c);
        }
        let _ = destroy(&exec);
        None
    };
    match (stepped, target, canvas) {
        (Ok(()), Ok(texture), Ok(canvas)) => {
            let published = Canvas::publish_gpu(&canvas, &texture);
            let command = ir::create(vec![Command::Image(ir::Image {
                resource_id: 1,
                source: ir::Rect { x: 0.0, y: 0.0, width: dw as f64, height: dh as f64 },
                destination: ir::Rect { x: 0.0, y: 0.0, width: f.logical_width as f64, height: f.logical_height as f64 },
            })]);
            match (published, command) {
                (Ok(()), Ok(ir_)) => Some(retained_target { target_exec: exec.clone(), target_canvas: canvas, target_ir: Rc::new(ir_) }),
                _ => fail_with(Some(&canvas)),
            }
        }
        (_, _, Ok(canvas)) => fail_with(Some(&canvas)),
        _ => fail_with(None),
    }
}

/// Adopts a 3D layer's draws, retaining the layer's render once it keeps the same `layer` identity
/// on consecutive frames. A window execution only; offscreen ones adopt the draws directly.
pub fn adopt_retained_view(submission_: &submission, density: i64, layer: &Rc<prepared_scene3>, draws: Vec<draw>) -> Result<batch, Error> {
    let value = &submission_.0.owner;
    let direct = |draws: Vec<draw>| adopt_draws(submission_, draws);
    if !runtime::is_window(&value.0.runtime.borrow()) {
        return direct(draws);
    }
    let f = match presentation_facts_of(value) {
        Ok(f) => f,
        Err(_) => return direct(draws),
    };
    let drawable = (f.drawable_width, f.drawable_height);
    let position = value.0.retained_views.borrow().iter().position(|view| Rc::ptr_eq(&view.view_layer, layer));
    let current: Option<Rc<retained_view>> = match position {
        Some(i) => {
            let view = value.0.retained_views.borrow()[i].clone();
            if view.view_drawable == drawable {
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
            let view = Rc::new(retained_view {
                view_layer: layer.clone(),
                view_drawable: drawable,
                view_target: RefCell::new(None),
                view_failed: Cell::new(false),
                view_seen: Cell::new(1),
                view_retain_after: value.0.retain_after.clone(),
            });
            let evicted = {
                let mut views = value.0.retained_views.borrow_mut();
                views.insert(0, view);
                if views.len() > retained_view_capacity { views.pop() } else { None }
            };
            if let Some(evicted) = evicted {
                release_view(&evicted);
            }
            direct(draws)
        }
        Some(view) => {
            view.view_seen.set(view.view_seen.get() + 1);
            if view.view_target.borrow().is_none() && !view.view_failed.get() && view.view_seen.get() >= value.0.retain_after.get() {
                match make_retained_target(&f, &draws) {
                    Some(target) => {
                        *view.view_target.borrow_mut() = Some(target);
                        // from here [view_seen] counts the frames the target serves
                        view.view_seen.set(0);
                    }
                    None => view.view_failed.set(true),
                }
            }
            let target = view.view_target.borrow().as_ref().map(|target| (target.target_canvas.clone(), target.target_ir.clone()));
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
pub fn acquire_gpu() -> Result<gpu, Error> {
    let operation = "Renderer.acquire_gpu";
    let (device, gpu_shared, gpu_owner_) = acquire_device(operation)?;
    match backend::create_queue(&device) {
        Err(e) => {
            release_device(gpu_shared, &gpu_owner_);
            Err(e)
        }
        Ok(queue) => Ok(gpu(Rc::new(gpu_data {
            gpu_device: device,
            gpu_queue: queue,
            gpu_owner: gpu_owner_,
            gpu_shared,
            gpu_released: Cell::new(false),
        }))),
    }
}

pub fn gpu_device(value: &gpu) -> Device {
    value.0.gpu_device.clone()
}

pub fn gpu_queue(value: &gpu) -> Queue {
    value.0.gpu_queue.clone()
}

/// True when the device is the presenting window's.
pub fn gpu_shared(value: &gpu) -> bool {
    value.0.gpu_shared
}

pub fn release_gpu(value: &gpu) {
    if !value.0.gpu_released.get() {
        let owned: Vec<gpu_marks> = value
            .0
            .gpu_owner
            .marks_registry
            .borrow()
            .values()
            .filter(|marks_| Rc::ptr_eq(&marks_.0.marks_gpu.0, &value.0))
            .cloned()
            .collect();
        for marks_ in &owned {
            close_gpu_marks(marks_);
        }
        value.0.gpu_released.set(true);
        let _ = backend::destroy_queue(&value.0.gpu_queue);
        release_device(value.0.gpu_shared, &value.0.gpu_owner);
    }
}

#[allow(non_snake_case)]
pub mod Native {
    use super::*;

    pub use super::{batch, new_scaled_cache, scale_draws, scale_sampled_cached, scale_sampled_resources, scaled_cache, submission};
    pub use super::{adopt_draws, adopt_retained_view, begin_submission};

    /// The borrowed SDL/runtime shell; its lifetime remains owned by `t`.
    pub fn runtime_of(value: &t, f: &mut dyn FnMut(&runtime::t)) {
        f(&value.0.runtime.borrow())
    }

    /// The same shell, mutably (an offscreen shell refuses an IME area and a cursor).
    pub fn runtime_of_mut(value: &t, f: &mut dyn FnMut(&mut runtime::t)) {
        f(&mut value.0.runtime.borrow_mut())
    }

    pub fn gpu_vertices_owner(value: &t) -> scene_execution::gpu_vertices_owner {
        value.0.gpu_owner.scene_vertices.clone()
    }

    pub fn create_offscreen_with_gpu(gpu_: &gpu, configuration_: &configuration) -> Result<t, Error> {
        let operation = "Renderer.Native.create_offscreen_with_gpu";
        if gpu_.0.gpu_released.get() {
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
        let renderer = (|| -> Result<scene_execution::t, Error> {
            let driver = ogpu::impl_::create_driver()?;
            renderer_pipeline::create_renderer(
                &backend_name()?,
                Some(gpu_.0.gpu_device.clone()),
                Some(gpu_.0.gpu_owner.scene_vertices.clone()),
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
            Ok(renderer) => Ok(finish_create(false, false, gpu_.0.gpu_owner.clone(), runtime_, renderer)),
        }
    }

    /// A `Marks`-family source for one mark set (specification/marks.md).
    pub fn create_gpu_marks(gpu_: &gpu) -> Result<gpu_marks, Error> {
        let operation = "Renderer.create_gpu_marks";
        if gpu_.0.gpu_released.get() {
            return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU lease is released"));
        }
        if gpu_.0.gpu_owner.marks_registry.borrow().len() >= gpu_marks_capacity {
            return Err(fail_error(operation, ErrorKind::Capacity, "GPU marks capacity reached"));
        }
        let Some(identity) = fresh_gpu_marks_id() else {
            return Err(fail_error(operation, ErrorKind::Capacity, "GPU marks identity capacity reached"));
        };
        let value = gpu_marks(Rc::new(gpu_marks_data {
            marks_id: identity,
            marks_gpu: gpu_.clone(),
            marks_domain: std::thread::current().id(),
            marks_closed: Cell::new(false),
            marks_stamp: Cell::new(0),
            marks_count: Cell::new(0),
            marks_owned: RefCell::new(vec![None; 6]),
            marks_last: RefCell::new((0..6).map(|_| scene_execution::empty_bytes()).collect()),
            marks_device: RefCell::new(Vec::new()),
            marks_registration: RefCell::new(None),
        }));
        gpu_.0.gpu_owner.marks_registry.borrow_mut().insert(identity, value.clone());
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
        value: &gpu_marks,
        kind: i64,
        count: i64,
        attributes: &[marks_source],
    ) -> Result<marks::GpuToken, Error> {
        let operation = "Renderer.gpu_marks";
        let v = &value.0;
        if v.marks_closed.get() || v.marks_gpu.0.gpu_released.get() {
            return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU marks or lease is closed"));
        }
        if v.marks_domain != std::thread::current().id() {
            return Err(fail_error(operation, ErrorKind::InvalidArgument, "GPU marks used on another domain"));
        }
        if count <= 0 || count > 1_000_000 || attributes.len() != 5 {
            return Err(fail_error(operation, ErrorKind::InvalidArgument, "a mark set has one to 1,000,000 instances and five attributes"));
        }
        let mut changed = v.marks_registration.borrow().is_none() || v.marks_count.get() != count;
        let mut device: Vec<(Rc<dyn Fn() -> Option<Buffer>>, Buffer)> = Vec::new();
        let upload = |index: usize, data: &bytes, changed: &mut bool| -> Result<Buffer, Error> {
            let owned = v.marks_owned.borrow()[index].clone();
            let length = data.borrow().len() as i64;
            let size = 16.max(length);
            let buffer = match owned {
                Some(b) if backend::buffer_size(&b) >= size => b,
                _ => {
                    let capacity = 16.max(2 * length);
                    let b = backend::create_buffer(
                        &v.marks_gpu.0.gpu_device,
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
                    v.marks_owned.borrow_mut()[index] = Some(b.clone());
                    v.marks_last.borrow_mut()[index] = scene_execution::empty_bytes();
                    *changed = true;
                    b
                }
            };
            let last = v.marks_last.borrow()[index].clone();
            if Rc::ptr_eq(&last, data) || *last.borrow() == *data.borrow() {
                Ok(buffer)
            } else {
                backend::write_buffer(&buffer, 0, &data.borrow())?;
                v.marks_last.borrow_mut()[index] = data.clone();
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
                marks_source::Host(data, stride) => {
                    strides[index] = *stride;
                    upload(index, data, &mut changed)?
                }
                marks_source::Device(source, stride) => {
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
            marks_source::Device(..) => 1i64,
            marks_source::Host(..) => 0,
        };
        let mut params = vec![0u8; 32];
        for (i, word) in [kind, strides[0], strides[1], strides[2], strides[3], strides[4], mode(2), mode(3)].into_iter().enumerate() {
            params[i * 4..i * 4 + 4].copy_from_slice(&(word as i32).to_le_bytes());
        }
        let params_buffer = upload(5, &new_bytes(params), &mut changed)?;
        let token = || marks::gpu_token(v.marks_id, count, v.marks_stamp.get()).map_err(|m| oerror::make(operation, ErrorKind::InvalidArgument, &m));
        let same_device = device.len() == v.marks_device.borrow().len()
            && device.iter().zip(v.marks_device.borrow().iter()).all(|((_, a), (_, b))| same_buffer(a, b));
        if !changed && valid_gpu_marks(value) && same_device {
            return token();
        }
        unregister_marks(value);
        v.marks_stamp.set(v.marks_stamp.get() + 1);
        v.marks_count.set(count);
        *v.marks_device.borrow_mut() = device;
        let mut extra: Vec<Buffer> = buffers[1..].to_vec();
        extra.push(params_buffer);
        let owner_value = value.clone();
        match scene_execution::register_gpu_vertices(
            &v.marks_gpu.0.gpu_owner.scene_vertices,
            Some(extra),
            None,
            indirect,
            None,
            None,
            &gpu_marks_key(value),
            &v.marks_gpu.0.gpu_device,
            &buffers[0],
            4 * count,
            Box::new(move || valid_gpu_marks(&owner_value)),
        ) {
            Err(e) => {
                v.marks_device.borrow_mut().clear();
                Err(e)
            }
            Ok(registration) => {
                *v.marks_registration.borrow_mut() = Some(registration);
                token()
            }
        }
    }

    pub use super::close_gpu_marks;

    /// Draws the renderer encodes for the batch after neighbours merge (-1 on error).
    pub fn encoded_draws_for_test(batch_: &batch) -> i64 {
        scene_execution::Private::coalesced_draws_for_test(&batch_.batch_draws)
    }

    pub fn snapshot_count_for_test(value: &t) -> i64 {
        side_lru::length(&mut value.0.snapshots.borrow_mut())
    }

    pub fn window_handle(value: &t, f: &mut dyn FnMut(Option<&sdl3::window::Window>)) {
        f(runtime::window_handle(&value.0.runtime.borrow()))
    }

    pub fn lower_scene2(submission_: &submission, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ir_: &Rc<RenderIr>) -> Result<batch, Error> {
        lower_scene2_submission(submission_, density, resource_, ir_)
    }

    pub fn lower_scene2_segment(
        submission_: &submission,
        identity: i64,
        version: i64,
        cacheable: bool,
        density: i64,
        resource_: &dyn Fn(i64) -> Option<retained_resource>,
        ir_: &Rc<RenderIr>,
    ) -> Result<batch, Error> {
        super::lower_scene2_segment(submission_, identity, version, cacheable, density, resource_, ir_)
    }

    pub fn lower_ui(submission_: &submission, density: i64, resource_: &dyn Fn(i64) -> Option<retained_resource>, ui: &Shapes) -> Result<batch, Error> {
        super::lower_ui(submission_, density, resource_, ui)
    }

    pub fn step(
        clear: Option<(f64, f64, f64, f64)>,
        identity: Option<&str>,
        version: Option<i64>,
        submission_: &submission,
        batches: &[batch],
    ) -> Result<(), Error> {
        step_submission(clear, identity, version, submission_, batches)
    }

    pub fn replay(clear: Option<(f64, f64, f64, f64)>, identity: &str, version: i64, value: &t) -> Result<Option<()>, Error> {
        replay_step(clear, identity, version, value)
    }

    /// Idempotently releases an unsubmitted transaction.
    pub fn cancel(submission_: &submission) {
        close_submission(submission_)
    }

    /// What this renderer holds to stage texture uploads through.
    pub fn staging_bytes(value: &t) -> i64 {
        scene_execution::Private::staging_bytes(&value.0.renderer.borrow())
    }

    /// Whether the device has memoryless attachments, and how many this renderer's pool holds.
    pub fn memoryless_attachments(value: &t) -> (bool, i64) {
        scene_execution::Private::memoryless_attachments(&value.0.renderer.borrow())
    }

    pub fn retained_scene2_segment_stats(value: &t) -> (i64, i64, i64) {
        (
            renderer_retained_plans::segment_count(&mut value.0.retained_plans.borrow_mut()),
            value.0.retained_scene2_segment_hits.get(),
            value.0.retained_scene2_segment_misses.get(),
        )
    }
}

pub fn create_gpu_marks(gpu_: &gpu) -> Result<gpu_marks, Error> {
    Native::create_gpu_marks(gpu_)
}

pub fn gpu_marks_token(
    indirect: Option<(Rc<dyn Fn() -> Option<Buffer>>, i64)>,
    value: &gpu_marks,
    kind: i64,
    count: i64,
    attributes: &[marks_source],
) -> Result<marks::GpuToken, Error> {
    Native::gpu_marks_token(indirect, value, kind, count, attributes)
}

static next_gpu_positions: AtomicI64 = AtomicI64::new(0);

/// The packed float3 positions, normals (if registered) and count that the valid registration under
/// `key` borrows, for another consumer of the same device (the path tracer); the producer keeps them
/// while the device mesh naming `key` lives. Initial domain.
pub fn gpu_positions(gpu_: &gpu, key: &str) -> Option<(Buffer, Option<Buffer>, i64)> {
    if gpu_.0.gpu_released.get() {
        return None;
    }
    match scene_execution::Private::find_gpu_vertices(&gpu_.0.gpu_owner.scene_vertices, key) {
        Some((positions, extra, count)) if extra.len() <= 1 => Some((positions, extra.into_iter().next(), count)),
        _ => None,
    }
}

/// The indices (count) and colours (width) borrowed under `key`.
pub fn gpu_streams(gpu_: &gpu, key: &str) -> Option<(Option<(Buffer, i64)>, Option<(Buffer, i64)>)> {
    if gpu_.0.gpu_released.get() {
        return None;
    }
    scene_execution::Private::find_gpu_streams(&gpu_.0.gpu_owner.scene_vertices, key)
}

/// Borrows another producer's `count` packed float3 positions (and normals) for a Scene3 mesh drawn
/// from the device: the key a device mesh names and its unregistration (initial domain). The
/// producer keeps the buffers while `valid`; a draw of a registration no longer valid is refused as
/// stale. Optional `indices` borrows uint32 triangle indices with their settled count; `colors`
/// borrows packed float3/float4 colours with their component width. At most 128 borrowed sources are
/// live.
pub fn register_gpu_positions(
    gpu_: &gpu,
    positions: &Buffer,
    normals: Option<Buffer>,
    indices: Option<(Buffer, i64)>,
    colors: Option<(Buffer, i64)>,
    count: i64,
    valid: Rc<dyn Fn() -> bool>,
) -> Result<(String, Box<dyn Fn()>), Error> {
    let operation = "Renderer.register_gpu_positions";
    if gpu_.0.gpu_released.get() {
        return Err(fail_error(operation, ErrorKind::StaleHandle, "GPU lease is released"));
    }
    let key = format!("positions:{}", next_gpu_positions.fetch_add(1, Ordering::SeqCst));
    let lease = gpu_.clone();
    let registration = scene_execution::register_gpu_vertices(
        &gpu_.0.gpu_owner.scene_vertices,
        Some(normals.into_iter().collect()),
        Some(12),
        None,
        indices,
        colors,
        &key,
        &gpu_.0.gpu_device,
        positions,
        count,
        Box::new(move || !lease.0.gpu_released.get() && valid()),
    )?;
    Ok((key, Box::new(move || scene_execution::Private::unregister_gpu_vertices(&registration))))
}
