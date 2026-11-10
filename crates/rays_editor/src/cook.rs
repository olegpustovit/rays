//! The editor's cook: one geometry object's latest cook (its displayed SOP graph, the sketch's
//! prepared value, and bounds in the object's own space), the async worker, the schedule, the probe
//! footers and the framing jobs.
use std::cell::{OnceCell, RefCell};
use std::collections::{BTreeMap, VecDeque};
use std::rc::Rc;
use std::sync::Arc;

use editor_document::settings;
use flow_graph::probe;
use flow_ir::flow_ir::gpu;
use flow_sop::network::Network;
use flow_sop::value_lane;
use param::Effects;
use rays::frame;
use rays_math::{mat4, vec3};
use rdk_rays::geometry_view as gv;
use rdk_rays::surface;
use sop::async_cook::{self, Completion, Prepared as AsyncPrepared};
use sop::context::{self, dependencies as deps};
use sop::edit_graph;
use sop::node;
use sop::session::Output;

use crate::pick;
use crate::schedule;
use crate::timeline;

pub type Bounds = (vec3::T, vec3::T);
pub type Geometry = rdk_core::geometry::T;
pub type Key2 = (i64, i64);

/// One geometry object's latest cook: its displayed SOP graph, the sketch's prepared value, and
/// bounds in the object's own space.
pub struct Prepared {
    pub geometry: Geometry,
    pub surface: surface::T,
}

/// The prepared value is shared between the cache and the pieces. A surface keeps device handles in
/// `Rc`s, so it never leaves the initial domain: the worker cooks, the initial domain prepares (see
/// `Raw` and `finish`).
pub type PreparedRef = Rc<Prepared>;

#[derive(Clone)]
pub struct Piece {
    pub id: i64,
    pub graph: node::T,
    pub prepared: PreparedRef,
    /// read on the initial domain when asked (framing, the selected box): a device's P stays there
    /// until then
    pub bounds: Rc<OnceCell<Option<Bounds>>>,
    pub settings: settings::T,
    /// projection of the graph's declared context dependencies
    pub context: Vec<u8>,
    /// what `prepared` was made from, before the tint
    pub output: Output,
    /// the provenance tags `prepared` highlights, see `pick`
    pub lit: pick::Set,
    /// built by the first click
    pub surface: Rc<OnceCell<Option<gv::Surface>>>,
}

/// What a footer shows of a geometry value: its object and compiled node.
pub type Summary = (Key2, probe::Geometry);
pub type Source = (Key2, gv::T);

pub enum Cooked {
    Displayed(Vec<Piece>, Vec<Summary>, Vec<Source>),
    /// the footers' counts alone: the displayed objects are cache hits
    Probed(Vec<Piece>, Vec<Summary>, Vec<Source>),
    Framed(Option<Bounds>),
}

/// What the worker hands back (it is `Send`): the cooked outputs of the displayed graphs with the
/// footers' counts. The initial domain turns it into pieces (`finish`), because a prepared surface
/// holds device handles that stay there.
pub enum Raw {
    Cooked {
        probe_only: bool,
        graphs: Vec<(i64, node::T)>,
        outputs: Vec<Output>,
        settings: settings::T,
        lit: pick::Set,
        context: context::T,
        found: Vec<Summary>,
        data: Vec<Source>,
    },
    Framed(Option<Bounds>),
}

/// Compute a piece's bounds on first use.
pub fn piece_bounds(piece: &Piece) -> Option<Bounds> {
    *piece.bounds.get_or_init(|| output_bounds(&piece.output).ok().flatten())
}

/// Build a piece's ray index on first use.
pub fn piece_surface(piece: &Piece) -> Option<&gv::Surface> {
    piece
        .surface
        .get_or_init(|| sop::payload::geometry(&piece.output.payload).ok().and_then(|g| pick::surface(&g)))
        .as_ref()
}

const PREPARED_BYTE_CAPACITY: i64 = 64 * 1024 * 1024;
const PREPARED_ENTRIES: usize = 16;

pub type PrepareFn = Rc<dyn Fn(&settings::T, &Output) -> Result<Prepared, String>>;
pub type Clock = Arc<dyn Fn() -> f64 + Send + Sync>;

struct CacheEntry {
    id: i64,
    settings: settings::T,
    output: Output,
    context: Vec<u8>,
    value: PreparedRef,
    bytes: i64,
}

/// AsyncPrepared results use a 16-entry/64MiB MRU cache keyed by settings/output identity. ponytail: it remains an initial-thread list outside global Lru retreat; a surface cannot be shared between threads.
struct PreparedCache {
    entries: VecDeque<CacheEntry>,
    bytes: i64,
}

/// What `prepare_cached` needs.
#[derive(Clone)]
pub struct Preparer {
    cache: Rc<RefCell<PreparedCache>>,
    prepare: PrepareFn,
    clock: Clock,
}

#[derive(Clone)]
pub struct Cook {
    pub worker: async_cook::T,
    pub seed: i64,
    pub grain: i64,
    pub domains: i64,
    /// block on each cook: fixed-step runs, see `create`
    pub await_: bool,
    pub schedule: schedule::Schedule,
    pub preparer: Preparer,
    pub pieces: Vec<Piece>,
    /// the latest desired submission settings
    pub settings: Option<settings::T>,
    pub error: Option<String>,
    /// the code and compiled node id of the node the last failed cook names (read while `error` is
    /// set)
    pub failure: Rc<RefCell<Option<(String, i64)>>>,
    pub seconds: Option<f64>,
    /// A framing job can supersede a display cook that must be resubmitted.
    pub framing: Option<bool>,
    pub force: bool,
    /// Each object's last compiled network, reused node-by-node on the next edit, and this frame's
    /// displayed graphs.
    pub compiled: BTreeMap<i64, (edit_graph::T, Arc<edit_graph::Compiled>)>,
    pub value_lanes: BTreeMap<i64, Arc<value_lane::Lane>>,
    pub state: flow_eval::State,
    pub applied: BTreeMap<i64, value_lane::Resolved>,
    pub graphs: Vec<(i64, node::T)>,
    /// the display node each graph compiles
    pub displayed: Vec<Key2>,
    /// the job in flight only counts footers: a display cook may supersede it
    pub probe_job: bool,
    /// the (object, node) pairs the last submission asked to summarise
    pub probing: Vec<Key2>,
    /// the last update wanted other pairs: a quiet moment will count them
    pub probes_owed: bool,
    /// updates since the last edit: footers are counted once an edit settles
    pub quiet: i64,
    pub summaries: Vec<Summary>,
    /// same bounded optional cook targets as summaries
    pub sources: Vec<Source>,
}

pub struct UpdateResult {
    pub cook: Cook,
    pub edit_error: Option<String>,
    pub prepared_changed: bool,
    pub framed: Option<Option<Bounds>>,
}

pub fn geometry_bounds(g: &Geometry) -> Option<Bounds> {
    gv::bounds(g)
}

/// The eight corners of a box under a transform: the one walk for every box taken to another space
/// (an instance's, the world's, the screen's).
pub fn iter_corners(matrix: &mat4::T, b: &Bounds, f: &mut dyn FnMut(vec3::T)) {
    let (lo, hi) = b;
    for corner in 0..8 {
        f(mat4::transform_point(
            matrix,
            vec3::create(
                if corner & 1 == 0 { lo.x } else { hi.x },
                if corner & 2 == 0 { lo.y } else { hi.y },
                if corner & 4 == 0 { lo.z } else { hi.z },
            ),
        ));
    }
}

pub fn union(a: &Bounds, b: &Bounds) -> Bounds {
    let ((a, bb), (c, d)) = (a, b);
    (
        vec3::create(a.x.min(c.x), a.y.min(c.y), a.z.min(c.z)),
        vec3::create(bb.x.max(d.x), bb.y.max(d.y), bb.z.max(d.z)),
    )
}

/// The box around a box under each of `matrices`.
pub fn transformed_all(matrices: &[mat4::T], b: &Bounds) -> Bounds {
    let mut lower = vec3::create(f64::INFINITY, f64::INFINITY, f64::INFINITY);
    let mut upper = vec3::create(f64::NEG_INFINITY, f64::NEG_INFINITY, f64::NEG_INFINITY);
    for matrix in matrices {
        iter_corners(matrix, b, &mut |p| {
            lower = vec3::create(lower.x.min(p.x), lower.y.min(p.y), lower.z.min(p.z));
            upper = vec3::create(upper.x.max(p.x), upper.y.max(p.y), upper.z.max(p.z));
        });
    }
    (lower, upper)
}

pub fn transformed(matrix: &mat4::T, b: &Bounds) -> Bounds {
    transformed_all(std::slice::from_ref(matrix), b)
}

/// A packed output's bounds: the prototype's box at every instance.
pub fn output_bounds(output: &Output) -> Result<Option<Bounds>, rays_diagnostic::Diagnostic> {
    let geometry = sop::payload::geometry(&output.payload)?;
    Ok(match (geometry_bounds(&geometry), &output.instances) {
        (None, _) => None,
        (Some(b), None) => Some(b),
        (Some(b), Some(t)) if t.is_empty() => Some(b),
        (Some(b), Some(transforms)) => Some(transformed_all(transforms, &b)),
    })
}

fn unix_now() -> f64 {
    std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map(|d| d.as_secs_f64()).unwrap_or(0.0)
}

/// `await_`: a fixed-step run (export, a finite --max-frames run, tests) must show the geometry of
/// exactly frame n, so `update` waits for the cook it submitted; interactive play never blocks
/// (latest request wins). The default is false; `Frame.t` does not say the clock is fixed, so an
/// export host passes `await_: Some(true)`.
pub fn create(
    prepare: PrepareFn,
    seed: i64,
    grain: i64,
    domains: Option<i64>,
    await_: Option<bool>,
    clock: Option<Clock>,
    max_entries: i64,
    max_payload_bytes: i64,
) -> Result<Cook, String> {
    let await_ = await_.unwrap_or(false);
    if grain <= 0 {
        return Err("Rays_editor: grain must be positive".to_string());
    }
    let domains = domains.unwrap_or_else(|| (parallel::recommended_domains() - 1).max(1));
    if domains <= 0 {
        return Err("Rays_editor: domains must be positive".to_string());
    }
    let worker = async_cook::create(max_entries, max_payload_bytes)?;
    if let Some(clock) = &clock {
        async_cook::set_clock(&worker, clock.clone());
    }
    let clock: Clock = clock.unwrap_or_else(|| Arc::new(unix_now));
    Ok(Cook {
        worker,
        seed,
        grain,
        domains,
        await_,
        schedule: schedule::initial(),
        preparer: Preparer { cache: Rc::new(RefCell::new(PreparedCache { entries: VecDeque::new(), bytes: 0 })), prepare, clock },
        pieces: Vec::new(),
        settings: None,
        error: None,
        failure: Rc::new(RefCell::new(None)),
        seconds: None,
        framing: None,
        force: false,
        compiled: BTreeMap::new(),
        value_lanes: BTreeMap::new(),
        state: flow_eval::create_state(),
        applied: BTreeMap::new(),
        graphs: Vec::new(),
        displayed: Vec::new(),
        probe_job: false,
        probing: Vec::new(),
        probes_owed: false,
        quiet: 0,
        summaries: Vec::new(),
        sources: Vec::new(),
    })
}

/// The failing node of a cook error: the innermost entry of its trace.
fn failure_of(error: &async_cook::Error) -> Option<(String, i64)> {
    match error {
        async_cook::Error::CookError(d) if !d.trace.is_empty() => Some((d.code.clone(), d.trace[d.trace.len() - 1].node_id)),
        _ => None,
    }
}

fn set_failure(value: &Cook, failure: Option<(String, i64)>) {
    *value.failure.borrow_mut() = failure;
}

fn failure(value: &Cook) -> Option<(String, i64)> {
    value.failure.borrow().clone()
}

/// The last cook was refused for memory: worth another try once the process is under its limit.
pub fn out_of_memory(value: &Cook) -> bool {
    value.error.is_some() && matches!(failure(value), Some((code, _)) if code == "E_MEMORY")
}

pub fn failed_node(value: &Cook) -> Option<(String, i64)> {
    if value.error.is_none() { None } else { failure(value) }
}

pub fn status(value: &Cook) -> async_cook::Status {
    async_cook::status(&value.worker)
}

/// `Lower.is_volatile` of the current lowering: the nodes that recook per frame.
pub fn set_volatile(value: &Cook, predicate: Arc<dyn Fn(i64) -> bool + Send + Sync>) {
    async_cook::set_volatile(&value.worker, predicate)
}

pub fn stats(value: &Cook) -> sop::session::Stats {
    async_cook::stats(&value.worker)
}

pub fn pieces(value: &Cook) -> &[Piece] {
    &value.pieces
}

fn same_key(entry: &CacheEntry, id: i64, settings: &settings::T, output: &Output, context: &[u8]) -> bool {
    entry.id == id && settings::same_values(&entry.settings, settings) && sop::session::same_output(&entry.output, output) && entry.context == context
}

/// `Gpu` (when the editor draws on a device) is fed the time preparing took, the measured display
/// sink the mesh placement prices, per route of the geometry.
pub fn prepare_cached(
    gpu: Option<&gpu::Config>,
    p: &Preparer,
    id: i64,
    context: &[u8],
    settings: &settings::T,
    output: &Output,
) -> Result<PreparedRef, String> {
    {
        let mut cache = p.cache.borrow_mut();
        if let Some(at) = cache.entries.iter().position(|e| same_key(e, id, settings, output, context)) {
            let entry = cache.entries.remove(at).expect("present cache entry");
            let value = entry.value.clone();
            cache.entries.push_front(entry);
            return Ok(value);
        }
    }
    let began = (p.clock)();
    let made = (p.prepare)(settings, output);
    if let (Some(gpu), Ok(made)) = (gpu, &made) {
        flow_sop::lower::observe_display(gpu, &made.geometry, (p.clock)() - began);
    }
    let made = made?;
    // Geometry's payload is metadata-only, including for a resident carrier. Budget both that input
    // and the derived display arrays conservatively.
    let source = sop::payload::geometry(&output.payload).map(|g| rdk_core::geometry::payload_bytes(&g)).unwrap_or(0);
    let bytes = rdk_core::geometry::payload_bytes(&made.geometry);
    // preparing a device's output is a few records, and cached it holds the frame's buffers, bytes
    // this budget never sees: a live cook's old frames, asked for never again
    let cacheable = flow_sop::attribute_kernel::resident_vec3(&made.geometry, "P").is_none()
        && source <= PREPARED_BYTE_CAPACITY
        && bytes <= (PREPARED_BYTE_CAPACITY - source) / 2;
    let prepared: PreparedRef = Rc::new(made);
    if cacheable {
        let bytes = source + bytes * 2;
        let instances = output.instances.as_ref().map_or(0, |a| a.len() as i64);
        if instances <= (PREPARED_BYTE_CAPACITY - bytes) / 128 {
            let size = bytes + instances * 128;
            let mut cache = p.cache.borrow_mut();
            if let Some(at) = cache.entries.iter().position(|e| same_key(e, id, settings, output, context)) {
                let old = cache.entries.remove(at).expect("present cache entry");
                cache.bytes -= old.bytes;
            }
            cache.entries.push_front(CacheEntry {
                id,
                settings: settings.clone(),
                output: output.clone(),
                context: context.to_vec(),
                value: prepared.clone(),
                bytes: size,
            });
            cache.bytes += size;
            while cache.entries.len() > PREPARED_ENTRIES || cache.bytes > PREPARED_BYTE_CAPACITY {
                match cache.entries.pop_back() {
                    Some(old) => cache.bytes -= old.bytes,
                    None => break,
                }
            }
        }
    }
    Ok(prepared)
}

/// The counts of a compiled node of an object, once a cook has reported them.
pub fn geometry(value: &Cook, object_id: i64, node_id: i64) -> Option<&probe::Geometry> {
    value.summaries.iter().find(|(k, _)| *k == (object_id, node_id)).map(|(_, g)| g)
}

pub fn source(value: &Cook, object_id: i64, node_id: i64) -> Option<&gv::T> {
    value.sources.iter().find(|(k, _)| *k == (object_id, node_id)).map(|(_, g)| g)
}

pub fn applied(value: &Cook, id: i64) -> Option<&value_lane::Resolved> {
    value.applied.get(&id)
}

pub fn reset_state(host_state: bool, value: &Cook) {
    flow_eval::reset_state(&value.state, host_state);
    for lane in value.value_lanes.values() {
        value_lane::reset(host_state, lane);
    }
}

pub fn context(value: &Cook, timeline: &timeline::Timeline, frame: &frame::Frame) -> Result<context::T, String> {
    context::create(context::Options {
        seed: Some(value.seed),
        grain: Some(value.grain),
        domains: Some(value.domains),
        frame: Some(timeline::frame(timeline)),
        time: Some(timeline::time(timeline)),
        input: Some(flow_native::live_frame::of_frame(frame)),
        ..context::Options::default()
    })
}

pub fn busy(value: &Cook) -> bool {
    !matches!(status(value), async_cook::Status::Idle)
}

pub fn force(value: &Cook) -> Cook {
    Cook { force: true, ..value.clone() }
}

/// no cook running or owed, and no footers waiting for their quiet updates (`update`'s `settled`)
pub fn at_rest(value: &Cook) -> bool {
    !busy(value) && !value.schedule.dirty && !value.probes_owed
}

pub fn dependencies(value: &Cook) -> deps::T {
    value.schedule.dependencies
}

/// Preparation, probes and framing are CPU-capable readers of published geometry. Budget their
/// shared host materialisation even when a supported display borrows it. A displayed root a device
/// owner can borrow reads the host in no cook (preparation, bounds and drawing keep it resident;
/// picking and inspection materialise on their gesture), so unless probed it has none.
pub fn prepared_context(readers: i64, context: context::T, nodes: &[node::T]) -> context::T {
    nodes.iter().fold(context, |context, n| context::with_cpu_consumers(&context, node::id(n), Some(readers)))
}


/// Prepare visible geometry tuples and optional frame targets; live/input control the run and pump services owner work while fixed-step execution waits.
#[allow(clippy::too_many_arguments)]
pub fn update(
    gpu: Option<&gpu::Config>,
    input: Option<input::Frame>,
    live: bool,
    probes: &[Key2],
    lit: &pick::Set,
    pump: Option<&dyn Fn()>,
    value: &Cook,
    settings: &settings::T,
    objects: &[(i64, &Network, i64)],
    edit_error: Option<String>,
    effects: Effects,
    timeline_changes: &[timeline::Change],
    timeline: &timeline::Timeline,
    frame: &frame::Frame,
    frame_request: Option<Key2>,
) -> UpdateResult {
    let _phase = flow_syntax::phase_timer::measure("cook");
    if timeline_changes.iter().any(|c| matches!(c, timeline::Change::ResetNow | timeline::Change::StoppedNow)) {
        reset_state(false, value);
    }
    let mut input = input.unwrap_or_else(|| flow_native::live_frame::of_frame(frame));
    {
        let frame_index = timeline::frame(timeline);
        // a stopped clock is one frame, however many host frames pass: no step and no host count, or
        // every idle frame would differ and cook again
        let still = timeline::mode(timeline) != timeline::Mode::Playing;
        input.t = timeline::time(timeline);
        input.frame = frame_index;
        input.dt = if still { 0.0 } else { input.dt };
        input.tick = if still { frame_index } else { input.tick };
    }
    let mut resolved_objects: Vec<(i64, edit_graph::T, i64)> = Vec::new();
    let mut value_lanes: BTreeMap<i64, Arc<value_lane::Lane>> = BTreeMap::new();
    let mut applied: BTreeMap<i64, value_lane::Resolved> = BTreeMap::new();
    let mut resolve_error: Option<String> = None;
    for (id, network, displayed) in objects {
        let lane = match value.value_lanes.get(id) {
            Some(lane) => lane.clone(),
            None => Arc::new(value_lane::create(Some(value.state.clone()))),
        };
        value_lanes.insert(*id, lane.clone());
        match value_lane::resolve(gpu, Some(&input), &lane, timeline::time(timeline), network) {
            Ok(resolved) => {
                resolved_objects.push((*id, resolved.geometry.clone(), *displayed));
                applied.insert(*id, resolved);
            }
            Err(diagnostic) => {
                if let Some(previous) = value.applied.get(id) {
                    resolved_objects.push((*id, previous.geometry.clone(), *displayed));
                    applied.insert(*id, previous.clone());
                }
                resolve_error = Some(rays_diagnostic::to_string(&diagnostic));
            }
        }
    }
    let objects = resolved_objects;
    let edit_error = if edit_error.is_some() { edit_error } else { resolve_error };
    let mut compiled = value.compiled.clone();
    for (id, document, _) in &objects {
        match compiled.get(id) {
            Some((source, _)) if edit_graph::ptr_eq(source, document) => {}
            previous => {
                let built = edit_graph::compile_all(previous.map(|(_, c)| c.as_ref()), document);
                compiled.insert(*id, (document.clone(), Arc::new(built)));
            }
        }
    }
    compiled.retain(|id, _| objects.iter().any(|(object_id, _, _)| object_id == id));
    // An unchanged network and display node keep their graph physically, so an idle frame never looks
    // like an edit and never resubmits a cook.
    let mut graphs: Vec<(i64, node::T)> = Vec::new();
    let mut edit_error = edit_error;
    for (id, document, displayed) in &objects {
        let unchanged = matches!(value.compiled.get(id), Some((source, _)) if edit_graph::ptr_eq(source, document));
        let previous_graph = value.graphs.iter().find(|(g, _)| g == id).map(|(_, n)| n.clone());
        let previous_displayed = value.displayed.iter().find(|(g, _)| g == id).map(|(_, d)| *d);
        match (&previous_graph, previous_displayed) {
            (Some(graph), Some(previous)) if unchanged && previous == *displayed => graphs.push((*id, graph.clone())),
            _ => match edit_graph::compiled_node(&compiled[id].1, *displayed) {
                Ok(graph) => graphs.push((*id, graph)),
                Err(message) => {
                    if let Some(graph) = previous_graph {
                        graphs.push((*id, graph));
                    }
                    edit_error = Some(message);
                }
            },
        }
    }
    let displayed: Vec<Key2> = objects.iter().map(|(id, _, displayed)| (*id, *displayed)).collect();
    let changed = !(graphs.len() == value.graphs.len()
        && graphs.iter().zip(value.graphs.iter()).all(|((a, g), (b, h))| a == b && node::ptr_eq(g, h)));
    let completion: Option<Completion> = async_cook::poll(&value.worker);
    let completed = completion.is_some();
    let resume = value.framing == Some(true);
    let mut summaries = value.summaries.clone();
    let mut sources = value.sources.clone();
    let (mut pieces, mut error, mut seconds, mut prepared_changed, mut framed, mut framing, force_next): (
        Vec<Piece>,
        Option<String>,
        Option<f64>,
        bool,
        Option<Option<Bounds>>,
        Option<bool>,
        bool,
    ) = match completion {
        None => (value.pieces.clone(), value.error.clone(), value.seconds, false, None, value.framing, false),
        Some(Completion { result: Ok(prepared), seconds: took, .. }) => match complete(gpu, &value.preparer, &value.pieces, prepared) {
            Ok(Cooked::Displayed(pieces, found, data)) => {
                // a display cook that counted nothing keeps the counts it has
                if !found.is_empty() {
                    summaries = found;
                    sources = data;
                }
                (pieces, None, Some(took), true, None, None, false)
            }
            Ok(Cooked::Probed(pieces, found, data)) => {
                summaries = found;
                sources = data;
                (pieces, value.error.clone(), value.seconds, true, None, None, false)
            }
            Ok(Cooked::Framed(b)) => (value.pieces.clone(), value.error.clone(), value.seconds, false, Some(b), None, resume),
            Err(message) => {
                set_failure(value, None);
                (value.pieces.clone(), Some(message), Some(took), false, None, None, false)
            }
        },
        Some(Completion { result: Err(_), .. }) if value.framing.is_some() => {
            (value.pieces.clone(), value.error.clone(), value.seconds, false, Some(None), None, resume)
        }
        Some(Completion { result: Err(e), seconds: took, .. }) => {
            set_failure(value, failure_of(&e));
            (value.pieces.clone(), Some(async_cook::error_to_string(&e)), Some(took), false, None, None, false)
        }
    };
    // Empty/removed objects have no current preview, including when a job from before the deletion
    // completes. Invalid nonempty SOPs keep their last successful graph and preview through the
    // compilation fallback.
    if (changed || completed) && pieces.iter().any(|p| !compiled.contains_key(&p.id)) {
        pieces.retain(|p| compiled.contains_key(&p.id));
        prepared_changed = true;
    }
    let force_flag = changed
        || value.force
        || (value.await_ && probes != value.probing.as_slice())
        || !matches!(&value.settings, Some(s) if settings::same_values(s, settings));
    let (schedule, submit) = schedule::step(
        live,
        Some(&input),
        &value.schedule,
        &graphs.iter().map(|(_, g)| g.clone()).collect::<Vec<_>>(),
        effects,
        timeline::changed_context(timeline_changes),
        force_flag,
        (busy(value) && !value.probe_job) || framing.is_some(),
        frame,
    );
    // refused for memory: the last good frame stays. Time alone does not cook again, or each frame
    // rebuilds what the refusal dropped, on top of the garbage of the last try
    let submit = submit && (changed || value.force || !out_of_memory(value) || mem::retry_worthwhile());
    // Unchanged objects keep their prepared value: the worker only cooks (a session cache hit) and
    // re-prepares what changed.
    // the probed nodes are cooked after the displayed ones, so an upstream one is a cache hit (or a
    // volatile slot); one that is not upstream of the display is cooked too, and either may fail
    // without failing the display
    let node_of = |key: Key2| -> Option<node::T> {
        compiled
            .get(&key.0)
            .and_then(|(_, c)| edit_graph::compiled_node(c, key.1).ok())
            .or_else(|| graphs.iter().find(|(id, _)| *id == key.0).and_then(|(_, g)| node::find(g, key.1)))
    };
    // Footers are counted after the display cook, not with it: a changed graph is displayed first, and
    // the probe job follows when the worker is idle. A live recook carries the probes only when the
    // visible set changed.
    let split = !value.await_;
    // Footers wait for the edit to settle: a probed node is cooked on its own (a chain of GPU kernels
    // is not a cache hit for its intermediate nodes), so during a scrub every step would pay for it
    // again. A scrub's steps carry no probes; the first quiet moment counts them once.
    let quiet = if changed { 0 } else { value.quiet + 1 };
    let settled = quiet >= 8;
    let new_probes = probes != value.probing.as_slice();
    let inline = submit && (!split || (!changed && settled && new_probes));
    let probe_only = split && settled && !submit && !probes.is_empty() && new_probes && framing.is_none() && (!busy(value) || value.probe_job);
    let probing: Vec<Key2> = if inline || probe_only {
        probes.to_vec()
    } else if submit && (changed || value.probe_job) {
        Vec::new()
    } else {
        value.probing.clone()
    };
    let probed: Vec<(Key2, node::T)> = if !(inline || probe_only) {
        Vec::new()
    } else {
        probes.iter().take(64).filter_map(|key| node_of(*key).map(|n| (*key, n))).collect()
    };
    let (error_now, framing_now) = if submit || probe_only {
        let result = context(value, timeline, frame).and_then(|ctx| {
            let device = gpu.is_some_and(|g| g.backend.is_some());
            let graph_nodes: Vec<node::T> = graphs.iter().map(|(_, g)| g.clone()).collect();
            let probe_nodes: Vec<node::T> = probed.iter().map(|(_, n)| n.clone()).collect();
            let ctx = prepared_context(if device { 0 } else { 1 }, ctx, &graph_nodes);
            let ctx = prepared_context(1, ctx, &probe_nodes);
            let job = Job {
                worker: value.worker.clone(),
                graphs: graphs.clone(),
                probed: probed.clone(),
                settings: settings.clone(),
                lit: lit.clone(),
                probe_only,
                context: ctx.clone(),
            };
            async_cook::submit_some(
                &value.worker,
                ctx,
                graph_nodes,
                probe_nodes,
                Box::new(move |outputs, optional| job.prepare(outputs, optional).map(|raw| Box::new(raw) as AsyncPrepared)),
            )
        });
        match result {
            Ok(_) => (None, None),
            Err(message) => (Some(message), None),
        }
    } else {
        (error.clone(), framing)
    };
    // the job a probe-only submission replaced shows no error of its own
    let (mut error_now, framing_now) = if probe_only { (value.error.clone(), value.framing) } else { (error_now, framing_now) };
    error = error_now.clone();
    framing = framing_now;
    // A fixed-step run waits for the cook it just submitted, serving the device meanwhile: a cook whose
    // regions run on the GPU queues them for this (the initial) domain, so a blocking await would
    // deadlock. `pump` serves and then sleeps: on a queued request (the editor's, a pipe) or at most
    // 0.5 ms, which is what bounds noticing the cook's own completion.
    if submit && error_now.is_none() && value.await_ {
        let awaited = loop {
            match async_cook::poll(&value.worker) {
                Some(c) => break c,
                None => match pump {
                    Some(pump) => pump(),
                    None => std::thread::sleep(std::time::Duration::from_micros(500)),
                },
            }
        };
        match awaited.result {
            Ok(prepared) => match complete(gpu, &value.preparer, &value.pieces, prepared) {
                Ok(Cooked::Displayed(awaited_pieces, found, data)) => {
                    summaries = found;
                    sources = data;
                    pieces = awaited_pieces.into_iter().filter(|p| compiled.contains_key(&p.id)).collect();
                    error = None;
                    seconds = Some(awaited.seconds);
                    prepared_changed = true;
                }
                Ok(Cooked::Framed(_) | Cooked::Probed(..)) => {}
                Err(message) => {
                    error = Some(message);
                    seconds = Some(awaited.seconds);
                }
            },
            Err(failure) => {
                set_failure(value, failure_of(&failure));
                error = Some(async_cook::error_to_string(&failure));
                seconds = Some(awaited.seconds);
            }
        }
        error_now = error.clone();
    }
    let _ = error_now;
    // a piece prepared under another highlight is prepared again from its kept output: no recook, no
    // lowering, and nothing when no tag is involved
    let mut retinted = false;
    let mut retinted_pieces: Vec<Piece> = Vec::new();
    for piece in pieces {
        if pick::same_set(&piece.lit, lit) {
            retinted_pieces.push(piece);
            continue;
        }
        match sop::payload::geometry(&piece.output.payload) {
            Err(_) => retinted_pieces.push(piece),
            Ok(geometry) if pick::tags(&geometry).is_none() => retinted_pieces.push(Piece::with_lit(piece, lit)),
            Ok(_) => {
                let made = pick::tint(&piece.output, lit)
                    .map_err(|d| rays_diagnostic::error_to_string(&d))
                    .and_then(|tinted| prepare_cached(None, &value.preparer, piece.id, &piece.context, &piece.settings, &tinted));
                match made {
                    Ok(prepared) => {
                        retinted = true;
                        retinted_pieces.push(Piece { prepared, lit: lit.clone(), ..piece });
                    }
                    Err(_) => retinted_pieces.push(piece),
                }
            }
        }
    }
    let pieces = retinted_pieces;
    let prepared_changed = prepared_changed || retinted;
    let (framed, framing) = match frame_request {
        None => (framed.take(), framing),
        Some((object_id, node_id)) => {
            let ctx = context(value, timeline, frame);
            let hit = match (pieces.iter().find(|p| p.id == object_id), graphs.iter().find(|(id, _)| *id == object_id)) {
                (Some(piece), Some((_, graph)))
                    if node::id(graph) == node_id
                        && node::ptr_eq(&piece.graph, graph)
                        && matches!(&ctx, Ok(c) if piece.context == context::cache_projection(node::graph_dependencies(graph), c)) =>
                {
                    Some(piece_bounds(piece))
                }
                _ => None,
            };
            match hit {
                Some(b) => (Some(b), framing),
                None => match compiled.get(&object_id).map(|(_, c)| edit_graph::compiled_node(c, node_id)) {
                    None | Some(Err(_)) => (Some(None), framing),
                    Some(Ok(n)) => {
                        let was_busy = busy(value) && framing.is_none();
                        let submitted = ctx.and_then(|c| {
                            let c = prepared_context(1, c, std::slice::from_ref(&n));
                            async_cook::submit(
                                &value.worker,
                                c,
                                n.clone(),
                                Box::new(|output| {
                                    output_bounds(&output)
                                        .map(|b| Box::new(Raw::Framed(b)) as AsyncPrepared)
                                        .map_err(|d| rays_diagnostic::error_to_string(&d))
                                }),
                            )
                        });
                        match submitted {
                            Ok(_) => (framed, Some(was_busy || framing.unwrap_or(false))),
                            Err(_) => (Some(None), framing),
                        }
                    }
                },
            }
        }
    };
    let cook = Cook {
        schedule,
        pieces,
        settings: Some(settings.clone()),
        error,
        seconds,
        framing,
        force: force_next,
        compiled,
        value_lanes,
        applied,
        graphs,
        displayed,
        probe_job: probe_only,
        quiet,
        probes_owed: probes != probing.as_slice(),
        probing,
        summaries,
        sources,
        ..value.clone()
    };
    UpdateResult { cook, edit_error, prepared_changed, framed }
}

impl Piece {
    fn with_lit(mut piece: Piece, lit: &pick::Set) -> Piece {
        piece.lit = lit.clone();
        piece
    }
}

/// Everything the worker's prepare step reads, owned so it can move to the worker thread. It makes
/// the footers' counts and hands the outputs back; the pieces are made by `finish` on the initial
/// domain.
struct Job {
    worker: async_cook::T,
    graphs: Vec<(i64, node::T)>,
    probed: Vec<(Key2, node::T)>,
    settings: settings::T,
    lit: pick::Set,
    probe_only: bool,
    context: context::T,
}

impl Job {
    fn summary(&self, key: Key2, n: &node::T, output: &Output) -> Option<Summary> {
        let g = sop::payload::geometry(&output.payload).ok()?;
        let v = gv::summary(&g);
        Some((
            key,
            probe::Geometry {
                seconds: async_cook::node_seconds(&self.worker, node::id(n)),
                points: v.points,
                prims: v.prims,
                extent: geometry_bounds(&g).map(|(lo, hi)| (hi.x - lo.x, hi.y - lo.y, hi.z - lo.z)),
                groups: v.groups,
                attributes: v.attributes,
            },
        ))
    }

    fn prepare(self, outputs: Vec<Output>, optional: Vec<Option<Output>>) -> Result<Raw, String> {
        let mut found: Vec<Summary> = Vec::new();
        let mut data: Vec<Source> = Vec::new();
        for ((key, n), output) in self.probed.iter().zip(optional.iter()) {
            let Some(output) = output else { continue };
            if let sop::payload::T::Geometry(_) = &output.payload {
                found.extend(self.summary(*key, n, output));
            }
        }
        for ((key, _), output) in self.probed.iter().zip(optional.iter()) {
            if let Some(output) = output {
                if let Ok(geometry) = sop::payload::geometry(&output.payload) {
                    data.push((*key, geometry));
                }
            }
        }
        if self.graphs.len() != outputs.len() {
            return Err("cook returned a different number of outputs".to_string());
        }
        Ok(Raw::Cooked {
            probe_only: self.probe_only,
            graphs: self.graphs,
            outputs,
            settings: self.settings,
            lit: self.lit,
            context: self.context,
            found,
            data,
        })
    }
}

/// The initial domain's half of a cook: a piece per displayed graph, reusing the previous pieces of
/// an unchanged graph, settings, context projection and highlight, else preparing the (tinted)
/// output.
fn finish(gpu: Option<&gpu::Config>, preparer: &Preparer, previous: &[Piece], raw: Raw) -> Result<Cooked, String> {
    let (probe_only, graphs, outputs, settings, lit, context, found, data) = match raw {
        Raw::Framed(b) => return Ok(Cooked::Framed(b)),
        Raw::Cooked { probe_only, graphs, outputs, settings, lit, context, found, data } => (probe_only, graphs, outputs, settings, lit, context, found, data),
    };
    let mut pieces = Vec::new();
    for ((id, graph), output) in graphs.iter().zip(outputs.into_iter()) {
        let projection = context::cache_projection(node::graph_dependencies(graph), &context);
        let reused = previous.iter().find(|piece| {
            piece.id == *id
                && node::ptr_eq(&piece.graph, graph)
                && settings::same_values(&piece.settings, &settings)
                && piece.context == projection
                && pick::same_set(&piece.lit, &lit)
        });
        if let Some(piece) = reused {
            pieces.push(piece.clone());
            continue;
        }
        let _geometry = sop::payload::geometry(&output.payload).map_err(|d| rays_diagnostic::error_to_string(&d))?;
        let tinted = pick::tint(&output, &lit).map_err(|d| rays_diagnostic::error_to_string(&d))?;
        let prepared = prepare_cached(gpu, preparer, *id, &projection, &settings, &tinted)?;
        pieces.push(Piece {
            id: *id,
            graph: graph.clone(),
            prepared,
            output,
            lit: lit.clone(),
            surface: Rc::new(OnceCell::new()),
            bounds: Rc::new(OnceCell::new()),
            settings: settings.clone(),
            context: projection,
        });
    }
    Ok(if probe_only { Cooked::Probed(pieces, found, data) } else { Cooked::Displayed(pieces, found, data) })
}

fn complete(gpu: Option<&gpu::Config>, preparer: &Preparer, previous: &[Piece], prepared: AsyncPrepared) -> Result<Cooked, String> {
    match prepared.downcast::<Raw>() {
        Ok(raw) => finish(gpu, preparer, previous, *raw),
        Err(_) => Err("cook returned an unexpected value".to_string()),
    }
}

pub fn close(value: &Cook) {
    async_cook::close(&value.worker);
    let mut cache = value.preparer.cache.borrow_mut();
    cache.entries.clear();
    cache.bytes = 0;
}

/// What a ray cast against a piece reports: the provenance tag, or the primitive's material path.
#[derive(Clone, Debug, PartialEq)]
pub enum Hit {
    Tag(i64),
    Material(Option<String>),
}

type Cast<'a> = &'a dyn Fn(Option<&gv::Surface>, &Geometry, vec3::T, vec3::T) -> Option<(f64, Hit)>;

/// The nearest displayed primitive under a ray in the piece's own space, as (t, hit) where the hit
/// is `origin + t * direction`: the parameter, not the local distance, so hits of pieces and
/// instances with any scale compare in the world's units. A piece drawn as instances casts the ray
/// into each instance's own frame (about 2 us each; per click, never per frame).
fn pick_by(cast_hit: Cast, piece: &Piece, origin: vec3::T, direction: vec3::T) -> Option<(f64, Hit)> {
    let cast = |origin: vec3::T, direction: vec3::T| -> Option<(f64, Hit)> {
        let geometry = match &piece.output.payload {
            sop::payload::T::Geometry(g) => g,
            sop::payload::T::Image(_) | sop::payload::T::Kernel(_) => return None,
        };
        cast_hit(piece_surface(piece), geometry, origin, direction).map(|(distance, found)| (distance / vec3::length(direction), found))
    };
    match &piece.output.instances {
        None => cast(origin, direction),
        Some(t) if t.is_empty() => cast(origin, direction),
        Some(transforms) => {
            let mut best: Option<(f64, Hit)> = None;
            for matrix in transforms.iter() {
                let Some(inverse) = mat4::inverse(matrix) else { continue };
                let hit = cast(mat4::transform_point(&inverse, origin), mat4::transform_direction(&inverse, direction));
                best = match (hit, best) {
                    (Some((t, _)), Some((nearer, found))) if t >= nearer => Some((nearer, found)),
                    (Some(hit), _) => Some(hit),
                    (None, best) => best,
                };
            }
            best
        }
    }
}

/// The nearest provenance tag under the ray, see `pick_by`.
pub fn pick(piece: &Piece, origin: vec3::T, direction: vec3::T) -> Option<(f64, i64)> {
    let cast: Cast = &|surface, geometry, origin, direction| pick::cast(surface, geometry, origin, direction).map(|(d, tag)| (d, Hit::Tag(tag)));
    match pick_by(cast, piece, origin, direction) {
        Some((t, Hit::Tag(tag))) => Some((t, tag)),
        _ => None,
    }
}

/// The `shop_materialpath` of the nearest primitive under the ray (Alt-click).
pub fn pick_material(piece: &Piece, origin: vec3::T, direction: vec3::T) -> Option<(f64, Option<String>)> {
    let cast: Cast = &|surface, geometry, origin, direction| pick::cast_material(surface, geometry, origin, direction).map(|(d, m)| (d, Hit::Material(m)));
    match pick_by(cast, piece, origin, direction) {
        Some((t, Hit::Material(path))) => Some((t, path)),
        _ => None,
    }
}

/// As the app prepares a cooked object (instances, pieces and materials), and draws it.
pub fn prepare(_: &settings::T, output: &Output) -> Result<Prepared, String> {
    let geometry = sop::payload::geometry(&output.payload).map_err(|d| rays_diagnostic::error_to_string(&d))?;
    let instances = output.instances.as_ref().map(|a| a.as_ref().clone());
    let surface = surface::of_geometry(instances, &geometry)?;
    Ok(Prepared { geometry, surface })
}

pub type Display = Rc<dyn Fn(&Geometry) -> Option<surface::DeviceBinding>>;

thread_local! {
    /// the editor's device positions binder (`Flow_native.Workspace_gpu.display`) while it draws
    static DISPLAY: RefCell<Option<Display>> = const { RefCell::new(None) };
}

pub fn with_display(device: Display, f: &mut dyn FnMut()) {
    let previous = DISPLAY.with(|d| d.replace(Some(device)));
    f();
    DISPLAY.with(|d| d.replace(previous));
}

pub fn draw(n: &node::T, p: &Prepared) -> Result<rays::scene3::Scene3, surface::ConversionError> {
    let explosion = rdk_rays::packed_pieces::explosion(node::operation(n), &node::parameter_fields(n));
    let device = DISPLAY.with(|d| d.borrow().clone());
    match &device {
        Some(device) => surface::scene3(explosion.as_ref(), Some(&|g: &Geometry| device(g)), &p.surface),
        None => surface::scene3(explosion.as_ref(), None, &p.surface),
    }
}
