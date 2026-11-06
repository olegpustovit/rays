//! Port of lib/flow_native/workspace_gpu.ml: the workspace's GPU service. One owner (the thread
//! that created it, OCaml's initial domain) holds the GPU lease and the `flow_gpu` host; cook
//! workers reach it through a proxy backend whose every call is queued here and served by `pump`.
//!
//! Rust shape notes. OGPU handles are `Rc`, so the owner's state lives in a thread-local registry
//! (as `flow_gpu::host` does) and the `Send + Sync` closures of a `Gpu::Backend` hold only an id.
//! A worker's call becomes a `Job` (a boxed `FnOnce` returning a boxed `Any`) that the owner runs
//! in `pump`. A resident scope the owner makes for a worker stays in a thread-local table; the
//! worker's proxy closures hold its id and route every call. `Domain.is_main_domain` is "the
//! thread that created this service". The wake byte on a socket pair is a `Condvar`.
use crate::drawing;
use crate::marks_scene;
use flow_gpu::host as h;
use flow_gpu::image_sink;
use flow_gpu::run;
use flow_gpu::small_lru::List;
use flow_ir::core_exec as c;
use flow_ir::flow_ir::Gpu as G;
use flow_ir::packed::host as phost;
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as v, Value};
use input::Frame;
use ogpu::backend as ob;
use ogpu::types::{BufferDescriptor, BufferUsage};
use renderer::renderer as x;
use rays_diagnostic::Diagnostic;
use std::any::Any;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, VecDeque};
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::ThreadId;
use std::time::{Duration, Instant};

type Packed = flow_ir::packed::T;

/// The workspace owner's immutable execution policy.
pub type policy = G::Config;

pub fn default_policy() -> policy {
    G::default()
}

static NEXT: AtomicI64 = AtomicI64::new(1);
static NEXT_SCOPE: AtomicI64 = AtomicI64::new(1);
/// Resident scopes whose proxies were dropped (any thread); the owner removes them.
static RELEASED_SCOPES: Mutex<Vec<i64>> = Mutex::new(Vec::new());

fn error(message: &str) -> Diagnostic {
    v::fail("E_GPU", message)
}

fn native<A>(result: Result<A, ogpu::error::Error>) -> Result<A, Diagnostic> {
    result.map_err(|e| error(&e.message))
}

// ---------------------------------------------------------------------------------------------
// The queue between cook workers and the owner.

type Work = Box<dyn FnOnce() -> Box<dyn Any + Send> + Send>;

struct Job {
    run: Mutex<Option<Work>>,
    slot: Mutex<Option<Result<Box<dyn Any + Send>, Diagnostic>>>,
    finished: AtomicBool,
    waits: bool,
}

struct Queue {
    jobs: VecDeque<Arc<Job>>,
    closed: bool,
}

struct Shared {
    domain: ThreadId,
    queue: Mutex<Queue>,
    /// signalled when a job finishes
    ready: Condvar,
    /// signalled when a job is queued: `wait` sleeps on it
    wake: Condvar,
}

fn is_owner(shared: &Shared) -> bool {
    std::thread::current().id() == shared.domain
}

/// Marks a job failed if serving it unwinds, so its worker never waits forever.
struct Finisher<'a> {
    shared: &'a Shared,
    job: &'a Arc<Job>,
}
impl Drop for Finisher<'_> {
    fn drop(&mut self) {
        let mut queue = self.shared.queue.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        if !self.job.finished.load(Ordering::SeqCst) {
            *self.job.slot.lock().unwrap() = Some(Err(error("The workspace GPU request failed.")));
            self.job.finished.store(true, Ordering::SeqCst);
        }
        let _ = &mut queue;
        self.shared.ready.notify_all();
    }
}

/// Runs `f` on the owner: directly when called there, else queued for `pump` and waited for.
fn route(shared: &Arc<Shared>, waits: bool, f: Work) -> Result<Box<dyn Any + Send>, Diagnostic> {
    if is_owner(shared) {
        return Ok(f());
    }
    let job = Arc::new(Job { run: Mutex::new(Some(f)), slot: Mutex::new(None), finished: AtomicBool::new(false), waits });
    let mut queue = shared.queue.lock().unwrap();
    if queue.closed {
        return Err(error("The workspace GPU service is closed."));
    }
    queue.jobs.push_back(job.clone());
    shared.wake.notify_all();
    while !job.finished.load(Ordering::SeqCst) {
        queue = shared.ready.wait(queue).unwrap();
    }
    drop(queue);
    let result = job.slot.lock().unwrap().take();
    result.unwrap_or_else(|| Err(error("The workspace GPU request was cancelled.")))
}

fn serve(shared: &Shared, job: &Arc<Job>) {
    let finisher = Finisher { shared, job };
    let run = job.run.lock().unwrap().take();
    let result = run.map(|run| run());
    {
        let queue = shared.queue.lock().unwrap();
        *job.slot.lock().unwrap() = Some(match result {
            Some(r) => Ok(r),
            None => Err(error("The workspace GPU request was cancelled.")),
        });
        job.finished.store(true, Ordering::SeqCst);
        shared.ready.notify_all();
        drop(queue);
    }
    drop(finisher);
}

// Typed routes: one concrete function per result shape a proxy needs.

fn route_value_opt(s: &Arc<Shared>, waits: bool, f: Box<dyn FnOnce() -> Result<Option<Value>, Diagnostic> + Send>) -> Result<Option<Value>, Diagnostic> {
    route(s, waits, Box::new(move || Box::new(f()) as Box<dyn Any + Send>))
        .and_then(|b| *b.downcast::<Result<Option<Value>, Diagnostic>>().expect("route type"))
}
fn route_value(s: &Arc<Shared>, f: Box<dyn FnOnce() -> Result<Value, Diagnostic> + Send>) -> Result<Value, Diagnostic> {
    route(s, false, Box::new(move || Box::new(f()) as Box<dyn Any + Send>)).and_then(|b| *b.downcast::<Result<Value, Diagnostic>>().expect("route type"))
}
fn route_count(s: &Arc<Shared>, f: Box<dyn FnOnce() -> Result<Option<i64>, Diagnostic> + Send>) -> Result<Option<i64>, Diagnostic> {
    route(s, false, Box::new(move || Box::new(f()) as Box<dyn Any + Send>)).and_then(|b| *b.downcast::<Result<Option<i64>, Diagnostic>>().expect("route type"))
}
fn route_unit_result(s: &Arc<Shared>, waits: bool, f: Box<dyn FnOnce() -> Result<(), Diagnostic> + Send>) -> Result<(), Diagnostic> {
    route(s, waits, Box::new(move || Box::new(f()) as Box<dyn Any + Send>)).and_then(|b| *b.downcast::<Result<(), Diagnostic>>().expect("route type"))
}
fn route_unit(s: &Arc<Shared>, waits: bool, f: Box<dyn FnOnce() + Send>) -> Result<(), Diagnostic> {
    route(s, waits, Box::new(move || {
        f();
        Box::new(()) as Box<dyn Any + Send>
    }))
    .map(|_| ())
}
fn route_i64_opt(s: &Arc<Shared>, f: Box<dyn FnOnce() -> Option<i64> + Send>) -> Result<Option<i64>, Diagnostic> {
    route(s, false, Box::new(move || Box::new(f()) as Box<dyn Any + Send>)).map(|b| *b.downcast::<Option<i64>>().expect("route type"))
}
fn route_floats_opt(s: &Arc<Shared>, f: Box<dyn FnOnce() -> Option<Vec<f64>> + Send>) -> Result<Option<Vec<f64>>, Diagnostic> {
    route(s, false, Box::new(move || Box::new(f()) as Box<dyn Any + Send>)).map(|b| *b.downcast::<Option<Vec<f64>>>().expect("route type"))
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ImageStats {
    pub created: i64,
    pub closed: i64,
    pub buffers: i64,
    pub textures: i64,
}

fn close_image(stats: &RefCell<ImageStats>, sink: image_sink::T) {
    {
        let mut s = stats.borrow_mut();
        s.closed += 1;
        s.buffers += image_sink::buffer_creations(&sink);
        s.textures += image_sink::texture_creations(&sink);
    }
    image_sink::close(&sink);
}

/// What a display's mesh keeps alive: dropping it queues the display's key for release.
struct DropToken {
    key: String,
    dropped: Arc<Mutex<Vec<String>>>,
}
impl Drop for DropToken {
    fn drop(&mut self) {
        self.dropped.lock().unwrap().push(self.key.clone());
    }
}

type Shown = (String, bool, Option<i64>, i64, Weak<DropToken>);

pub struct Data {
    id: i64,
    shared: Arc<Shared>,
    clock: Arc<dyn Fn() -> f64 + Send + Sync>,
    owner: RefCell<Option<(x::gpu, h::T)>>,
    mark_sets: RefCell<List<i64, x::gpu_marks>>,
    images: RefCell<List<String, image_sink::T>>,
    image_stats: Rc<RefCell<ImageStats>>,
    closed_host: RefCell<h::Stats>,
    backend: RefCell<Option<Arc<G::Backend>>>,
    proxy: RefCell<Option<Arc<G::Backend>>>,
    closed: Rc<Cell<bool>>,
    policy: Cell<G::Policy>,
    measurement_ready: Cell<bool>,
    /// the device's measured readback, once taken
    readback: RefCell<Option<Option<f64>>>,
    measured: RefCell<List<(String, i64, Vec<i64>), Option<f64>>>,
    entries: RefCell<List<(usize, bool), Option<String>>>,
    /// the keys of displays whose meshes were collected (any thread)
    dropped: Arc<Mutex<Vec<String>>>,
    displays: Cell<i64>,
    display_refusal: RefCell<Option<String>>,
    /// each live display's geometry id and release, by its key
    bound: RefCell<HashMap<String, (i64, Rc<dyn Fn()>)>>,
    /// a live display by its geometry's data id
    shown: RefCell<HashMap<i64, Shown>>,
    frames: Cell<i64>,
    /// dropped displays, released at a later `frame`
    releasing: RefCell<Vec<(i64, Rc<dyn Fn()>)>>,
}

#[derive(Clone)]
pub struct t(pub Rc<Data>);

thread_local! {
    static SERVICES: RefCell<HashMap<i64, Rc<Data>>> = RefCell::new(HashMap::new());
    static SCOPES: RefCell<HashMap<i64, Rc<G::Residency>>> = RefCell::new(HashMap::new());
}

fn service(id: i64) -> Option<Rc<Data>> {
    SERVICES.with(|s| s.borrow().get(&id).cloned())
}

pub fn create(clock: Option<Arc<dyn Fn() -> f64 + Send + Sync>>) -> t {
    let id = NEXT.fetch_add(1, Ordering::SeqCst);
    let image_stats = Rc::new(RefCell::new(ImageStats::default()));
    let stats = image_stats.clone();
    let data = Rc::new(Data {
        id,
        shared: Arc::new(Shared { domain: std::thread::current().id(), queue: Mutex::new(Queue { jobs: VecDeque::new(), closed: false }), ready: Condvar::new(), wake: Condvar::new() }),
        clock: clock.unwrap_or_else(|| Arc::new(rays_math::clock::now)),
        owner: RefCell::new(None),
        // plans, not caches: what the frame draws
        mark_sets: RefCell::new(List::new(64, Box::new(|_, sink: x::gpu_marks| x::close_gpu_marks(&sink)))),
        images: RefCell::new(List::new(64, Box::new(move |_, sink: image_sink::T| close_image(&stats, sink)))),
        image_stats,
        closed_host: RefCell::new(h::zero()),
        backend: RefCell::new(None),
        proxy: RefCell::new(None),
        closed: Rc::new(Cell::new(false)),
        policy: Cell::new(G::Policy::Measured),
        measurement_ready: Cell::new(false),
        readback: RefCell::new(None),
        // count only: a float per kernel, and a measurement is not free to take again
        measured: RefCell::new(List::new(64, Box::new(|_, _| {}))),
        // count only: compiled kernels, small and slow to make again
        entries: RefCell::new(List::new(64, Box::new(|_, _| {}))),
        dropped: Arc::new(Mutex::new(Vec::new())),
        displays: Cell::new(0),
        display_refusal: RefCell::new(None),
        bound: RefCell::new(HashMap::new()),
        shown: RefCell::new(HashMap::new()),
        frames: Cell::new(0),
        releasing: RefCell::new(Vec::new()),
    });
    SERVICES.with(|s| s.borrow_mut().insert(id, data.clone()));
    t(data)
}

fn on_owner(d: &Data) -> bool {
    is_owner(&d.shared)
}

pub fn qualification(t_: &t) {
    let d = &*t_.0;
    if d.closed.get() || !on_owner(d) {
        panic!("Workspace_gpu.qualification: open initial-domain owner required");
    }
    d.policy.set(G::Policy::Qualification);
}

fn owner(d: &Data) -> Result<(x::gpu, h::T), Diagnostic> {
    if d.closed.get() || !on_owner(d) {
        return Err(error("Workspace GPU resources require their open initial-domain owner."));
    }
    if let Some((gpu, host)) = &*d.owner.borrow() {
        return Ok((gpu.clone(), host.clone()));
    }
    let gpu = native(x::acquire_gpu())?;
    let clock = d.clock.clone();
    let host = h::create(&*clock, &gpu);
    *d.owner.borrow_mut() = Some((gpu.clone(), host.clone()));
    Ok((gpu, host))
}

fn median3(mut runs: Vec<f64>) -> f64 {
    runs.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    runs[1]
}

/// Measure packing, upload and synchronized completion: median of three after a warm-up. Prepared
/// inputs retain their exact count, tables, frame and resident bindings; CPU arrays get fresh
/// identities to charge upload. Shape-only probes use zeroed inputs and power-of-two counts. The
/// 64-entry cache distinguishes both modes and binding layouts. A failed prepared frame is not
/// cached.
fn measure(d: &Data, prepared: Option<&phost_inputs>, fast: bool, packed: &Packed, count: i64) -> Option<f64> {
    let count = if prepared.is_some() {
        count
    } else {
        let mut p = 1;
        while p < count {
            p *= 2;
        }
        p
    };
    if !d.measurement_ready.get() || count < 0 || prepared.is_some_and(|i| i.count != count) {
        return None;
    }
    // a site's kernel entry, by its code: emitting it again (and digesting it) is per-draw work
    let code = Arc::as_ptr(&packed.code) as usize;
    let cached = d.entries.borrow().peek(&|k, _| *k == (code, fast)).cloned();
    let entry = match cached {
        Some(entry) => entry,
        None => {
            let options = flow_shader::emit::Options { spirv: flow_gpu::pipelines::is_vulkan(), fast, ..Default::default() };
            let entry = flow_shader::emit::kernel(&options, packed).ok().map(|k| k.entry);
            d.entries.borrow_mut().add((code, fast), entry.clone());
            entry
        }
    };
    let entry = entry?;
    let view = phost::view(packed);
    let indices = phost::index_sources(packed);
    let inputs: phost_inputs = match prepared {
        Some(inputs) => inputs.clone(),
        None => phost_inputs {
            arrays: view.widths.iter().map(|_| Arc::new(Vec::new())).collect(),
            uniforms: view.uniform_widths.iter().map(|w| vec![0.; *w]).collect(),
            tables: view.uniform_widths.iter().map(|_| Arc::new(vec![0.; 4])).collect(),
            frame: vec![0.; view.code.len()],
            count,
        },
    };
    let binding = |values: &Arc<Vec<f64>>| -> Option<i64> {
        if values.len() > 4 {
            return None;
        }
        run::pin(values).and_then(|(_, count, valid, unpin)| {
            let live = valid();
            unpin();
            if live { Some(count) } else { None }
        })
    };
    let sources: Vec<Option<i64>> = inputs.arrays.iter().map(&binding).collect();
    let tables: Vec<Option<i64>> = inputs.tables.iter().map(&binding).collect();
    let size = |held: Option<i64>, values: &Arc<Vec<f64>>| -> i64 {
        match held {
            None => values.len() as i64,
            Some(count) => -count - 2,
        }
    };
    let mut layout: Vec<i64> = vec![if prepared.is_some() { 1 } else { 0 }];
    for (i, values) in inputs.arrays.iter().enumerate() {
        layout.push(if indices[i] {
            -1
        } else if prepared.is_none() {
            count * view.widths[i]
        } else {
            size(sources[i], values)
        });
    }
    for (i, values) in inputs.tables.iter().enumerate() {
        layout.push(size(tables[i], values));
    }
    let key = (entry, count, layout);
    let seen = d.measured.borrow().peek(&|k, _| *k == key).cloned();
    if let Some(seconds) = seen {
        return seconds;
    }
    let inputs = match prepared {
        Some(_) => inputs,
        None => phost_inputs {
            arrays: view.widths.iter().enumerate().map(|(i, width)| Arc::new(if indices[i] { Vec::new() } else { vec![0.; (count * width) as usize] })).collect(),
            ..inputs
        },
    };
    let seconds = (|| -> Option<f64> {
        let (_, host) = owner(d).ok()?;
        let backend = h::backend(&host);
        let kernel = (backend.prepare)(fast, packed).ok()?;
        let producer: RefCell<Option<i64>> = RefCell::new(None);
        let once = || -> Option<f64> {
            // Every sample pays its transfer: each runs on a new runner, whose upload cache is
            // empty.
            if let Some(p) = *producer.borrow() {
                h::forget_producer(&host, p);
            }
            let start = (d.clock)();
            let output = (kernel.run)(&inputs).ok()?;
            *producer.borrow_mut() = Some(output.identity);
            let planes = (kernel.planes)(&output).ok()?;
            let settled = (planes.settle)();
            (planes.release)(false);
            settled.ok()?;
            Some((d.clock)() - start)
        };
        // a measurement's runner would keep its inputs' copies and upload buffers
        let result = (|| {
            once()?;
            let runs: Vec<f64> = (0..3).filter_map(|_| once()).collect();
            if runs.len() < 3 { None } else { Some(median3(runs)) }
        })();
        if let Some(p) = *producer.borrow() {
            h::forget_producer(&host, p);
        }
        result
    })();
    // Actual frame/data errors need not hold on the next invocation.
    if prepared.is_none() || seconds.is_some() {
        d.measured.borrow_mut().add(key, seconds);
    }
    seconds
}

#[allow(non_camel_case_types)]
type phost_inputs = flow_ir::packed::host::Inputs;

fn backend(t_: &t) -> Arc<G::Backend> {
    let d = &*t_.0;
    if let Some(b) = &*d.backend.borrow() {
        return b.clone();
    }
    let id = d.id;
    let unavailable = || error("Workspace GPU resources require their open initial-domain owner.");
    let backend = Arc::new(G::Backend {
        residency: Box::new(move |fast| {
            if !fast {
                return None;
            }
            let d = service(id)?;
            match owner(&d) {
                Ok((_, host)) => (h::backend(&host).residency)(fast),
                Err(_) => None,
            }
        }),
        cost: Box::new(move |inputs, fast, packed, count| {
            let d = service(id)?;
            measure(&d, inputs, fast, packed, count)
        }),
        prepare: Box::new(move |fast, packed| {
            let d = service(id).ok_or_else(unavailable)?;
            let (_, host) = owner(&d)?;
            (h::backend(&host).prepare)(fast, packed)
        }),
    });
    *d.backend.borrow_mut() = Some(backend.clone());
    backend
}

/// Replaces the real backend (and drops the proxy built over the old one).
pub fn install_test_backend(t_: &t, backend_: G::Backend) {
    let d = &*t_.0;
    if !on_owner(d) || d.closed.get() {
        panic!("Workspace_gpu.Private.install_test_backend: open initial-domain owner required");
    }
    *d.backend.borrow_mut() = Some(Arc::new(backend_));
    *d.proxy.borrow_mut() = None;
}

/// Runs `f` on the owner and waits for it (`waits`: the request blocks on the device, as a settle
/// does). A closed service answers the typed diagnostic.
pub fn on_owner_for_test(remote: &Remote, waits: bool, f: Box<dyn FnOnce() + Send>) -> Result<(), Diagnostic> {
    route_unit(&remote.0, waits, f)
}

/// The `Send` half of a service: what a worker thread holds to queue requests for the owner.
#[derive(Clone)]
pub struct Remote(Arc<Shared>);

pub fn remote(t_: &t) -> Remote {
    Remote(t_.0.shared.clone())
}

fn scope_of(id: i64) -> Result<Rc<G::Residency>, Diagnostic> {
    SCOPES.with(|s| s.borrow().get(&id).cloned()).ok_or_else(|| error("The resident scope is closed."))
}

/// Drops the resident scopes whose proxies are gone (owner only).
fn reap_scopes() {
    let released: Vec<i64> = std::mem::take(&mut *RELEASED_SCOPES.lock().unwrap());
    if !released.is_empty() {
        SCOPES.with(|s| {
            let mut s = s.borrow_mut();
            for id in released {
                s.remove(&id);
            }
        });
    }
}

/// On a cook worker, before a call that reads the scope's counts: waits off the owner until the
/// device has finished the scope's work, asking the owner only to commit and poll. The owner keeps
/// drawing frames meanwhile instead of blocking in the read. On the owner it does nothing.
fn ready_first(shared: &Arc<Shared>, id: i64) {
    if is_owner(shared) {
        return;
    }
    loop {
        let ready = route(shared, false, Box::new(move || Box::new(scope_of(id).map_or(true, |r| (r.ready)())) as Box<dyn Any + Send>));
        match ready.map(|b| *b.downcast::<bool>().expect("route type")) {
            Ok(false) => std::thread::sleep(Duration::from_micros(200)),
            _ => return,
        }
    }
}

/// Held by every closure of a proxy residency: the owner's scope goes when the last is dropped.
struct ScopeGuard(i64);
impl Drop for ScopeGuard {
    fn drop(&mut self) {
        RELEASED_SCOPES.lock().unwrap().push(self.0);
    }
}

fn proxy_residency(shared: &Arc<Shared>, real: &Arc<G::Backend>, fast: bool) -> Option<G::Residency> {
    let r = real.clone();
    let made = route(
        shared,
        false,
        Box::new(move || {
            reap_scopes();
            let scope = (r.residency)(fast).map(|scope| {
                let id = NEXT_SCOPE.fetch_add(1, Ordering::SeqCst);
                SCOPES.with(|s| s.borrow_mut().insert(id, Rc::new(scope)));
                id
            });
            Box::new(scope) as Box<dyn Any + Send>
        }),
    )
    .ok()?;
    let id = (*made.downcast::<Option<i64>>().expect("route type"))?;
    let guard = Arc::new(ScopeGuard(id));
    let s = shared.clone();
    let value_opt = |s: &Arc<Shared>, g: &Arc<ScopeGuard>| (s.clone(), g.clone());
    let (s1, g1) = value_opt(&s, &guard);
    let (s2, g2) = value_opt(&s, &guard);
    let (s3, g3) = value_opt(&s, &guard);
    let (s4, g4) = value_opt(&s, &guard);
    let (s5, g5) = value_opt(&s, &guard);
    let (s6, g6) = value_opt(&s, &guard);
    let (s7, g7) = value_opt(&s, &guard);
    let (s8, g8) = value_opt(&s, &guard);
    let (s9, g9) = value_opt(&s, &guard);
    let (s10, g10) = value_opt(&s, &guard);
    let (s11, g11) = value_opt(&s, &guard);
    let (s12, g12) = value_opt(&s, &guard);
    let (s13, g13) = value_opt(&s, &guard);
    let (s14, g14) = value_opt(&s, &guard);
    let (s15, g15) = value_opt(&s, &guard);
    let (s16, g16) = value_opt(&s, &guard);
    let (s17, g17) = value_opt(&s, &guard);
    let executor = c::Resident {
        fill: Box::new(move |ty: &Ty, length: i64, value: &Value| {
            let (ty, value, g) = (ty.clone(), value.clone(), g1.clone());
            route_value_opt(&s1, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.fill)(&ty, length, &value))))
        }),
        invoke: Box::new(move |packed: &Packed, values: &[Value], locals: &[Value], live: &Frame| {
            let (packed, values, locals, live, g) = (packed.clone(), values.to_vec(), locals.to_vec(), live.clone(), g2.clone());
            route_value_opt(&s2, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.invoke)(&packed, &values, &locals, &live))))
        }),
        invoke_all: Box::new(move |packed: &Packed, tys: &[Ty], values: &[Value], locals: &[Value], live: &Frame| {
            let (packed, tys, values, locals, live, g) = (packed.clone(), tys.to_vec(), values.to_vec(), locals.to_vec(), live.clone(), g17.clone());
            route_value_opt(&s17, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.invoke_all)(&packed, &tys, &values, &locals, &live))))
        }),
        unfold: Box::new(move |packed: &Packed, values: &[Value], steps: i64, source: &Value, locals: &[Value], live: &Frame| {
            let (packed, values, source, locals, live, g) = (packed.clone(), values.to_vec(), source.clone(), locals.to_vec(), live.clone(), g3.clone());
            route_value_opt(&s3, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.unfold)(&packed, &values, steps, &source, &locals, &live))))
        }),
        compact: Box::new(move |value: &Value| {
            let (value, g) = (value.clone(), g4.clone());
            route_value_opt(&s4, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.compact)(&value))))
        }),
        coerce: Box::new(move |ty: &Ty, value: &Value| {
            let (ty, value, g) = (ty.clone(), value.clone(), g5.clone());
            route_value_opt(&s5, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.coerce)(&ty, &value))))
        }),
        concat: Box::new(move |a: &Value, b: &Value| {
            let (a, b, g) = (a.clone(), b.clone(), g6.clone());
            route_value_opt(&s6, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.concat)(&a, &b))))
        }),
        finish: Box::new(move |value: &Value| {
            let (value, g) = (value.clone(), g7.clone());
            ready_first(&s7, g.0);
            route_value(&s7, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.finish)(&value))))
        }),
        keep: Box::new(move |value: &Value| {
            let (value, g) = (value.clone(), g8.clone());
            ready_first(&s8, g.0);
            route_value_opt(&s8, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.keep)(&value))))
        }),
        count: Box::new(move |value: &Value| {
            let (value, g) = (value.clone(), g9.clone());
            ready_first(&s9, g.0);
            route_count(&s9, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.count)(&value))))
        }),
        capacity: Box::new(move |value: &Value| {
            let (value, g) = (value.clone(), g10.clone());
            route_i64_opt(&s10, Box::new(move || scope_of(g.0).ok().and_then(|r| (r.executor.capacity)(&value)))).ok().flatten()
        }),
        close: Box::new(move || {
            let g = g11.clone();
            ready_first(&s11, g.0);
            let _ = route_unit(&s11, false, Box::new(move || {
                if let Ok(r) = scope_of(g.0) {
                    (r.executor.close)()
                }
            }));
        }),
        settle: Box::new(move || {
            let g = g12.clone();
            ready_first(&s12, g.0);
            route_unit_result(&s12, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.settle)())))
        }),
        range: Box::new(move |ty: &Ty, value: &Value, div: i64, length: i64| {
            let (ty, value, g) = (ty.clone(), value.clone(), g13.clone());
            route_value_opt(&s13, false, Box::new(move || scope_of(g.0).and_then(|r| (r.executor.range)(&ty, &value, div, length))))
        }),
    };
    Some(G::Residency {
        executor: Arc::new(executor),
        retain_uint32: Box::new(move |value: &Value| {
            let (value, g) = (value.clone(), g14.clone());
            ready_first(&s14, g.0);
            route(&s14, false, Box::new(move || Box::new(scope_of(g.0).ok().and_then(|r| (r.retain_uint32)(&value))) as Box<dyn Any + Send>))
                .ok()
                .and_then(|b| *b.downcast::<Option<G::Integers>>().expect("route type"))
        }),
        retain_float: Box::new(move |value: &Value| {
            let (value, g) = (value.clone(), g15.clone());
            ready_first(&s15, g.0);
            route(&s15, false, Box::new(move || Box::new(scope_of(g.0).ok().and_then(|r| (r.retain_float)(&value))) as Box<dyn Any + Send>))
                .ok()
                .and_then(|b| *b.downcast::<Option<G::Planes>>().expect("route type"))
        }),
        validate_topology: Box::new(move |point_count: i64, vertices: &Value, offsets: &Value, kinds: &Value, max_polygon_size: i64| {
            let (vertices, offsets, kinds, g) = (vertices.clone(), offsets.clone(), kinds.clone(), g16.clone());
            ready_first(&s16, g.0);
            match route(
                &s16,
                false,
                Box::new(move || {
                    Box::new(match scope_of(g.0) {
                        Ok(r) => (r.validate_topology)(point_count, &vertices, &offsets, &kinds, max_polygon_size),
                        Err(d) => Some(Err(d)),
                    }) as Box<dyn Any + Send>
                }),
            ) {
                Ok(b) => *b.downcast::<Option<Result<(), Diagnostic>>>().expect("route type"),
                Err(d) => Some(Err(d)),
            }
        }),
        ready: {
            let (s, g) = (s.clone(), guard.clone());
            Box::new(move || {
                let g = g.clone();
                route(&s, false, Box::new(move || Box::new(scope_of(g.0).map_or(true, |r| (r.ready)())) as Box<dyn Any + Send>))
                    .map_or(true, |b| *b.downcast::<bool>().expect("route type"))
            })
        },
    })
}

fn proxy_kernel(shared: &Arc<Shared>, kernel: Arc<G::Kernel>) -> G::Kernel {
    let (s1, k1) = (shared.clone(), kernel.clone());
    let (s2, k2) = (shared.clone(), kernel.clone());
    let (s3, k3) = (shared.clone(), kernel);
    G::Kernel {
        run: Box::new(move |inputs| {
            let (inputs, k) = (inputs.clone(), k1.clone());
            route(&s1, false, Box::new(move || Box::new((k.run)(&inputs)) as Box<dyn Any + Send>)).and_then(|b| *b.downcast::<Result<G::Value, Diagnostic>>().expect("route type"))
        }),
        readback: Box::new(move |value| {
            let (value, k) = (value.clone(), k2.clone());
            route_value(&s2, Box::new(move || (k.readback)(&value)))
        }),
        planes: Box::new(move |value| {
            let (value, k) = (value.clone(), k3.clone());
            let planes = route(&s3, false, Box::new(move || Box::new((k.planes)(&value)) as Box<dyn Any + Send>))
                .and_then(|b| *b.downcast::<Result<G::Planes, Diagnostic>>().expect("route type"))?;
            let real = Arc::new(planes);
            let (settle_real, settle_shared) = (real.clone(), s3.clone());
            let (bounds_real, bounds_shared) = (real.clone(), s3.clone());
            let (release_real, materialise_real) = (real.clone(), real.clone());
            Ok(G::Planes {
                count: real.count,
                width: real.width,
                materialise: Box::new(move || (materialise_real.materialise)()),
                resident: real.resident.clone(),
                settle: Box::new(move || {
                    let p = settle_real.clone();
                    route_unit_result(&settle_shared, true, Box::new(move || (p.settle)()))
                }),
                release: Box::new(move |held| (release_real.release)(held)),
                bounds: Box::new(move || {
                    let p = bounds_real.clone();
                    route_floats_opt(&bounds_shared, Box::new(move || (p.bounds)())).ok().flatten()
                }),
            })
        }),
    }
}

/// The backend handed to cook workers: every call is served on the owner by `pump`.
fn proxy(t_: &t) -> Arc<G::Backend> {
    let d = &*t_.0;
    if let Some(p) = &*d.proxy.borrow() {
        return p.clone();
    }
    let real = backend(t_);
    let shared = d.shared.clone();
    let (rs, rr) = (shared.clone(), real.clone());
    let (cs, cr) = (shared.clone(), real.clone());
    let (ps, pr) = (shared, real);
    let proxy = Arc::new(G::Backend {
        residency: Box::new(move |fast| proxy_residency(&rs, &rr, fast)),
        cost: Box::new(move |inputs, fast, packed, count| {
            let (inputs, packed, r) = (inputs.cloned(), packed.clone(), cr.clone());
            route(&cs, false, Box::new(move || Box::new((r.cost)(inputs.as_ref(), fast, &packed, count)) as Box<dyn Any + Send>))
                .ok()
                .and_then(|b| *b.downcast::<Option<f64>>().expect("route type"))
        }),
        prepare: Box::new(move |fast, packed| {
            let (packed, r) = (packed.clone(), pr.clone());
            let prepared = route(&ps, false, Box::new(move || Box::new((r.prepare)(fast, &packed)) as Box<dyn Any + Send>))
                .and_then(|b| *b.downcast::<Result<Arc<G::Kernel>, Diagnostic>>().expect("route type"))?;
            Ok(Arc::new(proxy_kernel(&ps, prepared)))
        }),
    });
    *d.proxy.borrow_mut() = Some(proxy.clone());
    proxy
}

/// A cook's requests come in chains (a dispatch, its planes, the next dispatch), each sent a
/// fraction of a millisecond after the last returns. The pump serves what is queued, then waits
/// `follow_up` for the next link, so a chain takes one pump rather than a frame per request. A
/// settle is not waited for: it blocks until the GPU finishes, which by the next pump it usually
/// has.
const FOLLOW_UP: Duration = Duration::from_micros(500);

fn take(shared: &Shared, chained: bool) -> Option<Arc<Job>> {
    let mut queue = shared.queue.lock().unwrap();
    match queue.jobs.front() {
        Some(job) if chained && job.waits => None,
        Some(_) => queue.jobs.pop_front(),
        None => None,
    }
}

/// Serve this workspace's queued GPU requests on the initial domain. `follow_up` (default true)
/// spins 0.5 ms after a request for the worker's next link of the chain; a caller that `wait`s
/// passes false.
pub fn pump(t_: &t, follow_up: bool) {
    let d = &*t_.0;
    if !on_owner(d) {
        panic!("Workspace_gpu.pump: initial domain required");
    }
    reap_scopes();
    run::service();
    let shared = &d.shared;
    // only the pump's first take may be a settle; everything after it is a follow-up
    let Some(first) = take(shared, false) else { return };
    serve(shared, &first);
    if !follow_up {
        while let Some(job) = take(shared, false) {
            serve(shared, &job);
        }
        return;
    }
    let mut until = Instant::now() + FOLLOW_UP;
    loop {
        let empty = shared.queue.lock().unwrap().jobs.is_empty();
        if !empty {
            match take(shared, true) {
                Some(job) => {
                    serve(shared, &job);
                    until = Instant::now() + FOLLOW_UP;
                }
                None => return,
            }
        } else if Instant::now() >= until {
            return;
        } else {
            std::hint::spin_loop();
        }
    }
}

/// Sleep on the initial domain until a request is queued or `timeout` seconds pass (the cook
/// finishing signals nothing here, so a caller waiting on that passes a short timeout).
pub fn wait(t_: &t, timeout: f64) {
    let d = &*t_.0;
    if !on_owner(d) {
        panic!("Workspace_gpu.wait: initial domain required");
    }
    let queue = d.shared.queue.lock().unwrap();
    if queue.jobs.is_empty() {
        let _ = d.shared.wake.wait_timeout(queue, Duration::from_secs_f64(timeout.max(0.))).unwrap();
    }
}

/// Seconds per float of reading a device-local buffer back through `invalidate_mapped`, taken once
/// per owner on the first config after the service is ready. Unified memory (the M1) keeps the
/// table in `Flow_ir::Cost`; a discrete card crosses PCIe and the transfer queue, and says so here.
fn readback_rate(d: &Data) -> Option<f64> {
    if let Some(rate) = *d.readback.borrow() {
        return rate;
    }
    if !d.measurement_ready.get() || !on_owner(d) {
        return None;
    }
    let rate = (|| -> Option<f64> {
        let (gpu, _) = owner(d).ok()?;
        let device = x::gpu_device(&gpu);
        if ob::capabilities(&device).unified_memory {
            return None;
        }
        let floats: i64 = 1 << 20;
        let buffer = ob::create_mapped_buffer(&device, BufferDescriptor { label: Some("Flow readback calibration".to_owned()), size: floats * 4, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] }).ok()?;
        let once = || -> Option<f64> {
            let start = (d.clock)();
            ob::invalidate_mapped(&buffer, floats * 4).ok()?;
            Some(((d.clock)() - start) / floats as f64)
        };
        let _ = once();
        let runs: Vec<f64> = (0..3).filter_map(|_| once()).collect();
        let _ = ob::destroy_buffer(&buffer);
        if runs.len() < 3 { None } else { Some(median3(runs)) }
    })();
    *d.readback.borrow_mut() = Some(rate);
    rate
}

/// The frame is the pump: cook workers queue device requests, served here between frames' work.
pub fn config(t_: &t, fast: bool) -> G::Config {
    let d = &*t_.0;
    G::Config { backend: Some(proxy(t_)), fast, policy: d.policy.get(), readback_per_float: readback_rate(d) }
}

struct PumpOnDrop<'a>(&'a t);
impl Drop for PumpOnDrop<'_> {
    fn drop(&mut self) {
        pump(self.0, true);
    }
}

pub fn with_backend(t_: &t, fast: bool, run: &mut dyn FnMut(&G::Config)) {
    pump(t_, true);
    // Static lowering can ask for a cost before the render owner is serving frames. Defer the
    // count-sized calibration until the persistent workspace service has entered its pump loop.
    t_.0.measurement_ready.set(true);
    let gpu = config(t_, fast);
    let _guard = PumpOnDrop(t_);
    run(&gpu);
}

/// Inside `with_backend(fast = true)`, a resident fold for one drawing on this workspace's host,
/// and its mark attributes over the arrays it keeps after its loop (`host::show`).
pub fn resident(gpu: &G::Config, t_: &t) -> Option<drawing::resident> {
    if !gpu.fast {
        return None;
    }
    let (_, host) = owner(&t_.0).ok()?;
    let show_host = host.clone();
    let show: drawing::show = Rc::new(move |packed: &Packed, live: &Frame| h::show(&show_host, packed, live));
    Some((h::resident(false, gpu.fast, &host), show))
}

pub fn image(t_: &t, key: &str, width: i64, height: i64, publish: &mut dyn FnMut(&image_sink::Output) -> Result<(), Diagnostic>, value: &G::Value) -> Result<(), Diagnostic> {
    let d = &*t_.0;
    let (gpu, host) = owner(d)?;
    let Some(output) = h::output(&host, value)? else {
        return Err(error("The image's GPU producer was closed or superseded."));
    };
    let mut convert = |sink: &image_sink::T| -> Result<(), Diagnostic> {
        let converted = image_sink::convert(sink, width, height, &output)?;
        publish(&converted)
    };
    let found = d.images.borrow_mut().find(&|k, _| k == key).cloned();
    if let Some(sink) = found {
        return convert(&sink);
    }
    if d.images.borrow().length() >= 64 {
        return Err(error("A workspace owns at most 64 GPU image sinks."));
    }
    struct PendingImage<'a> {
        stats: &'a RefCell<ImageStats>,
        sink: Option<image_sink::T>,
    }
    impl Drop for PendingImage<'_> {
        fn drop(&mut self) {
            if let Some(sink) = self.sink.take() {
                close_image(self.stats, sink);
            }
        }
    }
    let mut pending = PendingImage { stats: &d.image_stats, sink: Some(image_sink::create(&gpu)?) };
    d.image_stats.borrow_mut().created += 1;
    convert(pending.sink.as_ref().unwrap())?;
    d.images.borrow_mut().add(key.to_owned(), pending.sink.take().unwrap());
    Ok(())
}

/// One `Marks`-family source per GPU mark set of a frame, numbered in drawing order (at most 64),
/// each owning the uploads of its CPU attributes and borrowing its GPU ones.
pub fn marks(t_: &t) -> Rc<marks_scene::gpu> {
    let id = t_.0.id;
    Rc::new(move |slot: i64, kind: i64, count: i64, attributes: &[marks_scene::attribute]| {
        let d = service(id).ok_or_else(|| error("Workspace GPU resources require their open initial-domain owner."))?;
        let (gpu, host) = owner(&d)?;
        let found = d.mark_sets.borrow_mut().find(&|k, _| *k == slot).cloned();
        let sink = match found {
            Some(sink) => sink,
            None => {
                let sink = native(x::create_gpu_marks(&gpu))?;
                d.mark_sets.borrow_mut().add(slot, sink.clone());
                sink
            }
        };
        let mut outputs: Vec<(G::Value, run::Output)> = Vec::new();
        for a in attributes {
            if let marks_scene::source::Device(value) = &a.source {
                match h::output(&host, value)? {
                    None => return Err(error("GPU display output is closed or superseded.")),
                    Some(output) => outputs.push((value.clone(), output)),
                }
            }
        }
        let output_of = |value: &G::Value| -> Option<run::Output> {
            outputs.iter().find(|(c, _)| c.identity == value.identity && c.stamp == value.stamp && c.output == value.output).map(|(_, o)| o.clone())
        };
        let sources: Vec<x::marks_source> = attributes
            .iter()
            .map(|a| match &a.source {
                marks_scene::source::Bytes(bytes) => x::marks_source::Host(renderer::scene_execution::new_bytes(bytes.clone()), a.stride),
                marks_scene::source::Device(value) => {
                    let output = output_of(value);
                    x::marks_source::Device(Rc::new(move || output.as_ref().and_then(|o| run::buffer(o, None))), a.stride)
                }
            })
            .collect();
        // positions a fast kernel wrote come with its count record: the set's instance count is
        // the one the device wrote, drawn indirect
        let indirect: Option<(Rc<dyn Fn() -> Option<ob::Buffer>>, i64)> = match &attributes[0].source {
            marks_scene::source::Device(value) => {
                let output = output_of(value);
                if output.as_ref().and_then(run::count_record).is_some() {
                    Some((Rc::new(move || output.as_ref().and_then(run::count_record)), 4))
                } else {
                    None
                }
            }
            _ => None,
        };
        native(x::gpu_marks_token(indirect, &sink, kind, count, &sources))
    })
}

fn drop_display(d: &Data, key: &str) {
    let bound = d.bound.borrow_mut().remove(key);
    if let Some((id, release)) = bound {
        // a re-display after the token died may already name this geometry with a newer key
        let same = d.shown.borrow().get(&id).is_some_and(|s| s.0 == key);
        if same {
            d.shown.borrow_mut().remove(&id);
        }
        d.releasing.borrow_mut().insert(0, (d.frames.get() + 1, release));
    }
}

/// One presented frame (initial domain): `keep` is every display key the frame's scenes still
/// draw, when they changed; a bound display left out is dropped. Displays dropped the frame before
/// are unregistered and unpinned, so their buffers return to the pool.
///
/// A dropped display's buffers wait for the next frame: the frame that last drew it is the one in
/// flight when it is dropped, and the renderer settles that before it draws again.
/// ponytail: counted in frames, not the presenting queue's completion, which needs the renderer's
/// epoch; a window not drawn (hidden) ticks with no settle, so a slower frame could see its
/// buffers reused, in a picture nobody sees.
pub fn frame(t_: &t, keep: Option<&[String]>) {
    let d = &*t_.0;
    if d.closed.get() || !on_owner(d) {
        return;
    }
    d.frames.set(d.frames.get() + 1);
    let dropped: Vec<String> = std::mem::take(&mut *d.dropped.lock().unwrap());
    for key in dropped {
        drop_display(d, &key);
    }
    if let Some(keep) = keep {
        let mut dead: Vec<String> = d.bound.borrow().keys().filter(|k| !keep.contains(k)).cloned().collect();
        dead.sort();
        for key in dead {
            drop_display(d, &key);
        }
    }
    let frames = d.frames.get();
    let (due, later): (Vec<_>, Vec<_>) = std::mem::take(&mut *d.releasing.borrow_mut()).into_iter().partition(|(at, _)| *at <= frames);
    *d.releasing.borrow_mut() = later;
    for (_, release) in due {
        release();
    }
}

/// The device's allocated bytes, its working set and whether its memory is the host's (one pool),
/// through OGPU; `None` with no device.
pub fn device_limits(t_: &t) -> Option<(i64, i64, bool)> {
    let d = &*t_.0;
    let read = |gpu: &x::gpu| {
        let device = x::gpu_device(gpu);
        (ob::allocated_size(&device), ob::recommended_working_set(&device), ob::capabilities(&device).unified_memory)
    };
    if d.closed.get() {
        return None;
    }
    if let Some((gpu, _)) = &*d.owner.borrow() {
        return Some(read(gpu));
    }
    // a read must not start this owner's host: a lease of the one process device
    match x::acquire_gpu() {
        Ok(gpu) => {
            let limits = read(&gpu);
            x::release_gpu(&gpu);
            Some(limits)
        }
        Err(_) => None,
    }
}

/// The frame boundary's read: an owner with no host yet holds nothing, and a lease per frame is
/// not free.
pub fn device_bytes(t_: &t) -> i64 {
    let d = &*t_.0;
    match &*d.owner.borrow() {
        Some((gpu, _)) if !d.closed.get() => ob::allocated_size(&x::gpu_device(gpu)),
        _ => 0,
    }
}

/// Displays bound or dropped and not yet released.
pub fn held_displays(t_: &t) -> i64 {
    (t_.0.bound.borrow().len() + t_.0.releasing.borrow().len()) as i64
}

/// A point cloud's or a triangle mesh's resident P (and N; a mesh without N takes normals made
/// from P on the device) as the renderer's device positions: pinned in the pool and registered
/// while the mesh that keeps the returned closure lives. Initial domain, after the cook settled.
/// `(key, whether N is bound, device triangle index count if any, colour width, keep-alive)`;
/// `None` (draw the host copy) otherwise.
pub fn display(t_: &t, geometry: &rdk::geometry::T) -> Option<(String, bool, Option<i64>, i64, Box<dyn Fn()>)> {
    let d = &*t_.0;
    if !on_owner(d) {
        return None;
    }
    if d.closed.get() {
        *d.display_refusal.borrow_mut() = Some("The workspace GPU service is closed.".to_owned());
        return None;
    }
    *d.display_refusal.borrow_mut() = None;
    let id = rdk::geometry::data_id(geometry);
    let again = {
        let shown = d.shown.borrow();
        match shown.get(&id) {
            Some((key, normals, indices, width, weak)) if d.bound.borrow().contains_key(key) => weak.upgrade().map(|token| {
                let keep: Box<dyn Fn()> = Box::new(move || {
                    let _ = &token;
                });
                (key.clone(), *normals, *indices, *width, keep)
            }),
            _ => None,
        }
    };
    if again.is_some() {
        return again;
    }
    let owner_ = d.owner.borrow().as_ref().map(|(g, hst)| (g.clone(), hst.clone()));
    let positions = flow_sop::attribute_kernel::resident_vec3(geometry, "P");
    let (Some((gpu, host)), Some(positions)) = (owner_, positions) else {
        *d.display_refusal.borrow_mut() = Some("Resident positions or the workspace device are unavailable.".to_owned());
        return None;
    };
    let count = rdk::geometry::point_count(geometry);
    let releases: Rc<RefCell<Vec<Box<dyn Fn()>>>> = Rc::new(RefCell::new(Vec::new()));
    let checks: Rc<RefCell<Vec<Rc<dyn Fn() -> bool>>>> = Rc::new(RefCell::new(Vec::new()));
    let hold = |valid: Rc<dyn Fn() -> bool>, release: Box<dyn Fn()>| {
        checks.borrow_mut().push(valid);
        releases.borrow_mut().push(release);
    };
    let release_all = {
        let releases = releases.clone();
        move || {
            for release in releases.borrow().iter() {
                release();
            }
        }
    };
    let pin = |placeholder: &Arc<Vec<f64>>| -> Result<ob::Buffer, String> {
        match run::pin(placeholder) {
            Some((buffer, n, valid, unpin)) if n == count => {
                hold(Rc::from(valid), unpin);
                Ok(buffer)
            }
            Some((_, _, _, unpin)) => {
                unpin();
                Err("Resident display stream count differs from the geometry.".to_owned())
            }
            None => Err("Resident display stream is no longer available.".to_owned()),
        }
    };
    let owned = |buffer: ob::Buffer, release: Box<dyn Fn()>| -> ob::Buffer {
        let live = Rc::new(Cell::new(true));
        let l2 = live.clone();
        hold(Rc::new(move || l2.get()), Box::new(move || {
            live.set(false);
            release();
        }));
        buffer
    };
    let topology = rdk::geometry::topology(geometry);
    let bound = (|| -> Result<(String, bool, Option<i64>, i64, Box<dyn Fn()>), String> {
        let buffer = pin(&positions)?;
        let normals = match rdk::geometry::find_attribute(rdk::attribute::Owner::Point, "N", geometry) {
            None if rdk::geometry::primitive_count(geometry) == 0 => None,
            None if rdk::topology::resident(topology) => return Err("Resident topology without authored normals requires the host display.".to_owned()),
            None if !rdk::topology::all_triangles(topology).unwrap_or(false) => return Err("Host polygon topology requires the host display.".to_owned()),
            None => {
                // lit triangles without N: the host display's normals, made from P on the device
                let corners = rdk::topology::View::view(topology).map_err(|_| "Device normals could not be derived.".to_owned())?.vertex_points.to_vec();
                match h::normals(&host, &buffer, count, &Arc::new(corners)) {
                    Some((buffer, release)) => Some(owned(buffer, release)),
                    None => return Err("Device normals could not be derived.".to_owned()),
                }
            }
            Some(_) => match flow_sop::attribute_kernel::resident_vec3(geometry, "N") {
                Some(value) => Some(pin(&value)?),
                None => return Err("Point normals are not an unread resident vec3 stream.".to_owned()),
            },
        };
        let colors = match rdk::geometry::find_attribute(rdk::attribute::Owner::Point, "Cd", geometry) {
            None => None,
            Some(_) => match flow_sop::attribute_kernel::resident_color(geometry) {
                Some((value, width)) => Some((pin(&value)?, width as i64)),
                None => return Err("Point colours are not an unread resident float3/float4 stream.".to_owned()),
            },
        };
        let indices = if rdk::geometry::primitive_count(geometry) == 0 {
            None
        } else {
            match flow_sop::topology_kernel::resident(geometry) {
                None if !rdk::topology::resident(topology) && rdk::topology::all_triangles(topology).unwrap_or(false) => None,
                None => return Err("Topology is unavailable as resident triangle/quad streams.".to_owned()),
                Some((vertices, offsets, kinds)) => {
                    let pins: Vec<Option<run::Pin>> = [&vertices, &offsets, &kinds].iter().map(|x| run::pin_integers(x)).collect();
                    let result = match (pins.get(0), pins.get(1), pins.get(2)) {
                        (Some(Some((v, nv, vv, _))), Some(Some((o, no, ov, _))), Some(Some((k, nk, kv, _)))) if vv() && ov() && kv() => {
                            h::triangle_indices(&host, (&buffer, count), (v, *nv), (o, *no), (k, *nk))
                                .map_err(|e| rays_diagnostic::to_string(&e))
                                .map(|(indices, n, release)| Some((owned(indices, release), n)))
                        }
                        _ => Err("Resident topology streams cannot be pinned.".to_owned()),
                    };
                    for pin in pins.iter().flatten() {
                        (pin.3)();
                    }
                    result?
                }
            }
        };
        let checks2 = checks.clone();
        let closed = d.closed.clone();
        let (key, unregister) = x::register_gpu_positions(
            &gpu,
            &buffer,
            normals.clone(),
            indices.clone(),
            colors.clone(),
            count,
            Rc::new(move || !closed.get() && checks2.borrow().iter().all(|valid| valid())),
        )
        .map_err(|e| ogpu::error::to_string(&e))?;
        d.displays.set(d.displays.get() + 1);
        let releases2 = releases.clone();
        let unregister: Rc<dyn Fn()> = Rc::new(unregister);
        d.bound.borrow_mut().insert(
            key.clone(),
            (
                id,
                Rc::new(move || {
                    unregister();
                    for release in releases2.borrow().iter() {
                        release();
                    }
                }),
            ),
        );
        let token = Rc::new(DropToken { key: key.clone(), dropped: d.dropped.clone() });
        let width = colors.as_ref().map_or(0, |c| c.1);
        d.shown.borrow_mut().insert(id, (key.clone(), normals.is_some(), indices.as_ref().map(|i| i.1), width, Rc::downgrade(&token)));
        let keep: Box<dyn Fn()> = Box::new(move || {
            let _ = &token;
        });
        Ok((key, normals.is_some(), indices.as_ref().map(|i| i.1), width, keep))
    })();
    match bound {
        Ok(result) => Some(result),
        Err(reason) => {
            release_all();
            flow_ir::fallback::note("display", &reason);
            *d.display_refusal.borrow_mut() = Some(reason);
            None
        }
    }
}

pub fn displays(t_: &t) -> i64 {
    t_.0.displays.get()
}

/// The last initial-domain display attempt's host-fallback reason, or `None` on success.
pub fn display_refusal(t_: &t) -> Option<String> {
    t_.0.display_refusal.borrow().clone()
}

/// Host totals, sinks created/closed and sink buffer/texture creations, including failed
/// uncommitted sinks and totals retained after close.
pub fn image_stats(t_: &t) -> (h::Stats, i64, i64, i64, i64) {
    let d = &*t_.0;
    if !on_owner(d) {
        panic!("Workspace_gpu.image_stats: initial domain required");
    }
    let host = match &*d.owner.borrow() {
        Some((_, host)) => h::stats(host),
        None => d.closed_host.borrow().clone(),
    };
    let stats = d.image_stats.borrow().clone();
    let (mut buffers, mut textures) = (stats.buffers, stats.textures);
    for (_, sink) in d.images.borrow().iter() {
        buffers += image_sink::buffer_creations(sink);
        textures += image_sink::texture_creations(sink);
    }
    (host, stats.created, stats.closed, buffers, textures)
}

pub fn close(t_: &t) {
    let d = &*t_.0;
    if !on_owner(d) {
        panic!("Workspace_gpu.close: initial domain required");
    }
    if d.closed.get() {
        return;
    }
    {
        let mut queue = d.shared.queue.lock().unwrap();
        queue.closed = true;
        for b in [d.backend.borrow().as_ref(), d.proxy.borrow().as_ref()].into_iter().flatten() {
            flow_sop::lower::forget_backend(b);
        }
        let diagnostic = error("The workspace GPU service closed before the request ran.");
        while let Some(job) = queue.jobs.pop_front() {
            *job.run.lock().unwrap() = None;
            *job.slot.lock().unwrap() = Some(Err(diagnostic.clone()));
            job.finished.store(true, Ordering::SeqCst);
        }
        d.shared.ready.notify_all();
    }
    let bound: Vec<(i64, Rc<dyn Fn()>)> = d.bound.borrow_mut().drain().map(|(_, v)| v).collect();
    for (_, release) in bound {
        release();
    }
    let releasing: Vec<(i64, Rc<dyn Fn()>)> = std::mem::take(&mut *d.releasing.borrow_mut());
    for (_, release) in releasing {
        release();
    }
    d.mark_sets.borrow_mut().clear();
    d.images.borrow_mut().clear();
    let owner_ = d.owner.borrow_mut().take();
    if let Some((gpu, host)) = owner_ {
        h::close(&host);
        *d.closed_host.borrow_mut() = h::stats(&host);
        x::release_gpu(&gpu);
    }
    d.closed.set(true);
    SERVICES.with(|s| s.borrow_mut().remove(&d.id));
}
