//! The GPU host owns bounded runner/pipeline caches, resident fold buffers, display storage and topology validation. Rc resources stay on their creating thread; Send+Sync proxies carry IDs/guards and resolve through a thread-local owner registry. Foreign-thread calls return a typed ownership error or decline the resident fold.
use crate::kernels;
use crate::pipelines::{self, is_vulkan};
use crate::run::{self, gpu as gpu_r, Options, Uint32Holder};
use crate::small_lru::List;
use flow_ir::core_exec;
use flow_ir::core_ir::region::Instruction;
use flow_ir::flow_ir::gpu as G;
use flow_ir::packed::host as phost;
use flow_shader::emit::{self, Kernel};
use flow_syntax::ty::Ty;
use flow_syntax::value::{self, Value};
use input::Frame;
use ogpu_core::backend::{self as b, Buffer, Commands, ComputeEncoder, Queue};
use ogpu_core::types::{BufferDescriptor, BufferUsage};
use rays_diagnostic::Diagnostic;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, LazyLock};
use std::thread::ThreadId;

type Packed = flow_ir::packed::T;

static NEXT: AtomicI64 = AtomicI64::new(1);
static NEXT_SCOPE: AtomicI64 = AtomicI64::new(1);
static IOTA_OWNER: LazyLock<mem::Owner> = LazyLock::new(|| mem::plan_in(mem::Pool::Device, "flow_gpu.iota"));
static DISPLAY_OWNER: LazyLock<mem::Owner> = LazyLock::new(|| mem::plan_in(mem::Pool::Device, "flow_gpu.display"));
static RESIDENT_OWNER: LazyLock<mem::Owner> = LazyLock::new(|| mem::plan_in(mem::Pool::Device, "flow_gpu.resident"));

fn me() -> ThreadId {
    std::thread::current().id()
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Stats {
    pub runners_created: i64,
    pub runners_released: i64,
    pub pipeline_compilations: i64,
    pub pipeline_releases: i64,
    pub buffer_creations: i64,
    pub input_uploads: i64,
    pub input_uploaded_bytes: i64,
    pub status_reads: i64,
    pub readback_bytes: i64,
    pub payload_readback_bytes: i64,
    pub status_readback_bytes: i64,
    pub count_readback_bytes: i64,
    pub validation_readback_bytes: i64,
    /// Successful readbacks by purpose, including the resident executor's direct reads.
    /// `readback_bytes` remains payload plus bounds; status, counts and validation are separate.
    pub bounds_readback_bytes: i64,
    /// kept outputs a lazy store read (`Run::keep`)
    pub materialisations: i64,
    /// buffers kept outputs hold now
    pub held_buffers: i64,
    /// kept outputs whose owner freed them unread, never handed out
    pub freed: i64,
    /// resident regions dispatched (a drawing's marks over their arrays are not counted)
    pub resident_dispatches: i64,
}

pub fn zero() -> Stats {
    Stats::default()
}

fn add_run(stats: &Stats, r: &run::T) -> Stats {
    Stats {
        buffer_creations: stats.buffer_creations + run::private::buffer_creations(r),
        input_uploads: stats.input_uploads + run::private::input_uploads(r),
        input_uploaded_bytes: stats.input_uploaded_bytes + run::private::input_uploaded_bytes(r),
        status_reads: stats.status_reads + run::private::status_reads(r),
        readback_bytes: stats.readback_bytes + run::private::readback_bytes(r),
        payload_readback_bytes: stats.payload_readback_bytes + run::private::readback_bytes(r) - run::private::bounds_readback_bytes(r),
        status_readback_bytes: stats.status_readback_bytes + 4 * run::private::status_reads(r),
        bounds_readback_bytes: stats.bounds_readback_bytes + run::private::bounds_readback_bytes(r),
        materialisations: stats.materialisations + run::private::materialisations(r),
        held_buffers: stats.held_buffers,
        ..stats.clone()
    }
}

struct Runner {
    identity: i64,
    run: run::T,
    stamp: Cell<i64>,
    output: RefCell<Option<run::Output>>,
}

/// A resident fold's slot: a site's Nth region in one fold run, with the compactions and retypings
/// of its array. Its buffers and placeholders come back to it as the run closes and serve the next
/// draw's run; what a run did not take (a size it outgrew) is destroyed then. `capacity` is the
/// smallest power of two above the largest count it has held, 0 before it held one: it never
/// shrinks while the site lives, so a count that falls back costs memory, never a cut.
pub(crate) struct HSlot {
    capacity: Cell<i64>,
    /// false once its site left the host: its things are destroyed at close
    kept: Cell<bool>,
    /// buffers by byte size
    free: RefCell<HashMap<i64, Vec<Buffer>>>,
    /// this run's placeholders in it
    held: RefCell<Vec<Rc<Placeholder>>>,
    /// this run's compaction scratch in it
    spent: RefCell<Vec<(Buffer, i64)>>,
    /// this run's buffers it took back (ping-pong), for its next use in the run
    spare: RefCell<Vec<(Buffer, i64)>>,
}

fn new_slot(kept: bool) -> Rc<HSlot> {
    Rc::new(HSlot { capacity: Cell::new(0), kept: Cell::new(kept), free: RefCell::new(HashMap::new()), held: RefCell::new(vec![]), spent: RefCell::new(vec![]), spare: RefCell::new(vec![]) })
}

pub(crate) struct Placeholder {
    /// identity of the value it stands for
    key: usize,
    /// keeps `key`'s storage alive
    hold: Value,
    kind: i64,
    uint32: bool,
    buffer: (Buffer, i64),
    length: i64,
    cell: Option<(Buffer, i64)>,
    slot: Rc<HSlot>,
    /// its count is the device's: it sizes the slot
    counted: bool,
    /// its count is its length, known on the host (an exact array/concat may take it)
    known: bool,
    /// with its length, the slot's capacity, below its bound, the run's number of its region; else max
    clamped: i64,
    /// its count is its cell's times `mul` over `div` (a range of a kept count)
    mul: i64,
    div: i64,
    /// its buffers are its slot's (a range's buffer is the host's index buffer)
    owned: Cell<bool>,
    /// its slot was taken again in the run (nothing held it): only its count cell is still its
    dead: Cell<bool>,
    /// after the loop: the device count it was dispatched with, its own count when uncut
    origin: Option<(Buffer, i64, i64)>,
}

impl Placeholder {
    /// `{p with ...}`: a copy with its own mutable flags.
    fn copy(&self) -> Placeholder {
        Placeholder {
            key: self.key,
            hold: self.hold.clone(),
            kind: self.kind,
            uint32: self.uint32,
            buffer: self.buffer.clone(),
            length: self.length,
            cell: self.cell.clone(),
            slot: self.slot.clone(),
            counted: self.counted,
            known: self.known,
            clamped: self.clamped,
            mul: self.mul,
            div: self.div,
            owned: Cell::new(self.owned.get()),
            dead: Cell::new(self.dead.get()),
            origin: self.origin.clone(),
        }
    }
}

/// a region site's runner (None when the device refuses its form), its slots, and the runner that
/// draws it as a mark attribute over a fold's kept arrays (`resident`'s show)
struct Site {
    run: Option<run::T>,
    slots: RefCell<Vec<Rc<HSlot>>>,
    shown: RefCell<Option<Rc<Runner>>>,
    /// from its first unbound region
    plan: RefCell<Option<Option<phost::Plan>>>,
    /// the uniform widths and code of the region: a site outlives its lowering, so it keeps no
    /// `Packed.t`, which holds that lowering's whole program and source
    shape: RefCell<Option<(Vec<usize>, Arc<Vec<Instruction>>)>>,
    /// the plan's inputs, refilled per call
    inputs: RefCell<Option<phost::Inputs>>,
    /// its kernel run n passes in a lane, by n
    unfolds: RefCell<Vec<(i64, Option<run::T>)>>,
}

pub(crate) struct Data {
    id: i64,
    gpu: renderer::renderer::Gpu,
    pipelines: pipelines::T,
    runners: RefCell<List<String, Rc<Runner>>>,
    /// outputs a mark set drew without waiting for: their status is read once the queue is past them
    shown_unsettled: RefCell<Vec<(Rc<Runner>, i64)>>,
    created: Cell<i64>,
    retired: Rc<RefCell<Stats>>,
    /// resident fold sites by their code
    sites: RefCell<List<(Arc<Vec<Instruction>>, u8), Rc<Site>>>,
    /// fast dispatches' epochs, oldest first
    flight: RefCell<Vec<i64>>,
    /// 0., 1., 2., ... on the device, for range sources
    iota: RefCell<Option<(Buffer, i64)>>,
    iota_uint32: RefCell<Option<(Buffer, i64)>>,
    /// the largest uint32 index count asked for, so a frame whose index buffer a holder took makes
    /// one buffer of the size it ends at, not one per growth step
    iota_uint32_hint: Cell<i64>,
    /// outgrown index buffers, each with the newest epoch that can read it: named at the next
    /// scope, when the scope that outgrew it has committed; destroyed once that epoch completes
    retired_buffers: RefCell<Vec<(Buffer, Option<i64>)>>,
    /// display normals' buffers by byte size
    buffers: RefCell<HashMap<i64, Vec<Buffer>>>,
    closed: Cell<bool>,
    /// buffers a resident fold's slots made
    resident_buffers: Cell<i64>,
    resident_payload_bytes: Cell<i64>,
    adopted_uploads: Cell<i64>,
    /// the large host arrays scopes uploaded and the buffers that hold them (weak: an array that
    /// is gone is dropped from it), so a frame whose array repeats uploads nothing
    adopted_cache: RefCell<Vec<(std::sync::Weak<Vec<f64>>, (Buffer, i64))>>,
    /// host arrays a resident scope uploaded once
    adopted_uploaded_bytes: Cell<i64>,
    count_bytes: Cell<i64>,
    validation_bytes: Cell<i64>,
    validation_status: RefCell<Option<Buffer>>,
    /// the open resident fold's mark attributes, after its loop (`show`)
    showing: Cell<Option<i64>>,
    /// set by `close`: its runners' outputs are not read back
    discarding: Rc<Cell<bool>>,
    domain: ThreadId,
    /// the kept outputs' buffers of every runner, recycled across edits
    shared: run::Pool,
    ownership: RefCell<Option<run::T>>,
    /// a triangle mesh's corners and per-point corner CSR on the device, by its corners array
    tables: RefCell<List<Arc<Vec<u32>>, Option<Vec<Buffer>>>>,
    /// the buffers scopes upload adopted host arrays into (`Scope::adopt`)
    adopt_slot: Rc<HSlot>,
    /// the buffers a drawing's scopes fill and range into (`Scope::range_slot`)
    range_slot: Rc<HSlot>,
    dispatches: Cell<i64>,
}

#[derive(Clone)]
pub struct T(pub(crate) Rc<Data>);

thread_local! {
    static HOSTS: RefCell<HashMap<i64, Weak<Data>>> = RefCell::new(HashMap::new());
    static SCOPES: RefCell<HashMap<i64, Rc<Scope>>> = RefCell::new(HashMap::new());
}

fn host_of(id: i64) -> Option<T> {
    HOSTS.with(|h| h.borrow().get(&id).and_then(|w| w.upgrade())).map(T)
}

fn buffer_bytes(buffer: &Buffer) -> i64 {
    b::buffer_size(buffer)
}

fn destroy_owned(owner: &mem::Owner, buffer: &Buffer) {
    mem::release(owner, buffer_bytes(buffer));
    let _ = b::destroy_buffer(buffer);
}

fn resident_bytes(buffer: &Buffer) -> i64 {
    buffer_bytes(buffer)
}

fn destroy_resident(buffer: &Buffer) {
    mem::release(&RESIDENT_OWNER, resident_bytes(buffer));
    let _ = b::destroy_buffer(buffer);
}

fn purge(slot: &HSlot) {
    for (_, bs) in slot.free.borrow_mut().drain() {
        bs.iter().rev().for_each(destroy_resident);
    }
}

fn retire(slot: &HSlot) {
    slot.kept.set(false);
    purge(slot);
}

fn e_gpu(message: &str) -> Diagnostic {
    value::fail("E_GPU", message)
}

pub fn create(_clock: &dyn Fn() -> f64, gpu_: &renderer::renderer::Gpu) -> T {
    let created = Cell::new(0);
    let retired = Rc::new(RefCell::new(zero()));
    let discarding = Rc::new(Cell::new(false));
    let (r1, d1) = (retired.clone(), discarding.clone());
    let sites_release = Box::new(move |_: &(Arc<Vec<Instruction>>, u8), site: Rc<Site>| {
        if let Some(r) = &site.run {
            run::close(r, d1.get());
            let s = add_run(&r1.borrow(), r);
            *r1.borrow_mut() = s;
        }
        if let Some(runner) = site.shown.borrow().as_ref() {
            *runner.output.borrow_mut() = None;
            run::close(&runner.run, d1.get());
            let mut s = add_run(&r1.borrow(), &runner.run);
            s.runners_released = r1.borrow().runners_released + 1;
            *r1.borrow_mut() = s;
        }
        site.slots.borrow().iter().for_each(|s| retire(s));
    });
    let (r2, d2) = (retired.clone(), discarding.clone());
    let runners_release = Box::new(move |_: &String, runner: Rc<Runner>| {
        *runner.output.borrow_mut() = None;
        run::close(&runner.run, d2.get());
        let mut s = add_run(&r2.borrow(), &runner.run);
        s.runners_released = r2.borrow().runners_released + 1;
        *r2.borrow_mut() = s;
    });
    let tables_release = Box::new(|_: &Arc<Vec<u32>>, v: Option<Vec<Buffer>>| {
        if let Some(bs) = v {
            bs.iter().for_each(|x| {
                let _ = b::destroy_buffer(x);
            });
        }
    });
    let id = NEXT.fetch_add(1, Ordering::SeqCst);
    let data = Rc::new(Data {
        id,
        gpu: gpu_.clone(),
        pipelines: pipelines::create(_clock, &renderer::renderer::gpu_device(gpu_)),
        runners: RefCell::new(List::new(64, runners_release)),
        created,
        retired,
        sites: RefCell::new(List::new(32, sites_release)),
        flight: RefCell::new(vec![]),
        iota: RefCell::new(None),
        iota_uint32: RefCell::new(None),
        iota_uint32_hint: Cell::new(0),
        retired_buffers: RefCell::new(vec![]),
        buffers: RefCell::new(HashMap::new()),
        closed: Cell::new(false),
        resident_buffers: Cell::new(0),
        resident_payload_bytes: Cell::new(0),
        adopted_uploads: Cell::new(0),
        adopted_cache: RefCell::new(vec![]),
        adopted_uploaded_bytes: Cell::new(0),
        count_bytes: Cell::new(0),
        validation_bytes: Cell::new(0),
        validation_status: RefCell::new(None),
        showing: Cell::new(None),
        shown_unsettled: RefCell::new(Vec::new()),
        discarding,
        domain: me(),
        shared: run::pool(),
        ownership: RefCell::new(None),
        tables: RefCell::new(List::new(8, tables_release)),
        adopt_slot: new_slot(true),
        range_slot: new_slot(true),
        dispatches: Cell::new(0),
    });
    HOSTS.with(|h| h.borrow_mut().insert(id, Rc::downgrade(&data)));
    T(data)
}

fn live(t: &T) -> bool {
    !t.0.closed.get() && me() == t.0.domain
}

fn error<X>(message: &str) -> Result<X, Diagnostic> {
    Err(e_gpu(message))
}

fn queue_of(t: &T) -> Queue {
    renderer::renderer::gpu_queue(&t.0.gpu)
}

fn device_of(t: &T) -> b::Device {
    renderer::renderer::gpu_device(&t.0.gpu)
}

/// In fast precision a dispatch does not wait; its output is waited for when first borrowed or
/// read, and at most two stay in flight (the queue admits three command buffers).
fn complete(t: &T, epoch: i64) -> Result<(), Diagnostic> {
    let queue = queue_of(t);
    if epoch <= b::completed_epoch(&queue) {
        return Ok(());
    }
    gpu_r(b::complete_through(&queue, epoch))
}

/// Every command buffer the host commits without waiting (fast display regions, a resident fold's
/// levels) shares one limit: before another, all but the newest in flight are waited for, so at
/// most two are (the queue admits three).
fn admit(t: &T) -> Result<(), Diagnostic> {
    let queue = queue_of(t);
    let completed = b::completed_epoch(&queue);
    t.0.flight.borrow_mut().retain(|e| *e > completed);
    let older = {
        let f = t.0.flight.borrow();
        if f.len() >= 2 { Some(f[f.len() - 2]) } else { None }
    };
    match older {
        Some(older) => {
            complete(t, older)?;
            let mut f = t.0.flight.borrow_mut();
            let stale = f.len().saturating_sub(1);
            f.drain(..stale);
            Ok(())
        }
        None => Ok(()),
    }
}

fn record(t: &T, epoch: i64) {
    if epoch > 0 {
        t.0.flight.borrow_mut().push(epoch);
    }
}

/// The runner's current output for `value`, not waited for.
fn current(t: &T, value: &G::Value) -> Option<(Rc<Runner>, run::Output)> {
    if !live(t) {
        return None;
    }
    let found = t.0.runners.borrow().peek(&|_, r| r.identity == value.identity).cloned().or_else(|| {
        t.0.sites.borrow().iter().find_map(|(_, site)| match site.shown.borrow().as_ref() {
            Some(runner) if runner.identity == value.identity => Some(runner.clone()),
            _ => None,
        })
    });
    let runner = found?;
    if runner.stamp.get() != value.stamp {
        return None;
    }
    let output = runner.output.borrow().as_ref().and_then(|o| run::select_output(value.output, o))?;
    if run::count(&output) == value.count && run::width(&output) == value.width && (run::count(&output) == 0 || run::buffer(&output, None).is_some()) {
        Some((runner, output))
    } else {
        None
    }
}

/// As [`output`] for a mark set, which only binds the buffer: the draw is queued behind the kernel,
/// so nothing waits for it (the main thread stalled 2 to 3 ms a device attribute a frame). Its
/// status is read when the queue has passed it, by the next call here: a kernel's error (a
/// nonfinite or over-range value) is reported one frame after the frame that drew it.
pub fn output_shown(t: &T, value: &G::Value) -> Result<Option<run::Output>, Diagnostic> {
    let completed = b::completed_epoch(&queue_of(t));
    let due: Vec<(Rc<Runner>, i64)> = {
        let mut pending = t.0.shown_unsettled.borrow_mut();
        let (due, later) = std::mem::take(&mut *pending).into_iter().partition(|(_, epoch)| *epoch <= completed);
        *pending = later;
        due
    };
    for (runner, epoch) in due {
        run::private::check_status(&runner.run, epoch)?;
    }
    let Some((runner, output)) = current(t, value) else { return Ok(None) };
    let epoch = run::epoch(&output);
    if run::count(&output) > 0 && !t.0.shown_unsettled.borrow().iter().any(|(r, e)| Rc::ptr_eq(r, &runner) && *e == epoch) {
        t.0.shown_unsettled.borrow_mut().push((runner, epoch));
    }
    Ok(Some(output))
}

pub fn output(t: &T, value: &G::Value) -> Result<Option<run::Output>, Diagnostic> {
    let Some((runner, output)) = current(t, value) else { return Ok(None) };
    complete(t, run::epoch(&output))?;
    if run::count(&output) == 0 {
        return Ok(Some(output));
    }
    run::private::check_status(&runner.run, run::epoch(&output))?;
    Ok(Some(output))
}

/// the index buffer of a range source: grown geometrically, kept by the host
fn iota(t: &T, length: i64) -> Option<Buffer> {
    if let Some((buffer, capacity)) = &*t.0.iota.borrow() {
        if *capacity >= length {
            return Some(buffer.clone());
        }
    }
    let old = t.0.iota.borrow().clone();
    let capacity = 1.max(length.max(2 * old.as_ref().map_or(0, |o| o.1)));
    let buffer = b::create_mapped_buffer(
        &device_of(t),
        BufferDescriptor { label: Some("Flow resident indices".to_owned()), size: capacity * 4, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] },
    )
    .ok()?;
    if run::fill_float_indices(&queue_of(t), &t.0.pipelines, &buffer, capacity).is_err() {
        let _ = b::destroy_buffer(&buffer);
        return None;
    }
    mem::charge(&IOTA_OWNER, buffer_bytes(&buffer));
    if let Some((old, _)) = old {
        t.0.retired_buffers.borrow_mut().push((old, None));
    }
    *t.0.iota.borrow_mut() = Some((buffer.clone(), capacity));
    Some(buffer)
}

fn gpu_value(runner: &Runner, output: &run::Output) -> G::Value {
    G::Value {
        identity: runner.identity,
        count: run::count(output),
        width: run::width(output),
        output: 0,
        stamp: runner.stamp.get(),
        gpu_seconds: run::gpu_seconds(output),
    }
}

/// A prepared kernel. Its runners are the host's, found by `id`.
struct HostKernel {
    id: i64,
    fast: bool,
    index: Vec<bool>,
    key: String,
    msl: Kernel,
    packed: Packed,
}

/// A prepared kernel's runners go with it. A region whose count moves every frame (a filter's
/// consumers) is prepared again each frame under a new key, and the 64-runner list would otherwise
/// keep 64 dead producers with their output and upload buffers: 1.5 GB for 400,000 marks, over
/// the memory budget, where one live producer needs 30 MB.
impl Drop for HostKernel {
    fn drop(&mut self) {
        // another domain, a closed host or a list in use: the list's own eviction still applies
        let Some(t) = host_of(self.id).filter(live) else { return };
        let Ok(mut runners) = t.0.runners.try_borrow_mut() else { return };
        let typed = format!("{}:", self.key);
        let mine = |k: &String, _: &Rc<Runner>| *k == self.key || k.starts_with(&typed);
        while runners.peek(&mine).is_some() {
            runners.remove(&mine);
        }
    }
}

impl HostKernel {
    fn runner(&self, t: &T, inputs: Option<&phost::Inputs>) -> Result<Rc<Runner>, Diagnostic> {
        let fast = self.fast;
        let uint32_inputs: Vec<i64> = inputs.into_iter().flat_map(|i| i.arrays.iter().enumerate()).filter_map(|(i, a)| run::uint32_input(a).then_some(i as i64)).collect();
        let uint32_tables: Vec<i64> = inputs.into_iter().flat_map(|i| i.tables.iter().enumerate()).filter_map(|(i, a)| run::uint32_input(a).then_some(i as i64)).collect();
        let typed = !uint32_inputs.is_empty() || !uint32_tables.is_empty();
        let key = if typed { format!("{}:{uint32_inputs:?}:{uint32_tables:?}", self.key) } else { self.key.clone() };
        if let Some(r) = t.0.runners.borrow_mut().find(&|k, _| *k == key) {
            return Ok(r.clone());
        }
        let typed_msl;
        let msl = if typed {
            typed_msl = emit::kernel(&emit::Options { spirv: is_vulkan(), fast, uint32_inputs, uint32_tables, ..emit::Options::default() }, &self.packed)?;
            &typed_msl
        } else { &self.msl };
        let runner = Rc::new(Runner {
            identity: NEXT.fetch_add(1, Ordering::SeqCst),
            run: run::create(Some(&t.0.shared), &t.0.gpu, &t.0.pipelines, msl),
            stamp: Cell::new(0),
            output: RefCell::new(None),
        });
        t.0.created.set(t.0.created.get() + 1);
        t.0.runners.borrow_mut().add(key, runner.clone());
        Ok(runner)
    }
}

impl G::Kernel for HostKernel {
    fn run(&self, inputs: &phost::Inputs) -> Result<G::Value, Diagnostic> {
        let Some(t) = host_of(self.id).filter(live) else {
            return error("GPU host is closed or called from another domain.");
        };
        let runner = self.runner(&t, Some(inputs))?;
        *runner.output.borrow_mut() = None;
        if self.fast {
            admit(&t)?;
        }
        let t2 = t.clone();
        let index = &self.index; let count = inputs.count; let bound = move |i: i64| if index[i as usize] { iota(&t2, count) } else { None };
        let output = run::dispatch(&runner.run, &Options { wait: Some(!self.fast), ..Options::default() }, Some(&bound), None, inputs)?;
        if self.fast {
            record(&t, run::epoch(&output));
        }
        runner.stamp.set(runner.stamp.get() + 1);
        let v = gpu_value(&runner, &output);
        *runner.output.borrow_mut() = Some(output);
        Ok(v)
    }

    fn readback(&self, value: &G::Value) -> Result<Value, Diagnostic> {
        let Some(t) = host_of(self.id) else { return error("GPU output was closed or superseded.") };
        match output(&t, value)? {
            None => error("GPU output was closed or superseded."),
            Some(output) => run::readback(&output, None),
        }
    }

    fn planes(&self, value: &G::Value) -> Result<G::Planes, Diagnostic> {
        let Some(t) = host_of(self.id) else { return error("GPU output was closed or superseded.") };
        match current(&t, value) {
            None => error("GPU output was closed or superseded."),
            Some((_, output)) => run::keep(&output, None),
        }
    }
}

// ---- the resident fold
//
// A resident fold: from a region of 256 elements, a loop's regions and compactions are encoded
// level by level, committed without a wait, into buffers the fold owns; nothing is read until the
// loop ends. A region's value is a placeholder (a small array standing for its capacity, an upper
// bound) the table maps, physically, to its buffer and to the count cell the device wrote, so the
// next region's count (`array/range` of an array's count) is taken on the device. `finish` reads a
// count, then the array.
//
// A counted region's capacity is its slot's from earlier draws when it has one (no wait), else the
// bound. The kernel stops at its capacity and writes its count, so a count that reaches a capacity
// below the bound may have been cut: `finish` then raises `Resident_overflow` and the loop runs
// again, its regions from the first cut one on (`exact`) at their bounds. The device is
// deterministic, so the ones before it repeat their uncut counts. The slots have grown at close.
//
// After the loop (`settle`: its counts checked, nothing read), the arrays it kept may stay where
// they are for the rest of the draw: an `array/range` of a kept array's count is the device's
// index buffer counted by that array's cell (`range`), a later region over them runs here at its
// bound (no slot capacity, so nothing is cut), and a mark attribute over them is drawn by the
// site's `shown` runner, its grid and count record from the device count. `close`, at the draw's
// end, hands everything back.

/// the smallest power of two above `n`: a count that fits leaves room, so reaching it means a cut
fn above(n: i64) -> i64 {
    let mut c = 1;
    while c <= n {
        c *= 2;
    }
    c
}

/// a site keeps its first 64 ordinals' slots; a longer fold's later levels own theirs for one run
const SITE_SLOTS: usize = 64;

/// An outgrown index buffer goes when nothing can read it. A scope may still encode with one it
/// outgrew, so it is named at the next scope, by the newest dispatch then.
fn drain_retired(t: &T, queue: &Queue) {
    let newest = t.0.flight.borrow().last().copied().unwrap_or(0);
    let completed = b::completed_epoch(queue);
    let old = std::mem::take(&mut *t.0.retired_buffers.borrow_mut());
    let mut keep = vec![];
    for (buffer, epoch) in old.into_iter().rev() {
        let epoch = epoch.unwrap_or(newest);
        if completed >= epoch {
            destroy_owned(&IOTA_OWNER, &buffer);
        } else {
            keep.push((buffer, Some(epoch)));
        }
    }
    keep.reverse();
    *t.0.retired_buffers.borrow_mut() = keep;
}

struct Guard(i64);
impl Drop for Guard {
    fn drop(&mut self) {
        SCOPES.with(|s| s.borrow_mut().remove(&self.0));
    }
}

fn scope_of(g: &Guard) -> Option<Rc<Scope>> {
    SCOPES.with(|s| s.borrow().get(&g.0).cloned())
}

struct SState {
    table: Vec<Rc<Placeholder>>,
    epoch: i64,
    status_runs: Vec<run::T>,
    integers: Vec<(usize, Uint32Holder)>,
    floats: Vec<(usize, Arc<SharedPlanes>)>,
    on_close: Vec<Box<dyn FnOnce()>>,
    touched: Vec<Rc<Site>>,
    invoked: i64,
    exact: i64,
    cut: i64,
    extra_slots: Vec<Rc<HSlot>>,
    pending: Option<(Commands, ComputeEncoder)>,
    settled: bool,
    post: bool,
    /// the words written into a buffer an array/concat appends to, by buffer id
    appended: Vec<(i64, i64)>,
    durables: Vec<usize>,
    adopted: Vec<usize>,
    range_slot: Option<Rc<HSlot>>,
}

pub(crate) struct Scope {
    id: i64,
    t: T,
    uint32: bool,
    /// with `uint32`, its words are signed 32-bit Ints: [-2^31, 2^31 - 1] exact, else E_GPU_UINT32_RANGE
    signed: bool,
    queue: Queue,
    s: RefCell<SState>,
}

fn raw(v: &Value) -> Option<usize> {
    match v {
        Value::Floats(1..=4, a) => Some(Arc::as_ptr(a) as usize),
        Value::IntArray(a) => Some(Arc::as_ptr(a) as usize),
        _ => None,
    }
}

fn kind_of(v: &Value) -> i64 {
    match v {
        Value::Floats(width @ 1..=4, _) => *width as i64,
        _ => 4,
    }
}

fn float_array(v: &Value) -> Option<&Arc<Vec<f64>>> {
    match v {
        Value::Floats(1..=4, a) => Some(a),
        _ => None,
    }
}

fn same_buffer(a: &Buffer, b_: &Buffer) -> bool {
    b::buffer_id(a) == b::buffer_id(b_)
}

/// Every output an int array's placeholder reads back as uint32: each scalar integer register, not
/// only the first output (a resident fold's sibling maps dispatch as one multi-output kernel).
fn integer_outputs(packed: &Packed) -> Vec<i64> {
    let mut outputs = Vec::new();
    for (k, registers) in packed.outputs.iter().enumerate() {
        if let [slot] = registers.as_slice() {
            if flow_ir::core_ir::region::integer_register(&packed.code[*slot as usize]) {
                outputs.push(k as i64);
            }
        }
    }
    outputs
}

fn int_array_ty() -> Ty {
    Ty::Array(Box::new(Ty::Int))
}


/// What `planned`/`prepared` hand `invoke`: a region's inputs and its device bindings.
struct Level {
    count: Option<(Buffer, i64, i64)>,
    n: i64,
    held: bool,
    inputs: phost::Inputs,
    bound_arrays: Vec<Option<Buffer>>,
    bound_tables: Vec<Option<(Buffer, i64)>>,
}

impl Scope {
    fn pack_planes(&self, values: &[Value; 3]) -> Result<Option<Value>, Diagnostic> {
        for value in values {
            if self.find(value).is_none() && float_array(value).is_some_and(|a| a.iter().any(|x| !(*x as f32).is_finite())) { return Ok(None); }
        }
        let Some(planes) = values.iter().map(|v| self.held_or_adopted(v)).collect::<Option<Vec<_>>>() else { return Ok(None) };
        if planes.iter().any(|p| p.uint32 || p.kind != 1) { return Ok(None); }
        self.settle_inner()?;
        let count = self.count_of(&planes[0])?;
        for p in &planes[1..] {
            if self.count_of(p)? != count { return Err(value::fail("E_ARRAY_COUNT", "Mesh coordinate planes need the same element count.")); }
        }
        let slot = self.range_slot();
        if !self.s.borrow().extra_slots.iter().any(|s| Rc::ptr_eq(s, &slot)) { self.s.borrow_mut().extra_slots.push(slot.clone()); }
        let buffer = self.alloc(&slot, (count * 12).max(4))?;
        let made = self.placeholder(false, true, i64::MAX, None, &slot, &Ty::Array(Box::new(Ty::Vec3)), buffer.clone(), None, count);
        run::pack_planes(&self.queue, &self.t.0.pipelines, [&planes[0].buffer.0, &planes[1].buffer.0, &planes[2].buffer.0], &buffer.0, count)?;
        Ok(Some(made))
    }
    fn live_ok(&self) -> bool {
        live(&self.t)
    }

    // a level's command buffer and its one compute encoder, which orders its dispatches
    fn commands(&self) -> Result<(Commands, ComputeEncoder), Diagnostic> {
        self.s.borrow_mut().settled = false;
        if let Some(c) = &self.s.borrow().pending {
            return Ok(c.clone());
        }
        let c = gpu_r(b::begin_commands(&self.queue))?;
        match b::compute_encoder(&c) {
            Err(e) => {
                let _ = b::abandon(&c);
                Err(run::diagnostic(&e))
            }
            Ok(encoder) => {
                self.s.borrow_mut().pending = Some((c.clone(), encoder.clone()));
                Ok((c, encoder))
            }
        }
    }

    // committed per level without waiting, so the device runs a level while the host encodes the
    // next; at most two in flight (the queue admits three)
    fn flush(&self) -> Result<(), Diagnostic> {
        let Some((c, encoder)) = self.s.borrow_mut().pending.take() else { return Ok(()) };
        let _ = b::end_compute(&encoder);
        if let Err(d) = admit(&self.t) {
            // an index buffer filled in these commands was never written
            *self.t.0.iota_uint32.borrow_mut() = None;
            let _ = b::abandon(&c);
            return Err(d);
        }
        let r = gpu_r(b::commit(&c))?;
        record(&self.t, r.epoch);
        self.s.borrow_mut().epoch = r.epoch;
        Ok(())
    }

    fn lookup(&self, key: usize) -> Option<Rc<Placeholder>> {
        self.s.borrow().table.iter().rev().find(|p| p.key == key && !p.dead.get()).cloned()
    }

    fn find(&self, v: &Value) -> Option<Rc<Placeholder>> {
        match v {
            Value::Floats(1..=4, a) => {
                self.lookup(Arc::as_ptr(a) as usize).or_else(|| self.durable(v))
            }
            Value::IntArray(a) => self.lookup(Arc::as_ptr(a) as usize).or_else(|| self.durable(v)),
            _ => None,
        }
    }

    /// an array a device holds past its scope (`Eval::Device`), bound where it is
    fn durable(&self, v: &Value) -> Option<Rc<Placeholder>> {
        if !flow_eval::device::held_by_device(v) {
            return None;
        }
        // an integer array's words are signed ones: only an exact scope binds them
        let (key, (buffer, count, _, unpin), uint32) = match v {
            Value::IntArray(a) if self.signed => {
                let (buffer, whole, valid, unpin) = run::pin_words(&flow_ir::packed::host::integer_input(a)?, true)?;
                (Arc::as_ptr(a) as usize, (buffer, if a.len() == 2 { a[1] } else { whole }, valid, unpin), true)
            }
            _ => {
                let a = float_array(v)?;
                (Arc::as_ptr(a) as usize, run::pin(a)?, false)
            }
        };
        self.s.borrow_mut().on_close.push(Box::new(move || unpin()));
        let slot = self.adopt_slot();
        let p = Rc::new(Placeholder {
            key,
            hold: v.clone(),
            kind: if uint32 { 0 } else { kind_of(v) },
            uint32,
            buffer: (buffer.clone(), buffer_bytes(&buffer)),
            length: count,
            cell: None,
            slot,
            counted: false,
            known: true,
            clamped: i64::MAX,
            mul: 1,
            div: 1,
            owned: Cell::new(false),
            dead: Cell::new(false),
            origin: None,
        });
        let mut s = self.s.borrow_mut();
        s.table.push(p.clone());
        s.durables.push(p.key);
        Some(p)
    }

    /// the host's, kept across scopes: an adopted array's buffer serves the next draw's upload
    fn adopt_slot(&self) -> Rc<HSlot> {
        let slot = self.t.0.adopt_slot.clone();
        let mut s = self.s.borrow_mut();
        if !s.extra_slots.iter().any(|x| Rc::ptr_eq(x, &slot)) {
            s.extra_slots.push(slot.clone());
        }
        slot
    }

    /// a drawing's scope (`signed`) fills and ranges into the host's slot, kept across draws, so a
    /// warm frame creates no buffer for them; any other scope's is its own, spent with it
    fn range_slot(&self) -> Rc<HSlot> {
        if self.signed {
            return self.t.0.range_slot.clone();
        }
        let mut s = self.s.borrow_mut();
        s.range_slot.get_or_insert_with(|| new_slot(false)).clone()
    }

    /// A placeholder is an identity and a type: one element of its width, never read. Its length is
    /// `length` (`capacity`), so it costs no host memory by its size.
    fn placeholder(&self, counted: bool, known: bool, clamped: i64, origin: Option<(Buffer, i64, i64)>, slot: &Rc<HSlot>, ty: &Ty, buffer: (Buffer, i64), cell: Option<(Buffer, i64)>, length: i64) -> Value {
        let v = match ty {
            Ty::Array(inner) if **inner == Ty::Int => Value::IntArray(Arc::new(vec![0])),
            Ty::Array(inner) if **inner == Ty::Float => Value::Floats(1, Arc::new(vec![0.0])),
            Ty::Array(inner) if **inner == Ty::Vec2 => Value::Floats(2, Arc::new(vec![0.0; 2])),
            Ty::Array(inner) if **inner == Ty::Vec3 => Value::Floats(3, Arc::new(vec![0.0; 3])),
            _ => Value::Floats(4, Arc::new(vec![0.0; 4])),
        };
        let kind = match &v {
            Value::IntArray(_) => 0,
            other => kind_of(other),
        };
        let p = Rc::new(Placeholder {
            key: raw(&v).unwrap(),
            hold: v.clone(),
            kind,
            uint32: self.uint32 && *ty == int_array_ty(),
            buffer,
            length,
            cell,
            slot: slot.clone(),
            counted,
            known,
            clamped,
            mul: 1,
            div: 1,
            owned: Cell::new(true),
            dead: Cell::new(false),
            origin,
        });
        self.s.borrow_mut().table.push(p.clone());
        slot.held.borrow_mut().push(p);
        v
    }

    fn integer_source(&self, code: &[Instruction], source: i64) -> bool {
        self.uint32
            && code.iter().any(|c| match flow_shader::trace::flat(c).1 {
                Instruction::IntInput(i) => *i == source,
                _ => false,
            })
    }

    fn iota(&self, integer: Option<bool>, length: i64) -> Option<Buffer> {
        let integer = integer.unwrap_or(self.uint32);
        let t = &self.t;
        if !integer {
            return iota(t, length);
        }
        let old = t.0.iota_uint32.borrow().clone();
        if let Some((buffer, capacity)) = &old {
            if *capacity >= length {
                return Some(buffer.clone());
            }
        }
        // ponytail: the hint never shrinks; decay it if a workspace's counts fall for good
        let wanted = 1.max(length.max(2 * old.as_ref().map_or(0, |o| o.1)).max(t.0.iota_uint32_hint.get()));
        t.0.iota_uint32_hint.set(wanted);
        let device = device_of(t);
        // a buffer a dropped holder gave back (last frame's taken index buffer, say) before a new one
        let (buffer, capacity) = match run::recycled(&t.0.shared, &device, wanted * 4) {
            Some((buffer, bytes)) => (buffer, bytes / 4),
            None => {
                let size = run::size_class(wanted * 4);
                if size >= 1 << 20 {
                    run::admit(size).ok()?;
                    run::created(&t.0.shared, size);
                }
                let buffer = b::create_mapped_buffer(
                    &device,
                    BufferDescriptor { label: Some("Flow uint32 indices".to_owned()), size, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] },
                )
                .ok()?;
                t.0.resident_buffers.set(t.0.resident_buffers.get() + 1);
                (buffer, size / 4)
            }
        };
        // ordered before this scope's dispatches by its serial encoder: no wait
        if self.commands().and_then(|(_, encoder)| run::encode_fill(&encoder, &t.0.pipelines, None, &buffer, capacity)).is_err() {
            let _ = b::destroy_buffer(&buffer);
            return None;
        }
        mem::charge(&IOTA_OWNER, buffer_bytes(&buffer));
        if let Some((old, _)) = old {
            t.0.retired_buffers.borrow_mut().push((old, None));
        }
        *t.0.iota_uint32.borrow_mut() = Some((buffer.clone(), capacity));
        Some(buffer)
    }

    fn held_or_adopted(&self, v: &Value) -> Option<Rc<Placeholder>> {
        self.find(v).or_else(|| self.adopt(v))
    }

    fn bound(&self, v: &Value) -> Option<Buffer> {
        self.held_or_adopted(v).map(|p| p.buffer.0.clone())
    }

    /// a placeholder's elements, a capacity; its one-element block is never read
    fn capacity(&self, v: &Value) -> Option<i64> {
        self.find(v).map(|p| p.length)
    }

    fn bound_table_of(&self, v: &Value) -> Option<(Buffer, i64)> {
        self.held_or_adopted(v).map(|p| (p.buffer.0.clone(), p.length * 1.max(p.kind)))
    }

    fn give_back(&self, slot: &HSlot, (buffer, bytes): (Buffer, i64)) {
        slot.free.borrow_mut().entry(bytes).or_default().push(buffer);
    }

    /// a slot's buffer sizes repeat the last draw's; close hands them back
    fn alloc(&self, slot: &HSlot, bytes: i64) -> Result<(Buffer, i64), Diagnostic> {
        {
            let mut spare = slot.spare.borrow_mut();
            if let Some(at) = spare.iter().rposition(|(_, n)| *n == bytes) {
                return Ok(spare.remove(at));
            }
        }
        if let Some(bs) = slot.free.borrow_mut().get_mut(&bytes) {
            if !bs.is_empty() {
                return Ok((bs.pop().unwrap(), bytes));
            }
        }
        // a large buffer comes from what retained outputs gave back; sizes by class, so a mesh
        // whose count moves between frames finds one
        let large = bytes >= 1 << 20;
        let t = &self.t;
        let device = device_of(t);
        if large {
            if let Some((buffer, _)) = run::recycled(&t.0.shared, &device, bytes) {
                mem::charge(&RESIDENT_OWNER, resident_bytes(&buffer));
                return Ok((buffer, bytes));
            }
        }
        let size = if large { run::size_class(bytes) } else { bytes };
        if large {
            run::admit(size)?;
            run::created(&t.0.shared, size);
        }
        let buffer = gpu_r(b::create_mapped_buffer(&device, BufferDescriptor { label: Some("Flow resident".to_owned()), size, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] }))?;
        t.0.resident_buffers.set(t.0.resident_buffers.get() + 1);
        mem::charge(&RESIDENT_OWNER, resident_bytes(&buffer));
        Ok((buffer, bytes))
    }

    /// A large host array (a state the lane keeps, say) several regions read is uploaded once a
    /// scope and bound by each, rather than once a runner; it stays the host's: `finish` returns
    /// it as it is.
    fn adopt(&self, v: &Value) -> Option<Rc<Placeholder>> {
        let f = float_array(v)?;
        if f.len() < 1 << 16 {
            return None;
        }
        let slot = self.adopt_slot();
        let t = &self.t;
        // an array uploaded in an earlier scope is bound where it is: a host array never changes
        // under its Arc (a weak count refuses in-place writes), and its buffer is the cache's, so
        // no kernel takes it back as scratch
        let cached = {
            let mut cache = t.0.adopted_cache.borrow_mut();
            let (live, dead): (Vec<_>, Vec<_>) = std::mem::take(&mut *cache).into_iter().partition(|(w, _)| w.strong_count() > 0);
            *cache = live;
            for (_, x) in dead {
                self.give_back(&slot, x);
            }
            cache.iter().find(|(w, _)| w.upgrade().is_some_and(|a| Arc::ptr_eq(&a, f))).map(|(_, x)| x.clone())
        };
        let (b_, owned) = match cached {
            Some(x) => (x, false),
            None => {
                let b_ = self.alloc(&slot, 4 * f.len() as i64).ok()?;
                // a constant array (a fill's zero planes) is filled on the device, not uploaded
                let first = (f[0] as f32).to_bits();
                if f32::from_bits(first).is_finite() && f.iter().all(|x| (*x as f32).to_bits() == first) {
                    if !self.commands().and_then(|(_, encoder)| run::encode_float_fill(&encoder, &self.t.0.pipelines, &b_.0, f.len() as i64, &[f[0]])).is_ok() {
                        self.give_back(&slot, b_);
                        return None;
                    }
                    (b_, true)
                } else {
                    let mut bytes = vec![0u8; 4 * f.len()];
                    for (k, x) in f.iter().enumerate() {
                        bytes[4 * k..4 * k + 4].copy_from_slice(&(*x as f32).to_le_bytes());
                    }
                    if b::write_buffer(&b_.0, 0, &bytes).is_err() {
                        self.give_back(&slot, b_);
                        return None;
                    }
                    t.0.adopted_uploads.set(t.0.adopted_uploads.get() + 1);
                    t.0.adopted_uploaded_bytes.set(t.0.adopted_uploaded_bytes.get() + bytes.len() as i64);
                    // ponytail: the 8 latest uploads are kept, whatever their bytes; a byte cap if tables over 100 MB are adopted
                    let mut cache = t.0.adopted_cache.borrow_mut();
                    if cache.len() >= 8 {
                        let (_, x) = cache.remove(0);
                        self.give_back(&slot, x);
                    }
                    cache.push((Arc::downgrade(f), b_.clone()));
                    (b_, false)
                }
            }
        };
        let kind = kind_of(v);
        let p = Rc::new(Placeholder {
            key: Arc::as_ptr(f) as usize,
            hold: v.clone(),
            kind,
            uint32: false,
            buffer: b_,
            length: f.len() as i64 / kind,
            cell: None,
            slot: slot.clone(),
            counted: false,
            known: true,
            clamped: i64::MAX,
            mul: 1,
            div: 1,
            owned: Cell::new(owned),
            dead: Cell::new(false),
            origin: None,
        });
        let mut s = self.s.borrow_mut();
        s.table.push(p.clone());
        if owned {
            slot.held.borrow_mut().push(p.clone());
        }
        s.adopted.push(p.key);
        Some(p)
    }

    /// a region's kernel; `unfold` n runs it n passes in each lane (`Emit.kernel ~unfold`)
    fn emit(&self, unfold: Option<i64>, packed: &Packed) -> Result<Kernel, Diagnostic> {
        let base = emit::Options { spirv: is_vulkan(), fast: true, unfold, ..emit::Options::default() };
        if !self.uint32 {
            return emit::kernel(&base, packed);
        }
        let mut inputs: Vec<i64> = vec![];
        let mut tables: Vec<i64> = vec![];
        for code in packed.code.iter() {
            match flow_shader::trace::flat(code).1 {
                Instruction::IntInput(i) => inputs.push(*i),
                Instruction::ArrayPrepare(u, Ty::Array(inner)) if **inner == Ty::Int => tables.push(*u),
                _ => {}
            }
        }
        inputs.sort();
        inputs.dedup();
        tables.sort();
        tables.dedup();
        emit::kernel(
            &emit::Options { uint32_scalars: true, signed_words: self.signed, uint32_inputs: inputs, uint32_tables: tables, uint32_outputs: integer_outputs(packed), ..base },
            packed,
        )
    }

    fn site(&self, packed: &Packed) -> Rc<Site> {
        let code = packed.code.clone();
        let mode = self.uint32 as u8 + self.signed as u8;
        if let Some(site) = self.t.0.sites.borrow().peek(&|(c, u), _| Arc::ptr_eq(c, &code) && *u == mode) {
            return site.clone();
        }
        let ty = packed.term.ty();
        let types = [Ty::Array(Box::new(Ty::Int)), Ty::Array(Box::new(Ty::Float)), Ty::Array(Box::new(Ty::Vec2)), Ty::Array(Box::new(Ty::Vec3)), Ty::Array(Box::new(Ty::Vec4))];
        let run_ = if !flow_ir::packed::gpu_refusals(true, true, packed).is_empty() || !types.contains(ty) {
            None
        } else {
            self.emit(None, packed).ok().map(|msl| run::create(None, &self.t.0.gpu, &self.t.0.pipelines, &msl))
        };
        // the 32 most recent region sites are kept; a re-prepared drawing has new sites
        let site = Rc::new(Site {
            run: run_,
            unfolds: RefCell::new(vec![]),
            slots: RefCell::new(vec![]),
            shown: RefCell::new(None),
            plan: RefCell::new(None),
            shape: RefCell::new(None),
            inputs: RefCell::new(None),
        });
        self.t.0.sites.borrow_mut().add((code, mode), site.clone());
        site
    }

    /// Ping-pong: a site's slot is taken again in a run once none of the draw's locals holds an
    /// array of it (a fold's level reads the last one's: two slots take turns), so a fold of any
    /// depth keeps about two sets of buffers a site, each sized by the largest count it held. What
    /// the last use wrote stays the device's (queue order), its count cell stays its own (the
    /// loop's end checks every count), its buffers and arrays go back at once.
    fn held_by(locals: &[Value], key: usize) -> bool {
        locals.iter().any(|v| raw(v) == Some(key))
    }

    fn take(&self, slot: &HSlot) {
        let mut given: Vec<i64> = vec![];
        let held: Vec<Rc<Placeholder>> = std::mem::take(&mut *slot.held.borrow_mut());
        for p in held.iter().rev() {
            if !p.dead.get() {
                p.dead.set(true);
                if p.owned.get() && !given.contains(&b::buffer_id(&p.buffer.0)) {
                    given.push(b::buffer_id(&p.buffer.0));
                    slot.spare.borrow_mut().push(p.buffer.clone());
                }
            }
        }
        let spent: Vec<(Buffer, i64)> = std::mem::take(&mut *slot.spent.borrow_mut());
        let mut spare = slot.spare.borrow_mut();
        for x in spent.into_iter().rev() {
            spare.push(x);
        }
    }

    fn slot(&self, site: &Rc<Site>, locals: &[Value]) -> Rc<HSlot> {
        {
            let mut s = self.s.borrow_mut();
            if !s.touched.iter().any(|x| Rc::ptr_eq(x, site)) {
                s.touched.push(site.clone());
            }
        }
        let free = |s: &HSlot| s.held.borrow().iter().all(|p| p.dead.get() || !Self::held_by(locals, p.key));
        let found = site.slots.borrow().iter().find(|s| free(s)).cloned();
        match found {
            Some(s) => {
                self.take(&s);
                s
            }
            None => {
                let kept = site.slots.borrow().len() < SITE_SLOTS;
                let slot = new_slot(kept);
                if kept {
                    site.slots.borrow_mut().push(slot.clone());
                }
                slot
            }
        }
    }

    /// a placeholder's device count; after the loop nothing is cut: it is the one it was
    /// dispatched with
    fn placeholder_count(&self, p: &Placeholder) -> Option<(Buffer, i64, i64)> {
        match &p.origin {
            Some(origin) if self.s.borrow().post => Some(origin.clone()),
            _ => p.cell.as_ref().map(|(c, _)| (c.clone(), p.mul, p.div)),
        }
    }

    /// The device count bound of a region, if its sources allow the device to keep counts: `Err`
    /// when a source or uniform reads a placeholder in a form the host would evaluate over zeros or
    /// a capacity, or when two device counts bound it.
    fn device_count(&self, packed: &Packed) -> Result<Option<(Buffer, i64, i64)>, String> {
        let opaque = |shape: &phost::Shape| match shape {
            phost::Shape::Opaque(None) => !self.s.borrow().table.is_empty(),
            phost::Shape::Opaque(Some(vs)) => vs.iter().any(|v| self.find(v).is_some()),
            phost::Shape::Value(_) | phost::Shape::Range(..) => false,
        };
        let sources = phost::source_shapes(packed);
        if sources.iter().any(opaque) || phost::uniform_shapes(packed).iter().any(opaque) {
            return Err("a source or uniform reads a device array in a form the host would evaluate".to_owned());
        }
        let mut counts: Vec<(Buffer, i64, i64)> = vec![];
        for shape in &sources {
            match shape {
                phost::Shape::Value(v) => {
                    if let Some(c) = self.find(v).and_then(|p| self.placeholder_count(&p)) {
                        counts.push(c);
                    }
                }
                phost::Shape::Range(v, mul, div) => {
                    if let Some((c, m, d)) = self.find(v).and_then(|p| self.placeholder_count(&p)) {
                        counts.push((c, m * mul, d * div));
                    }
                }
                phost::Shape::Opaque(_) => {}
            }
        }
        match counts.split_first() {
            None => Ok(None),
            Some(((c, m, d), rest)) => {
                if rest.iter().all(|(c2, m2, d2)| same_buffer(c2, c) && m2 == m && d2 == d) {
                    Ok(Some((c.clone(), *m, *d)))
                } else {
                    Err("two device counts bound it".to_owned())
                }
            }
        }
    }

    fn unbound_table(&self, v: &Value) -> Value {
        if self.find(v).is_none() { v.clone() } else { Value::Floats(1, Arc::new(vec![])) }
    }

    /// A region's inputs, through its site's plan (from the arguments, nothing bound or
    /// evaluated).
    fn planned(&self, site: &Site, plan: &phost::Plan, args: &[Value], live_: &Frame) -> Result<Level, String> {
        let mut device: Option<(Buffer, i64, i64)> = None;
        let mut agree = true;
        let mut n = i64::MAX;
        let mut held_ = false;
        for s in 0..plan.plan_sources.len() {
            let (i, mul, div) = match &plan.plan_sources[s] {
                phost::Argument::Argument(i) => (*i, 1, 1),
                phost::Argument::ArgumentRange(i, mul, div) => (*i, *mul, *div),
            };
            let arg = &args[i as usize];
            match self.find(arg) {
                Some(p) => {
                    held_ = true;
                    n = n.min(p.length * mul / div);
                    if let Some((c, m, d)) = self.placeholder_count(&p) {
                        match &device {
                            None => device = Some((c, m * mul, d * div)),
                            Some((c2, m2, d2)) => {
                                if !(same_buffer(c2, &c) && *m2 == m * mul && *d2 == d * div) {
                                    agree = false;
                                }
                            }
                        }
                    }
                }
                None => n = n.min(value::array_length(arg).unwrap_or(0) as i64 * mul / div),
            }
        }
        let (widths, code) = site.shape.borrow().clone().unwrap();
        let mut slot_inputs = site.inputs.borrow_mut();
        if slot_inputs.is_none() {
            *slot_inputs = Some(phost::Inputs {
                arrays: vec![Arc::new(vec![]); plan.plan_sources.len()],
                uniforms: widths.iter().map(|w| vec![0.0; *w]).collect(),
                tables: vec![Arc::new(vec![]); plan.plan_uniforms.len()],
                frame: vec![0.0; code.len()],
                count: 0,
            });
        }
        let inputs = slot_inputs.as_mut().unwrap();
        let mut bound_arrays: Vec<Option<Buffer>> = vec![];
        for s in 0..plan.plan_sources.len() {
            bound_arrays.push(match &plan.plan_sources[s] {
                phost::Argument::Argument(i) => self.bound(&args[*i as usize]),
                phost::Argument::ArgumentRange(..) => self.iota(Some(self.integer_source(&code, s as i64)), n),
            });
        }
        let mut bound_tables: Vec<Option<(Buffer, i64)>> = vec![];
        for u in 0..plan.plan_uniforms.len() {
            bound_tables.push(self.bound_table_of(&args[plan.plan_uniforms[u] as usize]));
        }
        for s in 0..plan.plan_sources.len() {
            inputs.arrays[s] = match &plan.plan_sources[s] {
                phost::Argument::Argument(i) if self.find(&args[*i as usize]).is_none() => phost::floats(&args[*i as usize]).ok().flatten().unwrap_or_else(|| Arc::new(vec![])),
                _ => Arc::new(vec![]),
            };
        }
        for u in 0..plan.plan_uniforms.len() {
            let v = &args[plan.plan_uniforms[u] as usize];
            let table = if self.find(v).is_some() { Some(Arc::new(vec![])) } else { phost::floats(v).ok().flatten() };
            match table {
                Some(table) => inputs.tables[u] = table,
                None => {
                    inputs.tables[u] = Arc::new(vec![]);
                    let c = &mut inputs.uniforms[u];
                    match v {
                        Value::Vec2(x, y) => {
                            c[0] = *x;
                            c[1] = *y;
                        }
                        Value::Vec3(x, y, z) => {
                            c[0] = *x;
                            c[1] = *y;
                            c[2] = *z;
                        }
                        Value::Vec4(x, y, z, w) => {
                            c[0] = *x;
                            c[1] = *y;
                            c[2] = *z;
                            c[3] = *w;
                        }
                        v => c[0] = value::num(v).map_err(|d| d.message)?,
                    }
                }
            }
        }
        for (i, f) in &plan.plan_frames {
            inputs.frame[*i] = f(live_);
        }
        if !agree {
            return Err("two device counts bound it".to_owned());
        }
        Ok(Level { count: device, n: if n == i64::MAX { 0 } else { n }, held: held_, inputs: inputs.clone(), bound_arrays, bound_tables })
    }

    /// Bound and prepared: a region's device count, its count, whether it reads a placeholder, and
    /// its device inputs with the buffers bound in place of placeholders and ranges.
    fn prepared(&self, packed: &Packed, live_: &Frame) -> Result<Level, String> {
        let count = self.device_count(packed)?;
        let shapes = phost::source_shapes(packed);
        let indices = phost::index_sources(packed);
        // a range source is the device's index buffer: its length is known, it is not built
        let lengths: Arc<dyn Fn(usize) -> Option<usize> + Send + Sync> = {
            let (g, shapes) = (self.id, shapes.clone());
            Arc::new(move |i| match &shapes[i] {
                phost::Shape::Range(v, mul, div) => {
                    let scope = scope_by(g)?;
                    let n = match scope.find(v) {
                        Some(p) => p.length,
                        None => value::array_length(v).unwrap_or(0) as i64,
                    };
                    Some((n * mul / div) as usize)
                }
                _ => None,
            })
        };
        let capacity: Arc<dyn Fn(&Value) -> Option<usize> + Send + Sync> = {
            let g = self.id;
            Arc::new(move |v| scope_by(g).and_then(|s| s.capacity(v)).map(|c| c as usize))
        };
        let options = phost::PrepareOptions { index: self.uint32, lengths: Some(lengths), capacity: Some(capacity), ..Default::default() };
        let values = phost::prepare_values(packed, &options, live_).map_err(|d| d.message)?;
        let resident_input = |vs: &[Value]| vs.iter().any(|v| self.find(v).is_some());
        let resident = resident_input(&values.arrays) || resident_input(&values.uniforms);
        // a bound input is not uploaded: it needs no float copy, only its length
        let mut bound_inputs: Vec<Option<Buffer>> = vec![];
        for i in 0..values.arrays.len() {
            bound_inputs.push(match (self.bound(&values.arrays[i]), &shapes[i]) {
                (Some(b_), _) => Some(b_),
                (None, phost::Shape::Range(..)) => self.iota(Some(self.integer_source(&packed.code, i as i64)), values.count),
                (None, _) if self.uint32 && indices[i] => self.iota(Some(self.integer_source(&packed.code, i as i64)), values.count),
                (None, _) => None,
            });
        }
        let bound_tables: Vec<Option<(Buffer, i64)>> = values.uniforms.iter().map(|v| self.bound_table_of(v)).collect();
        let device_values = phost::ValueInputs {
            arrays: values.arrays.iter().enumerate().map(|(i, v)| if bound_inputs[i].is_none() { v.clone() } else { Value::Floats(1, Arc::new(vec![])) }).collect(),
            uniforms: values.uniforms.iter().map(|v| self.unbound_table(v)).collect(),
            frame: values.frame.clone(),
            count: values.count,
        };
        let inputs = phost::device_inputs(&device_values).map_err(|d| d.message)?;
        Ok(Level { count, n: values.count, held: resident, inputs, bound_arrays: bound_inputs, bound_tables })
    }

    /// `packed` is the region with its arguments unbound (`args` are them)
    fn off(&self, packed: &Packed, reason: &str) -> Option<Value> {
        let (_, path, _) = flow_ir::packed::site(packed);
        flow_ir::fallback::note(&flow_syntax::symbol::join_path(&path, "/"), reason);
        None
    }

    fn invoke(&self, steps: i64, source: Option<&Value>, packed: &Packed, args: &[Value], locals: &[Value], live_: &Frame) -> Result<Option<Value>, Diagnostic> {
        Ok(self.invoke_all(steps, source, packed, &[packed.term.ty().clone()], args, locals, live_)?.and_then(|vs| vs.into_iter().next()))
    }

    /// `invoke` of a region with an output for each of `tys` (siblings fused into one dispatch):
    /// a placeholder each, every one with its own copy of the count
    fn invoke_all(&self, steps: i64, source: Option<&Value>, packed: &Packed, tys: &[Ty], args: &[Value], locals: &[Value], live_: &Frame) -> Result<Option<Vec<Value>>, Diagnostic> {
        if !self.live_ok() || tys.len() != packed.outputs.len() || (steps > 0 && tys.len() != 1) {
            return Ok(None);
        }
        // a scalar result (a standalone vector's uniform region) is never a device kernel
        let types = [Ty::Array(Box::new(Ty::Int)), Ty::Array(Box::new(Ty::Float)), Ty::Array(Box::new(Ty::Vec2)), Ty::Array(Box::new(Ty::Vec3)), Ty::Array(Box::new(Ty::Vec4))];
        if !tys.iter().all(|ty| types.contains(ty)) {
            return Ok(None);
        }
        let site = self.site(packed);
        if site.plan.borrow().is_none() {
            *site.plan.borrow_mut() = Some(phost::plan(packed));
            *site.shape.borrow_mut() = Some((phost::view(packed).uniform_widths, packed.code.clone()));
        }
        // `steps` > 0: the kernel run that many passes in each lane, its output `steps` times as long
        let run_: Option<run::T> = if steps == 0 {
            site.run.clone()
        } else {
            let found = site.unfolds.borrow().iter().find(|(s, _)| *s == steps).map(|(_, r)| r.clone());
            match found {
                Some(r) => r,
                None => {
                    let r = self.emit(Some(steps), packed).ok().map(|msl| run::create(None, &self.t.0.gpu, &self.t.0.pipelines, &msl));
                    site.unfolds.borrow_mut().push((steps, r.clone()));
                    r
                }
            }
        };
        let Some(run_) = run_ else {
            if steps > 0 {
                return Ok(None);
            }
            let reason = match flow_ir::packed::gpu_refusals(true, true, packed).first() {
                Some(d) => d.message.clone(),
                None => "its output type or kernel is refused".to_owned(),
            };
            return Ok(self.off(packed, &reason).map(|v| vec![v]));
        };
        let plan = site.plan.borrow().clone();
        let level = match plan {
            Some(Some(plan)) => self.planned(&site, &plan, args, live_),
            _ => match phost::bind_arguments(packed, args, Some(false)) {
                Ok(bound) => self.prepared(&bound, live_),
                Err(d) => Err(d.message),
            },
        };
        let level = match level {
            Err(reason) => return Ok(self.off(packed, &format!("its inputs could not be bound: {reason}")).map(|v| vec![v])),
            Ok(level) => level,
        };
        let Level { mut count, n, held: held_, inputs, bound_arrays, bound_tables } = level;
        // A mapped head can still carry a count cell although its length is exact.
        // Unfold needs the exact length, not an indirect dispatch from that cell.
        if steps > 0 && source.and_then(|v| self.find(v)).is_some_and(|p| p.known && p.length == n) {
            count = None;
        }
        let slot = self.slot(&site, locals);
        let order = {
            let mut s = self.s.borrow_mut();
            s.invoked += 1;
            s.invoked - 1
        };
        // the device sizes a counted dispatch's grid (`Run.dispatch ~count`), but bounds double each
        // level while real counts level off, and placeholders and buffers are sized by the
        // capacity: the slot's from earlier draws, or without one, past 256K the device count is
        // read (4 bytes, one wait) and becomes the bound; the data stays put
        let (exact, post) = {
            let s = self.s.borrow();
            (s.exact, s.post)
        };
        let (n, clamped, length) = match &count {
            Some(_) if slot.capacity.get() > 0 && order < exact && !post => (n.min(slot.capacity.get()), if slot.capacity.get() < n { order } else { i64::MAX }, slot.capacity.get()),
            Some((cell, mul, div)) if n > 1 << 18 => {
                self.flush()?;
                let k = self.read_count(cell)?;
                let n = n.min(k * mul / div);
                (n, i64::MAX, n)
            }
            _ => (n, i64::MAX, n),
        };
        // from 256 elements a level's device work (no wait) is cheaper than the CPU's, and a site
        // whose deeper levels went to the device keeps its small ones there too, so its arrays never
        // cross. Native count widths and the actual device allocation limits bound storage.
        if n == 0 || !(held_ || n >= 256 || site.slots.borrow().iter().any(|s| s.capacity.get() > 0)) {
            return Ok(None);
        }
        if steps > 0 && (count.is_some() || length != n) {
            return Ok(None);
        }
        if steps > 0 {
            // the kernel's one input is the carried head: bound from its buffer or its array
            let ok = match source {
                Some(src) => match self.find(src) {
                    Some(p) => matches!(bound_arrays.first(), Some(Some(b_)) if same_buffer(b_, &p.buffer.0)),
                    None => matches!((float_array(src), inputs.arrays.first()), (Some(f), Some(a)) if Arc::ptr_eq(f, a)),
                },
                None => false,
            };
            if !ok {
                return Ok(None);
            }
        }
        let width = |ty: &Ty| match ty {
            Ty::Array(i) if **i == Ty::Vec2 => 2,
            Ty::Array(i) if **i == Ty::Vec3 => 3,
            Ty::Array(i) if **i == Ty::Vec4 => 4,
            _ => 1,
        };
        let widest = tys.iter().map(width).max().unwrap_or(1);
        if n < 0 || n > i32::MAX as i64 || length < 0 || length > u32::MAX as i64 / widest / steps.max(1) {
            return Err(value::fail("E_GPU", "Resident output exceeds native 32-bit GPU addressing."));
        }
        let Ok((commands, encoder)) = self.commands() else { return Ok(None) };
        // an output and a count cell for each type, all or none
        let mut made: Vec<((Buffer, i64), (Buffer, i64))> = vec![];
        for ty in tys {
            match (self.alloc(&slot, 4.max(length * width(ty) * 4 * 1.max(steps))), self.alloc(&slot, 24)) {
                (Ok(o), Ok(c)) => made.push((o, c)),
                (a, b_) => {
                    let failure = a.as_ref().err().or_else(|| b_.as_ref().err()).unwrap().clone();
                    for x in [a, b_].into_iter().flatten().chain(made.into_iter().flat_map(|(o, c)| [o, c])) {
                        self.give_back(&slot, x);
                    }
                    return Err(failure);
                }
            }
        }
        let (output, cell) = made[0].clone();
        let more: Vec<Buffer> = made[1..].iter().map(|(o, _)| o.0.clone()).collect();
        let reset_status = !self.s.borrow().status_runs.iter().any(|r| run_same(r, &run_));
        let mut inputs = inputs;
        inputs.count = n;
        let bound = |i: i64| bound_arrays.get(i as usize).cloned().flatten();
        let bound_table = |u: i64| bound_tables.get(u as usize).cloned().flatten();
        let opts = Options { uint32_cpu: self.uint32, commands: Some(commands), encoder: Some(encoder.clone()), reset_status: Some(reset_status), count: count.clone(), into: Some((output.0.clone(), cell.0.clone())), into_more: more, ..Options::default() };
        let r = run::dispatch(&run_, &opts, Some(&bound), Some(&bound_table), &inputs);
        if r.is_ok() { self.t.0.dispatches.set(self.t.0.dispatches.get() + 1); }
        // the outputs past the first take a copy of its count cell, encoded after the dispatch
        let r = r.and_then(|o| {
            for (_, c) in &made[1..] {
                run::copy(&encoder, &self.t.0.pipelines, &cell.0, 0, &c.0, 0, 6)?;
            }
            Ok(o)
        });
        match r {
            Err(d) => {
                for (o, c) in made {
                    self.give_back(&slot, o);
                    self.give_back(&slot, c);
                }
                Err(d)
            }
            Ok(_) if steps > 0 => {
                {
                    let mut s = self.s.borrow_mut();
                    s.settled = false;
                    if reset_status {
                        s.status_runs.push(run_);
                    }
                }
                slot.spent.borrow_mut().push(cell);
                Ok(Some(vec![self.placeholder(false, true, i64::MAX, None, &slot, &tys[0], output, None, length * steps)]))
            }
            Ok(_) => {
                {
                    let mut s = self.s.borrow_mut();
                    s.settled = false;
                    if reset_status {
                        s.status_runs.push(run_);
                    }
                }
                let known = length == n
                    && match &count {
                        None => true,
                        Some((cell_, mul, div)) => {
                            // a count followed from an array whose count is known
                            mul == div
                                && self.s.borrow().table.iter().any(|q| q.known && q.length == n && matches!(&q.cell, Some((c, _)) if same_buffer(c, cell_)))
                        }
                    };
                Ok(Some(made.into_iter().zip(tys).map(|((output, cell), ty)| self.placeholder(count.is_some(), known, clamped, count.clone(), &slot, ty, output, Some(cell), length)).collect()))
            }
        }
    }

    fn read_count(&self, cell: &Buffer) -> Result<i64, Diagnostic> {
        let epoch = self.s.borrow().epoch;
        let k = run::read_count(&self.queue, cell, epoch)?;
        self.t.0.count_bytes.set(self.t.0.count_bytes.get() + 4);
        Ok(k)
    }

    fn compact(&self, mask: &Value) -> Result<Option<Value>, Diagnostic> {
        let Some(p) = self.find(mask) else { return Ok(None) };
        if self.uint32 && !p.uint32 {
            return Ok(None);
        }
        let slot = p.slot.clone();
        let kept = self.alloc(&slot, 4.max(p.length * 4));
        let total = self.alloc(&slot, 4);
        let blocks = self.alloc(&slot, run::compact_blocks(p.length));
        let groups = self.alloc(&slot, 12);
        let give_all = |xs: [Result<(Buffer, i64), Diagnostic>; 4]| {
            for x in xs.into_iter().flatten() {
                self.give_back(&slot, x);
            }
        };
        let cx = self.commands();
        match (cx, &kept, &total, &blocks, &groups) {
            (Ok((commands, encoder)), Ok(kept_), Ok(total_), Ok(blocks_), Ok(groups_)) => {
                let r = run::compact(p.uint32, Some(&encoder), &commands, &self.t.0.pipelines, &p.buffer.0, p.length, p.cell.as_ref().map(|(c, _)| c), &kept_.0, &total_.0, &blocks_.0, &groups_.0);
                match r {
                    Err(_) => {
                        give_all([kept, total, blocks, groups]);
                        Ok(None)
                    }
                    Ok(()) => {
                        self.s.borrow_mut().settled = false;
                        {
                            let mut spent = slot.spent.borrow_mut();
                            spent.push(groups_.clone());
                            spent.push(blocks_.clone());
                        }
                        let v = self.placeholder(false, false, i64::MAX, None, &slot, &int_array_ty(), kept_.clone(), Some(total_.clone()), p.length);
                        self.flush()?;
                        Ok(Some(v))
                    }
                }
            }
            _ => {
                give_all([kept, total, blocks, groups]);
                Ok(None)
            }
        }
    }

    /// the device keeps integers as floats, so a retyped placeholder shares its buffer and count
    fn coerce(&self, ty: &Ty, v: &Value) -> Result<Option<Value>, Diagnostic> {
        match (ty, self.find(v)) {
            (Ty::Array(i), Some(p)) if **i == Ty::Float && !p.uint32 => {
                let nv = Value::Floats(1, Arc::new(vec![0.0]));
                let mut alias = p.copy();
                alias.key = raw(&nv).unwrap();
                alias.hold = nv.clone();
                alias.kind = 1;
                let alias = Rc::new(alias);
                self.s.borrow_mut().table.push(alias.clone());
                p.slot.held.borrow_mut().push(alias);
                Ok(Some(nv))
            }
            (Ty::Array(i), Some(p)) if **i == Ty::Int && p.uint32 => Ok(Some(v.clone())),
            _ => Ok(None),
        }
    }

    /// array/concat of two arrays the device holds or the host gives, both of counts known on the
    /// host: the result's words are the first's then the second's. When the first is the newest
    /// view of its buffer and the buffer has room, the second is appended there and the result
    /// shares the buffer (the first's prefix never changes); otherwise a buffer twice the length
    /// takes both. An appended history grows in place, a pass's new words at a time.
    fn concat(&self, a: &Value, bv: &Value) -> Result<Option<Value>, Diagnostic> {
        enum Side {
            Device(Rc<Placeholder>),
            Host(Arc<Vec<f64>>),
        }
        let width = |v: &Value| match v {
            Value::Floats(width @ 1..=4, _) => Some(*width as i64),
            _ => None,
        };
        let side = |v: &Value| -> Option<Side> {
            match self.find(v) {
                Some(p) if p.known && !p.uint32 => Some(Side::Device(p)),
                Some(_) => None,
                None => float_array(v).map(|f| Side::Host(f.clone())),
            }
        };
        let words = |s: &Side| match s {
            Side::Device(p) => p.length * 1.max(p.kind),
            Side::Host(f) => f.len() as i64,
        };
        let (Some(w), Some(w2), Some(sa), Some(sb)) = (width(a), width(bv), side(a), side(bv)) else { return Ok(None) };
        if w != w2 || (matches!(sa, Side::Host(_)) && matches!(sb, Side::Host(_))) {
            return Ok(None);
        }
        let (wa, wb) = (words(&sa), words(&sb));
        let total = wa + wb;
        let slot = match (&sa, &sb) {
            (Side::Device(p), _) | (_, Side::Device(p)) => p.slot.clone(),
            _ => unreachable!(),
        };
        let upload = |f: &Arc<Vec<f64>>| -> Option<Buffer> {
            let mut bytes = vec![0u8; 4 * 1.max(f.len())];
            for (k, v) in f.iter().enumerate() {
                bytes[4 * k..4 * k + 4].copy_from_slice(&(*v as f32).to_le_bytes());
            }
            let b_ = self.alloc(&slot, bytes.len() as i64).ok()?;
            if b::write_buffer(&b_.0, 0, &bytes).is_ok() {
                slot.spent.borrow_mut().push(b_.clone());
                Some(b_.0)
            } else {
                self.give_back(&slot, b_);
                None
            }
        };
        let source = |s: &Side| match s {
            Side::Device(p) => Some(p.buffer.0.clone()),
            Side::Host(f) => upload(f),
        };
        let in_place = match &sa {
            Side::Device(p) if p.owned.get() => {
                let id = b::buffer_id(&p.buffer.0);
                let used_ok = match self.s.borrow().appended.iter().find(|(i, _)| *i == id) {
                    Some((_, used)) => *used == wa,
                    None => true,
                };
                used_ok && p.buffer.1 >= total * 4
            }
            _ => false,
        };
        let Ok((_, encoder)) = self.commands() else { return Ok(None) };
        let ty = match w {
            1 => Ty::Array(Box::new(Ty::Float)),
            2 => Ty::Array(Box::new(Ty::Vec2)),
            3 => Ty::Array(Box::new(Ty::Vec3)),
            _ => Ty::Array(Box::new(Ty::Vec4)),
        };
        let copy = |src: Option<Buffer>, into: i64, words_: i64, dst: &Buffer| match src {
            None => false,
            Some(src) => run::copy(&encoder, &self.t.0.pipelines, &src, 0, dst, into, words_).is_ok(),
        };
        let finish = |buffer: (Buffer, i64)| {
            {
                let mut s = self.s.borrow_mut();
                let id = b::buffer_id(&buffer.0);
                s.appended.retain(|(i, _)| *i != id);
                s.appended.push((id, total));
                s.settled = false;
            }
            Some(self.placeholder(false, true, i64::MAX, None, &slot, &ty, buffer, None, total / w))
        };
        match &sa {
            Side::Device(p) if in_place => {
                if copy(source(&sb), wa, wb, &p.buffer.0) {
                    // the result shares the buffer: the slot gives it back once
                    p.owned.set(false);
                    Ok(finish(p.buffer.clone()))
                } else {
                    Ok(None)
                }
            }
            _ => {
                let Ok(out) = self.alloc(&slot, 4.max(2 * total * 4)) else { return Ok(None) };
                if copy(source(&sa), 0, wa, &out.0) && copy(source(&sb), wa, wb, &out.0) {
                    Ok(finish(out))
                } else {
                    self.give_back(&slot, out);
                    Ok(None)
                }
            }
        }
    }

    /// commits what is encoded and polls it: an error is left for the call that follows to report
    fn ready(&self) -> bool {
        if self.flush().is_err() {
            return true;
        }
        let epoch = self.s.borrow().epoch;
        epoch <= b::completed_epoch(&self.queue) || b::poll_through(&self.queue, epoch).unwrap_or(true)
    }

    fn count_of(&self, p: &Placeholder) -> Result<i64, Diagnostic> {
        match &p.cell {
            None => Ok(p.length),
            Some((cell, _)) => Ok(p.length.min(self.read_count(cell)? * p.mul / p.div)),
        }
    }

    /// the encoded work completed, no count cut and no status raised: what the host reads rests on
    /// every count before it, so a cut is checked before the statuses a cut array may raise
    fn settle_inner(&self) -> Result<(), Diagnostic> {
        if self.s.borrow().settled {
            return Ok(());
        }
        self.flush()?;
        let table: Vec<Rc<Placeholder>> = self.s.borrow().table.clone();
        let mut first = i64::MAX;
        for p in table.iter().rev() {
            if p.clamped < first && self.count_of(p)? == p.length {
                first = p.clamped;
            }
        }
        if first < i64::MAX {
            let mut s = self.s.borrow_mut();
            s.cut = first;
            s.status_runs.clear();
            return Err(core_exec::resident_overflow());
        }
        let (runners, epoch) = {
            let mut s = self.s.borrow_mut();
            (std::mem::take(&mut s.status_runs), s.epoch)
        };
        for r in runners.into_iter().rev() {
            run::private::check_status(&r, epoch)?;
        }
        self.s.borrow_mut().settled = true;
        Ok(())
    }

    fn host_of_planes(&self, holder: &SharedPlanes) -> Result<Value, Diagnostic> {
        let planes = holder.ops.materialise()?;
        let (count, width) = (holder.count as usize, holder.width as usize);
        let data: Vec<f64> = (0..count * width).map(|i| planes[i % width][i / width]).collect();
        let a = Arc::new(data);
        Ok(Value::Floats(width, a))
    }

    fn finish(&self, v: &Value) -> Result<Value, Diagnostic> {
        let Some(p) = self.find(v) else { return Ok(v.clone()) };
        if self.s.borrow().durables.contains(&p.key) {
            return Ok(flow_eval::device::thaw(v));
        }
        if self.s.borrow().adopted.contains(&p.key) {
            return Ok(v.clone());
        }
        self.settle_inner()?;
        let integer = self.s.borrow().integers.iter().find(|(k, _)| *k == p.key).map(|(_, h)| h.clone());
        let float = self.s.borrow().floats.iter().find(|(k, _)| *k == p.key).map(|(_, h)| h.clone());
        if let Some(holder) = integer {
            return Ok(Value::IntArray(Arc::new((holder.materialise)()?)));
        }
        if let Some(holder) = float {
            return self.host_of_planes(&holder);
        }
        let length = self.count_of(&p)?;
        let width = match v {
            Value::Floats(width, _) => *width as i64,
            _ => 1,
        };
        let epoch = self.s.borrow().epoch;
        let t = &self.t;
        if p.uint32 {
            let values = run::read_uint32(&self.queue, &p.buffer.0, epoch, length, self.signed)?;
            t.0.resident_payload_bytes.set(t.0.resident_payload_bytes.get() + length * 4);
            return Ok(Value::IntArray(Arc::new(values)));
        }
        let xs = run::read_floats(&self.queue, &p.buffer.0, epoch, length * width)?;
        t.0.resident_payload_bytes.set(t.0.resident_payload_bytes.get() + length * width * 4);
        Ok(match v {
            Value::IntArray(_) => Value::IntArray(Arc::new(xs.iter().map(|x| *x as i64).collect())),
            Value::Floats(width, _) => Value::Floats(*width, Arc::new(xs)),
            _ => Value::Floats(4, Arc::new(xs)),
        })
    }

    fn close(&self) {
        let _ = self.flush();
        let (epoch, finishers) = {
            let mut s = self.s.borrow_mut();
            (s.epoch, std::mem::take(&mut s.on_close))
        };
        if epoch > b::completed_epoch(&self.queue) {
            let _ = b::complete_through(&self.queue, epoch);
        }
        for f in finishers {
            f();
        }
        // the device is done, so reading a counted slot's count waits for nothing: it is the slot's
        // history (a cut count is its capacity, so the slot doubles; the run that follows records
        // the rest)
        let table: Vec<Rc<Placeholder>> = self.s.borrow().table.clone();
        for p in table.iter() {
            if let Some((cell, _)) = &p.cell {
                if p.counted {
                    if let Ok(n) = self.read_count(cell) {
                        p.slot.capacity.set(p.slot.capacity.get().max(above(n)));
                    }
                }
            }
        }
        self.t.0.showing.set(None);
        let (touched_slots, table) = {
            let mut s = self.s.borrow_mut();
            s.post = false;
            s.settled = false;
            let mut slots: Vec<Rc<HSlot>> = s.touched.iter().rev().flat_map(|site| site.slots.borrow().clone()).collect();
            slots.extend(std::mem::take(&mut s.extra_slots));
            s.touched.clear();
            s.invoked = 0;
            s.exact = s.cut;
            s.cut = i64::MAX;
            (slots, s.table.clone())
        };
        // back to their slots (a retyped placeholder shares its buffers), after what a slot kept
        // and this run did not take, a size it outgrew, is destroyed; a slot its site no longer
        // keeps destroys them
        let mut buffers: Vec<(Rc<HSlot>, (Buffer, i64))> = vec![];
        let mut add = |slot: &Rc<HSlot>, x: (Buffer, i64)| {
            if !buffers.iter().any(|(_, (b2, _))| same_buffer(b2, &x.0)) {
                buffers.push((slot.clone(), x));
            }
        };
        for s in touched_slots.iter() {
            for x in s.spent.borrow().iter().rev().chain(s.spare.borrow().iter().rev()) {
                add(s, x.clone());
            }
        }
        for p in table.iter().rev() {
            if !p.owned.get() {
                continue;
            }
            if p.dead.get() {
                if let Some(c) = &p.cell {
                    add(&p.slot, c.clone());
                }
            } else {
                add(&p.slot, p.buffer.clone());
                if let Some(c) = &p.cell {
                    add(&p.slot, c.clone());
                }
            }
        }
        let mut purged: Vec<Rc<HSlot>> = vec![];
        for (s, _) in buffers.iter().rev() {
            if s.kept.get() && !purged.iter().any(|x| Rc::ptr_eq(x, s)) {
                purged.push(s.clone());
            }
        }
        purged.iter().for_each(|s| purge(s));
        for (slot, b_) in buffers.into_iter().rev() {
            if slot.kept.get() {
                self.give_back(&slot, b_);
            } else {
                destroy_resident(&b_.0);
            }
        }
        for s in touched_slots.iter() {
            s.held.borrow_mut().clear();
            s.spent.borrow_mut().clear();
            s.spare.borrow_mut().clear();
        }
        let mut s = self.s.borrow_mut();
        s.durables.clear();
        s.table.clear();
        s.integers.clear();
        s.floats.clear();
    }

    /// after the loop: `array/range` of a kept array's count over `div` is the index buffer,
    /// counted by that array's cell
    fn range(&self, ty: &Ty, v: &Value, div: i64, length: i64) -> Result<Option<Value>, Diagnostic> {
        let integer = *ty == int_array_ty();
        if self.uint32 && !(integer || *ty == Ty::Array(Box::new(Ty::Float))) {
            return Ok(None);
        }
        let found = self.find(v);
        if let Value::Int(n) = v {
            if (self.uint32 || !integer) && *n >= 0 && div > 0 && length == n / div {
                let Some(buffer) = self.iota(Some(self.uint32 && integer), length) else { return Ok(None) };
                let slot = self.range_slot();
                {
                    let mut s = self.s.borrow_mut();
                    if !s.extra_slots.iter().any(|x| Rc::ptr_eq(x, &slot)) {
                        s.extra_slots.push(slot.clone());
                    }
                }
                let value = self.placeholder(false, false, i64::MAX, None, &slot, ty, (buffer, 0), None, length);
                // the placeholder's table entry is a copy that is not owned
                let mut s = self.s.borrow_mut();
                let head = s.table.pop().unwrap();
                let copy = head.copy();
                copy.owned.set(false);
                s.table.push(Rc::new(copy));
                return Ok(Some(value));
            }
        }
        match found {
            Some(p) if self.s.borrow().post && p.cell.is_some() && div > 0 => {
                let Some(buffer) = self.iota(Some(self.uint32 && integer), length) else { return Ok(None) };
                let (kind, a) = match ty {
                    Ty::Array(i) if **i == Ty::Float => (1, Value::Floats(1, Arc::new(vec![0.0]))),
                    _ => (0, Value::IntArray(Arc::new(vec![0]))),
                };
                let mut copy = p.copy();
                copy.key = raw(&a).unwrap();
                copy.hold = a.clone();
                copy.kind = kind;
                copy.uint32 = self.uint32 && integer;
                copy.buffer = (buffer, 0);
                copy.length = length;
                copy.counted = false;
                copy.clamped = i64::MAX;
                copy.div = p.div * div;
                copy.owned.set(false);
                copy.dead.set(false);
                copy.origin = p.origin.as_ref().map(|(c, m, d)| (c.clone(), *m, d * div));
                self.s.borrow_mut().table.push(Rc::new(copy));
                Ok(Some(a))
            }
            _ => Ok(None),
        }
    }

    fn fill(&self, ty: &Ty, length: i64, v: &Value) -> Result<Option<Value>, Diagnostic> {
        let row = match (ty, v) {
            (Ty::Array(t), Value::Float(x)) if **t == Ty::Float => vec![*x],
            (Ty::Array(t), Value::Vec2(x,y)) if **t == Ty::Vec2 => vec![*x,*y],
            (Ty::Array(t), Value::Vec3(x,y,z)) if **t == Ty::Vec3 => vec![*x,*y,*z],
            (Ty::Array(t), Value::Vec4(x,y,z,w)) if **t == Ty::Vec4 => vec![*x,*y,*z,*w],
            _ => Vec::new(),
        };
        if !row.is_empty() && length >= 0 && length <= u32::MAX as i64 / row.len() as i64 && row.iter().all(|x| (*x as f32).is_finite()) {
            let slot = self.range_slot();
            if !self.s.borrow().extra_slots.iter().any(|s| Rc::ptr_eq(s, &slot)) {
                self.s.borrow_mut().extra_slots.push(slot.clone());
            }
            let buffer = self.alloc(&slot, (length * row.len() as i64 * 4).max(4))?;
            let result = self.commands().and_then(|(_, encoder)| run::encode_float_fill(&encoder, &self.t.0.pipelines, &buffer.0, length, &row));
            return match result {
                Ok(()) => Ok(Some(self.placeholder(false, true, i64::MAX, None, &slot, ty, buffer, None, length))),
                Err(e) => { slot.spent.borrow_mut().push(buffer); Err(e) }
            };
        }
        match (ty, v) {
            (Ty::Array(i), Value::Int(x)) if **i == Ty::Int && self.uint32 && *x >= 0 && *x <= 4294967295 && length >= 0 && length <= u32::MAX as i64 => {
                let slot = self.range_slot();
                {
                    let mut s = self.s.borrow_mut();
                    if !s.extra_slots.iter().any(|y| Rc::ptr_eq(y, &slot)) {
                        s.extra_slots.push(slot.clone());
                    }
                }
                let buffer = self.alloc(&slot, 4.max(length * 4))?;
                let r = self.commands().and_then(|(_, encoder)| run::encode_fill(&encoder, &self.t.0.pipelines, Some(*x), &buffer.0, length));
                match r {
                    Ok(()) => Ok(Some(self.placeholder(false, false, i64::MAX, None, &slot, ty, buffer, None, length))),
                    Err(e) => {
                        slot.spent.borrow_mut().push(buffer);
                        Err(e)
                    }
                }
            }
            _ => Ok(None),
        }
    }

    /// A mark attribute over kept arrays, drawn by its site's `shown` runner: bound to their
    /// buffers, its grid and count record from their device count, committed after the fold's last
    /// level. `None`: it reads none; `Some(None)`: it reads one but cannot run here.
    fn show(&self, packed: &Packed, live_: &Frame) -> Option<Option<G::Value>> {
        let held_ = |s: &phost::Shape| match s {
            phost::Shape::Value(v) | phost::Shape::Range(v, _, _) => self.find(v).is_some(),
            phost::Shape::Opaque(_) => false,
        };
        let sources = phost::source_shapes(packed);
        if !sources.iter().any(held_) && !phost::uniform_shapes(packed).iter().any(held_) {
            return None;
        }
        if !self.live_ok() || !self.s.borrow().post {
            return Some(None);
        }
        let site = self.site(packed);
        Some((|| -> Option<G::Value> {
            site.run.as_ref()?;
            let count = self.device_count(packed).ok()?;
            let lengths: Arc<dyn Fn(usize) -> Option<usize> + Send + Sync> = {
                let (g, shapes) = (self.id, sources.clone());
                Arc::new(move |i| match &shapes[i] {
                    phost::Shape::Range(v, mul, div) => {
                        let scope = scope_by(g)?;
                        let n = match scope.find(v) {
                            Some(p) => p.length,
                            None => value::array_length(v).unwrap_or(0) as i64,
                        };
                        Some((n * mul / div) as usize)
                    }
                    _ => None,
                })
            };
            let capacity: Arc<dyn Fn(&Value) -> Option<usize> + Send + Sync> = {
                let g = self.id;
                Arc::new(move |v| scope_by(g).and_then(|s| s.capacity(v)).map(|c| c as usize))
            };
            let indices = phost::index_sources(packed);
            let values = phost::prepare_values(packed, &phost::PrepareOptions { index: self.uint32, lengths: Some(lengths), capacity: Some(capacity), ..Default::default() }, live_).ok()?;
            if values.count == 0 {
                return None;
            }
            let t = &self.t;
            let runner = {
                let existing = site.shown.borrow().clone();
                match existing {
                    Some(r) => Some(r),
                    None => {
                        let msl = self.emit(None, packed).ok()?;
                        let runner = Rc::new(Runner {
                            identity: NEXT.fetch_add(1, Ordering::SeqCst),
                            run: run::create(Some(&t.0.shared), &t.0.gpu, &t.0.pipelines, &msl),
                            stamp: Cell::new(0),
                            output: RefCell::new(None),
                        });
                        t.0.created.set(t.0.created.get() + 1);
                        *site.shown.borrow_mut() = Some(runner.clone());
                        Some(runner)
                    }
                }
            };
            let mut bound_inputs: Vec<Option<Buffer>> = vec![];
            for i in 0..values.arrays.len() {
                bound_inputs.push(match (self.bound(&values.arrays[i]), &sources[i]) {
                    (Some(b_), _) => Some(b_),
                    (None, phost::Shape::Range(..)) => self.iota(Some(self.integer_source(&packed.code, i as i64)), values.count),
                    (None, _) if self.uint32 && indices[i] => self.iota(Some(self.integer_source(&packed.code, i as i64)), values.count),
                    (None, _) => None,
                });
            }
            let bound_tables: Vec<Option<(Buffer, i64)>> = values.uniforms.iter().map(|v| self.bound_table_of(v)).collect();
            let device_values = phost::ValueInputs {
                arrays: values.arrays.iter().enumerate().map(|(i, v)| if bound_inputs[i].is_none() { v.clone() } else { Value::Floats(1, Arc::new(vec![])) }).collect(),
                uniforms: values.uniforms.iter().map(|v| self.unbound_table(v)).collect(),
                frame: values.frame.clone(),
                count: values.count,
            };
            let inputs = phost::device_inputs(&device_values).ok()?;
            let runner = runner?;
            self.flush().ok()?;
            admit(t).ok()?;
            *runner.output.borrow_mut() = None;
            let bound = |i: i64| bound_inputs.get(i as usize).cloned().flatten();
            let bound_table = |u: i64| bound_tables.get(u as usize).cloned().flatten();
            let output = run::dispatch(&runner.run, &Options { wait: Some(false), count, uint32_cpu: self.uint32, ..Options::default() }, Some(&bound), Some(&bound_table), &inputs).ok()?;
            record(t, run::epoch(&output));
            runner.stamp.set(runner.stamp.get() + 1);
            let v = gpu_value(&runner, &output);
            *runner.output.borrow_mut() = Some(output);
            Some(v)
        })())
    }

    fn count_value(&self, v: &Value) -> Result<Option<i64>, Diagnostic> {
        match self.find(v) {
            None => Ok(None),
            Some(p) => {
                self.settle_inner()?;
                Ok(Some(self.count_of(&p)?))
            }
        }
    }

    fn owner(&self) -> run::T {
        let t = &self.t;
        if let Some(r) = &*t.0.ownership.borrow() {
            return r.clone();
        }
        let r = run::create(Some(&t.0.shared), &t.0.gpu, &t.0.pipelines, kernels::indices());
        t.0.created.set(t.0.created.get() + 1);
        *t.0.ownership.borrow_mut() = Some(r.clone());
        r
    }

    fn detach(&self, p: &Placeholder) {
        let t = &self.t;
        let buffer = &p.buffer.0;
        // the holder's now: `flow_gpu.held` charges it
        if p.owned.get() {
            mem::release(&RESIDENT_OWNER, resident_bytes(buffer));
        }
        let mut adopted = false;
        if matches!(&*t.0.iota.borrow(), Some((x, _)) if same_buffer(x, buffer)) {
            *t.0.iota.borrow_mut() = None;
            adopted = true;
        }
        if matches!(&*t.0.iota_uint32.borrow(), Some((x, _)) if same_buffer(x, buffer)) {
            *t.0.iota_uint32.borrow_mut() = None;
            adopted = true;
        }
        if t.0.retired_buffers.borrow().iter().any(|(x, _)| same_buffer(x, buffer)) {
            adopted = true;
        }
        t.0.retired_buffers.borrow_mut().retain(|(x, _)| !same_buffer(x, buffer));
        if adopted {
            mem::release(&IOTA_OWNER, buffer_bytes(buffer));
        }
        let mut all: Vec<Rc<Placeholder>> = self.s.borrow().table.clone();
        all.extend(p.slot.held.borrow().iter().cloned());
        for alias in all {
            if same_buffer(&alias.buffer.0, buffer) {
                if alias.owned.get() {
                    if let Some(cell) = alias.cell.clone() {
                        let a2 = alias.clone();
                        self.s.borrow_mut().on_close.push(Box::new(move || a2.slot.spent.borrow_mut().push(cell)));
                    }
                }
                alias.owned.set(false);
                alias.slot.held.borrow_mut().retain(|q| !same_buffer(&q.buffer.0, buffer));
            }
        }
    }

    fn retain_uint32(&self, value_: &Value) -> Option<Uint32Holder> {
        if !live(&self.t) {
            return None;
        }
        let key = raw(value_)?;
        if let Some((_, holder)) = self.s.borrow().integers.iter().find(|(k, _)| *k == key) {
            return Some(holder.clone());
        }
        let p = self.find(value_).filter(|p| p.uint32 && !self.s.borrow().durables.contains(&p.key))?;
        self.settle_inner().ok()?;
        let aliases: Vec<(Rc<Placeholder>, i64)> = {
            let table = self.s.borrow().table.clone();
            let mut v = vec![];
            for a in table.iter().rev().filter(|a| !a.dead.get() && same_buffer(&a.buffer.0, &p.buffer.0)) {
                v.push((a.clone(), self.count_of(a).ok()?));
            }
            v
        };
        let length = aliases.iter().map(|(_, c)| *c).fold(0, i64::max);
        let mut holder = run::adopt_uint32(&self.owner(), &p.buffer.0, length).ok()?;
        if self.signed {
            // the words are two's complement: the holder's reads (and its views') are signed
            let read = holder.materialise.clone();
            holder.materialise = Arc::new(move || read().map(|ws| ws.into_iter().map(|w| w as u32 as i32 as i64).collect()));
        }
        if let Some((_, _, _, unpin)) = (holder.pin)() {
            self.s.borrow_mut().on_close.push(Box::new(move || unpin()));
        }
        let mut views: Vec<(i64, Uint32Holder)> = vec![];
        let mut view = |count: i64| -> Uint32Holder {
            if let Some((_, v)) = views.iter().find(|(c, _)| *c == count) {
                return v.clone();
            }
            let view = if aliases.iter().all(|(_, n)| *n == length) {
                holder.clone()
            } else {
                run::uint32_view(&holder, count)
            };
            views.push((count, view.clone()));
            view
        };
        let made: Vec<(usize, Uint32Holder)> = aliases.iter().map(|(a, count)| (a.key, view(*count))).collect();
        {
            let mut s = self.s.borrow_mut();
            let mut ints = made;
            ints.extend(std::mem::take(&mut s.integers));
            s.integers = ints;
        }
        self.detach(&p);
        self.s.borrow().integers.iter().find(|(k, _)| *k == p.key).map(|(_, h)| h.clone())
    }

    fn retain_float(&self, value_: &Value) -> Option<G::Planes> {
        // Aliases receive thin views over the same non-Clone plane owner.
        if !live(&self.t) {
            return None;
        }
        let key = raw(value_)?;
        if let Some((_, holder)) = self.s.borrow().floats.iter().find(|(k, _)| *k == key) {
            return Some(share_planes(holder));
        }
        let p = self.find(value_).filter(|p| !p.uint32 && p.kind > 0 && !self.s.borrow().durables.contains(&p.key) && !self.s.borrow().adopted.contains(&p.key))?;
        self.settle_inner().ok()?;
        let aliases: Vec<Rc<Placeholder>> = self.s.borrow().table.iter().rev().filter(|a| !a.dead.get() && same_buffer(&a.buffer.0, &p.buffer.0)).cloned().collect();
        let holder = run::adopt_float(&self.owner(), &p.buffer.0, self.count_of(&p).ok()?, p.kind).ok()?;
        if let Some(Value::Floats(1..=4, a)) = &holder.resident {
            if let Some((_, _, _, unpin)) = run::pin(a) {
                self.s.borrow_mut().on_close.push(Box::new(move || unpin()));
            }
        }
        let G::Planes { count, width, resident, ops } = holder;
        let holder = Arc::new(SharedPlanes { count, width, resident, ops: Arc::from(ops) });
        {
            let mut s = self.s.borrow_mut();
            let mut fl: Vec<(usize, Arc<SharedPlanes>)> = aliases.iter().map(|a| (a.key, holder.clone())).collect();
            fl.extend(std::mem::take(&mut s.floats));
            s.floats = fl;
        }
        self.detach(&p);
        Some(share_planes(&holder))
    }

    /// element `i` of a placeholder, read alone: its words only, never its array
    fn nth(&self, v: &Value, i: i64) -> Result<Option<Value>, Diagnostic> {
        let Some(p) = self.find(v) else { return Ok(None) };
        let held = |keys: &Vec<usize>| keys.contains(&p.key);
        // a held array is settled and complete: its words are read with no wait on this scope's work
        let durable = held(&self.s.borrow().durables);
        if (durable && !p.uint32) || held(&self.s.borrow().adopted) || (p.kind == 0 && !p.uint32) {
            return Ok(None);
        }
        if !durable {
            self.settle_inner()?;
        }
        if i < 0 || i >= self.count_of(&p)? {
            return Ok(None);
        }
        let width = p.kind.max(1);
        let epoch = self.s.borrow().epoch;
        if !durable && epoch > b::completed_epoch(&self.queue) {
            gpu_r(b::complete_through(&self.queue, epoch))?;
        }
        let bytes = gpu_r(b::read_buffer(&p.buffer.0, i * width * 4, width * 4))?;
        let word = |k: usize| u32::from_le_bytes(bytes[4 * k..4 * k + 4].try_into().unwrap());
        let t = &self.t;
        t.0.resident_payload_bytes.set(t.0.resident_payload_bytes.get() + width * 4);
        let f = |k: usize| f32::from_bits(word(k)) as f64;
        Ok(Some(match (p.uint32, width) {
            (true, _) => Value::Int(if self.signed { word(0) as i32 as i64 } else { word(0) as i64 }),
            (_, 1) => Value::Float(f(0)),
            (_, 2) => Value::Vec2(f(0), f(1)),
            (_, 3) => Value::Vec3(f(0), f(1), f(2)),
            _ => Value::Vec4(f(0), f(1), f(2), f(3)),
        }))
    }

    /// a state step's array stays on the device: its holder lives as long as the placeholder a
    /// state stores (`Eval::Device`), read to the host only when host code asks
    fn keep(&self, v: &Value) -> Result<Option<Value>, Diagnostic> {
        let Some(p) = self.find(v) else { return Ok(None) };
        if self.s.borrow().durables.contains(&p.key) {
            return Ok(Some(v.clone()));
        }
        if self.s.borrow().adopted.contains(&p.key) {
            return Ok(None);
        }
        if p.uint32 {
            let Some(mut holder) = self.retain_uint32(v).filter(|_| self.signed) else { return Ok(None) };
            let Some(placeholder) = holder.resident.take() else { return Ok(None) };
            let h = Arc::new(holder);
            // ponytail: the reader cannot fail, as the float one; a fallible reader in flow_eval
            let read: Arc<dyn Fn() -> Value + Send + Sync> = Arc::new(move || Value::IntArray(Arc::new((h.materialise)().expect("a state's device array is read while its holder lives"))));
            flow_eval::device::add(&placeholder, read)?;
            return Ok(Some(placeholder));
        }
        let Some(mut holder) = self.retain_float(v) else { return Ok(None) };
        // The registry's reader must not own its own weakly keyed placeholder.
        let Some(placeholder) = holder.resident.take() else { return Ok(None) };
        // a host read copies: the state's buffer stays its, for the next step to bind
        let buffer = p.buffer.0.clone();
        let (count, width) = (holder.count, holder.width);
        let id = self.t.0.id;
        let read: Arc<dyn Fn() -> Value + Send + Sync> = {
            let h = Arc::new(holder);
            let slot_key = buffer_id_of(&buffer);
            Arc::new(move || {
                // the holder's planes carry the values; the buffer view is only valid on the
                // creating thread (a CPU frame on a worker reads the planes the holder kept)
                let _ = (id, slot_key);
                // ponytail: `Device::add`'s reader cannot fail; the holder lives as long as the placeholder, so an Err is a broken invariant. Upgrade: a fallible reader in flow_eval.
                let planes = h.materialise().expect("a state's device array is read while its holder lives");
                let (c, w) = (count as usize, width as usize);
                let xs: Vec<f64> = (0..c * w).map(|i| planes[i % w][i / w]).collect();
                let a = Arc::new(xs);
                Value::Floats(w, a)
            })
        };
        flow_eval::device::add(&placeholder, read)?;
        Ok(Some(placeholder))
    }
}

fn buffer_id_of(buffer: &Buffer) -> i64 {
    b::buffer_id(buffer)
}

fn run_same(a: &run::T, b_: &run::T) -> bool {
    run::same(a, b_)
}

/// A retained float array, shared by every alias. A view holds `ops` only, never `resident`: the
/// registry's reader (which holds a view) must not own the placeholder it is keyed by.
struct SharedPlanes {
    count: i64,
    width: i64,
    resident: Option<Value>,
    ops: Arc<dyn G::PlanesOps>,
}

struct SharedOps(Arc<dyn G::PlanesOps>);

impl G::PlanesOps for SharedOps {
    fn materialise(&self) -> Result<Vec<Vec<f64>>, Diagnostic> { self.0.materialise() }
    fn settle(&self) -> Result<(), Diagnostic> { self.0.settle() }
    fn release(&self, held: bool) { self.0.release(held) }
    fn bounds(&self) -> Option<Vec<f64>> { self.0.bounds() }
}

fn share_planes(holder: &SharedPlanes) -> G::Planes {
    G::Planes { count: holder.count, width: holder.width, resident: holder.resident.clone(), ops: Box::new(SharedOps(holder.ops.clone())) }
}

#[cfg(test)]
#[test]
fn a_shared_view_does_not_own_the_placeholder() {
    let placeholder = Arc::new(vec![0.0]);
    let weak = Arc::downgrade(&placeholder);
    let holder = SharedPlanes { count: 1, width: 1, resident: Some(Value::Floats(1, placeholder)), ops: Arc::new(G::ReadPlanes(Arc::new(vec![vec![0.0]]))) };
    let mut view = share_planes(&holder);
    // what `keep` does before it registers a reader holding the view
    view.resident.take();
    drop(holder);
    assert!(weak.upgrade().is_none());
    assert_eq!(view.materialise().unwrap(), vec![vec![0.0]]);
}

fn scope_by(id: i64) -> Option<Rc<Scope>> {
    SCOPES.with(|s| s.borrow().get(&id).cloned())
}

fn new_scope(t: &T, uint32: bool, signed: bool) -> (Rc<Scope>, Arc<Guard>) {
    let queue = queue_of(t);
    if !t.0.retired_buffers.borrow().is_empty() {
        drain_retired(t, &queue);
    }
    let id = NEXT_SCOPE.fetch_add(1, Ordering::SeqCst);
    let scope = Rc::new(Scope {
        id,
        t: t.clone(),
        uint32,
        signed,
        queue,
        s: RefCell::new(SState {
            table: vec![],
            epoch: 0,
            status_runs: vec![],
            integers: vec![],
            floats: vec![],
            on_close: vec![],
            touched: vec![],
            invoked: 0,
            exact: i64::MAX,
            cut: i64::MAX,
            extra_slots: vec![],
            pending: None,
            settled: false,
            post: false,
            appended: vec![],
            durables: vec![],
            adopted: vec![],
            range_slot: None,
        }),
    });
    SCOPES.with(|s| s.borrow_mut().insert(id, scope.clone()));
    (scope, Arc::new(Guard(id)))
}

/// A resident fold on this host: keeps a loop's region arrays on the device and reads them back
/// as the loop ends. One per drawing. Each region site's calls keep their buffers, placeholders
/// and counts on the host between drawings (64 calls a site, 32 sites) and size its arrays by
/// them; a count that outgrew one reruns the loop exactly. `uint32` uses exact unsigned words for
/// integer arrays and captures, with device-generated integer ranges.
pub fn resident_scope(uint32: bool, _fast: bool, t: &T) -> HostResidency {
    resident_words(uint32, false, t)
}

/// A drawing's resident scope: its integer arrays and captures are signed 32-bit words on the
/// device, exact in [-2^31, 2^31 - 1]; a value outside it is E_GPU_UINT32_RANGE, never rounded.
pub fn resident_exact(t: &T) -> HostResidency {
    resident_words(true, true, t)
}

fn resident_words(uint32: bool, signed: bool, t: &T) -> HostResidency {
    let (_, guard) = new_scope(t, uint32, signed);
    HostResidency { executor: Arc::new(HostResident(guard.clone())), guard, id: t.0.id, t: t.clone() }
}

/// The resident fold's device: the scope, found by its guard, which the last holder dropping ends.
struct HostResident(Arc<Guard>);

impl core_exec::Resident for HostResident {
    fn fill(&self, ty: &Ty, length: i64, v: &Value) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.fill(ty, length, v))
    }
    fn invoke(&self, packed: &Packed, args: &[Value], locals: &[Value], live_: &Frame) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.invoke(0, None, packed, args, locals, live_))
    }
    fn invoke_all(&self, packed: &Packed, tys: &[Ty], args: &[Value], locals: &[Value], live_: &Frame) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| Ok(s.invoke_all(0, None, packed, tys, args, locals, live_)?.map(|vs| Value::List(Arc::new(vs)))))
    }
    fn unfold(&self, packed: &Packed, args: &[Value], steps: i64, source: &Value, locals: &[Value], live_: &Frame) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.invoke(steps, Some(source), packed, args, locals, live_))
    }
    fn compact(&self, mask: &Value) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.compact(mask))
    }
    fn coerce(&self, ty: &Ty, v: &Value) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.coerce(ty, v))
    }
    fn concat(&self, a: &Value, b_: &Value) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.concat(a, b_))
    }
    fn finish(&self, v: &Value) -> Result<Value, Diagnostic> {
        scope_of(&self.0).map_or(Ok(v.clone()), |s| s.finish(v))
    }
    fn keep(&self, v: &Value) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.keep(v))
    }
    fn count(&self, v: &Value) -> Result<Option<i64>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.count_value(v))
    }
    fn capacity(&self, v: &Value) -> Option<i64> {
        scope_of(&self.0).and_then(|s| s.capacity(v))
    }
    fn close(&self) {
        if let Some(s) = scope_of(&self.0) {
            s.close();
            // the integers handed out are the host's; the scope is spent
            s.s.borrow_mut().integers.clear();
        }
    }
    fn settle(&self) -> Result<(), Diagnostic> {
        scope_of(&self.0).map_or(Ok(()), |s| {
            s.settle_inner()?;
            s.s.borrow_mut().post = true;
            s.t.0.showing.set(Some(s.id));
            Ok(())
        })
    }
    fn range(&self, ty: &Ty, v: &Value, div: i64, length: i64) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.range(ty, v, div, length))
    }
    fn nth(&self, v: &Value, i: i64) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.0).map_or(Ok(None), |s| s.nth(v, i))
    }
    fn exact(&self) -> Option<Arc<dyn core_exec::Resident>> {
        scope_of(&self.0).and_then(|s| live(&s.t).then(|| resident_exact(&s.t).executor))
    }
}

/// A resident scope as a backend-neutral residency; it and its executor share the scope's guard.
pub struct HostResidency {
    executor: Arc<dyn core_exec::Resident>,
    guard: Arc<Guard>,
    id: i64,
    t: T,
}

impl HostResidency {
    /// A settled uint32 stream's buffer and count, borrowed from the scope.
    pub fn uint32_stream(&self, v: &Value) -> Option<(Buffer, i64)> {
        let s = scope_of(&self.guard)?;
        let p = s.find(v).filter(|p| p.uint32)?;
        s.settle_inner().ok()?;
        let c = s.count_of(&p).ok()?;
        Some((p.buffer.0.clone(), c))
    }
}

impl G::Residency for HostResidency {
    fn executor(&self) -> &Arc<dyn core_exec::Resident> { &self.executor }
    fn retain_uint32(&self, v: &Value) -> Option<G::Integers> {
        scope_of(&self.guard).and_then(|s| s.retain_uint32(v)).map(|holder| run::integers(&holder))
    }
    fn retain_float(&self, v: &Value) -> Option<G::Planes> {
        scope_of(&self.guard).and_then(|s| s.retain_float(v))
    }
    fn pack_planes(&self, v: &[Value; 3]) -> Result<Option<Value>, Diagnostic> {
        scope_of(&self.guard).map_or(Ok(None), |s| s.pack_planes(v))
    }
    fn incidence(&self) -> G::IncidenceBuilder {
        let id = self.id;
        Arc::new(move |points, streams| match host_of(id).filter(live) {
            Some(t) => build_incidence(&t, points, streams),
            None => error("GPU host is closed or called from another domain."),
        })
    }
    fn validate_topology(&self, point_count: i64, vertices: &Value, offsets: &Value, kinds: &Value, max_polygon_size: i64) -> Option<Result<(), Diagnostic>> {
        match (self.uint32_stream(vertices), self.uint32_stream(offsets), self.uint32_stream(kinds)) {
            (Some(v), Some(o), Some(k)) => Some(validate_topology(&self.t, (&v.0, v.1), (&o.0, o.1), (&k.0, k.1), point_count, max_polygon_size)),
            _ => None,
        }
    }
    fn ready(&self) -> bool { scope_of(&self.guard).is_none_or(|s| s.ready()) }
}

pub fn resident(uint32: bool, fast: bool, t: &T) -> Arc<dyn core_exec::Resident> {
    resident_scope(uint32, fast, t).executor
}

/// A drawing's mark attribute over the arrays its resident fold kept after the loop, dispatched
/// over their buffers with the device count: its output, which `output` resolves and which
/// carries a count record. `None` when it reads no kept array (the ordinary display path),
/// `Some(None)` when it reads one but cannot run on this host.
pub fn show(t: &T, packed: &Packed, live_: &Frame) -> Option<Option<G::Value>> {
    let id = t.0.showing.get()?;
    let scope = SCOPES.with(|s| s.borrow().get(&id).cloned())?;
    scope.show(packed, live_)
}

fn validation_status(t: &T) -> Result<Buffer, Diagnostic> {
    if let Some(b_) = &*t.0.validation_status.borrow() {
        return Ok(b_.clone());
    }
    let buffer = gpu_r(b::create_buffer(&device_of(t), BufferDescriptor { label: Some("Flow topology validation".to_owned()), size: run::TOPOLOGY_STATUS_BYTES, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc] }, None))?;
    *t.0.validation_status.borrow_mut() = Some(buffer.clone());
    t.0.resident_buffers.set(t.0.resident_buffers.get() + 1);
    Ok(buffer)
}

/// Validates exact uint32 streams without reading payload, with one cached status buffer.
/// Successful status reads, including topology rejections, count as validation bytes.
pub fn validate_topology(t: &T, vertices: (&Buffer, i64), offsets: (&Buffer, i64), kinds: (&Buffer, i64), point_count: i64, max_polygon_size: i64) -> Result<(), Diagnostic> {
    if !live(t) {
        return error("GPU host is closed or called from another domain.");
    }
    let status = validation_status(t)?;
    let t2 = t.clone();
    run::validate_topology(&move |bytes| t2.0.validation_bytes.set(t2.0.validation_bytes.get() + bytes), &queue_of(t), &t.0.pipelines, vertices, offsets, kinds, &status, point_count, max_polygon_size)
}

/// Close before releasing the caller's GPU lease.
pub fn close(t: &T) {
    let d = &*t.0;
    assert!(me() == d.domain, "Host.close: creating domain required");
    if d.closed.get() {
        return;
    }
    d.discarding.set(true);
    d.shown_unsettled.borrow_mut().clear();
    d.runners.borrow_mut().clear();
    d.sites.borrow_mut().clear();
    if let Some(r) = d.ownership.borrow_mut().take() {
        run::close(&r, true);
        let mut s = add_run(&d.retired.borrow(), &r);
        s.runners_released = d.retired.borrow().runners_released + 1;
        *d.retired.borrow_mut() = s;
    }
    run::close_pool(&d.shared);
    let mut doomed: Vec<Buffer> = vec![];
    doomed.extend(d.iota.borrow_mut().take().map(|x| x.0));
    doomed.extend(d.iota_uint32.borrow_mut().take().map(|x| x.0));
    doomed.extend(std::mem::take(&mut *d.retired_buffers.borrow_mut()).into_iter().rev().map(|x| x.0));
    doomed.iter().for_each(|x| destroy_owned(&IOTA_OWNER, x));
    if let Some(b_) = d.validation_status.borrow_mut().take() {
        let _ = b::destroy_buffer(&b_);
    }
    for (_, bs) in d.buffers.borrow_mut().drain() {
        bs.iter().rev().for_each(|x| destroy_owned(&DISPLAY_OWNER, x));
    }
    d.tables.borrow_mut().clear();
    for (_, x) in d.adopted_cache.borrow_mut().drain(..) {
        destroy_resident(&x.0);
    }
    purge(&d.adopt_slot);
    purge(&d.range_slot);
    pipelines::close(&d.pipelines);
    HOSTS.with(|h| h.borrow_mut().remove(&d.id));
    d.closed.set(true);
}

/// Shared display storage: at most four free buffers per size class and 64 classes, and the kept
/// outputs' free bytes in all.
fn return_display_buffer(t: &T, bytes: i64, out: &Buffer) {
    assert!(me() == t.0.domain, "Host display buffer released on another domain");
    let bytes = run::size_class(bytes);
    let (free_len, known, sizes, idle) = {
        let bs = t.0.buffers.borrow();
        (bs.get(&bytes).map_or(0, |v| v.len()), bs.contains_key(&bytes), bs.len(), bs.iter().map(|(size, v)| size * v.len() as i64).sum::<i64>())
    };
    if live(t) && free_len < 4 && (known || sizes < 64) && idle + bytes <= run::FREE_BUDGET.max(4 * bytes) {
        t.0.buffers.borrow_mut().entry(bytes).or_default().push(out.clone());
    } else {
        destroy_owned(&DISPLAY_OWNER, out);
    }
}

fn display_buffer(t: &T, bytes: i64) -> Result<Buffer, Diagnostic> {
    // by size class: a mesh whose triangle count moves each frame finds last frame's buffer
    let bytes = run::size_class(bytes);
    if let Some(bs) = t.0.buffers.borrow_mut().get_mut(&bytes) {
        if !bs.is_empty() {
            return Ok(bs.pop().unwrap());
        }
    }
    if bytes >= 1 << 20 {
        run::admit(bytes)?;
    }
    let buffer = gpu_r(b::create_buffer(&device_of(t), BufferDescriptor { label: Some("Flow display storage".to_owned()), size: bytes, usage: vec![BufferUsage::Storage, BufferUsage::Index] }, None))?;
    t.0.resident_buffers.set(t.0.resident_buffers.get() + 1);
    mem::charge(&DISPLAY_OWNER, buffer_bytes(&buffer));
    Ok(buffer)
}

/// The host-rule triangle index buffer for resident triangle/quad streams, its settled count and
/// idempotent release (creating thread). Borrows all inputs, reads only four validation bytes.
/// Uncertain or unsupported polygons return `E_GPU_DISPLAY_TRIANGLES` for a caller's host
/// fallback. Release the output before releasing the host's GPU lease.
pub fn triangle_indices(t: &T, positions: (&Buffer, i64), vertices: (&Buffer, i64), offsets: (&Buffer, i64), kinds: (&Buffer, i64)) -> Result<(Buffer, i64, Box<dyn Fn()>), Diagnostic> {
    if !live(t) {
        return error("GPU host is closed or called from another domain.");
    }
    let (nv, np) = (vertices.1, kinds.1);
    if np <= 0 || np > u32::MAX as i64 - 1 || nv < 3 * np || nv > 4 * np || nv > u32::MAX as i64 || 3 * (nv - 2 * np) > u32::MAX as i64 {
        return Err(value::fail("E_GPU_DISPLAY_TRIANGLES", "Resident display requires nonempty triangle/quad topology."));
    }
    let status = validation_status(t)?;
    let bytes = 3 * (nv - 2 * np) * 4;
    let scratch = ((np + 255) / 256) * 4;
    let out = display_buffer(t, bytes)?;
    let result = (|| {
        let blocks = display_buffer(t, scratch)?;
        let t2 = t.clone();
        let r = run::triangle_indices(&move |n| t2.0.validation_bytes.set(t2.0.validation_bytes.get() + n), &queue_of(t), &t.0.pipelines, positions, vertices, offsets, kinds, &out, &blocks, &status);
        return_display_buffer(t, scratch, &blocks);
        r
    })();
    match result {
        Err(d) => {
            return_display_buffer(t, bytes, &out);
            Err(d)
        }
        Ok(count) => {
            let released = Cell::new(false);
            let (t3, out2) = (t.clone(), out.clone());
            Ok((out, count, Box::new(move || {
                if !released.get() {
                    released.set(true);
                    return_display_buffer(&t3, bytes, &out2);
                }
            })))
        }
    }
}

/// A triangle mesh's display normals (packed float3) from the `count` packed float3 `positions`
/// the device holds and the triangles' `corners`, computed on the device and waited for. The
/// buffer is the caller's until the release. The corner tables are kept by the `corners` array's
/// identity (8). `None` when the device cannot, or a point has more than 4,096 corners.
pub fn normals(t: &T, positions: &Buffer, count: i64, corners: &Arc<Vec<u32>>) -> Option<(Buffer, Box<dyn Fn()>)> {
    let device = device_of(t);
    let queue = queue_of(t);
    let upload = |label: &str, values: &[u32]| -> Option<Buffer> {
        let mut bytes = vec![0u8; 4 * 1.max(values.len())];
        for (i, v) in values.iter().enumerate() {
            bytes[i * 4..i * 4 + 4].copy_from_slice(&v.to_le_bytes());
        }
        let buffer = b::create_buffer(&device, BufferDescriptor { label: Some(label.to_owned()), size: bytes.len() as i64, usage: vec![BufferUsage::Storage, BufferUsage::CopyDst] }, None).ok()?;
        if b::write_buffer(&buffer, 0, &bytes).is_ok() {
            Some(buffer)
        } else {
            let _ = b::destroy_buffer(&buffer);
            None
        }
    };
    let tables = || -> Option<Vec<Buffer>> {
        let mut offsets = vec![0u32; count as usize + 1];
        for p in corners.iter() {
            offsets[*p as usize + 1] += 1;
        }
        // the stage's loop stops at 4,096 corners a point
        if offsets.iter().any(|n| *n > 4096) {
            return None;
        }
        for p in 1..=count as usize {
            offsets[p] += offsets[p - 1];
        }
        let mut next: Vec<u32> = offsets[..count as usize].to_vec();
        let mut vertices = vec![0u32; corners.len()];
        for (v, p) in corners.iter().enumerate() {
            vertices[next[*p as usize] as usize] = v as u32;
            next[*p as usize] += 1;
        }
        let made = [upload("Flow display corners", corners), upload("Flow display point offsets", &offsets), upload("Flow display point corners", &vertices)];
        if made.iter().all(|m| m.is_some()) {
            Some(made.into_iter().flatten().collect())
        } else {
            made.iter().flatten().for_each(|x| {
                let _ = b::destroy_buffer(x);
            });
            None
        }
    };
    let bytes = count * 12;
    if !live(t) || count <= 0 || corners.len() % 3 != 0 {
        return None;
    }
    let found = t.0.tables.borrow_mut().find(&|k, _| Arc::ptr_eq(k, corners)).map(|v| v.clone());
    let tables_ = match found {
        Some(tb) => tb,
        None => {
            let made = tables();
            t.0.tables.borrow_mut().add(corners.clone(), made.clone());
            made
        }
    };
    let ts = tables_?;
    let out = display_buffer(t, bytes).ok()?;
    let run_ = || -> Option<()> {
        let compiled = pipelines::get(&t.0.pipelines, kernels::normals()).ok()?;
        let commands = b::begin_commands(&queue).ok()?;
        let encoded = (|| -> Option<()> {
            let encoder = b::compute_encoder(&commands).ok()?;
            let mut n = [0u8; 4];
            run::put_i32(&mut n, 0, count as i32);
            let ok = b::set_pipeline(&encoder, &compiled.pipeline).is_ok()
                && b::set_buffer(&encoder, 0, positions).is_ok()
                && b::set_buffer(&encoder, 1, &ts[0]).is_ok()
                && b::set_buffer(&encoder, 2, &ts[1]).is_ok()
                && b::set_buffer(&encoder, 3, &ts[2]).is_ok()
                && b::set_buffer(&encoder, 4, &out).is_ok()
                && b::set_bytes(&encoder, 5, &n).is_ok()
                && b::dispatch_threads(&encoder, (count, 1, 1), (256, 1, 1)).is_ok();
            if b::end_compute(&encoder).is_ok() && ok { Some(()) } else { None }
        })();
        if encoded.is_none() {
            let _ = b::abandon(&commands);
            return None;
        }
        let receipt = b::commit(&commands).ok()?;
        b::complete_through(&queue, receipt.epoch).ok()
    };
    match run_() {
        Some(()) => {
            let released = Cell::new(false);
            let (t2, out2) = (t.clone(), out.clone());
            Some((out, Box::new(move || {
                if !released.get() {
                    released.set(true);
                    return_display_buffer(&t2, bytes, &out2);
                }
            })))
        }
        None => {
            return_display_buffer(t, bytes, &out);
            None
        }
    }
}

/// Cumulative successful operations, including evicted runners and closed owners. Inspect on the
/// creating thread, outside measured frame work.
pub fn stats(t: &T) -> Stats {
    let d = &*t.0;
    assert!(me() == d.domain, "Host.stats: creating domain required");
    let mut totals = d.retired.borrow().clone();
    for (_, r) in d.runners.borrow().iter() {
        totals = add_run(&totals, &r.run);
    }
    if let Some(r) = &*d.ownership.borrow() {
        totals = add_run(&totals, r);
    }
    // a resident fold's runners and buffers count too: a frame's draw should make none
    for (_, site) in d.sites.borrow().iter() {
        if let Some(r) = &site.run {
            totals = add_run(&totals, r);
        }
        if let Some(runner) = site.shown.borrow().as_ref() {
            totals = add_run(&totals, &runner.run);
        }
    }
    Stats {
        buffer_creations: totals.buffer_creations + d.resident_buffers.get(),
        input_uploads: totals.input_uploads + d.adopted_uploads.get(),
        input_uploaded_bytes: totals.input_uploaded_bytes + d.adopted_uploaded_bytes.get(),
        readback_bytes: totals.readback_bytes + d.resident_payload_bytes.get(),
        payload_readback_bytes: totals.payload_readback_bytes + d.resident_payload_bytes.get(),
        count_readback_bytes: totals.count_readback_bytes + d.count_bytes.get(),
        validation_readback_bytes: totals.validation_readback_bytes + d.validation_bytes.get(),
        runners_created: d.created.get(),
        pipeline_compilations: pipelines::private::compilations(&d.pipelines),
        pipeline_releases: pipelines::private::releases(&d.pipelines),
        held_buffers: if d.closed.get() { 0 } else { run::private::pool_held(&d.shared) },
        freed: run::private::pool_freed(&d.shared),
        resident_dispatches: d.dispatches.get(),
        ..totals
    }
}

/// Closes the runner of a prepared kernel, by its output's identity: its upload buffers and the
/// inputs they were compared to go with it. A later run of that kernel makes a new one.
pub fn forget_producer(t: &T, identity: i64) {
    t.0.runners.borrow_mut().remove(&|_, r| r.identity == identity);
}

/// The backend: owns at most 64 runners and 64 pipelines. An absent cost keeps production CPU
/// placement until native measurements supply a model. The caller owns the GPU lease.
fn build_incidence(t: &T, points: i64, streams: [Arc<G::Integers>; 3]) -> Result<Option<G::Incidence>, Diagnostic> {
    let mut pins: Vec<run::Pin> = Vec::new();
    for stream in &streams {
        match run::pin_integers(stream) {
            Some(pin) => pins.push(pin),
            None => { for (_, _, _, unpin) in pins { unpin(); } return Ok(None); }
        }
    }
    let (scope, _guard) = new_scope(t, true, false);
    let slot = scope.range_slot();
    scope.s.borrow_mut().extra_slots.push(slot.clone());
    let mut retained: Vec<Arc<G::Integers>> = Vec::new();
    let result = (|| {
        let nv = streams[0].count;
        let mut values = Vec::new();
        let mut buffers = Vec::new();
        for count in [points + 1, nv, nv, nv, nv] {
            let buffer = scope.alloc(&slot, (count * 4).max(4))?;
            values.push(scope.placeholder(false, true, i64::MAX, None, &slot, &int_array_ty(), buffer.clone(), None, count));
            buffers.push(buffer.0);
        }
        let scratch = scope.alloc(&slot, (nv * 4).max(4))?;
        slot.spent.borrow_mut().push(scratch.clone());
        let blocks = scope.alloc(&slot, run::compact_blocks(nv) + 4)?;
        slot.spent.borrow_mut().push(blocks.clone());
        run::point_incidence(&scope.queue, &t.0.pipelines, points,
            (&pins[0].0, nv), (&pins[1].0, streams[1].count), (&pins[2].0, streams[2].count),
            &run::Incidence { point_offsets: &buffers[0], point_vertices: &buffers[1], primitive_of_vertex: &buffers[2], first: &buffers[3], corners: &buffers[4] },
            &scratch.0, &blocks.0)?;
        for value in &values {
            let kept = scope.retain_uint32(value).ok_or_else(|| value::fail("E_GPU", "Point incidence could not retain its output."))?;
            retained.push(Arc::new(run::integers(&kept)));
        }
        Ok(G::Incidence { point_offsets: retained[0].clone(), point_vertices: retained[1].clone(), primitive_of_vertex: retained[2].clone(), first: retained[3].clone(), corners: retained[4].clone() })
    })();
    scope.close();
    for (_, _, _, unpin) in pins { unpin(); }
    if result.is_err() { for stream in &retained { stream.release(false); } }
    result.map(Some)
}

/// The backend: found by its host's id, so it is `Send + Sync`. Residency is the fast path's.
struct HostBackend(i64);

impl G::Backend for HostBackend {
    fn residency(&self, fast: bool) -> Option<Box<dyn G::Residency>> {
        let t = host_of(self.0).filter(live)?;
        fast.then(|| Box::new(resident_scope(true, fast, &t)) as Box<dyn G::Residency>)
    }

    fn prepare(&self, fast: bool, packed: &Packed) -> Result<Arc<dyn G::Kernel>, Diagnostic> {
        let id = self.0;
        let Some(t) = host_of(id).filter(live) else {
            return error("GPU host is closed or called from another domain.");
        };
        // a range source (`index_sources`) is the host's index buffer, never uploaded
        let index = phost::index_sources(packed);
        let msl = emit::kernel(&emit::Options { spirv: is_vulkan(), fast, ..emit::Options::default() }, packed)?;
        pipelines::get(&t.0.pipelines, &msl)?;
        // Each prepared producer owns an output generation. Sharing only its shader source
        // would overwrite an earlier drawing's borrowed output.
        let key = NEXT.fetch_add(1, Ordering::SeqCst).to_string();
        let kernel = HostKernel { id, fast, index, key, msl, packed: packed.clone() };
        let _ = kernel.runner(&t, None)?;
        Ok(Arc::new(kernel))
    }
}

pub fn backend(t: &T) -> Box<dyn G::Backend> {
    Box::new(HostBackend(t.0.id))
}

/// Drops this thread's host and fold-scope registries (see `run::clear_thread_state`).
pub fn clear_thread_state() {
    let scopes: Vec<Rc<Scope>> = SCOPES.with(|s| s.borrow_mut().drain().map(|(_, v)| v).collect());
    drop(scopes);
    HOSTS.with(|h| h.borrow_mut().clear());
    crate::run::clear_thread_state();
}
