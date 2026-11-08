//! Port of lib/flow_gpu/run.ml: one runner per prepared kernel (its buffers, uniforms, status and
//! the outputs it keeps), the buffer pool runners share, and the resident fold's device stages.
//!
//! Mutable OGPU handles and command submission stay on their creating thread. A holder's
//! `Arc<Token>` carries its fate and an owning read-only native allocation handle: after owner
//! settlement, any thread can perform the first conversion without an owner pump. Unsettled
//! foreign reads still ask `service` to perform owner-only completion and status validation.
//! Dropping the last token queues release (OCaml: `Gc.finalise`); materialisation releases its
//! native readback lease as soon as real owned planes have been published.
use crate::kernels;
use crate::pipelines;
use flow_ir::flow_ir::gpu;
use flow_shader::emit::Kernel;
use flow_syntax::value::{self, Value};
use ogpu_core::backend::{self as b, Accel, Buffer, Commands, ComputeEncoder, Device, Queue};
use ogpu_core::types::{BufferDescriptor, BufferUsage, Memory};
use rays_diagnostic::Diagnostic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::{Arc, Condvar, LazyLock, Mutex, Weak};
use std::thread::ThreadId;

pub const MAX_STRING: i64 = 144115188075855863;
/// Idle bytes kept for reuse: the pool's free list, and a host's display storage.
pub const free_budget: i64 = 256 << 20;
const free_idle_drains: i64 = 1024;
pub const slot_shrink_after: i64 = 240;
const slot_shrink_floor: i64 = 1 << 20;

static HELD_OWNER: LazyLock<mem::Owner> = LazyLock::new(|| mem::plan_in(mem::Pool::Device, "flow_gpu.held"));
static FREE_OWNER: LazyLock<mem::Owner> = LazyLock::new(|| mem::cache_in(mem::Pool::Device, "flow_gpu.free"));
static SLOT_OWNER: LazyLock<mem::Owner> = LazyLock::new(|| mem::plan_in(mem::Pool::Device, "flow_gpu.slots"));
static NEXT_KEPT: AtomicI64 = AtomicI64::new(0);

pub fn diagnostic(error: &ogpu_core::error::Error) -> Diagnostic {
    pipelines::diagnostic(error)
}
pub(crate) fn gpu<X>(r: Result<X, ogpu_core::error::Error>) -> Result<X, Diagnostic> {
    r.map_err(|e| diagnostic(&e))
}
fn e_gpu(message: &str) -> Diagnostic {
    value::fail("E_GPU", message)
}
fn e_kernel(message: &str) -> Diagnostic {
    value::fail("E_KERNEL", message)
}
/// A device buffer's size for `bytes`: eight classes an octave above 1 MiB (at most 12.5% unused, where
/// doubling left up to half of a 100 MB mesh attribute empty), whole below; never under 256 bytes.
pub fn size_class(bytes: i64) -> i64 {
    if bytes <= 1 << 20 {
        return bytes.max(256);
    }
    let step = (1i64 << (63 - (bytes - 1).leading_zeros())) / 8;
    (bytes + step - 1) / step * step
}

pub(crate) fn admit(n: i64) -> Result<(), Diagnostic> {
    mem::admit_bytes(mem::Pool::Device, n).map_err(|r| value::fail("E_ARRAY_MEMORY", &r.to_string()))
}
fn me() -> ThreadId {
    std::thread::current().id()
}

pub struct Slot {
    pub buffer: Option<Buffer>,
    pub capacity: i64,
    pub bytes: Vec<u8>,
    /// a mapped slot (inputs, gathers, outputs): its view is written and read in place
    pub mapped: bool,
    pub uploaded: Option<(Arc<Vec<f64>>, i64)>,
    /// dispatches in a row asking under a quarter of `capacity`
    pub low: i64,
}

fn slot() -> Slot {
    Slot { buffer: None, capacity: 0, bytes: Vec::new(), mapped: false, uploaded: None, low: 0 }
}

/// What a holder's buffer has come to. Who only retires a holder or borrows its buffer keeps this,
/// never the holder.
pub struct Fate {
    /// orders an invalidate against close's destroy; never held while converting
    lock: Mutex<()>,
    planes: Mutex<Option<Arc<Vec<Vec<f64>>>>>,
    /// its buffer is gone: closed (read first) or freed (never read)
    destroyed: AtomicBool,
    /// handed out once: it stays readable from then on
    retired: AtomicBool,
    /// conversions and pins in progress: its buffer is not reused under them
    readers: AtomicI64,
    readback: Mutex<Option<b::OwnedReadback>>,
    count: i64,
    width: i64,
    uint32: bool,
    materialised: Arc<AtomicI64>,
    materialised_bytes: Arc<AtomicI64>,
}

impl Fate {
    fn destroy(&self) {
        self.destroyed.store(true, Ordering::SeqCst);
        self.readback.lock().unwrap().take();
    }
    fn planes(&self) -> Option<Arc<Vec<Vec<f64>>>> {
        self.planes.lock().unwrap().clone()
    }
    fn destroyed(&self) -> bool {
        self.destroyed.load(Ordering::SeqCst)
    }
}

/// The queues the holders' closures push to from any thread.
struct Shared {
    released: Mutex<Vec<i64>>,
    retiring: Mutex<Vec<(i64, Weak<Token>)>>,
    /// holders freed unread
    freed: AtomicI64,
}

/// The holder's liveness: when the last clone drops its buffer is released (a finaliser's job).
pub struct Token {
    settled: Arc<AtomicBool>,
    id: i64,
    owner: ThreadId,
    fate: Arc<Fate>,
    shared: Arc<Shared>,
}
impl Drop for Token {
    fn drop(&mut self) {
        // its last holder gone unread and never released: freed as `release(false)` frees
        if self.fate.planes().is_none() {
            let _g = self.fate.lock.lock().unwrap();
            if !self.fate.destroyed() && !self.fate.retired.load(Ordering::SeqCst) {
                self.shared.freed.fetch_add(1, Ordering::SeqCst);
            }
        }
        self.shared.released.lock().unwrap().push(self.id);
    }
}

pub struct Kept {
    id: i64,
    buffer: Buffer,
    count: i64,
    width: i64,
    uint32: bool,
    fate: Arc<Fate>,
    /// its contents are its id: a cache keying values by contents tells two held arrays apart
    /// Uint32 holders need an input marker; float markers belong to their returned planes.
    placeholder: Option<Arc<Vec<f64>>>,
    epoch: i64,
    /// completed, its status read: CPU readers may read it
    settled: Arc<AtomicBool>,
    /// its box once reduced: it never changes
    bounds_memo: RefCell<Option<Option<Vec<f64>>>>,
    owner: T,
}

thread_local! {
    static KEPT: RefCell<HashMap<i64, Rc<Kept>>> = RefCell::new(HashMap::new());
    /// Kept outputs a later dispatch may bind by their placeholder, on the creating thread.
    static BINDABLE: RefCell<Vec<(Weak<Vec<f64>>, Weak<Token>, i64, i64)>> = const { RefCell::new(Vec::new()) };
}

fn kept_of(id: i64) -> Option<Rc<Kept>> {
    KEPT.with(|k| k.borrow().get(&id).cloned())
}

fn bindable_live() {
    BINDABLE.with(|b| b.borrow_mut().retain(|(p, t, _, _)| p.strong_count() > 0 && t.strong_count() > 0));
}

/// The held buffer a placeholder stands for and the floats it holds, while its holder has not read
/// it back (its buffer is then released).
fn held_storage(array: &Arc<Vec<f64>>) -> Option<(Rc<Kept>, i64)> {
    bindable_live();
    let (id, length) = BINDABLE.with(|b| {
        b.borrow().iter().rev().find(|(p, _, _, _)| p.upgrade().is_some_and(|p| Arc::ptr_eq(&p, array))).map(|(_, _, id, length)| (*id, *length))
    })?;
    let kept = kept_of(id)?;
    if kept.fate.planes().is_none() && !kept.fate.destroyed() {
        Some((kept, length))
    } else {
        None
    }
}

pub fn held(array: &Arc<Vec<f64>>) -> Option<(Buffer, i64)> {
    held_storage(array).map(|(kept, length)| (kept.buffer.clone(), length))
}

pub fn uint32_input(array: &Arc<Vec<f64>>) -> bool {
    held_storage(array).is_some_and(|(kept, _)| kept.uint32)
}

fn typed_held(array: &Arc<Vec<f64>>, uint32: bool) -> Option<(Buffer, i64)> {
    held_storage(array).filter(|(kept, _)| kept.uint32 == uint32).map(|(kept, length)| (kept.buffer.clone(), length))
}

/// ... as an input, when it covers `length` floats
fn held_input(array: &Arc<Vec<f64>>, length: i64, uint32: bool) -> Option<Buffer> {
    typed_held(array, uint32).and_then(|(buffer, words)| if words >= length { Some(buffer) } else { None })
}

/// a uniform layout entry, parsed once per runner: a dispatch only writes it
#[derive(Clone, Copy, Debug)]
enum Entry {
    /// uniform, offset, width
    Uniform_at(usize, usize, usize),
    /// gather, offset
    Gather_at(usize, usize),
    Strands_at(usize),
    Spheres_at(usize),
    /// frame instruction, offset
    Frame_at(usize, usize),
}

fn entries(kernel: &Kernel) -> Vec<Entry> {
    kernel
        .uniform_layout
        .iter()
        .map(|(name, offset, width)| {
            let (offset, width) = (*offset as usize, *width as usize);
            if let Some(rest) = name.strip_prefix("uniform:") {
                Entry::Uniform_at(rest.parse().unwrap(), offset, width)
            } else {
                let parts: Vec<&str> = name.split(':').collect();
                match parts.as_slice() {
                    ["gather", k, _] => Entry::Gather_at(k.parse().unwrap(), offset),
                    ["scene", "strands"] => Entry::Strands_at(offset),
                    ["scene", "spheres"] => Entry::Spheres_at(offset),
                    ["frame", index, _] => Entry::Frame_at(index.parse().unwrap(), offset),
                    _ => panic!("Run: uniform layout entry {name}"),
                }
            }
        })
        .collect()
}

// ---- the pool

struct PoolState {
    held: HashMap<i64, (Buffer, i64, Arc<Fate>)>,
    free: Vec<(Buffer, i64)>,
    /// the tick each free buffer came back, by buffer id
    free_ages: HashMap<i64, i64>,
    /// drains so far: an idle device's clock, no real one
    tick: i64,
    /// oldest first, by id
    retired: Vec<(i64, Weak<Token>)>,
    /// bytes created by the pool's owner at its last requested cycle
    collected_at: i64,
    /// live holders of closed runners: settled, unread, theirs until `close_pool`
    orphans: Vec<Weak<Token>>,
    created: i64,
    free_limit: i64,
}

/// Kept outputs' buffers that runners share, recycled by size class (256 MiB free at most).
#[derive(Clone)]
pub struct Pool(Rc<PoolData>);
struct PoolData {
    s: RefCell<PoolState>,
    shared: Arc<Shared>,
}

pub fn pool() -> Pool {
    Pool(Rc::new(PoolData {
        s: RefCell::new(PoolState {
            held: HashMap::new(),
            free: Vec::new(),
            free_ages: HashMap::new(),
            tick: 0,
            retired: Vec::new(),
            collected_at: 0,
            orphans: Vec::new(),
            created: 0,
            free_limit: 512 << 20,
        }),
        shared: Arc::new(Shared { released: Mutex::new(Vec::new()), retiring: Mutex::new(Vec::new()), freed: AtomicI64::new(0) }),
    }))
}

/// An upload's source is its content: a recook that rebuilds the same array (a fresh session, a
/// constant fill) reuses the buffer. Bits, not `==`: -0.0 and 0.0 upload differently.
fn same_values(old: &Arc<Vec<f64>>, new: &Arc<Vec<f64>>) -> bool {
    Arc::ptr_eq(old, new) || (old.len() == new.len() && old.iter().zip(new.iter()).all(|(a, b)| a.to_bits() == b.to_bits()))
}

/// on the creating thread: recycle or destroy what collected or read holders gave back (a buffer a
/// reader is still converting waits for the next drain)
fn drain_pool(pool: &Pool) {
    let mut s = pool.0.s.borrow_mut();
    s.tick += 1;
    if s.tick % 64 == 0 {
        s.orphans.retain(|w| w.strong_count() > 0);
    }
    // a free buffer nothing has taken for this long goes back to the device
    if !s.free.is_empty() && s.tick % 64 == 0 {
        let tick = s.tick;
        let stale: Vec<bool> = s
            .free
            .iter()
            .map(|(buffer, _)| tick - s.free_ages.get(&b::buffer_id(buffer)).copied().unwrap_or(tick) > free_idle_drains)
            .collect();
        let free = std::mem::take(&mut s.free);
        let mut keep = vec![];
        for ((buffer, capacity), old) in free.into_iter().zip(stale).rev() {
            if old {
                mem::release(&FREE_OWNER, capacity);
                s.free_ages.remove(&b::buffer_id(&buffer));
                let _ = b::destroy_buffer(&buffer);
            } else {
                keep.push((buffer, capacity));
            }
        }
        keep.reverse();
        s.free = keep;
    }
    let ids = std::mem::take(&mut *pool.0.shared.released.lock().unwrap());
    for id in ids {
        let Some((_, _, fate)) = s.held.get(&id) else {
            KEPT.with(|k| k.borrow_mut().remove(&id));
            continue;
        };
        if fate.readers.load(Ordering::SeqCst) > 0 {
            pool.0.shared.released.lock().unwrap().push(id);
            continue;
        }
        let (buffer, capacity, _) = s.held.remove(&id).unwrap();
        KEPT.with(|k| k.borrow_mut().remove(&id));
        mem::release(&HELD_OWNER, capacity);
        // the size class's own free bytes: another class's big buffers must not turn this one's away
        let fits = |free: i64, limit: i64| free + capacity <= limit.max(8 * capacity);
        let (total, own) = s.free.iter().fold((0, 0), |(total, own), (_, c)| (total + c, if *c == capacity { own + c } else { own }));
        // a frame of big outputs must find its buffers again
        if fits(own, free_budget) && fits(total, s.free_limit) {
            mem::charge(&FREE_OWNER, capacity);
            // a dead buffer: under RAYS_MEM_CLOBBER a wrong lifetime reads NaN at once
            if mem::clobber() {
                if let Some(v) = b::mapped_view(&buffer) {
                    v.borrow_mut().iter_mut().for_each(|x| *x = f32::NAN);
                    // Poison the actual allocation too: owned readers do not use this host copy.
                    gpu(b::flush_mapped(&buffer, capacity)).expect("clobber recycled GPU buffer");
                }
            }
            // the host is done with it: a discrete device drops its host copy
            let _ = b::release_mapped_view(&buffer);
            let tick = s.tick;
            s.free_ages.insert(b::buffer_id(&buffer), tick);
            s.free.push((buffer, capacity));
        } else {
            let _ = b::destroy_buffer(&buffer);
        }
    }
}

fn take_free(pool: &Pool, length: i64) -> Option<(Buffer, i64)> {
    let mut s = pool.0.s.borrow_mut();
    let at = s.free.iter().rposition(|(_, capacity)| *capacity >= length && *capacity <= 2 * length)?;
    let (buffer, capacity) = s.free.remove(at);
    s.free_ages.remove(&b::buffer_id(&buffer));
    mem::release(&FREE_OWNER, capacity);
    Some((buffer, capacity))
}

/// Past this many bytes of retired holders still unread, the oldest is read now and its buffer
/// comes back, so device memory stays bounded. A quarter of the device's working set, 512 MiB when
/// it does not say.
fn retired_budget_for(device: &Device) -> i64 {
    let recommended = b::recommended_working_set(device);
    if recommended > 0 { recommended / 4 } else { 512 << 20 }
}

fn reclaim(pool: &Pool, device: &Device) {
    pool.0.s.borrow_mut().free_limit = retired_budget_for(device);
    drain_pool(pool);
    {
        let mut s = pool.0.s.borrow_mut();
        let incoming: Vec<(i64, Weak<Token>)> = std::mem::take(&mut *pool.0.shared.retiring.lock().unwrap());
        let mut all: Vec<(i64, Weak<Token>)> = std::mem::take(&mut s.retired);
        all.extend(incoming.into_iter().rev());
        let held = &s.held;
        all.retain(|(id, w)| w.strong_count() > 0 && held.contains_key(id));
        s.retired = all;
    }
    // Rust drops dead holders at once, so the OCaml "ask for a collection" step has nothing to do.
    let over = {
        let s = pool.0.s.borrow();
        s.retired.iter().map(|(id, _)| s.held.get(id).map_or(0, |(_, c, _)| *c)).sum::<i64>() > retired_budget_for(device)
    };
    if over {
        let oldest = {
            let mut s = pool.0.s.borrow_mut();
            if s.retired.is_empty() { None } else { Some(s.retired.remove(0)) }
        };
        if let Some((id, _)) = oldest {
            if let Some(k) = kept_of(id) {
                if k.fate.readers.load(Ordering::SeqCst) == 0 {
                    thaw(&k);
                }
            }
        }
        drain_pool(pool);
    }
}

pub fn recycled(pool: &Pool, device: &Device, length: i64) -> Option<(Buffer, i64)> {
    if let Some(found) = take_free(pool, length) {
        return Some(found);
    }
    reclaim(pool, device);
    take_free(pool, length)
}

/// Tell the pool's pacing of bytes made outside it.
pub fn created(pool: &Pool, bytes: i64) {
    pool.0.s.borrow_mut().created += bytes;
}

/// On the creating thread, after its runners are closed: destroys what it still holds.
pub fn close_pool(pool: &Pool) {
    drain_pool(pool);
    let mut s = pool.0.s.borrow_mut();
    for w in s.orphans.drain(..).rev() {
        if let Some(token) = w.upgrade() {
            let _g = token.fate.lock.lock().unwrap();
            token.fate.destroy();
        }
    }
    pool.0.shared.released.lock().unwrap().clear();
    pool.0.shared.retiring.lock().unwrap().clear();
    s.retired.clear();
    for (id, (buffer, capacity, fate)) in s.held.drain() {
        // a pin outlives a collected holder: its `valid` must see this
        {
            let _g = fate.lock.lock().unwrap();
            fate.destroy();
        }
        KEPT.with(|k| k.borrow_mut().remove(&id));
        mem::release(&HELD_OWNER, capacity);
        let _ = b::destroy_buffer(&buffer);
    }
    for (buffer, capacity) in s.free.drain(..).rev() {
        mem::release(&FREE_OWNER, capacity);
        let _ = b::destroy_buffer(&buffer);
    }
    s.free_ages.clear();
}

// ---- the runner

struct KeptRef {
    epoch: i64,
    settled: Arc<AtomicBool>,
    token: Weak<Token>,
    id: i64,
}

struct State {
    inputs: Vec<Slot>,
    outputs: Vec<Slot>,
    table: Slot,
    gathers: Vec<Slot>,
    status: Slot,
    counted: Slot,
    /// a counted dispatch's threadgroup counts, written on the device
    groups: Slot,
    uniforms: Vec<u8>,
    closed: bool,
    /// the reduction's lanes, made once and reused
    bounds_out: Option<[Buffer; 2]>,
    creations: i64,
    input_uploads: i64,
    input_uploaded_bytes: i64,
    status_reads: i64,
    readback_bytes: i64,
    bounds_readback_bytes: i64,
    status_checked: bool,
    generation: i64,
    table_ready: bool,
    last_epoch: i64,
    /// live holders, materialised at close, with each one's epoch and settled flag
    kept: Vec<KeptRef>,
    /// failed statuses of superseded dispatches no one had settled, oldest first, at most 8
    verdicts: Vec<(i64, Diagnostic)>,
}

struct Data {
    device: Device,
    queue: Queue,
    pipelines: pipelines::T,
    kernel: Kernel,
    layout: Vec<Entry>,
    domain: ThreadId,
    pool: Pool,
    /// made for this runner alone: closed with it
    own_pool: bool,
    materialised: Arc<AtomicI64>,
    materialised_bytes: Arc<AtomicI64>,
    state: RefCell<State>,
}

#[derive(Clone)]
pub struct T(Rc<Data>);

#[derive(Clone)]
pub struct Scene {
    pub accel: Accel,
    pub vertices: Buffer,
    pub spheres: Buffer,
    pub sphere_count: i64,
    pub instances: Option<Buffer>,
}

/// Bindings of a moving or stranded scene, after the instance table.
#[derive(Clone, Default)]
pub struct Extras {
    pub vertices_end: Option<Buffer>,
    pub strands: Option<Buffer>,
    pub strand_count: i64,
    pub curves: Option<Accel>,
    pub prototypes: Option<Accel>,
}

pub fn no_extras() -> Extras {
    Extras::default()
}

#[derive(Clone)]
pub struct Output {
    pub owner: T,
    count: i64,
    width: i64,
    index: i64,
    gpu_seconds: Option<f64>,
    generation: i64,
    epoch: i64,
    check_status: bool,
}

fn kernel_status(flags: i32) -> Diagnostic {
    // The status word is an aggregate across parallel rows, so diagnostics have a stable class
    // priority rather than claiming CPU row-order precedence.
    if flags & 4 != 0 {
        value::fail("E_RANGE", "Integer shift count must be within [0, 63].")
    } else if flags & 2 != 0 {
        value::fail("E_RANGE", "Integer division by zero.")
    } else if flags & 8 != 0 {
        value::fail("E_LIST_RANGE", "nth index is out of range or is not a whole number.")
    } else if flags & 32 != 0 {
        value::fail("E_ARRAY_RANGE", "array/slice needs a non-negative whole length.")
    } else if flags & 16 != 0 {
        value::fail("E_ARRAY_RANGE", "An array slice must be within its source.")
    } else if flags & 64 != 0 {
        value::fail("E_GPU_UINT32_RANGE", "GPU uint32 value must be within [0, 4294967295].")
    } else {
        value::fail("E_KERNEL", "GPU output contains a nonfinite value.")
    }
}

fn nonfinite() -> Diagnostic {
    e_kernel("GPU inputs must remain finite when represented as float32.")
}

pub(crate) fn put_i32(bytes: &mut [u8], offset: usize, v: i32) {
    bytes[offset..offset + 4].copy_from_slice(&v.to_le_bytes());
}
pub(crate) fn get_i32(bytes: &[u8], offset: usize) -> i32 {
    i32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap())
}

fn pack(bytes: &mut [u8], offset: usize, v: f64) -> Result<(), Diagnostic> {
    let f = v as f32;
    if !f.is_finite() {
        return Err(nonfinite());
    }
    bytes[offset..offset + 4].copy_from_slice(&f.to_le_bytes());
    Ok(())
}

pub(crate) fn pack_uint32(bytes: &mut [u8], offset: usize, v: f64) -> Result<(), Diagnostic> {
    if !v.is_finite() || v.fract() != 0.0 || v < 0.0 || v > 4294967295.0 {
        return Err(kernel_status(64));
    }
    bytes[offset..offset + 4].copy_from_slice(&(v as u32).to_le_bytes());
    Ok(())
}

fn upload_uint32(buffer: &Buffer, slot: &mut Slot, source: &[f64], count: usize) -> Result<(), Diagnostic> {
    let length = count * 4;
    if slot.bytes.len() != length {
        slot.bytes = vec![0; length];
    }
    for i in 0..count {
        pack_uint32(&mut slot.bytes, i * 4, source[i])?;
    }
    if count > 0 {
        gpu(b::write_buffer(buffer, 0, &slot.bytes))?;
    }
    Ok(())
}

/// a mapped buffer's host view, made on a discrete device when first asked for
fn view(buffer: &Buffer) -> b::Floats {
    b::mapped_view(buffer).expect("mapped view")
}

/// Straight into the mapped buffer the kernel reads: one float32 rounding per element. A finite
/// double past float32's range rounds to infinity and is refused.
fn pack_floats(floats: &b::Floats, values: &[f64], length: usize) -> Result<(), Diagnostic> {
    let mut f = floats.borrow_mut();
    for index in 0..length {
        let v = values[index] as f32;
        f[index] = v;
        if !v.is_finite() {
            return Err(nonfinite());
        }
    }
    Ok(())
}

pub(crate) fn unpack_array(bytes: &[u8], length: usize) -> (Vec<f64>, bool) {
    let mut finite = true;
    let values = (0..length)
        .map(|i| {
            let v = f32::from_bits(u32::from_le_bytes(bytes[i * 4..i * 4 + 4].try_into().unwrap()));
            if !v.is_finite() {
                finite = false;
            }
            v as f64
        })
        .collect();
    (values, finite)
}

pub fn create_owned(pool_: Option<&Pool>, device: &Device, queue: &Queue, pipelines: &pipelines::T, kernel: &Kernel) -> T {
    let (pool_, own_pool) = match pool_ {
        Some(p) => (p.clone(), false),
        None => (pool(), true),
    };
    T(Rc::new(Data {
        device: device.clone(),
        queue: queue.clone(),
        pipelines: pipelines.clone(),
        kernel: kernel.clone(),
        layout: entries(kernel),
        domain: me(),
        pool: pool_,
        own_pool,
        materialised: Arc::new(AtomicI64::new(0)),
        materialised_bytes: Arc::new(AtomicI64::new(0)),
        state: RefCell::new(State {
            inputs: kernel.input_widths.iter().map(|_| slot()).collect(),
            outputs: kernel.output_widths.iter().map(|_| slot()).collect(),
            table: slot(),
            gathers: kernel.gathers.iter().map(|_| slot()).collect(),
            status: slot(),
            counted: slot(),
            groups: slot(),
            uniforms: vec![0; kernel.uniform_bytes as usize],
            closed: false,
            bounds_out: None,
            creations: 0,
            input_uploads: 0,
            input_uploaded_bytes: 0,
            status_reads: 0,
            readback_bytes: 0,
            bounds_readback_bytes: 0,
            status_checked: false,
            generation: 0,
            table_ready: false,
            last_epoch: 0,
            kept: Vec::new(),
            verdicts: Vec::new(),
        }),
    }))
}

pub fn create(pool_: Option<&Pool>, gpu_: &renderer::renderer::Gpu, pipelines: &pipelines::T, kernel: &Kernel) -> T {
    create_owned(pool_, &renderer::renderer::gpu_device(gpu_), &renderer::renderer::gpu_queue(gpu_), pipelines, kernel)
}

fn drain(d: &Data, st: &mut State) {
    drain_pool(&d.pool);
    st.kept.retain(|k| k.token.strong_count() > 0);
}

/// An input the device now holds elsewhere: its upload buffer and the host array it was compared to
/// would otherwise stay pinned for the runner's life.
fn forget_input(slot: &mut Slot) {
    if let Some(buffer) = &slot.buffer {
        let _ = b::destroy_buffer(buffer);
    }
    slot.buffer = None;
    slot.mapped = false;
    slot.capacity = 0;
    slot.uploaded = None;
}

fn ensure(d: &Data, slot: &mut Slot, creations: &mut i64, mapped: bool, length: i64) -> Result<Buffer, Diagnostic> {
    if mapped && slot.buffer.is_none() && length > 0 {
        // an output slot whose last buffer a holder took: a recycled buffer of the size class
        if let Some((buffer, capacity)) = recycled(&d.pool, &d.device, length) {
            slot.buffer = Some(buffer);
            slot.mapped = true;
            slot.capacity = capacity;
            mem::charge(&SLOT_OWNER, capacity);
            slot.uploaded = None;
        }
    }
    // a slot that has asked for under a quarter of its size for this long comes back down
    if length > 0 && length * 4 < slot.capacity && slot.capacity > slot_shrink_floor {
        slot.low += 1;
        if slot.low >= slot_shrink_after {
            if let Some(buffer) = &slot.buffer {
                let _ = b::destroy_buffer(buffer);
            }
            mem::release(&SLOT_OWNER, slot.capacity);
            slot.buffer = None;
            slot.mapped = false;
            slot.uploaded = None;
            slot.capacity = 0;
            slot.low = 0;
        }
    } else {
        slot.low = 0;
    }
    if length > slot.capacity {
        let capacity = size_class(length);
        let descriptor = BufferDescriptor { label: Some("Flow packed compute".to_owned()), size: capacity, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] };
        if capacity >= 1 << 20 {
            admit(capacity)?;
        }
        let fresh = if mapped {
            let buffer = gpu(b::create_mapped_buffer(&d.device, descriptor))?;
            created(&d.pool, capacity);
            buffer
        } else {
            gpu(b::create_buffer(&d.device, descriptor, None))?
        };
        if let Some(buffer) = &slot.buffer {
            let _ = b::destroy_buffer(buffer);
        }
        mem::release(&SLOT_OWNER, slot.capacity);
        mem::charge(&SLOT_OWNER, capacity);
        slot.uploaded = None;
        slot.buffer = Some(fresh);
        slot.capacity = capacity;
        slot.bytes = if mapped { Vec::new() } else { vec![0; capacity as usize] };
        slot.mapped = mapped;
        *creations += 1;
    }
    Ok(slot.buffer.clone().unwrap())
}

/// Encodes into `encoder` the threadgroup counts of a dispatch of at most `capacity` threads in
/// groups of `group` whose count is the uint in `cell` x mul / div (Kernels.dispatch_arguments),
/// into `groups`: its grid is sized on the device, nothing is read back. The caller sets its
/// pipeline next.
pub fn dispatch_arguments(encoder: &ComputeEncoder, pipelines: &pipelines::T, cell: &Buffer, mul: i64, div: i64, capacity: i64, group: i64, groups: &Buffer) -> Result<(), Diagnostic> {
    let compiled = pipelines::get(pipelines, kernels::dispatch_arguments())?;
    let mut bound = [0u8; 16];
    put_i32(&mut bound, 0, capacity as i32);
    put_i32(&mut bound, 4, mul as i32);
    put_i32(&mut bound, 8, div as i32);
    put_i32(&mut bound, 12, group as i32);
    gpu(b::set_pipeline(encoder, &compiled.pipeline))?;
    gpu(b::set_buffer(encoder, 0, cell))?;
    gpu(b::set_bytes(encoder, 1, &bound))?;
    gpu(b::set_buffer(encoder, 2, groups))?;
    gpu(b::dispatch_threads(encoder, (1, 1, 1), (1, 1, 1)))
}

// ---- dispatch

/// The optional arguments of [`dispatch`] (`bound`/`bound_table` are separate parameters).
#[derive(Clone, Default)]
pub struct Options {
    /// default true; false leaves the dispatch in flight, without its status read
    pub wait: Option<bool>,
    /// default true; false keeps ORed error flags from earlier encodings on this runner
    pub reset_status: Option<bool>,
    pub uint32_cpu: bool,
    /// only encodes into the caller's command buffer (epoch 0 until the caller commits it)
    pub commands: Option<Commands>,
    /// the caller's open compute encoder (the caller ends it)
    pub encoder: Option<ComputeEncoder>,
    /// bounds a fast kernel's count by `cell * mul / div` on the device, and its grid too
    pub count: Option<(Buffer, i64, i64)>,
    /// writes a one-output fast kernel's output and count into the caller's buffers
    pub into: Option<(Buffer, Buffer)>,
    /// with `into`, the buffers of the outputs past the first
    pub into_more: Vec<Buffer>,
    pub scene: Option<Scene>,
    pub extras: Option<Extras>,
    /// the bottom-level structures an instanced scene references, declared read so they stay resident
    pub resident: Vec<Accel>,
}

pub type Bound<'a> = Option<&'a dyn Fn(i64) -> Option<Buffer>>;
pub type BoundTable<'a> = Option<&'a dyn Fn(i64) -> Option<(Buffer, i64)>>;

enum SceneTail {
    Buffer(Buffer),
    Accel(Accel),
}

pub fn dispatch(t: &T, o: &Options, explicit_input: Bound, explicit_table: BoundTable, values: &flow_ir::packed::host::Inputs) -> Result<Output, Diagnostic> {
    let d = &*t.0;
    let kernel = &d.kernel;
    let wait = o.wait.unwrap_or(true);
    let reset_status = o.reset_status.unwrap_or(true);
    let explicit_input_f = |i: i64| explicit_input.and_then(|f| f(i));
    let explicit_table_f = |u: i64| explicit_table.and_then(|f| f(u));
    // a table another runner keeps on the device (its placeholder) is bound to its buffer
    let bound_table = |u: i64| -> Option<(Buffer, i64)> {
        if let Some(found) = explicit_table_f(u) {
            return Some(found);
        }
        let uint32 = kernel.gathers.iter().position(|(index, _)| *index == u).is_some_and(|k| kernel.gather_uint32[k]);
        if u >= 0 && (u as usize) < values.tables.len() && values.tables[u as usize].len() <= 4 {
            return typed_held(&values.tables[u as usize], uint32);
        }
        None
    };
    // a source another runner keeps on the device (its placeholder) is bound to its buffer
    let bound = |index: i64| -> Option<Buffer> {
        if let Some(found) = explicit_input_f(index) {
            return Some(found);
        }
        let uint32 = index >= 0 && (index as usize) < kernel.input_uint32.len() && kernel.input_uint32[index as usize];
        if index >= 0 && (index as usize) < values.arrays.len() && values.arrays[index as usize].len() <= 4 {
            let width = kernel.input_widths[(index as usize).min(kernel.input_widths.len().saturating_sub(1))];
            return held_input(&values.arrays[index as usize], values.count * width, uint32);
        }
        None
    };
    let empty_extras = Extras::default();
    let extras = o.extras.as_ref().unwrap_or(&empty_extras);
    // the scene's buffers after its spheres, in Kernel_layout's order
    let scene_tail: Vec<SceneTail> = match &o.scene {
        None => vec![],
        Some(s) => {
            let mut v: Vec<SceneTail> = [&s.instances, &extras.vertices_end, &extras.strands].into_iter().flatten().map(|x| SceneTail::Buffer(x.clone())).collect();
            v.extend([&extras.curves, &extras.prototypes].into_iter().flatten().map(|a| SceneTail::Accel(a.clone())));
            v
        }
    };
    let error = |m: &str| -> Result<Output, Diagnostic> { Err(e_kernel(m)) };
    let closed = d.state.borrow().closed;
    if closed || me() != d.domain {
        return error("GPU program owner is closed or called from another domain.");
    }
    let widest = kernel.output_widths.iter().copied().fold(1, i64::max);
    if values.count < 0 || values.count > i32::MAX as i64 || values.count > MAX_STRING / 4 / widest {
        return error("GPU count exceeds native storage bounds.");
    }
    if kernel.scene.is_some() != o.scene.is_some()
        || match kernel.scene {
            None => false,
            Some(base) => kernel.interface.iter().filter(|x| x.binding > base + 2).count() > scene_tail.len(),
        }
    {
        return error("GPU scene binding changed.");
    }
    let n_inputs = d.state.borrow().inputs.len();
    if values.arrays.len() != n_inputs {
        return error("GPU input arity changed.");
    }
    if values.count > 0 {
        let mut missing = false;
        for (i, typed) in kernel.input_uint32.iter().enumerate() {
            if *typed && bound(i as i64).is_none() && (!o.uint32_cpu || held(&values.arrays[i]).is_some()) {
                missing = true;
            }
        }
        for (k, typed) in kernel.gather_uint32.iter().enumerate() {
            let u = kernel.gathers[k].0;
            if *typed && bound_table(u).is_none() && (!o.uint32_cpu || u as usize >= values.tables.len() || held(&values.tables[u as usize]).is_some()) {
                missing = true;
            }
        }
        if missing {
            return Err(value::fail("E_GPU_UINT32_BINDING", "Uint32 inputs require explicit device buffers."));
        }
    }
    for index in 0..n_inputs {
        if bound(index as i64).is_none() && (values.arrays[index].len() as i64) < values.count * kernel.input_widths[index] {
            return error("GPU input count changed.");
        }
    }
    // Invalidate borrowed output before reallocating or submitting any write, including
    // dispatches whose completion subsequently fails.
    let mut guard = d.state.borrow_mut();
    let st = &mut *guard;
    st.generation += 1;
    drain(d, st);
    if values.count == 0 {
        return Ok(Output { owner: t.clone(), count: 0, width: kernel.output_widths[0], index: 0, gpu_seconds: None, generation: st.generation, epoch: st.last_epoch, check_status: false });
    }
    let compiled = pipelines::get(&d.pipelines, kernel)?;
    put_i32(&mut st.uniforms, 0, values.count as i32);
    for e in &d.layout {
        match *e {
            Entry::Uniform_at(index, offset, width) => {
                if index >= values.uniforms.len() || values.uniforms[index].len() != width {
                    return Err(e_kernel("GPU uniform type changed."));
                }
                for component in 0..width {
                    let at = offset + component * 4;
                    if kernel.uint32_uniforms.contains(&(at as i64)) {
                        pack_uint32(&mut st.uniforms, at, values.uniforms[index][component])?;
                    } else {
                        pack(&mut st.uniforms, at, values.uniforms[index][component])?;
                    }
                }
            }
            Entry::Gather_at(k, offset) => {
                let (u, width) = kernel.gathers[k];
                if u as usize >= values.tables.len() {
                    return Err(e_kernel("GPU gather table changed."));
                }
                let length = match bound_table(u) {
                    Some((_, length)) => length,
                    None => values.tables[u as usize].len() as i64,
                };
                put_i32(&mut st.uniforms, offset, (length / width) as i32);
            }
            Entry::Strands_at(offset) => put_i32(&mut st.uniforms, offset, extras.strand_count as i32),
            Entry::Spheres_at(offset) => put_i32(&mut st.uniforms, offset, o.scene.as_ref().map_or(0, |s| s.sphere_count) as i32),
            Entry::Frame_at(index, offset) => {
                if index >= values.frame.len() {
                    return Err(e_kernel("GPU frame uniform changed."));
                }
                if kernel.uint32_uniforms.contains(&(offset as i64)) {
                    pack_uint32(&mut st.uniforms, offset, values.frame[index])?;
                } else {
                    pack(&mut st.uniforms, offset, values.frame[index])?;
                }
            }
        }
    }
    let captures: Vec<usize> = d.layout.iter().flat_map(|e| match *e {
        Entry::Uniform_at(_, offset, width) => (0..width).map(|c| offset + 4 * c).collect(),
        _ => vec![],
    }).collect();
    let compiled = pipelines::specialized(&d.pipelines, kernel, &captures, &st.uniforms).unwrap_or(compiled);
    // a buffer this runner's last dispatch still reads is not rewritten under it
    if o.commands.is_none() && st.last_epoch > b::completed_epoch(&d.queue) {
        gpu(b::complete_through(&d.queue, st.last_epoch))?;
    }
    let mut buffers: Vec<Buffer> = Vec::with_capacity(n_inputs);
    for index in 0..n_inputs {
        match bound(index as i64) {
            Some(buffer) => {
                forget_input(&mut st.inputs[index]);
                buffers.push(buffer);
            }
            None => {
                let length = values.count * kernel.input_widths[index] * 4;
                let buffer = ensure(d, &mut st.inputs[index], &mut st.creations, !kernel.input_uint32[index], length)?;
                let source = &values.arrays[index];
                let same = matches!(&st.inputs[index].uploaded, Some((old, covered)) if *covered == length && same_values(old, source));
                if !same {
                    // A failed write may have partially replaced the previous source.
                    st.inputs[index].uploaded = None;
                    if kernel.input_uint32[index] {
                        upload_uint32(&buffer, &mut st.inputs[index], source, (length / 4) as usize)?;
                    } else {
                        pack_floats(&view(&buffer), source, (length / 4) as usize)?;
                        gpu(b::flush_mapped(&buffer, length))?;
                    }
                    st.inputs[index].uploaded = Some((source.clone(), length));
                    st.input_uploads += 1;
                    st.input_uploaded_bytes += length;
                }
                buffers.push(buffer);
            }
        }
    }
    let outputs: Vec<Buffer> = match &o.into {
        Some((output, _)) if st.outputs.len() == 1 + o.into_more.len() => std::iter::once(output.clone()).chain(o.into_more.iter().cloned()).collect(),
        _ => {
            let mut v = Vec::new();
            for k in 0..st.outputs.len() {
                v.push(ensure(d, &mut st.outputs[k], &mut st.creations, true, values.count * kernel.output_widths[k] * 4)?);
            }
            v
        }
    };
    let status = ensure(d, &mut st.status, &mut st.creations, false, 4)?;
    // the device count words and cells of a fast kernel; without a `count` its own cell stands in
    let counted: Option<(Buffer, Buffer)> = match kernel.counts {
        None => None,
        Some(offset) => {
            let offset = offset as usize;
            let out = match &o.into {
                Some((_, cell)) => cell.clone(),
                None => ensure(d, &mut st.counted, &mut st.creations, false, 24)?,
            };
            let (cell, mul, div, used) = match &o.count {
                Some((cell, mul, div)) => (cell.clone(), *mul, *div, 1),
                None => (out.clone(), 1, 1, 0),
            };
            put_i32(&mut st.uniforms, offset, mul as i32);
            put_i32(&mut st.uniforms, offset + 4, div as i32);
            put_i32(&mut st.uniforms, offset + 8, used);
            Some((cell, out))
        }
    };
    if reset_status {
        // the last dispatch's status, if no one settled it, is kept before this one resets it
        if !st.status_checked && st.last_epoch > 0 && st.status.buffer.is_some() {
            if st.last_epoch > b::completed_epoch(&d.queue) {
                gpu(b::complete_through(&d.queue, st.last_epoch))?;
            }
            let flags = get_i32(&gpu(b::read_buffer(&status, 0, 4))?, 0);
            st.status_reads += 1;
            if flags != 0 {
                st.verdicts.push((st.last_epoch, kernel_status(flags)));
                let stale = st.verdicts.len().saturating_sub(8);
                st.verdicts.drain(..stale);
            }
        }
        put_i32(&mut st.status.bytes, 0, 0);
        gpu(b::write_buffer(&status, 0, &st.status.bytes))?;
        st.status_checked = false;
    }
    let table = if kernel.table_seeds.is_empty() {
        None
    } else {
        let length = kernel.table_seeds.len() as i64 * 512 * 4;
        let fresh = !st.table_ready;
        let buffer = ensure(d, &mut st.table, &mut st.creations, false, length)?;
        if fresh {
            for (index, seed) in kernel.table_seeds.iter().enumerate() {
                let permutation = rays_math::noise::permutation(&rays_math::noise::create(*seed));
                for (entry, v) in permutation.iter().enumerate() {
                    put_i32(&mut st.table.bytes, (index * 512 + entry) * 4, *v as i32);
                }
            }
            gpu(b::write_buffer(&buffer, 0, &st.table.bytes))?;
            st.table_ready = true;
        }
        Some(buffer)
    };
    // the tables gathers read, uploaded when their array changed
    let mut gathers: Vec<Buffer> = Vec::new();
    for k in 0..st.gathers.len() {
        let u = kernel.gathers[k].0;
        match bound_table(u) {
            Some((buffer, _)) => gathers.push(buffer),
            None => {
                let source = &values.tables[u as usize];
                let length = 4.max(source.len() as i64 * 4);
                let buffer = ensure(d, &mut st.gathers[k], &mut st.creations, !kernel.gather_uint32[k], length)?;
                let same = matches!(&st.gathers[k].uploaded, Some((old, covered)) if *covered == length && same_values(old, source));
                if !same {
                    st.gathers[k].uploaded = None;
                    if kernel.gather_uint32[k] {
                        upload_uint32(&buffer, &mut st.gathers[k], source, source.len())?;
                    } else {
                        pack_floats(&view(&buffer), source, source.len())?;
                        gpu(b::flush_mapped(&buffer, source.len() as i64 * 4))?;
                    }
                    st.gathers[k].uploaded = Some((source.clone(), length));
                    st.input_uploads += 1;
                    st.input_uploaded_bytes += length;
                }
                gathers.push(buffer);
            }
        }
    }
    let groups_buffer = if counted.is_some() && o.count.is_some() { Some(ensure(d, &mut st.groups, &mut st.creations, false, 12)?) } else { None };
    let uniforms = st.uniforms.clone();
    let encode = |commands: &Commands| -> Result<(), Diagnostic> {
        let shared = o.encoder.clone();
        let encoder = match &shared {
            Some(encoder) => encoder.clone(),
            None => gpu(b::compute_encoder(commands))?,
        };
        let result = (|| -> Result<(), Diagnostic> {
            // a device count sizes the grid on the device: groups of 256 to it, not the capacity
            let groups = match (&counted, &o.count, &groups_buffer) {
                (Some((cell, _)), Some((_, mul, div)), Some(groups)) => {
                    dispatch_arguments(&encoder, &d.pipelines, cell, *mul, *div, values.count, 256, groups)?;
                    Some(groups.clone())
                }
                _ => None,
            };
            gpu(b::set_pipeline(&encoder, &compiled.pipeline))?;
            for (index, buffer) in buffers.iter().enumerate() {
                gpu(b::set_buffer(&encoder, index as i64, buffer))?;
            }
            let index = buffers.len() as i64;
            for (k, output) in outputs.iter().enumerate() {
                gpu(b::set_buffer(&encoder, index + k as i64, output))?;
            }
            let index = index + outputs.len() as i64;
            gpu(b::set_bytes(&encoder, index, &uniforms))?;
            gpu(b::set_buffer(&encoder, index + 1, &status))?;
            if let Some(table) = &table {
                gpu(b::set_buffer(&encoder, index + 2, table))?;
            }
            let index = index + 2 + if table.is_none() { 0 } else { 1 };
            for (k, buffer) in gathers.iter().enumerate() {
                gpu(b::set_buffer(&encoder, index + k as i64, buffer))?;
            }
            let index = index + gathers.len() as i64;
            if let Some((cell, out)) = &counted {
                gpu(b::set_buffer(&encoder, index, cell))?;
                gpu(b::set_buffer(&encoder, index + 1, out))?;
            }
            if let Some(s) = &o.scene {
                let index = kernel.scene.unwrap();
                gpu(b::set_accel(&encoder, index, &s.accel))?;
                gpu(b::set_buffer(&encoder, index + 1, &s.vertices))?;
                gpu(b::set_buffer(&encoder, index + 2, &s.spheres))?;
                for (k, tail) in scene_tail.iter().enumerate() {
                    match tail {
                        SceneTail::Buffer(buffer) => gpu(b::set_buffer(&encoder, index + 3 + k as i64, buffer))?,
                        SceneTail::Accel(a) => gpu(b::set_accel(&encoder, index + 3 + k as i64, a))?,
                    }
                }
                let mut used = vec![s.accel.clone()];
                used.extend(extras.curves.iter().cloned());
                used.extend(extras.prototypes.iter().cloned());
                used.extend(o.resident.iter().cloned());
                gpu(b::compute_use_accels(&encoder, &used))?;
            }
            match groups {
                Some(groups) => gpu(b::dispatch_threadgroups_indirect(&encoder, &groups, 0, (256, 1, 1))),
                None => gpu(b::dispatch_threads(&encoder, (values.count, 1, 1), (256, 1, 1))),
            }
        })();
        if shared.is_none() {
            let _ = b::end_compute(&encoder);
        }
        result
    };
    let width = kernel.output_widths[0];
    match &o.commands {
        Some(commands) => {
            encode(commands)?;
            Ok(Output { owner: t.clone(), count: values.count, width, index: 0, gpu_seconds: None, generation: st.generation, epoch: 0, check_status: true })
        }
        None => {
            let commands = gpu(b::begin_commands(&d.queue))?;
            let committed = (|| -> Result<b::Receipt, Diagnostic> {
                encode(&commands)?;
                gpu(b::commit(&commands))
            })();
            let receipt = match committed {
                Ok(r) => r,
                Err(e) => {
                    let _ = b::abandon(&commands);
                    return Err(e);
                }
            };
            st.last_epoch = receipt.epoch;
            if !wait {
                return Ok(Output { owner: t.clone(), count: values.count, width, index: 0, gpu_seconds: None, generation: st.generation, epoch: receipt.epoch, check_status: true });
            }
            gpu(b::complete_through(&d.queue, receipt.epoch))?;
            let flags = gpu(b::read_buffer(&status, 0, 4))?;
            st.status_reads += 1;
            let flags = get_i32(&flags, 0);
            if flags != 0 {
                return Err(kernel_status(flags));
            }
            st.status_checked = true;
            Ok(Output { owner: t.clone(), count: values.count, width, index: 0, gpu_seconds: b::gpu_duration(&d.queue, receipt), generation: st.generation, epoch: receipt.epoch, check_status: true })
        }
    }
}

/// A fused producer's output; an invalid index returns `None`. The selected view shares the
/// producer's lifetime and dispatch generation.
pub fn select_output(index: i64, output: &Output) -> Option<Output> {
    let st = output.owner.0.state.borrow();
    if index < 0 || index as usize >= st.outputs.len() {
        None
    } else {
        Some(Output { index, width: output.owner.0.kernel.output_widths[index as usize], ..output.clone() })
    }
}

pub(crate) fn check_status(t: &T, epoch: i64) -> Result<(), Diagnostic> {
    let d = &*t.0;
    let mut guard = d.state.borrow_mut();
    let st = &mut *guard;
    if epoch > b::completed_epoch(&d.queue) {
        gpu(b::complete_through(&d.queue, epoch))?;
    }
    if epoch < st.last_epoch {
        return match st.verdicts.iter().rev().find(|(e, _)| *e == epoch) {
            Some((_, diagnostic)) => Err(diagnostic.clone()),
            None => Ok(()),
        };
    }
    if !st.status_checked {
        let flags = gpu(b::read_buffer(st.status.buffer.as_ref().unwrap(), 0, 4))?;
        st.status_reads += 1;
        let flags = get_i32(&flags, 0);
        if flags != 0 {
            return Err(kernel_status(flags));
        }
        st.status_checked = true;
    }
    Ok(())
}

enum Contents {
    Floats(b::Floats, i64),
    Bytes(Vec<u8>, i64),
}

fn closed_output() -> Diagnostic {
    e_gpu("GPU output was closed or superseded.")
}

/// A completed, status-checked output's slot buffer and width.
fn ready(output: &Output, index: Option<i64>) -> Result<(Option<Buffer>, bool, i64), Diagnostic> {
    let index = index.unwrap_or(output.index);
    let d = &*output.owner.0;
    {
        let st = d.state.borrow();
        if st.closed || me() != d.domain || output.generation != st.generation {
            return Err(closed_output());
        }
        if index < 0 || index as usize >= st.outputs.len() {
            return Err(e_gpu("GPU output index is outside its producer."));
        }
    }
    // a `wait:false` dispatch is read once its queue has completed it
    if output.epoch > b::completed_epoch(&d.queue) {
        gpu(b::complete_through(&d.queue, output.epoch))?;
    }
    if output.check_status {
        check_status(&output.owner, output.epoch)?;
    }
    let st = d.state.borrow();
    let slot = &st.outputs[index as usize];
    Ok((slot.buffer.clone(), slot.mapped, d.kernel.output_widths[index as usize]))
}

/// A completed, status-checked output's mapped contents, `count * width` floats.
fn completed(output: &Output, index: Option<i64>) -> Result<Contents, Diagnostic> {
    let (buffer, mapped, width) = ready(output, index)?;
    let length = output.count * width;
    output.owner.0.state.borrow_mut().readback_bytes += length * 4;
    match buffer {
        Some(buffer) if mapped && length > 0 => {
            gpu(b::invalidate_mapped(&buffer, length * 4))?;
            Ok(Contents::Floats(view(&buffer), width))
        }
        _ => {
            if length == 0 {
                Ok(Contents::Floats(Rc::new(RefCell::new(Vec::new())), width))
            } else {
                Ok(Contents::Bytes(gpu(b::read_buffer(buffer.as_ref().unwrap(), 0, length * 4))?, width))
            }
        }
    }
}

fn nonfinite_output<X>() -> Result<X, Diagnostic> {
    Err(e_kernel("GPU output contains a nonfinite value."))
}

fn uint32_output(output: &Output, index: Option<i64>) -> bool {
    let index = index.unwrap_or(output.index);
    index >= 0 && (index as usize) < output.owner.0.kernel.output_uint32.len() && output.owner.0.kernel.output_uint32[index as usize]
}

/// read a device buffer the queue has finished writing
pub fn read_uint32(queue: &Queue, buffer: &Buffer, epoch: i64, length: i64) -> Result<Vec<i64>, Diagnostic> {
    if epoch > b::completed_epoch(queue) {
        gpu(b::complete_through(queue, epoch))?;
    }
    if length == 0 {
        return Ok(vec![]);
    }
    let bytes = gpu(b::read_buffer(buffer, 0, length * 4))?;
    Ok((0..length as usize).map(|i| u32::from_le_bytes(bytes[i * 4..i * 4 + 4].try_into().unwrap()) as i64).collect())
}

fn uint32_values(output: &Output, index: Option<i64>) -> Result<Vec<i64>, Diagnostic> {
    let (buffer, _, _) = ready(output, index)?;
    if output.count == 0 {
        return Ok(vec![]);
    }
    let values = read_uint32(&output.owner.0.queue, buffer.as_ref().unwrap(), output.epoch, output.count)?;
    output.owner.0.state.borrow_mut().readback_bytes += output.count * 4;
    Ok(values)
}

/// `index` overrides the selected output (initially the first). Uint32 storage returns an exact
/// `Int_array`, including values above 2^24 and through 2^32-1.
pub fn readback(output: &Output, index: Option<i64>) -> Result<Value, Diagnostic> {
    if uint32_output(output, index) {
        return uint32_values(output, index).map(|v| Value::Int_array(Arc::new(v)));
    }
    let contents = completed(output, index)?;
    let count = output.count as usize;
    let (values, finite, width) = match contents {
        Contents::Floats(floats, width) => {
            let f = floats.borrow();
            let mut finite = true;
            let values: Vec<f64> = f[..count * width as usize]
                .iter()
                .map(|v| {
                    if !v.is_finite() {
                        finite = false;
                    }
                    *v as f64
                })
                .collect();
            (values, finite, width)
        }
        Contents::Bytes(bytes, width) => {
            let (values, finite) = unpack_array(&bytes, count * width as usize);
            (values, finite, width)
        }
    };
    if !finite {
        return nonfinite_output();
    }
    let a = Arc::new(values);
    Ok(Value::Floats(width as usize, a))
}

/// `width` planes of `count` doubles from float32 contents.
fn to_planes(floats: &b::Floats, count: i64, width: i64, checked: bool) -> Result<Vec<Vec<f64>>, Diagnostic> {
    let f = floats.borrow();
    let (count, width) = (count as usize, width as usize);
    let mut finite = true;
    let planes: Vec<Vec<f64>> = (0..width)
        .map(|component| {
            (0..count)
                .map(|i| {
                    let v = f[i * width + component];
                    if !checked && !v.is_finite() {
                        finite = false;
                    }
                    v as f64
                })
                .collect()
        })
        .collect();
    if finite { Ok(planes) } else { nonfinite_output() }
}

/// The output as `width` owned planes of `count` doubles (component `c` of element `i` at
/// `planes[c][i]`), with the finite check folded in.
pub fn planes(output: &Output, index: Option<i64>) -> Result<Vec<Vec<f64>>, Diagnostic> {
    if uint32_output(output, index) {
        return uint32_values(output, index).map(|v| vec![v.iter().map(|x| *x as f64).collect()]);
    }
    match completed(output, index)? {
        Contents::Floats(floats, width) => to_planes(&floats, output.count, width, false),
        Contents::Bytes(bytes, width) => {
            let n = (output.count * width) as usize;
            let floats: b::Floats = Rc::new(RefCell::new((0..n).map(|i| f32::from_bits(u32::from_le_bytes(bytes[4 * i..4 * i + 4].try_into().unwrap()))).collect()));
            to_planes(&floats, output.count, width, false)
        }
    }
}

// ---- kept outputs

/// On the creating thread: the dispatch completed and its status read (once).
fn settle(t: &T, kept_epoch: i64, settled: &Arc<AtomicBool>, fate: &Fate) -> Result<(), Diagnostic> {
    if settled.load(Ordering::SeqCst) || fate.destroyed() {
        return Ok(());
    }
    if me() != t.0.domain {
        return Err(e_gpu("A device output settles on its owner's domain."));
    }
    check_status(t, kept_epoch)?;
    // every output of the dispatch: its status covered them all
    for k in t.0.state.borrow().kept.iter() {
        if k.epoch == kept_epoch {
            k.settled.store(true, Ordering::SeqCst);
        }
    }
    settled.store(true, Ordering::SeqCst);
    Ok(())
}

/// Reads a holder's planes now if it can without waiting on a state a dispatch holds.
fn thaw(kept: &Kept) {
    if !kept.settled.load(Ordering::SeqCst) && kept.owner.0.state.try_borrow_mut().is_err() {
        return;
    }
    let _ = materialise(kept);
}

/// Only immutable native allocation access crosses threads. Fate serializes conversion against
/// retirement/destruction, and settlement publishes completion before a foreign reader enters.
fn materialise_owned(fate: &Fate, shared: &Shared, id: i64) -> Result<Arc<Vec<Vec<f64>>>, String> {
    let _guard = fate.lock.lock().unwrap();
    if let Some(planes) = fate.planes() {
        return Ok(planes);
    }
    if fate.destroyed() {
        return Err("Run.materialise: a device output was read after its free".to_owned());
    }
    let length = fate.count * fate.width;
    let mut readback = fate.readback.lock().unwrap();
    let bytes = (readback.as_ref().ok_or("Run.materialise: storage was released")?)(0, (length * 4) as usize)
        .map_err(|error| ogpu_core::error::to_string(&error))?;
    let mut planes = vec![vec![0.0; fate.count as usize]; fate.width as usize];
    for (i, word) in bytes.chunks_exact(4).enumerate() {
        let word = u32::from_le_bytes(word.try_into().unwrap());
        planes[i % fate.width as usize][i / fate.width as usize] =
            if fate.uint32 { word as f64 } else { f32::from_bits(word) as f64 };
    }
    let planes = Arc::new(planes);
    *fate.planes.lock().unwrap() = Some(planes.clone());
    readback.take();
    fate.materialised_bytes.fetch_add(length * 4, Ordering::SeqCst);
    fate.materialised.fetch_add(1, Ordering::SeqCst);
    shared.released.lock().unwrap().push(id);
    Ok(planes)
}
/// The planes, on the creating thread (from any thread once read). Once read, the holder no longer
/// needs the buffer: it goes back to the pool at once, not at drop.
fn materialise(kept: &Kept) -> Result<Arc<Vec<Vec<f64>>>, String> {
    let t = &kept.owner;
    let d = &*t.0;
    if let Some(planes) = kept.fate.planes() {
        return Ok(planes);
    }
    // a reader on the creating thread waits for the kernel itself
    if !kept.settled.load(Ordering::SeqCst) {
        settle(t, kept.epoch, &kept.settled, &kept.fate).map_err(|diagnostic| rays_diagnostic::to_string(&diagnostic))?;
    }
    materialise_owned(&kept.fate, &d.pool.0.shared, kept.id)
}

// ponytail: fixed lane count (groups of 1,024 to fill an 8-core M1); tune per device if one underfills.
const BOUNDS_LANES: i64 = 16 * 1024;

/// A settled, unread vec3 output's bounds, reduced where it is (shaders/bounds.rays). Each pass
/// treats the preceding min/max pairs as float3 points, down to six floats (24 bytes) read back.
/// On the creating thread; `None` elsewhere, once read or
/// on any device failure.
fn bounds_of(kept: &Kept) -> Option<Vec<f64>> {
    let t = &kept.owner;
    let d = &*t.0;
    if kept.width != 3 || kept.count == 0 || d.state.borrow().closed || me() != d.domain || kept.fate.destroyed() || kept.fate.planes().is_some() || !kept.settled.load(Ordering::SeqCst) {
        return None;
    }
    if let Some(box_) = kept.bounds_memo.borrow().clone() {
        return box_;
    }
    let n = kept.count;
    let compute = || -> Result<Option<Vec<f64>>, Diagnostic> {
        let compiled = pipelines::get(&d.pipelines, kernels::bounds())?;
        let out = {
            let mut st = d.state.borrow_mut();
            match &st.bounds_out {
                Some(out) => out.clone(),
                None => {
                    let make = |lanes| gpu(b::create_buffer(&d.device, BufferDescriptor { label: Some("Flow bounds".to_owned()), size: lanes * 24, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc] }, None));
                    let first = make(BOUNDS_LANES)?;
                    let second = match make(1024) {
                        Ok(out) => out,
                        Err(e) => { let _ = b::destroy_buffer(&first); return Err(e); }
                    };
                    let out = [first, second];
                    st.bounds_out = Some(out.clone());
                    out
                }
            }
        };
        let commands = gpu(b::begin_commands(&d.queue))?;
        let mut input = kept.buffer.clone();
        let mut points = n;
        let mut lanes = n.min(BOUNDS_LANES);
        let mut pass = 0;
        let receipt = (|| -> Result<b::Receipt, Diagnostic> {
            loop {
                let mut count = [0u8; 8];
                put_i32(&mut count, 0, points as i32);
                put_i32(&mut count, 4, lanes as i32);
                let encoder = gpu(b::compute_encoder(&commands))?;
                let r = (|| -> Result<(), Diagnostic> {
                    gpu(b::set_pipeline(&encoder, &compiled.pipeline))?;
                    gpu(b::set_buffer(&encoder, 0, &input))?;
                    gpu(b::set_buffer(&encoder, 1, &out[pass % 2]))?;
                    gpu(b::set_bytes(&encoder, 2, &count))?;
                    gpu(b::dispatch_threads(&encoder, (lanes, 1, 1), (lanes.min(1024), 1, 1)))
                })();
                let ended = gpu(b::end_compute(&encoder));
                r?;
                ended?;
                input = out[pass % 2].clone();
                if lanes == 1 { break; }
                points = lanes * 2;
                lanes = if lanes > 1024 { 1024 } else { 1 };
                pass += 1;
            }
            gpu(b::commit(&commands))
        })();
        let receipt = match receipt {
            Ok(r) => r,
            Err(e) => {
                let _ = b::abandon(&commands);
                return Err(e);
            }
        };
        gpu(b::complete_through(&d.queue, receipt.epoch))?;
        let (bounds, finite) = unpack_array(&gpu(b::read_buffer(&input, 0, 24))?, 6);
        {
            let mut st = d.state.borrow_mut();
            st.readback_bytes += 24;
            st.bounds_readback_bytes += 24;
        }
        if !finite {
            return Ok(None);
        }
        Ok(Some(bounds))
    };
    let result = compute().unwrap_or(None);
    *kept.bounds_memo.borrow_mut() = Some(result.clone());
    result
}

/// Its owner (a session dropping the result) no longer needs it, from any thread. `held` (it was
/// handed out, now or before): it is retired, still readable; its buffer comes back when it is
/// dropped or read, or when `reclaim` reads it. Otherwise no one reads it again: its buffer comes
/// back at once.
fn release_kept(shared: &Shared, fate: &Fate, id: i64, weak: Weak<Token>, held: bool) {
    if fate.planes().is_none() {
        let retire = {
            let _g = fate.lock.lock().unwrap();
            if fate.destroyed() {
                false
            } else if held || fate.retired.load(Ordering::SeqCst) {
                fate.retired.store(true, Ordering::SeqCst);
                true
            } else {
                fate.destroy();
                shared.freed.fetch_add(1, Ordering::SeqCst);
                shared.released.lock().unwrap().push(id);
                false
            }
        };
        if retire {
            shared.retiring.lock().unwrap().push((id, weak));
        }
    }
}

/// A read another thread asked the owner for (OGPU handles are `Rc`, so only the owner can touch the
/// buffer). The owner answers in `service`; the asker sleeps on `done`.
struct Request {
    owner: ThreadId,
    id: i64,
    result: Mutex<Option<Result<Arc<Vec<Vec<f64>>>, String>>>,
    done: Condvar,
}

static REQUESTS: Mutex<Vec<Arc<Request>>> = Mutex::new(Vec::new());

fn answer(request: &Request, result: Result<Arc<Vec<Vec<f64>>>, String>) {
    *request.result.lock().unwrap() = Some(result);
    request.done.notify_all();
}

/// Owner thread: answers the reads other threads asked for (every `materialise` off the creating
/// thread waits for this). Call it wherever the owner waits or pumps; `Workspace_gpu.pump` does.
pub fn service() {
    let mine: Vec<Arc<Request>> = {
        let mut all = REQUESTS.lock().unwrap();
        let (mine, rest): (Vec<_>, Vec<_>) = all.drain(..).partition(|r| r.owner == me());
        *all = rest;
        mine
    };
    for request in mine {
        let result = match kept_of(request.id) {
            Some(kept) => materialise(&kept),
            None => Err("Run.materialise: a device output was read after its owner closed".to_owned()),
        };
        answer(&request, result);
    }
}

fn token_planes(token: &Token) -> Result<Arc<Vec<Vec<f64>>>, String> {
    if let Some(p) = token.fate.planes() {
        return Ok(p);
    }
    if token.settled.load(Ordering::SeqCst) {
        return materialise_owned(&token.fate, &token.shared, token.id);
    }
    if me() == token.owner {
        return match kept_of(token.id) {
            Some(kept) => materialise(&kept),
            None => Err("Run.materialise: a device output was read after its owner closed".to_owned()),
        };
    }
    let request = Arc::new(Request { owner: token.owner, id: token.id, result: Mutex::new(None), done: Condvar::new() });
    REQUESTS.lock().unwrap().push(request.clone());
    let mut slot = request.result.lock().unwrap();
    while slot.is_none() {
        slot = request.done.wait(slot).unwrap();
    }
    slot.take().unwrap()
}

/// A held output's planes: its token keeps the buffer alive, and every call reads the token's fate.
struct Held(Arc<Token>);

impl gpu::PlanesOps for Held {
    fn materialise(&self) -> Result<Vec<Vec<f64>>, Diagnostic> {
        token_planes(&self.0).map(|planes| (*planes).clone()).map_err(|message| e_gpu(&message))
    }
    fn settle(&self) -> Result<(), Diagnostic> {
        let t = &self.0;
        if t.fate.planes().is_some() || t.fate.destroyed() {
            return Ok(());
        }
        match kept_of(t.id) {
            Some(k) if me() == t.owner => settle(&k.owner, k.epoch, &k.settled, &k.fate),
            _ if t.settled.load(Ordering::SeqCst) => Ok(()),
            _ => Err(e_gpu("A device output settles on its owner's domain.")),
        }
    }
    fn release(&self, held: bool) {
        release_kept(&self.0.shared, &self.0.fate, self.0.id, Arc::downgrade(&self.0), held)
    }
    fn bounds(&self) -> Option<Vec<f64>> {
        kept_of(self.0.id).and_then(|k| bounds_of(&k))
    }
}

fn hold(t: &T, uint32: bool, buffer: &Buffer, capacity: i64, count: i64, width: i64, epoch: i64, settled: bool) -> (gpu::Planes, Rc<Kept>) {
    let d = &*t.0;
    mem::charge(&HELD_OWNER, capacity);
    let id = NEXT_KEPT.fetch_add(1, Ordering::SeqCst);
    let fate = Arc::new(Fate {
        lock: Mutex::new(()), planes: Mutex::new(None),
        destroyed: AtomicBool::new(false), retired: AtomicBool::new(false), readers: AtomicI64::new(0),
        readback: Mutex::new(Some(b::owned_readback(buffer).expect("kept output owns mapped storage"))),
        count, width, uint32,
        materialised: d.materialised.clone(), materialised_bytes: d.materialised_bytes.clone(),
    });
    let settled = Arc::new(AtomicBool::new(settled));
    let placeholder = Arc::new(vec![id as f64; width as usize]);
    let kept = Rc::new(Kept {
        id,
        buffer: buffer.clone(),
        count,
        width,
        uint32,
        fate: fate.clone(),
        placeholder: uint32.then(|| placeholder.clone()),
        epoch,
        settled: settled.clone(),
        bounds_memo: RefCell::new(None),
        owner: t.clone(),
    });
    let token = Arc::new(Token { id, owner: d.domain, fate: fate.clone(), shared: d.pool.0.shared.clone(), settled: settled.clone() });
    let weak = Arc::downgrade(&token);
    d.state.borrow_mut().kept.push(KeptRef { epoch, settled: settled.clone(), token: weak.clone(), id });
    d.pool.0.s.borrow_mut().held.insert(id, (buffer.clone(), capacity, fate.clone()));
    KEPT.with(|k| k.borrow_mut().insert(id, kept.clone()));
    BINDABLE.with(|b| b.borrow_mut().push((Arc::downgrade(&placeholder), weak.clone(), id, count * width)));
    let resident = Value::Floats(width as usize, placeholder);
    let planes = gpu::Planes { count, width, resident: if uint32 { None } else { Some(resident) }, ops: Box::new(Held(token)) };
    (planes, kept)
}


/// Hand the output's buffer to a lazy planar store: nothing is read or waited for now.
fn keep_storage(uint32: bool, index: Option<i64>, output: &Output) -> Result<(gpu::Planes, Option<Rc<Kept>>), Diagnostic> {
    let index = index.unwrap_or(output.index);
    let t = &output.owner;
    let d = &*t.0;
    {
        let st = d.state.borrow();
        if st.closed || me() != d.domain || output.generation != st.generation {
            return Err(closed_output());
        }
        if index < 0 || index as usize >= st.outputs.len() {
            return Err(e_gpu("GPU output index is outside its producer."));
        }
    }
    if uint32_output(output, Some(index)) != uint32 {
        return Err(value::fail(
            "E_GPU_UINT32_KEEP",
            if uint32 { "Expected a uint32 output for a typed topology holder." } else { "Uint32 outputs require a typed topology holder." },
        ));
    }
    let width = d.kernel.output_widths[index as usize];
    let taken = {
        let mut st = d.state.borrow_mut();
        let slot = &mut st.outputs[index as usize];
        match &slot.buffer {
            Some(buffer) if slot.mapped && output.count > 0 && output.epoch > 0 => {
                let (buffer, capacity) = (buffer.clone(), slot.capacity);
                mem::release(&SLOT_OWNER, slot.capacity);
                slot.buffer = None;
                slot.mapped = false;
                slot.capacity = 0;
                slot.uploaded = None;
                Some((buffer, capacity))
            }
            _ => None,
        }
    };
    match taken {
        Some((buffer, capacity)) => {
            let (planes, kept) = hold(t, uint32, &buffer, capacity, output.count, width, output.epoch, false);
            Ok((planes, Some(kept)))
        }
        None => {
            // an empty, unmapped or caller-encoded output is read now
            let read = self::planes(output, Some(index))?;
            Ok((gpu::Planes { count: output.count, width, resident: None, ops: Box::new(gpu::ReadPlanes(Arc::new(read))) }, None))
        }
    }
}

/// Hand an output, possibly still in flight, to a lazy planar store without waiting. `settle` (on
/// the creating thread) waits for the dispatch and reads its status once; `materialise` reads
/// `planes` once. `release(true)` retires it; `release(false)` frees it. Uint32 outputs refuse with
/// `E_GPU_UINT32_KEEP` until adopted by a typed topology holder.
pub fn keep(output: &Output, index: Option<i64>) -> Result<gpu::Planes, Diagnostic> {
    keep_storage(false, index, output).map(|(planes, _)| planes)
}

/// The buffer, borrowed: `valid` until the unpin or its destruction. Closes over the fate only.
pub type Pin = (Buffer, i64, Box<dyn Fn() -> bool>, Box<dyn Fn()>);

fn borrow(kept: &Kept) -> Pin {
    let fate = kept.fate.clone();
    fate.readers.fetch_add(1, Ordering::SeqCst);
    let pinned = Arc::new(AtomicBool::new(true));
    let (p2, f2, f3) = (pinned.clone(), fate.clone(), fate);
    (
        kept.buffer.clone(),
        kept.count,
        Box::new(move || p2.load(Ordering::SeqCst) && !f2.destroyed()),
        Box::new(move || {
            if pinned.swap(false, Ordering::SeqCst) {
                f3.readers.fetch_sub(1, Ordering::SeqCst);
            }
        }),
    )
}

fn pin_kept(kept: &Kept) -> Option<Pin> {
    if kept.fate.destroyed() || !kept.settled.load(Ordering::SeqCst) || kept.fate.planes().is_some() {
        None
    } else {
        Some(borrow(kept))
    }
}

#[derive(Clone)]
pub struct Uint32Holder {
    pub count: i64,
    pub resident: Option<Value>,
    input: Option<Arc<Vec<f64>>>,
    pub materialise: Arc<dyn Fn() -> Result<Vec<i64>, Diagnostic> + Send + Sync>,
    pub settle: Arc<dyn Fn() -> Result<(), Diagnostic> + Send + Sync>,
    pub release: Arc<dyn Fn(bool) + Send + Sync>,
    pub pin: Arc<dyn Fn() -> Option<Pin> + Send + Sync>,
}

/// A holder's backend-neutral integers: `live` is its storage while that is still held.
struct HolderIntegers { holder: Arc<Uint32Holder>, live: Option<(Arc<Fate>, Arc<AtomicBool>)> }

impl gpu::IntegersOps for HolderIntegers {
    fn resident(&self) -> Option<Value> {
        self.live.as_ref().filter(|(f, s)| s.load(Ordering::SeqCst) && !f.destroyed() && f.planes().is_none()).and_then(|_| self.holder.resident.clone())
    }
    fn materialise(&self) -> Result<Vec<i64>, Diagnostic> { (self.holder.materialise)() }
    fn release(&self, held: bool) { (self.holder.release)(held) }
}

/// The same holder through the backend-neutral integer carrier, with shared ownership.
pub fn integers(holder: &Uint32Holder) -> gpu::Integers {
    let live = holder.input.as_ref().and_then(held_storage).map(|(kept, _)| (kept.fate.clone(), kept.settled.clone()));
    let holder = Arc::new(holder.clone());
    gpu::Integers { count: holder.count, storage: Some(holder.clone()), ops: Box::new(HolderIntegers { holder, live }) }
}

/// Borrow native UInt32 storage without materialising it. Other backend tokens decline.
pub fn pin_integers(holder: &gpu::Integers) -> Option<Pin> {
    holder.storage.as_ref()?.downcast_ref::<Uint32Holder>().and_then(|native| (native.pin)())
}

fn integer_holder(domain: ThreadId, planes: gpu::Planes, kept: Option<Rc<Kept>>) -> Uint32Holder {
    let cache: Arc<Mutex<Option<Arc<Vec<i64>>>>> = Arc::new(Mutex::new(None));
    let kept_id = kept.as_ref().map(|k| k.id);
    let (count, ops): (i64, Arc<dyn gpu::PlanesOps>) = (planes.count, Arc::from(planes.ops));
    let input = kept.as_ref().and_then(|k| k.placeholder.clone());
    let resident = kept_id.map(|id| {
        let values = Arc::new(vec![id]);
        flow_ir::packed::host::register_integer_input(&values, input.as_ref().unwrap());
        Value::Int_array(values)
    });
    let (m, s) = (ops.clone(), ops.clone());
    Uint32Holder {
        count,
        resident,
        input,
        materialise: Arc::new(move || {
            // not under the lock: a foreign reader waits for the owner, who may be reading too
            if let Some(values) = &*cache.lock().unwrap() {
                return Ok((**values).clone());
            }
            let values: Vec<i64> = m.materialise()?[0].iter().map(|v| *v as i64).collect();
            *cache.lock().unwrap() = Some(Arc::new(values.clone()));
            Ok(values)
        }),
        settle: Arc::new(move || s.settle()),
        release: Arc::new(move |held| ops.release(held)),
        pin: Arc::new(move || {
            if me() != domain {
                return None;
            }
            kept_id.and_then(kept_of).and_then(|k| pin_kept(&k))
        }),
    }
}

/// A logical prefix of shared integer storage. Views retain their exact gather bound.
pub(crate) fn uint32_view(holder: &Uint32Holder, count: i64) -> Uint32Holder {
    assert!((0..=holder.count).contains(&count));
    let cache: Arc<Mutex<Option<Vec<i64>>>> = Arc::new(Mutex::new(None));
    let (h1, h2, h3) = (holder.clone(), holder.clone(), holder.clone());
    let (resident, input) = match (&holder.resident, &holder.input) {
        (Some(Value::Int_array(old)), Some(input)) => {
            let values = Arc::new(vec![old[0], count]);
            let view = Arc::new(vec![input[0], count as f64]);
            let binding = BINDABLE.with(|b| b.borrow().iter().rev().find(|(p, _, _, _)| p.upgrade().is_some_and(|p| Arc::ptr_eq(&p, input))).map(|(_, token, id, _)| (token.clone(), *id)));
            if let Some((token, id)) = binding {
                BINDABLE.with(|b| b.borrow_mut().push((Arc::downgrade(&view), token, id, count)));
            }
            flow_ir::packed::host::register_integer_input(&values, &view);
            (Some(Value::Int_array(values)), Some(view))
        }
        _ => (None, None),
    };
    Uint32Holder {
        count, resident, input,
        materialise: Arc::new(move || {
            if let Some(values) = &*cache.lock().unwrap() { return Ok(values.clone()); }
            let values = if count == 0 { Vec::new() } else { (h1.materialise)()?[..count as usize].to_vec() };
            *cache.lock().unwrap() = Some(values.clone());
            Ok(values)
        }),
        settle: holder.settle.clone(),
        // ponytail: shared views retire together; reference-count releases if memory pressure warrants it.
        release: Arc::new(move |_| (h2.release)(true)),
        pin: Arc::new(move || (h3.pin)().map(|(b, _, valid, unpin)| (b, count, valid, unpin))),
    }
}

/// Transfers an unsigned output into an exact integer holder, using the same bounded pool and
/// release lifecycle as `keep`. Its numeric input carrier preserves the UInt32 storage type.
pub fn keep_uint32(output: &Output, index: Option<i64>) -> Result<Uint32Holder, Diagnostic> {
    let (planes, kept) = keep_storage(true, index, output)?;
    Ok(integer_holder(output.owner.0.domain, planes, kept))
}

fn adopt_storage(t: &T, uint32: bool, buffer: &Buffer, count: i64, width: i64) -> Result<(gpu::Planes, Rc<Kept>), Diagnostic> {
    let d = &*t.0;
    if d.state.borrow().closed || me() != d.domain {
        return Err(e_gpu("GPU owner is closed or called from another domain."));
    }
    if width < 1 || width > 4 || (uint32 && width != 1) || count < 0 || count > b::buffer_size(buffer) / (4 * width) {
        return Err(e_gpu("Adopted array exceeds its buffer or has an invalid width."));
    }
    if b::release_mapped_view(buffer).is_err() {
        return Err(e_gpu("Adopted array requires mapped storage for worker reads."));
    }
    drain(d, &mut d.state.borrow_mut());
    Ok(hold(t, uint32, buffer, b::buffer_size(buffer), count, width, b::completed_epoch(&d.queue), true))
}

pub fn adopt_uint32(t: &T, buffer: &Buffer, count: i64) -> Result<Uint32Holder, Diagnostic> {
    let (planes, kept) = adopt_storage(t, true, buffer, count, 1)?;
    Ok(integer_holder(t.0.domain, planes, Some(kept)))
}

/// Transfer a caller-owned, completed and status-validated buffer without copying or reading it.
pub fn adopt_float(t: &T, buffer: &Buffer, count: i64, width: i64) -> Result<gpu::Planes, Diagnostic> {
    adopt_storage(t, false, buffer, count, width).map(|(planes, _)| planes)
}

/// A display's borrow of the held buffer a settled output's placeholder (`keep`'s `resident`)
/// stands for, while it is held: the buffer (packed float32, `width` per element, from offset 0),
/// its element count, `valid` and `unpin`. Until `unpin` the pool neither reuses nor reads back the
/// buffer. `None` for an unsettled, freed or unknown output. Creating thread.
pub fn pin(placeholder: &Arc<Vec<f64>>) -> Option<Pin> {
    bindable_live();
    let id = BINDABLE.with(|b| b.borrow().iter().rev().find(|(p, _, _, _)| p.upgrade().is_some_and(|p| Arc::ptr_eq(&p, placeholder))).map(|(_, _, id, _)| *id))?;
    let kept = kept_of(id)?;
    if kept.uint32 || kept.fate.destroyed() || !kept.settled.load(Ordering::SeqCst) {
        None
    } else {
        Some(borrow(&kept))
    }
}

pub fn buffer(output: &Output, index: Option<i64>) -> Option<Buffer> {
    let index = index.unwrap_or(output.index);
    let d = &*output.owner.0;
    let st = d.state.borrow();
    if index < 0 || index as usize >= st.outputs.len() || st.closed || me() != d.domain || output.generation != st.generation {
        None
    } else {
        st.outputs[index as usize].buffer.clone()
    }
}

/// The count record of a fast kernel's dispatch, while it is the runner's current output.
pub fn count_record(output: &Output) -> Option<Buffer> {
    let d = &*output.owner.0;
    let st = d.state.borrow();
    if st.closed || me() != d.domain || output.generation != st.generation || d.kernel.counts.is_none() {
        None
    } else {
        st.counted.buffer.clone()
    }
}

pub fn count(output: &Output) -> i64 {
    output.count
}
pub fn epoch(output: &Output) -> i64 {
    output.epoch
}
pub fn width(output: &Output) -> i64 {
    output.width
}
pub fn gpu_seconds(output: &Output) -> Option<f64> {
    output.gpu_seconds
}

/// The kernel pipeline's most threads per threadgroup: Metal lowers it when the kernel needs more registers.
pub fn max_total_threads(t: &T) -> Result<i64, Diagnostic> {
    pipelines::get(&t.0.pipelines, &t.0.kernel).map(|c| b::max_total_threads(&c.pipeline))
}

pub fn close(t: &T, discard: bool) {
    let d = &*t.0;
    assert!(me() == d.domain, "Run.close: creating domain required");
    if d.state.borrow().closed {
        return;
    }
    // a holder still alive reads its planes now, while its buffer exists. On a shared pool its
    // buffer outlives this runner: once settled it stays unread and the pool's, read only by a reader.
    drain(d, &mut d.state.borrow_mut());
    let kept: Vec<(i64, Weak<Token>)> = d.state.borrow().kept.iter().map(|k| (k.id, k.token.clone())).collect();
    d.state.borrow_mut().kept.clear();
    for (id, weak) in kept.into_iter().rev() {
        let Some(k) = kept_of(id) else { continue };
        if weak.upgrade().is_none() {
            continue;
        }
        let held_entry = d.pool.0.s.borrow().held.contains_key(&id);
        if !held_entry {
            continue;
        }
        if !discard && !d.own_pool && settle(t, k.epoch, &k.settled, &k.fate).is_ok() {
            d.pool.0.s.borrow_mut().orphans.push(weak);
        } else {
            let (buffer, capacity) = {
                let s = d.pool.0.s.borrow();
                let (b_, c, _) = s.held.get(&id).unwrap();
                (b_.clone(), *c)
            };
            if !discard {
                let _ = materialise(&k);
            }
            {
                let _g = k.fate.lock.lock().unwrap();
                k.fate.destroy();
            }
            mem::release(&HELD_OWNER, capacity);
            d.pool.0.s.borrow_mut().held.remove(&id);
            KEPT.with(|m| m.borrow_mut().remove(&id));
            let _ = b::destroy_buffer(&buffer);
        }
    }
    {
        let mut st = d.state.borrow_mut();
        if let Some(buffers) = st.bounds_out.take() {
            for buffer in buffers { let _ = b::destroy_buffer(&buffer); }
        }
    }
    if d.own_pool {
        close_pool(&d.pool);
    }
    let mut st = d.state.borrow_mut();
    let st = &mut *st;
    let tail = [&mut st.table, &mut st.status, &mut st.counted, &mut st.groups];
    for slot in st.inputs.iter_mut().chain(st.outputs.iter_mut()).chain(st.gathers.iter_mut()).chain(tail.into_iter()) {
        if let Some(buffer) = &slot.buffer {
            let _ = b::destroy_buffer(buffer);
        }
        mem::release(&SLOT_OWNER, slot.capacity);
        slot.buffer = None;
        slot.bytes = Vec::new();
        slot.mapped = false;
        slot.capacity = 0;
        slot.uploaded = None;
    }
    st.closed = true;
}

pub mod private {
    use super::*;

    pub fn diagnostic(error: &ogpu_core::error::Error) -> Diagnostic {
        super::diagnostic(error)
    }
    pub fn buffer_creations(t: &T) -> i64 {
        t.0.state.borrow().creations
    }
    pub fn input_uploads(t: &T) -> i64 {
        t.0.state.borrow().input_uploads
    }
    pub fn input_uploaded_bytes(t: &T) -> i64 {
        t.0.state.borrow().input_uploaded_bytes
    }
    pub fn status_reads(t: &T) -> i64 {
        t.0.state.borrow().status_reads
    }
    pub fn check_status(t: &T, epoch: i64) -> Result<(), Diagnostic> {
        super::check_status(t, epoch)
    }
    pub fn readback_bytes(t: &T) -> i64 {
        t.0.state.borrow().readback_bytes + t.0.materialised_bytes.load(Ordering::SeqCst)
    }
    pub fn bounds_readback_bytes(t: &T) -> i64 {
        t.0.state.borrow().bounds_readback_bytes
    }
    /// Kept outputs read so far.
    pub fn materialisations(t: &T) -> i64 {
        t.0.materialised.load(Ordering::SeqCst)
    }
    /// Buffers kept outputs hold, after destroying those of collected holders.
    pub fn held_buffers(t: &T) -> i64 {
        drain(&t.0, &mut t.0.state.borrow_mut());
        t.0.pool.0.s.borrow().held.len() as i64
    }
    pub fn create_owned(pool: Option<&Pool>, device: &Device, queue: &Queue, pipelines: &pipelines::T, kernel: &Kernel) -> T {
        super::create_owned(pool, device, queue, pipelines, kernel)
    }
    pub fn pool_free_floats(pool: &Pool) -> Vec<b::Floats> {
        pool.0.s.borrow().free.iter().rev().map(|(buffer, capacity)| {
            gpu(b::invalidate_mapped(buffer, *capacity)).expect("inspect free GPU buffer");
            view(buffer)
        }).collect()
    }
    pub fn pool_free_buffers(pool: &Pool) -> usize {
        pool.0.s.borrow().free.len()
    }
    pub fn pool_drain(pool: &Pool) {
        drain_pool(pool)
    }
    /// a free buffer of `bytes` put into the pool, as if a holder of that size had come back
    pub fn pool_give(pool: &Pool, device: &Device, bytes: i64) {
        let descriptor = BufferDescriptor { label: Some("test free".to_owned()), size: bytes, usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] };
        let buffer = gpu(b::create_mapped_buffer(device, descriptor)).expect("pool_give");
        mem::charge(&FREE_OWNER, bytes);
        let mut s = pool.0.s.borrow_mut();
        let tick = s.tick;
        s.free_ages.insert(b::buffer_id(&buffer), tick);
        s.free.push((buffer, bytes));
    }
    pub const slot_shrink_after: i64 = super::slot_shrink_after;
    /// Holders freed unread (`release(false)`).
    pub fn pool_freed(pool: &Pool) -> i64 {
        pool.0.shared.freed.load(Ordering::SeqCst)
    }
    /// Buffers its kept outputs hold now, after recycling what dropped or read ones gave back.
    pub fn pool_held(pool: &Pool) -> i64 {
        drain_pool(pool);
        pool.0.s.borrow().held.len() as i64
    }
}

// ---- the resident fold's device stages

fn one_pass(queue: &Queue, f: &dyn Fn(&Commands) -> Result<(), Diagnostic>) -> Result<(), Diagnostic> {
    let commands = gpu(b::begin_commands(queue))?;
    let r = (|| -> Result<b::Receipt, Diagnostic> {
        f(&commands)?;
        gpu(b::commit(&commands))
    })();
    match r {
        Ok(receipt) => gpu(b::complete_through(queue, receipt.epoch)),
        Err(e) => {
            let _ = b::abandon(&commands);
            Err(e)
        }
    }
}

/// Encodes into `encoder` the writing of `0..count-1`, or the constant `value`, as exact uint32 words:
/// a later dispatch of the same serial encoder, or of a later command buffer on its queue, reads them.
pub fn encode_fill(encoder: &ComputeEncoder, pipelines: &pipelines::T, value: Option<i64>, buffer: &Buffer, count: i64) -> Result<(), Diagnostic> {
    if count < 0 || count > b::buffer_size(buffer) / 4 {
        return Err(e_gpu("Integer fill count exceeds its buffer capacity."));
    }
    if count == 0 {
        return Ok(());
    }
    let compiled = pipelines::get(pipelines, kernels::indices())?;
    gpu(b::set_pipeline(encoder, &compiled.pipeline))?;
    gpu(b::set_buffer(encoder, 0, buffer))?;
    let mut word = [0u8; 12];
    pack_uint32(&mut word, 0, count as f64)?;
    pack_uint32(&mut word, 4, if value.is_none() { 0.0 } else { 1.0 })?;
    pack_uint32(&mut word, 8, value.unwrap_or(0) as f64)?;
    gpu(b::set_bytes(encoder, 1, &word))?;
    gpu(b::dispatch_threads(encoder, (count, 1, 1), (256, 1, 1)))
}

/// `encode_fill` in a command buffer of its own, waited for: no size-dependent CPU staging.
pub fn fill_indices(value: Option<i64>, queue: &Queue, pipelines: &pipelines::T, buffer: &Buffer, count: i64) -> Result<(), Diagnostic> {
    if count < 0 || count > b::buffer_size(buffer) / 4 {
        return Err(e_gpu("Integer fill count exceeds its buffer capacity."));
    }
    if count == 0 {
        return Ok(());
    }
    one_pass(queue, &|commands| {
        let encoder = gpu(b::compute_encoder(commands))?;
        let r = encode_fill(&encoder, pipelines, value, buffer, count);
        let _ = b::end_compute(&encoder);
        r
    })
}

pub fn fill_float_indices(queue: &Queue, pipelines: &pipelines::T, buffer: &Buffer, count: i64) -> Result<(), Diagnostic> {
    if count < 0 || count > u32::MAX as i64 || count > b::buffer_size(buffer) / 4 { return Err(e_gpu("Float range exceeds its buffer capacity.")); }
    if count == 0 { return Ok(()); }
    let compiled = pipelines::get(pipelines, kernels::float_indices())?;
    one_pass(queue, &|commands| {
        let encoder = gpu(b::compute_encoder(commands))?;
        let result = (|| {
            gpu(b::set_pipeline(&encoder, &compiled.pipeline))?;
            gpu(b::set_buffer(&encoder, 0, buffer))?;
            gpu(b::set_bytes(&encoder, 1, &(count as u32).to_le_bytes()))?;
            gpu(b::dispatch_threads(&encoder, (count, 1, 1), (256, 1, 1)))
        })();
        result.and(gpu(b::end_compute(&encoder)))
    })
}

/// Encode a repeated scalar/vector row without constructing a dense CPU array.
pub fn encode_float_fill(encoder: &ComputeEncoder, pipelines: &pipelines::T, buffer: &Buffer, count: i64, row: &[f64]) -> Result<(), Diagnostic> {
    if !(1..=4).contains(&row.len()) || count < 0 || count > b::buffer_size(buffer) / (row.len() as i64 * 4)
        || row.iter().any(|x| !(*x as f32).is_finite()) || count * row.len() as i64 > u32::MAX as i64
    { return Err(e_gpu("Float fill exceeds buffer capacity or float32 range.")); }
    if count == 0 { return Ok(()); }
    let compiled = pipelines::get(pipelines, kernels::fill_vectors())?;
    let mut bound = [0u8; 24];
    put_i32(&mut bound, 0, (count * row.len() as i64) as i32);
    put_i32(&mut bound, 4, row.len() as i32);
    for (i, x) in row.iter().enumerate() { put_i32(&mut bound, 8 + i * 4, (*x as f32).to_bits() as i32); }
    gpu(b::set_pipeline(encoder, &compiled.pipeline))?;
    gpu(b::set_buffer(encoder, 0, buffer))?;
    gpu(b::set_bytes(encoder, 1, &bound))?;
    gpu(b::dispatch_threads(encoder, (count * row.len() as i64, 1, 1), (256, 1, 1)))
}

pub fn compact_blocks(count: i64) -> i64 {
    8 * 1.max((count + 1023) / 1024)
}

/// Coordinate planes into a packed float3 buffer. The caller validated and settled the planes.
pub fn pack_planes(queue: &Queue, pipelines: &pipelines::T, planes: [&Buffer; 3], output: &Buffer, count: i64) -> Result<(), Diagnostic> {
    if count < 0 || planes.iter().any(|p| count > b::buffer_size(p) / 4) || count > b::buffer_size(output) / 12 {
        return Err(e_gpu("Coordinate plane count exceeds buffer capacity."));
    }
    if count == 0 { return Ok(()); }
    let compiled = pipelines::get(pipelines, kernels::planes())?;
    one_pass(queue, &|commands| {
        let encoder = gpu(b::compute_encoder(commands))?;
        let result = (|| {
            gpu(b::set_pipeline(&encoder, &compiled.pipeline))?;
            for (i, buffer) in planes.into_iter().chain([output]).enumerate() {
                gpu(b::set_buffer(&encoder, i as i64, buffer))?;
            }
            gpu(b::set_bytes(&encoder, 4, &(count as u32).to_le_bytes()))?;
            gpu(b::dispatch_threads(&encoder, (count, 1, 1), (256, 1, 1)))
        })();
        let ended = gpu(b::end_compute(&encoder));
        result.and(ended)
    })
}

/// Device outputs of the point incidence CSR, plus fan data in the CSR's slot order.
pub struct Incidence<'a> {
    pub point_offsets: &'a Buffer,
    pub point_vertices: &'a Buffer,
    pub primitive_of_vertex: &'a Buffer,
    pub first: &'a Buffer,
    pub corners: &'a Buffer,
}

/// Build exact, deterministically ordered point incidence and polygon fans from validated topology.
/// `scratch` needs `vertices.1` words; `blocks` needs `compact_blocks(vertices.1) + 4` bytes.
/// Every size-dependent operation stays on the device; this call performs no readbacks or uploads.
pub fn point_incidence(
    queue: &Queue, pipelines: &pipelines::T, point_count: i64,
    vertices: (&Buffer, i64), offsets: (&Buffer, i64), kinds: (&Buffer, i64),
    out: &Incidence<'_>, scratch: &Buffer, blocks: &Buffer,
) -> Result<(), Diagnostic> {
    let nv = vertices.1;
    let np = kinds.1;
    if !(0..=1 << 30).contains(&point_count) || !(0..=1 << 30).contains(&nv)
        || np < 0 || offsets.1 != np + 1 || (nv > 0 && (point_count == 0 || np == 0))
        || b::buffer_size(blocks) < compact_blocks(nv) + 4
    {
        return Err(e_gpu("Point incidence requires validated, bounded topology."));
    }
    for (buffer, words) in [vertices, offsets, kinds, (out.point_offsets, point_count + 1),
        (out.point_vertices, nv), (out.primitive_of_vertex, nv), (out.first, nv), (out.corners, nv), (scratch, nv)]
    {
        if words < 0 || words > b::buffer_size(buffer) / 4 {
            return Err(e_gpu("Point incidence count exceeds buffer capacity."));
        }
    }
    let partition = pipelines::get(pipelines, kernels::incidence_partition())?;
    let tables = pipelines::get(pipelines, kernels::incidence_tables())?;
    // At least one bit also writes implicit iota for a topology containing only point zero.
    let bits = (64 - (point_count - 1).max(0).leading_zeros()).max(1);
    let groups = (nv + 1023) / 1024;
    one_pass(queue, &|commands| {
        let encoder = gpu(b::compute_encoder(commands))?;
        let result = (|| {
            if nv > 0 {
                gpu(b::set_pipeline(&encoder, &partition.pipeline))?;
                gpu(b::set_buffer(&encoder, 0, vertices.0))?;
                gpu(b::set_buffer(&encoder, 3, blocks))?;
                for bit in 0..bits {
                    let output = if (bits - bit) % 2 == 1 { out.point_vertices } else { scratch };
                    let input = if (bits - bit) % 2 == 1 { scratch } else { out.point_vertices };
                    gpu(b::set_buffer(&encoder, 1, input))?;
                    gpu(b::set_buffer(&encoder, 2, output))?;
                    for pass in 0..3 {
                        let mut bound = [0u8; 12];
                        for (i, n) in [nv, bit as i64, pass].into_iter().enumerate() { put_i32(&mut bound, i * 4, n as i32); }
                        gpu(b::set_bytes(&encoder, 4, &bound))?;
                        gpu(b::dispatch_threads(&encoder, (1024 * if pass == 1 { 1 } else { groups }, 1, 1), (1024, 1, 1)))?;
                    }
                }
            }
            gpu(b::set_pipeline(&encoder, &tables.pipeline))?;
            for (i, buffer) in [vertices.0, offsets.0, kinds.0, out.point_vertices, out.point_offsets,
                out.primitive_of_vertex, out.first, out.corners].into_iter().enumerate()
            { gpu(b::set_buffer(&encoder, i as i64, buffer))?; }
            let mut bound = [0u8; 12];
            for (i, n) in [point_count, nv, np].into_iter().enumerate() { put_i32(&mut bound, i * 4, n as i32); }
            gpu(b::set_bytes(&encoder, 8, &bound))?;
            gpu(b::dispatch_threads(&encoder, (nv.max(point_count + 1), 1, 1), (256, 1, 1)))
        })();
        let ended = gpu(b::end_compute(&encoder));
        result.and(ended)
    })
}

// ponytail: at most 64 groups (fills an 8-core M1); raise it for a wider device.
pub const TOPOLOGY_STATUS_BYTES: i64 = 64 * 36;

/// One group per 65,536 rows (a small mesh reads back one record), at most 64.
fn topology_groups(n: i64) -> i64 {
    ((n + 65535) / 65536).clamp(1, TOPOLOGY_STATUS_BYTES / 36)
}

/// Groups of 256 across the grid check exact uint32 topology, reading back one 36-byte status record
/// a group (`topology_groups`: one up to 65,536 rows). Counts are actual element counts within
/// buffer capacity; status needs 36 bytes a group, `TOPOLOGY_STATUS_BYTES` at most. No input
/// ownership transfers. Polygon sizes above `max_polygon_size` refuse. `on_read` records successful
/// status bytes even when validation rejects the mesh.
pub fn validate_topology(
    on_read: &dyn Fn(i64),
    queue: &Queue,
    pipelines: &pipelines::T,
    vertices: (&Buffer, i64),
    offsets: (&Buffer, i64),
    kinds: (&Buffer, i64),
    status: &Buffer,
    point_count: i64,
    max_polygon_size: i64,
) -> Result<(), Diagnostic> {
    let groups = topology_groups(vertices.1.max(offsets.1).max(kinds.1));
    for (buffer, count) in [vertices, offsets, kinds, (status, 9 * groups)] {
        if count < 0 || count > b::buffer_size(buffer) / 4 {
            return Err(value::fail("E_TOPOLOGY", "Topology: array count exceeds buffer capacity"));
        }
    }
    let mut bound = [0u8; 24];
    for (i, n) in [point_count, vertices.1, offsets.1, kinds.1, max_polygon_size, groups * 256].iter().enumerate() {
        pack_uint32(&mut bound, i * 4, *n as f64)?;
    }
    let compiled = pipelines::get(pipelines, kernels::topology())?;
    one_pass(queue, &|commands| {
        let encoder = gpu(b::compute_encoder(commands))?;
        let r = (|| -> Result<(), Diagnostic> {
            gpu(b::set_pipeline(&encoder, &compiled.pipeline))?;
            for (index, buffer) in [vertices.0, offsets.0, kinds.0, status].iter().enumerate() {
                gpu(b::set_buffer(&encoder, index as i64, buffer))?;
            }
            gpu(b::set_bytes(&encoder, 4, &bound))?;
            gpu(b::dispatch_threads(&encoder, (groups * 256, 1, 1), (256, 1, 1)))
        })();
        let _ = b::end_compute(&encoder);
        r
    })?;
    let bytes = gpu(b::read_buffer(status, 0, groups * 36))?;
    on_read(groups * 36);
    let failed = |i: usize| (0..groups as usize).any(|g| get_i32(&bytes, 4 * (g * 9 + i)) != 0);
    let table: [(usize, &str, &str); 9] = [
        (6, "E_TOPOLOGY_KIND", "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline)."),
        (0, "E_TOPOLOGY", "Topology: primitive offsets must contain the initial zero"),
        (1, "E_TOPOLOGY", "Topology: first primitive offset must be zero"),
        (2, "E_TOPOLOGY", "Topology: final primitive offset must equal vertex count"),
        (3, "E_TOPOLOGY", "Topology: primitive kind count must match primitive count"),
        (4, "E_TOPOLOGY", "Topology: vertex references an invalid point"),
        (5, "E_TOPOLOGY", "Topology: primitive offsets are not ordered"),
        (7, "E_TOPOLOGY", "Topology: primitive has too few vertices for its kind"),
        (8, "E_TOPOLOGY", "Topology: polygon exceeds the supported vertex count"),
    ];
    match table.iter().find(|(bit, _, _)| failed(*bit)) {
        None => Ok(()),
        Some((_, code, message)) => Err(value::fail(code, message)),
    }
}

/// Generate UInt32 triangle indices for resident polygons with three/four corners, using the
/// host's first-ear rule and positions. Returns the settled index count. Numerically
/// uncertain/degenerate quads and other primitives refuse with `E_GPU_DISPLAY_TRIANGLES`, so
/// callers can take the host path. `blocks` needs one word per 256 primitives; `status` one word.
pub fn triangle_indices(
    on_read: &dyn Fn(i64),
    queue: &Queue,
    pipelines: &pipelines::T,
    positions: (&Buffer, i64),
    vertices: (&Buffer, i64),
    offsets: (&Buffer, i64),
    kinds: (&Buffer, i64),
    indices: &Buffer,
    blocks: &Buffer,
    status: &Buffer,
) -> Result<i64, Diagnostic> {
    let refuse = |m: &str| Err(value::fail("E_GPU_DISPLAY_TRIANGLES", m));
    let (points, nv, no, np) = (positions.1, vertices.1, offsets.1, kinds.1);
    if points <= 0 || points > 1 << 30 || np <= 0 || np > 1 << 28 || no != np + 1 || nv < 3 * np || nv > 4 * np || nv > MAX_STRING / 12 {
        return refuse("Resident display requires nonempty triangle/quad topology.");
    }
    let count = 3 * (nv - 2 * np);
    let groups = (np + 255) / 256;
    for (buffer, words) in [(positions.0, points * 3), (vertices.0, nv), (offsets.0, no), (kinds.0, np), (indices, count), (blocks, groups), (status, 1)] {
        if words < 0 || words > b::buffer_size(buffer) / 4 {
            return refuse("Resident display stream count exceeds buffer capacity.");
        }
    }
    let compiled = pipelines::get(pipelines, kernels::triangles())?;
    let mut bound = [0u8; 16];
    for (i, n) in [points, nv, np].iter().enumerate() {
        put_i32(&mut bound, i * 4, *n as i32);
    }
    let commands = gpu(b::begin_commands(queue))?;
    let r = (|| -> Result<b::Receipt, Diagnostic> {
        for pass in 0..2 {
            put_i32(&mut bound, 12, pass);
            let encoder = gpu(b::compute_encoder(&commands))?;
            let r = (|| -> Result<(), Diagnostic> {
                gpu(b::set_pipeline(&encoder, &compiled.pipeline))?;
                for (index, buffer) in [positions.0, vertices.0, offsets.0, kinds.0, indices, blocks, status].iter().enumerate() {
                    gpu(b::set_buffer(&encoder, index as i64, buffer))?;
                }
                gpu(b::set_bytes(&encoder, 7, &bound))?;
                gpu(b::dispatch_threads(&encoder, (if pass == 0 { groups * 256 } else { 256 }, 1, 1), (256, 1, 1)))
            })();
            let _ = b::end_compute(&encoder);
            r?;
        }
        gpu(b::commit(&commands))
    })();
    match r {
        Ok(receipt) => gpu(b::complete_through(queue, receipt.epoch))?,
        Err(e) => {
            let _ = b::abandon(&commands);
            return Err(e);
        }
    }
    let result = gpu(b::read_buffer(status, 0, 4))?;
    on_read(4);
    if get_i32(&result, 0) != 0 {
        return refuse("Resident display polygon is unsupported, degenerate or numerically ambiguous.");
    }
    Ok(count)
}

/// Encodes into `commands` the indices of `mask`'s first elements that are nonzero, as floats into
/// `kept`, and their count into the cell `total`. The mask's count is `count`, or at most the uint
/// in `count_in`; `blocks` is scratch of `compact_blocks count` bytes. With `count_in` the count and
/// scatter passes take their group count from it on the device, through `groups` (12 bytes).
/// Nothing is committed or read. `uint32` instead reads a uint32 mask and writes exact uint32 indices.
pub fn compact(
    uint32: bool,
    encoder: Option<&ComputeEncoder>,
    commands: &Commands,
    pipelines: &pipelines::T,
    mask: &Buffer,
    count: i64,
    count_in: Option<&Buffer>,
    kept: &Buffer,
    total: &Buffer,
    blocks: &Buffer,
    groups: &Buffer,
) -> Result<(), Diagnostic> {
    let compiled = pipelines::get(pipelines, if uint32 { kernels::compaction_uint32() } else { kernels::compaction() })?;
    let encoder_ = match encoder {
        Some(e) => e.clone(),
        None => gpu(b::compute_encoder(commands))?,
    };
    let r = (|| -> Result<(), Diagnostic> {
        if let Some(cell) = count_in {
            dispatch_arguments(&encoder_, pipelines, cell, 1, 1, count, 1024, groups)?;
        }
        gpu(b::set_pipeline(&encoder_, &compiled.pipeline))?;
        gpu(b::set_buffer(&encoder_, 0, mask))?;
        gpu(b::set_buffer(&encoder_, 1, kept))?;
        gpu(b::set_buffer(&encoder_, 2, total))?;
        gpu(b::set_buffer(&encoder_, 4, count_in.unwrap_or(total)))?;
        gpu(b::set_buffer(&encoder_, 5, blocks))?;
        for pass in [0, 1, 2] {
            let mut bound = [0u8; 12];
            put_i32(&mut bound, 0, count as i32);
            put_i32(&mut bound, 4, if count_in.is_none() { 0 } else { 1 });
            put_i32(&mut bound, 8, pass);
            gpu(b::set_bytes(&encoder_, 3, &bound))?;
            if pass != 1 && count_in.is_some() {
                gpu(b::dispatch_threadgroups_indirect(&encoder_, groups, 0, (1024, 1, 1)))?;
            } else {
                let g = if pass == 1 { 1 } else { compact_blocks(count) / 8 };
                gpu(b::dispatch_threads(&encoder_, (1024 * g, 1, 1), (1024, 1, 1)))?;
            }
        }
        Ok(())
    })();
    if encoder.is_none() {
        let _ = b::end_compute(&encoder_);
    }
    r
}

/// A resident `array/concat`'s part: `words` raw words of `src` from word `from` into `dst` from
/// word `into`, encoded on `encoder` (queue order makes it see what was encoded before it).
pub fn copy(encoder: &ComputeEncoder, pipelines: &pipelines::T, src: &Buffer, from: i64, dst: &Buffer, into: i64, words: i64) -> Result<(), Diagnostic> {
    if words > 0 {
        let compiled = pipelines::get(pipelines, kernels::copy())?;
        let mut bound = [0u8; 12];
        put_i32(&mut bound, 0, words as i32);
        put_i32(&mut bound, 4, from as i32);
        put_i32(&mut bound, 8, into as i32);
        gpu(b::set_pipeline(encoder, &compiled.pipeline))?;
        gpu(b::set_buffer(encoder, 0, src))?;
        gpu(b::set_buffer(encoder, 1, dst))?;
        gpu(b::set_bytes(encoder, 2, &bound))?;
        gpu(b::dispatch_threads(encoder, (words, 1, 1), (256, 1, 1)))?;
    }
    Ok(())
}

/// a count cell, once the queue has completed `epoch`
pub fn read_count(queue: &Queue, buffer: &Buffer, epoch: i64) -> Result<i64, Diagnostic> {
    if epoch > b::completed_epoch(queue) {
        gpu(b::complete_through(queue, epoch))?;
    }
    Ok(get_i32(&gpu(b::read_buffer(buffer, 0, 4))?, 0) as u32 as i64)
}

/// read `length` floats of a device buffer the queue has finished writing
pub fn read_floats(queue: &Queue, buffer: &Buffer, epoch: i64, length: i64) -> Result<Vec<f64>, Diagnostic> {
    if epoch > b::completed_epoch(queue) {
        gpu(b::complete_through(queue, epoch))?;
    }
    if length == 0 {
        return Ok(vec![]);
    }
    Ok(unpack_array(&gpu(b::read_buffer(buffer, 0, length * 4))?, length as usize).0)
}

// ---- the device scene of scene/trace

#[derive(Clone, Debug)]
pub struct Instance {
    pub first: i64,
    pub triangles: i64,
    /// row-major 3x4 object-to-world
    pub transform: Vec<f64>,
    pub transform_end: Option<Vec<f64>>,
}

/// A scene for [`build_scene`]: packed float3 triangle corners (nine float32 per triangle, in
/// primitive order; instanced, every prototype's), the same corners at the shutter's end (vertex
/// motion; flat scenes only), the instances (empty: a flat scene), float4 (centre, radius)
/// spheres, and strand segments, eight float32 each (end a, radius, end b, radius; world space).
#[derive(Clone, Debug)]
pub struct Description {
    pub triangles: Vec<u8>,
    pub triangles_end: Option<Vec<u8>>,
    pub instances: Vec<Instance>,
    pub sphere_data: Vec<u8>,
    pub strand_data: Vec<u8>,
}

pub struct Built {
    pub scene: Scene,
    pub extras: Extras,
    pub instanced: bool,
    pub motion: bool,
    pub curves: Option<flow_shader::emit::Curves>,
    pub release: Box<dyn Fn()>,
}

fn floats_of(bytes: &[u8]) -> Vec<f64> {
    (0..bytes.len() / 4).map(|k| f32::from_le_bytes(bytes[4 * k..4 * k + 4].try_into().unwrap()) as f64).collect()
}

fn bytes_of(floats: &[f64]) -> Vec<u8> {
    floats.iter().flat_map(|v| (*v as f32).to_le_bytes()).collect()
}

/// Builds and waits for the scene's structures. Motion builds motion structures (keyframes at
/// times 0 and 1, clamped) when `caps` (default: the device's) has `ray_tracing_motion`; otherwise
/// the scene is built at the shutter midpoint and `motion` is false. `release` destroys every
/// structure and buffer.
pub fn build_scene(caps: Option<&ogpu_core::caps::Caps>, gpu_: &renderer::renderer::Gpu, d: &Description) -> Result<Built, Diagnostic> {
    use ogpu_core::backend::{AccelDescriptor, Border, Geometry, InstanceKind, InstanceRecord, Keyframe, Motion, MotionInstance};
    let device = renderer::renderer::gpu_device(gpu_);
    let queue = renderer::renderer::gpu_queue(gpu_);
    let caps = caps.copied().unwrap_or_else(|| b::capabilities(&device));
    let moving = d.triangles_end.is_some() || d.instances.iter().any(|i| i.transform_end.is_some());
    let motion = moving && caps.ray_tracing_motion;
    let ending = |i: &Instance| i.transform_end.clone().unwrap_or_else(|| i.transform.clone());
    // without motion support: the shutter midpoint, as rays_pathtracer's Mesh_data.shutter_midpoint
    let (start, triangles): (Box<dyn Fn(&Instance) -> Vec<f64>>, Vec<u8>) = if moving && !motion {
        (
            Box::new(move |i: &Instance| flow_check::ray_host::lerp(&i.transform, &i.transform_end.clone().unwrap_or_else(|| i.transform.clone()), 0.5).expect("lerp")),
            match &d.triangles_end {
                None => d.triangles.clone(),
                Some(e) => bytes_of(&flow_check::ray_host::lerp(&floats_of(&d.triangles), &floats_of(e), 0.5).expect("lerp")),
            },
        )
    } else {
        (Box::new(|i: &Instance| i.transform.clone()), d.triangles.clone())
    };
    let buffers: Rc<RefCell<Vec<Buffer>>> = Rc::new(RefCell::new(vec![]));
    let structures: Rc<RefCell<Vec<Accel>>> = Rc::new(RefCell::new(vec![]));
    let release: Box<dyn Fn()> = {
        let (s, bu) = (structures.clone(), buffers.clone());
        Box::new(move || {
            for a in s.borrow().iter() {
                let _ = b::destroy_accel(a);
            }
            for x in bu.borrow().iter() {
                let _ = b::destroy_buffer(x);
            }
        })
    };
    let buffer = |memory: Memory, size: i64| -> Result<Buffer, Diagnostic> {
        let x = gpu(b::create_buffer(&device, BufferDescriptor { label: Some("Flow scene".to_owned()), size: size.max(16), usage: vec![BufferUsage::Storage] }, Some(memory)))?;
        buffers.borrow_mut().push(x.clone());
        Ok(x)
    };
    let upload = |bytes: &[u8]| -> Result<Buffer, Diagnostic> {
        let x = buffer(Memory::Shared, bytes.len() as i64)?;
        if !bytes.is_empty() {
            gpu(b::write_buffer(&x, 0, bytes))?;
        }
        Ok(x)
    };
    let accel = |descriptor: AccelDescriptor| -> Result<Accel, Diagnostic> {
        let a = gpu(b::create_accel(&device, descriptor, false))?;
        structures.borrow_mut().push(a.clone());
        let scratch = buffer(Memory::DeviceLocal, b::accel_sizes(&a).build_scratch_size)?;
        let commands = gpu(b::begin_commands(&queue))?;
        let encoder = gpu(b::accel_encoder(&commands))?;
        gpu(b::build_accel(&encoder, &a, &scratch, None))?;
        gpu(b::end_accel(&encoder))?;
        let receipt = gpu(b::commit(&commands))?;
        gpu(b::complete_through(&queue, receipt.epoch))?;
        Ok(a)
    };
    let shutter = Motion { keyframes: 2, start_time: 0.0, end_time: 1.0, start_border: Border::Clamp, end_border: Border::Clamp };
    let result = (|| -> Result<Built, Diagnostic> {
        if !d.instances.is_empty() && d.triangles_end.is_some() {
            return Err(e_gpu("scene/trace: vertex motion is a flat scene's; instances move by their transforms."));
        }
        let vertices = upload(&triangles)?;
        let spheres = upload(&d.sphere_data)?;
        let sphere_count = d.sphere_data.len() as i64 / 16;
        let triangles_at = |first: i64, count: i64| Geometry::Triangles { vertices: vertices.clone(), offset: 36 * first, length: 36 * count, vertex_stride: 12, vertex_count: 3 * count };
        let mut built = if d.instances.is_empty() {
            let count = triangles.len() as i64 / 36;
            match &d.triangles_end {
                Some(e) if motion => {
                    let vertices_end = upload(e)?;
                    let a = accel(AccelDescriptor::MotionBlas {
                        geometries: vec![Geometry::MotionTriangles {
                            keyframes: vec![Keyframe { buffer: vertices.clone(), offset: 0 }, Keyframe { buffer: vertices_end.clone(), offset: 0 }],
                            vertex_stride: 12,
                            vertex_count: 3 * count,
                        }],
                        motion: shutter,
                    })?;
                    Built {
                        scene: Scene { accel: a, vertices: vertices.clone(), spheres, sphere_count, instances: None },
                        extras: Extras { vertices_end: Some(vertices_end), ..Extras::default() },
                        instanced: false,
                        motion: true,
                        curves: None,
                        release,
                    }
                }
                _ => Built {
                    scene: Scene { accel: accel(AccelDescriptor::Blas { geometries: vec![triangles_at(0, count)] })?, vertices: vertices.clone(), spheres, sphere_count, instances: None },
                    extras: Extras::default(),
                    instanced: false,
                    motion: false,
                    curves: None,
                    release,
                },
            }
        } else {
            let mut prototypes: Vec<(i64, i64)> = d.instances.iter().map(|i| (i.first, i.triangles)).collect();
            prototypes.sort();
            prototypes.dedup();
            let mut blas = Vec::new();
            for (first, count) in &prototypes {
                blas.push(accel(AccelDescriptor::Blas { geometries: vec![triangles_at(*first, *count)] })?);
            }
            let prototype_accel = if motion && pipelines::is_vulkan() {
                let identity = vec![1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0];
                let instances: Vec<InstanceRecord> = (0..prototypes.len() as i64)
                    .map(|structure_index| InstanceRecord { instance: b::Instance { transform: identity.clone(), mask: 0xFFFF_FFFF, structure_index }, user_id: structure_index })
                    .collect();
                let instance_buffer = upload(&gpu(b::pack_instance_records(&device, &instances))?)?;
                Some(accel(AccelDescriptor::TlasOf { instances: instance_buffer, offset: 0, instance_count: instances.len() as i64, kind: InstanceKind::UserIdInstances, structures: blas.clone(), motion_transforms: None })?)
            } else {
                None
            };
            let records: Vec<InstanceRecord> = d
                .instances
                .iter()
                .enumerate()
                .map(|(k, i)| {
                    let structure_index = prototypes.iter().position(|p| *p == (i.first, i.triangles)).unwrap() as i64;
                    InstanceRecord { instance: b::Instance { transform: start(i), mask: 0xFFFF_FFFF, structure_index }, user_id: k as i64 }
                })
                .collect();
            let count = d.instances.len() as i64;
            let (records_bytes, kind, motion_transforms) = if motion {
                let motion_instances: Vec<MotionInstance> = records
                    .iter()
                    .enumerate()
                    .map(|(k, record)| MotionInstance { record: record.clone(), transforms_start: 2 * k as i64, transforms_count: 2, start_time: 0.0, end_time: 1.0, start_border: Border::Clamp, end_border: Border::Clamp })
                    .collect();
                let transforms: Vec<Vec<f64>> = d.instances.iter().flat_map(|i| [i.transform.clone(), ending(i)]).collect();
                (
                    gpu(b::pack_motion_instances(&device, &motion_instances))?,
                    InstanceKind::MotionInstances,
                    Some((upload(&gpu(b::pack_transforms(&transforms))?)?, 0, 2 * count)),
                )
            } else {
                (gpu(b::pack_instance_records(&device, &records))?, InstanceKind::UserIdInstances, None)
            };
            let a = accel(AccelDescriptor::TlasOf { instances: upload(&records_bytes)?, offset: 0, instance_count: count, kind, structures: blas, motion_transforms })?;
            // per instance: its first triangle, then the world-to-object 3x4 (static) or both
            // object-to-world keyframes (motion), padded to 16 or 32 floats
            let table: Vec<f64> = d
                .instances
                .iter()
                .flat_map(|i| {
                    let mut row = vec![i.first as f64];
                    if motion {
                        row.extend(&i.transform);
                        row.extend(ending(i));
                        row.extend([0.0; 7]);
                    } else {
                        row.extend(flow_check::ray_host::world_to_object(&start(i)));
                        row.extend([0.0, 0.0, 0.0]);
                    }
                    row
                })
                .collect();
            Built {
                scene: Scene { accel: a, vertices: vertices.clone(), spheres, sphere_count, instances: Some(upload(&bytes_of(&table))?) },
                extras: Extras { prototypes: prototype_accel, ..Extras::default() },
                instanced: true,
                motion,
                curves: None,
                release,
            }
        };
        // Native curves use their curve structure; other devices use per-segment swept AABBs and
        // resolve capsule candidates against the same strand records.
        let segments = d.strand_data.len() as i64 / 32;
        if segments == 0 {
            return Ok(built);
        }
        let strands = upload(&d.strand_data)?;
        let native = caps.ray_tracing_curves;
        let curves = if native {
            let mut indices = vec![0u8; 4 * segments as usize];
            for k in 0..segments as usize {
                indices[4 * k..4 * k + 4].copy_from_slice(&((2 * k) as u32).to_le_bytes());
            }
            let indices = upload(&indices)?;
            accel(AccelDescriptor::Blas {
                geometries: vec![Geometry::Curves {
                    control_points: vec![Keyframe { buffer: strands.clone(), offset: 0 }],
                    control_stride: 16,
                    control_point_count: 2 * segments,
                    radii: vec![Keyframe { buffer: strands.clone(), offset: 12 }],
                    radius_stride: 16,
                    indices,
                    index_offset: 0,
                    segment_count: segments,
                    control_points_per_segment: 2,
                    curve_type: b::CurveType::RoundCurve,
                    basis: b::CurveBasis::LinearBasis,
                    caps: b::CurveCaps::SphereCaps,
                }],
            })?
        } else {
            let mut boxes = vec![0u8; segments as usize * 24];
            let value = |at: usize| f32::from_le_bytes(d.strand_data[at..at + 4].try_into().unwrap()) as f64;
            for segment in 0..segments as usize {
                let record = segment * 32;
                let radius = value(record + 12).abs().max(value(record + 28).abs());
                for axis in 0..3 {
                    let a = value(record + axis * 4);
                    let bb = value(record + 16 + axis * 4);
                    let low = a.min(bb) - radius;
                    let high = a.max(bb) + radius;
                    let epsilon = 1e-5 * 1.0f64.max(low.abs().max(high.abs()));
                    boxes[segment * 24 + axis * 4..segment * 24 + axis * 4 + 4].copy_from_slice(&((low - epsilon) as f32).to_le_bytes());
                    boxes[segment * 24 + 12 + axis * 4..segment * 24 + 12 + axis * 4 + 4].copy_from_slice(&((high + epsilon) as f32).to_le_bytes());
                }
            }
            let boxes = upload(&boxes)?;
            accel(AccelDescriptor::Blas {
                geometries: vec![Geometry::BoundingBoxes { boxes: vec![Keyframe { buffer: boxes, offset: 0 }], stride: 24, count: segments, options: b::DEFAULT_GEOMETRY_OPTIONS }],
            })?
        };
        built.extras.strands = Some(strands);
        built.extras.strand_count = segments;
        built.extras.curves = Some(curves);
        built.curves = Some(if native { flow_shader::emit::Curves::Native } else { flow_shader::emit::Curves::Loop });
        Ok(built)
    })();
    match result {
        Ok(built) => Ok(built),
        Err(e) => {
            // the `release` moved into `built` is gone on error: rebuild it from the shared lists
            for a in structures.borrow().iter() {
                let _ = b::destroy_accel(a);
            }
            for x in buffers.borrow().iter() {
                let _ = b::destroy_buffer(x);
            }
            Err(e)
        }
    }
}

/// Idle bytes kept for reuse (see [`free_budget`]).
pub fn free_budget_bytes() -> i64 {
    free_budget
}

/// The same runner (physical equality in OCaml).
pub fn same(a: &T, b: &T) -> bool {
    Rc::ptr_eq(&a.0, &b.0)
}

/// Drops this thread's registries of kept outputs (their buffers go with them). Call at the end of
/// a thread that used the runners, before its thread-locals are torn down.
pub fn clear_thread_state() {
    BINDABLE.with(|b| b.borrow_mut().clear());
    let kept: Vec<Rc<Kept>> = KEPT.with(|k| k.borrow_mut().drain().map(|(_, v)| v).collect());
    drop(kept);
    // reads nobody will answer fail rather than sleep forever
    let mine: Vec<Arc<Request>> = {
        let mut all = REQUESTS.lock().unwrap();
        let (mine, rest): (Vec<_>, Vec<_>) = all.drain(..).partition(|r| r.owner == me());
        *all = rest;
        mine
    };
    for request in mine {
        answer(&request, Err("Run.materialise: a device output was read after its owner closed".to_owned()));
    }
}

#[cfg(test)]
mod size_class_tests {
    #[test]
    fn classes_waste_at_most_an_eighth() {
        use super::size_class;
        assert_eq!(size_class(10), 256);
        assert_eq!(size_class(1 << 20), 1 << 20);
        assert_eq!(size_class(1 << 26), 1 << 26);
        assert_eq!(size_class((1 << 26) + 1), (1 << 26) + (1 << 23));
        for bytes in [(1 << 20) + 1, 3_000_000, 68_985_984, 134_217_729] {
            let c = size_class(bytes);
            assert!(c >= bytes && c - bytes <= bytes / 8, "{bytes} -> {c}");
        }
    }
}
