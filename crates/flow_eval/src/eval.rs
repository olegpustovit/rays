#![forbid(unsafe_code)]

use flow_check::{op, workspace::{self as W, Graph, Node as TermNode, Path, Term, Workspace}};
use flow_check::term_arena::{TermArena, TermId, PatternId, PatternNode, Binding, RangeId, TermBuilder, Draft, Literal};
use flow_syntax::{context::Context, ty, value};
pub use flow_syntax::ty::{Ty, FnSignature};
pub use flow_syntax::value::{FnId, ResidualId, Value};
use input::Frame;
use rays_diagnostic::{Diagnostic, Severity};
use std::{cell::{Cell, RefCell}, collections::{BTreeMap, BTreeSet, HashMap, HashSet}, sync::{Arc, Condvar, LazyLock, Mutex, RwLock, Weak, atomic::{AtomicI64, AtomicU64, Ordering}}};

pub use flow_syntax::value::{FnId as FunctionId, ResidualId as LiveId};
pub type Live = Frame;
pub type ValuePayload = Value;
pub type InlineParameterCanonicalizer = Arc<dyn Fn(&[(String, Value)]) -> Result<Option<Vec<(String, Value)>>, Diagnostic> + Send + Sync>;
#[derive(Clone)]
pub struct InlineParameterPolicy {
    pub definition: W::NodeDefinition,
    pub key: String,
    pub canonicalize: InlineParameterCanonicalizer,
}
pub type InlineParametersHook = Arc<dyn Fn(&Workspace, &W::NodeDefinition) -> InlineParameterPolicy + Send + Sync>;
static INLINE_PARAMETERS_HOOK: LazyLock<RwLock<InlineParametersHook>> = LazyLock::new(|| RwLock::new(Arc::new(|workspace, definition| InlineParameterPolicy {
    definition: definition.clone(),
    key: authored_key(workspace),
    canonicalize: Arc::new(|arguments| Ok(Some(arguments.to_vec()))),
})));
pub fn set_inline_parameters(hook: InlineParametersHook) { *INLINE_PARAMETERS_HOOK.write().unwrap() = hook; }
pub fn inline_parameters() -> InlineParametersHook { INLINE_PARAMETERS_HOOK.read().unwrap().clone() }
pub type StaticMapFinish = Arc<dyn Fn(usize, usize, bool) + Send + Sync>;
pub type StaticMapRequest = Arc<dyn Fn(&Path, &Term, usize) -> StaticMapFinish + Send + Sync>;
#[derive(Clone, Default)]
pub struct ResidualHandlers {
    pub observe: Option<Arc<dyn Fn(ResidualId) + Send + Sync>>,
    pub kernel: Option<Arc<dyn Fn(ResidualId) -> Option<Value> + Send + Sync>>,
    pub bulk: Option<Arc<dyn Fn(ResidualId) -> Option<Value> + Send + Sync>>,
    pub defer: Option<Arc<dyn Fn(ResidualId) -> bool + Send + Sync>>,
    pub bulk_approx: Option<Arc<dyn Fn(ResidualId) -> Option<Value> + Send + Sync>>,
    pub trusted: bool,
    pub replay: bool,
    pub map: Option<StaticMapRequest>,
}
thread_local! { static ACTIVE_RESIDUAL_HANDLERS: RefCell<Vec<ResidualHandlers>> = const { RefCell::new(Vec::new()) }; }
fn current_residual_handlers() -> Option<ResidualHandlers> {
    ACTIVE_RESIDUAL_HANDLERS.with(|handlers| handlers.borrow().last().cloned())
}
thread_local! { static ACTIVE_OBSERVED_RESIDUALS: RefCell<Vec<Vec<ResidualId>>> = const { RefCell::new(Vec::new()) }; }
// ---- arena ownership ----
// OCaml's evaluator has no lock: a host callback (the resolver, kernel/bulk/defer/map handlers, the inline-parameter hook and policy, the
// live-kernels and static facts/source hooks, a nested rewrite) re-enters and mutates the very evaluation that called it. Here an arena lives
// in a Slot. An evaluation checks it out (takes it out of the slot) and owns it while it runs callback-free code; around every host callback it
// lends it back (`lend`), so the callback, on this thread or any other, finds the live arena in its slot and changes it in place, and the
// evaluation takes it again when the callback returns: OCaml's sharing, with no copy. A thread holds at most one arena outside a lend and never
// waits for an arena while holding one (`not_holding` panics instead), so no wait can close a cycle: re-entrance cannot deadlock, and a callback
// reached without a lend is a panic that names the arena, never a hang. Trusted calls made while holding (operator bodies and checks, the
// interrupt probe) never reach an arena.
struct Slot { id: i64, cell: Mutex<Option<Arena>>, returned: Condvar }
#[path = "memory.rs"]
mod memory;
pub use memory::memory_allocations;
impl Drop for Slot { fn drop(&mut self) { ARENAS.lock().unwrap_or_else(|e| e.into_inner()).remove(&self.id); } }
thread_local! {
    static HELD: RefCell<Option<Arc<Slot>>> = const { RefCell::new(None) };
    static SPARE: RefCell<Vec<Arena>> = const { RefCell::new(Vec::new()) };
}
fn not_holding(id: i64) {
    HELD.with(|held| if let Some(slot) = held.borrow().as_ref() {
        panic!("flow_eval: arena {id} was reached while this thread holds arena {} outside a lend (a host callback or foreign access was not lent)", slot.id)
    });
}
/// A short callback-free access in place (`peek` as a guard).
fn held(slot: &Slot) -> std::sync::MutexGuard<'_, Option<Arena>> { not_holding(slot.id); filled(slot) }
fn filled(slot: &Slot) -> std::sync::MutexGuard<'_, Option<Arena>> {
    let mut cell = slot.cell.lock().unwrap_or_else(|e| e.into_inner());
    while cell.is_none() { cell = slot.returned.wait(cell).unwrap_or_else(|e| e.into_inner()); }
    cell
}
/// An arena checked out of its slot for an evaluation that may call back (it lends around each callback).
struct Out { slot: Arc<Slot>, arena: Option<Arena>, previous_interrupt: Option<Interrupt> }
impl std::ops::Deref for Out { type Target = Arena; fn deref(&self) -> &Arena { self.arena.as_ref().expect("checked out") } }
impl std::ops::DerefMut for Out { fn deref_mut(&mut self) -> &mut Arena { self.arena.as_mut().expect("checked out") } }
impl Drop for Out {
    fn drop(&mut self) {
        HELD.with(|held| { held.borrow_mut().take(); });
        // a hollow left by a callback that unwound is dropped: the live arena is already back in the slot
        let Some(mut arena) = self.arena.take().filter(|arena| arena.owns_index) else { return; };
        arena.interrupt = self.previous_interrupt.take();
        *self.slot.cell.lock().unwrap_or_else(|e| e.into_inner()) = Some(arena);
        self.slot.returned.notify_all();
    }
}
fn checkout(slot: &Arc<Slot>) -> Out {
    not_holding(slot.id);
    let mut arena = filled(slot).take();
    // A retained evaluation belongs to its current caller's cancellation scope.
    let previous_interrupt = arena.as_mut().and_then(|arena| std::mem::replace(&mut arena.interrupt, current_interrupt()));
    HELD.with(|held| *held.borrow_mut() = Some(slot.clone()));
    Out { slot: slot.clone(), arena, previous_interrupt }
}
/// A short callback-free read or update of an arena in place, waiting while another thread has it out.
fn peek<T>(slot: &Slot, f: impl FnOnce(&mut Arena) -> T) -> T {
    not_holding(slot.id);
    let mut cell = filled(slot);
    f(cell.as_mut().expect("filled"))
}
/// Run a host callback with `a` back in its slot (OCaml: the callback sees and changes this evaluation). A detached copy (a probe worker)
/// has no slot: its callback runs as is.
fn lend<T>(a: &mut Arena, f: impl FnOnce() -> T) -> T {
    let slot = HELD.with(|held| held.borrow().as_ref().filter(|slot| slot.id == a.id && a.owns_index).cloned());
    let Some(slot) = slot else { return f(); };
    let hollow = SPARE.with(|spare| spare.borrow_mut().pop()).unwrap_or_else(Arena::hollow);
    let mut live = std::mem::replace(a, hollow);
    live.lent += 1;
    // this evaluation's own frame, state, memo and counts: whatever runs while it is lent (a nested or another thread's evaluation) leaves them as they were
    let own = live_fields(&live);
    HELD.with(|held| { held.borrow_mut().take(); });
    *slot.cell.lock().unwrap_or_else(|e| e.into_inner()) = Some(live);
    slot.returned.notify_all();
    let result = f();
    not_holding(slot.id);
    let mut live = filled(&slot).take().expect("filled");
    live.lent -= 1;
    restore_live(&mut live, Some(own));
    HELD.with(|held| *held.borrow_mut() = Some(slot.clone()));
    let hollow = std::mem::replace(a, live);
    SPARE.with(|spare| { let mut spare = spare.borrow_mut(); if spare.len() < 8 { spare.push(hollow); } });
    result
}
// OCaml's with_live evaluates in a copy of the evaluation's state (`live_state`): its frame, state, resolver, elements, step count, memo and
// call counts are its own, it records nothing, and the evaluation it was reached from keeps its own. An entry reached from a callback of an
// evaluation of the same arena (the arena is lent) runs that way and puts them back on exit; residuals and functions it makes stay, as OCaml's
// values do.
struct LiveSaved { time: Option<Frame>, state: Option<State>, resolve: Option<ResolveFn>, elems: BTreeMap<String, Value>, step_count: usize, fn_calls: Shared<IdMap<FnId, i64>>, memo: Shared<IdMap<ResidualId, Value>>, record: bool }
fn save_live(a: &mut Arena) -> Option<LiveSaved> {
    (a.lent > 0).then(|| LiveSaved {
        time: a.time.clone(), state: a.state.clone(), resolve: a.resolve.clone(), elems: a.elems.clone(), step_count: a.step_count,
        fn_calls: std::mem::take(&mut a.fn_calls), memo: std::mem::take(&mut a.memo), record: std::mem::replace(&mut a.record, false),
    })
}
fn live_fields(a: &Arena) -> LiveSaved {
    LiveSaved { time: a.time.clone(), state: a.state.clone(), resolve: a.resolve.clone(), elems: a.elems.clone(), step_count: a.step_count, fn_calls: a.fn_calls.clone(), memo: a.memo.clone(), record: a.record }
}
fn restore_live(a: &mut Arena, saved: Option<LiveSaved>) {
    let Some(saved) = saved else { return; };
    (a.time, a.state, a.resolve, a.elems, a.step_count, a.fn_calls, a.memo, a.record) = (saved.time, saved.state, saved.resolve, saved.elems, saved.step_count, saved.fn_calls, saved.memo, saved.record);
}
// A residual handler's callback runs outside that handler, as an OCaml effect handler's does: an evaluation the callback starts (a kernel
// compile evaluating a term of its residual) offers nothing to the handler that is running it, only to the handlers around it.
fn outside_handler<T>(f: impl FnOnce() -> T) -> T {
    let handler = ACTIVE_RESIDUAL_HANDLERS.with(|handlers| handlers.borrow_mut().pop());
    let observed = ACTIVE_OBSERVED_RESIDUALS.with(|observed| observed.borrow_mut().pop());
    struct Restore(Option<ResidualHandlers>, Option<Vec<ResidualId>>);
    impl Drop for Restore {
        fn drop(&mut self) {
            if let Some(handler) = self.0.take() { ACTIVE_RESIDUAL_HANDLERS.with(|handlers| handlers.borrow_mut().push(handler)); }
            if let Some(observed) = self.1.take() { ACTIVE_OBSERVED_RESIDUALS.with(|queues| queues.borrow_mut().push(observed)); }
        }
    }
    let _restore = Restore(handler, observed);
    f()
}
fn call_lent(a: &mut Arena, id: ResidualId, callback: &dyn Fn(ResidualId) -> Option<Value>) -> Option<Value> { lend(a, || outside_handler(|| callback(id))) }
fn slot_of(home: i64) -> Option<Arc<Slot>> { ARENAS.lock().unwrap_or_else(|e| e.into_inner()).get(&home)?.upgrade() }
struct ResidualHandlerScope;
impl Drop for ResidualHandlerScope {
    fn drop(&mut self) {
        ACTIVE_RESIDUAL_HANDLERS.with(|handlers| { handlers.borrow_mut().pop(); });
        ACTIVE_OBSERVED_RESIDUALS.with(|observed| { observed.borrow_mut().pop(); });
    }
}

const MAX_STEPS: usize = 600_000;
const MAX_ITERATIONS: usize = 4096;
const MAX_DEPTH: usize = 64;
// Logical limits, not an estimate of available native stack. A callback re-entering
// evaluation shares the same scoped counter, even when it uses another arena.
const MAX_EVAL_NESTING: usize = 256;
const MAX_AUTHORED_NESTING: usize = 64;
thread_local! { static EVAL_NESTING: Cell<usize> = const { Cell::new(0) }; }
struct EvalNesting;
impl EvalNesting {
    fn enter() -> Result<Self, Diagnostic> {
        EVAL_NESTING.with(|depth| {
            if depth.get() >= MAX_EVAL_NESTING { return Err(depth_error()); }
            depth.set(depth.get() + 1);
            Ok(Self)
        })
    }
}
impl Drop for EvalNesting {
    fn drop(&mut self) { EVAL_NESTING.with(|depth| depth.set(depth.get() - 1)); }
}
fn depth_error() -> Diagnostic { error("E_DEPTH", "Evaluation is nested too deeply.") }
fn array_memory_error() -> Diagnostic { error("E_ARRAY_MEMORY", "Frame data exceeds available memory.") }
// Measured on uv_sphere (debug): a trusted bulk kernel costs ~1.5 ms to offer and wins over walking from a few hundred elements
// (25-48 elements: walk 0.4 ms vs kernel 1.5-2.4 ms; 1,106: walk 56-135 ms vs 13-22 ms; 2,209: 10 ms vs 6.5 ms).
const BULK_MIN_ELEMENTS: usize = 256;
const PACKED_BLOCK: usize = 1024;
pub const CONTEXT_LABEL: &str = "$label";
pub const CARD_KIND: &str = "$card";
pub fn context_label() -> &'static str { CONTEXT_LABEL }
pub fn card_kind() -> &'static str { CARD_KIND }
pub fn max_iterations() -> usize { MAX_ITERATIONS }

fn diagnostic(code: &str, message: &str) -> Diagnostic {
    Diagnostic { code: code.into(), severity: Severity::ErrorSeverity, position: None, message: message.into(), span: None, cause: None, hints: Vec::new(), trace: Vec::new() }
}
fn error(code: &str, message: &str) -> Diagnostic { diagnostic(code, message) }
fn errorf(code: &str, message: String) -> Diagnostic { diagnostic(code, &message) }
fn add_span(mut d: Diagnostic, term: &Term) -> Diagnostic {
    let form = term.form();
    if d.span.is_none() && (form.span.start != 0 || form.span.finish != 0) { d.span = Some(form.span.clone()); }
    d
}
fn fail_frame(frame: &Frame) -> Result<(), Diagnostic> { input::validate(frame).map_err(|message| error("E_FRAME", &message)) }
fn text(s: &str) -> Value { Value::Text(s.as_bytes().to_vec()) }
fn path_text(path: &[String]) -> String { path.join("/") }
fn int_value(v: &Value) -> Result<i64, Diagnostic> { value::int_of(v) }
fn num(v: &Value) -> Result<f64, Diagnostic> { value::num(v) }
fn truth(v: &Value) -> Result<bool, Diagnostic> { value::truthy(v) }
fn coerce(ty: &Ty, v: &Value) -> Result<Value, Diagnostic> { value::coerce_to(ty, v) }
fn array_len(v: &Value) -> Result<usize, Diagnostic> { value::array_length(v) }
fn array_get(v: &Value, index: usize) -> Result<Value, Diagnostic> {
    let i = i64::try_from(index).map_err(|_| error("E_LIST_RANGE", "Index is outside native storage bounds."))?;
    value::array_get(v, i)
}
fn children(term: &Term) -> impl Iterator<Item = Term> + '_ {
    let bypass = matches!(term.node, TermNode::Bypass(_));
    let input = if let TermNode::Bypass(call) = term.node { W::bypass_input(&term.child(call)) } else { None };
    input.into_iter().chain(term.children().iter().filter(move |_| !bypass).map(|id| term.child(*id)))
}
// Reject before recursive free-name analysis or compilation without consuming the native stack.
fn check_term_nesting(term: &Term) -> Result<(), Diagnostic> {
    let arena = term.arena();
    let mut pending = vec![(term.row_id(), 0)];
    while let Some((term, depth)) = pending.pop() {
        if depth >= MAX_AUTHORED_NESTING { return Err(depth_error()); }
        if let TermNode::Bypass(call) = arena.row(term).node {
            if let Some(child) = W::bypass_input(&arena.handle(call)) {
                pending.try_reserve(1).map_err(|_| array_memory_error())?;
                pending.push((child.row_id(), depth + 1));
            }
        } else {
            for child in arena.children(term) {
                pending.try_reserve(1).map_err(|_| array_memory_error())?;
                pending.push((*child, depth + 1));
            }
        }
    }
    Ok(())
}
fn check_value_nesting(value: &Value) -> Result<(), Diagnostic> {
    let mut pending = vec![(value, 0)];
    while let Some((value, depth)) = pending.pop() {
        if depth >= MAX_AUTHORED_NESTING { return Err(depth_error()); }
        match value {
            Value::List(values) => { pending.try_reserve(values.len()).map_err(|_| array_memory_error())?; pending.extend(values.iter().map(|value| (value, depth + 1))); },
            Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => {
                pending.try_reserve(fields.len()).map_err(|_| array_memory_error())?;
                pending.extend(fields.iter().map(|(_, value)| (value, depth + 1)));
            },
            _ => (),
        }
    }
    Ok(())
}

#[derive(Clone, Debug)]
pub struct Node {
    pub id: i64,
    pub inst: i64,
    pub site: Path,
    pub iter: Vec<i64>,
    pub kind: String,
    pub ty: Ty,
    pub args: Vec<(String, Value)>,
    pub error: Option<Diagnostic>,
}
#[derive(Clone, Debug)]
pub struct Instance { pub graph: String, pub default: bool, pub inputs: Vec<(String, Value)>, pub result: Value }
/// `id` is the plan's identity (OCaml `==`): assigned by `fresh_plan_id` where a plan is built, kept by clones.
#[derive(Clone, Debug)]
pub struct Plan { pub id: u64, pub instances: Vec<Instance>, pub nodes: Vec<Node> }
static NEXT_PLAN_ID: AtomicU64 = AtomicU64::new(1);
pub fn fresh_plan_id() -> u64 { NEXT_PLAN_ID.fetch_add(1, Ordering::Relaxed) }
pub type Record = (Path, Vec<(Vec<i64>, Value)>);
pub type Records = Vec<Record>;
#[derive(Clone)]
pub struct Eval {
    pub plan: Plan,
    pub authored: Vec<i64>,
    pub results: Vec<(String, Value)>,
    pub states: Vec<Value>,
    pub records: Records,
    owner: Arc<Slot>,
}
pub type T = Eval;
/// Explicit ownership for values that escape their evaluation as cook/host closures.
/// These roots live outside arenas, so mutually captured functions cannot form owner cycles.
#[derive(Clone, Default)]
pub struct Roots(Vec<Arc<Slot>>);
impl std::fmt::Debug for Roots {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_list().entries(self.0.iter().map(|home| home.id)).finish() }
}
impl Roots {
    pub fn is_empty(&self) -> bool { self.0.is_empty() }
}

pub fn roots<'a>(values: impl IntoIterator<Item = &'a Value>) -> Roots {
    fn visit(value: &Value, roots: &mut Roots, seen: &mut BTreeSet<(bool, i64, i64)>) {
        match value {
            Value::Residual(id) if seen.insert((false, id.0, id.1)) => {
                if let Some(home) = slot_of(id.1) {
                    if !roots.0.iter().any(|root| root.id == home.id) { roots.0.push(home); }
                    if let Some(bindings) = Residual::residual_bindings(*id) {
                        for (_, value) in &bindings { visit(value, roots, seen); }
                    }
                }
            }
            Value::Fn(id) if seen.insert((true, id.0, id.1)) => {
                if let Some(home) = slot_of(id.1) {
                    if !roots.0.iter().any(|root| root.id == home.id) { roots.0.push(home); }
                    for (_, value) in &Residual::function_bindings(*id) { visit(value, roots, seen); }
                }
            }
            Value::List(values) => for value in values.iter() { visit(value, roots, seen); },
            Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => {
                for (_, value) in fields { visit(value, roots, seen); }
            }
            _ => (),
        }
    }
    let mut roots = Roots::default();
    let mut seen = BTreeSet::new();
    for value in values { visit(value, &mut roots, &mut seen); }
    roots
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct StateKey { instance: i64, path: Path, iter: Vec<i64> }
// instance, then path, then iter: the OCaml compare of the key tuple
impl Ord for StateKey { fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.instance.cmp(&other.instance).then_with(|| self.path.cmp(&other.path)).then_with(|| self.iter.cmp(&other.iter)) } }
impl PartialOrd for StateKey { fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(other)) } }
#[derive(Clone, Debug, Default)]
struct StatePlane { source: String, frame: Option<i64>, before: BTreeMap<StateKey, Value>, next: BTreeMap<StateKey, Value>, epoch: i64 }
#[derive(Clone, Debug, Default)]
pub struct State { inner: Arc<Mutex<StateData>> }
#[derive(Clone, Debug, Default)]
struct StateData { source: String, frame: Option<i64>, before: BTreeMap<StateKey, Value>, next: BTreeMap<StateKey, Value>, host: Option<StatePlane>, epoch: i64 }
static STATE_EPOCH: AtomicI64 = AtomicI64::new(1);
static NEXT_ARENA: AtomicI64 = AtomicI64::new(1);
static NEXT_FID: AtomicI64 = AtomicI64::new(1);
static NEXT_RID: AtomicI64 = AtomicI64::new(1);
static NEXT_BULK: AtomicI64 = AtomicI64::new(-1);
static FREE_WALKS: AtomicI64 = AtomicI64::new(0);
static FREE_NAMES_CACHE: LazyLock<Mutex<HashMap<(u64, u32), BTreeSet<String>>>> = LazyLock::new(|| Mutex::new(HashMap::new()));
type Interrupt = Arc<dyn Fn() -> bool + Send + Sync>;
static INTERRUPTS: LazyLock<Mutex<HashMap<std::thread::ThreadId, Interrupt>>> = LazyLock::new(|| Mutex::new(HashMap::new()));
static ARENAS: LazyLock<Mutex<HashMap<i64, Weak<Slot>>>> = LazyLock::new(|| Mutex::new(HashMap::new()));
// ponytail: OCaml keeps an arena alive through any value that points into it; ids here do not, so each thread keeps its 64 newest arenas alive so a value
// outliving its Eval still resolves.  Per thread, not global: a shared ring let unrelated threads evict an arena a test still used.  Key by owner instead if 64 is not enough.
thread_local! { static RECENT_ARENAS: RefCell<std::collections::VecDeque<Arc<Slot>>> = const { RefCell::new(std::collections::VecDeque::new()) }; }
static SOURCE_HOOK: LazyLock<RwLock<Arc<dyn Fn(&dyn Fn(i64)->Option<Node>, &Value)->Option<Value> + Send + Sync>>> = LazyLock::new(|| RwLock::new(Arc::new(|_,_|None)));
static FACTS_HOOK: LazyLock<RwLock<Arc<dyn Fn(&dyn Fn(i64)->Option<Node>, i64)->String + Send + Sync>>> = LazyLock::new(|| RwLock::new(Arc::new(|_,_|String::new())));
static LIVE_KERNELS_HOOK: LazyLock<RwLock<Arc<dyn Fn(&[Node])->bool + Send + Sync>>> = LazyLock::new(|| RwLock::new(Arc::new(|_|true)));

// Hash every component of composite arena identities and public value IDs.
#[derive(Default)]
struct AddrHasher(u64);
impl std::hash::Hasher for AddrHasher {
    fn finish(&self) -> u64 { self.0 }
    fn write(&mut self, bytes: &[u8]) { for byte in bytes { self.0 = (self.0.rotate_left(5) ^ u64::from(*byte)).wrapping_mul(0x517c_c1b7_2722_0a95); } }
    fn write_usize(&mut self, n: usize) { self.write(&n.to_le_bytes()); }
    fn write_i64(&mut self, n: i64) { self.write(&n.to_le_bytes()); }
}
type AddrMap<V> = HashMap<(u64, u32), V, std::hash::BuildHasherDefault<AddrHasher>>;
type IdMap<K, V> = HashMap<K, V, std::hash::BuildHasherDefault<AddrHasher>>;
fn intern(a: &mut Arena, name: &str) -> Arc<str> {
    if let Some(found) = a.names.get(name) { return found.clone(); }
    let found: Arc<str> = Arc::from(name);
    a.names.insert(found.clone());
    found
}
fn base_of(a: &mut Arena, path: &Path) -> Arc<Path> {
    if let Some(base) = a.bases.get(path) { return base.clone(); }
    let base = Arc::new(path.clone());
    a.bases.insert(base.clone());
    base
}
fn term_base(a: &mut Arena, term: &Term) -> Option<Arc<Path>> {
    term.path.map(|path| packed_base(a, term, path))
}
fn packed_base(a: &mut Arena, term: &Term, path: flow_check::term_arena::PathId) -> Arc<Path> {
    let key = (term.identity().0, path);
    if let Some(base) = a.packed_bases.get(&key) { return base.clone(); }
    let base = Arc::new(packed_path(term, path));
    a.packed_bases.insert(key, base.clone());
    base
}
#[derive(Clone)]
struct ClosureData { params: (Term, RangeId), replayable: bool, body: Arc<Term>, env: Arc<BTreeMap<String, Value>>, zone: Path, calls: i64, at: ExecCtx }
#[derive(Clone)]
struct NamedData { name: String, calls: i64 }
#[derive(Clone)]
enum Function { Closure(Arc<ClosureData>), Named(NamedData) }
#[derive(Clone)]
struct ResidualData { term: Arc<Term>, env: Arc<BTreeMap<String, Value>>, ctx: ExecCtx, previous: bool, live: Option<Frame> }
#[derive(Clone)]
struct GraphCell { instance: i64, graph: String, default: bool, inputs: Vec<(String,Value)>, result: Value }
// Copy-on-write cell: a probe worker copies an Arena in O(fields); a write unshares only while such a copy is alive.
struct Shared<T>(Arc<T>);
impl<T> Shared<T> { fn new(value: T) -> Self { Shared(Arc::new(value)) } }
impl<T> Clone for Shared<T> { fn clone(&self) -> Self { Shared(self.0.clone()) } }
impl<T: Default> Default for Shared<T> { fn default() -> Self { Shared(Arc::new(T::default())) } }
impl<T> std::ops::Deref for Shared<T> { type Target = T; fn deref(&self) -> &T { &self.0 } }
impl<T: Clone> std::ops::DerefMut for Shared<T> { fn deref_mut(&mut self) -> &mut T { Arc::make_mut(&mut self.0) } }
#[derive(Clone)]
struct Arena {
    id: i64, workspace: Shared<Workspace>, functions: Shared<IdMap<FnId, Function>>, residuals: Shared<IdMap<ResidualId, ResidualData>>,
    // Replays share the immutable workspace and its lazily computed state-reset identity.
    source_key: Arc<std::sync::OnceLock<String>>,
    nodes: Shared<Vec<Node>>, authored: Vec<i64>, cells: Shared<Vec<GraphCell>>, cache: Shared<HashMap<String,usize>>, states: Shared<Vec<Value>>, records: Shared<BTreeMap<Path, Vec<(Vec<i64>,Value)>>>,
    observed: Vec<ResidualId>, res_log: Shared<Vec<ResidualId>>, fn_log: Shared<Vec<FnId>>, approx: u64, policies: Shared<HashMap<String, Option<Arc<InlineParameterPolicy>>>>,
    record: bool, time: Option<Frame>, resolve: Option<Arc<dyn Fn(&Value)->Result<Value,Diagnostic> + Send + Sync>>,
    state: Option<State>, elems: BTreeMap<String,Value>, step_count: usize, budget: usize, fn_calls: Shared<IdMap<FnId, i64>>, memo: Shared<IdMap<ResidualId, Value>>, interrupt: Option<Interrupt>, owns_index: bool, lent: u32, bases: Shared<HashSet<Arc<Path>>>, ops_seen: Shared<HashMap<String, (&'static str, OpRef)>>, names: Shared<HashSet<Arc<str>>>, arg_pool: Shared<AddrMap<Vec<(String, Value)>>>, routes: Shared<AddrMap<(usize, Seg, Route)>>, plans: Shared<AddrMap<CodePlan>>, fn_plans: Shared<AddrMap<CodePlan>>, pure: Shared<AddrMap<(&'static str, Option<usize>)>>, terms: Shared<AddrMap<(Stamp, Arc<Term>)>>, frees: Shared<AddrMap<(Stamp, Arc<Vec<String>>)>>,
    packed_bases: Shared<IdMap<(u64, flow_check::term_arena::PathId), Arc<Path>>>,
    // while an inlined body with a live argument evaluates: its deferred checks, oldest first (OCaml st.guards, newest first). A test that reads
    // only frame facts and selects an arm that fails statically is checked when the card cooks: the card fails with that diagnostic when the test is that bool
    guards: Option<Vec<(Value, bool, Diagnostic)>>,
}
static EMPTY_WORKSPACE: LazyLock<Shared<Workspace>> = LazyLock::new(|| Shared::new(Workspace {
    name: String::new(), graphs: Vec::new().into(), defs: Vec::new().into(), nodes: Vec::new().into(), macros: Vec::new().into(), source: Vec::new().into(),
    live: BTreeSet::new(), invariant: BTreeSet::new(), approx: BTreeSet::new(), packed: Vec::new().into(), packed_roots: Vec::new(), approx_reasons: Vec::new().into(),
    kind_fns: Vec::new(), ops: Vec::new().into(),
}));
impl Arena {
    fn fresh(id: i64, workspace: Shared<Workspace>, record: bool, budget: usize, interrupt: Option<Interrupt>, owns_index: bool) -> Arena {
        Arena {
            id, workspace, functions: Shared::default(), residuals: Shared::default(),
            source_key: Arc::new(std::sync::OnceLock::new()),
            nodes: Shared::default(), authored: Vec::new(), cells: Shared::default(), cache: Shared::default(),
            states: Shared::default(), records: Shared::default(), observed: Vec::new(), res_log: Shared::default(), fn_log: Shared::default(), approx: 0, policies: Shared::default(), record,
            time: None, resolve: None, state: Some(State::default()), elems: BTreeMap::new(), step_count: 0,
            budget, fn_calls: Shared::default(), memo: Shared::default(), interrupt, owns_index, lent: 0, bases: Shared::default(), ops_seen: Shared::default(), names: Shared::default(), arg_pool: Shared::default(), routes: Shared::default(), plans: Shared::default(), fn_plans: Shared::default(), pure: Shared::default(), terms: Shared::default(), frees: Shared::default(), guards: None,
            packed_bases: Shared::default(),
        }
    }
    /// What stands in for an arena while it is lent (never read: `owns_index` false marks it).
    fn hollow() -> Arena { Arena::fresh(0, EMPTY_WORKSPACE.clone(), false, 0, None, false) }
    fn new(workspace: &Workspace, record: bool, budget: usize) -> Arc<Slot> {
        let id = NEXT_ARENA.fetch_add(1, Ordering::Relaxed);
        let interrupt = INTERRUPTS.lock().unwrap().get(&std::thread::current().id()).cloned();
        let arena = Arena::fresh(id, Shared::new(workspace.clone()), record, budget, interrupt, true);
        let slot = Arc::new(Slot { id, cell: Mutex::new(Some(arena)), returned: Condvar::new() });
        ARENAS.lock().unwrap().insert(id, Arc::downgrade(&slot));
        RECENT_ARENAS.with(|recent| { let mut recent = recent.borrow_mut(); recent.push_back(slot.clone()); if recent.len() > 64 { recent.pop_front(); } });
        slot
    }
}
// the path of argument names below the term being run, shared between the contexts that extend it
#[derive(Clone)]
struct Route(Option<Arc<RouteNode>>);
// a route segment, spelled only when a site is read: most segments name a fixed part or a numbered one
#[derive(Clone)]
enum Seg { Static(&'static str), Index(&'static str, usize), Owned(Arc<str>) }
impl Seg {
    fn same(&self, other: &Seg) -> bool {
        match (self, other) { (Seg::Static(x), Seg::Static(y)) => x == y, (Seg::Index(p, i), Seg::Index(q, j)) => p == q && i == j, (Seg::Owned(x), Seg::Owned(y)) => x == y, _ => false }
    }
    fn text(&self) -> String { match self { Seg::Static(text) => (*text).to_owned(), Seg::Index(prefix, index) => format!("{prefix}{index}"), Seg::Owned(text) => text.to_string() } }
}
impl From<&'static str> for Seg { fn from(text: &'static str) -> Seg { Seg::Static(text) } }
impl From<String> for Seg { fn from(text: String) -> Seg { Seg::Owned(Arc::from(text)) } }
struct RouteNode { seg: Seg, up: Route }
impl Route {
    fn to_vec(&self) -> Vec<String> {
        let mut out = Vec::new();
        let mut at = &self.0;
        while let Some(node) = at { out.push(node.seg.text()); at = &node.up.0; }
        out.reverse();
        out
    }
}
#[derive(Clone)]
struct ExecCtx { arena: i64, inst: i64, prefix: Arc<Path>, base: Arc<Path>, route: Route, iter: Arc<Vec<i64>>, depth: usize, record: bool, data: bool, geometry: bool, host: bool, context: Context, card: Option<Arc<str>> }
// A scope: the captured bindings it starts from, and the names bound since, newest first (a bind is one node, not a map copy).
struct EnvNode { name: Arc<str>, value: Value, next: Option<Arc<EnvNode>> }
impl Drop for EnvNode {
    // unlink iteratively: a long chain must not recurse once per binding
    fn drop(&mut self) {
        let mut next = self.next.take();
        while let Some(node) = next { next = Arc::try_unwrap(node).ok().and_then(|mut node| node.next.take()); }
    }
}
#[derive(Clone)]
struct Env { values: Arc<BTreeMap<String,Value>>, top: Option<Arc<EnvNode>> }
impl Env {
    fn empty() -> Self { static EMPTY: LazyLock<Arc<BTreeMap<String, Value>>> = LazyLock::new(|| Arc::new(BTreeMap::new())); Self { values: EMPTY.clone(), top: None } }
    fn of(values: Arc<BTreeMap<String, Value>>) -> Self { Self { values, top: None } }
    fn get_ref(&self, name: &str) -> Option<&Value> {
        let mut at = &self.top;
        while let Some(node) = at { if &*node.name == name { return Some(&node.value); } at = &node.next; }
        self.values.get(name)
    }
    fn get(&self, name: &str) -> Option<Value> {
        let mut at = &self.top;
        while let Some(node) = at { if &*node.name == name { return Some(node.value.clone()); } at = &node.next; }
        self.values.get(name).cloned()
    }
    fn bind(&mut self, name: Arc<str>, value: Value) { self.top = Some(Arc::new(EnvNode { name, value, next: self.top.take() })); }
}

fn new_fid(a: &Arena) -> FnId { FnId(NEXT_FID.fetch_add(1, Ordering::Relaxed), a.id) }
fn new_rid(a: &Arena) -> ResidualId { ResidualId(NEXT_RID.fetch_add(1, Ordering::Relaxed), a.id) }
// OCaml closures are values, so a function made by another evaluation still calls; here it is read from the arena that made it (`a` lent meanwhile).
fn foreign_fn(a: &mut Arena, id: FnId) -> Option<Function> {
    if id.1 == a.id { return None; }
    lend(a, || slot_of(id.1).and_then(|home| peek(&home, |home| home.functions.get(&id).cloned())))
}
fn unshare(c: Arc<ClosureData>) -> ClosureData { Arc::try_unwrap(c).unwrap_or_else(|c| (*c).clone()) }
fn get_fn(a: &mut Arena, id: FnId) -> Result<Function, Diagnostic> {
    match own_fn(a, &id) { Some(f) => Ok(f.clone()), None => foreign_fn(a, id).ok_or_else(|| error("E_FUNCTION_ID", "Function belongs to an expired evaluation.")) }
}
// A residual or function of this arena: ids are keys only within their home (two plans may mint the same deterministic id).
fn own_res<'a>(a: &'a Arena, id: &ResidualId) -> Option<&'a ResidualData> { if id.1 == a.id { a.residuals.get(id) } else { None } }
fn own_fn<'a>(a: &'a Arena, id: &FnId) -> Option<&'a Function> { if id.1 == a.id { a.functions.get(id) } else { None } }
fn get_res(a: &Arena, id: ResidualId) -> Result<ResidualData, Diagnostic> { own_res(a, &id).cloned().ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation.")) }
fn closure_id(a:&mut Arena, f:Function)->FnId{let id=new_fid(a);insert_fn(a,id,f);id}
fn insert_fn(a:&mut Arena,id:FnId,f:Function){a.functions.insert(id,f);a.fn_log.push(id);}
fn replace_fn(a: &mut Arena, id: FnId, function: Function) {
    let unchanged = match (a.functions.get(&id), &function) {
        (Some(Function::Closure(old)), Function::Closure(new)) => old.body.identity() == new.body.identity()
            && Arc::ptr_eq(&old.env, &new.env) && old.at.context == new.at.context,
        _ => false,
    };
    a.functions.insert(id, function);
    // Residuals in other homes can capture this same function ID. A function never
    // read by a dependency query cannot affect a retained answer.
    if !unchanged {
        let mut memo = DEP_MEMO.lock().unwrap();
        if memo.watched.contains(&(id.1, id.0)) {
            memo.invalidate();
        }
    }
}
fn residual_id(a: &mut Arena, residual: ResidualData) -> ResidualId {
    let id = new_rid(a);
    insert_residual(a, id, residual);
    id
}
fn insert_residual(a: &mut Arena, id: ResidualId, residual: ResidualData) {
    a.residuals.insert(id, residual);
    a.res_log.push(id);
}
fn root_ctx(a:&Arena)->ExecCtx{ExecCtx{arena:a.id,inst:-1,prefix:Arc::new(Vec::new()),base:Arc::new(Vec::new()),route:Route(None),iter:Arc::new(Vec::new()),depth:0,record:true,data:false,geometry:false,host:false,context:flow_syntax::context::host(),card:None}}
fn site(c:&ExecCtx)->Path{let mut p=(*c.prefix).clone();p.extend(c.base.iter().cloned());p.extend(c.route.to_vec());p}
fn eval_sub(a: &mut Arena, c: &ExecCtx, seg: Seg, env: &Env, t: &Term) -> Result<Value, Diagnostic> {
    if t.path.is_some() { eval_at(a, c, env, t) } else { let cx = sub_at(a, c, t, seg); eval_at(a, &cx, env, t) }
}
// `eval_sub` under a name: the segment is spelled only when the term has no path of its own
fn eval_named(a: &mut Arena, c: &ExecCtx, name: &str, env: &Env, t: &Term) -> Result<Value, Diagnostic> {
    if t.path.is_some() { eval_at(a, c, env, t) } else { let seg = Seg::Owned(intern(a, name)); let cx = sub_at(a, c, t, seg); eval_at(a, &cx, env, t) }
}
// `sub` for a term's own position: a term is reached from one place, so its route node is made once per arena and parent route.
fn sub_at(a: &mut Arena, c: &ExecCtx, t: &Term, seg: Seg) -> ExecCtx {
    let key = t.identity();
    let parent = c.route.0.as_ref().map_or(0, |node| Arc::as_ptr(node) as usize);
    if let Some((at, known, route)) = a.routes.get(&key) {
        if *at == parent && known.same(&seg) { let mut cx = c.clone(); cx.route = route.clone(); return cx; }
    }
    let cx = sub(c, seg.clone());
    a.routes.insert(key, (parent, seg, cx.route.clone()));
    cx
}
fn sub(c:&ExecCtx,seg:Seg)->ExecCtx{let mut c=c.clone();c.route=Route(Some(Arc::new(RouteNode{seg,up:c.route.clone()})));c}
fn record_slot_open(a: &Arena, path: &Path) -> bool {
    a.records.get(path).map_or(true, |e| e.len() < MAX_ITERATIONS)
}
fn note(a:&mut Arena,c:&ExecCtx,path:Path,v:Value){if a.record&&c.record&&record_slot_open(a,&path){let e=a.records.entry(path).or_default();if e.len()<MAX_ITERATIONS{e.push(((*c.iter).clone(),v));}}}
fn named_args(a: &mut Arena, c: &ExecCtx, env: &Env, owner: &Term, args: &[flow_check::term_arena::Field]) -> Result<Vec<(String, Value)>, Diagnostic> {
    let mut values = Vec::with_capacity(args.len());
    for (index, arg) in args.iter().enumerate() {
        let name = owner.name(arg.name);
        let term = owner.child(arg.term);
        let count = args[..index].iter().filter(|other| other.name == arg.name).count();
        let value = if count == 0 { eval_named(a, c, name, env, &term)? } else { eval_sub(a, c, format!("{}#{}", name, count).into(), env, &term)? };
        values.push((name.to_owned(), value));
    }
    Ok(values)
}
fn cooked_geometry(a: &mut Arena, value: &Value) -> bool {
    match value {
        Value::Residual(id) if id.1 == a.id => own_res(a, id).is_some_and(|residual| *residual.term.ty() == flow_syntax::ty::geometry()),
        Value::Residual(id) => { let id = *id; lend(a, || slot_of(id.1).and_then(|home| peek(&home, |home| own_res(home, &id).map(|residual| *residual.term.ty() == flow_syntax::ty::geometry()))).unwrap_or(false)) }
        Value::Record(fields) | Value::Struct(_, _, fields) => fields.iter().any(|(_, value)| cooked_geometry(a, value)),
        _ => false,
    }
}
fn mk_node(a:&mut Arena,c:&ExecCtx,authored:i64,ty:Ty,kind:String,args:Vec<(String,Value)>)->Result<Value,Diagnostic>{
    if c.data{return eval_error("E_ARRAY_TYPE","Packed-array functions produce data, not graph nodes.")}
    if a.time.is_some(){return Err(errorf("E_LIVE_GEOMETRY",format!("{} makes geometry; geometry cannot be created while evaluating a live value.",kind)))}
    // A geometry computed from cooked data (a fold over sop/attr_names) is only a value at cook time; a node reading it is deferred with it,
    // up to the graph root, whose structural node cooks the lot (OCaml's `cooked_geometry`).
    if args.iter().any(|(_, value)| cooked_geometry(a, value)) { return need_time(); }
    push_node(a, c, authored, ty, kind, args)
}
// A plan node, as the card marker makes it (OCaml builds the marker without `mk_node`'s cooked-geometry deferral: its value may be a
// residual geometry the marker cooks behind).
fn push_node(a: &mut Arena, c: &ExecCtx, authored: i64, ty: Ty, kind: String, args: Vec<(String, Value)>) -> Result<Value, Diagnostic> {
    if c.data{return eval_error("E_ARRAY_TYPE","Packed-array functions produce data, not graph nodes.")}
    if a.time.is_some(){return Err(errorf("E_LIVE_GEOMETRY",format!("{} makes geometry; geometry cannot be created while evaluating a live value.",kind)))}
    let i=a.nodes.len() as i64;let node=Node{id:i,inst:c.inst,site:site(c),iter:(*c.iter).clone(),kind,ty:ty.clone(),args,error:None};
    a.nodes.push(node);a.authored.push(authored);Ok(Value::Deferred(ty,i))
}
fn context_result(ctx:Context)->Ty{flow_syntax::context::result(ctx)}
fn catalog_call(a:&mut Arena,c:&ExecCtx,authored:i64,kind:String,ctx:Context,args:Vec<(String,Value)>)->Result<Value,Diagnostic>{
    if !flow_syntax::context::supports_values(ctx)||ctx==flow_syntax::context::material(){return Ok(Value::Struct(kind,context_result(ctx),args))}
    mk_node(a,c,authored,context_result(ctx),kind,args)
}
fn literal_case(x: &flow_syntax::syntax::Syntax) -> Result<Value, Diagnostic> {
    use flow_syntax::syntax::Node as S;
    match x.node {
        S::Num(id) => {
            let s = x.name(id);
            if !s.contains('.') && !s.contains('e') && !s.contains('E') { if let Ok(n) = s.parse::<i64>() { return Ok(Value::Int(n)); } }
            s.parse::<f64>().map(Value::Float).map_err(|_| error("E_CASE", "Invalid numeric case literal."))
        }
        S::Str(id) => Ok(Value::Text(x.bytes(id).to_vec())),
        S::Sym(id) => Ok(Value::Bool(x.name(id) == "true")),
        _ => eval_error("E_CASE", "case matches literal numbers, text or booleans."),
    }
}
fn field<F: FnOnce() -> String>(v: Value, name: &str, form: F) -> Result<Value, Diagnostic> {
    match v {
        Value::Vec2(x, _) if name == "x" => Ok(Value::Float(x)),
        Value::Vec2(_, y) if name == "y" => Ok(Value::Float(y)),
        Value::Vec3(x, _, _) if name == "x" => Ok(Value::Float(x)),
        Value::Vec3(_, y, _) if name == "y" => Ok(Value::Float(y)),
        Value::Vec3(_, _, z) if name == "z" => Ok(Value::Float(z)),
        Value::Vec4(x, _, _, _) if name == "x" => Ok(Value::Float(x)),
        Value::Vec4(_, y, _, _) if name == "y" => Ok(Value::Float(y)),
        Value::Vec4(_, _, z, _) if name == "z" => Ok(Value::Float(z)),
        Value::Vec4(_, _, _, w) if name == "w" => Ok(Value::Float(w)),
        Value::Record(fs) => fs.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone()).ok_or_else(|| {
            let form = form();
            errorf("E_FIELD", format!("{} has no field {}.", form, name))
        }),
        _ => {
            let form = form();
            Err(errorf("E_FIELD", format!("{} has no output {}. A vec3 has .x .y .z; a record has its fields.", form, name)))
        }
    }
}
fn elem_type(vs:&[Value])->Ty{value::elem_ty(vs)}
fn join_list(vs:Vec<Value>)->Value{let ty=elem_type(&vs);Value::List(Arc::new(vs.iter().map(|v|coerce(&ty,v).unwrap_or_else(|_|v.clone())).collect()))}
fn live_in(v:&Value,a:&Arena)->bool{match v{Value::Residual(_)|Value::Source(..)=>true,Value::Struct(_,Ty::Array(_),_)=>true,Value::List(v)=>v.iter().any(|x|live_in(x,a)),Value::Record(fs)|Value::Struct(_,_,fs)=>fs.iter().any(|(_,v)|live_in(v,a)),_=>false}}
fn packed_pattern_name(arena: &TermArena, pattern: PatternId) -> String {
    match arena.pattern(pattern) {
        PatternNode::Name(name) => arena.name(name).to_owned(),
        PatternNode::Seq(range) => format!("[{}]", arena.pattern_ids(range).iter().map(|id| packed_pattern_name(arena, *id)).collect::<Vec<_>>().join(" ")),
        PatternNode::Keys(range) => format!("{{:keys [{}]}}", arena.name_ids(range).iter().map(|name| arena.name(*name)).collect::<Vec<_>>().join(" ")),
    }
}
fn bind_packed(a: &mut Arena, c: &ExecCtx, env: &mut Env, arena: &TermArena, pattern: PatternId, value: Value, mk: Option<&dyn Fn(&str) -> Path>) -> Result<(), Diagnostic> {
    match arena.pattern(pattern) {
        PatternNode::Name(name) => {
            let name = arena.name(name);
            if let Some(mk) = mk { note(a, c, mk(name), value.clone()); }
            env.bind(intern(a, name), value);
        }
        PatternNode::Seq(range) => {
            let value = concrete(a, value)?;
            let values = match value {
                Value::List(values) => values,
                Value::Vec2(x, y) => Arc::new(vec![Value::Float(x), Value::Float(y)]),
                Value::Vec3(x, y, z) => Arc::new(vec![Value::Float(x), Value::Float(y), Value::Float(z)]),
                Value::Vec4(x, y, z, w) => Arc::new(vec![Value::Float(x), Value::Float(y), Value::Float(z), Value::Float(w)]),
                _ => return Err(errorf("E_PATTERN", format!("{} destructures a list or vec3.", packed_pattern_name(arena, pattern)))),
            };
            let patterns = arena.pattern_ids(range);
            if values.len() < patterns.len() { return Err(errorf("E_PATTERN", format!("{} needs {} elements; the list has {}.", packed_pattern_name(arena, pattern), patterns.len(), values.len()))); }
            for (pattern, value) in patterns.iter().zip(values.iter()) { bind_packed(a, c, env, arena, *pattern, value.clone(), mk)?; }
        }
        PatternNode::Keys(range) => {
            let value = concrete(a, value)?;
            let pattern_name = packed_pattern_name(arena, pattern);
            for name in arena.name_ids(range) {
                let name = arena.name(*name);
                let field = field(value.clone(), name, || pattern_name.clone())?;
                if let Some(mk) = mk { note(a, c, mk(name), field.clone()); }
                env.bind(intern(a, name), field);
            }
        }
    }
    Ok(())
}
fn record_packed(a: &mut Arena, c: &ExecCtx, zone: &Path, arena: &TermArena, pattern: PatternId, value: &Value) -> Result<(), Diagnostic> {
    if !a.record || !c.record { return Ok(()); }
    let pattern_name = packed_pattern_name(arena, pattern);
    let mut path = zone.clone();
    path.push(format!(":{pattern_name}"));
    if !record_slot_open(a, &path) {
        return Ok(());
    }
    note(a, c, path, value.clone());
    match arena.pattern(pattern) {
        PatternNode::Name(_) => (),
        PatternNode::Seq(range) => {
            let values = match value {
                Value::List(values) => values.clone(),
                Value::Vec2(x, y) => Arc::new(vec![Value::Float(*x), Value::Float(*y)]),
                Value::Vec3(x, y, z) => Arc::new(vec![Value::Float(*x), Value::Float(*y), Value::Float(*z)]),
                Value::Vec4(x, y, z, w) => Arc::new(vec![Value::Float(*x), Value::Float(*y), Value::Float(*z), Value::Float(*w)]),
                _ => return Err(errorf("E_PATTERN", format!("{} destructures a list or vec3.", packed_pattern_name(arena, pattern)))),
            };
            let patterns = arena.pattern_ids(range);
            if values.len() < patterns.len() { return Err(errorf("E_PATTERN", format!("{} needs {} elements; the list has {}.", packed_pattern_name(arena, pattern), patterns.len(), values.len()))); }
            for (pattern, value) in patterns.iter().zip(values.iter()) { record_packed(a, c, zone, arena, *pattern, value)?; }
        }
        PatternNode::Keys(range) => {
            for name in arena.name_ids(range) {
                let name = arena.name(*name);
                let mut path = zone.clone();
                path.push(format!(":{name}"));
                if !record_slot_open(a, &path) {
                    continue;
                }
                let value = field(value.clone(), name, || pattern_name.clone())?;
                note(a, c, path, value);
            }
        }
    }
    Ok(())
}
fn owned_pattern(arena: &TermArena, pattern: PatternId) -> W::Pattern {
    match arena.pattern(pattern) {
        PatternNode::Name(name) => W::Pattern::Name(arena.name(name).to_owned()),
        PatternNode::Seq(range) => W::Pattern::Seq(arena.pattern_ids(range).iter().map(|pattern| owned_pattern(arena, *pattern)).collect()),
        PatternNode::Keys(range) => W::Pattern::Keys(arena.name_ids(range).iter().map(|name| arena.name(*name).to_owned()).collect()),
    }
}
fn function_params(term: &Term, range: RangeId) -> Vec<(W::Pattern, Option<Ty>)> {
    term.arena().params(range).iter().map(|parameter| (owned_pattern(term.arena(), parameter.pattern), parameter.ty.map(|ty| term.arena().ty(ty).clone()))).collect()
}
fn closure_params(closure: &ClosureData) -> &[flow_check::term_arena::Parameter] {
    closure.params.0.arena().params(closure.params.1)
}
fn packed_path(term: &Term, path: flow_check::term_arena::PathId) -> Path {
    term.arena().path(path).iter().map(|name| term.name(*name).to_owned()).collect()
}
fn packed_names(arena: &flow_check::term_arena::TermArena, pattern: flow_check::term_arena::PatternId, out: &mut BTreeSet<String>) {
    use flow_check::term_arena::PatternNode;
    match arena.pattern(pattern) {
        PatternNode::Name(name) => { out.insert(arena.name(name).to_owned()); }
        PatternNode::Seq(range) => for pattern in arena.pattern_ids(range) { packed_names(arena, *pattern, out); },
        PatternNode::Keys(range) => out.extend(arena.name_ids(range).iter().map(|name| arena.name(*name).to_owned())),
    }
}
fn free_names(term: &Term) -> BTreeSet<String> {
    let key = term.identity();
    if let Some(names) = FREE_NAMES_CACHE.lock().unwrap().get(&key) { return names.clone(); }
    FREE_WALKS.fetch_add(1, Ordering::Relaxed);
    fn walk(t: &Term, bound: &mut BTreeSet<String>, free: &mut BTreeSet<String>) {
        let arena = t.arena();
        match t.node {
            TermNode::RefBinding(id) => {
                let name = t.name(arena.ref_binding(id).name);
                if !bound.contains(name) { free.insert(name.to_owned()); }
            }
            TermNode::FnRef(id) => {
                let name = t.name(id);
                if !bound.contains(name) { free.insert(name.to_owned()); }
            }
            TermNode::CallFn(id) => {
                let call = arena.call_fn(id);
                let name = t.name(call.name);
                if !bound.contains(name) { free.insert(name.to_owned()); }
                for term in arena.term_ids(call.args) { walk(&t.child(*term), bound, free); }
                if let Some(term) = call.body { walk(&t.child(term), bound, free); }
            }
            TermNode::Let(id) => {
                let node = arena.let_node(id);
                let prior = bound.clone();
                for binding in arena.bindings(node.bindings) { walk(&t.child(binding.term), bound, free); packed_names(arena, binding.pattern, bound); }
                walk(&t.child(node.body), bound, free);
                *bound = prior;
            }
            TermNode::Fn(id) => {
                let node = arena.function(id);
                if let Some(term) = node.capture { walk(&t.child(term), bound, free); }
                let prior = bound.clone();
                for parameter in arena.params(node.params) { packed_names(arena, parameter.pattern, bound); }
                walk(&t.child(node.body), bound, free);
                *bound = prior;
            }
            TermNode::Loop(id) => {
                let node = arena.loop_node(id);
                let prior = bound.clone();
                for binding in arena.bindings(node.accs).iter().chain(arena.bindings(node.clauses)) {
                    walk(&t.child(binding.term), bound, free); packed_names(arena, binding.pattern, bound);
                }
                walk(&t.child(node.body), bound, free);
                *bound = prior;
            }
            TermNode::State(id) => {
                let node = arena.state(id);
                walk(&t.child(node.init), bound, free);
                let prior = bound.clone();
                packed_names(arena, node.binder, bound);
                walk(&t.child(node.step), bound, free);
                *bound = prior;
            }
            _ => for child in children(t) { walk(&child, bound, free); },
        }
    }
    let mut free = BTreeSet::new();
    walk(term, &mut BTreeSet::new(), &mut free);
    let mut cache = FREE_NAMES_CACHE.lock().unwrap();
    if cache.len() >= 16_384 { cache.clear(); }
    cache.insert(key, free.clone());
    free
}
fn capture_env(term: &Term, env: &Env) -> BTreeMap<String, Value> {
    free_names(term).into_iter().filter_map(|name| env.get(&name).map(|value| (name, value))).collect()
}
#[derive(Clone)]
struct Stamp((u64, u32));
impl Stamp {
    fn of(t: &Term) -> Stamp { Stamp(t.identity()) }
    fn is(&self, t: &Term) -> bool { self.0 == t.identity() }
}
// A term shared by the closures and residuals made from it: one copy per arena, not one per closure.
fn shared_term(a: &mut Arena, t: &Term) -> Arc<Term> {
    let key = t.identity();
    if let Some((stamp, term)) = a.terms.get(&key) { if stamp.is(t) { return term.clone(); } }
    let term = Arc::new(t.clone());
    a.terms.insert(key, (Stamp::of(t), term.clone()));
    term
}
fn make_residual(a: &mut Arena, t: &Term, env: &Env, c: &ExecCtx, previous: bool, live: Option<Frame>) -> ResidualId {
    let (term, captured) = (shared_term(a, t), capture_in(a, t, env));
    residual_id(a, ResidualData { term, env: Arc::new(captured), ctx: c.clone(), previous, live })
}
// `capture_env` with the free names of the term found once per arena.
fn capture_in(a: &mut Arena, t: &Term, env: &Env) -> BTreeMap<String, Value> {
    let key = t.identity();
    let names = match a.frees.get(&key) {
        Some((stamp, names)) if stamp.is(t) => names.clone(),
        _ => { let names = Arc::new(free_names(t).into_iter().collect::<Vec<_>>()); a.frees.insert(key, (Stamp::of(t), names.clone())); names }
    };
    names.iter().filter_map(|name| env.get(name).map(|value| (name.clone(), value))).collect()
}

fn eval_at(a: &mut Arena, c: &ExecCtx, env: &Env, t: &Term) -> Result<Value, Diagnostic> {
    let _nesting = EvalNesting::enter()?;
    if let Some(result) = run_compiled(a, c, env, t) { return result; }
    let owned;
    let c = if let Some(base) = term_base(a, t) { owned = ExecCtx { base, route: Route(None), ..c.clone() }; &owned } else { c };
    let r = eval_raw(a, c, env, t).and_then(|v| if a.time.is_some() { concrete(a, v) } else { Ok(v) }).map_err(|e| add_span(e, t));
    match r {
    Ok(v) => { if a.record && c.record { if let Some(path) = term_base(a, t) { note(a, c, (*path).clone(), v.clone()); } } Ok(v) },
    Err(d) if d.code == "$NEEDS_TIME" => {
        if a.time.is_some() { Err(error("E_EVAL", "A live value was needed while evaluating dynamically.")) }
        else {
            let residual = ResidualData { term: shared_term(a, t), env: Arc::new(capture_in(a, t, env)), ctx: c.clone(), previous: false, live: None };
            let value = Value::Residual(residual_id(a, residual));
            if matches!(t.node, TermNode::State(_)) { a.states.push(value.clone()); }
            if a.record && c.record { if let Some(path) = term_base(a, t) { note(a, c, (*path).clone(), value.clone()); } }
            Ok(value)
        }
    },
    Err(d)=>Err(d)} }
fn eval_error(code: &str, message: &str) -> Result<Value, Diagnostic> {
    Err(error(code, message))
}
fn need_time() -> Result<Value, Diagnostic> { Err(error("$NEEDS_TIME", "")) }
// ---- deferred checks (OCaml guardable / guarding / add_guard / arm_error) ----
// A deferred check's test reads frame facts only: no cooked data, fold state or zone element, so the card forces it with its cook's input alone.
// A residual or closure of another evaluation is not one (its environment is not at hand): the call stays the plain catalog node.
fn guardable(a: &mut Arena, v: &Value) -> bool {
    fn reads_elem(t: &Term) -> bool {
        matches!(t.node, TermNode::RefBinding(id) if t.name(t.arena().ref_binding(id).name).starts_with("$elem:")) || children(t).any(|child| reads_elem(&child))
    }
    // residual environments share values: each residual is walked once
    fn free(a: &Arena, v: &Value, seen: &mut HashSet<ResidualId>) -> bool {
        match v {
            Value::Residual(id) => seen.contains(id) || {
                seen.insert(*id);
                own_res(a, id).is_some_and(|r| r.env.values().all(|v| free(a, v, seen)) && !reads_elem(&r.term))
            },
            Value::Source(..) | Value::Struct(_, Ty::Array(_), _) => false,
            Value::Deferred(ty, _) => !flow_syntax::ty::is_cooked(ty),
            Value::List(xs) => xs.iter().all(|x| free(a, x, seen)),
            Value::Record(fs) | Value::Struct(_, _, fs) => fs.iter().all(|(_, v)| free(a, v, seen)),
            Value::Fn(id) => match own_fn(a, id) {
                Some(Function::Closure(cl)) => { let cl = cl.clone(); cl.env.values().all(|v| free(a, v, seen)) && !reads_elem(&cl.body) }
                Some(Function::Named(_)) => true,
                None => false,
            },
            _ => true,
        }
    }
    dep_value(a, v, true, &mut HashSet::new(), &mut HashMap::new()) && !dep_value(a, v, false, &mut HashSet::new(), &mut HashMap::new()) && free(a, v, &mut HashSet::new())
}
// a live test of an inlined body's if/cond that can be a deferred check
fn guarding(a: &mut Arena, v: &Value) -> bool { a.guards.is_some() && matches!(v, Value::Residual(_)) && guardable(a, v) }
fn add_guard(a: &mut Arena, test: Value, fails: bool, d: Diagnostic) { if let Some(guards) = a.guards.as_mut() { guards.push((test, fails, d)); } }
// the diagnostic an arm fails with whatever the frame, or None: the nodes it made are dropped
fn arm_error(a: &mut Arena, c: &ExecCtx, env: &Env, arm: &Term) -> Result<Option<Diagnostic>, Diagnostic> {
    let (nodes, authored) = (a.nodes.len(), a.authored.len());
    let result = eval_at(a, c, env, arm);
    a.nodes.truncate(nodes);
    a.authored.truncate(authored);
    match result {
        Ok(_) => Ok(None),
        // OCaml's Needs_t passes through arm_error
        Err(d) if d.code == "$NEEDS_TIME" => Err(d),
        Err(d) if ["cancelled", "E_CANCELLED", "E_EVAL_BUDGET", "E_DEPTH"].contains(&d.code.as_str()) => Err(d),
        Err(d) => Ok(Some(d)),
    }
}
// a validation refusal (OCaml `refusal`): E_LOWER naming the validation's code, an E_PARAM by its message alone
fn refusal(code: &str, message: &str) -> Diagnostic { error("E_LOWER", &if code == "E_PARAM" { message.to_owned() } else { format!("{code}: {message}") }) }
fn force_res(a: &mut Arena, id: ResidualId) -> Result<Value, Diagnostic> {
    // a residual a closure carried in from another evaluation (a library body called with the graph's function) is forced where it lives;
    // it reads cooked sources through the evaluation that is forcing it
    if id.1 != a.id {
        if let Some(home) = slot_of(id.1) {
            let (time, state, resolve, elems) = (a.time.clone(), a.state.clone(), a.resolve.clone(), a.elems.clone());
            return lend(a, || {
                let mut home = checkout(&home);
                let saved = (home.time.clone(), home.state.clone(), home.resolve.clone(), home.elems.clone());
                (home.time, home.state, home.resolve, home.elems) = (time, state, resolve, elems);
                let forced = force_res(&mut home, id);
                (home.time, home.state, home.resolve, home.elems) = saved;
                forced
            });
        }
    }
    if let Some(value) = a.memo.get(&id) { return Ok(value.clone()); }
    let value = execute_residual(a, id)?;
    a.memo.insert(id, value.clone());
    Ok(value)
}
fn execute_residual(a: &mut Arena, id: ResidualId) -> Result<Value, Diagnostic> {
    let _nesting = EvalNesting::enter()?;
    let residual = get_res(a, id)?;
    let env = Env::of(residual.env);
    if residual.previous {
        if let TermNode::State(id) = residual.term.node {
            let node = residual.term.arena().state(id);
            let zone = packed_base(a, &residual.term, node.zone);
            return state_previous(a, &residual.ctx, &env, &zone, &residual.term.child(node.init));
        }
        return eval_error("E_STATE", "A previous-state residual is not a state term.");
    }
    if a.time.is_some() && kernel_eligible(&residual.term) && current_residual_handlers().is_some_and(|handler| handler.kernel.is_some()) {
        if let Some(value) = kernel_value(a, id, residual.term.ty()) { return Ok(value); }
    }
    eval_at(a, &residual.ctx, &env, &residual.term)
}
fn kernel_eligible(term: &Term) -> bool {
    match term.node {
        TermNode::Hof(id) => matches!(term.arena().hof(id).kind, W::HofKind::Map | W::HofKind::Reduce),
        TermNode::Loop(_) => true,
        TermNode::Op(id) => matches!(term.name(term.arena().op(id).op), "array/sum" | "image/map"),
        _ => false,
    }
}
fn kernel_value(a: &mut Arena, id: ResidualId, ty: &Ty) -> Option<Value> {
    let handler = current_residual_handlers()?;
    let kernel = handler.kernel?;
    let value = call_lent(a, id, &*kernel)?;
    if value::ty_of(&value) != *ty || value::validate(&value).is_err() { return None; }
    Some(value)
}
fn concrete(a: &mut Arena, value: Value) -> Result<Value, Diagnostic> {
    match value {
        Value::Residual(id) => if a.time.is_none() { need_time() } else { force_res(a, id) },
        Value::Source(..) | Value::Struct(_, Ty::Array(_), _) => {
            if a.time.is_none() { return need_time(); }
            resolve_source(a, &value)
        }
        value => Ok(value),
    }
}
fn eval_concrete(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term) -> Result<Value, Diagnostic> {
    let value = eval_at(a, c, env, term)?;
    concrete(a, value)
}
fn geometry_ids(value: &Value, out: &mut Vec<i64>) {
    match value {
        Value::Deferred(ty, id) if flow_syntax::ty::is_cooked(ty) => out.push(*id),
        Value::List(values) => for value in values.iter() { geometry_ids(value, out); },
        _ => (),
    }
}
fn function_value(value: &Value) -> bool {
    match value {
        Value::Fn(_) => true,
        Value::List(values) => values.iter().any(function_value),
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().any(|(_, value)| function_value(value)),
        _ => false,
    }
}
// ---- inline body memo (OCaml's inline_memo / replay_inline) ----
// An inline body's evaluation, kept for a later evaluation of the same card (declaration, canonical parameters, input shapes):
// its nodes in creation order and value, the residuals and functions it made, the kernels it offered, and the site and iteration it ran at.
// A replay puts them at the next ids of the new evaluation. Only a body whose values reference nothing but what it made (or its cooked inputs) is kept.
struct InlineRegion { nodes: usize, authored: usize, residuals: usize, functions: usize, observed: usize, states: usize, cells: usize, approx: u64 }
struct InlineEntry {
    nodes: Vec<Node>, authored: Vec<i64>, value: Value, observed: Vec<ResidualId>, lo: i64, inputs: Vec<i64>, site: Path, iter: Vec<i64>,
    residuals: Vec<(ResidualId, ResidualData)>, functions: Vec<(FnId, Function, Option<i64>)>,
    guards: Vec<(Value, bool, Diagnostic)>, // the body's deferred checks, oldest first
}
#[derive(Default)]
struct InlineMemo { entries: HashMap<Vec<u8>, Arc<InlineEntry>>, sites: HashMap<(Path, Vec<i64>, i64), Vec<Vec<u8>>> }
static INLINE_MEMO: LazyLock<Mutex<InlineMemo>> = LazyLock::new(|| Mutex::new(InlineMemo::default()));
const INLINE_VARIANTS: usize = 8;
const INLINE_ENTRIES: usize = 256;
// bounded twice: 256 entries (a whole-table clear) and INLINE_VARIANTS per call site, the newest kept
fn inline_store(form: i64, key: Vec<u8>, entry: InlineEntry) {
    let site = (entry.site.clone(), entry.iter.clone(), form);
    let mut memo = INLINE_MEMO.lock().unwrap();
    if memo.entries.len() >= INLINE_ENTRIES { memo.entries.clear(); memo.sites.clear(); }
    let mut keys = memo.sites.get(&site).cloned().unwrap_or_default();
    if !keys.contains(&key) { keys.insert(0, key.clone()); }
    for stale in keys.iter().skip(INLINE_VARIANTS) { memo.entries.remove(stale); }
    keys.truncate(INLINE_VARIANTS);
    memo.sites.insert(site, keys);
    memo.entries.insert(key, Arc::new(entry));
}
// The cooked inputs of an inlined call, in argument order.
fn inline_inputs(parameters: &[(String, Value)], args: &[(String, Value)]) -> Vec<i64> {
    fn ids(value: &Value, out: &mut Vec<i64>) {
        match value { Value::Deferred(_, id) => out.push(*id), Value::List(values) => for value in values.iter() { ids(value, out); }, _ => () }
    }
    let mut out = Vec::new();
    for (name, value) in args { if !parameters.iter().any(|(p, _)| p == name) { ids(value, &mut out); } }
    out
}
fn has_fn(value: &Value) -> bool {
    match value {
        Value::Fn(_) => true,
        Value::List(values) => values.iter().any(has_fn),
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().any(|(_, value)| has_fn(value)),
        _ => false,
    }
}
fn key_bytes(out: &mut Vec<u8>, bytes: &[u8]) { out.extend_from_slice(bytes.len().to_string().as_bytes()); out.push(b':'); out.extend_from_slice(bytes); }
type FactsFn = dyn Fn(&dyn Fn(i64) -> Option<Node>, i64) -> String;
fn input_shape(value: &Value, out: &mut Vec<u8>, facts: &FactsFn, plan_node: &dyn Fn(i64) -> Option<Node>) -> bool {
    match value {
        Value::Deferred(ty, id) => { out.push(b'D'); key_bytes(out, ty::to_string(ty).as_bytes()); key_bytes(out, facts(plan_node, *id).as_bytes()); true }
        Value::No_geo => { out.push(b'N'); true }
        Value::List(values) => { out.push(b'['); for value in values.iter() { if !input_shape(value, out, facts, plan_node) { return false; } out.push(b','); } out.push(b']'); true }
        Value::Source(..) | Value::Struct(..) | Value::Record(..) | Value::Fn(..) | Value::Residual(..) => false,
        value => { out.push(b'='); key_bytes(out, &value::key_of(&|_| 0, value)); true }
    }
}
// What an inline body's evaluation is a function of: the declaration, the canonical parameters and the shape of the inputs (cooked handles by type and static facts, not identity).
// None for a live or function parameter, a non-data input, or outside the static evaluation.
const INLINE_MEMO_ON: bool = true;
fn inline_key(a: &mut Arena, c: &ExecCtx, policy: &InlineParameterPolicy, parameters: &[(String, Value)], args: &[(String, Value)]) -> Option<Vec<u8>> {
    if !INLINE_MEMO_ON || a.time.is_some() || c.data { return None; }
    if parameters.iter().any(|(_, value)| has_fn(value) || is_live_value(value)) { return None; }
    let mut key = Vec::new();
    key_bytes(&mut key, policy.key.as_bytes());
    key_bytes(&mut key, policy.definition.body.body.form().id.to_string().as_bytes());
    key_bytes(&mut key, &value::key_of(&|_| 0, &Value::Record(parameters.to_vec())));
    let facts = static_facts_get();
    let nodes = a.nodes.clone();
    let plan_node = |id: i64| -> Option<Node> { nodes.get(usize::try_from(id).ok()?).filter(|node| node.id == id).cloned() };
    for (name, value) in args {
        if parameters.iter().any(|(p, _)| p == name) { continue; }
        key.push(1);
        key_bytes(&mut key, name.as_bytes());
        // the facts hook is the host's: it runs with the arena lent
        if !lend(a, || input_shape(value, &mut key, &*facts, &plan_node)) { return None; }
    }
    key.push(c.geometry as u8);
    key_bytes(&mut key, c.context.as_str().as_bytes());
    key.push(current_residual_handlers().is_some_and(|handlers| handlers.defer.is_some()) as u8);
    Some(key)
}
fn value_ids(value: &Value, residuals: &mut Vec<ResidualId>, functions: &mut Vec<FnId>) {
    match value {
        Value::Residual(id) => residuals.push(*id),
        Value::Fn(id) => functions.push(*id),
        Value::List(values) => for value in values.iter() { value_ids(value, residuals, functions); },
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => for (_, value) in fields { value_ids(value, residuals, functions); },
        _ => (),
    }
}
// None when the body's results point at anything it did not make: such a body is evaluated again each time.
fn inline_entry(a: &Arena, c: &ExecCtx, region: &InlineRegion, value: &Value, inputs: Vec<i64>, guards: &[(Value, bool, Diagnostic)]) -> Option<InlineEntry> {
    let mut residuals = Vec::new();
    for id in &a.res_log[region.residuals..] { residuals.push((*id, a.residuals.get(id)?.clone())); }
    let mut functions = Vec::new();
    for id in &a.fn_log[region.functions..] { functions.push((*id, a.functions.get(id)?.clone(), a.fn_calls.get(id).copied())); }
    let made_r: HashSet<ResidualId> = residuals.iter().map(|(id, _)| *id).collect();
    let made_f: HashSet<FnId> = functions.iter().map(|(id, _, _)| *id).collect();
    let (mut rs, mut fs) = (Vec::new(), Vec::new());
    value_ids(value, &mut rs, &mut fs);
    for (test, _, _) in guards { value_ids(test, &mut rs, &mut fs); }
    let nodes: Vec<Node> = a.nodes[region.nodes..].to_vec();
    for node in &nodes { for (_, v) in &node.args { value_ids(v, &mut rs, &mut fs); } }
    for (_, residual) in &residuals { for v in residual.env.values() { value_ids(v, &mut rs, &mut fs); } }
    for (_, function, _) in &functions { if let Function::Closure(closure) = function { for v in closure.env.values() { value_ids(v, &mut rs, &mut fs); } } }
    let observed = a.observed[region.observed..].to_vec();
    rs.extend(observed.iter().copied());
    if !rs.iter().all(|id| made_r.contains(id)) || !fs.iter().all(|id| made_f.contains(id)) { return None; }
    Some(InlineEntry {
        nodes, authored: a.authored[region.authored..].to_vec(), value: value.clone(), observed, lo: region.nodes as i64, inputs,
        site: site(c), iter: (*c.iter).clone(), residuals, functions, guards: guards.to_vec(),
    })
}
struct Reloc { lo: i64, count: i64, new_lo: i64, inputs: Vec<(i64, i64)>, rmap: HashMap<ResidualId, ResidualId>, fmap: HashMap<FnId, FnId>, site: Path, iter: Vec<i64>, new_site: Path, new_iter: Vec<i64>, arena: i64, inst: i64 }
impl Reloc {
    fn id(&self, n: i64) -> i64 {
        if n >= self.lo && n < self.lo + self.count { n - self.lo + self.new_lo } else { self.inputs.iter().find(|(old, _)| *old == n).map_or(n, |(_, new)| *new) }
    }
    fn path(&self, p: &Path) -> Path {
        if p.len() >= self.site.len() && p[..self.site.len()] == self.site[..] { let mut out = self.new_site.clone(); out.extend_from_slice(&p[self.site.len()..]); out } else { p.clone() }
    }
    fn iter(&self, i: &[i64]) -> Vec<i64> {
        if i.len() >= self.iter.len() && i[..self.iter.len()] == self.iter[..] { let mut out = self.new_iter.clone(); out.extend_from_slice(&i[self.iter.len()..]); out } else { i.to_vec() }
    }
    fn ctx(&self, x: &ExecCtx) -> ExecCtx {
        let mut y = x.clone();
        y.arena = self.arena; y.inst = self.inst;
        y.prefix = Arc::new(self.path(&x.prefix));
        y.iter = Arc::new(self.iter(&x.iter));
        y
    }
    fn fields(&self, fields: &[(String, Value)]) -> Vec<(String, Value)> { fields.iter().map(|(name, value)| (name.clone(), self.value(value))).collect() }
    fn changes(&self, value: &Value) -> bool {
        match value {
            Value::Deferred(_, id) => self.id(*id) != *id,
            Value::Residual(id) => self.rmap.get(id).is_some_and(|new| new != id),
            Value::Fn(id) => self.fmap.get(id).is_some_and(|new| new != id),
            Value::List(values) => values.iter().any(|value| self.changes(value)),
            Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().any(|(_, value)| self.changes(value)),
            _ => false,
        }
    }
    fn value(&self, value: &Value) -> Value {
        match value {
            Value::Deferred(ty, n) => Value::Deferred(ty.clone(), self.id(*n)),
            Value::List(values) if !values.iter().any(|value| self.changes(value)) => Value::List(values.clone()),
            Value::List(values) => Value::List(Arc::new(values.iter().map(|value| self.value(value)).collect())),
            Value::Record(fields) => Value::Record(self.fields(fields)),
            Value::Struct(name, ty, fields) => Value::Struct(name.clone(), ty.clone(), self.fields(fields)),
            Value::Source(name, ty, fields) => Value::Source(name.clone(), ty.clone(), self.fields(fields)),
            Value::Residual(id) => Value::Residual(*self.rmap.get(id).unwrap_or(id)),
            Value::Fn(id) => Value::Fn(*self.fmap.get(id).unwrap_or(id)),
            value => value.clone(),
        }
    }
    fn env(&self, env: &Arc<BTreeMap<String, Value>>) -> Arc<BTreeMap<String, Value>> {
        if !env.values().any(|value| self.changes(value)) { return env.clone(); }
        Arc::new(env.iter().map(|(name, value)| (name.clone(), self.value(value))).collect())
    }
}
// An inline body's kept evaluation, in this evaluation: its nodes at the next ids over these inputs, its residuals and functions at fresh ids,
// its sites and iterations under this call's, its kernels offered again. The value equals what evaluating the body here gives.
fn replay_inline(a: &mut Arena, c: &ExecCtx, e: &InlineEntry, inputs: &[i64]) -> Option<(Value, Vec<(Value, bool, Diagnostic)>)> {
    if inputs.len() != e.inputs.len() { return None; }
    let mut reloc = Reloc {
        lo: e.lo, count: e.nodes.len() as i64, new_lo: a.nodes.len() as i64, inputs: e.inputs.iter().copied().zip(inputs.iter().copied()).collect(),
        rmap: HashMap::new(), fmap: HashMap::new(), site: e.site.clone(), iter: e.iter.clone(), new_site: site(c), new_iter: (*c.iter).clone(), arena: c.arena, inst: c.inst,
    };
    for (id, _) in &e.residuals { reloc.rmap.insert(*id, new_rid(a)); }
    for (id, _, _) in &e.functions { reloc.fmap.insert(*id, new_fid(a)); }
    let nodes: Vec<Node> = e.nodes.iter().map(|n| Node {
        id: reloc.id(n.id), inst: c.inst, site: reloc.path(&n.site), iter: reloc.iter(&n.iter), kind: n.kind.clone(), ty: n.ty.clone(), args: reloc.fields(&n.args), error: n.error.clone(),
    }).collect();
    let residuals: Vec<(ResidualId, ResidualData)> = e.residuals.iter().map(|(id, r)| (reloc.rmap[id], ResidualData {
        term: r.term.clone(), env: reloc.env(&r.env), ctx: reloc.ctx(&r.ctx), previous: r.previous, live: r.live.clone(),
    })).collect();
    let functions: Vec<(FnId, Function, Option<i64>)> = e.functions.iter().map(|(id, f, calls)| {
        let f = match f {
            Function::Closure(closure) => {
                let mut closure = (**closure).clone();
                closure.env = reloc.env(&closure.env);
                closure.zone = reloc.path(&closure.zone);
                closure.at = reloc.ctx(&closure.at);
                Function::Closure(Arc::new(closure))
            }
            Function::Named(named) => Function::Named(named.clone()),
        };
        (reloc.fmap[id], f, *calls)
    }).collect();
    let value = reloc.value(&e.value);
    let guards: Vec<(Value, bool, Diagnostic)> = e.guards.iter().map(|(test, fails, d)| (reloc.value(test), *fails, d.clone())).collect();
    let observed: Vec<ResidualId> = e.observed.iter().map(|id| reloc.rmap[id]).collect();
    for (node, form) in nodes.into_iter().zip(e.authored.iter()) { a.nodes.push(node); a.authored.push(*form); }
    for (id, residual) in residuals { insert_residual(a, id, residual); }
    for (id, function, calls) in functions { insert_fn(a, id, function); if let Some(calls) = calls { a.fn_calls.insert(id, calls); } }
    for id in observed {
        a.observed.push(id);
        ACTIVE_OBSERVED_RESIDUALS.with(|queues| { if let Some(queue) = queues.borrow_mut().last_mut() { queue.push(id); } });
    }
    Some((value, guards))
}

fn inline_policy_for(a: &mut Arena, kind: &str) -> Option<Arc<InlineParameterPolicy>> {
    if let Some(found) = a.policies.get(kind) { return found.clone(); }
    let workspace = a.workspace.clone();
    let hook = inline_parameters();
    let found = workspace.nodes.iter().find(|definition| definition.kind.qualified == kind).map(|original| Arc::new(lend(a, || hook(&workspace, original))));
    a.policies.insert(kind.to_owned(), found.clone());
    found
}
fn inline_node(a: &mut Arena, c: &ExecCtx, _authored: i64, policy: &InlineParameterPolicy, args: &[(String, Value)], call: &Term) -> Result<Value, Diagnostic> {
    if c.depth > MAX_DEPTH { return eval_error("E_DEPTH", "Call depth exceeds 64."); }
    let authored = call.form().id;
    let definition = &policy.definition;
    // the policy reads and rebinds residual arguments (with_bindings): it runs with this arena lent, as every host callback does
    let Some(parameters) = lend(a, || (policy.canonicalize)(args))? else {
        return catalog_call(a, c, authored, definition.kind.qualified.clone(), definition.kind.context.clone(), args.to_vec());
    };
    let live = args.iter().any(|(_, value)| live_in(value, a));
    let mut context = c.clone();
    context.prefix = Arc::new(site(c));
    context.base = Arc::new(vec![format!("node:{}", definition.kind.qualified)]);
    context.route = Route(None);
    context.depth += 1;
    context.record = false;
    context.card = definition.kind.qualified.split_once('/').map(|(_, label)| Arc::<str>::from(label)).or_else(|| Some(Arc::<str>::from(definition.kind.qualified.as_str())));
    let region = InlineRegion { nodes: a.nodes.len(), authored: a.authored.len(), residuals: a.res_log.len(), functions: a.fn_log.len(), observed: a.observed.len(), states: a.states.len(), cells: a.cells.len(), approx: a.approx };
    let inputs = inline_inputs(&parameters, args);
    let key = inline_key(a, c, policy, &parameters, args);
    let replayed = key.as_ref().and_then(|key| {
        let entry = INLINE_MEMO.lock().unwrap().entries.get(key).cloned()?;
        replay_inline(a, c, &entry, &inputs)
    });
    let was_replayed = replayed.is_some();
    let mut body_nodes = region.nodes;
    let mut body_authored = region.authored;
    // the body's deferred checks (a replay brings its own); gathered only while an argument is live, as OCaml's st.guards
    let guards;
    let evaluated = if let Some((value, replayed_guards)) = replayed { guards = replayed_guards; Ok(value) } else {
        let gathers = args.iter().any(|(_, value)| matches!(value, Value::Residual(_)));
        let outer = std::mem::replace(&mut a.guards, if gathers { Some(Vec::new()) } else { None });
        let outer_steps = std::mem::replace(&mut a.step_count, 0);
        let body = inline_body(a, &context, definition, &parameters, args, &mut body_nodes, &mut body_authored);
        a.step_count = outer_steps;
        guards = std::mem::replace(&mut a.guards, outer).unwrap_or_default();
        match body? {
            Some(evaluated) => evaluated,
            // a validation that reads a live argument it cannot defer (OCaml's Needs_t in the call): the plain catalog node
            None => return catalog_call(a, c, authored, definition.kind.qualified.clone(), definition.kind.context.clone(), args.to_vec()),
        }
    };
    let result = match evaluated {
        Ok(value) => value,
        Err(diagnostic) if !is_cancelled(&diagnostic) => {
            a.nodes.truncate(body_nodes);
            a.authored.truncate(body_authored);
            let slots = definition.slots.iter().map(|slot| {
                let mut ids = Vec::new();
                for (_, value) in args.iter().filter(|(name, _)| name == &slot.name) { geometry_ids(value, &mut ids); }
                (slot.name.clone(), Value::List(Arc::new(ids.into_iter().map(Value::Int).collect())))
            }).collect::<Vec<_>>();
            let mut marker_args = vec![
                ("value".into(), Value::No_geo),
                ("kind".into(), text(&definition.kind.qualified)),
                ("parameters".into(), Value::Record(parameters.clone())),
                ("slots".into(), Value::Record(slots)),
            ];
            marker_args.extend(args.iter().filter(|(_, value)| function_value(value)).cloned());
            let Value::Deferred(_, id) = push_node(a, c, authored, flow_syntax::ty::geometry(), "$card".into(), marker_args)? else {
                return eval_error("E_TYPE", "A failed node definition must produce a geometry marker.");
            };
            if let Some(node) = a.nodes.iter_mut().find(|node| node.id == id) { node.error = Some(diagnostic); }
            return Ok(Value::Deferred(flow_syntax::ty::geometry(), id))
        }
        Err(diagnostic) => return Err(diagnostic),
    };
    let made = a.nodes.clone();
    // the hook reads other values of this arena (frame_dependent, compile): it runs with the arena lent
    let takes_nodes = made.len() == body_nodes || { let hook = live_kernels_get(); lend(a, || hook(&made[body_nodes..])) };
    drop(made);
    if live && (matches!(&result, Value::Residual(_)) || !takes_nodes) {
        a.nodes.truncate(body_nodes);
        a.authored.truncate(body_authored);
        return catalog_call(a, c, authored, definition.kind.qualified.clone(), definition.kind.context.clone(), args.to_vec());
    }
    if !was_replayed && a.approx == region.approx && a.states.len() == region.states && a.cells.len() == region.cells {
        if let Some((key, entry)) = key.and_then(|key| inline_entry(a, c, &region, &result, inputs, &guards).map(|entry| (key, entry))) { inline_store(definition.body.body.form().id, key, entry); }
    }
    let geometry = match &result {
        Value::Deferred(ty, _) if flow_syntax::ty::is_geometry(ty) => true,
        Value::Residual(id) => get_res(a, *id).is_ok_and(|residual| flow_syntax::ty::is_geometry(residual.term.ty())),
        _ => false,
    };
    if a.time.is_some() || c.data || !geometry { return Ok(result); }
    let mut slot_values = Vec::new();
    for slot in &definition.slots {
        let mut ids = Vec::new();
        for (_, value) in args.iter().filter(|(name, _)| name == &slot.name) { geometry_ids(value, &mut ids); }
        slot_values.push((slot.name.clone(), Value::List(Arc::new(ids.into_iter().map(Value::Int).collect()))));
    }
    let mut marker_args = vec![
        ("value".into(), result),
        ("kind".into(), text(&definition.kind.qualified)),
        ("parameters".into(), Value::Record(parameters)),
        ("slots".into(), Value::Record(slot_values)),
    ];
    // its deferred checks, oldest first: test, fails (the test's failing value) and error (the diagnostic as `code: message`, which the
    // host's check_guards decodes; OCaml marshals it)
    if !guards.is_empty() {
        marker_args.push(("guards".into(), Value::List(Arc::new(guards.into_iter().map(|(test, fails, d)| Value::Record(vec![
            ("test".into(), test), ("fails".into(), Value::Bool(fails)), ("error".into(), text(&format!("{}: {}", d.code, d.message))),
        ])).collect()))));
    }
    marker_args.extend(args.iter().filter(|(_, value)| function_value(value)).cloned());
    push_node(a, c, authored, flow_syntax::ty::geometry(), "$card".into(), marker_args)
}
// An inlined body's fresh evaluation: its inputs bound, its validations checked (one of a live argument deferred to the card's cook while
// a.guards gathers), its body evaluated. Ok(None) when a validation reads a live argument it cannot defer (OCaml's Needs_t): the plain node.
fn inline_body(a: &mut Arena, context: &ExecCtx, definition: &W::NodeDefinition, parameters: &[(String, Value)], args: &[(String, Value)], body_nodes: &mut usize, body_authored: &mut usize) -> Result<Option<Result<Value, Diagnostic>>, Diagnostic> {
    let context = context.clone();
    let mut env = Env::empty();
    for (name, ty, default) in &definition.body.inputs {
        let mut value = if definition.slots.iter().any(|slot| slot.rest && slot.name == *name) {
            let mut flattened = Vec::new();
            for (_, value) in args.iter().filter(|(arg_name, _)| arg_name == name) {
                match value {
                    Value::List(items) => flattened.extend(items.iter().filter(|item| !matches!(item, Value::No_geo)).cloned()),
                    Value::No_geo => (),
                    value => flattened.push(value.clone()),
                }
            }
            Value::List(Arc::new(flattened))
        } else if let Some(value) = parameters.iter().find(|(arg_name, _)| arg_name == name).or_else(|| args.iter().find(|(arg_name, _)| arg_name == name)).map(|(_, value)| value.clone()) {
            value
        } else if let Some(default) = default {
            eval_named(a, &context, name, &env, default)?
        } else {
            Value::No_geo
        };
        value = coerce(ty, &value)?;
        env.bind(intern(a, name), value);
    }
    let validation_nodes = (a.nodes.len(), a.authored.len());
    for (predicate, code, message) in &definition.validations {
        // a validation of a live argument is checked as the card cooks, refusing as here; so are the checks its own if/cond deferred
        let before = a.guards.as_ref().map_or(0, Vec::len);
        let refuse_later = |a: &mut Arena| if let Some(guards) = a.guards.as_mut() { for guard in &mut guards[before..] { guard.2 = refusal(&guard.2.code, &guard.2.message); } };
        let value = match eval_at(a, &sub(&context, "validate".into()), &env, predicate) { Ok(value) => value, Err(e) => return Err(refusal(&e.code, &e.message)) };
        if matches!(value, Value::Residual(_)) && a.guards.is_some() && guardable(a, &value) {
            refuse_later(a);
            add_guard(a, value, false, refusal(code, message));
            continue;
        }
        match concrete(a, value) {
            Ok(Value::Bool(true)) => refuse_later(a),
            Ok(Value::Bool(false)) => return Err(refusal(code, message)),
            Err(d) if d.code == "$NEEDS_TIME" => {
                a.nodes.truncate(validation_nodes.0);
                a.authored.truncate(validation_nodes.1);
                return Ok(None);
            }
            Err(e) => return Err(refusal(&e.code, &e.message)),
            Ok(_) => return Err(error("E_TYPE", "A node validation must return bool.")),
        }
    }
    *body_nodes = a.nodes.len();
    *body_authored = a.authored.len();
    let evaluated = eval_at(a, &context, &env, &definition.body.body);
    Ok(Some(evaluated))
}
// ---- temporary profiler (perf4) ----
pub static PROFILE_ON: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
struct ProfFrame { label: String, t0: std::time::Instant, a0: u64, child_ns: u64, child_allocs: u64 }
#[derive(Default)] struct ProfTable { stack: Vec<ProfFrame>, table: HashMap<String,(u64,u64,u64)> }
thread_local! { static PROF: RefCell<ProfTable> = RefCell::new(ProfTable::default()); }
fn prof_label(t: &Term) -> String {
    match t.node {
        TermNode::Op(id) => format!("Op {}", t.name(t.arena().op(id).op)),
        TermNode::Call(id) => format!("Call {}", t.name(t.arena().call(id).kind)),
        TermNode::Hof(id) => format!("Hof {:?}", t.arena().hof(id).kind),
        TermNode::Loop(id) => format!("Loop {:?}", t.arena().loop_node(id).kind),
        TermNode::Lit(_) => "Lit".into(), TermNode::Text(_) => "Text".into(), TermNode::Nil => "Nil".into(), TermNode::Time => "Time".into(),
        TermNode::Vec(_) => "Vec".into(), TermNode::RefBinding(_) => "Ref".into(), TermNode::CallFn(_) => "CallFn".into(), TermNode::FnRef(_) => "FnRef".into(),
        TermNode::GraphRef(_) => "GraphRef".into(), TermNode::Let(_) => "Let".into(), TermNode::State(_) => "State".into(),
        TermNode::If(_) => "If".into(), TermNode::Cond(_) => "Cond".into(), TermNode::Case(_) => "Case".into(), TermNode::Fn(_) => "Fn".into(),
        TermNode::ListLit(_) => "ListLit".into(), TermNode::Record(_) => "Record".into(), TermNode::Get(_) => "Get".into(), TermNode::Assoc(_) => "Assoc".into(),
        TermNode::Str(_) => "Str".into(), TermNode::ListOp(_) => "ListOp".into(), TermNode::Bypass(_) => "Bypass".into(), TermNode::Expanded(_) => "Expanded".into(),
        _ => "Other".into(),
    }
}
fn prof_enter(t: &Term) {
    PROF.with(|p| p.borrow_mut().stack.push(ProfFrame { label: prof_label(t), t0: std::time::Instant::now(), a0: mem_sys::alloc_count(), child_ns: 0, child_allocs: 0 }));
}
fn prof_exit() {
    PROF.with(|p| { let mut p = p.borrow_mut(); let f = p.stack.pop().unwrap();
        let ns = f.t0.elapsed().as_nanos() as u64; let al = mem_sys::alloc_count() - f.a0;
        let e = p.table.entry(f.label).or_default(); e.0 += 1; e.1 += ns - f.child_ns; e.2 += al - f.child_allocs;
        if let Some(parent) = p.stack.last_mut() { parent.child_ns += ns; parent.child_allocs += al; } });
}
pub fn profile_report() -> String {
    PROF.with(|p| { let p = p.borrow(); let mut rows: Vec<_> = p.table.iter().collect(); rows.sort_by_key(|(_, v)| std::cmp::Reverse(v.1));
        rows.iter().take(25).map(|(k, v)| format!("  {:<28} calls {:>9} self {:>8.1} ms  allocs {:>9}", k, v.0, v.1 as f64 / 1e6, v.2)).collect::<Vec<_>>().join("\n") })
}
pub fn profile_reset() { PROF.with(|p| p.borrow_mut().table.clear()); }
fn eval_raw(a: &mut Arena, c: &ExecCtx, env: &Env, t: &Term) -> Result<Value, Diagnostic> {
    if !PROFILE_ON.load(Ordering::Relaxed) { return eval_raw_body(a, c, env, t); }
    prof_enter(t); let r = eval_raw_body(a, c, env, t); prof_exit(); r
}

fn large_constant_array(name: &str, values: &[(String, Value)]) -> bool {
    matches!(name, "array/range" | "array/int" | "array/float" | "array/vec3" | "array/vec4")
        && values.iter().any(|(k, v)| k == "count" && match v { Value::Int(n) => *n > MAX_ITERATIONS as i64, Value::Float(n) => *n > MAX_ITERATIONS as f64, _ => false })
}
fn defer_constant_array(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term, name: &str, values: &[(String, Value)]) -> Result<Option<Value>, Diagnostic> {
    if a.time.is_some() || !large_constant_array(name, values) || values.iter().any(|(_, v)| is_live_value(v)) { return Ok(None); }
    let Some(defer) = current_residual_handlers().and_then(|h| h.defer) else { return Ok(None) };
    let workspace = a.workspace.clone();
    let (Some(actual), Some(builtin)) = (find_op(a, &workspace, name, &c.context), op::find_ref(&[], name, &c.context)) else { return Ok(None) };
    if actual.live || !Arc::ptr_eq(&actual.body, &builtin.body) || !Arc::ptr_eq(&actual.check, &builtin.check) { return Ok(None); }
    if name == "array/range" {
        (actual.check)(values)?;
        op::array_count(None, name, &values.iter().find(|(k, _)| k == "count").unwrap().1)?;
    } else {
        let Value::Source(_, _, fields) = op::constant_array_descriptor(name, values)? else { unreachable!() };
        if !matches!(fields.first(), Some((_, Value::Int(n))) if *n > MAX_ITERATIONS as i64) { return Ok(None); }
    }
    let residual = make_residual(a, term, env, c, false, None);
    Ok(call_lent(a, residual, &|id| defer(id).then_some(Value::Residual(id))))
}
pub static EVAL_STEPS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
fn eval_raw_body(a: &mut Arena, c: &ExecCtx, env: &Env, t: &Term) -> Result<Value, Diagnostic> {
    EVAL_STEPS.fetch_add(1, Ordering::Relaxed);
    a.step_count += 1;
    if a.step_count % PACKED_BLOCK == 0 {
        if a.interrupt.as_ref().is_some_and(|stop| stop()) { return Err(error("cancelled", "Evaluation interrupted.")); }
    }
    if a.step_count > a.budget { return Err(errorf("E_EVAL_BUDGET", format!("Evaluation budget exceeded ({} steps). Reduce iteration counts.", a.budget))); }
    if c.depth > MAX_DEPTH { return eval_error("E_DEPTH", "Call depth exceeds 64."); }
    if a.time.is_some() && kernel_eligible(t) && current_residual_handlers().is_some_and(|handler| handler.kernel.is_some()) {
        let residual = make_residual(a, t, env, c, false, a.time.clone());
        if let Some(value) = kernel_value(a, residual, t.ty()) { return Ok(value); }
    }
    let observe = kernel_eligible(t);
    if a.time.is_none() && observe && current_residual_handlers().is_some() {
        let id = make_residual(a, t, env, c, false, None);
        a.observed.push(id);
        ACTIVE_OBSERVED_RESIDUALS.with(|observed| { if let Some(queue) = observed.borrow_mut().last_mut() { queue.push(id); } });
    }
    let arena = t.arena();
    match t.node {
        TermNode::Lit(id) => match arena.literal(id) {
            Literal::Int(n) => Ok(Value::Int(n)), Literal::Float(n) => Ok(Value::Float(n)), Literal::Bool(v) => Ok(Value::Bool(v)),
            _ => eval_error("E_TYPE", "Unexpected literal."),
        },
        TermNode::Text(id) => Ok(Value::Text(arena.bytes(id).to_vec())),
        TermNode::Nil => Ok(Value::No_geo),
        TermNode::Time => if let Some(frame) = &a.time { Ok(Value::Float(frame.t)) } else { need_time() },
        TermNode::Vec(range) => (|| -> Result<Value, Diagnostic> {
            let terms = arena.term_ids(range);
            let mut values = Vec::with_capacity(terms.len());
            for (i, term) in terms.iter().enumerate() {
                let context = sub(c, Seg::Index("", i));
                values.push(num(&eval_concrete(a, &context, env, &t.child(*term))?)?);
            }
            match values.as_slice() {
                [x, y] => Ok(Value::Vec2(*x, *y)),
                [x, y, z] => Ok(Value::Vec3(*x, *y, *z)),
                [x, y, z, w] => Ok(Value::Vec4(*x, *y, *z, *w)),
                _ => Err(errorf("E_VECTOR", format!("A vector has 2, 3 or 4 components; this one has {}.", values.len()))),
            }
        })(),
        TermNode::RefBinding(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.ref_binding(id);
            let name = t.name(node.name);
            let fields = arena.name_ids(node.fields);
            if fields.is_empty() && name.starts_with("$elem:") { return a.elems.get(name).cloned().ok_or_else(|| error("E_ELEMENT", "A geometry element is not bound.")); }
            let mut value = env.get(name).ok_or_else(|| errorf("E_UNBOUND", format!("{} is not bound.", name)))?;
            let mut walked: Vec<&str> = Vec::new();
            for field_name in fields {
                let field_name = t.name(*field_name);
                value = field(concrete(a, value)?, field_name, || {
                    let mut path = String::from(name);
                    for prefix in &walked {
                        path.push('.');
                        path.push_str(prefix);
                    }
                    path
                })?;
                walked.push(field_name);
            }
            Ok(value)
        })(),
        TermNode::Call(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.call(id);
            let kind = t.name(node.kind);
            let mut args = named_args(a, c, env, t, arena.fields(node.args))?;
            if kind == "sop/merge" {
                let mut flattened = Vec::new();
                for (name, value) in args {
                    match concrete(a, value)? {
                        Value::List(values) => flattened.extend(values.iter().cloned().filter(|v| !matches!(v, Value::No_geo)).map(|v| (name.clone(), v))),
                        Value::No_geo => (),
                        value => flattened.push((name, value)),
                    }
                }
                args = flattened;
            }
            match inline_policy_for(a, kind) {
                Some(policy) => inline_node(a, c, t.form().id, &policy, &args, t),
                None => catalog_call(a, c, t.form().id, kind.to_owned(), node.ctx, args),
            }
        })(),
        TermNode::Op(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.op(id);
            let name = t.name(node.op);
            let args = arena.fields(node.args);
            let skip = arena.skip(node.skip);
            if (name == "and" || name == "or") && args.len() == 2 {
                let first_name = t.name(args[0].name);
                let first = eval_named(a, c, first_name, env, &t.child(args[0].term))?;
                if let Value::Bool(value) = &first { if *value == (name == "or") { return Ok(Value::Bool(*value)); } }
                let second_name = t.name(args[1].name);
                let second = eval_named(a, c, second_name, env, &t.child(args[1].term))?;
                return apply_op(a, c, t.form().id, name, vec![(first_name.to_owned(), first), (second_name.to_owned(), second)]);
            }
            if name == "live?" && args.len() == 1 {
                return Ok(Value::Bool(is_live_value(&eval_named(a, c, t.name(args[0].name), env, &t.child(args[0].term))?)));
            }
            let values = if name == "scene/merge" && !skip.is_empty() {
                let mut counts = HashMap::<String, usize>::new();
                let mut kept = Vec::new();
                for (index, arg) in args.iter().enumerate() {
                    let arg_name = t.name(arg.name);
                    let mut tuple = c.iter.iter().map(|index| usize::try_from(*index).unwrap_or(usize::MAX)).collect::<Vec<_>>();
                    tuple.push(index);
                    if skip.iter().any(|indices| arena.indices(*indices) == tuple) { continue; }
                    let count = counts.entry(arg_name.to_owned()).or_default();
                    let route = if *count == 0 { arg_name.to_owned() } else { format!("{}#{}", arg_name, *count) };
                    *count += 1;
                    kept.push((arg_name.to_owned(), route, arg.term));
                }
                let mut values = kept.iter().map(|(name, _, _)| (name.clone(), Value::No_geo)).collect::<Vec<_>>();
                for index in 0..kept.len() { values[index].1 = eval_named(a, c, &kept[index].1, env, &t.child(kept[index].2))?; }
                values
            } else {
                // the argument list of this term, reused across its calls (its names are spelled once per arena)
                let key = t.identity();
                let mut values = a.arg_pool.remove(&key).unwrap_or_else(|| args.iter().map(|arg| (t.name(arg.name).to_owned(), Value::No_geo)).collect());
                for (index, arg) in args.iter().enumerate() {
                    let name = t.name(arg.name);
                    let term = t.child(arg.term);
                    let count = args[..index].iter().filter(|other| other.name == arg.name).count();
                    values[index].1 = if count == 0 { eval_named(a, c, name, env, &term)? } else { eval_sub(a, c, format!("{}#{}", name, count).into(), env, &term)? };
                }
                let result = match defer_constant_array(a, c, env, t, name, &values)? {
                    Some(value) => Ok(value),
                    None => apply_op_in(a, c, t.form().id, name, &mut values),
                };
                if values.len() == args.len() { for entry in values.iter_mut() { entry.1 = Value::No_geo; } a.arg_pool.insert(key, values); }
                return result;
            };
            apply_op(a, c, t.form().id, name, values)
        })(),
        TermNode::CallFn(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.call_fn(id);
            let args = arena.term_ids(node.args);
            let fn_name = t.name(node.name);
            let body = node.body.map(|body| t.child(body));
            let mut values = vec![Value::No_geo; args.len()];
            for index in 0..args.len() { values[index] = eval_sub(a, c, Seg::Index("a", index), env, &t.child(args[index]))?; }
            match env.get(fn_name) {
                Some(Value::Fn(id)) => call_fn(a, c, id, &values, body.as_ref()),
                _ => apply_def(a, c, fn_name, &values, body.as_ref()),
            }
        })(),
        TermNode::FnRef(name) => Ok(Value::Fn(closure_id(a, Function::Named(NamedData { name: t.name(name).to_owned(), calls: 0 })))),
        TermNode::GraphRef(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.graph_ref(id);
            let values = named_args(a, c, env, t, arena.fields(node.inputs))?;
            graph_value(a, c, t.name(node.graph), &values, values.is_empty())
        })(),
        TermNode::Let(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.let_node(id);
            let mut local = env.clone();
            for binding in arena.bindings(node.bindings) {
                let term = t.child(binding.term);
                let mut value = eval_at(a, c, &local, &term)?;
                if matches!(value, Value::Residual(_)) && diagnostic_effect(&term) { value = concrete(a, value)?; }
                if matches!(arena.pattern(binding.pattern), PatternNode::Name(_)) { bind_packed(a, c, &mut local, arena, binding.pattern, value, None)?; }
                else {
                    // a destructuring pattern records its parts beside the binding (the path without its last segment)
                    let prefix: Path = term_base(a, &term).map(|p| p[..p.len().saturating_sub(1)].to_vec()).unwrap_or_default();
                    let mk = move |n: &str| { let mut q = prefix.clone(); q.push(n.to_owned()); q };
                    bind_packed(a, c, &mut local, arena, binding.pattern, value, Some(&mk))?;
                }
            }
            eval_at(a, c, &local, &t.child(node.body))
        })(),
        TermNode::Loop(id) => eval_loop(a, c, env, t, arena.loop_node(id)),
        TermNode::State(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.state(id);
            let zone = packed_base(a, t, node.zone);
            let init = t.child(node.init);
            let step = t.child(node.step);
            if c.geometry { return eval_error("E_STATE_ELEMENT", "Frame state belongs to the workspace value lane; bind it outside a loop over cooked geometry."); }
            let previous = if a.time.is_none() {
                Value::Residual(make_residual(a, t, env, c, true, None))
            } else { state_previous(a, c, env, &zone, &init)? };
            let mut local = env.clone();
            record_packed(a, c, &zone, arena, node.binder, &previous)?;
            bind_packed(a, c, &mut local, arena, node.binder, previous, None)?;
            if a.time.is_none() { let _ = eval_at(a, c, &local, &step)?; need_time() }
            else {
                let (state, _) = state_plane(a, c)?;
                let key = state_key(c, &zone);
                let existing = { let data = state.inner.lock().unwrap(); if c.host { data.host.as_ref().and_then(|p| p.next.get(&key).cloned()) } else { data.next.get(&key).cloned() } };
                if let Some(v) = existing { return Ok(Device::thaw(&v)); }
                let value = coerce(t.ty(), &eval_at(a, c, &local, &step)?)?;
                state_write(a, c, &zone, value)
            }
        })(),
        TermNode::If(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.if_node(id);
            let (test, yes, no) = (&t.child(node.test), &t.child(node.yes), &t.child(node.no));
            let test = eval_sub(a, c, "if".into(), env, test)?;
            // a live test whose one arm fails statically: the other arm, the test checked as the card cooks
            if guarding(a, &test) {
                if let Some(d) = arm_error(a, &sub(c, "then".into()), env, yes)? {
                    add_guard(a, test, true, d);
                    return eval_sub(a, c, "else".into(), env, no);
                }
                if let Some(d) = arm_error(a, &sub(c, "else".into()), env, no)? {
                    add_guard(a, test, false, d);
                    return eval_sub(a, c, "then".into(), env, yes);
                }
                return need_time();
            }
            if truth(&concrete(a, test)?)? { eval_sub(a, c, "then".into(), env, yes) } else { eval_sub(a, c, "else".into(), env, no) }
        })(),
        TermNode::Cond(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.cond(id);
            for (i, arm) in arena.arms(node.arms).iter().enumerate() {
                let body = &t.child(arm.body);
                let context = sub(c, Seg::Index("test", i));
                let test = eval_at(a, &context, env, &t.child(arm.test))?;
                if guarding(a, &test) {
                    match arm_error(a, &sub(c, Seg::Index("arm", i)), env, body)? {
                        Some(d) => { add_guard(a, test, true, d); continue; }
                        None => return need_time(),
                    }
                }
                if truth(&concrete(a, test)?)? { return eval_sub(a, c, Seg::Index("arm", i), env, body); }
            }
            eval_sub(a, c, "else".into(), env, &t.child(node.default))
        })(),
        TermNode::Case(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.case(id);
            let value = eval_concrete(a, &sub(c, "case".into()), env, &t.child(node.value))?;
            for (i, arm) in arena.case_arms(node.arms).iter().enumerate() {
                let body = &t.child(arm.body);
                let literal = literal_case(&arena.syntax().handle(arm.form))?;
                let matches = if matches!(&value, Value::Bool(_)) || matches!(&literal, Value::Bool(_)) {
                    let truthy = |value: &Value| match value { Value::Bool(value) => Ok(*value), value => num(value).map(|number| number != 0.0) };
                    truthy(&value)? == truthy(&literal)?
                } else {
                    match (&value, &literal) {
                        (Value::Text(a), Value::Text(b)) => a == b,
                        (Value::Int(_) | Value::Float(_), Value::Int(_) | Value::Float(_)) => num(&value)? == num(&literal)?,
                        _ => false,
                    }
                };
                if matches { return eval_sub(a, c, Seg::Index("arm", i), env, body); }
            }
            eval_sub(a, c, "else".into(), env, &t.child(node.default))
        })(),
        TermNode::Fn(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.function(id);
            let params = (t.clone(), node.params);
            let zone = packed_path(t, node.zone);
            let body = &t.child(node.body);
            if let Some(original) = node.capture {
                let Value::Fn(id) = eval_at(a, c, env, &t.child(original))? else { return eval_error("E_TYPE", "A function port requires a function."); };
                match get_fn(a, id)? {
                    Function::Closure(closure) => { let mut closure = unshare(closure); { closure.params = params; closure.body = shared_term(a, body); replace_fn(a, id, Function::Closure(Arc::new(closure))); Ok(Value::Fn(id)) } }
                    Function::Named(named) => { let body = shared_term(a, body); Ok(Value::Fn(closure_id(a, Function::Closure(Arc::new(ClosureData { params, replayable: false, body, env: Arc::new(BTreeMap::new()), zone, calls: named.calls, at: c.clone() }))))) },
                }
            } else {
                { let (shared, captured) = (shared_term(a, body), capture_in(a, body, env)); Ok(Value::Fn(closure_id(a, Function::Closure(Arc::new(ClosureData { params, replayable: true, body: shared, env: Arc::new(captured), zone, calls: 0, at: c.clone() }))))) }
            }
        })(),
        TermNode::Hof(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.hof(id);
            let terms = arena.term_ids(node.args);
            if terms.is_empty() { return eval_error("E_ARITY", "A higher-order form takes a function."); }
            eval_hof(a, c, env, t, &node.kind, &t.child(terms[0]), &terms[1..])
        })(),
        TermNode::ListLit(range) => (|| -> Result<Value, Diagnostic> {
            let terms = arena.term_ids(range);
            let mut values = Vec::with_capacity(terms.len());
            for (i, term) in terms.iter().enumerate() { values.push(eval_sub(a, c, Seg::Index("", i), env, &t.child(*term))?); }
            Ok(join_list(values))
        })(),
        TermNode::Record(range) => (|| -> Result<Value, Diagnostic> {
            let fields = arena.fields(range);
            let mut values = Vec::with_capacity(fields.len());
            for field in fields { let name = t.name(field.name); values.push((name.to_owned(), eval_named(a, c, name, env, &t.child(field.term))?)); }
            Ok(Value::Record(values))
        })(),
        TermNode::Get(id) => { let node = arena.get(id); field(eval_concrete(a, c, env, &t.child(node.value))?, t.name(node.name), || "value".into()) },
        TermNode::Assoc(id) => (|| -> Result<Value, Diagnostic> {
            let node = arena.assoc(id);
            let Value::Record(mut fields) = eval_concrete(a, c, env, &t.child(node.value))? else { return eval_error("E_TYPE", "assoc updates a record."); };
            for update in arena.fields(node.fields) {
                let name = t.name(update.name);
                let value = eval_named(a, c, name, env, &t.child(update.term))?;
                if let Some((_, old)) = fields.iter().find(|(key, _)| key == name) {
                    let value = coerce_like(a, old, &value)?;
                    if let Some(slot) = fields.iter_mut().find(|(key, _)| key == name) { slot.1 = value; }
                } else { fields.push((name.to_owned(), value)); }
            }
            Ok(Value::Record(fields))
        })(),
        TermNode::Str(range) => (|| -> Result<Value, Diagnostic> {
            let terms = arena.term_ids(range);
            let mut bytes = Vec::new();
            for (i, term) in terms.iter().enumerate() {
                let context = sub(c, Seg::Index("", i));
                let value = eval_concrete(a, &context, env, &t.child(*term))?;
                // OCaml shows through `concrete`: captured fields of a record are forced first.
                let value = concrete_data(a, value)?;
                bytes.extend(value::show_with(&|value| value.clone(), &value));
            }
            Ok(Value::Text(bytes))
        })(),
        TermNode::ListOp(id) => (|| -> Result<Value, Diagnostic> {
            let terms = arena.term_ids(arena.list_op(id).args);
            let mut values = Vec::new();
            for (i, term) in terms.iter().enumerate() {
                let context = sub(c, Seg::Index("", i));
                match eval_concrete(a, &context, env, &t.child(*term))? {
                    Value::List(list) => values.extend(list.iter().cloned()),
                    _ => return eval_error("E_TYPE", "Expected a list."),
                }
            }
            if values.len() > MAX_ITERATIONS { return eval_error("E_ITER_BOUND", "concat exceeds 4,096 elements."); }
            Ok(join_list(values))
        })(),
        TermNode::Bypass(call) => match W::bypass_input(&t.child(call)) { Some(input) => coerce(t.ty(), &eval_sub(a, c, "bypass".into(), env, &input)?), None => Err(error("E_BYPASS", "Nothing to pass through.")) },
        TermNode::Expanded(id) => eval_at(a, c, env, &t.child(arena.expanded(id).body)),
        TermNode::WithError(id) => { let node = arena.pair(id); eval_with_error(a, c, env, &t.child(node.first), &t.child(node.second)) },
        TermNode::ErrorRows(id) => { let node = arena.error_rows(id); eval_error_rows(a, c, env, &t.child(node.rows), node.policy, &t.child(node.mapper)) },
    }
}
fn diagnostic_effect(term: &Term) -> bool {
    matches!(term.node, TermNode::WithError(_) | TermNode::ErrorRows(_)) || children(term).any(|child| diagnostic_effect(&child))
}
fn diagnostic_payload(diagnostic: &Diagnostic) -> Value {
    Value::Record(vec![
        ("code".into(), Value::Text(diagnostic.code.as_bytes().to_vec())),
        ("message".into(), Value::Text(diagnostic.message.as_bytes().to_vec())),
        ("cause".into(), Value::List(Arc::new(diagnostic.cause.iter().map(|cause| Value::Text(cause.clone())).collect()))),
    ])
}
fn is_cancelled(diagnostic: &Diagnostic) -> bool { diagnostic.code == "cancelled" || diagnostic.code == "E_CANCELLED" }
fn resolve_source(a: &mut Arena, source: &Value) -> Result<Value, Diagnostic> {
    let resolve = a.resolve.clone().ok_or_else(|| error("E_DATA_SOURCE", "This data source needs its host's cooked data."))?;
    let value = lend(a, || resolve(source))?;
    check_value_nesting(&value)?;
    if matches!(value, Value::Source(..) | Value::Struct(_, Ty::Array(_), _)) { return eval_error("E_DATA_SOURCE", "The host returned an unresolved data source."); }
    if value::ty_of(source) != Ty::Any && value::ty_of(source) != value::ty_of(&value) { return eval_error("E_DATA_SOURCE", "The host returned a data source of the wrong type."); }
    value::validate(&value)?;
    Ok(value)
}
fn concrete_data(a: &mut Arena, value: Value) -> Result<Value, Diagnostic> {
    let _nesting = EvalNesting::enter()?;
    match value {
        Value::List(values) => Ok(Value::List(Arc::new(values.iter().cloned().map(|value| concrete_data(a, value)).collect::<Result<Vec<_>, _>>()?))),
        Value::Record(fields) => Ok(Value::Record(fields.into_iter().map(|(name, value)| Ok((name, concrete_data(a, value)?))).collect::<Result<Vec<_>, Diagnostic>>()?)),
        Value::Struct(name, ty, fields) => Ok(Value::Struct(name, ty, fields.into_iter().map(|(name, value)| Ok((name, concrete_data(a, value)?))).collect::<Result<Vec<_>, Diagnostic>>()?)),
        value => concrete(a, value),
    }
}
fn eval_with_error(a: &mut Arena, c: &ExecCtx, env: &Env, protected: &Term, mapper: &Term) -> Result<Value, Diagnostic> {
    if a.time.is_none() { return need_time(); }
    // OCaml runs both inside Value.with_error: an error raised while evaluating the protected term or the mapper is mapped / inherits the envelope
    let guarded = (|| -> Result<Value, Diagnostic> {
        let protected = eval_sub(a, c, "0".into(), env, protected)?;
        match concrete_data(a, protected)? {
            Value::Deferred(ty, id) if flow_syntax::ty::is_cooked(&ty) => {
                let source = Value::Source("$core/complete".into(), ty.clone(), vec![("value".into(), Value::Deferred(ty, id))]);
                resolve_source(a, &source)
            }
            value => Ok(value),
        }
    })();
    let diagnostic = match guarded {
        Ok(value) => return Ok(value),
        Err(diagnostic) if is_cancelled(&diagnostic) => return Err(diagnostic),
        Err(diagnostic) => diagnostic,
    };
    let payload = diagnostic_payload(&diagnostic);
    let mapped = (|| -> Result<Value, Diagnostic> {
        match eval_concrete(a, &sub(c, "1".into()), env, mapper)? {
            Value::Fn(function) => {
                let mapped = call_fn(a, c, function, &[payload], None)?;
                concrete_data(a, mapped)
            }
            _ => eval_error("E_DIAGNOSTIC_MAP", "An error mapper is a function."),
        }
    })();
    value::with_error(&|| Err(diagnostic.clone()), &|_| mapped.clone())
}
fn eval_error_rows(a: &mut Arena, c: &ExecCtx, env: &Env, rows: &Term, policy: W::DiagnosticPolicy, mapper: &Term) -> Result<Value, Diagnostic> {
    let policy_name = match policy { W::DiagnosticPolicy::First => "first", W::DiagnosticPolicy::RangeLast => "range-last" };
    let rows = if a.time.is_none() {
        if c.card.is_none() || policy != W::DiagnosticPolicy::First { return need_time(); }
        match eval_sub(a, c, "0".into(), env, rows)? {
            Value::Int_array(values) => Value::Int_array(values),
            Value::Residual(id) => {
                let residual = get_res(a, id)?;
                let term = match residual.term.node {
                    TermNode::Op(id) if residual.term.name(residual.term.arena().op(id).op) == "exact" && residual.term.arena().fields(residual.term.arena().op(id).args).len() == 1 => Arc::new(residual.term.child(residual.term.arena().fields(residual.term.arena().op(id).args)[0].term)),
                    _ => residual.term,
                };
                let bindings = Env::of(residual.env);
                let needed = residual_id(a, ResidualData { env: Arc::new(capture_env(&term, &bindings)), term, ctx: residual.ctx, previous: false, live: None });
                // OCaml answers Bulk_needed with the host's bulk callback: a check the card needs now (a deferred mask) is asked of it
                match current_residual_handlers().and_then(|handlers| handlers.bulk).and_then(|bulk| call_lent(a, needed, &|id| bulk(id))) {
                    Some(Value::Int_array(values)) => Value::Int_array(values),
                    _ => return need_time(),
                }
            }
            _ => return need_time(),
        }
    } else {
        let value = eval_sub(a, c, "0".into(), env, rows)?;
        concrete_data(a, value)?
    };
    let Some(first) = value::error_row_index(&rows, None, None)? else { return Ok(Value::No_geo); };
    let index = if a.resolve.is_some() {
        match resolve_source(a, &Value::Source("$core/diagnostic_index".into(), Ty::Int, vec![("rows".into(), rows.clone()), ("policy".into(), text(policy_name))]))? {
            Value::Int(index) => index,
            _ => return eval_error("E_DIAGNOSTIC_CONTEXT", "Host diagnosis returns one integer row."),
        }
    } else if policy == W::DiagnosticPolicy::First {
        first
    } else {
        return eval_error("E_DIAGNOSTIC_CONTEXT", "Range-last diagnosis needs its native host.");
    };
    match &rows {
        Value::Int_array(values) if index >= 0 && (index as u128) < values.len() as u128 && values[index as usize] == 1 => (),
        _ => return eval_error("E_DIAGNOSTIC_CONTEXT", "Host diagnosis returned a non-failing row."),
    }
    let mapped = (|| -> Result<Value, Diagnostic> {
        match eval_concrete(a, &sub(c, "1".into()), env, mapper)? {
            Value::Fn(function) => {
                let mapped = call_fn(a, c, function, &[Value::Int(index)], None)?;
                concrete_data(a, mapped)
            }
            _ => eval_error("E_DIAGNOSTIC_MAP", "A row mapper is a function."),
        }
    })();
    value::with_error(&|| Err(error("E_DIAGNOSTIC_ROW", "A diagnostic row failed.")), &|_| mapped.clone())
}

fn data_type(ty: &Ty) -> bool {
    match ty {
        Ty::Float | Ty::Int | Ty::Bool | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text | Ty::Color => true,
        Ty::List(element) | Ty::Array(element) => data_type(element),
        Ty::Record(fields) => !fields.is_empty() && fields.iter().all(|(_, ty)| data_type(ty)),
        Ty::Any | Ty::Named(_) | Ty::Fn(_) => false,
    }
}
// An operator found by name, remembered per arena by the address of the name (checked against the name and context it was found for).
#[derive(Clone, Copy)]
enum OpRef { Extra(usize), Builtin(usize) }
fn find_op<'w>(a: &mut Arena, workspace: &'w Workspace, name: &str, context: &Context) -> Option<&'w op::Op> {
    let resolve = |found: OpRef| -> &'w op::Op { match found { OpRef::Extra(i) => &workspace.ops[i], OpRef::Builtin(i) => &op::all()[i] } };
    if let Some((within, found)) = a.ops_seen.get(name) { if *within == context.as_str() { return Some(resolve(*found)); } }
    let found = op::find_indexed(&workspace.ops, name, context)?;
    let index_in = |table: &[op::Op]| { let start = table.as_ptr() as usize; let at = found as *const op::Op as usize; (at >= start && at < start + table.len() * std::mem::size_of::<op::Op>()).then(|| (at - start) / std::mem::size_of::<op::Op>()) };
    let slot = index_in(&workspace.ops).map(OpRef::Extra).or_else(|| index_in(op::all()).map(OpRef::Builtin))?;
    a.ops_seen.insert(name.to_owned(), (context.as_str(), slot));
    Some(resolve(slot))
}
fn apply_op(a: &mut Arena, c: &ExecCtx, authored: i64, name: &str, mut values: Vec<(String, Value)>) -> Result<Value, Diagnostic> { apply_op_in(a, c, authored, name, &mut values) }
fn apply_op_in(a: &mut Arena, c: &ExecCtx, authored: i64, name: &str, values: &mut Vec<(String, Value)>) -> Result<Value, Diagnostic> {
    let workspace = a.workspace.clone();
    let op = find_op(a, &workspace, name, &c.context).ok_or_else(|| errorf("E_UNKNOWN", format!("Unknown operator {}.", name)))?;
    if let Some(label) = &c.card {
        if W::BODY_CONTEXT_OPS.contains(&name) && !values.iter().any(|(key, _)| key == CONTEXT_LABEL) { values.push((CONTEXT_LABEL.into(), text(label))); }
    }
    if let op::Shape::Struct { splice: true } = &op.shape {
        let mut spliced = Vec::new();
        for (name, value) in std::mem::take(values) {
            match concrete(a, value)? {
                Value::List(items) => spliced.extend(items.iter().cloned().map(|value| (name.clone(), value))),
                value => spliced.push((name, value)),
            }
        }
        *values = spliced;
    }
    if matches!(&op.shape, op::Shape::Scalar) && op.ctx == flow_syntax::context::value() {
        let declared = |name: &str| op.signature.pos.iter().chain(&op.signature.opt).chain(&op.signature.kw).find(|(arg, _)| arg == name).map(|(_, ty)| ty)
            .or_else(|| op.signature.rest.as_ref().filter(|(arg, _)| arg == name).map(|(_, ty)| ty));
        for entry in values.iter_mut() {
            let deep = op.name == "equal?" || declared(&entry.0).is_some_and(|ty| matches!(ty, Ty::List(_) | Ty::Record(_)) && data_type(ty));
            let value = std::mem::replace(&mut entry.1, Value::No_geo);
            entry.1 = if deep { concrete_data(a, value)? } else { concrete(a, value)? };
        }
    }
    (op.check)(values)?;
    if a.time.is_none() && op.live { return need_time(); }
    static AT_ZERO: LazyLock<Frame> = LazyLock::new(|| input::at_time(0.0));
    let live_frame = a.time.clone();
    let frame = live_frame.as_ref().unwrap_or(&AT_ZERO);
    // op::result_type, without spelling the argument list when the type reads only the declared arguments' types
    let ty = if op.out_args.is_none() {
        let named = |key: &str| op.signature.pos.iter().chain(op.signature.opt.iter()).any(|(n, _)| n == key);
        (op.out)(&values.iter().filter(|(name, _)| named(name)).map(|(_, value)| value::ty_of(value)).collect::<Vec<_>>())?
    } else {
        let typed = values.iter().map(|(name, value)| {
            let literal = match value {
                Value::Int(n) => Some(param::Value::Int(*n)),
                Value::Float(n) => Some(param::Value::Float(*n)),
                Value::Bool(b) => Some(param::Value::Bool(*b)),
                Value::Text(bytes) => Some(param::Value::Text(bytes.clone())),
                _ => None,
            };
            (name.clone(), value::ty_of(value), literal)
        }).collect::<Vec<_>>();
        op::result_type(op, &typed)?
    };
    let args: &[(String, Value)] = values;
    let result = {
        let cell = RefCell::new(&mut *a);
        let node = |kind: &str, args: &[(String, Value)]| mk_node(&mut cell.borrow_mut(), c, authored, ty.clone(), kind.to_owned(), args.to_vec());
        (op.body)(frame, &node, args)?
    };
    if a.time.is_none() {
        if matches!(&result, Value::Source(..)) {
            let hook = SOURCE_HOOK.read().unwrap().clone();
            let nodes = a.nodes.clone();
            let node = |id| nodes.iter().find(|node| node.id == id).cloned();
            return Ok(lend(a, || hook(&node, &result)).unwrap_or(result));
        }
    }
    Ok(result)
}
fn apply_def(a:&mut Arena,c:&ExecCtx,name:&str,vals:&[Value],body:Option<&Term>)->Result<Value,Diagnostic>{let workspace=a.workspace.clone();let graph=workspace.defs.iter().find(|g|g.name==name).ok_or_else(||errorf("E_UNKNOWN",format!("Unknown function {}.",name)))?;if c.depth>=MAX_DEPTH{return eval_error("E_DEPTH","Call depth exceeds 64.")}let mut cx=c.clone();cx.prefix=Arc::new(site(c));cx.base=Arc::new(vec![format!("def:{}",name)]);cx.route = Route(None);cx.depth+=1;let mut env=Env::empty();for(i,(n,ty,default))in graph.inputs.iter().enumerate(){let v=if let Some(v)=vals.get(i){coerce(ty,v)?}else if let Some(t)=default{eval_at(a,&sub(&cx,n.clone().into()),&env,t)?}else{return Err(errorf("E_ARGS",format!("{} needs :{}.",name,n)))};if a.record&&cx.record{let mut p=(*cx.base).clone();p.push(format!(":{}",n));note(a,&cx,p,v.clone());}env.bind(intern(a,n),v)}eval_at(a,&cx,&env,body.unwrap_or(&graph.body))}
fn call_fn(a: &mut Arena, c: &ExecCtx, id: FnId, values: &[Value], body: Option<&Term>) -> Result<Value, Diagnostic> {
    if let Some(result) = call_compiled(a, c, id, values, body) { return result; }
    call_fn_walked(a, c, id, values, body)
}
fn call_fn_walked(a: &mut Arena, c: &ExecCtx, id: FnId, values: &[Value], body: Option<&Term>) -> Result<Value, Diagnostic> {
    match get_fn(a, id)? {
        Function::Closure(closure) => {
            let params = closure_params(&closure);
            let param_arena = closure.params.0.arena();
            if params.len() != values.len() {
                return Err(errorf("E_ARITY", format!("{} takes {} arguments; got {}.", path_text(&closure.zone), params.len(), values.len())));
            }
            let index = *a.fn_calls.get(&id).unwrap_or(&closure.calls);
            a.fn_calls.insert(id, index + 1);
            let mut context = c.clone();
            context.inst = closure.at.inst;
            context.prefix = closure.at.prefix.clone();
            context.base = base_of(a, &closure.zone);
            context.route = Route(None);
            Arc::make_mut(&mut context.iter).push(index);
            context.depth += 1;
            let mut env = Env::of(closure.env.clone());
            for (parameter, value) in params.iter().zip(values) {
                let value = if let Some(ty) = parameter.ty { coerce(param_arena.ty(ty), value)? } else { value.clone() };
                record_packed(a, &context, &closure.zone, param_arena, parameter.pattern, &value)?;
                bind_packed(a, &context, &mut env, param_arena, parameter.pattern, value, None)?;
            }
            eval_at(a, &context, &env, body.unwrap_or(&closure.body))
        }
        Function::Named(mut named) => {
            let index = *a.fn_calls.get(&id).unwrap_or(&named.calls);
            named.calls = index + 1;
            a.fn_calls.insert(id, index + 1);
            a.functions.insert(id, Function::Named(named.clone()));
            if a.workspace.defs.iter().any(|graph| graph.name == named.name) {
                let mut context = c.clone();
                Arc::make_mut(&mut context.iter).push(index);
                return apply_def(a, &context, &named.name, values, body);
            }
            if let Some(operation) = op::find_indexed(&a.workspace.ops, &named.name, &c.context) {
                let names = operation.signature.pos.iter().chain(&operation.signature.opt).map(|(name, _)| name.clone()).collect::<Vec<_>>();
                let args = values.iter().enumerate().map(|(index, value)| {
                    let name = names.get(index).cloned().or_else(|| operation.signature.rest.as_ref().map(|(name, _)| name.clone())).unwrap_or_else(|| format!("${}", index));
                    (name, value.clone())
                }).collect::<Vec<_>>();
                return apply_op(a, c, 0, &named.name, args);
            }
            let (kind, context, slots) = a.workspace.kind_fns.iter().find(|(name, _)| name == &named.name).map(|(_, kind)| kind.clone()).ok_or_else(|| errorf("E_UNKNOWN", format!("Unknown function {}.", named.name)))?;
            let mut args = Vec::with_capacity(values.len());
            for (index, value) in values.iter().enumerate() {
                let name = slots.get(index).map(|slot| slot.name.clone()).or_else(|| slots.last().filter(|slot| slot.rest).map(|slot| slot.name.clone())).ok_or_else(|| errorf("E_ARITY", format!("{} takes at most {} inputs.", kind, slots.len())))?;
                args.push((name, value.clone()));
            }
            catalog_call(a, c, 0, kind, context, args)
        }
    }
}
fn closure_fits(signature: &flow_syntax::ty::FnSignature, closure: &ClosureData) -> bool {
    signature.params.len() == closure_params(closure).len()
        && signature.params.iter().zip(closure_params(closure)).all(|(expected, actual)| actual.ty.is_some_and(|actual| ty::fits(expected, closure.params.0.arena().ty(actual))))
        && ty::fits(closure.body.ty(), &signature.result)
}
fn graph_input_fits(arena: &mut Arena, expected: &Ty, value: &Value) -> bool {
    match (expected, value) {
        (Ty::Fn(Some(signature)), Value::Fn(id)) if id.1 == arena.id => match arena.functions.get(id) {
            Some(Function::Closure(closure)) => closure_fits(signature, closure),
            _ => ty::fits(&value::ty_of(value), expected),
        },
        // a function a library's arena made, handed to a graph of this one
        (Ty::Fn(Some(signature)), Value::Fn(id)) => match foreign_fn(arena, *id) {
            Some(Function::Closure(closure)) => closure_fits(signature, &closure),
            _ => ty::fits(&value::ty_of(value), expected),
        },
        (Ty::List(element), Value::List(values)) => values.iter().all(|value| graph_input_fits(arena, element, value)),
        _ => ty::fits(&value::ty_of(value), expected),
    }
}
fn value_key(value: &Value) -> String {
    stable_digest(&value::key_of(&|id| id.0, value))
}
fn graph_value(a: &mut Arena, c: &ExecCtx, name: &str, over: &[(String, Value)], record: bool) -> Result<Value, Diagnostic> {
    let workspace = a.workspace.clone();
    let graph = workspace.graphs.iter().find(|graph| graph.name == name).ok_or_else(|| errorf("E_UNKNOWN_GRAPH", format!("Unknown graph reference: {}.", name)))?;
    let mut seen = BTreeSet::new();
    let mut coerced: Vec<(String, Value)> = Vec::with_capacity(over.len());
    for (input, value) in over {
        let (_, ty, _) = graph.inputs.iter().find(|(key, _, _)| key == input).ok_or_else(|| errorf("E_INPUT_UNKNOWN", format!("Graph {} has no input {}.", name, input)))?;
        if !graph_input_fits(a, ty, value) {
            return Err(errorf("E_INPUT_TYPE", format!("Graph input {} of {} needs {}; got {}.", input, name, ty::to_string(ty), ty::to_string(&value::ty_of(value)))));
        }
        coerced.push((input.clone(), coerce(ty, value)?));
        if !seen.insert(input) { return Err(errorf("E_INPUT_DUPLICATE", format!("Graph {} has duplicate input overrides.", name))); }
    }
    let over = coerced.as_slice();
    let mut keys = over.iter().map(|(name, value)| (name.clone(), value_key(value))).collect::<Vec<_>>();
    keys.sort_by(|left, right| left.0.cmp(&right.0));
    let key = format!("{}|{}", name, keys.iter().map(|(name, value)| format!("{}={}", name, value)).collect::<Vec<_>>().join(","));
    if let Some(index) = a.cache.get(&key) { return Ok(a.cells[*index].result.clone()); }
    if a.time.is_some() { return eval_graph_body(a, c, &graph, over, record, -1).map(|(value, _)| value); }
    let instance = a.cells.len() as i64;
    a.cells.push(GraphCell { instance, graph: name.to_owned(), default: over.is_empty(), inputs: Vec::new(), result: Value::No_geo });
    a.cache.insert(key, instance as usize);
    let (value, inputs) = eval_graph_body(a, c, &graph, over, record, instance)?;
    a.cells[instance as usize].inputs = inputs;
    a.cells[instance as usize].result = value.clone();
    Ok(value)
}
fn eval_graph_body(a:&mut Arena,c:&ExecCtx,g:&Graph,over:&[(String,Value)],record:bool,inst:i64)->Result<(Value,Vec<(String,Value)>),Diagnostic>{let mut cx=c.clone();cx.inst=inst;cx.prefix=Arc::new(Vec::new());cx.base=Arc::new(vec![g.name.clone()]);cx.route = Route(None);cx.iter=Arc::new(Vec::new());cx.depth+=1;cx.record=record;cx.context=g.context.clone();cx.host=g.context==flow_syntax::context::host();let mut env=Env::empty();let mut ins=Vec::new();for(n,ty,default)in &g.inputs{let v=if let Some((_,v))=over.iter().find(|(k,_)|k==n){coerce(ty,v)?}else if let Some(t)=default{coerce(ty,&eval_at(a,&sub(&cx,n.clone().into()),&env,t)?)?}else{return Err(errorf("E_INPUT_DEFAULT",format!("Graph input {} of {} needs a default.",n,g.name)))};let mut p=vec![g.name.clone(),format!(":{}",n)];note(a,&cx,p.clone(),v.clone());env.bind(intern(a,n),v.clone());ins.push((n.clone(),v));}let v=eval_at(a,&cx,&env,&g.body)?;Ok((v,ins))}
fn state_plane(a: &Arena, c: &ExecCtx) -> Result<(State, i64), Diagnostic> {
    let state = a.state.clone().ok_or_else(|| error("E_STATE", "State is not available."))?;
    let frame = a.time.as_ref().map_or(0, |frame| if c.host { frame.tick } else { frame.frame });
    if c.host { state.inner.lock().unwrap().host.get_or_insert_with(StatePlane::default); }
    Ok((state, frame))
}
fn state_key(c: &ExecCtx, zone: &[String]) -> StateKey {
    let mut path = (*c.prefix).clone();
    path.extend(zone.to_vec());
    StateKey { instance: c.inst, path, iter: (*c.iter).clone() }
}
fn begin_frame(state: &mut StateData, source: &str, frame: i64) {
    if state.source != source || state.frame.is_some_and(|old| frame < old) {
        state.source = source.into();
        state.frame = None;
        state.before.clear();
        state.next.clear();
        state.epoch = STATE_EPOCH.fetch_add(1, Ordering::Relaxed);
    }
    if state.frame != Some(frame) {
        let newer = std::mem::take(&mut state.next);
        state.before.extend(newer);
        state.frame = Some(frame);
    }
}
fn begin_host_frame(state: &mut StatePlane, source: &str, frame: i64) {
    if state.source != source || state.frame.is_some_and(|old| frame < old) {
        state.source = source.into();
        state.frame = None;
        state.before.clear();
        state.next.clear();
        state.epoch = STATE_EPOCH.fetch_add(1, Ordering::Relaxed);
    }
    if state.frame != Some(frame) {
        let newer = std::mem::take(&mut state.next);
        state.before.extend(newer);
        state.frame = Some(frame);
    }
}
fn state_data(value: Value) -> Result<Value, Diagnostic> {
    let _nesting = EvalNesting::enter()?;
    match value {
        Value::Deferred(..) | Value::No_geo => eval_error("E_STATE_TYPE", "State stores data, not deferred nodes or layouts."),
        Value::Struct(_, ty, _) if flow_syntax::ty::shape(&ty) => eval_error("E_STATE_TYPE", "State stores data, not deferred nodes or layouts."),
        Value::Fn(_) => eval_error("E_FN_ESCAPES", "A function value cannot be stored in state."),
        Value::List(values) => Ok(Value::List(Arc::new(values.iter().cloned().map(state_data).collect::<Result<Vec<_>, _>>()?))),
        Value::Record(fields) => Ok(Value::Record(fields.into_iter().map(|(name, value)| Ok((name, state_data(value)?))).collect::<Result<Vec<_>, Diagnostic>>()?)),
        Value::Struct(name, ty, fields) => Ok(Value::Struct(name, ty, fields.into_iter().map(|(name, value)| Ok((name, state_data(value)?))).collect::<Result<Vec<_>, Diagnostic>>()?)),
        value => Ok(value),
    }
}
fn state_previous(a: &mut Arena, c: &ExecCtx, env: &Env, zone: &Path, init: &Term) -> Result<Value, Diagnostic> {
    let (state, frame) = state_plane(a, c)?;
    let source = workspace_source_key(a);
    let key = state_key(c, zone);
    let previous = {
        let mut state_data = state.inner.lock().unwrap();
        if c.host {
            let plane = state_data.host.get_or_insert_with(StatePlane::default);
            begin_host_frame(plane, &source, frame);
            plane.before.get(&key).cloned()
        } else {
            begin_frame(&mut state_data, &source, frame);
            state_data.before.get(&key).cloned()
        }
    };
    let value = match previous { Some(value) => state_data(value), None => state_data(eval_at(a, c, env, init)?) }?;
    // The interpreter needs host data; compiled state reads keep device placeholders.
    Ok(Device::thaw(&value))
}
fn state_write(a: &mut Arena, c: &ExecCtx, zone: &Path, value: Value) -> Result<Value, Diagnostic> {
    let value = state_data(concrete(a, value)?)?;
    let (state, frame) = state_plane(a, c)?;
    let source = workspace_source_key(a);
    let key = state_key(c, zone);
    let mut data = state.inner.lock().unwrap();
    if c.host {
        let plane = data.host.get_or_insert_with(StatePlane::default);
        begin_host_frame(plane, &source, frame);
        plane.next.insert(key, value.clone());
        plane.epoch = STATE_EPOCH.fetch_add(1, Ordering::Relaxed);
    } else {
        begin_frame(&mut data, &source, frame);
        data.next.insert(key, value.clone());
        data.epoch = STATE_EPOCH.fetch_add(1, Ordering::Relaxed);
    }
    Ok(value)
}

fn eval_sequence(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term, route: Seg) -> Result<(Value, usize), Diagnostic> {
    let value = eval_concrete(a, &sub(c, route), env, term)?;
    let count = match &value { Value::List(values) => values.len(), _ => array_len(&value)? };
    Ok((value, count))
}
fn sequence_item(value: &Value, index: usize) -> Result<Value, Diagnostic> {
    match value { Value::List(values) => Ok(values[index].clone()), _ => array_get(value, index) }
}
fn eval_hof(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term, kind: &W::HofKind, function_term: &Term, rest: &[TermId]) -> Result<Value, Diagnostic> {
    let packed = rest.iter().any(|id| matches!(term.arena().ty(term.arena().row(*id).ty), Ty::Array(_)));
    let packed_context;
    let c = if packed {
        packed_context = ExecCtx { data: true, ..c.clone() };
        &packed_context
    } else { c };
    let saved_steps = a.step_count;
    let result = (|| {
        let function = eval_concrete(a, &sub(c, "f".into()), env, function_term)?;
        let Value::Fn(function) = function else { return eval_error("E_TYPE", "Expected a function."); };
        // OCaml records a map's `~calls` (where its closure's call count starts, and the map) before the frame test below gives way.
        let calls = if matches!(kind, W::HofKind::Map) && a.record && c.record && matches!(term.ty(), Ty::Array(_)) {
            match own_fn(a, &function).cloned() {
                Some(Function::Closure(closure)) => {
                    let id = map_residual(a, c, term, env);
                    let offset = function_call_offset(a, function);
                    let mut path = closure.zone.clone();
                    path.push("~calls".into());
                    note(a, c, path, Value::Record(vec![("offset".into(), Value::Int(offset)), ("call".into(), Value::Residual(id))]));
                    Some(id)
                }
                _ => None,
            }
        } else { None };
        // A function that reads the frame is live whichever elements read it: walking until one does could spend the static budget on a branch that leaves it unread.
        if a.time.is_none() && read_value(a, &Value::Fn(function), true, &mut BTreeSet::new(), &mut HashSet::new(), &mut HashSet::new(), &mut HashSet::new()) { return need_time(); }
        eval_hof_work(a, c, env, term, kind, function, rest, packed, saved_steps, calls)
    })();
    if packed { a.step_count = saved_steps; }
    result
}
fn check_packed_block(a: &mut Arena, iter: &mut usize, packed: bool, saved_steps: usize) -> Result<(), Diagnostic> {
    if packed && *iter % PACKED_BLOCK == 0 {
        if a.interrupt.as_ref().is_some_and(|stop| stop()) { return Err(error("cancelled", "Evaluation interrupted.")); }
        a.step_count = saved_steps;
    }
    *iter += 1;
    Ok(())
}
struct MapProgress { finish: Option<StaticMapFinish>, head: usize, tail: usize, completed: bool }
fn map_residual(a: &mut Arena, c: &ExecCtx, term: &Term, env: &Env) -> ResidualId {
    a.observed.iter().rev().find(|id| own_res(a, id).is_some_and(|residual| residual.term.identity() == term.identity() && residual.ctx.iter == c.iter && site(&residual.ctx) == site(c))).copied()
        .unwrap_or_else(|| residual_id(a, ResidualData { term: Arc::new(term.clone()), env: Arc::new(capture_env(term, env)), ctx: c.clone(), previous: false, live: None }))
}
fn function_call_offset(a: &Arena, function: FnId) -> i64 {
    a.fn_calls.get(&function).copied().unwrap_or_else(|| match own_fn(a, &function) { Some(Function::Closure(closure)) => closure.calls, Some(Function::Named(named)) => named.calls, None => 0 })
}
fn reserve_function_calls(a: &mut Arena, function: FnId, count: usize) {
    let next = function_call_offset(a, function).saturating_add(count as i64);
    a.fn_calls.insert(function, next);
}
fn function_replayable(a: &Arena, function: FnId) -> bool {
    matches!(own_fn(a, &function), Some(Function::Closure(closure)) if closure.replayable)
}
// The host's map request and its finish run with the arena lent; finish runs on every exit of the walk.
fn eval_map(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term, function: FnId, sources: &[TermId], packed: bool, saved_steps: usize, iter: &mut usize, calls: Option<ResidualId>) -> Result<Value, Diagnostic> {
    let mut progress = MapProgress { finish: None, head: 0, tail: 0, completed: false };
    let result = eval_map_walk(a, c, env, term, function, sources, packed, saved_steps, iter, &mut progress, calls);
    if let Some(finish) = progress.finish.take() { lend(a, || finish(progress.head, progress.tail, progress.completed)); }
    result
}
fn eval_map_walk(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term, function: FnId, sources: &[TermId], packed: bool, saved_steps: usize, iter: &mut usize, progress: &mut MapProgress, calls: Option<ResidualId>) -> Result<Value, Diagnostic> {
    let mut caller = Caller::new(a, c);
    let mut lists = Vec::with_capacity(sources.len());
    for (index, source) in sources.iter().enumerate() { lists.push(eval_sequence(a, c, env, &term.child(*source), Seg::Index("l", index))?); }
    let count = if lists.is_empty() { 0 } else { lists.iter().map(|(_, count)| *count).min().unwrap_or(0) };
    let numeric = !matches!(term.ty(), Ty::Array(element) if **element == Ty::Text) && !sources.iter().any(|source| matches!(term.arena().ty(term.arena().row(*source).ty), Ty::Array(element) if **element == Ty::Text));
    let static_map = a.time.is_none() && packed && numeric;
    let id = if static_map || (a.record && c.record && matches!(term.ty(), Ty::Array(_))) { Some(calls.unwrap_or_else(|| map_residual(a, c, term, env))) } else { None };
    if static_map {
        if let Some(map) = current_residual_handlers().and_then(|handlers| handlers.map) { let at = site(c); progress.finish = Some(lend(a, || outside_handler(|| map(&at, term, count)))); }
    }
    // A packed map of at least BULK_MIN_ELEMENTS (and at most 4,096, where OCaml walks it) is offered to a trusted bulk handler before
    // it is walked: the handler's whole array is the walker's (trusted), the call counts are reserved as for a large map, and packed
    // walks restore their step count, so nothing observable differs; it only asks the host where OCaml would not (post_parity_backlog).
    if static_map && count >= BULK_MIN_ELEMENTS && count <= MAX_ITERATIONS && matches!(term.ty(), Ty::Array(_))
        // never while recording: the walk records each element (a large map replays its records; a walked one is the record)
        && !(a.record && c.record) && current_residual_handlers().is_some_and(|handlers| handlers.trusted)
    {
        let id = id.expect("a static map has its residual");
        if let Some(candidate) = current_residual_handlers().and_then(|handlers| handlers.bulk).and_then(|bulk| call_lent(a, id, &|id| bulk(id))) {
            if value::ty_of(&candidate) == *term.ty() && array_len(&candidate).ok() == Some(count) {
                reserve_function_calls(a, function, count);
                progress.completed = true;
                return Ok(candidate);
            }
        }
    }
    let mut large = static_map && count > MAX_ITERATIONS && matches!(term.ty(), Ty::Array(_));
    if large {
        let id = id.unwrap();
        let can_defer = matches!(own_fn(a, &function), Some(Function::Closure(_)));
        if can_defer && current_residual_handlers().and_then(|handlers| handlers.defer).is_some_and(|defer| call_lent(a, id, &|id| defer(id).then_some(Value::Bool(true))).is_some()) {
            reserve_function_calls(a, function, count);
            progress.completed = false;
            return Ok(Value::Residual(id));
        }
        let trusted = current_residual_handlers().is_some_and(|handlers| handlers.trusted && (!a.record || !c.record || (handlers.replay && function_replayable(a, function))));
        if trusted {
            if let Some(candidate) = current_residual_handlers().and_then(|handlers| handlers.bulk).and_then(|bulk| call_lent(a, id, &|id| bulk(id))) {
                if value::ty_of(&candidate) == *term.ty() && array_len(&candidate).ok() == Some(count) {
                    reserve_function_calls(a, function, count);
                    progress.completed = true;
                    return Ok(candidate);
                }
            }
            large = false; // OCaml walks the whole map after a refused whole-array offer, with no second offer
        }
    }
    let walked = if large { MAX_ITERATIONS } else { count };
    let mut output = value::array_buffer(walked)?;
    for index in 0..walked {
        check_packed_block(a, iter, packed, saved_steps)?;
        let args = lists.iter().map(|(values, _)| sequence_item(values, index)).collect::<Result<Vec<_>, _>>()?;
        let value = call_in(&mut caller, a, c, function, &args)?;
        output.push(concrete(a, value)?);
        if index < MAX_ITERATIONS { progress.head += 1; } else { progress.tail += 1; }
    }
    if large {
        let id = id.unwrap();
        let head_value = pack(term.ty(), output.clone())?;
        let approximate = current_residual_handlers().and_then(|handlers| handlers.bulk_approx).and_then(|bulk| call_lent(a, id, &|id| bulk(id)))
            .filter(|candidate| bulk_matches(&head_value, candidate, count, walked, true));
        if let Some(candidate) = approximate {
            a.approx += 1;
            reserve_function_calls(a, function, count - walked);
            progress.completed = true;
            return Ok(candidate);
        }
        let exact = current_residual_handlers().and_then(|handlers| handlers.bulk).and_then(|bulk| call_lent(a, id, &|id| bulk(id)))
            .filter(|candidate| bulk_matches(&head_value, candidate, count, walked, false));
        if let Some(candidate) = exact {
            reserve_function_calls(a, function, count - walked);
            progress.completed = true;
            return Ok(candidate);
        }
        for index in walked..count {
            check_packed_block(a, iter, packed, saved_steps)?;
            let args = lists.iter().map(|(values, _)| sequence_item(values, index)).collect::<Result<Vec<_>, _>>()?;
            let value = call_in(&mut caller, a, c, function, &args)?;
            output.try_reserve(1).map_err(|_| array_memory_error())?;
            output.push(concrete(a, value)?);
            progress.tail += 1;
        }
    }
    let result = if matches!(term.ty(), Ty::Array(_)) { pack(term.ty(), output)? } else { join_list(output) };
    progress.completed = true;
    Ok(result)
}
fn eval_hof_work(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term, kind: &W::HofKind, function: FnId, rest: &[TermId], packed: bool, saved_steps: usize, calls: Option<ResidualId>) -> Result<Value, Diagnostic> {
    let mut caller = Caller::new(a, c);
    let mut iter = 0usize;
    match kind {
        W::HofKind::Map => eval_map(a, c, env, term, function, rest, packed, saved_steps, &mut iter, calls),
        W::HofKind::Filter => {
            if rest.len() != 1 { return eval_error("E_ARITY", "filter takes one list."); }
            let (values, count) = eval_sequence(a, c, env, &term.child(rest[0]), "l0".into())?;
            let mut output = value::array_buffer(count)?;
            for index in 0..count {
                check_packed_block(a, &mut iter, packed, saved_steps)?;
                let value = sequence_item(&values, index)?;
                let selected = call_in(&mut caller, a, c, function, &[value.clone()])?;
                if truth(&concrete(a, selected)?)? { output.push(value); }
            }
            if matches!(term.ty(), Ty::Array(_)) { pack(term.ty(), output) } else { Ok(join_list(output)) }
        }
        W::HofKind::SortBy => {
            if rest.len() != 1 { return eval_error("E_ARITY", "sort-by takes one list."); }
            let (values, count) = eval_sequence(a, c, env, &term.child(rest[0]), "l0".into())?;
            let mut keyed = value::array_buffer(count)?;
            for index in 0..count {
                check_packed_block(a, &mut iter, packed, saved_steps)?;
                let value = sequence_item(&values, index)?;
                let result = call_in(&mut caller, a, c, function, &[value.clone()])?;
                keyed.push((num(&concrete(a, result)?)?, index, value));
            }
            keyed.sort_unstable_by(|left, right| left.0.total_cmp(&right.0).then(left.1.cmp(&right.1)));
            let mut output = value::array_buffer(count)?;
            output.extend(keyed.into_iter().map(|(_, _, value)| value));
            if matches!(term.ty(), Ty::Array(_)) { pack(term.ty(), output) } else { Ok(join_list(output)) }
        }
        W::HofKind::Reduce => {
            if rest.len() != 2 { return eval_error("E_ARITY", "reduce takes an initial value and list."); }
            let mut accumulator = eval_sub(a, c, "init".into(), env, &term.child(rest[0]))?;
            let (values, count) = eval_sequence(a, c, env, &term.child(rest[1]), "l0".into())?;
            for index in 0..count {
                check_packed_block(a, &mut iter, packed, saved_steps)?;
                let value = sequence_item(&values, index)?;
                let next = call_in(&mut caller, a, c, function, &[accumulator.clone(), value])?;
                accumulator = widen_like(a, &accumulator, &next)?;
            }
            Ok(accumulator)
        }
        W::HofKind::Iterate => {
            if rest.len() < 2 || rest.len() > 3 { return eval_error("E_ARITY", "iterate takes an initial value, count and optional stop function."); }
            let mut accumulator = eval_sub(a, c, "init".into(), env, &term.child(rest[0]))?;
            let count_term = eval_sub(a, c, "n".into(), env, &term.child(rest[1]))?;
            let count = int_value(&concrete(a, count_term)?)?;
            let stop = if let Some(id) = rest.get(2) {
                match eval_concrete(a, &sub(c, "until".into()), env, &term.child(*id))? { Value::Fn(function) => Some(function), _ => return eval_error("E_TYPE", "Expected a function.") }
            } else { None };
            for _ in 0..count {
                check_packed_block(a, &mut iter, packed, saved_steps)?;
                let next = call_in(&mut caller, a, c, function, &[accumulator.clone()])?;
                let next = widen_like(a, &accumulator, &next)?;
                if let Some(stop) = stop {
                    let finished = call_in(&mut caller, a, c, stop, &[accumulator.clone(), next.clone()])?;
                    if truth(&concrete(a, finished)?)? { accumulator = next; break; }
                }
                accumulator = next;
            }
            Ok(accumulator)
        }
    }
}
fn v_len(value: &Value) -> usize { match value { Value::List(items) => items.len(), _ => array_len(value).unwrap_or(0) } }
fn pack(ty: &Ty, values: Vec<Value>) -> Result<Value, Diagnostic> {
    let Ty::Array(element) = ty else { return Ok(join_list(values)); };
    match element.as_ref() {
        Ty::Int => {
            let mut data = value::array_buffer(values.len())?;
            for value in values { data.push(int_value(&value)?); }
            Ok(Value::Int_array(Arc::new(data)))
        },
        Ty::Text => {
            let mut data = value::array_buffer(values.len())?;
            for value in values { match value { Value::Text(bytes) => data.push(bytes), _ => return eval_error("E_ARRAY_TYPE", "Text arrays contain text values.") } }
            Ok(Value::Text_array(Arc::new(data)))
        },
        Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => {
            let width = match element.as_ref() { Ty::Float => 1, Ty::Vec2 => 2, Ty::Vec3 => 3, _ => 4 };
            let mut data = value::array_buffer(values.len().checked_mul(width).ok_or_else(array_memory_error)?)?;
            for value in values {
                match (width, value) {
                    (1, number @ (Value::Int(_) | Value::Float(_) | Value::Bool(_))) => data.push(value::fin("array loop", num(&number)?)?),
                    (2, Value::Vec2(x, y)) => data.extend([value::fin("array loop", x)?, value::fin("array loop", y)?]),
                    (3, Value::Vec3(x, y, z)) => data.extend([value::fin("array loop", x)?, value::fin("array loop", y)?, value::fin("array loop", z)?]),
                    (4, Value::Vec4(x, y, z, w)) => data.extend([value::fin("array loop", x)?, value::fin("array loop", y)?, value::fin("array loop", z)?, value::fin("array loop", w)?]),
                    (_, number @ (Value::Int(_) | Value::Float(_) | Value::Bool(_))) => {
                        let number = value::fin("array loop", num(&number)?)?;
                        data.extend(std::iter::repeat_n(number, width));
                    }
                    _ => return eval_error("E_ARRAY_TYPE", "Packed loop element has the wrong vector width."),
                }
            }
            let data = Arc::new(data);
            Ok(match width { 1 => Value::Float_array(data), 2 => Value::Vec2_array(data), 3 => Value::Vec3_array(data), _ => Value::Vec4_array(data) })
        }
        _ => Ok(join_list(values)),
    }
}
fn loop_values(source: Value) -> Result<Vec<Value>, Diagnostic> {
    match source {
        Value::List(values) => { let mut output = value::array_buffer(values.len())?; output.extend(values.iter().cloned()); Ok(output) },
        Value::Int_array(_) | Value::Text_array(_) | Value::Float_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_) => {
            let count = array_len(&source)?;
            let mut output = value::array_buffer(count)?;
            for index in 0..count { output.push(array_get(&source, index)?); }
            Ok(output)
        }
        Value::Struct(op, Ty::List(_), _) => Err(errorf("E_ZONE", format!("{op}: only a for with one clause and no :skip can iterate the elements of geometry."))),
        _ => Err(error("E_TYPE", "Expected a list or packed array.")),
    }
}
// A record update keeps the existing field's tag (the current one, if it is still a residual); an Int seed widens to a Float body.
fn coerce_like(a: &mut Arena, w: &Value, v: &Value) -> Result<Value, Diagnostic> {
    let w = concrete(a, w.clone())?;
    let v = concrete(a, v.clone())?;
    match (&w, &v) {
        (Value::List(wa), Value::List(_)) if wa.is_empty() => Ok(v),
        (Value::List(wa), Value::List(vb)) => Ok(Value::List(Arc::new(vb.iter().map(|x| coerce_like(a, &wa[0], x)).collect::<Result<Vec<_>, _>>()?))),
        (Value::Record(wf), Value::Record(fs)) => Ok(Value::Record(fs.iter().map(|(n, x)| Ok((n.clone(), match wf.iter().find(|(k, _)| k == n) { Some((_, old)) => coerce_like(a, old, x)?, None => x.clone() }))).collect::<Result<Vec<_>, Diagnostic>>()?)),
        _ => coerce(&value::ty_of(&w), &v),
    }
}
fn widen_like(a: &mut Arena, w: &Value, v: &Value) -> Result<Value, Diagnostic> {
    let w = concrete(a, w.clone())?;
    let v = concrete(a, v.clone())?;
    match (&w, &v) { (Value::Int(_), Value::Float(_)) => Ok(v), _ => coerce_like(a, &w, &v) }
}
fn eval_loop(a: &mut Arena, c: &ExecCtx, env: &Env, term: &Term, node: &flow_check::term_arena::Loop) -> Result<Value, Diagnostic> {
    let arena = term.arena();
    let kind = &node.kind;
    let accs = arena.bindings(node.accs);
    let clauses = arena.bindings(node.clauses);
    let skip = arena.skip(node.skip);
    let body = &term.child(node.body);
    let zone = &packed_base(a, term, node.zone);
    let packed = clauses.iter().any(|binding| matches!(arena.ty(arena.row(binding.term).ty), Ty::Array(_)));
    fn geometry_accumulator(ty: &Ty) -> bool {
        match ty {
            Ty::Named(name) if name == "geometry" => true,
            Ty::Record(fields) => fields.iter().any(|(_, ty)| geometry_accumulator(ty)),
            _ => false,
        }
    }
    let packed_context;
    let c = if packed && !(kind == &W::LoopKind::Fold && geometry_accumulator(term.ty())) {
        packed_context = ExecCtx { data: true, ..c.clone() };
        &packed_context
    } else { c };
    let saved_steps = a.step_count;
    let mut accumulators = Vec::with_capacity(accs.len());
    for binding in accs {
        let mut context = c.clone();
        context.base = zone.clone();
        context.route = Route(None);
        accumulators.push(eval_sub(a, &context, "init".into(), env, &term.child(binding.term))?);
    }
    let first = {
        let binding = &clauses[0];
        let source = eval_concrete(a, &sub(c, "in0".into()), env, &term.child(binding.term))?;
        match source {
            Value::Struct(_, Ty::List(_), _) if kind == &W::LoopKind::For && clauses.len() == 1 && skip.is_empty() => {
                return eval_geometry_loop(a, c, env, arena, binding.pattern, body, zone, source);
            }
            source => loop_values(source)?,
        }
    };
    let lists = [first];
    let mut output = Vec::new();
    let mut sum = None;
    let mut count = 0usize;
    let mut selected = Vec::with_capacity(clauses.len());
    let mut tuple = Vec::with_capacity(clauses.len());
    fn walk(
        a: &mut Arena, c: &ExecCtx, env: &Env, owner: &Term, kind: &W::LoopKind, accs: &[Binding],
        clauses: &[Binding], lists: &[Vec<Value>], skip: &[RangeId], body: &Term,
        zone: &Arc<Path>, depth: usize, selected: &mut Vec<Value>, tuple: &mut Vec<i64>,
        accumulators: &mut Vec<Value>, output: &mut Vec<Value>, sum: &mut Option<Value>,
        count: &mut usize, packed: bool, saved_steps: usize,
    ) -> Result<(), Diagnostic> {
        let _nesting = EvalNesting::enter()?;
        if depth < clauses.len() {
            let binding = &clauses[depth];
            // A later source is evaluated where the earlier names are bound, when its turn comes.
            let fresh = if depth == 0 { None } else { Some(loop_values(eval_concrete(a, &sub(c, Seg::Index("in", depth)), env, &owner.child(binding.term))?)?) };
            let items: &[Value] = match &fresh { Some(v) => v, None => &lists[0] };
            for (index, item) in items.iter().enumerate() {
                selected.push(item.clone());
                tuple.push(index as i64);
                let mut inner = env.clone();
                bind_packed(a, c, &mut inner, owner.arena(), binding.pattern, item.clone(), None)?;
                walk(a, c, &inner, owner, kind, accs, clauses, lists, skip, body, zone, depth + 1, selected, tuple, accumulators, output, sum, count, packed, saved_steps)?;
                tuple.pop();
                selected.pop();
            }
            return Ok(());
        }
        if !packed && *count >= MAX_ITERATIONS { return Err(errorf("E_ITER_BOUND", format!("{} runs more than 4,096 iterations.", path_text(zone)))); }
        if packed && *count % PACKED_BLOCK == 0 {
            if a.interrupt.as_ref().is_some_and(|stop| stop()) { return Err(error("cancelled", "Evaluation interrupted.")); }
            a.step_count = saved_steps;
        }
        let ordinal = *count;
        let mut cx = c.clone();
        cx.base = zone.clone();
        cx.route = Route(None);
        Arc::make_mut(&mut cx.iter).push(ordinal as i64);
        // OCaml matches the skip tuples against the iteration path: the outer iteration plus this flat ordinal.
        let skipped = skip.iter().any(|indices| owner.arena().indices(*indices).iter().copied().map(|index| index as i64).eq(cx.iter.iter().copied()));
        let mut local = env.clone();
        for (binding, value) in clauses.iter().zip(selected.iter()) {
            record_packed(a, &cx, zone, owner.arena(), binding.pattern, value)?;
            bind_packed(a, &cx, &mut local, owner.arena(), binding.pattern, value.clone(), None)?;
        }
        for (binding, value) in accs.iter().zip(accumulators.iter()) {
            record_packed(a, &cx, zone, owner.arena(), binding.pattern, value)?;
            bind_packed(a, &cx, &mut local, owner.arena(), binding.pattern, value.clone(), None)?;
        }
        if !skipped {
            let mut value = eval_at(a, &cx, &local, body)?;
            if packed { value = concrete(a, value)?; }
            match kind {
                W::LoopKind::Fold | W::LoopKind::Scan => {
                    let Some(accumulator) = accumulators.first_mut() else { return Err(error("E_ARITY", "fold needs an accumulator.")); };
                    let next = value;
                    *accumulator = widen_like(a, accumulator, &next)?;
                    if kind == &W::LoopKind::Scan { output.try_reserve(1).map_err(|_| array_memory_error())?; output.push(accumulator.clone()); }
                }
                W::LoopKind::Sum => *sum = Some(match sum.take() {
                    Some(previous) => { let (p, v) = (concrete(a, previous)?, concrete(a, value)?); value::arith("sum", &|left, right| left + right, &p, &v)? }
                    None => value,
                }),
                W::LoopKind::For => { output.try_reserve(1).map_err(|_| array_memory_error())?; output.push(value); },
            }
        }
        *count += 1;
        Ok(())
    }
    let result = (|| {
        walk(a, c, env, term, kind, accs, clauses, &lists, skip, body, zone, 0, &mut selected, &mut tuple, &mut accumulators, &mut output, &mut sum, &mut count, packed, saved_steps)?;
        match kind {
            W::LoopKind::Fold => accumulators.into_iter().next().ok_or_else(|| error("E_ARITY", "fold needs an accumulator.")),
            W::LoopKind::Sum => match sum { None => Ok(Value::Int(0)), Some(total) => concrete(a, total) },
            W::LoopKind::For | W::LoopKind::Scan => if matches!(term.ty(), Ty::Array(_)) { pack(term.ty(), output) } else { Ok(join_list(output)) },
        }
    })();
    if packed { a.step_count = saved_steps; }
    result
}
fn eval_geometry_loop(a: &mut Arena, c: &ExecCtx, env: &Env, arena: &TermArena, pattern: PatternId, body: &Term, zone: &Arc<Path>, source: Value) -> Result<Value, Diagnostic> {
    let (operation, fields) = match source { Value::Struct(operation, Ty::List(_), fields) => (operation, fields), _ => return eval_error("E_ZONE", "Geometry loop source is malformed.") };
    let source_id = match fields.iter().find(|(name, _)| name == "geometry").map(|(_, value)| value) {
        Some(Value::Deferred(ty, id)) if flow_syntax::ty::is_geometry(ty) => *id,
        _ => return Err(errorf("E_TYPE", format!("{}: {} needs geometry.", path_text(zone), operation))),
    };
    let points = operation == "sop/point_list";
    let key = fields.iter().find(|(name, _)| name == "key").map(|(_, value)| value.clone());
    let mut context = c.clone();
    context.base = zone.clone();
    context.route = Route(None);
    Arc::make_mut(&mut context.iter).push(0);
    context.geometry = true;
    let element_name = element_key(zone);
    let lo = a.nodes.len() as i64;
    let (element, element_arg) = if points {
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&body.form());
        let id = builder.push(None, &Ty::Vec3, form, Draft::RefBinding(element_name.clone(), Vec::new()));
        let term = builder.freeze().handle(id);
        let residual = residual_id(a, ResidualData { term: Arc::new(term), env: Arc::new(BTreeMap::new()), ctx: context.clone(), previous: false, live: None });
        (Value::Residual(residual), text(&element_name))
    } else {
        let value = mk_node(a, &sub(&context, "element".into()), 0, flow_syntax::ty::geometry(), "zone/element".into(), Vec::new())?;
        let Value::Deferred(_, id) = value.clone() else { return eval_error("E_TYPE", "A geometry element must be a plan node."); };
        (value, Value::Int(id))
    };
    let mut local = env.clone();
    record_packed(a, &context, zone, arena, pattern, &element)?;
    bind_packed(a, &context, &mut local, arena, pattern, element, None)?;
    let value = eval_at(a, &context, &local, body)?;
    let root = match value {
        Value::Deferred(ty, id) if ty::is_geometry(&ty) => id,
        Value::Residual(_) => return Err(errorf("E_ZONE", format!("{}: what a loop over geometry builds cannot depend on the element; only arguments can.", path_text(zone)))),
        _ => return Err(errorf("E_TYPE", format!("{}: the body of a loop over geometry returns geometry.", path_text(zone)))),
    };
    let hi = a.nodes.len() as i64;
    let mut args = vec![("geometry".into(), Value::Deferred(flow_syntax::ty::geometry(), source_id))];
    if let Some(key) = key { args.push(("key".into(), key)); }
    args.extend([("body".into(), Value::Int(root)), ("lo".into(), Value::Int(lo)), ("hi".into(), Value::Int(hi)), ("element".into(), element_arg)]);
    mk_node(a, c, 0, flow_syntax::ty::geometry(), if points { "zone/points" } else { "zone/pieces" }.into(), args)
}

fn result_from(owner: &Arc<Slot>, arena: &Arena, results: Vec<(String, Value)>) -> Eval {
    let records = arena.records.iter().map(|(path, values)| (path.clone(), values.clone())).collect();
    let instances = arena.cells.iter().map(|cell| Instance { graph: cell.graph.clone(), default: cell.default, inputs: cell.inputs.clone(), result: cell.result.clone() }).collect();
    Eval { plan: Plan { id: fresh_plan_id(), instances, nodes: arena.nodes.to_vec() }, authored: arena.authored.clone(), results, states: arena.states.to_vec(), records, owner: owner.clone() }
}
 
pub fn static_eval(workspace: &Workspace) -> Result<Eval, Diagnostic> { static_with(workspace, false, None, None) }
pub fn r#static(workspace: &Workspace, record: bool, inputs: Option<&[(String, Vec<(String, Value)>)]>) -> Result<Eval, Diagnostic> { static_with(workspace, record, inputs, None) }
pub fn static_with(workspace: &Workspace, record: bool, inputs: Option<&[(String, Vec<(String, Value)>)]>, budget: Option<usize>) -> Result<Eval, Diagnostic> {
    let _nesting = EvalNesting::enter()?;
    for graph in workspace.graphs.iter().chain(workspace.defs.iter()) {
        check_term_nesting(&graph.body)?;
        for (_, _, default) in &graph.inputs { if let Some(term) = default { check_term_nesting(term)?; } }
    }
    for node in workspace.nodes.iter() {
        check_term_nesting(&node.body.body)?;
        for (term, _, _) in &node.validations { check_term_nesting(term)?; }
    }
    let _timer = flow_syntax::phase_timer::measure("evaluate");
    if let Some(overrides) = inputs {
        let mut names = BTreeSet::new();
        for (name, inputs) in overrides {
            if !names.insert(name.clone()) { return Err(error("E_INPUT_DUPLICATE", "A graph receives more than one input override set.")); }
            if !workspace.graphs.iter().any(|graph| graph.name == *name) { return Err(errorf("E_UNKNOWN_GRAPH", format!("Unknown graph input target: {}.", name))); }
            for (_, value) in inputs { check_value_nesting(value)?; value::validate(value)?; }
        }
    }
    FREE_NAMES_CACHE.lock().unwrap().clear();
    let slot = Arena::new(workspace, record, budget.unwrap_or(MAX_STEPS));
    let mut arena = checkout(&slot);
    let (graphs, root) = (arena.workspace.graphs.clone(), root_ctx(&arena));
    let mut results = Vec::with_capacity(graphs.len());
    for graph in graphs.iter() {
        let overrides = inputs.and_then(|inputs| inputs.iter().find(|(name, _)| name == &graph.name)).map_or(&[][..], |(_, values)| values.as_slice());
        let value = graph_value(&mut arena, &root, &graph.name, overrides, true)?;
        results.push((graph.name.clone(), value));
    }
    Ok(result_from(&slot, &arena, results))
}
pub fn static_with_requests(budget: Option<usize>, record: bool, inputs: Option<&[(String, Vec<(String, Value)>)]>, workspace: &Workspace) -> Result<Eval, Diagnostic> {
    static_with(workspace, record, inputs, budget)
}

pub fn create_state()->State{State::default()}
pub fn reset_state(state:&State,host_state:bool){let mut s=state.inner.lock().unwrap();s.source.clear();s.frame=None;s.before.clear();s.next.clear();s.epoch=STATE_EPOCH.fetch_add(1,Ordering::Relaxed);if host_state{s.host=None}}
pub fn fork_state(state:&State)->State{let s=state.inner.lock().unwrap().clone();State{inner:Arc::new(Mutex::new(s))}}
pub fn state_stamp(state:&State)->String{let s=state.inner.lock().unwrap();format!("{}{}",s.epoch,s.host.as_ref().map_or(String::new(),|x|format!(":{}",x.epoch)))}
fn restore_state(state:&State,saved:&State){*state.inner.lock().unwrap()=saved.inner.lock().unwrap().clone()}
pub fn state_values(state:&State)->Vec<Value>{let state=state.inner.lock().unwrap();let mut values=state.before.values().chain(state.next.values()).cloned().collect::<Vec<_>>();if let Some(host)=&state.host{values.extend(host.before.values().chain(host.next.values()).cloned());}values}
pub fn transaction(state:&State,f:&mut dyn FnMut()->Result<(),Diagnostic>)->Result<(),Diagnostic>{let saved=fork_state(state);let result=f();if result.is_err(){*state.inner.lock().unwrap()=saved.inner.lock().unwrap().clone()}result}

pub fn force(value:&Value,live:&Frame,state:Option<&State>)->Result<Value,Diagnostic>{force_with(value,live,state,None,None)}
type ResolveFn = Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>;
fn static_source_fields(value: &Value) -> bool {
    match value {
        Value::Source(_, _, f) | Value::Struct(_, _, f) | Value::Record(f) => f.iter().all(|(_, v)| static_source_fields(v)),
        Value::List(vs) => vs.iter().all(static_source_fields),
        v => !is_live_value(v),
    }
}
// OCaml with_live: a fresh state unless one is supplied, restored when the walk fails.
pub fn force_with(value: &Value, live: &Frame, state: Option<&State>, elems: Option<&[(String, Value)]>, resolve: Option<ResolveFn>) -> Result<Value, Diagnostic> {
    let _nesting = EvalNesting::enter()?;
    check_value_nesting(value)?;
    if !is_live_value(value) { return Ok(value.clone()); }
    if let (Value::Source(_, _, fields) | Value::Struct(_, Ty::Array(_), fields), None, None, Some(resolve)) = (value, state, elems, &resolve) {
        if fields.iter().all(|(_, v)| static_source_fields(v)) {
            fail_frame(live)?;
            let resolved = resolve_source_with(resolve, value)?;
            return if is_live_value(&resolved) { force_with(&resolved, live, None, None, Some(resolve.clone())) } else { Ok(resolved) };
        }
    }
    let owned = state.cloned().unwrap_or_default();
    let saved = fork_state(&owned);
    fail_frame(live)?;
    let result = force_general(value, live, Some(&owned), elems, resolve);
    if result.is_err() { restore_state(&owned, &saved); }
    result
}
fn force_general(value: &Value, live: &Frame, state: Option<&State>, elems: Option<&[(String, Value)]>, resolve: Option<ResolveFn>) -> Result<Value, Diagnostic> {
    let _nesting = EvalNesting::enter()?;
    match value {
        Value::Residual(id) => {
            let slot = arena_for_residual(id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
            let forced = {
                let mut arena = checkout(&slot);
                let saved_live = save_live(&mut arena);
                let previous = arena.state.clone();
                arena.time = Some(live.clone());
                arena.state = state.cloned().or(arena.state.clone());
                arena.resolve = resolve.clone();
                arena.elems = elems.unwrap_or(&[]).iter().cloned().collect();
                arena.step_count = 0;
                arena.fn_calls.clear();
                arena.memo.clear();
                let forced = execute_residual(&mut arena, *id);
                arena.state = previous;
                restore_live(&mut arena, saved_live);
                forced?
            };
            // OCaml's `go (force_res c r)`: a result that still holds residuals (captured record fields) is forced again.
            if is_live_value(&forced) { force_general(&forced, live, state, elems, resolve) } else { Ok(forced) }
        }
        Value::Source(..) | Value::Struct(_, Ty::Array(_), _) => {
            if let Some(slot) = arena_for_value(value) {
                let (previous, saved_live) = peek(&slot, |arena| {
                    let previous = arena.state.clone();
                    let saved_live = save_live(arena);
                    arena.time = Some(live.clone());
                    arena.state = state.cloned().or(arena.state.clone());
                    arena.resolve = resolve.clone();
                    arena.elems = elems.unwrap_or(&[]).iter().cloned().collect();
                    (previous, saved_live)
                });
                let forced = (|| match &resolve {
                    Some(r) => { let resolved = resolve_source_with(r, value)?; force_with(&resolved, live, state, elems, resolve.clone()) }
                    None => Err(error("E_DATA_SOURCE", "This data source needs its host's cooked data.")),
                })();
                peek(&slot, |arena| { arena.state = previous; restore_live(arena, saved_live); });
                forced
            } else if let Some(resolve) = resolve {
                let result = resolve_source_with(&resolve, value)?;
                force_with(&result, live, state, elems, Some(resolve))
            } else {
                eval_error("E_DATA_SOURCE", "This data source needs its host's cooked data.")
            }
        }
        Value::List(values) => Ok(Value::List(Arc::new(values.iter().map(|value| force_general(value, live, state, elems, resolve.clone())).collect::<Result<Vec<_>, _>>()?))),
        Value::Record(fields) => Ok(Value::Record(fields.iter().map(|(name, value)| Ok((name.clone(), force_general(value, live, state, elems, resolve.clone())?))).collect::<Result<Vec<_>, Diagnostic>>()?)),
        Value::Struct(name, ty, fields) => Ok(Value::Struct(name.clone(), ty.clone(), fields.iter().map(|(name, value)| Ok((name.clone(), force_general(value, live, state, elems, resolve.clone())?))).collect::<Result<Vec<_>, Diagnostic>>()?)),
        _ => Ok(value.clone()),
    }
}
fn resolve_source_with(resolve: &Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>, source: &Value) -> Result<Value, Diagnostic> {
    let value = resolve(source)?;
    check_value_nesting(&value)?;
    if matches!(value, Value::Source(..) | Value::Struct(_, Ty::Array(_), _)) { return eval_error("E_DATA_SOURCE", "The host returned an unresolved data source."); }
    if value::ty_of(source) != Ty::Any && value::ty_of(source) != value::ty_of(&value) { return eval_error("E_DATA_SOURCE", "The host returned a data source of the wrong type."); }
    value::validate(&value)?;
    Ok(value)
}
fn arena_for_residual(id: &ResidualId) -> Option<Arc<Slot>> { slot_of(id.1) }
pub fn residual_eval(id: ResidualId, live: &Frame, state: Option<&State>) -> Result<Value, Diagnostic> {
    let slot = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
    fail_frame(live)?;
    let mut arena = checkout(&slot);
    let saved_live = save_live(&mut arena);
    let previous = arena.state.clone();
    arena.time = Some(live.clone());
    arena.state = state.cloned().or(arena.state.clone());
    arena.step_count = 0;
    arena.fn_calls.clear();
    arena.memo.clear();
    let saved = arena.state.as_ref().map(fork_state);
    let result = execute_residual(&mut arena, id);
    if result.is_err() { if let (Some(state), Some(saved)) = (arena.state.as_ref(), saved.as_ref()) { restore_state(state, saved); } }
    arena.state = previous;
    restore_live(&mut arena, saved_live);
    result
}
pub fn run(workspace: &Workspace, record: bool, inputs: Option<&[(String, Vec<(String, Value)>)]>, state: Option<&State>, live: Option<&Frame>, time: f64) -> Result<Eval, Diagnostic> {
    let mut result = static_with(workspace, record, inputs, None)?;
    let frame = live.cloned().unwrap_or_else(|| input::at_time(time));
    let state = state.cloned().unwrap_or_default();
    let saved = fork_state(&state);
    let evaluated: Result<Eval, Diagnostic> = (|| {
        for (_, value) in &mut result.results { *value = force(value, &frame, Some(&state))?; }
        for node in &mut result.plan.nodes { for (_, value) in &mut node.args { *value = force(value, &frame, Some(&state))?; } }
        for instance in &mut result.plan.instances {
            for (_, value) in &mut instance.inputs { *value = force(value, &frame, Some(&state))?; }
            instance.result = force(&instance.result, &frame, Some(&state))?;
        }
        for (_, values) in &mut result.records { for (_, value) in values { *value = force(value, &frame, Some(&state))?; } }
        for value in &mut result.states { *value = force(value, &frame, Some(&state))?; }
        Ok(result)
    })();
    if evaluated.is_err() { *state.inner.lock().unwrap() = saved.inner.lock().unwrap().clone(); }
    evaluated
}

pub fn hash(values:&[f64])->f64{value::hash(values)}
pub fn show(v: &Value) -> Vec<u8> { value::show_with(&|v| v.clone(), v) }
pub fn is_live(value: &Value) -> bool { is_live_value(value) }
pub fn is_live_value(value: &Value) -> bool {
    match value {
        Value::Residual(_) | Value::Source(..) | Value::Struct(_, Ty::Array(_), _) => true,
        Value::List(values) => values.iter().any(is_live_value),
        Value::Record(fields) | Value::Struct(_, _, fields) => fields.iter().any(|(_, value)| is_live_value(value)),
        _ => false,
    }
}
fn read_value(arena: &mut Arena, value: &Value, frame: bool, reads: &mut BTreeSet<String>, residuals: &mut HashSet<(i64, i64)>, functions: &mut HashSet<(i64, i64)>, named: &mut HashSet<(i64, (u64, u32), &'static str)>) -> bool {
    let home = match value { Value::Residual(id) => id.1, Value::Fn(id) => id.1, _ => arena.id };
    if home != arena.id {
        // Captures may return to this home; never hold two arena locks while walking them.
        return lend(arena, || slot_of(home).is_some_and(|slot| {
            read_value(&mut checkout(&slot), value, frame, reads, residuals, functions, named)
        }));
    }
    match value {
        Value::Residual(id) => {
            if !residuals.insert((id.1, id.0)) { return false; }
            if let Some(residual) = own_res(arena, id).cloned() { read_term(arena, &residual.term, &residual.env, &residual.ctx.context, frame, reads, residuals, functions, named) } else { false }
        }
        Value::Fn(id) => {
            if !functions.insert((id.1, id.0)) { return false; }
            match own_fn(arena, id).cloned() {
                Some(Function::Closure(closure)) => read_term(arena, &closure.body, &closure.env, &closure.at.context, frame, reads, residuals, functions, named),
                Some(Function::Named(function)) => read_named(arena, &function.name, frame, reads, residuals, functions, named),
                None => false,
            }
        }
        Value::List(values) => values.iter().fold(false, |found, value| read_value(arena, value, frame, reads, residuals, functions, named) || found),
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().fold(false, |found, (_, value)| read_value(arena, value, frame, reads, residuals, functions, named) || found),
        _ => false,
    }
}
fn read_named(arena: &mut Arena, name: &str, frame: bool, reads: &mut BTreeSet<String>, residuals: &mut HashSet<(i64, i64)>, functions: &mut HashSet<(i64, i64)>, named: &mut HashSet<(i64, (u64, u32), &'static str)>) -> bool {
    let workspace = arena.workspace.clone();
    if let Some(graph) = workspace.defs.iter().find(|graph| graph.name == name) {
        // Reads accumulate in one set, so a shared named body contributes only once.
        if !named.insert((arena.id, graph.body.identity(), graph.context.as_str())) { return false; }
        return read_term(arena, &graph.body, &BTreeMap::new(), &graph.context, frame, reads, residuals, functions, named);
    }
    if frame && op::find_indexed(&arena.workspace.ops, name, &flow_syntax::context::value()).is_some_and(|operation| operation.live) {
        reads.insert(name.to_owned());
        return true;
    }
    false
}
fn read_term(arena: &mut Arena, term: &Term, env: &BTreeMap<String, Value>, context: &Context, frame: bool, reads: &mut BTreeSet<String>, residuals: &mut HashSet<(i64, i64)>, functions: &mut HashSet<(i64, i64)>, named: &mut HashSet<(i64, (u64, u32), &'static str)>) -> bool {
    let mut found = false;
    let packed = term.arena();
    match term.node {
        TermNode::Time if frame => { reads.insert("time".into()); found = true; }
        TermNode::State(_) => { reads.insert("state".into()); found = true; }
        TermNode::RefBinding(id) => if let Some(value) = env.get(term.name(packed.ref_binding(id).name)) { found |= read_value(arena, value, frame, reads, residuals, functions, named); },
        TermNode::FnRef(name) => found |= read_named(arena, term.name(name), frame, reads, residuals, functions, named),
        TermNode::CallFn(id) => {
            let name = term.name(packed.call_fn(id).name);
            if let Some(Value::Fn(function)) = env.get(name) { found |= read_value(arena, &Value::Fn(*function), frame, reads, residuals, functions, named); }
            else { found |= read_named(arena, name, frame, reads, residuals, functions, named); }
        }
        TermNode::Op(id) if frame => {
            let name = term.name(packed.op(id).op);
            if op::find_indexed(&arena.workspace.ops, name, context).is_some_and(|operation| operation.live) { reads.insert(name.to_owned()); found = true; }
        }
        TermNode::GraphRef(id) => {
            let workspace = arena.workspace.clone();
            if let Some(graph) = workspace.graphs.iter().find(|definition| definition.name == term.name(packed.graph_ref(id).graph)) {
                found |= read_term(arena, &graph.body, &BTreeMap::new(), &graph.context, frame, reads, residuals, functions, named);
                for (_, _, default) in &graph.inputs { if let Some(term) = default { found |= read_term(arena, term, env, context, frame, reads, residuals, functions, named); } }
            }
        },
        _ => (),
    }
    for child in children(term) { found |= read_term(arena, &child, env, context, frame, reads, residuals, functions, named); }
    found
}
fn dependent(value: &Value, frame: bool, reads: &mut BTreeSet<String>) -> bool {
    let Some(arena) = arena_for_value(value) else { return false; };
    let mut arena = checkout(&arena);
    read_value(&mut arena, value, frame, reads, &mut HashSet::new(), &mut HashSet::new(), &mut HashSet::new())
}
pub fn frame_dependent(value: &Value) -> bool { dependent_fast(value, true) }
pub fn state_dependent(value: &Value) -> bool { dependent_fast(value, false) }
pub fn frame_reads(value: &Value) -> Vec<String> {
    let mut reads = BTreeSet::new();
    let _ = dependent(value, true, &mut reads);
    reads.into_iter().collect()
}
fn stable_digest(bytes: &[u8]) -> String {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes { hash ^= u64::from(*byte); hash = hash.wrapping_mul(0x100000001b3); }
    format!("{hash:016x}")
}
fn structure_bytes(out: &mut String, bytes: &[u8]) {
    use std::fmt::Write;
    let _ = write!(out, "{}:", bytes.len());
    const HEX: &[u8; 16] = b"0123456789abcdef";
    out.reserve(bytes.len() * 2);
    for byte in bytes { out.push(HEX[(byte >> 4) as usize] as char); out.push(HEX[(byte & 15) as usize] as char); }
}
fn structure_path(out: &mut String, path: &[String]) {
    out.push('P');
    for segment in path { structure_bytes(out, segment.as_bytes()); }
    out.push(';');
}
fn structure_context(out: &mut String, context: &ExecCtx) {
    out.push_str(&format!("{}:{}:{}:{}:{}:{}:{}:{}:{}",
        context.inst, context.depth, context.iter.iter().map(i64::to_string).collect::<Vec<_>>().join(","),
        context.record as u8, context.data as u8, context.geometry as u8, context.host as u8,
        context.card.as_deref().unwrap_or(""), context.context.as_str()));
    structure_path(out, &context.prefix);
    structure_path(out, &context.base);
    structure_path(out, &context.route.to_vec());
}
// OCaml's `structure` term: the form spells an authored term (its id and flat text); a library form's id alone names it; a synthetic
// term (form 0) spells its node kind and children.
fn structure_term(out: &mut String, term: &Term) {
    out.push('t');
    let form = term.form();
    out.push_str(&form.id.to_string());
    structure_bytes(out, term.path().map_or_else(|| "-".to_owned(), |path| path.iter().map(|name| term.name(*name)).collect::<Vec<_>>().join("/")).as_bytes());
    structure_bytes(out, ty::to_string(term.ty()).as_bytes());
    if form.id < flow_syntax::syntax::LIBRARY_BASE {
        structure_bytes(out, flow_syntax::lisp::flat(&form).as_bytes());
        if form.id == 0 {
            out.push_str(&format!("{:?}", std::mem::discriminant(&term.node)));
            out.push('(');
            for child in children(term) { structure_term(out, &child); }
            out.push(')');
        }
    }
}
fn structure_value(
    out: &mut String, value: &Value, arena: Option<&Arena>, seen_residuals: &mut HashMap<ResidualId, usize>,
    seen_functions: &mut HashMap<FnId, usize>, next: &mut usize,
) {
    match value {
        Value::Residual(id) => {
            if let Some(index) = seen_residuals.get(id) { out.push_str(&format!("#{index}")); return; }
            let Some(residual) = arena.and_then(|arena| own_res(arena, id)) else { out.push_str("R?"); return; };
            let index = *next; *next += 1; seen_residuals.insert(*id, index);
            out.push_str(&format!("R{}:{}", index, residual.previous as u8));
            structure_term(out, &residual.term);
            structure_context(out, &residual.ctx);
            for (name, value) in residual.env.iter() {
                structure_bytes(out, name.as_bytes());
                structure_value(out, value, arena, seen_residuals, seen_functions, next);
            }
        }
        Value::Fn(id) => {
            let Some(function) = arena.and_then(|arena| own_fn(arena, id)) else { out.push_str("F?"); return; };
            match function {
                Function::Named(function) => { out.push('N'); structure_bytes(out, function.name.as_bytes()); }
                Function::Closure(closure) => {
                    if let Some(index) = seen_functions.get(id) { out.push_str(&format!("#{index}")); return; }
                    let index = *next; *next += 1; seen_functions.insert(*id, index);
                    out.push_str(&format!("C{}:{}", index, closure.replayable as u8));
                    structure_bytes(out, format!("{:?}", function_params(&closure.params.0, closure.params.1)).as_bytes());
                    structure_term(out, &closure.body);
                    structure_path(out, &closure.zone);
                    structure_context(out, &closure.at);
                    for (name, value) in closure.env.iter() {
                        structure_bytes(out, name.as_bytes());
                        structure_value(out, value, arena, seen_residuals, seen_functions, next);
                    }
                }
            }
        }
        Value::List(values) => {
            out.push('[');
            for value in values.iter() { structure_value(out, value, arena, seen_residuals, seen_functions, next); }
            out.push(']');
        }
        Value::Record(fields) => {
            out.push('{');
            for (name, value) in fields { structure_bytes(out, name.as_bytes()); structure_value(out, value, arena, seen_residuals, seen_functions, next); }
            out.push('}');
        }
        Value::Struct(name, ty, fields) | Value::Source(name, ty, fields) => {
            out.push(if matches!(value, Value::Struct(..)) { 'S' } else { 'Q' });
            structure_bytes(out, name.as_bytes());
            structure_bytes(out, ty::to_string(ty).as_bytes());
            for (name, value) in fields { structure_bytes(out, name.as_bytes()); structure_value(out, value, arena, seen_residuals, seen_functions, next); }
            out.push('}');
        }
        Value::Text(bytes) => { out.push('s'); structure_bytes(out, bytes); }
        value => structure_bytes(out, &value::key_of(&|_| 0, value)),
    }
}
pub fn structure(value: &Value) -> String {
    let _phase = flow_syntax::phase_timer::measure("structure");
    let mut output = String::new();
    let mut seen_residuals = HashMap::new();
    let mut seen_functions = HashMap::new();
    let mut next = 0;
    if let Some(arena) = arena_for_value(value) {
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        structure_value(&mut output, value, Some(&arena), &mut seen_residuals, &mut seen_functions, &mut next);
    } else {
        structure_value(&mut output, value, None, &mut seen_residuals, &mut seen_functions, &mut next);
    }
    stable_digest(output.as_bytes())
}
pub fn authored_key(workspace: &Workspace) -> String {
    let forms = workspace.defs.iter().map(|graph| &graph.form)
        .chain(workspace.macros.iter())
        .chain(workspace.nodes.iter().map(|node| &node.body.form))
        .filter(|form| form.id < flow_syntax::syntax::LIBRARY_BASE)
        .map(flow_syntax::lisp::flat)
        .collect::<Vec<_>>().join("\0");
    stable_digest(forms.as_bytes())
}
fn workspace_source_key(arena: &Arena) -> &str {
    arena.source_key.get_or_init(|| {
        let source = flow_syntax::lisp::text(&arena.workspace.source);
        stable_digest(source.as_bytes())
    })
}
pub fn element_key(zone:&Path)->String{format!("$elem:{}",zone.join("/"))}
fn arena_for_function(id: FnId) -> Option<Arc<Slot>> { slot_of(id.1) }
fn map_function(site: Option<(i64, Path, Vec<i64>)>, path: Option<Path>, signature: &FnSignature, function: FnId, arrays: &[Value]) -> Result<Value, Diagnostic> {
    let arena = arena_for_function(function).ok_or_else(|| error("E_FUNCTION_ID", "Function belongs to an expired evaluation."))?;
    let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
    let Function::Closure(closure) = get_fn(arena, function)? else { return eval_error("E_KERNEL_FORM", "A bulk function needs an instantiated local body."); };
    if arrays.len() != closure_params(&closure).len() { return eval_error("E_ARITY", "Bulk function inputs differ from its parameter count."); }
    if signature.params.len() != arrays.len() || !signature.params.iter().zip(arrays).all(|(ty, value)| value::ty_of(value) == Ty::Array(Box::new(ty.clone()))) {
        return eval_error("E_ARRAY_TYPE", "Bulk function columns must match their declared parameter types.");
    }
    if arrays.iter().any(|value| !matches!(value, Value::Int_array(_) | Value::Text_array(_) | Value::Float_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_))) {
        return eval_error("E_ARRAY_TYPE", "Bulk function inputs must be packed arrays.");
    }
    for array in arrays { value::validate(array)?; }
    let counts = arrays.iter().map(array_len).collect::<Result<Vec<_>, _>>()?;
    if counts.first().is_some_and(|first| counts.iter().any(|count| count != first)) { return eval_error("E_ARRAY_RANGE", "Bulk function inputs must have equal counts."); }
    if !ty::fits(closure.body.ty(), &signature.result) { return eval_error("E_KERNEL_FORM", "Bulk function body differs from its declared result type."); }
    let result = match &signature.result {
        Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text => signature.result.clone(),
        Ty::Record(_) if ty::numeric_record(&signature.result) => signature.result.clone(),
        _ => return eval_error("E_KERNEL_FORM", "Bulk functions must return int, float, vec2, vec3, vec4, text or a record of numbers."),
    };
    let mut builder = TermBuilder::new();
    let form = builder.import_form(&closure.body.form());
    let function_name = "$kernel-function".to_owned();
    let mut terms = vec![builder.push(None, &Ty::Fn(None), form, Draft::RefBinding(function_name.clone(), Vec::new()))];
    let mut env = BTreeMap::from([(function_name, Value::Fn(function))]);
    for (index, value) in arrays.iter().enumerate() {
        let name = format!("$kernel-input:{}", index);
        terms.push(builder.push(None, &value::ty_of(value), form, Draft::RefBinding(name.clone(), Vec::new())));
        env.insert(name, value.clone());
    }
    let mut context = closure.at.clone();
    if let Some((instance, prefix, iter)) = site { context.inst = instance; context.prefix = Arc::new(prefix); context.base = Arc::new(Vec::new()); context.route = Route(None); context.iter = Arc::new(iter); }
    context.data = true;
    context.record = false;
    let id = builder.push(path.as_deref(), &Ty::Array(Box::new(result)), form, Draft::Hof(W::HofKind::Map, terms));
    let term = builder.freeze().handle(id);
    Ok(Value::Residual(residual_id(arena, ResidualData { term: Arc::new(term), env: Arc::new(env), ctx: context, previous: false, live: None })))
}
pub fn map_kernel(site: Option<(i64, Path, Vec<i64>)>, path: Option<Path>, signature: &FnSignature, function: FnId, values: &[Value]) -> Result<Value, Diagnostic> {
    Host::map_function(site, path, signature, function, values)
}
pub fn bind_captures(function: FnId, replace: &dyn Fn(&Value) -> Value) -> FnId {
    let Some(arena) = arena_for_function(function) else { return function; };
    // Compiler bindings belong to a new closure: a declined kernel must leave the
    // original callable intact for sibling kernels and reference execution.
    let Some(Function::Closure(closure)) = peek(&arena, |arena| arena.functions.get(&function).cloned()) else { return function; };
    let bindings = closure.env.iter().map(|(name, value)| (name.clone(), replace(value))).collect();
    if *closure.env == bindings { return function; }
    let mut closure = unshare(closure);
    closure.env = Arc::new(bindings);
    peek(&arena, |arena| closure_id(arena, Function::Closure(Arc::new(closure))))
}
struct InterruptScope { id: std::thread::ThreadId, previous: Option<Interrupt> }
impl Drop for InterruptScope {
    fn drop(&mut self) {
        let mut interrupts = INTERRUPTS.lock().unwrap();
        match self.previous.take() { Some(previous) => { interrupts.insert(self.id, previous); }, None => { interrupts.remove(&self.id); } }
    }
}
pub fn with_interrupt(expired: Arc<dyn Fn() -> bool + Send + Sync>, f: &mut dyn FnMut()) -> bool {
    let id = std::thread::current().id();
    let parent = current_interrupt();
    let fired = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let observed = fired.clone();
    let probe: Interrupt = Arc::new(move || {
        if observed.load(Ordering::Relaxed) || parent.as_ref().is_some_and(|stop| stop()) || expired() { observed.store(true, Ordering::Relaxed); true } else { false }
    });
    let previous = INTERRUPTS.lock().unwrap().insert(id, probe);
    let _scope = InterruptScope { id, previous };
    f();
    !fired.load(Ordering::Relaxed)
}
/// Capture before dispatching work: workers inherit this probe and its fired latch.
pub fn current_interrupt() -> Option<Arc<dyn Fn() -> bool + Send + Sync>> {
    INTERRUPTS.lock().unwrap().get(&std::thread::current().id()).cloned()
}
pub fn interruptible(f: &mut dyn FnMut()) -> Result<(), Diagnostic> {
    if current_interrupt().is_some_and(|expired| expired()) { return Err(error("cancelled", "Evaluation interrupted.")); }
    f();
    Ok(())
}

struct HeldArray {
    data: Weak<Vec<f64>>,
    read: Arc<dyn Fn() -> Value + Send + Sync>,
}
static HELD_ARRAYS: LazyLock<Mutex<HashMap<usize, HeldArray>>> = LazyLock::new(|| Mutex::new(HashMap::new()));
pub mod Device {
    use super::*;
    pub fn add(placeholder: &Value, read: Arc<dyn Fn() -> Value + Send + Sync>) -> Result<(), Diagnostic> {
        let data = match placeholder {
            Value::Float_array(data) | Value::Vec2_array(data) | Value::Vec3_array(data) | Value::Vec4_array(data) => data,
            _ => return Err(error("E_INVALID_ARGUMENT", "Eval.Device.add")),
        };
        let key = Arc::as_ptr(data) as usize;
        let mut held = HELD_ARRAYS.lock().unwrap();
        held.retain(|_, entry| entry.data.strong_count() != 0);
        held.insert(key, HeldArray { data: Arc::downgrade(data), read });
        Ok(())
    }
    fn reader(data: &Arc<Vec<f64>>) -> Option<Arc<dyn Fn() -> Value + Send + Sync>> {
        if data.len() > 4 { return None; }
        let key = Arc::as_ptr(data) as usize;
        let mut held = HELD_ARRAYS.lock().unwrap();
        let entry = held.get(&key)?;
        if entry.data.upgrade().is_some_and(|owner| Arc::ptr_eq(&owner, data)) { return Some(entry.read.clone()); }
        held.remove(&key);
        None
    }
    pub fn thaw(value: &Value) -> Value {
        match value {
            Value::Float_array(data) | Value::Vec2_array(data) | Value::Vec3_array(data) | Value::Vec4_array(data) => reader(data).map_or_else(|| value.clone(), |read| read()),
            Value::List(values) => Value::List(Arc::new(values.iter().map(thaw).collect())),
            Value::Record(fields) => Value::Record(fields.iter().map(|(name, value)| (name.clone(), thaw(value))).collect()),
            Value::Struct(name, ty, fields) => Value::Struct(name.clone(), ty.clone(), fields.iter().map(|(name, value)| (name.clone(), thaw(value))).collect()),
            _ => value.clone(),
        }
    }
    pub fn held_by_device(value: &Value) -> bool {
        matches!(value, Value::Float_array(data) | Value::Vec2_array(data) | Value::Vec3_array(data) | Value::Vec4_array(data) if reader(data).is_some())
    }
}

fn same_array_storage(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Int_array(left), Value::Int_array(right)) => Arc::ptr_eq(left, right),
        (Value::Text_array(left), Value::Text_array(right)) => Arc::ptr_eq(left, right),
        (Value::Float_array(left), Value::Float_array(right)) => Arc::ptr_eq(left, right),
        (Value::Vec2_array(left), Value::Vec2_array(right)) => Arc::ptr_eq(left, right),
        (Value::Vec3_array(left), Value::Vec3_array(right)) => Arc::ptr_eq(left, right),
        (Value::Vec4_array(left), Value::Vec4_array(right)) => Arc::ptr_eq(left, right),
        _ => false,
    }
}
fn numeric_components(value: &Value) -> Option<Vec<f64>> {
    match value {
        Value::Float(value) => Some(vec![*value]),
        Value::Vec2(x, y) => Some(vec![*x, *y]),
        Value::Vec3(x, y, z) => Some(vec![*x, *y, *z]),
        Value::Vec4(x, y, z, w) => Some(vec![*x, *y, *z, *w]),
        _ => None,
    }
}
fn bulk_matches(expected: &Value, candidate: &Value, count: usize, head: usize, approximate: bool) -> bool {
    if value::ty_of(expected) != value::ty_of(candidate) || array_len(candidate).ok() != Some(count) { return false; }
    (0..head.min(count)).all(|index| {
        let (Ok(expected), Ok(candidate)) = (array_get(expected, index), array_get(candidate, index)) else { return false; };
        if !approximate { return value::key_of(&|_| 0, &expected) == value::key_of(&|_| 0, &candidate); }
        let (Some(expected), Some(candidate)) = (numeric_components(&expected), numeric_components(&candidate)) else { return false; };
        expected.iter().zip(candidate).all(|(expected, candidate)| (expected - candidate).abs() <= 1e-4 * (1.0 + expected.abs()))
    })
}
fn replace_array(value: &mut Value, original: &Value, replacement: &Value) {
    if same_array_storage(value, original) { *value = replacement.clone(); return; }
    match value {
        Value::List(values) => for value in Arc::make_mut(values).iter_mut() { replace_array(value, original, replacement); },
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => {
            for (_, value) in fields { replace_array(value, original, replacement); }
        }
        _ => (),
    }
}
fn replace_eval_value(evaluated: &mut Eval, original: &Value, replacement: &Value) {
    for (_, value) in &mut evaluated.results { replace_array(value, original, replacement); }
    for node in &mut evaluated.plan.nodes { for (_, value) in &mut node.args { replace_array(value, original, replacement); } }
    for instance in &mut evaluated.plan.instances {
        for (_, value) in &mut instance.inputs { replace_array(value, original, replacement); }
        replace_array(&mut instance.result, original, replacement);
    }
    for (_, values) in &mut evaluated.records { for (_, value) in values { replace_array(value, original, replacement); } }
    for value in &mut evaluated.states { replace_array(value, original, replacement); }
    let mut cell = held(&evaluated.owner); let arena = cell.as_mut().expect("filled");
    for node in arena.nodes.iter_mut() { for (_, value) in &mut node.args { replace_array(value, original, replacement); } }
    for cell in arena.cells.iter_mut() {
        for (_, value) in &mut cell.inputs { replace_array(value, original, replacement); }
        replace_array(&mut cell.result, original, replacement);
    }
    for values in arena.records.values_mut() { for (_, value) in values { replace_array(value, original, replacement); } }
    for value in arena.states.iter_mut() { replace_array(value, original, replacement); }
}
pub mod Residual {
    use super::*;
    #[derive(Clone, Debug)]
    pub struct View {
        pub term: Arc<Term>, pub bindings: Vec<(String, Value)>, pub site: Path, pub owner: Path,
        pub iter: Vec<i64>, pub instance: i64, pub prefix: Path, pub previous: bool,
        pub host: bool, pub geometry: bool, pub context: Context,
    }
    pub type ResidualView = View;
    pub fn handle(handlers: ResidualHandlers, run: &mut dyn FnMut() -> Result<Eval, Diagnostic>) -> Result<Eval, Diagnostic> {
        ACTIVE_RESIDUAL_HANDLERS.with(|active| active.borrow_mut().push(handlers.clone()));
        ACTIVE_OBSERVED_RESIDUALS.with(|observed| observed.borrow_mut().push(Vec::new()));
        let scope = ResidualHandlerScope;
        let result = run();
        let observed = ACTIVE_OBSERVED_RESIDUALS.with(|queues| queues.borrow_mut().last_mut().map(std::mem::take).unwrap_or_default());
        if let Some(observe) = &handlers.observe { for residual in observed { observe(residual); } }
        drop(scope);
        result
    }
    pub fn free_names(term: &Term) -> Vec<String> { super::free_names(term).into_iter().collect() }
    pub fn residual_id(id: ResidualId) -> i64 { id.0 }
    /// Cheap single-field reads (no ResidualData / Term clone) for the lowering's hot predicates.
    pub fn residual_context(id: ResidualId) -> Option<Context> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.residuals.get(&id).map(|r| r.ctx.context.clone())
    }
    pub fn residual_ty(id: ResidualId) -> Option<Ty> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.residuals.get(&id).map(|r| r.term.ty().clone())
    }
    pub fn residual_term(id: ResidualId) -> Option<Arc<Term>> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.residuals.get(&id).map(|r| r.term.clone())
    }
    pub fn residual_bindings(id: ResidualId) -> Option<Vec<(String, Value)>> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.residuals.get(&id).map(|r| r.env.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
    }
    pub fn residual_binding(id: ResidualId, name: &str) -> Option<Value> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.residuals.get(&id)?.env.get(name).cloned()
    }
    pub fn residual_view(id: ResidualId) -> Result<View, Diagnostic> {
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        let residual = own_res(arena, &id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        Ok(View {
            term: residual.term.clone(), bindings: residual.env.iter().map(|(name, value)| (name.clone(), value.clone())).collect(), site: site(&residual.ctx),
            owner: (*residual.ctx.base).clone(), iter: (*residual.ctx.iter).clone(), instance: residual.ctx.inst,
            prefix: (*residual.ctx.prefix).clone(), previous: residual.previous, host: residual.ctx.host,
            geometry: residual.ctx.geometry, context: residual.ctx.context.clone(),
        })
    }
    pub fn bind_live(id: ResidualId, live: &Frame, state: Option<&State>) -> Result<(), Diagnostic> {
        fail_frame(live)?;
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        // a bind reached from a callback of this evaluation binds the residual only: the evaluation keeps its own frame
        if arena.lent == 0 {
            arena.time = Some(live.clone());
            arena.state = state.cloned().or(arena.state.clone());
        }
        if let Some(residual) = arena.residuals.get_mut(&id) { residual.live = Some(live.clone()); }
        Ok(())
    }
    pub fn function_bindings(id: FnId) -> Vec<(String, Value)> {
        arena_for_function(id).and_then(|arena| {
            let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
            match arena.functions.get(&id)? { Function::Closure(closure) => Some(closure.env.iter().map(|(k, v)| (k.clone(), v.clone())).collect()), Function::Named(_) => Some(Vec::new()) }
        }).unwrap_or_default()
    }
    pub fn function_id(id: FnId) -> i64 { id.0 }
    pub fn function_body(id: FnId) -> Option<(Vec<(W::Pattern, Option<Ty>)>, Term)> {
        let arena = arena_for_function(id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        match arena.functions.get(&id)? { Function::Closure(closure) => Some((function_params(&closure.params.0, closure.params.1), (*closure.body).clone())), Function::Named(_) => None }
    }
    pub fn function_origin(id: FnId) -> Option<(i64, Path, Path)> {
        let arena = arena_for_function(id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        match arena.functions.get(&id)? { Function::Closure(closure) => Some((closure.at.inst, (*closure.at.prefix).clone(), closure.zone.clone())), Function::Named(_) => None }
    }
    pub fn function_name(id: FnId) -> Option<String> {
        let arena = arena_for_function(id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        match arena.functions.get(&id)? { Function::Named(function) => Some(function.name.clone()), Function::Closure(_) => None }
    }
    pub fn residual_definition(id: ResidualId, name: &str) -> Option<Graph> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.workspace.defs.iter().find(|graph| graph.name == name).cloned()
    }
    pub fn residual_graph(id: ResidualId, name: &str) -> Option<Graph> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.workspace.graphs.iter().find(|graph| graph.name == name).cloned()
    }
    pub fn graph_instance(id: ResidualId, name: &str) -> Option<i64> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        arena.cache.get(&format!("{}|", name)).map(|index| arena.cells[*index].instance)
    }
    pub fn checkpoint(id: ResidualId) -> Result<Box<dyn Fn()>, Diagnostic> {
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let state = peek(&arena, |arena| arena.state.clone()).ok_or_else(|| error("E_STATE", "State is not available."))?;
        let saved = fork_state(&state);
        Ok(Box::new(move || { *state.inner.lock().unwrap() = saved.inner.lock().unwrap().clone(); }))
    }
    pub fn rerun(id: ResidualId, on_walk: Option<&dyn Fn()>) -> Result<Value, Diagnostic> {
        if let Some(on_walk) = on_walk { on_walk(); }
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut arena = checkout(&arena);
        let residual = get_res(&arena, id)?;
        let saved_live = save_live(&mut arena);
        if arena.time.is_none() {
            if let Some(live) = &residual.live { arena.time = Some(live.clone()); }
            else { restore_live(&mut arena, saved_live); return eval_error("E_FRAME", "Bind a live frame before rerunning this residual."); }
        }
        let result = execute_residual(&mut arena, id);
        restore_live(&mut arena, saved_live);
        result
    }
    fn state_location(residual: &ResidualData, instance: i64, path: Path, iter: Vec<i64>, host: bool) -> ExecCtx {
        let mut context = residual.ctx.clone();
        context.inst = instance; context.prefix = Arc::new(path); context.iter = Arc::new(iter); context.host = host;
        context
    }
    pub fn state_read(id: ResidualId, instance: i64, path: Path, iter: Vec<i64>, host: bool, init: &dyn Fn() -> Value) -> Result<Value, Diagnostic> {
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        let residual = get_res(&arena, id)?;
        let context = state_location(&residual, instance, path, iter, host);
        let (state, frame) = state_plane(&arena, &context)?;
        let source = workspace_source_key(arena);
        let key = state_key(&context, &[]);
        let previous = {
            let mut state = state.inner.lock().unwrap();
            if host {
                let plane = state.host.get_or_insert_with(StatePlane::default);
                begin_host_frame(plane, &source, frame);
                plane.before.get(&key).cloned()
            } else {
                begin_frame(&mut state, &source, frame);
                state.before.get(&key).cloned()
            }
        };
        drop(cell);
        state_data(previous.unwrap_or_else(init))
    }
    pub fn state_peek(id: ResidualId, instance: i64, path: Path, iter: Vec<i64>, host: bool) -> Option<Value> {
        let arena = arena_for_residual(&id)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        let residual = get_res(&arena, id).ok()?;
        let context = state_location(&residual, instance, path, iter, host);
        let state = arena.state.as_ref()?.inner.lock().ok()?;
        let value = if host { state.host.as_ref()?.next.get(&state_key(&context, &[])).cloned() } else { state.next.get(&state_key(&context, &[])).cloned() }?;
        Some(value)
    }
    pub fn state_write(id: ResidualId, instance: i64, path: Path, iter: Vec<i64>, host: bool, value: Value) -> Result<Value, Diagnostic> {
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut arena = checkout(&arena);
        let residual = get_res(&arena, id)?;
        let context = state_location(&residual, instance, path, iter, host);
        super::state_write(&mut arena, &context, &Vec::new(), value)
    }
    // the workspace's operator table, shared (a lowering asks for it per operator: copying the catalog each time was most of a call)
    pub fn residual_ops(id: ResidualId) -> Arc<Vec<op::Op>> {
        arena_for_residual(&id).map(|arena| peek(&arena, |arena| arena.workspace.ops.clone())).unwrap_or_default()
    }
    pub fn residual_authored_key(id: ResidualId) -> Option<String> {
        arena_for_residual(&id).map(|arena| peek(&arena, |arena| authored_key(&arena.workspace)))
    }
    pub fn residual_eval(id: ResidualId, live: &Frame, state: Option<&State>) -> Result<Value, Diagnostic> { super::residual_eval(id, live, state) }
    pub fn eval_term(id: ResidualId, term: &Term, live: &Frame, state: Option<&State>, elems: Option<&[(String, Value)]>, resolve: Option<Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>>) -> Result<Value, Diagnostic> {
        let _nesting = EvalNesting::enter()?;
        check_term_nesting(term)?;
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut arena = checkout(&arena);
        fail_frame(live)?;
        let residual = get_res(&arena, id)?;
        let saved_live = save_live(&mut arena);
        let previous = arena.state.clone();
        let prior_time = std::mem::replace(&mut arena.time, Some(live.clone()));
        arena.state = state.cloned().or(arena.state.clone());
        arena.elems = elems.unwrap_or(&[]).iter().cloned().collect();
        arena.resolve = resolve;
        // OCaml's with_live builds a fresh live state per call: no memo, step count or call counts survive a previous time.
        arena.step_count = 0;
        arena.fn_calls.clear();
        arena.memo.clear();
        let saved = arena.state.as_ref().map(fork_state);
        let result = eval_concrete(&mut arena, &residual.ctx, &Env::of(residual.env), term);
        // a term evaluated at its own frame (a compile-time capture) must not move the frame a bound live walk is on
        if prior_time.is_some() { arena.time = prior_time; }
        if result.is_err() { if let (Some(state), Some(saved)) = (arena.state.as_ref(), saved.as_ref()) { restore_state(state, saved); } }
        arena.state = previous;
        restore_live(&mut arena, saved_live);
        result
    }
    pub fn with_bindings(id: ResidualId, term: &Term, bindings: &[(String, Value)]) -> Result<ResidualId, Diagnostic> {
        check_term_nesting(term)?;
        for (_, value) in bindings { check_value_nesting(value)?; }
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        let mut residual = get_res(&arena, id)?;
        for (name, value) in bindings { Arc::make_mut(&mut residual.env).insert(name.clone(), value.clone()); }
        residual.term = Arc::new(term.clone());
        let free = free_names(term);
        Arc::make_mut(&mut residual.env).retain(|name, _| free.contains(name));
        residual.live = None;
        Ok(super::residual_id(arena, residual))
    }
    pub fn compiled_function(id: ResidualId, instance: i64, prefix: Path, zone: Path, params: Vec<(W::Pattern, Option<Ty>)>, body: Term, bindings: Vec<(String, Value)>) -> Result<FnId, Diagnostic> {
        check_term_nesting(&body)?;
        for (_, value) in &bindings { check_value_nesting(value)?; }
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        let residual = get_res(&arena, id)?;
        let mut context = residual.ctx;
        context.inst = instance;
        context.prefix = Arc::new(prefix);
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&body.form());
        let body_id = builder.import(&body);
        let function = builder.push(None, &Ty::Fn(None), form, Draft::Fn { params, body: body_id, zone: zone.clone(), capture: None });
        let function = builder.freeze().handle(function);
        let TermNode::Fn(node) = function.node else { unreachable!() };
        let params = (function.clone(), function.arena().function(node).params);
        let body = function.child(body_id);
        let closure = ClosureData { params, replayable: true, body: Arc::new(body), env: Arc::new(bindings.into_iter().collect()), zone, calls: 0, at: context };
        Ok(closure_id(arena, Function::Closure(Arc::new(closure))))
    }
}

pub type RetargetResidual = dyn Fn(&Term, &[(String, Value)]) -> Option<(Term, Vec<(String, Value)>)>;
fn arena_for_value(value: &Value) -> Option<Arc<Slot>> {
    match value {
        Value::Residual(id) => arena_for_residual(id),
        Value::Fn(id) => arena_for_function(*id),
        Value::List(values) => values.iter().find_map(arena_for_value),
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().find_map(|(_, value)| arena_for_value(value)),
        _ => None,
    }
}
// A rewrite runs the host's `replace`, `fresh` and `retarget` between short accesses of the value's home arena and holds none across them, so
// a callback may rewrite again (a folded write copying an earlier one): each copy gets its own fresh id, as in OCaml.
// A bound kernel owns a fresh capture arena; dropping it releases that frame's arrays,
// including closures evaluation creates while preparing the bound inputs.
fn rewrite_home(home: &Arc<Slot>, owned: &mut Option<HashMap<i64, Arc<Slot>>>) -> Arc<Slot> {
    let Some(homes) = owned else { return home.clone(); };
    homes.entry(home.id).or_insert_with(|| {
        let id = NEXT_ARENA.fetch_add(1, Ordering::Relaxed);
        let arena = peek(home, |a| {
            let mut copy = Arena::fresh(id, a.workspace.clone(), false, a.budget, current_interrupt(), true);
            copy.source_key = a.source_key.clone();
            copy
        });
        let slot = Arc::new(Slot { id, cell: Mutex::new(Some(arena)), returned: Condvar::new() });
        ARENAS.lock().unwrap().insert(id, Arc::downgrade(&slot));
        slot
    }).clone()
}
fn rewrite_value(
    value: &Value, replace: &dyn Fn(&Value) -> Option<Value>, fresh: &dyn Fn() -> i64,
    retarget: &RetargetResidual, residuals: &mut HashMap<(i64, i64), ResidualId>, functions: &mut HashMap<(i64, i64), FnId>,
    owned: &mut Option<HashMap<i64, Arc<Slot>>>,
) -> Value {
    if let Some(value) = replace(value) { return value; }
    match value {
        Value::Residual(id) => {
            let key = (id.1, id.0);
            if let Some(copy) = residuals.get(&key) { return Value::Residual(*copy); }
            let Some(home) = slot_of(id.1) else { return value.clone(); };
            let Some(mut residual) = peek(&home, |arena| arena.residuals.get(id).cloned()) else { return value.clone(); };
            let original_bindings = residual.env.iter().map(|(name, value)| (name.clone(), value.clone())).collect::<Vec<_>>();
            // an unretargeted copy shares its term (one term, not one per copy along a write chain)
            let (term, overrides) = retarget(&residual.term, &original_bindings).map(|(term, overrides)| (Arc::new(term), overrides)).unwrap_or_else(|| (residual.term.clone(), Vec::new()));
            let target = rewrite_home(&home, owned);
            let copy = ResidualId(fresh(), target.id);
            residuals.insert(key, copy);
            residual.term = term;
            let names = overrides.iter().map(|(name, _)| name.clone()).collect::<BTreeSet<_>>();
            let mut env = BTreeMap::new();
            for (name, value) in residual.env.iter() {
                if names.contains(name) { env.insert(name.clone(), value.clone()); }
                else { env.insert(name.clone(), rewrite_value(value, replace, fresh, retarget, residuals, functions, owned)); }
            }
            env.extend(overrides);
            residual.env = Arc::new(env);
            peek(&target, |arena| { arena.residuals.insert(copy, residual); });
            Value::Residual(copy)
        }
        Value::Fn(id) => {
            let key = (id.1, id.0);
            if let Some(copy) = functions.get(&key) { return Value::Fn(*copy); }
            let Some(home) = slot_of(id.1) else { return value.clone(); };
            let Some(Function::Closure(closure)) = peek(&home, |arena| arena.functions.get(id).cloned()) else { return value.clone(); };
            let mut closure = unshare(closure);
            let target = rewrite_home(&home, owned);
            let copy = FnId(fresh(), target.id);
            functions.insert(key, copy);
            peek(&target, |arena| { arena.functions.insert(copy, Function::Closure(Arc::new(closure.clone()))); });
            closure.env = Arc::new(Arc::unwrap_or_clone(closure.env).into_iter().map(|(name, value)| (name, rewrite_value(&value, replace, fresh, retarget, residuals, functions, owned))).collect());
            peek(&target, |arena| replace_fn(arena, copy, Function::Closure(Arc::new(closure))));
            Value::Fn(copy)
        }
        Value::List(values) => Value::List(Arc::new(values.iter().map(|value| rewrite_value(value, replace, fresh, retarget, residuals, functions, owned)).collect())),
        Value::Record(fields) => Value::Record(fields.iter().map(|(name, value)| (name.clone(), rewrite_value(value, replace, fresh, retarget, residuals, functions, owned))).collect()),
        Value::Struct(name, ty, fields) => Value::Struct(name.clone(), ty.clone(), fields.iter().map(|(name, value)| (name.clone(), rewrite_value(value, replace, fresh, retarget, residuals, functions, owned))).collect()),
        Value::Source(name, ty, fields) => Value::Source(name.clone(), ty.clone(), fields.iter().map(|(name, value)| (name.clone(), rewrite_value(value, replace, fresh, retarget, residuals, functions, owned))).collect()),
        _ => value.clone(),
    }
}
fn remap_value(arena: &mut Arena, value: &Value, sources: &[(i64, i64)], residuals: &mut HashSet<ResidualId>, functions: &mut HashSet<FnId>) -> Value {
    match value {
        Value::Deferred(ty, id) if flow_syntax::ty::is_geometry(ty) => Value::Deferred(ty.clone(), sources.iter().find(|(source, _)| source == id).map_or(*id, |(_, target)| *target)),
        Value::Residual(id) => {
            if id.1 != arena.id || !residuals.insert(*id) { return value.clone(); }
            if let Some(mut residual) = arena.residuals.get(id).cloned() {
                residual.env = Arc::new(residual.env.iter().map(|(name, value)| (name.clone(), remap_value(arena, value, sources, residuals, functions))).collect());
                arena.residuals.insert(*id, residual);
            }
            value.clone()
        }
        Value::Fn(id) => {
            if id.1 != arena.id || !functions.insert(*id) { return value.clone(); }
            if let Some(Function::Closure(closure)) = arena.functions.get(id).cloned() {
                let mut closure = unshare(closure);
                closure.env = Arc::new(Arc::unwrap_or_clone(closure.env).into_iter().map(|(name, value)| (name, remap_value(arena, &value, sources, residuals, functions))).collect());
                arena.functions.insert(*id, Function::Closure(Arc::new(closure)));
            }
            value.clone()
        }
        Value::List(values) => Value::List(Arc::new(values.iter().map(|value| remap_value(arena, value, sources, residuals, functions)).collect())),
        Value::Record(fields) => Value::Record(fields.iter().map(|(name, value)| (name.clone(), remap_value(arena, value, sources, residuals, functions))).collect()),
        Value::Struct(name, ty, fields) => Value::Struct(name.clone(), ty.clone(), fields.iter().map(|(name, value)| (name.clone(), remap_value(arena, value, sources, residuals, functions))).collect()),
        Value::Source(name, ty, fields) => Value::Source(name.clone(), ty.clone(), fields.iter().map(|(name, value)| (name.clone(), remap_value(arena, value, sources, residuals, functions))).collect()),
        _ => value.clone(),
    }
}
pub mod Host {
    use super::*;
    pub fn free_name_walks() -> usize { FREE_WALKS.load(Ordering::Relaxed) as usize }
    pub fn static_with_requests(budget: Option<usize>, record: bool, inputs: Option<&[(String, Vec<(String, Value)>)]>, workspace: &Workspace) -> Result<Eval, Diagnostic> {
        static_with(workspace, record, inputs, budget)
    }
    pub fn map_function(site: Option<(i64, Path, Vec<i64>)>, path: Option<Path>, signature: &FnSignature, function: FnId, arrays: &[Value]) -> Result<Value, Diagnostic> {
        super::map_function(site, path, signature, function, arrays)
    }
    pub fn map_function_bindings(function: FnId, replace: &dyn Fn(&Value) -> Value) -> FnId {
        let Some(arena) = arena_for_function(function) else { return function; };
        // `replace` is the host's: it runs between two accesses, holding nothing
        if let Some(Function::Closure(closure)) = peek(&arena, |arena| arena.functions.get(&function).cloned()) {
            let mut closure = unshare(closure);
            closure.env = Arc::new(Arc::unwrap_or_clone(closure.env).into_iter().map(|(name, value)| (name, replace(&value))).collect());
            peek(&arena, |arena| replace_fn(arena, function, Function::Closure(Arc::new(closure))));
        }
        function
    }
    pub fn rewrite(value: &Value, replace: &dyn Fn(&Value) -> Option<Value>) -> Value {
        rewrite_with(value, replace, &|| NEXT_BULK.fetch_add(-1, Ordering::Relaxed), &|_, _| None)
    }
    /// Bind temporary compiler captures outside the source evaluation's lifetime.
    pub fn rewrite_owned(value: &Value, replace: &dyn Fn(&Value) -> Option<Value>) -> (Value, Roots) {
        let mut homes = Some(HashMap::new());
        let value = rewrite_value(value, replace, &|| NEXT_BULK.fetch_add(-1, Ordering::Relaxed), &|_, _| None, &mut HashMap::new(), &mut HashMap::new(), &mut homes);
        let roots = roots(std::iter::once(&value));
        (value, roots)
    }
    pub fn rewrite_with(value: &Value, replace: &dyn Fn(&Value) -> Option<Value>, fresh: &dyn Fn() -> i64, retarget: &RetargetResidual) -> Value {
        rewrite_value(value, replace, fresh, retarget, &mut HashMap::new(), &mut HashMap::new(), &mut None)
    }
    pub fn remap_function_geometry(sources: &[(i64, i64)], function: FnId) -> FnId {
        let Some(arena) = arena_for_function(function) else { return function; };
        peek(&arena, |arena| remap_value(arena, &Value::Fn(function), sources, &mut HashSet::new(), &mut HashSet::new()));
        function
    }
    pub fn state_values(state: &State) -> Vec<Value> { super::state_values(state) }
    pub fn function_scope(function: FnId) -> Option<(Path, Vec<i64>)> {
        let arena = arena_for_function(function)?;
        let mut cell = held(&arena); let arena = cell.as_mut().expect("filled");
        match arena.functions.get(&function)? {
            Function::Closure(closure) => {
                let mut path = (*closure.at.prefix).clone();
                path.extend(closure.zone.clone());
                Some((path, (*closure.at.iter).clone()))
            }
            Function::Named(_) => None,
        }
    }
    pub fn force_reference(value: &Value, live: &Frame, state: Option<&State>, elems: Option<&[(String, Value)]>, resolve: Option<Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>>) -> Result<Value, Diagnostic> {
        force_with(value, live, state, elems, resolve)
    }
    pub fn force_reference_observed(value: &Value, live: &Frame, state: Option<&State>, elems: Option<&[(String, Value)]>, resolve: Option<Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>>, on_walk: &dyn Fn()) -> Result<Value, Diagnostic> {
        if is_live_value(value) { on_walk(); }
        force_with(value, live, state, elems, resolve)
    }
    pub fn map_probe(
        id: ResidualId, live: &Frame, state: Option<&State>,
        resolve: Option<Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>>,
        execute: Option<Arc<dyn Fn(ResidualId) -> Result<Value, Diagnostic> + Send + Sync>>,
        offset: usize,
    ) -> Result<(usize, Box<dyn Fn(usize) -> Result<Records, Diagnostic>>), Diagnostic> {
        let arena = arena_for_residual(&id).ok_or_else(|| error("E_RESIDUAL_ID", "Residual belongs to an expired evaluation."))?;
        fail_frame(live)?;
        if let Some(execute) = execute { let _ = execute(id)?; }
        let (context, function, lists, count) = {
            let mut data = checkout(&arena);
            let saved_live = save_live(&mut data);
            data.time = Some(live.clone());
            data.state = state.cloned().or(data.state.clone());
            data.resolve = resolve;
            let probed = (|| -> Result<_, Diagnostic> {
            let residual = get_res(&data, id)?;
            let TermNode::Hof(id) = residual.term.node else { return Err(error("E_TYPE", "A map probe needs a checked map.")); };
            let hof = residual.term.arena().hof(id);
            if hof.kind != W::HofKind::Map { return Err(error("E_TYPE", "A map probe needs a checked map.")); }
            let terms = residual.term.arena().term_ids(hof.args);
            let Some(function_term) = terms.first() else { return Err(error("E_TYPE", "A map probe needs a checked map.")); };
            let Value::Fn(function) = eval_concrete(&mut data, &residual.ctx, &Env::of(residual.env.clone()), &residual.term.child(*function_term))? else { return Err(error("E_TYPE", "Expected a function.")); };
            let env = Env::of(residual.env.clone());
            let mut lists = Vec::with_capacity(terms.len().saturating_sub(1));
            for (index, term) in terms.iter().skip(1).enumerate() { lists.push(eval_sequence(&mut data, &residual.ctx, &env, &residual.term.child(*term), Seg::Index("l", index))?.0); }
            let count = if lists.is_empty() { 0 } else { lists.iter().map(v_len).min().unwrap_or(0) };
            Ok((residual.ctx, function, lists, count))
            })();
            restore_live(&mut data, saved_live);
            probed?
        };
        let arena = arena.clone();
        let at = move |index: usize| -> Result<Records, Diagnostic> {
            if index >= count { return Err(error("E_LIST_RANGE", "Probe index is outside this map.")); }
            let mut worker = peek(&arena, |arena| arena.clone());
            worker.interrupt = current_interrupt();
            worker.owns_index = false;
            worker.record = true;
            worker.records.clear();
            worker.fn_calls.clear();
            worker.memo.clear();
            worker.step_count = 0;
            worker.fn_calls.insert(function, (offset + index) as i64);
            let mut context = context.clone();
            context.record = true;
            let args = lists.iter().map(|value| sequence_item(value, index)).collect::<Result<Vec<_>, _>>()?;
            let value = call_fn(&mut worker, &context, function, &args, None)?;
            let _ = concrete(&mut worker, value)?;
            let mut records = std::mem::take(&mut *worker.records).into_iter().collect::<Records>();
            records.sort_by(|left, right| left.0.cmp(&right.0));
            Ok(records)
        };
        Ok((count, Box::new(at)))
    }
}
pub fn static_source_get()->Arc<dyn Fn(&dyn Fn(i64)->Option<Node>,&Value)->Option<Value>+Send+Sync>{SOURCE_HOOK.read().unwrap().clone()}
pub fn set_static_source(f:Arc<dyn Fn(&dyn Fn(i64)->Option<Node>,&Value)->Option<Value>+Send+Sync>){*SOURCE_HOOK.write().unwrap()=f}
pub fn set_static_facts(f:Arc<dyn Fn(&dyn Fn(i64)->Option<Node>,i64)->String+Send+Sync>){*FACTS_HOOK.write().unwrap()=f}
pub fn set_live_kernels(f:Arc<dyn Fn(&[Node])->bool+Send+Sync>){*LIVE_KERNELS_HOOK.write().unwrap()=f}
pub fn static_facts_get() -> Arc<dyn Fn(&dyn Fn(i64) -> Option<Node>, i64) -> String + Send + Sync> { FACTS_HOOK.read().unwrap().clone() }
pub fn live_kernels_get() -> Arc<dyn Fn(&[Node]) -> bool + Send + Sync> { LIVE_KERNELS_HOOK.read().unwrap().clone() }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inline_relocation_shares_unchanged_environments_and_rewrites_nested_ids() {
        let reloc = Reloc {
            lo: 10, count: 2, new_lo: 20, inputs: vec![(3, 30)],
            rmap: HashMap::from([(ResidualId(1, 1), ResidualId(2, 1))]),
            fmap: HashMap::from([(FnId(7, 1), FnId(8, 1))]),
            site: Vec::new(), iter: Vec::new(), new_site: Vec::new(), new_iter: Vec::new(), arena: 1, inst: 1,
        };
        let values = Arc::new(vec![Value::Float(f64::NAN), Value::Residual(ResidualId(99, 1))]);
        let env = Arc::new(BTreeMap::from([("pure".into(), Value::List(values.clone()))]));
        assert!(Arc::ptr_eq(&env, &reloc.env(&env)));
        let Value::List(shared) = reloc.value(&Value::List(values.clone())) else { panic!("list") };
        assert!(Arc::ptr_eq(&values, &shared));
        let nested = |node, input, residual, function| Value::List(Arc::new(vec![
            Value::Record(vec![("node".into(), Value::Deferred(Ty::Int, node))]),
            Value::Struct("s".into(), Ty::Any, vec![("input".into(), Value::Deferred(Ty::Int, input))]),
            Value::Source("host".into(), Ty::Any, vec![("residual".into(), Value::Residual(residual)), ("fn".into(), Value::Fn(function))]),
            Value::Deferred(Ty::Int, 5),
        ]));
        let env = Arc::new(BTreeMap::from([("nested".into(), nested(10, 3, ResidualId(1, 1), FnId(7, 1)))]));
        let rewritten = reloc.env(&env);
        assert!(!Arc::ptr_eq(&env, &rewritten));
        assert_eq!(rewritten["nested"], nested(20, 30, ResidualId(2, 1), FnId(8, 1)));
        assert_eq!(env["nested"], nested(10, 3, ResidualId(1, 1), FnId(7, 1)), "the captured environment stays immutable");
    }

    fn form() -> flow_syntax::syntax::Syntax {
        flow_syntax::syntax::make(flow_syntax::syntax::Draft::Sym("test".into()))
    }
    fn workspace(body: Term) -> Workspace {
        let graph_form = form();
        Workspace {
            name: "test".into(),
            graphs: vec![Graph { name: "g".into(), context: flow_syntax::context::value(), inputs: Vec::new(), body, form: graph_form.clone() }].into(),
            defs: Vec::new().into(), nodes: Vec::new().into(), macros: Vec::new().into(), source: vec![graph_form].into(),
            live: BTreeSet::new(), invariant: BTreeSet::new(), approx: BTreeSet::new(),
            packed: Vec::new().into(), packed_roots: Vec::new(), approx_reasons: Vec::new().into(),
            kind_fns: Vec::new(), ops: Vec::new().into(),
        }
    }
    #[test]
    fn id_maps_hash_sequential_ids_and_replace_by_identity() {
        use std::hash::BuildHasher;

        let mut functions: IdMap<FnId, i64> = IdMap::default();
        let mut residuals: IdMap<ResidualId, i64> = IdMap::default();
        let mut function_hashes = HashSet::new();
        let mut residual_hashes = HashSet::new();
        let arena = 7;
        for id in 1..=64 {
            let function = FnId(id, arena);
            let residual = ResidualId(id, arena);
            function_hashes.insert(functions.hasher().hash_one(function));
            residual_hashes.insert(residuals.hasher().hash_one(residual));
            assert_eq!(functions.insert(function, id), None);
            assert_eq!(residuals.insert(residual, id), None);
        }
        assert_eq!(function_hashes.len(), 64);
        assert_eq!(residual_hashes.len(), 64);
        // The home arena locates values, but identity (and therefore hashing) uses only .0.
        for id in 1..=64 {
            assert_eq!(functions.get(&FnId(id, arena)), Some(&id));
            assert_eq!(residuals.get(&ResidualId(id, arena)), Some(&id));
            assert_eq!(functions.insert(FnId(id, arena + 1), -id), Some(id));
            assert_eq!(residuals.insert(ResidualId(id, arena + 1), -id), Some(id));
            assert_eq!(functions.get(&FnId(id, arena)), Some(&-id));
            assert_eq!(residuals.get(&ResidualId(id, arena)), Some(&-id));
        }
        assert_eq!(functions.len(), 64);
        assert_eq!(residuals.len(), 64);
        assert_eq!(functions.get(&FnId(65, arena)), None);
        assert_eq!(residuals.get(&ResidualId(65, arena)), None);
    }
    #[test]
    fn nesting_budget_restores_after_errors_and_unwinding() {
        let scopes = (0..MAX_EVAL_NESTING).map(|_| EvalNesting::enter().unwrap()).collect::<Vec<_>>();
        assert!(matches!(EvalNesting::enter(), Err(d) if d.code == "E_DEPTH"));
        drop(scopes);
        let _ = std::panic::catch_unwind(|| { let _scope = EvalNesting::enter().unwrap(); panic!("host unwound"); });
        EVAL_NESTING.with(|depth| assert_eq!(depth.get(), 0));
    }
    #[test]
    fn hash_is_stable_and_in_unit_interval() {
        let result = hash(&[1.0, 2.0]);
        assert_eq!(result, hash(&[1.0, 2.0]));
        assert!((0.0..1.0).contains(&result));
    }
    #[test]
    fn formatting_preserves_shared_byte_value_output() {
        assert_eq!(show(&Value::Int(7)), b"7");
        assert_eq!(show(&Value::Bool(true)), b"true");
        assert_eq!(show(&Value::Text(vec![0xff])), vec![0xff]);
    }
    #[test]
    fn checked_workspace_if_runs_only_its_selected_branch() {
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&form());
        let test = builder.push(None, &Ty::Bool, form, Draft::Lit(param::Value::Bool(true)));
        let yes = builder.push(None, &Ty::Int, form, Draft::Lit(param::Value::Int(42)));
        let no = builder.push(None, &Ty::Int, form, Draft::Lit(param::Value::Int(7)));
        let syntax = builder.syntax_builder();
        let name = syntax.intern("test");
        let authored = syntax.push(12, flow_syntax::syntax::Node::Sym(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
        let root = builder.push(Some(&["g".into(), "@result".into()]), &Ty::Int, authored, Draft::If(test, yes, no));
        let body = builder.freeze().handle(root);
        assert_eq!(static_eval(&workspace(body)).unwrap().results, vec![("g".into(), Value::Int(42))]);
    }
    #[test]
    fn checked_workspace_loop_binds_each_item_in_order() {
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&form());
        let one = builder.push(None, &Ty::Int, form, Draft::Lit(param::Value::Int(1)));
        let two = builder.push(None, &Ty::Int, form, Draft::Lit(param::Value::Int(2)));
        let source = builder.push(None, &Ty::List(Box::new(Ty::Int)), form, Draft::ListLit(vec![one, two]));
        let body = builder.push(Some(&["g".into(), "values".into(), "@result".into()]), &Ty::Int, form, Draft::RefBinding("i".into(), Vec::new()));
        let root = builder.push(Some(&["g".into(), "values".into()]), &Ty::List(Box::new(Ty::Int)), form, Draft::Loop {
            kind: W::LoopKind::For, accs: Vec::new(), clauses: vec![(W::Pattern::Name("i".into()), source)],
            skip: Vec::new(), body, zone: vec!["g".into(), "values".into()],
        });
        let term = builder.freeze().handle(root);
        assert_eq!(static_eval(&workspace(term)).unwrap().results, vec![("g".into(), Value::List(Arc::new(vec![Value::Int(1), Value::Int(2)])))]);
    }
    #[test]
    fn packed_term_cache_keys_distinguish_owners_with_identical_forms() {
        let make = || {
            let mut builder = TermBuilder::new();
            let form = builder.import_form(&form());
            let row = builder.push(Some(&["g".into()]), &Ty::Int, form, Draft::Lit(param::Value::Int(7)));
            builder.freeze().handle(row)
        };
        let left = make();
        let right = make();
        assert_eq!(left.form().id, right.form().id);
        assert_ne!(left.identity(), right.identity());
        let mut cache: AddrMap<i32> = AddrMap::default();
        cache.insert(left.identity(), 1);
        cache.insert(right.identity(), 2);
        assert_eq!(cache.get(&left.identity()), Some(&1));
        assert_eq!(cache.get(&right.identity()), Some(&2));
        assert!(Stamp::of(&left).is(&left));
        assert!(!Stamp::of(&left).is(&right));
    }
}

// Dependence without a collecting walk (frame_dependent / state_dependent): every residual remembers its answer, as the OCaml
// residual does (dep_frame / dep_state); without it a let-bound array read by many bindings is walked once per reader and
// `static_sop` asks every residual. It is keyed by id and home: deterministic ids repeat across plans.
#[derive(Default)]
struct DependencyMemo {
    generation: u64,
    values: HashMap<(i64, i64, bool), bool>,
    watched: HashSet<(i64, i64)>,
}
impl DependencyMemo {
    fn invalidate(&mut self) { self.generation = self.generation.wrapping_add(1); self.values.clear(); self.watched.clear(); }
    fn watch(&mut self, function: FnId) {
        let key = (function.1, function.0);
        if self.watched.contains(&key) { return; }
        // Forget watches only with the answers they protect. The generation also rejects
        // an in-flight query that read a function before its watch was forgotten.
        if self.watched.len() >= 4096 { self.invalidate(); }
        self.watched.insert(key);
    }
    fn remember(&mut self, generation: u64, key: (i64, i64, bool), answer: bool) {
        if generation != self.generation { return; }
        // ponytail: clear at 4096 answers; use LRU if eviction profiles hot.
        if self.values.len() >= 4096 { self.invalidate(); return; }
        self.values.insert(key, answer);
    }
}
static DEP_MEMO: LazyLock<Mutex<DependencyMemo>> = LazyLock::new(|| Mutex::new(DependencyMemo::default()));
fn dep_value(arena: &mut Arena, value: &Value, frame: bool, active: &mut HashSet<(i64, i64, bool)>, named: &mut HashMap<(i64, (u64, u32), &'static str), bool>) -> bool {
    let home = match value { Value::Residual(id) => id.1, Value::Fn(id) => id.1, _ => arena.id };
    if home != arena.id {
        return lend(arena, || slot_of(home).is_some_and(|slot| {
            dep_value(&mut checkout(&slot), value, frame, active, named)
        }));
    }
    match value {
        Value::Residual(id) => {
            let key = (id.0, id.1, frame);
            let (known, generation) = { let memo = DEP_MEMO.lock().unwrap(); (memo.values.get(&key).copied(), memo.generation) };
            if let Some(known) = known { return known; }
            if !active.insert((id.1, id.0, false)) { return false; }
            let answer = match own_res(arena, id).cloned() { Some(r) => dep_term(arena, &r.term, &r.env, &r.ctx.context, frame, active, named), None => false };
            active.remove(&(id.1, id.0, false));
            if own_res(arena, id).is_some() { DEP_MEMO.lock().unwrap().remember(generation, key, answer); }
            answer
        }
        Value::Fn(id) => {
            DEP_MEMO.lock().unwrap().watch(*id);
            match own_fn(arena, id).cloned() {
                Some(Function::Closure(closure)) => {
                    if !active.insert((id.1, id.0, true)) { return false; }
                    let answer = dep_term(arena, &closure.body, &closure.env, &closure.at.context, frame, active, named);
                    active.remove(&(id.1, id.0, true));
                    answer
                }
                Some(Function::Named(function)) => dep_named(arena, &function.name, frame, active, named),
                None => false,
            }
        },
        Value::List(values) => values.iter().any(|value| dep_value(arena, value, frame, active, named)),
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => fields.iter().any(|(_, value)| dep_value(arena, value, frame, active, named)),
        _ => false,
    }
}
fn dep_named(arena: &mut Arena, name: &str, frame: bool, active: &mut HashSet<(i64, i64, bool)>, named: &mut HashMap<(i64, (u64, u32), &'static str), bool>) -> bool {
    let workspace = arena.workspace.clone();
    if let Some(graph) = workspace.defs.iter().find(|graph| graph.name == name) {
        // Named bodies have an empty capture environment. Reuse their completed answer only
        // within this query: callers can remap function captures between queries.
        let key = (arena.id, graph.body.identity(), graph.context.as_str());
        if let Some(answer) = named.get(&key) { return *answer; }
        let answer = dep_term(arena, &graph.body, &BTreeMap::new(), &graph.context, frame, active, named);
        named.insert(key, answer);
        return answer;
    }
    frame && op::find_indexed(&arena.workspace.ops, name, &flow_syntax::context::value()).is_some_and(|operation| operation.live)
}
fn dep_term(arena: &mut Arena, term: &Term, env: &BTreeMap<String, Value>, context: &Context, frame: bool, active: &mut HashSet<(i64, i64, bool)>, named: &mut HashMap<(i64, (u64, u32), &'static str), bool>) -> bool {
    let packed = term.arena();
    let own = match term.node {
        TermNode::Time => frame,
        TermNode::State(_) => true,
        TermNode::RefBinding(id) => env.get(term.name(packed.ref_binding(id).name)).is_some_and(|value| dep_value(arena, value, frame, active, named)),
        TermNode::FnRef(name) => dep_named(arena, term.name(name), frame, active, named),
        TermNode::CallFn(id) => {
            let name = term.name(packed.call_fn(id).name);
            match env.get(name) { Some(Value::Fn(function)) => dep_value(arena, &Value::Fn(*function), frame, active, named), _ => dep_named(arena, name, frame, active, named) }
        }
        TermNode::Op(id) => frame && op::find_indexed(&arena.workspace.ops, term.name(packed.op(id).op), context).is_some_and(|operation| operation.live),
        TermNode::GraphRef(id) => {
            let workspace = arena.workspace.clone();
            workspace.graphs.iter().find(|definition| definition.name == term.name(packed.graph_ref(id).graph)).is_some_and(|graph| {
                dep_term(arena, &graph.body, &BTreeMap::new(), &graph.context, frame, active, named)
                    || graph.inputs.iter().any(|(_, _, default)| default.as_ref().is_some_and(|term| dep_term(arena, term, env, context, frame, active, named)))
            })
        },
        _ => false,
    };
    own || children(term).any(|child| dep_term(arena, &child, env, context, frame, active, named))
}
fn dependent_fast(value: &Value, frame: bool) -> bool {
    let Some(arena) = arena_for_value(value) else { return false; };
    let mut arena = checkout(&arena);
    dep_value(&mut arena, value, frame, &mut HashSet::new(), &mut HashMap::new())
}

// ---- compiled pure expressions ----
// A subtree of plain value code (literals, bound names, scalar value operators, let, if, cond, case, vectors, calls of local closures and
// the closures it makes) is compiled once per arena into `Code`: names bound inside it are numbered slots, operators are resolved with their
// argument lists spelled once, and it runs without contexts, routes or residual handling. It is exactly the walker's evaluation of those
// terms: the same steps (counted and budgeted), the same records, the same operator calls in the same order, the same call counts and
// closures (a closure's context is rebuilt from its route when it is made). Anything else the walker would do differently (a live or
// residual value, an error, an interrupt, the budget, a node or data source made by an operator, a call it cannot run) abandons the
// compiled run: the call counts and closures it changed are put back, and the walker evaluates the term instead, from the same state.
#[derive(Clone)]
struct Code { path: Option<Arc<Path>>, kind: Kind }
#[derive(Clone)]
enum Kind {
    Lit(Value),
    Local(usize, Vec<String>),
    Outer(String, Vec<String>),
    Op { op: OpRef, args: Vec<Code>, names: Vec<(String, Value)>, named: Vec<bool>, fast: Option<Fast> },
    AndOr { or: bool, op: OpRef, first: Box<Code>, second: Box<Code>, names: Vec<(String, Value)>, named: Vec<bool>, fast: bool },
    If(Box<Code>, Box<Code>, Box<Code>),
    Cond(Vec<(Code, Code)>, Box<Code>),
    Case(Box<Code>, Vec<(Value, Code)>, Box<Code>),
    Let(Vec<(usize, Code)>, Box<Code>),
    Vec(Vec<Code>),
    Call { callee: Callee, args: Vec<Code>, def: Option<Box<DefCall>>, body: Option<Arc<Term>> },
    MakeFn(Box<MakeFn>),
    // a function port: the captured function takes this term's parameters and body
    PortFn(Box<Code>, Box<MakeFn>),
}
#[derive(Clone)]
enum Callee { Local(usize), Outer(String) }
// A call the walker sends to `apply_def` when its name is not a bound function: the definition, the body it runs, and the route to the call.
#[derive(Clone)]
struct DefCall { name: String, body: Arc<Term>, inputs: Vec<(String, Ty)>, steps: Arc<Vec<Step>> }
// A closure made by compiled code: what the walker's `Fn` arm puts in it, with the route from the frame's context to the term.
#[derive(Clone)]
struct MakeFn { params: (Term, RangeId), body: Arc<Term>, zone: Path, captures: Vec<(String, Option<usize>)>, steps: Vec<Step> }
#[derive(Clone)]
enum Step { Base(Arc<Path>), Seg(Seg) }
// One compiled term (or closure body): its code, the context its operators were resolved in, its slots, and how its runs went.
#[derive(Clone)]
struct Compiled { code: Code, context: &'static str, slots: usize, stamp: Stamp, params: Option<((u64, u32), RangeId)>, inputs: Vec<String>, runs: u32, abandons: u32 }
#[derive(Clone)]
enum CodePlan { Walk(Stamp), Run(Box<Compiled>) }
const COMPILE_MIN_NODES: usize = 4;

fn op_at(workspace: &Workspace, op: OpRef) -> &op::Op { match op { OpRef::Extra(i) => &workspace.ops[i], OpRef::Builtin(i) => &op::all()[i] } }
// A built-in scalar operator with arguments `a` (and `b`), run without the operator call: these have no check, a result type that
// cannot fail, and a body that is exactly the arm below (flow_check op.rs: value_op integer and comparison operators, binary_op through
// its `arithmetic`, `not`, and the float comparisons).
#[derive(Clone, Copy)]
enum Fast { Int(flow_syntax::packed_ops::Integer), Cmp(flow_syntax::packed_ops::Comparison), Arith, FloatCmp(fn(f64, f64) -> bool), Not }
fn op_fast(workspace: &Workspace, op: OpRef, owner: &Term, args: &[flow_check::term_arena::Field]) -> Option<Fast> {
    let OpRef::Builtin(_) = op else { return None; };
    let found = op_at(workspace, op);
    let binary = args.len() == 2 && owner.name(args[0].name) == "a" && owner.name(args[1].name) == "b";
    match found.name.as_str() {
        "not" if args.len() == 1 && owner.name(args[0].name) == "a" => Some(Fast::Not),
        "<" if binary => Some(Fast::FloatCmp(|x, y| x < y)),
        ">" if binary => Some(Fast::FloatCmp(|x, y| x > y)),
        "<=" if binary => Some(Fast::FloatCmp(|x, y| x <= y)),
        ">=" if binary => Some(Fast::FloatCmp(|x, y| x >= y)),
        "=" if binary => Some(Fast::FloatCmp(|x, y| x == y)),
        "+" | "-" | "*" | "/" | "mod" | "pow" | "min" | "max" if binary && found.arithmetic.is_some() => Some(Fast::Arith),
        name if binary => flow_syntax::packed_ops::integer(name).map(Fast::Int).or_else(|| flow_syntax::packed_ops::comparison(name).map(Fast::Cmp)),
        _ => None,
    }
}
// Which arguments `op::result_type` reads (the positional and optional ones), found once per compiled call site.
fn op_named(workspace: &Workspace, op: OpRef, owner: &Term, args: &[flow_check::term_arena::Field]) -> Vec<bool> {
    let found = op_at(workspace, op);
    args.iter().map(|arg| found.signature.pos.iter().chain(found.signature.opt.iter()).any(|(n, _)| n == owner.name(arg.name))).collect()
}
struct Compiler { workspace: Arc<Workspace>, context: Context, scope: Vec<(String, usize)>, slots: usize, nodes: usize, steps: Vec<Step> }
impl Compiler {
    fn op(&self, a: &mut Arena, name: &str) -> Option<OpRef> {
        let workspace = self.workspace.clone();
        let found = find_op(a, &workspace, name, &self.context)?;
        let scalar = matches!(&found.shape, op::Shape::Scalar) && found.ctx == flow_syntax::context::value() && !found.live && found.out_args.is_none()
            && found.name != "live?" && !W::BODY_CONTEXT_OPS.contains(&found.name.as_str());
        if !scalar { return None; }
        let index_in = |table: &[op::Op]| { let start = table.as_ptr() as usize; let at = found as *const op::Op as usize; (at >= start && at < start + table.len() * std::mem::size_of::<op::Op>()).then(|| (at - start) / std::mem::size_of::<op::Op>()) };
        index_in(&workspace.ops).map(OpRef::Extra).or_else(|| index_in(op::all()).map(OpRef::Builtin))
    }
    // A child entered as the walker enters it: `eval_at` (no segment) or `eval_sub` under a segment.
    fn term(&mut self, a: &mut Arena, t: &Term, seg: Option<Seg>) -> Option<Code> {
        let path = term_base(a, t);
        let pushed = match (&path, seg) { (Some(path), _) => { self.steps.push(Step::Base(path.clone())); true } (None, Some(seg)) => { self.steps.push(Step::Seg(seg)); true } (None, None) => false };
        let kind = self.kind(a, t);
        if pushed { self.steps.pop(); }
        Some(Code { path, kind: kind? })
    }
    fn kind(&mut self, a: &mut Arena, t: &Term) -> Option<Kind> {
        self.nodes += 1;
        let arena = t.arena();
        Some(match t.node {
            TermNode::Lit(id) => Kind::Lit(match arena.literal(id) { Literal::Int(n) => Value::Int(n), Literal::Float(n) => Value::Float(n), Literal::Bool(v) => Value::Bool(v), _ => return None }),
            TermNode::Text(id) => Kind::Lit(Value::Text(arena.bytes(id).to_vec())),
            TermNode::Nil => Kind::Lit(Value::No_geo),
            TermNode::RefBinding(id) => {
                let binding = arena.ref_binding(id);
                let name = t.name(binding.name);
                if name.starts_with("$elem:") { return None; }
                let fields = arena.name_ids(binding.fields).iter().map(|name| t.name(*name).to_owned()).collect();
                match self.scope.iter().rev().find(|(bound, _)| bound == name) {
                    Some((_, slot)) => Kind::Local(*slot, fields),
                    None => Kind::Outer(name.to_owned(), fields),
                }
            }
            TermNode::Op(id) => {
                let node = arena.op(id);
                let name = t.name(node.op);
                if !arena.skip(node.skip).is_empty() || name == "live?" { return None; }
                let args = arena.fields(node.args);
                let op = self.op(a, name)?;
                if (name == "and" || name == "or") && args.len() == 2 {
                    let seg = Seg::Owned(intern(a, t.name(args[0].name)));
                    let first = Box::new(self.term(a, &t.child(args[0].term), Some(seg))?);
                    let seg = Seg::Owned(intern(a, t.name(args[1].name)));
                    let second = Box::new(self.term(a, &t.child(args[1].term), Some(seg))?);
                    let named = op_named(&self.workspace, op, t, args);
                    let fast = matches!(op, OpRef::Builtin(_)) && t.name(args[0].name) == "a" && t.name(args[1].name) == "b";
                    Kind::AndOr { or: name == "or", op, first, second, names: args.iter().map(|arg| (t.name(arg.name).to_owned(), Value::No_geo)).collect(), named, fast }
                } else {
                    let mut code = Vec::with_capacity(args.len());
                    for (index, arg) in args.iter().enumerate() {
                        let count = args[..index].iter().filter(|other| other.name == arg.name).count();
                        let name = t.name(arg.name);
                        let seg = if count == 0 { Seg::Owned(intern(a, name)) } else { format!("{}#{}", name, count).into() };
                        code.push(self.term(a, &t.child(arg.term), Some(seg))?);
                    }
                    Kind::Op { op, args: code, names: args.iter().map(|arg| (t.name(arg.name).to_owned(), Value::No_geo)).collect(), named: op_named(&self.workspace, op, t, args), fast: op_fast(&self.workspace, op, t, args) }
                }
            }
            TermNode::If(id) => {
                let node = arena.if_node(id);
                let test = self.term(a, &t.child(node.test), Some("if".into()))?;
                let yes = self.term(a, &t.child(node.yes), Some("then".into()))?;
                Kind::If(Box::new(test), Box::new(yes), Box::new(self.term(a, &t.child(node.no), Some("else".into()))?))
            }
            TermNode::Cond(id) => {
                let node = arena.cond(id);
                let arms = arena.arms(node.arms);
                let mut code = Vec::with_capacity(arms.len());
                for (index, arm) in arms.iter().enumerate() {
                    let test = self.term(a, &t.child(arm.test), Some(Seg::Index("test", index)))?;
                    code.push((test, self.term(a, &t.child(arm.body), Some(Seg::Index("arm", index)))?));
                }
                Kind::Cond(code, Box::new(self.term(a, &t.child(node.default), Some("else".into()))?))
            }
            TermNode::Case(id) => {
                let node = arena.case(id);
                let subject = Box::new(self.term(a, &t.child(node.value), Some("case".into()))?);
                let arms = arena.case_arms(node.arms);
                let mut code = Vec::with_capacity(arms.len());
                for (index, arm) in arms.iter().enumerate() { let literal = literal_case(&arena.syntax().handle(arm.form)).ok()?; code.push((literal, self.term(a, &t.child(arm.body), Some(Seg::Index("arm", index)))?)); }
                Kind::Case(subject, code, Box::new(self.term(a, &t.child(node.default), Some("else".into()))?))
            }
            TermNode::Let(id) => {
                let node = arena.let_node(id);
                let bindings = arena.bindings(node.bindings);
                let depth = self.scope.len();
                let mut code = Vec::with_capacity(bindings.len());
                for binding in bindings {
                    let PatternNode::Name(name) = arena.pattern(binding.pattern) else { self.scope.truncate(depth); return None; };
                    let Some(value) = self.term(a, &t.child(binding.term), None) else { self.scope.truncate(depth); return None; };
                    let slot = self.slots;
                    self.slots += 1;
                    self.scope.push((t.name(name).to_owned(), slot));
                    code.push((slot, value));
                }
                let body = self.term(a, &t.child(node.body), None);
                self.scope.truncate(depth);
                Kind::Let(code, Box::new(body?))
            }
            TermNode::Vec(range) => {
                let terms = arena.term_ids(range);
                if !(2..=4).contains(&terms.len()) { return None; }
                let mut code = Vec::with_capacity(terms.len());
                for (index, term) in terms.iter().enumerate() { code.push(self.term(a, &t.child(*term), Some(Seg::Index("", index)))?); }
                Kind::Vec(code)
            }
            TermNode::CallFn(id) => {
                let node = arena.call_fn(id);
                let fn_name = t.name(node.name);
                let args = arena.term_ids(node.args);
                let callee = match self.scope.iter().rev().find(|(bound, _)| bound == fn_name) { Some((_, slot)) => Callee::Local(*slot), None => Callee::Outer(fn_name.to_owned()) };
                let specialized = node.body.map(|id| t.child(id));
                let def = if matches!(callee, Callee::Local(_)) { None } else {
                    let workspace = self.workspace.clone();
                    match workspace.defs.iter().find(|graph| graph.name == fn_name) {
                        Some(graph) if graph.inputs.len() == args.len() => {
                            let body = shared_term(a, specialized.as_ref().unwrap_or(&graph.body));
                            Some(Box::new(DefCall { name: fn_name.to_owned(), body, inputs: graph.inputs.iter().map(|(name, ty, _)| (name.clone(), ty.clone())).collect(), steps: Arc::new(self.steps.clone()) }))
                        }
                        Some(_) => return None,
                        None => None,
                    }
                };
                let body = specialized.as_ref().map(|body| shared_term(a, body));
                let mut code = Vec::with_capacity(args.len());
                for (index, term) in args.iter().enumerate() { code.push(self.term(a, &t.child(*term), Some(Seg::Index("a", index)))?); }
                Kind::Call { callee, args: code, def, body }
            }
            TermNode::Fn(id) => {
                let node = arena.function(id);
                let body = t.child(node.body);
                let params = (t.clone(), node.params);
                let zone = packed_path(t, node.zone);
                if let Some(original) = node.capture {
                    let original = Box::new(self.term(a, &t.child(original), None)?);
                    Kind::PortFn(original, Box::new(MakeFn { params, body: shared_term(a, &body), zone, captures: Vec::new(), steps: self.steps.clone() }))
                } else {
                    let captures = free_names(&body).into_iter().map(|name| {
                        let slot = self.scope.iter().rev().find(|(bound, _)| *bound == name).map(|(_, slot)| *slot);
                        (name, slot)
                    }).collect();
                    Kind::MakeFn(Box::new(MakeFn { params, body: shared_term(a, &body), zone, captures, steps: self.steps.clone() }))
                }
            }
            _ => return None,
        })
    }
}

// Why a compiled run gives way to the walker.
struct Abandon;
enum Undo { Calls(FnId, Option<i64>), Closure(FnId), Replace(FnId, Function) }
enum Root { Ctx(ExecCtx), Call { closure: Arc<ClosureData>, index: i64 }, Def { name: String, caller: usize, steps: Arc<Vec<Step>> } }
struct CallFrame { slots: Vec<Value>, env: Env, root: Root }
struct Run { workspace: Arc<Workspace>, frames: Vec<CallFrame>, spare: Vec<Vec<Value>>, steps: usize, base: usize, depth: usize, notes: Vec<(Path, Value)>, record: bool, undo: Vec<Undo>, context: Context }
impl Run {
    fn step(&mut self, a: &Arena) -> Result<(), Abandon> {
        self.steps += 1;
        let at = self.base + self.steps;
        if at % PACKED_BLOCK == 0 && a.interrupt.as_ref().is_some_and(|stop| stop()) { return Err(Abandon); }
        if at > a.budget { return Err(Abandon); }
        Ok(())
    }
    // A value made by compiled code is plain by construction (operators, literals, slots of such values); one read from the enclosing
    // scope is checked.
    fn done(&mut self, path: &Option<Arc<Path>>, value: Value, outside: bool) -> Result<Value, Abandon> {
        if outside && is_live_value(&value) { return Err(Abandon); }
        if self.record { if let Some(path) = path { self.notes.push(((**path).clone(), value.clone())); } }
        Ok(value)
    }
    // `name.f.g` read through references: only the field itself is copied (an error gives way, so no message is spelled).
    fn fields(value: &Value, fields: &[String]) -> Result<Value, Abandon> {
        let mut at = value;
        let mut owned;
        for field_name in fields {
            if matches!(at, Value::Residual(_) | Value::Source(..) | Value::Struct(_, Ty::Array(_), _)) { return Err(Abandon); }
            match at {
                Value::Record(entries) => at = &entries.iter().find(|(name, _)| name == field_name).ok_or(Abandon)?.1,
                other => { owned = field(other.clone(), field_name, String::new).map_err(|_| Abandon)?; at = &owned; }
            }
        }
        Ok(at.clone())
    }
    // The context of frame `k`, as the walker's would be: the entry context, or a call's (`call_fn`) over its caller's.
    fn root_ctx(&self, a: &mut Arena, k: usize) -> ExecCtx {
        match &self.frames[k].root {
            Root::Ctx(context) => context.clone(),
            Root::Def { name, caller, steps } => {
                let mut context = self.root_ctx(a, *caller);
                for step in steps.iter() { match step { Step::Base(path) => { context.base = path.clone(); context.route = Route(None); } Step::Seg(seg) => context = sub(&context, seg.clone()) } }
                let mut cx = context.clone();
                cx.prefix = Arc::new(site(&context)); cx.base = Arc::new(vec![format!("def:{}", name)]); cx.route = Route(None); cx.depth += 1;
                cx
            }
            Root::Call { closure, index } => {
                let mut context = self.root_ctx(a, k - 1);
                context.inst = closure.at.inst;
                context.prefix = closure.at.prefix.clone();
                context.base = base_of(a, &closure.zone);
                context.route = Route(None);
                Arc::make_mut(&mut context.iter).push(*index);
                context.depth += 1;
                context
            }
        }
    }
    // A `Fast` operator: its arguments, then its arm; arguments of other kinds take the operator call.
    fn fast(&mut self, a: &mut Arena, op: OpRef, fast: Fast, args: &mut [Code], names: &mut Vec<(String, Value)>, named: &[bool]) -> Result<Value, Abandon> {
        let first = self.eval(a, &mut args[0])?;
        let second = if args.len() > 1 { Some(self.eval(a, &mut args[1])?) } else { None };
        let value = match (fast, &first, &second) {
            (Fast::Int(operation), Value::Int(x), Some(Value::Int(y))) => Some(Value::Int(flow_syntax::packed_ops::integer_apply(operation, *x, *y).map_err(|_| Abandon)?)),
            (Fast::Cmp(operation), Value::Int(x), Some(Value::Int(y))) => Some(Value::Bool(flow_syntax::packed_ops::comparison_apply(operation, *x, *y))),
            (Fast::FloatCmp(predicate), x, Some(y)) => Some(Value::Bool(predicate(num(x).map_err(|_| Abandon)?, num(y).map_err(|_| Abandon)?))),
            (Fast::Arith, x, Some(y)) => {
                let apply = &op_at(&self.workspace, op).arithmetic.as_ref().expect("an arithmetic operator").apply;
                let result = apply(x, y).map_err(|_| Abandon)?;
                if is_live_value(&result) { return Err(Abandon); }
                Some(result)
            }
            (Fast::Not, x, None) => Some(Value::Bool(!truth(x).map_err(|_| Abandon)?)),
            _ => None,
        };
        if let Some(value) = value { return Ok(value); }
        let mut values = std::mem::take(names);
        values[0].1 = first;
        if let Some(second) = second { values[1].1 = second; }
        let result = self.apply(a, op, &values, named);
        for entry in values.iter_mut() { entry.1 = Value::No_geo; }
        *names = values;
        result
    }
    fn apply(&mut self, a: &Arena, op: OpRef, values: &[(String, Value)], named: &[bool]) -> Result<Value, Abandon> {
        let found = op_at(&self.workspace, op);
        if a.time.is_none() && large_constant_array(&found.name, values) && current_residual_handlers().is_some_and(|h| h.defer.is_some()) { return Err(Abandon); }
        (found.check)(values).map_err(|_| Abandon)?;
        let mut types: [Ty; 4] = [Ty::Any, Ty::Any, Ty::Any, Ty::Any];
        let mut count = 0;
        let mut spilled = Vec::new();
        for ((_, value), named) in values.iter().zip(named) {
            if !named { continue; }
            if count < 4 { types[count] = value::ty_of(value); } else { if count == 4 { spilled.extend(types.iter().cloned()); } spilled.push(value::ty_of(value)); }
            count += 1;
        }
        (found.out)(if count <= 4 { &types[..count] } else { &spilled }).map_err(|_| Abandon)?;
        static AT_ZERO: LazyLock<Frame> = LazyLock::new(|| input::at_time(0.0));
        let made = std::cell::Cell::new(false);
        let node = |_: &str, _: &[(String, Value)]| -> Result<Value, Diagnostic> { made.set(true); Err(error("E_EVAL", "A compiled scalar operator made a node.")) };
        let result = match &a.time { Some(frame) => (found.body)(frame, &node, values), None => (found.body)(&AT_ZERO, &node, values) };
        if made.get() { return Err(Abandon); }
        let result = result.map_err(|_| Abandon)?;
        if matches!(result, Value::Source(..)) || is_live_value(&result) { return Err(Abandon); }
        Ok(result)
    }
    fn call(&mut self, a: &mut Arena, function: Value, mut values: Vec<Value>, body: Option<&Arc<Term>>) -> Result<Value, Abandon> {
        if self.record { return Err(Abandon); }
        let Value::Fn(id) = function else { return Err(Abandon); };
        if id.1 != a.id { return Err(Abandon); }
        let Some(Function::Closure(closure)) = a.functions.get(&id).cloned() else { return Err(Abandon); };
        let parameters = closure_params(&closure);
        let parameter_arena = closure.params.0.arena();
        if parameters.len() != values.len() || self.depth + 1 > MAX_DEPTH { return Err(Abandon); }
        let body = body.unwrap_or(&closure.body).clone();
        let key = body.identity();
        let signature = Some((closure.params.0.identity(), closure.params.1));
        let mut compiled = match a.fn_plans.remove(&key) {
            Some(CodePlan::Run(compiled)) if compiled.stamp.is(&body) && compiled.params == signature => compiled,
            Some(CodePlan::Walk(stamp)) if stamp.is(&body) => { a.fn_plans.insert(key, CodePlan::Walk(stamp)); return Err(Abandon); }
            _ => {
                let mut compiler = Compiler { workspace: self.workspace.clone(), context: self.context.clone(), scope: Vec::new(), slots: 0, nodes: 0, steps: Vec::new() };
                let mut ok = true;
                for parameter in parameters {
                    match parameter_arena.pattern(parameter.pattern) { PatternNode::Name(name) => { compiler.scope.push((parameter_arena.name(name).to_owned(), compiler.slots)); compiler.slots += 1; } _ => { ok = false } }
                }
                match ok.then(|| compiler.term(a, &body, None)).flatten() {
                    Some(code) => Box::new(Compiled { code, context: self.context.as_str(), slots: compiler.slots, stamp: Stamp::of(&body), params: signature, inputs: Vec::new(), runs: 0, abandons: 0 }),
                    None => { a.fn_plans.insert(key, CodePlan::Walk(Stamp::of(&body))); return Err(Abandon); }
                }
            }
        };
        if compiled.context != self.context.as_str() { a.fn_plans.insert(key, CodePlan::Run(compiled)); return Err(Abandon); }
        let previous = a.fn_calls.get(&id).copied();
        let index = previous.unwrap_or(closure.calls);
        self.undo.push(Undo::Calls(id, previous));
        a.fn_calls.insert(id, index + 1);
        let mut slots = self.spare.pop().unwrap_or_default();
        slots.resize(compiled.slots, Value::No_geo);
        for (slot, (parameter, value)) in parameters.iter().zip(values.drain(..)).enumerate() {
            slots[slot] = match parameter.ty { Some(ty) => match coerce(parameter_arena.ty(ty), &value) { Ok(value) => value, Err(_) => { a.fn_plans.insert(key, CodePlan::Run(compiled)); return Err(Abandon); } }, None => value };
        }
        self.recycle(values);
        self.frames.push(CallFrame { slots, env: Env::of(closure.env.clone()), root: Root::Call { closure: closure.clone(), index } });
        self.depth += 1;
        let result = self.eval(a, &mut compiled.code);
        self.depth -= 1;
        if let Some(frame) = self.frames.pop() { self.recycle(frame.slots); }
        a.fn_plans.insert(key, CodePlan::Run(compiled));
        result
    }
    // `apply_def` for a definition with all its inputs given: a frame of its inputs, under `def:<name>` at the call's site.
    fn call_def(&mut self, a: &mut Arena, def: &DefCall, mut values: Vec<Value>) -> Result<Value, Abandon> {
        if self.record || self.depth >= MAX_DEPTH { return Err(Abandon); }
        let key = def.body.identity();
        let same = |compiled: &Compiled| compiled.stamp.is(&def.body) && compiled.params.is_none() && compiled.inputs.len() == def.inputs.len()
            && compiled.inputs.iter().zip(&def.inputs).all(|(bound, (name, _))| bound == name);
        let mut compiled = match a.fn_plans.remove(&key) {
            Some(CodePlan::Run(compiled)) if same(&compiled) => compiled,
            Some(CodePlan::Walk(stamp)) if stamp.is(&def.body) => { a.fn_plans.insert(key, CodePlan::Walk(stamp)); return Err(Abandon); }
            _ => {
                let mut compiler = Compiler { workspace: self.workspace.clone(), context: self.context.clone(), scope: Vec::new(), slots: 0, nodes: 0, steps: Vec::new() };
                for (name, _) in &def.inputs { compiler.scope.push((name.clone(), compiler.slots)); compiler.slots += 1; }
                match compiler.term(a, &def.body, None) {
                    Some(code) => Box::new(Compiled { code, context: self.context.as_str(), slots: compiler.slots, stamp: Stamp::of(&def.body), params: None, inputs: def.inputs.iter().map(|(name, _)| name.clone()).collect(), runs: 0, abandons: 0 }),
                    None => { a.fn_plans.insert(key, CodePlan::Walk(Stamp::of(&def.body))); return Err(Abandon); }
                }
            }
        };
        if compiled.context != self.context.as_str() { a.fn_plans.insert(key, CodePlan::Run(compiled)); return Err(Abandon); }
        let mut slots = self.spare.pop().unwrap_or_default();
        slots.resize(compiled.slots, Value::No_geo);
        for (slot, ((_, ty), value)) in def.inputs.iter().zip(values.drain(..)).enumerate() {
            match coerce(ty, &value) { Ok(value) => slots[slot] = value, Err(_) => { a.fn_plans.insert(key, CodePlan::Run(compiled)); return Err(Abandon); } }
        }
        self.recycle(values);
        let caller = self.frames.len() - 1;
        self.frames.push(CallFrame { slots, env: Env::empty(), root: Root::Def { name: def.name.clone(), caller, steps: def.steps.clone() } });
        self.depth += 1;
        let result = self.eval(a, &mut compiled.code);
        self.depth -= 1;
        if let Some(frame) = self.frames.pop() { self.recycle(frame.slots); }
        a.fn_plans.insert(key, CodePlan::Run(compiled));
        result
    }
    fn make_fn(&mut self, a: &mut Arena, made: &MakeFn) -> Value {
        let top = self.frames.len() - 1;
        let mut context = self.root_ctx(a, top);
        for step in &made.steps {
            match step { Step::Base(path) => { context.base = path.clone(); context.route = Route(None); } Step::Seg(seg) => context = sub(&context, seg.clone()) }
        }
        let frame = &self.frames[top];
        let env: BTreeMap<String, Value> = made.captures.iter().filter_map(|(name, slot)| match slot { Some(slot) => Some((name.clone(), frame.slots[*slot].clone())), None => frame.env.get(name).map(|value| (name.clone(), value)) }).collect();
        let id = closure_id(a, Function::Closure(Arc::new(ClosureData { params: made.params.clone(), replayable: true, body: made.body.clone(), env: Arc::new(env), zone: made.zone.clone(), calls: 0, at: context })));
        self.undo.push(Undo::Closure(id));
        Value::Fn(id)
    }
    // Put back what a run that gave way changed: call counts and the closures it made.
    fn undo_into(&mut self, a: &mut Arena) {
        for undo in self.undo.drain(..).rev() {
            match undo {
                Undo::Calls(id, Some(count)) => { a.fn_calls.insert(id, count); }
                Undo::Calls(id, None) => { a.fn_calls.remove(&id); }
                Undo::Closure(id) => { a.functions.remove(&id); if a.fn_log.last() == Some(&id) { a.fn_log.pop(); } }
                Undo::Replace(id, function) => { replace_fn(a, id, function); }
            }
        }
    }
    // A slot or argument vector, emptied and kept for the next call (calls in a loop allocate none).
    fn recycle(&mut self, mut values: Vec<Value>) { values.clear(); if self.spare.len() < 16 { self.spare.push(values); } }
    fn local(&self, slot: usize) -> Value { self.frames.last().expect("a frame").slots[slot].clone() }
    fn eval(&mut self, a: &mut Arena, code: &mut Code) -> Result<Value, Abandon> {
        let _nesting = EvalNesting::enter().map_err(|_| Abandon)?;
        self.step(a)?;
        let Code { path, kind } = code;
        let outside = matches!(kind, Kind::Outer(..));
        let value = match kind {
            Kind::Lit(value) => value.clone(),
            Kind::Local(slot, fields) => {
                if fields.is_empty() { self.local(*slot) } else { Self::fields(&self.frames.last().expect("a frame").slots[*slot], fields)? }
            }
            Kind::Outer(name, fields) => {
                let env = &self.frames.last().expect("a frame").env;
                if fields.is_empty() { env.get(name).ok_or(Abandon)? } else { Self::fields(env.get_ref(name).ok_or(Abandon)?, fields)? }
            }
            Kind::Op { op, args, names, named, fast } => {
                if let Some(fast) = *fast { let value = self.fast(a, *op, fast, args, names, named)?; return self.done(path, value, false); }
                let mut values = std::mem::take(names);
                for (index, arg) in args.iter_mut().enumerate() {
                    match self.eval(a, arg) { Ok(value) => values[index].1 = value, Err(abandon) => { *names = values; return Err(abandon); } }
                }
                let result = self.apply(a, *op, &values, named);
                for entry in values.iter_mut() { entry.1 = Value::No_geo; }
                *names = values;
                result?
            }
            Kind::AndOr { or, op, first, second, names, named, fast } => {
                let left = self.eval(a, first)?;
                if let Value::Bool(value) = &left { if *value == *or { return self.done(path, Value::Bool(*value), false); } }
                let right = self.eval(a, second)?;
                // `and` / `or`: truthy(a) and/or truthy(b)
                if *fast {
                    let x = truth(&left).map_err(|_| Abandon)?;
                    let value = if *or { x || truth(&right).map_err(|_| Abandon)? } else { x && truth(&right).map_err(|_| Abandon)? };
                    return self.done(path, Value::Bool(value), false);
                }
                let mut values = std::mem::take(names);
                values[0].1 = left;
                values[1].1 = right;
                let result = self.apply(a, *op, &values, named);
                for entry in values.iter_mut() { entry.1 = Value::No_geo; }
                *names = values;
                result?
            }
            Kind::If(test, yes, no) => {
                let test = self.eval(a, test)?;
                if truth(&test).map_err(|_| Abandon)? { self.eval(a, yes)? } else { self.eval(a, no)? }
            }
            Kind::Cond(arms, default) => {
                let mut chosen = None;
                for (index, (test, _)) in arms.iter_mut().enumerate() {
                    let test = self.eval(a, test)?;
                    if truth(&test).map_err(|_| Abandon)? { chosen = Some(index); break; }
                }
                match chosen { Some(index) => self.eval(a, &mut arms[index].1)?, None => self.eval(a, default)? }
            }
            Kind::Case(subject, arms, default) => {
                let value = self.eval(a, subject)?;
                let mut chosen = None;
                for (index, (literal, _)) in arms.iter().enumerate() {
                    let matches = if matches!(&value, Value::Bool(_)) || matches!(literal, Value::Bool(_)) {
                        let truthy = |value: &Value| match value { Value::Bool(value) => Ok(*value), value => num(value).map(|number| number != 0.0) };
                        truthy(&value).map_err(|_| Abandon)? == truthy(literal).map_err(|_| Abandon)?
                    } else {
                        match (&value, literal) {
                            (Value::Text(x), Value::Text(y)) => x == y,
                            (Value::Int(_) | Value::Float(_), Value::Int(_) | Value::Float(_)) => num(&value).map_err(|_| Abandon)? == num(literal).map_err(|_| Abandon)?,
                            _ => false,
                        }
                    };
                    if matches { chosen = Some(index); break; }
                }
                match chosen { Some(index) => self.eval(a, &mut arms[index].1)?, None => self.eval(a, default)? }
            }
            Kind::Let(bindings, body) => {
                for (slot, term) in bindings.iter_mut() { let value = self.eval(a, term)?; self.frames.last_mut().expect("a frame").slots[*slot] = value; }
                self.eval(a, body)?
            }
            Kind::Vec(terms) => {
                let mut values = [0.0; 4];
                for (index, term) in terms.iter_mut().enumerate() { values[index] = num(&self.eval(a, term)?).map_err(|_| Abandon)?; }
                match terms.len() { 2 => Value::Vec2(values[0], values[1]), 3 => Value::Vec3(values[0], values[1], values[2]), _ => Value::Vec4(values[0], values[1], values[2], values[3]) }
            }
            Kind::Call { callee, args, def, body } => {
                let mut values = self.spare.pop().unwrap_or_default();
                for arg in args.iter_mut() { match self.eval(a, arg) { Ok(value) => values.push(value), Err(abandon) => { self.recycle(values); return Err(abandon); } } }
                let function = match callee {
                    Callee::Local(slot) => Some(self.local(*slot)),
                    Callee::Outer(name) => self.frames.last().expect("a frame").env.get(name),
                };
                match (function, def) {
                    (Some(function @ Value::Fn(_)), _) => self.call(a, function, values, body.as_ref())?,
                    (Some(_), None) | (None, None) => return Err(Abandon),
                    (_, Some(def)) => self.call_def(a, def, values)?,
                }
            }
            Kind::MakeFn(made) => self.make_fn(a, made),
            Kind::PortFn(original, made) => {
                let Value::Fn(id) = self.eval(a, original)? else { return Err(Abandon); };
                if id.1 != a.id { return Err(Abandon); }
                match a.functions.get(&id).cloned() {
                    Some(Function::Closure(closure)) => {
                        let mut ported = (*closure).clone();
                        ported.params = made.params.clone();
                        ported.body = made.body.clone();
                        self.undo.push(Undo::Replace(id, Function::Closure(closure)));
                        replace_fn(a, id, Function::Closure(Arc::new(ported)));
                        Value::Fn(id)
                    }
                    Some(Function::Named(named)) => {
                        let top = self.frames.len() - 1;
                        let mut context = self.root_ctx(a, top);
                        for step in &made.steps { match step { Step::Base(path) => { context.base = path.clone(); context.route = Route(None); } Step::Seg(seg) => context = sub(&context, seg.clone()) } }
                        let id = closure_id(a, Function::Closure(Arc::new(ClosureData { params: made.params.clone(), replayable: false, body: made.body.clone(), env: Arc::new(BTreeMap::new()), zone: made.zone.clone(), calls: named.calls, at: context })));
                        self.undo.push(Undo::Closure(id));
                        Value::Fn(id)
                    }
                    None => return Err(Abandon),
                }
            }
        };
        self.done(path, value, outside)
    }
}
// `call_fn` of a closure whose body compiles, run compiled (None: call it walking).
fn call_compiled(a: &mut Arena, c: &ExecCtx, id: FnId, values: &[Value], body: Option<&Term>) -> Option<Result<Value, Diagnostic>> {
    let mut caller = Caller::new(a, c);
    caller.call(a, id, values, body)
}
// Calls of closures from a walked loop (map, filter, sort-by, reduce, iterate): one compiled caller for the whole loop.
struct Caller { run: Option<Run> }
impl Caller {
    fn new(a: &Arena, c: &ExecCtx) -> Caller {
        if a.record && c.record { return Caller { run: None }; }
        Caller { run: Some(Run {
            workspace: a.workspace.0.clone(), frames: vec![CallFrame { slots: Vec::new(), env: Env::empty(), root: Root::Ctx(c.clone()) }],
            steps: 0, base: a.step_count, depth: c.depth, notes: Vec::new(), record: false, undo: Vec::new(), context: c.context.clone(), spare: Vec::new(),
        }) }
    }
    fn call(&mut self, a: &mut Arena, id: FnId, values: &[Value], body: Option<&Term>) -> Option<Result<Value, Diagnostic>> {
        // arguments from the walker may be live; compiled code takes only plain ones
        if values.iter().any(is_live_value) { return None; }
        let run = self.run.as_mut()?;
        let body = body.map(|body| shared_term(a, body));
        run.base = a.step_count;
        run.steps = 0;
        let mut arguments = run.spare.pop().unwrap_or_default();
        arguments.extend_from_slice(values);
        match run.call(a, Value::Fn(id), arguments, body.as_ref()) {
            Ok(value) => {
                a.step_count += run.steps;
                EVAL_STEPS.fetch_add(run.steps as u64, Ordering::Relaxed);
                run.undo.clear();
                Some(Ok(value))
            }
            Err(Abandon) => { run.undo_into(a); None }
        }
    }
}
// `call_fn` through a loop's caller.
fn call_in(caller: &mut Caller, a: &mut Arena, c: &ExecCtx, id: FnId, values: &[Value]) -> Result<Value, Diagnostic> {
    match caller.call(a, id, values, None) { Some(result) => result, None => call_fn_walked(a, c, id, values, None) }
}
// Whether a term could compile at all, and how many nodes it has, found once per term: a walked subtree is not compiled again and again
// down to its first refusal (a cold lowering asks every term once). A term judged compilable may still be refused by the compiler.
fn pure_size(a: &mut Arena, t: &Term, context: &Context) -> Option<usize> {
    let key = t.identity();
    if let Some((within, size)) = a.pure.get(&key) { if *within == context.as_str() { return *size; } }
    let arena = t.arena();
    let own = match t.node {
        TermNode::Lit(id) => matches!(arena.literal(id), Literal::Int(_) | Literal::Float(_) | Literal::Bool(_)),
        TermNode::Text(_) | TermNode::Nil => true,
        TermNode::RefBinding(id) => !t.name(arena.ref_binding(id).name).starts_with("$elem:"),
        TermNode::Op(id) => {
            let node = arena.op(id);
            let name = t.name(node.op);
            arena.skip(node.skip).is_empty() && name != "live?" && {
                let workspace = a.workspace.0.clone();
                find_op(a, &workspace, name, context).is_some_and(|found| matches!(&found.shape, op::Shape::Scalar) && found.ctx == flow_syntax::context::value() && !found.live && found.out_args.is_none() && !W::BODY_CONTEXT_OPS.contains(&found.name.as_str()))
            }
        }
        TermNode::If(_) | TermNode::Cond(_) => true,
        TermNode::Case(id) => arena.case_arms(arena.case(id).arms).iter().all(|arm| literal_case(&arena.syntax().handle(arm.form)).is_ok()),
        TermNode::Let(id) => arena.bindings(arena.let_node(id).bindings).iter().all(|binding| matches!(arena.pattern(binding.pattern), PatternNode::Name(_))),
        TermNode::Vec(range) => (2..=4).contains(&arena.term_ids(range).len()),
        TermNode::CallFn(_) | TermNode::Fn(_) => true,
        _ => false,
    };
    let size = if !own { None } else {
        let mut size = Some(1usize);
        match t.node {
            TermNode::CallFn(id) => {
                for part in arena.term_ids(arena.call_fn(id).args) {
                    size = match (size, pure_size(a, &t.child(*part), context)) { (Some(total), Some(more)) => Some(total + more), _ => None };
                    if size.is_none() { break; }
                }
            }
            TermNode::Fn(id) => if let Some(part) = arena.function(id).capture {
                size = pure_size(a, &t.child(part), context).map(|more| 1 + more);
            },
            _ => for part in children(t) {
                size = match (size, pure_size(a, &part, context)) { (Some(total), Some(more)) => Some(total + more), _ => None };
                if size.is_none() { break; }
            },
        }
        size
    };
    a.pure.insert(key, (context.as_str(), size));
    size
}
// The walker's evaluation of `t`, run compiled when `t` is a pure subtree (None: walk it).
fn run_compiled(a: &mut Arena, c: &ExecCtx, env: &Env, t: &Term) -> Option<Result<Value, Diagnostic>> {
    if c.depth > MAX_DEPTH { return None; }
    let key = t.identity();
    let mut compiled = match a.plans.remove(&key) {
        Some(CodePlan::Walk(at)) if at.is(t) => { a.plans.insert(key, CodePlan::Walk(at)); return None; }
        Some(CodePlan::Run(compiled)) if compiled.stamp.is(t) && compiled.context == c.context.as_str() => compiled,
        _ => {
            if pure_size(a, t, &c.context).is_none_or(|size| size < COMPILE_MIN_NODES) { a.plans.insert(key, CodePlan::Walk(Stamp::of(t))); return None; }
            let mut compiler = Compiler { workspace: a.workspace.0.clone(), context: c.context.clone(), scope: Vec::new(), slots: 0, nodes: 0, steps: Vec::new() };
            match compiler.term(a, t, None) {
                Some(code) if compiler.nodes >= COMPILE_MIN_NODES => Box::new(Compiled { code, context: c.context.as_str(), slots: compiler.slots, stamp: Stamp::of(t), params: None, inputs: Vec::new(), runs: 0, abandons: 0 }),
                _ => { a.plans.insert(key, CodePlan::Walk(Stamp::of(t))); return None; }
            }
        }
    };
    let mut run = Run {
        workspace: a.workspace.0.clone(), frames: vec![CallFrame { slots: vec![Value::No_geo; compiled.slots], env: env.clone(), root: Root::Ctx(c.clone()) }],
        steps: 0, base: a.step_count, depth: c.depth, notes: Vec::new(), record: a.record && c.record, undo: Vec::new(), context: c.context.clone(), spare: Vec::new(),
    };
    let outcome = run.eval(a, &mut compiled.code);
    compiled.runs += 1;
    let result = match outcome {
        Ok(value) => {
            a.step_count += run.steps;
            EVAL_STEPS.fetch_add(run.steps as u64, Ordering::Relaxed);
            for (path, value) in std::mem::take(&mut run.notes) { note(a, c, path, value); }
            Some(Ok(value))
        }
        Err(Abandon) => {
           
            run.undo_into(a);
            compiled.abandons += 1;
            None
        }
    };
    // a term that keeps giving way is walked from now on
    if compiled.abandons >= 3 && compiled.abandons * 2 > compiled.runs { a.plans.insert(key, CodePlan::Walk(Stamp::of(t))); } else { a.plans.insert(key, CodePlan::Run(compiled)); }
    result
}

#[cfg(test)]
mod ownership_tests {
    use super::*;

    #[test]
    fn residual_dependency_memo_tracks_capture_edits() {
        let mut memo = DependencyMemo::default();
        for id in 0..8192 { memo.remember(memo.generation, (id, 0, false), true); }
        assert!(memo.values.len() <= 4096, "the memo has a fixed retained-entry ceiling");
        let prior = memo.generation;
        memo.invalidate();
        memo.remember(prior, (0, 0, false), true);
        assert!(memo.values.is_empty(), "an in-flight old query cannot restore invalidated answers");
        memo.remember(memo.generation, (0, 0, false), false);
        assert_eq!(memo.values.get(&(0, 0, false)), Some(&false));
        let prior = memo.generation;
        for id in 0..8192 { memo.watch(FnId(id, 0)); }
        assert!(memo.watched.len() <= 4096, "watched functions have a fixed retained-entry ceiling too");
        memo.remember(prior, (0, 0, false), true);
        assert!(memo.values.is_empty(), "forgetting watches also rejects old in-flight answers");
        let checked = crate::test_workspace_eval::check("(workspace w (graph g :context host [(x : float 0)]
          (let* [f (fn [(i : float)] (+ x (+ i t)))] (map f (array/float 4)))))");
        let evaluation = r#static(&checked, true, None).unwrap();
        let value = &evaluation.results[0].1;
        assert!(matches!(value, Value::Residual(_)));
        let Value::Fn(function) = evaluation.records.iter().find(|(path, _)| path == &["g", "f"]).unwrap().1[0].1 else { panic!("recorded mapper"); };
        let Value::Residual(original) = value else { unreachable!() };
        let mut captured = peek(&slot_of(original.1).unwrap(), |arena| own_res(arena, original).unwrap().clone());
        let foreign = Arena::new(&checked, false, 1000);
        let foreign = {
            let mut arena = checkout(&foreign);
            captured.ctx.arena = arena.id;
            Value::Residual(residual_id(&mut arena, captured))
        };
        let values = [value, &foreign];
        for value in values { assert!(!state_dependent(value)); }
        let generation = DEP_MEMO.lock().unwrap().generation;
        peek(&slot_of(function.1).unwrap(), |arena| {
            let same = own_fn(arena, &function).unwrap().clone();
            replace_fn(arena, function, same.clone());
            let unused = closure_id(arena, same.clone());
            let Function::Closure(closure) = same else { unreachable!() };
            let mut changed = unshare(closure);
            changed.env = Arc::new(BTreeMap::new());
            replace_fn(arena, unused, Function::Closure(Arc::new(changed)));
        });
        assert_eq!(DEP_MEMO.lock().unwrap().generation, generation, "unchanged or unread dependency inputs keep warm answers");
        let state = static_eval(&crate::test_workspace_eval::check("(workspace stateful (graph g :context host (state [n 0] (+ n 1))))")).unwrap().results[0].1.clone();
        Host::map_function_bindings(function, &|_| state.clone());
        for value in values { assert!(state_dependent(value), "cached residuals in either home must see edited function captures"); }
        Host::map_function_bindings(function, &|_| Value::Float(0.));
        for value in values { assert!(!state_dependent(value), "removing state must invalidate cached positive answers too"); }
    }

    #[test]
    fn compiler_capture_binding_preserves_original_closure() {
        let checked = crate::test_workspace_eval::check("(workspace w (graph g :context host [(bias : float 3)]
          (let* [f (fn [(x : float)] (+ x bias))] (map f (array/float 2)))))");
        let evaluation = r#static(&checked, true, None).unwrap();
        let Value::Fn(function) = evaluation.records.iter().find(|(p, _)| p == &["g", "f"]).unwrap().1[0].1 else { panic!("closure"); };
        let before = Residual::function_bindings(function);
        assert!(!before.is_empty());
        assert_eq!(bind_captures(function, &Value::clone), function);
        let bound = bind_captures(function, &|_| Value::Float(7.));
        assert_ne!(bound, function);
        assert_eq!(Residual::function_bindings(function), before);
        assert!(Residual::function_bindings(bound).iter().all(|(_, v)| *v == Value::Float(7.)));
    }

    #[test]
    fn shared_named_dependencies_preserve_reads_across_capture_edits() {
        use flow_syntax::context;
        let checked = crate::test_workspace_eval::check("(workspace w
          (defn leaf :context host [] t)
          (defn left :context host [] (leaf))
          (defn right :context host [] (leaf))
          (defn diamond :context host [] (+ (left) (right)))
          (graph g :context host [(x : float 0)] (let* [f (fn [] (+ x (diamond)))] (f))))");
        let evaluation = r#static(&checked, true, None).unwrap();
        let function = &evaluation.records.iter().find(|(path, _)| path == &["g", "f"]).unwrap().1[0].1;
        let Value::Fn(id) = function else { panic!("capturing function"); };
        assert_eq!(frame_reads(function), vec!["time"]);
        assert!(frame_dependent(function));
        assert!(!state_dependent(function));

        let state = crate::test_workspace_eval::check("(workspace stateful (graph g :context host (state [n 0] (+ n 1))))");
        let state = static_eval(&state).unwrap().results[0].1.clone();
        Host::map_function_bindings(*id, &|_| state.clone());
        assert_eq!(frame_reads(function), vec!["state", "time"]);
        assert!(state_dependent(function));
        Host::map_function_bindings(*id, &|_| Value::Float(0.));
        assert_eq!(frame_reads(function), vec!["time"]);
        assert!(!state_dependent(function));

        // A packed body can be shared by definitions with different operator contexts.
        let mut scoped = crate::test_workspace_eval::check("(workspace w
          (defn quiet :context value [] (pointer/x))
          (defn live :context value [] (pointer/x))
          (graph g :context host 0))");
        let defs = Arc::make_mut(&mut scoped.defs);
        defs[0].context = context::sop();
        defs[1].context = context::draw();
        let mut terms = TermBuilder::new();
        let form = terms.import_form(&defs[0].form);
        let body = terms.push(None, &Ty::Float, form, Draft::Op { op: "probe".into(), args: Vec::new(), skip: Vec::new() });
        defs[0].body = terms.freeze().handle(body);
        defs[1].body = defs[0].body.clone();
        let mut quiet = op::find(&[], "pointer/x", &context::value()).unwrap();
        quiet.name = "sop/probe".into();
        quiet.ctx = context::sop();
        quiet.live = false;
        let mut live = quiet.clone();
        live.name = "draw/probe".into();
        live.ctx = context::draw();
        live.live = true;
        scoped.ops = Arc::new(vec![quiet, live]);
        let slot = Arena::new(&scoped, false, 1000);
        let mut arena = checkout(&slot);
        let (mut active, mut named) = (HashSet::new(), HashMap::new());
        assert!(!dep_named(&mut arena, "quiet", true, &mut active, &mut named));
        assert!(dep_named(&mut arena, "live", true, &mut active, &mut named));
        let (mut reads, mut residuals, mut functions, mut named) = (BTreeSet::new(), HashSet::new(), HashSet::new(), HashSet::new());
        assert!(!read_named(&mut arena, "quiet", true, &mut reads, &mut residuals, &mut functions, &mut named));
        assert!(read_named(&mut arena, "live", true, &mut reads, &mut residuals, &mut functions, &mut named));
        assert_eq!(reads.into_iter().collect::<Vec<_>>(), vec!["probe"]);
    }

    #[test]
    fn foreign_dependencies_are_independent_of_query_order_and_numeric_ids() {
        for same in [false, true] {
            let value = |body| {
                let checked = crate::test_workspace_eval::check(&format!("(workspace w (graph g :context host {body}))"));
                let value = static_eval(&checked).unwrap().results[0].1.clone();
                let next = Cell::new(-8_440_299);
                if same { Host::rewrite_with(&value, &|_| None, &|| { let id = next.get(); next.set(id - 1); id }, &|_, _| None) } else { value }
            };
            let (clock, state) = (value("t"), value("(state [n 0] (+ n 1))"));
            let both = Value::Record(vec![("clock".into(), clock.clone()), ("state".into(), state.clone())]);
            assert!(state_dependent(&both), "cold mixed-home state, same IDs={same}");
            assert!(frame_dependent(&both));
            assert_eq!(frame_reads(&both), vec!["state", "time"]);
            assert!(!state_dependent(&clock));
            assert!(state_dependent(&state));
            assert!(state_dependent(&both));
            assert_eq!(frame_reads(&both), vec!["state", "time"]);

            let caller = crate::test_workspace_eval::check("(workspace w (graph g :context host [(x : float 0.0)] (let* [f (fn [] (+ x t))] (f))))");
            let evaluation = r#static(&caller, true, Some(&[("g".into(), vec![("x".into(), state.clone())])])).unwrap();
            let function = evaluation.records.iter().find(|(path, _)| path == &["g", "f"]).unwrap().1[0].1.clone();
            let captured = &evaluation.results[0].1;
            for value in [&function, captured] {
                assert!(state_dependent(value), "foreign state captured by a closure/residual");
                assert_eq!(frame_reads(value), vec!["state", "time"]);
            }
            // Opposite traversal orders must release the current home before entering another.
            let barrier = std::sync::Barrier::new(2);
            std::thread::scope(|scope| {
                for values in [vec![captured.clone(), clock.clone()], vec![clock.clone(), captured.clone()]] {
                    let barrier = &barrier;
                    scope.spawn(move || {
                        barrier.wait();
                        let value = Value::List(Arc::new(values));
                        for _ in 0..16 { assert_eq!(frame_reads(&value), vec!["state", "time"]); }
                    });
                }
            });
        }
    }

    #[test]
    fn rewriting_foreign_functions_preserves_both_homes_and_shared_captures() {
        let function = |add| {
            let checked = crate::test_workspace_eval::check(&format!("(workspace w (graph g :context value (let* [f (fn [(x : float)] (+ x {add}))] 0)))"));
            let evaluation = r#static(&checked, true, None).unwrap();
            let value = &evaluation.records.iter().find(|(path, _)| path == &["g", "f"]).unwrap().1[0].1;
            Host::rewrite_with(value, &|_| None, &|| -8_440_099, &|_, _| None)
        };
        let (left, right) = (function(1), function(2));
        let (Value::Fn(a), Value::Fn(b)) = (&left, &right) else { panic!("recorded functions"); };
        let (a, b) = (*a, *b);
        assert_eq!(a.0, b.0);
        assert_ne!(a.1, b.1);
        let rewritten = Host::rewrite(&Value::List(Arc::new(vec![left.clone(), right.clone(), left])), &|_| None);
        let Value::List(values) = rewritten else { panic!("function list"); };
        let ids: Vec<_> = values.iter().map(|value| match value { Value::Fn(id) => (id.1, id.0), _ => panic!("function") }).collect();
        assert_ne!(ids[0], ids[1], "foreign closures must not share one rewritten body");
        assert_eq!(ids[0], ids[2], "repeated captures of the same closure stay shared");
        assert_eq!((ids[0].0, ids[1].0), (a.1, b.1));
    }

    #[test]
    fn state_source_digest_is_lazy_exact_and_shared_by_replays() {
        let checked = crate::test_workspace_eval::check("(workspace w (graph g :context value (state [n 0] (+ n 1))))");
        let arena = Arena::fresh(0, Shared::new(checked.clone()), false, 0, None, false);
        let replay = arena.clone();
        assert!(arena.source_key.get().is_none());
        assert_eq!(workspace_source_key(&replay), stable_digest(flow_syntax::lisp::text(&checked.source).as_bytes()));
        assert!(Arc::ptr_eq(&arena.source_key, &replay.source_key));
        assert_eq!(arena.source_key.get().unwrap(), workspace_source_key(&replay));
    }

    #[test]
    fn escaping_roots_retain_foreign_homes_without_cycles() {
        let checked = crate::test_workspace_eval::check("(workspace w (graph g :context host (+ t 1.0)))");
        let first = static_eval(&checked).unwrap();
        let imported = first.results[0].1.clone();
        let caller = crate::test_workspace_eval::check("(workspace w (graph g :context host [(x : float 0.0)] (+ x t)))");
        let inputs = vec![("g".to_owned(), vec![("x".to_owned(), imported)])];
        let second = static_with(&caller, false, Some(&inputs), None).unwrap();
        let value = second.results[0].1.clone();
        let roots = roots(std::iter::once(&value));
        assert_eq!(roots.0.len(), 2, "the residual also owns its imported capture's home");
        let homes: Vec<_> = roots.0.iter().map(Arc::downgrade).collect();
        drop((first, second));
        // Evict both homes from the legacy convenience ring, without changing its capacity.
        for _ in 0..65 { drop(static_eval(&checked).unwrap()); }
        assert!(homes.iter().all(|home| home.upgrade().is_some()));
        assert_eq!(force(&value, &input::at_time(3.0), None).unwrap(), Value::Float(7.0));
        drop(roots);
        assert!(homes.iter().all(|home| home.upgrade().is_none()), "raw ids and captured ids cannot keep their arenas alive");
    }
}
