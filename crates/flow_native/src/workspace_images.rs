//! Workspace images retain bounded load/noise/map/render/exact caches with CPU snapshots and GPU textures. Reentrant child resolution uses brief Cell/RefCell borrows of one Rc owner. Send+Sync resolver closures carry an ID resolved through a thread-local table; foreign-thread resolution is a typed error. Plan IDs are assigned at construction and preserved by clones.
use flow_syntax::symbol::Symbol;
use crate::drawing;
use crate::workspace_gpu;
use crate::workspace_resources;
use flow_check::workspace::{Path, Paths};
use flow_eval as e;
use flow_gpu::image_sink;
use flow_gpu::small_lru::List;
use flow_ir::flow_ir::{executor as I, gpu as G};
use flow_sop::{attribute_kernel, image_kernel, lower as l, network, value_lane};
use flow_syntax::ty;
use flow_syntax::value::{self as v, FnId, Value};
use input::Frame;
use rays::{canvas, image, texture};
use rays_diagnostic::Diagnostic;
use sop::context::dependencies as deps;
use sop::{context, edit_graph, node, payload, session};
use std::cell::{Cell, OnceCell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

static NEXT: AtomicI64 = AtomicI64::new(1);

const CAPTURE_LIMIT: i64 = 64 * 1024 * 1024;
const ENTRY_CAPACITY: usize = 64;

pub type PlanId = u64;

/// A plan ID is assigned at construction and retained by clones.
pub fn plan_id(plan: &e::Plan) -> PlanId {
    plan.id
}

struct Captured {
    geometry: rdk_core::geometry::T,
    attributes: Vec<(Symbol, Value)>,
    bytes: i64,
}

struct Source {
    network: network::Network,
    cone: network::Network,
    id: i64,
    lane: Rc<value_lane::Lane>,
    state_stamp: e::StateStamp,
    live: Frame,
    geometry: Option<edit_graph::T>,
    compiled: Option<edit_graph::Compiled>,
    root: Option<node::T>,
    dependencies: deps::T,
    projection: Vec<u8>,
    revision: i64,
    materializer: Option<node::T>,
}

type SourceRef = Rc<RefCell<Source>>;

struct Request {
    active: Vec<(PlanId, i64)>,
    pending_images: i64,
    session: Option<session::T>,
    contexts: Vec<(PlanId, l::ImageContext)>,
    cooked: Vec<(i64, i64, Rc<Captured>)>,
}

type RequestRef = Rc<RefCell<Request>>;

struct Stamp {
    serial: i64,
    frame: Frame,
    state_stamp: Option<e::StateStamp>,
    sources: Vec<(i64, i64)>,
}

#[derive(Clone)]
struct Snapshot {
    payload: sop::image::T,
    stamp: Rc<Stamp>,
}

enum TextureSlot {
    /// built from the CPU payload on first use
    Lazy(sop::image::T, OnceCell<Result<texture::Texture, String>>),
    Ready(texture::Texture),
}

/// Exact in-process image identity; file paths remain OS data, while authored sites retain Symbols.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImageKey {
    Load(String),
    NoiseLive(i64, i64),
    Noise(String),
    Render((i64, flow_ir::flow_ir::Id)),
    Map((i64, flow_ir::flow_ir::Id)),
    Exact((i64, flow_ir::flow_ir::Id), (i64, i64, i64, i64, i64)),
}

struct Entry {
    key: ImageKey,
    /// the plan that last asked for it: `reserve` prunes the others
    plan_serial: i64,
    image: Option<image::Image>,
    cpu: Option<Snapshot>,
    texture: Option<Rc<TextureSlot>>,
    borrowed: Option<texture::Texture>,
    display: Option<Rc<Stamp>>,
    canvas: Option<canvas::Canvas>,
}

type EntryRef = Rc<RefCell<Entry>>;

fn make_entry(key: &ImageKey) -> EntryRef {
    Rc::new(RefCell::new(Entry { key: key.to_owned(), plan_serial: -1, image: None, cpu: None, texture: None, borrowed: None, display: None, canvas: None }))
}

/// The image entries: a second-chance table (`Lru.Make`'s order: oldest first, a touched entry
/// moves to newest in a lap) of at most 64, whose victims are the entries the current plan has not
/// asked for.
struct Entries {
    /// oldest first: (key, entry, touched, bytes)
    slots: Vec<(ImageKey, EntryRef, bool, i64)>,
}

impl Entries {
    fn length(&self) -> usize {
        self.slots.len()
    }
    fn find_opt(&mut self, key: &ImageKey) -> Option<EntryRef> {
        let slot = self.slots.iter_mut().find(|s| &s.0 == key)?;
        slot.2 = true;
        Some(slot.1.clone())
    }
    fn iter(&self) -> Vec<EntryRef> {
        self.slots.iter().map(|s| s.1.clone()).collect()
    }
    /// The first untouched evictable entry; a touched or pinned one moves to newest, untouched.
    fn victim(&mut self, serial: i64) -> Option<usize> {
        let mut remaining = self.slots.len();
        let mut at = 0;
        while remaining > 0 && !self.slots.is_empty() {
            let slot = &self.slots[at];
            if slot.2 || slot.1.borrow().plan_serial == serial {
                let mut moved = self.slots.remove(at);
                moved.2 = false;
                self.slots.push(moved);
                remaining -= 1;
                if self.slots.len() == 1 {
                    return None;
                }
                if at >= self.slots.len() - 1 {
                    at = 0;
                }
                continue;
            }
            return Some(at);
        }
        None
    }
}

struct CaptureCache {
    /// newest first: (id, (revision, captured, bytes))
    items: Vec<(i64, (i64, Rc<Captured>, i64))>,
    bytes: i64,
}

impl CaptureCache {
    fn remove(&mut self, id: i64) {
        if let Some(at) = self.items.iter().position(|(k, _)| *k == id) {
            let (_, (_, _, bytes)) = self.items.remove(at);
            self.bytes -= bytes;
        }
    }
    fn find_opt(&mut self, id: i64) -> Option<(i64, Rc<Captured>)> {
        let at = self.items.iter().position(|(k, _)| *k == id)?;
        let item = self.items.remove(at);
        let found = (item.1 .0, item.1 .1.clone());
        self.items.insert(0, item);
        Some(found)
    }
    fn add(&mut self, id: i64, revision: i64, captured: Rc<Captured>, bytes: i64) {
        self.remove(id);
        self.items.insert(0, (id, (revision, captured, bytes)));
        self.bytes += bytes;
        while self.items.len() > 64 || self.bytes > CAPTURE_LIMIT {
            match self.items.pop() {
                Some((_, (_, _, b))) => self.bytes -= b,
                None => break,
            }
        }
    }
}

pub struct Data {
    id: i64,
    resources: workspace_resources::WorkspaceResources,
    gpu: workspace_gpu::WorkspaceGpu,
    seed: i64,
    grain: i64,
    domains: i64,
    plan: RefCell<Option<Rc<e::Plan>>>,
    plan_id: Cell<Option<PlanId>>,
    serial: Cell<i64>,
    lowered: RefCell<Option<(PlanId, Rc<l::T>)>>,
    request: RefCell<Option<RequestRef>>,
    revision: Cell<i64>,
    source_cache: RefCell<List<i64, SourceRef>>,
    capture_cache: Rc<RefCell<CaptureCache>>,
    source_ids: RefCell<Vec<Vec<i64>>>,
    freshness: RefCell<Vec<(bool, deps::T)>>,
    qualification: RefCell<Option<(PlanId, Paths, Vec<Option<Path>>)>>,
    arguments: RefCell<Vec<Option<I::Program>>>,
    drawings: RefCell<Vec<Option<Rc<drawing::prepared>>>>,
    maps: RefCell<Vec<Option<(i64, i64, FnId, image_kernel::T)>>>,
    sizes: RefCell<Vec<(bool, bool)>>,
    resolved: RefCell<Vec<Option<EntryRef>>>,
    entries: RefCell<Entries>,
    source_cooks: Cell<i64>,
    attribute_flattens: Cell<i64>,
    canvases_created: Cell<i64>,
    canvases_destroyed: Cell<i64>,
    captures: Cell<i64>,
    readbacks: Cell<i64>,
}

#[derive(Clone)]
pub struct WorkspaceImages(pub Rc<Data>);

thread_local! {
    static OWNERS: RefCell<HashMap<i64, Rc<Data>>> = RefCell::new(HashMap::new());
    /// The capture resolvers running on this thread, innermost last (see `with_capture`).
    static CAPTURES: RefCell<Vec<Rc<dyn Fn(&Value) -> Result<Value, Diagnostic>>>> = const { RefCell::new(Vec::new()) };
}

fn owner_of(id: i64) -> Option<Rc<Data>> {
    OWNERS.with(|o| o.borrow().get(&id).cloned())
}

pub fn create(seed: Option<i64>, grain: Option<i64>, domains: Option<i64>, gpu: &workspace_gpu::WorkspaceGpu, resources: &workspace_resources::WorkspaceResources) -> WorkspaceImages {
    let capture_cache = Rc::new(RefCell::new(CaptureCache { items: Vec::new(), bytes: 0 }));
    let cache = capture_cache.clone();
    let data = Rc::new(Data {
        id: NEXT.fetch_add(1, Ordering::SeqCst),
        resources: resources.clone(),
        gpu: gpu.clone(),
        seed: seed.unwrap_or(0),
        grain: grain.unwrap_or(16384),
        domains: domains.unwrap_or_else(parallel::recommended_domains),
        plan: RefCell::new(None),
        plan_id: Cell::new(None),
        serial: Cell::new(0),
        lowered: RefCell::new(None),
        request: RefCell::new(None),
        revision: Cell::new(0),
        // count only: a source is its network, which the document holds; captures are charged
        source_cache: RefCell::new(List::new(64, Box::new(move |id: &i64, _| cache.borrow_mut().remove(*id)))),
        capture_cache,
        source_ids: RefCell::new(Vec::new()),
        freshness: RefCell::new(Vec::new()),
        qualification: RefCell::new(None),
        arguments: RefCell::new(Vec::new()),
        drawings: RefCell::new(Vec::new()),
        maps: RefCell::new(Vec::new()),
        sizes: RefCell::new(Vec::new()),
        resolved: RefCell::new(Vec::new()),
        entries: RefCell::new(Entries { slots: Vec::new() }),
        source_cooks: Cell::new(0),
        attribute_flattens: Cell::new(0),
        canvases_created: Cell::new(0),
        canvases_destroyed: Cell::new(0),
        captures: Cell::new(0),
        readbacks: Cell::new(0),
    });
    OWNERS.with(|o| o.borrow_mut().insert(data.id, data.clone()));
    WorkspaceImages(data)
}

fn error(message: &str) -> Diagnostic {
    v::fail("E_IMAGE", message)
}

fn message<A>(result: Result<A, String>) -> Result<A, Diagnostic> {
    result.map_err(|m| error(&m))
}

fn pp_error(e: &ogpu_core::error::Error) -> String {
    ogpu_core::error::to_string(e)
}

// -- entry table operations that release ---------------------------------------------------------

fn release_entry(d: &Data, entry: &EntryRef) {
    let canvas = entry.borrow_mut().canvas.take();
    if let Some(canvas) = canvas {
        let (captures, readbacks) = canvas::pixel_stats(&canvas);
        let _ = canvas::destroy(&canvas);
        d.canvases_destroyed.set(d.canvases_destroyed.get() + 1);
        d.captures.set(d.captures.get() + captures);
        d.readbacks.set(d.readbacks.get() + readbacks);
    }
    let mut e = entry.borrow_mut();
    e.image = None;
    e.cpu = None;
    e.texture = None;
    e.borrowed = None;
    e.display = None;
}

fn entries_add(d: &Data, key: &ImageKey, entry: &EntryRef, bytes: i64) {
    let replaced = {
        let mut es = d.entries.borrow_mut();
        match es.slots.iter().position(|s| &s.0 == key) {
            Some(at) => {
                let old = es.slots.remove(at);
                Some(old.1)
            }
            None => None,
        }
    };
    if let Some(old) = replaced {
        // the same entry again is its size changing, not an entry leaving
        if !Rc::ptr_eq(&old, entry) {
            release_entry(d, &old);
        }
    }
    d.entries.borrow_mut().slots.push((key.to_owned(), entry.clone(), false, bytes));
    while d.entries.borrow().length() > ENTRY_CAPACITY {
        if !drop_oldest(d) {
            break;
        }
    }
}

fn drop_oldest(d: &Data) -> bool {
    let victim = {
        let mut es = d.entries.borrow_mut();
        let at = es.victim(d.serial.get());
        at.map(|at| es.slots.remove(at))
    };
    match victim {
        Some((_, entry, _, _)) => {
            release_entry(d, &entry);
            true
        }
        None => false,
    }
}

/// A slot for one more image. The plan decides who goes: only entries the current plan has not
/// asked for (a deleted node's, an old time's), oldest first. A plan that truly needs more than the
/// capacity gets the typed refusal it always did, and no slot is consumed by asking.
fn reserve(d: &Data, request: &RequestRef) -> Result<(), Diagnostic> {
    loop {
        if (d.entries.borrow().length() as i64) + request.borrow().pending_images < ENTRY_CAPACITY as i64 {
            return Ok(());
        }
        if !drop_oldest(d) {
            return Err(error("A workspace owns at most 64 image snapshots."));
        }
    }
}

fn entry_bytes(entry: &Entry) -> i64 {
    match &entry.cpu {
        // doubles, four a pixel
        Some(s) => sop::image::width(&s.payload) * sop::image::height(&s.payload) * 4 * 8,
        None => 0,
    }
}

pub fn capture_stats(t_: &WorkspaceImages) -> (i64, i64, i64, i64, i64) {
    let d = &*t_.0;
    let cache = d.capture_cache.borrow();
    (d.source_cache.borrow().length() as i64, cache.items.len() as i64, cache.bytes, d.source_cooks.get(), d.attribute_flattens.get())
}

fn cook_context(d: &Data, live: &Frame) -> Result<context::T, Diagnostic> {
    context::create(context::Options { seed: Some(d.seed), grain: Some(d.grain), domains: Some(d.domains), input: Some(live.clone()), time: Some(live.t), frame: Some(live.frame), ..Default::default() }).map_err(|m| error(&m))
}

fn with_active(request: &RequestRef, pid: PlanId, id: i64, run: &mut dyn FnMut() -> Result<EntryRef, Diagnostic>) -> Result<EntryRef, Diagnostic> {
    if request.borrow().active.iter().any(|(p, n)| *p == pid && *n == id) {
        return Err(v::fail("E_IMAGE_CYCLE", "Image and geometry dependencies form a cycle."));
    }
    request.borrow_mut().active.insert(0, (pid, id));
    let result = run();
    let mut r = request.borrow_mut();
    if !r.active.is_empty() {
        r.active.remove(0);
    }
    result
}

/// A session for one cook, taken out of the request (a nested cook makes its own): it keeps
/// nothing (`max_entries` 0), so the sessions are interchangeable.
fn take_session(request: &RequestRef) -> Result<session::T, Diagnostic> {
    let existing = request.borrow_mut().session.take();
    match existing {
        Some(s) => Ok(s),
        None => session::create(0, 0).map_err(|m| error(&m)),
    }
}

fn return_session(request: &RequestRef, mut s: session::T) {
    let mut r = request.borrow_mut();
    if r.session.is_none() {
        r.session = Some(s);
    } else {
        session::close(&mut s);
    }
}

fn cook_node(request: &RequestRef, context: &context::T, n: &node::T) -> Result<session::Output, Diagnostic> {
    let mut s = take_session(request)?;
    let result = session::cook(&mut s, None, context, n);
    return_session(request, s);
    result
}

fn source_context(d: &Data, request: &RequestRef, pid: PlanId, id: i64) -> Result<l::ImageContext, Diagnostic> {
    let contains = |context: &l::ImageContext| context.compiled.get(&id).is_some_and(|cid| edit_graph::find(&context.network.geometry, *cid).is_some());
    let found = request.borrow().contexts.iter().find(|(p, c)| *p == pid && contains(c)).map(|(_, c)| c.clone());
    if let Some(context) = found {
        return Ok(context);
    }
    let lowered = d.lowered.borrow().clone();
    match lowered {
        Some((lid, lowered)) if lid == pid => {
            let context = l::source_context(&lowered, id)?;
            request.borrow_mut().contexts.insert(0, (pid, context.clone()));
            Ok(context)
        }
        _ => Err(v::fail("E_DATA_SOURCE", "Capture needs its current owning network.")),
    }
}

fn prepare_source(d: &Data, gpu: Option<&G::Config>, request: &RequestRef, state: &e::State, live: &Frame, pid: PlanId, id: i64) -> Result<SourceRef, Diagnostic> {
    let owner = source_context(d, request, pid, id)?;
    let state_stamp = e::state_stamp(state);
    let cached = d.source_cache.borrow_mut().find(&|k, _| *k == id).cloned();
    let source: SourceRef = match cached {
        Some(source) if network::ptr_eq(&source.borrow().network, &owner.network) => source,
        _ => {
            let (cone, root) = l::source_cone(&owner, id)?;
            let source = Rc::new(RefCell::new(Source {
                network: owner.network,
                cone,
                id: root,
                lane: Rc::new(value_lane::create(Some(e::fork_state(state)))),
                state_stamp,
                live: live.clone(),
                geometry: None,
                compiled: None,
                root: None,
                dependencies: deps::STATIC,
                projection: Vec::new(),
                revision: 0,
                materializer: None,
            }));
            d.source_cache.borrow_mut().remove(&|k, _| *k == id);
            d.source_cache.borrow_mut().add(id, source.clone());
            source
        }
    };
    {
        let mut s = source.borrow_mut();
        if s.state_stamp != state_stamp || (!s.cone.states.is_empty() && context::input_changed(value_lane::dependencies(&s.cone), &s.live, live)) {
            s.lane = Rc::new(value_lane::create(Some(e::fork_state(state))));
            s.state_stamp = state_stamp;
        }
    }
    let (lane, cone) = {
        let s = source.borrow();
        (s.lane.clone(), s.cone.clone())
    };
    let resolved = value_lane::resolve(gpu, Some(live), &lane, live.t, &cone)?;
    let (compiled, root_id, previous_root, previous_deps, previous_projection) = {
        let s = source.borrow();
        let compiled = match (&s.geometry, &s.compiled) {
            (Some(g), Some(c)) if edit_graph::ptr_eq(g, &resolved.geometry) => c.clone(),
            _ => edit_graph::compile_all(s.compiled.as_ref(), &resolved.geometry),
        };
        (compiled, s.id, s.root.clone(), s.dependencies, s.projection.clone())
    };
    let root = edit_graph::compiled_node(&compiled, root_id).map_err(|m| v::fail("E_DATA_SOURCE", &m))?;
    let unchanged = previous_root.as_ref().is_some_and(|r| node::ptr_eq(r, &root));
    let dependencies = if unchanged { previous_deps } else { node::graph_dependencies(&root) };
    let context = cook_context(d, live)?;
    let projection = context::cache_projection(dependencies, &context);
    let mut s = source.borrow_mut();
    if !unchanged || previous_projection != projection {
        d.revision.set(d.revision.get() + 1);
        s.revision = d.revision.get();
        d.capture_cache.borrow_mut().remove(id);
        s.materializer = Some(match &s.materializer {
            None => attribute_kernel::materialized_source(&root),
            Some(consumer) => node::build::rebuild_with_inputs(consumer, std::slice::from_ref(&root)),
        });
    }
    s.compiled = Some(compiled);
    s.root = Some(root);
    s.geometry = Some(resolved.geometry);
    s.live = live.clone();
    s.dependencies = dependencies;
    s.projection = projection;
    drop(s);
    Ok(source)
}

fn retain_capture(d: &Data, id: i64, revision: i64, captured: &Rc<Captured>) {
    let current = d.source_cache.borrow().peek(&|k, _| *k == id).is_some_and(|s| s.borrow().revision == revision);
    if current {
        if captured.bytes > CAPTURE_LIMIT {
            d.capture_cache.borrow_mut().remove(id);
        } else {
            d.capture_cache.borrow_mut().add(id, revision, captured.clone(), captured.bytes);
        }
    }
}

fn captured(d: &Data, request: &RequestRef, live: &Frame, id: i64, source: &SourceRef) -> Result<Rc<Captured>, Diagnostic> {
    let revision = source.borrow().revision;
    let found = request.borrow().cooked.iter().find(|(n, r, _)| *n == id && *r == revision).map(|(_, _, c)| c.clone());
    if let Some(c) = found {
        return Ok(c);
    }
    let cached = d.capture_cache.borrow_mut().find_opt(id);
    let captured = match cached {
        Some((r, c)) if r == revision => c,
        _ => {
            let context = cook_context(d, live)?;
            let materializer = source.borrow().materializer.clone().expect("materializer");
            let output = cook_node(request, &context, &materializer)?;
            let geometry = payload::geometry(&output.payload)?;
            d.source_cooks.set(d.source_cooks.get() + 1);
            let bytes = rdk_core::geometry::payload_bytes(&geometry);
            Rc::new(Captured { geometry, attributes: Vec::new(), bytes })
        }
    };
    request.borrow_mut().cooked.insert(0, (id, revision, captured.clone()));
    retain_capture(d, id, revision, &captured);
    Ok(captured)
}

fn resolve_capture(d: &Data, request: &RequestRef, live: &Frame, sources: &[(i64, SourceRef)], value: &Value) -> Result<Value, Diagnostic> {
    let Value::Struct(g) = value else { return Err(v::fail("E_DATA_SOURCE", "Unknown packed data source.")) };
    let (name, args) = (&g.name, &g.fields);
    if name != "sop/attr" {
        return Err(v::fail("E_DATA_SOURCE", "Unknown packed data source."));
    }
    let (Some(Value::Deferred(gty, id)), Some(Value::Text(attr))) = (args.get_str("geometry"), args.get_str("attribute")) else {
        return Err(v::fail("E_ATTR_TYPE", "sop/attr needs geometry and an attribute name."));
    };
    if !ty::is_geometry(gty) {
        return Err(v::fail("E_ATTR_TYPE", "sop/attr needs geometry and an attribute name."));
    }
    let name = flow_syntax::symbol::intern(String::from_utf8_lossy(attr).as_ref());
    let Some((_, source)) = sources.iter().find(|(n, _)| n == id) else { return Err(v::fail("E_DATA_SOURCE", "Attribute source was not prepared.")) };
    let c = captured(d, request, live, *id, source)?;
    if let Some((_, value)) = c.attributes.iter().find(|(n, _)| *n == name) {
        return Ok(value.clone());
    }
    let (geometry, node_id) = (c.geometry.clone(), *id);
    let lookup: attribute_kernel::GeometryLookup = Arc::new(move |n| if n == node_id { Some(geometry.clone()) } else { None });
    let resolved = attribute_kernel::resolve(None, &lookup, value)?;
    d.attribute_flattens.set(d.attribute_flattens.get() + 1);
    let size = match &resolved {
        Value::Floats(3, values) => values.len() as i64 * 8 + 16,
        _ => 0,
    };
    let bytes = if size > i64::MAX - c.bytes { i64::MAX } else { c.bytes + size };
    let mut attributes = c.attributes.clone();
    attributes.insert(0, (name, resolved.clone()));
    let updated = Rc::new(Captured { geometry: c.geometry.clone(), attributes, bytes });
    {
        let mut r = request.borrow_mut();
        r.cooked.retain(|(n, _, _)| *n != *id);
        let revision = source.borrow().revision;
        r.cooked.insert(0, (*id, revision, updated.clone()));
    }
    let revision = source.borrow().revision;
    retain_capture(d, *id, revision, &updated);
    Ok(resolved)
}

fn create_canvas(d: &Data, width: i64, height: i64) -> Result<canvas::Canvas, Diagnostic> {
    let c = message(canvas::create(width, height))?;
    d.canvases_created.set(d.canvases_created.get() + 1);
    Ok(c)
}

fn destroy_canvas(d: &Data, c: &canvas::Canvas) {
    let (captures, readbacks) = canvas::pixel_stats(c);
    let _ = canvas::destroy(c);
    d.canvases_destroyed.set(d.canvases_destroyed.get() + 1);
    d.captures.set(d.captures.get() + captures);
    d.readbacks.set(d.readbacks.get() + readbacks);
}

/// (canvases created, destroyed, captures, readbacks)
pub fn render_stats(t_: &WorkspaceImages) -> (i64, i64, i64, i64) {
    let d = &*t_.0;
    let (mut captures, mut readbacks) = (d.captures.get(), d.readbacks.get());
    for entry in d.entries.borrow().iter() {
        if let Some(c) = &entry.borrow().canvas {
            let (cap, read) = canvas::pixel_stats(c);
            captures += cap;
            readbacks += read;
        }
    }
    (d.canvases_created.get(), d.canvases_destroyed.get(), captures, readbacks)
}

fn authored_key(node: &e::Node) -> (i64, flow_ir::flow_ir::Id) {
    (node.inst, (node.site.clone(), node.iter.clone()))
}

fn invalidate_display(entry: &EntryRef) {
    let canvas = {
        let mut e = entry.borrow_mut();
        e.display = None;
        e.canvas.clone()
    };
    if let Some(c) = canvas {
        let _ = canvas::invalidate(&c);
    }
}

fn size_dependencies(plan: &e::Plan, root: i64) -> (bool, bool) {
    struct Walk<'a> {
        plan: &'a e::Plan,
        nodes: Vec<bool>,
        functions: HashSet<i64>,
        residuals: HashSet<i64>,
        width: bool,
        height: bool,
    }
    fn visit(w: &mut Walk, value: &Value) {
        if w.width && w.height {
            return;
        }
        match value {
            Value::Deferred(_, id) if *id >= 0 && (*id as usize) < w.nodes.len() && !w.nodes[*id as usize] => {
                w.nodes[*id as usize] = true;
                let node = &w.plan.nodes[*id as usize];
                if node.kind == "image/render" {
                    w.width = w.width || !node.args.iter().any(|(k, _)| k == "width");
                    w.height = w.height || !node.args.iter().any(|(k, _)| k == "height");
                }
                for (_, x) in &node.args {
                    visit(w, x);
                }
            }
            Value::List(values) => values.iter().for_each(|x| visit(w, x)),
            v @ (Value::Record(_) | Value::Struct(_)) => v.geo_fields().unwrap().iter().for_each(|(_, x)| visit(w, x)),
            Value::Fn(f) if !w.functions.contains(&f.0) => {
                w.functions.insert(f.0);
                for (_, x) in e::residual::function_bindings(*f) {
                    visit(w, &x);
                }
            }
            Value::Residual(r) if !w.residuals.contains(&r.0) => {
                w.residuals.insert(r.0);
                if let Ok(view) = e::residual::residual_view(*r) {
                    for (_, x) in view.bindings.iter() {
                        visit(w, x);
                    }
                }
            }
            _ => {}
        }
    }
    let mut w = Walk { plan, nodes: vec![false; plan.nodes.len()], functions: HashSet::new(), residuals: HashSet::new(), width: false, height: false };
    visit(&mut w, &Value::Deferred(ty::image(), root));
    (w.width, w.height)
}

fn image_dependencies(plan: &e::Plan, root: i64) -> (Vec<i64>, (bool, deps::T)) {
    struct Walk<'a> {
        plan: &'a e::Plan,
        nodes: Vec<bool>,
        ids: Vec<i64>,
        stateful: bool,
        reads: deps::T,
        functions: HashSet<i64>,
        residuals: HashSet<i64>,
    }
    fn visit(w: &mut Walk, value: &Value) {
        w.stateful = w.stateful || e::state_dependent(value);
        w.reads = deps::union(w.reads, attribute_kernel::input_reads(value));
        if e::is_live(value) && !e::frame_dependent(value) {
            w.reads = deps::union(w.reads, deps::one(deps::Fact::Input));
        }
        match value {
            Value::Deferred(gty, id) if ty::is_geometry(gty) => w.ids.insert(0, *id),
            Value::Deferred(_, id) if *id >= 0 && (*id as usize) < w.nodes.len() && !w.nodes[*id as usize] => {
                w.nodes[*id as usize] = true;
                let args = w.plan.nodes[*id as usize].args.clone();
                for (_, x) in &args {
                    visit(w, x);
                }
            }
            Value::List(values) => values.iter().for_each(|x| visit(w, x)),
            v @ (Value::Record(_) | Value::Struct(_)) => v.geo_fields().unwrap().iter().for_each(|(_, x)| visit(w, x)),
            Value::Fn(f) if !w.functions.contains(&f.0) => {
                w.functions.insert(f.0);
                for (_, x) in e::residual::function_bindings(*f) {
                    visit(w, &x);
                }
            }
            Value::Residual(r) if !w.residuals.contains(&r.0) => {
                w.residuals.insert(r.0);
                if let Ok(view) = e::residual::residual_view(*r) {
                    for (_, x) in view.bindings.iter() {
                        visit(w, x);
                    }
                }
            }
            _ => {}
        }
    }
    let mut w = Walk { plan, nodes: vec![false; plan.nodes.len()], ids: Vec::new(), stateful: false, reads: deps::STATIC, functions: HashSet::new(), residuals: HashSet::new() };
    visit(&mut w, &Value::Deferred(ty::image(), root));
    let mut ids = w.ids;
    ids.sort();
    ids.dedup();
    (ids, (w.stateful, w.reads))
}

fn current(d: &Data, sources: &[(i64, i64)], state: &e::State, live: &Frame, pid: PlanId, node: &e::Node, old: &Stamp) -> bool {
    if d.plan_id.get() != Some(pid) || old.serial != d.serial.get() || node.id < 0 || node.id as usize >= d.freshness.borrow().len() {
        return false;
    }
    let (stateful, reads) = d.freshness.borrow()[node.id as usize];
    let prepared = match node.kind.as_str() {
        "image/map" => old.serial == d.serial.get() && d.maps.borrow()[node.id as usize].is_some(),
        "image/render" => {
            d.plan_id.get() == Some(pid) && old.serial == d.serial.get() && d.drawings.borrow()[node.id as usize].is_some() && {
                let (width, height) = d.sizes.borrow()[node.id as usize];
                (!width || old.frame.size.0 == live.size.0) && (!height || old.frame.size.1 == live.size.1)
            }
        }
        _ => true,
    };
    prepared && old.sources == sources && (reads == deps::STATIC || (!context::input_changed(reads, &old.frame, live) && old.state_stamp == stateful.then(|| e::state_stamp(state))))
}

/// Binds the lowering the next resolutions read their sources from (the plan it was lowered for).
pub fn bind(t_: &WorkspaceImages, lowered: &l::T) {
    let d = &*t_.0;
    let pid = plan_id(&lowered.plan);
    let same = matches!(&*d.lowered.borrow(), Some((id, _)) if *id == pid);
    if !same {
        // ponytail: the clone is taken once per plan, not per bind; a lowering rebuilt over the same plan vectors is the same lowering
        *d.lowered.borrow_mut() = Some((pid, Rc::new(lowered.clone())));
    }
    let unchanged = matches!(&*d.qualification.borrow(), Some((id, approx, sites)) if *id == pid && *approx == lowered.approx && *sites == lowered.image_sites);
    if !unchanged {
        *d.qualification.borrow_mut() = Some((pid, lowered.approx.clone(), lowered.image_sites.clone()));
        d.plan_id.set(None);
        *d.plan.borrow_mut() = None;
    }
}

fn bytes_of_image(image: &sop::image::T) -> Result<Vec<u8>, Diagnostic> {
    let count = sop::image::channel_count(sop::image::width(image), sop::image::height(image), 1)?;
    let mut out = v::array_buffer(count)?;
    match sop::image::rgba8(image) {
        Some(bytes) => out.extend_from_slice(bytes),
        None => {
            // Clamp, scale, round ties to even: the same bytes Sop.Image.of_vec4 writes.
            out.extend(sop::image::storage(image)?.iter().map(|x| {
                let q = x.min(1.).max(0.) * 255.;
                let n = q.floor() as i64;
                let f = q - n as f64;
                (if f > 0.5 || (f == 0.5 && n & 1 == 1) { n + 1 } else { n }) as u8
            }));
        }
    }
    Ok(out)
}

fn cpu_of_image(image_: &image::Image) -> Result<sop::image::T, Diagnostic> {
    let bytes = message(image::pixels(image_))?;
    let (width, height) = image::dimensions(image_);
    let mut rgba = v::array_buffer::<f64>(bytes.len())?;
    rgba.extend(bytes.iter().map(|b| *b as f64 / 255.));
    sop::image::of_owned_rgba(width, height, rgba)
}

fn texture_slot(payload: &sop::image::T) -> Rc<TextureSlot> {
    Rc::new(TextureSlot::Lazy(payload.clone(), OnceCell::new()))
}

fn force_texture(slot: &TextureSlot) -> Result<texture::Texture, String> {
    match slot {
        TextureSlot::Ready(tex) => Ok(tex.clone()),
        TextureSlot::Lazy(payload, cell) => cell
            .get_or_init(|| {
                let bytes = bytes_of_image(payload).map_err(|d| format!("{}: {}", d.code, d.message))?;
                let mut pixels = v::array_buffer(bytes.len() / 4).map_err(|d| format!("{}: {}", d.code, d.message))?;
                pixels.extend(bytes.chunks(4).map(|c| rays_math::color::rgba(c[0] as i64, c[1] as i64, c[2] as i64, c[3] as i64)));
                texture::create_owned(sop::image::width(payload), sop::image::height(payload), pixels)
            })
            .clone(),
    }
}

fn publish(d: &Data, key: &ImageKey, entry: &EntryRef, width: i64, height: i64, source: Rc<dyn Fn() -> Option<ogpu_core::backend::Texture>>) -> Result<(), Diagnostic> {
    let existing = entry.borrow().image.clone();
    let image_ = match existing {
        None => workspace_resources::image(&d.resources, key, &|| {
            let resource = runtime_resources::image::of_gpu(width, height, source.clone()).map_err(|e| pp_error(&e))?;
            let view = match texture::of_image(&resource) {
                Ok(view) => view,
                Err(m) => {
                    let _ = runtime_resources::image::destroy(&resource);
                    return Err(m);
                }
            };
            match image::of_resource(resource.clone()) {
                Ok(public_image) => {
                    entry.borrow_mut().borrowed = Some(view);
                    Ok(public_image)
                }
                Err(m) => {
                    let _ = runtime_resources::image::destroy(&resource);
                    Err(m)
                }
            }
        })?,
        Some(image_) => {
            runtime_resources::image::replace_gpu_source(&image::resource(&image_), width, height, source.clone()).map_err(|e| error(&pp_error(&e)))?;
            image_
        }
    };
    let borrowed = match entry.borrow().borrowed.clone() {
        Some(view) => view,
        None => message(texture::of_image(&image::resource(&image_)))?,
    };
    let mut e = entry.borrow_mut();
    e.image = Some(image_);
    e.borrowed = Some(borrowed.clone());
    e.texture = Some(Rc::new(TextureSlot::Ready(borrowed)));
    Ok(())
}

fn prepare(d: &Data, plan: &e::Plan, pid: PlanId) -> Result<(), Diagnostic> {
    if d.plan_id.get() == Some(pid) {
        return Ok(());
    }
    let options = I::CompileOptions::default();
    let mut programs: Vec<Option<I::Program>> = Vec::with_capacity(plan.nodes.len());
    for n in &plan.nodes {
        if n.ty == ty::image() {
            programs.push(Some(I::compile(&Value::Record(n.args.clone().into()), &options)?));
        } else {
            programs.push(None);
        }
    }
    *d.plan.borrow_mut() = Some(Rc::new(plan.clone()));
    d.plan_id.set(Some(pid));
    d.serial.set(d.serial.get() + 1);
    *d.arguments.borrow_mut() = programs;
    *d.drawings.borrow_mut() = vec![None; plan.nodes.len()];
    *d.maps.borrow_mut() = (0..plan.nodes.len()).map(|_| None).collect();
    *d.sizes.borrow_mut() = plan.nodes.iter().map(|n| if n.kind == "image/render" { size_dependencies(plan, n.id) } else { (false, false) }).collect();
    d.source_cache.borrow_mut().clear();
    d.capture_cache.borrow_mut().items.clear();
    d.capture_cache.borrow_mut().bytes = 0;
    let dependencies: Vec<(Vec<i64>, (bool, deps::T))> = plan.nodes.iter().map(|n| if n.ty == ty::image() { image_dependencies(plan, n.id) } else { (Vec::new(), (false, deps::STATIC)) }).collect();
    *d.source_ids.borrow_mut() = dependencies.iter().map(|(ids, _)| ids.clone()).collect();
    *d.freshness.borrow_mut() = dependencies.iter().map(|(_, f)| *f).collect();
    *d.resolved.borrow_mut() = vec![None; plan.nodes.len()];
    Ok(())
}

/// The image the plan's node `id` last resolved to, if this plan is the current one.
pub fn peek(t_: &WorkspaceImages, plan: &e::Plan, id: i64) -> Option<image::Image> {
    peek_in(&t_.0, plan_id(plan), id)
}

fn peek_in(d: &Data, pid: PlanId, id: i64) -> Option<image::Image> {
    if workspace_resources::closed(&d.resources) || d.plan_id.get() != Some(pid) || id < 0 || id as usize >= d.resolved.borrow().len() {
        return None;
    }
    let entry = d.resolved.borrow()[id as usize].clone();
    entry.and_then(|e| e.borrow().image.clone())
}

/// Per-resolution inputs shared by the recursion.
#[derive(Clone)]
struct Ctx {
    gpu: Option<G::Config>,
    state: e::State,
    live: Frame,
    pid: PlanId,
}

fn int_arg(args: &v::Record, key: &str, default: i64) -> Result<i64, Diagnostic> {
    match args.get_str(key) {
        None => Ok(default),
        Some(x) => v::int_of(x),
    }
}

fn run_with_capture(f: Rc<dyn Fn(&Value) -> Result<Value, Diagnostic>>, run: &mut dyn FnMut() -> Result<Option<I::Displayed>, Diagnostic>) -> Result<Option<I::Displayed>, Diagnostic> {
    CAPTURES.with(|c| c.borrow_mut().push(f));
    let result = run();
    CAPTURES.with(|c| c.borrow_mut().pop());
    result
}

/// The `Send + Sync` resolver the core demands, over this thread's innermost capture resolver.
fn capture_fn() -> flow_ir::core_exec::ResolveFn {
    let depth = CAPTURES.with(|c| c.borrow().len().saturating_sub(1));
    Arc::new(move |value: &Value| {
        let f = CAPTURES.with(|c| c.borrow().get(depth).cloned());
        match f {
            Some(f) => f(value),
            None => Err(error("Image captures must resolve on the initial domain.")),
        }
    })
}

fn resolve(t_: &Rc<Data>, ctx: &Ctx, context: Option<&l::ImageContext>, display: bool, plan: &e::Plan, value: &Value) -> Result<EntryRef, Diagnostic> {
    let d = &**t_;
    if OWNERS.with(|o| !o.borrow().contains_key(&d.id)) {
        return Err(error("Image resources must resolve on the initial domain."));
    }
    if workspace_resources::closed(&d.resources) {
        return Err(error("Workspace image resources are closed."));
    }
    let mut outcome: Option<Result<EntryRef, Diagnostic>> = None;
    let owner_id = d.id;
    let resolver: l::ImageResolver = Arc::new(move |gpu, context, plan, state, live, value| {
        let Some(d) = owner_of(owner_id) else { return Err(error("Image resources must resolve on the initial domain.")) };
        let ctx = Ctx { gpu: gpu.cloned(), state: state.clone(), live: live.clone(), pid: plan_id(plan) };
        let entry = resolve(&d, &ctx, context, false, plan, value)?;
        let cpu = entry.borrow().cpu.clone();
        Ok(cpu.expect("cpu snapshot").payload)
    });
    let metadata: l::ImageMetadata = Arc::new(move |plan: &e::Plan, id: i64| {
        let d = owner_of(owner_id)?;
        peek_in(&d, plan_id(plan), id).map(|i| image::dimensions(&i))
    });
    l::with_images(ctx.gpu.clone(), Some(metadata), resolver, &mut || {
        outcome = Some(with_request(t_, ctx, context, display, plan, value));
    });
    outcome.expect("with_images ran")
}

fn with_request(t_: &Rc<Data>, ctx: &Ctx, context: Option<&l::ImageContext>, display: bool, plan: &e::Plan, value: &Value) -> Result<EntryRef, Diagnostic> {
    let d = &**t_;
    let existing = d.request.borrow().clone();
    if let Some(request) = existing {
        return run_request(t_, &request, ctx, display, plan, value);
    }
    let request = Rc::new(RefCell::new(Request { active: Vec::new(), pending_images: 0, session: None, contexts: context.map(|c| vec![(ctx.pid, c.clone())]).unwrap_or_default(), cooked: Vec::new() }));
    *d.request.borrow_mut() = Some(request.clone());
    let result = run_request(t_, &request, ctx, display, plan, value);
    let session = request.borrow_mut().session.take();
    if let Some(mut s) = session {
        session::close(&mut s);
    }
    *d.request.borrow_mut() = None;
    result
}

fn run_request(t_: &Rc<Data>, request: &RequestRef, ctx: &Ctx, display: bool, plan: &e::Plan, value: &Value) -> Result<EntryRef, Diagnostic> {
    let d = &**t_;
    let mut run = || -> Result<EntryRef, Diagnostic> {
        let mut outcome: Option<EntryRef> = None;
        e::transaction(&ctx.state, &mut || {
            outcome = Some(resolve_value(t_, request, ctx, display, plan, value)?);
            Ok(())
        })?;
        Ok(outcome.expect("resolved"))
    };
    let result = match value {
        Value::Deferred(_, id) => with_active(request, ctx.pid, *id, &mut run),
        _ => run(),
    };
    if display && result.is_err() {
        if let Value::Deferred(_, id) = value {
            if *id >= 0 && (*id as usize) < plan.nodes.len() {
                let node = &plan.nodes[*id as usize];
                let key = match node.kind.as_str() {
                    "image/render" => Some(ImageKey::Render(authored_key(node))),
                    "image/map" => Some(ImageKey::Map(authored_key(node))),
                    _ => None,
                };
                if let Some(key) = key {
                    let found = d.entries.borrow_mut().find_opt(&key);
                    if let Some(entry) = found {
                        invalidate_display(&entry);
                    }
                }
            }
        }
    }
    result
}

fn resolve_value(t_: &Rc<Data>, request: &RequestRef, ctx: &Ctx, display: bool, plan: &e::Plan, value: &Value) -> Result<EntryRef, Diagnostic> {
    let d = &**t_;
    let (state, live, pid) = (&ctx.state, &ctx.live, ctx.pid);
    if let Value::Deferred(vty, id) = value {
        if *vty == ty::image() && display && *id >= 0 && (*id as usize) < plan.nodes.len() {
            let node = &plan.nodes[*id as usize];
            if node.kind == "image/render" {
                let found = d.entries.borrow_mut().find_opt(&ImageKey::Render(authored_key(node)));
                if let Some(entry) = found {
                    let old = entry.borrow().display.clone();
                    let fresh = old.as_ref().is_some_and(|old| current(d, &old.sources, state, live, pid, node, old));
                    if !fresh {
                        invalidate_display(&entry);
                    }
                }
            }
        }
    }
    prepare(d, plan, pid)?;
    let Value::Deferred(vty, id) = value else { return Err(error("Expected an image value.")) };
    if *vty != ty::image() || *id < 0 || *id as usize >= plan.nodes.len() {
        return Err(error("Expected an image value."));
    }
    let id = *id;
    let program = d.arguments.borrow()[id as usize].clone().expect("image program");
    let forced = I::force(&program, &I::ForceOptions { state: Some(state.clone()), ..Default::default() }, live)?;
    let Value::Record(args) = forced else { unreachable!("assert false") };
    let node = &plan.nodes[id as usize];
    if node.kind == "exact" {
        return resolve_exact(t_, request, ctx, plan, node, &args);
    }
    resolve_producer(t_, request, ctx, display, plan, node, &args)
}

fn resolve_exact(t_: &Rc<Data>, request: &RequestRef, ctx: &Ctx, plan: &e::Plan, node: &e::Node, args: &v::Record) -> Result<EntryRef, Diagnostic> {
    let d = &**t_;
    let child = resolve_with(t_, ctx, true, plan, args.get_str("value").expect("value"))?;
    let source = child.borrow().image.clone().expect("child image");
    let (width, height) = image::dimensions(&source);
    let resource = image::resource(&source);
    runtime_resources::image::gpu_snapshot(&resource).map_err(|e| error(&pp_error(&e)))?;
    let key = ImageKey::Exact(authored_key(node), (d.serial.get(), image::identity(&source), runtime_resources::image::generation(&resource), width, height));
    let found = d.entries.borrow_mut().find_opt(&key);
    let result = match found {
        Some(entry) => entry,
        None => {
            reserve(d, request)?;
            request.borrow_mut().pending_images += 1;
            let made = (|| -> Result<EntryRef, Diagnostic> {
                let payload: RefCell<Option<sop::image::T>> = RefCell::new(None);
                let image_ = workspace_resources::image(&d.resources, &key, &|| {
                    // Admission precedes the read. Pixels returns owned bytes; the native image
                    // copies them and the CPU snapshot owns them.
                    let bytes = image::pixels(&source)?;
                    let cpu = sop::image::of_owned_rgba8(width, height, bytes.clone()).map_err(|d| d.message)?;
                    let image_ = image::upload_rgba(None, width, height, &bytes)?;
                    *payload.borrow_mut() = Some(cpu);
                    Ok(image_)
                })?;
                let payload = payload.borrow_mut().take().expect("payload");
                let stamp = Rc::new(Stamp { serial: d.serial.get(), frame: ctx.live.clone(), state_stamp: None, sources: Vec::new() });
                let entry = Rc::new(RefCell::new(Entry {
                    key: key.clone(),
                    plan_serial: -1,
                    image: Some(image_),
                    cpu: Some(Snapshot { payload: payload.clone(), stamp: stamp.clone() }),
                    texture: Some(texture_slot(&payload)),
                    borrowed: None,
                    display: Some(stamp),
                    canvas: None,
                }));
                let bytes = entry_bytes(&entry.borrow());
                entries_add(d, &key, &entry, bytes);
                Ok(entry)
            })();
            request.borrow_mut().pending_images -= 1;
            made?
        }
    };
    result.borrow_mut().plan_serial = d.serial.get();
    d.resolved.borrow_mut()[node.id as usize] = Some(result.clone());
    Ok(result)
}

/// `resolve` for a child value inside a running request (same plan, state and frame).
fn resolve_with(t_: &Rc<Data>, ctx: &Ctx, display: bool, plan: &e::Plan, value: &Value) -> Result<EntryRef, Diagnostic> {
    // Resolve child images without inheriting the parent's GPU option.
    resolve(t_, &Ctx { gpu: None, ..ctx.clone() }, None, display, plan, value)
}

fn resolve_producer(t_: &Rc<Data>, request: &RequestRef, ctx: &Ctx, display: bool, plan: &e::Plan, node: &e::Node, args: &v::Record) -> Result<EntryRef, Diagnostic> {
    let d = &**t_;
    let (state, live, pid) = (&ctx.state, &ctx.live, ctx.pid);
    let id = node.id;
    let source_ids = d.source_ids.borrow()[id as usize].clone();
    let mut sources: Vec<(i64, SourceRef)> = Vec::new();
    for source in source_ids {
        let prepared = prepare_source(d, ctx.gpu.as_ref(), request, state, live, pid, source)?;
        sources.insert(0, (source, prepared));
    }
    let source_stamp: Vec<(i64, i64)> = sources.iter().map(|(id, s)| (*id, s.borrow().revision)).collect();
    let key = match node.kind.as_str() {
        "image/load" => match args.get_str("path") {
            Some(Value::Text(path)) => ImageKey::Load(String::from_utf8_lossy(path).into_owned()),
            _ => return Err(error("Image path is text.")),
        },
        "image/noise" if node.args.iter().any(|(_, x)| e::is_live(x)) => ImageKey::NoiseLive(d.serial.get(), id),
        "image/noise" => ImageKey::Noise(format!("{args:?}")),
        "image/render" => ImageKey::Render(authored_key(node)),
        "image/map" => ImageKey::Map(authored_key(node)),
        _ => return Err(error("Unknown image producer.")),
    };
    let (stateful, _) = d.freshness.borrow()[id as usize];
    let previous = d.entries.borrow_mut().find_opt(&key);
    let state_stamp = stateful.then(|| e::state_stamp(state));
    let stamp = Rc::new(Stamp { serial: d.serial.get(), frame: live.clone(), state_stamp, sources: source_stamp.clone() });
    let valid = |s: &Stamp| current(d, &source_stamp, state, live, pid, node, s);
    if let Some(entry) = &previous {
        let ok = {
            let e = entry.borrow();
            if display {
                e.display.as_ref().is_some_and(|s| valid(s))
            } else {
                e.cpu.as_ref().is_some_and(|c| valid(&c.stamp))
            }
        };
        if ok {
            entry.borrow_mut().plan_serial = d.serial.get();
            d.resolved.borrow_mut()[id as usize] = Some(entry.clone());
            return Ok(entry.clone());
        }
    }
    let reserved = previous.is_none();
    if reserved {
        reserve(d, request)?;
        request.borrow_mut().pending_images += 1;
    }
    let made = compute_entry(t_, request, ctx, display, plan, node, args, &sources, &key, previous, &stamp, &valid);
    if reserved {
        request.borrow_mut().pending_images -= 1;
    }
    let entry = made?;
    entry.borrow_mut().plan_serial = d.serial.get();
    d.resolved.borrow_mut()[id as usize] = Some(entry.clone());
    Ok(entry)
}

fn compute_entry(
    t_: &Rc<Data>,
    request: &RequestRef,
    ctx: &Ctx,
    display: bool,
    plan: &e::Plan,
    node: &e::Node,
    args: &v::Record,
    sources: &[(i64, SourceRef)],
    key: &ImageKey,
    previous: Option<EntryRef>,
    stamp: &Rc<Stamp>,
    valid: &dyn Fn(&Stamp) -> bool,
) -> Result<EntryRef, Diagnostic> {
    let d = &**t_;
    let (state, live) = (&ctx.state, &ctx.live);
    let id = node.id;
    let entry = previous.unwrap_or_else(|| make_entry(key));
    if display {
        invalidate_display(&entry);
    }
    let upload = |width: i64, height: i64, payload: &sop::image::T| -> Result<image::Image, Diagnostic> {
        let existing = entry.borrow().image.clone();
        let bytes = bytes_of_image(payload)?;
        match existing {
            Some(image_) => message(image::upload_rgba(Some(&image_), width, height, &bytes)),
            None => workspace_resources::image(&d.resources, key, &|| image::upload_rgba(None, width, height, &bytes)),
        }
    };
    if node.kind == "image/map" {
        let width = int_arg(args, "width", 256)?;
        let height = int_arg(args, "height", 256)?;
        let Some(Value::Fn(function)) = args.get_str("function") else { return Err(error("Image map needs a pixel function.")) };
        let function = *function;
        let ids = attribute_kernel::sources(&Value::Fn(function));
        let mut inputs: Vec<node::T> = Vec::new();
        for sid in &ids {
            let Some((_, source)) = sources.iter().find(|(n, _)| n == sid) else { return Err(error("Image source is not bound to a compiled input.")) };
            match source.borrow().root.clone() {
                Some(root) => inputs.push(root),
                None => return Err(error("Image source is not bound to a compiled input.")),
            }
        }
        let prepare_map = || -> Result<image_kernel::T, Diagnostic> {
            let (path, approx) = match &*d.qualification.borrow() {
                Some((bound, approx, image_sites)) if *bound == ctx.pid => (image_sites.get(id as usize).cloned().flatten(), approx.clone()),
                _ => (None, Paths::new()),
            };
            let p = image_kernel::prepare(Some((node.inst, node.site.clone(), node.iter.clone())), path, Some(approx), node::build::fresh_id(), width, height, function, &ids, &inputs)?;
            d.maps.borrow_mut()[id as usize] = Some((width, height, function, p.clone()));
            Ok(p)
        };
        let known = d.maps.borrow()[id as usize].clone();
        let prepared = match known {
            Some((w, h, f, p)) if w == width && h == height && f == function => match image_kernel::with_inputs(&p, &inputs) {
                Ok(p) => {
                    d.maps.borrow_mut()[id as usize] = Some((width, height, function, p.clone()));
                    p
                }
                Err(_) => prepare_map()?,
            },
            _ => prepare_map()?,
        };
        let cook = || -> Result<sop::image::T, Diagnostic> {
            let cached = entry.borrow().cpu.clone();
            if let Some(cpu) = cached {
                if valid(&cpu.stamp) {
                    return Ok(cpu.payload);
                }
            }
            let source = image_kernel::node(Some(&e::fork_state(state)), &prepared);
            let context = cook_context(d, live)?;
            let cooked = cook_node(request, &context, &source)?;
            let payload = payload::image(&cooked.payload)?;
            entry.borrow_mut().cpu = Some(Snapshot { payload: payload.clone(), stamp: stamp.clone() });
            Ok(payload)
        };
        if !display {
            cook()?;
        } else {
            let resolver_data = t_.clone();
            let (request2, live2, sources2) = (request.clone(), live.clone(), sources.to_vec());
            let capture: Rc<dyn Fn(&Value) -> Result<Value, Diagnostic>> = Rc::new(move |value| resolve_capture(&resolver_data, &request2, &live2, &sources2, value));
            let program = image_kernel::program(&prepared);
            let selected = {
                let mut selected: Option<Result<Option<I::Displayed>, Diagnostic>> = None;
                let mut try_with = |gpu: &G::Config| {
                    selected = Some(run_with_capture(capture.clone(), &mut || {
                        let resolve = capture_fn();
                        I::try_display(Some(state), Some(&resolve), false, gpu, &program, live)
                    }));
                };
                match &ctx.gpu {
                    Some(gpu) => try_with(gpu),
                    None => workspace_gpu::with_backend(&d.gpu, false, &mut try_with),
                }
                selected.expect("selected")?
            };
            match selected {
                None => {
                    let payload = cook()?;
                    let image_ = upload(width, height, &payload)?;
                    let mut e = entry.borrow_mut();
                    e.image = Some(image_);
                    e.texture = Some(texture_slot(&payload));
                }
                Some(I::Displayed::Cpu(_)) => return Err(error("Image display unexpectedly requested a GPU readback.")),
                Some(I::Displayed::Gpu(value)) => {
                    workspace_gpu::image(&d.gpu, key, width, height, &mut |output| {
                        let output = output.clone();
                        let source: Rc<dyn Fn() -> Option<ogpu_core::backend::Texture>> = Rc::new(move || image_sink::texture(&output));
                        publish(d, key, &entry, width, height, source)
                    }, &value)?;
                }
            }
        }
    } else if node.kind == "image/render" {
        let width = int_arg(args, "width", live.size.0)?;
        let height = int_arg(args, "height", live.size.1)?;
        if width <= 0 || height <= 0 {
            return Err(error("Image dimensions exceed native storage bounds."));
        }
        sop::image::channel_count(width, height, 1)?;
        let drawing_ = args.get_str("drawing").expect("drawing").clone();
        let known = d.drawings.borrow()[id as usize].clone();
        let prepared = match known {
            Some(p) => p,
            None => {
                let p = Rc::new(drawing::prepare(&drawing::PrepareOptions::default(), plan, &drawing_)?);
                d.drawings.borrow_mut()[id as usize] = Some(p.clone());
                p
            }
        };
        let temporary: Rc<RefCell<Vec<image::Image>>> = Rc::new(RefCell::new(Vec::new()));
        let resolver_data = t_.clone();
        let (state2, live2, pid2, _gpu2, temporary2) = (state.clone(), live.clone(), ctx.pid, ctx.gpu.clone(), temporary.clone());
        let plan_rc: Rc<e::Plan> = d.plan.borrow().clone().expect("plan");
        let image_cb: drawing::image_resolver = Rc::new(move |child_value| {
            let ctx = Ctx { gpu: None, state: state2.clone(), live: live2.clone(), pid: pid2 };
            let child = resolve(&resolver_data, &ctx, None, display, &plan_rc, child_value)?;
            let dd = &*resolver_data;
            if display {
                return Ok(child.borrow().image.clone().expect("child image"));
            }
            let (payload, cpu_stamp, display_stamp, child_image, child_key) = {
                let c = child.borrow();
                let cpu = c.cpu.clone().expect("cpu");
                (cpu.payload, cpu.stamp, c.display.clone(), c.image.clone(), c.key.clone())
            };
            let (width, height) = (sop::image::width(&payload), sop::image::height(&payload));
            if display_stamp.as_ref().is_some_and(|s| Rc::ptr_eq(s, &cpu_stamp)) {
                return Ok(child_image.expect("child image"));
            }
            let bytes = bytes_of_image(&payload)?;
            match child_image {
                Some(image_) if matches!(runtime_resources::image::gpu_snapshot(&image::resource(&image_)), Ok(None)) => {
                    child.borrow_mut().display = None;
                    message(image::upload_rgba(Some(&image_), width, height, &bytes))
                }
                None => {
                    let image_ = workspace_resources::image(&dd.resources, &child_key, &|| image::upload_rgba(None, width, height, &bytes))?;
                    child.borrow_mut().image = Some(image_.clone());
                    Ok(image_)
                }
                Some(_) => {
                    let image_ = message(image::upload_rgba(None, width, height, &bytes))?;
                    temporary2.borrow_mut().push(image_.clone());
                    Ok(image_)
                }
            }
        });
        let result = (|| -> Result<(), Diagnostic> {
            let scene = drawing::render_prepared(&prepared, live, (width, height), &drawing::RenderOptions { state: Some(state.clone()), image: Some(image_cb), ..Default::default() })?;
            if display {
                let previous = entry.borrow().canvas.clone();
                let (canvas_, created) = match &previous {
                    Some(c) => {
                        let size = message(canvas::size(c))?;
                        if size == (width, height) { (c.clone(), false) } else { (create_canvas(d, width, height)?, true) }
                    }
                    None => (create_canvas(d, width, height)?, true),
                };
                let attempt = (|| -> Result<(), Diagnostic> {
                    message(canvas::render(None, &canvas_, &scene))?;
                    let (w, h, source) = message(canvas::gpu_source(&canvas_))?;
                    publish(d, key, &entry, w, h, Rc::new(source))?;
                    entry.borrow_mut().canvas = Some(canvas_.clone());
                    Ok(())
                })();
                match attempt {
                    Ok(()) => {
                        if created {
                            if let Some(p) = &previous {
                                destroy_canvas(d, p);
                            }
                        }
                        Ok(())
                    }
                    Err(err) => {
                        if created {
                            destroy_canvas(d, &canvas_);
                        }
                        Err(err)
                    }
                }
            } else {
                let canvas_ = create_canvas(d, width, height)?;
                let attempt = (|| -> Result<(), Diagnostic> {
                    message(canvas::render(None, &canvas_, &scene))?;
                    let image_ = message(canvas::to_image(&canvas_))?;
                    let cpu = cpu_of_image(&image_);
                    let _ = image::destroy(&image_);
                    entry.borrow_mut().cpu = Some(Snapshot { payload: cpu?, stamp: stamp.clone() });
                    Ok(())
                })();
                destroy_canvas(d, &canvas_);
                attempt
            }
        })();
        for image_ in temporary.borrow().iter() {
            let _ = image::destroy(image_);
        }
        result?;
    } else {
        entry.borrow_mut().display = None;
        let (image_, payload) = match node.kind.as_str() {
            "image/load" => {
                let path = match args.get_str("path") {
                    Some(Value::Text(path)) => String::from_utf8_lossy(path).into_owned(),
                    _ => unreachable!("assert false"),
                };
                let image_ = workspace_resources::image(&d.resources, key, &|| image::load(&path))?;
                let payload = cpu_of_image(&image_)?;
                (image_, payload)
            }
            "image/noise" => {
                let factory = sop::image_nodes::noise_factory();
                let source = edit_graph::instantiate(&factory, &[]).map_err(|m| error(&m))?;
                let mut changes: Vec<(String, param::Value)> = Vec::new();
                for field in edit_graph::factory_fields(&factory) {
                    if let Some(x) = args.get_str(&field.name) {
                        changes.push((
                            field.name.clone(),
                            match field.kind {
                                param::KindView::Integer(_) => param::Value::Int(v::int_of(x)?),
                                param::KindView::Floating(_) => param::Value::Float(v::num(x)?),
                                _ => unreachable!("assert false"),
                            },
                        ));
                    }
                }
                let (source, _) = node::apply_parameters(&source, &changes).map_err(|m| error(&m))?;
                let context = context::create(context::Options::default()).map_err(|m| error(&m))?;
                let cooked = node::build::cook(&source, &context, &[]).map_err(|d| error(&d.message))?;
                let payload = payload::image(&cooked.payload).map_err(|d| error(&d.message))?;
                let (width, height) = (sop::image::width(&payload), sop::image::height(&payload));
                let image_ = upload(width, height, &payload)?;
                (image_, payload)
            }
            _ => unreachable!("assert false"),
        };
        let mut e = entry.borrow_mut();
        e.cpu = Some(Snapshot { payload: payload.clone(), stamp: stamp.clone() });
        e.image = Some(image_);
        e.texture = Some(texture_slot(&payload));
    }
    if display {
        entry.borrow_mut().display = Some(stamp.clone());
    }
    let bytes = entry_bytes(&entry.borrow());
    entries_add(d, key, &entry, bytes);
    Ok(entry)
}

/// The image of `value`, resolved for display (a GPU-resident image) or, with `display` false, with
/// a host copy uploaded.
pub fn image(gpu: Option<&G::Config>, display: bool, t_: &WorkspaceImages, state: &e::State, live: &Frame, plan: &e::Plan, value: &Value) -> Result<image::Image, Diagnostic> {
    let d = &t_.0;
    let ctx = Ctx { gpu: gpu.cloned(), state: state.clone(), live: live.clone(), pid: plan_id(plan) };
    let entry = resolve(d, &ctx, None, display, plan, value)?;
    if display {
        return Ok(entry.borrow().image.clone().expect("image"));
    }
    let (cpu, shown, key, existing) = {
        let e = entry.borrow();
        (e.cpu.clone().expect("cpu"), e.display.clone(), e.key.clone(), e.image.clone())
    };
    if shown.as_ref().is_some_and(|s| Rc::ptr_eq(s, &cpu.stamp)) {
        return Ok(existing.expect("image"));
    }
    let (width, height) = (sop::image::width(&cpu.payload), sop::image::height(&cpu.payload));
    let bytes = bytes_of_image(&cpu.payload)?;
    let image_ = match existing {
        Some(image_) => message(image::upload_rgba(Some(&image_), width, height, &bytes))?,
        None => workspace_resources::image(&d.resources, &key, &|| image::upload_rgba(None, width, height, &bytes))?,
    };
    let mut e = entry.borrow_mut();
    e.image = Some(image_.clone());
    e.texture = Some(texture_slot(&cpu.payload));
    e.display = Some(cpu.stamp);
    Ok(image_)
}

/// The resolver `Lower` installs for a cook: the CPU payload of an image node, resolved here.
pub fn payload(t_: &WorkspaceImages) -> l::ImageResolver {
    let owner_id = t_.0.id;
    Arc::new(move |gpu, context, plan, state, live, value| {
        let Some(d) = owner_of(owner_id) else { return Err(error("Image resources must resolve on the initial domain.")) };
        let ctx = Ctx { gpu: gpu.cloned(), state: state.clone(), live: live.clone(), pid: plan_id(plan) };
        let entry = resolve(&d, &ctx, context, false, plan, value)?;
        let cpu = entry.borrow().cpu.clone();
        Ok(cpu.expect("cpu").payload)
    })
}

pub fn texture(gpu: Option<&G::Config>, t_: &WorkspaceImages, state: &e::State, live: &Frame, plan: &e::Plan, value: &Value) -> Result<texture::Texture, Diagnostic> {
    let ctx = Ctx { gpu: gpu.cloned(), state: state.clone(), live: live.clone(), pid: plan_id(plan) };
    let entry = resolve(&t_.0, &ctx, None, true, plan, value)?;
    let slot = entry.borrow().texture.clone().expect("texture");
    message(force_texture(&slot))
}

pub fn close(t_: &WorkspaceImages) {
    let d = &*t_.0;
    let entries = std::mem::take(&mut d.entries.borrow_mut().slots);
    for (_, entry, _, _) in entries {
        release_entry(d, &entry);
    }
    *d.plan.borrow_mut() = None;
    d.plan_id.set(None);
    *d.qualification.borrow_mut() = None;
    d.arguments.borrow_mut().clear();
    d.drawings.borrow_mut().clear();
    *d.lowered.borrow_mut() = None;
    d.source_cache.borrow_mut().clear();
    d.capture_cache.borrow_mut().items.clear();
    d.capture_cache.borrow_mut().bytes = 0;
    d.source_ids.borrow_mut().clear();
    d.freshness.borrow_mut().clear();
    d.maps.borrow_mut().clear();
    d.sizes.borrow_mut().clear();
    d.resolved.borrow_mut().clear();
    OWNERS.with(|o| o.borrow_mut().remove(&d.id));
}
