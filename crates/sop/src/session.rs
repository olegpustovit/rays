//! explicit, bounded graph-evaluation state. A session is
//! single-caller. Cache keys are opaque byte strings (`Vec<u8>`).
//!
//! Independent branches fan out to the shared pool: workers cook on a `snapshot` of the cache
//! metadata and journal their cache traffic; the parent validates and replays the journals in input
//! order (`evaluate_many`), so CLOCK order, retention and counters equal input-order execution.
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};

use lru::{Cache, CacheOptions, Key, Shed};
use mem::Owner;
use rays_diagnostic::Diagnostic;
use rays_math::mat4;

use crate::context::{self, private as consumers_of};
use crate::node::{self, build, Topology};
use crate::payload;

#[derive(Clone)]
pub struct NodeTiming {
    pub node_id: i64,
    pub label: String,
    pub operation: String,
    pub input_points: i64,
    pub points: i64,
    pub seconds: f64,
    pub cache_hit: bool,
}

#[derive(Clone)]
pub struct Stats {
    pub cooks: i64,
    pub hits: i64,
    pub misses: i64,
    pub evictions: i64,
    /// subset of `hits`
    pub volatile_hits: i64,
    /// subset of `misses`
    pub volatile_misses: i64,
    pub retained_entries: i64,
    /// one slot per volatile node, outside the LRU
    pub volatile_entries: i64,
    pub retained_payload_bytes: i64,
    pub last_node: Option<NodeTiming>,
    pub consumer_scans: i64,
}

/// A packed result (e.g. Copy to Points with Pack and instance): draw the `Geometry` payload once
/// per transform. A node that consumes it receives it materialized.
#[derive(Clone)]
pub struct Output {
    pub payload: payload::T,
    pub instances: Option<Arc<Vec<mat4::T>>>,
}

/// Output identity requires the same payload and instance array.
pub fn same_output(a: &Output, b: &Output) -> bool {
    payload::ptr_eq(&a.payload, &b.payload)
        && match (&a.instances, &b.instances) {
            (None, None) => true,
            (Some(x), Some(y)) => Arc::ptr_eq(x, y),
            _ => false,
        }
}

#[derive(Clone)]
struct Delta {
    source: i64,
    positions: Option<rdk_core::packed::float3::T>,
    attributes: Vec<(rdk_core::attribute::Owner, String, Option<rdk_core::attribute::T>)>,
}

#[derive(Clone)]
struct Entry {
    output: Output,
    components: Vec<(i64, i64)>,
    delta: Option<Delta>,
}

type Release = Arc<dyn Fn(bool) + Send + Sync>;

/// Cook outputs share packed payload planes; the pool counts each plane once. A plane a device
/// still holds (resident) has a release, by id: once no entry retains it, it is dropped, and at the
/// end of a cook released unless the cook's root output holds it.
struct Pool {
    refs: HashMap<i64, (i64, i64)>,
    bytes: i64,
    owner: Owner,
    /// what the ledger holds per component and how many entries and slots hold it
    charged: HashMap<i64, (Owner, i64, i64)>,
    resident: HashMap<i64, Release>,
    dropped: Vec<i64>,
    /// resident ids a cook's root held: a caller may read them, so they are only retired
    handed: HashSet<i64>,
    /// false in a worker snapshot: the parent charges when it replays
    ledgered: bool,
}

type SharedPool = Arc<Mutex<Pool>>;

fn drop_id(pool: &mut Pool, id: i64) {
    if pool.resident.contains_key(&id) {
        pool.dropped.push(id);
    }
}

fn releasers(output: &Output) -> Vec<(i64, Release)> {
    match &output.payload {
        payload::T::Geometry(g) => rdk_core::geometry::releasers(g),
        _ => Vec::new(),
    }
}

fn charge_component(pool: &mut Pool, owner: &Owner, id: i64, bytes: i64) {
    if !pool.ledgered {
        return;
    }
    match pool.charged.get_mut(&id) {
        Some((_, _, holders)) => *holders += 1,
        None => {
            let size = if pool.resident.contains_key(&id) { 0 } else { bytes };
            pool.charged.insert(id, (owner.clone(), size, 1));
            mem::charge(owner, size);
        }
    }
}

fn discharge_component(pool: &mut Pool, id: i64) {
    match pool.charged.get_mut(&id) {
        None => {}
        Some((_, _, holders)) if *holders > 1 => *holders -= 1,
        Some(_) => {
            let (owner, size, _) = pool.charged.remove(&id).unwrap();
            mem::release(&owner, size);
        }
    }
}

fn discharge_all(pool: &mut Pool) {
    for (_, (owner, size, _)) in pool.charged.drain() {
        mem::release(&owner, size);
    }
}

fn release_components(pool: &mut Pool, components: &[(i64, i64)]) {
    for (id, bytes) in components {
        match pool.refs.get(id).copied() {
            None => {}
            Some((_, 1)) => {
                pool.refs.remove(id);
                drop_id(pool, *id);
                pool.bytes -= bytes;
                discharge_component(pool, *id);
            }
            Some((_, references)) => {
                pool.refs.insert(*id, (*bytes, references - 1));
            }
        }
    }
}

/// Who still reads each node of a cook's DAG. The census of `consumer_context` freezes it per root:
/// a node's serial, its distinct inputs by serial and its distinct consumers; each cook's memo counts
/// the consumers down. A DAG with an expansion has an empty census: nothing is dropped.
struct Census {
    nodes: Vec<node::T>,
    serials: HashMap<i64, Vec<(node::T, usize)>>,
    distinct_inputs: Vec<Vec<usize>>,
    consumers: Vec<i64>,
}

fn serial_of(census: &Census, n: &node::T) -> Option<usize> {
    census.serials.get(&node::id(n))?.iter().find(|(seen, _)| node::ptr_eq(seen, n)).map(|(_, s)| *s)
}

#[derive(Clone)]
struct ConsumerPlan {
    root: node::T,
    readers: Option<i64>,
    previous: context::Consumers,
    planned: context::Consumers,
    census: Arc<Census>,
}

enum Written {
    Key(Vec<u8>),
    Slot(i64),
}

/// One cache interaction of a worker branch, replayed on the parent in input order.
enum CacheEvent {
    Read(Option<i64>, Vec<u8>, Option<Arc<Entry>>),
    Write(Option<i64>, Vec<u8>, Entry),
    MaterializedRead(Output, Option<rdk_core::geometry::T>),
    MaterializedWrite(Output, rdk_core::geometry::T),
    Time(NodeTiming),
}

/// The node last cooked or hit; its names are copied only when `stats` is read.
#[derive(Clone)]
struct LastNode {
    node: node::T,
    input_points: i64,
    points: i64,
    seconds: f64,
    cache_hit: bool,
}

pub struct T {
    /// the per-cook memo's storage, kept between cooks: a cache hit builds no table
    /// the cache key's buffer, kept after a hit
    key_pool: Vec<u8>,
    memo_pool: (HashMap<i64, Vec<(node::T, Result<Output, Diagnostic>)>>, Vec<i64>),
    clock: Arc<dyn Fn() -> f64 + Send + Sync>,
    max_entries: i64,
    max_payload_bytes: i64,
    cache: Cache,
    payload: SharedPool,
    volatile_owner: Owner,
    cooks: i64,
    hits: i64,
    misses: i64,
    volatile: Arc<dyn Fn(i64) -> bool + Send + Sync>,
    /// volatile node id -> latest
    slots: HashMap<i64, (Vec<u8>, Arc<Entry>)>,
    /// Resident ids registered when a volatile slot was written. Geometry releasers become empty
    /// after a deferred attribute is materialised, so replacement drops these ids.
    slot_residents: HashMap<i64, Vec<i64>>,
    /// volatile node id -> the components it charged
    slot_bytes: HashMap<i64, Vec<i64>>,
    volatile_hits: i64,
    volatile_misses: i64,
    evictions: i64,
    last_node: Option<LastNode>,
    times: Arc<Mutex<HashMap<i64, NodeTiming>>>,
    closed: bool,
    materialized: Vec<(Output, rdk_core::geometry::T)>,
    /// Some in a worker snapshot while it records its cache traffic
    journal: Option<Vec<CacheEvent>>,
    parallel_override: Option<bool>,
    fanouts: i64,
    /// the resident components of the root the last cook returned
    previous_root: Vec<i64>,
    /// cache keys and volatile slots this cook attempt wrote, dropped if it settles a failure
    written: Vec<Written>,
    consumer_plan: Option<ConsumerPlan>,
    consumer_scans: i64,
}

fn key_hash(bytes: &[u8]) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    bytes.hash(&mut hasher);
    hasher.finish()
}

fn key_of(bytes: &[u8]) -> Key {
    Key::new(key_hash(bytes), Arc::new(bytes.to_vec()))
}

fn entry_of(value: &lru::Value) -> Arc<Entry> {
    value.clone().downcast::<Entry>().unwrap_or_else(|_| unreachable!("session cache holds only entries"))
}

pub fn create(max_entries: i64, max_payload_bytes: i64) -> Result<T, String> {
    if max_entries < 0 {
        return Err("Session.create: max_entries must be non-negative".to_owned());
    }
    if max_payload_bytes < 0 {
        return Err("Session.create: max_payload_bytes must be non-negative".to_owned());
    }
    let owner = lru::cache("sop.session.payload");
    let pool: SharedPool = Arc::new(Mutex::new(Pool {
        refs: HashMap::new(),
        bytes: 0,
        owner: owner,
        charged: HashMap::new(),
        resident: HashMap::new(),
        dropped: Vec::new(),
        handed: HashSet::new(),
        ledgered: true,
    }));
    let release_pool = Arc::clone(&pool);
    let cache = Cache::with_options(
        lru::cache("sop.session"),
        max_entries,
        Arc::new(|key, other| key.as_any().downcast_ref::<Vec<u8>>() == other.downcast_ref::<Vec<u8>>()),
        // Never: only the cook's thread touches it; Async_cook sheds it while that one is idle
        CacheOptions {
            shed: Shed::Never,
            release: Some(Arc::new(move |_, value| {
                let entry = entry_of(value);
                release_components(&mut release_pool.lock().unwrap(), &entry.components);
            })),
            ..CacheOptions::default()
        },
    );
    Ok(T {
        clock: Arc::new(|| {
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs_f64())
                .unwrap_or(0.0)
        }),
        max_entries,
        max_payload_bytes,
        cache,
        payload: pool,
        volatile_owner: lru::cache("sop.volatile"),
        cooks: 0,
        hits: 0,
        misses: 0,
        memo_pool: Default::default(),
        key_pool: Vec::new(),
        volatile: Arc::new(|_| false),
        slots: HashMap::new(),
        slot_residents: HashMap::new(),
        slot_bytes: HashMap::new(),
        volatile_hits: 0,
        volatile_misses: 0,
        evictions: 0,
        last_node: None,
        times: Arc::new(Mutex::new(HashMap::new())),
        closed: false,
        materialized: Vec::new(),
        journal: None,
        parallel_override: None,
        fanouts: 0,
        previous_root: Vec::new(),
        written: Vec::new(),
        consumer_plan: None,
        consumer_scans: 0,
    })
}

/// Replace the wall clock behind node timings (default: the system clock).
pub fn set_clock(session: &mut T, clock: Arc<dyn Fn() -> f64 + Send + Sync>) {
    session.clock = clock;
}

pub fn now(session: &T) -> f64 {
    (session.clock)()
}

/// One cached entry gone, on the thread that owns the session or while that one is known idle.
pub fn shed(session: &mut T) -> bool {
    let dropped = session.cache.drop_oldest();
    if dropped {
        session.evictions += 1;
    }
    dropped
}

pub fn payload_owner(session: &T) -> Owner {
    session.payload.lock().unwrap().owner.clone()
}

pub fn payload_bytes(session: &T) -> i64 {
    session.payload.lock().unwrap().bytes
}

/// A handle on the payload byte count readable without the session (for a memory evictor).
pub fn payload_pool(session: &T) -> Arc<dyn Fn() -> i64 + Send + Sync> {
    let pool = Arc::clone(&session.payload);
    Arc::new(move || pool.lock().unwrap().bytes)
}

fn insert(session: &mut T, key: &[u8], entry: Entry) {
    if let Some(events) = &mut session.journal {
        events.push(CacheEvent::Write(None, key.to_vec(), entry.clone()));
    }
    if session.max_entries <= 0 {
        return;
    }
    let components = payload::payload_components(&entry.output.payload);
    let owner;
    {
        let mut pool = session.payload.lock().unwrap();
        owner = pool.owner.clone();
        session.written.push(Written::Key(key.to_vec()));
        for (id, release) in releasers(&entry.output) {
            pool.resident.insert(id, release);
        }
        for (id, bytes) in &components {
            match pool.refs.get(id).copied() {
                None => {
                    pool.refs.insert(*id, (*bytes, 1));
                    pool.bytes += bytes;
                    charge_component(&mut pool, &owner, *id, *bytes);
                }
                Some((known_bytes, references)) => {
                    pool.refs.insert(*id, (known_bytes, references + 1));
                }
            }
        }
    }
    let before = session.cache.length();
    let replaced = session.cache.peek(&key_of(key)).is_some();
    // Payload planes are charged on the pool owner; the entry itself is a small handle.
    session.cache.add(key_of(key), Arc::new(Entry { components, ..entry }), 0);
    while session.payload.lock().unwrap().bytes > session.max_payload_bytes && session.cache.drop_oldest() {}
    session.evictions += before + i64::from(!replaced) - session.cache.length();
}

fn lookup(session: &mut T, volatile: bool, node_value: &node::T, key: &Vec<u8>) -> Option<Arc<Entry>> {
    let result = if !volatile {
        // the stored keys are `Vec<u8>`: probe with the one in hand
        session.cache.find_opt_by(key_hash(key), key).map(|v| entry_of(&v))
    } else {
        match session.slots.get(&node::id(node_value)) {
            Some((latest, output)) if latest.as_slice() == key => Some(Arc::clone(output)),
            _ => None,
        }
    };
    if let Some(events) = &mut session.journal {
        events.push(CacheEvent::Read(volatile.then(|| node::id(node_value)), key.to_vec(), result.clone()));
    }
    result
}

fn release_slot_bytes(session: &mut T, id: i64) {
    if let Some(components) = session.slot_bytes.remove(&id) {
        let mut pool = session.payload.lock().unwrap();
        for c in components {
            discharge_component(&mut pool, c);
        }
    }
}

fn store_slot(session: &mut T, id: i64, key: &[u8], entry: Entry) {
    if let Some(events) = &mut session.journal {
        events.push(CacheEvent::Write(Some(id), key.to_vec(), entry.clone()));
    }
    if session.max_entries <= 0 {
        return;
    }
    let fresh = releasers(&entry.output);
    let fresh_ids: Vec<i64> = fresh.iter().map(|(i, _)| *i).collect();
    // bounded: one slot per node id, dropped wholesale past max_entries
    let mut gone: Vec<(i64, Option<Vec<i64>>, Vec<i64>)> = Vec::new();
    let collect = |slot_id: i64, old: &Entry, residents: Option<&Vec<i64>>| -> (i64, Option<Vec<i64>>, Vec<i64>) {
        (slot_id, residents.cloned(), releasers(&old.output).iter().map(|(i, _)| *i).collect())
    };
    if session.slots.len() as i64 >= session.max_entries {
        for (slot_id, (_, old)) in &session.slots {
            gone.push(collect(*slot_id, old, session.slot_residents.get(slot_id)));
        }
        session.slots.clear();
        // slot_residents/slot_bytes are reset after their bytes are released below
    } else if let Some((_, old)) = session.slots.get(&id) {
        gone.push(collect(id, old, session.slot_residents.get(&id)));
    }
    for (slot_id, residents, current) in &gone {
        release_slot_bytes(session, *slot_id);
        let ids = residents.clone().unwrap_or_else(|| current.clone());
        let mut pool = session.payload.lock().unwrap();
        for i in ids {
            if !pool.refs.contains_key(&i) && !fresh_ids.contains(&i) {
                drop_id(&mut pool, i);
            }
        }
    }
    if session.slots.is_empty() {
        session.slot_residents.clear();
        session.slot_bytes.clear();
    }
    let components = payload::payload_components(&entry.output.payload);
    {
        let mut pool = session.payload.lock().unwrap();
        for (i, r) in fresh {
            pool.resident.insert(i, r);
        }
        let owner = session.volatile_owner.clone();
        for (c, bytes) in &components {
            charge_component(&mut pool, &owner, *c, *bytes);
        }
    }
    session.written.push(Written::Slot(id));
    session.slot_residents.insert(id, fresh_ids);
    session.slot_bytes.insert(id, components.iter().map(|(c, _)| *c).collect());
    session.slots.insert(id, (key.to_vec(), Arc::new(Entry { components, ..entry })));
}

/// Marks nodes whose cache key changes every cook (time-driven nodes and what is downstream of
/// them). A volatile node keeps exactly one entry, its latest, replaced in place; it is never
/// counted in or evicted from the LRU. Replaces the previous predicate and drops the slots of nodes
/// that stopped being volatile.
pub fn set_volatile(session: &mut T, predicate: Arc<dyn Fn(i64) -> bool + Send + Sync>) {
    let stale: Vec<i64> = session.slots.keys().copied().filter(|id| !predicate(*id)).collect();
    session.volatile = predicate;
    for id in stale {
        release_slot_bytes(session, id);
        session.slots.remove(&id);
    }
    let keep = Arc::clone(&session.volatile);
    session.slot_residents.retain(|id, _| keep(*id));
    session.slot_bytes.retain(|id, _| keep(*id));
}

/// An opaque, unambiguous cache identity: integers are fixed-width binary and every variable-length
/// string is length-prefixed, so no field boundary can shift.
fn selective(n: &node::T, inputs: &[payload::T]) -> bool {
    let facts = node::facts(n);
    inputs.len() == 1
        && matches!(inputs[0], payload::T::Geometry(_))
        && !build::has_expand(n)
        && facts.topology == Topology::Preserved
        && !(facts.reads.iter().any(|r| r == "*") || facts.writes.iter().any(|w| w == "*"))
        && (facts.cook_mode == node::CookMode::DuplicateInput(0) || facts.cook_mode == node::CookMode::Passthrough(0))
}

fn cache_key(mut buffer: Vec<u8>, n: &node::T, context: &context::T, inputs: &[payload::T]) -> Vec<u8> {
    let parameters = build::cache_parameters(n);
    let parameter_key = node::parameter_key(n);
    let facts_key = build::cache_facts(n);
    buffer.clear();
    buffer.reserve(64 + parameters.len() + parameter_key.len() + facts_key.len() + 8 * inputs.len());
    fn add_int(buffer: &mut Vec<u8>, v: i64) {
        buffer.extend_from_slice(&v.to_le_bytes());
    }
    fn add_sized(buffer: &mut Vec<u8>, v: &[u8]) {
        add_int(buffer, v.len() as i64);
        buffer.extend_from_slice(v);
    }
    add_int(&mut buffer, node::id(n));
    add_int(&mut buffer, node::version(n));
    add_sized(&mut buffer, node::operation(n).as_bytes());
    add_sized(&mut buffer, parameters);
    add_sized(&mut buffer, parameter_key);
    add_int(&mut buffer, inputs.len() as i64);
    let facts = node::facts(n);
    add_sized(&mut buffer, facts_key);
    if !selective(n, inputs) {
        add_int(&mut buffer, 0);
        for p in inputs {
            add_int(&mut buffer, payload::data_id(p));
        }
    } else {
        add_int(&mut buffer, 1);
        let geometry = payload::geometry(&inputs[0]).expect("selective input is geometry");
        add_int(&mut buffer, rdk_core::topology::data_id(rdk_core::geometry::topology(&geometry)));
        add_int(
            &mut buffer,
            if facts.reads.iter().any(|r| r == "P") {
                rdk_core::packed::float3::data_id(rdk_core::geometry::positions(&geometry))
            } else {
                0
            },
        );
        let attributes: Vec<rdk_core::attribute::T> = rdk_core::geometry::attributes(&geometry)
            .into_iter()
            .filter(|a| facts.reads.iter().any(|r| r == rdk_core::attribute::name(a)))
            .collect();
        add_int(&mut buffer, attributes.len() as i64);
        for a in &attributes {
            add_int(&mut buffer, rdk_core::attribute::data_id(a));
        }
        // Presence and order affect set-initial, normal removal and output order. Unread payload
        // IDs remain absent from the key.
        let layout = rdk_core::geometry::attributes(&geometry);
        add_int(&mut buffer, layout.len() as i64);
        for a in &layout {
            add_int(&mut buffer, rdk_core::attribute::owner(a) as i64);
            add_sized(&mut buffer, rdk_core::attribute::name(a).as_bytes());
        }
        let groups = rdk_core::geometry::groups(&geometry);
        let edges = rdk_core::geometry::edge_groups(&geometry);
        add_int(&mut buffer, groups.len() as i64);
        for g in &groups {
            add_int(&mut buffer, rdk_core::group::data_id(g));
        }
        add_int(&mut buffer, edges.len() as i64);
        for g in &edges {
            add_int(&mut buffer, rdk_core::edge_group::data_id(g));
        }
    }
    buffer.extend_from_slice(&context::cache_projection(node::dependencies(n), context));
    buffer
}

fn settle_failed() -> Diagnostic {
    crate::support::diagnostic("E_DEVICE_SETTLE", "device work failed its status check; recooking on the CPU")
}

fn facts_error(message: &str) -> Diagnostic {
    crate::support::diagnostic("E_NODE_FACTS", message)
}

/// Node cook panics become that node's stable error, allowing the session and worker to cook again.
fn node_result<X>(f: impl FnOnce() -> Result<X, Diagnostic>) -> Result<X, Diagnostic> {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(payload) => {
            let cause = payload.downcast_ref::<String>().cloned().or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_owned())).unwrap_or_else(|| "panic".to_owned());
            Err(crate::support::diagnostic_with("uncaught_node_exception", "a procedural node raised an exception while cooking", Some(cause), Vec::new()))
        }
    }
}

fn delta(n: &node::T, inputs: &[payload::T], cooked: &node::Cooked) -> Result<Option<Delta>, Diagnostic> {
    use rdk_core::{attribute as A, geometry as G};
    if !selective(n, inputs) {
        return Ok(None);
    }
    let source = payload::geometry(&inputs[0]).expect("selective input is geometry");
    let geometry = match &cooked.payload {
        payload::T::Geometry(g) => g.clone(),
        _ => return Err(facts_error("component-cached geometry nodes must return geometry")),
    };
    if cooked.instances.is_some() {
        return Err(facts_error("component-cached nodes must return unpacked geometry"));
    }
    if rdk_core::topology::data_id(G::topology(&source)) != rdk_core::topology::data_id(G::topology(&geometry)) {
        return Err(facts_error("Preserved topology was replaced"));
    }
    let ids = |g: &G::T| -> (Vec<i64>, Vec<i64>) {
        (
            G::groups(g).iter().map(rdk_core::group::data_id).collect(),
            G::edge_groups(g).iter().map(rdk_core::edge_group::data_id).collect(),
        )
    };
    if ids(&source) != ids(&geometry) {
        return Err(facts_error("component-cached nodes must preserve groups and edge groups"));
    }
    let writes = &node::facts(n).writes;
    let declared = |name: &str| writes.iter().any(|w| w == name);
    let before: HashMap<(u8, String), A::T> =
        G::attributes(&source).into_iter().map(|a| ((A::owner(&a) as u8, A::name(&a).to_owned()), a)).collect();
    let after_attrs = G::attributes(&geometry);
    let after: HashSet<(u8, String)> =
        after_attrs.iter().map(|a| (A::owner(a) as u8, A::name(a).to_owned())).collect();
    let mut attributes = Vec::new();
    for a in &after_attrs {
        let key = (A::owner(a) as u8, A::name(a).to_owned());
        let changed = match before.get(&key) {
            Some(previous) => A::data_id(previous) != A::data_id(a),
            None => true,
        };
        if changed {
            if !declared(&key.1) {
                return Err(facts_error(&format!("undeclared write {}", key.1)));
            }
            attributes.push((A::owner(a), key.1, Some(a.clone())));
        }
    }
    for a in G::attributes(&source) {
        let key = (A::owner(&a) as u8, A::name(&a).to_owned());
        if !after.contains(&key) {
            if !declared(&key.1) {
                return Err(facts_error(&format!("undeclared deletion {}", key.1)));
            }
            attributes.push((A::owner(&a), key.1, None));
        }
    }
    let positions = if rdk_core::packed::float3::data_id(G::positions(&source)) == rdk_core::packed::float3::data_id(G::positions(&geometry)) {
        None
    } else {
        if !declared("P") {
            return Err(facts_error("undeclared P write"));
        }
        Some(G::positions(&geometry).clone())
    };
    Ok(Some(Delta { source: G::data_id(&source), positions, attributes }))
}

/// `Some(new entry)` when the cached components must be re-based on the current input.
fn refresh(inputs: &[payload::T], entry: &Entry) -> Result<Option<Entry>, Diagnostic> {
    use rdk_core::{attribute as A, geometry as G};
    let Some(delta) = &entry.delta else { return Ok(None) };
    if delta.source == payload::data_id(&inputs[0]) {
        return Ok(None);
    }
    let source = payload::geometry(&inputs[0]).expect("delta input is geometry");
    let mut values: HashMap<(u8, String), A::T> =
        G::attributes(&source).into_iter().map(|a| ((A::owner(&a) as u8, A::name(&a).to_owned()), a)).collect();
    for (owner, name, value) in &delta.attributes {
        match value {
            Some(a) => {
                values.insert((*owner as u8, name.clone()), a.clone());
            }
            None => {
                values.remove(&(*owner as u8, name.clone()));
            }
        }
    }
    let cached = payload::geometry(&entry.output.payload).expect("cached geometry");
    let attributes: Vec<A::T> = G::attributes(&cached)
        .iter()
        .map(|a| values.get(&(A::owner(a) as u8, A::name(a).to_owned())).cloned().ok_or_else(|| facts_error("cannot refresh cached components: missing attribute")))
        .collect::<Result<_, _>>()?;
    let geometry = G::create(
        delta.positions.clone().unwrap_or_else(|| G::positions(&source).clone()),
        G::topology(&source).clone(),
        Some(attributes),
        Some(G::groups(&source)),
        Some(G::edge_groups(&source)),
    )
    .map_err(|e| facts_error(&format!("cannot refresh cached components: {}", rdk_core::error::to_string(&e))))?;
    Ok(Some(Entry {
        output: Output { payload: payload::T::Geometry(geometry), instances: entry.output.instances.clone() },
        components: entry.components.clone(),
        delta: Some(Delta { source: G::data_id(&source), ..delta.clone() }),
    }))
}

fn timing(n: &node::T, geometries: &[payload::T], points: i64, seconds: f64, cache_hit: bool) -> NodeTiming {
    NodeTiming {
        node_id: node::id(n),
        label: node::label(n).to_owned(),
        operation: node::operation(n).to_owned(),
        input_points: geometries.iter().map(payload::element_count).sum(),
        points,
        seconds,
        cache_hit,
    }
}

/// a cache hit keeps the node's last real cook time; bounded: a full table starts over
const TIMES_CAPACITY: usize = 4096;

fn record_time(session: &mut T, sample: NodeTiming) {
    if let Some(events) = &mut session.journal {
        events.push(CacheEvent::Time(sample.clone()));
    }
    let mut times = session.times.lock().unwrap();
    if times.len() >= TIMES_CAPACITY {
        times.clear();
    }
    times.insert(sample.node_id, sample);
}

/// The seconds node `id` took the last time it was really cooked (its own work, not its inputs';
/// a cache hit leaves it). At most 4,096 nodes are kept.
pub fn node_seconds(session: &T, id: i64) -> Option<f64> {
    session.times.lock().unwrap().get(&id).map(|s| s.seconds)
}

/// The shared timing table, readable from another thread.
pub fn times(session: &T) -> Arc<Mutex<HashMap<i64, NodeTiming>>> {
    Arc::clone(&session.times)
}

fn cancellation_error(n: &node::T) -> Diagnostic {
    rays_diagnostic::prepend_trace(node::trace(n), crate::support::diagnostic("cancelled", "procedural cook was cancelled"))
}

fn traced(n: &node::T, error: Diagnostic) -> Diagnostic {
    rays_diagnostic::prepend_trace(node::trace(n), error)
}

/// A packed input reaches its consumer materialized (the explicit boundary), once per packed
/// output: the copy keeps a stable data id, so downstream cache keys still hit. ponytail: the last
/// 8 packed outputs are kept; an LRU keyed by output identity if graphs pack more than that.
fn input_geometry(session: &mut T, output: &Output) -> Result<payload::T, Diagnostic> {
    let (payload::T::Geometry(source), Some(transforms)) = (&output.payload, &output.instances) else {
        return Ok(output.payload.clone());
    };
    let existing = session.materialized.iter().find(|(o, _)| same_output(o, output)).map(|(_, g)| g.clone());
    if let Some(events) = &mut session.journal {
        events.push(CacheEvent::MaterializedRead(output.clone(), existing.clone()));
    }
    if let Some(geometry) = existing {
        return Ok(payload::T::Geometry(geometry));
    }
    match rdk_ops::instance_copy::materialize_instances(transforms, source) {
        Err(error) => Err(crate::support::diagnostic_with(
            rdk_core::error::code(&error).as_str(),
            "packed instances could not be materialized",
            Some(rdk_core::error::to_string(&error)),
            Vec::new(),
        )),
        Ok(geometry) => {
            if let Some(events) = &mut session.journal {
                events.push(CacheEvent::MaterializedWrite(output.clone(), geometry.clone()));
            }
            session.materialized.insert(0, (output.clone(), geometry.clone()));
            session.materialized.truncate(8);
            Ok(payload::T::Geometry(geometry))
        }
    }
}

// ------------------------------------------------------------------ per-cook memo

/// A table of this cook's results by node id so a node reachable through several paths is
/// evaluated once; the physical check guards reused ids.
struct Memo {
    results: HashMap<i64, Vec<(node::T, Result<Output, Diagnostic>)>>,
    census: Arc<Census>,
    left: Vec<i64>,
}

fn memo_find(memo: &Memo, n: &node::T) -> Option<Result<Output, Diagnostic>> {
    memo.results.get(&node::id(n))?.iter().find(|(seen, _)| node::ptr_eq(seen, n)).map(|(_, r)| r.clone())
}

fn memo_remove(memo: &mut Memo, n: &node::T) {
    if let Some(list) = memo.results.get_mut(&node::id(n)) {
        list.retain(|(seen, _)| !node::ptr_eq(seen, n));
        if list.is_empty() {
            memo.results.remove(&node::id(n));
        }
    }
}

/// `n` completed: an input with no reader left drops out of the memo. Keyed by physical node and by
/// graph edge, so a forwarding diamond counts each consumer once.
fn consumed(memo: &mut Memo, n: &node::T) {
    let Some(serial) = serial_of(&memo.census, n) else { return };
    let census = Arc::clone(&memo.census);
    for input in &census.distinct_inputs[serial] {
        memo.left[*input] -= 1;
        if memo.left[*input] == 0 {
            memo_remove(memo, &census.nodes[*input]);
        }
    }
}

fn selected_inputs(n: &node::T) -> Vec<node::T> {
    let inputs = build::input_array(n);
    match build::input_policy(n) {
        node::InputPolicy::All => inputs,
        node::InputPolicy::Only(i) => vec![inputs[i as usize].clone()],
    }
}

struct ConsumerEntry {
    node: node::T,
    serial: usize,
    readers: BTreeSet<usize>,
    external_readers: i64,
    known_consumer: bool,
    unknown_consumer: bool,
}

struct Scan {
    entries: Vec<ConsumerEntry>,
    by_id: HashMap<i64, Vec<usize>>,
    /// post-order: inputs before consumers
    post: Vec<usize>,
}

fn scan_visit(scan: &mut Scan, n: &node::T) -> usize {
    if let Some(found) = scan.by_id.get(&node::id(n)).and_then(|v| v.iter().find(|i| node::ptr_eq(&scan.entries[**i].node, n))) {
        return *found;
    }
    let serial = scan.entries.len();
    scan.entries.push(ConsumerEntry {
        node: n.clone(),
        serial,
        readers: BTreeSet::new(),
        external_readers: 0,
        known_consumer: false,
        unknown_consumer: false,
    });
    scan.by_id.entry(node::id(n)).or_default().push(serial);
    for child in selected_inputs(n) {
        scan_visit(scan, &child);
    }
    scan.post.push(serial);
    serial
}

fn scan_find(scan: &Scan, n: &node::T) -> usize {
    *scan.by_id[&node::id(n)].iter().find(|i| node::ptr_eq(&scan.entries[**i].node, n)).expect("scanned node")
}

fn consumer_context(session: &mut T, context: &context::T, root: &node::T, external_readers: Option<i64>) -> (context::T, Arc<Census>) {
    let base = consumers_of::consumers(context);
    if let Some(plan) = &session.consumer_plan {
        if node::ptr_eq(&plan.root, root) && plan.readers == external_readers && Arc::ptr_eq(&plan.previous, &base) {
            return (consumers_of::with_consumers(context, Arc::clone(&plan.planned)), Arc::clone(&plan.census));
        }
    }
    session.consumer_scans += 1;
    let mut scan = Scan { entries: Vec::new(), by_id: HashMap::new(), post: Vec::new() };
    let first = scan_visit(&mut scan, root);
    if let Some(count) = external_readers {
        scan.entries[first].known_consumer = true;
        scan.entries[first].external_readers = count;
    }
    // consumers before inputs
    let order: Vec<usize> = scan.post.iter().rev().copied().collect();
    for serial in &order {
        let consumer = scan.entries[*serial].node.clone();
        match node::input_use(&consumer) {
            node::InputUse::Unknown => {
                for child in selected_inputs(&consumer) {
                    let c = scan_find(&scan, &child);
                    scan.entries[c].unknown_consumer = true;
                }
            }
            node::InputUse::Cpu => {
                for child in selected_inputs(&consumer) {
                    let c = scan_find(&scan, &child);
                    scan.entries[c].known_consumer = true;
                    scan.entries[c].readers.insert(*serial);
                }
            }
            node::InputUse::Forward(i) => {
                let child = build::input_array(&consumer)[i as usize].clone();
                let c = scan_find(&scan, &child);
                let (known, unknown, external, readers) = {
                    let e = &scan.entries[*serial];
                    (e.known_consumer, e.unknown_consumer, e.external_readers, e.readers.clone())
                };
                let target = &mut scan.entries[c];
                target.known_consumer = target.known_consumer || known;
                target.unknown_consumer = target.unknown_consumer || unknown;
                target.external_readers = target.external_readers.max(external);
                target.readers.extend(readers);
            }
        }
    }
    let ids: BTreeSet<i64> = scan.by_id.keys().copied().collect();
    let mut planned = Arc::clone(&base);
    for id in ids {
        let versions = &scan.by_id[&id];
        let count = if versions.len() == 1 {
            let entry = &scan.entries[versions[0]];
            if entry.known_consumer && !entry.unknown_consumer {
                let local = entry.readers.len() as i64;
                entry.external_readers.checked_add(local)
            } else {
                None
            }
        } else {
            None
        };
        // An enclosing cook's facts also cover uses outside this nested graph. Do not replace them
        // with a smaller local view. Reused ids are ambiguous.
        let count = match (versions.len(), consumers_of::find(&base, id)) {
            (1, Some(outer)) if id != node::id(root) => outer,
            _ => count,
        };
        planned = consumers_of::set(&planned, id, count);
    }
    // ponytail: keep only the latest root's census; add a bounded multi-root cache if alternating
    // unchanged roots makes this metadata scan measurable.
    let census = if scan.entries.iter().any(|e| build::has_expand(&e.node)) {
        Census { nodes: Vec::new(), serials: HashMap::new(), distinct_inputs: Vec::new(), consumers: Vec::new() }
    } else {
        let count = scan.entries.len();
        let mut census = Census {
            nodes: vec![root.clone(); count],
            serials: HashMap::new(),
            distinct_inputs: vec![Vec::new(); count],
            consumers: vec![0; count],
        };
        for entry in &scan.entries {
            census.nodes[entry.serial] = entry.node.clone();
            census.serials.entry(node::id(&entry.node)).or_default().push((entry.node.clone(), entry.serial));
            let inputs: BTreeSet<usize> =
                selected_inputs(&entry.node).iter().map(|child| scan.entries[scan_find(&scan, child)].serial).collect();
            for input in &inputs {
                census.consumers[*input] += 1;
            }
            census.distinct_inputs[entry.serial] = inputs.into_iter().collect();
        }
        census
    };
    let census = Arc::new(census);
    session.consumer_plan = Some(ConsumerPlan {
        root: root.clone(),
        readers: external_readers,
        previous: base,
        planned: Arc::clone(&planned),
        census: Arc::clone(&census),
    });
    (consumers_of::with_consumers(context, planned), census)
}

// ------------------------------------------------------------------ cached

#[derive(Clone)]
enum Prediction {
    Known(Output),
    DeferredRefresh,
    Missing,
}

type Results = HashMap<i64, Vec<(node::T, Result<Output, Diagnostic>)>>;
type Predictions = HashMap<i64, Vec<(node::T, Prediction)>>;

fn cached_output(predictions: &mut Predictions, results: &Results, session: &T, context: &context::T, n: &node::T) -> Prediction {
    if let Some(found) = predictions.get(&node::id(n)).and_then(|v| v.iter().find(|(seen, _)| node::ptr_eq(seen, n))) {
        return found.1.clone();
    }
    let remembered = results.get(&node::id(n)).and_then(|v| v.iter().find(|(seen, _)| node::ptr_eq(seen, n))).map(|(_, r)| r.clone());
    let prediction = match remembered {
        Some(Ok(output)) => Prediction::Known(output),
        Some(Err(_)) => Prediction::Missing,
        None => predict(predictions, results, session, context, n),
    };
    predictions.entry(node::id(n)).or_default().push((n.clone(), prediction.clone()));
    prediction
}

fn predict(predictions: &mut Predictions, results: &Results, session: &T, context: &context::T, n: &node::T) -> Prediction {
    let inputs = selected_inputs(n);
    let input_predictions: Vec<Prediction> = inputs.iter().map(|i| cached_output(predictions, results, session, context, i)).collect();
    let values: Vec<Option<payload::T>> = input_predictions
        .iter()
        .map(|p| match p {
            Prediction::Known(output) => match (&output.payload, &output.instances) {
                (payload::T::Geometry(_), Some(_)) => session
                    .materialized
                    .iter()
                    .find(|(o, _)| same_output(o, output))
                    .map(|(_, g)| payload::T::Geometry(g.clone())),
                _ => Some(output.payload.clone()),
            },
            _ => None,
        })
        .collect();
    if input_predictions.iter().any(|p| matches!(p, Prediction::DeferredRefresh)) {
        Prediction::DeferredRefresh
    } else if values.iter().any(Option::is_none) {
        Prediction::Missing
    } else {
        let geometries: Vec<payload::T> = values.into_iter().flatten().collect();
        let key = cache_key(Vec::new(), n, context, &geometries);
        let found = if (session.volatile)(node::id(n)) {
            match session.slots.get(&node::id(n)) {
                Some((stored, entry)) if stored == &key => Some(Arc::clone(entry)),
                _ => None,
            }
        } else {
            session.cache.peek(&key_of(&key)).map(|v| entry_of(&v))
        };
        match found {
            None => Prediction::Missing,
            Some(entry) => match &entry.delta {
                Some(delta) if delta.source != payload::data_id(&geometries[0]) => Prediction::DeferredRefresh,
                _ => Prediction::Known(entry.output.clone()),
            },
        }
    }
}

/// What `cook` would answer from the cache for this node under `context`, without cooking,
/// materialising or reading anything back: `None` when any node on the way is not cached (a node
/// fused into a kernel has no output of its own).
pub fn cached(session: &T, context: &context::T, n: &node::T) -> Option<Output> {
    if session.closed {
        return None;
    }
    match cached_output(&mut HashMap::new(), &HashMap::new(), session, context, n) {
        Prediction::Known(output) => Some(output),
        _ => None,
    }
}

// ------------------------------------------------------------------ evaluation

fn evaluate(memo: &mut Memo, session: &mut T, context: &context::T, n: &node::T) -> Result<Output, Diagnostic> {
    if let Some(result) = memo_find(memo, n) {
        return result;
    }
    let result = evaluate_uncached(memo, session, context, n);
    memo.results.entry(node::id(n)).or_default().push((n.clone(), result.clone()));
    consumed(memo, n);
    result
}

const BRANCH_MIN_SECONDS: f64 = 0.002;
const BRANCH_MIN_POINTS: i64 = 10_000;

/// Snapshot metadata, not payload planes. Exact CLOCK touch bits matter when an earlier branch
/// evicts an entry a later branch initially found.
fn snapshot(session: &T) -> T {
    // a worker's drops are the parent's to decide when it replays the worker's writes
    let pool: SharedPool = {
        let source = session.payload.lock().unwrap();
        Arc::new(Mutex::new(Pool {
            refs: source.refs.clone(),
            bytes: source.bytes,
            owner: source.owner.clone(),
            charged: HashMap::new(),
            resident: HashMap::new(),
            dropped: Vec::new(),
            handed: HashSet::new(),
            ledgered: false,
        }))
    };
    let release_pool = Arc::clone(&pool);
    // The snapshot charges nothing: what it writes the parent charges when it replays.
    let cache = session.cache.copy(Arc::new(move |_, value| {
        let entry = entry_of(value);
        release_components(&mut release_pool.lock().unwrap(), &entry.components);
    }));
    T {
        clock: Arc::clone(&session.clock),
        max_entries: session.max_entries,
        max_payload_bytes: session.max_payload_bytes,
        cache,
        payload: pool,
        volatile_owner: session.volatile_owner.clone(),
        cooks: 0,
        hits: 0,
        misses: 0,
        volatile: Arc::clone(&session.volatile),
        slots: session.slots.clone(),
        slot_residents: session.slot_residents.clone(),
        slot_bytes: session.slot_bytes.clone(),
        memo_pool: Default::default(),
        key_pool: Vec::new(),
        volatile_hits: 0,
        volatile_misses: 0,
        evictions: 0,
        last_node: None,
        times: Arc::new(Mutex::new(HashMap::new())),
        closed: session.closed,
        materialized: session.materialized.clone(),
        journal: None,
        parallel_override: Some(false),
        fanouts: 0,
        previous_root: session.previous_root.clone(),
        written: Vec::new(),
        consumer_plan: session.consumer_plan.clone(),
        consumer_scans: session.consumer_scans,
    }
}

/// Cached entries are the same owner, or retain the same output without a delta.
fn same_entry(a: &Arc<Entry>, b: &Arc<Entry>) -> bool {
    Arc::ptr_eq(a, b) || (same_output(&a.output, &b.output) && a.delta.is_none() && b.delta.is_none())
}

/// Applies one journaled event; `false` when a read no longer observes what the worker saw.
fn replay_event(session: &mut T, event: &CacheEvent) -> bool {
    match event {
        CacheEvent::Read(volatile, key, expected) => {
            let observed = match volatile {
                None => session.cache.find_opt(&key_of(key)).map(|v| entry_of(&v)),
                Some(id) => match session.slots.get(id) {
                    Some((stored, entry)) if stored == key => Some(Arc::clone(entry)),
                    _ => None,
                },
            };
            match (observed, expected) {
                (None, None) => true,
                (Some(a), Some(b)) => same_entry(&a, b),
                _ => false,
            }
        }
        CacheEvent::Write(None, key, entry) => {
            insert(session, key, entry.clone());
            true
        }
        CacheEvent::Write(Some(id), key, entry) => {
            store_slot(session, *id, key, entry.clone());
            true
        }
        CacheEvent::MaterializedRead(output, expected) => {
            let found = session.materialized.iter().find(|(o, _)| same_output(o, output)).map(|(_, g)| g);
            match (found, expected) {
                (None, None) => true,
                (Some(a), Some(b)) => rdk_core::geometry::data_id(a) == rdk_core::geometry::data_id(b),
                _ => false,
            }
        }
        CacheEvent::MaterializedWrite(output, geometry) => {
            session.materialized.insert(0, (output.clone(), geometry.clone()));
            session.materialized.truncate(8);
            true
        }
        CacheEvent::Time(sample) => {
            record_time(session, sample.clone());
            true
        }
    }
}

fn results_has(results: &Results, n: &node::T) -> bool {
    results.get(&node::id(n)).is_some_and(|entries| entries.iter().any(|(seen, _)| node::ptr_eq(seen, n)))
}

/// A node list keyed by id with the physical check, for visited sets.
type Seen = HashMap<i64, Vec<node::T>>;

fn seen_has(seen: &Seen, n: &node::T) -> bool {
    seen.get(&node::id(n)).is_some_and(|v| v.iter().any(|s| node::ptr_eq(s, n)))
}

fn seen_add(seen: &mut Seen, n: &node::T) {
    seen.entry(node::id(n)).or_default().push(n.clone());
}

/// The root's own time excludes its inputs. Placement budgets the uncached subtree, otherwise a
/// cheap final operator hides an expensive branch.
fn costly(predictions: &mut Predictions, results: &Results, session: &T, context: &context::T, n: &node::T) -> bool {
    match session.parallel_override {
        Some(forced) => forced,
        None => {
            struct Budget {
                visited: Seen,
                seconds: f64,
                large: bool,
            }
            fn enough(b: &Budget) -> bool {
                b.seconds >= BRANCH_MIN_SECONDS || b.large
            }
            fn visit(
                b: &mut Budget,
                predictions: &mut Predictions,
                results: &Results,
                session: &T,
                context: &context::T,
                n: &node::T,
            ) {
                if enough(b)
                    || seen_has(&b.visited, n)
                    || results_has(results, n)
                    || !matches!(cached_output(predictions, results, session, context, n), Prediction::Missing)
                {
                    return;
                }
                seen_add(&mut b.visited, n);
                match node_seconds(session, node::id(n)) {
                    Some(elapsed) => b.seconds += elapsed,
                    None => {
                        let id = node::id(n);
                        b.large = session
                            .cache
                            .find_first(&|key, _| {
                                key.as_any()
                                    .downcast_ref::<Vec<u8>>()
                                    .is_some_and(|k| k.len() >= 8 && k[..8] == id.to_le_bytes())
                            })
                            .is_some_and(|value| payload::element_count(&entry_of(&value).output.payload) >= BRANCH_MIN_POINTS);
                    }
                }
                for input in selected_inputs(n) {
                    visit(b, predictions, results, session, context, &input);
                }
            }
            let mut budget = Budget { visited: HashMap::new(), seconds: 0.0, large: false };
            visit(&mut budget, predictions, results, session, context, n);
            enough(&budget)
        }
    }
}

/// Complete the same sequential DFS prefix that reaches every shared node. Once it is memoized, the
/// remaining branch DAGs have disjoint mutable state. ponytail: opaque expansions stay sequential
/// until their concrete roots exist; broaden scheduling only if nested-zone measurements justify it.
fn shared_prefix(memo: &mut Memo, session: &mut T, context: &context::T, roots: &[node::T]) -> Option<Result<(), Diagnostic>> {
    let mut counts: HashMap<i64, Vec<(node::T, usize)>> = HashMap::new();
    let mut opaque = false;
    for root in roots {
        let mut visited: Seen = HashMap::new();
        fn visit(
            n: &node::T,
            memo: &Memo,
            visited: &mut Seen,
            counts: &mut HashMap<i64, Vec<(node::T, usize)>>,
            opaque: &mut bool,
        ) {
            if results_has(&memo.results, n) || seen_has(visited, n) {
                return;
            }
            seen_add(visited, n);
            let list = counts.entry(node::id(n)).or_default();
            match list.iter_mut().find(|(seen, _)| node::ptr_eq(seen, n)) {
                Some((_, count)) => *count += 1,
                None => list.push((n.clone(), 1)),
            }
            if build::has_expand(n) {
                *opaque = true;
            }
            for input in selected_inputs(n) {
                visit(&input, memo, visited, counts, opaque);
            }
        }
        visit(root, memo, &mut visited, &mut counts, &mut opaque);
    }
    if opaque {
        return None;
    }
    let mut pending: Vec<node::T> =
        counts.into_values().flatten().filter(|(_, count)| *count > 1).map(|(n, _)| n).collect();
    // ponytail: shared-node membership scans this coarse DAG frontier; use a physical-node set if
    // large shared frontiers dominate cook measurements.
    fn contains(n: &node::T, memo: &Memo, pending: &[node::T]) -> bool {
        !results_has(&memo.results, n)
            && (pending.iter().any(|p| node::ptr_eq(p, n)) || selected_inputs(n).iter().any(|c| contains(c, memo, pending)))
    }
    fn prefix(
        n: &node::T,
        memo: &mut Memo,
        session: &mut T,
        context: &context::T,
        pending: &mut Vec<node::T>,
    ) -> Result<(), Diagnostic> {
        if pending.is_empty() || results_has(&memo.results, n) {
            return Ok(());
        }
        if pending.iter().any(|p| node::ptr_eq(p, n)) || !contains(n, memo, pending) {
            evaluate(memo, session, context, n)?;
            pending.retain(|shared| !results_has(&memo.results, shared));
            return Ok(());
        }
        for child in selected_inputs(n) {
            if pending.is_empty() {
                break;
            }
            prefix(&child, memo, session, context, pending).map_err(|e| traced(n, e))?;
        }
        Ok(())
    }
    for root in roots {
        if pending.is_empty() {
            break;
        }
        if let Err(error) = prefix(root, memo, session, context, &mut pending) {
            return Some(Err(error));
        }
    }
    Some(Ok(()))
}

fn clone_memo(memo: &Memo, left: Vec<i64>) -> Memo {
    Memo { results: memo.results.clone(), census: Arc::clone(&memo.census), left }
}

/// Worker copies of memoized results retain their parent's result identity.
fn same_result(a: &Result<Output, Diagnostic>, b: &Result<Output, Diagnostic>) -> bool {
    match (a, b) {
        (Ok(a), Ok(b)) => same_output(a, b),
        (Err(a), Err(b)) => rays_diagnostic::error_to_string(a) == rays_diagnostic::error_to_string(b),
        _ => false,
    }
}

type Branch = Result<(Output, payload::T), Diagnostic>;

fn evaluate_branch(memo: &mut Memo, session: &mut T, context: &context::T, root: &node::T) -> Branch {
    let output = evaluate(memo, session, context, root)?;
    let geometry = input_geometry(session, &output)?;
    Ok((output, geometry))
}

/// Workers mutate only copied cache metadata and disjoint memo tables. Reads are validated against
/// the current parent before any journal is applied; a prior branch's eviction may require a
/// sequential recook. This keeps CLOCK order, retention and counters identical to input-order
/// execution.
fn evaluate_many(memo: &mut Memo, session: &mut T, context: &context::T, roots: &[node::T]) -> Result<Vec<(Output, payload::T)>, Diagnostic> {
    let count = roots.len();
    let candidates = |memo: &Memo, session: &T| -> Vec<usize> {
        let mut predictions: Predictions = HashMap::new();
        (0..count)
            .filter(|i| {
                let n = &roots[*i];
                !results_has(&memo.results, n)
                    && costly(&mut predictions, &memo.results, session, context, n)
                    && matches!(cached_output(&mut predictions, &memo.results, session, context, n), Prediction::Missing)
            })
            .collect()
    };
    let sequential = |memo: &mut Memo, session: &mut T| -> Result<Vec<(Output, payload::T)>, Diagnostic> {
        roots.iter().map(|root| evaluate_branch(memo, session, context, root)).collect()
    };
    if count < 2
        || context::domains(context) < 2
        || session.parallel_override == Some(false)
        || candidates(memo, session).len() < 2
    {
        return sequential(memo, session);
    }
    match shared_prefix(memo, session, context, roots) {
        None => return sequential(memo, session),
        Some(Err(error)) => return Err(error),
        Some(Ok(())) => {}
    }
    let indices = candidates(memo, session);
    if indices.len() < 2 {
        return sequential(memo, session);
    }
    // the shared prefix's device work settles before branches read it
    context::settle(context, true);
    session.fanouts += 1;
    let forked = memo.left.clone();
    type Worker = (T, Memo);
    let workers: Vec<Mutex<Option<Worker>>> = indices
        .iter()
        .map(|_| Mutex::new(Some((snapshot(session), clone_memo(memo, forked.clone())))))
        .collect();
    type Transaction = (T, Memo, Branch);
    let transactions: Vec<Mutex<Option<Transaction>>> = (0..count).map(|_| Mutex::new(None)).collect();
    let _ = parallel::for_(1, 0, indices.len() as i64 - 1, &|worker_index| {
        let i = indices[worker_index as usize];
        let (mut local, mut local_memo) = workers[worker_index as usize].lock().unwrap().take().expect("worker taken once");
        local.journal = Some(Vec::new());
        let result = evaluate_branch(&mut local_memo, &mut local, context, &roots[i]);
        *transactions[i].lock().unwrap() = Some((local, local_memo, result));
        Ok(())
    });
    let cancelled_error = |transactions: &[Mutex<Option<Transaction>>], from: usize| -> Diagnostic {
        for i in from..count {
            match &*transactions[i].lock().unwrap() {
                Some((_, _, Err(error))) => return error.clone(),
                Some((_, _, Ok(_))) => {}
                None => return cancellation_error(&roots[i]),
            }
        }
        cancellation_error(&roots[0])
    };
    let mut outputs: Vec<(Output, payload::T)> = Vec::with_capacity(count);
    for i in 0..count {
        if context::cancelled(context) {
            return Err(cancelled_error(&transactions, i));
        }
        let transaction = transactions[i].lock().unwrap().take();
        let result: Branch = match transaction {
            None => evaluate_branch(memo, session, context, &roots[i]),
            Some((local, local_memo, result)) => {
                let events = local.journal.as_ref().expect("worker journal");
                let compatible = local_memo.results.values().flatten().all(|(n, worker_result)| {
                    memo.results.get(&node::id(n)).and_then(|entries| entries.iter().find(|(seen, _)| node::ptr_eq(seen, n))).map(|(_, r)| r).is_none_or(|parent_result| same_result(worker_result, parent_result))
                });
                let mut probe = snapshot(session);
                if compatible && events.iter().all(|event| replay_event(&mut probe, event)) {
                    for event in events {
                        replay_event(session, event);
                    }
                    session.cooks += local.cooks;
                    session.hits += local.hits;
                    session.misses += local.misses;
                    session.volatile_hits += local.volatile_hits;
                    session.volatile_misses += local.volatile_misses;
                    if local.last_node.is_some() {
                        session.last_node = local.last_node.clone();
                    }
                    // the worker's completed consumers count against the parent's readers
                    for (serial, left) in local_memo.left.iter().enumerate() {
                        let completed = forked[serial] - left;
                        if completed > 0 {
                            memo.left[serial] -= completed;
                            if memo.left[serial] == 0 {
                                let census = Arc::clone(&memo.census);
                                memo_remove(memo, &census.nodes[serial]);
                            }
                        }
                    }
                    for (id, list) in &local_memo.results {
                        for (n, value) in list {
                            let wanted = match serial_of(&memo.census, n) {
                                None => true,
                                Some(serial) => memo.left[serial] > 0,
                            };
                            if wanted && !results_has(&memo.results, n) {
                                memo.results.entry(*id).or_default().push((n.clone(), value.clone()));
                            }
                        }
                    }
                    result
                } else {
                    evaluate_branch(memo, session, context, &roots[i])
                }
            }
        };
        outputs.push(result?);
    }
    Ok(outputs)
}

/// A zone expands on its caller; its concrete element roots use the same input-order join as
/// ordinary independent branches.
fn cook_node(memo: &mut Memo, session: &mut T, context: &context::T, n: &node::T, geometries: &[payload::T]) -> Result<node::Cooked, Diagnostic> {
    match build::expand(n) {
        None => build::cook(n, context, geometries),
        Some(expand) => {
            let roots = expand(context, &build::input_array(n), geometries)?;
            match evaluate_many(memo, session, context, &roots) {
                Err(error) => Err(traced(n, error)),
                Ok(outputs) => {
                    let payloads: Vec<payload::T> = outputs.into_iter().map(|(_, p)| p).collect();
                    build::cook(n, context, &payloads)
                }
            }
        }
    }
}

fn evaluate_uncached(memo: &mut Memo, session: &mut T, context: &context::T, n: &node::T) -> Result<Output, Diagnostic> {
    if context::cancelled(context) {
        return Err(cancellation_error(n));
    }
    let outputs = evaluate_many(memo, session, context, &selected_inputs(n)).map_err(|e| traced(n, e))?;
    let geometries: Vec<payload::T> = outputs.into_iter().map(|(_, p)| p).collect();
    // Zero-entry sessions need only the per-cook memo. Speculative worker journals still need semantic
    // keys for ordered parent replay.
    let cache_active = session.max_entries > 0 || session.journal.is_some();
    let key = if cache_active { cache_key(std::mem::take(&mut session.key_pool), n, context, &geometries) } else { Vec::new() };
    let volatile = (session.volatile)(node::id(n));
    let found = if cache_active { lookup(session, volatile, n, &key) } else { None };
    if let Some(entry) = found {
        let refreshed = node_result(|| refresh(&geometries, &entry)).map_err(|e| traced(n, e))?;
        let output = match refreshed {
            Some(fresh) => {
                let output = fresh.output.clone();
                if volatile {
                    store_slot(session, node::id(n), &key, fresh);
                } else {
                    insert(session, &key, fresh);
                }
                output
            }
            None => entry.output.clone(),
        };
        session.hits += 1;
        if volatile {
            session.volatile_hits += 1;
        }
        session.last_node = Some(LastNode { node: n.clone(), input_points: geometries.iter().map(payload::element_count).sum(), points: payload::element_count(&output.payload), seconds: 0.0, cache_hit: true });
        session.key_pool = key;
        return Ok(output);
    }
    session.misses += 1;
    if volatile {
        session.volatile_misses += 1;
    }
    session.cooks += 1;
    let started = (session.clock)();
    let cooked: Result<(node::Cooked, Option<Delta>), Diagnostic> = node_result(|| {
        // no CPU reader sees device work still in flight
        if !context::settle(context, false).is_empty() {
            return Err(settle_failed());
        }
        // a parallel branch.s journal replays later: only the cook.s own thread sheds
        let admitted = {
            let cache = &session.cache;
            let journaling = session.journal.is_some();
            let evictions = RefCell::new(0_i64);
            let result = mem::admit_with(&mut || {
                let dropped = !journaling && cache.drop_oldest();
                if dropped {
                    *evictions.borrow_mut() += 1;
                }
                dropped
            });
            session.evictions += evictions.into_inner();
            result
        };
        if let Err(overdraft) = admitted {
            return Err(crate::support::diagnostic("E_MEMORY", &mem::describe(&overdraft)));
        }
        let cooked = cook_node(memo, session, context, n, &geometries)?;
        let delta = delta(n, &geometries, &cooked)?;
        Ok((cooked, delta))
    });
    let seconds = ((session.clock)() - started).max(0.0);
    let points = match &cooked {
        Ok((output, _)) => payload::element_count(&output.payload),
        Err(_) => 0,
    };
    let sample = timing(n, &geometries, points, seconds, false);
    session.last_node = Some(LastNode { node: n.clone(), input_points: sample.input_points, points, seconds, cache_hit: false });
    record_time(session, sample);
    match cooked {
        Err(error) => Err(traced(n, error)),
        Ok(_) if context::cancelled(context) => Err(cancellation_error(n)),
        Ok((cooked, delta)) => {
            let output = Output { payload: cooked.payload, instances: cooked.instances };
            if cache_active {
                let entry = Entry { output: output.clone(), components: Vec::new(), delta };
                if volatile {
                    store_slot(session, node::id(n), &key, entry);
                } else {
                    insert(session, &key, entry);
                }
            }
            Ok(output)
        }
    }
}

/// At a cook's end, on the cooking thread: what entries dropped and neither this cook's root nor
/// the previous one holds goes back to its device now, not when it is collected: retired (still
/// readable) if a root ever held it, else reused at once, since nothing outside the session saw it.
fn release_dropped(session: &mut T, result: &Result<Output, Diagnostic>) {
    let root: Vec<i64> = match result {
        Ok(output) => releasers(output).iter().map(|(i, _)| *i).collect(),
        Err(_) => Vec::new(),
    };
    let mut releases: Vec<(Release, bool)> = Vec::new();
    {
        let mut pool = session.payload.lock().unwrap();
        for id in &root {
            if pool.resident.contains_key(id) {
                pool.handed.insert(*id);
            }
        }
        let mut held = root.clone();
        held.extend(session.previous_root.iter().copied());
        session.previous_root = root;
        if !pool.dropped.is_empty() {
            // Prefer slot_residents: Geometry releasers are empty after a deferred plane is materialised.
            let mut slots: Vec<i64> = session.slot_residents.values().flatten().copied().collect();
            for (id, (_, entry)) in &session.slots {
                if !session.slot_residents.contains_key(id) {
                    slots.extend(releasers(&entry.output).iter().map(|(i, _)| *i));
                }
            }
            let dropped = std::mem::take(&mut pool.dropped);
            let unique: BTreeSet<i64> = dropped.into_iter().collect();
            for id in unique {
                if pool.refs.contains_key(&id) || slots.contains(&id) {
                    continue;
                }
                if held.contains(&id) {
                    pool.dropped.push(id);
                } else if let Some(release) = pool.resident.remove(&id) {
                    let handed = pool.handed.remove(&id);
                    releases.push((release, handed));
                }
            }
        }
    }
    for (release, handed) in releases {
        release(handed);
    }
}

/// Consumer facts are inferred only through explicitly declared CPU readers and nonforcing
/// forwarders. `cpu_consumers` declares the root's external use; `None` is unknown, zero is a known
/// resident consumer. Counts are scoped to this cook.
pub fn cook(session: &mut T, cpu_consumers: Option<i64>, context: &context::T, n: &node::T) -> Result<Output, Diagnostic> {
    if session.closed {
        return Err(traced(n, crate::support::diagnostic("session_closed", "cannot cook with a closed procedural session")));
    }
    if cpu_consumers.is_some_and(|c| c < 0) {
        return Err(crate::support::diagnostic("consumer_count", "CPU consumer count must be non-negative."));
    }
    let external = match cpu_consumers {
        Some(_) => cpu_consumers,
        None => context::cpu_consumers(context, node::id(n)),
    };
    let (context, census) = consumer_context(session, context, n, external);
    let cell = RefCell::new(&mut *session);
    let outcome: RefCell<Option<Result<Output, Diagnostic>>> = RefCell::new(None);
    let _ = parallel::run_unit(Some(context::domains(&context)), &|| {
        let mut guard = cell.borrow_mut();
        let session: &mut T = &mut guard;
        // a device failure found at a settle point refuses the device to the nodes that failed:
        // what this attempt wrote is dropped and the graph cooks again (each retry refuses one
        // more node, so it ends)
        let result = loop {
            session.written.clear();
            let refused = context::refusals(&context);
            let mut memo = Memo { results: std::mem::take(&mut session.memo_pool.0), left: std::mem::take(&mut session.memo_pool.1), census: Arc::clone(&census) };
            memo.left.clone_from(&census.consumers);
            let result = evaluate(&mut memo, session, &context, n);
            memo.results.clear();
            session.memo_pool = (memo.results, memo.left);
            context::settle(&context, true);
            if context::refusals(&context) == refused {
                break result;
            }
            for written in std::mem::take(&mut session.written) {
                match written {
                    Written::Key(key) => session.cache.remove(&key_of(&key)),
                    Written::Slot(id) => {
                        release_slot_bytes(session, id);
                        session.slots.remove(&id);
                        session.slot_residents.remove(&id);
                    }
                }
            }
        };
        session.written.clear();
        release_dropped(session, &result);
        *outcome.borrow_mut() = Some(result);
        Ok(())
    });
    outcome.into_inner().expect("cook ran")
}

pub fn stats(session: &T) -> Stats {
    Stats {
        cooks: session.cooks,
        hits: session.hits,
        misses: session.misses,
        evictions: session.evictions,
        volatile_hits: session.volatile_hits,
        volatile_misses: session.volatile_misses,
        retained_entries: session.cache.length(),
        volatile_entries: session.slots.len() as i64,
        retained_payload_bytes: payload_bytes(session),
        last_node: session.last_node.as_ref().map(|l| NodeTiming {
            node_id: node::id(&l.node),
            label: node::label(&l.node).to_owned(),
            operation: node::operation(&l.node).to_owned(),
            input_points: l.input_points,
            points: l.points,
            seconds: l.seconds,
            cache_hit: l.cache_hit,
        }),
        consumer_scans: session.consumer_scans,
    }
}

/// what a clear or close drops is left to its holders' collection: results handed out stay valid
fn forget_resident(session: &mut T) {
    session.previous_root.clear();
    let mut pool = session.payload.lock().unwrap();
    pool.resident.clear();
    pool.handed.clear();
    pool.dropped.clear();
}

fn reset_slots_and_pool(session: &mut T) {
    let ids: Vec<i64> = session.slots.keys().copied().collect();
    for id in ids {
        release_slot_bytes(session, id);
    }
    session.slots.clear();
    session.slot_residents.clear();
    session.slot_bytes.clear();
    let mut pool = session.payload.lock().unwrap();
    pool.refs.clear();
    discharge_all(&mut pool);
    pool.bytes = 0;
}

pub fn clear(session: &mut T) {
    session.consumer_plan = None;
    session.times.lock().unwrap().clear();
    session.cache.clear();
    reset_slots_and_pool(session);
    forget_resident(session);
}

pub fn close(session: &mut T) {
    if !session.closed {
        clear(session);
        session.closed = true;
    }
}

/// Force or disable the coarse branch gate for exactness checks.
pub fn set_parallel_override(session: &mut T, value: Option<bool>) {
    session.parallel_override = value;
}

pub mod private {
    use super::*;

    pub fn fanouts(session: &T) -> i64 {
        session.fanouts
    }

    pub fn cache_keys(session: &T) -> Vec<Vec<u8>> {
        let keys = RefCell::new(Vec::new());
        session.cache.iter(&mut |key, _| {
            keys.borrow_mut().push(key.as_any().downcast_ref::<Vec<u8>>().cloned().unwrap_or_default())
        });
        let mut keys = keys.into_inner();
        keys.sort();
        keys
    }

    pub fn resident_count(session: &T) -> i64 {
        session.payload.lock().unwrap().resident.len() as i64
    }

    /// Cache-cold benchmarks after learning branch costs; counters are kept.
    pub fn clear_cache_keep_timings(session: &mut T) {
        session.cache.clear();
        reset_slots_and_pool(session);
        forget_resident(session);
        session.materialized.clear();
    }
}
