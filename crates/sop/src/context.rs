//! Port of lib/sop/context.ml: target-neutral facts available while cooking a procedural graph.
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::thread::ThreadId;

use rays_diagnostic::Diagnostic;
use rdk_core::cancel;

pub mod dependencies {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Fact {
        Frame,
        Time,
        Seed,
        Domains,
        Grain,
        Input,
        Pointer,
        Keys,
        Size,
        Step,
    }

    /// A bit set of facts.
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
    pub struct T(i64);

    pub const STATIC: T = T(0);

    fn bit(fact: Fact) -> i64 {
        match fact {
            Fact::Frame => 1,
            Fact::Time => 2,
            Fact::Seed => 4,
            Fact::Domains => 8,
            Fact::Grain => 16,
            Fact::Input => 32,
            Fact::Pointer => 64,
            Fact::Keys => 128,
            Fact::Size => 256,
            Fact::Step => 512,
        }
    }

    pub const ALL: [Fact; 10] = [
        Fact::Frame,
        Fact::Time,
        Fact::Seed,
        Fact::Domains,
        Fact::Grain,
        Fact::Input,
        Fact::Pointer,
        Fact::Keys,
        Fact::Size,
        Fact::Step,
    ];

    pub fn one(fact: Fact) -> T {
        T(bit(fact))
    }

    pub fn union(a: T, b: T) -> T {
        T(a.0 | b.0)
    }

    pub fn mem(fact: Fact, value: T) -> bool {
        value.0 & bit(fact) != 0
    }

    pub fn to_list(value: T) -> Vec<Fact> {
        ALL.iter().copied().filter(|fact| mem(*fact, value)).collect()
    }

    pub fn fact_name(fact: Fact) -> &'static str {
        match fact {
            Fact::Frame => "frame",
            Fact::Time => "time",
            Fact::Seed => "seed",
            Fact::Domains => "domains",
            Fact::Grain => "grain",
            Fact::Input => "input",
            Fact::Pointer => "pointer",
            Fact::Keys => "keys",
            Fact::Size => "size",
            Fact::Step => "step",
        }
    }

    pub fn to_string(value: T) -> String {
        let facts = to_list(value);
        if facts.is_empty() {
            "static".to_owned()
        } else {
            facts.iter().map(|fact| fact_name(*fact)).collect::<Vec<_>>().join(",")
        }
    }
}

use dependencies::Fact;

/// Known CPU-reader budget per node id (absent: unknown). Shared by pointer so a session can tell an
/// unchanged plan (`Arc::ptr_eq`).
pub type Consumers = Arc<BTreeMap<i64, Option<i64>>>;

pub type Settle = Box<dyn FnOnce() -> Result<(), Diagnostic> + Send>;

#[derive(Clone)]
pub struct T {
    frame: i64,
    time: f64,
    input: input::Frame,
    seed: i64,
    domains: i64,
    grain: i64,
    cancel: Arc<cancel::T>,
    deferred: Arc<Mutex<Vec<(ThreadId, i64, Settle)>>>,
    refused: Arc<Mutex<Vec<i64>>>,
    cpu_consumers: Consumers,
}

#[derive(Default)]
pub struct Options {
    pub frame: Option<i64>,
    pub time: Option<f64>,
    pub input: Option<input::Frame>,
    pub seed: Option<i64>,
    pub domains: Option<i64>,
    pub grain: Option<i64>,
    pub cancel: Option<Arc<cancel::T>>,
}

/// `Context.create`: the arguments are `Options` fields; `Options::default()` is `create ()`.
pub fn create(options: Options) -> Result<T, String> {
    let frame = options.frame.unwrap_or(0);
    let time = options.time.unwrap_or(0.0);
    let domains = options.domains.unwrap_or_else(parallel::recommended_domains);
    let grain = options.grain.unwrap_or(16_384);
    if !time.is_finite() {
        return Err("Context.create: time must be finite".to_owned());
    }
    if frame < 0 {
        return Err("Context.create: frame must be non-negative".to_owned());
    }
    if domains <= 0 {
        return Err("Context.create: domains must be positive".to_owned());
    }
    if grain <= 0 {
        return Err("Context.create: grain must be positive".to_owned());
    }
    let mut input = options.input.unwrap_or_else(|| input::at_time(time));
    input.t = time;
    input.frame = frame;
    input::validate(&input)?;
    Ok(T {
        frame,
        time,
        input,
        seed: options.seed.unwrap_or(0),
        domains,
        grain,
        cancel: options.cancel.unwrap_or_else(|| Arc::new(cancel::create())),
        deferred: Arc::new(Mutex::new(Vec::new())),
        refused: Arc::new(Mutex::new(Vec::new())),
        cpu_consumers: Arc::new(BTreeMap::new()),
    })
}

pub fn frame(value: &T) -> i64 {
    value.frame
}
pub fn time(value: &T) -> f64 {
    value.time
}
/// Full logical frame. Its time and index follow this context's clock.
pub fn input(value: &T) -> &input::Frame {
    &value.input
}
pub fn seed(value: &T) -> i64 {
    value.seed
}
pub fn domains(value: &T) -> i64 {
    value.domains
}
pub fn grain(value: &T) -> i64 {
    value.grain
}
pub fn cancel_token(value: &T) -> &Arc<cancel::T> {
    &value.cancel
}
pub fn cancelled(value: &T) -> bool {
    cancel::is_cancelled(&value.cancel)
}

/// Known CPU-reader budget for a node's output in this cook; `None` means unknown.
pub fn cpu_consumers(value: &T, node: i64) -> Option<i64> {
    value.cpu_consumers.get(&node).copied().flatten()
}

/// Add immutable consumer metadata, sharing cancellation and deferred checks. A negative count is
/// a programming error (OCaml `Invalid_argument`).
pub fn with_cpu_consumers(value: &T, node: i64, count: Option<i64>) -> T {
    if let Some(n) = count {
        assert!(n >= 0, "Context.with_cpu_consumers: negative count");
    }
    let mut map = (*value.cpu_consumers).clone();
    map.insert(node, count);
    T { cpu_consumers: Arc::new(map), ..value.clone() }
}

pub mod private {
    use super::*;

    pub type ConsumersMap = Consumers;

    pub fn consumers(value: &T) -> Consumers {
        Arc::clone(&value.cpu_consumers)
    }

    pub fn with_consumers(value: &T, cpu_consumers: Consumers) -> T {
        if Arc::ptr_eq(&value.cpu_consumers, &cpu_consumers) {
            value.clone()
        } else {
            T { cpu_consumers, ..value.clone() }
        }
    }

    /// `Some(count)` when the node has an entry; the inner option is the count itself.
    pub fn find(consumers: &Consumers, node: i64) -> Option<Option<i64>> {
        consumers.get(&node).copied()
    }

    pub fn set(consumers: &Consumers, node: i64, count: Option<i64>) -> Consumers {
        let mut map = (**consumers).clone();
        map.insert(node, count);
        Arc::new(map)
    }
}

/// Node `node` left device work in flight (from any thread): `settle` waits for it and checks its
/// status. The session settles before it cooks any node and at a cook's end.
pub fn defer(value: &T, node: i64, settle: Settle) {
    value
        .deferred
        .lock()
        .unwrap()
        .push((std::thread::current().id(), node, settle));
}

pub fn device_refused(value: &T, node: i64) -> bool {
    value.refused.lock().unwrap().contains(&node)
}

pub fn refusals(value: &T) -> i64 {
    value.refused.lock().unwrap().len() as i64
}

/// Runs the deferred checks this thread registered (`all`: every one), oldest first, and returns
/// the nodes whose device work failed; those are `device_refused` from then on.
pub fn settle(value: &T, all: bool) -> Vec<i64> {
    let pending: Vec<(ThreadId, i64, Settle)> = {
        let mut deferred = value.deferred.lock().unwrap();
        if all {
            std::mem::take(&mut *deferred)
        } else {
            let me = std::thread::current().id();
            let (own, others): (Vec<_>, Vec<_>) = std::mem::take(&mut *deferred).into_iter().partition(|(thread, _, _)| *thread == me);
            *deferred = others;
            own
        }
    };
    let mut failed = Vec::new();
    for (_, node, settle) in pending {
        if settle().is_err() {
            value.refused.lock().unwrap().insert(0, node);
            failed.push(node);
        }
    }
    failed
}

/// The slice of the host input a fact reads (empty for a fact that is not an input slice); exact bits.
pub fn input_part(fact: Fact, i: &input::Frame) -> Vec<u8> {
    fn bits(out: &mut Vec<u8>, x: f64) {
        out.extend_from_slice(&x.to_bits().to_le_bytes());
    }
    let mut out = Vec::new();
    match fact {
        Fact::Time => bits(&mut out, i.t),
        Fact::Pointer => {
            bits(&mut out, i.pointer.0);
            bits(&mut out, i.pointer.1);
            out.extend_from_slice(format!("{:?}", i.buttons).as_bytes());
        }
        Fact::Keys => out.extend_from_slice(format!("{:?}", i.keys).as_bytes()),
        Fact::Size => {
            out.extend_from_slice(&i.size.0.to_le_bytes());
            out.extend_from_slice(&i.size.1.to_le_bytes());
        }
        Fact::Step => {
            bits(&mut out, i.dt);
            out.extend_from_slice(&i.tick.to_le_bytes());
            out.extend_from_slice(&i.frame.to_le_bytes());
        }
        Fact::Input => out.extend_from_slice(input::identity(i).as_bytes()),
        Fact::Frame | Fact::Seed | Fact::Domains | Fact::Grain => {}
    }
    out
}

/// Whether any input slice the dependencies read differs between two frames.
pub fn input_changed(deps: dependencies::T, a: &input::Frame, b: &input::Frame) -> bool {
    [Fact::Time, Fact::Pointer, Fact::Keys, Fact::Size, Fact::Step, Fact::Input]
        .into_iter()
        .filter(|fact| dependencies::mem(*fact, deps))
        .any(|fact| input_part(fact, a) != input_part(fact, b))
}

/// Exact cache-key projection for the declared facts. Static dependencies always return the same
/// (empty) projection, independent of the context.
pub fn cache_projection(deps: dependencies::T, value: &T) -> Vec<u8> {
    if deps == dependencies::STATIC {
        return Vec::new();
    }
    let mut buffer = Vec::with_capacity(80);
    let add = |buffer: &mut Vec<u8>, tag: u8, data: &[u8]| {
        buffer.push(tag);
        buffer.extend_from_slice(data);
        buffer.push(b';');
    };
    if dependencies::mem(Fact::Frame, deps) {
        add(&mut buffer, b'f', value.frame.to_string().as_bytes());
    }
    if dependencies::mem(Fact::Time, deps) {
        add(&mut buffer, b't', (value.time.to_bits() as i64).to_string().as_bytes());
    }
    if dependencies::mem(Fact::Seed, deps) {
        add(&mut buffer, b's', value.seed.to_string().as_bytes());
    }
    if dependencies::mem(Fact::Domains, deps) {
        add(&mut buffer, b'd', value.domains.to_string().as_bytes());
    }
    if dependencies::mem(Fact::Grain, deps) {
        add(&mut buffer, b'g', value.grain.to_string().as_bytes());
    }
    if dependencies::mem(Fact::Input, deps) {
        add(&mut buffer, b'i', input::identity(&value.input).as_bytes());
    }
    for (tag, fact) in [(b'p', Fact::Pointer), (b'k', Fact::Keys), (b'z', Fact::Size), (b'e', Fact::Step)] {
        if dependencies::mem(fact, deps) {
            add(&mut buffer, tag, &input_part(fact, &value.input));
        }
    }
    buffer
}
