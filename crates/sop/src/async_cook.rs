//! Port of lib/sop/async_cook.ml: bounded latest-request background cooking for interactive
//! sketches.
//!
//! One persistent worker thread owns one procedural session. Submitting a newer request cancels the
//! active context, replaces the single pending slot, and prevents stale results from being
//! published. Prepared values must remain target-neutral; SDL and GPU work belongs on the initial
//! thread after polling.
//!
//! The OCaml `'a` of a worker is `Prepared = Box<dyn Any + Send>`: a caller downcasts what its own
//! `prepare` produced. The OCaml code also converts an exception in a cook into
//! `Uncaught_exception`; so does `execute` here (a panic in a cook is a typed error; the worker lives).
use std::any::Any;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex, Weak};
use std::thread::JoinHandle;

use rays_diagnostic::Diagnostic;
use rdk_core::cancel;

use crate::context;
use crate::node;
use crate::session::{self, Output};

pub type Prepared = Box<dyn Any + Send>;

pub enum Error {
    CookError(Diagnostic),
    PrepareError(String),
    UncaughtException(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Status {
    Idle,
    Cooking { request_id: i64, seconds: f64, queued: bool },
}

pub struct Completion {
    pub request_id: i64,
    pub seconds: f64,
    pub result: Result<Prepared, Error>,
}

pub type Prepare = Box<dyn FnOnce(Vec<Output>, Vec<Option<Output>>) -> Result<Prepared, String> + Send>;

struct Request {
    id: i64,
    context: context::T,
    nodes: Vec<node::T>,
    /// cooked after `nodes`; one that fails is `None`
    optional: Vec<node::T>,
    /// installed in the session by the worker before this cook
    volatile: Arc<dyn Fn(i64) -> bool + Send + Sync>,
    prepare: Prepare,
}

struct Active {
    started: f64,
    cancel: Arc<cancel::T>,
}

struct State {
    pending: Option<Request>,
    active: Option<Active>,
    completion: Option<Completion>,
    latest_id: i64,
    /// what the next request carries
    next_volatile: Arc<dyn Fn(i64) -> bool + Send + Sync>,
    stopping: bool,
    closed: bool,
    worker: Option<JoinHandle<()>>,
}

struct Shared {
    state: Mutex<State>,
    ready: Condvar,
    /// a request completed or the worker stopped
    finished: Condvar,
    session: Mutex<session::T>,
    clock: Mutex<Arc<dyn Fn() -> f64 + Send + Sync>>,
    times: Arc<Mutex<std::collections::HashMap<i64, session::NodeTiming>>>,
    stats: Mutex<session::Stats>,
    closed: AtomicBool,
}

#[derive(Clone)]
pub struct T(Arc<Shared>);

pub fn error_to_string(error: &Error) -> String {
    match error {
        Error::CookError(error) => rays_diagnostic::error_to_string(error),
        Error::PrepareError(message) => message.clone(),
        Error::UncaughtException(cause) => format!("background cook raised: {cause}"),
    }
}

fn clock_now(shared: &Shared) -> f64 {
    (shared.clock.lock().unwrap())()
}

fn none_volatile() -> Arc<dyn Fn(i64) -> bool + Send + Sync> {
    Arc::new(|_| false)
}

/// OCaml `with exn -> Error (Uncaught_exception ...)`: whatever the cook, the footer reads or `prepare`
/// raises is an error of this request; the worker lives on.
fn execute(session: &mut session::T, request: Request) -> Result<Prepared, Error> {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| execute_request(session, request))) {
        Ok(result) => result,
        Err(payload) => {
            let cause = payload.downcast_ref::<String>().cloned().or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_owned())).unwrap_or_else(|| "panic".to_owned());
            Err(Error::UncaughtException(cause))
        }
    }
}

fn execute_request(session: &mut session::T, request: Request) -> Result<Prepared, Error> {
    let mut outputs = Vec::with_capacity(request.nodes.len());
    for n in &request.nodes {
        match session::cook(session, None, &request.context, n) {
            Err(error) => return Err(Error::CookError(error)),
            Ok(output) => outputs.push(output),
        }
    }
    // a footer reads what the cooks left in the cache. A node nobody cached is cooked for it only
    // while it is known to be cheap: one that took over 2 ms of its own (a chain fused into a
    // kernel, run again on its own) is never cooked just to be counted.
    let optional: Vec<Option<Output>> = request
        .optional
        .iter()
        .map(|n| match session::cached(session, &request.context, n) {
            Some(hit) => Some(hit),
            None => match session::node_seconds(session, node::id(n)) {
                Some(seconds) if seconds > 0.002 => None,
                _ => session::cook(session, None, &request.context, n).ok(),
            },
        })
        .collect();
    (request.prepare)(outputs, optional).map_err(Error::PrepareError)
}

fn worker_loop(shared: Arc<Shared>) {
    // the predicate the session holds: only this thread touches the session, so a predicate set
    // while a cook runs waits for the next request
    let mut applied = none_volatile();
    loop {
        let request = {
            let mut state = shared.state.lock().unwrap();
            while !state.stopping && state.pending.is_none() {
                state = shared.ready.wait(state).unwrap();
            }
            if state.stopping {
                return;
            }
            let request = state.pending.take().expect("pending request");
            state.active = Some(Active {
                started: clock_now(&shared),
                cancel: Arc::clone(context::cancel_token(&request.context)),
            });
            request
        };
        let id = request.id;
        let volatile = Arc::clone(&request.volatile);
        let started = clock_now(&shared);
        let result = {
            let mut session = shared.session.lock().unwrap();
            if !Arc::ptr_eq(&volatile, &applied) {
                session::set_volatile(&mut session, Arc::clone(&volatile));
            }
            let result = execute(&mut session, request);
            *shared.stats.lock().unwrap() = session::stats(&session);
            result
        };
        let seconds = (clock_now(&shared) - started).max(0.0);
        {
            let mut state = shared.state.lock().unwrap();
            state.active = None;
            if !state.stopping && id == state.latest_id {
                state.completion = Some(Completion { request_id: id, seconds, result });
            }
            shared.finished.notify_all();
        }
        applied = volatile;
    }
}

pub fn create(max_entries: i64, max_payload_bytes: i64) -> Result<T, String> {
    let session = session::create(max_entries, max_payload_bytes)?;
    let owner = session::payload_owner(&session);
    let held = session::payload_pool(&session);
    let shared = Arc::new(Shared {
        times: session::times(&session),
        stats: Mutex::new(session::stats(&session)),
        session: Mutex::new(session),
        state: Mutex::new(State {
            pending: None,
            active: None,
            completion: None,
            latest_id: 0,
            next_volatile: none_volatile(),
            stopping: false,
            closed: false,
            worker: None,
        }),
        ready: Condvar::new(),
        finished: Condvar::new(),
        clock: Mutex::new(Arc::new(|| {
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs_f64())
                .unwrap_or(0.0)
        })),
        closed: AtomicBool::new(false),
    });
    let worker_shared = Arc::clone(&shared);
    let worker = std::thread::Builder::new()
        .name("sop-async-cook".to_owned())
        .stack_size(256 << 20)
        .spawn(move || worker_loop(worker_shared))
        .map_err(|e| format!("Async_cook.create: {e}"))?;
    shared.state.lock().unwrap().worker = Some(worker);
    // The retreat runs on the frame's thread. Only the worker touches the session, and only while
    // `active` is set, which it does under the state lock: holding that with nothing active, the
    // session is this thread's until the lock is given back. A cook in progress is left alone.
    let weak: Weak<Shared> = Arc::downgrade(&shared);
    let live_weak = weak.clone();
    mem::register_evictor(
        &owner,
        mem::EvictorOptions {
            rebuild: 8.0,
            held: Some(held),
            live: Some(Arc::new(move || live_weak.upgrade().is_some_and(|s| !s.closed.load(Ordering::Relaxed)))),
            ..mem::EvictorOptions::default()
        },
        Arc::new(move || {
            let Some(shared) = weak.upgrade() else { return false };
            let Ok(state) = shared.state.try_lock() else { return false };
            if state.active.is_some() || shared.closed.load(Ordering::Relaxed) {
                return false;
            }
            let Ok(mut session) = shared.session.try_lock() else { return false };
            session::shed(&mut session)
        }),
    );
    Ok(T(shared))
}

/// The wall clock behind cook timings (default: the system clock). Call while idle.
pub fn set_clock(value: &T, clock: Arc<dyn Fn() -> f64 + Send + Sync>) {
    *value.0.clock.lock().unwrap() = Arc::clone(&clock);
    session::set_clock(&mut value.0.session.lock().unwrap(), clock);
}

/// A batch cook with `optional` nodes taken after the required ones from what the session cached;
/// one with no cached output is cooked only if its last real cook took at most 2 ms of its own (or
/// it never ran). A node that fails, was skipped as costly or was cancelled is `None` and never
/// fails the request. Returns the request id.
pub fn submit_some(value: &T, context: context::T, nodes: Vec<node::T>, optional: Vec<node::T>, prepare: Prepare) -> Result<i64, String> {
    let mut state = value.0.state.lock().unwrap();
    if state.closed || state.stopping {
        return Err("Async_cook.submit: worker is closed".to_owned());
    }
    if let Some(pending) = &state.pending {
        cancel::cancel(context::cancel_token(&pending.context));
    }
    if let Some(active) = &state.active {
        cancel::cancel(&active.cancel);
    }
    state.latest_id += 1;
    let id = state.latest_id;
    state.pending = Some(Request { id, context, nodes, optional, prepare, volatile: Arc::clone(&state.next_volatile) });
    state.completion = None;
    value.0.ready.notify_one();
    Ok(id)
}

pub fn submit_all(value: &T, context: context::T, nodes: Vec<node::T>, prepare: Box<dyn FnOnce(Vec<Output>) -> Result<Prepared, String> + Send>) -> Result<i64, String> {
    submit_some(value, context, nodes, Vec::new(), Box::new(move |outputs, _| prepare(outputs)))
}

/// Submit the newest desired graph. The request and preparation callback are retained only until
/// this bounded job completes or is superseded.
pub fn submit(value: &T, context: context::T, node: node::T, prepare: Box<dyn FnOnce(Output) -> Result<Prepared, String> + Send>) -> Result<i64, String> {
    submit_all(
        value,
        context,
        vec![node],
        Box::new(move |mut outputs| {
            if outputs.len() == 1 {
                prepare(outputs.remove(0))
            } else {
                Err("Async_cook.submit: expected one output".to_owned())
            }
        }),
    )
}

/// `Session.set_volatile` on the worker's session, applied by the worker before the next request
/// it starts (a queued one included): the cook in flight keeps the predicate it started with, and
/// the predicate is only ever called on the worker's thread.
pub fn set_volatile(value: &T, predicate: Arc<dyn Fn(i64) -> bool + Send + Sync>) {
    let mut state = value.0.state.lock().unwrap();
    state.next_volatile = Arc::clone(&predicate);
    if let Some(pending) = &mut state.pending {
        pending.volatile = predicate;
    }
}

/// `Session.node_seconds` on the worker's session.
pub fn node_seconds(value: &T, id: i64) -> Option<f64> {
    value.0.times.lock().unwrap().get(&id).map(|s| s.seconds)
}

/// The worker's session counters as of the last finished request. Call while idle for a settled
/// reading.
pub fn stats(value: &T) -> session::Stats {
    value.0.stats.lock().unwrap().clone()
}

/// Block until the newest submitted request completes and take it, like `poll`. For fixed-step runs
/// only (an interactive sketch never blocks); panics (OCaml `Invalid_argument`) when nothing is
/// pending or the worker is closed.
pub fn await_completion(value: &T) -> Completion {
    let mut state = value.0.state.lock().unwrap();
    assert!(!(state.closed || state.stopping), "Async_cook.await: worker is closed");
    assert!(
        !(state.completion.is_none() && state.pending.is_none() && state.active.is_none()),
        "Async_cook.await: nothing was submitted"
    );
    while state.completion.is_none() && (state.pending.is_some() || state.active.is_some()) {
        state = value.0.finished.wait(state).unwrap();
    }
    state.completion.take().expect("Async_cook.await: request was superseded")
}

/// Remove and return the newest completed result, if any.
pub fn poll(value: &T) -> Option<Completion> {
    value.0.state.lock().unwrap().completion.take()
}

pub fn status(value: &T) -> Status {
    let state = value.0.state.lock().unwrap();
    match (&state.active, &state.pending) {
        (None, None) => Status::Idle,
        (Some(active), pending) => Status::Cooking {
            request_id: state.latest_id,
            seconds: (clock_now(&value.0) - active.started).max(0.0),
            queued: pending.is_some(),
        },
        (None, Some(pending)) => Status::Cooking { request_id: pending.id, seconds: 0.0, queued: true },
    }
}

pub fn close(value: &T) {
    let worker = {
        let mut state = value.0.state.lock().unwrap();
        if state.closed {
            None
        } else {
            state.closed = true;
            state.stopping = true;
            value.0.closed.store(true, Ordering::Relaxed);
            if let Some(pending) = &state.pending {
                cancel::cancel(context::cancel_token(&pending.context));
            }
            if let Some(active) = &state.active {
                cancel::cancel(&active.cancel);
            }
            state.pending = None;
            state.completion = None;
            value.0.ready.notify_all();
            value.0.finished.notify_all();
            state.worker.take()
        }
    };
    if let Some(worker) = worker {
        let _ = worker.join();
        session::close(&mut value.0.session.lock().unwrap());
    }
}
